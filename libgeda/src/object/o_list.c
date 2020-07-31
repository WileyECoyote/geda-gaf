/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*! \file o_list.c
 *  \brief utility functions for GedaList
 *
 *  GedaList is a List object wrapper for GLists. The GedaListclass
 *  provides advanced methods for manipulation of GLists, including
 *  Gobject signals.
 */

/** \defgroup list-object-proc GedaList Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaList Objects
 */

#include "../../../config.h"

#include <stdio.h>

#include <libgeda_priv.h>

/*!
 * \brief Copy GList to GList
 * \par Function Description
 *  This function copies the objects in the src GList, \a src_list, to
 *  the destination GList \a dest_list. Objects selected in the src_list
 *  are unselected before they are copied and then reselected to preserve
 *  the color info.
 *
 * \param [in] src_list   The GList to copy from
 * \param [in] dest_list  Head node of GList to copy to
 *
 * \return dest_list GList with objects appended
 *
 * \todo should be o_list_concat?
 */
GList *geda_object_list_copy_all (const GList *src_list, GList *dest_list)
{
   GedaObject *src_object, *dst_object;
  const GList *src;
        GList *dest;

  if (src_list == NULL) {
    return(NULL);
  }

  /* Reverse any existing items, as we will prepend, then reverse at the end */
  dest = g_list_reverse (dest_list);

  src = src_list;

  /* First do all NON text items */
  while (src != NULL) {

    src_object = src->data;

    if (GEDA_IS_OBJECT(src_object) && (src_object->type != OBJ_TEXT)) {

      dst_object = geda_object_copy (src_object);

      dest = g_list_prepend (dest, dst_object);
    }
    src = g_list_next(src);
  }

  src = src_list;

  /* then do all text items */
  while (src != NULL) {

    src_object = (GedaObject *) src->data;

    if (GEDA_IS_OBJECT(src_object) && (src_object->type == OBJ_TEXT)) {

      dst_object = geda_object_copy (src_object);

      dest = g_list_prepend (dest, dst_object);

      /* If an object in the */
      if (src_object->attached_to != NULL &&
          src_object->attached_to->copied_to != NULL)
      {

        GedaObject *copied_to = src_object->attached_to->copied_to;

        geda_attrib_object_attach(copied_to, dst_object, FALSE);

        /* handle slot= attribute, it's a special case */
        if (!geda_strncmpi (dst_object->text->string, "slot=", 5))
        {
          geda_struct_slot_update_object (copied_to);
        }
      }
    }
    src = g_list_next(src);
  }

  /* Clean up dangling copied_to pointers */
  src = src_list;
  while(src != NULL) {
    src_object = src->data;
    src_object->copied_to = NULL;
    src = g_list_next (src);
  }

  /* Reverse the list to be in the correct order */
  dest = g_list_reverse (dest);

  return(dest);
}

/*!
 * \brief Find an attribute in a list.
 * \par Function Description
 *  Case sensitive search for attribute by name. Counter is the n'th
 *  occurance of the attribute, and starts searching from zero.  Zero
 *  is the first occurance of an attribute.
 *
 * \param [in] list   GList of attributes to search.
 * \param [in] name   Character string with attribute name to search for.
 * \param [in] count  Which occurance to return.
 *
 * \return The n'th attribute object in the given list with the given name.
 *
 * \todo This function should not be called geda_attrib_object since it
 *       does not accept an object as a argument!
 */
GedaObject *geda_object_list_find_attrib_by_name (const GList *list,
                                                  const char  *name,
                                                        int    count)
{
  if (name) {

    const GList *iter;

    int counter;
    int length;

    counter = 0;
    length  = strlen(name);

    for (iter = list; iter != NULL; iter = iter->next) {

      GedaObject *attribute = iter->data;

      if (geda_object_get_is_valid_attribute(attribute)) {

        const char *string = attribute->text->string;

        if (strncmp (string, name, length) == 0) {

          if (string[length] == '=') {

            if (counter == count) {
              return attribute;
            }
            counter++;
          }
        }
      }
    }
  }

  return NULL;
}

/*!
 * \brief Find all floating attributes in the given object list.
 * \par Function Description
 *  Find all floating attributes in the given object list.
 *
 * \param [in] list  GList of Objects to search for floating attributes.
 *
 * \return GList of floating attributes from the input list
 *
 * \note Caller should g_list_free returned list.
 */
GList *geda_object_list_find_floating (const GList *list)
{
  GList *floating_attributes = NULL;
  const  GList *iter;

  for (iter = list; iter != NULL; iter = iter->next) {

    GedaObject *o_current = iter->data;

    /* Skip non text objects, attached attributes and text which doesn't
     * constitute a valid attributes (e.g. general text placed on the page)
     */
    if (o_current->type == OBJ_TEXT &&
        o_current->attached_to == NULL &&
        geda_object_get_is_valid_attribute (o_current)) {

      floating_attributes = g_list_prepend (floating_attributes, o_current);
    }
  }

  return g_list_reverse (floating_attributes);
}

/*!
 * \brief Mirror a glist of Objects
 * \par Function Description
 *  Calls geda_object_mirror for each glist data member
 */
void geda_object_list_mirror (const GList *list, int x, int y)
{
  const GList *o_iter;

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    geda_struct_conn_remove_object (o_current);
    o_iter = o_iter->next;
  }

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = (GedaObject *)o_iter->data;
    geda_object_mirror (o_current, x, y);
    o_iter = o_iter->next;
  }

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    geda_struct_conn_update_object (o_current);
    o_iter = o_iter->next;
  }
}

/*!
 * \brief Rotate a glist of Objects
 * \par Function Description
 *  Calls geda_object_rotate for each glist data member.
 *
 *  \param [in,out] list   The list with objects to rotate.
 *  \param [in]     x      The x coordinate of rotation.
 *  \param [in]     y      The y coordinate of rotation.
 *  \param [in]     angle  The angle of rotation.
 */
void geda_object_list_rotate (const GList *list, int x, int y, int angle)
{
  const GList *o_iter;

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    geda_struct_conn_remove_object (o_current);
    o_iter = o_iter->next;
  }

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = (GedaObject *)o_iter->data;
    geda_object_rotate (o_current, x, y, angle);
    o_iter = o_iter->next;
  }

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    geda_struct_conn_update_object (o_current);
    o_iter = o_iter->next;
  }
}

/*!
 * \brief Scale a set of lines.
 * \par Function Description
 *  This function takes a list of objects and scales each by
 *  the values of x_scale and y_scale.
 *
 *  \param [in,out] list     The list with lines to scale.
 *  \param [in]     x_scale  The x scale value for the lines.
 *  \param [in]     y_scale  The y scale value for the lines.
 */
void geda_object_list_scale (const GList *list, int x_scale, int y_scale)
{
  const GList *iter;

  /* is okay if nothing selected */
  if (list) {

    iter = list;

    while (iter != NULL) {

      GedaObject *o_current = (GedaObject*)iter->data;

      switch (o_current->type) {

        case(OBJ_ARC):
          geda_arc_object_scale(o_current, x_scale);
          break;

        case(OBJ_BOX):
          geda_box_object_scale(o_current, x_scale, y_scale);
          break;

        case(OBJ_BUS):
          geda_line_object_scale(o_current, x_scale, y_scale);
          break;

        case(OBJ_CIRCLE):
          geda_circle_object_scale(o_current, x_scale);
          break;

        case(OBJ_LINE):
        case(OBJ_NET):
          geda_line_object_scale(o_current, x_scale, y_scale);
          break;

        case(OBJ_PICTURE):
          geda_picture_object_scale(o_current, x_scale, y_scale);
          break;

        case(OBJ_TEXT):
          geda_text_object_scale(o_current, x_scale);
          break;
      }
      iter = iter->next;
    }
  }
}

/*!
 * \brief Change the color of a list of objects
 * \par Function Description
 *  This function changes the the new color of a list of objects.
 *
 * \param [in] list    The list of Objects to change color.
 * \param [in] color   The new color.
 */
void geda_object_list_set_color (const GList *list, int color)
{
  const GList *iter;

  for (iter = list; iter != NULL; iter = g_list_next (iter)) {
    geda_set_object_color (iter->data, color);
  }
}

/*!
 * \brief Translates a glist of Objects
 * \par Function Description
 *  Calls geda_object_translate for each glist data member.
 *
 *  \param [in,out] list  The list with lines to scale.
 *  \param [in]     dx    The x distance to translate.
 *  \param [in]     dy    The y distance to translate.
 */
void geda_object_list_translate(const GList *list, int dx, int dy)
{
  const GList *o_iter;

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    geda_struct_conn_remove_object (o_current);
    o_iter = o_iter->next;
  }

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    geda_object_translate(o_current, dx, dy);
    o_iter = o_iter->next;
  }

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    geda_struct_conn_update_object (o_current);
    o_iter = o_iter->next;
  }
}

/** @} endgroup list-object-proc */
