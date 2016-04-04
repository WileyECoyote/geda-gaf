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

#include <config.h>
#include <stdio.h>

#include <libgeda_priv.h>

/*!
 *  \brief Copy GList to GList
 *  \par Function Description
 *  you need to pass in a head_node for dest_list_head flag is either
 *  NORMAL_FLAG or SELECTION_FLAG this function copies the objects in
 *  the src GList, \a src_list, to the destination GList, \a dest_list
 *  this routine assumes that objects in src_list are selected objects
 *  are unselected before they are copied and then reselected this is
 *  necessary to preserve the color info.
 *
 *  \param [in] src_list   The GList to copy from
 *  \param [in] dest_list  The GList to copy to
 *
 *  \return dest_list GList with objects appended
 *
 *  \todo should be o_list_concat?
 */
GList* o_list_copy_all (const GList *src_list, GList *dest_list)
{
  const GList *src;
  GList *dest;
  GedaObject *src_object, *dst_object;
  int selected_save;

  src = src_list;
  /* Reverse any existing items, as we will prepend, then reverse at the end */
  dest = g_list_reverse (dest_list);

  if (src == NULL) {
    return(NULL);
  }

  /* first do all NON text items */
  while(src != NULL) {

    src_object = (GedaObject *) src->data;

    if (GEDA_IS_OBJECT(src_object)) {

      /* unselect the object before the copy */
      selected_save = src_object->selected;

      if (selected_save)
        o_selection_unselect (src_object);

      if (src_object->type != OBJ_TEXT) {
        dst_object = o_copy_object (src_object);
        dest = g_list_prepend (dest, dst_object);
      }

      /* reselect it */
      if (selected_save) {
        o_selection_select (src_object);
      }
    }

    src = g_list_next(src);
  }

  src = src_list;

  /* then do all text items */
  while(src != NULL) {

    src_object = (GedaObject *) src->data;

    if (GEDA_IS_OBJECT(src_object)) {

      /* unselect the object before the copy */
      selected_save = src_object->selected;

      if (selected_save)
        o_selection_unselect (src_object);

      if (src_object->type == OBJ_TEXT) {
        dst_object = o_copy_object (src_object);
        dest = g_list_prepend (dest, dst_object);

        if (src_object->attached_to != NULL &&
            src_object->attached_to->copied_to != NULL)
        {
          geda_attrib_object_attach(src_object->attached_to->copied_to, dst_object, FALSE);
          /* handle slot= attribute, it's a special case */
          if (g_ascii_strncasecmp (dst_object->text->string, "slot=", 5) == 0)
            s_slot_update_object (src_object->attached_to->copied_to);
        }
      }

      /* reselect it */
      if (selected_save) {
        o_selection_select (src_object);
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

/*! \brief Translates a glist of Objects
 *  \par Function Description
 *  Calls o_translate_object for each glist data member
 */
void o_list_translate(const GList *list, int dx, int dy)
{
  const GList *o_iter;

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    s_conn_remove_object (o_current);
    o_iter = o_iter->next;
  }

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    o_translate_object(o_current, dx, dy);
    o_iter = o_iter->next;
  }

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    s_conn_update_object (o_current);
    o_iter = o_iter->next;
  }
}

/*! \brief Rotate a glist of Objects
 *  \par Function Description
 *  Calls o_rotate_object for each glist data member
 */
void o_list_rotate (const GList *list, int x, int y, int angle)
{
  const GList *o_iter;

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    s_conn_remove_object (o_current);
    o_iter = o_iter->next;
  }

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = (GedaObject *)o_iter->data;
    o_rotate_object (o_current, x, y, angle);
    o_iter = o_iter->next;
  }

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    s_conn_update_object (o_current);
    o_iter = o_iter->next;
  }
}

/*! \brief Mirror a glist of Objects
 *  \par Function Description
 *   Calls o_mirror_object for each glist data member
 */
void o_list_mirror (const GList *list, int x, int y)
{
  const GList *o_iter;

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    s_conn_remove_object (o_current);
    o_iter = o_iter->next;
  }

  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = (GedaObject *)o_iter->data;
    o_mirror_object (o_current, x, y);
    o_iter = o_iter->next;
  }

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  o_iter = list;
  while (o_iter != NULL) {
    GedaObject *o_current = o_iter->data;
    s_conn_update_object (o_current);
    o_iter = o_iter->next;
  }
}

/*! \brief Change the color of a list of objects
 *  \par Function Description
 *   This function changes the the new color of a list of objects
 *
 *  \param [in] list    The list of Objects to change color.
 *  \param [in] color   The new color.
 */
void o_list_set_color (const GList *list, int color)
{
  const GList *iter;

  for (iter = list; iter != NULL; iter = g_list_next (iter))
    o_set_color (iter->data, color);
}
