/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#include <config.h>

#include <libgeda_priv.h>

/*!
 * \brief Create an Empty GedaObject
 * \par Function Description
 * Create and return an empty <b>GedaObject</b> object with sensible
 * default properties.
 *
 * \returns the newly created GedaObject.
 */
GedaObject *geda_struct_object_new (int type)
{
  return geda_object_new(type);
}

/*!
 * \brief Attach attribute to an GedaObject and append Parent's Page
 * \par Function Description
 *  This function is similar to geda_attrib_object_add, which is called by
 *  the function, the difference being that this function also
 *  adds the attribute being attached to the page if the parent
 *  is already on a page.
 *
 * \param [in]  parent The GedaObject that child is being added to.
 * \param [in]  child  The item you want to add as an attribute.
 *
 * \return nothing.
 */
void geda_struct_object_add_child(GedaObject *parent, GedaObject *child) {

  /* if the object is on a page then add the child */
  Page *page = geda_object_get_page(parent);

  if (page && (GEDA_IS_PAGE(page))) {
    geda_struct_page_append_object(page, child);
  }

  geda_attrib_object_add(parent, child);
}

/*!
 * \brief Remove an GedaObject
 * \par Function Description
 *  This function unreferences a GedaObject after first removing the GedaObject
 *  from a page if the object is on a page, and disconnecting any "electrical"
 *  connections.
 */
/*
 * Note: WEH (11/04/13): Modified to add conditional for geda_struct_conn_remove_
 * object to else clause of if page member, because the connections would be removed
 * by pre_object_remove if the object was on a page, Also added check for NULL
 * conn_list since there is no point in making the call if no connections exist.
 */
void geda_struct_object_release(GedaObject *o_current)
{
  if (GEDA_IS_OBJECT(o_current)) {

    /* If currently attached to a page, remove it from the page */
    if (GEDA_IS_PAGE(o_current->page)) {
      geda_struct_page_remove_object (o_current->page, o_current);
    }
    else if (o_current->conn_list != NULL) {
      geda_struct_conn_remove_object (o_current);
    }

    if (o_current->attached_to != NULL) {
      geda_attrib_object_remove(&o_current->attached_to->attribs, o_current);
    }

    geda_attrib_object_detach_all (o_current);

    if (o_current->complex && o_current->complex->prim_objs) {
      geda_struct_object_release_objects (o_current->complex->prim_objs);
    }

    geda_object_unref(o_current);
  }
}

/*!
 * \brief Deletes a list of Objects
 * \par Function Description
 *  Deletes everything including the GList
 */
void geda_struct_object_release_objects(GList *list)
{
  GList *ptr = g_list_last(list);

  /* Do the delete backwards */
  while (ptr != NULL) {

    GedaObject *o_current = ptr->data;

    geda_struct_object_release(o_current);

    ptr = g_list_previous (ptr);
  }
}

/*!
 * \brief Mark GedaObject's Page as modified
 * \par Function Description
 *  Updates the CHANGED flag of the page associated with \a object
 *  if the object is attached to a page.
 */
void geda_struct_object_set_page_changed (const GedaObject *object)
{
  Page *page = geda_object_get_page (object);
  geda_page_set_changed (page, TRUE);        /* possibly set CHANGED flag */
}
