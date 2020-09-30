/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 *
 *  Date: October, 11, 2013
 *  Contributing Author: Wiley Edward Hill
 */

/*! \file s_place.c
 *  \brief
 *   The contain utility functions to manipulate Placement
 *  GedaObject List
 *
 */

#include "../../../config.h"
#include <libgeda_priv.h>


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void geda_struct_place_append_place_list(GedaToplevel *toplevel, GedaObject *object)
{
  Page *page = geda_toplevel_get_current_page (toplevel);

  if (page && GEDA_IS_OBJECT (object)) {

    page->place_list = g_list_append (page->place_list, object);

  }
}

/*! \brief Free Place List
 *  \par
 *   The functions releases all objects reference by the current
 *   toplevel place-list and sets the place_list pointer to NULL.
 *
 *  \param [in] toplevel pointer to GedaToplevel object
 *
 */
void geda_struct_place_free_place_list(GedaToplevel *toplevel)
{
  Page  *page = geda_toplevel_get_current_page (toplevel);

  if (page) {

    GList *list = geda_page_get_place_list(page);

    if (list) {
      geda_struct_object_release_objects(list);
      toplevel->page_current->place_list = NULL;
    }
  }
}

/*! \brief Get Place List
 *  \par Function Description
 *   This functions can be used to both check and get a pointer to the
 *   current place list. If \a toplevel and toplevel->page_current are
 *   valid, page_current->place_list is returned, which could be NULL.
 *   If \a toplevel or toplevel->page_current are not valid then NULL
 *   is returned.
 *
 *  \param [in] toplevel pointer to GedaToplevel object
 *
 *  \return list or NULL if there was an error or no list.
 */
GList *geda_struct_place_get_place_list(GedaToplevel *toplevel)
{
  Page *page = geda_toplevel_get_current_page (toplevel);

  if (page) {

    return (page->place_list);

  }

  return NULL;
}

/*! \brief Set Place List
 *  \par Function Description
 *   This functions can be used to either set or clear the place list
 *   associated with the given GedaToplevel object. If new_place_list
 *   references data, geda_struct_place_free_place_list is called to
 *   release the current list before adding new objects. No error is
 *   generated if the new place list is NULL.
 *
 *  \param [in] toplevel       pointer to GedaToplevel object
 *  \param [in] new_place_list Glist of objects to append or NULL to clear
 *                             the current place list.
 */
void geda_struct_place_set_place_list(GedaToplevel *toplevel, GList *new_place_list)
{
  Page *page = geda_toplevel_get_current_page (toplevel);

  if (page) {

    geda_struct_place_free_place_list(toplevel);

    if (new_place_list) {

      GList *list;

      list = geda_copy_list (new_place_list, page->place_list);

      geda_page_set_place_list (page, list);
    }
  }
}
