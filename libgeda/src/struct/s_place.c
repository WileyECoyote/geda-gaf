/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
 *  Object List
 *
 */

#include "libgeda_priv.h"

/*! \brief Free Place List
 *  \par
 *   The functions releases all objects reference by the current
 *   toplevel place-list and sets the place_list pointer to NULL.
 *
 *  \param [in] toplevel pointer to GedaToplevel object
 *
 */
void s_place_free_place_list(GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {

    if (toplevel->page_current->place_list != NULL) {
      s_object_release_objects(toplevel->page_current->place_list);
      toplevel->page_current->place_list = NULL;
    }

  }
  else {
    BUG_MSG("TopLevel is NULL\n");
  }
}

/*! \brief Set Place List
 *  \par
 *   The functions can be used to either set or clear the place list
 *   associated with the given GedaToplevel object. If the place references
 *   data, s_place_free_place_list is called to release the current list
 *   before adding any new object. No error is genereated of the new place
 *   list is NULL.
 *
 *  \param [in] toplevel pointer to GedaToplevel object
 *
 */
void s_place_set_place_list(GedaToplevel *toplevel, GList *new_place_list )
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {

    if (toplevel->page_current->place_list) {
      s_place_free_place_list(toplevel);
    }

    if (new_place_list) {
      toplevel->page_current->place_list =
      o_glist_copy_all (new_place_list,
                        toplevel->page_current->place_list);
    }

  }
  else {
    BUG_MSG("TopLevel is NULL\n");
  }
}








