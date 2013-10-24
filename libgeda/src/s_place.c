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
 *  \brief The Placement Object List
 *
 * 
 */

#include "libgeda_priv.h"
void s_place_free_place_list(TOPLEVEL *toplevel)
{
  if (toplevel != NULL) {
    if (toplevel->page_current->place_list != NULL) {
      s_delete_object_glist(toplevel, toplevel->page_current->place_list);
      toplevel->page_current->place_list = NULL;
    }
  }
  else {
    fprintf(stderr, "oops, libgeda.s_place_free_place_list: TopLevel is NULL\n");
  }
}

void s_place_set_place_list(TOPLEVEL *toplevel, GList *new_place_list )
{
  if (toplevel != NULL) {
    if (toplevel->page_current->place_list) {
      s_place_free_place_list(toplevel);
    }
    if (new_place_list) {
      toplevel->page_current->place_list =
      o_glist_copy_all (toplevel, new_place_list,
                        toplevel->page_current->place_list);
    }
  }
  else {
    fprintf(stderr, "oops, libgeda.s_place_set_place_list: TopLevel is NULL\n");
  }
}








