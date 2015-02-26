/* -*- C o_copy.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file o_copy.c
 * \brief Low-level module for copying objects
 */

#include <gschem.h>
#include <geda_debug.h>

/*! \brief Start a Copy operation
 *  \par Function Description
 *  This function is called at the beginning of a copy operation
 *  to save the x and y coordinates for the event and if there is
 *  a selection, add the current selection to the place list.
 */
bool o_copy_start(GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int status = FALSE;

  /* Copy the objects into the buffer at their current position,
   * with future motion relative to the mouse origin, (w_x, w_y). */

  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  if (o_select_is_selection(w_current)) {

    GList *s_current = geda_list_get_glist(Top_Selection);
    s_place_set_place_list(toplevel, s_current);

    status = o_place_start(w_current, w_x, w_y);
  }
  return w_current->inside_action = status;
}

/*! \brief Start Multiple Copy Mode
 *  \par Function Description
 *  This function is called at the beginning of a copy multiple operation.
 *  The function uses the normal o_copy_start function to save the x and y
 *  coordinates for the event and if o_copy_start is successful the event
 *  state is set to ENDMCOPY.
 */
void o_copy_multiple_start(GschemToplevel *w_current, int w_x, int w_y)
{
  if (o_copy_start(w_current, w_x, w_y)) {
    w_current->event_state = ENDMCOPY;
  }
}

/*! \brief Can cancel a copy operation
 *  \par Function Description
 *
 */
void o_copy_cancel(GschemToplevel *w_current)
{
  s_place_free_place_list(w_current->toplevel);

  w_current->inside_action = 0;
  i_status_set_state(w_current, SELECT);
}

/*! \brief Finalize Copy operation of a single object
 *  \par Function Description
 *  This function is called to complete a Copy operation for a single
 *  object, this is the normal copy operation not envolving the system
 *  clip board. If the SHIFTKEY is set then SELECTION is replaced with
 *  the new objects, otherwise SELECTION is not altered.
 */
void o_copy_end(GschemToplevel *w_current)
{
  if (!w_current->SHIFTKEY) {

    int wx = w_current->second_wx;
    int wy = w_current->second_wy;

    o_place_end (w_current, wx, wy, FALSE, NULL, "%copy-objects-hook");
  }
  else {

    GList *list = NULL;
    int    wx   = w_current->second_wx;
    int    wy   = w_current->second_wy;

    o_place_end (w_current, wx, wy, FALSE, &list, "%copy-objects-hook");

    o_select_unselect_all(w_current);
    o_select_add_list(w_current, list);
    g_list_free(list);

  }
  w_current->inside_action = FALSE;
}

/*! \brief  Finalize Copy operation of a multible objects
 *  \par Function Description
 *
 */
void o_copy_multiple_end(GschemToplevel *w_current)
{
  int wx = w_current->second_wx;
  int wy = w_current->second_wy;

  o_place_end (w_current, wx, wy, TRUE, NULL, "%paste-objects-hook");

  /* Stay on ENDMCOPY mode */
  w_current->inside_action = TRUE;
}
