/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_window.c
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include "x_window.h"

#include <geda_debug.h>

/*! \brief get the pointer position of a given GschemToplevel
 *  \par Function Description
 *  This function gets the pointer position of the drawing area of the
 *  current workspace <b>GschemToplevel</b>. The flag <b>snapped</b> specifies
 *  whether the pointer position should be snapped to the current grid.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] snapped    An option flag to specify the wished coords
 *  \param [out] wx        snapped/unsnapped world x coordinate
 *  \param [out] wy        snapped/unsnapped world y coordinate
 *
 *  \return Returns TRUE if the pointer position is inside the drawing area.
 *
 */
bool i_window_get_pointer_position (GschemToplevel *w_current,
                                   bool snapped, int *wx, int *wy)
{
  int sx, sy, x, y;

  gtk_widget_get_pointer(w_current->drawing_area, &sx, &sy);

  /* check if we are inside the drawing area */
  if (sx < 0 || sx >= w_current->screen_width  ||
      sy < 0 || sy >= w_current->screen_height) {
    return FALSE;
  }

  SCREENtoWORLD (w_current, sx, sy, &x, &y);

  if (snapped) {
    x = snap_grid (w_current, x);
    y = snap_grid (w_current, y);
  }

  *wx = x;
  *wy = y;

  return TRUE;
}

/*! \brief Set Pointer Position Relative to the Drawing Area
 *  \par Function Description
 *   This function sets the pointer position to relative
 *  screen coordinates off the given widget.
 *
 *  \param [in] w_current   The GschemToplevel object
 *  \param [in] wx     integer abscissa in World units
 *  \param [in] wy     integer ordinate in World units
 *
 */
void i_window_set_pointer_position (GschemToplevel *w_current, int wx, int wy)
{
  int sx, sy;

  gtk_window_present (GTK_WINDOW(w_current->main_window));

  WORLDtoSCREEN (w_current,  wx, wy, &sx, &sy);

  /* set cursor position */
  i_pan_warp_cursor (w_current->drawing_area, sx, sy);

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is used during an open command to setup the correct sizes
 */
void i_window_set_viewport_size(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  /* of the actual win window (drawing_area) */
  w_current->screen_width  = DrawingArea->allocation.width;
  w_current->screen_height = DrawingArea->allocation.height;

#if DEBUG_EVENTS
  printf("manual: %d %d\n", w_current->screen_width, w_current->screen_height);
#endif

  /* need to do this every time the width / height change */
  x_window_setup_page(w_current, toplevel->page_current,
                      toplevel->page_current->left,
                      toplevel->page_current->right,
                      toplevel->page_current->top,
                      toplevel->page_current->bottom);

#if DEBUG_EVENTS
  float width  = (float) w_current->screen_width;
  float height = (float) w_current->screen_height);
  printf("Window aspect: %f\n", width / height);
#endif
}
