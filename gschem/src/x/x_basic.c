/* -*- C x_basic.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file x_basic.c
 * \brief Module to provide general suppport for the Main Window
 * TODO: Eliminate or re-assign this module
 */

#include "gschem.h"
#include <geda_debug.h>

/*! \brief Set Cursor/Pointer Position
 *  \par Function Description
 *   This function sets the pointer position to relative
 *  screen coordinates of the given widget.
 *
 *  \param [in] widget to which the coordinates will be relative
 *  \param [in] x      integer abscissa in screen units
 *  \param [in] y      integer ordinate in screen units
 *
 * \note WEH: For setting the cursor relative to the drawing area
 *       using world coordinates see x_event_set_pointer_position.
 */
void x_basic_warp_cursor (GtkWidget* widget, int x, int y)
{
  GdkScreen *screen;
  GdkDisplay *display;
  int window_x, window_y;

  gdk_window_get_origin (widget->window, &window_x, &window_y);

  screen = gtk_widget_get_screen (widget);
  display = gdk_screen_get_display (screen);

  gdk_display_warp_pointer (display, screen, window_x + x, window_y + y);
}
