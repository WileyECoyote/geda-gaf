/* -*- x_window-h -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 *
 *  Date: October, 11, 2013
 *  Contributing Author: Wiley Edward Hill
 */

#ifndef __X_WINDOW_H__
#define __X_WINDOW_H__

#define MainWindow  w_current->main_window
#define DrawingArea w_current->drawing_area

#define HorizontalScroll       w_current->h_scrollbar
#define VerticalScroll         w_current->v_scrollbar
#define HorizontalScrollRange  GTK_RANGE(HorizontalScroll)
#define VerticalScrollRange    GTK_RANGE(VerticalScroll)

#define GSE_HANDLER(f) ((GschemDrawEvent) (f))

#endif
