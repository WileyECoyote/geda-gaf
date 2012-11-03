/* gtkextra - set of widgets for gtk+
 * Copyright 1999-2001  Adrian E. Feiguin <feiguin@ifir.edu.ar>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef GTK_SHEET_H
#define GTK_SHEET_H

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <glib.h>


/* sheet flags */
enum
{ 
  GTK_SHEET_IS_LOCKED       = 1 << 0,
  GTK_SHEET_IS_FROZEN       = 1 << 1,
  GTK_SHEET_IN_XDRAG        = 1 << 2,
  GTK_SHEET_IN_YDRAG        = 1 << 3,
  GTK_SHEET_IN_DRAG         = 1 << 4,
  GTK_SHEET_IN_SELECTION    = 1 << 5,
  GTK_SHEET_IN_RESIZE       = 1 << 6,
  GTK_SHEET_IN_CLIP         = 1 << 7,
  GTK_SHEET_REDRAW_PENDING  = 1 << 8,
};

#define GTK_SHEET_FLAGS(sheet)             (GTK_SHEET (sheet)->flags)
#define GTK_SHEET_SET_FLAGS(sheet,flag)    (GTK_SHEET_FLAGS (sheet) |= (flag))
#define GTK_SHEET_UNSET_FLAGS(sheet,flag)  (GTK_SHEET_FLAGS (sheet) &= ~(flag))

#define GTK_SHEET_IS_FROZEN(sheet)   (GTK_SHEET_FLAGS (sheet) & GTK_SHEET_IS_FROZEN)
#define GTK_SHEET_IN_XDRAG(sheet)    (GTK_SHEET_FLAGS (sheet) & GTK_SHEET_IN_XDRAG)
#define GTK_SHEET_IN_YDRAG(sheet)    (GTK_SHEET_FLAGS (sheet) & GTK_SHEET_IN_YDRAG)
#define GTK_SHEET_IN_DRAG(sheet)     (GTK_SHEET_FLAGS (sheet) & GTK_SHEET_IN_DRAG)
#define GTK_SHEET_IN_SELECTION(sheet) (GTK_SHEET_FLAGS (sheet) & GTK_SHEET_IN_SELECTION)
#define GTK_SHEET_IN_RESIZE(sheet) (GTK_SHEET_FLAGS (sheet) & GTK_SHEET_IN_RESIZE)
#define GTK_SHEET_IN_CLIP(sheet) (GTK_SHEET_FLAGS (sheet) & GTK_SHEET_IN_CLIP)
#define GTK_SHEET_REDRAW_PENDING(sheet)   (GTK_SHEET_FLAGS (sheet) & GTK_SHEET_REDRAW_PENDING)
 
#define CELL_SPACING 1
#define DRAG_WIDTH 6
#define TIMEOUT_SCROLL 20
#define TIMEOUT_FLASH 200
#define TIME_INTERVAL 8
#define COLUMN_MIN_WIDTH 10
#define MINROWS 1
#define MINCOLS 1
#define MAXLENGTH 30
#define CELLOFFSET 4
#define DEFAULT_COLUMN_WIDTH 80
#define DEFAULT_SHEET_TITLE "untitled"

#include <gtksheetfeatures.h>
#include <gtkitementry.h>
#include <gtksheetwidget.h>

#endif /* GTK_EXTRA_H */

