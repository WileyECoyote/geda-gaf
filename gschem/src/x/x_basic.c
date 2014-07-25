/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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

#include "gschem.h"
#include <geda_debug.h>

/*! \brief Repaint Background Region
 *  \par Function Description
 *  This function is called by o_redraw_rects to redraw the grid
 *  within a rectangular region given by specific parameters.
 *
 */
void x_repaint_background_region (GschemToplevel *w_current,
                                  int x, int y, int width, int height)
{
  gdk_gc_set_foreground (w_current->gc,
                         x_get_color (w_current->background_color));

  gdk_draw_rectangle (w_current->drawable,
                      w_current->gc, TRUE, x, y, width, height);

  x_grid_draw_region (w_current, x, y, width, height);
}

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

/* WEH: Credit for the next two functions goes to The Geeqie Team, I added
 * the test for iter.stamp to surpress gtk's non-sense.
 *
 * (SLIK) SimpLIstic sKin functions
 * (C) 2004 John Ellis
 * Copyright (C) 2004 - 2014 The Geeqie Team
 *
 * Author: John Ellis   http://geeqie.sourceforge.net/
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

/*! \brief Get visibility of Row in GtkTreeView
 *  \par Function Description
 *  This function is primarily used by the next function to determine if
 *  a row in a scrollable view is centered.
 */
int tree_view_row_get_visibility(GtkTreeView *tree_view, GtkTreeIter *iter,
                                 bool fully_visible)
{
  GtkTreeModel *model;
  GtkTreePath *path, *start_path, *end_path;
  int ret = 0;
  bool valid;

  valid = ( iter->stamp != 0 ? TRUE : FALSE);

  if (valid) {
    if (!gtk_tree_view_get_visible_range(tree_view, &start_path, &end_path)) return -1;
    /* we will most probably scroll down, needed for tree_view_row_make_visible */

    model = gtk_tree_view_get_model(tree_view);
    path = gtk_tree_model_get_path(model, iter);

    if (fully_visible) {
      if (gtk_tree_path_compare(path, start_path) <= 0) {
        ret = -1;
      }
      else if (gtk_tree_path_compare(path, end_path) >= 0) {
        ret = 1;
      }
    }
    else {
      if (gtk_tree_path_compare(path, start_path) < 0) {
        ret = -1;
      }
      else
        if (gtk_tree_path_compare(path, end_path) > 0) {
          ret = 1;
        }
    }

    gtk_tree_path_free(path);
    gtk_tree_path_free(start_path);
    gtk_tree_path_free(end_path);
  }
  else ret = -1;
  return ret;
}

/*! \brief Make Tree Row Visible in GtkTreeView
 *  \par Function Description
 *  This function provides a reliable method to adjust tree views to
 *  positions. GTK lacks comparable functionality. Don't bother with
 *  gtk_tree_view_set_cursor.
 */
int tree_view_row_make_visible(GtkTreeView *tree_view, GtkTreeIter *iter,
                               bool center)
{
  GtkTreePath *path;
  int visible;
  bool valid;

  valid = ( iter->stamp != 0 ? TRUE : FALSE);

  if (valid) {
  visible = tree_view_row_get_visibility(tree_view, iter, TRUE);

  path = gtk_tree_model_get_path(gtk_tree_view_get_model(tree_view), iter);
  if (center && visible != 0) {
    gtk_tree_view_scroll_to_cell(tree_view, path, NULL, TRUE, 0.5, 0.0);
  }
  else
    if (visible < 0) {
      gtk_tree_view_scroll_to_cell(tree_view, path, NULL, TRUE, 0.0, 0.0);
    }
    else if (visible > 0) {
      gtk_tree_view_scroll_to_cell(tree_view, path, NULL, TRUE, 1.0, 0.0);
    }

  gtk_tree_path_free(path);

  }
  else {
    visible = -1;
  }
  return visible;
}