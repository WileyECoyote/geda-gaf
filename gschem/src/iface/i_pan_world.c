/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_pan_world.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file i_pan_world.c
 * \brief This module provides Panning functions and zoom
 *  warping the cursor
 */

#include <gschem.h>
#include <math.h>
#include <geda_debug.h>

/*! \brief Auto Pan Scroll Down Source Function
 *  \par Function Description
 *   Called every #AUTO_PAN_INTERVAL milliseconds to scroll/pan the
 *   view port downwards, until doing_pan is FALSE. Calls i_pan_world_
 *   mouse with x = 0, y = - auto_pan_step.
 *
 *  \param [in] user_data  pointer to GschemToplevel data structure.
 */
static bool i_pan_auto_down(void *user_data)
{
  GschemToplevel *w_current = (GschemToplevel*)user_data;

  if (w_current->doing_pan) {
    i_pan_world_mouse(w_current, 0, - w_current->auto_pan_step);
  }
  return (w_current->doing_pan != 0);
}

/*! \brief Auto Pan Scroll Left Source Function
 *  \par Function Description
 *   Called every #AUTO_PAN_INTERVAL milliseconds to scroll/pan the
 *   view port westwards, until doing_pan is FALSE. Calls i_pan_world_
 *   mouse with x = + auto_pan_step, y = 0.
 *
 *  \param [in] user_data  pointer to GschemToplevel data structure.
 */
static bool i_pan_auto_left(void *user_data)
{
  GschemToplevel *w_current = (GschemToplevel*)user_data;

  if (w_current->doing_pan) {
    i_pan_world_mouse(w_current, w_current->auto_pan_step, 0);
  }
  return (w_current->doing_pan != 0);
}

/*! \brief Auto Pan Scroll Right Source Function
 *  \par Function Description
 *   Called every #AUTO_PAN_INTERVAL milliseconds to scroll/pan the
 *   view port eastwards, until doing_pan is FALSE. Calls i_pan_world_
 *   mouse with x = - auto_pan_step, y = 0.
 *
 *  \param [in] user_data  pointer to GschemToplevel data structure.
 */
static bool i_pan_auto_right(void *user_data)
{
  GschemToplevel *w_current = (GschemToplevel*)user_data;

  if (w_current->doing_pan) {
    i_pan_world_mouse(w_current, - w_current->auto_pan_step, 0);
  }
  return (w_current->doing_pan != 0);
}

/*! \brief Auto Pan Scroll Up Source Function
 *  \par Function Description
 *   Called every #AUTO_PAN_INTERVAL milliseconds to scroll/pan the
 *   view port upwards, until doing_pan is FALSE. Calls i_pan_world_
 *   mouse with x = 0, y = +auto_pan_step.
 *
 *  \param [in] user_data  pointer to GschemToplevel data structure.
 */
static bool i_pan_auto_up(void *user_data)
{
  GschemToplevel *w_current = (GschemToplevel*)user_data;

  if (w_current->doing_pan) {
    i_pan_world_mouse(w_current, 0, w_current->auto_pan_step);
  }
  return (w_current->doing_pan != 0);
}

/*! \brief Auto Pan On Leave Canvas Event Handler
 *  \par Function Description
 *   Determines which side of the canvas the pointer exited and
 *   adds a time-out source function to pan/scroll in the same
 *   direction at #AUTO_PAN_INTERVAL intervals.
 *
 *  \param [in] w_current  pointer to GschemToplevel data structure.
 *  \param [in] event      Structure describing the crossing event.
 *
 *  \sa i_event_leave
 */
void i_pan_auto(GschemToplevel *w_current, GdkEventCrossing *event)
{
  if (w_current->auto_pan) {

    if (!((int) event->x - w_current->screen_width) ) {
      w_current->doing_pan = 1;
      g_timeout_add(AUTO_PAN_INTERVAL, i_pan_auto_right, w_current);
    }
    else if (!((int) event->y - w_current->screen_height) ) {
      w_current->doing_pan = 1;
      g_timeout_add(AUTO_PAN_INTERVAL, i_pan_auto_down, w_current);
    }
    else if (!((int) event->y + 1) ) {
      w_current->doing_pan = 1;
      g_timeout_add(AUTO_PAN_INTERVAL, i_pan_auto_up, w_current);
    }
    else if (!((int) event->x + 1) ) {
      w_current->doing_pan = 1;
      g_timeout_add(AUTO_PAN_INTERVAL, i_pan_auto_left, w_current);
    }
  }
}

/*!
 * \brief Set Cursor/Pointer Position
 * \par Function Description
 *  This function sets the pointer position to relative screen coordinates
 *  of the given widget. For setting the cursor relative to the drawing
 *  area using world coordinates see i_window_set_pointer_position.
 *
 * \param [in] widget to which the coordinates will be relative
 * \param [in] x      integer abscissa in screen units
 * \param [in] y      integer ordinate in screen units
 *
 * \sa i_window_set_pointer_position
 */
void i_pan_warp_cursor (GtkWidget* widget, int x, int y)
{
  GdkScreen *screen;
  GdkDisplay *display;
  int window_x, window_y;

  gdk_window_get_origin (widget->window, &window_x, &window_y);

  screen  = gtk_widget_get_screen (widget);
  display = gdk_screen_get_display (screen);

  gdk_display_warp_pointer (display, screen, window_x + x, window_y + y);
}

/*!
 * \brief Pan World to X,Y Center with given Zoom Factor
 * \par Function Description
 *  Sets page left, right, top and bottom base on the center given by
 *  \a world_cx and \a world_cy and the given \a relative_zoom_factor.
 *  If the borders should be ignored include I_PAN_IGNORE_BORDERS in
 *  the \a flags. The function calls o_invalidate_all by default, to
 *  inhibit include I_PAN_DONT_REDRAW in the \a flags.
 *
 * \param [in] w_current            The GschemToplevel object
 * \param [in] page                 Page for which panning is to be performed
 * \param [in] world_cx             world value of X center point
 * \param [in] world_cy             world value of Y center point
 * \param [in] relative_zoom_factor Unit-less scale factor
 * \param [in] flags                Enumerated #EID_PAN_DIRECTIVES
 */
void i_pan_world_general (GschemToplevel *w_current,
                          Page           *page,
                          double          world_cx,
                          double          world_cy,
                          double          relative_zoom_factor,
                          int             flags)
{
  /* think it's better that the zoomfactor is defined as pix/mils
   * this will be the same as page->to_screen_x/y_constant */
  double zx, zy, zoom_old, zoom_new, zoom_min;

#if DEBUG
  printf("%s: world_cx=%f, world_cy=%f", __func__, world_cx, world_cy);
#endif

  /* calc minimum zoomfactors and choose the smaller one. They are equal
   * if the aspectratio of the world is the same as the screen ratio */
  zx = (double) w_current->screen_width  / (w_current->world_right  - w_current->world_left);
  zy = (double) w_current->screen_height / (w_current->world_bottom - w_current->world_top);
  zoom_min = zx < zy ? zx : zy;

#if DEBUG
  printf(" zx_min=%f, zy_min=%f , flags=%d\n ", zx, zy, flags);
#endif

  /* to_screen_x_constant and to_screen_y_constant are almost the same,
   * so lets use to_screen_y_constant */
  zoom_old = page->to_screen_y_constant;

  /* calc new zooming factor */
  /* check if there's a zoom_full (relative_zoom_factor == -1) */
  if (relative_zoom_factor < 0)  {
    zoom_new = zoom_min;
  }
  else {

    int zoom_max = MAX_ZOOM_FACTOR;

    zoom_new = zoom_old * relative_zoom_factor;
    zoom_new = zoom_new > zoom_max ? zoom_max : zoom_new;

    if (!(flags & I_PAN_IGNORE_BORDERS)) {
      zoom_new = zoom_new < zoom_min ? zoom_min : zoom_new;
    }
  }

  /* calculate the new visible area; adding 0.5 to round */
  int page_left   = world_cx - (double) w_current->screen_width / 2 / zoom_new + 0.5;
  int page_right  = world_cx + (double) w_current->screen_width / 2 / zoom_new + 0.5;
  int page_top    = world_cy - (double) w_current->screen_height / 2 / zoom_new + 0.5;
  int page_bottom = world_cy + (double) w_current->screen_height / 2 / zoom_new + 0.5;

  /* and put it back to the borders */
  if (!(flags & I_PAN_IGNORE_BORDERS)) {

    int diff;

    /* check right border */
    if (page_right > w_current->world_right) {
        page_left += w_current->world_right - page_right;
        page_right = w_current->world_right;
    }

    /* check left border */
    if (page_left < w_current->world_left) {
        page_right += w_current->world_left - page_left;
        page_left   = w_current->world_left;
    }

    /* If there is any slack, center the view */
    diff = (page_right - page_left) - (w_current->world_right - w_current->world_left);

    if (diff > 0) {
        page_left  -= diff >> 1;  /* Divide by 2 */
        page_right -= diff >> 1;
    }

    /* check bottom border */
    if (page_bottom > w_current->world_bottom) {
        page_top   += w_current->world_bottom - page_bottom;
        page_bottom = w_current->world_bottom;
    }

    /* check top border */
    if (page_top < w_current->world_top) {
        page_bottom += w_current->world_top - page_top;
        page_top     = w_current->world_top;
    }

      /* If there is any slack, center the view */
    diff = (page_bottom - page_top) - (w_current->world_bottom - w_current->world_top);
    if (diff > 0) {
        page_top    -= diff >> 1;  /* Divide by 2 */
        page_bottom -= diff >> 1;
    }
  }

#if DEBUG
  printf("zoom_old: %f, zoom_new: %f \n ",zoom_old, zoom_new);
  printf("left: %d, right: %d, top: %d, bottom: %d\n",
         page_left, page_right, page_top, page_bottom);
  printf("aspect: %f\n", (float) fabs(page_right - page_left) /
         (float) fabs(page_bottom - page_top ));
#endif

  x_window_setup_page(w_current, page,
                                 page_left,
                                 page_right,
                                 page_top,
                                 page_bottom);

  /* update the status bar if the zoom changed */
  if (zoom_new != zoom_old && w_current->status_bar) {
    i_status_update_grid_info (w_current);
  }

  /* redraw */
  if (!(flags & I_PAN_DONT_REDRAW)) {
    x_scrollbars_update(w_current);
    o_invalidate_all (w_current);
  }
}

/*! \brief Pan and zoom the Drawing Area
 *  \par Function Description
 *   Pan the view by the given amount.
 *
 *  \param [in] w_current  Pointer to #GschemToplevel Object
 *  \param [in] w_x        X distance to pan in world units
 *  \param [in] w_y        Y distance to pan in world units
 */
void i_pan_world(GschemToplevel *w_current, int w_x, int w_y)
{
  /* make mouse to the new world-center;
   *    attention: there are information looses because of type cast in mil_x */

  i_pan_world_general(w_current, Current_Page, w_x, w_y, 1, 0);

  /*! \bug FIXME? This call will trigger a motion event (x_event_motion()),
   * even if the user doesn't move the mouse
   * Not ready for prime time, maybe there is another way to trigger the
   * motion event without changing the cursor position (Werner)
   */
  /* i_pan_warp_cursor(w_current->drawing_area, x, y); */
}

/*! \brief Pan to Mouse
 *  \par Function Description
 *  Calculates coordinate to pan the view by the given amount.
 *
 *  \param [in] w_current  Pointer to #GschemToplevel Object
 *  \param [in] diff_x     X pan displacment in world units
 *  \param [in] diff_y     Y pan displacment in world units
 */
void i_pan_world_mouse(GschemToplevel *w_current, int diff_x, int diff_y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  double world_cx, world_cy;
  double page_cx, page_cy;

#if DEBUG
  printf("%s: diff_x=%d, diff_y=%d", __func__, diff_x, diff_y);
#endif

  page_cx = (toplevel->page_current->left + toplevel->page_current->right) / 2.0;
  page_cy = (toplevel->page_current->top + toplevel->page_current->bottom) / 2.0;

  world_cx = page_cx - WORLDabs (w_current, diff_x);
  world_cy = page_cy + WORLDabs (w_current, diff_y);

#if DEBUG
  printf("  world_cx=%f, world_cy=%f\n",
     world_cx, world_cy);
#endif

  i_pan_world_general(w_current, toplevel->page_current, world_cx, world_cy, 1, 0);
}
