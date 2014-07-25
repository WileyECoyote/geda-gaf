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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include <math.h>
#include <geda_debug.h>

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  see gschem_idefines.h for flags
 *  if the borders should be ignored always, remove, outcomment or changes
 *  the flags in the function-calls flags |= I_PAN_IGNORE_BORDERS;
 */
void i_pan_world_general(GschemToplevel *w_current, double world_cx, double world_cy,
                         double relative_zoom_factor,int flags)
{
  GedaToplevel *toplevel = w_current->toplevel;

  /* think it's better that the zoomfactor is defined as pix/mills
   *    this will be the same as w_current->page_current->to_screen_x/y_constant*/
  int zoom_max = 5;
  int diff;
  double zx, zy, zoom_old, zoom_new, zoom_min;

  #if DEBUG
  printf("i_pan_world_general(): world_cx=%f, world_cy=%f\n",world_cx, world_cy);
  #endif

  /* calc minimum zoomfactors and choose the smaller one. They are equal
   *    if the aspectratio of the world is the same as the screen ratio */
  zx = (double) w_current->screen_width / (w_current->world_right - w_current->world_left);
  zy = (double) w_current->screen_height / (w_current->world_bottom - w_current->world_top);
  zoom_min = zx < zy ? zx : zy;

  #if DEBUG
  printf("  zx_min=%f, zy_min=%f , flags=%d\n ",zx, zy, flags);
  #endif

  /* to_screen_x_constant and to_screen_y_constant are almost the same.
   *    lets use to_screen_y_constant */
  zoom_old = toplevel->page_current->to_screen_y_constant;

  /* calc new zooming factor */
  /* check if there's a zoom_full (relative_zoom_factor == -1) */
  if (relative_zoom_factor < 0)  {
    zoom_new = zoom_min;
  }
  else {
    zoom_new = zoom_old * relative_zoom_factor;
    zoom_new = zoom_new > zoom_max ? zoom_max : zoom_new;
    if (!(flags & I_PAN_IGNORE_BORDERS)) {
      zoom_new = zoom_new < zoom_min ? zoom_min : zoom_new;
    }
  }

  /* calculate the new visible area; adding 0.5 to round */
  toplevel->page_current->left = world_cx - (double) w_current->screen_width
  / 2 / zoom_new + 0.5;
  toplevel->page_current->right = world_cx + (double) w_current->screen_width
  / 2 / zoom_new + 0.5;
  toplevel->page_current->top = world_cy - (double) w_current->screen_height
  / 2 / zoom_new + 0.5;
  toplevel->page_current->bottom = world_cy + (double) w_current->screen_height
  / 2 / zoom_new + 0.5;

  /* and put it back to the borders */
  if (!(flags & I_PAN_IGNORE_BORDERS)) {
    /* check right border */
    if (toplevel->page_current->right > w_current->world_right) {
      toplevel->page_current->left += w_current->world_right -
      toplevel->page_current->right;
      toplevel->page_current->right = w_current->world_right;
    }
    /* check left border */
    if (toplevel->page_current->left < w_current->world_left) {
      toplevel->page_current->right += w_current->world_left - toplevel->page_current->left;
      toplevel->page_current->left = w_current->world_left; }

    /* If there is any slack, center the view */
    diff = (toplevel->page_current->right -
    toplevel->page_current->left) -
    (w_current->world_right - w_current->world_left);
    if (diff > 0) {
      toplevel->page_current->left -= diff / 2;
      toplevel->page_current->right -= diff / 2;
    }

    /* check bottom border */
    if (toplevel->page_current->bottom > w_current->world_bottom) {
      toplevel->page_current->top += w_current->world_bottom -
      toplevel->page_current->bottom;
      toplevel->page_current->bottom = w_current->world_bottom;
    }
    /* check top border */
    if (toplevel->page_current->top < w_current->world_top) {
      toplevel->page_current->bottom += w_current->world_top -
      toplevel->page_current->top;
      toplevel->page_current->top = w_current->world_top;
    }

    /* If there is any slack, center the view */
    diff = (toplevel->page_current->bottom -
    toplevel->page_current->top) -
    (w_current->world_bottom - w_current->world_top);
    if (diff > 0) {
      toplevel->page_current->top -= diff / 2;
      toplevel->page_current->bottom -= diff / 2;
    }

  }

  #if DEBUG
  printf("zoom_old: %f, zoom_new: %f \n ",zoom_old, zoom_new);
  printf("left: %d, right: %d, top: %d, bottom: %d\n",
         toplevel->page_current->left, toplevel->page_current->right,
         toplevel->page_current->top, toplevel->page_current->bottom);
  printf("aspect: %f\n",
         (float) fabs(toplevel->page_current->right
         - toplevel->page_current->left) /
         (float) fabs(toplevel->page_current->bottom
         - toplevel->page_current->top ));
  #endif

  /* x_window_setup_page */
  x_window_setup_page(w_current, toplevel->page_current,
             toplevel->page_current->left  ,
             toplevel->page_current->right ,
             toplevel->page_current->top   ,
             toplevel->page_current->bottom);

  /* update the status bar if the zoom changed */
  if( zoom_new  != zoom_old) {
    i_update_grid_info (w_current);
  }

  /* redraw */
  if (!(flags & I_PAN_DONT_REDRAW)) {
    x_scrollbars_update(w_current);
    o_invalidate_all (w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void i_pan_world(GschemToplevel *w_current, int w_x, int w_y)
{
  /* make mouse to the new world-center;
   *    attention: there are information looses because of type cast in mil_x */

  i_pan_world_general(w_current, w_x, w_y, 1, 0);

  /*! \bug FIXME? This call will trigger a motion event (x_event_motion()),
   * even if the user doesn't move the mouse
   * Not ready for prime time, maybe there is another way to trigger the
   * motion event without changing the cursor position (Werner)
   */
  /* x_basic_warp_cursor(w_current->drawing_area, x, y); */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_pan_world_mouse(GschemToplevel *w_current, int diff_x, int diff_y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  double world_cx, world_cy;
  double page_cx, page_cy;

#if DEBUG
  printf("i_pan_world_mouse(): diff_x=%d, diff_y=%d\n", diff_x, diff_y);
#endif

  page_cx = (toplevel->page_current->left + toplevel->page_current->right) / 2.0;
  page_cy = (toplevel->page_current->top + toplevel->page_current->bottom) / 2.0;

  world_cx = page_cx - WORLDabs (w_current, diff_x);
  world_cy = page_cy + WORLDabs (w_current, diff_y);

#if DEBUG
  printf("  world_cx=%f, world_cy=%f\n",
     world_cx, world_cy);
#endif

  i_pan_world_general(w_current, world_cx, world_cy, 1, 0);
}
