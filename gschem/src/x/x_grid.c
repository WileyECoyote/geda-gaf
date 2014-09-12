/* -*- C x_grid.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
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
/*!
 * \file x_grid.c
 * \brief Main Window Auxiliary Module for Grid systems applied to drawing area
 */

#include <math.h>

#include "gschem.h"
#include <geda_debug.h>

/** \defgroup grid-module Grid Module
 *  @{\brief This group contains functions for Grid systems
 *    \ingroup (main-window)
*/

#define DOTS_POINTS_ARRAY_SIZE       5000
#define DOTS_VARIABLE_MODE_SPACING   30
#define COARSE_GRID_MULTIPLIER  5
#define TILES_FONT_SIZE              21

/*! \brief Query the spacing in world coordinates at which the dots grid is drawn.
 *
 *  \par Function Description
 *  Returns the world spacing of the rendered grid, taking into account where
 *  the grid drawing code may drop elelments which are too densly packed for a
 *  given zoom level.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \returns The grid spacing in world units of the grid as rendered, or -1
 *           if there are no items drawn.
 */
static int query_dots_grid_spacing (GschemToplevel *w_current)
{
  int incr, screen_incr;

  g_return_val_if_fail(w_current->toplevel != NULL, -1);

  if (GEDA_IS_PAGE(Current_Page)) {

    if (w_current->dots_grid_mode == DOTS_GRID_VARIABLE_MODE) {

      /* In the variable mode around every (DOTS_VARIABLE_MODE_SPACING)'th
       * screenpixel will be grid-point. Adding 0.1 for correct cast*/
      incr = m_round_5_2_1 (Current_Page->to_world_x_constant *
                            DOTS_VARIABLE_MODE_SPACING) + 0.1;

      /* limit minimum grid spacing to grid to snap_size */
      if (incr < w_current->snap_size) {
        incr = w_current->snap_size;
      }
    }
    else {

      /* Fixed size grid in world coorinates */
      incr        = w_current->snap_size;
      screen_incr = SCREENabs (w_current, incr);

      if (screen_incr < w_current->dots_grid_threshold) {
        /* No grid drawn if the on-screen spacing is less than the threshold */
        incr = -1;
      }
    }
  }
  else {
    BUG_MSG("page_current == NULL");
    incr = -1;
  }
  return incr;
}

/*! \brief Query the spacing in world coordinates at which the mesh grid is drawn.
 *
 *  \par Function Description
 *  Returns the world spacing of the rendered grid, taking into account where
 *  the grid drawing code may drop elelments which are too densly packed for a
 *  given zoom level.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \returns The grid spacing in world units of the grid as rendered, or -1
 *           if there are no items drawn.
 */
static int query_mesh_grid_spacing (GschemToplevel *w_current)
{
  int incr, screen_incr;

  incr = w_current->snap_size;
  screen_incr = SCREENabs (w_current, incr);

  /* We draw a fine grid if its on-screen spacing is large enough */
  if (screen_incr >= w_current->mesh_grid_threshold) {
    return incr;
  }

  incr *= COARSE_GRID_MULTIPLIER;
  screen_incr = SCREENabs (w_current, incr);

  /* We draw a coarse grid if its on-screen spacing is large enough */
  if (screen_incr >= w_current->mesh_grid_threshold)
    return incr;

  return -1;
}

/*! \brief Draw an area of the screen with a dotted grid pattern
 *
 *  \par Function Description
 *  Draws the dotted grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
 *  \param [in] height     The height of the region to draw.
 */
static void
draw_dots_grid_region (GschemToplevel *w_current,
                       int x, int y, int width, int height)
{
  int i, j;
  int dot_x, dot_y;
  int x_start, y_start, x_end, y_end;
  int count = 0;
  GdkPoint points[DOTS_POINTS_ARRAY_SIZE];

  int incr = query_dots_grid_spacing (w_current);

  if (incr == -1)
  return;

  gdk_gc_set_foreground (w_current->gc, x_get_color (DOTS_GRID_COLOR));

  SCREENtoWORLD (w_current, x - 1, y + height + 1, &x_start, &y_start);
  SCREENtoWORLD (w_current, x + width + 1, y - 1, &x_end, &y_end);

  /* figure starting grid coordinates, work by taking the start
   * and end coordinates and rounding down to the nearest increment */
  x_start -= (x_start % incr);
  y_start -= (y_start % incr);

  for (i = x_start; i <= x_end; i = i + incr) {

    for(j = y_start; j <= y_end; j = j + incr) {

      WORLDtoSCREEN (w_current, i,j, &dot_x, &dot_y);

      if (inside_region (Current_Page->left,
                         Current_Page->top,
                         Current_Page->right,
                         Current_Page->bottom, i, j)) {

        if (w_current->dots_grid_dot_size == 1) {
          points[count].x = dot_x;
          points[count].y = dot_y;
          count++;

          /* get out of loop if we're hit the end of the array */
          if (count == DOTS_POINTS_ARRAY_SIZE) {
            gdk_draw_points (w_current->drawable,
                             w_current->gc, points, count);
            count = 0;
          }
        }
        else {
          gdk_draw_arc (w_current->drawable, w_current->gc,
                        TRUE, dot_x, dot_y,
                        w_current->dots_grid_dot_size,
                        w_current->dots_grid_dot_size, 0, FULL_CIRCLE);
        }
      }
    }
  }

  /* now draw all the points in one step */
  if(count != 0) {
    gdk_draw_points (w_current->drawable, w_current->gc, points, count);
  }
}


/*! \brief Helper function for draw_mesh_grid_regin
 */
static void draw_mesh (GschemToplevel *w_current,
                       int x_start, int y_start, int x_end, int y_end,
                       int incr, int coarse_mult)
{
  int i, j;
  int x1, y1, x2, y2;
  int next_coarse_x, next_coarse_y;
  int coarse_incr = incr * coarse_mult;

  /* figure starting grid coordinates, work by taking the start
   * and end coordinates and rounding down to the nearest increment */
  x_start -= (x_start % incr);
  y_start -= (y_start % incr);

  if (coarse_incr == 0) {
    next_coarse_x = x_start - 1; /* Ensures we never hit this when looping */
    next_coarse_y = y_start - 1; /* Ensures we never hit this when looping */
  }
  else {
    next_coarse_x = x_start - (x_start % coarse_incr);
    next_coarse_y = y_start - (y_start % coarse_incr);
    if (next_coarse_x < x_start) next_coarse_x += coarse_incr;
    if (next_coarse_y < y_start) next_coarse_y += coarse_incr;
  }

  /* Draw the horizontal grid lines */
  for (j = y_start; j < y_end; j = j + incr) {

    /* Skip lines which will be drawn in the coarser grid */
    if (j == next_coarse_y) {
      next_coarse_y += coarse_incr;
      continue;
    }

    WORLDtoSCREEN (w_current, x_start, j, &x1, &y1);
    WORLDtoSCREEN (w_current, x_end,   j, &x2, &y2);
    cairo_move_to (w_current->cr, x1, y1);
    cairo_line_to (w_current->cr, x2, y2);

  }
  cairo_stroke (w_current->cr);

  /* Draw the vertical grid lines  */
  for (i = x_start; i < x_end; i = i + incr) {

    /* Skip lines which will be drawn in the coarser grid */
    if (i == next_coarse_x) {
      next_coarse_x += coarse_incr;
      continue;
    }

    WORLDtoSCREEN (w_current, i, y_start, &x1, &y1);
    WORLDtoSCREEN (w_current, i, y_end,   &x2, &y2);
    cairo_move_to (w_current->cr, x1, y1);
    cairo_line_to (w_current->cr, x2, y2);

  }
  cairo_stroke (w_current->cr);

}

/*! \brief Draw an area of the screen with a mesh grid pattern
 *
 *  \par Function Description
 *  Draws the mesh grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
 *  \param [in] height     The height of the region to draw.
 */
static void
draw_mesh_grid_region (GschemToplevel *w_current, int x, int y, int width, int height)
{
  edaColor *c;
  int x_start, y_start, x_end, y_end;
  int incr;
  int screen_incr;

  incr        = w_current->snap_size;
  screen_incr = SCREENabs (w_current, incr);

  SCREENtoWORLD (w_current, x - 1, y + height + 1, &x_start, &y_start);
  SCREENtoWORLD (w_current, x + width + 1, y - 1, &x_end, &y_end);

  cairo_set_line_width (w_current->cr, w_current->grid_size_factor);

  cairo_set_line_cap (w_current->cr, CAIRO_LINE_CAP_SQUARE);

  /** Draw the fine grid if its on-screen spacing is large enough **/
  if (screen_incr >= w_current->mesh_grid_threshold) {

    c = &w_current->grid_minor_color;

    cairo_set_source_rgba (w_current->cr, c->r, c->g, c->b, c->a);

    draw_mesh (w_current, x_start, y_start,
               x_end, y_end, incr, COARSE_GRID_MULTIPLIER);
  }

  incr       *= COARSE_GRID_MULTIPLIER;
  screen_incr = SCREENabs (w_current, incr);

  /** Draw the coarse grid if its on-screen spacing is large enough **/
  if (screen_incr >= w_current->mesh_grid_threshold) {

    c = &w_current->grid_major_color;

    cairo_set_source_rgba (w_current->cr, c->r, c->g, c->b, c->a);

    draw_mesh (w_current, x_start, y_start, x_end, y_end, incr, 0);
  }
}

/*! \brief Draw an area of the screen with the current grid pattern.
 *
 *  \par Function Description
 *  Draws the desired grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
 *  \param [in] height     The height of the region to draw.
 */
void
x_grid_draw_region (GschemToplevel *w_current, int x, int y, int width, int height)
{
  switch (w_current->grid_mode) {
    case GRID_NONE:
      return;

    case GRID_DOTS:
      draw_dots_grid_region (w_current, x, y, width, height);
      break;

    case GRID_MESH:
      draw_mesh_grid_region (w_current, x, y, width, height);
      break;
  }

#if DEBUG_TILES
  /* For diagnostic purposes */
  x_draw_tiles(w_current);
#endif

}

#if DEBUG_GRID
static void x_grid_print_parameters (GschemToplevel *w_current, char *when)
{
  printf("%s on %s:\n", __func__, when);
  printf("mesh_grid_minor_color: ");
  printf("\tred=%d ",    w_current->mesh_grid_minor_color.red);
  printf("\tgreen=%d ",  w_current->mesh_grid_minor_color.green);
  printf("\tblue=%d\n",  w_current->mesh_grid_minor_color.blue);

  printf("grid_minor_color:");
  printf("\tred=%f ",    w_current->grid_minor_color.r);
  printf("\tgreen=%f ",  w_current->grid_minor_color.g);
  printf("\tblue=%f",    w_current->grid_minor_color.b);
  printf("\talpha=%f\n", w_current->grid_minor_color.a);

  printf("mesh_grid_major_color: ");
  printf("\tred=%d ",    w_current->mesh_grid_major_color.red);
  printf("\tgreen=%d ",  w_current->mesh_grid_major_color.green);
  printf("\tblue=%d\n",  w_current->mesh_grid_major_color.blue);

  printf("grid_minor_color: ");
  printf("\tred=%f ",    w_current->grid_major_color.r);
  printf("\tgreen=%f ",  w_current->grid_major_color.g);
  printf("\tblue=%f",    w_current->grid_major_color.b);
  printf("\talpha=%f\n", w_current->grid_major_color.a);

  printf("grid_size_factor=%f\n", w_current->grid_size_factor);
  printf("\n");
}
#endif

/*! \brief Configure Grid Variables for the Current Grid Mode
 *
 *  \par Function Description
 *  This function sets up toplevel variables used by the grid system
 *  for the grid mode and must be called if the grid color settings
 *  are changed. Numerical adjustments are made to data here so that
 *  the computations are performed outside of the expose event loop.
 *
 *  \param [in] w_current  The GschemToplevel
 *
 */
void x_grid_configure_variables (GschemToplevel *w_current)
{
  double red;
  double green;
  double blue;

#if DEBUG_GRID
  x_grid_print_parameters (w_current, "entry");
#endif

  /* mesh_grid_minor_color is a GdkColor structure */
  red   = w_current->mesh_grid_minor_color.red   / 65535.0;
  green = w_current->mesh_grid_minor_color.green / 65535.0;
  blue  = w_current->mesh_grid_minor_color.blue  / 65535.0;

  w_current->grid_minor_color.r  = red;
  w_current->grid_minor_color.g  = green;
  w_current->grid_minor_color.b  = blue;

  w_current->grid_minor_color.a  = w_current->mesh_grid_minor_alpha * 0.01;

  red   = w_current->mesh_grid_major_color.red   / 65535.0;
  green = w_current->mesh_grid_major_color.green / 65535.0;
  blue  = w_current->mesh_grid_major_color.blue  / 65535.0;

  w_current->grid_major_color.r  = red;
  w_current->grid_major_color.g  = green;
  w_current->grid_major_color.b  = blue;

  w_current->grid_major_color.a  = w_current->mesh_grid_major_alpha * 0.01;

  w_current->grid_size_factor    = w_current->mesh_line_width_factor * 0.01;

#if DEBUG_GRID
  x_grid_print_parameters (w_current, "exit");
#endif
}

/*! \brief Query the spacing in world coordinates at which the grid is drawn.
 *
 *  \par Function Description
 *  Returns the world spacing of the rendered grid, taking into account where
 *  the grid drawing code may drop elelments which are too densly packed for a
 *  given zoom level.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \returns The grid spacing in world units of the grid as rendered, or -1
 *           if there are no items drawn.
 */
int x_grid_query_drawn_spacing (GschemToplevel *w_current)
{
  switch (w_current->grid_mode) {
    default:
    case GRID_NONE: return -1;
    case GRID_DOTS: return query_dots_grid_spacing (w_current);
    case GRID_MESH: return query_mesh_grid_spacing (w_current);
  }
}

/*! \brief Draw tile grid pattern
 *
 *  \par Function Description
 *  Draws the tile grid pattern over the screen area.
 *
 *  \param [in] w_current  The GschemToplevel.
 */
void x_draw_tiles(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  TILE         *t_current;
  char         *tempstring;
  int i,j;
  int x1, y1, x2, y2;
  int screen_x, screen_y;
  int width, height;

  GdkColor *color;

  color = x_get_color (LOCK_COLOR);

  cairo_set_source_rgb (w_current->cr,
                        color->red   / 65535.0,
                        color->green / 65535.0,
                        color->blue  / 65535.0);

  cairo_select_font_face (w_current->cr,
                          "Sans",
                          CAIRO_FONT_SLANT_NORMAL,
                          CAIRO_FONT_WEIGHT_BOLD);

  for (j = 0; j < MAX_TILES_Y; j++) {

    for (i = 0; i < MAX_TILES_X; i++) {

      t_current = &toplevel->page_current->world_tiles[i][j];

      WORLDtoSCREEN (w_current, t_current->left, t_current->top, &x1, &y1);
      WORLDtoSCREEN (w_current, t_current->right, t_current->bottom, &x2, &y2);

      screen_x = min(x1, x2);
      screen_y = min(y1, y2);

      width  = abs(x1 - x2);
      height = abs(y1 - y2);

      cairo_rectangle (w_current->cr, screen_x, screen_y, width, height);

      cairo_stroke(w_current->cr);

      cairo_save (w_current->cr);

      cairo_translate (w_current->cr, screen_x,  screen_y);

      cairo_move_to (w_current->cr, TILES_FONT_SIZE / 7, TILES_FONT_SIZE / 1.1);

      cairo_set_font_size (w_current->cr, TILES_FONT_SIZE);

      tempstring = u_string_sprintf ("(%d,%d)", i, j);

      cairo_show_text (w_current->cr, tempstring);

      cairo_restore (w_current->cr);

      GEDA_FREE(tempstring);
    }
  }
}

/** @} endgroup grid-module */
