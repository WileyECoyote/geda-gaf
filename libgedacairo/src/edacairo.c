/* -*- C edacairo.c indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-
 *
 * File: edacairo.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedacairo - Rendering gEDA schematics with Cairo
 *
 * Copyright (C) 2010-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#include <config.h>

/* GNU Compiler Headers */
#include <stdint.h>
#include <math.h>

/* GTK Headers */
#include <glib.h>
#include <stdio.h>

#include <cairo.h>

#include <geda/geda.h>
#include <libgeda/s_struct.h>

#include "../include/edacairo.h"

static inline int screen_width(cairo_t *cr, double width)
{
  double dummy = 0;
  cairo_user_to_device_distance (cr, &width, &dummy);
  if (width < 1)
    width = 1;

#ifdef HAVE_RINT
   return rint (width);
#else
   return width + 0.5;
#endif
}

static inline int SCREENabs (cairo_t *cr, double dist)
{
  double dummy = 0;
  cairo_user_to_device_distance (cr, &dist, &dummy);
  return rint (dist);
}

static inline void
WORLDtoSCREEN (cairo_t *cr, double wx, double wy, double *sx, double *sy)
{
  cairo_user_to_device (cr, &wx, &wy);
  *sx = round (wx); *sy = round (wy);
}

/*!
 * \brief Render Cairo Arc Common
 * \par Function Description
 *  Common routine to call cairo arc.
 */
static inline void
do_arc (cairo_t *cr, double x, double y, double radius,
                     double start_angle, double arc_sweep)
{
  cairo_new_sub_path (cr);

  if (arc_sweep > 0) {
    cairo_arc (cr, x, y, radius, start_angle * (M_PI / 180.),
                   (start_angle + arc_sweep) * (M_PI / 180.));
  }
  else {
    cairo_arc_negative (cr, x, y, radius, start_angle * (M_PI / 180.),
                            (start_angle + arc_sweep) * (M_PI / 180.));
  }
}

/*!
 * \brief Render an Arc at center point using Cairo Graphic
 * \par Function Description
 *  Scale coordinates of centerpoint to screen coordinates, calculate
 *  endpoint and render using Cairo graphics library.
 *
 * \param [in]  cr           EdaRenderer Cairo Context
 * \param [in]  flags        EdaRenderer Cairo Flags
 * \param [in]  line_width   Thickness of the arc line
 * \param [in]  x            Arc center X
 * \param [in]  y            Arc center Y
 * \param [in]  radius       Radius of the arc
 * \param [in]  start_angle  Starting angle in degrees
 * \param [in]  arc_sweep    Arc sweep angle in degrees
 */
void
eda_cairo_arc (cairo_t *cr, int flags,
               double line_width, double x, double y,
               double radius, double start_angle, double arc_sweep)
{
  int s_width;
  double x1, y1, x2, y2;
  double s_x, s_y, s_radius;
  double offset;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    do_arc (cr, x, y, radius, start_angle, arc_sweep);
    return;
  }

  WORLDtoSCREEN (cr, x - radius, y + radius, &x1, &y1);
  WORLDtoSCREEN (cr, x + radius, y - radius, &x2, &y2);

  s_width  = screen_width (cr, line_width);
  offset   = ((s_width % 2) == 0) ? 0 : 0.5;

  s_x      = (double)(x1 + x2) / 2.0;
  s_y      = (double)(y1 + y2) / 2.0;
  s_radius = (double)(y2 - y1) / 2.0;

  cairo_device_to_user (cr, &s_x, &s_y);
  cairo_device_to_user_distance (cr, &offset, &s_radius);

  s_radius = -1 * s_radius;

  do_arc (cr, s_x + offset, s_y + offset, s_radius, start_angle, arc_sweep);
}

/*! \brief Render an Arc at center point using Cairo Graphic
 *  \par Function Description
 *  Scale coordinates of centerpoint to screen coordinates, calculate
 *  endpoint and render using Cairo graphics library.
 */
void
eda_cairo_center_arc (cairo_t *cr, int flags,
                      double center_width,
                      double line_width, double x, double y,
                      double radius, double start_angle, double arc_sweep)
{
  int s_center_width, s_line_width;
  double s_x, s_y, dummy = 0;
  int s_diameter;
  double even_center_width;
  double even_line_width;
  double even_diameter;
  double center_offset;
  double s_radius;
  int do_radius_hint = TRUE;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    do_arc (cr, x, y, radius, start_angle, arc_sweep);
    return;
  }

  WORLDtoSCREEN (cr, x, y, &s_x, &s_y);
  s_diameter = SCREENabs (cr, 2 * radius);
  even_diameter = ((s_diameter % 2) == 0);
  s_radius = (double) s_diameter / 2.;

  /* Switch off radius hinting for small radii. If we don't, then we get
   * a very abrupt transition once the arc reaches a single pixel size. */
  if (s_radius <= 1.) do_radius_hint = FALSE;

  /* Hint the center of the arc based on where a line
   * of thickness center_width (world) would drawn */
  s_center_width = screen_width (cr, center_width);
  even_center_width = (center_width == -1 || (s_center_width % 2) == 0);
  center_offset = even_center_width ? 0. : 0.5;

  /* Hint the radius to land its extermity on the pixel grid */
  s_line_width = screen_width (cr, line_width);
  even_line_width = (line_width == -1 || (s_line_width % 2) == 0);
  if (do_radius_hint)
    s_radius += ((even_center_width ==
                        even_line_width) == even_diameter) ? 0. : 0.5;

  s_x += center_offset;
  s_y += center_offset;
  cairo_device_to_user (cr, &s_x, &s_y);
  cairo_device_to_user_distance (cr, &s_radius, &dummy);

  do_arc (cr, s_x, s_y, s_radius, start_angle, arc_sweep);
}


/*!
 * \brief Render a Rectangular Box using Cairo Graphic
 * \par Function Description
 *  Scale coordinates of vertices to screen coordinates and render a box
 *  using Cairo graphics library.
 */
void
eda_cairo_box (cairo_t *cr, int flags, double line_width,
               double x1, double y1, double x2, double y2)
{
  int s_line_width;
  double s_x1, s_y1, s_x2, s_y2;
  double offset;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    /* outline box */
    cairo_rectangle (cr, x1, y1, (x2 - x1), (y2 - y1));
    return;
  }

  WORLDtoSCREEN (cr, x1, y1, &s_x1, &s_y1);
  WORLDtoSCREEN (cr, x2, y2, &s_x2, &s_y2);

  s_line_width = screen_width (cr, line_width);

  offset = (line_width == -1 || (s_line_width % 2) == 0) ? 0 : 0.5;

  /* Allow filled boxes (inferred from line_width == -1)
   * to touch an extra pixel, so the filled span is inclusive */
  if (line_width == -1) {
    if (s_x1 > s_x2) s_x1 += 1; else s_x2 += 1;
    if (s_y1 > s_y2) s_y1 += 1; else s_y2 += 1;
  }

  s_x1 += offset; s_y1 += offset;
  s_x2 += offset; s_y2 += offset;

  cairo_device_to_user (cr, &s_x1, &s_y1);
  cairo_device_to_user (cr, &s_x2, &s_y2);

  cairo_move_to (cr, s_x2, s_y2);

  cairo_line_to (cr, s_x1, s_y2);
  cairo_line_to (cr, s_x1, s_y1);
  cairo_line_to (cr, s_x2, s_y1);

  cairo_close_path (cr);
}

/*!
 * \brief Render a Box at center point using Cairo Graphic
 * \par Function Description
 *  Scale coordinates of centerpoint to screen coordinates, calculate
 *  vertices of the rectangle and render using Cairo graphics library.
 */
void
eda_cairo_center_box (cairo_t *cr, int flags,
                      double center_width,
                      double line_width, double x, double y,
                      double half_width, double half_height)
{
  int s_center_width, s_line_width;
  int s_width, s_height;
  double s_half_width, s_half_height;
  double s_x, s_y;
  double even_center_width;
  double even_line_width;
  double even_width, even_height;
  double x1, y1, x2, y2;
  double center_offset;
  int do_width_hint = TRUE;
  int do_height_hint = TRUE;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    cairo_rectangle (cr, (x - half_width), (y - half_height),
                     2*half_width, 2*half_height);
    return;
  }

  WORLDtoSCREEN (cr, x, y, &s_x, &s_y);
  s_width  = SCREENabs (cr, 2 * half_width);
  s_height = SCREENabs (cr, 2 * half_height);
  even_width  = (s_width % 2 == 0);
  even_height = (s_width % 2 == 0);
  s_half_width  = (double) s_width  / 2.;
  s_half_height = (double) s_height / 2.;

#if 0 /* Not as nice an effect as with arcs */
  /* Switch off radius hinting for small radii. If we don't, then we get
   * a very abrupt transition once the box reaches a single pixel size. */
  if (s_half_width  <= 1.)  do_width_hint  = FALSE;
  if (s_half_height <= 1.)  do_height_hint = FALSE;
#endif

  /* Hint the center of the box based on where a line
   * of thickness center_width (world) would drawn */
  s_center_width = screen_width (cr, center_width);
  even_center_width = (center_width == -1 || (s_center_width % 2) == 0);
  center_offset = even_center_width ? 0. : 0.5;

  /* Hint the half-widths to land the stroke on the pixel grid */
  s_line_width = screen_width (cr, line_width);
  even_line_width = (line_width == -1 || (s_line_width % 2) == 0);
  if (do_width_hint)
    s_half_width  += ((even_center_width ==
                             even_line_width) == even_width ) ? 0. : 0.5;
  if (do_height_hint)
    s_half_height += ((even_center_width ==
                             even_line_width) == even_height) ? 0. : 0.5;

  x1 = (double) s_x + center_offset - s_half_width;
  y1 = (double) s_y + center_offset - s_half_height;
  x2 = (double) s_x + center_offset + s_half_width;
  y2 = (double) s_y + center_offset + s_half_height;

  /* Allow filled boxes (inferred from line_width == -1)
   * to touch an extra pixel, so the filled span is inclusive */
  if (line_width == -1) {
    x2 += 1;  y2 += 1;
  }

  cairo_device_to_user (cr, &x1, &y1);
  cairo_device_to_user (cr, &x2, &y2);
  cairo_move_to (cr, x2, y2);
  cairo_line_to (cr, x1, y2);
  cairo_line_to (cr, x1, y1);
  cairo_line_to (cr, x2, y1);
  cairo_close_path (cr);
}

/*!\brief Draw line using Cairo Graphic Renderer
 * \par Function Description
 *  Scale line to screen coordinates, establish end points and render
 *  a line using Cairo graphics library.
 */
void
eda_cairo_line (cairo_t *cr, int flags, int line_end,
                double w_line_width,
                double w_x1, double w_y1, double w_x2, double w_y2)
{
  double x1, y1, x2, y2;
  double offset;
  double xoffset    = 0;
  double yoffset    = 0;
  double horizontal = 0;
  double vertical   = 0;
  int    line_width;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    cairo_move_to (cr, w_x1, w_y1);
    cairo_line_to (cr, w_x2, w_y2);
    return;
  }

  WORLDtoSCREEN (cr, w_x1, w_y1, &x1, &y1);
  WORLDtoSCREEN (cr, w_x2, w_y2, &x2, &y2);
  line_width = screen_width (cr, w_line_width);

  offset = (line_width & 1) ? 0 : 0.5;

  if (y1 == y2) horizontal = 1;
  if (x1 == x2) vertical   = 1;

  /* Hint so the length of the line runs along a pixel boundary */

  if (horizontal) {
    yoffset = offset;
  }
  else if (vertical) {
    xoffset = offset;
  }
  else {
    xoffset = yoffset = offset;
  }

  /* Now hint the ends of the lines */

  switch (line_end) {
    case END_NONE:
      /* Line terminates at the passed coordinate */

      /* Add an extra pixel to give an inclusive span */
      if (horizontal) {
        if (x1 > x2) {
          x1 += 1;
        }
        else {
          x2 += 1;
        }
      }
      else if (vertical) {
        if (y1 > y2) {
          y1 += 1;
        }
        else {
          y2 += 1;
        }
      }
      break;

    case END_SQUARE:
    case END_ROUND:

      /* Line terminates half a width away from the passed coordinate */
      if (horizontal) {
        xoffset = offset;
      }
      else if (vertical) {
        yoffset = offset;
      }
      break;
  }

  x1 += xoffset;
  y1 += yoffset;
  x2 += xoffset;
  y2 += yoffset;

  cairo_device_to_user (cr, &x1, &y1);
  cairo_device_to_user (cr, &x2, &y2);
  cairo_move_to        (cr, x1,   y1);
  cairo_line_to        (cr, x2,   y2);
}

static inline void
eda_cairo_path_hint (cairo_t *cr, int flags,
                     double *x, double *y, int width)
{
  if (flags & EDA_CAIRO_ENABLE_HINTS) {

    double offset;

    cairo_user_to_device (cr, x, y);

    offset = ((width % 2) == 0) ? 0 : 0.5;
    *x    += offset; *y += offset;

    cairo_device_to_user (cr, x, y);
  }
}

/*!
 * \brief Render an Path using Cairo Graphic
 * \par Function Description
 *  Scale coordinates of path segment to screen coordinates, and calls correct
 *  Cairo graphics library routine based in the segment type.
 */
void eda_cairo_path (cairo_t *cr, int flags, double line_width,
                     int nsections, PATH_SECTION *sections)
{
  int i;
  int s_line_width;

  if (flags & EDA_CAIRO_ENABLE_HINTS) {
    s_line_width = screen_width (cr, line_width);
  }
  else {
    double dummy = 0;
    cairo_user_to_device (cr, &line_width, &dummy);
    s_line_width = line_width;
  }

  for (i = 0; i < nsections; i++) {

    PATH_SECTION *section = sections + i;
    double x1 = section->x1;
    double x2 = section->x2;
    double x3 = section->x3;
    double y1 = section->y1;
    double y2 = section->y2;
    double y3 = section->y3;

    switch (section->code) {
      case PATH_CURVETO:
        /* Two control point grips */
        eda_cairo_path_hint (cr, flags, &x1, &y1, s_line_width);
        eda_cairo_path_hint (cr, flags, &x2, &y2, s_line_width);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        eda_cairo_path_hint (cr, flags, &x3, &y3, s_line_width);
      case PATH_END:
        break;
    }

    switch (section->code) {
      case PATH_MOVETO:
        cairo_close_path (cr);
        /* fall-through */
      case PATH_MOVETO_OPEN:
        cairo_move_to (cr, x3, y3);
        break;
      case PATH_CURVETO:
        cairo_curve_to (cr, x1, y1, x2, y2, x3, y3);
        break;
      case PATH_LINETO:
        cairo_line_to (cr, x3, y3);
        break;
      case PATH_END:
        cairo_close_path (cr);
        break;
    }
  }
}

/*!
 * \brief Set the Cairo Source Color
 * \par Function Description
 *  This function sets the source color using the color in the given map
 *  at the given index.
 */
void
eda_cairo_set_source_color (cairo_t *cr, int color, GArray *map)
{
  if (map == NULL) {
    BUG_MSG ("map = NULL");
  }
  else if ((color < 0) || (color > map->len - 1)) {
    BUG_IMSG("Invalid color index", color);
  }
  else {

    COLOR c;

    c = g_array_index (map, COLOR, color);

    cairo_set_source_rgba (cr, (double)c.r / 255.0,
                               (double)c.g / 255.0,
                               (double)c.b / 255.0,
                               (double)c.a / 255.0);
  }
}

/*!
 * \brief Render a Stroke using Cairo Graphic
 * \par Function Description
 *  Sets end point type and render stroke using Cairo graphics library.
 */
void
eda_cairo_stroke (cairo_t *cr, int flags, int line_type, int line_end,
                  double wwidth, double wlength, double wspace)
{

  double dashes[4];
  double dummy  = 0;
  double offset = 0;
  double width;
  double length;
  double space;

  cairo_line_cap_t cap;
  cairo_line_cap_t round_cap_if_legible = CAIRO_LINE_CAP_ROUND;
  int num_dashes;

  if (flags & EDA_CAIRO_ENABLE_HINTS) {

    int iwidth;

    /* cairo_user_to_device_distance */
    width  = iwidth = screen_width (cr, wwidth);

    length = screen_width (cr, wlength);
    space  = screen_width (cr, wspace);

    cairo_device_to_user_distance (cr, &width, &dummy);
    cairo_device_to_user_distance (cr, &length, &dummy);
    cairo_device_to_user_distance (cr, &space, &dummy);

    offset = (iwidth & 1) ? 0 : 0.5;

    round_cap_if_legible =
      (iwidth <= 1) ? CAIRO_LINE_CAP_SQUARE : CAIRO_LINE_CAP_ROUND;
  }
  else {

    width  = wwidth;
    length = wlength;
    space  = wspace;
  }

  cairo_set_line_width (cr, width);
  cairo_set_line_join (cr, CAIRO_LINE_JOIN_MITER);

  switch (line_end) {
    case END_NONE:   cap = CAIRO_LINE_CAP_BUTT;   break;
    case END_SQUARE: cap = CAIRO_LINE_CAP_SQUARE; break;
    case END_ROUND:  cap = round_cap_if_legible;  break;
    default:
      g_warn_if_reached ();
      cap = CAIRO_LINE_CAP_BUTT;
    break;
  }

  switch (line_type) {

    default:
      g_warn_if_reached ();
      /* Fall through */

    case TYPE_SOLID:

      num_dashes = 0;
      offset     = 0.0;
      break;

    case TYPE_DOTTED:

      dashes[0]  = 0;                    /* DOT */
      dashes[1]  = space;
      num_dashes = 2;
      cap        = round_cap_if_legible;
      break;

    case TYPE_DASHED:

      dashes[0]  = length;               /* DASH */
      dashes[1]  = space;
      num_dashes = 2;
      offset     = 0.0;
      cap        = CAIRO_LINE_CAP_BUTT;
      break;

    case TYPE_CENTER:

      dashes[0]  = length;               /* DASH */
      dashes[1]  = 2 * space;
      num_dashes = 2;

      cairo_set_dash (cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
      cairo_stroke_preserve (cr);

      dashes[0]  = 0;                    /* DOT */
      dashes[1]  = 2 * space + length;
      num_dashes = 2;
      offset     = offset - length - space;
      cap        = round_cap_if_legible;
      break;

    case TYPE_PHANTOM:

      dashes[0]  = length;               /* DASH */
      dashes[1]  = 3 * space;
      num_dashes = 2;

      cairo_set_dash (cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
      cairo_stroke_preserve (cr);

      dashes[0]  = 0;                    /* DOT */
      dashes[1]  = space;
      dashes[2]  = 0;                    /* DOT */
      dashes[3]  = 2 * space + length;
      num_dashes = 4;
      offset     = offset - length - space;
      cap        = round_cap_if_legible;
      break;
  }

  cairo_set_dash (cr, dashes, num_dashes, offset);

  cairo_set_line_cap (cr, cap);

  cairo_stroke (cr);

  cairo_set_dash (cr, NULL, 0, 0.);
}
