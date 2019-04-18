/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2015 Wiley Edward Hill
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

/*! \file m_arc.c
 *
 *  \brief Low-level mathmatical functions for arcs
 */

#include "../../../config.h"

#include <math.h>
#include <stdio.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

/*!
 * \brief Calculates the length of an Arc sector
 * \par Function Description
 *  Returns length of the Arc sector.
 *
 * \param [in] radius  The radius of the arc.
 * \param [in] sweep   The included angle.
 *
 * \return The length of the sector.
 */
double
geda_math_arc_length (int radius, int sweep)
{
  return 2 * M_PI * radius * (sweep / 360);
}

/*!
 * \brief Determine if Arc Sector includes a Point
 * \par Function Description
 *  Compares distance from point to the center of the Arc to the radius
 *  of the Arc, if there is no difference after accounting for the line
 *  width, compares the angle of the ray from the center to the point to
 *  the starting and ending angles of \a arc. Returns True if the ray is
 *  within the Arc's included angle otherwise false.
 *
 * \param [in] arc    The arc object.
 * \param [in] point  Point to test for inclusion.
 *
 * \return True if \a arc includes \a point
 */
bool
geda_math_arc_includes_point (GedaArc *arc, GedaPoint *point)
{
  bool answer;
  int  delta;  /* Will be difference between point to center and radius */
  int  width;
  int  half_width;

  g_return_val_if_fail ((arc != NULL) && (point != NULL), FALSE);

  /* Rounding here provides a fuzz distance effect */
  delta = geda_distance(arc->x, arc->y, point->x, point->y) - (arc->radius);

#if DEBUG

  int dist;
  int radius = arc->radius;

#ifdef HAVE_LRINT

  dist   = lrint(geda_distance(arc->x, arc->y, point->x, point->y));

#else

  dist   = (int) (geda_distance(arc->x, arc->y, point->x, point->y)) + 0.5;

#endif

  fprintf(stderr, "point->x %d, point->y %d\n", point->x, point->y);
  fprintf(stderr, "dist %d <> radius %d delta %d\n", dist, radius, delta);

#endif

  /* Get the line-width of the arc */
  width = geda_arc_get_line_width(arc);

  /* Calculate 1/2 the width or the drawn width if line-width was zero */
  half_width = width ? width / 2 : MIN_LINE_WIDTH_THRESHOLD / 2;

  /* First compare the distance from the arc center to the radius */
  if (delta < half_width) {

    /* Second, check angle of ray is within the arc's included angle */

    int    arc_angle = arc->start_angle + arc->arc_sweep;
    double min_angle = geda_math_degrees_to_radians (arc->start_angle);
    double max_angle = geda_math_degrees_to_radians (arc_angle);

    /* Get angle of ray */
    double radians = atan2((point->y - arc->y), (point->x - arc->x));

    /* If negative, make the angle positive */
    while (radians < 0) {
      radians += 2 * M_PI;
    }

    if (radians < min_angle || radians > max_angle) {
      answer = FALSE;
    }
    else {
      answer = TRUE;
    }
  }
  else {
    answer = FALSE;
  }

  return answer;
}
