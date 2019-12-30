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

/** \defgroup math-arc-proc Math Procedures for Arcs
 * @{
 * \brief Math Related Procedures for Arcs
 */

#include "../../../config.h"

#include <math.h>
#include <stdio.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

void
geda_math_arc_chord (GedaArc *arc, LINE *line)
{
  double angle;
  double radius;
  int sx, sy, ex, ey;

  radius = arc->radius;

  angle = M_PI * ((double)arc->start_angle) / 180.0;

#ifdef HAVE_LRINT

  sx = lrint ((double)arc->x + radius * cos (angle));
  sy = lrint ((double)arc->y + radius * sin (angle));

#else

  sx = ((double)arc->x + radius * cos (angle)) + 0.5;
  sy = ((double)arc->y + radius * sin (angle)) + 0.5;

#endif

  line->x[0] = sx;
  line->y[0] = sy;

  angle += M_PI * ((double)arc->arc_sweep) / 180.0;

#ifdef HAVE_LRINT

  ex = lrint ((double)arc->x + radius * cos (angle));
  ey = lrint ((double)arc->y + radius * sin (angle));

#else

  ex = ((double)arc->x + radius * cos (angle)) + 0.5;
  ey = ((double)arc->y + radius * sin (angle)) + 0.5;

#endif

  line->x[1] = ex;
  line->y[1] = ey;
}

/*!
 * \brief Calculates the length of an Arc sector
 * \par Function Description
 *  Returns length of the Arc sector.
 *
 * \param [in] arc An arc object.
 *
 * \return The sector length of the arc.
 */
double
geda_math_arc_length (GedaArc *arc)
{
  g_return_val_if_fail (arc != NULL, 0.0);

  int radius = arc->radius;
  int sweep  = arc->arc_sweep;

  return 2.0 * M_PI * radius * (sweep / 360.0);
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
  bool   answer;
  double dist;
  int    delta;  /* Will be difference between point to center and radius */
  int    width;
  int    half_width;

  g_return_val_if_fail ((arc != NULL) && (point != NULL), FALSE);

  dist = geda_distance(arc->x, arc->y, point->x, point->y);

  /* Rounding here provides a fuzz distance effect */
  delta = abs (dist - arc->radius);

#if DEBUG

  int radius = arc->radius;

  fprintf(stderr, "point (%d, %d) ", point->x, point->y);
  fprintf(stderr, "dist %.1f radius %d delta %d\n", dist, radius, delta);

#endif

  /* Get the line-width of the arc */
  width = geda_arc_get_line_width(arc);

  /* Calculate 1/2 the width or the drawn width if line-width was zero */
  half_width = width ? width >> 1 : MIN_LINE_WIDTH_THRESHOLD / 2;

  /* First compare the distance from the arc center to the radius */
  if (delta < half_width) {

    /* Secondly, check angle of ray is within the arc's included angle.
     * If the start_angle + arc_angle is > 360 degrees then angle of the
     * ray could be less than the min_angle but the point could still on
     * the arc where the arc swept beyond zero, will check for this later.
     */
    int    arc_angle = arc->start_angle + arc->arc_sweep;
    double min_angle = geda_math_degrees_to_radians (arc->start_angle);
    double max_angle = geda_math_degrees_to_radians (arc_angle);

    /* Get angle of ray */
    double radians = atan2((point->y - arc->y), (point->x - arc->x));

    /* If negative, make the angle positive */
    while (radians < 0) {
      radians += 2 * M_PI;
    }

    if (radians > max_angle) {
      answer = FALSE;
    }
    else if (radians < min_angle) {

      /* Check arc swept pass zero */
      if (max_angle > 2 * M_PI) {
        /* If ray is within sector that swept pass zero pt is on the arc */
        answer = radians < max_angle - 2 * M_PI;
      }
      else {
        answer = FALSE;
      }
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

/*!
 * \brief Determine Shortest Distance to an Arc
 * \par Function Description
 *  Calculates the distance between the given point and the closest
 *  point on the perimeter of the arc.
 *
 * \param [in] arc    A GedaArc object
 * \param [in] x      The x coordinate of the given point
 * \param [in] y      The y coordinate of the given point
 * \param [in] solid  Treat arc as solid if TRUE, otherwise treat as hollow.
 *
 * \return The shortest distance from the object to the point. With an
 *         invalid parameter, this function returns G_MAXDOUBLE.
 */
double geda_math_arc_shortest_distance (GedaArc *arc, int x, int y, int solid)
{
  double shortest_distance;
  double radius;

  g_return_val_if_fail (GEDA_IS_ARC(arc), G_MAXDOUBLE);

  radius = (double)arc->radius;

  if (geda_arc_within_sweep (arc, x, y)) {

    double distance_to_center;
    double dx;
    double dy;

    dx = ((double)x) - ((double)arc->x);
    dy = ((double)y) - ((double)arc->y);

#if HAVE_HYPOT
    distance_to_center = hypot (dx, dy);
#else
    distance_to_center = sqrt ((dx * dx) + (dy * dy));
#endif

    if (solid && distance_to_center - radius < 0) {
      shortest_distance = 0.0;
    }
    else {
      shortest_distance = fabs (distance_to_center - radius);
    }
  }
  else {

    double angle;
    double distance_to_end0;
    double distance_to_end1;
    double dx, dy;

    angle = M_PI * ((double)arc->start_angle) / 180;

    dx = ((double)x) - radius * cos (angle) - ((double)arc->x);
    dy = ((double)y) - radius * sin (angle) - ((double)arc->y);

#if HAVE_HYPOT
    distance_to_end0 = hypot (dx, dy);
#else
    distance_to_end0 = sqrt ((dx * dx) + (dy * dy));
#endif

    angle += M_PI * ((double)arc->arc_sweep) / 180;

    dx = ((double)x) - radius * cos (angle) - ((double)arc->x);
    dy = ((double)y) - radius * sin (angle) - ((double)arc->y);

#if HAVE_HYPOT
    distance_to_end1 = hypot (dx, dy);
#else
    distance_to_end1 = sqrt ((dx * dx) + (dy * dy));
#endif

    shortest_distance = min (distance_to_end0, distance_to_end1);
  }

  return shortest_distance;
}

/** @} endgroup math-arc-proc */
