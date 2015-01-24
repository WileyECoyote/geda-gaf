/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2014 Wiley Edward Hill
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

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"

#include <geda_debug.h>

/*! \brief Calculates the length of an Arc sector
 * point on the perimeter or interior of the circle.
 *
 *  \param [in] radius  The radius of the arc.
 *  \param [in] sweep   The included angle.
 *
 *  \return The length of the sector.
 */
double  m_arc_length (int radius, int sweep)
{
  return 2 * M_PI * radius * (sweep / 360);
}

/*! \brief Determine if Arc Sector includes Point
 *  Compares distance from point to the center of the Arc to the radius
 *  of the Arc, if there is no difference compare the angle of the ray
 *  from the center to the point to the starting and ending angles of
 *  \a arc. Returns True if the ray is within the Arc's included angle
 *  otherwise false.
 *
 *  \param [in] arc    The arc object.
 *  \param [in] point  Point to test for inclusion.
 *
 *  \return True if \a arc includes \a point
 */
bool m_arc_includes_point (Arc *arc, POINT *point)
{
  bool answer;
  int  delta;

  /* Rounding here provides a fuzz distance effect */
  delta = m_distance(arc->x, arc->y, point->x, point->y) - (arc->width / 2);

#if DEBUG
  int  dist;
  int  radius;

#ifdef HAVE_LRINT

  dist   = lrint(m_distance(arc->x, arc->y, point->x, point->y));
  radius = lrint(arc->width / 2);

#else

  dist   = (int) (m_distance(arc->x, arc->y, point->x, point->y)) + 0.5;
  radius = (int) (arc->width / 2) + 0.5;

#endif

  fprintf(stderr, "point->x %d, point->y %d\n", point->x, point->y);
  fprintf(stderr, "dist %d <> radius %d delta %d\n", dist, radius, delta);

#endif

  /* First compare the distance from the arc center to the radius */

  if (!delta) {

    /* Second, check angle of ray is within the arc's included angle */

    int    arc_angle = arc->start_angle + arc->arc_sweep;
    double min_angle = m_degrees_to_radians(arc->start_angle);
    double max_angle = m_degrees_to_radians(arc_angle);

    /* Get angle of ray */
    double radians = atan2((point->y - arc->y), (point->x - arc->x));

    /* If negative, make the angle positive */
    if (radians < 0) {
      radians = radians + 2 * M_PI;
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
