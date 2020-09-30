/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2020 gEDA Contributors (see ChangeLog for details)
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

/*! \file m_box.c
 *
 *  \brief Low-level mathmatical functions for boxes
 */

/** \defgroup math-box-proc Math Procedures for Boxes
 * @{
 * \brief Math Related Procedures for Boxes
 */

#include "../../../config.h"

#include <math.h>
#include <stdio.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

int
geda_math_box_area (GedaBox *box)
{
  int x1, y1, x2, y2;
  int area;

  g_return_val_if_fail (box != NULL, 0.0);

  x1 = min (box->upper_x, box->lower_x);
  y1 = min (box->upper_y, box->lower_y);
  x2 = max (box->upper_x, box->lower_x);
  y2 = max (box->upper_y, box->lower_y);

  area = (x2 - x1) * (y2 - y1);

  return area;
}

/*!
 * \brief Determine shortest distance from a Box to Point
 * \par Function Description
 *  Calculates the distance between the given point and the closest
 *  point on the perimeter or interior of the Gedabox.
 *
 * \param [in] box    The Gedabox.
 * \param [in] x      The x coordinate of the given point.
 * \param [in] y      The y coordinate of the given point.
 * \param [in] solid  TRUE if the box should be treated as solid, FALSE if
 *                     the box should be treated as hollow.
 *
 * \return The shortest distance from the box to the point. With a solid
 *         shape, this function returns a distance of zero for interior points.
 *         With an invalid parameter, this function returns G_MAXDOUBLE.
 */
double
geda_math_box_shortest_distance (GedaBox *box, int x, int y, int solid)
{
  double shortest_distance;
  double x1, y1, x2, y2;
  double dx, dy;

  g_return_val_if_fail (box != NULL, G_MAXDOUBLE);

  x1 = (double) min (box->upper_x, box->lower_x);
  y1 = (double) min (box->upper_y, box->lower_y);
  x2 = (double) max (box->upper_x, box->lower_x);
  y2 = (double) max (box->upper_y, box->lower_y);

  dx = min (((double)x) - x1, x2 - ((double)x));
  dy = min (((double)y) - y1, y2 - ((double)y));

  if (solid) {
    dx = min (dx, 0);
    dy = min (dy, 0);
  }

  if (dx < 0) {

    if (dy < 0) {

#if HAVE_HYPOT

      shortest_distance = hypot(dx, dy);

#else

      shortest_distance = sqrt ((dx * dx) + (dy * dy));

#endif

    }
    else {
      shortest_distance = fabs (dx);
    }
  }
  else {

    if (dy < 0) {
      shortest_distance = fabs (dy);
    }
    else {
      shortest_distance = min (dx, dy);
    }
  }

  return shortest_distance;
}

/** @} endgroup math-box-proc */
