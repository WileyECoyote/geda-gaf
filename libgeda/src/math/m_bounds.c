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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */
/*!
 * \file m_bounds.c
 * \brief Functions for working with boundaries
 */

#include "../../../config.h"
#include <libgeda_priv.h>

/*!
 * \brief Initialize a bounds by setting it to empty
 * \par Function Description
 *  The \a bonds parameter must not be NULL.
 *
 * \param [in] bounds The bounds to set to empty.
 */
void
geda_math_bounds_init(BOUNDS *bounds)
{
  g_return_if_fail (bounds != NULL);

  bounds->min_x = INT_MAX;
  bounds->min_y = INT_MAX;
  bounds->max_x = INT_MIN;
  bounds->max_y = INT_MIN;
}

/*!
 * \brief Calculate the bounds of a set of points
 * \par Function Description
 *  For an empty set of points, this function returns an empty bounds.
 *  The \a bounds does not need to be initialized before calling this
 *  function, but this  parameter must not be NULL. If the \a count is
 *  greater than zero, \a points argument must not be NULL.
 *
 * \param [out] bounds  The bounds of the given set of points.
 * \param [in]  points  The given set of points.
 * \param [in]  count   The number of points in the set.
 */
void
geda_math_bounds_of_points(BOUNDS *bounds, GedaPoint points[], int count)
{
  int index;

  g_return_if_fail (bounds != NULL);

  geda_math_bounds_init(bounds);

  for (index = 0; index < count; index++) {

    int x = points[index].x;
    int y = points[index].y;

    if (x < bounds->min_x) {
      bounds->min_x = x;
    }

    if (y < bounds->min_y) {
      bounds->min_y = y;
    }

    if (x > bounds->max_x) {
      bounds->max_x = x;
    }

    if (y > bounds->max_y) {
      bounds->max_y = y;
    }
  }
}
