/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 Ales Hvezda
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

/*! \file m_line.c
 ** \brief Low-level mathmatical functions for lines
 *  \par
 *  Validation of line objects in this module is limited to checking for
 *  NULL pointers. Functions in this module do not utilize is_a_geda_line
 *  to allow callers to use GedaLine structures as arguments. For an
 *  example see geda_box_object_get_nearest_point. All internal library
 *  routines calling any of the functions in this module should validate
 *  the objects as lines, noting that pin, net and buses are derived from
 *  GedaLine objects.
 */

#include "../../../config.h"

#include <math.h>
#include <stdio.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

/*!
 * \brief Determine the Intersection of two lines
 * \par Function Description
 *  This function determines if two lines intersect. If lines intersect
 *  the function returns true and the value of points is set to that of
 *  the intersection. Otherwise the function returns false.
 *
 *  \param [in]  line1 First Line
 *  \param [in]  line2 Second Line
 *  \param [out] point Intersection if lines intersect
 *
 *  \return TRUE if lines intersect
 *
 *  \sa geda_math_line_intersection
 */
bool
geda_math_line_get_intersection(GedaLine *line1, GedaLine *line2, GedaPoint *point)
{
  g_return_val_if_fail (line1 != NULL, FALSE);
  g_return_val_if_fail (line2 != NULL, FALSE);
  g_return_val_if_fail (point != NULL, FALSE);

  LINE *L1 = (LINE*) &line1->x[0];
  LINE *L2 = (LINE*) &line2->x[0];

  return geda_math_line_intersection(L1, L2, point);
}

/*!
 * \brief Returns True if Line contains point
 */
bool geda_math_line_includes_point (GedaLine *line, GedaPoint *point)
{
  bool included;

  g_return_val_if_fail (line != NULL, FALSE);
  g_return_val_if_fail (point != NULL, FALSE);

  int x11 = min(line->x[0], line->x[1]);
  int x21 = max(line->x[0], line->x[1]);

  int y11 = min(line->y[0], line->y[1]);
  int y21 = max(line->y[0], line->y[1]);

  if ((x11 <= point->x && point->x <= x21) &&
      (y11 <= point->y && point->y <= y21))
  {
    included = TRUE;
  }
  else {
    included = FALSE;
  }
  return included;
}

/*!
 * \brief Determine the Intersection of two lines with structures
 * \par Function Description
 *  This function determines if two lines intersect. If lines intersect
 *  the function returns true and the value of points is set to that of
 *  the intersection. Otherwise the function returns false.
 *
 * \param [in]  line1 First Line
 * \param [in]  line2 Second Line
 * \param [out] point Intersection if lines intersect
 *
 * \return TRUE if lines intersect
 *
 * \sa geda_math_line_get_intersection
 */
bool geda_math_line_intersection(LINE *line1, LINE *line2, GedaPoint *point)
{
  bool   has_slope1;
  bool   has_slope2;
  bool   intersect;

  double dy,dx;
  double slope1;
  double slope2;

  g_return_val_if_fail (line1 != NULL, FALSE);
  g_return_val_if_fail (line2 != NULL, FALSE);
  g_return_val_if_fail (point != NULL, FALSE);

  has_slope1 = line1->x[0] == line1->x[1] ? FALSE : TRUE;
  has_slope2 = line2->x[0] == line2->x[1] ? FALSE : TRUE;

  if (has_slope1 && has_slope2) { /* Both are lines and are on an angle */

    dy     = line1->y[1] - line1->y[0];
    dx     = line1->x[1] - line1->x[0];
    slope1 = dy / dx;

    dy     = line2->y[1] - line2->y[0];
    dx     = line2->x[1] - line2->x[0];
    slope2 = dy / dx;

    if (slope1 != slope2) {

      /* y-intercept = ordinate - slope x abscissa */
      double b11 = line1->y[0] - (slope1 * line1->x[0]);
      double b21 = line2->y[0] - (slope2 * line2->x[0]);

      /* abscissa = y-intercept2 - y-intercept1 / slope1 - slope2 */
      double x = (b21 - b11) / (slope1 - slope2);

#ifdef HAVE_LRINT

      point->x = lrint(x);
      point->y = lrint(slope1 * x + b11); /* pick 1 */

#else

      point->x = x + 0.5;
      point->y = slope1 * x + b11 + 0.5; /* pick 1 */

#endif

      intersect = TRUE; /* Not arbitrary */
    }
    else { /* lines are parallel and do not intersect */
      intersect = FALSE;
    }
  }
  else if (has_slope1) {               /* Line 2 is vertical */

    /* Get where line 1 intersects */
    point->x = line2->x[0];            /* arbitrary, x's are equal */

    dy       = line1->y[1] - line1->y[0];
    dx       = line1->x[1] - line1->x[0];
    slope1   = dy / dx;

    if (slope1 == 0) {                 /* if line 1 is horizontal */
      point->y = line1->y[0];          /* arbitrary, y's are equal */
    }
    else {                             /* get y-intercept for line 1 */

      /* intercept = y - mx */
      double b11 = line1->y[0] - (slope1 * line1->x[0]);

#ifdef HAVE_LRINT

      /* solve for y1(1) at x */
      point->y = lrint(slope1 * point->x + b11);  /* y = mx + b */

#else

      /* solve for y1(1) at x */
      point->y = (int) (slope1 * point->x + b11) + 0.5;  /* y = mx + b */

#endif

    }

    intersect = TRUE;

  }
  else if (has_slope2) {               /* line 1 maybe vertical */

    /* Get where line 2 intersects */
    point->x = line1->x[0];            /* arbitrary, x's are equal */

    dy       = line2->y[1] - line2->y[0];
    dx       = line2->x[1] - line2->x[0];
    slope2   = dy / dx;


    if (slope2 == 0) {                 /* if line 2 is horizontal */
      point->y = line2->y[0];          /* arbitrary, y's are equal */
    }
    else {                            /* get y-intercept for line2 */

      /* intercept = y - mx */
      double b21  = line2->y[0] - (slope2 * line2->x[0]);

#ifdef HAVE_LRINT

      /* solve for y2(1) at x */
      point->y = lrint(slope2 * point->x + b21);   /* y = mx + b */

#else

      /* solve for y2(1) at x */
      point->y = (int) (slope2 * point->x + b21) + 0.5;   /* y = mx + b */

#endif

    }

    intersect = TRUE;

  }
  else {  /* both are vertical and do not intersect even if conincide */
    intersect = FALSE;
  }

  return intersect;
}

/*!
 * \brief Returns integer distance between two points
 *
 * \sa geda_distance
 */
int geda_math_line_length (int x1, int y1, int x2, int y2)
{
  int length;

#ifdef HAVE_lRINT
      length = lrint(geda_distance (x1, y1, x2, y2));
#else
      length = (int)geda_distance (x1, y1, x2, y2) + 0.5;
#endif

  return length;
};

/*!
 * \brief Get Shortest distance from Point to a Line segment.
 *  If the closest point on the line resides beyond the line segment's
 *  end point, this function returns the distance from the given point
 *  to the closest end point.
 *
 *  If the line represents a single point (the endpoints are the same),
 *  this function calculates the distance to that point.
 *
 * \param [in] line  The line object or structure,
 * \param [in] x     The x coordinate of the given point,
 * \param [in] y     The y coordinate of the given point.
 *
 * \return The shortest distance from the object to the point. With an
 *         invalid parameter, this function returns G_MAXDOUBLE.
 */
double geda_math_line_shortest_distance (GedaLine *line, int x, int y)
{
  double dx, dy;
  double lx0, ly0;
  double ldx, ldy;

  g_return_val_if_fail (line != NULL, G_MAXDOUBLE);

  lx0 = (double)line->x[0];
  ly0 = (double)line->y[0];
  ldx = (double)(line->x[1] - line->x[0]);
  ldy = (double)(line->y[1] - line->y[0]);

  if (ldx == 0 && ldy == 0) {

    /* if line is a point, just calculate distance to the point */
    dx = x - lx0;
    dy = y - ly0;

  }
  else {

    double cx, cy;
    double dx0, dy0;
    double t;

    /* calculate parametric value of perpendicular intersection */
    dx0 = ldx * (x - lx0);
    dy0 = ldy * (y - ly0);

    t = (dx0 + dy0) / (ldx * ldx + ldy * ldy);

    /* constrain the parametric value to a point on the line */
    t = max (t, 0);
    t = min (t, 1);

    /* calculate closest point on the line */
    cx = t * ldx + lx0;
    cy = t * ldy + ly0;

    /* calculate distance to closest point */
    dx = x - cx;
    dy = y - cy;
  }

  return sqrt ((dx * dx) + (dy * dy));
}
