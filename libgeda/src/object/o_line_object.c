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

/*! \file o_line_object.c
 *  \brief Functions for the line object
 */

/** \defgroup geda-line-object-proc GedaLine Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaLine Objects
 */

#include "../../../config.h"

#include <stdio.h>
#include <math.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

static void
geda_object_error(const char *func, const void *object, IDE_OBJECT_TYPE type)
{
  geda_error_object_argument(__FILE__, func, object, type);
}

static void
geda_line_object_error(const char *func, const void *object)
{
  geda_object_error(func, object, GEDA_OBJECT_LINE);
}

/*!
 * \brief Create a copy of a line.
 * \par Function Description
 *  This function creates a copy of the \a GedaLine object pointed by
 *  <B>\a o_current</B> describing a line. The coordinates of the ends
 *  of the new line are set with the ones of the original line.
 *  The two lines have the same #LINE_TYPE
 *
 * \param [in]  o_current  GedaLine GedaObject to copy.
 *
 * \return The new GedaObject
 */
GedaObject *geda_line_object_copy(GedaObject *o_current)
{
  if (GEDA_IS_LINE(o_current)) {

    GedaObject *new_obj;
    GedaLine   *old_line;

    old_line = GEDA_LINE(o_current);

    /* A new line object is created with #geda_line_object_new().
     * Values for its fields are default and need to be modified. */
    new_obj = geda_line_object_new (o_current->color,
                          old_line->x[0], old_line->y[0],
                          old_line->x[1], old_line->y[1]);

    /* copy the line type options */
    geda_set_object_line_options(new_obj, &old_line->line_options);

    /* return the new tail of the object list */
    return new_obj;
  }
  geda_line_object_error(__func__, o_current);
  return NULL;
}

/*!
 * \brief Get which end of Line is closest to a Point
 * \par Function Description
 *  Determines which end-point of \a object is closest to the
 *  given point.
 *
 * \returns 0 or 1 or -1 if object is not a line
 */
int geda_line_object_get_closest_endpoint(const GedaObject *object, int x, int y)
{
  int anwser;

  if (GEDA_IS_LINE(object)) {
    double first  = geda_distance (object->line->x[0], object->line->y[0], x, y);
    double second = geda_distance (object->line->x[1], object->line->y[1], x, y);
    anwser = first < second ? 0 : 1;
  }
  else {
    geda_line_object_error(__func__, object);
    anwser = -1;
  }
  return anwser;
}

/*!
 * \brief Determine the Intersection of a linear GedaObject
 * \par Function Description
 *  This function determines if two linear objects intersect. If both
 *  are derived from Line and intersect the function returns true and
 *  the value of points is set to that of the intersection. Otherwise
 *  the function returns false.
 *
 * \param [in]  object1 First Linear object
 * \param [in]  object2 Second Linear object
 * \param [out] point   Intersection if both are lines and intersect
 *
 * \return TRUE if both are lines and intersect
 */
bool geda_line_object_get_intersection(GedaObject *object1,
                                       GedaObject *object2,
                                       GedaPoint  *point)
{
  bool   has_slope1;
  bool   has_slope2;
  bool   intersect;

  double slope1;
  double slope2;

  has_slope1 = geda_object_get_has_slope(object1);
  has_slope2 = geda_object_get_has_slope(object2);

  if (has_slope1 && has_slope2) { /* Both are lines and are on an angle */

    geda_line_object_get_slope(object1, &slope1);
    geda_line_object_get_slope(object2, &slope2);

    if (slope1 != slope2) {

      /* y-intercept = ordinate - slope x abscissa */
      long double b11 = object1->line->y[0] - (slope1 * object1->line->x[0]);
      long double b21 = object2->line->y[0] - (slope2 * object2->line->x[0]);

      /* abscissa = y-intercept2 - y-intercept1 / slope1 - slope2 */
      long double x = (b21 - b11) / (slope1 - slope2);

#ifdef HAVE_LRINT

      point->x = lrint(x);
      point->y = lrint((slope1 * x) + b11); /* pick 1 */

#else

      point->x = x + 0.5;
      point->y = (slope1 * x) + b11 + 0.5; /* pick 1 */

#endif

      intersect = TRUE; /* Not arbitrary */
    }
    else { /* linears are parallel and do not intersect */
      intersect = FALSE;
    }
  }
  else if (has_slope1) { /* Linear 2 is vertical */

    if (GEDA_IS_LINE(object2)) {

      /* Get where linear 1 intersects */
      point->x = object2->line->x[0];  /* arbitrary, x's are equal */

      geda_line_object_get_slope(object1, &slope1);

      if (slope1 == 0) {                 /* if linear is horizontal */
        point->y = object1->line->y[0];  /* arbitrary, y's are equal */
      }
      else { /* get y-intercept for object1 */

        long double b11;

        b11 = object1->line->y[0] - (slope1 * object1->line->x[0]);

#ifdef HAVE_LRINT

        point->y = lrint(slope1 * point->x + b11); /* solve for y1(1) at x */

#else

        point->y = slope1 * point->x + b11 + 0.5; /* solve for y1(1) at x */

#endif
      }

      intersect = TRUE;
    }
    else { /* GedaObject 2 is not a derived from a line */
      geda_line_object_error(__func__, object2);
      intersect = FALSE;
    }
  }
  else if (has_slope2) { /* object 1 maybe vertical */

    if (GEDA_IS_LINE(object1)) {

      /* Get where linear 2 intersects */
      point->x = object1->line->x[0];  /* arbitrary, x's are equal */

      geda_line_object_get_slope(object2, &slope2);

      if (slope2 == 0) { /* if linear is horizontal */
        point->y = object2->line->y[0];  /* arbitrary, y's are equal */
      }
      else { /* get y-intercept for object2 */

        long double b21;

        b21 = object2->line->y[0] - (slope2 * object2->line->x[0]);

#ifdef HAVE_LRINT

        point->y = lrint(slope2 * point->x + b21); /* solve for y2(1) at x */

#else

        point->y = slope2 * point->x + b21 + 0.5; /* solve for y2(1) at x */

#endif

      }

      intersect = TRUE;
    }
    else { /* GedaObject 1 is not a derived from a line */
      geda_line_object_error(__func__, object1);
      intersect = FALSE;
    }
  }
  else {  /* both are vertical and do not intersect even if conincide */
    intersect = FALSE;
  }

  return intersect;
}

/*!
 * \brief Retrieve End Cap type Property of an GedaObject
 * \par Function Description
 *  Returns the value of \a object end-cap type if and only if \a object is
 *  a valid GedaObject object.
 *
 * \return integer value of end-cap type or -0 if \a object is invalid.
 *
 * \sa geda_line_object_set_end_cap
 */
int geda_line_object_get_end_cap (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line_options->line_end;
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Line Length Property of a Circle object
 * \par Function Description
 *  Returns the value of the \a object line length property if \a object
 *  is a valid GedaLine object. The line-length property controls the
 *  length of line segments for line types dashed, center and phantom,
 *  to get the "length" of a line see geda_math_line_length.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \return integer value of line length or -0 if \a object is invalid.
 *
 * \sa geda_line_object_set_line_length
 */
int geda_line_object_get_line_length (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line_options->line_length;
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Line Space Property of a Circle object
 * \par Function Description
 *  Returns the value of the \a object line space property if and only if \a object
 *  is a valid GedaObject object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \return integer value of line space or -0 if \a object is invalid.
 *
 * \sa geda_line_object_set_line_space
 */
int geda_line_object_get_line_space (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line_options->line_space;
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve End Type Property of a Circle object
 * \par Function Description
 *  Returns the value of \a object line type if and only if \a object is
 *  a valid GedaObject object.
 *
 * \return integer value of line type or -0 if \a object is invalid.
 *
 * \sa geda_line_object_set_line_type
 */
int geda_line_object_get_line_type (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line_options->line_type;
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve End Width Property of a Circle object
 * \par Function Description
 *  Returns the value of the \a object line width property if and only if
 *  \a object is a valid GedaObject object.
 *
 * \return integer value of line width or -0 if \a object is invalid.
 *
 * \sa geda_line_object_set_line_width
 */
int geda_line_object_get_line_width (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line_options->line_width;
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Calculate the Midpoint of a linear GedaObject
 * \par Function Description
 *  This function calculates the midpoint of a linear object.
 *
 * \param [in]  object Linear object whose midpoint is to be determined
 * \param [out] point  The midpoint if \a object was linear
 *
 * \return TRUE if \a object is linear
 */
bool geda_line_object_get_midpoint(GedaObject *object, GedaPoint *point)
{
  bool status;

  if (GEDA_IS_LINE(object)) {
    point->x = (object->line->x[0] + object->line->x[1]) / 2;
    point->y = (object->line->y[0] + object->line->y[1]) / 2;
    status = TRUE;
  }
  else {
    geda_line_object_error(__func__, object);
    status = FALSE;
  }
  return status;
}

/*!
 * \brief Get Point on a Line that is Nearest a Given Point
 * \par Function Description
 *  This function is intended to locate a point on a Line object given
 *  a point \a x, \a y, that is on or about the vicinity of \a object. If
 *  True is returned, <B>nx</B> and <B>ny</B> are set to the point on the
 *  line that is the closest point on the line to the point given by \a x, \a y.
 *
 * \param [in]  object  Pointer to a Line object
 * \param [in]  x       Integer x of point near or on the line
 * \param [in]  y       Integer y of point near or on the line
 * \param [out] nx      Integer pointer to resulting x value
 * \param [out] ny      Integer pointer to resulting y value
 *
 * \returns TRUE is the results are valid, FALSE if \a object was not a Line.
 */
bool geda_line_object_get_nearest_point (const GedaObject *object, int x, int y, int *nx, int *ny)
{
  bool      result;
  int       ax, ay;

  if (GEDA_IS_LINE(object)) {

    GedaLine *line;

    line = object->line;

    if (line->x[0] == line->x[1]) {  /* The Line is vertical */

      int ymin = line->y[0] > line->y[1] ? line->y[1] : line->y[0];
      int ymax = line->y[0] > line->y[1] ? line->y[0] : line->y[1];

      ax = line->x[0];

      if (y >= ymax) {
        ay = ymax;
      }
      else if (y <= ymin) {
        ay = ymin;
      }
      else {
        ay = y;
      }
    }
    else if (line->y[0] == line->y[1]) {  /* The Line is horizontal */

      int xmin = line->x[0] > line->x[1] ? line->x[1] : line->x[0];
      int xmax = line->x[0] > line->x[1] ? line->x[0] : line->x[1];

      ay = line->y[0];

      if (x >= xmax) {
        ax = xmax;
      }
      else if (x <= xmin) {
        ax = xmin;
      }
      else {
        ax = x;
      }
    }
    else { /* The line is on non-zero angle*/

      double dx, dy, ix, iy;
      double m1, m2, b1, b2;
      GedaPoint  point;

      dx = line->x[1] - line->x[0];
      dy = line->y[1] - line->y[0];

      m1 = dy / dx;
      b1 = line->y[0] - m1 * line->x[0];
      m2 = -1 / m1;
      b2 = y - m2 * x;

      ix = (b2 - b1) / (m1 - m2);
      iy = m2 * ix + b2;;

#ifdef HAVE_LRINT

      point.x = lrint(ix);
      point.y = lrint(iy);

#else

      point.x = ix + 0.5;
      point.y = iy + 0.5;

#endif

      if (geda_math_line_includes_point(line, &point)) {
        ax = point.x;
        ay = point.y;
      }
      else {

        int index = geda_line_object_get_closest_endpoint(object, x, y);

        ax = line->x[index];
        ay = line->y[index];
      }
    }

    result = TRUE;
  }
  else { /* was not an Line */
    geda_line_object_error(__func__, object);
    result = FALSE;
  }

  if (!result) {
    ax = x;
    ay = y;
  }

  if (nx) {
    *nx = ax;
  }

  if (ny) {
    *ny = ay;
  }

  return result;
}

/*!
 * \brief get the position of the first line point
 * \par Function Description
 *  This function gets the position of the first point of a line object.
 *
 * \param [in] object  GedaLine object whose position is to be returned
 * \param [out] x      pointer to the x-position
 * \param [out] y      pointer to the y-position
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_line_object_get_position (GedaObject *object, int *x, int *y)
{
  if (GEDA_IS_LINE(object)) {

    if (x) *x = object->line->x[0];

    if (y) *y = object->line->y[0];

    return (x || y) ? TRUE : FALSE;
  }

  geda_line_object_error(__func__, object);

  return FALSE;
}

/*!
 * \brief Calculates the Slope of a Line GedaObject
 * \par Function Description
 *  This function calculates the slope of a line object
 *
 * \param [in]  object  A line Object
 * \param [out] slope   The slope if not infinite
 *
 * \return True if the slope was set, otherwise false
 */
bool geda_line_object_get_slope (GedaObject *object, double *slope)
{
  bool has_slope;

  if (GEDA_IS_LINE(object)) {

    has_slope = object->line->x[0] == object->line->x[1] ? FALSE : TRUE;

    if (has_slope) {

      double dx, dy;

      dy    = object->line->y[1] - object->line->y[0];
      dx    = object->line->x[1] - object->line->x[0];
     *slope = dy / dx;
    }
  }
  else {
    geda_line_object_error(__func__, object);
    has_slope = FALSE;
  }

  return has_slope;
}

/*!
 * \brief Get the First X coordinate of a Line object
 *
 * \param [in] object  Pointer to a Line GedaObject
 *
 * \return The x coordinate of the first point of a Line
 *
 * \sa geda_line_get_x1
 */
int geda_line_object_get_x1 (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line->x[0];
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Get the Second X coordinate of a Line object
 *
 * \param [in] object  Pointer to a Line GedaObject
 *
 * \return The x coordinate of the second point of a Line
 *
 * \sa geda_line_get_x2
 */
int geda_line_object_get_x2 (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line->x[1];
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Get the First Y coordinate of a Line object
 *
 * \param [in] object  Pointer to a Line GedaObject
 *
 * \return The Y coordinate of the first point of a Line
 *
 * \sa geda_line_get_y1
 */
int geda_line_object_get_y1 (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line->y[0];
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Get the Second Y coordinate of a Line object
 *
 * \param [in] object  Pointer to a Line GedaObject
 *
 * \return The y coordinate of the second point of a Line
 *
 * \sa geda_line_get_y2
 */
int
geda_line_object_get_y2 (const GedaObject *object)
{
  if (GEDA_IS_LINE(object)) {
    return object->line->y[1];
  }
  geda_line_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Is point an End Point of the given GedaLine
 * \par Function Description
 *  This function check if \a point is an end-point of \a object
 *
 * \param [in] object GedaLine object
 * \param [in] point  Point
 *
 * \return TRUE if point is an end-point of the line
 */
bool geda_line_object_is_endpoint (GedaObject *object, GedaPoint *point)
{
  bool anwser;

  if (GEDA_IS_LINE(object)) {
    if ((point->x == object->line->x[0] && point->y == object->line->y[0]) ||
        (point->x == object->line->x[1] && point->y == object->line->y[1]))
    {
      anwser = TRUE;
    }
    else {
      anwser = FALSE;
    }
  }
  else {
    geda_line_object_error(__func__, object);
    anwser = FALSE;
  }
  return anwser;
}

/*!
 * \brief calculate the length of a line object
 * \par Function Description
 *  This function calculates the length of a line object
 *
 * \param [in] object  a line Object
 *
 * \return The length of the line
 */
double geda_line_object_length(GedaObject *object)
{
  double length;

  if (GEDA_IS_LINE(object)) {

    double dx, dy;

    dx = object->line->x[0]-object->line->x[1];
    dy = object->line->y[0]-object->line->y[1];

#if HAVE_HYPOT
    length = hypot (dx, dy);
#else
    length = sqrt((dx*dx) + (dy*dy));
#endif

  }
  else {
    geda_line_object_error(__func__, object);
    length = 0.0;
  }
  return (length);
}

/*!
 * \brief Mirror a line
 * \par Function Description
 *  This function mirrors the line from the point
 *  (<B>center_x</B>,<B>center_y</B>) in world unit.
 *
 *  The line if first translated to the origin, then mirrored
 *  and finally translated back at its previous position.
 *
 * \param [in,out] object    GedaLine Object to mirror
 * \param [in]     center_x  Origin x coordinate
 * \param [in]     center_y  Origin y coordinate.
 */
void geda_line_object_mirror(GedaObject *object, int center_x, int center_y)
{
  g_return_if_fail(GEDA_IS_LINE(object));

  /* translate object to origin */
  geda_line_object_translate(object, -center_x, -center_y);

  /* mirror the line ends */
  object->line->x[0] = -object->line->x[0];
  object->line->x[1] = -object->line->x[1];

  /* translate back in position */
  geda_line_object_translate(object, center_x, center_y);
}

/*!
 * \brief Modify the description of a line Object.
 * \par Function Description
 *  This function modifies the coordinates of one of the two ends of
 *  the line described by <B>\a object</B>. The new coordinates of this end,
 *  identified by <B>\a whichone</B>, are given by <B>\a x</B> and <B>\a y</B>
 *  in world unit.
 *
 *  The coordinates of the end of line is modified in the world
 *  coordinate system. Screen coordinates and boundings are then updated.
 *
 * \param [in,out] object     GedaLine Object to modify.
 * \param [in]     x          New x coordinate.
 * \param [in]     y          New y coordinate.
 * \param [in]     whichone   Which line parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>#LINE_END1
 *    <DT>*</DT><DD>#LINE_END2
 *  </DL>
 */
void geda_line_object_modify(GedaObject *object, int x, int y, int whichone)
{
  g_return_if_fail(GEDA_IS_LINE(object));
  g_return_if_fail(whichone == LINE_END1 || whichone == LINE_END2);

  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  /* recalculate the bounding line */
  object->bounds_valid = FALSE;
}

/*!
 * \brief Create and add line Object to list
 * \par Function Description
 *  This function creates a new object representing a line.
 *
 *  The line is described by its two ends - <B>x1</B>,<B>y1</B> and
 *  <B>x2</B>,<B>y2</B>.
 *  The <B>color</B> parameter corresponds to the color the line
 *  will be drawn with.
 *
 *  The #GedaObject structure is allocated with the #geda_line_new()
 *  function. The structure describing the line is allocated and
 *  initialized with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default
 *  values : solid line type with a width of 0, and no filling.
 *  It can be changed after with the #geda_set_object_line_options() and
 *  #geda_set_object_fill_options().
 *
 * \param [in]     color        Circle line color.
 * \param [in]     x1           First x coordinate.
 * \param [in]     y1           First y coordinate.
 * \param [in]     x2           Second x coordinate.
 * \param [in]     y2           Second y coordinate.
 *
 * \return A pointer to the new end of the object list.
 */
GedaObject *geda_line_object_new( int color, int x1, int y1, int x2, int y2)
{
  GedaObject *new_obj;
  GedaLine   *line;

  new_obj = geda_line_new();

  line = GEDA_LINE(new_obj);

  new_obj->color = color;

  /* describe the line with its two ends */
  line->x[0] = x1;
  line->y[0] = y1;
  line->x[1] = x2;
  line->y[1] = y2;

  return new_obj;
}

/*!
 * \brief Print line to Postscript document.
 * \par Function Description
 *  This function prints the line described by the <B>\a o_current</B>
 *  parameter to a Postscript document.
 *  The Postscript document is described by the <B>fp</B> file pointer.
 *
 *  Parameters of the line are extracted from object pointed by
 *  <B>\a o_current</B>.
 *
 * \param [in] toplevel  The GedaToplevel object.
 * \param [in] fp         FILE pointer to Postscript document.
 * \param [in] o_current  GedaLine Object to write to document.
 * \param [in] origin_x   Page x coordinate to place line Object.
 * \param [in] origin_y   Page y coordinate to place line Object.
 */
void geda_line_object_print(GedaToplevel *toplevel, FILE *fp,
                            GedaObject   *o_current,
                            int           origin_x,
                            int           origin_y)
{
  int x1, y1, x2, y2;
  int color;
  int capstyle;
  int line_width, length, space;

  void (*outl_func)() = NULL;

  if (o_current == NULL) {
    printf("got null in geda_line_object_print\n");
    return;
  }

  x1    = o_current->line->x[0];
  y1    = o_current->line->y[0];
  x2    = o_current->line->x[1];
  y2    = o_current->line->y[1];
  color = o_current->color;
  capstyle = geda_object_get_capstyle (o_current->line_options->line_end);

  /*
   * Depending on the type of the line for this particular line, the
   * appropriate function is chosen among
   * #geda_line_object_print_solid(), #geda_line_object_print_dotted()#, #geda_line_object_print_dashed(),
   * #geda_line_object_print_center() and #geda_line_object_print_phantom().
   *
   * The needed parameters for each of these types are extracted from the
   * <B>\a o_current</B> object. Depending on the type, unused parameters are
   * set to -1.
   *
   * In the eventuality of a length and/or space null, the line is printed
   * solid to avoid and endless loop produced by other functions.
   */
  /* 09/08/12 | W.E.Hill Modified algorithms to incorperate both THICK & THIN
   *            styles, and eliminated hard-coded integer values.
   */
  line_width = o_current->line_options->line_width;

  if (line_width < MIN_LINE_WIDTH_THRESHOLD) {
    line_width = geda_object_style_get_line_width(toplevel); /* 1st try updating style */
  }

  if (line_width < MIN_LINE_WIDTH_THRESHOLD) {
      line_width = MIN_LINE_WIDTH_THRESHOLD;        /* if STYLE_NONE  */
  }

  length = o_current->line_options->line_length;
  space  = o_current->line_options->line_space;

  switch(o_current->line_options->line_type) {
    case(TYPE_SOLID):
      length = -1; space = -1;
      outl_func = geda_line_object_print_solid;
      break;

    case(TYPE_DOTTED):
      length = -1;
      outl_func = geda_line_object_print_dotted;
      break;

    case(TYPE_DASHED):
      outl_func = geda_line_object_print_dashed;
      break;

    case(TYPE_CENTER):
      outl_func = geda_line_object_print_center;
      break;

    case(TYPE_PHANTOM):
      outl_func = geda_line_object_print_phantom;
      break;

    case(TYPE_ERASE):
      /* Unused for now, print it solid */
      length = -1; space = -1;
      outl_func =  geda_line_object_print_solid;
      break;
  }

  if ((length == 0) || (space == 0)) {
    length = -1; space = -1;
    outl_func = geda_line_object_print_solid;
  }

  (*outl_func)(toplevel, fp,
               x1 - origin_x, y1 - origin_y,
               x2 - origin_x, y2 - origin_y,
               color,
               line_width, capstyle, length, space,
               origin_x, origin_y);
}

/*!
 * \brief Print a centered line type line to Postscript document.
 * \par Function Description
 *  This function prints a line when a centered line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 *  The function sets the color in which the line will be printed and the
 *  width of the line - that is the width of the dashes and the diameter
 *  of the dots.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x1            First x coordinate.
 * \param [in] y1            First y coordinate.
 * \param [in] x2            Second x coordinate.
 * \param [in] y2            Second y coordinate.
 * \param [in] color         Line color.
 * \param [in] line_width    Width of line.
 * \param [in] capstyle      Capstyle of line.
 * \param [in] length        Length of a dash.
 * \param [in] space         Space between dashes.
 * \param [in] origin_x      Page x coordinate to place line Object.
 * \param [in] origin_y      Page y coordinate to place line Object.
 */
void
geda_line_object_print_center(GedaToplevel *toplevel, FILE *fp,
                              int x1, int y1, int x2, int y2, int color,
                              int line_width, int capstyle,
                              int length,     int space,
                              int origin_x,   int origin_y)
{
  double dx, dy, l, d;
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;

  f_print_set_color(toplevel, fp, color);

  fprintf(fp, "[");

  /*
   * Depending on the slope of the line the <B>length</B> (resp. <B>space</B>)
   * parameter is projected on each of the two directions x and y resulting
   * in <B>dx1</B> and <B>dy1</B> (resp. <B>dx2</B> and <B>dy2</B>).
   * Starting from one end and incrementing alternatively by <B>space</B>
   * and <B>length</B> the dashes and dots are printed.
   *
   * It prints as many sets of dash and dot as possible.
   */
  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);

#if HAVE_HYPOT
  l = hypot (dx, dy);
#else
  l = sqrt((dx * dx) + (dy * dy));
#endif

  dx1 = (dx * length) / l;
  dy1 = (dy * length) / l;

  dx2 = (dx * space) / l;
  dy2 = (dy * space) / l;

  d  = 0;
  xa = x1; ya = y1;

  while((d + length + 2 * space) < l) {

    d  = d + length;
    xb = xa + dx1;
    yb = ya + dy1;

    fprintf(fp, "[%d %d %d %d] ", (int) xa, (int) ya, (int) xb, (int) yb);

    d  = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

    fprintf(fp,"[%d %d] ", (int) xa, (int) ya);

    d  = d + space;
    xa = xa + dx2;
    ya = ya + dy2;
  }

  /*
   * When the above condition is no more satisfied, then it is not possible
   * to print a dash of length <B>length</B>.
   * However two cases are possible :
   * <DL>
   *   <DT>*</DT><DD>it is possible to print the dash and the dot.
   *   <DT>*</DT><DD>it is possible to print the dash or a part
   *                 of the original dash.
   * </DL>
   */

  if ((d + length + space) < l) {

    d  = d + length;
    xb = xa + dx1;
    yb = ya + dy1;

    fprintf(fp, "[%d %d %d %d] ", (int) xa, (int) ya, (int) xb, (int) yb);

    d  = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

    fprintf(fp,"[%d %d] ", (int) xa, (int) ya);

  }
  else {

    if (d + length < l) {
      xb = xa + dx1;
      yb = ya + dy1;
    }
    else {
      xb = x2;
      yb = y2;
    }

    fprintf(fp, "[%d %d %d %d] ", (int) xa, (int) ya, (int) xb, (int) yb);

  }

  fprintf(fp,"] %d %d dashed\n", line_width, capstyle);

  /* A dot is represented by a filled circle. Position of the circle is
   * (<B>xa</B>, <B>ya</B>) and its radius by the <B>line_width</B> parameter.
   */
}

/*!
 * \brief Print a dashed line to Postscript document.
 * \par Function Description
 *  This function prints a line when a dashed line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 *  The function sets the color in which the line will be printed and
 *  the width of the line - that is the width of the dashes.
 *
 * \param [in] toplevel      The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x1            First x coordinate.
 * \param [in] y1            First y coordinate.
 * \param [in] x2            Second x coordinate.
 * \param [in] y2            Second y coordinate.
 * \param [in] color         Line color.
 * \param [in] line_width    Width of line.
 * \param [in] capstyle      Capstyle of line.
 * \param [in] length        Length of a dash.
 * \param [in] space         Space between dashes.
 * \param [in] origin_x      Page x coordinate to place line Object.
 * \param [in] origin_y      Page y coordinate to place line Object.
 */
void
geda_line_object_print_dashed(GedaToplevel *toplevel, FILE *fp,
                              int x1, int y1, int x2, int y2, int color,
                              int line_width, int capstyle,
                              int length,     int space,
                              int origin_x,   int origin_y)
{
  double dx, dy, l, d;
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;

  f_print_set_color(toplevel, fp, color);

  /* the dashed line function takes an array of start-finish pairs
   * output the beginnings of the array now
   */
  fprintf(fp,"[");

  /*
   * Depending on the slope of the line the <B>length</B> (resp. <B>space</B>)
   * parameter is projected on each of the two directions x and y
   * resulting in <B>dx1</B> and <B>dy1</B> (resp. <B>dx2</B> and <B>dy2</B>).
   * Starting from one end and incrementing alternatively by <B>space</B>
   * and <B>length</B> the dashes are printed.
   *
   * It prints as many dashes of length <B>length</B> as possible.
   */
  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);

#if HAVE_HYPOT
  l = hypot (dx, dy);
#else
  l = sqrt ((dx * dx) + (dy * dy));
#endif

  dx1 = (dx * length) / l;
  dy1 = (dy * length) / l;

  dx2 = (dx * space) / l;
  dy2 = (dy * space) / l;

  d  = 0;
  xa = x1; ya = y1;

  while((d + length + space) < l) {

    d  = d + length;
    xb = xa + dx1;
    yb = ya + dy1;

    fprintf(fp, "[%d %d %d %d] ", (int) xa, (int) ya, (int) xb, (int) yb);

    d  = d + space;
    xa = xb + dx2;
    ya = yb + dy2;
  }

  /*
   * When the above condition is no more satisfied, then it is not possible
   * to print a dash of length <B>length</B>. However it may be possible to
   * print the complete dash or a shorter one.
   */

  if ((d + length) < l) {
    xb = xa + dx1;
    yb = ya + dy1;
  }
  else {
    xb = x2;
    yb = y2;
  }

  fprintf(fp, "[%d %d %d %d] ", (int) xa, (int) ya, (int) xb, (int) yb);

  fprintf(fp,"] %d %d dashed\n", line_width, capstyle);
}

/*!
 * \brief Print a dotted line to Postscript document.
 * \par Function Description
 *  This function prints a line when a dotted line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *  The parameter <B>length</B> is ignored whereas <B>line_width</B>
 *  specifies the diameter of the dots and <B>space</B> the distance
 *  between two dots.
 *
 *  A negative value for <B>space</B> leads to an endless loop.
 *
 *  All dimensions are in mils.
 *
 *  The function sets the color in which the line will be printed with.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x1            First x coordinate.
 * \param [in] y1            First y coordinate.
 * \param [in] x2            Second x coordinate.
 * \param [in] y2            Second y coordinate.
 * \param [in] color         Line color.
 * \param [in] line_width    Width of line.
 * \param [in] capstyle      Capstyle of circle lines.
 * \param [in] length        (unused).
 * \param [in] space         Space between dots.
 * \param [in] origin_x      Page x coordinate to place line Object.
 * \param [in] origin_y      Page y coordinate to place line Object.
 */
void
geda_line_object_print_dotted(GedaToplevel *toplevel, FILE *fp,
                              int x1, int y1, int x2, int y2, int color,
                              int line_width, int capstyle,
                              int length,     int space,
                              int origin_x,   int origin_y)
{
  double dx, dy, l, d;
  double dx1, dy1;
  double xa, ya;

  f_print_set_color(toplevel, fp, color);

  /* The dotted line command takes an array of dots so print out the
   * beginnings of the array
   */
  fprintf(fp,"[");
  /* is the width relevant for a dot (circle) ? */
  /* f_print_set_line_width(fp, line_width); */

  /*
   * Depending on the slope of the line the space parameter is
   * projected on each of the two directions x and y resulting
   * in <B>dx1</B> and <B>dy1</B>. Starting from one end by increments
   * of space the dots are printed.
   *
   * A dot is represented by a filled circle. Position of the
   * circle is (<B>xa</B>, <B>ya</B>) and its radius is the <B>line_width</B>
   * parameter.
   */

  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);

#if HAVE_HYPOT
  l = hypot (dx, dy);
#else
  l = sqrt ((dx * dx) + (dy * dy));
#endif

  dx1 = (dx * space) / l;
  dy1 = (dy * space) / l;

  d = 0;
  xa = x1; ya = y1;

  while(d < l) {

    fprintf(fp,"[%d %d] ", (int) xa, (int) ya);
    d = d + space;
    xa = xa + dx1;
    ya = ya + dy1;
  }

  fprintf(fp,"] %d %d dashed\n", line_width, capstyle);
}

/*!
 * \brief Print a phantom line type line to Postscript document.
 * \par Function Description
 *  This function prints a line when a phantom line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 *  The function sets the color in which the line will be printed and the
 *  width of the line - that is the width of the dashes and the diameter
 *  of the dots.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x1            First x coordinate.
 * \param [in] y1            First y coordinate.
 * \param [in] x2            Second x coordinate.
 * \param [in] y2            Second y coordinate.
 * \param [in] color         Line color.
 * \param [in] line_width    Width of line.
 * \param [in] capstyle      Capstyle of line.
 * \param [in] length        Length of a dash.
 * \param [in] space         Space between dashes.
 * \param [in] origin_x      Page x coordinate to place line Object.
 * \param [in] origin_y      Page y coordinate to place line Object.
 */
void
geda_line_object_print_phantom(GedaToplevel *toplevel, FILE *fp,
                               int x1, int y1, int x2, int y2, int color,
                               int line_width, int capstyle,
                               int length,     int space,
                               int origin_x,   int origin_y)
{
  double dx, dy, l, d;
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;

  f_print_set_color(toplevel, fp, color);

  fprintf(fp,"[");

  /*
   * Depending on the slope of the line the <B>length</B> (resp. <B>space</B>)
   * parameter is projected on each of the two directions x and y resulting
   * in <B>dx1</B> and <B>dy1</B> (resp. <B>dx2</B> and <B>dy2</B>).
   * Starting from one end and incrementing alternatively by <B>space</B>
   * and <B>length</B> the dashes and dots are printed.
   *
   * It prints as many sets of dash-dot-dot as possible.
   */
  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);

#if HAVE_HYPOT
  l = hypot (dx, dy);
#else
  l = sqrt((dx * dx) + (dy * dy));
#endif

  dx1 = (dx * length) / l;
  dy1 = (dy * length) / l;

  dx2 = (dx * space) / l;
  dy2 = (dy * space) / l;

  d  = 0;
  xa = x1; ya = y1;

  while((d + length + 3 * space) < l) {

    d  = d + length;
    xb = xa + dx1;
    yb = ya + dy1;

    fprintf(fp,"[%d %d %d %d] ", (int) xa, (int)ya, (int) xb, (int)yb);

    d  = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

    fprintf(fp,"[%d %d] ",(int) xa, (int) ya);

    d  = d + space;
    xa = xa + dx2;
    ya = ya + dy2;

    fprintf(fp,"[%d %d] ",(int) xa, (int) ya);

    d  = d + space;
    xa = xa + dx2;
    ya = ya + dy2;
  }

  /*
   * When the above condition is no more satisfied, then it is not possible
   * to print a complete set of dash-dot-dot.
   * However three cases are possible :
   * <DL>
   *   <DT>*</DT><DD>it is possible to print a dash and a dot and a dot.
   *   <DT>*</DT><DD>it is possible to print a dash and a dot.
   *   <DT>*</DT><DD>it is possible to print the dash or a part
   *                 of the original dash.
   * </DL>
   */

  if ((d + length + 2 * space) < l) {

    d  = d + length;
    xb = xa + dx1;
    yb = ya + dy1;

    fprintf(fp,"[%d %d %d %d] ", (int) xa, (int) ya, (int) xb, (int) yb);

    d  = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

    fprintf(fp,"[%d %d] ", (int) xa, (int) ya);

    d  = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

    fprintf(fp,"[%d %d] ", (int) xa, (int) ya);

  }
  else {

    if (d + length + space < l) {

      d  = d + length;
      xb = xa + dx1;
      yb = ya + dy1;

      fprintf(fp,"[%d %d %d %d] ", (int) xa, (int) ya, (int) xb, (int) yb);

      d  = d + space;
      xa = xb + dx2;
      ya = yb + dy2;

      fprintf(fp,"[%d %d] ",(int) xa, (int) ya);

    }
    else {

      if (d + length < l) {
        xb = xa + dx1;
        yb = ya + dy1;
      }
      else {
        xb = x2;
        yb = y2;
      }

      fprintf(fp,"[%d %d %d %d] ",
              (int) xa, (int)ya,
              (int) xb, (int)yb);

    }
  }

  fprintf(fp,"] %d %d dashed\n", line_width, capstyle);
}

/*!
 * \brief Print a solid line to Postscript document.
 * \par Function Description
 *  This function prints a line when a solid line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> and <B>space</B> are ignored whereas
 *  <B>line_width</B> specifies the width of the printed line.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x1            First x coordinate.
 * \param [in] y1            First y coordinate.
 * \param [in] x2            Second x coordinate.
 * \param [in] y2            Second y coordinate.
 * \param [in] color         Line color.
 * \param [in] line_width    Width of line.
 * \param [in] capstyle      Capstyle of line.
 * \param [in] length        (unused).
 * \param [in] space         (unused).
 * \param [in] origin_x      Page x coordinate to place line Object.
 * \param [in] origin_y      Page y coordinate to place line Object.
 */
void
geda_line_object_print_solid(GedaToplevel *toplevel, FILE *fp,
                             int x1, int y1, int x2, int y2, int color,
                             int line_width, int capstyle,
                             int length,     int space,
                             int origin_x,   int origin_y)
{
  f_print_set_color(toplevel, fp, color);

  fprintf(fp,"%d %d %d %d %d %d line\n", x1,y1,x2,y2, line_width, capstyle);
}

/*!
 * \brief Create line Object from character string.
 *
 * \par Function Description
 *  This function creates a line Object from the character string
 *  <B>*buf</B> the description of a line.
 *
 *  The function returns a pointer on the new last element, that is
 *  the added line object.
 *
 *  Depending on <B>*version</B>, the correct file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 20010704 release.
 *    <DT>*</DT><DD>the file format used for the releases after 20010704.
 *  </DL>
 *
 * \param [in]  buf             Character string with line description.
 * \param [in]  release_ver     libgeda release version number.
 * \param [in]  fileformat_ver  libgeda file format version number.
 *
 * \param [out] err             A GError obejct
 *
 * \return A pointer to the new line object, or NULL on error.
 */
GedaObject*
geda_line_object_read (const char buf[], unsigned int release_ver,
                                         unsigned int fileformat_ver,
                                         GError ** err)
{
  GedaObject *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int line_width, line_space, line_length;
  int line_end;
  int line_type;
  int color;

  if (release_ver <= VERSION_20000704) {
    /*
     * The old geda file format, i.e. releases 20000704 and older, does
     * not handle the line type and the filling - here filling is irrelevant.
     * They are set to default.
     */
    if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6)
    {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse line object"));
      return NULL;
    }

      line_width = 0;
      line_end   = END_NONE;
      line_type  = TYPE_SOLID;
      line_length= -1;
      line_space = -1;
  }
  else {
    /*
     * The current line format to describe a line is a space separated
     * list of characters and numbers in plain ASCII on a single line.
     * The meaning of each item is described in the file format documentation.
     */
    if (sscanf (buf, "%c %d %d %d %d %d %d %d %d %d %d\n", &type,
        &x1, &y1, &x2, &y2, &color,
        &line_width, &line_end, &line_type, &line_length, &line_space) != 11)
    {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse line object"));
      return NULL;
    }
    /* TODO Should check line type here! */
  }

  /*
   * Null length line are not allowed. If such a line is detected a
   * message is issued.
   *
   * It also checks is the required color is valid.
   */
  if (x1 == x2 && y1 == y2) {
    const char *msg = _("Found a line with zero length");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: (%d, %d) (%d, %d).\n", msg, x1, y1, x2, y2);
    }
  }

  if (color < 0 || color > MAX_COLORS) {
    const char *msg = _("Found an invalid color");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, color);
    }
    geda_log_w (_("Setting color to default color\n"));
    color = DEFAULT_LINE_COLOR_INDEX;
  }

  /*
   * A line is internally described by its two ends. A new object is
   * allocated, initialized and added to the list of objects. Its line
   * type is set according to the values of the fields on the line.
   */
  /* create and add the line to the list */
  new_obj = geda_line_object_new (color, x1, y1, x2, y2);

  /* set line options */
  new_obj->line_options->line_end    = line_end;
  new_obj->line_options->line_type   = line_type;
  new_obj->line_options->line_width  = line_width;
  new_obj->line_options->line_length = line_length;
  new_obj->line_options->line_space  = line_space;

  return new_obj;
}

/*!
 * \brief Rotate a GedaLine Object
 * \par Function Description
 *  This function rotates the line described by
 *  <B>*object</B> around the (<B>center_x</B>,<B>center_y</B>)
 *  point by <B>angle</B> degrees.
 *  The center of rotation is in world units.
 *
 * \param [in,out]  object    GedaLine Object to rotate
 * \param [in]      center_x  Rotation center x coordinate
 * \param [in]      center_y  Rotation center y coordinate
 * \param [in]      angle     Rotation angle in degrees (See note below).
 */
void
geda_line_object_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  if (GEDA_IS_LINE(object)) {

    GedaLine *line = GEDA_LINE(object);
    int newx, newy;

    if (angle == 0)
      return;

    /* angle must be positive */
    /*if (angle < 0) angle = -angle;*/

    /* angle must be 90 multiple or no rotation performed */
    if ((angle % 90) != 0) {
      fprintf(stderr, "%s angle must be multiple of 90 <%d>\n", __func__, angle);
      return;
    }

    /*
     * The center of rotation (<B>center_x</B>,<B>center_y</B>)
     * is translated to the origin. The rotation of the two ends of
     * the line is performed. Finally, the rotated line is translated
     * back to its previous location.
     */
    /* translate object to origin */
    geda_line_object_translate(object, -center_x, -center_y);

    /* rotate line end 1 */
    geda_math_rotate_point_90(line->x[0], line->y[0], angle, &newx, &newy);

    line->x[0] = newx;
    line->y[0] = newy;

    /* rotate line end 2 */
    geda_math_rotate_point_90(line->x[1], line->y[1], angle, &newx, &newy);

    line->x[1] = newx;
    line->y[1] = newy;

    /* translate object back to normal position */
    geda_line_object_translate(object, center_x, center_y);
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Scale a GedaLine object
 * \par Function Description
 *
 * \param [in] object
 * \param [in] x_scale
 * \param [in] y_scale
 */
void
geda_line_object_scale(GedaObject *object, int x_scale, int y_scale)
{
  if (GEDA_IS_LINE(object)) {

    object->line->x[0] = object->line->x[0] * x_scale;
    object->line->y[0] = object->line->y[0] * y_scale;
    object->line->x[1] = object->line->x[1] * x_scale;
    object->line->y[1] = object->line->y[1] * y_scale;

    /* update boundingline */
    object->bounds_valid = FALSE;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set the End Cap type Property of a GedaLine
 * \par Function Description
 *  Sets the value of \a object end-cap type if and only if \a object is
 *  a valid GedaLine object. The line-end properties is only applicable
 *  for fill types FILLING_MESH and FILLING_HATCH.
 *
 * \sa geda_line_object_get_end_cap
 */
void geda_line_object_set_end_cap (GedaObject *object, int line_end)
{
  if (GEDA_IS_LINE(object)) {
    object->line_options->line_end = line_end < END_NONE ? END_NONE :
                                     line_end > END_VOID ? END_VOID :
                                     line_end;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Line Length Property of an GedaObject
 * \par Function Description
 *  Returns the value of the \a object line length property if and only if
 *  \a object is a valid GedaObject object. The line-length property controls
 *  the length of line segments for line types dashed, center and phantom.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \param [in] object  Pointer to an GedaLine Object
 * \param [in] length  new value for the line-length property
 *
 * \sa geda_line_object_get_line_length
 */
void geda_line_object_set_line_length (GedaObject *object, int length)
{
  if (GEDA_IS_LINE(object)) {
    object->line_options->line_length = length > 0 ? length : 0;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Line Space Property of a GedaLine
 * \par Function Description
 *  Sets the value of the \a object line space property if and only if \a object
 *  is a valid GedaObject object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \param [in] object  Pointer to an GedaLine Object
 * \param [in] space   new LINE_TYPE value for the line type
 *
 * \sa geda_line_object_get_line_space
 */
void geda_line_object_set_line_space (GedaObject *object, int space)
{
  if (GEDA_IS_LINE(object)) {
    object->line_options->line_space = space > 0 ? space : 0;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Line Type Property of a GedaObject
 * \par Function Description
 *  Sets the value of \a object line type if and only if \a object is a
 *  valid GedaObject object.
 *
 * \param [in] object  Pointer to an GedaLine Object
 * \param [in] type    new LINE_TYPE value for the line type
 *
 * \sa geda_line_object_get_line_type
 */
void geda_line_object_set_line_type (GedaObject *object, int type)
{
  if (GEDA_IS_LINE(object)) {
    object->line_options->line_type = type < TYPE_SOLID ? TYPE_SOLID :
                                      type > TYPE_ERASE ? TYPE_ERASE :
                                      type;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set the End Width Property of a GedaLine
 * \par Function Description
 *  Sets the value of the \a object line width property if and only if
 *  \a object is a valid GedaLine object.
 *
 * \param [in] object  Pointer to an GedaLine Object
 * \param [in] width   new value for the line width
 *
 * \sa geda_line_object_get_line_width
 */
void geda_line_object_set_line_width (GedaObject *object, int width)
{
  if (GEDA_IS_LINE(object)) {
    object->line_options->line_width = width > 0 ? width : 0;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set First X coordinate of a Line object
 * \par Function Description
 *  Sets the first x value of the line \a object. Does nothing if
 *  \a object is not a valid GedaLine object.
 *
 * \param [in] object  Pointer to an Line GedaObject
 * \param [in] x       New value for the first x coordinate
 *
 * \sa geda_line_set_x1 geda_line_object_get_x1
 */
void
geda_line_object_set_x1 (GedaObject *object, int x) {
  if (GEDA_IS_LINE(object)) {
    object->line->x[0] = x;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set Second X coordinate of a Line obje
 * \par Function Description
 *  Sets the second x value of the line \a object. Does nothing if
 *  \a object is not a valid GedaLine object.
 *
 * \param [in] object  Pointer to an Line GedaObject
 * \param [in] x       New value for the second x coordinate
 *
 * \sa geda_line_set_x2 geda_line_object_get_x2
 */
void geda_line_object_set_x2 (GedaObject *object, int x) {
  if (GEDA_IS_LINE(object)) {
    object->line->x[1] = x;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set First Y coordinate of a Line obje
 * \par Function Description
 *  Sets the second y value of the line \a object. Does nothing if
 *  \a object is not a valid GedaLine object.
 *
 * \param [in] object  Pointer to an Line GedaObject
 * \param [in] y       New value for the second y coordinate
 *
 * \sa geda_line_set_y1 geda_line_object_get_y1
 */
void geda_line_object_set_y1 (GedaObject *object, int y) {
  if (GEDA_IS_LINE(object)) {
    object->line->y[0] = y;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Set Second Y coordinate of a Line object
 * \par Function Description
 *  Sets the second y value of the line \a object. Does nothing if
 *  \a object is not a valid GedaLine object.
 *
 * \param [in] object  Pointer to an Line GedaObject
 * \param [in] y       New value for the second y coordinate
 *
 * \sa geda_line_set_y2
 */
void geda_line_object_set_y2 (GedaObject *object, int y) {
  if (GEDA_IS_LINE(object)) {
    object->line->y[1] = y;
  }
  else {
    geda_line_object_error(__func__, object);
  }
}

/*!
 * \brief Calculates the distance between the given point and the closest
 *  point on the centerline of the given line segment.
 *
 *  If the closest point on the line resides beyond the line segment's
 *  end point, this function returns the distance from the given point to the
 *  closest end point.
 *
 *  If the line represents a single point (the endpoints are the same), this
 *  function calcualtes the distance to that point.
 *
 * \param [in] object       A line Object.
 * \param [in] x            The x coordinate of the given point.
 * \param [in] y            The y coordinate of the given point.
 * \param [in] force_solid  If true, force treating the object as solid.
 *
 * \return The shortest distance from the object to the point. With an
 *         invalid parameter, this function returns G_MAXDOUBLE.
 */
double geda_line_object_shortest_distance (ConstObject *object, int x, int y, int force_solid)
{
  g_return_val_if_fail (GEDA_IS_LINE(object), G_MAXDOUBLE);

  return geda_math_line_shortest_distance (object->line, x, y);
}

/*!
 * \brief Create a character string representation of a line Object.
 * \par Function Description
 *  The function formats a string in the buffer <B>*buff</B> to describe
 *  the line <B>*object</B> following the post-20000704 release file format
 *  that handle the line type and fill options - filling is irrelevant here.
 *
 * \note object was validated by geda_object_save_objects
 *
 * \param [in] object  GedaLine Object to create string from.
 *
 * \return A pointer to the line Object character string.
 *
 * \note Caller must GEDA_FREE returned character string.
 */
char *geda_line_object_to_buffer(GedaObject *object)
{
  char     *buf;
  GedaLine *line;
  LINE_END  line_end;
  LINE_TYPE line_type;
  int line_width, line_space, line_length;
  int x1, x2, y1, y2;

  line = GEDA_LINE(object);

  /* get the two ends */
  x1 = line->x[0];
  y1 = line->y[0];
  x2 = line->x[1];
  y2 = line->y[1];

  /* description of the line type */
  line_width = line->line_options.line_width;
  line_end   = line->line_options.line_end;
  line_type  = line->line_options.line_type;
  line_length= line->line_options.line_length;
  line_space = line->line_options.line_space;

  buf = geda_sprintf("%c %d %d %d %d %d %d %d %d %d %d", object->type,
                          x1, y1, x2, y2, object->color,
                          line_width, line_end, line_type,
                          line_length, line_space);

  return(buf);
}

/*!
 * \brief Translate a line position in by a delta
 * \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the line
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 * \param [in,out] object     GedaLine Object to translate
 * \param [in]     dx         x distance to move
 * \param [in]     dy         y distance to move.
 */
void geda_line_object_translate( GedaObject *object, int dx, int dy)
{
  g_return_if_fail(GEDA_IS_LINE(object));

  GedaLine *line = GEDA_LINE(object);

  /* Update world coords */
  line->x[0] = line->x[0] + dx;
  line->y[0] = line->y[0] + dy;
  line->x[1] = line->x[1] + dx;
  line->y[1] = line->y[1] + dy;

  /* Update bounding line */
  object->bounds_valid = FALSE;
}

/** @} endgroup geda-line-object-proc */
