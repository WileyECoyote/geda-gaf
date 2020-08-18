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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*! \file o_circle_object.c
 *  \brief functions for the circle object
 */

#include "../../../config.h"

#include <stdio.h>
#include <math.h>

#include <libgeda_priv.h>

/** \defgroup geda-circle-object-proc GedaCircle Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaCircle Objects
 */

static void
geda_object_error(const char *func, const void *object, IDE_OBJECT_TYPE type)
{
  geda_error_object_argument(__FILE__, func, object, type);
}

static void
geda_circle_object_error(const char *func, const void *object)
{
  geda_object_error(func, object, GEDA_OBJECT_CIRCLE);
}

/*!
 * \brief Create a copy of a circle.
 * \par Function Description
 *  The function #geda_circle_object_copy() creates a verbatim copy of the object
 *  pointed by <B>\a o_current</B> describing a circle.
 *
 * \param [in]  o_current  Circle GedaObject to copy.
 *
 * \return The new GedaObject
 */
GedaObject *
geda_circle_object_copy(GedaObject *o_current)
{
  if (GEDA_IS_CIRCLE(o_current)) {

    GedaObject *new_obj;
    GedaCircle *old_circle;

    old_circle = GEDA_CIRCLE(o_current);

    /* A new circle object is created with #geda_circle_object_new().
     * Values for its fields are default and need to be modified. */
    new_obj = geda_circle_object_new (o_current->color, 0, 0, 0);

    /* The parameters of the new circle are set with the ones of the original
     * circle. The two circle have the same line type and filling options.
     */

    new_obj->circle->center_x = old_circle->center_x;
    new_obj->circle->center_y = old_circle->center_y;
    new_obj->circle->radius   = old_circle->radius;

    geda_set_object_line_options(new_obj, &old_circle->line_options);
    geda_set_object_fill_options(new_obj, &old_circle->fill_options);

    return new_obj;
  }
  geda_circle_object_error(__func__, o_current);
  return NULL;
}

/*! O060?
 * \brief Get the center X coordinate of a GedaCircle object
 * \par Function Description
 *  Retrieves the circle center X property.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 *
 * \return The center X coordinate of the circle
 *
 * \sa geda_circle_object_set_center_x
 */
int
geda_circle_object_get_center_x (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->circle->center_x;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*! O060?
 * \brief Get the center Y coordinate of a GedaCircle object
 * \par Function Description
 *  Retrieves the circle center Y property.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 *
 * \return The center Y coordinate of the circle
 *
 * \sa geda_circle_object_set_center_y
 */
int
geda_circle_object_get_center_y (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->circle->center_y;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve End Cap type Property of an GedaObject
 * \par Function Description
 *  Returns the value of \a object end-cap type if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \return integer value of end-cap type or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_end_cap
 */
int
geda_circle_object_get_end_cap (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->line_options->line_end;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Angle 1 Property of a Circle object
 * \par Function Description
 *  Returns the value of \a object fill angle 1 if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill angle 1 or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_fill_angle1
 */
int
geda_circle_object_get_fill_angle1 (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->fill_options->fill_angle1;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Angle 2 Property of a Circle object
 * \par Function Description
 *  Returns the value of \a object fill angle 2 if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill angle 2 or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_fill_angle2
 */
int
geda_circle_object_get_fill_angle2 (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->fill_options->fill_angle2;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Pitch 1 Property of a Circle object
 * \par Function Description
 *  Returns the value of \a object fill pitch 1 if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill pitch 1 or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_fill_pitch1
 */

int
geda_circle_object_get_fill_pitch1 (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->fill_options->fill_pitch1;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Pitch 2 Property of a Circle object
 * \par Function Description
 *  Returns the value of \a object fill pitch 2 if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill pitch 2 or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_fill_pitch2
 */
int
geda_circle_object_get_fill_pitch2 (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->fill_options->fill_pitch2;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Type Property of a Circle object
 * \par Function Description
 *  Returns the value of \a object fill type if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill type or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_fill_type
 */
int
geda_circle_object_get_fill_type (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->fill_options->fill_type;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Width Property of a Circle object
 * \par Function Description
 *  Returns the value of the \a object fill width property if and only
 *  if \a object is a valid GedaCircle object.
 *
 * \return integer value of fill width or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_fill_width
 */
int
geda_circle_object_get_fill_width (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->fill_options->fill_width;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Line Length Property of a Circle object
 * \par Function Description
 *  Returns the value of the \a object line length property if \a object
 *  is a valid GedaCircle object. The line-length property controls the
 *  length of line segments for line types dashed, center and phantom,
 *  to get the "length" of a line see geda_math_line_length.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \return integer value of line length or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_line_length
 */
int
geda_circle_object_get_line_length (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->line_options->line_length;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Line Space Property of a Circle object
 * \par Function Description
 *  Returns the value of the \a object line space property if and only if \a object
 *  is a valid GedaCircle object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \return integer value of line space or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_line_space
 */
int
geda_circle_object_get_line_space (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->line_options->line_space;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve End Type Property of a Circle object
 * \par Function Description
 *  Returns the value of \a object line type if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \return integer value of line type or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_line_type
 */
int
geda_circle_object_get_line_type (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->line_options->line_type;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve End Width Property of a Circle object
 * \par Function Description
 *  Returns the value of the \a object line width property if and only if
 *  \a object is a valid GedaCircle object.
 *
 * \return integer value of line width or -0 if \a object is invalid.
 *
 * \sa geda_circle_object_set_line_width
 */
int
geda_circle_object_get_line_width (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->line_options->line_width;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Get Point on a Circle Nearest a Given Point
 * \par Function Description
 *  This function is intended to locate a point on an Circle object given
 *  a point \a x, \a y, that is on or about the vicinity of the \a object.
 *  If True is returned, <B>nx</B> and <B>ny</B> are set world unit to a
 *  point on the circle that is the closest point on \a object to the point
 *  given by \a x, \a y.
 *
 * \param [in]  object  Pointer to an Circle object
 * \param [in]  x       Integer x of point near or on the circle
 * \param [in]  y       Integer y of point near or on the circle
 * \param [out] nx      Integer pointer to resulting x value
 * \param [out] ny      Integer pointer to resulting y value
 *
 * \returns TRUE is the results are valid, FALSE if \a object was not an
 *          Circle object, or if (<B>dx</B>,<B>dy</B>) is the centerpoint
 *          of the circle.
 */
bool
geda_circle_object_get_nearest_point (const GedaObject *object, int x, int y, int *nx, int *ny)
{
  bool    result;

  if (GEDA_IS_CIRCLE(object)) {

    int cx, cy;

    cx = object->circle->center_x;
    cy = object->circle->center_y;

    /* If the point is the center, every point on the circle is equal
     * distance to the point, so answer is false */
    if ((y == cy) && (x == cx)) {
      result = FALSE;
    }
    else {

      if (x == cx) {      /* On vertical line through center point */

        *nx = x;

        if (y > cy) {
          *ny = cy + object->circle->radius;
        }
        else {
          *ny = cy - object->circle->radius;
        }
      }
      else if (y == cy) { /* On horizontal line through center point */

        *ny = y;

        if (x > cx) {
          *nx = cx + object->circle->radius;
        }
        else {
          *nx = cx - object->circle->radius;
        }
      }
      else {

        double  dx, dy, r;
        double  A, /*B*/ C, D;
        double  tmp_x, tmp_y;

        dx = cx - x;
        dy = cy - y;

        r  = object->circle->radius;

        volatile double  b;
        volatile double  m;

        /* Conventional: (x - cx)^2 + (mx + b - cy)^2 = r^2, solve for x
         * note: calculating as if the circle is at the origin, cx = cy = 0,
         * to prevent over-flow errors for circles > ~32k from origin */

        /* get slope of line connecting the point to the center of the circle */
        m = dy / dx;

        A = m * m + 1;
        C = -1 * r * r;

        /* The D = (B * B) - (4 * A * C) reduces to */
        D = 0 - (4 * A * C);                         /* The discriminant */

        if (cx > x) {                                /* Easterly */
          tmp_x = (0 - sqrt(D)) / (2 * A);
        }
        else {                                       /* Westward */
          tmp_x = sqrt(D) / (2 * A);
        }

        tmp_x = tmp_x + cx;                          /* Add offset to true cx */

        /* Using original x, y to get original intercept */
        b = y - m * x;

        tmp_y = m * tmp_x + b;

#ifdef HAVE_LRINT

        *nx = lrint(tmp_x);
        *ny = lrint(tmp_y);

#else

        *nx = tmp_x + 0.5;
        *ny = tmp_y + 0.5;

#endif
      }
      result = TRUE;
    }
  }
  else { /* was not an Circle */
    geda_circle_object_error(__func__, object);
    result = FALSE;
  }

  if (!result) {
    *nx = x;
    *ny = y;
  }
  return result;
}

/*!
 * \brief get the position of the center point
 * \par Function Description
 *  This function gets the position of the center point of a circle object.
 *
 * \param [in]  object  Pointer to a #GedaObject object
 * \param [out] x       pointer to the x-position
 * \param [out] y       pointer to the y-position

 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool
geda_circle_object_get_position (GedaObject *object, int *x, int *y)
{
  if (GEDA_IS_CIRCLE(object)) {

    if (x) *x = object->circle->center_x;

    if (y) *y = object->circle->center_y;
    return (x || y) ? TRUE : FALSE;
  }
  geda_circle_object_error(__func__, object);
  return FALSE;
}

/*! O060?
 * \brief Get the Radius of a GedaCircle object
 * \par Function Description
 *  Retrieves the circle radius property.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 *
 * \return The radius of the circle
 *
 * \sa geda_circle_object_set_radius
 */
int
geda_circle_object_get_radius (const GedaObject *object)
{
  if (GEDA_IS_CIRCLE(object)) {
    return object->circle->radius;
  }
  geda_circle_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Mirror a Circle.
 * \par Function Description
 *  This function recalculates the screen coords of the <B>\a o_current</B> pointed
 *  circle object from its world coords.
 *
 *  The circle coordinates and its bounding are recalculated as well as the
 *  GedaObject specific (line width, filling ...).
 *
 * \param [in,out] object    Circle GedaObject to mirror
 * \param [in]     center_x  Origin x coordinate
 * \param [in]     center_y  Origin y coordinate
 */
void
geda_circle_object_mirror(GedaObject *object, int center_x, int center_y)
{
  if (GEDA_IS_CIRCLE(object)) {

    /* translate object to origin */
    object->circle->center_x -= center_x;
    object->circle->center_y -= center_y;

    /* mirror the center of the circle */
    object->circle->center_x = -object->circle->center_x;

    /* translate back in position */
    object->circle->center_x += center_x;
    object->circle->center_y += center_y;

    /* recalc boundings and screen coords */
    object->bounds_valid = FALSE;
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Modify the description of a circle Object.
 * \par Function Description
 *  This function modifies the description of the circle object <B>*object</B>
 *  depending on <B>whichone</B> that give the meaning of the <B>x</B> and <B>y</B>
 *  parameters.
 *
 *  If <B>whichone</B> is equal to <B>CIRCLE_CENTER</B>, the new center of the
 *  circle is given by (<B>x</B>,<B>y</B>) where <B>x</B> and <B>y</B> are in world units.
 *
 *  If <B>whichone</B> is equal to <B>CIRCLE_RADIUS</B>, the radius is given by
 *  <B>x</B> - in world units. <B>y</B> is ignored.
 *
 *  The bounding box of the circle object is updated after the modification of its
 *  parameters.
 *
 * \param [in,out] object     Circle GedaObject to modify.
 * \param [in]     x          New center x coordinate, or radius value.
 * \param [in]     y          New center y coordinate.
 *                            Unused if radius is being modified.
 * \param [in]     whichone   Which circle parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>CIRCLE_CENTER
 *    <DT>*</DT><DD>CIRCLE_RADIUS
 *  </DL>
 */
void
geda_circle_object_modify(GedaObject *object, int x, int y, int whichone)
{
  if (GEDA_IS_CIRCLE(object)) {

    switch(whichone) {
      case CIRCLE_CENTER:
        /* modify the center of the circle */
        object->circle->center_x = x;
        object->circle->center_y = y;
        break;

      case CIRCLE_RADIUS:
        /* modify the radius of the circle */
        if (x == 0) {
          geda_log(_("Null radius circles are not allowed\n"));
          return;
        }
        object->circle->radius = x;
        break;
      default:
        break;
    }

    /* recalculate the boundings */
    object->bounds_valid = FALSE;
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Create and add circle Object to list.
 * \par Function Description
 *  This function creates a new object representing a circle.
 *
 *  The circle is described by its center (<B>x</B>,<B>y</B>) and its radius
 *  <B>radius</B>.
 *  The <B>color</B> corresponds to the color the box will be drawn with.
 *
 *  The <B>GedaObject</B> structure is allocated with the #geda_circle_new()
 *  function. The structure describing the circle is allocated and initialized
 *  with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default values : solid
 *  line type with a width of 0, and no filling. It can be changed after
 *  with#geda_set_object_line_options() and#geda_set_object_fill_options().
 *
 * \param [in]     color        Circle line color
 * \param [in]     x            Center x coordinate
 * \param [in]     y            Center y coordinate
 * \param [in]     radius       Radius of new circle
 *
 * \return A pointer to the new end of the object list.
 */
GedaObject*
geda_circle_object_new(int color, int x, int y, int radius)
{
  GedaObject *new_obj;
  GedaCircle *circle;

  new_obj = geda_circle_new();
  circle  = GEDA_CIRCLE(new_obj);

  new_obj->color = color;

  /* describe the circle with its center and radius */
  circle->center_x = x;
  circle->center_y = y;
  circle->radius   = radius;

  return new_obj;
}

/*!
 * \brief Print circle to Postscript document.
 * \par Function Description
 *  This function prints the circle described by the <B>\a o_current</B>
 *  parameter to a Postscript document. It takes into account its line type
 *  and fill type.
 *  The Postscript document is descibed by the file pointer <B>fp</B>.
 *
 *  The validity of the <B>\a o_current</B> pointer is checked :
 *  a null pointer causes an error message and a return.
 *
 *  The description of the circle is extracted from the <B>\a o_current</B>
 *  parameter : the coordinates of the center of the circle, its radius,
 *  its line type, its fill type.
 *
 *  The outline and the inside of the circle are successively handled by
 *  two differend sets of functions.
 *
 * \param [in] toplevel  The GedaToplevel object.
 * \param [in] fp         FILE pointer to Postscript document.
 * \param [in] o_current  Circle GedaObject to write to document.
 * \param [in] origin_x   Page x coordinate to place circle Object.
 * \param [in] origin_y   Page y coordinate to place circle Object.
 */
void
geda_circle_object_print(GedaToplevel *toplevel, FILE *fp,
                         GedaObject   *o_current, int origin_x, int origin_y)
{
  int x, y, radius;
  int color;
  int circle_width, capstyle, length, space;

  void (*outl_func)();

  if (!GEDA_IS_CIRCLE(o_current)) {
    geda_circle_object_error(__func__, o_current);
    return;
  }

  x      = o_current->circle->center_x;
  y      = o_current->circle->center_y;
  radius = o_current->circle->radius;

  color     = o_current->color;
  capstyle  = geda_object_get_capstyle (o_current->line_options->line_end);
  outl_func = NULL;

  /*
   * Depending on the type of the line for this particular circle, the
   * appropriate function is chosen among #geda_circle_object_print_solid(),
   * #geda_circle_object_print_dotted(),#geda_circle_object_print_dashed(),
   * #geda_circle_object_print_center() and#geda_circle_object_print_phantom().
   *
   * The needed parameters for each of these type is extracted from the
   * <B>\a o_current</B> object. Depending on the type, unused parameters are
   * set to -1.
   *
   * In the eventuality of a length and/or space null, the line is
   * printed solid to avoid and endless loop produced by other functions
   * in such a case.
   */
  /* 09/08/12 | W.E.Hill Modified algorithms to incorperate both THICK & THIN
   *            styles, and eliminated hard-coded integer values.
   */

  circle_width = o_current->line_options->line_width;

  if (circle_width < MIN_LINE_WIDTH_THRESHOLD) {
     circle_width = geda_object_style_get_line_width(toplevel); /* 1st try updating style */
  }

  if (circle_width < MIN_LINE_WIDTH_THRESHOLD) {
     circle_width = MIN_LINE_WIDTH_THRESHOLD;         /* if STYLE_NONE  */
  }

  length       = o_current->line_options->line_length;
  space        = o_current->line_options->line_space;

  switch (o_current->line_options->line_type) {
    case (TYPE_SOLID):
      length = -1; space  = -1;
      outl_func = geda_circle_object_print_solid;
      break;

    case (TYPE_DOTTED):
      length = -1;
      outl_func = geda_circle_object_print_dotted;
      break;

    case (TYPE_DASHED):
      outl_func = geda_circle_object_print_dashed;
      break;

    case (TYPE_CENTER):
      outl_func = geda_circle_object_print_center;
      break;

    case (TYPE_PHANTOM):
      outl_func = geda_circle_object_print_phantom;
      break;

    case (TYPE_ERASE):
      /* Unused for now print it solid */
      length = -1; space  = -1;
      outl_func = geda_circle_object_print_solid;
      break;
  }

  if ((length == 0) || (space == 0)) {
    length = -1; space  = -1;
    outl_func = geda_circle_object_print_solid;
  }

  (*outl_func)(toplevel, fp,
               x - origin_x, y - origin_y,
               radius,
               color,
               circle_width, capstyle, length, space,
               origin_x, origin_y);

  /*
   * If the filling type of the circle is not <B>HOLLOW</B>, the appropriate
   * function is chosen among #geda_circle_object_print_filled(),
   * #geda_circle_object_print_mesh() and #geda_circle_object_print_hatch().
   * The corresponding parameters are extracted from the <B>\a o_current</B>
   * object and corrected afterward.
   *
   * The case where <B>pitch1</B> and <B>pitch2</B> are null or negative is
   * avoided as it leads to an endless loop in most of the called functions.
   * In such a case, the circle is printed filled. Unused parameters for
   * each of these functions are set to -1 or any passive value.
   */
  if (o_current->fill_options->fill_type != FILLING_HOLLOW) {

    void (*fill_func)();

    int fill_width, angle1, pitch1, angle2, pitch2;

    fill_width = o_current->fill_options->fill_width;
    angle1     = o_current->fill_options->fill_angle1;
    pitch1     = o_current->fill_options->fill_pitch1;
    angle2     = o_current->fill_options->fill_angle2;
    pitch2     = o_current->fill_options->fill_pitch2;

    switch(o_current->fill_options->fill_type) {
      case(FILL_SOLID):
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func = geda_circle_object_print_filled;
        break;

      case(FILLING_MESH):
        fill_func = geda_circle_object_print_mesh;
        break;

      case(FILLING_HATCH):
        angle2 = -1; pitch2 = 1;
        fill_func = geda_circle_object_print_hatch;
        break;

      case(FILLING_VOID):          /* Unused for now, print it filled */
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func = geda_circle_object_print_filled;
        break;

      case(FILLING_HOLLOW):
      default:
        fill_func = NULL;
        break;
    }

    if((pitch1 <= 0) || (pitch2 <= 0)) {
      angle1 = -1; pitch1 = 1;
      angle2 = -1; pitch2 = 1;
      fill_func = geda_circle_object_print_filled;
    }

    if (fill_func) {
      (*fill_func)(toplevel, fp,
                   x, y, radius,
                   color,
                   fill_width,
                   angle1, pitch1, angle2, pitch2,
                   origin_x, origin_y);
    }
  }
}

/*!
 * \brief Print a centered line type circle to Postscript document.
 * \par Function Description
 *  This function prints the outline of a circle when a centered line
 *  type is required. The circle is defined by its center in
 *  (<B>x</B>, <B>y</B>) and its radius in <B>radius</B>. It is printed with the
 *  color given in <B>color</B>.
 *
 *  It uses the function #geda_arc_object_print_center() to print the outline.
 *  Therefore it acts as an interface between the way a circle is
 *  defined and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x             Center x coordinate of circle.
 * \param [in] y             Center y coordinate of circle.
 * \param [in] radius        Circle radius.
 * \param [in] color         Circle color.
 * \param [in] circle_width  Width of circle.
 * \param [in] capstyle      Capstyle of circle lines.
 * \param [in] length        Length of dashed lines.
 * \param [in] space         Space between dashes.
 * \param [in] origin_x      Page x coordinate to place circle Object.
 * \param [in] origin_y      Page y coordinate to place circle Object.
 */
void
geda_circle_object_print_center(GedaToplevel *toplevel, FILE *fp,
                                int x,            int y,
                                int radius,       int color,
                                int circle_width, int capstyle,
                                int length,       int space,
                                int origin_x,     int origin_y)
{

  geda_arc_object_print_center(toplevel, fp,
                               x, y, radius,
                               0, FULL_CIRCLE / 64,
                               color,
                               circle_width, capstyle, length, space,
                               origin_x, origin_y);

}

/*!
 * \brief Print a dashed circle to Postscript document.
 * \par Function Description
 *  This function prints the outline of a circle when a dashed line type
 *  is required. The circle is defined by its center in
 *  (<B>x</B>, <B>y</B>) and its radius in <B>radius</B>. It is printed with the
 *  color given in <B>color</B>.
 *
 *  It uses the function #geda_arc_object_print_dashed() to print the outline.
 *  Therefore it acts as an interface between the way a circle is
 *  defined and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x             Center x coordinate of circle.
 * \param [in] y             Center y coordinate of circle.
 * \param [in] radius        Circle radius.
 * \param [in] color         Circle color.
 * \param [in] circle_width  Width of circle.
 * \param [in] capstyle      Capstyle of circle lines.
 * \param [in] length        Length of dashed lines.
 * \param [in] space         Space between dashes.
 * \param [in] origin_x      Page x coordinate to place circle Object.
 * \param [in] origin_y      Page y coordinate to place circle Object.
 */
void
geda_circle_object_print_dashed(GedaToplevel *toplevel, FILE *fp,
                                int x,            int y,
                                int radius,       int color,
                                int circle_width, int capstyle,
                                int length,       int space,
                                int origin_x,     int origin_y)
{

  geda_arc_object_print_dashed(toplevel, fp,
                               x, y, radius,
                               0, FULL_CIRCLE / 64,
                               color,
                               circle_width, capstyle, length, space,
                               origin_x, origin_y);

}

/*!
 * \brief Print a dotted circle to Postscript document.
 * \par Function Description
 *  This function prints the outline of a circle when a dotted line
 *  type is required. The circle is defined by its center
 *  in (<B>x</B>, <B>y</B>) and its radius in <B>radius</B>. It is printed
 *  with the color given in <B>color</B>.
 *  The parameter <B>length</B> is ignored.
 *
 *  It uses the function #geda_arc_object_print_dotted() to print the outline.
 *  Therefore it acts as an interface between the way a circle is
 *  defined and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x             Center x coordinate of circle.
 * \param [in] y             Center y coordinate of circle.
 * \param [in] radius        Circle radius.
 * \param [in] color         Circle color.
 * \param [in] circle_width  Width of circle.
 * \param [in] capstyle      Capstyle of circle lines.
 * \param [in] length        (unused).
 * \param [in] space         Space between dots.
 * \param [in] origin_x      Page x coordinate to place circle Object.
 * \param [in] origin_y      Page y coordinate to place circle Object.
 */
void
geda_circle_object_print_dotted(GedaToplevel *toplevel, FILE *fp,
                                int x,            int y,
                                int radius,       int color,
                                int circle_width, int capstyle,
                                int length,       int space,
                                int origin_x,     int origin_y)
{

  geda_arc_object_print_dotted(toplevel, fp,
                               x, y, radius,
                               0, FULL_CIRCLE / 64,
                               color,
                               circle_width, capstyle, -1, space,
                               origin_x, origin_y);

}

/*!
 * \brief Print a solid pattern circle to Postscript document
 * \par Function Description
 *  The function prints a filled circle with a solid pattern.
 *  No outline is printed.
 *  The circle is defined by the coordinates of its center in
 *  (<B>x</B>,<B>y</B>) and its radius given by the <B>radius</B> parameter.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B> and <B>pitch1</B>, <B>angle2</B>
 *  and <B>pitch2</B> parameters are ignored in this functions but
 *  kept for compatibility with other fill functions.
 *
 *  All dimensions are in mils (except <B>angle1</B> and <B>angle2</B> in degree).
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Center x coordinate of circle.
 * \param [in] y           Center y coordinate of circle.
 * \param [in] radius      Radius of circle.
 * \param [in] color       Circle color.
 * \param [in] fill_width  Circle fill width. (unused).
 * \param [in] angle1      (unused).
 * \param [in] pitch1      (unused).
 * \param [in] angle2      (unused).
 * \param [in] pitch2      (unused).
 * \param [in] origin_x    Page x coordinate to place circle Object.
 * \param [in] origin_y    Page y coordinate to place circle Object.
 */
void
geda_circle_object_print_filled(GedaToplevel *toplevel, FILE *fp,
                                int x, int y, int radius,
                                int color,
                                int fill_width,
                                int angle1, int pitch1,
                                int angle2, int pitch2,
                                int origin_x, int origin_y)
{
  f_print_set_color(toplevel, fp, color);

  fprintf(fp, "%d %d %d dot\n",
          x-origin_x, y-origin_y,
          radius);
}

/*!
 * \brief Print a hatch pattern circle to Postscript document.
 * \par Function Description
 *  The function prints a hatched circle. No outline is printed.
 *  The circle is defined by the coordinates of its center in
 *  (<B>x</B>,<B>y</B>) and its radius by the <B>radius</B> parameter.
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *  <B>angle2</B> and <B>pitch2</B> parameters are ignored in this
 *  functions but kept for compatibility with other fill functions.
 *
 *  The only attribute of line here is its width from the parameter <B>width</B>.
 *
 *  Negative or null values for <B>pitch1</B> is not allowed as it
 *  leads to an endless loop.
 *
 *  All dimensions are in mils (except <B>angle1</B> is in degrees).
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Center x coordinate of circle.
 * \param [in] y           Center y coordinate of circle.
 * \param [in] radius      Radius of circle.
 * \param [in] color       Circle color.
 * \param [in] fill_width  Circle fill width.
 * \param [in] angle1      Angle for hatch pattern.
 * \param [in] pitch1      Pitch for hatch pattern.
 * \param [in] angle2      (unused).
 * \param [in] pitch2      (unused).
 * \param [in] origin_x    Page x coordinate to place circle Object.
 * \param [in] origin_y    Page y coordinate to place circle Object.
 */
void
geda_circle_object_print_hatch(GedaToplevel *toplevel, FILE *fp,
                               int x, int y, int radius,
                               int color,
                               int fill_width,
                               int angle1, int pitch1,
                               int angle2, int pitch2,
                               int origin_x, int origin_y)
{
  GArray    *lines;
  GedaCircle circle;
  int        index;

  g_return_if_fail(toplevel != NULL);
  g_return_if_fail(fp != NULL);

  f_print_set_color(toplevel, fp, color);

  /* Avoid printing line widths too small */
  if (fill_width <= 1) fill_width = 2;

  lines = g_array_new(FALSE, FALSE, sizeof(LINE));

  circle.center_x = x;
  circle.center_y = y;
  circle.radius   = radius;

  geda_math_hatch_circle(&circle, angle1, pitch1, lines);

  for(index=0; index<lines->len; index++) {
    LINE *line = &g_array_index(lines, LINE, index);

    fprintf(fp,"%d %d %d %d %d %d line\n",
            line->x[0], line->y[0],
            line->x[1], line->y[1],
            fill_width, BUTT_CAP);
  }

  g_array_free(lines, TRUE);
}

/*!
 * \brief Print a mesh pattern circle to Postscript document
 * \par Function Description
 *  This function prints a meshed circle. No outline is printed.
 *  The circle is defined by the coordinates of its center in
 *  (<B>x</B>,<B>y</B>) and its radius by the <B>radius</B> parameter.
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *
 *  The inside mesh is achieved by two successive call to the
 *  #geda_circle_object_print_hatch() function, given <B>angle1</B> and <B>pitch1</B>
 *  the first time and <B>angle2</B> and <B>pitch2</B> the second time.
 *
 *  Negative or null values for <B>pitch1</B> and/or <B>pitch2</B> are
 *  not allowed as it leads to an endless loop in #geda_circle_object_print_hatch().
 *
 *  All dimensions are in mils (except <B>angle1</B> and <B>angle2</B> in degree).
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Center x coordinate of circle.
 * \param [in] y           Center y coordinate of circle.
 * \param [in] radius      Radius of circle.
 * \param [in] color       Circle color.
 * \param [in] fill_width  Circle fill width.
 * \param [in] angle1      1st angle for mesh pattern.
 * \param [in] pitch1      1st pitch for mesh pattern.
 * \param [in] angle2      2nd angle for mesh pattern.
 * \param [in] pitch2      2nd pitch for mesh pattern.
 * \param [in] origin_x    Page x coordinate to place circle Object.
 * \param [in] origin_y    Page y coordinate to place circle Object.
 */
void
geda_circle_object_print_mesh(GedaToplevel *toplevel, FILE *fp,
                              int x, int y, int radius,
                              int color,
                              int fill_width,
                              int angle1, int pitch1,
                              int angle2, int pitch2,
                              int origin_x, int origin_y)
{
  geda_circle_object_print_hatch(toplevel, fp,
                       x, y, radius,
                       color,
                       fill_width,
                       angle1, pitch1,
                       -1, -1,
                       origin_x, origin_y);
  geda_circle_object_print_hatch(toplevel, fp,
                       x, y, radius,
                       color,
                       fill_width,
                       angle2, pitch2,
                       -1, -1,
                       origin_x, origin_y);

}

/*!
 * \brief Print a phantom line type circle to Postscript document.
 * \par Function Description
 *  This function prints the outline of a circle when a phantom line type
 *  is required. The circle is defined by its center in
 *  (<B>x</B>, <B>y</B>) and its radius in <B>radius</B>. It is printed with the
 *  color given in <B>color</B>.
 *
 *  It uses the function #geda_arc_object_print_phantom() to print the outline.
 *  Therefore it acts as an interface between the way a circle is defined
 *  and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x             Center x coordinate of circle.
 * \param [in] y             Center y coordinate of circle.
 * \param [in] radius        Circle radius.
 * \param [in] color         Circle color.
 * \param [in] circle_width  Width of circle.
 * \param [in] capstyle      Capstyle of circle lines.
 * \param [in] length        Length of dashed lines.
 * \param [in] space         Space between dashes.
 * \param [in] origin_x      Page x coordinate to place circle Object.
 * \param [in] origin_y      Page y coordinate to place circle Object.
 */
void
geda_circle_object_print_phantom(GedaToplevel *toplevel, FILE *fp,
                                 int x,            int y,
                                 int radius,       int color,
                                 int circle_width, int capstyle,
                                 int length,       int space,
                                 int origin_x,     int origin_y)
{

  geda_arc_object_print_phantom(toplevel, fp,
                                x, y, radius,
                                0, FULL_CIRCLE / 64,
                                color,
                                circle_width, capstyle, length, space,
                                origin_x, origin_y);

}

/*!
 * \brief Print a solid circle to Postscript document.
 * \par Function Description
 *  This function prints the outline of a circle when a solid line type
 *  is required. The circle is defined by its center in (<B>x</B>, <B>y</B>)
 *  and its radius in <B>radius</B>. It is printed with the color given
 *  in <B>color</B>.
 *  The parameters <B>length</B> and <B>space</B> are ignored.
 *
 *  It uses the function #geda_arc_object_print_solid() to print the outline.
 *  Therefore it acts as an interface between the way a circle is defined
 *  and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel     The GedaToplevel object.
 * \param [in] fp            FILE pointer to Postscript document.
 * \param [in] x             Center x coordinate of circle.
 * \param [in] y             Center y coordinate of circle.
 * \param [in] radius        Circle radius.
 * \param [in] color         Circle color.
 * \param [in] circle_width  Width of circle.
 * \param [in] capstyle      Capstyle of circle lines.
 * \param [in] length        (unused).
 * \param [in] space         (unused).
 * \param [in] origin_x      Page x coordinate to place circle Object.
 * \param [in] origin_y      Page y coordinate to place circle Object.
 */
void
geda_circle_object_print_solid(GedaToplevel *toplevel, FILE *fp,
                               int x,            int y,
                               int radius,       int color,
                               int circle_width, int capstyle,
                               int length,       int space,
                               int origin_x,     int origin_y)
{
  geda_arc_object_print_solid(toplevel, fp,
                              x, y, radius,
                              0, FULL_CIRCLE / 64,
                              color,
                              circle_width, BUTT_CAP, -1, -1,
                              origin_x, origin_y);

}

/*!
 * \brief Create circle Object from character string.
 * \par Function Description
 *  The #geda_circle_object_read() function gets from the character string
 *  <B>*buff</B> the description of a circle.
 *
 *  Depending on <B>*version</B>, the right file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 2000704 release.
 *    <DT>*</DT><DD>the file format used for the releases after 20000704.
 *  </DL>
 *
 * \param [in]  buf             Character string with circle description.
 * \param [in]  release_ver     libgeda release version number.
 * \param [in]  fileformat_ver  libgeda file format version number.
 *
 * \param [out] err             A GError object
 *
 * \return A pointer to the new circle object, or NULL on error.
 */
GedaObject*
geda_circle_object_read (const char buf[], unsigned int release_ver,
                                           unsigned int fileformat_ver,
                                           GError **err)
{
  GedaObject *new_obj;
  char type;
  int  x1, y1;
  int  radius;
  int  color;
  int  circle_width, circle_space, circle_length;
  int  fill_width, angle1, pitch1, angle2, pitch2;
  int  circle_end;
  int  circle_type;
  int  circle_fill;

  if (release_ver <= VERSION_20000704) {
    /*
     * The old geda file format, i.e. releases 20000704 and older, does not
     * handle the line type and the filling of the box object. They are set
     * to default.
     */
    if (sscanf(buf, "%c %d %d %d %d\n", &type, &x1, &y1, &radius, &color) != 5) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse circle object"));
      return NULL;
    }

    circle_width  = 0;
    circle_end    = END_NONE;
    circle_type   = TYPE_SOLID;
    circle_length = -1;
    circle_space  = -1;

    circle_fill   = FILLING_HOLLOW;
    fill_width    = 0;
    angle1        = -1;
    pitch1        = -1;
    angle2        = -1;
    pitch2        = -1;

  }
  else {

    /*
     * The current line format to describe a circle is a space separated
     * list of characters and numbers in plain ASCII on a single line. The
     * meaning of each item is described in the file format documentation.
     */
    if (sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
       &type, &x1, &y1, &radius, &color,
       &circle_width, &circle_end, &circle_type,
       &circle_length, &circle_space, &circle_fill,
       &fill_width, &angle1, &pitch1, &angle2, &pitch2) != 16) {
       g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse circle object"));
    return NULL;
    }
  }

  /* Error check */
  if (radius < 0) {
    const char *msg = _("Found circle with a negative radius");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, radius);
    }
    radius = abs(radius);
    geda_log_w ("%s %d\n", _("Setting radius to"), radius);
  }
  else if (radius == 0) {
    const char *msg = _("Found a circle with radius zero");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s\n", msg);
    }
    geda_log_w (_("Setting zero radius to 1000\n"));
    radius = 1000;
  }

  if (color < 0 || color > MAX_COLORS) {
    const char *msg = _("Found an invalid color");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, color);
    }
    geda_log_w (_("Setting color to default color\n"));
    color = DEFAULT_CIRCLE_COLOR_INDEX;
  }

  /*
   * A circle is internally described by the center and the radius.
   *
   * A new object is allocated, initialized and added to the object list.
   * Its filling and line type are set according to the values of the field
   * on the line.
   */
  new_obj = geda_circle_object_new(color, x1, y1, radius);

  /* set the line options */
  new_obj->line_options->line_end     = circle_end;
  new_obj->line_options->line_type    = circle_type;
  new_obj->line_options->line_width   = circle_width;
  new_obj->line_options->line_length  = circle_length;
  new_obj->line_options->line_space   = circle_space;

  /* set the fill options */
  new_obj->fill_options->fill_type   = circle_fill;
  new_obj->fill_options->fill_width  = fill_width;
  new_obj->fill_options->fill_angle1 = angle1;
  new_obj->fill_options->fill_angle2 = angle2;
  new_obj->fill_options->fill_pitch1 = pitch1;
  new_obj->fill_options->fill_pitch2 = pitch2;

  return new_obj;
}

/*!
 * \brief Rotate Circle GedaObject
 * \par Function Description
 *  This function rotate the circle described by <B>*object</B> around
 *  the (<B>center_x</B>,<B>center_y</B>) point by angle <B>angle</B>
 *  degrees.
 *
 * \param [in,out]  object    Circle GedaObject to rotate.
 * \param [in]      center_x  Rotation center x coordinate.
 * \param [in]      center_y  Rotation center y coordinate.
 * \param [in]      angle     Rotation angle in degrees (See note below).
 */
void
geda_circle_object_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  if (GEDA_IS_CIRCLE(object)) {

    int newx, newy;
    int x, y;

    /* Only 90 degree multiple and positive angles are allowed. */
    /* angle must be positive */
    if(angle < 0) {
      angle = -angle;
    }

    /* angle must be a 90 multiple or no rotation performed */
    if((angle % 90) != 0) {
      return;
    }

    /*
     * The center of rotation (<B>center_x</B>,<B>center_y</B>) is
     * translated to the origin. The rotation of the center around the origin
     * is then performed. Finally, the rotated circle is translated back to
     * its previous location.
     */

    /* translate object to origin */
    object->circle->center_x -= center_x;
    object->circle->center_y -= center_y;

    /* rotate the center of the circle around the origin */
    x = object->circle->center_x;
    y = object->circle->center_y;
    geda_math_rotate_point_90(x, y, angle, &newx, &newy);
    object->circle->center_x = newx;
    object->circle->center_y = newy;

    /* translate back in position */
    object->circle->center_x += center_x;
    object->circle->center_y += center_y;

    object->bounds_valid = FALSE;
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*! O0232
 * \brief Scale an Circle GedaObject
 * \par Function Description
 *  Increase or decreases the radius of the circle object by the
 *  given \a scale factor.
 *
 * \param [in] object  Pointer to a Circle GedaObject
 * \param [in] scale   Scale factor.
 */
void geda_circle_object_scale (GedaObject *object, int scale)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (scale) {
      object->circle->radius = object->circle->radius * scale;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*! O033
 * \brief Set the center X coordinate of a GedaCircle object
 * \par Function Description
 *  Set the arc center X property to the given value.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] x       New value for the circle X coordinate
 *
 * \sa geda_circle_object_get_center_x
 */
void
geda_circle_object_set_center_x (GedaObject *object, int x)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->circle->center_x != x) {
      object->circle->center_x = x;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*! O060?
 * \brief Set the center Y coordinate of a GedaCircle object
 * \par Function Description
 *  Set the arc center Y property to the given value.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] y       New value for the circle Y coordinate
 *
 * \sa geda_circle_object_get_center_y
 */
void
geda_circle_object_set_center_y (GedaObject *object, int y)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->circle->center_y != y) {
      object->circle->center_y = y;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the End Cap type Property of a GedaObject
 * \par Function Description
 *  Sets the value of \a object end-cap type if and only if \a object is
 *  a valid GedaCircle object. The line-end properties is only applicable
 *  for fill types FILLING_MESH and FILLING_HATCH.
 *
 * \sa geda_circle_object_get_end_cap
 */
void
geda_circle_object_set_end_cap (GedaObject *object, int line_end)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->line_options->line_end != line_end) {
      object->line_options->line_end = line_end < END_NONE ? END_NONE :
                                       line_end > END_VOID ? END_VOID :
                                       line_end;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Angle 1 Property of a circle
 * \par Function Description
 *  Sets the value of \a object fill angle 1 if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] angle   new value for the fill-angle1 property
 *
 * \sa geda_circle_object_get_fill_angle1
 */
void
geda_circle_object_set_fill_angle1 (GedaObject *object, int angle)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->fill_options->fill_angle1 != angle) {
      object->fill_options->fill_angle1 = angle;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Angle 2 Property of a circle
 * \par Function Description
 *  Sets the value of \a object fill angle 2 if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] angle   new value for the fill-angle2 property
 *
 * \sa geda_circle_object_get_fill_angle2
 */
void
geda_circle_object_set_fill_angle2 (GedaObject *object, int angle)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->fill_options->fill_angle2 != angle) {
      object->fill_options->fill_angle2 = angle;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Pitch 1 Property of a circle
 * \par Function Description
 *  Sets the value of \a object fill pitch 1 if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] pitch   new value for the fill-pitch1 property
 *
 * \sa geda_circle_object_get_fill_pitch1
 */
void
geda_circle_object_set_fill_pitch1 (GedaObject *object, int pitch)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->fill_options->fill_pitch1 != pitch) {
      object->fill_options->fill_pitch1 = pitch < 0 ? 0 : pitch;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Pitch 2 Property of a circle
 * \par Function Description
 *  Sets the value of \a object fill pitch 2 if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] pitch   new value for the fill-pitch2 property
 *
 * \sa geda_circle_object_get_fill_pitch2
 */
void
geda_circle_object_set_fill_pitch2 (GedaObject *object, int pitch)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->fill_options->fill_pitch2 != pitch) {
      object->fill_options->fill_pitch2 = pitch < 0 ? 0 : pitch;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Type Property of a object
 * \par Function Description
 *  Sets the value of \a object fill type if and only if \a object is
 *  a valid GedaCircle object.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] type    new #OBJECT_FILLING for the fill-type property
 *
 * \sa geda_circle_object_get_fill_type
 */
void
geda_circle_object_set_fill_type (GedaObject *object, int type)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->fill_options->fill_type != type) {
      object->fill_options->fill_type = type < TYPE_SOLID ? TYPE_SOLID :
                                        type > TYPE_ERASE ? TYPE_ERASE :
                                        type;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Width Property of a circle
 * \par Function Description
 *  Sets the value of \a circle width of the fill if and only
 *  if \a circle is a valid GedaCircle object.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] width   new value for the fill-width property
 *
 * \sa geda_circle_object_get_fill_width
 */
void
geda_circle_object_set_fill_width (GedaObject *object, int width)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->fill_options->fill_width != width) {
      object->fill_options->fill_width = width < 0 ? 0 : width;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Line Length Property of an GedaCircle
 * \par Function Description
 *  Returns the value of the \a object line length property if and only if
 *  \a object is a valid GedaCircle object. The line-length property controls
 *  the length of line segments for line types dashed, center and phantom.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] length  new value for the line-length property
 *
 * \sa geda_circle_object_get_line_length
 */
void
geda_circle_object_set_line_length (GedaObject *object, int length)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->line_options->line_length != length) {
      object->line_options->line_length = length > 0 ? length : 0;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Line Space Property of a GedaCircle
 * \par Function Description
 *  Sets the value of the \a object line space property if and only if \a object
 *  is a valid GedaCircle object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] space   new LINE_TYPE value for the line type
 *
 * \sa geda_circle_object_get_line_space
 */
void
geda_circle_object_set_line_space (GedaObject *object, int space)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->line_options->line_space != space) {
      object->line_options->line_space = space > 0 ? space : 0;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Line Type Property of a GedaCircle
 * \par Function Description
 *  Sets the value of \a object line type if and only if \a object is a
 *  valid GedaCircle object.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] type    new LINE_TYPE value for the line type
 *
 * \sa geda_circle_object_get_line_type
 */
void
geda_circle_object_set_line_type (GedaObject *object, int type)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->line_options->line_type != type) {
      object->line_options->line_type = type < TYPE_SOLID ? TYPE_SOLID :
                                        type > TYPE_ERASE ? TYPE_ERASE :
                                        type;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Set the End Width Property of a GedaCircle
 * \par Function Description
 *  Sets the value of the \a object line width property if and only if
 *  \a object is a valid GedaCircle object.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] width   new value for the line width
 *
 * \sa geda_circle_object_get_line_width
 */
void
geda_circle_object_set_line_width (GedaObject *object, int width)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->line_options->line_width != width) {
      object->line_options->line_width = width > 0 ? width : 0;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*! O060?
 * \brief Set the Radius coordinate of a GedaCircle object
 * \par Function Description
 *  Set the arc radius property to the given value.
 *
 * \param [in] object  Pointer to a GedaCircle Object
 * \param [in] radius  new radius value
 *
 * \sa geda_circle_object_get_radius
 */
void
geda_circle_object_set_radius (GedaObject *object, int radius)
{
  if (GEDA_IS_CIRCLE(object)) {
    if (object->circle->radius != radius) {
      object->circle->radius = radius;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/*!
 * \brief Calculates shortest distance to a Circle
 * \par Function Description
 *   Calculates the distance between the given point and the closest
 *   point on the perimeter of the circle.
 *
 * \param [in] object       A circle Object.
 * \param [in] x            The x coordinate of the given point.
 * \param [in] y            The y coordinate of the given point.
 * \param [in] force_solid  If true, force treating the object as solid.
 *
 * \return The shortest distance from point to \a object to the point or
 *         G_MAXDOUBLE if the parameters are invalid parameter.
 */
double
geda_circle_object_shortest_distance (ConstObject *object, int x, int y, int force_solid)
{
  if (GEDA_IS_CIRCLE(object)) {

    int solid;

    solid = force_solid || object->fill_options->fill_type != FILLING_HOLLOW;

    return geda_math_circle_shortest_distance (object->circle, x, y, solid);
  }

  geda_circle_object_error(__func__, object);

  return (G_MAXDOUBLE);
}

/*!
 * \brief Create a character string representation of a circle Object
 * \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe the
 *  circle <B>*object</B> following the post-20000704 release file format that
 *  handles the line type and fill options.
 *
 * \note object was validated by geda_object_save_objects
 *
 * \param [in] object  Circle GedaObject to create string from.
 *
 * \return A pointer to the circle Object character string.
 *
 * \remarks The string should be freed at some point.
 *
 */
char*
geda_circle_object_to_buffer(GedaObject *object)
{
  char *buf;
  int   x,y;
  int   radius;
  int   circle_width, circle_space, circle_length;
  int   fill_width, angle1, pitch1, angle2, pitch2;

  LINE_END       circle_end;
  LINE_TYPE      circle_type;
  OBJECT_FILLING circle_fill;

  /* circle center and radius */
  x = object->circle->center_x;
  y = object->circle->center_y;
  radius = object->circle->radius;

  /* line type parameters */
  circle_width = object->circle->line_options.line_width;
  circle_end   = object->circle->line_options.line_end;
  circle_type  = object->circle->line_options.line_type;
  circle_length= object->circle->line_options.line_length;
  circle_space = object->circle->line_options.line_space;

  /* filling parameters */
  circle_fill  = object->circle->fill_options.fill_type;
  fill_width   = object->circle->fill_options.fill_width;
  angle1       = object->circle->fill_options.fill_angle1;
  pitch1       = object->circle->fill_options.fill_pitch1;
  angle2       = object->circle->fill_options.fill_angle2;
  pitch2       = object->circle->fill_options.fill_pitch2;

  buf = geda_sprintf("%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                     object->type, x, y, radius, object->color,
                     circle_width, circle_end, circle_type, circle_length,
                     circle_space, circle_fill,
                     fill_width, angle1, pitch1, angle2, pitch2);
  return(buf);
}

/*!
 * \brief Translate a circle position by a delta
 * \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the circle
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 * \param [in]     dx         x distance to move.
 * \param [in]     dy         y distance to move.
 * \param [in,out] object     Circle GedaObject to translate.
 */
void
geda_circle_object_translate(GedaObject *object, int dx, int dy)
{
  if (GEDA_IS_CIRCLE(object)) {
    /* Do world coords */
    object->circle->center_x = object->circle->center_x + dx;
    object->circle->center_y = object->circle->center_y + dy;

    /* recalc the screen coords and the bounding box */
    object->bounds_valid = FALSE;
  }
  else {
    geda_circle_object_error(__func__, object);
  }
}

/** @} endgroup geda-circle-object-proc */
