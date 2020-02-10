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

/*! \file o_box_object.c
 *  \brief functions for the box object
 */

/** \defgroup geda-box-object-proc GedaBox Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaBox Objects
 */

#include "../../../config.h"

#include <math.h>
#include <stdio.h>

#include <libgeda_priv.h>

static void geda_object_error(const char *func, const void *object, IDE_OBJECT_TYPE type)
{
  geda_error_object_argument(__FILE__, func, object, type);
}

static void geda_box_object_error(const char *func, const void *object)
{
  geda_object_error(func, object, GEDA_OBJECT_BOX);
}

/*! O0401
 * \brief Copy a box to a list.
 * \par Function Description
 *  The function #geda_box_object_copy() creates a verbatim copy of the object
 *  pointed by <B>\a o_current</B> describing a box.
 *
 * \param [in] o_source Box Object to copy.
 *
 * \return The new GedaObject
 */
GedaObject *geda_box_object_copy(const GedaObject *o_source)
{
  if (GEDA_IS_BOX(o_source)) {

    GedaObject *new_obj;
    GedaBox    *old_box;

    old_box = GEDA_BOX(o_source);

    /* A new box object is created with #geda_box_object_new().
     * Values for its fields are default and need to be modified. */
    new_obj = geda_box_object_new (o_source->color, 0, 0, 0, 0);

    /* The dimensions of the new box are set with the ones of the original box.
     * The two boxes have the same line type and the same filling options.
     */
    new_obj->box->upper_x = old_box->upper_x;
    new_obj->box->upper_y = old_box->upper_y;
    new_obj->box->lower_x = old_box->lower_x;
    new_obj->box->lower_y = old_box->lower_y;

    geda_set_object_line_options(new_obj, &old_box->line_options);
    geda_set_object_fill_options(new_obj, &old_box->fill_options);

    new_obj->bounds_valid = FALSE;

    return new_obj;
  }
  geda_box_object_error(__func__, o_source);
  return NULL;
}

/*!
 * \brief Retrieve End Cap Property of a Box Object
 * \par Function Description
 *  Returns the value of \a box end-cap type if and only if \a box
 *  is a valid GedaBox object.
 *
 * \return integer value of line_end type or -0 if \a box is invalid.
 *
 * \sa geda_box_get_end_cap geda_box_object_set_end_cap
 */
int geda_box_object_get_end_cap (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->line_options->line_end;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Angle 1 Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object fill-angle1 if and only if \a object
 *  is a valid GedaBox object.
 *
 * \return integer value of fill_angle1 or -0 if \a object is invalid.
 *
 * \sa geda_box_get_fill_angle1 geda_box_object_set_fill_angle1
 */
int geda_box_object_get_fill_angle1 (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->fill_options->fill_angle1;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Angle 2 Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object fill-angle2 if and only if \a object
 *  is a valid GedaBox object.
 *
 * \return integer value of fill_angle2 or -0 if \a object is invalid.
 *
 * \sa geda_box_get_fill_angle2 geda_box_object_set_fill_angle2
 */
int geda_box_object_get_fill_angle2 (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->fill_options->fill_angle2;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Pitch 1 Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object fill-pitch1 if and only if \a object
 *  is a valid GedaBox object.
 *
 * \return integer value of fill_pitch1 or -0 if \a object is invalid.
 *
 * \sa geda_box_get_fill_pitch1 geda_box_object_set_fill_pitch1
 */
int geda_box_object_get_fill_pitch1 (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->fill_options->fill_pitch1;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Pitch 2 Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object fill-pitch2 if and only if \a object
 *  is a valid GedaBox object.
 *
 * \return integer value of fill_pitch2 or -0 if \a object is invalid.
 *
 * \sa geda_box_get_fill_pitch2 geda_box_object_set_fill_pitch2
 */
int geda_box_object_get_fill_pitch2 (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->fill_options->fill_pitch2;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Type Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object fill-type if and only if \a object
 *  is a valid GedaBox object.
 *
 * \return integer value of fill_type or -0 if \a object is invalid.
 *
 * \sa geda_box_get_fill_type
 */
int geda_box_object_get_fill_type (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->fill_options->fill_type;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Fill Width Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object fill-width if and only if \a object
 *  is a valid GedaBox object.
 *
 * \return integer value of fill_width or -0 if \a object is invalid.
 *
 * \sa geda_box_get_fill_width
 */
int geda_box_object_get_fill_width (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->fill_options->fill_width;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Line Length Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object line length if \a object is a
 *  valid GedaBox object. The line-length property controls the
 *  length of line segments for line types dashed, center and
 *  phantom.
 *
 * \note Line length is only applicable when line-type is not
 *       TYPE_SOLID or TYPE_DOTTED.
 *
 * \return integer value of line_length or -0 if \a object is invalid.
 *
 * \sa geda_box_get_line_length
 */
int geda_box_object_get_line_length (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->line_options->line_length;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Line Space Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object line space if \a object is a
 *  valid GedaBox object. The line-space property controls the
 *  distance between line-length for line types dashed, center,
 *  phantom and between dots for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \return integer value of line_length or -0 if \a object is invalid.
 *
 * \sa geda_box_get_line_space
 */
int geda_box_object_get_line_space (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->line_options->line_space;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Line Type Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object line type if \a object is a
 *  valid GedaBox object.
 *
 * \return integer value of line_type or -0 if \a object is invalid.
 *
 * \sa geda_box_get_line_type
 */
int geda_box_object_get_line_type (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->line_options->line_type;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*!
 * \brief Retrieve Line Width Property of a Box Object
 * \par Function Description
 *  Returns the value of \a object line width if \a object is a
 *  valid GedaBox object.
 *
 * \return integer value of line_width or -0 if \a object is invalid.
 *
 * \sa geda_box_get_line_width
 */
int geda_box_object_get_line_width (const GedaObject *object)
{
  if (GEDA_IS_BOX(object)) {
    return object->line_options->line_width;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*! O0413
 * \brief Retrieve lower X coordinate of the a Box object
 * \par Function Description
 *  Returns the lower X value of the box \a object or zero if
 *  \a object is not a valid GedaBox object.
 *
 * \return integer value of lower X or 0 if \a box is invalid.
 *
 * \sa geda_box_set_lower_x
 */
int geda_box_object_get_lower_x (const GedaObject *object) {
  if (GEDA_IS_BOX(object)) {
    return object->box->lower_x;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*! O0414
 * \brief Retrieve lower Y coordinate of the a Box object
 * \par Function Description
 *  Returns the lower y value of the box \a object or zero if
 *  \a object is not a valid GedaBox object.
 *
 * \return integer value of lower X or 0 if \a object is invalid.
 *
 * \sa geda_box_set_lower_x
 */
int geda_box_object_get_lower_y (const GedaObject *object) {
  if (GEDA_IS_BOX(object)) {
    return object->box->lower_y;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*! O0415
 * \brief Get Point on a Box Nearest a Given Point
 * \par Function Description
 *  This function is intended to locate a point on a Box object given
 *  a point \a x, \a y, that is on or about the vicinity of \a object. If
 *  True is returned, <B>nx</B> and <B>ny</B> are set in world unit to a point
 *  on the box that is the closest point on the box to the point given by \a x, \a y.
 *
 * \param [in]  object  Pointer to a Box object
 * \param [in]  x       Integer x of point near or on the box
 * \param [in]  y       Integer y of point near or on the box
 * \param [out] nx      Integer pointer to resulting x value
 * \param [out] ny      Integer pointer to resulting y value
 *
 * \returns TRUE is the results are valid, FALSE if \a object was not a GedaBox.
 */
bool geda_box_object_get_nearest_point (GedaObject *object, int x, int y, int *nx, int *ny)
{
  GedaBox *box;
  bool result;

  if (GEDA_IS_BOX(object)) {

    GedaLine *closest;

    box    = object->box;
    result = FALSE;

    int  left   = /* min */ box->upper_x < box->lower_x ? box->upper_x : box->lower_x;
    int  bottom = /* min */ box->upper_y < box->lower_y ? box->upper_y : box->lower_y;
    int  right  = /* max */ box->upper_x > box->lower_x ? box->upper_x : box->lower_x;
    int  top    = /* max */ box->upper_y > box->lower_y ? box->upper_y : box->lower_y;

    if (left >= x) {

      *nx = left;

      if (y >= top) {
        *ny = top;
      }
      else if (bottom >= y) {
        *ny = bottom;
      }
      else {
        *ny = y;
      }
      result = TRUE;
    }
    else if (x >= right) {

      *nx = right;

      if (y >= top) {
        *ny = top;
      }
      else if (bottom >= y) {
        *ny = bottom;
      }
      else {
        *ny = y;
      }
      result = TRUE;
    }
    else if (y >= top) {
      *ny = top;
      *nx = x;
       result = TRUE;
    }
    else if (bottom >= y) {
      *ny = bottom;
      *nx = x;
       result = TRUE;
    }
    else { /* point is inside the box */

      GedaLine  segments[4];
      double dl, dr, dt, db;

      /* Left Side */
      segments[0].x[0] = left;
      segments[0].y[0] = bottom;
      segments[0].x[1] = left;
      segments[0].y[1] = top;

      dl = geda_math_line_shortest_distance (&segments[0], x, y);

      /* Right Side */
      segments[1].x[0] = right;
      segments[1].y[0] = bottom;
      segments[1].x[1] = right;
      segments[1].y[1] = top;

      dr = geda_math_line_shortest_distance (&segments[1], x, y);

      /* Top Side */
      segments[2].x[0] = left;
      segments[2].y[0] = top;
      segments[2].x[1] = right;
      segments[2].y[1] = top;

      dt = geda_math_line_shortest_distance (&segments[2], x, y);

      /* Bottom Side */
      segments[3].x[0] = left;
      segments[3].y[0] = bottom;
      segments[3].x[1] = right;
      segments[3].y[1] = bottom;

      db = geda_math_line_shortest_distance (&segments[3], x, y);

      /* Check for diagonals, if the point is on a diagonal then the
       * point is equidistant to two sides, the return point is set
       * to the corner but the result is not set TRUE */
      if (db == dl) {                       /* bottom left */
        *ny = bottom;
        *nx = left;
      }
      else if (dt == dl) {                  /* top left */
        *ny = top;
        *nx = left;
      }
      else if (db == dr) {                  /* bottom right */
        *ny = bottom;
        *nx = right;
      }
      else if (dt == dr) {                  /* top right */
        *ny = top;
        *nx = right;
      }
      else {

        /* Inside not on a diagonal */

        if (dl < db && dl < dt) {           /* left */
          closest = &segments[0];
        }
        else if (dr < db && dr < dt) {      /* right */
          closest = &segments[1];
        }
        else if (db > dt) {                 /* top */
          closest = &segments[2];
        }
        else {                              /* bottom */
          closest = &segments[3];
        }

        double dx, dy, ix, iy;
        double m1, m2, b1, b2;

        dx = closest->x[1] - closest->x[0];
        dy = closest->y[1] - closest->y[0];

        m1 = dy / dx;
        b1 = closest->y[0] - m1 * closest->x[0];
        m2 = -1 / m1;
        b2 = y - m2 * x;

        ix = (b2 - b1) / (m1 - m2);
        iy = m2 * ix + b2;

#ifdef HAVE_LRINT

        *nx = lrint(ix);
        *nx = lrint(iy);

#else

        *nx = ix + 0.5;
        *nx = iy + 0.5;

#endif
        result = TRUE;
      }
    }
  }
  else { /* was not an Box */
    geda_box_object_error(__func__, object);
    result = FALSE;
  }

  if (!result) {
    *nx = x;
    *ny = y;
  }
  return result;
}

/*! O0416
 * \brief get the position of the left bottom point
 * \par Function Description
 *  This function gets the position of the bottom left point of a box object.
 *
 * \param [in]  object  GedaBox object whose position is to be returned
 * \param [out] x       pointer to the x-position
 * \param [out] y       pointer to the y-position
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_box_object_get_position (GedaObject *object, int *x, int *y)
{
  if (GEDA_IS_BOX(object)) {
    *x = min(object->box->lower_x, object->box->upper_x);
    *y = min(object->box->lower_y, object->box->upper_y);
    return TRUE;
  }

  geda_box_object_error(__func__, object);
  return 0;
}

/*! O0417
 * \brief Retrieve Upper X coordinate of the a Box object
 * \par Function Description
 *  Returns the upper X value of the box \a object or zero if
 *  \a object is not a valid GedaBox object.
 *
 * \return integer value of upper X or 0 if \a box is invalid.
 *
 * \sa geda_box_set_lower_x
 */
int geda_box_object_get_upper_x (const GedaObject *object) {
  if (GEDA_IS_BOX(object)) {
    return object->box->upper_x;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*! O0418
 * \brief Retrieve Upper Y coordinate of the a Box object
 * \par Function Description
 *  Returns the upper y value of the box \a object or zero if
 *  \a object is not a valid GedaBox object.
 *
 * \return integer value of upper X or 0 if \a object is invalid.
 *
 * \sa geda_box_set_upper_x
 */
int geda_box_object_get_upper_y (const GedaObject *object) {
  if (GEDA_IS_BOX(object)) {
    return object->box->upper_y;
  }
  geda_box_object_error(__func__, object);
  return -0;
}

/*! O0419
 * \brief Mirror a Box.
 * \par Function Description
 *  This function mirrors the box from the point
 *  (<B>center_x</B>,<B>center_y</B>).
 *
 *  The box is first translated to the origin, then mirrored and finally
 *  translated back at its previous position.
 *
 * \param [in,out] object    GedaBox Object to mirror
 * \param [in]     center_x  Origin x coordinate
 * \param [in]     center_y  Origin y coordinate
 */
void geda_box_object_mirror(GedaObject *object, int center_x, int center_y)
{
  if (GEDA_IS_BOX(object)) {

    int newx1, newy1;
    int newx2, newy2;

    /* translate object to origin */
    object->box->upper_x -= center_x;
    object->box->upper_y -= center_y;
    object->box->lower_x -= center_x;
    object->box->lower_y -= center_y;

    /* mirror the corners */
    newx1 = -object->box->upper_x;
    newy1 =  object->box->upper_y;
    newx2 = -object->box->lower_x;
    newy2 =  object->box->lower_y;

    /* reorder the corners */
    object->box->upper_x = min(newx1,newx2);
    object->box->upper_y = max(newy1,newy2);
    object->box->lower_x = max(newx1,newx2);
    object->box->lower_y = min(newy1,newy2);

    /* translate back in position */
    object->box->upper_x += center_x;
    object->box->upper_y += center_y;
    object->box->lower_x += center_x;
    object->box->lower_y += center_y;

    /* recalc boundings and world coords */
    object->bounds_valid = FALSE;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*! O0420
 * \brief Modify a Box Object's coordinates.
 * \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the box. The new coordinates of the corner identified by <B>whichone</B>
 *  are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate system.
 *  Screen coordinates and boundings are then updated.
 *
 * \param [in,out] object     GedaBox Object to be modified.
 * \param [in]     x          x coordinate.
 * \param [in]     y          y coordinate.
 * \param [in]     whichone   coordinate to change.
 *
 * \note
 *  <B>whichone</B> can take the following values:
 *  <DL>
 *    <DT>*</DT><DD>BOX_UPPER_LEFT
 *    <DT>*</DT><DD>BOX_LOWER_LEFT
 *    <DT>*</DT><DD>BOX_UPPER_RIGHT
 *    <DT>*</DT><DD>BOX_LOWER_RIGHT
 *  </DL>
 */
void geda_box_object_modify(GedaObject *object, int x, int y, int whichone)
{
  if (GEDA_IS_BOX(object)) {

    int tmp;

    /* change the position of the selected corner */
    switch(whichone) {
      case BOX_UPPER_LEFT:
        object->box->upper_x = x;
        object->box->upper_y = y;
        break;

      case BOX_LOWER_LEFT:
        object->box->upper_x = x;
        object->box->lower_y = y;
        break;

      case BOX_UPPER_RIGHT:
        object->box->lower_x = x;
        object->box->upper_y = y;
        break;

      case BOX_LOWER_RIGHT:
        object->box->lower_x = x;
        object->box->lower_y = y;
        break;

      default:
        return;
    }

    /* need to update the upper left and lower right corners */
    if (object->box->upper_x > object->box->lower_x) {
      tmp                  = object->box->upper_x;
      object->box->upper_x = object->box->lower_x;
      object->box->lower_x = tmp;
    }

    if (object->box->upper_y < object->box->lower_y) {
      tmp                  = object->box->upper_y;
      object->box->upper_y = object->box->lower_y;
      object->box->lower_y = tmp;
    }

    /* recalculate the world coords and the boundings */
    object->bounds_valid = FALSE;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*! O0421
 * \brief Modify a Box Object's coordinates.
 * \par Function Description
 *  Modifies the coordinates of all four corners of \a box, by setting
 *  the box to the rectangle enclosed by the points (\a x1, \a y1) and
 *  (\a x2, \a y2).
 *
 * \param [in,out] object   box #GedaObject to be modified.
 * \param [in]     x1       x coordinate of first corner of box.
 * \param [in]     y1       y coordinate of first corner of box.
 * \param [in]     x2       x coordinate of second corner of box.
 * \param [in]     y2       y coordinate of second corner of box,
 */
void geda_box_object_modify_all (GedaObject *object, int x1, int y1, int x2, int y2)
{
  if (GEDA_IS_BOX(object)) {

    object->box->lower_x = (x1 > x2) ? x1 : x2;
    object->box->lower_y = (y1 > y2) ? y2 : y1;

    object->box->upper_x = (x1 > x2) ? x2 : x1;
    object->box->upper_y = (y1 > y2) ? y1 : y2;

    /* recalculate the world coords and bounds */
    object->bounds_valid = FALSE;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*! O0422
 * \brief Create a Box Object
 * \par Function Description
 *  This function creates a new object representing a box.
 *
 *  The box is described by its upper left corner - <B>x1</B>, <B>y1</B> - and
 *  its lower right corner - <B>x2</B>, <B>y2</B>.
 *  The <B>type</B> parameter must be equal to <B>OBJ_BOX</B>. The <B>color</B>
 *  corresponds to the color the box will be drawn with.
 *  The <B>GedaObject</B> structure is allocated with the #geda_box_new()
 *  function. The structure describing the box is allocated and initialized
 *  with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default values : solid
 *  line type with a width of 0, and no filling. It can be changed after
 *  with the #geda_set_object_line_options() and #geda_set_object_fill_options().
 *
 * \param [in]     color        Box border color.
 * \param [in]     x1           Upper x coordinate.
 * \param [in]     y1           Upper y coordinate.
 * \param [in]     x2           Lower x coordinate.
 * \param [in]     y2           Lower y coordinate.
 *
 * \return The new GedaObject
 */
GedaObject *geda_box_object_new(int color, int x1, int y1, int x2, int y2)
{
  GedaBox    *box;
  GedaObject *new_obj;

  /* create the object */
  new_obj = geda_box_new();
  box     = GEDA_BOX(new_obj);

  new_obj->color = color;

  /* describe the box with its upper left and lower right corner */
  box->upper_x = x1;
  box->upper_y = y1;
  box->lower_x = x2;
  box->lower_y = y2;

  return new_obj;
}

/*!
 * \brief Print a GedaBox to Postscript document.
 * \par Function Description
 *  This function prints the box described by the <B>\a o_current</B>
 *  parameter to a Postscript document. It takes into account its line
 *  type and fill type.
 *  The Postscript document is descibed by the file pointer <B>fp</B>.
 *
 *  The validity of the <B>\a o_current</B> parameter is verified : a null pointer
 *  causes an error message and a return.
 *
 *  The description of the box is extracted from
 *  the <B>\a o_current</B> parameter :
 *  the coordinates of the box - upper left corner and width and
 *  height of the box -, its line type, its fill type.
 *
 *  The outline and the inside of the box are successively handled by two
 *  differend sets of functions.
 *
 * \param [in] toplevel  The GedaToplevel object.
 * \param [in] fp         FILE pointer to Postscript document.
 * \param [in] o_current  GedaBox Object to write to document.
 * \param [in] origin_x   Page x coordinate to place GedaBox Object.
 * \param [in] origin_y   Page y coordinate to place GedaBox Object.
 */
void geda_box_object_print(GedaToplevel *toplevel,
                           FILE         *fp,
                           GedaObject   *o_current,
                           int           origin_x,
                           int           origin_y)
{
  int x, y, width, height;
  int color;
  int line_width, capstyle, length, space;
  void (*outl_func)() = NULL;
  GedaBox *box;

  g_return_if_fail(GEDA_IS_BOX(o_current));

  box = GEDA_BOX(o_current);

  x = box->upper_x;
  y = box->upper_y;

  width  = abs(box->lower_x - box->upper_x);
  height = abs(box->lower_y - box->upper_y);

  color    = o_current->color;
  capstyle = geda_object_get_capstyle (o_current->line_options->line_end);

  /*! \note
   *  Depending on the type of the line for this particular box, the
   *  appropriate function is chosen among #geda_box_object_print_solid(),
   *  #geda_box_object_print_dotted(), #geda_box_object_print_dashed(),
   *  #geda_box_object_print_center() and #geda_box_object_print_phantom().
   *
   *  The needed parameters for each of these type is extracted from the
   *  <B>#GedaBox</B> object. Depending on the type, unused parameters are
   *  set to -1.
   *
   *  In the eventuality of a length and/or space null, the line is printed
   *  solid to avoid and endless loop produced by other functions in such a
   *  case.
   */

  line_width = o_current->line_options->line_width;

  if (line_width < MIN_LINE_WIDTH_THRESHOLD)
    line_width = geda_object_style_get_line_width(toplevel); /* 1st try updating style */

  if (line_width < MIN_LINE_WIDTH_THRESHOLD)
    line_width = MIN_LINE_WIDTH_THRESHOLD;        /* if STYLE_NONE  */

  length = o_current->line_options->line_length;
  space  = o_current->line_options->line_space;

  switch(o_current->line_options->line_type) {
    case(TYPE_SOLID):
      length = -1; space  = -1;
      outl_func = geda_box_object_print_solid;
      break;

    case(TYPE_DOTTED):
      length = -1;
      outl_func = geda_box_object_print_dotted;
      break;

    case(TYPE_DASHED):
      outl_func = geda_box_object_print_dashed;
      break;

    case(TYPE_CENTER):
      outl_func = geda_box_object_print_center;
      break;

    case(TYPE_PHANTOM):
      outl_func = geda_box_object_print_phantom;
      break;

    case(TYPE_ERASE):
      /* Unused for now, print it solid */
      length = -1; space  = -1;
      outl_func = geda_box_object_print_solid;
      break;
  }

  if ((length == 0) || (space == 0)) {
    length = -1; space  = -1;
    outl_func = geda_box_object_print_solid;
  }

  (*outl_func)(toplevel, fp,
               x, y, width, height,
               color,
               line_width,
               capstyle,
               length, space,
               origin_x, origin_y);

  /*! \note
   *  If the filling type of the box is not <B>HOLLOW</B>, the appropriate
   *  function is chosen among #geda_box_object_print_filled(), #geda_box_object_print_mesh()
   *  and #geda_box_object_print_hatch(). The corresponding parameters are extracted
   *  from the <B>\a o_current</B> object and corrected afterward.
   *
   *  The case where <B>pitch1</B> and <B>pitch2</B> are null or negative is
   *  avoided as it leads to an endless loop in most of the called functions.
   *  In such a case, the box is printed filled. Unused parameters for each of
   *  these functions are set to -1 or any passive value.
   */
  if (box->fill_options.fill_type != FILLING_HOLLOW) {

    void (*fill_func)();

    int fill_width, angle1, pitch1, angle2, pitch2;

    fill_width = box->fill_options.fill_width;
    angle1     = box->fill_options.fill_angle1;
    pitch1     = box->fill_options.fill_pitch1;
    angle2     = box->fill_options.fill_angle2;
    pitch2     = box->fill_options.fill_pitch2;

    switch(box->fill_options.fill_type) {
      case(FILL_SOLID):
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func = geda_box_object_print_filled;
        break;

      case(FILLING_MESH):
        fill_func = geda_box_object_print_mesh;
        break;

      case(FILLING_HATCH):
        angle2 = -1; pitch2 = 1;
        fill_func = geda_box_object_print_hatch;
        break;

      case(FILLING_VOID):
        /* Unused for now, print it filled */
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func = geda_box_object_print_filled;
        break;

      case(FILLING_HOLLOW):
        /* nop */
      default:
        fill_func = NULL;

    }

    if ((pitch1 <= 0) || (pitch2 <= 0)) {
      angle1 = -1; pitch1 = 1;
      angle2 = -1; pitch2 = 1;
      fill_func = geda_box_object_print_filled;
    }

    if (fill_func) {
      (*fill_func)(toplevel, fp,
                   x, y, width, height,
                   color,
                   fill_width,
                   angle1, pitch1, angle2, pitch2,
                   origin_x, origin_y);
    }
  }
}

/*!
 * \brief Print centered line type Box to Postscript document.
 * \par Function Description
 *  This function prints the outline of a box when a centered line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #geda_line_object_print_center() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Upper x coordinate of Box.
 * \param [in] y           Upper y coordinate of Box.
 * \param [in] width       Width of Box.
 * \param [in] height      Height of Box.
 * \param [in] color       Box color.
 * \param [in] line_width  Box Line width.
 * \param [in] capstyle    Box Line capstyle.
 * \param [in] length      Dashed line length.
 * \param [in] space       Amount of space between dashes.
 * \param [in] origin_x    Page x coordinate to place Box Object.
 * \param [in] origin_y    Page y coordinate to place Box Object.
 */
void geda_box_object_print_center(GedaToplevel *toplevel, FILE *fp,
                                  int x, int y,
                                  int width, int height,
                                  int color,
                                  int line_width, int capstyle, int length, int space,
                                  int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  geda_line_object_print_center(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_center(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_center(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_center(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
}

/*!
 * \brief Print a dashed Box to Postscript document.
 * \par Function Description
 *  This function prints the outline of a box when a dashed line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #geda_line_object_print_dashed() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Upper x coordinate of Box.
 * \param [in] y           Upper y coordinate of Box.
 * \param [in] width       Width of Box.
 * \param [in] height      Height of Box.
 * \param [in] color       Box color.
 * \param [in] line_width  Box Line width.
 * \param [in] capstyle    Box Line capstyle.
 * \param [in] length      Dashed line length.
 * \param [in] space       Amount of space between dashes.
 * \param [in] origin_x    Page x coordinate to place Box Object.
 * \param [in] origin_y    Page y coordinate to place Box Object.
 */
void geda_box_object_print_dashed(GedaToplevel *toplevel, FILE *fp,
                                  int x, int y,
                                  int width, int height,
                                  int color,
                                  int line_width, int capstyle, int length, int space,
                                  int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);


  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  geda_line_object_print_dashed(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_dashed(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_dashed(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_dashed(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
}

/*!
 * \brief Print a dotted GedaBox to Postscript document.
 * \par Function Description
 *  This function prints the outline of a box when a dotted line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> is ignored.
 *
 *  It uses the function #geda_line_object_print_dotted() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Upper x coordinate of Box.
 * \param [in] y           Upper y coordinate of Box.
 * \param [in] width       Width of Box.
 * \param [in] height      Height of Box.
 * \param [in] color       Box color.
 * \param [in] line_width  Box Line width.
 * \param [in] capstyle    Box Line capstyle.
 * \param [in] length      Dashed line length.
 * \param [in] space       Amount of space between dashes.
 * \param [in] origin_x    Page x coordinate to place Box Object.
 * \param [in] origin_y    Page y coordinate to place Box Object.
 */
void geda_box_object_print_dotted(GedaToplevel *toplevel, FILE *fp,
                                  int x,     int y,
                                  int width, int height,
                                  int color,
                                  int line_width, int capstyle,
                                  int length,     int space,
                                  int origin_x,   int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  geda_line_object_print_dotted(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_dotted(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_dotted(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  geda_line_object_print_dotted(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
}

/*!
 * \brief Print a solid pattern Box to Postscript document.
 * \par Function Description
 *  The function prints a filled box with a solid pattern. No outline is
 *  printed.
 *  The box is defined by the coordinates of its upper left corner in
 *  (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters. The postscript file is defined by the file
 *  pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B> and <B>pitch1</B>, <B>angle2</B> and <B>pitch2</B>
 *  parameters are ignored in this functions but kept for compatibility
 *  with other fill functions.
 *
 *  It uses the fbox postscript function defined in the prolog to
 *  specify a filled box.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Upper x coordinate of Box.
 * \param [in] y           Upper y coordinate of Box.
 * \param [in] width       Width of Box.
 * \param [in] height      Height of Box.
 * \param [in] color       Box color.
 * \param [in] fill_width  Box fill width. (unused).
 * \param [in] angle1      (unused).
 * \param [in] pitch1      (unused).
 * \param [in] angle2      (unused).
 * \param [in] pitch2      (unused).
 * \param [in] origin_x    Page x coordinate to place Box Object.
 * \param [in] origin_y    Page y coordinate to place Box Object.
 */
void geda_box_object_print_filled(GedaToplevel *toplevel, FILE *fp,
                                  int x,     int y,
                                  int width, int height,
                                  int color,
                                  int fill_width,
                                  int angle1,   int pitch1,
                                  int angle2,   int pitch2,
                                  int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y-height; /* move the origin to 0, 0*/
  fprintf(fp, "%d %d %d %d fbox\n", width, height, x1-origin_x, y1-origin_y);

}

/*!
 * \brief Print a hatch pattern Box to Postscript document.
 * \par Function Description
 *  The function prints a hatched box. No outline is printed. The box is
 *  defined by the coordinates of its upper left corner in (<B>x</B>,<B>y</B>) and
 *  its width and height given by the <B>width</B> and <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B>, <B>pitch1</B> parameters define the way the box
 *  has to be hatched.
 *  <B>angle2</B> and <B>pitch2</B> parameters are unused but kept for compatibility
 *  with other fill functions.
 *
 *  Negative or null values for <B>pitch1</B> are not allowed as it leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Upper x coordinate of the box.
 * \param [in] y           Upper y coordinate of the box.
 * \param [in] width       Width of the box.
 * \param [in] height      Height of the box.
 * \param [in] color       Box color.
 * \param [in] fill_width  Box fill width.
 * \param [in] angle1      Angle of hatch pattern.
 * \param [in] pitch1      Pitch of hatch pattern.
 * \param [in] angle2      (unused).
 * \param [in] pitch2      (unused).
 * \param [in] origin_x    Page x coordinate to place box Object.
 * \param [in] origin_y    Page y coordinate to place box Object.
 */
void geda_box_object_print_hatch(GedaToplevel *toplevel, FILE *fp,
                                 int x,     int y,
                                 int width, int height,
                                 int color,
                                 int fill_width,
                                 int angle1,   int pitch1,
                                 int angle2,   int pitch2,
                                 int origin_x, int origin_y)
{
  GedaBox box;
  int index;
  GArray *lines;

  g_return_if_fail(toplevel != NULL);
  g_return_if_fail(fp != NULL);

  f_print_set_color(toplevel, fp, color);

  /* Avoid printing line widths too small */
  if (fill_width <= 1) fill_width = 2;

  lines = g_array_new(FALSE, FALSE, sizeof(LINE));

  box.upper_x = x;
  box.upper_y = y;
  box.lower_x = x + width;
  box.lower_y = y - height;    /* Hmmm... */

  geda_math_hatch_box(&box, angle1, pitch1, lines);

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
 * \brief Print a mesh pattern Box to Postscript document.
 * \par Function Description
 *  This function prints a meshed box. No outline is printed. The box is
 *  defined by the coordinates of its upper left corner in (<B>x</B>,<B>y</B>) and
 *  its width and height given by the <B>width</B> and <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  The inside mesh is achieved by two successive call to the
 *  #geda_box_object_print_hatch() function, given <B>angle1</B> and <B>pitch1</B> the first
 *  time and <B>angle2</B> and <B>pitch2</B> the second time.
 *
 *  Negative or null values for <B>pitch1</B> and/or <B>pitch2</B> are not allowed
 *  as it leads to an endless loop in #geda_box_object_print_hatch().
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Upper x coordinate of Box.
 * \param [in] y           Upper y coordinate of Box.
 * \param [in] width       Width of Box.
 * \param [in] height      Height of Box.
 * \param [in] color       Box color.
 * \param [in] fill_width  Box fill width.
 * \param [in] angle1      1st angle for mesh pattern.
 * \param [in] pitch1      1st pitch for mesh pattern.
 * \param [in] angle2      2nd angle for mesh pattern.
 * \param [in] pitch2      2nd pitch for mesh pattern.
 * \param [in] origin_x    Page x coordinate to place Box Object.
 * \param [in] origin_y    Page y coordinate to place Box Object.
 */
void geda_box_object_print_mesh(GedaToplevel *toplevel, FILE *fp,
                                int x,     int y,
                                int width, int height,
                                int color,
                                int fill_width,
                                int angle1,   int pitch1,
                                int angle2,   int pitch2,
                                int origin_x, int origin_y)
{
  geda_box_object_print_hatch(toplevel, fp,
                    x, y, width, height,
                    color,
                    fill_width,
                    angle1, pitch1, -1, -1,
                    origin_x, origin_y);
  geda_box_object_print_hatch(toplevel, fp,
                    x, y, width, height,
                    color,
                    fill_width,
                    angle2, pitch2, -1, -1,
                    origin_x, origin_y);

}

/*!
 * \brief Print phantom line type Box to Postscript document.
 * \par Function Description
 *  This function prints the outline of a box when a phantom line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #geda_line_object_print_phantom() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Upper x coordinate of Box.
 * \param [in] y           Upper y coordinate of Box.
 * \param [in] width       Width of Box.
 * \param [in] height      Height of Box.
 * \param [in] color       Box color.
 * \param [in] line_width  Box Line width.
 * \param [in] capstyle    Box Line capstyle.
 * \param [in] length      Dashed line length.
 * \param [in] space       Amount of space between dashes.
 * \param [in] origin_x    Page x coordinate to place Box Object.
 * \param [in] origin_y    Page y coordinate to place Box Object.
 */
void geda_box_object_print_phantom(GedaToplevel *toplevel, FILE *fp,
                                   int x, int y,
                                   int width, int height,
                                   int color,
                                   int line_width, int capstyle, int length, int space,
                                   int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  geda_line_object_print_phantom(toplevel, fp,
                       x1, y1, x1 + width, y1,
                       color,
                       line_width, capstyle, length, space,
                       origin_x, origin_y);
  geda_line_object_print_phantom(toplevel, fp,
                       x1 + width, y1, x1 + width, y1 + height,
                       color,
                       line_width, capstyle, length, space,
                       origin_x, origin_y);
  geda_line_object_print_phantom(toplevel, fp,
                       x1 + width, y1 + height, x1, y1 + height,
                       color,
                       line_width, capstyle, length, space,
                       origin_x, origin_y);
  geda_line_object_print_phantom(toplevel, fp,
                       x1, y1 + height, x1, y1,
                       color,
                       line_width, capstyle, length, space,
                       origin_x, origin_y);
}

/*!
 * \brief Print a solid Box to Postscript document.
 * \par Function Description
 *  This function prints the outline of a box when a solid line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> and <B>space</B> are ignored.
 *
 *  It uses the function #geda_line_object_print_solid() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp          FILE pointer to Postscript document.
 * \param [in] x           Upper x coordinate of the box.
 * \param [in] y           Upper y coordinate of the box.
 * \param [in] width       Width of the box.
 * \param [in] height      Height of the box.
 * \param [in] color       Box color.
 * \param [in] line_width  Box Line width.
 * \param [in] capstyle    Box Line capstyle.
 * \param [in] length      Dashed line length.
 * \param [in] space       Amount of space between dashes.
 * \param [in] origin_x    Page x coordinate to place Box Object.
 * \param [in] origin_y    Page y coordinate to place Box Object.
 */
void geda_box_object_print_solid(GedaToplevel *toplevel, FILE *fp,
                                 int x, int y,
                                 int width, int height,
                                 int color,
                                 int line_width, int capstyle, int length, int space,
                                 int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  geda_line_object_print_solid(toplevel, fp,
                     x1, y1, x1 + width, y1,
                     color,
                     line_width, capstyle, length, space,
                     origin_x, origin_y);
  geda_line_object_print_solid(toplevel, fp,
                     x1 + width, y1, x1 + width, y1 + height,
                     color,
                     line_width, capstyle, length, space,
                     origin_x, origin_y);
  geda_line_object_print_solid(toplevel, fp,
                     x1 + width, y1 + height, x1, y1 + height,
                     color,
                     line_width, capstyle, length, space,
                     origin_x, origin_y);
  geda_line_object_print_solid(toplevel, fp,
                     x1, y1 + height, x1, y1,
                     color,
                     line_width, capstyle, length, space,
                     origin_x, origin_y);
}

/*! O0432
 * \brief Create a box from a character string.
 * \par Function Description
 *  This function gets the description of a box from the <B>*buf</B> character
 *  string.
 *
 *  Depending on <B>*version</B>, the correct file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 20000704 release
 *    <DT>*</DT><DD>the file format used for the releases after 2000704.
 *  </DL>
 *
 * \param [in]     buf             Character string with box description.
 * \param [in]     release_ver     libgeda release version number.
 * \param [in]     fileformat_ver  libgeda file format version number.
 *
 * \param [out] err                A GError object
 *
 * \return The GedaBox Object that was created, or NULL on error.
 */
GedaObject *geda_box_object_read (const char buf[], unsigned int release_ver,
                                                    unsigned int fileformat_ver,
                                                    GError **err)
{
  GedaObject *new_obj;
  char type;
  int x1, y1;
  int width, height;
  int d_x1, d_y1;
  int d_x2, d_y2;
  int color;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  int box_end;
  int box_type;
  int box_filling;

  if (release_ver <= VERSION_20000704) {

  /*! \note
   *  The old geda file format, i.e. releases 20000704 and older, does not
   *  handle the line type and the filling of the box object. They are set
   *  to default.
   */

    if (sscanf (buf, "%c %d %d %d %d %d\n",
        &type, &x1, &y1, &width, &height, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse box object"));
      return NULL;
    }

    box_width   = 0;
    box_end     = END_NONE;
    box_type    = TYPE_SOLID;
    box_length  = -1;
    box_space   = -1;

    box_filling = FILLING_HOLLOW;
    fill_width  = 0;
    angle1      = -1;
    pitch1      = -1;
    angle2      = -1;
    pitch2      = -1;

  }
  else {

    /*! \note
     *  The current line format to describe a box is a space separated list of
     *  characters and numbers in plain ASCII on a single line. The meaning of
     *  each item is described in the file format documentation.
     */
    if (sscanf (buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
                &type, &x1, &y1, &width, &height, &color,
                &box_width, &box_end, &box_type, &box_length,
                &box_space, &box_filling,
                &fill_width, &angle1, &pitch1, &angle2, &pitch2) != 17)
    {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse box object"));
      return NULL;
    }
    /* TODO Should check line type and filling here! */
  }

  if (width == 0 || height == 0) {
    const char *msg = _("Found a box with zero width/height");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: [%d x %d].\n", msg, width, height);
    }
  }

  if (color < 0 || color > MAX_COLORS) {
    const char *msg = _("Found an invalid color");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, color);
    }
    geda_log_w (_("Setting color to default color\n"));
    color = DEFAULT_BOX_COLOR_INDEX;
  }

  /*! \note
   *  A box is internally described by the lower right and upper left corner.
   *
   *  A new object is allocated, initialized and added to the object list.
   *  Its filling and line type are set according to the values of the field
   *  on the line.
   */

  /* upper left corner of the box */
  d_x1 = x1;
  d_y1 = y1 + height; /* move box origin to top left */

  /* lower right corner of the box */
  d_x2 = x1 + width;  /* end points of the box */
  d_y2 = y1;

  /* create a new box */
  new_obj = geda_box_object_new (color, d_x1, d_y1, d_x2, d_y2);

  /* set the line options */
  new_obj->line_options->line_end     = box_end;
  new_obj->line_options->line_type    = box_type;
  new_obj->line_options->line_width   = box_width;
  new_obj->line_options->line_length  = box_length;
  new_obj->line_options->line_space   = box_space;

  /* set the fill options */
  new_obj->fill_options->fill_type   = box_filling;
  new_obj->fill_options->fill_width  = fill_width;
  new_obj->fill_options->fill_angle1 = angle1;
  new_obj->fill_options->fill_angle2 = angle2;
  new_obj->fill_options->fill_pitch1 = pitch1;
  new_obj->fill_options->fill_pitch2 = pitch2;

  return new_obj;
}

/*! O0433
 * \brief Rotate GedaBox Object
 * \par Function Description
 *  The function #geda_box_object_rotate() rotate the box described by <B>*object</B>
 *  around the (<B>center_x</B>, <B>center_y</B>) point by <B>angle</B> degrees.
 *  The center of rotation is in world unit.
 *
 * \param [in,out]  object    GedaBox Object to rotate
 * \param [in]      center_x  Rotation center x coordinate
 * \param [in]      center_y  Rotation center y coordinate
 * \param [in]      angle     Rotation angle in degrees (See note below)
 *
 */
void geda_box_object_rotate(GedaObject *object, int center_x, int center_y, int angle)
{

  if (GEDA_IS_BOX(object)) {

    int newx1, newy1;
    int newx2, newy2;

    /*! \note
     *  Only 90 degree multiple and positive angles are allowed.
     */

    /* angle must be positive */
    if (angle < 0) angle = -angle;
    /* angle must be a 90 multiple or no rotation performed */
    if ((angle % 90) != 0) return;

    /*! \note
     *  The center of rotation (<B>center_x</B>, <B>center_y</B>) is
     *  translated to the origin. The rotation of the upper left and lower right
     *  corner are then performed. Finally, the rotated box is translated back
     *  to its previous location.
     */
    /* translate object to origin */
    object->box->upper_x -= center_x;
    object->box->upper_y -= center_y;
    object->box->lower_x -= center_x;
    object->box->lower_y -= center_y;

    /* rotate the upper left corner of the box */
    geda_math_rotate_point_90(object->box->upper_x, object->box->upper_y, angle,
                      &newx1, &newy1);

    /* rotate the lower left corner of the box */
    geda_math_rotate_point_90(object->box->lower_x, object->box->lower_y, angle,
                      &newx2, &newy2);

    /* reorder the corners after rotation */
    object->box->upper_x = min(newx1,newx2);
    object->box->upper_y = max(newy1,newy2);
    object->box->lower_x = max(newx1,newx2);
    object->box->lower_y = min(newy1,newy2);

    /* translate object back to normal position */
    object->box->upper_x += center_x;
    object->box->upper_y += center_y;
    object->box->lower_x += center_x;
    object->box->lower_y += center_y;

    /* recalc boundings and world coords */
    object->bounds_valid = FALSE;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Scale a GedaBox object
 * \par Function Description
 *  Increases the width of the box by \a x_scale and the height of the
 *  box by \a y_scale. If the scale factor is zero, then the corresponding
 *  dimension is not scale.
 *
 * \param [in] object  Must be a GedaBox object
 * \param [in] x_scale the scale factor by which to increase the width
 * \param [in] y_scale the scale factor by which to increase the height
 */
void geda_box_object_scale (GedaObject *object, int x_scale, int y_scale)
{
  if (GEDA_IS_BOX(object)) {

    int offset;

    if (x_scale) {

      int width;

      width = object->box->upper_x - object->box->lower_x;

      offset = ((width * x_scale) - width) >> 1;

      object->box->lower_x = object->box->lower_x - offset;
      object->box->upper_x = object->box->upper_x + offset;
    }

    if (y_scale) {

      int height;

      height = (object->box->upper_y - object->box->lower_y);

      offset = ((height * y_scale) - height) >> 1;

      object->box->lower_y = object->box->lower_y - offset;
      object->box->upper_y = object->box->upper_y + offset;
    }

    /* recalc boundings and world coords */
    object->bounds_valid = FALSE;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set Line Length of a Box object
 * \par Function Description
 *  Sets the line length value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] end_cap New value for the end-cap property
 *
 * \sa geda_box_set_line_type geda_box_object_get_end_cap
 */
void geda_box_object_set_end_cap (GedaObject *object, int end_cap)
{
  if (GEDA_IS_BOX(object)) {
    object->line_options->line_end = end_cap < END_NONE ? END_NONE :
                                     end_cap > END_VOID ? END_VOID :
                                     end_cap;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Angle 1 Property of a box object
 * \par Function Description
 *  Sets the fill angle 1 of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] angle   Angle value for the fill-angle1 in degrees
 *
 * \sa geda_box_set_fill_angle1 geda_box_object_get_fill_angle1
 */
void geda_box_object_set_fill_angle1 (GedaObject *object, int angle)
{
  if (GEDA_IS_BOX(object)) {
    object->fill_options->fill_angle1 = angle;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Angle 2 Property of a box object
 * \par Function Description
 *  Sets the fill angle 2 of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] angle   Angle value for the fill-angle2 in degrees
 *
 * \sa geda_box_set_fill_angle2 geda_box_object_get_fill_angle2
 */
void geda_box_object_set_fill_angle2 (GedaObject *object, int angle)
{
  if (GEDA_IS_BOX(object)) {
    object->fill_options->fill_angle2 = angle;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Pitch 1 Property of a box object
 * \par Function Description
 *  Sets the fill pitch 1 of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] pitch   New value for the fill-pitch1 property
 *
 * \sa geda_box_set_fill_pitch1 geda_box_object_get_fill_pitch1
 */
void geda_box_object_set_fill_pitch1 (GedaObject *object, int pitch)
{
  if (GEDA_IS_BOX(object)) {
    object->fill_options->fill_pitch1 = pitch;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Pitch 2 Property of a box object
 * \par Function Description
 *  Sets the fill pitch 2 of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] pitch   New value for the fill-pitch2 property
 *
 * \sa geda_box_set_fill_pitch1 geda_box_object_get_fill_pitch2
 */
void geda_box_object_set_fill_pitch2 (GedaObject *object, int pitch)
{
  if (GEDA_IS_BOX(object)) {
    object->fill_options->fill_pitch2 = pitch;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Type Property of a box object
 * \par Function Description
 *  Sets the fill type of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] type    New value for the fill-type property
 *
 * \sa geda_box_set_fill_type geda_box_object_get_fill_type
 */
void geda_box_object_set_fill_type (GedaObject *object, int type)
{
  if (GEDA_IS_BOX(object)) {
    object->fill_options->fill_type = type < TYPE_SOLID ? TYPE_SOLID :
                                      type > TYPE_ERASE ? TYPE_ERASE :
                                      type;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set the Fill Width Property of a box object
 * \par Function Description
 *  Sets the fill width of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] width   New value for the fill-width property
 *
 * \sa geda_box_set_fill_width geda_box_object_get_fill_width
 */
void geda_box_object_set_fill_width (GedaObject *object, int width)
{
  if (GEDA_IS_BOX(object)) {
    object->fill_options->fill_width = width < 0 ? 0 : width;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set Line Length of a Box object
 * \par Function Description
 *  Sets the line length value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] length  New value for the line-length property
 *
 * \sa geda_box_set_line_type
 */
void geda_box_object_set_line_length (GedaObject *object, int length)
{
  if (GEDA_IS_BOX(object)) {
    object->line_options->line_length = length > 0 ? length : 0;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set Line Space of a Box object
 * \par Function Description
 *  Sets the line space value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] space   New value for the line-space property
 *
 * \sa geda_box_set_line_type
 */
void geda_box_object_set_line_space (GedaObject *object, int space)
{
  if (GEDA_IS_BOX(object)) {
    object->line_options->line_space = space > 0 ? space : 0;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set Line Type of a Box object
 * \par Function Description
 *  Sets the line type value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] type    New value for the type property
 *
 * \sa geda_box_set_line_type
 */
void geda_box_object_set_line_type (GedaObject *object, int type)
{
  if (GEDA_IS_BOX(object)) {
    object->line_options->line_type = type < TYPE_SOLID ? TYPE_SOLID :
                                      type > TYPE_ERASE ? TYPE_ERASE :
                                      type;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*!
 * \brief Set Line Width of a Box object
 * \par Function Description
 *  Sets the line width value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] width   New value for the width property
 *
 * \sa geda_box_set_line_width
 */
void geda_box_object_set_line_width (GedaObject *object, int width)
{
  if (GEDA_IS_BOX(object)) {
    object->line_options->line_width = width > 0 ? width : 0;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*! O0445
 * \brief Set Lower X coordinate of a Box object
 * \par Function Description
 *  Sets the lower x value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] x       New value for the lower x coordinate
 *
 * \sa geda_box_object_get_lower_x
 */
void geda_box_object_set_lower_x (GedaObject *object, int x) {
  if (GEDA_IS_BOX(object)) {
    object->box->lower_x = x;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*! O0446
 * \brief Set Lower Y coordinate of a Box object
 * \par Function Description
 *  Sets the lower y value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] y       New value for the lower y coordinate
 *
 * \sa geda_box_object_get_lower_y
 */
void geda_box_object_set_lower_y (GedaObject *object, int y) {
  if (GEDA_IS_BOX(object)) {
    object->box->lower_y = y;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*! O0447
 * \brief Set Upper X coordinate of a Box obje
 * \par Function Description
 *  Sets the upper x value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] x       New value for the upper x coordinate
 *
 * \sa geda_box_object_get_upper_x
 */
void geda_box_object_set_upper_x (GedaObject *object, int x) {
  if (GEDA_IS_BOX(object)) {
    object->box->upper_x = x;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*! O0448
 * \brief Set Upper Y coordinate of a Box obje
 * \par Function Description
 *  Sets the upper y value of the box \a object. Does nothing if
 *  \a object is not a valid GedaBox object.
 *
 * \param [in] object  Pointer to an Box GedaObject
 * \param [in] y       New value for the upper y coordinate
 *
 * \sa geda_box_object_get_upper_y
 */
void geda_box_object_set_upper_y (GedaObject *object, int y) {
  if (GEDA_IS_BOX(object)) {
    object->box->upper_y = y;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/*! O0449
 * \brief Calculates the distance between the given point and the closest
 *  point on the perimeter of the box.
 *
 * \param [in] object       A box GedaObject.
 * \param [in] x            The x coordinate of the given point.
 * \param [in] y            The y coordinate of the given point.
 * \param [in] force_solid  If true, force treating the object as solid.
 *
 * \return The shortest distance from the object to the point. With an
 *         invalid parameter, this function returns G_MAXDOUBLE.
 */
double geda_box_object_shortest_distance (ConstObject *object, int x, int y, int force_solid)
{
  if (GEDA_IS_BOX(object)) {

    int solid;

    solid = force_solid || object->fill_options->fill_type != FILLING_HOLLOW;

    return geda_math_box_shortest_distance (object->box, x, y, solid);
  }
  geda_box_object_error(__func__, object);
  return (G_MAXDOUBLE);
}

/*! O0450
 * \brief Create a character string representation of a GedaBox.
 * \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe the
 *  box object <B>*object</B> following the post-20000704 release file format,
 *  (which handles the line type and fill options).
 *
 * \note object was validated by geda_object_save_objects
 *
 * \param [in] object  The GedaBox Object to create string from.
 *
 * \return A pointer to the GedaBox character string.
 *
 * \remarks Caller should GEDA_FREE returned character string.
 */
char *geda_box_object_to_buffer(GedaObject *object)
{
  GedaBox *box;
  char    *buf;

  int x1, y1;
  int width, height;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;

  LINE_END       box_end;
  LINE_TYPE      box_type;
  OBJECT_FILLING box_fill;

  box = GEDA_BOX(object);

  /*! \note
   *  A box is internally represented by its lower right and upper left corner
   *  whereas it is described in the file format as its lower left corner and
   *  its width and height.
   */

  /* calculate the width and height of the box */
  width  = abs(box->lower_x - box->upper_x);
  height = abs(box->upper_y - box->lower_y);

  /* calculate the lower left corner of the box */
  x1 = box->upper_x;
  y1 = box->upper_y - height; /* move the origin to 0, 0*/

#if DEBUG
  printf("box: %d %d %d %d\n", x1, y1, width, height);
#endif

  /* description of the line type for the outline */
  box_end    = object->line_options->line_end;
  box_width  = object->line_options->line_width;
  box_type   = object->line_options->line_type;
  box_length = object->line_options->line_length;
  box_space  = object->line_options->line_space;

  /* description of the filling of the box */
  box_fill   = object->fill_options->fill_type;
  fill_width = object->fill_options->fill_width;
  angle1     = object->fill_options->fill_angle1;
  pitch1     = object->fill_options->fill_pitch1;
  angle2     = object->fill_options->fill_angle2;
  pitch2     = object->fill_options->fill_pitch2;

  buf = geda_sprintf("%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                     object->type,
                     x1, y1, width, height, object->color,
                     box_width, box_end, box_type, box_length, box_space,
                     box_fill,
                     fill_width, angle1, pitch1, angle2, pitch2);

  return(buf);
}

/*! O0451
 * \brief Translate a Box position by a delta.
 * \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the box
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 * \param [in,out] object     Box Object to translate
 * \param [in]     dx         x distance to move
 * \param [in]     dy         y distance to move
 */
void geda_box_object_translate(GedaObject *object, int dx, int dy)
{
  if (GEDA_IS_BOX(object)) {

    object->box->upper_x = object->box->upper_x + dx;
    object->box->upper_y = object->box->upper_y + dy;
    object->box->lower_x = object->box->lower_x + dx;
    object->box->lower_y = object->box->lower_y + dy;

    /* recalc the screen coords and the bounding box */
    object->bounds_valid = FALSE;
  }
  else {
    geda_box_object_error(__func__, object);
  }
}

/** @} endgroup geda-box-object-proc */
