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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */

/*! \file o_arc_object.c
 *  \brief Functions for the arc object
 */

/** \defgroup arc-object-proc GedaArc Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaArc Objects
 */

#include "../../../config.h"

#include <stdio.h>
#include <math.h>

#include <libgeda_priv.h>

static void geda_object_error(const char *func, const void *object, IDE_OBJECT_TYPE type)
{
  geda_error_object_argument(__FILE__, func, object, type);
}

static void geda_arc_object_error(const char *func, const void *object)
{
  geda_object_error(func, object, GEDA_OBJECT_ARC);
}

/*! O0201
 * \brief
 * \par Function Description
 *  This function creates a new object representing an arc. The
 *  values of the <B>\a o_current</B> pointed GedaObject are then copied
 *  to the new object. Line options are initialized whereas the
 *  fill options are initialized to passive values - as an arc
 *  can not be filled.
 *
 * \param [in] o_source  GedaObject object of type GedaArc
 *
 * \return The new GedaObject
 */
GedaObject *geda_arc_object_copy(GedaObject *o_source)
{
  if (GEDA_IS_ARC(o_source)) {

    GedaObject *new_obj;

    new_obj = geda_arc_object_new (o_source->color,
                                   o_source->arc->x,
                                   o_source->arc->y,
                                   o_source->arc->radius,
                                   o_source->arc->start_angle,
                                   o_source->arc->arc_sweep);

    geda_set_object_line_options(new_obj, o_source->line_options);

    geda_set_object_fill_options(new_obj, o_source->fill_options);

    return new_obj;
  }
  geda_arc_object_error(__func__, o_source);
  return NULL;
}

/*! O0202
 * \brief Get the sweep angle of the arc
 * \par Function Description
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return The sweep angle of the arc
 *
 * \sa geda_arc_get_arc_sweep
 */
int geda_arc_object_get_arc_sweep (const GedaObject *object)
{
  if (GEDA_IS_ARC(object))
    return object->arc->arc_sweep;

  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0203
 * \brief Get the x coordinate of the center of the arc
 * \par Function Description
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return The x coordinate of the center of the arc
 *
 * \sa geda_arc_get_center_x
 */
int geda_arc_object_get_center_x (const GedaObject *object)
{
  if (GEDA_IS_ARC(object))
    return object->arc->x;

  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0204
 * \brief Get the y coordinate of the center of the arc
 * \par Function Description
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return The y coordinate of the center of the arc
 *
 * \sa geda_arc_get_center_y
 */
int geda_arc_object_get_center_y (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->arc->y;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0205
 * \brief Get End Cap type Property of an Arc Object
 * \par Function Description
 *  Returns the value of arc \a object end-cap type property.
 *
 * \param [in] object Pointer to an Arc GedaObject
 *
 * \return integer value of end-cap type or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_end_cap
 */
int geda_arc_object_get_end_cap (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->line_options->line_end;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0206
 * \brief Get Fill Angle 1 of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object fill angle1 property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of fill angle1 or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_fill_angle1 geda_arc_object_set_fill_angle1
 */
int geda_arc_object_get_fill_angle1 (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->fill_options->fill_angle1;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0207
 * \brief Get Fill Angle 2 of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object fill angle2 property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of fill angle2 or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_fill_angle2 geda_arc_object_set_fill_angle2
 */
int geda_arc_object_get_fill_angle2 (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->fill_options->fill_angle2;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0208
 * \brief Get Fill Pitch 1 of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object fill pitch1 property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of fill pitch1 or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_fill_pitch1 geda_arc_object_set_fill_pitch1
 */
int geda_arc_object_get_fill_pitch1 (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->fill_options->fill_pitch1;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0209
 * \brief Get Fill Pitch 2 of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object fill pitch2 property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of fill pitch2 or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_fill_pitch2 geda_arc_object_set_fill_pitch2
 */
int geda_arc_object_get_fill_pitch2 (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->fill_options->fill_pitch2;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0210
 * \brief Get Fill Type Property of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object fill type property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of fill type or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_fill_type geda_arc_object_set_fill_type
 */
int geda_arc_object_get_fill_type (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->fill_options->fill_type;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0211
 * \brief Get Fill Width Property of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object fill width property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of fill width or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_fill_width geda_arc_object_set_fill_width
 */
int geda_arc_object_get_fill_width (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->fill_options->fill_width;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0212
 * \brief Get Line Length Property of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object line length property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of line length or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_line_length geda_arc_object_set_line_length
 */
int geda_arc_object_get_line_length (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->line_options->line_length;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0213
 * \brief Get Line Space Property of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object line space property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of line space or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_line_space geda_arc_object_set_line_space
 */
int geda_arc_object_get_line_space (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->line_options->line_space;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0214
 * \brief Get Line Type Property of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object line type property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of line type or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_line_type geda_arc_object_set_line_type
 */
int geda_arc_object_get_line_type (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->line_options->line_type;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0215
 * \brief Get Line Width Property of an Arc Object
 * \par Function Description
 *  Returns the value of the arc \a object line width property.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return integer value of line width or -0 if \a arc is invalid.
 *
 * \sa geda_arc_get_line_width geda_arc_object_set_line_width
 */
int geda_arc_object_get_line_width (const GedaObject *object)
{
  if (GEDA_IS_ARC(object)) {
    return object->line_options->line_width;
  }
  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0216
 * \brief Get Point on an GedaArc Nearest a Given Point
 * \par Function Description
 *  This function is intended to locate a point on an GedaArc object given
 *  a point \a x, \a y, that is on or about the vicinity of the \a object.
 *  If True is returned, <B>nx</B> and <B>ny</B> are set world unit to a
 *  point on the arc that is the closest point on the arc to the point
 *  given by \a x, \a y.
 *
 * \param [in]  object  Pointer to an Arc GedaObject
 * \param [in]  x       Integer x of point near or on the arc
 * \param [in]  y       Integer y of point near or on the arc
 * \param [out] nx      Integer pointer to resulting x value
 * \param [out] ny      Integer pointer to resulting y value
 *
 * \returns TRUE if the results are valid, FALSE if \a object was not an
 *          GedaArc object, or if (<B>dx</B>,<B>dy</B>) is the centerpoint of the arc.
 */
bool geda_arc_object_get_nearest_point (const GedaObject *object, int x, int y, int *nx, int *ny)
{
  GedaArc *arc;
  bool result;

  if (GEDA_IS_ARC(object)) {

    arc = object->arc;

    /* If the point is the center, every point on the arc is equal
     * distance to the point, so the answer is false */
    if ((y == arc->y) && (x == arc->x)) {
      result = FALSE;
    }
    else {

      int    cx, cy, r;
      double dx, dy;

      cx = arc->x;
      cy = arc->y;
      r  = arc->radius;

      int    arc_angle   = arc->start_angle + arc->arc_sweep;
      double start_angle = geda_math_degrees_to_radians (arc->start_angle);
      double end_angle   = geda_math_degrees_to_radians (arc_angle);

      /* Get angle of ray from point to center */
      double radians = atan2((y - cy), (x - cx));

      /* If negative, make the angle positive */
      if (radians < 0) {
        radians += 2.0 * M_PI;
      }

      if (radians < end_angle && radians > start_angle) {

        double A, /* B, */ C, D;

        volatile double b;
        volatile double m;
                 double tmp_x, tmp_y;

        dx = x - cx;
        dy = y - cy;

        /* Conventional: (x - cx)^2 + (mx + b - cy)^2 = r^2, solve for x
         *
         * note: calculating as if the arc is at the origin, cx = cy = 0,
         * to prevent over-flow errors for arcs > ~32k from origin */

        /* get slope of line connecting the point to the center of the arc */
        m = dy / dx;

        A = m * m + 1;

        /* reduce   B = 2 * ((m * b) - (m * cy) - cx);
         * to  ==>  B = 2 * m * b;
         * to  ==>  B = 0;  */

        /* B = 0; since intercept is the origin */

        /* reduce   C = (cy * cy) + (cx * cx) - (r * r) - (2 * (b * cy)) + (b * b);
         * to  ==>  C = -1 * r * r + b * b; and since intercept is the origin
         * to  ==>  C = -1 * r * r;
         */
        C = -1 * r * r;

        /* The D = (B * B) - (4 * A * C) reduces to */
        D = 0 - (4.0 * A * C);                       /* The discriminant */

        if (cx > x) {                                /* Easterly */
          tmp_x = (0 - sqrt(D)) / (2.0 * A);
        }
        else {                                       /* Westward */
          tmp_x = sqrt(D) / (2.0 * A);
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
      else {

        double distance_to_end0;
        double distance_to_end1;
        double sx, sy, ex, ey;

        sx = cx + r * cos (start_angle);
        sy = cy + r * sin (start_angle);

        dx = sx - x;
        dy = sy - y;

#if HAVE_HYPOT
        distance_to_end0 = hypot (dx, dy);
#else
        distance_to_end0 = sqrt ((dx * dx) + (dy * dy));
#endif

        ex = arc->x + r * cos (end_angle);
        ey = arc->y + r * sin (end_angle);

        dx = ex - x;
        dy = ey - y;

#if HAVE_HYPOT
        distance_to_end1 = hypot (dx, dy);
#else
        distance_to_end1 = sqrt ((dx * dx) + (dy * dy));
#endif

        if (distance_to_end0 < distance_to_end1) {
          *nx = sx;
          *ny = sy;
        }
        else {
          *nx = ex;
          *ny = ey;
        }
      }
      result = TRUE;
    }
  }
  else { /* was not an GedaArc */
    geda_arc_object_error(__func__, object);
    result = FALSE;
  }

  /* If FALSE set output to the input values */
  if (!result) {
    *nx = x;
    *ny = y;
  }
  return result;
}

/*! O0217
 * \brief get the position of the center point
 * \par Function Description
 *  This function gets the position of the center point of an arc object.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [out] x      pointer to the x-position
 * \param [out] y      pointer to the y-position
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_arc_object_get_position (GedaObject *object, int *x, int *y)
{
  if (GEDA_IS_ARC(object)) {
    *x = object->arc->x;
    *y = object->arc->y;
    return (x || y) ? TRUE : FALSE;
  }

  geda_arc_object_error(__func__, object);
  return 0;
}

/*! O0218
 * \brief Get the radius of the arc
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return radius of the arc
 *
 * \sa geda_arc_get_arc_radius
 */
int
geda_arc_object_get_radius (const GedaObject *object)
{
  if (GEDA_IS_ARC(object))
    return object->arc->radius;

  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0219
 * \brief Get the starting angle of the arc
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 *
 * \return The starting angle of the arc
 *
 * \sa geda_arc_get_arc_start_angle
 */
int geda_arc_object_get_start_angle (const GedaObject *object)
{
  if (GEDA_IS_ARC(object))
    return object->arc->start_angle;

  geda_arc_object_error(__func__, object);
  return -0;
}

/*! O0220
 * \brief Mirror the coordinates of an ARC.
 * \par Function Description
 *  This function mirrors the world coordinates of an arc.
 *  The symetry axis is given by the vertical line going through the point (<B>center_x</B>,<B>center_y</B>).
 *
 *  The arc is translated in order to put the point (<B>center_x</B>,<B>center_y</B>)
 *  on the origin. The center of the arc is then mirrored. The start angle of the arc
 *  and the sweep of the arc are also mirrored.
 *
 *  The arc is finally back translated to its previous location on the page.
 *
 * \param [in] object    GedaObject object of type GedaArc
 * \param [in] center_x
 * \param [in] center_y
 */
void geda_arc_object_mirror(GedaObject *object, int center_x, int center_y)
{
  if (GEDA_IS_ARC(object)) {

    /* translate object to origin */
    object->arc->x -= center_x;
    object->arc->y -= center_y;

    /* get center, and mirror it (vertical mirror) */
    object->arc->x = -object->arc->x;

    /* apply mirror to angles (vertical mirror) */
    object->arc->start_angle = (180 - object->arc->start_angle) % 360;
    /* start_angle *MUST* be positive */
    if(object->arc->start_angle < 0) object->arc->start_angle += 360;
    object->arc->arc_sweep = -object->arc->arc_sweep;

    /* translate object back to its previous position */
    object->arc->x += center_x;
    object->arc->y += center_y;

    /* update the screen coords and bounding box */
    object->bounds_valid = FALSE;
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0221
 * \brief
 * \par Function Description
 *  This function modifies the internal values of the arc object
 *  object according to the whichone parameter.
 *
 *  The new values are given by <B>x</B> and/or <B>y</B>. Their meaning depends
 *  on the value of whichone. If <B>whichone</B> is equal to #ARC_CENTER, the (<B>x</B>,<B>y</B>)
 *  point is taken as the new center of the arc in world unit.
 *
 *  If <B>whichone</B> is equal to #ARC_RADIUS, the <B>x</B> parameter is taken to
 *  be the radius of the arc in world unit. The <B>y</B> parameter is ignored.
 *
 *  If <B>whichone</B> is equal to #ARC_START_ANGLE, the <B>x</B> parameter is the
 *  starting angle of the arc. <B>x</B> is in degrees. <B>y</B> is ignored.
 *
 *  If <B>whichone</B> is equal to #ARC_END_ANGLE, the <B>x</B> parameter is the
 *  ending angle of the arc. <B>x</B> is in degrees. <B>y</B> is ignored.
 *
 * \param [in,out] object    A GedaObject object of type GedaArc
 * \param [in]     x
 * \param [in]     y
 * \param [in]     whichone
 */
void geda_arc_object_modify(GedaObject *object, int x, int y, int whichone)
{
  if (GEDA_IS_ARC(object)) {

    switch(whichone) {
      case ARC_CENTER:
        /* modify the center of arc object */
        object->arc->x = x;
        object->arc->y = y;
        break;

      case ARC_RADIUS:
        /* modify the radius of arc object */
        object->arc->radius = x;
        break;

      case ARC_START_ANGLE:
        /* modify the start angle of the arc object */
        object->arc->start_angle = x;
        break;

      case ARC_END_ANGLE:
        /* modify the end angle of the arc object */
        object->arc->arc_sweep = x;
        break;

      default:
        break;
    }

    /* update the screen coords and the bounding box */
    object->bounds_valid = FALSE;
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0222
 * \brief Create a New GedaArc Object
 * \par Function Description
 *  The function creates a new GedaObject of type GedaArc.
 *
 *  The arc is defined by its center in parameters x and y.
 *  The radius parameter specifies the radius of the arc. The start
 *  angle is given by start_angle and the end angle by arc_sweep.
 *  The line and fill type of the created arc are set to default.
 *
 *  All coordinates are in dimensionless, except start_angle and
 *  arc_sweep, which must be in degrees.
 *
 *  A new object of type GedaObject is allocated. Its type and color
 *  are initilized. The description of the arc characteristics
 *  are stored in a new GedaArc structure.
 *
 * \param [in] color
 * \param [in] x
 * \param [in] y
 * \param [in] radius
 * \param [in] start_angle
 * \param [in] arc_sweep
 *
 * \returns new GedaObject of type GedaArc
 */
GedaObject *geda_arc_object_new (int color, int x, int y, int radius, int start_angle, int arc_sweep)
{
  GedaObject *new_obj;
  GedaArc    *arc;

  new_obj = geda_arc_new();
  arc     = GEDA_ARC(new_obj);

  new_obj->color = color;

  /*! \note
   *  The GedaArc structure is initialized with the parameters.
   *  A default initialization is performed for the line and
   *  fill type to avoid misunderstanding.
   *
   *  The functions relative to the use of the object are sets.
   */

  /* User coordinates */
  arc->x      = x;
  arc->y      = y;
  arc->radius = radius;

  /* must check the sign of start_angle, arc_sweep ... */
  if (arc_sweep < 0) {
    start_angle = start_angle + arc_sweep;
    arc_sweep   = -arc_sweep;
  }

  if (start_angle < 0) {
    start_angle = 360 + start_angle;
  }

  arc->start_angle = start_angle;
  arc->arc_sweep   = arc_sweep;

  return new_obj;
}

/*! O0223
 * \brief
 * \par Function Description
 *  This function writes in a postscript file pointed to by <B>\a fp</B>
 *  an  Arc to descripe by<B>\a object </B>.  Parameters extracted from
 *  \a object are formatted to suit future calls to specialized arc
 *  printing functions.
 *
 * \param [in] toplevel   The GedaToplevel object.
 * \param [in] fp         The postscript document to print to.
 * \param [in] object
 * \param [in] origin_x
 * \param [in] origin_y
 */
void geda_arc_object_print(GedaToplevel *toplevel,
                           FILE         *fp,
                           GedaObject   *object,
                           int origin_x, int origin_y)
{
  int x, y, radius, start_angle, arc_sweep;
  int color;
  int capstyle;
  int arc_width, space, length;
  void (*outl_func)() = NULL;

  g_return_if_fail(GEDA_IS_ARC(object));

  x = object->arc->x;
  y = object->arc->y;

  radius      = object->arc->radius;
  start_angle = object->arc->start_angle;
  arc_sweep   = object->arc->arc_sweep;
  color       = object->color;
  capstyle    = geda_object_get_capstyle (object->line_options->line_end);

  /*! \note
   *  Depending on the type of the line for this particular arc, the
   *  appropriate function is chosen among #geda_arc_object_print_solid(),
   *  #geda_arc_object_print_dotted(), #geda_arc_object_print_dashed(),
   *  #geda_arc_object_print_center() and #geda_arc_object_print_phantom().
   *
   *  The needed parameters for each of these types are extracted from the
   *  <B>object</B> object. Depending on the type, unused parameters are set to -1.
   *
   *  In the eventuality of a length and/or space null, the arc is printed
   *  solid to avoid and endless loop produced by other functions.
   */

  /* 09/08/12 | W.E.Hill Modified algorithms to incorperate both THICK & THIN
   *            styles, and eliminate hard-coded integer values.
   */

  arc_width = object->line_options->line_width;
  if(arc_width < MIN_LINE_WIDTH_THRESHOLD)
     arc_width = geda_object_style_get_line_width(toplevel); /* 1st try updating style */
  if(arc_width < MIN_LINE_WIDTH_THRESHOLD)
     arc_width = MIN_LINE_WIDTH_THRESHOLD;        /* if STYLE_NONE  */

  length = object->line_options->line_length;
  space  = object->line_options->line_space;

  switch(object->line_options->line_type) {
    case(TYPE_SOLID):
      length = -1; space = -1;
      outl_func = geda_arc_object_print_solid;
      break;

    case(TYPE_DOTTED):
      length = -1;
      outl_func = geda_arc_object_print_dotted;
      break;

    case(TYPE_DASHED):
      outl_func = geda_arc_object_print_dashed;
      break;

    case(TYPE_CENTER):
      outl_func = geda_arc_object_print_center;
      break;

    case(TYPE_PHANTOM):
      outl_func = geda_arc_object_print_phantom;
      break;

    case(TYPE_ERASE):
      /* Unused for now, print it solid */
      length = -1; space = -1;
      outl_func = geda_arc_object_print_solid;
      break;
  }

  if((space == 0) || (length == 0)) {
    length = -1; space = -1;
    outl_func = geda_arc_object_print_solid;
  }

  (*outl_func)(toplevel, fp,
               x - origin_x, y - origin_x, radius,
               start_angle, arc_sweep,
               color, arc_width, capstyle, length, space, origin_x, origin_y);
}

/*! O0224
 * \brief
 *  \par Function Description
 *  This function prints an arc when a centered line type is required. The
 *  arc is defined by its center in <B>x</B> and <B>y</B>, its radius in
 *  <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>. The
 *  <B>arc_width</B> parameter specifies the diameter of the dots and the
 *  width of the dashes of the printed line.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an endless loop.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 *  The function sets the color in which the line will be printed with.
 *
 *  \param [in] toplevel  The GedaToplevel object.
 *  \param [in] fp        FILE pointer to postscript document.
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] angle1
 *  \param [in] angle2
 *  \param [in] color
 *  \param [in] arc_width
 *  \param [in] capstyle
 *  \param [in] length
 *  \param [in] space
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void geda_arc_object_print_center(GedaToplevel *toplevel, FILE *fp,
                                  int x, int y, int radius,
                                  int angle1, int angle2,
                                  int color,
                                  int arc_width,
                                  int capstyle, int length, int space,
                                  int origin_x, int origin_y)
{
  int da, db, a1, d;

  f_print_set_color(toplevel, fp, color);

  /*! \note
   *  Depending on the radius of the arc, the <B>space</B> (resp. <B>length</B>)
   *  parameter is changed into a small angle <B>da</B> (resp. <B>db</B>).
   *  Starting from <B>angle1</B> - the start angle - the dashes are printed
   *  along the arc by increments of these new angles.
   *
   *  As <B>da</B> (resp. <B>db</B>) is rounded as an integer, it can take a null
   *  value which will make the function enter an endless loop. In such a case,
   *  the arc is printed solid. The <B>da</B> (resp. <B>db</B>) variable should
   *  never be negative except if <B>space</B> (resp. <B>length</B>) is negative.
   *
   *  Print as many sets of dash-dot as possible.
   */

  /* Inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }

  da = (int) ((length * 180) / (M_PI * ((double) radius)));
  db = (int) ((space  * 180) / (M_PI * ((double) radius)));

  /* If either da or db are too small to be displayed, draw a solid arc */
  if ((da <= 0) || (db <= 0)) {
    geda_arc_object_print_solid(toplevel, fp,
                      x, y, radius,
                      angle1, angle2,
                      color,
                      arc_width, capstyle, length, space, origin_x, origin_y);
    return;
  }

  fprintf(fp, "[");
  d = angle1;
  while ((d + da + 2 * db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;
    fprintf(fp,"[%d %d] ",(int) a1, (int) a1 + da);

    d = d + db;
    /*  xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
     *  ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
     */
    fprintf(fp,"[%d] ",d);
    d = d + db;
  }

  /*! \note
   *  When the above condition is no longer satisfied, then it is not
   *  possible to print a dash of length <B>length</B>. However two cases
   *  are possible:
   *  <DL>
   *      <DT>*</DT><DD>it is possible to print the dash and the dot
   *      <DT>*</DT><DD>it is possible to print the dash or a part of the original dash
   *  </DL>
   */

  a1 = d;
  d  = d + da;

  fprintf(fp,"[%d %d] ",(int) a1, (int) a1 + da);


  if ((d + db) < (angle1 + angle2)) {
    /*
     *     xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
     *     ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
     */
    fprintf(fp,"[%d] ",d);

  }

  fprintf(fp,"] %d %d %d %d %d dashedarc %% center\n",
          x,y, radius, arc_width, capstyle);
}

/*! O0225
 * \brief
 *  \par Function Description
 *  This function prints an arc when a dashed line type is required. The arc
 *  is defined by its center in <B>x</B> and <B>y</B>, its radius in <B>radius</B>
 *  and the start and end angles of the arc on the circle. The postscript file
 *  is defined by the file pointer <B>fp</B>. The parameter <B>arc_width</B> specifies
 *  the diameter of the dots of the printed line.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an endless loop.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 *  The function sets the color the line will be printed with.
 *
 *  \param [in] toplevel  The GedaToplevel object.
 *  \param [in] fp         FILE pointer to postscript document.
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] angle1
 *  \param [in] angle2
 *  \param [in] color
 *  \param [in] arc_width
 *  \param [in] capstyle
 *  \param [in] length
 *  \param [in] space
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void geda_arc_object_print_dashed(GedaToplevel *toplevel, FILE *fp,
                                  int x, int y, int radius,
                                  int angle1, int angle2,
                                  int color,
                                  int arc_width,
                                  int capstyle, int length, int space,
                                  int origin_x, int origin_y)
{
  int da, db, a1, d;

  f_print_set_color(toplevel, fp, color);

  /*! \note
   *  Depending on the radius of the arc, the <B>space</B> (resp. <B>length</B>)
   *  parameter is changed into a small angle <B>da</B> (resp. <B>db</B>).
   *  Starting from <B>angle1</B> - the start angle - the dashes are printed
   *  along the arc by increments of these new angles.
   *
   *  As <B>da</B> (resp. <B>db</B>) is rounded as an integer, it can take a
   *  null value which will make the function enter an endless loop. In such a case,
   *  the arc is printed solid. The <B>da</B> (resp. <B>db</B>) variable should never
   *  be negative except if <B>space</B> (resp. <B>length</B>) is negative.
   *
   *  It prints as many dashes of length <B>length</B> as possible.
   */

  /* Inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }

  da = (int) ((length * 180) / (M_PI * ((double) radius)));
  db = (int) ((space  * 180) / (M_PI * ((double) radius)));

  /* If da or db too small for arc to be displayed as dotted, draw a solid arc */
  if ((da <= 0) || (db <= 0)) {
    geda_arc_object_print_solid(toplevel, fp,
                      x, y, radius,
                      angle1, angle2,
                      color,
                      arc_width, capstyle, length, space, origin_x, origin_y);
    return;
  }

  fprintf(fp,"[");
  d = angle1;
  while ((d + da + db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;

    fprintf(fp,"[%d %d] ",
            a1, a1+da);

    d = d + db;
  }
  /*! \note
   *  When the above condition is no more satisfied, then it is not possible
   *  to print a dash of length <B>length</B> and the following <B>space</B>.
   *  However it may be possible to print the complete dash or a shorter one.
   */

  a1 = d;

  fprintf(fp,"[%d %d] ", a1, a1 + da);


  fprintf(fp,"] %d %d %d %d %d dashedarc %% dashed\n",
          x,y, radius, arc_width, capstyle);

}

/*! O0226
 * \brief
 *  \par Function Description
 *  This function prints an arc when a dotted line type is required.
 *  The arc is defined by its center in <B>x</B> and <B>y</B>, its
 *  radius in <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameter <B>length</B> is ignored whereas <B>arc_width</B> specifies
 *  the diameter of the dots of the printed line and <B>space</B> the distance
 *  between two dots.
 *
 *  A negative value for <B>space</B> leads to an endless loop.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 *  The function sets the color the line will be printed with.
 *
 *  \param [in] toplevel  The GedaToplevel object.
 *  \param [in] fp         FILE pointer to postscript document.
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] angle1
 *  \param [in] angle2
 *  \param [in] color
 *  \param [in] arc_width
 *  \param [in] capstyle
 *  \param [in] length
 *  \param [in] space
 *  \param [in] origin_x
 *  \param [in] origin_y
 */
void geda_arc_object_print_dotted(GedaToplevel *toplevel, FILE *fp,
                                  int x, int y, int radius,
                                  int angle1, int angle2,
                                  int color,
                                  int arc_width,
                                  int capstyle, int length, int space,
                                  int origin_x, int origin_y)
{
  int da, d;

  f_print_set_color(toplevel, fp, color);

  /*! \note
   *  Depending on the radius of the arc, the <B>space</B> parameter is
   *  changed into a small angle <B>da</B>.
   *  Starting from <B>angle1</B> - the start angle - the dots are printed
   *  along the arc by increments of this new angle.
   *
   *  As <B>da</B> is rounded as an integer, it can take a null value which
   *  will make the function enter an endless loop. In such a case, the arc
   *  is printed solid. The <B>da</B> variable should never be negative
   *  except if <B>space</B> is negative.
   */

  /* Inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }
  da = (int) ((space * 180) / (M_PI * ((double) radius)));

  /* If da is too small to display arc as dotted, draw a solid arc */
  if (da <= 0) {
    geda_arc_object_print_solid(toplevel, fp,
                      x, y, radius,
                      angle1, angle2,
                      color,
                      arc_width, capstyle, length, space, origin_x, origin_y);
    return;
  }

  fprintf(fp,"[");
  d = angle1;
  while (d < (angle2 + angle1)) {
    /*  xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
     *  ya = ((double) y) + ((double) radius) * sin(d * M_PI / 180);
     */
    fprintf(fp,"[%d] ",d);

    d = d + da;
  }
  fprintf(fp,"] %d %d %d %d %d dashedarc %% dotted\n",
          x,y, radius, arc_width, capstyle);
}

/*! \note
 *  A dot is represented by a filled circle. Position of the circle is (<B>xa</B>,
 *  <B>ya</B>) and its radius is the <B>arc_width</B> parameter.
 */

/*! O0227
 * \brief
 * \par Function Description
 *  This function prints an arc when a phantom line type is required.
 *  The arc is defined by its center in <B>x</B> and <B>y</B>, its radius
 *  in <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameter <B>arc_width</B> specifies the diameter of the dots and
 *  the width of the dashes of the printed line.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an endless loop.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 * The function sets the color in which the line will be printed with.
 *
 * \param [in] toplevel  The GedaToplevel object.
 * \param [in] fp        FILE pointer to postscript document.
 * \param [in] x
 * \param [in] y
 * \param [in] radius
 * \param [in] angle1
 * \param [in] angle2
 * \param [in] color
 * \param [in] arc_width
 * \param [in] capstyle
 * \param [in] length
 * \param [in] space
 * \param [in] origin_x
 * \param [in] origin_y
 */
void geda_arc_object_print_phantom(GedaToplevel *toplevel, FILE *fp,
                                   int x, int y, int radius,
                                   int angle1, int angle2,
                                   int color,
                                   int arc_width,
                                   int capstyle, int length, int space,
                                   int origin_x, int origin_y)
{
  int da, db, a1, d;

  f_print_set_color(toplevel, fp, color);

  /*! \note
   *  Depending on the radius of the arc, the <B>space</B> (resp. <B>length</B>)
   *  parameter is changed into a small angle <B>da</B> (resp. <B>db</B>).
   *  Starting from <B>angle1</B> - the start angle - the dashes are printed
   *  along the arc by increments of these new angles.
   *
   *  As <B>da</B> (resp. <B>db</B>) is rounded as an integer, it can take a null
   *  value which will make the function enter an endless loop. In such a case,
   *  the arc is printed solid. The <B>da</B> (resp. <B>db</B>) variable should
   *  never be negative except if <B>space</B> (resp. <B>length</B>) is negative.
   *
   *  Prints as many sets of dash-dot-dot as possible.
   */

  /* Inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }

  da = (int) ((length * 180) / (((double) radius) * M_PI));
  db = (int) ((space  * 180) / (((double) radius) * M_PI));

  /* If either da or db are too small for arc to be displayed as dotted,
   * then draw a solid arc */
  if ((da <= 0) || (db <= 0)) {
    geda_arc_object_print_solid(toplevel, fp,
                      x, y, radius,
                      angle1, angle2,
                      color,
                      arc_width, capstyle, length, space, origin_x, origin_y);
    return;
  }

  fprintf(fp,"[");

  d = angle1;
  while ((d + da + 3 * db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;

    fprintf(fp,"[%d %d] ",(int) a1, (int) a1 + da);

    d = d + db;
    /*     xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
     *     ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
     */
    fprintf(fp,"[%d] ",d);

    d = d + db;

    /*     xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
     *     ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
     */
    fprintf(fp,"[%d] ",d);

    d = d + db;
  }

  /*! \note
   *  When the above condition is no longer satisfied, then it is not
   *  possible to print a dash of length <B>length</B>.
   *  However three cases are possible :
   *  <DL>
   *    <DT>*</DT><DD>it is possible to print a dash and a dot and a dot
   *    <DT>*</DT><DD>it is possible to print a dash and a dot
   *    <DT>*</DT><DD>it is possible to print the dash or a part of the original dash
   *  </DL>
   */

  a1 = d;
  d  = d + da;

  fprintf(fp,"[%d %d] ",(int) a1, (int) a1 + da);

  if ((d + db) < (angle1 + angle2)) {
    d = d + db;

    /*
     *     xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
     *     ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
     */
    fprintf(fp,"[%d] ",d);

  }

  if ((d + db) < (angle1 + angle2)) {
    d = d + db;

    /*
     *     xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
     *     ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));
     */

    fprintf(fp,"[%d] ",d);

  }

  fprintf(fp,"] %d %d %d %d %d dashedarc %% phantom\n",
          x,y, radius, arc_width, capstyle);
}

/*! O0228
 * \brief Print a Solid Arc
 * \par Function Description
 *  This function prints an arc when a solid line type is required.
 *  The arc is defined by its center in <B>x</B> and <B>y</B>, its radius
 *  in <B>radius</B> and the start and end angles of the arc on the circle.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  The parameters <B>length</B> and <B>space</B> are ignored
 *  whereas <B>arc_width</B> specifies the width of the printed line.
 *
 *  All dimensions are in mils, except <B>angle1</B> and <B>angle2</B> in degrees.
 *
 * \param [in] toplevel  The GedaToplevel object.
 * \param [in] fp         FILE pointer to postscript document.
 * \param [in] x
 * \param [in] y
 * \param [in] radius
 * \param [in] angle1
 * \param [in] angle2
 * \param [in] color
 * \param [in] arc_width
 * \param [in] capstyle
 * \param [in] length
 * \param [in] space
 * \param [in] origin_x
 * \param [in] origin_y
 */
void geda_arc_object_print_solid(GedaToplevel *toplevel, FILE *fp,
                                 int x, int y, int radius,
                                 int angle1, int angle2,
                                 int color,
                                 int arc_width,
                                 int capstyle, int length, int space,
                                 int origin_x, int origin_y)
{
  f_print_set_color(toplevel, fp, color);

  /* inverting angle2 if < 0 and changing angle1 accordingly */
  if (angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }

  fprintf(fp, "%d %d %d %d %d %d %d darc\n",
          x,y, radius, angle1, angle1 + angle2, arc_width, capstyle);

}

/*! O0229
 * \brief
 * \par Function Description
 *  This function reads a formatted text buffer describing an arc
 *  in the gEDA file format and initializes the corresponding object.
 *
 *  Depending on the version of the file format the data extraction is
 *  performed differently : currently pre-20000704 and 20000704 on one
 *  hand and post-20000704 file format version on the other hand are supported.
 *  The version is specified in string pointed by <B>fileformat_ver</B>.
 *
 *  To get information on the various file formats have a
 *  look to the fileformats.html document.
 *
 *  The object is initialized with the functions #geda_set_object_line_options() and
 *  #geda_set_object_fill_options(). The second one is only used to put initialize
 *  unused values for an arc as an arc can not be filled.
 *
 *  The arc is allocated initialized with the function #geda_arc_object_new().
 *
 *  A negative or null radius is not allowed.
 *
 * \param [in] buf
 * \param [in] release_ver
 * \param [in] fileformat_ver
 *
 * \param [out] err           A GError object
 *
 * \return The ARC GedaObject that was created, or NULL on error.
 */
GedaObject *geda_arc_object_read (const char buf[], unsigned int release_ver,
                                  unsigned int fileformat_ver,
                                  GError **err)
{
  GedaObject *new_obj;
  char type;
  int  x1, y1;
  int  radius;
  int  start_angle, arc_sweep;
  int  color;
  int  arc_width, arc_length, arc_space;
  int  arc_type;
  int  arc_end;

  int  fill_width, angle1, pitch1, angle2, pitch2;
  int  arc_fill;

  /*! \note
   *  Depending on the version of the file format used to describe this arc,
   *  the buffer is parsed differently. The unknown parameters of the less
   *  restrictive - the oldest - file format are set to common values
   */
  if (fileformat_ver >= 3) {

    if (sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
               &type, &x1, &y1, &radius, &start_angle, &arc_sweep, &color,
               &arc_width, &arc_end, &arc_type, &arc_length, &arc_space,
               &arc_fill, &fill_width, &angle1, &pitch1, &angle2, &pitch2) != 18)
    {
      g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse arc object"));
      return NULL;
    }
  }
  else {

    /* set fill options to defaults for older file formats */
    arc_fill   = FILLING_HOLLOW;
    fill_width = 0;
    angle1     = -1;
    pitch1     = -1;
    angle2     = -1;
    pitch2     = -1;

    if (release_ver <= VERSION_20000704) {
      if (sscanf(buf, "%c %d %d %d %d %d %d", &type,
        &x1, &y1, &radius, &start_angle, &arc_sweep, &color) != 7) {
        g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse arc object"));
        return NULL;
      }

        arc_width = 0;
        arc_end   = END_NONE;
        arc_type  = TYPE_SOLID;
        arc_space = -1;
        arc_length= -1;
    }
    else {
      if (sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d", &type,
        &x1, &y1, &radius, &start_angle, &arc_sweep, &color,
        &arc_width, &arc_end, &arc_type, &arc_length, &arc_space) != 12)
      {
        g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse arc object"));
        return NULL;
      }
    }
  }

  /* Error check */
  if (radius < 0) {
    const char *msg = _("Found arc with negative radius");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, radius);
    }
    radius = abs(radius);
    geda_log_w ("%s %d\n", _("Setting radius to"), radius);
  }
  else if (radius == 0) {
    const char *msg = _("Found an arc with radius zero");
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
    color = DEFAULT_ARC_COLOR_INDEX;
  }

  /* Allocation and initialization */
  new_obj = geda_arc_object_new(color, x1, y1, radius, start_angle, arc_sweep);

  /* set the line options */
  new_obj->line_options->line_end     = arc_end;
  new_obj->line_options->line_type    = arc_type;
  new_obj->line_options->line_width   = arc_width;
  new_obj->line_options->line_space   = arc_space;
  new_obj->line_options->line_length  = arc_length;

  /* set the fill options */
  new_obj->fill_options->fill_type   = arc_fill;
  new_obj->fill_options->fill_width  = fill_width;
  new_obj->fill_options->fill_angle1 = angle1;
  new_obj->fill_options->fill_angle2 = angle2;
  new_obj->fill_options->fill_pitch1 = pitch1;
  new_obj->fill_options->fill_pitch2 = pitch2;

  return new_obj;
}

/*! O0230
 * \brief
 * \par Function Description
 *  This function rotates the world coordinates of an arc of an angle
 *  specified by <B>angle</B>. The center of the rotation is given by
 *  (<B>center_x</B>,<B>center_y</B>).
 *
 *  The arc is translated in order to put the center of the rotation
 *  on the origin. The center of the arc is then rotated of the angle
 *  specified by <B>angle</B>. The start angle of the arc is incremented by <B>angle</B>.
 *
 *  The arc is finally back translated to its previous location on the page.
 *
 *  <B>center_x</B> and <B>center_y</B> are in world units, <B>angle</B> is in degrees.
 *
 * \param [in] object
 * \param [in] center_x
 * \param [in] center_y
 * \param [in] angle
 */
void geda_arc_object_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  if (GEDA_IS_ARC(object)) {

    int x, y, newx, newy;

    /* translate object to origin */
    object->arc->x -= center_x;
    object->arc->y -= center_y;

    /* get center, and rotate center */
    x = object->arc->x;
    y = object->arc->y;

    if (angle % 90 == 0) {
      geda_math_rotate_point_90(x, y, angle % 360, &newx, &newy);
    }
    else {
      geda_math_rotate_point(x, y, angle % 360, &newx, &newy);
    }
    object->arc->x = newx;
    object->arc->y = newy;

    /* apply rotation to angles */
    object->arc->start_angle = (object->arc->start_angle + angle) % 360;

    /* arc_sweep is unchanged as it is the sweep of the arc */

    /* translate object to its previous place */
    object->arc->x += center_x;
    object->arc->y += center_y;

    /* update the screen coords and the bounding box */
    object->bounds_valid = FALSE;
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0231
 * \brief Scale an Arc GedaObject
 * \par Function Description
 *  Increase or decreases the radius of the arc object by the
 *  given \a scale factor.
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] scale   Scale factor.
 *
 * \sa geda_arc_set_arc_arc_sweep
 */
void geda_arc_object_scale (GedaObject *object, int scale)
{
  if (GEDA_IS_ARC(object)) {

    object->arc->radius = object->arc->radius * scale;

    object->bounds_valid = FALSE;
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0232
 * \brief Set sweep angle of an Arc GedaObject
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] sweep   New value for the arc sweep
 *
 * \sa geda_arc_set_arc_arc_sweep
 */
void geda_arc_object_set_arc_sweep (GedaObject *object, int sweep)
{
  if (GEDA_IS_ARC(object)) {
    if (object->arc->arc_sweep != sweep) {
      object->arc->arc_sweep = sweep;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0233
 * \brief Set center X coordinate of an Arc GedaObject
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] x       New value for the arc X coordinate
 *
 * \sa geda_arc_set_arc_center_x
 */
void
geda_arc_object_set_center_x (GedaObject *object, int x)
{
  if (GEDA_IS_ARC(object)) {
    if (object->arc->x != x) {
      object->arc->x = x;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0234
 * \brief Set center Y coordinate of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] y       New value for the arc Y coordinate
 *
 * \sa geda_arc_set_arc_center_y
 */
void geda_arc_object_set_center_y (GedaObject *object, int y)
{
  if (GEDA_IS_ARC(object)) {
    if (object->arc->y != y) {
      object->arc->y = y;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0235
 * \brief Set Line End Cap Type of an Arc Object
 * \par Function Description
 *
 * \param [in] object   Pointer to an Arc GedaObject
 * \param [in] line_end New value for the arc line end type
 *
 * \sa geda_arc_set_end_cap geda_arc_object_get_end_cap
 */
void geda_arc_object_set_end_cap (GedaObject *object, int line_end)
{
  if (GEDA_IS_ARC(object)) {
    if (object->line_options->line_end != line_end) {
      object->line_options->line_end = line_end < END_NONE ? END_NONE :
                                       line_end > END_VOID ? END_VOID :
                                       line_end;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0236
 * \brief Set Fill Angle 1 property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] angle   New value for the arc fill angle 1
 *
 * \sa geda_arc_set_fill_angle1 geda_arc_object_get_fill_angle1
 */
void geda_arc_object_set_fill_angle1 (GedaObject *object, int angle)
{
  if (GEDA_IS_ARC(object)) {
    if (object->fill_options->fill_angle1 != angle) {
      object->fill_options->fill_angle1 = angle;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0237
 * \brief Set Fill Angle 2 property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] angle   New value for the arc fill angle 2
 *
 * \sa geda_arc_set_fill_angle2 geda_arc_object_get_fill_angle2
 */
void geda_arc_object_set_fill_angle2 (GedaObject *object, int angle)
{
  if (GEDA_IS_ARC(object)) {
    if (object->fill_options->fill_angle2 != angle) {
      object->fill_options->fill_angle2 = angle;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0238
 * \brief Set Fill Pitch 1 property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] pitch   New value for the arc fill pitch 1
 *
 * \sa geda_arc_set_fill_pitch1 geda_arc_object_get_fill_pitch2
 */
void geda_arc_object_set_fill_pitch1 (GedaObject *object, int pitch)
{
  if (GEDA_IS_ARC(object)) {
    if (object->fill_options->fill_pitch1 != pitch) {
      object->fill_options->fill_pitch1 = pitch < 0 ? 0 : pitch;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0239
 * \brief Set Fill Pitch 2 property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] pitch   New value for the arc fill pitch 2
 *
 * \sa geda_arc_set_fill_pitch2 geda_arc_object_get_fill_pitch2
 */
void geda_arc_object_set_fill_pitch2 (GedaObject *object, int pitch)
{
  if (GEDA_IS_ARC(object)) {
    if (object->fill_options->fill_pitch2 != pitch) {
      object->fill_options->fill_pitch2 = pitch < 0 ? 0 : pitch;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0240
 * \brief Set Fill Type property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] type    New value for the arc fill type
 *
 * \sa geda_arc_set_fill_type geda_arc_object_get_fill_type
 */
void geda_arc_object_set_fill_type (GedaObject *object, int type)
{
  if (GEDA_IS_ARC(object)) {
    if (object->fill_options->fill_type != type) {
      object->fill_options->fill_type = type < TYPE_SOLID ? TYPE_SOLID :
                                        type > TYPE_ERASE ? TYPE_ERASE :
                                        type;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0241
 * \brief Set Fill Width property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] width   New value for the arc fill width
 *
 * \sa geda_arc_set_fill_width geda_arc_object_get_fill_width
 */
void geda_arc_object_set_fill_width (GedaObject *object, int width)
{
  if (GEDA_IS_ARC(object)) {
    if (object->fill_options->fill_width != width) {
      object->fill_options->fill_width = width < 0 ? 0 : width;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0242
 * \brief Set Line Length property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] length  New value for the arc line length
 *
 * \sa geda_arc_set_line_length geda_arc_object_get_line_length
 */
void geda_arc_object_set_line_length (GedaObject *object, int length)
{
  if (GEDA_IS_ARC(object)) {
    if (object->line_options->line_length != length) {
      object->line_options->line_length = length > 0 ? length : 0;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0243
 * \brief Set Line Space property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] space   New value for the arc line space
 *
 * \sa geda_arc_set_line_space geda_arc_object_get_line_space
 */
void geda_arc_object_set_line_space (GedaObject *object, int space)
{
  if (GEDA_IS_ARC(object)) {
    if (object->line_options->line_space != space) {
      object->line_options->line_space =  space > 0 ? space : 0;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0244
 * \brief Set Line Type property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] type    New value for the arc line type
 *
 * \sa geda_arc_set_line_type geda_arc_object_get_line_type
 */
void geda_arc_object_set_line_type (GedaObject *object, int type)
{
  if (GEDA_IS_ARC(object)) {
    if (object->line_options->line_type != type) {
      object->line_options->line_type = type < TYPE_SOLID ? TYPE_SOLID :
                                        type > TYPE_ERASE ? TYPE_ERASE :
                                        type;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0245
 * \brief Set Line Width property of an Arc Object
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] width   New value for the arc line width
 *
 * \sa geda_arc_set_line_length geda_arc_object_get_line_width
 */
void geda_arc_object_set_line_width (GedaObject *object, int width)
{
  if (GEDA_IS_ARC(object)) {
    if (object->line_options->line_width != width) {
      object->line_options->line_width =  width > 0 ? width : 0;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0246
 * \brief Set radius of an Arc GedaObject
 * \par Function Description
 *
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] radius  New value for the arc radius
 *
 * \sa geda_arc_set_arc_start_angle
 */
void geda_arc_object_set_radius (GedaObject *object, int radius)
{
  if (GEDA_IS_ARC(object)) {
    if (object->arc->radius != radius) {
      object->arc->radius = radius;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0247
 * \brief Set the starting angle of an Arc GedaObject
 * \par Function Description
 * \param [in] object  Pointer to an Arc GedaObject
 * \param [in] angle   New value for the arc starting angle
 *
 * \sa geda_arc_set_arc_start_angle
 */
void geda_arc_object_set_start_angle (GedaObject *object, int angle)
{
  if (GEDA_IS_ARC(object)) {
    if (object->arc->start_angle != angle) {
      object->arc->start_angle = angle;
      object->bounds_valid = FALSE;
    }
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0248
 * \brief Determine Shortest Distance to an Arc
 * \par Function Description
 *  Calculates the distance between the given point and the closest
 *  point on the perimeter of the arc.
 *
 * \param [in] object       An GedaObject object of type GedaArc
 * \param [in] x            The x coordinate of the given point
 * \param [in] y            The y coordinate of the given point
 * \param [in] force_solid  If true, force treating the object as solid.
 *
 * \return The shortest distance from the object to the point. With an
 *         invalid parameter, this function returns G_MAXDOUBLE.
 */
double geda_arc_object_shortest_distance (ConstObject *object, int x, int y, int force_solid)
{
  if (GEDA_IS_ARC(object)) {

    int solid;

    solid = force_solid || object->fill_options->fill_type != FILLING_HOLLOW;

    return geda_math_arc_shortest_distance (object->arc, x, y, solid);
  }

  geda_arc_object_error(__func__, object);

  return (G_MAXDOUBLE);
}

/*! O0249
 * \brief Create String Representation of an Arc object
 * \par Function Description
 *  This function formats a string in the <B>buffer</B> to describe
 *  the #GedaArc <B>\a object</B>. A pointer to the new allocated
 *  formated string is returned.
 *
 * \note object was validated by geda_object_save_objects
 *
 * \param [in] object
 *
 * \return the string representation of the arc object
 *
 * \remarks The string should be freed at some point.
 */
char *geda_arc_object_to_buffer(GedaObject *object)
{
  int x, y, radius, start_angle, arc_sweep;
  int arc_width, arc_length, arc_space;
  int fill_width, angle1, pitch1, angle2, pitch2;
  char *buf;

  LINE_END arc_end;
  LINE_TYPE arc_type;
  OBJECT_FILLING arc_fill;

  /* radius, center and angles of the arc */
  radius      = object->arc->radius;
  x           = object->arc->x;
  y           = object->arc->y;
  start_angle = object->arc->start_angle;
  arc_sweep   = object->arc->arc_sweep;

  /* line type parameters */
  arc_width  = object->line_options->line_width;
  arc_end    = object->line_options->line_end;
  arc_type   = object->line_options->line_type;
  arc_length = object->line_options->line_length;
  arc_space  = object->line_options->line_space;

  /* filling parameters */
  arc_fill   = object->arc->fill_options.fill_type;
  fill_width = object->arc->fill_options.fill_width;
  angle1     = object->arc->fill_options.fill_angle1;
  pitch1     = object->arc->fill_options.fill_pitch1;
  angle2     = object->arc->fill_options.fill_angle2;
  pitch2     = object->arc->fill_options.fill_pitch2;

  /* Describe an arc with post-20190401 file format 3 */
  buf = geda_sprintf("%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                      object->type,
                      x, y, radius, start_angle, arc_sweep, object->color,
                      arc_width, arc_end, arc_type, arc_length, arc_space,
                      arc_fill, fill_width, angle1, pitch1, angle2, pitch2);

  return(buf);
}

/*! O0250
 * \brief Apply Translation to an Arc Object
 * \par Function Description
 *  This function applies a translation of (<B>dx</B>,<B>dy</B>)
 *  to the arc described in <B>*object</B>. <B>dx</B> and <B>dy</B> are in world unit.
 *
 * \param [in] object
 * \param [in] dx
 * \param [in] dy
 */
void geda_arc_object_translate(GedaObject *object, int dx, int dy)
{
  if (GEDA_IS_ARC(object)) {

    object->arc->x = object->arc->x + dx;
    object->arc->y = object->arc->y + dy;

    /* Set flag bounds invalid */
    object->bounds_valid = FALSE;
  }
  else {
    geda_arc_object_error(__func__, object);
  }
}

/*! O0251
 * \brief Determines if a point lies within the sweep of the arc.
 * \par Function Description
 *
 * \param [in] object GedaObject object of type GedaArc
 * \param [in] x      The x coordinate of the given point
 * \param [in] y      The y coordinate of the given point
 *
 * \return TRUE if the point lies within the sweep of the arc.
 *         FALSE if the point lies outside the sweep of the arc.
 *         With an invalid parameter, this function returns FALSE.
 */
bool geda_arc_object_within_sweep(GedaObject *object, int x, int y)
{
  if (GEDA_IS_ARC(object)) {
    return geda_arc_within_sweep(object->arc, x, y);
  }
  else {
    geda_arc_object_error(__func__, object);
  }
  return FALSE;
}

/** @} endgroup arc-object-proc */
