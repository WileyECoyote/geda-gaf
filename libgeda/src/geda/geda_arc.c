/* -*- geda_arc.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2016 Ales Hvezda
 * Copyright (C) 2013-2016 Wiley Edward Hill
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 18, 2013
 */

/*! \file geda_arc.c
 *  \brief Geda Arc Object Class Module
 */

/** \defgroup geda-arc-object Geda Arc Object
 * @{
 * \brief Implmentation of #GedaArc Class
 * \par
 *  A Geda Arc Object is a graphical object, do not involve electrical
 *  interconnections. GedaArcs have line-type and fill-type properties
 *  and are derived from the GedaObject base class.
 *
 * \class GedaArc geda_arc.h "include/libgeda/geda_arc.h"
 * \implements geda-object
 */

#include <config.h>

#include <libgeda_priv.h>
#include <math.h>

enum {
  PROP_0,
  PROP_CENTER_X,
  PROP_CENTER_Y,
  PROP_RADIUS,
  PROP_START_ANGLE,
  PROP_ARC_SWEEP,
  PROP_END_CAP,
  PROP_TYPE,
  PROP_WIDTH,
  PROP_SPACE,
  PROP_LENGTH
};

static GObjectClass *geda_arc_parent_class = NULL;

/*! \brief Geda Arc Bounds
 *  \par Function Description
 *  This function calculates the smallest rectangle the arc can be drawn
 *  into. The <B>Object</B> pointed by \a object is assumed to be an arc.
 *  The <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B> pointed
 *  integers define this rectangle at the end of the function. The arc is
 *  expressed in world units. The process is divided into two steps : the
 *  first step is to calculate the coordinates of the two ends of the arc
 *  and the coordinates of the center. They forms a first rectangle but
 * (depending on the start angle and the sweep of the arc) not the right.
 *
 *  \param [in]  object
 */
int
geda_arc_bounds(GedaObject *object)
{
  int x1, y1, x2, y2, x3, y3;
  int left, top, right, bottom;
  int radius, start_angle, arc_sweep;
  int i, angle;
  int halfwidth;

  g_return_val_if_fail (GEDA_IS_ARC(object), FALSE);

  halfwidth = object->line_options->line_width / 2;

  radius      = object->arc->radius;
  start_angle = object->arc->start_angle;
  arc_sweep   = object->arc->arc_sweep;

  x1 = object->arc->x;
  y1 = object->arc->y;
  x2 = x1 + radius * cos(start_angle * M_PI / 180);
  y2 = y1 + radius * sin(start_angle * M_PI / 180);
  x3 = x1 + radius * cos((start_angle + arc_sweep) * M_PI / 180);
  y3 = y1 + radius * sin((start_angle + arc_sweep) * M_PI / 180);

  left   = (x1 < x2) ? ((x1 < x3) ? x1 : x3) : ((x2 < x3) ? x2 : x3);
  right  = (x1 > x2) ? ((x1 > x3) ? x1 : x3) : ((x2 > x3) ? x2 : x3);
  bottom = (y1 > y2) ? ((y1 > y3) ? y1 : y3) : ((y2 > y3) ? y2 : y3);
  top    = (y1 < y2) ? ((y1 < y3) ? y1 : y3) : ((y2 < y3) ? y2 : y3);

  /*! \note
   *  The previous rectangle is extended to the final one by checking
   *  whether the arc is over a main axis (vertical or horizontal).
   *  If so, the rectangle is extended in these directions.
   *
   *  In the mirror mode, the sweep angle is negativ. To get a
   *  CCW arc before this calculation we have to move the
   *  start angle to the end angle and reverse the sweep angle.
   */
  if (arc_sweep < 0) {
    start_angle = (start_angle + arc_sweep + 360) % 360;
    arc_sweep   = -arc_sweep;
  }

  angle = ((int) (start_angle / 90)) * 90;

  for(i = 0; i < 4; i++) {
    angle = angle + 90;
    if(angle < start_angle + arc_sweep) {
      if(angle % 360 == 0)   right  = x1 + radius;
      if(angle % 360 == 90)  bottom = y1 + radius;
      if(angle % 360 == 180) left   = x1 - radius;
      if(angle % 360 == 270) top    = y1 - radius;
    }
    else {
      break;
    }
  }

  /* This isn't strictly correct, but a 1st order approximation */
  object->left   = left   - halfwidth;
  object->top    = top    - halfwidth;
  object->right  = right  + halfwidth;
  object->bottom = bottom + halfwidth;

  return TRUE;
}

/*! \brief Type instance initializer for Arc
 *
 *  \par Function Description
 *  Type instance initializer for Arc, initializes a new empty
 *  Arc object by setting pointers to NULL and numbers to zero,
 *
 *  \param [in] instance The Arc structure being initialized,
 *  \param [in] class    The Arc class we are initializing.
 */
static void geda_arc_instance_init(GTypeInstance *instance, void *class)
{
  GedaArc    *arc    = (GedaArc*)instance;
  GedaObject *object = &arc->parent_instance;

  arc->x            = 0;
  arc->y            = 0;

  arc->radius       = 0;
  arc->start_angle  = 0;
  arc->arc_sweep    = 0;

  arc->fill_options.fill_type     = default_fill_type;
  arc->fill_options.fill_width    = default_fill_width;
  arc->fill_options.fill_angle1   = default_fill_angle1;
  arc->fill_options.fill_angle2   = default_fill_angle2;
  arc->fill_options.fill_pitch1   = default_fill_pitch1;
  arc->fill_options.fill_pitch2   = default_fill_pitch2;

  arc->line_options.line_end      = default_line_end;
  arc->line_options.line_type     = default_line_type;
  arc->line_options.line_width    = default_line_width;
  arc->line_options.line_space    = default_line_space;
  arc->line_options.line_length   = default_line_length;

  object->arc                     = arc;
  object->fill_options            = &arc->fill_options;
  object->line_options            = &arc->line_options;
}

static void
geda_arc_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_arc_parent_class)->dispose(object);
}

/*! \brief Geda Arc Object Finalization Function
 *  \par Function Description
 *   This function invalidates the Arc's markers and then chains up to
 *   the parent's finalize handler. Once invalidated, GEDA_IS_ARC will
 *   fail.
 */
static void geda_arc_finalize(GObject *object)
{
  GedaObject *obj = GEDA_OBJECT(object);

  /* The object is no longer a GedaArc */
  obj->arc    = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_arc_parent_class)->finalize(object);
}

static void
get_property (GObject *object, unsigned int  prop_id,
                               GValue       *value,
                               GParamSpec   *pspec)

{
  GedaArc      *arc          = GEDA_ARC(object);
  LINE_OPTIONS *line_options = &arc->line_options;

  switch (prop_id)
  {
    case PROP_CENTER_X:
      g_value_set_int (value, arc->x);
      break;

    case PROP_CENTER_Y:
      g_value_set_int (value, arc->y);
      break;

    case PROP_RADIUS:
      g_value_set_int (value, arc->radius);
      break;

    case PROP_START_ANGLE:
      g_value_set_int (value, arc->start_angle);
      break;

    case PROP_ARC_SWEEP:
      g_value_set_int (value, arc->arc_sweep);
      break;

    case PROP_END_CAP:
      g_value_set_int (value, line_options->line_end);
      break;

    case PROP_TYPE:
      g_value_set_int (value, line_options->line_type);
      break;

    case PROP_WIDTH:
      g_value_set_int (value, line_options->line_width);
      break;

    case PROP_SPACE:
      g_value_set_int (value, line_options->line_space);
      break;

    case PROP_LENGTH:
      g_value_set_int (value, line_options->line_length);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
set_property (GObject *object, unsigned int  prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
  GedaArc      *arc          = GEDA_ARC(object);
  LINE_OPTIONS *line_options = &arc->line_options;

  switch (prop_id)
  {
    case PROP_CENTER_X:
      arc->x = g_value_get_int (value);
      break;

    case PROP_CENTER_Y:
      arc->y = g_value_get_int (value);
      break;

    case PROP_RADIUS:
      arc->radius = g_value_get_int (value);
      break;

    case PROP_START_ANGLE:
      arc->start_angle = g_value_get_int (value);
      break;

    case PROP_ARC_SWEEP:
      arc->arc_sweep = g_value_get_int (value);
      break;

    case PROP_END_CAP:
      line_options->line_end = g_value_get_int (value);
      break;

    case PROP_TYPE:
      line_options->line_type = g_value_get_int (value);
      break;

    case PROP_WIDTH:
      line_options->line_width = g_value_get_int (value);
      break;

    case PROP_SPACE:
      line_options->line_space = g_value_get_int (value);
      break;

    case PROP_LENGTH:
      line_options->line_length = g_value_get_int (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/*! \brief Type class initializer for Arc
 *
 *  \par Function Description
 *  Type class initializer for Arc. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class      The Arc class we are initialising
 *  \param [in]  class_data   The Arc structure associated with the class
 */
static void geda_arc_class_init(void *g_class, void *class_data)
{
  GedaArcClass    *class        = (GedaArcClass*)g_class;
  GObjectClass    *object_class = G_OBJECT_CLASS(class);
  GedaObjectClass *geda_class   = GEDA_OBJECT_CLASS(class);
  GParamSpec      *params;

  geda_arc_parent_class         = g_type_class_peek_parent(class);

  object_class->dispose         = geda_arc_dispose;
  object_class->finalize        = geda_arc_finalize;

  object_class->get_property    = get_property;
  object_class->set_property    = set_property;

  geda_class->bounds            = geda_arc_bounds;

  params = g_param_spec_int ("center-x",
                           _("Center X"),
                           _("Abscissa of the arc center point"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_CENTER_X, params);

  params = g_param_spec_int ("center-y",
                           _("Center Y"),
                           _("Ordinate of the arc center point"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_CENTER_Y, params);

  params = g_param_spec_int ("radius",
                           _("Radius"),
                           _("Radius of the arc center"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_RADIUS, params);

  params = g_param_spec_int ("start-angle",
                           _("Start Angle"),
                           _("The arc starting angle"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_START_ANGLE, params);

  params = g_param_spec_int ("arc-sweep",
                           _("Arc Sweep"),
                           _("The arc sweep angle"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_ARC_SWEEP, params);

  params = g_param_spec_int ("end-cap",
                           _("End Cap"),
                           _("Line end cap"),
                             END_NONE,
                             END_ROUND,
                             END_NONE,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_END_CAP, params);

  params = g_param_spec_int ("line-type",
                           _("Line Type"),
                           _("The line type"),
                             TYPE_SOLID,
                             TYPE_PHANTOM,
                             TYPE_SOLID,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_TYPE, params);

  params = g_param_spec_int ("line-width",
                           _("Line Width"),
                           _("The line width"),
                             0,
                             500,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_WIDTH, params);

  params = g_param_spec_int ("line-space",
                           _("Line Space"),
                           _("The line space"),
                             0,
                             G_MAXINT,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_SPACE, params);

  params = g_param_spec_int ("line-length",
                           _("Line Length"),
                           _("The line length"),
                             0,
                             G_MAXINT,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_LENGTH, params);
}

/*! \brief Function to retrieve GedaArc's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaArc Type identifier. When first called,
 *  the function registers a #GedaArc in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a GedaArc and returns
 *  the unsigned integer value. The retained value is returned on
 *  all Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with GedaArc.
 */
GedaObjectType geda_arc_get_type (void)
{
  static volatile GedaObjectType geda_arc_type = 0;

  if (g_once_init_enter (&geda_arc_type)) {

    static const GTypeInfo info = {
      sizeof(GedaArcClass),
      NULL,                  /* base_init           */
      NULL,                  /* base_finalize       */
      geda_arc_class_init,   /* (GClassInitFunc)    */
      NULL,                  /* class_finalize      */
      NULL,                  /* class_data          */
      sizeof(GedaArc),
      0,                     /* n_preallocs         */
      geda_arc_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaArc");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_arc_type, type);
  }

  return geda_arc_type;
}

/*! \brief Returns a pointer to a new Arc object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Arc object.
 *
 *  \return pointer to the new Arc object.
 */
GedaObject *geda_arc_new (void)
{
  GedaObject *arc = g_object_new(GEDA_TYPE_ARC,
                                 "type", OBJ_ARC,
                                 "name", "arc",
                                 NULL );
  return GEDA_OBJECT(arc);
}

/*!
 * \brief Determine if an object is a Geda Arc Object.
 * \par Function Description
 *  Returns true if the argument is a Geda Arc object.
 *
 * \return boolean.
 */
bool is_a_geda_arc_object (GedaArc *arc)
{
  return GEDA_IS_OBJECT(arc) && (((GedaObject*)arc)->type == OBJ_ARC);
}

/*!
 * \brief Retrieve arc_sweep value from a Geda Arc Object.
 * \par Function Description
 *  Returns the current arc_sweep value of \a arc if and only if
 *  \a arc is a valid Geda Arc object.
 *
 * \return integer value of arc_sweep or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_arc_sweep
 */
int geda_arc_get_arc_sweep (GedaArc *arc) {
  if (is_a_geda_arc_object(arc)) {
    return arc->arc_sweep;
  }
  return -0;
}

/*!
 * \brief Retrieve ordinate value of the an Arc center coordinate.
 * \par Function Description
 *  Returns the current X value of \a arc if and only if \a arc is
 *  a valid Geda Arc object.
 *
 * \return integer value of center X or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_center_x
 */
int geda_arc_get_center_x (GedaArc *arc)  {
  if (is_a_geda_arc_object(arc)) {
    return arc->x;
  }
  return -0;
}

/*!
 * \brief Retrieve abscisa of an Arc center coordinate.
 * \par Function Description
 *  Returns the current Y value of \a arc if and only if \a arc is
 *  a valid Geda Arc object.
 *
 * \return integer value of center Y or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_center_y
 */
int geda_arc_get_center_y (GedaArc *arc) {
  if (is_a_geda_arc_object(arc)) {
    return arc->y;
  }
  return -0;
}

/*!
 * \brief get the position of the center point
 * \par Function Description
 *  This function gets the position of the center point of an arc object.
 *
 * \param [in]  arc  Pointer to an Arc GedaObject
 * \param [out] x    pointer to the x-position
 * \param [out] y    pointer to the y-position
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool
geda_arc_get_position (GedaArc *arc, int *x, int *y)
{
  if (is_a_geda_arc_object(arc)) {
    *x = arc->x;
    *y = arc->y;
    return TRUE;
  }
  return 0;
}

/*!
 * \brief Retrieve the radius of a Geda Arc Object
 * \par Function Description
 *  Returns the current radius value of \a arc if and only if
 * \a arc is a valid Geda Arc object.
 *
 * \return integer value of radius or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_radius
 */
int geda_arc_get_radius (GedaArc *arc) {
  if (is_a_geda_arc_object(arc)) {
    return arc->radius;
  }
  return -0;
}

/*!
 * \brief Retrieve start_angle of a Geda Arc Object
 * \par Function Description
 *  Returns the current start_angle of \a arc if and only if
 * \a arc is a valid Geda Arc object.
 *
 * \return integer value of start_angle or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_start_angle
 */
int geda_arc_get_start_angle (GedaArc *arc) {
  if (is_a_geda_arc_object(arc)) {
    return arc->start_angle;
  }
  return -0;
}

/*!
 * \brief Set the sweep of a Geda Arc Object
 * \par Function Description
 *  Sets the current arc_sweep of \a arc if \a arc is a valid Geda
 *  Arc object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_arc_sweep
 */
void geda_arc_set_arc_sweep (GedaArc *arc, int sweep) {
  if (is_a_geda_arc_object(arc)) {
    arc->arc_sweep = sweep;
  }
}

/*!
 * \brief Set the ordinate of a Geda Arc Object
 * \par Function Description
 *  Sets the X center value of \a arc if \a arc is a valid Geda
 *  Arc object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_center_x
 */
void geda_arc_set_center_x (GedaArc *arc, int x) {
  if (is_a_geda_arc_object(arc)) {
    arc->x = x;
  }
}

/*!
 * \brief Set the Y center value of a Geda Arc Object
 * \par Function Description
 *  Sets the abscisa of \a arc if \a arc is a valid Geda Arc
 *  object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_center_y
 */
void geda_arc_set_center_y (GedaArc *arc, int y) {
  if (is_a_geda_arc_object(arc)) {
    arc->y = y;
  }
}

/*!
 * \brief Set the position of a Geda Arc Object
 * \par Function Description
 *  Sets the radius of \a arc if \a arc is a valid Geda Arc
 *  object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_set_center_x geda_arc_set_center_y
 */
void geda_arc_set_position (GedaArc *arc, int x, int y) {
  if (is_a_geda_arc_object(arc)) {
    arc->x = x;
    arc->y = y;
  }
}

/*!
 * \brief Set the radius of a Geda Arc Object
 * \par Function Description
 *  Sets the radius of \a arc if \a arc is a valid Geda Arc
 *  object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_radius
 */
void geda_arc_set_radius (GedaArc *arc, int radius) {
  if (is_a_geda_arc_object(arc)) {
    arc->radius = radius;
  }
}

/*!
 * \brief Set the start_angle of a Geda Arc Object
 * \par Function Description
 *  Sets the starting angle of \a arc if \a arc is a valid Geda
 *  Arc object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_start_angle
 */
void geda_arc_set_start_angle (GedaArc *arc, int angle) {
  if (is_a_geda_arc_object(arc)) {
    arc->start_angle = angle;
  }
}

/*!
 * \brief Determines if a point lies within the sweep of the arc.
 *  The "sweep" of the arc includes all points on the rays of the arc,
 *  including those beyond the arc radius, inclusive of the insertion
 *  point and both stating and ending rays.
 *
 * \param [in] arc The arc of object
 * \param [in] x   The x coordinate of the given point
 * \param [in] y   The y coordinate of the given point
 *
 * \return TRUE if the point lies within the sweep of the arc.
 *         FALSE if the point lies outside the sweep of the arc.
 *         With an invalid parameter, this function returns FALSE.
 */
bool
geda_arc_within_sweep(GedaArc *arc, int x, int y)
{
  if (is_a_geda_arc_object(arc)) {

    double a0;
    double a1;
    double angle;
    double dx;
    double dy;

    dx = ((double) x) - ((double) arc->x);
    dy = ((double) y) - ((double) arc->y);

    /* atan2 is undefined if both dx and dy are zero, this would
     * be the insertion position of the arc, which was included
     * in our definition of sweep */
    if ((dx == 0) && (dy == 0)) {
      return TRUE;
    }

    angle = 180.0 * atan2(dy, dx) / M_PI;

    if (arc->arc_sweep > 0) {
      a0 = arc->start_angle;
      a1 = arc->start_angle + arc->arc_sweep;
    }
    else {
      a0 = arc->start_angle + arc->arc_sweep + 360.0;
      a1 = arc->start_angle + 360.0;
    }

    while (angle < a0) {
      angle += 360.0;
    }

    return (angle <= a1);
  }
  return FALSE;
}

/** @} endgroup geda-arc-object */
