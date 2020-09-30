/* -*- geda_arc.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2016 Wiley Edward Hill
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 18, 2013
 */

/*! \file geda_arc.c
 *  \brief GedaArc Class Module
 */

/** \defgroup geda-arc-object GedaArc Object
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

#include "../../../config.h"

#include <libgeda_priv.h>
#include <math.h>

enum {
  PROP_0,
  PROP_CENTER_X,
  PROP_CENTER_Y,
  PROP_RADIUS,
  PROP_START_ANGLE,
  PROP_ARC_SWEEP,
  PROP_LINE_CAP,
  PROP_LINE_TYPE,
  PROP_LINE_WIDTH,
  PROP_LINE_SPACE,
  PROP_LINE_LENGTH,
  PROP_FILL_TYPE,
  PROP_FILL_WIDTH,
  PROP_FILL_ANGLE1,
  PROP_FILL_PITCH1,
  PROP_FILL_ANGLE2,
  PROP_FILL_PITCH2
};

static GObjectClass *geda_arc_parent_class = NULL;

/*!
 * \brief Geda Arc Bounds
 * \par Function Description
 *  This function calculates the smallest rectangle the arc can be drawn
 *  into. The <B>Object</B> pointed by \a object is assumed to be an arc.
 *  The <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B> pointed
 *  integers define this rectangle at the end of the function. The arc is
 *  expressed in world units. The process is divided into two steps : the
 *  first step is to calculate the coordinates of the two ends of the arc
 *  and the coordinates of the center. They forms a first rectangle but
 * (depending on the start angle and the sweep of the arc) not the right.
 *
 * \param [in]  object
 */
static int geda_arc_bounds(GedaObject *object)
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
  top    = (y1 < y2) ? ((y1 < y3) ? y1 : y3) : ((y2 < y3) ? y2 : y3);
  bottom = (y1 > y2) ? ((y1 > y3) ? y1 : y3) : ((y2 > y3) ? y2 : y3);

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

  /* This is not strictly correct, but a 1st order approximation */
  object->left   = left   - halfwidth;
  object->top    = top    - halfwidth;
  object->right  = right  + halfwidth;
  object->bottom = bottom + halfwidth;

  object->bounds_valid = TRUE;

  return TRUE;
}

/*!
 * \brief Type instance initializer for Arc
 * \par Function Description
 *  Type instance initializer for Arc, initializes a new empty
 *  Arc object by setting pointers to NULL and numbers to zero,
 *
 * \param [in] instance The Arc structure being initialized,
 * \param [in] class    The Arc class being initialized.
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

static void geda_arc_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_arc_parent_class)->dispose(object);
}

/*!
 * \brief GedaArc Finalization Function
 * \par Function Description
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

static void get_property (GObject      *object, unsigned int  prop_id,
                          GValue       *value,  GParamSpec   *pspec)

{
  GedaArc      *arc          = GEDA_ARC(object);
  LINE_OPTIONS *line_options = &arc->line_options;
  FILL_OPTIONS *fill_options = &arc->fill_options;

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

    case PROP_LINE_CAP:
      g_value_set_int (value, line_options->line_end);
      break;

    case PROP_LINE_TYPE:
      g_value_set_int (value, line_options->line_type);
      break;

    case PROP_LINE_WIDTH:
      g_value_set_int (value, line_options->line_width);
      break;

    case PROP_LINE_SPACE:
      g_value_set_int (value, line_options->line_space);
      break;

    case PROP_LINE_LENGTH:
      g_value_set_int (value, line_options->line_length);
      break;

    case PROP_FILL_TYPE:
      g_value_set_int (value, fill_options->fill_type);
      break;

    case PROP_FILL_WIDTH:
      g_value_set_int (value, fill_options->fill_width);
      break;

    case PROP_FILL_ANGLE1:
      g_value_set_int (value, fill_options->fill_angle1);
      break;

    case PROP_FILL_PITCH1:
      g_value_set_int (value, fill_options->fill_pitch1);
      break;

    case PROP_FILL_ANGLE2:
      g_value_set_int (value, fill_options->fill_angle2);
      break;

    case PROP_FILL_PITCH2:
      g_value_set_int (value, fill_options->fill_pitch2);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void set_property (GObject *object, unsigned int  prop_id,
                                           const GValue *value,
                                           GParamSpec   *pspec)
{
  GedaArc      *arc          = GEDA_ARC(object);
  LINE_OPTIONS *line_options = &arc->line_options;
  FILL_OPTIONS *fill_options = &arc->fill_options;

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

    case PROP_LINE_CAP:
      line_options->line_end = g_value_get_int (value);
      break;

    case PROP_LINE_TYPE:
      line_options->line_type = g_value_get_int (value);
      break;

    case PROP_LINE_WIDTH:
      line_options->line_width = g_value_get_int (value);
      break;

    case PROP_LINE_SPACE:
      line_options->line_space = g_value_get_int (value);
      break;

    case PROP_LINE_LENGTH:
      line_options->line_length = g_value_get_int (value);
      break;

    case PROP_FILL_TYPE:
      fill_options->fill_type = g_value_get_int (value);
      break;

    case PROP_FILL_WIDTH:
      fill_options->fill_width = g_value_get_int (value);
      break;

    case PROP_FILL_ANGLE1:
      fill_options->fill_angle1 = g_value_get_int (value);
      break;

    case PROP_FILL_PITCH1:
      fill_options->fill_pitch1 = g_value_get_int (value);
      break;

    case PROP_FILL_ANGLE2:
      fill_options->fill_angle2 = g_value_get_int (value);
      break;

    case PROP_FILL_PITCH2:
      fill_options->fill_pitch2 = g_value_get_int (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/*!
 * \brief Type class initializer for Arc
 * \par Function Description
 *  Type class initializer for Arc. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  klass       The Arc class being initializing
 * \param [in]  class_data  The Arc structure associated with the class
 */
static void geda_arc_class_init(void *klass, void *class_data)
{
  GedaArcClass    *class        = (GedaArcClass*)klass;
  GObjectClass    *object_class = (GObjectClass*)klass;
  GedaObjectClass *geda_class   = (GedaObjectClass*)klass;
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
                           _("Radius of the arc"),
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

  /* GedaArcs have line-type properties but are not derived from GedaLine
   * therefore these properties must be defined for the GedaArc... */

  params = g_param_spec_int ("end-cap",
                           _("End Cap"),
                           _("Line end cap"),
                             END_NONE,
                             END_VOID,
                             END_NONE,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_LINE_CAP, params);

  params = g_param_spec_int ("line-type",
                           _("Line Type"),
                           _("The line type"),
                             TYPE_SOLID,
                             TYPE_ERASE,
                             TYPE_SOLID,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_LINE_TYPE, params);

  params = g_param_spec_int ("line-width",
                           _("Line Width"),
                           _("The line width"),
                             0,
                             500,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_LINE_WIDTH, params);

  params = g_param_spec_int ("line-space",
                           _("Line Space"),
                           _("The line space"),
                             0,
                             G_MAXINT,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_LINE_SPACE, params);

  params = g_param_spec_int ("line-length",
                           _("Line Length"),
                           _("The line length"),
                             0,
                             G_MAXINT,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_LINE_LENGTH, params);

  /* TODO: object filling is not fully implemented for GedaArc's */

  params = g_param_spec_int ("fill-type",
                           _("Fill Type"),
                           _("The Object fill type; hatch mesh, solid, etc..."),
                             FILLING_HOLLOW,
                             FILLING_VOID,
                             FILLING_HOLLOW,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FILL_TYPE, params);

  params = g_param_spec_int ("fill-width",
                           _("Fill Width"),
                           _("The Object fill width applies to fill hatch and mesh"),
                             0,
                             500,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FILL_WIDTH, params);

  params = g_param_spec_int ("fill-angle1",
                           _("Fill Angle 1"),
                           _("The Object fill angle1 applies to fill hatch and mesh"),
                             0,
                             360, /* Does not really make sense to be more than 180 */
                             45,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FILL_ANGLE1, params);

  params = g_param_spec_int ("fill-pitch1",
                           _("Fill Pitch 1"),
                           _("The Object fill angle1 applies to fill hatch and mesh"),
                             0,
                             G_MAXINT,
                             100,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FILL_PITCH1, params);

  params = g_param_spec_int ("fill-angle2",
                           _("Fill Angle 2"),
                           _("The Object fill angle1 applies to fill mesh"),
                             0,
                             360, /* Does not really make sense to be more than 180 */
                             135,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FILL_ANGLE2, params);

  params = g_param_spec_int ("fill-pitch2",
                           _("Fill Pitch 2"),
                           _("The Object fill angle1 applies to fill mesh"),
                             0,
                             G_MAXINT,
                             100,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FILL_PITCH2, params);
}

/*!
 * \brief Function to retrieve GedaArc's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaArc Type identifier. When first called,
 *  the function registers a #GedaArc in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a GedaArc and returns
 *  the unsigned integer value. The retained value is returned on
 *  all Subsequent calls.
 *
 * \return GedaObjectType identifier associated with GedaArc.
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

/*!
 * \brief Returns a pointer to a new Arc object
 * \par Function Description
 *  Returns a pointer to a new Arc object.
 *
 * \return pointer to the new Arc object.
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
 * \par Function Description
 *  Returns true if the argument is a GedaArc object.
 *
 * \return boolean.
 */
bool is_a_geda_arc (const GedaArc *arc)
{
  return GEDA_IS_OBJECT(arc) && (((GedaObject*)arc)->type == OBJ_ARC);
}

/*!
 * \brief Retrieve arc_sweep value from a GedaArc
 * \par Function Description
 *  Returns the current arc_sweep value of \a arc if and only if
 *  \a arc is a valid GedaArc object.
 *
 * \return integer value of arc_sweep or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_arc_sweep
 */
int geda_arc_get_arc_sweep (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->arc_sweep;
  }
  return -0;
}

/*!
 * \brief Retrieve ordinate value of the an Arc center coordinate
 * \par Function Description
 *  Returns the current X value of \a arc if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of center X or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_center_x
 */
int geda_arc_get_center_x (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->x;
  }
  return -0;
}

/*!
 * \brief Retrieve abscisa of an Arc center coordinate
 * \par Function Description
 *  Returns the current Y value of \a arc if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of center Y or -0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_center_y
 */
int geda_arc_get_center_y (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->y;
  }
  return -0;
}

/*!
 * \brief Retrieve End Cap type Property of an Arc
 * \par Function Description
 *  Returns the value of \a arc end-cap type if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of end-cap type or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_end_cap
 */
int geda_arc_get_end_cap (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->line_options.line_end;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Angle 1 Property of a Box
 * \par Function Description
 *  Returns the value of \a arc fill angle 1 if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of fill angle 1 or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_fill_angle1
 */
int geda_arc_get_fill_angle1 (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->fill_options.fill_angle1;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Angle 2 Property of a Box
 * \par Function Description
 *  Returns the value of \a arc fill angle 2 if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of fill angle 2 or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_fill_angle2
 */
int geda_arc_get_fill_angle2 (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->fill_options.fill_angle2;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Pitch 1 Property of a Box
 * \par Function Description
 *  Returns the value of \a arc fill pitch 1 if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of fill pitch 1 or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_fill_pitch1
 */

int geda_arc_get_fill_pitch1 (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->fill_options.fill_pitch1;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Pitch 2 Property of a Box
 * \par Function Description
 *  Returns the value of \a arc fill pitch 2 if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of fill pitch 2 or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_fill_pitch2
 */
int geda_arc_get_fill_pitch2 (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->fill_options.fill_pitch2;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Type Property of a arc
 * \par Function Description
 *  Returns the value of \a arc fill type if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of fill type or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_fill_type
 */
int geda_arc_get_fill_type (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->fill_options.fill_type;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Width Property of a arc
 * \par Function Description
 *  Returns the value of the \a arc fill width property if and only
 *  if \a arc is a valid GedaArc object.
 *
 * \return integer value of fill width or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_fill_width
 */
int geda_arc_get_fill_width (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->fill_options.fill_width;
  }
  return -0;
}

/*!
 * \brief Retrieve Line Length Property of an Arc
 * \par Function Description
 *  Returns the value of the \a arc line length property if and only if
 *  \a arc is a valid GedaArc object. The is not the length of the arc
 *  the line-length property controls the length of line segments for
 *  line types dashed, center and phantom, to get the "length" of
 *  a line see geda_math_line_length.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \return integer value of line length or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_line_length
 */
int geda_arc_get_line_length (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->line_options.line_length;
  }
  return -0;
}

/*!
 * \brief Retrieve Line Space Property of an Arc
 * \par Function Description
 *  Returns the value of the \a arc line space property if and only if \a arc
 *  is a valid GedaArc object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \return integer value of line space or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_line_space
 */
int geda_arc_get_line_space (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->line_options.line_space;
  }
  return -0;
}

/*!
 * \brief Retrieve Line Type Property of an Arc
 * \par Function Description
 *  Returns the value of \a arc line type if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \return integer value of line type or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_line_type
 */
int geda_arc_get_line_type (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->line_options.line_type;
  }
  return -0;
}

/*!
 * \brief Retrieve End Width Property of an Arc
 * \par Function Description
 *  Returns the value of the \a arc line width property if and only if \a arc
 *  is a valid GedaArc object.
 *
 * \return integer value of line width or -0 if \a arc is invalid.
 *
 * \sa geda_arc_set_line_width
 */
int geda_arc_get_line_width (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->line_options.line_width;
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
bool geda_arc_get_position (const GedaArc *arc, int *x, int *y)
{
  if (is_a_geda_arc(arc)) {
    *x = arc->x;
    *y = arc->y;
    return TRUE;
  }
  return 0;
}

/*!
 * \brief Retrieve the radius of a GedaArc
 * \par Function Description
 *  Returns the current radius value of \a arc if and only if
 * \a arc is a valid GedaArc object.
 *
 * \return integer value of radius or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_radius
 */
int geda_arc_get_radius (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->radius;
  }
  return -0;
}

/*!
 * \brief Retrieve start_angle of a GedaArc
 * \par Function Description
 *  Returns the current start_angle of \a arc if and only if
 *  \a arc is a valid GedaArc object.
 *
 * \return integer value of start_angle or 0 if \a arc is invalid.
 *
 * \sa geda_arc_object_get_start_angle
 */
int geda_arc_get_start_angle (const GedaArc *arc)
{
  if (is_a_geda_arc(arc)) {
    return arc->start_angle;
  }
  return -0;
}

/*!
 * \brief Set the sweep of a GedaArc
 * \par Function Description
 *  Sets the current arc_sweep of \a arc if \a arc is a valid Geda
 *  Arc object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_arc_sweep
 */
void geda_arc_set_arc_sweep (GedaArc *arc, int sweep)
{
  if (is_a_geda_arc(arc)) {
    arc->arc_sweep = sweep;
  }
}

/*!
 * \brief Set the center X of a GedaArc
 * \par Function Description
 *  Sets the abscissa of \a arc center if \a arc is a valid Geda
 *  Arc object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_center_x
 */
void geda_arc_set_center_x (GedaArc *arc, int x)
{
  if (is_a_geda_arc(arc)) {
    arc->x = x;
  }
}

/*!
 * \brief Set the Y center value of a GedaArc
 * \par Function Description
 *  Sets the ordinate of \a arc if \a arc is a valid Geda Arc
 *  object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_center_y
 */
void geda_arc_set_center_y (GedaArc *arc, int y)
{
  if (is_a_geda_arc(arc)) {
    arc->y = y;
  }
}

/*!
 * \brief Set the End Cap type Property of an Arc
 * \par Function Description
 *  Sets the value of \a arc end-cap type if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \sa geda_arc_get_end_cap
 */
void geda_arc_set_end_cap (GedaArc *arc, int line_end)
{
  if (is_a_geda_arc(arc)) {
    arc->line_options.line_end = line_end < END_NONE ? END_NONE :
                                 line_end > END_VOID ? END_VOID :
                                 line_end;
  }
}

/*!
 * \brief Set the Fill Angle 1 Property of a arc
 * \par Function Description
 *  Sets the value of \a arc fill angle 1 if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \sa geda_arc_get_fill_angle1
 */
void geda_arc_set_fill_angle1 (GedaArc *arc, int angle)
{
  if (is_a_geda_arc(arc)) {
    arc->fill_options.fill_angle1 = angle;
  }
}

/*!
 * \brief Set the Fill Angle 2 Property of a arc
 * \par Function Description
 *  Sets the value of \a arc fill angle 2 if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \sa geda_arc_get_fill_angle2
 */
void geda_arc_set_fill_angle2 (GedaArc *arc, int angle)
{
  if (is_a_geda_arc(arc)) {
    arc->fill_options.fill_angle2 = angle;
  }
}

/*!
 * \brief Set the Fill Pitch 1 Property of a arc
 * \par Function Description
 *  Sets the value of \a arc fill pitch 1 if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \sa geda_arc_get_fill_pitch1
 */
void geda_arc_set_fill_pitch1 (GedaArc *arc, int pitch)
{
  if (is_a_geda_arc(arc)) {
    arc->fill_options.fill_pitch1 = pitch;
  }
}

/*!
 * \brief Set the Fill Pitch 2 Property of a arc
 * \par Function Description
 *  Sets the value of \a arc fill pitch 2 if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \sa geda_arc_get_fill_pitch2
 */
void geda_arc_set_fill_pitch2 (GedaArc *arc, int pitch)
{
  if (is_a_geda_arc(arc)) {
    arc->fill_options.fill_pitch2 = pitch;
  }
}

/*!
 * \brief Set the Fill Type Property of a arc
 * \par Function Description
 *  Sets the value of \a arc fill type if and only if \a arc is
 *  a valid GedaArc object.
 *
 * \sa geda_arc_get_fill_type
 */
void geda_arc_set_fill_type (GedaArc *arc, int type)
{
  if (is_a_geda_arc(arc)) {
    arc->fill_options.fill_type = type < TYPE_SOLID ? TYPE_SOLID :
                                  type > TYPE_ERASE ? TYPE_ERASE :
                                  type;
  }
}

/*!
 * \brief Set the Fill Width Property of a arc
 * \par Function Description
 *  Sets the value of \a arc width of the fill if and only
 *  if \a arc is a valid GedaArc object.
 *
 * \sa geda_arc_get_fill_width
 */
void geda_arc_set_fill_width (GedaArc *arc, int width)
{
  if (is_a_geda_arc(arc)) {
    arc->fill_options.fill_width = width;
  }
}

/*!
 * \brief Set the Line Length Property of an Arc
 * \par Function Description
 *  Returns the value of the \a arc line length property if and only if
 *  \a arc is a valid GedaArc object. The line-length property controls
 *  the length of line segments for line types dashed, center and phantom.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \sa geda_arc_get_line_length
 */
void geda_arc_set_line_length (GedaArc *arc, int line_length)
{
  if (is_a_geda_arc(arc)) {
    arc->line_options.line_length = line_length > 0 ? line_length : 0;
  }
}

/*!
 * \brief Set the Line Space Property of an Arc
 * \par Function Description
 *  Sets the value of the \a arc line space property if and only if \a arc
 *  is a valid GedaArc object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \sa geda_arc_get_line_space
 */
void geda_arc_set_line_space (GedaArc *arc, int space)
{
  if (is_a_geda_arc(arc)) {
    arc->line_options.line_space = space > 0 ? space : 0;
  }
}

/*!
 * \brief Set the Line Type Property of an Arc
 * \par Function Description
 *  Sets the value of \a arc line type if and only if \a arc is a
 *  valid GedaArc object.
 *
 * \sa geda_arc_get_line_type
 */
void geda_arc_set_line_type (GedaArc *arc, int line_type)
{
  if (is_a_geda_arc(arc)) {
    arc->line_options.line_type = line_type < TYPE_SOLID ? TYPE_SOLID :
                                  line_type > TYPE_ERASE ? TYPE_ERASE :
                                  line_type;
  }
}

/*!
 * \brief Set the End Width Property of an Arc
 * \par Function Description
 *  Sets the value of the \a arc line width property if and only if \a arc
 *  is a valid GedaArc object.
 *
 * \sa geda_arc_get_line_width
 */
void geda_arc_set_line_width (GedaArc *arc, int width)
{
  if (is_a_geda_arc(arc)) {
    arc->line_options.line_width = width > 0 ? width : 0;
  }
}

/*!
 * \brief Set the position of a GedaArc
 * \par Function Description
 *  Sets the radius of \a arc if \a arc is a valid Geda Arc
 *  object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_set_center_x geda_arc_set_center_y
 */
void geda_arc_set_position (GedaArc *arc, int x, int y)
{
  if (is_a_geda_arc(arc)) {
    arc->x = x;
    arc->y = y;
  }
}

/*!
 * \brief Set the radius of a GedaArc
 * \par Function Description
 *  Sets the radius of \a arc if \a arc is a valid Geda Arc
 *  object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_radius
 */
void geda_arc_set_radius (GedaArc *arc, int radius)
{
  if (is_a_geda_arc(arc)) {
    arc->radius = radius;
  }
}

/*!
 * \brief Set the start_angle of a GedaArc
 * \par Function Description
 *  Sets the starting angle of \a arc if \a arc is a valid Geda
 *  Arc object, if \a arc is invalid then nothing is done.
 *
 * \sa geda_arc_object_set_start_angle
 */
void geda_arc_set_start_angle (GedaArc *arc, int angle)
{
  if (is_a_geda_arc(arc)) {
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
bool geda_arc_within_sweep(const GedaArc *arc, int x, int y)
{
  if (is_a_geda_arc(arc)) {

    /* gcc 5.3.1 20160413 32bit yields incorrect values from
     * rounding unless angle is declared volatile */
    volatile double a0;
    volatile double a1;
    volatile double angle;
    volatile double dx;
    volatile double dy;

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
