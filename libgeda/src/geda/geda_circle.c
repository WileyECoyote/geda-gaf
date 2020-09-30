/* -*- geda_circle.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
/*! \file geda_circle.c
 *  \brief GedaCircle Class Module
 */

/** \defgroup geda-circle-object GedaCircle Object
 * @{
 * \brief Implmentation of #GedaCircle Class
 * \par
 *  A GedaCircle is a graphical object that does not involve electrical
 *  interconnections. Circles have line-type and fill-type properties
 *  and are derived from the GedaObject base class.
 *
 * \class GedaCircle geda_circle.h "include/libgeda/geda_circle.h"
 * \implements geda-object
 */

#include "../../../config.h"

#include <libgeda_priv.h>

enum {
  PROP_0,
  PROP_CENTER_X,
  PROP_CENTER_Y,
  PROP_RADIUS,
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

static GObjectClass *geda_circle_parent_class = NULL;

/*!
 * \brief Get circle bounding rectangle
 * \par Function Description
 *  Sets the <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B>
 *  parameters to the boundings of the circle object described in
 *  the \a circle in world units.
 */
int geda_circle_bounds(GedaObject *object)
{
  int halfwidth;

  g_return_val_if_fail (GEDA_IS_CIRCLE(object), FALSE);

  halfwidth = object->circle->line_options.line_width >> 1; /* divide by 2 */

  /* This is not strictly correct, but a 1st order approximation */
  object->left   = object->circle->center_x - object->circle->radius - halfwidth;
  object->top    = object->circle->center_y - object->circle->radius - halfwidth;
  object->right  = object->circle->center_x + object->circle->radius + halfwidth;
  object->bottom = object->circle->center_y + object->circle->radius + halfwidth;

  object->bounds_valid = TRUE;

  return TRUE;
}

static void geda_circle_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_circle_parent_class)->dispose(object);
}

/*!
 * \brief Geda Circle GedaObject Finalization Function
 * \par Function Description
 *  Invalidates the Circle and then chains up to the parent's
 *  finalize handler. Once invalidated, GEDA_IS_CIRCLE will fail.
 */
static void geda_circle_finalize(GObject *object)
{
  GedaObject *obj = GEDA_OBJECT(object);

  /* The object is no longer a GedaCircle */
  obj->circle = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_circle_parent_class)->finalize(object);
}

static void get_property (GObject *object, unsigned int  prop_id,
                                           GValue       *value,
                                           GParamSpec   *pspec)

{
  GedaCircle   *circle       = GEDA_CIRCLE(object);
  LINE_OPTIONS *line_options = &circle->line_options;
  FILL_OPTIONS *fill_options = &circle->fill_options;

  switch (prop_id)
  {
    case PROP_CENTER_X:
      g_value_set_int (value, circle->center_x);
      break;

    case PROP_CENTER_Y:
      g_value_set_int (value, circle->center_y);
      break;

    case PROP_RADIUS:
      g_value_set_int (value, circle->radius);
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
  GedaCircle   *circle       = GEDA_CIRCLE(object);
  LINE_OPTIONS *line_options = &circle->line_options;
  FILL_OPTIONS *fill_options = &circle->fill_options;

  switch (prop_id)
  {
    case PROP_CENTER_X:
      circle->center_x = g_value_get_int (value);
      break;

    case PROP_CENTER_Y:
      circle->center_y = g_value_get_int (value);
      break;

    case PROP_RADIUS:
      circle->radius = g_value_get_int (value);
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
 * \brief Type instance initializer for GedaCircle
 * \par Function Description
 *  Type instance initializer for GedaCircle, initializes a new empty
 *  GedaCircle object by setting pointers to NULL and numbers to zero.
 *
 * \param [in] instance The GedaCircle structure being initialized,
 *
 * \param [in] class    The GedaCircle class we are initializing.
 */
static void geda_circle_instance_init(GTypeInstance *instance, void *class)
{
  GedaCircle *circle       = (GedaCircle*)instance;
  GedaObject *object       = &circle->parent_instance;

  circle->center_x     = 0;
  circle->center_y     = 0;
  circle->radius       = 0;

  circle->fill_options.fill_type    = default_fill_type;
  circle->fill_options.fill_width   = default_fill_width;
  circle->fill_options.fill_angle1  = default_fill_angle1;
  circle->fill_options.fill_angle2  = default_fill_angle2;
  circle->fill_options.fill_pitch1  = default_fill_pitch1;
  circle->fill_options.fill_pitch2  = default_fill_pitch2;

  circle->line_options.line_end     = default_line_end;
  circle->line_options.line_type    = default_line_type;
  circle->line_options.line_width   = default_line_width;
  circle->line_options.line_space   = default_line_space;
  circle->line_options.line_length  = default_line_length;

  object->circle                    = circle;
  object->fill_options              = &circle->fill_options;
  object->line_options              = &circle->line_options;
}

/*!
 * \brief Type class initializer for GedaCircle
 * \par Function Description
 *  Type class initializer for GedaCircle. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  klass       The GedaCircle class we are initializing
 * \param [in]  class_data  The Circle structure associated with the class
 */
static void geda_circle_class_init(void *klass, void *class_data)
{
  GedaCircleClass *class        = (GedaCircleClass*)klass;
  GObjectClass    *object_class = (GObjectClass*)klass;
  GedaObjectClass *geda_class   = (GedaObjectClass*)klass;
  GParamSpec      *params;

  geda_circle_parent_class      = g_type_class_peek_parent(class);

  object_class->dispose         = geda_circle_dispose;
  object_class->finalize        = geda_circle_finalize;

  object_class->get_property    = get_property;
  object_class->set_property    = set_property;

  geda_class->bounds            = geda_circle_bounds;

  params = g_param_spec_int ("center-x",
                           _("Center X"),
                           _("Abscissa of the circle center point"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_CENTER_X, params);

  params = g_param_spec_int ("center-y",
                           _("Center Y"),
                           _("Ordinate of the circle center point"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_CENTER_Y, params);

  params = g_param_spec_int ("radius",
                           _("Radius"),
                           _("Radius of the circle"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_RADIUS, params);

  /* Gedacircles have some line-type properties but are not derived from
   * GedaLine therefore these properties must be defined for the GedaCircle... */

  params = g_param_spec_int ("end-cap",
                           _("End Cap"),
                           _("Line end cap is not used by circles"),
                             END_NONE,
                             END_ROUND,
                             END_NONE,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_LINE_CAP, params);

  params = g_param_spec_int ("line-type",
                           _("Line Type"),
                           _("The line type"),
                             TYPE_SOLID,
                             TYPE_PHANTOM,
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

  params = g_param_spec_int ("fill-type",
                           _("Fill Type"),
                           _("The Object fill type; hatch mesh, solid, etc..."),
                             FILLING_HOLLOW,
                             FILLING_HATCH,
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
 * \brief Function to retrieve GedaCircle's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaCircle Type identifier. When first called,
 *  the function registers a #GedaCircle in the GType system to obtain
 *  an identifier that uniquely itentifies a GedaCircle and returns the
 *  unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 * \return GedaType identifier associated with GedaCircle.
 */
GedaType geda_circle_get_type (void)
{
  static volatile GedaType geda_circle_type = 0;

  if (g_once_init_enter (&geda_circle_type)) {

    static const GTypeInfo info = {
      sizeof(GedaCircleClass),
      NULL,                     /* base_init           */
      NULL,                     /* base_finalize       */
      geda_circle_class_init,   /* (GClassInitFunc)    */
      NULL,                     /* class_finalize      */
      NULL,                     /* class_data          */
      sizeof(GedaCircle),
      0,                        /* n_preallocs         */
      geda_circle_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaCircle");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_circle_type, type);
  }

  return geda_circle_type;
}

/*!
 * \brief Returns a pointer to a new Circle object.
 * \par Function Description
 *  Returns a pointer to a new Circle object.
 *
 * \return pointer to the new Circle object.
 */
GedaObject *geda_circle_new (void)
{
  GedaObject *circle = g_object_new( GEDA_TYPE_CIRCLE,
                                     "type", OBJ_CIRCLE,
                                     "name", "circle",
                                     NULL );
  return GEDA_OBJECT(circle);
}

/*!
 * \brief Determine if object is a GedaCircle.
 * \par Function Description
 *  Returns true if the argument is a Geda Circle object.
 *
 * \return boolean.
 */
bool is_a_geda_circle (const GedaCircle *cir)
{
  return GEDA_IS_OBJECT(cir) && (((GedaObject*)cir)->type == OBJ_CIRCLE);
}

/*!
 * \brief Retrieve Ordinate of an GedaCircle center coordinate
 * \par Function Description
 *  Returns the current center X value of \a circle if and only
 *  if \a circle is a valid GedaCircle object.
 *
 * \return integer value of center X or 0 if \a circle is invalid.
 *
 * \sa geda_circle_object_get_center_x
 */
int geda_circle_get_center_x (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->center_x;
  }
  return -0;
}

/*!
 * \brief Retrieve abscisa of an GedaCircle center coordinate
 * \par Function Description
 *  Returns the current center Y value of \a circle if and only
 *  if \a circle is a valid GedaCircle object.
 *
 * \return integer value of center Y or 0 if \a circle is invalid.
 *
 * \sa geda_circle_object_get_center_x
 */
int geda_circle_get_center_y (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->center_y;
  }
  return -0;
}

/*!
 * \brief Retrieve End Cap type Property of an GedaCircle
 * \par Function Description
 *  Returns the value of \a circle end-cap type if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \return integer value of end-cap type or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_end_cap
 */
int geda_circle_get_end_cap (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->line_options.line_end;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Angle 1 Property of a Box
 * \par Function Description
 *  Returns the value of \a circle fill angle 1 if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill angle 1 or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_fill_angle1
 */
int geda_circle_get_fill_angle1 (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->fill_options.fill_angle1;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Angle 2 Property of a Box
 * \par Function Description
 *  Returns the value of \a circle fill angle 2 if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill angle 2 or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_fill_angle2
 */
int geda_circle_get_fill_angle2 (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->fill_options.fill_angle2;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Pitch 1 Property of a Box
 * \par Function Description
 *  Returns the value of \a circle fill pitch 1 if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill pitch 1 or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_fill_pitch1
 */

int geda_circle_get_fill_pitch1 (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->fill_options.fill_pitch1;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Pitch 2 Property of a Box
 * \par Function Description
 *  Returns the value of \a circle fill pitch 2 if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill pitch 2 or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_fill_pitch2
 */
int geda_circle_get_fill_pitch2 (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->fill_options.fill_pitch2;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Type Property of a circle
 * \par Function Description
 *  Returns the value of \a circle fill type if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \return integer value of fill type or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_fill_type
 */
int geda_circle_get_fill_type (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->fill_options.fill_type;
  }
  return -0;
}

/*!
 * \brief Retrieve Fill Width Property of a circle
 * \par Function Description
 *  Returns the value of the \a circle fill width property if and only
 *  if \a circle is a valid GedaCircle object.
 *
 * \return integer value of fill width or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_fill_width
 */
int geda_circle_get_fill_width (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->fill_options.fill_width;
  }
  return -0;
}

/*!
 * \brief Retrieve Line Length Property of an Arc
 * \par Function Description
 *  Returns the value of the \a circle line length property if and only if
 *  \a circle is a valid GedaCircle object. The is not the length of the circle
 *  the line-length property controls the length of line segments for
 *  line types dashed, center and phantom, to get the "length" of
 *  a line see geda_math_line_length.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \return integer value of line length or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_line_length
 */
int geda_circle_get_line_length (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->line_options.line_length;
  }
  return -0;
}

/*!
 * \brief Retrieve Line Space Property of an Arc
 * \par Function Description
 *  Returns the value of the \a circle line space property if and only if \a circle
 *  is a valid GedaCircle object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \return integer value of line space or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_line_space
 */
int geda_circle_get_line_space (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->line_options.line_space;
  }
  return -0;
}

/*!
 * \brief Retrieve End Type Property of an Arc
 * \par Function Description
 *  Returns the value of \a circle line type if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \return integer value of line type or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_line_type
 */
int geda_circle_get_line_type (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->line_options.line_type;
  }
  return -0;
}

/*!
 * \brief Retrieve End Width Property of an Arc
 * \par Function Description
 *  Returns the value of the \a circle line width property if and only if \a circle
 *  is a valid GedaCircle object.
 *
 * \return integer value of line width or -0 if \a circle is invalid.
 *
 * \sa geda_circle_set_line_width
 */
int geda_circle_get_line_width (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->line_options.line_width;
  }
  return -0;
}

/*!
 * \brief Retrieve the radius of a GedaCircle
 * \par Function Description
 *  Returns the current radius value of \a circle if and only if
 * \a circle is a valid GedaCircle object.
 *
 * \return integer value of radius or 0 if \a circle is invalid.
 *
 * \sa geda_circle_object_get_radius
 */
int geda_circle_get_radius (const GedaCircle *circle) {
  if (is_a_geda_circle(circle)) {
    return circle->radius;
  }
  return -0;
}

/*!
 * \brief Set the center X of a GedaCircle
 * \par Function Description
 *  Sets the abscissa of \a circle center if \a circle is a valid
 *  Gedacircle, if \a circle is invalid then nothing is done.
 *
 * \sa geda_circle_object_set_center_x
 */
void geda_circle_set_center_x (GedaCircle *circle, int x) {
  if (is_a_geda_circle(circle)) {
    circle->center_x = x;
  }
}

/*!
 * \brief Set the Y center value of a GedaCircle
 * \par Function Description
 *  Sets the ordinate of \a circle if \a circle is a valid
 *  GedaCircle, if \a circle is invalid then nothing is done.
 *
 * \sa geda_circle_object_set_center_y
 */
void geda_circle_set_center_y (GedaCircle *circle, int y) {
  if (is_a_geda_circle(circle)) {
    circle->center_y = y;
  }
}

/*!
 * \brief Set the End Cap type Property of a GedaCircle
 * \par Function Description
 *  Sets the value of \a circle end-cap type if and only if \a circle is
 *  a valid GedaCircle object. The line-end properties is only applicable
 *  for fill types FILLING_MESH and FILLING_HATCH.
 *
 * \sa geda_circle_get_end_cap
 */
void geda_circle_set_end_cap (GedaCircle *circle, int line_end) {
  if (is_a_geda_circle(circle)) {
    circle->line_options.line_end = line_end < END_NONE ? END_NONE :
                                    line_end > END_VOID ? END_VOID :
                                    line_end;
  }
}

/*!
 * \brief Set the Fill Angle 1 Property of a circle
 * \par Function Description
 *  Sets the value of \a circle fill angle 1 if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \sa geda_circle_get_fill_angle1
 */
void geda_circle_set_fill_angle1 (GedaCircle *circle, int angle) {
  if (is_a_geda_circle(circle)) {
    circle->fill_options.fill_angle1 = angle;
  }
}

/*!
 * \brief Set the Fill Angle 2 Property of a circle
 * \par Function Description
 *  Sets the value of \a circle fill angle 2 if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \sa geda_circle_get_fill_angle2
 */
void geda_circle_set_fill_angle2 (GedaCircle *circle, int angle) {
  if (is_a_geda_circle(circle)) {
    circle->fill_options.fill_angle2 = angle;
  }
}

/*!
 * \brief Set the Fill Pitch 1 Property of a circle
 * \par Function Description
 *  Sets the value of \a circle fill pitch 1 if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \sa geda_circle_get_fill_pitch1
 */
void geda_circle_set_fill_pitch1 (GedaCircle *circle, int pitch) {
  if (is_a_geda_circle(circle)) {
    circle->fill_options.fill_pitch1 = pitch;
  }
}

/*!
 * \brief Set the Fill Pitch 2 Property of a circle
 * \par Function Description
 *  Sets the value of \a circle fill pitch 2 if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \sa geda_circle_get_fill_pitch2
 */
void geda_circle_set_fill_pitch2 (GedaCircle *circle, int pitch) {
  if (is_a_geda_circle(circle)) {
    circle->fill_options.fill_pitch2 = pitch;
  }
}

/*!
 * \brief Set the Fill Type Property of a circle
 * \par Function Description
 *  Sets the value of \a circle fill type if and only if \a circle is
 *  a valid GedaCircle object.
 *
 * \sa geda_circle_get_fill_type
 */
void geda_circle_set_fill_type (GedaCircle *circle, int type) {
  if (is_a_geda_circle(circle)) {
    circle->fill_options.fill_type = type < TYPE_SOLID ? TYPE_SOLID :
                                     type > TYPE_ERASE ? TYPE_ERASE :
                                     type;
  }
}

/*!
 * \brief Set the Fill Width Property of a circle
 * \par Function Description
 *  Sets the value of \a circle width of the fill if and only
 *  if \a circle is a valid GedaCircle object.
 *
 * \sa geda_circle_get_fill_width
 */
void geda_circle_set_fill_width (GedaCircle *circle, int width) {
  if (is_a_geda_circle(circle)) {
    circle->fill_options.fill_width = width;
  }
}

/*!
 * \brief Set the Line Length Property of an GedaCircle
 * \par Function Description
 *  Returns the value of the \a circle line length property if and only if
 *  \a circle is a valid GedaCircle object. The line-length property controls
 *  the length of line segments for line types dashed, center and phantom.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \sa geda_circle_get_line_length
 */
void geda_circle_set_line_length (GedaCircle *circle, int line_length) {
  if (is_a_geda_circle(circle)) {
    circle->line_options.line_length = line_length > 0 ? line_length : 0;
  }
}

/*!
 * \brief Set the Line Space Property of an Arc
 * \par Function Description
 *  Sets the value of the \a circle line space property if and only if \a circle
 *  is a valid GedaCircle object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \sa geda_circle_get_line_space
 */
void geda_circle_set_line_space (GedaCircle *circle, int space) {
  if (is_a_geda_circle(circle)) {
    circle->line_options.line_space = space > 0 ? space : 0;
  }
}

/*!
 * \brief Set the Line Type Property of an Arc
 * \par Function Description
 *  Sets the value of \a circle line type if and only if \a circle is a
 *  valid GedaCircle object.
 *
 * \sa geda_circle_get_line_type
 */
void geda_circle_set_line_type (GedaCircle *circle, int line_type) {
  if (is_a_geda_circle(circle)) {
    circle->line_options.line_type = line_type < TYPE_SOLID ? TYPE_SOLID :
                                  line_type > TYPE_ERASE ? TYPE_ERASE :
                                  line_type;
  }
}

/*!
 * \brief Set the End Width Property of an Arc
 * \par Function Description
 *  Sets the value of the \a circle line width property if and only if \a circle
 *  is a valid GedaCircle object.
 *
 * \sa geda_circle_get_line_width
 */
void geda_circle_set_line_width (GedaCircle *circle, int width) {
  if (is_a_geda_circle(circle)) {
    circle->line_options.line_width = width > 0 ? width : 0;
  }
}

/*!
 * \brief Set the radius of a GedaCircle
 * \par Function Description
 *  Sets the radius of \a circle if \a circle is a valid
 *  GedaCircle, if \a circle is invalid then nothing is done.
 *
 * \sa geda_circle_object_set_radius
 */
void geda_circle_set_radius (GedaCircle *circle, int radius) {
  if (is_a_geda_circle(circle)) {
    circle->radius = radius;
  }
}

/** @} endgroup geda-circle-object */
