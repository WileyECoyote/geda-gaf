/* -*- geda_line.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Ales Hvezda
 * Copyright (C) 2013-2015 Wiley Edward Hill
 *
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
/*! \file geda_line.c
 *  \brief Geda Line Object Class Module
 */

/** \defgroup geda-line-object GedaLine Object
 * @{
 * \brief Implmentation of #GedaLine Class
 * \par
*  A GedaLine object is a graphical object that does not involve
 * electrical interconnections. Lines have line-type properties.
 * The GedaLineClass is derived from the GedaObject base class.
 *
 * \class GedaLine geda_line.h "include/libgeda/geda_line.h"
 * \implements geda-object
 */

#include "../../../config.h"
#include <math.h>
#include <libgeda_priv.h>

enum {
  PROP_0,
  PROP_FIRST_X,
  PROP_FIRST_Y,
  PROP_SECOND_X,
  PROP_SECOND_Y,
  PROP_END_CAP,
  PROP_TYPE,
  PROP_WIDTH,
  PROP_SPACE,
  PROP_LENGTH
};

static GObjectClass *geda_line_parent_class = NULL;

/*!
 * \brief Calculate and return the boundaries of a Line object
 * \par Function Description
 *  This function calculates the object boundaries of a Line \a object
 *  but does not account for the thickness of the line.
 *
 * \note Bus, Net and Pin objects are derived from a GedaLine, so this
 *       is also the bounds function for those object types.
 *
 * \param [in]  object Pointer to Line object
 */
static int geda_line_bounds(GedaObject *object)
{
  int expand;

  g_return_val_if_fail(GEDA_IS_LINE(object), FALSE);

  expand = ceil (0.5 * G_SQRT2 * object->line_options->line_width);

  /* This is not strictly correct, but a 1st order approximation */
  object->left   = min( object->line->x[0], object->line->x[1] ) - expand;
  object->right  = max( object->line->x[0], object->line->x[1] ) + expand;
  object->top    = min( object->line->y[0], object->line->y[1] ) - expand;
  object->bottom = max( object->line->y[0], object->line->y[1] ) + expand;

  object->bounds_valid = TRUE;

  return TRUE;
}

/*!
 * \brief GedaType instance initializer for Line
 * \par Function Description
 *  GedaType instance initializer for Line, initializes a new empty
 *  Line object by setting pointers to NULL and numbers to zero.
 *
 * \param [in] instance The Line structure being initialized,
 * \param [in] g_class  The Line class we are initializing.
 */
static void geda_line_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaLine   *line   = (GedaLine*)instance;
  GedaObject *object = &line->parent_instance;

  line->x[0]         = 0;
  line->y[0]         = 0;
  line->x[1]         = 0;
  line->y[1]         = 0;

  line->line_options.line_end     = default_line_end;
  line->line_options.line_type    = default_line_type;
  line->line_options.line_width   = default_line_width;
  line->line_options.line_space   = default_line_space;
  line->line_options.line_length  = default_line_length;

  object->line                    = line;
  object->line_options            = &line->line_options;
}

static void geda_line_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_line_parent_class)->dispose(object);
}

/*!
 * \brief Geda Line GedaObject Finalization Function
 * \par Function Description
 *  Invalidates the Line's markers and then chains up to the parent
 *  finalize handler. Once invalidated, GEDA_IS_LINE will fail.
 */
static void geda_line_finalize(GObject *object)
{
  GedaObject *obj  = GEDA_OBJECT(object);

  /* The object is no longer a GedaLine */
  obj->line    = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_line_parent_class)->finalize(object);
  /* Possibly return to net or pin finalizer */
}

static void get_property (GObject *object, unsigned int  prop_id,
                                           GValue       *value,
                                           GParamSpec   *pspec)

{
  GedaObject   *obj          = GEDA_OBJECT(object);
  GedaLine     *line         = obj->line;
  LINE_OPTIONS *line_options = &line->line_options;

  switch (prop_id)
  {
    case PROP_FIRST_X:
      g_value_set_int (value, line->x[0]);
      break;

    case PROP_FIRST_Y:
      g_value_set_int (value, line->y[0]);
      break;

    case PROP_SECOND_X:
      g_value_set_int (value, line->x[1]);
      break;

    case PROP_SECOND_Y:
      g_value_set_int (value, line->y[1]);
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

static void set_property (GObject *object, unsigned int  prop_id,
                                           const GValue *value,
                                           GParamSpec   *pspec)
{
  GedaObject   *obj          = GEDA_OBJECT(object);
  GedaLine     *line         = obj->line;
  LINE_OPTIONS *line_options = &line->line_options;

  switch (prop_id)
  {
    case PROP_FIRST_X:
      line->x[0] = g_value_get_int (value);
      break;

    case PROP_FIRST_Y:
      line->y[0] = g_value_get_int (value);
      break;

    case PROP_SECOND_X:
      line->x[1] = g_value_get_int (value);
      break;

    case PROP_SECOND_Y:
      line->y[1] = g_value_get_int (value);
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

/*!
 * \brief GedaType class initializer for GedaLine
 * \par Function Description
 *  GedaType class initializer for GedaLine. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  class       The GedaLine class we are initializing
 * \param [in]  class_data  The GedaLine structure associated with the class
 */
static void geda_line_class_init(void *class, void *class_data)
{
  GedaLineClass   *line_class   = (GedaLineClass*)class;
  GObjectClass    *object_class = (GObjectClass*)class;
  GedaObjectClass *geda_class   = (GedaObjectClass*)class;
  GParamSpec      *params;

  geda_line_parent_class        = g_type_class_peek_parent(class);

  line_class->finalize          = geda_line_finalize;

  object_class->dispose         = geda_line_dispose;
  object_class->finalize        = line_class->finalize;

  object_class->get_property    = get_property;
  object_class->set_property    = set_property;

  geda_class->bounds            = geda_line_bounds;

  params = g_param_spec_int ("first-x",
                           _("First X"),
                           _("X coordinate of the first point"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FIRST_X, params);

  params = g_param_spec_int ("first-y",
                           _("First Y"),
                           _("Y coordinate of the first point"),
                             0,
                             G_MAXINT,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_FIRST_Y, params);

  params = g_param_spec_int ("second-x",
                           _("Second X"),
                           _("X coordinate of the second point"),
                             0,
                             G_MAXINT,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_SECOND_X, params);

  params = g_param_spec_int ("second-y",
                           _("Second Y"),
                           _("Y coordinate of the second point"),
                             0,
                             G_MAXINT,
                             0,
                            (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_SECOND_Y, params);

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

/*!
 * \brief Function to retrieve Line's Type identifier.
 * \par Function Description
 *  Function to retrieve a #Line Type identifier. When first called,
 *  the function registers a #Line in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Line and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 * \return GedaObjectType identifier associated with Line.
 */
GedaObjectType geda_line_get_type (void)
{
  static volatile GedaType geda_line_type = 0;

  if (g_once_init_enter (&geda_line_type)) {

    static const GTypeInfo info = {
      sizeof(GedaLineClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_line_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(GedaLine),
      0,                      /* n_preallocs         */
      geda_line_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaLine");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_line_type, type);
  }

  return geda_line_type;
}

/*!
 * \brief Returns a pointer to a new Line object.
 * \par Function Description
 *  Returns a pointer to a new Line object.
 *
 * \return pointer to the new Line object.
 */
GedaObject *geda_line_new (void)
{
  GedaObject *line = g_object_new (GEDA_TYPE_LINE,
                                   "type", OBJ_LINE,
                                   "name", "line",
                                   NULL);

  return GEDA_OBJECT(line);
}

/*!
 * \brief Determine if object is a Geda Line GedaObject.
 * \par Function Description
 *  Returns true if the argument is a Geda Line object.
 *
 * \return boolean.
 */
bool is_a_geda_line (const GedaLine *lin)
{
 if (GEDA_IS_OBJECT (lin)) {
   GedaObject *obj = (GedaObject*)lin;
   return (obj->type == OBJ_LINE || obj->type == OBJ_NET ||
           obj->type == OBJ_PIN  || obj->type == OBJ_BUS);
 }

 return FALSE;

}

/*!
 * \brief Retrieve End Cap type Property of an GedaLine
 * \par Function Description
 *  Returns the value of \a line end-cap type if and only if \a line
 *  is a valid GedaLine object.
 *
 * \return integer value of end-cap type or -0 if \a line is invalid.
 *
 * \sa geda_line_object_set_end_cap
 */
int geda_line_get_end_cap (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->line_options.line_end;
  }
  return -0;
}

/*!
 * \brief Retrieve Line Length Property of a GedaLine
 * \par Function Description
 *  Returns the value of the \a line line-length property if \a line
 *  is a valid GedaLine object. The line-length property controls the
 *  length of line segments for line types dashed, center and phantom,
 *  to get the "length" of a line see geda_math_line_length.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \return integer value of line length or -0 if \a line is invalid.
 *
 * \sa geda_line_object_set_line_length
 */
int geda_line_get_line_length (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->line_options.line_length;
  }
  return -0;
}

/*!
 * \brief Retrieve Line Space Property of a GedaLine
 * \par Function Description
 *  Returns the value of the \a line line space property if and only if \a line
 *  is a valid GedaLine object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \return integer value of line space or -0 if \a line is invalid.
 *
 * \sa geda_line_object_set_line_space
 */
int geda_line_get_line_space (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->line_options.line_space;
  }
  return -0;
}

/*!
 * \brief Retrieve End Type Property of a GedaLine
 * \par Function Description
 *  Returns the value of \a line line type if and only if \a line is
 *  a valid GedaLine object.
 *
 * \return integer value of line type or -0 if \a line is invalid.
 *
 * \sa geda_line_object_set_line_type
 */
int geda_line_get_line_type (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->line_options.line_type;
  }
  return -0;
}

/*!
 * \brief Retrieve End Width Property of a GedaLine
 * \par Function Description
 *  Returns the value of the \a line line width property if and only
 *  if \a line is a valid GedaLine object.
 *
 * \return integer value of line-width or -0 if \a line is invalid.
 *
 * \sa geda_line_object_set_line_width
 */
int geda_line_get_line_width (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->line_options.line_width;
  }
  return -0;
}

/*!
 * \brief Retrieve First X coordinate of the a GedaLine
 * \par Function Description
 *  Returns the current X value of \a line if and only if \a line is
 *  a valid GedaLine object.
 *
 * \return integer value of first X or 0 if \a line is invalid.
 *
 * \sa geda_line_object_get_x1
 */
int geda_line_get_x1 (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->x[0];
  }
  return -0;
}

/*!
 * \brief Retrieve Second X coordinate of the a GedaLine
 * \par Function Description
 *  Returns the second X value of \a line if and only if \a line
 *  is a valid GedaLine object.
 *
 * \return integer value of second X or 0 if \a line is invalid.
 *
 * \sa geda_line_object_get_x2
 */
int geda_line_get_x2 (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->x[1];
  }
  return -0;
}

/*!
 * \brief Retrieve First Y coordinate of the a GedaLine
 * \par Function Description
 *  Returns the first Y value of \a line if and only if \a line is
 *  a valid GedaLine object.
 *
 * \return integer value of first Y or 0 if \a line is invalid.
 *
 * \sa geda_line_object_get_y1
 */
int geda_line_get_y1 (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->y[0];
  }
  return -0;
}

/*!
 * \brief Retrieve Second Y coordinate of the a GedaLine
 * \par Function Description
 *  Returns the second Y value of \a line if and only if \a line
 *  is a valid GedaLine object.
 *
 * \return integer value of second Y or 0 if \a line is invalid.
 *
 * \sa geda_line_object_get_y2
 */
int geda_line_get_y2 (const GedaLine *line)
{
  if (is_a_geda_line(line)) {
    return line->y[1];
  }
  return -0;
}

/*!
 * \brief Set the End Cap type Property of a GedaLine
 * \par Function Description
 *  Sets the value of \a line end-cap type if and only if \a line is
 *  a valid GedaLine object. The line-end properties is only applicable
 *  for fill types FILLING_MESH and FILLING_HATCH.
 *
 * \sa geda_line_object_get_end_cap
 */
void geda_line_set_end_cap (GedaLine *line, int line_end)
{
  if (is_a_geda_line(line)) {
    line->line_options.line_end = line_end < END_NONE ? END_NONE :
                                  line_end > END_VOID ? END_VOID :
                                  line_end;
  }
}

/*!
 * \brief Set the Line Length Property of an GedaLine
 * \par Function Description
 *  Returns the value of the \a line line-length property if and only if
 *  \a line is a valid GedaLine. The line-length property controls the
 *  length of line segments for line types dashed, center and phantom.
 *
 * \note Line length is only applicable when line-type is not TYPE_SOLID
 *       or TYPE_DOTTED.
 *
 * \param [in] line    Pointer to an GedaLine Object
 * \param [in] length  new value for the line-length property
 *
 * \sa geda_line_object_get_line_length
 */
void geda_line_set_line_length (GedaLine *line, int length)
{
  if (is_a_geda_line(line)) {
    line->line_options.line_length = length > 0 ? length : 0;
  }
}

/*!
 * \brief Set the Line Space Property of a GedaLine
 * \par Function Description
 *  Sets the value of the \a line line space property if and only if \a line
 *  is a valid GedaObject object. The line-space property controls the distance
 *  between line-length for line types dashed, center, phantom and between dots
 *  for line type dotted.
 *
 * \note Line space is only applicable when line-type is not TYPE_SOLID.
 *
 * \param [in] line    Pointer to an GedaLine Object
 * \param [in] space   new LINE_TYPE value for the line type
 *
 * \sa geda_line_object_get_line_space
 */
void geda_line_set_line_space (GedaLine *line, int space)
{
  if (is_a_geda_line(line)) {
    line->line_options.line_space = space > 0 ? space : 0;
  }
}

/*!
 * \brief Set the Line Type Property of an GedaLine
 * \par Function Description
 *  Sets the value of \a line line type if and only if \a line is a
 *  valid GedaObject object.
 *
 * \param [in] line   Pointer to an GedaLine Object
 * \param [in] type   new LINE_TYPE value for the line type
 *
 * \sa geda_line_object_get_line_type
 */
void geda_line_set_line_type (GedaLine *line, int type)
{
  if (is_a_geda_line(line)) {
    line->line_options.line_type = type < TYPE_SOLID ? TYPE_SOLID :
                                    type > TYPE_ERASE ? TYPE_ERASE :
                                    type;
  }
}

/*!
 * \brief Set the End Width Property of a GedaLine
 * \par Function Description
 *  Sets the value of the \a line line width property if and only if
 *  \a line is a valid GedaLine object.
 *
 * \param [in] line   Pointer to an GedaLine Object
 * \param [in] width  new value for the line width
 *
 * \sa geda_line_object_get_line_width
 */
void geda_line_set_line_width (GedaLine *line, int width)
{
  if (is_a_geda_line(line)) {
    line->line_options.line_width = width > 0 ? width : 0;
  }
}

/*!
 * \brief Set the coordinate value of first X of a GedaLine
 * \par Function Description
 *  Sets the first abscissa of \a line if \a line is a valid
 *  GedaLine, if \a line is invalid then nothing is done.
 *
 * \sa geda_line_object_set_x1
 */
void geda_line_set_x1 (GedaLine *line, int x)
{
  if (is_a_geda_line(line)) {
    line->x[0] = x;
  }
}

/*!
 * \brief Set the coordinate value of Second X of a GedaLine
 * \par Function Description
 *  Sets the second abscissa of \a line if \a line is a valid
 *  GedaLine, if \a line is invalid then nothing is done.
 *
 * \sa geda_line_object_set_x2
 */
void geda_line_set_x2 (GedaLine *line, int x)
{
  if (is_a_geda_line(line)) {
    line->x[1] = x;
  }
}

/*!
 * \brief Set the coordinate value of first Y of a GedaLine
 * \par Function Description
 *  Sets the first ordinate of \a line if \a line is a valid
 *  GedaLine, if \a line is invalid then nothing is done.
 *
 * \sa geda_line_object_set_y1
 */
void geda_line_set_y1 (GedaLine *line, int y)
{
  if (is_a_geda_line(line)) {
    line->y[0] = y;
  }
}

/*!
 * \brief Set the coordinate value of Second Y of a GedaLine
 * \par Function Description
 *  Sets the second ordinate of \a line if \a line is a valid
 *  GedaLine, if \a line is invalid then nothing is done.
 *
 * \sa geda_line_object_set_y2
 */
void geda_line_set_y2 (GedaLine *line, int y)
{
  if (is_a_geda_line(line)) {
    line->y[1] = y;
  }
}

/** @} endgroup geda-line-object */