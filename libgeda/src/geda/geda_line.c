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
 *  \brief Geda Line GedaObject Class derived from the GedaObject Class
 */
/** \defgroup geda-line-object Geda Line GedaObject
 *  @{
 */
/*! \class Line geda_line.h "include/libgeda/geda_line.h"
 *  \implements geda-object
 *  \brief This is an implementaion class for GEDA Line GedaObjects.
 *  A Geda Line GedaObject is a graphical object that does not involve
 *  electrical interconnections. Lines have line-type properties.
 */

#include <config.h>
#include <math.h>
#include <libgeda_priv.h>

static GObjectClass *geda_line_parent_class = NULL;

/*! \brief Calculate and return the boundaries of a Line object
 *
 *  \par Function Description
 *  This function calculates the object boudaries of a Line \a object.
 *
 *  \param [in]  object Pointer to Line object
 */
int
geda_line_bounds(GedaObject *object)
{
  int expand;

  g_return_val_if_fail(GEDA_IS_LINE(object), FALSE);

  expand =  ceil (0.5 * G_SQRT2 * object->line_options->line_width);

  /* This isn't strictly correct, but a 1st order approximation */
  object->left   = min( object->line->x[0], object->line->x[1] ) - expand;
  object->right  = max( object->line->x[0], object->line->x[1] ) + expand;
  object->top    = min( object->line->y[0], object->line->y[1] ) - expand;
  object->bottom = max( object->line->y[0], object->line->y[1] ) + expand;

  return TRUE;
}

/*! \brief GedaType instance initializer for Line
 *
 *  \par Function Description
 *  GedaType instance initializer for Line, initializes a new empty
 *  Line object by setting pointers to NULL and numbers to zero.
 *
 *  \param [in] instance The Line structure being initialized,
 *  \param [in] g_class  The Line class we are initializing.
 */
static void geda_line_instance_init(GTypeInstance *instance, void *g_class)
{
  Line       *line   = (Line*)instance;
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

static void
geda_line_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_line_parent_class)->dispose(object);
}

/*! \brief Geda Line GedaObject Finalization Function
 *  \par Function Description
 *   Invalidates the Line's markers and then chains up to the parent's
 *   finalize handler. Once invalidated, GEDA_IS_LINE will fail.
 */
static void geda_line_finalize(GObject *object)
{
  GedaObject *obj  = GEDA_OBJECT(object);

  /* The object is no longer a GedaLine */
  obj->line    = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_line_parent_class)->finalize(object);
  /* Possible return to line, net, or pin finalizer */
}

/*! \brief GedaType class initializer for Line
 *
 *  \par Function Description
 *  GedaType class initializer for Line. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  class       The Line class we are initialising
 *  \param [in]  class_data  The Line structure associated with the class
 */
static void geda_line_class_init(void *class, void *class_data)
{
  LineClass       *line_class    = (LineClass*)class;
  GObjectClass    *gobject_class = G_OBJECT_CLASS(class);
  GedaObjectClass *geda_class    = GEDA_OBJECT_CLASS(class);

  geda_line_parent_class         = g_type_class_peek_parent(class);

  line_class->finalize           = geda_line_finalize;

  gobject_class->dispose         = geda_line_dispose;
  gobject_class->finalize        = line_class->finalize;

  geda_class->bounds             = geda_line_bounds;
}

/*! \brief Function to retrieve Line's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #Line Type identifier. When first called,
 *  the function registers a #Line in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Line and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with Line.
 */
GedaObjectType geda_line_get_type (void)
{
  static volatile GedaType geda_line_type = 0;

  if (g_once_init_enter (&geda_line_type)) {

    static const GTypeInfo info = {
      sizeof(LineClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_line_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(Line),
      0,                      /* n_preallocs         */
      geda_line_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("Line");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_line_type, type);
  }

  return geda_line_type;
}

/*! \brief Returns a pointer to a new Line object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Line object.
 *
 *  \return pointer to the new Line object.
 */
GedaObject *geda_line_new (void)
{
  GedaObject *line = g_object_new( GEDA_TYPE_LINE,
                              "type", OBJ_LINE,
                              "name", "line",
                               NULL);
  return GEDA_OBJECT(line);
}

/*! \brief Determine if object is a Geda Line GedaObject.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Line object.
 *
 *  \return boolean.
 */
bool is_a_geda_line_object (Line *lin)
{
 if (GEDA_IS_OBJECT (lin)) {
   GedaObject *obj = (GedaObject*)lin;
   return (obj->type == OBJ_LINE || obj->type == OBJ_NET ||
           obj->type == OBJ_PIN  || obj->type == OBJ_BUS);
 }

 return FALSE;

}
/** @} endgroup geda-line-object */