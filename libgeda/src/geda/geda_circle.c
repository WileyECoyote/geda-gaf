/* -*- geda_circle.c -*-
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
/*! \file geda_circle.c
 *  \brief Geda Circle Object Class derived from the GedaObject Class
 */
/** \defgroup geda-circle-object Geda Circle Object
 *  @{
 */
/*! \class Circle geda_circle.h "include/libgeda/geda_circle.h"
 *  \implements geda-object
 *  \brief This is an implementaion class for GEDA Circle Objects.
 *  A Geda Circle Object is a graphical object that does not involve
 *  electrical interconnections. Circles have line-type and fill-type
 *  properties.
 */

#include <config.h>

#include <libgeda_priv.h>

static GObjectClass *geda_circle_parent_class = NULL;

/*! \brief Get circle bounding rectangle in WORLD coordinates
 *
 *  \par Function Description
 *  Sets the <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B>
 *  parameters to the boundings of the circle object described in
 *  the \a circle in world units.
 *
 */
int
geda_circle_bounds(Object *object)
{
  int halfwidth;

  g_return_val_if_fail (GEDA_IS_CIRCLE(object), FALSE);

  halfwidth = object->circle->line_options.line_width / 2;

  /* This isn't strictly correct, but a 1st order approximation */
  object->left   = object->circle->center_x - object->circle->radius - halfwidth;
  object->top    = object->circle->center_y - object->circle->radius - halfwidth;
  object->right  = object->circle->center_x + object->circle->radius + halfwidth;
  object->bottom = object->circle->center_y + object->circle->radius + halfwidth;

  return TRUE;
}

/*! \brief Type instance initializer for Circle
 *
 *  \par Function Description
 *  Type instance initializer for Circle, initializes a new empty
 *  Circle object by setting pointers to NULL and numbers to zero,
 *  the circle PID variable is set to the next circle index.
 *
 *  \param [in] instance The Circle structure being initialized,
 *  \param [in] class    The Circle class we are initializing.
 */
static void geda_circle_instance_init(GTypeInstance *instance, void *class)
{
  Circle *circle       = (Circle*)instance;
  Object *object       = &circle->parent_instance;

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

  circle->head_marker               = GEDA_TYPE_CIRCLE;
  circle->tail_marker               = circle->head_marker;
}

static void
geda_circle_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_circle_parent_class)->dispose(object);
}

/*! \brief Geda Circle Object Finalization Function
 *  \par Function Description
 *   Invalidates the Circle's markers and then chains up to the parent's
 *   finalize handler. Once invalidated, GEDA_IS_CIRCLE will fail.
 */
static void geda_circle_finalize(GObject *object)
{
  Circle *circle = GEDA_CIRCLE(object);

  /* The object is no longer a GedaCircle */
  circle->head_marker = 1;
  circle->tail_marker = 0;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_circle_parent_class)->finalize(object);
}

/*! \brief Type class initializer for Circle
 *
 *  \par Function Description
 *  Type class initializer for Circle. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class      The Circle class we are initializing
 *  \param [in]  class_data   The Circle structure associated with the class
 */
static void geda_circle_class_init(void *g_class, void *class_data)
{
  CircleClass  *class          = (CircleClass*)g_class;
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );
  ObjectClass  *object_class   = GEDA_OBJECT_CLASS( class );

  geda_circle_parent_class     = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_circle_dispose;
  gobject_class->finalize      = geda_circle_finalize;

  object_class->bounds         = geda_circle_bounds;
}

/*! \brief Function to retrieve Circle's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #Circle Type identifier. When first called,
 *  the function registers a #Circle in the GedaType system to obtain
 *  an identifier that uniquely itentifies a Circle and returns the
 *  unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaType identifier associated with Circle.
 */
GedaType geda_circle_get_type (void)
{
  static GedaType geda_circle_type = 0;

  if (g_once_init_enter (&geda_circle_type)) {

    static const GTypeInfo info = {
      sizeof(CircleClass),
      NULL,                     /* base_init           */
      NULL,                     /* base_finalize       */
      geda_circle_class_init,   /* (GClassInitFunc)    */
      NULL,                     /* class_finalize      */
      NULL,                     /* class_data          */
      sizeof(Circle),
      0,                        /* n_preallocs         */
      geda_circle_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("Circle");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_circle_type, type);
  }

  return geda_circle_type;
}

/*! \brief Returns a pointer to a new Circle object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Circle object.
 *
 *  \return pointer to the new Circle object.
 */
Object *geda_circle_new (void)
{
  Object *circle = g_object_new( GEDA_TYPE_CIRCLE,
                                 "type", OBJ_CIRCLE,
                                 "name", "circle",
                                 NULL );
  return GEDA_OBJECT(circle);
}

/*! \brief Determine if object is a Geda Circle Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Circle object.
 *
 *  \return boolean.
 */
bool is_a_geda_circle_object (Circle *cir)
{
  return GEDA_IS_OBJECT(cir) &&
        (GEDA_TYPE_CIRCLE == (cir->head_marker & cir->tail_marker));
}
/** @} endgroup geda-circle-object */
