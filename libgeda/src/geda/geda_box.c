/* -*- geda_box.c -*-
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
/*! \file geda_box.c
 *  \brief Geda Box Object Class Module
 */

/** \defgroup geda-box-object Geda Box Object
 * @{
 * \brief Implmentation of #GedaBox Class
 * \par
 *  A Geda Box Object is a graphical object that does not involve
 *  electrical interconnections. GedaBox objects have line-type and
 *  fill-type properties and are derived from the GedaObject base
 *  class.
 *
 * \class GedaBox geda_box.h "include/libgeda/geda_box.h"
 * \implements geda-object
 */

#include <config.h>

#include <libgeda_priv.h>

static GObjectClass *geda_box_parent_class = NULL;

/*! \brief Get Box bounding rectangle.
 *
 *  \par Function Description
 *  Sets the <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B>
 *  parameters to the boundings of the box object described in <B>*box</B>
 *  in world units.
 *
 *  \param [in]  object  Box GedaObject to read coordinates from.
 */
int
geda_box_bounds(GedaObject *object)
{
  int halfwidth;

  g_return_val_if_fail (GEDA_IS_BOX(object), FALSE);

  halfwidth = object->line_options->line_width / 2;

  /* This isn't strictly correct, but a 1st order approximation */
  object->left   = min(object->box->upper_x, object->box->lower_x) - halfwidth;
  object->top    = min(object->box->upper_y, object->box->lower_y) - halfwidth;
  object->right  = max(object->box->upper_x, object->box->lower_x) + halfwidth;
  object->bottom = max(object->box->upper_y, object->box->lower_y) + halfwidth;

  return TRUE;
}

/*! \brief Type instance initializer for Box
 *
 *  \par Function Description
 *  Type instance initializer for GedaBox, initializes a new empty
 *  GedaBox object by setting pointers to NULL and numbers to zero.
 *
 *  \param [in] instance The GedaBox structure being initialized,
 *  \param [in] g_class  The GedaBox class we are initializing.
 */
static void geda_box_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaBox    *box    = (GedaBox*)instance;
  GedaObject *object = &box->parent_instance;

  box->upper_x       = 0;
  box->upper_y       = 0;
  box->lower_x       = 0;
  box->lower_y       = 0;

  box->fill_options.fill_type     = default_fill_type;
  box->fill_options.fill_width    = default_fill_width;
  box->fill_options.fill_angle1   = default_fill_angle1;
  box->fill_options.fill_angle2   = default_fill_angle2;
  box->fill_options.fill_pitch1   = default_fill_pitch1;
  box->fill_options.fill_pitch2   = default_fill_pitch2;

  box->line_options.line_end      = default_line_end;
  box->line_options.line_type     = default_line_type;
  box->line_options.line_width    = default_line_width;
  box->line_options.line_space    = default_line_space;
  box->line_options.line_length   = default_line_length;

  object->box                     = box;
  object->fill_options            = &box->fill_options;
  object->line_options            = &box->line_options;
}

static void
geda_box_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_box_parent_class)->dispose(object);
}

/*! \brief Geda Box GedaObject Finalization Function
 *  \par Function Description
 *   This function invalidates the Box's markers and then chains up to
 *   the parent's finalize handler. Once invalidated, GEDA_IS_BOX will
 *   fail.
 */
static void geda_box_finalize(GObject *object)
{
  GedaObject *obj = GEDA_OBJECT(object);

  /* The object is no longer a GedaBox */
  obj->box    = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_box_parent_class)->finalize(object);
}

/*! \brief Type class initializer for Box
 *
 *  \par Function Description
 *  Type class initializer for Box. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class      The Box class we are initializing
 *  \param [in]  class_data   The Box structure associated with the class
 */
static void geda_box_class_init(void *g_class, void *class_data)
{
  GedaBoxClass    *class         = (GedaBoxClass*)g_class;
  GObjectClass    *gobject_class = G_OBJECT_CLASS(class);
  GedaObjectClass *geda_class    = GEDA_OBJECT_CLASS(class);

  geda_box_parent_class          = g_type_class_peek_parent(class);

  gobject_class->dispose         = geda_box_dispose;
  gobject_class->finalize        = geda_box_finalize;

  geda_class->bounds             = geda_box_bounds;
}

/*! \brief Function to retrieve GedaBox's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaBox Type identifier. When first called,
 *  the function registers a #GedaBox in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a GedaBox and returns
 *  the unsigned integer value. The retained value is returned on
 *  all Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with GedaBox.
 */
GedaObjectType geda_box_get_type (void)
{
  static volatile GedaObjectType geda_box_type = 0;

  if (g_once_init_enter (&geda_box_type)) {

    static const GTypeInfo info = {
      sizeof(GedaBoxClass),
      NULL,                  /* base_init           */
      NULL,                  /* base_finalize       */
      geda_box_class_init,   /* (GClassInitFunc)    */
      NULL,                  /* class_finalize      */
      NULL,                  /* class_data          */
      sizeof(GedaBox),
      0,                     /* n_preallocs         */
      geda_box_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaBox");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_box_type, type);
  }

  return geda_box_type;
}

/*! \brief Returns a pointer to a new Box object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Box object.
 *
 *  \return pointer to the new Box object.
 */
GedaObject *geda_box_new (void)
{
  GedaObject *box = g_object_new( GEDA_TYPE_BOX,
                             "type", OBJ_BOX,
                             "name", "box",
                              NULL );
  return GEDA_OBJECT(box);
}

/*! \brief Determine if object is a Geda Box GedaObject.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Box object.
 *
 *  \return boolean.
 */
bool is_a_geda_box_object (GedaBox *box)
{
  return GEDA_IS_OBJECT(box) && (((GedaObject*)box)->type == OBJ_BOX);
}
/** @} endgroup geda-box-object */