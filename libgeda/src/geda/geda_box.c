/* -*- geda_box.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2014 Ales Hvezda
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
 * 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 18, 2013
 */
#include <config.h>

#include "libgeda_priv.h"

static GObjectClass *geda_box_parent_class = NULL;

/*! \brief Get Box bounding rectangle in WORLD coordinates.
 *
 *  \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B>
 *  parameters to the boundings of the box object described in <B>*box</B>
 *  in world units.
 *
 *  \param [in]  object     Box Object to read coordinates from.
 */
int
geda_box_bounds(Object *object)
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
 *  Type instance initializer for Box, initializes a new empty
 *  Box object by setting pointers to NULL and numbers to zero,
 *  the box PID variable is set to the next box index.
 *
 *  \param [in] instance The Box structure being initialized,
 *  \param [in]  g_class The Box class we are initializing.
 */
static void geda_box_instance_init(GTypeInstance *instance, void *g_class)
{
  Box    *box       = (Box*)instance;
  Object *object    = &box->parent_instance;

  box->upper_x      = 0;
  box->upper_y      = 0;
  box->lower_x      = 0;
  box->lower_y      = 0;

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

  box->head_marker                = GEDA_TYPE_BOX;
  box->tail_marker                = box->head_marker;
}

static void
geda_box_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_box_parent_class)->dispose(object);
}

/*! \brief Geda Box Object Finalization Function
 *  \par Function Description
 *   This function invalidates the Box's markers and then chains up to
 *   the parent's finalize handler. Once invalidated, GEDA_IS_BOX will
 *   fail.
 */
static void geda_box_finalize(GObject *object)
{
  Box *box = GEDA_BOX(object);

  /* The object is no longer a GedaBox */
  box->head_marker = 1;
  box->tail_marker = 0;

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
  BoxClass     *class          = (BoxClass*)g_class;
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );
  ObjectClass  *object_class   = GEDA_OBJECT_CLASS( class );

  geda_box_parent_class        = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_box_dispose;
  gobject_class->finalize      = geda_box_finalize;

  object_class->bounds         = geda_box_bounds;
}

/*! \brief Function to retrieve Box GedaType identifier.
 *
 *  \par Function Description
 *  Function to retrieve Box's Type identifier. On first call, the
 *  function registers the Box in the GedaType system. Subsequently
 *  the function returns the saved value from its first execution.
 *
 *  \return GedaType identifier associated with Box.
 */
GedaType geda_box_get_type(void)
{
  static GedaType type = 0;
  if (type == 0) {

    static const GTypeInfo info = {
      sizeof (BoxClass),
      NULL,                            // base_init
      NULL,                            // base_finalize
      geda_box_class_init,             // class_init
      NULL,                            // class_finalize
      NULL,                            // class_data
      sizeof(Box),
      0,                               // n_preallocs
      geda_box_instance_init                    // instance_init
    };
    type = g_type_register_static (GEDA_TYPE_OBJECT, "Box", &info, 0);
  }
  return type;
}

/*! \brief Returns a pointer to a new Box object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Box object.
 *
 *  \return pointer to the new Box object.
 */
Object *geda_box_new (void)
{
  Object *box = g_object_new( GEDA_TYPE_BOX,
                             "type", OBJ_BOX,
                             "name", "box",
                              NULL );
  return GEDA_OBJECT(box);
}

/*! \brief Determine if object is a Geda Box Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Box object.
 *
 *  \return boolean.
 */
bool is_a_geda_box_object (Box *box)
{
  return GEDA_IS_OBJECT(box) && (GEDA_TYPE_BOX == (box->head_marker & box->tail_marker));
}
