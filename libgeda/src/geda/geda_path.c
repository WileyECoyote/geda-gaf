/* -*- geda_path.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Ales Hvezda
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
/*! \file geda_path.c
 *  \brief Geda Path Object Class Module
 */

/** \defgroup geda-path-object Geda Path Object
 * @{
 * \brief Implmentation of #GedaPath Class
 * \par
 *  This module implements GedaPath Objects in libgeda. A GedaPath object
 *  is a graphical object that does not involve electrical interconnections.
 *  A Path object has line-type and fill-type properties and are derived
 *  from the GedaObject base class.
 *
 * \class GedaPath geda_path.h "include/libgeda/geda_path.h"
 * \implements geda-object
 */

#include "../../../config.h"

#include <libgeda_priv.h>

static GObjectClass *geda_path_parent_class = NULL;

/*! \brief Get path bounding rectangle.
 *  \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and
 *  <B>bottom</B> parameters to the boundings of the path object described
 *  in <B>*path</B> in world units.
 *
 *  \note Bounding box for bezier curves is loose because we just consider
 *        the convex hull of the curve control and end-points.
 *
 *  \param [in]  object     Path GedaObject to read coordinates from.
 */
static int
geda_path_bounds (GedaObject *object)
{
  int i;
  int found_bound = FALSE;
  int left   = 0;
  int top    = 0;
  int right  = 0;
  int bottom = 0;

  g_return_val_if_fail (GEDA_IS_PATH (object), FALSE);

  /* Find the bounds of the path region */
  for (i = 0; i < object->path->num_sections; i++) {

    PATH_SECTION *section = &object->path->sections[i];

    switch (section->code) {
      case PATH_CURVETO:
        /* Bezier curves with this construction of control points will lie
         * within the convex hull of the control and curve end points */
        left   = (found_bound) ? MIN (left,   section->x1) : section->x1;
        top    = (found_bound) ? MIN (top,    section->y1) : section->y1;
        right  = (found_bound) ? MAX (right,  section->x1) : section->x1;
        bottom = (found_bound) ? MAX (bottom, section->y1) : section->y1;
        found_bound = TRUE;
        left   = MIN (left,   section->x2);
        top    = MIN (top,    section->y2);
        right  = MAX (right,  section->x2);
        bottom = MAX (bottom, section->y2);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        left   = (found_bound) ? MIN (left,   section->x3) : section->x3;
        top    = (found_bound) ? MIN (top,    section->y3) : section->y3;
        right  = (found_bound) ? MAX (right,  section->x3) : section->x3;
        bottom = (found_bound) ? MAX (bottom, section->y3) : section->y3;
        found_bound = TRUE;
        break;
      case PATH_END:
        break;
    }
  }

  if (found_bound) {

    /* This isn't strictly correct, but a 1st order approximation */
    int halfwidth = object->line_options->line_width / 2;

    object->left   = left   - halfwidth;
    object->top    = top    - halfwidth;
    object->right  = right  + halfwidth;
    object->bottom = bottom + halfwidth;
  }
  return found_bound;
}

/*! \brief GedaType instance initializer for Path
 *
 *  \par Function Description
 *  GedaType instance initializer for Path, initializes a new empty
 *  Path object by setting pointers to NULL and numbers to zero.
 *
 *  \param [in] instance The Path structure being initialized,
 *  \param [in] g_class  The Path class we are initializing.
 */
static void geda_path_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaPath   *path                = (GedaPath*)instance;
  GedaObject *object              = &path->parent_instance;

  path->sections                  = NULL;
  path->num_sections              = 0;
  path->num_sections_max          = 0;

  path->fill_options.fill_type    = default_fill_type;
  path->fill_options.fill_width   = default_fill_width;
  path->fill_options.fill_angle1  = default_fill_angle1;
  path->fill_options.fill_angle2  = default_fill_angle2;
  path->fill_options.fill_pitch1  = default_fill_pitch1;
  path->fill_options.fill_pitch2  = default_fill_pitch2;

  path->line_options.line_end     = default_line_end;
  path->line_options.line_type    = default_line_type;
  path->line_options.line_width   = default_line_width;
  path->line_options.line_space   = default_line_space;
  path->line_options.line_length  = default_line_length;

  object->path                    = path;
  object->fill_options            = &path->fill_options;
  object->line_options            = &path->line_options;
}

static void
geda_path_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_path_parent_class)->dispose(object);
}

/*! \brief Geda Path GedaObject Finalization Function
 *  \par Function Description
 *   This function invalidates the Path's markers and then chains up to
 *   the parent's finalize handler. Once invalidated, GEDA_IS_PATH will
 *   fail.
 */
static void geda_path_finalize(GObject *object)
{
  GedaPath   *path = GEDA_PATH(object);
  GedaObject *obj  = GEDA_OBJECT(object);

  if (path->sections)
    GEDA_FREE(path->sections);

  /* The object is no longer a GedaPath */
  obj->path    = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS( geda_path_parent_class )->finalize(object);
}

/*! \brief GedaType class initializer for GedaPath
 *
 *  \par Function Description
 *  GedaType class initializer for GedaPath. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  klass       The GedaPath class we are initialising
 *  \param [in]  class_data  The Path structure associated with the class
 */
static void geda_path_class_init(void *klass, void *class_data)
{
  GedaPathClass   *class         = (GedaPathClass*)klass;
  GObjectClass    *gobject_class = (GObjectClass*)klass;
  GedaObjectClass *geda_class    = (GedaObjectClass*)klass;

  geda_path_parent_class         = g_type_class_peek_parent(class);

  gobject_class->dispose         = geda_path_dispose;
  gobject_class->finalize        = geda_path_finalize;

  geda_class->bounds             = geda_path_bounds;
}

/*! \brief Function to retrieve Path's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #Path Type identifier. When first called,
 *  the function registers a #Path in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Path and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with Path.
 */
GedaObjectType geda_path_get_type (void)
{
  static volatile GedaType geda_path_type = 0;

  if (g_once_init_enter (&geda_path_type)) {

    static const GTypeInfo info = {
      sizeof(GedaPathClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_path_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(GedaPath),
      0,                      /* n_preallocs         */
      geda_path_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaPath");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_path_type, type);
  }

  return geda_path_type;
}

/*! \brief Returns a pointer to a new Path object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Path object.
 *
 *  \return pointer to the new Path object.
 */
GedaObject *geda_path_new (void)
{
  GedaObject *path = g_object_new( GEDA_TYPE_PATH,
                              "type", OBJ_PATH,
                              "name", "path",
                               NULL );
  return GEDA_OBJECT(path);
}

/*! \brief Determine if object is a Geda Path GedaObject.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Path object.
 *
 *  \return boolean.
 */
bool is_a_geda_path (const GedaPath *path)
{
  return GEDA_IS_OBJECT(path) && (((GedaObject*)path)->type == OBJ_PATH);
}
/** @} endgroup geda-path-object */
