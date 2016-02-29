/* -*- geda_arc.c -*-
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

/*! \file geda_arc.c
 *  \brief Geda Arc Object Class Module
 */

/** \defgroup geda-arc-object Geda Arc Object
 * @{
 * \brief Implmentation of #GedaArc Class
 * \par
 *  A Geda Arc Object is a graphical object, do not involve electrical
 *  interconnections. Arcs have line-type and fill-type properties
 *  and are derived from the GedaObject base class.
 *
 * \class GedaArc geda_arc.h "include/libgeda/geda_arc.h"
 * \implements geda-object
 */

#include <config.h>

#include <libgeda_priv.h>
#include <math.h>

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

  radius      = object->arc->width / 2;
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
    } else {
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

  arc->width        = 0;
  arc->height       = 0;

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
  GedaArcClass    *class         = (GedaArcClass*)g_class;
  GObjectClass    *gobject_class = G_OBJECT_CLASS(class);
  GedaObjectClass *geda_class    = GEDA_OBJECT_CLASS(class);

  geda_arc_parent_class          = g_type_class_peek_parent(class);

  gobject_class->dispose         = geda_arc_dispose;
  gobject_class->finalize        = geda_arc_finalize;

  geda_class->bounds             = geda_arc_bounds;
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

/*! \brief Determine if object is a Geda Arc Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Arc object.
 *
 *  \return boolean.
 */
bool is_a_geda_arc_object (GedaArc *arc)
{
  return GEDA_IS_OBJECT(arc) && (((GedaObject*)arc)->type == OBJ_ARC);
}
/** @} endgroup geda-arc-object */
