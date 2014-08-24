/* -*- geda_arc.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2014 Ales Hvezda
 * Copyright (C) 2013-2014 Wiley Edward Hill
 *
 * Copyright (C) 2013-2014 gEDA Contributors (see ChangeLog for details)
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

/*! \file geda_arc.c
 *  \brief Geda Arc Object Class derived from the GedaObject Class
 */
/** \defgroup geda-arc-object Geda Arc Object
 *  @{
 */
/*! \class Arc geda_arc.h "include/libgeda/geda_arc.h"
 *  \implements geda-object
 *  \brief This is an implementaion class for GEDA Arc Objects.
 *  A Geda Arc Object is a graphical object, do not involve electrical
 *  interconnections. Arcs have line-type and fill-type properties.
 */

#include <config.h>

#include "libgeda_priv.h"
#include <math.h>

G_DEFINE_TYPE (Arc, geda_arc, GEDA_TYPE_OBJECT);

/*! \brief Geda Arc Bounds
 *  \par Function Description
 *  This function calculates the smallest rectangle the arc can be drawn into.
 *  The <B>Object</B> pointed by object is assumed to be an arc.
 *  The <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B> pointed integers define
 *  this rectangle at the end of the function. It is expressed in world units.
 *  The process is divided into two steps : the first step is to calculate the
 *  coordinates of the two ends of the arc and the coordinates of the center.
 *  They forms a first rectangle but (depending on the start angle and the
 *  sweep of the arc) not the right.
 *
 *  \param [in]  object
 */
int
geda_arc_bounds(Object *object)
{
  int x1, y1, x2, y2, x3, y3;
  int left, top, right, bottom;
  int radius, start_angle, end_angle;
  int i, angle;
  int halfwidth;

  g_return_val_if_fail (GEDA_IS_ARC(object), FALSE);

  halfwidth = object->line_options->line_width / 2;

  radius      = object->arc->width / 2;
  start_angle = object->arc->start_angle;
  end_angle   = object->arc->end_angle;

  x1 = object->arc->x;
  y1 = object->arc->y;
  x2 = x1 + radius * cos(start_angle * M_PI / 180);
  y2 = y1 + radius * sin(start_angle * M_PI / 180);
  x3 = x1 + radius * cos((start_angle + end_angle) * M_PI / 180);
  y3 = y1 + radius * sin((start_angle + end_angle) * M_PI / 180);

  left   = (x1 < x2) ? ((x1 < x3) ? x1 : x3) : ((x2 < x3) ? x2 : x3);
  right  = (x1 > x2) ? ((x1 > x3) ? x1 : x3) : ((x2 > x3) ? x2 : x3);
  bottom = (y1 > y2) ? ((y1 > y3) ? y1 : y3) : ((y2 > y3) ? y2 : y3);
  top    = (y1 < y2) ? ((y1 < y3) ? y1 : y3) : ((y2 < y3) ? y2 : y3);

  /*! \note
   *  The previous rectangle is extended to the final one
   *  by checking whether the arc is over a main axis (vertical or horizontal).
   *  If so, the rectangle is extended in these directions.
   *
   *  In the mirror mode, the sweep angle is negativ. To get a
   *  CCW arc before this calculation we have to move the
   *  start angle to the end angle and reverse the sweep angle.
   */
  if (end_angle < 0) {
    start_angle = (start_angle + end_angle + 360) % 360;
    end_angle   = -end_angle;
  }
  angle = ((int) (start_angle / 90)) * 90;
  for(i = 0; i < 4; i++) {
    angle = angle + 90;
    if(angle < start_angle + end_angle) {
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

/*! \brief Type instance initialiser for Arc
 *
 *  \par Function Description
 *  Type instance initialiser for Arc, initializes a new empty
 *  Arc object by setting pointers to NULL and numbers to zero,
 *  the arc PID variable is set to the next arc index.
 *
 *  \param [in] arc The Arc instance being initialising.
 */
static void geda_arc_init(Arc *arc)
{
  Object *object    = &arc->parent_instance;

  arc->x            = 0;
  arc->y            = 0;

  arc->width        = 0;
  arc->height       = 0;

  arc->start_angle  = 0;
  arc->end_angle    = 0;

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

  arc->head_marker                = GEDA_TYPE_ARC;
  arc->tail_marker                = arc->head_marker;
}

static void
geda_arc_dispose(GObject *object)
{

  G_OBJECT_CLASS(geda_arc_parent_class)->dispose(object);

}

/*! \brief Geda Arc Object Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references
 *   and releases the memory allocated to the given Arc
 *   data structure and then chain up to the parent's finalize
 *   handler.
 */
static void geda_arc_finalize(GObject *object)
{
  GEDA_OBJECT_CLASS( geda_arc_parent_class )->finalize(object);
}

/*! \brief Type class initialiser for Arc
 *
 *  \par Function Description
 *  Type class initialiser for Arc. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  class       The Arc we are initialising
 */
static void geda_arc_class_init(ArcClass *class)
{
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );
  ObjectClass  *object_class   = GEDA_OBJECT_CLASS( class );

  geda_arc_parent_class        = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_arc_dispose;
  gobject_class->finalize      = geda_arc_finalize;

  object_class->bounds         = geda_arc_bounds;
}

/*! \brief Returns a pointer to a new Arc object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Arc object.
 *
 *  \return pointer to the new Arc object.
 */
Object *geda_arc_new (void)
{
  Object *arc = g_object_new( GEDA_TYPE_ARC,
                             "type", OBJ_ARC,
                             "name", "arc",
                              NULL );
  return GEDA_OBJECT(arc);
}

#define ARC_MARKER(target) (unsigned int)(arc + offsetof(Arc, target))
#define ARC_MARKERS (ARC_MARKER(head_marker) & ARC_MARKER(tail_marker))

/*! \brief Determine if object is a Geda Arc Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Arc object.
 *
 *  \return boolean.
 */
bool is_a_geda_arc_object (Arc *arc)
{
  return GEDA_IS_OBJECT(arc) && (GEDA_TYPE_ARC == (arc->head_marker & arc->tail_marker));
}
/** @} endgroup geda-arc-object */
