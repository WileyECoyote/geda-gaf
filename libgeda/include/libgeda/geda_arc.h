/* C header                                           -*- geda_arc.h -*-
 * file: geda_arc.h
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
#ifndef __GEDA_ARC_H__
#define __GEDA_ARC_H__

#define GEDA_TYPE_ARC            (geda_arc_get_type())
#define GEDA_ARC(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_ARC, Arc))
#define GEDA_ARC_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_ARC, ArcClass))
#define GEDA_IS_ARC(obj)         (is_a_geda_arc_object((Arc*)obj))
#define GEDA_IS_ARC_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_ARC))
#define GEDA_ARC_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_ARC, ArcClass))

G_BEGIN_DECLS

typedef struct _GedaArcClass ArcClass;

struct _GedaArcClass {
  ObjectClass parent_class;
};

struct _GedaArc {
  Object parent_instance;

  unsigned int head_marker;            /* structure type signature */

  int       x;
  int       y;

  int       width;
  int       height;

  int       start_angle;
  int       arc_sweep;

  FILL_OPTIONS  fill_options;
  LINE_OPTIONS  line_options;

  unsigned int tail_marker; /* structure type signature */
};

unsigned int   geda_arc_get_type        (void);
bool           is_a_geda_arc_object     (Arc *object);

Object        *geda_arc_new             (void);


G_END_DECLS
#endif /* __GEDA_ARC_H__ */
