/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_arc.h
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
/*! \class Arc geda_arc.h "libgeda/geda_arc.h"
 *  \brief GedaType for GedaArc Objects.
 *
 *  GedaArc is a derivative of the GedaObject class specialized
 *  for representation and manipulation of Arc object data.
 */

#ifndef __GEDA_ARC_H__
#define __GEDA_ARC_H__

#if defined(__LP64__) || defined(_LP64)
# define GedaArcType unsigned long
#else
# define GedaArcType unsigned int
#endif

#define GEDA_TYPE_ARC            (geda_arc_get_type())
#define GEDA_ARC(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_ARC, Arc))
#define GEDA_ARC_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_ARC, ArcClass))
#define GEDA_IS_ARC(obj)         (is_a_geda_arc_object((Arc*)obj))
#define GEDA_IS_ARC_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_ARC))
#define GEDA_ARC_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_ARC, ArcClass))

typedef struct _GedaArcClass ArcClass;

struct _GedaArcClass {
  ObjectClass parent_class;
};

struct _GedaArc {
  Object parent_instance;

  GedaArcType head_marker;       /* structure type signature */

  int       x;
  int       y;

  int       width;
  int       height;

  int       start_angle;
  int       arc_sweep;

  FILL_OPTIONS  fill_options;
  LINE_OPTIONS  line_options;

  GedaArcType tail_marker;       /* structure type signature */
};

#ifdef __cplusplus
extern "C" {
#endif

GedaArcType    geda_arc_get_type        (void);
bool           is_a_geda_arc_object     (Arc *object);

Object        *geda_arc_new             (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#undef GedaArcType

#endif /* __GEDA_ARC_H__ */
