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
/*! \class GedaArc geda_arc.h "libgeda/geda_arc.h"
 *  \brief GedaType for GedaArc Objects.
 *
 *  GedaArc is a derivative of the GedaObject class specialized
 *  for representation and manipulation of GedaArc object data.
 */

#ifndef __GEDA_ARC_H__
#define __GEDA_ARC_H__

#define GEDA_TYPE_ARC            (geda_arc_get_type())
#define GEDA_ARC(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_ARC, GedaArc))
#define GEDA_ARC_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_ARC, ArcClass))
#define GEDA_IS_ARC(obj)         (is_a_geda_arc((GedaArc*)obj))
#define GEDA_IS_ARC_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_ARC))
#define GEDA_ARC_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_ARC, ArcClass))

typedef struct _GedaArcClass GedaArcClass;

struct _GedaArcClass {
  GedaObjectClass parent_class;
};

struct _GedaArc {

  GedaObject parent_instance;

  int x;
  int y;

  int radius;
  int start_angle;
  int arc_sweep;

  FILL_OPTIONS fill_options;
  LINE_OPTIONS line_options;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaObjectType geda_arc_get_type            (void);
bool           is_a_geda_arc                (const GedaArc *arc);

GedaObject    *geda_arc_new                 (void);

int            geda_arc_get_arc_sweep       (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_center_x        (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_center_y        (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_end_cap         (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_fill_angle1     (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_fill_angle2     (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_fill_pitch1     (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_fill_pitch2     (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_fill_type       (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_fill_width      (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_line_length     (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_line_space      (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_line_type       (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_line_width      (const GedaArc *arc) WARN_UNUSED;
bool           geda_arc_get_position        (const GedaArc *arc, int *x, int *y);
int            geda_arc_get_radius          (const GedaArc *arc) WARN_UNUSED;
int            geda_arc_get_start_angle     (const GedaArc *arc) WARN_UNUSED;

void           geda_arc_set_arc_sweep       (GedaArc *arc, int sweep);
void           geda_arc_set_center_x        (GedaArc *arc, int x);
void           geda_arc_set_center_y        (GedaArc *arc, int y);
void           geda_arc_set_end_cap         (GedaArc *arc, int cap);
void           geda_arc_set_fill_angle1     (GedaArc *arc, int angle);
void           geda_arc_set_fill_angle2     (GedaArc *arc, int angle);
void           geda_arc_set_fill_pitch1     (GedaArc *arc, int pitch);
void           geda_arc_set_fill_pitch2     (GedaArc *arc, int pitch);
void           geda_arc_set_fill_type       (GedaArc *arc, int type);
void           geda_arc_set_fill_width      (GedaArc *arc, int width);
void           geda_arc_set_line_length     (GedaArc *arc, int length);
void           geda_arc_set_line_space      (GedaArc *arc, int space);
void           geda_arc_set_line_type       (GedaArc *arc, int type);
void           geda_arc_set_line_width      (GedaArc *arc, int width);
void           geda_arc_set_position        (GedaArc *arc, int x, int y);
void           geda_arc_set_radius          (GedaArc *arc, int radius);
void           geda_arc_set_start_angle     (GedaArc *arc, int angle);
bool           geda_arc_within_sweep        (const GedaArc *arc, int x, int y) WARN_UNUSED;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_ARC_H__ */
