/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_circle.h
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
/*! \class Circle geda_circle.h "libgeda/geda_circle.h"
 *  \brief GedaType for GedaCircle Objects.
 *
 *  GedaCircle is a derivative of the GedaObject class specialized
 *  for representation and manipulation of Circle object data.
 */
#ifndef __GEDA_CIRCLE_H__
#define __GEDA_CIRCLE_H__

#define GEDA_TYPE_CIRCLE            (geda_circle_get_type())
#define GEDA_CIRCLE(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_CIRCLE, GedaCircle))
#define GEDA_CIRCLE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_CIRCLE, GedaCircleClass))
#define GEDA_IS_CIRCLE(obj)         (is_a_geda_circle((GedaCircle*)(obj)))
#define GEDA_IS_CIRCLE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_CIRCLE))
#define GEDA_CIRCLE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_CIRCLE, GedaCircleClass))

typedef struct _GedaCircleClass GedaCircleClass;

struct _GedaCircleClass {
  GedaObjectClass parent_class;
};

struct _GedaCircle {

  GedaObject parent_instance;

  int center_x;
  int center_y;
  int radius;

  FILL_OPTIONS fill_options;
  LINE_OPTIONS line_options;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaObjectType geda_circle_get_type         (void);
bool           is_a_geda_circle             (const GedaCircle *circle);
GedaObject    *geda_circle_new              (void);

int            geda_circle_get_center_x     (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_center_y     (const GedaCircle *circle) WARN_UNUSED;

int            geda_circle_get_end_cap      (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_fill_angle1  (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_fill_angle2  (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_fill_pitch1  (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_fill_pitch2  (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_fill_type    (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_fill_width   (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_line_length  (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_line_space   (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_line_type    (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_line_width   (const GedaCircle *circle) WARN_UNUSED;
int            geda_circle_get_radius       (const GedaCircle *circle) WARN_UNUSED;

void           geda_circle_set_center_x     (GedaCircle *circle, int x);
void           geda_circle_set_center_y     (GedaCircle *circle, int y);

void           geda_circle_set_end_cap      (GedaCircle *circle, int cap);
void           geda_circle_set_fill_angle1  (GedaCircle *circle, int angle);
void           geda_circle_set_fill_angle2  (GedaCircle *circle, int angle);
void           geda_circle_set_fill_pitch1  (GedaCircle *circle, int pitch);
void           geda_circle_set_fill_pitch2  (GedaCircle *circle, int pitch);
void           geda_circle_set_fill_type    (GedaCircle *circle, int type);
void           geda_circle_set_fill_width   (GedaCircle *circle, int width);
void           geda_circle_set_line_length  (GedaCircle *circle, int length);
void           geda_circle_set_line_space   (GedaCircle *circle, int space);
void           geda_circle_set_line_type    (GedaCircle *circle, int type);
void           geda_circle_set_line_width   (GedaCircle *circle, int width);
void           geda_circle_set_radius       (GedaCircle *circle, int r);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_CIRCLE_H__ */
