/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_box.h
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
/*! \class Box geda_box.h "libgeda/geda_box.h"
 *  \brief GedaType for GedaBox Objects.
 *
 *  GedaBox is a derivative of the GedaObject class specialized
 *  for representation and manipulation of Box object data.
 */
#ifndef __GEDA_BOX_H__
#define __GEDA_BOX_H__

#define GEDA_TYPE_BOX            (geda_box_get_type())
#define GEDA_BOX(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_BOX, GedaBox))
#define GEDA_BOX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_BOX, GedaBoxClass))
#define GEDA_IS_BOX(obj)         (is_a_geda_box((GedaBox*)(obj)))
#define GEDA_IS_BOX_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_BOX))
#define GEDA_BOX_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_BOX, GedaBoxClass))

typedef struct _GedaGedaBoxClass GedaBoxClass;

struct _GedaGedaBoxClass {
  GedaObjectClass parent_class;
};

struct _GedaBox {

  GedaObject parent_instance;

  /* upper is considered the origin */
  int          upper_x;
  int          upper_y;
  int          lower_x;
  int          lower_y;

  LINE_OPTIONS line_options;
  FILL_OPTIONS fill_options;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaObjectType geda_box_get_type            (void) GEDA_CONST;
bool           is_a_geda_box                (const GedaBox *box);
GedaObject    *geda_box_new                 (void);

int            geda_box_get_end_cap         (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_fill_angle1     (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_fill_angle2     (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_fill_pitch1     (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_fill_pitch2     (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_fill_type       (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_fill_width      (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_line_length     (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_line_space      (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_line_type       (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_line_width      (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_lower_x         (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_lower_y         (const GedaBox *box) WARN_UNUSED;
bool           geda_box_get_position        (const GedaBox *box, int *x, int *y);
int            geda_box_get_upper_x         (const GedaBox *box) WARN_UNUSED;
int            geda_box_get_upper_y         (const GedaBox *box) WARN_UNUSED;

void           geda_box_set_end_cap         (GedaBox *box, int cap);
void           geda_box_set_fill_angle1     (GedaBox *box, int angle);
void           geda_box_set_fill_angle2     (GedaBox *box, int angle);
void           geda_box_set_fill_pitch1     (GedaBox *box, int pitch);
void           geda_box_set_fill_pitch2     (GedaBox *box, int pitch);
void           geda_box_set_fill_type       (GedaBox *box, int type);
void           geda_box_set_fill_width      (GedaBox *box, int width);
void           geda_box_set_line_length     (GedaBox *box, int length);
void           geda_box_set_line_space      (GedaBox *box, int space);
void           geda_box_set_line_type       (GedaBox *box, int type);
void           geda_box_set_line_width      (GedaBox *box, int width);
void           geda_box_set_lower_x         (GedaBox *box, int x);
void           geda_box_set_lower_y         (GedaBox *box, int y);
void           geda_box_set_upper_x         (GedaBox *box, int x);
void           geda_box_set_upper_y         (GedaBox *box, int y);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_BOX_H__ */
