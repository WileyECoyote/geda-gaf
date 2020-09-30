/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_line.h
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
/*! \class Line geda_line.h "libgeda/geda_line.h"
 *  \brief GedaType for GedaLine Objects.
 *
 *  GedaLine is a derivative of the GedaObject class specialized
 *  for representation and manipulation of linear object data.
 */
#ifndef __GEDA_LINE_H__
#define __GEDA_LINE_H__

#define GEDA_TYPE_LINE            (geda_line_get_type())
#define GEDA_LINE(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_LINE, GedaLine))
#define GEDA_LINE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_LINE, GedaLineClass))
#define GEDA_IS_LINE(obj)         (is_a_geda_line((GedaLine*)(obj)))
#define GEDA_IS_LINE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_LINE))
#define GEDA_LINE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_LINE, GedaLineClass))

typedef struct _GedaLineClass GedaLineClass;

struct _GedaLineClass {
  GedaObjectClass parent_class;
  void (*finalize) (GObject *object);
};

struct _GedaLine {

  GedaObject parent_instance;

  int x[2];
  int y[2];

  LINE_OPTIONS  line_options;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaObjectType geda_line_get_type           (void) GEDA_CONST;
bool           is_a_geda_line               (const GedaLine *line);

GedaObject    *geda_line_new                (void);

int            geda_line_get_end_cap        (const GedaLine *line) WARN_UNUSED;
int            geda_line_get_line_length    (const GedaLine *line) WARN_UNUSED;
int            geda_line_get_line_space     (const GedaLine *line) WARN_UNUSED;
int            geda_line_get_line_type      (const GedaLine *line) WARN_UNUSED;
int            geda_line_get_line_width     (const GedaLine *line) WARN_UNUSED;

int            geda_line_get_x1             (const GedaLine *line) WARN_UNUSED;
int            geda_line_get_x2             (const GedaLine *line) WARN_UNUSED;
int            geda_line_get_y1             (const GedaLine *line) WARN_UNUSED;
int            geda_line_get_y2             (const GedaLine *line) WARN_UNUSED;

void           geda_line_set_end_cap        (GedaLine *line, int cap);
void           geda_line_set_line_length    (GedaLine *line, int length);
void           geda_line_set_line_space     (GedaLine *line, int space);
void           geda_line_set_line_type      (GedaLine *line, int type);
void           geda_line_set_line_width     (GedaLine *line, int width);

void           geda_line_set_x1             (GedaLine *line, int x);
void           geda_line_set_x2             (GedaLine *line, int x);
void           geda_line_set_y1             (GedaLine *line, int y);
void           geda_line_set_y2             (GedaLine *line, int y);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_LINE_H__ */
