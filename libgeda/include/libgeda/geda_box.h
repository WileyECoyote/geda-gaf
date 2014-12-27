/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_box.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Ales Hvezda
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
 * 02110-1301 USA
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
#define GEDA_BOX(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_BOX, Box))
#define GEDA_BOX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_BOX, BoxClass))
#define GEDA_IS_BOX(obj)         (is_a_geda_box_object((Box*)obj))
#define GEDA_IS_BOX_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_BOX))
#define GEDA_BOX_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_BOX, BoxClass))

#define BOX_MARKER(target) (unsigned long int)(box + offsetof(Box, target))
#define BOX_MARKERS (BOX_MARKER(head_marker) & BOX_MARKER(tail_marker))

G_BEGIN_DECLS

typedef struct _GedaBoxClass BoxClass;

struct _GedaBoxClass {
  ObjectClass parent_class;
};

struct _GedaBox {

  Object parent_instance;

  unsigned int head_marker;            /* structure type signature */

  /* upper is considered the origin */
  int          upper_x;
  int          upper_y;
  int          lower_x;
  int          lower_y;

  LINE_OPTIONS line_options;
  FILL_OPTIONS fill_options;

  unsigned int tail_marker;       /* structure type signature */

};

GedaType   geda_box_get_type        (void);
bool     is_a_geda_box_object     (Box *object);
Object  *geda_box_new             (void);


G_END_DECLS
#endif /* __GEDA_BOX_H__ */
