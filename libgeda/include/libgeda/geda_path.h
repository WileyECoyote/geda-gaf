/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_path.h
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
/*! \class Path geda_path.h "libgeda/geda_path.h"
 *  \brief GedaType for GedaPath Objects.
 *
 *  GedaPath is a derivative of the GedaObject class specialized
 *  for representation and manipulation of SVG Path data, which
 *  can include Bézier curves.
 */
#ifndef __GEDA_PATH_H__
#define __GEDA_PATH_H__

#define GEDA_TYPE_PATH            (geda_path_get_type())
#define GEDA_PATH(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_PATH, Path))
#define GEDA_PATH_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_PATH, PathClass))
#define GEDA_IS_PATH(obj)         (is_a_geda_path_object((Path*)obj))
#define GEDA_IS_PATH_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_PATH))
#define GEDA_PATH_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_PATH, PathClass))

BEGIN_DECLS

typedef struct _GedaPathClass PathClass;

struct _GedaPathClass {
  ObjectClass parent_class;
};

struct _GedaPath {

  Object parent_instance;

  GedaType head_marker;       /* structure type signature */

  PATH_SECTION *sections;         /* Bezier path segments  */

  int num_sections;
  int num_sections_max;

  FILL_OPTIONS fill_options;
  LINE_OPTIONS line_options;

  GedaType tail_marker;       /* structure type signature */
};

GedaType     geda_path_get_type        (void) GEDA_CONST;
bool         is_a_geda_path_object     (Path *object);

Object      *geda_path_new             (void);


END_DECLS
#endif /* __GEDA_PATH_H__ */
