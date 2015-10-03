/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_complex.h
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
/*! \class Complex geda_complex.h "libgeda/geda_complex.h"
 *  \brief GedaType for GedaComplex Objects.
 *
 *  GedaComplex is a derivative of the GedaObject class specialized
 *  for representation and manipulation of Complex objects. Complex
 *  data is usually in separate files, and describes other objects.
 */
#ifndef __GEDA_COMPLEX_H__
#define __GEDA_COMPLEX_H__

#define GEDA_TYPE_COMPLEX            (geda_complex_get_type())
#define GEDA_COMPLEX(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_COMPLEX, Complex))
#define GEDA_COMPLEX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_COMPLEX, ComplexClass))
#define GEDA_IS_COMPLEX(obj)         (is_a_geda_complex_object((Complex*)obj))
#define GEDA_IS_COMPLEX_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_COMPLEX))
#define GEDA_COMPLEX_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_COMPLEX, ComplexClass))

BEGIN_DECLS

typedef struct _GedaComplexClass ComplexClass;

struct _GedaComplexClass {
  ObjectClass parent_class;
};

struct _GedaComplex {

  Object  parent_instance;

  unsigned int head_marker;  /* structure type signature */

  char   *filename;          /* Component Library Symbol name */
  bool    is_embedded;       /* is embedded component? */

  int x;                     /* world origin */
  int y;

  int angle;                 /* orientation in degrees */

  int mirror;

  GList *pin_objs;           /* A list of pins belonging to this complex */
  GList *prim_objs;          /* Primitive objects which make up the complex */
  unsigned int tail_marker;  /* structure type signature */
};

GedaType geda_complex_get_type    (void) GEDA_CONST;
bool     is_a_geda_complex_object (Complex *object);

Object  *geda_complex_new         (void);


END_DECLS
#endif /* __GEDA_COMPLEX_H__ */
