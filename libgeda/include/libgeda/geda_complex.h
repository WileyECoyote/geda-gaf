/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_complex.h
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
#define GEDA_COMPLEX(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_COMPLEX, GedaComplex))
#define GEDA_COMPLEX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_COMPLEX, GedaComplexClass))
#define GEDA_IS_COMPLEX(obj)         (is_a_geda_complex((GedaComplex*)(obj)))
#define GEDA_IS_COMPLEX_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_COMPLEX))
#define GEDA_COMPLEX_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_COMPLEX, GedaComplexClass))

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _GedaGedaComplexClass GedaComplexClass;

struct _GedaGedaComplexClass {
  GedaObjectClass parent_class;
};

struct _GedaComplex {

  GedaObject parent_instance;  /* Pointer to _GedaObject */

  char  *filename;             /* Component Library Symbol name */
  bool   is_embedded;          /* is embedded component? */

  int    x;                    /* insertion coordinates */
  int    y;

  int    angle;                /* orientation in degrees */

  bool   mirror;

  GList *pin_objs;             /* A list of pins belonging to this complex */
  GList *prim_objs;            /* Primitive objects which make up the complex */
};

GedaObjectType geda_complex_get_type        (void) GEDA_CONST;
bool           is_a_geda_complex            (const GedaComplex *complex);

GedaObject    *geda_complex_new             (void);
bool           geda_complex_append          (GedaComplex *complex, GedaObject *object);

int            geda_complex_get_angle       (const GedaComplex *complex) WARN_UNUSED;
char          *geda_complex_get_filename    (const GedaComplex *complex) WARN_UNUSED;
bool           geda_complex_get_is_embedded (const GedaComplex *complex) WARN_UNUSED;
bool           geda_complex_get_is_mirror   (const GedaComplex *complex) WARN_UNUSED;
GList         *geda_complex_get_pin_objs    (const GedaComplex *complex) WARN_UNUSED;
GList         *geda_complex_get_prim_objs   (const GedaComplex *complex) WARN_UNUSED;
int            geda_complex_get_x           (const GedaComplex *complex) WARN_UNUSED;
int            geda_complex_get_y           (const GedaComplex *complex) WARN_UNUSED;

void           geda_complex_set_angle       (GedaComplex *complex, int angle);
void           geda_complex_set_filename    (GedaComplex *complex, const char *filename);
void           geda_complex_set_is_embedded (GedaComplex *complex, bool is_embedded);
void           geda_complex_set_is_mirror   (GedaComplex *complex, bool is_mirror);
void           geda_complex_set_pin_objs    (GedaComplex *complex, GList *pin_objs);
void           geda_complex_set_prim_objs   (GedaComplex *complex, GList *prim_objs);
void           geda_complex_set_x           (GedaComplex *complex, int x);
void           geda_complex_set_y           (GedaComplex *complex, int y);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_COMPLEX_H__ */
