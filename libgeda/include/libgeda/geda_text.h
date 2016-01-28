/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: geda_text.h
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
/*! \class Text geda_text.h "libgeda/geda_text.h"
 *  \brief GedaType for GedaText Objects.
 *
 *  GedaText is a derivative of the GedaObject class specialized
 *  for representation and manipulation of Text object data. When
 *  the containing string data includes an "=" character, ASCII
 *  0x3D, adjoining non-space characters, the GedaText object is
 *  considered a special type of object and is interchangeably
 *  referred to as an "Attribute" Object.
 */
#ifndef __GEDA_TEXT_H__
#define __GEDA_TEXT_H__

#if defined(__LP64__) || defined(_LP64)
# define GedaTextType unsigned long
#else
# define GedaTextType unsigned int
#endif

#define GEDA_TYPE_TEXT            (geda_text_get_type())
#define GEDA_TEXT(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GEDA_TYPE_TEXT, Text))
#define GEDA_TEXT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),  GEDA_TYPE_TEXT, TextClass))
#define GEDA_IS_TEXT(obj)         (is_a_geda_text_object((Text*)obj))
#define GEDA_IS_TEXT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass),  GEDA_TYPE_TEXT))
#define GEDA_TEXT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),  GEDA_TYPE_TEXT, TextClass))

typedef struct _GedaTextClass TextClass;

struct _GedaTextClass {
  ObjectClass parent_class;
};

struct _GedaText {

  Object parent_instance;

  int x;                     /* world origin */
  int y;

  char *string;              /* text stuff */
  char *disp_string;
  int   length;
  int   size;
  int   alignment;
  int   angle;

  int    font_text_size;     /* used only with fonts defs */
  GList *font_prim_objs;     /* used only with fonts defs */

  /* Callback function for calculating text bounds */
  RenderedBoundsFunc rendered_text_bounds_func;
  void *rendered_text_bounds_data;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaTextType geda_text_get_type        (void) GEDA_CONST;
bool         is_a_geda_text_object     (Text *object);

Object      *geda_text_new             (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* __GEDA_TEXT_H__ */
