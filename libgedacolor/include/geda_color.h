/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_color.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#ifndef COLOR_STRUCT_H
#define COLOR_STRUCT_H

/* Structures for grapical shapes and objects */
typedef struct st_object_color  COLOR;

/* Structures for user interface objects */
typedef struct st_color         edaColor;
typedef struct st_color_element ColorElement;

/** ********** structures for grapical shapes and objects ***********/

/*------------------------------------------------------------------
 *                            COLOR
 *------------------------------------------------------------------*/
struct st_color {
    double r;
    double g;
    double b;
    double a;
};

struct st_object_color {
  unsigned char r, g, b, a;
  int enabled;
};

/* Color element structure */
struct st_color_element {
  unsigned char  r, g, b;
  const char *name;
};


#endif /* COLOR_STRUCT_H */
