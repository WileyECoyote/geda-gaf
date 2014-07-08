/* C header                                           -*- g_types.h -*-
 * File: g_types.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2014 Ales Hvezda
 * Copyright (C) 2013-2014  Wiley Edward Hill
 *
 * Copyright (C) 2013-2014  gEDA Contributors (see ChangeLog for details)
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

#ifndef _G_TYPES_H_INCL
#define _G_TYPES_H_INCL

/* Object types */

typedef struct _GedaObject       Object;
typedef struct _GedaPage         Page;
typedef struct _GedaToplevel     GedaToplevel;
typedef struct _GedaArc          Arc;
typedef struct _GedaBox          Box;
typedef struct _GedaBus          Bus;
typedef struct _GedaCircle       Circle;
typedef struct _GedaComplex      Complex;
typedef struct _GedaLine         Line;
typedef struct _GedaNet          Net;
typedef struct _GedaPath         Path;
typedef struct _GedaPicture      Picture;
typedef struct _GedaPin          Pin;
typedef struct _GedaText         Text;


#endif
