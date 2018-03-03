/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: o_types.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2015 Ales Hvezda
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
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */

#ifndef _O_TYPES_H_INCL
#define _O_TYPES_H_INCL

/* Object types */
#define OBJ_LINE        'L'
#define OBJ_PATH        'H'
#define OBJ_BOX         'B'
#define OBJ_PICTURE     'G'
#define OBJ_CIRCLE      'V'
#define OBJ_NET         'N'
#define OBJ_BUS         'U'
#define OBJ_COMPLEX     'C'
#define OBJ_TEXT        'T'
#define OBJ_PIN         'P'
#define OBJ_ARC         'A'
#define OBJ_PLACEHOLDER 'X'  /* added 1.19.2005 by SDB to prevent
                              * deletion of unfound symbol files */

/* Schematic and Sysmbol file delimiter characters used in
 * o_object.c while loading and parsing files, also used in
 * geda_attrib_object_read()
 */
#define STARTATTACH_ATTR	'{'
#define ENDATTACH_ATTR		'}'
#define START_EMBEDDED		'['
#define END_EMBEDDED		']'

/* font stuff */
#define INFO_FONT         'F'
#define VERSION_CHAR      'v'

/* misc stuff */
#define COMMENT         '#'

#endif
