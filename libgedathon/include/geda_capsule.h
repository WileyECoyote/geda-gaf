/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*!
 * \file geda_capsule.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedathon - gEDA's Python API Extension library
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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
 *  Date Contributed: November, 17, 2013
 */

#include <Python.h>

typedef struct st_py_capsule GedaCapsule;

struct st_py_capsule {
    PyObject_HEAD
    int  sid;
    int  type;
    const char *name;
    void *object;
};

void         *GedaCapsule_GetPointer ( PyObject *py_object);
const char   *GedaCapsule_GetName    ( PyObject *py_object);
PyObject     *GedaCapsule_New        ( void *object );
PyTypeObject *GedaCapsuleClass       ( void );
void          initGedaCapsule        ( void );

