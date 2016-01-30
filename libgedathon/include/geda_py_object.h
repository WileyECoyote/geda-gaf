/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*!
 * \file geda_py_object.h
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

/* Note: Structures are defined in geda_py_struct.h */

void initPyGedaObject(PyObject *module);
PyTypeObject *PyGedaObjectClass(void);

void initArc(PyObject *module);
PyTypeObject *PyGedaArcClass(void);

void initBox(PyObject *module);
PyTypeObject *PyGedaBoxClass(void);

void initBus(PyObject *module);
PyTypeObject *PyGedaBusClass(void);

void initCircle(PyObject *module);
PyTypeObject *PyGedaCircleClass(void);

void initComplex(PyObject *module);
PyTypeObject *PyGedaComplexClass(void);

void initLine(PyObject *module);
PyTypeObject *PyGedaLineClass(void);

void initMissing(PyObject *module);
PyTypeObject *PyGedaMissingClass(void);

void initNet(PyObject *module);
PyTypeObject *PyGedaNetClass(void);

void initPath(PyObject *module);
PyTypeObject *PyGedaPathClass(void);

void initPicture(PyObject *module);
PyTypeObject *PyGedaPictureClass(void);

void initPin(PyObject *module);
PyTypeObject *PyGedaPinClass(void);

void initText(PyObject *module);
PyTypeObject *PyGedaTextClass(void);
