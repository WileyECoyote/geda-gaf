/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*!
 * \file geda_python.h
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
#ifndef __GEDA_PYTHON_H__
#define __GEDA_PYTHON_H__

#ifndef PyMODINIT_FUNC  /* declarations for DLL import/export */
#define PyMODINIT_FUNC void
#endif

#define ALLOC_EMPTY_PY_STRING(symbol)       \
    self->symbol = PyString_FromString(""); \
    if (self->symbol == NULL) {             \
      Py_DECREF(self);                      \
      return NULL;                          \
    }

#define SWAP_PY_TMP_OBJECT(symbol)          \
  if (py_##symbol) {                        \
    tmp = self->symbol;                     \
    Py_INCREF(py_##symbol);                 \
    self->symbol = py_##symbol;             \
    Py_XDECREF(tmp);                        \
  }
  
#endif
