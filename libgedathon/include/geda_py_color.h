/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*!
 * \file geda_py_color.h
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

#ifndef __GEDA_PY_COLOR_H__
#define __GEDA_PY_COLOR_H__

#define RGB_NOCOLOR   {0xff, 0xff, 0xff, 0xff}
#define RGB_BLACK     {0x00, 0x00, 0x00, 0xff}
#define RGB_BLUE      {0x00, 0x00, 0xff, 0xff}
#define RGB_CYAN      {0x00, 0xff, 0xff, 0xff} /* aqua */
#define RGB_GRAY      {0x88, 0x88, 0x88, 0xff}
#define RGB_GREY      {0x88, 0x88, 0x88, 0xff}
#define RGB_GREEN     {0x00, 0xff, 0x00, 0xff}
#define RGB_MAGENTA   {0xff, 0x00, 0xff, 0xff} /* fuchsia */
#define RGB_ORANGE    {0xff, 0xa5, 0xff, 0xff}
#define RGB_RED       {0xff, 0x00, 0x00, 0xff}
#define RGB_YELLOW    {0xff, 0xff, 0x00, 0xff}
#define RGB_WHITE     {0xff, 0xff, 0xff, 0xff}
#define RGB_ENDMAP    {0x00, 0x00, 0x00, 0x00}

#define ATTRIBUTE_COLOR        RGB_RED
#define BUS_COLOR              RGB_BLUE
#define GRAPHIC_COLOR          RGB_GREEN
#define JUNCTION_COLOR         RGB_ORANGE
#define LOCK_COLOR             RGB_GREY
#define LOGIC_BUBBLE_COLOR     RGB_CYAN
#define NET_COLOR              RGB_BLUE
#define TEXT_COLOR             RGB_YELLOW
#define PIN_COLOR              RGB_BLUE

#define DEFAULT_ARC_COLOR      GRAPHIC_COLOR
#define DEFAULT_BOX_COLOR      GRAPHIC_COLOR
#define DEFAULT_BUS_COLOR      BUS_COLOR
#define DEFAULT_NET_COLOR      NET_COLOR
#define DEFAULT_TEXT_COLOR     TEXT_COLOR
#define DEFAULT_PIN_COLOR      PIN_COLOR
#define DEFAULT_LINE_COLOR     GRAPHIC_COLOR
#define DEFAULT_CIRCLE_COLOR   GRAPHIC_COLOR
#define DEFAULT_PATH_COLOR     GRAPHIC_COLOR

#define SET_COLOR_MEMBER(m) G_STMT_START {      \
  if (PyInt_Check(m))                           \
    rgb.m = (double) PyInt_AS_LONG(m) / 255.0;  \
  else if (PyFloat_Check(m))                    \
    rgb.m = PyFloat_AS_DOUBLE(m);               \
  else {                                        \
    PyErr_SetString(PyExc_TypeError,            \
            #m " must be an int or a float");   \
    return -1;                                  \
    }                                           \
} G_STMT_END

#endif
