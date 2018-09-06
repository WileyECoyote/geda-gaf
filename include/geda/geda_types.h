/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_types.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
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
 */

/** \file geda_types.h
 *
 *   \defgroup geda-global-types Global Type Definitions
 * @{\par This group contains global type definitions
 *   \ingroup geda-globals
 */

#  include <stdint.h>

#ifndef __cplusplus

typedef int bool;

#ifdef TRUE
#undef TRUE
#endif

#ifdef FALSE
#undef FALSE
#endif

typedef enum { FALSE, TRUE } Boolean;

#endif

#ifndef DWORD
typedef unsigned long DWORD;
#endif

typedef int8_t   int8;
typedef int16_t  int16;
typedef int32_t  int32;
typedef int64_t  int64;

typedef uint8_t  uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;

#if defined(__LP64__) || defined(_LP64)
# define IS64BIT
#else
# define IS32BIT
#endif

#ifdef IS64BIT
typedef unsigned long GedaType;
#else
typedef unsigned int GedaType;
#endif

#ifdef IS64BIT
# define INT_TO_POINTER(u) ((void*)(long)(u))
# define POINTER_TO_INT(u) ((long)(void*)(u))
# define UINT_TO_POINTER(u) ((void*)(unsigned long)(u))
# define POINTER_TO_UINT(u) ((unsigned long)(void*)(u))
#else
# define INT_TO_POINTER(u) ((void*)(int)(u))
# define POINTER_TO_INT(u) ((int)(void*)(u))
# define UINT_TO_POINTER(u) ((void*)(unsigned int)(u))
# define POINTER_TO_UINT(u) ((unsigned int)(void*)(u))
#endif

/* In older headers this does not happen, so fix here
#ifndef OS_WIN32_NATIVE
typedef __mode_t mode_t;
#endif */

/** @} endgroup geda-global-types */
