/* -*- C header file: geda_types.h indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

/** \file geda_types.h
 *
 *   \defgroup geda-global-types Global Type Definitions
 * @{\par This group contains global type definitions
 *   \ingroup (geda-globals)
 */

#ifndef __cplusplus

typedef int bool;

#endif

typedef unsigned char uint8;
typedef unsigned long DWORD;

#if ((ULONG_MAX) == (UINT_MAX))
# define IS32BIT
#else
# define IS64BIT
#endif

#ifdef IS64BIT
typedef DWORD GedaType;
#else
typedef unsigned int GedaType;
#endif


/** @} endgroup geda-global-types */