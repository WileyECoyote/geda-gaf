/* -*- C header file: geda_system.h indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill
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
#ifndef __GEDA_SYSTEM__
#define __GEDA_SYSTEM__

/** \file geda_system.h
 *
 *   \defgroup geda-global-system Global System Defines
 * @{\par This group contains system related defines
 *   \ingroup (geda-globals)
 */
#if defined(__linux__) || defined(UNIX)

#define DIR_SEPARATOR 0x2F
#define DIR_SEPARATOR_S "/"
#define IS_DIR_SEPARATOR(c) ((c) == DIR_SEPARATOR)
#define SEARCHPATH_SEPARATOR ':'
#define SEARCHPATH_SEPARATOR_S ":"

#elif defined(OS_WIN32_NATIVE) || defined(_WIN32) || defined(__MINGW32__)

/* On Win32, the directory separator is the backslash, and the search path
 * separator is the semicolon. Note that also the (forward) slash works as
 * directory separator.
 */
#define DIR_SEPARATOR '\\'
#define DIR_SEPARATOR_S "\\"
#define IS_DIR_SEPARATOR(c) ((c) == DIR_SEPARATOR || (c) == '/')
#define SEARCHPATH_SEPARATOR ';'
#define SEARCHPATH_SEPARATOR_S ";"

#else
#error "No suitable OS defined"
#endif

/** @} endgroup geda-global-system */
#endif
