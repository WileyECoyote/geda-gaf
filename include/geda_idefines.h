/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * * File: geda_idefines.h
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

/** \file geda_idefines.h
 *
 *   \defgroup geda-global-integers Global Integer Constants
 * @{\par This group defines global integer constants
 *   \ingroup (geda-globals)
 */

#ifdef __GNUC__

  #define WARN_UNUSED __attribute__((warn_unused_result))
  #define NOWARN_UNUSED __attribute__((unused))

  #define MAX_FILE 255

#endif

#ifndef _WIN32
  #define MAX_PATH 248
#endif

#define MAX_FILENAME 64

#define NO_ERROR    0

#define BITS_BYTE   8
#define BITS_WORD   16
#define BITS_DWORD  32

/* These are for where status information goes */
#define CONSOLE_WINDOW		0
#define STDOUT_TTY		1
#define BOTH_CONWIN_STDOUT	2

#define DEFAULT_TEXT_SIZE   10

#define DEFAULT_ATTRIBUTE_OFFSET 50
#define DEFAULT_ATTRIBUTE_SIZE   8

/* for text cap style */
#define LOWER_CASE      0
#define UPPER_CASE      1
#define BOTH_CASES      2

/* Flag for defaults to detect keywords absent from RC files */
#define RC_NIL -1

/** @} endgroup geda-global-integers */