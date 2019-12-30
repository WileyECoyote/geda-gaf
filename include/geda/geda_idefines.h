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
 *   \ingroup geda-globals
 */

#ifndef MAX_FILE
#  define MAX_FILE 255
#endif

#define MAX_FILENAME 64

#ifndef MAX_PATH
#  define MAX_PATH 248
#endif

#ifndef NO_ERROR
#  define NO_ERROR  0
#endif

#define BITS_BYTE   8
#define BITS_WORD   16
#define BITS_DWORD  32

/* These are for where status information goes */
#define CONSOLE_WINDOW     0
#define STDOUT_TTY         1
#define BOTH_CONWIN_STDOUT 2

/*! \def DEFAULT_TEXT_SIZE  Default text size */
#define DEFAULT_TEXT_SIZE   10

/*! \def MINIMUM_TEXT_SIZE  Minimum text size */
#define MINIMUM_TEXT_SIZE 1

/*! \def DEFAULT_ATTRIBUTE_OFFSET Distance from objects to new attributes */
#define DEFAULT_ATTRIBUTE_OFFSET 50

/*! \def DEFAULT_ATTRIBUTE_SIZE Text size for attributes if not specified */
#define DEFAULT_ATTRIBUTE_SIZE   8

/* for text cap style */
#define LOWER_CASE      0
#define UPPER_CASE      1
#define BOTH_CASES      2

/*! \def RC_NIL Flag for defaults to detect keywords absent from RC files */
#define RC_NIL -1

/*! \def MAX_COLORS for color mapping systems */
#define MAX_COLORS 30

/** @} endgroup geda-global-integers */
