/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2013 Wiley Edward Hill
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*!
 * \file geda_enum.h
 * \brief Global gEDA Enumerated Semi-Constants
 *
 * \section
 *
 */
/* ------------------------------------------------------------------ */

#ifndef __GEDA_ENUMERATED__
#define __GEDA_ENUMERATED__

/*! \brief line end style for an open line of an object */
typedef enum {END_NONE, END_SQUARE, END_ROUND, END_VOID} OBJECT_END;

/*! \brief line style of lines, rect, circles, arcs */
typedef enum {TYPE_SOLID, TYPE_DOTTED, TYPE_DASHED, TYPE_CENTER, TYPE_PHANTOM, TYPE_ERASE} OBJECT_TYPE;

/*! \brief fill style of objects like cirle, rect, path */
typedef enum {FILLING_HOLLOW, FILLING_FILL, FILLING_MESH, FILLING_HATCH, FILLING_VOID} OBJECT_FILLING;

typedef enum { png_image, tiff_image, bmp_image, ico_image, jpeg_image, eps_image, pdf_image } IMAGE_TYPES;

/* File System Stuff */

/* f_open behaviour flags.  See documentation for f_open_flags() in f_basic.c. */
typedef enum { F_OPEN_RC           = 1,
               F_OPEN_CHECK_BACKUP = 2,
               F_OPEN_RESTORE_CWD  = 4,
} FOpenFlags;

#endif
