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

/*! \brief pin type styles */
typedef enum {PIN_TYPE_NET, PIN_TYPE_BUS, PIN_TYPE_BUMP, PIN_TYPE_BALL,
              PIN_TYPE_WEDGE, PIN_TYPE_RIBBON, PIN_TYPE_VOID} PIN_TYPE;

/*! \brief pin type attributes */
typedef enum {PIN_ATTRIB_IN,  PIN_ATTRIB_OUT, PIN_ATTRIB_IO,  PIN_ATTRIB_OC,
              PIN_ATTRIB_OE,  PIN_ATTRIB_PAS, PIN_ATTRIB_TP,  PIN_ATTRIB_TRI,
              PIN_ATTRIB_CLK, PIN_ATTRIB_PWR, PIN_ATTRIB_VOID
} PIN_ATTRIBUTE;

typedef enum { png_image, tiff_image, bmp_image, ico_image, jpeg_image,
               eps_image, pdf_image, last_image
} IMAGE_TYPES;

typedef enum
{
  GEDA_MESSAGE_INFO,
  GEDA_MESSAGE_WARNING,
  GEDA_MESSAGE_QUESTON,
  GEDA_MESSAGE_ERROR,
  GEDA_MESSAGE_OTHER,
} gEDA_MessageType;

/* File System Stuff */

/* f_open behaviour flags.  See documentation for f_open_flags() in f_basic.c. */
typedef enum { F_OPEN_RC           = 1,
               F_OPEN_CHECK_BACKUP = 2,
               F_OPEN_RESTORE_CWD  = 4,
} FOpenFlags;

/**
 * @GEDA_STATE_FLAG_NORMAL: State during normal operation.
 * @GEDA_STATE_FLAG_ACTIVE: Widget is active.
 * @GEDA_STATE_FLAG_PRELIGHT: Widget has a mouse pointer over it.
 * @GEDA_STATE_FLAG_SELECTED: Widget is selected.
 * @GEDA_STATE_FLAG_INSENSITIVE: Widget is insensitive.
 * @GEDA_STATE_FLAG_INCONSISTENT: Widget is inconsistent.
 * @GEDA_STATE_FLAG_FOCUSED: Widget has the keyboard focus.
 * @GEDA_STATE_FLAG_BACKDROP: Widget is in a background toplevel window.
 *
 * Describes a widget state.
 */
typedef enum
{
  GEDA_STATE_FLAG_NORMAL       = 0,
  GEDA_STATE_FLAG_ACTIVE       = 1 << 0,
  GEDA_STATE_FLAG_PRELIGHT     = 1 << 1,
  GEDA_STATE_FLAG_SELECTED     = 1 << 2,
  GEDA_STATE_FLAG_INSENSITIVE  = 1 << 3,
  GEDA_STATE_FLAG_INCONSISTENT = 1 << 4,
  GEDA_STATE_FLAG_FOCUSED      = 1 << 5,
  GEDA_STATE_FLAG_BACKDROP     = 1 << 6
} GedaStateFlags;
/**
 * GtkSizeRequestMode:
 * @GEDA_SIZE_REQUEST_HEIGHT_FOR_WIDTH: Prefer height-for-width geometry management
 * @GEDA_SIZE_REQUEST_WIDTH_FOR_HEIGHT: Prefer width-for-height geometry management
 * @GEDA_SIZE_REQUEST_CONSTANT_SIZE: Dont trade height-for-width or width-for-height
 * 
 * Specifies a preference for height-for-width or
 * width-for-height geometry management.
 */
typedef enum
{
  GEDA_SIZE_REQUEST_HEIGHT_FOR_WIDTH = 0,
  GEDA_SIZE_REQUEST_WIDTH_FOR_HEIGHT,
  GEDA_SIZE_REQUEST_CONSTANT_SIZE
} GedaSizeRequestMode;
#endif

