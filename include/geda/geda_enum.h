/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_enum.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2013-2016 Wiley Edward Hill
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */

/* ------------------------------------------------------------------ */

#ifndef GEDA_ENUMERATED_H
#define GEDA_ENUMERATED_H

/** \file geda_enum.h
 *
 *   \defgroup geda-global-enumerators Global Enumerated Semi-Constants
 * @{\par This group contains enumerations used through-out the gEDA suite.
 *   \ingroup geda-globals
 */

typedef enum
{
  GEDA_OBJECT_ALL,
  GEDA_OBJECT_ARC,
  GEDA_OBJECT_BOX,
  GEDA_OBJECT_BUS,
  GEDA_OBJECT_CIRCLE,
  GEDA_OBJECT_COMPLEX,
  GEDA_OBJECT_LINE,
  GEDA_OBJECT_NET,
  GEDA_OBJECT_PATH,
  GEDA_OBJECT_PICTURE,
  GEDA_OBJECT_PIN,
  GEDA_OBJECT_TEXT
} IDE_OBJECT_TYPE;

/*! \enum LINE_END line end style for an open line of an object */
typedef enum {END_NONE, END_SQUARE, END_ROUND, END_VOID} LINE_END;

/*! \enum LINE_TYPE style of lines, rect, circles, arcs */
typedef enum {TYPE_SOLID, TYPE_DOTTED, TYPE_DASHED, TYPE_CENTER, TYPE_PHANTOM, TYPE_ERASE} LINE_TYPE;

/*! \enum OBJECT_FILLING style of objects like cirle, rect, path */
typedef enum {FILLING_HOLLOW, FILL_SOLID, FILLING_MESH, FILLING_HATCH, FILLING_VOID} OBJECT_FILLING;

/*! \enum PIN_NODE type attributes */
typedef enum {PIN_NET_NODE, PIN_BUS_NODE} PIN_NODE;

/*! \enum PIN_MECH electrical type attributes */
typedef enum {PIN_MECH_LEAD,  PIN_MECH_BODY, PIN_MECH_PAD, PIN_MECH_BUMP,
              PIN_MECH_BALL,  PIN_MECH_WEDGE, PIN_MECH_RIBBON, PIN_MECH_VOID
} PIN_MECH;

/*! \enum PIN_ELECT electrical type attributes */
typedef enum {PIN_ELECT_IN,  PIN_ELECT_OUT, PIN_ELECT_IO,
              PIN_ELECT_OC,  PIN_ELECT_OE,  PIN_ELECT_PAS, PIN_ELECT_TP,
              PIN_ELECT_TRI, PIN_ELECT_CLK, PIN_ELECT_PWR, PIN_ELECT_VOID
} PIN_ELECT;

/*! \enum IMAGE_TYPES Supported image formats */
typedef enum { ico_image, bmp_image, tiff_image, jpeg_image, png_image,
               eps_image, pdf_image, last_image
} IMAGE_TYPES;

/*! \enum IDE_MESSAGE_TYPE Message type */
typedef enum
{
  GEDA_MESSAGE_INFO,
  GEDA_MESSAGE_WARNING,
  GEDA_MESSAGE_QUESTON,
  GEDA_MESSAGE_ERROR,
  GEDA_MESSAGE_OTHER,
} IDE_MESSAGE_TYPE;

typedef enum
{
  GEDA_RESPONSE_HELP           = -11,
  GEDA_RESPONSE_APPLY,        /* -10 */
  GEDA_RESPONSE_NO,           /* -9 */
  GEDA_RESPONSE_YES,          /* -8 */
  GEDA_RESPONSE_CLOSE,        /* -7 */
  GEDA_RESPONSE_CANCEL,       /* -6 */
  GEDA_RESPONSE_OK,           /* -5 */
  GEDA_RESPONSE_DELETE_EVENT, /* -4 */
  GEDA_RESPONSE_ACCEPT,       /* -3 */
  GEDA_RESPONSE_REJECT,       /* -2 */
  GEDA_RESPONSE_NONE,         /* -1 */
  GEDA_RESPONSE_ERROR,        /* 0 is more of a spacer */
  GEDA_RESPONSE_PLACE,        /* 1 */
  GEDA_RESPONSE_HIDE,         /* 2 */
  GEDA_RESPONSE_REFRESH,      /* 3 */
  GEDA_RESPONSE_SELECT,       /* 4 */
  GEDA_RESPONSE_DESELECT,     /* 4 */
  GEDA_RESPONSE_GET_DIST,     /* 5 */
} IDE_RESPONSE_TYPE;

/*! \enum IDE_OBJECT_FILTER Used by Libgedathon
 *  \brief Control returned Object types.
 */
typedef enum
{
  GEDA_FILTER_ALL        = 0,
  GEDA_FILTER_ARC        = 1 << 1,
  GEDA_FILTER_BOX        = 1 << 2,
  GEDA_FILTER_BUS        = 1 << 3,
  GEDA_FILTER_CIRCLE     = 1 << 4,
  GEDA_FILTER_COMPLEX    = 1 << 5,
  GEDA_FILTER_LINE       = 1 << 6,
  GEDA_FILTER_NET        = 1 << 7,
  GEDA_FILTER_PATH       = 1 << 8,
  GEDA_FILTER_PICTURE    = 1 << 9,
  GEDA_FILTER_PIN        = 1 << 10,
  GEDA_FILTER_TEXT       = 1 << 11
} IDE_OBJECT_FILTER;

/* File System Stuff */

/*! \enum IDE_OPEN_FLAGS flags used by libgeda controling file open events
 *  \brief geda_file_open behaviour flags. See documentation for
 *         geda_file_open_flags() in f_basic.c.
 */
typedef enum {
  F_OPEN_NONE         = 0,
  F_OPEN_RC           = 1,
  F_OPEN_CHECK_BACKUP = 2,
  F_OPEN_RESTORE_CWD  = 4,
} IDE_OPEN_FLAGS;

/*! \enum IDE_FILTER Used to control filter in GedaFileChooser Dialogs */
typedef  enum {
  FILTER_NONE,
  FILTER_SCHEMATIC,
  FILTER_SYMBOL,
  FILTER_GSCHEM,
} IDE_FILTER;

typedef  enum {
  FILTER_IMAGE_NONE,
  FILTER_PNG,
  FILTER_JPG,
  FILTER_GIF,
  FILTER_BMP,
  FILTER_ICO,
  FILTER_TIF,
  FILTER_XPM,
  FILTER_PNM,
  FILTER_RAS,
  FILTER_IMAGES
} IDE_IMAGE_FILTER;

/*! \enum IDE_STATE_FLAG Used by Libgedauio classes to control widget states
 *  \brief Describes a widget state.
 * #GEDA_STATE_FLAG_NORMAL:       State during normal operation.
 * #GEDA_STATE_FLAG_ACTIVE:       Widget is active.
 * #GEDA_STATE_FLAG_PRELIGHT:     Widget has a mouse pointer over it.
 * #GEDA_STATE_FLAG_SELECTED:     Widget is selected.
 * #GEDA_STATE_FLAG_INSENSITIVE:  Widget is insensitive.
 * #GEDA_STATE_FLAG_INCONSISTENT: Widget is inconsistent.
 * #GEDA_STATE_FLAG_FOCUSED:      Widget has the keyboard focus.
 * #GEDA_STATE_FLAG_BACKDROP:     Widget is in a background toplevel window.
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
} IDE_STATE_FLAG;

/*! \enum IDE_SIZE_REQUEST Used by Libgedauio classes to control widget allocations
 *  \brief
 *   Used to specifies a preference for height-for-width or width-for-height geometry
 *   management.
 * #GEDA_SIZE_REQUEST_HEIGHT_FOR_WIDTH: Prefer height-for-width geometry management
 * #GEDA_SIZE_REQUEST_WIDTH_FOR_HEIGHT: Prefer width-for-height geometry management
 * #GEDA_SIZE_REQUEST_CONSTANT_SIZE: Dont trade height-for-width or width-for-height
 *
 */
typedef enum
{
  GEDA_SIZE_REQUEST_HEIGHT_FOR_WIDTH = 0,
  GEDA_SIZE_REQUEST_WIDTH_FOR_HEIGHT,
  GEDA_SIZE_REQUEST_CONSTANT_SIZE
} IDE_SIZE_REQUEST;

/** @} endgroup geda-global-enumerators */

#endif /* GEDA_ENUMERATED_H */
