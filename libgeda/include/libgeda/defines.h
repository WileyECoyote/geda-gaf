/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: defines.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * MA 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

/*! \file defines.h
 *  \brief global libgeda definitions
 */

#ifndef _DEFINES_H_INCL
#define _DEFINES_H_INCL

/* Current schematic/symbol file format */
#define FILEFORMAT_VERSION     3

/* release version which had file format changes */
/* New file format changes after 20030921 use the above version */
/* and not these #defines anymore. */
#define VERSION_20000220 20000220
#define VERSION_20000704 20000704   /*!< last arc, box, circle and line with line type */
#define VERSION_20020825 20020825   /*!< Last version without bus ripper_dir */
#define VERSION_20030921 20030921   /*!< Last version which did not have a fileformat */

/* 20030921 wasn't a real version, just a MinGW test version, but it is */
/* out there */

/* Set this string to something interesting to create a custom */
/* version of gEDA/gaf.  This string is prepended to all messages that */
/* output the program's version output.  You would set this if you are */
/* creating a specific custom version of gEDA/gaf.  */
/* For example, if you set this string to "FIX-", the resulting output is:  */
/* FIX-1.0.0.20060906. */
#define PREPEND_VERSION_STRING ""

/* \note
 * Kazu Hirata <kazu@seul.org> on July 16, 1999 - Added these absolute
 * defaults used when default_... is NULL.
 *
 * W.E.Hill Sept 5, 2012 relocated "these" defines from i_var.c to
 * better organize the code.
 */
#define DEFAULT_SCHEME_DIRECTORY   "./"
#define DEFAULT_BITMAP_DIRECTORY   "../lib/bitmaps"
#define DEFAULT_POSTSCRIPT_PROLOG  "prolog.ps"
#define DEFAULT_UNTITLED_NAME      "untitled"

/*! \def INIT_STR(t, name, str) */
#define INIT_STR(t, name, str) {                               \
        if((t)->name) geda_free((t)->name);                    \
        (t)->name = geda_strdup(((default_ ## name) != NULL) ? \
                              (default_ ## name) : (str));     \
}

#define DISK_BUFFER_SIZE        4096

#define DEFAULT_PAGE_WIDTH      121000
#define DEFAULT_PAGE_HEIGHT     90750

/*! \def NEAR_DISTANCE Used in get nearest routine to snap endpoints */
#define NEAR_DISTANCE           5

/* X's obsession with *64 */
#define FULL_CIRCLE             360*64

/* for show_name_value in st_objects */
#define SHOW_NAME_VALUE         0
#define SHOW_VALUE              1
#define SHOW_NAME               2
#define LEAVE_NAME_VALUE_ALONE -1

/* for visibility in st_objects */
#define INVISIBLE               0
#define VISIBLE                 1
#define LEAVE_VISIBILITY_ALONE -1

/* For bus, line, net, and pin styles */
#define STYLE_NONE              0
#define STYLE_THIN              1
#define STYLE_THICK             2

#define RC_STR_STYLE_NONE      "none"
#define RC_STR_STYLE_THIN      "thin"
#define RC_STR_STYLE_THICK     "thick"

#define DEFAULT_BUS_STYLE       STYLE_THICK
#define DEFAULT_LINE_STYLE      STYLE_THIN
#define DEFAULT_NET_STYLE       STYLE_NONE
#define DEFAULT_PIN_STYLE       STYLE_NONE

/* various thicknesses (in mils) */
#define MIN_LINE_WIDTH_THRESHOLD 10
#define MIN_BUS_WIDTH            0
#define MIN_LINE_WIDTH           0
#define MIN_NET_WIDTH            0
#define MIN_PIN_WIDTH            0

#define DEFAULT_WIDTH_NONE       0

#define DEFAULT_THIN_BUS_WIDTH   15
#define DEFAULT_THICK_BUS_WIDTH  30

#define DEFAULT_THIN_LINE_WIDTH  10
#define DEFAULT_THICK_LINE_WIDTH 30

#define DEFAULT_THIN_NET_WIDTH    5
#define DEFAULT_THICK_NET_WIDTH  20

#define DEFAULT_THIN_PIN_WIDTH   10
#define DEFAULT_THICK_PIN_WIDTH  30

/* various visual cue sizes (in mils) */
#define CUE_BOX_SIZE            30
#define JUNCTION_CUE_SIZE_NET   50
#define JUNCTION_CUE_SIZE_BUS   30
#define PIN_CUE_SIZE_NET        30
#define PIN_CUE_SIZE_BUS        50

/* For text location on component not found graphics */
#define NOT_FOUND_TEXT_X        100
#define NOT_FOUND_TEXT_Y        100

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))

#undef min
#define min(a,b) ((a) < (b) ? (a) : (b))

/* for s_slib_search() */
#define SLIB_SEARCH_START   0
#define SLIB_SEARCH_NEXT    1
#define SLIB_SEARCH_DONE    2

/* for text alignment */
/*   2 -- 5 -- 8  */
/*   |    |    |  */
/*   1 -- 4 -- 7  */
/*   |    |    |  */
/*   0 -- 3 -- 6  */
#define LOWER_LEFT         0
#define MIDDLE_LEFT        1
#define UPPER_LEFT         2
#define LOWER_MIDDLE       3
#define MIDDLE_MIDDLE      4
#define UPPER_MIDDLE       5
#define LOWER_RIGHT        6
#define MIDDLE_RIGHT       7
#define UPPER_RIGHT        8

/* one character string used to calculate tab's width */
/* Warning: it MUST be a string. */
#define TAB_CHAR_MODEL "b"

/* The conn modes for type */
#define CONN_NULL           0
#define CONN_ENDPOINT       1
#define CONN_MIDPOINT       2

/* used for undo_savestate flag */
#define UNDO_ALL            0
#define UNDO_VIEWPORT_ONLY  1

/* for console-window keyword */
#define MAP_LATER           0
#define MAP_ON_STARTUP      1

/* for console-window-type */
#define DECORATED           0
#define TRANSIENT           1

/* list copying flags */
#define NORMAL_FLAG         0
#define SELECTION_FLAG      1

/* hierarchy loading flags */
#define HIERARCHY_NORMAL_LOAD   0
#define HIERARCHY_FORCE_LOAD    1

/* hierarchy traversing flags */
#define HIERARCHY_NODUPS    (1<<0)
#define HIERARCHY_POSTORDER (1<<1)
#define HIERARCHY_INNERLOOP (1<<7)

#define MILS_PER_INCH       1000

/* for text_output */
#define VECTOR_FONTS        0
#define PS_FONTS            1

#define DEFAULT_OBJECT_END END_SQUARE
/* for print dialog box */
#define EXTENTS             0
#define WINDOW              1
#define EXTENTS_NOMARGINS   2

/* for output-capstyle */
#define BUTT_CAP        0
#define ROUND_CAP       1
#define SQUARE_CAP      2

/* for print dialog box */
#define LANDSCAPE       0
#define PORTRAIT        1
#define AUTOLAYOUT      2

/* for type to geda_struct_cue_output_all */
#define POSTSCRIPT      0
#define PNG             1

/* for geda_net_object_orientation */
#define NEITHER         0
#define HORIZONTAL      1
#define VERTICAL        2

/* gschcheck: Error types */
#define NO_ERR                  0
#define FLOAT_NET               1
#define FLOAT_PIN               2
#define DUP_NET_NAME            4

/* Max level of symlinks */
#define MAX_LINK_LEVEL 256

#if !defined(M_PI)
#define M_PI  3.14159265358979323846
#endif

/* Logs a normal message. */
#define u_log_message g_message

/* Backup filename creation string */
#define AUTOSAVE_BACKUP_FILENAME_STRING "#%s#"

/*! \def RETURN_G_RC_MODE(rc, var, mt)
 *  \brief Utility Macro used by the rc loading mechanism
 *  \note The 1st argument to g_rc_parse_mode is not passed to the macro, RC
 *   handlers incorporating this macro must name their Scheme argument "mode".
 *
 *  \param [in] rc   String keyword,
 *  \param [in] var  default_variable_name
 *  \param [in] mt   vstbl_entry structure containing valid options
 *
 *  \todo This macro probably does not belong in this file
 */
#define RETURN_G_RC_MODE(rc, var, mt) \
  return g_rc_parse_mode(mode, (rc), &(var), mt, (G_N_ELEMENTS(mt)))

#endif /* !_DEFINES_H_INCL */
