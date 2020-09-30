/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_idefines.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */

/*!
 * \file gschem_idefines.h
 *
 * \brief Defines Integer constants used in gschem
 */

#ifndef _GSCHEM_IDEFINES_H_INCL
#define _GSCHEM_IDEFINES_H_INCL

/*! \brief different kind of snapping mechanisms used in GedaToplevel */
typedef enum {SNAP_OFF, SNAP_GRID, SNAP_RESNAP, SNAP_STATE_COUNT} SNAP_STATE;

/* used by x_rc_parse_gschem */
#define MAX_RC_ATTEMPTS           3

#define MAX_KEYWORD              48
#define MAX_RECENT_FILES         10  /* Some day */
#define MESSAGE_BUFFER_SIZE    2048

#define DEFAULT_RAISE_TIMER     100 /* milliseconds */

/*-------- Color & Image Stuff ------- */

/* For default_image_width  */
#define MIN_IMAGE_WIDTH         160 /* For error trapping*/
#define DEFAULT_IMAGE_WIDTH     800

/* For default_image_height  */
#define MIN_IMAGE_HEIGHT        120 /* For error trapping*/
#define DEFAULT_IMAGE_HEIGHT    600

/*--------- Display Category --------- */
#define MAX_ZOOM_FACTOR         6
#define ZOOM_EXTENTS_PADDING_PX 5

/* For Grid */
#define GRID_NONE               0
#define GRID_DOTS               1
#define GRID_MESH               2

/* For dots_grid_mode */
#define DOTS_GRID_VARIABLE_MODE 0
#define DOTS_GRID_FIXED_MODE    1

/* For dots-grid-threshold */
#define MIN_GRID_DOT_THRESHOLD      5
#define MAX_GRID_DOT_THRESHOLD     50
#define DEFAULT_GRID_DOT_THRESHOLD 10

/* For dots_grid_size */
#define MIN_GRID_DOT_SIZE       1
#define DEFAULT_GRID_DOT_SIZE   1

/* For mesh_grid_threshold */
#define MIN_GRID_MESH_THRESHOLD        1
#define MAX_GRID_MESH_THRESHOLD        99
#define DEFAULT_GRID_MESH_THRESHOLD    10

#define MIN_MESH_LINE_WIDTH_FACTOR     10
#define MAX_MESH_LINE_WIDTH_FACTOR     500
#define DEFAULT_MESH_LINE_WIDTH_FACTOR 120

#define MIN_GRID_ALPHA                 0
#define MAX_GRID_ALPHA                 100

#define DEFAULT_GRID_MINOR_ALPHA       30
#define DEFAULT_GRID_MAJOR_ALPHA       40

#define MAX_GRID_COLOR                 65535
#define DEFAULT_GRID_COLOR             MAX_GRID_COLOR

/* for junctions */
#define MAX_JUNCTION_SIZE              100
#define MIN_JUNCTION_SIZE              0
#define DEFAULT_JUNCTION_SIZE          40
#define DEFAULT_JUNCTION_COLOR         JUNCTION_COLOR

/* For Both Scrollbars and Mousewheel */
#define DEFAULT_SCROLLPAN_STEPS        8

/* For Window-Size */
/* Note Min/Max is really just an attempt to catch typo's, WEH */
#define DEFAULT_WINDOW_WIDTH    800
#define MIN_WINDOW_WIDTH        640
#define MAX_WINDOW_WIDTH       3840

#define DEFAULT_WINDOW_HEIGHT   600
#define MIN_WINDOW_HEIGHT       480
#define MAX_WINDOW_HEIGHT      3500

/* For zoom_gain */
#define MIN_ZOOM_GAIN          -99
#define MAX_ZOOM_GAIN           99
#define DEFAULT_ZOOM_GAIN       20

/* For grip size in pixels (i.e. device units) */
#define GRIP_SIZE_ZOOM1         60
#define GRIP_ZOOM_THREASHOLD_1  15

#define DEFAULT_RENDERER         0

#define DEFAULT_ACTION_COLOR    NET_COLOR

#define MIN_ANTI_ALIASING       CAIRO_ANTIALIAS_DEFAULT
#define MAX_ANTI_ALIASING       CAIRO_ANTIALIAS_BEST
#define DEFAULT_ANTI_ALIASING   CAIRO_ANTIALIAS_SUBPIXEL

#define MIN_GRIP_SIZE            6
#define DEFAULT_GRIP_SIZE       12
#define MAX_GRIP_SIZE           32

/* These aren't int's, but they don't belong in sdefines */
#define DEFAULT_GRIP_STROKE_COLOR  SELECT_COLOR
#define DEFAULT_GRIP_FILL_COLOR    BACKGROUND_COLOR

/*
 * These are used in g_funcs.c, supposely Flags for generic_filesel_dialog()
 * but FSB_MAY_EXIST, FSB_SHOULD_NOT_EXIST are not referenced in any other
 * file outside of g_funcs.c, need to check usage
 */
#define FSB_MAY_EXIST           1
#define FSB_MUST_EXIST          2
#define FSB_SHOULD_NOT_EXIST    4
#define FSB_SAVE                256
#define FSB_LOAD                512

/*----------- Logging -----------*/

#define MAP_LATER       0
#define MAP_ON_STARTUP  1

/* for console-window-type */
#define DECORATED       0
#define TRANSIENT       1

/*----------- Hierarchy -----------*/

#define CLOSE_NONE              0
#define CLOSE_SYM               1
#define CLOSE_BOTH              2

/*--------- Miscellaneous ----------*/

/*! \def DEFAULT_TAB_SIZE Size of a tab in characters */
#define DEFAULT_TAB_SIZE 8

/* These modes are for action_feedback_mode. There's a hack in */
/*  i_keypress.c dealing with the 0 and 1 (has to be these values */
#define OUTLINE         0
#define BOUNDINGBOX     1

/* This is an additional mode for last_drawb_mode, to indicate there was no
 * last bounding box drawn. last_drawb_mode also takes actionfeedback_mode
 * constants, so be sure not to clash with those */
#define LAST_DRAWB_MODE_NONE     -1

/* For auto_load_last */
#define DEFAULT_AUTO_LOAD_LAST    1

/* For auto_save_interval */
#define DEFAULT_SAVE_INTERVAL   180

/* For autoplace_attributes_grid */
#define MIN_AUTOPLACE_GRID          0
#define MAX_AUTOPLACE_GRID        500
#define DEFAULT_ATTRIB_PLACE_GRID  50

/* select_slack_pixels */
#define DEFAULT_SLACK_PIXELS      4

/* For keyboardpan_gain */
#define MIN_KEYBOARD_GAIN         1
#define MAX_KEYBOARD_GAIN       999
#define DEFAULT_KEYBOARD_GAIN   20

/* For snap-size */
#define DEFAULT_SNAP_SIZE       100
#define MIN_SNAP_SIZE           1
#define MAX_SNAP_SIZE           500

/*--------- Nets and Routing ----------*/

#define DEFAULT_NET_ENDPOINT_COLOR NET_ENDPOINT_COLOR

/* These modes are for net_endpoint_mode and net_midpoint_mode*/
#define NET_NONE                0
#define EMPTY_BOX               1
#define FILLED_BOX              2

/* These modes are for net_selection_mode */
#define NET_SELECT_NET          2
#define NET_SELECT_ALL          3

/* for bus_ripper_size */
#define DEFAULT_RIPPER_SIZE     200

/* for bus_ripper_rotation */
#define NON_SYMMETRIC           0
#define SYMMETRIC               1

#define FREE                    1
#define CONSTRAINED             2

/* for bus_ripper_type */
#define NET_BUS_RIPPER          0
#define COMP_BUS_RIPPER         1

/*--------- Pointer/Mouse ---------*/
#define DEFAULT_CURSOR_INDEX    0

/* for third-mouse */
#define POPUP_ENABLED           0
#define MOUSEPAN_ENABLED        1

/* for middle-mouse */
#define MOUSE_MIDDLE_STROKE     0
#define MOUSE_MIDDLE_REPEAT     1
#define MOUSE_MIDDLE_ACTION     2
#define MOUSE_MIDDLE_PAN        3
#define MOUSE_MIDDLE_POPUP      4
#define DEFAULT_MOUSE_MIDDLE    MOUSE_MIDDLE_POPUP

/* for auto-pan, unit is milliseconds */
#define AUTO_PAN_INTERVAL       75
#define DEFAULT_AUTO_PAN        1
#define MIN_AUTO_PAN_STEP       1
#define MAX_AUTO_PAN_STEP       99
#define DEFAULT_AUTO_PAN_STEP   16

/* for mousepan_gain */
#define DEFAULT_MOUSEPAN_GAIN   5

/* for scrollbar-update type */
#define DISPLAY_CONTINUOUS      0
#define DISPLAY_DELAYED         1

/* for scroll-wheel */
#define SCROLL_WHEEL_GTK        0
#define SCROLL_WHEEL_CLASSIC    1

/* for selected_from */
#define DONTCARE                0

/* selection types */
/* used in o_select_object */
#define SINGLE                  0
#define MULTIPLE                1

/* Print Related */
#define MIN_PAPER_DIMENSION     1000 /* For error trapping, seems resonable? */
#define DEFAULT_PAPER_WIDTH     11000
#define DEFAULT_PAPER_HEIGHT    85000

/*----------- Text Category -----------*/

/* text cap style was here, relocated to include/geda_idefines.h 01/02/13 */

/* for text_display_zoomfactor */
#define DEFAULT_TEXT_ZOOM       30
#define MIN_TEXT_ZOOM            1
#define MAX_TEXT_ZOOM           99

/* modes for text-feedback */
#define ONLY_WHEN_READABLE       0
#define ALWAYS_FEEDBACK          1

/* text_marker */
#define MIN_TEXT_MARKER_SIZE      5
#define MAX_TEXT_MARKER_SIZE      100
#define DEFAULT_TEXT_MARKER_COLOR LOCK_COLOR
#define DEFAULT_TEXT_MARKER_SIZE  15
#define MIN_TEXT_MARKER_THLD      5
#define MAX_TEXT_MARKER_THLD      250
#define DEFAULT_TEXT_MARKER_THLD  20

/* text_size */
#ifndef DEFAULT_TEXT_SIZE
  #define DEFAULT_TEXT_SIZE     10
#endif
#define MIN_TEXT_SIZE            1
#define MAX_TEXT_SIZE           99

/* toolbars_mode */
#define TOOLBAR_SHOW_ICONS      GTK_TOOLBAR_ICONS
#define TOOLBAR_SHOW_TEXT       GTK_TOOLBAR_TEXT
#define TOOLBAR_SHOW_BOTH       GTK_TOOLBAR_BOTH
#define TOOLBAR_SHOW_HORIZ      GTK_TOOLBAR_BOTH_HORIZ
#define TOOLBAR_RETENTION      -88

/*----------- Undo System -----------*/

/* used in o_undo_callback */
#define UNDO_ACTION              0
#define REDO_ACTION              1

/* For undo_levels */
#define DEFAULT_UNDO_LEVELS     20

#endif /* !_GSCHEM_IDEFINES_H_INCL */
