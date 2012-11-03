/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

#ifndef _GSCHEM_IDEFINES_H_INCL
#define _GSCHEM_IDEFINES_H_INCL

#define MAX_KEYWORD 48

#define MAX_FILENAME 64
#ifndef _WIN32
#define MAX_PATH 248
#endif

#define MAX_RECENT_FILES 10

/*----------- Color Stuff ----------- */

/* For default_image_width  */
#define DEFAULT_IMAGE_WIDTH     800

/* For default_image_height  */
#define DEFAULT_IMAGE_HEIGHT    600

/*--------- Display Category --------- */

#define ZOOM_OUT 0
#define ZOOM_IN 1
#define ZOOM_FULL 2

/* For Grid */
#define GRID_NONE               0
#define GRID_DOTS               1
#define GRID_MESH               2

/* For dots_grid_mode */
#define DOTS_GRID_VARIABLE_MODE 0
#define DOTS_GRID_FIXED_MODE    1

/* For dots-grid-fixed-threshold */
#define MIN_GRID_DOT_THRESHOLD      5
#define MAX_GRID_DOT_THRESHOLD     50
#define DEFAULT_GRID_DOT_THRESHOLD 10

/* For dots_grid_size */
#define MIN_GRID_DOT_SIZE       1
#define DEFAULT_GRID_DOT_SIZE   1

/* For mesh_grid_threshold */
#define MIN_GRID_MESH_THRESHOLD 1
#define MAX_GRID_MESH_THRESHOLD 99
#define DEFAULT_GRID_MESH_THRESHOLD 25

/* For mesh_grid_threshold */
#define DEFAULT_SCROLLPAN_STEPS 8

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

/* For grip size */
#define GRIP_SIZE1		25
#define GRIP_SIZE2		50
#define GRIP_SIZE3		80
#define SMALL_ZOOMFACTOR1	150
#define SMALL_ZOOMFACTOR2	30
#define MAXIMUM_GRIP_PIXELS     30

/*----------- Logging -----------*/

#define MAP_LATER		0
#define MAP_ON_STARTUP		1

/* for log-window-type */
#define DECORATED		0
#define TRANSIENT		1

/*--------- Miscellaneous ----------*/

/* These modes are for action_feedback_mode. There's a hack in */
/*  i_keypress.c dealing with the 0 and 1 (has to be these values */
#define OUTLINE         0
#define BOUNDINGBOX     1

/* This is an additional mode for last_drawb_mode, to indicate there was no
 * last bounding box drawn. last_drawb_mode also takes actionfeedback_mode
 * constants, so be sure not to clash with those */
#define LAST_DRAWB_MODE_NONE   -1

/* For add_attribute_offset */
#define DEFAULT_ATTRIBUTE_OFFSET 50

/* For auto_load_last */
#define DEFAULT_AUTO_LOAD_LAST    1

/* For auto_save_interval */
#define DEFAULT_SAVE_INTERVAL   120

/* For autoplace_attributes_grid */
#define MIN_AUTOPLACE_GRID        0
#define MAX_AUTOPLACE_GRID      500
#define DEFAULT_AUTOPLACE_GRID   50

/* select_slack_pixels */
#define DEFAULT_SLACK_PIXELS      4

/* For keyboardpan_gain */
#define MIN_KEYBOARD_GAIN         1
#define MAX_KEYBOARD_GAIN        99
#define DEFAULT_KEYBOARD_GAIN    20

/* For snap-size */
#define DEFAULT_SNAP_SIZE       100

/*--------- Nets and Routing ----------*/
/* These modes are for net_endpoint_mode and net_midpoint_mode*/
#define NET_NONE        0
#define EMPTY_BOX	1
#define FILLED_BOX	2
#define X		3

/* These modes are for net_selection_mode */
#define NET_SELECT_NET	2
#define NET_SELECT_ALL	3

/* for bus_ripper_size */
#define DEFAULT_RIPPER_SIZE     200

/* for bus_ripper_rotation */
#define SYMMETRIC               0
#define NON_SYMMETRIC           1

#define FREE        1
#define CONSTRAINED 2

/* for bus_ripper_type */
#define COMP_BUS_RIPPER         0
#define NET_BUS_RIPPER          1

/* for attrib_edit_dialog invocation flag */
#define FROM_MENU		0
#define FROM_HOTKEY		1

/*--------- Pointer/Mouse ---------*/

/* for third-mouse */
#define POPUP_ENABLED		0
#define MOUSEPAN_ENABLED	1

/* for middle-mouse */
#define MOUSE_MIDDLE_STROKE	0
#define MOUSE_MIDDLE_REPEAT	1
#define MOUSE_MIDDLE_ACTION	2
#define MOUSE_MIDDLE_PAN	3

/* for mousepan_gain */
#define DEFAULT_MOUSEPAN_GAIN   5

/* for scroll-wheel */
#define SCROLL_WHEEL_GTK        0
#define SCROLL_WHEEL_CLASSIC    1

/* for selected_from */
#define DONTCARE		0
#define MENU			1
#define HOTKEY			2

/* for a_pan_general and a_zoom */
#define A_PAN_IGNORE_BORDERS 	1
#define A_PAN_DONT_REDRAW 	2

/* selection types */
/* used in o_select_object */
#define SINGLE                  0
#define MULTIPLE                1

/* Print Related */
#define MIN_PAPER_DIMENSION     1000 /* For error trapping, seems resonable? */
#define DEFAULT_PAPER_WIDTH     11000
#define DEFAULT_PAPER_HEIGHT    85000

/*----------- Text Category -----------*/
/* for text cap style */
#define LOWER_CASE      0
#define UPPER_CASE      1
#define BOTH_CASES      2

/* for text_display_zoomfactor */
#define DEFAULT_TEXT_ZOOM      30
#define MIN_TEXT_ZOOM           1
#define MAX_TEXT_ZOOM          99

/* modes for text-feedback */
#define ONLY_WHEN_READABLE	0
#define ALWAYS_FEEDBACK		1

/* text_size */
#define DEFAULT_TEXT_SIZE       10
#define MIN_TEXT_SIZE           1
#define MAX_TEXT_SIZE           99

/*----------- Undo System -----------*/

/* used in o_undo_callback */
#define UNDO_ACTION		0
#define REDO_ACTION		1

/* For undo_levels */
#define DEFAULT_UNDO_LEVELS     10

/* used for undo_type */
#define UNDO_DISK		0
#define UNDO_MEMORY		1



#endif /* !_GSCHEM_IDEFINES_H_INCL */
