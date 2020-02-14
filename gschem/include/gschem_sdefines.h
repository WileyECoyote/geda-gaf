/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/gschem_sdefines.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
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
 *
 * Date: Aug, 22, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
*/

/*!
 * \file gschem_sdefines.h
 *
 * \brief Defines String constants used in gschem
 */

/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 09/17/12 |  Inital release.
 * ------------------------------------------------------------------
 * WEH | 12/30/12 |  Added msgbox (for debugging). Changed RC_STR_LOGWIN
 *                |  RC_STR_CONWIN (to integrate new functionality of
 *                |  the "retrofitted" log window to a console window)
 * ------------------------------------------------------------------
 * WEH | 01/06/13 |  Added Bus Ripper Strings for intergration with the
 *     |          |  configure settings dailog.
 * WEH | 01/24/13 |  Add filenames for help documents
 * ------------------------------------------------------------------
 * WEH | 08/24/13 | Changed IDS_SINGLE_ATTRRI to IDS_SINGLE_ATTRIR and
 *                | updated to type modeless.
 * ------------------------------------------------------------------
 * WEH | 10/16/13 | Remove string for Schematic's and Symbols and relocated
 *                | to include/libgeda/defines.h.
 * ------------------------------------------------------------------
 * WEH | 08/30/14 | Added SESSIONS_CONFIG_GROUP.
 * ------------------------------------------------------------------
 * WEH | 09/15/14 | Add strings for anti-aliasing rc setting.
 * ------------------------------------------------------------------
 * WEH | 06/27/15 | Added LOG_CONFIG_GROUP.
 * ------------------------------------------------------------------
 * WEH | 02/07/19 | Remove obsolete GSCHEM_EDA_STORE, DIALOG_CONFIG_STORE
 *                | and WINDOW_GEOMETRY_STORE.
*/

#ifndef _GSCHEM_SDEFINES_H_INCL
#define _GSCHEM_SDEFINES_H_INCL

#ifndef OS_WIN32_NATIVE

/* Can use like: msgbox("example = [%d]", g_list_length(geda_toplevel_get_pages(toplevel))) */
#define msgbox(...)x_dialog_show_message(geda_sprintf(__VA_ARGS__), GTK_MESSAGE_INFO, NULL);

#endif

/* Name of retention file for Toolbar configuration data */
#define TOOLBAR_CONFIG_STORE       "gschem-toolbar-user.conf"

/* string name of configuration groups */
#define IVAR_CONFIG_GROUP          "global"
#define LOG_CONFIG_GROUP           "log"
#define MENU_CONFIG_GROUP          "menu"
#define WINDOW_CONFIG_GROUP        "window"
#define SESSIONS_CONFIG_GROUP      "Sessions"
#define WIDGET_CONFIG_GROUP        "widgets"

#define GEDA_TYPE_OBJECTS          "application/x-geda-objects"
#define MIME_TYPE_SCHEMATIC        "application/x-geda-schematic"
#define MIME_TYPE_SYMBOL           "application/x-geda-symbol"

#define TITLE_BLOCK_PATH           "/sym/titleblock"
#define DEFAULT_TITLEBLOCK         "title-B.sym"

#define DEFAULT_UNTITLED_NAME      "untitled"

/* Sessions */
#define SESSIONS_DIRECTORY         "sessions"
#define SESSIONS_FILE_SUFFIX       "session"
#define SESSIONS_FILE_DOT_SUFFIX   ".session"
#define SESSIONS_RESTORE_SUBMENU   "Sessio_ns/_Restore"

/* For x_icons.c */
#define GSCHEM_THEME_ICON_NAME     "geda-gschem"

#define MSG_SELECT_Object_1ST      "Select object first"

#define COORD_DISPLAY_OFF          "OFF"
#define COORD_DISPLAY_VEC180       "Vector 180"
#define COORD_DISPLAY_VEC360       "Vector 360"
#define COORD_DISPLAY_XY           "X=, Y="
#define COORD_DISPLAY_COORD        "(X,Y)"
#define COORD_DISPLAY_COMMA        "X,Y"
#define COORD_DISPLAY_X            "X="
#define COORD_DISPLAY_Y            "Y="
#define COORD_DISPLAY_XONLY        "X only"
#define COORD_DISPLAY_YONLY        "Y only"

/* -----------------  Strings for Dialogs Boxes  ---------------- */

/* Identifcation Strings for Saving & Restoring Window Geometry */
#define IDS_AUTONUMBER         "autonumber"  /* Type: MODELESS */
#define IDS_SNAP_SIZE          "snap-size"   /* Type: MODAL    */
#define IDS_TEXT_SIZE          "text-size"   /* Type: MODAL    */
#define IDS_ARC_ANGLE          "arc-angle"   /* Type: MODELESS */
#define IDS_COLOR_EDIT         "color-edit"  /* Type: MODELESS */
#define IDS_ARRAY_EDIT         "array-edit"  /* Type: MODELESS */
#define IDS_FILL_TYPE          "fill-type"   /* Type: MODELESS */
#define IDS_LINE_TYPE          "line-type"   /* Type: MODELESS */
#define IDS_PIN_EDIT           "pin-edit"    /* Type: MODELESS */
#define IDS_PROP_EDIT          "prop-edit"   /* Type: MODELESS */
#define IDS_SLOT_EDIT          "slot-edit"   /* Type: MODELESS */
#define IDS_TEXT_EDIT          "text-edit"   /* Type: MODELESS */
#define IDS_FIND_TEXT          "find-text"   /* Type: MODELESS */
#define IDS_HIDE_TEXT          "hide-text"   /* Type: MODELESS */
#define IDS_SHOW_TEXT          "show-text"   /* Type: MODELESS */
#define IDS_TEXT_INPUT         "text-input"  /* Type: MODELESS */
#define IDS_TRANSLATE          "translate"   /* Type: MODAL    */

#define IDS_HOTKEYS            "hotkeys"      /* Type: MODELESS */
#define IDS_COORDINATES        "coordinates"  /* Type: MODELESS */
#define IDS_CONSOLE            "console"      /* Type: MODELESS */
#define IDS_CONFIG_SETTINGS    "settings"     /* Type: MODAL    */
#define IDS_COMP_SELECT        "compselect"
#define IDS_SINGLE_ATTRIR      "singleattrib" /* Type: MODELESS */

#define IDS_MULTI_ATTRBI       "multiattrib"  /* Type: MODELESS */
#define IDS_PAGE_MANAGER       "page-man"     /* Type: MODELESS */

/* -----------------  Strings for RC Read/Write ----------------- */

/* Generic Strings for RC code */
#define RC_STR_BOTH            "both"
#define RC_STR_NONE            "none"
#define RC_STR_SYM             "sym"

/* Renderer Options */
#define RC_RENDERER_OPTION_CAIRO  "Cario"
#define RC_RENDERER_OPTION_X11    "X11"

/* Strings for Anti-Aliasing RC mode */
#define RC_STR_ANTIALIAS_DEFAULT  "default"
#define RC_STR_ANTIALIAS_NONE     RC_STR_NONE
#define RC_STR_ANTIALIAS_GRAY     "gray"
#define RC_STR_ANTIALIAS_SUBPIXEL "subpixel"
#define RC_STR_ANTIALIAS_FAST     "fast"
#define RC_STR_ANTIALIAS_GOOD     "good"
#define RC_STR_ANTIALIAS_BEST     "best"

/* Generic Boolean Strings for RC code */
#define RC_STR_ENABLED            "enabled"
#define RC_STR_DISABLED           "disabled"

/* Grid Mode Strings for RC code */
#define RC_STR_DOTS               "dots"
#define RC_STR_MESH               "mesh"

/* Dots Grid Mode Strings for RC code */
#define RC_STR_DOTS_MODE_VARIABLE "variable"
#define RC_STR_DOTS_MODE_FIXED    "fixed"

/* ScrollBar Update Strings for RC code */
#define RC_STR_BARS_CONTINUOUS    "continuous"
#define RC_STR_BARS_DELAYED       "delayed"

/* WindowSize Strings for RC code */
#define RC_STR_WINDOW_W650H487    "window-size 650 487)  ; Good size for 800x600"
#define RC_STR_WINDOW_W900H650    "window-size 900 650)  ; Good size for 1024x768"
#define RC_STR_WINDOW_W950H712    "window-size 950 712)  ; Good size for 1152x864"
#define RC_STR_WINDOW_W1100H825   "window-size 1100 825) ; Good size for 1280x1024"

/* WorldSize Strings for RC code */
#define RC_STR_WORLD_SMALL        "60.0 45.0 1.0"
#define RC_STR_WORLD_MEDIUM       "120.0 90.0 1.0"
#define RC_STR_WORLD_LARGE        "180.0 135.0 1.0"

/* Log Destiny Strings for RC code */
#define RC_STR_DESTINY_WINDOW     "console_window"
#define RC_STR_DESTINY_TTY        "tty"
#define RC_STR_DESTINY_BOTH       RC_STR_BOTH

/* Console Window Type Strings for RC code */
#define RC_STR_CONWIN_DECORATED   "decorated"
#define RC_STR_CONWIN_TRANSIENT   "transient"

/* Action Feedback Mode Strings for RC code */
#define RC_STR_FEEDBACK_OUTLINE   "outline"
#define RC_STR_FEEDBACK_BOUNDBOX  "boundingbox"

/* Net POINT MODE Strings for RC code */
#define RC_STR_NET_NONE           RC_STR_NONE
#define RC_STR_FILLED_BOX         "filledbox"
#define RC_STR_EMPTY_BOX          "empty"

#define RC_STR_NET_NET            "net"
#define RC_STR_NET_ALL            "all"

/* Bus Ripper Strings for RC code */
/* Ripper Rotation */
#define RC_STR_RIP_SYMMETRIC      "symmetric"
#define RC_STR_RIP_NON_SYMMETRIC  "non-symmetric"

/* Ripper Type */
#define RC_STR_RIP_COMPONENT   "component"
#define RC_STR_RIP_NET          RC_STR_NET_NET

/* Ripper Symbol Name */
#define DEFAULT_BUS_RIPPER_SYMNAME "busripper-1.sym"
#define SECOND_BUS_RIPPER_SYMNAME  "busripper-2.sym"

/* RC string related to the Pointer/Mouse */
/* Middle button options */
#define RC_STR_MID_STROKE      "Stroke"
#define RC_STR_MID_REPEAT      "Repeat"
#define RC_STR_MID_ACTION      "Action"
#define RC_STR_MID_MOUSEPAN    "Pan"
#define RC_STR_MID_MOUSEPOP    "Popup"

/* Third button Function */
#define RC_STR_3RD_POPUP       "Popup"
#define RC_STR_3RD_PAN         "Pan"

/* Scroll Wheel Mode */
#define RC_STR_SCROLL_GTK      "gtk"
#define RC_STR_SCROLL_CLASSIC  "classic"

/* RC string for TEXT related options */
/** String for Text Case */
#define RC_STR_TEXT_LOWER      "lower"
#define RC_STR_TEXT_UPPER      "upper"
#define RC_STR_TEXT_BOTH       "both"

/* RC String for Text Zoom Factor */
#define RC_STR_TXT_ALWAYS      "always"
#define RC_STR_TXT_READABLE    "only-when-readable"

/* RC string for Toolbar related options */
/** String for Toolbar Mode */
#define RC_STR_TB_ICONS        "only-icons"
#define RC_STR_TB_TEXT         "only-text"
#define RC_STR_TB_BOTH         "show-both"
#define RC_STR_TB_HORIZ        "show-both-horiz"
#define RC_STR_TB_LAST         "retention"

/* RC string for Undo options */
#define RC_STR_UNDO_NONE       RC_STR_NONE
#define RC_STR_UNDO_DISK       "disk"
#define RC_STR_UNDO_MEMORY     "memory"

#endif /* !_GSCHEM_IDEFINES_H_INCL */
