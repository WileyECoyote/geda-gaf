/* C header
;  File: gschem_sdefines.h
;;
;;; gEDA - GPL Electronic Design Automation
;;; gschem - gEDA Schematic Capture
;;; Copyright (C) 1998-2012 Ales Hvezda
;;; Copyright (C) 1998-2012 gEDA Contributors (see ChangeLog for details)
;;
;;; Copyright (C) 2012 Wiley Edward Hill <wileyhill@gmail.com>
;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;
;;; Date: Aug, 22, 2012
;;; Contributing Author: Wiley Edward Hill
;;
*/
/************************ REVISION HISTORY *************************
;; Who |   When   |  What (Why)
;; ------------------------------------------------------------------
;; WEH | 09/17/12 |  Inital release.
;; ------------------------------------------------------------------
;; WEH | 12/30/12 |  Added msgbox (for debugging).
;; ------------------------------------------------------------------
;;
*/

#ifndef _GSCHEM_SDEFINES_H_INCL
#define _GSCHEM_SDEFINES_H_INCL

#ifndef _WIN32

/* Logs a normal message. */
#define s_log_message g_message

/* Can use like: msgbox("example = [%d]", g_list_length(geda_list_get_glist(toplevel->pages))) */
#define msgbox(...)generic_msg_dialog(g_strdup_printf(__VA_ARGS__));

#endif

#define SCHEMATIC_FILE_SUFFIX      "sch"
#define SCHEMATIC_FILE_DOT_SUFFIX  ".sch"
#define SCHEMATIC_FILTER           "*.sch"

#define SYMBOL_FILE_SUFFIX         "sym"
#define SYMBOL_FILE_DOT_SUFFIX     ".sym"
#define SYMBOL_FILTER              "*.sym"

#define TITLE_BLOCK_PATH           "/sym/titleblock"
#define DEFAULT_UNTITLED_NAME      "untitled"

/* Filenames for Stock Color Maps  */
#define DARK_COLOR_MAP   	   "gschem-colormap-darkbg"
#define LIGHT_COLOR_MAP 	   "gschem-colormap-lightbg"
#define BW_COLOR_MAP 		   "gschem-colormap-bw"
#define CUSTOM_COLOR_MAP           "gschem-colormap-custom"

/* For x_window.c */
#define GSCHEM_THEME_ICON_NAME     "geda-gschem"

/* -----------------  Strings for RC Read/Write ----------------- */

/* Generic Strings for RC code */
#define RC_STR_NONE     "none"

/* Generic Boolean Strings for RC code */
#define RC_STR_ENABLED  "enabled"
#define RC_STR_DISABLED "disabled"

/* Grid Mode Strings for RC code */
#define RC_STR_DOTS "dots"
#define RC_STR_MESH "mesh"

/* Dots Grid Mode Strings for RC code */
#define RC_STR_DOTS_MODE_VARIABLE "variable"
#define RC_STR_DOTS_MODE_FIXED    "fixed"

/* ScrollBar Update Strings for RC code */
#define RC_STR_BARS_CONTINUOUS "continuous"
#define RC_STR_BARS_DELAYED    "delayed"

/* WindowSize Strings for RC code */
#define RC_STR_WINDOW_W650H487  "window-size 650 487)  ; Good size for 800x600"
#define RC_STR_WINDOW_W900H650  "window-size 900 650)  ; Good size for 1024x768"
#define RC_STR_WINDOW_W950H712  "window-size 950 712)  ; Good size for 1152x864"
#define RC_STR_WINDOW_W1100H825 "window-size 1100 825) ; Good size for 1280x1024"

/* WorldSize Strings for RC code */
#define RC_STR_WORLD_SMALL  "60.0 45.0 1.0"
#define RC_STR_WORLD_MEDIUM "120.0 90.0 1.0"
#define RC_STR_WORLD_LARGE  "180.0 135.0 1.0"

/* Log Destiny Strings for RC code */
#define RC_STR_DESTINY_WINDOW "log_window"
#define RC_STR_DESTINY_TTY    "tty"
#define RC_STR_DESTINY_BOTH   "both"

/* Log Window Type Strings for RC code */
#define RC_STR_LOGWIN_DECORATED "decorated"
#define RC_STR_LOGWIN_TRANSIENT "transient"

/* Action Feedback Mode Strings for RC code */
#define RC_STR_FEEDBACK_OUTLINE  "outline"
#define RC_STR_FEEDBACK_BOUNDBOX "boundingbox"

/* Net POINT MODE Strings for RC code */
#define RC_STR_NET_NONE   "none"
#define RC_STR_FILLED_BOX "filledbox"
#define RC_STR_EMPTY_BOX  "empty"

#define RC_STR_NET_NET "net"
#define RC_STR_NET_ALL "all"

/* RC string related to the Pointer/Mouse */
/* Middle button options */
#define RC_STR_MID_STROKE   "stroke"
#define RC_STR_MID_REPEAT   "repeat"
#define RC_STR_MID_ACTION   "action"
#define RC_STR_MID_MOUSEPAN "mousepan"

/* Third button Function */
#define RC_STR_3RD_POPUP    "popup"
#define RC_STR_3RD_PAN	    "mousepan"

/* Scroll Wheel Mode */
#define RC_STR_SCROLL_GTK     "gtk"
#define RC_STR_SCROLL_CLASSIC "classic"

/* RC string for TEXT related options */
/** String for Text Case */
#define RC_STR_TEXT_LOWER "lower"
#define RC_STR_TEXT_UPPER "upper"
#define RC_STR_TEXT_BOTH  "both"

/* RC String for Text Zoom Factor */
#define RC_STR_TXT_ALWAYS   "always"
#define RC_STR_TXT_READABLE "only-when-readable"

/* RC string for Undo options */
#define RC_STR_UNDO_DISK    "disk"
#define RC_STR_UNDO_MEMORY  "memory"

#endif /* !_GSCHEM_IDEFINES_H_INCL */


