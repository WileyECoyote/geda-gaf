/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/i_action.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 * Date Contributed: February, 02, 2013
 *
 */
/*! \file i_actions.h
 *
 *  \brief List all Builtin User Interface Commands/Actions
 *  \par Description
 *   Maybe a temporary scheme used to synchronize the action
 *  "text". With the old system, all actions in gschem were
 *   dependent on finding and successfully reading a Scheme
 *   RC file, which has proved both unstable and unreliable,
 *   particularly on nix's due to stricter enforcement of file
 *   "permissions" and there is no control over the contents of
 *   these files. Maybe the Menu definitions need their own file
 *   instead of mixing with global RC options.
 *
 *   This file provides a common "compiled-in list that's ensures
 *   UI actions generated from input systems, such as the toolbars
 *   or Menus, will pass a valid action string to the command
 *   processor. Maybe not the best idea, was/is a "quick fix"
 *
 *   Also note, action strings can and are combined with strings
 *   like "button", "window", combobox, etc, after any hyphen
 *   have been stripped, and used as descriptions and names for
 *   ATK objects, along with tooltip text and widget names, to
 *   support accessiblity beyond the basic GTK-2 provisions.
 *   For an example of how this is implemented see "geda_tool
 *   bars.h" in the toplevel include folder. Therefore, action
 *   strings should be chosen carefully.
 *
 */

 #if defined(OS_WIN32_NATIVE) || defined(_WIN32) || defined(__MINGW32__)
 #  undef FILE_OPEN /* ..\include\windef.h: */
 #endif

#define QUOTE_SYMBOL(symbol) #symbol
#define ACTION(...) QUOTE_SYMBOL(__VA_ARGS__)

#define FILE_NEW          file-new
#define FILE_NEW_WINDOW   file-new-window
#define FILE_OPEN         file-open
#define FILE_SAVE         file-save
#define FILE_SAVE_AS      file-save-as
#define FILE_SAVE_ALL     file-save-all
#define FILE_SAVE_MODS    file-save-modified
#define FILE_PRINT        file-print
#define FILE_WRITE_IMAGE  file-write-image
#define FILE_WRITE_PDF    file-write-pdf
#define FILE_RUN_SCRIPT   file-run-script
#define FILE_CLOSE        file-close
#define FILE_CLOSE_ALL    file-close-all
#define FILE_QUIT         file-quit

#define EXPORT_SYMBOL     export-symbol
#define EXPORT_PICTURE    export-picture

#define EDIT_UNDO         edit-undo
#define EDIT_REDO         edit-redo
#define EDIT_CB_CUT       clipboard-cut
#define EDIT_CB_COPY      clipboard-copy
#define EDIT_BUF_cut      clipboard-cut
#define EDIT_BUF_copy     clipboard-copy
#define EDIT_BUF_paste    clipboard-paste
#define EDIT_CB_PASTE     clipboard-paste
#define EDIT_DELETE       edit-delete
#define EDIT_COPY         edit-copy
#define EDIT_MCOPY        edit-mcopy
#define EDIT_MIRROR       edit-mirror
#define EDIT_MOVE         edit-move
#define EDIT_OFFSET       edit-offset
#define EDIT_ROTATE_LEFT  edit-rotate-left
#define EDIT_ROTATE_RIGHT edit-rotate-right
#define EDIT_SNAP         edit-snap
#define EDIT_ARRAY        edit-array
#define EDIT_BREAK        edit-break
#define EDIT_EXTEND       edit-extend
#define EDIT_ARC          edit-arc
#define EDIT_ATTRIB       edit-attributes
#define EDIT_TEXT         edit-text
#define EDIT_COLOR        edit-color
#define EDIT_PIN          edit-pintype
#define EDIT_LINE         edit-linetype
#define EDIT_FILL         edit-filltype
#define EDIT_SLOT         edit-slot
#define EDIT_COMPONENT    edit-component

#define EDIT_SELECT       edit-select
#define EDIT_SELECT_ALL   edit-select-all
#define EDIT_INVERT       edit-select-invert
#define EDIT_DESELECT     edit-deselect
#define EDIT_DESELECT_ALL edit-deselect-all
#define EDIT_SELECT_LAST  edit-select-last

#define EDIT_LOCK         edit-lock
#define EDIT_UNLOCK       edit-unlock

#define VIEW_REDRAW       view-redraw
#define VIEW_PAN          view-pan
#define VIEW_BOX          view-zoom-box
#define VIEW_SELECTED     view-zoom-selected
#define VIEW_EXTENTS      view-zoom-extents
#define VIEW_ZOOM_IN      view-zoom-in
#define VIEW_ZOOM_OUT     view-zoom-out
#define VIEW_ZOOM_ALL     view-zoom-all
#define VIEW_ZOOM_MAG     view-zoom-to-mag
#define VIEW_DOCUMENT     view-documentation
#define VIEW_HIDDEN       view-show-hidden
#define VIEW_INHERITED    view-show-inherited
#define VIEW_NETS         view-show-nets
#define VIEW_DARK         view-dark-colors
#define VIEW_LIGHT        view-light-colors
#define VIEW_BLACK_WHITE  view-bw-colors

#define PAGE_DRAW_AFTER   page-draw-after
#define PAGE_DRAW_BEFORE  page-draw-before
#define PAGE_DRAW_FIRST   page-draw-first
#define PAGE_DRAW_LAST    page-draw-last

#define PAGE_MANAGER      page-manager
#define PAGE_FIRST        page-first
#define PAGE_PREV         page-prev
#define PAGE_NEXT         page-next
#define PAGE_UP           page-up
#define PAGE_DOWN         page-down
#define PAGE_LAST         page-last
#define PAGE_NEW          page-new
#define PAGE_PRINT        page-print
#define PAGE_REVERT       page-revert
#define PAGE_REVERT_ALL   page-revert-all
#define PAGE_CLOSE        page-close
#define PAGE_DISCARD      page-discard
#define DOWN_SCHEMATIC    hierarchy-down-schematic
#define DOWN_SYMBOL       hierarchy-down-symbol
#define HIERARCHY_UP      hierarchy-up

#define ADD_COMPONENT     add-component
#define ADD_NET           add-net
#define ADD_BUS           add-bus
#define ADD_ATTRIB        add-attribute
#define ADD_SOURCE        add-source-attribute
#define ADD_TEXT          add-text
#define ADD_LINE          add-line
#define ADD_PIN           add-pin
#define ADD_BOX           add-box
#define ADD_CIRCLE        add-circle
#define ADD_ARC           add-arc
#define ADD_PATH          add-path
#define ADD_PICTURE       add-picture

#define SESSION_NEW       session-new
#define SESSION_OPEN      session-open
#define SESSION_SAVE      session-save
#define SESSION_SAVE_AS   session-save-as
#define SESSION_MANAGE    session-manage

#define ATTRIB_ATTACH        attributes-attach
#define ATTRIB_DETACH        attributes-detach
#define ATTRIB_HOME          attributes-home
#define ATTRIB_VALUE         attributes-show-value
#define ATTRIB_NAME          attributes-show-name
#define ATTRIB_BOTH          attributes-show-both
#define ATTRIB_VISIBILITY    attributes-visibility

#define ATTRIB_FIND          attributes-find-text
#define ATTRIB_HIDE          attributes-hide-text
#define ATTRIB_SHOW          attributes-show-text
#define ATTRIB_PATCH         attributes-patch
#define ATTRIB_EDIT          attributes-editor

/* Tools */
#define TOOLS_AUTONUM        tools-autonumber
#define TOOLS_CONSOLE        tools-show-console
#define TOOLS_COORDINATES    tools-show-coordinates
#define TOOLS_GUILE          tools-guile-path
#define TOOLS_MACRO          tools-invoke-macro
#define TOOLS_TRANSLATE      tools-translate

#define TOOLS_EMBED          tools-embed
#define TOOLS_UNEMBED        tools-unembed
#define TOOLS_UPDATE         tools-update

/* Grid */
#define OPT_GRID_DOT     options-grid-dot
#define OPT_GRID_MESH    options-grid-mesh
#define OPT_GRID_OFF     options-grid-off

/* Toggles */
#define OPT_CYLCE_GRID   options-cycle-grid
#define OPT_SNAP_UP      scale-up-snap-size
#define OPT_SNAP_DOWN    scale-down-snap-size
#define OPT_SNAP_SIZE    options-snap-size
#define OPT_SNAP_OFF     options-snap-off
#define OPT_SNAP_ON      options-snap-on

/* Toggles */
#define CYCLE_SNAP       options-cycle-snap
#define TOGGLE_RUBBER    options-rubberband
#define TOGGLE_MAGNETIC  options-magneticnet
#define TOGGLE_DRAG_MOVE options-dragcanmove
#define TOGGLE_FEEDBACK  options-action-feedback
#define TOGGLE_AUTO_PAN  options-auto-pan

#define OPT_TEXT_SIZE    options-show-text-size
#define OPT_SETTINGS     options-show-settings

#define HELP_MANUAL      help-show-manual
#define HELP_HOTKEYS     help-show-hotkeys
#define HELP_FAQ         help-show-faq
#define HELP_GEDA        help-show-geda
#define HELP_WIKI        help-show-wiki
#define HELP_ABOUT       help-show-about
