/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/i_command.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2010 Ales Hvezda
 * Copyright (C) 2013-2015 Wiley Edward Hill
 *
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
/*!
 * \file i_command.h
 *
 * \brief header for the command interface module
 *
 * \warning Unless you really know what your doing - Don't do it.
 *          This file is included twice in i_command.c and once in
 *          i_callbacks.c and is compiled differently each time!
 */
/*
clear (log screen)
install
reload
reset.
run
set
shell
[editor]
connect

*/
#ifndef __ACTION_FLAGS__
#define __ACTION_FLAGS__

/*! \enum ActionFlag
 *   Action flags are used as argument 3 to the Command macro in the second group
 *   in this file, which initializes the command_struc.aflag in i_command.c.
 *   The enumerator determines what type of task will be spawned to complete the
 *   action, see i_command_router(), and whether the cursor coordinates will be
 *   captured in i_command_process(). Basically, if the action handler is going
 *   to use the CMD_X or CMD_Y macros then the enumerated XY version should be
 *   used (to set the correct bits in the action flag field).
 */
enum ActionFlag
{
    USE_WORKER_THREAD   = 0,
    USE_MAIN_LOOP       = 1 << 0,
    USE_INLINE_MODE     = 1 << 1,
    USE_XY_WORKER       = 1 << 2,
    USE_XY_MAIN_LOOP    = 5
};

#define ActionTaskMask 1
#define XY_ActionMask  4

#endif /* __ACTION_FLAGS__ */

#ifndef I_DO_DECLARE

#ifndef COMMAND

/* define Macro for declarations if I_DO_DECLARE is not defined */
#define COMMAND(func) void i_cmd_##func(GschemToplevel *w_current);

        COMMAND ( do_debug )
        COMMAND ( do_repeat_last )

/* File */
        COMMAND ( do_file )
        COMMAND ( do_file_new )
        COMMAND ( do_file_new_window )
        COMMAND ( do_open )
        COMMAND ( do_save )
        COMMAND ( do_save_as )
        COMMAND ( do_save_all )
        COMMAND ( do_save_mods )
        COMMAND ( do_print )
        COMMAND ( do_write_image )
        COMMAND ( do_write_pdf )
        COMMAND ( do_run_script )
        COMMAND ( do_close )
        COMMAND ( do_close_all )
        COMMAND ( do_quit )

/* File/Export */
        COMMAND ( do_export_symbol )
        COMMAND ( do_export_picture )

/* Edit */
        COMMAND ( do_edit )
        COMMAND ( do_undo )
        COMMAND ( do_redo )
        COMMAND ( do_cut_clip )
        COMMAND ( do_copy_clip )
        COMMAND ( do_paste_clip )
        COMMAND ( do_delete )
        COMMAND ( do_copy )
        COMMAND ( do_mcopy )
        COMMAND ( do_mirror )
        COMMAND ( do_move )
        COMMAND ( do_offset )
        COMMAND ( do_rotate_left )
        COMMAND ( do_rotate_right )
        COMMAND ( do_snap )
        COMMAND ( do_array )
        COMMAND ( do_break )
        COMMAND ( do_extend )
        COMMAND ( do_edit_butes )
        COMMAND ( do_edit_ponent )
        COMMAND ( do_edit_text )
        COMMAND ( do_edit_slot )
        COMMAND ( do_edit_color )
        COMMAND ( do_edit_arc )
        COMMAND ( do_pintype )
        COMMAND ( do_linetype )
        COMMAND ( do_filltype )
        COMMAND ( do_lock )
        COMMAND ( do_unlock )

/* Select */
        COMMAND ( do_select )
        COMMAND ( do_select_all )
        COMMAND ( do_select_invert )
        COMMAND ( do_select_last )
        COMMAND ( do_deselect )
        COMMAND ( do_deselect_all )

/* View */
        COMMAND ( do_view )
        COMMAND ( do_redraw )
        COMMAND ( do_pan )
        COMMAND ( do_zoom_box )
        COMMAND ( do_zoom_selected )
        COMMAND ( do_zoom_extents )
        COMMAND ( do_zoom_in )
        COMMAND ( do_zoom_out )
        COMMAND ( do_zoom_all )
        COMMAND ( do_zoom_to_mag )
        COMMAND ( do_documentation )
        COMMAND ( do_show_hidden )
        COMMAND ( do_show_inherited )
        COMMAND ( do_show_nets )
        COMMAND ( do_dark_colors )
        COMMAND ( do_light_colors )
        COMMAND ( do_bw_colors )

/* Page */
        COMMAND ( do_page )
        COMMAND ( do_draw_after )
        COMMAND ( do_draw_before )
        COMMAND ( do_draw_first )
        COMMAND ( do_draw_last )
        COMMAND ( do_page_manager )
        COMMAND ( do_page_first )
        COMMAND ( do_page_prev )
        COMMAND ( do_page_next )
        COMMAND ( do_page_up )
        COMMAND ( do_page_down )
        COMMAND ( do_page_last )
        COMMAND ( do_page_new )
        COMMAND ( do_page_print )
        COMMAND ( do_page_revert )
        COMMAND ( do_page_revert_all )
        COMMAND ( do_page_close )
        COMMAND ( do_page_discard )

        COMMAND ( do_down_schematic )
        COMMAND ( do_down_symbol )
        COMMAND ( do_hierarchy_up )

/* Add */
        COMMAND ( do_add )
        COMMAND ( do_add_component )
        COMMAND ( do_add_net )
        COMMAND ( do_add_bus )
        COMMAND ( do_add_attribute )
        COMMAND ( do_add_text )
        COMMAND ( do_add_line )
        COMMAND ( do_add_pin )
        COMMAND ( do_add_box )
        COMMAND ( do_add_circle )
        COMMAND ( do_add_arc )
        COMMAND ( do_add_path )
        COMMAND ( do_add_picture )

/* Sessions */
        COMMAND ( do_session_new )
        COMMAND ( do_session_open )
        COMMAND ( do_session_save )
        COMMAND ( do_session_save_as )
        COMMAND ( do_session_manage )

/* Attributes */
        COMMAND ( do_attach )
        COMMAND ( do_detach )
        COMMAND ( do_home_attributes )
        COMMAND ( do_show_value )
        COMMAND ( do_show_name )
        COMMAND ( do_show_both )
        COMMAND ( do_toggle_visibility )

        COMMAND ( do_find_text )
        COMMAND ( do_hide_text )
        COMMAND ( do_show_text )
        COMMAND ( do_attributes )

/* Tools */
        COMMAND ( do_autonumber )
        COMMAND ( do_show_console )
        COMMAND ( do_show_coordinates )
        COMMAND ( do_macro )
        COMMAND ( do_guile_path )
        COMMAND ( do_translate )
        COMMAND ( do_embed )
        COMMAND ( do_unembed )
        COMMAND ( do_update )

/* Options */
/* Grid Snap */
        COMMAND ( do_grid_dots )
        COMMAND ( do_grid_mesh )
        COMMAND ( do_grid_off )

/* Toggles */
        COMMAND ( do_cycle_grid )
        COMMAND ( do_snap_up )
        COMMAND ( do_snap_down )
        COMMAND ( do_show_snap )
        COMMAND ( do_snap_off )
        COMMAND ( do_snap_on )

/* Toggles */
        COMMAND ( do_cycle_snap )
        COMMAND ( do_toggle_rubberband )
        COMMAND ( do_toggle_magneticnet )
        COMMAND ( do_toggle_dragcanmove )
        COMMAND ( do_toggle_feedback )
        COMMAND ( do_toggle_auto_pan )

        COMMAND ( do_show_text_size )
        COMMAND ( do_show_settings )

        COMMAND ( do_show_manual )
        COMMAND ( do_show_hotkeys )
        COMMAND ( do_show_faq )
        COMMAND ( do_show_geda )
        COMMAND ( do_show_wiki )
        COMMAND ( do_show_about )

     /* Accept variable name as a command */
        COMMAND ( draw_grips )
        COMMAND ( grid_mode )
        COMMAND ( dots_grid_dot_size )
        COMMAND ( dots_grid_threshold )
        COMMAND ( dots_grid_mode )
        COMMAND ( mesh_grid_threshold )
        COMMAND ( object_clipping )
        COMMAND ( scrollbars )
        COMMAND ( scrollbar_update )
        COMMAND ( scrollbars_visible )
        COMMAND ( scrollpan_steps )
        COMMAND ( warp_cursor )
        COMMAND ( world_size )
        COMMAND ( zoom_gain )
        COMMAND ( zoom_with_pan )
        COMMAND ( logging )
        COMMAND ( log_destiny )
        COMMAND ( console_window )
        COMMAND ( console_window_type )
        COMMAND ( action_feedback_mode )
        COMMAND ( add_attribute_offset )
        COMMAND ( auto_load_last )
        COMMAND ( auto_pan )
        COMMAND ( auto_pan_step )
        COMMAND ( auto_save_interval )
        COMMAND ( attribute_placement_grid )
        COMMAND ( continue_component_place )
        COMMAND ( embed_components )
        COMMAND ( enforce_hierarchy )
        COMMAND ( hierarchy_up_close )
        COMMAND ( file_preview )
        COMMAND ( force_boundingbox )
        COMMAND ( keyboardpan_gain )
        COMMAND ( magnetic_net_mode )
        COMMAND ( netconn_rubberband )
        COMMAND ( raise_dialog_boxes )
        COMMAND ( select_slack_pixels )
        COMMAND ( snap_size )
        COMMAND ( sort_component_library )
        COMMAND ( untitled_name )
        COMMAND ( net_consolidate )
        COMMAND ( net_endpoint_mode )
        COMMAND ( net_midpoint_mode )
        COMMAND ( net_direction_mode )
        COMMAND ( net_selection_mode )

        COMMAND ( bus_style )
        COMMAND ( net_style )
        COMMAND ( pin_style )
        COMMAND ( line_style )
        COMMAND ( thick_bus_width )
        COMMAND ( thick_line_width )
        COMMAND ( thick_net_width )
        COMMAND ( thick_pin_width )
        COMMAND ( thin_bus_width )
        COMMAND ( thin_line_width )
        COMMAND ( thin_net_width )
        COMMAND ( thin_pin_width )
        COMMAND ( bus_ripper_rotation )
        COMMAND ( bus_ripper_size )
        COMMAND ( bus_ripper_type )
        COMMAND ( bus_ripper_symname )
        COMMAND ( fast_mousepan )
        COMMAND ( drag_can_move )
        COMMAND ( middle_button )
        COMMAND ( third_button )
        COMMAND ( mousepan_gain )
        COMMAND ( scroll_wheel )
        COMMAND ( image_color )
        COMMAND ( invert_images )
        COMMAND ( text_case )
        COMMAND ( text_display_zoomfactor )
        COMMAND ( text_feedback )
        COMMAND ( text_origin_marker )
        COMMAND ( text_marker_size )
        COMMAND ( text_marker_threshold )
        COMMAND ( text_size )
        COMMAND ( undo_control )
        COMMAND ( undo_levels )
        COMMAND ( undo_panzoom )
        COMMAND ( undo_preserve )
        COMMAND ( undo_type )

#undef  COMMAND

#define _MAKE_COMMAND_ENUM_

/* MACRO Enumerate function base name on this pass */
#define COMMAND(action, rstr, flags, func) cmd_##func,

enum {
        cmd_unknown,

#endif  /* COMMAND was not defined when the header was loaded so enumerate */

#else   /* I_DO_DECLARE was not defined so filling a command structure */

/* MACRO( string-to-look-for, 0=once OR 1=recursive, not used, handler function base name */
 #define COMMAND(action, rstr, flags, func) extern void i_cmd_##func(GschemToplevel *w_current);
#endif  /* I_DO_DECLARE */

#include "i_actions.h" /* Note: Need to resolve action strings on this pass */

/*! \note #1 set aflag = 0 for a worker thread or aflag = 1 to run in Gtk
 *           main-loop thread, i.e. if guile is to be called use 1, else use
 *           0, or their corresponding XY versions if the action can come from
 *           either ID_ORIGIN_KEYBOARD or ID_ORIGIN_MOUSE. If all else fails,
 *           use the USE_INLINE_MODE to disable threading for that action! If
 *           USE_INLINE_MODE does not work then something else is wrong some
 *           where, is not a threading issue. Actually, if USE_INLINE_MODE
 *           works and the others do not, then there is probably something
 *           wrong somewhere, for example inadvertent recursion in a signal
 *           handler. This might turn up when threading if a mutex blocks the
 *           reentering thread access to a variable that the first thread is
 *           waiting on to change state, maybe something like: while(i<#).
 *           The single threading mode will mask this problem, but that does
 *           not mean code in not flawed!
 *           If code leaves C, executes guile, returns to C, and C then runs
 *           new GTK code, particulary the filechooserdialog, as oppose to a
 *           custom dialog, then after returning to C the orginal GTK code
 *           will not run, appears to lock up - GTK is doing this on purpose.
 *           This can be handled by using USE_MAIN_LOOP, and wrapping Gtk
 *           between gdk_threads_enter and gdk_threads_leave. Examples are
 *           in x_image_setup and x_fileselect_load_backup.
 *           Or use USE_INLINE_MODE. (WEH: So far I have been able to avoid
 *           USE_INLINE_MODE by fixing the problem or using wrappers!)
 *
 *           We could use scm_init_guile (which does not seem to work)
 *           for threading but manual states that this is not portable.
 *           (WEH: maybe some propeller-head can fix!)
 *
 *           Action handlers, of any type, are not reentrant. They will wait
 *           up to MAX_WAIT_FOR_ACTION for the code to become available before
 *           reporting an issues, and then give up, see function BlockThread.
 */
     COMMAND ( debug,               "debug",            USE_MAIN_LOOP,          do_debug)
     COMMAND ( repeat-last,         NULL,               USE_MAIN_LOOP,          do_repeat_last)
/* Menu and Toolbar Commands */
     COMMAND ( file,                NULL,               USE_MAIN_LOOP,          do_file)
     COMMAND ( FILE_NEW,            NULL,               USE_MAIN_LOOP,          do_file_new)
     COMMAND ( FILE_NEW_WINDOW,     NULL,               USE_MAIN_LOOP,          do_file_new_window)
     COMMAND ( FILE_OPEN,           "open",             USE_WORKER_THREAD,      do_open)
     COMMAND ( FILE_SAVE,           NULL,               USE_WORKER_THREAD,      do_save)
     COMMAND ( FILE_SAVE_AS,        "save as",          USE_WORKER_THREAD,      do_save_as)
     COMMAND ( FILE_SAVE_ALL,       NULL,               USE_MAIN_LOOP,          do_save_all)
     COMMAND ( FILE_SAVE_MODS,      NULL,               USE_MAIN_LOOP,          do_save_mods)
     COMMAND ( FILE_PRINT,          "print",            USE_WORKER_THREAD,      do_print)
     COMMAND ( FILE_WRITE_IMAGE,    "write image",      USE_WORKER_THREAD,      do_write_image)
/* never works with selection inline*/
     COMMAND ( FILE_WRITE_PDF,      "write pdf",        USE_WORKER_THREAD,      do_write_pdf)
     COMMAND ( FILE_RUN_SCRIPT,     "run script",       USE_MAIN_LOOP,          do_run_script)
     COMMAND ( FILE_CLOSE,          "close",            USE_WORKER_THREAD,      do_close)
     COMMAND ( FILE_CLOSE_ALL,      "close all",        USE_WORKER_THREAD,      do_close_all)
     COMMAND ( FILE_QUIT,           NULL,               USE_WORKER_THREAD,      do_quit)

     COMMAND ( EXPORT_SYMBOL,       "export symbol",    USE_WORKER_THREAD,      do_export_symbol)
     COMMAND ( EXPORT_PICTURE,      "export picture",   USE_WORKER_THREAD,      do_export_picture)

     COMMAND ( edit,                NULL,               USE_WORKER_THREAD,      do_edit)
     COMMAND ( EDIT_UNDO,           "undo",             USE_MAIN_LOOP,          do_undo)
     COMMAND ( EDIT_REDO,           "redo",             USE_MAIN_LOOP,          do_redo)
     COMMAND ( EDIT_CB_CUT,         "cut",              USE_MAIN_LOOP,          do_cut_clip)
     COMMAND ( EDIT_CB_COPY,        "copy",             USE_MAIN_LOOP,          do_copy_clip)
     COMMAND ( EDIT_CB_PASTE,       "paste",            USE_XY_WORKER,          do_paste_clip)
     COMMAND ( EDIT_DELETE,         "delete",           USE_MAIN_LOOP,          do_delete)
     COMMAND ( EDIT_COPY,           "copy mode",        USE_XY_WORKER,          do_copy)
     COMMAND ( EDIT_MCOPY,          "multi-copy",       USE_XY_WORKER,          do_mcopy)
     COMMAND ( EDIT_MIRROR,         "mirror",           USE_XY_MAIN_LOOP,       do_mirror)
     COMMAND ( EDIT_MOVE,           "move",             USE_XY_WORKER,          do_move)
     COMMAND ( EDIT_OFFSET,         "offset",           USE_XY_WORKER,          do_offset)
     COMMAND ( EDIT_ROTATE_LEFT,    "rotate",           USE_XY_MAIN_LOOP,       do_rotate_left)
     COMMAND ( EDIT_ROTATE_RIGHT,   "rotate",           USE_XY_MAIN_LOOP,       do_rotate_right)
     COMMAND ( EDIT_SNAP,           "snap",             USE_XY_MAIN_LOOP,       do_snap)
     COMMAND ( EDIT_ARRAY,          "array",            USE_XY_MAIN_LOOP,       do_array)
     COMMAND ( EDIT_BREAK,          "break",            USE_XY_MAIN_LOOP,       do_break)
     COMMAND ( EDIT_EXTEND,         "extend",           USE_XY_WORKER,          do_extend)
     COMMAND ( EDIT_ATTRIB,         "edit butes",       USE_WORKER_THREAD,      do_edit_butes)
     COMMAND ( EDIT_COMPONENT,      "edit ponents",     USE_WORKER_THREAD,      do_edit_ponent)
     COMMAND ( EDIT_TEXT,           "edit text",        USE_WORKER_THREAD,      do_edit_text)
     COMMAND ( EDIT_SLOT,           "edit slot",        USE_WORKER_THREAD,      do_edit_slot)
     COMMAND ( EDIT_COLOR,          "edit color",       USE_WORKER_THREAD,      do_edit_color)
     COMMAND ( EDIT_ARC,            "edit arc",         USE_WORKER_THREAD,      do_edit_arc )
     COMMAND ( EDIT_PIN,            "pin type",         USE_WORKER_THREAD,      do_pintype)
     COMMAND ( EDIT_LINE,           "line type",        USE_WORKER_THREAD,      do_linetype)
     COMMAND ( EDIT_FILL,           "fill type",        USE_WORKER_THREAD,      do_filltype)
     COMMAND ( EDIT_LOCK,           "lock",             USE_MAIN_LOOP,          do_lock)
     COMMAND ( EDIT_UNLOCK,         "unlock",           USE_WORKER_THREAD,      do_unlock)

     COMMAND ( EDIT_SELECT,         NULL,               USE_MAIN_LOOP,          do_select)
     COMMAND ( EDIT_SELECT_ALL,     "select all",       USE_MAIN_LOOP,          do_select_all)
     COMMAND ( EDIT_INVERT,         "invert",           USE_MAIN_LOOP,          do_select_invert)
     COMMAND ( EDIT_SELECT_LAST,    NULL,               USE_WORKER_THREAD,      do_select_last)
     COMMAND ( EDIT_DESELECT,       "deselect",         USE_MAIN_LOOP,          do_deselect)
     COMMAND ( EDIT_DESELECT_ALL,   "deselect-all",     USE_MAIN_LOOP,          do_deselect_all)

     COMMAND ( view,                NULL,               USE_WORKER_THREAD,      do_view)
     COMMAND ( VIEW_REDRAW,         "redraw",           USE_MAIN_LOOP,          do_redraw)
     COMMAND ( VIEW_PAN,            "pan",              USE_MAIN_LOOP,          do_pan)
     COMMAND ( VIEW_BOX,            NULL,               USE_XY_MAIN_LOOP,       do_zoom_box)
     COMMAND ( VIEW_SELECTED,       NULL,               USE_WORKER_THREAD,      do_zoom_selected)
     COMMAND ( VIEW_EXTENTS,        NULL,               USE_WORKER_THREAD,      do_zoom_extents)
     COMMAND ( VIEW_ZOOM_IN,        NULL,               USE_XY_WORKER,          do_zoom_in)
     COMMAND ( VIEW_ZOOM_OUT,       NULL,               USE_XY_WORKER,          do_zoom_out)
     COMMAND ( VIEW_ZOOM_ALL,       NULL,               USE_WORKER_THREAD,      do_zoom_all)
     COMMAND ( VIEW_ZOOM_MAG,       "magnification",    USE_XY_WORKER,          do_zoom_to_mag)
     COMMAND ( VIEW_DOCUMENT,       "documentation",    USE_MAIN_LOOP,          do_documentation)
     COMMAND ( VIEW_HIDDEN,         "show hidden",      USE_MAIN_LOOP,          do_show_hidden)
     COMMAND ( VIEW_INHERITED,      "show inherited",   USE_MAIN_LOOP,          do_show_inherited)
     COMMAND ( VIEW_NETS,           "show nets",        USE_MAIN_LOOP,          do_show_nets)
     COMMAND ( VIEW_DARK,           NULL,               USE_MAIN_LOOP,          do_dark_colors)
     COMMAND ( VIEW_LIGHT,          NULL,               USE_MAIN_LOOP,          do_light_colors)
     COMMAND ( VIEW_BLACK_WHITE,    NULL,               USE_MAIN_LOOP,          do_bw_colors)

     COMMAND ( page,                NULL,               USE_WORKER_THREAD,      do_page)
     COMMAND ( PAGE_DRAW_AFTER,     "draw after",       USE_WORKER_THREAD,      do_draw_after)
     COMMAND ( PAGE_DRAW_BEFORE,    "draw before",      USE_WORKER_THREAD,      do_draw_before)
     COMMAND ( PAGE_DRAW_FIRST,     "draw first",       USE_WORKER_THREAD,      do_draw_first)
     COMMAND ( PAGE_DRAW_LAST,      "draw last",        USE_WORKER_THREAD,      do_draw_last)
     COMMAND ( PAGE_MANAGER,        "page manager",     USE_MAIN_LOOP,          do_page_manager)
     COMMAND ( PAGE_FIRST,          NULL,               USE_MAIN_LOOP,          do_page_first)
     COMMAND ( PAGE_PREV,           NULL,               USE_MAIN_LOOP,          do_page_prev)
     COMMAND ( PAGE_NEXT,           NULL,               USE_MAIN_LOOP,          do_page_next)
     COMMAND ( PAGE_UP,             NULL,               USE_MAIN_LOOP,          do_page_up)
     COMMAND ( PAGE_DOWN,           NULL,               USE_MAIN_LOOP,          do_page_down)
     COMMAND ( PAGE_LAST,           NULL,               USE_MAIN_LOOP,          do_page_last)
     COMMAND ( PAGE_NEW,            NULL,               USE_MAIN_LOOP,          do_page_new)
     COMMAND ( PAGE_PRINT,          "page print",       USE_MAIN_LOOP,          do_page_print)
     COMMAND ( PAGE_REVERT,         "revert",           USE_WORKER_THREAD,      do_page_revert)
     COMMAND ( PAGE_REVERT_ALL,     "revert all",       USE_WORKER_THREAD,      do_page_revert_all)
     COMMAND ( PAGE_CLOSE,          "close",            USE_WORKER_THREAD,      do_page_close)
     COMMAND ( PAGE_DISCARD,        NULL,               USE_MAIN_LOOP,          do_page_discard)

     COMMAND ( DOWN_SCHEMATIC,      "hierarchy down",   USE_WORKER_THREAD,      do_down_schematic)
     COMMAND ( DOWN_SYMBOL,         "hierarchy down",   USE_MAIN_LOOP,          do_down_symbol)
     COMMAND ( HIERARCHY_UP,        "hierarchy up",     USE_WORKER_THREAD,      do_hierarchy_up)

     COMMAND ( add,                 NULL,               USE_XY_WORKER,          do_add)
     COMMAND ( ADD_COMPONENT,       "component",        USE_XY_WORKER,          do_add_component)
     COMMAND ( ADD_NET,             "Net Mode",         USE_XY_WORKER,          do_add_net)
     COMMAND ( ADD_BUS,             "Bus",              USE_XY_WORKER,          do_add_bus)
     COMMAND ( ADD_ATTRIB,          "attribute",        USE_XY_MAIN_LOOP,       do_add_attribute)
     COMMAND ( ADD_TEXT,            "text",             USE_XY_WORKER,          do_add_text)
     COMMAND ( ADD_LINE,            "Line Mode",        USE_XY_WORKER,          do_add_line)
     COMMAND ( ADD_PIN,             "Pin Mode",         USE_XY_WORKER,          do_add_pin)
     COMMAND ( ADD_BOX,             "Box Mode",         USE_XY_WORKER,          do_add_box)
     COMMAND ( ADD_CIRCLE,          "Circle Mode",      USE_XY_WORKER,          do_add_circle)
     COMMAND ( ADD_ARC,             "Arc Mode",         USE_XY_MAIN_LOOP,       do_add_arc)
     COMMAND ( ADD_PATH,            "Path Mode",        USE_XY_WORKER,          do_add_path)
     COMMAND ( ADD_PICTURE,         "picture",          USE_XY_WORKER,          do_add_picture)

     COMMAND ( SESSION_NEW,         "new session",      USE_XY_WORKER,          do_session_new)
     COMMAND ( SESSION_OPEN,        "open session",     USE_XY_WORKER,          do_session_open)
     COMMAND ( SESSION_SAVE,        "save session",     USE_XY_WORKER,          do_session_save)
     COMMAND ( SESSION_SAVE_AS,     "save session as",  USE_XY_WORKER,          do_session_save_as)
     COMMAND ( SESSION_MANAGE,      "manage sessions",  USE_XY_WORKER,          do_session_manage)

     COMMAND ( ATTRIB_ATTACH,       "attach",           USE_MAIN_LOOP,          do_attach)
     COMMAND ( ATTRIB_DETACH,       "detach",           USE_MAIN_LOOP,          do_detach)
     COMMAND ( ATTRIB_HOME,         "home attributes",  USE_MAIN_LOOP,          do_home_attributes)
     COMMAND ( ATTRIB_VALUE,        "show value",       USE_MAIN_LOOP,          do_show_value)
     COMMAND ( ATTRIB_NAME,         "show name",        USE_MAIN_LOOP,          do_show_name)
     COMMAND ( ATTRIB_BOTH,         "show both",        USE_MAIN_LOOP,          do_show_both)
     COMMAND ( ATTRIB_VISIBILITY,   "toggle visible",   USE_MAIN_LOOP,          do_toggle_visibility)

     COMMAND ( ATTRIB_FIND,         "find text",        USE_MAIN_LOOP,          do_find_text)
     COMMAND ( ATTRIB_HIDE,         "hide text",        USE_MAIN_LOOP,          do_hide_text)
     COMMAND ( ATTRIB_SHOW,         "show text",        USE_MAIN_LOOP,          do_show_text)
     COMMAND ( ATTRIB_EDIT,         "edit attrib",      USE_WORKER_THREAD,      do_attributes)

/* Tools */
     COMMAND ( TOOLS_AUTONUM,       "autonumber",       USE_MAIN_LOOP,          do_autonumber)
     COMMAND ( TOOLS_CONSOLE,       NULL,               USE_MAIN_LOOP,          do_show_console)
     COMMAND ( TOOLS_COORDINATES,   NULL,               USE_WORKER_THREAD,      do_show_coordinates)
     COMMAND ( TOOLS_MACRO,         "macro",            USE_MAIN_LOOP,          do_macro)
     COMMAND ( TOOLS_GUILE,         NULL,               USE_WORKER_THREAD,      do_guile_path)
     COMMAND ( TOOLS_TRANSLATE,     "translate",        USE_WORKER_THREAD,      do_translate)
     COMMAND ( TOOLS_EMBED,         "embed",            USE_WORKER_THREAD,      do_embed)
     COMMAND ( TOOLS_UNEMBED,       "unembed",          USE_WORKER_THREAD,      do_unembed)
     COMMAND ( TOOLS_UPDATE,        "update",           USE_MAIN_LOOP,          do_update)

/* Options */

/* Grid Snap Options Actions  */
     COMMAND ( OPT_GRID_DOT,        "grid dot",         USE_MAIN_LOOP,          do_grid_dots)
     COMMAND ( OPT_GRID_MESH,       "grid mesh",        USE_MAIN_LOOP,          do_grid_mesh)
     COMMAND ( OPT_GRID_OFF,        "grid off",         USE_MAIN_LOOP,          do_grid_off)
     COMMAND ( OPT_CYLCE_GRID,      "cycle grid",       USE_MAIN_LOOP,          do_cycle_grid)

     COMMAND ( OPT_SNAP_UP,         "snap up",          USE_MAIN_LOOP,          do_snap_up)
     COMMAND ( OPT_SNAP_DOWN,       "snap down",        USE_MAIN_LOOP,          do_snap_down)
     COMMAND ( OPT_SNAP_SIZE,       "show snap",        USE_MAIN_LOOP,          do_show_snap)
     COMMAND ( OPT_SNAP_OFF,        "options-snap-off", USE_MAIN_LOOP,          do_snap_off)
     COMMAND ( OPT_SNAP_ON,         "options-snap-on",  USE_MAIN_LOOP,          do_snap_on)

/* Toggle Actions */
     COMMAND ( CYCLE_SNAP,          "cycle_snap",       USE_WORKER_THREAD,      do_cycle_snap)
     COMMAND ( TOGGLE_RUBBER,       "rubberband",       USE_WORKER_THREAD,      do_toggle_rubberband)
     COMMAND ( TOGGLE_MAGNETIC,     "magnetic net",     USE_WORKER_THREAD,      do_toggle_magneticnet)
     COMMAND ( TOGGLE_DRAG_MOVE,    "drag can move",    USE_WORKER_THREAD,      do_toggle_dragcanmove)
     COMMAND ( TOGGLE_FEEDBACK,     "feedback",         USE_WORKER_THREAD,      do_toggle_feedback)
     COMMAND ( TOGGLE_AUTO_PAN,     "auto pan",         USE_WORKER_THREAD,      do_toggle_auto_pan)

     COMMAND ( OPT_TEXT_SIZE,       NULL,               USE_WORKER_THREAD,      do_show_text_size)
     COMMAND ( OPT_SETTINGS,        "Preferences",      USE_MAIN_LOOP,          do_show_settings)

/* Help */
     COMMAND ( HELP_MANUAL,         NULL,               USE_WORKER_THREAD,      do_show_manual)
     COMMAND ( HELP_HOTKEYS,        NULL,               USE_MAIN_LOOP,          do_show_hotkeys)
     COMMAND ( HELP_FAQ,            NULL,               USE_WORKER_THREAD,      do_show_faq)
     COMMAND ( HELP_GEDA,           NULL,               USE_WORKER_THREAD,      do_show_geda)
     COMMAND ( HELP_WIKI,           NULL,               USE_WORKER_THREAD,      do_show_wiki)
     COMMAND ( HELP_ABOUT,          NULL,               USE_WORKER_THREAD,      do_show_about)

     /* Accept variable name as a command */
     COMMAND ( draw-grips,                   NULL, USE_WORKER_THREAD, draw_grips)
     COMMAND ( grid-mode,                    NULL, USE_WORKER_THREAD, grid_mode)
     COMMAND ( dots-grid-dot-size,           NULL, USE_WORKER_THREAD, dots_grid_dot_size)
     COMMAND ( dots-grid-threshold,          NULL, USE_WORKER_THREAD, dots_grid_threshold)
     COMMAND ( dots-grid-mode,               NULL, USE_WORKER_THREAD, dots_grid_mode)
     COMMAND ( mesh-grid-threshold,          NULL, USE_WORKER_THREAD, mesh_grid_threshold)
     COMMAND ( object-clipping,              NULL, USE_WORKER_THREAD, object_clipping)
     COMMAND ( scrollbars,                   NULL, USE_WORKER_THREAD, scrollbars)
     COMMAND ( scrollbar-update,             NULL, USE_WORKER_THREAD, scrollbar_update)
     COMMAND ( scrollbars-visible,           NULL, USE_WORKER_THREAD, scrollbars_visible)
     COMMAND ( scrollpan-steps,              NULL, USE_WORKER_THREAD, scrollpan_steps)
     COMMAND ( warp-cursor,                  NULL, USE_WORKER_THREAD, warp_cursor)
     COMMAND ( world-size,                   NULL, USE_WORKER_THREAD, world_size)
     COMMAND ( zoom-gain,                    NULL, USE_WORKER_THREAD, zoom_gain)
     COMMAND ( zoom-with-pan,                NULL, USE_WORKER_THREAD, zoom_with_pan)
     COMMAND ( logging,                      NULL, USE_WORKER_THREAD, logging)
     COMMAND ( log-destiny,                  NULL, USE_WORKER_THREAD, log_destiny)
     COMMAND ( console-window,               NULL, USE_WORKER_THREAD, console_window)
     COMMAND ( console-window-type,          NULL, USE_WORKER_THREAD, console_window_type)
     COMMAND ( action-feedback-mode,         NULL, USE_WORKER_THREAD, action_feedback_mode)
     COMMAND ( add-attribute-offset,         NULL, USE_WORKER_THREAD, add_attribute_offset)
     COMMAND ( auto-load-last,               NULL, USE_WORKER_THREAD, auto_load_last)
     COMMAND ( auto-pan,                     NULL, USE_WORKER_THREAD, auto_pan)
     COMMAND ( auto-pan-step,                NULL, USE_WORKER_THREAD, auto_pan_step)
     COMMAND ( auto-save-interval,           NULL, USE_WORKER_THREAD, auto_save_interval)
     COMMAND ( attribute-placement-grid,     NULL, USE_WORKER_THREAD, attribute_placement_grid)
     COMMAND ( continue-component-place,     NULL, USE_WORKER_THREAD, continue_component_place)
     COMMAND ( embed-components,             NULL, USE_WORKER_THREAD, embed_components)
     COMMAND ( enforce-hierarchy,            NULL, USE_WORKER_THREAD, enforce_hierarchy)
     COMMAND ( hierarchy-up-close,           NULL, USE_WORKER_THREAD, hierarchy_up_close)
     COMMAND ( file-preview,                 NULL, USE_WORKER_THREAD, file_preview)
     COMMAND ( force-boundingbox,            NULL, USE_WORKER_THREAD, force_boundingbox)
     COMMAND ( keyboardpan-gain,             NULL, USE_WORKER_THREAD, keyboardpan_gain)
     COMMAND ( magnetic-net-mode,            NULL, USE_WORKER_THREAD, magnetic_net_mode)
     COMMAND ( netconn-rubberband,           NULL, USE_WORKER_THREAD, netconn_rubberband)
     COMMAND ( raise-dialog-boxes-on-expose, NULL, USE_WORKER_THREAD, raise_dialog_boxes)
     COMMAND ( select-slack-pixels,          NULL, USE_WORKER_THREAD, select_slack_pixels)
     COMMAND ( snap-size,                    NULL, USE_WORKER_THREAD, snap_size)
     COMMAND ( sort-component-library,       NULL, USE_WORKER_THREAD, sort_component_library)
     COMMAND ( untitled-name,                NULL, USE_WORKER_THREAD, untitled_name)
     COMMAND ( net-consolidate,              NULL, USE_WORKER_THREAD, net_consolidate)
     COMMAND ( net-endpoint-mode,            NULL, USE_WORKER_THREAD, net_endpoint_mode)
     COMMAND ( net-midpoint-mode,            NULL, USE_WORKER_THREAD, net_midpoint_mode)
     COMMAND ( net-direction-mode,           NULL, USE_WORKER_THREAD, net_direction_mode)
     COMMAND ( net-selection-mode,           NULL, USE_WORKER_THREAD, net_selection_mode)
     COMMAND ( bus-style,                    NULL, USE_WORKER_THREAD, bus_style)
     COMMAND ( net-style,                    NULL, USE_WORKER_THREAD, net_style)
     COMMAND ( pin-style,                    NULL, USE_WORKER_THREAD, pin_style)
     COMMAND ( line-style,                   NULL, USE_WORKER_THREAD, line_style)
     COMMAND ( thick-bus-width,              NULL, USE_WORKER_THREAD, thick_bus_width)
     COMMAND ( thick-line-width,             NULL, USE_WORKER_THREAD, thick_line_width)
     COMMAND ( thick-net-width,              NULL, USE_WORKER_THREAD, thick_net_width)
     COMMAND ( thick-pin-width,              NULL, USE_WORKER_THREAD, thick_pin_width)
     COMMAND ( thin-bus-width,               NULL, USE_WORKER_THREAD, thin_bus_width)
     COMMAND ( thin-line-width,              NULL, USE_WORKER_THREAD, thin_line_width)
     COMMAND ( thin-net-width,               NULL, USE_WORKER_THREAD, thin_net_width)
     COMMAND ( thin-pin-width,               NULL, USE_WORKER_THREAD, thin_pin_width)
     COMMAND ( bus-ripper-rotation,          NULL, USE_WORKER_THREAD, bus_ripper_rotation)
     COMMAND ( bus-ripper-size,              NULL, USE_WORKER_THREAD, bus_ripper_size)
     COMMAND ( bus-ripper-type,              NULL, USE_WORKER_THREAD, bus_ripper_type)
     COMMAND ( bus-ripper-symname,           NULL, USE_WORKER_THREAD, bus_ripper_symname)
     COMMAND ( fast-mousepan,                NULL, USE_WORKER_THREAD, fast_mousepan)
     COMMAND ( drag-can-move,                NULL, USE_WORKER_THREAD, drag_can_move)
     COMMAND ( middle-button,                NULL, USE_WORKER_THREAD, middle_button)
     COMMAND ( third-button,                 NULL, USE_WORKER_THREAD, third_button)
     COMMAND ( mousepan-gain,                NULL, USE_WORKER_THREAD, mousepan_gain)
     COMMAND ( scroll-wheel,                 NULL, USE_WORKER_THREAD, scroll_wheel)
     COMMAND ( image-color,                  NULL, USE_WORKER_THREAD, image_color)
     COMMAND ( invert-images,                NULL, USE_WORKER_THREAD, invert_images)
     COMMAND ( text-case,                    NULL, USE_WORKER_THREAD, text_case)
     COMMAND ( text-display-zoomfactor,      NULL, USE_WORKER_THREAD, text_display_zoomfactor)
     COMMAND ( text-feedback,                NULL, USE_WORKER_THREAD, text_feedback)
     COMMAND ( text-origin-marker,           NULL, USE_WORKER_THREAD, text_origin_marker)
     COMMAND ( text-marker-size,             NULL, USE_WORKER_THREAD, text_marker_size)
     COMMAND ( text-marker-threshold,        NULL, USE_WORKER_THREAD, text_marker_threshold)
     COMMAND ( text-size,                    NULL, USE_WORKER_THREAD, text_size)
     COMMAND ( undo-control,                 NULL, USE_WORKER_THREAD, undo_control)
     COMMAND ( undo-levels,                  NULL, USE_WORKER_THREAD, undo_levels)
     COMMAND ( undo-panzoom,                 NULL, USE_WORKER_THREAD, undo_panzoom)
     COMMAND ( undo-preserve,                NULL, USE_WORKER_THREAD, undo_preserve)
     COMMAND ( undo-type,                    NULL, USE_WORKER_THREAD, undo_type)

#ifndef I_DO_DECLARE
#ifdef _MAKE_COMMAND_ENUM_
     COMMAND_COUNT,
};
#undef _MAKE_COMMAND_ENUM_
#endif
#endif
#undef COMMAND
#undef I_DO_DECLARE