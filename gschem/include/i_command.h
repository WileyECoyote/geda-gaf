/*

clear (log screen)
import
install
reload
reset.
run
set
shell
[editor]
connect
close, new, save, save-as, save-all, revert, run, print

*/
#ifndef __ACTION_FLAGS__
#define __ACTION_FLAGS__
enum ActionFlag
{
    USE_WORKER_THREAD   = 0,
    USE_MAIN_LOOP       = 1 << 0,
};

#define ActionTaskMask 1

#endif

#ifndef I_DO_DECLARE

#ifndef COMMAND

#define COMMAND(func) void i_cmd_##func(GSCHEM_TOPLEVEL *w_current);

        COMMAND ( do_debug )
        COMMAND ( do_repeat_last )
        COMMAND ( do_file )
        COMMAND ( do_file_new )
        COMMAND ( do_file_new_window )
        COMMAND ( do_open )
        COMMAND ( do_save )
        COMMAND ( do_save_as )
        COMMAND ( do_save_all )
        COMMAND ( do_print )
        COMMAND ( do_write_image )
        COMMAND ( do_write_pdf )
        COMMAND ( do_run_script )
        COMMAND ( do_close )
        COMMAND ( do_quit )

        COMMAND ( do_edit )
        COMMAND ( do_undo )
        COMMAND ( do_redo )
        COMMAND ( do_cut_clip )
        COMMAND ( do_copy_clip )
        COMMAND ( do_paste_clip )
        COMMAND ( do_delete )
        COMMAND ( do_select )
        COMMAND ( do_select_all )
        COMMAND ( do_select_invert )
        COMMAND ( do_deselect )
        COMMAND ( do_copy )
        COMMAND ( do_mcopy )
        COMMAND ( do_move )
        COMMAND ( do_rotate )
        COMMAND ( do_mirror )
        COMMAND ( do_edit_butes )
        COMMAND ( do_edit_text )
        COMMAND ( do_edit_slot )
        COMMAND ( do_edit_color )
        COMMAND ( do_pintype )
        COMMAND ( do_linetype )
        COMMAND ( do_filltype )
        COMMAND ( do_translate )
        COMMAND ( do_lock )
        COMMAND ( do_unlock )
        COMMAND ( do_macro  )
        COMMAND ( do_embed )
        COMMAND ( do_unembed )
        COMMAND ( do_update )

        COMMAND ( do_view )
        COMMAND ( do_redraw )
        COMMAND ( do_pan )
        COMMAND ( do_zoom_box )
        COMMAND ( do_zoom_extents )
        COMMAND ( do_zoom_in )
        COMMAND ( do_zoom_out )
        COMMAND ( do_zoom_all )
        COMMAND ( do_documentation )
        COMMAND ( do_show_hidden )
        COMMAND ( do_show_nets )
        COMMAND ( do_dark_colors )
        COMMAND ( do_light_colors )
        COMMAND ( do_bw_colors )

        COMMAND ( do_page )
        COMMAND ( do_page_manager )
        COMMAND ( do_page_prev )
        COMMAND ( do_page_next )
        COMMAND ( do_page_new )
        COMMAND ( do_page_print )
        COMMAND ( do_page_revert )
        COMMAND ( do_page_close )
        COMMAND ( do_page_discard )

        COMMAND ( do_add )
        COMMAND ( do_add_component )
        COMMAND ( do_add_net )
        COMMAND ( do_add_bus )
        COMMAND ( do_add_attribute )
        COMMAND ( do_add_text )
        COMMAND ( do_add_line )
        COMMAND ( do_add_box )
        COMMAND ( do_add_circle )
        COMMAND ( do_add_arc )
        COMMAND ( do_add_pin )
        COMMAND ( do_add_picture )

        COMMAND ( do_down_schematic )
        COMMAND ( do_down_symbol )
        COMMAND ( do_hierarchy_up )

        COMMAND ( do_attach )
        COMMAND ( do_detach )
        COMMAND ( do_show_value )
        COMMAND ( do_show_name )
        COMMAND ( do_show_both )
        COMMAND ( do_toggle_visibility )

        COMMAND ( do_find_text )
        COMMAND ( do_hide_text )
        COMMAND ( do_show_text )
        COMMAND ( do_autonumber )

/* Toggles */
        COMMAND ( do_cycle_grid )
        COMMAND ( do_snap_up )
        COMMAND ( do_snap_down )
        COMMAND ( do_show_snap )

/* Toggles */
        COMMAND ( do_cycle_snap )
        COMMAND ( do_toggle_feedback )
        COMMAND ( do_toggle_rubberband )
        COMMAND ( do_toggle_magneticnet )

        COMMAND ( do_show_console )
        COMMAND ( do_show_coordinates )
        COMMAND ( do_show_text_size )
        COMMAND ( do_show_settings )

        COMMAND ( do_show_manual )
        COMMAND ( do_show_hotkeys )
        COMMAND ( do_show_faq )
        COMMAND ( do_show_geda)
        COMMAND ( do_show_wiki )
        COMMAND ( do_show_about )

     /* Accept variable name as a command */
        COMMAND ( draw_grips )
        COMMAND ( grid_mode )
        COMMAND ( dots_grid_dot_size )
        COMMAND ( dots_grid_fixed_threshold )
        COMMAND ( dots_grid_mode )
        COMMAND ( mesh_grid_threshold )
        COMMAND ( object_clipping )
        COMMAND ( scrollbars )
        COMMAND ( scrollbar_update )
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
        COMMAND ( auto_save_interval )
        COMMAND ( attribute_placement_grid )
        COMMAND ( continue_component_place )
        COMMAND ( embed_components )
        COMMAND ( enforce_hierarchy )
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
        COMMAND ( text_size )
        COMMAND ( undo_levels )
        COMMAND ( undo_control )
        COMMAND ( undo_panzoom )
        COMMAND ( undo_type )

#undef  COMMAND
#define _MAKE_COMMAND_ENUM_

/* MACRO( string-to-look-for, 0=once OR 1=recursive, not used, handler function base name */
#define COMMAND(action, rstr, flags, func) cmd_##func,

enum {
        cmd_unknown,
#endif

#else

 #define COMMAND(action, rstr, flags, func) extern void i_cmd_##func(GSCHEM_TOPLEVEL *w_current);
#endif

#include "i_actions.h"
/*! \note #1 set arg1 = 0 for a worker thread or arg1 = 1 to run in Gtk
 *           main-loop thread, i.e. if guile is to be called use 1 else use 0!
 *           we could use scm_init_guile for thread but manual states that this
 *           is not portable. (maybe some propeller-head can fix!)
 */
     COMMAND ( debug,               "debug",            USE_WORKER_THREAD,      do_debug)
     COMMAND ( repeat-last,         "",                 USE_MAIN_LOOP,          do_repeat_last)
/* Menu and Toolbar Commands */
     COMMAND ( file,                "",                 USE_MAIN_LOOP,          do_file)
     COMMAND ( FILE_NEW,            "",                 USE_MAIN_LOOP,          do_file_new)
     COMMAND ( FILE_NEW_WINDOW,     "",                 USE_MAIN_LOOP,          do_file_new_window)
     COMMAND ( FILE_OPEN,           "open",             USE_WORKER_THREAD,      do_open)
     COMMAND ( FILE_SAVE,           "",                 USE_WORKER_THREAD,      do_save)
     COMMAND ( FILE_SAVE_AS,        "save as",          USE_WORKER_THREAD,      do_save_as)
     COMMAND ( FILE_SAVE_ALL,       "",                 USE_MAIN_LOOP,          do_save_all)
     COMMAND ( FILE_PRINT,          "print",            USE_WORKER_THREAD,      do_print)
     COMMAND ( FILE_WRITE_IMAGE,    "write image",      USE_WORKER_THREAD,      do_write_image)
     COMMAND ( FILE_WRITE_PDF,      "write_pdf",        USE_WORKER_THREAD,      do_write_pdf)
     COMMAND ( FILE_RUN_SCRIPT,     "run_script",       USE_MAIN_LOOP,          do_run_script)
     COMMAND ( FILE_CLOSE,          "close",            USE_WORKER_THREAD,      do_close)
     COMMAND ( FILE_QUIT,           "",                 USE_WORKER_THREAD,      do_quit)

     COMMAND ( edit,                "",                 USE_WORKER_THREAD,      do_edit)
     COMMAND ( EDIT_UNDO,           "undo",             USE_MAIN_LOOP,          do_undo)
     COMMAND ( EDIT_REDO,           "redo",             USE_MAIN_LOOP,          do_redo)
     COMMAND ( EDIT_CB_CUT,         "cut",              USE_MAIN_LOOP,          do_cut_clip)
     COMMAND ( EDIT_CB_COPY,        "copy",             USE_MAIN_LOOP,          do_copy_clip)
     COMMAND ( EDIT_CB_PASTE,       "paste",            USE_MAIN_LOOP,          do_paste_clip)
     COMMAND ( EDIT_DELETE,         "delete",           USE_MAIN_LOOP,          do_delete)
     COMMAND ( EDIT_SELECT,         "",                 USE_MAIN_LOOP,          do_select)
     COMMAND ( EDIT_SELECT_ALL,     "select all",       USE_MAIN_LOOP,          do_select_all)
     COMMAND ( EDIT_INVERT,         "invert",           USE_MAIN_LOOP,          do_select_invert)
     COMMAND ( EDIT_DESELECT,       "deselect",         USE_MAIN_LOOP,          do_deselect)

     COMMAND ( EDIT_COPY,           "copy mode",        USE_WORKER_THREAD,      do_copy)
     COMMAND ( EDIT_MCOPY,          "multi-copy",       USE_WORKER_THREAD,      do_mcopy)
     COMMAND ( EDIT_MOVE,           "move",             USE_WORKER_THREAD,      do_move)
     COMMAND ( EDIT_ROTATE,         "rotate",           USE_WORKER_THREAD,      do_rotate)
     COMMAND ( EDIT_MIRROR,         "mirror",           USE_WORKER_THREAD,      do_mirror)
     COMMAND ( EDIT_ATTRIB,         "edit butes",       USE_WORKER_THREAD,      do_edit_butes)
     COMMAND ( EDIT_TEXT,           "edit text",        USE_WORKER_THREAD,      do_edit_text)
     COMMAND ( EDIT_SLOT,           "edit slot",        USE_WORKER_THREAD,      do_edit_slot)
     COMMAND ( EDIT_COLOR,          "edit color",       USE_WORKER_THREAD,      do_edit_color)
     COMMAND ( EDIT_PIN,            "pin type",         USE_WORKER_THREAD,      do_pintype)
     COMMAND ( EDIT_LINE,           "line type",        USE_WORKER_THREAD,      do_linetype)
     COMMAND ( EDIT_FILL,           "fill type",        USE_WORKER_THREAD,      do_filltype)
     COMMAND ( EDIT_TRANSLATE,      "translate",        USE_WORKER_THREAD,      do_translate)
     COMMAND ( EDIT_LOCK,           "lock",             USE_MAIN_LOOP,          do_lock)
     COMMAND ( EDIT_UNLOCK,         "unlock",           USE_WORKER_THREAD,      do_unlock)
     COMMAND ( EDIT_MACRO,          "macro",            USE_MAIN_LOOP,          do_macro )
     COMMAND ( EDIT_EMBED,          "embed",            USE_WORKER_THREAD,      do_embed)
     COMMAND ( EDIT_UNEMBED,        "unembed",          USE_WORKER_THREAD,      do_unembed)
     COMMAND ( EDIT_UPDATE,         "update",           USE_MAIN_LOOP,          do_update)

     COMMAND ( view,                "",                 USE_WORKER_THREAD,      do_view)
     COMMAND ( VIEW_REDRAW,         "redraw",           USE_MAIN_LOOP,          do_redraw)
     COMMAND ( VIEW_PAN,            "pan",              USE_MAIN_LOOP,          do_pan)
     COMMAND ( VIEW_BOX,            "",                 USE_MAIN_LOOP,          do_zoom_box)
     COMMAND ( VIEW_EXTENTS,        "",                 USE_MAIN_LOOP,          do_zoom_extents)
     COMMAND ( VIEW_ZOOM_IN,        "",                 USE_MAIN_LOOP,          do_zoom_in)
     COMMAND ( VIEW_ZOOM_OUT,       "",                 USE_MAIN_LOOP,          do_zoom_out)
     COMMAND ( VIEW_ZOOM_ALL,       "",                 USE_MAIN_LOOP,          do_zoom_all)
     COMMAND ( VIEW_DOCUMENT,       "documentation",    USE_MAIN_LOOP,          do_documentation)
     COMMAND ( VIEW_HIDDEN,         "show hidden",      USE_MAIN_LOOP,          do_show_hidden)
     COMMAND ( VIEW_NETS,           "show nets",        USE_MAIN_LOOP,          do_show_nets)
     COMMAND ( VIEW_DARK,           "",                 USE_MAIN_LOOP,          do_dark_colors)
     COMMAND ( VIEW_LIGHT,          "",                 USE_MAIN_LOOP,          do_light_colors)
     COMMAND ( VIEW_BLACK_WHITE,    "",                 USE_MAIN_LOOP,          do_bw_colors)

     COMMAND ( page,                "",                 USE_WORKER_THREAD,      do_page)
     COMMAND ( PAGE_MANAGER,        "page manager",     USE_MAIN_LOOP,          do_page_manager)
     COMMAND ( PAGE_PREV,           "",                 USE_MAIN_LOOP,          do_page_prev)
     COMMAND ( PAGE_NEXT,           "",                 USE_MAIN_LOOP,          do_page_next)
     COMMAND ( PAGE_NEW,            "",                 USE_MAIN_LOOP,          do_page_new)
     COMMAND ( PAGE_PRINT,          "page print",       USE_MAIN_LOOP,          do_page_print)
     COMMAND ( PAGE_REVERT,         "revert",           USE_MAIN_LOOP,          do_page_revert)
     COMMAND ( PAGE_CLOSE,          "close",            USE_MAIN_LOOP,          do_page_close)
     COMMAND ( PAGE_DISCARD,        "",                 USE_MAIN_LOOP,          do_page_discard)

     COMMAND ( add,                 "",                 USE_WORKER_THREAD,      do_add)
     COMMAND ( ADD_COMPONENT,       "component",        USE_WORKER_THREAD,      do_add_component)
     COMMAND ( ADD_NET,             "Net Mode",         USE_WORKER_THREAD,      do_add_net)
     COMMAND ( ADD_BUS,             "Bus",              USE_WORKER_THREAD,      do_add_bus)
     COMMAND ( ADD_ATTRIB,          "attribute",        USE_MAIN_LOOP,          do_add_attribute)
     COMMAND ( ADD_TEXT,            "text",             USE_WORKER_THREAD,      do_add_text)
     COMMAND ( ADD_LINE,            "Line Mode",        USE_WORKER_THREAD,      do_add_line)
     COMMAND ( ADD_BOX,             "Box Mode",         USE_WORKER_THREAD,      do_add_box)
     COMMAND ( ADD_CIRCLE,          "Circle Mode",      USE_WORKER_THREAD,      do_add_circle)
     COMMAND ( ADD_ARC,             "Arc Mode",         USE_WORKER_THREAD,      do_add_arc)
     COMMAND ( ADD_PIN,             "Pin Mode",         USE_WORKER_THREAD,      do_add_pin)
     COMMAND ( ADD_PICTURE,         "picture",          USE_WORKER_THREAD,      do_add_picture)

     COMMAND ( DOWN_SCHEMATIC,      "hierarchy down",   USE_WORKER_THREAD,      do_down_schematic)
     COMMAND ( DOWN_SYMBOL,         "hierarchy down",   USE_WORKER_THREAD,      do_down_symbol)
     COMMAND ( HIERARCHY_UP,        "hierarchy up",     USE_WORKER_THREAD,      do_hierarchy_up)

     COMMAND ( ATTRIB_ATTACH,       "attach",           USE_MAIN_LOOP,          do_attach)
     COMMAND ( ATTRIB_DETACH,       "detach",           USE_MAIN_LOOP,          do_detach)
     COMMAND ( ATTRIB_VALUE,        "show value",       USE_MAIN_LOOP,          do_show_value)
     COMMAND ( ATTRIB_NAME,         "show name",        USE_MAIN_LOOP,          do_show_name)
     COMMAND ( ATTRIB_BOTH,         "show both",        USE_MAIN_LOOP,          do_show_both)
     COMMAND ( ATTRIB_VISIBILITY,   "toggle visible",   USE_MAIN_LOOP,          do_toggle_visibility)

     COMMAND ( ATTRIB_FIND,         "find text",        USE_MAIN_LOOP,          do_find_text)
     COMMAND ( ATTRIB_HIDE,         "show text",        USE_MAIN_LOOP,          do_hide_text)
     COMMAND ( ATTRIB_SHOW,         "hide text",        USE_MAIN_LOOP,          do_show_text)
     COMMAND ( ATTRIB_AUTONUM,      "autonumber",       USE_MAIN_LOOP,          do_autonumber)

/* Options */
     COMMAND ( OPT_CYLCE_GRID,      "cycle grid",       USE_MAIN_LOOP,          do_cycle_grid)
     COMMAND ( OPT_SNAP_UP,         "snap up",          USE_MAIN_LOOP,          do_snap_up)
     COMMAND ( OPT_SNAP_DOWN,       "snap down",        USE_MAIN_LOOP,          do_snap_down)
     COMMAND ( OPT_SNAP_SIZE,       "show snap",        USE_MAIN_LOOP,          do_show_snap)

/* Toggles */
     COMMAND ( TOGGLE_SNAP,         "cycle_snap",       USE_WORKER_THREAD,      do_cycle_snap)
     COMMAND ( TOGGLE_FEEDBACK,     "feedback",         USE_WORKER_THREAD,      do_toggle_feedback)
     COMMAND ( TOGGLE_RUBBER,       "rubberband",       USE_WORKER_THREAD,      do_toggle_rubberband)
     COMMAND ( TOGGLE_MAGNETIC,     "magnetic net",     USE_WORKER_THREAD,      do_toggle_magneticnet)

     COMMAND ( OPT_CONSOLE,         "",                 USE_MAIN_LOOP,          do_show_console)
     COMMAND ( OPT_COORDINATES,     "",                 USE_WORKER_THREAD,      do_show_coordinates)
     COMMAND ( OPT_TEXT_SIZE,       "",                 USE_WORKER_THREAD,      do_show_text_size)
     COMMAND ( OPT_SETTINGS,        "Preferences",      USE_MAIN_LOOP,          do_show_settings)

/* Help */
     COMMAND ( HELP_MANUAL,         "",                 USE_WORKER_THREAD,      do_show_manual)
     COMMAND ( HELP_HOTKEYS,        "",                 USE_MAIN_LOOP,          do_show_hotkeys)
     COMMAND ( HELP_FAQ,            "",                 USE_WORKER_THREAD,      do_show_faq)
     COMMAND ( HELP_GEDA,           "",                 USE_WORKER_THREAD,      do_show_geda)
     COMMAND ( HELP_WIKI,           "",                 USE_WORKER_THREAD,      do_show_wiki)
     COMMAND ( HELP_ABOUT,          "",                 USE_WORKER_THREAD,      do_show_about)

     /* Accept variable name as a command */
     COMMAND ( draw-grips,                   "", USE_WORKER_THREAD, draw_grips)
     COMMAND ( grid-mode,                    "", USE_WORKER_THREAD, grid_mode)
     COMMAND ( dots-grid-dot-size,           "", USE_WORKER_THREAD, dots_grid_dot_size)
     COMMAND ( dots-grid-fixed-threshold,    "", USE_WORKER_THREAD, dots_grid_fixed_threshold)
     COMMAND ( dots-grid-mode,               "", USE_WORKER_THREAD, dots_grid_mode)
     COMMAND ( mesh-grid-threshold,          "", USE_WORKER_THREAD, mesh_grid_threshold)
     COMMAND ( object-clipping,              "", USE_WORKER_THREAD, object_clipping)
     COMMAND ( scrollbars,                   "", USE_WORKER_THREAD, scrollbars)
     COMMAND ( scrollbar-update,             "", USE_WORKER_THREAD, scrollbar_update)
     COMMAND ( scrollpan-steps,              "", USE_WORKER_THREAD, scrollpan_steps)
     COMMAND ( warp-cursor,                  "", USE_WORKER_THREAD, warp_cursor)
     COMMAND ( world-size,                   "", USE_WORKER_THREAD, world_size)
     COMMAND ( zoom-gain,                    "", USE_WORKER_THREAD, zoom_gain)
     COMMAND ( zoom-with-pan,                "", USE_WORKER_THREAD, zoom_with_pan)
     COMMAND ( logging,                      "", USE_WORKER_THREAD, logging)
     COMMAND ( log-destiny,                  "", USE_WORKER_THREAD, log_destiny)
     COMMAND ( console-window,               "", USE_WORKER_THREAD, console_window)
     COMMAND ( console-window-type,          "", USE_WORKER_THREAD, console_window_type)
     COMMAND ( action-feedback-mode,         "", USE_WORKER_THREAD, action_feedback_mode)
     COMMAND ( add-attribute-offset,         "", USE_WORKER_THREAD, add_attribute_offset)
     COMMAND ( auto-load-last,               "", USE_WORKER_THREAD, auto_load_last)
     COMMAND ( auto-save-interval,           "", USE_WORKER_THREAD, auto_save_interval)
     COMMAND ( attribute-placement-grid,     "", USE_WORKER_THREAD, attribute_placement_grid)
     COMMAND ( continue-component-place,     "", USE_WORKER_THREAD, continue_component_place)
     COMMAND ( embed-components,             "", USE_WORKER_THREAD, embed_components)
     COMMAND ( enforce-hierarchy,            "", USE_WORKER_THREAD, enforce_hierarchy)
     COMMAND ( file-preview,                 "", USE_WORKER_THREAD, file_preview)
     COMMAND ( force-boundingbox,            "", USE_WORKER_THREAD, force_boundingbox)
     COMMAND ( keyboardpan-gain,             "", USE_WORKER_THREAD, keyboardpan_gain)
     COMMAND ( magnetic-net-mode,            "", USE_WORKER_THREAD, magnetic_net_mode)
     COMMAND ( netconn-rubberband,           "", USE_WORKER_THREAD, netconn_rubberband)
     COMMAND ( raise-dialog-boxes-on-expose, "", USE_WORKER_THREAD, raise_dialog_boxes)
     COMMAND ( select-slack-pixels,          "", USE_WORKER_THREAD, select_slack_pixels)
     COMMAND ( snap-size,                    "", USE_WORKER_THREAD, snap_size)
     COMMAND ( sort-component-library,       "", USE_WORKER_THREAD, sort_component_library)
     COMMAND ( untitled-name,                "", USE_WORKER_THREAD, untitled_name)
     COMMAND ( net-consolidate,              "", USE_WORKER_THREAD, net_consolidate)
     COMMAND ( net-endpoint-mode,            "", USE_WORKER_THREAD, net_endpoint_mode)
     COMMAND ( net-midpoint-mode,            "", USE_WORKER_THREAD, net_midpoint_mode)
     COMMAND ( net-direction-mode,           "", USE_WORKER_THREAD, net_direction_mode)
     COMMAND ( net-selection-mode,           "", USE_WORKER_THREAD, net_selection_mode)
     COMMAND ( bus-style,                    "", USE_WORKER_THREAD, bus_style)
     COMMAND ( net-style,                    "", USE_WORKER_THREAD, net_style)
     COMMAND ( pin-style,                    "", USE_WORKER_THREAD, pin_style)
     COMMAND ( line-style,                   "", USE_WORKER_THREAD, line_style)
     COMMAND ( thick-bus-width,              "", USE_WORKER_THREAD, thick_bus_width)
     COMMAND ( thick-line-width,             "", USE_WORKER_THREAD, thick_line_width)
     COMMAND ( thick-net-width,              "", USE_WORKER_THREAD, thick_net_width)
     COMMAND ( thick-pin-width,              "", USE_WORKER_THREAD, thick_pin_width)
     COMMAND ( thin-bus-width,               "", USE_WORKER_THREAD, thin_bus_width)
     COMMAND ( thin-line-width,              "", USE_WORKER_THREAD, thin_line_width)
     COMMAND ( thin-net-width,               "", USE_WORKER_THREAD, thin_net_width)
     COMMAND ( thin-pin-width,               "", USE_WORKER_THREAD, thin_pin_width)
     COMMAND ( bus-ripper-rotation,          "", USE_WORKER_THREAD, bus_ripper_rotation)
     COMMAND ( bus-ripper-size,              "", USE_WORKER_THREAD, bus_ripper_size)
     COMMAND ( bus-ripper-type,              "", USE_WORKER_THREAD, bus_ripper_type)
     COMMAND ( bus-ripper-symname,           "", USE_WORKER_THREAD, bus_ripper_symname)
     COMMAND ( fast-mousepan,                "", USE_WORKER_THREAD, fast_mousepan)
     COMMAND ( drag-can-move,                "", USE_WORKER_THREAD, drag_can_move)
     COMMAND ( middle-button,                "", USE_WORKER_THREAD, middle_button)
     COMMAND ( third-button,                 "", USE_WORKER_THREAD, third_button)
     COMMAND ( mousepan-gain,                "", USE_WORKER_THREAD, mousepan_gain)
     COMMAND ( scroll-wheel,                 "", USE_WORKER_THREAD, scroll_wheel)
     COMMAND ( image-color,                  "", USE_WORKER_THREAD, image_color)
     COMMAND ( invert-images,                "", USE_WORKER_THREAD, invert_images)
     COMMAND ( text-case,                    "", USE_WORKER_THREAD, text_case)
     COMMAND ( text-display-zoomfactor,      "", USE_WORKER_THREAD, text_display_zoomfactor)
     COMMAND ( text-feedback,                "", USE_WORKER_THREAD, text_feedback)
     COMMAND ( text-origin-marker,           "", USE_WORKER_THREAD, text_origin_marker)
     COMMAND ( text-marker-size,             "", USE_WORKER_THREAD, text_marker_size)
     COMMAND ( text-size,                    "", USE_WORKER_THREAD, text_size)
     COMMAND ( undo-levels,                  "", USE_WORKER_THREAD, undo_levels)
     COMMAND ( undo-control,                 "", USE_WORKER_THREAD, undo_control)
     COMMAND ( undo-panzoom,                 "", USE_WORKER_THREAD, undo_panzoom)
     COMMAND ( undo-type,                    "", USE_WORKER_THREAD, undo_type)

#ifndef I_DO_DECLARE
#ifdef _MAKE_COMMAND_ENUM_
     COMMAND_COUNT,
};
#undef _MAKE_COMMAND_ENUM_
#endif
#endif
#undef COMMAND
#undef I_DO_DECLARE