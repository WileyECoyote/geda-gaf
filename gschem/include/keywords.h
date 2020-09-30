/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/keywords.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 2012-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: Aug, 17, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */
/*!
 * \file keywords.h
 *
 * \brief header used by the settings module
 * The file is used by the setting module to regenerate rc files.
 */
/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 10/31/12 |  Inital release.
 * ------------------------------------------------------------------
 * WEH | 12/02/12 |  Renamed autoplace_attributes_grid to attribute_
 *                |  placement-grid
 * ------------------------------------------------------------------
 * WEH | 01/06/16 |  Setup remaining Net Ripper variables (to complete
 *     |          |  the integration of the Ripper settings on the
 *     |          |  configure settings dailog).
 * WEH | 03/26/16 |  Add auto_pan and auto_pan_step (to support new
 *                |  keywords).
 * ------------------------------------------------------------------
*/
/*************************** CAUTION! ******************************/
/*
 * This is a Multi-Pass header file. This file is included twice by
 * x_setting.c and contains macros references to variables defined
 * in in either x_setting.c or x_setting.h
 *
 * Notes:
 *
 *    1.) Functions are defined in x_setting.c with "do_kw_" prefix
 *        (The function can be referenced with enumerator - but this
 *        is not done here, at least not directly.)
 *
 *    2.) In the code body, KEYWORD(variable) refers to the unquoted
 *        keyword. Example KEYWORD(log_destiny) yields log-destiny
 *
 *    3.) The SCM version doesn't really have to match the C version.
 *        keyword_struc.func is called when keyword_struc.name is
 *        encountered in the data.
 *
 *    4.) 1st pass = Declarations and Enumeration. 2nd pass uses the
 *        enumerators to populates structure with strings and pointers
 *        to functions
 */

#ifndef KEYWORD
#define KEYWORD(func) void do_kw_##func(GschemToplevel *w_current, FILE* input, FILE* output);

     KEYWORD ( define_in_rc )
     KEYWORD ( load_in_rc )
     KEYWORD ( render_adaptor)
     KEYWORD ( action_color )
     KEYWORD ( anti_aliasing )
     KEYWORD ( draw_grips )
     KEYWORD ( grip_size )
     KEYWORD ( grid_mode )
     KEYWORD ( dots_grid_dot_size )
     KEYWORD ( dots_grid_mode )
     KEYWORD ( dots_grid_threshold )
     KEYWORD ( dots_grid_minor_alpha )
     KEYWORD ( dots_grid_major_alpha )
     KEYWORD ( mesh_grid_threshold )
     KEYWORD ( mesh_line_width_factor )
     KEYWORD ( mesh_grid_minor_alpha )
     KEYWORD ( mesh_grid_major_alpha )
     //KEYWORD ( mesh_grid_minor_color )
     //KEYWORD ( mesh_grid_major_color )
     KEYWORD ( object_clipping )
     KEYWORD ( scrollbars )
     KEYWORD ( scrollbar_update )
     KEYWORD ( scrollbars_visible )
     KEYWORD ( scrollpan_steps )
     KEYWORD ( warp_cursor )
     KEYWORD ( window_size )
     KEYWORD ( world_size )
     KEYWORD ( zoom_gain )
     KEYWORD ( zoom_with_pan )
     KEYWORD ( logging )
     KEYWORD ( log_destiny )
     KEYWORD ( console_window )
     KEYWORD ( console_window_type )
     KEYWORD ( action_feedback_mode )
     KEYWORD ( add_attribute_offset )
     KEYWORD ( attribute_placement_grid )
     KEYWORD ( auto_load_last )
     KEYWORD ( auto_pan )
     KEYWORD ( auto_pan_step )
     KEYWORD ( auto_save_interval )
     KEYWORD ( component_dialog_attributes )
     KEYWORD ( continue_component_place )
     KEYWORD ( embed_components )
     KEYWORD ( enforce_hierarchy )
     KEYWORD ( file_preview )
     KEYWORD ( force_boundingbox )
     KEYWORD ( keyboardpan_gain )
     KEYWORD ( magnetic_net_mode )
     KEYWORD ( netconn_rubberband )
     KEYWORD ( raise_dialog_boxes )
     KEYWORD ( select_slack_pixels )
     KEYWORD ( snap_size )
     KEYWORD ( sort_component_library )
//     KEYWORD ( untitled_name )
     KEYWORD ( net_consolidate )
     KEYWORD ( net_endpoint_mode )
     KEYWORD ( net_midpoint_mode )
     KEYWORD ( net_direction_mode )
     KEYWORD ( net_selection_mode )

     KEYWORD ( bus_style )
     KEYWORD ( net_style )
     KEYWORD ( pin_style )
     KEYWORD ( line_style )

     KEYWORD ( thick_bus_width )
     KEYWORD ( thick_line_width )
     KEYWORD ( thick_net_width )
     KEYWORD ( thick_pin_width )

     KEYWORD ( thin_bus_width )
     KEYWORD ( thin_line_width )
     KEYWORD ( thin_net_width )
     KEYWORD ( thin_pin_width )

     KEYWORD ( bus_ripper_rotation )
     KEYWORD ( bus_ripper_size )
     KEYWORD ( bus_ripper_type )
     KEYWORD ( bus_ripper_symname )

     KEYWORD ( fast_mousepan )
     KEYWORD ( drag_can_move )
     KEYWORD ( middle_button )
     KEYWORD ( third_button )
     KEYWORD ( mousepan_gain )
     KEYWORD ( scroll_wheel )

     KEYWORD ( image_color )
     KEYWORD ( invert_images )
     KEYWORD ( text_case )
     KEYWORD ( text_display_zoomfactor )
     KEYWORD ( text_feedback )
     KEYWORD ( text_origin_marker )
     KEYWORD ( text_marker_size )
     KEYWORD ( text_marker_threshold )
     KEYWORD ( text_size )
     KEYWORD ( undo_control )
     KEYWORD ( undo_levels )
     KEYWORD ( undo_panzoom )
     KEYWORD ( undo_preserve )
     KEYWORD ( undo_type )
     KEYWORD ( attribute_name )

#undef KEYWORD
#define _MAKE_KEYWORD_ENUM_

/* MACRO( string-to-look-for, 0=once OR 1=recursive, not used, handler function base name */
#define KEYWORD(symbol, narg, sarg, func) kw_##func,
enum {
     kw_unknown,
#endif
     KEYWORD(define,                       1, 0, define_in_rc)
     KEYWORD(load,                         1, 0, load_in_rc)
     KEYWORD(render-adaptor,               0, 0, render_adaptor)
     KEYWORD(action-color,                 0, 0, action_color)
     KEYWORD(anti-aliasing,                0, 0, anti_aliasing)
     KEYWORD(draw-grips,                   0, 0, draw_grips)
     KEYWORD(grip-size,                    0, 0, grip_size)
     KEYWORD(grid-mode,                    0, 0, grid_mode)
     KEYWORD(dots-grid-dot-size,           0, 0, dots_grid_dot_size)
     KEYWORD(dots-grid-mode,               0, 0, dots_grid_mode)
     KEYWORD(dots-grid-threshold,          0, 0, dots_grid_threshold)
     KEYWORD(dots-grid-minor-alpha,        0, 0, dots_grid_minor_alpha)
     KEYWORD(dots-grid-major-alpha,        0, 0, dots_grid_major_alpha)
     KEYWORD(mesh-grid-threshold,          0, 0, mesh_grid_threshold)
     KEYWORD(mesh-line-width-factor,       0, 0, mesh_line_width_factor)
     KEYWORD(mesh-grid-minor-alpha,        0, 0, mesh_grid_minor_alpha)
     KEYWORD(mesh-grid-major-alpha,        0, 0, mesh_grid_major_alpha)
     //KEYWORD(mesh-grid-minor-color,        0, 0, mesh_grid_minor_color)
     //KEYWORD(mesh-grid-major-color,        0, 0, mesh_grid_major_color)
     KEYWORD(object-clipping,              0, 0, object_clipping)
     KEYWORD(scrollbars,                   0, 0, scrollbars)
     KEYWORD(scrollbar-update,             0, 0, scrollbar_update)
     KEYWORD(scrollbars-visible,           0, 0, scrollbars_visible)
     KEYWORD(scrollpan-steps,              0, 0, scrollpan_steps)
     KEYWORD(warp-cursor,                  0, 0, warp_cursor)
     KEYWORD(window-size,                  0, 0, window_size)
     KEYWORD(world-size,                   0, 0, world_size)
     KEYWORD(zoom-gain,                    0, 0, zoom_gain )
     KEYWORD(zoom-with-pan,                0, 0, zoom_with_pan )
     KEYWORD(logging,                      0, 0, logging)
     KEYWORD(log-destiny,                  0, 0, log_destiny)
     KEYWORD(console-window,               0, 0, console_window)
     KEYWORD(console-window-type,          0, 0, console_window_type)
     KEYWORD(action-feedback-mode,         0, 0, action_feedback_mode)
     KEYWORD(add-attribute-offset,         0, 0, add_attribute_offset)
     KEYWORD(attribute-placement-grid,     0, 0, attribute_placement_grid)
     KEYWORD(auto-load-last,               0, 0, auto_load_last)
     KEYWORD(auto-pan,                     0, 0, auto_pan)
     KEYWORD(auto-pan-step,                0, 0, auto_pan_step)
     KEYWORD(auto-save-interval,           0, 0, auto_save_interval)
     /* component-groups here */
     KEYWORD(component-dialog-attributes,  1, 0, component_dialog_attributes)
     KEYWORD(continue-component-place,     0, 0, continue_component_place)
     KEYWORD(embed-components,             0, 0, embed_components)
     KEYWORD(enforce-hierarchy,            0, 0, enforce_hierarchy)
     KEYWORD(file-preview,                 0, 0, file_preview)
     KEYWORD(force-boundingbox,            0, 0, force_boundingbox)
     KEYWORD(keyboardpan-gain,             0, 0, keyboardpan_gain)
     KEYWORD(magnetic-net-mode,            0, 0, magnetic_net_mode)
     KEYWORD(netconn-rubberband,           0, 0, netconn_rubberband)
     KEYWORD(raise-dialog-boxes-on-expose, 0, 0, raise_dialog_boxes)
     KEYWORD(select-slack-pixels,          0, 0, select_slack_pixels)
     KEYWORD(snap-size,                    0, 0, snap_size)
     KEYWORD(sort-component-library,       0, 0, sort_component_library)
//     KEYWORD(untitled-name,                0, 0, untitled_name)
     KEYWORD(net-consolidate,              0, 0, net_consolidate)
     KEYWORD(net-endpoint-mode,            0, 0, net_endpoint_mode)
     KEYWORD(net-midpoint-mode,            0, 0, net_midpoint_mode)
     KEYWORD(net-direction-mode,           0, 0, net_direction_mode)
     KEYWORD(net-selection-mode,           0, 0, net_selection_mode)
     KEYWORD(bus-style,                    0, 0, bus_style)
     KEYWORD(net-style,                    0, 0, net_style)
     KEYWORD(pin-style,                    0, 0, pin_style)
     KEYWORD(line-style,                   0, 0, line_style)
     KEYWORD(thick-bus-width,              0, 0, thick_bus_width)
     KEYWORD(thick-line-width,             0, 0, thick_line_width)
     KEYWORD(thick-net-width,              0, 0, thick_net_width)
     KEYWORD(thick-pin-width,              0, 0, thick_pin_width)
     KEYWORD(thin-bus-width,               0, 0, thin_bus_width)
     KEYWORD(thin-line-width,              0, 0, thin_line_width)
     KEYWORD(thin-net-width,               0, 0, thin_net_width)
     KEYWORD(thin-pin-width,               0, 0, thin_pin_width)
     KEYWORD(bus-ripper-rotation,          0, 0, bus_ripper_rotation)
     KEYWORD(bus-ripper-size,              0, 0, bus_ripper_size)
     KEYWORD(bus-ripper-type,              0, 0, bus_ripper_type)
     KEYWORD(bus-ripper-symname,           0, 0, bus_ripper_symname)
     KEYWORD(fast-mousepan,                0, 0, fast_mousepan)
     KEYWORD(drag-can-move,                0, 0, drag_can_move)
     KEYWORD(middle-button,                0, 0, middle_button)
     KEYWORD(third-button,                 0, 0, third_button)
     KEYWORD(mousepan-gain,                0, 0, mousepan_gain)
     KEYWORD(scroll-wheel,                 0, 0, scroll_wheel)
     KEYWORD(image-color,                  0, 0, image_color)
     KEYWORD(invert-images,                0, 0, invert_images)
     KEYWORD(text-case,                    0, 0, text_case)
     KEYWORD(text-display-zoomfactor,      0, 0, text_display_zoomfactor)
     KEYWORD(text-feedback,                0, 0, text_feedback)
     KEYWORD(text-origin-marker,           0, 0, text_origin_marker)
     KEYWORD(text-marker-size,             0, 0, text_marker_size)
     KEYWORD(text-marker-threshold,        0, 0, text_marker_threshold)
     KEYWORD(text-size,                    0, 0, text_size)
     KEYWORD(undo-levels,                  0, 0, undo_levels)
     KEYWORD(undo-control,                 0, 0, undo_control)
     KEYWORD(undo-panzoom,                 0, 0, undo_panzoom)
     KEYWORD(undo-preserve,                0, 0, undo_preserve)
     KEYWORD(undo-type,                    0, 0, undo_type)
     KEYWORD(attribute-name,               0, 0, attribute_name)

#ifdef _MAKE_KEYWORD_ENUM_
     KEYWORD_COUNT,
};
#undef _MAKE_KEYWORD_ENUM_
#endif
#undef KEYWORD

/* \defgroup KEYWORD_RC_MACROS Macros to Output Variable Data to RC Files
 *  @{
 */

/**************** Macros for Writing RC_BOOLEAN ****************/

/* \defgroup KEYWORD_RC_MAC_BOOLEAN Macros to Output Boolean Variables to RC Files
 *  @{
 */

/*! Write out entry for ENABLED Bool keyword
 * \warning INTERNAL Do not use directly */
#define RC_BOOLEAN_ENABLED(variable)              \
  strcpy(output_buffer, (state) ? "(" : ";(" );   \
  strcat(output_buffer, KEY_NAME(variable));      \
  strcat(output_buffer, " \""); /* open quote */  \
  strcat(output_buffer, RC_BOOL_STRINGS (TRUE));  \
  strcat(output_buffer, "\")\n"); /*close quote */ \
  fputs(output_buffer, output);

/*! Write out entry for DISABLED Bool keyword
 * \warning INTERNAL Do not use directly */
#define RC_BOOLEAN_DISABLED(variable)             \
  strcpy(output_buffer, !(state) ? "(" : ";(");   \
  strcat(output_buffer, KEY_NAME(variable));      \
  strcat(output_buffer, " \"");  /* open quote */ \
  strcat(output_buffer, RC_BOOL_STRINGS (FALSE)); \
  strcat(output_buffer, "\")\n"); /*close quote */\
  fputs(output_buffer, output);

/*! \remark Not used directly */
#define RC_BOOLEAN_OUT(variable) \
 RC_BOOLEAN_ENABLED(variable)     /* Write ENABLED */ \
 RC_BOOLEAN_DISABLED(variable)   /* Write DISABLED */

/*! \brief Macro to write a Global Boolean Variable
 * Use this macro to write a Global Boolean Variable
 * @param[in] variable integer variable within scope of macro expansion
 */
#define RC_BOOLEAN_GOUT(variable) \
 int state = variable;    /* Retrieve Variable */ \
 RC_BOOLEAN_OUT(variable)     /* Expand Base */

/*\remark
   Use this Macro to write a CairoRenderer Boolean Variable
   @param[in] variable   CairoRenderer integer variable
 */
#define RC_BOOLEAN_ROUT(variable) \
 int state = CairoRenderer->variable; \
 RC_BOOLEAN_OUT(variable)     /* Expand Base */

/*\remark
   Use this Macro to write a TopLevel Boolean Variable
   @param[in] variable   toplevel integer variable
 */
#define RC_BOOLEAN_TOUT(variable) \
 int state = w_current->toplevel->variable; \
 RC_BOOLEAN_OUT(variable)     /* Expand Base */

/*\remark
   Use this Macro to write a w_current Boolean Variable
   @param[in] variable   w_current integer variable
 */
#define RC_BOOLEAN_WOUT(variable) \
 int state = w_current->variable; \
 RC_BOOLEAN_OUT(variable)     /* Expand Base */

/** @} END Group KEYWORD_RC_MAC_BOOLEAN */

/*************** Maros for Writing RC Intergers ***************/

/* \defgroup KEYWORD_RC_MAC_INTEGERS Macros to Output Integer Variables to RC Files
 *  @{
 */

/*\warning INTERNAL Do not use directly */
#define RC_INTEGER_vOUT(variable, number) \
  strcpy(output_buffer, (state == number) ? "(" : ";("); \
  strcat(output_buffer, KEY_NAME(variable)); /* keyword_struc[kw_xxx_xxxx].name) */ \
  strcat(output_buffer, " "); \
  strcat(output_buffer, #number); \
  strcat(output_buffer, ")\n"); \
  fputs(output_buffer, output);

/*\remark
 * Use this Macro to write w_current var with selection of 3 integers
 * @param[in] variable   w_current integer variable
 * @param[in] N1         number
 * @param[in] N2         number
 * @param[in] N3         number
 *
 * \example
 *  RC_INTEGER_TRIAD_WOUT (dots_grid_dot_size, 1, 2, 3);
 */
#define RC_INTEGER_TRIAD_WOUT(variable, N1, N2, N3) \
  int state = w_current->variable; \
  RC_INTEGER_vOUT(variable, N1) \
  RC_INTEGER_vOUT(variable, N2) \
  RC_INTEGER_vOUT(variable, N3)

/*\warning INTERNAL Do not use directly */
#define RC_INTEGER_OUT(variable, digits) \
  char s_val[digits]; \
  strcpy(output_buffer, "(" ); \
  strcat(output_buffer, KEY_NAME(variable)); \
  strcat(output_buffer, " "); \
  strcat(output_buffer, geda_utility_string_int2str( number, s_val, 10 )); \
  strcat(output_buffer, ")\n"); \
  fputs(output_buffer, output);

/*\remark
 * Use this Macro to write w_current var with single integer
 * @param[in] variable   w_current integer variable
*/
#define RC_INTEGER_WOUT(variable) \
  int number = w_current->variable; \
  RC_INTEGER_OUT(variable, 4)

  /*\remark
 * Use this Macro to write a CairoRenderer var with single integer
 * @param[in] variable a CairoRenderer integer variable
*/
#define RC_INTEGER_ROUT(variable) \
  int number = CairoRenderer->variable; \
  RC_INTEGER_OUT(variable, 4)

/*\remark
 * Use this Macro to write toplevel var with single integer
 * @param[in] variable   toplevel integer variable
*/
#define RC_INTEGER_TOUT(variable) \
  int number = w_current->toplevel->variable; \
  RC_INTEGER_OUT(variable, 4)

/** @} END Group KEYWORD_RC_MAC_INTEGERS */

/**************** Maros for Writing RC Strings ****************/

/* \defgroup KEYWORD_RC_MAC_STRINGS Macros to Output String Variables to RC Files
 *  @{
 */

/*\warning INTERNAL Do not use directly */ /* NQ = No Quotes */
#define RC_STRING_TABLE_NQ_OUT(variable, WhichOne, member) \
  strcpy(output_buffer, (state == WhichOne) ? "(" : ";("); \
  strcat(output_buffer, KEY_NAME(variable)); /* keyword_struc[kw_xxx_xxxx].name) */ \
  strcat(output_buffer, " "); \
  strcat(output_buffer, member); /* struct of 4 strings */\
  strcat(output_buffer, ")\n"); \
  fputs(output_buffer, output);

/*\remark
 * Use this Macro to write local var with selection of 3 strings without quotes
 * @param[in] variable   string variable within the rc_options structure
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_NQ_ROUT(variable) \
  int state = rc_options.variable; \
  RC_STRING_TABLE_NQ_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_NQ_OUT(variable, 1, string_table.one) \
  RC_STRING_TABLE_NQ_OUT(variable, 2, string_table.two)

/*\warning INTERNAL Do not use directly */
#define RC_STRING_OUT(variable) \
  strcpy(output_buffer, "(" ); \
  strcat(output_buffer, KEY_NAME(variable)); \
  strcat(output_buffer, " \""); \
  strcat(output_buffer, string); \
  strcat(output_buffer, "\")\n"); \
  fputs(output_buffer, output);

/*\remark
 * Use this Macro to write Generic string variable
 * @param[in] variable   string variable within scope of macro expansion
 * @param[in] variable   string associated with this variable
 */
#define RC_STRING_GOUT(variable, str) \
  char* string = str; \
  RC_STRING_OUT(variable)

/*\remark
 * Use this Macro to write w_current string variable
 * @param[in] variable   w_current string variable
 */
#define RC_STRING_WOUT(variable) \
  char* string = w_current->variable; \
  RC_STRING_OUT(variable)

/*\remark
 * Use this Macro to write TopLevel string variable
 * @param[in] variable   toplevel string variable
 */
#define RC_STRING_TOUT(variable) \
  char* string = w_current->toplevel->variable; \
  RC_STRING_OUT(variable)

/*\warning INTERNAL Do not use directly */
#define RC_STRING_TABLE_OUT(variable, WhichOne, member) \
  strcpy(output_buffer, (state == WhichOne) ? "(" : ";("); \
  strcat(output_buffer, KEY_NAME(variable)); /* keyword_struc[kw_xxx_xxxx].name) */ \
  strcat(output_buffer, " \""); \
  strcat(output_buffer, member); /* struct of 7 strings */\
  strcat(output_buffer, "\")\n"); \
  fputs(output_buffer, output);

/*\remark
 * Use this Macro to write Global var with selection of 2 strings
 * @param[in] variable   string variable within scope of macro expansion
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_G2OUT(variable) \
  int state = variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one)

/*\remark
 * Use this Macro to write w_current var with selection of 2 strings
 * @param[in] variable   w_current string variable
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_W2OUT(variable) \
  int state = w_current->variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one)

/*\remark
 * Use this Macro to write global var with selection of 3 strings
 * @param[in] variable   string variable within scope of macro expansion
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_G3OUT(variable) \
  int state = variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one) \
  RC_STRING_TABLE_OUT(variable, 2, string_table.two)

/*\remark
 * Use this Macro to write w_current var with selection of 3 strings
 * @param[in] variable   w_current string variable
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_W3OUT(variable) \
  int state = w_current->variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one) \
  RC_STRING_TABLE_OUT(variable, 2, string_table.two)

/*\remark
 * Use this Macro to write toplevel var with selection of 3 strings
 * @param[in] variable   toplevel string variable
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_T3OUT(variable) \
  int state = w_current->toplevel->variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one) \
  RC_STRING_TABLE_OUT(variable, 2, string_table.two)

/*\remark
 * Use this Macro to write w_current var with selection of 4 strings
 * @param[in] variable   w_current string variable
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_W4OUT(variable) \
  int state = w_current->variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one) \
  RC_STRING_TABLE_OUT(variable, 2, string_table.two) \
  RC_STRING_TABLE_OUT(variable, 3, string_table.three)


/*\remark
 * Use this Macro to write w_current var with selection of 5 strings
 * @param[in] variable   w_current string variable
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_W5OUT(variable) \
  int state = w_current->variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one) \
  RC_STRING_TABLE_OUT(variable, 2, string_table.two) \
  RC_STRING_TABLE_OUT(variable, 3, string_table.three) \
  RC_STRING_TABLE_OUT(variable, 4, string_table.four)

/*\remark
 * Use this Macro to write w_current var with selection of 6 strings
 * @param[in] variable   w_current string variable
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_W6OUT(variable) \
  int state = w_current->variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one) \
  RC_STRING_TABLE_OUT(variable, 2, string_table.two) \
  RC_STRING_TABLE_OUT(variable, 3, string_table.three) \
  RC_STRING_TABLE_OUT(variable, 4, string_table.four) \
  RC_STRING_TABLE_OUT(variable, 5, string_table.five)

/*\remark
 * Use this Macro to write w_current var with selection of 7 strings
 * @param[in] variable   w_current string variable
 *\attention Associated strings are assume to be in string_table structure
 */
#define RC_STRING_TABLE_W7OUT(variable) \
  int state = w_current->variable; \
  RC_STRING_TABLE_OUT(variable, 0, string_table.zero) \
  RC_STRING_TABLE_OUT(variable, 1, string_table.one) \
  RC_STRING_TABLE_OUT(variable, 2, string_table.two) \
  RC_STRING_TABLE_OUT(variable, 3, string_table.three) \
  RC_STRING_TABLE_OUT(variable, 4, string_table.four) \
  RC_STRING_TABLE_OUT(variable, 5, string_table.five) \
  RC_STRING_TABLE_OUT(variable, 6, string_table.six)

/** @} END Group KEYWORD_RC_MAC_STRINGS */
/** @} END Group KEYWORD_RC_MACROS */

