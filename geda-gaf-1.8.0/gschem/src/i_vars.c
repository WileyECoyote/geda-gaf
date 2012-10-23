/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2012 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>
#include <stdio.h>

#include <gschem.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \def INIT_STR(w, name, str) */
#define INIT_STR(w, name, str) {                                        \
        g_free((w)->name);                                              \
        (w)->name = g_strdup(((default_ ## name) != NULL) ?             \
                             (default_ ## name) : (str));               \
}

/* Absolute default used when default_... strings are NULL */
/* Which is to say that the values assigned here will be the */
/* value assigned if a SCM does not return a different value */
/* while processing an rc file */

#define DEFAULT_PRINT_COMMAND "lpr"

/* Color Related */
int     default_attribute_color    = ATTRIBUTE_COLOR;
int     default_background_color   = BACKGROUND_COLOR;
int     default_detachattr_color   = DETACHED_ATTRIBUTE_COLOR;
int     default_junction_color     = JUNCTION_COLOR;
int     default_net_endpoint_color = NET_ENDPOINT_COLOR;
int     default_override_net_color = -1;
int     default_override_bus_color = -1;
int     default_override_pin_color = -1;

/* Display Sub-System */

int     default_draw_grips                = TRUE;

int     default_grid_mode                 = GRID_MESH;
int     default_dots_grid_dot_size        = DEFAULT_GRID_DOT_SIZE;
int     default_dots_grid_fixed_threshold = DEFAULT_GRID_DOT_THRESHOLD;
int     default_dots_grid_mode            = DOTS_GRID_VARIABLE_MODE;
int     default_mesh_grid_threshold       = DEFAULT_GRID_MESH_THRESHOLD;

int     default_object_clipping           = TRUE;
int     default_scrollbars                = TRUE;
int     default_scrollbar_update          = TRUE;
int     default_scrollpan_steps           = DEFAULT_SCROLLPAN_STEPS;

/* This should be renamed to decribe h&w of what, maybe something like default_screen_height */
int     default_window_height             = DEFAULT_WINDOW_HEIGHT;  /* these variables are used in x_window.c */
int     default_window_width              = DEFAULT_WINDOW_WIDTH;

int     default_warp_cursor        	  = TRUE;
int     default_zoom_gain          	  = DEFAULT_ZOOM_GAIN;
int     default_zoom_with_pan      	  = TRUE;

/* Image Related */
int     default_image_color               = FALSE;
int     default_image_width               = DEFAULT_IMAGE_WIDTH;
int     default_image_height              = DEFAULT_IMAGE_HEIGHT;

/* Log Related */
int     default_logging            	  = TRUE;
int     default_log_destiny        	  = LOG_WINDOW;
int     default_log_window         	  = MAP_ON_STARTUP;
int     default_log_window_type    	  = DECORATED;

/* Miscellaneous - in  alphabetical order */
int     default_action_feedback_mode      = OUTLINE;
int     default_add_attribute_offset      = DEFAULT_ATTRIBUTE_OFFSET;
int     default_auto_load_last            = DEFAULT_AUTO_LOAD_LAST;
int     default_auto_save_interval        = DEFAULT_SAVE_INTERVAL;
int     default_autoplace_attributes_grid = DEFAULT_AUTOPLACE_GRID;
GList  *default_component_select_attrlist = NULL;
int     default_continue_component_place  = TRUE;
int     default_embed_components          = FALSE;
int     default_enforce_hierarchy         = TRUE;
int     default_file_preview              = FALSE;
int     default_force_boundingbox         = FALSE;
int     default_handleboxes               = TRUE;
int     default_include_complex           = FALSE;
int     default_keyboardpan_gain          = DEFAULT_KEYBOARD_GAIN;
int     default_magnetic_net_mode         = TRUE;
int     default_netconn_rubberband        = FALSE;
int     default_raise_dialog_boxes        = FALSE;
int     default_select_slack_pixels       = DEFAULT_SLACK_PIXELS;
int     default_snap_size                 = DEFAULT_SNAP_SIZE;
int     default_sort_component_library    = FALSE;
int     default_toolbars                  = TRUE;

/* Nets and Routing */
int     default_net_consolidate    = TRUE;
int     default_net_endpoint_mode  = FILLED_BOX;
int     default_net_midpoint_mode  = FILLED_BOX;
int     default_net_direction_mode = TRUE;
int     default_net_selection_mode = NET_SELECT_NET;

  /* Net Ripper */
int     default_bus_ripper_rotation = NON_SYMMETRIC;
int     default_bus_ripper_size     = DEFAULT_RIPPER_SIZE;
int     default_bus_ripper_type     = COMP_BUS_RIPPER;

/* Pointer Device, aka Mouse stuff */
int     default_fast_mousepan      = TRUE;
int     default_drag_can_move      = TRUE;
#ifdef HAVE_LIBSTROKE
  int   default_middle_button      = MOUSE_MIDDLE_STROKE;
#else
  int   default_middle_button      = MOUSE_MIDDLE_REPEAT;
#endif
int     default_mousepan_gain      = DEFAULT_MOUSEPAN_GAIN;
int     default_scroll_wheel       = SCROLL_WHEEL_CLASSIC;
int     default_third_button       = POPUP_ENABLED;

/* Print Related */
int     default_paper_width               = DEFAULT_PAPER_WIDTH; /* letter size */
int     default_paper_height              = DEFAULT_PAPER_HEIGHT;
char   *default_print_command             = NULL;
int     default_print_color               = FALSE;
int     default_print_color_background    = OUTPUT_BACKGROUND_COLOR;
int     default_print_orientation         = LANDSCAPE;
int     default_print_output_type         = EXTENTS;
int     default_print_output_capstyle     = SQUARE_CAP;
int     default_setpagedevice_orientation = FALSE;
int     default_setpagedevice_pagesize    = FALSE;

/* Text Related */
int     default_text_case                 = LOWER_CASE;

/* default zoom_factor at which text is displayed completely */
int     default_text_display_zoomfactor   = DEFAULT_TEXT_ZOOM;
int     default_text_feedback             = ONLY_WHEN_READABLE;
int     default_text_origin_marker        = TRUE;
int     default_text_size                 = DEFAULT_TEXT_SIZE;

/* Undo System */
int     default_undo_control              = TRUE;
int     default_undo_levels               = DEFAULT_UNDO_LEVELS;
int     default_undo_panzoom              = FALSE;
int     default_undo_type                 = UNDO_DISK;

/*!
 *  \brief This functions sets the values of top-level interger variables.
 *
 *  \par Function Description
 *       This functions assigns the current default values to the toplevel
 *       variables. The first time this occurs is in the main-line after
 *       rc initialization files have "executed", which may have changed
 *       the value of the defaults variables.
 *
 * Maintenance:
 *
 *   The variables in this section have been organized into groups. The
 *   groups are listed "mostly" alphabeticaly, with the non-catagorized
 *   group last. Variables are listed alphabeticaly within each group.
 *
 */
void i_vars_set(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  i_vars_libgeda_set(toplevel);

/* Color Related */
  toplevel->background_color           = default_background_color;
  toplevel->override_net_color         = default_override_net_color;
  toplevel->override_bus_color         = default_override_bus_color;
  toplevel->override_pin_color         = default_override_pin_color;

/* Display Sub-System */
  w_current->draw_grips                = default_draw_grips;

  w_current->grid_mode                 = default_grid_mode;
  w_current->dots_grid_dot_size        = default_dots_grid_dot_size;
  w_current->dots_grid_mode            = default_dots_grid_mode;
  w_current->dots_grid_fixed_threshold = default_dots_grid_fixed_threshold;
  w_current->mesh_grid_threshold       = default_mesh_grid_threshold;

  toplevel->object_clipping            = default_object_clipping;
  w_current->scrollbars                = default_scrollbars;
  w_current->scrollbar_update          = default_scrollbar_update;
  w_current->scrollpan_steps           = default_scrollpan_steps;
  w_current->warp_cursor               = default_warp_cursor;
  w_current->zoom_gain                 = default_zoom_gain;
  w_current->zoom_with_pan             = default_zoom_with_pan;

/* Miscellaneous - in  alphabetical order */
  w_current->action_feedback_mode      = default_action_feedback_mode;
  w_current->add_attribute_offset      = default_add_attribute_offset;
  toplevel->auto_save_interval         = default_auto_save_interval;
  w_current->autoplace_attributes_grid = default_autoplace_attributes_grid;
  w_current->component_select_attrlist = default_component_select_attrlist;
  w_current->continue_component_place  = default_continue_component_place;
  w_current->embed_components          = default_embed_components;
  w_current->enforce_hierarchy         = default_enforce_hierarchy;
  w_current->file_preview              = default_file_preview;
  toplevel->force_boundingbox          = default_force_boundingbox;
  w_current->handleboxes               = default_handleboxes;
  w_current->include_complex           = default_include_complex;
  w_current->keyboardpan_gain          = default_keyboardpan_gain;
  w_current->netconn_rubberband        = default_netconn_rubberband;
  w_current->raise_dialog_boxes        = default_raise_dialog_boxes;
  w_current->select_slack_pixels       = default_select_slack_pixels;
  w_current->snap_size                 = default_snap_size;
  w_current->sort_component_library    = default_sort_component_library;
  w_current->toolbars                  = default_toolbars;

/* Nets and Routing */
  w_current->magnetic_net_mode    = default_magnetic_net_mode;
  toplevel->net_consolidate       = default_net_consolidate;
  w_current->net_endpoint_mode    = default_net_endpoint_mode;
  w_current->net_direction_mode   = default_net_direction_mode;
  w_current->net_midpoint_mode    = default_net_midpoint_mode;
  w_current->net_selection_mode   = default_net_selection_mode; 

  w_current->bus_ripper_rotation  = default_bus_ripper_rotation;
  w_current->bus_ripper_size      = default_bus_ripper_size;
  w_current->bus_ripper_type      = default_bus_ripper_type;

/* Pointer Device, aka Mouse stuff */
  w_current->fast_mousepan        = default_fast_mousepan;
  w_current->drag_can_move        = default_drag_can_move;
  w_current->middle_button        = default_middle_button;
  w_current->third_button         = default_third_button;
  w_current->mousepan_gain        = default_mousepan_gain;
  w_current->scroll_wheel         = default_scroll_wheel;

/* Print Related */
  INIT_STR(w_current, print_command, DEFAULT_PRINT_COMMAND);

  toplevel->image_color                 = default_image_color;
  w_current->image_width                = default_image_width;
  w_current->image_height               = default_image_height;
  toplevel->paper_width                 = default_paper_width;
  toplevel->paper_height                = default_paper_height;
  toplevel->print_color                 = default_print_color;
  toplevel->print_color_background      = default_print_color_background;
  toplevel->print_orientation           = default_print_orientation;
  toplevel->print_output_type           = default_print_output_type;
  toplevel->print_output_capstyle       = default_print_output_capstyle;
  toplevel->setpagedevice_orientation   = default_setpagedevice_orientation;
  toplevel->setpagedevice_pagesize      = default_setpagedevice_pagesize;

/* Text Related */
  w_current->text_case                  = default_text_case;

/* default zoom_factor at which text is displayed completely */
  w_current->text_display_zoomfactor    = default_text_display_zoomfactor;
  w_current->text_feedback              = default_text_feedback;
  w_current->text_origin_marker         = default_text_origin_marker;
  w_current->text_size                  = default_text_size;

/* Undo Sub-System */
  w_current->undo_levels           = default_undo_levels;
  w_current->undo_control          = default_undo_control;
  w_current->undo_type             = default_undo_type;
  w_current->undo_panzoom          = default_undo_panzoom;

}

/*! \brief Free default names
 *  \par Function Description
 *  This function will free all of the default variables for gschem.
 *
 */
GList* g_list_clear(GList* list){

  if (list != NULL ) {

    g_list_foreach(list, (GFunc)g_free, NULL);

    lambda (const char* data)
    {
      list = g_list_remove( list, data);
      return FALSE;
    }
    foreach (list);

    g_list_free(list);
    list = NULL;
  }
  return list;
}

/*! \brief Free default names
 *  \par Function Description
 *  This function will free all of the default variables for gschem.
 *
 */
void i_vars_freenames()
{
  default_component_select_attrlist = g_list_clear(default_component_select_attrlist);
  g_free(default_print_command);
}
