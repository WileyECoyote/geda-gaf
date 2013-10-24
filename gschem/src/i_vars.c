/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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

#define Renderer w_current->renderer
/* Absolute default used when default_... strings are NULL */
/* Which is to say that the values assigned here will be the */
/* value assigned if a SCM does not return a different value */
/* while processing an rc file */

#define DEFAULT_PRINT_COMMAND "lpr"

/*! \brief Set Default Defaults
 *  \par  This section creates and assigns default values for top-level
 *  "default" variables. Each of these is declare extern in i_var.h so
 *  that they maybe written to by when RC files are processed by libgeda
 *  or gschem.
 */
/* Color Related */
int     default_attribute_color           = ATTRIBUTE_COLOR;
int     default_background_color          = BACKGROUND_COLOR;
int     default_detachattr_color          = DETACHED_ATTRIBUTE_COLOR;
int     default_junction_color            = JUNCTION_COLOR;
int     default_net_endpoint_color        = NET_ENDPOINT_COLOR;
int     default_override_net_color        = -1;
int     default_override_bus_color        = -1;
int     default_override_pin_color        = -1;

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
int     default_scrollbars_visible        = TRUE;

/* This should be renamed to describe h&w of what, maybe something like default_screen_height */
int     default_window_height             = DEFAULT_WINDOW_HEIGHT;  /* these variables are used in x_window.c */
int     default_window_width              = DEFAULT_WINDOW_WIDTH;

int     default_warp_cursor               = TRUE;
int     default_zoom_gain                 = DEFAULT_ZOOM_GAIN;
int     default_zoom_with_pan             = TRUE;

/* Image Related */
int     default_image_color               = FALSE;
int     default_invert_images             = TRUE;
int     default_image_width               = DEFAULT_IMAGE_WIDTH;
int     default_image_height              = DEFAULT_IMAGE_HEIGHT;

/* Log Related */
int     default_logging                   = TRUE;
int     default_log_destiny               = CONSOLE_WINDOW;
int     default_console_window            = MAP_ON_STARTUP;
int     default_console_window_type       = DECORATED;

/* Miscellaneous - in  alphabetical order */
int     default_action_feedback_mode      = OUTLINE;
int     default_add_attribute_offset      = DEFAULT_ATTRIBUTE_OFFSET;
int     default_auto_load_last            = DEFAULT_AUTO_LOAD_LAST;
int     default_auto_save_interval        = DEFAULT_SAVE_INTERVAL;
int     default_attribute_placement_grid  = DEFAULT_ATTRIB_PLACE_GRID;
GList  *default_component_select_attrlist = NULL;
int     default_continue_component_place  = TRUE;
int     default_embed_components          = FALSE;
int     default_enforce_hierarchy         = TRUE;
int     default_force_boundingbox         = FALSE;
int     default_include_complex           = FALSE;
int     default_keyboardpan_gain          = DEFAULT_KEYBOARD_GAIN;
int     default_magnetic_net_mode         = TRUE;
int     default_netconn_rubberband        = FALSE;
int     default_select_slack_pixels       = DEFAULT_SLACK_PIXELS;
int     default_snap_size                 = DEFAULT_SNAP_SIZE;
int     default_sort_component_library    = FALSE;

/* Nets and Routing */
int     default_net_consolidate           = TRUE;
int     default_net_endpoint_mode         = FILLED_BOX;
int     default_net_midpoint_mode         = FILLED_BOX;
int     default_net_direction_mode        = TRUE;
int     default_net_selection_mode        = NET_SELECT_NET;

  /* Net Ripper */
int     default_bus_ripper_rotation       = NON_SYMMETRIC;
int     default_bus_ripper_size           = DEFAULT_RIPPER_SIZE;
int     default_bus_ripper_type           = COMP_BUS_RIPPER;
char   *default_bus_ripper_symname        = NULL;

/* Pointer Device, aka Mouse stuff */
int     default_drag_can_move             = TRUE;
int     default_fast_mousepan             = TRUE;

int     default_middle_button             = DEFAULT_MOUSE_MIDDLE;
int     default_mousepan_gain             = DEFAULT_MOUSEPAN_GAIN;
int     default_scrollpan_steps           = DEFAULT_SCROLLPAN_STEPS;
int     default_scroll_wheel              = SCROLL_WHEEL_CLASSIC;
int     default_pointer_hscroll           = FALSE;
int     default_third_button              = POPUP_ENABLED;

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

/* System Related */
int     default_file_preview              = FALSE;
int     default_handleboxes               = TRUE;
int     default_raise_dialog_boxes        = FALSE;
int     default_save_ui_settings          = TRUE;
int     default_toolbars                  = TRUE;
int     default_toolbars_mode             = RC_NIL;
int     default_show_toolbar_tips         = RC_NIL;

/* Text Related */
int     default_text_case                 = LOWER_CASE;

/* default zoom_factor at which text is displayed completely */
int     default_text_display_zoomfactor   = DEFAULT_TEXT_ZOOM;
int     default_text_feedback             = ONLY_WHEN_READABLE;
int     default_text_origin_marker        = TRUE;
int     default_text_marker_size          = DEFAULT_TEXT_MARKER_SIZE;
int     default_text_size                 = DEFAULT_TEXT_SIZE;

/* Undo System */
int     default_undo_control              = TRUE;
int     default_undo_levels               = DEFAULT_UNDO_LEVELS;
int     default_undo_panzoom              = FALSE;
int     default_undo_type                 = UNDO_DISK;

char *i_var_get_gschem_config_string(EdaConfig *cfg, char *str) {

  GError *err = NULL;
  char *tmpstr;

  tmpstr = eda_config_get_string (cfg, "gschem", str, &err);
  if (err != NULL) {
    g_warning ("Error retrieving user configuration: '%s'", err->message);
    g_clear_error (&err);
  }
  return tmpstr;
}

void
i_var_restore_gschem_boolean(EdaConfig *cfg, char *key, int *var, bool def_val)
{
  GError *err = NULL;
  bool tmp_bool;

  tmp_bool = eda_config_get_boolean (cfg, "gschem", key, &err);
  if (err != NULL) {
    g_clear_error (&err);
    *var = def_val;
  }
  else {
    *var = tmp_bool;
  }
}

void
i_var_restore_gschem_integer(EdaConfig *cfg, char *key, int *var, int def_val)
{
  GError *err = NULL;
  int tmp_int;

  tmp_int = eda_config_get_integer (cfg, "gschem", key, &err);
  if (err != NULL) {
    g_clear_error (&err);
    *var = def_val;
  }
  else {
    *var = tmp_int;
  }
}
void
i_var_restore_gschem_color(EdaConfig *cfg, char *key, GdkColor *var, int index)
{
  GError   *err = NULL;
  GdkColor *color;
  int      *array;
  unsigned int rediculus = 4;

  array = eda_config_get_int_list (cfg, "gschem", key, &rediculus, &err);
  if (err != NULL) {
    g_clear_error (&err);
    color = x_get_color(index);
    var->pixel = color->pixel;
    var->red   = color->red;
    var->green = color->green;
    var->blue  = color->blue;
  }
  else {
    var->pixel = array[0];
    var->red   = array[1];
    var->green = array[2];
    var->blue  = array[3];
  }
}
/*! \brief Recall User Settings
 *  \par  This section retrieves the values of configuration settings from
 *  the new configuration system and assigns the values to the cooresponding
 *  top-level variable. Note that this is done after the "old" system, so
 *  the RC values will be ignored.
 */
void i_vars_recall_user_settings(GschemToplevel *w_current)
{
  EdaConfig *cfg     = eda_config_get_user_context ();
  TOPLEVEL *toplevel = w_current->toplevel;
  char *tmp_str;

  v_log_message("Restoring user settings\n");

  tmp_str = i_var_get_gschem_config_string (cfg, "default-font-name");
  if (tmp_str != NULL) {
    eda_renderer_set_font_name(w_current->renderer, tmp_str);
    g_free (tmp_str);
  }

  i_var_restore_gschem_boolean(cfg, "image-color",   &toplevel->image_color,   TRUE);
  i_var_restore_gschem_boolean(cfg, "invert-images", &toplevel->invert_images, TRUE);
  i_var_restore_gschem_integer(cfg, "image-width",   &w_current->image_width,  DEFAULT_IMAGE_WIDTH);
  i_var_restore_gschem_integer(cfg, "image-height",  &w_current->image_height, DEFAULT_IMAGE_HEIGHT);

  /* Scrolling Settings - Saved by: x_window_save_settings */
  i_var_restore_gschem_integer(cfg, "scrollbars",         &w_current->scrollbars,         TRUE);
  i_var_restore_gschem_integer(cfg, "scrollbar-update",   &w_current->scrollbar_update,   TRUE);
  i_var_restore_gschem_integer(cfg, "scrollbars-visible", &w_current->scrollbars_visible, TRUE);

  /* Grips Settings - Saved by: x_window_save_settings */
  i_var_restore_gschem_boolean(cfg, "draw-grips",  &w_current->renderer->
                                     draw_grips,      TRUE);

  i_var_restore_gschem_integer(cfg, "grip-pixels", &w_current->
                                     grip_pixel_size, DEFAULT_GRIP_SIZE);

  i_var_restore_gschem_color(cfg, "grips-stroke",  &w_current->renderer->
                                   grip_stroke_color, DEFAULT_GRIP_STROKE_COLOR);

  i_var_restore_gschem_color(cfg, "grips-fill",    &w_current->renderer->
                                   grip_fill_color,   DEFAULT_GRIP_FILL_COLOR);

  /* Restore Cues & Endpoints settings - Saved by: x_window_save_settings */
  i_var_restore_gschem_integer(cfg, "junction-size", &w_current->renderer->
                                     junction_size,   DEFAULT_JUNCTION_SIZE);

  i_var_restore_gschem_color(cfg, "junction-color",  &w_current->renderer->
                                   junction_color,    DEFAULT_JUNCTION_COLOR);

  i_var_restore_gschem_color(cfg, "net-endpoint-color", &w_current->renderer->
                                   net_endpoint_color,   DEFAULT_NET_ENDPOINT_COLOR);

  /* Restore text related stuff - Saved by: x_settings_save_settings */
  i_var_restore_gschem_integer(cfg, "text-case",          &w_current->text_case,     BOTH_CASES);
  i_var_restore_gschem_integer(cfg, "text-zoomfactor",    &w_current->text_display_zoomfactor, DEFAULT_TEXT_ZOOM);
  i_var_restore_gschem_integer(cfg, "text-feedback",      &w_current->text_feedback, ONLY_WHEN_READABLE);
  i_var_restore_gschem_integer(cfg, "text-size",          &w_current->text_size,     DEFAULT_TEXT_SIZE);
  i_var_restore_gschem_boolean(cfg, "text-origin-marker", &w_current->renderer->
                                     text_origin_marker,   TRUE);
  i_var_restore_gschem_integer(cfg, "text-marker-size",   &w_current->renderer->
                                     text_marker_size,     DEFAULT_TEXT_MARKER_SIZE);
  i_var_restore_gschem_color(cfg,   "text_marker_color",  &w_current->renderer->
                                     text_marker_color,    DEFAULT_TEXT_MARKER_COLOR);

  /* Pointer Device, aka Mouse stuff - Saved by: x_window_save_settings */
  i_var_restore_gschem_integer(cfg, "cursor-index",    &w_current->drawing_pointer, DEFAULT_CURSOR_INDEX);
  i_var_restore_gschem_integer(cfg, "drag-can-move",   &w_current->drag_can_move,   TRUE);
  i_var_restore_gschem_integer(cfg, "fast-mousepan",   &w_current->fast_mousepan,   TRUE);
  i_var_restore_gschem_integer(cfg, "middle-button",   &w_current->middle_button,   DEFAULT_MOUSE_MIDDLE);
  i_var_restore_gschem_integer(cfg, "mousepan-gain",   &w_current->mousepan_gain,   DEFAULT_MOUSEPAN_GAIN);
  i_var_restore_gschem_integer(cfg, "pointer-hscroll", &w_current->pointer_hscroll, FALSE);
  i_var_restore_gschem_integer(cfg, "scrollpan-steps", &w_current->scrollpan_steps, DEFAULT_SCROLLPAN_STEPS);
  i_var_restore_gschem_integer(cfg, "scroll-wheel",    &w_current->scroll_wheel,    SCROLL_WHEEL_CLASSIC);
  i_var_restore_gschem_integer(cfg, "third-button",    &w_current->third_button,    POPUP_ENABLED);
}

/*!
 *  \brief This functions sets the values of top-level interger variables.
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

void i_vars_set(GschemToplevel *w_current)
{
  TOPLEVEL *toplevel                   = w_current->toplevel;
  i_vars_libgeda_set(toplevel);

/* Color Related */
  w_current->background_color          = default_background_color;
  toplevel->override_net_color         = default_override_net_color;
  toplevel->override_bus_color         = default_override_bus_color;
  toplevel->override_pin_color         = default_override_pin_color;

  w_current->grid_mode                 = default_grid_mode;
  w_current->dots_grid_dot_size        = default_dots_grid_dot_size;
  w_current->dots_grid_mode            = default_dots_grid_mode;
  w_current->dots_grid_fixed_threshold = default_dots_grid_fixed_threshold;
  w_current->mesh_grid_threshold       = default_mesh_grid_threshold;

  toplevel->object_clipping            = default_object_clipping;
  w_current->scrollbars                = default_scrollbars;
  w_current->scrollbar_update          = default_scrollbar_update;
  w_current->scrollbars_visible        = default_scrollbars_visible;
  w_current->warp_cursor               = default_warp_cursor;
  w_current->zoom_gain                 = default_zoom_gain;
  w_current->zoom_with_pan             = default_zoom_with_pan;

/* Imaging Related */
  toplevel->image_color                = default_image_color;
  toplevel->invert_images              = default_invert_images;
  w_current->image_width               = default_image_width;
  w_current->image_height              = default_image_height;

/* Miscellaneous - in  alphabetical order */
  w_current->action_feedback_mode      = default_action_feedback_mode;
  w_current->add_attribute_offset      = default_add_attribute_offset;
  toplevel->auto_save_interval         = default_auto_save_interval;
  w_current->attribute_placement_grid  = default_attribute_placement_grid;
  w_current->component_select_attrlist = default_component_select_attrlist;
  w_current->continue_component_place  = default_continue_component_place;
  w_current->embed_components          = default_embed_components;
  w_current->enforce_hierarchy         = default_enforce_hierarchy;
  toplevel->force_boundingbox          = default_force_boundingbox;
  w_current->include_complex           = default_include_complex;
  w_current->keyboardpan_gain          = default_keyboardpan_gain;
  w_current->netconn_rubberband        = default_netconn_rubberband;
  w_current->select_slack_pixels       = default_select_slack_pixels;
  w_current->snap_size                 = default_snap_size;
  w_current->sort_component_library    = default_sort_component_library;

/* Nets and Routing */
  w_current->magnetic_net_mode         = default_magnetic_net_mode;
  toplevel->net_consolidate            = default_net_consolidate;
  w_current->net_endpoint_mode         = default_net_endpoint_mode;
  w_current->net_direction_mode        = default_net_direction_mode;
  w_current->net_midpoint_mode         = default_net_midpoint_mode;
  w_current->net_selection_mode        = default_net_selection_mode;

  w_current->bus_ripper_rotation       = default_bus_ripper_rotation;
  w_current->bus_ripper_size           = default_bus_ripper_size;
  w_current->bus_ripper_type           = default_bus_ripper_type;
  INIT_STR(w_current, bus_ripper_symname, DEFAULT_BUS_RIPPER_SYMNAME);

/* Pointer Device, aka Mouse stuff */
  w_current->drag_can_move             = default_drag_can_move;
  w_current->fast_mousepan             = default_fast_mousepan;
  w_current->middle_button             = default_middle_button;
  w_current->mousepan_gain             = default_mousepan_gain;
  w_current->pointer_hscroll           = default_pointer_hscroll;
  w_current->scrollpan_steps           = default_scrollpan_steps;
  w_current->scroll_wheel              = default_scroll_wheel;
  w_current->third_button              = default_third_button;

/* Print & Related */
  INIT_STR(w_current, print_command, DEFAULT_PRINT_COMMAND);

  toplevel->paper_width                = default_paper_width;
  toplevel->paper_height               = default_paper_height;
  toplevel->print_color                = default_print_color;
  toplevel->print_color_background     = default_print_color_background;
  toplevel->print_orientation          = default_print_orientation;
  toplevel->print_output_type          = default_print_output_type;
  toplevel->print_output_capstyle      = default_print_output_capstyle;
  toplevel->setpagedevice_orientation  = default_setpagedevice_orientation;
  toplevel->setpagedevice_pagesize     = default_setpagedevice_pagesize;

  /* System Related */
  w_current->file_preview              = default_file_preview;
  w_current->handleboxes               = default_handleboxes;
  w_current->raise_dialog_boxes        = default_raise_dialog_boxes;
  w_current->save_ui_settings          = default_save_ui_settings;
  w_current->toolbars                  = default_toolbars;
  w_current->toolbars_mode             = default_toolbars_mode;
  w_current->show_toolbar_tips         = default_show_toolbar_tips;

/* Text Related */
  w_current->text_case                 = default_text_case;

/* default zoom_factor at which text is displayed completely */
  w_current->text_display_zoomfactor   = default_text_display_zoomfactor;
  w_current->text_feedback             = default_text_feedback;
   Renderer->text_origin_marker        = default_text_origin_marker;
   Renderer->text_marker_size          = default_text_marker_size;
  w_current->text_size                 = default_text_size;

/* Undo Sub-System */
  w_current->undo_levels               = default_undo_levels;
  w_current->undo_control              = default_undo_control;
  w_current->undo_type                 = default_undo_type;
  w_current->undo_panzoom              = default_undo_panzoom;

  i_vars_recall_user_settings (w_current);

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
  g_free(default_bus_ripper_symname);
}

/*! \brief Setup gschem default configuration.
 * \par Function Description
 * Populate the default configuration context with compiled-in
 * defaults.
 */
void
i_vars_init(GschemToplevel *w_current)
{
  EdaConfig *cfg = eda_config_get_default_context ();

  /* read in Gtk RC files */
  g_rc_parse_gtkrc();

  /* Set values for all variables */
  v_log_message("Assigning default configuration settings\n");

  /* This is the prefix of the default filename used for newly created
   * schematics and symbols. */
  /// TRANSLATORS: this string is used to generate a filename for
  /// newly-created files.  It will be used to create a filename of
  /// the form "untitled_N.sch", where N is a number.  Please make
  /// sure that the translation contains characters suitable for use
  /// in a filename.
  eda_config_set_string (cfg, "gschem", "default-filename",   _(DEFAULT_UNTITLED_NAME));
  eda_config_set_string (cfg, "gschem", "default-font-name",   (DEFAULT_FONT_NAME));
  eda_config_set_string (cfg, "gschem", "default-titleblock", _(DEFAULT_TITLEBLOCK));

  eda_config_set_boolean (cfg, "gschem", "auto-file-suffix",  TRUE);

  eda_config_set_boolean (cfg, "gschem.library", "sort",      FALSE);
  eda_config_set_boolean (cfg, "gschem.library", "groups",    TRUE);
  eda_config_set_boolean (cfg, "gschem.library", "subgroups", TRUE);
  eda_config_set_boolean (cfg, "gschem.library", "showtips",  TRUE);
  eda_config_set_integer (cfg, "gschem.library", "style",     255);

  /* read in Gscehm RC files, which may over-ride the hard-coded defaults! */
  x_rc_parse_gschem (w_current, rc_filename);

}

/*! \brief Save user config on exit.
 * \par Function Description
 * When gschem exits, try to save the user configuration to disk.
 */
void
i_vars_atexit_save_user_config (gpointer user_data)
{
  EdaConfig *cfg = eda_config_get_user_context ();
  GError    *err = NULL;

  eda_config_save (cfg, &err);
  if (err != NULL) {
    g_warning (_("Failed to save user configuration to '%s': %s."),
                  eda_config_get_filename (cfg),
                  err->message);
    g_clear_error (&err);
  }
}
#undef Renderer