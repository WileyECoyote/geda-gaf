/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_vars.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include <geda_file_chooser.h>  /* Need for group and key defines */
#include <geda_debug.h>

/* Absolute default used when default_... strings are NULL */
/* Which is to say that the values assigned here will be the */
/* value assigned if a SCM does not return a different value */
/* while processing an rc file */

#define DEFAULT_PRINT_COMMAND "lpr"

/*!
 * \brief Set Default Defaults
 * \par  This section creates and assigns default values for top-level
 *  "default" variables. Each of these is declare extern in i_var.h so
 *  that they maybe written to when RC files are processed.
 */
/* Color Related */
int     default_override_net_color        = RC_NIL;
int     default_override_bus_color        = RC_NIL;
int     default_override_pin_color        = RC_NIL;

/* Display Sub-System */
int     default_render_adaptor            = RC_NIL;
int     default_action_color              = RC_NIL;
int     default_anti_aliasing             = RC_NIL;
int     default_draw_grips                = RC_NIL;
int     default_draw_complex_grips        = TRUE;
int     default_grip_size                 = RC_NIL;

int     default_grid_mode                 = RC_NIL;
int     default_dots_grid_dot_size        = RC_NIL;
int     default_dots_grid_mode            = RC_NIL;
int     default_dots_grid_threshold       = RC_NIL;
int     default_dots_grid_minor_alpha     = DEFAULT_GRID_MINOR_ALPHA;
int     default_dots_grid_major_alpha     = DEFAULT_GRID_MAJOR_ALPHA;

int     default_mesh_grid_threshold       = RC_NIL;
int     default_mesh_line_width_factor    = RC_NIL;
int     default_mesh_grid_minor_alpha     = DEFAULT_GRID_MINOR_ALPHA;
int     default_mesh_grid_major_alpha     = DEFAULT_GRID_MAJOR_ALPHA;

GdkColor default_dots_grid_major_color    = { 88 };
GdkColor default_dots_grid_minor_color    = { 88 };
GdkColor default_mesh_grid_minor_color    = { 88 };
GdkColor default_mesh_grid_major_color    = { 88 };

int     default_object_clipping           = RC_NIL;
int     default_scrollbars                = RC_NIL;
int     default_scrollbar_update          = RC_NIL;
int     default_scrollbars_visible        = RC_NIL;

/* This should be renamed to describe h&w of what, maybe something like default_screen_height */
int     default_window_height             = DEFAULT_WINDOW_HEIGHT;  /* these variables are used in x_window.c */
int     default_window_width              = DEFAULT_WINDOW_WIDTH;

int     default_warp_cursor               = TRUE;
int     default_zoom_gain                 = DEFAULT_ZOOM_GAIN;
int     default_zoom_with_pan             = TRUE;

int     default_world_left                = 0;
int     default_world_right               = WIDTH_C;
int     default_world_bottom              = HEIGHT_C;
int     default_world_top                 = 0;

/* Image Related */
int     default_image_color               = RC_NIL;
int     default_invert_images             = RC_NIL;
int     default_image_width               = RC_NIL;
int     default_image_height              = RC_NIL;

/* Log Related */
int     default_logging                   = RC_NIL;
int     default_log_destiny               = RC_NIL;
int     default_console_window            = RC_NIL;
int     default_console_window_type       = RC_NIL;

/* Miscellaneous - in  alphabetical order */
int     default_action_feedback_mode      = OUTLINE;
int     default_add_attribute_offset      = DEFAULT_ATTRIBUTE_OFFSET;
int     default_attribute_placement_grid  = DEFAULT_ATTRIB_PLACE_GRID;
int     default_auto_load_last            = DEFAULT_AUTO_LOAD_LAST;
int     default_auto_pan                  = DEFAULT_AUTO_PAN;
int     default_auto_pan_step             = DEFAULT_AUTO_PAN_STEP;
int     default_auto_save_interval        = RC_NIL;

int     default_chooser_filter            = RC_NIL;
GList  *default_component_select_attrlist = NULL;
int     default_continue_component_place  = RC_NIL;
int     default_embed_components          = RC_NIL;
int     default_enforce_hierarchy         = RC_NIL;
int     default_hierarchy_up_close        = RC_NIL;
int     default_force_boundingbox         = RC_NIL;
//int     default_include_complex           = FALSE;
int     default_keyboardpan_gain          = RC_NIL;
int     default_magnetic_net_mode         = TRUE;
int     default_netconn_rubberband        = RC_NIL;
int     default_select_slack_pixels       = RC_NIL;
int     default_snap_size                 = RC_NIL;
int     default_sort_component_library    = RC_NIL;

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
int     default_drag_can_move             = RC_NIL;
int     default_fast_mousepan             = RC_NIL;
int     default_middle_button             = RC_NIL;
int     default_mousepan_gain             = RC_NIL;
int     default_pointer_hscroll           = RC_NIL;
int     default_scrollpan_steps           = RC_NIL;
int     default_scroll_wheel              = RC_NIL;
int     default_third_button              = RC_NIL;
int     default_third_button_cancel       = TRUE;

/* Print Related */
int     default_paper_width               = DEFAULT_PAPER_WIDTH; /* letter size */
int     default_paper_height              = DEFAULT_PAPER_HEIGHT;
char   *default_print_command             = NULL;
int     default_print_color               = FALSE;
int     default_print_color_background    = OUTPUT_BACKGROUND_COLOR;
int     default_print_orientation         = LANDSCAPE;
int     default_print_output_extents      = EXTENTS;
int     default_print_output_capstyle     = SQUARE_CAP;
int     default_setpagedevice_orientation = FALSE;
int     default_setpagedevice_pagesize    = FALSE;

/* System Related */
int     default_file_preview              = TRUE;
int     default_handleboxes               = TRUE;
int     default_raise_dialog_boxes        = DEFAULT_RAISE_TIMER;
int     default_save_ui_settings          = TRUE;
int     default_toolbars                  = RC_NIL;
int     default_toolbars_mode             = RC_NIL;
int     default_show_toolbar_tips         = RC_NIL;

/* Text Related */
int     default_text_case                 = LOWER_CASE;

/* default zoom_factor at which text is displayed completely */
int     default_text_display_zoomfactor   = RC_NIL;
int     default_text_feedback             = ONLY_WHEN_READABLE;
int     default_text_origin_marker        = TRUE;
int     default_text_marker_size          = DEFAULT_TEXT_MARKER_SIZE;
int     default_text_marker_threshold     = DEFAULT_TEXT_MARKER_THLD;
int     default_text_size                 = DEFAULT_TEXT_SIZE;

/* Undo System */
int     default_undo_control              = RC_NIL;
int     default_undo_levels               = RC_NIL;
int     default_undo_panzoom              = RC_NIL;
int     default_undo_preserve             = RC_NIL;
int     default_undo_type                 = RC_NIL;

/* Retrieves a string from key file in specified group */
char *i_var_get_global_config_string(EdaConfig *cfg, const char *key) {

  GError *err = NULL;
  char   *tmpstr;
  const char *group = IVAR_CONFIG_GROUP;

  tmpstr = eda_config_get_string (cfg, group, key, &err);

  if (err != NULL) {
    geda_utility_log_quite("%s\n", err->message);
    g_clear_error (&err);
  }

  return tmpstr;
}

/*!
 * \brief Retrieve a color from config given a key and specified group
 * \par Function Description
 *  Retrieve interger list from configuration with \a key in \a group and
 *  set members of \a var from the retrieved values, if the key does not
 *  exist then the values are set from the current display color map using
 *  the color \a index.
 *
 * \param cfg    Pointer to EdaConfig Object
 * \param group  String name of configuration group
 * \param key    string name of record key to be retrieved
 * \param var    Pointer to GdkColor structure whose values are to be set
 * \param index  index of default color used if key does not exist.
 */
void i_var_restore_group_color(EdaConfig *cfg, const char *group, const char *key,
                               GdkColor  *var, int index)
{
  GError   *err = NULL;
  GdkColor *color;
  int      *array;
  unsigned  ridiculous = 4;

  array = eda_config_get_int_list (cfg, group, key, &ridiculous, &err);
  if (err != NULL) {
    g_clear_error (&err);
    color = geda_color_x11_color_from_index(index);
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
    g_free (array);
  }
}

/*!
 * \brief Retrieve a boolean from key file in and specified group
 * \par Function Description
 *  Returns True if the value was restored from configuration or
 *  False if \a def_val was assigned.
 *
 * \param cfg      Pointer to EdaConfig Object
 * \param group    String name of configuration group
 * \param key      string name of record key to be retrieved
 * \param var      Pointer to variable whose value is to be set
 * \param def_val  default returned value if key does not exist.
 */
bool i_var_restore_group_boolean(EdaConfig *cfg, const char *group,
                                                 const char *key,
                                                 int *var, int def_val)
{
  GError *err = NULL;
  bool tmp_bool;
  bool result;

  tmp_bool = eda_config_get_boolean (cfg, group, key, &err);
  if (err != NULL) {
    g_clear_error (&err);
   *var = def_val;
    result = FALSE;
  }
  else {
   *var = tmp_bool;
    result = TRUE;
  }
  return result;
}

/*!
 * \brief Retrieve a double from key file in and specified group
 * \par Function Description
 *  Retrieves a double from key file in specified group. Returns True
 *  if the value was restored from configuration or False if \a def_val
 *  was assigned.
 *
 * \param cfg      Pointer to EdaConfig Object
 * \param group    String name of configuration group
 * \param key      string name of record key to be retrieved
 * \param var      Pointer to variable whose value is to be set
 * \param def_val  default returned value if key does not exist.
 */
bool i_var_restore_group_double (EdaConfig *cfg, const char *group, const char *key, double *var, double def_val)
{
  GError *err = NULL;
  double tmp_val;
  bool result;

  tmp_val = eda_config_get_double (cfg, group, key, &err);
  if (err != NULL) {
    g_clear_error (&err);
   *var = def_val;
    result = FALSE;
  }
  else {
   *var = tmp_val;
    result = TRUE;
  }
  return result;
}

/*!
 * \brief Retrieve a integer from key file in and specified group
 * \par Function Description
 *  Retrieves a integer from key file in specified group. Returns True
 *  if the value was restored from configuration or False if \a def_val
 *  was assigned.
 *
 * \param cfg      Pointer to EdaConfig Object
 * \param group    String name of configuration group
 * \param key      string name of record key to be retrieved
 * \param var      Pointer to variable whose value is to be set
 * \param def_val  default returned value if key does not exist.
 */
bool i_var_restore_group_integer(EdaConfig *cfg, const char *group, const char *key, int *var, int def_val)
{
  GError *err = NULL;
  int tmp_int;
  bool result;

  tmp_int = eda_config_get_integer (cfg, group, key, &err);
  if (err != NULL) {
    g_clear_error (&err);
   *var = def_val;
    result = FALSE;
  }
  else {
   *var = tmp_int;
    result = TRUE;
  }
  return result;
}

/* Retrieve a boolean from key file in global group */
void i_var_restore_global_boolean(EdaConfig *cfg, const char *key, int *var, bool def_val)
{
  const char *group = IVAR_CONFIG_GROUP;
  i_var_restore_group_boolean (cfg, group, key, var, def_val);
}

/* Retrieve a double from key file in global group */
void i_var_restore_global_double(EdaConfig *cfg, const char *key, double *var, double def_val)
{
  const char *group = IVAR_CONFIG_GROUP;
  i_var_restore_group_double (cfg, group, key, var, def_val);
}

/* Retrieve integer from key file in global group */
void i_var_restore_global_integer(EdaConfig *cfg, const char *key, int *var, int def_val)
{
  const char *group = IVAR_CONFIG_GROUP;
  i_var_restore_group_integer (cfg, group, key, var, def_val);
}

/* Retrieve a color from key file in global group */
void i_var_restore_global_color(EdaConfig *cfg, const char *key, GdkColor *var, int index)
{
  i_var_restore_group_color(cfg, IVAR_CONFIG_GROUP, key, var, index);
}

/* Retrieve boolean from key file in global group */
void i_var_restore_window_boolean(EdaConfig *cfg, const char *key, int *var, bool def_val)
{
  const char *group = WINDOW_CONFIG_GROUP;
  i_var_restore_group_boolean (cfg, group, key, var, def_val);
}

/* Retrieve integer from key file in Window group */
void i_var_restore_window_integer(EdaConfig *cfg, const char *key, int *var, int def_val)
{
  const char *group = WINDOW_CONFIG_GROUP;
  i_var_restore_group_integer (cfg, group, key, var, def_val);
}

/* Retrieve a color from key file in Window group */
void i_var_restore_window_color(EdaConfig *cfg, const char *key, GdkColor *var, int index)
{
  i_var_restore_group_color(cfg, WINDOW_CONFIG_GROUP, key, var, index);
}

/*!
 * \internal Recall User Settings
 * \par
 *  This functions retrieves the values of configuration settings from the
 *  new configuration system and assigns the values to the corresponding top
 *  level variables. Note that this is done after the "old" system, so the
 *  RC values will be ignored.
 *
 *  Logging is not performed here because i_vars_set is called to initialize
 *  temporary toplevels for previews, and hence would result in redundant log
 *  entries of the settings being initialize. Therfore the log is written to
 *  before calling in x_window_setup.
 */
static void i_vars_recall_user_settings(GschemToplevel *w_current)
{
  EdaConfig    *cfg      = eda_config_get_user_context ();
  GedaToplevel *toplevel = w_current->toplevel;

  const char   *group;
  const char   *key;

  if (comline_font) { /* If font-family was specified on command line */
    eda_renderer_set_font_name(CairoRenderer, comline_font);
  }
  else {

    char *tmp_str;

    tmp_str = i_var_get_global_config_string (cfg, "default-font-name");

    if (tmp_str != NULL && strlen(tmp_str) > 1) {
      eda_renderer_set_font_name(CairoRenderer, tmp_str);
      GEDA_FREE (tmp_str);
    }
  }

  /* Image Related */
  i_var_restore_global_boolean(cfg, "image-color",        &toplevel->
                                     image_color,          TRUE);

  i_var_restore_global_boolean(cfg, "invert-images",      &toplevel->
                                     invert_images,        TRUE);

  i_var_restore_global_integer(cfg, "image-width",        &w_current->
                                     image_width,          DEFAULT_IMAGE_WIDTH);

  i_var_restore_global_integer(cfg, "image-height",       &w_current->
                                     image_height,         DEFAULT_IMAGE_HEIGHT);

  /* Miscellaneous - in  alphabetical order, saved by: x_settings_save_settings */
  i_var_restore_global_integer(cfg, "auto-save-interval", &toplevel->
                                     auto_save_interval,   DEFAULT_SAVE_INTERVAL);

  i_var_restore_global_boolean(cfg, "continue-component-place", &w_current->
                                     continue_component_place,  TRUE);

  i_var_restore_global_boolean(cfg, "embed-components",   &w_current->
                                     embed_components,     FALSE);

  i_var_restore_global_boolean(cfg, "enforce-hierarchy",  &w_current->
                                     enforce_hierarchy,    TRUE);

  i_var_restore_global_boolean(cfg, "force-boundingbox",  &w_current->
                                     force_boundingbox,    FALSE);

  i_var_restore_global_integer(cfg, "keyboardpan-gain",   &w_current->
                                     keyboardpan_gain,     DEFAULT_KEYBOARD_GAIN);

  i_var_restore_global_boolean(cfg, "netconn-rubberband", &w_current->
                                     netconn_rubberband,   FALSE);

  i_var_restore_global_integer(cfg, "select-slack-pixels", &w_current->
                                     select_slack_pixels,   DEFAULT_SLACK_PIXELS);

  i_var_restore_global_integer(cfg, "snap-size",           &w_current->
                                     snap_size,             DEFAULT_SNAP_SIZE);

  i_var_restore_global_boolean(cfg, "sort-component-library", &w_current->
                                     sort_component_library,   FALSE);

  /* User GedaFileChooser filter preference - Saved by: x_window_save_settings */
  group = FILE_CHOOSER_CONFIG_GROUP;
  key   = FILE_CHOOSER_CONFIG_FILTER;

  i_var_restore_group_integer (cfg,  group, key,           &w_current->
                                     chooser_filter,        FILTER_GSCHEM);

  /* Scrolling Settings - Saved by: x_window_save_settings */
  i_var_restore_window_integer(cfg, "scrollbars",          &w_current->
                                     scrollbars,            TRUE);

  i_var_restore_window_integer(cfg, "scrollbar-update",    &w_current->
                                     scrollbar_update,      TRUE);

  i_var_restore_window_integer(cfg, "scrollbars-visible",  &w_current->
                                     scrollbars_visible,    TRUE);

  /* Display Settings - Saved by: x_window_save_settings */
  i_var_restore_window_integer(cfg, "render-adaptor",      &w_current->
                                     render_adaptor,        DEFAULT_RENDERER);

  i_var_restore_window_integer(cfg, "action-color",        &w_current->
                                     action_color,          DEFAULT_ACTION_COLOR);

  i_var_restore_window_integer(cfg, "anti-aliasing",       &w_current->
                                     anti_aliasing,         DEFAULT_ANTI_ALIASING);

  /* Grips Settings - Saved by: x_window_save_settings */
  i_var_restore_window_boolean(cfg, "draw-grips",          &CairoRenderer->
                                     draw_grips,            TRUE);

  i_var_restore_window_integer(cfg, "grip-size",            &w_current->
                                     grip_size,              DEFAULT_GRIP_SIZE);

  i_var_restore_window_color (cfg,  "grips-stroke-color",  &CairoRenderer->
                                     grip_stroke_color,     DEFAULT_GRIP_STROKE_COLOR);

  i_var_restore_window_color (cfg,  "grips-fill-color",    &CairoRenderer->
                                     grip_fill_color,       DEFAULT_GRIP_FILL_COLOR);

  /* Grid Settings - Saved by: x_window_save_settings */
  i_var_restore_window_integer(cfg, "grid-mode",           &w_current->
                                     grid_mode,             GRID_MESH);

  i_var_restore_window_color (cfg,  "dots-grid-minor-color",  &w_current->
                                     dots_grid_minor_color,    DOTS_GRID_COLOR);

  i_var_restore_window_color (cfg,  "dots-grid-major-color",  &w_current->
                                     dots_grid_major_color,    DOTS_GRID_COLOR);

  i_var_restore_window_integer(cfg, "dots-grid-dot-size",     &w_current->
                                     dots_grid_dot_size,       DEFAULT_GRID_DOT_SIZE);

  i_var_restore_window_integer(cfg, "dots-grid-mode",         &w_current->
                                     dots_grid_mode,           DOTS_GRID_VARIABLE_MODE);

  i_var_restore_window_integer(cfg, "grid-dot-threshold",     &w_current->
                                     dots_grid_threshold,      DEFAULT_GRID_DOT_THRESHOLD);

  i_var_restore_window_integer(cfg, "mesh-grid-threshold",    &w_current->
                                     mesh_grid_threshold,      DEFAULT_GRID_MESH_THRESHOLD);

  i_var_restore_window_integer(cfg, "mesh-line-width-factor", &w_current->
                                     mesh_line_width_factor,   DEFAULT_MESH_LINE_WIDTH_FACTOR);

  i_var_restore_window_color (cfg,  "mesh-grid-minor-color",  &w_current->
                                     mesh_grid_minor_color,    MESH_GRID_MINOR_COLOR);

  i_var_restore_window_color (cfg,  "mesh-grid-major-color",  &w_current->
                                     mesh_grid_major_color,    MESH_GRID_MAJOR_COLOR);

  /* Restore Cues & Endpoints settings - Saved by: x_window_save_settings */
  i_var_restore_window_integer(cfg, "junction-size",       &CairoRenderer->
                                     junction_size,         DEFAULT_JUNCTION_SIZE);

  i_var_restore_window_color (cfg,  "junction-color",      &CairoRenderer->
                                     junction_color,        DEFAULT_JUNCTION_COLOR);

  i_var_restore_window_color (cfg,  "net-endpoint-color",  &CairoRenderer->
                                     net_endpoint_color,    DEFAULT_NET_ENDPOINT_COLOR);

  /* Misc Drawing Window Related - Saved by: x_window_save_settings */
  i_var_restore_window_boolean(cfg, "object-clipping",     &w_current->
                                     object_clipping,       TRUE);

  /* Restore text related stuff - Saved by: x_settings_save_settings */
  i_var_restore_global_integer(cfg, "text-case",           &w_current->text_case,     BOTH_CASES);
  i_var_restore_global_integer(cfg, "text-zoomfactor",     &w_current->text_display_zoomfactor, DEFAULT_TEXT_ZOOM);
  i_var_restore_global_integer(cfg, "text-feedback",       &w_current->text_feedback, ALWAYS_FEEDBACK);
  i_var_restore_global_integer(cfg, "text-size",           &w_current->text_size,     DEFAULT_TEXT_SIZE);
  i_var_restore_global_boolean(cfg, "text-origin-marker",  &CairoRenderer->
                                     text_origin_marker,      TRUE);
  i_var_restore_global_integer(cfg, "text-marker-size",      &CairoRenderer->
                                     text_marker_size,        DEFAULT_TEXT_MARKER_SIZE);
  i_var_restore_global_double(cfg,  "text-marker-threshold", &CairoRenderer->
                                     text_marker_threshold,   DEFAULT_TEXT_MARKER_THLD);
  i_var_restore_global_color(cfg,   "text_marker_color",     &CairoRenderer->
                                     text_marker_color,       DEFAULT_TEXT_MARKER_COLOR);

  /* Pointer Device, aka Mouse stuff - Saved by: x_window_save_settings */
  i_var_restore_global_integer(cfg, "cursor-index",    &w_current->drawing_pointer, DEFAULT_CURSOR_INDEX);
  i_var_restore_global_integer(cfg, "drag-can-move",   &w_current->drag_can_move,   TRUE);
  i_var_restore_global_integer(cfg, "fast-mousepan",   &w_current->fast_mousepan,   TRUE);
  i_var_restore_global_integer(cfg, "middle-button",   &w_current->middle_button,   DEFAULT_MOUSE_MIDDLE);
  i_var_restore_global_integer(cfg, "mousepan-gain",   &w_current->mousepan_gain,   DEFAULT_MOUSEPAN_GAIN);
  i_var_restore_global_integer(cfg, "pointer-hscroll", &w_current->pointer_hscroll, FALSE);
  i_var_restore_global_integer(cfg, "scrollpan-steps", &w_current->scrollpan_steps, DEFAULT_SCROLLPAN_STEPS);
  i_var_restore_global_integer(cfg, "scroll-wheel",    &w_current->scroll_wheel,    SCROLL_WHEEL_CLASSIC);
  i_var_restore_global_integer(cfg, "third-button",    &w_current->third_button,    POPUP_ENABLED);
}

/*!
 * \brief This functions sets the values of top-level interger variables.
 * \par Function Description
 *  This functions assigns the current default values to the toplevel
 *  variables. The first time this occurs is in the main-line after
 *  rc initialization files have "executed", which may have changed
 *  the value of the defaults variables.
 *
 * Maintenance:
 *
 *   The variables in this section have been organized into groups. The
 *   groups are listed "mostly" alphabeticaly, with the non-catagorized
 *   group last. Variables are listed alphabeticaly within each group.
 */
void i_vars_set(GschemToplevel *w_current)
{
  inline void i_set_rc(int *var, int rc) { if (rc != RC_NIL) *var = rc; };

  GedaToplevel *toplevel               = w_current->toplevel;

  geda_iface_vars_set(toplevel);

  i_vars_recall_user_settings (w_current);

  w_current->world_left                = default_world_left;
  w_current->world_right               = default_world_right;
  w_current->world_bottom              = default_world_bottom;
  w_current->world_top                 = default_world_top;

  i_set_rc (&w_current->action_color,    default_action_color);
  i_set_rc (&w_current->anti_aliasing,   default_anti_aliasing);
  i_set_rc (&CairoRenderer->draw_grips,  default_draw_grips);

  CairoRenderer->draw_complex_grips    = default_draw_complex_grips;

  i_set_rc (&w_current->grip_size,       default_grip_size);
  i_set_rc (&w_current->render_adaptor,  default_render_adaptor);

/* Color Related */
  w_current->override_net_color        = default_override_net_color;
  w_current->override_bus_color        = default_override_bus_color;
  w_current->override_pin_color        = default_override_pin_color;

  i_set_rc (&w_current->grid_mode,             default_grid_mode);
  i_set_rc (&w_current->dots_grid_dot_size,    default_dots_grid_dot_size);
  i_set_rc (&w_current->dots_grid_mode,        default_dots_grid_mode);
  i_set_rc (&w_current->dots_grid_threshold,   default_dots_grid_threshold);

  w_current->dots_grid_minor_alpha     = default_dots_grid_minor_alpha;
  w_current->dots_grid_major_alpha     = default_dots_grid_major_alpha;

  i_set_rc (&w_current->mesh_grid_threshold,     default_mesh_grid_threshold);
  i_set_rc (&w_current->mesh_line_width_factor,  default_mesh_line_width_factor);

  w_current->mesh_grid_minor_alpha     = default_mesh_grid_minor_alpha;
  w_current->mesh_grid_major_alpha     = default_mesh_grid_major_alpha;

  if (default_dots_grid_minor_color.pixel != 88) {
    w_current->dots_grid_minor_color.red   = default_dots_grid_minor_color.red;
    w_current->dots_grid_minor_color.green = default_dots_grid_minor_color.green;
    w_current->dots_grid_minor_color.blue  = default_dots_grid_minor_color.blue;
  }

  if (default_dots_grid_major_color.pixel != 88) {
    w_current->dots_grid_major_color.red   = default_dots_grid_major_color.red;
    w_current->dots_grid_major_color.green = default_dots_grid_major_color.green;
    w_current->dots_grid_major_color.blue  = default_dots_grid_major_color.blue;
  }

  if (default_mesh_grid_minor_color.pixel != 88) {
    w_current->mesh_grid_minor_color.red   = default_mesh_grid_minor_color.red;
    w_current->mesh_grid_minor_color.green = default_mesh_grid_minor_color.green;
    w_current->mesh_grid_minor_color.blue  = default_mesh_grid_minor_color.blue;
  }

  if (default_mesh_grid_minor_color.pixel != 88) {
    w_current->mesh_grid_major_color.red   = default_mesh_grid_major_color.red;
    w_current->mesh_grid_major_color.green = default_mesh_grid_major_color.green;
    w_current->mesh_grid_major_color.blue  = default_mesh_grid_major_color.blue;
  }

/* Misc Drawing Window Related */
  i_set_rc (&w_current->object_clipping,    default_object_clipping);

/* Scrolling Settings */
  i_set_rc (&w_current->scrollbars,         default_scrollbars);
  i_set_rc (&w_current->scrollbar_update,   default_scrollbar_update);
  i_set_rc (&w_current->scrollbars_visible, default_scrollbars_visible);

  w_current->warp_cursor               = default_warp_cursor;
  w_current->zoom_gain                 = default_zoom_gain;
  w_current->zoom_with_pan             = default_zoom_with_pan;

/* Imaging Related */
  i_set_rc (&toplevel->image_color,      default_image_color);
  i_set_rc (&toplevel->invert_images,    default_invert_images);
  i_set_rc (&w_current->image_width,     default_image_width);
  i_set_rc (&w_current->image_height,    default_image_height);


/* Miscellaneous - in  alphabetical order */
  w_current->action_feedback_mode      = default_action_feedback_mode;
  w_current->add_attribute_offset      = default_add_attribute_offset;
  toplevel->attribute_offset           = default_add_attribute_offset;

  w_current->attribute_placement_grid  = default_attribute_placement_grid;
  w_current->auto_pan                  = default_auto_pan;
  w_current->auto_pan_step             = default_auto_pan_step;

  i_set_rc (&toplevel->auto_save_interval, default_auto_save_interval);

  i_set_rc (&w_current->chooser_filter, default_chooser_filter);

  w_current->component_select_attrlist = default_component_select_attrlist;

  i_set_rc (&w_current->continue_component_place, default_continue_component_place);
  i_set_rc (&w_current->embed_components,         default_embed_components);
  i_set_rc (&w_current->enforce_hierarchy,        default_enforce_hierarchy);
  i_set_rc (&w_current->force_boundingbox,        default_force_boundingbox);
  i_set_rc (&w_current->keyboardpan_gain,         default_keyboardpan_gain);
  i_set_rc (&w_current->netconn_rubberband,       default_netconn_rubberband);

  i_set_rc (&w_current->select_slack_pixels,      default_select_slack_pixels);
  i_set_rc (&w_current->snap_size,                default_snap_size);
  i_set_rc (&w_current->sort_component_library,   default_sort_component_library);

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
  i_set_rc (&w_current->drag_can_move,   default_drag_can_move);
  i_set_rc (&w_current->fast_mousepan,   default_fast_mousepan);
  i_set_rc (&w_current->middle_button,   default_middle_button);
  i_set_rc (&w_current->mousepan_gain,   default_mousepan_gain);
  i_set_rc (&w_current->pointer_hscroll, default_pointer_hscroll);
  i_set_rc (&w_current->scrollpan_steps, default_scrollpan_steps);
  i_set_rc (&w_current->scroll_wheel,    default_scroll_wheel);
  i_set_rc (&w_current->third_button,    default_third_button);

  w_current->third_button_cancel       = default_third_button_cancel;

/* Print & Related */
  INIT_STR(w_current, print_command, DEFAULT_PRINT_COMMAND);

  toplevel->paper_width                = default_paper_width;
  toplevel->paper_height               = default_paper_height;
  toplevel->print_color                = default_print_color;
  toplevel->print_color_background     = default_print_color_background;
  toplevel->print_orientation          = default_print_orientation;
  toplevel->print_output_extents       = default_print_output_extents;
  toplevel->print_output_capstyle      = default_print_output_capstyle;
  toplevel->setpagedevice_orientation  = default_setpagedevice_orientation;
  toplevel->setpagedevice_pagesize     = default_setpagedevice_pagesize;

  /* System Related */
  w_current->file_preview              = default_file_preview;
  w_current->handleboxes               = default_handleboxes;
  w_current->raise_dialog_boxes        = default_raise_dialog_boxes ? DEFAULT_RAISE_TIMER : 0;
  w_current->save_ui_settings          = default_save_ui_settings;

  i_set_rc (&w_current->toolbars,           default_toolbars);
  i_set_rc (&w_current->toolbars_mode,      default_toolbars_mode);
  i_set_rc (&w_current->show_toolbar_tips,  default_show_toolbar_tips);

/* Text Related */
  w_current->text_case                   = default_text_case;

/* default zoom_factor at which text is displayed completely */
  i_set_rc (&w_current->text_display_zoomfactor, default_text_display_zoomfactor);

  w_current->text_feedback               = default_text_feedback;
   CairoRenderer->text_origin_marker     = default_text_origin_marker;
   CairoRenderer->text_marker_size       = default_text_marker_size;
   CairoRenderer->text_marker_threshold  = default_text_marker_threshold  / 10.0;
  w_current->text_size                   = default_text_size;

/* Undo Sub-System */
  i_set_rc (&w_current->undo_levels,     default_undo_levels);
  i_set_rc (&w_current->undo_control,    default_undo_control);
  i_set_rc (&w_current->undo_type,       default_undo_type);
  i_set_rc (&w_current->undo_panzoom,    default_undo_panzoom);
  i_set_rc (&w_current->undo_preserve,   default_undo_preserve);
}

/*!
 * \brief Free default names
 * \par Function Description
 *  This function will free all of the default variables for gschem.
 */
void i_vars_freenames()
{
  GEDA_FREE(comline_font);
  GEDA_FREE(default_print_command);
  GEDA_FREE(default_bus_ripper_symname);
  geda_glist_free_full (default_component_select_attrlist, g_free);
}

/*!
 * \brief Setup gschem default configuration.
 * \par Function Description
 *  Populate the default configuration context with compiled-in
 *  defaults.
 */
void i_vars_init(GschemToplevel *w_current)
{
  EdaConfig  *cfg   = eda_config_get_default_context ();
  const char *group = IVAR_CONFIG_GROUP;

  /* read in Gtk RC files */
  g_rc_parse_gtkrc();

  /* Set values for all variables */
  v_log_message(_("Initializing default configuration settings\n"));

  /* This is the prefix of the default filename used for newly created
   * schematics and symbols. */
  /*! \remark TRANSLATORS: This string is used to generate a filename for
   *   newly-created files in the form "untitled_N.sch", where N is a number.
   *   Please make sure the translation contains characters suitable for use
   *   in a filename. */
  eda_config_set_string (cfg, group, "default-filename",   _(DEFAULT_UNTITLED_NAME));
  eda_config_set_string (cfg, group, "default-font-name",   (DEFAULT_FONT_NAME));
  eda_config_set_string (cfg, group, "default-titleblock", _(DEFAULT_TITLEBLOCK));

  eda_config_set_boolean (cfg, group, "auto-file-suffix",    TRUE);

  eda_config_set_boolean (cfg, IDS_COMP_SELECT, "sort",      FALSE);
  eda_config_set_boolean (cfg, IDS_COMP_SELECT, "groups",    TRUE);
  eda_config_set_boolean (cfg, IDS_COMP_SELECT, "subgroups", TRUE);
  eda_config_set_boolean (cfg, IDS_COMP_SELECT, "showtips",  TRUE);
  eda_config_set_integer (cfg, IDS_COMP_SELECT, "style",     255);

  /* read additional RC files, which may over-ride the hard-coded defaults! */
  x_rc_parse_gschem (w_current, rc_filename);

  if (w_current->save_ui_settings) {
    gschem_atexit (i_vars_atexit_save_user_config, NULL);
  }
}

/*!
 * \brief Save user config on exit.
 * \par Function Description
 *  When gschem exits, try to save the user configuration to disk.
 */
void i_vars_atexit_save_user_config (void * user_data)
{
  EdaConfig *cfg = eda_config_get_user_context ();
  GError    *err = NULL;

  eda_config_save (cfg, &err);

  if (err != NULL) {

    fprintf (stderr, "%s '%s': %s.", _("Failed to save user configuration to"),
                                        eda_config_get_filename (cfg),
                                        err->message);
    g_clear_error (&err);
  }
}
