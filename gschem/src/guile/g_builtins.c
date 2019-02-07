/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/*! \file g_builtins.c
 * \brief gschem built-in actions
 */

#include <gschem.h>

struct BuiltinInfo {
  const char *name;
  int req, opt, rst;
  SCM (*func)();
};

static struct BuiltinInfo builtins[] = {
  /*
  { "%file-new-window",           0, 0, 0, g_keys_file_new_window },
  { "%file-new",                  0, 0, 0, g_keys_file_new },
  { "%file-open",                 0, 0, 0, g_keys_file_open },
  { "%file-script",               0, 0, 0, g_keys_file_script },
  { "%file-save",                 0, 0, 0, g_keys_file_save },
  { "%file-save-as",              0, 0, 0, g_keys_file_save_as },
  { "%file-save-all",             0, 0, 0, g_keys_file_save_all },
  { "%file-print",                0, 0, 0, g_keys_file_print },
  { "%file-write-image",          0, 0, 0, g_keys_file_write_png },
  { "%file-close",                0, 0, 0, g_keys_file_close },
  { "%file-quit",                 0, 0, 0, g_keys_file_quit },
  { "%edit-undo",                 0, 0, 0, g_keys_edit_undo },
  { "%edit-redo",                 0, 0, 0, g_keys_edit_redo },
  { "%edit-select",               0, 0, 0, g_keys_edit_select },
  { "%edit-select-all",           0, 0, 0, g_keys_edit_select_all },
  { "%edit-deselect",             0, 0, 0, g_keys_edit_deselect },
  { "%edit-copy",                 0, 0, 0, g_keys_edit_copy },
  { "%edit-mcopy",                0, 0, 0, g_keys_edit_mcopy },
  { "%edit-move",                 0, 0, 0, g_keys_edit_move },
  { "%edit-delete",               0, 0, 0, g_keys_edit_delete },
  { "%edit-rotate-left",          0, 0, 0, g_keys_edit_rotate_left },
  { "%edit-rotate-right",         0, 0, 0, g_keys_edit_rotate_right },
  { "%edit-mirror",               0, 0, 0, g_keys_edit_mirror },
  { "%edit-slot",                 0, 0, 0, g_keys_edit_slot },
  { "%edit-color",                0, 0, 0, g_keys_edit_color },
  { "%edit-edit",                 0, 0, 0, g_keys_edit_edit },
  { "%edit-text",                 0, 0, 0, g_keys_edit_text },
  { "%edit-lock",                 0, 0, 0, g_keys_edit_lock },
  { "%edit-unlock",               0, 0, 0, g_keys_edit_unlock },
  { "%edit-linetype",             0, 0, 0, g_keys_edit_linetype },
  { "%edit-filltype",             0, 0, 0, g_keys_edit_filltype },
  { "%edit-pin-type",             0, 0, 0, g_keys_edit_pin_type },
  { "%edit-translate",            0, 0, 0, g_keys_edit_translate },
  { "%edit-invoke-macro",         0, 0, 0, g_keys_edit_invoke_macro },
  { "%edit-embed",                0, 0, 0, g_keys_edit_embed },
  { "%edit-unembed",              0, 0, 0, g_keys_edit_unembed },
  { "%edit-update",               0, 0, 0, g_keys_edit_update },
  { "%edit-show-hidden",          0, 0, 0, g_keys_edit_show_hidden },
  { "%edit-find-text",            0, 0, 0, g_keys_edit_find },
  { "%edit-show-text",            0, 0, 0, g_keys_edit_show_text },
  { "%edit-hide-text",            0, 0, 0, g_keys_edit_hide_text },
  { "%edit-autonumber",           0, 0, 0, g_keys_edit_autonumber_text },

  { "%clipboard-copy",            0, 0, 0, g_keys_clipboard_copy },
  { "%clipboard-cut",             0, 0, 0, g_keys_clipboard_cut },
  { "%clipboard-paste",           0, 0, 0, g_keys_clipboard_paste },

  { "%buffer-copy1",              0, 0, 0, g_keys_buffer_copy1 },
  { "%buffer-copy2",              0, 0, 0, g_keys_buffer_copy2 },
  { "%buffer-copy3",              0, 0, 0, g_keys_buffer_copy3 },
  { "%buffer-copy4",              0, 0, 0, g_keys_buffer_copy4 },
  { "%buffer-copy5",              0, 0, 0, g_keys_buffer_copy5 },
  { "%buffer-cut1",               0, 0, 0, g_keys_buffer_cut1 },
  { "%buffer-cut2",               0, 0, 0, g_keys_buffer_cut2 },
  { "%buffer-cut3",               0, 0, 0, g_keys_buffer_cut3 },
  { "%buffer-cut4",               0, 0, 0, g_keys_buffer_cut4 },
  { "%buffer-cut5",               0, 0, 0, g_keys_buffer_cut5 },
  { "%buffer-paste1",             0, 0, 0, g_keys_buffer_paste1 },
  { "%buffer-paste2",             0, 0, 0, g_keys_buffer_paste2 },
  { "%buffer-paste3",             0, 0, 0, g_keys_buffer_paste3 },
  { "%buffer-paste4",             0, 0, 0, g_keys_buffer_paste4 },
  { "%buffer-paste5",             0, 0, 0, g_keys_buffer_paste5 },

  { "%view-sidebar",              0, 0, 0, g_keys_view_sidebar },
  { "%view-status",               0, 0, 0, g_keys_view_status },
  { "%view-redraw",               0, 0, 0, g_keys_view_redraw },
  { "%view-zoom-full",            0, 0, 0, g_keys_view_zoom_full },
  { "%view-zoom-extents",         0, 0, 0, g_keys_view_zoom_extents },
  { "%view-zoom-in",              0, 0, 0, g_keys_view_zoom_in },
  { "%view-zoom-out",             0, 0, 0, g_keys_view_zoom_out },
  { "%view-zoom-box",             0, 0, 0, g_keys_view_zoom_box },
  { "%view-pan",                  0, 0, 0, g_keys_view_pan },
 */
  { "%view-pan-left",             0, 0, 0, h_keys_view_pan_left },
  { "%view-pan-right",            0, 0, 0, h_keys_view_pan_right },
  { "%view-pan-up",               0, 0, 0, h_keys_view_pan_up },
  { "%view-pan-down",             0, 0, 0, h_keys_view_pan_down },

/*
  { "%view-dark-colors",          0, 0, 0, g_keys_view_dark_colors },
  { "%view-light-colors",         0, 0, 0, g_keys_view_light_colors },
  { "%view-bw-colors",            0, 0, 0, g_keys_view_bw_colors },
  { "%page-manager",              0, 0, 0, g_keys_page_manager },
  { "%page-next",                 0, 0, 0, g_keys_page_next },
  { "%page-prev",                 0, 0, 0, g_keys_page_prev },
  { "%page-close",                0, 0, 0, g_keys_page_close },
  { "%page-revert",               0, 0, 0, g_keys_page_revert },
  { "%page-print",                0, 0, 0, g_keys_page_print },
  { "%add-component",             0, 0, 0, g_keys_add_component },
  { "%add-attribute",             0, 0, 0, g_keys_add_attribute },
  { "%add-net",                   0, 0, 0, g_keys_add_net },
  { "%add-bus",                   0, 0, 0, g_keys_add_bus },
  { "%add-text",                  0, 0, 0, g_keys_add_text },
  { "%add-path",                  0, 0, 0, g_keys_add_path },
  { "%add-line",                  0, 0, 0, g_keys_add_line },
  { "%add-box",                   0, 0, 0, g_keys_add_box },
  { "%add-picture",               0, 0, 0, g_keys_add_picture},
  { "%add-circle",                0, 0, 0, g_keys_add_circle },
  { "%add-arc",                   0, 0, 0, g_keys_add_arc },
  { "%add-pin",                   0, 0, 0, g_keys_add_pin },
  { "%hierarchy-down-schematic",  0, 0, 0, g_keys_hierarchy_down_schematic },
  { "%hierarchy-down-symbol",     0, 0, 0, g_keys_hierarchy_down_symbol },
  { "%hierarchy-up",              0, 0, 0, g_keys_hierarchy_up },
  { "%attributes-attach",         0, 0, 0, g_keys_attributes_attach },
  { "%attributes-detach",         0, 0, 0, g_keys_attributes_detach },
  { "%attributes-show-name",      0, 0, 0, g_keys_attributes_show_name },
  { "%attributes-show-value",     0, 0, 0, g_keys_attributes_show_value },
  { "%attributes-show-both",      0, 0, 0, g_keys_attributes_show_both },
  { "%attributes-visibility-toggle", 0, 0, 0, g_keys_attributes_visibility_toggle },
  { "%options-text-size",         0, 0, 0, g_keys_options_text_size },
  { "%options-snap-size",         0, 0, 0, g_keys_options_snap_size },
  { "%options-scale-up-snap-size",  0, 0, 0, g_keys_options_scale_up_snap_size },
  { "%options-scale-down-snap-size",0, 0, 0, g_keys_options_scale_down_snap_size },
  { "%options-action-feedback",   0, 0, 0, g_keys_options_afeedback },
  { "%options-grid",              0, 0, 0, g_keys_options_grid },
  { "%options-snap",              0, 0, 0, g_keys_options_snap },
  { "%options-rubberband",        0, 0, 0, g_keys_options_rubberband },
  { "%options-magneticnet",       0, 0, 0, g_keys_options_magneticnet },
  { "%options-show-log-window",   0, 0, 0, g_keys_options_show_log_window },
  { "%options-show-coord-window", 0, 0, 0, g_keys_options_show_coord_window },
  { "%help-about",                0, 0, 0, g_keys_help_about },
  { "%help-hotkeys",              0, 0, 0, g_keys_help_hotkeys },
  { "%cancel",                    0, 0, 0, h_keys_cancel },
*/
  { NULL, 0, 0, 0, NULL }, /* sentinel */
};

/*!
 * \brief Create the (gschem core builtins) Scheme module.
 * \par Function Description
 *  Defines procedures in the (gschem core builtins) module. The module
 *  can be accessed using (use-modules (gschem core builtins)).
 */
static void
init_module_gschem_core_builtins ()
{
  static GList *action_list;

  char name[56] = {"%\0",};

  char *built_action = &name[1];

  /* Register functions and add them to the module's public
   * definitions.*/
  i_command_get_action_list(&action_list);

  lambda (char *action) {
    strcpy(built_action, action);
    scm_c_define_gsubr (&name[0], 0, 0, 0, g_process_anonymous_action);
    scm_c_export (&name[0], NULL);
    return FALSE;
  }
  foreach(action_list)

  int i;

  for (i = 0; builtins[i].name != NULL; ++i) {
    struct BuiltinInfo b = builtins[i];
    scm_c_define_gsubr (b.name, b.req, b.opt, b.rst, b.func);
    scm_c_export (b.name, NULL);
  }
}

/*!
 * \brief Initialise gschem built-in actions.
 * \par Function Description
 *  Registers the Scheme procedures used to access gschem's built-in
 *  editing actions implemented in C. Should only be called by main_prog().
 */
void g_init_builtins ()
{
  /* Define the (gschem core builtins) module */
  scm_c_define_module ("gschem core builtins",
                       init_module_gschem_core_builtins,
                       NULL);
}
