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
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief */
struct gsubr_t {
  char* name;
  int req;
  int opt;
  int rst;
  SCM (*fnc)();
};

/*! \brief */
static struct gsubr_t gschem_rc_funcs[] = {
  /* rc file */

  /* Depreciated */
  /* This sections exist to segregate depreciated functions or features
   * but are still maintained in order to help user transition from
   * previous version.
   */

  /* extending support for 20110115 */
  { "drag-can-move",               1, 0, 0, g_rc_drag_can_move },
  { "logging-destination",         1, 0, 0, g_rc_log_destiny },
  { "text-caps-style",             1, 0, 0, g_rc_text_case },
  { "mesh-grid-display-threshold", 1, 0, 0, g_rc_mesh_grid_threshold },
  /* End legacy support */

  { "gschem-version",            1, 0, 0, g_rc_gschem_version },

  { "display-color-map",         0, 1, 0, g_rc_display_color_map },
  { "display-outline-color-map", 0, 1, 0, g_rc_display_outline_color_map },
  { "draw-grips",                1, 0, 0, g_rc_draw_grips },
  { "grid-mode",                 1, 0, 0, g_rc_grid_mode },
  { "dots-grid-dot-size",        1, 0, 0, g_rc_dots_grid_dot_size },
  { "dots-grid-fixed-threshold", 1, 0, 0, g_rc_dots_grid_fixed_threshold },
  { "dots-grid-mode",            1, 0, 0, g_rc_dots_grid_mode },
  { "mesh-grid-threshold",       1, 0, 0, g_rc_mesh_grid_threshold },

  { "net-endpoint-mode",         1, 0, 0, g_rc_net_endpoint_mode },
  { "net-midpoint-mode",         1, 0, 0, g_rc_net_midpoint_mode },
  { "net-direction-mode",        1, 0, 0, g_rc_net_direction_mode },
  { "net-selection-mode",        1, 0, 0, g_rc_net_selection_mode },
  { "zoom-with-pan",             1, 0, 0, g_rc_zoom_with_pan },
  { "action-feedback-mode",      1, 0, 0, g_rc_action_feedback_mode },
  { "object-clipping",           1, 0, 0, g_rc_object_clipping },
  { "embed-components",          1, 0, 0, g_rc_embed_components },
  { "component-dialog-attributes",1, 0, 0, g_rc_component_dialog_attributes },
  { "attribute-name",            1, 0, 0, g_rc_attribute_name },
  { "logging",                   1, 0, 0, g_rc_logging },
  { "log-destiny",               1, 0, 0, g_rc_log_destiny },
  { "console-window",            1, 0, 0, g_rc_console_window },
  { "console-window-type",       1, 0, 0, g_rc_console_window_type },

  { "snap-size",                 1, 0, 0, g_rc_snap_size },

  { "paper-size",                2, 0, 0, g_rc_paper_size },
  { "paper-sizes",               3, 0, 0, g_rc_paper_sizes },
  { "output-type",               1, 0, 0, g_rc_output_type },
  { "output-orientation",        1, 0, 0, g_rc_output_orientation },
  { "output-color",              1, 0, 0, g_rc_output_color },
  { "output-capstyle",           1, 0, 0, g_rc_output_capstyle },
  { "image-color",               1, 0, 0, g_rc_image_color },
  { "invert-images",             1, 0, 0, g_rc_invert_images },
  { "image-size",                2, 0, 0, g_rc_image_size },
  { "third-button",              1, 0, 0, g_rc_third_button },
  { "map-keys",                  2, 0, 0, g_rc_map_keys },
  { "middle-button",             1, 0, 0, g_rc_middle_button },
  { "scroll-wheel",              1, 0, 0, g_rc_scroll_wheel },
  { "pointer-hscroll",           1, 0, 0, g_rc_pointer_hscroll },
  { "net-consolidate",           1, 0, 0, g_rc_net_consolidate },
  { "enforce-hierarchy",         1, 0, 0, g_rc_enforce_hierarchy },
  { "fast-mousepan",             1, 0, 0, g_rc_fast_mousepan },
  { "continue-component-place",  1, 0, 0, g_rc_continue_component_place },

  { "netconn-rubberband",        1, 0, 0, g_rc_netconn_rubberband },
  { "magnetic-net-mode",         1, 0, 0, g_rc_magnetic_net_mode },
  { "sort-component-library",    1, 0, 0, g_rc_sort_component_library },
  { "add-menu",                  2, 0, 0, g_rc_add_menu },
  { "window-size",               2, 0, 0, g_rc_window_size },
  { "warp-cursor",               1, 0, 0, g_rc_warp_cursor },
  { "setpagedevice-orientation", 1, 0, 0, g_rc_setpagedevice_orientation },
  { "setpagedevice-pagesize",    1, 0, 0, g_rc_setpagedevice_pagesize },
  { "bus-ripper-size",           1, 0, 0, g_rc_bus_ripper_size },
  { "bus-ripper-type",           1, 0, 0, g_rc_bus_ripper_type },
  { "bus-ripper-rotation",       1, 0, 0, g_rc_bus_ripper_rotation },
  { "bus-ripper-symname",        1, 0, 0, g_rc_bus_ripper_symname },
  { "force-boundingbox",         1, 0, 0, g_rc_force_boundingbox },

  { "add-attribute-offset",      1, 0, 0, g_rc_add_attribute_offset },
  { "auto-load-last",            1, 0, 0, g_rc_auto_load_last },
  { "attribute-placement-grid",  1, 0, 0, g_rc_attribute_placement_grid },
  { "mousepan-gain",             1, 0, 0, g_rc_mousepan_gain },
  { "keyboardpan-gain",          1, 0, 0, g_rc_keyboardpan_gain },
  { "print-command",             1, 0, 0, g_rc_print_command },
  { "select-slack-pixels",       1, 0, 0, g_rc_select_slack_pixels },
  { "zoom-gain",                 1, 0, 0, g_rc_zoom_gain },

  /* backup functions */
  { "auto-save-interval",        1, 0, 0, g_rc_auto_save_interval },

  /* System Related */
  { "file-preview",              1, 0, 0, g_rc_file_preview },
  { "handleboxes",               1, 0, 0, g_rc_handleboxes },
  { "raise-dialog-boxes-on-expose", 1, 0, 0, g_rc_raise_dialog_boxes_on_expose },
  { "save-ui-settings",          1, 0, 0, g_rc_save_ui_settings },
  { "toolbars",                  1, 0, 0, g_rc_toolbars },
  { "toolbars-mode",             1, 0, 0, g_rc_toolbars_mode },

  /* Scrollbar Options */
  { "scrollbars",                1, 0, 0, g_rc_scrollbars },
  { "scrollbar-update",          1, 0, 0, g_rc_scrollbar_update },
  { "scrollbars-visible",        1, 0, 0, g_rc_scrollbars_visible },
  { "scrollpan-steps",           1, 0, 0, g_rc_scrollpan_steps },

  /* Text Options */
  { "text-case",                 1, 0, 0, g_rc_text_case },
  { "text-feedback",             1, 0, 0, g_rc_text_feedback },
  { "text-display-zoomfactor",   1, 0, 0, g_rc_text_display_zoomfactor },
  { "text-origin-marker",        1, 0, 0, g_rc_text_origin_marker },
  { "text-marker-size",          1, 0, 0, g_rc_text_marker_size },
  { "text-size",                 1, 0, 0, g_rc_text_size },

  /* Undo System */
  { "undo-levels",               1, 0, 0, g_rc_undo_levels },
  { "undo-control",              1, 0, 0, g_rc_undo_control },
  { "undo-type",                 1, 0, 0, g_rc_undo_type },
  { "undo-panzoom",              1, 0, 0, g_rc_undo_panzoom },

  { NULL,                        0, 0, 0, NULL }
};

static struct gsubr_t gschem_g_funcs[] = {
  /* general guile functions */
  { "gschem-print",              1, 0, 0, g_funcs_print },
  { "gschem-postscript",         1, 0, 0, g_funcs_postscript },
  { "gschem-image",              1, 0, 0, g_funcs_image },
  { "gschem-use-rc-values",      0, 0, 0, g_funcs_use_rc_values },
  { "gschem-exit",               0, 0, 0, g_funcs_exit },
  { "gschem-log",                1, 0, 0, g_funcs_log },
  { "gschem-msg",                1, 0, 0, g_funcs_msg },
  { "gschem-confirm",            1, 0, 0, g_funcs_confirm },
  { "gschem-filesel",            2, 0, 1, g_funcs_filesel },

  { NULL,                        0, 0, 0, NULL }
};

static struct gsubr_t gschem_buffer_funcs [] = {

  { "buffer-copy1",              0, 0, 0, buffer_copy1 },
  { "buffer-copy2",              0, 0, 0, buffer_copy2 },
  { "buffer-copy3",              0, 0, 0, buffer_copy3 },
  { "buffer-copy4",              0, 0, 0, buffer_copy4 },
  { "buffer-copy5",              0, 0, 0, buffer_copy5 },

  { "buffer-cut1",               0, 0, 0, buffer_cut1 },
  { "buffer-cut2",               0, 0, 0, buffer_cut2 },
  { "buffer-cut3",               0, 0, 0, buffer_cut3 },
  { "buffer-cut4",               0, 0, 0, buffer_cut4 },
  { "buffer-cut5",               0, 0, 0, buffer_cut5 },

  { "buffer-paste1",             0, 0, 0, buffer_paste1 },
  { "buffer-paste2",             0, 0, 0, buffer_paste2 },
  { "buffer-paste3",             0, 0, 0, buffer_paste3 },
  { "buffer-paste4",             0, 0, 0, buffer_paste4 },
  { "buffer-paste5",             0, 0, 0, buffer_paste5 },

  { "buffer-copy1-menu",         0, 0, 0, buffer_copy1_menu },
  { "buffer-copy2-menu",         0, 0, 0, buffer_copy2_menu },
  { "buffer-copy3-menu",         0, 0, 0, buffer_copy3_menu },
  { "buffer-copy4-menu",         0, 0, 0, buffer_copy4_menu },
  { "buffer-copy5-menu",         0, 0, 0, buffer_copy5_menu },

  { "buffer-cut1-menu",          0, 0, 0, buffer_cut1_menu },
  { "buffer-cut2-menu",          0, 0, 0, buffer_cut2_menu },
  { "buffer-cut3-menu",          0, 0, 0, buffer_cut3_menu },
  { "buffer-cut4-menu",          0, 0, 0, buffer_cut4_menu },
  { "buffer-cut5-menu",          0, 0, 0, buffer_cut5_menu },

  { "buffer-paste1-menu",        0, 0, 0, buffer_paste1_menu },
  { "buffer-paste2-menu",        0, 0, 0, buffer_paste2_menu },
  { "buffer-paste3-menu",        0, 0, 0, buffer_paste3_menu },
  { "buffer-paste4-menu",        0, 0, 0, buffer_paste4_menu },
  { "buffer-paste5-menu",        0, 0, 0, buffer_paste5_menu },

  { NULL,                        0, 0, 0, NULL }
};

static struct gsubr_t gschem_hk_funcs[] = {
  /* Hotkey Key Mapping callbacks */
  { "view-pan-hotkey",           0, 0, 0, h_keys_view_pan_hotkey },
  { "view-pan-left",             0, 0, 0, h_keys_view_pan_left },
  { "view-pan-right",            0, 0, 0, h_keys_view_pan_right },
  { "view-pan-up",               0, 0, 0, h_keys_view_pan_up },
  { "view-pan-down",             0, 0, 0, h_keys_view_pan_down },

  { "misc-misc",                 0, 0, 0, h_keys_misc },
  { "misc-misc2",                0, 0, 0, h_keys_misc2 },
  { "misc-misc3",                0, 0, 0, h_keys_misc3 },
  { "cancel",                    0, 0, 0, h_keys_cancel },
  { NULL,                        0, 0, 0, NULL }
};

/*! \brief Define a hook.
 * \par Function Description
 * Creates a Guile new hook with \a n_args arguments, and binds it to
 * the variable \a name, returning the newly created hook.
 *
 * \param n_args Number of arguments the hook should take.
 * \param name   Name of variable to bind the hook to.
 *
 * \return the newly-created hook.
 */
static SCM
create_hook (const char *name, int n_args)
{
  SCM hook = scm_make_hook (scm_from_int (n_args));
  scm_c_define (name, hook);
  return scm_permanent_object (hook);
}

/*! \brief Register function with Scheme.
 *  \par Function Description
 *  Creates <B>subr</B> objects to make <B>g_rc_*</B> functions that are
 *  defined *  #g_rc.c, #g_keys.c and #g_funcs.c visible to Scheme.
 */
void g_register_funcs (void)
{
  struct gsubr_t *func[4] = { gschem_rc_funcs, gschem_g_funcs, gschem_hk_funcs, gschem_buffer_funcs};
  struct gsubr_t *grp;
  static GList   *action_list;

  int j;
  for(j = 0; j < 4; j++) {
    grp = func[j];
    while (grp->name != NULL) {
      scm_c_define_gsubr (grp->name, grp->req, grp->opt, grp->rst, grp->fnc);
      grp++;
    }
  }

  i_command_get_action_list(&action_list);

  lambda (char* action) {
    scm_c_define_gsubr (action, 1, 0, 0, g_process_action);
    return FALSE;
  }
  foreach(action_list)

  /* Hook stuff */
  complex_place_list_changed_hook = create_hook ("complex-place-list-changed-hook", 1);
}
