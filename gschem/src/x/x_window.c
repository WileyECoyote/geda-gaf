/* -*- C x_window.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2017 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file x_window.c
 * \brief Main Window Module
 */

#include <errno.h>
#include <libgen.h>        /* dirname */

#include "../../include/gschem.h"
#include "../../include/gschem_macros.h"
#include "../../include/x_menus.h"
#include "../../include/x_window.h"

#include <geda/geda_stat.h>
#include <geda/geda_dialog_controls.h>
#include <geda_file_chooser.h>         /* Need for group and key defines */

extern int iconify_main_window;

/** \defgroup main-window Main Window Module
 *  @{
 *  \brief Contains functions to create and support the Main Window
*/

/*!
 * \brief Setup the Graphical User Interface
 * \par Function Description
 *  This function is the top-level constructor for the GUI, the function
 *  calls various lower level functions to actually create sub-components
 *  of the graphical interface.
 */
void x_window_setup (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  v_log_message(_("Restoring user settings\n"));

  /* immediately setup user params */
  i_vars_set(w_current);

  /* Initialize the autosave callback */
  geda_struct_page_autosave_init(toplevel);

  /* setup world, world_right and world_bottom were set in i_vars.c
   * before this function is called */
  toplevel->width          = w_current->world_right;
  toplevel->height         = w_current->world_bottom;

  w_current->screen_width  = default_window_width;
  w_current->screen_height = default_window_height;

  /* Add to the list of windows */
  global_window_list = g_list_append (global_window_list, w_current);

  /* X related stuff */
  x_icons_initialize();  /* Initialize icons - must be done before menus! */

  x_window_create_main (w_current);

  /* Restores the Main Window size and position */
  x_window_restore_settings (w_current);

  /* Configure grid colors and set toolbar radio state */
  i_window_set_grid_type (w_current);

  /* Update allocation of canvas after restoring window geometry */
  i_window_set_viewport_size (w_current);

#ifdef WITH_LIBGEDADRAW

  x_draw_initialize(w_current);

#endif

  /* Load recent files list before calling x_menu_attach_recent_submenu */
  x_menu_recent_files_load();
  gschem_atexit(x_menu_recent_files_save, NULL);

  x_menu_attach_recent_submenu (w_current);

  /* Initialize the clipboard callback */
  x_clipboard_init (w_current);

#ifdef HAVE_LIBSTROKE
  x_stroke_init ();
#endif

}

/*!
 * \brief Create Graphic Context for Drawing Area
 * \par Function Description
 *  This function trys to create a new Graphic Context associated
 *  with the GdKWindow'ed Drawing Area for later use by low level
 *  drawing routines, but not libgedacairo.
 */
bool x_window_setup_context(GschemToplevel *w_current)
{
  const char *log_msg = _("Could not allocate a graphics context");

  bool result = FALSE;

  if (!w_current) {
    g_critical("%s %s\n", log_msg, _("there is no current window"));
  }
  else {

    if ((w_current->window) && GDK_IS_WINDOW(w_current->window)) {

      w_current->cr  = gdk_cairo_create(w_current->drawing_area->window);

      if (w_current->cr == NULL) {
        g_critical(_("Could not create a Cairo context, is window drawable?\n"));
      }
      else {
        result = TRUE;
      }
    }
    else {
      g_critical("%s %s\n", log_msg, _("window is not valid"));
    }
  }
  return result;
}

/*!
 * \brief Create the Drawing Area
 * \par Function Description
 *  Create and setup the drawing area widget, the widget is added
 *  to the given container \a window and assigned a name base on
 *  the programs process ID.
 *
 * \param [in] w_current The toplevel environment.
 * \param [in] window    scroll_window container
 */
static void
x_window_create_drawing_area (GschemToplevel *w_current, GtkWidget *window)
{
  char *unique_name;

  DrawingArea = GTK_WIDGET (gschem_page_view_new ());

  gtk_widget_set_double_buffered (DrawingArea, FALSE);

  /* Set the size here. Be sure that it has an aspect ratio of 1.333
   * We could calculate this based on root window size, but for now
   * lets just set it to:
   * Width = root_width*3/4 Height = Width/1.3333333333
   * 1.3333333 is the desired aspect ratio!
   */
  gtk_drawing_area_size (GTK_DRAWING_AREA (DrawingArea),
                         w_current->screen_width,
                         w_current->screen_height);

  geda_container_add(window, DrawingArea);
  gtk_widget_set_can_focus(DrawingArea, TRUE);
  gtk_widget_grab_focus (DrawingArea);
  gtk_widget_show (DrawingArea);

  unique_name = geda_sprintf("GschemDrawingArea:%i", prog_pid);
  gtk_widget_set_name(DrawingArea, unique_name);
  GEDA_FREE(unique_name);
}

/*!
 * \brief Save Window Geometry
 * \par Function Description
 *  This functions retrieves the given window size, and position on the
 *  screen and writes the settings to the key file.
 *
 * \param [in] w_current  Gschem toplevel structure.
 *
 * \note: Settings are also saved in other modules, generally, only settings
 *        related to the Window are saved here. We can not save this data in
 *        an at_exit because w_current was destroyed by x_window_close()!
 */
void x_window_save_settings(GschemToplevel *w_current)
{
  EdaConfig  *cfg;
  const char *win_group     = WINDOW_CONFIG_GROUP;
  const char *global_group  = IVAR_CONFIG_GROUP;
  const char *chooser_group = FILE_CHOOSER_CONFIG_GROUP;
  const char *chooser_key   = FILE_CHOOSER_CONFIG_FILTER;

  int array[4];

  geda_log_v(_("Saving main window settings.\n"));

  cfg = eda_config_get_user_context ();

  /* All settings from here down are restored by i_vars_recall_user_settings */

  /* FileChooser filter users preference */
  eda_config_set_integer (cfg, chooser_group, chooser_key, w_current->chooser_filter);

  /* Grid Setup - mark check */
  eda_config_set_integer (cfg, win_group, "grid-mode",           w_current->grid_mode);
  eda_config_set_integer (cfg, win_group, "dots-grid-dot-size",  w_current->dots_grid_dot_size);
  eda_config_set_integer (cfg, win_group, "dots-grid-mode",      w_current->dots_grid_mode);
  eda_config_set_integer (cfg, win_group, "grid-dot-threshold",  w_current->dots_grid_threshold);
  eda_config_set_integer (cfg, win_group, "mesh-grid-threshold",   w_current->mesh_grid_threshold);
  eda_config_set_integer (cfg, win_group, "mesh-line-width-factor",  w_current->mesh_line_width_factor);

  array[0] = w_current->dots_grid_minor_color.pixel;
  array[1] = w_current->dots_grid_minor_color.red;
  array[2] = w_current->dots_grid_minor_color.green;
  array[3] = w_current->dots_grid_minor_color.blue;
  eda_config_set_int_list (cfg, win_group, "dots-grid-minor-color", array, 4);

  array[0] = w_current->dots_grid_major_color.pixel;
  array[1] = w_current->dots_grid_major_color.red;
  array[2] = w_current->dots_grid_major_color.green;
  array[3] = w_current->dots_grid_major_color.blue;
  eda_config_set_int_list (cfg, win_group, "dots-grid-major-color", array, 4);

  array[0] = w_current->mesh_grid_minor_color.pixel;
  array[1] = w_current->mesh_grid_minor_color.red;
  array[2] = w_current->mesh_grid_minor_color.green;
  array[3] = w_current->mesh_grid_minor_color.blue;
  eda_config_set_int_list (cfg, win_group, "mesh-grid-minor-color", array, 4);

  array[0] = w_current->mesh_grid_major_color.pixel;
  array[1] = w_current->mesh_grid_major_color.red;
  array[2] = w_current->mesh_grid_major_color.green;
  array[3] = w_current->mesh_grid_major_color.blue;
  eda_config_set_int_list (cfg, win_group, "mesh-grid-major-color", array, 4);

  eda_config_set_integer (cfg, win_group, "render-adaptor",  w_current->render_adaptor);
  eda_config_set_integer (cfg, win_group, "action-color",    w_current->action_color);
  eda_config_set_integer (cfg, win_group, "anti-aliasing",   w_current->anti_aliasing);

  /* Grips Settings */
  eda_config_set_boolean (cfg, win_group, "draw-grips",      CairoRenderer->draw_grips);
  eda_config_set_integer (cfg, win_group, "grip-size",       GET_GRIP_SIZE (w_current));

  array[0] = CairoRenderer->grip_stroke_color.pixel;
  array[1] = CairoRenderer->grip_stroke_color.red;
  array[2] = CairoRenderer->grip_stroke_color.green;
  array[3] = CairoRenderer->grip_stroke_color.blue;
  eda_config_set_int_list (cfg, win_group, "grips-stroke-color", array, 4);

  array[0] = CairoRenderer->grip_fill_color.pixel;
  array[1] = CairoRenderer->grip_fill_color.red;
  array[2] = CairoRenderer->grip_fill_color.green;
  array[3] = CairoRenderer->grip_fill_color.blue;
  eda_config_set_int_list (cfg, win_group, "grips-fill-color", array, 4);

  /* Junction Cues and Nets */
  eda_config_set_integer (cfg, win_group, "junction-size", CairoRenderer->junction_size);

  array[0] = CairoRenderer->junction_color.pixel;
  array[1] = CairoRenderer->junction_color.red;
  array[2] = CairoRenderer->junction_color.green;
  array[3] = CairoRenderer->junction_color.blue;
  eda_config_set_int_list (cfg, win_group, "junction-color", array, 4);

  array[0] = CairoRenderer->net_endpoint_color.pixel;
  array[1] = CairoRenderer->net_endpoint_color.red;
  array[2] = CairoRenderer->net_endpoint_color.green;
  array[3] = CairoRenderer->net_endpoint_color.blue;
  eda_config_set_int_list (cfg, win_group, "net-endpoint-color", array, 4);

  /* Misc Drawing Window Related */
  eda_config_set_boolean (cfg, win_group,    "object-clipping", w_current->object_clipping);

  /* Pointer, aka Mouse stuff */
  eda_config_set_integer (cfg, global_group, "cursor-index",    w_current->drawing_pointer);
  eda_config_set_integer (cfg, global_group, "drag-can-move",   w_current->drag_can_move);
  eda_config_set_integer (cfg, global_group, "fast-mousepan",   w_current->fast_mousepan);
  eda_config_set_integer (cfg, global_group, "middle-button",   w_current->middle_button);
  eda_config_set_integer (cfg, global_group, "mousepan-gain",   w_current->mousepan_gain);
  eda_config_set_integer (cfg, global_group, "pointer-hscroll", w_current->pointer_hscroll);
  eda_config_set_integer (cfg, global_group, "scrollpan-steps", w_current->scrollpan_steps);
  eda_config_set_integer (cfg, global_group, "scroll-wheel",    w_current->scroll_wheel);
  eda_config_set_integer (cfg, global_group, "third-button",    w_current->third_button);

  /* Scrolling Settings */
  eda_config_set_integer (cfg, win_group, "scrollbars",         w_current->scrollbars);
  eda_config_set_integer (cfg, win_group, "scrollbar-update",   w_current->scrollbar_update);
  eda_config_set_integer (cfg, win_group, "scrollbars-visible", w_current->scrollbars_visible);
}

/*!
 * \brief Zoom Extents in an Idle Thread
 * \par Function Description
 *  This function is called once, after initialization is complete, and
 *  hopefully, after Gtk has finally restored the geometry of the main
 *  window.
 *
 * \param [in] toplevel  The #GschemToplevel to restore geometry.
 */
static bool x_window_idle_thread_zoom_extents (void *toplevel)
{
  GschemToplevel *w_current = (GschemToplevel*)toplevel;

  i_zoom_world_extents (w_current, NULL, 0);

  return FALSE;
}

/*!
 * \brief Re-restore Window Geometry and Position
 * \par Function Description
 *  Do to a flaw in the design of Gtk, see documentation gschem_main_window_update,
 *  Gtk ignores request to set the size and position of GtkWindows and subsequently
 *  set the size based on erroneous calculations, including text on tool bar, with
 *  out having updated whether the text is shown or the font that will actually be
 *  used if the text is shown. This routine is called in an idle thread in order to
 *  restore the main window's last size and position even though this has already
 *  been requested but Gtk failed to complete the task.
 *
 * \param [in] toplevel  The #GschemToplevel to restore geometry.
 */
static bool x_window_idle_thread_restore_geometry (void *toplevel)
{
  GschemToplevel *w_current = (GschemToplevel*)toplevel;

  if (!auto_place_mode) {
    g_signal_emit_by_name(w_current->main_window, "geometry-restore", w_current->window);
    g_signal_emit_by_name(w_current->main_window, "restore-position", w_current->window);
  }

  g_idle_add (x_window_idle_thread_zoom_extents, w_current);

  return FALSE;
}

/*!
 * \brief Restore Window Geometry and Cursor
 * \par Function Description
 *  This functions retrieves the given window size and position from the
 *  key file and sets the given window to the retrived values.
 *
 * \param [in] w_current  Gschem toplevel object.
 */
void x_window_restore_settings(GschemToplevel *w_current)
{
  geda_log_v(_("Retrieving main Window settings.\n"));

  if (!auto_place_mode) {
    gschem_main_window_update(MainWidget);
  }

  /* Restore Cursor/Pointer setting */
  int pointer_id = x_settings_lookup_cursor(w_current->drawing_pointer);
  i_window_set_cursor(w_current, pointer_id);

  if (!iconify_main_window) {
    gtk_window_deiconify (MainWindow);
  }

  g_idle_add (x_window_idle_thread_restore_geometry, w_current);
}

/*!
 * \brief Macro Widget Invoke Macro Response Callback
 * \par Function Description
 *  Called to handle the response emitted from the Macro Widget, if the
 *  response is GEDA_RESPONSE_OK the macro string is evaluated.
 */
static void
x_window_invoke_macro (GtkWidget *widget, int response, GschemToplevel *w_current)
{
  if (response == GEDA_RESPONSE_OK) {

    const char *macro = gschem_macro_widget_get_macro_string (widget);

    SCM interpreter = scm_list_2(scm_from_utf8_symbol("invoke-macro"),
                                 scm_from_utf8_string(macro));

    scm_dynwind_begin (0);
    g_dynwind_window (w_current);
    g_evaluate_scm_protected(interpreter, SCM_UNDEFINED);
    scm_dynwind_end ();
  }

  gtk_widget_hide (GTK_WIDGET (widget));
  gtk_widget_grab_focus (DrawingArea);
}

/*!
 * \brief Create Main Window
 * \par Function Description
 *  This function is called from x_window_setup to create the
 *  Main window and it's contents. This function creates mostly
 *  high-level containers, calling auxiliary functions to populate
 *  the containers with the menus and toolbars.
 */
void x_window_create_main(GschemToplevel *w_current)
{
  GtkPolicyType policy;

  GtkWidget *center_hbox;
  GtkWidget *center_vbox;
  GtkWidget *draw_window;

  GtkWidget *main_box;
  GtkWidget *menubar;

  /* used to signify that the window is not mapped yet */
  w_current->window       = NULL;

  w_current->main_window = gschem_main_window_new (!auto_place_mode);

  /* We want the widgets to flow around the drawing area, so we do not
   * set a size of the main window.  The drawing area's size is fixed,
   * see below. Normally we let the window manager handle locating and
   * sizing the window.  However, for some batch processing of schematics
   * (generating a pdf of all schematics for example) we want to
   * override this.  Hence "auto_place_mode".
   */
  if (auto_place_mode) {
    gtk_widget_set_uposition (MainWidget, 10, 10);
  }

  /* delete_event is sent of close with the X in the window */
  g_signal_connect_swapped (MainWindow, "delete_event",
                            G_CALLBACK (i_event_close_wm),
                            w_current);

  /* Containers first */
  main_box = gtk_vbox_new(FALSE, 1);
  geda_set_container_border_width (main_box, 0);
  geda_container_add(MainWindow, main_box);
  gtk_widget_show (main_box);

  /* Main Menu */
  if (GEDA_IS_MENU_BAR(menubar = x_menu_setup_ui (w_current))) {

    if (w_current->handleboxes) {

      GtkWidget *handlebox;

      handlebox = gtk_handle_box_new ();
      PACK_START(main_box, handlebox, FALSE, FALSE, 0);
      geda_container_add (handlebox, menubar);
      gtk_widget_show (handlebox);
    }
    else {
      geda_container_add(main_box, menubar);
    }
    gtk_widget_show (menubar);
  }

  x_menu_set_togglable(w_current, RESET_TOGGLERS, 0);

  gtk_widget_realize (MainWidget);

  /* End Main Menu */

  if (w_current->toolbars) {
     x_toolbars_init_window(w_current);
     x_toolbars_init_top(w_current, main_box);
  }

  /* --------------------------------------------------------- */
  /*  Setup right mouse popup context menu */
  x_menu_setup_popup(w_current);

  center_hbox = gtk_hbox_new(FALSE, 1);
  geda_set_container_border_width (center_hbox, 0);
  geda_container_add(main_box, center_hbox);
  gtk_widget_show (center_hbox);

  if (w_current->toolbars) {
     x_toolbars_init_left(w_current, center_hbox);
  }

  center_vbox = gtk_vbox_new(FALSE, 1);
  geda_set_container_border_width (center_vbox, 0);
  geda_container_add(center_hbox, center_vbox);
  gtk_widget_show (center_vbox);

  /*! Setup the Scroll bars
   * The scroll-bars are constructed here even if w_current->scrollbars
   * is not enabled. The visibility is intentionaly not set here. This
   * is set near the end of this procedure.
   */
  {
    GtkScrolledWindow *scroll_window;
    GtkAdjustment     *h_adjustment;
    GtkAdjustment     *v_adjustment;

    h_adjustment = geda_adjustment_new (0.0,
                                        w_current->world_left,
                                        w_current->world_right,
                                        100.0, 100.0, 10.0);

    v_adjustment = geda_adjustment_new (w_current->world_bottom,
                                        0.0,
                                        w_current->world_bottom - w_current->world_top,
                                        100.0, 100.0, 10.0);

    draw_window = gtk_scrolled_window_new (h_adjustment, v_adjustment);

    geda_set_container_border_width (draw_window, 0);
    geda_container_add(center_vbox, draw_window);
    gtk_widget_show (draw_window);

    scroll_window    = GTK_SCROLLED_WINDOW (draw_window);
    HorizontalScroll = gtk_scrolled_window_get_hscrollbar(scroll_window);
    VerticalScroll   = gtk_scrolled_window_get_vscrollbar(scroll_window);

    policy = (w_current->scrollbars) ? GTK_POLICY_ALWAYS : GTK_POLICY_NEVER;

    gtk_scrolled_window_set_policy (scroll_window, policy, policy);

    gtk_range_set_update_policy (GTK_RANGE (HorizontalScroll), GTK_UPDATE_CONTINUOUS);
    gtk_range_set_update_policy (GTK_RANGE (VerticalScroll), GTK_UPDATE_CONTINUOUS);

    g_signal_connect (h_adjustment, "value_changed",
                      G_CALLBACK (x_event_hschanged),
                      w_current);

    g_signal_connect (v_adjustment, "value_changed",
                      G_CALLBACK (x_event_vschanged),
                      w_current);
  }

  x_window_create_drawing_area(w_current, draw_window);

  i_event_setup_handlers (w_current);

  /* ----------------- Bottom Toolbar ------------------ */

  if (w_current->toolbars) {
     x_toolbars_init_bottom(w_current, main_box);
  }

  /* -------------------- macro box -------------------- */

  w_current->macro_widget = gschem_macro_widget_new();

  PACK_START (main_box, w_current->macro_widget, FALSE, FALSE, 0);

  g_signal_connect (w_current->macro_widget,
                    "response",
                    G_CALLBACK (&x_window_invoke_macro),
                    w_current);

 /* -------------------- status-bar -------------------- */

  w_current->status_bar = x_status_bar_create(w_current);

  PACK_START (main_box, w_current->status_bar, FALSE, FALSE, 0);
  gtk_widget_show (w_current->status_bar);

  /* Iconize the main window until after the size and position have been
   * restored, otherwise the main window visibly changes sizes and this
   * does not look so good */
  gtk_window_iconify ((GtkWindow*)MainWindow);

  gtk_widget_show(MainWidget);

  /*! The preceeding "show" used to be "show_all", which revealed Everything,
   *  including somethings we did't want to show. These had to be turned-off
   *  in the code below. This is not the case now, but we keep these here as
   *  a reminder of the painful agony "show_all" caused us, and of how hard
   *  it was to hide all those little "red" x's in the handle boxes, just to
   *  discover this "show_all". And maybe somebody will get bored and devise
   *  a geda_show_widget_list!
   */
  /* gtk_widget_hide(w_current->macro_box);*/

  if (w_current->toolbars) {
    /* Hide the little red x's in the toolbars */
    x_toolbars_finalize(w_current);
  }

  /*! Set visibility of the scroll-bars based on user settings */
  x_window_set_scroll_visibility(w_current);

  /* Save a pointer to the canvas GdkWindow */
  w_current->window = DrawingArea->window;

  x_window_setup_context(w_current);

}

/*!
 * \brief Close All Edit Dialogs
 * \par Function Description
 *  This function close any currently open editing dialog boxes
 *  This includes the edit preference dialog.
 */
void x_window_close_edit_dialogs(GschemToplevel *w_current)
{
  /* close all the dialog boxes */

  if (w_current->sowindow)
    gtk_widget_destroy(w_current->sowindow);

  if (w_current->cswindow)
    gtk_widget_destroy(w_current->cswindow);

  if (w_current->tiwindow)
    gtk_widget_destroy(w_current->tiwindow);

  if (w_current->tewindow)
    gtk_widget_destroy(w_current->tewindow);

  if (w_current->aawindow)
    gtk_widget_destroy(w_current->aawindow);

  x_multiattrib_close (w_current);

  if (w_current->aewindow)
    gtk_widget_destroy(w_current->aewindow);

  if (w_current->trwindow)
    gtk_widget_destroy(w_current->trwindow);

  if (w_current->tswindow)
    gtk_widget_destroy(w_current->tswindow);

  if (w_current->clwindow)
    gtk_widget_destroy(w_current->clwindow);

  if (w_current->sewindow)
    gtk_widget_destroy(w_current->sewindow);
}

/*!
 * \brief Close all open Dialogs
 * \par Function Description
 *  This function closes all currently open dialog windows.
 *  This called in preperation for program shutdown.
 */
void x_window_close_all_dialogs(GschemToplevel *w_current)
{
  x_window_close_edit_dialogs(w_current);

  x_pagesel_close (w_current);

  if (w_current->hkwindow) /* Help/Hotkeys */
    gtk_widget_destroy(w_current->hkwindow);

  if (w_current->cowindow) /* Coordinate Dialog */
    gtk_widget_destroy(w_current->cowindow);

  x_console_close();
}

/*!
 * \brief Close A Window
 * \par Function Description
 *  Close the current window, in effect the GUI.
 */
void x_window_close(GschemToplevel *w_current)
{
  bool last_window = FALSE;

  /* If we're closing whilst inside an action, re-wind the
   * page contents back to their state before we started */
  if (w_current->inside_action) {
    geda_log_v(_("Aborting action\n"));
    i_callback_cancel (w_current, 0, NULL);
  }

  /* last chance to save possible unsaved pages */
  if (!x_confirm_close_window (w_current)) {
    geda_log_v(_("Close Window canceled\n"));
    /* user canceled the close */
    return;
  }

  x_clipboard_finish (w_current);

  /* stuff that has to be done before we free w_current */
  if (g_list_length (global_window_list) == 1) {

    /* no more windows after this one, remember to quit */
    last_window = TRUE;
    if(w_current->save_ui_settings == TRUE) {
      if (w_current->toolbars) {
        x_toolbars_save_state(w_current);
      }
      x_menu_save_state(w_current);
      x_window_close_all_dialogs(w_current);
      x_window_save_settings(w_current);
      x_sessions_save_settings(w_current);
      x_settings_save_settings(w_current);
    }

    if (geda_utility_log_get_log_time()) {
      geda_log(_("Shutdown schematic editor\n"));
    }

    /* close the log file */
    geda_utility_log_close ();

    /* free the buffers */
    o_buffer_free (w_current); /* w_current not used */
  }

  if (w_current->toolbars) {
    x_toolbars_free_window(w_current);
  }

  /* Clear Guile smob weak ref */
  if (!scm_is_eq (w_current->smob, SCM_UNDEFINED)) {
    SCM_SET_SMOB_DATA (w_current->smob, NULL);
    w_current->smob = SCM_UNDEFINED;
  }

  g_signal_handlers_disconnect_by_func (w_current->macro_widget,
                                        x_window_invoke_macro,
                                        w_current);

  gtk_widget_set_name(DrawingArea, NULL);

  /* finally close the main window */
  gtk_widget_destroy(MainWidget);

  global_window_list = g_list_remove (global_window_list, w_current);

  gschem_toplevel_free (w_current);

  /* If closed last window, so quit */
  if (last_window) {
    shut_down_gui();
  }
}

/*!
 * \brief Close All Windows
 * \par Function Description
 *  Wrapper for x_window_close function. Currently the option to
 *  open a new window is not enabled by default so normally there
 *  is only one window.
 */
void x_window_close_all(GschemToplevel *w_current)
{
  GList *iter;

  iter = global_window_list;
  while (iter != NULL ) {

   GschemToplevel *current = (GschemToplevel *)iter->data;

    iter = g_list_next (iter);

    x_window_close (current);
  }
}

/* Threaded from x_window_open_page after successful open */
static bool
x_window_idle_thread_post_load_file (void *filename)
{
  geda_log_q ("%s \"%s\"\n", _("Loading file"), filename);
  x_menu_recent_files_add (filename);
  return FALSE;
}

/*!
 * \brief Reset Page Geometry
 * \par Function Description
 *  Sets the current viewport for \a page to the current extent of the
 *  bounds. Helper for:
 *
 *      i_window_idle_zoom_pages
 *      x_window_open_page
 */
void x_window_reset_page_geometry(GschemToplevel *w_current, Page *page)
{
  const GList *list = geda_struct_page_get_objects(page);
  int left, right, top, bottom;

  if (!geda_object_get_bounds_list (list, &left, &top, &right, &bottom)) {
    return;
  }

  page->left   = left;
  page->right  = right;
  page->top    = top;
  page->bottom = bottom;

  i_zoom_world_extents (w_current, list, I_PAN_DONT_REDRAW);
}

/*!
 * \brief Opens a new page from a file.
 * \par Function Description
 *  This function opens the file whose name is <B>filename</B> in a new
 *  Page of <B>toplevel</B>.
 *
 *  If there is no page for <B>filename</B> in <B>toplevel</B>'s list of
 *  pages, it creates a new Page, loads the file in it and returns a pointer
 *  to the new page. Otherwise it returns a pointer to the existing page.
 *
 *  If the filename passed is NULL, this function creates an empty, untitled
 *  page. The name of the untitled page is built from configuration data
 *  by appending a unique counter  "untitled-name".
 *
 *  The opened page becomes the current page of <B>toplevel</B>.
 *
 * \param [in] w_current The toplevel environment.
 * \param [in] filename  The name of the file to open or NULL for a blank page.
 *
 * \returns pointer to the new page.
 *
 * \note Uses glib file utilities when a new string is to be allocated, uses
 *       uses local buffer and glibc when we do not want to deal with freeing.
 *
 */
Page *x_window_open_page(GschemToplevel *w_current, const char *filename)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Page *old_current, *page;
  char  strbuff[MAX_PATH];
  char *ptr;

  g_return_val_if_fail (toplevel != NULL, NULL);

  /* Generate unique untitled filename if none was specified */
  char *generate_untitled(void) {

    char       *str;
    const char *untitled;

    inline void unique_untitled(void) {

      char *tmp;
      char  s_val[3];

      /* Get DIR in buffer */
      ptr = str = getcwd (&strbuff[0], MAX_PATH - 1);

      /* Append a seperator onto the end of DIR */
      while (*ptr != '\0') {
        ++ptr; /* advance to end of string */
      }

       *ptr = DIR_SEPARATOR;     /* add separator */
      ++ptr;                     /* advance over separator */
       *ptr = '\0';              /* Add new NULL */

      /* Append untitled-name */
      str = strcat (str, untitled);

      /* Converted and append an integer to the string */
      tmp = geda_utility_string_int2str (++toplevel->num_untitled, &s_val[0], 10);

      str = strcat (str, tmp);

      /* Append our file extension */
      str = strcat (str, SCHEMATIC_FILE_DOT_SUFFIX);
    }

    EdaConfig  *cfg;
    char       *tmp_str;

    cfg = eda_config_get_user_context();

    tmp_str = i_var_get_global_config_string (cfg, "default-filename");

    /* Get untitled-name prior to looping */
    if (tmp_str != NULL) {
      untitled = tmp_str;
    }
    else if (!toplevel->untitled_name) {
      untitled = _(DEFAULT_UNTITLED_NAME); /* Set to fall-back name */
    }
    else {
      untitled = toplevel->untitled_name;  /* Set to string from config */
    }

    memset(&strbuff[0], '\0', sizeof(strbuff));
    unique_untitled ();
    while (g_file_test (str, G_FILE_TEST_EXISTS)) unique_untitled ();
    GEDA_FREE (tmp_str);
    return str;
  }

  /* Create an empty page with optional filename */
  inline Page *new_page(const char *fname) {

    page = geda_struct_page_new_with_notify (toplevel, fname);

    /* No objects yet, set values to entire world */
    x_window_setup_page(w_current, page, w_current->world_left,
                                         w_current->world_right,
                                         w_current->world_top,
                                         w_current->world_bottom);
    geda_toplevel_set_current_page(toplevel, page);

    return page;
  }

  /* Create an empty page with optional filename */
  inline Page *empty_page(const char *name) {

    char *fname;

    fname = geda_utility_string_strdup (name ? name : generate_untitled());

    new_page(fname);

    /* Hack: There is no page so status bar did not get updated */
    i_status_update_grid_info (w_current);
    geda_log_v ("%s \"%s\"\n", _("New file"), fname);
    GEDA_FREE (fname);
    return page;
  }

  /* Recover by switching back to Old or a create blank */
  inline void resolve_2_recover(const char *name ) {
    /* There was an error, try go back to old page */
    if (old_current != NULL ) {
      geda_struct_page_goto (old_current);
      page = NULL;
    }
    else { /* There was error and no previous page */
      page = empty_page(name);
    }
  }

  if (filename == NULL) {
    page = empty_page(NULL); /* and were done */
  }
  else {

    old_current = toplevel->page_current; /* save fallback point */

    if (g_file_test (filename, G_FILE_TEST_EXISTS)) {

      /* An existing filename was passed, see if already loaded */
      page = geda_struct_page_search (toplevel, filename);

      if (page == NULL) {

        GError *err = NULL;

        /* Do not check for backups is user is doing a command-line operation */
        if (output_filename || iconify_main_window) {
          geda_toplevel_set_file_open_flags(toplevel, F_OPEN_RC | F_OPEN_RESTORE_CWD);
        }

        /* Problem: geda_open_file needs a pointer to a page so we have to create
         * a page struct without knowing the file can be read. If an error
         * occurs then we have to delete this page but geda_struct_page_delete is
         * going to free the name, the one passed to us as a constant, so
         * we have to make a copy here for the maybe future page */
        page = new_page(filename);

        /* Try to load the file */
        if (!geda_open_file (toplevel, page, (char *) filename, &err)) {
          fprintf(stderr, "Error loading file:%s\n", err->message);
          geda_log ("%s \"%s\"\n", _("Failed to load file"), err->message);
          g_error_free (err);
          geda_struct_page_delete (toplevel, page, FALSE); /* FALSE for now */
          resolve_2_recover(NULL);
        }
        else { /* the file was loaded */
          /* Have idle thread assigned to update recent history */
          g_idle_add (x_window_idle_thread_post_load_file, page->filename);
        }
      }
      else { /* File is already open, so make it the current page */
        geda_struct_page_goto (page);
        /* Fall through and return existing page */
      }
    }
    else {  /* File name specified but does not exist, check path */

      char *path;
      int   file_err;

      errno = 0;
      access (filename,  W_OK && F_OK);

      file_err = errno;                     /* save file error */
      path     = strcpy (&strbuff[0], filename);
      path     = dirname(path);             /* geda_get_dirname make copy */

      /* If the path is OK but no file then just create a new file */
      if ((access(path, W_OK && X_OK && F_OK) == 0) && (file_err == ENOENT)) {

        geda_log_q ("%s \"%s\"\n", _("Creating new file"), filename);

        /* Filespec may not exist but user has authority to create */
        page = empty_page(filename);
      }
      else { /* Houston, we have problem */

        /* Filename was specified but path error, so we still
         * don't know if base name is okay. Break down filespec and try
         * to sort out the problem:
         */
        if (errno == ENOENT) { /* 100% sure file_err == ENOENT */

          const char *_Path = _("Path");

          if (geda_create_path (path, S_IRWXU | S_IRWXG) == NO_ERROR ) {
            const char *log_msg = _("did not exist\nsuccessfully created");
            geda_log("%s \"%s\": %s\n",_Path, path, log_msg);
            page = empty_page(filename);
            errno = NO_ERROR;
          }
          else {
            const char *log_msg = _("is not accessible");
            geda_log("%s \"%s\": %s: %s\n", _Path, path, log_msg, strerror (errno));
          }
        }

        if (errno != NO_ERROR) {

          const char *home_dir;

#ifdef OS_LINUX

          home_dir = g_getenv ("HOME");  /* does not allocate */

          if (!home_dir)
            home_dir = g_get_home_dir (); /* does not allocate */
#else
          home_dir = (char*)g_get_home_dir ();
#endif
          path = strcpy(&strbuff[0], home_dir);

          ptr  = (char*)filename;

          while (*ptr != '\0') ++ptr;      /* advance to end of argument */
            while (*ptr != DIR_SEPARATOR) --ptr;  /* backup to separator */
              path = strcat(path, ptr);

          /* set Flag for file-save to use file-saveas */
          w_current->force_save_as = TRUE;
#if DEBUG
          perror(stderr, "filename:%s\n path:%s\n", path, filename);
#endif
          resolve_2_recover(path);
        }
      }
    }
  }

  if (page) {

    /* Damage notifications should invalidate the object on screen */
    geda_object_notify_change_add (page,
                                   (ChangeNotifyFunc) o_invalidate_object,
                                   (ChangeNotifyFunc) o_invalidate_object, w_current);

    x_window_reset_page_geometry(w_current, page);

    /* This line is generally un-needed, however if some code wants
     * to open a page, yet not bring it to the front, it is needed
     * to add it into the page manager. Otherwise, it will get done
     * in x_window_set_current_page.
     */
    x_pagesel_update (w_current); /* If dialog open, update tree */

    o_undo_savestate (w_current, UNDO_ALL);
  }

  return page;
}

/*!
 * \brief Closes a page.
 * \par Function Description
 *  This function closes the page <B>page</B> of toplevel
 *  <B>toplevel</B>.
 *
 *  If necessary, the current page of <B>toplevel</B> is changed to
 *  the next valid page or to a new untitled page.
 *
 * \param [in] w_current The toplevel environment.
 * \param [in] page      The page to close.
 */
void x_window_close_page (GschemToplevel *w_current, Page *page)
{
  GedaToplevel *toplevel = w_current->toplevel;

  g_return_if_fail (toplevel != NULL);

  if (page != NULL) {

    if (page->pid == -1) {
      BUG_IMSG ("invalid page ID", page->pid);
    }
    else {

      Page  *current_page;
      Page  *new_current;
      bool   deleted_current;

      new_current = NULL;

      /* If we're closing whilst inside an action, re-wind the
       * page contents back to their state before we started */
      if (w_current->inside_action) {
        geda_log_v(_("Aborting action\n"));
        i_callback_cancel (w_current, 0, NULL);
      }

      current_page = geda_toplevel_get_current_page(toplevel);

      gschem_page_history_remove_page(w_current->page_history, current_page);

      if (current_page && page == current_page) {

        int pid = page->hierarchy_up;

        /* select new current page first look up in page hierarchy */
        new_current = geda_struct_page_search_by_page_id (toplevel->pages, pid);

        if (new_current == NULL) {

          /* no up in hierarchy, choice is prev, next, new page */
          GList *iter = geda_toplevel_get_page(toplevel, page);

          if (g_list_previous(iter)) {
            new_current = (Page*)g_list_previous (iter)->data;
          }
          else if (g_list_next(iter)) {
            new_current = (Page*)g_list_next( iter )->data;
          }
          else {
            /* need to add a new untitled page */
            new_current = NULL;
          }
        }
        /* new_current will be the new current page at the end of the function */

        deleted_current = TRUE;
      }
      else {

        if (current_page) {
          deleted_current = FALSE;
        }
        else {
          deleted_current = TRUE;
        }
      }

      if ((geda_strncmpi(geda_get_basename(page->filename), "untitled", 8) != 0) ||
           verbose_mode)
      {
        const char *log_msg1 = _("Discarding page");
        const char *log_msg2 = _("Closing");

        geda_log_q ("%s \"%s\"\n", page->CHANGED ? log_msg1 : log_msg2, page->filename);
      }

      geda_page_freeze_notify(page); /* don't bother with thawing */

      /* remove page from toplevel list of page and free */
      geda_struct_page_delete (toplevel, page, TRUE);

      /* Switch to a different page if we just removed the current */
      if (deleted_current) {

        /* Create a new page if there wasn't another to switch to */
        if (!GEDA_IS_PAGE(new_current)) {
          new_current = x_window_open_page (w_current, NULL);
        }

        /* change to new_current and update display */
        x_window_set_current_page (w_current, new_current);
      }
    }
  }
  else {
    BUG_MSG("page should not be NULL");
  }
}

/*!
 * \brief Saves a page to a file.
 * \par Function Description
 *  This function saves the page <B>page</B> to a file named
 *  <B>filename</B>.
 *
 *  It returns the value returned by function <B>geda_save_file()</B> trying
 *  to save page <B>page</B> to file <B>filename</B> (1 on success, 0
 *  on failure).
 *
 *  <B>page</B> may not be the current page of <B>toplevel</B>. The
 *  current page of <B>toplevel</B> is not affected by this function.
 *
 * \param [in] w_current The toplevel environment.
 * \param [in] page      The page to save.
 * \param [in] filename  The name of the file in which to save page.
 *
 * \returns 1 on success, 0 otherwise.
 */
int x_window_save_page (GschemToplevel *w_current, Page *page, const char *filename)
{
  GedaToplevel *toplevel = w_current->toplevel;
  const char   *log_msg;
  const char   *state_msg;
  int           result;

  GError *err = NULL;

  g_return_val_if_fail (toplevel != NULL, 0);
  g_return_val_if_fail (page     != NULL, 0);
  g_return_val_if_fail (filename != NULL, 0);


  /* and try saving current page to filename */
  result = geda_save_file (toplevel, toplevel->page_current, filename, &err);

  if (result != 1) {

    log_msg    = _("Could NOT save page");

    state_msg  = _("Error while trying to save");

    pango_error_dialog("Failed to save file", err->message);

    g_clear_error (&err);
  }
  else {

    /* successfully saved page to file, update page... */
    /* change page name if necessary and prepare log message */
    if (g_ascii_strcasecmp (page->filename, filename) != 0) {

      geda_page_set_filename(page, filename);

      log_msg = _("Saved as");
    }
    else {
      log_msg = _("Saved");
    }

    state_msg  = _("Saved");

    /* Update recent file list */
    x_menu_recent_files_add(filename);
  }

  /* log status of operation */
  geda_log ("%s \"%s\"\n", log_msg, filename);

  /* update display and page manager */
  x_pagesel_update (w_current);
  i_status_update_title (w_current);
  i_status_set_state_msg  (w_current, SELECT, state_msg);

  return result;
}

/*!
 * \brief Changes the current page.
 * \par Function Description
 *  This function displays the specified page <B>page</B> in the
 *  window attached to <B>toplevel</B>.
 *
 *  It changes the <B>toplevel</B>'s current page to <B>page</B>,
 *  draws it and updates the user interface.
 *
 *  <B>page</B> has to be in the list of Pages attached to <B>toplevel</B>.
 *
 * \param [in] w_current The toplevel environment.
 * \param [in] page      The page to become current page.
 */
void x_window_set_current_page (GschemToplevel *w_current, Page *page)
{
  if (gschem_toplevel_set_current_page (w_current, page)) {
    o_redraw_cleanstates (w_current);
    geda_struct_page_goto (page);
    i_window_on_page_changed(w_current);
    x_hscrollbar_update (w_current);
    x_vscrollbar_update (w_current);
    o_invalidate_all (w_current);
  }
}

/*!
 * \brief Set the Visibility of scrollbars
 * \par Function Description
 *  This function updates visibility of the scroll-bars for the drawing
 *  window based on the current settings. Visibility of the bars is user
 *  configurable and users may prefer to turn off the visibility of the
 *  scroll-bars, particularly on newer Ubuntu machines, doing so also
 *  increases the area of the drawing canvas, which might be desired for
 *  laptops with limited display area. The scroll-bars can be hidden but
 *  remain active so that the mouse wheel remains functional.
 */
void x_window_set_scroll_visibility(GschemToplevel *w_current)
{
  if (w_current->scrollbars == TRUE ) {

    GtkScrolledWindow *scroll_window;

    int visible;
    int policy;

    scroll_window = GTK_SCROLLED_WINDOW (DrawingArea->parent);

    visible = w_current->scrollbars_visible != FALSE;

    policy = visible ? GTK_POLICY_ALWAYS : GTK_POLICY_NEVER;

    gtk_scrolled_window_set_policy (scroll_window, policy, policy);
  }
}

/*!
 * \brief Set the contraints for the current page.
 * \par Function Description
 *  This function will set the current page constraints.
 *
 * \param [in]     w_current  The toplevel environment.
 * \param [in,out] page       The Page object to set constraints on.
 * \param [in]     xmin       The minimum x coordinate for the page.
 * \param [in]     xmax       The maximum x coordinate for the page.
 * \param [in]     ymin       The minimum y coordinate for the page.
 * \param [in]     ymax       The maximum y coordinate for the page.
 */
void x_window_setup_page(GschemToplevel *w_current, Page *page,
                         int xmin, int xmax, int ymin, int ymax)
{
  double f_width, f_height;
  double f_left, f_right;
  double f_top, f_bottom;

  page->left   = xmin;
  page->right  = xmax;
  page->top    = ymin;
  page->bottom = ymax;

  /* now do the constant setups */

  f_left   = xmin;
  f_right  = xmax;
  f_top    = ymin;
  f_bottom = ymax;

  f_width  = w_current->screen_width;
  f_height = w_current->screen_height;

  /* pix_x */
  page->to_screen_x_constant = f_width / (f_right - f_left);

  /* pix_y */
  page->to_screen_y_constant = f_height / (f_bottom - f_top);

  /* mil_x */
  page->to_world_x_constant = (f_right - f_left) / f_width;

  /* mil_y */
  page->to_world_y_constant = (f_bottom - f_top) / f_height;
}

/*!
 * \brief Set filename as gschem window title
 * \par Function Description
 *  Set filename as gschem window title using the Qt HID
 *  format style.
 *
 * \param [in] w_current GschemToplevel structure
 */
void x_window_update_title(GschemToplevel *w_current)
{

  Page *current_page = gschem_toplevel_get_current_page(w_current);

  if (current_page && w_current->main_window) {

    GtkWindow  *window;
    const char *filename;
    const char *curr_title;
    char       *print_string;

    if (current_page->filename) {

      if (w_current->toplevel->show_full_path) {
        filename = current_page->filename;
      }
      else {
        filename = geda_file_get_basename(current_page->filename);
      }

    }
    else {
      filename = "undefined"; /* aka BUG */
    }

    if (geda_page_get_changed(current_page) > 0) {

      if (w_current->session_name != NULL) {
        print_string = geda_sprintf ("*%s: %s - gschem",
                                     w_current->session_name,
                                     filename);
      }
      else {
        print_string = geda_sprintf("*%s - gschem", filename);
      }

    }
    else {

      if (w_current->session_name != NULL) {
        print_string = geda_sprintf("%s: %s - gschem",
                                    w_current->session_name,
                                    filename);
      }
      else {
        print_string = geda_sprintf("%s - gschem", filename);
      }
    }

    window = w_current->main_window;

    curr_title = gtk_window_get_title (window);

    if (curr_title) {

      /* Check if changed because Gtk does not */
      if (strcmp(print_string,curr_title )) {
        gtk_window_set_title(window, print_string);
      }
    }
    else {
      gtk_window_set_title(window, print_string);
    }

    GEDA_FREE(print_string);

  }
}

/* --------------------- Main Window Toolbar Processors -------------------- */
/*!
 * \brief View toggle Add toolbar
 * \par Function Description
 *  This function toggles the visibility of the Add toolbar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_add_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->add_handlebox);
  }
  else {
    gtk_widget_hide(w_current->add_handlebox);
  }
}

/*!
 * \brief View toggle Attribute toolbar
 * \par Function Description
 *  This function toggles the visibility of the Attribute toolbar.
 *  Note: the function actually toggles visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_attribute_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->attribute_handlebox);
  }
  else {
    gtk_widget_hide(w_current->attribute_handlebox);
  }
}

/*!
 * \brief View toggle Grid/Snap toolbar
 * \par Function Description
 *  This function toggles the visibility of the Grid/Snap toolbar.
 *  Note: the function actually toggles visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_gridsnap_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->grid_snap_handlebox);
  }
  else {
    gtk_widget_hide(w_current->grid_snap_handlebox);
  }
}

/*!
 * \brief View toggle Edit toolbar
 * \par Function Description
 *  This function toggles the visibility of the Edit toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_edit_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->edit_handlebox);
  }
  else {
    gtk_widget_hide(w_current->edit_handlebox);
  }
}

/*!
 * \brief View toggle Page toolbar
 * \par Function Description
 *  This function toggles the visibility of the Page toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_page_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->page_handlebox);
  }
  else {
    gtk_widget_hide(w_current->page_handlebox);
  }
}

/*!
 * \brief View toggle standard toolbar
 * \par Function Description
 *  This function toggles the visibility of the Standard toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_standard_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->standard_handlebox);
  }
  else {
    gtk_widget_hide(w_current->standard_handlebox);
  }
}

/*!
 * \brief View toggle selection toolbar
 * \par Function Description
 *  This function toggles the visibility of the Select toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_select_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->select_handlebox);
  }
  else {
    gtk_widget_hide(w_current->select_handlebox);
  }
}

/*!
 * \brief View toggle Symbol toolbar
 * \par Function Description
 *  This function toggles the visibility of the Symbol toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_symbol_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->symbol_handlebox);
  }
  else {
    gtk_widget_hide(w_current->symbol_handlebox);
  }
}

/*!
 * \brief View toggle Modify toolbar
 * \par Function Description
 *  This function toggles the visibility of the Modify toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar.
 */
void x_window_modify_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->modify_handlebox);
  }
  else {
    gtk_widget_hide(w_current->modify_handlebox);
  }
}

/*!
 * \brief View toggle Zoom toolbar
 * \par Function Description
 *  This function toggles the visibility of the Zoom toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar
 */
void x_window_zoom_toolbar_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget))) {
    gtk_widget_show(w_current->zoom_handlebox);
  }
  else {
    gtk_widget_hide(w_current->zoom_handlebox);
  }
}

/*!
 * \brief Toggle visibility of toolbar tooltips
 * \par Function Description
 *  Callback function to toggle the visibility of tooltips on toolbars.
 *  Passes the state of the toggle item to x_toolbars_set_show_tooltips.
 *
 * \param [in] widget     "View/Toolbars/Display tips" menu-item widget
 * \param [in] w_current  Gschem toplevel object.
 */
void x_window_toolbar_tips_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  bool state;

  state = geda_check_menu_item_get_active(GEDA_CHECK_MENU_ITEM(widget));

  if (state != w_current->show_toolbar_tips) {
   x_toolbars_set_show_tooltips (w_current, state);
  }
}

/** @} endgroup main-window */
