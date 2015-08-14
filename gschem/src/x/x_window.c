/* -*- C x_window.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */
/*!
 * \file x_window.c
 * \brief Main Window Module
 */

#include <errno.h>
#include <libgen.h>        /* dirname */

#include <geda_stat.h>

#include "gschem.h"
#include "x_menus.h"
#include "x_window.h"
#include <geda_dialog_controls.h>
#include <geda_file_chooser.h>  /* Need for group and key defines */

/** \defgroup main-window Main Window Module
 *  @{ \par
*/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_setup (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  /* immediately setup user params */
  i_vars_set(w_current);

  /* Initialize the autosave callback */
  s_page_autosave_init(toplevel);

  /* setup world */
  //w_current->world_left = -45;
  //w_current->world_top  = -45;

  /* init_right and _bottom are set before this function is called */
  toplevel->width          = w_current->world_right;
  toplevel->height         = w_current->world_bottom;

  w_current->screen_width  = default_window_width;
  w_current->screen_height = default_window_height;

  /* Add to the list of windows */
  global_window_list = g_list_append (global_window_list, w_current);

  /* X related stuff */
  x_icons_initialize();  /* Initialize icons - must be done before menus! */

  x_window_create_main (w_current);

  x_window_restore_settings (w_current);

  i_window_set_grid_type (w_current);

  x_draw_initialize(w_current);

  x_menu_attach_recent_submenu (w_current);

  /* Initialize the clipboard callback */
  x_clipboard_init (w_current);
}

/*! \brief Create Graphic Context for Drawing Area
 *  \par Function Description
 *   This function trys to create a new Graphic Context associated
 *  with the GdKWindow'ed Drawing Area for later use by low level
 *  drawing routines, but not libgedacairo.
 */
bool x_window_setup_gc(GschemToplevel *w_current)
{
  bool result = FALSE;
  if (!w_current) {
    g_critical(_("Could not allocate gc, w_current is NULL\n"));
  }
  else {

    if ((w_current->window) && GDK_IS_WINDOW(w_current->window)) {

      w_current->gc  = gdk_gc_new(w_current->window);
      //w_current->xgc = GDK_GC_XGC (w_current->gc);
      w_current->cr  = gdk_cairo_create(w_current->drawing_area->window);

      if (w_current->gc == NULL) {
        g_critical(_("Could not allocate gc, is window?\n"));
      }
      else if (w_current->cr == NULL) {
        g_critical(_("Could not create a Cairo context, is window drawable?\n"));
      }
      else {
        result = TRUE;
      }
    }
    else {
      g_critical(_("Could not allocate gc, w_current->window is not a valid Window\n"));
    }
  }
  return result;
}

/*! \brief Free the Graphic Context
 *  \par Function Description
 *  We don't actually free the graphic context, we just
 *  dereference here, which result in it's destruction.
 */
void x_window_free_gc(GschemToplevel *w_current)
{
  gdk_gc_unref(w_current->gc);
}

/*! \brief Create the Drawing Area
 *  \par Function Description
 *  the routine create and setup the drawing area widget, the widget is
 *  added to the given container \a window and assigned a name base on
 *  the programs process ID.
 *
 * \param [in] window    The Main window
 * \param [in] w_current The toplevel environment.
 */
static
void x_window_create_drawing_area (GtkWidget *window, GschemToplevel *w_current)
{
  /* drawing next */
  //DrawingArea = gtk_drawing_area_new ();
  DrawingArea = GTK_WIDGET (gschem_page_view_new ());
  GTK_WIDGET_UNSET_FLAGS (DrawingArea, GTK_DOUBLE_BUFFERED);
  /* Set the size here. Be sure that it has an aspect ratio of 1.333
* We could calculate this based on root window size, but for now
* lets just set it to:
* Width = root_width*3/4 Height = Width/1.3333333333
* 1.3333333 is the desired aspect ratio!
*/
  gtk_drawing_area_size (GTK_DRAWING_AREA (DrawingArea),
                         w_current->screen_width,
                         w_current->screen_height);
  gtk_container_add(GTK_CONTAINER(window), DrawingArea);
  gtk_widget_set_can_focus(DrawingArea, TRUE);
  gtk_widget_grab_focus (DrawingArea);
  char *unique_name = u_string_sprintf("GschemDrawingArea:%i", prog_pid);
  g_object_set (DrawingArea, "visible", TRUE, "name", unique_name, NULL);
  GEDA_FREE(unique_name);

}

/*! \brief Save Window Geometry
 *  \par Function Description
 *  This functions retrieves the given window size, and position on the
 *  screen and writes the settings to the key file.
 *
 *  \param [in] w_current  Gschem toplevel structure.
 *
 *  \note: Settings are also saved in other modules, generally, only
 *  settings related to the Window are saved here. We can not save this
 *  data in an at_exit because w_current was destroyed by x_window_close()!
 */
void x_window_save_settings(GschemToplevel *w_current)
{
  GtkWindow  *window;
  EdaConfig  *cfg;
  const char *win_group     = WINDOW_CONFIG_GROUP;
  const char *global_group  = IVAR_CONFIG_GROUP;
  const char *chooser_group = FILE_CHOOSER_CONFIG_GROUP;
  const char *chooser_key   = FILE_CHOOSER_CONFIG_FILTER;

  int x, y, width, height;
  int array[4];

  v_log_message(_("Saving main window geometry and settings.\n"));

  /* Get the Window Geometry - Restored by x_window_restore_settings */
  window = GTK_WINDOW(MainWindow);
  cfg    = eda_config_get_user_context ();
  gtk_window_get_position (window, &x, &y);
  gtk_window_get_size (window, &width, &height);

  /* Save the Window Geometry data */
  eda_config_set_integer (cfg, win_group, "window-x-position", x);
  eda_config_set_integer (cfg, win_group, "window-y-position", y);
  eda_config_set_integer (cfg, win_group, "window-width",      width );
  eda_config_set_integer (cfg, win_group, "window-height",     height);

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
  eda_config_set_integer (cfg, win_group, "anti-aliasing",   w_current->anti_aliasing);

  /* Grips Settings */
  eda_config_set_boolean (cfg, win_group, "draw-grips",      CairoRenderer->draw_grips);
  eda_config_set_integer (cfg, win_group, "grip-size",       w_current->grip_size);

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

/*! \brief Restore Window Geometry and Cursor
 *  \par Function Description
 *  This functions retrieves the given window size and position from the
 *  key file and sets the given window to the retrived values.
 *
 *  \param [in] w_current  Gschem toplevel object.
 */
void x_window_restore_settings(GschemToplevel *w_current)
{
  GtkWindow *window;
  EdaConfig  *cfg;
  GError     *err        = NULL;
  const char *group_name = WINDOW_CONFIG_GROUP;
  bool        xy_error   = FALSE;

  int x, y, width, height;

  v_log_message(_("Retrieving main Window geometry and settings.\n"));

  window = GTK_WINDOW(MainWindow);
  cfg    = eda_config_get_user_context ();

  x = eda_config_get_integer (cfg, group_name, "window-x-position", &err);
  if (err != NULL) {
    fprintf(stderr, "Error retrieving user configuration: '%s'", err->message);
    g_clear_error (&err);
    xy_error = TRUE;
  }
  y = eda_config_get_integer (cfg, group_name, "window-y-position", &err);
  if (err != NULL) {
    g_clear_error (&err);
    xy_error = TRUE;
  }

  width  = eda_config_get_integer (cfg, group_name, "window-width", &err);
  if (err != NULL) {
    g_clear_error (&err);
    width = DEFAULT_WINDOW_WIDTH;
  }
  height = eda_config_get_integer (cfg, group_name, "window-height", &err);
  if (err != NULL) {
    g_clear_error (&err);
    height = DEFAULT_WINDOW_HEIGHT;
  }

  if (xy_error)
    gtk_window_set_position(window, GTK_WIN_POS_CENTER);
  else
    gtk_window_move (window, x, y);

  /* If, for any reason, we pass a zero value to gtk_window_resize an error
   * will be generated. We double check these as fail safe because the above
   * conditionals only set default values if an error occurred retrieving
   * settings, so...*/
  if (width == 0 ) {
    width = 800;
  }
  if (height == 0) {
    height=600;
  }
  gtk_window_resize (window, width, height);

  /* Restore Cursor/Pointer setting */
  int pointer_id = x_settings_lookup_cursor(w_current->drawing_pointer);
  i_window_set_cursor(w_current, pointer_id);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void x_window_invoke_macro (GtkWidget      *widget, int response,
                                   GschemToplevel *w_current)
{
  if (response == GEDA_RESPONSE_OK) {
    const char *macro = gschem_macro_widget_get_macro_string (widget);

    SCM interpreter = scm_list_2(scm_from_utf8_symbol("invoke-macro"),
                                 scm_from_utf8_string(macro));

    scm_dynwind_begin (0);
    g_dynwind_window (w_current);
    g_scm_eval_protected(interpreter, SCM_UNDEFINED);
    scm_dynwind_end ();
  }

  gtk_widget_hide (GTK_WIDGET (widget));
  gtk_widget_grab_focus (DrawingArea);
}

/*! \brief Create Main Window
 *  \par Function Description
 *  This function is called from x_window_setup to create the
 *  Main window and it's contents. This function creates mostly
 *  high-level containers, calling auxiliary functions to populate
 *  the containers with the menus and toolbars.
 */
void x_window_create_main(GschemToplevel *w_current)
{
  GtkPolicyType policy;

  GtkWidget *center_hbox  = NULL;
  GtkWidget *center_vbox  = NULL;
  GtkWidget *draw_window  = NULL;

  GtkWidget *main_box     = NULL;
  GtkWidget *menubar      = NULL;
  GtkWidget *handlebox    = NULL;

  /* used to signify that the window isn't mapped yet */
  w_current->window       = NULL;

  MainWindow = GTK_WIDGET (gschem_main_window_new ());

  gtk_widget_set_name (MainWindow, "gschem");
  gtk_window_set_resizable (GTK_WINDOW(MainWindow), TRUE);

  /* We want the widgets to flow around the drawing area, so we don't
   * set a size of the main window.  The drawing area's size is fixed,
   * see below. Normally we let the window manager handle locating and
   * sizing the window.  However, for some batch processing of schematics
   * (generating a pdf of all schematics for example) we want to
   * override this.  Hence "auto_place_mode".
   */
  if( auto_place_mode )
    gtk_widget_set_uposition (MainWindow, 10, 10);

  /* this should work fine */
  g_signal_connect (G_OBJECT (MainWindow), "delete_event",
                    G_CALLBACK (i_callback_close_wm),
                    w_current);

  /* Containers first */
  main_box = gtk_vbox_new(FALSE, 1);
  gtk_container_border_width(GTK_CONTAINER(main_box), 0);
  gtk_container_add(GTK_CONTAINER(MainWindow), main_box);
  g_object_set (main_box, "visible", TRUE, NULL);

  /* Main Menu */
  if (GTK_IS_MENU_BAR(menubar = x_menu_setup_ui (w_current))) {

    if (w_current->handleboxes) {
      handlebox = gtk_handle_box_new ();
      gtk_box_pack_start(GTK_BOX(main_box), handlebox, FALSE, FALSE, 0);
      gtk_container_add (GTK_CONTAINER (handlebox), menubar);
      g_object_set (handlebox, "visible", TRUE, NULL);
    }
    else {
      gtk_container_add(GTK_CONTAINER(main_box), menubar);
    }
    g_object_set (menubar, "visible", TRUE, NULL);
  }

  x_menu_set_togglable(w_current, RESET_TOGGLERS, 0);

  gtk_widget_realize (MainWindow);
  /* End Main Menu */

  if (w_current->handleboxes && w_current->toolbars) {
     x_toolbars_init_window(w_current);
     x_toolbars_init_top(w_current, main_box);
  }

  /* --------------------------------------------------------- */
  /*  Try to create popup menu (appears in right mouse button  */
  x_menu_setup_popup(w_current);

  center_hbox = gtk_hbox_new(FALSE, 1);
  gtk_container_border_width(GTK_CONTAINER(center_hbox), 0);
  gtk_container_add(GTK_CONTAINER(main_box), center_hbox);
  g_object_set (center_hbox, "visible", TRUE, NULL);

  if (w_current->handleboxes && w_current->toolbars) {
     x_toolbars_init_left(w_current, center_hbox);
  }

  center_vbox = gtk_vbox_new(FALSE, 1);
  gtk_container_border_width(GTK_CONTAINER(center_vbox), 0);
  gtk_container_add(GTK_CONTAINER(center_hbox), center_vbox);
  g_object_set (center_vbox, "visible", TRUE, NULL);

  /*! Setup the Scroll bars
   * The scroll-bars are constructed here if w_current->scrollbars is
   * enabled. The visibility is intentionaly not set here. This is set
   * near the end of this procedure.
   */
  {
    GtkScrolledWindow *scroll_window;
    GtkAdjustment *h_adjustment;
    GtkAdjustment *v_adjustment;

    h_adjustment = GTK_ADJUSTMENT( gtk_adjustment_new (0.0, 0.0,
                                                       w_current->world_right,
                                                       100.0, 100.0, 10.0));

    v_adjustment = GTK_ADJUSTMENT( gtk_adjustment_new (w_current->world_bottom,
                                                       0.0,
                                                       w_current->world_bottom,
                                                       100.0, 100.0, 10.0));

    draw_window = gtk_scrolled_window_new (h_adjustment, v_adjustment);

    gtk_container_border_width(GTK_CONTAINER(draw_window), 0);
    g_object_set (draw_window, "visible", TRUE, NULL);
    gtk_container_add(GTK_CONTAINER(center_vbox), draw_window);

    scroll_window = GTK_SCROLLED_WINDOW (draw_window);
    HorizontalScroll = gtk_scrolled_window_get_hscrollbar(scroll_window);
    VerticalScroll = gtk_scrolled_window_get_vscrollbar(scroll_window);

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

  x_window_create_drawing_area(draw_window, w_current);

  i_event_setup_handlers (w_current);

  /* ----------------- Bottom Toolbar ------------------ */

  if (w_current->handleboxes && w_current->toolbars) {
     x_toolbars_init_bottom(w_current, main_box);
  }

  /* -------------------- macro box -------------------- */

  w_current->macro_widget = GTK_WIDGET (g_object_new (GSCHEM_TYPE_MACRO_WIDGET, NULL));

  gtk_box_pack_start (GTK_BOX (main_box),
                      w_current->macro_widget,
                      FALSE,
                      FALSE,
                      0);

  g_signal_connect (w_current->macro_widget,
                    "response",
                    G_CALLBACK (&x_window_invoke_macro),
                    w_current);

 /* -------------------- status-bar -------------------- */

  w_current->status_bar = x_status_bar_create(w_current);

  gtk_box_pack_start (GTK_BOX (main_box), w_current->status_bar, FALSE, FALSE, 0);
  g_object_set (w_current->status_bar, "visible", TRUE, NULL);

  gtk_widget_show(MainWindow);

  /*! The preceeding "show" used to be "show_all", which revealed Everything,
   *  including somethings we did't want to show. These had to be turned-off
   *  in the code below. This is not the case now, but we keep these here as
   *  a reminder of the painful agony "show_all" caused us, and of how hard
   *  it was to hide all those little "red" x's in the handle boxes, just to
   *  discover this "show_all". And maybe somebody will get bored and devise
   *  a geda_show_widget_list!
   */
  /* gtk_widget_hide(w_current->macro_box);*/

  /* Hide the little red x's in the toolbars */
  x_toolbars_finialize(w_current);

  /*! Set visibility of the scroll-bars based on user settings, noting that
   *  the bars could be enabled so that the mouse wheel work but visibility
   *  turned off, presumably so the user can maximize the drawing area.
   */
  if (w_current->scrollbars == TRUE ) {
    g_object_set (VerticalScroll, "visible",
                 (w_current->scrollbars_visible != FALSE), NULL);
    g_object_set (HorizontalScroll, "visible",
                 (w_current->scrollbars_visible != FALSE), NULL);

  }

  /* Not sure why we need two pointer to GdkWindow */
  w_current->window = DrawingArea->window;
  w_current->drawable = w_current->window;
  x_window_setup_gc(w_current);
}

/*! \brief Close All Edit Dialogs
 *  \par Function Description
 *   This function close any currently open editing dialog boxes
 *   This includes the edit preference dialog.
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

/*! \brief Close all open Dialogs
 *  \par Function Description
 *   This function closes all currently open dialog windows.
 *   This called in preperation for program shutdown.
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_close(GschemToplevel *w_current)
{
  bool last_window = FALSE;

  /* If we're closing whilst inside an action, re-wind the
   * page contents back to their state before we started */
  if (w_current->inside_action) {
    v_log_message("Aborting action\n");
    i_callback_cancel (w_current, 0, NULL);
  }

  /* last chance to save possible unsaved pages */
  if (!x_confirm_close_window (w_current)) {
    v_log_message("Close Window canceled\n");
    /* user cancelled the close */
    return;
  }
  x_clipboard_finish (w_current);

  /* stuff that has to be done before we free w_current */
  if (g_list_length (global_window_list) == 1) {
    /* no more windows after this one, remember to quit */
    last_window = TRUE;
    if(w_current->save_ui_settings == TRUE) {
      x_toolbars_save_state(w_current);
      x_menu_save_state(w_current);
      x_window_close_all_dialogs(w_current);
      x_window_save_settings(w_current);
      x_sessions_save_settings(w_current);
      x_settings_save_settings(w_current);
    }

    /* close the log file */
    u_log_close ();

    /* free the buffers */
    o_buffer_free (w_current); /* w_current not used */
  }

  x_toolbars_free_window(w_current);

  x_window_free_gc(w_current);

  /* Clear Guile smob weak ref */
  if (w_current->smob != SCM_UNDEFINED) {
    SCM_SET_SMOB_DATA (w_current->smob, NULL);
    w_current->smob = SCM_UNDEFINED;
  }

  /* finally close the main window */
  gtk_widget_destroy(MainWindow);

  global_window_list = g_list_remove (global_window_list, w_current);

  gschem_toplevel_free (w_current);

  /* If closed last window, so quit */
  if (last_window) {
    shut_down_gui();
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_close_all(GschemToplevel *w_current)
{
  GschemToplevel *current;
  GList *list_copy, *iter;

  iter = list_copy = g_list_copy (global_window_list);
  while (iter != NULL ) {
    current = (GschemToplevel *)iter->data;
    iter = g_list_next (iter);
    x_window_close (current);
  }
  g_list_free (list_copy);

}

/* Threaded from x_window_open_page after successful open */
static bool x_window_idle_thread_post_load_file (void *filename)
{
  q_log_message (_("Loading \"%s\"\n"), filename);
  x_menu_recent_files_add (filename);
  return FALSE;
}

/*! \brief Opens a new page from a file.
 *  \par Function Description
 *  This function opens the file whose name is <B>filename</B> in a
 *  new Page of <B>toplevel</B>.
 *
 *  If there is no page for <B>filename</B> in <B>toplevel</B>'s list
 *  of pages, it creates a new Page, loads the file in it and returns
 *  a pointer on the new page. Otherwise it returns a pointer on the
 *  existing page.
 *
 *  If the filename passed is NULL, this function creates an empty,
 *  untitled page.  The name of the untitled page is build from
 *  configuration data ('untitled-name') and a counter for uniqueness.
 *
 *  The opened page becomes the current page of <B>toplevel</B>.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] filename The name of the file to open or NULL for a blank page.
 *  \returns A pointer on the new page.
 *
 *  \note When we want a new string allocated we use the glib file utilities,
 *  When we don't want to deal with freeing we our local buffer and glibc.
 *
 */
Page* x_window_open_page (GschemToplevel *w_current, const char *filename)
{
  GedaToplevel *toplevel = w_current->toplevel;

  Page *old_current, *page;
  char  untitled[] = "untitled";
  char  strbuff[MAX_PATH];
  char *path;
  char *ptr;
  int   file_err;

  g_return_val_if_fail (toplevel != NULL, NULL);

  /* Generate unique untitled filename if none was specified */
  char *generate_untitled() {
    char  s_val[3];
    char *tmp;
    char *str;

    inline void unique_untitled () {
      /* Get DIR in buffer */
      ptr = str = getcwd  ( &strbuff[0], MAX_PATH - 1 );
      /* Append a seperator onto the end of DIR */
      while ( *ptr != '\0') ++ptr; /* advance to end of string */
        *ptr = DIR_SEPARATOR;     /* add separator */
        ++ptr;                       /* advance over separator */
        *ptr = '\0';                /* Add new NULL */

        /* Append default name from config */
        if (toplevel->untitled_name) {
          str = strcat  ( str, toplevel->untitled_name );
        }
        else {
          str = &untitled[0];
        }

        /* Converted and append an integer to the string */
        tmp = u_string_int2str ( ++toplevel->num_untitled, &s_val[0], 10 );
        str = strcat  ( str, tmp );

        /* Append our file extension */
        str = strcat  ( str, SCHEMATIC_FILE_DOT_SUFFIX );
    }

    memset(&strbuff[0], '\0', sizeof(strbuff));
    unique_untitled ();
    while ( g_file_test (str, G_FILE_TEST_EXISTS)) unique_untitled ();
    return str;
  }

  /* Create an empty page with optional filename */
  inline Page* new_page( const char *fname ) {
    page = s_page_new_with_notify (toplevel, fname);
    x_window_setup_page(w_current, page, w_current->world_left,
                        w_current->world_right,
                        w_current->world_top,
                        w_current->world_bottom);
    s_page_goto (toplevel, page);
    return page;
  }

  /* Create an empty page with optional filename */
  inline Page* empty_page( const char *name ) {
    char     *fname;
    fname = u_string_strdup ( name ? name : generate_untitled() );
    new_page(fname);
    /* Hack: There is no page so status bar did not get updated */
    i_status_update_grid_info (w_current);
    v_log_message (_("New file [%s]\n"), fname);
    GEDA_FREE (fname);
    return page;
  }

  /* Recover by switching back to Old or a create blank */
  inline void resolve_2_recover( const char *name ) {
    /* There was an error, try go back to old page */
    if ( old_current != NULL ) {
      s_page_goto (toplevel, old_current);
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

    if ( g_file_test (filename, G_FILE_TEST_EXISTS)) {

      /* An existing filename was passed, see if already loaded */
      page = s_page_search (toplevel, filename);

      if ( page == NULL ) {

        GError *err = NULL;
        /* Problem: f_open needs a pointer to a page so we have to create
         * a page struct without knowing the file can be read. If an error
         * occurs then we have to delete this page but s_page_delete is
         * going to free the name, the one passed to us as a constant, so
         * we have to make a copy here for the maybe future page */
        page = new_page(filename);
        /* Try to load the file */

        if (!f_open (toplevel, page, (char *) filename, &err)) {
          fprintf(stderr, "Error loading file:%s\n", err->message);
          u_log_message( "Failed to load file:%s\n", err->message);
          g_error_free (err);
          s_page_delete (toplevel, page);
          resolve_2_recover(NULL);
        }
        else { /* the file was loaded */
          g_idle_add (x_window_idle_thread_post_load_file,
                      Current_Page->filename);
        }
      }
      else { /* File is already open, so make it the current page */
        s_page_goto (toplevel, page);
        /* Fall through and return existing page */
      }
    }
    else {  /* File name specified but does not exist, check path */
      errno = 0;
      access (filename,  W_OK && F_OK);
      file_err = errno;                        /* save file error */
      path = strcpy (&strbuff[0], filename);
      path = dirname(path);                    /* f_get_dirname make copy */
      /* If the path is OK but no file then just create a new file */
      if ((access(path, W_OK && X_OK && F_OK) == 0) && (file_err == ENOENT)) {
        q_log_message("Creating new file \"%s\"\n", filename);
        /* Filespec may not exist but user has authority to create */
        page = empty_page(filename);
      }
      else { /* Houston, we have problem */
        /* Filename was specified but path error, so we still
         * don't know if base name is okay. Break down filespec and try
         * to sort out the problem:
         */
        if( errno == ENOENT) { /* 100% sure file_err == ENOENT */
          if( f_path_create (path, S_IRWXU | S_IRWXG) == NO_ERROR ) {
            u_log_message("Path \"%s\": did not exist\n, successfully created\n", path);
            page = empty_page(filename);
            errno = NO_ERROR;
          }
          else {
            u_log_message("Path \"%s\": is not accessible: %s\n", path, strerror (errno));
          }
        }

        if( errno != NO_ERROR) {
          const char   *homedir = g_getenv ("HOME"); /* does not allocate */
          if (!homedir) homedir = g_get_home_dir (); /* does not allocate */
            path = strcpy(&strbuff[0], homedir);
          ptr  = (char*) filename;
          while ( *ptr != '\0') ++ptr;      /* advance to end of argument */
            while ( *ptr != DIR_SEPARATOR) --ptr;  /* backup to separator */
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

  /* Damage notifications should invalidate the object on screen */
  o_notify_change_add (page,
                       (ChangeNotifyFunc) o_invalidate_object,
                       (ChangeNotifyFunc) o_invalidate_object, w_current);

  i_zoom_world_extents (w_current,
                        s_page_get_objects (toplevel->page_current),
                        I_PAN_DONT_REDRAW);

  o_undo_savestate (w_current, UNDO_ALL);

  /* This line is generally un-needed, however if some code
   * wants to open a page, yet not bring it to the front, it is
   * needed to add it into the page manager. Otherwise, it will
   * get done in x_window_set_current_page.
   */
  x_pagesel_update (w_current); /* If dialog open, update tree */
  o_invalidate_all(w_current);

  return page;
}

/*! \brief Changes the current page.
 *  \par Function Description
 *  This function displays the specified page <B>page</B> in the
 *  window attached to <B>toplevel</B>.
 *
 *  It changes the <B>toplevel</B>'s current page to <B>page</B>,
 *  draws it and updates the user interface.
 *
 *  <B>page</B> has to be in the list of Pages attached to <B>toplevel</B>.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to become current page.
 */
void x_window_set_current_page (GschemToplevel *w_current, Page *page)
{
  GedaToplevel *toplevel = w_current->toplevel;

  g_return_if_fail (toplevel != NULL);

  if (page) {

    o_redraw_cleanstates (w_current);

    s_page_goto (toplevel, page);

    i_window_on_page_changed(w_current);

    x_hscrollbar_update (w_current);
    x_vscrollbar_update (w_current);
    o_invalidate_all (w_current);

    v_log_message("Set page <%s> active.", f_get_basename(page->filename));
  }
}

/*! \brief Set the contraints for the current page.
 *  \par Function Description
 *  This function will set the current page constraints.
 *
 *  \param [in]     w_current  The toplevel environment.
 *  \param [in,out] page       The Page object to set constraints on.
 *  \param [in]     xmin       The minimum x coordinate for the page.
 *  \param [in]     xmax       The maximum x coordinate for the page.
 *  \param [in]     ymin       The minimum y coordinate for the page.
 *  \param [in]     ymax       The maximum y coordinate for the page.
 */
void x_window_setup_page(GschemToplevel *w_current, Page *page,
                       int xmin, int xmax, int ymin, int ymax)
{
  double fs,f0,f1;
  double fw0,fw1,fw;

  page->left   = xmin;
  page->right  = xmax;
  page->top    = ymin;
  page->bottom = ymax;

  /* now do the constant setups */

  /* pix_x */
  f0 = page->left;
  f1 = page->right;
  fs = w_current->screen_width;
  page->to_screen_x_constant = fs / (f1 - f0);

  /* pix_y */
  f0 = page->top;
  f1 = page->bottom;
  fs = w_current->screen_height;
  page->to_screen_y_constant = fs / (f1 - f0);

  /* mil_x */
  fw1 = page->right;
  fw0 = page->left;
  fw  = w_current->screen_width;
  page->to_world_x_constant = (fw1 - fw0) / fw;

  /* mil_y */
  fw1 = page->bottom;
  fw0 = page->top;
  fw  = w_current->screen_height;
  page->to_world_y_constant = (fw1 - fw0) / fw;
}

/*! \brief Saves a page to a file.
 *  \par Function Description
 *  This function saves the page <B>page</B> to a file named
 *  <B>filename</B>.
 *
 *  It returns the value returned by function <B>f_save()</B> trying
 *  to save page <B>page</B> to file <B>filename</B> (1 on success, 0
 *  on failure).
 *
 *  <B>page</B> may not be the current page of <B>toplevel</B>. The
 *  current page of <B>toplevel</B> is not affected by this function.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to save.
 *  \param [in] filename  The name of the file in which to save page.
 *  \returns 1 on success, 0 otherwise.
 */
int x_window_save_page (GschemToplevel *w_current, Page *page, const char *filename)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Page         *old_current;
  const char   *log_msg;
  const char   *state_msg;
  int           result;

  GError *err = NULL;

  g_return_val_if_fail (toplevel != NULL, 0);
  g_return_val_if_fail (page     != NULL, 0);
  g_return_val_if_fail (filename != NULL, 0);

  /* save current page for restore after saving */
  old_current = toplevel->page_current;

  /* change to page */
  s_page_goto (toplevel, page);
  /* and try saving current page to filename */
  result = f_save (toplevel, toplevel->page_current, filename, &err);

  if (result != 1) {

    log_msg    = _("Could NOT save page [%s]:\n");

    state_msg  = _("Error while trying to save");

    pango_error_dialog("Failed to save file", err->message);

    g_clear_error (&err);
  }
  else {
    /* successful save of page to file, update page... */
    /* change page name if necessary and prepare log message */
    if (g_ascii_strcasecmp (page->filename, filename) != 0) {
      GEDA_FREE (page->filename);
      page->filename = u_string_strdup (filename);

      log_msg = _("Saved as [%s] Okay\n");
    }
    else {
      log_msg = _("Saved [%s] Okay\n");
    }

    state_msg  = _("Saved");

    /* reset page CHANGED flag */
    page->CHANGED = FALSE;
    /* add to recent file list */
    x_menu_recent_files_add(filename);
  }

  /* log status of operation */
  u_log_message (log_msg, filename);

  /* update display and page manager */
  x_window_set_current_page (w_current, old_current);

  i_status_set_state_msg  (w_current, SELECT, state_msg);

  return result;
}

/*! \brief Closes a page.
 *  \par Function Description
 *  This function closes the page <B>page</B> of toplevel
 *  <B>toplevel</B>.
 *
 *  If necessary, the current page of <B>toplevel</B> is changed to
 *  the next valid page or to a new untitled page.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to close.
 */
void x_window_close_page (GschemToplevel *w_current, Page *page)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Page *new_current = NULL;
  GList *iter;

  g_return_if_fail (toplevel != NULL);

  if (page != NULL) {

    if (page->pid == -1) {
      BUG_IMSG ("invalid page ID=<%d>", page->pid);
    }
    else {

      /* If we're closing whilst inside an action, re-wind the
       * page contents back to their state before we started */
      if (w_current->inside_action) {
        v_log_message("Aborting action\n");
        i_callback_cancel (w_current, 0, NULL);
      }

      if (page == toplevel->page_current) {

        /* select new current page first look up in page hierarchy */
        new_current = s_page_search_by_page_id (toplevel->pages, page->up);

        if (new_current == NULL) {

          /* no up in hierarchy, choice is prev, next, new page */
          iter = g_list_find(geda_list_get_glist(toplevel->pages), page);

          if (g_list_previous(iter)) {
            new_current = (Page*)g_list_previous (iter)->data;
          }
          else if (g_list_next(iter)) {
            new_current = (Page *)g_list_next( iter )->data;
          }
          else {
            /* need to add a new untitled page */
            new_current = NULL;
          }
        }
        /* new_current will be the new current page at the end of the function */
      }

      if ((u_string_strncmpi(f_get_basename(page->filename), "untitled", 8) != 0) ||
        verbose_mode)
      {
        q_log_message (page->CHANGED ? _("Discarding page [%s]\n") : _("Closing [%s]\n"),
        page->filename);
      }

      /* remove page from toplevel list of page and free */
      s_page_delete (toplevel, page);

      /* Switch to a different page if we just removed the current */
      if (toplevel->page_current == NULL) {

        /* Create a new page if there wasn't another to switch to */
        if (new_current == NULL) {
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

/*! \brief Set filename as gschem window title
 *
 *  \par Function Description
 *  Set filename as gschem window title using the Qt HID
 *  format style.
 *
 *  \param [in] w_current GschemToplevel structure
 */
void x_window_update_title(GschemToplevel *w_current)
{
  const char *filename=NULL;
  char       *print_string=NULL;

  if (w_current->main_window) {

    if (w_current->toplevel && Current_Page) {

      if (Current_Page->filename) {

        if (w_current->toplevel->show_full_path) {
          filename = Current_Page->filename;
        }
        else {
          filename = f_get_basename(Current_Page->filename);
        }

      }
      else {
        filename = "undefined"; /* aka BUG */
      }
    }
    else {
      filename = "loading"; /* Should never happen */
    }

    if (Current_Page->CHANGED) {

      if (w_current->session_name != NULL) {
        print_string = u_string_sprintf("*%s: %s - gschem",
        w_current->session_name,
        filename);
      }
      else {
        print_string = u_string_sprintf("*%s - gschem", filename);
      }

    }
    else {

      if (w_current->session_name != NULL) {
        print_string = u_string_sprintf("%s: %s - gschem",
        w_current->session_name,
        filename);
      }
      else {
        print_string = u_string_sprintf("%s - gschem", filename);
      }
    }

    gtk_window_set_title(GTK_WINDOW(w_current->main_window), print_string);

    GEDA_FREE(print_string);

  }
}

/* --------------------- Main Window Toolbar Processors -------------------- */
/*!
 * \brief View toogle Add toolbar
 * \par Function Description
 *      This function toggles the visibility of the Add toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar.
 */
void x_window_add_toolbar_toggle(GtkWidget *widget,
                                      GschemToplevel *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->add_handlebox);
  else
    gtk_widget_hide(w_current->add_handlebox);
}
/*!
 * \brief View toogle Attribute toolbar
 * \par Function Description
 *      This function toggles the visibility of the Attribute toobar.
 * Note: the function actually toggles visibility of the handlebox
 * containing the toolbar.
 */
void x_window_attribute_toolbar_toggle(GtkWidget *widget,
                                       GschemToplevel *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->attribute_handlebox);
  else
    gtk_widget_hide(w_current->attribute_handlebox);
}
/*!
 * \brief View toogle Grid/Snap toolbar
 * \par Function Description
 *      This function toggles the visibility of the Grid/Snap toobar.
 * Note: the function actually toggles visibility of the handlebox
 * containing the toolbar.
 */
void x_window_gridsnap_toolbar_toggle(GtkWidget *widget,
                                       GschemToplevel *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->grid_snap_handlebox);
  else
    gtk_widget_hide(w_current->grid_snap_handlebox);
}
/*!
 * \brief View toogle Edit toolbar
 * \par Function Description
 *      This function toggles the visibility of the Edit toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar.
 */
void x_window_edit_toolbar_toggle(GtkWidget *widget,
                                      GschemToplevel *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->edit_handlebox);
  else
    gtk_widget_hide(w_current->edit_handlebox);
}
/*!
 * \brief View toogle Page toolbar
 * \par Function Description
 *      This function toggles the visibility of the Page toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar.
 */
void x_window_page_toolbar_toggle(GtkWidget *widget,
                                      GschemToplevel *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->page_handlebox);
  else
    gtk_widget_hide(w_current->page_handlebox);
}
/*!
 * \brief View toogle standard toolbar
 * \par Function Description
 *      This function toggles the visibility of the Standard toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar.
 */
void x_window_standard_toolbar_toggle(GtkWidget *widget,
                                      GschemToplevel *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->standard_handlebox);
  else
    gtk_widget_hide(w_current->standard_handlebox);
}

/*!
 * \brief View toogle selection toolbar
 * \par Function Description
 *      This function toggles the visibility of the Select toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar.
 */
void x_window_select_toolbar_toggle(GtkWidget *widget,
                                      GschemToplevel *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->select_handlebox);
  else
    gtk_widget_hide(w_current->select_handlebox);
}
/*!
 * \brief View toogle Zoom toolbar
 * \par Function Description
 *      This function toggles the visibility of the Zoom toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar
 */
void x_window_zoom_toolbar_toggle(GtkWidget *widget,
                                      GschemToplevel *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->zoom_handlebox);
  else
    gtk_widget_hide(w_current->zoom_handlebox);
}

/** @} endgroup main-window */
