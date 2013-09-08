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


#include <glib/gstdio.h>   /* mkdir */
#include <libgen.h>        /* dirname */

#include <gschem.h>
#include <x_menu.h>

#include <stdio.h>
#include <errno.h>

#include <x_window.h>

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_setup (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* immediately setup user params */
  i_vars_set(w_current);

  /* Initialize the autosave callback */
  s_page_autosave_init(toplevel);

  /* x_window_setup_world() - BEGIN */
  toplevel->init_left = -45;
  toplevel->init_top  = -45;
  /* init_right and _bottom are set before this function is called */

  toplevel->width  = default_window_width;
  toplevel->height = default_window_height;

  w_current->win_width  = toplevel->width;
  w_current->win_height = toplevel->height;
  /* x_window_setup_world() - END */

  /* Add to the list of windows */
  global_window_list = g_list_append (global_window_list, w_current);

  /* X related stuff */
  x_icons_setup_factory();          /* Must setup factory before menus! */

  x_window_create_main (w_current);
  x_window_restore_geometry((GtkWindow*)w_current->main_window, "gschem");
  x_menu_attach_recent_files_submenu(w_current);

  /* Initialize the clipboard callback */
  x_clipboard_init (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_setup_gc(GSCHEM_TOPLEVEL *w_current)
{
  w_current->gc = gdk_gc_new(w_current->window);

  if (w_current->gc == NULL) {
    fprintf(stderr, _("Couldn't allocate gc\n"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_free_gc(GSCHEM_TOPLEVEL *w_current)
{
  gdk_gc_unref(w_current->gc);
}

/*! \brief Set up callbacks for window events that affect drawing.
 *  \par Function Description
 * Installs GTK+ callback handlers for signals that are emitted by
 * the drawing area, and some for the main window that affect the drawing
 * area.
 *
 * \param [in] w_current The toplevel environment.
 */
void x_window_create_drawing(GtkWidget *drawbox, GSCHEM_TOPLEVEL *w_current)
{
  /* drawing next */
  w_current->drawing_area = gtk_drawing_area_new ();
  /* Set the size here.  Be sure that it has an aspect ratio of 1.333
   * We could calculate this based on root window size, but for now
   * lets just set it to:
   * Width = root_width*3/4   Height = Width/1.3333333333
   * 1.3333333 is the desired aspect ratio!
   */
  gtk_drawing_area_size (GTK_DRAWING_AREA (w_current->drawing_area),
                         w_current->win_width,
                         w_current->win_height);

  gtk_box_pack_start (GTK_BOX (drawbox), w_current->drawing_area, TRUE, TRUE, 0);
  GTK_WIDGET_SET_FLAGS (w_current->drawing_area, GTK_CAN_FOCUS );
  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_show (w_current->drawing_area);

}

/*! \brief Save Window Geometry
 *  \par Function Description
 *  This functions retrieves the given window size, and position on the
 *  screen and writes the settings to the key file.
 *
 *  \param [in] window     The Window whose size and position is to be saved.
 *  \param [in] group_name The group name in the key file.
 */
void x_window_save_geometry(GtkWindow *window, char* group_name)
{
  EdaConfig  *cfg;
  int x, y, width, height;

  cfg = eda_config_get_user_context ();
  gtk_window_get_position (window, &x, &y);
  gtk_window_get_size (window, &width, &height);

  eda_config_set_integer (cfg, group_name, "window-x-position", x);
  eda_config_set_integer (cfg, group_name, "window-y-position", y);
  eda_config_set_integer (cfg, group_name, "window-width",      width );
  eda_config_set_integer (cfg, group_name, "window-height",     height);

}

/*! \brief Restore Window Geometry
 *  \par Function Description
 *  This functions retrieves the given window size and position from the
 *  key file and sets the given window to the retrived values.
 *
 *  \param [in] window     The Window to restore the size and position.
 *  \param [in] group_name The group name in the key file.
 */
void x_window_restore_geometry(GtkWindow *window, char* group_name)
{

  EdaConfig  *cfg;
  GError     *err      = NULL;
  bool        xy_error = FALSE;

  int x, y, width, height;

  v_log_message("Retrieving Window geometry\n");

  cfg = eda_config_get_user_context ();

  x = eda_config_get_integer (cfg, group_name, "window-x-position", &err);
  if (err != NULL) {
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

  gtk_window_resize (window, width, height);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_setup_draw_events(GSCHEM_TOPLEVEL *w_current)
{
  struct event_reg_t {
    char *detailed_signal;
    GCallback c_handler;
  };

  struct event_reg_t drawing_area_events[] = {
    { "expose_event",         G_CALLBACK(x_event_expose)          },
    { "button_press_event",   G_CALLBACK(x_event_button_pressed)  },
    { "button_release_event", G_CALLBACK(x_event_button_released) },
    { "motion_notify_event",  G_CALLBACK(x_event_motion)          },
    { "configure_event",      G_CALLBACK(x_event_configure)       },
    { "key_press_event",      G_CALLBACK(x_event_key)             },
    { "key_release_event",    G_CALLBACK(x_event_key)             },
    { NULL,                   NULL                                } };
  struct event_reg_t main_window_events[] = {
    { "enter_notify_event",   G_CALLBACK(x_event_enter)           },
    { "scroll_event",         G_CALLBACK(x_event_scroll)          },
    { NULL,                   NULL                                } };
  struct event_reg_t *tmp;

  /* is the configure event type missing here? hack */
  gtk_widget_set_events (w_current->drawing_area,
                         GDK_EXPOSURE_MASK |
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK   |
                         GDK_ENTER_NOTIFY_MASK |
                         GDK_KEY_PRESS_MASK |
                         GDK_BUTTON_RELEASE_MASK);

  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++) {
    g_signal_connect (w_current->drawing_area,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      w_current);
  }

  for (tmp = main_window_events; tmp->detailed_signal != NULL; tmp++) {
    g_signal_connect (w_current->main_window,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void x_window_invoke_macro(GtkEntry *entry, void *userdata)
{
  GSCHEM_TOPLEVEL *w_current = userdata;
  SCM interpreter;

  interpreter = scm_list_2(scm_from_utf8_symbol("invoke-macro"),
                           scm_from_utf8_string(gtk_entry_get_text(entry)));

  scm_dynwind_begin (0);
  g_dynwind_window (w_current);
  g_scm_eval_protected(interpreter, SCM_UNDEFINED);
  scm_dynwind_end ();

  gtk_widget_hide(w_current->macro_box);
  gtk_widget_grab_focus(w_current->drawing_area);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_create_main(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL  *toplevel = w_current->toplevel;
  GtkWidget *label=NULL;
  GtkWidget *center_hbox;
  GtkWidget *center_vbox;
  GtkWidget *drawbox=NULL;
  GtkWidget *bottom_box=NULL;

  GtkWidget *main_box=NULL;
  GtkWidget *menubar=NULL;
  GtkWidget *handlebox=NULL;

  /* used to signify that the window isn't mapped yet */
  w_current->window = NULL;

  w_current->main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_widget_set_name (w_current->main_window, "gschem");
  gtk_window_set_resizable (GTK_WINDOW(w_current->main_window), TRUE);

  /* We want the widgets to flow around the drawing area, so we don't
   * set a size of the main window.  The drawing area's size is fixed,
   * see below. Normally we let the window manager handle locating and
   * sizing the window.  However, for some batch processing of schematics
   * (generating a pdf of all schematics for example) we want to
   * override this.  Hence "auto_place_mode".
   */
  if( auto_place_mode )
    gtk_widget_set_uposition (w_current->main_window, 10, 10);

  /* this should work fine */
  g_signal_connect (G_OBJECT (w_current->main_window), "delete_event",
                    G_CALLBACK (i_callback_close_wm),
                    w_current);

  /* Containers first */
  main_box = gtk_vbox_new(FALSE, 1);
  gtk_container_border_width(GTK_CONTAINER(main_box), 0);
  gtk_container_add(GTK_CONTAINER(w_current->main_window), main_box);

  /* Main Menu */
  menubar = x_menu_setup_ui (w_current);

  if (w_current->handleboxes) {
    handlebox = gtk_handle_box_new ();
    gtk_box_pack_start(GTK_BOX(main_box), handlebox, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER (handlebox), menubar);
  } else {
    gtk_box_pack_start(GTK_BOX(main_box), menubar, FALSE, FALSE, 0);
  }

  x_menu_set_toggle(w_current, RESET_TOGGLERS, 0);

  gtk_widget_realize (w_current->main_window);
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

  if (w_current->handleboxes && w_current->toolbars) {
     x_toolbars_init_left(w_current, center_hbox);
  }

  center_vbox = gtk_vbox_new(FALSE, 1);
  gtk_container_border_width(GTK_CONTAINER(center_vbox), 0);
  gtk_container_add(GTK_CONTAINER(center_hbox), center_vbox);

  drawbox = gtk_hbox_new(FALSE, 0);
  gtk_container_border_width(GTK_CONTAINER(drawbox), 0);

  gtk_container_add(GTK_CONTAINER(center_vbox), drawbox);

  x_window_create_drawing(drawbox, w_current);
  x_window_setup_draw_events(w_current);

  if (w_current->scrollbars == TRUE) {
    /* setup scroll bars */
    w_current->v_adjustment = GTK_ADJUSTMENT (
      gtk_adjustment_new (toplevel->init_bottom, 0.0, toplevel->init_bottom,
                          100.0, 100.0, 10.0));

    w_current->v_scrollbar = gtk_vscrollbar_new (w_current->v_adjustment);

    gtk_range_set_update_policy (GTK_RANGE (w_current->v_scrollbar),
                                 GTK_UPDATE_CONTINUOUS);

    gtk_box_pack_start (GTK_BOX (drawbox), w_current->v_scrollbar,
                        FALSE, FALSE, 0);

    g_signal_connect (w_current->v_adjustment,
                      "value_changed",
                      G_CALLBACK (x_event_vschanged),
                      w_current);

    w_current->h_adjustment = GTK_ADJUSTMENT (
      gtk_adjustment_new (0.0, 0.0, toplevel->init_right, 100.0, 100.0, 10.0));

    w_current->h_scrollbar = gtk_hscrollbar_new (w_current->h_adjustment);

    gtk_range_set_update_policy (GTK_RANGE (w_current->h_scrollbar),
                                 GTK_UPDATE_CONTINUOUS);

    gtk_box_pack_start (GTK_BOX (center_vbox), w_current->h_scrollbar, FALSE, FALSE, 0);

    g_signal_connect (w_current->h_adjustment,
                      "value_changed",
                      G_CALLBACK (x_event_hschanged),
                      w_current);
  }
  if (w_current->handleboxes && w_current->toolbars) {
     x_toolbars_init_bottom(w_current, main_box);
  }

  /* macro box */
  w_current->macro_entry = gtk_entry_new();
  g_signal_connect(w_current->macro_entry, "activate",
                   G_CALLBACK(&x_window_invoke_macro), w_current);

  w_current->macro_box = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX (w_current->macro_box),
                     gtk_label_new (_("Evaluate:")), FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(w_current->macro_box), w_current->macro_entry,
                     TRUE, TRUE, 2);
  gtk_container_border_width(GTK_CONTAINER(w_current->macro_box), 1);
  gtk_box_pack_start (GTK_BOX (main_box), w_current->macro_box,
                      FALSE, FALSE, 0);

  /* bottom box */
  bottom_box = gtk_hbox_new(FALSE, 0);
  gtk_container_border_width(GTK_CONTAINER(bottom_box), 1);
  gtk_box_pack_start (GTK_BOX (main_box), bottom_box, FALSE, FALSE, 0);

  /*	label = gtk_label_new ("Mouse buttons:");
        gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 10);
  */

  label = gtk_label_new (" ");
  gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 2);

  w_current->left_label = gtk_label_new (_("Pick"));
  gtk_box_pack_start (GTK_BOX (bottom_box), w_current->left_label,
                      FALSE, FALSE, 0);

  label = gtk_label_new ("|");
  gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 5);


  w_current->middle_label = gtk_label_new ("middle");
  i_update_middle_button(w_current, "none");
  gtk_box_pack_start (GTK_BOX (bottom_box), w_current->middle_label,
                      FALSE, FALSE, 0);

  label = gtk_label_new ("|");
  gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 5);

  if (default_third_button == POPUP_ENABLED) {
    w_current->right_label = gtk_label_new (_("Menu/Cancel"));
  } else {
    w_current->right_label = gtk_label_new (_("Pan/Cancel"));
  }
  gtk_box_pack_start (GTK_BOX (bottom_box), w_current->right_label,
                      FALSE, FALSE, 0);

  label = gtk_label_new (" ");
  gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 5);

  w_current->grid_label = gtk_label_new (" ");
  gtk_box_pack_start (GTK_BOX (bottom_box), w_current->grid_label,
                      FALSE, FALSE, 10);

  w_current->status_label = gtk_label_new (_("Select Mode"));
  gtk_box_pack_end (GTK_BOX (bottom_box), w_current->status_label, FALSE,
                    FALSE, 10);

  gtk_widget_show_all (w_current->main_window);

  /* The preceeding show_all revealed Everything, so
   * turn-off widgets that should be hidden */
  gtk_widget_hide(w_current->macro_box);

  /* hide the little red x's in the toolbars */

  x_toolbars_finialize(w_current);

  /* hide the srollbars based on user settings */
  if (w_current->scrollbars == TRUE &&
      w_current->scrollbars_visible == FALSE ) {
    gtk_widget_hide(w_current->v_scrollbar);
    gtk_widget_hide(w_current->h_scrollbar);
  }

  w_current->window = w_current->drawing_area->window;
  w_current->drawable = w_current->window;
  x_window_setup_gc(w_current);

}
/*! \brief Close all open Dialogs
 *  \par Function Description
 *   This function close any dialog boxes that are currently open.
 */
static void x_window_close_all_dialogs(GSCHEM_TOPLEVEL *w_current){
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

  x_pagesel_close (w_current);

  if (w_current->tswindow)
  gtk_widget_destroy(w_current->tswindow);

  if (w_current->iwindow)
  gtk_widget_destroy(w_current->iwindow);

  if (w_current->hkwindow)
  gtk_widget_destroy(w_current->hkwindow);

  if (w_current->cowindow)
  gtk_widget_destroy(w_current->cowindow);

  if (w_current->clwindow)
  gtk_widget_destroy(w_current->clwindow);

  if (w_current->sewindow)
  gtk_widget_destroy(w_current->sewindow);

  x_console_close();

}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_close(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  bool last_window = FALSE;

  /* If we're closing whilst inside a move action, re-wind the
   * page contents back to their state before we started */
  if (w_current->inside_action &&
      (w_current->event_state == MOVE ||
       w_current->event_state == ENDMOVE)) {
    o_move_cancel (w_current);
  }

  /* last chance to save possible unsaved pages */
  if (!x_dialog_close_window (w_current)) {
    v_log_message("Close Window canceled");
    /* user cancelled the close */
    return;
  }
  x_clipboard_finish (w_current);

#if DEBUG
  o_conn_print_hash(w_current->page_current->conn_table);
#endif

  if (toplevel->major_changed_refdes) {
    GList* current = toplevel->major_changed_refdes;
    while (current)
    {
      g_free(current->data);
      current = g_list_next(current);
    }
    g_list_free(toplevel->major_changed_refdes);
  }

  /* stuff that has to be done before we free w_current */
  if (g_list_length (global_window_list) == 1) {
    /* no more window after this one, remember to quit */
    last_window = TRUE;
    if(w_current->save_ui_settings == TRUE) {
      x_toolbars_save_state(w_current);
      x_menu_save_state(w_current);
      x_window_close_all_dialogs(w_current);
      x_window_save_geometry((GtkWindow*)w_current->main_window, "gschem");
    }

    /* close the log file */
    s_log_close ();

    /* free the buffers */
    o_buffer_free (w_current);
  }

  x_toolbars_free_window(w_current);
  x_window_free_gc(w_current);

  /* Clear Guile smob weak ref */
  if (w_current->smob != SCM_UNDEFINED) {
    SCM_SET_SMOB_DATA (w_current->smob, NULL);
    w_current->smob = SCM_UNDEFINED;
  }

  /* finally close the main window */
  gtk_widget_destroy(w_current->main_window);

  s_toplevel_delete (toplevel);
  global_window_list = g_list_remove (global_window_list, w_current);
  g_free (w_current);

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
void x_window_close_all(GSCHEM_TOPLEVEL *w_current)
{
  GSCHEM_TOPLEVEL *current;
  GList *list_copy, *iter;

  iter = list_copy = g_list_copy (global_window_list);
  while (iter != NULL ) {
    current = (GSCHEM_TOPLEVEL *)iter->data;
    iter = g_list_next (iter);
    x_window_close (current);
  }
  g_list_free (list_copy);
}

/*! \brief Opens a new page from a file.
 *  \par Function Description
 *  This function opens the file whose name is <B>filename</B> in a
 *  new PAGE of <B>toplevel</B>.
 *
 *  If there is no page for <B>filename</B> in <B>toplevel</B>'s list
 *  of pages, it creates a new PAGE, loads the file in it and returns
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
PAGE*
x_window_open_page (GSCHEM_TOPLEVEL *w_current, const char *filename)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  PAGE     *old_current, *page;
  char      strbuff[MAX_PATH];
  char     *path;
  char     *ptr;
  int       file_err;

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
       *ptr = G_DIR_SEPARATOR;     /* add separator */
      ++ptr;                       /* advance over separator */
       *ptr = '\0';                /* Add new NULL */

      /* Append default name from config */
      str = strcat  ( str, toplevel->untitled_name );

      /* Coverted and append an integer to the string */
      tmp = int2str ( ++w_current->num_untitled, &s_val[0], 10 );
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
  inline PAGE* empty_page( const char *name ) {
    char     *fname;
    fname = g_strdup ( name ? name : generate_untitled() );
    page = s_page_new (toplevel, fname);
    s_page_goto (toplevel, page);
    if (!quiet_mode)
      s_log_message (_("New file [%s]\n"),
                     toplevel->page_current->page_filename);
    g_free (fname);
    return page;
  }

  /* Recover by switching back to Old or a create blank */
  inline void resolve_2_recover( const char *name ) {
    /* There was an error, try go back to old page */
    if ( old_current != NULL ) {
      s_page_goto (toplevel, old_current);
    }
    else { /* There was error and no previous page */
      /*fprintf(stderr, "creating empty page\n"); */
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
        page = s_page_new (toplevel, g_strdup (filename));
        s_page_goto (toplevel, page);
        /* Try to load the file */

        if (!f_open (toplevel, page, (char *) filename, &err)) {
          g_warning ("%s\n", err->message);
          fprintf(stderr, "Error loading file:%s\n", err->message);
          s_log_message( "Failed to load file:%s\n", err->message);
          g_error_free (err);
          s_page_delete (toplevel, page);
          resolve_2_recover(NULL);
        }
        else { /* the file was loaded */
          if (!quiet_mode) {
            s_log_message (_("Loading schematic \"%s\"\n"), filename);
          }
          recent_files_add (filename);
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
      path = dirname(path);                    /* g_path_get_dirname() */
      /* If the path is OK but no file then just create a new file */
      if ((access(path, W_OK && X_OK && F_OK) == 0) && (file_err == ENOENT)) {
        if (!quiet_mode) {
          s_log_message("Creating new file \"%s\"\n", filename);
        }
       /* Filespec may not exist but user has authority to create */
        page = empty_page(filename);
      }
      else { /* Houston, we have problem */
        /* Filename was specified and but path error, so we still
         * don't know if base name is okay. Break down filespec and try
         * to sort out the problem:
         */
        if( errno == ENOENT) { /* 100% sure file_err == ENOENT */
          if( mkdir (path, S_IRWXU | S_IRWXG) == NO_ERROR ) {
            s_log_message("Path \"%s\": does not exist\n, successfully created\n", path);
            page = empty_page(filename);
            errno = NO_ERROR;
          }
          else {
            s_log_message("Path \"%s\": is not accessible!\n", path);
          }
        }

        if( errno != NO_ERROR) {
          const char   *homedir = g_getenv ("HOME"); /* does not allocate */
          if (!homedir) homedir = g_get_home_dir (); /* does not allocate */
          path = strcpy(&strbuff[0], homedir);
          ptr  = (char*) filename;
          while ( *ptr != '\0') ++ptr;      /* advance to end of argument */
          while ( *ptr != G_DIR_SEPARATOR) --ptr;  /* backup to separator */
          path = strcat(path, ptr);
          /* set Flag for file-save to use file-saveas */
          w_current->force_save_as = TRUE;
#if DEBUG
          fprintf(stderr, "filename:%s\n path:%s\n", path, filename);
#endif
          resolve_2_recover(path);
        }
      }
    }
  }

  a_zoom_extents (w_current,
                  s_page_objects (toplevel->page_current),
                  A_PAN_DONT_REDRAW);

  o_undo_savestate (w_current, UNDO_ALL);

  /* This line is generally un-needed, however if some code
   * wants to open a page, yet not bring it to the front, it is
   * needed to add it into the page manager. Otherwise, it will
   * get done in x_window_set_current_page(...)
   */
  x_pagesel_update (w_current); /* ??? */

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
 *  <B>page</B> has to be in the list of PAGEs attached to <B>toplevel</B>.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to become current page.
 */
void
x_window_set_current_page (GSCHEM_TOPLEVEL *w_current, PAGE *page)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (page != NULL);

  o_redraw_cleanstates (w_current);

  s_page_goto (toplevel, page);
  i_update_sensitivities (w_current);

  i_set_filename (w_current, page->page_filename);
  x_pagesel_update (w_current);
  x_multiattrib_update (w_current);

  x_manual_resize (w_current);
  x_hscrollbar_update (w_current);
  x_vscrollbar_update (w_current);
  o_invalidate_all (w_current);

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
int
x_window_save_page (GSCHEM_TOPLEVEL *w_current, PAGE *page, const char *filename)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  PAGE *old_current;
  const char *log_msg, *state_msg;
  int result;
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
    log_msg   = _("Could NOT save page [%s]\n");
    state_msg = _("Error while trying to save");

    titled_error_dialog(err->message, "Failed to save file");

    g_clear_error (&err);
  } else {
    /* successful save of page to file, update page... */
    /* change page name if necessary and prepare log message */
    if (g_ascii_strcasecmp (page->page_filename, filename) != 0) {
      g_free (page->page_filename);
      page->page_filename = g_strdup (filename);

      log_msg = _("Saved as [%s]\n");
    } else {
      log_msg = _("Saved [%s]\n");
    }
    state_msg = _("Saved");

    /* reset page CHANGED flag */
    page->CHANGED = FALSE;
    /* add to recent file list */
    recent_files_add(filename);
  }

  /* log status of operation */
  s_log_message (log_msg, filename);

  /* update display and page manager */
  x_window_set_current_page (w_current, old_current);

  i_set_state_msg  (w_current, SELECT, state_msg);

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
void
x_window_close_page (GSCHEM_TOPLEVEL *w_current, PAGE *page)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  PAGE *new_current = NULL;
  GList *iter;

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (page     != NULL);

  if (page->pid == -1) {
    s_log_message ("Internal Error: <%s><x_window_close_page>"
                   "invalid page ID=<%d>, line %d.\n",
                   __FILE__, page->pid, __LINE__);
    return;
  }

  /* If we're closing whilst inside a move action, re-wind the
   * page contents back to their state before we started */
  if (w_current->inside_action &&
      (w_current->event_state == MOVE ||
       w_current->event_state == ENDMOVE)) {
    o_move_cancel (w_current);
  }

  if (page == toplevel->page_current) {
    /* as it will delete current page, select new current page */
    /* first look up in page hierarchy */
    new_current = s_page_search_by_page_id (toplevel->pages, page->up);

    if (new_current == NULL) {
      /* no up in hierarchy, choice is prev, next, new page */
      iter = g_list_find( geda_list_get_glist( toplevel->pages ), page );

      if ( g_list_previous( iter ) ) {
        new_current = (PAGE *)g_list_previous( iter )->data;
      } else if ( g_list_next( iter ) ) {
        new_current = (PAGE *)g_list_next( iter )->data;
      } else {
        /* need to add a new untitled page */
        new_current = NULL;
      }
    }
    /* new_current will be the new current page at the end of the function */
  }

  s_log_message (page->CHANGED ? _("Discarding page [%s]\n") : _("Closing [%s]\n"),
                 page->page_filename);
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


/*! \brief Setup default icon for GTK windows
 *
 *  \par Function Description
 *  Sets the default window icon by name, to be found in the current icon
 *  theme. The name used is \#defined above as GSCHEM_THEME_ICON_NAME.
 */
void x_window_set_default_icon( void )
{
  gtk_window_set_default_icon_name( GSCHEM_THEME_ICON_NAME );
}
/* ---------------------- Main Window Toolbar Processor -------------------- */
/*!
 * \brief View toogle Add toolbar
 * \par Function Description
 *      This function toggles the visibility of the Add toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar.
 */
void x_window_add_toolbar_toggle(GtkWidget *widget,
                                      GSCHEM_TOPLEVEL *w_current)
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
 * containing the toolbar
 */
void x_window_attribute_toolbar_toggle(GtkWidget *widget,
                                       GSCHEM_TOPLEVEL *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->attribute_handlebox);
  else
    gtk_widget_hide(w_current->attribute_handlebox);
}
/*!
 * \brief View toogle Edit toolbar
 * \par Function Description
 *      This function toggles the visibility of the Edit toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar.
 */
void x_window_edit_toolbar_toggle(GtkWidget *widget,
                                      GSCHEM_TOPLEVEL *w_current)
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
                                      GSCHEM_TOPLEVEL *w_current)
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
 * containing the toolbarx_window_page_toolbar_toggle
 */
void x_window_standard_toolbar_toggle(GtkWidget *widget,
                                      GSCHEM_TOPLEVEL *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->standard_handlebox);
  else
    gtk_widget_hide(w_current->standard_handlebox);
}
/*!
 * \brief View toogle Zoom toolbar
 * \par Function Description
 *      This function toggles the visibility of the Zoom toobar.
 * Note the function actually toggle visibility of the handlebox
 * containing the toolbar
 */
void x_window_zoom_toolbar_toggle(GtkWidget *widget,
                                      GSCHEM_TOPLEVEL *w_current)
{
  bool show = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
  if(show)
    gtk_widget_show(w_current->zoom_handlebox);
  else
    gtk_widget_hide(w_current->zoom_handlebox);
}
