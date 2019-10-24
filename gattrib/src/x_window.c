/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2015 Stuart D. Brorson.
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

/*------------------------------------------------------------------*/

/*! \file
 * \brief Functions for the toplevel window
 * This file holds functions used to handle the toplevel window and
 * various widgets held by that window. Widgets used to handle
 * GtkSheet pointers are in a different file.
 */

#include "../include/gattrib.h"

#include <gtk/gtk.h>
#include <gtksheet.h>

#include <geda_widgets.h>

/*------------------------------------------------------------------
 * Gattrib specific defines
 *------------------------------------------------------------------*/
#define GATTRIB_THEME_ICON_NAME "geda-gattrib"

/*!
 * \brief Set application icon
 * \par Function Description
 *  This function sets the default window icon by name, to be found in
 *  the current icon theme. The name used is defined by the directive
 *  GATTRIB_THEME_ICON_NAME.
 */
static void x_window_set_default_icon( void )
{
  gtk_window_set_default_icon_name(GATTRIB_THEME_ICON_NAME);
}

/*!
 * \brief Set or Update the Window Title
 * \par Function Description
 *  This function obtains the filename for the toplevel and set the
 *  Title for the main window. If the PageData->CHANGED is set then
 *  the title is prefixed with an asterisk.
 *
 * \param [in] toplevel pointer to toplevel (pr_current)
 * \param [in] PageData pointer to Sheet_data structure
 */
void x_window_update_title(GedaToplevel *toplevel, PageDataSet *PageData)
{
  const char *filename = toplevel->page_current->filename;

  if (filename != NULL) {

    char buffer[MAX_WINDOW_TITLE];

    if (!toplevel->show_full_path) {
      filename = geda_file_get_basename(filename);
    }

    while (strlen(filename) > MAX_WINDOW_TITLE - 13) {
      if (strstr(filename, DIR_SEPARATOR_S)) {
        filename = strstr(filename, DIR_SEPARATOR_S);
      }
      else {
        filename = filename + 10;
      }
    }

    if (PageData->CHANGED) {
      strcpy (buffer, "*");
      strcat (buffer, filename);
    }
    else {
      strcpy (buffer, filename);
    }
    strcat(buffer, " -- gattrib");
    gtk_window_set_title(main_window, buffer);
  }
  else {
    gtk_window_set_title(main_window, "gattrib -- gEDA attribute editor");
  }
}

/*!
 * \brief Handle Cut, Copy, Paste for Menus and Toolbar
 * \par Function Description
 *  This function is called from the menu and toolbar callbacks to
 *  process Cut, Copy, Paste request.
 *
 * \param [in] do_what Enumerated integer ID of operation to perform
 */
void x_window_clipboard_handler(int do_what)
{
  GtkWidget *widget = gtk_window_get_focus(main_window);

  switch (do_what ) {
    case tb_cut:
      if (GEDA_IS_LABEL(widget) && geda_label_widget_get_selectable(widget)) {
        g_signal_emit_by_name(widget, "copy-clipboard", NULL); /* just copy */
      }
      else if(GTK_IS_ENTRY(widget) || GTK_IS_TEXT_VIEW(widget)) {
        g_signal_emit_by_name(widget, "cut-clipboard", NULL);
        sheet_head->CHANGED = TRUE;
        x_window_update_title(pr_current, sheet_head);
      }
      break;

    case tb_copy:
      if ((GEDA_IS_LABEL(widget) && geda_label_widget_get_selectable(widget))
        || GTK_IS_ENTRY(widget) || GTK_IS_TEXT_VIEW(widget))
      {
        g_signal_emit_by_name(widget, "copy-clipboard", NULL);
      }
      break;

    case tb_paste:
      if (GTK_IS_ENTRY(widget) || GTK_IS_TEXT_VIEW(widget)) {
        g_signal_emit_by_name(widget, "paste-clipboard", NULL);
        sheet_head->CHANGED = TRUE;
        x_window_update_title(pr_current, sheet_head);
      }
      break;

    default:
      geda_log ("%s: unknown Id [%d]\n", __func__, do_what);
  }
}

/** \brief on_notebook_switch_page in X_Windows_Support_Functions */

/*!
 * \brief Callback on TAB change.
 * \par Function Description
 *  This function is called when ever a TAB sheet is selected. This
 *  allows all sensitivities for menus and toolbars to be set on a
 *  per sheet basis.
 */
static void
on_notebook_switch_page (GtkNotebook *notebook, GtkNotebookPage *page,
                         unsigned int page_num, void *    user_data)
{

  switch ( page_num ) {
  case Components:
    x_menus_set_sensitivities(ComponentMenuItems, TRUE);
    x_toolbar_set_sensitivities(ComponentToolbarButtons, TRUE);
    break;

  case Nets:
  case Pins:
    x_menus_set_sensitivities(ComponentMenuItems, FALSE);
    x_toolbar_set_sensitivities(ComponentToolbarButtons, FALSE);
    break;

  default:
    geda_log ("notebook_switch_page(): BAD_TAB ID %d\n", page_num);
  }

  return;
}

/* Disconnects the on_notebook_switch_page handler at exit */
static void x_window_disconnect_notebook_switch_page(void *notebook)
{
  g_signal_handlers_disconnect_by_func(notebook,
                                       on_notebook_switch_page,
                                       NULL);
}

static void x_window_save_settings(void *data)
{
  if (GTK_IS_WINDOW(data)) {

    EdaConfig  *cfg;
    GtkWindow  *window;

    const char *win_group = "gattrib";
    int x, y, width, height;

    window = (GtkWindow*)data;

    cfg = eda_config_get_user_context ();

    /* Get the Window Geometry - Restored by x_window_restore_settings */
    gtk_window_get_position (window, &x, &y);
    gtk_window_get_size (window, &width, &height);

    geda_log (_("Saving main window geometry and settings.\n"));

    /* Save the Window Geometry data */
    eda_config_set_integer (cfg, win_group, "window-x-position", x);
    eda_config_set_integer (cfg, win_group, "window-y-position", y);
    eda_config_set_integer (cfg, win_group, "window-width",      width);
    eda_config_set_integer (cfg, win_group, "window-height",     height);
  }
}

#define DEFAULT_WINDOW_WIDTH  1000
#define DEFAULT_WINDOW_HEIGHT 600

/*!
 * \brief Restore Window Geometry and Cursor
 * \par Function Description
 *  This functions retrieves the given window size and position from the
 *  key file and sets the given window to the retrived values.
 *
 * \param [in] window  Gattrib toplevel MainWindow.
 */
void x_window_restore_settings(GtkWindow *window)
{
  EdaConfig  *cfg;
  GError     *err        = NULL;
  const char *group_name = "gattrib";
  bool        xy_error   = FALSE;

  int x, y, width, height;

  geda_log (_("Retrieving main Window geometry and settings.\n"));

  cfg = eda_config_get_user_context ();

  x = eda_config_get_integer (cfg, group_name, "window-x-position", &err);

  if (err != NULL) {
    geda_utility_log_quite("%s\n", err->message);
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

  if (xy_error) {
    gtk_window_set_position(window, GTK_WIN_POS_CENTER);
  }
  else {
    gtk_window_move (window, x, y);
  }

  /* If, for any reason, we pass a zero value to gtk_window_resize an error
   * will be generated. We double check these as fail safe because the above
   * conditionals only set default values if an error occurred retrieving
   * settings, so...*/
  if (width == 0 ) {
    width = DEFAULT_WINDOW_WIDTH;
  }

  if (height == 0) {
    height = DEFAULT_WINDOW_HEIGHT;
  }

  gtk_window_resize (window, width, height);
}

/*!
 * \brief Delete Event handler for the Main Window
 * \par Function Description
 *  When the File/Quit menu option is used to exit gattrib, an extra reference
 *  is held on the menu_bar. This function increases the reference on the menu
 *  bar when the main window receives a "delete_event" to balance the reference
 *  count so that x_menu_release_all can unreference the menu_bar regardless of
 *  which method was used to terminate the program.
 */
static bool x_window_quit(void)
{
  g_object_ref_sink(menu_bar);

  return gattrib_really_quit();
}

/*!
 * \brief Initializes the Main Window
 * \par Function Description
 *  This function creates and initializes the Main window, calling various
 *  other function to add the primary window widgets like menus, toolbars,
 *  the GTK notebook container, etc. Each widget is set visible here except
 *  the main window itself, which is done later.
 *
 * \sa x_window_finalize_startup
 */
void x_window_init()
{
  GtkWidget      *main_vbox;
  GtkRequisition  request;

  /* Set default icon */
  x_window_set_default_icon();

  /*  window is a global declared in globals.h.  */
  main_window = (GtkWindow*)gtk_window_new(GTK_WINDOW_TOPLEVEL);

  geda_atexit(x_window_save_settings, main_window);

  x_window_restore_settings(main_window);

  GEDA_SIGNAL_CONNECT (main_window, "delete_event",
                       G_CALLBACK (x_window_quit), 0);

  /* -----  Now create main_vbox container to hold everthing ----- */

  main_vbox = gtk_vbox_new(FALSE,1);
  geda_set_container_border_width(main_vbox, 1);
  geda_container_add(main_window, main_vbox);
  gtk_widget_show(main_vbox);

  /* -----  Now create menu bar  ----- */
  menu_bar = x_menu_create_menu(main_window);
  gtk_box_pack_start((GtkBox*)main_vbox, menu_bar, FALSE, TRUE, 0);

  /* -----  Initialize the Toolbar Module ----- */
  x_toolbars_init(main_vbox);

  edit_box = gtk_hbox_new(FALSE, 1);
  geda_set_container_border_width(edit_box, 0);
  gtk_box_pack_start((GtkBox*)main_vbox, edit_box, FALSE, TRUE, 0);
  gtk_widget_show(edit_box);

   /* This is the RC box in the top left cell */
  location = geda_visible_label_new(" ");

  gtk_widget_size_request(location, &request);
  gtk_widget_set_usize(location, 150, request.height);
  gtk_box_pack_start((GtkBox*)edit_box, location, FALSE, TRUE, 0);

   /* Global in include/globals.h - likely a very bad thing */
  entry = geda_entry_new_visible ();
  gtk_box_pack_start((GtkBox*)edit_box, entry, TRUE, TRUE, 0);

  /* TODO togglable legend ? */

  /* -----  Initialize notebook widget  ----- */
  notebook = gtk_notebook_new();
  gtk_notebook_set_tab_pos((GtkNotebook*)notebook, GTK_POS_BOTTOM);
  gtk_box_pack_start((GtkBox*)main_vbox, notebook, TRUE, TRUE, 0);

  GEDA_SIGNAL_CONNECT (notebook, "switch-page",
                       on_notebook_switch_page,
                       NULL);

  geda_atexit(x_window_disconnect_notebook_switch_page, notebook);
}

/*!
 * \brief Load Blank Document
 * \par Function Description
 *  This function is called in place of s_toplevel_init_data_set
 *  when there is no file specified or there was an error while
 *  attempting to load a file. This allows gattrib to startup
 *  without the initial file-open dialog.
 */
void x_window_blank_document(GedaToplevel *toplevel, PageDataSet *PageData)
{
  Page *page;

  s_sheet_data_load_blank(PageData);

  page = geda_struct_page_new (toplevel, NULL);

  geda_toplevel_set_current_page(page);
}

/*!
 * \brief Add all items to the top level window
 * \par Function Description
 *  The function calls x_gktsheet_add_row_labels and
 *  x_gktsheet_add_col_labels for each worksheet and
 *  then loads the cell values for each sheet.
 */
void x_window_add_items(PageDataSet *PageData)
{
  int col, row;
  char *text;
  int visibility, show_name_value, is_inherited;

  /* Add labels to the component sheet */
  if (PageData->comp_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[Components]),
                              PageData->comp_count,
                              PageData->master_comp_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[Components]),
                              PageData->comp_attrib_count,
                              PageData->master_comp_attrib_list_head);
  }

  /* This is not ready.  Need to implement net attributes */
  if (PageData->net_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[Nets]),
                              PageData->net_count, PageData->master_net_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[Nets]),
                              PageData->net_attrib_count,
                              PageData->master_net_attrib_list_head);
  }
  else {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[Nets]), 1, NULL);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[Nets]), 1, NULL);
  }

  if (PageData->pin_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[Pins]),
                              PageData->pin_count, PageData->master_pin_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[Pins]),
                              PageData->pin_attrib_count,
                              PageData->master_pin_attrib_list_head);
  }

  /* ------ Comp sheet: put values in the individual cells ------- */
  for (col = 0; col < PageData->comp_attrib_count; col++) {
    for (row = 0; row < PageData->comp_count; row++) {
      if ((PageData->component_table)[col][row].attrib_value ) { /* NULL = no entry */
        text            = (PageData->component_table)[col][row].attrib_value;
        visibility      = (PageData->component_table)[col][row].visibility;
        show_name_value = (PageData->component_table)[col][row].show_name_value;
        is_inherited    = (PageData->component_table)[col][row].is_inherited;
        x_gtksheet_add_cell_item(GTK_SHEET(sheets[Components]), row, col, text,
                                  visibility, show_name_value, is_inherited);
      }
    }
  }

  /* ------ Net sheet: put values in the individual cells ------- */
  for (col = 0; col <PageData->net_attrib_count; col++) {
    for (row = 0; row < PageData->net_count; row++) {
      if ((PageData->net_table)[col][row].attrib_value ) { /* NULL = no entry */
        text            = (PageData->net_table)[col][row].attrib_value;
        visibility      = (PageData->net_table)[col][row].visibility;
        show_name_value = (PageData->component_table)[col][row].show_name_value;
        x_gtksheet_add_cell_item (GTK_SHEET(sheets[1]), row, col, text, visibility, show_name_value, 0);
      }
    }
  }

  /* ------ Pin sheet: put pin attribs in the individual cells ------- */
  for (col = 0; col < PageData->pin_attrib_count; col++) {
    for (row = 0; row < PageData->pin_count; row++) {
      if ((PageData->pin_table)[col][row].attrib_value) { /* NULL = no entry */
        text = (PageData->pin_table)[col][row].attrib_value;
        /* pins have no visibility attributes, must therefore provide default. */
        x_gtksheet_add_cell_item( GTK_SHEET(sheets[2]), row, col, text,  VISIBLE, SHOW_VALUE, 0);
      }
    }
  }
}

/*!
 * \brief Install the Application Icon for Windows platform
 * \par Function Description
 *  Install the app icon for Windows platforms. The bitmap was installed
 *  to the themes directory, which for windows should be the prefix/geda
 *  /icons/hicolor/48x48/apps/geda-gattrib.png. If icon is not installed
 *  then Gtk assigns an ugly generic icon.
 */
void x_window_set_main_icon()
{

#ifdef OS_WIN32

  GdkPixbuf  *icon;
  GError     *error;
  const char *base;
        char *filename;

  error = NULL;
  icon  = NULL;

  base = geda_file_path_sys_data();

  filename = g_build_filename (base, "../icons/hicolor/48x48/apps/geda-gattrib.png", NULL);

  if (filename) {
    icon = gdk_pixbuf_new_from_file (filename, &error);
  }

  if (icon) {
    gtk_window_set_icon ((GtkWindow*)main_window, icon);
  }
  else if (verbose_mode) {
    fprintf(stderr, "%s is missing\n", filename);
  }

#endif

}

/*!
 * \brief Complete startup initialization for Main Window
 * \par Function Description
 *  This function is called from the main-line after the GTKSheet is up.
 *  The function calls the previous function, x_window_add_items to load
 *  attribute values, then sets the main window position and displayed
 *  the window and update the title bar.
 */
void x_window_finalize_startup(GtkWindow *main_window, PageDataSet *PageData)
{

  x_window_set_main_icon();

  /* -------------- update data in windows --------------- */
  x_window_add_items(PageData); /* This updates the top level stuff,and then
                                   calls another fcn to update the GtkSheet */

  gtk_window_set_position (main_window, GTK_WIN_POS_MOUSE);

  gtk_widget_show((GtkWidget*)main_window);

  x_window_update_title(pr_current, PageData);
}

/* ---------------------- Main Window Toolbar Processor -------------------- */
/*!
 * \brief View toogle Attribute toolbar
 * \par Function Description
 *  This function toggles the visibility of the Attribute toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar
 */
void x_window_attribute_toolbar_toggle(GtkToggleAction *action,
                                       GtkWindow       *main_window)
{
  bool show = gtk_toggle_action_get_active(action);
  if(show)
    gtk_widget_show(Attribute_handlebox);
  else
    gtk_widget_hide(Attribute_handlebox);
  /* TODO: WEH: save the toggle setting */
  //config_file_set_bool(PREFS_TOOLBAR_VISIBLE, show);
}

/*!
 * \brief View toogle standard toolbar
 * \par Function Description
 *  This function toggles the visibility of the Standard toobar.
 *  Note the function actually toggle visibility of the handlebox
 *  containing the toolbar
 */
void x_window_standard_toolbar_toggle(GtkToggleAction *action,
                                      GtkWindow       *main_window)
{
  bool show = gtk_toggle_action_get_active(action);

  if(show) {
    gtk_widget_show(Standard_handlebox);
  }
  else {
    gtk_widget_hide(Standard_handlebox);
  }

  /* TODO: WEH: save the toggle setting */
  //config_file_set_bool(PREFS_TOOLBAR_VISIBLE, show);
}

/*!
 * \brief Toggle View between All and Attached Attributes
 * \par Function Description
 *  This function checks each column name and hides or unhides the
 *  column, depending on the action parameter, all columns that are
 *  not members of attached atribute list ->attached_attrib.
 */
#define TOGGLE_ATTACH_X_OFFSET 100
void x_window_attached_toggle(GtkToggleAction *action, GtkWindow *main_window)
{
  int i, row;
  int x, y;
  char *col_name;
  char *attached;
  bool toggle;

  GtkSheet *sheet = sheets[Components];

  bool show = gtk_toggle_action_get_active(action);
  int count = sheet->maxcol;

  for ( i = 0; i <= count; i++) {

    if (show) {

      int num_ribs;

      toggle   = TRUE;
      col_name = sheet->column[i]->title;
      num_ribs = sheet_head->attached_attrib_count;

      for (row = 0; row < num_ribs; row++) {

        attached = s_string_list_get_data_at_index(sheet_head->attached_attrib, row);

        if (strcmp(attached, col_name) == 0) {
          toggle = FALSE;
          break;
        }
      } /* Next row */

      if (toggle) {
        gtk_sheet_column_set_visibility(sheet, i, FALSE);
      }
    }
    else {
      /* Make a columns visible */
      gtk_sheet_column_set_visibility(sheet, i, TRUE);
    }
  } /* Next i */

  /* The background outside the sheet will not get updated until something
   * happens to the parent window and this will leave residual lines, (newer
   * version of gtksheet claiming fix had the same problem, so we will resize,
   * which needs to be done anyways */
  /* TODO: WEH: Determine the real x size */
  gtk_window_get_size(main_window, &x, &y);

  x = (show) ? x - TOGGLE_ATTACH_X_OFFSET : x + TOGGLE_ATTACH_X_OFFSET;

  gtk_window_resize(main_window, x, y);

  /* TODO: WEH: save the toggle setting */
  //config_file_set_bool(PREFS_ATTACHED_VISIBLE, show);
}

/*!
 * \brief Toggle View of Inherited Attributes
 * \par Function Description
 *      This function loops through all rows and columns and either
 * delete or restore attribute values if the attribute is inherited.
 */
void x_window_inherited_toggle(GtkToggleAction *action, GtkWindow *main_window) {

  bool show = gtk_toggle_action_get_active(action);

  char *text;

  int maxcol = sheet_head->comp_attrib_count;
  int maxrow = sheet_head->comp_count;
  int row, col;

  for( col = 0; col < maxcol; col++) {
    for( row = 0; row < maxrow; row++) {
      if (sheet_head->component_table[col][row].is_inherited) {
        if (show) {
          text = (sheet_head->component_table)[col][row].attrib_value;
          gtk_sheet_set_cell(sheets[0], row, col, GTK_JUSTIFY_LEFT, text);
        }
        else {
          gtk_sheet_set_cell(sheets[0], row, col, GTK_JUSTIFY_LEFT, "");
        }
      }
    }
  }
}

/*!
 * \brief Toggle Edit-bar On Off
 * \par Function Description
 *      This function toggles the visibility of the Edit Entry bar
 */
/* View->Statusbar */
void x_window_editbar_toggle(GtkToggleAction *action, GtkWindow *main_window)
{
  bool show = gtk_toggle_action_get_active(action);
  if(show)
    gtk_widget_show(edit_box);
  else
    gtk_widget_hide(edit_box);
  //config_file_set_bool(PREFS_STATUSBAR_VISIBLE, show);
}

/*!
 * \brief Toggle Auto-Resize Option On Off
 * \par Function Description
 *      This function toggles the GTKSheet Autoresize flag, which would normally
 *  be turned off because some of the entries for documentation can be lengthly.
 */
void x_window_autoresize_toggle(GtkToggleAction *action, GtkWindow *main_window)
{
  bool show = gtk_toggle_action_get_active(action);
  gtk_sheet_set_autoresize(sheets[Components], show);
  gtk_sheet_set_autoresize(sheets[Nets], show);
  gtk_sheet_set_autoresize(sheets[Pins], show);
}

/*!
 * \brief Toggle Auto-Scroll Option On Off
 * \par Function Description
 *      This function toggles the GTKSheet Auto-Scroll flag, The flag is normally
 * turned on.
 */
void x_window_autoscroll_toggle(GtkToggleAction *action, GtkWindow *main_window)
{
  bool show = gtk_toggle_action_get_active(action);

  gtk_sheet_set_autoscroll(sheets[Components], show);
  gtk_sheet_set_autoscroll(sheets[Nets], show);
  gtk_sheet_set_autoscroll(sheets[Pins], show);
}

/*!
 * \brief Toggle Sheet Grid Option On Off
 * \par Function Description
 *  This function toggles visibility of the grid lines in the GTKSheets
 *  The flag is normally turned on.
 */
void x_window_grid_toggle(GtkToggleAction *action, GtkWindow *main_window)
{
  bool show = gtk_toggle_action_get_active(action);

  gtk_sheet_show_grid(sheets[Components], show);
  gtk_sheet_show_grid(sheets[Nets], show);
  gtk_sheet_show_grid(sheets[Pins], show);
}

/*!
 * \brief Release GUI related resources
 * \par Function Description
 *  This function releases various resources associated with the GUI.
 */
void x_window_release_all(void)
{
  /* Release Menu before the toolbar stuff */
  x_menu_release_all();
  x_toolbar_release_all();

  /* edit_box contains entry and location */
  gtk_widget_destroy (entry);
  gtk_widget_destroy (location);
  gtk_widget_destroy (edit_box);
  gtk_widget_destroy (notebook);

  gtk_widget_set_name((GtkWidget*)main_window, NULL);
}
