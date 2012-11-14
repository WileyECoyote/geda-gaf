/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * 
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
#include "config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <gtk/gtk.h>
#include <gattrib.h>  /* include Gattrib specific headers  */

//#include <glib/gstdio.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static void toolbar_icons_only( void );
static void toolbar_text_only( void );
static void toolbar_display_both( void );

/*!------------------------ Menus And Toolbars ---------------------*/
/*!
 * \brief File->Save menu item
 *
 * Implement the File->Save menu
 */
void x_menu_file_save()
{
  s_toplevel_gtksheet_to_toplevel(pr_current);  /* Dumps sheet data into TOPLEVEL */
  s_page_save_all(pr_current);                  /* saves all pages in design */

  sheet_head->CHANGED = FALSE;
}

/*!
 * \brief File Save As menu
 * \par Function Description
 *
 */
void x_menu_file_save_as()
{
  char *filename = malloc(MAX_FILE * sizeof(char)); /* be 255 * 1 */
  
  if (filename) {
    if (x_fileselect(filename)) {
      /* Dumps sheet data into TOPLEVEL */
      s_toplevel_gtksheet_to_toplevel(pr_current);

      /* replace page filename with new one, do not free filename */
      g_free (pr_current->page_current->page_filename);
      pr_current->page_current->page_filename = filename;
      
      s_page_save_all(pr_current);
      /* reset the changed flag of current page*/
      pr_current->page_current->CHANGED = FALSE;
      sheet_head->CHANGED = FALSE;
    } /* else user aborted, do nothing */
  }
  else
     s_log_message("setup_titleblock: Memory allocation error\n");
}
/*!
 * \brief File Open menu
 *
 * File open menu.
 * -# close the current project and reinitialize structures
 * -# load the new project
 */
void x_menu_file_open()
{
  GSList *file_list =NULL;

  file_list = x_fileselect_open();
  if (file_list != NULL ) {
    if( sheet_head->CHANGED == TRUE) {
      switch (x_dialog_file_not_saved()) {
	case GTK_RESPONSE_CANCEL:
	  return; /* user canceled from the save unsaved file dialog */
	case GTK_RESPONSE_YES:
           x_menu_file_save();
        case GTK_RESPONSE_NO:
      	   /* No need to do anything here, just fall through */
        default:
          break;
      }
    }
    s_toplevel_close(sheet_head);
    sheet_head = s_sheet_data_new();
    /* Load the files, don't check if it went OK */
    x_fileselect_load_files(file_list);
    s_toplevel_init_data_set(pr_current, sheet_head);
  /* -------------- update windows --------------- */
    x_gtksheet_reinititialize(sheet_head);
    x_window_add_items(); /* updates toplevel & GtkSheet */
  }
#ifdef DEBUG
    fprintf(stderr, "open file canceled:%s\n", (gchar*) g_slist_nth_data(file_list,0));
#endif
  if (file_list != NULL ){

    g_slist_foreach(file_list, (GFunc)g_free, NULL);
    g_slist_free(file_list);
  }
}
/*!
 * \brief Menu Open Recent
 *
 * Menu Ppen Recent file.
 * -# close the current project and reinitialize structures
 * -# load the new project
 */
static void menu_open_recent( char* filename)
{ 
  if( sheet_head->CHANGED == TRUE) {
    switch (x_dialog_file_not_saved()) {
    case GTK_RESPONSE_CANCEL:
      return; /* user canceled from the save unsaved file dialog */
    case GTK_RESPONSE_YES:
      x_menu_file_save();
    case GTK_RESPONSE_NO:
    /* No need to do anything here, just fall through */
      default:
      break;
    }
  }
  s_toplevel_close(sheet_head);
  sheet_head = s_sheet_data_new();
  /* Load the files, don't check if it went OK */
  x_fileselect_load_file(filename);
  s_toplevel_init_data_set(pr_current, sheet_head);
  x_gtksheet_reinititialize(sheet_head);
  /* -------------- update windows --------------- */
  x_window_add_items(); /* updates toplevel & GtkSheet */
}
/*!
 * \brief File->Export CSV menu item
 *
 * Implement the File->Export CSV menu item
 */
void x_menu_file_export_csv()
{
  int cur_page;

  /* first verify that we are on the correct page (components) */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

  /* Check that we are on components page. */
  if (cur_page == Components) {
    x_dialog_export_file();
  } else {
    x_dialog_unimplemented_feature();  /* We only support export 
                                          of components now */
  }
}

/*!
 * \brief Edit->New attrib menu item
 *
 * Implement the New attrib menu item
 */
void x_menu_edit_newattrib()
{
  int cur_page;

  /* first verify that we are on the correct page (components) */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

  /* Check that we are on components page. */
  if (cur_page == 0) {
    x_dialog_newattrib();  /* This creates dialog box  */
  }
}

/*!
 * \brief Edit->Delete Attribute menu item
 *
 * Implements the Delete Attribute menu item
 */
void x_menu_edit_delattrib()
{
  x_dialog_delattrib();
}
static void menu_edit_cut()
{
  x_window_clipboard_handler(cut);
}
static void menu_edit_copy()
{
  x_window_clipboard_handler(copy);
}
static void menu_edit_paste()
{
  x_window_clipboard_handler(paste);
}
/*!
 * \brief View Toolbar Icons
 * \par Function Description
 *
 */
/* View->Toolbar */
static void toolbar_icons_only( void )
{
  gtk_toolbar_set_style (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_ICONS);
}
/*!
 * \brief View Toolbar Text
 * \par Function Description
 *
 */
static void toolbar_text_only( void )
{
  gtk_toolbar_set_style (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_TEXT);
}
/*!
 * \brief View Toolbar Icons & Text
 * \par Function Description
 *
 */
static void toolbar_display_both( void )
{
  gtk_toolbar_set_style (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_BOTH);
}

/* ----------- Recent Files Menu Stuff ----------- */
#define MAX_RECENT_FILES 10
/*! \brief Callback for recent-chooser.
 *
 * Will be called if element of recent-file-list is activated
 */
static void on_recent_selection (GtkRecentChooser *chooser)
{
  char *uri;
  char *filename;

  uri = gtk_recent_chooser_get_current_uri (chooser);
  filename = g_filename_from_uri(uri, NULL, NULL);
  gtk_recent_manager_add_item(recent_manager, uri);
  
  menu_open_recent(filename);

  g_free(uri);
  g_free(filename);
}

/*!
 * The Gtk action table
 */
static const GtkActionEntry actions[] = {
  /* name, stock-id, label, accelerator, tooltip, callback function */
  /* File menu */
  { "file", NULL, "_File"},
  { "file-open", GTK_STOCK_OPEN, "Open", "<Control>O", "", x_menu_file_open},
  { "file-save", GTK_STOCK_SAVE, "Save", "<Control>S", "", x_menu_file_save},
  { "file-save-as", GTK_STOCK_SAVE_AS, "Save As", "<Shift><Control>S", "", x_menu_file_save_as},
  { "file-export-csv", NULL, "Export CSV", "", "", x_menu_file_export_csv},
  /* { "file-print", GTK_STOCK_PRINT, "Print", "<Control>P", "", x_dialog_unimplemented_feature}, */
  { "file-quit", GTK_STOCK_QUIT, "Quit", "<Control>Q", "", G_CALLBACK(gattrib_really_quit)},

  /* Edit menu */
  { "edit", NULL, "_Edit"},
  { "edit-clipboard-cut", GTK_STOCK_CUT, "Cut", "<Control>X", "", menu_edit_cut},
  { "edit-clipboard-copy", GTK_STOCK_COPY, "Copy", "<Control>C", "", menu_edit_copy},
  { "edit-clipboard-paste", GTK_STOCK_PASTE, "Paste", "<Control>V", "", menu_edit_paste},
      
  { "edit-add-attrib", NULL, "Add new attrib column", "", "", x_menu_edit_newattrib},
  { "edit-delete-attrib", NULL, "Delete attrib column", "", "", x_menu_edit_delattrib},
  /* { "edit-find-attrib", GTK_STOCK_FIND, "Find attrib value", "<Control>F", "", x_dialog_unimplemented_feature}, */
  /* { "edit-search-replace-attrib-value", NULL, "Search and replace attrib value", "", "", x_dialog_unimplemented_feature}, */
  /* { "edit-search-for-refdes", NULL, "Search for refdes", "", "", x_dialog_unimplemented_feature}, */

  /* View menu */
  { "view", NULL, "_View"},
  { "toolbar", NULL, "_Toolbar"},
    { "view-toolbar-icons", NULL, "_Icons", NULL, "Display Icons on the toolbar", toolbar_icons_only},
    { "view-toolbar-text", NULL,  "_Text", NULL, "Display Text on the toolbar", toolbar_text_only},
    { "view-toolbar-both", NULL, "_Both", NULL, "Display Icons and Text on the toolbar", toolbar_display_both},

  /* Visibility menu */
  { "visibility", NULL, "_Visibility"},
  { "visibility-invisible", NULL, "Set selected invisible", "", "", s_visibility_set_invisible},
  { "visibility-name-only", NULL, "Set selected name visible only", "", "", s_visibility_set_name_only},
  { "visibility-value-only", NULL, "Set selected value visible only", "", "", s_visibility_set_value_only},
  { "visibility-name-value", NULL, "Set selected name and value visible", "", "", s_visibility_set_name_and_value},

  /* Help menu */
  { "help", NULL, "_Help"},
  { "help-about", GTK_STOCK_ABOUT, "About", "", "", x_dialog_about_dialog},
};
/* Toggle items */
static const GtkToggleActionEntry toggle_entries[] = {
  { "view-statusbar", "", "Statusbar", "", "Display Status bar", G_CALLBACK(x_window_editbar_toggle), TRUE },
  { "view-toolbar-standard", "", "_Standard", "", "Display Standard Toolbar", G_CALLBACK(x_window_standard_toolbar_toggle), TRUE },
  { "view-attached-attribs", "", "_Attached", "", "Hide or unhide non-attached attributes", G_CALLBACK(x_window_attached_toggle), FALSE },
};

GtkRecentFilter *x_menu_geda_filter() {
  
  GtkRecentFilter *geda_filter;
  
  geda_filter = gtk_recent_filter_new();
  gtk_recent_filter_add_mime_type(geda_filter, "application/x-geda-schematic");
  gtk_recent_filter_add_mime_type(geda_filter, "application/x-geda-symbol");
  gtk_recent_filter_add_pattern(geda_filter, "*.sch");
  gtk_recent_filter_add_pattern(geda_filter, "*.sym");
  
  return geda_filter;
}
GtkRecentChooser *GetRecentMenuChooser(GtkRecentManager *rm )  {
  GtkWidget *rc = gtk_recent_chooser_menu_new_for_manager(rm );
  return (GtkRecentChooser*) rc;
}

void x_menu_fix_gtk_recent_submenu() {

  GtkWidget        *recent_items;          /* Be ones GTK errently added */
  GtkRecentFilter  *recent_filter;
  GtkRecentChooser *recent_file_chooser;
  GtkWidget        *menu_files;
  
  recent_items =  gtk_ui_manager_get_widget (menu_manager,"/ui/menubar/file/OpenRecent");

  if (recent_manager == NULL) {
     GtkContainer *menu = GTK_CONTAINER (gtk_widget_get_parent (recent_items));
     gtk_container_remove (menu, recent_items);
     return;
  }

  menu_files = gtk_recent_chooser_menu_new_for_manager (recent_manager);

  {
    recent_filter = x_menu_geda_filter ();
    recent_file_chooser = (GtkRecentChooser*) menu_files;
    gtk_recent_chooser_add_filter (recent_file_chooser, recent_filter);
    gtk_recent_chooser_set_show_tips (recent_file_chooser, TRUE);
    gtk_recent_chooser_set_sort_type (recent_file_chooser, GTK_RECENT_SORT_MRU);
    gtk_recent_chooser_set_limit(recent_file_chooser, MAX_RECENT_FILES);
    gtk_recent_chooser_set_local_only(recent_file_chooser, FALSE);
    gtk_recent_chooser_menu_set_show_numbers(GTK_RECENT_CHOOSER_MENU(menu_files), TRUE);
  }
  gtk_menu_item_set_submenu (GTK_MENU_ITEM (recent_items), menu_files);

  g_signal_connect (menu_files, "selection-done", G_CALLBACK (on_recent_selection), window);

  return;

}

/*! \brief Attach a submenu with filenames to the 'Open Recent'
 *         menu item.
 *
 *  Called from x_window_setup().
 */
GtkActionGroup *x_menu_create_recent_action_group() {
  GtkRecentAction  *recent_action;
  GtkActionGroup   *recent_action_group;

  recent_manager      = gtk_recent_manager_new();
  recent_action_group = gtk_action_group_new("OpenRecentAction");
/*
 *  Filtering here does not work, maybe someday
 * 
  recent_filter = x_menu_geda_filter();
  gtk_recent_filter_add_group (recent_filter, "OpenRecentAction" );

  recent_file_chooser = GetRecentMenuChooser(recent_manager);
  gtk_recent_chooser_add_filter(recent_file_chooser, recent_filter);
*/
  recent_action =
  (GtkRecentAction*) gtk_recent_action_new_for_manager("OpenRecentAction",
                                                       "Open _Recent",
                                                       "Open recently used files",
                                                        NULL,
                                                        recent_manager);
  
/*
 * Filtering here does not work either, maybe someday
  gtk_recent_chooser_set_filter (recent_file_chooser, recent_filter);
 */
   
  gtk_action_group_add_action (recent_action_group, GTK_ACTION (recent_action));
  g_signal_connect (recent_action, "item-activated",
                    G_CALLBACK (on_recent_selection),
                    NULL);

  return recent_action_group;
}

/*! \brief Create and attach the menu bar
 *
 * Create the menu bar and attach it to the main window.
 *
 *  First, the GtkActionGroup object is created and filled with
 *  entries of type GtkActionEntry (each entry specifies a single
 *  action, such as opening a file). Then the GtkUIManager object
 *  is created and used to load menus.xml file with the menu
 *  description. Finally, the GtkAccelGroup is added to the
 *  main window to enable keyboard accelerators and a pointer
 *  to the menu bar is retrieved from the GtkUIManager object.
 * \param window Window to add the menubar to
 * \param [out] menubar Created menubar
 */
/*
 * 11/05/12 WEH Revised to include toggle_actions group
 * 11/10/12 WEH Revised to include recent_group
 *
 */
GtkWidget* x_menu_create_menu(GtkWindow *window)
{
  char             *menu_file;
  GError           *error = NULL;
  GtkWidget        *menubar;
  GtkActionGroup   *action_group;
  GtkActionGroup   *recent_group;

  /* Create and fill the action group object */
  action_group = gtk_action_group_new("MenuActions");
  gtk_action_group_add_actions(action_group, actions, G_N_ELEMENTS(actions), NULL);
  gtk_action_group_add_toggle_actions (action_group, toggle_entries, G_N_ELEMENTS (toggle_entries), window);

  recent_group = x_menu_create_recent_action_group();
  /* Create the UI manager object */
  menu_manager  = gtk_ui_manager_new();

  gtk_ui_manager_insert_action_group(menu_manager, action_group, 0);
  gtk_ui_manager_insert_action_group(menu_manager, recent_group, 0);
  
  menu_file = g_build_filename(s_path_sys_data (), "gattrib-menus.xml", NULL);

  gtk_ui_manager_add_ui_from_file(menu_manager, menu_file, &error);
  if(error != NULL) {
    /* An error occured, terminate */
    fprintf(stderr, _("Error loading %s:\n%s\n"), menu_file, error->message);
    exit(1);
  }

  g_free(menu_file);

  gtk_window_add_accel_group (window, gtk_ui_manager_get_accel_group(menu_manager));

  menubar = gtk_ui_manager_get_widget(menu_manager, "/ui/menubar/");
  if (menubar == NULL)
    fprintf(stderr, "ERROR: GTK function failed to return Menu object\n");
  return menubar; /* WEH: Does this really get saved? */
}


