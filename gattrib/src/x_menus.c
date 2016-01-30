/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
#include <gtk/gtk.h>
#include <gattrib.h>

#include <geda_help.h>   /* Include file names for HTML files */
#include <geda_debug.h>

static void toolbar_icons_only( void );
static void toolbar_text_only( void );
static void toolbar_display_both( void );

/*!------------------------ Menus And Toolbars ---------------------*/
/*!
 * \brief Save File
 * \par Function Description
 * This is a menu callback to implement the File->Save menu options. This
 * function can also be called by the toolbar button handler.
 */
void x_menu_file_save()
{
  s_toplevel_gtksheet_to_toplevel(pr_current);   /* Dumps sheet data into GedaToplevel */
  s_page_save_all(pr_current);                   /* saves all pages in design */
  sheet_head->CHANGED = FALSE;                   /* reset the status flag */
  x_window_update_title(pr_current, sheet_head); /* remove the asterisk from title */
}

/*!
 * \brief File Save As menu
 * \par Function Description
 * This is a menu callback to implement the File->SaveAs menu options. This
 * function can also be called by the toolbar button handler.
 */
void x_menu_file_save_as()
{
  char *filename = malloc(MAX_FILE * sizeof(char)); /* be 255 * 1 */

  if (filename) {
    if (x_fileselect(filename)) {
      /* Dumps sheet data into GedaToplevel */
      s_toplevel_gtksheet_to_toplevel(pr_current);
      /* replace page filename with new one, do not free filename */
      GEDA_FREE (pr_current->page_current->filename);
      pr_current->page_current->filename = filename;
      s_page_save_all(pr_current);
      /* reset the changed flag of current sheet*/
      sheet_head->CHANGED = FALSE;
      x_window_update_title(pr_current, sheet_head);
    } /* else user aborted, do nothing */
  }
  else
    fprintf(stderr, "gattrib file_save_as: Memory allocation error\n");
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
	case GEDA_RESPONSE_CANCEL:
	  return; /* user canceled from the save unsaved file dialog */
	case GEDA_RESPONSE_YES:
           x_menu_file_save();
        case GEDA_RESPONSE_NO:
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
    x_window_add_items(sheet_head); /* updates toplevel & GtkSheet */
    x_window_update_title(pr_current, sheet_head);
  }
#ifdef DEBUG
    fprintf(stderr, "open file canceled:%s\n", (char*) g_slist_nth_data(file_list,0));
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
    case GEDA_RESPONSE_CANCEL:
      return; /* user canceled from the save unsaved file dialog */
    case GEDA_RESPONSE_YES:
      x_menu_file_save();
    case GEDA_RESPONSE_NO:
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
  x_window_add_items(sheet_head); /* updates toplevel & GtkSheet */
  x_window_update_title(pr_current, sheet_head);
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
void x_menu_edit_new_attrib()
{
  s_toplevel_add_new_attrib(-1);
}

/*!
 * \brief Edit->Delete Attribute menu item
 *
 * Implements the Delete Attribute menu item
 *
 * \TODO Implement the function that Implements Delete Attribute
 */
void x_menu_edit_delete_attrib()
{
  x_dialog_unimplemented_feature();
  return;
  GtkSheet *sheet = x_gtksheet_get_current_sheet();

  if (sheet->state == GTK_SHEET_COLUMN_SELECTED)
    s_toplevel_delete_attrib_col(sheet);
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

/* Handlers for Help menu */
static void menu_help_manual()
{
  i_show_wiki_help (HELP_GATTRIB_GUIDE_HTML);
}
static void menu_help_faq()
{
  i_show_wiki_help (HELP_GATTRIB_FAQ_HTML);
}

static void menu_help_geda()
{
  i_show_wiki_help (HELP_GEDA_DOC_HTML);
}
static void menu_help_wiki()
{
  i_show_wiki_help (HELP_GEDA_WIKI_HTML);
}

static void menu_help_attribs()
{
  i_show_wiki_help (HELP_ATTRIBUTES_HTML);
}
static void menu_help_glossary()
{
  i_show_wiki_help (HELP_GEDA_GLOSSARY_HTML);
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
  gtk_toolbar_set_style (GTK_TOOLBAR (Attribute_Toolbar), GTK_TOOLBAR_ICONS);
}
/*!
 * \brief View Toolbar Text
 * \par Function Description
 *
 */
static void toolbar_text_only( void )
{
  gtk_toolbar_set_style (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_TEXT);
  gtk_toolbar_set_style (GTK_TOOLBAR (Attribute_Toolbar), GTK_TOOLBAR_TEXT);
}
/*!
 * \brief View Toolbar Icons & Text
 * \par Function Description
 *
 */
static void toolbar_display_both( void )
{
  gtk_toolbar_set_style (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_BOTH);
  gtk_toolbar_set_style (GTK_TOOLBAR (Attribute_Toolbar), GTK_TOOLBAR_BOTH);
}

/* ----------- Recent Files Menu Stuff ----------- */
#define MAX_RECENT_FILES 10
/*! \brief Callback for recent-chooser.
 *  \par Function Description
 * This function is called if an element of recent-file-list
 * is selected.
 */
static void on_recent_selection (GtkRecentChooser *chooser)
{
  char *uri;
  char *filename;

  uri = gtk_recent_chooser_get_current_uri (chooser);
  filename = g_filename_from_uri(uri, NULL, NULL);
  gtk_recent_manager_add_item(recent_manager, uri);

  menu_open_recent(filename);

  GEDA_FREE(uri);
  GEDA_FREE(filename);
}

/*! \brief Set Senitivity of Menu Items.
 *  \par Function Description
 *       This function is called by on_notebook_switch_page when ever a TAB
 *       is selected, passing a gslist of menu widget items to be set to the
 *       specified sensitivity
 */
void x_menus_set_sensitivities(GSList *ListMenuItems, int sensitive)
{
    lambda (GtkWidget *menu_item)
    {
      gtk_widget_set_sensitive(menu_item, sensitive);
      return FALSE;
    }
    mapcar(ListMenuItems);
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
  { "file-export-csv", GTK_STOCK_CONVERT, "Export CSV", "", "", x_menu_file_export_csv},
  /* { "file-print", GTK_STOCK_PRINT, "Print", "<Control>P", "", x_dialog_unimplemented_feature}, */
  { "file-quit", GTK_STOCK_QUIT, "Quit", "<Control>Q", "", G_CALLBACK(gattrib_really_quit)},

  /* Edit menu */
  { "edit", NULL, "_Edit"},
  { "edit-clipboard-cut", GTK_STOCK_CUT, "Cut", "<Control>X", "", menu_edit_cut},
  { "edit-clipboard-copy", GTK_STOCK_COPY, "Copy", "<Control>C", "", menu_edit_copy},
  { "edit-clipboard-paste", GTK_STOCK_PASTE, "Paste", "<Control>V", "", menu_edit_paste},

  { "edit-add-attrib",    GTK_STOCK_ADD, "_Add new Attribute", "", "", x_menu_edit_new_attrib},
  { "edit-delete-attrib", GTK_STOCK_DELETE, "D_elete Attribute",  "", "", x_menu_edit_delete_attrib},
  { "edit-promote-attrib", NULL, "_Promote Attribute", "", "Attach the selected attribute",  s_properties_promote_attribute},
  { "edit-demote-attrib",  NULL, "_Demote Attribute",  "", "Dettach the selected attribute", s_properties_demote_attribute},

  { "edit-find-value", GTK_STOCK_FIND, "Find Value", "<Control>F", "Find attribute value", x_find_attribute_value},
  { "edit-search-replace-value", GTK_STOCK_FIND_AND_REPLACE, "Replace value", "<Control>R", "Search and Replace Attribute value", x_find_replace_attrib_value},
  { "edit-locate-attrib", GTK_STOCK_FIND, "Locate Attribute", "", "Search for an attribute", x_find_attribute},
  { "edit-locate-refdes", NULL, "Locate Reference", "", "Search for reference designator", x_find_refdes},

  /* View menu */
  { "view", NULL, "_View"},
  { "toolbar", NULL, "_Toolbar"},
  { "view-toolbar-icons", NULL, "_Icons", NULL, "Display Icons on the toolbar", toolbar_icons_only},
  { "view-toolbar-text", NULL,  "_Text",  NULL, "Display Text on the toolbar", toolbar_text_only},
  { "view-toolbar-both", NULL,  "_Both",  NULL, "Display Icons and Text on the toolbar", toolbar_display_both},

  /* Visibility menu */
  { "visibility", NULL, "_Visibility"},
  { "visibility-invisible",  NULL, "Set selected invisible", "", "", s_properties_set_invisible},
  { "visibility-visible",    NULL, "Set selected visible",   "", "", s_properties_set_visible},
  { "visibility-name-only",  NULL, "Set selected name visible only",   "", "", s_properties_set_name_only},
  { "visibility-value-only", NULL, "Set selected value visible only", "", "", s_properties_set_value_only},
  { "visibility-name-value", NULL, "Set selected name and value visible", "", "", s_properties_set_name_and_value},

  { "window", NULL, "_Window"},
  /* Help menu */
  { "help", NULL, "_Help"},
  { "help-show-manual",   GTK_STOCK_HELP,   "Gattrib Guide",          "", "", menu_help_manual},
  { "help-show-faq",      GTK_STOCK_INFO ,  "Gattrib FAQ",            "", "", menu_help_faq},
  { "help-show-geda",     GTK_STOCK_INFO ,  "gEDA Docu_mentation...", "", "", menu_help_geda},
  { "help-show-wiki",     GTK_STOCK_INFO ,  "gEDA _Wiki...",          "", "", menu_help_wiki},

  { "help-show-attribs",  GTK_STOCK_INDEX , "gEDA Attributes",        "", "", menu_help_attribs},
  { "help-show-glossary", GTK_STOCK_SPELL_CHECK ,  "gEDA Glossary",          "", "", menu_help_glossary},
  { "help-about",         GTK_STOCK_ABOUT,  "About",                  "", "", x_dialog_about_dialog},
};

  /* name, stock-id, label, accelerator, tooltip, callback function */
/* Toggle items */
static const GtkToggleActionEntry toggle_entries[] = {
/* View menu */
  { "view-editbar",           "", "_Edit bar", "",  "Display the Edit Status bar", G_CALLBACK(x_window_editbar_toggle), TRUE },
  { "view-toolbar-standard",  "", "_Standard", "",  "Display Standard Toolbar", G_CALLBACK(x_window_standard_toolbar_toggle), TRUE },
  { "view-toolbar-attribute", "", "A_ttribute", "", "Display Attribute Toolbar", G_CALLBACK(x_window_attribute_toolbar_toggle), TRUE },
  { "view-attached-attribs",  "", "_Attached", "",  "Hide or unhide non-attached attributes", G_CALLBACK(x_window_attached_toggle), FALSE },
  { "view-inherited-attribs", "", "_Inherited", "", "Hide or unhide inherited attributes", G_CALLBACK(x_window_inherited_toggle), TRUE},
/* Window menu */
  { "window-auto-resize", "", "_Autoresize", "", "Enable/Disable Auto resize columns", G_CALLBACK(x_window_autoresize_toggle), FALSE },
  { "window-auto-scroll", "", "Auto_Scroll", "", "Enable/Disable AutoScroll", G_CALLBACK(x_window_autoscroll_toggle), TRUE },
  { "window-show_grid",   "", "Show _Grid", "", "Toggle grid visibility", G_CALLBACK(x_window_grid_toggle), TRUE },
};

/*! \brief Create geda file filter
 *  \par Function Description
 *  This function creates a new files and setups the mine type and extensions
 *  for the Open Recent sub-sytstem.
 */
GtkRecentFilter *x_menu_geda_filter() {

  GtkRecentFilter *geda_filter;

  geda_filter = gtk_recent_filter_new();
  gtk_recent_filter_add_mime_type(geda_filter, "application/x-geda-schematic");
  gtk_recent_filter_add_mime_type(geda_filter, "application/x-geda-symbol");
  gtk_recent_filter_add_pattern(geda_filter, "*.sch");
  gtk_recent_filter_add_pattern(geda_filter, "*.sym");

  return geda_filter;
}

/*! \brief Rename gtk_recent_chooser_menu_new_for_manager
 *  \par Function Description
 *  The name of this GTK function is way too long, and the names chosen for
 *  data type don't help, so we rename here using something more reasonable.
 */
GtkRecentChooser *GetRecentMenuChooser(GtkRecentManager *rm )  {
  GtkWidget *rc = gtk_recent_chooser_menu_new_for_manager(rm );
  return (GtkRecentChooser*) rc;
}

/*! \brief Fix the GTK 'Open Recent' Menu Option
 *  \par Function Description
 *  gtk_ui_manager deliberately setups up the Recent Menu, in the presents
 *  of a recent manager with chooser when XML menu data is used. The setup
 *  is a generic one without filters and ALL of the GTK functions intended
 *  to setup the recent chooser become unresponsive and ignore settings.
 *  The GTK solution will likely be to depreciated the defective functions
 *  rather than fix them. The solution here is to remove the container and
 *  create a new recent_manager and setup manually as would normally be done
 *  if the XML menu data had not used.
 */
void x_menu_fix_gtk_recent_submenu(void) {

  GtkWidget        *menu_files;
  GtkWidget        *recent_items;         /* Be the ones GTK errently added */
  GtkRecentFilter  *recent_filter;
  GtkRecentChooser *recent_file_chooser;

  recent_items =  gtk_ui_manager_get_widget (menu_manager,"/ui/menubar/file/OpenRecent");

  if (recent_manager == NULL) {
     GtkContainer *menu = GTK_CONTAINER (gtk_widget_get_parent (recent_items));
     gtk_container_remove (menu, recent_items);
     return;
  }

  menu_files = gtk_recent_chooser_menu_new_for_manager (recent_manager);

  recent_filter = x_menu_geda_filter ();
  recent_file_chooser = (GtkRecentChooser*)menu_files;
  gtk_recent_chooser_add_filter (recent_file_chooser, recent_filter);
  gtk_recent_chooser_set_show_tips (recent_file_chooser, TRUE);
  gtk_recent_chooser_set_sort_type (recent_file_chooser, GTK_RECENT_SORT_MRU);
  gtk_recent_chooser_set_limit(recent_file_chooser, MAX_RECENT_FILES);
  gtk_recent_chooser_set_local_only(recent_file_chooser, FALSE);
  gtk_recent_chooser_menu_set_show_numbers(GTK_RECENT_CHOOSER_MENU(menu_files), TRUE);

  gtk_menu_item_set_submenu (GTK_MENU_ITEM (recent_items), menu_files);

  GEDA_SIGNAL_CONNECT (menu_files, "selection-done",
                       G_CALLBACK (on_recent_selection), main_window);
  return;
}

/*! \brief Attach a submenu with filenames to the 'Open Recent'
 *  \par Function Description
 *  Called from x_window_init function to attach the Open Recent
 * option under the File menu.
 */
GtkActionGroup *x_menu_create_recent_action_group(void) {

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
  GEDA_SIGNAL_CONNECT (recent_action, "item-activated",
                       G_CALLBACK (on_recent_selection),
                       NULL);

  return recent_action_group;
}

/*! \brief Compile List of Menu Widgets.
 *  \par Function Description
 *       This function is called after the xml menu is constructed in
 *       x_menu_create_menu in order to collect a list of pointers to
 *       menu items. The list is used for convenience to later set
 *       sensitivities so the on_notebook_switch_page callback won't
 *       have to waste time looking up each widget when users switch
 *       between Sheets/Tabs.
 */
static void x_menu_get_collections (GtkUIManager *ui_man) {

  ComponentMenuItems = NULL;
  GtkWidget *item;

  item = gtk_ui_manager_get_widget(ui_man, "/menubar/edit/edit-add-attrib");
  ComponentMenuItems = g_slist_append(ComponentMenuItems, item);
  item = gtk_ui_manager_get_widget(ui_man, "/menubar/edit/edit-delete-attrib");
  ComponentMenuItems = g_slist_append(ComponentMenuItems, item);
  item = gtk_ui_manager_get_widget(ui_man, "/menubar/edit/edit-promote-attrib");
  ComponentMenuItems = g_slist_append(ComponentMenuItems, item);
  item = gtk_ui_manager_get_widget(ui_man, "/menubar/edit/edit-demote-attrib");
  ComponentMenuItems = g_slist_append(ComponentMenuItems, item);
  item = gtk_ui_manager_get_widget(ui_man, "/menubar/visibility/visibility-name-only");
  ComponentMenuItems = g_slist_append(ComponentMenuItems, item);
  item = gtk_ui_manager_get_widget(ui_man, "/menubar/visibility/visibility-value-only");
  ComponentMenuItems = g_slist_append(ComponentMenuItems, item);
  item = gtk_ui_manager_get_widget(ui_man, "/menubar/visibility/visibility-name-value");
  ComponentMenuItems = g_slist_append(ComponentMenuItems, item);

}

/*! \brief Create and attach the menu bar
 *  \par Function Description
 * Create the menu bar and attach it to the main window.
 *
 *  First, the GtkActionGroup object is created and filled with
 *  entries of type GtkActionEntry (each entry specifies a single
 *  action, such as opening a file). Then the GtkUIManager object
 *  is created and used to load menus.xml file with the menu
 *  description. Finally, the GtkAccelGroup is added to the
 *  main window to enable keyboard accelerators and a pointer
 *  to the menu bar is retrieved from the GtkUIManager object.
 *
 * \param main_window Window to add the menubar to
 *
 * \return menubar The Created menubar
 */
/*
 * 11/05/12 WEH Revised to include toggle_actions group
 * 11/10/12 WEH Revised to include recent_group
 */
GtkWidget* x_menu_create_menu(GtkWindow *main_window)
{
  char            *menu_file;
  GError          *error = NULL;
  GtkWidget       *menubar;
  GtkActionGroup  *action_group;
  GtkActionGroup  *recent_group;

  /* Create and fill the action group object */
  action_group = gtk_action_group_new("MenuActions");
  gtk_action_group_add_actions(action_group, actions, G_N_ELEMENTS(actions), NULL);
  gtk_action_group_add_toggle_actions (action_group, toggle_entries, G_N_ELEMENTS (toggle_entries), main_window);

  recent_group = x_menu_create_recent_action_group();
  /* Create the UI manager object */
  menu_manager  = gtk_ui_manager_new();

  gtk_ui_manager_insert_action_group(menu_manager, action_group, 0);
  gtk_ui_manager_insert_action_group(menu_manager, recent_group, 0);

  menu_file = g_build_filename(f_path_sys_data (), "gattrib-menus.xml", NULL);

  gtk_ui_manager_add_ui_from_file(menu_manager, menu_file, &error);
  if(error != NULL) {
    /* An error occured, terminate */
    fprintf(stderr, _("Error loading %s:\n%s\n"), menu_file, error->message);
    exit(1);
  }

  GEDA_FREE(menu_file);

  gtk_window_add_accel_group (main_window, gtk_ui_manager_get_accel_group(menu_manager));

  menubar = gtk_ui_manager_get_widget(menu_manager, "/ui/menubar/");

  x_menu_get_collections(menu_manager);

  if (menubar == NULL)
    fprintf(stderr, "ERROR: GTK function failed to return Menu object\n");
  return menubar; /* WEH: Does this really get saved? */
}
