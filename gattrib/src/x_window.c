/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2012 Stuart D. Brorson.
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

/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions for the toplevel window
 *
 * This file holds functions used to handle the toplevel window and
 * various widgets held by that window.  Widges used to handle
 * (GtkSheet *sheet) itself are held in a different file.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/*------------------------------------------------------------------
 * Includes required to run graphical widgets.
 *------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <gtk/gtk.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <glib-object.h>
#include <gtksheet.h>

#include <geda_toolbars.h>

#include <gattrib.h>  /* include Gattrib specific headers  */

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*------------------------------------------------------------------
 * Gattrib specific defines
 *------------------------------------------------------------------*/
#define GATTRIB_THEME_ICON_NAME "geda-gattrib"

static ToolbarStringData ToolbarStrings[] = {
  { "open_button", 	"Open", 	"Open file", "Private"},
  { "save_button", 	"Save",		"Save file", "Private"},
  { "save_as_button", 	"Save As", 	"Save the file to different name or location", "Private"},
  { "cut_button", 	"Cut", 		"Cut selection to the clipboard", "Private"},
  { "copy_button",  	"Copy", 	"Copy selection to the clipboard", "Private"},
  { "paste_button",	"Paste",	"Paste selection from the clipboard", "Private"},
  { NULL, NULL, NULL},
};

/*! \brief Set application icon
 *
 * Setup default icon for GTK windows
 *
 *  Sets the default window icon by name, to be found in the current icon
 *  theme. The name used is #defined above as GATTRIB_THEME_ICON_NAME.
 */
static void x_window_set_default_icon( void )
{
  gtk_window_set_default_icon_name( GATTRIB_THEME_ICON_NAME );
}

/*! \brief Set Window tiltle */
/* TODO: Need to add astericl if file is changed */
static void x_window_set_title( void )
{
    gtk_window_set_title(GTK_WINDOW(window), _("gattrib -- gEDA attribute editor")); 
}

/*! \brief Handle Cut, Copy, Paste for Menus and Toolbar */
void x_window_clipboard_handler(int do_what) {
  
  GtkWidget *widget = gtk_window_get_focus(GTK_WINDOW(window));
  switch (do_what ) {
    case cut:
      if(GTK_IS_LABEL(widget) && gtk_label_get_selectable(GTK_LABEL(widget)))
        g_signal_emit_by_name(widget, "copy-clipboard", NULL); /* just copy */
      else if(GTK_IS_ENTRY(widget) || GTK_IS_TEXT_VIEW(widget))
        g_signal_emit_by_name(widget, "cut-clipboard", NULL);
      break;
    case copy:
      //gtk_text_buffer_copy_clipboard(buffer, clipboard);
      if((GTK_IS_LABEL(widget) && gtk_label_get_selectable(GTK_LABEL(widget)))
          || GTK_IS_ENTRY(widget) || GTK_IS_TEXT_VIEW(widget))
        g_signal_emit_by_name(widget, "copy-clipboard", NULL);
      break;
    case paste:
      if(GTK_IS_ENTRY(widget) || GTK_IS_TEXT_VIEW(widget))
        g_signal_emit_by_name(widget, "paste-clipboard", NULL);
      break;
    default:
     s_log_message("clipboardhandler: Ignoring unknown ID [%d]\n", do_what);
  }
}

/*! \brief Redirect Cut, Copy, Paste from Toolbar to Handler function */
static void callBack_clipboard (GtkWidget *button_widget, IDS_Toolbar *Control)
{
  int button = (int)(long*) Control;
  x_window_clipboard_handler(button);
  return;
}

/*! \brief Redirect Open, Save & Save As from Toolbar to Handler functions */
static void callBack_toolbar0 (GtkWidget *widget, IDS_Toolbar *Control)
{
  int button = (int)(long*) Control;

  switch ( button ) {
    case open:
      x_menu_file_open();
      break;
    case save:
      x_menu_file_save();
      break;
    case save_as:
      x_menu_file_save_as();
      break;
    default:
     s_log_message("toolbar0(): Button ID %d\n", button);
  }

  return;
}

/*! \brief Initialises the toplevel gtksheet
 *
 * This function initializes the toplevel gtksheet stuff.
 *
 *  It basically just initializes the following widgets:
 *  GTK_WINDOW *window 
 *  GTK_CONTAINER *main_vbox
 *  GTK_MENU 
 * 
 *  Note that it doesn't display the spreadsheet itself.  This is done
 *  in x_sheet_build_sheet. I suppose I could postpone all initialization 
 *  until x_sheet_build_sheet, but I figured that I could at least do 
 *  some initialization here. In particular, the stuff to put up the 
 *  menus is long & it is worthwhile to separate it from other code.  
 *  Maybe I'll refactor this later.
 */
void x_window_init()
{
  GtkWidget *main_vbox;
  GtkRequisition request;

  GtkWidget *tmp_toolbar_icon;
 
  /* Set default icon */
  x_window_set_default_icon();

  /*  window is a global declared in globals.h.  */
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);  

  x_window_set_title();
  
  gtk_window_set_default_size(GTK_WINDOW(window), 1000, 600);  
  
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
		      GTK_SIGNAL_FUNC (gattrib_really_quit), 0);

  /* -----  Now create main_vbox container to hold everthing ----- */   
  main_vbox = gtk_vbox_new(FALSE,1);
  gtk_container_set_border_width(GTK_CONTAINER(main_vbox), 1);
  gtk_container_add(GTK_CONTAINER(window), GTK_WIDGET(main_vbox) );
  
  /* -----  Now create menu bar  ----- */  
  menu_bar = x_menu_create_menu(GTK_WINDOW(window));
  gtk_box_pack_start(GTK_BOX (main_vbox), menu_bar, FALSE, TRUE, 0);
      
  handlebox = gtk_handle_box_new ();
  gtk_box_pack_start(GTK_BOX (main_vbox), handlebox, FALSE, FALSE, 5);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and put it into our handlebox */
  Standard_Toolbar = gtk_toolbar_new ();
  gtk_toolbar_set_orientation (GTK_TOOLBAR (Standard_Toolbar),
			       GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (Standard_Toolbar), GTK_TOOLBAR_BOTH);
  gtk_container_set_border_width (GTK_CONTAINER (Standard_Toolbar), 5);
//gtk_toolbar_set_space_size (GTK_TOOLBAR (toolbar), 5);
  gtk_container_add (GTK_CONTAINER (handlebox), Standard_Toolbar);

  /* Add Open Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, open, PIX, gschem-open.xpm, callBack_toolbar0)

  /* Add Save Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, save, PIX, gschem-save.xpm, callBack_toolbar0)

  /* Add Save As Button to Toolbar */
  TOOLBAR_STD_BUTTON(Standard, save_as, STK, GTK_STOCK_SAVE_AS, callBack_toolbar0)
  
  gtk_toolbar_append_space(GTK_TOOLBAR(Standard_Toolbar));

  TOOLBAR_STD_BUTTON(Standard, cut, STK, GTK_STOCK_CUT, callBack_clipboard)
  TOOLBAR_STD_BUTTON(Standard, copy, STK, GTK_STOCK_COPY, callBack_clipboard)
  TOOLBAR_STD_BUTTON(Standard, paste, STK, GTK_STOCK_PASTE, callBack_clipboard)

  gtk_widget_show (Standard_Toolbar);
  gtk_widget_show (handlebox);
  
  edit_box=gtk_hbox_new(FALSE, 1);
  gtk_container_set_border_width(GTK_CONTAINER(edit_box),0);
  gtk_box_pack_start(GTK_BOX(main_vbox), edit_box, FALSE, TRUE, 0);
  gtk_widget_show(edit_box);

   /* This is the RC box in the top left cell */
  location=gtk_label_new(""); 
  gtk_widget_size_request(location, &request); 
  gtk_widget_set_usize(location, 150, request.height);
  gtk_box_pack_start(GTK_BOX(edit_box), location, FALSE, TRUE, 0);
  gtk_widget_show(location);

  entry=gtk_entry_new(); 
  gtk_box_pack_start(GTK_BOX(edit_box), entry, TRUE, TRUE, 0); 
  gtk_widget_show(entry);
  
  /* -----  Now init notebook widget  ----- */  
  notebook = gtk_notebook_new();
  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_BOTTOM);
  gtk_box_pack_start(GTK_BOX(main_vbox), notebook, TRUE, TRUE, 0);
  
  /* -----  Now malloc -- but don't fill out -- space for sheets  ----- */  
  /* This basically sets up the overhead for the sheets, as I understand
   * it.  The memory for the actual sheet cells is allocated later,
   * when gtk_sheet_new is invoked, I think.  */
  sheets = g_malloc0(NUM_SHEETS * sizeof(GtkWidget *));

  x_menu_fix_gtk_recent_submenu();
}

/*! \brief Add all items to the top level window
 *
 * This function updates the top level window
 *         after a new page is read in.  
 *
 *  It does the following:
 * 
 *  -# Create a new gtksheet having the current dimensions.
 *  -# Call x_gktsheet_add_row_labels(comp_count, master_*_list_head)
 *  -# Call x_gktsheet_add_col_labels(comp_attrib_count, master_*_attrib_list_head)
 *  -# Call x_gktsheet_add_row_labels(net_count, master_*_list_head)
 *  -# Call x_gktsheet_add_col_labels(net_attrib_count, master_*_attrib_list_head)
 *  -# loop on i, j -- call x_gtksheet_add_entry(i, j, attrib_value)
 *  -# Call gtk_widget_show(window) to show new window.
 */
void x_window_add_items()
{
  int i, j;
  int num_rows, num_cols;
  char *text, *error_string;
  int visibility, show_name_value;
  
  /* Do these sanity check to prevent later segfaults */
  if (sheet_head->comp_count == 0) {
    error_string = _("No components found in entire design!\nDo you have refdeses on your components?");
    x_dialog_fatal_error(error_string, 1);
  }

  if (sheet_head->comp_attrib_count == 0) {
    error_string = _("No configurable component attributes found in entire design!\nPlease attach at least some attributes before running gattrib.");
    x_dialog_fatal_error(error_string, 2);
  }

  if (sheet_head->pin_count == 0) {
    error_string = _("No pins found on any components!\nPlease check your design.");
    x_dialog_fatal_error(error_string, 3);
  }

  /*  reinitialize the gtksheet. */

  if (sheet_head->comp_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[Components]), 
			      sheet_head->comp_count,
			      sheet_head->master_comp_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[Components]), 
			      sheet_head->comp_attrib_count,
			      sheet_head->master_comp_attrib_list_head);
  }

#ifdef UNIMPLEMENTED_FEATURES
  /* This is not ready.  Need to implement net attributes */
  if (sheet_head->net_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[Nets]), 
			      sheet_head->net_count, sheet_head->master_net_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[Nets]), 
			      sheet_head->net_attrib_count, sheet_head->master_net_attrib_list_head);
  } else {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[Nets]), 1, NULL);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[Nets]), 1, NULL);
  }  
#endif

  if (sheet_head->pin_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[Pins]), 
			      sheet_head->pin_count, sheet_head->master_pin_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[Pins]), 
			      sheet_head->pin_attrib_count, sheet_head->master_pin_attrib_list_head);
  }

  /* ------ Comp sheet: put values in the individual cells ------- */
  num_rows = sheet_head->comp_count;
  num_cols = sheet_head->comp_attrib_count;
  for (i = 0; i < num_rows; i++) {
    for (j = 0; j < num_cols; j++) {
      if ( (sheet_head->component_table)[i][j].attrib_value ) { /* NULL = no entry */
	text = (char *) g_strdup( (sheet_head->component_table)[i][j].attrib_value );
	visibility = (sheet_head->component_table)[i][j].visibility;
	show_name_value = (sheet_head->component_table)[i][j].show_name_value;
	x_gtksheet_add_cell_item( GTK_SHEET(sheets[0]), i, j, (char *) text, 
				  visibility, show_name_value );
	g_free(text);
      }
    }
  }

#ifdef UNIMPLEMENTED_FEATURES
  /* ------ Net sheet: put values in the individual cells ------- */
  num_rows = sheet_head->net_count;
  num_cols = sheet_head->net_attrib_count;
  for (i = 0; i < num_rows; i++) {
    for (j = 0; j < num_cols; j++) {
      if ( (sheet_head->net_table)[i][j].attrib_value ) { /* NULL = no entry */
	text = (char *) g_strdup( (sheet_head->net_table)[i][j].attrib_value );
	visibility = (sheet_head->net_table)[i][j].visibility;
	show_name_value = (sheet_head->component_table)[i][j].show_name_value;
	x_gtksheet_add_cell_item( GTK_SHEET(sheets[1]), i, j, (char *) text,
				  visibility, show_name_value );
	g_free(text);
      }
    }
  }
#endif

  /* ------ Pin sheet: put pin attribs in the individual cells ------- */
  num_rows = sheet_head->pin_count;
  num_cols = sheet_head->pin_attrib_count;
  for (i = 0; i < num_rows; i++) {
    for (j = 0; j < num_cols; j++) {
      if ( (sheet_head->pin_table)[i][j].attrib_value ) { /* NULL = no entry */
	text = (char *) g_strdup( (sheet_head->pin_table)[i][j].attrib_value );
	/* pins have no visibility attributes, must therefore provide default. */
	x_gtksheet_add_cell_item( GTK_SHEET(sheets[2]), i, j, (char *) text, 
				  VISIBLE, SHOW_VALUE );
	g_free(text);
      }
    }
  }

  gtk_widget_show_all( GTK_WIDGET(window) );
}

/*!
 * \brief View toogle standard toolbar
 * \par Function Description
 *
 */
void x_window_standard_toolbar_toggle(GtkToggleAction *action, GtkWindow *window)
{
  bool show = gtk_toggle_action_get_active(action);
  if(show)
    gtk_widget_show(Standard_Toolbar);
  else
    gtk_widget_hide(Standard_Toolbar);
  /* TODO: WEH: save the toggle setting */
  //config_file_set_bool(PREFS_TOOLBAR_VISIBLE, show);
}

#define TOGGLE_ATTACH_X_OFFSET 100
void x_window_attached_toggle(GtkToggleAction *action, GtkWindow *window)
{
  int i, j;
  int x, y;
  char *curr_title;
  char *attached;
  bool toggle;

  GtkSheet *sheet = sheets[Components];

  bool show = gtk_toggle_action_get_active(action);
  int count = sheet->maxcol;
  
  for( i = 0; i <= count; i++) {
    if (show) {
      toggle = TRUE;
      curr_title =sheets[Components]->column[i].name;
      for(j=0; j < sheet_head->attached_attrib_count; j++) {
        attached = s_string_list_get_data_at_index(sheet_head->attached_attrib,j);
        if(strcmp(attached, curr_title) == 0) {
          toggle = FALSE;
	  break;
        }
      } /* Next j */
      if(toggle)
        gtk_sheet_column_set_visibility(sheet, i, FALSE);
    }
    else /* Make a columns visible */
      gtk_sheet_column_set_visibility(sheet, i, TRUE);
  } /* Next i */
  /* The background outside the sheet will not get updated until something
   * happens to the parent window and this will leave residual lines, (newer
   * version of gtksheet claiming fix had the same problem, so we will resize,
   * which needs to be some anyways */
  /* TODO: WEH: Determine the real x size */
  gtk_window_get_size(GTK_WINDOW(window), &x, &y);
  x =  (show) ? x - TOGGLE_ATTACH_X_OFFSET : x + TOGGLE_ATTACH_X_OFFSET;
  gtk_window_resize(GTK_WINDOW(window), x, y);
  
  /* TODO: WEH: save the toggle setting */
  //config_file_set_bool(PREFS_ATTACHED_VISIBLE, show);
}
/*!
 * \brief View toogle Statusbar
 * \par Function Description
 *
 */
/* View->Statusbar */
void x_window_editbar_toggle(GtkToggleAction *action, GtkWindow *window)
{
  bool show = gtk_toggle_action_get_active(action);
  if(show)
    gtk_widget_show(edit_box);
  else
    gtk_widget_hide(edit_box);
  //config_file_set_bool(PREFS_STATUSBAR_VISIBLE, show);
}