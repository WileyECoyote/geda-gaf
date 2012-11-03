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
 * \brief Functions to interface to the spreadsheet widget.
 *
 * This file holds functions used to handle the spreadsheet widget.
 * Much of this was hacked from testgtksheet.c starting in Jan 2004 
 * by SDB.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/*------------------------------------------------------------------
 * Includes required to run graphical widgets.
 *------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>


#include <glib.h>
#include <glib-object.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <gtkitementry.h>

#include "../include/gattrib.h"  /* include Gattrib specific headers  */

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

const char* Colors [] = { "black", "blue", "green", "red",
                          "violet", "yellow", "white", "gray" };

char EditBuffer[255];

void x_gtksheet_destroy_all(){
  int i;
  for(i=0; i<NUM_SHEETS; i++){
    if (sheets[i] !=NULL) {
      gtk_widget_destroy( (GtkWidget*) sheets[i]);
      sheets[i]=NULL;
    }
  }
}
static int popup_activated(GtkWidget *widget, gpointer data)
{
  GtkSheet *sheet;
  int cur_page;
  char *item;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet=GTK_SHEET(sheets[cur_page]);

  item = (char *)data;

  if(strcmp(item,"Add Column")==0)
    gtk_sheet_add_column(sheet,1);

  if(strcmp(item,"Add Row")==0)
    gtk_sheet_add_row(sheet,1);

  if(strcmp(item,"Insert Row")==0){
     if(sheet->state==GTK_SHEET_ROW_SELECTED)
       gtk_sheet_insert_rows(sheet,sheet->range.row0,                       
                             sheet->range.rowi-sheet->range.row0+1);
  }

  if(strcmp(item,"Insert Column")==0){
    if(sheet->state==GTK_SHEET_COLUMN_SELECTED)
      gtk_sheet_insert_columns(sheet,sheet->range.col0,                       
                               sheet->range.coli-sheet->range.col0+1);

  } 

  if(strcmp(item,"Delete Row")==0){
    if(sheet->state==GTK_SHEET_ROW_SELECTED)
      gtk_sheet_delete_rows(sheet,sheet->range.row0,
	   		    sheet->range.rowi-sheet->range.row0+1);
 }

 if(strcmp(item,"Delete Column")==0){
   if(sheet->state==GTK_SHEET_COLUMN_SELECTED)
     gtk_sheet_delete_columns(sheet,sheet->range.col0,
                              sheet->range.coli-sheet->range.col0+1);   
 } 

 if(strcmp(item,"Clear Cells")==0){
   if(sheet->state!=GTK_SHEET_NORMAL)
     gtk_sheet_range_clear(sheet, &sheet->range);
 } 

 gtk_widget_destroy(popup);
 return TRUE;
}

static GtkWidget *
build_menu(GtkWidget *sheet) {

  static char *items[]={ "Add Column",
		         "Add Row",
		         "Insert Row",
		         "Insert Column",
		         "Delete Row",
                         "Delete Column",
                         "Clear Cells"
  };
  
  GtkWidget *menu;
  GtkWidget *item;
  int i;

  menu=gtk_menu_new();

  for (i=0; i < (sizeof(items)/sizeof(items[0])) ; i++) {
    item=gtk_menu_item_new_with_label(items[i]);

    gtk_signal_connect(GTK_OBJECT(item),"activate",
		      (GtkSignalFunc) popup_activated,
		       items[i]);

    GTK_WIDGET_SET_FLAGS (item, GTK_SENSITIVE | GTK_CAN_FOCUS);
    switch(i){
      case 2:
        GTK_WIDGET_UNSET_FLAGS (item, GTK_SENSITIVE | GTK_CAN_FOCUS);
        break;
      case 3:
        if(GTK_SHEET(sheet)->state!=GTK_SHEET_COLUMN_SELECTED)
           GTK_WIDGET_UNSET_FLAGS (item, GTK_SENSITIVE | GTK_CAN_FOCUS);
        break;
      case 4:
        if(GTK_SHEET(sheet)->state!=GTK_SHEET_ROW_SELECTED)
           GTK_WIDGET_UNSET_FLAGS (item, GTK_SENSITIVE | GTK_CAN_FOCUS);
        break;
      case 5:
        if(GTK_SHEET(sheet)->state!=GTK_SHEET_COLUMN_SELECTED)
           GTK_WIDGET_UNSET_FLAGS (item, GTK_SENSITIVE | GTK_CAN_FOCUS);
        break;
    } 

    gtk_widget_show(item);
    gtk_menu_append(GTK_MENU(menu),item);
  }
  return menu;
}

int do_popup(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
   GdkModifierType mods;
   GtkWidget *sheet;

   sheet=GTK_WIDGET(widget);

   gdk_window_get_pointer (sheet->window, NULL, NULL, &mods);
   if(mods&GDK_BUTTON3_MASK){ 

    if(popup)
       g_free(popup);

    popup=build_menu(sheet);

    gtk_menu_popup(GTK_MENU(popup), NULL, NULL, NULL, NULL,
		   event->button, event->time);
   }

   return FALSE;
}

int clipboard_handler(GtkWidget *widget, GdkEventKey *key)
{
  GtkSheet *sheet;

  sheet = GTK_SHEET(widget);

  if(key->state & GDK_CONTROL_MASK || key->keyval==GDK_Control_L ||
     key->keyval==GDK_Control_R){
    if((key->keyval=='c' || key->keyval == 'C') && sheet->state != GTK_STATE_NORMAL){
            if(gtk_sheet_in_clip(sheet)) gtk_sheet_unclip_range(sheet);
            gtk_sheet_clip_range(sheet, &sheet->range);
/*            gtk_sheet_unselect_range(sheet);
*/
    }
    if(key->keyval=='x' || key->keyval == 'X')
            gtk_sheet_unclip_range(sheet);    
  }

  return FALSE;
}

void 
resize_handler(GtkWidget *widget, GtkSheetRange *old_range, 
                                  GtkSheetRange *new_range, 
                                  gpointer data)
{
  printf("OLD SELECTION: %d %d %d %d\n",old_range->row0, old_range->col0,
                                    old_range->rowi, old_range->coli);
  printf("NEW SELECTION: %d %d %d %d\n",new_range->row0, new_range->col0,
                                    new_range->rowi, new_range->coli);
}

void move_handler(GtkWidget *widget, GtkSheetRange *old_range,
		  GtkSheetRange *new_range, gpointer data)
{
  printf("OLD SELECTION: %d %d %d %d\n",old_range->row0, old_range->col0,
                                    old_range->rowi, old_range->coli);
  printf("NEW SELECTION: %d %d %d %d\n",new_range->row0, new_range->col0,
                                    new_range->rowi, new_range->coli);
}

bool change_entry(GtkWidget *widget, 
		  gint row, gint col, gint *new_row, gint *new_col,
                  gpointer data)
{
  GtkSheet *sheet;

  sheet = GTK_SHEET(widget);

  if(*new_col == 0 && (col != 0 || sheet->state != GTK_STATE_NORMAL))
         gtk_sheet_change_entry(sheet, gtk_combo_get_type());

  if(*new_col == 1 && (col != 1 || sheet->state != GTK_STATE_NORMAL))
         gtk_sheet_change_entry(sheet, GTK_TYPE_ENTRY);

  if(*new_col == 2 && (col != 2 || sheet->state != GTK_STATE_NORMAL))
         gtk_sheet_change_entry(sheet, GTK_TYPE_SPIN_BUTTON);

  if(*new_col >= 3 && (col < 3 || sheet->state != GTK_STATE_NORMAL))
         gtk_sheet_change_entry(sheet, GTK_TYPE_CELL_EDITABLE);

  return TRUE;
}

void cell_change(GtkWidget *widget, gint row, gint col, 
                  gpointer data)
{

}

bool cell_activate(GtkWidget *widget, int row, int col, 
                    gpointer data)
{
  char *celltext;

  celltext = gtk_sheet_cell_get_text((GtkSheet*)widget, row, col);

  if ( NULL != celltext) {
    /* Make a copy of the contents */
    strcpy(EditBuffer, celltext);
  }
/*
 GtkSheetRange range;
 range.row0 = range.rowi = row;
 range.col0 = range.coli = col;

 gtk_sheet_range_set_justification(GTK_SHEET(widget), &range, GTK_JUSTIFY_LEFT);
*/

 return TRUE;
}

bool cell_deactivate(GtkWidget *widget, int row, int col, gpointer data)
{
  char *celltext;

  celltext = gtk_sheet_cell_get_text((GtkSheet*)widget, row, col);
  if (EditBuffer != NULL) { /* If NULL then we're loading data from file */
    if (celltext != NULL) { /* If NULL then cell was empty */
      if ( strcmp(EditBuffer, celltext) != 0) {
        sheet_head->CHANGED = TRUE;
      }
    }
  }
  /*
  GtkSheetRange range;

  GtkSheet *sheet = GTK_SHEET(widget);

  range.row0 = range.rowi = row;
  range.col0 = range.coli = col;

 gtk_sheet_range_set_justification(GTK_SHEET(widget), &range, GTK_JUSTIFY_RIGHT);

  text = g_strdup(gtk_sheet_cell_get_text(GTK_SHEET(widget), row, col));

  if(text && strlen(text) > 0){
    gtk_sheet_set_cell_text(sheet, row, col, text);
    g_free(text); 
  }
*/

 return TRUE;
}
bool alarm_traverse(GtkWidget *widget, 
                    int row, int col, int *new_row, int *new_col,
                    gpointer data)
{
 printf("TRAVERSE: %d %d %d %d\n",row,col,*new_row,*new_col);
 return TRUE;
}

void SetupCSheetHandlers(GtkSheet *sheet)
{
  
  GtkObject *SheetObj;
  SheetObj = GTK_OBJECT(sheet);

  gtk_signal_connect(SheetObj,
                    "activate",
                    (GtkSignalFunc) cell_activate, 
                     NULL);
  
  gtk_signal_connect(SheetObj,
                     "deactivate",
                     (GtkSignalFunc) cell_deactivate, 
                     NULL);
  return;

  gtk_signal_connect(SheetObj,
                    "changed", /* or just clicked on */
                    (GtkSignalFunc) cell_change, 
                    NULL); 
 
  gtk_signal_connect(SheetObj,
                     "key_press_event",
                     (GtkSignalFunc) clipboard_handler, 
                     NULL);

 gtk_signal_connect(SheetObj,
                    "resize_range",
                    (GtkSignalFunc) resize_handler, 
                    NULL);

 gtk_signal_connect(SheetObj,
                    "move_range",
                    (GtkSignalFunc) move_handler, 
                    NULL);

 gtk_signal_connect(SheetObj,
                    "traverse",
                    (GtkSignalFunc) alarm_traverse, 
                    NULL);
}

void
set_cell(GtkWidget *widget, char *insert, int text_length, int position, 
         gpointer data)
{
  const char *text;
  GtkEntry *sheet_entry;

  sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(GTK_SHEET(widget)));

  if((text = gtk_entry_get_text (sheet_entry))) {
                          gtk_entry_set_text(GTK_ENTRY(entry), text);
  }
  GTK_WIDGET_UNSET_FLAGS(entry, GTK_HAS_FOCUS);
  GTK_WIDGET_SET_FLAGS(GTK_SHEET(widget)->sheet_entry, GTK_HAS_FOCUS);
} 

void show_sheet_entry(GtkWidget *widget, gpointer data)
{
 const char *text;
 GtkSheet *sheet;
 GtkEntry *sheet_entry;
 int cur_page;

 if(!GTK_WIDGET_HAS_FOCUS(widget)) return;

 cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
 sheet=GTK_SHEET(sheets[cur_page]);
 sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(sheet));

 if((text=gtk_entry_get_text (GTK_ENTRY(entry)))){
   gtk_entry_set_text(sheet_entry, text);
 }
}

void activate_sheet_entry(GtkWidget *widget, gpointer data)
{
  GtkSheet *sheet;
  GtkEntry *sheet_entry;
  int cur_page, row, col;
  int justification=GTK_JUSTIFY_LEFT;
  
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet=GTK_SHEET(sheets[cur_page]);
  row=sheet->active_cell.row; col=sheet->active_cell.col;

  sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(sheet));

  if(GTK_IS_ITEM_ENTRY(sheet_entry))
         justification = GTK_ITEM_ENTRY(sheet_entry)->justification;

  gtk_sheet_set_cell(sheet, row, col, justification,
		     gtk_entry_get_text (sheet_entry));

}

void show_entry(GtkWidget *widget, gpointer data)
{
 const char *text; 
 GtkSheet *sheet;
 GtkWidget * sheet_entry;
 int cur_page;

 if(!GTK_WIDGET_HAS_FOCUS(widget)) return;

 cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
 sheet=GTK_SHEET(sheets[cur_page]);
 sheet_entry = gtk_sheet_get_entry(sheet);

 if((text=gtk_entry_get_text (GTK_ENTRY(sheet_entry))))
			  gtk_entry_set_text(GTK_ENTRY(entry), text);

}

int activate_sheet_cell(GtkWidget *widget, int row, int column, gpointer data) 
{
  GtkSheet *sheet;
  GtkEntry *sheet_entry;
  char cell[100];
  const char *text;
  GtkSheetCellAttr attributes;

  sheet=GTK_SHEET(widget);
  sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(sheet));

  if(GTK_SHEET(widget)->column[column].name)
   sprintf(cell,"  %s:%d  ",GTK_SHEET(widget)->column[column].name, row);
  else
   sprintf(cell, "R:%d C: %d", row, column);

  gtk_label_set(GTK_LABEL(location), cell);

  gtk_entry_set_max_length(GTK_ENTRY(entry),
	GTK_ENTRY(sheet_entry)->text_max_length);

  if((text=gtk_entry_get_text(GTK_ENTRY(gtk_sheet_get_entry(sheet)))))
			  gtk_entry_set_text(GTK_ENTRY(entry), text);
  else
			  gtk_entry_set_text(GTK_ENTRY(entry), "");

  gtk_sheet_get_attributes(sheet,sheet->active_cell.row,
				sheet->active_cell.col, &attributes);

  gtk_entry_set_editable(GTK_ENTRY(entry), attributes.is_editable);

  return TRUE;
}

/*! \brief Create the GtkSheet
 *
 * Creates and initializes the GtkSheet widget, which is the
 * spreadsheet widget used for displaying the data.
 * 
 */
void x_gtksheet_init()
{
  int i;
  char *SheetNames[]= { "Components",  "Nets", "Pins"};

  /* ---  Create three new sheets.   were malloc'ed in x_window_init  --- */

  /* -----  Components  ----- */
  if ((sheet_head->comp_count > 0) && (sheet_head->comp_attrib_count >0)) {
    sheets[Components] = (GtkSheet *) gtk_sheet_new( sheet_head->comp_count,
					    sheet_head->comp_attrib_count,
					   _(SheetNames[Components]));
  } else {
    x_dialog_fatal_error(_("No components found in design.  Please check your schematic and try again!\n"), 1);
  }

  /* -----  Nets  ----- */
  if ((sheet_head->net_count > 0) && (sheet_head->net_attrib_count >0)) {
    sheets[Nets] = (GtkSheet *) gtk_sheet_new(sheet_head->net_count, sheet_head->net_attrib_count, _(SheetNames[Nets]));
    gtk_sheet_set_locked(GTK_SHEET(sheets[Nets]), TRUE);   /* disallow editing of attribs for now */

  }
  else {
    sheets[Nets] = (GtkSheet *) gtk_sheet_new(1, 1, _(SheetNames[Nets]));
    gtk_sheet_row_button_add_label(sheets[Nets], 0, _("TBD"));
    gtk_sheet_row_button_justify(sheets[Nets], 0, GTK_JUSTIFY_LEFT);
    gtk_sheet_column_button_add_label(sheets[Nets], 0, _("TBD"));
    gtk_sheet_column_button_justify(sheets[Nets], 0, GTK_JUSTIFY_LEFT);
    gtk_sheet_set_locked(GTK_SHEET(sheets[Nets]), TRUE);   // disallow editing of attribs for now 
  }

  /* -----  Pins  ----- */
  if ((sheet_head->pin_count > 0) && (sheet_head->pin_attrib_count >0)) {
    sheets[Pins] = (GtkSheet *) gtk_sheet_new(sheet_head->pin_count, sheet_head->pin_attrib_count, _(SheetNames[Pins]));
  }
  else {
    sheets[Pins] = (GtkSheet *) gtk_sheet_new(1, 1, _(SheetNames[Pins]));
  }
  
  /* --- Finally stick labels on the notebooks holding the sheets. --- */
  for(i=0; i<NUM_SHEETS; i++){
    if (sheets[i] != NULL) {  /* is this check needed? 
			       * Yes, it prevents us from segfaulting on empty nets sheet. */
      scrolled_windows=(GtkWidget **)realloc(scrolled_windows, (i+1)*sizeof(GtkWidget *));
      scrolled_windows[i]=gtk_scrolled_window_new(NULL, NULL);
      
      gtk_container_add( GTK_CONTAINER(scrolled_windows[i]), GTK_WIDGET(sheets[i]) );

      /* First remove old notebook page. Maybe should probably do some checking here. */
      if (notebook != NULL) 
	gtk_notebook_remove_page(GTK_NOTEBOOK(notebook), i);

      /* Then add new, updated notebook page */
      label= gtk_label_new(_(SheetNames[i]));

      gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			       GTK_WIDGET(scrolled_windows[i]),
			       GTK_WIDGET(label) );

      sheets[i]->autoresize=FALSE;
	    
      gtk_widget_show( GTK_WIDGET(sheets[i]) );
      gtk_widget_show( GTK_WIDGET(scrolled_windows[i]) );
      gtk_widget_show( GTK_WIDGET(notebook) );  /* show updated notebook  */     
     
      /*  The entry cell is the text entry field is the one at the top */
      gtk_signal_connect(GTK_OBJECT(gtk_sheet_get_entry(GTK_SHEET(sheets[i]))),
		         "changed", (GtkSignalFunc)show_entry, NULL);

      gtk_signal_connect(GTK_OBJECT(sheets[i]),
		         "activate", (GtkSignalFunc)activate_sheet_cell, NULL);
    }  
  }
  gtk_signal_connect(GTK_OBJECT(entry),
		      "changed", (GtkSignalFunc)show_sheet_entry, NULL);

  gtk_signal_connect(GTK_OBJECT(entry),
		      "activate", (GtkSignalFunc)activate_sheet_entry,
		      NULL);

  SetupCSheetHandlers(sheets[Components]);

  sheets[Pins]->autoresize=TRUE;
  
}

/*------------------------------------------------------------------*/
/*! \brief Add row labels to GtkSheet
 *
 * Add row labels to GtkSheet
 * \param sheet Pointer to the GtkSheet object
 * \param count Number of row labels to add
 * \param list_head Top of the string list
 */
void x_gtksheet_add_row_labels(GtkSheet *sheet, int count,
			       STRING_LIST *list_head)
{
  STRING_LIST *string_list_item;
  char *text;
  int j;
  int width = 0;
  int new_width = 0;
  int char_width;

  /* Leave if no items to add are available */
  if ((count == 0) || (list_head == NULL)) return;

  /* Get character width based upon "X", which is a large char. */
  if ( GTK_WIDGET(sheet)->style->private_font )
    char_width = gdk_char_width (GTK_WIDGET(sheet)->style->private_font, (char)'X'); 
  else
    char_width = DEFAULT_FONT_WIDTH;

  string_list_item = list_head;
  for (j = 0; j < count; j++) {
    text = string_list_item->data;

    new_width = strlen(text);  
    width = (new_width > width) ? new_width : width;
    
    gtk_sheet_row_button_add_label(sheet, j, text);
    gtk_sheet_row_button_justify(sheet, j, GTK_JUSTIFY_LEFT);
    string_list_item = string_list_item->next;
  }
  width = char_width * width;
  gtk_sheet_set_row_titles_width(sheet, width+8);
}

/*------------------------------------------------------------------*/
/*! \brief Add column labels to GtkSheet
 *
 * Add column labels to GtkSheet.
 * \param sheet GtkSheet to add columns to
 * \param count
 * \param list_head pointer to top of STRING_LIST
 */
void x_gtksheet_add_col_labels(GtkSheet *sheet, int count,
			       STRING_LIST *list_head)
{
  STRING_LIST *string_list_item;
  char *text;
  int j;
  int width = 0;
  int char_width;
  
  /* Leave if no items to add are available */
  if ((count == 0) || (list_head == NULL)) return;
  
  if ( GTK_WIDGET(sheet)->style->private_font )
    char_width = gdk_char_width (GTK_WIDGET(sheet)->style->private_font, (char)'X'); 
  else
    char_width = DEFAULT_FONT_WIDTH;

  string_list_item = list_head;
  for (j = 0; j < count; j++) {

    text = string_list_item->data;
    width = strlen(text);
    if (width < COLUMN_MIN_WIDTH) width = COLUMN_MIN_WIDTH;
    gtk_sheet_set_column_width(sheet, j, char_width * width);

    gtk_sheet_column_button_add_label(sheet, j, text);
    gtk_sheet_column_button_justify(sheet, j, GTK_JUSTIFY_LEFT);
    string_list_item = string_list_item->next;
  }
}

/*------------------------------------------------------------------*/
/*! \brief Add a cell item to the GtkSheet
 *
 * Add a cell item to the GtkSheet
 * \param sheet GtkSheet to add the cell item to
 * \param i
 * \param j
 * \param text
 * \param visibility
 * \param show_name_value
 */
void x_gtksheet_add_cell_item(GtkSheet *sheet, int i, int j, 
			      char *text, 
			      int visibility, 
			      int show_name_value)
{
  int length = strlen(text);
  int desired_width = length * DEFAULT_FONT_WIDTH;
  
  /* Auto resize up to limit */
  if (( desired_width <= COLUMN_WIDTH_LIMIT) &&
      ( desired_width > sheet->column[j].width )) {
    gtk_sheet_set_column_width(sheet, j, desired_width);
  }
  
  //char strBuff [TEXT_WIDTH_LIMIT + 2];

   /*
  if ( strlen(text) > TEXT_WIDTH_LIMIT) {
    strncpy( strBuff, text, TEXT_WIDTH_LIMIT);
    strBuff[TEXT_WIDTH_LIMIT] = '\0';    // manually added null character
    strBuff[TEXT_WIDTH_LIMIT +1] = '\0'; // and another null character 
  }
  else
    strcpy( strBuff, text);
  */
  gtk_sheet_set_cell(sheet, i, j, GTK_JUSTIFY_LEFT, text);

  if (visibility == INVISIBLE) {
    x_gtksheet_set_cell_fgcolor(sheet, i, j, Gray);
  } else {
    switch(show_name_value) {

    case(SHOW_NAME_VALUE):
      	x_gtksheet_set_cell_fgcolor(sheet, i, j, Blue);
	break;

    case(SHOW_NAME):
      	x_gtksheet_set_cell_fgcolor(sheet, i, j, Red);
	break;

    case(SHOW_VALUE):
      	x_gtksheet_set_cell_fgcolor(sheet, i, j, Black);
	break;
    }
  } /* if (visibility == INVISIBLE) */

  /* Need to find a way to ensure that the text in a cell is clipped.
   * Otherwise, long attribs overwrite adjacent cells.  */
}

/*! \brief Get the first column selected in the GtkSheet
 *
 * Get the first column selected in the GtkSheet
 * Returns the index of the first column selected, or -1 if
 *         no column is selected.
 * \param sheet GtkSheet to query
 * \returns index of the first column selected, or -1 if
 *          no column is selected.
 */
int x_gtksheet_get_min_col(GtkSheet *sheet) {
  if (sheet->state == GTK_SHEET_COLUMN_SELECTED) {
    return sheet->range.col0;
  } else {
    return -1;
  }
}

/*! \brief Get the last column selected in the GtkSheet
 *
 * Get the last column selected in the GtkSheet
 * \param GtkSheet to query
 * \returns the index of the last column selected, or -1 if
 *         no column is selected.
 */
int x_gtksheet_get_max_col(GtkSheet *sheet) {
  if (sheet->state == GTK_SHEET_COLUMN_SELECTED) {
    return sheet->range.coli;
  } else {
    return -1;
  }
}

/*! \brief Set the text color of a range of cells
 *
 * Sets the color of a range cells identified by row, col.
 * \param sheet GtkSheet to operate on
 * \param row Row of cell to set
 * \param col Column of cell to set
 * \param Color id Color to set text to
 */

void x_gtksheet_set_cell_fgcolor(GtkSheet *sheet, int row, int col, 
			         ColorId Color )
{
  
  GdkColormap *color_map;
  GtkSheetRange range;
  GdkColor color_t;

  /* get pointer to system color map */
  color_map = gdk_colormap_get_system ();
  
  /* fill in the RGB values for associated string */
  gdk_color_parse ( Colors[Color], &color_t);
 
  /* given the RGB, gtk->resolve the 32 bit pixel data */
  if (!gdk_colormap_alloc_color (color_map, &color_t, FALSE, FALSE)) {
    g_error ("couldn't allocate color");
    return;
  }
  
  /* set color of range */
  range.row0 = row;
  range.rowi = row;
  range.col0 = col;
  range.coli = col;

  /* set color */
  gtk_sheet_range_set_foreground(sheet, &range, &color_t);

}


