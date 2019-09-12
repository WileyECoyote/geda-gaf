/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 *
 * Copyright (C) 2003-2015 Stuart D. Brorson.
 * Copyright (C) 2003-2015 gEDA Contributors (see ChangeLog for details)
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
 * \brief Functions to interface to the spreadsheet widget.
 *
 * This file holds functions used to handle the spreadsheet widget.
 * Much of this was hacked from testgtksheet.c starting in Jan 2004
 * by SDB.
 */

/*------------------------------------------------------------------
 * Includes required to run graphical widgets.
 *------------------------------------------------------------------*/
#include "../include/gattrib.h"

#include <gtk/gtk.h>
#include <gtksheet.h>

#include <geda/geda_gui_funcs.h>
#include <geda_widgets.h>

#define COLUMN_MIN_WIDTH 10

const char *Colors [] = { "black",      "red",    "blue",
                          "green",      "orange", "purple",
                          "gray",       "pink",   "skyblue",
                          "lightgreen", "tan",    "violet",
                          "yellow",     "white"
};

static char *popup_items[]={ N_("Toggle Visibility"),
                             N_("Add Attribute"),
                             N_("Insert Attribute"),
                             N_("Hide Attribute"),
                             N_("Reveal Attribute"),
                             N_("Delete Attribute"),
                             N_("Clear Attribute Data")
};

char EditBuffer[255];

/*! \brief Return pointer to Active GtkSheet */
GtkSheet *x_gtksheet_get_current_sheet() {
  return GTK_SHEET(sheets[gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook))]);
}

/*!
 * \brief Release GtkSheet related resources
 * \par Function Description
 *  Destroy All GtkSheets.
 */
void x_gtksheet_destroy_all() {

  int i;

  for(i = 0; i < NUM_SHEETS; i++) {

    if (sheets[i] !=NULL) {

      if (GTK_IS_SHEET (sheets[i])) {

        gtk_widget_destroy ((GtkWidget*)sheets[i]);

      }
    }
  }

  g_free(sheets);
  sheets = NULL;

  if (popup) {

    /* Only occurs if user views the context menu without selecting
     * an item, otherwise the callack would have destroyed the popup */
    gtk_widget_destroy (GTK_WIDGET(popup));
    g_object_unref (popup);
    popup = NULL;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void x_gtksheet_reveal_columns(GtkSheet *sheet) {

  ColumnVisible *cv;
  GList *list = NULL;
  GList *iter;
  int i;

  for (i = 0; i < sheet->maxalloccol; i++) {

    cv  = GEDA_MEM_ALLOC (sizeof(ColumnVisible));
    cv->name = gtk_sheet_get_column_title(sheet, i);
    cv->visible = gtk_sheet_column_visible(sheet, i);

    list = g_list_append (list, cv);
  }

  if (x_dialog_column_visibility(list)) {

    iter = list;
    i = 0;

    while (iter) {

      bool visible;

      cv  = iter->data;
      visible =gtk_sheet_column_visible(sheet, i);

      if (cv->visible != visible) {
        gtk_sheet_column_set_visibility(sheet, i, cv->visible);
      }

      i++;
      iter = iter->next;
    }
  }

  geda_glist_free_all (list);
}

/*!
 * \brief Callback Handler for Popup Mouse Context Menu
 * \par Function Description
 *  This function calls the appropriate functions to process request
 *  from the mouse menu that can be activated when not in editing mode.
 *  Unlike the example in the testgtksheet program this function
 *  receives a pointer to an enumerated integer value rather than a string.
 *
 * \param [in] widget is button widget
 * \param [in] selection pointer to enumerated menu selection
 */
static int popup_activated(GtkWidget *widget, IDS_Popup_items *selection)
{
  GtkSheet *sheet = x_gtksheet_get_current_sheet();

  int WhichItem = (int)(long)selection;

  switch ( WhichItem ) {
    case ToggleVisibility:
      if (!x_gtksheet_get_is_empty(sheet, sheet->active_cell.row,
        sheet->active_cell.col))
      {
        if (s_properties_get_visibility(sheet->active_cell.row,
          sheet->active_cell.col))
          s_properties_set_invisible();
        else
          s_properties_set_visible();
      }
      break;

    case AddAttribute:
      s_toplevel_add_new_attrib(-1);
      break;

    case InsertAttribute:
      s_toplevel_add_new_attrib(sheet->range.col0);
      break;

    case HideAttribute:
      gtk_sheet_column_set_visibility(sheet, sheet->range.col0, FALSE);
      gtk_sheet_unselect_range(sheet);
      break;

    case RevealAttribute:
      x_gtksheet_reveal_columns(sheet);
      break;

    case DeleteAttribute:
      s_toplevel_delete_attrib_col(sheet);
      break;

    case ClearAttributeData:
      gtk_sheet_range_clear(sheet, &sheet->range);
      break;

    default:
      geda_log ("%s: unknown button ID: %d\n", __func__, WhichItem);
  } /* End Switch WhichItem */

  gtk_widget_destroy(popup);
  g_object_unref(popup);
  popup = NULL;

  return (TRUE);
}

/*!
 * \brief Create and Setup Popup Mouse Menu
 * \par Function Description
 *  This function is called when the user right clicks on the gtksheet
 *  and the sheet is not in editing mode. The function sets sensitivities
 *  on menu choices based on the sheet state.
 *
 * \param [in] sheet is the active sheet widget
 */
static GtkWidget *build_popup_menu(GtkWidget *sheet)
{
  GtkWidget *menu;
  int i;

  menu = geda_menu_new();

  for (i = 0; i < (sizeof(popup_items)/sizeof(popup_items[0])); i++)
  {
    GtkWidget *item = geda_menu_item_new_with_label(popup_items[i]);

    GEDA_SIGNAL_CONNECT(item, "activate", popup_activated, (void*)(long) i);

    switch (i) {
      case ToggleVisibility:
      case AddAttribute:
      case ClearAttributeData:
        if (GTK_SHEET(sheet)->state!=GTK_SHEET_NORMAL) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        else {
          gtk_widget_set_sensitive(GTK_WIDGET(item), TRUE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), TRUE);
        }
        break;

      case InsertAttribute:
      case HideAttribute:
      case RevealAttribute:
      case DeleteAttribute:
        if (GTK_SHEET(sheet)->state!=GTK_SHEET_COLUMN_SELECTED) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        else {
          gtk_widget_set_sensitive(GTK_WIDGET(item), TRUE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), TRUE);
        }
        break;
      }

      gtk_widget_show(item);
      geda_menu_append (menu, item);
    }
    return (menu);
}

/*!
 * \brief Mouse Button Call Back
 * \par Function Description
 *  This function check mouse botton press and when the 3rd button is
 *  released the build_popup_menu function is called to create the mouse
 *  menu.
 *
 * \param [in] widget is the active sheet widget
 * \param [in] event  the mouse event record
 * \param [in] data   NULL, this parameter is not used
 */
static int on_mouse_button_press(GtkWidget *widget, GdkEventButton *event, void *data)
{
  GdkModifierType mods;
  GtkSheet *sheet = GTK_SHEET(widget);
  bool handled = FALSE;

  gdk_window_get_pointer (gtk_widget_get_window(widget), NULL, NULL, &mods);

  if (mods & GDK_BUTTON3_MASK) {

    /* popup is a global pointer to a GtkWidget, see include/globals.h */
    if (popup) {
      gtk_widget_destroy(GTK_WIDGET(popup));
      g_object_unref(popup);
      popup = NULL;
    }

    popup = build_popup_menu(widget);

    /* Display the menu we just created */
    geda_menu_popup(GEDA_MENU(popup), NULL, NULL, NULL, NULL,
                    event->button, event->time);
    handled = TRUE;
  }
  else if (mods & GDK_BUTTON2_MASK) {

    GtkWidget *entry = gtk_sheet_get_entry (sheet);

    g_signal_emit_by_name(entry, "paste-clipboard", NULL);
    sheet_head->CHANGED = TRUE;
    x_window_update_title(pr_current, sheet_head);
    handled = TRUE;
  }

  return handled;
}

/*!
 * \brief Callback for key-press event
 * \par Function Description
 *
 * \param [in] sheet  is the active sheet widget
 * \param [in] event  Keyboard Event record
 */
static int clipboard_handler(GtkSheet *sheet, GdkEventKey *event)
{
  bool state = sheet->state;

  if (event->state & GDK_CONTROL_MASK ||
      event->keyval==GDK_Control_L ||
      event->keyval==GDK_Control_R)
  {

    if ((event->keyval=='c' ||
         event->keyval == 'C') &&
         state != GTK_STATE_NORMAL) {
      if (gtk_sheet_in_clip(sheet)) {
        gtk_sheet_unclip_range(sheet);
      }

      gtk_sheet_clip_range(sheet, &sheet->range);
    }

    if (event->keyval=='x' || event->keyval == 'X') {
      gtk_sheet_unclip_range(sheet);
    }
  }

  return (FALSE);
}

/*! \todo This callback should get pointers from the stack not global */
static void clipboard_receive_entry_text(GtkClipboard *clipboard,
                                           const char *text,
                                                 void *data)
{
  if (text && strlen(text) > 0) {

    GedaEntry *entry = data;
    geda_entry_set_selected_text(entry, text);
    sheet_head->CHANGED = TRUE;
    x_window_update_title(pr_current, sheet_head);
  }
}

/*!
 * \brief Callback on button pressed in the entry
 * \par Function Description
 *  Paste contents of the clip board into the entry if the event was
 *  a button 2 press, obilterating the contents.
 */
static int
x_gtksheet_button_pressed(GtkWidget *widget, GdkEventButton *event, void *nothing)
{
  if (event->button == 2) {

      GtkClipboard *clip = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);

      gtk_clipboard_request_text (clip, clipboard_receive_entry_text, widget);

      return TRUE;
  }

  return FALSE;
}

/*!
 * \brief Callback on sheet resize
 * \par Function Description
 *  This function is not used.
 */
static void on_resize(GtkWidget *widget, GtkSheetRange *old_range,
                    GtkSheetRange *new_range, void *data)
{
  printf("OLD SELECTION: %d %d %d %d\n",old_range->row0, old_range->col0,
                                    old_range->rowi, old_range->coli);
  printf("NEW SELECTION: %d %d %d %d\n",new_range->row0, new_range->col0,
                                    new_range->rowi, new_range->coli);
}

/*! \brief Callback on sheet Move
 *  \par Function Description
 *  This function is not used.
 */
static void on_move(GtkWidget *widget, GtkSheetRange *old_range,
                    GtkSheetRange *new_range, void *data)
{
  printf("OLD SELECTION: %d %d %d %d\n",old_range->row0, old_range->col0,
                                    old_range->rowi, old_range->coli);
  printf("NEW SELECTION: %d %d %d %d\n",new_range->row0, new_range->col0,
                                    new_range->rowi, new_range->coli);
}

/*!
 * \brief Callback on Change Entry
 * \par Function Description
 *  This function is not used.
 */
bool change_entry(GtkWidget *widget,
                  int row, int col, int *new_row, int *new_col,
                  void *data)
{
  GtkSheet *sheet;

  sheet = GTK_SHEET(widget);
  bool state = sheet->state;

  if (*new_col == 0 && (col != 0 || state != GTK_STATE_NORMAL))
         gtk_sheet_change_entry(sheet, gtk_combo_get_type());

  if (*new_col == 1 && (col != 1 || state != GTK_STATE_NORMAL))
         gtk_sheet_change_entry(sheet, GTK_TYPE_ENTRY);

  if (*new_col == 2 && (col != 2 || state != GTK_STATE_NORMAL))
         gtk_sheet_change_entry(sheet, GTK_TYPE_SPIN_BUTTON);

  if (*new_col >= 3 && (col < 3 || state != GTK_STATE_NORMAL))
         gtk_sheet_change_entry(sheet, GTK_TYPE_CELL_EDITABLE);

  return TRUE;
}

/*!
 * \brief Callback on Change Cell
 * \par Function Description
 *  This function is not used. Despite the apparent usefulness of
 *  this function, the change signal is not very useful because it
 *  is called every time a cell is selected or written too. This
 *  signal is emitted even when the data in the cell has not been
 *  changed.
 */
static void on_change(GtkWidget *widget, int row, int col, void *data)
{

}

/*!
 * \brief Call Back on cell activate
 * \par Function Description
 *  This function is call each time a cell is selected. This function
 *  copies the cell text to a temporary string buffer.
 *
 * \param [in] widget is the active sheet widget
 * \param [in] row    integer value of the row of the selected cell
 * \param [in] col    integer value of the column of the selected cell
 * \param [in] data   NULL, this parameter is not used
 *
 * \retval [out] TRUE
 */
static bool on_activate_cell(GtkWidget *widget, int row, int col, void *data)
{

  char *celltext;

  celltext = gtk_sheet_cell_get_text((GtkSheet*)widget, row, col);

  if ( NULL != celltext) {
    /* Make a copy of the contents */
    strcpy(EditBuffer, celltext);
  }

 return TRUE;
}

/*!
 * \brief Call Back on cell de-activate
 * \par Function Description
 *  This function is checks the cell when the cell are deactivated and
 *  if the cell has a value, the value is compared to the string in the
 *  temporary string buffer, EditBuffer, to see if the data has changed.
 *  If the strings are not equal the the CHANGE flag is set and the
 *  x_window_update_title is called.
 *
 * \param [in] widget    is the active sheet widget
 * \param [in] row       integer value of the row of the selected cell
 * \param [in] col       integer value of the column of the selected cell
 * \param [in] PageData  Pointer to page data structure
 *
 * \retval [out] TRUE
 */
static bool on_deactivate_cell(GtkWidget *widget, int row, int col,
                               PageDataSet *PageData)
{
  char *celltext;

  celltext = gtk_sheet_cell_get_text((GtkSheet*)widget, row, col);
  if (EditBuffer != NULL) { /* If NULL then we're loading data from file */
    if (celltext != NULL) { /* If NULL then cell was empty */
      if (strcmp(EditBuffer, celltext) != 0) {
        PageData->CHANGED = TRUE;
        x_window_update_title(pr_current, PageData);
      }
    }
  }

  return TRUE;
}

/*!
 * \brief Callback on Traverse Cell
 * \par Function Description
 *  This function is not used. This function could be called after
 *  a cell has been selected, The row, col parameters are the cell
 *  that was previously select. If the function returns FALSE the
 *  cursor is moved back to the old cell. This is used for validation
 *  of the input data.
 */
static bool on_traverse(GtkWidget *widget,
                    int row, int col, int *new_row, int *new_col,
                    void *data)
{
  printf("TRAVERSE: %d %d %d %d\n",row,col,*new_row,*new_col);
  return TRUE;
}

/*!
 * \brief Setup Call Back handlers for the Component Sheet
 * \par Function Description
 *  This function is  setup the callbacks for the Component sheet.
 *  Note that we only use 3 before returing. The others are listed
 *  for possiable future.
 *
 * \param [in] sheet is the active sheet widget
 * \param [in] PageData Pointer to data structure
 */
static void SetupCSheetHandlers(GtkSheet *sheet, PageDataSet *PageData)
{
  GObject *SheetObj;
  SheetObj = G_OBJECT(sheet);

  GEDA_SIGNAL_CONNECT(SheetObj, "button_press_event",
                      on_mouse_button_press, NULL);

  GEDA_SIGNAL_CONNECT(SheetObj, "activate",
                      on_activate_cell,
                      NULL);

  GEDA_SIGNAL_CONNECT(SheetObj, "deactivate",
                      on_deactivate_cell,
                      PageData);

  return;

  GEDA_SIGNAL_CONNECT(SheetObj, "changed", (GtkSignalFunc) on_change, NULL);

  GEDA_SIGNAL_CONNECT(SheetObj, "resize_range", (GtkSignalFunc) on_resize, NULL);

  GEDA_SIGNAL_CONNECT(SheetObj, "move_range", (GtkSignalFunc) on_move, NULL);

  GEDA_SIGNAL_CONNECT(SheetObj, "traverse", (GtkSignalFunc) on_traverse, NULL);
}

/*!
 *  \brief Callback for Entry Combo "change" signal
 *  \param [in] widget     Pointer to global entry
 *  \param [in] nothing    is nothing, NULL
 */
static void on_entry_changed(GtkWidget *widget, void *nothing)
{
  const char *text;
  GtkSheet   *sheet;
  GtkEntry   *sheet_entry;

  if (!gtk_widget_has_focus (widget))
    return;

  sheet       = x_gtksheet_get_current_sheet();
  sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(sheet));

  text = GetEntryText(entry);
  if (text != NULL){
    SetEntryText(sheet_entry, text);
  }
}

/*! \brief Call back for Entry Combo activate */
static void on_entry_activate(GedaEntry *global_entry)
{
  GtkSheet *sheet;
  int row, col;

  sheet = x_gtksheet_get_current_sheet();
  row   = sheet->active_cell.row;
  col   = sheet->active_cell.col;

  gtk_sheet_set_active_cell(sheet, ++row, col);
}

/*! \brief Call back for "change" signal from embedded Entry widget */
static void show_entry(GtkWidget *widget, void *data)
{
  if (gtk_widget_has_focus (widget)) {

    GtkSheet   *sheet;
    GtkWidget  *sheet_entry;
    const char *text;

    sheet       = x_gtksheet_get_current_sheet();
    sheet_entry = gtk_sheet_get_entry(sheet);
    text        = GetEntryText (sheet_entry);

    if (text != NULL) {
      SetEntryText(entry, text);
    }
  }
}

/*! \brief Call back for "activate" signal from sheet cell array widget */
static int activate_sheet_cell(GtkWidget *widget, int row, int column, void *data)
{
  GtkSheetCellAttr attributes;
  GtkSheet *sheet;
  GtkEntry *sheet_entry;
  char      cell[100];

  sheet       = GTK_SHEET(widget);
  sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(sheet));

  if (GTK_SHEET(widget)->column[column]->title) {
    sprintf(cell,"  %s:%d  ",GTK_SHEET(widget)->column[column]->title, row);
  }
  else {
    sprintf(cell, "R:%d C: %d", row, column);
  }

  geda_label_widget_set_text(location, cell);

  g_object_set (entry, "max-length", GTK_ENTRY(sheet_entry)->text_max_length, NULL);

  SetEntryText(entry, GetEntryText(gtk_sheet_get_entry(sheet)));

  gtk_sheet_get_attributes(sheet,sheet->active_cell.row,
                           sheet->active_cell.col, &attributes);

  gtk_editable_set_editable(GTK_EDITABLE(sheet_entry), attributes.is_editable);

  return TRUE;
}

/*!
 * \brief Reinitialize the GtkSheet
 * \par Function Description
 * Reinitializes the GtkSheet widget. This function is called
 * after a file has been opened to resize the gtksheet by adding
 * or removing rows and columns are required.
 */
void x_gtksheet_reinititialize(PageDataSet *PageData)
{
  void RedimensionSheet(GtkSheet *sheet, int nRows, int nCols) {
    unsigned int cRows = gtk_sheet_get_rows_count(sheet);
    unsigned int cCols = gtk_sheet_get_columns_count(sheet);

    if (nRows > 0) {
      if ( nRows > cRows) {
        gtk_sheet_add_row(sheet, nRows - cRows );
      }
      else {
        if (  cRows > nRows) {
          gtk_sheet_delete_rows	(sheet, 0, cRows - nRows);
        }
      }
      /* else they are the same size so do nothing */
    }

    if (nCols > 0) {
      if ( nCols > cCols) {
        gtk_sheet_add_column(sheet, nCols - cCols );
      }
      else {
        if ( cCols > nCols) {
          gtk_sheet_delete_columns(sheet, 0, cCols - nCols);
        }
      }
      /* else they are the same size so do nothing */
    }
  }

  /* -----  Components  ----- */
  RedimensionSheet(sheets[Components], PageData->comp_count, PageData->comp_attrib_count);

  /* -----  Nets  ----- */
  RedimensionSheet(sheets[Nets], PageData->net_count, PageData->net_attrib_count);

  /* -----  Pins  ----- */
  RedimensionSheet(sheets[Pins], PageData->pin_count, PageData->pin_attrib_count);
}

/*!
 * \brief Create the GtkSheet
 * \par Function Description
 *  Creates and initializes the GtkSheet widget, which is the
 *  spreadsheet widget used for displaying the data.
 */
void x_gtksheet_init(PageDataSet *PageData)
{
  char *SheetNames[]= { N_("Components"),  N_("Nets"), N_("Pins")};

  void CreateSheet(SheetId index, int nRow, int nCol) {

    if ((sheets[index] != NULL) && (GTK_IS_SHEET (sheets[index]))) {
      fprintf(stderr, "ERROR: %s sheet already exist!\n", SheetNames[index]);
    }
    else {
      if ((nRow > 0) && (nCol > 0)) {
        sheets[index] = (GtkSheet*)gtk_sheet_new( nRow , nCol, SheetNames[index]);
      }
      else {
        fprintf(stderr, "ERROR: (%s) row count =[%d], col count=[%d]\n",
                SheetNames[index], nRow , nCol);
        sheets[index] = (GtkSheet*) gtk_sheet_new(1, 1, SheetNames[index]);
        gtk_sheet_set_locked(GTK_SHEET(sheets[index]), TRUE);   /* disallow editing */
      }
    }

    if (!GTK_IS_SHEET (sheets[index])) {
      fprintf(stderr, "ERROR: could not create %s sheet!\n", SheetNames[index]);
    }
  }

  /* Dynamically allocate storage for pointers to sheet, this block
   * is released in x_gtksheet_destroy_all */
  sheets = GEDA_MEM_ALLOC0(NUM_SHEETS * sizeof(GtkWidget*));

  /* ---  Create three new sheets  --- */

  /* -----  Components  ----- */
  CreateSheet(Components, PageData->comp_count, PageData->comp_attrib_count);

  /* -----  Nets  ----- */
  CreateSheet(Nets, PageData->net_count, PageData->net_attrib_count);

  /* -----  Pins  ----- */
  CreateSheet(Pins, PageData->pin_count, PageData->pin_attrib_count);

  int i;

  /* Finally stick labels on the notebooks holding the sheets. */
  for (i = 0; i < NUM_SHEETS; i++) {

    /* this prevents us from segfaulting on empty nets sheet. */
    if (sheets[i] != NULL) {

      scrolled_windows = (GtkWidget**)realloc(scrolled_windows, (i+1)*sizeof(GtkWidget*));
      scrolled_windows[i] = gtk_scrolled_window_new(NULL, NULL);

      geda_container_add (scrolled_windows[i], (GtkWidget*)sheets[i]);

      /* First remove old notebook page. Maybe should probably do some checking here. */
      if (notebook != NULL) {
        gtk_notebook_remove_page((GtkNotebook*)notebook, i);
      }

      /* Then add new, updated notebook page */
      GtkWidget *label= gtk_label_new(SheetNames[i]);

      gtk_notebook_insert_page_menu ((GtkNotebook*)notebook,
                                     (GtkWidget*)scrolled_windows[i],
                                     (GtkWidget*)label, NULL, -1);

      sheets[i]->autoresize_columns = FALSE;
      sheets[i]->autoresize_rows = FALSE;

      gtk_sheet_set_autoscroll(sheets[i], TRUE);

      /* Maybe this fixes a long time sore spot for gattrib */
      gtk_sheet_set_clip_text(sheets[i], TRUE);

      /* For now we keep the nets sheet invisible is still useless */
      if (i != Nets) {
        gtk_widget_show((GtkWidget*)sheets[i]);
        gtk_widget_show((GtkWidget*)scrolled_windows[i]);
      }

      gtk_widget_show(notebook);  /* show updated notebook  */

      g_signal_connect (sheets[i], "key_press_event",
                       (GtkSignalFunc) clipboard_handler, NULL);

      /*  The entry cell is the text entry field is the one at the top */
      g_signal_connect(gtk_sheet_get_entry((GtkSheet*)sheets[i]),
                       "changed", (GtkSignalFunc)show_entry, NULL);

      g_signal_connect(sheets[i],
                       "activate", (GtkSignalFunc)activate_sheet_cell, NULL);
    }
  }

  /* The next 2 functions setup callbacks for the Entry widget in the would
   * be status bar */
  g_signal_connect (entry, "button_press_event",
                   (GtkSignalFunc) x_gtksheet_button_pressed, NULL);

  g_signal_connect(entry, "changed", (GtkSignalFunc)on_entry_changed, NULL);

  GedaEntryClass *entry_class = GEDA_ENTRY_GET_CLASS(entry);

  entry_class->activate = on_entry_activate;

  SetupCSheetHandlers(sheets[Components], PageData);

  sheets[Pins]->autoresize_columns=TRUE;
}

/*------------------------------------------------------------------*/

/*!
 * \brief Add row labels to GtkSheet
 * \par Function Description
 *  Add row labels to GtkSheet.
 *
 * \param sheet Pointer to the GtkSheet object
 * \param count Number of row labels to add
 * \param list_head Top of the string list
 */
void x_gtksheet_add_row_labels(GtkSheet *sheet, int count, STRING_LIST *list_head)
{
  STRING_LIST *string_list_item;
  GtkStyle    *style;

  int j;
  int width = 1;
  int char_width;

  /* Leave if no items to add are available */
  if ((count == 0) || (list_head == NULL))
    return;

  style =  gtk_widget_get_style((GtkWidget*)sheet);

  /* Get character width based upon "X", which is a large char. */
  if (style->private_font)
    char_width = gdk_char_width (style->private_font, (char)'X');
  else
    char_width = DEFAULT_FONT_WIDTH;

  string_list_item = list_head;

  for (j = 0; j < count; j++) {

    char *text;
    int   new_width;

    text      = string_list_item->data;
    new_width = strlen(text);
    width     = (new_width > width) ? new_width : width;

    gtk_sheet_row_button_add_label(sheet, j, text);
    gtk_sheet_row_button_justify(sheet, j, GTK_JUSTIFY_LEFT);
    gtk_sheet_set_row_title(sheet, j, text);
    string_list_item = string_list_item->next;
  }

  width = char_width * width;

  gtk_sheet_set_row_titles_width(sheet, width+8);
}

/*------------------------------------------------------------------*/

/*!
 * \brief Add column labels to GtkSheet
 * \par Function Description
 *  This function adds column labels to GtkSheet. The width of the
 *  columns is increased based on the data.
 *
 * \param sheet GtkSheet to add columns to
 * \param count
 * \param list_head pointer to top of STRING_LIST
 */
void x_gtksheet_add_col_labels(GtkSheet    *sheet,
                               int          count,
                               STRING_LIST *list_head)
{
  STRING_LIST *string_list_item;
  GtkStyle    *style;

  int j;
  int char_width;

  /* Leave if no items to add are available */
  if ((count == 0) || (list_head == NULL))
    return;

  style =  gtk_widget_get_style((GtkWidget*)sheet);

  if (style->private_font)
    char_width = gdk_char_width (style->private_font, (char)'X');
  else
    char_width = DEFAULT_FONT_WIDTH;

  string_list_item = list_head;

  for (j = 0; j < count; j++) {

    char *text;
    int   width;

    text  = string_list_item->data;
    width = strlen(text);

    if (width < COLUMN_MIN_WIDTH) {
      width = COLUMN_MIN_WIDTH;
    }
    gtk_sheet_set_column_width(sheet, j, char_width * width);

    gtk_sheet_column_button_add_label(sheet, j, text);
    gtk_sheet_column_button_justify(sheet, j, GTK_JUSTIFY_LEFT);
    gtk_sheet_set_column_title(sheet, j, text);

    if (geda_utility_glist_find_string(hide_columns, text) > -1) {
      gtk_sheet_column_set_visibility(sheet, j, FALSE);
    }

    string_list_item = string_list_item->next;
  }
}

/*------------------------------------------------------------------*/
/*!
 * \brief Set the text foreground color of a range of cells
 * \par Function Description
 *  This functions sets the color of a range cells identified by row,
 *  col.
 *
 * \param sheet GtkSheet to operate on
 * \param row   Row of cell to set
 * \param col   Column of cell to set
 * \param Color id Color to set text to
 */
void x_gtksheet_set_cell_fgcolor(GtkSheet *sheet, int row, int col,
                                 ColorId Color)
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
    g_error (_("could not allocate color"));
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

/*!
 * \brief Set the text background color of a range of cells
 * \par Function Description
 *  This functions sets the color of a range cells identified by
 *  \a row and \a col.
 *
 * \param sheet GtkSheet to operate on
 * \param row   Row of cell to set
 * \param col   Column of cell to set
 * \param Color id Color to set text to
 */
void x_gtksheet_set_cell_bgcolor(GtkSheet *sheet, int row, int col,
                                 ColorId Color)
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
    g_error (_("could not allocate color"));
    return;
  }

  /* set color of range */
  range.row0 = row;
  range.rowi = row;
  range.col0 = col;
  range.coli = col;

  /* set color */
  gtk_sheet_range_set_background(sheet, &range, &color_t);
}

/*------------------------------------------------------------------*/
/*!
 * \brief Add a cell item to the GtkSheet
 * \par Function Description
 *  This functions loads text into a cell and increases the column width
 *  up to COLUMN_WIDTH_LIMIT if needed. The function calls x_gtksheet_set
 *  _cell_fgcolor to set the text color based on the properties of the
 *  attribute.
 *
 * \param sheet GtkSheet to add the cell item to
 * \param row
 * \param col
 * \param text
 * \param visibility
 * \param show_name_value
 * \param is_inherited
 */
void x_gtksheet_add_cell_item(GtkSheet *sheet, int row, int col, char *text,
                              int visibility, int show_name_value, int is_inherited)
{
  int length = strlen(text);
  int desired_width = length * DEFAULT_FONT_WIDTH;
  int fgcolor;

  /* Auto resize up to limit */
  if ((desired_width <= COLUMN_WIDTH_LIMIT) &&
      (desired_width > sheet->column[col]->width)) {
    gtk_sheet_set_column_width(sheet, col, desired_width);
  }

  gtk_sheet_set_cell(sheet, row, col, GTK_JUSTIFY_LEFT, text);

  fgcolor = s_properties_get_fgcolor_index(visibility, show_name_value, is_inherited);

  x_gtksheet_set_cell_fgcolor(sheet, row, col, fgcolor);
}

/*!
 * \brief Get GtkSheet cell at given Row Col is Empty
 * \par Function Description
 *
 * \param sheet GtkSheet to query
 * \param row   The Row to query
 * \param col   The column to query
 *
 * \returns TRUE if the cell at Row Col is empty.
 */
bool x_gtksheet_get_is_empty(GtkSheet *sheet, int row, int col)
{
  return gtk_sheet_cell_get_text(sheet, row, col) == NULL;
}

/*!
 * \brief Get the first column selected in the GtkSheet
 * \par Function Description
 *  Get the first column selected in the GtkSheet
 *  Returns the index of the first column selected, or -1 if
 *  no column is selected.
 *
 * \param sheet GtkSheet to query
 *
 * \returns index of the first column selected, or -1 if
 *          no column is selected.
 */
int x_gtksheet_get_min_col(GtkSheet *sheet) {

  if (sheet->state == GTK_SHEET_COLUMN_SELECTED) {
    return sheet->range.col0;
  }

  return -1;
}

/*!
 * \brief Get the last column selected in the GtkSheet
 * \par Function Description
 *  Get the last column selected in the GtkSheet
 *
 * \param sheet The GtkSheet object to query
 *
 * \returns The index of the last column selected, or -1 if
 *          no column is selected.
 */
int x_gtksheet_get_max_col(GtkSheet *sheet) {

  if (sheet->state == GTK_SHEET_COLUMN_SELECTED) {
    return sheet->range.coli;
  }

  return -1;
}

/*!
 * \brief Copy a GtkSheet Range
 * \par Function Description
 *  This functions set the integer values in the Target Ranges to
 *  the values in the Source Range
 *
 * \param s_range pointer to the source Range structure
 * \param t_range pointer to the target Range structure
 */
void x_gtksheet_range_copy(GtkSheetRange *s_range, GtkSheetRange *t_range)
{
  t_range->row0 = s_range->row0;
  t_range->rowi = s_range->rowi;
  t_range->col0 = s_range->col0;
  t_range->coli = s_range->coli;
}

/*!
 * \brief Set Range to Maxium value
 * \par Function Description
 *  This function set the values in a Range structure from the first cell
 *  on a sheet to the last cell on the sheet.
 *
 * \param sheet The GtkSheet object to query
 * \param range pointer to a source Range structure
 *
 * \returns The index of the last column selected, or -1 if
 *          no column is selected.
 */
void x_gtksheet_set_max_range(GtkSheet *sheet, GtkSheetRange *range)
{
  range->row0 = 0;
  range->rowi = sheet->maxrow;
  range->col0 = 0;
  range->coli = sheet->maxcol;
}
