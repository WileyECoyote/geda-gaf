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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * Date: November, 17, 2012
 * Contributing Author: Wiley Edward Hill
 */

#include <gattrib.h>
#include <geda/geda_gui_funcs.h>
#include <geda_debug.h>

#define MAX_SEARCH_STRING 128
#define SEARCH_ALL 0

static char search_string[MAX_SEARCH_STRING+12];
static SearchRecord Search;

/*! \brief Notify Search Text Not Found Dialog
 *  \par
 *  Common pre-search routine to save the range of the current
 * selection of the active worksheet and sets the search mode
 * flag to row, column or all based on the current state.
 */
static void x_find_set_search_parameters()
{
  int cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  Search.sheet = sheets[cur_page];

  x_gtksheet_range_copy(&Search.sheet->range, &Search.range);

  if (Search.sheet->state==GTK_SHEET_COLUMN_SELECTED) {
     Search.mode = Search.sheet->range.col0;
  }
  else {
    if (Search.sheet->state==GTK_SHEET_ROW_SELECTED) {
       Search.mode = -1 * Search.sheet->range.row0;
    }
    else {
       Search.mode = SEARCH_ALL; /* is just 0 flag */
       x_gtksheet_set_max_range(Search.sheet, &Search.range);
    }
  }

  /* STUB: Load Previous Settings */
  Search.Case      = FALSE;
  Search.Whole     = TRUE;
  Search.Backword  = FALSE;
  Search.Wrap      = TRUE;
  Search.Found     = FALSE;
  Search.count     = 0;

}

/*! \brief Notify Search Text Not Found Dialog
 * called by x_find_attribute and x_find_refdes when the search
 * text was not found
 */
static void x_find_notify_not_found(char *text)
{
  strcpy(search_string, text);
  strcat(search_string, " ");
  strcat(search_string, _("not found!"));
  x_dialog_generic_confirm_dialog (search_string, GTK_MESSAGE_INFO);
}

/*! \brief Search Sheet for text and optionally replace string
 *  \par Function Description
 *  Performs search, either forward or backward over the active search
 *  range based on flags in SearchRecord.
 */
bool x_find_main_search(char *text, char *replacement) {

  int row, col;
  int srow, scol;
  int max_col = Search.sheet->maxcol;
  int max_row = Search.sheet->maxrow;
  int first_cell;
  int inc; /* increment */
  void (*search_func)();

  char *cell_text;
  bool found = FALSE;

  gtk_sheet_get_active_cell (Search.sheet, &srow, &scol);

  int ishit( ) {

    if (!cell_text)
      return 0;

    if (Search.Whole)
      if (Search.Case)
        return !(strcmp ( cell_text, text));
      else
        return (!geda_utility_string_stricmp ( cell_text, text));
    else
      if (Search.Case)
        return (strstr ( cell_text, text)) ? (strlen (strstr ( cell_text, text))) : 0;
      else
        return (geda_string_istr ( cell_text, text)) ? (strlen (geda_string_istr ( cell_text, text))) : 0;
  }

  void do_replace_text(int row, int col) {

    char *new;

    new = malloc(strlen (cell_text) - strlen (text) + strlen (replacement) +2 );

    if (Search.Whole)
        strcpy(new, replacement);
    else {
      strcpy(new, cell_text);
      if (Search.Case)
        geda_utility_string_strsubst(new, text, replacement);
      else
        geda_utility_string_strisubst(new, text, replacement);
    }

    gtk_sheet_set_cell_text(Search.sheet, row, col, new);
    free(new);
  }

  void search_range_backword() {
    for( col = scol; col > (Search.range.col0-1); col--) {
      /*check current cell and advance here?*/
      for( row = srow; row > (Search.range.row0-1); row--) {
        cell_text = gtk_sheet_cell_get_text(Search.sheet, row, col);
        if (ishit() > 0) {
          found = TRUE;
          gtk_sheet_set_active_cell (Search.sheet, row, col);
          if (replacement) {
            do_replace_text(row, col);
          }
          if (!Search.ReplaceAll)
            break;
        }
      }
      if ((found) && (!Search.ReplaceAll))
          break;
        srow = Search.range.rowi; /* for subsequent rows start at the beginning*/
    }
  }

  void search_range_forward() {
    for( col = scol; col < (Search.range.coli+1); col++) {
      /*check current cell and advance here?*/
      for( row = srow; row < (Search.range.rowi+1); row++) {
        cell_text = gtk_sheet_cell_get_text(Search.sheet, row, col);
        if (ishit() > 0) {
          found = TRUE;
          gtk_sheet_set_active_cell (Search.sheet, row, col);
          if (replacement) {
            do_replace_text(row, col);
          }
          if (!Search.ReplaceAll)
            break;
        }
      }
      if ((found) && (!Search.ReplaceAll))
          break;
        srow = Search.range.row0; /* for subsequent rows start at the beginning*/
    }
  }

  /* Begin */

  if (Search.Backword) {
    inc = -1;
    search_func = search_range_backword;
    first_cell = max_row + max_col;
  }
  else {
    inc = 1;
    search_func = search_range_forward;
    first_cell = 0;
  }

  /* How we start depends on whether we are replacing AND if
   * the current cell is a target, so get current cell value */
  cell_text = gtk_sheet_cell_get_text(Search.sheet, srow, scol);

  if ((replacement) && (ishit() > 0) && (!Search.ReplaceAll)) { /* if need to replace */
    do_replace_text(srow, scol);
    found = TRUE;
  }
  else {
    /* else we need to advance the starting position depending on the
     * search mode and then search. For our data, a forward search is
     * top-to-bottom and then left to right */
      if (Search.mode < 0) /* if row was selected*/
        scol = scol + inc; /* advance 1 column to left or right */
      else
        srow = srow + inc; /* advance the cursor in case we are on a hit */
      search_func();
  }

  /* if Wrap is enabled and we did not find and was not from the beginning */
  if ((Search.Wrap) && (!found || Search.ReplaceAll) && (srow + scol != first_cell )) {
    /* reset starting index to the beginning and search again */
    if (Search.Backword){
      srow = (Search.mode < 0) ? abs(Search.mode) : Search.range.rowi;
      scol = (Search.mode < 0) ? Search.mode : Search.range.coli;
    }
    else {
      srow = (Search.mode < 0) ? abs(Search.mode) : Search.range.row0;
      scol = (Search.mode < 0) ? Search.mode : Search.range.col0;
    }
    search_func();                 /* and try again */
  }
  return found;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void x_find_attribute_value(void)
{
  GtkEntry   *entry;
  const char *text;

  x_find_set_search_parameters();

  entry = GTK_ENTRY(gtk_sheet_get_entry(Search.sheet));
  text  = NULL;

  if (gtk_widget_has_focus (GTK_WIDGET(entry))) {

    int start, end;

    if (gtk_editable_get_selection_bounds (GTK_EDITABLE(entry), &start, &end)) {
      if (end - start > 3) {
        text = gtk_editable_get_chars (GTK_EDITABLE(entry), start, end);
      }
    }
  }

  gtk_sheet_unselect_range(Search.sheet);
  Search.FindOnlyMode=TRUE;
  x_dialog_search_replace(&Search, text);
}

/*! \brief Start Search and Replace
 *  \par Function Description
 *  This is a callback for the menu and tool-bar to launch the bi-modal
 *  Search and Replace dialog in the search and replace mode.
 */
void x_find_replace_attrib_value()
{
  x_find_set_search_parameters();
  gtk_sheet_unselect_range(Search.sheet);
  Search.FindOnlyMode=FALSE;
  x_dialog_search_replace(&Search, NULL);
}

/*! \brief Find Reference Designator
 *  \par Function Description
 *  Opens x_dialog_get_search_text dialog for search string and
 *  searches column labels.
 */
void x_find_attribute()
{
  char *text = x_dialog_get_search_text(_("Attribute"));

  if (text) {

    GtkSheet *sheet;
    bool found;
    int  cur_page;
    int  count;
    int  i;

    cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
    sheet    = sheets[cur_page];
    found    = FALSE;
    count    = sheet->maxcol;

    for( i = 0; i <= count; i++) {

      if (strcmp(sheet->column[i]->title, text) == 0) {
        found = TRUE;
        gtk_sheet_select_column(sheet,i);
        break;
      }
    }

    if (!found)
      x_find_notify_not_found(text);
    GEDA_FREE(text);
  }
}

/*! \brief Find Reference Designator
 *  \par Function Description
 *  Opens x_dialog_get_search_text dialog for search string and
 *  searches row labels.
 */
void x_find_refdes()
{
  char *text = x_dialog_get_search_text(_("Designator"));

  if (text) {

    GtkSheet *sheet;
    bool found;
    int  cur_page;
    int  count;
    int  i;

    cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
    sheet    = sheets[cur_page];
    found    = FALSE;
    count    = sheet->maxrow;

    for( i = 0; i <= count; i++) {

      if (strcmp(sheet->row[i].name, text) == 0) {
        found = TRUE;
        gtk_sheet_select_row(sheet,i);
        break;
      }
    }
    if (!found) x_find_notify_not_found(text);
    GEDA_FREE(text);
  }
}
