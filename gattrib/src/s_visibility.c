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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions to manipulate attribute visibility
 *
 * This file holds widgets and functions used in conjunction
 * with setting attribute visibility.
 * \todo There seems to be a lot of duplicated code in this file -
 *       a good candidate for refactoring.
 */

#include <gattrib.h>
#include <geda_debug.h>

/* ----- s_visibility stuff begins here ----- */

/* ---------------------------------------------------------------------- */
/*!
 * \brief Set the selected cells to INVISIBLE
 * \par Function Description
 *  This function is called from the menu to set the selected
 *  cells to INVISIBLE, assumming there is selected a range of
 *  cells which are carried in the global variable "sheet".
 */
void s_visibility_set_invisible() {
  int i, j;
  int row_start, row_end, col_start, col_end;
  GtkSheet *sheet;
  int cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  switch (sheet->state) {

  case GTK_SHEET_RANGE_SELECTED:
  case GTK_SHEET_COLUMN_SELECTED:
  case GTK_SHEET_ROW_SELECTED:

    row_start = sheet->range.row0;
    row_end   = sheet->range.rowi;
    col_start = sheet->range.col0;
    col_end   = sheet->range.coli;

    for (i=row_start; i<=row_end; i++) {
      for (j=col_start; j<=col_end; j++) {
        /* first set cell in SHEET_DATA to invisible */
        s_visibility_set_cell(cur_page, i, j, INVISIBLE, LEAVE_NAME_VALUE_ALONE);
        /* Now set cell in gtksheet to desired color */
        /* Color names are defined in gattrib/include/globals.h */
        x_gtksheet_set_cell_fgcolor(sheet, i, j, Gray);
      }
    }

    /* Now return the sheet to normal -- unselect range */
    gtk_sheet_unselect_range (sheet);
    break;

  case GTK_SHEET_NORMAL:
#ifdef DEBUG
    printf("In s_visibility_set_invisible, normal selection.\n");
#endif
    s_visibility_set_cell(cur_page,
                          sheet->active_cell.row,
                          sheet->active_cell.col,
                          INVISIBLE,
                          LEAVE_NAME_VALUE_ALONE);

    x_gtksheet_set_cell_fgcolor(sheet,
                                sheet->active_cell.row,
                                sheet->active_cell.col,
                                Gray);

    break;
  }
  x_window_update_title(pr_current, sheet_head);
}

/* ---------------------------------------------------------------------- */
/*!
 * \brief Set the visibility of the selected cells to NAME_ONLY.
 * \par Function Description
 *  This sets the selected cells to NAME_ONLY. This function is
 *  invoked from the menu, it assumes that a range is selected,
 *  which are carried in the global variable "sheet".
 */
void s_visibility_set_name_only() {
  GtkSheet *sheet;
  int i, j;
  int row_start, row_end, col_start, col_end;
  int cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  switch (sheet->state) {

  case GTK_SHEET_RANGE_SELECTED:
  case GTK_SHEET_COLUMN_SELECTED:
  case GTK_SHEET_ROW_SELECTED:
#ifdef DEBUG
    printf("In s_visibility_set_name_only, range/col/row selected.\n");
#endif
    row_start = sheet->range.row0;
    row_end = sheet->range.rowi;
    col_start = sheet->range.col0;
    col_end = sheet->range.coli;
    for (i=row_start; i<=row_end; i++) {
      for (j=col_start; j<=col_end; j++) {
        s_visibility_set_cell(cur_page, i, j, VISIBLE, SHOW_NAME);
        /* Color names are defined
         * in libgeda/include/geda_colors.h */
        x_gtksheet_set_cell_fgcolor(sheet, i, j, Red);
      }
    }

    /* Now return the sheet to normal -- unselect range */
    gtk_sheet_unselect_range (sheet);

    break;

  case GTK_SHEET_NORMAL:
    s_visibility_set_cell(cur_page,
                          sheet->active_cell.row,
                          sheet->active_cell.col,
                          VISIBLE, SHOW_NAME);
    x_gtksheet_set_cell_fgcolor(sheet,
                                sheet->active_cell.row,
                                sheet->active_cell.col,
                                Red);

    break;

  }
  x_window_update_title(pr_current, sheet_head);
}

/* ---------------------------------------------------------------------- */
/*!
 * \brief Set the selected cells' visibility to VALUE_ONLY
 * \par Function Description
 *  s_visibility_set_value_only -- This sets the selected cells to
 *  VALUE_ONLY. * This fcn is invoked from the menu, it assumes you
 *  have selected a range of cells which are carried in the global
 *  variable "sheet".
 */
void s_visibility_set_value_only() {
  GtkSheet *sheet;
  int i, j;
  int row_start, row_end, col_start, col_end;
  int cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

#ifdef DEBUG
  printf("%s: sheet->state [%s]\n", __func__, sheet->state);
#endif

  switch (sheet->state) {

    case GTK_SHEET_RANGE_SELECTED:
    case GTK_SHEET_COLUMN_SELECTED:
    case GTK_SHEET_ROW_SELECTED:

      row_start = sheet->range.row0;
      row_end = sheet->range.rowi;
      col_start = sheet->range.col0;
      col_end = sheet->range.coli;
      for (i=row_start; i<=row_end; i++) {
        for (j=col_start; j<=col_end; j++) {
          s_visibility_set_cell(cur_page, i, j, VISIBLE, SHOW_VALUE);
          /* Color names are defined in geda_colors.h */
          x_gtksheet_set_cell_fgcolor(sheet, i, j, Black);
        }
      }
      /* Now return sheet to normal -- unselect range */
      gtk_sheet_unselect_range (sheet);

      break;

    case GTK_SHEET_NORMAL:
      s_visibility_set_cell(cur_page,
                            sheet->active_cell.row,
                            sheet->active_cell.col,
                            VISIBLE, SHOW_VALUE);

      x_gtksheet_set_cell_fgcolor(sheet,
                                  sheet->active_cell.row,
                                  sheet->active_cell.col,
                                  Black);
      break;

  }
  x_window_update_title(pr_current, sheet_head);
}

/* ---------------------------------------------------------------------- */

/*!
 * \brief Set the visibility of the selected cells to NAME_AND_VALUE
 * \par Function Description
 *  This sets the selected cells to NAME_AND_VALUE This fcn is invoked
 *  from the menu, it assumes you have selected a range of cells which
 *  are carried in the global variable "sheet".
 */
void s_visibility_set_name_and_value(void)
{
  int i, j;
  int row_start, row_end, col_start, col_end;
  GtkSheet *sheet;
  int cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  switch (sheet->state) {

    case GTK_SHEET_RANGE_SELECTED:
    case GTK_SHEET_COLUMN_SELECTED:
    case GTK_SHEET_ROW_SELECTED:
      row_start = sheet->range.row0;
      row_end = sheet->range.rowi;
      col_start = sheet->range.col0;
      col_end = sheet->range.coli;
      for (i=row_start; i<=row_end; i++) {
        for (j=col_start; j<=col_end; j++) {
          s_visibility_set_cell(cur_page, i, j, VISIBLE, SHOW_NAME_VALUE);
          /* Color names are defined
           * in libgeda/include/geda_colors.h */
          x_gtksheet_set_cell_fgcolor(sheet, i, j, Blue);
        }
      }
      /* Now return sheet to normal -- unselect range */
      gtk_sheet_unselect_range (sheet);

      break;

    case GTK_SHEET_NORMAL:
      s_visibility_set_cell(cur_page,
                            sheet->active_cell.row,
                            sheet->active_cell.col,
                            VISIBLE,
                            SHOW_NAME_VALUE);
      x_gtksheet_set_cell_fgcolor(sheet,
                                  sheet->active_cell.row,
                                  sheet->active_cell.col,
                                  Blue);

      break;

  }
  x_window_update_title(pr_current, sheet_head);
}


/* ==================  Private functions  =================== */

/* ---------------------------------------------------------------------- */
/*!
 * \brief set the visibility of an individual cell
 * \par Function Description
 *  Set the visibility of an individual cell
 *  to "state".  The cell is identified by (row, col)
 *
 * \param cur_page index of spreadsheet tab
 * \param row Row index of target cell
 * \param col Column index of target cell
 * \param visibility Visibility value to set cell to
 * \param show_name_value Name, Value visibility flag
 */
void s_visibility_set_cell(int cur_page, int row, int col,
                           int visibility,
                           int show_name_value)
{
  TABLE **local_table = NULL;

#ifdef DEBUG
    printf("In %s, setting row = %d, col = %d.\n", __func__, row, col);
#endif

  switch (cur_page) {

  case 0:
    local_table = sheet_head->component_table;
    break;

  case 1:
    local_table = sheet_head->net_table;
    break;

  case 2:
    local_table = sheet_head->pin_table;
    break;
  }

  /* Question:  how to sanity check (row, col) selection? */
  local_table[col][row].visibility;
  sheet_head->CHANGED = 1;  /* cell has been updated.  */

  if (show_name_value != LEAVE_NAME_VALUE_ALONE) {
    local_table[col][row].show_name_value = show_name_value;
    sheet_head->CHANGED = 1;  /* cell has been updated.  */
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
bool s_visibility_get_cell(int cur_page, int row, int col)
{
  TABLE **local_table = NULL;

  switch (cur_page) {

  case 0:
    local_table = sheet_head->component_table;
    break;

  case 1:
    local_table = sheet_head->net_table;
    break;

  case 2:
    local_table = sheet_head->pin_table;
    break;
  }

  return local_table[col][row].visibility;
}
