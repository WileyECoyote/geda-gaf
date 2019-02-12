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
 * This module contains functions used in conjunction
 * with setting attribute visibility.
 *
 * \todo refactoring?
 */

#include <gattrib.h>
#include <geda_debug.h>

/*! \brief Return index based on attribute properties
  * \par Function Description
  * This function returns an enumerated color index
  */
int s_properties_get_fgcolor_index(int visibility, int show_name_value, int is_inherited) {

  int fgcolor = 0;

  switch(show_name_value) {
    case (SHOW_VALUE):
      fgcolor = Black;
      break;

    case (SHOW_NAME):
      fgcolor = Red;
      break;

    case (SHOW_NAME_VALUE):
      fgcolor = Blue;
      break;
  }

  if (is_inherited)
    fgcolor = fgcolor + 3;

  if (visibility == INVISIBLE) {
    fgcolor = fgcolor + 6;
  }
  return fgcolor;
}

/*! \brief Return pointer to Table associated with the active sheet
 */
static TABLE **s_properties_get_current_table() {

  int cur_page;
  TABLE **ptr_table = NULL;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

  switch (cur_page) {
    case Components:
      ptr_table = sheet_head->component_table;
      break;

    case Nets:
      ptr_table = sheet_head->net_table;
      break;

    case Pins:
      ptr_table = sheet_head->pin_table;
      break;
  }
  return ptr_table;
}

/* ---------------------------------------------------------------------- */

/*! \brief set visibility of an individual cell
 *
 * Set the visibility of cell identified by (row, col) to the passed value
 * The function is called by the range handler in this module. This has
 * absolutely nothing to do with the cell visibility property, this is for
 * visibility flag associated with geda-gaf attributes, which determines
 * whether or not attributes are visible in the schematic editor.
 *
 * \param row Row index of target cell
 * \param col Column index of target cell
 * \param visibility Visibility value to set cell to
 */
void s_properties_set_cell_visibility(int row, int col, int visibility)
{
  TABLE **local_table = NULL;

#ifdef DEBUG
    printf("In s_properties_set_cell, setting row = %d, col = %d.\n", row, col);
#endif

  local_table = s_properties_get_current_table();

  /* Question:  how to sanity check (row, col) selection? */
  local_table[col][row].visibility = visibility;
  sheet_head->CHANGED = 1;  /* cell has been updated.  */
}

/*! \brief set Show Name Value of an individual cell
 *
 *  \par Function Description
 * Set the value of the Show Name flag for the cell identified by (row, col)
 * to the passed value. The function is called by the range handler in this
 * module. The value of the Show Name flag determines whether editor should
 * display the Attribute Name, the Value or Both.
 *
 * \param row             Row index of target cell
 * \param col             Column index of target cell
 * \param show_name_value value to set the Show Name flag
 */
void s_properties_set_cell_show_name(int row, int col, int show_name_value)
{
  TABLE **local_table = NULL;

  local_table = s_properties_get_current_table();

  if (show_name_value != LEAVE_NAME_VALUE_ALONE) {
    local_table[col][row].show_name_value = show_name_value;
    sheet_head->CHANGED = 1;  /* cell has been updated.  */
  }
}

/*! \brief Returns the current visibility setting of a cell */
bool s_properties_get_visibility(int row, int col) {

  TABLE **local_table = NULL;

  local_table = s_properties_get_current_table();

  return local_table[col][row].visibility;
}

/*! \brief Returns the current Show Name value of a cell */
int s_properties_get_show_name_value(int row, int col) {

  TABLE **local_table = NULL;

  local_table = s_properties_get_current_table();

  return local_table[col][row].show_name_value;
}

/*! \brief Returns heredity of the current cell */
int s_properties_get_heritence(int row, int col) {

  TABLE **local_table = NULL;

  local_table = s_properties_get_current_table();

  return local_table[col][row].is_inherited;
}

/*! \brief Set the Foreground color of the cell
  * \par Function Description
  *      This function retrieves the color index based on the current
  * visibility and show_name_value and calls a function in x_gtksheet
  * to set the color of the cell identified row, col.
  */
void s_properties_set_cell_fgcolor(GtkSheet *sheet, int row, int col) {

  int fgcolor;

  int visibility = s_properties_get_visibility(row, col);
  int show_name_value = s_properties_get_show_name_value(row, col);
  int is_inherited = s_properties_get_heritence(row, col);

  fgcolor = s_properties_get_fgcolor_index(visibility, show_name_value, is_inherited);

  x_gtksheet_set_cell_fgcolor(sheet, row, col, fgcolor);
}

/* ------------------ Visibility Range Operators ----------------- */

/*! \brief Set visibility of selected cells
 *
 * This function sets the visibility of selected cells to the passed
 * value, either VISIBLE or INVISIBLE. This has nothing to do with the
 * cell visibility property. This function is called from the menu
 * handlers in this modules.
 */
static void s_properties_set_range_visibility(int visibility) {

  int i, j;
  int row_start, row_end, col_start, col_end;
  GtkSheet *sheet;
  int cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

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
        /* first set cell in SHEET_DATA to invisible */
        s_properties_set_cell_visibility(i, j, visibility);
        /* Now set cell in gtksheet to desired color */
        /* Color names are defined in gattrib/include/globals.h */
        s_properties_set_cell_fgcolor(sheet, i, j);
      }
    }
    /* Now return sheet to normal -- unselect range */
    gtk_sheet_unselect_range (sheet);
    break;

  case GTK_SHEET_NORMAL:
    s_properties_set_cell_visibility(sheet->active_cell.row,
                                     sheet->active_cell.col,
                                     visibility);

    s_properties_set_cell_fgcolor(sheet,
                                  sheet->active_cell.row,
                                  sheet->active_cell.col);

    break;
  }
  x_window_update_title(pr_current, sheet_head);
}

/* ---------------------------------------------------------------------- */

/*!
 * \brief Set Show Name Value of selected cells.
 * \par Function Description
 *  This functions sets the show_name attrinbute of the selected cells
 *  to the value passed. This function is invoked from the menu handlers
 *  in this module.
 */
static void s_properties_set_show_name_value(int value) {

  GtkSheet *sheet;
  int cur_page;
  int i, j;
  int row_start, row_end, col_start, col_end;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

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
        s_properties_set_cell_show_name( i, j, value);
        /* Color names are defined in libgedacolor/include/geda_colors.h */
        s_properties_set_cell_fgcolor(sheet, i, j);
      }
    }
    /* Now return sheet to normal -- unselect range */
    gtk_sheet_unselect_range (sheet);

    break;

  case GTK_SHEET_NORMAL:
    s_properties_set_cell_show_name(sheet->active_cell.row,
                                    sheet->active_cell.col,
                                    value);
    s_properties_set_cell_fgcolor(sheet,
                                  sheet->active_cell.row,
                                  sheet->active_cell.col);
    break;

  }
  x_window_update_title(pr_current, sheet_head);
}

/* ------------------------ Menu and Toolbar Handler ------------------ */

/*!
 * \brief Set selection to INVISIBLE.
 * \par Function Description
 *  This sets the selected cells to INVISIBLE.
 */
void s_properties_set_invisible(void) {
  s_properties_set_range_visibility(INVISIBLE);
}

/*!
 * \brief Set selection to VISIBLE.
 * \par Function Description
 *  This sets the selected cells to VISIBLE.
 */
void s_properties_set_visible(void) {
  s_properties_set_range_visibility(VISIBLE);
}

/* ------------------------- SHOW NAME ---------------------------- */

/*!
 * \brief Set selected Attributes display only the Name of the attribute.
 * \par Function Description
 *  This func is invoked from the menu or the toolbar and sets selected
 *  cells to Show Name.
 */
void s_properties_set_name_only(void) {
  s_properties_set_show_name_value(SHOW_NAME);
}

/*!
 * \brief Set selected Attributes display only the Value of the attribute.
 * \par Function Description
 *  This func is invoked from the menu or the toolbar and sets selected
 *  cells to Show Value.
 */
void s_properties_set_value_only(void) {
  s_properties_set_show_name_value(SHOW_VALUE);
}

/*!
 * \brief Set selected Attributes display the Name and Value of the attribute.
 * \par Function Description
 *  This func is invoked from the menu or the toolbar and sets selected
 *  cells to Show Name and Value.
 */
void s_properties_set_name_and_value(void) {
  s_properties_set_show_name_value(SHOW_NAME_VALUE);
}

/*!
 * \brief Set selected Attributes display the Name and Value of the attribute.
 * \par Function Description
 *  This func is invoked from the menu or the toolbar and sets selected
 *  cells to Show Name and Value.
 */
void s_properties_promote_attribute(void) {
  int row, col;
  int cur_page;
  GtkSheet *sheet;
  TABLE **local_table = NULL;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

  row = sheet->active_cell.row;
  col = sheet->active_cell.col;

  local_table = s_properties_get_current_table();
  local_table[col][row].is_inherited = FALSE;
  local_table[col][row].is_promoted = TRUE;

  s_properties_set_cell_fgcolor(sheet, row, col);
}

/*!
 * \brief Demote an Atttribute
 * \par Function Description
 *  Changes the properties of the attribute reference by
 *  the cell in the local_table so that the attribute is
 *  not directly attached to the complex for which it is
 *  associated.
 */
void s_properties_demote_attribute(void) {

  int row, col;
  int cur_page;
  GtkSheet *sheet;
  TABLE **local_table = NULL;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

  row = sheet->active_cell.row;
  col = sheet->active_cell.col;

  local_table = s_properties_get_current_table();

  /* we only demote attributes previously having been promote */
  if(local_table[col][row].is_promoted > 0) {
    local_table[col][row].is_inherited = TRUE;
    local_table[col][row].is_promoted = FALSE;
  }

  s_properties_set_cell_fgcolor(sheet, row, col);
}
