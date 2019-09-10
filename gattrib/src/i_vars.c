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

/*! \file
 * \brief Functions for variable setting.
 *
 * Functions for variable setting.
 */

/*------------------------------------------------------------------
 * Gattrib specific includes.  Note that include order is important.
 *------------------------------------------------------------------*/
#include "../include/gattrib.h"
#include "../include/i_vars.h"

/*------------------------------------------------------------------*/
/*
 * Define the vars used later
 *------------------------------------------------------------------*/
/*! width for letter paper (landscape */
int default_paper_width = 11000;

/*! height for letter paper (landscape) */
int default_paper_height = 85000;

int default_sort_components = 0;
int default_tearoff_menus   = 1;

GList *default_hide_columns = NULL;

/*------------------------------------------------------------------*/
/*!
 * \brief Initialize variables in the GedaToplevel
 * \par Function Description
 * Initializes the variables in toplevel. In practice, this is only
 * the paper size for the sheet.
 *
 * \param toplevel pointer to the GedaToplevel to set paper size in.
 */
void i_vars_set (GedaToplevel *toplevel)
{
  toplevel->paper_width = default_paper_width;
  toplevel->paper_height = default_paper_height;

  sort_components = default_sort_components;

  tearoff_menus = default_tearoff_menus;

  hide_columns = default_hide_columns;
}

/*!
 * \brief Release Resources in i_vars
 * \par Function Description
 *  Releases the list of default columns to be initially hidden and
 *  the reference string allocations.
 */
void i_vars_release_all(void)
{
  geda_glist_free_all(default_hide_columns);
  default_hide_columns = NULL;
}
