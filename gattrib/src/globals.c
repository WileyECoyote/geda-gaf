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
 *  \brief Global declarations
 *
 * Global declarations
 */

#include <glib.h>

/* command line arguments */
int export_mode;         /*!< Reflects the value of -e command line flag */
int verbose_mode;        /*!< Reflects the value of -v command line flag */
int quiet_mode;          /*!< Reflects the value of -q command line flag */

/* rc variables */
GList *hide_columns;     /*!< Pointer to list of invisible columns */
int    sort_components;  /*!< Determines if sorting list by refdes */
int    tearoff_menus;    /*!< Determines tear-off menus are enabled */
