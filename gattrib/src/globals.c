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

#include <gattrib.h>
#include <geda_debug.h>

/* command line arguments */
int verbose_mode; //!< Reflects the value of the command line flag
int quiet_mode;   //!< Reflects the value of the command line flag

/* rc variables */
int sort_components;
int tearoff_menus;

/*!
 * these are required by libgeda
 * I have made most of these NULL because they aren't needed
 * for gattrib -- no drawing is done.
 */
void (*variable_set_func)() = i_vars_set;

