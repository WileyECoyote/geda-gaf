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
 * \brief Functions to register Scheme functions
 *
 * Functions to register Scheme functions
 */

#include <gattrib.h>

/* ---------------------------------------------------------------------- */
/*! \brief Register Scheme functions
 *
 * This function registers the Scheme functions required to use
 * gattrib.  They are mostly unnecessary, except for reading in the gattribrc
 * file at the beginning of the prog which gives the library search paths.
 */
void g_register_funcs(void)
{
  /* general functions */
  scm_c_define_gsubr ("quit", 0, 0, 0, g_quit);
  scm_c_define_gsubr ("exit", 0, 0, 0, g_quit);

  scm_c_define_gsubr ("hide-columns", 1, 0, 0, g_rc_hide_columns);
  scm_c_define_gsubr ("sort-components", 1, 0, 0, g_rc_sort_components),
  scm_c_define_gsubr ("tearoff-menus", 1, 0, 0, g_rc_tearoff_menus),

  /* gattrib functions */
  scm_c_define_gsubr ("gattrib-version", 1, 0, 0, g_rc_gattrib_version);
}

/*! \brief Scheme function to quit the application
 *
 * Quit the application from within Scheme.
 */
SCM g_quit(void)
{
#ifdef DEBUG
  printf("In g_quit, calling exit(0)\n");
#endif

  gattrib_quit(0);
  /*  exit(0);  */  /* Necessary? */

  /* we don't really get here, but otherwise gcc complains */
  return SCM_BOOL_F;
}

