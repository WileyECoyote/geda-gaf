/*!
 * \file g_register.c
 *
 * \brief Register Scheme Routines in gschlas
 *
 * <hr>
 *
 * <h1><b>Copyright.</b></h1>\n
 * gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 *
 * Copyright (C) 2002-2010 Ales Hvezda
 * Copyright (C) 2002-2015 gEDA Contributors (see ChangeLog for details)
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
 * along with this program; if  not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#include "../include/common.h"

/*! \brief Register Scheme functions
 *  \par Function Description
 * This function registers Scheme functions required to use gschlas,
 * which are necessary for reading the gschlasrc file during initialization
 * which yields the library search paths.
 */
void g_register_funcs(void)
{
  /* general functions */
  scm_c_define_gsubr ("quit", 0, 0, 0, g_quit);
  scm_c_define_gsubr ("exit", 0, 0, 0, g_quit);

  /* gschlas functions */
  scm_c_define_gsubr ("gschlas-version", 1, 0, 0, g_rc_gschlas_version);

}

/*! \brief Scheme function to quit the application
 *
 * Quit the application from within Scheme.
 */
SCM g_quit(void)
{
  gschlas_quit();
  exit(0);
}

