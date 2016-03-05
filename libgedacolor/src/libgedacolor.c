/* -*- C indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-*/
/*
 * File: libgedacolor.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedacolor - gEDA's Extension library for Color
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Contributing Author: Wiley Edward Hill
 * Date Contributed: September, 15, 2015
 */

#include <config.h>

//#include <geda_standard.h>

#include <gdk/gdk.h>

#include <libgeda/libgeda.h>

#include "../include/libgedacolor.h"
#include "../include/globals.h"
#include "../include/private.h"
#include "../include/gettext_priv.h"

#include <geda_debug.h>

/*! \brief Perform Guile runtime initialization of libgeda library.
 *  \par Function Description
 *  This function is called internally by libgeda_init using the
 *  scm_with_guile function to initialize <b>Guile</b> runtime
 *  routines. This function does not require any arguments, nor
 *  does the function return a meaningful value. The argument and
 *  return pointer is a requirement of the scm_with_guile
 *  function.
 *
 *  \sa libgedacolor_init
 *
 */
static void *guile_init(void *lame)
{
  geda_color_guile_register_handlers();
  return lame;
}

/*! \brief Perform runtime initialization of libgeda library.
 *  \par Function Description
 *  This function calls "satellite" initialization functions in
 *  various modules to initialize data structures for runtime.
 *  This function should normally be called before any other
 *  libgeda functions are called. The call scm_with_guile is
 *  used to ensure we are in guile mode, regardless of whether
 *  the client is in guile mode.
 *
 */
int libgedacolor_init(int *argc, char **argv)
{
  int lame;

  if (gdk_init_check (argc, &argv)) {

    /* Initialize scheme even if client has not booted Guile */
    scm_with_guile(guile_init, &lame);

    geda_color_x11_init();
    geda_color_struct_init();

    geda_color_x11_allocate();
  }
  else {
    fprintf (stderr, "FAILED: gdk_init_check\n");
    return FALSE;
  }
  return TRUE;
}

int geda_color_load_display_scheme (char *scheme)
{
  return geda_color_x11_load_scheme(scheme);
}


/*! \brief Loads and executes a color map scheme
 *  \par Function Description
 *       This function executes a color map scm file after
 *       verifying accessibility. The file must be referenced
 *       relative to the path returned by geda-rc-path. The
 *       current colors are free and the new color allocated.
 */
int geda_color_load_print_scheme(char *scheme) {

  char *inputfile;
  int   result;

  inputfile = f_get_data_filespec(scheme);

  if (inputfile) {

    if (geda_color_guile_load_scheme(inputfile)) {

      result = TRUE;
    }
    else {
      u_log_message (_("Something went wrong, check:%s\n"), scheme);
      result = FALSE;
    }
  }
  else {
    u_log_message (_("%s: Could not locate file:%s\n"), __func__,scheme);
    result = FALSE;
  }

  GEDA_FREE(inputfile);

  return result;
}

void libgedacolor_release(void)
{
  geda_color_struct_release_resources();
  geda_color_x11_release_resources();
}

