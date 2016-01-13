/* -*- u_program.c -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
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
 *
 * Contributing Author: Wiley Edward Hill
 * Date Contributed: April, 06, 2014
 */

#ifdef HAVE_CONFIG_H
#include "../../config.h"
#endif

#include <geda_standard.h>

#include <glib.h>

#include <execinfo.h>

#include <geda_debug.h>

#define BACK_TRACE_SIZE 20

static GMemVTable memvtable;

static void inline traceback(void)  /* "static" means don't export the symbol... */
{
  int j, nptrs;

  void *buffer[BACK_TRACE_SIZE];
  char **strings;

  nptrs = backtrace(buffer, BACK_TRACE_SIZE);
  printf("backtrace() returned %d addresses\n", nptrs);

  /* The call backtrace_symbols_fd(buffer, nptrs, STDOUT_FILENO)
   *              would produce similar output to the following: */

  strings = backtrace_symbols(buffer, nptrs);
  if (strings == NULL) {
    perror("backtrace_symbols");
    exit(EXIT_FAILURE);
  }

  /* Note we start at 1 so as to skip ourself */
  for (j = 1; j < nptrs; j++)
    printf("%s\n", strings[j]);

  free(strings);
}

/*! \brief Wrapper for Backtrace Utilitity
 *
 *  \par Function Description
 *  This is a utlitity function that calls the traceback function
 * to show the call sequence. The function servers as a as a built
 * in debugging tool that can be used by application* of library
 * functions after detecting an incorrect parameter as means to
 * locate errent routines.
 *
 */
void u_program_backtrace(void)
{
  traceback();
}

/*! \brief Setup GLib Memory Table
 *
 *  \par Function Description
 *  This function provides a pre-set memory table to Glibc memory
 * functions in order to set \a GLIB to only use \a GLIBC memory
 * routines. This would normally be the default but GLIB does not
 * provide any other means to insure GLIB is not using non-standard
 * routines. GLIBs g_mem_is_system_malloc() is pretty much useless
 * as run-time is a little too late to find out we are not using
 * GLIBC.
 * \note As a bonus, if applications call this function then malloc
 * and g_malloc, free and g_free can be freely mixed. Guile uses
 * GLIBC, and not GLIB.
 */
void u_program_mem_set_vtable(void)
{
   memvtable.malloc      = malloc;
   memvtable.realloc     = realloc;
   memvtable.free        = free;
   memvtable.calloc      = calloc;
   memvtable.try_malloc  = 0;
   memvtable.try_realloc = 0;
   g_mem_set_vtable (&memvtable);
}

/* Virutal over-ride, because we set the vtable, glib incorrectly
 * reports that we are not using malloc, but we are. Setting the
 * vtable forces glib to use malloc even when glib would not other
 * wise do so. This slows down glib but not for us, when linked
 * against libgeda g_mem_is_system_malloc correctly reports that
 * malloc is being used, that's because the linker must respect
 * our version of ....
 */
int
g_mem_is_system_malloc (void)
{
  return TRUE;
}
