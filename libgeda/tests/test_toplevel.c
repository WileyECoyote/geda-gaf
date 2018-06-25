/* -*- test_toplevel.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: December, 1st, 2016
 */

#include "../../config.h"

#include <libgeda.h>

#define TOBJECT "GedaToplevel"

/*! \file test_toplevel.c
 *  \brief Tests for geda_toplevel.c module
 *  \par
 *  This module provides basic unit tests for construction and destruction
 *  of GedaToplevel objects.
 */


int check_toplevel (void)
{
  int result = 0;

  GedaToplevel *toplevel = geda_toplevel_new();

  if (!GEDA_IS_TOPLEVEL(toplevel)) {
    fprintf(stderr, "Is a %s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  if (GEDA_IS_PAGE(toplevel)) {
    fprintf(stderr, "%s matched type Page\n", TOBJECT);
    result++;
  }

  g_object_unref(toplevel);

  if (GEDA_IS_TOPLEVEL(toplevel)) {
    fprintf(stderr, "%s toplevel was not destroyed\n", TOBJECT);
    result++;
  }

  return result;
}

int
check_accessors ()
{
  int result = 0;

  GedaToplevel *toplevel = geda_toplevel_new();

  if (!GEDA_IS_TOPLEVEL(toplevel)) {
    fprintf(stderr, "Is a %s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {
    g_object_unref(toplevel);
  }

  return result;
}

int
check_properties (void)
{
  int result = 0;

  GedaToplevel *toplevel = geda_toplevel_new();

  if (!GEDA_IS_TOPLEVEL(toplevel)) {
    fprintf(stderr, "Is a %s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {
    g_object_unref(toplevel);
  }

  return result;
}

int
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = check_toplevel();

  result += check_properties();

  result += check_accessors();

  if (result) {
    fprintf(stderr, "Check module geda_toplevel.c");
  }

  return (result > 0);
}
