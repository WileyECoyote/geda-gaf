/* -*- test_color_object.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2018 gEDA Contributors (see ChangeLog for details)
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
 *  Date Contributed: June, 06th, 2018
 */

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>
#include "test-suite.h"

/*! \file test_color_object.c
 *  \brief Tests for o_color.c module
 *  \par
 *  This module provides basic unit tests for functions in the o_color.c
 *  module.
 */

/** \defgroup test-object-geda-color Test GEDA Picture object Module
 * @{
 * \brief Group 7 src/object/o_color.c geda_object_color
 *  Group 7 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  07  88  88
 *                     ^   ^   ^   ^
 *    group-code ______|   |   |   |
 *                         |   |   |
 *    Module/File No. _____|   |   |
 *                             |   |
 *    Function No._____________|   |
 *                                 |
 *    Tests Number ________________|
 *
 *  See tests/README for more details on the nomenclature for test identifiers.
 *
 *      O0701    geda_object_color_get_default
 */

int check_color_get_default (void)
{
  int result = 0;

  return result;
}

/** @} endgroup test-object-geda-color */

int
main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif


  if (setjmp(point) == 0) {
    result = check_color_get_default();
  }
  else {
    fprintf(stderr, "Caught signal checking geda_object_color_get_default\n\n");
    return 1;
  }

  return result;
}
