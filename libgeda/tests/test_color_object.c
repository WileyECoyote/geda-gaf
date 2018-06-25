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

#include "../../config.h"

#include <libgeda.h>

#include <o_types.h>
#include <geda_colors.h>

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
  int index;

  index = geda_object_color_get_default (OBJ_LINE);

  if (index != DEFAULT_LINE_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010101) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_PATH);

  if (index != DEFAULT_PATH_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010102) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_BOX);

  if (index != DEFAULT_BOX_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010103) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_PICTURE);

  if (index != DEFAULT_PICTURE_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010104) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_CIRCLE);

  if (index != DEFAULT_CIRCLE_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010105) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_NET);

  if (index != DEFAULT_NET_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010106) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_BUS);

  if (index != DEFAULT_BUS_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010107) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_COMPLEX);

  if (index != DEFAULT_COMPLEX_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010108) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_TEXT);

  if (index != DEFAULT_TEXT_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010109) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_PIN);

  if (index != DEFAULT_PIN_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010110) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_ARC);

  if (index != DEFAULT_ARC_COLOR_INDEX) {
    fprintf(stderr, "FAILED: (F010111) geda_object_color_get_default <%d>\n", index);
    result++;
  }

  index = geda_object_color_get_default(OBJ_PLACEHOLDER);
  if (index != -1) {
    fprintf(stderr, "FAILED: (F010112) geda_object_color_get_default <%d>\n", index);
    result++;
  }

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
