/* -*- test_line_object.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 *  Date Contributed: March, 17th, 2016
 */

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>
#include <math.h>
#include "test-suite.h"

/*! \file test_line_object.c
 *  \brief Tests for o_line_object.c module
 *  \par
 *  This module provides basic unit tests for functions in the
 *  geda_line_object_module.
 */

#define TOBJECT "GedaLine"

/** \defgroup test-object-geda-circle Test GEDA Circle object Module
 * @{
 * \brief Group 10 src/object/o_line_object.c geda_line_object_
 *  Group 10 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  11  88, 88
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
 *      O1001    geda_line_object_copy
 *               geda_line_object_get_closest_endpoint
 *               geda_line_object_get_end_cap
 *               geda_line_object_get_line_length
 *               geda_line_object_get_line_space
 *               geda_line_object_get_line_type
 *               geda_line_object_get_line_width
 *               geda_line_object_get_intersection
 *               geda_line_object_get_midpoint
 *               geda_line_object_get_nearest_point
 *               geda_line_object_get_position
 *               geda_line_object_get_slope
 *               geda_line_object_get_x1
 *               geda_line_object_get_x2
 *               geda_line_object_get_y1
 *               geda_line_object_get_y2
 *               geda_line_object_is_endpoint
 *               geda_line_object_length
 *               geda_line_object_mirror
 *               geda_line_object_modify
 *      O1021    geda_line_object_new
 *               geda_line_object_print
 *               geda_line_object_print_center
 *               geda_line_object_print_dashed
 *               geda_line_object_print_dotted
 *               geda_line_object_print_phantom
 *               geda_line_object_print_solid
 *               geda_line_object_read
 *               geda_line_object_rotate
 *               geda_line_object_set_end_cap
 *               geda_line_object_set_line_length
 *               geda_line_object_set_line_space
 *               geda_line_object_set_line_type
 *               geda_line_object_set_line_width
 *               geda_line_object_set_x1
 *               geda_line_object_set_x2
 *               geda_line_object_set_y1
 *               geda_line_object_set_y2
 *               geda_line_object_scale
 *               geda_line_object_shortest_distance
 *               geda_line_object_to_buffer
 *               geda_line_object_translate
 */

int
check_construction ()
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int c  = m_random_number (0,  MAX_COLORS - 1);
    int x0 = m_random_number (0,  115000);
    int y0 = m_random_number (0,  75000);
    int x1 = m_random_number (x0, 120000);
    int y1 = m_random_number (y0, 80000);

    /* === Function 21: geda_circle_object_new  === */

    GedaObject *object0 = geda_line_object_new(c, x0, y0, x1, y1);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O102101A) New GedaObject Failed: %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_LINE(object0->line)) {
      fprintf(stderr, "FAILED: (O102101B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaLine *line = object0->line;

      int       fail = 0;
      int       value;

      value = geda_object_get_color (object0);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-A) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      value = line->x[0];
      if (value - x0) {
        fprintf(stderr, "FAILED: (O102101CX) line first x %d != %d\n", value, x0);
        fail++;
      }

      value = line->y[0];
      if (value - y0) {
        fprintf(stderr, "FAILED: (O102101CY) line first y %d != %d\n", value, y0);
        fail++;
      }

      value = line->x[1];
      if (value - x1) {
        fprintf(stderr, "FAILED: (O102101CX) line second x %d != %d\n", value, x1);
        fail++;
      }

      value = line->y[1];
      if (value - y1) {
        fprintf(stderr, "FAILED: (O102101CY) line second y %d != %d\n", value, y1);
        fail++;
      }

      if (!fail) {

        /* === Function 01: geda_line_object_copy  === */

        GedaObject *object1 = geda_line_object_copy (object0);
        GedaLine   *line2   = object1->line;

        value = geda_object_get_color (object0);
        if (value - c) {
          fprintf(stderr, "FAILED: _get_color (%s-B) %d != %d\n", TOBJECT, value, c);
          fail++;
        }

        value = line2->x[0];
        if (value - x0) {
          fprintf(stderr, "FAILED: (O060101CX) line first x %d != %d\n", value, x0);
          fail++;
        }

        value = line2->y[0];
        if (value - y0) {
          fprintf(stderr, "FAILED: (O060101CY) line first y %d != %d\n", value, y0);
          fail++;
        }

        value = line2->x[1];
        if (value - x1) {
          fprintf(stderr, "FAILED: (O060101CX) line second x %d != %d\n", value, x1);
          fail++;
        }

        value = line2->y[1];
        if (value - y1) {
          fprintf(stderr, "FAILED: (O060101CY) line second y %d != %d\n", value, y1);
          fail++;
        }
      }

      if (fail) {
        result++;
        break;
      }
    }
    g_object_unref (object0);
  }
  return result;
}

int
check_accessors ()
{
  int count;
  int result = 0;

  return result;
}

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
    result = check_construction();
  }
  else {
    fprintf(stderr, "Caught signal in constructors %s\n\n", __FILE__);
    return 1;
  }

  if (!result) {

    if (setjmp(point) == 0) {
      result = check_accessors();
    }
    else {
      fprintf(stderr, "Caught signal checking accessors in object/o_line_object.c\n\n");
      return 1;
    }
  }
  else {
    fprintf(stderr, "discontinuing checks for object/o_line_object.c\n\n");
  }

  return result;
}