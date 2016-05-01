/* -*- test_circle_object.c -*-
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
 *  Date Contributed: April, 26th, 2016
 */

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>
#include <math.h>
#include "test-suite.h"

/*! \file test_circle_object.c
 *  \brief Tests for o_circle_object.c module
 *  \par
 *  This module provides basic unit tests for functions in the geda_circle_object_
 *  module.
 */

#define TOBJECT "GedaCircle"

/** \defgroup test-object-geda-circle Test GEDA Circle object Module
 * @{
 * \brief Group 6 src/object/o_circle_object.c geda_circle_object_
 *  Group 6 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  06  88, 88
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
 *      O0601    geda_circle_object_copy
 *               geda_circle_object_get_nearest_point
 *               geda_circle_object_modify
 *               geda_circle_object_mirror
 *               geda_circle_object_new
 *               geda_circle_object_get_position
 *               geda_circle_object_print
 *               geda_circle_object_print_center
 *               geda_circle_object_print_dashed
 *               geda_circle_object_print_dotted
 *               geda_circle_object_print_filled
 *               geda_circle_object_print_hatch
 *               geda_circle_object_print_mesh
 *               geda_circle_object_print_phantom
 *               geda_circle_object_print_solid
 *               geda_circle_object_read
 *               geda_circle_object_rotate
 *               geda_circle_object_save
 *               geda_circle_object_shortest_distance
 *               geda_circle_object_translate
 */

int
check_construction ()
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int c  = m_random_number (0, MAX_COLORS - 1);
    int x1 = m_random_number (0, 115000);
    int y1 = m_random_number (0, 75000);
    int r  = m_random_number (0, 5000);

    /* === Function 05: geda_circle_object_new  === */

    GedaObject *object0 = geda_circle_object_new(c, x1, y1, r);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O060501A) New GedaObject Failed: %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_CIRCLE(object0->circle)) {
      fprintf(stderr, "FAILED: (O060501B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaCircle *circle = object0->circle;

      int       fail = 0;
      int       value;

      value = geda_object_get_color (object0);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-A) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      value = circle->center_x;
      if (value - x1) {
        fprintf(stderr, "FAILED: (O060501CX) circle center_x %d != %d\n", value, x1);
        fail++;
      }

      value = circle->center_y;
      if (value - y1) {
        fprintf(stderr, "FAILED: (O060501CY) circle center_y %d != %d\n", value, y1);
        fail++;
      }

      value = circle->radius;
      if (value - r) {
        fprintf(stderr, "FAILED: (O060501CR) circle radius %d != %d\n", value, r);
        fail++;
      }

      if (!fail) {

        /* === Function 02: geda_circle_object_copy  === */
        GedaObject *object1 = geda_circle_object_copy (object0);
        GedaCircle *circle  = object1->circle;

        value = geda_object_get_color (object0);
        if (value - c) {
          fprintf(stderr, "FAILED: _get_color (%s-B) %d != %d\n", TOBJECT, value, c);
          fail++;
        }

        value = circle->center_x;
        if (value - x1) {
          fprintf(stderr, "FAILED: (O060101CX) circle center_x %d != %d\n", value, x1);
          fail++;
        }

        value = circle->center_y;
        if (value - y1) {
          fprintf(stderr, "FAILED: (O060101CY) circle center_y %d != %d\n", value, y1);
          fail++;
        }

        value = circle->radius;
        if (value - r) {
          fprintf(stderr, "FAILED: (O060101CR) circle radius %d != %d\n", value, r);
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
   ;
  }
  else {
    fprintf(stderr, "discontinuing checks for object/o_circle_object.c\n\n");
  }

  return result;
}