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

/** \defgroup test-object-geda-line Test GEDA Line object Module
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
 *      O1003    geda_line_object_get_end_cap
 *      O1004    geda_line_object_get_line_length
 *      O1005    geda_line_object_get_line_space
 *      O1006    geda_line_object_get_line_type
 *      O1007    geda_line_object_get_line_width
 *               geda_line_object_get_intersection
 *               geda_line_object_get_midpoint
 *               geda_line_object_get_nearest_point
 *               geda_line_object_get_position
 *               geda_line_object_get_slope
 *      O1013    geda_line_object_get_x1
 *      O1014    geda_line_object_get_x2
 *      O1015    geda_line_object_get_y1
 *      O1016    geda_line_object_get_y2
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
 *      O1030    geda_line_object_set_end_cap
 *      O1031    geda_line_object_set_line_length
 *      O1032    geda_line_object_set_line_space
 *      O1033    geda_line_object_set_line_type
 *      O1034    geda_line_object_set_line_width
 *      O1035    geda_line_object_set_x1
 *      O1036    geda_line_object_set_x2
 *      O1037    geda_line_object_set_y1
 *      O1038    geda_line_object_set_y2
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

    /* === Function 21: geda_line_object_new  === */

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

  if (geda_line_object_get_x1(NULL)) {
    fprintf(stderr, "FAILED: (O101300) %s first x not zero\n", TOBJECT);
    result++;
  }

  if (geda_line_object_get_x2(NULL)) {
    fprintf(stderr, "FAILED: (O101400) %s second x not zero\n", TOBJECT);
    result++;
  }

  if (geda_line_object_get_y1(NULL)) {
    fprintf(stderr, "FAILED: (O101500) %s first x not zero\n", TOBJECT);
    result++;
  }

  if (geda_line_object_get_y2(NULL)) {
    fprintf(stderr, "FAILED: (O101600) %s second x not zero\n", TOBJECT);
    result++;
  }

  for (count = 0; count < 3; count++) {

    GedaObject *object0 = geda_line_object_new (3, 11, 21, 31, 41);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O102101C) New GedaObject Failed\n");
      result++;
      break;
    }
    else if (!GEDA_IS_LINE(object0->line)) {
      fprintf(stderr, "FAILED: (O102101D) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int fail;
      int value;

      fail = 0;

      int c  = m_random_number (0,  MAX_COLORS - 1);
      int x0 = m_random_number (0,  115000);
      int y0 = m_random_number (0,  75000);
      int x1 = m_random_number (x0, 120000);
      int y1 = m_random_number (y0, 80000);

      /* Line type options */
      int e = m_random_number (END_NONE, END_ROUND);
      int t = m_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = m_random_number (0, 500);
      int p = m_random_number (0, 500);
      int w = m_random_number (0, 500);

      o_set_color (object0, c);

      geda_line_object_set_x1 (object0, x0);
      geda_line_object_set_y1 (object0, y0);
      geda_line_object_set_x2 (object0, x1);
      geda_line_object_set_y2 (object0, y1);

      value = geda_object_get_color (object0);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-C) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      value = object0->line->x[0];
      if (value - x0) {
        fprintf(stderr, "FAILED: (O103501) set_x1 %d != %d\n", value, x0);
        fail++;
      }

      value = object0->line->x[1];
      if (value - x1) {
        fprintf(stderr, "FAILED: (O103601) set_x2 %d != %d\n", value, x1);
        fail++;
      }

      value = object0->line->y[0];
      if (value - y0) {
        fprintf(stderr, "FAILED: (O103701) set_y1 %d != %d\n", value, y0);
        fail++;
      }

      value = object0->line->y[1];
      if (value - y1) {
        fprintf(stderr, "FAILED: (O103801) set_y2 %d != %d\n", value, y1);
        fail++;
      }

      value = geda_line_object_get_x1(object0);
      if (value - x0) {
        fprintf(stderr, "FAILED: (O101301) get x1 %d != %d\n", value, x0);
        fail++;
      }

      value = geda_line_object_get_x2(object0);
      if (value - x1) {
        fprintf(stderr, "FAILED: (O101401) get x2 %d != %d\n", value, x1);
        fail++;
      }

      value = geda_line_object_get_y1(object0);
      if (value - y0) {
        fprintf(stderr, "FAILED: (O101501) get y1 %d != %d\n", value, y0);
        fail++;
      }

      value = geda_line_object_get_y2(object0);
      if (value - y1) {
        fprintf(stderr, "FAILED: (O101601) get y2 %d != %d\n", value, y1);
        fail++;
      }

      /* Check line type properties */

      geda_line_object_set_end_cap (object0, e);

      value = object0->line_options->line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: (O103001) %d != %d\n", value, e);
        fail++;
      }

      geda_line_object_set_line_length (object0, l);

      value = object0->line_options->line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: (O103101) %d != %d\n", value, l);
        fail++;
      }

      geda_line_object_set_line_space (object0, p);

      value = object0->line_options->line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: (O103201) %d != %d\n", value, p);
        fail++;
      }

      geda_line_object_set_line_type (object0, t);

      value = object0->line_options->line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: (O103301) %d != %d\n", value, t);
        fail++;
      }

      geda_line_object_set_line_width (object0, w);

      value = object0->line_options->line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: (O103401) %d != %d\n", value, w);
        fail++;
      }

      value = geda_line_object_get_end_cap(object0);

      if (value - e) {
        fprintf(stderr, "FAILED: (O100301) %d != %d\n", value, e);
        fail++;
      }

      value = geda_line_object_get_line_length(object0);

      if (value - l) {
        fprintf(stderr, "FAILED: (O100401) %d != %d\n", value, l);
        fail++;
      }

      value = geda_line_object_get_line_space(object0);

      if (value - p) {
        fprintf(stderr, "FAILED: (O100501) %d != %d\n", value, p);
        fail++;
      }

      value = geda_line_object_get_line_type(object0);

      if (value - t) {
        fprintf(stderr, "FAILED: (O100601) %d != %d\n", value, t);
        fail++;
      }

      value = geda_line_object_get_line_width(object0);

      if (value - w) {
        fprintf(stderr, "FAILED: (O100701) %d != %d\n", value, w);
        fail++;
      }

      g_object_unref (object0);

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t      x0: %d\n", x0);
        fprintf(stderr, "\t      y0: %d\n", y0);
        fprintf(stderr, "\t      x1: %d\n", x1);
        fprintf(stderr, "\t      y1: %d\n", y1);

        result = result + fail;
        break;
      }
    }
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