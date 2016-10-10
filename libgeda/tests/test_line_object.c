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
 *  geda_line_object.c module.
 */

/*! \def MUT Module Under Tests */
#define MUT "src/object/o_line_object.c"

#define TOBJECT "GedaLine"

/** \defgroup test-object-geda-line Test GEDA Line object Module
 * @{
 * \brief Group 11 src/object/o_line_object.c geda_line_object_
 *  Group 11 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  11  88  88
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
 *      O1101    geda_line_object_copy
 *               geda_line_object_get_closest_endpoint
 *      O1103    geda_line_object_get_end_cap
 *      O1104    geda_line_object_get_line_length
 *      O1105    geda_line_object_get_line_space
 *      O1106    geda_line_object_get_line_type
 *      O1107    geda_line_object_get_line_width
 *               geda_line_object_get_intersection
 *               geda_line_object_get_midpoint
 *               geda_line_object_get_nearest_point
 *               geda_line_object_get_position
 *               geda_line_object_get_slope
 *      O1113    geda_line_object_get_x1
 *      O1114    geda_line_object_get_x2
 *      O1115    geda_line_object_get_y1
 *      O1116    geda_line_object_get_y2
 *               geda_line_object_is_endpoint
 *               geda_line_object_length
 *               geda_line_object_mirror
 *               geda_line_object_modify
 *      O1121    geda_line_object_new
 *               geda_line_object_print
 *               geda_line_object_print_center
 *               geda_line_object_print_dashed
 *               geda_line_object_print_dotted
 *               geda_line_object_print_phantom
 *               geda_line_object_print_solid
 *      O1128    geda_line_object_read
 *               geda_line_object_rotate
 *      O1130    geda_line_object_set_end_cap
 *      O1131    geda_line_object_set_line_length
 *      O1132    geda_line_object_set_line_space
 *      O1133    geda_line_object_set_line_type
 *      O1134    geda_line_object_set_line_width
 *      O1135    geda_line_object_set_x1
 *      O1136    geda_line_object_set_x2
 *      O1137    geda_line_object_set_y1
 *      O1138    geda_line_object_set_y2
 *               geda_line_object_scale
 *               geda_line_object_shortest_distance
 *      O1141    geda_line_object_to_buffer
 *               geda_line_object_translate
 */

int
check_construction (void)
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int c  = geda_random_number (0,  MAX_COLORS - 1);
    int x0 = geda_random_number (0,  115000);
    int y0 = geda_random_number (0,  75000);
    int x1 = geda_random_number (x0, 120000);
    int y1 = geda_random_number (y0, 80000);

    /* === Function 21: geda_line_object_new  === */

    GedaObject *object0 = geda_line_object_new(c, x0, y0, x1, y1);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O112101A) New GedaObject Failed: %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_LINE(object0->line)) {
      fprintf(stderr, "FAILED: (O112101B) sub-pointer not a %s\n", TOBJECT);
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
        fprintf(stderr, "FAILED: (O112101CX) line first x %d != %d\n", value, x0);
        fail++;
      }

      value = line->y[0];
      if (value - y0) {
        fprintf(stderr, "FAILED: (O112101CY) line first y %d != %d\n", value, y0);
        fail++;
      }

      value = line->x[1];
      if (value - x1) {
        fprintf(stderr, "FAILED: (O112101CX) line second x %d != %d\n", value, x1);
        fail++;
      }

      value = line->y[1];
      if (value - y1) {
        fprintf(stderr, "FAILED: (O112101CY) line second y %d != %d\n", value, y1);
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
check_accessors (void)
{
  int count;
  int result = 0;

  if (geda_line_object_get_x1(NULL)) {
    fprintf(stderr, "FAILED: (O111300) %s first x not zero\n", TOBJECT);
    result++;
  }

  if (geda_line_object_get_x2(NULL)) {
    fprintf(stderr, "FAILED: (O111400) %s second x not zero\n", TOBJECT);
    result++;
  }

  if (geda_line_object_get_y1(NULL)) {
    fprintf(stderr, "FAILED: (O111500) %s first y not zero\n", TOBJECT);
    result++;
  }

  if (geda_line_object_get_y2(NULL)) {
    fprintf(stderr, "FAILED: (O111600) %s second y not zero\n", TOBJECT);
    result++;
  }

  for (count = 0; count < 3; count++) {

    GedaObject *object0 = geda_line_object_new (3, 11, 21, 31, 41);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O112101C) New GedaObject Failed\n");
      result++;
      break;
    }
    else if (!GEDA_IS_LINE(object0->line)) {
      fprintf(stderr, "FAILED: (O112101D) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int fail;
      int value;

      fail = 0;

      int c  = geda_random_number (0,  MAX_COLORS - 1);
      int x0 = geda_random_number (0,  115000);
      int y0 = geda_random_number (0,  75000);
      int x1 = geda_random_number (x0, 120000);
      int y1 = geda_random_number (y0, 80000);

      /* Line type options */
      int e = geda_random_number (END_NONE, END_ROUND);
      int t = geda_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = geda_random_number (0, 500);
      int p = geda_random_number (0, 500);
      int w = geda_random_number (0, 500);

      geda_set_object_color (object0, c);

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
        fprintf(stderr, "FAILED: (O113501) set_x1 %d != %d\n", value, x0);
        fail++;
      }

      value = object0->line->x[1];
      if (value - x1) {
        fprintf(stderr, "FAILED: (O113601) set_x2 %d != %d\n", value, x1);
        fail++;
      }

      value = object0->line->y[0];
      if (value - y0) {
        fprintf(stderr, "FAILED: (O113701) set_y1 %d != %d\n", value, y0);
        fail++;
      }

      value = object0->line->y[1];
      if (value - y1) {
        fprintf(stderr, "FAILED: (O113801) set_y2 %d != %d\n", value, y1);
        fail++;
      }

      value = geda_line_object_get_x1(object0);
      if (value - x0) {
        fprintf(stderr, "FAILED: (O111301) get x1 %d != %d\n", value, x0);
        fail++;
      }

      value = geda_line_object_get_x2(object0);
      if (value - x1) {
        fprintf(stderr, "FAILED: (O111401) get x2 %d != %d\n", value, x1);
        fail++;
      }

      value = geda_line_object_get_y1(object0);
      if (value - y0) {
        fprintf(stderr, "FAILED: (O111501) get y1 %d != %d\n", value, y0);
        fail++;
      }

      value = geda_line_object_get_y2(object0);
      if (value - y1) {
        fprintf(stderr, "FAILED: (O111601) get y2 %d != %d\n", value, y1);
        fail++;
      }

      /* Check line type properties */

      geda_line_object_set_end_cap (object0, e);

      value = object0->line_options->line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: (O113001) %d != %d\n", value, e);
        fail++;
      }

      geda_line_object_set_line_length (object0, l);

      value = object0->line_options->line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: (O113101) %d != %d\n", value, l);
        fail++;
      }

      geda_line_object_set_line_space (object0, p);

      value = object0->line_options->line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: (O113201) %d != %d\n", value, p);
        fail++;
      }

      geda_line_object_set_line_type (object0, t);

      value = object0->line_options->line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: (O113301) %d != %d\n", value, t);
        fail++;
      }

      geda_line_object_set_line_width (object0, w);

      value = object0->line_options->line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: (O113401) %d != %d\n", value, w);
        fail++;
      }

      value = geda_line_object_get_end_cap(object0);

      if (value - e) {
        fprintf(stderr, "FAILED: (O110301) %d != %d\n", value, e);
        fail++;
      }

      value = geda_line_object_get_line_length(object0);

      if (value - l) {
        fprintf(stderr, "FAILED: (O110401) %d != %d\n", value, l);
        fail++;
      }

      value = geda_line_object_get_line_space(object0);

      if (value - p) {
        fprintf(stderr, "FAILED: (O110501) %d != %d\n", value, p);
        fail++;
      }

      value = geda_line_object_get_line_type(object0);

      if (value - t) {
        fprintf(stderr, "FAILED: (O110601) %d != %d\n", value, t);
        fail++;
      }

      value = geda_line_object_get_line_width(object0);

      if (value - w) {
        fprintf(stderr, "FAILED: (O110701) %d != %d\n", value, w);
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
check_serialization (void)
{
  int  count;
  int  converted;
  int  result;
  unsigned version;

  result    = 0;
  converted = sscanf (PACKAGE_DATE_VERSION, "%u", &version);

  if (!converted) {
    fprintf(stderr, "File %s, <%s>: could not scan version", __FILE__, __func__);
    version=19700101;
    result++;
  }

  for (count = 0; count < 10; count++) {

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       119800);
    int y2 = geda_random_number ( 0,        79800);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y1 = geda_random_number (y2 + 100,  80000);

    GedaObject *object0 = geda_line_object_new(c, x1, y1, x2, y2);

    char *buffer0 = geda_line_object_to_buffer (object0);

    g_object_unref (object0);

    if (!buffer0) {
      fprintf(stderr, "FAILED: (O114101A) line object to buffer\n");
      result++;
      break;
    }

    GedaObject *object1 = geda_line_object_read (buffer0,
                                                 version,
                                                 FILEFORMAT_VERSION,
                                                 NULL);

    if (!GEDA_IS_OBJECT(object1)) {
      fprintf(stderr, "FAILED: (O112801A) Read GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_LINE(object1->line)) {
      fprintf(stderr, "FAILED: (O112801B) sub-pointer not a GedaLine\n");
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaLine *line = object1->line;
      int       fail = 0;
      int       value;

      value = geda_object_get_color (object1);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-C) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      if (line->x[0] - x1) {
        fprintf(stderr, "FAILED: (O1141/O1128X1) line x1 %d != %d\n", line->x[0], x1);
        fail++;
      }

      if (line->y[0] - y1) {
        fprintf(stderr, "FAILED: (O1141/O1128Y1) line y1 %d != %d\n", line->y[0], y1);
        fail++;
      }

      if (line->x[1] - x2) {
        fprintf(stderr, "FAILED: (O1141/O1128X2) line x2 %d != %d\n", line->x[1], x2);
        fail++;
      }

      if (line->y[1] - y2) {
        fprintf(stderr, "FAILED: (O1141/O1128Y2) line y2 %d != %d\n", line->y[1], y2);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to read/write %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t  color: %d\n", c);
        fprintf(stderr, "    line->x[0]=%d, x1=%d\n", line->x[0], x1);
        fprintf(stderr, "    line->y[0]=%d, y1=%d\n", line->y[0], y1);
        fprintf(stderr, "    line->x[1]=%d, x2=%d\n", line->x[1], x2);
        fprintf(stderr, "    line->y[1]=%d, y2=%d\n", line->y[1], y2);

        result = result + fail;
        break;
      }

      char *buffer1 = geda_line_object_to_buffer (object1);
      g_object_unref (object1);

      if (strcmp (buffer0, buffer1)) {
        fprintf(stderr, "FAILED: (O114101B) %s buffer mismatch\n", TOBJECT);
        result++;
        break;
      }

      g_free (buffer0);
      g_free (buffer1);
    }
  }
  return result;
}

int
check_get_closest_endpoint(GedaObject *object)
{
  int result = 0;

  int x1 = geda_line_object_get_x1 (object);
  int x2 = geda_line_object_get_x2 (object);
  int y1 = geda_line_object_get_y1 (object);
  int y2 = geda_line_object_get_y2 (object) ;

  /* === Function 02: geda_line_object_get_closest_endpoint  === */

  if (geda_line_object_get_closest_endpoint(object, x1 - 10, y1 - 10)) {
    fprintf(stderr, "FAILED: (O110101) %s closest_endpoint != 0\n", TOBJECT);
    result++;
  }

  if (!geda_line_object_get_closest_endpoint(object, x2 + 10, y2 + 10)) {
    fprintf(stderr, "FAILED: (O110102) %s closest_endpoint != 0\n", TOBJECT);
    result++;
  }

  return result;
}

int
check_get_intersection(GedaObject *object)
{
  int result = 0;

  /* === Function 08: geda_line_object_get_intersection  === */

  return result;
}

int
check_get_midpoint(GedaObject *object)
{
  int result = 0;

  /* === Function 09: geda_line_object_get_midpoint  === */

  return result;
}

int
check_get_nearest_point(GedaObject *object)
{
  int result = 0;

  /* === Function 10: geda_line_object_get_nearest_point  === */

  return result;
}

int
check_get_position(GedaObject *object)
{
  int result = 0;

  /* === Function 11: geda_line_object_get_position  === */

  return result;
}

int
check_get_slope(GedaObject *object)
{
  int result = 0;

  /* === Function 12: geda_line_object_get_slope  === */

  return result;
}

int
check_is_endpoint(GedaObject *object)
{
  int result = 0;

  /* === Function 17: geda_line_object_is_endpoint  === */

  return result;
}

int
check_length(GedaObject *object)
{
  int result = 0;

  /* === Function 18: geda_line_object_length  === */

  return result;
}

int
check_shortest_distance(GedaObject *object)
{
  int result = 0;

  /* === Function 40: geda_line_object_shortest_distance  === */

  return result;
}

int
check_query(void)
{
  int count;
  int result = 0;

  for (count = 0; count < 3; count++) {

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       119800);
    int y2 = geda_random_number ( 0,        79800);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y1 = geda_random_number (y2 + 100,  80000);

    GedaObject *object = geda_line_object_new(c, x1, y1, x2, y2);

    result  = check_get_closest_endpoint(object);

    result += check_get_intersection(object);

    result += check_get_midpoint(object);

    result += check_get_nearest_point(object);

    result += check_get_position(object);

    result += check_get_slope(object);

    result += check_is_endpoint(object);

    result += check_length(object);

    result += check_shortest_distance(object);

    g_object_unref (object);
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
      fprintf(stderr, "Caught signal checking accessors in %s\n\n", MUT);
      return 1;
    }

    if (setjmp(point) == 0) {
      result += check_serialization();
    }
    else {
      fprintf(stderr, "Caught signal checking serialization in %s\n\n", MUT);
      return 1;
    }

    if (setjmp(point) == 0) {
      result += check_query();
    }
    else {
      fprintf(stderr, "Caught signal during query in %s\n\n", MUT);
      return 1;
    }

  }
  else {
    fprintf(stderr, "discontinuing checks for %s\n\n", MUT);
  }

  return result;
}