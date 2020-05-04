/* -*- test_line_object.c -*-
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
 *  Date Contributed: March, 17th, 2016
 */

#include "../../config.h"

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

#define USE_RANDOM_NUMBERS 1

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
 *      O1102    geda_line_object_get_closest_endpoint
 *      O1103    geda_line_object_get_end_cap
 *      O1104    geda_line_object_get_line_length
 *      O1105    geda_line_object_get_line_space
 *      O1106    geda_line_object_get_line_type
 *      O1107    geda_line_object_get_line_width
 *      O1108    geda_line_object_get_intersection
 *      O1109    geda_line_object_get_midpoint
 *      O1110    geda_line_object_get_nearest_point
 *      O1111    geda_line_object_get_position
 *      O1112    geda_line_object_get_slope
 *      O1113    geda_line_object_get_x1
 *      O1114    geda_line_object_get_x2
 *      O1115    geda_line_object_get_y1
 *      O1116    geda_line_object_get_y2
 *      O1117    geda_line_object_is_endpoint
 *      O1118    geda_line_object_length
 *      O1119    geda_line_object_mirror
 *      O1120    geda_line_object_modify
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
 *      O1140    geda_line_object_shortest_distance
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

int check_accessors (void)
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
      int l = geda_random_number (5, 500);
      int p = geda_random_number (5, 500);
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

int check_serialization (void)
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
        fprintf(stderr, "buffer0=%s\n", buffer0);
        fprintf(stderr, "buffer1=%s\n", buffer1);
        result++;
        break;
      }

      g_free (buffer0);
      g_free (buffer1);
    }
  }
  return result;
}

/* === Function 02: geda_line_object_get_closest_endpoint  === */
int check_get_closest_endpoint(GedaObject *object)
{
  int result = 0;

  int x1 = geda_line_object_get_x1 (object);
  int x2 = geda_line_object_get_x2 (object);
  int y1 = geda_line_object_get_y1 (object);
  int y2 = geda_line_object_get_y2 (object);

  if (geda_line_object_get_closest_endpoint(object, x1 - 10, y1 - 10)) {
    fprintf(stderr, "FAILED: (O110201) %s closest_endpoint != 0\n", TOBJECT);
    result++;
  }

  if (!geda_line_object_get_closest_endpoint(object, x2 + 10, y2 + 10)) {
    fprintf(stderr, "FAILED: (O110202) %s closest_endpoint != 0\n", TOBJECT);
    result++;
  }

  return result;
}

/* === Function 08: geda_line_object_get_intersection  === */
int check_get_intersection(GedaObject *object0)
{
  int result = 0;

  int x1 = geda_line_object_get_x1 (object0);
  int x2 = geda_line_object_get_x2 (object0);
  int y1 = geda_line_object_get_y1 (object0);
  int y2 = geda_line_object_get_y2 (object0);

  int x = geda_random_number (x1, x2);
  int y = geda_random_number (y1, y2);

  GedaPoint point;
  double    slope;

  /* Reference */
  if (!geda_line_object_get_slope(object0, &slope)) {
    slope = 90;
  }

  /* Does not intersect */
  GedaObject *object1 = geda_line_object_new(0, x1, y1 + 10, x2, y2 + 10);

  if (geda_line_object_get_intersection(object0, object1, &point)) {
    fprintf(stderr, "FAILED: (O110801) %s intersection FALSE\n", TOBJECT);
    result++;
  }

  g_object_unref (object1);

  /* intersects at first endpoint */
  GedaObject *object2 = geda_line_object_new(0, x1, y1, x, y);

  if (!geda_line_object_get_intersection(object0, object2, &point)) {
    fprintf(stderr, "FAILED: (O110802F) %s intersection FALSE\n", TOBJECT);
    result++;
  }
  else {

    if (point.x != x1) {
      fprintf(stderr, "FAILED: (O110802X) intersection %d != %d\n", point.x, x1);
      result++;
    }

    if (point.y != y1) {
      fprintf(stderr, "FAILED: (O110802Y) intersection %d != %d\n", point.y, y1);
      result++;
    }
  }

  g_object_unref (object2);

  double x31 = (x2 + x1) / 2.0;
  double y31 = (y2 + y1) / 2.0;

  double m = -1 / slope;
  double b = y31  - m * x31;

  int ix31 = x31;
  int iy31 = y31;
  int ix32 = x31 + 20;
  int iy32 = (m * ix32) + b;

  /* intersects at midpoint */
  GedaObject *object3 = geda_line_object_new(0, ix31, iy31, ix32, iy32);

  if (!geda_line_object_get_intersection(object0, object3, &point)) {
    fprintf(stderr, "FAILED: (O110803F) %s intersection FALSE\n", TOBJECT);
    result++;
  }
  else {

    if (point.x - ix31 > 1) {
      fprintf(stderr, "FAILED: (O110803X) intersection %d != %d\n", point.x, ix31);
       fprintf(stderr, "x31=%d, y31=%d, x32=%d, y32=%d\n", ix31, iy31, ix32, iy32);
      result++;
    }

    if (point.y - iy31 > 1) {
      fprintf(stderr, "FAILED: (O110803Y) intersection %d != %d\n", point.y, iy31);
      fprintf(stderr, "x31=%d, y31=%d, x32=%d, y32=%d\n", ix31, iy31, ix32, iy32);
      result++;
    }
  }

  g_object_unref (object2);

  return result;
}

/* === Function 09: geda_line_object_get_midpoint  === */
int
check_get_midpoint(GedaObject *object)
{
  int result = 0;

  int x1 = geda_line_object_get_x1 (object);
  int x2 = geda_line_object_get_x2 (object);
  int y1 = geda_line_object_get_y1 (object);
  int y2 = geda_line_object_get_y2 (object);

  GedaPoint point;

  if (geda_line_object_get_midpoint(NULL, &point)) {
    fprintf(stderr, "FAILED: (O110900) %s midpoint NULL\n", TOBJECT);
    result++;
  }

  if (!geda_line_object_get_midpoint(object, &point)) {
    fprintf(stderr, "FAILED: (O110901) %s midpoint\n", TOBJECT);
    result++;
  }
  else {

    int mx = (x1 + x2) / 2;
    int my = (y1 + y2) / 2;

    if (point.x != mx ) {
      fprintf(stderr, "FAILED: (O110901X) %s %d != %d\n", TOBJECT, point.x, mx);
      result++;
    }

    if (point.y != my ) {
      fprintf(stderr, "FAILED: (O110901Y) %s %d != %d\n", TOBJECT, point.y, my);
      result++;
    }
  }

  return result;
}

/* === Function 10: geda_line_object_get_nearest_point  === */
int
check_get_nearest_point(GedaObject *object)
{
  int result = 0;

  double m;

  /* When testing there always will be a slope */
  if (geda_line_object_get_slope(object, &m)) {

    int x1 = geda_line_object_get_x1 (object);
    int x2 = geda_line_object_get_x2 (object);
    int y1 = geda_line_object_get_y1 (object);
    int y2 = geda_line_object_get_y2 (object);

    double b = y1 - (m * x1);

    int ax, ay;

    if (x2 > x1) {
      ax = x2;
      ay = y2;
    }
    else {
      ax = x1;
      ay = y1;
    }

    int qx = ax + 100;
    int qy = (m * qx) + b;

    int nx, ny;

    if (geda_line_object_get_nearest_point (NULL, qx, qy, NULL, NULL)) {
      fprintf(stderr, "FAILED: (O111000A) %s nearest NULL\n", TOBJECT);
      result++;
    }

    if (!geda_line_object_get_nearest_point (object, qx, qy, NULL, NULL)) {
      fprintf(stderr, "FAILED: (O111000B) %s nearest NULL\n", TOBJECT);
      result++;
    }

    if (!geda_line_object_get_nearest_point (object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O111001) %s nearest\n", TOBJECT);
      result++;
    }
    else {

      if (nx != ax ) {
        fprintf(stderr, "FAILED: (O111001X) %s %d != %d,", TOBJECT, nx, ax);
        fprintf(stderr, " query point (%d,%d)\n", qx, qy);
        result++;
      }

      if (ny != ay ) {
        fprintf(stderr, "FAILED: (O111001Y) %s %d != %d,", TOBJECT, ny, ay);
        fprintf(stderr, " query point (%d,%d)\n", qx, qy);
        result++;
      }
    }
  }
  return result;
}

int
check_get_position(GedaObject *object, int x1, int y1)
{
  int result = 0;

  int ax, ay;

  /* === Function 11: geda_line_object_get_position  === */
  if (!geda_line_object_get_position(object, &ax, &ay)) {
    fprintf(stderr, "FAILED: (O111101A) %s NOT TRUE,", TOBJECT);
    result++;
  }
  else if (ax != object->line->x[0] || ay != object->line->y[0]) {
    fprintf(stderr, "FAILED: (O111101B) %s (%d,%d),", TOBJECT, ax, ay);
    fprintf(stderr, " query point (%d,%d)\n", x1, y1);
    result++;
  }

  return result;
}

int
check_get_slope(GedaObject *object, int x1, int y1, int x2, int y2)
{
  int result = 0;
  int answer;
  double slope;

  /* === Function 12: geda_line_object_get_slope  === */

  answer = geda_line_object_get_slope(NULL, &slope);

  if (answer) {
    fprintf(stderr, "FAILED: (O111200) get_slope NULL\n");
    result++;
  }

  answer = geda_line_object_get_slope(object, &slope);

  if (!answer) {
    fprintf(stderr, "FAILED: (O111201A) get_slope FALSE\n");
    result++;
  }
  else {

    double dx = x2 - x1;

    if (dx) {

      double dy = y2 - y1;
      double cs = dy / dx;

      if (abs(cs - slope) > 0.000001) {
        fprintf(stderr, "FAILED: (O111201B) get_slope slope %f,", slope);
        fprintf(stderr, " computed <%f>\n", cs);
        result++;
      }
    }
    else if (slope) {
      fprintf(stderr, "FAILED: (O111201C) get_slope slope %f,", slope);
      fprintf(stderr, " line (%d,%d) to (%d,%d)\n", x1, y1, x2, y2);
      result++;
    }
  }

  return result;
}

int
check_is_endpoint(GedaObject *object)
{
  int result = 0;

  GedaPoint pt1, pt2, pt3;

  pt1.x = geda_line_object_get_x1 (object);
  pt1.y = geda_line_object_get_y1 (object);

  pt2.x = geda_line_object_get_x2 (object);
  pt2.y = geda_line_object_get_y2 (object);

  pt3.x = pt1.x;
  pt3.y = pt2.y;

  /* === Function 17: geda_line_object_is_endpoint  === */

  if (geda_line_object_is_endpoint(NULL, &pt1)) {
    fprintf(stderr, "FAILED: (O111700) is_endpoint NULL\n");
    result++;
  }

  if (!geda_line_object_is_endpoint(object, &pt1)) {
    fprintf(stderr, "FAILED: (O111701) is_endpoint\n");
    result++;
  }

  if (!geda_line_object_is_endpoint(object, &pt2)) {
    fprintf(stderr, "FAILED: (O111702) is_endpoint\n");
    result++;
  }

  if (geda_line_object_is_endpoint(object, &pt3)) {
    fprintf(stderr, "FAILED: (O111703) is_endpoint\n");
    result++;
  }

  return result;
}

int
check_length(GedaObject *object)
{
  int result = 0;

  GedaPoint pt1, pt2;

  double dx, dy;
  double length_a;

  pt1.x = geda_line_object_get_x1 (object);
  pt1.y = geda_line_object_get_y1 (object);

  pt2.x = geda_line_object_get_x2 (object);
  pt2.y = geda_line_object_get_y2 (object);

  dx = abs(pt2.x - pt1.x);
  dy = abs(pt2.y - pt1.y);

  length_a = sqrt((dx*dx) + (dy*dy));

  /* === Function 18: geda_line_object_length === */

  double length_b;

  length_b = geda_line_object_length(NULL);

  if (length_b != 0.0) {
    fprintf(stderr, "FAILED: (O111800) line length NULL\n");
    result++;
  }

  length_b = geda_line_object_length(object);

  if (length_b != length_a) {
    fprintf(stderr, "FAILED: (O111801) line length <%f>\n", length_b);
    result++;
  }

  return result;
}

int
check_shortest_distance(GedaObject *object)
{
  int result = 0;

  int x1, y1, x2, y2, xt, yt;

  double dist;

  x1 = geda_line_object_get_x1 (object);
  y1 = geda_line_object_get_y1 (object);

  x2 = geda_line_object_get_x2 (object);
  y2 = geda_line_object_get_y2 (object);

  /* === Function 40: geda_line_object_shortest_distance  === */

  /* Check with end points */

  dist = geda_line_object_shortest_distance(object, x1, y1, 0);

  if (dist != 0.0) {
    fprintf(stderr, "FAILED: (O114001) shortest distance (x1,y1)\n");
    result++;
  }

  dist = geda_line_object_shortest_distance(object, x2, y2, 0);

  if (dist != 0.0) {
    fprintf(stderr, "FAILED: (O114002) shortest distance (x2,y2)\n");
    result++;
  }

  /* Check from left side */
  if (x1 < x2) {
    xt = x1 - 300;
    yt = y1 - 400;
  }
  else {
    xt = x2 - 300;
    yt = y2 - 400;
  }

  dist = geda_line_object_shortest_distance(object, xt, yt, 0);

  if (dist != 500.0) {
    fprintf(stderr, "FAILED: (O114003) shortest distance (%d,%d) <%f>\n", xt, yt, dist);
    result++;
  }

  /* Check from right side */
  if (x1 > x2) {
    xt = x1 + 500;
    yt = y1 + 1200;
  }
  else {
    xt = x2 + 500;
    yt = y2 + 1200;
  }

  dist = geda_line_object_shortest_distance(object, xt, yt, 0);

  if (dist != 1300.0) {
    fprintf(stderr, "FAILED: (O114004) shortest distance (%d,%d) <%f>\n", xt, yt, dist);
    result++;
  }

  return result;
}

int
check_query(void)
{
  int count;
  int result = 0;

  GedaObject *object0 = geda_line_object_new(3, 500, 500, 500, 1000);

  /* bounds_valid should NOT be set */
  if (object0->bounds_valid) {
    fprintf(stderr, "FAILED: (O112102A) %s bounds_valid %d\n", TOBJECT, object0->bounds_valid);
    result++;
  }

  /* === Virtual geda_line_bounds  === */
  if (!geda_object_bounds(object0)) {
    fprintf(stderr, "FAILED: (O112102B) %s bounds_valid %d\n", TOBJECT, object0->bounds_valid);
    result++;
  }

  /* bounds_valid should be set */
  if (!object0->bounds_valid) {
    fprintf(stderr, "FAILED: (O112102C) %s bounds_valid %d\n", TOBJECT, object0->bounds_valid);
    result++;
  }

  g_object_unref (object0);

  for (count = 0; count < 3; count++) {

#if USE_RANDOM_NUMBERS

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       119800);
    int y1 = geda_random_number ( 0,        79800);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y2 = geda_random_number (y1 + 100,  80000);

#else

    int c  = 0;
    int x1 = 118097;
    int y1 = 61417;
    int x2 = 119978;
    int y2 = 71137;

#endif

    int fail = 0;

    GedaObject *object = geda_line_object_new(c, x1, y1, x2, y2);

    fail += check_get_closest_endpoint(object);

    fail += check_get_intersection(object);

    fail += check_get_midpoint(object);

    fail += check_get_nearest_point(object);

    fail += check_get_position(object, x1, y1);

    fail += check_get_slope(object, x1, y1, x2, y2);

    fail += check_is_endpoint(object);

    fail += check_length(object);

    fail += check_shortest_distance(object);

    if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to determine %d %s quer%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t      c: %d\n", c);
        fprintf(stderr, "\t      x1: %d\n", x1);
        fprintf(stderr, "\t      y1: %d\n", y1);
        fprintf(stderr, "\t      x2: %d\n", x2);
        fprintf(stderr, "\t      y2: %d\n", y2);

        result = result + fail;
    }

    g_object_unref (object);

#if !USE_RANDOM_NUMBERS
    break;
#endif

  }
  return result;
}

int
check_line_object_mirror(GedaObject *object, int x1, int y1, int x2, int y2)
{
  int mx1, my1, mx2, my2;
  int result = 0;

  /* === Function 19: geda_line_object_mirror  === */

  geda_line_object_mirror(object, x1 + 500, y2 - y1);

  mx1 = geda_line_object_get_x1 (object);
  my1 = geda_line_object_get_y1 (object);

  mx2 = geda_line_object_get_x2 (object);
  my2 = geda_line_object_get_y2 (object);

  if (mx1 != x1 + 2 * 500) {
    fprintf(stderr, "FAILED: (O111902X1) %s line_object_mirror %d\n", TOBJECT, mx1);
    result++;
  }

  if (my1 != y1) {
    fprintf(stderr, "FAILED: (O111902Y1) %s line_object_mirror %d\n", TOBJECT, my1);
    result++;
  }

  if (mx2 != (x1 + 500 - x2) + x1 + 500) {
    fprintf(stderr, "FAILED: (O111902X2) %s line_object_mirror %d\n", TOBJECT, mx2);
    result++;
  }

  if (my2 != y2) {
    fprintf(stderr, "FAILED: (O111902Y2) %s line_object_mirror %d\n", TOBJECT, my2);
    result++;
  }

  /* Mirroring the line again about the same point should restore the
   * line to the original coordinates */

  geda_line_object_mirror(object, x1 + 500, y2 - y1);

  return result;
}

int
check_line_object_modify(GedaObject *object, int x1, int y1, int x2, int y2)
{
  int mx1, my1, mx2, my2;
  int result = 0;

  /* === Function 20: geda_line_object_modify  === */

  geda_line_object_modify(object, x1 + 100, y1 + 100, 0);

  mx1 = geda_line_object_get_x1 (object);
  my1 = geda_line_object_get_y1 (object);

  mx2 = geda_line_object_get_x2 (object);
  my2 = geda_line_object_get_y2 (object);

  if (mx1 != x1 + 100) {
    fprintf(stderr, "FAILED: (O112001X1) %s line_object_modify %d\n", TOBJECT, mx1);
    result++;
  }

  if (my1 != y1 + 100) {
    fprintf(stderr, "FAILED: (O112001Y1) %s line_object_modify %d\n", TOBJECT, my1);
    result++;
  }

  /* object->line->x,y[1] should not be modified */

  if (mx2 != x2) {
    fprintf(stderr, "FAILED: (O112001X2) %s line_object_modify %d != %d\n", TOBJECT, mx2, x2);
    result++;
  }

  if (my2 != y2) {
    fprintf(stderr, "FAILED: (O112001Y2) %s line_object_modify %d != %d\n", TOBJECT, my2, y2);
    result++;
  }

  geda_line_object_modify(object, mx1 - 100, my1 - 100, 0);

  return result;
}

int
check_line_object_rotate(GedaObject *object, int x1, int y1, int x2, int y2)
{
  int rx1, ry1, rx2, ry2;
  int result = 0;
  int center_x;
  int center_y;

  /* Calculate the mid point of the line */
  center_x = (x1 + x2) / 2;
  center_y = (y1 + y2) / 2;

  /* === Function 29: geda_line_object_rotate  === */

  geda_line_object_rotate(object, center_x, center_y, 0);

  rx1 = geda_line_object_get_x1 (object);
  ry1 = geda_line_object_get_y1 (object);

  rx2 = geda_line_object_get_x2 (object);
  ry2 = geda_line_object_get_y2 (object);

  if (rx1 != x1) {
    fprintf(stderr, "FAILED: (O112201X1) %s line_object_rotate %d != %d\n", TOBJECT, rx1, x1);
    result++;
  }

  if (ry1 != y1) {
    fprintf(stderr, "FAILED: (O112901Y1) %s line_object_modify %d != %d\n", TOBJECT, ry1, y1);
    result++;
  }

  /* object->line->x,y[1] should not be modified */

  if (rx2 != x2) {
    fprintf(stderr, "FAILED: (O112901X2) %s line_object_modify %d != %d\n", TOBJECT, rx2, x2);
    result++;
  }

  if (ry2 != y2) {
    fprintf(stderr, "FAILED: (O112901Y2) %s line_object_modify %d != %d\n", TOBJECT, ry2, y2);
    result++;
  }

  return result;
}

int
check_line_object_scale(GedaObject *object, int x1, int y1, int x2, int y2)
{
  int sx1, sy1, sx2, sy2;
  int result = 0;

  /* === Function 39: geda_line_object_scale  === */

  geda_line_object_scale(object, 10, 10);

  sx1 = geda_line_object_get_x1(object);
  sy1 = geda_line_object_get_y1(object);
  sx2 = geda_line_object_get_x2(object);
  sy2 = geda_line_object_get_y2(object);

  if (sx1 - x1 * 10) {
    fprintf(stderr, "FAILED: (O112391X1) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  if (sx2 - x2 * 10) {
    fprintf(stderr, "FAILED: (O112391X2) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  if (sy1 - y1 * 10) {
    fprintf(stderr, "FAILED: (O112391Y1) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  if (sy2 - y2 * 10) {
    fprintf(stderr, "FAILED: (O112391Y2) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  object->line->x[0] = x1;
  object->line->y[0] = y1;
  object->line->x[1] = x2;
  object->line->y[1] = y2;

  return result;
}

int check_methods (void)
{
  int index;
  int result = 0;

  for (index = 0; index < 10; index++) {

    int x1, y1, x2, y2;

    x1 = geda_random_number (0, 119500);
    y1 = geda_random_number (0, 79000);

    x2 = x1;
    y2 = y1 + 500;

    GedaObject *object = geda_line_object_new(3, x1, y1, x2, y2);

    int fail = 0;

    /* === Function 19: geda_line_object_mirror  === */

    fail += check_line_object_mirror(object, x1, y1, x2, y2);

    /* === Function 20: geda_line_object_modify  === */

    fail += check_line_object_modify(object, x1, y1, x2, y2);

    /* === Function 29: geda_line_object_rotate  === */

    fail += check_line_object_rotate(object, x1, y1, x2, y2);

    /* === Function 39: geda_line_object_scale  === */

    fail += check_line_object_scale(object, x1, y1, x2, y2);

    /* === Function 42: geda_line_object_translate  === */

    if (fail) {

      fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, index);
      fprintf(stderr, "Conditions: ");
      fprintf(stderr, "(x1, y1)=(%d,%d) ", x1, y1);
      fprintf(stderr, "(x2, y2)=(%d,%d)\n", x2, y2);

      result = result + fail;
      break;
    }

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

    if (setjmp(point) == 0) {
      result += check_methods();
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

/** @} endgroup test-object-geda-line */
