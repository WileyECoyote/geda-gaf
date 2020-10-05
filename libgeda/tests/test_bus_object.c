/* -*- test_bus_object.c -*-
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
 *  Date Contributed: April, 27th, 2016
 */
#include "../../config.h"

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>
#include <math.h>
#include "test-suite.h"

/*! \file test_bus_object.c
 *  \brief Tests for o_bus_object.c module
 *  \par
 *  This module provides basic unit tests for functions in the geda_bus_object_
 *  module.
 */

/*! \def MUT Module Under Tests */
#define MUT "src/object/o_bus_object.c"

#define TOBJECT "GedaBus"

/** \defgroup test-object-geda-bus Test GEDA Bus object Module
 * @{
 * \brief Group 5 src/object/o_bus_object.c geda_bus_object_
 *  Group 5 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  05  88  88
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
 *               geda_bus_object_consolidate
 *      O0502    geda_bus_object_copy
 *      O0503    geda_bus_object_get_direction
 *      O0504    geda_bus_object_get_ripper_direction
 *      O0505    geda_bus_object_get_position
 *      O0506    geda_bus_object_get_x1
 *      O0507    geda_bus_object_get_x2
 *      O0508    geda_bus_object_get_y1
 *      O0509    geda_bus_object_get_y2
 *      O0510    geda_bus_object_mirror
 *      O0511    geda_bus_object_modify
 *      O0512    geda_bus_object_new
 *      O0513    geda_bus_object_orientation
 *               geda_bus_object_print
 *      O0515    geda_bus_object_read
 *      O0516    geda_bus_object_rotate
 *      O0517    geda_bus_object_set_ripper_direction
 *      O0518    geda_bus_object_set_x1
 *      O0519    geda_bus_object_set_x2
 *      O0520    geda_bus_object_set_y1
 *      O0521    geda_bus_object_set_y2
 *      O0522    geda_bus_object_to_buffer
 *      O0523    geda_bus_object_translate
 */

int
check_construction (void)
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       111000);
    int y1 = geda_random_number ( 0,        70000);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y2 = geda_random_number (y1 + 100,  80000);
    int d  = geda_random_number (0, 1);

    /* === Function 12: geda_bus_object_new  === */

    GedaObject *object0 = geda_bus_object_new(c, x1, y1, x2, y2, d);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O051201A) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_BUS(object0->bus)) {
      fprintf(stderr, "FAILED: (O051201B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else if (!GEDA_IS_LINE(object0->line)) {
      fprintf(stderr, "FAILED: (O051201C) sub-pointer not a GedaLine\n");
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

      if (line->x[0] - x1) {
        fprintf(stderr, "FAILED: (O051201X1) bus x1 %d != %d\n", line->x[0], x1);
        fail++;
      }

      if (line->y[0] - y1) {
        fprintf(stderr, "FAILED: (O051201Y1) bus y1 %d != %d\n", line->y[0], y1);
        fail++;
      }

      if (line->x[1] - x2) {
        fprintf(stderr, "FAILED: (O051201X2) bus x2 %d != %d\n", line->x[1], x2);
        fail++;
      }

      if (line->y[1] - y2) {
        fprintf(stderr, "FAILED: (O051201Y2) bus y2 %d != %d\n", line->y[1], y2);
        fail++;
      }

      if (!fail) {

        /* === Function 02: geda_bus_object_copy  === */
        GedaObject *object1 = geda_bus_object_copy (object0);
        GedaLine   *line    = object1->line;

        value = geda_object_get_color (object0);
        if (value - c) {
          fprintf(stderr, "FAILED: _get_color (%s-B) %d != %d\n", TOBJECT, value, c);
          fail++;
        }

        if (line->x[0] - x1) {
          fprintf(stderr, "FAILED: (O051202X1) bus x1 %d != %d\n", line->x[0], x1);
          fail++;
        }

        if (line->y[0] - y1) {
          fprintf(stderr, "FAILED: (O051202Y1) bus y1 %d != %d\n", line->y[0], y1);
          fail++;
        }

        if (line->x[1] - x2) {
          fprintf(stderr, "FAILED: (O051202X2) bus x2 %d != %d\n", line->x[1], x2);
          fail++;
        }

        if (line->y[1] - y2) {
          fprintf(stderr, "FAILED: (O051202Y2) bus y2 %d != %d\n", line->y[1], y2);
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

  count = geda_bus_object_get_direction(NULL);
  count = geda_bus_object_get_x1(NULL);
  count = geda_bus_object_get_x2(NULL);
  count = geda_bus_object_get_y1(NULL);
  count = geda_bus_object_get_y2(NULL);
  count = geda_bus_object_get_ripper_direction(NULL);

  geda_bus_object_set_ripper_direction(NULL, 1);
  geda_bus_object_set_x1(NULL, 18);
  geda_bus_object_set_x2(NULL, 19);
  geda_bus_object_set_y1(NULL, 20);
  geda_bus_object_set_y2(NULL, 21);

  for (count = 0; count < 3; count++) {

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       119800);
    int y2 = geda_random_number ( 0,        79800);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y1 = geda_random_number (y2 + 100,  80000);
    int d  = geda_random_number (0, 1);

    GedaObject *object0 = geda_bus_object_new(c, x1, y1, x2, y2, d);

    int px, py;
    int value;
    int fail;

    fail = 0;

    /* === Function 03: geda_bus_object_get_direction  === */

    int direction = 0;

    if (x2 > x1 && y2 == y1 ) {
      direction = 1;
    }
    else if (x1 > x2 && y2 == y1) {
      direction = -1;
    }
    else if (x1 == x2 && y2 > y1) {
      direction = 1;
    }
    else if (x1 == x2 && y1 > y2) {
      direction = -1;
    }

    value = geda_bus_object_get_direction(object0);

    if (value != direction) {
      fprintf(stderr, "FAILED: (O050301) geda_bus_object_get_direction <%d>\n", value);
      fail++;
    }

    /* === Function 04: geda_bus_object_get_ripper_direction  === */

    value = geda_bus_object_get_ripper_direction(object0);

    if (value - d) {
      fprintf(stderr, "FAILED: (O050401) geda_bus_object_get_ripper_direction <%d>\n", value);
      fail++;
    }

    /* Toggle the direction */
    geda_bus_object_set_ripper_direction(object0, !value);

    if (object0->bus->ripper_direction == value) {
      fprintf(stderr, "FAILED: (O051701) geda_bus_object_set_ripper_direction\n");
      fail++;
    }

    /* === Function 05: geda_bus_object_get_position  === */

    if (!geda_bus_object_get_position (object0, &px, &py)) {
      fprintf(stderr, "FAILED: (O050501A) geda_bus_object_get_position\n");
      fail++;
    }

    if (px - x1) {
      fprintf(stderr, "FAILED: (O050501B) bus %d != %d\n", px, x1);
      fail++;
    }

    if (py - y1) {
      fprintf(stderr, "FAILED: (O050501C) bus %d != %d\n", py, y1);
      fail++;
    }

    /* === Function 06: geda_bus_object_get_x1  === */

    value = geda_bus_object_get_x1(object0);
    if (value - x1) {
      fprintf(stderr, "FAILED: (O050601) bus %d != %d\n", value, x1);
      fail++;
    }

    /* === Function 07: geda_bus_object_get_x2  === */

    value = geda_bus_object_get_x2(object0);

    if (value - x2) {
      fprintf(stderr, "FAILED: (O050701) bus %d != %d\n", value, x2);
      fail++;
    }

    /* === Function 08: geda_bus_object_get_y1  === */

    value = geda_bus_object_get_y1(object0);
    if (value - y1) {
      fprintf(stderr, "FAILED: (O050801) bus %d != %d\n", value, y1);
      fail++;
    }

    /* === Function 09: geda_bus_object_get_y2  === */

    value = geda_bus_object_get_y2(object0);

    if (value - y2) {
      fprintf(stderr, "FAILED: (O050901) bus %d != %d\n", value, y2);
      fail++;
    }

    /* Reverse the coordinates */

    /* === Function 18: geda_bus_object_set_x1  === */
    geda_bus_object_set_x1(object0, x2);

    if (object0->line->x[0] - x2) {
      fprintf(stderr, "FAILED: (O051801) geda_bus_object_set_x1\n");
      fail++;
    }

    /* === Function 19: geda_bus_object_set_x2  === */
    geda_bus_object_set_x2(object0, x1);

    if (object0->line->x[1] - x1) {
      fprintf(stderr, "FAILED: (O051901) geda_bus_object_set_x2\n");
      fail++;
    }

    /* === Function 20: geda_bus_object_set_y1  === */
    geda_bus_object_set_y1(object0, y2);

    if (object0->line->y[0] - y2) {
      fprintf(stderr, "FAILED: (O052001) geda_bus_object_set_y1\n");
      fail++;
    }

    /* === Function 21: geda_bus_object_set_y2  === */
    geda_bus_object_set_y2(object0, y1);

    if (object0->line->y[1] - y1) {
      fprintf(stderr, "FAILED: (O052101) geda_bus_object_set_y2\n");
      fail++;
    }

    if (fail) {
      result++;
      break;
    }
    g_object_unref (object0);
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
    int d  = geda_random_number (0, 1);

    GedaObject *object0 = geda_bus_object_new(c, x1, y1, x2, y2, d);

    char *buffer0 = geda_bus_object_to_buffer (object0);

    g_object_unref (object0);

    if (!buffer0) {
      fprintf(stderr, "FAILED: (O052201A) bus object to buffer\n");
      result++;
      break;
    }

    GedaObject *object1 = geda_bus_object_read (buffer0,
                                                version,
                                                FILEFORMAT_VERSION,
                                                NULL);

    if (!GEDA_IS_OBJECT(object1)) {
      fprintf(stderr, "FAILED: (O051501A) Read GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_BUS(object1->bus)) {
      fprintf(stderr, "FAILED: (O051501B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else if (!GEDA_IS_LINE(object1->line)) {
      fprintf(stderr, "FAILED: (O051501C) sub-pointer not a GedaLine\n");
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
        fprintf(stderr, "FAILED: (O0522/O0515X1) bus x1 %d != %d\n", line->x[0], x1);
        fail++;
      }

      if (line->y[0] - y1) {
        fprintf(stderr, "FAILED: (O0522/O0515Y1) bus y1 %d != %d\n", line->y[0], y1);
        fail++;
      }

      if (line->x[1] - x2) {
        fprintf(stderr, "FAILED: (O0522/O0515X2) bus x2 %d != %d\n", line->x[1], x2);
        fail++;
      }

      if (line->y[1] - y2) {
        fprintf(stderr, "FAILED: (O0522/O0515Y2) bus y2 %d != %d\n", line->y[1], y2);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to read/write %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t  color: %d\n", c);
        fprintf(stderr, "bus line->x[0]=%d, x1=%d\n", line->x[0], x1);
        fprintf(stderr, "bus line->y[0]=%d, y1=%d\n", line->y[0], y1);
        fprintf(stderr, "bus line->x[1]=%d, x2=%d\n", line->x[1], x2);
        fprintf(stderr, "bus line->y[1]=%d, y2=%d\n", line->y[1], y2);

        result = result + fail;
        break;
      }

      char *buffer1 = geda_bus_object_to_buffer (object1);
      g_object_unref (object1);

      if (strcmp (buffer0, buffer1)) {
        fprintf(stderr, "FAILED: (O052201B) %s buffer mismatch\n", TOBJECT);
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

int check_query(void)
{
  int result = 0;

  int c  = geda_random_number ( 0, MAX_COLORS - 1);
  int x1 = geda_random_number ( 0,       119800);
  int y2 = geda_random_number ( 0,        79800);
  int x2 = geda_random_number (x1 + 100, 120000);
  int y1 = geda_random_number (y2 + 100,  80000);
  int d  = geda_random_number (0, 1);

  GedaObject *object = geda_bus_object_new(c, x1, y1, x2, y2, d);

  /* bounds_valid should NOT be set */
  if (object->bounds_valid) {
    fprintf(stderr, "FAILED: (O051203A) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Virtual geda_line_bounds  === */
  if (!geda_object_bounds(object)) {
    fprintf(stderr, "FAILED: (O051203B) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* bounds_valid should be set */
  if (!object->bounds_valid) {
    fprintf(stderr, "FAILED: (O051203C) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Function 05: geda_bus_object_get_position  === */
  int px, py;

  if (geda_bus_object_get_position (NULL, &px, &py)) {
    fprintf(stderr, "FAILED: (O050500A) object NULL\n");
    result++;
  }

  if (geda_bus_object_get_position (object, NULL, NULL)) {
    fprintf(stderr, "FAILED: (O050500B) X Y NULL\n");
    result++;
  }

  /* === Function 13: geda_bus_object_orientation  === */

  int value = geda_bus_object_orientation(NULL);

  if (value != NEITHER) {
    fprintf(stderr, "FAILED: (O051300) geda_bus_object_orientation NULL\n");
    result++;
  }

  geda_bus_object_set_y2(object, y1); /* set y point 2 == point 1 */

  value = geda_bus_object_orientation(object);

  if (value != HORIZONTAL) {
    fprintf(stderr, "FAILED: (O051302) geda_bus_object_orientation\n");
    result++;
  }

  geda_bus_object_set_y2(object, y2); /* set y point 2 back */
  geda_bus_object_set_x2(object, x1); /* set x point 2 == point 1 */

  value = geda_bus_object_orientation(object);

  if (value != VERTICAL) {
    fprintf(stderr, "FAILED: (O051303) geda_bus_object_orientation\n");
    result++;
  }

  g_object_unref (object);

  return result;
}

int
check_transformer(void)
{
  int result = 0;
  int count;

  geda_bus_object_modify(NULL, 0, 0, 0);

  geda_bus_object_mirror(NULL, 0, 0);

  geda_bus_object_rotate(NULL, 0, 0, 0);

  geda_bus_object_translate(NULL, 0, 0);

  for (count = 0; count < 10; count++) {

    int fail = 0;

    int x1  = geda_random_number ( 0, 119800);
    int y1  = geda_random_number ( 0, 79800);
    int y2  = y1 + 1000;
    int x2  = x1 + 1000;
    int off = geda_random_number (10, 100);

    GedaObject *object = geda_bus_object_new(3, x1, y1, x2, y2, 1);
    GedaLine   *line   = object->line;

    int nx = x1 + off;
    int ny = y1 + off;

    /* O0511 geda_bus_object_modify */

    geda_bus_object_modify(object, nx, ny, 0);

    if (line->x[0] - nx || line->y[0] - ny) {
      fprintf(stderr, "FAILED: (O051101) bus nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    nx = x2 + off;
    ny = y2 + off;

    geda_bus_object_modify(object, nx, ny, 1);

    if (line->x[1] - nx || line->y[1] - ny) {
      fprintf(stderr, "FAILED: (O051102) bus nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    geda_bus_object_modify(object, x1, y1, 0);
    geda_bus_object_modify(object, x2, y1, 1);

    /* O0510 geda_bus_object_mirror */
    geda_bus_object_mirror(object, x2, y2);

    if (line->x[1] - x2 || line->y[0] - y1) {
      fprintf(stderr, "FAILED: (O051001) bus x[0]=%d, y[0]=%d\n", line->x[1], line->y[0]);
      fail++;
    }

    /* O0516 geda_bus_object_rotate */

    geda_bus_object_rotate(object, line->x[1], line->y[1], 180);

    if (line->x[0] - x1 || line->y[0] - y1) {
      fprintf(stderr, "FAILED: (O051601) bus x[0]=%d, y[0]=%d\n", line->x[0], line->y[0]);
      fail++;
    }

    /* O0523 geda_bus_object_translate */

    geda_bus_object_translate(object, -1000, off);

    if (line->x[0] - x1 + 1000 || line->y[0] - y1 - off) {
      fprintf(stderr, "FAILED: (O052301) bus x[0]=%d, y[0]=%d\n", line->x[0], line->y[0]);
      fail++;
    }

    if (fail) {
      fprintf(stderr, "Conditions:\n");
      fprintf(stderr, "Offset %d\n", off);
      fprintf(stderr, "bus line->x[0]=%d, x1=%d\n", line->x[0], x1);
      fprintf(stderr, "bus line->y[0]=%d, y1=%d\n", line->y[0], y1);
      fprintf(stderr, "bus line->x[1]=%d, x2=%d\n", line->x[1], x2);
      fprintf(stderr, "bus line->y[1]=%d, y2=%d\n", line->y[1], y2);
      result++;
      break;
    }
    g_object_unref (object);
  }

  return result;
}

/** @} endgroup test-object-geda-bus */

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
      result += check_transformer();
    }
    else {
      fprintf(stderr, "Caught signal in transformers for %s\n\n", MUT);
      return 1;
    }

  }
  else {
    fprintf(stderr, "discontinuing checks for %s\n\n", MUT);
  }

  return result;
}

