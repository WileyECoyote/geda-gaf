/* -*- test_circle_object.c -*-
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
 *  Date Contributed: April, 26th, 2016
 */

#include "../../config.h"

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

/*! \def MUT Module Under Tests */
#define MUT "src/object/o_circle_object.c"

#define TOBJECT "GedaCircle"

/** \defgroup test-object-geda-circle Test GEDA Circle object Module
 * @{
 * \brief Group 6 src/object/o_circle_object.c geda_circle_object_
 *  Group 6 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  06  88  88
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
 *      O0602    geda_circle_object_get_center_x
 *      O0603    geda_circle_object_get_center_y
 *      O0604    geda_circle_object_get_end_cap
 *      O0605    geda_circle_object_get_fill_angle1
 *      O0606    geda_circle_object_get_fill_angle2
 *      O0607    geda_circle_object_get_fill_pitch1
 *      O0608    geda_circle_object_get_fill_pitch2
 *      O0609    geda_circle_object_get_fill_type
 *      O0610    geda_circle_object_get_fill_width
 *      O0611    geda_circle_object_get_line_length
 *      O0612    geda_circle_object_get_line_space
 *      O0613    geda_circle_object_get_line_type
 *      O0614    geda_circle_object_get_line_width
 *      O0615    geda_circle_object_get_nearest_point
 *      O0616    geda_circle_object_get_radius
 *      O0617    geda_circle_object_modify
 *      O0618    geda_circle_object_mirror
 *      O0619    geda_circle_object_new
 *      O0620    geda_circle_object_get_position
 *               geda_circle_object_print
 *               geda_circle_object_print_center
 *               geda_circle_object_print_dashed
 *               geda_circle_object_print_dotted
 *               geda_circle_object_print_filled
 *               geda_circle_object_print_hatch
 *               geda_circle_object_print_mesh
 *               geda_circle_object_print_phantom
 *               geda_circle_object_print_solid
 *      O0630    geda_circle_object_read
 *      O0631    geda_circle_object_rotate
 *      O0632    geda_circle_object_scale
 *      O0633    geda_circle_object_set_center_x
 *      O0634    geda_circle_object_set_center_y
 *      O0635    geda_circle_object_set_end_cap
 *      O0636    geda_circle_object_set_fill_angle1
 *      O0637    geda_circle_object_set_fill_angle2
 *      O0638    geda_circle_object_set_fill_pitch1
 *      O0639    geda_circle_object_set_fill_pitch2
 *      O0640    geda_circle_object_set_fill_type
 *      O0641    geda_circle_object_set_fill_width
 *      O0642    geda_circle_object_set_line_length
 *      O0643    geda_circle_object_set_line_space
 *      O0644    geda_circle_object_set_line_type
 *      O0645    geda_circle_object_set_line_width
 *      O0646    geda_circle_object_set_radius
 *      O0647    geda_circle_object_shortest_distance
 *      O0648    geda_circle_object_to_buffer
 *      O0649    geda_circle_object_translate
 */

int
check_construction (void)
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int c  = geda_random_number (0, MAX_COLORS - 1);
    int x1 = geda_random_number (0, 115000);
    int y1 = geda_random_number (0, 75000);
    int r  = geda_random_number (0, 5000);

    /* === Function 14: geda_circle_object_new  === */

    GedaObject *object0 = geda_circle_object_new(c, x1, y1, r);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O061901A) New GedaObject Failed: %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_CIRCLE(object0->circle)) {
      fprintf(stderr, "FAILED: (O061901B) sub-pointer not a %s\n", TOBJECT);
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
        fprintf(stderr, "FAILED: (O061901CX) circle center_x %d != %d\n", value, x1);
        fail++;
      }

      value = circle->center_y;
      if (value - y1) {
        fprintf(stderr, "FAILED: (O061901CY) circle center_y %d != %d\n", value, y1);
        fail++;
      }

      value = circle->radius;
      if (value - r) {
        fprintf(stderr, "FAILED: (O061901CR) circle radius %d != %d\n", value, r);
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
check_accessors (void)
{
  int count;
  int result = 0;

  if (geda_circle_object_get_center_x(NULL)) {
    fprintf(stderr, "FAILED: (O060200) %s center x not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_center_y(NULL)) {
    fprintf(stderr, "FAILED: (O060300) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_end_cap(NULL)) {
    fprintf(stderr, "FAILED: (O060400) %s end_cap not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_line_length(NULL)) {
    fprintf(stderr, "FAILED: (O061100) %s line_length not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_line_space(NULL)) {
    fprintf(stderr, "FAILED: (O061200) %s line_space not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_line_type(NULL)) {
    fprintf(stderr, "FAILED: (O061300) %s line_type not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_line_width(NULL)) {
    fprintf(stderr, "FAILED: (O061400) %s line_type not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_angle1(NULL)) {
    fprintf(stderr, "FAILED: (O060500) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_angle2(NULL)) {
    fprintf(stderr, "FAILED: (O060600) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_pitch1(NULL)) {
    fprintf(stderr, "FAILED: (O060700) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_pitch2(NULL)) {
    fprintf(stderr, "FAILED: (O060800) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_type(NULL)) {
    fprintf(stderr, "FAILED: (O060900) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_width(NULL)) {
    fprintf(stderr, "FAILED: (O061000) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_radius(NULL)) {
    fprintf(stderr, "FAILED: (O061600) %s radius not zero\n", TOBJECT);
    result++;
  }


  for (count = 0; count < 10; count++) {

    GedaObject *object0 = geda_circle_object_new (11, 21, 31, 41);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O061901?) New GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_CIRCLE(object0->circle)) {
      fprintf(stderr, "FAILED: (O061901?) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int fail;
      int value;

      fail = 0;

      int c = geda_random_number (0, MAX_COLORS - 1);
      int r = geda_random_number (5, 20000);
      int x = geda_random_number (0, 120000);
      int y = geda_random_number (0, 80000);

      /* Line type options */
      int e = geda_random_number (END_NONE, END_ROUND);
      int t = geda_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = geda_random_number (5, 500);
      int p = geda_random_number (5, 500);
      int w = geda_random_number (0, 500);

      /* Filling options */
      int ft  = geda_random_number (FILLING_HOLLOW, FILLING_HATCH);
      int fw  = geda_random_number (0, 100);
      int fa1 = geda_random_number (0, 180);
      int fp1 = geda_random_number (0, 500);
      int fa2 = geda_random_number (0, 180);
      int fp2 = geda_random_number (0, 500);

      geda_set_object_color (object0, c);

      geda_circle_object_set_center_x (object0, x);
      geda_circle_object_set_center_y (object0, y);
      geda_circle_object_set_radius (object0, r);

      value = geda_object_get_color (object0);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-C) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      value = object0->circle->center_x;
      if (value - x) {
        fprintf(stderr, "FAILED: (O063301) set_center_x %d != %d\n", value, x);
        fail++;
      }

      value = object0->circle->center_y;
      if (value - y) {
        fprintf(stderr, "FAILED: (O063401) set_center_y %d != %d\n", value, y);
        fail++;
      }

      value = object0->circle->radius;
      if (value - r) {
        fprintf(stderr, "FAILED: (O064601) set_radius %d != %d\n", value, r);
        fail++;
      }

      value = geda_circle_object_get_center_x(object0);
      if (value - x) {
        fprintf(stderr, "FAILED: (O060201) center x %d != %d\n", value, x);
        fail++;
      }

      value = geda_circle_object_get_center_y(object0);
      if (value - y) {
        fprintf(stderr, "FAILED: (O060301) center y %d != %d\n", value, y);
        fail++;
      }

      value = geda_circle_object_get_radius (object0);
      if (value - r) {
        fprintf(stderr, "FAILED: (O061701) get_radius %d != %d\n", value, r);
        fail++;
      }

      /* Check line type properties */

      geda_circle_object_set_end_cap (object0, e);

      value = object0->line_options->line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: (O063501) %d != %d\n", value, e);
        fail++;
      }

      geda_circle_object_set_line_length (object0, l);

      value = object0->line_options->line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: (O064201) %d != %d\n", value, l);
        fail++;
      }

      geda_circle_object_set_line_space (object0, p);

      value = object0->line_options->line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: (O064301) %d != %d\n", value, p);
        fail++;
      }

      geda_circle_object_set_line_type (object0, t);

      value = object0->line_options->line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: (O064401) %d != %d\n", value, t);
        fail++;
      }

      geda_circle_object_set_line_width (object0, w);

      value = object0->line_options->line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: (O064501) %d != %d\n", value, w);
        fail++;
      }

      value = geda_circle_object_get_end_cap(object0);

      if (value - e) {
        fprintf(stderr, "FAILED: (O060401) %d != %d\n", value, e);
        fail++;
      }

      value = geda_circle_object_get_line_length(object0);

      if (value - l) {
        fprintf(stderr, "FAILED: (O061101) %d != %d\n", value, l);
        fail++;
      }

      value = geda_circle_object_get_line_space(object0);

      if (value - p) {
        fprintf(stderr, "FAILED: (O061201) %d != %d\n", value, p);
        fail++;
      }

      value = geda_circle_object_get_line_type(object0);

      if (value - t) {
        fprintf(stderr, "FAILED: (O061301) %d != %d\n", value, t);
        fail++;
      }

      value = geda_circle_object_get_line_width(object0);

      if (value - w) {
        fprintf(stderr, "FAILED: (O061401) %d != %d\n", value, w);
        fail++;
      }

      /* Check Filling properties */

      geda_circle_object_set_fill_angle1 (object0, fa1);

      value = object0->fill_options->fill_angle1;
      if (value - fa1) {
        fprintf(stderr, "FAILED: (O063601) %d != %d\n", value, fa1);
        fail++;
      }

      geda_circle_object_set_fill_angle2 (object0, fa2);

      value = object0->fill_options->fill_angle2;
      if (value - fa2) {
        fprintf(stderr, "FAILED: (O063701) %d != %d\n", value, fa2);
        fail++;
      }

      geda_circle_object_set_fill_pitch1 (object0, fp1);

      value = object0->fill_options->fill_pitch1;
      if (value - fp1) {
        fprintf(stderr, "FAILED: (O063801) %d != %d\n", value, fp1);
        fail++;
      }

      geda_circle_object_set_fill_pitch2 (object0, fp2);

      value = object0->fill_options->fill_pitch2;
      if (value - fp2) {
        fprintf(stderr, "FAILED: (O063901) %d != %d\n", value, fp2);
        fail++;
      }

      geda_circle_object_set_fill_type (object0, ft);

      value = object0->fill_options->fill_type;
      if (value - ft) {
        fprintf(stderr, "FAILED: (O064001) %d != %d\n", value, ft);
        fail++;
      }

      geda_circle_object_set_fill_width (object0, fw);

      value = object0->fill_options->fill_width;
      if (value - fw) {
        fprintf(stderr, "FAILED: (O064101) %d != %d\n", value, fw);
        fail++;
      }

      value = geda_circle_object_get_fill_angle1(object0);

      if (value - fa1) {
        fprintf(stderr, "FAILED: (O060501) %d != %d\n", value, fa1);
        fail++;
      }

      value = geda_circle_object_get_fill_angle2(object0);

      if (value - fa2) {
        fprintf(stderr, "FAILED: (O060601) %d != %d\n", value, fa2);
        fail++;
      }
      value = geda_circle_object_get_fill_pitch1(object0);

      if (value - fp1) {
        fprintf(stderr, "FAILED: (O060701) %d != %d\n", value, fp1);
        fail++;
      }

      value = geda_circle_object_get_fill_pitch2(object0);

      if (value - fp2) {
        fprintf(stderr, "FAILED: (O060801) %d != %d\n", value, fp2);
        fail++;
      }

      value = geda_circle_object_get_fill_type(object0);

      if (value - ft) {
        fprintf(stderr, "FAILED: (O060901) %d != %d\n", value, ft);
        fail++;
      }

      value = geda_circle_object_get_fill_width(object0);

      if (value - fw) {
        fprintf(stderr, "FAILED: (O061001) %d != %d\n", value, fw);
        fail++;
      }

      g_object_unref (object0);

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t     radius: %d\n", r);
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);

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

    int c = geda_random_number (0, MAX_COLORS - 1);
    int r = geda_random_number (5, 20000);
    int x = geda_random_number (0, 120000);
    int y = geda_random_number (0, 80000);

    GedaObject *object0 = geda_circle_object_new (c, x, y, r);

    char *buffer0 = geda_circle_object_to_buffer (object0);

    g_object_unref (object0);

    if (!buffer0) {
      fprintf(stderr, "FAILED: (O064701A) circle to buffer\n");
      result++;
      break;
    }

    GedaObject *object1 = geda_circle_object_read (buffer0,
                                                   version,
                                                   FILEFORMAT_VERSION,
                                                   NULL);

    if (!GEDA_IS_OBJECT(object1)) {
      fprintf(stderr, "FAILED: (O063001A) Read GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_CIRCLE(object1->circle)) {
      fprintf(stderr, "FAILED: (O063001B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int       fail = 0;
      int       value;

      value = geda_object_get_color (object1);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-C) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      value = geda_circle_object_get_center_x(object1);
      if (value - x) {
        fprintf(stderr, "FAILED: (O0648/O0630X) center x %d != %d\n", value, x);
        fail++;
      }

      value = geda_circle_object_get_center_y(object1);
      if (value - y) {
        fprintf(stderr, "FAILED: (O0648/O0630Y) center y %d != %d\n", value, y);
        fail++;
      }

      value = geda_circle_object_get_radius (object1);
      if (value - r) {
        fprintf(stderr, "FAILED: (O0648/O0630R) get_radius %d != %d\n", value, r);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t     radius: %d\n", r);
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);

        result = result + fail;
        break;
      }

      char *buffer1 = geda_circle_object_to_buffer (object1);
      g_object_unref (object1);

      if (strcmp (buffer0, buffer1)) {
        fprintf(stderr, "FAILED: (O062201B) %s buffer mismatch\n", TOBJECT);
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

int
check_query(void)
{
  int  count;
  int result = 0;

  int dum = 1;
  if (geda_circle_object_get_nearest_point(NULL, dum, dum, &dum, &dum)) {
    fprintf(stderr, "FAILED: (O061500) circle_get_nearest_point NULL\n");
    result++;
  }

  if (geda_circle_object_get_position(NULL, &dum, &dum)) {
    fprintf(stderr, "FAILED: (O062000) circle_get_position NULL\n");
    result++;
  }

  if (geda_circle_object_shortest_distance(NULL, 0, 0, 0) != G_MAXDOUBLE ) {
    fprintf(stderr, "FAILED: (O064700) circle_shortest_distance NULL\n");
    result++;
  }

  GedaObject *object = geda_circle_object_new (3, 100, 200, 300);

  /* bounds_valid should NOT be set */
  if (object->bounds_valid) {
    fprintf(stderr, "FAILED: (O061902A) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Virtual geda_line_bounds  === */
  if (!geda_object_bounds(object)) {
    fprintf(stderr, "FAILED: (O061902B) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* bounds_valid should be set */
  if (!object->bounds_valid) {
    fprintf(stderr, "FAILED: (O061902C) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  g_object_unref (object);

  for (count = 0; count < 10; count++) {

    int c = geda_random_number (0, MAX_COLORS - 1);
    int r = geda_random_number (5, 20000);
    int x = geda_random_number (0, 120000);
    int y = geda_random_number (0, 80000);

    GedaObject *object0 = geda_circle_object_new (c, x, y, r);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O061901?) New GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_CIRCLE(object0->circle)) {
      fprintf(stderr, "FAILED: (O061901?) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int fail;
      int px, py;

      fail = 0;

      /* === Function 15: geda_circle_object_get_nearest_point  === */

      int qx, qy;
      int expect;

      qx = object0->circle->center_x - (r << 1);
      qy = object0->circle->center_y;

      geda_circle_object_get_nearest_point(object0, qx, qy, &px, &py);

      expect = object0->circle->center_x - r;

      if (px - expect) {
        fprintf(stderr, "FAILED: (O061501XL) get_nearest_point x %d != %d\n", px, expect);
        fail++;
      }

      /* === Function 20: geda_circle_object_get_position  === */

      geda_circle_object_get_position(object0, &px, &py);

      if (px - x) {
        fprintf(stderr, "FAILED: (O062001X) get_position x %d != %d\n", px, x);
        fail++;
      }

      if (py - y) {
        fprintf(stderr, "FAILED: (O062001Y) get_position x %d != %d\n", py, y);
        fail++;
      }

      /* === Function 46: geda_circle_object_shortest_distance  === */
      int dist;

      dist = (int)geda_circle_object_shortest_distance(object0, qx, qy, 1);


      if (dist != r) {
        fprintf(stderr, "FAILED: (O064701) shortest_distance %d != %d\n", dist, r);
        fail++;
      }

      g_object_unref (object0);

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t     radius: %d\n", r);
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);

        result = result + fail;
        break;
      }
    }
  }

  return result;
}

int
check_transformer(void)
{
  int result = 0;
  int count;

  int dum = 1;
  geda_circle_object_modify(NULL, dum, dum, dum);

  geda_circle_object_mirror(NULL, dum, dum);

  geda_circle_object_rotate(NULL, 0, 0, 0);

  geda_circle_object_translate(NULL, 0, 0);

  for (count = 0; count < 10; count++) {

    int c = geda_random_number (0, MAX_COLORS - 1);
    int r = geda_random_number (5, 20000);
    int x = geda_random_number (0, 120000);
    int y = geda_random_number (0, 80000);

    GedaObject *object0 = geda_circle_object_new (c, x, y, r);

    if (!GEDA_IS_CIRCLE(object0->circle)) {
      fprintf(stderr, "FAILED: (O061901?) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int fail;

      fail = 0;

      /* === Function 17: geda_circle_object_modify  === */

      int nx, ny;
      int value;

      nx = x + 10;
      ny = y + 20;

      geda_circle_object_modify(object0, nx, ny, CIRCLE_CENTER);

      value = object0->circle->center_x;
      if (value - nx) {
        fprintf(stderr, "FAILED: (O061701A) modify circle %d != %d\n", value, nx);
        fail++;
      }

      value = object0->circle->center_y;
      if (value - ny) {
        fprintf(stderr, "FAILED: (O061701B) modify circle %d != %d\n", value, ny);
        fail++;
      }

      nx = r + 30;

      geda_circle_object_modify(object0, nx, 40, CIRCLE_RADIUS);

      value = object0->circle->radius;
      if (value - nx) {
        fprintf(stderr, "FAILED: (O061702) modify circle %d != %d\n", value, nx);
        fail++;
      }

      /* Put the circle back at (x,y) */

      geda_circle_object_modify(object0, x, y, CIRCLE_CENTER);

      /* === Function 18: geda_circle_object_mirror  === */

      geda_circle_object_mirror(object0, x - r, y);

      value = object0->circle->center_x;
      if (value - (x - 2 * r)) {
        fprintf(stderr, "FAILED: (O061801A) modify circle %d != %d\n", value, x);
        fail++;
      }

      value = object0->circle->center_y;
      if (value - y) {
        fprintf(stderr, "FAILED: (O061801B) modify circle %d != %d\n", value, y);
        fail++;
      }

      /* Put the circle back at (x,y) */

      geda_circle_object_modify(object0, x, y, CIRCLE_CENTER);

      geda_circle_object_mirror(object0, x, y - r);

      value = object0->circle->center_x;
      if (value - x) {
        fprintf(stderr, "FAILED: (O061802A) modify circle %d != %d\n", value, x);
        fail++;
      }

      value = object0->circle->center_y;
      if (value - y) {
        fprintf(stderr, "FAILED: (O061802B) modify circle %d != %d\n", value, y);
        fail++;
      }

      /* Put the circle back at (x,y) is case there was an error */

      geda_circle_object_modify(object0, x, y, CIRCLE_CENTER);

      /* === Function 31: geda_circle_object_rotate  === */

      geda_circle_object_rotate(object0, x, y + r, 90);

      value = object0->circle->center_x;
      if (value - x - r) {
        fprintf(stderr, "FAILED: (O063101A) modify circle %d != %d\n", value, x);
        fail++;
      }

      value = object0->circle->center_y;
      if (value - y - r) {
        fprintf(stderr, "FAILED: (O063101B) modify circle %d != %d\n", value, y);
        fail++;
      }

      geda_circle_object_modify(object0, x, y, CIRCLE_CENTER);

      /* === Function 48: geda_circle_object_translate  === */

      geda_circle_object_translate(object0, r, r);

      value = object0->circle->center_x;
      if (value - x - r) {
        fprintf(stderr, "FAILED: (O064901A) modify circle %d != %d\n", value, x);
        fail++;
      }

      value = object0->circle->center_y;
      if (value - y - r) {
        fprintf(stderr, "FAILED: (O064901B) modify circle %d != %d\n", value, y);
        fail++;
      }

      g_object_unref (object0);

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t     radius: %d\n", r);
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);

        result = result + fail;
        break;
      }
    }
  }
  return result;
}

/** @} endgroup test-object-geda-circle */

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
