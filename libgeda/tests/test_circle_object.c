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
 *      O0602    geda_circle_object_get_center_x
 *      O0603    geda_circle_object_get_center_y
 *      O0604    geda_circle_object_get_end_cap
 *               geda_circle_object_get_fill_angle1
 *               geda_circle_object_get_fill_angle2
 *               geda_circle_object_get_fill_pitch1
 *               geda_circle_object_get_fill_pitch2
 *               geda_circle_object_get_fill_type
 *               geda_circle_object_get_fill_width
 *      O0611    geda_circle_object_get_line_length
 *      O0612    geda_circle_object_get_line_space
 *      O0613    geda_circle_object_get_line_type
 *      O0614    geda_circle_object_get_line_width
 *               geda_circle_object_get_nearest_point
 *      O0616    geda_circle_object_get_radius
 *               geda_circle_object_modify
 *               geda_circle_object_mirror
 *      O0619    geda_circle_object_new
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
 *      O0632    geda_circle_object_set_center_x
 *      O0633    geda_circle_object_set_center_y
 *      O0634    geda_circle_object_set_end_cap
 *               geda_circle_object_set_fill_angle1
 *               geda_circle_object_set_fill_angle2
 *               geda_circle_object_set_fill_pitch1
 *               geda_circle_object_set_fill_pitch2
 *               geda_circle_object_set_fill_type
 *               geda_circle_object_set_fill_width
 *      O0641    geda_circle_object_set_line_length
 *      O0642    geda_circle_object_set_line_space
 *      O0643    geda_circle_object_set_line_type
 *      O0644    geda_circle_object_set_line_width
 *      O0645    geda_circle_object_set_radius
 *               geda_circle_object_shortest_distance
 *               geda_circle_object_to_buffer
 *               geda_circle_object_translate
 *
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
check_accessors ()
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

/*
  if (geda_circle_object_get_fill_angle1(NULL)) {
    fprintf(stderr, "FAILED: (O060400) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_angle2(NULL)) {
    fprintf(stderr, "FAILED: (O060500) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_pitch1(NULL)) {
    fprintf(stderr, "FAILED: (O060600) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_pitch2(NULL)) {
    fprintf(stderr, "FAILED: (O060700) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_type(NULL)) {
    fprintf(stderr, "FAILED: (O060800) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_circle_object_get_fill_width(NULL)) {
    fprintf(stderr, "FAILED: (O060900) %s center y not zero\n", TOBJECT);
    result++;
  }

 */

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

      int c = m_random_number (0, MAX_COLORS - 1);
      int r = m_random_number (5, 20000);
      int x = m_random_number (0, 120000);
      int y = m_random_number (0, 80000);

      /* Line type options */
      int e = m_random_number (END_NONE, END_ROUND);
      int t = m_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = m_random_number (0, 500);
      int p = m_random_number (0, 500);
      int w = m_random_number (0, 500);

      o_set_color (object0, c);

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
        fprintf(stderr, "FAILED: (O063201) set_center_x %d != %d\n", value, x);
        fail++;
      }

      value = object0->circle->center_y;
      if (value - y) {
        fprintf(stderr, "FAILED: (O063301) set_center_y %d != %d\n", value, y);
        fail++;
      }

      value = object0->circle->radius;
      if (value - r) {
        fprintf(stderr, "FAILED: (O064501) set_radius %d != %d\n", value, r);
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
        fprintf(stderr, "FAILED: (O061601) get_radius %d != %d\n", value, r);
        fail++;
      }

      /* Check line type properties */

      geda_circle_object_set_end_cap (object0, e);

      value = object0->line_options->line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: (O063401) %d != %d\n", value, e);
        fail++;
      }

      geda_circle_object_set_line_length (object0, l);

      value = object0->line_options->line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: (O064101) %d != %d\n", value, l);
        fail++;
      }

      geda_circle_object_set_line_space (object0, p);

      value = object0->line_options->line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: (O064201) %d != %d\n", value, p);
        fail++;
      }

      geda_circle_object_set_line_type (object0, t);

      value = object0->line_options->line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: (O064301) %d != %d\n", value, t);
        fail++;
      }

      geda_circle_object_set_line_width (object0, w);

      value = object0->line_options->line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: (O064401) %d != %d\n", value, w);
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

      /* Filling options */

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
      fprintf(stderr, "Caught signal checking accessors in object/o_circle_object.c\n\n");
      return 1;
    }
  }
  else {
    fprintf(stderr, "discontinuing checks for object/o_circle_object.c\n\n");
  }

  return result;
}