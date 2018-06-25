/* -*- test_circle.c -*-
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
 *  Date Contributed: February, 1st, 2016
 */

#include "../../config.h"

#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaCircle"

/*! \file test_circle.c
 *  \brief Tests for geda_circle.c module
 */

int check_circle (void)
{
  int result = 0;

  GedaObject *object = geda_circle_new();

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s: is a GedaObject Failed\n", TOBJECT);
    result++;
  }

  /* The one with the Not operator is the one being tested */

  if (GEDA_IS_ARC(object)) {
    fprintf(stderr, "%s matched type GedaArc\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_BOX(object)) {
    fprintf(stderr, "%s matched type GedaBox\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_BUS(object)) {
    fprintf(stderr, "%s matched type GedaBus\n", TOBJECT);
    result++;
  }

  if (!GEDA_IS_CIRCLE(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
    result++;
  }

  if (GEDA_IS_COMPLEX(object)) {
    fprintf(stderr, "%s matched type GedaComplex\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s matched type GedaLine\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_NET(object)) {
    fprintf(stderr, "%s: matched type GedaNet\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_PATH(object)) {
    fprintf(stderr, "%s matched type GedaPath\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_PICTURE(object)) {
    fprintf(stderr, "%s matched type GedaPicture\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_PIN(object)) {
    fprintf(stderr, "%s matched type GedaPin\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_TEXT(object)) {
    fprintf(stderr, "%s matched type GedaText\n", TOBJECT);
    result++;
  }

  GedaCircle *circle = object->circle;

  if (!GEDA_IS_CIRCLE(circle)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_CIRCLE) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_CIRCLE);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_CIRCLE(object)) {
    fprintf(stderr, "%s was not destroyed\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s parent was not destroyed\n", TOBJECT);
    result++;
  }

  return result;
}

int
check_properties (void)
{
  int result = 0;

  GedaObject *object = geda_circle_new();

  if (!GEDA_IS_CIRCLE(object->circle)) {
    fprintf(stderr, "is a %s Failed at line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaCircle *circle = object->circle;

    int count;
    int fail;
    int value;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x  = geda_random_number ( 0,   105000);
      int y  = geda_random_number ( 0,    65000);
      int r  = geda_random_number ( 100,  15000);

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

      g_object_set(circle, "center-x",    x,
                           "center-y",    y,
                           "radius",      r,
                            NULL);

      int rr, rx, ry;

      g_object_get(circle, "center-x",    &rx,
                           "center-y",    &ry,
                           "radius",      &rr,
                            NULL);

      value = circle->center_x;
      if (value - x) {
        fprintf(stderr, "FAILED: %s set center x property %d != %d\n", TOBJECT, value, x);
        fail++;
      }
      else if (rx - x) {
        fprintf(stderr, "FAILED: %s get center x property %d != %d\n", TOBJECT, rx, x);
        fail++;
      }

      value = circle->center_y;
      if (value - y) {
        fprintf(stderr, "FAILED: %s set center y property %d != %d\n", TOBJECT, value, y);
        fail++;
      }
      else if (ry - y) {
        fprintf(stderr, "FAILED: %s get center y property %d != %d\n", TOBJECT, rx, y);
        fail++;
      }

      value = circle->radius;
      if (value - r) {
        fprintf(stderr, "FAILED: %s set radius property %d != %d\n", TOBJECT, value, r);
        fail++;
      }
      else if (rr - r) {
        fprintf(stderr, "FAILED: %s get radius property %d != %d\n", TOBJECT, rx, r);
        fail++;
      }

      /* Check line type properties */

      g_object_set(circle, "end-cap",     e,
                           "line-type",   t,
                           "line-width",  w,
                           "line-space",  p,
                           "line-length", l,
                            NULL);

      int re, rt, rw, rp, rl;

      g_object_get(circle, "end-cap",     &re,
                           "line-type",   &rt,
                           "line-width",  &rw,
                           "line-space",  &rp,
                           "line-length", &rl,
                            NULL);

      value = circle->line_options.line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: %s set end-cap property %d != %d\n", TOBJECT, value, e);
        fail++;
      }
      else if (re - e) {
        fprintf(stderr, "FAILED: %s get end-cap property %d != %d\n", TOBJECT, re, e);
        fail++;
      }

      value = circle->line_options.line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: %s set line-type property %d != %d\n", TOBJECT, value, t);
        fail++;
      }
      else if (rt - t) {
        fprintf(stderr, "FAILED: %s get line-type property %d != %d\n", TOBJECT, rt, t);
        fail++;
      }

      value = circle->line_options.line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: %s set line-width property %d != %d\n", TOBJECT, value, w);
        fail++;
      }
      else if (rw - w) {
        fprintf(stderr, "FAILED: %s get line-width property %d != %d\n", TOBJECT, rw, w);
        fail++;
      }

      value = circle->line_options.line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: %s set line-space property %d != %d\n", TOBJECT, value, p);
        fail++;
      }
      else if (rp - p) {
        fprintf(stderr, "FAILED: %s get line-space property %d != %d\n", TOBJECT, rp, p);
        fail++;
      }

      value = circle->line_options.line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: %s set line-length property %d != %d\n", TOBJECT, value, l);
        fail++;
      }
      else if (rl - l) {
        fprintf(stderr, "FAILED: %s get line-length property %d != %d\n", TOBJECT, rl, l);
        fail++;
      }

      /* Check Filling properties */

      g_object_set(circle, "fill-type",   ft,
                           "fill-width",  fw,
                           "fill-angle1", fa1,
                           "fill-pitch1", fp1,
                           "fill-angle2", fa2,
                           "fill-pitch2", fp2,
                            NULL);

      int rft, rfw, rfa1, rfp1, rfa2, rfp2;

      g_object_get(circle, "fill-type",   &rft,
                           "fill-width",  &rfw,
                           "fill-angle1", &rfa1,
                           "fill-pitch1", &rfp1,
                           "fill-angle2", &rfa2,
                           "fill-pitch2", &rfp2,
                            NULL);

      value = circle->fill_options.fill_type;
      if (value - ft) {
        fprintf(stderr, "FAILED: %s set fill-type property %d != %d\n", TOBJECT, value, ft);
        fail++;
      }
      else if (rft - ft) {
        fprintf(stderr, "FAILED: %s get fill-type property %d != %d\n", TOBJECT, rft, ft);
        fail++;
      }

      value = circle->fill_options.fill_width;
      if (value - fw) {
        fprintf(stderr, "FAILED: %s set fill-width property %d != %d\n", TOBJECT, value, fw);
        fail++;
      }
      else if (rfw - fw) {
        fprintf(stderr, "FAILED: %s get fill-width property %d != %d\n", TOBJECT, rfw, fw);
        fail++;
      }

      value = circle->fill_options.fill_angle1;
      if (value - fa1) {
        fprintf(stderr, "FAILED: %s set fill-angle1 property %d != %d\n", TOBJECT, value, fa1);
        fail++;
      }
      else if (rfa1 - fa1) {
        fprintf(stderr, "FAILED: %s get fill-angle1 property %d != %d\n", TOBJECT, rfa1, fa1);
        fail++;
      }

      value = circle->fill_options.fill_angle2;
      if (value - fa2) {
        fprintf(stderr, "FAILED: %s set fill-angle2 property %d != %d\n", TOBJECT, value, fa2);
        fail++;
      }
      else if (rfa2 - fa2) {
        fprintf(stderr, "FAILED: %s get fill-angle2 property %d != %d\n", TOBJECT, rfa1, fa2);
        fail++;
      }

      value = circle->fill_options.fill_pitch1;
      if (value - fp1) {
        fprintf(stderr, "FAILED: %s set fill-pitch1 property %d != %d\n", TOBJECT, value, fp1);
        fail++;
      }
      else if (rfp1 - fp1) {
        fprintf(stderr, "FAILED: %s get fill-pitch1 property %d != %d\n", TOBJECT, rfp1, fp1);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);
        fprintf(stderr, "\t     radius: %d\n", r);

        result = fail;
        break;
      }
    }
  }

  return result;
}

int
check_accessors (void)
{
  int result = 0;

  GedaObject *object = geda_circle_new();

  if (!GEDA_IS_CIRCLE(object->circle)) {
    fprintf(stderr, "is a %s Failed at line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaCircle *circle = object->circle;

    int count;
    int fail;
    int value;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x  = geda_random_number ( 0,   105000);
      int y  = geda_random_number ( 0,    65000);
      int r  = geda_random_number ( 100,  15000);

      /* Line type options */
      int e = geda_random_number (END_NONE, END_ROUND);
      int t = geda_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = geda_random_number (0, 500);
      int p = geda_random_number (0, 500);
      int w = geda_random_number (0, 500);

      /* Filling options */
      int ft  = geda_random_number (FILLING_HOLLOW, FILLING_HATCH);
      int fw  = geda_random_number (0, 100);
      int fa1 = geda_random_number (0, 180);
      int fp1 = geda_random_number (0, 500);
      int fa2 = geda_random_number (0, 180);
      int fp2 = geda_random_number (0, 500);

      geda_circle_set_center_x(circle, x);

      value = circle->center_x;
      if (value - x) {
        fprintf(stderr, "FAILED: geda_circle_set_center_x %d != %d\n", value, x);
        fail++;
      }

      value = geda_circle_get_center_x(circle);
      if (value - x) {
        fprintf(stderr, "FAILED: geda_circle_get_center_x %d != %d\n", value, x);
        fail++;
      }

      geda_circle_set_center_y(circle, y);

      value = circle->center_y;
      if (value - y) {
        fprintf(stderr, "FAILED: geda_circle_set_center_y %d != %d\n", value, y);
        fail++;
      }

      value = geda_circle_get_center_y(circle);
      if (value - y) {
        fprintf(stderr, "FAILED: geda_circle_get_center_y %d != %d\n", value, y);
        fail++;
      }

      geda_circle_set_radius(circle, r);

      value = circle->radius;
      if (value - r) {
        fprintf(stderr, "FAILED: geda_circle_set_radius %d != %d\n", value, r);
        fail++;
      }

      value = geda_circle_get_radius(circle);
      if (value - r) {
        fprintf(stderr, "FAILED: geda_circle_get_radius %d != %d\n", value, r);
        fail++;
      }

      /* Check line type properties */

      geda_circle_set_end_cap (circle, e);

      value = circle->line_options.line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: geda_circle_set_end_cap %d != %d\n", value, e);
        fail++;
      }

      value = geda_circle_get_end_cap(circle);

      if (value - e) {
        fprintf(stderr, "FAILED: geda_circle_get_end_cap %d != %d\n", value, e);
        fail++;
      }

      geda_circle_set_line_length (circle, l);

      value = circle->line_options.line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: geda_circle_set_line_length %d != %d\n", value, l);
        fail++;
      }

      value = geda_circle_get_line_length(circle);

      if (value - l) {
        fprintf(stderr, "FAILED: geda_circle_get_line_length %d != %d\n", value, l);
        fail++;
      }

      geda_circle_set_line_space (circle, p);

      value = circle->line_options.line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: geda_circle_set_line_space %d != %d\n", value, p);
        fail++;
      }

      value = geda_circle_get_line_space(circle);

      if (value - p) {
        fprintf(stderr, "FAILED: geda_circle_get_line_space %d != %d\n", value, p);
        fail++;
      }

      geda_circle_set_line_type (circle, t);

      value = circle->line_options.line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: geda_circle_set_line_type %d != %d\n", value, t);
        fail++;
      }

      value = geda_circle_get_line_type(circle);

      if (value - t) {
        fprintf(stderr, "FAILED: geda_circle_get_line_type %d != %d\n", value, t);
        fail++;
      }

      geda_circle_set_line_width (circle, w);

      value = circle->line_options.line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: geda_circle_set_line_width %d != %d\n", value, w);
        fail++;
      }

      value = geda_circle_get_line_width(circle);

      if (value - w) {
        fprintf(stderr, "FAILED: geda_circle_get_line_width %d != %d\n", value, w);
        fail++;
      }

      /* Check Filling properties */

      geda_circle_set_fill_type (circle, ft);

      value = circle->fill_options.fill_type;
      if (value - ft) {
        fprintf(stderr, "FAILED: geda_circle_set_fill_type %d != %d\n", value, ft);
        fail++;
      }

      value = geda_circle_get_fill_type(circle);

      if (value - ft) {
        fprintf(stderr, "FAILED: geda_circle_get_fill_type %d != %d\n", value, ft);
        fail++;
      }

      geda_circle_set_fill_angle1 (circle, fa1);

      value = circle->fill_options.fill_angle1;
      if (value - fa1) {
        fprintf(stderr, "FAILED: geda_circle_set_fill_angle1 %d != %d\n", value, fa1);
        fail++;
      }

      value = geda_circle_get_fill_angle1(circle);

      if (value - fa1) {
        fprintf(stderr, "FAILED: geda_circle_get_fill_angle1 %d != %d\n", value, fa1);
        fail++;
      }

      geda_circle_set_fill_angle2 (circle, fa2);

      value = circle->fill_options.fill_angle2;
      if (value - fa2) {
        fprintf(stderr, "FAILED: geda_circle_set_fill_angle2 %d != %d\n", value, fa2);
        fail++;
      }

      value = geda_circle_get_fill_angle2(circle);

      if (value - fa2) {
        fprintf(stderr, "FAILED: geda_circle_get_fill_angle2 %d != %d\n", value, fa2);
        fail++;
      }

      geda_circle_set_fill_pitch1 (circle, fp1);

      value = circle->fill_options.fill_pitch1;
      if (value - fp1) {
        fprintf(stderr, "FAILED: geda_circle_set_fill_pitch1 %d != %d\n", value, fp1);
        fail++;
      }

      value = geda_circle_get_fill_pitch1(circle);

      if (value - fp1) {
        fprintf(stderr, "FAILED: geda_circle_get_fill_pitch1 %d != %d\n", value, fp1);
        fail++;
      }

      geda_circle_set_fill_pitch2 (circle, fp2);

      value = circle->fill_options.fill_pitch2;
      if (value - fp2) {
        fprintf(stderr, "FAILED: geda_circle_set_fill_pitch2 %d != %d\n", value, fp2);
        fail++;
      }

      value = geda_circle_get_fill_pitch2(circle);

      if (value - fp2) {
        fprintf(stderr, "FAILED: geda_circle_get_fill_pitch2 %d != %d\n", value, fp2);
        fail++;
      }

      geda_circle_set_fill_width (circle, fw);

      value = circle->fill_options.fill_width;
      if (value - fw) {
        fprintf(stderr, "FAILED: geda_circle_set_fill_width %d != %d\n", value, fw);
        fail++;
      }

      value = geda_circle_get_fill_width(circle);

      if (value - fw) {
        fprintf(stderr, "FAILED: geda_circle_get_fill_width %d != %d\n", value, fw);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tcenter-x: %d\n", x);
        fprintf(stderr, "\tcenter-y: %d\n", y);
        fprintf(stderr, "\t  radius: %d\n", r);

        result = fail;
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

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result = check_circle();

  result += check_properties();

  result += check_accessors();

  return result > 0;
}
