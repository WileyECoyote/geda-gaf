/* -*- test_line.c -*-
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
 *  Date Contributed: February, 2nd, 2016
 */

#include "../../config.h"

#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaLine"

/*! \file test_line.c
 *  \brief Tests for geda_line.c module
 */

int test_line (void)
{
  int result = 0;

  GedaObject *object = geda_line_new();

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

  if (GEDA_IS_CIRCLE(object)) {
    fprintf(stderr, "%s matched type GedaCircle\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_COMPLEX(object)) {
    fprintf(stderr, "%s matched type GedaComplex\n", TOBJECT);
    result++;
  }

  if (!GEDA_IS_LINE(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  GedaLine *line = object->line;

  if (!GEDA_IS_LINE(line)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_LINE) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_LINE);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_LINE(object)) {
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

  GedaObject *object = geda_line_new();

  if (!GEDA_IS_LINE(object->line)) {
    fprintf(stderr, "is a %s Failed at line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaLine *line = object->line;

    int count;
    int fail;
    int value;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x1 = geda_random_number (0,  115000);
      int y0 = geda_random_number (0,  75000);
      int x2 = geda_random_number (x1, 120000);
      int y1 = geda_random_number (y0, 80000);

      /* Line type options */
      int e = geda_random_number (END_NONE, END_ROUND);
      int t = geda_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = geda_random_number (5, 500);
      int p = geda_random_number (5, 500);
      int w = geda_random_number (0, 500);

      g_object_set(line, "first-x",    x1,
                         "first-y",    y0,
                         "second-x",   x2,
                         "second-y",   y1,
                          NULL);

      int rx1, ry0, rx2, ry1;

      g_object_get(line, "first-x",    &rx1,
                         "first-y",    &ry0,
                         "second-x",   &rx2,
                         "second-y",   &ry1,
                          NULL);

      value = line->x[0];
      if (value - x1) {
        fprintf(stderr, "FAILED: %s set first x property %d != %d\n", TOBJECT, value, x1);
        fail++;
      }
      else if (rx1 - x1) {
        fprintf(stderr, "FAILED: %s get first x property %d != %d\n", TOBJECT, rx1, x1);
        fail++;
      }

      value = line->y[0];
      if (value - y0) {
        fprintf(stderr, "FAILED: %s set first y property %d != %d\n", TOBJECT, value, y0);
        fail++;
      }
      else if (ry0 - y0) {
        fprintf(stderr, "FAILED: %s get first y property %d != %d\n", TOBJECT, rx1, y0);
        fail++;
      }

      value = line->x[1];
      if (value - x2) {
        fprintf(stderr, "FAILED: %s set second x property %d != %d\n", TOBJECT, value, x2);
        fail++;
      }
      else if (rx2 - x2) {
        fprintf(stderr, "FAILED: %s get second x property %d != %d\n", TOBJECT, rx1, x2);
        fail++;
      }

      value = line->y[1];
      if (value - y1) {
        fprintf(stderr, "FAILED: %s set second y property %d != %d\n", TOBJECT, value, y1);
        fail++;
      }
      else if (ry1 - y1) {
        fprintf(stderr, "FAILED: %s get second y property %d != %d\n", TOBJECT, rx2, y1);
        fail++;
      }

      /* Check line type properties */

      g_object_set(line, "end-cap",     e,
                         "line-type",   t,
                         "line-width",  w,
                         "line-space",  p,
                         "line-length", l,
                          NULL);

      int re, rt, rw, rp, rl;

      g_object_get(line, "end-cap",     &re,
                         "line-type",   &rt,
                         "line-width",  &rw,
                         "line-space",  &rp,
                         "line-length", &rl,
                          NULL);

      value = line->line_options.line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: %s set end-cap property %d != %d\n", TOBJECT, value, e);
        fail++;
      }
      else if (re - e) {
        fprintf(stderr, "FAILED: %s get end-cap property %d != %d\n", TOBJECT, re, e);
        fail++;
      }

      value = line->line_options.line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: %s set line-type property %d != %d\n", TOBJECT, value, t);
        fail++;
      }
      else if (rt - t) {
        fprintf(stderr, "FAILED: %s get line-type property %d != %d\n", TOBJECT, rt, t);
        fail++;
      }

      value = line->line_options.line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: %s set line-width property %d != %d\n", TOBJECT, value, w);
        fail++;
      }
      else if (rw - w) {
        fprintf(stderr, "FAILED: %s get line-width property %d != %d\n", TOBJECT, rw, w);
        fail++;
      }

      value = line->line_options.line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: %s set line-space property %d != %d\n", TOBJECT, value, p);
        fail++;
      }
      else if (rp - p) {
        fprintf(stderr, "FAILED: %s get line-space property %d != %d\n", TOBJECT, rp, p);
        fail++;
      }

      value = line->line_options.line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: %s set line-length property %d != %d\n", TOBJECT, value, l);
        fail++;
      }
      else if (rl - l) {
        fprintf(stderr, "FAILED: %s get line-length property %d != %d\n", TOBJECT, rl, l);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t      x1: %d\n", x1);
        fprintf(stderr, "\t      y0: %d\n", y0);
        fprintf(stderr, "\t      x2: %d\n", x2);
        fprintf(stderr, "\t      y1: %d\n", y1);

        result = fail;
        break;
      }
    }
  }

  return result;
}

int
check_accessors ()
{
  int count;
  int result = 0;

  count = geda_line_get_x1(NULL);
  count = geda_line_get_x2(NULL);
  count = geda_line_get_y1(NULL);
  count = geda_line_get_y2(NULL);

  geda_line_set_x1(NULL, 18);
  geda_line_set_x2(NULL, 19);
  geda_line_set_y1(NULL, 20);
  geda_line_set_y2(NULL, 21);

  GedaObject *object = geda_line_new();

  if (!GEDA_IS_LINE(object->line)) {
    fprintf(stderr, "is a %s Failed at line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaLine *line = object->line;

    int fail;
    int value;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x1 = geda_random_number (0,   115000);
      int y1 = geda_random_number (0,   75000);
      int x2 = geda_random_number (x1 + 100, 120000);
      int y2 = geda_random_number (y1 + 100, 80000);

      /* Line type options */
      int e = geda_random_number (END_NONE, END_ROUND);
      int t = geda_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = geda_random_number (5, 500);
      int p = geda_random_number (5, 500);
      int w = geda_random_number (0, 500);

      /* === Function: geda_line_set_x1  === */
      geda_line_set_x1(line, x1);

      value = line->x[0];
      if (value - x1) {
        fprintf(stderr, "FAILED: geda_line_set_x1 %d != %d\n", value, x1);
        fail++;
      }
      else {

        /* === Function: geda_line_get_x1  === */

        value = geda_line_get_x1(line);
        if (value - x1) {
          fprintf(stderr, "FAILED: geda_line_get_x1 %d != %d\n", value, x2);
          fail++;
        }
      }

      /* === Function: geda_line_set_x2  === */
      geda_line_set_x2(line, x2);

      value = line->x[1];
      if (value - x2) {
        fprintf(stderr, "FAILED: geda_line_set_x2 %d != %d\n", value, x2);
        fail++;
      }
      else {
        /* === Function: geda_line_get_x2  === */

        value = geda_line_get_x2(line);

        if (value - x2) {
          fprintf(stderr, "FAILED: geda_line_get_x2 %d != %d\n", value, x2);
          fail++;
        }
      }

      /* === Function: geda_line_set_y1  === */
      geda_line_set_y1(line, y1);

      value = line->y[0];
      if (value - y1) {
        fprintf(stderr, "FAILED: geda_line_set_y1 %d != %d\n", value, y1);
        fail++;
      }
      else {

        /* === Function: geda_line_get_y1  === */

        value = geda_line_get_y1(line);

        if (value - y1) {
          fprintf(stderr, "FAILED: geda_line_get_y1 %d != %d\n", value, y1);
          fail++;
        }
      }

      /* === Function: geda_line_set_y2  === */
      geda_line_set_y2(line, y2);

      value = line->y[1];
      if (value - y2) {
        fprintf(stderr, "FAILED: geda_line_set_y2 %d != %d\n", value, y2);
        fail++;
      }
      else {

        /* === Function: geda_line_get_y2  === */

        value = geda_line_get_y2(line);

        if (value - y2) {
          fprintf(stderr, "FAILED: geda_line_get_y2 %d != %d\n", value, y2);
          fail++;
        }
      }

      /* Check line type properties */

      geda_line_set_end_cap (line, e);

      value = line->line_options.line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: geda_line_set_end_cap %d != %d\n", value, e);
        fail++;
      }
      else {

        value = geda_line_get_end_cap(line);

        if (value - e) {
          fprintf(stderr, "FAILED: geda_line_get_end_cap %d != %d\n", value, e);
          fail++;
        }
      }

      geda_line_set_line_length (line, l);

      value = line->line_options.line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: geda_line_set_line_length %d != %d\n", value, l);
        fail++;
      }
      else {

        value = geda_line_get_line_length(line);

        if (value - l) {
          fprintf(stderr, "FAILED: geda_line_get_line_length %d != %d\n", value, l);
          fail++;
        }
      }

      geda_line_set_line_space (line, p);

      value = line->line_options.line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: geda_line_set_line_space %d != %d\n", value, p);
        fail++;
      }
      else {

        value = geda_line_get_line_space(line);

        if (value - p) {
          fprintf(stderr, "FAILED: geda_line_get_line_space %d != %d\n", value, p);
          fail++;
        }
      }

      geda_line_set_line_type (line, t);

      value = line->line_options.line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: geda_line_set_line_type %d != %d\n", value, t);
        fail++;
      }
      else {

        value = geda_line_get_line_type(line);

        if (value - t) {
          fprintf(stderr, "FAILED: geda_line_get_line_type %d != %d\n", value, t);
          fail++;
        }
      }

      geda_line_set_line_width (line, w);

      value = line->line_options.line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: geda_line_set_line_width %d != %d\n", value, w);
        fail++;
      }
      else {
        value = geda_line_get_line_width(line);

        if (value - w) {
          fprintf(stderr, "FAILED: geda_line_get_line_width %d != %d\n", value, w);
          fail++;
        }
      }

      if (fail) {
        result++;
        break;
      }
    }

    g_object_unref (object);

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

  result = test_line();

  result += check_properties();

  result += check_accessors();

  return result;
}
