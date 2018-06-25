/* -*- test_net.c -*-
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
 *  Date Contributed: February, 3rd, 2016
 */

#include "../../config.h"

#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaNet"

/*! \file test_net.c
 *  \brief Tests for geda_net.c module
 */

int test_net (void)
{
  int result = 0;

  GedaObject *object = geda_net_new();

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
    fprintf(stderr, "%s: matched type GedaBus\n", TOBJECT);
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

  /* GedaNet objects are derived from GedaLine object class */
  if (!GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s is a GedaLine Failed\n", TOBJECT);
    result++;
  }

  if (!GEDA_IS_NET(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  GedaNet  *net  = object->net;
  GedaLine *line = object->line;

  if (!GEDA_IS_NET(net)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (!GEDA_IS_LINE(line)) {
    fprintf(stderr, "%s sub-pointer is a GedaLine\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_NET) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_NET);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_NET(object)) {
    fprintf(stderr, "%s was not destroyed\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s parent was not destroyed\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s grand-parent was not destroyed\n", TOBJECT);
    result++;
  }

  return result;
}

int
check_accessors ()
{
  int count;
  int result = 0;

  count = geda_net_get_x0(NULL);
  count = geda_net_get_x1(NULL);
  count = geda_net_get_y0(NULL);
  count = geda_net_get_y1(NULL);

  geda_net_set_x0(NULL, 18);
  geda_net_set_x1(NULL, 19);
  geda_net_set_y0(NULL, 20);
  geda_net_set_y1(NULL, 21);

  for (count = 0; count < 3; count++) {

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       119800);
    int y2 = geda_random_number ( 0,        79800);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y1 = geda_random_number (y2 + 100,  80000);

    GedaObject *object0 = geda_net_object_new(c, x1, y1, x2, y2);
    GedaNet    *net     = object0->net;

    int value;
    int fail;

    fail = 0;

    /* === Function: geda_net_get_x0  === */

    value = geda_net_get_x0(net);
    if (value - x1) {
      fprintf(stderr, "FAILED: geda_net_get_x0 %d != %d\n", value, x1);
      fail++;
    }

    /* === Function: geda_net_get_x1  === */

    value = geda_net_get_x1(net);

    if (value - x2) {
      fprintf(stderr, "FAILED: geda_net_get_x1 %d != %d\n", value, x2);
      fail++;
    }

    /* === Function: geda_net_get_y0  === */

    value = geda_net_get_y0(net);
    if (value - y1) {
      fprintf(stderr, "FAILED: geda_net_get_y0 %d != %d\n", value, y1);
      fail++;
    }

    /* === Function: geda_net_get_y1  === */

    value = geda_net_get_y1(net);

    if (value - y2) {
      fprintf(stderr, "FAILED: geda_net_get_y1 %d != %d\n", value, y2);
      fail++;
    }

    /* Reverse the coordinates */

    /* === Function: geda_net_set_x0  === */
    geda_net_set_x0(net, x2);

    if (GEDA_LINE(net)->x[0] - x2) {
      fprintf(stderr, "FAILED: geda_net_set_x0\n");
      fail++;
    }

    /* === Function: geda_net_set_x1  === */
    geda_net_set_x1(net, x1);

    if (GEDA_LINE(net)->x[1] - x1) {
      fprintf(stderr, "FAILED: geda_net_set_x1\n");
      fail++;
    }

    /* === Function: geda_net_set_y0  === */
    geda_net_set_y0(net, y2);

    if (GEDA_LINE(net)->y[0] - y2) {
      fprintf(stderr, "FAILED: geda_net_set_y0\n");
      fail++;
    }

    /* === Function: geda_net_set_y1  === */
    geda_net_set_y1(net, y1);

    if (GEDA_LINE(net)->y[1] - y1) {
      fprintf(stderr, "FAILED: geda_net_set_y1\n");
      fail++;
    }

    if (fail) {
      result++;
      break;
    }
    g_object_unref (object0);
  }

  GedaObject *object1 = geda_net_object_new(0, 1, 1, 10, 10);

  if (object1) {

    const char *str;

    geda_net_set_netname (object1, "net_24");

    str = object1->net->net_name;

    if (!str) {
      fprintf(stderr, "FAILED: geda_net_set_netname\n");
      result++;
    }
    else if (strcmp(str, "net_24")) {
      fprintf(stderr, "FAILED: geda_net_set_netname <%s>\n", str);
      result++;
    }
    else {
      str = geda_net_get_netname (object1);
      if (!str) {
        fprintf(stderr, "FAILED: geda_net_get_netname\n");
        result++;
      }
      else if (strcmp(str, "net_24")) {
        fprintf(stderr, "FAILED: geda_net_get_netname <%s>\n", str);
        result++;
      }
    }

    geda_net_set_pin_label (object1, "VCC");

    str = object1->net->pin_label;

    if (!str) {
      fprintf(stderr, "FAILED: geda_net_set_pin_label\n");
      result++;
    }
    else if (strcmp(str, "VCC")) {
      fprintf(stderr, "FAILED: geda_net_set_pin_label <%s>\n", str);
      result++;
    }
    else {
      str = geda_net_get_pin_label (object1);
      if (!str) {
        fprintf(stderr, "FAILED: geda_net_get_pin_label\n");
        result++;
      }
      else if (strcmp(str, "VCC")) {
        fprintf(stderr, "FAILED: geda_net_get_pin_label <%s>\n", str);
        result++;
      }
    }

    g_object_unref (object1);
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

  result  = test_net();
  result += check_accessors();

  return result;
}
