/* -*- test_bus.c -*-
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
 *  Date Contributed: January, 30th, 2016
 */

#include "../../config.h"

#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaBus"

/*! \file test_bus.c
 *  \brief Tests for geda_bus.c module
 */

int check_bus (void)
{
  int result = 0;

  GedaObject *object = geda_bus_new();

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

  if (!GEDA_IS_BUS(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  /* GedaBus objects are derived from GedaLine object class */
  if (!GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s is a GedaLine Failed\n", TOBJECT);
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

  GedaBus  *bus  = object->bus;
  GedaLine *line = object->line;

  if (!GEDA_IS_BUS(bus)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (!GEDA_IS_LINE(line)) {
    fprintf(stderr, "%s sub-pointer is a GedaLine\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_BUS) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_BUS);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_BUS(object)) {
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
check_properties (void)
{
  int result = 0;

  GedaObject *object = geda_bus_new();

  if (!GEDA_IS_BUS(object->bus)) {
    fprintf(stderr, "is a %s Failed line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaBus *bus = object->bus;

    int count;
    int fail;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x1 = geda_random_number ( 0,       119800);
      int y2 = geda_random_number ( 0,        79800);
      int x2 = geda_random_number (x1 + 100, 120000);
      int y1 = geda_random_number (y2 + 100,  80000);
      int d  = geda_random_number (0, 1);

      g_object_set(bus, "bus-ripper-direction", d,
                        "first-x",  x1,
                        "first-y",  y1,
                        "second-x", x2,
                        "second-y", y2,
                        NULL);

      int rd, rx1, ry1, rx2, ry2;

      g_object_get(bus, "bus-ripper-direction", &rd,
                        "first-x",  &rx1,
                        "first-y",  &ry1,
                        "second-x", &rx2,
                        "second-y", &ry2,
                        NULL);

      if (d - rd) {
        fprintf(stderr, "FAILED: %s get/set bus-ripper-direction property <%d>\n", TOBJECT, rd);
        fail++;
      }

      if (x1 - rx1) {
        fprintf(stderr, "FAILED: %s get/set first x property <%d>\n", TOBJECT, rx1);
        fail++;
      }

      if (y1 - ry1) {
        fprintf(stderr, "FAILED: %s get/set first y property <%d>\n", TOBJECT, ry1);
        fail++;
      }

      if (x2 - rx2) {
        fprintf(stderr, "FAILED: %s get/set second x property <%d>\n", TOBJECT, rx2);
        fail++;
      }

      if (y2 - ry2) {
        fprintf(stderr, "FAILED: %s get/set second y property <%d>\n", TOBJECT, ry2);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tdirection: %d\n", d);
        fprintf(stderr, "\tfirst-x: %d\n",  x1);
        fprintf(stderr, "\tfirst-y: %d\n",  y1);
        fprintf(stderr, "\tsecond-x: %d\n", x2);
        fprintf(stderr, "\tsecond-y: %d\n", y2);

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

  if (geda_bus_get_position(NULL, NULL, NULL)) {
    fprintf(stderr, "FAILED: geda_bus_get_position NULL\n");
  }

  count = geda_bus_get_ripper_direction(NULL);
  count = geda_bus_get_x0(NULL);
  count = geda_bus_get_x1(NULL);
  count = geda_bus_get_y0(NULL);
  count = geda_bus_get_y1(NULL);

  geda_bus_set_ripper_direction(NULL, 1);
  geda_bus_set_x0(NULL, 18);
  geda_bus_set_x1(NULL, 19);
  geda_bus_set_y0(NULL, 20);
  geda_bus_set_y1(NULL, 21);

  for (count = 0; count < 3; count++) {

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       119800);
    int y2 = geda_random_number ( 0,        79800);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y1 = geda_random_number (y2 + 100,  80000);
    int d  = geda_random_number (0, 1);

    GedaObject *object0 = geda_bus_object_new(c, x1, y1, x2, y2, d);
    GedaBus    *bus     = object0->bus;

    int value;
    int fail;

    fail = 0;

    /* === Function: geda_bus_get_position  === */

    int tx, ty;;

    if (!geda_bus_get_position(bus, &tx, &ty)) {
      fprintf(stderr, "FAILED: geda_bus_get_position\n");
      fail++;
    }
    else {
      if (tx != x1) {
        fprintf(stderr, "FAILED: geda_bus_get_position left != <%d>\n", tx);
        fail++;
      }
      if (ty != y1) {
        fprintf(stderr, "FAILED: geda_bus_get_position lower != <%d>\n", ty);
        fail++;
      }
    }

    /* === Function: geda_bus_get_ripper_direction  === */

    value = geda_bus_get_ripper_direction(bus);

    if (value - d) {
      fprintf(stderr, "FAILED: geda_bus_get_ripper_direction <%d>\n", value);
      fail++;
    }

    /* Toggle the direction */
    geda_bus_set_ripper_direction(bus, !value);

    if (bus->ripper_direction == value) {
      fprintf(stderr, "FAILED: geda_bus_set_ripper_direction\n");
      fail++;
    }

    /* === Function: geda_bus_get_x0  === */

    value = geda_bus_get_x0(bus);
    if (value - x1) {
      fprintf(stderr, "FAILED: geda_bus_get_x0 %d != %d\n", value, x1);
      fail++;
    }

    /* === Function: geda_bus_get_x1  === */

    value = geda_bus_get_x1(bus);

    if (value - x2) {
      fprintf(stderr, "FAILED: geda_bus_get_x1 %d != %d\n", value, x2);
      fail++;
    }

    /* === Function: geda_bus_get_y0  === */

    value = geda_bus_get_y0(bus);
    if (value - y1) {
      fprintf(stderr, "FAILED: geda_bus_get_y0 %d != %d\n", value, y1);
      fail++;
    }

    /* === Function: geda_bus_get_y1  === */

    value = geda_bus_get_y1(bus);

    if (value - y2) {
      fprintf(stderr, "FAILED: geda_bus_get_y1 %d != %d\n", value, y2);
      fail++;
    }

    /* Reverse the coordinates */

    /* === Function: geda_bus_set_x0  === */
    geda_bus_set_x0(bus, x2);

    if (GEDA_LINE(bus)->x[0] - x2) {
      fprintf(stderr, "FAILED: geda_bus_set_x0\n");
      fail++;
    }

    /* === Function: geda_bus_set_x1  === */
    geda_bus_set_x1(bus, x1);

    if (GEDA_LINE(bus)->x[1] - x1) {
      fprintf(stderr, "FAILED: geda_bus_set_x1\n");
      fail++;
    }

    /* === Function: geda_bus_set_y0  === */
    geda_bus_set_y0(bus, y2);

    if (GEDA_LINE(bus)->y[0] - y2) {
      fprintf(stderr, "FAILED: geda_bus_set_y0\n");
      fail++;
    }

    /* === Function: geda_bus_set_y1  === */
    geda_bus_set_y1(bus, y1);

    if (GEDA_LINE(bus)->y[1] - y1) {
      fprintf(stderr, "FAILED: geda_bus_set_y1\n");
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
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = check_bus();
  result += check_properties();
  result += check_accessors();

  if (result) {
    fprintf(stderr, "Check module geda_bus.c");
  }

  return result;
}
