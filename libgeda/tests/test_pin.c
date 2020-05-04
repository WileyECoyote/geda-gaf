/* -*- test_pin.c -*-
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
 *  Date Contributed: February, 7th, 2016
 */

#include "../../config.h"

#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaPin"

/*! \file test_pin.c
 *  \brief Tests for geda_pin.c module
 */

int test_pin (void)
{
  int result = 0;

  GedaObject *object = geda_pin_new();

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

  /* GedaPin objects are derived from GedaLine object class */
  if (!GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s is a GedaLine Failed\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_NET(object)) {
    fprintf(stderr, "%s matched type GedaNet\n", TOBJECT);
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

  if (!GEDA_IS_PIN(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
    result++;
  }

  if (GEDA_IS_TEXT(object)) {
    fprintf(stderr, "%s matched type GedaText\n", TOBJECT);
    result++;
  }

  GedaPin  *pin  = object->pin;
  GedaLine *line = object->line;

  if (!GEDA_IS_PIN(pin)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (!GEDA_IS_LINE(line)) {
    fprintf(stderr, "%s sub-pointer is a GedaLine\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_PIN) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_PIN);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_PIN(object)) {
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

int test_pin_properties (void)
{
  int result = 0;

  GedaPin *pin = g_object_new( GEDA_TYPE_PIN,
                             "type", OBJ_PIN,
                             "name", "pin",
                             "label", "Test",
                             "number", "88",
                             "sequence", "99",
                             "pin-type", "pas",
                             NULL);
  if (GEDA_IS_PIN(pin)) {

    if (!strcmp(geda_pin_get_label(pin),"Test") == 0) {
      const char *property = geda_pin_get_label(pin);
      fprintf(stderr, "%s pin label FAILED: <%s>\n", TOBJECT, property);
      result++;
    }

    if (!strcmp(pin->number,"88") == 0) {
      fprintf(stderr, "%s pin number FAILED: <%s>\n", TOBJECT, pin->number);
      result++;
    }

    if (pin->sequence != 99) {
      fprintf(stderr, "%s pin sequence FAILED: <%d>\n", TOBJECT, pin->sequence);
      result++;
    }

    if (!strcmp(geda_pin_get_electrical(pin),"pas") == 0) {
      const char *property = geda_pin_get_electrical(pin);
      fprintf(stderr, "%s electrical 1 FAILED: <%s>\n", TOBJECT, property);
      result++;
    }

    geda_pin_set_electrical(pin,"clk");
    geda_pin_set_mechanical(pin,"ball");

    if (pin->elect_type != PIN_ELECT_CLK) {
      fprintf(stderr, "%s electrical 2 FAILED: <%d>\n", TOBJECT, pin->elect_type);
      result++;
    }

    if (pin->mech_type != PIN_MECH_BALL) {
      fprintf(stderr, "%s mechanical FAILED: <%d>\n", TOBJECT, pin->mech_type);
      result++;
    }

    if (geda_pin_lookup_etype ("input") != PIN_ELECT_VOID) {
      fprintf(stderr, "%s electrical lookup type FAILED!\n", TOBJECT);
      result++;
    }

    if (geda_pin_lookup_mtype ("bar") != PIN_MECH_VOID) {
      fprintf(stderr, "%s mechanical lookup type FAILED!\n", TOBJECT);
      result++;
    }

    if (!strcmp(geda_pin_lookup_estring(PIN_ELECT_IO),"io") == 0) {
      const char *e_str = geda_pin_lookup_estring(PIN_ELECT_IO);
      fprintf(stderr, "%s lookup electrical FAILED: <%s>\n", TOBJECT, e_str);
      result++;
    }
    if (!strcmp(geda_pin_lookup_mstring(PIN_MECH_LEAD),"lead") == 0) {
      const char *m_str = geda_pin_lookup_mstring(PIN_MECH_LEAD);
      fprintf(stderr, "%s lookup mechanical FAILED: <%s>\n", TOBJECT, m_str);
      result++;
    }

  }
  else {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
    result++;
  }

  g_object_unref(pin);

  return result;
}

int check_accessors ()
{
  int result = 0;

  GedaObject *object = geda_pin_new();

  if (!GEDA_IS_PIN(object->pin)) {
    fprintf(stderr, "is a %s Failed line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaLine *line = object->line;

    int count;
    int fail;
    int value;

    fail = 0;
    for (count = 0; count < 100; count++) {

      int x1 = geda_random_number (0, 120000);
      int y1 = geda_random_number (0, 80000);

      int x2 = x1 + geda_random_number (0, 1000);
      int y2 = y1 + geda_random_number (0, 1000);

      geda_line_set_x1(line, x1);
      geda_line_set_y1(line, y1);
      geda_line_set_x2(line, x2);
      geda_line_set_y2(line, y2);

      value = line->x[0];
      if (value - x1) {
        fprintf(stderr, "FAILED: pin x1 %d != %d\n", value, x1);
        fail++;
      }
      else {

        value = geda_line_get_x1(line);
        if (value - x1) {
          fprintf(stderr, "FAILED: pin %d != %d\n", value, x2);
          fail++;
        }
      }

      value = line->x[1];
      if (value - x2) {
        fprintf(stderr, "FAILED: pin %d != %d\n", value, x2);
        fail++;
      }
      else {

        value = geda_line_get_x2(line);

        if (value - x2) {
          fprintf(stderr, "FAILED: pin %d != %d\n", value, x2);
          fail++;
        }
      }

      value = line->y[0];
      if (value - y1) {
        fprintf(stderr, "FAILED: pin %d != %d\n", value, y1);
        fail++;
      }
      else {

        value = geda_line_get_y1(line);

        if (value - y1) {
          fprintf(stderr, "FAILED: pin %d != %d\n", value, y1);
          fail++;
        }
      }

      value = line->y[1];
      if (value - y2) {
        fprintf(stderr, "FAILED: pin %d != %d\n", value, y2);
        fail++;
      }
      else {

        value = geda_line_get_y2(line);

        if (value - y2) {
          fprintf(stderr, "FAILED: pin %d != %d\n", value, y2);
          fail++;
        }
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t          x1: %d\n", x1);
        fprintf(stderr, "\t          y1: %d\n", y1);
        fprintf(stderr, "\t          x2: %d\n", x2);
        fprintf(stderr, "\t          y2: %d\n", y2);

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

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result = test_pin();

  if (!result) {

    result = test_pin_properties();

    result += check_accessors();
  }

  return result;
}
