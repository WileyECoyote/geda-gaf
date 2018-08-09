/* -*- test_complex.c -*-
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

#include <libgeda.h>

#include "test-suite.h"

#define TOBJECT "GedaComplex"

/*! \file test_complex.c
 *  \brief Tests for geda_complex.c module
 */

int test_complex (void)
{
  int result = 0;

  GedaObject *object = geda_complex_new();

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

  if (!GEDA_IS_COMPLEX(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  GedaComplex *complex = object->complex;

  if (!GEDA_IS_COMPLEX(complex)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_COMPLEX) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_COMPLEX);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_COMPLEX(object)) {
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
check_append_object (GedaComplex *complex)
{
  int result = 0;

  if (geda_complex_append(NULL, NULL)) {
    fprintf(stderr, "FAILED: geda_complex_append NULLx2\n");
    return 1;
  }

  if (geda_complex_append(complex, NULL)) {
    fprintf(stderr, "FAILED: geda_complex_append NULL\n");
    return 1;
  }

  GedaObject *text_object = geda_text_new();
  GedaObject *pin_object  = geda_pin_new();

  if (!geda_complex_append(complex, text_object)) {
    fprintf(stderr, "FAILED: geda_complex_append text 1\n");
    result++;
  }
  else{

    GList *list = complex->prim_objs;

    if (!geda_complex_get_prim_objs(complex)) {
      fprintf(stderr, "FAILED: geda_complex_get_prim_objs 1\n");
      result++;
    }
    else if (geda_complex_get_prim_objs(complex) != list) {
      fprintf(stderr, "FAILED: geda_complex_get_prim_objs 2\n");
      result++;
    }

    if (list->data != text_object) {
      fprintf(stderr, "FAILED: geda_complex_append text 2\n");
      result++;
    }
  }

  if (!geda_complex_append(complex, pin_object)) {
    fprintf(stderr, "FAILED: geda_complex_append pin\n");
    result++;
  }
  else{

    GList *list = complex->prim_objs;

    if (g_list_length(list) != 2) {
      fprintf(stderr, "FAILED: geda_complex_append pin 1\n");
      result++;
    }
    else {

      list = list->next;

      if (list->data != pin_object) {
        fprintf(stderr, "FAILED: geda_complex_append pin 2\n");
        result++;
      }
    }

    list = complex->pin_objs;

    if (!geda_complex_get_pin_objs(complex)) {
      fprintf(stderr, "FAILED: geda_complex_get_pin_objs 1\n");
      result++;
    }
    else if (geda_complex_get_pin_objs(complex) != list) {
      fprintf(stderr, "FAILED: geda_complex_get_pin_objs 2\n");
      result++;
    }

    if (list->data != pin_object) {
      fprintf(stderr, "FAILED:: geda_complex_append pin 3\n");
      result++;
    }
  }

  return result;
}

int
check_accessors (void)
{
  int result = 0;
  int value;

  GedaComplex *complex;
  GedaObject  *object = geda_complex_new();

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s: is a GedaObject Failed in %s\n", TOBJECT, __func__);
    return 1;
  }

  complex = object->complex;

  result  = check_append_object(complex);

  int a = geda_random_number (0, 359);
  int x = geda_random_number (0, 115000);
  int y = geda_random_number (0, 75000);

  geda_complex_set_angle(complex, a);

  value = complex->angle;
  if (value - a) {
    fprintf(stderr, "FAILED: geda_complex_set_angle %d != %d\n", value, a);
    result++;
  }
  else {

    value = geda_complex_get_angle(complex);

    if (value - a) {
      fprintf(stderr, "FAILED: geda_complex_get_angle %d != %d\n", value, a);
      result++;
    }
  }

  geda_complex_set_filename(complex, TOBJECT);

  char *name = complex->filename;
  if (!name || strcmp(name, TOBJECT)) {
    fprintf(stderr, "FAILED: geda_complex_set_filename Line <%d> %s != %s\n", __LINE__, name, TOBJECT);
    result++;
  }
  else {

    name = geda_complex_get_filename(complex);

    if (!name || strcmp(name,TOBJECT)) {
      fprintf(stderr, "FAILED: geda_complex_set_filename Line <%d> %s != %s\n", __LINE__, name, TOBJECT);
      result++;
    }

    geda_complex_set_filename(complex, NULL);

    name = complex->filename;
    if (name) {
      fprintf(stderr, "FAILED: geda_complex_set_filename NULL Line <%d> <%p>\n", __LINE__, name);
      result++;
    }

    name = geda_complex_get_filename(complex);
    if (name) {
      fprintf(stderr, "FAILED: geda_complex_get_filename NULL Line <%d> <%p>\n", __LINE__, name);
      result++;
    }
  }

  geda_complex_set_is_embedded(complex, TRUE);

  if (!complex->is_embedded) {
    fprintf(stderr, "FAILED: geda_complex_set_is_embedded != TRUE\n");
    result++;
  }
  else {

    if (!geda_complex_get_is_embedded(complex)) {
      fprintf(stderr, "FAILED: geda_complex_get_angle != TRUE\n");
      result++;
    }
  }

  geda_complex_set_is_embedded(complex, FALSE);

  if (complex->is_embedded) {
    fprintf(stderr, "FAILED: geda_complex_set_is_embedded != FALSE\n");
    result++;
  }
  else {

    if (geda_complex_get_is_embedded(complex)) {
      fprintf(stderr, "FAILED: geda_complex_get_angle != FALSE\n");
      result++;
    }
  }

  geda_complex_set_is_mirror(complex, TRUE);

  if (!complex->mirror) {
    fprintf(stderr, "FAILED: geda_complex_set_is_mirror != TRUE\n");
    result++;
  }
  else {

    if (!geda_complex_get_is_mirror(complex)) {
      fprintf(stderr, "FAILED: geda_complex_get_is_mirror != TRUE\n");
      result++;
    }
  }

  geda_complex_set_is_mirror(complex, FALSE);

  if (complex->mirror) {
    fprintf(stderr, "FAILED: geda_complex_set_is_mirror != FALSE\n");
    result++;
  }
  else {

    if (geda_complex_get_is_mirror(complex)) {
      fprintf(stderr, "FAILED: geda_complex_get_is_mirror != FALSE\n");
      result++;
    }
  }

  geda_complex_set_x(complex, x);

  value = complex->x;
  if (value - x) {
    fprintf(stderr, "FAILED: geda_complex_set_x %d != %d\n", value, x);
    result++;
  }
  else {

    value = geda_complex_get_x(complex);

    if (value - x) {
      fprintf(stderr, "FAILED: geda_complex_get_x %d != %d\n", value, x);
      result++;
    }
  }

  geda_complex_set_y(complex, y);

  value = complex->y;
  if (value - y) {
    fprintf(stderr, "FAILED: geda_complex_set_y %d != %d\n", value, y);
    result++;
  }
  else {

    value = geda_complex_get_y(complex);

    if (value - y) {
      fprintf(stderr, "FAILED: geda_complex_get_y %d != %d\n", value, y);
      result++;
    }
  }

  g_object_unref(object);

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
    result = test_complex();
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
      fprintf(stderr, "Caught signal checking accessors for %s\n\n", TOBJECT);
      return 1;
    }
  }
  else {
    fprintf(stderr, "discontinuing checks for %s\n\n", TOBJECT);
  }

  return result > 0;
}
