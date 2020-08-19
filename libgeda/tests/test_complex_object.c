/* -*- test_complex_object.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2018 gEDA Contributors (see ChangeLog for details)
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
 *  Date Contributed: June, 28th, 2018
 */

#include "../../config.h"

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "test-suite.h"

/*! \file test_complex_object.c
 *  \brief Tests for o_complex_object.c module
 *  \par
 *  This module provides basic unit tests for functions in the
 *  o_complex_object module.
 */

/*! \def MUT Module Under Tests */
#define MUT "src/object/o_complex_object.c"

#define TOBJECT  "GedaComplex"
#define SYM_FILE "data/ATMega32-DIP_test.sym"
#define SYMBOL   "ATMega32-DIP_test.sym"

/** \defgroup test-object-geda-complex Test GEDA Complex object Module
 * @{
 * \brief Group 8 src/object/o_complex_object.c geda_complex_object_
 *  Group 8 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  08  88  88
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
 *      O0801     geda_complex_object_copy
 *      O0802     geda_complex_object_check_symbol_version
 *      O0803     geda_complex_object_get_filename
 *      O0804     geda_complex_object_get_nearest_point
 *      O0805     geda_complex_object_get_pin_objs
 *      O0806     geda_complex_object_get_position
 *      O0807    geda_complex_object_get_prim_objs
 *      O0808     geda_complex_object_get_promotable
 *      O0809     geda_complex_object_find_pin_by_attribute
 *      O0810    geda_complex_object_is_embedded
 *      O0811     geda_complex_object_mirror
 *      O0812    geda_complex_object_new
 *      O0813    geda_complex_object_new_embedded
 *      O0814     geda_complex_object_promote_attribs
 *      O0815     geda_complex_object_read
 *      O0816     geda_complex_object_reset_refdes
 *      O0817     geda_complex_object_rotate
 *      O0818     geda_complex_object_shortest_distance
 *      O0819     geda_complex_object_translate
 */

/* Adds data as a symbol source directory */
int pretest(void)
{
  int result = 0;

  geda_struct_clib_init();

  if (access(SYM_FILE, R_OK) != 0) {

    char *src_dir;

    src_dir = getenv ("srcdir");

    if (src_dir) {

      char *source;

      source = g_build_filename(src_dir, "data", NULL);

      geda_struct_clib_add_directory (source, "data");

      g_free (source);
    }

  }
  else {
    geda_struct_clib_add_directory ("./data", "data");
  }

  return result;
}

/* Releases clib resources */
void posttest(void)
{
  geda_struct_clib_free();
}

int check_construction (void)
{
  int count;
  int result = 0;

  const CLibSymbol *sym = geda_struct_clib_get_symbol_by_name(SYMBOL);

  const char *sym_name = geda_struct_clib_symbol_get_name (sym);

  for (count = 0; count < 10; count++) {

    int x = geda_random_number ( 0, 115000);
    int y = geda_random_number ( 0,  75000);

    /* === Function 12: geda_complex_object_new  === */

    GedaObject *object0 = geda_complex_object_new(NULL, x, y, 0, 0,
                                                  sym, sym_name, 1);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O081201A) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_COMPLEX(object0->complex)) {
      fprintf(stderr, "FAILED: (O081201B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaComplex *complex = object0->complex;
      int fail = 0;

      if (!complex->filename) {
       fprintf(stderr, "FAILED: (O081202A-%d) complex->filename is NULL\n", count);
       fail++;
      }
      else {
        if (strcmp(complex->filename, sym_name)) {
          fprintf(stderr, "FAILED: (O081202B-%d) complex->filename <%s>\n", count, complex->filename);
          fail++;
        }
      }

      if (complex->x != x) {
       fprintf(stderr, "FAILED: (O081203-%d) complex->x <%d> != <%d>\n", count, complex->x, x);
       fail++;
      }

      if (complex->y != y) {
       fprintf(stderr, "FAILED: (O081204-%d) complex->y <%d> != <%d>\n", count, complex->y, y);
       fail++;
      }

      if (complex->angle) {
       fprintf(stderr, "FAILED: (O081205-%d) complex->angle <%d>\n", count, complex->angle);
       fail++;
      }

      if (complex->mirror) {
       fprintf(stderr, "FAILED: (O081206-%d) complex->mirror <%d>\n", count, complex->mirror);
       fail++;
      }

      /* Base class member selectable is set by geda_complex_object_new */
      if (object0->selectable - 1) {
       fprintf(stderr, "FAILED: (O081207-%d) complex->selectable <%d>\n", count, object0->selectable);
       fail++;
      }

      if (fail) {
        result++;
        break;
      }

    }
    g_object_unref (object0);
  }

  /* Force creation of a placeholder object */

  GedaObject *object1 = geda_complex_object_new(NULL, 1000, 2000, 90, 0,
                                                sym, "", 0);

  if (!GEDA_IS_OBJECT(object1)) {
    fprintf(stderr, "FAILED: (O081208A) New GedaObject Failed\n");
    result++;
  }
  else if (!GEDA_IS_COMPLEX(object1->complex)) {
    fprintf(stderr, "FAILED: (O081208B) sub-pointer not a %s\n", TOBJECT);
    result++;
  }
  else if (object1->type != OBJ_PLACEHOLDER) {
    fprintf(stderr, "FAILED: (O081208C) type <%d> is not a placeholder\n", object1->type);
    result++;
  }
  else {

    GedaComplex *placeholder = object1->complex;

    if (placeholder->x != 1000) {
      fprintf(stderr, "FAILED: (O081209) placeholder->x <%d> != 1000\n", placeholder->x);
      result++;
    }

    if (placeholder->y != 2000) {
      fprintf(stderr, "FAILED: (O081210) placeholder->y <%d> != 2000\n", placeholder->y);
      result++;
    }

    if (placeholder->angle != 90) {
      fprintf(stderr, "FAILED: (O081211) placeholder->angle <%d>\n", placeholder->angle);
      result++;
    }

    if (placeholder->mirror) {
      fprintf(stderr, "FAILED: (O081212) placeholder->mirror <%d>\n", placeholder->mirror);
      result++;
    }

    /* Place holder can be non-selectable */

    if (object1->selectable) {
      fprintf(stderr, "FAILED: (O081213) placeholder->selectable <%d>\n", object1->selectable);
      result++;
    }
  }

  if (G_IS_OBJECT(object1)) {
    g_object_unref (object1);
  }

  for (count = 0; count < 10; count++) {

    int x = geda_random_number ( 0, 115000);
    int y = geda_random_number ( 0,  75000);

    /* === Function 13: geda_complex_object_new_embedded  === */

    GedaObject *object2 = geda_complex_object_new_embedded(x, y, 180, 1,sym_name, 1);

    if (!GEDA_IS_OBJECT(object2)) {
      fprintf(stderr, "FAILED: (O081301A) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_COMPLEX(object2->complex)) {
      fprintf(stderr, "FAILED: (O081301B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaComplex *complex = object2->complex;
      int fail = 0;

      if (!complex->filename) {
       fprintf(stderr, "FAILED: (O081302A-%d) complex->filename is NULL\n", count);
       fail++;
      }
      else {
        if (strcmp(complex->filename, sym_name)) {
          fprintf(stderr, "FAILED: (O081302B-%d) complex->filename <%s>\n", count, complex->filename);
          fail++;
        }
      }

      if (complex->x != x) {
       fprintf(stderr, "FAILED: (O081303-%d) complex->x <%d> != <%d>\n", count, complex->x, x);
       fail++;
      }

      if (complex->y != y) {
       fprintf(stderr, "FAILED: (O081304-%d) complex->y <%d> != <%d>\n", count, complex->y, y);
       fail++;
      }

      if (complex->angle != 180) {
       fprintf(stderr, "FAILED: (O081305-%d) complex->angle <%d>\n", count, complex->angle);
       fail++;
      }

      if (!complex->mirror) {
       fprintf(stderr, "FAILED: (O081306-%d) complex->mirror <%d>\n", count, complex->mirror);
       fail++;
      }

      /* Base class member selectable is set by geda_complex_object_new */
      if (object2->selectable - 1) {
       fprintf(stderr, "FAILED: (O081307-%d) complex->selectable <%d>\n", count, object2->selectable);
       fail++;
      }

      if (fail) {
        result++;
        break;
      }
    }

    g_object_unref (object2);

  }

  return result;
}

int check_accessors (void)
{
  int count;
  int result = 0;

  GedaToplevel *toplevel = geda_toplevel_new ();

  /* === Function 10: geda_complex_object_is_embedded NULL === */
  if (geda_complex_object_is_embedded (NULL)) {
    fprintf(stderr, "FAILED: (O081000) object_is_embedded NULL\n");
    result++;
  }

  const CLibSymbol *sym = geda_struct_clib_get_symbol_by_name(SYMBOL);

  const char *sym_name = geda_struct_clib_symbol_get_name (sym);

  for (count = 0; count < 10; count++) {

    int x = geda_random_number ( 0, 115000);
    int y = geda_random_number ( 0,  75000);

    GedaObject *object1 = geda_complex_object_new(toplevel, x, y, 0, 0,
                                                  sym, sym_name, 1);

    if (!GEDA_IS_OBJECT(object1)) {
      fprintf(stderr, "FAILED: (O081201C) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_COMPLEX(object1->complex)) {
      fprintf(stderr, "FAILED: (O081201D) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      int fail = 0;

      /* === Function 03: geda_complex_object_get_filename  === */

      const char *fname;

      fname = geda_complex_object_get_filename (object1);

      if (!fname) {
        fprintf(stderr, "FAILED: (O080301A) get_filename Failed\n");
        result++;
      }
      else if (strcmp(fname, sym_name)) {
        fprintf(stderr, "FAILED: (O080301B) get_filename Failed <%s>\n", fname);
        result++;
      }

      /* === Function 05: geda_complex_object_get_pin_objs  === */

      GList *p_list;
      int    p_count;

      p_list = geda_complex_object_get_pin_objs(object1);

      if (!p_list) {
        fprintf(stderr, "FAILED: (O080501A) get_pin_objs Failed <%p>\n", object1->complex->pin_objs);
        result++;
      }
      else {

        p_count = g_list_length(p_list);

        /* The number of pins in ATMega32-DIP_test.sym */
        if (p_count != 40) {
          fprintf(stderr, "FAILED: (O080501B) get_pin_objs Failed <%d>\n", p_count);
          result++;
        }
      }

      /* === Function 07: geda_complex_object_get_prim_objs  === */

      GList *prim_objs;

      prim_objs = geda_complex_object_get_prim_objs(object1);

      if (prim_objs == NULL) {
        fprintf(stderr, "FAILED: (O080701A) get_prim_objs\n");
        fail++;
      }
      else {

        int count = g_list_length(prim_objs);

        if (count != 210) {
          fprintf(stderr, "FAILED: (O080701B) get_prim_objs <%d>\n", count);
          fail++;
        }
      }

      /* === Function 08: geda_complex_object_is_embedded  === */

      if (geda_complex_object_is_embedded (object1)) {
        fprintf(stderr, "FAILED: (O081001) is_embedded\n");
        fail++;
      }

      /* terminate loop if there was a failure */
      if (fail) {
        result++;
        break;
      }
    }
    g_object_unref (object1);
  }

  GedaObject *object2;

  object2 = geda_complex_object_new_embedded(100, 100, 180, 1,sym_name, 1);

  /* === Function 10: geda_complex_object_is_embedded  === */

  if (!geda_complex_object_is_embedded (object2)) {
    fprintf(stderr, "FAILED: (O081002) is_embedded\n");
    result++;
  }

  g_object_unref (object2);
  g_object_unref(toplevel);

  return result;
}

int check_get_nearest_point(GedaObject *object)
{
  int result = 0;
  int answer;
  int x, y, nx, ny;

  x = 0; y = 0;

  answer = geda_complex_object_get_nearest_point(NULL, x, y, &nx, &ny);

  if (answer) {
    fprintf(stderr, "FAILED: (O080400) get_nearest NULL\n");
    result++;
  }

  /* Q1 */

  x = 4500; y = 10000;
  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080401A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 4100) {
      fprintf(stderr, "FAILED: (O080401X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 9600) {
      fprintf(stderr, "FAILED: (O080401Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  /* Mostly North */

  x = 2000; y = 10200;

  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080402A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 2100) {
      fprintf(stderr, "FAILED: (O080402X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 9900) {
      fprintf(stderr, "FAILED: (O080402Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  /* Q2 */

  x = 100; y = 10000;

  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080403A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 500) {
      fprintf(stderr, "FAILED: (O080403X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 9600) {
      fprintf(stderr, "FAILED: (O080403Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  /* -X Axis */
  x = 100; y = 5599;

  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080404A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 200) {
      fprintf(stderr, "FAILED: (O080404X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 5300) {
      fprintf(stderr, "FAILED: (O080404Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  x = 100; y = 5601;
  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080405A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 200) {
      fprintf(stderr, "FAILED: (O080405X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 5900) {
      fprintf(stderr, "FAILED: (O080405Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  /* Q3 */

  x = 100; y = 100;
  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080406A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 500) {
      fprintf(stderr, "FAILED: (O080406X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 500) {
      fprintf(stderr, "FAILED: (O080406Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  /* South */

  x = 2300; y = 100;
  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080407A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 2500) {
      fprintf(stderr, "FAILED: (O080407X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 200) {
      fprintf(stderr, "FAILED: (O080407Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  /* South East */

  x = 4500; y = 100;
  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080408A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 4100) {
      fprintf(stderr, "FAILED: (O080408X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 500) {
      fprintf(stderr, "FAILED: (O080408Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  /* East */

  x = 7700; y = 5500;
  answer = geda_complex_object_get_nearest_point(object, x, y, &nx, &ny);

  if (!answer) {
    fprintf(stderr, "FAILED: (O080409A) get_nearest: %s\n", TOBJECT);
    result++;
  }
  else {
    if (nx - 4400) {
      fprintf(stderr, "FAILED: (O080409X) get_nearest: %s, nx=%d\n", TOBJECT, nx);
      result++;
    }
    if (ny - 5300) {
      fprintf(stderr, "FAILED: (O080409Y) get_nearest: %s, ny=%d\n", TOBJECT, ny);
      result++;
    }
  }

  return result;
}

int check_query(void)
{
  int result = 0;
  GedaToplevel *toplevel = geda_toplevel_new ();

  const CLibSymbol *sym = geda_struct_clib_get_symbol_by_name(SYMBOL);

  const char *sym_name = geda_struct_clib_symbol_get_name (sym);

  GedaObject *object = geda_complex_object_new(toplevel, 100, 100, 0, 0,
                                               sym, sym_name, 1);

  /* bounds_valid should NOT be set */
  if (object->bounds_valid) {
    fprintf(stderr, "FAILED: (O081214A) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Virtual geda_complex_bounds  === */
  if (!geda_object_bounds(object)) {
    fprintf(stderr, "FAILED: (O081214B) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* bounds_valid should be set */
  if (!object->bounds_valid) {
    fprintf(stderr, "FAILED: (O081214C) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Function 04: geda_complex_object_get_nearest_point  === */

  result += check_get_nearest_point (object);

  g_object_unref (object);

  return result;
}

/** @} endgroup test-object-geda-complex */

int
main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (pretest()) {
    fprintf(stderr, "Error during pretest, check %s\n\n", __FILE__);
    return 1;
  }

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
      result += check_query();
    }
    else {
      fprintf(stderr, "Caught signal during query in %s\n\n", MUT);
      return 1;
    }
  }
  else {
    fprintf(stderr, "discontinuing checks for %s\n\n", MUT);
  }

  posttest();
  return result;
}
