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
 *      O0806     geda_complex_object_get_prim_objs
 *      O0807     geda_complex_object_find_pin_by_attribute
 *      O0808     geda_complex_object_is_embedded
 *      O0809     geda_complex_object_mirror
 *      O0810    geda_complex_object_new
 *      O0811    geda_complex_object_new_embedded
 *      O0812     geda_complex_object_promote_attribs
 *      O0813     geda_complex_object_reset_refdes
 *      O0814     geda_complex_object_rotate
 *      O0815     geda_complex_object_translate
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

    /* === Function 10: geda_complex_object_new  === */

    GedaObject *object0 = geda_complex_object_new(NULL, x, y, 0, 0,
                                                  sym, sym_name, 1);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O081001A) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_COMPLEX(object0->complex)) {
      fprintf(stderr, "FAILED: (O081001B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaComplex *complex = object0->complex;
      int fail = 0;

      if (!complex->filename) {
       fprintf(stderr, "FAILED: (O081002A-%d) complex->filename is NULL\n", count);
       fail++;
      }
      else {
        if (strcmp(complex->filename, sym_name)) {
          fprintf(stderr, "FAILED: (O081002B-%d) complex->filename <%s>\n", count, complex->filename);
          fail++;
        }
      }

      if (complex->x != x) {
       fprintf(stderr, "FAILED: (O081003-%d) complex->x <%d> != <%d>\n", count, complex->x, x);
       fail++;
      }

      if (complex->y != y) {
       fprintf(stderr, "FAILED: (O081004-%d) complex->y <%d> != <%d>\n", count, complex->y, y);
       fail++;
      }

      if (complex->angle) {
       fprintf(stderr, "FAILED: (O081005-%d) complex->angle <%d>\n", count, complex->angle);
       fail++;
      }

      if (complex->mirror) {
       fprintf(stderr, "FAILED: (O081006-%d) complex->mirror <%d>\n", count, complex->mirror);
       fail++;
      }

      /* Base class member selectable is set by geda_complex_object_new */
      if (object0->selectable - 1) {
       fprintf(stderr, "FAILED: (O081007-%d) complex->selectable <%d>\n", count, object0->selectable);
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
    fprintf(stderr, "FAILED: (O081008A) New GedaObject Failed\n");
    result++;
  }
  else if (!GEDA_IS_COMPLEX(object1->complex)) {
    fprintf(stderr, "FAILED: (O081008B) sub-pointer not a %s\n", TOBJECT);
    result++;
  }
  else if (object1->type != OBJ_PLACEHOLDER) {
    fprintf(stderr, "FAILED: (O081008C) type <%d> is not a placeholder\n", object1->type);
    result++;
  }
  else {

    GedaComplex *placeholder = object1->complex;

    if (placeholder->x != 1000) {
      fprintf(stderr, "FAILED: (O081009) placeholder->x <%d> != 1000\n", placeholder->x);
      result++;
    }

    if (placeholder->y != 2000) {
      fprintf(stderr, "FAILED: (O081010) placeholder->y <%d> != 2000\n", placeholder->y);
      result++;
    }

    if (placeholder->angle != 90) {
      fprintf(stderr, "FAILED: (O081011) placeholder->angle <%d>\n", placeholder->angle);
      result++;
    }

    if (placeholder->mirror) {
      fprintf(stderr, "FAILED: (O081012) placeholder->mirror <%d>\n", placeholder->mirror);
      result++;
    }

    /* Place holder can be non-selectable */

    if (object1->selectable) {
      fprintf(stderr, "FAILED: (O081013) placeholder->selectable <%d>\n", object1->selectable);
      result++;
    }
  }

  if (G_IS_OBJECT(object1)) {
    g_object_unref (object1);
  }

  for (count = 0; count < 10; count++) {

    int x = geda_random_number ( 0, 115000);
    int y = geda_random_number ( 0,  75000);

    /* === Function 11: geda_complex_object_new_embedded  === */

    GedaObject *object2 = geda_complex_object_new_embedded(x, y, 180, 1,sym_name, 1);

    if (!GEDA_IS_OBJECT(object2)) {
      fprintf(stderr, "FAILED: (O081101A) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_COMPLEX(object2->complex)) {
      fprintf(stderr, "FAILED: (O081101B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaComplex *complex = object2->complex;
      int fail = 0;

      if (!complex->filename) {
       fprintf(stderr, "FAILED: (O081102A-%d) complex->filename is NULL\n", count);
       fail++;
      }
      else {
        if (strcmp(complex->filename, sym_name)) {
          fprintf(stderr, "FAILED: (O081102B-%d) complex->filename <%s>\n", count, complex->filename);
          fail++;
        }
      }

      if (complex->x != x) {
       fprintf(stderr, "FAILED: (O081103-%d) complex->x <%d> != <%d>\n", count, complex->x, x);
       fail++;
      }

      if (complex->y != y) {
       fprintf(stderr, "FAILED: (O081104-%d) complex->y <%d> != <%d>\n", count, complex->y, y);
       fail++;
      }

      if (complex->angle != 180) {
       fprintf(stderr, "FAILED: (O081105-%d) complex->angle <%d>\n", count, complex->angle);
       fail++;
      }

      if (!complex->mirror) {
       fprintf(stderr, "FAILED: (O081106-%d) complex->mirror <%d>\n", count, complex->mirror);
       fail++;
      }

      /* Base class member selectable is set by geda_complex_object_new */
      if (object2->selectable - 1) {
       fprintf(stderr, "FAILED: (O081107-%d) complex->selectable <%d>\n", count, object2->selectable);
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

  const CLibSymbol *sym = geda_struct_clib_get_symbol_by_name(SYMBOL);

  const char *sym_name = geda_struct_clib_symbol_get_name (sym);

  for (count = 0; count < 10; count++) {

    int x = geda_random_number ( 0, 115000);
    int y = geda_random_number ( 0,  75000);

    GedaObject *object1 = geda_complex_object_new(NULL, x, y, 0, 0,
                                                  sym, sym_name, 1);

    if (!GEDA_IS_OBJECT(object1)) {
      fprintf(stderr, "FAILED: (O081001C) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_COMPLEX(object1->complex)) {
      fprintf(stderr, "FAILED: (O081001D) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      int fail = 0;

      /* === Function 03: geda_complex_object_get_filename  === */
      /* === Function 04: geda_complex_object_get_nearest_point  === */
      /* === Function 05: geda_complex_object_get_pin_objs  === */
      /* === Function 06: geda_complex_object_get_prim_objs  === */
      /* === Function 08: geda_complex_object_is_embedded  === */

      if (fail) {
        result++;
        break;
      }
    }
    g_object_unref (object1);
  }
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
      fprintf(stderr, "Caught signal checking accessors in object/o_complex_object.c\n\n");
      return 1;
    }
  }
  else {
    fprintf(stderr, "discontinuing checks for object/o_complex_object.c\n\n");
  }

  posttest();
  return result;
}
