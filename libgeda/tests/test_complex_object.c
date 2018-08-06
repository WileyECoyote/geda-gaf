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
 *      O0801    geda_complex_object_copy
 *      O0802    geda_complex_object_check_symbol_version
 *      O0803    geda_complex_object_get_filename
 *      O0804    geda_complex_object_get_nearest_point
 *      O0805    geda_complex_object_get_pin_objs
 *      O0806    geda_complex_object_get_prim_objs
 *      O0807    geda_complex_object_find_pin_by_attribute
 *      O0808    geda_complex_object_is_embedded
 *      O0809    geda_complex_object_mirror
 *      O0810    geda_complex_object_new
 *      O0811    geda_complex_object_new_embedded
 *      O0812    geda_complex_object_promote_attribs
 *      O0813    geda_complex_object_reset_refdes
 *      O0814    geda_complex_object_rotate
 *      O0815    geda_complex_object_translate
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

int
check_construction (void)
{
  int count;
  int result = 0;

  const CLibSymbol *sym = geda_struct_clib_get_symbol_by_name(SYMBOL);

  const char *sym_name = geda_struct_clib_symbol_get_name (sym);

  for (count = 0; count < 10; count++) {

    int x = geda_random_number ( 0, 115000);
    int y = geda_random_number ( 0,  75000);

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

      int fail = 0;

      if (fail) {
        result++;
        break;
      }

    }
    g_object_unref (object0);
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

  posttest();
  return result;
}
