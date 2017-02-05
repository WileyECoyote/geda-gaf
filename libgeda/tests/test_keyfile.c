/* -*- test_keyfile.c -*-
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
 *  Date Contributed: December, 4th, 2016
 */

#include <libgeda.h>

#include "test_parsecmd.h"

#define TOBJECT "GedaKeyFile"
#define KEY_FILENAME "./test_keyfile.ini"

const char key_data[] = "[Test]\nT1=A\nT2=B\nT3=C\n";

/*! \file test_keyfile.c
 *  \brief Tests for geda_keyfile.c module
 *  \par
 *  This module provides basic unit tests for keyfile operations.
 */

int create_keyfile_data (void)
{
  int result = 0;
  GError *err;
  char   *data;

  /* === Function 01: geda_keyfile_new  === */
  GedaKeyFile *keyfile = geda_keyfile_new ();

  if (!GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080101) geda_keyfile_new\n");
    return 1;
  }

  err = NULL;

  result = !geda_keyfile_load_from_data (keyfile, &key_data[0], sizeof(key_data),
                                        GEDA_KEYFILE_NONE, &err);

  if (result) {
    fprintf(stderr, "FAILED: (KF080701) geda_keyfile_load_from_data: <%s>\n", err->message);
    g_error_free (err);
  }
  else {

    /* geda_keyfile_set_top_comment */
    if (!geda_keyfile_set_comment (keyfile, NULL, NULL, "Toplevel Comment", NULL))
      fprintf(stderr, "%s: set top comments returned FALSE, expect failure\n", __func__);

    /* geda_keyfile_set_group_comment */
    if (!geda_keyfile_set_comment (keyfile, "Test", NULL, "Group Comment", NULL))
      fprintf(stderr, "%s: set group comments returned FALSE, expect failure\n", __func__);

    /* geda_keyfile_set_key_comment */
    if (!geda_keyfile_set_comment (keyfile, "Test", "T1", "Key Comment", NULL))
      fprintf(stderr, "%s: set key comments returned FALSE, expect failure\n", __func__);

    /* geda_keyfile_set_group_comment non-existence group */
    if (geda_keyfile_set_comment (keyfile, "NoExist", NULL, __func__, &err)) {
      fprintf(stderr, "FAILED: (KF084201A) set non-existence group comment\n");
      result++;
    }
    else {
      if (!err) {
        fprintf(stderr, "FAILED: (KF084201B) set group comment error is NULL\n");
        result++;
      }
      else {
        vmessage("Message: (KF084201C) %s.\n", err->message);
        g_error_free (err);
      }
    }

    err = NULL;

    /* geda_keyfile_set_key_comment non-existence group */
    if (geda_keyfile_set_comment (keyfile, "NoExist", "BadKey", __func__, &err)) {
      fprintf(stderr, "FAILED: (KF084202A) set non-existence key comment\n");
      result++;
    }
    else {
      if (!err) {
        fprintf(stderr, "FAILED: (KF084202B) set group comment error is NULL\n");
        result++;
      }
      else {
        vmessage("Message: (KF084202C) %s.\n", err->message);
        g_error_free (err);
      }
    }

    data = geda_keyfile_to_data(keyfile, NULL, NULL);
    g_file_set_contents(KEY_FILENAME, data, -1, NULL);

    GEDA_FREE(data);
  }

  /* Add a reference to the object, cnt=2 */
  geda_keyfile_ref(keyfile);

  geda_keyfile_free(keyfile); /* cnt=1 */

  if (!GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080201) geda_keyfile_ref\n");
    result++;
  }

  /* Add another reference to the object, cnt=2 */
  geda_keyfile_ref(keyfile);

  /* Remove reference, cnt=1  */
  geda_keyfile_unref(keyfile);

  if (!GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080301) geda_keyfile_unref\n");
    result++;
  }

  /* Remove reference, cnt=0 */
  geda_keyfile_free(keyfile);

  if (GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080401) geda_keyfile_free\n");
    result++;
  }

  return result;
}

int check_load_keyfile (void)
{
  int result = 0;

  if (g_file_test (KEY_FILENAME, G_FILE_TEST_EXISTS)) {

  }
  else {
    fprintf(stderr, "FAILED: (KF081001) key file not found: <%s>\n", KEY_FILENAME);
    result++;
  }
  return result;
}

int check_get_set (void)
{
  int result = 0;

  return result;
}

/** @} endgroup test-geda-keyfile */

int
main (int argc, char *argv[])
{
  int result = 0;

  parse_commandline(argc, argv);

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = create_keyfile_data();

  result += check_load_keyfile();

  result += check_get_set();

  if (result) {
    fprintf(stderr, "Check module geda_keyfile.c");
  }
  else {
    geda_remove_file(KEY_FILENAME);
  }

  return (result > 0);
}
