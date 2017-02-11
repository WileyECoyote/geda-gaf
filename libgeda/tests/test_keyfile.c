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
#define NOT_KEY_FILENAME "./test_Nofile.ini"

/* Comment strings written to and read from KEY_FILENAME */
#define TOP_COMMENTS_STR "Toplevel Comment\nline 2"
#define GRP_COMMENTS_STR "Group Comment"
#define KEY_COMMENTS_STR "Key Comment"

const char key_data[] = "[G1]\nT1=A\n[G2]\nT1=B\nT2=C\nT3=D\n";
//const char key_data[] = "[G2]\nT1=B\nT2=C\nT3=D\n";

/*! \file test_keyfile.c
 *  \brief Tests for geda_keyfile.c module
 *  \par
 *  This module provides basic unit tests for keyfile operations.
 */

/** \defgroup test-geda-keyfile Test GEDA Keyfile Module
 * @{
 * \brief Group 8 src/geda/geda_keyfile.c geda_circle_object_
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
 *      KF0801    geda_keyfile_new
 *      KF0802    geda_keyfile_ref
 *      KF0803    geda_keyfile_unref
 *      KF0804    geda_keyfile_free
 *       KF0805    geda_keyfile_set_list_separator
 *      KF0806    geda_keyfile_load_from_file
 *      KF0807    geda_keyfile_load_from_data
 *       KF0808    geda_keyfile_load_from_dirs
 *       KF0809    geda_keyfile_load_from_data_dirs
 *      KF0810    geda_keyfile_to_data
 *       KF0811    geda_keyfile_get_start_group
 *       KF0812    geda_keyfile_get_groups
 *       KF0813    geda_keyfile_get_keys
 *       KF0814    geda_keyfile_has_group
 *       KF0815    geda_keyfile_has_key
 *       KF0816    geda_keyfile_get_value
 *       KF0817    geda_keyfile_set_value
 *       KF0818    geda_keyfile_get_string
 *       KF0819    geda_keyfile_set_string
 *       KF0820    geda_keyfile_get_locale_string
 *       KF0821    geda_keyfile_set_locale_string
 *       KF0822    geda_keyfile_get_boolean
 *       KF0823    geda_keyfile_set_boolean
 *       KF0824    geda_keyfile_get_integer
 *       KF0825    geda_keyfile_set_integer
 *       KF0826    geda_keyfile_get_int64
 *       KF0827    geda_keyfile_set_int64
 *       KF0828    geda_keyfile_get_uint64
 *       KF0829    geda_keyfile_set_uint64
 *       KF0830    geda_keyfile_get_double
 *       KF0831    geda_keyfile_set_double
 *       KF0832    geda_keyfile_get_string_list
 *       KF0833    geda_keyfile_set_string_list
 *       KF0834    geda_keyfile_get_locale_string_list
 *       KF0835    geda_keyfile_set_locale_string_list
 *       KF0836    geda_keyfile_get_boolean_list
 *       KF0837    geda_keyfile_set_boolean_list
 *       KF0838    geda_keyfile_set_double_list
 *       KF0839    geda_keyfile_get_double_list
 *       KF0840    geda_keyfile_get_integer_list
 *       KF0841    geda_keyfile_set_integer_list
 *      KF0842    geda_keyfile_set_comment
 *      KF0843    geda_keyfile_get_comment
 *      KF0844    geda_keyfile_remove_comment
 *       KF0845    geda_keyfile_remove_key
 *       KF0846    geda_keyfile_remove_group
 */

int create_keyfile_data (void)
{
  int result = 0;
  GError *err;
  char   *data;

  /* === Function 01: geda_keyfile_new  === */
  GedaKeyFile *keyfile = geda_keyfile_new ();

  if (!GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080101A) geda_keyfile_new\n");
    return 1;
  }

  err = NULL;

  /* === Function 07: geda_keyfile_load_from_data  === */
  result = !geda_keyfile_load_from_data (keyfile, &key_data[0], sizeof(key_data),
                                         GEDA_KEYFILE_NONE, &err);

  if (result) {
    fprintf(stderr, "FAILED: (KF080701) geda_keyfile_load_from_data: <%s>\n", err->message);
    g_error_free (err);
  }
  else {

    /* === Function 42: geda_keyfile_set_comment varients === */

    /* geda_keyfile_set_top_comment */
    if (!geda_keyfile_set_comment (keyfile, NULL, NULL, TOP_COMMENTS_STR, NULL))
      fprintf(stderr, "%s: set top comments returned FALSE, expect failure\n", __func__);

    /* geda_keyfile_set_group_comment */
    if (!geda_keyfile_set_comment (keyfile, "G2", NULL, GRP_COMMENTS_STR, NULL))
      fprintf(stderr, "%s: set group comments returned FALSE, expect failure\n", __func__);

    /* geda_keyfile_set_key_comment */
    if (!geda_keyfile_set_comment (keyfile, "G2", "T1", KEY_COMMENTS_STR, NULL))
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

    /* === Function 10: geda_keyfile_to_data === */

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

int check_keyfile_comments (void)
{
  int result = 0;
  GError *err = NULL;

  /* === Function 01: geda_keyfile_new  === */
  GedaKeyFile *keyfile = geda_keyfile_new ();

  if (!GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080101B) geda_keyfile_new\n");
    return 1;
  }

  if (geda_keyfile_load_from_file (NULL, NOT_KEY_FILENAME, 0, NULL))
  {
    fprintf(stderr, "FAILED: (KF080600A) NULL NOT_KEY_FILENAME NULL\n");
    result++;
  }

  if (geda_keyfile_load_from_file (keyfile, NOT_KEY_FILENAME, 0, &err))
  {
    fprintf(stderr, "FAILED: (KF080600B) NOT_KEY_FILENAME NULL\n");
    result++;
  }
  else if (!err) {
      fprintf(stderr, "FAILED: (KF080600C) NULL NOT_KEY_FILENAME error is NULL\n");
      result++;
  }
  else {
      vmessage("Message: (KF080600D) %s.\n", err->message);
      g_error_free (err);
  }

  if (g_file_test (KEY_FILENAME, G_FILE_TEST_EXISTS)) {

    err = NULL;

    /* === Function 06: geda_keyfile_load_from_file === */

    /* Actually Load the file, comments should be stripped */
    if (!geda_keyfile_load_from_file (keyfile, KEY_FILENAME, GEDA_KEYFILE_NONE, &err))
    {
      if (!err) {
        fprintf(stderr, "FAILED: (KF080601A) NOT_KEY_FILENAME error is NULL\n");
        result++;
      }
      else {
        vmessage("Message: (KF080601B) %s.\n", err->message);
        g_error_free (err);
      }
    }
    else {

      char *comment;

      err = NULL;

      /* === Function 43:  geda_keyfile_get_comment === */

      comment = geda_keyfile_get_comment (keyfile, NULL, NULL, &err);

      if (comment) {
        /* Comment should not have been loaded so error goes against KF0806 */
        fprintf(stderr, "FAILED: (KF080601C) comments without flags <%s>\n", comment);
        result++;
        g_free(comment);
      }
      else {
        /* Error was supplied so is error if error is not set */
        if (!err) {
          fprintf(stderr, "FAILED: (KFKF084301A) NULL NOT_KEY_FILENAME error is NULL\n");
          result++;
        }
        else {
          vmessage("Message: (KF084301B) %s.\n", err->message);
          g_error_free (err);
        }
      }

      err = NULL;

      /* ReLoad the file, without stripping comments */
      if (!geda_keyfile_load_from_file (keyfile, KEY_FILENAME,
                                        GEDA_KEYFILE_KEEP_COMMENTS, &err))
      {
        if (!err) {
          fprintf(stderr, "FAILED: (KF080602A) KEEP_COMMENTS error is NULL\n");
          result++;
        }
        else {
          vmessage("Message: (KF080602B) %s.\n", err->message);
          g_error_free (err);
        }
      }
      else {

        /* Get the top level comments */
        comment = geda_keyfile_get_comment (keyfile, NULL, NULL, NULL);

        if (!comment) {
          /* Comment should have been loaded so error goes against KF0806 */
          fprintf(stderr, "FAILED: (KF080602A) no comments with flags\n");
          result++;
        }
        else {

          /* Verify the comment string is correct */
          if (strcmp(comment, TOP_COMMENTS_STR)) {
            fprintf(stderr, "FAILED: (KF080602B) comments mismatched <%s>\n", comment);
            result++;
          }
          g_free(comment);
        }

        /* Get the G2 Group comments */
        comment = geda_keyfile_get_comment (keyfile, "G2", NULL, NULL);

        if (!comment) {
          /* Comment should have been loaded so error goes against KF0806 */
          fprintf(stderr, "FAILED: (KF080603A) no comments with flags\n");
          result++;
        }
        else {

          /* Verify the comment string is correct */
          if (strcmp(comment, GRP_COMMENTS_STR)) {
            fprintf(stderr, "FAILED: (KF080603B) comments mismatched <%s>\n", comment);
            result++;
          }
          g_free(comment);
        }

        /* Get the T2 Group comments */
        comment = geda_keyfile_get_comment (keyfile, "G2", "T1", NULL);

        if (!comment) {
          /* Comment should have been loaded so error goes against KF0806 */
          fprintf(stderr, "FAILED: (KF080604A) no comments with flags\n");
          result++;
        }
        else {

          /* Verify the comment string is correct */
          if (strcmp(comment, KEY_COMMENTS_STR)) {
            fprintf(stderr, "FAILED: (KF080604B) comments mismatched <%s>\n", comment);
            result++;
          }
          g_free(comment);
        }

        geda_keyfile_remove_comment(keyfile, "G2", "T1", NULL);

        /* Try and et the T2 Group comments */
        comment = geda_keyfile_get_comment (keyfile, "G2", "T1", NULL);

        if (comment) {
          fprintf(stderr, "FAILED: (KF084401) comments not removed <%s>\n", comment);
          g_free(comment);
          result++;
        }
        else {
          vmessage("PASS: (KF084400) geda_keyfile_remove_comment\n");
        }
      }
    }
  }
  else {
    fprintf(stderr, "FAILED: (ERROR) key file not found: <%s>\n", KEY_FILENAME);
    result++;
  }

  geda_keyfile_free(keyfile);

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

  result += check_keyfile_comments();

  result += check_get_set();

  if (result) {
    fprintf(stderr, "Check module geda_keyfile.c");
  }
  else {
    geda_remove_file(KEY_FILENAME);
  }

  return (result > 0);
}
