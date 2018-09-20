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

#include "../../config.h"

#include <libgeda.h>

#include "test_parsecmd.h"

#define TOBJECT "GedaKeyFile"
#define KEY_FILENAME "./test_keyfile.ini"
#define NOT_KEY_FILENAME "./test_Nofile.ini"

/* Comment strings written to and read from KEY_FILENAME */
#define TOP_COMMENTS_STR "Toplevel Comment\nline 2"
#define GRP_COMMENTS_STR "Group Comment"
#define KEY_COMMENTS_STR "Key Comment"

/* Data used to create a dummy ini file for testing */
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
 *      KF0811    geda_keyfile_get_start_group
 *      KF0812    geda_keyfile_get_groups
 *      KF0813    geda_keyfile_get_group_list
 *      KF0814    geda_keyfile_get_keys
 *      KF0816    geda_keyfile_has_group
 *      KF0817    geda_keyfile_has_key
 *      KF0818    geda_keyfile_get_value
 *      KF0819    geda_keyfile_set_value
 *      KF0820    geda_keyfile_get_string
 *      KF0821    geda_keyfile_set_string
 *      KF0822    geda_keyfile_get_locale_string
 *      KF0823    geda_keyfile_set_locale_string
 *      KF0824    geda_keyfile_get_boolean
 *      KF0825    geda_keyfile_set_boolean
 *      KF0826    geda_keyfile_get_integer
 *      KF0827    geda_keyfile_set_integer
 *      KF0828    geda_keyfile_get_int64
 *      KF0829    geda_keyfile_set_int64
 *      KF0830    geda_keyfile_get_uint64
 *      KF0831    geda_keyfile_set_uint64
 *      KF0832    geda_keyfile_get_double
 *      KF0833    geda_keyfile_set_double
 *      KF0834    geda_keyfile_get_string_list
 *      KF0835    geda_keyfile_set_string_list
 *      KF0836    geda_keyfile_get_locale_string_list
 *      KF0837    geda_keyfile_set_locale_string_list
 *      KF0838    geda_keyfile_get_boolean_list
 *      KF0839    geda_keyfile_set_boolean_list
 *      KF0840    geda_keyfile_set_double_list
 *      KF0841    geda_keyfile_get_double_list
 *      KF0842    geda_keyfile_get_integer_list
 *      KF0843    geda_keyfile_set_integer_list
 *      KF0844    geda_keyfile_set_comment
 *      KF0845    geda_keyfile_get_comment
 *      KF0846    geda_keyfile_remove_comment
 *      KF0847    geda_keyfile_remove_key
 *      KF0848    geda_keyfile_remove_group
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
      fprintf(stderr, "FAILED: (KF084401A) set non-existence group comment\n");
      result++;
    }
    else {
      if (!err) {
        fprintf(stderr, "FAILED: (KF084401B) set group comment error is NULL\n");
        result++;
      }
      else {
        vmessage("Message: (KF084401C) %s.\n", err->message);
        g_error_free (err);
      }
    }

    err = NULL;

    /* geda_keyfile_set_key_comment non-existence group */
    if (geda_keyfile_set_comment (keyfile, "NoExist", "BadKey", __func__, &err)) {
      fprintf(stderr, "FAILED: (KF084402A) set non-existence key comment\n");
      result++;
    }
    else {
      if (!err) {
        fprintf(stderr, "FAILED: (KF084402B) set group comment error is NULL\n");
        result++;
      }
      else {
        vmessage("Message: (KF084402C) %s.\n", err->message);
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
    fprintf(stderr, "FAILED: (KF080102) geda_keyfile_new\n");
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

      /* === Function 44:  geda_keyfile_get_comment === */

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
          fprintf(stderr, "FAILED: (KFKF084501A) NULL NOT_KEY_FILENAME error is NULL\n");
          result++;
        }
        else {
          vmessage("Message: (KF084501B) %s.\n", err->message);
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
          fprintf(stderr, "FAILED: (KF084502A) no comments with flags\n");
          result++;
        }
        else {

          /* Verify the comment string is correct */
          if (strcmp(comment, TOP_COMMENTS_STR)) {
            fprintf(stderr, "FAILED: (KF084502B) comments mismatched <%s>\n", comment);
            result++;
          }
          g_free(comment);
        }

        /* Get the G2 Group comments */
        comment = geda_keyfile_get_comment (keyfile, "G2", NULL, NULL);

        if (!comment) {
          fprintf(stderr, "FAILED: (KF084503A) no comments with flags\n");
          result++;
        }
        else {

          /* Verify the comment string is correct */
          if (strcmp(comment, GRP_COMMENTS_STR)) {
            fprintf(stderr, "FAILED: (KF084503B) comments mismatched <%s>\n", comment);
            result++;
          }
          g_free(comment);
        }

        /* Get the T2 Group comments */
        comment = geda_keyfile_get_comment (keyfile, "G2", "T1", NULL);

        if (!comment) {
          fprintf(stderr, "FAILED: (KF084504A) no comments with flags\n");
          result++;
        }
        else {

          /* Verify the comment string is correct */
          if (strcmp(comment, KEY_COMMENTS_STR)) {
            fprintf(stderr, "FAILED: (KF084504B) comments mismatched <%s>\n", comment);
            result++;
          }
          g_free(comment);
        }

        geda_keyfile_remove_comment(keyfile, "G2", "T1", NULL);

        /* Try and et the T2 Group comments */
        comment = geda_keyfile_get_comment (keyfile, "G2", "T1", NULL);

        if (comment) {
          fprintf(stderr, "FAILED: (KF084601) comments not removed <%s>\n", comment);
          g_free(comment);
          result++;
        }
        else {
          vmessage("PASS: (KF084600) geda_keyfile_remove_comment\n");
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

int check_groups (void)
{
  int result = 0;
  GError *err = NULL;

  /* === Function 01: geda_keyfile_new  === */
  GedaKeyFile *keyfile = geda_keyfile_new ();

  if (!GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080103) geda_keyfile_new\n");
    return 1;
  }

  err = NULL;

  /* === Function 06: geda_keyfile_load_from_file === */

  /* Actually Load the file, comments should be stripped */
  if (!geda_keyfile_load_from_file (keyfile, KEY_FILENAME, GEDA_KEYFILE_KEEP_COMMENTS, &err))
  {
    if (!err) {
      fprintf(stderr, "FAILED: (KF080603A) KEYFILE_KEEP_COMMENTS error is NULL\n");
      result++;
    }
    else {
      vmessage("Message: (KF080603B) %s.\n", err->message);
      g_error_free (err);
    }
  }
  else {

    char *start_group;

    /* === Function 11: geda_keyfile_get_start_group === */

    start_group = geda_keyfile_get_start_group(keyfile);

    if (!start_group) {
      fprintf(stderr, "FAILED: (KF081101A) get_start_group\n");
      result++;
    }
    else {
      /* Verify the group string is correct */
      if (strcmp(start_group, "G1")) {
        fprintf(stderr, "FAILED: (KF081101B) group mismatched <%s>\n", start_group);
        result++;
      }
      g_free(start_group);
    }

    unsigned int length;
    char **groups;

    /* === Function 12: geda_keyfile_get_groups === */

    groups = geda_keyfile_get_groups(keyfile, &length);

    if (length != 2) {
      fprintf(stderr, "FAILED: (KF0812101A) get_groups length=%d\n", length);
      result++;
    }
    else if (!groups) {
      fprintf(stderr, "FAILED: (KF0812101B) get_groups\n");
      result++;
    }
    else {

      /* Verify the comment strings are correct */
      if (groups[0] != NULL) {
        if (strcmp(groups[0], "G1")) {
          fprintf(stderr, "FAILED: (KF081201C) group <%s>\n", groups[0]);
          result++;
        }
        g_free(groups[0]);
      }

      if (groups[1] != NULL) {
        if (strcmp(groups[1], "G2")) {
          fprintf(stderr, "FAILED: (KF081201D) group <%s>\n", groups[0]);
          result++;
        }
        g_free(groups[1]);
      }
      g_free(groups);
    }

    GList *group_list;

    /* === Function 13: geda_keyfile_get_groups === */

    group_list = geda_keyfile_get_group_list(keyfile);

    if (!group_list) {
      fprintf(stderr, "FAILED: (KF081301A) get_group_list\n");
      result++;
    }
    else {

      unsigned int list_len = g_list_length(group_list);

      /* Verify the comment strings are correct */
      if (list_len != length) {
        fprintf(stderr, "FAILED: (KF081301B) group list length=%d\n", list_len);
        result++;
      }

      geda_glist_free_all(group_list);
    }

    /* === Function 15: geda_keyfile_has_group === */

    if (!geda_keyfile_has_group(keyfile, "G1")) {
      fprintf(stderr, "FAILED: (KF081601) has_group\n");
      result++;
    }

    /* === Function 47: geda_keyfile_remove_group === */

    if (!geda_keyfile_remove_group(keyfile, "G1", &err)) {
      fprintf(stderr, "FAILED: (KF084801A) remove_group\n");
    }
    else {
      vmessage("Message: (KF084801B) removed group G1\n");
    }

    /* See if the group was removed */
    if (geda_keyfile_has_group(keyfile, "G1")) {
      fprintf(stderr, "FAILED: (KF081602) has_group\n");
      result++;
    }
  }
  geda_keyfile_free(keyfile);
  return result;
}

int check_keys (void)
{
  int result = 0;
  GError *err;

  /* === Function 01: geda_keyfile_new  === */
  GedaKeyFile *keyfile = geda_keyfile_new ();

  if (!GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080104) geda_keyfile_new\n");
    return 1;
  }

  err = NULL;

  /* === Function 06: geda_keyfile_load_from_file === */

  /* Actually Load the file, comments should be stripped */
  if (!geda_keyfile_load_from_file (keyfile, KEY_FILENAME, GEDA_KEYFILE_KEEP_COMMENTS, &err))
  {
    if (!err) {
      fprintf(stderr, "FAILED: (KF080604A) KEYFILE_KEEP_COMMENTS error is NULL\n");
      result++;
    }
    else {
      vmessage("Message: (KF080604B) %s.\n", err->message);
      g_error_free (err);
    }
  }
  else {

    unsigned int length;
    char **keys;

    /* === Function 14: geda_keyfile_get_keys === */

    err = NULL;

    /* Non existent group */
    keys = geda_keyfile_get_keys(keyfile, "G0", &length, &err);

    if (!err) {
      fprintf(stderr, "FAILED: (KF081401A) non-existent, error is NULL\n");
      result++;
    }
    else {
      vmessage("Message: (KF081401A) %s.\n", err->message);
      g_error_free (err);
    }

    /* Group 1 */
    keys = geda_keyfile_get_keys(keyfile, "G1", &length, NULL);

    if (length != 1) {
      fprintf(stderr, "FAILED: (KF0814102A) get_keys length=%d\n", length);
      result++;
    }

    /* Group 2 */
    keys = geda_keyfile_get_keys(keyfile, "G2", &length, NULL);

    if (length != 3) {
      fprintf(stderr, "FAILED: (KF0814103A) get_keys length=%d\n", length);
      result++;
    }
    else if (!keys) {
      fprintf(stderr, "FAILED: (KF0814103B) get_keys\n");
      result++;
    }
    else {

      /* Verify the keys strings are correct in group 2 */
      if (keys[0] != NULL) {
        if (strcmp(keys[0], "T1")) {
          fprintf(stderr, "FAILED: (KF081401C) group <%s>\n", keys[0]);
          result++;
        }
        g_free(keys[0]);
      }

      if (keys[1] != NULL) {
        if (strcmp(keys[1], "T2")) {
          fprintf(stderr, "FAILED: (KF081401D) group <%s>\n", keys[0]);
          result++;
        }
        g_free(keys[1]);
      }

      if (keys[2] != NULL) {
        if (strcmp(keys[2], "T3")) {
          fprintf(stderr, "FAILED: (KF081401E) group <%s>\n", keys[0]);
          result++;
        }
        g_free(keys[2]);
      }

      g_free(keys); /* Free array of pointers */
    }

    /* === Function 16: geda_keyfile_has_key === */

    err = NULL;

    if (!geda_keyfile_has_key(keyfile, "G2", "T1", &err)) {
      fprintf(stderr, "FAILED: (KF081701A) has_key\n");
      result++;
    }
    else {
      /* Found should NOT generate an error */
      if (err) {
        fprintf(stderr, "FAILED: (KF081701B) existent, error NOT NULL\n");
        result++;
        g_error_free (err);
      }
    }

    err = NULL;

    /* Non existent key */
    if (geda_keyfile_has_key(keyfile, "G2", "NOT", &err)) {
      fprintf(stderr, "FAILED: (KF081702A) has_key NOT\n");
      result++;
    }
    else {
      /* Not found should NOT generate an error when group exist */
      if (err) {
        fprintf(stderr, "FAILED: (KF081702B) non-existent, error is NULL\n");
        result++;
        g_error_free (err);
      }
    }

    err = NULL;

    /* Non existent Group */
    if (geda_keyfile_has_key(keyfile, "G6", "NOT", &err)) {
      fprintf(stderr, "FAILED: (KF081703A) has_key group NOT\n");
      result++;
    }
    else {
      /* Should generate an error when group exist */
      if (!err) {
        fprintf(stderr, "FAILED: (KF081703B) non-existent, error is NULL\n");
        result++;

      }
      else {
        vmessage("Message: (KF081703C) %s.\n", err->message);
        g_error_free (err);
      }
    }

    err = NULL;

    /* === Function 16: geda_keyfile_remove_key === */

    /* Try to remove non-existent key*/
    if (geda_keyfile_remove_key(keyfile, "G2", "NOT", &err)) {
      fprintf(stderr, "FAILED: (KF084701A) remove_key NOT\n");
      result++;
    }
    else {
      if (!err) {
        fprintf(stderr, "FAILED: (KF084701B) remove_key NOT error is NULL\n");
        result++;
      }
      else {
        vmessage("Message: (KF084701C) %s.\n", err->message);
        g_error_free (err);
      }
    }

    if (!geda_keyfile_remove_key(keyfile, "G2", "T2", NULL)) {
      fprintf(stderr, "FAILED: (KF084702A) remove_key NOT\n");
      result++;
    }

    /* Verify that the key has been removed */
    if (geda_keyfile_has_key(keyfile, "G2", "T2", NULL)) {
      fprintf(stderr, "FAILED: (KF084702B) has_key\n");
      result++;
    }
  }
  geda_keyfile_free(keyfile);
  return result;
}

int check_data (void)
{
  int result = 0;
  GError *err = NULL;

  /* === Function 01: geda_keyfile_new  === */
  GedaKeyFile *keyfile = geda_keyfile_new ();

  if (!GEDA_IS_KEYFILE(keyfile)) {
    fprintf(stderr, "FAILED: (KF080104) geda_keyfile_new\n");
    return 1;
  }

  err = NULL;

  /* === Function 06: geda_keyfile_load_from_file === */

  /* Actually Load the file, comments should be stripped */
  if (!geda_keyfile_load_from_file (keyfile, KEY_FILENAME, GEDA_KEYFILE_KEEP_COMMENTS, &err))
  {
    if (!err) {
      fprintf(stderr, "FAILED: (KF080604A) KEYFILE_KEEP_COMMENTS error is NULL\n");
      result++;
    }
    else {
      vmessage("Message: (KF080604B) %s.\n", err->message);
      g_error_free (err);
    }
  }
  else {

    /* === Function 18: geda_keyfile_get_value === */

    char *string;

    string = geda_keyfile_get_value (keyfile, "G1", "T1", &err);

    if (!string) {
      fprintf(stderr, "FAILED: (KF081801A) get_value, no value\n");
      result++;
    }
    else if (strcmp(string, "A")) {
      fprintf(stderr, "FAILED: (KF081801B) get_value, value=%s\n", string);
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    /* === Function 19: geda_keyfile_set_value === */

    geda_keyfile_set_value (keyfile,"G1", "T1", "E");

    string = geda_keyfile_get_value (keyfile, "G1", "T1", &err);

    if (!string) {
      fprintf(stderr, "FAILED: (KF081901A) set_value, no value\n");
      result++;
    }
    else if (strcmp(string, "E")) {
      fprintf(stderr, "FAILED: (KF081901B) set_value, value=%s\n", string);
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    /* === Function 20: geda_keyfile_get_string === */

    string = geda_keyfile_get_string(keyfile, "G1", "T1", &err);

    if (!string) {
      fprintf(stderr, "FAILED: (KF082001A) get_string, no value\n");
      result++;
    }
    else if (strcmp(string, "E")) {
      fprintf(stderr, "FAILED: (KF082001B) get_string, value=%s\n", string);
      result++;
    }

    g_free (string);

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    /* === Function 21: geda_keyfile_set_string === */

    geda_keyfile_set_string (keyfile,"G1", "T1", "dog");

    string = geda_keyfile_get_string (keyfile, "G1", "T1", &err);

    if (!string) {
      fprintf(stderr, "FAILED: (KF082101A) set_string, no value\n");
      result++;
    }
    else if (strcmp(string, "dog")) {
      fprintf(stderr, "FAILED: (KF082101B) set_string, value=%s\n", string);
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    /* === Function 22: geda_keyfile_get_locale_string === */

    string = geda_keyfile_get_locale_string (keyfile,"G1", "T1", "en", &err);

    if (!string) {
      fprintf(stderr, "FAILED: (KF082201A) get_locale_string, no value\n");
      result++;
    }
    else if (strcmp(string, "dog")) {
      fprintf(stderr, "FAILED: (KF082201B) get_locale_string, value=%s\n", string);
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    string = geda_keyfile_get_locale_string (keyfile,"G1", "T1", "X1", &err);

    if (!string) {
      fprintf(stderr, "FAILED: (KF082202A) get_locale_string, no value\n");
      result++;
    }
    else if (strcmp(string, "dog")) {
      fprintf(stderr, "FAILED: (KF082202B) get_locale_string, value=%s\n", string);
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    string = geda_keyfile_get_locale_string (keyfile,"G1", "T1", "X1", NULL);

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    if (!string) {
      fprintf(stderr, "FAILED: (KF082203A) get_locale_string, no value\n");
      result++;
    }
    else if (strcmp(string, "dog")) {
      fprintf(stderr, "FAILED: (KF082203B) get_locale_string, value=%s\n", string);
      result++;
    }

    /* === Function 23: geda_keyfile_set_locale_string === */

    geda_keyfile_set_locale_string (keyfile, "G1", "T1", "X1", "bark");
    geda_keyfile_set_locale_string (keyfile, "G1", "T1", "X2", "sniff");
    geda_keyfile_set_locale_string (keyfile, "G1", "T1", "X3", "wag");

    string = geda_keyfile_get_locale_string (keyfile,"G1", "T1", "en", NULL);

    if (!string) {
      fprintf(stderr, "FAILED: (KF082304A) set_locale_string, no value\n");
      result++;
    }
    else if (strcmp(string, "dog")) {
      fprintf(stderr, "FAILED: (KF082304B) set_locale_string, value=%s\n", string);
      result++;
    }

    string = geda_keyfile_get_locale_string (keyfile,"G1", "T1", "X1", NULL);

    if (!string) {
      fprintf(stderr, "FAILED: (KF082205A) set_locale_string, no value\n");
      result++;
    }
    else if (strcmp(string, "bark")) {
      fprintf(stderr, "FAILED: (KF082205B) set_locale_string, value=%s\n", string);
      result++;
    }

    string = geda_keyfile_get_locale_string (keyfile,"G1", "T1", "X2", NULL);

    if (!string) {
      fprintf(stderr, "FAILED: (KF082206A) set_locale_string, no value\n");
      result++;
    }
    else if (strcmp(string, "sniff")) {
      fprintf(stderr, "FAILED: (KF082206B) set_locale_string, value=%s\n", string);
      result++;
    }

    string = geda_keyfile_get_locale_string (keyfile,"G1", "T1", "X3", NULL);

    if (!string) {
      fprintf(stderr, "FAILED: (KF082207A) set_locale_string, no value\n");
      result++;
    }
    else if (strcmp(string, "wag")) {
      fprintf(stderr, "FAILED: (KF082207B) set_locale_string, value=%s\n", string);
      result++;
    }

    /* === Function 25: geda_keyfile_set_boolean === */

    geda_keyfile_set_boolean (keyfile,"G3", "B0", TRUE);

    if (!geda_keyfile_has_group(keyfile, "G3")) {
      fprintf(stderr, "FAILED: (KF082501) set_boolean !G3\n");
      result++;
    }

    /* === Function 24: geda_keyfile_get_boolean === */

    if (geda_keyfile_get_boolean(keyfile, "G3", "NB0", &err)) {
      fprintf(stderr, "FAILED: (KF082401A) get_boolean NB0\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF082401B) get_boolean, no error\n");
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    if (!geda_keyfile_get_boolean(keyfile, "G3", "B0", &err)) {
      fprintf(stderr, "FAILED: (KF082402A) get_boolean, !B0\n");
      result++;
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF082402B) get_boolean, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    /* === Function 27: geda_keyfile_set_integer === */

    geda_keyfile_set_integer (keyfile,"G4", "I0", 125);

    if (!geda_keyfile_has_group(keyfile, "G4")) {
      fprintf(stderr, "FAILED: (KF082701) set_integer, G4\n");
      result++;
    }

    /* === Function 26: geda_keyfile_get_integer === */

    if (geda_keyfile_get_integer(keyfile, "G4", "NI0", &err)) {
      fprintf(stderr, "FAILED: (KF082601A) get_integer NI0\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF082601B) get_integer, no error\n");
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    int G4I0 = geda_keyfile_get_integer(keyfile, "G4", "I0", &err);

    if (G4I0 != 125) {
      fprintf(stderr, "FAILED: (KF082602A) get_integer, !I0\n");
      result++;
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF082602B) get_integer, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    /* === Function 29: geda_keyfile_set_int64 === */

    geda_keyfile_set_int64 (keyfile,"G5", "I64", G_MAXINT64);

    if (!geda_keyfile_has_group(keyfile, "G5")) {
      fprintf(stderr, "FAILED: (KF082901) set_int64, G5\n");
      result++;
    }

    /* === Function 28: geda_keyfile_get_int64 === */

    if (geda_keyfile_get_int64(keyfile, "G5", "NI64", &err)) {
      fprintf(stderr, "FAILED: (KF082801A) get_int64 NI64\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF082801B) get_int64, no error\n");
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    int64 G4I64 = geda_keyfile_get_int64(keyfile, "G5", "I64", &err);

    if (G4I64 != G_MAXINT64) {
      fprintf(stderr, "FAILED: (KF082802A) get_int64, !I64\n");
      result++;
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF082802B) get_int64, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    /* === Function 31: geda_keyfile_set_uint64 === */

    geda_keyfile_set_uint64 (keyfile,"G6", "UI64", -G_MAXINT64);

    if (!geda_keyfile_has_group(keyfile, "G6")) {
      fprintf(stderr, "FAILED: (KF083101) set_uint64, G6\n");
      result++;
    }

    /* === Function 30: geda_keyfile_get_uint64 === */

    if (geda_keyfile_get_uint64(keyfile, "G6", "NUI64", &err)) {
      fprintf(stderr, "FAILED: (KF083001A) get_uint64 NUI64\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF083001B) get_uint64, no error\n");
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    uint64 G4UI64 = geda_keyfile_get_uint64(keyfile, "G6", "UI64", &err);

    if (G4UI64 != -G_MAXINT64) {
      fprintf(stderr, "FAILED: (KF083002A) get_uint64, !UI64\n");
      result++;
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF083002B) get_uint64, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    /* === Function 33: geda_keyfile_set_double === */

    geda_keyfile_set_double (keyfile,"G7", "DBL", 9.9999999989);

    if (!geda_keyfile_has_group(keyfile, "G7")) {
      fprintf(stderr, "FAILED: (KF083301) set_double, G7\n");
      result++;
    }

    /* === Function 32: geda_keyfile_get_double === */

    if (geda_keyfile_get_double(keyfile, "G7", "NDBL", &err)) {
      fprintf(stderr, "FAILED: (KF083201A) get_double NDBL\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF083201B) get_double, no error\n");
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    double G7DBL = geda_keyfile_get_double(keyfile, "G7", "DBL", &err);

    if (G7DBL != 9.9999999989) {
      fprintf(stderr, "FAILED: (KF083202A) get_double, !DBL\n");
      result++;
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF083202B) get_double, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    /* === Function 35: geda_keyfile_set_string_list === */

    static const char *KF35_str[] =
    {
      "A",
      "B",
      "C",
      NULL
    };

    geda_keyfile_set_string_list (keyfile, "G8", "SL", KF35_str, 3);

    if (!geda_keyfile_has_group(keyfile, "G8")) {
      fprintf(stderr, "FAILED: (KF083501) set_string_list, G8\n");
      result++;
    }

    /* === Function 34: geda_keyfile_get_string_list === */

    unsigned int count = 0;

    if (geda_keyfile_get_string_list(keyfile, "G8", "NSL", &count, &err)) {
      fprintf(stderr, "FAILED: (KF083401A) get_string_list NSL\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF083401B) get_string_list, no error\n");
      result++;
    }

    if (count) {
      /* Should be zero since key does not exist */
      fprintf(stderr, "FAILED: (KF083401C) get_string_list, list count\n");
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    char **strings;

    strings = geda_keyfile_get_string_list(keyfile, "G8", "SL", &count, &err);

    if (!strings) {
      fprintf(stderr, "FAILED: (KF083402A) get_string_list, !strings\n");
      result++;
    }

    if (count != 3) {
      fprintf(stderr, "FAILED: (KF083402B) get_string_list, count=%d\n", count);
      result++;
    }
    else {

      char *str;

      str = strings[0];

      if (strcmp(str, "A")) {      /* See structure KF35_str */
        fprintf(stderr, "FAILED: (KF083402C) get_string_list <%s>\n", str);
        result++;
      }

      str = strings[1];

      if (strcmp(str, "B")) {
        fprintf(stderr, "FAILED: (KF083402D) get_string_list <%s>\n", str);
        result++;
      }

      str = strings[2];

      if (strcmp(str, "C")) {
        fprintf(stderr, "FAILED: (KF083402E) get_string_list <%s>\n", str);
        result++;
      }

      g_strfreev(strings);
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF083402F) get_string_list, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    /* === Function 37: geda_keyfile_set_locale_string_list === */

    geda_keyfile_set_locale_string_list (keyfile, "G9", "LSL", "X1", KF35_str, 3);

    if (!geda_keyfile_has_group(keyfile, "G9")) {
      fprintf(stderr, "FAILED: (KF083701) set_locale_string_list, G9\n");
      result++;
    }

    /* === Function 36: geda_keyfile_get_locale_string_list === */

    count = 0;

    if (geda_keyfile_get_locale_string_list(keyfile, "G9", "NLSL", "X1", &count, &err)) {
      fprintf(stderr, "FAILED: (KF083601A) get_locale_string_list NLSL\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF083601B) get_locale_string_list, no error\n");
      result++;
    }

    if (count) {
      /* Should be zero since key does not exist */
      fprintf(stderr, "FAILED: (KF083601C) get_locale_string_list, list count\n");
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    strings = geda_keyfile_get_locale_string_list(keyfile, "G9", "LSL", "X1", &count, &err);

    if (!strings) {
      fprintf(stderr, "FAILED: (KF083602A) get_locale_string_list, !strings\n");
      result++;
    }

    if (count != 3) {
      fprintf(stderr, "FAILED: (KF083602B) get_locale_string_list, count=%d\n", count);
      result++;
    }
    else {

      char *str;

      str = strings[0];

      if (strcmp(str, "A")) {      /* See structure KF35_str */
        fprintf(stderr, "FAILED: (KF083602C) get_locale_string_list <%s>\n", str);
        result++;
      }

      str = strings[1];

      if (strcmp(str, "B")) {
        fprintf(stderr, "FAILED: (KF083602D) get_locale_string_list <%s>\n", str);
        result++;
      }

      str = strings[2];

      if (strcmp(str, "C")) {
        fprintf(stderr, "FAILED: (KF083602E) get_locale_string_list <%s>\n", str);
        result++;
      }
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF083602F) get_locale_string_list, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    /* === Function 39: geda_keyfile_set_boolean_list === */

    bool KF39_bool[]={1,0,0,1,0,0,1};

    geda_keyfile_set_boolean_list (keyfile, "G10", "BL", KF39_bool, 7);

    if (!geda_keyfile_has_group(keyfile, "G10")) {
      fprintf(stderr, "FAILED: (KF083901) set_boolean_list, G10\n");
      result++;
    }

    /* === Function 38: geda_keyfile_get_boolean_list === */

    count = 0;

    if (geda_keyfile_get_boolean_list(keyfile, "G10", "NBL", &count, &err)) {
      fprintf(stderr, "FAILED: (KF083801A) get_boolean_list NLSL\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF083801B) get_boolean_list, no error\n");
      result++;
    }


    if (err) {
      g_error_free (err);
      err = NULL;
    }

    bool *bl;

    bl = geda_keyfile_get_boolean_list(keyfile, "G10", "BL", &count, &err);

    if (!bl) {
      fprintf(stderr, "FAILED: (KF083802A) get_boolean_list, !bl\n");
      result++;
    }

    if (count != 7) {
      fprintf(stderr, "FAILED: (KF083802B) get_boolean_list, count=%d\n", count);
      result++;
    }
    else {


      int i;

      for (i = 0; i < 7; i++) {

        bool value = bl[i];

        if (value != KF39_bool[i]) {      /* See structure KF39_bool */
          fprintf(stderr, "FAILED: (KF083802C) get_boolean_list <%d> != <%d>\n", value, KF39_bool[i]);
          result++;
        }
      }
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF083802F) get_boolean_list, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

#define KEY_LIST_LENGTH 4

    /* === Function 40: geda_keyfile_set_double_list === */

    double KF40_double[]={1.1,2.2,3.3,4.4};

    geda_keyfile_set_double_list (keyfile, "G11", "DL", KF40_double, KEY_LIST_LENGTH);

    if (!geda_keyfile_has_group(keyfile, "G11")) {
      fprintf(stderr, "FAILED: (KF084001) set_double_list, G11\n");
      result++;
    }

    /* === Function 41: geda_keyfile_get_double_list === */

    count = 0;

    if (geda_keyfile_get_boolean_list(keyfile, "G11", "NDL", &count, &err)) {
      fprintf(stderr, "FAILED: (KF084101A) get_double_list NLSL\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF084101B) get_double_list, no error\n");
      result++;
    }

    if (err) {
      g_error_free (err);
      err = NULL;
    }

    double *dl;

    dl = geda_keyfile_get_double_list(keyfile, "G11", "DL", &count, &err);

    if (!dl) {
      fprintf(stderr, "FAILED: (KF084102A) get_double_list, !dl\n");
      result++;
    }

    if (count != KEY_LIST_LENGTH) {
      fprintf(stderr, "FAILED: (KF084102B) get_double_list, count=%d\n", count);
      result++;
    }
    else {


      int i;

      for (i = 0; i < KEY_LIST_LENGTH; i++) {

        double value = dl[i];

        if (value != KF40_double[i]) {      /* See structure KF40_double */
          fprintf(stderr, "FAILED: (KF084102C) get_double_list <%f> != <%f>\n", value, KF40_double[i]);
          result++;
        }
      }
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF084102D) get_double_list, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    /* === Function 43: geda_keyfile_set_integer_list === */

    int KF43_interger[]={1,2,3,4};

    geda_keyfile_set_integer_list (keyfile, "G12", "IL", KF43_interger, KEY_LIST_LENGTH);

    if (!geda_keyfile_has_group(keyfile, "G12")) {
      fprintf(stderr, "FAILED: (KF084301) set_double_list, G12\n");
      result++;
    }

    /* === Function 42: geda_keyfile_get_integer_list === */

    count = 0;

    if (geda_keyfile_get_integer_list(keyfile, "G12", "NIL", &count, &err)) {
      fprintf(stderr, "FAILED: (KF084201A) get_integer_list NLSL\n");
      result++;
    }
    else if (!err) {
      /* Error should be set since key does not exist */
      fprintf(stderr, "FAILED: (KF084201B) get_integer_list, no error\n");
      result++;
    }


    if (err) {
      g_error_free (err);
      err = NULL;
    }

    int *il;

    il = geda_keyfile_get_integer_list(keyfile, "G12", "IL", &count, &err);

    if (!il) {
      fprintf(stderr, "FAILED: (KF084202A) get_integer_list, !il\n");
      result++;
    }

    if (count != KEY_LIST_LENGTH) {
      fprintf(stderr, "FAILED: (KF084202B) get_integer_list, count=%d\n", count);
      result++;
    }
    else {


      int i;

      for (i = 0; i < KEY_LIST_LENGTH; i++) {

        int value = dl[i];

        if (value != KF43_interger[i]) {      /* See structure KF43_interger */
          fprintf(stderr, "FAILED: (KF084202C) get_integer_list <%d> != <%d>\n", value, KF43_interger[i]);
          result++;
        }
      }
    }

    if (err) {
      /* Error should NOT be set since key does exist */
      fprintf(stderr, "FAILED: (KF084202D) get_integer_list, err set\n");
      result++;
      g_error_free (err);
      err = NULL;
    }

    char *data;
    data = geda_keyfile_to_data(keyfile, NULL, NULL);
    g_file_set_contents(KEY_FILENAME, data, -1, NULL);
  }

  geda_keyfile_free(keyfile);
  return result;
}

/** @} endgroup test-geda-keyfile */

int main (int argc, char *argv[])
{
  int result = 0;

  parse_commandline(argc, argv);

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = create_keyfile_data();

  result += check_keyfile_comments();

  result += check_groups();

  result += check_keys();

  result += check_data();

  if (result) {
    fprintf(stderr, "Check module geda_keyfile.c");
  }
  else {
    geda_remove_file(KEY_FILENAME);
  }

  return (result > 0);
}
