/* -*- test_file.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 *  Date Contributed: March, TBD, 2016
 */

//#include <glib.h>
#include <libgeda.h>
#include <stdlib.h>
#include <errno.h>

/*! \file test_file.c
 *  \brief Tests for geda file functions
 *  \par
 *   This is a composite test module that performs tests for all files
 *   in src/file. Tests for each module/file have been organized in
 *   to separate Doxygen groups and this allows indexing using slash-
 *   asterisk-asterick. Doxygen group numbers corresponds to the Module/
 *   File No. in the Test Identifiers:
 *
 *  <DL>
 *    <DT>Group 1 test-file-geda-file    src/file/f_file.c</DT>
 *    <DT>Group 2 test-file-geda-get     src/file/f_get.c</DT>
 *    <DT>Group 3 test-file-geda-path    src/file/f_path.c</DT>
 *    <DT>Group 4 test-file-geda-print   src/file/f_print.c</DT>
 *    <DT>Group 5 test-file-geda-sys     src/file/f_sys.c</DT>
 *  </DL>
 *
 *  Test Identifiers:  F  01  19  03
 *                     ^   ^   ^   ^
 *    group-code ______|   |   |   |
 *                         |   |   |
 *    Module/File No. _____|   |   |
 *                             |   |
 *    Function Number _________|   |
 *                                 |
 *    Tests Number ________________|
 *
 *  See tests/README for more details on the nomenclature for test identifiers.
 */

/** \defgroup test-file-geda-file Test GEDA f_file Module
 * @{
 * \brief Group 1 src/file/f_file.c geda_file_
 */

struct _TestData
{
  char *input;
  char *expected;
};

int test_file (void)
{
  int result = 0;

    /* === Function 01: geda_close_file           f_close === */
    /* === Function 02: geda_file_has_autosave    f_has_active_autosave === */
    /* === Function 03: geda_open_file            f_open === */
    /* === Function 04: geda_open_flags           f_open_flags === */
    /* === Function 05: geda_remove_backup_file   f_remove_backup_file === */
    /* === Function 06: geda_save_file            f_save === */
  return result;
}

/** @} endgroup test-file-geda-file */

/** \defgroup test-file-geda-get Test GEDA f_get Module
 * @{
 * \brief Group 2 src/file/f_get.c geda_file_get_
 */
int test_get (void)
{
  int result = 0;

  return result;
}

/** @} endgroup test-file-geda-get */

/** \defgroup test-file-geda-path Test GEDA f_path Module
 * @{
 * \brief Group 3 src/file/f_path.c geda_file_path_
 */
int test_path (void)
{
  int result = 0;

  return result;
}

/** @} endgroup test-file-geda-path */

/** \defgroup test-file-geda-print Test GEDA f_print Module
 * @{
 * \brief Group 4 src/file/f_print.c geda_file_print_
 */
int test_print (void)
{
  int result = 0;

  return result;
}

/** @} endgroup test-file-geda-print */

/** \defgroup test-file-geda-sys Test GEDA f_sys Module
 * @{
 * \brief Group 5 src/file/f_sys.c geda_file_sys_
 */
int test_sys (void)
{
  int   result;
  char *string;

  result = errno = 0;

  /* === Function 01: geda_copy_file f_sys_copy === */
  /* === Function 02: geda_cmp_file_mod_time f_sys_cmp_mod_time === */
  /* === Function 03: geda_follow_symlinks f_sys_follow_symlinks === */

  /* === Function 04: f_sys_normalize_name === */

  string = geda_normalize_name(NULL, NULL);
  if (string) {
    fprintf(stderr, "FAILED: (F050400) geda_normalize_name <%s>\n", string);
    result++;
  }

  string = geda_normalize_name("./../src", NULL);
  if (string) {
    if (strncmp(string, "/", 1)) {
      fprintf(stderr, "FAILED: (F050401A) geda_normalize_name <%s>\n", string);
      result++;
      string = NULL;
    }
    else {
      if (!strstr(string, "/libgeda/src")) {
        fprintf(stderr, "FAILED: (F050401B) geda_normalize_name <%s>\n", string);
        result++;
        string = NULL;
      }
    }

  }
  else {
    fprintf(stderr, "FAILED: (F050401C) geda_normalize_name NULL\n");
    result++;
    string = NULL;
  }

  if (string) { /* If passed last test continue testing F0504 */

    free(string);

    string = geda_normalize_name("../tests/../src", NULL);
    if (string) {
      if (strncmp(string, "/", 1)) {
        fprintf(stderr, "FAILED: (F050402A) geda_normalize_name <%s>\n", string);
        result++;
      }
      else {
        if (!strstr(string, "/libgeda/src")) {
          fprintf(stderr, "FAILED: (F050402B) geda_normalize_name <%s>\n", string);
          result++;
        }
        if (strstr(string, "..")) {
          fprintf(stderr, "FAILED: (F050402B) geda_normalize_name <%s>\n", string);
          result++;
        }
      }
      free(string);
    }
    else {
      fprintf(stderr, "FAILED: (F050402D) geda_normalize_name NULL\n");
      result++;
    }
  }

  string = geda_normalize_name("../noexist", NULL);
  if (string) {
    fprintf(stderr, "FAILED: (F050403) geda_normalize_name <%s>\n", string);
    result++;
  }

  GError *F0504_err = NULL;

  string = geda_normalize_name("../noexist", &F0504_err);

  if (!F0504_err) {
    fprintf(stderr, "FAILED: (F050404) geda_normalize_name <%s>\n", string);
    result++;
  }
  else {
    g_error_free(F0504_err);
  }

  /* === Function 05: geda_remove_file f_sys_remove === */

  /* === Function 06: f_sys_remove_extension === */

  static const struct _TestData F06_str[] =
  {
    { "",         ""  },
    { "a",        "a" },
    { "a.b",      "a" },
    { "a.b\n",    "a" },
    { "/a.b\r",   "/a" },
    { "a.b.c\r",  "a.b" }
  };

  char *expected;

  string = geda_strdup (F06_str[0].input); /* "" */

  if (geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050601) geda_remove_extension\n");
    result++;
  }
  g_free(string);

  string = geda_strdup (F06_str[1].input); /* "a" */

  if (geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050602) geda_remove_extension\n");
    result++;
  }
  g_free(string);

  string   = geda_strdup (F06_str[2].input); /* "a.b" */
  expected = F06_str[2].expected;

  if (!geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050603A) geda_remove_extension\n");
    result++;
  }
  else if (strcmp(string, expected)) {      /* See structure F06_str */
    fprintf(stderr, "FAILED: (F050603B) geda_remove_extension <%s>\n",string);
    result++;
  }
  g_free(string);

  string   = geda_strdup (F06_str[3].input); /* "a.b\n" */
  expected = F06_str[3].expected;

  if (!geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050604A) geda_remove_extension\n");
    result++;
  }
  else if (strcmp(string, expected)) {      /* See structure F06_str */
    fprintf(stderr, "FAILED: (F050604B) geda_remove_extension <%s>\n",string);
    result++;
  }
  g_free(string);

  string   = geda_strdup (F06_str[4].input); /* "/a.b\r" */
  expected = F06_str[4].expected;

  if (!geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050605A) geda_remove_extension\n");
    result++;
  }
  else if (strcmp(string, expected)) {      /* See structure F06_str */
    fprintf(stderr, "FAILED: (F050605B) geda_remove_extension <%s> expected <%s>\n",string, expected);
    result++;
  }
  g_free(string);

  string   = geda_strdup (F06_str[5].input); /* "a.b.c\r" */
  expected = F06_str[5].expected;

  if (!geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050606A) geda_remove_extension\n");
    result++;
  }
  else if (strcmp(string, expected)) {      /* See structure F06_str */
    fprintf(stderr, "FAILED: (F050606B) geda_remove_extension <%s> expected <%s>\n",string, expected);
    result++;
  }
  g_free(string);

  return result;
}

/** @} endgroup test-file-geda-sys */

int
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = test_file();
  result += test_get();
  result += test_path();
  result += test_print();
  result += test_sys();

  return result;
}
