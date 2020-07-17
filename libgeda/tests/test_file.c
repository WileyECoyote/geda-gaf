/* -*- test_file.c -*-
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
 *  Date Contributed: March, 22nd, 2016
 */

#include "../../config.h"

#include <libgeda.h>
#include <stdlib.h>
#include <errno.h>

#include <geda/geda_stat.h>

#include <glib.h>
#include <glib/gstdio.h>

#include "test_parsecmd.h"
#include "test-suite.h"

/*! \file test_file.c
 *  \brief Tests for geda file functions
 *  \par
 *   This is a composite test module that performs tests for all files
 *   in src/file. Tests for each module/file have been organized in
 *   to separate Doxygen groups and this allows indexing using slash-
 *   asterisk-asterick. Doxygen group numbers correspond to the Module/
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

#define TEST_FILE_PATH "data/"
#define TEST_FILE_PATH2 "../docs"

#define TEST_FILE "logo_256x101.png"
#define SYM_FILE "data/ATMega32-DIP_test.sym"

#define BAD_LINK_FILE "data/link_to_nowhere.sch"
#define GOOD_LINK_FILE "data/link_to_somewhere.sch"
#define READ_ONLY_FILE "data/read_only.sch"

/*! Where the file data/link_to_nowhere.sch points to */
#define LINK2NOWHERE "nowhere/tests/data/no_file.sch"

/*! Where the file data/link_to_somewhere.sch points to */
#define LINK2SOMEWHERE "ATMega32-DIP_test.sym"

struct _TestData
{
  char *input;
  char *expected;
};

bool vpath_build;

/** \defgroup test-file-geda-file Test GEDA f_file Module
 * @{
 * \brief Group 1 src/file/f_file.c geda_file_
 */

int test_file (void)
{
  GedaToplevel *toplevel;
  Page         *page;
  GError       *err;
  char         *cwd_sav;
  char         *cwd;
  char         *source;

  cwd_sav = getcwd(0,0);

  /* This is only needed for distcheck VPATH builds */
  char *src_dir = getenv ("srcdir");

  if (src_dir) {
    source = g_build_filename(src_dir, SYM_FILE, NULL);
  }
  else {
    source = geda_strdup(SYM_FILE);
  }

  toplevel = geda_toplevel_new ();

  geda_toplevel_set_file_open_flags(toplevel, F_OPEN_RESTORE_CWD);
  geda_toplevel_set_make_backups(toplevel, 0);

  int result = 0;

  /* === Function 01:  geda_file_close === */

  geda_close_file(NULL); /* Does absolutely nothing */

  /* === Function 02: geda_file_has_active_autosave === */

  if (geda_file_has_active_autosave(NULL, &err)) {
    fprintf(stderr, "FAILED: (F010200A) file_has_autosave NULL\n");
    result++;
  }

  if (geda_file_has_active_autosave(source, &err)) {
    fprintf(stderr, "FAILED: (F010201) file_has_autosave NULL\n");
    result++;
  }

  char *auto_fname = geda_file_get_autosave_filename (source);

  if (geda_file_copy(source, auto_fname)) {
    fprintf(stderr, "FAILED: (F010202A) error copying file <%s>\n", auto_fname);
    result++;
  }
  else {
    if (!geda_file_has_active_autosave(source, &err)) {
      fprintf(stderr, "FAILED: (F010202B) file_has_autosave <%s>\n", auto_fname);
      result++;
    }
  }

  /* === Function 03: geda_file_open === */

  err = NULL;

  if (geda_open_file(toplevel, NULL, NULL, &err)) {
    fprintf(stderr, "FAILED: (F010300A) geda_file_open NULL\n");
    result++;
  }
  else if (!err) {
    fprintf(stderr, "FAILED: (F010300B) geda_file_open NULL\n");
    result++;
  }
  else {
    g_error_free (err);
  }

  err = NULL;
  page = geda_struct_page_new (toplevel, NULL);

  if (geda_open_file(toplevel, page, NULL, &err)) {
    fprintf(stderr, "FAILED: (F010300C) geda_file_open NULL\n");
    result++;
  }
  else if (!err) {
    fprintf(stderr, "FAILED: (F010300D) geda_file_open NULL\n");
    result++;
  }
  else {
    g_error_free (err);
  }

  err = NULL;
  if (geda_open_file(toplevel, page, "nonexistence", &err)) {
    fprintf(stderr, "FAILED: (F010301A) geda_file_open NULL\n");
    result++;
  }
  else if (!err) {
    fprintf(stderr, "FAILED: (F010301B) geda_file_open NULL\n");
    result++;
  }
  else {
    g_error_free (err);
  }

  err = NULL;
  if (!geda_open_file(toplevel, page, source, &err)) {
    fprintf(stderr, "FAILED: (F010302A) geda_file_open %s\n", source);
    result++;
  }
  else if (err) {
    fprintf(stderr, "FAILED: (F010302B) geda_file_open %s\n", source);
    g_error_free (err);
  }

  cwd = getcwd(0,0);

  if (cwd && strcmp(cwd, cwd_sav) != 0) {
    fprintf(stderr, "FAILED: (F010302C) geda_file_open <%s>!=<%s>\n", cwd, cwd_sav);
    result++;
  }
  free(cwd);

  geda_struct_page_delete (toplevel, page, FALSE);

  cwd = getcwd(0,0);

  if (cwd && strcmp(cwd, cwd_sav) != 0) {
    fprintf(stderr, "FAILED: (S12TBD01) geda_file_open <%s>!=<%s>\n", cwd, cwd_sav);
    result++;
  }
  free(cwd);

  /* === Function 04: geda_file_open_flags === */

  unsigned int flags;

  flags = geda_open_flags(toplevel);

  if (flags ^ F_OPEN_RESTORE_CWD) {
    fprintf(stderr, "FAILED: (F010401) geda_file_open_flags <%u>\n", flags);
    result++;
  }

  geda_toplevel_set_file_open_flags(toplevel, F_OPEN_NONE);

  flags = geda_open_flags(toplevel);

  if (flags) {
    fprintf(stderr, "FAILED: (F010402) geda_file_open_flags <%u>\n", flags);
    result++;
  }

  /* === Function 05: geda_file_remove_backup === */

  geda_remove_backup_file(source);

  if (geda_file_has_active_autosave(source, &err)) {
    fprintf(stderr, "FAILED: (F010501) remove_backup_file %s\n", auto_fname);
    result++;
  }

  geda_file_sys_remove(auto_fname);
  GEDA_FREE(auto_fname);

  geda_remove_backup_file(NULL);

  err = NULL;

  /* === Function 06: geda_file_save === */

  if (access(READ_ONLY_FILE, F_OK) == 0)  {
    g_chmod(READ_ONLY_FILE, S_IRUSR|S_IRGRP|S_IROTH);
  }

  geda_save_file(NULL, NULL, NULL, &err);

  if (!err) {
    fprintf(stderr, "FAILED: (F010600A1) geda_file_save NULL\n");
    result++;
  }
  else {
    vmessage("Message: (F010600A2) %s.\n", err->message);
    g_error_free (err);
    err = NULL;
  }

  geda_save_file(NULL, NULL, READ_ONLY_FILE, &err);

  if (!err) {
    fprintf(stderr, "FAILED: (F010600B1) geda_file_save NULL\n");
    result++;
  }
  else {
    vmessage("Message: (F010600B2) %s.\n", err->message);
    g_error_free (err);
    err = NULL;
  }

  geda_save_file(NULL, NULL, "data/bad_geda.sch", &err);

  if (!err) {
    fprintf(stderr, "FAILED: (F010600C1) geda_file_save NULL\n");
    result++;
  }
  else {
    vmessage("Message: (F010600C2) %s.\n", err->message);
    g_error_free (err);
    err = NULL;
  }

  free(source);

  const char *fname06 = "data/test.sch";

  if (src_dir) {
    source = g_build_filename(src_dir, fname06, NULL);
  }
  else {
    source = geda_strdup(fname06);
  }

  page = geda_struct_page_new (toplevel, source);

  geda_save_file(NULL, page, source, &err);

  if (err) {
    fprintf(stderr, "FAILED: (F010601A) geda_file_save\n");
    g_error_free (err);
    err = NULL;
    result++;
  }
  else {
    if (access(source, R_OK))  {
      fprintf(stderr, "FAILED: (F010601B) %s not found\n", source);
      result++;
    }
    else {
      /* File containing only version should have been written to disk */
      geda_remove_file(source);
    }
  }

  /* Try to over-write a read only file, note that distcheck fail
   * because there is no data subdirectory */
  geda_save_file(toplevel, page, READ_ONLY_FILE, &err);

  if (!err) {
    fprintf(stderr, "FAILED: (F010602A) <%s>\n", READ_ONLY_FILE);
    result++;
  }
  else {
    vmessage("Message: (F010602B) %s.\n", err->message);
    g_error_free (err);
    err = NULL;
  }

  geda_struct_page_delete (toplevel, page, FALSE);

  geda_toplevel_set_file_open_flags(toplevel, F_OPEN_NONE);

  g_object_unref(toplevel);

  /* ensure directory is restored, regardless of what happened above */
  if (!chdir(cwd_sav));
  free(cwd_sav);
  free(source);

  return result;
}

/** @} endgroup test-file-geda-file */

/** \defgroup test-file-geda-get Test GEDA f_get Module
 * @{
 * \brief Group 2 src/file/f_get.c geda_file_get_
 */

int test_get (void)
{
  char *src_dir;
  char *string;
  int   index;
  int   result = 0;

  src_dir = getenv ("srcdir");

  /* === Function 01:  geda_file_get_autosave_filename === */

  /* LINK2SOMEWHERE is SYM_FILE without  the data/ prefix */
  string = geda_get_autosave_name(LINK2SOMEWHERE);

  if (!string) {
    fprintf(stderr, "FAILED: (F020101A) geda_file_get_autosave_filename\n");
    result++;
  }
  else {

    const char *expected_0101 = "." DIR_SEPARATOR_S "#" LINK2SOMEWHERE "#";

    if (strcmp(string, expected_0101)) {
      fprintf(stderr, "FAILED: (F020101B) geda_file_get_autosave_filename <%s>\n", string);
      result++;
    }

    free (string);
  }

  string = geda_get_autosave_name(LINK2NOWHERE);

  if (!string) {
    fprintf(stderr, "FAILED: (F020102A) geda_file_get_autosave_filename\n");
    result++;
  }
  else {

    const char *expected_0102 = "nowhere" DIR_SEPARATOR_S "tests" DIR_SEPARATOR_S "data" DIR_SEPARATOR_S "#no_file.sch#";

    if (strcmp(string, expected_0102)) {
      fprintf(stderr, "FAILED: (F020102B) geda_file_get_autosave_filename <%s>\n", string);
      result++;
    }

    free (string);
  }

  /* === Function 02: geda_file_get_basename === */

  static const struct _TestData F02_str[] =
  {
    { "",         ""    },
    { "a",        "a"   },
    { "a\n",      "a\n" },
    { "a\r",      "a\r" },
    { "a/b",      "b"   },
    { "a/b.c",    "b.c" },
    { "/a/b.c",   "b.c" },
    { "/a/b/c",   "c"   },
    { "/a/b/c.d", "c.d" },
    { 0 },
  };

  string = (char*)geda_get_basename (NULL);

  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (F020200) geda_file_get_basename <%s>\n", string);
    result++;
  }

  for (index = 0; F02_str[index].input; index++) {

    char *expected = F02_str[index].expected;
    char *input    = geda_strdup (F02_str[index].input);

    string = (char*)geda_get_basename (input);

    if (string) {
      if (strcmp(string, expected)) {      /* See structure F02_str */
        fprintf(stderr, "FAILED: (F020201A-%d) geda_file_get_basename <%s>\n",index, string);
        result++;
      }
      free (input);
    }
    else {
      fprintf(stderr, "FAILED: (F020201B-%d) expected <%s> NULL\n",index, expected);
    }
    string = NULL;
  }

  /* === Function 03: geda_file_get_basename_dup === */

  string = geda_get_basename_dup(F02_str[8].input);
  if (string) {
    if (strcmp(string, F02_str[8].expected)) {      /* See structure F02_str */
        fprintf(stderr, "FAILED: (F020301A) geda_file_get_basename_dup <%s>\n", string);
        result++;
    }
    free (string);
  }
    else {
      fprintf(stderr, "FAILED: (F020301B) expected <%s> got NULL\n", F02_str[8].expected);
      result++;
  }

  /* === Function 04: geda_get_bitmap_spec       === */

  string = geda_file_get_bitmap_filespec(NULL);

  if (string) {
    fprintf(stderr, "FAILED: (F020400) string <%s>\n", string);
    result++;
  }

  string = geda_file_get_bitmap_filespec("do_not_exist.png");

  if (string) {
    fprintf(stderr, "FAILED: (F020401) string <%s>\n", string);
    result++;
  }

  string = geda_file_get_bitmap_filespec("close_box.png");

  if (!string) {
    fprintf(stderr, "FAILED: (F020402) string NULL\n");
    result++;
  }

  /* === Function 05: geda_file_get_contents === */

  GError *err;
  char   *filename_05;
  char   *buffer_05;
  size_t  n_byte;

  err = NULL;

  if (geda_get_file_contents(NULL, NULL, &n_byte, &err)) {
    fprintf(stderr, "FAILED: (F020500A) geda_file_get_contents NULL\n");
    result++;
  }
  else {
    if (!err) {
      fprintf(stderr, "FAILED: (F020500B1) geda_file_get_contents NULL\n");
      result++;
    }
    else {
      vmessage("Message: (F020500B2) %s.\n", err->message);
      g_error_free (err);
      err = NULL;
    }
  }

  if (src_dir) {
    filename_05 = g_build_filename(src_dir, READ_ONLY_FILE, NULL);
  }
  else {
    filename_05 = geda_strdup(READ_ONLY_FILE);
  }

  if (geda_get_file_contents(filename_05, &buffer_05, &n_byte, NULL)) {

/* Account for the extra CR in the Win32 file */
#if defined (OS_WIN32_NATIVE)
#define BYTE_COUNT_05 14
#else
#define BYTE_COUNT_05 13
#endif

    if (n_byte != BYTE_COUNT_05) {
      fprintf(stderr, "FAILED: (F020501A) n_byte <%zu>\n", n_byte);
      result++;
    }
    free (filename_05);
    free (buffer_05);
  }
  else {
    fprintf(stderr, "FAILED: (F020501B) geda_file_get_contents\n");
    result++;
  }

  if (geda_get_file_contents(BAD_LINK_FILE, &buffer_05, &n_byte, &err)) {
    fprintf(stderr, "FAILED: (F020502A) geda_file_get_contents NULL\n");
    result++;
  }
  else {
    if (!err) {
      fprintf(stderr, "FAILED: (F020502B1) geda_file_get_contents NULL\n");
      result++;
    }
    else {
      vmessage("Message: (F020502B2) %s.\n", err->message);
      g_error_free (err);
      err = NULL;
    }
  }

  /* === Function 06: geda_file_get_data_filespec === */

  char *filename_06;

  if (geda_get_data_spec (NULL)) {
    fprintf(stderr, "FAILED: (F020600) geda_get_data_spec NULL\n");
    result++;
  }

  filename_06 = geda_get_data_spec (__FILE__);

  if (!filename_06) {
    fprintf(stderr, "FAILED: (F020601A) geda_get_data_spec __FILE__\n");
    result++;
  }
  else {

    char *cwd;

    cwd = getcwd(0,0);

    if (!strstr(filename_06, cwd)) { /* check path was included */
      fprintf(stderr, "FAILED: (F020601B) geda_get_data_spec %s\n", filename_06);
      result++;
    }

    free(cwd);

    if (!strstr(filename_06, __FILE__)) { /* check file is included */
      fprintf(stderr, "FAILED: (F020601C) geda_get_data_spec %s\n", filename_06);
      result++;
    }
    free(filename_06);
  }

  /* GEDADATA = top of source */
  filename_06 = geda_get_data_spec ("config.h");

  if (!filename_06) {
    fprintf(stderr, "FAILED: (F020602A) geda_get_data_spec __FILE__\n");
    result++;
  }
  else {
    if (!strstr(filename_06, "config.h")) {
      fprintf(stderr, "FAILED: (F020602B) geda_get_data_spec %s\n", filename_06);
      result++;
    }
    else {
      /* Above check allowed for the path seperate per platform, the real
       * requirement is that filename_06 points to the file */
      if (!g_file_test (filename_06, G_FILE_TEST_EXISTS)) {
        fprintf(stderr, "FAILED: (F020602C) geda_get_data_spec %s\n", filename_06);
        result++;
      }
    }

    free(filename_06);
  }

  filename_06 = geda_get_data_spec (LINK2NOWHERE);

  if (filename_06) {
    fprintf(stderr, "FAILED: (F020603) geda_get_data_spec %s\n", filename_06);
    result++;
  }

  /* === Function 07: geda_file_get_dir_list_files === */

  GSList *files;

  if (geda_get_dir_list(NULL, NULL, NULL)) {
    fprintf(stderr, "FAILED: (F020700A) geda_get_dir_list NULL\n");
    result++;
  }

  if (geda_get_dir_list(NULL, NULL, &err)) {
    fprintf(stderr, "FAILED: (F020700B) geda_get_dir_list NULL\n");
    result++;
  }
  else {
    if (!err) {
      fprintf(stderr, "FAILED: (F020700C1) geda_get_dir_list err\n");
      result++;
    }
    else {
      vmessage("Message: (F020700C2) %s.\n", err->message);
      g_error_free (err);
      err = NULL;
    }
  }

  if (!vpath_build) {

#if defined (OS_WIN32_NATIVE)
#define FILE_COUNT_07 5
#else
#define FILE_COUNT_07 7
#endif

    files = geda_get_dir_list(TEST_FILE_PATH, NULL, NULL);

    int num_file_07 = g_slist_length(files);

    /* current, parent, files and tmp test links = 7 */
    if (num_file_07 != FILE_COUNT_07) {
      fprintf(stderr, "FAILED: (F020701) geda_get_dir_list <%d>\n", num_file_07);
      result++;
    }

    geda_gslist_free_all(files);

    files = geda_get_dir_list(TEST_FILE_PATH, ".sym", NULL);

    num_file_07 = g_slist_length(files);

    /* ATMega32-DIP_test.sym is the sym file in data/ */
    if (num_file_07 - 1) {
      fprintf(stderr, "FAILED: (F020702A) geda_get_dir_list <%d>\n", num_file_07);
      result++;
    }
    else {

      string = files->data;

      if (strcmp(string, LINK2SOMEWHERE)) {
        fprintf(stderr, "FAILED: (F020702B) geda_get_dir_list <%s>\n", string);
        result++;
      }
      geda_gslist_free_all(files);
    }
  }

  /* === Function 08: geda_file_get_filename_ext === */

  static const struct _TestData F08_str[] =
  {
    { "",            ""     }, /* Emtpy */
    { ".",           ""     }, /* Current Directory */
    { "noextension", ""     }, /* Extensionless */
    { "with.2.dots", "dots" }, /* with 2 DOTS */
    { "a.png",       "png"  }, /* a png file */
    { "a/b.sym",     "sym"  }, /* a symbol named b in a */
    { "c.d/e.sch" ,  "sch"  }, /* a schematic named e in c.d */
    { "/a/b.c",      "c"    }, /* with leading separator */
    { "./a/b/c.d",   "d"    }, /* hidden directory */
    { ".hidden",     ""     }, /* hidden file */
    { "/a/b/.f",     ""     }, /* hidden file in nested dir */
    { 0 },
  };

  const char *ext = geda_get_extension (NULL);

  if (ext) {                           /* NULL input */
    fprintf(stderr, "FAILED: (F080200) geda_get_extension <%s>\n", ext);
    result++;
  }

  for (index = 0; F08_str[index].input; index++) {

    char *expected = F08_str[index].expected;
    char *input    = geda_strdup (F08_str[index].input);

    ext = geda_get_extension (input);

    if (ext) {
      if (strcmp(ext, expected)) {      /* See structure F08_str */
        fprintf(stderr, "FAILED: (F080201A-%d) geda_get_extension <%s>\n",index, ext);
        result++;
      }
      free (input);
    }
    else {
      if (*expected) {      /* See structure F08_str */
        fprintf(stderr, "FAILED: (F080201B-%d) expected <%s> NULL\n",index, expected);
        result++;
      }
    }
    ext = NULL;
  }


  /* === Function 09: geda_get_format_header    geda_file_get_format_header === */

  const char *header;

  header = geda_file_get_format_header();

  if (!header) {
    fprintf(stderr, "FAILED: (F020901) header is NULL\n");
    result++;
  }

  /* === Function 10: geda_file_get_name === */

  static const struct _TestData filename_10[] =
  {
    { "",  ""                      }, /* empty string */
    { "Rainbow",         "Rainbow" },
    { "Rainbow.sch",     "Rainbow" },
    { "Over/The/Rainbow","Rainbow" },
    { "Somewhere/Over/The/Rainbow.sym", "Rainbow"}
  };

  if (geda_get_file_name(NULL)) {                              /* NULL input */
    fprintf(stderr, "FAILED: (F021000) NULL file_get_name\n");
    result++;
  }

  int count = sizeof(filename_10) / sizeof(struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = filename_10[index].expected;
    char *input    = filename_10[index].input;

    string = geda_get_file_name (input);

    if (string) {
      if (strcmp(string, expected)) {      /* See structure U02_str */
        fprintf(stderr, "FAILED: (F021001-%d) file_get_name <%s>\n",index, string);
        result++;
      }
    }
    else {
      fprintf(stderr, "FAILED: (F021001B-%d) expected <%s> NULL\n",index, expected);
      result++;
    }
  }


  /* === Function 11: geda_file_get_is_path_absolute === */

  if (geda_is_path_absolute(NULL)) {                           /* NULL input */
    fprintf(stderr, "FAILED: (F021100) NULL path_absolute\n");
    result++;
  }

  if (geda_is_path_absolute("")) {                            /* Empty input */
    fprintf(stderr, "FAILED: (F021101) Empty path_absolute\n");
    result++;
  }

  if (geda_is_path_absolute("../")) {                        /* Relative input */
    fprintf(stderr, "FAILED: (F021102) parent path_absolute\n");
    result++;
  }

  if (geda_is_path_absolute("../src")) {                      /* Relative input */
    fprintf(stderr, "FAILED: (F021103) src path_absolute\n");
    result++;
  }

  if (geda_is_path_absolute("data/")) {                      /* subdirectory input */
    fprintf(stderr, "FAILED: (F021103) src path_absolute\n");
    result++;
  }

  if (geda_is_path_absolute("data/../src")) {                /* Relative input */
    fprintf(stderr, "FAILED: (F021104) src path_absolute\n");
    result++;
  }

  char *cwd;

  cwd = getcwd(0,0);

  if (!geda_is_path_absolute(cwd)) {                         /* absolute path */
    fprintf(stderr, "FAILED: (F021105) src path_absolute\n");
    result++;
  }

  char *this_file_06;

  this_file_06 = g_build_filename(cwd, __FILE__, NULL);

  if (!geda_is_path_absolute(this_file_06)) {                /* absolute file */
    fprintf(stderr, "FAILED: (F021106) src path_absolute\n");
    result++;
  }

  free(this_file_06);
  free(cwd);

  return result;
}

/** @} endgroup test-file-geda-get */

/** \defgroup test-file-geda-path Test GEDA f_path Module
 * @{
 * \brief Group 3 src/file/f_path.c geda_file_path_
 */

static void remove_one(const char *src_dir)
{
  char *path;

  if (src_dir) {
    path = g_build_filename(src_dir, "one", NULL);
  }
  else {
    path = geda_strdup("./one");
  }

  char *command = geda_strconcat("rm -rf ", path, NULL);

  if (system(command));

  free(command);
  free(path);
}

int test_path (void)
{
  int result = 0;

  char *cwd_sav;
  char *path;

  cwd_sav = getcwd(0,0);

  /* This is only needed for distcheck VPATH builds */
  char *src_dir = getenv ("srcdir");

  /* === Function 01: geda_file_path_create === */

  if (!geda_create_path(NULL, S_IRWXU | S_IRWXG | S_IRWXO)) {
    fprintf(stderr, "FAILED: (F030100) geda_file_path_create\n");
    result++;
  }

  if (src_dir && strlen(src_dir) > 1) { /* VPATH builds */
    path = geda_strdup("./one");
  }
  else {
    path = g_build_filename(src_dir, "one", NULL);
  }

  if (geda_create_path(path, S_IRWXU | S_IRWXG | S_IRWXO)) {
    fprintf(stderr, "FAILED: (F030101A) geda_file_path_create\n");
    result++;
  }
  else if (!g_file_test (path, G_FILE_TEST_IS_DIR)) {
    fprintf(stderr, "FAILED: (F030101B) geda_file_path_create\n");
    result++;
  }

  free(path);

  remove_one(src_dir);

  if (src_dir && strlen(src_dir) > 1) { /* VPATH builds */
    path = geda_strdup("./one/two/three");
  }
  else {
    path = g_build_filename(src_dir, "one/two/three", NULL);
  }

  if (geda_create_path(path, S_IRWXU | S_IRWXG | S_IRWXO)) {
    fprintf(stderr, "FAILED: (F030102A) geda_file_path_create\n");
    result++;
  }
  else if (!g_file_test (path, G_FILE_TEST_IS_DIR)) {
    fprintf(stderr, "FAILED: (F030102B) geda_file_path_create\n");
    result++;
  }

  free(path);

  remove_one(src_dir);

  /* === Function 02: geda_free_path geda_file_path_free === */

    /* Deferred */

  /* === Function 03: geda_get_dirname geda_file_path_get_dirname === */

  static const struct _TestData F03_str[] =
  {
    { "",                        "."   },
    { "./here",                  "."   },

#if defined (OS_WIN32_NATIVE)
    { "./here/there",            ".\\here"  },
    { "./here/there/everywhere", ".\\here\\there" },
#else
    { "./here/there",            "./here"  },
    { "./here/there/everywhere", "./here/there" },
#endif

  };

  int count;
  int index;

  count = sizeof(F03_str) / sizeof(struct _TestData);

  path = geda_get_dirname(NULL);
  if (path) {                           /* NULL input */
    fprintf(stderr, "FAILED: (F030300) geda_file_path_get_dirname <%s>\n", path);
    result++;
  }

  for (index = 0; index < count; index++) {

    char *expected = F03_str[index].expected;
    char *input    = F03_str[index].input;

    path = geda_get_dirname (input);

    if (path) {
      if (strcmp(path, expected)) {      /* See structure F03_str */
        fprintf(stderr, "FAILED: (F030301A-%d) geda_file_path_get_dirname <%s>\n",index, path);
        result++;
      }
    }
    else {
      fprintf(stderr, "FAILED: (F030301B-%d) geda_file_path_get_dirname NULL\n",index);
      result++;
    }
  }

  /* === Function 04: geda_sys_data_path geda_file_path_sys_data === */

  path = (char*) geda_sys_data_path();

  if (path == NULL) {
    fprintf(stderr, "FAILED: (F030401A) geda_file_path_sys_data\n");
    result++;
  }
  else {

    if (!g_file_test (path, G_FILE_TEST_IS_DIR)) {
      fprintf(stderr, "FAILED: (F030401B) geda_file_path_sys_data <%s>\n", path);
      result++;
    }
  }

  /* === Function 05: geda_file_path_sys_doc === */

  path = (char*)geda_sys_doc_path();

  if (path == NULL) {
    fprintf(stderr, "FAILED: (F030501A) geda_file_path_sys_doc NULL\n");
    result++;
  }
  else {

    if (!strstr(path, "share/doc/geda-gaf")) {
      fprintf(stderr, "FAILED: (F030501B) geda_file_path_sys_doc <%s>\n", path);
      result++;
    }
  }

  /* === Function 06: geda_file_path_sys_config === */

  path = (char*)geda_sys_config_path();

  if (path == NULL) {
    fprintf(stderr, "FAILED: (F030601A) geda_file_path_sys_config NULL\n");
    result++;
  }
  else {

    /* The following check is intentionally laxed to allow for different directory
     * seperators, the else if clause will verify the existence */
    if (!strstr(path, "libgeda")) {
      fprintf(stderr, "FAILED: (F030601B) geda_file_path_sys_config <%s>\n", path);
      result++;
    }
    else if (!g_file_test (path, G_FILE_TEST_IS_DIR)) {
      fprintf(stderr, "FAILED: (F030601C) geda_file_path_sys_config <%s>\n", path);
      result++;
    }
  }

  /* === Function 07: geda_file_path_user_config === */

  path = (char*)geda_user_config_path();

  if (path == NULL) {
    fprintf(stderr, "FAILED: (F030701A) geda_file_path_user_config NULL\n");
    result++;
  }
  else {

    char *config_dir = g_build_filename (USER_CONFIG_DIR, NULL);

    if (!strstr(path, config_dir)) {
      fprintf(stderr, "FAILED: (F030701B) geda_file_path_user_config <%s>\n", path);
      result++;
    }
    g_free(config_dir);
  }

  /* === Function 08: geda_file_path_user_cache === */

  path = (char*)geda_user_cache_path();

  if (path == NULL) {
    fprintf(stderr, "FAILED: (F030801A) geda_file_path_user_cache NULL\n");
    result++;
  }
  else {
    if (!strstr(path, STD_USER_CACHE_DIR)) {
      fprintf(stderr, "FAILED: (F030801B) geda_file_path_user_cache <%s>\n", path);
      result++;
    }
  }

  /* Ensure directory is restored, regardless of what happened above */

  geda_file_path_free();

  if (!chdir(cwd_sav));
  free(cwd_sav);

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
 *
 *  <DL>
 *    <DT>Function 01 geda_file_copy</DT>
 *    <DT>Function 02 geda_file_sys_ckmod</DT>
 *    <DT>Function 03 geda_file_sys_cmp_mod_time</DT>
 *    <DT>Function 04 geda_file_sys_follow_symlinks</DT>
 *    <DT>Function 05 geda_file_sys_normalize_name</DT>
 *    <DT>Function 06 geda_file_sys_remove</DT>
 *    <DT>Function 07 geda_file_sys_remove_extension</DT>
 *  </DL>
 */

static int remove_file = 0;

int test_geda_file_copy ()
{
  int result = 0;

  char *src_dir = getenv ("srcdir");

  if (src_dir) {

    char *source;

    source = g_build_filename(src_dir, TEST_FILE_PATH2, TEST_FILE, NULL);

    if (access(source, R_OK) == 0) {

      /* Should be copied to the current, aka tests, directory*/
      result = geda_copy_file(source, TEST_FILE);

      if (access(TEST_FILE, R_OK) != 0) {
        fprintf(stderr, "FAILED: (F050101) geda_copy_file <%s>\n", TEST_FILE);
        result++;
      }
      else {
        remove_file = 1;
      }
    }
    else {
      fprintf(stderr, "Could not access <%s>\n", source);
      result++;
    }
    g_free (source);

  }
  else {
    fprintf(stderr, "%s: src_dir is NULL, ignoring failure.\n", __func__);
  }

  return result;
}

int test_sys (void)
{
  char   *string;
  int     result;

  result = errno = 0;

  /* This is only needed for distcheck VPATH builds */
  const char *src_dir = getenv ("srcdir");

  /* === Function 01: geda_file_copy === */

      /* See also test_picture_object.c geda_file_copy() */

      result = test_geda_file_copy(); /* test performed in subfunction */

  /* === Function 02: geda_file_sys_ckmod === */

  /* === Function 03: geda_cmp_file_mod_time geda_file_sys_cmp_mod_time === */

  time_t now;
  int secs;

  secs = geda_file_sys_cmp_mod_time(NULL, time(&now));

  if (++secs) {
    fprintf(stderr, "FAILED: (F050300) geda_file_sys_cmp_mod_time secs <%d>\n", secs);
    result++;
  }

  secs = geda_file_sys_cmp_mod_time("nonexistence", time(&now));

  if (++secs) {
    fprintf(stderr, "FAILED: (F050301) geda_file_sys_cmp_mod_time secs <%d>\n", secs);
    result++;
  }

  struct stat stat_03;

  errno = 0;

  /* Check if VPATH builds */
  if (src_dir) {
    string = g_build_filename(src_dir, SYM_FILE, NULL);
  }
  else {
    string = geda_strdup(SYM_FILE);
  }

  if (stat (string, &stat_03) != 0) {

    if (errno == ENOENT) {
      /* The file does not exist. */
      fprintf(stderr, "FAILED: (F050302A) test file <%s> is missing\n", string);
      result++;
    }
    else {
      fprintf(stderr, "FAILED: (F050302B) an error occurred, file <%s>: %s\n", string, strerror(errno));
      result++;
    }
  }
  else {

    secs = geda_file_sys_cmp_mod_time(string, time(&now));

    if (secs != difftime (time(&now), stat_03.st_mtime)) {
      fprintf(stderr, "FAILED: (F050302C) geda_file_sys_cmp_mod_time secs <%d>\n", secs);
      result++;
    }
  }

  GEDA_FREE(string);

  /* === Function 04: geda_file_sys_follow_symlinks === */

  /* MinGW does not have symlinks */
#ifndef __MINGW32__

  GError *err = NULL;

  string = geda_follow_symlinks(NULL, &err);
  if (string) {
    fprintf(stderr, "FAILED: (F050400A) geda_file_sys_follow_symlinks NULL\n");
    result++;
  }
  else {
    if (!err) {
      fprintf(stderr, "FAILED: (F050400B) geda_file_sys_follow_symlinks NULL\n");
      result++;
    }
    else {
      g_error_free (err);
    }
  }

  string = geda_follow_symlinks(BAD_LINK_FILE, &err);
  if (!string) {
    if (!err) {
      fprintf(stderr, "FAILED: (F050401A) geda_file_sys_follow_symlinks <%s>\n", strerror(errno));
    }
    else {
      fprintf(stderr, "FAILED: (F050401B) geda_file_sys_follow_symlinks <%s>\n", err->message);
      g_error_free (err);
    }
    result++;
  }
  else {

    /* Because geda_file_sys_follow_symlinks was given a relative path
     * that contained a relative link, the returned "target" should be
     * relative to the current directory.
     */
    char *target;

    if (src_dir && strlen(src_dir) > 1) { /* VPATH builds */
      target = geda_strdup(BAD_LINK_FILE);
    }
    else {
      target = g_build_filename("data", LINK2NOWHERE, NULL);
    }

    if (strcmp(string, target)) {
      fprintf(stderr, "FAILED: (F050401C) geda_file_sys_follow_symlinks <%s>", string);
      fprintf(stderr, " expected <%s>\n",target);
      result++;
    }

    GEDA_FREE(target); /* string returned from g_build_filename */
    GEDA_FREE(string); /* string returned from geda_follow_symlinks */
  }

  string = geda_follow_symlinks(GOOD_LINK_FILE, &err);
  if (!string) {
    if (!err) {
      fprintf(stderr, "FAILED: (F050402A) geda_file_sys_follow_symlinks <%s>\n", strerror(errno));
    }
    else {
      fprintf(stderr, "FAILED: (F050402B) geda_file_sys_follow_symlinks <%s>\n", err->message);
      g_error_free (err);
    }
    result++;
  }
  else {

    char *target;

    if (src_dir && strlen(src_dir) > 1) { /* VPATH builds */
      target = geda_strdup(GOOD_LINK_FILE);
    }
    else {
      target = g_build_filename("data", LINK2SOMEWHERE, NULL);
    }

    /* string = "data/ATMega32-DIP_test.sym" */
    if (strcmp(string, target)) {
      fprintf(stderr, "FAILED: (F050402C) geda_file_sys_follow_symlinks <%s>",string);
      fprintf(stderr, " expected <%s>\n", target);
      result++;
    }

    GEDA_FREE(target); /* string returned from g_build_filename */
    GEDA_FREE(string); /* string returned from geda_follow_symlinks */
  }

#endif /* __MINGW32__ was not defined */

  /* === Function 05: geda_file_sys_normalize_name === */

  string = geda_normalize_filename(NULL, NULL);
  if (string) {
    fprintf(stderr, "FAILED: (F050500) geda_file_sys_normalize_name <%s>\n", string);
    result++;
  }

  string = geda_normalize_filename("./../src", NULL);
  if (string) {

#ifndef OS_WIN32_NATIVE

    if (strncmp(string, "/", 1)) {
      fprintf(stderr, "FAILED: (F050501A) geda_file_sys_normalize_name <%s>\n", string);
      result++;
      string = NULL;
    }
    else {
      if (!strstr(string, "/libgeda/src")) {
        fprintf(stderr, "FAILED: (F050501B) geda_file_sys_normalize_name <%s>\n", string);
        result++;
        string = NULL;
      }
    }

#else

    const char *expected_dir = DIR_SEPARATOR_S "libgeda" DIR_SEPARATOR_S "src";

    if (!strstr(string, expected_dir)) {
      fprintf(stderr, "FAILED: (F050501C) geda_file_sys_normalize_name <%s>\n", string);
      result++;
      string = NULL;
    }

#endif

  }
  else {
    fprintf(stderr, "FAILED: (F050501D) geda_file_sys_normalize_name NULL\n");
    result++;
    string = NULL;
  }

  if (string) { /* If passed last test continue testing F0505 */

    free(string);

    string = geda_normalize_filename("../tests/../src", NULL);

    if (string) {

#ifndef OS_WIN32_NATIVE

      if (strncmp(string, "/", 1)) {
        fprintf(stderr, "FAILED: (F050502A) geda_file_sys_normalize_name <%s>\n", string);
        result++;
      }
      else {
        if (!strstr(string, "/libgeda/src")) {
          fprintf(stderr, "FAILED: (F050502B) geda_file_sys_normalize_name <%s>\n", string);
          result++;
        }
        if (strstr(string, "..")) {
          fprintf(stderr, "FAILED: (F050502B) geda_file_sys_normalize_name <%s>\n", string);
          result++;
        }
      }

#else

      const char *expected_dir = DIR_SEPARATOR_S "libgeda" DIR_SEPARATOR_S "src";

      if (!strstr(string, expected_dir)) {
        fprintf(stderr, "FAILED: (F050502C) geda_file_sys_normalize_name <%s>\n", string);
        result++;
        string = NULL;
      }

#endif

      free(string);
    }
    else {
      fprintf(stderr, "FAILED: (F050502D) geda_file_sys_normalize_name NULL\n");
      result++;
    }
  }

  string = geda_normalize_filename("../noexist", NULL);
  if (string) {
    fprintf(stderr, "FAILED: (F050503) geda_file_sys_normalize_name <%s>\n", string);
    result++;
  }

  GError *F0505_err = NULL;

  string = geda_normalize_filename("../noexist", &F0505_err);

  if (!F0505_err) {
    fprintf(stderr, "FAILED: (F050504) geda_file_sys_normalize_name <%s>\n", string);
    result++;
  }
  else {
    g_error_free(F0505_err);
  }

  /* === Function 06: geda_file_sys_remove === */

  /* See also test_picture_object.c posttest() */

  errno = 0;

  if (remove_file) {

    if (access(TEST_FILE, R_OK) == 0) {

      /* This deletes the file that was copied to the tests directory
       * while testing geda_file_copy
       */

      if (geda_remove_file(TEST_FILE)) {
        fprintf(stderr,"FAILED: (F050601)  <%s>: %s\n", TEST_FILE, strerror(errno));
        exit (1);
      }

      /* Verify that the file was actually removed */
      if (access(TEST_FILE, R_OK) == 0) {
        fprintf(stderr, "FAILED: (F050602) geda_file_sys_remove <%s>\n", TEST_FILE);
        result++;
      }
    }
    else {
      fprintf(stderr,"Error accessing file <%s>: %s\n", TEST_FILE, strerror(errno));
      exit (1);
    }
  }

  /* === Function 07: geda_file_sys_remove_extension === */

  static const struct _TestData F07_str[] =
  {
    { "",         ""  },
    { "a",        "a" },
    { "a.b",      "a" },
    { "a.b\n",    "a" },
    { "/a.b\r",   "/a" },
    { "a.b.c\r",  "a.b" }
  };

  char *expected;

  string = geda_strdup (F07_str[0].input); /* "" */

  if (geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050701) geda_file_sys_remove_extension\n");
    result++;
  }
  g_free(string);

  string = geda_strdup (F07_str[1].input); /* "a" */

  if (geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050702) geda_file_sys_remove_extension\n");
    result++;
  }
  g_free(string);

  string   = geda_strdup (F07_str[2].input); /* "a.b" */
  expected = F07_str[2].expected;

  if (!geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050703A) geda_file_sys_remove_extension\n");
    result++;
  }
  else if (strcmp(string, expected)) {      /* See structure F07_str */
    fprintf(stderr, "FAILED: (F050703B) geda_file_sys_remove_extension <%s>\n",string);
    result++;
  }
  g_free(string);

  string   = geda_strdup (F07_str[3].input); /* "a.b\n" */
  expected = F07_str[3].expected;

  if (!geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050704A) geda_file_sys_remove_extension\n");
    result++;
  }
  else if (strcmp(string, expected)) {      /* See structure F07_str */
    fprintf(stderr, "FAILED: (F050704B) geda_file_sys_remove_extension <%s>\n",string);
    result++;
  }
  g_free(string);

  string   = geda_strdup (F07_str[4].input); /* "/a.b\r" */
  expected = F07_str[4].expected;

  if (!geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050705A) geda_file_sys_remove_extension\n");
    result++;
  }
  else if (strcmp(string, expected)) {      /* See structure F07_str */
    fprintf(stderr, "FAILED: (F050705B) geda_file_sys_remove_extension <%s> expected <%s>\n",string, expected);
    result++;
  }
  g_free(string);

  string   = geda_strdup (F07_str[5].input); /* "a.b.c\r" */
  expected = F07_str[5].expected;

  if (!geda_remove_extension (string)) {
    fprintf(stderr, "FAILED: (F050706A) geda_file_sys_remove_extension\n");
    result++;
  }
  else if (strcmp(string, expected)) {      /* See structure F06_str */
    fprintf(stderr, "FAILED: (F050706B) geda_file_sys_remove_extension <%s> expected <%s>\n",string, expected);
    result++;
  }
  g_free(string);

  return result;
}

void
file_links_4_test(bool create)
{
  char *test_dir;
  char *filename;

  /* This is only needed for distcheck VPATH builds */
  char *src_dir = getenv ("srcdir");

  if (src_dir) {

    char *datadir;

    test_dir = src_dir;

    /* Automake changes permissions of source directories,
     * so change the data subdirectory back to read/write */
    datadir = g_build_filename(test_dir, "data", NULL);

    g_chmod(datadir, S_IRWXU|S_IRWXG);
    g_free(datadir);

  }
  else {
    test_dir = getcwd(0,0);
  }

  filename = g_build_filename(test_dir, BAD_LINK_FILE, NULL);

  /* If filename exist then remove */
  geda_remove_file(filename);

  if (create) {

#ifndef __MINGW32__

    if (symlink(LINK2NOWHERE, filename)) {
      fprintf(stderr, "%s unexpected failure: <%s> %s\n", __func__, filename, strerror(errno));
      exit (1);
    }

#endif

  }
  g_free(filename);

  filename = g_build_filename(test_dir, GOOD_LINK_FILE, NULL);

  geda_remove_file(filename);

  if (create) {

#ifndef __MINGW32__

    if (symlink(LINK2SOMEWHERE, filename)) {
      fprintf(stderr, "%s unexpected failure: <%s> %s\n", __func__, filename, strerror(errno));
      exit (1);
    }

#endif

  }
  g_free(filename);

  if (!src_dir) {
    GEDA_FREE(test_dir);
  }
}

static char *old_sys_data_path;
static char *old_rc_path;

void setup_environment(void)
{
  const char *path;
        char *test_dir;
        char *lib_dir;
        char *top_dir;

  path = getenv ("GEDADATA");

  if (path != NULL) {
    old_sys_data_path = geda_strdup(path);
  }
  else {
    old_sys_data_path = NULL;
  }

  path = getenv ("GEDADATARC");

  if (path != NULL) {
    old_rc_path = geda_strdup(path);
  }
  else {
    old_rc_path = NULL;
  }

  if (g_file_test("Makefile.am", G_FILE_TEST_EXISTS)) {
    vpath_build = FALSE;
  }
  else {
    vpath_build = TRUE;
  }

  /* This is only needed for distcheck VPATH builds */
  char *src_dir = getenv ("srcdir");

  if (src_dir) {
    test_dir = src_dir;
  }
  else {
    test_dir = getcwd(0,0);
  }

  path = g_build_filename(test_dir, "../..", NULL);

#if HAVE_REALPATH

  top_dir = realpath(path, NULL);

#else

  top_dir = g_build_filename(src_dir, "../..", NULL);

#endif

  setenv ("GEDADATA", top_dir, TRUE);

  lib_dir = g_build_filename(top_dir, "libgeda", NULL);

  setenv ("GEDADATARC", lib_dir, TRUE);

  if (!src_dir) {
    GEDA_FREE(test_dir);
  }
  GEDA_FREE(lib_dir);
  GEDA_FREE(top_dir);
}

void restore_environment(void)
{
  if (old_sys_data_path != NULL) {
    setenv ("GEDADATA", old_sys_data_path, TRUE);
  }

  if (old_rc_path != NULL) {
    setenv ("GEDADATA", old_rc_path, TRUE);
  }
}

/** @} endgroup test-file-geda-sys */

int
main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  parse_commandline(argc, argv);

  file_links_4_test(1);
  setup_environment();

  if (setjmp(point) == 0) {
      result = test_file();
  }
  else {
    fprintf(stderr, "Caught signal checking file group 1 src/file/f_file.c\n\n");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_get();
  }
  else {
    fprintf(stderr, "Caught signal checking file group 2 src/file/f_get.c\n\n");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_path();
  }
  else {
    fprintf(stderr, "Caught signal checking file group 3 src/file/f_path.c\n\n");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_print();
  }
  else {
    fprintf(stderr, "Caught signal checking file group 4 src/file/f_print.c\n\n");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_sys();
  }
  else {
    fprintf(stderr, "Caught signal checking file group 5 src/file/f_sys.c\n\n");
    result++;
  }

  file_links_4_test(0);
  restore_environment();

  return result;
}
