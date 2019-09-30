/* -*- test_utility.c -*-
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
 *  Date Contributed: April, 1st, 2016
 */

#include "../../config.h"

#include <libgeda.h>
#include <stdlib.h>
#include <errno.h>

#include "test-suite.h"

/*! \file test_utility.c
 *  \brief Tests for geda utility functions
 *  \par
 *   This is a composite test module that performs tests for all files
 *   in src/utility. Tests for each module/file have been organized in
 *   to separate Doxygen groups and this allows indexing using slash-
 *   asterisk-asterick. Doxygen group numbers corresponds to the Module/
 *   File No. in the Test Identifiers:
 *
 *  <DL>
 *    <DT>Group 1 test-utility-geda-utility src/utility/u_utility.c</DT>
 *    <DT>Group 2 test-utility-geda-glist   src/utility/u_glist.c</DT>
 *    <DT>Group 3 test-utility-geda-log     src/utility/u_log.c</DT>
 *    <DT>Group 4 test-utility-geda-program src/utility/u_program.c</DT>
 *    <DT>Group 5 test-utility-geda-refdes  src/utility/u_refdes.c</DT>
 *    <DT>Group 6 test-utility-geda-string  src/utility/u_string.c</DT>
 *  </DL>
 *
 *  Test Identifiers:  U  01  19  03
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

struct _TestData
{
  char *input;
  char *expected;
};

/** \defgroup test-utility-geda-utility Test GEDA Utility Module
 * @{
 * \brief Group 1 src/utility/u_utility.c geda_utility_
 */

/*!
 * \remarks
 * Scheme API unit tests contains comprehensive tests for
 * expand_env_variable, see t0401-os-expand-env-variables
 */
int test_utility (void)
{
  int   result;
  char *str;

  result = errno = 0;

  /* === Function 01: geda_expand_env_variable === */

  str = geda_expand_env_variable(NULL);
  if (str) {
    fprintf(stderr, "FAILED: (U010100) expand_env_variable <%s>\n", str);
    result++;
  }

  str = geda_expand_env_variable("${geda_expand}");
  if (!str) {
    fprintf(stderr, "FAILED: (U010101A) expand_env_variable NULL\n");
    result++;
  }
  else {
    if (strcmp(str, "")) {
      fprintf(stderr, "FAILED: (U010101B) expand_env_variable\n");
      result++;
    }
    GEDA_FREE (str);
  }

  if (!setenv("geda_expand", "/tests/this", 1)) {

    str = geda_expand_env_variable("${geda_expand}");

    if (!str) {
      fprintf(stderr, "FAILED: (U010102A) expand_env_variable\n");
      result++;
    }
    else {
      if (strncmp(str, "/tests/this", 11)) {
        fprintf(stderr, "FAILED: (U010102B) expand_env_variable <%s>\n", str);
        result++;
      }
      GEDA_FREE (str);
    }
  }
  else {
    fprintf(stderr, "Skipped: (U010102C) %s\n", strerror(errno));
  }

  /* === Function 01: geda_utility_program_print_object === */
  // geda_print_object

  return result;
}

/** @} endgroup test-utility-geda-utility */

/** \defgroup test-utility-geda-glist Test GEDA glist Module
 * @{
 * \brief Group 2 src/utility/u_glist.c geda_utility_gxlist_
 */
int test_glist (void)
{
  GList  *dlist;
  GSList *slist;
  int     count;
  int     result;
  int     value;

  GList *load_list (void) {

    GList *Sharks;
    uint8_t i;

    static char *Chondrichthyes[] = {
                                     "Cladoselache",
                                     "Heterodontus",
                                     "Megalodon",
                                     "Orthacanthus",
                                     "Xenacanthus",
                                     NULL};
    Sharks = NULL;

    for (i = 0; Chondrichthyes[i] != NULL; i++) {
      Sharks = g_list_append(Sharks, geda_strdup(Chondrichthyes[i]));
    }

    return Sharks;
  }

  void Releaser (void *string) {
    g_free(string);
    count++;
  }

  result = 0;

  /* === Function 01: geda_utility_glist_clear === */

  dlist = geda_clear_glist(NULL);
  if (dlist) {
    fprintf(stderr, "FAILED: (U020100) geda_utility_glist_clear <%p>\n", dlist);
    result++;
  }

  dlist = load_list();

  dlist = geda_clear_glist(dlist);

  count = g_list_length(dlist);
  if (count) {
    fprintf(stderr, "FAILED: (U020101) geda_utility_glist_clear <%d>\n", count);
    result++;
  }

  g_list_free(dlist);
  dlist = NULL;

  /* === Function 02: geda_utility_glist_find_string === */

  value = geda_glist_find_string(NULL, NULL) ;
  if (value != -2) {
    fprintf(stderr, "FAILED: (U020200) geda_utility_glist_find_string %d\n", value);
    result++;
  }

  dlist = load_list();

  value = geda_glist_find_string(dlist, "");

  if (value != -1) {
    fprintf(stderr, "FAILED: (U020201) geda_utility_glist_find_string <%d>\n", value);
    result++;
  }

  value = geda_glist_find_string(dlist, "Megalodon");

  if (value != 2) {
    fprintf(stderr, "FAILED: (U020202) geda_utility_glist_find_string <%d>\n", value);
    result++;
  }

  value = geda_glist_find_string(dlist, "Chimaeras");

  if (value != -1) {
    fprintf(stderr, "FAILED: (U020203) geda_utility_glist_find_string <%d>\n", value);
    result++;
  }

  /* === Function 03: geda_utility_glist_free_all === */

  geda_glist_free_all(NULL);

  geda_glist_free_all(dlist);

  dlist = NULL;

  /* === Function 04: geda_utility_glist_free_full === */

  count = 0;

  geda_glist_free_full(NULL, Releaser);

  if (count) {
    fprintf(stderr, "FAILED: (U020400) geda_utility_glist_free_full <%d>\n", count);
    result++;
    count = 0;
  }

  dlist = load_list();

  geda_glist_free_full(dlist, Releaser);

  if (count != 5) {
    fprintf(stderr, "FAILED: (U020401) geda_utility_glist_free_full <%d>\n", count);
    result++;
  }

  dlist = NULL;

  /* === Function 05: geda_utility_glist_str_inlist === */

  if (geda_glist_str_inlist(NULL, NULL)) {
    fprintf(stderr, "FAILED: (U020500A) geda_utility_glist_str_inlist NULL\n");
    result++;
  }

  dlist = load_list();

  if (geda_glist_str_inlist(dlist, NULL)) {
    fprintf(stderr, "FAILED: (U020500B) geda_utility_glist_str_inlist NULL\n");
    result++;
  }

  if (geda_glist_str_inlist(dlist, "")) {
    fprintf(stderr, "FAILED: (U020501) geda_utility_glist_str_inlist\n");
    result++;
  }

  if (!geda_glist_str_inlist(dlist, "Megalodon")) {
    fprintf(stderr, "FAILED: (U020502) geda_utility_glist_str_inlist\n");
    result++;
  }

  if (!geda_glist_str_inlist(dlist, "Orthacanthus")) {
    fprintf(stderr, "FAILED: (U020503) geda_utility_glist_str_inlist\n");
    result++;
  }

  if (!geda_glist_str_inlist(dlist, "Heterodontus")) {
    fprintf(stderr, "FAILED: (U020504) geda_utility_glist_str_inlist\n");
    result++;
  }

  if (geda_glist_str_inlist(dlist, "Chimaeras")) {
    fprintf(stderr, "FAILED: (U020505) geda_utility_glist_str_inlist\n");
    result++;
  }

  /* === Function 06: geda_utility_glist_stri_inlist === */

  if (geda_glist_stri_inlist(NULL, NULL)) {
    fprintf(stderr, "FAILED: (U020600A) geda_utility_glist_stri_inlist NULL\n");
    result++;
  }

  if (geda_glist_stri_inlist(dlist, NULL)) {
    fprintf(stderr, "FAILED: (U020600B) geda_utility_glist_stri_inlist NULL\n");
    result++;
  }

  if (geda_glist_stri_inlist(dlist, "")) {
    fprintf(stderr, "FAILED: (U020601) geda_utility_glist_stri_inlist\n");
    result++;
  }

  if (!geda_glist_stri_inlist(dlist, "Megalodon")) {
    fprintf(stderr, "FAILED: (U020602) geda_utility_glist_stri_inlist\n");
    result++;
  }

  if (!geda_glist_stri_inlist(dlist, "cladoselache")) {
    fprintf(stderr, "FAILED: (U020603) geda_utility_glist_stri_inlist\n");
    result++;
  }

  if (!geda_glist_stri_inlist(dlist, "XENACANTHUS")) {
    fprintf(stderr, "FAILED: (U020604) geda_utility_glist_stri_inlist\n");
    result++;
  }

  if (geda_glist_stri_inlist(dlist, "Chimaeras")) {
    fprintf(stderr, "FAILED: (U020605) geda_utility_glist_stri_inlist\n");
    result++;
  }

  /* === Function 07: geda_utility_gslist_clear === */

  slist = geda_clear_gslist(NULL);
  if (slist) {
    fprintf(stderr, "FAILED: (U020700) geda_clear_gslist <%p>\n", slist);
    result++;
  }

  /* === Function 08: geda_utility_gslist_find_string === */

  value = geda_gslist_find_string(NULL, NULL) ;
  if (value != -2) {
    fprintf(stderr, "FAILED: (U020800) geda_gslist_find_string %d\n", value);
    result++;
  }

  /* === Function 09: geda_gslist_free_all    geda_utility_gslist_free_all === */

  geda_gslist_free_all(NULL);

  /* === Function 11: geda_gslist_free_full   geda_utility_gslist_free_full === */

  geda_gslist_free_full(NULL, free);

  /* === Function 12: geda_utility_gslist_str_inlist === */

  if (geda_gslist_str_inlist(NULL, NULL)) {
    fprintf(stderr, "FAILED: (U021100) geda_gslist_str_inlist NULL\n");
    result++;
  }

  /* === Function 13: geda_utility_gslist_stri_inlist === */

  if (geda_gslist_stri_inlist(NULL, NULL)) {
    fprintf(stderr, "FAILED: (U021200) geda_gslist_stri_inlist NULL\n");
    result++;
  }

  return result;
}
/** @} endgroup test-utility-geda-glist */

/** \defgroup test-utility-geda-log Test GEDA Log Module
 * @{
 * \brief Group 3 src/utility/u_log.c geda_utility_log_
 */

static void default_log_handler (const char    *log_domain,
                                 GLogLevelFlags log_level,
                                 const char    *message,
                                 void          *user_data)
{
  int *len = (int*)user_data;

  if (message)
    *len = strlen(message);
  else
    *len = 0;
}

char *log_mess;

void test_log_message (const char     *log_domain,
                       GLogLevelFlags  log_level,
                       const char     *message)
{
  log_mess = g_strdup(message);
}

int test_log (void)
{
  int result = 0;
  int default_handler;

  /*
   * Function 01: geda_utility_log_close
   * Function 02: geda_utility_log_get_log_time
   * Function 03  geda_utility_log_get_quiet_mode
   * Function 04  geda_utility_log_get_verbose_mode
   * Function 05: geda_utility_log_init
   * Function 06: geda_utility_log_quite
   * Function 07: geda_utility_log_read
   * Function 08: geda_utility_log_set_default_handler
   * Function 09: geda_utility_log_set_log_time
   * Function 10  geda_utility_log_set_quiet_mode
   * Function 11: geda_utility_log_set_update_func
   * Function 12: geda_utility_log_set_verbose_mode
   * Function 13: geda_utility_log_system
   * Function 14: geda_utility_log_verbose
   */

  /* === Function 08: geda_utility_log_set_default_handler === */
  geda_log ("Testing prior to setting handler");

  geda_set_default_logger(default_log_handler, &default_handler);

  default_handler = 0;

  geda_log ("Testing");

  if (default_handler - 7) {
    fprintf(stderr, "FAILED: (U030801) geda_set_default_logger %d\n", default_handler);
    result++;
  }

  char *cwd = g_get_current_dir();
  setenv ("GEDALOGS", cwd, 0);
  GEDA_FREE (cwd);

  geda_set_log_time(TRUE);

  /* === Function 02: geda_utility_log_get_log_time === */

  if (!geda_get_log_time()) {
    fprintf(stderr, "FAILED: (U0302/0701A) get/set log-time\n");
    result++;
  }
  else {

    /* === Function 09: geda_utility_log_set_log_time === */

    geda_set_log_time(FALSE);

    if (geda_get_log_time()) {
      fprintf(stderr, "FAILED: (U0302/0901B) get set log-time\n");
      result++;
    }
  }

  geda_set_quiet_mode(TRUE);

  /* === Function 03 geda_utility_log_get_quiet_mode === */

  if (!geda_get_quiet_mode()) {
    fprintf(stderr, "FAILED: (U0303/1001A) get/set quiet mode\n");
    result++;
  }
  else {

    /* === Function 10 geda_utility_log_set_quiet_mode === */

    geda_set_quiet_mode(FALSE);

    if (geda_get_quiet_mode()) {
      fprintf(stderr, "FAILED: (U0303/1001B) get/set quiet mode\n");
      result++;
    }
  }

  geda_set_verbose_mode(TRUE);

  /* === Function 04 geda_utility_log_get_verbose_mode === */

  if (!geda_get_verbose_mode()) {
    fprintf(stderr, "FAILED: (U0304/1201A) get/set verbose mode\n");
    result++;
  }
  else {

  /* === Function 12 geda_utility_log_set_verbose_mode === */

    geda_set_verbose_mode(FALSE);

    if (geda_get_verbose_mode()) {
      fprintf(stderr, "FAILED: (U0304/1201B) get/set verbose mode\n");
      result++;
    }
  }

  /* === Function 05: geda_utility_log_init === */

  geda_init_log("testing");

  /* === Function 11 geda_utility_log_set_update_func === */

  geda_set_log_update_func (test_log_message);

  log_mess = NULL;

  geda_log ("message 1");

  if (!log_mess) {
    fprintf(stderr, "FAILED: (U031101A) log_set_update_func\n");
    result++;
  }
  else {
    if (strcmp(log_mess, "message 1")) {
      fprintf(stderr, "FAILED: (U031101B) log_set_update_func <%s>\n", log_mess);
      result++;
    }
    GEDA_FREE(log_mess);
  }

  log_mess = NULL;
  geda_set_quiet_mode(TRUE);

  /* === Function 06 geda_utility_log_quite === */

  geda_log_q ("message q1");

  if (log_mess) {
    fprintf(stderr, "FAILED: (U030601) log_quite\n");
    result++;
    GEDA_FREE(log_mess);
  }

  /* === Function 07: geda_utility_log_read === */

  char *contents;

  /* read the content of the current log file */
  contents = geda_read_log ();

  if (!contents) {
    fprintf(stderr, "FAILED: (U030701A) geda_read_log NULL\n");
    result++;
  }
  else {
    if (strcmp(contents, "message 1")) {
      fprintf(stderr, "FAILED: (U030701B) geda_read_log <%s>\n", contents);
      result++;
    }
    GEDA_FREE (contents);
  }

  /* === Function 10 geda_utility_log_set_quiet_mode === */

  geda_set_quiet_mode(FALSE);

  geda_log_q ("message q2");

  if (!log_mess) {
    fprintf(stderr, "FAILED: (U0306/1001A) set log quiet\n");
    result++;
  }
  else {
    if (strcmp(log_mess, "message q2")) {
      fprintf(stderr, "FAILED: (U0306/1001B) set log quiet <%s>\n", log_mess);
      result++;
    }
    GEDA_FREE(log_mess);
  }

  log_mess = NULL;

  geda_set_verbose_mode(FALSE); /* Is already unset */

  /* === Function 14 geda_utility_log_verbose === */

  geda_log_v ("message v1");

  if (log_mess) {
    fprintf(stderr, "FAILED: (U031401) log_verbose\n");
    result++;
    GEDA_FREE(log_mess);
  }

  /* === Function 10 geda_utility_log_set_verbose_mode === */

  log_mess = NULL;

  geda_set_verbose_mode(TRUE);

  geda_log_v ("message v2");

  if (!log_mess) {
    fprintf(stderr, "FAILED: (U0312/1401A) set log verbose\n");
    result++;
  }
  else {
    if (strcmp(log_mess, "message v2")) {
      fprintf(stderr, "FAILED: (U0312/1401B) set log verbose <%s>\n", log_mess);
      result++;
    }
    GEDA_FREE(log_mess);
  }

  /* === Function 13: geda_log_s geda_utility_log_system === */

  /* === Function 01 geda_utility_log_close === */
  geda_close_log();

  log_mess = NULL;

  geda_log ("message 2");

  if (log_mess) {
    fprintf(stderr, "FAILED: (U030101) log close <%s>\n", log_mess);
    result++;
    GEDA_FREE (log_mess);
  }

  return result;
}
/** @} endgroup test-utility-geda-log */

/** \defgroup test-utility-geda-program Test GEDA Program Module
 * @{
 * \brief Group 4 src/utility/u_program.c geda_utility_program_
 */
int test_program (void)
{
  /* === Function 01: geda_program_backtrace  geda_utility_program_backtrace === */
  /* === Function 02: geda_malloc             geda_utility_program_mem_alloc === */
  /* === Function 03: geda_calloc             geda_utility_program_mem_calloc === */
  /* === Function 04: geda_free               geda_utility_program_mem_free === */
  /* === Function 05: geda_set_memory_vtable  geda_utility_program_mem_set_vtable === */
  return 0;
}
/** @} endgroup test-utility-geda-program */

/** \defgroup test-utility-geda-refdes Test GEDA RefDes Module
 * @{
 * \brief Group 5 src/utility/u_refdes.c geda_refdes_program_
 */
int test_refdes (void)
{
  const char *ref;
  int count;
  int index;
  int result = 0;

  const GedaRefDes *designators;

  /* === Function 01: geda_utility_refdes_get_ieee === */

  designators = geda_get_ieee_refdes();

  if (designators == NULL) {
    fprintf(stderr, "FAILED: (U050101) geda_utility_refdes_get_ieee NULL\n");
    result++;
  }

  /* === Function 02: geda_utility_refdes_get_spice === */

  designators = geda_get_spice_refdes();

  if (designators == NULL) {
    fprintf(stderr, "FAILED: (U050201) geda_utility_refdes_get_spice NULL\n");
    result++;
  }

  /* === Function 03: geda_utility_refdes_get_standard  === */

  designators = geda_get_standard_refdes();

  if (designators == NULL) {
    fprintf(stderr, "FAILED: (U050301) geda_utility_refdes_get_standard NULL\n");
    result++;
  }

  /* === Function 04: geda_utility_refdes_reset === */

  static const struct _TestData U04_str[] =
  {
    { "refdes=L",     "refdes=L"  },
    { "refdes=U?",    "refdes=U?" },
    { "refdes=U1",    "refdes=U?" },
    { "refdes=U2A",   "refdes=U?" },
    { "refdes=C188",  "refdes=C?" },
    { "refdes=R1904", "refdes=R?" },
    { "refdes=99",    "refdes=99" },
    { "refdes=8A",    "refdes=8A" }
  };

  count = sizeof(U04_str) / sizeof(struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U04_str[index].expected;
    char *input    = U04_str[index].input;

    GedaObject *object04;

    object04 = geda_text_object_new(3,                       /* color */
                                    10,                      /* X */
                                    20,                      /* Y */
                                    0,                       /* align */
                                    0,                       /* zero is angle */
                                    DEFAULT_ATTRIBUTE_SIZE,  /* default text size */
                                    1,                       /* visibility */
                                    1,                       /* show_name_value*/
                                    input);

    geda_reset_refdes(object04);

    ref = object04->text->string;

    if (!ref) {
      fprintf(stderr, "FAILED: (U050401A-%d) geda_utility_refdes_reset NULL\n", index);
      result++;
    }
    else {
      if (strcmp(ref, expected)) {      /* See structure U06_str */
        fprintf(stderr, "FAILED: (U050401B-%d) geda_utility_refdes_reset <%s>\n",index, ref);
        result++;
      }
    }
  }

  /* === Function 05: geda_utility_refdes_return_numeric === */

  static const struct _TestData U05_str[] =
  {
    { "U1",    "1"   },
    { "U2A",   "2A"  },
    { "U188",  "188" },
  };

  ref = geda_refdes_get_numeric(NULL);
  if (ref) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U050500) geda_refdes_get_numeric <%s>\n", ref);
    result++;
  }

  ref = geda_refdes_get_numeric("");
  if (ref) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U050501) geda_refdes_get_numeric <%s>\n", ref);
    result++;
  }

  count = sizeof(U05_str) / sizeof(struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U05_str[index].expected;
    char *input    = U05_str[index].input;

    ref = geda_refdes_get_numeric (input);

    if (ref) {
      if (strcmp(ref, expected)) {      /* See structure U06_str */
        fprintf(stderr, "FAILED: (U050502B-%d) geda_refdes_get_numeric <%s>\n",index, ref);
        result++;
      }
    }
    else {
      fprintf(stderr, "FAILED: (U050502C-%d) geda_refdes_get_numeric NULL\n",index);
      result++;
    }
  }

  /* === Function 06: geda_utility_refdes_return_prefix === */

  static const struct _TestData U06_str[] =
  {
    { "refdes=X1",    "X" },
    { "refdes=Y2A",   "Y" },
    { "refdes=Z188",  "Z" },
  };

  ref = geda_refdes_get_prefix(NULL);
  if (ref) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U050600) geda_refdes_get_prefix <%s>\n", ref);
    result++;
  }

  ref = geda_refdes_get_prefix("");
  if (ref) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U050601) geda_refdes_get_prefix <%s>\n", ref);
    result++;
  }

  count = sizeof(U06_str) / sizeof(struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U06_str[index].expected;
    char *input    = U06_str[index].input;
    char *prefix;

    prefix = geda_refdes_get_prefix (input);

    if (prefix) {
      if (strcmp(prefix, expected)) {      /* See structure U06_str */
        fprintf(stderr, "FAILED: (U050602B-%d) geda_refdes_get_prefix <%s>\n",index, prefix);
        result++;
      }
      GEDA_FREE (prefix);
    }
    else {
      fprintf(stderr, "FAILED: (U050602C-%d) geda_refdes_get_prefix NULL\n",index);
      result++;
    }
  }

  return result;
}
/** @} endgroup test-utility-geda-refdes */

/** \defgroup test-utility-geda-string Test GEDA String Module
 * @{
 * \brief Group 6 src/utility/u_string.c geda_utility_string_
 */

static int test_string_strsize (const char *format, ...)
{
  int size;

  va_list args;

  va_start (args, format);
  size = geda_strsize(format, args);
  va_end (args);

  return size;
}

int test_strings (void)
{
  int count;
  int index;
  int value;
  int result = 0;

  char *string;

  const char *str1 = "Dog";
  const char *str2 = "doo";
  const char *str3 = "hounddog";
  const char *str4 = "DOGBONE";

  /* === Function 01: geda_utility_string_concat === */

  string = geda_strconcat(NULL, NULL);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060100) geda_strconcat <%s>\n", string);
    result++;
  }

  string = geda_strconcat(str1, NULL);
  if (!string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060101A) geda_strconcat NULL\n");
    result++;
  }
  else if (strcmp(string, "Dog")) {
    fprintf(stderr, "FAILED: (U060101B) geda_strconcat <%s>\n", string);
    result++;
  }

  string = geda_strconcat(str1, " ", str2, " ",str2, NULL);
  if (strcmp(string, "Dog doo doo")) {    /* Dog in DOGBONE */
    fprintf(stderr, "FAILED: (U060102) geda_strconcat <%s>\n", string);
    result++;
  }

  string = geda_strconcat(str1, str2, NULL);
  if (strcmp(string, "Dogdoo")) {         /* Dog in DOGBONE */
    fprintf(stderr, "FAILED: (U060103) geda_strconcat <%s>\n", string);
    result++;
  }

  /* === Function 02: geda_utility_string_get_valid_utf8 === */

  static const struct _TestData U02_str[] =
  {
    { "",                 ""                  }, /* empty string */
    { "\27",              "\27"               }, /* "a"          */
    { "\27\010",          "\27\010"           }, /* "a\n"        */
    { "\357\277\275\013", "\357\277\275\013"  }, /* "\r"     */
    { "\010\127",         "\010\127"          }, /* */
    { "\0540\05C0",       "\0540\05C0"        }, /* Armenia thru Hebrew */
    { "\000\000",         ""                  }, /* MUTF-8 is not UTF-8 */
    { "\27\0xFE\0xFF\27", "\27"               }  /* 0xFE 0xFF not allowed */
  };

  string = geda_get_utf8(NULL);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060200) geda_get_utf8 <%s>\n", string);
    result++;
  }

  count = sizeof(U02_str) / sizeof(struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U02_str[index].expected;
    char *input    = U02_str[index].input;

    string = geda_get_utf8 (input);

    if (string) {
      if (strcmp(string, expected)) {      /* See structure U02_str */
        fprintf(stderr, "FAILED: (U060201A-%d) geda_get_utf8 <%s>\n",index, string);
        result++;
      }
    }
    else {
      fprintf(stderr, "FAILED: (U060201B-%d) expected <%s> NULL\n",index, expected);
      result++;
    }
  }

  /* === Function 03: geda_utility_string_int2str === */

  char U03_val[10];

  string = geda_string_int2str(0, NULL, 10);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060300) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(0, U03_val, 10);
  if (strcmp(string, "0")) {              /* Zero */
    fprintf(stderr, "FAILED: (U060301) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(123456789, U03_val, 10);
  if (strcmp(string, "123456789")) {      /* 9 digits plus NULL */
    fprintf(stderr, "FAILED: (U060302) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(-12345678, U03_val, 10);
  if (strcmp(string, "-12345678")) {      /* Neg 8 digits plus NULL */
    fprintf(stderr, "FAILED: (U060303) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(345.6, U03_val, 10);
  if (strcmp(string, "345")) {            /* Only integers returned */
    fprintf(stderr, "FAILED: (U060304) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(16, U03_val, 16);
  if (strcmp(string, "10")) {             /* Is hex */
    fprintf(stderr, "FAILED: (U060305) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(16, U03_val, 8);
  if (strcmp(string, "20")) {             /* 16 Dec = 20 base 8 */
    fprintf(stderr, "FAILED: (U060306) geda_string_int2str <%s>\n", string);
    result++;
  }

  /* === Function 04: geda_utility_string_isalnum === */

  value = geda_utility_string_isalnum(NULL);
  if (value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060400) geda_utility_string_isalnum\n");
    result++;
  }

  value = geda_utility_string_isalnum("60");
  if (!value) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060401) geda_utility_string_isalnum <%d>\n", value);
    result++;
  }

  value = geda_utility_string_isalnum(str4);
  if (!value) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060402) geda_utility_string_isalnum <%d>\n", value);
    result++;
  }

  value = geda_utility_string_isalnum("$#!");
  if (value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060403) geda_utility_string_isalnum <%d>\n", value);
    result++;
  }

  /* === Function 05: geda_utility_string_istr  === */

  const char *ptr;

  ptr = geda_string_istr(NULL, NULL);
  if (ptr) {                              /* NULL input */
    fprintf(stderr, "FAILED: (U060500) geda_string_istr <%s>\n", ptr);
    result++;
  }

  ptr = geda_string_istr(str2, str1);
  if (ptr) {                              /* doo not in dog */
    fprintf(stderr, "FAILED: (U060501) geda_string_istr NULL\n");
    result++;
  }

  ptr = geda_string_istr(str3, str1);
  if (!ptr) {                             /* NULL returned */
    fprintf(stderr, "FAILED: (U060502A) geda_string_istr NULL\n");
    result++;
  }
  else if (strcmp(ptr, "dog")) {          /* Dog in hounddog */
    fprintf(stderr, "FAILED: (U060502B) geda_string_istr <%s>\n", ptr);
    result++;
  }

  ptr = geda_string_istr(str4, str1);
  if (!ptr) {                             /* NULL returned */
    fprintf(stderr, "FAILED: (U060503A) geda_string_istr NULL\n");
    result++;
  }
  else if (strcmp(ptr, "DOGBONE")) {      /* Dog in DOGBONE */
    fprintf(stderr, "FAILED: (U060503B) geda_string_istr <%s>\n", ptr);
    result++;
  }

  /* === Function 06: geda_utility_string_parse_xy === */

  int X06, Y06;

  value = geda_string_parse_xy(NULL, &X06, &Y06);
  if (value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060600A) geda_string_parse_xy <%d>\n", value);
    result++;
  }

  X06 = Y06 = 1;

  value = geda_string_parse_xy("0,0", NULL, &Y06);
  if (!value) {                            /* NULL X, Y okay */
    fprintf(stderr, "FAILED: (U060600B1) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (Y06 != 0) {
      fprintf(stderr, "FAILED: (U060600B2) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 1;

  value = geda_string_parse_xy("0,0", &X06, NULL);
  if (!value) {                            /* NULL Y, X okay */
    fprintf(stderr, "FAILED: (U060600C1) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != 0) {
      fprintf(stderr, "FAILED: (U060600C2) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
  }

  X06 = Y06 = 1;

  value = geda_string_parse_xy("0,0", &X06, &Y06);
  if (!value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060601A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != 0) {
      fprintf(stderr, "FAILED: (U060601B) geda_string_parse_xy <%d>\n", value);
      result++;
    }
    if (Y06 != 0) {
      fprintf(stderr, "FAILED: (U060601C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 2;

  value = geda_string_parse_xy("(0,0)", &X06, &Y06);
  if (!value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060602A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != 0) {
      fprintf(stderr, "FAILED: (U060602B) geda_string_parse_xy <%d>\n", value);
      result++;
    }
    if (Y06 != 0) {
      fprintf(stderr, "FAILED: (U060602C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 3;

  value = geda_string_parse_xy("4500 380", &X06, &Y06);
  if (!value) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060603A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != 4500) {
      fprintf(stderr, "FAILED: (U060603B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != 380) {
      fprintf(stderr, "FAILED: (U060603C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 4;

  value = geda_string_parse_xy("2200", &X06, &Y06);
  if (!value) {                           /* No y input, x valid */
    fprintf(stderr, "FAILED: (U060604A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != 2200) {
      fprintf(stderr, "FAILED: (U060604B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != 0) {
      fprintf(stderr, "FAILED: (U060604C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 5;

  value = geda_string_parse_xy(",313", NULL, &Y06);
  if (!value) {                           /* NULL x io, y valid */
    fprintf(stderr, "FAILED: (U060605A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (Y06 != 313) {
      fprintf(stderr, "FAILED: (U060605B) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 6;

  value = geda_string_parse_xy("", &X06, &Y06);
  if (value) {                            /* empty string */
    fprintf(stderr, "FAILED: (U060606A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != 6) {
      fprintf(stderr, "FAILED: (U060606B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != 6) {
      fprintf(stderr, "FAILED: (U060606C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 7; /* neg */

  value = geda_string_parse_xy("-200", &X06, &Y06);
  if (!value) {                           /* No y input, x valid */
    fprintf(stderr, "FAILED: (U060607A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != -200) {
      fprintf(stderr, "FAILED: (U060607B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != 0) {
      fprintf(stderr, "FAILED: (U060607C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 8; /* neg pos */

  value = geda_string_parse_xy("-100,200", &X06, &Y06);
  if (!value) {
    fprintf(stderr, "FAILED: (U060608A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != -100) {
      fprintf(stderr, "FAILED: (U060608B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != 200) {
      fprintf(stderr, "FAILED: (U060608C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 9; /* pos neg */

  value = geda_string_parse_xy("100,-200", &X06, &Y06);
  if (!value) {
    fprintf(stderr, "FAILED: (U060609A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != 100) {
      fprintf(stderr, "FAILED: (U060609B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != -200) {
      fprintf(stderr, "FAILED: (U060609C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 10; /* neg neg */

  value = geda_string_parse_xy("-100,-200", &X06, &Y06);
  if (!value) {
    fprintf(stderr, "FAILED: (U060610A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != -100) {
      fprintf(stderr, "FAILED: (U060610B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != -200) {
      fprintf(stderr, "FAILED: (U060610C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 11; /* (neg pos) */

  value = geda_string_parse_xy("(-100, 200)", &X06, &Y06);
  if (!value) {
    fprintf(stderr, "FAILED: (U060611A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != -100) {
      fprintf(stderr, "FAILED: (U060611B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != 200) {
      fprintf(stderr, "FAILED: (U060611C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 12; /* (pos neg) */

  value = geda_string_parse_xy("(100,-200)", &X06, &Y06);
  if (!value) {
    fprintf(stderr, "FAILED: (U060612A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != 100) {
      fprintf(stderr, "FAILED: (U060612B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != -200) {
      fprintf(stderr, "FAILED: (U060612C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  X06 = Y06 = 13; /* (neg neg) */

  value = geda_string_parse_xy("(-100,-200)", &X06, &Y06);
  if (!value) {
    fprintf(stderr, "FAILED: (U060613A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X06 != -100) {
      fprintf(stderr, "FAILED: (U060613B) geda_string_parse_xy <%d>\n", X06);
      result++;
    }
    if (Y06 != -200) {
      fprintf(stderr, "FAILED: (U060613C) geda_string_parse_xy <%d>\n", Y06);
      result++;
    }
  }

  /* === Function 07: geda_utility_string_remove_last_nl   === */

  static const struct _TestData U07_str[] =
  {
    { "",       ""     },
    { "a",      "a"    },
    { "a\n",    "a"    },
    { "a\r",    "a"    },
    { "\na",    "\na"  },
    { "\ra",    "\ra"  },
    { "\n\n",   "\n"   },
    { "a\nb\n", "a\nb" }
  };

  string = geda_remove_last_newline(NULL);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060700) geda_remove_last_newline <%s>\n", string);
    result++;
  }

  count = sizeof(U07_str) / sizeof(struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U07_str[index].expected;
    char *input    = geda_strdup (U07_str[index].input);

    string = geda_remove_last_newline (input);

    if (string) {
      if (strcmp(string, expected)) {      /* See structure U07_str */
        fprintf(stderr, "FAILED: (U060701A-%d) geda_remove_last_newline <%s>\n",index, string);
        result++;
      }
      free (input);
    }
    else {
      fprintf(stderr, "FAILED: (U060701B-%d) expected <%s> NULL\n",index, expected);
      result++;
    }
  }

  /* === Function 08: geda_utility_string_remove_nl === */

  static const struct _TestData U08_str[] =
  {
    { "",       ""  },
    { "a",      "a" },
    { "a\n",    "a" },
    { "a\r",    "a" },
    { "\na",    ""  },
    { "\ra",    ""  },
    { "\n\n",   ""  },
    { "a\nb\n", "a" }
  };

  string = geda_remove_newline(NULL);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060800) geda_remove_newline <%s>\n", string);
    result++;
  }

  count = sizeof(U08_str) / sizeof(struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U08_str[index].expected;
    char *input    = geda_strdup (U08_str[index].input);

    string = geda_remove_newline (input);

    if (string) {
      if (strcmp(string, expected)) {      /* See structure U08_str */
        fprintf(stderr, "FAILED: (U060801A-%d) geda_remove_newline <%s>\n",index, string);
        result++;
      }
      free (input);
    }
    else {
      fprintf(stderr, "FAILED: (U060801B-%d) expected <%s> NULL\n",index, expected);
      result++;
    }
  }

  /* === Function 09: geda_utility_string_scm2c === */

  string = geda_utility_string_scm2c(NULL);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060900) geda_string_scm2c <%s>\n", string);
    result++;
  }

  /* Additional tests not performed because rc files were not processed */

  /* === Function 10: geda_utility_string_sort_array === */

  char *arr_010[] = {"Black", "red", "blue", "pink", "lavender"};

  geda_sort_string_array (arr_010, sizeof(arr_010));
  if (strcmp(arr_010[0], "Black"))
  {
    fprintf(stderr, "FAILED: (U061000A) geda_sort_string_array <%s>\n", arr_010[0]);
    result++;
  }

  if (strcmp(arr_010[1], "blue"))
  {
    fprintf(stderr, "FAILED: (U061000B) geda_sort_string_array <%s>\n", arr_010[1]);
    result++;
  }

  if (strcmp(arr_010[2], "lavender"))
  {
    fprintf(stderr, "FAILED: (U061000C) geda_sort_string_array <%s>\n", arr_010[2]);
    result++;
  }

  if (strcmp(arr_010[3], "pink"))
  {
    fprintf(stderr, "FAILED: (U061000D) geda_sort_string_array <%s>\n", arr_010[3]);
    result++;
  }

  if (strcmp(arr_010[4], "red"))
  {
    fprintf(stderr, "FAILED: (U061000E) geda_sort_string_array <%s>\n", arr_010[4]);
    result++;
  }

  /* === Function 11: geda_utility_string_split === */

  char *str_110 = "Kansas=Topeka=Capital";

  string = geda_strsplit(NULL, '=', 0); /* NULL Argument 1 */
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U061100A) geda_strsplit <%s>\n", string);
    result++;
  }

  string = geda_strsplit(str_110, 0, 0); /* NULL Argument 2 defaults space */
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061100B) geda_strsplit NULL\n");
    result++;
  }
  else {

    /* Original string should be returned because there is no space */
    if (strcmp(string, str_110) != 0) {
      fprintf(stderr, "FAILED: (U061100C) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  string = geda_strsplit(str_110, '=', 0);
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061101A) geda_strsplit\n");
    result++;
  }
  else {

    if (strcmp(string, "Kansas") != 0) {
      fprintf(stderr, "FAILED: (U061101B) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  string = geda_strsplit(str_110, '=', 1);
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061102A) geda_strsplit\n");
    result++;
  }
  else {

    if (strcmp(string, "Topeka") != 0) {
      fprintf(stderr, "FAILED: (U061102B) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  string = geda_strsplit(str_110, '=', 2);
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061103A) geda_strsplit\n");
    result++;
  }
  else {

    if (strcmp(string, "Capital") != 0) {
      fprintf(stderr, "FAILED: (U061103B) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  /* Verify that the orginal string was not altered or released */
  if (strcmp(str_110, "Kansas=Topeka=Capital") != 0) {
    fprintf(stderr, "FAILED: (U061104) geda_strsplit <%s>\n", str_110);
    result++;
  }

  char *str_111 = "  Kansas=Capital=Topeka";

  /* Verify leading whitespaces are skipped and not returned */
  string = geda_strsplit(str_111, '=', 0);
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061101A) geda_strsplit\n");
    result++;
  }
  else {

    if (strcmp(string, "Kansas") != 0) {
      fprintf(stderr, "FAILED: (U061105) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  /* Verify EOS is detected when only whitespaces */
  string = geda_strsplit("   ", '=', 1);
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U061106) geda_strsplit <%s>\n", string);
    result++;
  }

  /* === Function 12: geda_utility_string_sprintf  geda_sprintf === */

  char *str_120 = "The quick brown fox jumped\0";
  char *str_121 = "over the lazy dog's back\0";

  string = geda_sprintf ("%s %s", str_120, str_121);
  if (string) {
    if (strcmp(string, "The quick brown fox jumped over the lazy dog's back"))
    {
      fprintf(stderr, "FAILED: (U061201A) geda_sprintf <%s>\n", string);
      result++;
    }
    free (string);
  }
  else {
    fprintf(stderr, "FAILED: (U061201B) geda_sprintf returned NULL\n");
    result++;
  }

  string = geda_sprintf ("%s %1.3f meters in %d days", str_120, 3.45, 10);
  if (string) {
    if (strcmp(string, "The quick brown fox jumped 3.450 meters in 10 days"))
    {
      fprintf(stderr, "FAILED: (U061202A) geda_sprintf <%s>\n", string);
      result++;
    }
    free (string);
  }
  else {
    fprintf(stderr, "FAILED: (U061202B) geda_sprintf returned NULL\n");
    result++;
  }

  /* === Function 13: geda_utility_string_strdup === */

  string = geda_strdup(NULL);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U061300) geda_strdup <%s>\n", string);
    result++;
  }

  string = geda_strdup("");
  if (!string) {                           /* EMPTY input */
    fprintf(stderr, "FAILED: (U061301) geda_strdup returned NULL\n");
    result++;
  }

  string = geda_strdup(str3);
  if (string) {
    if (strcmp(string, str3) != 0) {      /* 9 > input */
      fprintf(stderr, "FAILED: (U061302A) geda_strdup <%s>\n", string);
      result++;
    }
    free(string);
  }
  else {
    fprintf(stderr, "FAILED: (U061302B) geda_strdup returned NULL\n");
    result++;
  }

  /* === Function 14: geda_utility_string_strequal === */

  value = geda_strequal(NULL, "due");
  if (value) {                            /* NULL != due */
    fprintf(stderr, "FAILED: (U061400A) geda_strequal <%d>\n", value);
    result++;
  }

  value = geda_strequal("dew", NULL);
  if (value) {                            /* due != NULL */
    fprintf(stderr, "FAILED: (U061400B) geda_strequal <%d>\n", value);
    result++;
  }

  value = geda_strequal("dew", "");
  if (value) {                            /* due != "" */
    fprintf(stderr, "FAILED: (U061401) geda_strequal <%d>\n", value);
    result++;
  }

  value = geda_strequal("dew", str2);
  if (value) {                            /* dew != doo */
    fprintf(stderr, "FAILED: (U061402) geda_strequal <%d>\n", value);
    result++;
  }

  value = geda_strequal("Doo", "Doo");
  if (!value) {                           /* Doo == Doo */
    fprintf(stderr, "FAILED: (U061403) geda_strequal <%d>\n", value);
    result++;
  }

  /* === Function 15: geda_utility_string_stricmp === */

  value = geda_stricmp(str1, "DOG");
  if (value) {                            /* Dog == DOG */
    fprintf(stderr, "FAILED: (U061501) geda_stricmp <%d>\n", value);
    result++;
  }

  value = geda_stricmp("dew", "due");
  if (!value) {                           /* dew != due */
    fprintf(stderr, "FAILED: (U061502) geda_stricmp <%d>\n", value);
    result++;
  }

  value = geda_stricmp(str2, "Do");
  if (!value) {                           /* doo != Do */
    fprintf(stderr, "FAILED: (U061503) geda_stricmp <%d>\n", value);
    result++;
  }

  /* === Function 16: geda_utility_string_stristr === */

  value = geda_stristr(NULL, "dog");
  if (value >= 0) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U061600A) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr("fox", NULL);
  if (value >= 0) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U061600B) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr(str_120, "fox");
  if (value != 16) {                      /* fox @ position 16 */
    fprintf(stderr, "FAILED: (U061601) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr(str_120, "Dog");
  if (value >= 0) {                            /* Dog not in str_120 */
    fprintf(stderr, "FAILED: (U061602) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr(str_121, "Dog");
  if (value != 14) {                      /* dog @ position 14 */
    fprintf(stderr, "FAILED: (U061603) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr(str_120, "The");
  if (value != 0) {                       /* The @ position 0 */
    fprintf(stderr, "FAILED: (U061604) geda_stristr <%d>\n", value);
    result++;
  }

  /* === Function 17: geda_utility_string_strisubst === */

  char str_170[32] = {"The quick Brown fox\0"};

  string = geda_strisubst(NULL, "Green", "Red");
  if (string != NULL) {                   /* Arg1 NULL */
    fprintf(stderr, "FAILED: (U061700A) geda_strisubst <%s>\n", string);
    result++;
  }

  string = geda_strisubst(str_170, NULL, "Red");
  if (string != NULL) {                   /* Arg2 NULL */
    fprintf(stderr, "FAILED: (U061700B) geda_strisubst <%s>\n", string);
    result++;
  }

  string = geda_strisubst(str_170, "red", NULL);
  if (string != NULL) {                   /* Arg3 NULL */
    fprintf(stderr, "FAILED: (U061700C) geda_strisubst <%s>\n", string);
    result++;
  }

  string = geda_strisubst(str_170, "brown", "Silver");
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061701) geda_strisubst NULL\n");
    result++;
  }

  /* Brown is now Silver */

  string = geda_strisubst(str_170, "Silver", "Grey");
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061702) geda_strisubst NULL\n");
    result++;
  }

  if (strcmp(str_170, "The quick Grey fox")) {
    fprintf(stderr, "FAILED: (U061703) geda_strisubst <%s>\n", str_170);
    result++;
  }

  /* === Function 18: geda_utility_string_strncmpi === */

  value = geda_strncmpi(NULL, NULL, 3);
  if (value != -2) {                      /* NULL input */
    fprintf(stderr, "FAILED: (U061800A) geda_strncmpi <%d>\n", value);
    result++;
  }
  if (errno != EINVAL) {                  /* err must be set */
    fprintf(stderr, "FAILED: (U061800B) errno != EINVAL\n");
    result++;
  }

  errno = 0; /* Reset */

 /* equal, same length, index more than string okay */
  value = geda_strncmpi(str1, str1, 99);  /* equal */
  if (value != 0) {
    fprintf(stderr, "FAILED: (U061801) geda_strncmpi <%d>\n", value);
    result++;
  }

  /* equal, not same length, index more than string not okay */
  value = geda_strncmpi(str1, str4, 99);  /* not equal */
  if (value != 1) {
    fprintf(stderr, "FAILED: (U061802) geda_strncmpi <%d>\n", value);
    result++;
  }

  /* "Dog" "doo" n = 3 */
  value = geda_strncmpi(str1, str2, 3);   /* str1 greater */
  if (value != 1) {
    fprintf(stderr, "FAILED: (U061803) geda_strncmpi <%d>\n", value);
    result++;
  }

  /* "Dog" "DOGBONE" n = 3 */
  value = geda_strncmpi(str1, str4, 3);   /* equal */
  if (value != 0) {
    fprintf(stderr, "FAILED: (U061804) geda_strncmpi <%d>\n", value);
    result++;
  }

  value = geda_strncmpi(str3, str4, 3);   /* str2 greater */
  if (value != -1) {
    fprintf(stderr, "FAILED: (U061805) geda_strncmpi <%d>\n", value);
    result++;
  }

  /* === Function 19: geda_utility_string_strndup === */

  string = geda_strndup(NULL, 5);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U061900A) geda_strndup <%s>\n", string);
    result++;
    free(string);
  }

  string = geda_strndup(str3, -5);
  if (string) {                           /* Bad/invalid input */
    fprintf(stderr, "FAILED: (U061900B) geda_strndup <%s>\n", string);
    result++;
    free(string);
  }

  string = geda_strndup(str3, 99);
  if (string) {
    if (strcmp(string, str3) != 0) {      /* 9 > input */
      fprintf(stderr, "FAILED: (U061901A) geda_strndup <%s>\n", string);
      result++;
    }
    free(string);
  }
  else {
    fprintf(stderr, "FAILED: (U061901B) geda_strndup returned NULL\n");
    result++;
  }

  string = geda_strndup(str3, 5);
  if (string) {
    if (strcmp(string, "hound") != 0) {   /* hound from hounddog */
      fprintf(stderr, "FAILED: (U061902A) geda_strndup <%s>\n", string);
      result++;
    }
    free(string);
  }
  else {
    fprintf(stderr, "FAILED: (U061902B) geda_strndup returned NULL\n");
    result++;
  }

  string = geda_strndup(str3, 0);
  if (string) {
    if (strcmp(string, "") != 0) {   /* Zero length string */
      fprintf(stderr, "FAILED: (U061903A) geda_strndup <%s>\n", string);
      result++;
    }
    free(string);
  }
  else {
    fprintf(stderr, "FAILED: (U061903B) geda_strndup returned NULL\n");
    result++;
  }

  /* === Function 20: geda_utility_string_strsize === */

/* The vsnprintf on MinGW32 platforms does catch a NULL argument */
#ifndef __MINGW32__

  count = test_string_strsize (NULL, str_120);

  if (count >= 0) {
    fprintf(stderr, "FAILED: (U062000A) geda_strstr_rep <%d>\n", count);
    result++;
  }

#endif

  count = test_string_strsize ("", str_120);

  if (count != 0) {
    fprintf(stderr, "FAILED: (U062000B) geda_strstr_rep <%d>\n", count);
    result++;
  }

  count = test_string_strsize ("%s %s", str_120, str_121);

  if (count != (strlen(str_120) + strlen(str_121) + 1)) {
    fprintf(stderr, "FAILED: (U062001) geda_strstr_rep <%d>\n", count);
    result++;
  }

    /* See function geda_utility_string_sprintf */

  /* === Function 21: geda_utility_string_strstr_rep === */

  char str_21[32] = {"The  quick  brown  fox\0"};

  string = geda_strstr_rep(NULL, "fox", "dog");
  if (string) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U062100A) geda_strstr_rep <%s>\n", string);
    result++;
  }

  string = geda_strstr_rep(str_21, NULL, "dog");
  if (string) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U062100B) geda_strstr_rep <%s>\n", string);
    result++;
  }

  string = geda_strstr_rep(str_21, "fox", NULL);
  if (string) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U062100B) geda_strstr_rep <%s>\n", string);
    result++;
  }

  string = geda_strstr_rep(str_21, "brown", "grey ");
  if (!string) {                            /* simple replacement */
    fprintf(stderr, "FAILED: (U062101A) geda_strstr_rep returned NULL\n");
    result++;
  }
  else {
    if (strcmp(str_21, "The  quick  grey   fox") != 0) {
      fprintf(stderr, "FAILED: (U062101B) geda_strstr_rep <%s>\n", str_21);
      result++;
    }
  }

  string = geda_strstr_rep(str_21, "  ", " ");
  if (!string) {                            /* Shrink space */
    fprintf(stderr, "FAILED: (U062102A) geda_strstr_rep returned NULL\n");
    result++;
  }
  else {
    if (strcmp(str_21, "The quick grey fox") != 0) {
      fprintf(stderr, "FAILED: (U062102B) geda_strstr_rep <%s>\n", str_21);
      result++;
    }
  }

  /* === Function 22: geda_utility_string_strsubst === */

  char str_22[32] = {"The quick Brown fox\0"};

  string = geda_strsubst(NULL, "Purple", "Red");
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U062200A) geda_strsubst <%s>\n", string);
    result++;
  }

  string = geda_strsubst(str_22, NULL, "Red");
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U062200B) geda_strsubst <%s>\n", string);
    result++;
  }

  string = geda_strsubst(str_22, "Brown", NULL);
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U062200C) geda_strsubst <%s>\n", string);
    result++;
  }

  string = geda_strsubst(str_22, "Brown", "Red");
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U062201) geda_strsubst NULL\n");
    result++;
  }

  /* Brown is now Red */
  string = geda_strsubst(str_22, "Brown", "Red");
  if (string != NULL) {                   /* "Brown" not string */
    fprintf(stderr, "FAILED: (U062202) geda_strsubst <%s>\n", string);
    result++;
  }

  /* === Function 23: geda_utility_string_strtrim === */

  static const struct _TestData U23_str[] =
  {
    { "",                        "" },
    { " metformin ",             "metformin" },
    { "\nimmuno-ag ",            "immuno-ag" },
    { " \t3-bromopyruvate\t",    "3-bromopyruvate" },
    { "  Dichloroacetate ",      "Dichloroacetate" },
    { "Asparagopsis taxiformis", "Asparagopsis taxiformis" },
    { "     ",                   "" }
  };

  if (geda_strtrim (NULL)) {
    fprintf(stderr, "FAILED: (U062300) geda_strtrim NULL\n");
    result++;
  };

  count = sizeof(U23_str) / sizeof(struct _TestData);

  for (index = 1; index < count; index++) {

    char *expected = U23_str[index].expected;
    char *input    = U23_str[index].input;
    char *output;

    output = geda_strtrim(input);

    if (!output) {
      fprintf(stderr, "FAILED: (U062301A-%d) geda_strtrim\n", index);
      result++;
    }
    else {
      if (strcmp(output, expected)) {      /* See structure U23_str */
        fprintf(stderr, "FAILED: (U062301B-%d) geda_strtrim <%s>\n", index, output);
        result++;
      }
      free(output);
    }
  }

  /* === Function 24: geda_utility_string_word_count === */

  value = geda_word_count(NULL);   /* NULL input */
  if (value != -1) {
    fprintf(stderr, "FAILED: (U062400) geda_word_count <%d> \n", value);
    result++;
  }

  value = geda_word_count(str_22);   /* reuse */
  if (value != 4) {
    fprintf(stderr, "FAILED: (U062401) geda_word_count <%d> \n", value);
    result++;
  }

  return result;
}

/** @} endgroup test-utility-geda-string */

int
main (int argc, char *argv[])
{
  int result = 0;

  const char *msg_signal;

  msg_signal = "Caught signal checking utility group 1 src/utility/%s\n\n";

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (setjmp(point) == 0) {
      result = test_utility();
  }
  else {
    fprintf(stderr, msg_signal, "u_utility.c");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_glist();
  }
  else {
    fprintf(stderr, msg_signal, "u_glist.c");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_log();
  }
  else {
    fprintf(stderr, msg_signal, "u_log.c");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_program();
  }
  else {
    fprintf(stderr, msg_signal, "u_program.c");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_refdes();
  }
  else {
    fprintf(stderr, msg_signal, "u_refdes.c");
    result++;
  }

  if (setjmp(point) == 0) {
    result += test_strings();
  }
  else {
    fprintf(stderr, msg_signal, "u_string.c");
    result++;
  }
  return result;
}
