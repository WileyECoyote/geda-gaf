/* -*- test_utility.c -*-
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
  int     result;
  int     value;

  result = 0;

  /* Create a string table to load into list, will need to copy
   * strings for free all */

  /* === Function 01: geda_utility_glist_clear === */
  dlist = NULL;
  dlist = geda_clear_glist(NULL);
  if (dlist) {
    fprintf(stderr, "FAILED: (U020100) geda_clear_glist <%p>\n", dlist);
    result++;
  }

  /* === Function 02: geda_utility_glist_find_string === */
  value = geda_glist_find_string(NULL, NULL) ;
  if (value != -2) {
    fprintf(stderr, "FAILED: (U020200) geda_glist_find_string %d\n", value);
    result++;
  }

  /* === Function 03: geda_utility_glist_free_all === */
  geda_glist_free_all(NULL);

  /* === Function 04: geda_utility_glist_free_full === */
  geda_glist_free_full(NULL, free);

  /* === Function 05: geda_utility_gslist_str_inlist === */
  if (geda_glist_str_inlist(NULL, NULL)) {
    fprintf(stderr, "FAILED: (U020500) geda_glist_str_inlist NULL\n");
    result++;
  }

  /* === Function 06: geda_utility_glist_stri_inlist === */
  if (geda_glist_stri_inlist(NULL, NULL)) {
    fprintf(stderr, "FAILED: (U020600) geda_glist_stri_inlist NULL\n");
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
/** @} endgroup test-utility-geda-log */

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

const char *log_mess;

void test_log_message (const char     *log_domain,
                       GLogLevelFlags  log_level,
                       const char     *message)
{
  log_mess = message;
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

  geda_set_default_logger(default_log_handler, &default_handler);

  default_handler = 0;

  geda_log ("Testing");

  if (default_handler - 7) {
    fprintf(stderr, "FAILED: (U030801) geda_set_default_logger\n");
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
    if (!strcmp(log_mess, "message 1")) {
      fprintf(stderr, "FAILED: (U031101B) log_set_update_func\n");
      result++;
    }
  }

  log_mess = NULL;
  geda_set_quiet_mode(TRUE);

  /* === Function 06 geda_utility_log_quite === */

  geda_log_q ("message q1");

  if (log_mess) {
    fprintf(stderr, "FAILED: (U030601) log_quite\n");
    result++;
    log_mess = NULL;
  }

  /* === Function 10 geda_utility_log_set_quiet_mode === */

  geda_set_quiet_mode(FALSE);

  geda_log_q ("message q2");

  if (!log_mess) {
    fprintf(stderr, "FAILED: (U0306/1001A) set log quiet\n");
    result++;
  }
  else {
    if (!strcmp(log_mess, "message q2")) {
      fprintf(stderr, "FAILED: (U0306/1001B) set log quiet\n");
      result++;
    }
  }

  log_mess = NULL;
  geda_set_verbose_mode(FALSE); /* Is already unset */

  /* === Function 14 geda_utility_log_verbose === */

  geda_log_v ("message v1");

  if (log_mess) {
    fprintf(stderr, "FAILED: (U031401) log_verbose\n");
    result++;
    log_mess = NULL;
  }

  /* === Function 10 geda_utility_log_set_verbose_mode === */

  geda_set_verbose_mode(TRUE);

  geda_log_v ("message v2");

  if (!log_mess) {
    fprintf(stderr, "FAILED: (U0312/1401A) set log verbose\n");
    result++;
  }
  else {
    if (!strcmp(log_mess, "message v2")) {
      fprintf(stderr, "FAILED: (U0312/1401B) set log verbose\n");
      result++;
    }
  }

  /* === Function 07: geda_read_log geda_utility_log_read === */
  /* === Function 13: geda_log_s geda_utility_log_system === */

  /* === Function 01 geda_utility_log_close === */
  geda_close_log();

  log_mess = NULL;

  geda_log ("message 2");

  if (log_mess) {
    fprintf(stderr, "FAILED: (U030101) log close\n");
    result++;
  }

  return result;
}
/** @} endgroup test-utility-geda-program */

/** \defgroup test-utility-geda-program Test GEDA Program Module
 * @{
 * \brief Group 4 src/utility/u_program.c geda_utility_program_
 */
int test_program (void)
{
  /* === Function 01: geda_program_backtrace  geda_utility_program_backtrace === */
  /* === Function 02: geda_set_memory_vtable  geda_utility_program_mem_set_vtable === */
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

  /* === Function 01: geda_refdes_get_ieee      geda_utility_refdes_get_ieee === */
  /* === Function 02: geda_refdes_get_standard  geda_utility_refdes_get_standard  === */
  /* === Function 03: geda_refdes_get_spice     geda_utility_refdes_get_spice === */

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

  count = sizeof (U04_str) / sizeof (struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U04_str[index].expected;
    char *input    = U04_str[index].input;

    GedaObject *object04 =  o_text_new(3,                       /* color */
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

  count = sizeof (U05_str) / sizeof (struct _TestData);

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
      fprintf(stderr, "FAILED: (U050502B-%d) geda_refdes_get_numeric NULL\n",index);
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

  string = geda_strconcat(str1, " ", str2, " ",str2, NULL);
  if (strcmp(string, "Dog doo doo")) {    /* Dog in DOGBONE */
    fprintf(stderr, "FAILED: (U060101) geda_strconcat <%s>\n", string);
    result++;
  }

  string = geda_strconcat(str1, str2, NULL);
  if (strcmp(string, "Dogdoo")) {         /* Dog in DOGBONE */
    fprintf(stderr, "FAILED: (U060101) geda_strconcat <%s>\n", string);
    result++;
  }

  /* === Function 02: geda_utility_string_int2str === */

  char U02_val[10];

  string = geda_string_int2str(0, NULL, 10);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060200) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(0, U02_val, 10);
  if (strcmp(string, "0")) {              /* Zero */
    fprintf(stderr, "FAILED: (U060201) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(123456789, U02_val, 10);
  if (strcmp(string, "123456789")) {      /* 9 digits plus NULL */
    fprintf(stderr, "FAILED: (U060202) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(-12345678, U02_val, 10);
  if (strcmp(string, "-12345678")) {      /* Neg 8 digits plus NULL */
    fprintf(stderr, "FAILED: (U060203) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(345.6, U02_val, 10);
  if (strcmp(string, "345")) {            /* Only integers returned */
    fprintf(stderr, "FAILED: (U060204) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(16, U02_val, 16);
  if (strcmp(string, "10")) {             /* Is hex */
    fprintf(stderr, "FAILED: (U060205) geda_string_int2str <%s>\n", string);
    result++;
  }

  string = geda_string_int2str(16, U02_val, 8);
  if (strcmp(string, "20")) {             /* 16 Dec = 20 base 8 */
    fprintf(stderr, "FAILED: (U060206) geda_string_int2str <%s>\n", string);
    result++;
  }

  /* === Function 03: geda_utility_string_isalnum === */

  value = geda_utility_string_isalnum(NULL);
  if (value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060300) geda_utility_string_isalnum\n");
    result++;
  }

  value = geda_utility_string_isalnum("60");
  if (!value) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060301) geda_utility_string_isalnum <%d>\n", value);
    result++;
  }

  value = geda_utility_string_isalnum(str4);
  if (!value) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060302) geda_utility_string_isalnum <%d>\n", value);
    result++;
  }

  value = geda_utility_string_isalnum("$#!");
  if (value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060303) geda_utility_string_isalnum <%d>\n", value);
    result++;
  }

  /* === Function 04: geda_utility_string_istr  === */

  const char *ptr;

  ptr = geda_string_istr(NULL, NULL);
  if (ptr) {                              /* NULL input */
    fprintf(stderr, "FAILED: (U060400) geda_string_istr <%s>\n", ptr);
    result++;
  }

  ptr = geda_string_istr(str2, str1);
  if (ptr) {                              /* doo not in dog */
    fprintf(stderr, "FAILED: (U060401) geda_string_istr NULL\n");
    result++;
  }

  ptr = geda_string_istr(str3, str1);
  if (!ptr) {                             /* NULL returned */
    fprintf(stderr, "FAILED: (U060402A) geda_string_istr NULL\n");
    result++;
  }
  else if (strcmp(ptr, "dog")) {          /* Dog in hounddog */
    fprintf(stderr, "FAILED: (U060402B) geda_string_istr <%s>\n", ptr);
    result++;
  }

  ptr = geda_string_istr(str4, str1);
  if (!ptr) {                             /* NULL returned */
    fprintf(stderr, "FAILED: (U060403A) geda_string_istr NULL\n");
    result++;
  }
  else if (strcmp(ptr, "DOGBONE")) {      /* Dog in DOGBONE */
    fprintf(stderr, "FAILED: (U060403B) geda_string_istr <%s>\n", ptr);
    result++;
  }

  /* === Function 05: geda_utility_string_parse_xy === */

  int X05, Y05;

  value = geda_string_parse_xy(NULL, &X05, &Y05);
  if (value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060500A) geda_string_parse_xy <%d>\n", value);
    result++;
  }

  X05 = Y05 = 1;

  value = geda_string_parse_xy("0,0", NULL, &Y05);
  if (!value) {                            /* NULL X, Y okay */
    fprintf(stderr, "FAILED: (U060500B1) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (Y05 != 0) {
      fprintf(stderr, "FAILED: (U060500B2) geda_string_parse_xy <%d>\n", Y05);
      result++;
    }
  }

  X05 = Y05 = 1;

  value = geda_string_parse_xy("0,0", &X05, NULL);
  if (!value) {                            /* NULL Y, X okay */
    fprintf(stderr, "FAILED: (U060500C1) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X05 != 0) {
      fprintf(stderr, "FAILED: (U060500C2) geda_string_parse_xy <%d>\n", X05);
      result++;
    }
  }

  X05 = Y05 = 1;

  value = geda_string_parse_xy("0,0", &X05, &Y05);
  if (!value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060501A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X05 != 0) {
      fprintf(stderr, "FAILED: (U060501B) geda_string_parse_xy <%d>\n", value);
      result++;
    }
    if (Y05 != 0) {
      fprintf(stderr, "FAILED: (U060501C) geda_string_parse_xy <%d>\n", Y05);
      result++;
    }
  }

  X05 = Y05 = 1;

  value = geda_string_parse_xy("(0,0)", &X05, &Y05);
  if (!value) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U060502A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X05 != 0) {
      fprintf(stderr, "FAILED: (U060502B) geda_string_parse_xy <%d>\n", value);
      result++;
    }
    if (Y05 != 0) {
      fprintf(stderr, "FAILED: (U060502C) geda_string_parse_xy <%d>\n", Y05);
      result++;
    }
  }

  X05 = Y05 = 1;

  value = geda_string_parse_xy("4500 380", &X05, &Y05);
  if (!value) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060503A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X05 != 4500) {
      fprintf(stderr, "FAILED: (U060503B) geda_string_parse_xy <%d>\n", X05);
      result++;
    }
    if (Y05 != 380) {
      fprintf(stderr, "FAILED: (U060503C) geda_string_parse_xy <%d>\n", Y05);
      result++;
    }
  }

  X05 = Y05 = 1;

  value = geda_string_parse_xy("2200", &X05, &Y05);
  if (!value) {                           /* No y input, x valid */
    fprintf(stderr, "FAILED: (U060504A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X05 != 2200) {
      fprintf(stderr, "FAILED: (U060504B) geda_string_parse_xy <%d>\n", X05);
      result++;
    }
    if (Y05 != 0) {
      fprintf(stderr, "FAILED: (U060504C) geda_string_parse_xy <%d>\n", Y05);
      result++;
    }
  }

  X05 = Y05 = 1;

  value = geda_string_parse_xy(",313", NULL, &Y05);
  if (!value) {                           /* NULL x io, y valid */
    fprintf(stderr, "FAILED: (U060505A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X05 != 1) {
      fprintf(stderr, "FAILED: (U060505B) geda_string_parse_xy <%d>\n", X05);
      result++;
    }
    if (Y05 != 313) {
      fprintf(stderr, "FAILED: (U060505C) geda_string_parse_xy <%d>\n", Y05);
      result++;
    }
  }

  X05 = Y05 = 1;

  value = geda_string_parse_xy("", &X05, &Y05);
  if (value) {                            /* empty string */
    fprintf(stderr, "FAILED: (U060505A) geda_string_parse_xy <%d>\n", value);
    result++;
  }
  else {
    if (X05 != 1) {
      fprintf(stderr, "FAILED: (U060505B) geda_string_parse_xy <%d>\n", X05);
      result++;
    }
    if (Y05 != 1) {
      fprintf(stderr, "FAILED: (U060505C) geda_string_parse_xy <%d>\n", Y05);
      result++;
    }
  }

  /* === Function 06: geda_utility_string_remove_last_nl   === */

  static const struct _TestData U06_str[] =
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
    fprintf(stderr, "FAILED: (U060600) geda_remove_last_newline <%s>\n", string);
    result++;
  }

  count = sizeof (U06_str) / sizeof (struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U06_str[index].expected;
    char *input    = geda_strdup (U06_str[index].input);

    string = geda_remove_last_newline (input);

    if (string) {
      if (strcmp(string, expected)) {      /* See structure U06_str */
        fprintf(stderr, "FAILED: (U060601A-%d) geda_remove_last_newline <%s>\n",index, string);
        result++;
      }
      free (string);
    }
    else {
      if (strcmp(string, expected)) {      /* See structure U06_str */
        fprintf(stderr, "FAILED: (U060601B-%d) expected <%s> NULL\n",index, expected);
        result++;
      }
    }
  }

  /* === Function 07: geda_utility_string_remove_nl === */

  static const struct _TestData U07_str[] =
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
    fprintf(stderr, "FAILED: (U060700) geda_remove_newline <%s>\n", string);
    result++;
  }

  count = sizeof (U07_str) / sizeof (struct _TestData);

  for (index = 0; index < count; index++) {

    char *expected = U07_str[index].expected;
    char *input    = geda_strdup (U07_str[index].input);

    string = geda_remove_newline (input);

    if (string) {
      if (strcmp(string, expected)) {      /* See structure U07_str */
        fprintf(stderr, "FAILED: (U060701A-%d) geda_remove_newline <%s>\n",index, string);
        result++;
      }
      free (string);
    }
    else {
      if (strcmp(string, expected)) {      /* See structure U07_str */
        fprintf(stderr, "FAILED: (U060701B-%d) expected <%s> NULL\n",index, expected);
        result++;
      }
    }
  }

  /* === Function 08: geda_utility_string_scm2c === */

  string = geda_utility_string_scm2c(NULL);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U060800) geda_string_scm2c <%s>\n", string);
    result++;
  }

  /* Additional tests not performed because rc files were not processed */

  /* === Function 09: geda_utility_string_sort_array === */

  char *arr_090[] = {"Black", "red", "blue", "pink", "lavender"};

  geda_sort_string_array (arr_090, sizeof(arr_090));
  if (strcmp(arr_090[0], "Black"))
  {
    fprintf(stderr, "FAILED: (U060900A) geda_sort_string_array <%s>\n", arr_090[0]);
    result++;
  }

  if (strcmp(arr_090[1], "blue"))
  {
    fprintf(stderr, "FAILED: (U060900B) geda_sort_string_array <%s>\n", arr_090[1]);
    result++;
  }

  if (strcmp(arr_090[2], "lavender"))
  {
    fprintf(stderr, "FAILED: (U060900C) geda_sort_string_array <%s>\n", arr_090[2]);
    result++;
  }

  if (strcmp(arr_090[3], "pink"))
  {
    fprintf(stderr, "FAILED: (U060900D) geda_sort_string_array <%s>\n", arr_090[3]);
    result++;
  }

  if (strcmp(arr_090[4], "red"))
  {
    fprintf(stderr, "FAILED: (U060900E) geda_sort_string_array <%s>\n", arr_090[4]);
    result++;
  }

  /* === Function 10: geda_utility_string_split === */

  char *str_100 = "Kansas=Topeka=Capital";

  string = geda_strsplit(NULL, '=', 0); /* NULL Argument 1 */
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U061000A) geda_strsplit <%s>\n", string);
    result++;
  }

  string = geda_strsplit(str_100, 0, 0); /* NULL Argument 2 defaults space */
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061000B) geda_strsplit <%s>\n", string);
    result++;
  }
  else {

    /* Orginal string should be returned because there is no space */
    if (strcmp(string, str_100) != 0) {
      fprintf(stderr, "FAILED: (U061000C) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  string = geda_strsplit(str_100, '=', 0);
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061001A) geda_strsplit\n");
    result++;
  }
  else {

    if (strcmp(string, "Kansas") != 0) {
      fprintf(stderr, "FAILED: (U061001B) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  string = geda_strsplit(str_100, '=', 1);
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061002A) geda_strsplit\n");
    result++;
  }
  else {

    if (strcmp(string, "Topeka") != 0) {
      fprintf(stderr, "FAILED: (U061002B) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  string = geda_strsplit(str_100, '=', 2);
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061003A) geda_strsplit\n");
    result++;
  }
  else {

    if (strcmp(string, "Capital") != 0) {
      fprintf(stderr, "FAILED: (U061003B) geda_strsplit <%s>\n", string);
      result++;
    }

    free(string);
  }

  /* Verify that the orginal string was not altered or released */
  if (strcmp(str_100, "Kansas=Topeka=Capital") != 0) {
    fprintf(stderr, "FAILED: (U061004) geda_strsplit <%s>\n", str_100);
    result++;
  }

  /* === Function 11: geda_utility_string_sprintf  geda_sprintf === */

  char *str_110 = "The quick brown fox jumped\0";
  char *str_111 = "over the lazy dog's back\0";

  string = geda_sprintf ("%s %s", str_110, str_111);
  if (string) {
    if (strcmp(string, "The quick brown fox jumped over the lazy dog's back"))
    {
      fprintf(stderr, "FAILED: (U061101A) geda_sprintf <%s>\n", string);
      result++;
    }
    free (string);
  }
  else {
    fprintf(stderr, "FAILED: (U061101B) geda_sprintf returned NULL\n");
    result++;
  }

  string = geda_sprintf ("%s %1.3f meters in %d days", str_110, 3.45, 10);
  if (string) {
    if (strcmp(string, "The quick brown fox jumped 3.450 meters in 10 days"))
    {
      fprintf(stderr, "FAILED: (U061102A) geda_sprintf <%s>\n", string);
      result++;
    }
    free (string);
  }
  else {
    fprintf(stderr, "FAILED: (U061102B) geda_sprintf returned NULL\n");
    result++;
  }

  /* === Function 12: geda_utility_string_strdup === */

  string = geda_strdup(NULL);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U061200) geda_strdup <%s>\n", string);
    result++;
  }

  string = geda_strdup(str3);
  if (string) {
    if (strcmp(string, str3) != 0) {      /* 9 > input */
      fprintf(stderr, "FAILED: (U061201A) geda_strdup <%s>\n", string);
      result++;
    }
    free(string);
  }
  else {
    fprintf(stderr, "FAILED: (U061201B) geda_strdup returned NULL\n");
    result++;
  }

  /* === Function 13: geda_utility_string_strequal === */

  value = geda_strequal(NULL, "due");
  if (value) {                            /* NULL != due */
    fprintf(stderr, "FAILED: (U061300A) geda_strequal <%d>\n", value);
    result++;
  }

  value = geda_strequal("dew", NULL);
  if (value) {                            /* due != NULL */
    fprintf(stderr, "FAILED: (U061300B) geda_strequal <%d>\n", value);
    result++;
  }

  value = geda_strequal("dew", str2);
  if (value) {                            /* dew != doo */
    fprintf(stderr, "FAILED: (U061301) geda_strequal <%d>\n", value);
    result++;
  }

  value = geda_strequal("Doo", "Doo");
  if (!value) {                           /* Doo == Doo */
    fprintf(stderr, "FAILED: (U061302) geda_strequal <%d>\n", value);
    result++;
  }

  /* === Function 14: geda_utility_string_stricmp === */

  value = geda_stricmp(str1, "DOG");
  if (value) {                            /* Dog == DOG */
    fprintf(stderr, "FAILED: (U061401) geda_stricmp <%d>\n", value);
    result++;
  }

  value = geda_stricmp("dew", "due");
  if (!value) {                           /* dew != due */
    fprintf(stderr, "FAILED: (U061402) geda_stricmp <%d>\n", value);
    result++;
  }

  value = geda_stricmp(str2, "Do");
  if (!value) {                           /* doo != Do */
    fprintf(stderr, "FAILED: (U061403) geda_stricmp <%d>\n", value);
    result++;
  }

  /* === Function 15: geda_utility_string_stristr === */

  value = geda_stristr(NULL, "dog");
  if (!value < 0) {                       /* NULL input */
    fprintf(stderr, "FAILED: (U061500A) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr("fox", NULL);
  if (!value < 0) {                       /* NULL input */
    fprintf(stderr, "FAILED: (U061500B) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr(str_110, "fox");
  if (value != 16) {                      /* fox @ position 16 */
    fprintf(stderr, "FAILED: (U061501) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr(str_110, "Dog");
  if (!value < 0) {                       /* Dog not in str_110 */
    fprintf(stderr, "FAILED: (U061502) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr(str_111, "Dog");
  if (value != 14) {                      /* dog @ position 14 */
    fprintf(stderr, "FAILED: (U061503) geda_stristr <%d>\n", value);
    result++;
  }

  value = geda_stristr(str_110, "The");
  if (value != 0) {                       /* The @ position 0 */
    fprintf(stderr, "FAILED: (U061504) geda_stristr <%d>\n", value);
    result++;
  }

  /* === Function 16: geda_utility_string_strisubst === */

  char str_160[32] = {"The quick Brown fox\0"};

  string = geda_strisubst(NULL, "Green", "Red");
  if (string != NULL) {                   /* Arg1 NULL */
    fprintf(stderr, "FAILED: (U061600A) geda_strisubst <%s>\n", string);
    result++;
  }

  string = geda_strisubst(str_160, NULL, "Red");
  if (string != NULL) {                   /* Arg2 NULL */
    fprintf(stderr, "FAILED: (U061600B) geda_strisubst <%s>\n", string);
    result++;
  }

  string = geda_strisubst(str_160, "red", NULL);
  if (string != NULL) {                   /* Arg3 NULL */
    fprintf(stderr, "FAILED: (U061600C) geda_strisubst <%s>\n", string);
    result++;
  }

  string = geda_strisubst(str_160, "brown", "Silver");
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061601) geda_strisubst <%s>\n", string);
    result++;
  }

  /* Brown is now Silver */

  string = geda_strisubst(str_160, "Silver", "Grey");
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U061602) geda_strisubst <%s>\n", string);
    result++;
  }

  if (strcmp(str_160, "The quick Grey fox")) {
    fprintf(stderr, "FAILED: (U061603) geda_strisubst <%s>\n", str_160);
    result++;
  }

  /* === Function 17: geda_utility_string_strncmpi === */

  value = geda_strncmpi(NULL, NULL, 3);
  if (value != -2) {                      /* NULL input */
    fprintf(stderr, "FAILED: (U061700A) geda_strncmpi <%d>\n", value);
    result++;
  }
  if (errno != EINVAL) {                  /* err must be set */
    fprintf(stderr, "FAILED: (U061700B) errno != EINVAL\n");
    result++;
  }

  errno = 0; /* Reset*/

 /* equal, same length, index more than string okay*/
  value = geda_strncmpi(str1, str1, 99);  /* equal */
  if (value != 0) {
    fprintf(stderr, "FAILED: (U061701) geda_strncmpi <%d>\n", value);
    result++;
  }

  /* equal, not same length, index more than string not okay*/
  value = geda_strncmpi(str1, str4, 99);  /* not equal */
  if (value != 1) {
    fprintf(stderr, "FAILED: (U061702) geda_strncmpi <%d>\n", value);
    result++;
  }

  /* "Dog" "doo" n = 3 */
  value = geda_strncmpi(str1, str2, 3);   /* str1 greater */
  if (value != 1) {
    fprintf(stderr, "FAILED: (U061703) geda_strncmpi <%d>\n", value);
    result++;
  }

  /* "Dog" "DOGBONE" n = 3 */
  value = geda_strncmpi(str1, str4, 3);   /* equal */
  if (value != 0) {
    fprintf(stderr, "FAILED: (U061704) geda_strncmpi <%d>\n", value);
    result++;
  }

  value = geda_strncmpi(str3, str4, 3);   /* str2 greater */
  if (value != -1) {
    fprintf(stderr, "FAILED: (U061705) geda_strncmpi <%d>\n", value);
    result++;
  }

  /* === Function 18: geda_utility_string_strndup === */

  string = geda_strndup(NULL, 5);
  if (string) {                           /* NULL input */
    fprintf(stderr, "FAILED: (U061800A) geda_strndup <%s>\n", string);
    result++;
    free(string);
  }

  string = geda_strndup(str3, -5);
  if (string) {                           /* Bad/invalid input */
    fprintf(stderr, "FAILED: (U061800B) geda_strndup <%s>\n", string);
    result++;
    free(string);
  }

  string = geda_strndup(str3, 99);
  if (string) {
    if (strcmp(string, str3) != 0) {      /* 9 > input */
      fprintf(stderr, "FAILED: (U061801A) geda_strndup <%s>\n", string);
      result++;
    }
    free(string);
  }
  else {
    fprintf(stderr, "FAILED: (U061801B) geda_strndup returned NULL\n");
    result++;
  }

  string = geda_strndup(str3, 5);
  if (string) {
    if (strcmp(string, "hound") != 0) {   /* hound from hounddog */
      fprintf(stderr, "FAILED: (U061802A) geda_strndup <%s>\n", string);
      result++;
    }
    free(string);
  }
  else {
    fprintf(stderr, "FAILED: (U061802B) geda_strndup returned NULL\n");
    result++;
  }

  string = geda_strndup(str3, 0);
  if (string) {
    if (strcmp(string, "") != 0) {   /* Zero length string */
      fprintf(stderr, "FAILED: (U061803A) geda_strndup <%s>\n", string);
      result++;
    }
    free(string);
  }
  else {
    fprintf(stderr, "FAILED: (U061803B) geda_strndup returned NULL\n");
    result++;
  }
  /* === Function 19: geda_utility_string_strsize === */

    /* See function geda_utility_string_sprintf */

  /* === Function 20: geda_utility_string_strstr_rep === */

  char str_20[32] = {"The  quick  brown  fox\0"};

  string = geda_strstr_rep(NULL, "fox", "dog");
  if (string) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U062000A) geda_strstr_rep <%s>\n", string);
    result++;
  }

  string = geda_strstr_rep(str_20, NULL, "dog");
  if (string) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U062000B) geda_strstr_rep <%s>\n", string);
    result++;
  }

  string = geda_strstr_rep(str_20, "fox", NULL);
  if (string) {                            /* NULL input */
    fprintf(stderr, "FAILED: (U062000B) geda_strstr_rep <%s>\n", string);
    result++;
  }

  string = geda_strstr_rep(str_20, "brown", "grey ");
  if (!string) {                            /* simple replacement */
    fprintf(stderr, "FAILED: (U062001A) geda_strstr_rep returned NULL\n");
    result++;
  }
  else {
    if (strcmp(str_20, "The  quick  grey   fox") != 0) {
      fprintf(stderr, "FAILED: (U062001B) geda_strstr_rep <%s>\n", str_20);
      result++;
    }
  }

  string = geda_strstr_rep(str_20, "  ", " ");
  if (!string) {                            /* Shrink space */
    fprintf(stderr, "FAILED: (U062002A) geda_strstr_rep returned NULL\n");
    result++;
  }
  else {
    if (strcmp(str_20, "The quick grey fox") != 0) {
      fprintf(stderr, "FAILED: (U062002B) geda_strstr_rep <%s>\n", str_20);
      result++;
    }
  }

  /* === Function 21: geda_utility_string_strsubst === */

  char str_21[32] = {"The quick Brown fox\0"};

  string = geda_strsubst(NULL, "Purple", "Red");
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U062100A) geda_strsubst <%s>\n", string);
    result++;
  }

  string = geda_strsubst(str_21, NULL, "Red");
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U062100B) geda_strsubst <%s>\n", string);
    result++;
  }

  string = geda_strsubst(str_21, "Brown", NULL);
  if (string != NULL) {
    fprintf(stderr, "FAILED: (U062100C) geda_strsubst <%s>\n", string);
    result++;
  }

  string = geda_strsubst(str_21, "Brown", "Red");
  if (string == NULL) {
    fprintf(stderr, "FAILED: (U062101) geda_strsubst <%s>\n", string);
    result++;
  }

  /* Brown is now Red */
  string = geda_strsubst(str_21, "Brown", "Red");
  if (string != NULL) {                   /* "Brown" not string */
    fprintf(stderr, "FAILED: (U062102) geda_strsubst <%s>\n", string);
    result++;
  }

  /* === Function 22: geda_utility_string_word_count === */

  value = geda_word_count(NULL);   /* NULL input */
  if (value != -1) {
    fprintf(stderr, "FAILED: (U062200) geda_word_count <%d> \n", value);
    result++;
  }

  value = geda_word_count(str_21);   /* reuse */
  if (value != 4) {
    fprintf(stderr, "FAILED: (U062201) geda_word_count <%d> \n", value);
    result++;
  }

  return result;
}

/** @} endgroup test-utility-geda-string */

int
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = test_utility();
  result += test_glist();
  result += test_log();
  result += test_program();
  result += test_refdes();
  result += test_strings();

  return result;
}
