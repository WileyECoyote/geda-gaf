/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_completion.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: May 5th, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_completion.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_completion.c"

#define TWIDGET "GedaCompletion"

/*! \file test_completion.c
 *  \brief Tests for geda_completion.c module
 */

int check_completion (void)
{
  int result = 0;
  int count;
  char *new_prefix;

  GedaCompletion *cmp = geda_completion_new(NULL);

  GList *l_list,*u_list, *words;

  static char *lower[] = {
                          "alpha",
                          "beta",
                          "gamma",
                          "delta",
                          "epsilon",
                          NULL};

  static char *upper[] = {
                          "Alpha",
                          "Beta",
                          "Gamma",
                          "Delta",
                          "Epsilon",
                          NULL};
  l_list = NULL;

  int i;

  for (i = 0; lower[i] != NULL; i++) {
    l_list = g_list_append(l_list, lower[i]);
  }

  geda_completion_add_items (cmp, l_list);

  words = geda_completion_complete (cmp, "al", &new_prefix);

  if (!words) {
    fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    if (strcmp(new_prefix, "alpha")) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, new_prefix);
      result++;
    }
    g_free(new_prefix);

    count = g_list_length(words);

    if (count != 1) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %d\n", __LINE__, count);
    }
    else {

      char *str = words->data;

      if (!str) {
        fprintf(stderr, "FAILED: line <%d> completion_complete no word\n", __LINE__);
        result++;
      }
      else {
        if (strcmp(str, "alpha")) {
          fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, str);
          result++;
        }
      }
    }
  }

  words = geda_completion_complete (cmp, "epsil", &new_prefix);

  if (!words) {
    fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    if (strcmp(new_prefix, "epsilon")) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, new_prefix);
      result++;
    }

    count = g_list_length(words);

    if (count != 1) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %d\n", __LINE__, count);
      result++;
    }
    else {

      char *str = words->data;

      if (!str) {
        fprintf(stderr, "FAILED: line <%d> completion_complete no word\n", __LINE__);
        result++;
      }
      else {
        if (strcmp(str, "epsilon")) {
          fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, str);
          result++;
        }
      }
    }
    g_free(new_prefix);
  }

  if (!result) { /* continue checking */

    u_list = NULL;

    for (i = 0; upper[i] != NULL; i++) {
      u_list = g_list_append(u_list, upper[i]);
    }

    geda_completion_add_items (cmp, u_list);

    words = geda_completion_complete_utf8 (cmp, "Del", &new_prefix);

    if (!words) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, TWIDGET);
      result++;
    }

    if (strcmp(new_prefix, "Delta")) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, new_prefix);
      result++;
    }

    count = g_list_length(words);

    if (count != 1) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %d\n", __LINE__, count);
      result++;
    }

    geda_completion_remove_items  (cmp, l_list);

    words = geda_completion_complete (cmp, "bet", &new_prefix);

    if (words) {
      fprintf(stderr, "FAILED: line <%d> completion_remove_items\n", __LINE__);
      result++;
    }

    geda_completion_add_items (cmp, l_list);

    g_list_free(u_list);
  }

  words = geda_completion_complete (cmp, "mu", &new_prefix);

  if (words) {
    fprintf(stderr, "FAILED: line <%d> completion_complete no mu\n", __LINE__);
    result++;
  }

  geda_completion_clear_items (cmp);

  words = geda_completion_complete (cmp, "epsil", &new_prefix);

  if (words) {
    fprintf(stderr, "FAILED: line <%d> completion_clear_items\n", __LINE__);
    result++;
  }

  l_list = g_list_append(l_list, "alabama");

  geda_completion_add_items (cmp, l_list);

  words = geda_completion_complete (cmp, "al", &new_prefix);

  if (!words) {
    fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    if (strcmp(new_prefix, "al")) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %s\n", __LINE__, new_prefix);
      result++;
    }
    g_free(new_prefix);

    count = g_list_length(words);

    if (count != 2) {
      fprintf(stderr, "FAILED: line <%d> completion_complete %d\n", __LINE__, count);
      result++;
    }
  }

  g_list_free(l_list);

  geda_completion_free(cmp); /* Does not destroy completion */

  return result;
}

int
main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (gtk_init_check(&argc, &argv)) {

    if (setjmp(point) == 0) {
      result = check_completion();
    }
    else {
      fprintf(stderr, "Caught signal checking %s\n\n", MUT);
    }
  }
  return result;
}
