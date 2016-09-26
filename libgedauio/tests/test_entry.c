/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_entry.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of the
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
 * Date: April, 15, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include <../include/geda_entry.h>
#include <../include/geda_bulb.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_entry.c"

#define TWIDGET "GedaEntry"

/*! \file test_entry.c
 *  \brief Tests for geda_entry.c module
 */

int check_construction (void)
{
  int result = 0;
  int value;

  /* geda_entry_new */

  GtkWidget *widget = geda_entry_new (NO_HISTORY, NO_COMPLETION);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GTK_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  if (GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TWIDGET);
    result++;
  }

  widget = NULL;

  /* geda_entry_new_visible */

  widget = geda_entry_new_visible (NO_HISTORY, NO_COMPLETION);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_get (widget, "visible", &value, NULL);

  if (!value) {
    fprintf(stderr, "FAILED: line <%d> is visible %s\n", __LINE__, TWIDGET);
    result++;
    value = 0;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_entry_new_with_buffer */

  GtkEntryBuffer *buffer;

  widget = geda_entry_new_with_buffer (NULL);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  buffer = gtk_entry_buffer_new ("abc", 6);

  widget = geda_entry_new_with_buffer (buffer);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_entry_new_with_max_length */

  widget = geda_entry_new_with_max_length (10);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

int
check_accessors ()
{
  int result = 0;
  int value;

  GtkWidget *widget = geda_entry_new_with_max_length (10);

  /* Check activates-default */

  /* uses geda_entry_get_activates_default */
  value = geda_entry_widget_get_activates_default(widget);

  if (value) { /* default is FALSE */
    fprintf(stderr, "FAILED: %s line <%d> activates-default %d\n", TWIDGET, __LINE__, value);
    result++;
  }

  /* uses geda_entry_set_activates_default */
  geda_entry_widget_set_activates_default(widget, TRUE);

  value = geda_entry_widget_get_activates_default(widget);

  if (!value) {
    fprintf(stderr, "FAILED: %s line <%d> activates-default %d\n", TWIDGET, __LINE__, value);
    result++;
  }

  /* Check input-case */

  value = geda_entry_widget_get_input_case(widget);

  if (value != BOTH_CASES) { /* default setting */
    fprintf(stderr, "FAILED: %s line <%d> input-case %d\n", TWIDGET, __LINE__, value);
    result++;
  }

  geda_entry_widget_set_input_case(widget, 3);  /* Invalid value */

  value = geda_entry_widget_get_input_case(widget);

  if (value != BOTH_CASES) {
    fprintf(stderr, "FAILED: %s line <%d> input-case %d != VALID\n", TWIDGET, __LINE__, value);
    result++;
  }

  geda_entry_widget_set_input_case(widget, LOWER_CASE);

  value = geda_entry_widget_get_input_case(widget);

  if (value != 0) {
    fprintf(stderr, "FAILED: %s line <%d> input-case %d != LOWER\n", TWIDGET, __LINE__, value);
    result++;
  }

  geda_entry_widget_set_input_case(widget, UPPER_CASE);

  value = geda_entry_widget_get_input_case(widget);

  if (value != UPPER_CASE) {
    fprintf(stderr, "FAILED: %s line <%d> input-case %d != UPPER\n", TWIDGET, __LINE__, value);
    result++;
  }

  geda_entry_widget_set_input_case(widget, BOTH_CASES);

  value = geda_entry_widget_get_input_case(widget);

  if (value != BOTH_CASES) {
    fprintf(stderr, "FAILED: %s line <%d> input-case %d != BOTH\n", TWIDGET, __LINE__, value);
    result++;
  }

  /* Check max-history */

  geda_entry_widget_set_max_history(widget, 8);

  value = geda_entry_widget_get_max_history(widget);

  if (value != 8) {
    fprintf(stderr, "FAILED: %s line <%d> max-history %d != 10\n", TWIDGET, __LINE__, value);
    result++;
  }

  /* Check max-length */

  value = geda_entry_widget_get_max_length(widget);

  if (value != 10) {
    fprintf(stderr, "FAILED: %s line <%d> max_length %d != 10\n", TWIDGET, __LINE__, value);
    result++;
  }

  geda_entry_widget_set_max_length(widget, 9);

  value = geda_entry_widget_get_max_length(widget);

  if (value != 9) {
    fprintf(stderr, "FAILED: %s line <%d> max_length %d != 9\n", TWIDGET, __LINE__, value);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

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
      result = check_construction();
    }
    else {
      fprintf(stderr, "Caught signal checking constructors in %s\n\n", MUT);
      result++;
    }

    if (!result) {

      if (setjmp(point) == 0) {
        result = check_accessors();
      }
      else {
        fprintf(stderr, "Caught signal checking accessors in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
