/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_label.c
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
 * Date: May, 27, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_label.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_label.c"

#define TWIDGET "GedaToolbar"

/*! \file test_label.c
 *  \brief Tests for geda_label.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_label_new(0);

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GTK_IS_MISC(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_mnemonic_label_new */

  widget = geda_mnemonic_label_new("<b>Harmonic</b>");

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_visible_label_new */

  widget = geda_visible_label_new("<b>Harmonic</b>");

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_visible_mnemonic_label_new */

  widget = geda_visible_mnemonic_label_new("<b>Harmonic</b>");

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_aligned_label_new */

  widget = geda_aligned_label_new("<b>Harmonic</b>", 10, 20);

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_aligned_visible_label_new */

  widget = geda_aligned_visible_label_new("<b>Harmonic</b>", 5, 15);

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_aligned_mnemonic_label_new */

  widget = geda_aligned_mnemonic_label_new("<b>Harmonic</b>", 11, 22);

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_aligned_visible_mnemonic_label_new */

  widget = geda_aligned_visible_mnemonic_label_new("<b>Harmonic</b>", 12, 24);

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  return result;
}

int
check_accessors ()
{
  int result = 0;
  const char *text;

  GtkWidget *widget = geda_label_new(0);
  GedaLabel *label  = GEDA_LABEL(widget);

  /* geda_label_widget_set_text */
  geda_label_widget_set_text(widget, "hippopotamus");

  /* geda_label_widget_get_text */
  text = geda_label_widget_get_text(widget);

  if (!text) {
    fprintf(stderr, "FAILED: line <%d> set_text %s\n", __LINE__, TWIDGET);
    result++;
  }
  else if (strcmp(text, "hippopotamus")) {
    fprintf(stderr, "FAILED: line <%d> set/get text <%s>\n", __LINE__, text);
    result++;
  }

  /* geda_label_set_label */
  geda_label_set_label(label, "rhinoceros");

  /* geda_label_get_label */
  text = geda_label_get_label(label);

  if (!text) {
    fprintf(stderr, "FAILED: line <%d> set label %s\n", __LINE__, TWIDGET);
    result++;
  }
  else if (strcmp(text, "rhinoceros")) {
    fprintf(stderr, "FAILED: line <%d> set/get label <%s>\n", __LINE__, text);
    result++;
  }

  PangoAttrList *attrs;

  attrs = geda_label_get_attributes (label);

  geda_label_set_attributes (label, attrs);

  /* use_markup property */

  geda_label_widget_set_use_markup (widget, FALSE);

  /* geda_label_get_use_markup */
  if (geda_label_widget_get_use_markup (widget)) {
    fprintf(stderr, "FAILED: line <%d> get/set use_markup %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* geda_label_widget_set_use_markup */
  geda_label_widget_set_use_markup (widget, TRUE);

  if (!geda_label_widget_get_use_markup (widget)) {
    fprintf(stderr, "FAILED: line <%d> get/set use_markup %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* use_underline property */

  geda_label_widget_set_use_underline (widget, FALSE);

  /* geda_label_widget_get_use_underline */
  if (geda_label_widget_get_use_underline (widget)) {
    fprintf(stderr, "FAILED: line <%d> get/set use_underline %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* geda_label_widget_set_use_underline */
  geda_label_widget_set_use_underline (widget, TRUE);

  if (!geda_label_widget_get_use_underline (widget)) {
    fprintf(stderr, "FAILED: line <%d> get/set use_underline %s\n", __LINE__, TWIDGET);
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
