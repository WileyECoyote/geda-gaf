/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_bulb.c
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
 * Date: April, 13, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include <../include/geda_combobox.h>
#include <../include/geda_bulb.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_bulb.c"

#define TWIDGET "GedaBulb"

/*! \file test_bulb.c
 *  \brief Tests for geda_bulb.c module
 */

int check_construction (void)
{
  int result = 0;
  int value;

  /* geda_bulb_new */

  GtkWidget *widget = geda_bulb_new(NULL);

  if (!GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GTK_IS_CHECK_BUTTON(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_unref(widget);    /* Destroy the widget */

  if (GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TWIDGET);
    result++;
  }

  widget = NULL;

  /* geda_bulb_new_visible */

  widget = geda_bulb_new_visible(NULL);

  if (!GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_get (widget, "visible", &value, NULL);

  if (!value) {
    fprintf(stderr, "FAILED: line <%d> is visible %s\n", __LINE__, TWIDGET);
    result++;
    value = 0;
  }

  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_bulb_new_with_label */

  widget = geda_bulb_new_with_label (NULL, "b0");

  if (!GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_bulb_new_visible_with_label */

  widget = geda_bulb_new_visible_with_label (NULL, "b0");

  if (!GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_get (widget, "visible", &value, NULL);

  if (!value) {
    fprintf(stderr, "FAILED: line <%d> is visible %s\n", __LINE__, TWIDGET);
    result++;
    value = 0;
  }

  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_bulb_new_with_mnemonic */

  widget = geda_bulb_new_with_mnemonic (NULL, "b0");

  if (!GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  GtkWidget *widget1;
  GtkWidget *widget2;
  GtkWidget *widget3;

  /* geda_bulb_new_visible_with_mnemonic */

  widget1 = geda_bulb_new_visible_with_mnemonic (NULL, "</b>b1</b>");

  if (!GEDA_IS_BULB(widget1)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_get (widget1, "visible", &value, NULL);

  if (!value) {
    fprintf(stderr, "FAILED: line <%d> is visible %s\n", __LINE__, TWIDGET);
    result++;
    value = 0;
  }

  /* geda_bulb_new_with_label_from_widget */

  widget2 = geda_bulb_new_with_label_from_widget (widget1, "b2", TRUE);

  if (!GEDA_IS_BULB(widget2)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  widget3 = geda_bulb_new_with_mnemonic_from_widget (widget2, "</b>b3</b>", TRUE);

  if (!GEDA_IS_BULB(widget3)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  GSList *group = geda_bulb_get_group(widget1);

  if (!group) {
    fprintf(stderr, "FAILED: line <%d> %s group\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    int count = g_slist_length(group);

    if (count != 3) {
      fprintf(stderr, "FAILED: line <%d> %s group count <%d>\n", __LINE__, TWIDGET, count);
      result++;
    }
  }

  g_object_unref(widget1);    /* Destroy widget1 */
  g_object_unref(widget2);    /* Destroy widget2 */
  g_object_unref(widget3);    /* Destroy widget3 */

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
      return 1;
    }

  }
  return result;
}
