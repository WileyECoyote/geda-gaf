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
 * Date: April, 13, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

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

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

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

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_bulb_new_with_label */

  widget = geda_bulb_new_with_label (NULL, "b0");

  if (!GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
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

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_bulb_new_with_mnemonic */

  widget = geda_bulb_new_with_mnemonic (NULL, "_b0");

  if (!GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  GtkWidget *widget1;
  GtkWidget *widget2;
  GtkWidget *widget3;

  /* geda_bulb_new_visible_with_mnemonic */

  widget1 = geda_bulb_new_visible_with_mnemonic (NULL, "</b>_b1</b>");

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

  /* geda_bulb_new_with_mnemonic_from_widget */
  widget3 = geda_bulb_new_with_mnemonic_from_widget (widget2, "</b>_b3</b>", TRUE);

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

  g_object_ref_sink(widget1); /* Sink reference to entry widget */
  g_object_ref_sink(widget2); /* Sink reference to entry widget */
  g_object_ref_sink(widget3); /* Sink reference to entry widget */

  g_object_unref(widget1);    /* Destroy widget1 */
  g_object_unref(widget2);    /* Destroy widget2 */
  g_object_unref(widget3);    /* Destroy widget3 */

  return result;
}

static void callback_toggled (GtkToggleButton *checkbox, void *user_data)
{
  int *index  = (int*)user_data;

  *index = 2;
}

int
check_accessors ()
{
  int result = 0;

  GtkWidget *widget1;
  GtkWidget *widget2;
  GtkWidget *widget3;
  GtkWidget *widget4;

  /* geda_bulb_new_visible_with_mnemonic */

  widget1 = geda_bulb_new_visible_with_mnemonic (NULL, "</b>b1</b>");

  if (!GEDA_IS_BULB(widget1)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GSList *group;
    int     count;

    /* geda_bulb_new_with_label_from_widget */

    widget2 = geda_bulb_new_with_label_from_widget (widget1, "b2", TRUE);

    widget3 = geda_bulb_new_visible (NULL);

    /* check geda_bulb_get_group */
    group   = geda_bulb_get_group(widget1);

    if (!group) {
      fprintf(stderr, "FAILED: line <%d> %s get_group\n", __LINE__, TWIDGET);
      result++;
    }

    /* Check geda_bulb_set_group */
    geda_bulb_set_group(widget3, group);

    /* Update pointer to the list */
    group = geda_bulb_get_group(widget1);

    count = g_slist_length(group);

    if (count != 3) {
      fprintf(stderr, "FAILED: line <%d> %s set_group <%d>\n", __LINE__, TWIDGET, count);
      result++;
    }

    widget4 = geda_bulb_new (NULL);

    /* Check geda_bulb_join_group */
    geda_bulb_join_group (widget4, widget3);

    group = geda_bulb_get_group(widget1);

    count = g_slist_length(group);

    if (count != 4) {
      fprintf(stderr, "FAILED: line <%d> %s join_group <%d>\n", __LINE__, TWIDGET, count);
      result++;
    }

    int index;

    /* Check geda_bulb_group_get_active_index */
    index = geda_bulb_group_get_active_index(group);

    if (index != 0) {
      fprintf(stderr, "FAILED: line <%d> %s get_active_index <%d>\n", __LINE__, TWIDGET, index);
      index = 0;
      result++;
    }

    /* dual purpose index is used here to indicate the callback occured */
    g_signal_connect (widget2, "toggled", G_CALLBACK (callback_toggled), &index);

    index = 0;

    /* Check geda_bulb_group_set_active_index */
    geda_bulb_group_set_active_index(group, 1);

    /* The callback should set the index = 2 */
    if (index != 2) {
      fprintf(stderr, "FAILED: line <%d> %s get_active_index <%d>\n", __LINE__, TWIDGET, index);
      result++;
    }

    index = geda_bulb_group_get_active_index(group);

    if (index != 1) {
      fprintf(stderr, "FAILED: line <%d> %s get_active_index <%d>\n", __LINE__, TWIDGET, index);
      result++;
    }

    g_object_ref_sink(widget1); /* Sink reference to entry widget */
    g_object_ref_sink(widget2); /* Sink reference to entry widget */
    g_object_ref_sink(widget3); /* Sink reference to entry widget */
    g_object_ref_sink(widget4); /* Sink reference to entry widget */

    g_object_unref(widget1);    /* Destroy widget1 */
    g_object_unref(widget2);    /* Destroy widget2 */
    g_object_unref(widget3);    /* Destroy widget3 */
    g_object_unref(widget4);    /* Destroy widget4 */
  }
  return result;
}

static int group_changed = 0;

static void on_group_changed (GedaBulb *bulb)
{
  group_changed++;
}

int
check_overides ()
{
  int result = 0;
  int value;

  GedaBulbClass *bulb_class;

  GtkWidget *widget1;
  GtkWidget *widget2;
  GtkWidget *widget3;

  widget1 = geda_bulb_new_visible (NULL);

  widget2 = geda_bulb_new_from_widget(widget1, TRUE);

  g_object_get (widget2, "visible", &value, NULL);

  if (!value) {
    fprintf(stderr, "FAILED: line <%d> is visible %s\n", __LINE__, TWIDGET);
    result++;
    value = 0;
  }

  widget3 = geda_bulb_new_visible (NULL);

  bulb_class = GEDA_BULB_GET_CLASS(widget3);

  bulb_class->group_changed = on_group_changed;

  geda_bulb_join_group (widget2, widget3);

  if (!group_changed) {
    fprintf(stderr, "FAILED: %s line <%d> group_changed not overridden\n", TWIDGET, __LINE__);
    result++;
  }

  if (!GEDA_IS_BULB(widget1)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_BULB(widget2)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_BULB(widget3)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget1); /* Sink reference to widget 1*/
  g_object_ref_sink(widget2); /* Sink reference to widget 2 */
  g_object_ref_sink(widget3); /* Sink reference to widget 3 */

  g_object_unref(widget1);    /* Destroy widget 1 */
  g_object_unref(widget2);    /* Destroy widget 2 */
  g_object_unref(widget3);    /* Destroy widget 3 */

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

      if (setjmp(point) == 0) {
        result += check_overides();
      }
      else {
        fprintf(stderr, "Caught signal checking overides in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
