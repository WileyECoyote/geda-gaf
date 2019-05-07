/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_combobox.c
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
#include <../include/geda_entry.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_combobox.c"

#define TWIDGET "GedaComboBox"

/*! \file test_combobox.c
 *  \brief Tests for geda_combobox.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_combo_box_new();

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GTK_IS_BIN(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  if (GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TWIDGET);
    result++;
  }

  widget = NULL;

  /* geda_combo_box_new_with_entry */

  widget = geda_combo_box_new_with_entry();

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaEntry *entry;

    /* Uses geda_combo_widget_get_entry_widget */
    entry = geda_combo_widget_get_entry(widget);

    if (!GEDA_IS_ENTRY(entry)) {
      fprintf(stderr, "FAILED: line <%d> _get_entry\n", __LINE__);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  GtkListStore *model = gtk_list_store_new (2, G_TYPE_INT, G_TYPE_INT);

  /* geda_combo_box_new_with_model */

  widget = geda_combo_box_new_with_model (GTK_TREE_MODEL(model));

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GtkTreeModel *tree_model;

    tree_model = geda_combo_widget_get_model (widget);

    if (!GTK_IS_TREE_MODEL(tree_model)) {
      fprintf(stderr, "FAILED: %s line <%d> _get_model\n", TWIDGET, __LINE__);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  /* geda_combo_box_new_with_model_and_entry */

  widget = geda_combo_box_new_with_model_and_entry (GTK_TREE_MODEL(model));

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaEntry *entry;

    /* Uses geda_combo_get_entry_widget */
    entry = geda_combo_get_entry(GEDA_COMBO_BOX(widget));

    if (!GEDA_IS_ENTRY(entry)) {
      fprintf(stderr, "FAILED: %s line <%d> _get_entry\n", TWIDGET, __LINE__);
      result++;
    }

    GtkTreeModel *tree_model;

    tree_model = geda_combo_widget_get_model (widget);

    if (!GTK_IS_TREE_MODEL(tree_model)) {
      fprintf(stderr, "FAILED: %s line <%d> _get_model\n", TWIDGET, __LINE__);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */
  g_object_unref(model);     /* Destroy the model */

  /* geda_combo_box_new_text */

  widget = geda_combo_box_new_text();

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GtkTreeModel *tree_model;

    tree_model = geda_combo_widget_get_model (widget);

    if (!GTK_IS_TREE_MODEL(tree_model)) {
      fprintf(stderr, "FAILED: %s line <%d> _get_model\n", TWIDGET, __LINE__);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  /* geda_combo_box_new_text_with_entry */

  widget = geda_combo_box_new_text_with_entry();

  if (!GEDA_IS_COMBO_BOX(widget)) {
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

  GtkWidget    *widget;
  GtkTreeModel *model = GTK_TREE_MODEL(gtk_list_store_new (2, G_TYPE_INT, G_TYPE_INT));

  widget = geda_combo_box_new_with_model_and_entry (model);

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    if (!geda_combo_widget_get_has_entry(widget)) {
      fprintf(stderr, "FAILED: line <%d> _has_entry\n", __LINE__);
      result++;
    }
    else {

      GedaEntry *entry;

      /* Uses geda_combo_get_entry_widget */
      entry = geda_combo_get_entry(GEDA_COMBO_BOX(widget));

      if (!GEDA_IS_ENTRY(entry)) {
        fprintf(stderr, "FAILED: line <%d> _get_entry\n", __LINE__);
        result++;
      }
    }

    /* wrap width */
    int width = g_random_int_range (0, 4 * GEDA_COMBO_DEFAULT_WRAP);

    geda_combo_box_set_wrap_width (GEDA_COMBO_BOX(widget), width);

    if (geda_combo_box_get_wrap_width (GEDA_COMBO_BOX(widget)) != width) {
      fprintf(stderr, "FAILED: line <%d> wrap width\n", __LINE__);
      result++;
    }

    width = g_random_int_range (0, 4 * GEDA_COMBO_DEFAULT_WRAP);

    geda_combo_widget_set_wrap_width (widget, width);

    if (geda_combo_widget_get_wrap_width (widget) != width) {
      fprintf(stderr, "FAILED: line <%d> widget wrap_width\n", __LINE__);
      result++;
    }

    /* row span */

    int rs = 1;

    geda_combo_box_set_row_span_column (GEDA_COMBO_BOX(widget), rs);

    if (geda_combo_box_get_row_span_column (GEDA_COMBO_BOX(widget)) != rs) {
      fprintf(stderr, "FAILED: line <%d> row span\n", __LINE__);
      result++;
    }

    rs = 0;

    geda_combo_widget_set_row_span_column (widget, rs);

    if (geda_combo_widget_get_row_span_column (widget) != rs) {
      fprintf(stderr, "FAILED: line <%d> widget row span\n", __LINE__);
      result++;
    }

    /* column span */

    int cs = 1;

    geda_combo_box_set_column_span_column (GEDA_COMBO_BOX(widget), cs);

    if (geda_combo_box_get_column_span_column (GEDA_COMBO_BOX(widget)) != cs) {
      fprintf(stderr, "FAILED: line <%d> column span\n", __LINE__);
      result++;
    }

    cs = 2;

    geda_combo_widget_set_column_span_column (widget, cs);

    if (geda_combo_widget_get_column_span_column (widget) != cs) {
      fprintf(stderr, "FAILED: line <%d> widget column span\n", __LINE__);
      result++;
    }

    /* add tearoffs */

    geda_combo_box_set_add_tearoffs (GEDA_COMBO_BOX(widget), TRUE);

    if (!geda_combo_box_get_add_tearoffs (GEDA_COMBO_BOX(widget))) {
      fprintf(stderr, "FAILED: line <%d> add tearoffs\n", __LINE__);
      result++;
    }

    geda_combo_box_set_add_tearoffs (GEDA_COMBO_BOX(widget), FALSE);

    if (geda_combo_box_get_add_tearoffs (GEDA_COMBO_BOX(widget))) {
      fprintf(stderr, "FAILED: line <%d> add tearoffs\n", __LINE__);
      result++;
    }

    geda_combo_widget_set_add_tearoffs (widget, TRUE);

    if (!geda_combo_widget_get_add_tearoffs (widget)) {
      fprintf(stderr, "FAILED: line <%d> widget add tearoffs\n", __LINE__);
      result++;
    }

    geda_combo_widget_set_add_tearoffs (widget, FALSE);

    if (geda_combo_widget_get_add_tearoffs (widget)) {
      fprintf(stderr, "FAILED: line <%d> widget add tearoffs\n", __LINE__);
      result++;
    }

    /* Title */

    geda_combo_box_set_title (GEDA_COMBO_BOX(widget), TWIDGET);

    const char *title = geda_combo_box_get_title(GEDA_COMBO_BOX(widget));

    if (strcmp(title, TWIDGET)) {
      fprintf(stderr, "FAILED: line <%d> title\n", __LINE__);
      result++;
    }

    geda_combo_widget_set_title (widget, NULL);

    title = geda_combo_widget_get_title(widget);

    if (!strcmp(title, TWIDGET)) {
      fprintf(stderr, "FAILED: line <%d> widget title\n", __LINE__);
      result++;
    }

    /* focus-on-click */

    geda_combo_box_set_focus_on_click (GEDA_COMBO_BOX(widget), TRUE);

    if (!geda_combo_box_get_focus_on_click (GEDA_COMBO_BOX(widget))) {
      fprintf(stderr, "FAILED: line <%d> focus-on-click\n", __LINE__);
      result++;
    }

    geda_combo_box_set_focus_on_click (GEDA_COMBO_BOX(widget), FALSE);

    if (geda_combo_box_get_focus_on_click (GEDA_COMBO_BOX(widget))) {
      fprintf(stderr, "FAILED: line <%d> focus-on-click\n", __LINE__);
      result++;
    }

    geda_combo_widget_set_focus_on_click (widget, TRUE);

    if (!geda_combo_widget_get_focus_on_click (widget)) {
      fprintf(stderr, "FAILED: line <%d> focus-on-click\n", __LINE__);
      result++;
    }

    geda_combo_widget_set_focus_on_click (widget, FALSE);

    if (geda_combo_widget_get_focus_on_click (widget)) {
      fprintf(stderr, "FAILED: line <%d> focus-on-click\n", __LINE__);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */
  g_object_unref(model);     /* Destroy the model */

  widget = geda_combo_box_new_text();

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaComboBox *combo_box;
    GtkTreeModel *tree_model;
    GtkTreeIter   iter;

    tree_model = geda_combo_widget_get_model (widget);

    if (!GTK_IS_TREE_MODEL(tree_model)) {
      fprintf(stderr, "FAILED: %s line <%d> _get_model\n", TWIDGET, __LINE__);
      result++;
    }

    combo_box = GEDA_COMBO_BOX(widget);

    geda_combo_box_append_text (combo_box, "4");

    geda_combo_box_prepend_text (combo_box, "1");

    geda_combo_box_insert_text (combo_box, 1, "2");

    geda_combo_box_append_text (combo_box, "3");

    geda_combo_box_remove_index (combo_box, 2);

    /* The model should contain "1", "2", "3" */

    int count = geda_combo_widget_get_count (widget);

    if (count != 3) {
      fprintf(stderr, "FAILED: %s line <%d> count %d != 3\n", TWIDGET, __LINE__, count);
      result++;
    }

    if (!gtk_tree_model_get_iter_first (tree_model, &iter)) {
      fprintf(stderr, "FAILED: %s line <%d> _iter_first\n", TWIDGET, __LINE__);
      result++;
    }
    else {

      char *str;

      gtk_tree_model_get (tree_model, &iter, 0, &str, -1);

      if (*str != '1') {
        fprintf(stderr, "FAILED: %s line <%d> check prepend_text\n", TWIDGET, __LINE__);
        result++;
      }

      if (!gtk_tree_model_iter_next (tree_model, &iter)) {
        fprintf(stderr, "FAILED: %s line <%d> _iter_next\n", TWIDGET, __LINE__);
        result++;
      }
      else {

        gtk_tree_model_get (tree_model, &iter, 0, &str, -1);

        if (*str != '2') {
          fprintf(stderr, "FAILED: %s line <%d> check insert_text\n", TWIDGET, __LINE__);
          result++;
        }

        if (!gtk_tree_model_iter_next (tree_model, &iter)) {
          fprintf(stderr, "FAILED: %s line <%d> _iter_next\n", TWIDGET, __LINE__);
          result++;
        }
        else {

          gtk_tree_model_get (tree_model, &iter, 0, &str, -1);

          if (*str != '3') {
            fprintf(stderr, "FAILED: %s line <%d> check append_text\n", TWIDGET, __LINE__);
            result++;
          }

          if (gtk_tree_model_iter_next (tree_model, &iter)) {
            fprintf(stderr, "FAILED: %s line <%d> check remove_index\n", TWIDGET, __LINE__);
            result++;
          }

          /* geda_combo_box_get_active_text */

          str = geda_combo_box_get_active_text(combo_box);

          if (str) {
            fprintf(stderr, "FAILED: %s line <%d> get_active_text\n", TWIDGET, __LINE__);
            result++;
          }

          /* geda_combo_box_set_active */

          geda_combo_box_set_active(combo_box, 0);

          str = geda_combo_box_get_active_text(combo_box);

          if (!str && *str != '1') {
            fprintf(stderr, "FAILED: %s line <%d> get_active_text\n", TWIDGET, __LINE__);
            result++;
          }

          /* geda_combo_widget_set_active */

          geda_combo_widget_set_active(widget, 2);

          /* geda_combo_box_get_active */

          if (geda_combo_box_get_active(combo_box) != 2) {
            fprintf(stderr, "FAILED: %s line <%d> get_active\n", TWIDGET, __LINE__);
            result++;
          }

          geda_combo_box_set_active(combo_box, 1);

          /* geda_combo_widget_get_active */

          if (geda_combo_widget_get_active(widget) != 1) {
            fprintf(stderr, "FAILED: %s line <%d> get_active\n", TWIDGET, __LINE__);
            result++;
          }
          else {

            str = geda_combo_box_get_active_text(combo_box);

            if (!str && *str != '2') {
              fprintf(stderr, "FAILED: %s line <%d> get_active_text\n", TWIDGET, __LINE__);
              result++;
            }
          }
        }
      }
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

static int changed = 0;
static int view_changed = 0;

static void combo_changed (GedaComboBox *combo)
{
  changed++;
}

static void combo_view_changed (GedaComboBox *combo, unsigned int mode)
{
  view_changed++;
}

static char *do_format_entry_text (GedaComboBox *combo_box, const char *path)
{
  return g_strdup("3");
}

int
check_overides ()
{
  int result = 0;

  GedaComboBoxClass *combo_class;

  GtkWidget *widget = geda_combo_box_new_text_with_entry();

  combo_class = GEDA_COMBO_BOX_GET_CLASS(widget);

  combo_class->changed      = combo_changed;
  combo_class->view_changed = combo_view_changed;

  geda_combo_box_append_text (GEDA_COMBO_BOX(widget), "1");

  /* This should trigger the "changed" */
  geda_combo_widget_set_active(widget, 1);

  if (!changed) {
    fprintf(stderr, "FAILED: %s line <%d> not changed\n", TWIDGET, __LINE__);
    result++;
  }

  /* Trigger a view changed */
  g_object_set (widget, "list-view", 0, NULL); /* GEDA_VIEW_AUTO */
  g_object_set (widget, "list-view", 1, NULL); /* GEDA_VIEW_TREE */

  if (!view_changed) {
    fprintf(stderr, "FAILED: %s line <%d> not changed\n", TWIDGET, __LINE__);
    result++;
  }

  geda_combo_box_append_text (GEDA_COMBO_BOX(widget), "2");

  combo_class->format_entry_text = do_format_entry_text;

  geda_combo_widget_set_active(widget, 1);

  GedaEntry  *entry = geda_combo_widget_get_entry (widget);
  const char *text = geda_entry_get_text(entry);

  if (!text) {
    fprintf(stderr, "FAILED: %s line <%d> format_entry_text\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strcmp(text, "3")) {
    fprintf(stderr, "FAILED: %s line <%d> format_entry_text <%s>\n", TWIDGET,  __LINE__, text);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

static void
on_changed (GedaComboBox *combo, void *nothing)
{
  changed++;
}

static void
on_view_changed (GedaComboBox *combo, void *nothing)
{
  view_changed++;
}

int
check_signals ()
{
  int result = 0;

  GtkWidget *widget = geda_combo_box_new_text_with_entry();

  g_signal_connect (widget, "changed", G_CALLBACK (on_changed), NULL);
  g_signal_connect (widget, "view-changed", G_CALLBACK (on_view_changed), NULL);

  geda_combo_box_append_text (GEDA_COMBO_BOX(widget), "1");

  changed = 0;

  /* This should trigger the "changed" signal */
  geda_combo_widget_set_active(widget, 1);

  if (!changed) {
    fprintf(stderr, "FAILED: %s line <%d> not changed\n", TWIDGET, __LINE__);
    result++;
  }

  view_changed = 0;

  /* Trigger a view change */
  g_object_set (widget, "list-view", 2, NULL); /* GEDA_VIEW_MENU */

  /* Is 2 , once for on_view_changed() and once for combo_view_changed() */
  if (view_changed != 2) {
    fprintf(stderr, "FAILED: %s line <%d> view not changed\n", TWIDGET, __LINE__);
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

      if (setjmp(point) == 0) {
        result += check_overides();
      }
      else {
        fprintf(stderr, "Caught signal checking overides in %s\n\n", MUT);
        return 1;
      }

      if (setjmp(point) == 0) {
        result += check_signals();
      }
      else {
        fprintf(stderr, "Caught signal checking signals in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
