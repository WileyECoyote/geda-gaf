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
#include <../include/geda_entry.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_combobox.c"

#define TWIDGET "GedaComboBoxText"

/*! \file test_combobox.c
 *  \brief Tests for accel_label.c module
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

  /* geda_combo_box_new_with_model */

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

  return result;
}

int
check_accessors ()
{
  int result = 0;

  GtkWidget *widget = geda_combo_box_new_with_entry();

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
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

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = geda_combo_box_new_text();

  if (!GEDA_IS_COMBO_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaComboBox *combo_box;
    GtkTreeModel *tree_model;
    GtkTreeIter iter;

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
        }
      }
    }
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
      return 1;
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
