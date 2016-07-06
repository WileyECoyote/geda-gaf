/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_combobox_text.c
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

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include <../include/geda_comboboxtext.h>
#include <../include/geda_bulb.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_combobox_text.c"

#define TWIDGET "GedaComboBoxText"

/*! \file test_combobox_text.c
 *  \brief Tests for accel_label.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_combo_box_text_new();

  if (!GEDA_IS_COMBO_BOX_TEXT(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

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

  g_object_unref(widget);    /* Destroy the widget */

  if (GEDA_IS_COMBO_BOX_TEXT(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TWIDGET);
    result++;
  }

  widget = NULL;

  /* geda_combo_box_text_new_with_entry */

  widget = geda_combo_box_text_new_with_entry();

  if (!GEDA_IS_COMBO_BOX_TEXT(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaComboBoxText *combo_text = GEDA_COMBO_BOX_TEXT(widget);
    GedaEntry *entry;

    /* geda_combo_box_text_get_entry_widget */
    entry = geda_combo_box_text_get_entry(combo_text);

    if (!GEDA_IS_ENTRY(entry)) {
      fprintf(stderr, "FAILED: line <%d> _get_entry\n", __LINE__);
      result++;
    }
  }

  g_object_unref(widget);    /* Destroy the widget */

  /* geda_combo_box_text_list_new */

  widget = geda_combo_box_text_list_new();

  if (!GEDA_IS_COMBO_BOX_TEXT(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

int
check_accessors ()
{
  int result = 0;

  GtkWidget *widget = geda_combo_box_text_new_with_entry();

  if (!GEDA_IS_COMBO_BOX_TEXT(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaComboBoxText *combo_text = GEDA_COMBO_BOX_TEXT(widget);
    GtkWidget *entry;

    /* Uses geda_combo_get_entry_widget */
    entry = geda_combo_box_text_get_entry_widget(combo_text);

    if (!GEDA_IS_ENTRY(entry)) {
      fprintf(stderr, "FAILED: line <%d> _get_entry\n", __LINE__);
      result++;
    }
  }

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
