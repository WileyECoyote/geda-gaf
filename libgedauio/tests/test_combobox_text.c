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

#include "../../config.h"

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
 *  \brief Tests for geda_combobox_text.c module
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

  g_object_ref_sink(widget); /* Sink reference to entry widget */
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

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  /* geda_combo_box_text_list_new */

  widget = geda_combo_box_text_list_new();

  if (!GEDA_IS_COMBO_BOX_TEXT(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaEntry *entry;

    /* Use base class getter */
    entry = geda_combo_widget_get_entry(widget);

    if (!GEDA_IS_ENTRY(entry)) {
      fprintf(stderr, "FAILED: line <%d> _get_entry\n", __LINE__);
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

  GtkWidget *widget = geda_combo_box_text_new_with_entry();

  if (!GEDA_IS_COMBO_BOX_TEXT(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaComboBoxText *combo_text = GEDA_COMBO_BOX_TEXT(widget);
    GtkWidget *entry;
    int value;

    /* Uses geda_combo_get_entry_widget */
    entry = geda_combo_box_text_get_entry_widget(combo_text);

    if (!GEDA_IS_ENTRY(entry)) {
      fprintf(stderr, "FAILED: line <%d> _get_entry\n", __LINE__);
      result++;
    }

    /* Check activates-default */

    /* use geda_entry_get_activates_default */
    value = geda_combo_box_text_get_activate_default(combo_text);

    if (value) { /* default is FALSE */
      fprintf(stderr, "FAILED: %s line <%d> activates-default %d\n", TWIDGET, __LINE__, value);
      result++;
    }

    /* use geda_entry_set_activates_default */
    geda_combo_box_text_set_activate_default(combo_text, TRUE);

    value = geda_combo_box_text_get_activate_default(combo_text);

    if (!value) {
      fprintf(stderr, "FAILED: %s line <%d> activates-default %d\n", TWIDGET, __LINE__, value);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */
  return result;
}


#ifdef DEBUG_COMBO_TEXT

static void
debug_print(GtkWidget *widget) {

  GedaComboBoxText *combo_text = GEDA_COMBO_BOX_TEXT(widget);

  int old_index = geda_combo_box_text_widget_get_active(widget);

  int i, count = geda_combo_widget_get_count(widget);

  for (i =0; i< count; i++ ) {

    char *city;

    geda_combo_box_text_set_active(combo_text, i);

    city = geda_combo_box_text_widget_get_active_text(widget);

    fprintf(stderr, "FAILED: %s city[%d]=%s\n", TWIDGET, i, city);
  }

  geda_combo_box_text_widget_set_active(widget, old_index);
}

#endif

static void
load_cities(GtkWidget *widget) {

  GedaComboBoxText *combo_text = GEDA_COMBO_BOX_TEXT(widget);

  /* append */
  geda_combo_box_text_append (combo_text, "kampong");         /* 5 */

  geda_combo_box_text_append_text (combo_text, "Poipait");    /* 6 */

  geda_combo_box_text_widget_append (widget, "Sereysophon");  /* 7 */

  /* insert */
  geda_combo_box_text_insert(combo_text, 0, "Chbamon");       /* 4 */

  geda_combo_box_text_insert_text (combo_text, 0, "Bavet");   /* 3 */

  geda_combo_box_text_widget_insert (widget, -1, "Soung");    /* 8 */

  /* prepend */
  geda_combo_box_text_prepend (combo_text, "Battambang");      /* 2 */

  geda_combo_box_text_prepend_text (combo_text, "Banteay");    /* 1 */

  geda_combo_box_text_widget_prepend (widget, "Banlung");      /* 0 */
}

int check_river(GedaComboBox *combo_box, char *tonle)
{
  GtkTreeIter iter;
  int result = 0;

  if (geda_combo_box_get_active_iter(combo_box, &iter)) {

    GtkTreeModel *model;
    char *river;

    model = geda_combo_box_get_model(combo_box);
    river = NULL;

    gtk_tree_model_get(model, &iter, 2, &river, -1);

    if (!river) {
      fprintf(stderr, "FAILED: %s no river at line <%d>\n", TWIDGET, __LINE__);
      result++;
    }
    else if (strncmp(river, tonle, strlen(tonle))) {
      fprintf(stderr, "FAILED: %s line <%d> river %s not %s\n", TWIDGET, __LINE__, river, tonle);
      result++;
    }
  }
  else {
    fprintf(stderr, "FAILED: %s line <%d> get active iter\n", TWIDGET, __LINE__);
    result++;
  }

  return result;
}

int
check_combo_box_text_pairs(GedaComboBoxText *combo_text)
{
  char *city;
  int result = 0;

  /* append */

  geda_combo_box_text_append_pair (combo_text, "Pursat", "Peam");   /* 1 */

  /* insert */

  geda_combo_box_text_insert_pair (combo_text, -1, "Koulen", "Sen"); /* 2 */

  /* prepend */

  geda_combo_box_text_prepend_pair (combo_text, "Kampong", "Mekong"); /* 0 */

  geda_combo_box_text_set_active(combo_text, 0);

  city = geda_combo_box_text_get_active_text(combo_text);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Kampong", 7)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  g_free(city);

  result += check_river (GEDA_COMBO_BOX(combo_text), "Mekong");

  geda_combo_box_text_set_active(combo_text, 2);

  result += check_river (GEDA_COMBO_BOX(combo_text), "Sen");

  geda_combo_box_text_set_active(combo_text, 1);

  result += check_river (GEDA_COMBO_BOX(combo_text), "Peam");

  return result;
}

int
check_methods ()
{
  int   result = 0;
  int   index;
  char *city;

  GtkWidget *widget = geda_combo_box_text_new();

  GedaComboBoxText *combo_text = GEDA_COMBO_BOX_TEXT(widget);

  load_cities(widget);

  index = geda_combo_box_text_get_active(combo_text);

  if (index + 1) {
    fprintf(stderr, "FAILED: %s line <%d> index should not be set <%d>\n", TWIDGET, __LINE__, index);
    result++;
  }

  geda_combo_box_text_set_active(combo_text, 1);

  index = geda_combo_box_text_widget_get_active(widget);

  if (index - 1) {
    fprintf(stderr, "FAILED: %s line <%d> bad index <%d>\n", TWIDGET, __LINE__, index);
    result++;
  }

  /* Uses geda_combo_box_text_get_active_text */
  city = geda_combo_box_text_widget_get_active_text(widget);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Banteay", 7)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  geda_combo_box_text_widget_set_active(widget, 4);

  /* Uses geda_combo_box_text_get_active_text */
  city = geda_combo_box_text_widget_get_active_text(widget);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Chbamon", 7)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  /* Add and set text to a new city not in the list */
  if (!geda_combo_box_text_widget_set_active_text(widget, "Takeo")) {
    fprintf(stderr, "FAILED: %s line <%d> set_active_text\n", TWIDGET, __LINE__);
    result++;
  }

#ifdef DEBUG_COMBO_TEXT
  debug_print(widget);
#endif

  /* "Takeo" should be the active text in the entry */
  city = geda_combo_box_text_widget_get_active_text(widget);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Takeo", 5)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  /* "Takeo" should have been prepended to the list so active index = 0 */
  index = geda_combo_box_text_widget_get_active(widget);

  if (index) {
    fprintf(stderr, "FAILED: %s line <%d> bad index <%d>\n", TWIDGET, __LINE__, index);
    result++;
  }

  /* 0 Should already be active */
  geda_combo_box_text_set_active(combo_text, 0);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Takeo", 5)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  geda_combo_box_text_remove_index(combo_text, 0);

  /* "Takeo" should be removed from the entry */
  city = geda_combo_box_text_widget_get_active_text(widget);

  if (city) {
    fprintf(stderr, "FAILED: %s city should not be %s at line <%d>\n", TWIDGET, city, __LINE__);
    result++;
  }

  geda_combo_box_text_set_active(combo_text, 0);

  city = geda_combo_box_text_widget_get_active_text(widget);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Banlung", 8)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  /* "Soung" should be back at 8 after "Takeo" was removed */
  geda_combo_box_text_set_active(combo_text, 8);

  city = geda_combo_box_text_widget_get_active_text(widget);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Soung", 5)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  /* Remove "Banlung" */
  geda_combo_box_text_widget_remove(widget, 0);

  /* "Banteay" Should now be at the first index */
  geda_combo_box_text_set_active(combo_text, 0);

  city = geda_combo_box_text_widget_get_active_text(widget);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Banteay", 7)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  geda_combo_box_text_remove_text(combo_text, "Sereysophon");

  /* "Soung" should be at 6 after "Sereysophon" was removed */
  geda_combo_box_text_set_active(combo_text, 6);

  city = geda_combo_box_text_widget_get_active_text(widget);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Soung", 5)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  /* Remove the text at index 0 and recheck _remove_text */
  geda_combo_box_text_set_active(combo_text, 0);

  city = geda_combo_box_text_widget_get_active_text(widget);

  geda_combo_box_text_remove_text(combo_text, city);

  /* "Soung" should be at 5 after "Banteay" was removed */
  geda_combo_box_text_set_active(combo_text, 5);

  city = geda_combo_box_text_widget_get_active_text(widget);

  if (!city) {
    fprintf(stderr, "FAILED: %s no city at line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strncmp(city, "Soung", 5)) {
    fprintf(stderr, "FAILED: %s line <%d> wrong city %s\n", TWIDGET, __LINE__, city);
    result++;
  }

  /* Check geda_combo_box_text_remove_all_text */

  geda_combo_box_text_remove_all_text(combo_text);

  int count = geda_combo_widget_get_count(widget);

  if (count) {
    fprintf(stderr, "FAILED: %s at <%d>; remove_all_text: count=%d\n", TWIDGET, __LINE__, count);
    result++;
  }

  /* Add and set text to a new city into the empty list */
  if (!geda_combo_box_text_widget_set_active_text(widget, "Khemarak")) {
    fprintf(stderr, "FAILED: %s line <%d> set_active_text\n", TWIDGET, __LINE__);
    result++;
  }

  count = geda_combo_widget_get_count(widget);

  if (count - 1) {
    fprintf(stderr, "FAILED: %s at <%d>; remove_all_text: count=%d\n", TWIDGET, __LINE__, count);
    result++;
  }

  geda_combo_box_text_widget_remove_all(widget);

  count = geda_combo_widget_get_count(widget);

  if (count) {
    fprintf(stderr, "FAILED: %s at <%d>; remove_all_text: count=%d\n", TWIDGET, __LINE__, count);
    result++;
  }

  result += check_combo_box_text_pairs(combo_text);

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */
  return result;
}

int
check_validations (void)
{
  int result = 0;

  GtkWidget *bulb = geda_bulb_new(NULL);

  /* geda_combo_box_text_get_activate_default */

  if (geda_combo_box_text_get_activate_default(NULL)) {
    fprintf(stderr, "FAILED: %s NULL widget at <%d>\n", TWIDGET, __LINE__);
    result++;
  }

  if (geda_combo_box_text_get_activate_default((GedaComboBoxText*)bulb)) {
    fprintf(stderr, "FAILED: %s invalid at <%d>\n", TWIDGET, __LINE__);
    result++;
  }

  /* geda_combo_box_text_set_activate_default */

  geda_combo_box_text_set_activate_default(NULL, 2);

  geda_combo_box_text_set_activate_default((GedaComboBoxText*)bulb, 2);

  /* geda_combo_box_text_get_entry */

  if (geda_combo_box_text_get_entry(NULL)) {
    fprintf(stderr, "FAILED: %s NULL widget at <%d>\n", TWIDGET, __LINE__);
    result++;
  }

  if (geda_combo_box_text_get_entry((GedaComboBoxText*)bulb)) {
    fprintf(stderr, "FAILED: %s invalid at <%d>\n", TWIDGET, __LINE__);
    result++;
  }

  /* geda_combo_box_text_get_entry_widget */

  if (geda_combo_box_text_get_entry_widget(NULL)) {
    fprintf(stderr, "FAILED: %s NULL widget at <%d>\n", TWIDGET, __LINE__);
    result++;
  }

  if (geda_combo_box_text_get_entry_widget((GedaComboBoxText*)bulb)) {
    fprintf(stderr, "FAILED: %s invalid at <%d>\n", TWIDGET, __LINE__);
    result++;
  }

  g_object_ref_sink(bulb); /* Sink reference to bulb widget */
  g_object_unref(bulb);    /* Destroy the bulb */

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
        result += check_methods();
      }
      else {
        fprintf(stderr, "Caught signal checking methods in %s\n\n", MUT);
        return 1;
      }

      if (setjmp(point) == 0) {
        result += check_validations();
      }
      else {
        fprintf(stderr, "Caught signal checking validations in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
