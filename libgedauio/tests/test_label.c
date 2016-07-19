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

#define TWIDGET "GedaLabel"

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

  widget = geda_aligned_label_new("<b>Harmonic</b>", 0.4, 0.5);

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_aligned_visible_label_new */

  widget = geda_aligned_visible_label_new("<b>Harmonic</b>", 0.4, 0.5);

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_aligned_mnemonic_label_new */

  widget = geda_aligned_mnemonic_label_new("<b>Harmonic</b>", 0.4, 0.5);

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_aligned_visible_mnemonic_label_new */

  widget = geda_aligned_visible_mnemonic_label_new("<b>Harmonic</b>", 0.4, 0.5);

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
  geda_label_widget_set_text(widget, "Hippopotamus");

  /* geda_label_widget_get_text */
  text = geda_label_widget_get_text(widget);

  if (!text) {
    fprintf(stderr, "FAILED: line <%d> set_text %s\n", __LINE__, TWIDGET);
    result++;
  }
  else if (strcmp(text, "Hippopotamus")) {
    fprintf(stderr, "FAILED: line <%d> set/get text <%s>\n", __LINE__, text);
    result++;
  }

  /* geda_label_set_label */
  geda_label_set_label(label, "Rhinoceros");

  /* geda_label_get_label */
  text = geda_label_get_label(label);

  if (!text) {
    fprintf(stderr, "FAILED: line <%d> set label %s\n", __LINE__, TWIDGET);
    result++;
  }
  else if (strcmp(text, "Rhinoceros")) {
    fprintf(stderr, "FAILED: line <%d> set/get label <%s>\n", __LINE__, text);
    result++;
  }

  /* -------------------- attributes -------------------- */

  PangoAttrList *attrs;

  attrs = geda_label_get_attributes (label);

  geda_label_set_attributes (label, attrs);

  /* -------------------- use_markup property -------------------- */

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

  /* -------------------- use_underline property -------------------- */

  /* geda_label_widget_get_use_underline */

  if (geda_label_widget_get_use_underline (widget)) { /* Default value is FALSE */
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

  widget = geda_aligned_visible_mnemonic_label_new("<b>_Elephant</b>", 0.4, 0.5);
  label  = GEDA_LABEL(widget);

  unsigned int mnemonic_keyval;

  mnemonic_keyval = geda_label_get_mnemonic_keyval(label);

  if (mnemonic_keyval != 101) {
    fprintf(stderr, "FAILED: get_mnemonic_keyval <%d> %s\n", mnemonic_keyval, TWIDGET);
    result++;
  }

  /* geda_label_set_markup_with_mnemonic */

  geda_label_set_markup_with_mnemonic(label, "_Gnu");

  mnemonic_keyval = geda_label_get_mnemonic_keyval(label);

  if (mnemonic_keyval != 103) {
    fprintf(stderr, "FAILED: get_mnemonic_keyval <%d> %s\n", mnemonic_keyval, TWIDGET);
    result++;
  }

  /* -------------------- alignment -------------------- */

  float x_align;
  float y_align;

  geda_label_get_alignment (label, &x_align, &y_align);

  if (x_align != (float)0.4) {
    fprintf(stderr, "FAILED: %s get_alignment x=<%f>\n", TWIDGET, x_align);
    result++;
  }

  if (y_align != (float)0.5) {
    fprintf(stderr, "FAILED: %s get_alignment y=<%f>\n", TWIDGET, y_align);
    result++;
  }

  /* geda_label_set_alignment */

  geda_label_set_alignment (label, 0.3, 0.7);

  /* geda_label_widget_get_alignment */

  geda_label_widget_get_alignment (widget, &x_align, &y_align);

  if (x_align != (float)0.3) {
    fprintf(stderr, "FAILED: %s get_alignment x=<%f>\n", TWIDGET, x_align);
    result++;
  }

  if (y_align != (float)0.7) {
    fprintf(stderr, "FAILED: %s get_alignment y=<%f>\n", TWIDGET, y_align);
    result++;
  }

  /* geda_label_widget_set_alignment */

  geda_label_widget_set_alignment (widget, 0.5, 0.4);

  geda_label_get_alignment (label, &x_align, &y_align);

  if (x_align != (float)0.5) {
    fprintf(stderr, "FAILED: %s line <%d> get_alignment x=<%f>\n", TWIDGET, __LINE__, x_align);
    result++;
  }

  if (y_align != (float)0.4) {
    fprintf(stderr, "FAILED: %s line <%d> get_alignment y=<%f>\n", TWIDGET, __LINE__, y_align);
    result++;
  }

  /* -------------------- justify -------------------- */

  GtkJustification jtype;

  /* geda_label_widget_get_justify */

  jtype = geda_label_widget_get_justify (widget);

  if (jtype != GTK_JUSTIFY_LEFT) { /* Default value */
    fprintf(stderr, "FAILED: %s line <%d> justify=<%d>\n", TWIDGET, __LINE__, jtype);
    result++;
  }

  /* geda_label_widget_set_justify */

  geda_label_widget_set_justify(widget, GTK_JUSTIFY_RIGHT);

  jtype = geda_label_widget_get_justify (widget);

  if (jtype != GTK_JUSTIFY_RIGHT) {
    fprintf(stderr, "FAILED: %s line <%d> justify=<%d>\n", TWIDGET, __LINE__, jtype);
    result++;
  }

  /* -------------------- ellipsize -------------------- */

  PangoEllipsizeMode esmode;

  /* geda_label_widget_get_ellipsize */

  esmode = geda_label_widget_get_ellipsize (widget);

  if (esmode != PANGO_ELLIPSIZE_NONE) { /* Default value */
    fprintf(stderr, "FAILED: %s line <%d> ellipsize=<%d>\n", TWIDGET, __LINE__, esmode);
    result++;
  }

  /* geda_label_widget_set_ellipsize */

  geda_label_widget_set_ellipsize (widget, PANGO_ELLIPSIZE_START);

  esmode = geda_label_widget_get_ellipsize (widget);

  if (esmode != PANGO_ELLIPSIZE_START) {
    fprintf(stderr, "FAILED: %s line <%d> ellipsize=<%d>\n", TWIDGET, __LINE__, esmode);
    result++;
  }

  /* -------------------- width_chars -------------------- */

  int n_chars;

  /* geda_label_widget_get_width_chars */

  n_chars = geda_label_widget_get_width_chars (widget);

  if (n_chars != -1) { /* Default value */
    fprintf(stderr, "FAILED: %s line <%d> ellipsize=<%d>\n", TWIDGET, __LINE__, n_chars);
    result++;
  }

  /* geda_label_widget_set_width_chars */

  geda_label_widget_set_width_chars (widget, 12);

  n_chars = geda_label_widget_get_width_chars (widget);

  if (n_chars != 12) {
    fprintf(stderr, "FAILED: %s line <%d> ellipsize=<%d>\n", TWIDGET, __LINE__, n_chars);
    result++;
  }

  /* geda_label_widget_get_max_width_chars */

  n_chars = geda_label_widget_get_max_width_chars (widget);

  if (n_chars != -1) { /* Default value */
    fprintf(stderr, "FAILED: %s line <%d> ellipsize=<%d>\n", TWIDGET, __LINE__, n_chars);
    result++;
  }

  /* geda_label_widget_set_max_width_chars */

  geda_label_widget_set_max_width_chars (widget, 15);

  n_chars = geda_label_widget_get_max_width_chars (widget);

  if (n_chars != 15) {
    fprintf(stderr, "FAILED: %s line <%d> ellipsize=<%d>\n", TWIDGET, __LINE__, n_chars);
    result++;
  }

  /* -------------------- line_wrap -------------------- */

  PangoWrapMode wrap_mode;

  if (geda_label_get_line_wrap (label)) { /* Default value is FALSE */
    fprintf(stderr, "FAILED: line <%d> default line_wrap %s\n", __LINE__, TWIDGET);
    result++;
  }

  geda_label_set_line_wrap (label, TRUE);

  if (!geda_label_get_line_wrap (label)) { /* Default value is FALSE */
    fprintf(stderr, "FAILED: line <%d> set_line_wrap %s\n", __LINE__, TWIDGET);
    result++;
  }

  wrap_mode = geda_label_get_line_wrap_mode (label);

  if (wrap_mode != PANGO_WRAP_WORD) { /* Default value is PANGO_WRAP_WORD */
    fprintf(stderr, "FAILED:  %s line <%d> default wrap mode %d\n", TWIDGET, __LINE__, wrap_mode);
    result++;
  }

  geda_label_set_line_wrap_mode (label, PANGO_WRAP_CHAR);

  wrap_mode = geda_label_get_line_wrap_mode (label);

  if (wrap_mode != PANGO_WRAP_CHAR) {
    fprintf(stderr, "FAILED:  %s line <%d> set wrap mode %d\n", TWIDGET, __LINE__, wrap_mode);
    result++;
  }

  /* -------------------- selectable -------------------- */

  if (geda_label_widget_get_selectable (widget)) { /* Default value is FALSE */
    fprintf(stderr, "FAILED:  %s line <%d> default selectable\n", TWIDGET, __LINE__);
    result++;
  }

  geda_label_widget_set_selectable (widget, TRUE);

  if (!geda_label_widget_get_selectable (widget)) {
    fprintf(stderr, "FAILED:  %s line <%d> set selectable\n", TWIDGET, __LINE__);
    result++;
  }

  /* -------------------- angle -------------------- */

  double angle;

  angle = geda_label_widget_get_angle (widget);

  if (angle != 0.0) {
    fprintf(stderr, "FAILED: %s line <%d> default angle=<%f>\n", TWIDGET, __LINE__, angle);
    result++;
  }

  geda_label_widget_set_angle (widget, 45.0);

  angle = geda_label_widget_get_angle (widget);

  if (angle != 45.0) {
    fprintf(stderr, "FAILED: %s line <%d> default angle=<%f>\n", TWIDGET, __LINE__, angle);
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
