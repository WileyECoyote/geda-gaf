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

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include <../include/geda_entry.h>
#include <../include/geda_label.h>

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
  geda_label_widget_set_label(widget, "Rhinoceros");

  /* geda_label_get_label */
  text = geda_label_widget_get_label(widget);

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

  /* -------------------- set_markup property -------------------- */

  const char *gnu = "Gnu";
  char *markup;

  markup = g_strdup_printf ("<span font=\"14\" color=\"red\">"
                            "<b>\tRed: %s</b>"
                            "</span>", gnu);

  geda_label_set_markup(label, NULL);

  geda_label_set_markup (label, markup);
  g_free (markup);

  if (!geda_label_get_use_markup (label)) {
    fprintf(stderr, "FAILED: %s line <%d> set markup\n", TWIDGET, __LINE__);
    result++;
  }

  text = geda_label_get_label(label);

  if (!strstr(text, "Red: Gnu")) {
    fprintf(stderr, "FAILED: line <%d> set_markup <%s>\n", __LINE__, text);
    result++;
  }

  /* -------------------- use_markup property -------------------- */

  geda_label_widget_set_use_markup (widget, FALSE);

  /* geda_label_get_use_markup */
  if (geda_label_widget_get_use_markup (widget)) {
    fprintf(stderr, "FAILED: %s line <%d> get/set use_markup\n", TWIDGET, __LINE__);
    result++;
  }

  /* geda_label_widget_set_use_markup */
  geda_label_widget_set_use_markup (widget, TRUE);

  if (!geda_label_widget_get_use_markup (widget)) {
    fprintf(stderr, "FAILED: %s line <%d> get/set use_markup\n", TWIDGET, __LINE__);
    result++;
  }

  /* -------------------- use_underline property -------------------- */

  /* geda_label_widget_get_use_underline */

  if (geda_label_widget_get_use_underline (widget)) { /* Default value is FALSE */
    fprintf(stderr, "FAILED: %s line <%d> get/set use_underline\n", TWIDGET, __LINE__);
    result++;
  }

  /* geda_label_widget_set_use_underline */

  geda_label_widget_set_use_underline (widget, TRUE);

  if (!geda_label_widget_get_use_underline (widget)) {
    fprintf(stderr, "FAILED: %s line <%d> get/set use_underline\n", TWIDGET, __LINE__);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = geda_aligned_visible_mnemonic_label_new("<b>_Elephant</b>", 0.4, 0.5);
  label  = GEDA_LABEL(widget);

  unsigned int mnemonic_keyval;

  mnemonic_keyval = geda_label_get_mnemonic_keyval(label);

  if (mnemonic_keyval != 101) {
    fprintf(stderr, "FAILED: %s get_mnemonic_keyval <%d>\n", TWIDGET, mnemonic_keyval);
    result++;
  }

  /* geda_label_set_markup_with_mnemonic */

  markup = g_strdup_printf ("&lt;span style=\"italic\"&gt;_%s&lt;/span&gt;", gnu);

  geda_label_set_markup_with_mnemonic(label, markup);

  mnemonic_keyval = geda_label_get_mnemonic_keyval(label);

  if (mnemonic_keyval != 103) {
    fprintf(stderr, "FAILED: %s get_mnemonic_keyval <%d>\n", TWIDGET, mnemonic_keyval);
    result++;
  }

  char mnemonic;

  mnemonic = geda_label_get_mnemonic_char(label);

  if (mnemonic != 'G') {
    fprintf(stderr, "FAILED:  %s get_mnemonic <%c>\n", TWIDGET, mnemonic);
    result++;
  }

  mnemonic = geda_label_get_mnemonic_lower(label);

  if (mnemonic != 'g') {
    fprintf(stderr, "FAILED:  %s mnemonic_lower <%c>\n", TWIDGET, mnemonic);
    result++;
  }

  /* geda_label_set_mnemonic_text */

  geda_label_set_mnemonic_text(label, "_Rhinoceros");

  mnemonic_keyval = geda_label_get_mnemonic_keyval(label);

  if (mnemonic_keyval != 114) { /* Lower case "R" */
    fprintf(stderr, "FAILED: %s get_mnemonic_keyval <%d>\n", TWIDGET, mnemonic_keyval);
    result++;
  }

  /* ----------------- mnemonic_visible ---------------- */

  if (!geda_label_get_mnemonic_visible(label)) {
    fprintf(stderr, "FAILED: %s line <%d> mnemonic_visible\n", TWIDGET, __LINE__);
    result++;
  }

  /* ----------------- mnemonic_widget ----------------- */

  GtkWidget *mw;

  mw = geda_label_get_mnemonic_widget(label);

  if (mw) {
    fprintf(stderr, "FAILED: %s line <%d> get_mnemonic_widget\n", TWIDGET, __LINE__);
    result++;
  }

  mw = geda_entry_new ();

  geda_label_set_mnemonic_widget(label, mw);

  if (geda_label_get_mnemonic_widget(label) != mw) {
    fprintf(stderr, "FAILED: %s line <%d> set_mnemonic_widget\n", TWIDGET, __LINE__);
    result++;
  }

  geda_label_set_mnemonic_widget(label, NULL);

  g_object_ref_sink(mw); /* Sink reference to entry widget */
  g_object_unref(mw);    /* Destroy the widget */

  mw = geda_label_get_mnemonic_widget(label);

  if (mw) {
    fprintf(stderr, "FAILED: %s line <%d> get_mnemonic_widget\n", TWIDGET, __LINE__);
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

  if (!geda_label_get_line_wrap (label)) {
    fprintf(stderr, "FAILED: line <%d> set_line_wrap %s\n", __LINE__, TWIDGET);
    result++;
  }

  wrap_mode = geda_label_get_line_wrap_mode (label);

  if (wrap_mode != PANGO_WRAP_WORD) { /* Default value is PANGO_WRAP_WORD */
    fprintf(stderr, "FAILED: %s line <%d> default wrap mode %d\n", TWIDGET, __LINE__, wrap_mode);
    result++;
  }

  geda_label_set_line_wrap_mode (label, PANGO_WRAP_CHAR);

  wrap_mode = geda_label_get_line_wrap_mode (label);

  if (wrap_mode != PANGO_WRAP_CHAR) {
    fprintf(stderr, "FAILED: %s line <%d> set wrap mode %d\n", TWIDGET, __LINE__, wrap_mode);
    result++;
  }

  /* -------------------- set_pattern -------------------- */

  geda_label_set_label(label, "FooBarBaz");

  geda_label_set_pattern(label, "___   ___");

  attrs = geda_label_get_effective_attributes (label);

  if (!attrs) {
    fprintf(stderr, "FAILED: %s line <%d> set_pattern\n", TWIDGET, __LINE__);
    result++;
  }

  /* -------------------- selectable -------------------- */

  if (geda_label_widget_get_selectable (widget)) { /* Default value is FALSE */
    fprintf(stderr, "FAILED: %s line <%d> default selectable\n", TWIDGET, __LINE__);
    result++;
  }

  geda_label_widget_set_selectable (widget, TRUE);

  if (!geda_label_widget_get_selectable (widget)) {
    fprintf(stderr, "FAILED: %s line <%d> set selectable\n", TWIDGET, __LINE__);
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

  /* -------------------- region -------------------- */

  int start;
  int end;

  geda_label_select_region (label, 1, 4);

  if (!geda_label_get_selection_bounds(label, &start, &end)) {
    fprintf(stderr, "FAILED: %s line <%d> select_region\n", TWIDGET, __LINE__);
    result++;
  }
  else if (start - 1) {
    fprintf(stderr, "FAILED: %s line <%d> select start %d\n", TWIDGET, __LINE__, start);
    result++;
  }
  else if (end - 4) {
    fprintf(stderr, "FAILED: %s line <%d> select end %d\n", TWIDGET, __LINE__, end);
    result++;
  }

  /* -------------------- layout -------------------- */

  PangoLayout *layout;
  int lx, ly;

  layout = geda_label_get_layout (label);

  if (!PANGO_IS_LAYOUT(layout)) {
    fprintf(stderr, "FAILED: line <%d> is a PangoLayout\n", __LINE__);
    result++;
  }

  geda_label_get_layout_offsets (label,  &lx, &ly);

  if (!lx) {
    fprintf(stderr, "FAILED: %s line <%d> layout x %d\n", TWIDGET, __LINE__, lx);
    result++;
  }

  if (ly != -1) {
    fprintf(stderr, "FAILED: %s line <%d> layout y %d\n", TWIDGET, __LINE__, ly);
    result++;
  }

  /* -------------------- single_line_mode -------------------- */

  /* geda_label_get_single_line_mode */

  if (geda_label_get_single_line_mode (label)) { /* Default value is FALSE */
    fprintf(stderr, "FAILED: %s line <%d> default single_line_mode\n", TWIDGET, __LINE__);
    result++;
  }

  /* geda_label_set_single_line_mode */

  geda_label_set_single_line_mode(label, TRUE);

  if (!geda_label_get_single_line_mode (label)) {
    fprintf(stderr, "FAILED: %s line <%d> single_line_mode\n", TWIDGET, __LINE__);

  }

  /* ----------------------- uri ----------------------- */

  const char *uri;

  uri = geda_label_get_current_uri (label);

  if (uri) {
    fprintf(stderr, "FAILED: %s line <%d> uri=%s\n", TWIDGET, __LINE__, uri);
    result++;
  }

  /* ------------------- track_links ------------------- */

  geda_label_set_track_visited_links (label, FALSE);

  if (geda_label_get_track_visited_links(label)) {
    fprintf(stderr, "FAILED: %s line <%d> default track_links\n", TWIDGET, __LINE__);
    result++;
  }

  geda_label_set_track_visited_links (label, TRUE);

  if (!geda_label_get_track_visited_links(label)) {
    fprintf(stderr, "FAILED: %s line <%d> track_links\n", TWIDGET, __LINE__);
    result++;
  }

  /* -------------------- cursor_position -------------------- */

  start = geda_label_get_cursor_position (label);

  if (start - 4) {
    fprintf(stderr, "FAILED: %s line <%d> cursor_position %d\n", TWIDGET, __LINE__, start);
    result++;
  }

  end = geda_label_get_selection_bound (label);

  if (end - 1) {
    fprintf(stderr, "FAILED: %s line <%d> selection_bound %d\n", TWIDGET, __LINE__, end);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

int
check_methods ()
{
  const char *func;

  int result = 0;

  GtkWidget *widget = geda_mnemonic_label_new("Horses");
  GedaLabel *label  = GEDA_LABEL(widget);

  /* -------  mnemonics_visible_apply_recursively ------ */

  func = "mnemonics_visible_apply_recursively";

  geda_label_set_mnemonics_visible_recursive(widget, FALSE);

  if (geda_label_get_mnemonic_visible(label)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  geda_label_set_mnemonics_visible_recursive(widget, TRUE);

  if (!geda_label_get_mnemonic_visible(label)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  /* recursively */

  GtkWidget *container = gtk_vbox_new (0,0);

  GtkWidget *widget1 = geda_mnemonic_label_new("Clydesdale");
  GtkWidget *widget2 = geda_mnemonic_label_new("Mustang");
  GtkWidget *widget3 = geda_mnemonic_label_new("Thoroughbred");

  gtk_container_add (GTK_CONTAINER(container), widget1);
  gtk_container_add (GTK_CONTAINER(container), widget2);
  gtk_container_add (GTK_CONTAINER(container), widget3);

  geda_label_set_mnemonics_visible_recursive(container, FALSE);

  if (geda_label_get_mnemonic_visible((GedaLabel*)widget1)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  if (geda_label_get_mnemonic_visible((GedaLabel*)widget2)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  if (geda_label_get_mnemonic_visible((GedaLabel*)widget3)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

    geda_label_set_mnemonics_visible_recursive(container, TRUE);

  if (!geda_label_get_mnemonic_visible((GedaLabel*)widget1)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  if (!geda_label_get_mnemonic_visible((GedaLabel*)widget2)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  if (!geda_label_get_mnemonic_visible((GedaLabel*)widget3)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  g_object_ref_sink(widget);    /* Sink reference to entry widget */
  g_object_ref_sink(container); /* Sink reference to entry container */
  g_object_unref(widget);       /* Destroy the widget */
  g_object_unref(container);    /* Destroy the container */

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
        result = check_methods();
      }
      else {
        fprintf(stderr, "Caught signal checking methods in %s\n\n", MUT);
        return 1;
      }
    }
  }

  return result;
}
