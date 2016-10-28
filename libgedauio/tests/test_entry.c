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

static GList *get_completion (void) {

  GList *Era;
  uint8_t i;


  static char *Paleozoic[] = {
                               "Permian",
                               "Carboniferous"
                               "Devonian",
                               "Silurian",
                               "Ordovician"
                               "Cambrian",
                               NULL};
  Era = NULL;

  for (i = 0; Paleozoic[i] != NULL; i++) {
    Era = g_list_append(Era, Paleozoic[i]);
  }

  return Era;
}

static GList *get_history (void) {

  GList *Eon;
  uint8_t i;

  static char *Phanerozoic[] = {
                                 "Cenozoic",
                                 "Mesozoic"
                                 "Paleozoic",
                                 NULL};
  Eon = NULL;

  for (i = 0; Phanerozoic[i] != NULL; i++) {
    Eon = g_list_append(Eon, Phanerozoic[i]);
  }

  return Eon;
}

int check_construction (void)
{
  int result = 0;
  int value;

  static GList *complete;
  static GList *history;

  /* geda_entry_new */

  GtkWidget *widget = geda_entry_new ();

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

  widget = NULL;

  complete = get_completion();
  history  = get_history();

  /* geda_entry_new_with_completion */

  widget = geda_entry_new_history_complete(&history, &complete);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    if (!geda_entry_widget_get_completion(widget)) {
      fprintf(stderr, "FAILED: line <%d> completion %s\n", __LINE__, TWIDGET);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_entry_new_visible */

  widget = geda_entry_new_visible ();

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    /* Verify the visibility was set */
    g_object_get (widget, "visible", &value, NULL);

    if (!value) {
      fprintf(stderr, "FAILED: line <%d> is visible %s\n", __LINE__, TWIDGET);
      result++;
      value = 0;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_entry_new_visible_buffer */

  GtkEntryBuffer *buffer;

  /* uses geda_entry_new_with_buffer */
  widget = geda_entry_new_visible_buffer (NULL);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  buffer = gtk_entry_buffer_new ("abc", 6);

  widget = geda_entry_new_visible_buffer (buffer);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    /* Verify the visibility was set */
    g_object_get (widget, "visible", &value, NULL);

    if (!value) {
      fprintf(stderr, "FAILED: line <%d> is visible %s\n", __LINE__, TWIDGET);
      result++;
      value = 0;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_entry_new_visible_completion  */

  /* uses geda_entry_new_with_completion */
  widget = geda_entry_new_with_completion (&complete);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {
    if (!geda_entry_widget_get_completion(widget)) {
      fprintf(stderr, "FAILED: line <%d> completion %s\n", __LINE__, TWIDGET);
      result++;
    }
  }

  g_list_free(complete);
  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_entry_new_visible_history */

  /* use geda_entry_new_with_history */
  widget = geda_entry_new_visible_history (&history);

  if (!GEDA_IS_ENTRY(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_list_free(history);
  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

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

#define HEART "♥"
const char text[] = "Everyone ♥ GEDA";

static PangoAttrList *
create_fancy_attr_list (GedaEntry *entry)
{
  PangoAttrList    *attrs;
  PangoLayout      *layout;
  PangoFontMetrics *metrics;
  PangoRectangle    ink_rect, logical_rect;

  int ascent;
  const char *p;

  layout = gtk_entry_get_layout ((GtkEntry*)entry);

  /* Get font metrics and prepare fancy shape size */
  metrics = pango_context_get_metrics (pango_layout_get_context (layout),
                                       pango_layout_get_font_description (layout),
                                       NULL);
  ascent              = pango_font_metrics_get_ascent (metrics);
  logical_rect.x      = 0;
  logical_rect.width  = ascent;
  logical_rect.y      = -ascent;
  logical_rect.height = ascent;
  ink_rect            = logical_rect;
  pango_font_metrics_unref (metrics);

  /* Set fancy shape attributes for all hearts */
  attrs = pango_attr_list_new ();

  for (p = text; (p = strstr (p, HEART)); p += strlen (HEART)) {

      PangoAttribute *attr;

      attr = pango_attr_shape_new_with_data (&ink_rect,
                                             &logical_rect,
                                             GUINT_TO_POINTER (g_utf8_get_char (p)),
                                             NULL, NULL);

      attr->start_index = p - text;
      attr->end_index = attr->start_index + strlen (HEART);

      pango_attr_list_insert (attrs, attr);
    }

  return attrs;
}

static int
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

  /* Check attributes */

  PangoAttrList *alist;

  alist = geda_entry_widget_get_attributes(widget);

  if (alist) { /* default is none */
    fprintf(stderr, "FAILED: %s line <%d> attributes\n", TWIDGET, __LINE__);
    result++;
  }

  alist = create_fancy_attr_list (GEDA_ENTRY (widget));

  geda_entry_widget_set_attributes (widget, alist);

  alist = geda_entry_widget_get_attributes(widget);

  if (!alist) { /* default is none */
    fprintf(stderr, "FAILED: %s line <%d> attributes\n", TWIDGET, __LINE__);
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

  const char *string;

  string = geda_entry_widget_get_text(widget);

  if (!string) {
    fprintf(stderr, "FAILED: %s line <%d> string is NULL\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strlen(string)) {
    fprintf(stderr, "FAILED: %s line <%d> buffer is not empty\n", TWIDGET, __LINE__);
    result++;
  }

  geda_entry_widget_set_text(widget, "Fandango");

  string = geda_entry_widget_get_text(widget);

  if (!string) {
    fprintf(stderr, "FAILED: %s line <%d> string is NULL\n", TWIDGET, __LINE__);
    result++;
  }
  else if (strcmp(string, "Fandango")) {
    fprintf(stderr, "FAILED: %s line <%d> string <%s>\n", TWIDGET, __LINE__, string);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

static int activated = 0;

void on_activate(GedaEntry *entry)
{
  activated++;
}

int
check_overides (void)
{
  int result = 0;

  GedaEntryClass *entry_class;

  GtkWidget *widget = geda_entry_new ();

  entry_class = GEDA_ENTRY_GET_CLASS(widget);

  entry_class->activate       = on_activate;

  g_signal_emit_by_name(widget, "process-entry");

  if (!activated) {
    fprintf(stderr, "FAILED: %s line <%d> not activated\n", TWIDGET, __LINE__);
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
    }
  }
  return result;
}
