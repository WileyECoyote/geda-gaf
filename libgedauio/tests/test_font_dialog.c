/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_font_dialog.c
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
 * Date: April, 19, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include <../include/geda_font_dialog.h>
#include <../include/geda_bulb.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_font_dialog.c"

#define TWIDGET "GedaFontDialog"

/*! \file test_font_dialog.c
 *  \brief Tests for geda_font_dialog.c module
 */

int check_construction (void)
{
  int result = 0;

  /* geda_font_dialog_new */

  GtkWidget *widget = geda_font_dialog_new ();

  if (!GEDA_IS_FONT_DIALOG(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GTK_IS_DIALOG(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  gtk_widget_destroy (widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_font_dialog_new_with_title */

  widget = geda_font_dialog_new_with_title (NULL);

  if (!GEDA_IS_FONT_DIALOG(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  gtk_widget_destroy (widget);    /* Destroy the widget */

  widget = NULL;

  widget = geda_font_dialog_new_with_title ("Testing");

  if (!GEDA_IS_FONT_DIALOG(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  gtk_widget_destroy (widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_font_dialog_new_with_font */

  widget = geda_font_dialog_new_with_font_name ("Arial");

  if (!GEDA_IS_FONT_DIALOG(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  gtk_widget_destroy (widget);    /* Destroy the widget */

  widget = NULL;

  return result;
}

int
check_properties (void)
{
  int result = 0;

  GtkWidget *widget = geda_font_dialog_new_with_font_name ("Sans");

  if (!GEDA_IS_FONT_DIALOG(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GdkFont *font;

    g_object_get(widget, "font", &font, NULL);

    if (!font) {
      fprintf(stderr, "FAILED: line <%d> get font <%s>\n", __LINE__, TWIDGET);
      result++;
    }

    PangoFontDescription *font_desc;

    g_object_get(widget, "font-desc", &font_desc, NULL);

    if (!font_desc) {
      fprintf(stderr, "FAILED: line <%d> get \"font-desc\" property <%s>\n", __LINE__, TWIDGET);
      result++;
    }
    else {

      GdkFont *font2;

      font2 = gdk_font_from_description (font_desc);

#ifndef __MINGW32__

      if (!gdk_font_equal(font, font2)) {

#else

      int id1, id2;

      id1 = gdk_font_id (font);
      id2 = gdk_font_id (font2);

      if (id1 != id2) {

#endif

        fprintf(stderr, "FAILED: line <%d> get \"font-desc\" property <%s>\n", __LINE__, TWIDGET);

        result++;
      }

      gdk_font_unref(font2);
    }

    PangoFontDescription *pft = pango_font_description_new();

    g_object_set(widget, "font-desc", &pft, NULL);

    g_object_get(widget, "font-desc", &font_desc, NULL);

    if (!font_desc) {
      fprintf(stderr, "FAILED: line <%d> set \"font-desc\" property <%s>\n", __LINE__, TWIDGET);
      result++;
    }
    else {

      if (pango_font_description_equal(font_desc, pft)) {
        fprintf(stderr, "FAILED: line <%d> set \"font-desc\" property <%s>\n", __LINE__, TWIDGET);
        result++;
      }
    }

    /* Destroy the widget and recreate a new one becasue a new font description
     * was set on the old widget */

    g_object_ref_sink(widget); /* Sink reference to the widget */
    g_object_unref(widget);    /* Destroy the widget */

    widget = geda_font_dialog_new_with_font_name ("Sans");

    char *font_name;

    g_object_get(widget, "font-name", &font_name, NULL);

    if (!font_name) {
      fprintf(stderr, "FAILED: get font name property NULL\n");
      result++;
    }
    else {
      if (strncmp(font_name, "Sans", 4)) {
        fprintf(stderr, "FAILED: get font name property <%s>\n", font_name);
        result++;
      }
    }

    g_object_set(widget, "font-name", "Monospace", NULL);

    g_object_get(widget, "font-name", &font_name, NULL);

    if (!font_name) {
      fprintf(stderr, "FAILED: set font name property NULL\n");
      result++;
    }
    else {
      if (strncmp(font_name, "Monospace", 9)) {
        fprintf(stderr, "FAILED: set font name property <%s>\n", font_name);
        result++;
      }
    }

    int font_size;

    geda_font_dialog_set_font_size(GEDA_FONT_DIALOG(widget), DEFAULT_FONT_SIZE);

    g_object_get(widget, "font-size", &font_size, NULL);

    if (font_size != DEFAULT_FONT_SIZE) {
      fprintf(stderr, "FAILED: get font size property <%d>\n", font_size);
      result++;
    }

    g_object_set(widget, "font-size", 13, NULL);

    g_object_get(widget, "font-size", &font_size, NULL);

    if (font_size != 13) {
      fprintf(stderr, "FAILED: get font size property <%d>!= 13\n", font_size);
      result++;
    }

    char *title;

    g_object_get(widget, "title", &title, NULL);

    if (title) {
      fprintf(stderr, "FAILED: get default title property not NULL\n");
      result++;
    }

    geda_font_dialog_set_title(widget, "Pick a Font");

    g_object_get(widget, "title", &title, NULL);

    if (!title) {
      fprintf(stderr, "FAILED: get default title property NULL\n");
      result++;
    }
    else {
      if (strncmp(title, "Pick a Font", 11)) {
        fprintf(stderr, "FAILED: get default title property <%s>\n", title);
        result++;
      }
    }

    g_object_set(widget, "title", "Select Font", NULL);

    g_object_get(widget, "title", &title, NULL);

    if (!title) {
      fprintf(stderr, "FAILED: set title property NULL\n");
      result++;
    }
    else {
      if (strncmp(title, "Select Font", 11)) {
        fprintf(stderr, "FAILED: set title property <%s>\n", title);
        result++;
      }
    }

    g_object_ref_sink(widget); /* Sink reference to the widget */
    g_object_unref(widget);    /* Destroy the widget */
  }

  return result;
}

int
check_accessors ()
{
  int result = 0;

  GtkWidget *widget = geda_font_dialog_new_with_font_name ("Arial");

  if (!GEDA_IS_FONT_DIALOG(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GdkFont *font1;
    GdkFont *font2;

    font1 = geda_font_dialog_get_font ((GedaFontDialog*)widget);

    if (!font1) {
      fprintf(stderr, "FAILED: line <%d> get_font %s\n", __LINE__, TWIDGET);
      result++;
    }

    // "-adobe-times-bold-i-normal--12-120-75-75-p-68-iso10646-1"
    font1 = gdk_font_load ("12x24");

    if (!font1) {
      fprintf(stderr, "GdkFont failure <%d> get_font %s\n", __LINE__, TWIDGET);
      return 0;
    }

    g_object_ref_sink(widget); /* Sink reference to the widget */
    g_object_unref(widget);    /* Destroy the widget */
  }

  return result;
}

int main (int argc, char *argv[])
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
        result = check_properties();
      }
      else {
        fprintf(stderr, "Caught signal checking properties in %s\n\n", MUT);
        return 1;
      }
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
