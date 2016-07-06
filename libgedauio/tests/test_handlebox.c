/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_handlebox.c
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
 * Date: April, 21, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include <../include/geda_handlebox.h>
#include <../include/geda_toolbar.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_handlebox.c"

#define TWIDGET "GedaHandleBox"

/*! \file test_handlebox.c
 *  \brief Tests for geda_handlebox.c module
 */

int check_construction (void)
{
  int result = 0;

  /* geda_handlebox_new */

  GtkWidget *widget = geda_handle_box_new ();

  if (!GEDA_IS_HANDLE_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GTK_IS_BIN(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* Is not a toolbar */
  if (GEDA_IS_TOOLBAR(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to the widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

int
check_accessors ()
{
  int result = 0;

  GtkWidget *widget = geda_handle_box_new ();

  if (!GEDA_IS_HANDLE_BOX(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    int value;

    GedaHandleBox *handlebox = (GedaHandleBox*)widget;

    geda_handle_box_set_shadow_type(handlebox, 0); // GTK_SHADOW_NONE

    value = geda_handle_box_get_shadow_type(handlebox);

    if (value) {
      fprintf(stderr, "FAILED: line <%d> %s shadow_type <%d>\n", __LINE__, TWIDGET, value);
      result++;
    }

    geda_handle_box_set_shadow_type(handlebox, 1); // GTK_SHADOW_IN

    value = geda_handle_box_get_shadow_type(handlebox);

    if (!value) {
      fprintf(stderr, "FAILED: line <%d> %s shadow_type <%d>\n", __LINE__, TWIDGET, value);
      result++;
    }

    geda_handle_box_set_handle_position(handlebox, 0); // GTK_POS_LEFT

    value = geda_handle_box_get_handle_position(handlebox);

    if (value) {
      fprintf(stderr, "FAILED: line <%d> %s handle_position <%d>\n", __LINE__, TWIDGET, value);
      result++;
    }

    geda_handle_box_set_handle_position(handlebox, 2); // GTK_POS_TOP

    value = geda_handle_box_get_handle_position(handlebox);

    if (value - 2) {
      fprintf(stderr, "FAILED: line <%d> %s handle_position <%d>\n", __LINE__, TWIDGET, value);
      result++;
    }

    geda_handle_box_set_snap_edge(handlebox, 0); // GTK_POS_LEFT

    value = geda_handle_box_get_snap_edge(handlebox);

    if (value) {
      fprintf(stderr, "FAILED: line <%d> %s handle_position <%d>\n", __LINE__, TWIDGET, value);
      result++;
    }

    geda_handle_box_set_snap_edge(handlebox, 1); // GTK_POS_RIGHT

    value = geda_handle_box_get_snap_edge(handlebox);

    if (value - 1) {
      fprintf(stderr, "FAILED: line <%d> %s handle_position <%d>\n", __LINE__, TWIDGET, value);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to the widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

int
check_integration ()
{
  int result = 0;

  GtkWidget *widget = geda_handle_box_new ();

  GtkWidget *bar_widget = geda_toolbar_new(0);

  gtk_container_add (GTK_CONTAINER  (widget), bar_widget);

  GtkToolbar *toolbar = geda_handle_box_get_toolbar (GEDA_HANDLE_BOX(widget));

  if (!toolbar) {
    fprintf(stderr, "FAILED: line <%d> %s handle_box_get_toolbar\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GtkToolbar *bin_child = (GtkToolbar*)gtk_bin_get_child(GTK_BIN(widget));

    if (toolbar != bin_child) {
      fprintf(stderr, "FAILED: line <%d> %s handle_box_get_toolbar\n", __LINE__, TWIDGET);
    result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to the widget */
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

      if (setjmp(point) == 0) {
        result = check_integration();
      }
      else {
        fprintf(stderr, "Caught signal checking accessors in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
