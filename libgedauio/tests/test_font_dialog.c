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
 * Date: April, 19, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include <../include/geda_font_dialog.h>
#include <../include/geda_bulb.h>

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

  if (GEDA_IS_FONT_DIALOG(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TWIDGET);
    result++;
  }

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
main (int argc, char *argv[])
{
  int result = 0;
  int subtotal = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (gtk_init_check(&argc, &argv)) {

    subtotal = check_construction();
    if (subtotal) {
      fprintf(stderr, "Check %s constructors", TWIDGET);
      result   = subtotal;
      subtotal = 0;
    }
  }
  return result;
}
