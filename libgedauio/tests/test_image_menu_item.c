/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_image_menu_item.c
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
 * Date: April, 24, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include <../include/geda_image_menu_item.h>
#include <../include/geda_bulb.h>

#include "print_xpm.h"
#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_image_menu_item.c"

#define TWIDGET "GedaImageMenuItem"

/*! \file test_image_menu_item.c
 *  \brief Tests for geda_image_menu_item.c module
 */

int check_construction (void)
{
  int result = 0;

  /* geda_image_menu_item_new */

  GtkWidget *widget = geda_image_menu_item_new ();

  if (!GEDA_IS_IMAGE_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (GEDA_IS_BULB(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to the widget */
  g_object_unref(widget);    /* Destroy the widget */

  if (GEDA_IS_IMAGE_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TWIDGET);
    result++;
  }

  widget = NULL;

  /* geda_image_menu_item_new_with_label */

  widget = geda_image_menu_item_new_with_label ("Casco");

  if (!GEDA_IS_IMAGE_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to the widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_image_menu_item_new_with_label */

  widget = geda_image_menu_item_new_with_mnemonic ("<b>Walrus</b>");

  if (!GEDA_IS_IMAGE_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to the widget */
  g_object_unref(widget);    /* Destroy the widget */

  widget = NULL;

  /* geda_image_menu_item_new_with_label */

  widget = geda_image_menu_item_new_from_stock (GTK_STOCK_DELETE, NULL);

  if (!GEDA_IS_IMAGE_MENU_ITEM(widget)) {
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

  GtkWidget *widget;

  widget = geda_image_menu_item_new_with_label ("Gammaridea");

  if (!GEDA_IS_IMAGE_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)widget;

    /* geda_image_menu_item_get_show_image */

    /* The default is FALSE */
    if (geda_image_menu_item_get_show_image (image_menu_item)) {
      fprintf(stderr, "FAILED: line <%d> get_show_image %s\n", __LINE__, TWIDGET);
      result++;
    }

    geda_image_menu_item_set_show_image(image_menu_item, TRUE);

    if (!geda_image_menu_item_get_show_image (image_menu_item)) {
      fprintf(stderr, "FAILED: line <%d> set_show_image %s\n", __LINE__, TWIDGET);
      result++;
    }

    GdkPixbuf *print_pixbuf;
    GtkWidget *print_image;

    print_pixbuf = gdk_pixbuf_new_from_xpm_data (print_xpm);
    print_image  = gtk_image_new_from_pixbuf (print_pixbuf);

    /* geda_image_menu_item_set_image */

    geda_image_menu_item_set_image(image_menu_item, print_image);

    /* geda_image_menu_item_get_image */

    GtkWidget *image = geda_image_menu_item_get_image(image_menu_item);

    if (image != print_image) {
      fprintf(stderr, "FAILED: line <%d> get_image %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* The default is FALSE */
    if (geda_image_menu_item_get_use_stock(image_menu_item)) {
      fprintf(stderr, "FAILED: line <%d> get_use_stock %s\n", __LINE__, TWIDGET);
      result++;
    }

    geda_image_menu_item_set_use_stock(image_menu_item, TRUE);

    if (!geda_image_menu_item_get_use_stock(image_menu_item)) {
      fprintf(stderr, "FAILED: line <%d> set_use_stock %s\n", __LINE__, TWIDGET);
      result++;
    }

    g_object_ref_sink(widget); /* Sink reference to the widget */
    g_object_unref(widget);    /* Destroy the widget */
  }

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
    }
  }
  return result;
}
