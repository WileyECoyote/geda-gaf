/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_accel_label.c
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
 * Date: March, 13, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_accel_label.h>
#include <geda_image_menu_item.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_accel_label.c"

#define TWIDGET "GedaAccelLabel"

/*! \file test_accel_label.c
 *  \brief Tests for accel_label.c module
 */

int check_construction (void)
{
  int result = 0;

  const char *label_string   = "Geda_Accel_Label";
  const char *multikey_accel = "G_A";

  GtkWidget *menu_item = geda_image_menu_item_new_with_label("GedaMenuItem");

  GtkWidget *child = gtk_bin_get_child(GTK_BIN(menu_item));

  gtk_container_remove (GTK_CONTAINER(menu_item), child);

  GtkWidget *widget = g_object_new (GEDA_TYPE_ACCEL_LABEL,
                                    "use-underline", TRUE,
                                    "xalign", 0.0,
                                    "visible", TRUE,
                                    "parent", menu_item,
                                    "label", label_string,
                                    "accel-string", multikey_accel,
                                    NULL);

  if (!GEDA_IS_ACCEL_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_LABEL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  if (!GEDA_IS_ACCEL_LABEL(widget)) {
    fprintf(stderr, "FAILED: is a %s <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else {

    GedaAccelLabel *accel_label = (GedaAccelLabel*)widget;

    if (!accel_label->accel_string) {
      fprintf(stderr, "FAILED: %s, accel_string is NULL\n", TWIDGET);
      result++;
    }
    else {

      const char *accel_string = accel_label->accel_string;
      const char *label;

      if (strcmp(accel_string, "G A")) {
        fprintf(stderr, "FAILED: accel_string <%s>\n", accel_string);
        result++;
      }

      label = geda_label_get_label(GEDA_LABEL(widget));

      if (strcmp(label, label_string)) {
        fprintf(stderr, "FAILED: accel_string <%s>\n", label);
        result++;
      }
    }
  }

  g_object_ref_sink(menu_item); /* menu_item is not attached to anything */
  g_object_unref(menu_item);    /* Destroy menu_item and widget */

  return result;
}

int check_accessors ()
{
  int result = 0;

  const char *multikey_accel = "G_A";

  GtkWidget    *label;
  GtkWidget    *widget;
  GedaMenuItem *menu_item;

  widget = geda_menu_item_new_with_label("GedaMenuItem");

  menu_item = GEDA_MENU_ITEM(widget);

  label = geda_menu_item_get_label_widget(menu_item);

  if (!GEDA_IS_LABEL(label)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_ACCEL_LABEL(label)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaAccelLabel *accel_label;
    GtkWidget *accel_widget;

    accel_label = (GedaAccelLabel*)label;

    /* geda_accel_label_get_accel_widget */

    accel_widget = geda_accel_label_get_accel_widget(accel_label);

    if (!GEDA_IS_MENU_ITEM(accel_widget)) {
      fprintf(stderr, "FAILED: line <%d> get_accel_widget %s\n", __LINE__, TWIDGET);
      result++;
    }
    else if (accel_widget != widget) {
      fprintf(stderr, "FAILED: line <%d> get_accel_widget %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* geda_accel_label_set_accel_widget */

    GtkWidget *widget2;

    widget2 = geda_menu_item_new_with_label("GedaMenuItem");

    g_object_ref(accel_label);

    gtk_container_remove (GTK_CONTAINER(widget), label);

    geda_accel_label_set_accel_widget(accel_label, widget2);

    accel_widget = geda_accel_label_get_accel_widget(accel_label);

    if (!GEDA_IS_MENU_ITEM(accel_widget)) {
      fprintf(stderr, "FAILED: line <%d> set_accel_widget %s\n", __LINE__, TWIDGET);
      result++;
    }
    else if (accel_widget != widget2) {
      fprintf(stderr, "FAILED: line <%d> set_accel_widget %s\n", __LINE__, TWIDGET);
      result++;
    }
    else {

      accel_widget = geda_accel_label_get_accel_widget(accel_label);

      if (!GEDA_IS_MENU_ITEM(accel_widget)) {
        fprintf(stderr, "FAILED: line <%d> set_accel_widget %s\n", __LINE__, TWIDGET);
        result++;
      }
    }

    /* deferring geda_accel_label_get_accel_width, see below */

    /* geda_accel_label_get_accel_string */
    const char *accel_string;

    accel_string = geda_accel_label_get_accel_string(accel_label);

    if (accel_string) {
      fprintf(stderr, "FAILED: line <%d> get_accel_string %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* geda_accel_label_set_accel_string */

    geda_accel_label_set_accel_string(accel_label, multikey_accel);

    accel_string = geda_accel_label_get_accel_string(accel_label);

    if (!accel_string) {
      fprintf(stderr, "FAILED: line <%d> set_accel_string NULL %s\n", __LINE__, TWIDGET);
      result++;
    }
    else if (strcmp(accel_string, "G A")) {
      fprintf(stderr, "FAILED: line <%d> set_accel_string %s <%s>\n", __LINE__, TWIDGET, accel_string);
      result++;
    }

  }

  g_object_ref_sink(menu_item); /* menu_item is not attached to anything */
  g_object_unref(menu_item);    /* Destroy menu_item and widget */

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
