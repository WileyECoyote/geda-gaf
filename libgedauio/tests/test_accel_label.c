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
 * Date: March, 13, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_accel_label.h>
#include <geda_image_menu_item.h>

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

      label = gtk_label_get_label(GTK_LABEL(widget));

      if (strcmp(label, label_string)) {
        fprintf(stderr, "FAILED: accel_string <%s>\n", label);
        result++;
      }
    }
  }

  g_object_ref_sink(menu_item); /* menu_item is not attached to anything */
  g_object_unref(menu_item);    /* Destroy menu_item and widget */

  if (GEDA_IS_ACCEL_LABEL(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TWIDGET);
    result++;
  }

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
      fprintf(stderr, "Check constructors in src/widgets/accel_label.c");
      result   = subtotal;
      subtotal = 0;
    }
  }
  return result;
}
