/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_radio_menu_item.c
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
 * Date: May, 14, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_radio_menu_item.h>

#define TWIDGET "GedaRadioMenuItem"

/*! \file test_radio_menu_item.c
 *  \brief Tests for geda_radio_menu_item.c module
 */

int check_construction (void)
{
  int result = 0;
  GSList *group = NULL;

  GtkWidget *widget = geda_radio_menu_item_new(NULL);

  if (!GEDA_IS_RADIO_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    if (!GEDA_IS_CHECK_MENU_ITEM(widget)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    if (!GEDA_IS_MENU_ITEM(widget)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    g_object_ref_sink(widget); /* Sink reference to menu_item */
    g_object_unref(widget);    /* Does not destroy widget */
  }

  widget = NULL;

  widget = geda_radio_menu_item_new(group);

  if (!GEDA_IS_RADIO_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GedaRadioMenuItem  *radio_menu_item;

    GtkWidget *widget2, *widget3, *widget4, *widget5, *widget6;

    radio_menu_item = GEDA_RADIO_MENU_ITEM(widget);

    /* geda_radio_menu_item_new_from_widget */
    widget2 = geda_radio_menu_item_new_from_widget (radio_menu_item);

    if (!GEDA_IS_RADIO_MENU_ITEM(widget2)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* geda_radio_menu_item_new_with_label */
    widget3 = geda_radio_menu_item_new_with_label (group, "3");

    if (!GEDA_IS_RADIO_MENU_ITEM(widget3)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* geda_radio_menu_item_new_with_label_from_widget */
    widget4 = geda_radio_menu_item_new_with_label_from_widget (radio_menu_item, "4");

    if (!GEDA_IS_RADIO_MENU_ITEM(widget4)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* geda_radio_menu_item_new_with_mnemonic */
    widget5 = geda_radio_menu_item_new_with_mnemonic (group, "5");

    if (!GEDA_IS_RADIO_MENU_ITEM(widget5)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* geda_radio_menu_item_new_with_mnemonic_from_widget */
    widget6 = geda_radio_menu_item_new_with_mnemonic_from_widget(radio_menu_item, "6");

    if (!GEDA_IS_RADIO_MENU_ITEM(widget6)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }
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
      fprintf(stderr, "Check constructors in src/widgets/geda_radio_menu_item.c");
      result   = subtotal;
      subtotal = 0;
    }
  }
  return result;
}
