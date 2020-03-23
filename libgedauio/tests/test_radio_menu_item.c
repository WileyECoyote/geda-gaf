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

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <../include/geda_menu.h>
#include <../include/geda_menu_bar.h>
#include <../include/geda_menu_item.h>
#include <../include/geda_radio_menu_item.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_radio_menu_item.c"

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
    widget2 = geda_radio_menu_item_new_from_widget (widget);

    if (!GEDA_IS_RADIO_MENU_ITEM(widget2)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }
    else {

      GSList *list = geda_radio_menu_item_get_group(GEDA_RADIO_MENU_ITEM(widget2));

      if (!list) {
        fprintf(stderr, "FAILED: line <%d> get_group in %s\n", __LINE__, TWIDGET);
        result++;
      }
      else {

        int len = g_slist_length(list);

        if (len != 2) {
          fprintf(stderr, "FAILED: line <%d> list length=%d\n", __LINE__, len);
          result++;
        }
      }
    }

    group = geda_radio_menu_item_get_group(radio_menu_item);

    /* geda_radio_menu_item_new_with_label */
    widget3 = geda_radio_menu_item_new_with_label (group, "3");

    if (!GEDA_IS_RADIO_MENU_ITEM(widget3)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }
    else {

      GSList *list = geda_radio_menu_item_get_group(GEDA_RADIO_MENU_ITEM(widget3));

      if (!list) {
        fprintf(stderr, "FAILED: line <%d> get_group in %s\n", __LINE__, TWIDGET);
        result++;
      }
      else {

        int len = g_slist_length(list);

        if (g_slist_length(list) != 3) {
          fprintf(stderr, "FAILED: line <%d> list length=%d\n", __LINE__, len);
          result++;
        }
      }
    }

    /* geda_radio_menu_item_new_with_label_from_widget */
    widget4 = geda_radio_menu_item_new_with_label_from_widget (widget, "4");

    if (!GEDA_IS_RADIO_MENU_ITEM(widget4)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }
    else {

      GSList *list = geda_radio_menu_item_get_group(GEDA_RADIO_MENU_ITEM(widget4));

      if (!list) {
        fprintf(stderr, "FAILED: line <%d> get_group in %s\n", __LINE__, TWIDGET);
        result++;
      }
      else {

        int len = g_slist_length(list);

        if (g_slist_length(list) != 4) {
          fprintf(stderr, "FAILED: line <%d> list length=%d\n", __LINE__, len);
          result++;
        }
      }
    }

    group = geda_radio_menu_item_get_group(radio_menu_item);

    /* geda_radio_menu_item_new_with_mnemonic */
    widget5 = geda_radio_menu_item_new_with_mnemonic (group, "5");

    if (!GEDA_IS_RADIO_MENU_ITEM(widget5)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }
    else {

      GSList *list = geda_radio_menu_item_get_group(GEDA_RADIO_MENU_ITEM(widget5));

      if (!list) {
        fprintf(stderr, "FAILED: line <%d> get_group in %s\n", __LINE__, TWIDGET);
        result++;
      }
      else {

        int len = g_slist_length(list);

        if (g_slist_length(list) != 5) {
          fprintf(stderr, "FAILED: line <%d> list length=%d\n", __LINE__, len);
          result++;
        }
      }
    }

    /* geda_radio_menu_item_new_with_mnemonic_from_widget */
    widget6 = geda_radio_menu_item_new_with_mnemonic_from_widget(widget, "6");

    if (!GEDA_IS_RADIO_MENU_ITEM(widget6)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }
    else {

      GSList *list = geda_radio_menu_item_get_group(GEDA_RADIO_MENU_ITEM(widget6));

      if (!list) {
        fprintf(stderr, "FAILED: line <%d> get_group in %s\n", __LINE__, TWIDGET);
        result++;
      }
      else {

        int len = g_slist_length(list);

        if (g_slist_length(list) != 6) {
          fprintf(stderr, "FAILED: line <%d> list length=%d\n", __LINE__, len);
          result++;
        }
      }
    }
  }

  return result;
}

static  GtkAccelGroup *accel_group;

GtkWidget *main_window()
{
  GtkWidget *vbox;
  GtkWidget *window;
  GtkWidget *menubar;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  /* Create a GtkAccelGroup and add it to the window. */
  accel_group = gtk_accel_group_new();
  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_show (vbox);

  menubar = geda_menu_bar_new ();
  gtk_box_pack_start (GTK_BOX (vbox), menubar, FALSE, TRUE, 0);

  gtk_widget_show (menubar);
  gtk_widget_show (window);

  gtk_window_resize (GTK_WINDOW (window), 250, 150);

  return menubar;
}

int
check_accessors ()
{
  int result = 0;
  int len;

  GSList *group = NULL;

  GtkWidget *widget1 = geda_radio_menu_item_new_with_label (NULL, "_Moschata");
  GtkWidget *widget2 = geda_radio_menu_item_new_with_label (NULL, "_Pepo");

  group = geda_radio_menu_item_get_group(GEDA_RADIO_MENU_ITEM(widget1));

  /* geda_radio_menu_item_set_group */

  geda_radio_menu_item_set_group(GEDA_RADIO_MENU_ITEM(widget2), group);

  group = NULL;

  group = geda_radio_menu_item_get_group(GEDA_RADIO_MENU_ITEM(widget1));

  len = g_slist_length(group);

  if (g_slist_length(group) != 2) {
    fprintf(stderr, "FAILED: line <%d> group length=%d\n", __LINE__, len);
    result++;
  }

  /* Base Class */

  GedaMenuItem *menu_item;

  menu_item = GEDA_MENU_ITEM(widget2);

  /* geda_menu_item_get_label */

  const char *text;

  text =  geda_menu_item_get_label  (menu_item);

  if (!text) {
    fprintf(stderr, "FAILED: line <%d> get_label %s\n", __LINE__, TWIDGET);
    result++;
  }
  else if (strcmp(text, "_Pepo")) {
    fprintf(stderr, "FAILED: %s line <%d> text <%s>\n", TWIDGET, __LINE__, text);
    result++;
  }

  return result;
}

int check_query()
{
  int result = 0;

  GtkWidget *widget = geda_radio_menu_item_new_with_label(NULL, "_Texana");

  if (!GEDA_IS_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GtkWidget    *menu;
    GtkWidget    *menu_bar;
    GedaMenuItem *menu_item;

    menu      = geda_menu_new ();
    menu_bar  = main_window();
    menu_item = GEDA_MENU_ITEM(widget);

    geda_menu_append (menu_bar, widget);

    geda_menu_item_set_submenu_widget (menu_item, menu);

    gtk_widget_show (widget);
    gtk_widget_show (menu);

    /* geda_menu_item_get_toggle_size */

    unsigned short toggle_size;

    toggle_size = geda_menu_item_get_toggle_size (menu_item);

    if (toggle_size) {
      fprintf(stderr, "FAILED: line <%d> get_toggle_size %s\n", __LINE__, TWIDGET);
      result++;
    }

    gtk_widget_destroy(gtk_widget_get_toplevel(widget));
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

      if (setjmp(point) == 0) {
        result = check_query();
      }
      else {
        fprintf(stderr, "Caught signal in query in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
