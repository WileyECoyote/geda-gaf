/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_menu_item.c
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
#include <geda_menu.h>
#include <geda_menu_bar.h>
#include <geda_menu_shell.h>
#include <geda_menu_item.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_menu_item.c"

#define TWIDGET "GedaMenuItem"

/*! \file test_menu_item.c
 *  \brief Tests for geda_menu_item.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_menu_item_new();

  /* Check instance identifier with NULL */

  if (GEDA_IS_MENU_ITEM(NULL)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* Check instance identifier with pointer to low memory location */

  unsigned int bad_address = 0x7FFF0;

  if (GEDA_IS_MENU_ITEM(&bad_address)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GTK_IS_BIN(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  widget = geda_menu_item_new_with_label(NULL);

  if (!GEDA_IS_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  widget = geda_menu_item_new_with_mnemonic("_Cherry");

  if (!GEDA_IS_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  return result;
}

GtkWidget *main_window()
{
  GtkWidget *vbox;
  GtkWidget *window;
  GtkWidget *menubar;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_show (vbox);

  menubar = geda_menu_bar_new ();
  gtk_box_pack_start (GTK_BOX (vbox), menubar, FALSE, TRUE, 0);

  gtk_widget_show (menubar);
  gtk_widget_show (window);

  return menubar;
}

int
check_accessors ()
{
  int result = 0;

  GtkWidget *widget = geda_menu_item_new_with_mnemonic("_Cherry");

  if (!GEDA_IS_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GtkWidget    *menu;
    GtkWidget    *menu_bar;
    GedaMenuItem *menu_item;
    GdkWindow    *event_window;

    menu      = geda_menu_new ();
    menu_bar  = main_window();
    menu_item = GEDA_MENU_ITEM(widget);

    geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menu_item), menu);
    geda_menu_append (menu_bar, widget);

    gtk_widget_show (widget);
    gtk_widget_show (menu);

    event_window = geda_menu_item_get_event_window (menu_item);

    if (!GDK_IS_WINDOW(event_window)) {
      fprintf(stderr, "FAILED: line <%d> event event_window %s\n", __LINE__, TWIDGET);
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
    }
  }
  return result;
}
