/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_menu.c
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
 * Date: June, 16, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_menu.h>
#include <geda_menu_bar.h>
#include <geda_menu_item.h>
#include <geda_menu_shell.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_menu.c"

#define TWIDGET "GedaMenu"

/*! \file test_menu.c
 *  \brief Tests for geda_menu.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_menu_new();

  if (!GEDA_IS_MENU(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_MENU_SHELL(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  return result;
}

static GtkWidget *main_window()
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

  GtkWidget    *menu;
  GtkWidget    *menu_bar;
  GtkWidget    *widget0;
  GtkWidget    *widget1;
  GtkWidget    *widget2;
  GtkWidget    *widget3;
  GedaMenuItem *menu_item;
  GtkWidget    *item;

  widget0 = geda_menu_item_new_with_mnemonic("_Fruit");

  menu      = geda_menu_new ();
  menu_bar  = main_window();
  menu_item = GEDA_MENU_ITEM(widget0);

  geda_menu_item_set_submenu (GEDA_MENU_ITEM (menu_item), menu);
  geda_menu_shell_append (GEDA_MENU_SHELL (menu_bar), widget0);

  widget1    = geda_menu_item_new_with_mnemonic("_Cherry");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu), widget1);
  gtk_widget_show (widget1);

  widget2    = geda_menu_item_new_with_mnemonic("_Apple");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu), widget2);
  gtk_widget_show (widget2);

  widget3    = geda_menu_item_new_with_mnemonic("_Pears");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu), widget3);
  gtk_widget_show (widget3);

  gtk_widget_show (menu);

  /* -------------------- active -------------------- */

  item = geda_menu_widget_get_active (menu);
  if (item != widget1) {
    fprintf(stderr, "FAILED: line <%d> get_active %p\n", __LINE__, item);
    result++;
  }

  geda_menu_widget_set_active(menu, 1);

  item = geda_menu_widget_get_active (menu);
  if (item != widget2) {
    fprintf(stderr, "FAILED: line <%d> set_active %s\n", __LINE__, TWIDGET);
    result++;
  }

  geda_menu_widget_set_active(menu, 0);

  item = geda_menu_widget_get_active (menu);
  if (item != widget1) {
    fprintf(stderr, "FAILED: line <%d> set_active %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* -------------------- accel_group -------------------- */

  GtkAccelGroup *accel_group;
  GtkAccelGroup *group;

  accel_group = gtk_accel_group_new ();

  geda_menu_widget_set_accel_group (menu, accel_group);

  group = geda_menu_widget_get_accel_group (menu);

  if (group != accel_group) {
    fprintf(stderr, "FAILED: line <%d> accel_group %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* -------------------- accel_path -------------------- */

  const char *accel_path;

  /* Sets accelerator path on sub-menu items */
  geda_menu_set_accel_path (GEDA_MENU(menu), "<trees>/Fruit");

  /* Get the path from an item under the menu */
  accel_path = geda_menu_item_get_accel_path (GEDA_MENU_ITEM(widget1));

  if (strcmp(accel_path, "<trees>/Fruit/Cherry")) {
    fprintf(stderr, "FAILED: line <%d> accel_path %s\n", __LINE__, accel_path);
    result++;
  }

  /* Get the path from the menu */
  accel_path = geda_menu_get_accel_path (GEDA_MENU(menu));

  if (strcmp(accel_path, "<trees>/Fruit")) {
    fprintf(stderr, "FAILED: line <%d> accel_path %s\n", __LINE__, accel_path);
    result++;
  }

  gtk_widget_destroy(gtk_widget_get_toplevel(widget0));

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

  if (argc > 1) {

     if (strcmp(argv[1], "-m") ==0) {
       fprintf(stderr, "%s: The Yahoos are comming!", argv[0]);
     }
     else {
       fprintf(stderr, "%s: unknown option: <%s>", argv[0], argv[1]);
       result++;
     }
  }
  else {

    if (gtk_init_check(&argc, &argv)) {

      if (setjmp(point) == 0) {
        result = check_construction();
      }
      else {
        fprintf(stderr, "Caught signal checking constructors in %s\n\n", MUT);
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
