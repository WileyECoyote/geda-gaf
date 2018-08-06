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

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>

#include <geda/geda.h>
#include <geda_menu.h>
#include <geda_menu_bar.h>
#include <geda_menu_item.h>
#include <geda_menu_shell.h>
#include <geda_tearoff_menu_item.h>

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

  /* Check instance identifier with NULL */

  if (GEDA_IS_MENU(NULL)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* Check instance identifier with pointer to low memory location */

  unsigned int bad_address = 0x7FFF0;

  if (GEDA_IS_MENU(&bad_address)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

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

static GtkWidget *top_Window;

static GtkWidget *main_window()
{
  GtkWidget *vbox;
  GtkWidget *menubar;

  top_Window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (top_Window), vbox);
  gtk_widget_show (vbox);

  menubar = geda_menu_bar_new ();
  gtk_box_pack_start (GTK_BOX (vbox), menubar, FALSE, TRUE, 0);

  gtk_widget_show (menubar);
  gtk_widget_show (top_Window);

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

  geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menu_item), menu);
  geda_menu_append (menu_bar, widget0);

  widget1    = geda_menu_item_new_with_mnemonic("_Cherry");
  geda_menu_append (menu, widget1);
  gtk_widget_show (widget1);

  widget2    = geda_menu_item_new_with_mnemonic("_Apple");
  geda_menu_append (menu, widget2);
  gtk_widget_show (widget2);

  widget3    = geda_menu_item_new_with_mnemonic("_Pears");
  geda_menu_append (menu, widget3);
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
  geda_menu_widget_set_accel_path (menu, "<trees>/Fruit");

  /* Get the path from an item under the menu */
  accel_path = geda_menu_item_get_accel_path (GEDA_MENU_ITEM(widget1));

  if (strcmp(accel_path, "<trees>/Fruit/Cherry")) {
    fprintf(stderr, "FAILED: line <%d> accel_path %s\n", __LINE__, accel_path);
    result++;
  }

  /* Get the path from the menu */
  accel_path = geda_menu_widget_get_accel_path (menu);

  if (strcmp(accel_path, "<trees>/Fruit")) {
    fprintf(stderr, "FAILED: line <%d> accel_path %s\n", __LINE__, accel_path);
    result++;
  }

  /* --------------------- toplevel -------------------- */

  GtkWidget *toplevel;

  toplevel = geda_menu_get_toplevel(GEDA_MENU(menu));

  if (!toplevel) {
    fprintf(stderr, "FAILED: %s line <%d>\n", TWIDGET, __LINE__);
    result++;
  }
  else if (toplevel != top_Window) {
    fprintf(stderr, "FAILED: %s line <%d> <%p>\n", TWIDGET, __LINE__, toplevel);
    result++;
  }

    if (item != widget1) {
    fprintf(stderr, "FAILED: line <%d> get_active %p\n", __LINE__, item);
    result++;
  }

  /* ------------------- tearoff_state ----------------- */

  GtkWidget *tearoff_item;

  /* The menu is not a tear-off menu item */
  if (geda_menu_widget_get_tearoff_state(menu)) {
    fprintf(stderr, "FAILED: %s line <%d> get_tearoff_state\n", TWIDGET, __LINE__);
    result++;
  }

  tearoff_item = geda_tearoff_menu_item_new ();

  if (!GEDA_IS_TEAROFF_MENU_ITEM(tearoff_item)) {
    fprintf(stderr, "FAILED: %s line <%d> tearoff_menu_item\n", TWIDGET, __LINE__);
    result++;
  }
  else {

    GtkWidget *submenu;
    GtkWidget *widget4;

    submenu  = geda_menu_new ();

    geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (tearoff_item), submenu);
    geda_menu_shell_append (GEDA_MENU_SHELL (menu_bar), tearoff_item);

    gtk_widget_show (tearoff_item);
    gtk_widget_show (submenu);

    widget4  = geda_menu_item_new_with_mnemonic("_Tomato");
    geda_menu_shell_append (GEDA_MENU_SHELL (submenu), widget4);
    gtk_widget_show (widget4);

    geda_menu_widget_set_tearoff_state(submenu, TRUE);

    if (!geda_menu_widget_get_tearoff_state(submenu)) {
      fprintf(stderr, "FAILED: %s line <%d> set torn\n", TWIDGET, __LINE__);
      result++;
    }

    geda_menu_widget_set_tearoff_state(submenu, FALSE);

    if (geda_menu_widget_get_tearoff_state(submenu)) {
      fprintf(stderr, "FAILED: %s line <%d> set not torn\n", TWIDGET, __LINE__);
      result++;
    }

    /* ----------------------- parent --------------------- */

    GtkWidget *parent_item;

    parent_item = geda_menu_get_parent_item(GEDA_MENU(submenu));

    if (parent_item != tearoff_item) {
      fprintf(stderr, "FAILED: %s line <%d> get_parent\n", TWIDGET, __LINE__);
      result++;
    }

    geda_menu_set_parent_item(GEDA_MENU(submenu), NULL);

    if (geda_menu_get_parent_item(GEDA_MENU(submenu))) {
      fprintf(stderr, "FAILED: %s line <%d> set_parent\n", TWIDGET, __LINE__);
      result++;
    }

    /* ----------------- attach_widget ----------------- */

    parent_item = geda_menu_get_attach_widget(GEDA_MENU(submenu));

    if (parent_item != tearoff_item) {
      fprintf(stderr, "FAILED: %s line <%d> get_parent\n", TWIDGET, __LINE__);
      result++;
    }

    /* ------------------- for_attach_ ----------------- */

    GList *attached;

    attached = geda_menu_get_for_attach_widget(tearoff_item);

    if (!attached) {
      fprintf(stderr, "FAILED: %s line <%d> get_for_attach\n", TWIDGET, __LINE__);
      result++;
    }
    else if (attached->data != submenu) {
      fprintf(stderr, "FAILED: %s line <%d> get_for_attach\n", TWIDGET, __LINE__);
      result++;
    }
  }

  /* ---------------- reserve_toggle_size -------------- */

  /* The default is yes */
  if (!geda_menu_get_reserve_toggle_size((GedaMenu*)menu)) {
    fprintf(stderr, "FAILED: %s line <%d> reserve_toggle_size\n", TWIDGET, __LINE__);
    result++;
  }

  geda_menu_set_reserve_toggle_size((GedaMenu*)menu, FALSE);

  if (geda_menu_get_reserve_toggle_size((GedaMenu*)menu)) {
    fprintf(stderr, "FAILED: %s line <%d> reserve_toggle_size\n", TWIDGET, __LINE__);
    result++;
  }

  geda_menu_set_reserve_toggle_size((GedaMenu*)menu, TRUE);

  if (!geda_menu_get_reserve_toggle_size((GedaMenu*)menu)) {
    fprintf(stderr, "FAILED: %s line <%d> reserve_toggle_size\n", TWIDGET, __LINE__);
    result++;
  }

  /* ----------------------- title --------------------- */

  const char *title;

  title = geda_menu_widget_get_title(menu);

  if (title) {
    fprintf(stderr, "FAILED: %s line <%d> title <%p>\n", TWIDGET, __LINE__, title);
    result++;
  }

  geda_menu_widget_set_title(menu, "title");

  title = geda_menu_widget_get_title(menu);

  if (!title) {
    fprintf(stderr, "FAILED: %s line <%d> title <%p>\n", TWIDGET, __LINE__, title);
    result++;
  }
  else {
    if (strcmp(title, "title") != 0) {
      fprintf(stderr, "FAILED: %s line <%d> title <%p>\n", TWIDGET, __LINE__, title);
      result++;
    }
  }

  /* ----------------------- screen --------------------- */

  GdkScreen *screen;

  geda_menu_set_screen((GedaMenu*)menu, NULL);

  if (GEDA_OBJECT_GET_DATA (menu, "menu-explicit-screen")) {
    fprintf(stderr, "FAILED: %s line <%d> screen NULL\n", TWIDGET, __LINE__);
    result++;
  }

  screen = gtk_widget_get_screen (menu);

  geda_menu_set_screen((GedaMenu*)menu, screen);

  if (GEDA_OBJECT_GET_DATA (menu, "menu-explicit-screen") != screen) {
    fprintf(stderr, "FAILED: %s line <%d> screen\n", TWIDGET, __LINE__);
    result++;
  }

  /* ----------------------- screen --------------------- */

  int monitor = 88;

  monitor = geda_menu_get_monitor((GedaMenu*)menu);

  if (monitor == 88) {
    fprintf(stderr, "FAILED: %s line <%d> monitor\n", TWIDGET, __LINE__);
    result++;
  }

  int pri_monitor = gdk_screen_get_primary_monitor (screen);

  geda_menu_set_monitor((GedaMenu*)menu, pri_monitor);

  monitor = geda_menu_get_monitor((GedaMenu*)menu);

  if (monitor != pri_monitor) {
    fprintf(stderr, "FAILED: %s line <%d> monitor <%d>\n", TWIDGET, __LINE__, monitor);
    result++;
  }

  /* ---------------------------------------------------- */

  gtk_widget_destroy(gtk_widget_get_toplevel(widget0));

  return result;
}

static bool was_detached;

static void
popup_menu_detach (GtkWidget *menu_item, GedaMenu *menu)
{
  was_detached = TRUE;
}

int
check_methods ()
{
  int result = 0;

  GtkWidget    *menu;
  GtkWidget    *menu_bar;
  GtkWidget    *widget0;
  GtkWidget    *widget1;
  GtkWidget    *widget2;
  GtkWidget    *widget3;
  GedaMenuItem *menu_item;

  widget0 = geda_menu_item_new_with_mnemonic("_Fruit");

  menu      = geda_menu_new ();
  menu_bar  = main_window();
  menu_item = GEDA_MENU_ITEM(widget0);

  geda_menu_append (menu_bar, widget0);

  widget1    = geda_menu_item_new_with_mnemonic("_Cherry");
  geda_menu_append (menu, widget1);
  gtk_widget_show (widget1);

  widget2    = geda_menu_item_new_with_mnemonic("_Apple");
  geda_menu_append (menu, widget2);
  gtk_widget_show (widget2);

  widget3    = geda_menu_item_new_with_mnemonic("_Pears");
  geda_menu_append (menu, widget3);
  gtk_widget_show (widget3);

  /* ----------- geda_menu_attach_to_widget ------------- */

  GtkWidget *attached;

  /* Alternative method to attach menu to a menu item */
  geda_menu_attach_to_widget ((GedaMenu*)menu, widget0, popup_menu_detach);

  attached = geda_menu_get_attach_widget((GedaMenu*)menu);

  if (attached != (GtkWidget*)menu_item) {
    fprintf(stderr, "FAILED: %s line <%d> attach_to_widget\n", TWIDGET, __LINE__);
    result++;
  }

  gtk_widget_show (menu);

  geda_menu_reorder_child((GedaMenu*)menu, widget3, 0);

  const GList *items = geda_menu_shell_get_children ((GedaMenuShell*)menu);

  if (items->data != widget3) {
    fprintf(stderr, "FAILED: %s line <%d> reorder_child\n", TWIDGET, __LINE__);
    result++;
  }

  geda_menu_reorder_child((GedaMenu*)menu, widget2, 0);

  items = geda_menu_shell_get_children ((GedaMenuShell*)menu);

  if (items->data != widget2) {
    fprintf(stderr, "FAILED: %s line <%d> reorder_child\n", TWIDGET, __LINE__);
    result++;
  }

  /* ---------------------------------------------------- */

  was_detached = FALSE;

  gtk_widget_destroy(gtk_widget_get_toplevel(widget0));

  if (!was_detached) {
    fprintf(stderr, "FAILED: %s line <%d> menu_detach\n", TWIDGET, __LINE__);
    result++;
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

  if (argc > 1) {

     if (strcmp(argv[1], "-m") != 0) {
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
        result++;
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

      if (setjmp(point) == 0) {
        result = check_methods();
      }
      else {
        fprintf(stderr, "Caught signal checking methods in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
