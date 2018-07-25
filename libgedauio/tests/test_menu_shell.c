/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_menu_shell.c
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
 * Date: March, 20, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

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
#define MUT "src/widgets/geda_menu_shell.c"

#define TWIDGET "GedaMenuShell"

/*! \file test_menu_shell.c
 *  \brief Tests for geda_menu_shell.c module
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

  if (!GTK_IS_CONTAINER(widget)) {
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

  GtkWidget     *menu;
  GtkWidget     *menu_bar;
  GtkWidget     *widget0;
  GtkWidget     *widget1;
  GtkWidget     *widget2;
  GtkWidget     *widget3;
  GedaMenuItem  *menu_item;
  GedaMenuShell *menu_shell;
  GtkWidget     *item;

  widget0 = geda_menu_item_new_with_mnemonic("_Fruit");

  menu       = geda_menu_new ();
  menu_bar   = main_window();
  menu_item  = GEDA_MENU_ITEM(widget0);
  menu_shell = GEDA_MENU_SHELL (menu);

  geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menu_item), menu);
  geda_menu_shell_append (GEDA_MENU_SHELL (menu_bar), widget0);

  widget2    = geda_menu_item_new_with_mnemonic("_Cherry");
  gtk_container_add (GTK_CONTAINER (menu_shell), widget2);
  gtk_widget_show (widget2);

  widget1    = geda_menu_item_new_with_mnemonic("_Apple");
  geda_menu_shell_prepend (menu_shell, widget1);
  gtk_widget_show (widget1);

  widget3    = geda_menu_item_new_with_mnemonic("_Pears");
  geda_menu_shell_append (menu_shell, widget3);
  gtk_widget_show (widget3);

  gtk_widget_show (menu);

  int value;

  /* -------------------- select_first -------------------- */

  geda_menu_shell_select_first (menu_shell, TRUE);

  item = geda_menu_shell_get_selected_item (menu_shell);

  if (item != widget1) {
    fprintf(stderr, "FAILED: %s line <%d> select_first\n", TWIDGET, __LINE__);
    result++;
  }

  /* -------------------- select_item -------------------- */

  geda_menu_shell_select_item (menu_shell, widget2);

  item = geda_menu_shell_get_selected_item (menu_shell);

  if (item != widget2) {
    fprintf(stderr, "FAILED: %s line <%d> select_item\n", TWIDGET, __LINE__);
    result++;
  }

  /* -------------------- select_last -------------------- */

  geda_menu_shell_select_last (menu_shell, TRUE);

  item = geda_menu_shell_get_selected_item (menu_shell);

  if (item != widget3) {
    fprintf(stderr, "FAILED: %s line <%d> select_last\n", TWIDGET, __LINE__);
    result++;
  }

  /* -------------------- take_focus -------------------- */

  value = geda_menu_shell_get_take_focus (menu_shell);
  if (!value) {  /* take_focus default is TRUE */
    fprintf(stderr, "FAILED: line <%d> take_focus %d\n", __LINE__, value);
    result++;
  }

  geda_menu_shell_set_take_focus (menu_shell, FALSE);

  value = geda_menu_shell_get_take_focus (menu_shell);
  if (value) {
    fprintf(stderr, "FAILED: line <%d> get/set take_focus %s\n", __LINE__, TWIDGET);
    result++;
  }

  geda_menu_shell_set_take_focus (menu_shell, TRUE);

  value = geda_menu_shell_get_take_focus (menu_shell);
  if (!value) {
    fprintf(stderr, "FAILED: line <%d> set/get take_focus %s\n", __LINE__, TWIDGET);
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
