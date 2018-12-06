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
 * Date: June, 20, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include "../include/geda_gtk_compat.h"

#include "../include/geda_label.h"
#include "../include/geda_menu.h"
#include "../include/geda_menu_bar.h"
#include "../include/geda_menu_item.h"
#include "../include/geda_menu_shell.h"

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_menu_bar.c"

#define TWIDGET "GedaMenuBar"

/*! \file test_menu_bar.c
 *  \brief Tests for geda_menu_bar.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_menu_bar_new();

  if (!GEDA_IS_MENU_BAR(widget)) {
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

static GtkWindow *main_window(GtkWidget *menubar)
{
  GtkWidget *vbox;
  GtkWidget *window;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_show (vbox);

  gtk_box_pack_start (GTK_BOX (vbox), menubar, FALSE, TRUE, 0);

  gtk_widget_show (menubar);
  gtk_widget_show (window);

  return GTK_WINDOW(window);
}

int
check_accessors ()
{
  int result = 0;

  GtkWidget *widget = geda_menu_bar_new();

  if (!GEDA_IS_MENU_BAR(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

  /* -------------------- pack_direction -------------------- */

    PackDirection pack_dir;

    GedaMenuBar *menubar = GEDA_MENU_BAR(widget);

    pack_dir = geda_menu_bar_get_pack_direction (menubar);

    if (pack_dir != PACK_DIRECTION_LTR) { /* Default value */
      fprintf(stderr, "FAILED: %s default pack_direction <%d>\n", TWIDGET, pack_dir);
      result++;
    }

    geda_menu_bar_set_pack_direction (menubar, PACK_DIRECTION_RTL);

    pack_dir = geda_menu_bar_get_pack_direction (menubar);

    if (pack_dir != PACK_DIRECTION_RTL) { /* Default value */
      fprintf(stderr, "FAILED: %s pack direction <%d>\n", TWIDGET, pack_dir);
      result++;
    }

    pack_dir = geda_menu_bar_get_child_pack_direction (menubar);

    if (pack_dir != PACK_DIRECTION_LTR) { /* Default value */
      fprintf(stderr, "FAILED: %s default child pack_direction <%d>\n", TWIDGET, pack_dir);
      result++;
    }

    geda_menu_bar_set_child_pack_direction (menubar, PACK_DIRECTION_RTL);

    pack_dir = geda_menu_bar_get_child_pack_direction (menubar);

    if (pack_dir != PACK_DIRECTION_RTL) { /* Default value */
      fprintf(stderr, "FAILED: %s child pack direction <%d>\n", TWIDGET, pack_dir);
      result++;
    }

    /* -------------------- viewable_menu_bars -------------------- */

    GtkWindow *window;
    GList     *bars;

    window = main_window(widget);
    bars = geda_menu_bar_get_viewable_menu_bars (window);

    if (!bars) {
      fprintf(stderr, "FAILED: %s viewable_menu_bars NULL\n", TWIDGET);
      result++;
    }
    else {

      int count = g_list_length(bars);

      if (g_list_length(bars) != 1) {
        fprintf(stderr, "FAILED: %s viewable_menu_bars count <%d>\n", TWIDGET, count);
        result++;
      }

      if (bars->data != menubar) {
        fprintf(stderr, "FAILED: %s viewable_menu_bars <%p>\n", TWIDGET, bars->data);
        result++;
      }
    }

    g_object_ref_sink(widget); /* Sink reference to the widget */
    g_object_unref(widget);    /* Destroy the widget */
  }

  return result;
}

int
check_methods ()
{
  const char *func;

  int result = 0;

  GtkWidget    *widget00;
  GtkWidget    *widget01;
  GtkWidget    *widget02;
  GtkWidget    *widget03;
  GtkWidget    *menu1;
  GtkWidget    *menu_bar1;
  GtkWindow    *window;
  GedaMenuItem *menu_item;

  menu_bar1 = geda_menu_bar_new ();
  window    = main_window(menu_bar1);

  widget00  = geda_menu_item_new_with_mnemonic("_Clemens");
  menu_item = GEDA_MENU_ITEM(widget00);
  menu1     = geda_menu_new ();

  geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menu_item), menu1);
  geda_menu_shell_append (GEDA_MENU_SHELL (menu_bar1), widget00);
  gtk_widget_set_can_focus (widget00, TRUE);
  gtk_widget_show (widget00);

  widget01   = geda_menu_item_new_with_mnemonic("_Pamela");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu1), widget01);
  gtk_widget_show (widget01);

  widget02   = geda_menu_item_new_with_mnemonic("_Benjamin");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu1), widget02);
  gtk_widget_show (widget02);

  widget03   = geda_menu_item_new_with_mnemonic("_Margaret");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu1), widget03);
  gtk_widget_show (widget03);

  gtk_widget_show (menu1);

  GtkWidget    *widget10;
  GtkWidget    *widget11;
  GtkWidget    *widget12;
  GtkWidget    *widget13;
  GtkWidget    *menu2;
  GtkWidget    *menu_bar2;
  GtkWidget    *vbox;

  menu_bar2 = geda_menu_bar_new ();
  vbox      = geda_get_child_widget(window);

  gtk_box_pack_start (GTK_BOX (vbox), menu_bar2, FALSE, TRUE, 0);
  gtk_widget_show (menu_bar2);

  widget10  = geda_menu_item_new_with_mnemonic("_Hemingway");
  menu_item = GEDA_MENU_ITEM(widget10);
  menu2     = geda_menu_new ();

  geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menu_item), menu2);
  geda_menu_shell_append (GEDA_MENU_SHELL (menu_bar2), widget10);
  gtk_widget_set_can_focus (widget10, TRUE);
  gtk_widget_show (widget10);

  widget11   = geda_menu_item_new_with_mnemonic("_Clarence");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu2), widget11);
  gtk_widget_show (widget11);

  widget12   = geda_menu_item_new_with_mnemonic("_Marcelline");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu2), widget12);
  gtk_widget_show (widget12);

  widget13   = geda_menu_item_new_with_mnemonic("_Sunny");
  geda_menu_shell_append (GEDA_MENU_SHELL (menu2), widget13);
  gtk_widget_show (widget13);

  gtk_widget_show (menu2);

  /* -------------------- cycle_focus ------------------ */

  func = "geda_menu_bar_cycle_focus";

  gtk_widget_grab_focus(widget00);

  /* Cycle focus to the other menu bar (menu_bar2) */
  geda_menu_bar_cycle_focus((GedaMenuBar*)menu_bar1, GTK_DIR_TAB_FORWARD);

  if (gtk_widget_has_focus(widget00)) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }
  else if (geda_menu_widget_get_active(menu2) != widget11) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  /* Cycle focus back the first menu bar (menu_bar1) */
  geda_menu_bar_cycle_focus((GedaMenuBar*)menu_bar1, GTK_DIR_TAB_BACKWARD);

  if (geda_menu_widget_get_active(menu1) != widget01) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  geda_menu_bar_cycle_focus((GedaMenuBar*)menu_bar1, GTK_DIR_TAB_FORWARD);

  geda_menu_bar_cycle_focus((GedaMenuBar*)menu_bar2, GTK_DIR_TAB_FORWARD);

  if (geda_menu_widget_get_active(menu1) != widget01) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  /* ---------------- show/hide mnemonics -------------- */

  GtkWidget *label;

  func = "geda_menu_bar_hide_mnemonics";

  geda_menu_bar_hide_mnemonics ((GedaMenuBar*)menu_bar1);
  geda_menu_bar_show_mnemonics ((GedaMenuBar*)menu_bar2);

  label = geda_menu_item_get_label_widget ((GedaMenuItem*)widget00);

  if (geda_label_get_mnemonic_visible(GEDA_LABEL(label))) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  label = geda_menu_item_get_label_widget ((GedaMenuItem*)widget10);

  if (!geda_label_get_mnemonic_visible(GEDA_LABEL(label))) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  /* Reverse the visibility of mnemonics */
  geda_menu_bar_show_mnemonics ((GedaMenuBar*)menu_bar1);
  geda_menu_bar_hide_mnemonics ((GedaMenuBar*)menu_bar2);

  label = geda_menu_item_get_label_widget ((GedaMenuItem*)widget00);

  if (!geda_label_get_mnemonic_visible(GEDA_LABEL(label))) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  label = geda_menu_item_get_label_widget ((GedaMenuItem*)widget10);

  if (geda_label_get_mnemonic_visible(GEDA_LABEL(label))) {
    fprintf(stderr, "FAILED: %s line <%d> %s\n", TWIDGET, __LINE__, func);
    result++;
  }

  gtk_widget_destroy(GTK_WIDGET(window));

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
