/* testmenus.c -- dynamically add and remove items to a menu
 * Copyright (C) 2002  Red Hat, Inc.
 * Author: Owen Taylor
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "../../config.h"

#include <gtk/gtk.h>
#include <geda/geda.h>
#include <geda_menu.h>
#include <geda_menu_bar.h>
#include <geda_menu_item.h>
#include <geda_menu_separator.h>
#include <geda_menu_shell.h>
#include <geda_image_menu_item.h>
#include <geda_option_menu.h>
#include <geda_radio_menu_item.h>
#include <geda_tearoff_menu_item.h>
#include <geda_accel_label.h>

#include <geda_separator.h>
#include <geda_keysyms.h>

#include <stdio.h>

static GtkWidget *create_menu (int depth, bool tearoff)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList    *group;
  char buf[32];
  int i, j;

  if (depth < 1)
    return NULL;

  menu  = geda_menu_new ();
  group = NULL;

  if (tearoff) {
    menuitem = geda_tearoff_menu_item_new ();
    geda_menu_append (menu, menuitem);
    gtk_widget_show (menuitem);
  }

  for (i = 0, j = 1; i < 5; i++, j++) {

    GtkWidget *sub_menu;

    sprintf (buf, "item %2d - %d", depth, j);
    menuitem = geda_radio_menu_item_new_with_label (group, buf);
    group    = geda_radio_menu_item_get_group (GEDA_RADIO_MENU_ITEM (menuitem));

    geda_menu_append (menu, menuitem);

    gtk_widget_show (menuitem);

    if (i == 3) {
      gtk_widget_set_sensitive (menuitem, FALSE);
    }

    sub_menu = create_menu (depth - 1, TRUE);

    if (sub_menu) {
      geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menuitem), sub_menu);
    }
  }

  return menu;
}

static bool change_item (void *user_data)
{
  GtkWidget     *widget;
  GedaMenuShell *shell = GEDA_MENU_SHELL (user_data);
  static int step = 0;

  if (((step++ / 40) % 2) == 0) {

      //g_message ("Idle add");

      widget = geda_menu_item_new_with_label ("Foo");
      gtk_widget_show (widget);

      geda_menu_shell_append (shell, widget);
  }
  else {

      GList *children = gtk_container_get_children (GTK_CONTAINER (shell));

      //g_message ("Idle remove");

      gtk_widget_destroy (g_list_last (children)->data);

      g_list_free (children);
    }

  return TRUE;
}

int main (int argc, char **argv)
{
  static GtkWidget *window = NULL;
         GtkWidget *box1;
         GtkWidget *box2;
         GtkWidget *button;
         GtkWidget *optionmenu;
         GtkWidget *separator;

  gtk_init (&argc, &argv);

  if (!window) {

    GtkWidget     *menubar;
    GtkWidget     *menu;
    GtkWidget     *submenu;
    GtkWidget     *menuitem;
    GtkAccelGroup *accel_group;

    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

    g_signal_connect (window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

    g_signal_connect (window, "delete-event", G_CALLBACK (gtk_true), NULL);

    accel_group = gtk_accel_group_new ();
    gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

    gtk_window_set_title (GTK_WINDOW (window), "menus");
    gtk_container_set_border_width (GTK_CONTAINER (window), 0);

    box1 = gtk_vbox_new (FALSE, 0);
    gtk_container_add (GTK_CONTAINER (window), box1);
    gtk_widget_show (box1);

    menubar = geda_menu_bar_new ();
    gtk_box_pack_start (GTK_BOX (box1), menubar, FALSE, TRUE, 0);
    gtk_widget_show (menubar);

    menu = create_menu (2, TRUE);

    menuitem = geda_menu_item_new_with_label ("test\nline2");
    geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menuitem), menu);
    geda_menu_shell_append (GEDA_MENU_SHELL (menubar), menuitem);
    gtk_widget_show (menuitem);

    menuitem = geda_menu_item_new_with_label ("dynamic");

    submenu = create_menu (3, TRUE);
    geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menuitem), submenu);
    geda_menu_shell_append (GEDA_MENU_SHELL (menubar), menuitem);
    gtk_widget_show (menuitem);

    gdk_threads_add_timeout (250, change_item, submenu);

    menuitem = geda_menu_item_new_with_label ("bar");
    geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menuitem), create_menu (4, TRUE));
    geda_menu_item_set_right_justified (GEDA_MENU_ITEM (menuitem), TRUE);
    geda_menu_shell_append (GEDA_MENU_SHELL (menubar), menuitem);
    gtk_widget_show (menuitem);

    box2 = gtk_vbox_new (FALSE, 10);
    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
    gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
    gtk_widget_show (box2);

    menu = create_menu (1, FALSE);
    geda_menu_set_accel_group (GEDA_MENU (menu), accel_group);

    menuitem = geda_menu_separator_new ();
    geda_menu_append (menu, menuitem);
    gtk_widget_show (menuitem);

    menuitem = geda_check_menu_item_new_with_label ("Accelerate Me");
    geda_menu_append (menu, menuitem);
    gtk_widget_show (menuitem);
    gtk_widget_add_accelerator (menuitem,
                                "activate",
                                accel_group,
                                GDK_F1,
                                0,
                                GTK_ACCEL_VISIBLE);
    menuitem = geda_check_menu_item_new_with_label ("Accelerator Locked");
    geda_menu_append (menu, menuitem);
    gtk_widget_show (menuitem);
    gtk_widget_add_accelerator (menuitem,
                                "activate",
                                accel_group,
                                GDK_F2,
                                0,
                                GTK_ACCEL_VISIBLE | GTK_ACCEL_LOCKED);
    menuitem = geda_check_menu_item_new_with_label ("Accelerators Frozen");
    geda_menu_append (menu, menuitem);
    gtk_widget_show (menuitem);
    gtk_widget_add_accelerator (menuitem,
                                "activate",
                                accel_group,
                                GDK_F2,
                                0,
                                GTK_ACCEL_VISIBLE);
    gtk_widget_add_accelerator (menuitem,
                                "activate",
                                accel_group,
                                GDK_F3,
                                0,
                                GTK_ACCEL_VISIBLE);

    optionmenu = geda_option_menu_new ();
    geda_option_menu_set_menu (GEDA_OPTION_MENU (optionmenu), menu);
    geda_option_menu_set_history (GEDA_OPTION_MENU (optionmenu), 3);
    gtk_box_pack_start (GTK_BOX (box2), optionmenu, TRUE, TRUE, 0);
    gtk_widget_show (optionmenu);

    separator = geda_hseparator_new ();
    gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 0);
    gtk_widget_show (separator);

    box2 = gtk_vbox_new (FALSE, 10);
    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
    gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, TRUE, 0);
    gtk_widget_show (box2);

    button = gtk_button_new_with_label ("close");
    g_signal_connect_swapped (button, "clicked",
                              G_CALLBACK(gtk_widget_destroy), window);
    gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
    gtk_widget_set_can_default (button, TRUE);
    gtk_widget_grab_default (button);
    gtk_widget_show (button);
  }

  if (!gtk_widget_get_visible (window)) {
      gtk_widget_show (window);
  }
  else {
      gtk_widget_destroy (window);
      window = NULL;
  }

  gtk_main ();

  return 0;
}
