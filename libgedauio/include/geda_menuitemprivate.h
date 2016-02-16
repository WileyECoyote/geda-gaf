/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menuitemprivate.h
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
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
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/.
 */

#ifndef __GEDA_MENU_ITEM_PRIVATE_H__
#define __GEDA_MENU_ITEM_PRIVATE_H__

#include <gtk/gtkmenuitem.h>
#include <gtk/gtkaction.h>

#include <gtk/gtk.h>

struct _GtkMenuItemPrivate
{
  GtkWidget *submenu;
  GdkWindow *event_window;

  unsigned short toggle_size;
  unsigned short accelerator_width;

  unsigned int timer;

  char        *accel_path;

  GtkAction   *action;
  //GtkActionHelper *action_helper;
  void        *action_helper;

  unsigned int show_submenu_indicator : 1;
  unsigned int submenu_placement      : 1;
  unsigned int submenu_direction      : 1;
  unsigned int right_justify          : 1;
  unsigned int timer_from_keypress    : 1;
  unsigned int from_menubar           : 1;
  unsigned int use_action_appearance  : 1;
  unsigned int reserve_indicator      : 1;
};

#ifdef __cplusplus
extern "C" {
#endif

void     _gtk_menu_item_refresh_accel_path   (GtkMenuItem   *menu_item,
                                              const char    *prefix,
                                              GtkAccelGroup *accel_group,
                                              int            group_changed);
int      _gtk_menu_item_is_selectable        (GtkWidget     *menu_item);
void     _gtk_menu_item_popup_submenu        (GtkWidget     *menu_item,
                                              int            with_delay);
void     _gtk_menu_item_popdown_submenu      (GtkWidget     *menu_item);
void	     _gtk_menu_item_refresh_accel_path   (GtkMenuItem   *menu_item,
                                              const char    *prefix,
                                              GtkAccelGroup *accel_group,
                                              int      	   group_changed);
int       _gtk_menu_item_is_selectable       (GtkWidget     *menu_item);
void      _gtk_menu_item_popup_submenu       (GtkWidget     *menu_item,
                                              int            with_delay);
void      _gtk_menu_item_popdown_submenu     (GtkWidget     *menu_item);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MENU_ITEM_PRIVATE_H__ */
