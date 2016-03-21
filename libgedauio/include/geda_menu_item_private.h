/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menu_item_private.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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

#include "geda_menu_item.h"
#include "geda_action.h"

struct _GedaMenuItemPrivate
{
  GtkWidget *submenu;
  GdkWindow *event_window;

  unsigned short toggle_size;
  unsigned short accelerator_width;

  unsigned int timer;

  char        *accel_path;

  GedaAction  *action;
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

#endif /* __GEDA_MENU_ITEM_PRIVATE_H__ */
