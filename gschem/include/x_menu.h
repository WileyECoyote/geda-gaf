/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture

 * Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

#ifndef __GSCHEM_X_MENU_H__
#define __GSCHEM_X_MENU_H__

#define RECENT_FILES_STORE "gschem-recent-files"
#define MAX_RECENT_FILES 10 /* Make this a variable like normal programs */

typedef enum {
  pop_add_net,
  pop_add_attribute,
  pop_add_component,
  pop_add_bus,
  pop_add_text,
  pop_zoom_in,
  pop_zoom_out,
  pop_zoom_box,
  pop_zoom_extents,
  pop_edit_select,
  pop_edit_butes,
  pop_edit_pintype,
  pop_edit_copy,
  pop_edit_move,
  pop_edit_delete,
  pop_down_schemat,
  pop_down_symbol,
  pop_hierarchy_up,
} pop_MenuItem;

typedef enum { RESET_TOGGLERS=-1, SNAP_TOGGLE, OUTLINE_TOGGLE, RUBBER_TOGGLE,
               MAGNETIC_TOGGLE
} MenuToggleItem;

typedef struct st_menu_data MenuData;
typedef struct st_recent_file_menu_data RecentMenuData;
typedef struct st_toggle_menu_data      ToggleMenuData;
typedef struct st_menu_radio_data       RadioMenuData;

struct st_menu_data {
  GtkWidget *menu_bar;
  GtkWidget *popup_menu;
  GSList    *menu_items;    /* Single Linked list of all non-toggle menu items */
  GSList    *menu_togglers;
};

struct st_recent_file_menu_data {
  GSCHEM_TOPLEVEL *w_current;
  char *filename;
};

struct st_toggle_menu_data {
  GSCHEM_TOPLEVEL *w_current;
  int   toggle_id;
  char *toggle_name;
  char *menu_item_name;
  char *menu_path;
  unsigned long handler;
};

struct st_menu_radio_data {
  GSCHEM_TOPLEVEL  *w_current;
  GtkCheckMenuItem *widget;
  unsigned long handler;
};

#endif /* __GSCHEM_X_MENU_H__ */
