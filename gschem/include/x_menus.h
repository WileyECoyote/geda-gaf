/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_menus.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */
/*!
 * \file x_menus.h
 *
 * \brief header for the Menu module
 */

#ifndef __GSCHEM_X_MENU_H__
#define __GSCHEM_X_MENU_H__

 /* Menu Paths */
#define OPT_STDBAR_MENU_PATH   "_View/_Toolbars/_Standard"
#define OPT_SELBAR_MENU_PATH   "_View/_Toolbars/Se_lect"
#define OPT_PageBAR_MENU_PATH  "_View/_Toolbars/_Page"
#define OPT_ADDBAR_MENU_PATH   "_View/_Toolbars/_Add"
#define OPT_ZOOMBAR_MENU_PATH  "_View/_Toolbars/_Zoom"
#define OPT_EDITBAR_MENU_PATH  "_View/_Toolbars/_Edit"
#define OPT_ATTRBAR_MENU_PATH  "_View/_Toolbars/A_ttribute"
#define OPT_GRIDBAR_MENU_PATH  "_View/_Toolbars/_Grid Snap"

#define OPT_BAR_ICON_MENU_PATH "_View/_Toolbars/_Icons"
#define OPT_BAR_TEXT_MENU_PATH "_View/_Toolbars/_Text"
#define OPT_BAR_VERT_MENU_PATH "_View/_Toolbars/Both _Vertical"
#define OPT_BAR_HOZI_MENU_PATH "_View/_Toolbars/Both _Horizontal"

#define OPT_ICON_MENU_PATH     "_View/_Menu/_Icons"
#define OPT_TIPS_MENU_PATH     "_View/_Menu/_ToolTips"
#define OPT_POPCONS_MENU_PATH  "_View/_Menu/_Context Icons"
#define OPT_POPTIPS_MENU_PATH  "_View/_Menu/Context Tip_s"

#define MAX_RECENT_FILES 10 /* Make this a variable like normal programs */
#define RECENT_FILES_STORE "gschem-recent-files"
#define SENSITIVITY_ERROR_LIMIT 5

typedef enum {
  pop_edit_select,
  pop_add_net,
  pop_add_attribute,
  pop_add_component,
  pop_add_bus,
  pop_add_text,
  pop_zoom_in,
  pop_zoom_out,
  pop_zoom_box,
  pop_zoom_extents,
  pop_zoom_to_mag,
  pop_zoom_to_select,
  pop_edit_objects,
  pop_edit_component,
  pop_edit_pintype,
  pop_edit_delete,
  pop_edit_copy,
  pop_edit_mcopy,
  pop_edit_move,
  pop_edit_mirror,
  pop_edit_rotate,
  pop_edit_extend,
  pop_down_schemat,
  pop_down_symbol,
  pop_hierarchy_up,
  pop_cb_cut,
  pop_cb_copy,
  pop_cb_paste
} pop_MenuItem;

/* Order of items is not important */
typedef enum { RESET_TOGGLERS=-1, SNAP_TOGGLE, RUBBER_TOGGLE, MAGNETIC_TOGGLE,
               DRAG_CAN_MOVE, OUTLINE_TOGGLE, AUTO_PAN_TOGGLE
} MenuToggleItem;

typedef struct st_menu_data             MenuData;
typedef struct st_recent_file_menu_data RecentMenuData;
typedef struct st_toggle_menu_data      ToggleMenuData;
typedef struct st_menu_radio_data       RadioMenuData;
typedef struct st_popup_menu_entry      PopupEntry;

struct st_menu_data {
  char       *buffer_menu_name;   /* Pointer name of menu above Cut/Copy/paste buffers */
  GtkWidget  *menu_bar;           /* The Main menu widget, unique to the Window, aka ui_index */
  GtkWidget  *popup_menu;         /* The "on-canvas" Popup menu widget */
  GSList     *menu_items;         /* Single Linked list of all non-toggle menu items */
  GSList     *popup_items;        /* Single Linked list of all non-toggle popup-menu items */
  GSList     *menu_togglers;      /* Single Linked list of all togglable Main menu items */
  GHashTable *popup_hash;         /* String table of Popup menu item names, used for sensitivity */
};

struct st_recent_file_menu_data {
  GschemToplevel *w_current;
  char *filename;
};

struct st_toggle_menu_data {
  GschemToplevel *w_current;   /* We can do this because the menu is unique to the window */
  GtkWidget      *action;      /* Action associated with this toggle menu item */
  int   toggle_id;             /* Index in st_menu_data.menu_togglers list */
  char *toggle_name;           /* Menu string DOES NOT APPEAR TO BE SET */
  char *menu_item_name;        /* String for the label in the menu */
  char *menu_path;             /* String path used to locate widget programmatically setting */
  unsigned long handler;       /* Callback signal handler ID, used to suspend emissions */
};

struct st_menu_radio_data {
  GschemToplevel   *w_current;
  GtkCheckMenuItem *widget;
  unsigned long     handler;
};

struct st_popup_menu_entry {
  const char const *name;
  void             *func;
  int               action_id;
  bool              use_stock;
  const char const *icon;
  const char const *tip;
};
#endif /* __GSCHEM_X_MENU_H__ */
