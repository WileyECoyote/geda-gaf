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

#define IDS_MENU_VIEW_GRID     "_View/_Grid"
#define IDS_MENU_GRID_DOTS     "_Dots"
#define IDS_MENU_GRID_MESH     "_Mesh"
#define IDS_MENU_GRID_NONE     "_None"

#define IDS_MENU_VIEW_TOOLBARS "_View/_Toolbars"

#define IDS_MENU_TB_ADD        "_Add"
#define IDS_MENU_TB_ATTRIB     "Attrib_ute"
#define IDS_MENU_TB_EDIT       "_Edit"
#define IDS_MENU_TB_GRID_SNAP  "_Grid Snap"
#define IDS_MENU_TB_MODIFY     "_Modify"
#define IDS_MENU_TB_PAGE       "_Page"
#define IDS_MENU_TB_SELECT     "Se_lect"
#define IDS_MENU_TB_STANDARD   "_Standard"
#define IDS_MENU_TB_SYMBOL     "Sym_bol"
#define IDS_MENU_TB_ZOOM       "_Zoom"

#define IDS_MENU_TB_TIPS       "_Display tips"

 /* Menu Paths */
#define OPT_STDBAR_MENU_PATH   IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_STANDARD
#define OPT_SELBAR_MENU_PATH   IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_SELECT
#define OPT_PAGEBAR_MENU_PATH  IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_PAGE
#define OPT_ADDBAR_MENU_PATH   IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_ADD
#define OPT_ZOOMBAR_MENU_PATH  IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_ZOOM
#define OPT_SYMBAR_MENU_PATH   IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_SYMBOL
#define OPT_EDITBAR_MENU_PATH  IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_EDIT
#define OPT_ATTRBAR_MENU_PATH  IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_ATTRIB
#define OPT_GRIDBAR_MENU_PATH  IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_GRID_SNAP

#define OPT_BAR_TIPS_MENU_PATH IDS_MENU_VIEW_TOOLBARS "/" IDS_MENU_TB_TIPS

#define OPT_BAR_ICON_MENU_PATH "_View/_Toolbars/_Icons"
#define OPT_BAR_TEXT_MENU_PATH "_View/_Toolbars/_Text"
#define OPT_BAR_VERT_MENU_PATH "_View/_Toolbars/Both _Vertical"
#define OPT_BAR_HOZI_MENU_PATH "_View/_Toolbars/Both _Horizontal"

#define IDS_MENU_VIEW_MENU     "_View/_Menu"

#define OPT_ICON_MENU_PATH     "_View/_Menu/_Icons"
#define OPT_TIPS_MENU_PATH     "_View/_Menu/_ToolTips"
#define OPT_POPCONS_MENU_PATH  "_View/_Menu/_Context Icons"
#define OPT_POPTIPS_MENU_PATH  "_View/_Menu/Context Tip_s"

#define MAX_RECENT_FILES 10 /* Make this a variable like normal programs */
#define RECENT_FILES_STORE "gschem-recent-files"
#define SENSITIVITY_ERROR_LIMIT 5

#define OPT_GRID_DOTS_MENU_PATH IDS_MENU_VIEW_GRID "/" IDS_MENU_GRID_DOTS
#define OPT_GRID_MESH_MENU_PATH IDS_MENU_VIEW_GRID "/" IDS_MENU_GRID_MESH
#define OPT_GRID_NONE_MENU_PATH IDS_MENU_VIEW_GRID "/" IDS_MENU_GRID_NONE

/* Short-cuts to reference records in st_menu_data */
#define MENU_BAR         menu_data->menu_bar
#define MENU_ITEMS_LIST  menu_data->menu_items
#define POPUP_ITEMS_LIST menu_data->popup_items
#define POPUP_MAIN_HASH  menu_data->popup_hash
#define POPUP_MAIN       menu_data->popup_main
#define POPUP_PATH       menu_data->popup_path
#define TOGGLERS_LIST    menu_data->menu_togglers
#define GRID_RADIO_LIST  menu_data->grid_mode_grp

/* Used to reference IDS_Popup_Actions in x_menus.c */
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
  pop_edit_color,
  pop_edit_component,
  pop_edit_pintype,
  pop_edit_array,
  pop_edit_break,
  pop_edit_extend,
  pop_edit_delete,
  pop_edit_copy,
  pop_edit_mcopy,
  pop_edit_move,
  pop_edit_mirror,
  pop_edit_rotate,
  pop_edit_snap,
  pop_down_schemat,
  pop_down_symbol,
  pop_hierarchy_up,
  pop_cb_cut,
  pop_cb_copy,
  pop_cb_paste
} pop_MenuItem;

typedef enum {
  pop_path_close,
  pop_path_continue,
  pop_path_done,
  pop_path_end,
  pop_path_undo,
  pop_path_cancel
} pop_PathItem;

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
  GSList     *menu_items;         /* Single Linked list of all non-toggle menu items */
  GSList     *menu_togglers;      /* Single Linked list of all togglable Main menu items */
  GSList     *grid_mode_grp;      /* Single Linked list of grid mode Main menu radio items */
  GtkWidget  *popup_main;         /* The main "on-canvas" popup context menu widget */
  GSList     *popup_items;        /* Single Linked list of all non-toggle popup-menu items */
  GHashTable *popup_hash;         /* String table of Popup menu item names, used for sensitivity */
  GtkWidget  *popup_path;
};

struct st_recent_file_menu_data {
  GschemToplevel *w_current;
  char           *filename;
};

struct st_toggle_menu_data {
  GschemToplevel *w_current;      /* We can do this because the menu is unique to the window */
  GtkWidget      *action;         /* Action associated with this toggle menu item */
  unsigned long   handler;        /* Callback signal handler ID, used to suspend emissions */
  char           *menu_item_name; /* String for the label in the menu */
  char           *menu_path;      /* String path used to locate widget programmatically setting */
  int             toggle_id;      /* Index in st_menu_data.menu_togglers list */
  char           *toggle_name;    /* Menu string DOES NOT APPEAR TO BE SET */
};

struct st_menu_radio_data {
  GschemToplevel    *w_current;
  GedaCheckMenuItem *widget;
  unsigned long      handler;
};

struct st_popup_menu_entry {
  const char *const name;
  void             *func;
  int               action_id;
  bool              use_stock;
  const char *const icon;
  const char *const tip;
};

#endif /* __GSCHEM_X_MENU_H__ */
