/* -*- C x_toolbars.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_toolbars.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file x_toolbars.c
 * \brief Main Window Auxiliary Module for Toolbars
 */

#include <gtk/gtk.h>

#include "../../include/gschem.h"
#include <geda/geda_stat.h>
#include <geda_handlebox.h>
#include <geda/geda_gui_funcs.h>

#define TOOLBAR_STYLE w_current->toolbars_mode           /* per window style variable  */
#define TOOLBAR_TIPS  w_current->show_toolbar_tips       /* per window toolbar tips */
#define DEFAULT_TOOLBAR_STYLE TOOLBAR_SHOW_ICONS         /* default style */
#define DEFAULT_TOOLBAR_TIPS  TRUE                       /* default style */
#define TheToolBars bar_widgets->toolbar_slist           /* convenience macro */

#define TOOLBAR_RADIO_VARIABLE(symbol) bar_widgets->toolbar_##symbol
#define BarRadio(...) TOOLBAR_RADIO_VARIABLE(__VA_ARGS__)

#define ToolBar_Radio_Responder x_toolbars_execute_radio

#include <geda/geda_toolbars.h>

#include "../../include/x_menus.h"
#include "../../include/x_toolbars.h"
#include "../../include/x_window.h"
#include "../../include/i_actions.h"

#include "../../../include/geda_debug.h"

/** toolbars-button-lists GLists of Toolbar Buttons
 *  \brief Collection GList of Toolbar Widgets
 *  \par
 *  These are convenience macros to reference GList in the bar_widgets
 *  structure, various buttons on toolbars are categorically added to
 *  the lists, which are used when setting sensitivities states
 */

#define ANY_OBJECT_LIST    bar_widgets->any_object       /*!< List of widgets on toolbars to set if some object is selected */
#define CAN_PASTE_LIST     bar_widgets->can_paste        /*!< List of widgets set active if something on clibboard */
#define CAN_UNDO_LIST      bar_widgets->can_undo
#define CAN_REDO_LIST      bar_widgets->can_redo

#define CAN_HATCH_LIST     bar_widgets->can_hatch
#define CAN_ELINE_LIST     bar_widgets->can_edit_line
#define HAVE_COMPLEX_LIST  bar_widgets->complex_selected
#define HAVE_PAGES_LIST    bar_widgets->multi_pages
#define HAVE_PIN_LIST      bar_widgets->pin_selected
#define TEXT_OBJECT_LIST   bar_widgets->text_selected

#define TOOLBAR_RADIOS bar_widgets->toolbar_radio_list    /*!< Single list of toolbar radios */

/** \defgroup toolbars-module Toolbars Module
 *  @{\brief This group contains functions to the toolbars
 *    \ingroup main-window
 */

static GSList    *ui_list = NULL;

static GtkWidget *popup_menu;

static ToolBarInfo ActiveToolBar;

/*!
 * \note #1: These numerators are used to access the structure of strings
 *           ToolbarStrings, i.e. if an item is added to one then an item
 *           must be added to the corresponding position in the other.
 *
 * \note #2: etb_none is a dummy member used in the add mode radio group.
 *           The group is a collection of radio button and when we don't
 *           want any of them selected then we set etb_none to be the
 *           active radio button.
 */
typedef enum  { etb_new, etb_open, etb_save, etb_save_as, etb_close,
                etb_print, etb_write_pdf,
                etb_cut, etb_copy, etb_paste,
                etb_undo, etb_redo,
                etb_selector, etb_deselector, etb_unselect_all,
                etb_none, etb_select_all, etb_select_invert, etb_add_component,
                etb_add_net, etb_add_bus, etb_add_attribute, etb_add_text,
                etb_add_line, etb_add_box, etb_add_circle, etb_add_arc,
                etb_add_path, etb_add_pin, etb_add_pic,
                etb_first_page, etb_prev_page, etb_next_page, etb_up_page,
                etb_down_page, etb_last_page, etb_new_page, etb_page_manager,
                etb_down_schematic, etb_down_symbol, etb_hierarchy_up,
                etb_view_document, etb_view_nets,
                etb_view_redraw, etb_zoom_pan, etb_zoom_box, etb_zoom_select,
                etb_zoom_extents, etb_zoom_in, etb_zoom_out, etb_zoom_all,
                etb_edit_prop, etb_translate, etb_lock, etb_unlock, etb_update,
                etb_edit_copy, etb_multi_copy, etb_move, etb_mirror, etb_rotate,
                etb_edit_butes, etb_edit_color, etb_edit_text, etb_edit_slot,
                etb_edit_pin, etb_edit_line, etb_edit_fill, etb_edit_arc,
                etb_edit_array,
                etb_attach, etb_detach, etb_show_value, etb_show_name,
                etb_show_both, etb_visibilty, etb_show_hidden, etb_show_inherited,
                etb_find_text, etb_hide_text, etb_show_specific, etb_auto_number,
                etb_grid_dot, etb_grid_mesh, etb_grid_off,
                etb_snap_up, etb_snap_down, etb_snap_set, etb_snap_off,
                etb_snap_on, etb_configure, etb_last
} IDE_GSCHEM_Toolbar;

/* Important: See IDS_Menu_Toolbar_Toggles in x_menu.c */
const char* IDS_Toolbar_Names[] = {  /* ToolBar Name Strings */
  "add-bar", "Attribute", "Edit", "GridSnap", "Modify", "Page", "Select",
  "Standard", "Symbol", "Zoom",
  NULL
};

typedef enum
{
  TB_ICON_STOCK         = 0,
  TB_ICON_BITMAP        = 1 << 1,
  TB_ICON_SOURCE2       = 1 << 2,
  TB_ICON_SOURCE3       = 1 << 3,
  TB_ICON_SOURCE4       = 1 << 4,
  TB_ICON_SOURCE5       = 1 << 5,
} IDE_TB_ICON_SOURCE;

/*  action,                     Label,        Tip,                     icon_id, iflag icon */;

static ToolbarStringData ToolbarStrings[] = {
   /* Standard Toolbar*/
  { ACTION(FILE_NEW),           "New",       TBTS_FILE_NEW,            GSCHEM_MAP(NEW),           TB_ICON_BITMAP, NULL},
  { ACTION(FILE_OPEN),          "Open",      TBTS_FILE_OPEN,           GSCHEM_MAP(OPEN),          TB_ICON_BITMAP, NULL},
  { ACTION(FILE_SAVE),          "Save",      TBTS_FILE_SAVE,           GSCHEM_MAP(SAVE),          TB_ICON_BITMAP, NULL},
  { ACTION(FILE_SAVE_AS),       "Save As",   TBTS_FILE_SAVE_AS,       "gschem-save-as",           TB_ICON_STOCK, NULL},
  { ACTION(FILE_CLOSE),         "Close",     TBTS_FILE_CLOSE,          GSCHEM_MAP(PROJECT_CLOSE), TB_ICON_BITMAP, NULL},

  { ACTION(FILE_PRINT),         "Print",     TBTS_FILE_PRINT,         "gschem-print-document",    TB_ICON_STOCK,  NULL},
  { ACTION(FILE_WRITE_PDF),     "PDF",       TBTS_FILE_WRITE_PDF,      GAF_PDF_BITMAP,            TB_ICON_BITMAP, NULL},

  { ACTION(EDIT_CB_CUT),        "Cut",       TBTS_EDIT_CB_CUT,        "gtk-cut",           TB_ICON_STOCK,  NULL},
  { ACTION(EDIT_CB_COPY),       "Copy",      TBTS_EDIT_CB_COPY,       "gtk-copy",          TB_ICON_STOCK,  NULL},
  { ACTION(EDIT_CB_PASTE),      "Paste",     TBTS_EDIT_CB_PASTE,      "gtk-paste",         TB_ICON_STOCK,  NULL},

  { ACTION(EDIT_UNDO),          "Undo",      TBTS_EDIT_UNDO,           GSCHEM_MAP(UNDO),   TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_REDO),          "Redo",      TBTS_EDIT_REDO,           GSCHEM_MAP(REDO),   TB_ICON_BITMAP, NULL},

  { ACTION(EDIT_SELECT),        "Select",    TBTS_EDIT_SELECT,        "gschem-select",     TB_ICON_STOCK, NULL},
  { ACTION(EDIT_DESELECT),      "Deselect",  TBTS_EDIT_DESELECT,      "gschem-unselect",   TB_ICON_STOCK, NULL},
  { ACTION(EDIT_DESELECT_ALL),  "Unselect",  TBTS_EDIT_DESELECT_ALL,  "unselect-all",      TB_ICON_STOCK, NULL},

  { "nil",                      "nil",        "nil",                  "gtk-no", 0, 0}, /* dummy corresponding to etb_none */

  { ACTION(EDIT_SELECT_ALL),    "All",       TBTS_EDIT_SELECT_ALL,    "gschem-select-all", TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_INVERT),        "Invert",    TBTS_EDIT_INVERT,        "gschem-invert",     TB_ICON_BITMAP, NULL},
  { ACTION(ADD_COMPONENT),      "Library",   TBTS_ADD_COMPONENT,      "gschem-transistor", TB_ICON_BITMAP, NULL},
  { ACTION(ADD_NET),            "Nets",      TBTS_ADD_NET,            "geda-net",          TB_ICON_STOCK, NULL},
  { ACTION(ADD_BUS),            "Bus",       TBTS_ADD_BUS,            "geda-bus",          TB_ICON_STOCK, NULL},
  { ACTION(ADD_ATTRIB),         "Attrib",    TBTS_ADD_ATTRIB,         "insert-attribute",  TB_ICON_STOCK, NULL},
  { ACTION(ADD_TEXT),           "Text",      TBTS_ADD_TEXT,           "gtk-bold",          TB_ICON_STOCK, NULL},

  /* Add Toolbar */
  { ACTION(ADD_LINE),           "Line",      TBTS_ADD_LINE,           "geda-line",      TB_ICON_STOCK, NULL},
  { ACTION(ADD_BOX),            "Box",       TBTS_ADD_BOX,            "geda-box",       TB_ICON_STOCK, NULL},
  { ACTION(ADD_CIRCLE),         "circle",    TBTS_ADD_CIRCLE,         "geda-circle",    TB_ICON_STOCK, NULL},
  { ACTION(ADD_ARC),            "Arc",       TBTS_ADD_ARC,            "geda-arc",       TB_ICON_STOCK, NULL},
  { ACTION(ADD_PATH),           "Path",      TBTS_ADD_PATH,           "geda-path",      TB_ICON_STOCK, NULL},
  { ACTION(ADD_PIN),            "Pin",       TBTS_ADD_PIN,            "geda-pin",       TB_ICON_STOCK, NULL},
  { ACTION(ADD_PICTURE),        "Image",     TBTS_ADD_PICTURE,        "geda-film-roll", TB_ICON_STOCK, NULL},

  /* Page Toolbar */
  { ACTION(PAGE_FIRST),         "First",     TBTS_PAGE_FIRST,         "gtk-goto-first",    TB_ICON_STOCK, NULL},
  { ACTION(PAGE_PREV),          "Prev",      TBTS_PAGE_UP,            "gtk-go-back",       TB_ICON_STOCK, NULL},
  { ACTION(PAGE_NEXT),          "Next",      TBTS_PAGE_DOWN,          "gtk-go-forward",    TB_ICON_STOCK, NULL},
  { ACTION(PAGE_UP),            "Up",        TBTS_PAGE_UP,            "gtk-go-up",         TB_ICON_STOCK, NULL},
  { ACTION(PAGE_DOWN),          "Down",      TBTS_PAGE_DOWN,          "gtk-go-down",       TB_ICON_STOCK, NULL},
  { ACTION(PAGE_LAST),          "Last",      TBTS_PAGE_LAST,          "gtk-goto-last",     TB_ICON_STOCK, NULL},
  { ACTION(PAGE_NEW),           "New",       TBTS_PAGE_NEW,           "gtk-new",           TB_ICON_STOCK, NULL},
  { ACTION(PAGE_MANAGER),       "Manage",    TBTS_PAGE_MANAGER,       GEDA_SHEETS_BITMAP,  TB_ICON_BITMAP, NULL},

  { ACTION(DOWN_SCHEMATIC),     "Down",      TBTS_DOWN_SCHEMATIC,     GEDA_DEMOTE_SCH_BITMAP, TB_ICON_BITMAP, NULL},
  { ACTION(DOWN_SYMBOL),        "Down",      TBTS_DOWN_SYMBOL,        GEDA_DEMOTE_SYM_BITMAP, TB_ICON_BITMAP, NULL},
  { ACTION(HIERARCHY_UP),       "Up",        TBTS_HIERARCHY_UP,       GEDA_PROMOTE_BITMAP,    TB_ICON_BITMAP, NULL},

  { ACTION(VIEW_DOCUMENT),      "Spec",      TBTS_VIEW_DOCUMENT,      "gaf-see-notes",        TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_NETS),          "Netnames",  TBTS_VIEW_NETNAMES,      "geda-show-nets",       TB_ICON_STOCK,  NULL},

  /* Zoom Toolbar */
  { ACTION(VIEW_REDRAW),        "Redraw",    TBTS_VIEW_REDRAW,        GEDA_MAP(VIEW_REDRAW),  TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_PAN),           "Pan",       TBTS_VIEW_PAN,           GEDA_MAP(ZOOM_PAN),     TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_BOX),           "Window",    TBTS_VIEW_BOX,           GEDA_MAP(ZOOM_BOX),     TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_SELECTED),      "Selected",  TBTS_VIEW_SELECTED,      "zoom-selection",       TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_EXTENTS),       "Extents",   TBTS_VIEW_EXTENTS,       GEDA_MAP(ZOOM_EXTENTS), TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_ZOOM_IN),       "In",        TBTS_VIEW_ZOOM_IN,       GEDA_MAP(ZOOM_IN),      TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_ZOOM_OUT),      "Out",       TBTS_VIEW_ZOOM_OUT,      "zoom-out",             TB_ICON_STOCK,  NULL},
  { ACTION(VIEW_ZOOM_ALL),      "All",       TBTS_VIEW_ZOOM_ALL,      GEDA_MAP(ZOOM_LIMITS),  TB_ICON_BITMAP, NULL},

  /* Symbol Toolbar */
  { ACTION(EDIT_COMPONENT),     "Properties",TBTS_EDIT_PROPERTIES,    "geda-properties",      TB_ICON_STOCK,  NULL},

  { ACTION(TOOLS_TRANSLATE),    "Translate", TBTS_TOOLS_TRANSLATE,    GEDA_MAP(TRANSLATE),    TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_LOCK),          "Lock",      TBTS_SELECT_LOCK,        "Private",              TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_UNLOCK),        "Unlock",    TBTS_SELECT_UNLOCK,      "Private",              TB_ICON_BITMAP, NULL},
  { ACTION(TOOLS_UPDATE),       "Update",    TBTS_TOOLS_UPDATE,       "Private",              TB_ICON_BITMAP, NULL},

  /* Edit Toolbar */
  { ACTION(EDIT_COPY),          "Copy",      TBTS_EDIT_COPY,          "Private",        TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_MCOPY),         "Multi",     TBTS_EDIT_MCOPY,         "Private",        TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_MOVE),          "Move",      TBTS_EDIT_MOVE,          "geda-move",      TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_MIRROR),        "Mirror",    TBTS_EDIT_MIRROR,        "Private",        TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_ROTATE_LEFT),   "Rotate",    TBTS_EDIT_ROTATE_LEFT,   "Private",        TB_ICON_BITMAP, NULL},

  { ACTION(EDIT_ATTRIB),        "Edit",      TBTS_EDIT_ATTRIB,        "Private",        TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_COLOR),         "Color",     TBTS_EDIT_COLOR,         "Private",        TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_TEXT),          "Text",      TBTS_EDIT_TEXT,          "Private",        TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_SLOT),          "Slots",     TBTS_EDIT_SLOT,          "geda-slot",      TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_PIN),           "Pins",      TBTS_EDIT_PIN,           "geda-pin-type",  TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_LINE),          "Line",      TBTS_EDIT_LINE,          "Private",        TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_FILL),          "Fill",      TBTS_EDIT_FILL,           GEDA_MAP(MESH),  TB_ICON_BITMAP, NULL},
  { ACTION(EDIT_ARC),           "Arcs",      TBTS_EDIT_ARC,           "geda-arc-edit",  TB_ICON_BITMAP, NULL},

  /* Modify Toolbar */
  { ACTION(EDIT_ARRAY),         "Array",     TBTS_EDIT_ARRAY,         "gschem-array",   TB_ICON_BITMAP, NULL},

  /* Attribute Toolbar */
  { ACTION(ATTRIB_ATTACH),      "Promote",   TBTS_ATTRIB_ATTACH,      "Private",                   TB_ICON_BITMAP, NULL},
  { ACTION(ATTRIB_DETACH),      "Demote",    TBTS_ATTRIB_DETACH,      "Private",                   TB_ICON_BITMAP, NULL},
  { ACTION(ATTRIB_VALUE),       "Value",     TBTS_ATTRIB_VALUE,        GEDA_MAP(VALUE),            TB_ICON_BITMAP, NULL},
  { ACTION(ATTRIB_NAME),        "Name",      TBTS_ATTRIB_NAME,        "show-name",                 TB_ICON_BITMAP, NULL},
  { ACTION(ATTRIB_BOTH),        "Both",      TBTS_ATTRIB_BOTH,         GEDA_MAP(NAME_VALUE),       TB_ICON_BITMAP, NULL},
  { ACTION(ATTRIB_VISIBILITY),  "Visible",   TBTS_ATTRIB_VISIBILITY,   GEDA_MAP(EYE_GLASSES),      TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_HIDDEN),        "Hidden",    TBTS_VIEW_HIDDEN,        "show-hidden",               TB_ICON_BITMAP, NULL},
  { ACTION(VIEW_INHERITED),     "Inherited", TBTS_VIEW_INHERITED,     "show-inherited",            TB_ICON_BITMAP, NULL},
  { ACTION(ATTRIB_FIND),        "Find",      TBTS_ATTRIB_FIND,         GEDA_MAP(FIND_ATTRIBUTE),   TB_ICON_BITMAP, NULL},
  { ACTION(ATTRIB_HIDE),        "Hide",      TBTS_ATTRIB_HIDE,        "geda-invisible",            TB_ICON_BITMAP, NULL},
  { ACTION(ATTRIB_SHOW),        "Show",      TBTS_ATTRIB_SHOW,         GEDA_MAP(LOCATE_REFERENCE), TB_ICON_BITMAP, NULL},

  { ACTION(TOOLS_AUTONUM),      "Auto #",    TBTS_TOOLS_AUTONUM,      "geda-autonum-blue.png",   TB_ICON_BITMAP, NULL},

  { ACTION(OPT_GRID_DOT),       "Dots",      TBTS_OPT_GRID_DOT,       "geda-grid-dot",     TB_ICON_BITMAP, NULL},
  { ACTION(OPT_GRID_MESH),      "Mesh",      TBTS_OPT_GRID_MESH,      "geda-grid-mesh",    TB_ICON_BITMAP, NULL},
  { ACTION(OPT_GRID_OFF),       "Off",       TBTS_OPT_GRID_OFF,       "geda-display",      TB_ICON_BITMAP, NULL},

  { ACTION(OPT_SNAP_UP),        "UP",        TBTS_OPT_SNAP_UP,        "geda-snap",         TB_ICON_BITMAP, NULL},
  { ACTION(OPT_SNAP_DOWN),      "Down",      TBTS_OPT_SNAP_DOWN,      "geda-snap",         TB_ICON_BITMAP, NULL},
  { ACTION(OPT_SNAP_SIZE),      "Set",       TBTS_OPT_SNAP_SIZE,      "geda-magnet",       TB_ICON_BITMAP, NULL},
  { ACTION(OPT_SNAP_OFF),       "Snap Off",  TBTS_OPT_SNAP_OFF,       "geda-snap-off",     TB_ICON_BITMAP, NULL},
  { ACTION(OPT_SNAP_ON),        "Snap On",   TBTS_OPT_SNAP_ON,        "geda-snap-on",      TB_ICON_BITMAP, NULL},

  { ACTION(OPT_SETTINGS),       "Config",    TBTS_OPT_SETTINGS,        GEDA_MAP(TOOLS),    TB_ICON_BITMAP, NULL},
  { NULL, NULL, NULL},
};

typedef enum  { DockBar,
                HideBar,
                MakeHorizontal,
                MakeVertical,
                ShowIcons,
                ShowText,
                ShowBoth,
                ShowHorizontal,

}  IDS_HB_Popup_items; /* Enumerators to reference the string below: */

static char *popup_items[]={ "Dock",
                             "Hide",
                             "Horizontal",
                             "Vertical",
                             "show Icons",
                             "show Text",
                             "show Both",
                             "show Horizontal"
};

static char *popup_tips[]={  "Dock",
                             "Hide",
                             "Horizontal",
                             "Vertical",
                             "Display only the icons",
                             "Display only the text",
                             "Display both icons and text",
                             "Display the icons and text side-by-side"
};

static GtkWidget *get_pixmap(GschemToplevel *w_current, const char *name)
{
  GtkWidget *wpixmap = NULL;

  char *filename = geda_file_get_bitmap_filespec (name);

  /* First check for custom icon */
  if (filename) {

    if (access(filename, R_OK) == 0) {

#if GTK_MAJOR_VERSION < 3

      GdkColor  *bg_color;
      GdkBitmap *mask;
      GdkPixmap *pixmap;
      GtkStyle  *style;
      GdkWindow *window;

      window   = gschem_main_window_get_window (MainWidget);
      style    = gschem_main_window_get_style (MainWidget);
      bg_color = &style->bg[GTK_STATE_NORMAL];
      pixmap   = gdk_pixmap_create_from_xpm (window, &mask, bg_color, filename);

      if (pixmap != NULL) {
        wpixmap = gtk_image_new_from_pixmap (pixmap, mask);
      }
#else
      wpixmap = gtk_image_new_from_file(filename);
#endif

    }

    GEDA_FREE(filename);
  }

  if (wpixmap == NULL) { /* Try falling back to Stock icon */
    wpixmap = gtk_image_new_from_stock(name, TB_SMALL_ICON);
  }

  if (wpixmap == NULL) {

    const char *log_msg = _("image file not found");

    geda_log_v("%s: %s \"%s\".\n", __func__, log_msg, name);

    wpixmap = gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE , TB_SMALL_ICON);
  }
  else {
    gtk_image_set_pixel_size((GtkImage*)wpixmap, TB_SMALL_ICON);
  }

  return wpixmap;
}

/*!
 * \brief Toolbar Button Callback
 * \par Function Description
 *  This function handles callbacks for all non-toggle type toolbar
 *  buttons, the function retrieves the action from the button widget
 *  and pass the action to i_command_process.
 */
static void x_toolbars_execute(GtkWidget *widget, GschemToplevel *w_current)
{
  char *action;

  action = GEDA_OBJECT_GET_DATA(widget, "action");

#if DEBUG_TOOLBARS
  fprintf(stderr, "%s: widget=%p action=%s\n", __func__, widget, action);
#endif

  i_command_process(w_current, action, 0, NULL, ID_ORIGIN_TOOLBAR);
}

/*!
 * \brief Preload Toolbar Button Icons
 * \par Function Description
 *  This function iterates over the ToolbarStrings structure, calling
 *  x_icons_get_action_icon with the action field, saving the returned
 *  pointer to the icon field, failing that, either get_pixmap or a gtk
 *  stock is sought based on iflag.
 */
static void x_toolbars_load_icons( GschemToplevel *w_current)
{

  ToolbarStringData *tb_data;
  int index;

  for (index = 0; index < etb_last; index++) {

    if (index == etb_none) continue;

    tb_data = &ToolbarStrings[index];

    tb_data->icon = x_icons_get_action_icon(tb_data->action, TB_SMALL_ICON);

    if (!GTK_IS_IMAGE(tb_data->icon)) {

      const char *icon_id = tb_data->icon_id;

      if (tb_data->iflag == TB_ICON_BITMAP) {

         tb_data->icon = get_pixmap (w_current, icon_id);

      }
      else {

        GtkStockItem stock_info;

        if (gtk_stock_lookup (icon_id, &stock_info)) {
          tb_data->icon = gtk_image_new_from_stock(icon_id, TB_SMALL_ICON);
        }
        else if (gtk_icon_factory_lookup_default(icon_id)) {
          tb_data->icon = x_icons_get_factory_icon(icon_id, TB_SMALL_ICON);
        }
      }
    }

    if (!tb_data->icon) {
      const char *log_msg = _("Toolbar icon not found");
      geda_log("%s: <%s>\n", log_msg, tb_data->icon_id);
    }

#if DEBUG_TB_CONS
    else {
      if (strcmp(tb_data->action, ACTION(VIEW_ZOOM_OUT)) == 0) {
        fprintf(stderr, "Got action <%s> icon %p\n", tb_data->action, tb_data->icon);
      }
    }
#endif

  }
}

static void x_toolbars_turn_off_radio(RadioMenuData *radio_data) {
  g_signal_handler_block   ( radio_data->widget,   radio_data->handler);
  g_object_set             ( radio_data->widget,  "active", FALSE, NULL);
  g_signal_handler_unblock ( radio_data->widget,   radio_data->handler);
}

static void x_toolbars_turn_on_radio(RadioMenuData *radio_data) {
  g_signal_handler_block   ( radio_data->widget,   radio_data->handler);
  g_object_set             ( radio_data->widget,  "active", TRUE, NULL);
  g_signal_handler_unblock ( radio_data->widget,   radio_data->handler);
}

/*!
 * \brief Toolbar Radio Button Callback
 * \par Function Description
 *  This function handles callbacks for radio toolbar buttons,
 *  the function retrieves the action from the button widget
 *  and passes the action to i_command_process.
 */
static void x_toolbars_execute_radio (GtkToggleButton *button, GschemToplevel *w_current)
{
  char *action = GEDA_OBJECT_GET_DATA(button, "action");

#if DEBUG_TOOLBARS
  fprintf(stderr, "%s: button=%p action=%s\n", __func__, button, action);
#endif

  if ((strcmp(action, "none") != 0) && (button->active)) {
    i_command_process(w_current, action, 0, NULL, ID_ORIGIN_TOOLBAR);
  }
}

/*!
 * \brief Toolbar Toggler Button Callback
 * \par Function Description
 *  This function handles callbacks for radio toolbar toggle buttons and
 *  is similar to x_toolbars_execute_radio() but toggle the the state of
 *  the snap-widget before passing the action from the button widget
 *  to i_command_process().
 */
static void x_toolbars_snap_toggle(GtkWidget *widget, GschemToplevel *w_current)
{
  char      *action;
  GtkWidget *button;

  action = GEDA_OBJECT_GET_DATA(widget, "action");
  button = GEDA_OBJECT_GET_DATA(widget, "snap-widget");

  g_object_set (widget, "visible",  FALSE, NULL);
  g_object_set (button, "visible", TRUE, NULL);

#if DEBUG_TOOLBARS
  fprintf(stderr, "%s: action=%s\n", __func__, action);
#endif
  i_command_process(w_current, action, 0, NULL, ID_ORIGIN_TOOLBAR);
}

/*!
 * \brief Save Toolbar Configuration
 * \par Function Description
 *  This function saves the state of the Toolbar widgets so we
 *  can restore them to same states the next time we run.
 */
void
x_toolbars_save_state(GschemToplevel *w_current)
{
  GedaKeyFile *key_file = NULL;
  char        *filename;

  void SaveBarProperties(GtkWidget *handlebox) {

    GtkWidget   *toolbar;
    const char  *group_name;
    int   bar_id;
    int   visible;
    int   style;
    int   tooltips;

    bar_id     = GET_TOOLBAR_ID(handlebox);
    group_name = IDS_Toolbar_Names[bar_id];
    visible    = gtk_widget_get_visible(handlebox);

    geda_keyfile_set_integer (key_file, group_name, "visible", visible);

    if (w_current->handleboxes) {
      toolbar = geda_get_child_widget (handlebox);
    }
    else {
      GList *list;
      list     = geda_container_get_children (handlebox);
      toolbar  = list->data;
      g_list_free(list);
    }

    style    = geda_toolbar_widget_get_style (toolbar);
    tooltips = geda_toolbar_widget_get_tooltips (toolbar);

    geda_keyfile_set_integer (key_file, group_name, "style", style);
    geda_keyfile_set_integer (key_file, group_name, "tooltips", tooltips);
  }

  void SaveAllBars() {
    SaveBarProperties(w_current->add_handlebox);
    SaveBarProperties(w_current->attribute_handlebox);
    SaveBarProperties(w_current->edit_handlebox);
    SaveBarProperties(w_current->grid_snap_handlebox);
    SaveBarProperties(w_current->modify_handlebox);
    SaveBarProperties(w_current->page_handlebox);
    SaveBarProperties(w_current->select_handlebox);
    SaveBarProperties(w_current->standard_handlebox);
    SaveBarProperties(w_current->symbol_handlebox);
    SaveBarProperties(w_current->zoom_handlebox);
  }

  bool setup_new_keyfile (char *filename) {

    key_file = geda_keyfile_new();

    if (access(filename, W_OK) != 0) {
      geda_log_v(_("Creating new Toolbar configuration\n"));
      geda_create_path (geda_user_config_path (), S_IRWXU | S_IRWXG);
      g_file_set_contents (filename, "", -1, NULL);
    }

    return g_file_test (filename, G_FILE_TEST_EXISTS);
  }

  geda_log_v(_("Saving Toolbar settings..."));

  filename = g_build_filename(geda_user_config_path (), TOOLBAR_CONFIG_STORE, NULL);

  if (!g_file_test (filename, G_FILE_TEST_EXISTS)) {
    setup_new_keyfile (filename);
  }
  else {
    key_file = geda_keyfile_new();
  }

  if (key_file) {

    char *data;

    SaveAllBars();

    data = geda_keyfile_to_data(key_file, NULL, NULL);

    g_file_set_contents(filename, data, -1, NULL);

    geda_log_v("%s %s\n", _("data saved to"), filename);

    GEDA_FREE(data);
    geda_keyfile_free(key_file);
  }
  else {
    /* Could not save the toolbar configuration to */
    const char *log_msg = _("Could not save Toolbar configuration to");
    geda_log("%s %s\n", log_msg, filename);
  }

  GEDA_FREE(filename);
}

/*!
 * \brief Restore Toolbar Configuration
 * \par Function Description
 *  This function restores the state of the Toolbar widgets.
 */
void
x_toolbars_restore_state(GschemToplevel *w_current) {

  GError      *err;
  GedaKeyFile *key_file;
  char        *filename;
  int          visible_count;   /* For counting visible toolbars */
  int          global_style;
  int          global_tooltips;

  void RestoreBarProperties(GtkWidget *handlebox) {

    const char *group_name;
    int         bar_id;

    bar_id     = GET_TOOLBAR_ID(handlebox);
    group_name = IDS_Toolbar_Names[bar_id];

    if (key_file) {

      int visible;

      err     = NULL;
      visible = geda_keyfile_get_integer (key_file, group_name, "visible", &err);

      if (err) {
        g_clear_error (&err);
        err     = NULL;
        visible = TRUE;
      }

      /* Set visibility based on ini file */
      gtk_widget_set_visible(handlebox, visible);
      x_menu_set_toolbar_toggle(w_current, bar_id, visible);

      if (visible) {

        GtkWidget *toolbar;

        int style;
        int tooltips;

        /* Retrieve the style from ini file for this toolbar */
        style = geda_keyfile_get_integer (key_file, group_name, "style", &err);

        if (err) {
          g_clear_error (&err);
          err   = NULL;
          style = DEFAULT_TOOLBAR_STYLE;
        }

        global_style += style;

        /* Retrieve the tooltips setting from ini file for this toolbar */
        tooltips = geda_keyfile_get_integer (key_file, group_name, "tooltips", &err);

        if (err) {
          g_clear_error (&err);
          err = NULL;
          tooltips = DEFAULT_TOOLBAR_TIPS;
        }

        global_tooltips += tooltips;

        /* Get pointer to toolbar */
        if (w_current->handleboxes) {
          toolbar = geda_get_child_widget (handlebox);
        }
        else {

          GList *list;
          list    = geda_container_get_children (handlebox);

          if (list) {
            toolbar = list->data;
            g_list_free(list);
          }
          else {
            fprintf(stderr, "%s: bar id <%d> group <%s> is empty\n", __func__, bar_id, group_name);
            toolbar = NULL;
          }
        }

        if (toolbar) {
           /* Set the toolbar style and tooltips properties */
           geda_toolbar_widget_set_style(toolbar, style);
           geda_toolbar_widget_set_tooltips(toolbar, tooltips);
        }
      }
      else {
        /* The bar was off, i.e. is not visible*/
        visible_count--;
      }
    }
    else {
      const char *log_msg = _("Error, Toolbar configuration key file");
      geda_log("%s, %s\n", log_msg, group_name);
    }
  }

  void RestoreAllBars() {

    if (key_file) {
      geda_log_v(_("Retrieving toolbar geometry\n"));
      RestoreBarProperties(w_current->add_handlebox);
      RestoreBarProperties(w_current->attribute_handlebox);
      RestoreBarProperties(w_current->edit_handlebox);
      RestoreBarProperties(w_current->grid_snap_handlebox);
      RestoreBarProperties(w_current->page_handlebox);
      RestoreBarProperties(w_current->select_handlebox);
      RestoreBarProperties(w_current->standard_handlebox);
      RestoreBarProperties(w_current->symbol_handlebox);
      RestoreBarProperties(w_current->zoom_handlebox);
    }
  }

  global_style    = 0;
  global_tooltips = 0;
  visible_count   = tb_Zoom + 1; /* Initialize to total bar count */
  key_file        = NULL;

  filename = g_build_filename(geda_user_config_path (), TOOLBAR_CONFIG_STORE, NULL);

  if (g_file_test (filename, G_FILE_TEST_EXISTS)) {

    if (access(filename, R_OK) == 0) {

      err      = NULL;
      key_file = geda_keyfile_new();

      if (geda_keyfile_load_from_file(key_file, filename, G_KEY_FILE_NONE, &err)) {
        const char *log_msg = _("Toolbar configuration restored from");
        RestoreAllBars();
        geda_log_v("%s %s\n", log_msg, filename);
      }
      else {
        const char *log_msg = _("Warning, error restoring toolbar configuration");
        geda_log("%s, %s %s\n", log_msg, filename, err->message);
        g_clear_error (&err);
      }
    }
    else {
      const char *log_msg = _("Warning, toolbar configuration file access error");
      geda_log("%s: %s %s\n", log_msg, filename, err->message);
    }

    /* Check if the style of visible toolbars is uniform and set radio
     * in menu if all the same style */
    global_style = global_style / visible_count;

    if ((global_style == TOOLBAR_SHOW_ICONS) ||
        (global_style == TOOLBAR_SHOW_TEXT)  ||
        (global_style == TOOLBAR_SHOW_BOTH)  ||
        (global_style == TOOLBAR_SHOW_HORIZ))
    {
      x_toolbars_turn_on_radio ((RadioMenuData*)g_slist_nth_data (w_current->toolbar_mode_grp,
                                                                  global_style));
    }

    /* Set menu toggle if all visible toolbars have the same setting */
    x_menu_set_toolbar_toggle_tips(w_current, (global_tooltips == visible_count));

  }
  else {
    const char *log_msg = _("Toolbar configuration not found");
    geda_log_v("%s: <%s>!\n", log_msg, filename);
  }

  if (key_file) {
    geda_keyfile_free(key_file);
  }

  GEDA_FREE(filename);
}

/*!
 * \brief Finalize Toolbar Initialization
 * \par Function Description
 * This function completes the final configuration of the toolbar setup
 * based on settings establish during gschem boot-up. The function also
 * sets the visibility of the Close buttons on all the handleboxes.
 * The Main window did a Show All and that revealed all the buttons on
 * handleboxes that should be hidden if bar is docked. Rather than setting
 * each widget individually when creating the main window, it's easier to
 * "fix" this by having this routine emit a signal to each handlebox.
 *
 * \param [in] w_current pointer to top-level data structure
 */
void
x_toolbars_finalize (GschemToplevel *w_current) {

  ToolBarWidgets *bar_widgets;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  if (w_current->toolbars_mode != RC_NIL) { /* If not keyword then let GTK decide */
    if (w_current->toolbars_mode == TOOLBAR_RETENTION) {
      x_toolbars_restore_state(w_current);
    }
    else { /* use rc value */

      lambda (GtkWidget *bar) {
        geda_toolbar_widget_set_style (bar, TOOLBAR_STYLE);
        geda_toolbar_widget_set_tooltips (bar, TOOLBAR_TIPS);
        return FALSE;
      }
      mapcar(TheToolBars);
    }
  }

  if (w_current->handleboxes) {
    g_signal_emit_by_name(w_current->add_handlebox,       "child-attached");
    g_signal_emit_by_name(w_current->attribute_handlebox, "child-attached");
    g_signal_emit_by_name(w_current->edit_handlebox,      "child-attached");
    g_signal_emit_by_name(w_current->grid_snap_handlebox, "child-attached");
    g_signal_emit_by_name(w_current->page_handlebox,      "child-attached");
    g_signal_emit_by_name(w_current->modify_handlebox,    "child-attached");
    g_signal_emit_by_name(w_current->select_handlebox,    "child-attached");
    g_signal_emit_by_name(w_current->standard_handlebox,  "child-attached");
    g_signal_emit_by_name(w_current->symbol_handlebox,    "child-attached");
    g_signal_emit_by_name(w_current->zoom_handlebox,      "child-attached");
  }

  /* Hide the toolbar_none radio button */
  gtk_widget_hide(bar_widgets->toolbar_none);

  x_toolbars_update(w_current);
}

static void x_toolbars_free_foreach (GtkWidget *widget, void *data)
{
  gtk_widget_set_name (widget, NULL);
}

/*!
 * \brief Free Window Specific Toolbar Widgets
 * \par Function Description
 *  This function releases the memory associated with a ToolBarWidgets
 *  structure that was allocated with malloc in x_toolbars_init_window.
 */
void
x_toolbars_free_window(GschemToplevel *w_current)
{
  ToolBarWidgets *bar_widgets;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GtkWidget *bar) {
    geda_container_foreach (bar, x_toolbars_free_foreach, NULL);
    gtk_widget_destroy (bar);
    return FALSE;
  }
  mapcar(TheToolBars);
  g_slist_free (TheToolBars);

  g_slist_free (ANY_OBJECT_LIST);
  g_slist_free (CAN_PASTE_LIST);
  g_slist_free (CAN_UNDO_LIST);
  g_slist_free (CAN_REDO_LIST);
  g_slist_free (CAN_HATCH_LIST);
  g_slist_free (CAN_ELINE_LIST);
  g_slist_free (HAVE_COMPLEX_LIST);
  g_slist_free (HAVE_PAGES_LIST);
  g_slist_free (HAVE_PIN_LIST);
  g_slist_free (TEXT_OBJECT_LIST);

  g_slist_free (TOOLBAR_RADIOS);

  ANY_OBJECT_LIST    = NULL;
  CAN_PASTE_LIST     = NULL;
  CAN_UNDO_LIST      = NULL;
  CAN_REDO_LIST      = NULL;
  CAN_HATCH_LIST     = NULL;
  CAN_ELINE_LIST     = NULL;
  HAVE_COMPLEX_LIST  = NULL;
  HAVE_PAGES_LIST    = NULL;
  HAVE_PIN_LIST      = NULL;
  TEXT_OBJECT_LIST   = NULL;

  TOOLBAR_RADIOS     = NULL;
  TheToolBars        = NULL;

  ui_list = g_slist_remove (ui_list, bar_widgets);
  GEDA_FREE(bar_widgets);
}

/*! \brief Callback helper, used by Close button and Popup */
static void
do_Hide_HandleBox(GedaHandleBox *handlebox)
{
  if (GEDA_IS_HANDLE_BOX(handlebox)) {

    gtk_widget_hide((GtkWidget*)handlebox);

    GschemToplevel *w_current = (GschemToplevel*)GET_TOOLBAR_WC(handlebox);
    int HandleBoxId = GET_TOOLBAR_ID(handlebox);

    x_menu_set_toolbar_toggle(w_current, HandleBoxId, FALSE);

  }
  else {
    BUG_MSG("container is not a handlebox");
  }
}

/*!
 * \brief Callback Handler for Popup Mouse Context Menu
 * \par Function Description
 * This function calls the appropriate functions to process request
 * from the mouse menu. This function receives a pointer to enumerated
 * integer value for the menu item that was selected.
 *
 * \param [in] widget is button widget
 * \param [in] selection pointer to enumerated menu selection
 */
static int popup_activated(GtkWidget *widget, IDS_HB_Popup_items* selection)
{
    int WhichItem = (int)(long) selection;

    switch ( WhichItem ) {
      case DockBar:
        geda_handle_box_dock(ActiveToolBar.handlebox);
        break;

      case HideBar:
        do_Hide_HandleBox((GedaHandleBox*)ActiveToolBar.handlebox);
        break;

      case MakeHorizontal:
        SET_TOOLBAR_ORIENTATION (ActiveToolBar.toolbar, HORIZONTAL);
        break;

      case MakeVertical:
        SET_TOOLBAR_ORIENTATION (ActiveToolBar.toolbar, VERTICAL);
        break;

      case ShowIcons:
        geda_toolbar_set_style (ActiveToolBar.toolbar, TOOLBAR_SHOW_ICONS);
        break;

      case ShowText:
        gtk_toolbar_set_style (GTK_TOOLBAR (ActiveToolBar.toolbar), TOOLBAR_SHOW_TEXT);
        break;

      case ShowBoth:
        gtk_toolbar_set_style (GTK_TOOLBAR (ActiveToolBar.toolbar), TOOLBAR_SHOW_BOTH);
        break;

      case ShowHorizontal:
        geda_toolbar_set_style (ActiveToolBar.toolbar, TOOLBAR_SHOW_HORIZ);
        break;

      default:
        fprintf(stderr, "%s: unhandled case <%d>\n",__func__, WhichItem);
    } /* End Switch WhichItem */

    gtk_widget_destroy(popup_menu);
    return (TRUE);
}

/*!
 * \brief Create and Setup Popup Mouse Menu for Toolbar
 * \par Function Description
 *  This function is called when the user right clicks on a handlebox.
 *  The function sets senitivty on menu choices based on the handlebox
 *  position and the state of the containing toolbar.
 *
 * \param [in] handlebox is the GedaHandleBox that was clicked on
 */
static GtkWidget *build_menu(GedaHandleBox *handlebox)
{
  GtkWidget   *menu;
  GtkTooltips *tooltips;

  bool is_floating;
  int  orientation;
  int  style;
  int  i;

  ActiveToolBar.handlebox = handlebox;
  ActiveToolBar.toolbar   = geda_handle_box_get_toolbar(ActiveToolBar.handlebox);

  is_floating = !geda_handle_box_get_child_detached (ActiveToolBar.handlebox);
  orientation =  geda_toolbar_get_orientation(ActiveToolBar.toolbar);
  style       =  geda_toolbar_get_style(ActiveToolBar.toolbar);
  tooltips    =  gtk_tooltips_new ();
  menu        =  geda_menu_new();

  for (i=0; i < (sizeof(popup_items)/sizeof(popup_items[0])) ; i++)
  {
    GtkWidget *item;

    item = geda_menu_item_new_with_label(_(popup_items[i]));

    gtk_tooltips_set_tip (tooltips, item, _(popup_tips[i]), NULL);

    g_signal_connect(item,"activate",
                    (void*) popup_activated,
                    (void*) (long) i);

    gtk_widget_set_sensitive(item, TRUE);
    gtk_widget_set_can_focus(item, TRUE);

    switch (i) {
      case DockBar:
        if (is_floating) {
          gtk_widget_set_sensitive(item, FALSE);
          gtk_widget_set_can_focus(item, FALSE);
        }
        break;
      case HideBar:
        break;
      case MakeHorizontal:
        if (is_floating || orientation == GTK_ORIENTATION_HORIZONTAL) {
          gtk_widget_set_sensitive(item, FALSE);
          gtk_widget_set_can_focus(item, FALSE);
        }
        break;
      case MakeVertical:
        if (is_floating || orientation == GTK_ORIENTATION_VERTICAL) {
          gtk_widget_set_sensitive(item, FALSE);
          gtk_widget_set_can_focus(item, FALSE);
        }
        break;
      case ShowIcons:
        if (style == TOOLBAR_SHOW_ICONS) {
          gtk_widget_set_sensitive(item, FALSE);
          gtk_widget_set_can_focus(item, FALSE);
        }
        break;
      case ShowText:
        if (style == TOOLBAR_SHOW_TEXT) {
          gtk_widget_set_sensitive(item, FALSE);
          gtk_widget_set_can_focus(item, FALSE);
        }
        break;
      case ShowBoth:
        if (style == TOOLBAR_SHOW_BOTH) {
          gtk_widget_set_sensitive(item, FALSE);
          gtk_widget_set_can_focus(item, FALSE);
        }
        break;
      }
      g_object_set (item, "visible", TRUE, NULL);
      geda_menu_append(menu, item);
    }
    return (menu);
}

/*!
 * \brief HandleBar Mouse Button Call Back
 * \par Function Description
 *  This function check mouse botton press and when the 3rd button
 *  is released the build_menu function is called to create the mouse
 *  menu.
 *
 * \param [in] widget     The handlebox widget when user "right-clicked"
 * \param [in] event      Mouse event record
 * \param [in] w_current  Gschem toplevel object.
 */
static int
On_mouse_button_press(GtkWidget *widget, GdkEventButton *event, GschemToplevel *w_current)
{
  GdkModifierType mods;
  GedaHandleBox *handlebox = (GedaHandleBox*)widget;

  gdk_window_get_pointer (gtk_widget_get_window(widget), NULL, NULL, &mods);

  if (mods & GDK_BUTTON3_MASK) {

    if (popup_menu) {

      gtk_widget_destroy(GTK_WIDGET(popup_menu));
      popup_menu = NULL;
    }

    popup_menu = build_menu(handlebox);

    /* Tell GTK to do the menu we just created */
    geda_menu_popup(GEDA_MENU(popup_menu), NULL, NULL, NULL, NULL,
                    event->button, event->time);
  }
  return (FALSE);
}

/*! @brief Toolbar Close Button Handler */
static void
On_Close_Handlebar(GtkWidget *CloseButton, GschemToplevel *w_current)
{
  GtkWidget *container;

  container = gtk_widget_get_parent(CloseButton);
  container = gtk_widget_get_parent(container);
  container = gtk_widget_get_parent(container);

  do_Hide_HandleBox((GedaHandleBox*) container);
}

/*! @brief Toolbar On Dock Handler to Hide the Close Button */
static void
On_Dock_ToolBar(GedaHandleBox *handlebox, GtkWidget *widget, GtkWidget *CloseButton) {
  geda_handle_box_set_shadow_type ( handlebox, GTK_SHADOW_ETCHED_OUT );
  gtk_widget_hide (CloseButton);
}

/*! @brief Toolbar On Float Handler to Show the Close Button */
static void
On_Float_ToolBar(GedaHandleBox *handlebox, GtkWidget *widget, GtkWidget *CloseButton) {
  geda_handle_box_set_shadow_type (handlebox, GTK_SHADOW_ETCHED_IN);
  gtk_widget_show (CloseButton);
}

/*!
 * \brief Add Close Button Toolbars - the little red X's
 * \par Function Description
 * This function creates a single close buttons on the each toolbars
 * along with the necessary alignment containers. The function sets up
 * callbacks to the functions defined above.
 *
 * \param [in] w_current  Gschem toplevel object
 * \param [in] HandleBar  The parent containing handlebox object
 * \param [in] ToolBar    Toolbar object
 */
static void
x_toolbars_add_closer(GschemToplevel *w_current, GtkWidget *HandleBar, GtkWidget *ToolBar)
{
  if (w_current->handleboxes) {

    GtkWidget *CloseButton;
    GtkWidget *fixed;
    GtkWidget *alignment;
    GtkStyle  *style;
    GtkWidget *x_image;

    /* Create a Fixed Widget to hold the Close Buttton and add to the Toolbar */
    fixed = gtk_fixed_new();
    geda_container_add (ToolBar, fixed);
    gtk_widget_show (fixed);

    /* Create the Close Button */
    CloseButton = gtk_button_new();
    gtk_widget_set_no_show_all (CloseButton, TRUE);

    /* Set Properties and Styles for the Close Button */
    gtk_widget_set_size_request(CloseButton, 16, 16);
    gtk_button_set_relief (GTK_BUTTON (CloseButton), GTK_RELIEF_NONE);
    gtk_button_set_focus_on_click (GTK_BUTTON (CloseButton), FALSE);

    style = gtk_widget_get_style(CloseButton);
    style->bg[GTK_STATE_PRELIGHT] = style->bg[GTK_STATE_NORMAL];
    gtk_widget_set_style (CloseButton, style);

    /* Put the Close Buttton inside the Fixed container and show it */
    geda_container_add (fixed, CloseButton);     /* Put Button Widget Inside the fixed container */
    gtk_widget_show (CloseButton);

    /* Create a New Alignment widget to hold the button Image */
    alignment = gtk_alignment_new (0, 0, 0, 0);
    gtk_widget_show (alignment);
    geda_container_add (CloseButton, alignment); /* Put Alignment Widget Inside the Button */

    /* Create a Pixmap widget containing the image and add the widget to the container */
    x_image = create_pixmap (CLOSE_TOOLBAR_BITMAP);
    geda_container_add (alignment, x_image);     /*Put image inside the Alignment container */
    gtk_widget_show (x_image);

    /* Setup the signal handlers */
    g_signal_connect (CloseButton, "pressed",
                      G_CALLBACK (On_Close_Handlebar),
                      w_current); /* not really needed since menu has embed ptr */

    g_signal_connect (HandleBar, "child-attached",
                      G_CALLBACK (On_Dock_ToolBar),
                      CloseButton);

    g_signal_connect (HandleBar, "child-detached",
                      G_CALLBACK (On_Float_ToolBar),
                      CloseButton);

    g_signal_connect (HandleBar, "button_press_event",
                      G_CALLBACK (On_mouse_button_press),
                      w_current);

    GEDA_OBJECT_SET_DATA(HandleBar, CloseButton, "CloseButton");
  }

  return;
}

/*!
 * \section Initialize-Toolbars
 * \par
 *  Note that this section relies heavily on MACROS defined in
 *  geda_toolbars.h in order to reduce coding errors and to help
 *  clarify the "subject" of the algorithms, rather then obsuring
 *  the intent in >10k lines of gtk_xxx's.
 *
 *  \param [in] w_current  Gschem toplevel object.
 */

void
x_toolbars_init_window(GschemToplevel *w_current)
{
  ToolBarWidgets *bar_widgets = GEDA_MEM_ALLOC0(sizeof(ToolBarWidgets));

  TheToolBars        = NULL;

  ANY_OBJECT_LIST    = NULL;
  CAN_PASTE_LIST     = NULL;
  CAN_UNDO_LIST      = NULL;
  CAN_REDO_LIST      = NULL;
  CAN_HATCH_LIST     = NULL;
  CAN_ELINE_LIST     = NULL;
  HAVE_COMPLEX_LIST  = NULL;
  HAVE_PAGES_LIST    = NULL;
  HAVE_PIN_LIST      = NULL;
  TEXT_OBJECT_LIST   = NULL;

  if (w_current->ui_index > -1)
    ui_list = g_slist_insert (ui_list, bar_widgets, w_current->ui_index);
  else
    ui_list = g_slist_append (ui_list, bar_widgets);

  x_toolbars_load_icons(w_current);
}

/*!
 * \brief Create a Container for a Toolbar
 * \par Function Description
 *  This function gets a container to hold a Toolbar. If handleboxes
 *  are enabled a GedaHandleBox is created, otherwise a GtkBox widget
 *  is created. The functions returns a pointer to the new widget.
 *
 * \param [in] w_current Gschem toplevel structure
 *
 * \returns either a GedaHandleBox or a GtkBox
 */
GtkWidget*
x_toolbars_get_box_container(GschemToplevel *w_current)
{
  GtkWidget *widget;

  if (w_current->handleboxes) {
    widget = geda_handle_box_new();
  }
  else {
    widget = gtk_hbox_new (FALSE, 0);
  }

  return widget;
}

/*!
 * \brief Initialize Toolbars at the Top of the Main Window
 * \par Function Description
 * This function creates handleboxes, toolbars, toolbar buttons and
 * related containers in order to create all of the Toolbars at the
 * Top of the Main Window. This function initializes the global
 * variables in the compilation unit.
 *
 * \param [in] w_current  Gschem toplevel structure
 * \param [in] parent_container is main vbox widget (main_box)
 */
void
x_toolbars_init_top(GschemToplevel *w_current, GtkWidget *parent_container)
{
  /* ------------------------ Top Toolbars ---------------------- */
  GtkWidget *Add_Toolbar;
  GtkWidget *Page_Toolbar;
  GtkWidget *Select_Toolbar;
  GtkWidget *Standard_Toolbar;
  GtkWidget *Zoom_Toolbar;

  /* --------------- Initialize Module Level Globals ------------ */
  ToolBarWidgets *bar_widgets;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

#if DEBUG_TOOLBARS
  fprintf(stderr, "init_top entry\n");
#endif

  /* ---------------------- Create Top ToolBox ------------------ */
  GtkWidget *toolbox_DT = geda_dock_box_new (GTK_ORIENTATION_HORIZONTAL);

  GEDA_PACK_TOOLBOX (parent_container, toolbox_DT)

  /* --------- Create and Populate the Standard Toolbar -------- */

  /* Standard Toolbar*/
  w_current->standard_handlebox = x_toolbars_get_box_container(w_current);

  GEDA_PACK_DOCKBOX(toolbox_DT, w_current->standard_handlebox, 0);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5 pixel spaced between items and put it into our handlebox */

  Standard_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  geda_set_container_border_width (Standard_Toolbar, 0);
  geda_container_add (w_current->standard_handlebox, Standard_Toolbar);

  /* Add New, Open, Save and Save As Buttons to the Standard Toolbar */
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_new);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_open);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_save);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_save_as);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_close);

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  /* Add Print and Export PDF Buttons to the Standard Toolbar */
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_print);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_write_pdf);

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  GSCHEM_TOOLBAR_BUTTON (Standard, etb_cut);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_copy);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_paste);

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  GSCHEM_TOOLBAR_BUTTON (Standard, etb_undo);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_redo);
  GSCHEM_TOOLBAR_BUTTON (Standard, etb_configure);

  gtk_widget_show (Standard_Toolbar);

  SET_TOOLBAR_ID  (w_current->standard_handlebox, tb_Standard);
  SET_TOOLBAR_WC  (w_current->standard_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->standard_handlebox, Standard_Toolbar);

  CAN_PASTE_LIST  = g_slist_append ( CAN_PASTE_LIST,   TB_BUTTON (etb_paste));
  ANY_OBJECT_LIST = g_slist_append ( ANY_OBJECT_LIST,  TB_BUTTON (etb_cut  ));
  ANY_OBJECT_LIST = g_slist_append ( ANY_OBJECT_LIST,  TB_BUTTON (etb_copy ));
  CAN_UNDO_LIST   = g_slist_append ( CAN_UNDO_LIST,    TB_BUTTON (etb_undo ));
  CAN_REDO_LIST   = g_slist_append ( CAN_REDO_LIST,    TB_BUTTON (etb_redo ));

  /* ----------- Create and Populate the Page Toolbar ----------- */

  w_current->page_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX(toolbox_DT, w_current->page_handlebox, 0);

  Page_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  geda_set_container_border_width (Page_Toolbar, 0);
  geda_container_add (w_current->page_handlebox, Page_Toolbar);

  GSCHEM_TOOLBAR_BUTTON (Page, etb_page_manager);

  GSCHEM_TOOLBAR_BUTTON (Page, etb_first_page);

  GSCHEM_TOOLBAR_BUTTON (Page, etb_up_page);
  GSCHEM_TOOLBAR_BUTTON (Page, etb_down_page);

  GSCHEM_TOOLBAR_BUTTON (Page, etb_prev_page);
  GSCHEM_TOOLBAR_BUTTON (Page, etb_next_page);
  GSCHEM_TOOLBAR_BUTTON (Page, etb_last_page);

  GSCHEM_TOOLBAR_BUTTON (Page, etb_new_page);

  gtk_toolbar_append_space (GTK_TOOLBAR(Page_Toolbar));

  GSCHEM_TOOLBAR_BUTTON (Page, etb_down_symbol);
  GSCHEM_TOOLBAR_BUTTON (Page, etb_down_schematic);
  GSCHEM_TOOLBAR_BUTTON (Page, etb_hierarchy_up);
  GSCHEM_TOOLBAR_BUTTON (Page, etb_view_document);

  HAVE_PAGES_LIST   = g_slist_append ( HAVE_PAGES_LIST, TB_BUTTON( etb_first_page ));
  HAVE_PAGES_LIST   = g_slist_append ( HAVE_PAGES_LIST, TB_BUTTON( etb_up_page ));
  HAVE_PAGES_LIST   = g_slist_append ( HAVE_PAGES_LIST, TB_BUTTON( etb_down_page ));
  HAVE_PAGES_LIST   = g_slist_append ( HAVE_PAGES_LIST, TB_BUTTON( etb_prev_page ));
  HAVE_PAGES_LIST   = g_slist_append ( HAVE_PAGES_LIST, TB_BUTTON( etb_next_page ));
  HAVE_PAGES_LIST   = g_slist_append ( HAVE_PAGES_LIST, TB_BUTTON( etb_last_page ));

  HAVE_COMPLEX_LIST = g_slist_append ( HAVE_COMPLEX_LIST, TB_BUTTON( etb_down_schematic));
  HAVE_COMPLEX_LIST = g_slist_append ( HAVE_COMPLEX_LIST, TB_BUTTON( etb_down_symbol   ));
  HAVE_PAGES_LIST   = g_slist_append ( HAVE_PAGES_LIST,   TB_BUTTON( etb_hierarchy_up  ));
  HAVE_COMPLEX_LIST = g_slist_append ( HAVE_COMPLEX_LIST, TB_BUTTON( etb_view_document ));

  gtk_widget_show (Page_Toolbar);

  SET_TOOLBAR_ID       (w_current->page_handlebox, tb_Page);
  SET_TOOLBAR_WC       (w_current->page_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->page_handlebox, Page_Toolbar);

#if DEBUG_TOOLBARS
  fprintf(stderr, "what happen to [%s]\n", GAF_SEE_NOTES_BITMAP ); /* can fill in missing icon */
#endif

  /* Start Second Toolbar Row */

  /* --------- Create and Populate the Add Toolbar -------- */

  w_current->add_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX(toolbox_DT, w_current->add_handlebox, 1);

  Add_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  geda_set_container_border_width (Add_Toolbar, 0);
  geda_container_add (w_current->add_handlebox, Add_Toolbar);

  /* not part of any radio button group */
  GSCHEM_TOOLBAR_BUTTON (Add, etb_add_attribute);
  GSCHEM_TOOLBAR_BUTTON (Add, etb_add_text);

  gtk_toolbar_append_space (GTK_TOOLBAR(Add_Toolbar));

  /* Toolbar radio button group - ToolBar_Radio_Responder defines a callback so ver 1 is expanded here*/
  /*                    bar, var,              grp,              name,            data */
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(pic),    NULL,             etb_add_pic,     w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(line),   BarRadio(pic),    etb_add_line,    w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(path),   BarRadio(line),   etb_add_path,    w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(arc),    BarRadio(path),   etb_add_arc,     w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(box),    BarRadio(arc),    etb_add_box,     w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(circle), BarRadio(box),    etb_add_circle,  w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(pin),    BarRadio(circle), etb_add_pin,     w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(bus),    BarRadio(pin),    etb_add_bus,     w_current)
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(net),    BarRadio(bus),    etb_add_net,     w_current)

  GSCHEM_TOOLBAR_BUTTON (Add, etb_add_component);

  gtk_toolbar_append_space (GTK_TOOLBAR(Add_Toolbar));

  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(select),   BarRadio(net),    etb_selector,   w_current)
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(deselect), BarRadio(net),    etb_deselector, w_current)

  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(none),     BarRadio(select), etb_none, w_current)

  /* Append all Toolbar "Mode" radio widgets to a GSlist, add a record in the struct
   * ToolBarWidgets, see function x_toolbars_update */
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(pic));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(line));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(path));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(arc));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(box));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(circle));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(pin));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(bus));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(net));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(select));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(deselect));
  TOOLBAR_RADIOS  = g_slist_append ( TOOLBAR_RADIOS, BarRadio(none));

  gtk_widget_show (Add_Toolbar);

  SET_TOOLBAR_ID       (w_current->add_handlebox, tb_Add);
  SET_TOOLBAR_WC       (w_current->add_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->add_handlebox, Add_Toolbar);

  /* ------ Create and Populate the Selection Toolbar ------ */

  /* Select Toolbar*/
  w_current->select_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX(toolbox_DT, w_current->select_handlebox, 1);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and put it into our handlebox */

  Select_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  geda_set_container_border_width (Select_Toolbar, 0);
  geda_container_add (w_current->select_handlebox, Select_Toolbar);

  GSCHEM_TOOLBAR_BUTTON (Select, etb_unselect_all);
  GSCHEM_TOOLBAR_BUTTON (Select, etb_select_all);
  GSCHEM_TOOLBAR_BUTTON (Select, etb_select_invert);

  gtk_widget_show (Select_Toolbar);

  SET_TOOLBAR_ID       (w_current->select_handlebox, tb_Select);
  SET_TOOLBAR_WC       (w_current->select_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->select_handlebox, Select_Toolbar);

  /* --------- Create and Populate the Zoom Toolbar -------- */

  w_current->zoom_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX(toolbox_DT, w_current->zoom_handlebox, 1);

  Zoom_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  geda_set_container_border_width (Zoom_Toolbar, 0);
  geda_container_add              (w_current->zoom_handlebox, Zoom_Toolbar);

  GSCHEM_TOOLBAR_BUTTON (Zoom, etb_view_redraw);
  GSCHEM_TOOLBAR_BUTTON (Zoom, etb_zoom_pan);
  GSCHEM_TOOLBAR_BUTTON (Zoom, etb_zoom_box);
  GSCHEM_TOOLBAR_BUTTON (Zoom, etb_zoom_select);
  GSCHEM_TOOLBAR_BUTTON (Zoom, etb_zoom_extents);
  GSCHEM_TOOLBAR_BUTTON (Zoom, etb_zoom_in);
  GSCHEM_TOOLBAR_BUTTON (Zoom, etb_zoom_out);
  GSCHEM_TOOLBAR_BUTTON (Zoom, etb_zoom_all);

  gtk_widget_show (Zoom_Toolbar);

  SET_TOOLBAR_ID        (w_current->zoom_handlebox, tb_Zoom);
  SET_TOOLBAR_WC        (w_current->zoom_handlebox, w_current);
  x_toolbars_add_closer (w_current, w_current->zoom_handlebox, Zoom_Toolbar);

  TheToolBars = g_slist_append (TheToolBars, Add_Toolbar);
  TheToolBars = g_slist_append (TheToolBars, Page_Toolbar);
  TheToolBars = g_slist_append (TheToolBars, Select_Toolbar);
  TheToolBars = g_slist_append (TheToolBars, Standard_Toolbar);
  TheToolBars = g_slist_append (TheToolBars, Zoom_Toolbar);

#if DEBUG_TOOLBARS
   fprintf(stderr, "init_top exit\n");
#endif
}

/*!
 * \brief Initialize Toolbar at the Left of the Main Window
 * \par Function Description
 * This function creates handleboxes, toolbars, toolbar buttons and
 * related containers in order to create the Toolbar at the Left
 * of the Main Window.
 *
 * \param [in] w_current  Gschem toplevel structure
 * \param [in] parent_container is the center_hbox widget
 */
void
x_toolbars_init_left(GschemToplevel *w_current, GtkWidget *parent_container)
{
  GtkWidget      *Edit_Toolbar;
  GtkWidget      *Modify_Toolbar;
  GtkWidget      *Symbol_Toolbar;
  ToolBarWidgets *bar_widgets;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  GtkWidget *toolbox_DL = geda_dock_box_new (GTK_ORIENTATION_VERTICAL);
  GEDA_PACK_TOOLBOX (parent_container, toolbox_DL);

  /* --------- Create and Populate the Symbol Toolbar -------- */

  w_current->symbol_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX(toolbox_DL, w_current->symbol_handlebox, 0);

  Symbol_Toolbar = geda_toolbar_new (GTK_ORIENTATION_VERTICAL);

  geda_set_container_border_width (Symbol_Toolbar, 0);

  if (w_current->handleboxes) {
    geda_handle_widget_set_toolbar(w_current->symbol_handlebox, Symbol_Toolbar);
    geda_handle_widget_set_handle_position(w_current->symbol_handlebox, GTK_POS_TOP);
  }

  GSCHEM_TOOLBAR_BUTTON (Symbol, etb_edit_prop);
  GSCHEM_TOOLBAR_BUTTON (Symbol, etb_translate);
  GSCHEM_TOOLBAR_BUTTON (Symbol, etb_lock);
  GSCHEM_TOOLBAR_BUTTON (Symbol, etb_unlock);
  GSCHEM_TOOLBAR_BUTTON (Symbol, etb_update);

  gtk_toolbar_append_space (GTK_TOOLBAR(Symbol_Toolbar));

  GSCHEM_TOOLBAR_BUTTON (Symbol, etb_edit_pin);

  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_edit_prop ));
  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_lock   ));
  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_unlock ));
  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_update ));

  HAVE_PIN_LIST   = g_slist_append (HAVE_PIN_LIST,   TB_BUTTON ( etb_edit_pin  ));

  gtk_widget_show (Symbol_Toolbar);

  SET_TOOLBAR_ID  (w_current->symbol_handlebox, tb_Symbol);
  SET_TOOLBAR_WC  (w_current->symbol_handlebox, w_current);
  x_toolbars_add_closer (w_current, w_current->symbol_handlebox, Symbol_Toolbar);

  TheToolBars = g_slist_append (TheToolBars, Symbol_Toolbar);

  /* --------- Create and Populate the Modify Toolbar -------- */

  w_current->modify_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX (toolbox_DL, w_current->modify_handlebox, 0);

  Modify_Toolbar = geda_toolbar_new (GTK_ORIENTATION_VERTICAL);

  geda_set_container_border_width (Modify_Toolbar, 0);

  if (w_current->handleboxes) {
    geda_handle_widget_set_toolbar (w_current->modify_handlebox, Modify_Toolbar);
    geda_handle_widget_set_handle_position (w_current->modify_handlebox, GTK_POS_TOP);
  }

  GSCHEM_TOOLBAR_BUTTON (Modify, etb_edit_butes);
  GSCHEM_TOOLBAR_BUTTON (Modify, etb_edit_text);
  GSCHEM_TOOLBAR_BUTTON (Modify, etb_edit_color);

  gtk_toolbar_append_space (GTK_TOOLBAR(Modify_Toolbar));

  GSCHEM_TOOLBAR_BUTTON (Modify, etb_edit_slot);
  GSCHEM_TOOLBAR_BUTTON (Modify, etb_edit_line);
  GSCHEM_TOOLBAR_BUTTON (Modify, etb_edit_fill);
  GSCHEM_TOOLBAR_BUTTON (Modify, etb_edit_arc);

  ANY_OBJECT_LIST   = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_edit_butes ));
  ANY_OBJECT_LIST   = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_edit_color ));

  TEXT_OBJECT_LIST  = g_slist_append (TEXT_OBJECT_LIST,  TB_BUTTON ( etb_edit_text ));
  HAVE_COMPLEX_LIST = g_slist_append (HAVE_COMPLEX_LIST, TB_BUTTON ( etb_edit_slot ));

  CAN_ELINE_LIST    = g_slist_append (CAN_ELINE_LIST,  TB_BUTTON ( etb_edit_line ));
  CAN_HATCH_LIST    = g_slist_append (CAN_HATCH_LIST,  TB_BUTTON ( etb_edit_fill ));
  ANY_OBJECT_LIST   = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_edit_arc  ));

  gtk_widget_show (Modify_Toolbar);

  SET_TOOLBAR_ID  (w_current->modify_handlebox, tb_Modify);
  SET_TOOLBAR_WC  (w_current->modify_handlebox, w_current);
  x_toolbars_add_closer (w_current, w_current->modify_handlebox, Modify_Toolbar);

  TheToolBars = g_slist_append (TheToolBars, Modify_Toolbar);

  /* --------- Create and Populate the Edit Toolbar -------- */

  w_current->edit_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX(toolbox_DL, w_current->edit_handlebox, 1);

  Edit_Toolbar = geda_toolbar_new (GTK_ORIENTATION_VERTICAL);

  geda_set_container_border_width (Edit_Toolbar, 0);

  if (w_current->handleboxes) {
    geda_handle_widget_set_toolbar(w_current->edit_handlebox, Edit_Toolbar);
    geda_handle_widget_set_handle_position(w_current->edit_handlebox, GTK_POS_TOP);
  }

  GSCHEM_TOOLBAR_BUTTON (Edit, etb_edit_copy);
  GSCHEM_TOOLBAR_BUTTON (Edit, etb_multi_copy);
  GSCHEM_TOOLBAR_BUTTON (Edit, etb_move);
  GSCHEM_TOOLBAR_BUTTON (Edit, etb_mirror);
  GSCHEM_TOOLBAR_BUTTON (Edit, etb_rotate);
  GSCHEM_TOOLBAR_BUTTON (Edit, etb_edit_array);

  //gtk_toolbar_append_space (GTK_TOOLBAR(Edit_Toolbar));

  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_edit_copy  ));
  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_multi_copy ));
  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_move   ));
  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_mirror ));
  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_rotate ));
  ANY_OBJECT_LIST = g_slist_append (ANY_OBJECT_LIST, TB_BUTTON ( etb_edit_array ));

  gtk_widget_show (Edit_Toolbar);

  SET_TOOLBAR_ID (w_current->edit_handlebox, tb_Edit);
  SET_TOOLBAR_WC (w_current->edit_handlebox, w_current);

  x_toolbars_add_closer (w_current, w_current->edit_handlebox, Edit_Toolbar);

  TheToolBars = g_slist_append (TheToolBars, Edit_Toolbar);
}

/*!
 * \brief Initialize Toolbar at the Bottom of the Main Window
 * \par Function Description
 *  This function creates handleboxes, toolbars, toolbar buttons and
 *  related containers in order to create the Toolbar at the Bottom
 *  of the Main Window.
 *
 * \param [in] w_current        Gschem toplevel structure
 * \param [in] parent_container is the main_box widget (same as the top bar)
 */
void
x_toolbars_init_bottom(GschemToplevel *w_current, GtkWidget *parent_container)
{

  GtkWidget      *Attribute_Toolbar;
  GtkWidget      *GripSnap_Toolbar;
  ToolBarWidgets *bar_widgets;

  /* Each toolbar created MUST be added to this single-link list: */
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  /* Start Bottom Toolbar Row */
  GtkWidget *toolbox_DB = geda_dock_box_new (GTK_ORIENTATION_HORIZONTAL);
  GEDA_PACK_TOOLBOX (parent_container, toolbox_DB);

  /* --------- Create and Populate the Attribute Toolbar -------- */

  w_current->attribute_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX(toolbox_DB, w_current->attribute_handlebox, 0);

  Attribute_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  geda_set_container_border_width (Attribute_Toolbar, 0);
  geda_container_add (w_current->attribute_handlebox, Attribute_Toolbar);

  /* Add Attribute Button to Toolbar */
  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_attach);
  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_detach);
  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_show_value);
  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_show_name);
  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_show_both);

  gtk_toolbar_append_space (GTK_TOOLBAR(Attribute_Toolbar));

  GSCHEM_TOOLBAR_BUTTON (Attribute, etb_visibilty);
  GSCHEM_TOOLBAR_BUTTON (Attribute, etb_show_hidden);
  GSCHEM_TOOLBAR_BUTTON (Attribute, etb_view_nets);
  GSCHEM_TOOLBAR_BUTTON (Attribute, etb_show_inherited);

  gtk_toolbar_append_space (GTK_TOOLBAR(Attribute_Toolbar));

  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_find_text);
  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_hide_text);
  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_show_specific);
  GSCHEM_TOOLBAR_BUTTON(Attribute, etb_auto_number);

  TEXT_OBJECT_LIST = g_slist_append ( TEXT_OBJECT_LIST, TB_BUTTON ( etb_attach     ));
  TEXT_OBJECT_LIST = g_slist_append ( TEXT_OBJECT_LIST, TB_BUTTON ( etb_detach     ));
  TEXT_OBJECT_LIST = g_slist_append ( TEXT_OBJECT_LIST, TB_BUTTON ( etb_show_value ));
  TEXT_OBJECT_LIST = g_slist_append ( TEXT_OBJECT_LIST, TB_BUTTON ( etb_show_name  ));
  TEXT_OBJECT_LIST = g_slist_append ( TEXT_OBJECT_LIST, TB_BUTTON ( etb_show_both  ));

  gtk_widget_show (Attribute_Toolbar);

  SET_TOOLBAR_ID       (w_current->attribute_handlebox, tb_Attribute);
  SET_TOOLBAR_WC       (w_current->attribute_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->attribute_handlebox, Attribute_Toolbar);

  TheToolBars = g_slist_append (TheToolBars, Attribute_Toolbar);

  /* -------- Create and Populate the GridSnap Toolbar -------- */

  w_current->grid_snap_handlebox = x_toolbars_get_box_container(w_current);
  GEDA_PACK_DOCKBOX(toolbox_DB, w_current->grid_snap_handlebox, 0);

  GripSnap_Toolbar = geda_toolbar_new (GTK_ORIENTATION_HORIZONTAL);

  geda_set_container_border_width (GripSnap_Toolbar, 0);
  geda_container_add (w_current->grid_snap_handlebox, GripSnap_Toolbar);

  /* Toolbar radio button group - ToolBar_Radio_Responder defines a callback so ver 1 is expanded here*/
  /*                    bar,      var,             grp,             name,            data */
  TOOLBAR_GSCHEM_RADIO( GripSnap, BarRadio(dot),   NULL,            etb_grid_dot,    w_current);
  TOOLBAR_GSCHEM_RADIO( GripSnap, BarRadio(mesh),  BarRadio(dot),   etb_grid_mesh,   w_current);
  TOOLBAR_GSCHEM_RADIO( GripSnap, BarRadio(off),   BarRadio(mesh),  etb_grid_off,    w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(GripSnap_Toolbar));

  GSCHEM_TOOLBAR_BUTTON(GripSnap, etb_snap_set);
  GSCHEM_TOOLBAR_BUTTON(GripSnap, etb_snap_up);
  GSCHEM_TOOLBAR_BUTTON(GripSnap, etb_snap_down);

  GSCHEM_TOOLBAR_BUTTON_FUNC(GripSnap, etb_snap_off, x_toolbars_snap_toggle);
  GSCHEM_TOOLBAR_BUTTON_FUNC(GripSnap, etb_snap_on, x_toolbars_snap_toggle);

  gtk_widget_show (GripSnap_Toolbar);

  GEDA_OBJECT_SET_DATA(etb_snap_off_button, etb_snap_on_button,  "snap-widget");
  GEDA_OBJECT_SET_DATA(etb_snap_on_button,  etb_snap_off_button, "snap-widget");

  if (w_current->snap == SNAP_OFF) {
    g_object_set (etb_snap_off_button, "visible", FALSE, NULL);
    //gtk_widget_hide(etb_snap_off_button);
  }
  else {
    g_object_set (etb_snap_on_button, "visible", FALSE, NULL);
    //gtk_widget_hide(etb_snap_on_button);
  }

  SET_TOOLBAR_ID       (w_current->grid_snap_handlebox, tb_Grid_Snap);
  SET_TOOLBAR_WC       (w_current->grid_snap_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->grid_snap_handlebox, GripSnap_Toolbar);

  TheToolBars = g_slist_append (TheToolBars, GripSnap_Toolbar);
}

/*!
 * \brief Set Sensitivity of Toolbar Buttons
 * \par Function Description
 *  This function is called by x_toolbars_set_sensitivities with a gslist
 *  of toolbar button widgets to be set to the specified sensitivity
 *
 * \param [in] ListToolBarItems SINGLE linked list of widgets
 * \param [in] sensitive        boolean TRUE = sensitive, FALSE = gray-out
 */
static void
x_toolbar_set_sensitivity(GSList *ListToolBarItems, int sensitive)
{
  lambda (GtkWidget *item)
  {
    if (GTK_IS_WIDGET(item)) {
      gtk_widget_set_sensitive(item, sensitive != FALSE);
      gtk_widget_set_has_tooltip (item, sensitive != FALSE);
    }
    else {
      fprintf(stderr, "x_toolbar_set_sensitivity, item is not a widget\n");
    }
    return FALSE;
  }
  mapcar(ListToolBarItems);
}

/*!
 * \brief Set Sensitivity of Toolbar Button Groups
 * \par Function Description
 *  This functions sets the sensitivities of toolbar button as a visual
 *  aid to the user. When the sensitivity is set to FALSE widgets are
 *  grayed-out to indicate they are not applicable to the current context.
 *  Note that this is TO purely aid the user, cosmetically the GUI "looks"
 *  better with color and nothing bad would happen even if a button was
 *  pressed when not applicable, gschem just ignores the signals.
 *
 * \param [in] w_current  Gschem toplevel object.
 * \param [in] mode is an enumerated group identifier (see in globals.h)
 * \param [in] state boolean TRUE = sensitive, FALSE = gray-out
 */
void
x_toolbars_set_sensitivities(GschemToplevel *w_current,
                             EID_SENITIVITY_MODE mode,
                             bool state)
{
  if (w_current->toolbars) {

    ToolBarWidgets *bar_widgets;

    bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

    switch (mode) {
      case ANY_OBJECT:     x_toolbar_set_sensitivity(ANY_OBJECT_LIST, state);
      break;
      case CAN_PASTE:      x_toolbar_set_sensitivity(CAN_PASTE_LIST, state);
      break;
      case CAN_UNDO:       x_toolbar_set_sensitivity(CAN_UNDO_LIST, state);
      break;
      case CAN_REDO:       x_toolbar_set_sensitivity(CAN_REDO_LIST, state);
      break;
      case CAN_HATCH:      x_toolbar_set_sensitivity(CAN_HATCH_LIST, state);
      break;
      case CAN_ELINE:      x_toolbar_set_sensitivity(CAN_ELINE_LIST, state);
      break;
      case COMPLEX_OBJECT: x_toolbar_set_sensitivity(HAVE_COMPLEX_LIST, state);
      break;
      case HAVE_PAGES:     x_toolbar_set_sensitivity(HAVE_PAGES_LIST, state);
      break;
      case HAVE_PIN:       x_toolbar_set_sensitivity(HAVE_PIN_LIST, state);
      break;
      case HAVE_TEXT:      x_toolbar_set_sensitivity(TEXT_OBJECT_LIST, state);
      break;
    }
  }
}

void
x_toolbars_set_show_tooltips (GschemToplevel *w_current, bool show_tips)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GedaToolbar *toolbar){
    geda_toolbar_set_tooltips (toolbar, show_tips);
    return FALSE;
  }
  mapcar(TheToolBars)

  w_current->show_toolbar_tips = show_tips;
}

/*!
 * \brief View Toolbar Icons
 * \par Function Description
 *  This function set all of buttons on toolbars to display icons.
 *
 * \param [in] widget     Menu-item widget that involked the call.
 * \param [in] w_current  Gschem toplevel object.
 */
/* View->Toolbar */
void
x_toolbar_icons_only(GtkWidget *widget, GschemToplevel *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GedaToolbar *toolbar){
    geda_toolbar_set_style (toolbar, TOOLBAR_SHOW_ICONS);
    return FALSE;
  }
  mapcar(TheToolBars)

  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 1));
  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 2));
  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 3));
}

/*!
 * \brief View Toolbar Text
 * \par Function Description
 *  This function set all of buttons on toolbars to display text.
 *
 * \param [in] widget     Menu-item widget that involked the call.
 * \param [in] w_current  Gschem toplevel object.
 */
void
x_toolbar_text_only(GtkWidget *widget, GschemToplevel *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GedaToolbar* toolbar) {
    geda_toolbar_set_style (toolbar, TOOLBAR_SHOW_TEXT);
    return FALSE;
  }
  mapcar(TheToolBars)

  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 0));
  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 2));
  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 3));
}

/*!
 * \brief View Toolbar Icons & Text Stacked Vertically
 * \par Function Description
 *  This function set all of buttons on toolbars to display both
 *  icons and the text.
 *
 * \param [in] widget     Menu-item widget that involked the call.
 * \param [in] w_current  Gschem toplevel object.
 */
void
x_toolbar_display_both(GtkWidget *widget, GschemToplevel *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GedaToolbar* toolbar){
    geda_toolbar_set_style (toolbar, TOOLBAR_SHOW_BOTH);
    return FALSE;
  }
  mapcar(TheToolBars)

  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 0));
  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 1));
  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 3));
}

/*!
 * \brief View Toolbar Icons & Text Side by Side
 * \par Function Description
 *  Callback function for the View/Menu/Toolbars/Both Horizontal
 *  radio menu item.
 */
void
x_toolbar_display_horiz(GtkWidget *widget, GschemToplevel *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GedaToolbar* toolbar){
    geda_toolbar_set_style (toolbar, TOOLBAR_SHOW_HORIZ);
    return FALSE;
  }
  mapcar(TheToolBars)

  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 0));
  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 1));
  x_toolbars_turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 2));
}

/*!
 * \brief Set All Toolbar Radios inactive
 * \par Function Description
 *  This function completes the final configuration of the toolbar setup.
 *  The hidden toolbar_none radio button (etb_none) is set active, which
 *  deactivates all of the visible buttons.
 *
 * \param [in] w_current pointer to top-level data structure
 */
void
x_toolbars_turn_off_all_radios (GschemToplevel *w_current )
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);
  SetToggleState(bar_widgets->toolbar_none, TRUE);
}

void
x_toolbars_activate_select (GschemToplevel *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);
  SetToggleState(bar_widgets->toolbar_none, TRUE);
}

/*!
 * \brief Set The Grid Radio
 * \par Function Description
 *  This function should be called after construction of the main
 *  window to insure the correct button is pressed in for the grid
 *  _mode or anytime the grid mode is changed programmatically.
 *
 * \param [in] w_current pointer to top-level data structure
 */
void
x_toolbars_set_grid_radio (GschemToplevel *w_current)
{
  if (w_current->toolbars) {

    ToolBarWidgets  *bar_widgets;
    GtkToggleButton *target = NULL;

    bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

    switch(w_current->grid_mode) {
      case(GRID_NONE):
        target = (GtkToggleButton*) bar_widgets->toolbar_off;
        break;
      case(GRID_DOTS):
        target = (GtkToggleButton*) bar_widgets->toolbar_dot;
        break;
      case(GRID_MESH):
        target = (GtkToggleButton*) bar_widgets->toolbar_mesh;
        break;
      default:
        break;
    }

    if (GTK_IS_TOGGLE_BUTTON(target)) {
      /* if button is not active then action was not initiated by the toolbar */
      if (!target->active) {
        g_signal_handlers_block_by_func (target, x_toolbars_execute_radio, w_current);
        SetToggleState (target, TRUE);
        g_signal_handlers_unblock_by_func (target, x_toolbars_execute_radio, w_current);
      }
    }
  }
}

#define HideFromDoxygen x_toolbars_execute_radio

/*!
 * \brief Update Toolbar Radio Buttons based on the current state
 *  This function sets the state of the "mode" radio buttons on the Add
 *  tool-bar. This is done to synchronize the tool-bars with the rest of
 *  the interface since the "mode" can be set by other means. For example
 *  if the Add/Line menu option is chosen then the Line mode button needs
 *  to be depressed, without emitting a signal. Where as if the Edit/Arc
 *  menu option is selected then the invisible "none" mode button should
 *  be "activated". And so forth.
 *
 * \note
 *    1.  Pointer to the Radio widgets are members of ToolBarWidgets
 *
 *    2.  This does not set the sensitivities of regular buttons, that is
 *        done by x_toolbars_set_sensitivities.
 *
 * \param [in] w_current GschemToplevel structure
 */
void
x_toolbars_update(GschemToplevel *w_current)
{
  if (!w_current->toolbars) return; /* if toolbars are disabled exit */

  ToolBarWidgets  *bar_widgets;
  GtkToggleButton *target;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);
  target = NULL;

  switch(w_current->event_state) {
    case(NONE):
    case(SELECT):
    case(STARTSELECT):
      target = (GtkToggleButton*) bar_widgets->toolbar_select;
      break;
    case(DESELECT):
    case(STARTDESELECT):
      target = (GtkToggleButton*) bar_widgets->toolbar_deselect;
      break;
    case(NETMODE):
      target = (GtkToggleButton*) bar_widgets->toolbar_net;
      break;
    case(BUSMODE):
      target = (GtkToggleButton*) bar_widgets->toolbar_bus;
      break;
    case(LINEMODE):
      target = (GtkToggleButton*) bar_widgets->toolbar_line;
      break;
    case(BOXMODE):
      target = (GtkToggleButton*) bar_widgets->toolbar_box;
      break;
    case(PATHMODE):
      target = (GtkToggleButton*) bar_widgets->toolbar_path;
      break;
    case(PICTUREMODE): /* \Launches Dialog */
      target = (GtkToggleButton*) bar_widgets->toolbar_pic;
      break;
    case(PINMODE):
      target = (GtkToggleButton*) bar_widgets->toolbar_pin;
      break;
    case(CIRCLEMODE):
      target = (GtkToggleButton*) bar_widgets->toolbar_circle;
      break;
    case(ARCMODE):
      target = (GtkToggleButton*) bar_widgets->toolbar_arc;
      break;
    default:
      target = (GtkToggleButton*)bar_widgets->toolbar_none;
      break;
  }

  if (GTK_IS_TOGGLE_BUTTON(target)) {
    /* if button is not active then action was not initiated by the toolbar */
    if (!target->active) {
      g_signal_handlers_block_by_func (target, HideFromDoxygen, w_current);
      SetToggleState (target, TRUE);
      g_signal_handlers_unblock_by_func (target, HideFromDoxygen, w_current);
    }
  }
}

/** @} endgroup toolbars-module */
