/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2013 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include "config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <unistd.h>
#include <sys/stat.h>
#include <gtk/gtk.h>

#include <gschem.h>           /* include gschem specific headers  */
#include <x_menu.h>

#define DEBUG_TOOLBARS 0

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define TOOLBAR_STYLE w_current->toolbars_mode           /* per window style variable  */
#define DEFAULT_TOOLBAR_STYLE GTK_TOOLBAR_ICONS          /* default style */
#define TheToolBars  bar_widgets->toolbar_slist          /* convenience macro */

#define TOOLBAR_RADIO_VARIABLE(symbol) bar_widgets->toolbar_##symbol
#define BarRadio(...) TOOLBAR_RADIO_VARIABLE(__VA_ARGS__)

#define ToolBar_Radio_Responder x_toolbars_execute_radio

#include <widgets/geda_handlebox.h>
#include <geda_toolbars.h>
#include <x_toolbars.h>
#include <i_actions.h>

/* convenience macros */
#define CAN_PASTE_LIST       bar_widgets->can_paste
#define CAN_UNDO_LIST        bar_widgets->can_undo
#define CAN_REDO_LIST        bar_widgets->can_redo
#define HAVE_PAGES_LIST      bar_widgets->have_pages
#define COMPLEX_OBJECTS_LIST bar_widgets->complex_object
#define SOME_OBJECTS_LIST    bar_widgets->some_object    /* List of widgets on toolbars to set if some object is selected */
#define TEXT_OBJECTS_LIST    bar_widgets->text_object

#define TOOLBAR_RADIOS bar_widgets->toolbar_radio_list   /* Single list of toolbar radios */

static GSList    *ui_list = NULL;

static GtkWidget *popup_menu;

static ToolBarInfo ActiveToolBar;

/*! \note #1: These numerators are used to access the structure of strings
 *            ToolbarStrings, i.e. if an item is added to one then an item
 *            must be added to the cooresponding position in the other.
 * 
 *  \note #2: etb_none is a dummy member used in the add mode radio group.
 *         The group is a collection of radio button and when don't want
 *         any of them selected then we set etb_none to be the active
 *         radio button.
 */
typedef enum  { etb_new, etb_open, etb_save, etb_save_as, etb_close,
                etb_print, etb_write_pdf, etb_cut, etb_copy, etb_paste,
                etb_undo, etb_redo, etb_configure, etb_selector, etb_deselector,
                etb_deselect_all, etb_none, etb_select_all, etb_select_invert,
                etb_add_component, etb_add_net, etb_add_bus, etb_add_attribute,
                etb_add_text, etb_add_line, etb_add_box, etb_add_circle,
                etb_add_arc, etb_add_path, etb_add_pin, etb_insert_pic,
                etb_prev_page, etb_next_page, etb_new_page, etb_page_manager,
                etb_down_schematic, etb_down_symbol, etb_hierarchy_up,
                etb_view_document, etb_view_redraw, etb_zoom_pan, etb_zoom_box,
                etb_zoom_select, etb_zoom_extents, etb_zoom_in, etb_zoom_out,
                etb_zoom_all,
                etb_edit_copy, etb_multi_copy, etb_move, etb_rotate, etb_mirror,
                etb_edit_butes, etb_edit_color, etb_edit_text, etb_edit_slot,
                etb_edit_pin, etb_edit_line, etb_edit_fill, etb_edit_arc,
                etb_translate, etb_lock, etb_unlock, etb_update,
                etb_attach, etb_detach, etb_show_value, etb_show_name,
                etb_show_both, etb_visibilty, etb_show_hidden, etb_find_text,
                etb_hide_text, etb_show_specific, etb_auto_number,
                etb_grid_dot, etb_grid_mesh, etb_grid_off,
                etb_snap_up, etb_snap_down, etb_snap_set, etb_snap_off,
                etb_snap_on,
} IDE_GSCHEM_Toolbar;

/* Important: See IDS_Menu_Toolbar_Toggles in x_menu.c */
const char* IDS_Toolbar_Names[] = {  /* ToolBar Name Strings */
  "add-bar", "Attribute", "Edit", "GridSnap", "Page", "Select", "Standard", "Zoom",
  NULL
};

static ToolbarStringData ToolbarStrings[] = {
   /* Standard Toolbar*/
  { ACTION(FILE_NEW),           "New",        "Create a new file", GSCHEM_MAP(NEW)},
  { ACTION(FILE_OPEN),          "Open",       "Open file",         GSCHEM_MAP(OPEN)},
  { ACTION(FILE_SAVE),          "Save",       "Save file",         GSCHEM_MAP(SAVE)},
  { ACTION(FILE_SAVE_AS),       "Save As",    "Save the file to different name or location", "Private"},
  { ACTION(FILE_CLOSE),         "Close",      "Close the current file", GSCHEM_MAP(PROJECT_CLOSE)},

  { ACTION(FILE_PRINT),         "Print",      "Open the Print Dialog",  "Private"}, /* Most do a ZAP print from bar */
  { ACTION(FILE_WRITE_PDF),     "Write PDF",  "Create PDF document", "Private"},

  { ACTION(EDIT_CB_CUT),        "Cut",        "Cut selection to the clipboard",     "Private"},
  { ACTION(EDIT_CB_COPY),       "Copy",       "Copy selection to the clipboard",    "Private"},
  { ACTION(EDIT_CB_PASTE),      "Paste",      "Paste selection from the clipboard", "Private"},

  { ACTION(EDIT_UNDO),          "Undo",       "Undo the last operation",       GSCHEM_MAP(UNDO)},
  { ACTION(EDIT_REDO),          "Redo",       "Redo the last undo",            GSCHEM_MAP(REDO)},
  { ACTION(OPT_SETTINGS),       "Config",     "Set configuration preferences", GEDA_MAP(TOOLS)},

  { ACTION(EDIT_SELECT),       "Select",      "Activate Select mode",               "gschem-select"},
  { ACTION(EDIT_DESELECT),     "Deselect",    "Activate UnSelect mode",             "gschem-unselect"},
  { ACTION(EDIT_DESELECT_ALL), "Deselect All","Unselect everything",                "gschem-unselect"},

  { "nil"              ,        "nil",        "nil",          "gtk-no"}, /* dummy corresponding to etb_none */

  { ACTION(EDIT_SELECT_ALL),    "Select All", "Select all objects in the database",  "gschem-select-all"},
  { ACTION(EDIT_INVERT),        "Invert Sel", "Invert the current selection set",    "gschem-invert"},

  { ACTION(ADD_COMPONENT),      "Component",  "Add Component...\nSelect library and component from list, move the mouse into main window, click to place\nRight mouse button to cancel", "Private"},
  { ACTION(ADD_NET),            "Nets",       "Add Nets mode\nRight mouse button to cancel",  "geda-net"},
  { ACTION(ADD_BUS),            "Bus",        "Add Buses mode\nRight mouse button to cancel", "geda-bus"},
  { ACTION(ADD_ATTRIB),         "Attrib",     "Add Attribute...", "Private"},
  { ACTION(ADD_TEXT),           "Text",       "Add Text...",      "Private"},

  /* Add Toolbar */
  { ACTION(ADD_LINE),           "Line",       "Add line",        "geda-line"},
  { ACTION(ADD_BOX),            "Box",        "Add Box",         "geda-box"},
  { ACTION(ADD_CIRCLE),         "circle",     "Add Circle",      "geda-circle"},
  { ACTION(ADD_ARC),            "Arc",        "Add Arc",         "geda-arc"},
  { ACTION(ADD_PATH),           "Path",       "Add Path",        "geda-path"},
  { ACTION(ADD_PIN),            "Pin",        "Add Pin",         "geda-pin"},
  { ACTION(ADD_PICTURE),        "Picture",    "Insert an image", "Private"},

  /* Page Toolbar */
  { ACTION(PAGE_PREV),          "Prev",       "Switch to the previous page", "Private"},
  { ACTION(PAGE_NEXT),          "next",       "Switch to the next page", "Private"},
  { ACTION(PAGE_NEW),           "New",        "Create a new page", "Private"},
  { ACTION(PAGE_MANAGER),       "Manage",     "Open the Page Manager", "Private"},

  { ACTION(DOWN_SCHEMATIC),     "Down",       "Lower schematic hierarchy", "Private"},
  { ACTION(DOWN_SYMBOL),        "Down",       "Lower symbol hierarchy", "Private"},
  { ACTION(HIERARCHY_UP),       "Up",         "Elevate hierarchy", "Private"},

  { ACTION(VIEW_DOCUMENT),      "Spec",       "View component documentation", "Private"},

  /* Zoom Toolbar */
  { ACTION(VIEW_REDRAW),        "Redraw",     "Redraw current display", GEDA_MAP(VIEW_REDRAW)},
  { ACTION(VIEW_PAN),           "Pan",        "Zoom Pan",               GEDA_MAP(ZOOM_PAN)},
  { ACTION(VIEW_BOX),           "Window",     "Zoom Window",            GEDA_MAP(ZOOM_BOX)},
  { ACTION(VIEW_SELECTED),      "Selected",   "Zoom to selection",      "gtk-zoom-fit"},
  { ACTION(VIEW_EXTENTS),       "Extents",    "Zoom to extents",        GEDA_MAP(ZOOM_EXTENTS)},
  { ACTION(VIEW_ZOOM_IN),       "In",         "Zoom In",                GEDA_MAP(ZOOM_IN)},
  { ACTION(VIEW_ZOOM_OUT),      "Out",        "Zoom Out",               GEDA_MAP(ZOOM_OUT)},
  { ACTION(VIEW_ZOOM_ALL),      "All",        "Zoom to Limits",         GEDA_MAP(ZOOM_LIMITS)},

  /* Edit Toolbar */
  { ACTION(EDIT_COPY),          "Copy",       "Copy selection",                    "Private"},
  { ACTION(EDIT_MCOPY),         "Multi",      "Make multible copies of selection", "Private"},
  { ACTION(EDIT_MOVE),          "Move",       "Move objects",                      "Private"},
  { ACTION(EDIT_ROTATE),        "Rotate",     "Rotate objects",                    "Private"},
  { ACTION(EDIT_MIRROR),        "Mirror",     "Mirror objects",                    "Private"},

  { ACTION(EDIT_ATTRIB),        "Attributes", "Edit Attribute properties",         "Private"},
  { ACTION(EDIT_COLOR),         "Color",      "Change Colors",                     "Private"},

  { ACTION(EDIT_TEXT),          "Text",       "Edit text properties",             "Private"},
  { ACTION(EDIT_SLOT),          "Slots",      "Edit Slot number of complex",    "geda-slot"},
  { ACTION(EDIT_PIN),           "Pins",       "Open the Pin Editor",        "geda-pin-type"},
  { ACTION(EDIT_LINE),          "Line",       "Edit line type",                   "Private"},
  { ACTION(EDIT_FILL),          "Fill",       "Edit Hatch pattern",          GEDA_MAP(MESH)},
  { ACTION(EDIT_ARC),           "Arcs",       "Edit Arc parameters",        "geda-arc-edit"},

  { ACTION(EDIT_TRANSLATE),     "Translate",  "Translate component positions", GEDA_MAP(TRANSLATE)},
  { ACTION(EDIT_LOCK),          "Lock",       "Lock Objects",   "Private"},
  { ACTION(EDIT_UNLOCK),        "Unlock",     "Unlock Objects", "Private"},
  { ACTION(EDIT_UPDATE),        "Update",     "Reload Component from library", "Private"},

  /* Attribute Toolbar */
  { ACTION(ATTRIB_ATTACH),      "Promote",    "Attach selected attribute", "Private"},
  { ACTION(ATTRIB_DETACH),      "Demote",     "Dettach selected attribute", "Private"},
  { ACTION(ATTRIB_VALUE),       "Value",      "Set selected value visible", GEDA_MAP(VALUE)},
  { ACTION(ATTRIB_NAME),        "Name",       "Set selected name visible"},
  { ACTION(ATTRIB_BOTH),        "Both",       "Set selected name and value visible", "Private"},
  { ACTION(ATTRIB_VISIBILITY),  "Visible",    "Toggle Visibilty", GEDA_MAP(EYE_GLASSES)},
  { ACTION(VIEW_HIDDEN),        "Hidden",     "Toggle hidden text attributes", "Private"},
  { ACTION(ATTRIB_FIND),        "Find",       "Find attribute", GEDA_MAP(FIND_ATTRIBUTE)},
  { ACTION(ATTRIB_HIDE),        "Hide",       "Hide selected attribute", "Private"},
  { ACTION(ATTRIB_SHOW),        "Show",       "Show a specific attribute value", "Private"},
  { ACTION(ATTRIB_AUTONUM),     "Auto #",     "Open Auto Number dialog", "Private"},

  { ACTION(OPT_GRID_DOT),       "Dots",       "Set the Grid Display to Dots Mode",  "geda-grid-dot"},
  { ACTION(OPT_GRID_MESH),      "Mesh",       "Set the Grid Display to Mesh Mode",  "geda-grid-mesh"},
  { ACTION(OPT_GRID_OFF),       "Off",        "Turn the display grid off",          "geda-display"},

  { ACTION(OPT_SNAP_UP),        "UP",         "Increase the Snap size",    "geda-snap"},
  { ACTION(OPT_SNAP_DOWN),      "Down",       "Descrease the Snap size",   "geda-snap"},
  { ACTION(OPT_SNAP_SIZE),      "Set",        "Open the Snap Size dialog", "geda-magnet"},
  { ACTION(OPT_SNAP_OFF),       "Snap Off",   "Turn Snap mode off",        "geda-snap-off"},
  { ACTION(OPT_SNAP_ON),        "Snap On",    "Turn Snap mode on",         "geda-snap-on"},

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

/*! \brief Creates a new Bitmap Image
 *
 * If for some reason our bitmap can not be found this function falls
 * back to the stock GTK icon bitmaps provided in the distribution.
 * This is the opposite of the predecessor function which used the stock
 * icon as the primary and our bitmap as the backup! This function is
 * used in combination with the TOOLBAR_xxx_BUTTON macros that use the
 * LOCAL_ALT option. When LOCAL_ALT options is used the primary icon is
 * the forth string in the ToolbarStrings, the secondary is will be GTK
 * _STK_ and the next macro parameter, for example:
 *
 * ex.:TOOLBAR_GEDA_BUTTON( Standard, new, LOCAL_ALT, NEW, callback, data);
 *                                     ^       ^       ^
 *                  enumerated index __|       |       |
 *                                             |       |
 *                         use this function __|       |
 *                                                     |
 *             if file not found then use GTK_STOCK_ __|
 *
 *
 * \param item Name of the stock icon ("new", "open", etc.)
 *
 */
static GtkWidget *get_stock_alt_pixmap(GSCHEM_TOPLEVEL *w_current, ToolbarItem* item )
{
  GtkWidget *wpixmap = NULL;
  GdkPixmap *pixmap;
  GdkBitmap *mask;

  GdkWindow *window=w_current->main_window->window;
  GdkColor  *background=&w_current->main_window->style->bg[GTK_STATE_NORMAL];

  char *filename=g_strconcat(w_current->toplevel->bitmap_directory,
                             G_DIR_SEPARATOR_S,
                             TB_ICON_NAME(item->ButtonId), NULL);

  /* 1ST Try custom icon */
  if(access(filename, R_OK) == 0) {
    pixmap = gdk_pixmap_create_from_xpm (window, &mask, background, filename);
    if (pixmap != NULL) {
      wpixmap = gtk_image_new_from_pixmap (pixmap, mask);
    }
  }
  if (wpixmap == NULL) { /* Try Falling back to Stock icon */
    wpixmap = gtk_image_new_from_stock(item->stock_id,
                                       GTK_ICON_SIZE_SMALL_TOOLBAR);
  }
  if (wpixmap == NULL) {
     s_log_message("get_stock_alt_pixmap: image file not found: \"%s\".\n", filename);
     wpixmap = gtk_image_new_from_stock(GTK_STOCK_MISSING_IMAGE ,
                                        GTK_ICON_SIZE_SMALL_TOOLBAR);
  }

  g_free(filename);

  return wpixmap;
}
/*! \brief Toolbar Button Callback
 *
 *  \par Function Description
 *  This function handles callbacks for all non-toogle type toolbar
 * buttons, the function retrieves the action from the button widget
 * and pass the action to i_command_process.
 */
static void x_toolbars_execute(GtkWidget* widget, GSCHEM_TOPLEVEL* w_current)
{
  char* action;

  action = g_object_get_data (G_OBJECT(widget), "action");
#if DEBUG_TOOLBARS
  fprintf(stderr, "x_toolbars_execute: action=%s\n",action);
#endif
  i_command_process(w_current, action, 0, NULL, ID_ORIGIN_TOOLBAR);
}
/*! \brief Toolbar Radio Button Callback
 *
 *  \par Function Description
 *  This function handles callbacks for radio toolbar buttons,
 * the function retrieves the action from the button widget
 * and pass the action to i_command_process.
 */
static void x_toolbars_execute_radio (GtkToggleButton *button, GSCHEM_TOPLEVEL* w_current)
{
  char* action;

  action = g_object_get_data (G_OBJECT(button), "action");

#if DEBUG_TOOLBARS
  fprintf(stderr, "x_toolbars_execute_radio: action=%s\n",action);
#endif

  if (strcmp(action, "none") != 0)
    if (button->active) {
      i_command_process(w_current, action, 0, NULL, ID_ORIGIN_TOOLBAR);
    }
}

static void x_toolbars_snap_toggle(GtkWidget* widget, GSCHEM_TOPLEVEL* w_current)
{
  char*      action;
  GtkWidget* button;

  action = g_object_get_data (G_OBJECT(widget), "action");
  button = g_object_get_data (G_OBJECT(widget), "snap-widget");

  g_object_set (widget, "visible",  FALSE, NULL);
  g_object_set (button, "visible", !FALSE, NULL);

#if DEBUG_TOOLBARS
  fprintf(stderr, "x_toolbars_execute: action=%s\n",action);
#endif
  i_command_process(w_current, action, 0, NULL, ID_ORIGIN_TOOLBAR);
}

/*! \brief Save Toolbar Configuration
 *
 *  \par Function Description
 *  This function saves the state of the Toolbar widgets so we
 *  can restore them to same states the next time we run.
 */
void x_toolbars_save_state(GSCHEM_TOPLEVEL *w_current) {

  char *data, *filename;
  GKeyFile    *key_file = NULL;

  void SaveBarProperties(GtkWidget * handlebox) {
    int   bar_id;
    int   visible;
    int   style;
    const char *group_name;

    bar_id     = GET_TOOLBAR_ID(handlebox);
    group_name = IDS_Toolbar_Names[bar_id];
    visible    = gtk_widget_get_visible(handlebox);

    g_key_file_set_integer (key_file, group_name, "visible", visible);
    style = gtk_toolbar_get_style (GTK_TOOLBAR (GTK_BIN (handlebox)->child));
    g_key_file_set_integer (key_file, group_name, "style", style);
    //g_key_file_set_integer (key_file, group_name, "x", x);
    //g_key_file_set_integer (key_file, group_name, "y", y);
  }

  void SaveAllBars() {
    SaveBarProperties(w_current->add_handlebox);
    SaveBarProperties(w_current->attribute_handlebox);
    SaveBarProperties(w_current->edit_handlebox);
    SaveBarProperties(w_current->grid_snap_handlebox);
    SaveBarProperties(w_current->page_handlebox);
    SaveBarProperties(w_current->select_handlebox);
    SaveBarProperties(w_current->standard_handlebox);
    SaveBarProperties(w_current->zoom_handlebox);
  }

  bool setup_new_keyfile (char *filename) {

    bool results = TRUE;

    key_file = g_key_file_new();

    if (access(filename, W_OK) != 0) {
      v_log_message("Creating new Toolbar configuration\n");
      mkdir (s_path_user_config (), S_IRWXU | S_IRWXG);
      g_file_set_contents (filename, "", -1, NULL);
    }
    if (!g_file_test (filename, G_FILE_TEST_EXISTS))
      results = FALSE;
    return results;
  }

  v_log_message("Saving Toolbar configuration\n");
  filename = g_build_filename(s_path_user_config (), TOOLBAR_GEOMETRY_STORE, NULL);

  if (!g_file_test (filename, G_FILE_TEST_EXISTS))
    setup_new_keyfile (filename);
  else
    key_file = g_key_file_new();

  if(key_file) {
    SaveAllBars();
    data = g_key_file_to_data(key_file, NULL, NULL);
    g_file_set_contents(filename, data, -1, NULL);
    g_free(data);
  }
  else
    g_warning("Warning, could not save Toolbar configuration to %s\n", filename);

  if(key_file) g_key_file_free(key_file);
  g_free(filename);
}

/*! \brief Restore Toolbar Configuration
 *
 *  \par Function Description
 *  This function restores the state of the Toolbar widgets.
 */
void x_toolbars_restore_state(GSCHEM_TOPLEVEL *w_current) {

  char       *filename;
  const char *group_name;
  GError     *err = NULL;
  GKeyFile   *key_file = NULL;

  void RestoreBarProperties(GtkWidget * handlebox) {
    int bar_id;
    int visible;
    int style;

    bar_id = GET_TOOLBAR_ID(handlebox);
    group_name = IDS_Toolbar_Names[bar_id];

    if(key_file) {
      visible = g_key_file_get_integer (key_file, group_name, "visible", &err);
      if(!err) {
        gtk_widget_set_visible(handlebox, visible);
        x_menu_set_toolbar_toggle(w_current, bar_id, visible);
      }
      else {
        gtk_widget_set_visible(handlebox, TRUE);
        g_clear_error (&err);
      }
      style = g_key_file_get_integer (key_file, group_name, "style", &err);
      if(!err) {
        gtk_toolbar_set_style(GTK_TOOLBAR (GTK_BIN (handlebox)->child), style);
      }
      else {
        gtk_toolbar_set_style(GTK_TOOLBAR (GTK_BIN (handlebox)->child), DEFAULT_TOOLBAR_STYLE);
        g_clear_error (&err);
      }
    }
    else
      s_log_message("Error, Toolbar configuration key file, %s\n", group_name);

  }

  void RestoreAllBars() {
    if(key_file) {
      v_log_message("Retrieving toolbar geometry\n");
      RestoreBarProperties(w_current->add_handlebox);
      RestoreBarProperties(w_current->attribute_handlebox);
      RestoreBarProperties(w_current->edit_handlebox);
      RestoreBarProperties(w_current->grid_snap_handlebox);
      RestoreBarProperties(w_current->page_handlebox);
      RestoreBarProperties(w_current->select_handlebox);
      RestoreBarProperties(w_current->standard_handlebox);
      RestoreBarProperties(w_current->zoom_handlebox);
    }
  }

  filename = g_build_filename(s_path_user_config (), TOOLBAR_GEOMETRY_STORE, NULL);

  if(g_file_test (filename, G_FILE_TEST_EXISTS)) {
    if (access(filename, R_OK) == 0) {
      key_file = g_key_file_new();
      if(g_key_file_load_from_file(key_file, filename, G_KEY_FILE_NONE, &err))
          RestoreAllBars();
      else {
        s_log_message("Warning, Error Restoring Toolbar configuration, %s\n", err->message);
        g_clear_error (&err);
      }
    }
    else {
      s_log_message("Warning, Toolbar configuration file access error:, %s\n", err->message);
    }
  }
  else
    v_log_message("Toolbar configuration geometry not found!\n");

  if(key_file) g_key_file_free(key_file);
  g_free(filename);
}

/*! \brief Finialize Toolbar Initialization
 *
 *  \par Function Description
 * This function completes the final configuration of the toolbar setup
 * based on settings establish during gschem boot-up. The function also
 * sets the visibility of the Close buttons on all the handleboxes.
 * The Main window did a Show All and that revealed all the buttons on
 * handleboxes that should be hidden if they are docked. Rather than turn
 * each widget individually when creating the main window, it's easier to
 * "fix" this by having this routine emit a signal to each handlebox.
 *
 *  \param [in] w_current pointer to top-level data structure
 */
void x_toolbars_finialize (GSCHEM_TOPLEVEL *w_current) {
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  if (w_current->toolbars_mode != RC_NIL) { /* If not keyword then let GTK decide */
    if (w_current->toolbars_mode == TOOLBAR_RETENTION)
      x_toolbars_restore_state(w_current);
    else { /* use rc value */
      lambda (GtkWidget *bar)
      {
        gtk_toolbar_set_style (GTK_TOOLBAR (bar), TOOLBAR_STYLE);
        return FALSE;
      }
      mapcar(TheToolBars);
    }
  }

 /* gtk_toolbar_set_tooltips (GTK_TOOLBAR (data), GTK_TOGGLE_BUTTON (widget)->active );*/

  g_signal_emit_by_name(GTK_WIDGET(w_current->add_handlebox),       "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->attribute_handlebox), "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->edit_handlebox),      "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->grid_snap_handlebox), "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->page_handlebox),      "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->select_handlebox),    "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->standard_handlebox),  "child-attached");
  g_signal_emit_by_name(GTK_WIDGET(w_current->zoom_handlebox),      "child-attached");

  gtk_widget_hide(bar_widgets->toolbar_none);

  x_toolbars_update(w_current);

}

/*! \brief Free Window Specific Toolbar Widgets
 *  \par Function Description
 *  This function releases the memory associated with a
 *  ToolBarWidgets structure that allocated with g_new0
 *  in x_toolbars_init_window().
 */
void x_toolbars_free_window(GSCHEM_TOPLEVEL *w_current) {

  ToolBarWidgets *bar_widgets;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  g_slist_free (TheToolBars);

  g_slist_free (CAN_PASTE_LIST);
  g_slist_free (CAN_UNDO_LIST);
  g_slist_free (CAN_REDO_LIST);
  g_slist_free (HAVE_PAGES_LIST);
  g_slist_free (COMPLEX_OBJECTS_LIST);
  g_slist_free (SOME_OBJECTS_LIST);
  g_slist_free (TEXT_OBJECTS_LIST);

  CAN_PASTE_LIST       = NULL;
  CAN_UNDO_LIST        = NULL;
  CAN_REDO_LIST        = NULL;
  HAVE_PAGES_LIST      = NULL;
  COMPLEX_OBJECTS_LIST = NULL;
  SOME_OBJECTS_LIST    = NULL;
  TEXT_OBJECTS_LIST    = NULL;
  TheToolBars          = NULL;

  ui_list = g_slist_remove (ui_list, bar_widgets);
  g_free(bar_widgets);
}

/*! @Callback helper, used by Close button and Popup */
static void do_Hide_HandleBox(GedaHandleBox *handlebox)
{
  if (GEDA_IS_HANDLE_BOX(handlebox)) {
    gtk_widget_hide((GtkWidget*)handlebox);
    int HandleBoxId =  GET_TOOLBAR_ID(handlebox);
    GSCHEM_TOPLEVEL *w_current  =  (GSCHEM_TOPLEVEL*)GET_TOOLBAR_WC(handlebox);
    x_menu_set_toolbar_toggle(w_current, HandleBoxId, FALSE);
  }
  else
    fprintf(stderr, "Error [On_Close_Handlebar]: container is not a handlebox\n");
}


/*! \brief Callback Handler for Popup Mouse Context Menu
 *
 *  \par Function Description
 * This function calls the appropriate functions to process request
 * from the mouse menu. This function receives a pointer to enumerated
 * integer value for the menu item that was selected.
 *
 *  \param [in] widget is button widget
 *  \param [in] selection pointer to enumerated menu selection
 */
static int popup_activated(GtkWidget *widget, IDS_HB_Popup_items* selection)
{
    int WhichItem = (int)(long*) selection;

    switch ( WhichItem ) {
      case DockBar:
        geda_handle_box_dock((GedaHandleBox*)ActiveToolBar.handlebox);
        break;
      case HideBar:
        do_Hide_HandleBox((GedaHandleBox*)ActiveToolBar.handlebox);
        break;
      case MakeHorizontal:
        gtk_toolbar_set_orientation (GTK_TOOLBAR (ActiveToolBar.toolbar), GTK_ORIENTATION_HORIZONTAL);
        break;
      case MakeVertical:
        gtk_toolbar_set_orientation (GTK_TOOLBAR (ActiveToolBar.toolbar), GTK_ORIENTATION_VERTICAL);
        break;
      case ShowIcons:
        gtk_toolbar_set_style (GTK_TOOLBAR (ActiveToolBar.toolbar), TOOLBAR_SHOW_ICONS);
        break;
      case ShowText:
        gtk_toolbar_set_style (GTK_TOOLBAR (ActiveToolBar.toolbar), TOOLBAR_SHOW_TEXT);
        break;
      case ShowBoth:
        gtk_toolbar_set_style (GTK_TOOLBAR (ActiveToolBar.toolbar), TOOLBAR_SHOW_BOTH);
        break;
      case ShowHorizontal:
        gtk_toolbar_set_style (GTK_TOOLBAR (ActiveToolBar.toolbar), TOOLBAR_SHOW_HORIZ);
        break;
      default:
        s_log_message("menu_responder(): UKNOWN MENU ID: %d\n", WhichItem);
    } /* End Switch WhichItem */

    gtk_widget_destroy(popup_menu);
    return (TRUE);
}
/*! \brief Create and Setup Popup Mouse Menu for Toolbar
 *
 *  \par Function Description
 * This function is called when the user right clicks on a handlebox.
 * The function sets senitivty on menu choices based on the handlebox
 * position and the state of the containing toolbar.
 *
 *  \param [in] sheet is the active sheet widget
 */
static GtkWidget *build_menu(GtkWidget *widget)
{
  GtkWidget   *menu;
  GtkWidget   *item;
  GtkTooltips *tooltips;

  bool is_floating;
  int  orientation;
  int  style;
  int i;

  tooltips = gtk_tooltips_new ();

  ActiveToolBar.handlebox = GEDA_HANDLE_BOX (widget);
  ActiveToolBar.toolbar   = (GtkToolbar*) gtk_bin_get_child(GTK_BIN(ActiveToolBar.handlebox));

  is_floating = !geda_handle_box_get_child_detached (ActiveToolBar.handlebox);
  orientation =  gtk_toolbar_get_orientation(ActiveToolBar.toolbar);
  style       =  gtk_toolbar_get_style(ActiveToolBar.toolbar);

  menu=gtk_menu_new();

  for (i=0; i < (sizeof(popup_items)/sizeof(popup_items[0])) ; i++)
  {
    item = gtk_menu_item_new_with_label(_(popup_items[i]));

    gtk_tooltips_set_tip (tooltips, item, _(popup_tips[i]), NULL);

    g_signal_connect(GTK_OBJECT(item),"activate",
                    (void *) popup_activated,
                    (void *) i);

    gtk_widget_set_sensitive(GTK_WIDGET(item), TRUE);
    gtk_widget_set_can_focus(GTK_WIDGET(item), TRUE);

    switch (i) {
      case DockBar:
        if (is_floating) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case HideBar:
        break;
      case MakeHorizontal:
        if (is_floating || orientation == GTK_ORIENTATION_HORIZONTAL) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case MakeVertical:
        if (is_floating || orientation == GTK_ORIENTATION_VERTICAL) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case ShowIcons:
        if (style == TOOLBAR_SHOW_ICONS) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case ShowText:
        if (style == TOOLBAR_SHOW_TEXT) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      case ShowBoth:
        if (style == TOOLBAR_SHOW_BOTH) {
          gtk_widget_set_sensitive(GTK_WIDGET(item), FALSE);
          gtk_widget_set_can_focus(GTK_WIDGET(item), FALSE);
        }
        break;
      }
      g_object_set (item, "visible", TRUE, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
    }
    return (menu);
}
/*! \brief HandleBar Mouse Button Call Back
 *
 *  \par Function Description
 * This function check mouse botton press and when the 3rd button
 * is released the build_menu function is called to create the mouse
 * menu.
 *
 *  \param [in] handlebox is the handlebox widget when user "right-clicked"
 */
static int
On_mouse_button_press(GtkWidget *widget, GdkEventButton *event, GSCHEM_TOPLEVEL *w_current)
{
    GdkModifierType mods;
    GtkWidget *handlebox = GTK_WIDGET(widget);

    gdk_window_get_pointer (gtk_widget_get_window(handlebox), NULL, NULL, &mods);

    if (mods&GDK_BUTTON3_MASK)
    {
        if (popup_menu)
        {
            gtk_object_destroy(GTK_OBJECT(popup_menu));
            popup_menu = NULL;
        }

        popup_menu = build_menu(handlebox);
        /* Tell GTK to do the menu we just created */
        gtk_menu_popup(GTK_MENU(popup_menu), NULL, NULL, NULL, NULL,
                       event->button, event->time);
    }
    return (FALSE);
}

/*! @brief Toolbar Close Button Handler */
static void
On_Close_Handlebar(GtkWidget *CloseButton, GSCHEM_TOPLEVEL *w_current)
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
  geda_handle_box_set_shadow_type ( handlebox, GTK_SHADOW_ETCHED_IN );
  gtk_widget_show (CloseButton);
}

/*! \brief Add Close Button Toolbars - the little red X's
 *
 *  \par Function Description
 * This function creates a single close buttons on the each toolbars
 * along with the necessary alignment containers. The function sets up
 * callbacks to the functions defined above.
 *
 *  \param [in] parent_container is main vbox widget (main_box)
 */
static void
x_toolbars_add_closer(GSCHEM_TOPLEVEL *w_current, GtkWidget *HandleBar, GtkWidget *ToolBar) {

  GtkWidget *CloseButton;
  GtkWidget *fixed;
  GtkWidget *alignment;
  GtkStyle  *style;
  GtkWidget *x_image;

  /* Create a Fixed Widget to hold the Close Buttton and add to the Toolbar */
  fixed = gtk_fixed_new();
  gtk_container_add(GTK_CONTAINER(ToolBar), fixed);

  /* Create the Close Button */
  CloseButton = gtk_button_new();

  /* Set Properties and Styles for the Close Button */
  gtk_widget_set_size_request(CloseButton, 16, 16);
  gtk_button_set_relief(GTK_BUTTON (CloseButton), GTK_RELIEF_NONE);
  gtk_button_set_focus_on_click(GTK_BUTTON (CloseButton), FALSE);
  style = gtk_widget_get_style(CloseButton);
  style->bg[GTK_STATE_PRELIGHT] = style->bg[GTK_STATE_NORMAL];
  gtk_widget_set_style(CloseButton, style);

  /* Put the Close Buttton inside the Fixed container and show it */
  gtk_container_add(GTK_CONTAINER(fixed), CloseButton);      /* Put Button Widget Inside the fixed container */
  gtk_widget_show (CloseButton);

  /* Create a New Alignment widget to hold the button Image */
  alignment = gtk_alignment_new (0, 0, 0, 0);
  gtk_widget_show (alignment);
  gtk_container_add (GTK_CONTAINER (CloseButton), alignment); /* Put Alignment Widget Inside the Button */

  /* Create a Pixmap widget containing the image and add the widget to the container */
  x_image = create_pixmap (CLOSE_TOOLBAR_BITMAP);
  gtk_container_add (GTK_CONTAINER (alignment), x_image);     /*Put image inside the Alignment container */
  gtk_widget_show (x_image);

  /* Setup the signal handlers */
  g_signal_connect (CloseButton,"pressed",
                    G_CALLBACK (On_Close_Handlebar),
                    w_current); /* not really needed since menu has embed ptr */

  g_signal_connect (HandleBar,"child-attached",
                    G_CALLBACK (On_Dock_ToolBar),
                    CloseButton);

  g_signal_connect (HandleBar,"child-detached",
                    G_CALLBACK (On_Float_ToolBar),
                    CloseButton);

  GtkObject *HandleBarObj = (GtkObject *)HandleBar;
  gtk_signal_connect(HandleBarObj, "button_press_event",
                    (GtkSignalFunc) On_mouse_button_press,
                    w_current);

  gtk_object_set_data(GTK_OBJECT(HandleBar), "CloseButton",  CloseButton);

  return;
}

/*! \section Initialize-Toolbars
 *
 *  \par
 *  Note that this section relies heavily on MACROS defined in
 *  geda_toolbars.h in order to reduce coding errors and to help
 *  clarify the "subject" of the algorythms, rather then obsuring
 *  the intent in >10k lines of gtk_xxx's.
 *
 */

void x_toolbars_init_window(GSCHEM_TOPLEVEL *w_current) {

  ToolBarWidgets *bar_widgets = g_new0 (ToolBarWidgets, 1);

  TheToolBars          = NULL;

  CAN_PASTE_LIST       = NULL;
  CAN_UNDO_LIST        = NULL;
  CAN_REDO_LIST        = NULL;
  HAVE_PAGES_LIST      = NULL;
  COMPLEX_OBJECTS_LIST = NULL;
  SOME_OBJECTS_LIST    = NULL;
  TEXT_OBJECTS_LIST    = NULL;

  if (w_current->ui_index > -1)
    ui_list = g_slist_insert (ui_list, bar_widgets, w_current->ui_index);
  else
    ui_list = g_slist_append (ui_list, bar_widgets);
}
/*! \brief Initialize Toolbars at the Top of the Main Window
 *
 *  \par Function Description
 * This function creates handleboxes, toolbars, toolbar buttons and
 * related containers in order to create all of the Toolbars at the
 * Top of the Main Window. This function initializes the global
 * variables in the compilation unit.
 *
 *  \param [in] parent_container is main vbox widget (main_box)
 */
void x_toolbars_init_top(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container) {

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
  GtkWidget *toolbox_T1 = gtk_hbox_new (FALSE, 0);
  GEDA_PACK_TOOLBOX (parent_container, toolbox_T1)

  /* --------- Create and Populate the Standard Toolbar -------- */
  /* Standard Toolbar*/
  w_current->standard_handlebox = geda_handle_box_new();
  GEDA_PACK_TOOLBOX (toolbox_T1, w_current->standard_handlebox);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and put it into our handlebox */

  Standard_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Standard_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_container_set_border_width (GTK_CONTAINER (Standard_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->standard_handlebox), Standard_Toolbar);

  /* Add New, Open, Save and Save As Buttons to the Standard Toolbar */
  TOOLBAR_GEDA_BUTTON( Standard, etb_new,     LOCAL_ALT, NEW,     x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Standard, etb_open,    LOCAL_STK, OPEN,    x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Standard, etb_save,    LOCAL_STK, SAVE,    x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Standard, etb_save_as, LOCAL_STK, SAVE_AS, x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Standard, etb_close,   LOCAL_ALT, CLOSE,   x_toolbars_execute, w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  /* Add Print and Export PDF Buttons to the Standard Toolbar */
  TOOLBAR_GEDA_BUTTON( Standard, etb_print,      LOCAL_STK, PRINT,          x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Standard, etb_write_pdf,  LOCAL_PIX, GAF_PDF_BITMAP, x_toolbars_execute, w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  TOOLBAR_GEDA_BUTTON( Standard, etb_cut,   LOCAL_STK, CUT,   x_toolbars_execute, w_current)
  TOOLBAR_GEDA_BUTTON( Standard, etb_copy,  LOCAL_STK, COPY,  x_toolbars_execute, w_current)
  TOOLBAR_GEDA_BUTTON( Standard, etb_paste, LOCAL_STK, PASTE, x_toolbars_execute, w_current)

  gtk_toolbar_append_space (GTK_TOOLBAR(Standard_Toolbar));

  TOOLBAR_GEDA_BUTTON( Standard, etb_undo,      LOCAL_STK, UNDO,        x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Standard, etb_redo,      LOCAL_STK, REDO,        x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Standard, etb_configure, LOCAL_ALT, PREFERENCES, x_toolbars_execute,  w_current);

  g_object_set (Standard_Toolbar, "visible", TRUE, NULL);

  SET_TOOLBAR_ID  (w_current->standard_handlebox, tb_Standard);
  SET_TOOLBAR_WC  (w_current->standard_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->standard_handlebox, Standard_Toolbar );

  CAN_PASTE_LIST    = g_slist_append ( CAN_PASTE_LIST,     TB_BUTTON (etb_paste));
  SOME_OBJECTS_LIST = g_slist_append ( SOME_OBJECTS_LIST,  TB_BUTTON (etb_cut  ));
  SOME_OBJECTS_LIST = g_slist_append ( SOME_OBJECTS_LIST,  TB_BUTTON (etb_copy ));
  CAN_UNDO_LIST     = g_slist_append ( CAN_UNDO_LIST,      TB_BUTTON (etb_undo ));
  CAN_REDO_LIST     = g_slist_append ( CAN_REDO_LIST,      TB_BUTTON (etb_redo ));

  /* ----------- Create and Populate the Page Toolbar ----------- */

  w_current->page_handlebox = geda_handle_box_new ();
  GEDA_PACK_TOOLBOX (toolbox_T1, w_current->page_handlebox);

  Page_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Page_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_container_set_border_width (GTK_CONTAINER (Page_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->page_handlebox), Page_Toolbar);

  TOOLBAR_GEDA_BUTTON( Page, etb_prev_page,    LOCAL_STK, GO_BACK,             x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Page, etb_next_page,    LOCAL_STK, GO_FORWARD,          x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Page, etb_new_page,     LOCAL_STK, NEW,                 x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Page, etb_page_manager, LOCAL_PIX, GEDA_SHEETS_BITMAP,  x_toolbars_execute, w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Page_Toolbar));

  TOOLBAR_GEDA_BUTTON( Page, etb_down_schematic, LOCAL_PIX, GEDA_DEMOTE_BITMAP,   x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Page, etb_down_symbol,    LOCAL_PIX, GEDA_DEMOTE_BITMAP,   x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Page, etb_hierarchy_up,   LOCAL_PIX, GEDA_PROMOTE_BITMAP,  x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Page, etb_view_document,  LOCAL_PIX, GAF_SEE_NOTES_BITMAP, x_toolbars_execute, w_current);

  HAVE_PAGES_LIST     = g_slist_append ( HAVE_PAGES_LIST, TB_BUTTON( etb_prev_page ));
  HAVE_PAGES_LIST     = g_slist_append ( HAVE_PAGES_LIST, TB_BUTTON( etb_next_page ));

  COMPLEX_OBJECTS_LIST = g_slist_append ( COMPLEX_OBJECTS_LIST, TB_BUTTON( etb_down_schematic));
  COMPLEX_OBJECTS_LIST = g_slist_append ( COMPLEX_OBJECTS_LIST, TB_BUTTON( etb_down_symbol   ));
  COMPLEX_OBJECTS_LIST = g_slist_append ( COMPLEX_OBJECTS_LIST, TB_BUTTON( etb_hierarchy_up  ));
  COMPLEX_OBJECTS_LIST = g_slist_append ( COMPLEX_OBJECTS_LIST, TB_BUTTON( etb_view_document ));

  g_object_set (Page_Toolbar, "visible", TRUE, NULL);

  SET_TOOLBAR_ID       (w_current->page_handlebox, tb_Page);
  SET_TOOLBAR_WC       (w_current->page_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->page_handlebox, Page_Toolbar );

#if DEBUG_TOOLBARS
  fprintf(stderr, "what happen to [%s]\n", GAF_SEE_NOTES_BITMAP ); /* can fill in missing icon */
#endif

  /* Start Second Toolbar Row */
  GtkWidget *toolbox_T2 = gtk_hbox_new (FALSE, 0);
  GEDA_PACK_TOOLBOX (parent_container, toolbox_T2);

  /* --------- Create and Populate the Add Toolbar -------- */
  w_current->add_handlebox = geda_handle_box_new ();
  GEDA_PACK_TOOLBOX (toolbox_T2, w_current->add_handlebox);

  Add_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Add_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_container_set_border_width (GTK_CONTAINER (Add_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->add_handlebox), Add_Toolbar);

  /* not part of any radio button group */
  TOOLBAR_GEDA_BUTTON( Add, etb_add_attribute, LOCAL_PIX, GAF_MAP(ADD_ATTRIBUTE), x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Add, etb_insert_pic,    LOCAL_PIX, GEDA_FILM_ROLL_BITMAP,  x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Add, etb_add_text,      LOCAL_PIX, GSCHEM_TEXT_BITMAP,     x_toolbars_execute,  w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Add_Toolbar));

  /* Toolbar radio button group - ToolBar_Radio_Responder defines a callback so ver 1 is expanded here*/
  /*                    bar, var,              grp,              name,            data */
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(line),   NULL,             etb_add_line,    w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(path),   BarRadio(line),   etb_add_path,    w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(arc),    BarRadio(path),   etb_add_arc,     w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(box),    BarRadio(arc),    etb_add_box,     w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(circle), BarRadio(box),    etb_add_circle,  w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(pin),    BarRadio(circle), etb_add_pin,     w_current);
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(bus),    BarRadio(pin),    etb_add_bus,     w_current)
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(net),    BarRadio(bus),    etb_add_net,     w_current)

  TOOLBAR_GEDA_BUTTON( Add, etb_add_component, LOCAL_PIX, GSCHEM_TRANSISTOR_BITMAP, x_toolbars_execute,  w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Add_Toolbar));

  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(select),   BarRadio(net),    etb_selector,   w_current)
  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(deselect), BarRadio(net),    etb_deselector, w_current)

  TOOLBAR_GSCHEM_RADIO( Add, BarRadio(none),     BarRadio(select), etb_none, w_current)

  /* Append all Toolbar "Mode" radio widgets to a GSlist, add a record in the struct
   * ToolBarWidgets, see function x_toolbars_update */
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

  g_object_set (Add_Toolbar, "visible", TRUE, NULL);

  SET_TOOLBAR_ID       (w_current->add_handlebox, tb_Add);
  SET_TOOLBAR_WC       (w_current->add_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->add_handlebox, Add_Toolbar );

  /* ------ Create and Populate the Selection Toolbar ------ */

  /* Select Toolbar*/
  w_current->select_handlebox = geda_handle_box_new();
  GEDA_PACK_TOOLBOX (toolbox_T2, w_current->select_handlebox);

  /* toolbar will be horizontal, with both icons and text, and with
   * 5pxl spaces between items and put it into our handlebox */

  Select_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Select_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_container_set_border_width (GTK_CONTAINER (Select_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->select_handlebox), Select_Toolbar);

  TOOLBAR_GEDA_BUTTON( Select, etb_deselect_all,  LOCAL_FAC, NULL, x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Select, etb_select_all,    LOCAL_FAC, NULL, x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Select, etb_select_invert,   LOCAL_FAC, NULL, x_toolbars_execute,  w_current);

  g_object_set (Select_Toolbar, "visible", TRUE, NULL);

  SET_TOOLBAR_ID       (w_current->select_handlebox, tb_Select);
  SET_TOOLBAR_WC       (w_current->select_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->select_handlebox, Select_Toolbar );

  /* --------- Create and Populate the Zoom Toolbar -------- */
  w_current->zoom_handlebox = geda_handle_box_new ();
  GEDA_PACK_TOOLBOX (toolbox_T2, w_current->zoom_handlebox);

  Zoom_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Zoom_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_container_set_border_width (GTK_CONTAINER (Zoom_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->zoom_handlebox), Zoom_Toolbar);

  TOOLBAR_GEDA_BUTTON( Zoom, etb_view_redraw,  LOCAL_ALT, ZOOM_FIT, x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, etb_zoom_pan,     LOCAL_ALT, ZOOM_FIT, x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, etb_zoom_box,     LOCAL_ALT, ZOOM_FIT, x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, etb_zoom_select,  LOCAL_FAC, ZOOM_FIT, x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, etb_zoom_extents, LOCAL_ALT, ZOOM_FIT, x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, etb_zoom_in,      LOCAL_ALT, ZOOM_IN,  x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, etb_zoom_out,     LOCAL_ALT, ZOOM_OUT, x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Zoom, etb_zoom_all,     LOCAL_ALT, ZOOM_100, x_toolbars_execute, w_current);

  g_object_set (Zoom_Toolbar, "visible", TRUE, NULL);

  SET_TOOLBAR_ID       (w_current->zoom_handlebox, tb_Zoom);
  SET_TOOLBAR_WC       (w_current->zoom_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->zoom_handlebox, Zoom_Toolbar );

  TheToolBars = g_slist_append ( TheToolBars, Add_Toolbar);
  TheToolBars = g_slist_append ( TheToolBars, Page_Toolbar);
  TheToolBars = g_slist_append ( TheToolBars, Select_Toolbar);
  TheToolBars = g_slist_append ( TheToolBars, Standard_Toolbar);
  TheToolBars = g_slist_append ( TheToolBars, Zoom_Toolbar);

#if DEBUG_TOOLBARS
   fprintf(stderr, "init_top exit\n");
#endif

}

/*! \brief Initialize Toolbar at the Left of the Main Window
 *
 *  \par Function Description
 * This function creates handleboxes, toolbars, toolbar buttons and
 * related containers in order to create the Toolbar at the Left
 * of the Main Window.
 *
 *  \param [in] parent_container is the center_hbox widget
 */
void x_toolbars_init_left(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container) {
  GtkWidget *Edit_Toolbar;
  ToolBarWidgets *bar_widgets;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  GtkWidget *toolbox_L1 = gtk_vbox_new (FALSE, 0);
  GEDA_PACK_TOOLBOX (parent_container, toolbox_L1);

  /* --------- Create and Populate the Edit Toolbar -------- */
  w_current->edit_handlebox = geda_handle_box_new ();
  GEDA_PACK_TOOLBOX (toolbox_L1, w_current->edit_handlebox);

  Edit_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (Edit_Toolbar), GTK_ORIENTATION_VERTICAL);
  gtk_container_set_border_width (GTK_CONTAINER (Edit_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->edit_handlebox), Edit_Toolbar);

  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_copy,  LOCAL_PIX, GEDA_COPY_BITMAP,    x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_multi_copy, LOCAL_PIX, GEDA_MULTI_BITMAP,   x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_move,       LOCAL_PIX, GEDA_MOVE_BITMAP,    x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_mirror,     LOCAL_PIX, GEDA_MIRROR_BITMAP,  x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_rotate,     LOCAL_PIX, GEDA_ROTATE_BITMAP,  x_toolbars_execute, w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Edit_Toolbar));

  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_butes, LOCAL_STK, INDENT,              x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_color, LOCAL_PIX, GEDA_DISPLAY_COLOR_BITMAP, x_toolbars_execute,  w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(Edit_Toolbar));

  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_text,  LOCAL_STK, EDIT,                   x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_slot,  LOCAL_FAC, NULL,                   x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_pin,   LOCAL_FAC, NULL,                   x_toolbars_execute, w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_line,  LOCAL_PIX, GEDA_LINE_TYPE_BITMAP,  x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_fill,  LOCAL_STR, NULL,                   x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_edit_arc,   LOCAL_FAC, NULL,                   x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_translate,  LOCAL_PIX, GEDA_TRANSLATE_BITMAP,  x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_lock,       LOCAL_PIX, GEDA_LOCK_BITMAP,       x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_unlock,     LOCAL_PIX, GEDA_UNLOCK_BITMAP,     x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( Edit, etb_update,     LOCAL_STK, REFRESH,                x_toolbars_execute, w_current);

  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_edit_copy  ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_multi_copy ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_move   ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_mirror ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_rotate ));

  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_edit_butes ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_edit_color ));

  TEXT_OBJECTS_LIST    = g_slist_append (TEXT_OBJECTS_LIST,    TB_BUTTON ( etb_edit_text ));
  COMPLEX_OBJECTS_LIST = g_slist_append (COMPLEX_OBJECTS_LIST, TB_BUTTON ( etb_edit_slot ));

  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_edit_pin  ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_edit_line ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_edit_fill ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_edit_arc  ));

  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_lock   ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_unlock ));
  SOME_OBJECTS_LIST = g_slist_append (SOME_OBJECTS_LIST, TB_BUTTON ( etb_update ));

  g_object_set (Edit_Toolbar, "visible", TRUE, NULL);

  SET_TOOLBAR_ID        (w_current->edit_handlebox, tb_Edit);
  SET_TOOLBAR_WC        (w_current->edit_handlebox, w_current);
  x_toolbars_add_closer (w_current, w_current->edit_handlebox, Edit_Toolbar );
  TheToolBars = g_slist_append ( TheToolBars, Edit_Toolbar);
}

/*! \brief Initialize Toolbar at the Bottom of the Main Window
 *  \par Function Description
 * This function creates handleboxes, toolbars, toolbar buttons and
 * related containers in order to create the Toolbar at the Bottom
 * of the Main Window.
 *
 *  \param [in] parent_container is the main_box widget (same as the top bar)
 */
void x_toolbars_init_bottom(GSCHEM_TOPLEVEL *w_current, GtkWidget *parent_container) {

  GtkWidget *Attribute_Toolbar;
  GtkWidget *GripSnap_Toolbar;
  ToolBarWidgets *bar_widgets;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);
  /* Start Bootom Toolbar Row */
  GtkWidget *toolbox_B1 = gtk_hbox_new (FALSE, 0);
  GEDA_PACK_TOOLBOX (parent_container, toolbox_B1);

  /* --------- Create and Populate the Attribute Toolbar -------- */

  w_current->attribute_handlebox = geda_handle_box_new ();
  GEDA_PACK_TOOLBOX (toolbox_B1,  w_current->attribute_handlebox);

  Attribute_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR (Attribute_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_container_set_border_width (GTK_CONTAINER (Attribute_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->attribute_handlebox), Attribute_Toolbar);

  /* Add Attribute Button to Toolbar */
  TOOLBAR_GEDA_BUTTON(Attribute, etb_attach,     LOCAL_PIX, GAF_PROMOTE_BITMAP,  x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, etb_detach,     LOCAL_PIX, GAF_DEMOTE_BITMAP,   x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, etb_show_value, LOCAL_PIX, GEDA_VALUE_BITMAP,   x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, etb_show_name,  LOCAL_PIX, GEDA_NAME_TAG_BITMAP,    x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, etb_show_both,  LOCAL_PIX, GEDA_NAME_VALUE_BITMAP,  x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, etb_visibilty,  LOCAL_PIX, GEDA_EYE_GLASSES_BITMAP,         x_toolbars_execute,  w_current);

  TOOLBAR_GEDA_BUTTON(Attribute, etb_show_hidden, LOCAL_STK, FIND_AND_REPLACE,               x_toolbars_execute, w_current);

  TOOLBAR_GEDA_BUTTON(Attribute, etb_find_text,  LOCAL_ALT, FIND,                            x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, etb_hide_text,  LOCAL_PIX, GEDA_GHOST_INVISIBLE_BITMAP,     x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, etb_show_specific, LOCAL_PIX, GEDA_LOCATE_REFERENCE_BITMAP, x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON(Attribute, etb_auto_number,   LOCAL_PIX, GEDA_NUMBER_BITMAP,           x_toolbars_execute,  w_current);

  TEXT_OBJECTS_LIST = g_slist_append ( TEXT_OBJECTS_LIST, TB_BUTTON ( etb_attach     ));
  TEXT_OBJECTS_LIST = g_slist_append ( TEXT_OBJECTS_LIST, TB_BUTTON ( etb_detach     ));
  TEXT_OBJECTS_LIST = g_slist_append ( TEXT_OBJECTS_LIST, TB_BUTTON ( etb_show_value ));
  TEXT_OBJECTS_LIST = g_slist_append ( TEXT_OBJECTS_LIST, TB_BUTTON ( etb_show_name  ));
  TEXT_OBJECTS_LIST = g_slist_append ( TEXT_OBJECTS_LIST, TB_BUTTON ( etb_show_both  ));

  g_object_set (Attribute_Toolbar, "visible", TRUE, NULL);

  SET_TOOLBAR_ID       (w_current->attribute_handlebox, tb_Attribute);
  SET_TOOLBAR_WC       (w_current->attribute_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->attribute_handlebox, Attribute_Toolbar );

  TheToolBars = g_slist_append ( TheToolBars, Attribute_Toolbar);

  /* -------- Create and Populate the GridSnap Toolbar -------- */
  w_current->grid_snap_handlebox = geda_handle_box_new ();
  GEDA_PACK_TOOLBOX (toolbox_B1, w_current->grid_snap_handlebox);

  GripSnap_Toolbar = gtk_toolbar_new ();

  gtk_toolbar_set_orientation    (GTK_TOOLBAR   (GripSnap_Toolbar), GTK_ORIENTATION_HORIZONTAL);
  gtk_container_set_border_width (GTK_CONTAINER (GripSnap_Toolbar), 0);
  gtk_container_add              (GTK_CONTAINER (w_current->grid_snap_handlebox), GripSnap_Toolbar);

  /* Toolbar radio button group - ToolBar_Radio_Responder defines a callback so ver 1 is expanded here*/
  /*                    bar,      var,             grp,             name,            data */
  TOOLBAR_GSCHEM_RADIO( GripSnap, BarRadio(dot),   NULL,            etb_grid_dot,    w_current);
  TOOLBAR_GSCHEM_RADIO( GripSnap, BarRadio(mesh),  BarRadio(dot),   etb_grid_mesh,   w_current);
  TOOLBAR_GSCHEM_RADIO( GripSnap, BarRadio(off),   BarRadio(mesh),  etb_grid_off,    w_current);

  gtk_toolbar_append_space (GTK_TOOLBAR(GripSnap_Toolbar));

  TOOLBAR_GEDA_BUTTON( GripSnap, etb_snap_up,     LOCAL_STK, GOTO_TOP,    x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( GripSnap, etb_snap_down,   LOCAL_STK, GOTO_BOTTOM, x_toolbars_execute,  w_current);
  TOOLBAR_GEDA_BUTTON( GripSnap, etb_snap_set,    LOCAL_FAC, JUMP_TO,     x_toolbars_execute,  w_current);

  TOOLBAR_GEDA_BUTTON( GripSnap, etb_snap_off,    LOCAL_FAC, NULL,     x_toolbars_snap_toggle,  w_current);
  TOOLBAR_GEDA_BUTTON( GripSnap, etb_snap_on,     LOCAL_FAC, NULL,     x_toolbars_snap_toggle,  w_current);

  g_object_set (GripSnap_Toolbar, "visible", TRUE, NULL);

  g_object_set_data (G_OBJECT(etb_snap_off_button), "snap-widget", etb_snap_on_button);
  g_object_set_data (G_OBJECT(etb_snap_on_button),  "snap-widget", etb_snap_off_button);

  if (w_current->snap == SNAP_OFF) {
    g_object_set (etb_snap_off_button, "visible", FALSE, NULL);
  }
  else {
    g_object_set (etb_snap_on_button, "visible", FALSE, NULL);
  }

  SET_TOOLBAR_ID       (w_current->grid_snap_handlebox, tb_Grid_Snap);
  SET_TOOLBAR_WC       (w_current->grid_snap_handlebox, w_current);
  x_toolbars_add_closer(w_current, w_current->grid_snap_handlebox, GripSnap_Toolbar );

  TheToolBars = g_slist_append ( TheToolBars, GripSnap_Toolbar);
}

/*! \brief Set Sensitivity of Toolbar Buttons
 *  \par Function Description
 *   This function is called by x_toolbars_set_sensitivities with a gslist
 *   of toolbar button widgets to be set to the specified sensitivity
 *
 *  \param [in] ListToolBarItems SINGLE linked list of widgets
 *  \param [in] sensitivee boolean TRUE = sensitive, FALSE = gray-out
 */
static void x_toolbar_set_sensitivity(GSList *ListToolBarItems, int sensitive)
{
    lambda (GtkWidget *item)
    {
      if (GTK_IS_WIDGET(item))
        gtk_widget_set_sensitive(item, sensitive);
      else
        fprintf(stderr, "x_toolbar_set_sensitivity, item is not a widget\n");
      return FALSE;
    }
    mapcar(ListToolBarItems);
}

/*! \brief Set Sensitivity of Toolbar Button Groups
 *
 *  \par Function Description
 *  This functions sets the sensitivities of toolbar button as a visual
 *  aid to the user. When the sensitivity is set to FALSE widgets are
 *  grayed-out to indicate they are not applicable to the current context.
 *  Note that this is TO purely aid the user, cosmetically the GUI "looks"
 *  better with color and nothing bad would happen even if a button was
 *  pressed when not applicable, gschem just ignores the signals.
 *
 *  \param [in] mode is an enumerated group identifier (see in globals.h)
 *  \param [in] state boolean TRUE = sensitive, FALSE = gray-out
 */
void x_toolbars_set_sensitivities(GSCHEM_TOPLEVEL *w_current, ID_SENITIVITY_MODE mode, bool state) {
  ToolBarWidgets *bar_widgets;

  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  switch (mode) {
    case CAN_PASTE:      x_toolbar_set_sensitivity( CAN_PASTE_LIST, state);
      break;
    case CAN_UNDO:       x_toolbar_set_sensitivity( CAN_UNDO_LIST, state);
      break;
    case CAN_REDO:       x_toolbar_set_sensitivity( CAN_REDO_LIST, state);
      break;
    case HAVE_PAGES:     x_toolbar_set_sensitivity( HAVE_PAGES_LIST, state);
      break;
    case COMPLEX_OBJECTS: x_toolbar_set_sensitivity( COMPLEX_OBJECTS_LIST, state);
      break; /* is optional */
    case TEXT_OBJECTS:    x_toolbar_set_sensitivity( TEXT_OBJECTS_LIST, state);
      break;
    case SOME_OBJECTS:    x_toolbar_set_sensitivity( SOME_OBJECTS_LIST, state);
  }
}

static void turn_off_radio(RadioMenuData* radio_data) {
  g_signal_handler_block   ( radio_data->widget,   radio_data->handler);
  g_object_set  ( G_OBJECT ( radio_data->widget), "active", FALSE, NULL);
  g_signal_handler_unblock ( radio_data->widget,   radio_data->handler);
}
/*!
 * \brief View Toolbar Icons
 * \par Function Description
 *
 */
/* View->Toolbar */
void x_toolbar_icons_only(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GtkToolbar* toolbar){
    gtk_toolbar_set_style (toolbar, TOOLBAR_SHOW_ICONS);
    return FALSE;
  }
  mapcar(TheToolBars)

  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 1));
  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 2));
  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 3));
}

/*!
 * \brief View Toolbar Text
 * \par Function Description
 *
 */
void x_toolbar_text_only(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GtkToolbar* toolbar) {
    gtk_toolbar_set_style (toolbar, TOOLBAR_SHOW_TEXT);
    return FALSE;
  }
  mapcar(TheToolBars)

  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 0));
  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 2));
  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 3));
}

/*!
 * \brief View Toolbar Icons & Text Stacked Vertically
 * \par Function Description
 *
 */
void x_toolbar_display_both(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GtkToolbar* toolbar){
    gtk_toolbar_set_style (toolbar, TOOLBAR_SHOW_BOTH);
    return FALSE;
  }
  mapcar(TheToolBars)

  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 0));
  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 1));
  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 3));
}

/*!
 * \brief View Toolbar Icons & Text Side by Side
 * \par Function Description
 *
 */
void x_toolbar_display_horiz(GtkWidget *widget, GSCHEM_TOPLEVEL *w_current)
{
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);

  lambda (GtkToolbar* toolbar){
    gtk_toolbar_set_style (toolbar, TOOLBAR_SHOW_HORIZ);
    return FALSE;
  }
  mapcar(TheToolBars)

  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 0));
  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 1));
  turn_off_radio ((RadioMenuData*) g_slist_nth_data (w_current->toolbar_mode_grp, 2));
}

/*! \brief Sett All Toolbar Radios InActive
 *
 *  \par Function Description
 * This function completes the final configuration of the toolbar setup

 *  \param [in] w_current pointer to top-level data structure
 */
void x_toolbars_turn_off_all_radios ( GSCHEM_TOPLEVEL *w_current ) {
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(bar_widgets->toolbar_none), TRUE);
}
void x_toolbars_activate_select ( GSCHEM_TOPLEVEL *w_current) {
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(bar_widgets->toolbar_none), TRUE);
}

/*! \brief Update the Toolbars based on the current state
 *  This function sets the state of the "mode" radio buttons on the Add
 *  tool-bar. This is done to synchronize the tool-bars with the rest of
 *  the interface since the "mode" can be set by other means. For example
 *  if the Add/Line menu option is chosen then the Line mode button needs
 *  to be depressed, without emitting a signal. Where as if the Edit/Arc
 *  menu option is selected then the invisible "none" mode button should
 *  be "activated". And so forth.
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 */
void x_toolbars_update(GSCHEM_TOPLEVEL *w_current)
{
  if (!w_current->toolbars) return; /* if toolbars are disabled exit */
  ToolBarWidgets *bar_widgets;
  bar_widgets = g_slist_nth_data (ui_list, w_current->ui_index);
  GtkToggleButton *target = NULL;

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
    case(DRAWNET):
    case(STARTDRAWNET):
    case(NETCONT):
      target = (GtkToggleButton*) bar_widgets->toolbar_net;
      break;
    case(DRAWBUS):
    case(STARTDRAWBUS):
    case(BUSCONT):
      target = (GtkToggleButton*) bar_widgets->toolbar_bus;
      break;
    case(DRAWLINE):
      target = (GtkToggleButton*) bar_widgets->toolbar_line;
      break;
    case(DRAWBOX):
      target = (GtkToggleButton*) bar_widgets->toolbar_box;
      break;
    case(DRAWPATH):
    case(PATHCONT):
      target = (GtkToggleButton*) bar_widgets->toolbar_path;
      break;
    case(DRAWPICTURE): /* \Launches Dialog */
      break;
    case(DRAWPIN):
      target = (GtkToggleButton*) bar_widgets->toolbar_pin;
      break;
    case(DRAWCIRCLE):
      target = (GtkToggleButton*) bar_widgets->toolbar_circle;
      break;
    case(DRAWARC):
      target = (GtkToggleButton*) bar_widgets->toolbar_arc;
      break;
    case(MOVE):
    case(COPY):
    case(ZOOM):
    case(PAN):
    case(STARTPAN):
    case(STARTCOPY):
    case(STARTMOVE):
    case(ENDCOPY):
    case(ENDMOVE):
    case(ENDLINE):
    case(ENDBOX):
    case(ENDPICTURE):
    case(ENDCIRCLE):
    case(ENDARC):
    case(ENDPIN):
    case(ENDCOMP):
    case(ENDTEXT):
    case(ENDROTATEP):
    case(ENDMIRROR):
    case(ZOOMBOXSTART):
    case(ZOOMBOXEND):
    case(STARTROUTENET):
    case(ENDROUTENET):
    case(MOUSEPAN):
    case(STARTPASTE):
    case(ENDPASTE):
    case(GRIPS):
    case(MCOPY):
    case(STARTMCOPY):
    case(ENDMCOPY):
    default:
      target = (GtkToggleButton*) bar_widgets->toolbar_none;
      break;
  }
  if(GTK_IS_TOGGLE_BUTTON(target)) {
    /* if button is not active then action was not initiated by the toolbar */
    if (!target->active) {
      g_signal_handlers_block_by_func (target, x_toolbars_execute_radio, w_current);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(target), TRUE);
      g_signal_handlers_unblock_by_func (target, x_toolbars_execute_radio, w_current);
    }
  }
}
