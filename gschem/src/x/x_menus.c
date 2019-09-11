/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: x_menus.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * \file x_menus.c
 * \brief Main Window Auxiliary Module for Menus, including Context
 */

#include <ctype.h>         /* isdigit */

#include "../../include/gschem.h"
#include "../../include/x_menus.h"
#include "../../include/i_actions.h"

#include <geda/geda_dialog_controls.h>
#include <geda/geda_stat.h>
#include <geda_widgets.h>

#include <geda_debug.h>

/** \defgroup Menu-Module Menu Module
 *  @{
 * \brief This group contains Menu related functions
 * \ingroup main-window
 */

static void x_menu_main_popup_execute(GtkWidget *widget, int action_id);
static void x_menu_path_popup_execute(GtkWidget *widget, int action_id);

/* Note: These are referenced using pop_MenuItem defined in x_menus.h
 *       Actions are defined in i_actions.h
 */
const char* IDS_Popup_Actions[] = {
  ACTION(EDIT_SELECT),    ACTION(ADD_NET),        ACTION(ADD_ATTRIB),
  ACTION(ADD_COMPONENT),  ACTION(ADD_BUS),        ACTION(ADD_TEXT),
  ACTION(VIEW_ZOOM_IN),   ACTION(VIEW_ZOOM_OUT),  ACTION(VIEW_BOX),
  ACTION(VIEW_EXTENTS),   ACTION(VIEW_ZOOM_MAG),  ACTION(VIEW_SELECTED),
  ACTION(EDIT_ATTRIB),    ACTION(EDIT_COLOR),     ACTION(EDIT_COMPONENT),
  ACTION(EDIT_PIN),
  ACTION(EDIT_ARRAY),     ACTION(EDIT_BREAK),     ACTION(EDIT_EXTEND),
  ACTION(EDIT_DELETE),    ACTION(EDIT_COPY),      ACTION(EDIT_MCOPY),
  ACTION(EDIT_MOVE),      ACTION(EDIT_MIRROR),    ACTION(EDIT_ROTATE_LEFT),
  ACTION(EDIT_SNAP),      ACTION(DOWN_SCHEMATIC), ACTION(DOWN_SYMBOL),
  ACTION(HIERARCHY_UP),   ACTION(EDIT_CB_CUT),    ACTION(EDIT_CB_COPY),
  ACTION(EDIT_CB_PASTE),
  NULL
};

/*! \struct <PopupMenuStructure> x_menus.h PopupEntry
 *
 *  Record Format: x_menus.h::st_popup_menu_entry
 *
 *    Menu Text,   function,  action_id, use_stock, icon, Tooltip
 *
 *  Where action_id is the action enumerated in pop_MenuItem defined in
 *  x_menus.h. For Separators and Sub-menu entries function must be NULL.
 *  When function is NULL the action_id fields set whether the record is
 *  is a seperator or a submenu. When action_id = 0 the record is for a
 *  separator, when action_id = 1 is the record is for a sub-menu.
 *
 *  Note: gschem factory, setup by x_icons_setup_factory, are referenced
 *  use_stock = True. if use_stock = False, the string in the icon field
 *  is passed to create_pixmap().
 *
 */
static PopupEntry main_popup_items[] = {

  { N_("Select"),            x_menu_main_popup_execute, pop_edit_select,    1, "gschem-select",  N_("Activate Select mode") },

  { "SEPARATOR",             NULL,                      0,                  0,  NULL,            NULL },

  { N_("Add"),               NULL,                      1,                  0,  NULL,            N_("Add modes") },
  { N_("Net"),               x_menu_main_popup_execute, pop_add_net,        1, "gschem-net",     N_("Add net") },
  { N_("Attribute..."),      x_menu_main_popup_execute, pop_add_attribute,  0,  GAF_MAP(ADD_ATTRIBUTE), N_("Add attribute")},
  { N_("Component..."),      x_menu_main_popup_execute, pop_add_component,  1, "geda-component", N_("Insert a symbol from the component library") },
  { N_("Bus"),               x_menu_main_popup_execute, pop_add_bus,        1, "gschem-bus",     N_("Add bus") },
  { N_("Text"),              x_menu_main_popup_execute, pop_add_text,       1, "gtk-bold",       N_("Add text") },

  { "END_SUB",               NULL,                      0,                  0,  NULL,            NULL },

  { N_("Zoom"),              NULL,                      1,                  0,  NULL,            N_("Add operations") },
  { N_("In"),                x_menu_main_popup_execute, pop_zoom_in,        1, "gtk-zoom-in",    N_("Increase the Zoom magnification") },
  { N_("Out"),               x_menu_main_popup_execute, pop_zoom_out,       1, "gtk-zoom-out",   N_("Decrease the Zoom magnification") },
  { N_("Box"),               x_menu_main_popup_execute, pop_zoom_box,       1, "geda-zoom-box",  N_("Zoom to a Windowed region") },
  { N_("Extents"),           x_menu_main_popup_execute, pop_zoom_extents,   1, "gtk-zoom-fit",   N_("Zoom to the extents of the drawing") },
  { N_("Mag"),               x_menu_main_popup_execute, pop_zoom_to_mag,    1, "zoom-mag",       N_("Zoom to a specified level")},
  { N_("Selection"),         x_menu_main_popup_execute, pop_zoom_to_select, 1, "geda-zoom-selection", N_("Zoom to selected objects")},

  { "END_SUB",               NULL,                      0,                  0,  NULL,            NULL },

  { N_("Edit"),              NULL,                      1,                  0,  NULL,                N_("Edit modes") },
  { N_("Object..."),         x_menu_main_popup_execute, pop_edit_objects,   1, "gtk-indent",         N_("Edit Object Attributes") },
  { N_("Color..."),          x_menu_main_popup_execute, pop_edit_color,     1, "geda-display-color", N_("Open the Color Editor Dialog") },
  { N_("Component..."),      x_menu_main_popup_execute, pop_edit_component, 1, "geda-component",     N_("Open the Component Editor Dialog") },
  { N_("Pin type..."),       x_menu_main_popup_execute, pop_edit_pintype,   1, "geda-pin-type",      N_("Open the Pin Type Dialog") },

  { "END_SUB",               NULL,                      0,                  0,  NULL,            NULL },

  { N_("Array"),             x_menu_main_popup_execute, pop_edit_array,     1, "gschem-array",       N_("Create and array of objects") },
  { N_("Break"),             x_menu_main_popup_execute, pop_edit_break,     1, "break",              N_("Break an object into separate objects") },
  { N_("Extend"),            x_menu_main_popup_execute, pop_edit_extend,    1, "extend",             N_("Project linear objects to other objects") },
  { N_("Delete"),            x_menu_main_popup_execute, pop_edit_delete,    1, "gtk-delete",         N_("Delete the current selection" )},
  { N_("Copy"),              x_menu_main_popup_execute, pop_edit_copy,      1, "geda-copy",          N_("Copy selection") },
  { N_("MCopy"),             x_menu_main_popup_execute, pop_edit_mcopy,     1, "geda-multi",         N_("Make multiple copies of selection") },
  { N_("Move"),              x_menu_main_popup_execute, pop_edit_move,      1, "geda-move",          N_("Move selection") },
  { N_("Mirror"),            x_menu_main_popup_execute, pop_edit_mirror,    1, "geda-mirror",        N_("Mirror an object about a point") },
  { N_("Rotate"),            x_menu_main_popup_execute, pop_edit_rotate,    1, "geda-rotate-left",   N_("Rotate the current selection about a point") },
  { N_("Snap"),              x_menu_main_popup_execute, pop_edit_snap,      1, "geda-snap",          N_("Snap objects to the current grid") },

  { "SEPARATOR",             NULL,                      0,                  0,  NULL,            NULL },

  /* Menu items for hierarchy added by SDB 1.9.2005. */

  { N_("Hierarchy"),         NULL,                      1,                  0,  NULL,             N_("Edit operations")},
  { N_("Down Schematic"),    x_menu_main_popup_execute, pop_down_schemat,   1, "gtk-go-down",     N_("Descend down in the schematic hierarchy")},
  { N_("Down Symbol"),       x_menu_main_popup_execute, pop_down_symbol,    1, "gtk-goto-bottom", N_("Descend down in the symbol hierarchy")},
  { N_("Up"),                x_menu_main_popup_execute, pop_hierarchy_up,   1, "gtk-go-up",       N_("ascend up in the schematic hierarchy")},

  /* Menu items for clip-board added by WEH 07.20.2013 */
  { "END_SUB",               NULL,                      0,                  0,  NULL,            NULL },
  { "SEPARATOR",             NULL,                      0,                  0,  NULL,            NULL },
  { N_("Cut to Clipboard"),  x_menu_main_popup_execute, pop_cb_cut,         1, "gtk-cut",        N_("Cut the current selection to the system clipboard") },
  { N_("Copy to Clipboard"), x_menu_main_popup_execute, pop_cb_copy,        1, "gtk-copy",       N_("Copy the current selection to the system clipboard") },
  { N_("Paste Clipboard"),   x_menu_main_popup_execute, pop_cb_paste,       1, "gtk-paste",      N_("Paste the contents of the system clipboard") },
  {NULL} /* sentinel */
};

static PopupEntry path_popup_items[] = {

  { N_("Close"),    x_menu_path_popup_execute,  pop_path_close,    1, "close-path",   N_("Close the current path") },
  { N_("Done"),     x_menu_path_popup_execute,  pop_path_done,     1, "process-stop", N_("Terminate the current path") },
  { N_("End"),      x_menu_path_popup_execute,  pop_path_end,      1, "gtk-quit",     N_("End this path and exit path mode") },

  { N_("In"),       x_menu_main_popup_execute,  pop_zoom_in,       1, "gtk-zoom-in",  N_("Increase the Zoom magnification") },
  { N_("Out"),      x_menu_main_popup_execute,  pop_zoom_out,      1, "gtk-zoom-out", N_("Decrease the Zoom magnification") },
  { N_("Extents"),  x_menu_main_popup_execute,  pop_zoom_extents,  1, "gtk-zoom-fit", N_("Zoom to the extents of the drawing") },

  { N_("Continue"), x_menu_path_popup_execute,  pop_path_continue, 1, "geda-path",    N_("Resume input of the current path") },
  { N_("Cancel"),   x_menu_path_popup_execute,  pop_path_cancel,   1, "gtk-cancel",   N_("Cancel path mode") },
  { N_("Undo"),     x_menu_path_popup_execute,  pop_path_undo,     1, "edit-undo",    N_("Undo the last path segment") },

  {NULL} /* sentinel */
};

/*! ToolBar Menu Strings, these must be in the same order
 *  as ID_GSCHEM_Toolbar in x_toolbars.h */
const char *IDS_Menu_Toolbar_Toggles[] = {
            IDS_MENU_TB_ADD,
            IDS_MENU_TB_ATTRIB,
            IDS_MENU_TB_EDIT,
            IDS_MENU_TB_GRID_SNAP,
            IDS_MENU_TB_PAGE,
            IDS_MENU_TB_SELECT,
            IDS_MENU_TB_STANDARD,
            IDS_MENU_TB_SYMBOL,
            IDS_MENU_TB_ZOOM,
            NULL
};

const char *IDS_Menu_Toggles[] = { /* temp Menu Toggle Strings*/
  "Snap On-Off", "Outline-Box", "Rubberband", "Magnetic",
   NULL
};

static int     show_recent_path;
static GList  *recent_files = NULL;
static GSList *ui_list      = NULL;

static void x_menu_toggle_icons        (GtkWidget *widget, GSList *list);
static void x_menu_toggle_tips         (GtkWidget *widget, GSList *list);
static void x_menu_toggle_main_tips    (GtkWidget *widget, GschemToplevel *w_current);
static void x_menu_update_recent_files (void);

/*!
 * \brief Execute Main Menu Selection
 * \par Function Description
 *  This retrieves the action from the menu items a checks if the
 *  action string is a valid command and if so, the action string
 *  is passed to i_command_process, otherwise the string is passed
 *  to g_action_eval_by_name for evaluation with guile.
 *
 * \remarks The process evaluated all of the action string with
 *  guile, resulting in c=>scheme=>c. This in efficiency and
 *  proved to be unstable.
 */
static void x_menu_execute(GedaAction *action, void *user_data)
{
  GschemToplevel *w_current    = (GschemToplevel*)user_data;
  const char     *action_name  = geda_action_get_action_name(action);
  char           *menu_action  = NULL;

  if (i_command_is_valid(action_name)) {

#if DEBUG
    fprintf(stderr, "Bypassing, guile for menu action %s\n",action_name);
#endif

    i_command_process(w_current, action_name, 0, NULL, ID_ORIGIN_MENU);
  }
  else {

    if (strncmp (action_name, "buffer-", 7) == 0 ) {

      /* Append the string with -menu, like "buffer-copy2-menu" */
      menu_action = geda_strconcat (action_name, "-menu", NULL);

      g_action_eval_by_name (w_current, menu_action);
      GEDA_FREE(menu_action);
    }
    else {

#if DEBUG
      fprintf(stderr, "passing action to guile %s\n", action_name);
#endif

      g_action_eval_by_name (w_current, action_name);
    }
  }
}

/*!
 * \brief  Execute Main Popup Menu Selection
 * \par Function Description
 *  This functions essentialy performs the same action as the preceeding
 *  main menu function but there is no Scheme involved and commands are
 *  known to be valid. The second argument to i_command_process is the
 *  action string referenced in the static string structure IDS_Popup_
 *  Actions using the enumerated integer from pop_MenuItem.
 */
static void x_menu_main_popup_execute(GtkWidget *widget, int action_id)
{
  GschemToplevel *w_current;
  const char     *action;

  w_current  = GEDA_OBJECT_GET_DATA(widget, "top-level");
  action     = IDS_Popup_Actions[action_id];

#if DEBUG
    fprintf(stderr, "<x_menu_main_popup_execute> procssing popup menu action %s\n",action);
#endif

  i_command_process(w_current, action, 0, NULL, ID_ORIGIN_MOUSE);
}

static void x_menu_torn(GedaMenuItem *menu_item, void *user_data)
{
  GschemToplevel *w_current = (GschemToplevel*)user_data;
  gtk_widget_grab_focus (w_current->drawing_area);
}

static void x_menu_path_popup_execute(GtkWidget *widget, int action_id)
{
  GschemToplevel *w_current;

  w_current  = GEDA_OBJECT_GET_DATA(widget, "top-level");

  switch (action_id) {
    case pop_path_close:
      o_path_close(w_current);
      /* Fall through */
    case pop_path_continue:
      break;

    case pop_path_undo:
      o_path_undo(w_current);
      break;

    case pop_path_done:
    case pop_path_end:
      if (w_current->temp_path != NULL) {
        o_path_end(w_current, w_current->second_wx, w_current->second_wy);
      }
      if (action_id == pop_path_done) {
        break;
      }
      /* Fall through */
    case pop_path_cancel:
    default:
      i_event_stop_action_handler(w_current);
      break;
  }
}

static void x_menu_free_toggler (void *data_record, void *nothing)
{
  ToggleMenuData *toggler_data = data_record;

  g_object_unref (toggler_data->action);

  g_free(toggler_data->toggle_name);
  g_free(toggler_data->menu_item_name);
  g_free(toggler_data->menu_path);
  g_free(toggler_data);
}

/*!
 * \brief Menu->Destroy->Toggle Items
 * \par Function Description
 *  This function is called by gschem_quit to free each ToggleMenuData
 *  structure that was allocated for toggle menu items.
 */
void x_menu_free_all(void)
{
  lambda (MenuData *menu_data) {

    g_slist_foreach (TOGGLERS_LIST, x_menu_free_toggler, NULL);
    g_slist_free (TOGGLERS_LIST);

    g_slist_free (MENU_ITEMS_LIST);

    /* List of both main and path popups */
    g_slist_free (POPUP_ITEMS_LIST);

    g_object_ref_sink(POPUP_PATH);
    g_object_unref(POPUP_PATH);

    g_hash_table_unref (POPUP_MAIN_HASH);
    g_object_ref_sink(POPUP_MAIN);
    g_object_unref(POPUP_MAIN);

    g_free(menu_data->buffer_menu_name);

    g_free(menu_data);

    return FALSE;
  }
  mapcar(ui_list);

  geda_iface_menu_free();
}

/*!
 * \brief Get Pointer to Main Menu Bar
 * \par Function Description
 * This function retrieves a pointer to the main menu bar.
 *
 * \retval GedaMenu pointer disguised as a GtkWidget
 */
GtkWidget *x_menu_get_main_menu(GschemToplevel *w_current)
{
  MenuData *menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
  return MENU_BAR;
}

/*!
 * \internal Helper common to the grid mode radio callbacks
 * The menu callbacks connected to the "toggled" signal do not
 * call this function if the radio was deactivated and public
 * x_menu_set_grid_radio, which gets called from action handlers
 * responding to the toolbar grid-radio group, blocks the signal
 * for the menu radio that is being set active so this function
 * is ONLY called when a grid mode menu toggle item is selected.
 */
static void
x_menu_set_grid_mode(GschemToplevel *w_current, int mode)
{
  w_current->grid_mode = mode;

  /* If toolbars are off then update manually */
  if (w_current->toolbars == FALSE) {
    x_grid_configure_variables (w_current);
    i_status_update_grid_info (w_current);
  }
  else {
    x_toolbars_set_grid_radio(w_current);
  }

  o_invalidate_all (w_current);
}

/*!
 * \internal Callback for grid dots mode radio "toggled" signal
 * Ignores the signal if the widget was toggled OFF, meaning,
 * one of the other radios is being toggled ON.
 */
static void
x_menu_grid_dots_mode(GedaCheckMenuItem *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(widget)) {
    x_menu_set_grid_mode(w_current, GRID_DOTS);
  }
}

/*!
 * \internal Callback for grid mesh mode radio "toggled" signal
 * Ignores the signal if the widget was toggled OFF, meaning,
 * one of the other radios is being toggled ON.
 */
static void
x_menu_grid_mesh_mode(GedaCheckMenuItem *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(widget)) {
    x_menu_set_grid_mode(w_current, GRID_MESH);
  }
}

/*!
 * \internal Callback for grid none mode radio "toggled" signal
 * Ignores the signal if the widget was toggled OFF, meaning,
 * one of the other radios is being toggled ON.
 */
static void
x_menu_grid_none_mode(GedaCheckMenuItem *widget, GschemToplevel *w_current)
{
  if (geda_check_menu_item_get_active(widget)) {
    x_menu_set_grid_mode(w_current, GRID_NONE);
  }
}

/* Callback for "Clear" popup menu on "Paste from" menu items.
 * Clears the buffer associated with the menu item and closes the menu.
 *
 * menuitem is the popup menu item, GedaMenuItem, that was clicked */
static void clear_buffer_popup_clicked (GtkWidget *menuitem, void *user_data)
{
  GschemToplevel *w_current = (GschemToplevel*)user_data;
  GtkWidget      *widget;
  const char     *label;

  widget = GEDA_OBJECT_GET_DATA(menuitem, "paste-menu-item");

  label = geda_menu_item_get_label ((GedaMenuItem*)widget);

  /* Extract the buffer # from the label */
  char alpha = label[strlen(label) - 1];

  if (isdigit(alpha)) {

    /* Clear the buffer, converting alpha to numeric */
    o_buffer_clear(w_current, alpha - 48);

    /* Update menu item sensitivity */
    gtk_widget_set_sensitive(widget, FALSE);

    if (!o_select_is_selection(w_current)) {

      GedaMenuShell *menu;

      /* Deactivate the widget's parent menu if nothing selected */
      menu = GEDA_MENU_SHELL(gtk_widget_get_parent(widget));

      geda_menu_shell_cancel(menu);
    }
  }

  /* Destroy the popup menu */
  gtk_widget_destroy(gtk_widget_get_parent(menuitem));
}

static bool on_paste_button_released (GtkWidget       *menuitem,
                                      GdkEventButton  *event,
                                      void            *w_current)
{
  bool ret_val;

  if (event->button == 3) {

    GtkWidget *menu;
    GtkWidget *popup_item;

    /* create a context menu */
    menu = geda_menu_new();

    popup_item = geda_menu_item_new_with_label (_("Clear"));

    g_signal_connect(popup_item, "activate",
                     G_CALLBACK(clear_buffer_popup_clicked),
                     w_current);

    GEDA_OBJECT_SET_DATA (popup_item, menuitem, "paste-menu-item");

    geda_menu_append (menu, popup_item);

    gtk_widget_show_all (menu);

    /* make menu a popup menu */
    geda_menu_popup (GEDA_MENU (menu), NULL, NULL, NULL, NULL,
                     event->button, gdk_event_get_time ((GdkEvent*)event));

    ret_val = TRUE;
  }
  else {
    ret_val = FALSE;
  }

  return ret_val;
}

static void x_menu_add_menu_popups(GschemToplevel *w_current, MenuData *menu_data)
{
  if (menu_data->buffer_menu_name) {

    GedaMenuItem  *rootitem;
    GedaMenuShell *menubar;
    GedaMenuItem  *menuitem;
    const GList   *children;

    menubar  = GEDA_MENU_SHELL(MENU_BAR);
    children = geda_menu_shell_get_children (menubar);
    rootitem = NULL;

    while (children) {

      const char *label;

      menuitem = children->data;

      label = geda_menu_item_get_label (menuitem);

      if (strcmp(label, menu_data->buffer_menu_name) == 0) {
        rootitem = menuitem;
        break;
      }

      children = children->next;
    }

    if (GEDA_IS_MENU_ITEM(rootitem)) {

      GtkWidget *submenu;

      submenu = geda_menu_item_get_submenu_widget (rootitem);

      if (GEDA_IS_MENU_SHELL(submenu)) {

        children = geda_menu_shell_get_children ((GedaMenuShell*)submenu);

        while (children) {

          menuitem = children->data;

          if (!GEDA_IS_MENU_SEPERATOR (menuitem)) {

            const char *label;

            label = geda_menu_item_get_label (menuitem);

            if (strstr(label, "Paste from")) {
              g_signal_connect (menuitem, "button-release-event",
                                G_CALLBACK (on_paste_button_released),
                                w_current);
            }
          }
          children = children->next;
        }
      }
    }
  }
}

/*!
 * \brief Create Main Menu
 * \par Function Description
 * This function creates the main menu based on data in a Scheme list that
 * was create during startup when RC files were executed. Presummably the
 * orginal intend was to allow for menu customization and this is possible.
 * A side-effects is that if there is a single error in the rc/scheme file,
 * the menu data does not get defined! If such an error is detected, a pre-
 * defined menu is constructed as a "fall-back" menu.
 */
GtkWidget *x_menu_setup_ui(GschemToplevel *w_current)
{
  EdaConfig  *cfg;
  GedaAction *action;
  GtkWidget  *image;
  GtkWidget  *menu_item;
  GtkWidget  *menu;

  SCM scm_items;
  SCM scm_item;
  SCM scm_item_name;
  SCM scm_item_tip;
  SCM scm_item_func;
  SCM scm_item_stock;
  SCM scm_index;

  char *dummy = NULL;

  const char  *group = MENU_CONFIG_GROUP;
        char  *menu_item_tip;
        char  *menu_item_stock;
        char  *raw_menu_item_name;
        char **raw_menu_name = &dummy;

  unsigned long handler;
  int i, j, menu_count;

  bool menus_broken = FALSE;   /* static for get_menu_item_from_scheme */

  bool show_menu_icons;
  bool show_menu_tips;
  bool show_pop_icons;
  bool show_pop_tips;

  GHashTable     *key_hash;
  ToggleMenuData *toggler_data;

  /* Glib-2.40 generates console noise from gtk-lib */
  MenuData *menu_data = GEDA_MEM_ALLOC0 (sizeof(MenuData));
  MENU_BAR            = geda_menu_bar_new ();
  MENU_ITEMS_LIST     = NULL;

  void menu_register_toggler (ToggleMenuData *toggler_data) {
    TOGGLERS_LIST = g_slist_append(TOGGLERS_LIST, toggler_data);
  }

  inline void setup_radio(GtkWidget *radio_button, void *func) {

    RadioMenuData *radio_data;

    radio_data            = GEDA_MEM_ALLOC0 (sizeof(RadioMenuData));
    radio_data->w_current = w_current;
    radio_data->widget    = (GedaCheckMenuItem*)radio_button;

    radio_data->handler   = GTK_CALLBACK_TOGGLED(radio_button, func, w_current);

    w_current->toolbar_mode_grp = g_slist_append (w_current->toolbar_mode_grp,
                                                  radio_data);
  }

  /* This function is used for recovery in the event of an error resulting in
   * the menu data not being loaded by scheme, the subfunction creates a basic
   * file menu with items for open, save and quit.
   */
  inline GtkWidget *create_file_menu(void) {

    GtkWidget *file_menu = geda_menu_new ();  /* Don't need to show menus */

    /* Create a Open menu items */
    action = geda_action_new ("file-open",                                   /* Action name */
                              "_Open...",                                    /* Text */
                            _("Open an existing schematic or symbol file"),  /* Tooltip */
                              "gtk-open",                                    /* Icon stock ID */
                              "F O");                                        /* Accelerator string */

    GtkWidget *open_item = geda_action_create_menu_item (action);
    g_signal_connect (action, "activate",
                      G_CALLBACK(x_menu_execute),  w_current);

    /* Create a Save menu items */
    action = geda_action_new ("file-save",                                   /* Action name */
                              "_Save",                                       /* Text */
                            _("Save the current document"),                  /* Tooltip */
                              "gtk-save",                                    /* Icon stock ID */
                              "F S");                                        /* Accelerator string */

    GtkWidget *save_item = geda_action_create_menu_item (action);
    g_signal_connect (action, "activate",
                      G_CALLBACK(x_menu_execute), w_current);

    action = geda_action_new ("file-quit",                                   /* Action name */
                              "_Quit",                                       /* Text */
                            _("Quit gschem and exit"),                       /* Tooltip */
                              "gtk-quit",                                    /* Icon stock ID */
                              "F Q");                                        /* Accelerator string */

    GtkWidget *quit_item = geda_action_create_menu_item (action);
    g_signal_connect (action, "activate",
                      G_CALLBACK(x_menu_execute), w_current);

    /* Add basic items to the file menu */
    geda_container_add (file_menu, open_item);
    g_object_set (open_item, "visible", TRUE, NULL);

    geda_container_add (file_menu, save_item);
    g_object_set (save_item, "visible", TRUE, NULL);

    geda_container_add (file_menu, quit_item);
    g_object_set (quit_item, "visible", TRUE, NULL);

    return file_menu;
  }

  /* This subfunction is used for recovery in the event of an error resulting
   * in menu data not being loaded by scheme, the function creates a toplevel
   * view menu with a single item for Redraw. Note that the parent function
   * will append other items under the View menu.
   */
  inline GtkWidget *create_View_menu(void) {

    GtkWidget *view_menu = geda_menu_new ();  /* Don't need to show menus */

    /* Create a Redraw menu item */
    action = geda_action_new ("view-redraw",                                  /* Action name */
                              "_Redraw",                                      /* Text */
                            _("redraw the current window"),                   /* Tooltip */
                              "gtk-refresh",                                  /* Icon stock ID */
                              "R");                                           /* Accelerator string */

    GtkWidget *redraw_item = geda_action_create_menu_item (action);

    handler = g_signal_connect (action, "activate",
                                G_CALLBACK(x_menu_execute),
                                w_current);


    /* Add the Redraw items to the View menu */
    geda_container_add (view_menu, redraw_item);
    GEDA_OBJECT_SET_DATA(MENU_BAR, redraw_item, "_View/_Redraw");
    g_object_set (redraw_item, "visible", TRUE, NULL);

    return view_menu;
  }

  /* Subfunction to extract menu item properties from scheme using
   * data at the given index and create a menu item widget */
  inline GtkWidget *get_menu_item_from_scheme (SCM scm_items, int index ) {

    GtkWidget *menu_item;
    char      *buf;
    int        scm_item_len;

    scm_dynwind_begin(0);

    scm_index      = scm_from_int (index);
    scm_item       = scm_list_ref (scm_items, scm_index);
    scm_item_len   = scm_ilength  (scm_item);
    scm_item_name  = SCM_CAR      (scm_item);
    scm_item_func  = SCM_CADR     (scm_item);
    scm_item_stock = scm_is_pair  (SCM_CDDR (scm_item)) ? SCM_CADDR (scm_item) : SCM_BOOL_F;

    /* Check the first member */
    if (!scm_is_string(scm_item_name)) {

      const char *err_msg = _("Error reading menu item");
      const char *bad_str = _("Bad string");

      if (!menus_broken) { /* Issue message only for first occurence */
        fprintf(stderr, "%s <%d>: %s\n", err_msg, i, bad_str);
      }
      else {
        u_log_message("%s <%d>: %s\n", err_msg, i, bad_str);
      }
      menus_broken = TRUE;
      menu_item    = NULL;
    }
    else {

      SCM_ASSERT(scm_is_symbol (scm_item_func) || scm_is_false (scm_item_func),
                 scm_item_func, SCM_ARGn, "x_menu_get_main_menu item_func");

      SCM_ASSERT(scm_is_string (scm_item_stock) || scm_is_false (scm_item_stock),
                 scm_item_stock, SCM_ARGn, "x_menu_get_main_menu stock");

      /* Check for a 4th parameter = tooltip string */
      if (scm_item_len == 4) {

        scm_item_tip = SCM_CAR (scm_cdddr (scm_item ));      /* Extract tooltip string */

        if (scm_is_string(scm_item_tip)) {                   /* Validate tip is really a string */
          menu_item_tip = scm_to_utf8_string (scm_item_tip); /* if valid, convert to c string */
        }
        else {
          menu_item_tip = NULL;
        }
      }
      else {
        menu_item_tip = NULL;
      } /* End tool-tip retrieval */

      raw_menu_item_name = scm_to_utf8_string(scm_item_name);

      scm_dynwind_free(raw_menu_item_name);

      if (strncmp(raw_menu_item_name, "SEPARATOR", 9) == 0) {
        menu_item = geda_menu_separator_new();
      }
      else {

        menu_item_stock = scm_is_false (scm_item_stock) ? NULL : scm_to_utf8_string (scm_item_stock);

        if (scm_is_false (scm_item_func)) { /* Then is a nested menu item */

          if (menu_item_stock) {            /* Nested menus can have icons */

            /* Items that might fall into this category are; Open Recen_t, _Export,
             * and _Restore, these are actionless items.
             */

            menu_item = geda_image_menu_item_new_with_mnemonic(raw_menu_item_name);

            if (strncmp(menu_item_stock, "gtk-",4) == 0) {

              /* Pre Gtk-2.6 */
              //image =  (GtkWidget*)gtk_image_new_from_stock(menu_item_stock, GTK_ICON_SIZE_MENU);

              image = gtk_image_new_from_icon_name (menu_item_stock, GTK_ICON_SIZE_MENU);
            }
            else {
              image = x_icons_get_factory_icon (menu_item_stock, GTK_ICON_SIZE_MENU);
            }

            g_object_set (image,
                          "no-show-all", TRUE,
                          "visible", show_menu_icons,
                          NULL);

            g_object_set (menu_item,
                          "image", image,
                          "show-image", show_menu_icons,
                          NULL);
          }
          else {
            /* Note: Currently there are no non-stock, non-action items */
            menu_item = geda_menu_item_new_with_mnemonic(raw_menu_item_name);
          }

          if (menu_item) {
            MENU_ITEMS_LIST = g_slist_append(MENU_ITEMS_LIST, menu_item);
          }
        }
        else {
                char *action_name;
                char *action_keys;
          const char *menu_icon_name;
          const char *menu_item_name;
                bool  is_a_toggle;

          action_name = scm_to_utf8_string (scm_symbol_to_string (scm_item_func));
          action_keys = g_hash_table_lookup (key_hash, action_name);

          if (!menu_data->buffer_menu_name) {

            if (strncmp(action_name, "buffer-copy1", 12) == 0) {
              /* Save a copy of the raw string for x_menu_get_buffer_menu*/
              menu_data->buffer_menu_name = geda_strdup(*raw_menu_name);
            }
          }

          if (!menu_item_stock) {
            /* Check if an icon is associated with the action */
            menu_icon_name = i_command_get_action_icon (action_name);
          }
          else {
            menu_icon_name = menu_item_stock; /* Icons specified in menu take presendence */
          }

          is_a_toggle = FALSE;

          if (strncmp (raw_menu_item_name, "Toggle", 6) == 0 ) {

            /* Skip over the word "Toggle" and the space after */
            menu_item_name = gettext(raw_menu_item_name + 7);

            is_a_toggle = TRUE;
            toggler_data                 = GEDA_MEM_ALLOC0(sizeof(ToggleMenuData));
            toggler_data->w_current      = w_current;
            toggler_data->menu_item_name = geda_strdup(menu_item_name);
            toggler_data->menu_path      = geda_strconcat (*raw_menu_name, "/", raw_menu_item_name, NULL);

            /* TODO: Tooltips do not work here, we will fix them later*/
            action = (GedaAction*)
            geda_toggle_action_new (action_name,       /* Action name */
                                    menu_item_name,    /* Text */
                                    menu_item_tip ? menu_item_tip : menu_item_name,
                                    menu_icon_name,    /* Icon stock ID */
                                    action_keys);      /* Accelerator string */
            toggler_data->action = (GtkWidget*)action;
          }
          else {

            menu_item_name = gettext(raw_menu_item_name);

            action = geda_action_new (action_name,     /* Action name */
                                      menu_item_name,  /* Text */
                                      menu_item_tip ? menu_item_tip : menu_item_name,  /* Tooltip */
                                      menu_icon_name,  /* Icon stock ID */
                                      action_keys);    /* Accelerator string */
          }

          /* Cast is for GedaToggleAction items */
          menu_item = geda_action_create_menu_item ((GedaAction*)action);

          free(action_name);

          handler = g_signal_connect (action, "activate",
                                      G_CALLBACK(x_menu_execute),
                                      w_current);

          if (is_a_toggle) {
            toggler_data->handler = handler;     /* Save handler ID of toggle items */
            menu_register_toggler(toggler_data); /* Appends the struct to a list */
          }
          else {
            /* save all non-toggle menu items to a single linked list */
            MENU_ITEMS_LIST = g_slist_append(MENU_ITEMS_LIST, menu_item);
            /* Kind of set the visibilty of the icon image */
            g_object_set (menu_item, "show-image", show_menu_icons, NULL);
          }
        }

        /* If tip not NULL then attach tip to menu widget */
        if (menu_item_tip) {
          gtk_widget_set_tooltip_text(menu_item, _(menu_item_tip));
          g_object_set (menu_item, "has-tooltip", show_menu_tips, NULL);
          free(menu_item_tip);
        }

        if (menu_item_stock) {
          free(menu_item_stock);
        }
      }
    }

    /* add a handle to the menu_bar object to get access to widget
     * objects. This string should NOT be internationalized */
    buf = geda_sprintf("%s/%s", *raw_menu_name, raw_menu_item_name);
    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_item, buf);
    GEDA_FREE(buf);

    scm_dynwind_end();

    return menu_item;
  }

  /* -------------------------------------------------------------- */

  /* private */

  cfg = eda_config_get_user_context ();

  show_menu_icons  = eda_config_get_boolean (cfg, group, "show-menu-icons",  NULL);
  show_menu_tips   = eda_config_get_boolean (cfg, group, "show-menu-tips",   NULL);
  show_pop_icons   = eda_config_get_boolean (cfg, group, "show-popup-icons", NULL);
  show_pop_tips    = eda_config_get_boolean (cfg, group, "show-popup-tips",  NULL);
  show_recent_path = eda_config_get_boolean (cfg, group, "show-recent-path", NULL);

  toggler_data = NULL;
  key_hash = g_keys_to_new_hash_table();

  scm_dynwind_begin (0);
  g_dynwind_window (w_current);

  menu_count = geda_iface_menu_return_num();

  /* Loop through all top-level menu container */
  for (i = 0 ; i < menu_count; i++) {

    GtkWidget *root_menu = NULL;
    char      *menu_name;
    int        scm_items_len;

    scm_items = geda_iface_menu_return_entry(i, raw_menu_name);

    if (*raw_menu_name == NULL) {
      fprintf(stderr, "%s: %s\n", __func__, _("Oops... got a NULL menu name"));
      return NULL;
    }

    menu_name = (char*)gettext(*raw_menu_name);

    /* Glib-2.40 generates console noise from gtk-lib */
    menu = geda_menu_new();

    menu_item = geda_tearoff_menu_item_new ();
    geda_container_add(menu, menu_item);
    g_object_set (menu_item, "visible", TRUE, NULL);
    g_signal_connect(menu_item, "torn-off", G_CALLBACK(x_menu_torn), w_current);


    /* Loop through all items subordinate to this top-level menu container */
    scm_items_len = (int) scm_ilength (scm_items);

    for (j = 0 ; j < scm_items_len; j++) {

      menu_item = get_menu_item_from_scheme(scm_items, j);
      geda_container_add(menu, menu_item);
      g_object_set (menu_item, "visible", TRUE, NULL);
    }

    if (strstr(menu_name, "/")) {
      root_menu = GEDA_OBJECT_GET_DATA (MENU_BAR, menu_name);
    }

    if (root_menu == NULL) {
      root_menu = geda_menu_item_new_with_mnemonic (menu_name);
      geda_container_add(MENU_BAR, root_menu);
    }

    geda_menu_item_set_submenu_widget ((GedaMenuItem*)root_menu, menu);

    g_object_set (root_menu, "visible", TRUE, NULL);

    /* Do not free *raw_menu_name */
  }
  scm_dynwind_end ();

  g_hash_table_destroy (key_hash);

  /* Ensure File menu item exist, if not then create the item */

  menu_item = GEDA_OBJECT_GET_DATA (MENU_BAR, "_File/_Save");

  if (menu_item == NULL) {

    menu = create_file_menu();

    menu_item = geda_menu_item_new_with_label ("_File");
    g_object_set (menu_item, "visible", TRUE, NULL);;

    geda_menu_item_set_submenu_widget ((GedaMenuItem*)menu_item, menu);
    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_item, "_File");
    geda_menu_bar_append ((GedaMenuBar*)MENU_BAR, menu_item);
  }

  /* Ensure View menu item exist, if not then create the item */

  menu_item = GEDA_OBJECT_GET_DATA (MENU_BAR, "_View/_Redraw");

  if (menu_item == NULL) {

    menu = create_View_menu();

    menu_item = geda_menu_item_new_with_label (_("_View"));
    g_object_set (menu_item, "visible", TRUE, NULL);;

    geda_menu_item_set_submenu_widget ((GedaMenuItem*)menu_item, menu);
    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_item, "_View");
    geda_menu_bar_append ((GedaMenuBar*)MENU_BAR, menu_item);
  }

  menu_item = GEDA_OBJECT_GET_DATA (MENU_BAR, "_View/_Redraw");

  if (menu_item != NULL) {

    GedaMenuShell *menu_shell = (GedaMenuShell*)gtk_widget_get_parent (menu_item);
    GtkWidget     *toggle_menu;

    /* Grid Options*/
    menu_item   = geda_menu_item_new_with_mnemonic(_("_Grid"));
    toggle_menu = geda_menu_new();

    geda_menu_item_set_submenu_widget ((GedaMenuItem*)menu_item, toggle_menu);
    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_item, IDS_MENU_VIEW_GRID);

    GSList *grp = NULL;

    /* Start View Grid Radios */
    GtkWidget *vw_dots_radio = geda_radio_menu_item_new_with_mnemonic (grp, "_Dots");
    GtkWidget *vw_mesh_radio = geda_radio_menu_item_new_with_mnemonic_from_widget (vw_dots_radio, "_Mesh");
    GtkWidget *vw_none_radio = geda_radio_menu_item_new_with_mnemonic_from_widget (vw_mesh_radio, "_None");

    if (w_current->grid_mode == GRID_NONE ) {
      g_object_set (vw_none_radio, "active", TRUE,  NULL);
    }
    else {
      if (w_current->grid_mode ==  GRID_DOTS) {
        g_object_set (vw_dots_radio,  "active", TRUE,  NULL);
      }
      else {
        if (w_current->grid_mode == GRID_MESH ) {
          g_object_set (vw_mesh_radio,  "active", TRUE,  NULL);
        }
      }
    }

    GEDA_OBJECT_SET_DATA(MENU_BAR, vw_dots_radio, OPT_GRID_DOTS_MENU_PATH);
    GEDA_OBJECT_SET_DATA(MENU_BAR, vw_mesh_radio, OPT_GRID_MESH_MENU_PATH);
    GEDA_OBJECT_SET_DATA(MENU_BAR, vw_none_radio, OPT_GRID_NONE_MENU_PATH);

    geda_container_add(toggle_menu, vw_dots_radio);
    geda_container_add(toggle_menu, vw_mesh_radio);
    geda_container_add(toggle_menu, vw_none_radio);

    gtk_widget_set_tooltip_text(vw_dots_radio, _("Display dots grid"));
    gtk_widget_set_tooltip_text(vw_mesh_radio, _("Display mesh grid"));
    gtk_widget_set_tooltip_text(vw_none_radio, _("Turn off the grid display"));

    geda_menu_shell_prepend(menu_shell, menu_item);

    GTK_CALLBACK_TOGGLED(vw_dots_radio, x_menu_grid_dots_mode, w_current);
    GTK_CALLBACK_TOGGLED(vw_mesh_radio, x_menu_grid_mesh_mode, w_current);
    GTK_CALLBACK_TOGGLED(vw_none_radio, x_menu_grid_none_mode, w_current);

    gtk_widget_set_has_tooltip (vw_dots_radio, show_menu_tips);
    gtk_widget_set_has_tooltip (vw_mesh_radio, show_menu_tips);
    gtk_widget_set_has_tooltip (vw_none_radio, show_menu_tips);

    gtk_widget_show_all(menu_item);

    GRID_RADIO_LIST = geda_radio_menu_item_group(vw_none_radio);

    if (w_current->toolbars == TRUE) {

      /* Toolbar Options*/
      menu_item   = geda_menu_item_new_with_mnemonic(_("_Toolbars"));
      toggle_menu = geda_menu_new();

      geda_menu_item_set_submenu_widget ((GedaMenuItem*)menu_item, toggle_menu);
      GEDA_OBJECT_SET_DATA(MENU_BAR, menu_item, IDS_MENU_VIEW_TOOLBARS);

      GtkWidget *stdbar_toggle   = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_STANDARD);
      GtkWidget *selbar_toggle   = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_SELECT);
      GtkWidget *pagebar_toggle  = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_PAGE);
      GtkWidget *addbar_toggle   = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_ADD);
      GtkWidget *zoombar_toggle  = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_ZOOM);
      GtkWidget *symbar_toggle   = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_SYMBOL);
      GtkWidget *editbar_toggle  = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_EDIT);
      GtkWidget *attribar_toggle = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_ATTRIB);
      GtkWidget *gridbar_toggle  = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_GRID_SNAP);

      geda_check_menu_item_set_active((GedaCheckMenuItem*)stdbar_toggle,   TRUE);
      geda_check_menu_item_set_active((GedaCheckMenuItem*)selbar_toggle,   TRUE);
      geda_check_menu_item_set_active((GedaCheckMenuItem*)pagebar_toggle,  TRUE);
      geda_check_menu_item_set_active((GedaCheckMenuItem*)addbar_toggle,   TRUE);
      geda_check_menu_item_set_active((GedaCheckMenuItem*)zoombar_toggle,  TRUE);
      geda_check_menu_item_set_active((GedaCheckMenuItem*)symbar_toggle,   TRUE);
      geda_check_menu_item_set_active((GedaCheckMenuItem*)editbar_toggle,  TRUE);
      geda_check_menu_item_set_active((GedaCheckMenuItem*)attribar_toggle, TRUE);
      geda_check_menu_item_set_active((GedaCheckMenuItem*)gridbar_toggle,  TRUE);

      /* Normally the ui manager would do this for us but we don't have one so...*/
      GEDA_OBJECT_SET_DATA(MENU_BAR, stdbar_toggle,   OPT_STDBAR_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, selbar_toggle,   OPT_SELBAR_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, pagebar_toggle,  OPT_PageBAR_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, addbar_toggle,   OPT_ADDBAR_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, zoombar_toggle,  OPT_ZOOMBAR_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, symbar_toggle,   OPT_SYMBAR_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, editbar_toggle,  OPT_EDITBAR_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, attribar_toggle, OPT_ATTRBAR_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, gridbar_toggle,  OPT_GRIDBAR_MENU_PATH);

      geda_container_add(toggle_menu, stdbar_toggle);
      geda_container_add(toggle_menu, selbar_toggle);
      geda_container_add(toggle_menu, pagebar_toggle);
      geda_container_add(toggle_menu, addbar_toggle);
      geda_container_add(toggle_menu, zoombar_toggle);
      geda_container_add(toggle_menu, symbar_toggle);
      geda_container_add(toggle_menu, editbar_toggle);
      geda_container_add(toggle_menu, attribar_toggle);
      geda_container_add(toggle_menu, gridbar_toggle);

      gtk_widget_set_tooltip_text(stdbar_toggle, _("Toggle visibility of the Standard toolbar"));
      gtk_widget_set_tooltip_text(selbar_toggle, _("Toggle visibility of the Selection toolbar"));
      gtk_widget_set_tooltip_text(pagebar_toggle, _("Toggle visibility of the Page toolbar"));
      gtk_widget_set_tooltip_text(addbar_toggle,  _("Toggle visibility of the Add toolbar"));
      gtk_widget_set_tooltip_text(zoombar_toggle,  _("Toggle visibility of the Zoom toolbar"));
      gtk_widget_set_tooltip_text(symbar_toggle,   _("Toggle visibility of the Symbol toolbar"));
      gtk_widget_set_tooltip_text(editbar_toggle,  _("Toggle visibility of the Edit toolbar"));
      gtk_widget_set_tooltip_text(attribar_toggle,  _("Toggle visibility of the Attributes toolbar"));
      gtk_widget_set_tooltip_text(gridbar_toggle,   _("Toggle visibility of the Grid/Snap toolbar"));

      gtk_widget_set_has_tooltip (stdbar_toggle, show_menu_tips);
      gtk_widget_set_has_tooltip (selbar_toggle, show_menu_tips);
      gtk_widget_set_has_tooltip (pagebar_toggle, show_menu_tips);
      gtk_widget_set_has_tooltip (addbar_toggle, show_menu_tips);
      gtk_widget_set_has_tooltip (zoombar_toggle, show_menu_tips);
      gtk_widget_set_has_tooltip (symbar_toggle, show_menu_tips);
      gtk_widget_set_has_tooltip (editbar_toggle, show_menu_tips);
      gtk_widget_set_has_tooltip (attribar_toggle, show_menu_tips);
      gtk_widget_set_has_tooltip (gridbar_toggle, show_menu_tips);

      GtkWidget *tb_separator  = geda_menu_item_new();
      geda_container_add(toggle_menu, tb_separator);

      GtkWidget *tb_tips_toggle = geda_check_menu_item_new_with_mnemonic (IDS_MENU_TB_TIPS);

      g_object_set (tb_tips_toggle, "draw-as-radio", TRUE,  NULL);

      geda_check_menu_item_set_active((GedaCheckMenuItem*)tb_tips_toggle, w_current->show_toolbar_tips);

      GEDA_OBJECT_SET_DATA(MENU_BAR, tb_tips_toggle, OPT_BAR_TIPS_MENU_PATH);

      geda_container_add(toggle_menu, tb_tips_toggle);

      gtk_widget_set_tooltip_text(tb_tips_toggle, _("Toggle visibility of tooltips on toolbars"));

      gtk_widget_set_has_tooltip (tb_tips_toggle, show_menu_tips);

      GTK_CALLBACK_TOGGLED (tb_tips_toggle, x_window_toolbar_tips_toggle, w_current);

      tb_separator  = geda_menu_item_new();
      geda_container_add(toggle_menu, tb_separator);

      /* Start Toolbar Mode Radios */
      GtkWidget *tb_icons_bulb = geda_check_menu_item_new_with_mnemonic ("_Icons");
      GtkWidget *tb_text_bulb  = geda_check_menu_item_new_with_mnemonic ("_Text");
      GtkWidget *tb_vert_bulb  = geda_check_menu_item_new_with_mnemonic ("Both _Vertical");
      GtkWidget *tb_hori_bulb  = geda_check_menu_item_new_with_mnemonic ("Both _Horizontal");

      g_object_set (tb_icons_bulb, "draw-as-radio", TRUE, NULL);
      g_object_set (tb_text_bulb,  "draw-as-radio", TRUE, NULL);
      g_object_set (tb_vert_bulb,  "draw-as-radio", TRUE, NULL);
      g_object_set (tb_hori_bulb,  "draw-as-radio", TRUE, NULL);

      g_object_set (tb_icons_bulb, "active", FALSE, NULL);
      g_object_set (tb_text_bulb,  "active", FALSE, NULL);
      g_object_set (tb_vert_bulb,  "active", FALSE, NULL);
      g_object_set (tb_hori_bulb,  "active", FALSE, NULL);

      if (w_current->toolbars_mode == TOOLBAR_SHOW_ICONS ) {
        g_object_set (tb_icons_bulb, "active", TRUE,  NULL);
      }
      else {
        if (w_current->toolbars_mode == TOOLBAR_SHOW_TEXT ) {
          g_object_set (tb_text_bulb,  "active", TRUE,  NULL);
        }
        else {
          if (w_current->toolbars_mode == TOOLBAR_SHOW_BOTH ) {
            g_object_set (tb_vert_bulb,  "active", TRUE,  NULL);
          }
          else {
            if (w_current->toolbars_mode == TOOLBAR_SHOW_HORIZ ) {
              g_object_set (tb_hori_bulb,  "active", TRUE,  NULL);
            }
          }
        }
      }

      setup_radio(tb_icons_bulb, x_toolbar_icons_only);
      setup_radio(tb_text_bulb,  x_toolbar_text_only);
      setup_radio(tb_vert_bulb,  x_toolbar_display_both);
      setup_radio(tb_hori_bulb,  x_toolbar_display_horiz);

      GEDA_OBJECT_SET_DATA(MENU_BAR, tb_icons_bulb, OPT_BAR_ICON_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, tb_text_bulb,  OPT_BAR_TEXT_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, tb_vert_bulb,  OPT_BAR_VERT_MENU_PATH);
      GEDA_OBJECT_SET_DATA(MENU_BAR, tb_hori_bulb,  OPT_BAR_HOZI_MENU_PATH);

      geda_container_add(toggle_menu, tb_icons_bulb);
      geda_container_add(toggle_menu, tb_text_bulb);
      geda_container_add(toggle_menu, tb_vert_bulb);
      geda_container_add(toggle_menu, tb_hori_bulb);

      gtk_widget_set_tooltip_text(tb_icons_bulb, _("Display Icons on the toolbar"));
      gtk_widget_set_tooltip_text(tb_text_bulb,  _("Display Text on the toolbar"));
      gtk_widget_set_tooltip_text(tb_vert_bulb,  _("Display Icons and Text vertically on the toolbar"));
      gtk_widget_set_tooltip_text(tb_hori_bulb,  _("Display Icons and Text horizontally on the toolbar"));

      gtk_widget_set_has_tooltip (tb_icons_bulb, show_menu_tips);
      gtk_widget_set_has_tooltip (tb_text_bulb, show_menu_tips);
      gtk_widget_set_has_tooltip (tb_vert_bulb, show_menu_tips);
      gtk_widget_set_has_tooltip (tb_hori_bulb, show_menu_tips);

      geda_menu_shell_prepend(menu_shell, menu_item);

      GTK_CALLBACK_TOGGLED (stdbar_toggle,   x_window_standard_toolbar_toggle,  w_current);
      GTK_CALLBACK_TOGGLED (selbar_toggle,   x_window_select_toolbar_toggle,    w_current);
      GTK_CALLBACK_TOGGLED (pagebar_toggle,  x_window_page_toolbar_toggle,      w_current);
      GTK_CALLBACK_TOGGLED (addbar_toggle,   x_window_add_toolbar_toggle,       w_current);
      GTK_CALLBACK_TOGGLED (zoombar_toggle,  x_window_zoom_toolbar_toggle,      w_current);
      GTK_CALLBACK_TOGGLED (symbar_toggle,   x_window_symbol_toolbar_toggle,    w_current);
      GTK_CALLBACK_TOGGLED (editbar_toggle,  x_window_edit_toolbar_toggle,      w_current);
      GTK_CALLBACK_TOGGLED (attribar_toggle, x_window_attribute_toolbar_toggle, w_current);
      GTK_CALLBACK_TOGGLED (gridbar_toggle,  x_window_gridsnap_toolbar_toggle,  w_current);

      gtk_widget_show_all(menu_item);
    }

    /* Menu Options */
    menu_item   = geda_menu_item_new_with_mnemonic("_Menu");
    toggle_menu = geda_menu_new();

    geda_menu_item_set_submenu_widget ((GedaMenuItem*)menu_item, toggle_menu);
    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_item, IDS_MENU_VIEW_MENU);

    GtkWidget *menu_icons_toggle   = geda_check_menu_item_new_with_mnemonic ("_Icons");
    GtkWidget *menu_tips_toggle    = geda_check_menu_item_new_with_mnemonic ("_ToolTips");
    GtkWidget *menu_popcons_toggle = geda_check_menu_item_new_with_mnemonic ("_Context Icons");
    GtkWidget *menu_poptips_toggle = geda_check_menu_item_new_with_mnemonic ("Context Tip_s");

    g_object_set (menu_icons_toggle,   "draw-as-radio", TRUE, NULL);
    g_object_set (menu_tips_toggle,    "draw-as-radio", TRUE, NULL);
    g_object_set (menu_popcons_toggle, "draw-as-radio", TRUE, NULL);
    g_object_set (menu_poptips_toggle, "draw-as-radio", TRUE, NULL);

    geda_check_menu_item_set_active((GedaCheckMenuItem*)menu_icons_toggle,   show_menu_icons);
    geda_check_menu_item_set_active((GedaCheckMenuItem*)menu_tips_toggle,    show_menu_tips);
    geda_check_menu_item_set_active((GedaCheckMenuItem*)menu_popcons_toggle, show_pop_icons);
    geda_check_menu_item_set_active((GedaCheckMenuItem*)menu_poptips_toggle, show_pop_tips);

    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_icons_toggle,   OPT_ICON_MENU_PATH);
    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_tips_toggle,    OPT_TIPS_MENU_PATH);
    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_popcons_toggle, OPT_POPCONS_MENU_PATH);
    GEDA_OBJECT_SET_DATA(MENU_BAR, menu_poptips_toggle, OPT_POPTIPS_MENU_PATH);

    geda_container_add(toggle_menu, menu_icons_toggle);
    geda_container_add(toggle_menu, menu_tips_toggle);
    geda_container_add(toggle_menu, menu_popcons_toggle);
    geda_container_add(toggle_menu, menu_poptips_toggle);

    gtk_widget_set_tooltip_text(menu_icons_toggle, _("Toggle visibility of main menu icons"));
    gtk_widget_set_tooltip_text(menu_tips_toggle,  _("Toggle main menu tooltips"));
    gtk_widget_set_tooltip_text(menu_popcons_toggle, _("Toggle visibility of main context menu icons"));
    gtk_widget_set_tooltip_text(menu_poptips_toggle, _("Toggle main context menu tooltips"));

    gtk_widget_set_has_tooltip (menu_icons_toggle, show_menu_tips);
    gtk_widget_set_has_tooltip (menu_tips_toggle, show_menu_tips);
    gtk_widget_set_has_tooltip (menu_popcons_toggle, show_menu_tips);
    gtk_widget_set_has_tooltip (menu_poptips_toggle, show_menu_tips);

    geda_menu_shell_prepend(menu_shell, menu_item);

    GTK_CALLBACK_TOGGLED (menu_icons_toggle, x_menu_toggle_icons, MENU_ITEMS_LIST);
    GTK_CALLBACK_TOGGLED (menu_tips_toggle, x_menu_toggle_main_tips, w_current);

    gtk_widget_show_all(menu_item);
  }
  else {
    fprintf(stderr, "No Menu!\n");
  }

  x_menu_add_menu_popups(w_current, menu_data);

  ui_list = g_slist_append(ui_list, menu_data);
  w_current->ui_index = g_slist_length(ui_list) -1;
  return MENU_BAR;
}

/** \defgroup Path-Context-Menu Path Context Menu Functions
 *  @{
 *  \brief Contains functions to create and support the Path Popup Mouse Menu
 *  \ingroup menu-module
 */

/*!
 * \brief Create and Setup Popup Mouse Menu for Path mode
 * \par Function Description
 *  This function is create the a context menu that is displayed when
 *  the user right-clicks while inputing a new path object.
 *
 *  \param [in] w_current      Pointer to GschemToplevel object
 *  \param [in] menu_data      accessed using POPUP_ITEMS_LIST
 *  \param [in] show_pop_icons determines visibility of menu icons
 *  \param [in] show_pop_tips  determines visibility of menu tooltips
 */
static GtkWidget*
x_menu_build_path_popup(GschemToplevel *w_current, MenuData *menu_data,
                                                   bool show_pop_icons,
                                                   bool show_pop_tips)
{
  GtkWidget *menu;
  int i;

  menu = geda_menu_new();

  for (i = 0; path_popup_items[i].name != NULL; i++) {

    GtkWidget *menu_item;
    GtkWidget *image;

    PopupEntry item = path_popup_items[i];

    menu_item = geda_image_menu_item_new_with_label(_(item.name));

    gtk_widget_set_tooltip_text (menu_item, _(item.tip));
    g_object_set (menu_item, "has-tooltip", show_pop_tips, NULL);

    if (item.use_stock) {
      image = gtk_image_new_from_stock(item.icon, GTK_ICON_SIZE_MENU);
    }
    else {
      image = create_pixmap (item.icon);
    }

    geda_image_menu_item_set_image (GEDA_IMAGE_MENU_ITEM(menu_item), image);

    /* Enable icon visibility based on the current setting */
    g_object_set (image, "visible", show_pop_icons, NULL);
    g_object_set (menu_item, "show-image", show_pop_icons, NULL);

    GEDA_OBJECT_SET_DATA(menu_item, w_current, "top-level");

    POPUP_ITEMS_LIST = g_slist_append (POPUP_ITEMS_LIST, menu_item);

    /* Connect things up so that the actions get run */
    g_signal_connect (menu_item, "activate",
                      (void*) item.func,
                      (void*)(long)item.action_id);


    g_object_set (menu_item, "visible", TRUE, NULL);
    geda_container_add(menu, menu_item);
  }

  return (menu);
}

int
x_menu_display_path_popup (GschemToplevel *w_current, GdkEventButton *event)
{
  GtkWidget *menu;
  MenuData  *menu_data;

  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  menu = POPUP_PATH;

  if (!GEDA_IS_MENU (menu)) {
    BUG_MSG ("path popup menu is NULL");
  }
  else {
    w_current->pointer_sx = event->x;
    w_current->pointer_sy = event->y;
    geda_menu_popup ((GedaMenu*)menu, NULL, NULL, NULL, NULL,
                    event->button, event->time);
  }
  return FALSE;
}

/** @} END Group Path-Context-Menu */

/** \defgroup Main-Context-Menu Main Context Popup Menu
 *  @{
 *  \brief Contains functions to create and support the Main Popup Mouse Menu
 *  \ingroup menu-module
 */

static bool strhashcmp (const char *a, const char *b) {

  int answer;

  if ((a[0] != '\0') && (b[0] != '\0')) {
     answer = strcmp (a, b) == 0;
  }
  else {
     answer = 0;
  }

  return answer;
}

/*!
 * \brief Setup Popup Context Menus
 * \par Function Description
 *  Creates the main context pop-up menu and connects callback to options in
 *  the main menu to control icons and tool-tip visibility. The pop-up menu
 *  is created using the data in the main_popup_items data structure. A pointer
 *  to each menu-item widget is saved in the single-linked list menu_data->
 *  main_popup_items using the macro POPUP_ITEMS_LIST. The POPUP_ITEMS_LIST list
 *  is used to toggle visibility of icon images and tool-tip on all of the
 *  pop-up menu items. The name of each item in main_popup_items is add to a hash
 *  table with a pointer to the widget. The hash table is referenced later
 *  when enabling/disabling sensitivities.
 *  A pointer to the menu is saved in menu_data->popup_menu using the macro
 *  POPUP_MAIN.
 *
 * \returns TRUE on success, other FALSE;.
 */
int x_menu_setup_popup (GschemToplevel *w_current)
{
  EdaConfig  *cfg;
  const char *group = MENU_CONFIG_GROUP;
  GtkWidget  *menu;
  GtkWidget  *menu_item;
  GtkWidget  *submenu;
  GtkWidget  *save_nest;
  GtkWidget  *image;
  MenuData   *menu_data;

  bool show_pop_icons;
  bool show_pop_tips;

  int i;

  /* We will assume the main menu has already allocated a structure */
  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  menu             = geda_menu_new ();
  POPUP_ITEMS_LIST = NULL;
  save_nest        = NULL;
  POPUP_MAIN_HASH  = g_hash_table_new_full (g_str_hash, (GEqualFunc) strhashcmp,
                                            NULL, NULL);

  /* Retrieve settings for user preferences */
  cfg              = eda_config_get_user_context ();
  show_pop_icons   = eda_config_get_boolean (cfg, group, "show-popup-icons", NULL);
  show_pop_tips    = eda_config_get_boolean (cfg, group, "show-popup-tips",  NULL);

  for (i = 0; main_popup_items[i].name != NULL; i++) {

    PopupEntry item = main_popup_items[i];

    if (item.func == NULL) { /* if sub-menu or seperator */

      /* Then is not an action item */
      if (item.action_id == 1) {

        /* Create and add the pop-out submenu item, note that the
         * sub-menus do not have icon images */
        submenu = geda_menu_item_new_with_label(_(item.name));
        g_object_set (submenu, "visible", TRUE, NULL);
        geda_container_add(menu, submenu);

        /* Save the current menu and create the new sub menu */
        save_nest = menu;
        menu = geda_menu_new ();

        geda_menu_item_set_submenu_widget ((GedaMenuItem*)submenu, menu) ;
        g_object_set (menu, "visible", TRUE, NULL);

        g_hash_table_insert (POPUP_MAIN_HASH, (char*)item.name, submenu);

#if DEBUG
        fprintf(stderr, "%s: submenu <%s> <%p>\n", __func__, item.name, submenu);
#endif

        continue;
      }
      else {
        if (save_nest != NULL) {
          menu = save_nest;
          save_nest = NULL;
          continue;
        }
        else {  /* add a separator */
          menu_item = geda_menu_separator_new();
        }
      }
    }
    else {

      menu_item = geda_image_menu_item_new_with_label(_(item.name));

      gtk_widget_set_tooltip_text (menu_item, _(item.tip));
      g_object_set (menu_item, "has-tooltip", show_pop_tips, NULL);

      if (item.use_stock) {
        image = gtk_image_new_from_stock(item.icon, GTK_ICON_SIZE_MENU);
      }
      else {
        image = create_pixmap (item.icon);
      }

      geda_image_menu_item_set_image ((GedaImageMenuItem*)menu_item, image);

      /* Enable icon visibility based on the current setting */
      g_object_set (image, "visible", show_pop_icons, NULL);
      g_object_set (menu_item, "show-image", show_pop_icons, NULL);

      /* Connect things up so that the actions get run */
      g_signal_connect (menu_item, "activate",
                        (void*) item.func,
                        (void*)(long)item.action_id);

      GEDA_OBJECT_SET_DATA(menu_item, w_current, "top-level");
      POPUP_ITEMS_LIST = g_slist_append (POPUP_ITEMS_LIST, menu_item);
      g_hash_table_insert (POPUP_MAIN_HASH, (char*)item.name, menu_item);

#if DEBUG
      fprintf(stderr, "%s: appending <%s> <%p>\n", __func__, item.name, menu_item);
#endif

    }

    g_object_set (menu_item, "visible", TRUE, NULL);
    geda_container_add(menu, menu_item);
  }

  /* Save the menu to the active menu data structure */
  POPUP_MAIN = menu;

  POPUP_PATH = x_menu_build_path_popup(w_current, menu_data, show_pop_icons,
                                                             show_pop_tips);

  menu = MENU_BAR; /* Get pointer to the main menu */

  if (GEDA_IS_MENU_BAR(menu)) {

    /* Setup the callback for the main menu options */
    char *popcons_path = OPT_POPCONS_MENU_PATH;
    char *poptips_path = OPT_POPTIPS_MENU_PATH;

    menu_item = GEDA_OBJECT_GET_DATA (menu, popcons_path);

    if (GEDA_IS_MENU_ITEM(menu_item)) {

      GTK_CALLBACK_TOGGLED (menu_item, x_menu_toggle_icons, POPUP_ITEMS_LIST);
    }

    menu_item = GEDA_OBJECT_GET_DATA (menu, poptips_path);

    if (GEDA_IS_MENU_ITEM(menu_item)) {

      GTK_CALLBACK_TOGGLED (menu_item, x_menu_toggle_tips, POPUP_ITEMS_LIST);
    }
  }

  return GTK_IS_WIDGET(POPUP_MAIN);
}

/*!
 * \brief Show the Popup Menu
 * \par Function Description
 *
 * \note need to look at this... here and the setup
 */
int
x_menu_display_main_popup (GschemToplevel *w_current, GdkEventButton *event)
{
  GtkWidget *menu;
  MenuData  *menu_data;

  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  menu = POPUP_MAIN;

  if (!GEDA_IS_MENU (menu)) {
    BUG_MSG ("popup menu is NULL");
  }
  else {
    w_current->pointer_sx = event->x;
    w_current->pointer_sy = event->y;
    geda_menu_popup ((GedaMenu*)menu, NULL, NULL, NULL, NULL,
                     event->button, event->time);
  }

  return FALSE;
}

/** @} END Group Main-Context-Menu (the main popup menu) */

/** \defgroup Main-Menu-Support Main Menu Support Functions
 *  @{
 * \ingroup menu-module
 * \brief Functions to support the Main Menu
 * \par
 * Functions in this group, mostly callbacks, support the main menu.
 */

static int sensitivity_errors = 0;

/*!
 * \brief Set Sensitivity of Main Menu Item
 * \par Function Description
 *  This function is called from i_status to set the senitivity of menu items!
 */
void x_menu_sensitivity (GschemToplevel *w_current, const char *buf, int flag)
{
  GtkWidget *menubar;
  GtkWidget *item;

  if (!buf) {
    return;
  }

  menubar = x_menu_get_main_menu (w_current);

  item = GEDA_OBJECT_GET_DATA (menubar, buf);

  if (item && GEDA_IS_MENU_ITEM(item)) {
    gtk_widget_set_sensitive((GtkWidget*)item, flag);
    /* item = pointer to menu widget -- do not free here */
  }
  else {

    const char *log_msg = _("Tried to set the sensitivity on non-existent menu item");

    if (verbose_mode) {
      u_log_message("%s '%s'\n", log_msg, buf);
    }
    else {
      if (sensitivity_errors < SENSITIVITY_ERROR_LIMIT) {
        q_log_message("%s '%s',\n", log_msg, buf);
      }
      sensitivity_errors++;
      if (sensitivity_errors == SENSITIVITY_ERROR_LIMIT) {
        const char *log_msg1 = _("Excessive errors");
        const char *log_msg2 = _("disabling sensitivity warnings");
        geda_log_q("%s <%d>, %s\n", log_msg1, sensitivity_errors, log_msg2);
      }
    }
  }
}

/*!
 * \brief Set Sensitivity of Popup Menu Item
 * \par Function Description
 *  This function sets the sensitivity of the items in the right button
 *  popup.
 */
void x_menu_popup_sensitivity (GschemToplevel *w_current, const char *name, int flag)
{
  MenuData *menu_data;
  GtkWidget *menu_item;

  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  menu_item = (GtkWidget*)g_hash_table_lookup (POPUP_MAIN_HASH, name);

  if (menu_item) {
    gtk_widget_set_sensitive(menu_item, flag);
  }
  else {
    fprintf(stderr, "%s popup item non-existent <%s>\n", __func__, name);
  }
}

/*!
 * \brief Save the State of Main Menu Toggle Options
 * \par Function Description
 *  This function retrieves and saves the state of the non-toolbar toggle
 *  menu items (like rubber-mode) to the user's menu configuration file.
 */
void x_menu_save_state(GschemToplevel *w_current)
{
  GtkWidget *menubar;
  EdaConfig *cfg;

  bool  state;
  int   errors = 0;

  void save_menu_toggler_state(const char *key, const char *path) {

    GedaCheckMenuItem *toggler = GEDA_OBJECT_GET_DATA(menubar, path);

    if (GEDA_IS_CHECK_MENU_ITEM (toggler)) {
      state = geda_check_menu_item_get_active(toggler);
      eda_config_set_boolean(cfg, MENU_CONFIG_GROUP, key, state);
    }
    else {
      errors++;
    }
  }

  menubar = x_menu_get_main_menu(w_current);

  if (menubar != NULL && GEDA_IS_MENU_BAR(menubar)) {

    char *icons_path   = OPT_ICON_MENU_PATH;      /* Menu Paths */
    char *tooltip_path = OPT_TIPS_MENU_PATH;
    char *popcons_path = OPT_POPCONS_MENU_PATH;
    char *poptips_path = OPT_POPTIPS_MENU_PATH;

    v_log_message (_("Saving menu toolbar options..."));

    cfg = eda_config_get_user_context ();

    save_menu_toggler_state("show-menu-icons",  icons_path);
    save_menu_toggler_state("show-menu-tips",   tooltip_path);
    save_menu_toggler_state("show-popup-icons", popcons_path);
    save_menu_toggler_state("show-popup-tips",  poptips_path);

    if (errors == 0) {
      v_log_message (_(" done\n"));
    }
    else {
      v_log_message (_(" there were %d errors\n"), errors);
    }

    eda_config_set_boolean(cfg, MENU_CONFIG_GROUP, "show-recent-path",
                           show_recent_path);
  }
}

/* Called to set the menu radio button for the grid mode when
 * the mode was changed by a means other than the menu. Unlike
 * the toolbars buttons, the Menu/View/Grid radios do not have
 * an associated action. If a menu grid-mode radio is selected
 * only the active button calls to update the toolbar radio
 * group, and when the toolbar grid button is toggled, the
 * action handler calls this function, which blocks the call
 * back for the menu grid-mode radio for the button that is
 * being set active. And in this way both radio button groups
 * can be synchronized and not recursively trigger the other.
 */
void
x_menu_set_grid_radio(GschemToplevel *w_current)
{
  GedaCheckMenuItem *radio;
  MenuData *menu_data;
  void *func;

  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
  radio = NULL;

  switch(w_current->grid_mode) {
    case(GRID_NONE):
      radio = GEDA_OBJECT_GET_DATA (MENU_BAR, OPT_GRID_NONE_MENU_PATH);
      func  = x_menu_grid_none_mode;
      break;

    case(GRID_DOTS):
      radio = GEDA_OBJECT_GET_DATA (MENU_BAR, OPT_GRID_DOTS_MENU_PATH);
      func  = x_menu_grid_dots_mode;
      break;

    case(GRID_MESH):
      radio = GEDA_OBJECT_GET_DATA (MENU_BAR, OPT_GRID_MESH_MENU_PATH);
      func  = x_menu_grid_mesh_mode;
      break;

    default:
      func = NULL;
      break;
  }

  if (GEDA_IS_CHECK_MENU_ITEM(radio)) {

    /* Only activate if radio not already active, this effectively
     * blocks us if a menu item indirectly initiated the action */
    if (!geda_check_menu_item_get_active(radio)) {
      /* Block callback so x_toolbars_set_grid_radio is not called
       * recursively */
      g_signal_handlers_block_by_func (radio, func, w_current);
        geda_check_menu_item_set_active (radio, TRUE);
      g_signal_handlers_unblock_by_func (radio, func, w_current);
    }
  }
}

const
char *x_menu_get_buffer_menu (GschemToplevel *w_current)
{
  MenuData *menu_data;

  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
  return menu_data->buffer_menu_name;
}

/*!
 * \brief Set Menu Icon Visibility
 * \par Function Description
 *  This function turns menu icons on or off for a given list of menu items
 *  based on the state argument. The list is a glist of all main menu items or
 *  all pop-menu items. This function is the only function that actually changes
 *  the visiblity of non-toggle type menu items.
 */
static void x_menu_lowlevel_set_icon_visibility (GSList* list, bool state)
{
  lambda (GObject *menu_item) {

    if (GEDA_IS_IMAGE_MENU_ITEM(menu_item)) {
      g_object_set(menu_item, "show-image", state, NULL);
    }
    else {

#if DEBUG
      fprintf(stderr, "%s: Ignoring invalid object, <%p>\n", __func__, menu_item);
#else
      fprintf(stderr, "%s: Ignoring invalid object, maybe a seperator\n", __func__);
#endif

    }
    return FALSE;
  }
  mapcar(list)

  if (state) {
    q_log_message(_("gschem: Enabling menu icons\n"));
  }
  else {
    q_log_message(_("gschem: Disabling menu icons\n"));
  }
}

/*!
 * \brief Set Menu Icon Visibility
 * \par Function Description
 *  This function exist so the menu icons can be turned off after the
 *  main menu is built. Setting "show-image", as was done in x_menu_setup_ui
 *  works for gnome display managers but not with Mate, which seems to try
 *  and enforce a global system-wide setting. Gschem's user settings takes
 *  precedence, so this function is used as a work-around.
 */
void x_menu_set_icon_visibility(GschemToplevel *w_current, bool state)
{
  MenuData *menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
  x_menu_lowlevel_set_icon_visibility(MENU_ITEMS_LIST, state);
}

/*!
 * \brief Toggle Menu Icon Visibility
 * \par Function Description
 *  Callback function calls x_menu_lowlevel_set_icon_visibility to turn
 *  menu icon on or off based on the state of the toggle menu item pointer
 *  pointed to by widget. This is a callback for the toggle menu icons
 *  option so widget is a toggle item. The list is a glist of all menu
 *  items or all pop-menu items.
 *
 * \sa x_menu_lowlevel_set_icon_visibility, x_menu_set_toolbar_toggle
 *
 * \remark This function is only applicable to non-toggle menu items
 *         as the toggle-type always have visible images. For toggle
 *         types see the group menu-toggle-action below.
 */
static void x_menu_toggle_icons(GtkWidget *widget, GSList* list)
{
  int state;

  state = geda_check_menu_item_get_active ((GedaCheckMenuItem*)widget);
  x_menu_lowlevel_set_icon_visibility(list, state);
}

/*!
 * \brief Enable Disable Menu Tooltips
 * \par Function Description
 *  Callback function turns menu tips on or off based on the state of
 *  the toggle menu item pointed to by widget. This is a callback for
 *  the toggle tips menu option so widget is a toggle item. The list
 *  is a glist of all main menu items or all pop-menu items depending
 *  on whether widget is the menu or popup tooltips toggle item.
 *
 * \note This is a callback for the popup tooltips toggle item and
 *       a helper for x_menu_toggle_main_tips, which is a callback
 *       for main menu tooltips toggle item.
 */
static void x_menu_toggle_tips(GtkWidget *widget, GSList *list)
{
  int state;

  state = geda_check_menu_item_get_active ((GedaCheckMenuItem*)widget);

  lambda (GObject *menu_item) {
    g_object_set (menu_item, "has-tooltip", state, NULL);
    return FALSE;
  }
  mapcar(list)

  x_menu_update_recent_files();

  if (state) {
    v_log_message(_("Enabling menu tooltips\n"));
  }
  else {
    v_log_message(_("Disabling menu tooltips\n"));
  }
}

/*!
 * \brief Callback to Toggle Visibility of Tooltips for All Main Menu Items
 * \par Function Description
 * Set visibility of tool tips for all menu items. Retrieves the menu data for
 * the current window and passes the containing menu list to x_menu_toggle_tips
 * to set the visibility of tooltips for non-toggle menu items and then  set
   the visibility of tips for menu toggle items.;
 */
static void x_menu_toggle_main_tips(GtkWidget *widget, GschemToplevel *w_current)
{

  GtkWidget *menu_item;
  MenuData  *menu_data;
  int state;

  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  /* Set visibility of tooltips for all non-toggle main menu items */
  x_menu_toggle_tips(widget, MENU_ITEMS_LIST);

  /* Get the state of the "Show menu tips" toggle widget */
  state = geda_check_menu_item_get_active ((GedaCheckMenuItem*)widget);

  /* Set visibility of tips for option menu toggle items */
  {
    GtkWidget *menubar;

    menubar = x_menu_get_main_menu(w_current);

    lambda (ToggleMenuData *toggler_data) {

      char *menu_path;

      menu_path = toggler_data->menu_path;
      menu_item = GEDA_OBJECT_GET_DATA (menubar, menu_path);

      g_object_set (menu_item, "has-tooltip", state, NULL);

      return FALSE;
    }
    mapcar(TOGGLERS_LIST)
  }

  {
    lambda (ToggleMenuData *menu_radio_item) {

      g_object_set (menu_radio_item, "has-tooltip", state, NULL);

      return FALSE;
    }
    mapcar(GRID_RADIO_LIST)
  }

  GtkWidget   *submenu;
  const GList *list, *iter;

  /* Set visibility of tips for _View/_Menu toggle items */
  menu_item = GEDA_OBJECT_GET_DATA(MENU_BAR, IDS_MENU_VIEW_MENU);
  submenu   = geda_menu_item_get_submenu_widget ((GedaMenuItem*)menu_item);
  list      = geda_menu_shell_get_children(GEDA_MENU_SHELL(submenu));

  for (iter = list; iter; iter=iter->next) {
    menu_item = iter->data;
    g_object_set (menu_item, "has-tooltip", state, NULL);
  }

  /* Set visibility of tips for _View/_Toolbars toggle items */
  menu_item = GEDA_OBJECT_GET_DATA(MENU_BAR, IDS_MENU_VIEW_TOOLBARS);
  submenu   = geda_menu_item_get_submenu_widget ((GedaMenuItem*)menu_item);
  list      = geda_menu_shell_get_children(GEDA_MENU_SHELL(submenu));

  for (iter = list; iter; iter=iter->next) {
    menu_item = iter->data;
    g_object_set (menu_item, "has-tooltip", state, NULL);
  }

}

/** \defgroup menu-toggle-action Menu Toggle Action
 *  @{
 * \ingroup main-menu-support
 * \brief Menu Toggle Action Support Functions
 * \par
 *  The Menu toggles buttons need the "activate" signal blocked
 *  temporily to prevent recursion with callbacks.
 */

/*!
 * \brief Set State of a Menu Toggle Item - Low LeveL
 * \par Function Description
 *
 */
static void x_menu_set_toggler(ToggleMenuData *toggler_data, bool state)
{
  GschemToplevel *w_current;

  GtkWidget *menubar;
  char      *menu_path;

  menu_path  = toggler_data->menu_path;
  w_current  = toggler_data->w_current;
  menubar    = x_menu_get_main_menu(w_current);

  if (menubar != NULL) {

    GtkWidget *menu_item  = GEDA_OBJECT_GET_DATA (menubar, menu_path);

     if (menu_item != NULL) {

       /* Get action for this item so we can block the signal */
       GtkAction *action = gtk_widget_get_action(menu_item);

       if (action != NULL) {
         g_signal_handler_block(action, toggler_data->handler);
           geda_check_menu_item_set_active((GedaCheckMenuItem*)menu_item, state);
         g_signal_handler_unblock(action, toggler_data->handler);
       }
       else {
         const char *log_msg = _("Action not found");
         u_log_message("%s: %s, \"%s\" \n", __func__, log_msg, menu_path);
       }
     }
     else {
       const char *log_msg = _("Menu path not found");
       u_log_message("%s: %s, \"%s\" \n", __func__, log_msg, menu_path);
     }
  }
  else {
    BUG_MSG("invalid pointer [menubar]");
  }
  return;
}

/*!
 * \brief Set State of Menu Toggle Item or Items
 * \par Function Description
 * This is a menu support function to set the state of menu toggle items.
 * The function is used when a menu option was changed by some other means,
 * for example when options are turned off with "Hot-Keys". \a toggle_id
 * can be RESET_TOGGLERS, resulting in all toggle items under the options
 * menu item being set to \a state.
 *
 * \param w_current Gschem toplevel object
 * \param toggle_id is integer index of the ToggleMenuData Glist item to set
 * \param state     is integer value to set, either TRUE (check) or FALSE (uncheck).
 *
 * \sa MenuToggleItem x_menu_set_toolbar_toggle
 */
void x_menu_set_togglable(GschemToplevel *w_current, int toggle_id, bool state)
{
  ToggleMenuData *toggler_data;
  MenuData *menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  void set_toggler(int index, bool value) {
    toggler_data = (ToggleMenuData*)g_slist_nth_data (TOGGLERS_LIST, index);
    if(toggler_data) {
      x_menu_set_toggler(toggler_data, value);
    }
  }

  if (toggle_id == RESET_TOGGLERS) {
    set_toggler(SNAP_TOGGLE,     (w_current->snap > 0));
    set_toggler(RUBBER_TOGGLE,   (w_current->netconn_rubberband > 0));
    set_toggler(MAGNETIC_TOGGLE, (w_current->magnetic_net_mode > 0));
    set_toggler(DRAG_CAN_MOVE,   (w_current->drag_can_move > 0));
    set_toggler(OUTLINE_TOGGLE,  (w_current->action_feedback_mode > 0));
    set_toggler(AUTO_PAN_TOGGLE, (w_current->auto_pan > 0));
  }
  else {

    if (toggle_id < g_slist_length(TOGGLERS_LIST)) {
      toggler_data = (ToggleMenuData*) g_slist_nth_data (TOGGLERS_LIST, toggle_id);
    }

    x_menu_set_toggler(toggler_data, state);
  }
  return;
}

/*!
 * \brief Set State of Menu ToolBar Toggle Item
 * \par Function Description
 *  This is a menu support function to set the state of an individual Toolbar
 *  Menu toggle item. The function is called when the menu option was changed
 *  by some other means, for example when a floating toolbars is turned off
 *  with the "X" box.
 *
 * \param w_current Gschem toplevel object
 * \param toggle_id is int index of the IDS_Menu_Toggles item to set
 * \param state     is int value to set, either TRUE (check) or FALSE (uncheck).
 *
 * \sa x_menu_set_togglable
 */
void x_menu_set_toolbar_toggle(GschemToplevel *w_current, int toggle_id, bool state)
{
  char  menu_name[36] = IDS_MENU_VIEW_TOOLBARS "/";
  char *menu_path;

  GtkWidget *menu_bar;
  GtkWidget *menu_item;

  menu_bar  = x_menu_get_main_menu(w_current);
  menu_path = geda_strconcat (menu_name, IDS_Menu_Toolbar_Toggles[toggle_id], NULL);
  menu_item = GEDA_OBJECT_GET_DATA (menu_bar, menu_path);

  if (menu_item != NULL) {
    geda_check_menu_item_set_active((GedaCheckMenuItem*)menu_item, state);
  }
  else {
    u_log_message("%s \"%s\"\n", _("Error: did not find path"), menu_path);
  }

  GEDA_FREE(menu_path);
  return;
}

/*!
 * \brief Set State of ToolBar Tooltips Toggle Menu item
 * \par Function Description
 *  This function sets the active state of the "Display Tips" under the
 *  Toolbar submenu and has nothing to do with tooltip displayed in the
 *  menus.
 *
 * \param w_current Gschem toplevel object
 * \param state     is int value to set, either TRUE (check) or FALSE (uncheck).
 *
 * \sa x_menu_set_togglable x_menu_toggle_main_tips
 */
void x_menu_set_toolbar_toggle_tips(GschemToplevel *w_current, bool state)
{
  const char *menu_path;

  GtkWidget *menu_bar;
  GtkWidget *menu_item;

  menu_bar  = x_menu_get_main_menu(w_current);
  menu_path = OPT_BAR_TIPS_MENU_PATH;
  menu_item = GEDA_OBJECT_GET_DATA (menu_bar, menu_path);

  if (menu_item != NULL) {
    geda_check_menu_item_set_active((GedaCheckMenuItem*)menu_item, state);
  }
  else {
    u_log_message("%s \"%s\"\n", _("Error: did not find path"), menu_path);
  }

  return;
}

/** @} end group menu-toggle-action  */

/* ---------------- Recent Files Menu ---------------- */

/** \defgroup recent-file-menu Recent Files Menu Functions
 *  @{
 *  \ingroup menu-module
 *  \par
 *  This is the old method, as oppose to the GTK Recent Chooser Manger
 *  version. This method seems to work better on Debian machines and
 *  derivatives (which is likely to be the majority) due to default
 *  security policies, which results in ALL the links to recent files
 *  being erased when ever ANY recent file link is accessed by any one
 *  having "administrative" privileges.
 *
 * \todo: The Recent files menu-item is a dynamic object not created by
 *        the regular menu systems, consequenty, no icons is assigned.
 *        The section should assign an icon.
 */

/** \defgroup recent-file-internal Recent Files Functions
 *  @{
 * \brief This group contains core Routine for the Recent Files Menu.
*/

/*!
 * \brief Update Recent Files Menus
 * \par Function Description
 *  Make all toplevels reflect changes to the recent files list.
 */
static void x_menu_update_recent_files(void)
{
   GList *iter;

   for (iter = global_window_list; iter != NULL; iter = g_list_next (iter)) {

     GschemToplevel *w_current;
     GtkWidget      *submenu;
     GtkWidget      *recent_menu_item;
     MenuData       *menu_data;

     w_current = (GschemToplevel*)iter->data;
     menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

     if (MENU_BAR == NULL)
       continue;

     recent_menu_item = GEDA_OBJECT_GET_DATA (MENU_BAR, "_File/Open Recen_t");

     if (recent_menu_item == NULL)
       return;

     submenu = geda_menu_item_get_submenu_widget((GedaMenuItem*)recent_menu_item);
     gtk_widget_destroy(submenu);

     x_menu_attach_recent_submenu(w_current);
   }
}

/*!
 * \brief Remove all entries from the recent files list
 *  and update the menus.
 */
static void x_menu_clear_recent_file_list(void *data)
{
   GList *iter;

   iter = recent_files;

   while (iter) {
      GEDA_FREE(iter->data);
      iter = g_list_next(iter);
   }
   g_list_free(recent_files);
   recent_files = NULL;

   x_menu_update_recent_files();
}

static void x_menu_recent_destroy_data (GedaMenuItem *menuitem, void *menu_data)
{
  GEDA_FREE (menu_data);
}

/*!
 * \brief Recent Menu item Clicked
 * \par Function Description
 *  Called with user clicks on a menu item on the recent files menu or when
 *  the user select "open" from the popup menu on the recent file submenu.
 */
static void x_menu_recent_file_clicked (GedaMenuItem *menuitem, void *menu_data)
{
   FILE           *fp;
   Page           *page;
   RecentMenuData *data      = (RecentMenuData*)menu_data;
   GschemToplevel *w_current = data->w_current;
   char           *filename  = data->filename;

   /* Check if the file exists */
   fp = fopen((char*) filename, "r");

   if (fp == NULL) {
      /* Remove this entry from all menus */
      u_log_message("%s \"%s\"\n", _("Could not open file"), filename);
      recent_files = g_list_remove(recent_files, filename);
      x_menu_update_recent_files();
      return;
   }
   fclose(fp);

   page = x_window_open_page(w_current, filename);
   x_window_set_current_page(w_current, page);
}

/*!
 * \brief Make RECENT_FILES_STORE contain an empty file list.
 *  Create a new empty key, nothing is written to the file.
 */
static void x_menu_recent_files_create_empty(void)
{
   GedaKeyFile *keyfile;
   char        *data;
   char        *file;
   const char  *path;
   const char  *const tmp[] = { NULL };

   path    = geda_user_config_path ();
   file    = g_build_filename(path, RECENT_FILES_STORE, NULL);
   keyfile = geda_keyfile_new();

   geda_keyfile_set_string_list(keyfile, "Recent files", "Files", tmp, 0);

   data = geda_keyfile_to_data(keyfile, NULL, NULL);

   geda_keyfile_free(keyfile);
   g_file_set_contents(file, data, -1, NULL);

   GEDA_FREE(data);
   GEDA_FREE(file);
}

/** \defgroup recent-popup-menu Recent Files Popup Menu
 *  @{
 */

/*!
 * \brief Recent Menu item Popup Show Recent Paths Toggled
 * \par Function Description
 *  Called with user toggles to Show path items on recent file pop-up menu.
 *  Toggles the state of show_recent_path, calls to update the menu and
 *  causes the recent file sub-menu to reappear with the opposite state.
 */
static void x_menu_toggle_recent_path (GedaCheckMenuItem *menuitem, void *user_data)
{
  RecentMenuData *data      = (RecentMenuData*)user_data;
  GschemToplevel *w_current = data->w_current;

  GedaMenuItem *menu_item;
  MenuData     *menu_data;

  show_recent_path = geda_check_menu_item_get_active (menuitem);
  x_menu_update_recent_files();

  /* Get pointer to the recent files submenu */
  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
  menu_item = GEDA_OBJECT_GET_DATA (MENU_BAR, "_File/Open Recen_t");

  /* Re-display the recent files submenu */
  geda_menu_item_activate_item(menu_item);
}

/*!
 * \brief Recent Files Menu Internal Populate Popup
 * \par Function Description
 *  This functions call when the remove option is selected from
 *  the Recent File Menu popup menu to remove the file whose
 *  name is in the RecentMenuData record from recent history.
 */
static void x_menu_recent_file_remove (GedaMenuItem *menuitem, void *user_data)
{
  RecentMenuData *menu_data = user_data;
  char           *filename  = menu_data->filename;

  /* Remove this entry from all menus */
  recent_files = g_list_remove(recent_files, filename);
  x_menu_update_recent_files();
}

/*!
 * \brief Recent Files Menu Internal Show Popup
 * \par Function Description
 *  This functions creates and displays a small pop-up menu on
 *  the recent files menu. The open option is provided for the
 *  sake of completeness, the real objective here is to allow
 *  users to remove individual recent menu items.
 */
static void x_menu_recent_show_popup (GedaMenuItem   *menu_widget,
                                      GdkEventButton *event,
                                      RecentMenuData *menu_data)
{
  GtkWidget *menu;
  GtkWidget *popup_item;

  /* create the context menu */
  menu = geda_menu_new();

  popup_item = geda_menu_item_new_with_label (_("Open"));

  g_signal_connect (popup_item, "activate",
                    G_CALLBACK(x_menu_recent_file_clicked), menu_data);

  geda_menu_append (menu, popup_item);

  popup_item = geda_menu_item_new_with_label (_("Remove"));

  g_signal_connect (popup_item, "activate",
                    G_CALLBACK(x_menu_recent_file_remove), menu_data);

  geda_menu_append (menu, popup_item);

  popup_item = geda_check_menu_item_new_with_mnemonic (_("_Show path"));
  geda_check_menu_item_set_active((GedaCheckMenuItem*)popup_item, show_recent_path);

  GTK_CALLBACK_TOGGLED (popup_item, x_menu_toggle_recent_path, menu_data);

  geda_menu_append (menu, popup_item);

  gtk_widget_show_all (menu);

  /* make menu a popup menu */
  geda_menu_popup ((GedaMenu*)menu, NULL, NULL, NULL, NULL,
                   (event != NULL) ? event->button : 0,
                    gdk_event_get_time ((GdkEvent*)event));

  /* Sink the menu so it will be automatically destroyed */
  g_object_ref_sink (menu);
  g_object_unref (menu);
}

/*!
 * \brief Popup Callback for recent files menu item
 * \par Function Description
 *  Called when a mouse button is release with the cursor over
 *  a menu items. If the 3rd button was released, a small popup
 *  menu is displays
 *
 * \sa x_menu_recent_show_popup
 */
static bool x_menu_recent_button_released (GedaMenuItem   *menu_item,
                                           GdkEventButton *event,
                                           RecentMenuData *menu_data)
{
  bool ret_val;

  if (event->button == 3) {

    x_menu_recent_show_popup(menu_item, event, menu_data);

    ret_val = TRUE;
  }
  else {
    ret_val = FALSE;
  }

  return ret_val;
}

/** @} endgroup recent-popup-menu */
/** @} endgroup recent-file-internal */

/*!
 * \brief Attach a submenu with filenames to the 'Open Recent'
 *  menu item. Called from x_window_setup().
 */
void x_menu_attach_recent_submenu(GschemToplevel *w_current)
{
   GtkWidget *item;
   GtkWidget *recent_menu_item, *recent_submenu;
   GList     *iter;
   MenuData  *menu_data;
   bool       show_menu_tips;

   menu_data        = g_slist_nth_data (ui_list, w_current->ui_index);
   recent_menu_item = GEDA_OBJECT_GET_DATA (MENU_BAR, "_File/Open Recen_t");

   if (recent_menu_item == NULL)
      return;

   /* disconnect all unblocked signals */
   while(1) {

     unsigned long id;

     id = g_signal_handler_find(recent_menu_item, G_SIGNAL_MATCH_UNBLOCKED,
                                0, 0, NULL, NULL, NULL);
     if (id == 0)
       break;
     g_signal_handler_disconnect(recent_menu_item, id);
   }

   g_object_get (recent_menu_item, "has-tooltip", &show_menu_tips, NULL);

   recent_submenu = geda_menu_new();
   iter = recent_files;

   while (iter) {

     const char *filename;

     RecentMenuData *menu_data = GEDA_MEM_ALLOC0 (sizeof(RecentMenuData));

     filename                  = iter->data;
     menu_data->filename       = iter->data;
     menu_data->w_current      = w_current;

     filename = show_recent_path ? filename : geda_file_get_basename(filename);

     item = geda_menu_item_new_with_label((char*) filename);

     /* if menu tooltips are enabled and not showing the path in the recent
      * files menu item then show the full name with path as the tooptip */
     if (show_menu_tips && !show_recent_path) {
       gtk_widget_set_tooltip_text(item, iter->data);
     }

     g_signal_connect (item, "activate",
                       G_CALLBACK(x_menu_recent_file_clicked), menu_data);

     g_signal_connect (item, "button-release-event",
                       G_CALLBACK (x_menu_recent_button_released), menu_data);

     g_signal_connect (item, "destroy",
                       G_CALLBACK(x_menu_recent_destroy_data), menu_data);

     geda_menu_append(recent_submenu, item);

     iter = g_list_next(iter);
   }

   if (recent_files != NULL) {

     GtkWidget *label;

     /* Append the 'Clear' menu item to the submenu */
     GtkWidget *alignment = gtk_alignment_new(0.5, 0, 0, 0);

     item = geda_menu_item_new();

     label = geda_label_new(_("Clear"));
     geda_container_add(alignment, label);

     geda_container_add(item, alignment);

     GEDA_SIGNAL_CONNECT(item, "activate",
                         x_menu_clear_recent_file_list, NULL);

     geda_menu_append(recent_submenu, geda_menu_separator_new());
     geda_menu_append(recent_submenu, item);
   }

   gtk_widget_show_all(recent_submenu);

   geda_menu_item_set_submenu_widget((GedaMenuItem*)recent_menu_item, recent_submenu);
}

/*!
 * \brief Add a filename to the list of recent files.
 *  If filename is already in the list, moves it to the head of the list.
 */
void x_menu_recent_files_add(const char *filename)
{
       GError *err = NULL;
        GList *p   = recent_files;
   const char *basename;
         char *save_fn;

   basename = geda_file_get_basename(filename);
   if(strstr(basename, "untitled_") == basename) {
      return;
   }

   /* Normalize the filename. */
   save_fn = geda_normalize_filename (filename, &err);
   if (err != NULL) {
     save_fn = geda_strdup (filename);
     g_error_free (err);
   }

   /* Check if the file is already in the list.  */
   while (p != NULL) {

#if defined (OS_WIN32)

     if (stricmp (save_fn, (char*) p->data) == 0) {
       break;
     }

#else

     if (strcmp (save_fn, (char*) p->data) == 0) {
       break;
     }

#endif

     p = g_list_next (p);
   }

   if (p != NULL) {
     /* Since we found the filename already in the list, move it to
      * the head of the list. */
     GEDA_FREE (save_fn);
     save_fn = (char*) p->data;
     recent_files = g_list_delete_link (recent_files, p);
     recent_files = g_list_prepend (recent_files, save_fn);
   } else {
     /* Otherwise, just add the new filename to the front of the
      * list. */
     recent_files = g_list_prepend (recent_files, save_fn);
   }

   x_menu_update_recent_files();
}

/*!
 * \brief Save the list of recent files to RECENT_FILES_STORE.
 * \par Function Description
 *  This function is called before exiting to save the list of recent
 *  files to disk.
 *
 * \param [in] user_data unused
 */
void x_menu_recent_files_save(void *user_data)
{
   char *files[MAX_RECENT_FILES];
   char *data;
   char *file;
   int   num;

   file = g_build_filename(geda_user_config_path(), RECENT_FILES_STORE, NULL);
   num  = 0;

   GList *p = recent_files;
   if(p == NULL) {
      x_menu_recent_files_create_empty();
      return;
   }

   while((p != NULL) && (num < MAX_RECENT_FILES)) {
     files[num++] = (char*) p->data;
     p = g_list_next(p);
   }

   GedaKeyFile *keyfile = geda_keyfile_new();

   geda_keyfile_set_string_list(keyfile, "Recent files", "Files", (const char**)files, num);
   data = geda_keyfile_to_data(keyfile, NULL, NULL);
   g_file_set_contents(file, data, -1, NULL);

   if (verbose_mode) {

      const char *log_msg = _("Recent menu items saved to");

      geda_log("%s %s\n", log_msg, file);
   }

   GEDA_FREE(data);
   GEDA_FREE(file);
   geda_keyfile_free(keyfile);
}

/*!
 * \brief Load the recent file list using data from RECENT_FILES_STORE.
 *  Must be called before any other recent-files-related
 *  functions.
 */
void x_menu_recent_files_load()
{
   GedaKeyFile  *keyfile;
   char         *file;
   char        **list;
   unsigned int  len;

   keyfile = geda_keyfile_new();
   file = g_build_filename(geda_user_config_path(), RECENT_FILES_STORE, NULL);

   if (!g_file_test(file, G_FILE_TEST_EXISTS)) {
     geda_create_path(geda_user_config_path (), S_IRWXU | S_IRWXG);
     x_menu_recent_files_create_empty();
   }

   if (!geda_keyfile_load_from_file(keyfile, file, G_KEY_FILE_NONE, NULL)) {
      /* error opening key file, create an empty one and try again */
      x_menu_recent_files_create_empty();
      if(!geda_keyfile_load_from_file(keyfile, file, G_KEY_FILE_NONE, NULL))
         return;
   }

   list = geda_keyfile_get_string_list(keyfile, "Recent files", "Files", &len, NULL);

   if (list == NULL) {
      /* There was an error reading key file, don't bother to correct;
       * just overwrite it with an empty one */
      x_menu_recent_files_create_empty();
      return;
   }

   while(len > 0) {
     len--;
     recent_files = g_list_prepend(recent_files, geda_strdup(list[len]));
   }

   if (verbose_mode) {

      const char *log_msg = _("Recent menu items restore from");

      geda_log("%s %s\n", log_msg, file);
   }

   g_strfreev(list);
   GEDA_FREE(file);

   geda_keyfile_free(keyfile);
}

/*!
 * \brief Get the Most Recent Filename
 * \par Function Description
 *  This function returns a char pointer to the name of the most
 *  recent file loaded and is used by the auto_load_last mechanism.
 *
 * \return  const char pointer to the filename string
 */
const char *x_menu_recent_files_last(void)
{
   return (g_list_nth_data(recent_files, 0));
}

/** @} end group recent-file-menu */
/** @} end group Main-Menu-Support */
/** @} end group Menu-Module */
