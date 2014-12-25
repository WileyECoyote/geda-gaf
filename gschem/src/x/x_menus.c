/* -*- C x_menus.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file x_menus.c
 * \brief Main Window Auxiliary Module for Menus, including Context
 */

#include "gschem.h"
#include <geda_stat.h>

#include <x_menus.h>
#include <geda_widgets.h>
#include <i_actions.h>
#include <geda_debug.h>

/** \defgroup Menu-Module Menu Module
 *  @{\brief This group contains Menu related functions
 *    \ingroup (main-window)
 */

#define MENU_BAR         menu_data->menu_bar
#define POPUP_MENU       menu_data->popup_menu
#define MENU_ITEMS_LIST  menu_data->menu_items
#define POPUP_ITEMS_LIST menu_data->popup_items
#define TOGGLERS_LIST    menu_data->menu_togglers
#define POPUP_HASH_TABLE menu_data->popup_hash

static void
x_menu_popup_execute(GtkObject *widget,int action_id);

/* Note: These are referenced using pop_MenuItem defined in our header */
const char* IDS_Popup_Actions[] = {
  ACTION(EDIT_SELECT),    ACTION(ADD_NET),       ACTION(ADD_ATTRIB),
  ACTION(ADD_COMPONENT),  ACTION(ADD_BUS),       ACTION(ADD_TEXT),
  ACTION(VIEW_ZOOM_IN),   ACTION(VIEW_ZOOM_OUT), ACTION(VIEW_BOX),
  ACTION(VIEW_EXTENTS),   ACTION(EDIT_ATTRIB),
  ACTION(EDIT_COMPONENT), ACTION(EDIT_PIN),      ACTION(EDIT_DELETE),
  ACTION(EDIT_COPY),      ACTION(EDIT_MCOPY),    ACTION(EDIT_MOVE),
  ACTION(EDIT_MIRROR),    ACTION(EDIT_ROTATE),
  ACTION(DOWN_SCHEMATIC), ACTION(DOWN_SYMBOL),   ACTION(HIERARCHY_UP),
  ACTION(EDIT_CB_CUT),    ACTION(EDIT_CB_COPY),  ACTION(EDIT_CB_PASTE),
  NULL
};

/*! \struct Popup-Menu-Structure x_menus.h
 *
 *  Record Format: x_menus.h::st_popup_menu_entry
 *
 *    Menu Text,   function,  action_id, use_stock, icon, Tooltip
 *
 *  Where action_id is the action enumerated in .pop_MenuItem
 *  function must be NULL for Separators and Sub-menu entries, when
 *  function is NULL action_id = 0 is a separator, action_id = 1 is
 *  sub-menu.
 *
 *  Note: gschem factory, setup by x_icons_setup_factory, are referenced
 *  use_stock = True. if use_stock = False, the string in the icon field
 *  is passed to create_pixmap().
 *
 */

static PopupEntry popup_items[] = {

  { N_("Select"),            x_menu_popup_execute, pop_edit_select,    1, "gschem-select",  NULL},

  { "SEPARATOR",             NULL,                 0,                  0,  NULL,            NULL },

  { N_("Add"),               NULL,                 1,                  0,  NULL,            NULL },
  { N_("Net"),               x_menu_popup_execute, pop_add_net,        1, "gschem-net",     NULL},
  { N_("Attribute..."),      x_menu_popup_execute, pop_add_attribute,  0,  GAF_MAP(ADD_ATTRIBUTE), NULL},
  { N_("Component..."),      x_menu_popup_execute, pop_add_component,  1, "geda-component", NULL},
  { N_("Bus"),               x_menu_popup_execute, pop_add_bus,        1, "gschem-bus",     NULL},
  { N_("Text"),              x_menu_popup_execute, pop_add_text,       1, "gtk-bold",       NULL},

  { "END_SUB",               NULL,                 0,                  0,  NULL,            NULL },

  { N_("Zoom"),              NULL,                 1,                  0,  NULL,            NULL },
  { N_("In"),                x_menu_popup_execute, pop_zoom_in,        1, "gtk-zoom-in",    NULL},
  { N_("Out"),               x_menu_popup_execute, pop_zoom_out,       1, "gtk-zoom-out",   NULL},
  { N_("Box"),               x_menu_popup_execute, pop_zoom_box,       1, "geda-zoom-box",  NULL},
  { N_("Extents"),           x_menu_popup_execute, pop_zoom_extents,   1, "gtk-zoom-fit",   NULL},

  { "END_SUB",               NULL,                 0,                  0,  NULL,            NULL },

  { N_("Edit"),              NULL,                 1,                  0,  NULL,            NULL},
  { N_("Object..."),         x_menu_popup_execute, pop_edit_objects,   1, "gtk-indent",     NULL},
  { N_("Component..."),      x_menu_popup_execute, pop_edit_component, 0, "geda-component", NULL},
  { N_("Pin type..."),       x_menu_popup_execute, pop_edit_pintype,   1, "geda-pin-type",  NULL},

  { "END_SUB",               NULL,                 0,                  0,  NULL,            NULL },

  { N_("Delete"),            x_menu_popup_execute, pop_edit_delete,    1, "gtk-delete"},
  { N_("Copy"),              x_menu_popup_execute, pop_edit_copy,      1, "geda-copy",      NULL },
  { N_("MCopy"),             x_menu_popup_execute, pop_edit_mcopy,     1, "geda-multi",     NULL },
  { N_("Move"),              x_menu_popup_execute, pop_edit_move,      1, "geda-move",      NULL },
  { N_("Mirror"),            x_menu_popup_execute, pop_edit_mirror,    1, "geda-rotate",    NULL },
  { N_("Rotate"),            x_menu_popup_execute, pop_edit_rotate,    1, "geda-mirror",    NULL },

  { "SEPARATOR",             NULL,                 0,                  0,  NULL,            NULL },

  /* Menu items for hierarchy added by SDB 1.9.2005. */

  { N_("Hierarchy"),         NULL,                 1,                  0,  NULL,             NULL },
  { N_("Down Schematic"),    x_menu_popup_execute, pop_down_schemat,   1, "gtk-go-down",     NULL},
  { N_("Down Symbol"),       x_menu_popup_execute, pop_down_symbol,    1, "gtk-goto-bottom", NULL},
  { N_("Up"),                x_menu_popup_execute, pop_hierarchy_up,   1, "gtk-go-up",       NULL},

  /* Menu items for clip-board added by WEH 07.20.2013 */
  { "END_SUB",               NULL,                 0,                  0,  NULL,            NULL },
  { "SEPARATOR",             NULL,                 0,                  0,  NULL,            NULL },
  { N_("Cut to Clipboard"),  x_menu_popup_execute, pop_cb_cut,         1, "gtk-cut",        NULL },
  { N_("Copy to Clipboard"), x_menu_popup_execute, pop_cb_copy,        1, "gtk-copy",       NULL },
  { N_("Paste Clipboard"),   x_menu_popup_execute, pop_cb_paste,       1, "gtk-paste",      NULL },
  {NULL} /* sentinel */
};

/* These must be in the same order as ID_GSCHEM_Toolbar in x_toolbars.c */
const char* IDS_Menu_Toolbar_Toggles[] = {
  "_Add", "A_ttribute", "_Edit", "_Grid Snap", "_Page", "Se_lect", "_Standard", "_Zoom", /* ToolBar Menu Strings */
  NULL
};

const char* IDS_Menu_Toggles[] = { /* temp Menu Toggle Strings*/
  "Snap On-Off", "Outline-Box", "Rubberband", "Magnetic",
   NULL
};

static GList   *recent_files = NULL;
static GSList  *ui_list      = NULL;

int npopup_items = sizeof(popup_items) / sizeof(popup_items[0]);

static void x_menu_toggle_icons(GtkWidget *widget, GSList* list);
static void x_menu_toggle_tips(GtkWidget  *widget, GSList* list);

/*! \brief Execute Main Menu Selection
 *  \par Function Description
 *  This is a modified version of the "old" routine that retrieve
 * the action from the menu items and evaluates with guile. The
 * might still do this but first checks of the string is a valid
 * command and if so, then the string is passed to i_command_process
 * process instead, this eliminates the c=>scheme=>c. This improves
 * efficiency and provides better stability.
 */
static void g_menu_execute(GtkAction *action, void *user_data)
{
  GschemToplevel *w_current    = (GschemToplevel *) user_data;
  const char     *action_name  = gtk_action_get_name (action);
  char           *menu_action  = NULL;

  if (i_command_is_valid(action_name)) {
#if DEBUG
    fprintf(stderr, "Bypassing, guile for menu action %s\n",action_name);
#endif
    i_command_process(w_current, action_name, 0, NULL, ID_ORIGIN_MENU);
  }
  else {
    if (strncmp (action_name, "buffer-", 7) == 0 ) {
      menu_action = u_string_concat ( action_name, "-menu", NULL);
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

/*! \brief  Execute Main Popup Menu Selection
 *  \par Function Description
 *  This functions essentialy performs the same action as the preceeding
 * main menu function but there is no Scheme involved and commands are
 * known to be valid. The second argument to i_command_process is the
 * action string referenced in the static string structure IDS_Popup_
 * Actions using the enumerated integer from pop_MenuItem.
 */
static void
x_menu_popup_execute(GtkObject *widget, int action_id)
{
  GschemToplevel *w_current;
  const char     *action;

  w_current  = gtk_object_get_data(widget, "top-level");
  action     = IDS_Popup_Actions[action_id];
#if DEBUG
    fprintf(stderr, "<x_menu_popup_execute> procssing popup menu action %s\n",action);
#endif
  i_command_process(w_current, action, 0, NULL, ID_ORIGIN_MOUSE);
}

static void free_toggler (void *data_record, void *user_data)
{
  ToggleMenuData *toggler_data = data_record;

  g_free(toggler_data->toggle_name);
  g_free(toggler_data->menu_item_name);
  g_free(toggler_data->menu_path);
  g_free(toggler_data);
}

/*! \brief Menu->Destroy->Toggle Items
 *  \par Function Description
 *  This function is called by gschem_quit to free each ToggleMenuData
 *  structure that was allocated for toggle menu items.
 */
void x_menu_free_all() {

  lambda (MenuData *menu_data){
    g_slist_free (MENU_ITEMS_LIST);
    g_slist_free (POPUP_ITEMS_LIST);
    g_slist_foreach(TOGGLERS_LIST, free_toggler, NULL);
    g_slist_free (TOGGLERS_LIST);
    g_hash_table_unref (POPUP_HASH_TABLE);
    g_free(menu_data);
    return FALSE;
  }
  mapcar(ui_list)
  i_menu_free();
}

/*! \brief Get Pointer to Main Menu Bar
 *  \par Function Description
 * This function retrieves a pointer to the main menu bar
 *
 * \retval GtkMenu pointer disguised as a GtkWidget
 */
GtkWidget *get_main_menu(GschemToplevel *w_current) {
  MenuData *menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
  return MENU_BAR;
}

/*! \brief Create Main Menu
 *  \par Function Description
 * This function creates the main menu based on data in a Scheme list that
 * was create during startup when an RC file executed. Presummably the
 * orginal intend was to allow for menu customization and this is possible.
 * A side-effects is that if there is a single error in the rc/scheme file,
 * the menu data does not get defined!
 * TODO: May should create a Pre-create menu function that checks for
 * existence of menu data and substitutes a "fall-back" menu if needed, WEH.
 */
GtkWidget *x_menu_setup_ui(GschemToplevel *w_current)
{
  EdaConfig    *cfg;
  const char   *group = MENU_CONFIG_GROUP;

  GedaAction   *action;
  GtkWidget    *image;
  GtkWidget    *menu_item;
  GtkWidget    *root_menu;
  GtkWidget    *menu;

  int scm_items_len;
  int scm_item_len;

  SCM scm_items;
  SCM scm_item;
  SCM scm_item_name;
  SCM scm_item_tip;
  SCM scm_item_func;
  SCM scm_item_stock;
  SCM scm_index;

  char  *buf;
  char  *menu_name;
  char  *action_name;
  char  *action_keys;
  char  *dummy = NULL;
  const char  *menu_item_name;
  char  *menu_item_keys;
  char  *menu_item_tip;
  char  *menu_item_stock;
  char  *raw_menu_item_name;
  char **raw_menu_name = &dummy;

  unsigned long handler;
  int i, j;
  bool menus_broken;
  bool is_a_toggle;
  ToggleMenuData *toggler_data;

  /* Glib-2.40 generates console noise from gtk-lib */
  MenuData *menu_data = GEDA_MEM_ALLOC0 (sizeof(MenuData));
  MENU_BAR = gtk_menu_bar_new ();
  MENU_ITEMS_LIST = NULL;

  void setup_radio(GtkCheckMenuItem *radio_button, void *func) {
    RadioMenuData *radio_data;

    radio_data            = GEDA_MEM_ALLOC0 (sizeof(RadioMenuData));
    radio_data->w_current = w_current;
    radio_data->widget    = (GtkCheckMenuItem *)radio_button;

    radio_data->handler = g_signal_connect (G_OBJECT(radio_button), "toggled",
                                            G_CALLBACK(func),
                                            w_current);

    w_current->toolbar_mode_grp = g_slist_append ( w_current->toolbar_mode_grp, radio_data);
  }

  void
  menu_register_toggler (ToggleMenuData *toggler_data) {
    TOGGLERS_LIST = g_slist_append(TOGGLERS_LIST, toggler_data);
  }

  cfg = eda_config_get_user_context ();

  bool show_menu_icons;
  bool show_menu_tips;
  bool show_pop_icons;
  bool show_pop_tips;

  show_menu_icons = eda_config_get_boolean (cfg, group, "show-menu-icons",  NULL);
  show_menu_tips  = eda_config_get_boolean (cfg, group, "show-menu-tips",   NULL);
  show_pop_icons  = eda_config_get_boolean (cfg, group, "show-popup-icons", NULL);
  show_pop_tips   = eda_config_get_boolean (cfg, group, "show-popup-tips",  NULL);

  menus_broken = FALSE;
  toggler_data = NULL;
  scm_dynwind_begin (0);
  g_dynwind_window (w_current);

  for (i = 0 ; i < i_menu_return_num(); i++) {

    scm_items = i_menu_return_entry(i, raw_menu_name);

    if (*raw_menu_name == NULL) {
      g_warning(_("Oops.. got a NULL menu name in get_main_menu()\n"));
      return NULL;
    }
    /* Glib-2.40 generates console noise from gtk-lib */
    menu = gtk_menu_new();

    menu_item = gtk_tearoff_menu_item_new ();
    gtk_container_add (GTK_CONTAINER (menu), menu_item);
    g_object_set (menu_item, "visible", TRUE, NULL);

    scm_items_len = (int) scm_ilength (scm_items);
    for (j = 0 ; j < scm_items_len; j++) {

      scm_index      = scm_from_int (j);
      scm_item       = scm_list_ref (scm_items, scm_index);
      scm_item_len   = scm_ilength (scm_item);
      scm_item_name  = SCM_CAR (scm_item);
      scm_item_func  = SCM_CADR (scm_item);
      scm_item_stock = scm_is_pair (SCM_CDDR (scm_item)) ? SCM_CADDR (scm_item) : SCM_BOOL_F;

      /* Check the first member */
      if ( !scm_is_string(scm_item_name)) {
        if ( !menus_broken ) /* Issue message only for first occurence */
          g_warning (_("Error reading menu item <%d>, Bad string\n"), i);
        else
          u_log_message(_("Error reading menu item <%d>, Bad string\n"), i);
        menus_broken = TRUE;
        continue;
      }

      SCM_ASSERT(scm_is_symbol (scm_item_func) || scm_is_false (scm_item_func),
                 scm_item_func, SCM_ARGn, "get_main_menu item_func");

      SCM_ASSERT(scm_is_string (scm_item_stock) || scm_is_false (scm_item_stock),
                 scm_item_stock, SCM_ARGn, "get_main_menu stock");

      /* check for a 4th parameter = tooltip string */
      if(scm_item_len == 4) {
        scm_item_tip = SCM_CAR (scm_cdddr (scm_item ));     /* Extract tooltip string */

        if (scm_is_string(scm_item_tip)) {                   /* Validate that it really is a string */
          menu_item_tip = scm_to_utf8_string (scm_item_tip); /* if valid, convert to c string */
        }
        else { menu_item_tip = NULL; }
      }
      else { menu_item_tip = NULL; }
      /* End tool-tip retrieval */

      raw_menu_item_name = scm_to_utf8_string(scm_item_name);

      scm_dynwind_begin(0);
      scm_dynwind_free(raw_menu_item_name);

      menu_item_name = gettext(raw_menu_item_name);

      if (strcmp(menu_item_name, "SEPARATOR") == 0) {
        menu_item = gtk_menu_item_new();
      }
      else {

        menu_item_stock = scm_is_false (scm_item_stock) ? NULL : scm_to_utf8_string (scm_item_stock);

        if(scm_is_false (scm_item_func)) {
          if (menu_item_stock) {

            menu_item = geda_image_menu_item_new_with_mnemonic(menu_item_name);

            image = gtk_image_new_from_icon_name (menu_item_stock, GTK_ICON_SIZE_MENU);

            /* Pre Gtk-2.6 */
            //image =  GTK_WIDGET(gtk_image_new_from_stock(menu_item_stock, GTK_ICON_SIZE_MENU));

            g_object_set (image,
                          "no-show-all",       TRUE,
                          "visible", show_menu_icons,
                          NULL);

            g_object_set (menu_item,
                          "image", image,
                          "show-image", show_menu_icons,
                          NULL);

          }
          else {
            menu_item = gtk_menu_item_new_with_mnemonic(menu_item_name);
          }
        }
        else {

          action_name = scm_to_utf8_string (scm_symbol_to_string (scm_item_func));

          action_keys = g_find_key(action_name);

          if ( !action_keys ) {
            menu_item_keys = NULL;
          }
          else {
            menu_item_keys = action_keys;
          }
          is_a_toggle = FALSE;
          if (strncmp (menu_item_name, "Toggle", 6) == 0 ) {
            is_a_toggle = TRUE;
            toggler_data                 = GEDA_MEM_ALLOC0(sizeof(ToggleMenuData));
            toggler_data->w_current      = w_current;
            toggler_data->menu_item_name = u_string_strdup(menu_item_name);
            toggler_data->menu_path      = u_string_concat (*raw_menu_name, "/", raw_menu_item_name, NULL);
            menu_item_name = menu_item_name + 7;                 /* is just for label */
            /* TODO: Tooltip don't work here, we will fix them later*/
            action = (GedaAction *)
            geda_toggle_action_new (action_name,     /* Action name */
                                    menu_item_name,  /* Text */
                                    menu_item_tip ? menu_item_tip : menu_item_name,
                                    menu_item_stock, /* Icon stock ID */
                                    menu_item_keys); /* Accelerator string */

            menu_item = gtk_action_create_menu_item (GTK_ACTION (action));

          }
          else {

            action = geda_action_new (action_name,     /* Action name */
                                      menu_item_name,  /* Text */
                                      menu_item_tip ? menu_item_tip : menu_item_name,  /* Tooltip */
                                      menu_item_stock, /* Icon stock ID */
                                      menu_item_keys); /* Accelerator string */

            menu_item = geda_action_create_menu_item (GEDA_ACTION(action));

          }

          free(action_name);

          if (action_keys) {
            g_free(action_keys);
            action_keys = NULL;
          }

          handler = g_signal_connect (G_OBJECT(action), "activate",
                                      G_CALLBACK(g_menu_execute),
                                      w_current);

          if(is_a_toggle) {
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

        if (menu_item_tip) { /* If tip not NULL then attach tip to menu widget */
          gtk_widget_set_tooltip_text(menu_item, _(menu_item_tip));
          g_object_set (menu_item, "has-tooltip", show_menu_tips, NULL);
          free(menu_item_tip);
        }
        free(menu_item_stock);
      }

      gtk_container_add (GTK_CONTAINER (menu), menu_item);
      g_object_set (menu_item, "visible", TRUE, NULL);

      /* add a handle to the menu_bar object to get access to widget objects */
      /* This string should NOT be internationalized */
      buf = g_strdup_printf("%s/%s", *raw_menu_name, raw_menu_item_name);
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), buf, menu_item);
      GEDA_FREE(buf);

      scm_dynwind_end();
    }

    menu_name = (char *) gettext(*raw_menu_name);
    root_menu = gtk_menu_item_new_with_mnemonic (menu_name);

    /* do not free *raw_menu_name */
    /* no longer right justify the help menu since that has gone out of style */
    g_object_set (root_menu, "visible", TRUE, NULL);

    gtk_menu_item_set_submenu (GTK_MENU_ITEM (root_menu), menu);
    gtk_container_add (GTK_CONTAINER (MENU_BAR), root_menu);
  }
  scm_dynwind_end ();

  menu_item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(MENU_BAR), "_View/_Redraw");
  if( menu_item != NULL) {

    GtkContainer *menu = GTK_CONTAINER (gtk_widget_get_parent (menu_item));
    GtkWidget    *toggle_menu;

    if( w_current->toolbars == TRUE ) {

      /* Toolbar Options*/
      menu_item   = gtk_menu_item_new_with_mnemonic("_Toolbars");
      toggle_menu = gtk_menu_new();

      gtk_menu_item_set_submenu ( GTK_MENU_ITEM( menu_item ) , toggle_menu ) ;
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), "_View/_Toolbars", menu_item);

      GtkWidget *stdbar_toggle   = gtk_check_menu_item_new_with_mnemonic ( "_Standard" );
      GtkWidget *selbar_toggle   = gtk_check_menu_item_new_with_mnemonic ( "Se_lect" );
      GtkWidget *pagebar_toggle  = gtk_check_menu_item_new_with_mnemonic ( "_Page" );
      GtkWidget *addbar_toggle   = gtk_check_menu_item_new_with_mnemonic ( "_Add" );
      GtkWidget *zoombar_toggle  = gtk_check_menu_item_new_with_mnemonic ( "_Zoom" );
      GtkWidget *editbar_toggle  = gtk_check_menu_item_new_with_mnemonic ( "_Edit" );
      GtkWidget *attribar_toggle = gtk_check_menu_item_new_with_mnemonic ( "Attrib_ute" );
      GtkWidget *gridbar_toggle  = gtk_check_menu_item_new_with_mnemonic ( "_Grid Snap" );

      gtk_check_menu_item_set_active((GtkCheckMenuItem *)stdbar_toggle,   TRUE);
      gtk_check_menu_item_set_active((GtkCheckMenuItem *)selbar_toggle,   TRUE);
      gtk_check_menu_item_set_active((GtkCheckMenuItem *)pagebar_toggle,  TRUE);
      gtk_check_menu_item_set_active((GtkCheckMenuItem *)addbar_toggle,   TRUE);
      gtk_check_menu_item_set_active((GtkCheckMenuItem *)zoombar_toggle,  TRUE);
      gtk_check_menu_item_set_active((GtkCheckMenuItem *)editbar_toggle,  TRUE);
      gtk_check_menu_item_set_active((GtkCheckMenuItem *)attribar_toggle, TRUE);
      gtk_check_menu_item_set_active((GtkCheckMenuItem *)gridbar_toggle,  TRUE);

      /* Normally the ui manager would do this for us but we don't have one so...*/
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_STDBAR_MENU_PATH,  stdbar_toggle);
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_SELBAR_MENU_PATH,  selbar_toggle);
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_PageBAR_MENU_PATH, pagebar_toggle);
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_ADDBAR_MENU_PATH,  addbar_toggle);
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_ZOOMBAR_MENU_PATH, zoombar_toggle);
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_EDITBAR_MENU_PATH, editbar_toggle);
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_ATTRBAR_MENU_PATH, attribar_toggle);
      gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_GRIDBAR_MENU_PATH, gridbar_toggle);

      gtk_container_add (GTK_CONTAINER (toggle_menu), stdbar_toggle);
      gtk_container_add (GTK_CONTAINER (toggle_menu), selbar_toggle);
      gtk_container_add (GTK_CONTAINER (toggle_menu), pagebar_toggle);
      gtk_container_add (GTK_CONTAINER (toggle_menu), addbar_toggle);
      gtk_container_add (GTK_CONTAINER (toggle_menu), zoombar_toggle);
      gtk_container_add (GTK_CONTAINER (toggle_menu), editbar_toggle);
      gtk_container_add (GTK_CONTAINER (toggle_menu), attribar_toggle);
      gtk_container_add (GTK_CONTAINER (toggle_menu), gridbar_toggle);

      gtk_widget_set_tooltip_text(stdbar_toggle, _("Toggle visibility of the Standard toolbar"));
      gtk_widget_set_tooltip_text(selbar_toggle, _("Toggle visibility of the Selection toolbar"));
      gtk_widget_set_tooltip_text(pagebar_toggle, _("Toggle visibility of the Page toolbar"));
      gtk_widget_set_tooltip_text(addbar_toggle,  _("Toggle visibility of the Add toolbar"));
      gtk_widget_set_tooltip_text(zoombar_toggle,  _("Toggle visibility of the Zoom toolbar"));
      gtk_widget_set_tooltip_text(editbar_toggle,  _("Toggle visibility of the Edit toolbar"));
      gtk_widget_set_tooltip_text(attribar_toggle,  _("Toggle visibility of the Attributes toolbar"));
      gtk_widget_set_tooltip_text(gridbar_toggle,   _("Toggle visibility of the Grid/Snap toolbar"));

      GtkWidget* tb_separator_1  = gtk_menu_item_new();
      gtk_container_add (GTK_CONTAINER (toggle_menu), tb_separator_1);

      /* Start Toolbar Mode Radios */
      GtkWidget *tb_icons_bulb = gtk_check_menu_item_new_with_mnemonic ( "_Icons" );
      GtkWidget *tb_text_bulb  = gtk_check_menu_item_new_with_mnemonic (  "_Text" );
      GtkWidget *tb_vert_bulb  = gtk_check_menu_item_new_with_mnemonic ( "Both _Vertical" );
      GtkWidget *tb_hori_bulb  = gtk_check_menu_item_new_with_mnemonic ( "Both _Horizontal" );

      g_object_set (G_OBJECT(tb_icons_bulb), "draw-as-radio", TRUE,  NULL);
      g_object_set (G_OBJECT(tb_text_bulb),  "draw-as-radio", TRUE, NULL);
      g_object_set (G_OBJECT(tb_vert_bulb),  "draw-as-radio", TRUE, NULL);
      g_object_set (G_OBJECT(tb_hori_bulb),  "draw-as-radio", TRUE, NULL);

      g_object_set (G_OBJECT(tb_icons_bulb), "active", FALSE, NULL);
      g_object_set (G_OBJECT(tb_text_bulb),  "active", FALSE, NULL);
      g_object_set (G_OBJECT(tb_vert_bulb),  "active", FALSE, NULL);
      g_object_set (G_OBJECT(tb_hori_bulb),  "active", FALSE, NULL);

      if (w_current->toolbars_mode == TOOLBAR_SHOW_ICONS ) {
        g_object_set (G_OBJECT(tb_icons_bulb), "active", TRUE,  NULL);
      }
      else {
        if (w_current->toolbars_mode == TOOLBAR_SHOW_TEXT ) {
          g_object_set (G_OBJECT(tb_text_bulb),  "active", TRUE,  NULL);
        }
        else {
          if (w_current->toolbars_mode == TOOLBAR_SHOW_BOTH ) {
            g_object_set (G_OBJECT(tb_vert_bulb),  "active", TRUE,  NULL);
          }
          else {
            if (w_current->toolbars_mode == TOOLBAR_SHOW_HORIZ ) {
              g_object_set (G_OBJECT(tb_hori_bulb),  "active", TRUE,  NULL);
            } } } }

            setup_radio((GtkCheckMenuItem*)tb_icons_bulb, x_toolbar_icons_only);
            setup_radio((GtkCheckMenuItem*)tb_text_bulb,  x_toolbar_text_only);
            setup_radio((GtkCheckMenuItem*)tb_vert_bulb,  x_toolbar_display_both);
            setup_radio((GtkCheckMenuItem*)tb_hori_bulb,  x_toolbar_display_horiz);

            gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_BAR_ICON_MENU_PATH, tb_icons_bulb);
            gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_BAR_TEXT_MENU_PATH, tb_text_bulb);
            gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_BAR_VERT_MENU_PATH, tb_vert_bulb);
            gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_BAR_HOZI_MENU_PATH, tb_hori_bulb);

            gtk_container_add (GTK_CONTAINER (toggle_menu), tb_icons_bulb);
            gtk_container_add (GTK_CONTAINER (toggle_menu), tb_text_bulb);
            gtk_container_add (GTK_CONTAINER (toggle_menu), tb_vert_bulb);
            gtk_container_add (GTK_CONTAINER (toggle_menu), tb_hori_bulb);

            gtk_widget_set_tooltip_text(tb_icons_bulb, _("Display Icons on the toolbar"));
            gtk_widget_set_tooltip_text(tb_text_bulb,  _("Display Text on the toolbar"));
            gtk_widget_set_tooltip_text(tb_vert_bulb,  _("Display Icons and Text vertically on the toolbar"));
            gtk_widget_set_tooltip_text(tb_hori_bulb,  _("Display Icons and Text horizontally on the toolbar"));

            gtk_menu_shell_prepend((GtkMenuShell *)menu, menu_item);

            g_signal_connect (G_OBJECT(stdbar_toggle), "toggled",
                                       G_CALLBACK(x_window_standard_toolbar_toggle),
                                       w_current);
            g_signal_connect (G_OBJECT(selbar_toggle), "toggled",
                                       G_CALLBACK(x_window_select_toolbar_toggle),
                                       w_current);
            g_signal_connect (G_OBJECT(pagebar_toggle), "toggled",
                                       G_CALLBACK(x_window_page_toolbar_toggle),
                                       w_current);
            g_signal_connect (G_OBJECT(addbar_toggle), "toggled",
                                       G_CALLBACK(x_window_add_toolbar_toggle),
                                       w_current);
            g_signal_connect (G_OBJECT(zoombar_toggle), "toggled",
                                       G_CALLBACK(x_window_zoom_toolbar_toggle),
                                       w_current);
            g_signal_connect (G_OBJECT(editbar_toggle), "toggled",
                                       G_CALLBACK(x_window_edit_toolbar_toggle),
                                       w_current);
            g_signal_connect (G_OBJECT(attribar_toggle), "toggled",
                                       G_CALLBACK(x_window_attribute_toolbar_toggle),
                                       w_current);
            g_signal_connect (G_OBJECT(gridbar_toggle), "toggled",
                                       G_CALLBACK(x_window_gridsnap_toolbar_toggle),
                                       w_current);

            gtk_widget_show_all(menu_item);
    }

    /* Menu Options */
    menu_item   = gtk_menu_item_new_with_mnemonic("_Menu");
    toggle_menu = gtk_menu_new();

    gtk_menu_item_set_submenu ( GTK_MENU_ITEM( menu_item ) , toggle_menu ) ;
    gtk_object_set_data(GTK_OBJECT(MENU_BAR), "_View/_Menu", menu_item);

    GtkWidget *menu_icons_toggle   = gtk_check_menu_item_new_with_mnemonic ( "_Icons" );
    GtkWidget *menu_tips_toggle    = gtk_check_menu_item_new_with_mnemonic ( "_ToolTips" );
    GtkWidget *menu_popcons_toggle = gtk_check_menu_item_new_with_mnemonic ( "_Context Icons" );
    GtkWidget *menu_poptips_toggle = gtk_check_menu_item_new_with_mnemonic ( "Context Tip_s" );

    g_object_set (G_OBJECT(menu_icons_toggle),   "draw-as-radio", TRUE, NULL);
    g_object_set (G_OBJECT(menu_tips_toggle),    "draw-as-radio", TRUE, NULL);
    g_object_set (G_OBJECT(menu_popcons_toggle), "draw-as-radio", TRUE, NULL);
    g_object_set (G_OBJECT(menu_poptips_toggle), "draw-as-radio", TRUE, NULL);

    gtk_check_menu_item_set_active((GtkCheckMenuItem *)menu_icons_toggle,   show_menu_icons);
    gtk_check_menu_item_set_active((GtkCheckMenuItem *)menu_tips_toggle,    show_menu_tips);
    gtk_check_menu_item_set_active((GtkCheckMenuItem *)menu_popcons_toggle, show_pop_icons);
    gtk_check_menu_item_set_active((GtkCheckMenuItem *)menu_poptips_toggle, show_pop_tips);

    gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_ICON_MENU_PATH,    menu_icons_toggle);
    gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_TIPS_MENU_PATH,    menu_tips_toggle);
    gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_POPCONS_MENU_PATH, menu_popcons_toggle);
    gtk_object_set_data(GTK_OBJECT(MENU_BAR), OPT_POPTIPS_MENU_PATH, menu_poptips_toggle);

    gtk_container_add (GTK_CONTAINER (toggle_menu), menu_icons_toggle);
    gtk_container_add (GTK_CONTAINER (toggle_menu), menu_tips_toggle);
    gtk_container_add (GTK_CONTAINER (toggle_menu), menu_popcons_toggle);
    gtk_container_add (GTK_CONTAINER (toggle_menu), menu_poptips_toggle);

    gtk_widget_set_tooltip_text(menu_icons_toggle, _("Toggle visibility of main menu icons"));
    gtk_widget_set_tooltip_text(menu_tips_toggle,  _("Toggle main menu tooltips"));
    gtk_widget_set_tooltip_text(menu_popcons_toggle, _("Toggle visibility of main context menu icons"));
    gtk_widget_set_tooltip_text(menu_poptips_toggle, _("Toggle main context menu tooltips"));

    gtk_menu_shell_prepend((GtkMenuShell *)menu, menu_item);

    g_signal_connect (G_OBJECT(menu_icons_toggle), "toggled",
                      G_CALLBACK(x_menu_toggle_icons),
                      MENU_ITEMS_LIST);

    g_signal_connect (G_OBJECT(menu_tips_toggle), "toggled",
                      G_CALLBACK(x_menu_toggle_tips),
                      MENU_ITEMS_LIST);

    gtk_widget_show_all(menu_item);
  }
  else
    g_warning("No Menu!");

  ui_list = g_slist_append(ui_list, menu_data);
  w_current->ui_index = g_slist_length(ui_list) -1;
  return MENU_BAR;
}

/** \defgroup Main-Context-Menu Mouse Menu Functions */

static bool strhashcmp (const void *a, const void *b) {
  int answer = 0;
  if (((char*)a != '\0') && ((char*)b != '\0')) {
     answer = strcmp ((const char*) a, (const char*) b) == 0;
  }
  return answer;
}

/*! \brief Setup Main Popup Context Menu
 *  \par Function Description
 *  Creates the main context pop-up menu and connects callback to options in
 *  the main menu to control icons and tool-tip visibility. The pop-up menu
 *  is created using the data in the popup_items data structure. A pointer
 *  to each menu-item widget is saved in the single-linked list menu_data->
 *  popup_items using the macro POPUP_ITEMS_LIST. The POPUP_ITEMS_LIST list
 *  is used to toggle visibility of icon images and tool-tip on all of the
 *  pop-up menu items.
 *  A pointer to the menu is saved in menu_data->popup_menu using the macro
 *  POPUP_MENU.
 */
int x_menu_setup_popup (GschemToplevel *w_current)
{
  EdaConfig    *cfg   = NULL;
  const char   *group = MENU_CONFIG_GROUP;
  GtkWidget    *menu;
  GtkWidget    *menu_item;
  GtkWidget    *submenu;
  GtkWidget    *save_nest;
  GtkWidget    *image;

  bool show_pop_icons;
  bool show_pop_tips;

  int i;

  MenuData     *menu_data;

  /* We will assume the main menu has already alocated a structure */
  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  menu             = gtk_menu_new ();
  POPUP_ITEMS_LIST = NULL;
  save_nest        = NULL;
  POPUP_HASH_TABLE = g_hash_table_new_full (g_str_hash, (GEqualFunc) strhashcmp,
                                            NULL, NULL);

  /* Retrieve preference settings */
  cfg              = eda_config_get_user_context ();
  show_pop_icons   = eda_config_get_boolean (cfg, group, "show-popup-icons", NULL);
  show_pop_tips    = eda_config_get_boolean (cfg, group, "show-popup-tips",  NULL);

  for (i = 0; popup_items[i].name != NULL; i++) {

    PopupEntry item = popup_items[i];

    if (item.func == NULL) { /* if sub-menu or seperator */

      /* Then is not an action item */
      if (item.action_id == 1) {
        /* Create and add the pop-out submenu item */
        submenu = gtk_menu_item_new_with_label(_(item.name));
        g_object_set (submenu, "visible", TRUE, NULL);
        gtk_container_add (GTK_CONTAINER (menu), submenu);

        /* Save the current menu and create the new sub menu */
        save_nest = menu;
        menu = gtk_menu_new ();

        gtk_menu_item_set_submenu (GTK_MENU_ITEM( submenu ), menu) ;
        g_object_set (menu, "visible", TRUE, NULL);

        POPUP_ITEMS_LIST = g_slist_append(POPUP_ITEMS_LIST, submenu);
        g_hash_table_insert (POPUP_HASH_TABLE, (char*)item.name, submenu);

        continue;
      }
      else {
        if (save_nest != NULL) {
          menu = save_nest;
          save_nest = NULL;
          continue;
        }
        else {  /* add a separator */
          menu_item = gtk_menu_item_new();
        }
      }
    }
    else {

      menu_item = geda_image_menu_item_new_with_label(_(item.name));

      gtk_widget_set_tooltip_text (menu_item, _(item.tip));
      g_object_set (menu_item, "has-tooltip", show_pop_tips, NULL);

      if ( item.use_stock ) {
        image = gtk_image_new_from_stock(item.icon, GTK_ICON_SIZE_MENU);
      } else {
        image = create_pixmap (item.icon);
      }

      geda_image_menu_item_set_image (GEDA_IMAGE_MENU_ITEM(menu_item), image);
      g_object_set (image, "visible", show_pop_icons, NULL);

      /* Connect things up so that the actions get run */
      g_signal_connect (G_OBJECT (menu_item), "activate",
                        (void *) item.func,
                        (void *) item.action_id);

      g_object_set_data( G_OBJECT(menu_item), "top-level", w_current);
      POPUP_ITEMS_LIST = g_slist_append(POPUP_ITEMS_LIST, menu_item);
      g_hash_table_insert (POPUP_HASH_TABLE, (char*)item.name, menu_item);
    }

    g_object_set (menu_item, "visible", TRUE, NULL);
    gtk_container_add (GTK_CONTAINER (menu), menu_item);
  }

  /* Save the menu to the active menu data structure */
  POPUP_MENU = menu;

  menu = MENU_BAR; /* Get pointer to the main menu */

  if (GTK_IS_MENU_BAR(menu)) {

    /* Setup the callback for the main menu options */
    char *popcons_path = OPT_POPCONS_MENU_PATH;
    char *poptips_path = OPT_POPTIPS_MENU_PATH;

    menu_item = (GtkWidget*) gtk_object_get_data(GTK_OBJECT(menu), popcons_path);
    if (GTK_IS_MENU_ITEM(menu_item)) {

      g_signal_connect (G_OBJECT(menu_item), "toggled",
                        G_CALLBACK(x_menu_toggle_icons),
                        POPUP_ITEMS_LIST);
    }

    menu_item = (GtkWidget*) gtk_object_get_data(GTK_OBJECT(menu), poptips_path);
    if (GTK_IS_MENU_ITEM(menu_item)) {

      g_signal_connect (G_OBJECT(menu_item), "toggled",
                        G_CALLBACK(x_menu_toggle_tips),
                        POPUP_ITEMS_LIST);
    }

  }

  return GTK_IS_WIDGET(POPUP_MENU);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  need to look at this... here and the setup
 */
int x_menu_display_popup (GschemToplevel *w_current, GdkEventButton *event)
{
  GtkWidget *menu;
  MenuData  *menu_data;

  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  menu = POPUP_MENU;

  if (menu == NULL) {
    g_warning( "Internal Error, <do_popup> got null popup menu\n");
  }
  else {
    w_current->pointer_sx = event->x;
    w_current->pointer_sy = event->y;
    gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                    event->button, event->time);
  }
  return FALSE;
}

/** @} END Group Main-Context-Menu (the main popup menu) */

/** \defgroup Main-Menu-Support Functions to support the Main Menu
 *  @{ \par
 *         The functions in this group, mostly callbacks, support
 *         main menu.
 */

/*! \brief Set Sensitivity of Main Menu Item
 *  \par Function Description
 *  This function is called from i_basic to set the senitivity of menu items!
 */
void x_menus_sensitivity (GschemToplevel *w_current, const char *buf, int flag)
{
  GtkWidget *item=NULL;
  GtkWidget *menubar;
  static int sensitivity_errors = 0;

  if (!buf) {
    return;
  }

  menubar = get_main_menu(w_current);

  if (GTK_IS_MENU_BAR(menubar)) {

    item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(menubar), buf);

    if (item && GTK_IS_MENU_ITEM(item)) {
      gtk_widget_set_sensitive(GTK_WIDGET(item), flag);
      /* item = pointer to menu widget -- don't free here */
    }
    else {
      if (verbose_mode) {
        u_log_message(_("Tried to set the sensitivity on non-existent menu item '%s'\n"), buf);
      }
      else {
        if (sensitivity_errors < SENSITIVITY_ERROR_LIMIT) {
          q_log_message(_("Tried to set the sensitivity on non-existent menu item '%s',\n"), buf);
        }
        sensitivity_errors++;
        if (sensitivity_errors == SENSITIVITY_ERROR_LIMIT) {
          q_log_message(_("Excessive errors <%d>, disabling sensitivity warnings\n"), sensitivity_errors);
        }
      }
    }
  }
}

/*! \brief Set Sensitivity of Popup Menu Item
 *  \par Function Description
 *  This function sets the sensitivity of the items in the right button
 *  popup.
 *
 */
void x_menus_popup_sensitivity (GschemToplevel *w_current, const char *name, int flag)
{
  GtkWidget *menu_item;
  MenuData  *menu_data;

  menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  if (!POPUP_MENU) {
    g_warning(_("Popup menu widget doesn't exist!\n"));
  }
  else {

    menu_item = (GtkWidget*) g_hash_table_lookup (POPUP_HASH_TABLE, name);

    if (menu_item) {
      gtk_widget_set_sensitive(GTK_WIDGET(menu_item), flag);
    }
    else {
      fprintf(stderr, "%s popup item non-existent <%s>\n", __func__, name);
    }
  }
}


/*! \brief Save the State of Main Menu Toggle Options
 *  \brief *  \par Function Description
 *  This function restrives and save the state of the non-toolbar toggle
 *  menu items (like rubber-mode) to the user's configuration file popup.
 *
 */
void x_menu_save_state(GschemToplevel *w_current) {

  GtkWidget        *menubar;
  EdaConfig        *cfg;

  char *icons_path   = OPT_ICON_MENU_PATH;      /* Menu Paths */
  char *tooltip_path = OPT_TIPS_MENU_PATH;
  char *popcons_path = OPT_POPCONS_MENU_PATH;
  char *poptips_path = OPT_POPTIPS_MENU_PATH;

  bool  state;
  int   errors = 0;

  void save_menu_toggler_state(const char* key, const char* path) {
    GtkCheckMenuItem *toggler =
   (GtkCheckMenuItem*) gtk_object_get_data(GTK_OBJECT(menubar), path);
    if (GTK_IS_CHECK_MENU_ITEM(toggler)) {
      state = gtk_check_menu_item_get_active (toggler);
      eda_config_set_boolean (cfg, MENU_CONFIG_GROUP, key, state);
    }
    else {
      errors++;
    }
  }

  menubar = get_main_menu(w_current);

  if (menubar != NULL && GTK_IS_MENU_BAR(menubar)) {

     v_log_message( _("Saving menu toolbar options..."));

     cfg = eda_config_get_user_context ();

     save_menu_toggler_state("show-menu-icons",  icons_path);
     save_menu_toggler_state("show-menu-tips",   tooltip_path);
     save_menu_toggler_state("show-popup-icons", popcons_path);
     save_menu_toggler_state("show-popup-tips",  poptips_path);

     if (errors == 0) {
       v_log_message( _(" done\n"));
     }
     else {
       v_log_message( _(" there were %d errors\n"), errors);
     }
  }
}

/*! \brief Set Menu Icon Visibility
 *  \par Function Description
 *   This function turns menu icons on or off for a given list of menu items
 * based on the state argument. The list is a glist of all main menu items or
 * all pop-menu items. This function is the only function that actually changes
 * the visiblity of non-toggle type menu items.
 */
static void
x_menu_lowlevel_set_icon_visibility (GSList* list, bool state)
{
  lambda (GObject* menu_item) {

    if ( GEDA_IS_IMAGE_MENU_ITEM(menu_item)) {
      g_object_set (menu_item, "show-image", state, NULL);
    }
    else {
      g_warning(_("<x_menu_toggle_icons> Ignoring invalid object, maybe a seperator\n"));
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
/*! \brief Set Menu Icon Visibility
 *  \par Function Description
 *   This function exist so the menu icons can turn off after the main menu
 * is built. Setting "show-image", as was done in x_menu_setup_ui
 * works for gnome display managers but not with Mate, which seems to try
 * and enforce a global system-wide setting. Gschem's user settings takes
 * precedence, so we use this function as a work-around.
 */
void x_menu_set_icon_visibility(GschemToplevel *w_current, bool state)
{
  MenuData *menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
  x_menu_lowlevel_set_icon_visibility(MENU_ITEMS_LIST, state);
}
/*! \brief Toggle Menu Icon Visibility
 *  \par Function Description
 *   Callback function calls x_menu_lowlevel_set_icon_visibility to turn
 * menu icon on or off based on the state of the toggle menu item pointer
 * pointed to by widget. This is a callback for the toggle menu icons
 * option so widget is a toggle item. The list is a glist of all menu
 * items or all pop-menu items.
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

  state = gtk_check_menu_item_get_active ((GtkCheckMenuItem*)widget);
  x_menu_lowlevel_set_icon_visibility(list, state);
}

/*! \brief Enable Disable Menu Tool Tips
 *  \par Function Description
 *   Callback function turns menu tips on or off based on the state of
 * the toggle menu item pointed to by widget. This is a callback for
 * the toggle tips menu option so widget is the toggle item. The list
 * is a glist of all menu items or all pop-menu items.
 *
 */
static void x_menu_toggle_tips(GtkWidget *widget, GSList* list)
{
  int state;

  state = gtk_check_menu_item_get_active ((GtkCheckMenuItem*)widget);

  lambda (GObject* menu_item) {
    g_object_set (menu_item, "has-tooltip", state, NULL);
    return FALSE;
  }
  mapcar(list)
  if (state) {
    v_log_message(_("gschem: Enabling menu tooltips\n"));
  }
  else {
    v_log_message(_("gschem: Disabling menu tooltips\n"));
  }
}

/** \defgroup menu-toggle-action Menu Toggle Action Support Functions
 *
 *  \par
 *     The Menu toggles buttons need the "activate" signal blocked
 *   temporily so we don't have recursion with callbacks.
 */

/*! \brief Set State of Menu Toggle Items - Low LeveL
 *  \par Function Description
 *
 */
static void menu_set_toggle(ToggleMenuData *toggler_data, bool state) {

  char* menu_path;
  GschemToplevel *w_current;
  GtkWidget* menubar;
  GtkWidget *menu_item;
  GtkAction *action;

  menu_path  = toggler_data->menu_path;
  w_current  = toggler_data->w_current;
  menubar    = get_main_menu(w_current);

  if (menubar != NULL) {
     menu_item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(menubar), menu_path);
     if( menu_item != NULL) {
       /* Get action for this item so we can block the signal */
       action = gtk_widget_get_action(menu_item);
       if( action != NULL) {
         g_signal_handler_block( action, toggler_data->handler);
         gtk_check_menu_item_set_active((GtkCheckMenuItem *)menu_item, state);
         /*re-enable the signal */
         g_signal_handler_unblock( action, toggler_data->handler);
       }
       else
         u_log_message(_("Error, x_menu_set_toggle: Action not found, \"%s\" \n"), menu_path);
     }
     else
       u_log_message(_("Error, x_menu_set_toggle: Menu path not found, \"%s\" \n"), menu_path);
  }
  else
    u_log_message(_("Error, x_menu_set_toggle: invalid pointer [menubar]\n"));
  return;
}

/*! \brief Set State of Menu Toggle Items .
 * \par Function Description
 *  This is a menu support function to "uncheck" menu toggle items. The function
 * can be used when the menu option was changed by some other means, for example
 * when options are turned off with "Hot-Keys".
 *
 * \param w_current Gschem toplevel object
 * \param toggle_id is int index of the ToggleMenuData Glist item to set
 * \param state     is int value to set, either TRUE (check) or FALSE (uncheck).
 *
 * \sa x_menu_set_toolbar_toggle
 */
void x_menu_set_toggle(GschemToplevel *w_current, int toggle_id, bool state){

  int  number_of_togglers;
  ToggleMenuData *toggler_data;
  MenuData *menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

  void set_toggler(int index, bool value) {
    toggler_data = (ToggleMenuData*) g_slist_nth_data (TOGGLERS_LIST, index);
    if(toggler_data)
      menu_set_toggle(toggler_data, value);
  }

  number_of_togglers = g_slist_length(TOGGLERS_LIST);

  if (toggle_id == RESET_TOGGLERS) {
    set_toggler(SNAP_TOGGLE,     (w_current->snap > 0));
    set_toggler(OUTLINE_TOGGLE,  (w_current->action_feedback_mode > 0));
    set_toggler(RUBBER_TOGGLE,   (w_current->netconn_rubberband > 0));
    set_toggler(MAGNETIC_TOGGLE, (w_current->magnetic_net_mode > 0));
    set_toggler(DRAG_CAN_MOVE,   (w_current->drag_can_move > 0));
  }
  else {
   if(toggle_id < number_of_togglers)
     toggler_data = (ToggleMenuData*) g_slist_nth_data (TOGGLERS_LIST, toggle_id);
     menu_set_toggle(toggler_data, state);
  }
  return;
}

/*! \brief Set State of Menu ToolBar Toggle Items
 * \par Function Description
 *  This is a menu support function to "uncheck" Toolbar Menu toggle items. The
 * function can be used when the menu option was changed by some other means,
 * for example when a floating toolbars is turned off with the "X" box.
 *
 * \param w_current Gschem toplevel object
 * \param toggle_id is int index of the IDS_Menu_Toggles item to set
 * \param state     is int value to set, either TRUE (check) or FALSE (uncheck).
 *
 * \sa x_menu_set_toggle
 */
void x_menu_set_toolbar_toggle(GschemToplevel *w_current, int toggle_id, bool state){

  char  menu_name[36] = "_View/_Toolbars/";
  char *menu_path;

  GtkWidget *menu_item;

  GtkWidget* menubar;
  menubar = get_main_menu(w_current);

  menu_path = u_string_concat (menu_name, IDS_Menu_Toolbar_Toggles[toggle_id], NULL);
  menu_item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(menubar), menu_path);
  if( menu_item != NULL) {
    gtk_check_menu_item_set_active((GtkCheckMenuItem *)menu_item, state);
  }
  else
    u_log_message(_("Error, x_menu_set_toolbar_toggle: Did not find path \"%s\"\n"), menu_path);
  GEDA_FREE(menu_path);
  return;
}

/** @} end group menu-toggle-action  */

/* ---------------- Recent Files Menu ---------------- */

/** \defgroup recent-file-menu Recent Files Menu Support Functions
 *
 *  @{ \remark This is the old method, as appose to the GTK Recent
 *  Chooser Manger version. This method seems to work better on Debian
 *  machines and derivatives (which is likely to be the majority) due to
 *  default security policies, which results in ALL the links to recent
 *  files being erased when ever ANY recent file link is accessed by any
 *  one having "administrative" privileges.
 *
 * TODO: The Recent files menu-item is a dynamic object not created by
 *       the regular menu systems, consequenty, no icons is assigned.
 *       The section should assign an icon.
 */

/*! \brief Update Recent Files Menus
 *  \par Function Description
 *   Make all toplevels reflect changes to the recent files list.
 */
static void update_recent_files_menus(void)
{
   GschemToplevel *w_current;
   GtkWidget *submenu, *recent_menu_item;
   GList *iter;
   MenuData *menu_data;

   for (iter = global_window_list; iter != NULL; iter = g_list_next (iter)) {

      w_current = (GschemToplevel *)iter->data;
      menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
      if (MENU_BAR == NULL)
        continue;

      recent_menu_item =
        (GtkWidget *) gtk_object_get_data(GTK_OBJECT(MENU_BAR),
                                          "_File/Open Recen_t");
      if(recent_menu_item == NULL)
         return;

      submenu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(recent_menu_item));
      gtk_widget_destroy(submenu);
      x_menu_attach_recent_files_submenu(w_current);
   }
}

/*! \brief Remove all entries from the recent files
 *         list and update all toplevels.
 */
static void clear_recent_file_list(void *data)
{
   GList *p;

   p = recent_files;
   while(p) {
      GEDA_FREE(p->data);
      p = g_list_next(p);
   }
   g_list_free(recent_files);
   recent_files = NULL;

   update_recent_files_menus();
}

static void
recent_file_free_menu_data (void *data, GClosure *closure) {
  GEDA_FREE (data);
}

static void recent_file_clicked(GtkMenuItem *menuitem, void *user_data)
{
   FILE *fp;
   Page *page;
   RecentMenuData *data = (RecentMenuData *) user_data;
   GschemToplevel *w_current = data->w_current;
   char *filename = data->filename;

   /* Check if the file exists */
   fp = fopen((char *) filename, "r");
   if(fp == NULL) {
      /* Remove this entry from all menus */
      u_log_message(_("Could not open file %s\n"), (char *) filename);
      recent_files = g_list_remove(recent_files, filename);
      update_recent_files_menus();
      return;
   }
   fclose(fp);

   page = x_window_open_page(w_current, (char *)filename);
   x_window_set_current_page(w_current, page);
}

/*! \brief Attach a submenu with filenames to the 'Open Recent'
 *         menu item.
 *
 *  Called from x_window_setup().
 */
void x_menu_attach_recent_files_submenu(GschemToplevel *w_current)
{
   unsigned long id;
   GtkWidget *tmp;
   GtkWidget *recent_menu_item, *recent_submenu;
   MenuData *menu_data;
   GtkWidget *label;

   menu_data = g_slist_nth_data (ui_list, w_current->ui_index);
   recent_menu_item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(MENU_BAR),
                                                        "_File/Open Recen_t");
   if(recent_menu_item == NULL)
      return;

   /* disconnect all unblocked signals */
   while(1) {
      id = g_signal_handler_find(recent_menu_item, G_SIGNAL_MATCH_UNBLOCKED,
            0, 0, NULL, NULL, NULL);
      if(id == 0)
         break;
      gtk_signal_disconnect(recent_menu_item, id);
   }

   recent_submenu = gtk_menu_new();
   GList *p = recent_files;
   while(p) {
     RecentMenuData *menu_data = g_new0 (RecentMenuData, 1);
     menu_data->filename = p->data;
     menu_data->w_current = w_current;
     tmp = gtk_menu_item_new_with_label((char *)p->data);
     g_signal_connect_data (GTK_OBJECT(tmp), "activate",
                            (GCallback) recent_file_clicked,
                            menu_data,
                            (GClosureNotify) recent_file_free_menu_data,
                            0);
     gtk_menu_append(GTK_MENU(recent_submenu), tmp);
     p = g_list_next(p);
   }

   if(recent_files != NULL) {
      /* Append the 'Clear' menu item to the submenu */
      GtkWidget *alignment = gtk_alignment_new(0.5, 0, 0, 0);

      tmp = gtk_menu_item_new();

      label = geda_label_new(_("Clear"));
      gtk_container_add(GTK_CONTAINER(alignment), label);

      gtk_container_add(GTK_CONTAINER(tmp), alignment);

      gtk_signal_connect_object(GTK_OBJECT(tmp), "activate",
            GTK_SIGNAL_FUNC (clear_recent_file_list), NULL);

      gtk_menu_append(GTK_MENU(recent_submenu), gtk_separator_menu_item_new());
      gtk_menu_append(GTK_MENU(recent_submenu), tmp);
   }

   gtk_widget_show_all(recent_submenu);
   g_object_set (recent_submenu, "visible", TRUE, NULL);
   gtk_menu_item_set_submenu(GTK_MENU_ITEM(recent_menu_item), recent_submenu);
}

/*! \brief Add a filename to the list of recent files.
 *
 *  If filename is already in the list, moves it to the head of the
 *  list.
 */
void recent_files_add(const char *filename)
{
   char *basename;
   char *save_fn;
   GError *err = NULL;
   GList *p = recent_files;

   basename = g_path_get_basename(filename);
   if(strstr(basename, "untitled_") == basename) {
      GEDA_FREE(basename);
      return;
   }

   GEDA_FREE(basename);

   /* Normalize the filename. */
   save_fn = f_file_normalize_name (filename, &err);
   if (err != NULL) {
     save_fn = u_string_strdup (filename);
     g_error_free (err);
   }

   /* Check if the file is already in the list.  */
   while (p != NULL) {
     if (strcmp (save_fn, (char *) p->data) == 0) {
       break;
     }
     p = g_list_next (p);
   }

   if (p != NULL) {
     /* Since we found the filename already in the list, move it to
      * the head of the list. */
     GEDA_FREE (save_fn);
     save_fn = (char *) p->data;
     recent_files = g_list_delete_link (recent_files, p);
     recent_files = g_list_prepend (recent_files, save_fn);
   } else {
     /* Otherwise, just add the new filename to the front of the
      * list. */
     recent_files = g_list_prepend (recent_files, save_fn);
   }

   update_recent_files_menus();
}

/*! \brief Make RECENT_FILES_STORE contain an empty file list.
 */
static void recent_files_create_empty()
{
   char *c;
   const char * const tmp[] = { NULL };
   GKeyFile *kf = g_key_file_new();
   char *file = g_build_filename(f_path_user_config (), RECENT_FILES_STORE, NULL);

   g_key_file_set_string_list(kf, "Recent files", "Files", tmp, 0);
   c = g_key_file_to_data(kf, NULL, NULL);
   g_key_file_free(kf);

   g_file_set_contents(file, c, -1, NULL);
   GEDA_FREE(c);
   GEDA_FREE(file);
}

/*! \brief Save the list of recent files to RECENT_FILES_STORE.
 *
 *  \param [in] user_data unused
 */
void recent_files_save(void *user_data)
{
   char *files[MAX_RECENT_FILES];
   int num = 0;
   char *c;
   char *file = g_build_filename(f_path_user_config (), RECENT_FILES_STORE, NULL);

   GList *p = recent_files;
   if(p == NULL) {
      recent_files_create_empty();
      return;
   }

   while((p != NULL) && (num < MAX_RECENT_FILES)) {
     files[num++] = (char *)p->data;
     p = g_list_next(p);
   }

   GKeyFile *kf = g_key_file_new();

   g_key_file_set_string_list(kf, "Recent files", "Files",
         (const char **)files, num);
   c = g_key_file_to_data(kf, NULL, NULL);
   g_file_set_contents(file, c, -1, NULL);

   GEDA_FREE(c);
   GEDA_FREE(file);
   g_key_file_free(kf);
}

/*! \brief Load the recent file list using data from
 *         RECENT_FILES_STORE.
 *
 *  Must be called before any other recent-files-related
 *  functions.
 */
void recent_files_load()
{
   GKeyFile *kf = g_key_file_new();
   char *file = g_build_filename(f_path_user_config (), RECENT_FILES_STORE, NULL);

   if(!g_file_test(file, G_FILE_TEST_EXISTS)) {
     f_path_create(f_path_user_config (), S_IRWXU | S_IRWXG);

      recent_files_create_empty();
   }

   if(!g_key_file_load_from_file(kf, file, G_KEY_FILE_NONE, NULL)) {
      /* error opening key file, create an empty one and try again */
      recent_files_create_empty();
      if(!g_key_file_load_from_file(kf, file, G_KEY_FILE_NONE, NULL))
         return;
   }

   gsize len;
   char **list = g_key_file_get_string_list(kf, "Recent files",
         "Files", &len, NULL);

   if(list == NULL) {
      /* error reading key file, don't bother to correct;
       * just overwrite it with an empty one */
      recent_files_create_empty();
      return;
   }

   while(len > 0) {
      len--;
      recent_files = g_list_prepend(recent_files, list[len]);
   }

   GEDA_FREE(list);
   GEDA_FREE(file);
   g_key_file_free(kf);
}

/* Date: Sept 05, 2012
 * Who:  Wiley E. Hill
 * What  Function: recent_files_last
 * Why:  This Function was added to support the auto_load_last mechanism.
*/
/*! \brief Get the Most Recent Filename
 *  \par Function Description
 *  This function returns a char pointer to the name of the most
 *  recent file loaded.
 *
 *  \return  const char pointer to the filename string
 */
const char *recent_files_last(void) {

   return (g_list_nth_data(recent_files, 0));

}

/** @} end group recent-file-menu */
/** @} end group Main-Menu-Support */
/** @} end group Menu-Module */