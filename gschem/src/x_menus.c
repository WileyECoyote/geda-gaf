/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2012 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>
#include <stdio.h>
#include <glib/gstdio.h>
#include <gschem.h>

#include <widgets.h>
#include <x_menu.h>


#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static GtkItemFactoryEntry popup_items[] = {
  { N_("/Add Net"), 	      NULL, i_callback_add_net,           0, NULL},
  { N_("/Add Attribute..."),  NULL, i_callback_add_attribute,     0, NULL},
  { N_("/Add Component..."),  NULL, i_callback_add_component,     0, NULL},
  { N_("/Add Bus"), 	      NULL, i_callback_add_bus,           0, NULL},
  { N_("/Add Text"), 	      NULL, i_callback_add_text,          0, NULL},
  { "/sep1", NULL, NULL, 0, "<Separator>"},
  { N_("/Zoom In"),           NULL, i_callback_view_zoom_in,      0, NULL},
  { N_("/Zoom Out"),          NULL, i_callback_view_zoom_out,     0, NULL},
  { N_("/Zoom Box"),          NULL, i_callback_view_zoom_box,     0, NULL},
  { N_("/Zoom Extents"),      NULL, i_callback_view_zoom_extents, 0, NULL},
  { "/sep1", NULL, NULL, 0, "<Separator>"},
  { N_("/Select"), 	      NULL, i_callback_edit_select,       0, NULL},
  { N_("/Edit..."),           NULL, i_callback_edit_edit,         0, NULL},
  { N_("/Edit pin type..."),  NULL, i_callback_edit_pin_type,     0, NULL},
  { N_("/Copy"),              NULL, i_callback_edit_copy,         0, NULL},
  { N_("/Move"),              NULL, i_callback_edit_move,         0, NULL},
  { N_("/Delete"),            NULL, i_callback_edit_delete,       0, NULL},
  /* Menu items for hierarchy added by SDB 1.9.2005.  */
  {"/sep1", NULL, NULL, 0, "<Separator>"},
  {N_("/Down Schematic"),     NULL, i_callback_hierarchy_down_schematic, 0, NULL},
  {N_("/Down Symbol"),        NULL, i_callback_hierarchy_down_symbol,    0, NULL},
  {N_("/Up"),                 NULL, i_callback_hierarchy_up,             0, NULL},
};


static GList *recent_files = NULL;

/* These must be in the same order as ID_GSCHEM_Toolbar in x_toolbars.c */
const char* IDS_Menu_Toolbar_Toggles[] = {
  "_Add", "A_ttribute", "_Edit", "_Page", "_Standard", "_Zoom", /* ToolBar Menu Strings*/
  NULL
};

const char* IDS_Menu_Toggles[] = {
  "Snap On-Off", "Outline-Box", "Rubberband", "Magnetic", /* temp Menu Toggle Strings*/
  NULL
};

static GList *menu_togglers = NULL;

int npopup_items = sizeof(popup_items) / sizeof(popup_items[0]);

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void g_menu_execute(GtkAction *action, gpointer user_data)
{
  char *guile_string = NULL;
  const char *func = gtk_action_get_name (action);
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) user_data;

  /* Must be g_strdup_printf, g_strdup does not work here */
  guile_string = g_strdup_printf("(%s)", func);

#if DEBUG
  printf("%s\n", guile_string);
#endif
  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler (g_free, guile_string, SCM_F_WIND_EXPLICITLY);
  g_dynwind_window (w_current);
  g_scm_c_eval_string_protected (guile_string);
  scm_dynwind_end ();
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void menu_register_toggler(ToggleMenuData *toggler_data) {
  menu_togglers = g_list_append(menu_togglers, toggler_data);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GtkWidget *get_main_menu(GSCHEM_TOPLEVEL *w_current)
{
  char *buf;
  GschemAction *action;
  GtkWidget *menu_item;
  GtkWidget *root_menu;
  GtkWidget *menu_bar;
  GtkWidget *menu;
  int scm_items_len;
  SCM scm_items;
  SCM scm_item;
  SCM scm_item_name;
  SCM scm_item_func;
  SCM scm_item_hotkey_func;
  SCM scm_item_stock;
  SCM scm_index;
  char *menu_name;
  char *action_name;
  char **raw_menu_name = g_malloc (sizeof(char *));
  char *menu_item_name;
  char *raw_menu_item_name;
  char *menu_item_hotkey_func;
  char *menu_item_stock;
  char *menu_item_keys;
  unsigned long handler;
  int i, j;
  bool is_a_toggle;
  ToggleMenuData *toggler_data;

  menu_bar = gtk_menu_bar_new ();


  scm_dynwind_begin (0);
  g_dynwind_window (w_current);
  /*! \bug This function may leak memory if there is a non-local exit
   * in Guile code. At some point, unwind handlers need to be added to
   * clean up heap-allocated strings. */

  for (i = 0 ; i < s_menu_return_num(); i++) {

    scm_items = s_menu_return_entry(i, raw_menu_name);
    if (*raw_menu_name == NULL) {
      fprintf(stderr, "Oops.. got a NULL menu name in get_main_menu()\n");
      exit(-1);
    }

    menu = gtk_menu_new();

    menu_item = gtk_tearoff_menu_item_new ();
    gtk_menu_append(GTK_MENU(menu), menu_item);
    gtk_widget_show(menu_item);

    scm_items_len = (int) scm_ilength (scm_items);
    for (j = 0 ; j < scm_items_len; j++) {

      scm_index      = scm_from_int (j);
      scm_item       = scm_list_ref (scm_items, scm_index);
      scm_item_name  = SCM_CAR (scm_item);
      scm_item_func  = SCM_CADR (scm_item);
      scm_item_hotkey_func = SCM_CADDR (scm_item);
      scm_item_stock = scm_is_pair (SCM_CDDDR (scm_item)) ? SCM_CADDDR (scm_item) : SCM_BOOL_F;

      SCM_ASSERT(scm_is_string(scm_item_name), scm_item_name, SCM_ARGn, "get_main_menu item_name");
      SCM_ASSERT(scm_is_symbol (scm_item_func) || scm_is_false (scm_item_func),
                 scm_item_func, SCM_ARGn, "get_main_menu item_func");
      SCM_ASSERT(scm_is_symbol (scm_item_hotkey_func) || scm_is_false (scm_item_hotkey_func),
                 scm_item_hotkey_func, SCM_ARGn, "get_main_menu hotkey_func");
      SCM_ASSERT(scm_is_string (scm_item_stock) || scm_is_false (scm_item_stock),
                 scm_item_stock, SCM_ARGn, "get_main_menu stock");

      raw_menu_item_name = scm_to_utf8_string(scm_item_name);
      scm_dynwind_begin(0);
      scm_dynwind_free(raw_menu_item_name);

      menu_item_name = (char *) gettext(raw_menu_item_name);

      if (strcmp(menu_item_name, "SEPARATOR") == 0) {
        menu_item = gtk_menu_item_new();
        gtk_menu_append(GTK_MENU(menu), menu_item);
      }
      else {
        if (scm_is_false (scm_item_hotkey_func)) {
          menu_item_hotkey_func = NULL;
        }
        else {
          menu_item_hotkey_func = scm_to_utf8_string (scm_symbol_to_string (scm_item_hotkey_func));
          scm_dynwind_free (menu_item_hotkey_func);
        }

        if (menu_item_hotkey_func != NULL) {
            menu_item_keys = g_find_key(menu_item_hotkey_func);
            if ( strcmp (menu_item_keys, "(null)") == 0 ) menu_item_keys="";
        }
        else {
          menu_item_keys = "";
        }

        if(scm_is_false (scm_item_func)) {
          menu_item = gtk_menu_item_new_with_mnemonic(menu_item_name);
        }
        else {
          if (scm_is_false (scm_item_stock))
            menu_item_stock = NULL;
          else
            menu_item_stock = scm_to_utf8_string (scm_item_stock);

          action_name = scm_to_utf8_string (scm_symbol_to_string (scm_item_func));

          is_a_toggle = FALSE;
          if (strncmp (menu_item_name, "Toggle", 6) == 0 ) {
            is_a_toggle = TRUE;
            toggler_data = g_new0 (ToggleMenuData, 1);
            toggler_data->w_current = w_current;
            toggler_data->menu_item_name = g_strdup(menu_item_name);
            toggler_data->menu_path      = g_strconcat (*raw_menu_name, "/", raw_menu_item_name, NULL);
            menu_item_name = menu_item_name + 7;                /* is just for label */
            action = (GschemAction *)
                      gschem_toggle_action_new (action_name,     /* Action name */
                                                menu_item_name,  /* Text */
                                                menu_item_name,  /* Tooltip */
                                                menu_item_stock, /* Icon stock ID */
                                                menu_item_keys); /* Accelerator string */

          }
          else
            action = gschem_action_new (action_name,     /* Action name */
                                        menu_item_name,  /* Text */
                                        menu_item_name,  /* Tooltip */
                                        menu_item_stock, /* Icon stock ID */
                                        menu_item_keys); /* Accelerator string */

          free(action_name);
          free(menu_item_stock);

          menu_item = gtk_action_create_menu_item (GTK_ACTION (action));

          handler = g_signal_connect (G_OBJECT(action), "activate",
                                      G_CALLBACK(g_menu_execute),
                                      w_current);
          if(is_a_toggle) {
            //gtk_menu_item_set_accel_path(menu_item, toggler_data->menu_path );
            toggler_data->handler = handler;
            menu_register_toggler(toggler_data);
          }
        }

        gtk_menu_append (GTK_MENU (menu), menu_item);
      }

      gtk_widget_show (menu_item);

      /* add a handle to the menu_bar object to get access to widget objects */
      /* This string should NOT be internationalized */
      buf = g_strdup_printf("%s/%s", *raw_menu_name, raw_menu_item_name);
      gtk_object_set_data(GTK_OBJECT(menu_bar), buf, menu_item);
      g_free(buf);

      scm_dynwind_end();
    }

    menu_name = (char *) gettext(*raw_menu_name);
    root_menu = gtk_menu_item_new_with_mnemonic (menu_name);
    /* do not free *raw_menu_name */
    /* no longer right justify the help menu since that has gone out of style */
    gtk_widget_show (root_menu);
    gtk_menu_item_set_submenu (GTK_MENU_ITEM (root_menu), menu);
    gtk_menu_bar_append (GTK_MENU_BAR (menu_bar), root_menu);
  }
  scm_dynwind_end ();

  menu_item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(menu_bar), "_View/_Redraw");
   if( menu_item != NULL) {
     GtkContainer *menu = GTK_CONTAINER (gtk_widget_get_parent (menu_item));

     menu_item = gtk_menu_item_new_with_mnemonic("_Toolbars");
     GtkWidget *toggle_menu = gtk_menu_new();

     gtk_menu_item_set_submenu ( GTK_MENU_ITEM( menu_item ) , toggle_menu ) ;
     gtk_object_set_data(GTK_OBJECT(menu_bar), "_View/_Toolbars", menu_item);

     GtkWidget *stdbar_toggle = gtk_check_menu_item_new_with_mnemonic (   "_Standard" );
     GtkWidget *pagebar_toggle = gtk_check_menu_item_new_with_mnemonic (  "_Page" );
     GtkWidget *addbar_toggle = gtk_check_menu_item_new_with_mnemonic (   "_Add" );
     GtkWidget *zoombar_toggle = gtk_check_menu_item_new_with_mnemonic (  "_Zoom" );
     GtkWidget *editbar_toggle = gtk_check_menu_item_new_with_mnemonic (  "_Edit" );
     GtkWidget *attribar_toggle = gtk_check_menu_item_new_with_mnemonic ( "A_ttribute" );

     gtk_check_menu_item_set_active((GtkCheckMenuItem *)stdbar_toggle, TRUE);
     gtk_check_menu_item_set_active((GtkCheckMenuItem *)pagebar_toggle, TRUE);
     gtk_check_menu_item_set_active((GtkCheckMenuItem *)addbar_toggle, TRUE);
     gtk_check_menu_item_set_active((GtkCheckMenuItem *)zoombar_toggle, TRUE);
     gtk_check_menu_item_set_active((GtkCheckMenuItem *)editbar_toggle, TRUE);
     gtk_check_menu_item_set_active((GtkCheckMenuItem *)attribar_toggle, TRUE);

     /* Normally the ui manager would do this for us but we don't have one so...*/
     gtk_object_set_data(GTK_OBJECT(menu_bar), "_View/_Toolbars/_Standard",  stdbar_toggle);
     gtk_object_set_data(GTK_OBJECT(menu_bar), "_View/_Toolbars/_Page",      pagebar_toggle);
     gtk_object_set_data(GTK_OBJECT(menu_bar), "_View/_Toolbars/_Add",       addbar_toggle);
     gtk_object_set_data(GTK_OBJECT(menu_bar), "_View/_Toolbars/_Zoom",      zoombar_toggle);
     gtk_object_set_data(GTK_OBJECT(menu_bar), "_View/_Toolbars/_Edit",      editbar_toggle);
     gtk_object_set_data(GTK_OBJECT(menu_bar), "_View/_Toolbars/A_ttribute", attribar_toggle);

     gtk_menu_append ( toggle_menu, stdbar_toggle);
     gtk_menu_append ( toggle_menu, pagebar_toggle);
     gtk_menu_append ( toggle_menu, addbar_toggle);
     gtk_menu_append ( toggle_menu, zoombar_toggle);
     gtk_menu_append ( toggle_menu, editbar_toggle);
     gtk_menu_append ( toggle_menu, attribar_toggle);

     gtk_menu_shell_prepend((GtkMenuShell *)menu, menu_item);

     g_signal_connect (G_OBJECT(stdbar_toggle), "toggled",
                       G_CALLBACK(x_window_standard_toolbar_toggle),
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
   }
   else
     fprintf(stderr, "No Dice");

  g_free(raw_menu_name);
  return menu_bar;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static char* gettext_fn(const char *path,
			 gpointer func_data ATTRIBUTE_UNUSED)
{
  /*! \bug Note that we have to discard the 'const' qualifier here to
   * avoid build warnings when gettext is disabled.  This is required
   * due to the prototype of the function pointer argument to
   * gtk_item_factory_set_translate_func() */
  return (char *) gettext(path);
}

GtkWidget *get_main_popup(GSCHEM_TOPLEVEL *w_current)
{
  static GtkItemFactory *item_factory;
  GtkAccelGroup *accel_group;
  GtkWidget *menu;

  accel_group = gtk_accel_group_new();

  /* This function initializes the item factory.
     Param 1: The type of menu - can be GTK_TYPE_MENU_BAR, GTK_TYPE_MENU, or GTK_TYPE_OPTION_MENU.
     Param 2: The path of the menu.
     Param 3: A pointer to a gtk_accel_group.  The item factory sets up
     the accelerator table while generating menus.
  */
  item_factory = gtk_item_factory_new(GTK_TYPE_MENU, "<popup>",
                                      accel_group);
  gtk_item_factory_set_translate_func (item_factory,
                                       gettext_fn,
                                       NULL, NULL);
  /* This function creates the pop-up menu itself & attaches it to the
     GtkItemFactory. Pass the item factory,
     the number of items in the array, the array itself, and any
     callback data for the the menu items. Note that npopup_items is
     a static var declared in this file above; popup_items is also a
     static var declared above.
  */
  gtk_item_factory_create_items(item_factory, npopup_items, popup_items, w_current);

  /* Finally, return the actual menu created by the item factory. */
  menu = (GtkWidget *) gtk_item_factory_get_widget(item_factory, "<popup>");
  return (menu);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  need to look at this... here and the setup
 */
int do_popup (GSCHEM_TOPLEVEL *w_current, GdkEventButton *event)
{
  GtkWidget *menu;   /* =NULL; */ /* was static */

  menu = NULL;  /* Why do I need to do this? */
  if (!menu)
    menu = (GtkWidget *) w_current->popup_menu;

  if (menu == NULL) {
    printf("null menu\n");
  }

  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                  event->button, event->time);

  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_menus_sensitivity (GSCHEM_TOPLEVEL *w_current, const char *buf, int flag)
{
  GtkWidget* item=NULL;
  static int sensitivity_error= FALSE;

  if (!buf) {
    return;
  }

  if (!w_current->menubar) {
    return;
  }

  item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(w_current->menubar), buf);

  if (item) {
    gtk_widget_set_sensitive(GTK_WIDGET(item), flag);
    /* item = pointer to menu widget -- don't free here */
  } else {
    if (!sensitivity_error) {
      s_log_message(_("Tried to set the sensitivity on non-existent menu item '%s',\nDisabling sensitivity warnings\n"), buf);
      sensitivity_error= TRUE;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function sets the sensitivity of the items in the right button
 *  popup.
 *
 *  \note
 *  1.9.2005 -- SDB.
 */
void x_menus_popup_sensitivity (GSCHEM_TOPLEVEL *w_current, const char *buf, int flag)
{
  GtkWidget *menu_item;
  GtkItemFactory *menu_item_factory;

  if (!buf) {
    return;
  }

  if (!w_current->popup_menu) {
    s_log_message(_("Popup_menu_item_factory doesn't exist!\n"));
    return;
  }

  /*
   * first get entire item factory from popup, then get the individual
   * menu item indexed by buf.
   */
  menu_item_factory = (GtkItemFactory *)gtk_item_factory_from_widget(w_current->popup_menu);
  menu_item = (GtkWidget *) gtk_item_factory_get_widget(menu_item_factory, buf);
  if (menu_item) {
    gtk_widget_set_sensitive(GTK_WIDGET(menu_item), flag);
  } else {
    s_log_message(_("Tried to set the sensitivity on a non-existent popup menu_item\n"));
  }
}

/*! \section menu-toggle-action Menu Toggle Action Support Functions
 *
 *  \par
 *     The Menu toggles buttons need the "activate" signal blocks temporily
 *   so we don't have recursion with i_callbacks.
 */
/*! \brief Set State of Menu Toggle Items - Low LeveL
 *  \par Function Description
 *
 */
static void menu_set_toggle(ToggleMenuData *toggler_data, bool state) {

  char* menu_path;
  GSCHEM_TOPLEVEL *w_current;
  GtkWidget *menu_item;
  GtkAction *action;

  menu_path  = toggler_data->menu_path;
  w_current  = toggler_data->w_current;

  if (w_current->menubar != NULL) {
     menu_item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(w_current->menubar), menu_path);
     if( menu_item != NULL) {
       /* Get action for this item so we can block the signal */
       action = gtk_widget_get_action(menu_item);
       g_signal_handler_block( action, toggler_data->handler);
       gtk_check_menu_item_set_active((GtkCheckMenuItem *)menu_item, state);
       /*re-enable the signal */
       g_signal_handler_unblock( action, toggler_data->handler);
     }
     else
       s_log_message("Error, x_menu_set_toggle: Menu path not found, \"%s\" \n", menu_path);
  }
  else
    s_log_message("Error, x_menu_set_toggle: invalid pointer [w_current->menubar]\n");
  return;
}

/*! \brief Set State of Menu Toggle Items .
 * \par Function Description
 *  This is a menu support function to "uncheck" menu toggle items. The function
 * can be used when the menu option was changed by some other means, for example
 * when options are turned off with "Hot-Keys".
 *
 * \param toggle_id is int index of the ToggleMenuData Glist item to set
 * \param state     is int value to set, either TRUE (check) or FALSE (uncheck).
 *
 */
void x_menu_set_toggle(GSCHEM_TOPLEVEL *w_current, int toggle_id, bool state){

  int  number_of_togglers;
  ToggleMenuData *toggler_data;

  void set_toggler(int index, bool value) {
    toggler_data = (ToggleMenuData*) g_list_nth_data (menu_togglers, index);
    menu_set_toggle(toggler_data, value);
  }

  number_of_togglers = g_list_length(menu_togglers);

  if (toggle_id == RESET_TOGGLERS) {
    set_toggler(SNAP_TOGGLE,     (w_current->snap > 0));
    set_toggler(OUTLINE_TOGGLE,  (w_current->action_feedback_mode > 0));
    set_toggler(RUBBER_TOGGLE,   (w_current->netconn_rubberband > 0));
    set_toggler(MAGNETIC_TOGGLE, (w_current->magnetic_net_mode > 0));
  }
  else {
   if(toggle_id < number_of_togglers)
     toggler_data = (ToggleMenuData*) g_list_nth_data (menu_togglers, toggle_id);
     menu_set_toggle(toggler_data, state);
  }

  return;
}

/*! \brief Set State of Menu ToolBar Toggle Items .
 * \par Function Description
 *  This is a menu support function to "uncheck" Toolbar Menu toggle items. The
 * function can be used when the menu option was changed by some other means,
 * for example when a floating toolbars is turned off with the "X" box.
 *
 * \param toggle_id is int index of the IDS_Menu_Toggles item to set
 * \param state     is int value to set, either TRUE (check) or FALSE (uncheck).
 */
void x_menu_set_toolbar_toggle(GSCHEM_TOPLEVEL *w_current, int toggle_id, bool state){

  char  menu_name[36] = "_View/_Toolbars/";
  char *menu_path;
  GtkWidget *menu_item;

  menu_path = g_strconcat (menu_name, IDS_Menu_Toolbar_Toggles[toggle_id], NULL);
  menu_item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(w_current->menubar), menu_path);
  if( menu_item != NULL) {
    gtk_check_menu_item_set_active((GtkCheckMenuItem *)menu_item, state);
  }
  else
      s_log_message("Error, x_menu_set_toolbar_toggle: Did not find path \"%s\"\n", menu_path);
  g_free(menu_path);
  return;
}

/* ---------------- Recently Files Menu ---------------- */
/*! \section recent-file Recent Menu Support Functions
 *
 *  \comment This is the old method, as appose to the GTK Recent
 *  Chooser Manger version. This method seems works better on Debian
 *  machines (which is likely to be the majority) due to default
 *  security policies, which result in all the links to recent files
 *  being erase when ever ANY recent file link is access by anyone
 *  having "administrative" privileges.
 */

/*! \brief Update Recent Files Menus
 *  \par Function Description
 * Make all toplevels reflect changes to the
 *         recent files list.
 */
static void update_recent_files_menus()
{
   GSCHEM_TOPLEVEL *w_current;
   GtkWidget *submenu, *recent_menu_item;
   GList *iter;

   for (iter = global_window_list;
        iter != NULL;
        iter = g_list_next (iter)) {
      w_current = (GSCHEM_TOPLEVEL *)iter->data;

      if (w_current->menubar == NULL)
        continue;

      recent_menu_item =
        (GtkWidget *) gtk_object_get_data(GTK_OBJECT(w_current->menubar),
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
static void clear_recent_file_list(gpointer data)
{
   GList *p;

   p = recent_files;
   while(p) {
      g_free(p->data);
      p = g_list_next(p);
   }
   g_list_free(recent_files);
   recent_files = NULL;

   update_recent_files_menus();
}

static void
recent_file_free_menu_data (gpointer data, GClosure *closure) {
  g_free (data);
}

static void recent_file_clicked(GtkMenuItem *menuitem, gpointer user_data)
{
   FILE *fp;
   PAGE *page;
   RecentMenuData *data = (RecentMenuData *) user_data;
   GSCHEM_TOPLEVEL *w_current = data->w_current;
   char *filename = data->filename;

   /* Check if the file exists */
   fp = fopen((char *) filename, "r");
   if(fp == NULL) {
      /* Remove this entry from all menus */
      s_log_message(_("Couldn't open file %s\n"), (char *) filename);
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
void x_menu_attach_recent_files_submenu(GSCHEM_TOPLEVEL *w_current)
{
   unsigned long id;
   GtkWidget *tmp;
   GtkWidget *recent_menu_item, *recent_submenu;

   recent_menu_item = (GtkWidget *) gtk_object_get_data(GTK_OBJECT(
            w_current->menubar), "_File/Open Recen_t");
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
      gtk_container_add(GTK_CONTAINER(alignment), gtk_label_new(_("Clear")));
      gtk_container_add(GTK_CONTAINER(tmp), alignment);

      gtk_signal_connect_object(GTK_OBJECT(tmp), "activate",
            GTK_SIGNAL_FUNC (clear_recent_file_list), NULL);

      gtk_menu_append(GTK_MENU(recent_submenu), gtk_separator_menu_item_new());
      gtk_menu_append(GTK_MENU(recent_submenu), tmp);
   }

   gtk_widget_show_all(recent_submenu);
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
      g_free(basename);
      return;
   }

   g_free(basename);

   /* Normalize the filename. */
   save_fn = f_normalize_filename (filename, &err);
   if (err != NULL) {
     save_fn = g_strdup (filename);
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
     g_free (save_fn);
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
   char *file = g_build_filename(s_path_user_config (), RECENT_FILES_STORE, NULL);

   g_key_file_set_string_list(kf, "Recent files", "Files", tmp, 0);
   c = g_key_file_to_data(kf, NULL, NULL);
   g_key_file_free(kf);

   g_file_set_contents(file, c, -1, NULL);
   g_free(c);
   g_free(file);
}

/*! \brief Save the list of recent files to RECENT_FILES_STORE.
 *
 *  \param [in] user_data unused
 */
void recent_files_save(gpointer user_data)
{
   char *files[MAX_RECENT_FILES];
   int num = 0;
   char *c;
   char *file = g_build_filename(s_path_user_config (), RECENT_FILES_STORE, NULL);

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

   g_free(c);
   g_free(file);
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
   char *file = g_build_filename(s_path_user_config (), RECENT_FILES_STORE, NULL);

   if(!g_file_test(file, G_FILE_TEST_EXISTS)) {
     g_mkdir(s_path_user_config (), S_IRWXU | S_IRWXG);

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

   g_free(list);
   g_free(file);
   g_key_file_free(kf);
}
/* Date: Sept 05, 2012
 * Who:  Wiley E. Hill
 * What  Function: recent_files_last
 * Why:  This Function was added to support the auto_load_last mechanism.
 *
 * This function simply returns a char pointer to the name of the most
 * recent file loaded.
*/
const char *recent_files_last() {

   return (g_list_nth_data(recent_files, 0));

}
