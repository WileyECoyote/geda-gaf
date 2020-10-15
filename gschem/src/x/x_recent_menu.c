/* -*- C x_recent_menu.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_recent_menu.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2020 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2012-2020 gEDA Contributors (see ChangeLog for details)
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
 * \file x_recent_menu.c
 * \brief Main Window Recent Files Menu
 */

#include <ctype.h>         /* isdigit */

#include "../../include/gschem.h"
#include "../../include/x_menus.h"
#include "../../include/i_actions.h"

#include <geda/geda_dialog_controls.h>
#include <geda/geda_stat.h>
#include <geda_widgets.h>

#include <geda_debug.h>

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
 *        This section should assign an icon.
 */

/** \defgroup recent-file-internal Recent Files Functions
 *  @{
 * \brief This group contains core Routine for the Recent Files Menu.
*/

static int    show_recent_path;
static GList *recent_files = NULL;

/*!
 * \brief Remove all entries from the recent files list
 *  and update the menus.
 */
static void x_recent_menu_clear_file_list(void *data)
{
   GList *iter;

   iter = recent_files;

   while (iter) {
      GEDA_FREE(iter->data);
      iter = g_list_next(iter);
   }
   g_list_free(recent_files);
   recent_files = NULL;

   x_recent_menu_update_files();
}

static void x_recent_menu_destroy_data (GedaMenuItem *menuitem, void *menu_data)
{
  GEDA_FREE (menu_data);
}

/*!
 * \brief Recent Menu item Clicked
 * \par Function Description
 *  Called with user clicks on a menu item on the recent files menu or when
 *  the user select "open" from the popup menu on the recent file submenu.
 */
static void x_recent_menu_file_clicked (GedaMenuItem *menuitem, void *menu_data)
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
      x_recent_menu_update_files();
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
static void x_recent_menu_files_create_empty(void)
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
 *  Called when the user toggles to Show path items on recent file pop-up
 *  menu. Toggles the state of show_recent_path, calls to update the menu
 *  and causes the recent file sub-menu to reappear with the opposite state.
 */
static void x_recent_menu_toggle_path (GedaCheckMenuItem *menuitem, void *user_data)
{
  RecentMenuData *data      = (RecentMenuData*)user_data;
  GschemToplevel *w_current = data->w_current;

  GedaMenuItem *menu_item;
  MenuData     *menu_data;
  GSList       *ui_list;

  show_recent_path = geda_check_menu_item_get_active (menuitem);

  x_recent_menu_update_files();

  /* Get pointer to the recent files submenu */
  ui_list   = x_menu_get_ui_list();
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
static void x_recent_menu_file_remove (GedaMenuItem *menuitem, void *user_data)
{
  RecentMenuData *menu_data = user_data;
  char           *filename  = menu_data->filename;

  /* Remove this entry from all menus */
  recent_files = g_list_remove(recent_files, filename);
  x_recent_menu_update_files();
}

/*!
 * \brief Recent Files Menu Internal Show Popup
 * \par Function Description
 *  This functions creates and displays a small pop-up menu on
 *  the recent files menu. The open option is provided for the
 *  sake of completeness, the real objective here is to allow
 *  users to remove individual recent menu items.
 */
static void x_recent_menu_show_popup (GedaMenuItem   *menu_widget,
                                      GdkEventButton *event,
                                      RecentMenuData *menu_data)
{
  GtkWidget *menu;
  GtkWidget *popup_item;

  /* create the context menu */
  menu = geda_menu_new();

  popup_item = geda_menu_item_new_with_label (_("Open"));

  g_signal_connect (popup_item, "activate",
                    G_CALLBACK(x_recent_menu_file_clicked), menu_data);

  geda_menu_append (menu, popup_item);

  popup_item = geda_menu_item_new_with_label (_("Remove"));

  g_signal_connect (popup_item, "activate",
                    G_CALLBACK(x_recent_menu_file_remove), menu_data);

  geda_menu_append (menu, popup_item);

  popup_item = geda_check_menu_item_new_with_mnemonic (_("_Show path"));

  geda_check_menu_item_set_state (popup_item, show_recent_path);

  GTK_CALLBACK_TOGGLED (popup_item, x_recent_menu_toggle_path, menu_data);

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
 * \sa x_recent_menu_show_popup
 */
static bool x_recent_menu_button_released (GedaMenuItem   *menu_item,
                                           GdkEventButton *event,
                                           RecentMenuData *menu_data)
{
  bool ret_val;

  if (event->button == 3) {

    x_recent_menu_show_popup(menu_item, event, menu_data);

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
void x_recent_menu_attach_submenu(GschemToplevel *w_current)
{
   GtkWidget *item;
   GtkWidget *recent_menu_item, *recent_submenu;
   GList     *iter;
   GSList    *ui_list;
   MenuData  *menu_data;
   bool       show_menu_tips;

   ui_list          = x_menu_get_ui_list();
   menu_data        = g_slist_nth_data (ui_list, w_current->ui_index);
   recent_menu_item = GEDA_OBJECT_GET_DATA (MENU_BAR, "_File/Open Recen_t");

   show_recent_path = eda_config_get_boolean (eda_config_get_user_context (),
                                              MENU_CONFIG_GROUP,
                                              "show-recent-path", NULL);

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
                       G_CALLBACK(x_recent_menu_file_clicked), menu_data);

     g_signal_connect (item, "button-release-event",
                       G_CALLBACK (x_recent_menu_button_released), menu_data);

     g_signal_connect (item, "destroy",
                       G_CALLBACK(x_recent_menu_destroy_data), menu_data);

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
                         x_recent_menu_clear_file_list, NULL);

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
void x_recent_menu_add_file(const char *filename)
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

   x_recent_menu_update_files();
}

/*!
 * \brief Save the list of recent files to RECENT_FILES_STORE.
 * \par Function Description
 *  This function is called before exiting to save the list of recent
 *  files to disk.
 *
 * \param [in] user_data unused
 */
void x_recent_menu_save_files(void *user_data)
{
   char *files[MAX_RECENT_FILES];
   char *data;
   char *file;
   int   num;

   file = g_build_filename(geda_user_config_path(), RECENT_FILES_STORE, NULL);
   num  = 0;

   GList *p = recent_files;
   if(p == NULL) {
      x_recent_menu_files_create_empty();
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

   EdaConfig *cfg;

   cfg = eda_config_get_user_context ();

   eda_config_set_boolean(cfg, MENU_CONFIG_GROUP, "show-recent-path",
                                                   show_recent_path);
}

/*!
 * \brief Load the recent file list using data from RECENT_FILES_STORE.
 *  Must be called before any other recent-files-related
 *  functions.
 */
void x_recent_menu_load_files()
{
   GedaKeyFile  *keyfile;
   char         *file;
   char        **list;
   unsigned int  len;

   keyfile = geda_keyfile_new();
   file = g_build_filename(geda_user_config_path(), RECENT_FILES_STORE, NULL);

   if (!g_file_test(file, G_FILE_TEST_EXISTS)) {
     geda_create_path(geda_user_config_path (), S_IRWXU | S_IRWXG);
     x_recent_menu_files_create_empty();
   }

   if (!geda_keyfile_load_from_file(keyfile, file, G_KEY_FILE_NONE, NULL)) {
      /* error opening key file, create an empty one and try again */
      x_recent_menu_files_create_empty();
      if(!geda_keyfile_load_from_file(keyfile, file, G_KEY_FILE_NONE, NULL))
         return;
   }

   list = geda_keyfile_get_string_list(keyfile, "Recent files", "Files", &len, NULL);

   if (list == NULL) {
      /* There was an error reading key file, don't bother to correct;
       * just overwrite it with an empty one */
      x_recent_menu_files_create_empty();
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
const char *x_recent_menu_last_file(void)
{
   return (g_list_nth_data(recent_files, 0));
}

/*!
 * \brief Update Recent Files Menus
 * \par Function Description
 *  Make all toplevels reflect changes to the recent files list.
 */
void x_recent_menu_update_files(void)
{
   GList *iter;
   GSList *ui_list;

   ui_list = x_menu_get_ui_list();

   for (iter = global_window_list; iter != NULL; iter = g_list_next (iter)) {

     GschemToplevel *w_current;
     GtkWidget      *submenu;
     GtkWidget      *recent_menu_item;
     MenuData       *menu_data;

     w_current = (GschemToplevel*)iter->data;
     menu_data = g_slist_nth_data (ui_list, w_current->ui_index);

     if (MENU_BAR == NULL) {
       continue;
     }

     recent_menu_item = GEDA_OBJECT_GET_DATA (MENU_BAR, "_File/Open Recen_t");

     if (recent_menu_item == NULL) {
       return;
     }

     submenu = geda_menu_item_get_submenu_widget((GedaMenuItem*)recent_menu_item);

     gtk_widget_destroy(submenu);

     x_recent_menu_attach_submenu(w_current);
   }
}

/** @} end group recent-file-menu */

