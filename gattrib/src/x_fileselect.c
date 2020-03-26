/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 *
 * Copyright (C) 2003-2015 Stuart D. Brorson.
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions to display file open/save dialog box.
 * \par
 * This file contains functions used to display the file open/save dialog
 * box. It was cloned from x_fileselect.c in gschem/src, and then hacked
 * by SDB for use in gattrib.
 */

#include <gattrib.h>
#include <geda_file_chooser.h>
#include <geda_debug.h>

/* ----- x_fileselect stuff begins here ----- */

/*!
 * \brief Generic File Dialog
 * \par Function Description
 *  This function opens a file chooser dialog and waits for the user to
 *  select a folder and enter a filename. The user can also select an
 *  existing filename or cancel. Called by x_menu_file_save_as().
 *
 * \param filename  char pointer to a buffer to receive the string.
 *
 * \returns True of dialog accepted input, false if the user cancels.
 */
bool x_fileselect (char *filename)
{
  GtkWidget *dialog;
  char      *cwd;
  bool       result;

  dialog = geda_file_chooser_new (main_window, FILE_CHOOSER_ACTION_SAVE);

  g_object_set (dialog,                             /* GedaFileChooser */
                "select-multiple", FALSE,
                "do-overwrite-confirmation", TRUE,  /* version?*/
                NULL);                              /* end options */

  cwd = g_get_current_dir();
  geda_file_chooser_set_current_folder (dialog, cwd);
  GEDA_FREE (cwd);

  /* preset a directory name */
  if (pr_current->page_current->filename != NULL) {

    cwd = geda_utility_string_strdup(pr_current->page_current->filename);
    geda_file_chooser_set_filename (dialog, cwd);

  }

  gtk_widget_show (dialog);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GEDA_RESPONSE_ACCEPT) {

    char *fname;

    fname = geda_file_chooser_get_filename (dialog);

    strcpy(filename, fname);
    GEDA_FREE(fname);
    result = TRUE;
  }
  else {
    result = FALSE;
  }

  gtk_widget_destroy (dialog);

  return result;
}

/*!
 * \brief Open a single file.
 * \par Function Description
 *  The function attempts to read in a file to the toplevel and if
 *  successful preloads the the sheet data that is used for column
 *  and row titles. If retuns FALSE of s_toplevel_read_page is
 *  successful otherwaise TRUE. Called by menu_open_recent().
 *
 * \param [in] filename name of file to be opened
 *
 * \retval FALSE if the file could not be opened, TRUE otherwise
 */
bool x_fileselect_load_file (char *filename) {

  const GList *objects;
         char *uri;

  if (!quiet_mode) {
    geda_log ("%s: \"%s\"\n", _("Loading file"), filename);
  }

  geda_struct_page_goto (geda_struct_page_new (pr_current, filename));

  if (s_toplevel_read_page(pr_current, filename) == 0) {
    fprintf(stderr,"%s: \"%s\"\n",  _("Could not load schematic"), filename);
    return FALSE;
  }

  objects = geda_struct_page_get_objects (pr_current->page_current);

  /* Now add all items found to the master lists */
  s_sheet_data_add_master_comp_list_items (objects);
  s_sheet_data_add_master_comp_attrib_list_items (objects);

  /* Note that this must be changed.  We need to input the entire project
   * before doing anything with the nets because we need to first
   * determine where they are all connected!   */
  s_sheet_data_add_master_net_list_items (objects);
  s_sheet_data_add_master_net_attrib_list_items (objects);

  s_sheet_data_add_master_pin_list_items (objects);
  s_sheet_data_add_master_pin_attrib_list_items (objects);

  uri = g_filename_to_uri(filename, NULL, NULL);

  gtk_recent_manager_add_item (recent_manager, uri);

  geda_free (uri);

  return TRUE;
}

/*!
 * \brief Open all files specified in the list.
 * \par Function Description
 *  Open all files specified in the list. The caller is responsible for
 *  freeing the strings and the list itself. Called by x_menu_file_open().
 *
 *  The function updates the user interface. At the end of the function,
 *  the toplevel's current page is set to the page of the last loaded page.
 *
 * \param [in] filenames list of files to be opened
 *
 * \retval FALSE if any of the files could not be opened, TRUE otherwise
 */
bool x_fileselect_load_files (GSList *filenames)
{
  GSList *ptrname;
  int ret_val = TRUE;

  /* iterate over selected files */
  for (ptrname = filenames; ptrname != NULL; ptrname = ptrname->next) {

    char *filename = ptrname->data;

    if (!x_fileselect_load_file(filename)) {
       ret_val = FALSE;
    }
  }   /* end of loop over files     */

  return ret_val;
}

/*!
 * \brief Open file dialog
 * \par Function Description
 *  This function opens a file chooser dialog and waits for the user
 *  to select at least one file to load as toplevel's new pages.
 *  Called from x_menu_file_open
 *
 * \returns list of files to be opened, or NULL if the user canceled
 */
GSList *x_fileselect_open (void)
{
  GtkWidget *dialog;
  char      *cwd;
  GSList    *filenames = NULL;

  dialog = geda_file_chooser_new (main_window, FILE_CHOOSER_ACTION_OPEN);

  g_object_set (dialog, "select-multiple", TRUE, NULL);

  /* Add file filters to dialog */

  gtk_widget_show (dialog);

  /* get current working dir */
  cwd = getcwd(0,0);
  geda_file_chooser_set_current_folder (dialog, cwd);
  free (cwd);

  geda_file_chooser_set_filter(dialog, FILTER_SCHEMATIC);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GEDA_RESPONSE_ACCEPT) {
    filenames = geda_file_chooser_get_filenames (dialog);
  }

  gtk_widget_destroy (dialog);

  return filenames;
}
