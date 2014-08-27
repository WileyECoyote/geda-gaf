/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 *
 * Copyright (C) 2003-2014 Stuart D. Brorson.
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
/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions to display file open/save dialog box.
 *
 * This file holds fcns used to display the file open/save dialog box.
 * It was cloned from x_fileselect.c in gschem/src, and then hacked
 * by SDB for use in gattrib.
 */

#include <gattrib.h>
#include <geda_debug.h>

/* ----- x_fileselect stuff begins here ----- */
/*------------------------------------------------------------------*/
/*! \brief Set up file filter for the file chooser
 *
 * This fcn creates and sets the file filter for the filechooser.
 *
 * \param filechooser GtkFileChooser to set up
 *
 */
static void
x_fileselect_setup_filechooser_filters (GtkFileChooser *filechooser)
{
  GtkFileFilter *filter;

  /* file filter for schematic files (*.sch) */
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("Schematics"));
  gtk_file_filter_add_pattern (filter, "*.sch");
  gtk_file_chooser_add_filter (filechooser, filter);
  /* file filter for symbol files (*.sym) */
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("Symbols"));
  gtk_file_filter_add_pattern (filter, "*.sym");
  gtk_file_chooser_add_filter (filechooser, filter);
  /* file filter for both symbol and schematic files (*.sym+*.sch) */
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("Schematics and symbols"));
  gtk_file_filter_add_pattern (filter, "*.sym");
  gtk_file_filter_add_pattern (filter, "*.sch");
  gtk_file_chooser_add_filter (filechooser, filter);
  /* file filter that match any file */
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("All files"));
  gtk_file_filter_add_pattern (filter, "*");
  gtk_file_chooser_add_filter (filechooser, filter);

}

/*------------------------------------------------------------------*/

/*! \brief Generic File Dialog
 *
 *  This function opens a file chooser dialog and waits for the user to
 *  select a folder and enter a filename. The user can also select an
 *  existing filename or cancel.
 *
 *  \param filename  char pointer to a buffer to receive the string.
 *
 *  \returns boolean ture of dialog accepted input. returns false if
 *           the user cancels.
 */
bool x_fileselect ( char* filename )
{
  GtkWidget *dialog;
  bool   result = FALSE;
  char  *fname  = NULL;
  char  *cwd    = NULL;

  dialog = gtk_file_chooser_dialog_new (_("Save as..."),
                                        GTK_WINDOW(main_window),
                                        GTK_FILE_CHOOSER_ACTION_SAVE,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_SAVE,   GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);

  g_object_set (dialog,                     /* GtkFileChooser */
                "select-multiple", FALSE,   /* only in GTK 2.8 */
                "do-overwrite-confirmation", TRUE,  /* version?*/
                NULL);                              /* end options */

  x_fileselect_setup_filechooser_filters (GTK_FILE_CHOOSER (dialog));

  /* preset a directory name */
  if (pr_current->page_current->filename != NULL) {
    cwd = u_string_strdup(pr_current->page_current->filename);
    gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog), cwd);
#ifdef DEBUG
    fprintf(stderr, "Going to use file name=%s\n", cwd);
#endif
  }
  else { /* no filename then get current working dir */
    cwd = g_get_current_dir();
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
  }
  GEDA_FREE (cwd);

  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    fname = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    strcpy(filename,fname);
    GEDA_FREE(fname); /* GTK actually does this when dialog is destroyed */
    result = TRUE;
  }
  else {
    result = FALSE;
  }
  gtk_widget_destroy (dialog);
  return result;
}

/*! \brief Open a single file.
 *
 *  The function attempts to read in a file to the toplevel and if
 *  successful preloads the the sheet data that is used for column
 *  and row titles. If retuns FALSE of s_toplevel_read_page is
 *  successful otherwaise TRUE.
 *
 *  \param [in] filename name of file to be opened
 *  \retval FALSE if the file could not be opened, TRUE otherwise
 */
/* TODO:move gtk_recent_manager_add_item from x_fileselect_open to here */
bool x_fileselect_load_file (char *filename) {

  const GList *Objects;

  if (!quiet_mode) {
    u_log_message(_("Loading file [%s]\n"), filename);
  }

  s_page_goto (pr_current, s_page_new (pr_current, filename));

  if(s_toplevel_read_page(pr_current, filename) == 0) {
     fprintf(stderr, _("Could not load schematic [%s]\n"), filename);
     return FALSE;
  }

  Objects = s_page_get_objects (pr_current->page_current);

  /* Now add all items found to the master lists */
  s_sheet_data_add_master_comp_list_items (Objects);
  s_sheet_data_add_master_comp_attrib_list_items (Objects);

  /* Note that this must be changed.  We need to input the entire project
   * before doing anything with the nets because we need to first
   * determine where they are all connected!   */
  s_sheet_data_add_master_net_list_items (Objects);
  s_sheet_data_add_master_net_attrib_list_items (Objects);

  s_sheet_data_add_master_pin_list_items (Objects);
  s_sheet_data_add_master_pin_attrib_list_items (Objects);

  return TRUE;
}

/*! \brief Open all files specified in the list.
 *
 * Open all files specified in the list. The caller is responsible for
 * freeing the strings and the list itself.
 *
 *  The function updates the user interface. At the end of the function,
 *  the toplevel's current page is set to the page of the last loaded page.
 *
 *  \param [in] filenames list of files to be opened
 *  \retval FALSE if any of the files could not be opened, TRUE otherwise
 */
bool x_fileselect_load_files (GSList *filenames)
{

  GSList *ptrname;
  char *filename;
  int ret_val = TRUE;

  /* iterate over selected files */
  for (ptrname = filenames;
       ptrname != NULL;
       ptrname = g_slist_next (ptrname)) {

    filename = (char*)ptrname->data;
    if ( !x_fileselect_load_file(filename))
       ret_val = FALSE;
  }   /* end of loop over files     */

  return ret_val;
}

/*! \brief Open file dialog
 *
 * This function opens a file chooser dialog and waits for the user
 *  to select at least one file to load as toplevel's new pages.
 *
 *  \returns list of files to be opened, or NULL if the user cancelled
 *           the dialog
 */
GSList *x_fileselect_open (void)
{
  GtkWidget *dialog;
  GSList *filenames = NULL;
  char  *cwd    = NULL;
  GSList *ptrname;
  char *filename;

  dialog = gtk_file_chooser_dialog_new (_("Open..."),
                                        GTK_WINDOW(main_window),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN,   GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);

  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", TRUE,
                NULL);
  /* add file filters to dialog */
  x_fileselect_setup_filechooser_filters (GTK_FILE_CHOOSER (dialog));
  gtk_widget_show (dialog);

  /* get current working dir */
  cwd = getcwd(0,0);
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
  free (cwd);

  if(gtk_dialog_run (GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
     filenames = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));
     for (ptrname = filenames;
       ptrname != NULL;
       ptrname = g_slist_next (ptrname)) {
       filename = (char*)ptrname->data;
       gtk_recent_manager_add_item (recent_manager,
				    g_filename_to_uri(filename,
						      NULL, NULL));
     }
  }

  gtk_widget_destroy (GTK_WIDGET(dialog));
  return filenames;
}

