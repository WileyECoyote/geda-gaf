/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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

#include "config.h"
#include <geda_stat.h>
#include "gschem.h"
#include <geda_gui_funcs.h>
#include <geda_file_chooser.h>
#include <geda_debug.h>

/*! \brief Updates the preview when the selection changes.
 *  \par Function Description
 *  This is the callback function connected to the 'update-preview'
 *  signal of the <B>GtkFileChooser</B> that updates the preview
 *  widget with the name of the newly selected file.
 *
 *  \param [in] chooser   The file chooser to add the preview to.
 *  \param [in] user_data A pointer on the preview widget.
 */
static void
x_fileselect_callback_update_preview (GtkFileChooser *chooser,
                                      void           *user_data)
{
  Preview *preview          = PREVIEW (user_data);
  char    *preview_filename = NULL;
  char    *filename;

  filename = gtk_file_chooser_get_preview_filename (chooser);
  if (filename != NULL && !g_file_test (filename, G_FILE_TEST_IS_DIR)) {
    preview_filename = filename;
  }

  /* update preview */
  g_object_set (preview,
                "filename", preview_filename,
                "active", (preview_filename != NULL),
                NULL);

  GEDA_FREE (filename);
}

static void
x_fileselect_callback_update_size (GtkToggleButton *button,
                                   void            *user_data)
{
  Preview *preview = PREVIEW (user_data);
  int state = GetToggleState(button);
  g_object_set (preview, "large-size", state, NULL);
}

/*! \brief Adds a preview to a file chooser.
 *  \par Function Description
 *  This function adds a preview section to the stock
 *  <B>GtkFileChooser</B>.
 *
 *  The <B>Preview</B> object is inserted in a frame and alignment
 *  widget for accurate positionning.
 *
 *  Other widgets can be added to this preview area for example to
 *  enable/disable the preview. Currently, the preview is always
 *  active.
 *
 *  Function <B>x_fileselect_callback_update_preview()</B> is
 *  connected to the signal 'update-preview' of <B>GtkFileChooser</B>
 *  so that it redraws the preview area every time a new file is
 *  selected.
 *
 *  \param [in] filechooser The file chooser to add the preview to.
 */
static void
x_fileselect_add_preview (GtkFileChooser *filechooser)
{
  GtkWidget *alignment, *frame, *preview;
  GtkWidget *vbox;
  GtkWidget *cb_size;

  /* Add our extra widget to the dialog */
  vbox = gtk_vbox_new(FALSE, 0);

  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                    "label", _("Preview"),
                                    NULL));

  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        "right-padding", 5,
                                        "left-padding", 5,
                                        "xscale", 0.0,
                                        "yscale", 0.0,
                                        "xalign", 0.5,
                                        "yalign", 0.5,
                                        NULL));

  preview = GTK_WIDGET (g_object_new (TYPE_PREVIEW,
                                      "active", TRUE,
                                      NULL));

  gtk_container_add (GTK_CONTAINER (alignment), preview);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  gtk_container_add (GTK_CONTAINER (vbox), frame);
  gtk_widget_show_all (frame);

  cb_size = gtk_check_button_new_with_label (_("Large"));
  gtk_widget_set_tooltip_text(cb_size, _("Enable to enlagre the preview"));
  gtk_toggle_button_set_active ((GtkToggleButton*)cb_size, FALSE);
  g_object_set (cb_size, "visible", TRUE, NULL);

  gtk_box_pack_start (GTK_BOX(vbox), cb_size, FALSE, FALSE, 0);

  g_object_set (filechooser, "use-preview-label", FALSE,
                             "preview-widget", vbox,
                              NULL);

  /* connect callback to update preview image */
  g_signal_connect (filechooser, "update-preview",
                    G_CALLBACK (x_fileselect_callback_update_preview),
                    preview);

  /* connect callback to update preview size */
  g_signal_connect (cb_size, "toggled",
                    G_CALLBACK (x_fileselect_callback_update_size),
                    preview);
}

static void x_fileselect_save_filter_index (GtkWidget      *chooser,
                                            GschemToplevel *w_current)
{
  w_current->chooser_filter = geda_file_chooser_get_filter(chooser);
  default_chooser_filter    = w_current->chooser_filter;
}

/*! \brief Opens a file chooser and selection of document to open.
 *  \par Function Description
 *  This function opens a file chooser dialog and restores the filter
 *  filter preference, and waits for the user to select at least one
 *  file to load. A single-linked list of selected files is returned
 *  or NULL to indicate the user is canceling the operation. If the
 *  user changes the filter, a callback retains the users preference
 *  regardless of whether the operation is canceled or not.
 *
 *  \param [in] w_current The GschemToplevel environment.
 *
 *  \return list of filenames selected by the user, single-linked or
 *          NULL if no files were selected
 *
 *  \sa x_fileselect_list
 *
 *  \remark: is remnant, replaced by x_fileselect_list
 */
GSList *x_fileselect_list(GschemToplevel *w_current)
{
  GtkWidget *dialog;
  GSList    *filenames;
  char      *cwd;

  dialog = geda_file_chooser_new (w_current->main_window,
                                  FILE_CHOOSER_ACTION_OPEN);

  /* Set filter to what user last time*/
  geda_file_chooser_set_filter (dialog, w_current->chooser_filter);

  /* Conditionally add the file previewer */
  if(w_current->file_preview == TRUE) {
    x_fileselect_add_preview (GTK_FILE_CHOOSER (dialog));
  }

  /* force start in current working directory, NOT in 'Recently Used' */
  cwd = g_get_current_dir ();
  geda_file_chooser_set_current_folder (dialog, cwd);
  GEDA_FREE (cwd);

  gtk_widget_show (dialog);

  /* This ratains filter, even if canceled, could retrieve in if got
   * filenames but this seems to work just fine, is saved if changed */
  g_signal_connect_after(G_OBJECT(dialog), "filter-changed",
                         G_CALLBACK (x_fileselect_save_filter_index),
                         w_current);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    filenames =  geda_file_chooser_get_filenames (dialog);
  }
  else {
    filenames = NULL;
  }
  gtk_widget_destroy (dialog);
  return filenames;
}

/*! \brief Opens a file chooser for opening one or more schematics.
 *  \par Function Description
 *  This function opens a file chooser dialog, restores the user's
 *  filter preference, and wait for the user to select at least one
 *  file to load as <B>w_current</B>'s new pages. If the user changes
 *  the filter, a callback retains the users preference regardless of
 *  whether the operation is canceled or not. If a document is opened,
 *  the function updates the user interface.
 *
 *  At the end of the function, the w_current->toplevel's current page
 *  is set to the page of the last loaded page.
 *
 *  \param [in] w_current The GschemToplevel environment.
 *
 *  \note: is remnant, replaced by x_fileselect_list
 *
 *  \sa x_fileselect_list
 */
void x_fileselect_open(GschemToplevel *w_current)
{
  GtkWidget *dialog;
  GSList    *filenames;
  char      *cwd;

  dialog = geda_file_chooser_new (w_current->main_window,
                                  FILE_CHOOSER_ACTION_OPEN);

  /* Set filter to what user last time*/
  geda_file_chooser_set_filter (dialog, w_current->chooser_filter);

  /* 09/09/12 W. E. Hill: Conditionally add the file previewer */
  if(w_current->file_preview == TRUE)
    x_fileselect_add_preview (GTK_FILE_CHOOSER (dialog));

  /* force start in current working directory, NOT in 'Recently Used' */
  cwd = g_get_current_dir ();
  geda_file_chooser_set_current_folder (dialog, cwd);
  GEDA_FREE (cwd);

  gtk_widget_show (dialog);

  /* This ratains filter, even if canceled, could retrieve in if got
   * filenames but his seems to work just fine, is saved if changed */
  g_signal_connect_after(G_OBJECT(dialog), "filter-changed",
                         G_CALLBACK (x_fileselect_save_filter_index),
                         w_current);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    filenames =  geda_file_chooser_get_filenames (dialog);
  }
  else {
    filenames = NULL;
  }
  gtk_widget_destroy (dialog);

  /* open each file */
  if (filenames) {

    GSList *iter;
    Page   *page = NULL;

    for (iter = filenames; iter != NULL; iter = g_slist_next (iter)) {

      if(iter->data != NULL) {
        page = x_window_open_page (w_current, (char*)iter->data);
      }
      else {
        BUG_MSG("file name should not be NULL");
      }
    }

    /* Switch to the last page opened */
    if ( page != NULL ) {
      x_window_set_current_page (w_current, page);
    }

    /* free the list of filenames */
    g_slist_foreach (filenames, (GFunc)g_free, NULL);
    g_slist_free (filenames);
  }
}

/*! \brief Opens a file chooser for saving the current page.
 *  \par Function Description
 *  This function opens a file chooser dialog and wait for the user to
 *  select a file where the <B>toplevel</B>'s current page will be
 *  saved.
 *
 *  If the user cancels the operation (with the cancel button), the
 *  page is not saved.
 *
 *  The function updates the user interface.
 *
 *  \param [in] w_current The GschemToplevel environment.
 */
void
x_fileselect_save (GschemToplevel *w_current)
{
  GedaToplevel      *toplevel = w_current->toplevel;

  GtkWidget     *dialog;
  GtkWidget     *hbox;
  GtkWidget     *cb_add_ext;

  bool       auto_ext;
  char      *cwd = NULL;

  EdaConfig *cfg = eda_config_get_user_context ();

  auto_ext = eda_config_get_boolean (cfg, IVAR_CONFIG_GROUP, "auto-file-suffix", NULL);

  dialog = geda_file_chooser_new (w_current->main_window,
                                  FILE_CHOOSER_ACTION_SAVE);

  if (s_page_is_symbol_file(toplevel->page_current)) {
    geda_file_chooser_set_filter (dialog, FILTER_SYMBOL);
  }
  else {
    geda_file_chooser_set_filter (dialog, FILTER_SCHEMATIC);
  }

  /* set the current filename or directory name if new document */
  if ((toplevel->page_current->filename != NULL) &&
       g_file_test (toplevel->page_current->filename,
       G_FILE_TEST_EXISTS)) {
    geda_file_chooser_set_filename (dialog, toplevel->page_current->filename);
  }
  else {
    cwd = g_get_current_dir ();
    /* force save in current working dir */
    geda_file_chooser_set_current_folder (dialog, cwd);
    GEDA_FREE (cwd);
    geda_file_chooser_set_current_name (dialog, toplevel->untitled_name);
  }

  /* Add our extra widget to the dialog */
  hbox = gtk_hbox_new(FALSE, 0);

  cb_add_ext = gtk_check_button_new_with_label (_("Auto file Suffix"));

  gtk_widget_show (cb_add_ext);
  gtk_box_pack_start (GTK_BOX(hbox), cb_add_ext, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text(cb_add_ext, _("Automatically append the file extension"));
  gtk_toggle_button_set_active ((GtkToggleButton*)cb_add_ext, auto_ext);
  geda_file_chooser_set_extra_widget (dialog, hbox);

  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    char          *filename;
    char          *filebase;
    char          *tmpname;
    int            index;

    filename = geda_file_chooser_get_filename (dialog);
    filebase = basename(filename);
    auto_ext = gtk_toggle_button_get_active ((GtkToggleButton*)cb_add_ext);
    tmpname  = NULL;
    if (auto_ext && (filebase != NULL)) {
      if (!f_get_filename_ext(filebase)) {
        index = geda_file_chooser_get_filter(dialog);
        if (index == FILTER_SCHEMATIC)
          tmpname = g_strconcat(filename, SCHEMATIC_FILE_DOT_SUFFIX, NULL);
        else
          if (index == FILTER_SYMBOL)
           tmpname = g_strconcat(filename, SYMBOL_FILE_DOT_SUFFIX, NULL);
        if (tmpname) {
          GEDA_FREE (filename);
          filename = tmpname;
        }
      }
    }

    /* If the file already exists, display a dialog box to check if
     *       the user really wants to overwrite it. */
    if ((filename != NULL) && g_file_test (filename, G_FILE_TEST_EXISTS)) {
      GtkWidget *checkdialog =
      gtk_message_dialog_new (GTK_WINDOW(dialog),
                              (GTK_DIALOG_MODAL |
                              GTK_DIALOG_DESTROY_WITH_PARENT),
                              GTK_MESSAGE_QUESTION,
                              GTK_BUTTONS_YES_NO,
                              _("The selected file `%s' already exists.\n\n"
                              "Would you like to overwrite it?"),
                              filename);
                              gtk_window_set_title (GTK_WINDOW (checkdialog), _("Overwrite file?"));
                              if (gtk_dialog_run (GTK_DIALOG (checkdialog)) != GTK_RESPONSE_YES) {
                                q_log_message (_("Save cancelled on user request\n"));
                                GEDA_FREE (filename);
                                filename = NULL;
                              }
                              gtk_widget_destroy (checkdialog);
    }
    /* try saving current page of toplevel to file filename */
    if (filename != NULL) {
      x_window_save_page (w_current,
                          w_current->toplevel->page_current,
                          filename);
    }
    GEDA_FREE (filename);
    eda_config_set_boolean (cfg, "gschem", "auto-file-suffix", auto_ext);
  }
  gtk_widget_destroy (dialog);

}

/*! \brief Load/Backup selection dialog.
 *  \par Function Description
 *  This function opens a message dialog and waits for the user to choose
 *  whether to load the backup or the original file. The function would
 *  typically be call during start-up, before the main-loop is started.
 *  Therefore we must manually lock and unlock gtk-threads
 *
 *  \todo Make this a registered callback function with user data,
 *        as we'd rather be passed a GschemToplevel than a GedaToplevel.
 *
 *  \param [in] message   Message to display to user.
 *  \param [in] w_current The GedaToplevel object.
 *
 *  \return TRUE if the user wants to load the backup file, FALSE otherwise.
 */
int x_fileselect_load_backup(GString *message, GschemToplevel *w_current)
{
  GtkWidget *dialog;
  GtkWindow *window;

  int result = FALSE;

  window = w_current ? GTK_WINDOW(w_current->main_window) : NULL;

  g_string_append(message, _("\nIf you load the original file, the backup file will be overwritten in the next autosave timeout and it will be lost.\n\nDo you want to load the backup file?\n"));
  gschem_threads_enter();

  dialog = gtk_message_dialog_new (window,
                                   GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_QUESTION,
                                   GTK_BUTTONS_YES_NO,
                                   "%s", message->str);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          -1);
  gtk_dialog_add_buttons (GTK_DIALOG(dialog), GTK_STOCK_DELETE, GTK_RESPONSE_APPLY, NULL);

  gtk_widget_show (dialog);

  switch (gtk_dialog_run ((GtkDialog*)dialog)) {
    case GTK_RESPONSE_YES:
     result = 1;
     break;
    case GTK_RESPONSE_APPLY:
     result = 2; /* No and Delete backup*/
     break;
    default:
     result = 0; /* Aka No */
  }
  gtk_widget_destroy(dialog);
  gschem_threads_leave();
  return result;
}
