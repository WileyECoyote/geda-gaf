/* -*- C x_fileselect.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * \file x_fileselect.c
 * \brief File system related dialogs.
 */

#include "../../include/gschem.h"
#include <geda/geda_stat.h>
#include <geda/geda_gui_funcs.h>
#include <geda_file_chooser.h>
#include <geda_image_chooser.h>
#include <geda_debug.h>
#include <errno.h>

/** \defgroup File-Dialogs File System Dialogs
 *  @{
 *  \ingroup Standard-Dialogs
 *  \defgroup File-Select-Dialogs File Select Dialog
 *    @{
 *    \ingroup File-Dialogs
 *    \image html fileselect_dialog.png
 *    \image latex fileselect_dialog.png
 */

/*! \brief Updates the preview when the selection changes.
 *  \par Function Description
 *  This is the callback function connected to the 'update-preview'
 *  signal of the <B>GedaFileChooser</B> that updates the preview
 *  widget with the name of the newly selected file.
 *
 *  \param [in] chooser   The file chooser to add the preview to.
 *  \param [in] user_data A pointer on the preview widget.
 */
static void
x_fileselect_callback_update_preview (GtkWidget *chooser,
                                      void      *user_data)
{
  GschemPreview *preview = GSCHEM_PREVIEW(user_data);
  char *preview_filename = NULL;
  char *filename;

  filename = geda_file_chooser_get_filename(chooser);

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

/*! \brief Update file chooser preview window size callback
 *  \par Function Description
 *  This function is called when the size check-box is toggled.
 *  The state of the check-box is retrieved and the "large-size"
 *  property of the Preview Widget is set to state.
 */
static void
x_fileselect_callback_update_size (GtkToggleButton *button,
                                   void            *user_data)
{
  GschemPreview *preview = GSCHEM_PREVIEW(user_data);
  int state = GetToggleState(button);
  g_object_set (preview, "large-size", state, NULL);
}

/*! \brief Adds a preview to a file chooser.
 *  \par Function Description
 *  This function adds a preview section to a <B>GedaFileChooser</B>.
 *
 *  The <B>Preview</B> object is inserted in a frame and alignment
 *  widget for accurate positionning.
 *
 *  Other widgets can be added to this preview area for example to
 *  enable/disable the preview. Currently, the preview is always
 *  active.
 *
 *  Function <B>x_fileselect_callback_update_preview()</B> is
 *  connected to the signal 'update-preview' of <B>GedaFileChooser</B>
 *  so that it redraws the preview area every time a new file is
 *  selected.
 *
 *  \param [in] filechooser The file chooser to add the preview to.
 */
static void
x_fileselect_add_preview (GedaFileChooser *filechooser)
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

  preview = GTK_WIDGET (g_object_new (GSCHEM_TYPE_PREVIEW,
                                      /*"active", TRUE,*/
                                      NULL));

  geda_container_add (alignment, preview);
  geda_container_add (frame, alignment);
  geda_container_add (vbox, frame);
  gtk_widget_show_all (frame);

  cb_size = gtk_check_button_new_with_label (_("Large"));
  gtk_widget_set_tooltip_text(cb_size, _("Enable to enlarge the preview"));
  gtk_box_pack_start (GTK_BOX(vbox), cb_size, FALSE, FALSE, 0);
  gtk_widget_show (cb_size);

  SetToggleState (cb_size, FALSE);

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

static void
x_fileselect_save_filter_index (GtkWidget      *chooser,
                                GschemToplevel *w_current)
{
  w_current->chooser_filter = geda_file_chooser_get_filter(chooser);
  default_chooser_filter    = w_current->chooser_filter;
}

/*! \brief Opens a file chooser and selection of document to open.
 *  \par Function Description
 *  This function opens a file chooser dialog and restores the users
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
 */
GSList *
x_fileselect_list(GschemToplevel *w_current)
{
  GtkWidget *dialog;
  GSList    *filenames;
  char      *cwd;

  dialog = geda_file_chooser_new (w_current->main_window,
                                  FILE_CHOOSER_ACTION_OPEN);

  /* Set filter to what user last time*/
  geda_file_chooser_set_filter (dialog, w_current->chooser_filter);

  /* Conditionally add the file previewer */
  if (w_current->file_preview == TRUE) {
    x_fileselect_add_preview (GEDA_FILE_CHOOSER (dialog));
  }

  /* force start in current working directory, NOT in 'Recently Used' */
  cwd = g_get_current_dir ();
  geda_file_chooser_set_current_folder (dialog, cwd);
  GEDA_FREE (cwd);

  gtk_widget_show (dialog);

  /* This retains filter, even if canceled, could retrieve in if got
   * filenames but this seems to work just fine, is saved if changed */
  g_signal_connect_after(dialog, "filter-changed",
                         G_CALLBACK (x_fileselect_save_filter_index),
                         w_current);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GEDA_RESPONSE_ACCEPT) {
    filenames =  geda_file_chooser_get_filenames (dialog);
  }
  else {
    filenames = NULL;
  }
  gtk_widget_destroy (dialog);
  return filenames;
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
  GedaToplevel *toplevel = w_current->toplevel;
  GtkWidget    *dialog;
  GtkWidget    *hbox;
  GtkWidget    *cb_add_ext;
  bool          auto_ext;

  EdaConfig *cfg = eda_config_get_user_context ();

  auto_ext =
  eda_config_get_boolean (cfg, IVAR_CONFIG_GROUP, "auto-file-suffix", NULL);

  dialog = geda_file_chooser_new (w_current->main_window,
                                  FILE_CHOOSER_ACTION_SAVE);

  if (geda_struct_page_is_symbol_file(toplevel->page_current)) {
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

    /* force save in current working directory */

    char *cwd = g_get_current_dir ();

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
  geda_file_chooser_set_extra_widget (dialog, hbox);

  SetToggleState (cb_add_ext, auto_ext);

  gtk_widget_show (dialog);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GEDA_RESPONSE_ACCEPT) {

    const char *filebase;
          char *filename;
          char *tmpname;

    filename = geda_file_chooser_get_filename (dialog);
    filebase = geda_get_basename(filename);
    auto_ext = GetToggleState (cb_add_ext);
    tmpname  = NULL;

    if (auto_ext && (filebase != NULL)) {

      if (!geda_file_get_filename_ext(filebase)) {

        int index = geda_file_chooser_get_filter(dialog);

        if (index == FILTER_SCHEMATIC) {
          tmpname = geda_strconcat(filename, SCHEMATIC_FILE_DOT_SUFFIX, NULL);
        }
        else if (index == FILTER_SYMBOL) {
           tmpname = geda_strconcat(filename, SYMBOL_FILE_DOT_SUFFIX, NULL);
        }

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
                              "%s %s.\n\n%s?", filename, _("already exists"), _("Overwrite"));
                              gtk_window_set_title (GTK_WINDOW (checkdialog), _("Overwrite file?"));
                              if (gtk_dialog_run (GTK_DIALOG (checkdialog)) != GEDA_RESPONSE_YES) {
                                q_log_message (_("Save canceled on user request\n"));
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

/** @} endgroup File-Select-Dialogs */

/** \defgroup Image-Select-Dialog Image Select Dialog
 *  @{
 *  \ingroup File-Dialogs
 *  \image html image_chooser_dialog.png
 *  \image latex image_chooser_dialog.png
 */

/*! \brief Opens a Image Chooser Dialog for selecting one image file.
 *  \par Function Description
 *  This function opens a Image Chooser Dialog, restores the user's
 *  filter preference, and wait for the user to select at one file to
 *  load. In the toplevel contains a pixbuf_filename, that file name
 *  is set in the dialog. The users filter preference is Preserved
 *  regardless of whether the operation is canceled or not.
 *
 *  \param [in] w_current The GschemToplevel environment
 *  \param [in] filename  Optional file name to fill in the chooser.
 *
 *  \returns pointer to filename string or NULL if the operation was
 *           canceled by the user. The returned string must be freed
 *           by the caller.
 *
 *  \sa x_fileselect_list
 */
char *
x_fileselect_select_image(GschemToplevel *w_current, const char *filename)
{
  GtkWidget  *dialog;
  char       *outfile;

  dialog = geda_image_chooser_new (w_current->main_window,
                                   IMAGE_CHOOSER_ACTION_OPEN);

  g_object_set (dialog, "select-multiple", FALSE, NULL);

  /* "local-only", TRUE, */

  /* If a file name was provided then use the path from the file
   * name then as the starting point if the path exist, and is
   * readable by the current user */
  if (filename) {

    char *filepath = geda_get_dirname (filename);

    if (filepath && g_file_test (filepath, G_FILE_TEST_IS_DIR)) {

      errno = 0;
      access(filepath, R_OK);

      if (!errno) {
        geda_image_chooser_set_current_folder(dialog, filepath);;
      }

      GEDA_FREE(filepath);
    }

    geda_image_chooser_set_filename (dialog, geda_file_get_basename(filename));
  }
  else {

    /* Check for w_current last_image_path and use if present */
    char *path = gschem_toplevel_get_last_image_path (w_current);

    if (path) {
      geda_image_chooser_set_current_folder (dialog, path);
    }
    else { /* start in current working directory, NOT in 'Recently Used' */

      char *cwd = g_get_current_dir ();

      geda_image_chooser_set_current_folder (dialog, cwd);

      GEDA_FREE (cwd);
    }
  }

  gtk_widget_show (dialog);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GEDA_RESPONSE_ACCEPT) {

    outfile = geda_image_chooser_get_filename (dialog);

    if (outfile !=NULL) {

      char *file_path = geda_get_dirname(outfile);

      gschem_toplevel_set_last_image_path(w_current, file_path);
    }
  }
  else {
    outfile = NULL;
  }

  gtk_widget_destroy (dialog);

  return outfile;
}

/** @} endgroup Image-Select-Dialog */
/** @} endgroup File-Dialog */

/** \defgroup Load-Backup-Dialog Load Backup Dialog
 *  @{
 *  \ingroup Systemic-Dialogs
 *  \image html load_backup.png
 *  \image latex load_backup.png
 */

/*! \brief Load/Backup selection dialog.
 *  \par Function Description
 *  This function opens a message dialog and waits for the user to choose
 *  whether to load the backup or the original file. The function would
 *  typically be call during start-up, before the main-loop is started.
 *  Therefore we must manually lock and unlock gtk-threads
 *
 *  \param [in] message   Message to display to user.
 *  \param [in] w_current The GedaToplevel object.
 *
 *  \return TRUE if the user wants to load the backup file, FALSE otherwise.
 */
int
x_fileselect_load_backup(const char *message, GschemToplevel *w_current)
{
  GtkWidget  *dialog;
  GtkWindow  *window;
  char       *string;
  const char *inquire;
  int result = FALSE;

  inquire = _("If you load the original file, the backup file will be "
              "overwritten in the next autosave timeout and will be lost.\n\n"
              "Do you want to load the backup file?");

  string = geda_sprintf ("%s\n%s", message, inquire);

  window  = w_current ? GTK_WINDOW(w_current->main_window) : NULL;

  dialog = gtk_message_dialog_new (window,
                                   GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_QUESTION,
                                   GTK_BUTTONS_YES_NO,
                                   "%s", string);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GEDA_RESPONSE_YES,
                                          GEDA_RESPONSE_NO,
                                          -1);
  gtk_dialog_add_buttons (GTK_DIALOG(dialog), GTK_STOCK_DELETE, GEDA_RESPONSE_APPLY, NULL);

  gtk_widget_show (dialog);

  switch (gtk_dialog_run ((GtkDialog*)dialog)) {

    case GEDA_RESPONSE_YES:
     result = 1;
     break;

    case GEDA_RESPONSE_APPLY:
     result = 2; /* No and Delete backup*/
     break;

    default:
     result = 0; /* Aka No */
  }

  gtk_widget_destroy(dialog);

  GEDA_FREE(string);

  return result;
}

/** @} endgroup Load-Backup-Dialog */
