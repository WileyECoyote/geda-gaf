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
#include <config.h>

#include "gschem.h"
#include "x_fileselect.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static GschemFileFilterDataDef filter_data[] = {
    GSCHEM_FILTER_SCHEMATIC,
    GSCHEM_FILTER_SYMBOL,
    GSCHEM_FILTER_BOTH,
    GSCHEM_FILTER_NONE,
    GSCHEM_NO_MORE_FILTERS
};

/*! \brief Creates filter for file chooser.
 *  \par Function Description
 *  This function adds file filters to <B>filechooser</B>.
 *
 *  \param [in] filechooser The file chooser to add filter to.
 */
static void
x_fileselect_setup_file_filters (GtkFileChooser *filechooser)
{
  GtkFileFilter           *filter;
  GschemFileFilterDataDef *data;
  int i;

  for (data = filter_data; data->name != NULL; data++) {
    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name(filter, data->name);
    for (i = 0; data->pattern[i] != '\0'; i++) {
      const char *ext = data->pattern[i];
      gtk_file_filter_add_pattern (filter, ext);
    }
    g_object_set_data( G_OBJECT(filter), "id", GINT_TO_POINTER(data->id));
    gtk_file_chooser_add_filter (filechooser, filter);
  }
}

/*! \brief Updates the preview when the selection changes.
 *  \par Function Description
 *  This is the callback function connected to the 'update-preview'
 *  signal of the <B>GtkFileChooser</B>.
 *
 *  It updates the preview widget with the name of the newly selected
 *  file.
 *
 *  \param [in] chooser   The file chooser to add the preview to.
 *  \param [in] user_data A pointer on the preview widget.
 */
static void
x_fileselect_callback_update_preview (GtkFileChooser *chooser,
                                      gpointer user_data)
{
  Preview *preview = PREVIEW (user_data);
  char *filename, *preview_filename = NULL;

  filename = gtk_file_chooser_get_preview_filename (chooser);
  if (filename != NULL &&
    !g_file_test (filename, G_FILE_TEST_IS_DIR)) {
    preview_filename = filename;
    }

    /* update preview */
    g_object_set (preview,
                  "filename", preview_filename,
                  "active", (preview_filename != NULL),
                  NULL);

    GEDA_FREE (filename);
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
  gtk_widget_show_all (frame);

  g_object_set (filechooser,
                /* GtkFileChooser */
                "use-preview-label", FALSE,
                "preview-widget", frame,
                NULL);

  /* connect callback to update preview */
  g_signal_connect (filechooser,
                    "update-preview",
                    G_CALLBACK (x_fileselect_callback_update_preview),
                    preview);

}

/*! \brief Opens a file chooser for opening one or more schematics.
 *  \par Function Description
 *  This function opens a file chooser dialog and wait for the user to
 *  select at least one file to load as <B>w_current</B>'s new pages.
 *
 *  The function updates the user interface.
 *
 *  At the end of the function, the w_current->toplevel's current page
 *  is set to the page of the last loaded page.
 *
 *  \param [in] w_current The GschemToplevel environment.
 */
void x_fileselect_open(GschemToplevel *w_current)
{
  Page *page = NULL;
  GtkWidget *dialog;
  char *cwd;

  dialog = gtk_file_chooser_dialog_new (_("Open..."),
                                        GTK_WINDOW(w_current->main_window),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN,   GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  /* 09/09/12 W. E. Hill Added conditional to check state of configuration
   * variable file_preview.
   *
   * Conditionally add the file previewer
   */
  if(w_current->file_preview == TRUE)
    x_fileselect_add_preview (GTK_FILE_CHOOSER (dialog));

  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", TRUE,
                NULL);
  /* add file filters to dialog */
  x_fileselect_setup_file_filters (GTK_FILE_CHOOSER (dialog));

  /* force start in current working directory, not in 'Recently Used' */
  cwd = g_get_current_dir ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
  GEDA_FREE (cwd);

  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {

    GSList *tmp, *filenames =
    gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));

    /* open each file */
    for (tmp = filenames; tmp != NULL; tmp = g_slist_next (tmp)) {

      if(tmp->data != NULL) {
        page = x_window_open_page (w_current, (char*)tmp->data);
      }
      else {
        u_log_message("<x_window_open_page> error: file name should not be NULL");
      }
    }
    /* Switch to the last page opened */
    if ( page != NULL )
      x_window_set_current_page (w_current, page);

    /* free the list of filenames */
    g_slist_foreach (filenames, (GFunc)g_free, NULL);
    g_slist_free (filenames);
  }

  gtk_widget_destroy (dialog);

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

  dialog = gtk_file_chooser_dialog_new (_("Save as..."),
                                        GTK_WINDOW(w_current->main_window),
                                        GTK_FILE_CHOOSER_ACTION_SAVE,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_SAVE,   GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  /* set default response signal. This is usually triggered by the
   *     "Return" key */
  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
                                  GTK_RESPONSE_ACCEPT);

  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", FALSE,
                /* only in GTK 2.8 */
                /* "do-overwrite-confirmation", TRUE, */
                NULL);
  /* add file filters to dialog */
  x_fileselect_setup_file_filters (GTK_FILE_CHOOSER (dialog));

  /* set the current filename or directory name if new document */
  if ((toplevel->page_current->filename != NULL) &&
       g_file_test (toplevel->page_current->filename,
       G_FILE_TEST_EXISTS)) {
    gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog),
                                   toplevel->page_current->filename);
  }
  else {
    cwd = g_get_current_dir ();
    /* force save in current working dir */
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
    GEDA_FREE (cwd);
    gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog),
                                       toplevel->untitled_name);
  }

  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

  /* Add our extra widget to the dialog */
  hbox = gtk_hbox_new(FALSE, 0);

  cb_add_ext = gtk_check_button_new_with_label (_("Auto file Suffix"));

  gtk_widget_show (cb_add_ext);
  gtk_box_pack_start (GTK_BOX(hbox), cb_add_ext, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text(cb_add_ext, _("Automatically append the file extension"));
  gtk_toggle_button_set_active ((GtkToggleButton*)cb_add_ext, auto_ext);
  gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(dialog), hbox);

  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    char          *filename;
    char          *filebase;
    char          *tmpname;
    GtkFileFilter *filter;
    int            type;

    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    filebase = basename(filename);
    auto_ext = gtk_toggle_button_get_active ((GtkToggleButton*)cb_add_ext);
    tmpname  = NULL;
    if (auto_ext && (filebase != NULL)) {
      if (!get_filename_ext(filebase)) {
        filter = gtk_file_chooser_get_filter( GTK_FILE_CHOOSER(dialog));
        type = GPOINTER_TO_INT( g_object_get_data(G_OBJECT(filter), "id" ));
        if (type == FILTER_SCHEMATIC)
          tmpname = g_strconcat(filename, SCHEMATIC_FILE_DOT_SUFFIX, NULL);
        else
          if (type == FILTER_SYMBOL)
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
  gdk_threads_enter();

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
  gdk_threads_leave();
  return result;
}
