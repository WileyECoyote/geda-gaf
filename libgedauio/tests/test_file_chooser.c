/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_file_chooser.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This Program is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of the
 * License.
 *
 * This Program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: March, 13, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <glib.h>
#include <gtk/gtk.h>
#include <geda/geda.h>
#include <geda_file_chooser.h>
#include "../include/gettext.h"

#define TDIALOG "GedaFileChooser"

/*! \file test_file_chooser.c
 *  \brief Tests for geda_file_chooser.c module
 */

#ifdef OS_WIN32
#  include <io.h>
#  define localtime_r(t,b) *(b) = *localtime (t)
#  ifndef S_ISREG
#    define S_ISREG(m) ((m) & _S_IFREG)
#  endif
#endif

#include "prop-editor.h"

static GtkWidget *preview_label;
static GtkWidget *preview_image;
static GtkFileChooserAction action;

static void print_current_folder (GtkFileChooser *chooser)
{
  char *uri;

  uri = gtk_file_chooser_get_current_folder_uri (chooser);

  printf ("Current folder changed :\n  %s\n", uri ? uri : "(null)");
  g_free (uri);
}

static void print_selected (GtkFileChooser *chooser)
{
  GSList *uris = gtk_file_chooser_get_uris (chooser);
  GSList *tmp_list;

  printf ("Selection changed :\n");

  for (tmp_list = uris; tmp_list; tmp_list = tmp_list->next) {
      char *uri = tmp_list->data;
      printf ("  %s\n", uri);
      g_free (uri);
  }

  printf ("\n");
  g_slist_free (uris);
}

static void response_cb (GtkDialog *dialog, int response_id)
{
  if (response_id == GEDA_RESPONSE_OK) {

      GSList *list;

      list = gtk_file_chooser_get_uris (GTK_FILE_CHOOSER (dialog));

      if (list) {

      GSList *l;

      printf ("Selected files:\n");

      for (l = list; l; l = l->next) {

          printf ("%s\n", (char *) l->data);
          g_free (l->data);
        }

      g_slist_free (list);
    }
      else
    printf ("No selected files\n");
    }
  else
    printf ("Dialog was closed\n");

  gtk_main_quit ();
}

static void filter_changed (GtkFileChooserDialog *dialog, void *data)
{
  printf ("file filter changed\n");
}

static char *format_time (time_t t)
{
  char   buf[128];
  struct tm tm_buf;
  time_t now = time (NULL);
  const char *format;

  if (abs (now - t) < 24*60*60) {
    format = "%X";
  }
  else {
    format = "%x";
  }

  localtime_r (&t, &tm_buf);

  if (strftime (buf, sizeof(buf), format, &tm_buf) == 0) {
    return g_strdup ("<unknown>");
  }
  else {
    return g_strdup (buf);
  }
}

static char *format_size (int64_t size)
{
  if (size < (int64_t)1024) {
    return g_strdup_printf ("%d bytes", (gint)size);
  }
  else if (size < (int64_t)1024*1024) {
    return g_strdup_printf ("%.1f K", size / (1024.));
  }
  else if (size < (int64_t)1024*1024*1024) {
    return g_strdup_printf ("%.1f M", size / (1024.*1024.));
  }

  return g_strdup_printf ("%.1f G", size / (1024.*1024.*1024.));
}

static void size_prepared_cb (GdkPixbufLoader *loader,
                              int              width,
                              int              height,
                              int             *data)
{
    int des_width = data[0];
    int des_height = data[1];

    if (des_height >= height && des_width >= width) {
        /* Nothing */
    }
    else if ((double)height * des_width > (double)width * des_height) {
        width = 0.5 + (double)width * des_height / (double)height;
        height = des_height;
    }
    else {
        height = 0.5 + (double)height * des_width / (double)width;
        width = des_width;
    }

    gdk_pixbuf_loader_set_size (loader, width, height);
}

GdkPixbuf *my_new_from_file_at_size (const char *filename,
                                     int         width,
                                     int         height,
                                     GError    **error)
{
    GdkPixbufLoader *loader;
    GdkPixbuf       *pixbuf;
    int              info[2];
    struct stat st;

    unsigned char buffer [4096];
    int length;
    FILE *f;

    g_return_val_if_fail (filename != NULL, NULL);
        g_return_val_if_fail (width > 0 && height > 0, NULL);

    if (stat (filename, &st) != 0) {
                int errsv = errno;

        g_set_error (error,
                 G_FILE_ERROR,
                 g_file_error_from_errno (errsv),
                 _("Could not get information for file '%s': %s"),
                 filename, g_strerror (errsv));
        return NULL;
    }

    if (!S_ISREG (st.st_mode)) {
        return NULL;
    }

    f = fopen (filename, "rb");
    if (!f) {

      int errsv = errno;

      g_set_error (error,
                   G_FILE_ERROR,
                   g_file_error_from_errno (errsv),
                   _("Failed to open file '%s': %s"),
                     filename, g_strerror (errsv));
                   return NULL;
    }

    loader = gdk_pixbuf_loader_new ();
#ifdef DONT_PRESERVE_ASPECT
    gdk_pixbuf_loader_set_size (loader, width, height);
#else
    info[0] = width;
    info[1] = height;
    g_signal_connect (loader, "size-prepared", G_CALLBACK (size_prepared_cb), info);
#endif

    while (!feof (f)) {

      length = fread (buffer, 1, sizeof(buffer), f);

      if (length > 0)
        if (!gdk_pixbuf_loader_write (loader, buffer, length, error)) {
          gdk_pixbuf_loader_close (loader, NULL);
          fclose (f);
          g_object_unref (loader);
          return NULL;
        }
    }

    fclose (f);

    if (!gdk_pixbuf_loader_close (loader, error)) {
        g_object_unref (loader);
        return NULL;
    }

    pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);

    if (!pixbuf) {
        g_object_unref (loader);

        /* did the loader set an error? */
        if (*error != NULL)
            return NULL;

        g_set_error (error,
                             GDK_PIXBUF_ERROR,
                             GDK_PIXBUF_ERROR_FAILED,
                             _("Failed to load image '%s': reason not known, probably a corrupt image file"),
                             filename);
        return NULL;
    }

    g_object_ref (pixbuf);

    g_object_unref (loader);

    return pixbuf;
}

static void
update_preview_cb (GtkFileChooser *chooser)
{
  char *filename = gtk_file_chooser_get_preview_filename (chooser);
  int have_preview = FALSE;

  if (filename) {

    GdkPixbuf *pixbuf;
    GError *error = NULL;

    pixbuf = my_new_from_file_at_size (filename, 128, 128, &error);

    if (pixbuf) {

      gtk_image_set_from_pixbuf (GTK_IMAGE (preview_image), pixbuf);
      g_object_unref (pixbuf);
      gtk_widget_show (preview_image);
      gtk_widget_hide (preview_label);
      have_preview = TRUE;
    }
    else {

      struct stat buf;
      if (stat (filename, &buf) == 0) {

        char *preview_text;
        char *size_str;
        char *modified_time;

        size_str = format_size (buf.st_size);
        modified_time = format_time (buf.st_mtime);

        preview_text = g_strdup_printf ("<i>Modified:</i>\t%s\n"
        "<i>Size:</i>\t%s\n",
        modified_time,
        size_str);
        gtk_label_set_markup (GTK_LABEL (preview_label), preview_text);
        g_free (modified_time);
        g_free (size_str);
        g_free (preview_text);

        gtk_widget_hide (preview_image);
        gtk_widget_show (preview_label);
        have_preview = TRUE;
      }
    }

    g_free (filename);

    if (error)
      g_error_free (error);
  }

  gtk_file_chooser_set_preview_widget_active (chooser, have_preview);
}

static void
set_current_folder (GtkFileChooser *chooser, const char *name)
{
  if (!gtk_file_chooser_set_current_folder (chooser, name)) {

    GtkWidget *dialog;

    dialog = gtk_message_dialog_new (GTK_WINDOW (chooser),
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     "Could not set the folder to %s",
                                     name);
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
  }
}

static void
set_folder_nonexistent_cb (GtkButton *button, GtkFileChooser *chooser)
{
  set_current_folder (chooser, "/nonexistent");
}

static void
set_folder_existing_nonexistent_cb (GtkButton *button, GtkFileChooser *chooser)
{
  set_current_folder (chooser, "/usr/nonexistent");
}

static void
set_filename (GtkWidget *chooser, const char *name)
{
  if (!geda_file_chooser_set_filename (chooser, name)) {

    GtkWidget *dialog;

    dialog = gtk_message_dialog_new (GTK_WINDOW (chooser),
                                     GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     "Could not select %s",
                                     name);
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
  }
}

static void
set_filename_nonexistent_cb (GtkButton *button, GedaFileChooser *chooser)
{
  set_filename (GTK_WIDGET(chooser), "/nonexistent");
}

static void
set_filename_existing_nonexistent_cb (GtkButton *button, GedaFileChooser *chooser)
{
  set_filename (GTK_WIDGET(chooser), "/usr/nonexistent");
}

static void
unmap_and_remap_cb (GtkButton *button, GtkFileChooser *chooser)
{
  gtk_widget_hide (GTK_WIDGET (chooser));
  gtk_widget_show (GTK_WIDGET (chooser));
}

static void
kill_dependent (GtkWindow *win, GtkWidget *dep)
{
  gtk_widget_destroy (GTK_WIDGET(dep));
}

static void
notify_multiple_cb (GtkWidget *dialog, GParamSpec *pspec, GtkWidget *button)
{
  int multiple;

  multiple = gtk_file_chooser_get_select_multiple (GTK_FILE_CHOOSER (dialog));

  gtk_widget_set_sensitive (button, multiple);
}

static GtkFileChooserConfirmation
confirm_overwrite_cb (GtkFileChooser *chooser, void *data)
{
  GtkWidget *dialog;
  GtkWidget *button;
  int response;
  GtkFileChooserConfirmation conf;

  dialog = gtk_message_dialog_new (GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (chooser))),
                   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                   GTK_MESSAGE_QUESTION,
                   GTK_BUTTONS_NONE,
                   "What do you want to do?");

  button = gtk_button_new_with_label ("Use the stock confirmation dialog");
  gtk_widget_show (button);
  gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, 1);

  button = gtk_button_new_with_label ("Type a new file name");
  gtk_widget_show (button);
  gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, 2);

  button = gtk_button_new_with_label ("Accept the file name");
  gtk_widget_show (button);
  gtk_dialog_add_action_widget (GTK_DIALOG (dialog), button, 3);

  response = gtk_dialog_run (GTK_DIALOG (dialog));

  switch (response) {

    case 1:
      conf = GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM;
      break;

    case 3:
      conf = GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME;
      break;

    default:
      conf = GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN;
      break;
    }

  gtk_widget_destroy (dialog);

  return conf;
}

int
main (int argc, char **argv)
{
  GtkWidget *control_window;
  GtkWidget *vbbox;
  GtkWidget *button;
  GtkWidget *dialog;
  GtkWidget *prop_editor;
  GtkWidget *extra;
  GtkWidget *preview_vbox;

  int   force_rtl        = FALSE;
  int   multiple         = FALSE;
  char *action_arg       = NULL;
  char *initial_filename = NULL;
  char *initial_folder   = NULL;
  GError *error          = NULL;

  GOptionEntry options[] = {
    { "action", 'a', 0, G_OPTION_ARG_STRING, &action_arg, "Filechooser action", "ACTION" },
    { "multiple", 'm', 0, G_OPTION_ARG_NONE, &multiple, "Select-multiple", NULL },
    { "right-to-left", 'r', 0, G_OPTION_ARG_NONE, &force_rtl, "Force right-to-left layout.", NULL },
    { "initial-filename", 'f', 0, G_OPTION_ARG_FILENAME, &initial_filename, "Initial filename to select", "FILENAME" },
    { "initial-folder", 'F', 0, G_OPTION_ARG_FILENAME, &initial_folder, "Initial folder to show", "FILENAME" },
    { NULL }
  };

  if (!gtk_init_with_args (&argc, &argv, "", options, NULL, &error)) {
      printf ("Failed to parse args: %s\n", error->message);
      g_error_free (error);
      return 1;
  }

  if (initial_filename && initial_folder) {
      printf ("Only one of --initial-filename and --initial-folder may be specified");
      return 1;
  }

  if (force_rtl) {
    gtk_widget_set_default_direction (GTK_TEXT_DIR_RTL);
  }

  action = FILE_CHOOSER_ACTION_OPEN;

  if (action_arg != NULL) {

    if (! strcmp ("open", action_arg))
      action = FILE_CHOOSER_ACTION_OPEN;
    else if (! strcmp ("save", action_arg))
      action = FILE_CHOOSER_ACTION_SAVE;
    else if (! strcmp ("select_folder", action_arg))
      action = FILE_CHOOSER_ACTION_SELECT_FOLDER;
    else if (! strcmp ("create_folder", action_arg))
      action = FILE_CHOOSER_ACTION_CREATE_FOLDER;

    g_free (action_arg);
  }

  dialog = geda_file_chooser_new (NULL, action);

  g_signal_connect (dialog, "selection-changed",
            G_CALLBACK (print_selected), NULL);
  g_signal_connect (dialog, "current-folder-changed",
            G_CALLBACK (print_current_folder), NULL);
  g_signal_connect (dialog, "response",
            G_CALLBACK (response_cb), NULL);
  g_signal_connect (dialog, "confirm-overwrite",
            G_CALLBACK (confirm_overwrite_cb), NULL);

  /* Preview widget */
  /* THIS IS A TERRIBLE PREVIEW WIDGET, AND SHOULD NOT BE COPIED AT ALL.
   */
  preview_vbox = gtk_vbox_new (0, FALSE);
  /*gtk_file_chooser_set_preview_widget (GTK_FILE_CHOOSER (dialog), preview_vbox);*/

  preview_label = gtk_label_new (NULL);
  gtk_box_pack_start (GTK_BOX (preview_vbox), preview_label, TRUE, TRUE, 0);
  gtk_misc_set_padding (GTK_MISC (preview_label), 6, 6);

  preview_image = gtk_image_new ();
  gtk_box_pack_start (GTK_BOX (preview_vbox), preview_image, TRUE, TRUE, 0);
  gtk_misc_set_padding (GTK_MISC (preview_image), 6, 6);

  update_preview_cb (GTK_FILE_CHOOSER (dialog));
  g_signal_connect (dialog, "update-preview",
                    G_CALLBACK (update_preview_cb), NULL);

  /* Extra widget */

  extra = gtk_check_button_new_with_mnemonic ("asks about this button");
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (extra), TRUE);
  geda_file_chooser_set_extra_widget (dialog, extra);

  /* Shortcuts */

  gtk_file_chooser_add_shortcut_folder_uri (GTK_FILE_CHOOSER (dialog),
                                            "file:///usr/share/pixmaps",
                                            NULL);

  /* Initial filename or folder */

  if (initial_filename) {
    set_filename (dialog, initial_filename);
  }

  if (initial_folder) {
    set_current_folder (GTK_FILE_CHOOSER (dialog), initial_folder);
  }

  g_signal_connect (dialog, "notify::filter",
                    G_CALLBACK (filter_changed), NULL);

  /* show_all() to reveal bugs in composite widget handling */
  gtk_widget_show_all (dialog);

  /* Extra controls for manipulating the test environment
   */
  prop_editor = create_prop_editor (G_OBJECT (dialog), GTK_TYPE_FILE_CHOOSER);
  prop_editor = prop_editor;

  control_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  vbbox = gtk_vbutton_box_new ();
  gtk_container_add (GTK_CONTAINER (control_window), vbbox);

  button = gtk_button_new_with_mnemonic ("_Select all");
  gtk_widget_set_sensitive (button, multiple);
  gtk_container_add (GTK_CONTAINER (vbbox), button);
  g_signal_connect_swapped (button, "clicked",
                G_CALLBACK (gtk_file_chooser_select_all), dialog);
  g_signal_connect (dialog, "notify::select-multiple",
            G_CALLBACK (notify_multiple_cb), button);

  button = gtk_button_new_with_mnemonic ("_Unselect all");
  gtk_container_add (GTK_CONTAINER (vbbox), button);
  g_signal_connect_swapped (button, "clicked",
                G_CALLBACK (gtk_file_chooser_unselect_all), dialog);

  button = gtk_button_new_with_label ("set_current_folder (\"/nonexistent\")");
  gtk_container_add (GTK_CONTAINER (vbbox), button);
  g_signal_connect (button, "clicked",
            G_CALLBACK (set_folder_nonexistent_cb), dialog);

  button = gtk_button_new_with_label ("set_current_folder (\"/usr/nonexistent\")");
  gtk_container_add (GTK_CONTAINER (vbbox), button);
  g_signal_connect (button, "clicked",
            G_CALLBACK (set_folder_existing_nonexistent_cb), dialog);

  button = gtk_button_new_with_label ("set_filename (\"/nonexistent\")");
  gtk_container_add (GTK_CONTAINER (vbbox), button);
  g_signal_connect (button, "clicked",
            G_CALLBACK (set_filename_nonexistent_cb), dialog);

  button = gtk_button_new_with_label ("set_filename (\"/usr/nonexistent\")");
  gtk_container_add (GTK_CONTAINER (vbbox), button);
  g_signal_connect (button, "clicked",
            G_CALLBACK (set_filename_existing_nonexistent_cb), dialog);

  button = gtk_button_new_with_label ("Unmap and remap");
  gtk_container_add (GTK_CONTAINER (vbbox), button);
  g_signal_connect (button, "clicked",
            G_CALLBACK (unmap_and_remap_cb), dialog);

  gtk_widget_show_all (control_window);

  g_object_ref (control_window);
  g_signal_connect (dialog, "destroy",
            G_CALLBACK (kill_dependent), control_window);

  /* We need to hold a ref until we have destroyed the widgets, just in case
   * someone else destroys them.  We explicitly destroy windows to catch leaks.
   */
  g_object_ref (dialog);
  gtk_main ();
  gtk_widget_destroy (dialog);
  g_object_unref (dialog);

  return 0;
}
