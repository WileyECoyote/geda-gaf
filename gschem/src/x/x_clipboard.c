/* -*- C x_clipboard.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */
/*!
 * \file x_clipboard.c
 * \brief Module to provide System Clipboard suppport
 */

#include <gschem.h>
#include <geda_debug.h>

#define CLIP_TYPE_SCHEMATIC 1

/** \defgroup Clipboard-Module Clipboard Module
 *  @{
 *  \par
 *  This module provides suppport for cut, copy and paste operations
 *  to and from the system clipboard using Gtk library routines.
 */

/*!
 * \internal Anonymous static mutex clip_got_answer_lock
 * \par
 * Mutex used to avoid collisions reading and writing got_answer
 * from gtk_clipboard_get. This is required to avoid summiting
 * queries while a query is in progress.
 */
static GedaMutex (clip_got_answer_lock);

static bool got_answer;

static void set_got_answer(int value)
{
  g_mutex_lock((GMutex*)&clip_got_answer_lock);
    got_answer = value;
  g_mutex_unlock((GMutex*)&clip_got_answer_lock);
}

static bool got_an_answer()
{
  bool ret_val;

  g_mutex_lock((GMutex*)&clip_got_answer_lock);
    ret_val = got_answer;
  g_mutex_unlock((GMutex*)&clip_got_answer_lock);

  return ret_val;
}

struct query_usable {
  void (*callback) (int, void *);
  void *userdata;
};

/*!
 * \internal Callback System Clipboard Change Ownership.
 * \par Function Description
 */
static void
clip_handle_owner_change (GtkClipboard *cb, GdkEvent *event, void *user_data)
{
  GschemToplevel *w_current = (GschemToplevel*)user_data;

  i_status_update_sensitivities (w_current);
}

static void clip_get (GtkClipboard     *cb,
                      GtkSelectionData *selection_data,
                      unsigned int      info,
                      void             *user_data_or_owner)
{
  GschemToplevel *w_current;
  GdkAtom         type;
  char           *buf;

  type = gdk_atom_intern (MIME_TYPE_SCHEMATIC, FALSE);

  if (info != CLIP_TYPE_SCHEMATIC) return;

  w_current = (GschemToplevel*)user_data_or_owner;

  /* Convert objects in the clipboard buffer to gEDA schematic format */
  buf = geda_object_save_buffer (w_current->clipboard_buffer);

  /* Set the selection appropriately */
  gtk_selection_data_set (selection_data, type,
                          8, /* 8-bit data (UTF-8) */
                          (unsigned char *) buf,
                          (int) strlen(buf));
  GEDA_FREE (buf);
}

static void clip_clear (GtkClipboard *cb, void * user_data_or_owner)
{
  GschemToplevel *w_current = user_data_or_owner;

  /* Free the objects in the clipboard buffer */
  geda_struct_object_release_objects (w_current->clipboard_buffer);

  w_current->clipboard_buffer = NULL;
}

/*!
 * \brief Initializes system clipboard support
 * \par Function Description
 *  Registers a signal handler to detect if the clipboard has changed
 *  and update the menu item sensitivity if necessary.
 */
void x_clipboard_init (GschemToplevel *w_current)
{
  w_current->clipboard_signal_id =
    g_signal_connect (gtk_clipboard_get (GDK_SELECTION_CLIPBOARD),
                      "owner-change",
                      G_CALLBACK (clip_handle_owner_change),
                      w_current);

  set_got_answer(TRUE);
}

/*!
 * \brief Finalize clipboard system
 * \par Function Description
 *  Unregisters clipboard callback handler and release ownership of
 *  contents to system.
 */
void x_clipboard_finish (GschemToplevel *w_current)
{
  GtkClipboard *cb;

  cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);

  if (w_current->clipboard_signal_id) {
    g_signal_handler_disconnect (cb, w_current->clipboard_signal_id);
  }

  gtk_clipboard_store (cb);
}

/*!
 * \brief Callback for determining if any clipboard targets are pastable
 * \par Function Description
 *  Checks if the clipboard targets match any format we recognise, then
 *  calls back into a supplied callback function which is interested in
 *  the TRUE / FALSE answer to whether we can paste from the clipboard.
 */
static void
query_usable_targets_cb (GtkClipboard *clip, GdkAtom *targets, int ntargets, void *data)
{
  struct query_usable *callback_info = data;

  int   i;
  int   is_usable = FALSE;

  for (i = 0; i < ntargets; i++) {

    char *name = gdk_atom_name (targets[i]);

    if (strcmp (name, MIME_TYPE_SCHEMATIC) == 0) {
      g_free(name);
      is_usable = TRUE;
      break;
    }

    g_free(name);
  }

  callback_info->callback (is_usable, callback_info->userdata);
  GEDA_FREE (callback_info);
  set_got_answer(TRUE);           /* Set flag */
}

/*!
 * \brief Checks if the system clipboard contains schematic data.
 * \par Function Description
 *  Checks whether the current owner of the system clipboard is
 *  advertising gEDA schematic data.
 *
 *  The check is performed asynchronously. When a response is
 *  received, the provided callback is called with a TRUE / FALSE
 *  result.
 *
 * \param [in] w_current  The current GschemToplevel.
 * \param [in] callback   The callback to receive the response.
 * \param [in] userdata   Arbitrary data to pass the callback.
 */

void x_clipboard_query_usable (GschemToplevel *w_current,
                               void (*callback) (int, void *), void *userdata)
{
  static int watch_dog;

  /* We do not want to reallocate another structure and put in a second
   * request if gtk has not responded to the previous request so check
   * if we got_an_answer, WEH (01/22/13).
   */
  if (got_an_answer()) {

    GtkClipboard *clip = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);

    /* Create a new instance of our callback structure */
    struct query_usable *callback_info;
    callback_info = g_malloc (sizeof(struct query_usable));

    /* Save callers cbfunc ptr and data ptr to the new structure */
    callback_info->callback = callback;
    callback_info->userdata = userdata;

    got_answer = FALSE; /* Block ourself; the callback has to unblock */

    /* submit the request */
    gtk_clipboard_request_targets (clip, query_usable_targets_cb, callback_info);
  }
  else {
    if (watch_dog == 2) { /* Should never get here */

      /* telling gtk to cancel does not work well, so... */
      set_got_answer(TRUE);
      watch_dog = 0;
    }
    else {
      watch_dog++;
    }
  }
}

/*!
 * \brief Set the contents of the system clipboard
 * \par Function Description
 *  Sets the system clipboard to contain the gschem objects listed in
 *  \a object_list.
 *
 * \param [in,out] w_current   The current GschemToplevel.
 * \param [in]     object_list The objects to put in the clipboard.
 *
 * \return TRUE if the clipboard is successfully set.
 */
bool x_clipboard_set (GschemToplevel *w_current, const GList *object_list)
{
  GtkClipboard  *cb;
  GtkTargetEntry target = { MIME_TYPE_SCHEMATIC, 0, CLIP_TYPE_SCHEMATIC };

  bool result;

  cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);

  /* Clear the clipboard buffer */
  if (w_current->clipboard_buffer) {
    gtk_clipboard_clear (cb);
  }

  /* Copy the objects to the clipboard buffer */
  w_current->clipboard_buffer =
    geda_copy_list (object_list, w_current->clipboard_buffer);

  /* Advertise that the data is available */
  result = gtk_clipboard_set_with_data (cb, &target, 1,
                                        clip_get, clip_clear, w_current);

  /* Notify Gtk the data should be stored to be accessed after the program
   * has quit. */
  gtk_clipboard_set_can_store (cb, NULL, 0);

  return result;
}

/*!
 * \brief Get the contents of the system clipboard
 * \par Function Description
 *  Retrieves schematic data from the system clipboard.
 *
 * \param [in,out] w_current The current GschemToplevel.
 *
 * \returns Objects retrieved from the system clipboard, or NULL
 *          if none were available.
 */
GList *x_clipboard_get (GschemToplevel *w_current)
{
  GedaToplevel        *toplevel;
  GtkClipboard        *cb;
  GdkAtom              type;
  GtkSelectionData    *selection_data;
  GList               *o_list;
  const unsigned char *buf;
  GError              *err;

  err  = NULL;
  cb   = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  type = gdk_atom_intern (MIME_TYPE_SCHEMATIC, FALSE);

  /* Try to get the contents of the clipboard */
  selection_data = gtk_clipboard_wait_for_contents (cb, type);

  if (selection_data == NULL)
    return FALSE;

  /* Convert the data buffer to Objects */
#if GTK_CHECK_VERSION(2,14,0)

  buf = gtk_selection_data_get_data (selection_data);

#else

  buf = selection_data->data;

#endif

  toplevel = w_current->toplevel;

  o_list = geda_object_read_buffer (toplevel, NULL,
                                   (char*)buf, -1, "Clipboard", &err);

  if (err) {

    const char *inv_clip = _("Invalid schematic on clipboard");
    const char *err_ins  = _("An error occurred while inserting clipboard data");
    const char *title    = _("Clipboard Insertion Failed");

    char *bold_msg;
    char *err_msg;

    u_log_message("%s. %s\n", inv_clip, err->message);

    bold_msg = geda_sprintf ("<b>%s.</b>", inv_clip);
    err_msg  = geda_sprintf ("%s: %s.", err_ins, err->message);

    titled_pango_error_dialog (bold_msg, err_msg, title);

    GEDA_FREE(bold_msg);
    GEDA_FREE(err_msg);
    g_error_free(err);
  }

  gtk_selection_data_free (selection_data);

  return o_list;
}

/** @} endgroup Clipboard-Module */
