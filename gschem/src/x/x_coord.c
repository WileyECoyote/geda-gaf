/* -*- C x_coord.c  indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_coord.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 * \file x_coord.c
 * \brief A dialog box for selecting Component Symbols
 */

#include <ctype.h>

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"
#include "../../include/x_dnd.h"

#include <geda_widgets.h>
#include <geda_debug.h>

/** \defgroup Coordinates-Dialog Coordinates Dialog
 *  @{
 *  \ingroup Systemic-Dialogs
 *
 *  \par
 *   This group contains routines for the coordinates dialog and handles
 *   Drag-N-Drop events on the coordinates world entry widget.
 */

/** \defgroup Coordinates-Drag-N-Drop-Destination Coordinates Dialog Drag-N-Drop
 *  @{
 *  \par
 *   This sub-group contains routines to handle signals receivable by
 *   the Coordinates-Dialog as the destination. The Coordinates-Dialog
 *   is not a Drag-N-Drop Source.
*/

static GtkTargetEntry dnd_target_list[] = {
    GSCHEM_TARGET_NONE,
    GSCHEM_TARGET_TEXT,
    GSCHEM_TARGET_STRING,
    GSCHEM_TARGET_TEXT_PLAIN,
    GSCHEM_TARGET_UTF8_STRING,
    GSCHEM_TARGET_OBJECTS,
};

/*!
 * \brief When Drag Received from the Source
 * \par Function Description
 * Called when the data has been received from the source. We should check
 * the GtkSelectionData sent by the source, and do something with the data.
 * After handling the data we call complete the operation by calling the
 * function gtk_drag_finish, which will emit the "data-delete" signal if
 * told to.
*/
void x_dialog_coord_dnd_drag_receive(GtkWidget        *widget,
                                     GdkDragContext   *context, int x, int y,
                                     GtkSelectionData *selection_data,
                                     unsigned int      target_type,
                                     unsigned int      time,
                                     GschemToplevel   *w_current)
{
  GedaToplevel *toplevel;
  GtkWidget    *world_entry;
  GError       *err;

  bool dnd_success           = FALSE;
  bool delete_selection_data = FALSE;

#if DEBUG || DEBUG_DND_EVENTS
  const char *WidgetId;
  WidgetId = gtk_widget_get_name (widget);
  printf ("%s: %s:", WidgetId, __func__);
#endif

  toplevel = w_current->toplevel;
  err = NULL;

  /* Deal with what we are given from source */
  if ((selection_data != NULL) &&
      (gtk_selection_data_get_length(selection_data) >= 0))
  {
    GtkWidget *Dialog;
    char      *buffer;

    if (context->suggested_action == GDK_ACTION_ASK) {
      /* Ask the user to move or copy, then set the context action. */
    }
    else if (context->suggested_action == GDK_ACTION_MOVE) {
      delete_selection_data = TRUE;
    }

    Dialog      = gtk_widget_get_toplevel(widget);
    world_entry = GEDA_OBJECT_GET_DATA(Dialog, "world_entry");

    /* Make sure the Drag&Drop Buffer is empty/freed */
    if (object_buffer[DND_BUFFER] != NULL) {
      geda_struct_object_release_objects(object_buffer[DND_BUFFER]);
      object_buffer[DND_BUFFER] = NULL;
    }

    /* Check that we got the format we can use */
    switch (target_type)
    {
      case DND_TARGET_TEXT:
      case DND_TARGET_STRING:
      case DND_TARGET_PLAIN_TEXT:
      case DND_TARGET_UTF8_STRING:

        buffer = (char*)gtk_selection_data_get_text(selection_data);

        if (buffer) {
          /* don't free(buffer) here! leave pointer for our entry to sort */
          object_buffer[DND_BUFFER] = g_list_prepend(object_buffer[DND_BUFFER], buffer);
          i_status_action_start(w_current);
          i_status_set_state (w_current, ENDDNDSTR);
          dnd_success = TRUE;
        }
        break;

      case DND_TARGET_OBJECTS:

        buffer = (char*)gtk_selection_data_get_data(selection_data);

        /* Copy received objects to the Drag&Drop buffer */
        object_buffer[DND_BUFFER] = geda_object_read_buffer (toplevel, NULL,
                                                             buffer, -1,
                                                             _("Drag & Drop"),
                                                             &err);

        /* Check for errors */
        if (err) {

          /* This message is also translated in x_dnd_receive_objects */
          const char *err_rcv = _("An error occurred while receiving Drag & Drop data");
          const char *dat_err = _("Data error");

          char *bold_msg = geda_sprintf ("<b>%s.</b>", dat_err);
          char *err_msg  = geda_sprintf ("%s: %s.", err_rcv, err->message);

          titled_pango_error_dialog (bold_msg, err_msg, _("Drag & Drop failed"));

          GEDA_FREE(bold_msg);
          GEDA_FREE(err_msg);
          g_error_free(err);
          dnd_success = FALSE;
        }
        else {

          if (w_current->inside_action ) {
             switch (w_current->dnd_save_state) {
             case DRAGMOVE:  /* Dragged-Moved something to the coord entry */
             case MOVEMODE:  /* Move Mode Moved something to the coord entry */
               i_status_set_state (w_current, ENDDND_MOVE_OBJ);
               break;
             case COPYMODE:
             case COMPMODE:
               i_status_set_state (w_current, ENDDND_COPY_OBJ);
             default:
               break;
             }
             w_current->dnd_state = w_current->event_state;
          }
          else {
            i_status_action_start(w_current);
            i_status_set_state (w_current, ENDDND_COPY_OBJ);
          }
          dnd_success = TRUE;
        }
        break;

      default:
        printf (" nothing good.\n");
    }
  }

  gtk_drag_finish (context, dnd_success, delete_selection_data, time);

  if (dnd_success == TRUE) {
    gtk_window_present(GTK_WINDOW(w_current->cowindow));
    gtk_widget_grab_focus (world_entry);
  }

#if DEBUG || DEBUG_DND_EVENTS
  printf ("\n");
#endif
}

#if 0
/*!
 * \brief When Drag Motion over the Destination
 * \par Function Description
 *  Called when a drag is over the destination.
 *
 * \return FALSE if the operation should continue
 */
/* Emitted  */
bool x_dialog_coord_drag_motion (GtkWidget      *widget,
                                 GdkDragContext *context, int x, int y,
                                 unsigned int    time,
                                 GschemToplevel *w_current)
{
  /* Fancy stuff here. This signal spams the console something horrible. */
  return  FALSE;
}
#endif

/*!
 * \brief When Drag gets Dropped om the Drawing Area
 * \par Function Description
 *  Called when the user releases (drops) the selection. We choose the
 *  target type we wish the source could send. We call gtk_drag_get_data
 *  which will emit "drag-data-get" on the source, passing along our wish.
 *
 * \return TRUE if the operation should continue, other FALSE
 */
bool x_dialog_coord_drag_drop (GtkWidget      *widget,
                               GdkDragContext *context,
                               int x, int y,
                               unsigned int    time,
                               GschemToplevel *w_current)
{
  GList *targets;
  bool   is_valid_drop_site;

  unsigned int dnd_ntargets = G_N_ELEMENTS (dnd_target_list);

#if DEBUG || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  printf ("%s: x_dialog_coord_drag_drop\n", name);
#endif

  /* Check to see if (x,y) is a valid drop site within widget */
  is_valid_drop_site = TRUE;

  /* Get a list of target types to choose from */
  targets = context->targets;

  /* If the source offers a target */
  if (targets) {

    GtkTargetEntry *target_entry;
    GdkAtom target_type;
    int     index;

    /* Set what we really want! */
    target_type = GDK_POINTER_TO_ATOM (&dnd_target_list[DND_TARGET_OBJECTS]);

    index  = dnd_ntargets - 1;

    /* For each of our targets, look backwards to see if we find a match */
    for (target_entry = &dnd_target_list[index]; index > -1 ; index--) {

      GList *iter;

      target_entry = &dnd_target_list[index];

      for (iter = targets; iter != NULL; iter = g_list_next (iter)) {

        target_type = GDK_POINTER_TO_ATOM(iter->data);

        if (!geda_stricmp (target_entry->target, gdk_atom_name(target_type))) {

#if DEBUG || DEBUG_DND_EVENTS
          printf ("%s, requesting a %s\n", __func__, gdk_atom_name(target_type));
#endif
          index = -1;
          break;
        }
      }
    }

    /* Request the data from the source. */
    gtk_drag_get_data (widget,           /* will receive 'drag-data-received' signal */
                       context,          /* represents the current state of the DnD */
                       target_type,      /* the target type we want */
                       time );           /* time stamp */

#if DEBUG || DEBUG_DND_EVENTS
    printf (" Valid\n");
#endif
  }

  /* No target offered by source => error */
  else {

    is_valid_drop_site = FALSE;

#if DEBUG || DEBUG_DND_EVENTS
    printf (" Invalid\n");
#endif

  }

  return  is_valid_drop_site;
}

/** @} end-subgroup Coordinates-Drag-N-Drop-Destination  */

/*!
 * \brief Response function for the coord dialog
 * \par Function Description
 *  This function destroys the coord dialog box and invalidates the
 *  pointers store in toplevel to the entries.
 */
void x_dialog_coord_dialog_response(GtkWidget      *Dialog, int response,
                                    GschemToplevel *w_current)
{
  gtk_widget_destroy(Dialog);
  w_current->world_entry  = NULL;
  w_current->screen_entry = NULL;
}

/*!
 * \brief Update the coordinates in the coord dialog box.
 * \par Function Description
 *  This function takes the screen coordinates and prints the
 *  screen and the world coordinates in the coord dialog.
 */
void x_dialog_coord_update_display(GschemToplevel *w_current, int sx, int sy, int wx, int wy)
{
  GtkWidget *Dialog;
  GedaEntry *screen_entry;
  GedaEntry *world_entry;

  char *string;

  Dialog = w_current->cowindow;
  screen_entry = GEDA_OBJECT_GET_DATA(Dialog, "screen");
  world_entry  = GEDA_OBJECT_GET_DATA(Dialog, "world");

  string = geda_sprintf("(%d, %d)", sx, sy);
  geda_entry_set_text(screen_entry, string);
  GEDA_FREE(string);

  string = geda_sprintf("(%d, %d)", wx, wy);
  geda_entry_set_text(world_entry, string);
  GEDA_FREE(string);
}

/*!
 * \internal Callback on world_entry "process-entry"
 * \par
 *  Signal handler called when the user presses the "Enter" key to
 *  activate the world coordinates entry on the coodinates dialog.
 *  If there are valid coodinates in the entry, then the pointer
 *  will be positioned to the coodinates if the DND_BUFFER is empty
 *  or the object dropped on the entry will be pasted into to page
 *  at the interpreted coodinates. If what was dropped, was a string,
 *  x_dnd_receive_string is called to handler the insertion.
 */
static void co_on_entry_activate (GedaEntry *entry, GschemDialog *Dialog)
{
  const char *str;

  str = GetEntryText(entry);

  if (str) {

    int x, y;

    if (geda_utility_string_parse_xy(str, &x, &y)) {

      GschemToplevel *w_current;
      GedaToplevel   *toplevel;

      w_current = Dialog->w_current;
      toplevel  = w_current->toplevel;

      /* If we were not in an action then just set the pointer to X,Y location */
      if (!w_current->inside_action) {
        i_window_set_pointer_position (w_current, x, y);
        w_current->first_wx = x;
        w_current->first_wy = y;
      }
      else if (object_buffer[DND_BUFFER] != NULL) {

        /* there is something in the Drag&Drop buffer */

        bool do_delete;
        bool valid;

        do_delete = TRUE;
        valid     = FALSE; /* so we can fall thru default without error msg */

        switch (w_current->event_state) {
          case ENDDNDSTR:
            /* Somebody, not gschem, dropped a string on the world entry */
            x_dnd_receive_string(w_current, x, y,
                                (char *) object_buffer[DND_BUFFER]->data,
                                 DROPPED_ON_COORD);
            GEDA_FREE(object_buffer[DND_BUFFER]->data);
            g_list_free(object_buffer[DND_BUFFER]);
            break;

          case ENDDND_COPY_OBJ:
            /* Object data is being copied, the objects in the buffer represent
             * new objects so set a flag so that we do not delete them! */
            do_delete = FALSE;

            /* fall through */

          case ENDDND_MOVE_OBJ:

            w_current->second_wx = x;
            w_current->second_wy = y;

            if (w_current->dnd_save_state == MOVEMODE ||
                w_current->dnd_save_state == DRAGMOVE)
            {
              o_move_end(w_current);
            }
            else {
              geda_struct_place_set_place_list(toplevel, object_buffer[DND_BUFFER] );
              o_place_end(w_current, 0, 0, 0); /* Not passing a Hook Id */
              o_undo_savestate (w_current, UNDO_ALL);
            }

            valid = TRUE;
            /* keep falling through */

            default:
              if (do_delete) {
                geda_struct_object_release_objects(object_buffer[DND_BUFFER]);
              }
              if (!valid) {
                fprintf(stderr, "Coord Entry freed the Drag&Drop buffer, "
                                "did not know what else to do\n");
              }
        }
      }

      object_buffer[DND_BUFFER] = NULL;
      w_current->dnd_save_state = NONE;
      o_invalidate_all (w_current);
    }
  }
}

/*!
 * \brief Create the Coordinates Dialog
 * \par Function Description
 *  This function creates the coordinates dialog box.
 */
void x_dialog_coord_dialog (GschemToplevel *w_current)
{
  GtkWidget *ThisDialog;

  unsigned int dnd_ntargets = G_N_ELEMENTS (dnd_target_list);

  ThisDialog = w_current->cowindow;

  if (!ThisDialog) {

    GtkWidget *vbox;
    GtkWidget *frame;
    GtkWidget *screen_entry;
    GtkWidget *world_entry;
    GdkColor   bg_color;
    char      *world_name;

    ThisDialog = gschem_dialog_new_with_buttons(_("Coordinates"),
                                     w_current->main_window,
                                     GSCHEM_MODELESS_DIALOG,
                                 IDS_COORDINATES, w_current,
                       GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                                       NULL);

    gtk_window_set_position (GTK_WINDOW (ThisDialog), GTK_WIN_POS_NONE);

    bg_color.red   = 0xEEEE;
    bg_color.green = 0xEBEB;
    bg_color.blue  = 0xE7E7;

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    frame = gtk_frame_new (_("Screen"));
    geda_container_add(vbox, frame);
    gtk_widget_show (frame);

    screen_entry = geda_entry_new_visible ();
    gtk_entry_set_has_frame (GTK_ENTRY(screen_entry), FALSE);
    gtk_entry_set_alignment (GTK_ENTRY(screen_entry), 0.5);
    geda_widget_modify_color (screen_entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
    geda_container_add(frame, screen_entry);

    frame = gtk_frame_new (_("World"));
    geda_container_add(vbox, frame);
    gtk_widget_show (frame);

    world_entry = geda_entry_new_visible ();
    gtk_entry_set_has_frame (GTK_ENTRY(world_entry), FALSE);
    gtk_entry_set_alignment (GTK_ENTRY(world_entry), 0.5);
    geda_widget_modify_color (world_entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
    geda_container_add(frame, world_entry);
    geda_entry_set_valid_input((GedaEntry*)world_entry, ACCEPT_COORDINATE);

    world_name = geda_sprintf("GschemWorldEntry:%i", prog_pid);
    g_object_set (world_entry, "name", world_name, NULL);
    GEDA_FREE(world_name);

    GEDA_HOOKUP_OBJECT ( ThisDialog, screen_entry, "screen");
    GEDA_HOOKUP_OBJECT ( ThisDialog, world_entry,  "world");

    g_signal_connect (world_entry, "process-entry",
                      G_CALLBACK (co_on_entry_activate),
                      ThisDialog);

    g_signal_connect (world_entry, "drag-data-received",
                      G_CALLBACK (x_dialog_coord_dnd_drag_receive),
                      w_current);
/*
    g_signal_connect (world_entry, "drag-motion",
                      G_CALLBACK (x_dialog_coord_drag_motion),
                      w_current);
*/
    g_signal_connect (world_entry, "drag-drop",
                      G_CALLBACK (x_dialog_coord_drag_drop),
                      w_current);

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (x_dialog_coord_dialog_response),
                      w_current);

  /* Make the world_entry a DnD destination. */
   gtk_drag_dest_set (world_entry,               /* Our widget that will accept the drop */
                      GTK_DEST_DEFAULT_MOTION,   /* default actions for dest on DnD */
                      dnd_target_list,           /* lists of targets we support */
                      dnd_ntargets,              /* size of list */
                      GDK_ACTION_COPY);          /* what to do with data after dropped */

    gtk_widget_show(ThisDialog);

    GEDA_HOOKUP_OBJECT(ThisDialog, world_entry,  "world_entry");
    GEDA_HOOKUP_OBJECT(ThisDialog, screen_entry, "screen_entry");

    w_current->world_entry  = world_entry;      /* Save Pointers to widgets Entry */
    w_current->screen_entry = screen_entry;
    w_current->cowindow = ThisDialog;

  }

  else { /* window already creatad  */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }

  /* always update the coords when the dialog is requested */
  x_dialog_coord_update_display(w_current, 0, 0, 0, 0);
}

/** @} end-subgroup Coordinates-Dialog  */
