/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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

#include <ascii.h>
#include <ctype.h>

#include "gschem.h"

#include <geda_dialog_controls.h>
#include <geda_widgets.h>
#include "x_dialog.h"

#include "x_dnd.h"

#define DEBUG_DND_EVENTS 0

/*! \defgroup Coordinates-Dialog ( Coordinates Systemic-Dialogs)
 *  @{ \par This group contains functions and data for Drag-N-Drop events
 */
static GtkTargetEntry dnd_target_list[] = {
    GSCHEM_TARGET_NONE,
    GSCHEM_TARGET_TEXT,
    GSCHEM_TARGET_STRING,
    GSCHEM_TARGET_TEXT_PLAIN,
    GSCHEM_TARGET_UTF8_STRING,
    GSCHEM_TARGET_OBJECTS,
};

/*! \defgroup Coordinates-Drag-N-Drop-Destination Coordinates Dialog Drag-N-Drop
 *  @{ \par
 *          This sub-group contains routines to handle signals receivable by
 *          the Coordinates-Dialog as the destination. The  Coordinates-Dialog
 *          is not a Drag-N-Drop Source.
*/

/*! \brief When Drag Received from the Source
 *
 *  \par Function Description
 *
 * Called when the data has been received from the source. We should check
 * the GtkSelectionData sent by the source, and do something with the data.
 * After handling the data we call complete the operation by calling the
 * function gtk_drag_finish, which will emit the "data-delete" signal if
 * told to.
*/
void
x_dialog_coord_dnd_drag_receive
(GtkWidget *widget, GdkDragContext *context, int x, int y,
        GtkSelectionData *selection_data, guint target_type, guint time,
        GschemToplevel *w_current)
{
  GedaToplevel   *toplevel;
  GtkWidget  *Dialog;
  GtkWidget  *world_entry;
  GError     *err;
  char       *buffer;


  bool dnd_success           = FALSE;
  bool delete_selection_data = FALSE;

#if DEBUG  || DEBUG_DND_EVENTS
  const char *WidgetId;
  WidgetId = gtk_widget_get_name (widget);
  g_print ("%s: x_dialog_coord_dnd_drag_receive:", WidgetId);
#endif

  toplevel = w_current->toplevel;
  err = NULL;

  /* Deal with what we are given from source */
  if((selection_data != NULL) && (gtk_selection_data_get_length(selection_data) >= 0))
  {
    if (gdk_drag_context_get_suggested_action(context) == GDK_ACTION_ASK)
    {
      /* Ask the user to move or copy, then set the context action. */
    }

    if (gdk_drag_context_get_suggested_action(context) == GDK_ACTION_MOVE)
      delete_selection_data = TRUE;

    Dialog      = gtk_widget_get_toplevel(widget);
    world_entry = g_object_get_data(G_OBJECT(Dialog), "world_entry");

    /* Make sure the Drag&Drop Buffer is empty/freed */
    if (object_buffer[DND_BUFFER] != NULL) {
      s_object_release_objects(object_buffer[DND_BUFFER]);
      object_buffer[DND_BUFFER] = NULL;
    }

    /* Check that we got the format we can use */
    switch (target_type)
    {
      case DND_TARGET_TEXT:
      case DND_TARGET_STRING:
      case DND_TARGET_PLAIN_TEXT:
      case DND_TARGET_UTF8_STRING:
        buffer = (char *)gtk_selection_data_get_text(selection_data);
        if (buffer) {
          /* don't free(buffer) here! leave pointer for our entry to sort */
          object_buffer[DND_BUFFER] = g_list_prepend(object_buffer[DND_BUFFER], buffer);
          w_current->inside_action = 1;
          i_set_state (w_current, ENDDNDSTR);
          dnd_success = TRUE;
        }
        break;

      case DND_TARGET_OBJECTS:
        buffer =  (char *)gtk_selection_data_get_data(selection_data);

        /* Copy received objects to the Drag&Drop buffer */
        object_buffer[DND_BUFFER] = o_read_buffer (toplevel,
                                                   object_buffer[DND_BUFFER],
                                                   buffer,
                                                   -1, "Drag&Drop", &err);

        /* Check for errors */
        if (err) {
          char *errmsg = g_strdup_printf ( _("An error occurred while receiving dropped data: %s."), err->message);
          titled_pango_error_dialog ( _("<b>Data error.</b>"), errmsg, _("Drag&Drop failed") );
          GEDA_FREE(errmsg);
          g_error_free(err);
          dnd_success = FALSE;
        }
        else {

          if (w_current->inside_action ) {
             g_print (" Received TARGET_ObjectS data while inside_action. save_state=%d\n", w_current->dnd_save_state);
             switch (w_current->dnd_save_state) {
             case ENDMOVE:  /* Dragged-Moved something to the coord entry */

               i_set_state (w_current, ENDDND_MOVE_OBJ);
               break;
             case STARTCOPY:
             case ENDCOPY:
             case ENDCOMP:

               i_set_state (w_current, ENDDND_COPY_OBJ);
             default:
               break;
             }
             w_current->dnd_state = w_current->event_state;
          }
          else {
            w_current->inside_action = 1;
            i_set_state (w_current, ENDDND_COPY_OBJ);
          }
          dnd_success = TRUE;
        }
        break;

      default:
        g_print (" nothing good.\n");
    }
  }

  gtk_drag_finish (context, dnd_success, delete_selection_data, time);

  if (dnd_success == TRUE) {
    gtk_window_present(GTK_WINDOW(w_current->cowindow));
    gtk_widget_grab_focus (world_entry);
  }

#if DEBUG  || DEBUG_DND_EVENTS
  g_print ("\n");
#endif
}

/*! \brief When Drag Motion over the Destination
 *
 *  \par Function Description
 *
 *  Called when a drag is over the destination.
 *
 * \return FALSE if the operation should continue
 */
/* Emitted  */
bool x_dialog_coord_drag_motion
(GtkWidget *widget, GdkDragContext *context, int x, int y, guint t, GschemToplevel *w_current)
{
  // Fancy stuff here. This signal spams the console something horrible.
  //const char *name = gtk_widget_get_name (widget);
  //g_print ("%s: drag_motion_handl\n", name);
  return  FALSE;
}


/*! \brief When Drag gets Dropped om the Drawing Area
 *
 *  \par Function Description
 *
 * Called when the user releases (drops) the selection. We choose the
 * target type we wish the source could send. We call gtk_drag_get_data
 * which will emit "drag-data-get" on the source, passing along our wish.
 *
 * \return TRUE if the operation should continue, other FALSE
 */
bool x_dialog_coord_drag_drop
(GtkWidget *widget, GdkDragContext *context, int x, int y, guint time, GschemToplevel *w_current)
{
  bool            is_valid_drop_site;
  GtkTargetEntry *target_entry;
  GdkAtom         target_type;
  GList          *targets;
  GList          *iter;
  int             index;
  unsigned int    dnd_ntargets = G_N_ELEMENTS (dnd_target_list);

#if DEBUG || DEBUG_DND_EVENTS
  const char *name = gtk_widget_get_name (widget);
  g_print ("%s: x_dialog_coord_drag_drop\n", name);
#endif

  /* Check to see if (x,y) is a valid drop site within widget */
  is_valid_drop_site = TRUE;
  targets = NULL;

  /* If the source offers a target */
  if (gdk_drag_context_list_targets (context)) {
    /* Get a list of target types to choose from */
    targets = gdk_drag_context_list_targets(context);

    /* Set what we really want! */
    target_type = GDK_POINTER_TO_ATOM (&dnd_target_list[DND_TARGET_OBJECTS]);

    index  = dnd_ntargets - 1;
    /* For each of our targets, look backwards to see if we find a match */
    for (target_entry = &dnd_target_list[index]; index > -1 ; index--) {
      target_entry = &dnd_target_list[index];
      for (iter = targets; iter != NULL; iter = g_list_next (iter)) {
        target_type = GDK_POINTER_TO_ATOM(iter->data);
        if (!strcasecmp (target_entry->target, gdk_atom_name(target_type))) {
#if DEBUG || DEBUG_DND_EVENTS
          g_print ("x_dialog_coord_drag_drop, requesting a %s\n", gdk_atom_name(target_type));
#endif
          index = -1;
          break;
        }
      }
    }

    /* Request the data from the source. */
    gtk_drag_get_data ( widget,          /* will receive 'drag-data-received' signal */
                        context,         /* represents the current state of the DnD */
                        target_type,     /* the target type we want */
                        time );          /* time stamp */

#if DEBUG || DEBUG_DND_EVENTS
    g_print (" Valid\n");
#endif
  }

  /* No target offered by source => error */
  else {
    is_valid_drop_site = FALSE;

#if DEBUG || DEBUG_DND_EVENTS
    g_print (" Invalid\n");
#endif

  }

  return  is_valid_drop_site;
}

/*! @} end-subgroup Coordinates-Drag-N-Drop-Destination  */

/*! \brief Response function for the coord dialog
 *
 *  \par Function Description
 *
 *  This function destroys the coord dialog box and does some cleanup.
 */
void
x_dialog_coord_dialog_response(GtkWidget *Dialog, int response, GschemToplevel *w_current)
{
  gtk_widget_destroy(Dialog);
  w_current->cowindow = NULL;
  w_current->world_entry = NULL;
  w_current->screen_entry = NULL;
}

/*! \brief Update the coordinates in the coord dialog box.
 *  \par Function Description
 *  This function takes the screen coordinates and prints the
 *  screen and the world coordinates in the coord dialog.
 */
void x_dialog_coord_update_display(GschemToplevel *w_current, int x, int y)
{
  GtkWidget *Dialog;
  GtkEntry  *screen_entry;
  GtkEntry  *world_entry;

  char *string;
  int world_x, world_y;

  Dialog = w_current->cowindow;
  screen_entry = gtk_object_get_data(GTK_OBJECT(Dialog), "screen");
  world_entry  = gtk_object_get_data(GTK_OBJECT(Dialog), "world");

  string = g_strdup_printf("(%d, %d)", x, y);
  SetEntryText(screen_entry, string );
  GEDA_FREE(string);

  SCREENtoWORLD (w_current, x, y, &world_x, &world_y);
  world_x = snap_grid (w_current, world_x);
  world_y = snap_grid (w_current, world_y);

  string = g_strdup_printf("(%d, %d)", world_x, world_y);
  SetEntryText(world_entry, string );
  GEDA_FREE(string);
}

static void co_on_entry_activate (GedaEntry *entry, GschemDialog *Dialog)
{
  GschemToplevel *w_current;
  GedaToplevel       *toplevel;

  const char *str;
  char  buffer[36];
  char *x_str, *y_str;
  int   icomma, x, y;
  int   index;
  bool  do_delete;
  bool  valid;

  icomma = -1;
  valid  = FALSE;
  x_str  = NULL;
  y_str  = NULL;
  str    = NULL;
  str    = GetEntryText(entry);


  if (str) {
    /* extract entry text and determine world X & Y integers */
    strcpy(&buffer[0], str);
    for (index =0; index < 36; index++) {

      if (!buffer[index])
        break;

      if ( isdigit(buffer[index])) {
        if (!x_str) {
          x_str = &buffer[index];
        }
        else if (!y_str && icomma > 0) {
          y_str = &buffer[index];
        }
      }
      else if ( buffer[index] == ASCII_COMMA) {
        icomma = index;
      }
      else if ( buffer[index] == ASCII_LEFT_PARENTHESIS ||
                buffer[index] == ASCII_RIGHT_PARENTHESIS ) {
        buffer[index] = ASCII_SPACE;
      }
    }
    if ( x_str && y_str) {
      if ( icomma > 0)
        buffer[icomma] = '\0';
      x = atoi(x_str);
      y = atoi(y_str);
      valid = TRUE;
    }

    if (valid) {

      w_current = Dialog->w_current;
      toplevel = w_current->toplevel;

#if DEBUG || DEBUG_DND_EVENTS
g_print ("begin: <co_on_entry_activate> inside_action=%d, event_state=%d, dnd_save_state=%d\n", w_current->inside_action, w_current->event_state, w_current->dnd_save_state );
#endif
      /* If we were not in an action the just set the pointer to X,Y location */
      if (!w_current->inside_action) {
        x_event_set_pointer_position (w_current, x, y);
      } /* is there something in the Drag&Drop buffer? */
      else if (object_buffer[DND_BUFFER] != NULL) {

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
              if (w_current->dnd_save_state == ENDMOVE) {
                w_current->second_wx = x;
                w_current->second_wy = y;
                o_move_end(w_current);
              }
              else {
                s_place_set_place_list(toplevel, object_buffer[DND_BUFFER] );
                o_place_end(w_current, x, y, 0, 0, NULL);
              }

              valid = TRUE;
              /* keep falling through */

              default:
                if (do_delete) {
                  s_object_release_objects(object_buffer[DND_BUFFER]);
                }
                if (!valid)
                  fprintf(stderr, "Coord Entry freed the Drag&Drop buffer, "
                                  "did not know what else to do\n");
        }
      }

      object_buffer[DND_BUFFER] = NULL;
      w_current->dnd_save_state = NONE;
      o_invalidate_all (w_current);
      i_set_state (w_current, SELECT);
    }
  }
}

/*! \brief Create the Coordinates Dialog
 *
 *  \par Function Description
 *
 *  This function creates the coord dialog box.
 */
void x_dialog_coord_dialog (GschemToplevel *w_current, int x, int y)
{
  GtkWidget *ThisDialog;
  GtkWidget *vbox;
  GtkWidget *frame;
  GtkWidget *screen_entry;
  GtkWidget *world_entry;
  GdkColor   bg_color;
  char      *world_name;

  unsigned int dnd_ntargets = G_N_ELEMENTS (dnd_target_list);

  ThisDialog = w_current->cowindow;

  if (!ThisDialog) {

    ThisDialog = gschem_dialog_new_with_buttons(_("Coords"),
                         GTK_WINDOW(w_current->main_window),
                         GSCHEM_MODELESS_DIALOG,
                                 IDS_COORDINATES, w_current,
                       GTK_STOCK_CLOSE, GTK_RESPONSE_REJECT,
                                                       NULL);

    gtk_window_position (GTK_WINDOW (ThisDialog), GTK_WIN_POS_NONE);

    gtk_container_border_width (GTK_CONTAINER(ThisDialog),
                                DIALOG_BORDER_SPACING);

    bg_color.red   = 0xEEEE;
    bg_color.green = 0xEBEB;
    bg_color.blue  = 0xE7E7;

    vbox = GTK_DIALOG(ThisDialog)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    frame = gtk_frame_new (_("Screen"));
    gtk_container_add(GTK_CONTAINER (vbox), frame);
    g_object_set (frame, "visible", TRUE, NULL);

    screen_entry = geda_visible_entry_new ( DISABLE, DISABLE);
    gtk_entry_set_has_frame (GTK_ENTRY(screen_entry), FALSE);
    gtk_entry_set_alignment (GTK_ENTRY(screen_entry), 0.5);
    geda_entry_widget_modify_color (screen_entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
    gtk_container_add(GTK_CONTAINER (frame), screen_entry);

    frame = gtk_frame_new (_("World"));
    gtk_container_add(GTK_CONTAINER (vbox), frame);
    g_object_set (frame, "visible", TRUE, NULL);

    world_entry = geda_visible_entry_new ( DISABLE, DISABLE);
    gtk_entry_set_has_frame (GTK_ENTRY(world_entry), FALSE);
    gtk_entry_set_alignment (GTK_ENTRY(world_entry), 0.5);
    geda_entry_widget_modify_color (world_entry, GTK_RC_BASE, GTK_STATE_NORMAL, &bg_color);
    gtk_container_add(GTK_CONTAINER (frame), world_entry);
    geda_entry_set_valid_input((GedaEntry*)world_entry, ACCEPT_COORDINATE);

    world_name = g_strdup_printf("GschemWorldEntry:%i", prog_pid);
    g_object_set (world_entry, "name", world_name, NULL);
    GEDA_FREE(world_name);

    GSCHEM_HOOKUP_OBJECT ( ThisDialog, screen_entry, "screen");
    GSCHEM_HOOKUP_OBJECT ( ThisDialog, world_entry,  "world");

    g_signal_connect (world_entry, "process-entry",
                      G_CALLBACK (co_on_entry_activate),
                      ThisDialog);

    g_signal_connect (world_entry, "drag-data-received",
                      G_CALLBACK (x_dialog_coord_dnd_drag_receive),
                      w_current);

    g_signal_connect (world_entry, "drag-motion",
                      G_CALLBACK (x_dialog_coord_drag_motion),
                      w_current);

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

    GSCHEM_HOOKUP_OBJECT(ThisDialog, world_entry,  "world_entry");
    GSCHEM_HOOKUP_OBJECT(ThisDialog, screen_entry, "screen_entry");

    w_current->world_entry  = world_entry;      /* Save Pointers to widgets Entry */
    w_current->screen_entry = screen_entry;
    w_current->cowindow = ThisDialog;

  }

  else { /* window already creatad  */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }

  /* always update the coords when the dialog is requested */
  x_dialog_coord_update_display(w_current, x, y);
}
/*! @} end-subgroup Coordinates-Dialog  */
/***************** End of coord dialog box **************************/
