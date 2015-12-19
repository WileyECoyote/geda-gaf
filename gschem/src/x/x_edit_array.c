/* -*- C indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_edit_array.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: March 05, 2015
 */
/*!
 * \file x_edit_array.c
 * \brief A dialog box for creating arrays.
 */

#include <gschem.h>
#include <x_dialog.h>
#include <x_window.h>

#include <geda_dialog_controls.h>
#include <geda_widgets.h>

/** \defgroup Array-Dialog Create Array Dialog
 *  @{
 *  \ingroup (Editing-Dialogs)
 *  \image html array_dialog.png
 *  \image latex array_dialog.png
 *  @} endgroup Array-Dialogs
 *
 *  \defgroup Create-Array-Dialog Create Array Dialog Module
 *  @{
 *  \ingroup (Array-Dialog)
 */

static void x_dialog_array_edit_disconnect_events (GschemToplevel *w_current,
                                                   array_data     *dialog_data)
{
  if (dialog_data->press_hid) {
    g_signal_handler_disconnect (DrawingArea, dialog_data->press_hid);
    dialog_data->press_hid = 0;
  }
  if (dialog_data->release_hid) {
    g_signal_handler_disconnect (DrawingArea, dialog_data->release_hid);
    dialog_data->release_hid = 0;
  }
  i_status_action_stop(w_current);
}

static void x_dialog_array_edit_enable_events(GschemToplevel *w_current,
                                              array_data     *dialog_data)
{
  i_event_block_buttons (w_current);

  x_dialog_array_edit_disconnect_events(w_current, dialog_data);

  dialog_data->press_hid   = g_signal_connect (DrawingArea, "button_press_event",
                                               G_CALLBACK(dialog_data->press_butt),
                                               w_current);
  dialog_data->release_hid = g_signal_connect (DrawingArea, "button_release_event",
                                               G_CALLBACK(dialog_data->release_butt),
                                               w_current);
}

static void x_dialog_array_edit_disable_events(GschemToplevel *w_current,
                                               array_data     *dialog_data)
{
  x_dialog_array_edit_disconnect_events(w_current, dialog_data);
  i_event_unblock_buttons (w_current);
}

/*! \brief Handle selection change event for x_dialog_array_edit
 *  \par Function Description
 *  Called when the selection changes. The functions call
 *  x_dialog_ep_component_change to update the data fields
 *
 *  \param w_current pointer to GschemToplevel context
 *  \param object    pointer to a selected Object.
 */
static void x_dialog_ca_update_selection (GschemToplevel *w_current,
                                          Object         *object)
{
  GtkWidget    *dialog;
  array_data   *dialog_data;

  /* Get ptr to the data structure */
  dialog      = w_current->cawindow;
  dialog_data = GEDA_OBJECT_GET_DATA (dialog, IDS_ARRAY_EDIT);

  if (o_select_is_selection(w_current)) {

    GschemDialog *dog;
    char  s_val[10];
    char *str;
    int   left, right, top, bottom;
    int   count;

    dog   = (GschemDialog*)dialog;

    count = g_list_length(dog->selection->glist);

    str = u_string_int2str(count, s_val, 10);
    gtk_entry_set_text ((GtkEntry*)dialog_data->cnt_sel_entry, str);

    if (o_get_bounds_list (Current_Selection->glist, &left, &top, &right, &bottom))
    {
      int dx, dy;

      dx = right - left;
      dy = bottom - top;
      str = u_string_int2str(dx, s_val, 10);
      gtk_entry_set_text ((GtkEntry*)dialog_data->x_size_entry, str);
      str = u_string_int2str(dy, s_val, 10);
      gtk_entry_set_text ((GtkEntry*)dialog_data->y_size_entry, str);
      /* Save the bottom left corner to data structure */
      dialog_data->wx = left;
      dialog_data->wy = bottom > top ? top : bottom;
    }
  }
}

static void post_response_row (GschemToplevel *w_current)
{
  GtkWidget  *dialog;
  array_data *dialog_data;

  /* Get ptr to the data structure */
  dialog      = w_current->cawindow;
  dialog_data = GEDA_OBJECT_GET_DATA (dialog, IDS_ARRAY_EDIT);

  /* if user supplied two points */
  if (w_current->second_wx != -0) {

    char s_val[6]; /* For integer 2 string conversion */

    int diff_y = abs(w_current->second_wy - w_current->first_wy);

    char *y_str = u_string_int2str(diff_y, s_val, 10);

    SetEntryText(dialog_data->row_off_entry, y_str);
  }
  dialog_data->post_responder = NULL;
}

static void post_response_row_col (GschemToplevel *w_current)
{
  GtkWidget  *dialog;
  array_data *dialog_data;


  /* Get ptr to the data structure */
  dialog      = w_current->cawindow;
  dialog_data = GEDA_OBJECT_GET_DATA (dialog, IDS_ARRAY_EDIT);

  /* if user supplied two points */
  if (w_current->second_wx != -0) {

    char s_val[6]; /* For integer 2 string conversion */

    int diff_x = abs(w_current->second_wx - w_current->first_wx);
    int diff_y = abs(w_current->second_wy - w_current->first_wy);

    char *x_str = u_string_int2str(diff_x, s_val, 10);

    SetEntryText(dialog_data->col_off_entry, x_str);

    char *y_str = u_string_int2str(diff_y, s_val, 10);

    SetEntryText(dialog_data->row_off_entry, y_str);
  }

  dialog_data->post_responder = NULL;
}

static void post_response_col (GschemToplevel *w_current)
{
  GtkWidget  *dialog;
  array_data *dialog_data;


  /* Get ptr to the data structure */
  dialog      = w_current->cawindow;
  dialog_data = GEDA_OBJECT_GET_DATA (dialog, IDS_ARRAY_EDIT);

  /* if user supplied two points */
  if (w_current->second_wx != -0) {

    int   diff_x;
    char *x_str;
    char  s_val[6];

    diff_x = abs(w_current->second_wx - w_current->first_wx);
    x_str  = u_string_int2str(diff_x, s_val, 10);

    SetEntryText(dialog_data->col_off_entry, x_str);
  }

  dialog_data->post_responder = NULL;
}

/* ----------------------- Button Event Handlers ----------------------- */

static
int x_dialog_array_edit_butt_pressed_dist(GtkWidget      *widget,
                                          GdkEventButton *event,
                                          GschemToplevel *w_current)
{
  if (event->button == 1) {

    int  x, y;
    int  w_x, w_y;

    SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

    w_x = snap_grid (w_current, x);
    w_y = snap_grid (w_current, y);

    if (w_current->inside_action) {
      w_current->second_wx = w_x;
      w_current->second_wy = w_y;
    }
    else {

      i_status_action_start(w_current);

      w_current->first_wx = w_current->second_wx = w_x;
      w_current->first_wy = w_current->second_wy = w_y;

      w_current->rubber_visible = TRUE;
    }
  }

  return(0);
}

static
int x_dialog_array_edit_butt_released_dist(GtkWidget      *widget,
                                           GdkEventButton *event,
                                           GschemToplevel *w_current)
{
  GtkWidget    *dialog;
  array_data   *dialog_data;

  /* Get ptr to the data structure */
  dialog      = w_current->cawindow;
  dialog_data = GEDA_OBJECT_GET_DATA (dialog, IDS_ARRAY_EDIT);

  if (w_current->second_wx != -0) {

    bool valid = FALSE;

    if (dialog_data->post_responder == post_response_row) {
      valid = (w_current->second_wy != w_current->first_wy);
    }
    else if (dialog_data->post_responder == post_response_row_col) {
      valid = (w_current->second_wx != w_current->first_wx) &&
              (w_current->second_wy != w_current->first_wy);
    }
    else { /* Must be post_responder == post_response_col*/
      valid = (w_current->second_wx != w_current->first_wx);
    }

    if (valid) {
      x_dialog_array_edit_disable_events(w_current, dialog_data);

      gtk_window_present (GTK_WINDOW (dialog));
      dialog_data->post_responder(w_current);
    }
  }
  else if (event->button == 3) {
    x_dialog_array_edit_disable_events(w_current, dialog_data);
    gtk_window_present (GTK_WINDOW (dialog));
  }
  return(0);
}

static
int x_dialog_array_edit_butt_pressed_select(GtkWidget      *widget,
                                            GdkEventButton *event,
                                            GschemToplevel *w_current)
{
  if (event->button == 1) {

    int  x, y;

    if (w_current->event_state == SELECT) {

      i_status_action_start(w_current);

      SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

      w_current->first_wx      = w_current->second_wx = x;
      w_current->first_wy      = w_current->second_wy = y;
      w_current->event_state   = STARTSELECT;
    }
  }
  return(0);
}

static
int x_dialog_array_edit_butt_released_select(GtkWidget      *widget,
                                             GdkEventButton *event,
                                             GschemToplevel *w_current)
{
  GtkWidget    *dialog;
  array_data   *dialog_data;

  /* Get ptr to the data structure */
  dialog      = w_current->cawindow;
  dialog_data = GEDA_OBJECT_GET_DATA (dialog, IDS_ARRAY_EDIT);

  if (event->button == 1) {

    int  x, y;

    SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

    if (w_current->event_state == STARTSELECT) {
      o_select_end(w_current, x, y);
      i_status_set_state (w_current, SELECT);
    }
    else if (w_current->event_state == SBOX) {
      o_select_box_end(w_current, x, y);
    }
  }
  else if (event->button == 3) {
    x_dialog_array_edit_disable_events(w_current, dialog_data);
    gtk_window_present (GTK_WINDOW (dialog));
  }
  return(0);
}

static
int x_dialog_array_edit_butt_pressed_deselect(GtkWidget      *widget,
                                              GdkEventButton *event,
                                              GschemToplevel *w_current)
{
  if (event->button == 1) {

    int  x, y;

    if (w_current->event_state == DESELECT) {

      i_status_action_start(w_current);

      SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

      w_current->first_wx      = w_current->second_wx = x;
      w_current->first_wy      = w_current->second_wy = y;
      w_current->event_state   = STARTDESELECT;
    }
  }

  return(0);
}

int x_dialog_array_edit_butt_released_deselect(GtkWidget      *widget,
                                               GdkEventButton *event,
                                               GschemToplevel *w_current)
{
  GtkWidget  *dialog;
  array_data *dialog_data;

  /* Get ptr to the data structure */
  dialog      = w_current->cawindow;
  dialog_data = GEDA_OBJECT_GET_DATA (dialog, IDS_ARRAY_EDIT);

  if (event->button == 1) {

    int  x, y;

    SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

    if (w_current->event_state == STARTDESELECT) {
      Object *object = o_find_selected_object(w_current, x, y);

      if (object) {
        if (!w_current->CONTROLKEY) {
          o_select_object(w_current, object, SINGLE, 1);
        }
      }
    }
    else if (w_current->event_state == SBOX) {
      o_select_box_end(w_current, x, y);
    }
  }
  else if (event->button == 3) {
    x_dialog_array_edit_disable_events(w_current, dialog_data);
    gtk_window_present (GTK_WINDOW (dialog));
  }
  return(0);
}

static int
create_array (GtkWidget *dialog, int columns, int rows, int x_pitch, int y_pitch)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(dialog)->w_current;
  array_data     *dialog_data;
  GList          *object_list;

  dialog_data = GEDA_OBJECT_GET_DATA (dialog, IDS_ARRAY_EDIT);
  object_list = gschem_dialog_get_selected(GSCHEM_DIALOG(dialog));

  bool changed;

  if (object_list) {

    int j;
    int left   = w_current->first_wx = dialog_data->wx;
    int bottom = w_current->first_wy = dialog_data->wy;

    s_place_set_place_list(w_current->toplevel, object_list);

    i_status_action_start(w_current);

    for (j = 0; j < columns; j++) {

      int k, x = left + j * x_pitch;

      for (k = 0; k < rows; k++) {

        if (j || k) {

          int y = bottom + k * y_pitch;

          w_current->second_wx = x;
          w_current->second_wy = y;
          o_place_end (w_current, TRUE, NULL, 0);
        }
      }
    }

    i_status_action_stop(w_current);

    g_hook_run_object_list (w_current, COPY_OBJECTS_HOOK, object_list);

    changed = TRUE;
  }
  else {
    changed = FALSE;
  }
  return changed;
}

/*! \brief Component Properties Dialog Apply Settings
 *  \par Function Description
 *  This function applies the settings of the properties dialog to the
 *  selected objects or to the symbol page depending on whether object
 *  has a value. The function handles the case of exchanging the symbol
 *  and calls helper functions for attribute handling.
 *
 *  \param [in] dialog      Pointer to a Component Dialog instance.
 *  \param [in] dialog_data Pointer to a Component Dialog data structure
 *
 */
static void x_dialog_array_edit_ok(GtkWidget  *dialog,
                                   array_data *dialog_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(dialog)->w_current;

  bool changed  = FALSE;

  int col_count =  GET_SPIN_IVALUE(dialog_data->col_spin);
  int row_count =  GET_SPIN_IVALUE(dialog_data->row_spin);

  if (col_count && row_count) {

    if (col_count > 1 || row_count > 1) {

      const char *x_msg = _("X pitch distence is less than extents\n");
      const char *y_msg = _("Y pitch distence is less than extents\n");



      int x_pitch = atoi(GetEntryText( dialog_data->col_off_entry ));
      int y_pitch = atoi(GetEntryText( dialog_data->row_off_entry ));

      if (x_pitch || y_pitch) {

        const char *mess = NULL;

        int x_size  = atoi(GetEntryText( dialog_data->x_size_entry ));
        int y_size  = atoi(GetEntryText( dialog_data->y_size_entry ));

        if (x_pitch && y_pitch ) {             /* Both True */

          int abs_x_pitch = abs(x_pitch);
          int abs_y_pitch = abs(y_pitch);

          if ((abs_x_pitch < x_size) && (abs_y_pitch < y_size)) {
            mess = _("X and Y pitch distence are less than extents\n");
          }
          else if (abs_x_pitch < x_size) {
            mess = x_msg;
          }
          else if (abs_y_pitch < y_size) {
            mess = y_msg;
          }
        }
        else if (abs(x_pitch)) {               /* Only X is True */
          if (x_pitch < x_size) {
            mess = x_msg;
          }
        }
        else if (abs(y_pitch) < y_size) {      /* Only Y is True */
          mess = y_msg;
        }

        if (mess) {
          const char *question = _("Components may overlay, Continue?");
          char *msg2 = u_string_concat (mess, question, NULL);
          int response = x_dialog_confirmation(msg2, GEDA_MESSAGE_WARNING, FALSE);
          GEDA_FREE(msg2);
          if (response == GEDA_RESPONSE_YES) {
            changed = create_array (dialog, col_count, row_count, x_pitch, y_pitch);
          }
        }
        else {
          changed = create_array (dialog, col_count, row_count, x_pitch, y_pitch);
        }
      }
      else {
        titled_information_dialog(_("Create Array:Error"), "%s",
                                  _("Both row and column offset\ncan not be zero"));
      }
    }
    else {
      titled_information_dialog(_("Create Array:Error"), "%s",
                                _("Either row or column count\n must be greater than 1"));
    }
  }
  else {
    titled_information_dialog(_("Create Array:Error"), "%s",
                              _("Both row and column\ncan not be zero"));
  }
  if (changed) {
    o_undo_savestate(w_current, UNDO_ALL);
  }
}

/*! \brief Response Function for the Component Properties Dialog
 *  \par Function Description
 *  This function handles the response to the Component Properties dialog.
 *  Either the is called to retrieve and process data in the dialog or
 *  the dialog session is terminated.
 */
static void x_dialog_array_edit_response(GtkWidget  *dialog,
                                         int         response,
                                         array_data *dialog_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(dialog)->w_current;

  switch (response) {
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_grab_remove (dialog);
    gtk_widget_destroy (dialog);
    w_current->cawindow = NULL;           /* remove after adding*/
    GEDA_FREE (dialog_data);
    i_status_set_state (w_current, SELECT);
    break;
  case GEDA_RESPONSE_ACCEPT:
    x_dialog_array_edit_ok(dialog, dialog_data);
    s_place_free_place_list(w_current->toplevel);
    i_status_set_state (w_current, SELECT);
    break;
  case GEDA_RESPONSE_SELECT:
    x_dialog_array_edit_enable_events(w_current, dialog_data);
    gtk_widget_hide (GTK_WIDGET (dialog));
    break;
  case GEDA_RESPONSE_GET_DIST:
    w_current->first_wx = w_current->second_wx = -0;
    w_current->first_wy = w_current->second_wy = -0;
    x_dialog_array_edit_enable_events(w_current, dialog_data);
    gtk_widget_hide (GTK_WIDGET (dialog));
    break;
  default:
    BUG_IMSG ("unhandled case for signal <%d>", response);
  }

}

/*! \brief Emit GEDA_RESPONSE_CLOSE signal.
 *  \par Function Description
 *  This function is called when the Close button on the Component
 *  Select dialog is pressed.
 */
static void on_close_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data), "response",
                           GEDA_RESPONSE_REJECT,
                           user_data);
}

/*! \brief Emit COMPSELECT_RESPONSE_HIDE signal.
 *  \par Function Description
 *  This function is called when the Okay button on the Component
 *  Select dialog is pressed.
 */
static void on_apply_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data), "response",
                           GEDA_RESPONSE_ACCEPT,
                           user_data);
}

/*! \brief Emit COMPSELECT_RESPONSE_HIDE signal.
 *  \par Function Description
 *  This function is called when the Okay button on the Component
 *  Select dialog is pressed.
 */
static void on_select_butt_clicked(GtkButton *button, void *user_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(user_data)->w_current;
  array_data     *dialog_data;

  dialog_data = GEDA_OBJECT_GET_DATA (user_data, IDS_ARRAY_EDIT);

  dialog_data->press_butt   = x_dialog_array_edit_butt_pressed_select;
  dialog_data->release_butt = x_dialog_array_edit_butt_released_select;
  w_current->event_state    = SELECT;

  g_signal_emit_by_name (GTK_DIALOG (user_data), "response",
                         GEDA_RESPONSE_SELECT,
                         dialog_data);
}

static void on_deselect_butt_clicked(GtkButton *button, void *user_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(user_data)->w_current;
  array_data     *dialog_data;

  dialog_data = GEDA_OBJECT_GET_DATA (user_data, IDS_ARRAY_EDIT);

  dialog_data->press_butt   = x_dialog_array_edit_butt_pressed_deselect;
  dialog_data->release_butt = x_dialog_array_edit_butt_released_deselect;
  w_current->event_state    = DESELECT;

  g_signal_emit_by_name (GTK_DIALOG (user_data), "response",
                         GEDA_RESPONSE_SELECT,
                         dialog_data);
}

static void x_dialog_array_edit_emit_get_dist (GschemToplevel *w_current,
                                               array_data     *dialog_data)
{
  dialog_data->press_butt     = x_dialog_array_edit_butt_pressed_dist;
  dialog_data->release_butt   = x_dialog_array_edit_butt_released_dist;

  g_signal_emit_by_name (GTK_DIALOG (w_current->cawindow), "response",
                         GEDA_RESPONSE_GET_DIST,
                         dialog_data);
}

static void on_row_butt_clicked(GtkButton *button, void *user_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(user_data)->w_current;
  array_data     *dialog_data;

  dialog_data = GEDA_OBJECT_GET_DATA (user_data, IDS_ARRAY_EDIT);

  dialog_data->post_responder = post_response_row;

  w_current->event_state      = LINEMODE;

  x_dialog_array_edit_emit_get_dist(w_current, dialog_data);

}

static void on_row_col_butt_clicked(GtkButton *button, void *user_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(user_data)->w_current;
  array_data     *dialog_data;

  dialog_data = GEDA_OBJECT_GET_DATA (user_data, IDS_ARRAY_EDIT);

  dialog_data->post_responder = post_response_row_col;

  w_current->event_state      = BOXMODE;

  x_dialog_array_edit_emit_get_dist(w_current, dialog_data);
}

static void on_col_butt_clicked(GtkButton *button, void *user_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(user_data)->w_current;
  array_data     *dialog_data;

  dialog_data = GEDA_OBJECT_GET_DATA (user_data, IDS_ARRAY_EDIT);

  dialog_data->post_responder = post_response_col;

  w_current->event_state      = LINEMODE;

  x_dialog_array_edit_emit_get_dist(w_current, dialog_data);
}

static void x_dialog_array_edit_action_area (GtkWidget  *ThisDialog,
                                             array_data *dialog_data)
{
  GtkWidget *action_area;
  GtkWidget *alignment;
  GtkWidget *action_hbox  = NULL;
  GtkWidget *butt_hbox    = NULL;
  GtkDialog *Dialog;

  Dialog = (GtkDialog*)ThisDialog;

  /* Remove Gtk action area from the dialog and don't re-use it */
  action_area = Dialog->action_area;
  gtk_container_remove(GTK_CONTAINER(Dialog->vbox),GTK_WIDGET(action_area));

  action_hbox = gtk_hbox_new(FALSE, 0);
  g_object_set (action_hbox, "visible", TRUE, NULL);
  gtk_box_pack_end (GTK_BOX (Dialog->vbox), action_hbox, FALSE, FALSE, 0);

  /* Replace the action_area with the new container */
  Dialog->action_area = action_hbox;

  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        "right-padding", 0,
                                        "left-padding",  50,
                                        "xscale",        1.0,
                                        "yscale",        0.0,
                                        "xalign",        1.0,
                                        "yalign",        0.5,
                                        NULL));

  g_object_set (alignment, "visible", TRUE, NULL);
  gtk_box_pack_end (GTK_BOX (action_hbox), alignment, TRUE, TRUE, 0);

  /* Create a Horizontal Box for the buttons to go into */
  butt_hbox = gtk_hbox_new(FALSE, 0);
  g_object_set (butt_hbox, "visible", TRUE, NULL);
  gtk_container_add (GTK_CONTAINER (alignment), butt_hbox);

  /* Create and connect the Close and Apply Buttons */
  GtkWidget *close_butt = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
  GtkWidget *apply_butt = gtk_button_new_from_stock (GTK_STOCK_APPLY);

  g_signal_connect (close_butt, "clicked",
                    G_CALLBACK (on_close_butt_clicked),
                    ThisDialog);

  g_signal_connect (apply_butt, "clicked",
                    G_CALLBACK (on_apply_butt_clicked),
                    ThisDialog);

  g_object_set (apply_butt, "visible", TRUE, "can-default", TRUE, NULL);
  g_object_set (close_butt, "visible", TRUE, NULL);

  gtk_box_pack_end (GTK_BOX (butt_hbox), apply_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);
  gtk_box_pack_end (GTK_BOX (butt_hbox), close_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_REJECT,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog), GEDA_RESPONSE_ACCEPT);
  gtk_widget_grab_default (apply_butt);
}

array_data *x_dialog_array_new_data_structure (GschemToplevel *w_current)
{
  array_data *dialog_data;

  dialog_data = (array_data*) GEDA_MEM_ALLOC (sizeof (struct st_array_data));

  dialog_data->press_hid   = 0;
  dialog_data->release_hid = 0;

  dialog_data->wx = 0;
  dialog_data->wy = 0;

  return dialog_data;
}

/*! \brief Component Properties Dialog Constructor
 *  \par Function Description
 *  Called to construct the Componenent Properties dialog box.
 *
 *  \param w_current pointer to GschemToplevel data structure
 *
 *  \return GtkDialog pointer to a new Componenent Properties Dialog
 */
static
GtkWidget* x_dialog_array_edit_constructor (GschemToplevel *w_current)
{
  AtkObject *atk_obj;
  GtkWidget *Dialog;
  GtkWidget *alignment;
  GtkWidget *frame;
  GtkWidget *hbox;
  GtkWidget *table;
  GtkWidget *vbox;
  GtkWidget *widget;
  GtkWidget *vbox2;
  GtkWidget *preview_area;

  GtkWidget *selection_label;
  GtkWidget *cnt_sel_label;
  GtkWidget *x_size_label;
  GtkWidget *y_size_label;

  GtkWidget *array_label;
  GtkWidget *row_label;
  GtkWidget *row_cnt_label;
  GtkWidget *row_off_label;
  GtkWidget *col_label;
  GtkWidget *col_cnt_label;
  GtkWidget *col_off_label;

  GtkWidget *select_butt;
  GtkWidget *deselect_butt;
  GtkWidget *row_butt;
  GtkWidget *col_butt;
  GtkWidget *row_col_butt;

  array_data *dialog_data;

  const char *cnt_sel_tip    = "Indicates total number of objects currently selected";
  const char *x_size_tip     = "x_size_tip";
  const char *y_size_tip     = "y_size_tip";
  const char *row_cnt_tip    = "Number of rows to create";
  const char *col_cnt_tip    = "Number of columns to create";
  const char *row_off_tip    = "Vertical pitch of array, negative is downward";
  const char *col_off_tip    = "Horizontal pitch of array, negative is to left";

  void x_dialog_array_edit_set_focus_chain(void) {
    GList *focus_chain; /* Aka Tab Order */
    focus_chain = NULL;
    focus_chain = g_list_append (focus_chain, select_butt);
    focus_chain = g_list_append (focus_chain, deselect_butt);
    focus_chain = g_list_append (focus_chain, dialog_data->row_spin);
    focus_chain = g_list_append (focus_chain, dialog_data->col_spin);
    focus_chain = g_list_append (focus_chain, dialog_data->row_off_entry);
    focus_chain = g_list_append (focus_chain, dialog_data->col_off_entry);
    focus_chain = g_list_append (focus_chain, row_butt);
    focus_chain = g_list_append (focus_chain, row_col_butt);
    focus_chain = g_list_append (focus_chain, col_butt);
    gtk_container_set_focus_chain (GTK_CONTAINER (table), focus_chain);
    g_list_free (focus_chain);
  }

  Dialog = gschem_dialog_new_empty(_("Create Array"),
                                   GTK_WINDOW(w_current->main_window),
  /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                   IDS_ARRAY_EDIT, w_current);

  dialog_data = x_dialog_array_new_data_structure(w_current);

  vbox = GTK_DIALOG(Dialog)->vbox;

  hbox = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox);
  gtk_container_add (GTK_CONTAINER (vbox), hbox);

  vbox2 = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (vbox2);
  gtk_box_pack_start (GTK_BOX (hbox), vbox2, TRUE, TRUE, 0);

  frame = gtk_frame_new (NULL);
  gtk_widget_show (frame);
  gtk_box_pack_start (GTK_BOX (vbox2), frame, TRUE, TRUE, 0);
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);

  alignment = gtk_alignment_new (0.5, 0.5, 1, 1);
  gtk_widget_show (alignment);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0, 10, 0);

  selection_label = geda_visible_label_new (_("<b>Selection</b>"));
  gtk_frame_set_label_widget (GTK_FRAME (frame), selection_label);
  geda_label_widget_set_use_markup (selection_label, TRUE);

  table = gtk_table_new (4, 7, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add (GTK_CONTAINER (alignment), table);
  gtk_widget_show (table);

  cnt_sel_label = GEDA_AV_LABEL_NEW (_("Count"), 0, 0.5);
  gtk_table_attach(GTK_TABLE(table), cnt_sel_label, 0, 1, 0, 1, GTK_FILL,0,0,0);

  widget = gtk_entry_new ();
  gtk_widget_show (widget);
  gtk_table_attach (GTK_TABLE (table), widget, 1, 2, 0, 1, (GTK_EXPAND), 0, 0, 0);
  gtk_editable_set_editable (GTK_EDITABLE (widget), FALSE);
  gtk_widget_set_can_focus(widget, FALSE);
  SetWidgetTip(widget, _(cnt_sel_tip));
  dialog_data->cnt_sel_entry = widget;

  x_size_label = GEDA_AV_LABEL_NEW (_("X Size"), 0, 0.5);
  gtk_table_attach(GTK_TABLE(table), x_size_label, 0, 1, 1, 2, GTK_FILL,0,0,0);

  y_size_label = GEDA_AV_LABEL_NEW (_("Y Size"), 0, 0.5);
  gtk_table_attach(GTK_TABLE(table), y_size_label, 0, 1, 2, 3, GTK_FILL,0,0,0);

  widget = gtk_entry_new ();
  gtk_widget_show (widget);
  gtk_table_attach (GTK_TABLE (table), widget, 1, 2, 1, 2, (GTK_EXPAND), 0, 0, 0);
  gtk_editable_set_editable (GTK_EDITABLE (widget), FALSE);
  gtk_widget_set_can_focus(widget, FALSE);
  SetWidgetTip(widget, _(x_size_tip));
  dialog_data->x_size_entry = widget;

  widget = gtk_entry_new ();
  gtk_widget_show (widget);
  gtk_table_attach (GTK_TABLE (table), widget, 1, 2, 2, 3, (GTK_EXPAND), 0, 0, 0);
  gtk_editable_set_editable (GTK_EDITABLE (widget), FALSE);
  gtk_widget_set_can_focus(widget, FALSE);
  SetWidgetTip(widget, _(y_size_tip));
  dialog_data->y_size_entry = widget;

  select_butt = gtk_button_new_with_mnemonic (_("Select"));
  gtk_widget_show (select_butt);
  gtk_table_attach (GTK_TABLE (table), select_butt, 6, 7, 0, 1, (GTK_FILL), 0, 0, 0);

  deselect_butt = gtk_button_new_with_mnemonic (_("Deselect"));
  gtk_widget_show (deselect_butt);
  gtk_table_attach (GTK_TABLE (table), deselect_butt, 6, 7, 1, 2, (GTK_FILL), 0, 0, 0);

  /* ----------------------- End Selection Frame ---------------------- */

  frame = gtk_frame_new (NULL);
  gtk_widget_show (frame);
  gtk_box_pack_start (GTK_BOX (vbox2), frame, TRUE, TRUE, 0);
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);

  alignment = gtk_alignment_new (0.5, 0.5, 1, 1);
  gtk_widget_show (alignment);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0, 10, 0);

  array_label = geda_visible_label_new (_("<b>Array</b>"));
  gtk_frame_set_label_widget (GTK_FRAME (frame), array_label);
  geda_label_widget_set_use_markup (array_label, TRUE);

  table = gtk_table_new (3, 7, FALSE);
  gtk_widget_show (table);
  gtk_container_add (GTK_CONTAINER (alignment), table);

/* Row 1 */
  row_label = GEDA_AV_LABEL_NEW (_("Row"), 0, 0.5);
  gtk_table_attach(GTK_TABLE(table), row_label, 0, 1, 0, 1, GTK_FILL,0,0,0);

  alignment = gtk_alignment_new (1, .5, 0, 0);
  gtk_widget_show (alignment);
  gtk_table_attach(GTK_TABLE(table), alignment, 1, 2, 0, 1, GTK_FILL,0,0,0);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0, 10, 5);

  row_cnt_label = GEDA_AV_LABEL_NEW (_("Count"), 1, 0.5);
  gtk_container_add (GTK_CONTAINER (alignment), row_cnt_label);

  widget = gtk_spin_button_new_with_range(1,999,1);
  gtk_widget_show (widget);
  gtk_table_attach(GTK_TABLE(table), widget, 2, 3, 0, 1, GTK_FILL,0,0,0);
  SetWidgetTip(widget, _(row_cnt_tip));
  SetSpinValue(widget, 2);
  dialog_data->row_spin = widget;

  alignment = gtk_alignment_new (1, .5, 0, 0);
  gtk_widget_show (alignment);
  gtk_table_attach(GTK_TABLE(table), alignment, 4, 5, 0, 1, GTK_FILL,0,0,0);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0, 15, 5);

  row_off_label = GEDA_AV_LABEL_NEW (_("Offset"), 1, 0.5);
  gtk_container_add (GTK_CONTAINER (alignment), row_off_label);

  widget = gtk_entry_new ();
  gtk_widget_show (widget);
  gtk_table_attach (GTK_TABLE (table), widget, 5, 6, 0, 1, (GTK_EXPAND), 0, 0, 0);
  SetWidgetTip(widget, _(row_off_tip));
  dialog_data->row_off_entry = widget;

/* Row 3*/
  col_label = GEDA_AV_LABEL_NEW (_("Column"), 0, 0.5);
  gtk_table_attach(GTK_TABLE(table), col_label, 0, 1, 2, 3, GTK_FILL,0,0,0);

  alignment = gtk_alignment_new (1, .5, 0, 0);
  gtk_widget_show (alignment);
  gtk_table_attach(GTK_TABLE(table), alignment, 1, 2, 2, 3, GTK_FILL,0,0,0);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0, 10, 5);

  col_cnt_label = GEDA_AV_LABEL_NEW (_("Count"), 1, 0.5);
  gtk_container_add (GTK_CONTAINER (alignment), col_cnt_label);

  widget = gtk_spin_button_new_with_range(1,999,1);
  gtk_widget_show (widget);
  gtk_table_attach(GTK_TABLE(table), widget, 2, 3, 2, 3, GTK_FILL,0,0,0);
  SetWidgetTip(widget, _(col_cnt_tip));
  SetSpinValue(widget, 2);
  dialog_data->col_spin = widget;

  alignment = gtk_alignment_new (1, 0.5, 0, 0);
  gtk_widget_show (alignment);
  gtk_table_attach_defaults(GTK_TABLE(table), alignment, 4, 5, 2, 3);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment), 0, 0, 15, 5);

  col_off_label = GEDA_AV_LABEL_NEW (_("Offset"), 1, 0.5);
  gtk_container_add (GTK_CONTAINER (alignment), col_off_label);

  widget = gtk_entry_new ();
  gtk_widget_show (widget);
  gtk_table_attach (GTK_TABLE (table), widget, 5, 6, 2, 3, (GTK_EXPAND), 0, 0, 0);
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  SetWidgetTip(widget, _(col_off_tip));
  dialog_data->col_off_entry = widget;

  row_butt = gtk_button_new_with_mnemonic (_("Y"));
  gtk_widget_show (row_butt);
  gtk_table_attach (GTK_TABLE (table), row_butt, 6, 7, 0, 1, (GTK_FILL), 0, 0, 0);

  alignment = gtk_alignment_new (1, 0.5, 1, 0);
  gtk_widget_show (alignment);
  gtk_table_attach_defaults(GTK_TABLE(table), alignment, 6, 7, 1, 2);

  row_col_butt = gtk_button_new_with_mnemonic (_("X,Y"));
  gtk_container_add (GTK_CONTAINER (alignment), row_col_butt);
  gtk_widget_show (row_col_butt);

  col_butt = gtk_button_new_with_mnemonic (_("X"));
  gtk_widget_show (col_butt);
  gtk_table_attach (GTK_TABLE (table), col_butt, 6, 7, 2, 3, (GTK_FILL), 0, 0, 0);

  preview_area = gtk_drawing_area_new ();
  gtk_widget_show (preview_area);
  gtk_box_pack_start (GTK_BOX (hbox), preview_area, TRUE, TRUE, 0);

  x_dialog_array_edit_set_focus_chain();

  /** Set the relationships between the label and their Widgets **/
  geda_label_set_mnemonic_widget (GEDA_LABEL(cnt_sel_label), dialog_data->cnt_sel_entry);

  atk_obj = atk_widget_linked_label_new (cnt_sel_label, dialog_data->cnt_sel_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Read only count of objects currently selected"));
    atk_object_set_description ( atk_obj,      cnt_sel_tip );
  }

  atk_obj = atk_widget_linked_label_new (x_size_label, dialog_data->x_size_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Read only x bounds displacement of selected object"));
    atk_object_set_description ( atk_obj,      x_size_tip );
  }

  atk_obj = atk_widget_linked_label_new (y_size_label, dialog_data->y_size_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Read only y bounds displacement of selected object"));
    atk_object_set_description ( atk_obj,      y_size_tip );
  }

  atk_obj = atk_widget_linked_label_new (row_cnt_label, dialog_data->row_spin);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Spinner entry number of rows to create"));
    atk_object_set_description ( atk_obj,      row_cnt_tip );
  }

  atk_obj = atk_widget_linked_label_new (col_cnt_label, dialog_data->col_spin);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Spinner entry number of columns to create"));
    atk_object_set_description ( atk_obj,      col_cnt_tip );
  }

  atk_obj = atk_widget_linked_label_new (row_off_label, dialog_data->row_off_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry for array horizontal pitch"));
    atk_object_set_description ( atk_obj,      row_off_tip );
  }

  atk_obj = atk_widget_linked_label_new (col_off_label, dialog_data->col_off_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("entry for array vertical pitch"));
    atk_object_set_description ( atk_obj,      col_off_tip );
  }

  gtk_widget_show_all (vbox);

  x_dialog_array_edit_action_area(Dialog, dialog_data);

  if (o_select_is_selection(w_current)) {
    gtk_widget_grab_focus (dialog_data->row_spin);
  }
  else {
    gtk_widget_grab_focus (select_butt);
  }

  g_signal_connect (select_butt, "clicked",
                    G_CALLBACK (on_select_butt_clicked),
                    Dialog);

  g_signal_connect (deselect_butt, "clicked",
                    G_CALLBACK (on_deselect_butt_clicked),
                    Dialog);

  g_signal_connect (row_butt, "clicked",
                    G_CALLBACK (on_row_butt_clicked),
                    Dialog);

  g_signal_connect (row_col_butt, "clicked",
                    G_CALLBACK (on_row_col_butt_clicked),
                    Dialog);

  g_signal_connect (col_butt, "clicked",
                    G_CALLBACK (on_col_butt_clicked),
                    Dialog);

  g_signal_connect (G_OBJECT (Dialog), "response",
                    G_CALLBACK (x_dialog_array_edit_response),
                    dialog_data);

  g_object_set (G_OBJECT (Dialog), DIALOG_SELECTION_TRACKER,
                x_dialog_ca_update_selection,
                NULL);

  GEDA_OBJECT_SET_DATA(Dialog, dialog_data, IDS_ARRAY_EDIT);

  return Dialog;
}

/*! \brief Creates the Properties dialog
 *  \par Function Description
 *  This function initiates creation of the Component Properties type dialog
 *  or raises the current dialog if called when the dialog is already open.
 */
void x_dialog_array_edit(GschemToplevel *w_current)
{
  GtkWidget *Dialog;

  Dialog = w_current->cawindow;

  if (!Dialog) {

    Dialog = x_dialog_array_edit_constructor(w_current);

    gtk_window_set_position(GTK_WINDOW (Dialog), GTK_WIN_POS_MOUSE);
    gtk_window_set_transient_for (GTK_WINDOW(Dialog),
                                  GTK_WINDOW(w_current->main_window));

    w_current->cawindow = Dialog;
    gtk_widget_show (Dialog);
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(Dialog));
  }

  x_dialog_ca_update_selection (w_current, NULL);
}

/********************** End of Array dialog box *************************/
/** @} end group Create-Array-Dialog */
