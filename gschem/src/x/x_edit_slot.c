/* -*- C x_edit_slot.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */
/*!
 * \file x_edit_slot.c
 * \brief A dialog box for editing Slot Attributes.
 */

#include "gschem.h"
#include "x_dialog.h"

#include <geda_dialog_controls.h>
#include <geda_widgets.h>

#include <geda_debug.h>

/** \defgroup Edit-Slots-Dialog Slots Editing-Dialogs
 *  @{
 *  \ingroup (Editing-Dialogs)
 *
 *  \par This Group contains routines for the Edit Slots Dialog.
 */

/*! \brief response function for the slot edit dialog
 *  \par Function Description
 *  The function calles o_slot_end to apply the dialog entry to the slot
 *  the selected symbol, if they exist.
 */
void x_dialog_edit_slot_response(GtkWidget      *ThisDialog, int response,
                                 GschemToplevel *w_current)
{
  GtkWidget  *textentry;
  Object     *object;
  char       *slot_string;
  const char *string = NULL;
  int         len;

  switch (response) {
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(ThisDialog);
    i_status_set_state(w_current, SELECT);
    break;
  case GEDA_RESPONSE_APPLY:
    textentry = g_object_get_data(G_OBJECT(ThisDialog), IDS_SLOT_EDIT);
    string =  GetEntryText( textentry );
    len = strlen(string);
    if (len != 0) {

      object = o_select_return_first_object (w_current);
      if (object != NULL) {
        slot_string = u_string_sprintf ("slot=%s", string);
        o_slot_end (w_current, object, slot_string);
        GEDA_FREE (slot_string);
        o_invalidate_object (w_current, object);
      }
    }
    break;

  default:
    BUG_IMSG ("unhandled case for signal <%d>", response);
  }

}

/*! \brief Handle selection change event for the Slot Editor Dialog
 *  \par Function Description
 *  Updates the Slot Properties dialog widgets when the selection changes.
 *  The initial value is set when x_dialog_edit_slot is first called.
 *
 *  \param w_current pointer to GschemToplevel context
 *  \param object    pointer to a selected Object.
 */
static void
x_dialog_slot_edit_update_selection (GschemToplevel *w_current, Object *object)
{
  GtkWidget *ThisDialog;
  GtkWidget *countentry;
  GtkWidget *textentry;
  char *slot_count = NULL;
  char *slot_value = NULL;

  if (object != NULL) {

    if (object->type == OBJ_COMPLEX) {
      slot_count = o_attrib_search_object_attribs_by_name (object, "numslots", 0);
      slot_value = o_attrib_search_object_attribs_by_name (object, "slot", 0);
    }
    else {
      if (object->type == OBJ_TEXT) {
        slot_value = object->text->string;
      }
    }

    /* Get ptr to the Dialog window */
    ThisDialog = w_current->sewindow;

    /* Get ptr to the text widget */
    countentry = g_object_get_data(G_OBJECT(ThisDialog), "slot-count");
    textentry  = g_object_get_data(G_OBJECT(ThisDialog), IDS_SLOT_EDIT);

    if (slot_count != NULL) {
      SetEntryText( countentry, slot_count);
    }
    else {
      SetEntryText( countentry, "0");
    }

    if (slot_value != NULL) {
      gtk_widget_set_sensitive (textentry, TRUE);
      SetEntryText( textentry, slot_value );
      gtk_editable_select_region (GTK_EDITABLE(textentry), 0, -1);
      /* And set focus to the widget */
      gtk_widget_grab_focus(textentry);
    }
    else {
      gtk_widget_set_sensitive (textentry, FALSE);
    }
  }

}

/*! \brief Create the slot entry dialog
 *  \par Function Description
 *  This function creates the slot edit dialog.
 *
 *  \param [in] w_current Pointer to a GschemToplevel object
 *  \param [in] slots     Pointer to numslots value string
 *  \param [in] slot      Pointer to slot value string
 */
void
x_dialog_edit_slot (GschemToplevel *w_current, const char *slots, const char *slot)
{
  GtkWidget *ThisDialog;
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *textslots;
  GtkWidget *vbox;

  ThisDialog = w_current->sewindow;

  if (!ThisDialog) {

    ThisDialog = gschem_dialog_new_with_buttons(_("Edit slot number"),
                                   GTK_WINDOW(w_current->main_window),
          /* nonmodal Editing ThisDialog */    GSCHEM_MODELESS_DIALOG,
                                             IDS_SLOT_EDIT, w_current,
                                 GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                 GTK_STOCK_APPLY, GEDA_RESPONSE_APPLY,
                                                                  NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GEDA_RESPONSE_APPLY,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(ThisDialog),
                        GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog),
                                     GEDA_RESPONSE_APPLY);

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    /* Number of slots */
    label = geda_aligned_label_new (_("Number of slots:"), 0, 0);
    gtk_box_pack_start(GTK_BOX (vbox), label, FALSE, FALSE, 0);

    textslots = gtk_entry_new();
    gtk_box_pack_start( GTK_BOX(vbox), textslots, FALSE, FALSE, 0);

    gtk_entry_set_max_length(GTK_ENTRY(textslots), 80);

    /* Set the current text to the number of slots */
    if (slots != NULL) {
      SetEntryText(textslots, slots);
    }
    gtk_editable_set_editable (GTK_EDITABLE(textslots), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(textslots), FALSE);

    /* Slot Number */
    label = geda_aligned_label_new (_("Edit slot number:"), 0, 0);
    gtk_box_pack_start(GTK_BOX (vbox), label, FALSE, FALSE, 0);

    textentry = gtk_entry_new();
    gtk_box_pack_start(GTK_BOX(vbox),textentry, FALSE, FALSE, 0);
    gtk_entry_set_max_length(GTK_ENTRY(textentry), 80);

    /* Set the current text to the slot number */
    if (slot != NULL) {
      SetEntryText(textentry, slot);
      gtk_editable_select_region (GTK_EDITABLE(textentry), 0, -1);
    }

    gtk_entry_set_activates_default (GTK_ENTRY(textentry),TRUE);

    GEDA_HOOKUP_OBJECT(ThisDialog, textslots, "slot-count");
    GEDA_HOOKUP_OBJECT(ThisDialog, textentry, IDS_SLOT_EDIT);

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_edit_slot_response),
                      w_current);

    g_object_set(G_OBJECT (ThisDialog), DIALOG_SELECTION_TRACKER,
                 x_dialog_slot_edit_update_selection,
                 NULL);

    w_current->sewindow = ThisDialog;
    gtk_widget_show_all (ThisDialog);
  }
  else { /* dialog already created */
    x_dialog_slot_edit_update_selection (w_current, NULL);
    gtk_window_present (GTK_WINDOW(ThisDialog));
  }

}
/******************** End of Slot Edit dialog box ***********************/
/** @} End Group Edit-Slots-Dialog */