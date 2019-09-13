/* -*- C x_edit_slot.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2015 gEDA Contributors (see ChangeLog for details)
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
 * \file x_edit_slot.c
 * \brief A dialog box for editing Slot Attributes.
 */

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"

#include <geda_widgets.h>
#include <geda_debug.h>

/** \defgroup Edit-Slots-Dialog Edit Slots Dialog
 *  @{
 *  \ingroup Editing-Dialogs
 *  \par This group contains routines for the Edit Slots Dialog.
 *  \image html slot_dialog.png
 *  \image latex slot_dialog.png
 */

/*!
 * \brief response function for the slot edit dialog
 * \par Function Description
 *  The function calles o_slot_end to apply the dialog entry to the slot
 *  the selected symbol, if they exist.
 */
void x_dialog_edit_slot_response(GtkWidget      *ThisDialog, int response,
                                 GschemToplevel *w_current)
{
  GtkWidget *slotspin;
  int        slot;

  switch (response) {
    case GEDA_RESPONSE_REJECT:
    case GEDA_RESPONSE_DELETE_EVENT:
      gtk_widget_destroy(ThisDialog);
      i_status_set_state(w_current, SELECT);
      break;

    case GEDA_RESPONSE_APPLY:
      slotspin = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_SLOT_EDIT);
      slot     = GET_SPIN_IVALUE(slotspin);

      if (slot) {

        GedaObject *object = o_select_return_first_object (w_current);

        if (object != NULL) {

          char *slot_string = geda_sprintf ("slot=%d", slot);

          o_slot_end (w_current, object, slot_string);
          GEDA_FREE (slot_string);
          o_invalidate_object (w_current, object);
        }
      }
      break;

    default:
      BUG_IMSG ("unhandled case", response);
  }
}

/*!
 * \brief Handle selection change event for the Slot Editor Dialog
 * \par Function Description
 *  Updates the Slot Properties dialog widgets when the selection changes.
 *  The initial value is set when x_dialog_edit_slot is first called.
 *
 * \param w_current pointer to GschemToplevel context
 * \param object    pointer to a selected Object.
 */
static void
x_dialog_slot_edit_update_selection (GschemToplevel *w_current, GedaObject *object)
{
  if (object != NULL) {

    GtkWidget *ThisDialog;
    GtkWidget *countentry;
    GtkWidget *slotspin;
    char *slot_count = NULL;
    char *slot_value = NULL;

    if (object->type == OBJ_COMPLEX) {
      slot_count = geda_attrib_search_object_by_name (object, "numslots", 0);
      slot_value = geda_attrib_search_object_by_name (object, "slot", 0);
    }
    else {
      if (object->type == OBJ_TEXT) {
        slot_value = object->text->string;
      }
    }

    /* Get ptr to the Dialog window */
    ThisDialog = w_current->sewindow;

    /* Get ptr to the text widget */
    countentry = GEDA_OBJECT_GET_DATA (ThisDialog, "slot-count");
    slotspin   = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_SLOT_EDIT);

    if (slot_count != NULL) {
      SetEntryText( countentry, slot_count);
      gtk_spin_button_set_range ((GtkSpinButton*)slotspin, 1, atoi(slot_count));
    }
    else {
      SetEntryText( countentry, "0");
      gtk_spin_button_set_range ((GtkSpinButton*)slotspin, 0, 1);
    }

    if (slot_value != NULL) {
      bool enable =  slot_count ? atoi(slot_count) > 1 : FALSE;
      gtk_widget_set_sensitive (GTK_WIDGET(slotspin), enable);
      SetSpinValue(slotspin, atoi(slot_value));
      /* And set focus to the widget */
      gtk_widget_grab_focus(slotspin);
    }
    else {
      gtk_widget_set_sensitive (slotspin, FALSE);
    }
  }
}

/*!
 * \brief Create the slot entry dialog
 * \par Function Description
 *  This function creates the slot edit dialog.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 * \param [in] slots     Pointer to numslots value string
 * \param [in] slot      Pointer to slot value string
 */
void
x_dialog_edit_slot (GschemToplevel *w_current, const char *slots, const char *slot)
{
  GtkWidget *ThisDialog = w_current->sewindow;

  if (!ThisDialog) {

    GtkWidget *label = NULL;
    GtkWidget *slotspin;
    GtkWidget *textslots;
    GtkWidget *vbox;
    int        max_slot;

    ThisDialog = gschem_dialog_new_with_buttons(_("Edit slot number"),
                                               w_current->main_window,
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

    gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog),
                                     GEDA_RESPONSE_APPLY);

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    /* Number of slots */
    label = geda_aligned_label_new (_("Number of slots:"), 0, 0);
    gtk_box_pack_start(GTK_BOX (vbox), label, FALSE, FALSE, 0);

    textslots = geda_entry_new_with_max_length(80);
    gtk_box_pack_start( GTK_BOX(vbox), textslots, FALSE, FALSE, 0);

    /* Set the current text to the number of slots */
    if (slots != NULL) {
      SetEntryText(textslots, slots);
    }

    gtk_entry_set_alignment ((GtkEntry*)textslots, 1.0);
    gtk_editable_set_editable (GTK_EDITABLE(textslots), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(textslots), FALSE);

    /* Slot Number */
    if (slots != NULL) {
      max_slot = atoi(slots) > 0 ? atoi(slots) : 1;
    }
    else {
      max_slot = 1;
    }

    label = geda_aligned_label_new (_("Slot number:"), 0, 0);
    gtk_box_pack_start(GTK_BOX (vbox), label, FALSE, FALSE, 0);

    slotspin = gtk_spin_button_new_with_range(1, max_slot, 1);
    gtk_box_pack_end(GTK_BOX (vbox), slotspin, FALSE, FALSE, 0);
    gtk_widget_show (slotspin);

    SetWidgetTip(slotspin, _("Set the active slot number"));

    /* Set the current text to the number of slots */
    if (slot != NULL) {
      SetSpinValue(slotspin, atoi(slot));

    }

    gtk_widget_set_sensitive (slotspin, max_slot > 1);

    gtk_entry_set_alignment ((GtkEntry*)slotspin, 1.0);
    gtk_entry_set_activates_default ((GtkEntry*)slotspin, TRUE);

    GEDA_HOOKUP_OBJECT(ThisDialog, textslots, "slot-count");
    GEDA_HOOKUP_OBJECT(ThisDialog, slotspin, IDS_SLOT_EDIT);

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (x_dialog_edit_slot_response),
                      w_current);

    g_object_set (ThisDialog, DIALOG_SELECTION_TRACKER,
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
