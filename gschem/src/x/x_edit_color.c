/* -*- C x_edit_color.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file x_edit_color.c
 * \brief A dialog box for editing object color property.
 */

#include <gschem.h>
#include <x_dialog.h>
#include <geda_widgets.h>
#include <geda_debug.h>

/** \defgroup Edit-Color-Dialog Edit Color Dialog
 *  @{
 *  \ingroup Editing-Dialogs
 *
 *  \par This group contains routines for the Edit Color dialog.
 */

/*! \brief Handle selection change event for x_dialog_edit_color
 *  \par Function Description
 *  Updates the color combobox when the selection changes.
 *
 *  \param w_current pointer to GschemToplevel context
 *  \param object    pointer to a selected Object.
 */
static void
x_dialog_color_update_selection (GschemToplevel *w_current, GedaObject *object)
{
  if (object != NULL) {

    GedaComboBox *ColorCombo;
    int index;

    index = object->color;
    ColorCombo = GEDA_OBJECT_GET_DATA(w_current->clwindow, IDS_COLOR_EDIT);

    geda_combo_box_set_active(ColorCombo, index);
  }
}

/*! \brief Apply a color change to selected objects
 *  \par Function Description
 *  This function applies a color change to the currently selected objects.
 */
static void
x_dialog_edit_color_apply(GtkWidget *ThisDialog, GschemToplevel *w_current)
{
  GList  *s_current = geda_list_get_glist(Current_Selection);

  if (s_current != NULL) {

    GedaComboBox *ColorCombo;
    GtkTreeIter   iter;
    GValue        value = {0, };

    /* Get ptr to the Combo widget */
    ColorCombo = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_COLOR_EDIT);

    /* Retrieve the current index from the tree model in the widget */
    if (geda_combo_box_get_active_iter(ColorCombo, &iter)) {

      int color_index;

      gtk_tree_model_get_value(geda_combo_box_get_model(ColorCombo), &iter, 1, &value);
      color_index = g_value_get_int (&value);

      while(s_current != NULL) {

        GedaObject *object = (GedaObject*) s_current->data;

        if (object == NULL) {
          BUG_MSG("ERROR: NULL object");
        }
        else {
          if(object->color != color_index) {
            geda_set_object_color (object, color_index);
          }
        }
        s_current = g_list_next(s_current);
      }
    }
    o_undo_savestate(w_current, UNDO_ALL);
  }
}

/*! \brief response function for the color edit dialog
 *  \par Function Description
 *  This function takes the user response from the color edit dialog
 */
static void
x_dialog_edit_color_response(GtkWidget *Dialog, int response,
                           GschemToplevel *w_current)
{
  switch (response) {
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    /* cut link from dialog to selection */
    gtk_widget_destroy(Dialog);
    break;
  case GEDA_RESPONSE_ACCEPT:
    x_dialog_edit_color_apply(Dialog, w_current);
    break;
  default:
    BUG_IMSG ("unhandled case", response);
  }
}

/*! \brief Create the color edit dialog
 *  \par Function Description
 *  This function creates the color edit dialog
 */
void x_dialog_edit_color (GschemToplevel *w_current)
{
  GtkWidget *Dialog = w_current->clwindow;

  GedaObject *object;
  int color_index;

  object = o_select_return_first_object (w_current);

  if (object != NULL)
    color_index = object->color;
  else
    color_index = ATTRIBUTE_COLOR;

  if (!Dialog) {

    GtkWidget *optionmenu;
    GtkWidget *label;
    GtkWidget *vbox;

    Dialog = gschem_dialog_new_with_buttons(_("Color Edit"),
                                            w_current->main_window,
           /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                            IDS_COLOR_EDIT, w_current,
                                            GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                            GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(Dialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);

    gtk_window_set_position (GTK_WINDOW (Dialog), GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (Dialog),
                                     GEDA_RESPONSE_ACCEPT);

    vbox = GTK_DIALOG(Dialog)->vbox;

    label = geda_aligned_label_new(_("Object color:"), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    optionmenu = create_color_menu (w_current, color_index);

    gtk_box_pack_start(GTK_BOX(vbox), optionmenu, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(optionmenu, _("Select a color for the selected objects."));

    HD_ACTION_SEPARATOR (vbox);

    GEDA_HOOKUP_OBJECT(Dialog, optionmenu, IDS_COLOR_EDIT);

    gtk_window_set_transient_for (GTK_WINDOW(Dialog), w_current->main_window);

    g_signal_connect (Dialog, "response",
                      G_CALLBACK (x_dialog_edit_color_response),
                      w_current);

    g_object_set (Dialog, DIALOG_SELECTION_TRACKER,
                  x_dialog_color_update_selection, NULL);

    gtk_widget_show_all(Dialog);
    w_current->clwindow = Dialog;
  }

  else { /* dialog already created */
    x_dialog_color_update_selection (w_current, object);
    gtk_window_present(GTK_WINDOW(Dialog));
  }
}

/******************** End of color edit dialog box **********************/
/** @} end group Edit-Color-Dialog */