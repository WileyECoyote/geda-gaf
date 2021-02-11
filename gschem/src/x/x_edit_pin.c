/* -*- C x_edit_pin.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2017 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2013-2017 gEDA Contributors (see ChangeLog for details)
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
 * \file x_edit_pin.c
 * \brief A dialog box for editing Pin Object properties.
 */

#include <gdk/gdk.h>

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"

#include <geda_widgets.h>
#include <geda_debug.h>

/** \defgroup Edit-Pin-Dialog Edit Pin Properties Dialog
 *  @{
 *  \ingroup Editing-Dialogs
 *  \par This group contains routines for the Edit Pin dialog.
 *  \image html pin_dialog.png
 *  \image latex pin_dialog.png
 */

/*! \brief Enumerated Pin Dialog Control IDs. */
typedef enum {

/* Combo Chooser */
  PinNodeType,
  PinElectrical,

/* Spinner Entries */
  PinNumber,
  PinSequence,

  /* Text Entry */
  PinLabel,

  /* Switches */
  SetPinNodeType,
  SetElectrical,
  AutoNumber,
  AutoSequence,

} ControlID;

/*! \brief String Arrays for Dialog Contrls.
 *  { "Hook-Up-String", "Label", "Tooltip string"},
 */
static WidgetStringData DialogStrings[] = {
  {"pin-type",         N_("_Type:"),          N_("Select the pin type")},
  {"pin-attribute",    N_("_Attributes:"),    N_("Set the pin attributes")},
  {"pin-number",       N_("_Number:"),        N_("Set the pin number, or the starting pin number if auto numbering")},
  {"pin-sequence",     N_("_Sequence:"),      N_("Set the sequence number")},
  {"pin-label",        N_("_Label:"),         N_("Enter pin name or description")},

  {"SetPinNodeTypeSwitch", N_("Type:"),       N_("Enable to set all selected attributes to the prescribed type")},
  {"SetElectricalSwitch",  N_("Attributes:"), N_("Enable to set all selected attributes to the prescribed value")},

  {"AutoNumberSwitch",     N_("Number:"),     N_("Enable or disable renumbering pin numbers, Number will be the starting pin number")},
  {"AutoSequenceSwitch",   N_("Sequence:"),   N_("Enable or disable re-sequencing pins\n, this only changes the sequence number, not the actual sequence in the symbol file")},

  { NULL, NULL, NULL},
};

/*!
 * \brief Create a pin attribute menu for the Pin Properties Editor dialog
 * \par Function Description
 *  This function creates a GedaMenu with the different pin attributes.
 */
static GtkWidget *create_menu_pin_electricals ( void )
{
  GtkWidget *menu;
  GSList *group;
  struct pin_electrical {
    char *str;
    PIN_ELECT electrical;
  } types[] = { { N_("in"),          PIN_ELECT_IN },
                { N_("out"),         PIN_ELECT_OUT },
                { N_("io"),          PIN_ELECT_IO },
                { N_("oc"),          PIN_ELECT_OC },
                { N_("oe"),          PIN_ELECT_OE },
                { N_("pas"),         PIN_ELECT_PAS },
                { N_("tp"),          PIN_ELECT_TP },
                { N_("tri"),         PIN_ELECT_TRI },
                { N_("clk"),         PIN_ELECT_CLK },
                { N_("pwr"),         PIN_ELECT_PWR },
                { N_("*missing*"),   PIN_ELECT_VOID }
              };
  int i;

  menu  = geda_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof(types) / sizeof(struct pin_electrical); i++) {

    GtkWidget *menuitem;

    menuitem = geda_radio_menu_item_new_with_label (group, _(types[i].str));
    group = geda_radio_menu_item_group (GEDA_RADIO_MENU_ITEM (menuitem));
    geda_menu_append (menu, menuitem);
    GEDA_OBJECT_SET_DATA(menuitem,
                         (void*)(long) (types[i].electrical),
                         WIDGET(PinElectrical));
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*!
 * \brief Create a pin type menu for the Pin Properties Editor dialog
 * \par Function Description
 *  This function creates a GedaMenu with the different pin types.
 */
static GtkWidget *create_menu_pin_type ( void )
{
  GtkWidget *menu;
  GSList    *group;

  struct pin_type {
    char *str;
    PIN_NODE type;
  } types[] = { { N_("Net"),         PIN_NET_NODE },
                { N_("Bus"),         PIN_BUS_NODE }
              };
  int i;

  menu  = geda_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof(types) / sizeof(struct pin_type); i++) {

    GtkWidget *menuitem;

    menuitem = geda_radio_menu_item_new_with_label (group, _(types[i].str));
    group    = geda_radio_menu_item_group (GEDA_RADIO_MENU_ITEM (menuitem));

    geda_menu_append (menu, menuitem);

    GEDA_OBJECT_SET_DATA(menuitem,
                         (void*)(long) (types[i].type),
                         WIDGET(PinNodeType));
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*!
 * \brief Set the Values in the Pin Properties Editor dialog
 * \par Function Description
 *  Set all widgets in the pin type dialog. Widgets with variables
 *  having a value -1 are disabled.
 *
 * \param [in]   pin_data   dialog structure
 * \param [in]   label      pin name
 * \param [in]   number     the pin number.
 * \param [in]   sequence   sequence of the pin
 * \param [in]   elect_type electrical type enumerated code
 * \param [in]   mech_type  mechanical type enumerated code
 * \param [in]   node_type  node type - either NET or BUS
 */
static void
x_dialog_edit_pin_type_set_values(pin_type_data *pin_data, const char *label, const char *number, int sequence,
                                  PIN_ELECT elect_type, PIN_MECH mech_type, PIN_NODE node_type)
{
  GtkWidget *menu, *menuitem;

  geda_option_widget_set_history(pin_data->node_type, node_type);
  menu = geda_option_widget_get_menu(pin_data->node_type);
  menuitem = geda_menu_widget_get_active(menu);
  geda_check_menu_item_set_active(GEDA_CHECK_MENU_ITEM(menuitem), TRUE);

  if (number == NULL) {
    SetEntryText( pin_data->number_entry, _("*missing*"));
  }
  else {
    SetEntryText( pin_data->number_entry, number);
  }

  if (sequence == -1) {
    gtk_widget_set_sensitive(pin_data->sequence_spin, FALSE);
  }
  else {
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(pin_data->sequence_spin), sequence);
  }

  if (label == NULL) {
    SetEntryText( pin_data->label_entry, _("*missing*") );
  }
  else {
    SetEntryText( pin_data->label_entry, label );
  }

  geda_option_widget_set_history(pin_data->pin_electrical, elect_type);
  menu = geda_option_widget_get_menu(pin_data->pin_electrical);
  menuitem = geda_menu_widget_get_active(menu);
  geda_check_menu_item_set_active(GEDA_CHECK_MENU_ITEM(menuitem), TRUE);

}

static const char *get_pin_entry_string(GtkWidget *entry)
{
  const char *string;

  if ( GetEntryLength(entry) == 0) {
    string = NULL;
  }
  else {
    string = GetEntryText( entry );
    if (strcmp(string, "*missing*") == 0) {
      string = NULL;
    }
  }

  return string;
}

/*!
 * \brief Apply function for the Pin Properties Editor Dialog
 * \par Function Description
 *  The function retrieves the values in the Pin Editor dialog
 *  and applies values to selected objects based on dialog settings
 *  and the current selection.
 */
static void
x_dialog_edit_pin_type_ok(GtkWidget *Dialog, pin_type_data *pin_data)
{
  GschemToplevel *w_current;

  GList *pin_objects;

  GedaObject *object;

  bool    set_node_type      = FALSE;
  bool    set_elect_type     = FALSE;
  bool    auto_sequence      = FALSE;
  bool    changed_something  = FALSE;
  int     num_selected;

  PIN_NODE  ntype, ontype;  /* bus, net*/
  PIN_ELECT etype, oetype;  /* in, out, io, pwr, etc ... */
  PIN_MECH  omtype;

  const char *label_str,  *olabel_str;
  const char *number_str, *onumber_str;
  int sequence, osequence;

  /* Initialize variables */
  w_current   = GSCHEM_DIALOG(Dialog)->w_current;
  pin_objects = NULL;

  /* if nothing selected then get out */
  if (!o_select_is_selection(w_current))
    return;

  ntype = (int)(long)(
    GEDA_OBJECT_GET_DATA (
        geda_menu_widget_get_active (
          geda_option_widget_get_menu (
            pin_data->node_type)), WIDGET(PinNodeType)));

  if (ntype != PIN_NET_NODE && ntype != PIN_BUS_NODE) {
    titled_warning_dialog(_("Pin Properties"), "%s", _("Invalid Pin Node Type"));
    return;
  }

  etype = (int)(long)(
    GEDA_OBJECT_GET_DATA (
        geda_menu_widget_get_active (
          geda_option_widget_get_menu (
            pin_data->pin_electrical)), WIDGET(PinElectrical)));

  if (etype == PIN_ELECT_VOID)
    titled_information_dialog(_("Pin Properties"), "%s", _("Ignoring Pin electrical VOID"));

  /* GtkEntry does not emit "active" properly, so tell spinner to check entry */
  gtk_spin_button_update (GTK_SPIN_BUTTON (pin_data->sequence_spin));
  sequence = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (pin_data->sequence_spin));

  /* Get the current selected pin objects and the count */
  pin_objects  = o_select_get_list_selected(w_current, OBJ_PIN);
  num_selected = g_list_length( pin_objects);

  if (num_selected == 1) { /* Only 1 pin selected */

    object = (GedaObject*)g_list_nth_data(pin_objects, 0);

    if (geda_pin_object_get_attributes(object, &olabel_str, &onumber_str,
                                       &osequence, &oetype, &omtype, &ontype))
    {
      /* get the new strings from the dialog */
      label_str  = get_pin_entry_string (pin_data->label_entry);
      number_str = get_pin_entry_string (pin_data->number_entry);

      if (ntype != ontype) {
        changed_something = TRUE;
        ontype = ntype;
      }

      if (etype != oetype) {
        changed_something = TRUE;
        oetype = etype;
      }
      else {
        oetype = -1;
      }

      if (sequence != osequence) {
        changed_something = TRUE;
        osequence = sequence;
      }
      else {
        osequence = -1;
      }

      if (onumber_str == NULL && number_str != NULL) {
        changed_something = TRUE;
        onumber_str = number_str;
      }
      else if (onumber_str != NULL && number_str != NULL ) {
        if (strcmp(number_str, onumber_str) == 0) {
          onumber_str = NULL;
        }
        else {
          changed_something = TRUE;
          onumber_str = number_str;
        }
      }

      if (olabel_str == NULL && label_str != NULL) {
        changed_something = TRUE;
        olabel_str = label_str;
      }
      else if (olabel_str != NULL && label_str != NULL ) {
        if (strcmp(label_str, olabel_str) == 0) {
          olabel_str = NULL;
        }
        else {
          changed_something = TRUE;
          olabel_str = label_str;
        }
      }

      if (changed_something) {
        geda_pin_object_set_attributes (object, olabel_str, onumber_str,
                                        osequence, oetype, omtype, ontype);
      }

    }
    else {
      BUG_MSG("got FALSE pin object\n");
    }
  }
  else { /* More than 1 pin is selected */

    GList *iter;
    int  auto_number;
    char s_val[10];

    set_node_type  = GET_SWITCH_STATE(pin_data->set_node_type);
    set_elect_type = GET_SWITCH_STATE(pin_data->set_elect_type);
    auto_number    = GET_SWITCH_STATE(pin_data->auto_number);
    auto_sequence  = GET_SWITCH_STATE(pin_data->auto_sequence);

    if (auto_number) {
      number_str = get_pin_entry_string (pin_data->number_entry);
      if (number_str) {
        auto_number = atoi(number_str);
      }
      /* else autonumber = switch TRUE = 1 = default starting number */
    }

    for (iter = pin_objects; iter != NULL; iter = g_list_next(iter)) {

      object = (GedaObject*) iter->data;

      if (geda_pin_object_get_attributes(object, &olabel_str, &onumber_str, &osequence, &oetype, &omtype, &ontype))
      {
        if (set_node_type) {
          if (ontype == -1 || ntype != ontype) {
            changed_something = TRUE;
            ontype = ntype;
          }
        }
        if(set_elect_type) {
          if (etype != -1 || etype != oetype) {
            changed_something = TRUE;
            oetype = etype;
          }
        }

        if(auto_number) {
          if (onumber_str) {
            if (auto_number != atoi(onumber_str)) {
              changed_something = TRUE;
              onumber_str = geda_utility_string_int2str( auto_number, s_val, 10 );
            }
          }
          else {
            changed_something = TRUE;
            onumber_str = geda_utility_string_int2str( auto_number, s_val, 10 );
          }
          auto_number++;
        }

        if(auto_sequence) {
          if (sequence != osequence) {
            changed_something = TRUE;
            osequence = sequence;
          }
          sequence++;
        }
        geda_pin_object_set_attributes (object, olabel_str, onumber_str, osequence, oetype, -1, ontype);
      }
    }
  }

  if(changed_something) {
    o_undo_savestate(w_current, UNDO_ALL);
  }

  g_list_free (pin_objects);
}

/*!
 * \brief Set Sensitivities of Widgets on the Pin Properties Editor
 * \par Function Description:
 *  This function obtains a list of selected pin objects and sets
 *  sensitivities and Tooltip visibilities based on the number of
 *  select pins and the state of the switch (check-box buttons).
 *  over_rides is the table widget containing all of the over-ride
 *  switches and their associated labels and is used to "disable"
 *  or enable all switches.
 *
 *  \param [in]  Dialog  ptr to the dialog widget
 */
static void xd_edit_pin_set_sensitivity(GschemDialog *Dialog)
{
  GtkWidget *over_rides;
  GtkWidget *apply_butt;

  GList *pin_objects = NULL;
  bool   state;
  int    num_selected;

  pin_type_data *pin_data;

  GschemToplevel *w_current = Dialog->w_current;

  /* Get ptr to the data structure */
  pin_data = GEDA_OBJECT_GET_DATA (Dialog, IDS_PIN_EDIT);

  /* Determine the number of selected pin objects */
  pin_objects   = o_select_get_list_selected(w_current, OBJ_PIN);
  num_selected  = g_list_length( pin_objects);
  g_list_free (pin_objects); /*Just needed to know how many */

  over_rides = GEDA_OBJECT_GET_DATA (Dialog, "over-rides");
  apply_butt = GEDA_OBJECT_GET_DATA (Dialog, "apply-butt");

  /* Set sensitivity of widgets */
  if ( num_selected == 0 ) {  /* No pins selected so disable everything! */

    /* Disable the Switches & their labels*/
    gtk_widget_set_sensitive (apply_butt, FALSE);

    /* Disable the Switches & their labels*/
    gtk_widget_set_sensitive (over_rides, FALSE);

    /* Hide Tooltips for the disabled Switches */
    g_object_set (pin_data->set_node_type,  "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->set_elect_type, "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->auto_number,    "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->auto_sequence,  "has-tooltip", FALSE, NULL);

    /* Disable all input widgets */
    gtk_widget_set_sensitive (pin_data->node_type,       FALSE);
    gtk_widget_set_sensitive (pin_data->pin_electrical,  FALSE);
    gtk_widget_set_sensitive (pin_data->number_entry,    FALSE);
    gtk_widget_set_sensitive (pin_data->sequence_spin,   FALSE);
    gtk_widget_set_sensitive (pin_data->label_entry,     FALSE);

    /* Disable all of the Tooltips for input widgets */
    g_object_set (pin_data->node_type,      "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->pin_electrical, "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->number_entry,   "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->sequence_spin,  "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->label_entry,    "has-tooltip", FALSE, NULL);
  }
  else if ( num_selected == 1 ) {

    /* Enable the apply button */
    gtk_widget_set_sensitive (apply_butt, TRUE);

    /* Disable the Switches & their labels */
    gtk_widget_set_sensitive (over_rides, FALSE);

    /* Hide Tooltips for the disabled Switches */
    g_object_set (pin_data->set_node_type,  "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->set_elect_type, "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->auto_number,    "has-tooltip", FALSE, NULL);
    g_object_set (pin_data->auto_sequence,  "has-tooltip", FALSE, NULL);

    /* Enable all input widgets */
    gtk_widget_set_sensitive (pin_data->node_type,       TRUE);
    gtk_widget_set_sensitive (pin_data->pin_electrical,   TRUE);
    gtk_widget_set_sensitive (pin_data->number_entry,     TRUE);
    gtk_widget_set_sensitive (pin_data->sequence_spin,   TRUE);
    gtk_widget_set_sensitive (pin_data->label_entry,     TRUE);

    /* Enable all the Tooltips for input widgets */
    g_object_set (pin_data->node_type,      "has-tooltip", TRUE, NULL);
    g_object_set (pin_data->pin_electrical, "has-tooltip", TRUE, NULL);
    g_object_set (pin_data->number_entry,   "has-tooltip", TRUE, NULL);
    g_object_set (pin_data->sequence_spin,  "has-tooltip", TRUE, NULL);
    g_object_set (pin_data->label_entry,    "has-tooltip", TRUE, NULL);

  }
  else {

    gtk_widget_set_sensitive (apply_butt, TRUE);

    /* Enable all  Switches & their labels */
    gtk_widget_set_sensitive (over_rides, TRUE);

    /* Enable Tooltips for the Switches */
    g_object_set (pin_data->set_node_type,  "has-tooltip", TRUE, NULL);
    g_object_set (pin_data->pin_electrical, "has-tooltip", TRUE, NULL);
    g_object_set (pin_data->auto_number,    "has-tooltip", TRUE, NULL);
    g_object_set (pin_data->auto_sequence,  "has-tooltip", TRUE, NULL);

    /* Set the remaining widgets based on switch states */
    /* The Pin Type Combo widget */
    state = GET_SWITCH_STATE (pin_data->set_node_type);
    gtk_widget_set_sensitive (pin_data->node_type,         state);
    g_object_set (pin_data->node_type,      "has-tooltip", state, NULL);

    /* The Attributes Combo widget */
    state = GET_SWITCH_STATE (pin_data->set_elect_type);
    gtk_widget_set_sensitive (pin_data->pin_electrical, state);
    g_object_set (pin_data->pin_electrical, "has-tooltip", state, NULL);

    /* The Pin Number Entry widget */
    state = GET_SWITCH_STATE (pin_data->auto_number);
    gtk_widget_set_sensitive (pin_data->number_entry, state);
    g_object_set (pin_data->number_entry,   "has-tooltip", state, NULL);

    /* The Sequence Number Spinner Entry widget */
    state = GET_SWITCH_STATE (pin_data->auto_sequence);
    gtk_widget_set_sensitive (pin_data->sequence_spin, state);
    g_object_set (pin_data->sequence_spin,  "has-tooltip", state, NULL);

    /* Disable the label widget and the label Tooltip */
    gtk_widget_set_sensitive (pin_data->label_entry,   FALSE);
    g_object_set (pin_data->label_entry,    "has-tooltip", FALSE, NULL);
  }
}

/*!
 * \brief Callback when a Switch is toggled on the Pin Properties Editor
 * \par Function Description:
 *  This function changes images for switches that are toggled. The image
 *  is set to the opposite state, i.e. if ON use OFF image and if OFF use ON
 *  image. The function then calls xd_edit_pin_set_sensitivity to update
 *  sensitivities of all the applicable widgets on the dialog.
 *
 * \param [in]  Switch  ptr to the switch, aka toggle-button, widget
 * \param [in]  Dialog  ptr to the dialog widget
 */
static void
xd_edit_pin_switch_toggled(GtkWidget *Switch, GschemDialog *Dialog)
{
  /* Change the Switch image */
  TOGGLE_SWITCH(Switch);

  /* Update Widget sensitivities */
  xd_edit_pin_set_sensitivity(Dialog);

  return;
}

/*!
 * \brief Handle selection change event for Pin Properties Editor Dialog
 * \par Function Description
 *  Updates the Pin Properties dialog widgets when the selection changes.
 *  It uses the selection to set it's initial values.
 *
 * \param w_current Pointer to GschemToplevel data structure
 * \param object    Pointer to a selected Object
 */
static void
xd_pin_type_update_selection (GschemToplevel *w_current, GedaObject *object)
{
  /* Get ptr to the Dialog window */
  GschemDialog *Dialog = GSCHEM_DIALOG (w_current->ptwindow);

  if (object != NULL && object->type == OBJ_PIN) {

    int sequence;

    const char *label;
    const char *number;

    PIN_ELECT      elect_type;
    PIN_MECH       mech_type;
    PIN_NODE       node_type;
    pin_type_data *pin_data;

    /* Initialize variables */
    label      = NULL;
    sequence   = -1;
    elect_type = PIN_ELECT_PAS;
    node_type  = PIN_NET_NODE;

    xd_edit_pin_set_sensitivity ( Dialog );

    /* Get ptr to the data structure */
    pin_data = GEDA_OBJECT_GET_DATA (Dialog, IDS_PIN_EDIT);

    /* Check this object */
    if (geda_pin_object_get_attributes(object, &label, &number, &sequence,  &elect_type, &mech_type, &node_type)) {
      /* fill in the fields of the dialog */
      x_dialog_edit_pin_type_set_values(pin_data, label, number, sequence, elect_type, mech_type, node_type);
      /* And set focus to the Pin-type combo menu */
      gtk_widget_grab_focus(pin_data->node_type);
    }
    /* Else do nothing! */
  }
  else {
    /* Disable all the widget on this dialog */
    xd_edit_pin_set_sensitivity(Dialog);
  }
}

/*!
 * \brief Response function for the Pin Properties Editor dialog
 * \par Function Description
 *  This is a response function called when the used selects one
 *  of the action bottons, either CLOSE or APPLY.
 *
 * \param [in]   Dialog    ptr to the dialog widget
 * \param [in]   response  int signal indicating which button
 * \param [in]   pin_data  ptr to THE pin_type_data struction
 */
void
x_dialog_edit_pin_type_response(GtkWidget     *Dialog,
                                int            response,
                                pin_type_data *pin_data)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
    case GEDA_RESPONSE_REJECT:
    case GEDA_RESPONSE_DELETE_EVENT:
      gtk_widget_destroy (Dialog);
      GEDA_FREE (pin_data);
      break;

    case GEDA_RESPONSE_ACCEPT:
      x_dialog_edit_pin_type_ok(Dialog, pin_data);
      break;

    default:
      BUG_IMSG ("unhandled case", response);
  }

  i_status_set_state (w_current, SELECT);

}

/*! \brief Emit GEDA_RESPONSE_REJECT response signal */
static void on_close_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data),
                           "response",
                           GEDA_RESPONSE_REJECT,
                           user_data);
}

/*! \brief Emit GEDA_RESPONSE_ACCEPT response signal */
static void on_apply_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data),
                           "response",
                           GEDA_RESPONSE_ACCEPT,
                           user_data);
}

/*!
 * \brief Creates Action Area for the Pin Type Dialog
 * \par Function Description
 *  We create our own "Action Area", because; 1.) GTK's entire
 *  concept of an action area is more of a hindrance then it is
 *  useful, and 2.) We need access to the apply button widget.
 */
static GtkWidget*
create_action_area (GschemDialog *ThisDialog, GtkWidget *parent) {

  GtkWidget *action_hbox = NULL;

  /* Create a Horizontal Box for everything to go into */
  NEW_HCONTROL_BOX(parent, action, DIALOG_H_SPACING);

  /* Create and connect the Close and Apply Buttons */
  GtkWidget *close_butt = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
  GtkWidget *apply_butt = gtk_button_new_from_stock (GTK_STOCK_APPLY);

  SetWidgetSize (close_butt, DIALOG_BUTTON_HSIZE, DIALOG_BUTTON_VSIZE);
  SetWidgetSize (apply_butt, DIALOG_BUTTON_HSIZE, DIALOG_BUTTON_VSIZE);

  g_signal_connect (close_butt,
                    "clicked",
                    G_CALLBACK (on_close_butt_clicked),
                    ThisDialog);

  g_signal_connect (apply_butt,
                    "clicked",
                    G_CALLBACK (on_apply_butt_clicked),
                    ThisDialog);

  gtk_box_pack_end (GTK_BOX (action_hbox), apply_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_box_pack_end (GTK_BOX (action_hbox), close_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);


  /* Do not set alternative button order here because we
   * replaced the action area */

  gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog), GEDA_RESPONSE_ACCEPT);

  GEDA_HOOKUP_OBJECT(ThisDialog, apply_butt, "apply-butt");

  return action_hbox;
}

/*!
 * \brief Create the Pin Properties Editor Dialog
 * \par Function Description
 *  This function creates the modaless Pin Properties Dialog, then
 *  connects callback handlers. Memory for a pin_type_data is allocated
 *  and assigned values with pointers to the input widgets, and the data
 *  structure is attached to the dialog. Sperately, a pointer to the
 *  second table containing the over-ride switch/check bottom and their
 *  associated labels is attach to the dialog.
 */
GtkWidget *x_dialog_pin_type_create_dialog(GschemToplevel *w_current)
{
  AtkObject *atk_type_obj;
  AtkObject *atk_attrib_obj;
  AtkObject *atk_num_obj;
  AtkObject *atk_seq_obj;
  AtkObject *atk_label_obj;

  GtkWidget *ThisDialog;
  GtkWidget *main_vbox;

  GtkWidget *action_area    = NULL;
  GtkWidget *optionmenu     = NULL;
  GtkWidget *number_entry   = NULL;
  GtkWidget *sequence_spin  = NULL;
  GtkWidget *label_entry    = NULL;
  GtkWidget *attributemenu  = NULL;

  GtkWidget *alignment;
  GtkWidget *label;
  GtkWidget *type_label;
  GtkWidget *attrib_label;
  GtkWidget *num_label;
  GtkWidget *seq_label;
  GtkWidget *pin_label;
  GtkTable  *table;

  GtkWidget *SetPinNodeTypeSwitch = NULL;
  GtkWidget *SetElectricalSwitch  = NULL;
  GtkWidget *AutoNumberSwitch     = NULL;
  GtkWidget *AutoSequenceSwitch   = NULL;

  const char *type_combo_tip      = _TOOLTIP(PinNodeType);
  const char *attrib_combo_tip    = _TOOLTIP(PinElectrical);
  const char *num_entry_tip       = _TOOLTIP(PinNumber);
  const char *seq_spin_tip        = _TOOLTIP(PinSequence);
  const char *label_entry_tip     = _TOOLTIP(PinLabel);

  pin_type_data *pin_data; /* Structure is allocated after widget creation */

  /* nonmodal Editing ThisDialog */
  ThisDialog = NEW_GSCHEM_DIALOG(_("Pin Properties Editor"),
                                    GSCHEM_MODELESS_DIALOG,
                                    IDS_PIN_EDIT,
                                    w_current);

  main_vbox = GTK_DIALOG (ThisDialog)->vbox;
  gtk_widget_show (main_vbox);

  table = (GtkTable*)gtk_table_new (5, 3, FALSE);
  gtk_table_set_row_spacings (table, DIALOG_V_SPACING);
  gtk_table_set_col_spacings (table, DIALOG_H_SPACING);
  geda_container_add         (main_vbox, table);
  g_object_set               (table, "visible", TRUE, NULL);

  type_label = GEDA_AVM_LABEL_NEW (_LABEL(PinNodeType), 0, 0);
  gtk_table_attach(table, type_label, 0,1,0,1, GTK_FILL,0,0,0);

  attrib_label = GEDA_AVM_LABEL_NEW (_LABEL(PinElectrical), 0, 0);
  gtk_table_attach(table, attrib_label, 0,1,1,2, GTK_FILL,0,0,0);

  num_label = GEDA_AVM_LABEL_NEW (_LABEL(PinNumber), 0, 0);
  gtk_table_attach(table, num_label, 0,1,2,3, GTK_FILL,0,0,0);

  seq_label = GEDA_AVM_LABEL_NEW (_LABEL(PinSequence), 0, 0);
  gtk_table_attach(table, seq_label, 0,1,3,4, GTK_FILL,0,0,0);

  pin_label = GEDA_AVM_LABEL_NEW (_LABEL(PinLabel), 0, 0);
  gtk_table_attach(table, pin_label, 0,1,4,5, GTK_FILL,0,0,0);

  optionmenu = geda_option_menu_new ();
  geda_option_widget_set_menu(optionmenu, create_menu_pin_type ());
  gtk_table_attach_defaults(table, optionmenu, 1,2,0,1);
  gtk_widget_set_tooltip_text(optionmenu, type_combo_tip);
  gtk_widget_show (optionmenu);

  attributemenu = geda_option_menu_new ();
  geda_option_widget_set_menu(attributemenu, create_menu_pin_electricals());

  gtk_table_attach_defaults(table, attributemenu, 1,2,1,2);
  gtk_widget_set_tooltip_text(attributemenu,  attrib_combo_tip);
  gtk_widget_show (attributemenu);

  number_entry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(number_entry), TRUE);
  gtk_table_attach_defaults(table, number_entry, 1,2,2,3);
  gtk_widget_set_tooltip_text(number_entry, num_entry_tip);
  gtk_widget_show (number_entry);

  sequence_spin = gtk_spin_button_new_with_range(1, 100000, 1);
  gtk_entry_set_activates_default(GTK_ENTRY(sequence_spin), TRUE);
  gtk_table_attach_defaults(table, sequence_spin, 1,2,3,4);
  gtk_widget_set_tooltip_text(sequence_spin, seq_spin_tip);
  gtk_widget_show (sequence_spin);

  label_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(label_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(label_entry), 0, -1);
  gtk_table_attach_defaults(table, label_entry, 1,2,4,5);
  gtk_widget_set_tooltip_text(label_entry, label_entry_tip);
  gtk_widget_show (label_entry);

  HD_SEPARATOR (main_vbox, Options);

  /* Alignment Widget - Not Text Alignment property */
  alignment = gtk_alignment_new(0, 0, 1, 1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0, DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(main_vbox), alignment, FALSE, FALSE, 0);
  gtk_widget_show (alignment);

  /* Create a second table for Switch controls */
  table = (GtkTable*)gtk_table_new (4, 5, FALSE);
  gtk_table_set_row_spacings (table, DIALOG_V_SPACING);
  gtk_table_set_col_spacings (table, DIALOG_H_SPACING);
  geda_container_add         (alignment, table);
  g_object_set               (table, "visible", TRUE, NULL);

  /* Set Label Row 1 */

  label = GEDA_AV_LABEL_NEW (_("Set"), 0, 0);
  gtk_table_attach(table, label, 0, 1, 0, 1, GTK_FILL, 0, 0, 0);

  label = GEDA_AV_LABEL_NEW (_("Auto"), 0, 0);
  gtk_table_attach(table, label, 0, 1, 2, 3, GTK_FILL, 0, 0, 0);

  /* Create the toggle switch widgets; Note the macro GSCHEM_SWITCH
   * is utilize to create the labels and switches in column 4, the
   * the labels and switches in column 1 are attached separately
   * to facilitate alignment of the switches in the table */

  /* SetPinNodeType Switch Row 2 Column 1 */

  GtkWidget *SwitchImage = NULL;
  GtkWidget *switch_hbox;

  switch_hbox = gtk_hbox_new (FALSE, 0);
  gtk_widget_show(switch_hbox);

  label = GEDA_AVM_LABEL_NEW (_LABEL(SetPinNodeType), 0, 0);
  geda_label_widget_set_justify (label, GTK_JUSTIFY_RIGHT);

  gtk_table_attach(table, label, 1, 2, 1, 2, GTK_FILL, 0, 0, 0);

  SetPinNodeTypeSwitch = create_geda_switch (switch_hbox, SwitchImage, FALSE);

  gtk_table_attach(table, switch_hbox, 2, 3, 1, 2, GTK_SHRINK, 0, 0, 0);

  /* SetElectrical Switch Row 2 Column 4 */
  GSCHEM_SWITCH(table, SetElectrical,  4, 1, FALSE)

  /* AutoNumber Switch Row 3 Column 1 */

  switch_hbox = gtk_hbox_new (FALSE, 0);
  gtk_widget_show(switch_hbox);

  label = GEDA_AVM_LABEL_NEW (_LABEL(AutoNumber), 0, 0);
  geda_label_widget_set_justify (label, GTK_JUSTIFY_RIGHT);
  gtk_table_attach(table, label, 1,2,3,4, GTK_SHRINK,0,0,0);

  AutoNumberSwitch = create_geda_switch (switch_hbox, SwitchImage, FALSE);

  gtk_table_attach(table, switch_hbox, 2, 3, 3, 4, GTK_SHRINK, 0, 0, 0);

  /* AutoSequence Switch Row 3 Column 4 */
  GSCHEM_SWITCH(table, AutoSequence,   4, 3, FALSE)

  /* Setup callback for Switch widget */
  GEDA_CALLBACK_SWITCH (SetPinNodeType,   xd_edit_pin_switch_toggled, ThisDialog)
  GEDA_CALLBACK_SWITCH (SetElectrical,    xd_edit_pin_switch_toggled, ThisDialog)
  GEDA_CALLBACK_SWITCH (AutoNumber,       xd_edit_pin_switch_toggled, ThisDialog)
  GEDA_CALLBACK_SWITCH (AutoSequence,     xd_edit_pin_switch_toggled, ThisDialog)

  GEDA_HOOKUP_OBJECT(ThisDialog, table, "over-rides");

    /** Set the relationships between the label and their Widgets **/
  geda_label_set_mnemonic_widget (GEDA_LABEL (type_label),   optionmenu);
  geda_label_set_mnemonic_widget (GEDA_LABEL (attrib_label), attributemenu);
  geda_label_set_mnemonic_widget (GEDA_LABEL (num_label),    number_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL (seq_label),    sequence_spin);
  geda_label_set_mnemonic_widget (GEDA_LABEL (pin_label),    label_entry);

  atk_type_obj   = atk_widget_linked_label_new (type_label,   optionmenu);
  atk_attrib_obj = atk_widget_linked_label_new (attrib_label, attributemenu);
  atk_num_obj    = atk_widget_linked_label_new (num_label,    number_entry);
  atk_seq_obj    = atk_widget_linked_label_new (seq_label,    sequence_spin);
  atk_label_obj  = atk_widget_linked_label_new (pin_label,    label_entry);

  if ( atk_type_obj ) {
    atk_object_set_name        ( atk_type_obj,   _("Attribute Type List"));
    atk_object_set_description ( atk_type_obj,      type_combo_tip);
  }
  if ( atk_attrib_obj ) {
    atk_object_set_name        ( atk_attrib_obj, _("Text Alignment Option"));
    atk_object_set_description ( atk_attrib_obj,    attrib_combo_tip);
  }
  if ( atk_num_obj ) {
    atk_object_set_name        ( atk_num_obj,    _("Pin Number"));
    atk_object_set_description ( atk_num_obj,       num_entry_tip);
  }
  if ( atk_seq_obj ) {
    atk_object_set_name        ( atk_seq_obj,    _("Sequence Number"));
    atk_object_set_description ( atk_seq_obj,       seq_spin_tip);
  }
  if ( atk_label_obj ) {
    atk_object_set_name        ( atk_label_obj,  _("Pin Label"));
    atk_object_set_description ( atk_label_obj,     label_entry_tip);
  }

  /* Allocate memory for a structure to hold pointers to our Widgets */
  pin_data = (pin_type_data*) GEDA_MEM_ALLOC (sizeof(struct st_pin_type_data));

  /* populate the data structure */
  pin_data->node_type      = optionmenu;
  pin_data->pin_electrical = attributemenu;
  pin_data->number_entry   = number_entry;
  pin_data->sequence_spin  = sequence_spin;
  pin_data->label_entry    = label_entry;

  pin_data->set_node_type  = SetPinNodeTypeSwitch;
  pin_data->set_elect_type = SetElectricalSwitch;
  pin_data->auto_number    = AutoNumberSwitch;
  pin_data->auto_sequence  = AutoSequenceSwitch;

  /* fill in the fields of the dialog */
  x_dialog_edit_pin_type_set_values(pin_data, NULL, NULL, 1, PIN_ELECT_PAS, PIN_MECH_LEAD, PIN_NET_NODE);

  GEDA_OBJECT_SET_DATA(ThisDialog, pin_data, IDS_PIN_EDIT);

  HD_ACTION_SEPARATOR (main_vbox);

  action_area = create_action_area (GSCHEM_DIALOG(ThisDialog),
                                   (GtkWidget*) main_vbox);

  gtk_widget_show_all (action_area);

  g_signal_connect (number_entry, "activate",
                    G_CALLBACK (on_apply_butt_clicked),
                    ThisDialog);

  g_signal_connect (sequence_spin, "activate",
                    G_CALLBACK (on_apply_butt_clicked),
                    ThisDialog);

  g_signal_connect (label_entry, "activate",
                    G_CALLBACK (on_apply_butt_clicked),
                    ThisDialog);

  g_signal_connect (G_OBJECT (ThisDialog), "response",
                    G_CALLBACK (x_dialog_edit_pin_type_response),
                    pin_data);

  g_object_set (G_OBJECT (ThisDialog), DIALOG_SELECTION_TRACKER,
                xd_pin_type_update_selection,
                NULL);

  return ThisDialog;
}

/*!
 * \brief Pin Properties Editor Dialog - Main Entry
 * \par Function Description
 *  This function initiates or activates the Pin Properties Dialog
 *  for manipulating the properties of pins objects.
 */
void x_dialog_edit_pin_type (GschemToplevel *w_current)
{
  GtkWidget  *ThisDialog;
  GedaObject *object;

  ThisDialog = w_current->ptwindow;
  if (!ThisDialog) {

    ThisDialog = x_dialog_pin_type_create_dialog(w_current);

    gtk_window_set_position(GTK_WINDOW (ThisDialog), GTK_WIN_POS_MOUSE);
    gtk_window_set_transient_for (GTK_WINDOW(ThisDialog), w_current->main_window);

    w_current->ptwindow = ThisDialog;

    gtk_widget_show (ThisDialog);

  }
  else { /* dialog already created */
    gtk_window_present (GTK_WINDOW(ThisDialog));
  }

  object = o_select_return_first_object(w_current);
  xd_pin_type_update_selection (w_current, object);
}

/****************** End of pin type edit dialog box *********************/
/** @} end group Edit-Pin-Dialog */