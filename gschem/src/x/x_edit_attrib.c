/* -*- C x_edit_attrib.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file x_edit_attrib.c
 * \brief A dialog box for editing Attribute Text objects
 */

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"
#include <geda/geda_gui_funcs.h>
#include <geda_widgets.h>
#include <geda_debug.h>

/***************** Start of Attrib Edit dialog box ********************/

/** \defgroup Single-Attrib-Edit-Dialog Single-Attribute Editor Dialog
 *  @{
 *  \ingroup Editing-Dialogs
 *
 *  \par This group contains routines for the Single-Attribute Editor dialog.
 *
 *  \remarks The Single Attribute dialog doubles as the Add Attribute Dialog
 *           based on a flag passed to the constructor.
 *
 *  \image html single_attribute_dialog.png
 *  \image latex single_attribute_dialog.png
 */

typedef enum
{
  /* This dialog is used for both edit and creating new attributes */
  SAE_EDIT_MODE,
  SAE_ADD_MODE
} AttributeEditMode;

/*!
 * \brief Callback for Editing Text Properties
 * \par Function Description
 *  This function updates widgets on the attrib_edit dialog with the text
 *  properties of the passed object. If multiple objects are selected the
 *  text editing field is set to NULL.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 * \param [in] object    Pointer to a selected object or NULL
 */
static void
x_dialog_attrib_edit_update_selection (GschemToplevel *w_current,
                                       GedaObject     *object)
{
  GtkWidget *ThisDialog = w_current->aewindow;

  if (object != NULL) {

    GtkEntry *entry;

    if (object->type == OBJ_TEXT) {

      GtkToggleButton *button;
      GedaOptionMenu  *optionmenu;

      char *name = NULL;
      char *val  = NULL;

      button = GEDA_OBJECT_GET_DATA (ThisDialog, "visbutton");

      /* Update the visibility button widget*/
      if (geda_object_get_visibility(object) != VISIBLE) {
        gtk_toggle_button_set_active(button, FALSE);
      }
      else {
        gtk_toggle_button_set_active(button, TRUE);
      }

      /* Update Show Options */
      optionmenu = GEDA_OBJECT_GET_DATA (ThisDialog, "show_options");

      if (object->show_name_value == SHOW_VALUE) {
        geda_option_menu_set_history (optionmenu, 0);
      }
      else if (object->show_name_value == SHOW_NAME) {
        geda_option_menu_set_history (optionmenu, 1);
      }
      else {
        geda_option_menu_set_history (optionmenu, 2);
      }

      /* Get the attribute name and value string components */
      geda_attrib_object_get_name_value (object, &name, &val);

      /* Update the Value Entry */
      entry = GEDA_OBJECT_GET_DATA (ThisDialog, "value_entry");
      if (val) {
        SetEntryText   (entry, val);
        EntrySelectAll (entry);
      }

      entry = GEDA_OBJECT_GET_DATA (ThisDialog, "attrib_name_entry");
      SetEntryText (entry, name);

      GEDA_FREE (name);
      GEDA_FREE (val);

    }
    else if (object->type == OBJ_NET) {

      entry = GEDA_OBJECT_GET_DATA (ThisDialog, "attrib_name_entry");
      SetEntryText (entry, "netname");

      entry = GEDA_OBJECT_GET_DATA (ThisDialog, "value_entry");
      gtk_widget_grab_focus((GtkWidget*)entry);
    }
  }

  GEDA_OBJECT_SET_DATA (ThisDialog, object, "attrib");
}

/*!
 * \brief Get Show Name Value on Single Attribute Editor Dialog
 * \par Function Documentation
 *  Helper for attrib_edit_dialog_ok to retrieve the index of the
 *  active widget in the \a option_menu widget list of children.
 *  The choices are show value, show name or show name and value.
 *
 * \param [in] option_menu Pointer the show_options widget
 *
 * \returns active index or -1 in the unlikely event there is no active widget.
 */
int option_menu_get_history (GedaOptionMenu *option_menu)
{
  GtkWidget *active_widget;

  g_return_val_if_fail (GEDA_IS_OPTION_MENU (option_menu), -1);

  active_widget = geda_menu_get_active (GEDA_MENU (option_menu->menu));

  if (active_widget) {
    return g_list_index ((GList*)
             geda_menu_shell_get_children(GEDA_MENU_SHELL(option_menu->menu)),
                                          active_widget);
  }
  else {
    return -1;
  }
}

/*!
 * \brief Single Attribute Editor Dialog Response Handler
 * \par Function Documentation
 *  This function handles signals generated by the button widgets
 *  in the dialog's action area. There are only two signal, one for
 *  "apply" and one to "close" the dialog.
 *
 * \param mode      flag to indicate create mode or edit an existing
 * \param w_current is pointer to a GschemToplevel structure
 */
static void
attrib_edit_dialog_ok(AttributeEditMode mode, GschemToplevel *w_current)
{
  GtkWidget *ThisDialog;
  GtkEntry   *value_entry;
  GtkEntry   *name_entry;
  GtkWidget  *visbutton;
  GtkWidget  *show_options;
  GedaObject *object;
  char       *label;
  char       *newtext;
  char       *value;

  int vis, show, option_index;

  ThisDialog = w_current->aewindow;

  value_entry  = GEDA_OBJECT_GET_DATA (ThisDialog, "value_entry");
  name_entry   = GEDA_OBJECT_GET_DATA (ThisDialog, "attrib_name_entry");
  visbutton    = GEDA_OBJECT_GET_DATA (ThisDialog, "visbutton");
  show_options = GEDA_OBJECT_GET_DATA (ThisDialog, "show_options");

  value   = geda_strtrim (GetEntryText(value_entry));
  label   = geda_strtrim (GetEntryText(name_entry));
  newtext = geda_strconcat (label, "=", value, NULL);

  GEDA_FREE(label);
  GEDA_FREE(value);

  if (!x_dialog_validate_attribute(GTK_WINDOW(ThisDialog), newtext))
  {
    GEDA_FREE(newtext);
    return;
  }

  if (GetToggleState(visbutton))
    vis = VISIBLE;
  else
    vis = INVISIBLE;

  option_index = option_menu_get_history(GEDA_OPTION_MENU(show_options));

  switch(option_index) {
    case(0):
      show = SHOW_VALUE;
      break;

    case(1):
      show = SHOW_NAME;
      break;

    case(2):
      show = SHOW_NAME_VALUE;
      break;

    default:
      fprintf(stderr, _("invalid show option; defaulting to show both\n"));
      show = SHOW_NAME_VALUE;
      break;
  }

  if (mode == SAE_ADD_MODE) {

    GedaObject *new = NULL;
    int color;

    object = o_select_return_first_object(w_current);

    if (strncmp(newtext, "pinlabel", 8) == 0){
      color = TEXT_COLOR;
    }
    else {
      color = -1;
    }

    new = o_attrib_add_attrib(w_current, newtext, vis, show, color, object);

    if (w_current->first_wx != -1 && w_current->first_wy != -1) {

      o_invalidate_object (w_current, new);

      new->text->x = w_current->first_wx;
      new->text->y = w_current->first_wy;
    }

    geda_text_object_recreate(new);
    o_undo_savestate(w_current, UNDO_ALL);

  }
  else { /* Editing an existing Attribute */

    object =  GEDA_OBJECT_GET_DATA (ThisDialog, "attrib");

    if (object != NULL && object->type == OBJ_TEXT) {

      int tmp_vis;

      tmp_vis = geda_object_get_visibility(object);

      /* This helps preserves "temporary" visibility states */
      if ( !vis && tmp_vis != vis) {
        vis = tmp_vis;
      }

      o_text_change(w_current, object, newtext, vis, show);
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
  GEDA_FREE(newtext);
}

/*!
 * \brief Response function for the attribute add/edit dialog
 * \par Function Description
 *  This function catches the user response for the add and edit
 *  attribute dialog.
 *
 * \param [in] widget    Either the Close or the Apply Button widget
 * \param [in] response  integer response associated with widget
 * \param [in] w_current Pointer to a GschemToplevel object
 */
static void
attribute_edit_dialog_response(GtkWidget *widget, int response,
                               GschemToplevel *w_current)
{
  GtkWidget *ThisDialog;
  ThisDialog = w_current->aewindow;

  switch(response) {
    case GEDA_RESPONSE_APPLY:
      attrib_edit_dialog_ok (SAE_EDIT_MODE, w_current);
      break;

    case GEDA_RESPONSE_ACCEPT:
      attrib_edit_dialog_ok (SAE_ADD_MODE, w_current);
      gtk_grab_remove(ThisDialog);
      break;

    case GEDA_RESPONSE_REJECT:
    case GEDA_RESPONSE_DELETE_EVENT:
      gtk_widget_destroy(ThisDialog);
      break;

    default:
      BUG_IMSG ("unhandled case", response);
  }
}

/*!
 * \brief Move Focus when Enter pressed in Name Entry
 * \par Function Description
 *  This function is call when the ENTER button is press
 *  in Attribute Name entry, the function sets focus to the
 *  Value Entry.
 *
 * \param [in] widget      Pointer to the Name GedaEntry widget
 * \param [in] value_entry Pointer to the Value GtkEntry widget
 */
static void
callback_attrib_entry_activate (GtkWidget *widget, GtkWidget *value_entry)
{
  if (GTK_IS_ENTRY(value_entry)) {
    gtk_widget_grab_focus(value_entry);
  }
}

static GtkWidget *x_attrib_option_menu_new(void)
{
  GtkWidget  *options_menu;
  GtkWidget  *show_options_menu;
  GtkWidget  *menuitem;

  const char *options_menu_tip;
  const char *options_name_tip;
  const char *options_value_tip;
  const char *options_both_tip;

  options_menu_tip  = _("Select to choose an attribute visibility options");
  options_name_tip  = _("Show only the name of the attribute");
  options_value_tip = _("Show only the value of the attribute");
  options_both_tip  = _("Show both the name and the value of the attribute");

  options_menu = geda_option_menu_new ();
  g_object_set (options_menu, "visible", TRUE, NULL);

  show_options_menu = geda_menu_new ();
  gtk_widget_set_tooltip_text (options_menu, options_menu_tip);

  menuitem = geda_menu_item_new_with_label (_("Show Value Only"));
  geda_menu_append (show_options_menu, menuitem);
  gtk_widget_set_tooltip_text (menuitem, options_name_tip);

  menuitem = geda_menu_item_new_with_label (_("Show Name Only"));
  geda_menu_append (show_options_menu, menuitem);
  gtk_widget_set_tooltip_text (menuitem, options_value_tip);

  menuitem = geda_menu_item_new_with_label (_("Show Name & Value"));
  geda_menu_append (show_options_menu, menuitem);
  gtk_widget_set_tooltip_text (menuitem, options_both_tip);

  geda_option_widget_set_menu (options_menu, show_options_menu);
  geda_option_widget_set_history (options_menu, 0);

  return options_menu;
}

/*!
 * \brief Create the attribute add/edit dialog
 * \par Function Description
 *  This function creates the single attribute edit dialog. This dialog
 *  is special in that it can be either an "add" new attribute or an
 *  "Edit" (existing) attribute dialog depending on the value of the
 *  flag argument.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 * \param [in] object    Currently select object or NULL
 * \param [in] flag      Enumerated AttributeEditMode mode flag
 */
static void
attrib_edit_dialog (GschemToplevel *w_current, GedaObject *object, int flag)
{
  GtkWidget *ThisDialog = w_current->aewindow;

  if (!ThisDialog) {

    AtkObject *atk_name_obj;
    AtkObject *atk_value_obj;

    GtkWidget *alignment, *label, *vbox;
    GtkWidget *name_label, *value_label;
    GtkWidget *show_options;
    GtkWidget *attrib_name_combo_box;
    GtkWidget *attrib_name_entry;
    GtkWidget *value_entry;
    GtkWidget *visbutton;
    GtkTable  *table;

    GList     *focus_chain; /* Aka Tab Order */

    GtkEntryCompletion *completion;
    GtkTreeModel       *tree_model;
    GtkResponseType     response;
    GschemDialogFlags   dialog_flags;

    /* gschem specific */
    char       *string = NULL;
    int i;

    const char *name_label_text;
    const char *name_label_text_add;
    const char *name_label_text_edit;

    const char *name_entry_tip;
    const char *name_list_tip;
    const char *name_list_add_tip;
    const char *name_list_edit_tip;
    const char *value_entry_tip;
    const char *value_entry_add_tip;
    const char *value_entry_edit_tip;
    const char *visibility_tip;

    name_entry_tip       = _("Enter or type an attribute name"); /* common */
    name_label_text_add  = _("Add Attribute");
    name_label_text_edit = _("Edit Attribute");

    name_list_add_tip    = _("Select the name of the attribute to add");
    name_list_edit_tip   = _("Select the name of the attribute to edit");

    value_entry_add_tip  = _("Input a value for the new attribute");
    value_entry_edit_tip = _("Input or edit the value of the attribute");

    visibility_tip       = _("Enable or disable visibility of the attribute");

    if (SAE_ADD_MODE == flag) {
      name_label_text    = name_label_text_add;
      name_list_tip      = name_list_add_tip;
      value_entry_tip    = value_entry_add_tip;
      response           = GEDA_RESPONSE_ACCEPT;
      dialog_flags       = GSCHEM_DIALOG_MODAL;
    }
    else {
      name_label_text    = name_label_text_edit;
      name_list_tip      = name_list_edit_tip;
      value_entry_tip    = value_entry_edit_tip;
      response           = GEDA_RESPONSE_APPLY;
      dialog_flags       = GSCHEM_MODELESS_DIALOG;
    }

    ThisDialog = gschem_dialog_new_with_buttons(_("Single Attribute Editor"),
                                                w_current->main_window,
                                                dialog_flags,
                                                IDS_SINGLE_ATTRIR, w_current,
                                                GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                                GTK_STOCK_APPLY, response,
                                                NULL);

    vbox = GTK_DIALOG(ThisDialog)->vbox;

    /* Main Body Label */
    label = geda_aligned_bold_label_new(name_label_text, 0, 0);
    geda_label_set_use_markup (GEDA_LABEL(label), TRUE);
    geda_container_add (vbox, label);

    /* Create alignment widget for main body and add to vbox */
    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              DIALOG_INDENTATION, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

    /* Create the "body" table and add to the alignment widget */
    table = (GtkTable*)gtk_table_new (3, 2, FALSE);
    gtk_table_set_row_spacings(table, DIALOG_V_SPACING);
    gtk_table_set_col_spacings(table, DIALOG_H_SPACING);
    geda_container_add (alignment, (GtkWidget*)table);

    /* Name selection */
    name_label = geda_aligned_label_new (_("Name:"), 0, 0.5);
    gtk_table_attach (table, name_label, 0, 1, 0, 1,
                      (GtkAttachOptions) (GTK_FILL),
                      (GtkAttachOptions) (GTK_FILL), 0, 0);

    attrib_name_combo_box = geda_combo_box_text_new_with_entry ();

    gtk_table_attach (table, attrib_name_combo_box, 1, 2, 0, 1,
                      (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                      (GtkAttachOptions) (0), 0, 0);
    geda_combo_box_set_focus_on_click (GEDA_COMBO_BOX(attrib_name_combo_box), FALSE);
    gtk_widget_set_tooltip_text (attrib_name_combo_box, name_list_tip);

    attrib_name_entry = geda_combo_widget_get_entry_widget(attrib_name_combo_box);
    gtk_widget_set_tooltip_text (attrib_name_entry, name_entry_tip);

    /* Value entry */
    value_label = geda_aligned_label_new (_("Value:"), 0, 0.5);
    gtk_table_attach (table, value_label, 0, 1, 1, 2,
                      (GtkAttachOptions) (GTK_FILL),
                      (GtkAttachOptions) (0), 0, 0);

    value_entry = gtk_entry_new ();

    gtk_table_attach (table, value_entry, 1, 2, 1, 2,
                      (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                      (GtkAttachOptions) (0), 0, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(value_entry), TRUE);
    gtk_widget_set_tooltip_text (GTK_WIDGET(value_entry), value_entry_tip);

    /* Visibility */
    visbutton = gtk_check_button_new_with_label (_("Visible"));
    SetToggleState (visbutton, TRUE);
    gtk_table_attach (table, visbutton, 0, 1, 2, 3,
                      (GtkAttachOptions) (GTK_FILL),
                      (GtkAttachOptions) (0), 0, 0);
    gtk_widget_set_tooltip_text (GTK_WIDGET(visbutton), visibility_tip);

    show_options = x_attrib_option_menu_new();

    gtk_table_attach (table, show_options, 1, 2, 2, 3,
                      (GtkAttachOptions) (GTK_FILL | GTK_EXPAND),
                      (GtkAttachOptions) (0), 0, 0);

    focus_chain = NULL;
    focus_chain = g_list_append (focus_chain, attrib_name_combo_box);
    focus_chain = g_list_append (focus_chain, value_entry);
    focus_chain = g_list_append (focus_chain, visbutton);
    focus_chain = g_list_append (focus_chain, show_options);
    geda_container_set_focus_chain (table, focus_chain);
    g_list_free (focus_chain);

    /** Set the relationships between the label and their Widgets **/
    geda_label_set_mnemonic_widget (GEDA_LABEL (name_label),  attrib_name_entry);
    geda_label_set_mnemonic_widget (GEDA_LABEL (value_label), value_entry);

    atk_name_obj  = atk_widget_linked_label_new (name_label,  attrib_name_entry);
    atk_value_obj = atk_widget_linked_label_new (value_label, value_entry);

    if ( atk_name_obj ) {
      atk_object_set_name        (atk_name_obj,  _("Attribute Name List"));
      atk_object_set_description (atk_name_obj,     name_list_tip);
    }
    if ( atk_value_obj ) {
      atk_object_set_name        (atk_value_obj, _("Attribute Value Entry"));
      atk_object_set_description (atk_value_obj,    value_entry_tip);
    }

    if (!object) {
      SetToggleState(visbutton, TRUE);
      /* show value only */
      geda_option_menu_set_history (GEDA_OPTION_MENU (show_options), 0);
    }

    GEDA_OBJECT_SET_DATA(ThisDialog, (void*)(long)(flag), "mode_flag");

    /* load the combo's tree with our list of attributes names */
    i = 0;
    string = (char*) geda_struct_attrib_get(i);
    while (string != NULL) {
      geda_combo_widget_append_text(attrib_name_combo_box, string);
      i++;
      string = (char*) geda_struct_attrib_get(i);
    }

    /* Add completion to attribute combo box entry */
    completion = gtk_entry_completion_new();
    tree_model = geda_combo_widget_get_model (attrib_name_combo_box);

    gtk_entry_completion_set_model(completion, tree_model);
    gtk_entry_completion_set_text_column(completion, 0);
    gtk_entry_completion_set_inline_completion(completion, TRUE);
    gtk_entry_completion_set_inline_selection (completion, TRUE);
    gtk_entry_completion_set_popup_single_match(completion, TRUE);

    gtk_entry_set_completion(GTK_ENTRY(attrib_name_entry), completion);

    GEDA_HOOKUP_OBJECT(ThisDialog, attrib_name_entry, "attrib_name_entry");
    GEDA_HOOKUP_OBJECT(ThisDialog, value_entry,       "value_entry");
    GEDA_HOOKUP_OBJECT(ThisDialog, visbutton,         "visbutton");
    GEDA_HOOKUP_OBJECT(ThisDialog, show_options,      "show_options");

    /* Connect Attribute Name Combo Entry widget in order to move focus
     * to the Value entry after changing the Attribute Name */
    g_signal_connect (attrib_name_entry, "activate",
                      G_CALLBACK (callback_attrib_entry_activate),
                      value_entry);

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (attribute_edit_dialog_response),
                      w_current);

    gtk_window_set_position ((GtkWindow*)ThisDialog, GTK_WIN_POS_MOUSE);

    gtk_widget_show_all(ThisDialog);

    if (SAE_ADD_MODE == flag) {
      gtk_widget_grab_focus(attrib_name_entry);
      gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                      GEDA_RESPONSE_ACCEPT);
    }
    else {
      gtk_widget_grab_focus(value_entry);
      gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                      GEDA_RESPONSE_APPLY);
    }

    /* Tell our inherited on-selection change callback handler which
     * function to use */
    g_object_set (ThisDialog, DIALOG_SELECTION_TRACKER,
                  x_dialog_attrib_edit_update_selection, NULL);

    if (!object) {
      gtk_grab_add(ThisDialog);
    }

    w_current->aewindow = ThisDialog;
  }
  else { /* dialog already there */
    gtk_window_present((GtkWindow*)ThisDialog);
  }

  x_dialog_attrib_edit_update_selection (w_current, object);
}

/*!
 * \brief Launch the Single Attribute Dialog in Add mode
 * \par Function Description
 *  This function calls the main dialog routine to construct and
 *  or present the single attribute editor in #SAE_ADD_MODE mode,
 *  this mode is used to add new attributes to objects.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 * \param [in] object    Pointer to a selected object or NULL
 *
 * \image html single_attribute_add.png
 * \image latex single_attribute_add.png
 */
void x_attrib_add_dialog (GschemToplevel *w_current, GedaObject *object)
{
  attrib_edit_dialog (w_current, object, SAE_ADD_MODE);
}

/*!
 * \brief Launch the Single Attribute Dialog in Edit mode
 * \par Function Description
 *  This function calls the main dialog routine to construct and
 *  display the single attribute editor in #SAE_EDIT_MODE mode,
 *  this mode is used to edit existing attributes of objects.
 *
 * \param [in] w_current Pointer to a GschemToplevel object
 * \param [in] object    Pointer to a selected object or NULL
 *
 * \image html single_attribute_edit.png
 * \image latex single_attribute_edit.png
 */
void x_attrib_edit_dialog (GschemToplevel *w_current, GedaObject *object)
{
  attrib_edit_dialog (w_current, object, SAE_EDIT_MODE);
}

#undef ThisDialog
/***************** End of Attrib Edit dialog box **********************/
/** @} endgroup Single-Attrib-Edit-Dialog  */
