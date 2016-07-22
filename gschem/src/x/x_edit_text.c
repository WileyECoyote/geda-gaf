/* -*- C x_edit_text.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
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
 * \file x_edit_text.c
 * \brief A dialog box for creating and editing Text Objects.
 */

#include <gdk/gdk.h>

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"

#include <geda_widgets.h>
#include <geda_debug.h>

/** \defgroup Edit-Text-Dialog Edit Text Editing-Dialog Functions
 *  @{
 *  \ingroup (Editing-Dialogs)
 *
 *  \par This Group contains Routines for the Edit Text dialog.
 */

/** @brief Enumerate Control IDs. */
typedef enum {
/* Combo Entry */
  TheText,

/* Combo Chooser */
  TextAlign,
  TextColor,
  TextFont,
  Rotation,

/* Switches */
  ScopeOverwrite

} ControlID;

/** @brief String Arrays for Dialog Contrls.
 *  { "Hook-Up-String", "Label", "Tooltip string"},
 */
static WidgetStringData DialogStrings[] = {
  { "text_string",     "Text Tring",     "Enter or edit the text string"},
  { "text_align",      "A_lignment:",    "Select the text alignment attribute"},
  { "text_color",      "_Color:",        "Select color attribute"},
  { "fontbutton",      "_Size:",         "Open font selection dialog"},
  { "rotation",        "_Rotation:",     "Set the text rotation angle"},
  { NULL, NULL, NULL},
};

/*!
 * \brief Callback for Editing Text Properties
 * \par Function Description
 *  This function updates widgets on the text_edit dialog with the text
 *  properties of the passed object. If multible objects are selected
 *  the text editing field is set to NULL.
 *
 */
static void x_dialog_text_edit_update_selection (GschemToplevel *w_current,
                                                 GedaObject     *object)
{
  if (object != NULL && object->type == OBJ_TEXT) {

    GtkWidget     *ThisDialog;
    GtkWidget     *widget;

    char *string;

    bool match_string = TRUE;
    bool match_align  = TRUE;
    bool match_color  = TRUE;
    bool match_size   = TRUE;
    bool match_angle  = TRUE;

    GdkColor gray; //   { 0, 0x8888, 0x8888, 0x8888 };
    gdk_color_parse("gray", &gray);

    ThisDialog = w_current->tewindow;

    int num_selected;
    int text_align;
    int text_angle;
    int text_color;
    int text_size;

    string      = object->text->string;
    text_align  = object->text->alignment;
    text_color  = object->color;
    text_size   = object->text->size;
    text_angle  = object->text->angle;

    GList *iter  = NULL;
    iter         = geda_list_get_glist(Current_Selection);
    num_selected = g_list_length(iter);

    if (num_selected > 1) {

      for ( ;  iter != NULL; iter = g_list_next(iter)) {

        GedaObject *o_current = iter->data;

        if (o_current->type == OBJ_TEXT) {
          if (strcmp(o_current->text->string, string) != 0)
            match_string = FALSE;

          if (o_current->text->alignment != text_align)
            match_align = FALSE;

          if (o_current->color != text_color)
            match_color = FALSE;

          if (o_current->text->size != text_size)
            match_size = FALSE;

          if (o_current->text->angle != text_angle)
            match_angle = FALSE;
        }
      }
    }

    /* Grey non matching values */
    {
      widget = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_TEXT_EDIT);

      if (GTK_IS_TEXT_VIEW (widget)) {

        GtkTextBuffer *textbuffer;

        textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget));

        if (match_string)
          gtk_text_buffer_set_text (GTK_TEXT_BUFFER (textbuffer), string, -1);
        else
          gtk_text_buffer_set_text (GTK_TEXT_BUFFER (textbuffer), "", -1);

        /* if only one object then wipe all text for user */
        if (num_selected == 1) {
          select_all_text_in_textview (GTK_TEXT_VIEW (widget));
        }
      }
    }

    { /* Text Font Size */
      widget = GEDA_OBJECT_GET_DATA (ThisDialog, WIDGET(TextFont));
      if (GEDA_IS_FONT_BUTTON(widget)) {
        if (match_size) {
          gtk_widget_set_can_default (widget, TRUE);
          geda_font_button_set_size ((GedaFontButton*)widget, text_size);
        }
        else {
          gtk_widget_set_can_default (widget, FALSE);
        }
      }
    }

    { /* Text Color */
      widget = GEDA_OBJECT_GET_DATA (ThisDialog, WIDGET(TextColor));
      if (match_color) {
        geda_combo_widget_set_active (widget, text_color);
      }
      else {
        geda_combo_widget_set_active (widget, -1);
        gtk_widget_modify_fg (widget, GTK_STATE_NORMAL, &gray);
      }
    }

    { /* Text Alignment */
      widget = GEDA_OBJECT_GET_DATA (ThisDialog, WIDGET(TextAlign));
      if (match_align) {

        /* Lookup table for translating between alignment values and the combo
         * box list indices, index is alignment value, value is list index */
        int alignment_lookup[] = {6, 3, 0, 7, 4, 1, 8, 5, 2};

        geda_combo_widget_set_active(widget, alignment_lookup[text_align]);
      }
      else {
        gtk_widget_modify_fg (widget, GTK_STATE_NORMAL, &gray);
      }
    }

    { /* Text Rotation */
      widget = GEDA_OBJECT_GET_DATA (ThisDialog, WIDGET(Rotation));
      if (match_angle) {
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(widget), text_angle);
        gtk_widget_set_can_default (widget, TRUE);
      }
      else {
        SetEntryText( widget, "");
        gtk_widget_set_can_default (widget, FALSE);
        gtk_entry_set_activates_default(GTK_ENTRY(widget), FALSE);
      }
    }
  }

  return;
}

/*!
 * \brief Create alignment combo box list store for the text property dialog
 * \par Function Description
 *  This function creates a GtkListStore with nine different alignment
 *  entries.
 */
static GtkListStore *create_menu_alignment (GschemToplevel *w_current)
{
  GtkListStore *store;
  GtkTreeIter   iter;

  store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_INT);

  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Upper Left"), -1);
  gtk_list_store_set(store, &iter, 1, 2, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Upper Middle"), -1);
  gtk_list_store_set(store, &iter, 1, 5, -1);
  gtk_list_store_append( store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Upper Right"), -1);
  gtk_list_store_set(store, &iter, 1, 8, -1);

  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Middle Left"), -1);
  gtk_list_store_set(store, &iter, 1, 1, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Middle Middle"), -1);
  gtk_list_store_set(store, &iter, 1, 4, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Middle Right"), -1);
  gtk_list_store_set(store, &iter, 1, 7, -1);

  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Lower Left"), -1);
  gtk_list_store_set(store, &iter, 1, 0, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Lower Middle"), -1);
  gtk_list_store_set(store, &iter, 1, 3, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Lower Right"), -1);
  gtk_list_store_set(store, &iter, 1, 6, -1);

  return store;
}

/*!
 * \brief Apply the settings from the text property dialog
 * \par Function Description
 *  This function retrieve the user settings to the selected text objects
 *  and closes the dialog
 */
void x_dialog_edit_text_ok(GschemToplevel *w_current, GedaObject *object)
{
  GtkWidget     *ThisDialog;

  GtkTextIter    start, end;
  GtkTreeIter    iter;
  GtkTreeModel  *model;
  GtkWidget     *widget;

  bool           has_value;

  char          *string;
  const char    *fontname;

  int            text_align;
  int            text_color;
  int            text_size;
  int            text_angle;

  ThisDialog = w_current->tewindow;

  /* Retrieve values from the object that was passed */
  string         = object->text->string;
  text_align     = -1;
  text_color     = object->color;
  text_size      = object->text->size;
  text_angle     = object->text->angle;

  fontname = NULL;

  /* text string entry will only show up if one object is selected */
  {
    GtkTextBuffer *textbuffer;

    widget = GEDA_OBJECT_GET_DATA (ThisDialog, IDS_TEXT_EDIT);
    textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget));
    gtk_text_buffer_get_bounds (textbuffer, &start, &end);
    string = gtk_text_iter_get_text (&start, &end);
  }

  { /* Text Size */
    widget = GEDA_OBJECT_GET_DATA (ThisDialog, WIDGET(TextFont));
    has_value = gtk_widget_get_can_default (widget);
    if (has_value) {
      fontname = geda_font_button_get_font_name(GEDA_FONT_BUTTON (widget));
      text_size = geda_font_button_get_size ((GedaFontButton*)widget);
    }
    else
      text_size = -1;
  }

  { /* Text Color */
    widget = GEDA_OBJECT_GET_DATA (ThisDialog, WIDGET(TextColor));
    text_color = geda_combo_widget_get_active(widget);
    if (text_color > 0) {
      if (geda_combo_widget_get_active_iter(widget, &iter)) {
        model = geda_combo_box_get_model((GedaComboBox*) widget);
        gtk_tree_model_get(model, &iter, 1, &text_color, -1);
      }
    }
  }

  { /* Text Alignment */
    widget = GEDA_OBJECT_GET_DATA (ThisDialog, WIDGET(TextAlign));
    has_value = geda_combo_widget_get_active(widget);
    if (has_value >= 0) {
      if (geda_combo_widget_get_active_iter(widget, &iter)) {
        model = geda_combo_box_get_model((GedaComboBox*) widget);
        gtk_tree_model_get(model, &iter, 1, &text_align, -1);
      }
    }
  }

  { /* Text Rotation */
    widget = GEDA_OBJECT_GET_DATA (ThisDialog, WIDGET(Rotation));
    has_value = gtk_widget_get_can_default (widget);
    if (has_value)
      text_angle = GET_SPIN_IVALUE(widget);
    else
      text_angle = -1;
  }

  fontname = fontname; /* stub */
  o_text_edit_end(w_current, string, text_align, text_color, text_size, text_angle);
  GEDA_FREE(string);
}

/*!
 * \brief Response function for the text property dialog
 * \par Function Description
 *  This function receives the user response of the text property dialog.
 *  The response is either <b>OK</b>, <b>Cancel</b> or delete.
 *
 */
void
x_dialog_edit_text_response(GtkWidget *Dialog, int response, GedaObject *object)
{
  GschemToplevel *w_current;

  w_current = GSCHEM_DIALOG (Dialog)->w_current;

  switch(response) {
  case GEDA_RESPONSE_ACCEPT:
    x_dialog_edit_text_ok(w_current, object);
    i_status_set_state(w_current, SELECT);
    break;
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(Dialog);
    break;
  default:
    BUG_IMSG ("unhandled case for signal <%d>", response);
  }
}

/*!
 * \brief Callback Widget Values Changed by User.
 * \par Function Description
 *  This function changes the wrap property to indicate that the
 *  user has enter a value.
 *
 */
static void
widget_value_modified (GtkWidget *widget, void * user_data)
{
  gtk_widget_set_can_default (widget, TRUE);
}

/*!
 * \brief Create the edit text properties dialog
 * \par Function Description
 *  This Function creates the dialog to edit text properties.
 */
void x_dialog_edit_text (GschemToplevel *w_current, GedaObject *text_object)
{
  GtkWidget *ThisDialog = w_current->tewindow;

  if (!ThisDialog) {

    AtkObject *atk_text_obj;
    AtkObject *atk_align_obj;
    AtkObject *atk_color_obj;
    AtkObject *atk_font_obj;
    AtkObject *atk_rotate_obj;

    GtkWidget *text_label;
    GtkWidget *color_label;
    GtkWidget *font_label;
    GtkWidget *align_label;
    GtkWidget *rotate_label;
    GtkWidget *label;

    GtkWidget *table;
    GtkWidget *vbox;
    GtkWidget *optionmenu;
    GtkWidget *combobox;
    GtkWidget *viewport1;
    GtkWidget *textentry;
    GtkWidget *font_button;
    GtkWidget *alignment;
    GtkWidget *RotationSpin;
    GtkWidget *scrolled_window;

    GtkListStore    *align_menu_model;
    GtkCellRenderer *cell;

    const char *font_name;

    const char *text_entry_tip;
    const char *text_align_tip;
    const char *color_menu_tip;
    const char *font_button_tip;
    const char *rotation_tip;

    text_entry_tip    = _TOOLTIP(TheText);
    text_align_tip    = _TOOLTIP(TextAlign);
    color_menu_tip    = _TOOLTIP(TextColor);
    font_button_tip   = _TOOLTIP(TextFont);
    rotation_tip      = _TOOLTIP(Rotation);

    ThisDialog = gschem_dialog_new_with_buttons(_("Edit Text Properties"),
                                                GTK_WINDOW(w_current->main_window),
                                                /* nonmodal Editing Dialog */              GSCHEM_MODELESS_DIALOG,
                                                IDS_TEXT_EDIT, w_current,
                                                GTK_STOCK_CLOSE, GEDA_RESPONSE_REJECT,
                                                GTK_STOCK_APPLY, GEDA_RESPONSE_ACCEPT,
                                                NULL);

    /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                            GEDA_RESPONSE_ACCEPT,
                                            GEDA_RESPONSE_REJECT,
                                            -1);
    /* Create the Dialog */
    vbox = GTK_DIALOG(ThisDialog)->vbox;

    /* Text Contents Label */
    text_label = geda_aligned_label_new (_("<b>Text Content</b>"), 0, 0);
    geda_label_set_use_markup (GEDA_LABEL (text_label), TRUE);
    gtk_box_pack_start(GTK_BOX(vbox), text_label, FALSE, FALSE, 0);

    /* Alignment Widget - Not Text Alignment property */
    alignment = gtk_alignment_new(0, 0, 1, 1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              DIALOG_INDENTATION, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

    /* Viewport Widget to hold the editing buffer window */
    viewport1 = gtk_viewport_new (NULL, NULL);
    gtk_widget_set_size_request(GTK_WIDGET(viewport1),-1,75);

    /* Create and add scrollable Window widget */
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_container_add (GTK_CONTAINER (viewport1), scrolled_window);
    gtk_container_add( GTK_CONTAINER(alignment), viewport1);

    /* Finally, create the actual text entry widget */
    textentry = gtk_text_view_new();
    gtk_text_view_set_editable (GTK_TEXT_VIEW (textentry), TRUE);
    gtk_container_add(GTK_CONTAINER(scrolled_window), textentry);
    gtk_widget_set_tooltip_text (GTK_WIDGET(textentry), text_entry_tip);
    gtk_widget_grab_focus(textentry);

    /* Text Properties Label */
    label = geda_aligned_label_new(_("<b>Text Properties</b>"), 0, 0);
    geda_label_set_use_markup(GEDA_LABEL(label), TRUE);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    /* Alignment Widget - Not Text Alignment property */
    alignment = gtk_alignment_new(0, 0, 1, 1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              DIALOG_INDENTATION, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

    /* Create Table Widget and put inside the alignment widget */
    table = gtk_table_new (4, 3, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
    gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
    gtk_container_add(GTK_CONTAINER(alignment), table);

    /* Text Alignment Label -- Table Row 1 */
    align_label = geda_aligned_mnemonic_label_new(_LABEL(TextAlign), 0, 0);
    gtk_table_attach(GTK_TABLE(table), align_label, 0,1,0,1, GTK_FILL,0,0,0);

    align_menu_model = create_menu_alignment(w_current);
    combobox = geda_combo_box_new_with_model(GTK_TREE_MODEL(align_menu_model));
    geda_combo_widget_set_wrap_width(combobox, 3);
    cell = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), cell, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox),
                                   cell, "text", 0, NULL);
    GEDA_UNREF (align_menu_model);
    gtk_table_attach_defaults(GTK_TABLE(table), combobox, 1,2,0,1);
    gtk_widget_set_tooltip_text (GTK_WIDGET(combobox), text_align_tip);

    /* Color Property Label -- Table Row 2 */
    color_label = geda_aligned_mnemonic_label_new(_LABEL(TextColor), 0, 0);
    gtk_table_attach(GTK_TABLE(table), color_label, 0,1,1,2, GTK_FILL,0,0,0);

    /* Use Subfucntion to create Color Selection Combobox */
    optionmenu = create_color_menu (w_current, text_object->color);
    gtk_table_attach_defaults(GTK_TABLE(table), optionmenu, 1,2,1,2);
    gtk_widget_set_tooltip_text (GTK_WIDGET(optionmenu), color_menu_tip);

    /* Font Button Label  -- Table Row 3 */
    font_label = geda_aligned_mnemonic_label_new(_LABEL(TextFont), 0, 0);
    gtk_table_attach(GTK_TABLE(table), font_label, 0,1,2,3, GTK_FILL,0,0,0);

    font_name = eda_renderer_get_font_name (CairoRenderer);
    font_button = geda_font_button_new_with_font (font_name);
    geda_font_button_set_title((GedaFontButton*)font_button,
                               "Select font and size");
    gtk_widget_set_tooltip_text (GTK_WIDGET(font_button), font_button_tip);
    g_object_set (font_button, "use-font", TRUE, "show-style", FALSE, NULL);

    gtk_table_attach (GTK_TABLE (table), font_button, 1, 2, 2, 3,
                      (GtkAttachOptions) ( GTK_FILL),
                      (GtkAttachOptions) (0), 0, 0);

    /* Text Rotation Label */
    rotate_label=geda_aligned_mnemonic_label_new(_LABEL(Rotation), 0,0);
    gtk_table_attach(GTK_TABLE(table), rotate_label, 0,1,3,4, GTK_FILL,0,0,0);

    GEDA_NUMERIC_SPIN(Rotation, 0, 0, 359);
    gtk_widget_set_tooltip_text (GTK_WIDGET(RotationSpin), rotation_tip);
    gtk_table_attach (GTK_TABLE (table), RotationSpin, 1, 2, 3, 4,
                      (GtkAttachOptions) ( GTK_FILL),
                      (GtkAttachOptions) (0), 0, 0);

    g_signal_connect (G_OBJECT (font_button), "font-set",
                      G_CALLBACK (widget_value_modified),
                      NULL);
    g_signal_connect (G_OBJECT (RotationSpin), "insert-text",
                      G_CALLBACK (widget_value_modified),
                      NULL);

    GEDA_HOOKUP_OBJECT(ThisDialog, textentry,   IDS_TEXT_EDIT);
    GEDA_HOOKUP_OBJECT(ThisDialog, combobox,    WIDGET(TextAlign));
    GEDA_HOOKUP_OBJECT(ThisDialog, optionmenu,  WIDGET(TextColor));
    GEDA_HOOKUP_OBJECT(ThisDialog, font_button, WIDGET(TextFont));

    /** Set the relationships between the label and their Widgets **/
    geda_label_set_mnemonic_widget (GEDA_LABEL (text_label), textentry);
    geda_label_set_mnemonic_widget (GEDA_LABEL (align_label), combobox);
    geda_label_set_mnemonic_widget (GEDA_LABEL (color_label), optionmenu);
    geda_label_set_mnemonic_widget (GEDA_LABEL (font_label), font_button);
    geda_label_set_mnemonic_widget (GEDA_LABEL (rotate_label), RotationSpin);

    atk_text_obj   = atk_widget_linked_label_new (text_label, textentry);
    atk_align_obj  = atk_widget_linked_label_new (align_label, combobox);
    atk_color_obj  = atk_widget_linked_label_new (color_label, optionmenu);
    atk_font_obj   = atk_widget_linked_label_new (font_label, font_button);
    atk_rotate_obj = atk_widget_linked_label_new (rotate_label, RotationSpin);

    if (atk_text_obj) {
      atk_object_set_name        (atk_text_obj,    _("Atrribute Name List"));
      atk_object_set_description (atk_text_obj,       text_entry_tip);
    }
    if (atk_align_obj) {
      atk_object_set_name        (atk_align_obj,   _("Text Alignment Attribute"));
      atk_object_set_description (atk_align_obj,      text_align_tip);
    }
    if (atk_color_obj) {
      atk_object_set_name        (atk_color_obj,   _("Attribute Value Entry"));
      atk_object_set_description (atk_color_obj,      color_menu_tip);
    }
    if (atk_font_obj) {
      atk_object_set_name        (atk_font_obj,    _("Atrribute Name List"));
      atk_object_set_description (atk_font_obj,       font_button_tip);
    }
    if (atk_rotate_obj) {
      atk_object_set_name        (atk_rotate_obj,  _("Text Rotation Angle Spinner Entry"));
      atk_object_set_description (atk_rotate_obj,     rotation_tip);
    }

    /* Set the OKAY button to be the default widget */
    gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                    GEDA_RESPONSE_ACCEPT);

    g_signal_connect (G_OBJECT (ThisDialog), "response",
                      G_CALLBACK (x_dialog_edit_text_response),
                      text_object);

    g_object_set (G_OBJECT (ThisDialog),
                  DIALOG_SELECTION_TRACKER,
                  x_dialog_text_edit_update_selection, NULL);

    gtk_widget_show_all(ThisDialog);
    w_current->tewindow = ThisDialog;
  }

  else { /* dialog already there */
    gtk_window_present(GTK_WINDOW(ThisDialog));
  }

  x_dialog_text_edit_update_selection (w_current, text_object);
}

/******************* End of Text Edit dialog box ************************/
/** @} end group Edit-Text-Dialog */
