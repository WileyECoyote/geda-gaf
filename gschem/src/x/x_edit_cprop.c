/* -*- C x_edit_cprop.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_edit_cprop.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
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
 *  Date Contributed: September, 27, 2013
 */
/*!
 * \file x_edit_cprop.c
 * \brief A dialog box for viewing and edit component properties.
 */

#include "gschem.h"
#include "x_dialog.h"
#include <geda_dialog_controls.h>
#include <geda_widgets.h>

#define Radio_Responder radio_responder

/** \defgroup Component-Properties-Dialog Component Properties Dialog
 *  @{ \memberof Editing-Dialogs
 *  \par
 *  The Component properties dialog has two modes of operation:
 *
 *     1.) Schematic mode.
 *     2.) Symbol mode
 *
 *   If the current file is a Schematic and the name in the symbol field
 *   has been changed then all fields except the reference designator are
 *   ignored. If a symbol with the new name exist, the current symbol is
 *   replaced with the new symbol and the reference designator from the
 *   reference designator entry field is assigned to the replacement symbol.
 *   If a symbol with the given name does not exist a warning is issued. If
 *   the symbol name has not been changed then electrical attributes are
 *   checked for non empty fields, and the corresponding attributes are
 *   either updated or created.
 *
 *   For Symbol editing, the symbol name is ignored, attribute fields are
 *   checked for non empty entries, and the corresponding attributes are
 *   either created updated.
 */

DECLARE_RADIO_TRIAD (RefDes, Std, Spice, Ieee);

typedef enum {
        RefDes,
        RefDesStd,
        RefDesSpice,
        RefDesIeee
} ControlID;

static WidgetStringData DialogStrings[] = {
        { "RefDesLabel",        " ",          "Set End Point markers for Selected nets."},
        { "RefDesStdRadio",     "Standard",   "None"},
        { "RefDesSpiceRadio",   "Spice",      "Empty box"},
        { "RefDesIeeeRadio",    "IEEE-315",   "Filled box mode"},
  { NULL, NULL, NULL},
};

/*! \brief Component Properties Internal Check and Update Attributes
 *  \par Function Description
 *  This function is called with or without and \a Object argument, when
 *  the \a object argument is present then attributes searching is limited
 *  to \a Object, otherwise all <b>Text</b> objects on the page are searched.
 *  If the new value is NULL or has zero length then the function does
 *  nothing. Otherwise the function attempts to find an attribute with
 *  the name key, and if found the attribute value is compared to new_
 *  value and changed if strings are difference. If an attribute is not
 *  found then a new attribute is created with the form key=value.
 *
 *  \param [in] w_current Gschem toplevel structure.
 *  \param [in] object    Pointer to an <b>Object</b> to search, also used as a flag
 *  \param [in] key       The name of the attribute
 *  \param [in] new_value String value that attribute should be assigned
 *
 *  \return boolean TRUE is a change was made, otherwise FALSE
 */
static bool x_dialog_ep_check_update_attribs (GschemToplevel *w_current,
                                              Object         *object,
                                              const char     *key,
                                              const char     *new_value)
{
  bool result = FALSE;

  if (new_value != NULL && strlen(new_value) !=0 ) {

    GList     *all_butes;
    GList     *attribs;
    Object    *a_current;
    char      *o_value;

    if (object) {

      /* This is object->attribs, do not free this list */
      attribs = o_attrib_get_attached_attribs(object);

      /* If object, then look for an attached attribute */
      a_current = o_attrib_find_attrib_by_name (attribs, key, 0);

    }
    else {

      /* symbol mode, get all objects on the page */
      attribs   = s_page_get_objects(Current_Page);
      a_current = NULL;

    }

    if (a_current) {

      if(o_attrib_get_name_value (a_current, NULL, &o_value)) {
        if (strcmp(o_value, new_value) != 0) {
          o_attrib_set_value (a_current, key, new_value);
          o_text_recreate(a_current);
          result = TRUE;
        }
        GEDA_FREE(o_value);
      }
    }
    else {

      if (object) {
        /* Not attached, check all attributes */
        all_butes = o_attrib_return_attribs(object);
      }
      else {
        all_butes = o_get_objects_by_type (attribs, OBJ_TEXT);
      }

      a_current = o_attrib_find_attrib_by_name (all_butes, key, 0);
      g_list_free (all_butes);

      if (a_current) {
        if(o_attrib_get_name_value (a_current, NULL, &o_value)) {
          if (strcmp(o_value, new_value) != 0) {
            o_attrib_set_value (a_current, key, new_value);
            o_text_recreate(a_current);
            result = TRUE;
          }
          GEDA_FREE(o_value);
        }
      }
      else {

        char *newtext = u_string_concat(key, "=", new_value, NULL);
        o_attrib_add_attrib(w_current, newtext, VISIBLE, SHOW_NAME_VALUE, NULL);
        GEDA_FREE (newtext);
        result = TRUE;
      }
    }
  }
  return result;
}

/*! \brief Component Properties Internal Revise Electrical Attributes
 *  \par Function Description
 *  This function is called with or without and \a Object argument, which is
 *  passed as a flag to x_dialog_ep_check_update_attribs to indicate the
 *  editing mode, with schematic or symbol. The function retrieves strings
 *  from the entries in the lower halve of the dialog, fields controlled by
 *  the electrical check-box, and calls x_dialog_ep_check_update_attribs
 *  for each field.
 *
 *  \param [in] w_current  Gschem toplevel structure.
 *  \param [in] properties Pointer to a Component Dialog data structure
 *  \param [in] object     Pointer to an<b>Object</b> to search, also used as a flag
 *
 *  \return boolean TRUE is a change was made, otherwise FALSE
 *
 *  \sa x_dialog_ep_revise_symbol_attribs
 */
static bool x_dialog_ep_revise_elect_attribs(GschemToplevel *w_current,
                                             property_data  *properties,
                                             Object         *object)
{
  const char *str_val;
  bool        result = 0;

  if (GetToggleState(properties->electrical_cb)) {

    str_val  = GetEntryText ( properties->refdes_entry );
    result   = x_dialog_ep_check_update_attribs (w_current, object, "refdes", str_val);

    str_val  = GetEntryText ( properties->slots_spin );
    result  += x_dialog_ep_check_update_attribs (w_current, object, "numslots", str_val);

    str_val  = GetEntryText ( properties->value_entry );
    result  += x_dialog_ep_check_update_attribs (w_current, object, "value", str_val);

    str_val  = GetEntryText ( properties->foot_entry );
    result  += x_dialog_ep_check_update_attribs (w_current, object, "footprint", str_val);

    str_val  = GetEntryText ( properties->spice_entry );
    result  += x_dialog_ep_check_update_attribs (w_current, object, "spice-type", str_val);

    str_val  = GetEntryText ( properties->mname_entry );
    result  += x_dialog_ep_check_update_attribs (w_current, object, "model-name", str_val);

  }
  else {

    if (!object) {
      result   = x_dialog_ep_check_update_attribs (w_current, object, "refdes", "none");
      result  += x_dialog_ep_check_update_attribs (w_current, object, "numslots", "0");
      result  += x_dialog_ep_check_update_attribs (w_current, object, "footprint", "none");
    }
  }

  return (result == !FALSE);
}

/*! \brief Component Properties Internal Revise Non-Electrical Attributes
 *  \par Function Description
 *  This function retrieves strings from the entries in the upper halve of
 *  the dialog, fields not controlled by the electrical check-box, and calls
 *  x_dialog_ep_check_update_attribs for each field. The functions returns
 *  by calling x_dialog_ep_revise_elect_attribs with a NULL argument of the
 *  object to indicate symbol editing mode. This function is not called when
 *  editing schematic files.
 *
 *  \param [in] w_current  Gschem toplevel structure.
 *  \param [in] properties Pointer to a Component Dialog data structure
 *
 *  \return boolean TRUE is a change was made, otherwise FALSE
 *
 *  \sa x_dialog_ep_revise_elect_attribs
 */
static bool x_dialog_ep_revise_symbol_attribs (GschemToplevel *w_current,
                                               property_data  *properties)
{
  bool result = FALSE;
  const char *str_val;

  /* get the name from the text entries of the dialog */
  str_val  = GetEntryText ( properties->device_entry );
  result  += x_dialog_ep_check_update_attribs (w_current, NULL, "device", str_val);

  str_val  = GetEntryText ( properties->author_entry );
  result  += x_dialog_ep_check_update_attribs (w_current, NULL, "author", str_val);

  str_val  = GetEntryText ( properties->version_entry );
  result  += x_dialog_ep_check_update_attribs (w_current, NULL, "symversion", str_val);

  str_val  = GetEntryText ( properties->ulicense_entry );
  result  += x_dialog_ep_check_update_attribs (w_current, NULL, "use-license", str_val);

  str_val  = GetEntryText ( properties->dlicense_entry );
  result  += x_dialog_ep_check_update_attribs (w_current, NULL, "dist-license", str_val);

  str_val  = GetEntryText ( properties->doc_entry );
  result  += x_dialog_ep_check_update_attribs (w_current, NULL, "documentation", str_val);

  str_val  = GetEntryText ( properties->descr_entry );
  result  += x_dialog_ep_check_update_attribs (w_current, NULL, "description", str_val);

  str_val  = GetEntryText ( properties->comment_entry );
  result  += x_dialog_ep_check_update_attribs (w_current, NULL, "comment", str_val);

  if (!GetToggleState(properties->electrical_cb)) {
    fprintf(stderr, "Create a graphical atrribute=1\n");
  }
  return (result == !FALSE) ||
        x_dialog_ep_revise_elect_attribs (w_current, properties, NULL);
}


static bool x_dialog_ep_check_symver_attribs (GschemToplevel *w_current,
                                              property_data  *properties,
                                              Object         *object)
{
  bool result = FALSE;
  const char *str_val;

  /* get the name from the text entries of the dialog */
  str_val = GetEntryText ( properties->version_entry );
  result  = x_dialog_ep_check_update_attribs (w_current, object,
                                              "symversion", str_val);
  return result;
}
/*! \brief Component Properties Dialog Apply Settings
 *  \par Function Description
 *  This function applies the settings of the properties dialog to the
 *  selected objects or to the symbol page depending on whether object
 *  has a value. The function handles the case of exchanging the symbol
 *  and calls helper functions for attribute handling.
 *
 *  \param [in] dialog     Pointer to a Component Dialog instance.
 *  \param [in] properties Pointer to a Component Dialog data structure
 *
 */
static void x_dialog_edit_properties_ok(GtkWidget     *dialog,
                                        property_data *properties)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(dialog)->w_current;
  bool changed              = FALSE;

  if (s_page_is_symbol_file(Current_Page)) {
    changed = x_dialog_ep_revise_symbol_attribs (w_current, properties);
  }
  else {

    Object *o_current;

    o_current = g_object_get_data (G_OBJECT (dialog), "object");

    if (o_current != NULL && o_current->type == OBJ_COMPLEX) {

      Complex    *o_complex;
      const char *filename;

      o_complex = o_current->complex;

      /* get the name from the text entries of the dialog */
      filename  = GetEntryText ( properties->symbol_entry );

      if (filename && (strcmp (filename, o_complex->filename) != 0)) {

        const CLibSymbol *clib;

        clib = s_clib_get_symbol_by_name (filename);
        s_clib_symbol_invalidate_data (clib);

        if (clib == NULL) {
          u_log_message (_("Could not find symbol [%s] in library. Update failed.\n"),
                         filename);
        }
        else {

          GList      *new_butes;
          GList      *old_ribs;
          GList      *iter;
          const char *refdes;
          Object     *o_new;
          Object     *attrib;

          /* do not use library, the selection change will trigger an event */
          Current_Selection->glist = g_list_remove (Current_Selection->glist, o_current);

          /* Create new object and set embedded */
          o_new = o_complex_new (w_current->toplevel,
                                 o_complex->x,
                                 o_complex->y,
                                 o_complex->angle,
                                 o_complex->mirror,
                                 clib, filename, 1);

          if (o_complex_is_embedded (o_current)) {
            o_embed (w_current->toplevel, o_new);
          }

          o_attrib_freeze_hooks (o_new);

          new_butes = o_complex_promote_attribs (w_current->toplevel, o_new);

          /* If new and old object have a "slot" then set new to match old */
          /* Don't care what the value new is, just if it has one */
          if (o_attrib_first_attrib_by_name (o_new, "slot")) {

            char *value;

            attrib = o_attrib_first_attrib_by_name (o_current, "slot");
            if (attrib) {
              /* Get the old slot value */
              o_attrib_get_name_value (attrib, NULL, &value);
              /* Set the slot value of the new object to the old value */
              o_attrib_set_value (attrib, "slot", value);
              GEDA_FREE(value);
            }
          }

           /* Add new attributes to page */
          s_page_append_list (Current_Page, new_butes);

          /* Update pinnumbers for current slot */
          s_slot_update_object (o_new);

          o_attrib_thaw_hooks (o_new);

          old_ribs = o_attrib_get_attached_attribs (o_current);

          for (iter = old_ribs; iter != NULL; iter = g_list_next (iter)) {
            Object *obj = (Object *) iter->data;
            Current_Selection->glist = g_list_remove (Current_Selection->glist, obj);
            s_page_remove_object (Current_Page, obj);
            s_object_release (obj);
          }

          /* Replace old Object with new Object */
          s_page_replace_object (Current_Page, o_current, o_new);

          s_object_release (o_current);

          /* Set refdes on new complex to what is in the dialog entry */
          refdes  = GetEntryText (properties->refdes_entry );

          attrib = o_attrib_first_attrib_by_name (o_new, "refdes");
          if (attrib) {
            o_attrib_set_value (attrib, "refdes", refdes);
            o_text_recreate(attrib);
          }

          /* Select new Object */
          o_selection_add (Current_Selection, o_new);

          changed = TRUE;
        }
      }
      else {
        changed = x_dialog_ep_revise_elect_attribs(w_current,
                                                   properties,
                                                   o_current);

        if (x_dialog_ep_check_symver_attribs(w_current,
                                             properties,
                                             o_current))
        {
          changed++;
        }
      }
    }
  }

  if (changed) {
    Current_Page->CHANGED = 1;
    o_undo_savestate(w_current, UNDO_ALL);
  }
}

/*! \brief Response Function for the Component Properties Dialog
 *  \par Function Description
 *  This function handles the response to the Component Properties dialog.
 *  Either the is called to retrieve and process data in the dialog or
 *  the dialog session is terminated.
 */
static void x_dialog_edit_properties_response(GtkWidget     *Dialog,
                                              int            response,
                                              property_data *properties)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GEDA_RESPONSE_CLOSE:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_grab_remove (Dialog);
    gtk_widget_destroy (Dialog);
    GEDA_FREE (properties);
    break;
  case GEDA_RESPONSE_ACCEPT:
    x_dialog_edit_properties_ok(Dialog, properties);
    break;
  default:
    BUG_IMSG ("unhandled case for signal <%d>", response);
  }

  i_status_set_state (w_current, SELECT);

}

static void x_dialog_ep_refdes_update_entry (GtkWidget *widget,
                                             GtkWidget *dialog)
{
  property_data *properties;

  Object *o_current;
  properties = g_object_get_data (G_OBJECT (dialog), IDS_PROP_EDIT);

  o_current  = g_object_get_data (G_OBJECT (dialog), "object");

  if (o_current != NULL && o_current->type == OBJ_COMPLEX) {

    Object     *attrib;
    const char *str_val;
    const char *curr_ref;
          char *new_text;

    attrib = o_attrib_first_attrib_by_name (o_current, "refdes");

    if (attrib) {

      curr_ref = u_refdes_return_numeric (attrib->text->string);

      if (curr_ref) {
        str_val = GetEntryText ( properties->refdes_entry );
        new_text = u_string_concat(str_val, curr_ref, NULL);
        g_signal_handler_block(widget,properties->ref_handler);
        SetEntryText(properties->refdes_entry, new_text);
        g_signal_handler_unblock(widget,properties->ref_handler);
        GEDA_FREE (new_text);
      }
    }
  }
}

/*! \brief Component Properties Dialog Electrical Check-box Callback
 *   Enable or disabled sensitivities of widgets within the electrical
 *   frame depending on the state of the check-box.
 *
 *  \param [in] check_butt Pointer to the CheckBox widget
 *  \param [in] data       Pointer to a Component Dialog data structure
 *
 */
static void x_dialog_ep_electrical_cb (GtkWidget *check_butt, void *data)
{
  property_data *properties = data;
  bool           state      = GetToggleState(check_butt);

  void set_sensitive (GtkWidget *widget, void *nothing){
    gtk_widget_set_sensitive(widget, state);
  }
  gtk_container_foreach (GTK_CONTAINER (properties->elect_table),
                         set_sensitive, NULL);
}

/*! \brief Component Properties Dialog Enable Version Check-box Callback
 *   Enable or disabled sensitivities of widgets within the electrical
 *   frame depending on the state of the check-box.
 *
 *  \param [in] check_butt Pointer to the CheckBox widget
 *  \param [in] data       Pointer to a Component Dialog data structure
 *
 */
static void x_dialog_ep_version_cb (GtkWidget *check_butt, void *data)
{
  property_data *properties = data;
  bool           state      = GetToggleState(check_butt);
  const char    *str_val    = GetEntryText(properties->version_entry);
  const char    *dash       = "-";

  char *str = NULL;

  if (strlen(str_val)) {
    if (!state && (str_val[0] != '-')) {
      str = u_string_concat(&dash[0], str_val,NULL);
    }
    else if (state && (str_val[0] == '-')) {
      str = u_string_strdup(&str_val[1]);
    }

    SetEntryText (properties->version_entry, str);
    GEDA_FREE(str);
  }
}

/*! \brief Handle selection change event for x_dialog_edit_properties
 *  \par Function Description
 *  Updates component information in dialog widgets when the selection
 *  changes.
 *
 *  \param w_current  pointer to GschemToplevel context
 *  \param object     pointer to a selected Object.
 *  \param properties pointer to property_data structure
 */
static void x_dialog_ep_component_change(GschemToplevel *w_current,
                                         Object         *object,
                                         property_data  *properties)
{
  GList     *attribs;
  GList     *all_butes;
  Object    *a_current;
  char      *filename;
  char      *value;
  int        pin_count;
  void      (*set_entry)(const char *name, GtkWidget *entry);

  void entry_page_setter(const char *name, GtkWidget *entry) {

    a_current = o_attrib_find_attrib_by_name (all_butes, name, 0);
    if (a_current) {
      o_attrib_get_name_value (a_current, NULL, &value);
      SetEntryText(entry, value);
      GEDA_FREE(value);
    }
    else {
      SetEntryText(entry, NULL);
    }
  }

  void entry_object_setter(const char *name, GtkWidget *entry) {

    a_current = o_attrib_find_attrib_by_name (attribs, name, 0);
    if (a_current) {
      o_attrib_get_name_value (a_current, NULL, &value);
      SetEntryText(entry, value);
      GEDA_FREE(value);
    }
    else {
      a_current = o_attrib_find_attrib_by_name (all_butes, name, 0);
      if (a_current) {
        o_attrib_get_name_value (a_current, NULL, &value);
        SetEntryText(entry, value);
        GEDA_FREE(value);
      }
      else {
        SetEntryText(entry, NULL);
      }
    }
  }

  if (object) {

    char *fullname;
    const CLibSymbol *clib;

    filename = object->complex->filename;

    SetEntryText(properties->symbol_entry, filename);

    /* Get the full name */
    clib = s_clib_get_symbol_by_name (filename);

    fullname = s_clib_symbol_get_filename(clib);

    /* set the tooltip as the full file name */
    SetWidgetTip(properties->symbol_entry, fullname);

    GEDA_FREE(fullname);

    attribs   = o_attrib_get_attached_attribs(object);
    all_butes = o_attrib_return_attribs(object);

    set_entry = entry_object_setter;
  }
  else {

    Page *page;

    page = geda_toplevel_get_current_page(w_current->toplevel);

    if (GEDA_IS_PAGE(page)) {

      filename = f_get_basename(Current_Page->filename);
      SetEntryText(properties->symbol_entry, filename);

      /* set the tooltip as the full file name */
      SetWidgetTip(properties->symbol_entry, Current_Page->filename);

      /* symbol mode, get all objects on the page */
      attribs   = s_page_get_objects(Current_Page);
      all_butes = o_get_objects_by_type (attribs, OBJ_TEXT);
    }
    else {
      all_butes = NULL;
    }
    set_entry = entry_page_setter;
  }

  set_entry ("device",        properties->device_entry);
  set_entry ("author",        properties->author_entry);
  set_entry ("symversion",    properties->version_entry);
  set_entry ("use-license",   properties->ulicense_entry);
  set_entry ("dist-license",  properties->dlicense_entry);
  set_entry ("description",   properties->descr_entry);
  set_entry ("documentation", properties->doc_entry);
  set_entry ("comment",       properties->comment_entry);

  g_signal_handler_block(properties->version_cb,properties->ver_handler);
  const char *str_val  = GetEntryText ( properties->version_entry );
  SetToggleState(properties->version_cb, (str_val[0] != '-'));
  g_signal_handler_unblock(properties->version_cb,properties->ver_handler);

  if (o_attrib_find_attrib_by_name (all_butes, "graphical", 0)) {

    /* Then this is a Graphical object */
    if (GetToggleState(properties->electrical_cb)) {
      SetToggleState(properties->electrical_cb, FALSE);
    }
    else {
      x_dialog_ep_electrical_cb(properties->electrical_cb, properties);
    }

    SetEntryText(properties->refdes_entry, NULL);
    SetEntryText(properties->value_entry, NULL);
    SetEntryText(properties->foot_entry, NULL);
    SetEntryText(properties->spice_entry, NULL);
    SetEntryText(properties->mname_entry, NULL);

    SetSpinValue(properties->slots_spin, 0);
    SetSpinValue(properties->pins_spin, 0);
  }
  else { /* This is a Non-Graphical object */

    if (!GetToggleState(properties->electrical_cb)) {
      SetToggleState(properties->electrical_cb, TRUE);
    }
    else {
      x_dialog_ep_electrical_cb(properties->electrical_cb, properties);
    }

    set_entry ("refdes",        properties->refdes_entry);
    set_entry ("value",         properties->value_entry);
    set_entry ("footprint",     properties->foot_entry);
    set_entry ("spice-type",    properties->spice_entry);
    set_entry ("model-name",    properties->mname_entry);

    /* Hard to image anyone promoting these */
    a_current = o_attrib_find_attrib_by_name (all_butes, "numslots", 0);
    if (a_current) {
      if (o_attrib_get_name_value (a_current, NULL, &value)) {
        SetSpinValue(properties->slots_spin, atoi(value));
        GEDA_FREE(value); /* likely a NULL terminated zero */
      }
    }
    else {
      SetSpinValue(properties->slots_spin, 0);
    }

    a_current = o_attrib_find_attrib_by_name (all_butes, "pins", 0);
    if (a_current) {
      if (o_attrib_get_name_value (a_current, NULL, &value)) {
        pin_count = atoi(value);
        SetSpinValue(properties->pins_spin, pin_count);
        GEDA_FREE(value); /* likely a NULL terminated zero */
      }
    }
    else {

      if (object) {
        pin_count = g_list_length(object->complex->pin_objs);
        SetSpinValue(properties->pins_spin, pin_count);
      }
    }
  }

  g_list_free (all_butes);
}


/*! \brief Handle selection change event for x_dialog_edit_properties
 *  \par Function Description
 *  Updates component information in dialog widgets when the selection
 *  changes.
 *
 *  \param w_current  pointer to GschemToplevel context
 *  \param object     pointer to a selected Object.
 *  \param properties pointer to property_data structure
 */
static void x_dialog_ep_no_selection(GschemToplevel *w_current,
                                     property_data  *properties)
{
  SetEntryText(properties->symbol_entry, NULL);
  SetEntryText(properties->device_entry, NULL);
  SetEntryText(properties->author_entry, NULL);
  SetEntryText(properties->version_entry, NULL);
  SetEntryText(properties->ulicense_entry, NULL);
  SetEntryText(properties->dlicense_entry, NULL);
  SetEntryText(properties->descr_entry, NULL);
  SetEntryText(properties->doc_entry, NULL);
  SetEntryText(properties->comment_entry, NULL);

  SetEntryText(properties->refdes_entry, NULL);
  SetEntryText(properties->value_entry, NULL);
  SetEntryText(properties->foot_entry, NULL);
  SetEntryText(properties->spice_entry, NULL);
  SetEntryText(properties->mname_entry, NULL);

  SetSpinValue(properties->slots_spin, 0);
  SetSpinValue(properties->pins_spin, 0);

  SetToggleState(properties->electrical_cb, FALSE);

  //GtkWidget *refdes_combo;
  //GtkWidget *elect_table;
}
/*! \brief Component Properties Dialog Check-box Callback
 *   Enable or disabled sensitivities of widgets within the electrical
 *   frame depending on the state of the check-box.
 *
 *  \param [in] check_butt Pointer to the CheckBox widget
 *  \param [in] data       Pointer to a Component Dialog data structure
 *
 */
static void x_dialog_ep_set_sensitive (property_data *properties, bool state)
{
  void set_sensitive (GtkWidget *widget, void *nothing){
    gtk_widget_set_sensitive(widget, state);
  }

  gtk_widget_set_sensitive(properties->symbol_entry, state);
  gtk_widget_set_sensitive(properties->device_entry, state);
  gtk_widget_set_sensitive(properties->author_entry, state);
  gtk_widget_set_sensitive(properties->version_entry, state);
  gtk_widget_set_sensitive(properties->ulicense_entry, state);
  gtk_widget_set_sensitive(properties->dlicense_entry, state);
  gtk_widget_set_sensitive(properties->descr_entry, state);
  gtk_widget_set_sensitive(properties->doc_entry, state);
  gtk_widget_set_sensitive(properties->comment_entry, state);
  gtk_widget_set_sensitive(properties->electrical_cb, state);
  gtk_container_foreach (GTK_CONTAINER (properties->elect_table),
                         set_sensitive, NULL);
}
/*! \brief Handle selection change event for x_dialog_edit_properties
 *  \par Function Description
 *  Called when the selection changes. The functions calls
 *  x_dialog_ep_component_change to update the data fields
 *
 *  \param w_current pointer to GschemToplevel context
 *  \param object    pointer to a selected Object.
 */
static void x_dialog_ep_update_selection (GschemToplevel *w_current,
                                          Object         *object)
{
  GtkWidget     *dialog;
  property_data *properties;

  /* Get ptr to the data structure */
  dialog     = w_current->prwindow;

  properties = g_object_get_data (G_OBJECT (dialog), IDS_PROP_EDIT);

  if (object != NULL && object->type == OBJ_COMPLEX) {
    x_dialog_ep_set_sensitive(properties, TRUE);
    x_dialog_ep_component_change(w_current, object, properties);
    g_object_set_data(G_OBJECT(dialog), "object", object);
    gtk_widget_grab_focus(properties->symbol_entry);
  }
  else if (s_page_is_symbol_file(Current_Page)) {
    x_dialog_ep_set_sensitive(properties, TRUE);
    x_dialog_ep_component_change(w_current, NULL, properties);
    g_object_set_data(G_OBJECT(dialog), "object", NULL);
    gtk_widget_grab_focus(properties->author_entry);
  }
  else {
    x_dialog_ep_no_selection(w_current, properties);
    x_dialog_ep_set_sensitive(properties, FALSE);
    g_object_set_data(G_OBJECT(dialog), "object", NULL);
  }
}

/*! \brief Component Properties Dialog Construction Helper to Load Combo
 *  \par Function Description
 *  Called during construction to load string into the combo text box for
 *  reference designators.
 *
 *  \param widget  pointer to GschemToplevel context
 *  \param type    integer type is the set of desginator to be loaded.
 */
static void x_dialog_edit_properties_load_refdes(GtkWidget *widget, int type)
{
  GedaComboBoxText *combo = (GedaComboBoxText*) widget;
  const GedaRefDes *designators;

  const char *ref;
  const char *descr;
  int   index;

  switch ( type ) {
    case RefDesStd:
      designators = u_refdes_get_standard_designators();
      break;
    case RefDesSpice:
      designators = u_refdes_get_spice_designators();
      break;
    case RefDesIeee:
      designators = u_refdes_get_ieee_designators();
      break;
    default:
      designators = u_refdes_get_standard_designators();
      break;
  }

  geda_combo_box_text_remove_all(combo);

  index = 0;
  while (designators[index].designator) {
    ref = designators[index].designator;
    descr = designators[index].description;
    geda_combo_box_text_list_append(combo, ref, descr);
    ++index;
  }
}

/*! \brief Function to toggle radio images
 *  \par Function Description: This function changes the images of
 *       controls created with create_geda_switch to the opposite
 *       state, i.e. if ON use OFF image and if OFF use ON image.
 */
static void radio_responder(GtkWidget *widget,  int control)
{
  bool state = GET_SWITCH_STATE (widget);

  if ((GTK_IS_BUTTON(widget)) && (state != TRUE)) {

    set_bulb_on(widget);

    switch ( control ) {
      case RefDesStd:
        set_bulb_off(RefDesIeeeRadio); set_bulb_off(RefDesSpiceRadio);
        break;
      case RefDesIeee:
        set_bulb_off(RefDesStdRadio); set_bulb_off(RefDesSpiceRadio);
        break;
      case RefDesSpice:
        set_bulb_off(RefDesStdRadio); set_bulb_off(RefDesIeeeRadio);
        break;
      default:
        break;
    }
  }

  return;
}
static void x_dialog_edit_properties_load_std_des(GtkWidget *widget,
                                                  void *user_data)
{
  property_data *properties = user_data;
  x_dialog_edit_properties_load_refdes(properties->refdes_combo, RefDesStd);
}

static void x_dialog_edit_properties_load_spice_des(GtkWidget *widget,
                                                    void *user_data)
{
  property_data *properties = user_data;
  x_dialog_edit_properties_load_refdes(properties->refdes_combo, RefDesSpice);
}

static void x_dialog_edit_properties_load_ieee_des(GtkWidget *widget,
                                                   void *user_data)
{
  property_data *properties = user_data;
  x_dialog_edit_properties_load_refdes(properties->refdes_combo, RefDesIeee);
}

/*! \brief Emit GEDA_RESPONSE_CLOSE signal.
 *  \par Function Description
 *  This function is called when the Close button on the Component
 *  Select dialog is pressed.
 */
static void on_close_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data),
                           "response",
                           GEDA_RESPONSE_CLOSE,
                           user_data);
}

/*! \brief Emit COMPSELECT_RESPONSE_HIDE signal.
 *  \par Function Description
 *  This function is called when the Okay button on the Component
 *  Select dialog is pressed.
 */
static void on_apply_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data),
                           "response",
                           GEDA_RESPONSE_ACCEPT,
                           user_data);
}

static void x_dialog_edit_properties_action_area (GtkWidget     *ThisDialog,
                                                  property_data *properties)
{
  GtkWidget *action_area;
  GtkWidget *alignment;
  GtkWidget *RefGroup_vbox;
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

  RefGroup_vbox = gtk_vbox_new(FALSE, 0);
  g_object_set (RefGroup_vbox, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (action_hbox), RefGroup_vbox, FALSE, FALSE, 0);

  GEDA_H_BULB_TRIAD( RefGroup_vbox, RefDes,  Std, Spice, Ieee, Std);

  gtk_box_set_spacing (GTK_BOX (RefDesGroup_hbox), 2);

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

  g_signal_connect (RefDesStdRadio, "pressed",
                    G_CALLBACK (x_dialog_edit_properties_load_std_des),
                    properties);

  g_signal_connect (RefDesSpiceRadio, "pressed",
                    G_CALLBACK (x_dialog_edit_properties_load_spice_des),
                    properties);

  g_signal_connect (RefDesIeeeRadio, "pressed",
                    G_CALLBACK (x_dialog_edit_properties_load_ieee_des),
                    properties);

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
                                          GEDA_RESPONSE_CLOSE,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog), GEDA_RESPONSE_ACCEPT);
  gtk_widget_grab_default (apply_butt);
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
GtkWidget* x_dialog_edit_properties_constructor (GschemToplevel *w_current)
{
  AtkObject *atk_obj;
  GtkWidget *Dialog;
  GtkWidget *alignment;
  GtkWidget *frame;
  GtkWidget *hbox;
  GtkWidget *table;
  GtkWidget *vbox;
  GtkWidget *verbox;
  GtkWidget *widget;

  GtkWidget *symbol_label;
  GtkWidget *device_label;
  GtkWidget *author_label;
  GtkWidget *enable_label;
  GtkWidget *version_label;
  GtkWidget *ulicense_label;
  GtkWidget *dlicense_label;
  GtkWidget *descr_label;
  GtkWidget *doc_label;
  GtkWidget *comment_label;
  GtkWidget *electrical_label;
  GtkWidget *refdes_label;
  GtkWidget *slots_label;
  GtkWidget *pins_label;
  GtkWidget *value_label;
  GtkWidget *foot_label;
  GtkWidget *spice_label;
  GtkWidget *mname_label;

  property_data *properties;

  const char *symbol_tip     = "Symbol file name";
  const char *device_tip     = "device identifier";
  const char *author_tip     = "The symbols author";
  const char *version_tip    = "version of symbol";
  const char *enable_ver_tip = "Enable version checking for this component";
  const char *ulicense_tip   = "use-license attribute";
  const char *dlicense_tip   = "redistribution license for the symbol";
  const char *descr_tip      = "Component description";
  const char *doc_tip        = "Device documentation";
  const char *comment_tip    = "Addition information related to this symbol";
  const char *electrical_tip = "Disable to set electricaly related attribute";
  const char *refdes_tip     = "Component designator";
  const char *slots_tip      = "Number of slots, equal device per package";
  const char *pins_tip       = "Number of pins per package";
  const char *value_tip      = "The value of the component";
  const char *foot_tip       = "Foot print used during component layout";
  const char *spice_tip      = "The spice-type attributes over-rides the refdes";
  const char *mname_tip      = "Name of the model for this component";

  Dialog = gschem_dialog_new_empty(_("Edit Component Properties"),
                                          GTK_WINDOW(w_current->main_window),
         /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                          IDS_PROP_EDIT, w_current);

  properties = (property_data*) GEDA_MEM_ALLOC (sizeof (struct st_property_data));

  vbox = GTK_DIALOG(Dialog)->vbox;

  table = gtk_table_new (6, 5, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  /* Row 1 Col 1 */
  symbol_label = GEDA_AVM_LABEL_NEW (_("_Symbol:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), symbol_label, 0,1,0,1, GTK_FILL,0,0,0);

  /* Row 1 Col 2 */
  widget = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 1,2,0,1);
  /* Tooltip not set here, see x_dialog_ep_component_change */
  properties->symbol_entry = widget;

  /* Row 1 Col 4 */
  device_label = GEDA_AVM_LABEL_NEW (_("_Device:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), device_label, 3,4,0,1, GTK_FILL,0,0,0);

  /* Row 1 Col 5 */
  widget = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 4,5,0,1);
  SetWidgetTip(widget, _(device_tip));
  properties->device_entry = widget;

  /* Row 2 Col 1 */
  author_label = GEDA_AVM_LABEL_NEW (_("_Author:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), author_label, 0,1,1,2, GTK_FILL,0,0,0);

  /* Row 2 Col 2 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 1,2,1,2);
  SetWidgetTip(widget, _(author_tip));
  properties->author_entry = widget;

  /* Row 2 Col 4 */
  version_label = GEDA_AVM_LABEL_NEW (_("_Version:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), version_label, 3,4,1,2, GTK_FILL,0,0,0);

  /* Row 2 Col 5 */
  verbox = gtk_hbox_new(FALSE, 0);
  g_object_set (verbox, "visible", TRUE, NULL);

  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_box_pack_start (GTK_BOX (verbox), widget, FALSE, FALSE, 0);
  SetWidgetTip(widget, _(version_tip));
  properties->version_entry = widget;

  widget = gtk_check_button_new ();
  gtk_box_pack_start (GTK_BOX (verbox), widget,  FALSE, FALSE, 5);
  SetWidgetTip(widget, _(enable_ver_tip));
  properties->version_cb = widget;

  enable_label = GEDA_AVM_LABEL_NEW (_("_Enable"), 0, 1);
  gtk_container_add (GTK_CONTAINER (verbox), enable_label);

  gtk_box_set_spacing (GTK_BOX (verbox), 2);
  g_object_set (verbox, "visible", TRUE, NULL);
  gtk_table_attach_defaults(GTK_TABLE(table), verbox, 4,5,1,2);

  /* Row 3 Col 1 */
  ulicense_label = GEDA_AVM_LABEL_NEW (_("_use-license:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), ulicense_label, 0,1,2,3, GTK_FILL,0,0,0);

  /* Row 3 Col 2 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 1,2,2,3);
  SetWidgetTip(widget, _(ulicense_tip));
  properties->ulicense_entry = widget;

  /* Row 3 Col 4 */
  dlicense_label = GEDA_AVM_LABEL_NEW (_("dist-_license:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), dlicense_label, 3,4,2,3, GTK_FILL,0,0,0);

  /* Row 3 Col 5 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 4,5,2,3);
  SetWidgetTip(widget, _(dlicense_tip));
  properties->dlicense_entry = widget;

  /* Row 4 Col 1 */
  descr_label = GEDA_AVM_LABEL_NEW (_("Descr_iption:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), descr_label, 0,1,3,4, GTK_FILL,0,0,0);

  /* Row 4 Col 3-5 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 1,5,3,4);
  SetWidgetTip(widget, _(descr_tip));
  properties->descr_entry = widget;

  /* Row 5 Col 1 */
  doc_label = GEDA_AVM_LABEL_NEW (_("Documen_tation:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), doc_label, 0,1,4,5, GTK_FILL,0,0,0);

  /* Row 5 Col 3-5 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 1,5,4,5);
  SetWidgetTip(widget, _(doc_tip));
  properties->doc_entry = widget;

  /* Row 6 Col 1 */
  comment_label = GEDA_AVM_LABEL_NEW (_("_Comment:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), comment_label, 0,1,5,6, GTK_FILL,0,0,0);

  /* Row 6 Col 3-5 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 1,5,5,6);
  SetWidgetTip(widget, _(comment_tip));
  properties->comment_entry = widget;

  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME, "label", "", NULL));
  gtk_box_pack_start(GTK_BOX(vbox), frame, FALSE, FALSE,
                     DEFAULT_WIDGET_SPACING);

  /* Create Checkbox and use as Label for the Frame, note that we
   * can not use gtk_check_button_new_with_label because we need
   * access to label for Atk. */
  hbox = gtk_hbox_new(FALSE, 0);

  widget = gtk_check_button_new ();
  gtk_container_add (GTK_CONTAINER (hbox), widget);
  SetWidgetTip(widget, _(electrical_tip));
  gtk_toggle_button_set_active ((GtkToggleButton*)widget, FALSE);
  properties->electrical_cb = widget;

  electrical_label = GEDA_AVM_LABEL_NEW (_(" _Electrical:"), 0, 0);
  gtk_container_add (GTK_CONTAINER (hbox), electrical_label);

  gtk_frame_set_label_widget (GTK_FRAME(frame), hbox);

  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        "right-padding",
                                         DIALOG_H_SPACING,
                                        "left-padding",
                                         DIALOG_H_SPACING,
                                        "xscale", 0.0,
                                        "yscale", 0.0,
                                        "xalign", 0.5,
                                        "yalign", 0.5,
                                        NULL));

  gtk_container_add (GTK_CONTAINER (frame), alignment);

  /* Create a second table and put in the alignment */
  table = gtk_table_new (4, 7, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add (GTK_CONTAINER (alignment), table);

  /* Save a reference tothe table*/
  properties->elect_table = table;

  /* Row 1 Col 1 = Reference Designator */
  refdes_label = GEDA_AVM_LABEL_NEW (_("_Ref Des:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), refdes_label, 0,1,0,1, GTK_FILL,0,0,0);

  /* Row 1 Col 2 = Reference Designator entry */
  widget = geda_combo_box_text_list_new ();
  geda_combo_box_text_set_activate_default(GEDA_COMBO_BOX_TEXT(widget), TRUE);
  gtk_table_attach(GTK_TABLE(table), widget, 1,2,0,1, GTK_FILL,0,0,0);
  SetWidgetTip(widget, _(refdes_tip));
  properties->refdes_combo = widget;
  widget = geda_combo_box_text_get_entry(GEDA_COMBO_BOX_TEXT(widget));
  properties->refdes_entry = widget;

  /* Row 1 Col 4 => Number of Slots */
  slots_label = GEDA_AVM_LABEL_NEW (_("_No. Slot:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), slots_label, 3,4,0,1, GTK_FILL,0,0,0);

  /* Row 1 Col 5 = Number of Slots Spinner */
  widget = gtk_spin_button_new_with_range(0,99,1);
  gtk_entry_set_activates_default(GTK_ENTRY(widget), TRUE);
  gtk_table_attach(GTK_TABLE(table), widget, 4,5,0,1, GTK_FILL,0,0,0);
  SetWidgetTip(widget, _(slots_tip));
  properties->slots_spin = widget;

  /* Row 1 Col 6 => Number of Pins */
  pins_label = GEDA_AVM_LABEL_NEW (_("No. _Pins:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), pins_label, 5,6,0,1, GTK_FILL,0,0,0);

  widget = gtk_spin_button_new_with_range(0,9999,1);
  gtk_entry_set_activates_default(GTK_ENTRY(widget), TRUE);
  gtk_table_attach(GTK_TABLE(table), widget, 6,7,0,1, GTK_FILL,0,0,0);
  SetWidgetTip(widget, _(pins_tip));
  properties->pins_spin = widget;

  /* Row 2 Col 1 */
  value_label = GEDA_AVM_LABEL_NEW (_("_Value:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), value_label, 0,1,1,2, GTK_FILL,0,0,0);

  /* Row 2 Col 2 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 1,2,1,2);
  SetWidgetTip(widget, _(value_tip));
  properties->value_entry = widget;

  x_dialog_edit_properties_load_refdes(properties->refdes_combo, RefDesStd);
  (GEDA_COMBO_BOX(properties->refdes_combo))->tip_column = 2;

  /* Row 2 Col 4 */
  foot_label = GEDA_AVM_LABEL_NEW (_("_Foot Print:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), foot_label, 3,4,1,2, GTK_FILL,0,0,0);

  /* Row 2 Col 4-7 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 4,7,1,2);
  SetWidgetTip(widget, _(foot_tip));
  properties->foot_entry = widget;

  /* Row 3 Col 1 */
  spice_label = GEDA_AVM_LABEL_NEW (_("Spice Type:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), spice_label, 0,1,2,3, GTK_FILL,0,0,0);

  /* Row 3 Col 2 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 1,2,2,3);
  SetWidgetTip(widget, _(spice_tip));
  properties->spice_entry = widget;

  /* Row 3 Col 4 */
  mname_label = GEDA_AVM_LABEL_NEW (_("Model Name:"), 0, 0);
  gtk_table_attach(GTK_TABLE(table), mname_label, 3,4,2,3, GTK_FILL,0,0,0);

  /* Row 3 Col 4-6 */
  widget = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(widget), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), widget, 4,7,2,3);
  SetWidgetTip(widget, _(mname_tip));
  properties->mname_entry = widget;

  /** Set the relationships between the label and their Widgets **/
  geda_label_set_mnemonic_widget (GEDA_LABEL(symbol_label), properties->symbol_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(device_label), properties->device_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(author_label), properties->author_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(version_label), properties->version_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(enable_label),  properties->version_cb);
  geda_label_set_mnemonic_widget (GEDA_LABEL(ulicense_label), properties->ulicense_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(dlicense_label), properties->dlicense_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(descr_label), properties->descr_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(doc_label), properties->doc_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(comment_label), properties->comment_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(electrical_label), properties->electrical_cb);
  geda_label_set_mnemonic_widget (GEDA_LABEL(refdes_label), properties->refdes_combo);
  geda_label_set_mnemonic_widget (GEDA_LABEL(slots_label), properties->slots_spin);
  geda_label_set_mnemonic_widget (GEDA_LABEL(pins_label), properties->pins_spin);
  geda_label_set_mnemonic_widget (GEDA_LABEL(value_label), properties->value_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(foot_label), properties->foot_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(spice_label), properties->spice_entry);
  geda_label_set_mnemonic_widget (GEDA_LABEL(mname_label), properties->mname_entry);

  atk_obj = atk_widget_linked_label_new (symbol_label, properties->symbol_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("File name entry of component symbol"));
    atk_object_set_description ( atk_obj,      symbol_tip );
  }

  atk_obj = atk_widget_linked_label_new (device_label, properties->device_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Device indentifier entry"));
    atk_object_set_description ( atk_obj,      device_tip );
  }

  atk_obj = atk_widget_linked_label_new (author_label, properties->author_entry);
    if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry for name of the author"));
    atk_object_set_description ( atk_obj,      author_tip );
  }

  atk_obj = atk_widget_linked_label_new (version_label, properties->version_entry);
  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry for the version of this symbol"));
    atk_object_set_description ( atk_obj,      version_tip );
  }

  atk_obj = atk_widget_linked_label_new (enable_label, properties->version_cb);
  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Enable or Disable version checking of this component"));
    atk_object_set_description ( atk_obj,      enable_ver_tip );
  }

  atk_obj = atk_widget_linked_label_new (ulicense_label, properties->ulicense_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("User license entry"));
    atk_object_set_description ( atk_obj,      ulicense_tip );
  }

  atk_obj = atk_widget_linked_label_new (dlicense_label, properties->dlicense_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry for distribution license of the symbol"));
    atk_object_set_description ( atk_obj,      dlicense_tip );
  }

  atk_obj = atk_widget_linked_label_new (descr_label, properties->descr_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Description of the component"));
    atk_object_set_description ( atk_obj,      descr_tip );
  }

  atk_obj = atk_widget_linked_label_new (doc_label, properties->doc_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Link to documentation for this device"));
    atk_object_set_description ( atk_obj,      doc_tip );
  }

  atk_obj = atk_widget_linked_label_new (comment_label, properties->comment_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Comments entry"));
    atk_object_set_description ( atk_obj,      comment_tip );
  }

  atk_obj = atk_widget_linked_label_new (electrical_label, properties->electrical_cb);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Checkbox to enable electrical properties"));
    atk_object_set_description ( atk_obj,      electrical_tip );
  }

  atk_obj = atk_widget_linked_label_new (refdes_label, properties->refdes_combo);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Reference designator entry"));
    atk_object_set_description ( atk_obj,      refdes_tip );
  }

  atk_obj = atk_widget_linked_label_new (slots_label, properties->slots_spin);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Spinner with entry for number of slots"));
    atk_object_set_description ( atk_obj,      slots_tip );
  }

  atk_obj = atk_widget_linked_label_new (pins_label, properties->pins_spin);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Spinner with entry for number of pins"));
    atk_object_set_description ( atk_obj,      pins_tip );
  }

  atk_obj = atk_widget_linked_label_new (value_label, properties->value_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry for the component value"));
    atk_object_set_description ( atk_obj,      value_tip );
  }

  atk_obj = atk_widget_linked_label_new (foot_label, properties->foot_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry for the component foot print"));
    atk_object_set_description ( atk_obj,      foot_tip );
  }

  atk_obj = atk_widget_linked_label_new (spice_label, properties->spice_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry for spice-type over-ride"));
    atk_object_set_description ( atk_obj,      spice_tip );
  }

  atk_obj = atk_widget_linked_label_new (mname_label, properties->mname_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry field for model file name"));
    atk_object_set_description ( atk_obj,      mname_tip );
  }

  gtk_widget_show_all (vbox);

  x_dialog_edit_properties_action_area(Dialog, properties);

  g_signal_connect (G_OBJECT (Dialog), "response",
                    G_CALLBACK (x_dialog_edit_properties_response),
                    properties);

  properties->ref_handler =
  g_signal_connect (G_OBJECT (properties->refdes_combo), "changed",
                    G_CALLBACK (x_dialog_ep_refdes_update_entry),
                    Dialog);

  properties->ver_handler =
  g_signal_connect(G_OBJECT (properties->version_cb), "toggled",
                   G_CALLBACK(x_dialog_ep_version_cb),
                   properties);

  g_signal_connect (G_OBJECT (properties->electrical_cb), "toggled",
                    G_CALLBACK (x_dialog_ep_electrical_cb),
                    properties);

  g_object_set (G_OBJECT (Dialog), DIALOG_SELECTION_TRACKER,
                x_dialog_ep_update_selection,
                NULL);

  g_object_set_data(G_OBJECT(Dialog), IDS_PROP_EDIT, properties);

  return Dialog;
}

/*! \brief Creates the Properties dialog
 *  \par Function Description
 *  This function initiates creation of the Component Properties type dialog
 *  or raises the current dialog if called when the dialog is already open.
 */
void x_dialog_edit_properties(GschemToplevel *w_current, Object *o_current)
{
  GtkWidget *Dialog;

  Dialog = w_current->prwindow;

  if (!Dialog) {

    Dialog = x_dialog_edit_properties_constructor(w_current);

    gtk_window_position(GTK_WINDOW (Dialog), GTK_WIN_POS_MOUSE);
    gtk_window_set_transient_for (GTK_WINDOW(Dialog),
                                  GTK_WINDOW(w_current->main_window));

    w_current->prwindow = Dialog;
    gtk_widget_show (Dialog);
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(Dialog));
  }

  x_dialog_ep_update_selection (w_current, o_current);
}

/*************** End of Component Properties dialog box *****************/

/** @} end group Component-Properties-Dialog */