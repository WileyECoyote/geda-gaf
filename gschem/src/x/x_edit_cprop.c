/* -*- C x_edit_cprop.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_edit_cprop.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2017 Wiley Edward Hill
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
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: September, 27, 2013
 */
/*!
 * \file x_edit_cprop.c
 * \brief A dialog box for viewing and editing component properties.
 */

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"
#include <geda_widgets.h>

#define Radio_Responder radio_responder

/** \defgroup Component-Properties-Dialog Component Properties Dialog
 *  @{
 *  \ingroup Standard-Dialogs Editing-Dialogs
 *  \image html component_properties_dialog.png
 *  \image latex component_properties_dialog.png
 *  \par
 *  The Component properties dialog has two modes of operation:
 *
 *     1.) Schematic mode.
 *     2.) Symbol mode
 *
 *  If the current file is a Schematic and the name in the symbol field
 *  has been changed then all fields except the reference designator are
 *  ignored. If a symbol with the new name exist, the current symbol is
 *  replaced with the new symbol and the reference designator from the
 *  reference designator entry field is assigned to the replacement symbol.
 *  If a symbol with the given name does not exist a warning is issued. If
 *  the symbol name has not been changed then electrical attributes are
 *  checked for non empty fields, and the corresponding attributes are
 *  either updated or created.
 *
 *  For Symbol editing, the symbol name is ignored, attribute fields are
 *  checked for non empty entries, and the corresponding attributes are
 *  either created updated.
 */

DECLARE_RADIO_TRIAD (RefDes, Std, Spice, Ieee);

typedef enum IDE_CP_ControlID {
        RefDes,
        RefDesStd,
        RefDesSpice,
        RefDesIeee
} ControlID;

/* Strings for dialog widgets */
static WidgetStringData DialogStrings[] = {
        { "RefDesLabel",        " ",             N_("Set preloaded reference designators.")},
        { "RefDesStdRadio",     N_("Standard"),  N_("Use standard designations")},
        { "RefDesSpiceRadio",   N_("Spice"),     N_("Use designations for SPICE")},
        { "RefDesIeeeRadio",    N_("IEEE-315"),  N_("Use IEEE-315 sanctioned designators")},
  { NULL, NULL, NULL},
};

/*!
 * \brief Component Properties Internal Check and Update Attributes
 * \par Function Description
 *  This function is called with or without and \a Object argument, when
 *  the \a object argument is present then attributes searching is limited
 *  to \a Object, otherwise all <b>Text</b> objects on the page are searched.
 *  If the new value is NULL or has zero length then the function does
 *  nothing. Otherwise the function attempts to find an attribute with
 *  the name key, and if found the attribute value is compared to new_
 *  value and changed if strings are difference. If an attribute is not
 *  found then a new attribute is created with the form key=value.
 *
 * \param [in] w_current Gschem toplevel structure.
 * \param [in] object    Pointer to an <b>Object</b> to search, also used as a flag
 * \param [in] key       The name of the attribute
 * \param [in] new_value String value that attribute should be assigned
 *
 * \return boolean TRUE is a change was made, otherwise FALSE
 */
static bool x_dialog_ep_check_update_attribs (GschemToplevel *w_current,
                                              GedaObject     *object,
                                              const char     *key,
                                              const char     *new_value)
{
  bool result = FALSE;

  if (new_value != NULL && strlen(new_value) !=0 ) {

    const GList *attribs;
    GedaObject  *a_current;
    char        *o_value;

    if (object) {

      /* This is object->attribs, do not free this list */
      attribs = geda_object_get_attached(object);

      /* If object, then look for an attached attribute */
      a_current = geda_find_attrib_by_name (attribs, key, 0);

    }
    else {

      Page *p_current = gschem_toplevel_get_current_page(w_current);

      /* symbol mode, get all objects on the page */
      attribs   = geda_struct_page_get_objects(p_current);
      a_current = NULL;

    }

    if (a_current) {

      if(geda_attrib_object_get_name_value (a_current, NULL, &o_value)) {
        if (strcmp(o_value, new_value) != 0) {
          geda_attrib_object_set_value (a_current, key, new_value);
          geda_text_object_recreate(a_current);
          result = TRUE;
        }
        GEDA_FREE(o_value);
      }
    }
    else {

      GList *all_butes;

      if (object) {
        /* Not attached, check all attributes */
        all_butes = geda_attrib_return_attribs(object);
      }
      else {
        all_butes = geda_object_get_objects_by_type (attribs, OBJ_TEXT);
      }

      a_current = geda_find_attrib_by_name (all_butes, key, 0);
      g_list_free (all_butes);

      if (a_current) {
        if(geda_attrib_object_get_name_value (a_current, NULL, &o_value)) {
          if (strcmp(o_value, new_value) != 0) {
            geda_attrib_object_set_value (a_current, key, new_value);
            geda_text_object_recreate(a_current);
            result = TRUE;
          }
          GEDA_FREE(o_value);
        }
      }
      else {

        char *newtext = geda_strconcat(key, "=", new_value, NULL);
        o_attrib_add_attrib(w_current, newtext, VISIBLE, SHOW_NAME_VALUE, -1, NULL);
        GEDA_FREE (newtext);
        result = TRUE;
      }
    }
  }
  return result;
}

/*!
 * \brief Component Properties Internal Revise Electrical Attributes
 * \par Function Description
 *  This function is called with or without and \a Object argument, which is
 *  passed as a flag to x_dialog_ep_check_update_attribs to indicate the
 *  editing mode, with schematic or symbol. The function retrieves strings
 *  from the entries in the lower halve of the dialog, fields controlled by
 *  the electrical check-box, and calls x_dialog_ep_check_update_attribs
 *  for each field.
 *
 * \param [in] w_current  Gschem toplevel structure.
 * \param [in] properties Pointer to a Component Dialog data structure
 * \param [in] object     Pointer to an<b>Object</b> to search, also used as a flag
 *
 * \return boolean TRUE is a change was made, otherwise FALSE
 *
 * \sa x_dialog_ep_revise_symbol_attribs
 */
static bool x_dialog_ep_revise_elect_attribs(GschemToplevel *w_current,
                                             property_data  *properties,
                                             GedaObject     *object)
{
  bool result = 0;

  if (GetToggleState(properties->electrical_cb)) {

    const char *str_val;

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

/*!
 * \brief Component Properties Internal Revise Non-Electrical Attributes
 * \par Function Description
 *  This function retrieves strings from the entries in the upper halve of
 *  the dialog, fields not controlled by the electrical check-box, and calls
 *  x_dialog_ep_check_update_attribs for each field. The functions returns
 *  by calling x_dialog_ep_revise_elect_attribs with a NULL argument of the
 *  object to indicate symbol editing mode. This function is not called when
 *  editing schematic files.
 *
 * \param [in] w_current  Gschem toplevel structure.
 * \param [in] properties Pointer to a Component Dialog data structure
 *
 * \return boolean TRUE is a change was made, otherwise FALSE
 *
 * \sa x_dialog_ep_revise_elect_attribs
 */
static bool x_dialog_ep_revise_symbol_attribs (GschemToplevel *w_current,
                                               property_data  *properties)
{
  bool  result = FALSE;
  const char *str_val;
  Page *p_current;

  /* Check if symbol file name changed */
  str_val   = GetEntryText ( properties->symbol_entry );
  p_current = gschem_toplevel_get_current_page(w_current);

  if (str_val && (strcmp (str_val, p_current->filename) != 0)) {
    result = geda_page_rename (p_current, str_val);
    if (result) {
      u_log_message(_("Symbol renamed, cannot be undone\n"));
    }
  }

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
                                              GedaObject     *object)
{
  bool result;
  const char *str_val;

  /* get the name from the text entries of the dialog */
  str_val = GetEntryText ( properties->version_entry );
  result  = x_dialog_ep_check_update_attribs (w_current, object,
                                              "symversion", str_val);
  return result;
}

/*!
 * \brief Component Properties Dialog Apply Settings
 * \par Function Description
 *  This function applies the settings in the properties dialog to the
 *  selected objects or to the symbol page depending on whether object
 *  has a value. The function handles the case of exchanging the symbol
 *  and calls helper functions for attribute handling.
 *
 * \param [in] dialog     Pointer to a Component Dialog instance.
 * \param [in] properties Pointer to a Component Dialog data structure
 */
static void x_dialog_edit_properties_ok(GtkWidget     *dialog,
                                        property_data *properties)
{
  GschemToplevel *w_current;

  bool  changed;
  Page *p_current;

  changed   = FALSE;
  w_current = GSCHEM_DIALOG(dialog)->w_current;
  p_current = gschem_toplevel_get_current_page(w_current);

  if (geda_struct_page_is_symbol_file(p_current)) {
    changed = x_dialog_ep_revise_symbol_attribs (w_current, properties);
  }
  else {

   GedaObject *o_current;

    o_current =GEDA_OBJECT_GET_DATA (dialog, "object");

    if (o_current != NULL && o_current->type == OBJ_COMPLEX) {

      GedaComplex *o_complex;
      const char  *filename;
      bool         state;

      o_complex = o_current->complex;

      /* Note that we do not actually embed/unembed objects here, the
       * embedding or unembedding is performed when the file is saved */
      state = GetToggleState(properties->embed_cb);

      if (o_complex->is_embedded != state) {
        geda_complex_set_is_embedded(o_complex, state);
        changed = TRUE;
      }

      state = !GetToggleState(properties->lock_cb);
      if (o_current->selectable != state) {
        o_edit_set_selectable(w_current, o_current, state);
        changed = TRUE;
      }

      /* Mirrored is for information only */

      /* get the name from the text entries of the dialog */
      filename  = GetEntryText ( properties->symbol_entry );

      if (filename && (strcmp (filename, o_complex->filename) != 0)) {

        const CLibSymbol *clib;

        clib = geda_struct_clib_get_symbol_by_name (filename);
        geda_struct_clib_symbol_invalidate_data (clib);

        if (clib == NULL) {
          const char *log_msg1 = _("Could not find symbol");
          const char *log_msg2 = _("in library. Update failed");
          geda_log ("%s \"%s\" %s.\n", log_msg1, filename, log_msg2);
        }
        else {

          const GList *new_butes;
          const GList *old_ribs;
          const GList *iter;
          GedaObject  *o_new;
          GedaObject  *attrib;
          const char  *refdes;

          /* do not use library, the selection change will trigger an event */
          Current_Selection->glist = g_list_remove (Current_Selection->glist, o_current);

          /* Create new object and set embedded */
          o_new = geda_complex_object_new (w_current->toplevel,
                                           o_complex->x,
                                           o_complex->y,
                                           o_complex->angle,
                                           o_complex->mirror,
                                           clib, filename, 1);

          if (geda_complex_object_is_embedded (o_current)) {
            geda_object_embed (o_new);
          }

          o_new->selectable = o_current->selectable;

          geda_attrib_object_freeze_hooks (o_new);

          new_butes = geda_complex_object_promote_attribs (w_current->toplevel, o_new);

          /* If new and old object have a "slot" then set new to match old */
          /* Don't care what the value new is, just if it has one */
          if (geda_attrib_first_attrib_by_name (o_new, "slot")) {

            char *value;

            attrib = geda_attrib_first_attrib_by_name (o_current, "slot");
            if (attrib) {
              /* Get the old slot value */
              geda_attrib_object_get_name_value (attrib, NULL, &value);
              /* Set the slot value of the new object to the old value */
              geda_attrib_object_set_value (attrib, "slot", value);
              GEDA_FREE(value);
            }
          }

           /* Add new attributes to page */
          geda_struct_page_append_list (p_current, new_butes);

          /* Update pinnumbers for current slot */
          geda_struct_slot_update_object (o_new);

          geda_attrib_object_thaw_hooks (o_new);

          old_ribs = geda_object_get_attached (o_current);

          for (iter = old_ribs; iter != NULL; iter = g_list_next (iter)) {

            GedaObject *obj = (GedaObject*) iter->data;

            Current_Selection->glist = g_list_remove (Current_Selection->glist, obj);
            geda_struct_page_remove_object (p_current, obj);
            geda_struct_object_release (obj);
          }

          /* Replace old Object with new Object */
          geda_struct_page_replace_object (p_current, o_current, o_new);

          geda_struct_object_release (o_current);

          /* Set refdes on new complex to what is in the dialog entry */
          refdes  = GetEntryText (properties->refdes_entry );

          attrib = geda_attrib_first_attrib_by_name (o_new, "refdes");
          if (attrib) {
            geda_attrib_object_set_value (attrib, "refdes", refdes);
            geda_text_object_recreate(attrib);
          }

          /* Select the new Object */
          geda_object_selection_add (Current_Selection, o_new);

          changed = TRUE;
        }
      }
      else {

        if (x_dialog_ep_revise_elect_attribs(w_current,
                                             properties,
                                             o_current))
        {
          changed = TRUE;
        }

        if (x_dialog_ep_check_symver_attribs(w_current,
                                             properties,
                                             o_current))
        {
          changed = TRUE;
        }
      }
    }
  }

  if (changed) {
    geda_page_set_changed (p_current, TRUE);
    o_undo_savestate (w_current, UNDO_ALL);
  }
}

/*!
 * \brief Response Function for the Component Properties Dialog
 * \par Function Description
 *  This function handles the response to the Component Properties dialog.
 *  This is called to either retrieve and process data in the dialog or
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
    BUG_IMSG ("unhandled case", response);
  }

  i_status_set_state (w_current, SELECT);

}

static void x_dialog_ep_refdes_update_entry (GtkWidget *widget,
                                             GtkWidget *dialog)
{
  property_data *properties;
  GedaObject    *o_current;

  properties = GEDA_OBJECT_GET_DATA (dialog, IDS_PROP_EDIT);
  o_current  = GEDA_OBJECT_GET_DATA (dialog, "object");

  if (o_current != NULL && o_current->type == OBJ_COMPLEX) {

   GedaObject *attrib = geda_attrib_first_attrib_by_name (o_current, "refdes");

    if (attrib) {

      const char *curr_ref = geda_utility_refdes_return_numeric (attrib);

      if (curr_ref) {

        char *new_text;
        char *prefix;

        prefix   = geda_refdes_get_prefix(attrib);
        new_text = geda_strconcat(prefix, curr_ref, NULL);

        SetEntryText(properties->refdes_entry, new_text);

        GEDA_FREE (new_text);
        GEDA_FREE (prefix);
      }
    }
  }
}

/*!
 * \brief Component Properties Dialog Electrical Check-box Callback
 *  Enable or disabled sensitivities of widgets within the electrical
 *  frame depending on the state of the check-box.
 *
 * \param [in] check_butt Pointer to the CheckBox widget
 * \param [in] data       Pointer to a Component Dialog data structure
 */
static void x_dialog_ep_electrical_cb (GtkWidget *check_butt, void *data)
{
  property_data *properties = data;
  bool           state      = GetToggleState(check_butt);

  void set_sensitive (GtkWidget *widget, void *nothing){
    gtk_widget_set_sensitive(widget, state);
  }
  geda_container_foreach (properties->elect_table, set_sensitive, NULL);
}

/*!
 * \brief Component Properties Dialog Enable Version Check-box Callback
 *  Enable or disabled sensitivities of widgets within the electrical
 *  frame depending on the state of the check-box.
 *
 * \param [in] check_butt Pointer to the CheckBox widget
 * \param [in] data       Pointer to a Component Dialog data structure
 *
 */
static void x_dialog_ep_version_cb (GtkWidget *check_butt, void *data)
{
  property_data *properties = data;
  bool           state      = GetToggleState(check_butt);
  const char    *str_val    = GetEntryText(properties->version_entry);

  if (strlen(str_val)) {

    char *str;

    if (!state && (str_val[0] != '-')) {
      const char *dash = "-";
      str = geda_strconcat(&dash[0], str_val,NULL);
    }
    else if (state && (str_val[0] == '-')) {
      str = geda_utility_string_strdup(&str_val[1]);
    }
    else{
      str = NULL;
    }

    if (str) {
      SetEntryText (properties->version_entry, str);
      GEDA_FREE(str);
    }
  }
}

/*!
 * \brief Handle selection change event for x_dialog_edit_properties
 * \par Function Description
 *  Updates component information in dialog widgets when the selection
 *  changes.
 *
 * \param w_current  pointer to GschemToplevel context
 * \param object     pointer to a selected Object.
 * \param properties pointer to property_data structure
 */
static void x_dialog_ep_component_change(GschemToplevel *w_current,
                                         GedaObject     *object,
                                         property_data  *properties)
{
  GedaObject  *a_current;
  const GList *attribs;
        GList *all_butes;
  const char  *filename;
        char  *value;

  void (*set_entry)(const char *name, GtkWidget *entry);

  void entry_page_setter(const char *name, GtkWidget *entry) {

    a_current = geda_find_attrib_by_name (all_butes, name, 0);
    if (a_current) {
      geda_attrib_object_get_name_value (a_current, NULL, &value);
      SetEntryText(entry, value);
      GEDA_FREE(value);
    }
    else {
      SetEntryText(entry, NULL);
    }
  }

  void entry_object_setter(const char *name, GtkWidget *entry) {

    a_current = geda_find_attrib_by_name (attribs, name, 0);
    if (a_current) {
      geda_attrib_object_get_name_value (a_current, NULL, &value);
      SetEntryText(entry, value);
      GEDA_FREE(value);
    }
    else {
      a_current = geda_find_attrib_by_name (all_butes, name, 0);
      if (a_current) {
        geda_attrib_object_get_name_value (a_current, NULL, &value);
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

    filename = geda_complex_object_get_filename(object);

    SetEntryText(properties->symbol_entry, filename);

    /* Get the full name */
    clib = geda_struct_clib_get_symbol_by_name (filename);

    fullname = geda_struct_clib_symbol_get_filename(clib);

    /* set the tooltip as the full file name */
    SetWidgetTip(properties->symbol_entry, fullname);

    GEDA_FREE(fullname);

    attribs   = geda_object_get_attached(object);
    all_butes = geda_attrib_return_attribs(object);

    set_entry = entry_object_setter;
  }
  else {

    Page *page;

    page = gschem_toplevel_get_current_page(w_current);

    if (GEDA_IS_PAGE(page)) {

      filename = geda_file_get_basename(page->filename);
      SetEntryText(properties->symbol_entry, filename);

      /* set the tooltip as the full file name */
      SetWidgetTip(properties->symbol_entry, page->filename);

      /* symbol mode, get all objects on the page */
      attribs   = geda_struct_page_get_objects(page);
      all_butes = geda_object_get_objects_by_type (attribs, OBJ_TEXT);
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

  if (geda_find_attrib_by_name (all_butes, "graphical", 0)) {

    /* Then this is a graphical object */
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
  else { /* This is a non-graphical object */

    int        pin_count;

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
    a_current = geda_find_attrib_by_name (all_butes, "numslots", 0);
    if (a_current) {
      if (geda_attrib_object_get_name_value (a_current, NULL, &value)) {
        SetSpinValue(properties->slots_spin, atoi(value));
        GEDA_FREE(value); /* likely a NULL terminated zero */
      }
    }
    else {
      SetSpinValue(properties->slots_spin, 0);
    }

    a_current = geda_find_attrib_by_name (all_butes, "pins", 0);

    if (a_current) {

      if (geda_attrib_object_get_name_value (a_current, NULL, &value)) {
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


/*!
 * \brief Handle selection change event for x_dialog_edit_properties
 * \par Function Description
 *  Updates component information in dialog widgets when the selection
 *  changes.
 *
 * \param w_current  pointer to GschemToplevel context
 * \param properties pointer to property_data structure
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

}

/*!
 * \brief Component Properties Dialog Check-box Callback
 *  Enable or disabled sensitivities of widgets within the electrical
 *  frame depending on the state of the check-box.
 *
 * \param [in] properties Pointer to property_data structure
 * \param [in] state      Boolean value to set main sensitivities
 * \param [in] sch        Boolean value to set electrical frame sensitivities
 */
static void x_dialog_ep_set_sensitive (property_data *properties, bool state, bool sch)
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
  geda_container_foreach (properties->elect_table, set_sensitive, NULL);

  gtk_widget_set_sensitive(properties->opt_frame, sch);
}

/*!
 * \brief Handle selection change event for x_dialog_edit_properties
 * \par Function Description
 *  Called when the selection changes. The functions calls
 *  x_dialog_ep_component_change to update the data fields
 *
 * \param w_current pointer to GschemToplevel context
 * \param object    pointer to a selected Object.
 */
static void x_dialog_ep_update_selection (GschemToplevel *w_current,
                                          GedaObject     *object)
{
  GtkWidget     *dialog;
  property_data *properties;
  Page          *p_current;

  /* Get ptr to the data structure */
  dialog     = w_current->prwindow;
  properties = GEDA_OBJECT_GET_DATA (dialog, IDS_PROP_EDIT);

  /* Get ptr to the current page */
  p_current  = gschem_toplevel_get_current_page(w_current);

  if (object != NULL && object->type == OBJ_COMPLEX) {

    SetToggleState(properties->embed_cb, object->complex->is_embedded);
    SetToggleState(properties->mirror_cb, object->complex->mirror);
    SetToggleState(properties->lock_cb, !object->selectable);

    x_dialog_ep_set_sensitive(properties, TRUE, TRUE);
    x_dialog_ep_component_change(w_current, object, properties);
    GEDA_OBJECT_SET_DATA(dialog, object, "object");
    gtk_widget_grab_focus(properties->symbol_entry);
  }
  else if (geda_struct_page_is_symbol_file(p_current)) {

    /* This widgets do not apply to symbols so uncheck boxes */
    SetToggleState(properties->embed_cb, FALSE);
    SetToggleState(properties->mirror_cb, FALSE);
    SetToggleState(properties->lock_cb, FALSE);

    x_dialog_ep_set_sensitive(properties, TRUE, FALSE);
    x_dialog_ep_component_change(w_current, NULL, properties);
    GEDA_OBJECT_SET_DATA(dialog, NULL, "object");
    gtk_widget_grab_focus(properties->author_entry);
  }
  else {
    x_dialog_ep_no_selection(w_current, properties);
    x_dialog_ep_set_sensitive(properties, FALSE, FALSE);
    GEDA_OBJECT_SET_DATA(dialog, NULL, "object");
  }
}

/*!
 * \brief Component Properties Dialog Construction Helper to Load Combo
 * \par Function Description
 *  Called during construction to load string into the combo text box for
 *  reference designators.
 *
 * \param dialog  pointer to the Properties Dialog
 * \param type    integer type is the set of desginator to be loaded.
 */
static void x_dialog_edit_properties_load_refdes(GtkWidget *dialog, int type)
{
  property_data    *properties = GEDA_OBJECT_GET_DATA (dialog, IDS_PROP_EDIT);
  GedaComboBoxText *combo      = (GedaComboBoxText*) properties->refdes_combo;
  const GedaRefDes *designators;

  int   index;

  switch ( type ) {
    case RefDesStd:
      designators = geda_utility_refdes_get_standard();
      break;

    case RefDesSpice:
      designators = geda_utility_refdes_get_spice();
      break;

    case RefDesIeee:
      designators = geda_utility_refdes_get_ieee();
      break;

    default:
      designators = geda_utility_refdes_get_standard();
      break;
  }

  geda_combo_box_text_remove_all(combo);

  index = 0;
  while (designators[index].designator) {

    const char *ref;
    const char *descr;

    ref = designators[index].designator;
    descr = designators[index].description;
    geda_combo_box_text_append_pair(combo, ref, descr);
    ++index;
  }

  x_dialog_ep_refdes_update_entry(properties->refdes_combo, dialog);
}

/*!
 * \brief Function to toggle radio images
 * \par Function Description
 *  This function changes the images of controls created with
 *  create_geda_switch to the opposite state, i.e. if ON use
 *  OFF image and if OFF use ON image.
 */
static void radio_responder(GtkWidget *widget,  int control)
{
  bool state = GET_SWITCH_STATE (widget);

  if ((GTK_IS_BUTTON(widget)) && (state != TRUE)) {

    x_dialog_set_bulb_on(widget);

    switch ( control ) {
      case RefDesStd:
        x_dialog_set_bulb_off(RefDesIeeeRadio); x_dialog_set_bulb_off(RefDesSpiceRadio);
        break;

      case RefDesIeee:
        x_dialog_set_bulb_off(RefDesStdRadio); x_dialog_set_bulb_off(RefDesSpiceRadio);
        break;

      case RefDesSpice:
        x_dialog_set_bulb_off(RefDesStdRadio); x_dialog_set_bulb_off(RefDesIeeeRadio);
        break;

      default:
        break;
    }
  }

  return;
}


static void x_dialog_edit_properties_load_std_des(GtkWidget *widget,
                                                  GtkWidget *dialog)
{
  x_dialog_edit_properties_load_refdes(dialog, RefDesStd);
}

static void x_dialog_edit_properties_load_spice_des(GtkWidget *widget,
                                                    GtkWidget *dialog)
{
  x_dialog_edit_properties_load_refdes(dialog, RefDesSpice);
}

static void x_dialog_edit_properties_load_ieee_des(GtkWidget *widget,
                                                   GtkWidget *dialog)
{
  x_dialog_edit_properties_load_refdes(dialog, RefDesIeee);
}

/*!
 * \brief Emit GEDA_RESPONSE_CLOSE signal.
 * \par Function Description
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

/*!
 * \brief Emit COMPSELECT_RESPONSE_HIDE signal.
 * \par Function Description
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
  geda_container_remove(Dialog->vbox, action_area);

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

  alignment = g_object_new (GTK_TYPE_ALIGNMENT,
                            "right-padding", 0,
                            "left-padding",  50,
                            "xscale",        1.0,
                            "yscale",        0.0,
                            "xalign",        1.0,
                            "yalign",        0.5,
                            NULL);

  g_object_set (alignment, "visible", TRUE, NULL);
  gtk_box_pack_end (GTK_BOX (action_hbox), alignment, TRUE, TRUE, 0);

  /* Create a Horizontal Box for the buttons to go into */
  butt_hbox = gtk_hbox_new(FALSE, 0);
  g_object_set (butt_hbox, "visible", TRUE, NULL);
  geda_container_add (alignment, butt_hbox);

  /* Create and connect the Close and Apply Buttons */
  GtkWidget *close_butt = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
  GtkWidget *apply_butt = gtk_button_new_from_stock (GTK_STOCK_APPLY);

  g_signal_connect (RefDesStdRadio, "pressed",
                    G_CALLBACK (x_dialog_edit_properties_load_std_des),
                    ThisDialog);

  g_signal_connect (RefDesSpiceRadio, "pressed",
                    G_CALLBACK (x_dialog_edit_properties_load_spice_des),
                    ThisDialog);

  g_signal_connect (RefDesIeeeRadio, "pressed",
                    G_CALLBACK (x_dialog_edit_properties_load_ieee_des),
                    ThisDialog);

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

  /* Set the alternative button order (ok, cancel, help) for other systems
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_CLOSE,
                                          -1);
 */

  gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog), GEDA_RESPONSE_ACCEPT);
  gtk_widget_grab_default (apply_butt);
}

/*!
 * \brief Component Properties Dialog Constructor
 * \par Function Description
 *  Called to construct the Componenent Properties dialog box.
 *
 * \param w_current pointer to GschemToplevel data structure
 *
 * \return GtkDialog pointer to a new Componenent Properties Dialog
 */
static
GtkWidget* x_dialog_edit_properties_constructor (GschemToplevel *w_current)
{
  AtkObject *atk_obj;
  GtkWidget *dialog;
  GtkWidget *alignment;
  GtkWidget *frame;
  GtkWidget *hbox;
  GtkWidget *table;
  GtkWidget *vbox;
  GtkWidget *verbox;
  GtkWidget *mirbox;
  GtkWidget *lockbox;
  GtkWidget *embedbox;
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
  GtkWidget *options_label;
  GtkWidget *embed_label;
  GtkWidget *lock_label;
  GtkWidget *mirror_label;
  GtkWidget *electrical_label;
  GtkWidget *refdes_label;
  GtkWidget *slots_label;
  GtkWidget *pins_label;
  GtkWidget *value_label;
  GtkWidget *foot_label;
  GtkWidget *spice_label;
  GtkWidget *mname_label;

  property_data *properties;

  const char *device_tip     = "device identifier";
  const char *author_tip     = "The symbols author";
  const char *version_tip    = "version of symbol";
  const char *enable_ver_tip = "Enable version checking for this component";
  const char *ulicense_tip   = "use-license attribute";
  const char *dlicense_tip   = "redistribution license for the symbol";
  const char *descr_tip      = "Component description";
  const char *doc_tip        = "Device documentation";
  const char *comment_tip    = "Addition information related to this symbol";
  const char *embed_tip      = "Whether the symbol is embedded";
  const char *lock_tip       = "Whether the symbol is selectable";
  const char *mirror_tip     = "Ready Only, whether the symbol is mirrored";
  const char *electrical_tip = "Disable to set electricaly related attribute";
  const char *refdes_tip     = "Component designator";
  const char *slots_tip      = "Number of slots, equal device per package";
  const char *pins_tip       = "Number of pins per package";
  const char *value_tip      = "The value of the component";
  const char *foot_tip       = "Foot print used during component layout";
  const char *spice_tip      = "The spice-type attributes over-rides the refdes";
  const char *mname_tip      = "Name of the model for this component";

  dialog = gschem_dialog_new_empty(_("Edit Component Properties"),
                                          w_current->main_window,
         /* nonmodal Editing Dialog */    GSCHEM_MODELESS_DIALOG,
                                          IDS_PROP_EDIT, w_current);

  properties = (property_data*) GEDA_MEM_ALLOC (sizeof(struct st_property_data));

  vbox = GTK_DIALOG(dialog)->vbox;

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
  geda_container_add (verbox, enable_label);

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

  /* ----------------------- Options ------------------------- */

  frame = g_object_new (GTK_TYPE_FRAME, "label", "", NULL);
  gtk_box_pack_start(GTK_BOX(vbox), frame,
                     FALSE, FALSE, DEFAULT_WIDGET_SPACING);

  /* Create Checkbox and use as Label for the Frame, note that we
   * can not use gtk_check_button_new_with_label because we need
   * access to label for Atk. */
  hbox = gtk_hbox_new(FALSE, 0);

  options_label = GEDA_AVM_LABEL_NEW (_("Options:"), 0, 0);
  geda_container_add (hbox, options_label);

  gtk_frame_set_label_widget (GTK_FRAME(frame), hbox);

  alignment = g_object_new (GTK_TYPE_ALIGNMENT,
                            "right-padding",
                            DIALOG_H_SPACING,
                            "left-padding",
                            DIALOG_H_SPACING,
                            "xscale", 0.5,
                            "yscale", 0.5,
                            "xalign", 0.5,
                            "yalign", 0.5,
                            NULL);

  geda_container_add (frame, alignment);

  properties->opt_frame = frame;

  /* Create a second table and put in the alignment */
  table = gtk_table_new (1, 3, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING + 30);
  geda_container_add (alignment, table);

    /* Row 1 Col 1 */
  embedbox = gtk_hbox_new(FALSE, 0);
  g_object_set (embedbox, "visible", TRUE, NULL);

  widget = gtk_check_button_new ();
  gtk_box_pack_start (GTK_BOX (embedbox), widget,  FALSE, FALSE, 5);
  SetWidgetTip(widget, _(embed_tip));
  properties->embed_cb = widget;

  embed_label = GEDA_AVM_LABEL_NEW (_("_Embedded"), 0, 1);
  geda_container_add (embedbox, embed_label);

  gtk_box_set_spacing (GTK_BOX (embedbox), 2);
  g_object_set (embedbox, "visible", TRUE, NULL);
  gtk_table_attach_defaults(GTK_TABLE(table), embedbox, 0,1,0,1);

  /* Row 1 Col 2 */
  lockbox = gtk_hbox_new(FALSE, 0);
  g_object_set (lockbox, "visible", TRUE, NULL);

  widget = gtk_check_button_new ();
  gtk_box_pack_start (GTK_BOX (lockbox), widget,  FALSE, FALSE, 5);
  SetWidgetTip(widget, _(lock_tip));
  properties->lock_cb = widget;

  lock_label = GEDA_AVM_LABEL_NEW (_("_Locked"), 0, 1);
  geda_container_add (lockbox, lock_label);

  gtk_box_set_spacing (GTK_BOX (lockbox), 2);
  g_object_set (lockbox, "visible", TRUE, NULL);
  gtk_table_attach_defaults(GTK_TABLE(table), lockbox, 1,2,0,1);

  /* Row 1 Col 3 */
  mirbox = gtk_hbox_new(FALSE, 0);
  g_object_set (mirbox, "visible", TRUE, NULL);

  gtk_widget_set_sensitive(mirbox, FALSE);

  widget = gtk_check_button_new ();
  gtk_box_pack_start (GTK_BOX (mirbox), widget,  FALSE, FALSE, 5);
  SetWidgetTip(widget, _(mirror_tip));
  properties->mirror_cb = widget;

  mirror_label = GEDA_AVM_LABEL_NEW (_("_Mirrored"), 0, 1);
  geda_container_add (mirbox, mirror_label);

  gtk_box_set_spacing (GTK_BOX (mirbox), 2);
  g_object_set (mirbox, "visible", TRUE, NULL);
  gtk_table_attach_defaults(GTK_TABLE(table), mirbox, 2,3,0,1);

  /* --------------------- Electrical ------------------------ */

  frame = g_object_new (GTK_TYPE_FRAME, "label", "", NULL);
  gtk_box_pack_start(GTK_BOX(vbox), frame,
                     FALSE, FALSE, DEFAULT_WIDGET_SPACING);

  /* Create Checkbox and use as Label for the Frame, note that we
   * can not use gtk_check_button_new_with_label because we need
   * access to label for Atk. */
  hbox = gtk_hbox_new(FALSE, 0);

  widget = gtk_check_button_new ();
  geda_container_add (hbox, widget);
  SetWidgetTip(widget, _(electrical_tip));
  gtk_toggle_button_set_active ((GtkToggleButton*)widget, FALSE);
  properties->electrical_cb = widget;

  electrical_label = GEDA_AVM_LABEL_NEW (_("_Electrical:"), 0, 0);
  geda_container_add (hbox, electrical_label);

  gtk_frame_set_label_widget (GTK_FRAME(frame), hbox);

  alignment = g_object_new (GTK_TYPE_ALIGNMENT,
                            "right-padding",
                            DIALOG_H_SPACING,
                            "left-padding",
                            DIALOG_H_SPACING,
                            "xscale", 0.0,
                            "yscale", 0.0,
                            "xalign", 0.5,
                            "yalign", 0.5,
                            NULL);

  geda_container_add (frame, alignment);

  /* Create a third table and put in the alignment */
  table = gtk_table_new (4, 7, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  geda_container_add (alignment, table);

  /* Save a reference to the table */
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
  widget = geda_combo_box_text_get_entry_widget(GEDA_COMBO_BOX_TEXT(widget));
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

  //x_dialog_edit_properties_load_refdes(dialog, RefDesStd);
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

  /* Set the relationships between the label and their Widgets */
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
  geda_label_set_mnemonic_widget (GEDA_LABEL(embed_label), properties->embed_cb);
  geda_label_set_mnemonic_widget (GEDA_LABEL(lock_label), properties->lock_cb);
  geda_label_set_mnemonic_widget (GEDA_LABEL(mirror_label), properties->mirror_cb);
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
    const char *symbol_tip     = "Symbol file name";
    atk_object_set_name        ( atk_obj,   _("File name entry of component symbol"));
    atk_object_set_description ( atk_obj,      symbol_tip );
  }

  atk_obj = atk_widget_linked_label_new (device_label, properties->device_entry);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Device identifier entry"));
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
    atk_object_set_name        ( atk_obj,   _("Entry for comments"));
    atk_object_set_description ( atk_obj,      comment_tip );
  }

  atk_obj = atk_widget_linked_label_new (embed_label, properties->embed_cb);
  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Embed symbol in schematic checkbox"));
    atk_object_set_description ( atk_obj,      embed_tip );
  }

  atk_obj = atk_widget_linked_label_new (lock_label, properties->lock_cb);
  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Lock symbol in schematic checkbox"));
    atk_object_set_description ( atk_obj,      lock_tip );
  }

  atk_obj = atk_widget_linked_label_new (mirror_label, properties->mirror_cb);
  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Checkbox indicating if symbol is mirrored"));
    atk_object_set_description ( atk_obj,      mirror_tip );
  }

  atk_obj = atk_widget_linked_label_new (electrical_label, properties->electrical_cb);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Checkbox to enable electrical properties"));
    atk_object_set_description ( atk_obj,      electrical_tip );
  }

  atk_obj = atk_widget_linked_label_new (refdes_label, properties->refdes_combo);

  if ( atk_obj ) {
    atk_object_set_name        ( atk_obj,   _("Entry for reference designator"));
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
    atk_object_set_name        ( atk_obj,   _("Entry for model file name"));
    atk_object_set_description ( atk_obj,      mname_tip );
  }

  gtk_widget_show_all (vbox);

  x_dialog_edit_properties_action_area(dialog, properties);

  g_signal_connect (G_OBJECT (dialog), "response",
                    G_CALLBACK (x_dialog_edit_properties_response),
                    properties);

  properties->ver_handler =
  g_signal_connect(G_OBJECT (properties->version_cb), "toggled",
                   G_CALLBACK(x_dialog_ep_version_cb),
                   properties);

  g_signal_connect (G_OBJECT (properties->electrical_cb), "toggled",
                    G_CALLBACK (x_dialog_ep_electrical_cb),
                    properties);

  g_object_set (G_OBJECT (dialog), DIALOG_SELECTION_TRACKER,
                x_dialog_ep_update_selection,
                NULL);

  GEDA_OBJECT_SET_DATA(dialog, properties, IDS_PROP_EDIT);

  return dialog;
}

/*!
 * \brief Creates the Properties dialog
 * \par Function Description
 *  This function initiates creation of the Component Properties type dialog
 *  or raises the current dialog if called when the dialog is already open.
 */
void x_dialog_edit_properties(GschemToplevel *w_current, GedaObject *o_current)
{
  GtkWidget *dialog;

  dialog = w_current->prwindow;

  if (!dialog) {

    dialog = x_dialog_edit_properties_constructor(w_current);

    gtk_window_set_position(GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
    gtk_window_set_transient_for (GTK_WINDOW(dialog), w_current->main_window);

    w_current->prwindow = dialog;
    gtk_widget_show (dialog);
  }
  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(dialog));
  }

  x_dialog_ep_update_selection (w_current, o_current);
}

/*************** End of Component Properties dialog box *****************/

/** @} end group Component-Properties-Dialog */