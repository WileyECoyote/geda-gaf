/* -*- C o_slot.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
/*!
 * \file o_slot.c
 * \brief Low-level module for editing Slot Properties
 */

/** \defgroup Slot-Operations Slot Operations
 *  @{
 *  \ingroup Editing-Operations
 *
 *  \par This group contains routines for manipulating Slot Attributes.
 */

#include <gschem.h>
#include <geda_debug.h>

#define MAX_SLOT_SIZE 10

/*! \brief Edit Complex Object's Slot Attribute
 *  \par Function Description
 *  This is really a pre-dialog launch for the Edit Slot Dialog.
 *  The function insures the selected object is a complex and
 *  checks for an existing slot attribute. Either the found slot
 *  attribute or a fictitious values is passed to the Edit-Slot
 *  Dialog.
 *
 *  \param w_current pointer to GschemToplevel context
 *  \param object    pointer to a "slotable" Object.
 */
void o_slot_start (GschemToplevel *w_current, GedaObject *object)
{
  char *slot_count;
  char *slot_value;

  /* single object for now */
  if (object->type != OBJ_COMPLEX)
    return;

  slot_count = geda_attrib_search_object_by_name (object, "numslots", 0);

  if (slot_count == NULL) {
    /* we didn't find a slot=? attribute, make something up */
    slot_count = geda_utility_string_strdup ("0");
  }

  slot_value = geda_attrib_search_object_by_name (object, "slot", 0);

  if (slot_value == NULL) {
    /* we didn't find a slot=? attribute, make something up */
    slot_value = geda_utility_string_strdup ("1");
  }

  x_dialog_edit_slot (w_current, slot_count, slot_value);
  GEDA_FREE (slot_count);
  GEDA_FREE (slot_value);
}

/*! \brief Add a Slot Attribute
 *  \par Function Description
 *  The function creates a new type "slots" attribute to the drawing and
 *  and attaches it to the object. This is function is called from:
 *  \par
 *  <DL>
 *    <DT>o_attrib</DT>
 *    <DT>o_text</DT>
 *    <DT>x_autonumber</DT>
 *    <DT>x_dialog_edit_slot_response_ok</DT>
 *  </DL>
 *
 *  \param w_current pointer to GschemToplevel context
 *  \param object    pointer to Object to receive slot attribute.
 *  \param string    pointer to attribute string.
 */
void o_slot_end(GschemToplevel *w_current, GedaObject *object, const char *string)
{
  GedaObject *o_slot;
  char       *slot_value;
  char       *numslots_value;
  char       *value;

  int numslots;
  int new_slot_number;

  g_return_if_fail (object != NULL);

  /* Retrieve the attribute value or abort */
  if (!geda_attrib_string_get_name_value (string, NULL, &value)) {
     geda_log (_("Slot attribute malformed\n"));
    return;
  }

  numslots_value = geda_attrib_search_object_by_name (object, "numslots", 0);

  if (!numslots_value) {
     geda_log (_("numslots attribute missing\n"));
     geda_log (_("Slotting not allowed for this component\n"));
    GEDA_FREE (value);
    return;
  }

  numslots = atoi (numslots_value);
  GEDA_FREE (numslots_value);

  new_slot_number = atoi (value);

#if DEBUG
  printf ("%s: new_slot_number=%d, numslots=%d\n", __func__, new_slot_number, numslots);
#endif

  if (new_slot_number > numslots || new_slot_number <=0 ) {
     geda_log (_("New slot number [%d], is out of range\n"), new_slot_number);
    GEDA_FREE (value);
    return;
  }

  /* first see if slot attribute already exists outside complex */
  slot_value = geda_struct_slot_search_slot (object, &o_slot);
  GEDA_FREE (slot_value);

  if (o_slot != NULL && !geda_attrib_is_inherited (o_slot)) {
    geda_text_object_set_string (o_slot, string);
  }
  else {

    Page       *p_current;
    GedaObject *new_obj;

    /* Add the slot attribute since it does not exist */
    new_obj = geda_text_object_new (ATTRIBUTE_COLOR,
                                    object->complex->x, object->complex->y,
                                    LOWER_LEFT, 0, /* zero is angle */
                                    10, INVISIBLE, SHOW_NAME_VALUE, string);

    p_current = geda_toplevel_get_current_page(w_current->toplevel);

    geda_struct_page_append_object (p_current, new_obj);

    /* manually attach attribute */
    geda_attrib_object_attach (object, new_obj, FALSE);

    /* Call add-objects-hook */
    g_hook_run_object (w_current, ADD_OBJECT_HOOK, new_obj);
  }

  geda_struct_slot_update_object (object);

  o_undo_savestate_object(w_current, UNDO_ALL, object);
  GEDA_FREE (value);
}

/** @} endgroup Slot-Operations */
