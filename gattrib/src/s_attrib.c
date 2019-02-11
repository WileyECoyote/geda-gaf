/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2015 Stuart D. Brorson.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*!
 * \file
 * \brief Functions to operate on attributes in STRING_LISTs
 *
 * Various functions to operate on attribute name=value pairs in
 * STRING_LIST structs arguments.
 */

#include <gattrib.h>
#include <geda_debug.h>

/*------------------------------------------------------------------*/

/*!
 * \brief Detect "name" in STRING_LIST
 * \par Function Description
 *  This function is passed a STRING_LIST of name=value pairs, and a
 *  name.
 *
 * \param name_value_list pointer to STRING_LIST to search
 * \param name            string to search for
 *
 * \returns TRUE if the name is in the STRING_LIST, otherwise returns FALSE.
 */
int s_attrib_name_in_list(STRING_LIST *name_value_list, char *name)
{
  STRING_LIST *local_list_item;

  for (local_list_item = name_value_list;
       local_list_item != NULL;
       local_list_item = local_list_item->next)
  {
    char *local_name;

    if (local_list_item->data == NULL)
      continue;

    local_name = geda_utility_string_split(local_list_item->data, '=', 0);

    if (strcmp(local_name, name) == 0) {
      GEDA_FREE (local_name);
      return TRUE;
    }
    GEDA_FREE (local_name);
  }

  return FALSE;
}

/*------------------------------------------------------------------*/

/*!
 * \brief Locate the refdes associated with an object.
 * \par Function Description
 *  This function returns the string of refdes attribute attached to
 *  \a object for normal objects. For slotted objects the refdes string
 *  is returned in the form "refdes.slot", or NULL if no refdes attribute
 *  exist.
 *
 * \param object Pointer to the object to search for.
 *
 * \return pointer to a string or NULL if no refdes is found.
 */
char *s_attrib_get_refdes(GedaObject *object)
{
  char *temp_uref;

  /*------ Try to get the refdes -----*/
  temp_uref = geda_attrib_search_object_by_name (object, "refdes", 0);

  if (temp_uref) {

    char *numslots;

    /*------- Now append .slot to refdes if part is slotted -------- */
    /* Check if this is a multislotted component */
    numslots = geda_attrib_search_object_by_name (object, "numslots", 0);

    if (numslots != NULL) {

      GedaObject *slot_text_object;
      char       *slot_value;

      slot_value = geda_struct_slot_search_slot (object, &slot_text_object);

      if (slot_value != NULL) {

        char *suffixed;

        suffixed = geda_strconcat(temp_uref, ".", slot_value, NULL);

        GEDA_FREE (slot_value);
        GEDA_FREE (temp_uref);

        temp_uref = suffixed;
      }
      GEDA_FREE (numslots);
    }
  }

  return temp_uref;
}
