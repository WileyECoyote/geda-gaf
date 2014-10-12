/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include <geda_standard.h>

#include "libgeda_priv.h"

#include <geda_debug.h>

/*! \brief Returns a pointer to a new SELECTION object
 *
 *  \par Function Description
 *  Returns a pointer to a new SELECTION object.
 *
 *  \return pointer to the new SELECTION object.
 */
SELECTION *o_selection_new( void )
{
  return (SELECTION*)geda_list_new();
}

/*! \brief Selects the given object and adds it to the selection list
 *
 *  \par Function Description
 *  Selects the given object and does the needed work to make the
 *  object visually selected. Skip objects that are already selected.
 *
 *  \param [in] selection  Pointer to the selection list
 *  \param [in] o_selected Object to select.
 */
void o_selection_add (SELECTION *selection, Object *o_selected)
{
  if (o_selection_select (o_selected) == 1) {
    geda_list_add( (GedaList *)selection, o_selected );
  }
}

/*! \brief Removes the given object from the selection list
 *
 *  \par Function Description
 *  Removes the given object from the selection list and does the
 *  needed work to make the object visually unselected. It's ok to
 *  call this function with an object which is not necessarily
 *  selected, or even with a NULL pointer for an object.
 *
 *  \param [in] selection  Pointer to the selection list
 *  \param [in] object     Object to unselect and remove from the list.
 *
 *  \returns TRUE if \a object was selected and was removed, FALSE if the
 *           object was either not selected or was not in the selection
 *           or -1 to indicate an error if \a object is not a valid gEDA
 *           object.
 */
int o_selection_remove (SELECTION *selection, Object *object)
{
  int result;

  if (object != NULL) {

    if (g_list_find( geda_list_get_glist(selection), object ) != NULL) {

      result = o_selection_unselect (object);

      geda_list_remove( (GedaList *)selection, object );

    }
    else {

      if (GEDA_IS_OBJECT(object)) {
        result = 0;
      }
      else {
        result = -1;
      }
    }
  }
  else {
    result = -1;
  }
  return result;
}


/*! \brief Prints the given selection list
 *
 *  \par Function Description
 *  Prints the given selection list.
 *
 *  \param [in] selection Pointer to selection list to print.
 *
 */
void o_selection_print_all(const SELECTION *selection)
{
  const GList *s_current;
  Object      *object;

  s_current = geda_list_get_glist( selection );

  printf("START printing selection ********************\n");
  while(s_current != NULL) {
    object = s_current->data;
    if (object) {
      printf("Selected object: %d, name=%s\n", object->sid, object->name);
    }
    s_current = g_list_next( s_current );
  }
  printf("DONE printing selection ********************\n");
  printf("\n");
}

/*! \brief Selects the given object
 *
 *  \par Function Description
 *  If the given object is not current select the prechange notifier
 *  is called before setting the select flag on the object. After
 *  setting the flag the change notifier is called.
 *
 *  \param [in] object    Object to select.
 *
 *  \returns TRUE if the was not selected, FALSE if the object
 *           was already selected, or -1 to indicate an error
 *           because \a object is not a valid gEDA object.
 */
int o_selection_select(Object *object)
{
  int result;

  if (GEDA_IS_OBJECT(object)) {

    result = !object->selected;

    if (result) { /* if was not selected */
      o_notify_emit_pre_change (object);
      object->selected = TRUE;
      o_notify_emit_change (object);
    }
  }
  else {
     fprintf(stderr, "%s: Is not gEDA Object:<%p>\n", __func__, object);
     result = -1;
  }

  return result;
}

/*! \brief Unselects the given object
 *
 *  \par Function Description
 *  Unsets the select flag for the given object.
 *
 *  \param [in] object    Object to unselect.
 *
 *  \returns TRUE if the was selected, FALSE if the object
 *           was not selected, or -1 to indicate an error
 *           because \a object is not a valid gEDA object.
 */
int o_selection_unselect (Object *object)
{
  int result;

  if (GEDA_IS_OBJECT(object)) {

    if ((result = object->selected)) { /* if was selected */
      o_notify_emit_pre_change (object);
      object->selected = FALSE;
      o_notify_emit_change (object);
    }
  }
  else {
    fprintf(stderr, "%s: Is not gEDA Object:<%p>\n", __func__, object);
    result = -1;
  }
  return result;
}
