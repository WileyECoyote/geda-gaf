/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include "../../../config.h"
#include <libgeda_priv.h>

#include <geda_debug.h>

/*!
 * \brief Returns a pointer to a new SELECTION object
 * \par Function Description
 *  Returns a pointer to a new SELECTION object.
 *
 * \return pointer to the new SELECTION object.
 */
SELECTION *geda_object_selection_new( void )
{
  return (SELECTION*)geda_list_new();
}

/*!
 * \brief Selects the given object and adds it to the selection list
 * \par Function Description
 *  Selects the given object and does the needed work to make the
 *  object visually selected. Skip objects that are already selected.
 *
 * \param [in] selection  Pointer to the selection list
 * \param [in] o_selected GedaObject to select.
 */
void geda_object_selection_add (SELECTION *selection, GedaObject *o_selected)
{
  if (geda_object_selection_select (o_selected) == 1) {
    geda_list_add( (GedaList *)selection, o_selected );
  }
}

/*!
 * \brief Get the first object in selection list
 * \par Function Description
 *  Returns the first data referenced in the glist associated
 *  with the selection or NULL if the list is empty.
 *
 * \param [in] selection  Pointer to the selection list
 *
 * \returns The first GedaObject or NULL if there is no selection
 */
GedaObject *geda_object_selection_get_first(SELECTION *selection)
{
  if (!geda_list_get_glist(selection))
    return NULL;

  return (GedaObject*)g_list_first(geda_list_get_glist(selection))->data;
}

/*!
 * \brief Removes the given object from the selection list
 * \par Function Description
 *  Removes the given object from the selection list and does the
 *  needed work to make the object visually unselected. It's ok to
 *  call this function with an object which is not necessarily
 *  selected, or even with a NULL pointer for an object.
 *
 *  \param [in] selection  Pointer to the selection list
 *  \param [in] object     GedaObject to unselect and remove from the list.
 *
 *  \returns TRUE if \a object was selected and was removed, FALSE if the
 *           object was either not selected or was not in the selection
 *           or -1 to indicate an error if \a object is not a valid gEDA
 *           object.
 */
int geda_object_selection_remove (SELECTION *selection, GedaObject *object)
{
  int result;

  if (object != NULL) {

    if (g_list_find(geda_list_get_glist(selection), object) != NULL) {

      result = geda_object_selection_unselect (object);

      geda_list_remove ((GedaList*)selection, object);

    }
    else {

      result = GEDA_IS_OBJECT(object) ? 0 : -1;

    }
  }
  else {
    result = -1;
  }
  return result;
}

/*!
 * \brief Prints the given selection list
 * \par Function Description
 *  Prints the given selection list.
 *
 * \param [in] selection Pointer to selection list to print.
 */
void geda_object_selection_print_all(const SELECTION *selection)
{
  const GList *s_current = geda_list_get_glist (selection);

  printf("START printing selection ********************\n");

  while(s_current != NULL) {

    GedaObject *object = s_current->data;

    if (object) {
      printf("Selected object: %d, name=%s\n", object->sid, object->name);
    }
    s_current = g_list_next( s_current );
  }
  printf("DONE printing selection ********************\n");
  printf("\n");
}

/*!
 * \brief Selects the given object
 * \par Function Description
 *  If the given object is not currently selected the prechange
 *  notifier is called before setting the select flag on the
 *  object. After setting the flag the change notifier is called.
 *
 * \param [in] object  GedaObject to select.
 *
 * \returns TRUE if the was not selected, FALSE if the object
 *          was already selected, or -1 to indicate an error
 *          because \a object is not a valid gEDA object.
 */
int geda_object_selection_select(GedaObject *object)
{
  int result;

  if (GEDA_IS_OBJECT(object)) {

    result = !object->selected;

    if (result) { /* if was not selected */
      geda_object_notify_emit_pre_change (object);
      object->selected = TRUE;
      geda_object_notify_emit_change (object);
    }
  }
  else {
    result = -1;
  }

  return result;
}

/*!
 * \brief Unselects the given object
 * \par Function Description
 *  Unsets the select flag for the given object.
 *
 * \param [in] object  GedaObject to unselect.
 *
 * \returns TRUE if the was selected, FALSE if the object
 *          was not selected, or -1 to indicate an error
 *          because \a object is not a valid gEDA object.
 */
int geda_object_selection_unselect (GedaObject *object)
{
  int result;

  if (GEDA_IS_OBJECT(object)) {

    if ((result = object->selected)) { /* if was selected */
      geda_object_notify_emit_pre_change (object);
      object->selected = FALSE;
      geda_object_notify_emit_change (object);
    }
  }
  else {
    BUG_PMSG("Not a gEDA GedaObject: <%p>", object);
    result = -1;
  }
  return result;
}

/*!
 * \brief Unselects all objects in given selection
 * \par Function Description
 *  Unsets the selected flag for each object in \a selection.
 *
 * \param [in] selection #GedaList of objects to unselect.
 *
 * \returns count of the number of objects unselected, FALSE if no
 *          objects were unselected, or -1 to indicate an error
 *          because \a object is not a valid #GedaList.
 */
int geda_object_selection_unselect_all (SELECTION *selection)
{
  int result;

  if (GEDA_IS_LIST(selection)) {

    GList  *iter;

    iter = geda_list_get_glist(selection);

    result = 0;

    while (iter) {

      GedaObject *object = iter->data;

      if ((result += object->selected)) { /* if was selected */
        geda_object_notify_emit_pre_change (object);
        object->selected = FALSE;
        geda_object_notify_emit_change (object);
      }
      iter = iter->next;
    }
  }
  else {
    /*! \todo need to set error message somewhere instead of blurting */
    fprintf(stderr, "%s: Invalid selection <%p>\n", __func__, selection);
    result = -1;
  }
  return result;
}
