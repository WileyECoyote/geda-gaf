/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#include <gschem.h>
#include <geda_debug.h>

/*! \brief Delete an object.
 *  \par Function Description
 *  This function erases the object \a object before deleting it. It
 *  deals with connection and object connected to it.
 *
 *  \param [in] w_current The GschemToplevel object.
 *  \param [in] object    The object to delete.
 */
void o_delete (GschemToplevel *w_current, Object *object)
{
  GedaToplevel *toplevel = w_current->toplevel;

  g_return_if_fail (object != NULL);

  o_selection_remove   (toplevel->page_current->selection_list, object);
  s_page_remove_object (toplevel->page_current, object);
  g_run_hook_object    (w_current, "%remove-objects-hook", object);
  s_object_release      (object);

  toplevel->page_current->CHANGED = 1;
}

/*! \brief Delete objects from the selection.
 *  \par Function Description
 *  This function deletes the objects selected on the current page of
 *  toplevel \a w_current.
 *
 *  \param [in] w_current The GschemToplevel object.
 */
void o_delete_selected (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  SELECTION *selection = Top_Selection;
  GList *to_remove;
  GList *iter;
  Object *obj;

  unsigned int locked_num = 0;

  if (o_select_is_selection (w_current)) {

    to_remove = g_list_copy (geda_list_get_glist (selection));

    for (iter = to_remove; iter != NULL; iter = g_list_next (iter)) {
      obj = (Object *) iter->data;
      if (obj->selectable == FALSE)
        locked_num++;
    }

    if (locked_num > 0) {
      GList *non_locked = NULL;
      char *msg;
      int resp;

      msg = g_strdup_printf(ngettext ("Delete locked object?", "Delete %u locked objects?", locked_num), locked_num);
      resp =  gschem_confirm_dialog(msg, GTK_MESSAGE_QUESTION, TRUE);
      switch (resp) {
        case GTK_RESPONSE_YES: /* Remove all */
          break;
        case GTK_RESPONSE_NO: /* Remove non locked */
          for (iter = to_remove; iter != NULL; iter = g_list_next (iter)) {
            obj = (Object *) iter->data;
            if (obj->selectable == TRUE)
              non_locked = g_list_append (non_locked, iter->data);
          }
          g_list_free (to_remove);
          to_remove = non_locked;
          break;
        default: /* Cancel */
          g_list_free (to_remove);
          return;
      }
    }

    for (iter = to_remove; iter != NULL; iter = g_list_next (iter)) {
      obj = (Object *) iter->data;
      o_selection_remove   (selection, obj);
      s_page_remove_object (toplevel->page_current, obj);
    }

    g_run_hook_object_list (w_current, "%remove-objects-hook", to_remove);

    for (iter = to_remove; iter != NULL; iter = g_list_next (iter)) {
      obj = (Object *) iter->data;
      s_object_release (obj);
    }

    g_list_free (to_remove);

    //w_current->inside_action = 0;
    o_undo_savestate (w_current, UNDO_ALL);
    //i_update_sensitivities (w_current);
  }
}
