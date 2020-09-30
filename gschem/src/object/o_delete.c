/* -*- C o_delete.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_delete.c
 * \brief Low-level module for deleting objects
 */

#include <gschem.h>
#include <geda_debug.h>

/*!
 * \brief Delete an object.
 * \par Function Description
 *  This function removes \a object before deleting it. Connectivity
 *  and objects connected to the object are resolved by libgeda when
 *  the object is removed from the page.
 *
 * \param [in] w_current The GschemToplevel object.
 * \param [in] object    The object to delete.
 *
 * \note This function is primarily used to delete temporary objects
 *       and special purpose operations, such when "breaking" objects,
 *       the caller should call o_undo_savestate if required.
 */
void o_delete (GschemToplevel *w_current, GedaObject *object)
{
  GedaToplevel *toplevel = w_current->toplevel;

  g_return_if_fail (object != NULL);

  geda_object_selection_remove   (toplevel->page_current->selection_list, object);
  geda_struct_page_remove_object (toplevel->page_current, object);
  g_hook_run_object              (w_current, REMOVE_OBJECTS_HOOK, object);
  geda_struct_object_release     (object);
}

/*!
 * \brief Delete objects from the selection.
 * \par Function Description
 *  This function deletes the objects selected on the current page of
 *  toplevel \a w_current.
 *
 * \param [in] w_current The GschemToplevel object.
 */
void o_delete_selected (GschemToplevel *w_current)
{
  if (o_select_is_selection (w_current)) {

    GedaToplevel *toplevel   = gschem_toplevel_get_geda_toplevel(w_current);
    SELECTION    *selection  = toplevel->page_current->selection_list;
    GList        *to_remove  = g_list_copy (geda_list_get_glist (selection));
    GList        *iter       = to_remove;
    unsigned int  locked_num = 0;

    while (iter) {
      if (geda_object_get_selectable(iter->data) == FALSE) {
        locked_num++;
      }
      iter = iter->next;
    }

    if (locked_num > 0) {

      GList *non_locked = NULL;
      char  *msg;
      int    resp;

      msg  = geda_sprintf(ngettext("Delete locked object?", "Delete %u locked objects?", locked_num), locked_num);
      resp = x_dialog_confirmation(msg, GTK_MESSAGE_QUESTION, TRUE);

      switch (resp) {
        case GEDA_RESPONSE_YES: /* Remove all */
          break;

        case GEDA_RESPONSE_NO:  /* Remove non locked */
          for (iter = to_remove; iter != NULL; iter = iter->next) {
            GedaObject *object = iter->data;
            if (geda_object_get_selectable(object) == TRUE)
              non_locked = g_list_append (non_locked, object);
          }
          g_list_free (to_remove);
          to_remove = non_locked;
          break;

        default: /* Cancel */
          g_list_free (to_remove);
          return;
      }
      geda_free(msg);
    }

    /* Remove objects from selection and page */
    for (iter = to_remove; iter != NULL; iter = iter->next) {
      GedaObject *object = iter->data;
      geda_object_selection_remove (selection, object);
      geda_struct_page_remove_object (toplevel->page_current, object);
    }

    g_hook_run_object_list (w_current, REMOVE_OBJECTS_HOOK, to_remove);

    /* Release the objects, normally will destroy them */
    for (iter = to_remove; iter != NULL; iter = iter->next) {
      geda_struct_object_release (iter->data);
    }

    g_list_free (to_remove);

    o_undo_savestate (w_current, UNDO_ALL);
  }
}
