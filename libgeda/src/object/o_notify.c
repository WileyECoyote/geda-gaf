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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

/*! \file o_notify.c
 *  \brief functions for the basic object type
 *
 *  This file contains the code used to manipulate <b>Objects</b>.
 *  The object is the basic type of all elements stored in schematic
 *  and symbol files.
 *
 */

#include <config.h>
#include <stdio.h>

#include "libgeda_priv.h"

/* Structure for each entry in a GedaToplevel's list of registered change
 * notification handlers, why is the defined here? */
struct change_notify_entry {
  ChangeNotifyFunc pre_change_func;
  ChangeNotifyFunc change_func;
  void *user_data;
};

/*! \brief Add change notification handlers to a GedaToplevel.
 * \par Function Description
 * Adds a set of change notification handlers to a #GedaToplevel instance.
 * \a pre_change_func will be called just before an object is
 * modified, and \a change_func will be called just after an object is
 * modified, with the affected object and the given \a user_data.
 *
 * \param page            #Page structure to add handlers to.
 * \param pre_change_func Function to be called just before changes.
 * \param change_func     Function to be called just after changes.
 * \param user_data       User data to be passed to callback functions.
 *
 * TODO: Does not check for uniquness!
 */
void o_add_change_notify (Page *page,
                          ChangeNotifyFunc pre_change_func,
                          ChangeNotifyFunc change_func,
                          void *user_data)
{
  struct change_notify_entry *entry = g_new0 (struct change_notify_entry, 1);
  entry->pre_change_func = pre_change_func;
  entry->change_func = change_func;
  entry->user_data = user_data;
  page->change_notify_funcs = g_list_prepend (page->change_notify_funcs, entry);
}

/*! \brief Remove change notification handlers from a Page.
 * \par Function Description
 * Removes a set of change notification handlers and their associated
 * \a user_data from \a toplevel.  If no registered set of handlers
 * matches the given \a pre_change_func, \a change_func and \a
 * user_data, does nothing.
 *
 * \see o_add_change_notify()
 *
 * \param page #Page structure to remove handlers from.
 * \param pre_change_func Function called just before changes.
 * \param change_func Function called just after changes.
 * \param user_data User data passed to callback functions.
 */
void
o_remove_change_notify (Page *page,
                        ChangeNotifyFunc pre_change_func,
                        ChangeNotifyFunc change_func,
                        void *user_data)
{
  GList *iter;
  for (iter = page->change_notify_funcs;
       iter != NULL; iter = g_list_next (iter)) {

    struct change_notify_entry *entry =
      (struct change_notify_entry *) iter->data;

    if ((entry != NULL)
        && (entry->pre_change_func == pre_change_func)
        && (entry->change_func == change_func)
        && (entry->user_data == user_data)) {
      GEDA_FREE (entry);
      iter->data = NULL;
    }
  }
  page->change_notify_funcs = g_list_remove_all (page->change_notify_funcs, NULL);
}

void
o_change_notify_remove_all (Page *page)
{
  GList *iter;
  for (iter = page->change_notify_funcs; iter != NULL; iter = g_list_next (iter)) {

    struct change_notify_entry *entry = (struct change_notify_entry *) iter->data;

    if (entry != NULL) {
      GEDA_FREE (entry);
      iter->data = NULL;
    }
  }
  page->change_notify_funcs = g_list_remove_all (page->change_notify_funcs, NULL);
}

/*! \brief Emit an object pre-change notification.
 * \par Function Description
 * Calls each pre-change callback function registered with #GedaToplevel
 * to notify listeners that \a object is about to be modified.  All
 * libgeda functions that modify #Object structures should call this
 * just before making a change to an #Object.
 *
 * \param object   #Object structure to emit notifications for.
 */
void
o_emit_pre_change_notify (Object *object)
{
  g_return_if_fail(GEDA_IS_OBJECT(object));

  if (GEDA_IS_PAGE(object->page) && IS_ACTIVE_PAGE(object->page)) {
    GList *iter;
    for (iter = object->page->change_notify_funcs; iter != NULL; NEXT(iter))
    {
      struct change_notify_entry *entry;
      entry = (struct change_notify_entry *) iter->data;
      if ((entry != NULL) && (entry->pre_change_func != NULL)) {
        entry->pre_change_func (entry->user_data, object);
      }
    }
  }
}

/*! \brief Emit an object change notification.
 * \par Function Description
 * Calls each change callback function registered with #Page to
 * notify listeners that \a object has just been modified.  All
 * libgeda functions that modify #Object structures should call this
 * just after making a change to an #Object.
 *
 * \param object   #Object structure to emit notifications for.
 *
 */
void
o_emit_change_notify (Object *object)
{
  g_return_if_fail(GEDA_IS_OBJECT(object));

  if (GEDA_IS_PAGE(object->page) && IS_ACTIVE_PAGE(object->page)) {

    GList *iter;

    for (iter = object->page->change_notify_funcs; iter != NULL; NEXT(iter)) {

      struct change_notify_entry *entry =
      (struct change_notify_entry *) iter->data;

      if ((entry != NULL) && (entry->change_func != NULL)) {
        entry->change_func (entry->user_data, object);
      }
    }
  }
}