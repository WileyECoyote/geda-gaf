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
 * TODO: WEH: Does not check for uniquness!
 */
void o_notify_change_add (Page *page, ChangeNotifyFunc pre_change_func,
                                      ChangeNotifyFunc change_func,
                                      void *user_data)
{
  change_notify *entry;
  entry = GEDA_MEM_ALLOC0( sizeof(change_notify));

  entry->pre_change_func = pre_change_func;
  entry->change_func     = change_func;
  entry->user_data       = user_data;

  geda_notify_list_add(page->change_notify_funcs, entry);
}

/*! \brief Remove change notification handlers from a Page.
 * \par Function Description
 * Removes a set of change notification handlers and their associated
 * \a user_data from \a toplevel.  If no registered set of handlers
 * matches the given \a pre_change_func, \a change_func and \a
 * user_data, does nothing.
 *
 * \see o_notify_change_add()
 *
 * \param page #Page structure to remove handlers from.
 * \param pre_change_func Function called just before changes.
 * \param change_func Function called just after changes.
 * \param user_data User data passed to callback functions.
 */
void
o_notify_change_remove (Page *page,
                        ChangeNotifyFunc pre_change_func,
                        ChangeNotifyFunc change_func,
                        void *user_data)
{
  GList *iter;

  iter = page->change_notify_funcs->glist;

  while (iter) {

    change_notify *entry = (change_notify *) iter->data;

    if ((entry != NULL)
      && (entry->pre_change_func == pre_change_func)
      && (entry->change_func == change_func)
      && (entry->user_data == user_data))
    {
      GEDA_FREE (entry);
      iter->data = NULL;
      break;
    }

    NEXT(iter);
  }
  page->change_notify_funcs->glist =
  g_list_remove_all (page->change_notify_funcs->glist, NULL);
}

void
o_notify_change_remove_all (Page *page)
{
  if (page->change_notify_funcs) {
    geda_notify_list_remove_all(page->change_notify_funcs);
  }
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
void o_notify_emit_pre_change (Object *object)
{
  g_return_if_fail(GEDA_IS_OBJECT(object));

  if (GEDA_IS_PAGE(object->page) && IS_ACTIVE_PAGE(object->page)) {

    if (object->page->change_notify_funcs != NULL &&
       !object->page->change_notify_funcs->freeze_count)
    {

      GList *iter;

      iter = geda_notify_list_get_glist(object->page->change_notify_funcs);

      while (iter != NULL)
      {
        change_notify *entry;
        entry = (change_notify *) iter->data;
        if ((entry != NULL) && (entry->pre_change_func != NULL)) {
          entry->pre_change_func (entry->user_data, object);
        }
        NEXT(iter);
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
o_notify_emit_change (Object *object)
{
  g_return_if_fail(GEDA_IS_OBJECT(object));

  if (GEDA_IS_PAGE(object->page) && IS_ACTIVE_PAGE(object->page)) {

    if (object->page->change_notify_funcs != NULL &&
       !object->page->change_notify_funcs->freeze_count)
    {

      GList *iter;

      iter = geda_notify_list_get_glist(object->page->change_notify_funcs);

      while (iter != NULL) {

        change_notify *entry = (change_notify *) iter->data;

        if ((entry != NULL) && (entry->change_func != NULL)) {

          entry->change_func (entry->user_data, object);

        }
        NEXT(iter);
      }
    }
  }
}

/*! \brief Suspense Notification for a GedaNotifyList
 *
 * \par Function Description
 *  This function increments the freeze count of an #GedaNotifyList.
 *  Notification of changes is suspended until the freeze is reduced
 *  to zero.
 *
 * \sa geda_notify_list_thaw
 *
 * \param list #GedaNotifyList to freeze notifications for.
 */
void geda_notify_list_freeze (GedaNotifyList *list)
{
  if (list != NULL) {
    list->freeze_count++;
  }
}

/*! \brief Thaw Notification for a GedaNotifyList
 *
 * \par Function Description
 *  This function add a hook to each new page
 *
 * \sa geda_notify_list_freeze
 *
 * \param list #GedaNotifyList to thaw notifications for.
 */
void geda_notify_list_thaw (GedaNotifyList *list)
{
  if (list != NULL) {

    list->freeze_count--;
    list->freeze_count = (list->freeze_count < 0) ? 0 : list->freeze_count;
  }
}