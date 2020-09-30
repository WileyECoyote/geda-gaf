/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
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
 *  This file contains the code used to manipulate <b>GedaObjects</b>.
 *  The object is the basic type of all elements stored in schematic
 *  and symbol files.
 *
 */

#include "../../../config.h"
#include <stdio.h>

#include <libgeda_priv.h>

/** \defgroup geda-notify-object-proc GedaNotifyList Object Procedures
 * @{
 * \brief Procedures to support operations with #GedaNotifyList Objects
 */

/*!
 * \brief Lookup a change notification handlers for a Geda Page Object
 * \par Function Description
 *  Searches the list of notification handlers for a change_notify entry
 *  containing data matching \a pre_change_func, \a change_func and the
 *  given \a user_data and returns the record if found.
 *
 * \param page            #Page structure to add handlers to.
 * \param pre_change_func Function to be called just before changes.
 * \param change_func     Function to be called just after changes.
 * \param user_data       User data to be passed to callback functions.
 *
 * \returns
 */
static change_notify*
o_notify_lookup (Page *page, ChangeNotifyFunc  pre_change_func,
                             ChangeNotifyFunc  change_func,
                             void             *user_data)
{
  GList         *iter;
  change_notify *entry;

  entry = NULL;
  iter  = page->change_notify_funcs->glist;

  while (iter) {

    entry = (change_notify *) iter->data;

    if ((entry != NULL)
      && (entry->pre_change_func == pre_change_func)
      && (entry->change_func == change_func)
      && (entry->user_data == user_data))
    {
      break;
    }

    NEXT(iter);
  }

  return entry;
}

/*!
 * \brief Add change notification handlers to to a Geda Page Object
 * \par Function Description
 *  Adds a set of change notification handlers to a #Page instance if
 *  a matching record is not found. \a pre_change_func will be called
 *  just before an object is modified, \a change_func will be called
 *  just after an object is modified, with the affected object and the
 *  given \a user_data.
 *
 * \param page            #Page structure to add handlers to.
 * \param pre_change_func Function to be called just before changes.
 * \param change_func     Function to be called just after changes.
 * \param user_data       User data to be passed to callback functions.
 */
void
geda_object_notify_change_add (Page             *page,
                               ChangeNotifyFunc  pre_change_func,
                               ChangeNotifyFunc  change_func,
                               void             *user_data)
{
  if (!o_notify_lookup(page, pre_change_func, change_func, user_data)) {

    change_notify *entry;
    entry = GEDA_MEM_ALLOC0( sizeof(change_notify));

    entry->pre_change_func = pre_change_func;
    entry->change_func     = change_func;
    entry->user_data       = user_data;

    geda_notify_list_add(page->change_notify_funcs, entry);
  }
}

/*!
 * \brief Remove change notification handlers from a Page
 * \par Function Description
 *  Removes a set of change notification handlers and their associated
 *  \a user_data from \a toplevel.  If no registered set of handlers
 *  matches the given \a pre_change_func, \a change_func and \a
 *  user_data, does nothing.
 *
 * \see geda_object_notify_change_add()
 *
 * \param page #Page structure to remove handlers from.
 * \param pre_change_func Function called just before changes.
 * \param change_func Function called just after changes.
 * \param user_data User data passed to callback functions.
 */
void
geda_object_notify_change_remove (Page             *page,
                                  ChangeNotifyFunc  pre_change_func,
                                  ChangeNotifyFunc  change_func,
                                  void             *user_data)
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

/*!
 * \brief Remove all change notification handlers from a Page
 * \par Function Description
 *  Wrapper for geda_object_notify_change_remove, and
 *  geda_object_notify_change_remove for each member of
 *  the change notify list in \a Page
 *
 * \see geda_object_notify_change_add
 *
 * \param page #Page structure to remove handlers from.
 */
void geda_object_notify_change_remove_all (Page *page)
{
  if (page->change_notify_funcs) {
    geda_notify_list_remove_all(page->change_notify_funcs);
  }
}

/*!
 * \brief Emit an object pre-change notification
 * \par Function Description
 *  Calls each pre-change callback function registered with a #Page
 *  to notify listeners that \a object is about to be modified.  All
 *  libgeda functions that modify #GedaObject structures should call
 *  this just before making a change to an #GedaObject.
 *
 * \param object   #GedaObject structure to emit notifications for.
 */
void geda_object_notify_emit_pre_change (GedaObject *object)
{
  g_return_if_fail(GEDA_IS_OBJECT(object));

  if (GEDA_IS_PAGE(object->page) && IS_ACTIVE_PAGE(object->page)) {

    if (!geda_notify_list_is_frozen(object->page->change_notify_funcs)) {

      GList *iter;

      iter = geda_notify_list_get_glist(object->page->change_notify_funcs);

      while (iter != NULL) {

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

/*!
 * \brief Emit an object change notification
 * \par Function Description
 *  Calls each change callback function registered with #Page to
 *  notify listeners that \a object has just been modified. All
 *  libgeda functions that modify #GedaObject structures should
 *  call this just after making a change to an #GedaObject.
 *
 * \param object #GedaObject structure to emit notifications for.
 */
void geda_object_notify_emit_change (GedaObject *object)
{
  g_return_if_fail(GEDA_IS_OBJECT(object));

  if (GEDA_IS_PAGE(object->page) && IS_ACTIVE_PAGE(object->page)) {

    if (!geda_notify_list_is_frozen(object->page->change_notify_funcs)) {

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

/** @} endgroup geda-notify-object-proc */
