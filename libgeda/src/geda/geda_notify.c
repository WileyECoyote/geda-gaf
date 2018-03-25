/* -*- geda_notify_list.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2014-2015 gEDA Contributors (see ChangeLog for details)
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
 *
 * The GedaNotifyList object was inspired by Peter Clifton's GList
 * wrapper, GedaList, and his code was used as the basis for the
 * GedaNotifyList module.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: September, 2nd, 2014
 */

/*! \file geda_notify.c
 *  \brief list derived from GList with GObject properties
 */
/** \defgroup geda-notify-list-object Geda Notify List
 *  @{
 * \brief Implmentation of #GedaNotifyList Class
 * \par
 *  GedaNotifyList provides a GOBJECT wrapper for GLIST similar to
 *  a GedaList but without the change signal. The list is intended
 *  hold change_notify_entry structures that are freed when the
 *  GedaNotifyList is destroyed, more importantly, it is not
 *  destroyed until all references are removed, this property
 *  allows the list to survive an Undo operation of the Page to
 *  which the GedaNotifyList is assigned.
 *
 * \class GedaNotifyList geda_notify.h "libgeda/geda_notify.h"
 */

#include "../../../config.h"

#include <glib-object.h>
#include <libgeda_priv.h>

static GObjectClass *geda_notify_list_parent_class = NULL;

/* List of pointers to GedaNotifyList instances */
static GList *list_of_objects = NULL;

/*!
 * \brief GedaType instance initializer for GedaNotifyList
 * \par Function Description
 *  GedaType instance initializer for GedaNotifyList.
 *
 *  \param [in]  instance  The GedaNotifyList we are initialising.
 *  \param [in]  class     The class of the type the instance is created for.
 */
static void geda_notify_list_instance_init (GTypeInstance *instance, void *class)
{
  GedaNotifyList *list = (GedaNotifyList*)instance;

  list->instance_type  = geda_notify_list_get_type();

  /* Strictly un-necessary, as the memory is zero'd after allocation */
  list->glist          = NULL;
  list->freeze_count   = 0;

  list_of_objects = g_list_append(list_of_objects, list);
}

/*!
 * \brief GObject finalise handler
 * \par Function Description
 *  Called before the GedaNotifyList GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 * \param [in] object  The GObject being finalized.
 */
static void geda_notify_list_finalize (GObject *object)
{
  GedaNotifyList *list = GEDA_NOTIFY_LIST( object );

  list_of_objects = g_list_remove(list_of_objects, object);

  if (!g_list_length(list_of_objects)) {
    g_list_free(list_of_objects);
    list_of_objects = NULL;
  }

  geda_utility_glist_free_full (list->glist, g_free);

  list->glist = NULL;

  G_OBJECT_CLASS( geda_notify_list_parent_class )->finalize (object);
}

/*!
 * \brief GedaType class initializer for GedaNotifyList
 * \par Function Description
 *  GedaType class initializer for GedaNotifyList. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  class       The GedaNotifyList we are initialising
 * \param [in]  class_data  (unused)
 */
static void geda_notify_list_class_init(void *class, void *class_data)
{
  GedaNotifyListClass *klass    = GEDA_NOTIFY_LIST_CLASS (class);
  GObjectClass *gobject_class   = G_OBJECT_CLASS (klass);

  geda_notify_list_parent_class = g_type_class_peek_parent (klass);

  gobject_class->finalize       = geda_notify_list_finalize;

}

/*!
 * \brief Function to retrieve GedaNotifyList's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaNotifyList Type identifier. When first called,
 *  the function registers a #GedaNotifyList in the GedaNotifyType system to
 *  obtain an identifier that uniquely itentifies a GedaNotifyList and returns
 *  the unsigned integer value. The retained value is returned on subsequent
 *  calls.
 *
 * \return GedaNotifyType identifier associated with GedaNotifyList.
 */
GedaNotifyType geda_notify_list_get_type (void)
{
  static volatile GedaNotifyType geda_notify_list_type = 0;

  if (g_once_init_enter (&geda_notify_list_type)) {

    static const GTypeInfo info = {
      sizeof(GedaNotifyListClass),
      NULL,                          /* base_init           */
      NULL,                          /* base_finalize       */
      geda_notify_list_class_init,   /* (GClassInitFunc)    */
      NULL,                          /* class_finalize      */
      NULL,                          /* class_data          */
      sizeof(GedaNotifyList),
      0,                             /* n_preallocs         */
      geda_notify_list_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaNotifyType type;

    string = g_intern_static_string ("GedaNotifyList");
    type   = g_type_register_static (G_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_notify_list_type, type);
  }

  return geda_notify_list_type;
}

bool is_a_geda_notify_list (const GedaNotifyList *list)
{
  if (list) {
    return g_list_find(list_of_objects, list) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Returns a pointer to a new GedaNotifyList object.
 * \par Function Description
 *  Returns a pointer to a new GedaNotifyList object.
 *
 * \return pointer to the new GedaNotifyList object.
 */
GedaNotifyList *geda_notify_list_new (void) {

  return g_object_new( GEDA_TYPE_NOTIFY_LIST, NULL );
}

/*!
 * \brief Returns is a GedaNotifyList Frozen.
 * \par Function Description
 *  If \a list is a valid #GedaNotifyList and the freeze count is 0
 *  then the functions returns zero, otherwise the function returns
 *  -1 if \a list was not valid #GedaNotifyList, or a positive interger
 *  equal to the current freeze count.
 *
 * \return TRUE if the freeze count is non zero
 */
int geda_notify_list_is_frozen (GedaNotifyList *list)
{
  bool answer;

  if (GEDA_IS_NOTIFY_LIST(list)) {
    answer = list->freeze_count;
  }
  else {
    answer = -1;
  }
  return answer;
}

/*!
 * \brief Suspense Notification for a GedaNotifyList
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

/*!
 * \brief Thaw Notification for a GedaNotifyList
 * \par Function Description
 *  This is a low level function that allows decrementing the
 *  freeze notification count but does not perform notifications
 *  even if the count went to zero as a result of the call.
 *
 * \sa geda_notify_list_freeze geda_object_notify_emit_change
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

/*!
 * \brief Adds the given item to the GedaNotifyList
 * \par Function Description
 *  Adds the given item to the GedaNotifyList
 *
 * \param [in] list Pointer to the GedaNotifyList
 * \param [in] item item to add to the GedaNotifyList.
 */
void geda_notify_list_add (GedaNotifyList *list, void *item)
{
  list->glist = g_list_append (list->glist, item);
}

/*!
 * \brief Adds the given glist of items to the GedaNotifyList
 * \par Function Description
 *  Adds the given glist of items to the GedaNotifyList
 *  A copy is made, so the original GList is not modified.
 *
 * \param [in] list Pointer to the GedaNotifyList
 * \param [in] items GList of items to add to the GedaNotifyList.
 */
void geda_notify_list_add_glist (GedaNotifyList *list, GList *items)
{
  GList *glist_copy = g_list_copy (items);
  list->glist = g_list_concat (list->glist, glist_copy);
}

/*!
 * \brief Returns a copy of the glist associated with GedaNotifyList
 * \par Function Description
 *  A copy is made of the glist and returned
 *
 * \param [in] list Pointer to the GedaNotifyList
 *
 * \retval pointer to copy of a Glist  the GedaNotifyList.
 */
GList *geda_notify_list_copy_glist (GedaNotifyList *list)
{
  return g_list_copy (list->glist);
}

/*!
 * \brief Get if Function is in a GedaNotifyList
 * \par Function Description
 *  Search \a list and return TRUE if \a func is found in the list
 *  otherwise returns FALSE.
 *
 * \param [in] list Pointer to the GedaNotifyList
 * \param [in] func Address to look for
 *
 * \retval TRUE if found, otherwise FALSE.
 */
int geda_notify_list_in_list (GedaNotifyList *list, void *func)
{
  bool answer;

  answer = FALSE;

  if (GEDA_IS_NOTIFY_LIST(list)) {

    int count;
    int index;

    count = g_list_length(list->glist);

    for (index = 0; index < count; index++) {

      change_notify *entry;

      entry = (change_notify*)g_list_nth_data(list->glist, index);

      if (entry->pre_change_func == func) {
        answer = TRUE;
        break;
      }
    }
  }
  return answer;
}

/*!
 * \brief Removes the given item from the GedaNotifyList
 * \par Function Description
 *  Removes the given item from the GedaNotifyList.
 *  It's ok to call this function with an item which
 *  is not necessarily in the list.
 *
 * \param [in] list Pointer to the GedaNotifyList
 * \param [in] item to remove from the GedaNotifyList.
 */
void geda_notify_list_remove (GedaNotifyList *list, void *item)
{
  if (g_list_find (list->glist, item) == NULL)
    return;

  list->glist = g_list_remove (list->glist, item);
}

/*!
 * \brief Removes all the items in the given GedaNotifyList.
 * \par Function Description
 *  Removes all items in the given GedaNotifyList.
 *
 * \param [in] list Pointer to the GedaNotifyList
 */
void geda_notify_list_remove_all (GedaNotifyList *list)
{
  geda_utility_glist_free_full (list->glist, g_free);

  list->glist = NULL;
}

/** @} endgroup geda-notify-list-object */
