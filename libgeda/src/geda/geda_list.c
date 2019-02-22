/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2000 Ales Hvezda
 * Copyright (C) 2007-2015 Peter Clifton
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

/*! \file geda_list.c
 *  \brief list derived from GList with GObject properties
 */

/** \defgroup geda-list-object Geda List
 *  @{
 * \brief Implmentation of #GedaList Class
 * \par
 *  GedaList provides a GOBJECT wrapper for GLIST which provides
 *  the ability to use GObject signaling mechanisms with GedaList.
 *
 * \class GedaList geda_list.h "libgeda/geda_list.h"
 */

#include <config.h>

#include <ctype.h>
#include <glib-object.h>

#include <libgeda_priv.h>
#include <geda_list.h>

enum {
  CHANGED,
  LAST_SIGNAL
};

static unsigned int  geda_list_signals[ LAST_SIGNAL ] = { 0 };
static GObjectClass *geda_list_parent_class = NULL;


/*!
 * \brief GedaType instance initializer for GedaList
 * \par Function Description
 *  GedaType instance initializer for GedaList.
 *
 * \param [in]  instance  The GedaList we are initializing.
 * \param [in]  g_class   The class of the type the instance is created for.
 */
static void geda_list_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaList *list       = (GedaList *)instance;

  list->instance_type  = geda_list_get_type();

  /* Strictly un-necessary, as the memory is zero'd after allocation */
  list->glist = NULL;
}


/*!
 * \brief GObject finalise handler
 * \par Function Description
 *  Just before the GedaList GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 * \param [in] object  The GObject being finalized.
 *
 * \note Macro geda_list_free_full sets list->glist to NULL
 */
static void geda_list_finalize(GObject *object)
{
  GedaList *list = GEDA_LIST(object);

  if (list && list->glist) {
    g_list_free(list->glist);
  }

  G_OBJECT_CLASS(geda_list_parent_class)->finalize(object);
}


/*!
 * \brief GedaType class initializer for GedaList
 * \par Function Description
 *  GedaType class initializer for GedaList. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  g_class       The GedaList we are initializing
 * \param [in]  g_class_data  (unused)
 */
static void geda_list_class_init(void *g_class, void *g_class_data)
{
  GedaListClass *klass        = GEDA_LIST_CLASS(g_class);
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
  geda_list_parent_class      = g_type_class_peek_parent(klass);

  gobject_class->finalize     = geda_list_finalize;

  geda_list_signals[ CHANGED ] =
    g_signal_new ("changed",
                  GEDA_TYPE_LIST,
                  0     /*signal_flags */,
                  0     /*class_offset */,
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE,
                  0     /* n_params */
                );
}

/*!
 * \brief Function to retrieve GedaList's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaList Type identifier. When first called,
 *  the function registers a #GedaList in the GedaListType system to
 *  obtain an identifier that uniquely itentifies a GedaList and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 * \return GedaListType identifier associated with GedaList.
 */
GedaListType geda_list_get_type (void)
{
  static volatile GedaListType geda_list_type = 0;

  if (g_once_init_enter (&geda_list_type)) {

    static const GTypeInfo info = {
      sizeof(GedaListClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_list_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(GedaList),
      0,                      /* n_preallocs         */
      geda_list_instance_init /* (GInstanceInitFunc) */
    };

    const char  *string;
    GedaListType type;

    string = g_intern_static_string ("GedaList");
    type   = g_type_register_static (G_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_list_type, type);
  }

  return geda_list_type;
}

bool is_a_geda_list (const GedaList *list)
{
  if (G_IS_OBJECT(list)) {
    return (geda_list_get_type() == list->instance_type);
  }
  return FALSE;
}

/*!
 * \brief Returns a pointer to a new GedaList object.
 * \par Function Description
 *  Returns a pointer to a new GedaList object.
 *
 * \return pointer to the new GedaList object.
 */
GedaList *geda_list_new(void) {
  return g_object_new(GEDA_TYPE_LIST, NULL);
}


/*!
 * \brief Adds the given item to the GedaList
 * \par Function Description
 *  Adds the given item to the GedaList
 *
 * \param [in] list Pointer to the GedaList
 * \param [in] item item to add to the GedaList.
 */
void geda_list_add(GedaList *list, void *item)
{
  list->glist = g_list_append(list->glist, item);
  g_signal_emit(list, geda_list_signals[ CHANGED ], 0);
}


/*!
 * \brief Adds the given glist of items to the GedaList
 * \par Function Description
 *  Adds the given glist of items to the GedaList
 *  A copy is made, so the original GList is not modified.
 *
 * \param [in] list Pointer to the GedaList
 * \param [in] items GList of items to add to the GedaList.
 */
void geda_list_add_glist(GedaList *list, GList *items)
{
  GList *glist_copy = g_list_copy(items);

  list->glist = g_list_concat(list->glist, glist_copy);

  g_signal_emit(list, geda_list_signals[ CHANGED ], 0);
}

/*!
 * \brief Adds glist of items if not already a member of GedaList
 * \par Function Description
 *  Adds all items in \a items to the GedaList that are not
 *  not already a member of \a list.
 *
 * \param [in] list  Pointer to the GedaList
 * \param [in] items GList of items to add to the GedaList.
 */
void geda_list_add_glist_unique (GedaList *list, GList *items)
{
  GList *one_list = NULL;
  GList *iter;

  for (iter = items; iter; iter = iter->next) {
    if (!geda_list_is_in_list(list, iter->data)) {
      if (!g_list_find(one_list, iter->data)) {
        one_list = g_list_append(one_list, iter->data);
      }
    }
  }
  geda_list_add_glist(list, one_list);
  g_list_free(one_list);
}

/*!
 * \brief Add item to the GedaList if not already in list
 * \par Function Description
 *  Adds the given item to the GedaList if the item is not
 *  already in the list.
 *
 * \param [in] list Pointer to the GedaList
 * \param [in] item item to add to the GedaList.
 *
 * \return TRUE if \a item was added or FALSE if \a item was not added.
 */
bool geda_list_add_unique (GedaList *list, void *item)
{
  if (!geda_list_is_in_list(list, item)) {
    geda_list_add(list, item);
    return TRUE;
  }
  return FALSE;
}

/*!
 * \brief Add pointer to a string to the GedaList if not already in list
 * \par Function Description
 *  Adds the given item to the GedaList if the string is not found
 *  in the data associated with the list.
 *
 * \param [in] list Pointer to the GedaList
 * \param [in] text string to add to the GedaList
 *
 * \returns TRUE if the string pointer was added
 */
bool geda_list_add_unique_string (GedaList *list, char  *text)
{
  GList *iter;
  bool   found;

  found = FALSE;

  for (iter = g_list_first(list->glist); iter; iter = iter->next) {

    char *str = iter->data;

    if (str != NULL && isalpha (str[0])) {
      if (strcmp(text, str) == 0) {
        found = TRUE;
        break;  /* No need to continue */
      }
    }
  }

  if (!found) {
    list->glist = g_list_append(list->glist, text);
    g_signal_emit(list, geda_list_signals[ CHANGED ], 0);
  }
  return !found;
}

/*!
 * \brief Returns a copy of the glist associated with the given GedaList
 * \par Function Description
 *  A copy is made of the glist and returned
 *
 * \param [in] list Pointer to the GedaList
 *
 * \retval pointer to copy of a Glist  the GedaList.
 */
GList *geda_list_copy_glist(GedaList *list)
{
  return g_list_copy(list->glist);
}


/*!
 * \brief Find a given item in a GedaList
 * \par Function Description
 *  Searches a GedaList for the item and returns the
 *  item if found or NULL if the item was not in the list.
 *
 * \param [in] list Pointer to the GedaList
 * \param [in] item to find for in the GedaList.
 */
void *geda_list_find(GedaList *list, void *item)
{
  return g_list_find(list->glist, item);
}

/*!
 * \brief Are all GedaObjects in a GedaList the same type
 * \par Function Description
 *  Iterates \a list comparing the type of objects to the first
 *  object. Returns FALSE if the first element was not a valid
 *  GedaObject or any objects thereafter were not of the same type.
 *
 * \param [in] list Pointer to the GedaList
 *
 * \return TRUE if all elements are objects of the same type
 */
int geda_list_is_homogeneous_objects (GedaList *list)
{
  bool        answer;
  GedaObject *object;
  GList      *o_iter;

  if (GEDA_IS_LIST(list)) {
    GList *glist = geda_list_get_glist(list);
    if (GEDA_IS_OBJECT(glist->data)) {
      object = (GedaObject*)glist->data;
      o_iter = glist;
    }
    else {
      object = NULL;
    }
  }
  else {
    BUG_PMSG("<%p> is not a valid GedaList", list);
    object = NULL;
  }

  if (object) {

    int otype;

    answer = TRUE;
    otype  = object->type;
    o_iter = g_list_next (o_iter);

    while (o_iter != NULL) {

      if (GEDA_IS_OBJECT(o_iter->data)) {
        object = (GedaObject*)o_iter->data;
      }
      else {
        answer = FALSE;
        break;
      }

      if (object->type != otype) {
        answer = FALSE;
        break;
      }
      o_iter = g_list_next (o_iter);
    }
  }
  else {
    answer = FALSE;
  }

  return answer;
}

/*!
 * \brief Get is a given item in a GedaList
 * \par Function Description
 *  Searches a GedaList for the item and returns TRUE
 *  if found or FALSE of the item was not in the list.
 *
 * \param [in] list Pointer to the GedaList
 * \param [in] item to find for in the GedaList.
 */
bool geda_list_is_in_list(GedaList *list, void *item)
{
  return g_list_find(list->glist, item) ? 1 : 0;
}

/*!
 * \brief Prepends the given item to the GedaList
 * \par Function Description
 *  Prepends the given item to the GedaList
 *
 * \param [in] list Pointer to the GedaList
 * \param [in] item item to prepend to the GedaList.
 */
void geda_list_prepend(GedaList *list, void *item)
{
  list->glist = g_list_prepend(list->glist, item);
  g_signal_emit(list, geda_list_signals[ CHANGED ], 0);
}

/*!
 * \brief Removes the given item from the GedaList
 * \par Function Description
 *  Removes the given item from the GedaList.
 *  It's ok to call this function with an item which
 *  is not necessarily in the list.
 *
 * \param [in] list Pointer to the GedaList
 * \param [in] item to remove from the GedaList.
 *
 * \note Only removes the first instance of item.
 */
void geda_list_remove(GedaList *list, void *item)
{
  if (g_list_find(list->glist, item)) {

    list->glist = g_list_remove(list->glist, item);
    g_signal_emit(list, geda_list_signals[ CHANGED ], 0);
  }
}

/*!
 * \brief Removes all the items in the given GedaList.
 * \par Function Description
 *  Removes all items in the given GedaList.
 *
 * \param [in] list Pointer to the GedaList
 */
void geda_list_remove_all(GedaList *list)
{
  g_list_free(list->glist);
  list->glist = NULL;
  g_signal_emit(list, geda_list_signals[ CHANGED ], 0);
}

/*!
 * \brief Reduce reference count of given GedaList by one.
 * \par Function Description
 *   Calls g_object_unref GedaList \a list.
 *
 * \param [in] list Pointer to the GedaList
 */
void geda_list_unref (GedaList *list)
{
  g_object_unref(list);
}
/** @} endgroup geda-list-object */
