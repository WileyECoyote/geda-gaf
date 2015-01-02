/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2000 Ales Hvezda
 * Copyright (C) 2007-2014 Peter Clifton
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

/*! \file geda_list.c
 *  \brief list derived from GList with GObject properties
 *
 *  GedaList provides a GOBJECT wrapper for GLIST which provides
 *  the ability to use GObject signaling mechanisms with GedaList.
 */
/** \defgroup geda-list-object Geda List
 *  @{
 */

#include <config.h>

#include <ctype.h>
#include <glib-object.h>

#include "libgeda_priv.h"
#include "geda_list.h"

enum {
  CHANGED,
  LAST_SIGNAL
};

static unsigned int geda_list_signals[ LAST_SIGNAL ] = { 0 };
static GObjectClass *geda_list_parent_class = NULL;


/*! \brief GedaType instance initialiser for GedaList
 *
 *  GedaType Function Description
 *  GedaType instance initialiser for GedaList.
 *
 *  \param [in]  instance       The GedaList we are initialising.
 *  \param [in]  g_class        The class of the type the instance is created for.
 */
static void geda_list_instance_init( GTypeInstance *instance, void *g_class )
{
  GedaList *list = (GedaList *)instance;

  /* Strictly un-necessary, as the memory is zero'd after allocation */
  list->glist = NULL;
}


/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the GedaList GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object  The GObject being finalized.
 */
static void geda_list_finalize( GObject *object )
{
  GedaList *list = GEDA_LIST( object );
  g_list_free( list->glist );

  G_OBJECT_CLASS( geda_list_parent_class )->finalize( object );
}


/*! \brief GedaType class initialiser for GedaList
 *
 *  \par Function Description
 *  GedaType class initialiser for GedaList. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class       The GedaList we are initialising
 *  \param [in]  g_class_data  (unused)
 */
static void geda_list_class_init(void *g_class, void *g_class_data )
{
  GedaListClass *klass        = GEDA_LIST_CLASS( g_class );
  GObjectClass *gobject_class = G_OBJECT_CLASS( klass );
  geda_list_parent_class      = g_type_class_peek_parent( klass );

  gobject_class->finalize     = geda_list_finalize;

  geda_list_signals[ CHANGED ] =
    g_signal_new ("changed",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  0     /*signal_flags */,
                  0     /*class_offset */,
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE,
                  0     /* n_params */
                 );
}


/*! \brief Function to retrieve GedaList's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve GedaList's Type identifier. On the first call,
 *  this registers the pagesel in the GedaType system. Subsequently
 *  the functions returns the saved value from its first execution..
 *
 *  \return the Type identifier associated with GedaList.
 */
GedaType geda_list_get_type(void)
{
  static GedaType type = 0;
  if (type == 0) {
    static const GTypeInfo info = {
      sizeof (GedaListClass),
      NULL,                         /* base_init */
      NULL,                         /* base_finalize */
      geda_list_class_init,         /* class_init */
      NULL,                         /* class_finalize */
      NULL,                         /* class_data */
      sizeof (GedaList),
      0,                            /* n_preallocs */
      geda_list_instance_init       /* instance_init */
    };
    type = g_type_register_static (G_TYPE_OBJECT, "GedaList", &info, 0);
  }
  return type;
}


/*! \brief Returns a pointer to a new GedaList object.
 *
 *  \par Function Description
 *  Returns a pointer to a new GedaList object.
 *
 *  \return pointer to the new GedaList object.
 */
GedaList *geda_list_new( void ) {
  return g_object_new( GEDA_TYPE_LIST, NULL );
}


/*! \brief Adds the given item to the GedaList
 *
 *  \par Function Description
 *  Adds the given item to the GedaList
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] item item to add to the GedaList.
 */
void geda_list_add( GedaList *list, void *item )
{
  list->glist = g_list_append(list->glist, item );
  g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
}


/*! \brief Adds the given glist of items to the GedaList
 *
 *  \par Function Description
 *  Adds the given glist of items to the GedaList
 *  A copy is made, so the original GList is not modified.
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] items GList of items to add to the GedaList.
 */
void geda_list_add_glist( GedaList *list, GList *items )
{
  GList *glist_copy = g_list_copy( items );
  list->glist = g_list_concat(list->glist, glist_copy );
  g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
}

void geda_list_add_glist_unique ( GedaList *list, GList *items )
{
  GList *one_list = NULL;
  GList *iter;

  for (iter = items; iter; iter = iter->next) {
    if (!geda_list_is_in_list( list, iter->data)) {
      if (!g_list_find(one_list, iter->data)) {
        one_list = g_list_append(one_list, iter->data);
      }
    }
  }
  geda_list_add_glist(list, one_list);
  g_list_free(one_list);
}

/*! \brief Add item to the GedaList if not already in list
 *
 *  \par Function Description
 *  Adds the given item to the GedaList if the item is not
 *  already in the list.
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] item item to add to the GedaList.
 */
void geda_list_add_unique ( GedaList *list, void *item )
{
  if (!geda_list_is_in_list(list, item)) {
    geda_list_add(list, item);
  }
}

/*! \brief Add pointer to a string to the GedaList if not already in list
 *
 *  \par Function Description
 *  Adds the given item to the GedaList if the string is not found
 *  in the data associated with the list.
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] text string to add to the GedaList
 *
 *  \returns TRUE if the string pointer was added
 */
bool geda_list_add_unique_string (GedaList *list, char  *text)
{
  GList *iter;
  char  *str;
  bool   found;

  found = FALSE;

  for (iter = g_list_first(list->glist); iter; iter = iter->next) {

    str = iter->data;

    if (str != NULL && isalpha (str[0])) {
      if (strcmp(text, str) == 0) {
        found = TRUE;
        break;  /* No need to continue */
      }
    }
  }

  if (!found) {
    list->glist = g_list_append(list->glist, text);
  }
  return !found;
}

/*! \brief Returns a copy of the glist associated with the given GedaList
 *
 *  \par Function Description
 *  A copy is made of the glist and returned
 *
 *  \param [in] list Pointer to the GedaList
 *
 *  \retval pointer to copy of a Glist  the GedaList.
 */
GList *geda_list_copy_glist( GedaList *list )
{
  return g_list_copy(list->glist);
}


/*! \brief Find a given item in a GedaList
 *
 *  \par Function Description
 *  Searches a GedaList for the item and returns the
 *  item if found or NULL if the item was not in the list.
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] item to find for in the GedaList.
 */
void *geda_list_find( GedaList *list, void *item )
{
  return g_list_find(list->glist, item);
}


int geda_glist_is_homogeneous_objects ( GList *list)
{
  GList  *o_iter;
  Object *object;
  int     otype;
  bool    answer;

  o_iter = list;
  object = (Object *)o_iter->data;
  if (object) {
    answer = TRUE;
    otype  = object->type;
    o_iter = g_list_next (o_iter);

    while (o_iter != NULL) {
      object = (Object *)o_iter->data;
      if (object->type != otype) {
        answer = FALSE;
        break;
      }
      o_iter = g_list_next (o_iter);
    }
  }
  else
    answer = FALSE;
  return answer;
}


/*! \brief Get is a given item in a GedaList
 *
 *  \par Function Description
 *  Searches a GedaList for the item and returns TRUE
 *  if found or FALSE of the item was not in the list.
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] item to find for in the GedaList.
 */
bool geda_list_is_in_list( GedaList *list, void *item )
{
  return g_list_find(list->glist, item) ? 1 : 0;
}


/*! \brief Removes the given item from the GedaList
 *
 *  \par Function Description
 *  Removes the given item from the GedaList.
 *  It's ok to call this function with an item which
 *  is not necessarily in the list.
 *
 *  \param [in] list Pointer to the GedaList
 *  \param [in] item to remove from the GedaList.
 */
void geda_list_remove( GedaList *list, void *item )
{
  if (g_list_find(list->glist, item)) {

  list->glist = g_list_remove(list->glist, item);
    g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
  }
}

/*! \brief Removes all the items in the given GedaList.
 *
 *  \par Function Description
 *  Removes all items in the given GedaList.
 *
 *  \param [in] list Pointer to the GedaList
 */
void geda_list_remove_all( GedaList *list )
{
  g_list_free(list->glist);
  list->glist = NULL;
  g_signal_emit( list, geda_list_signals[ CHANGED ], 0 );
}
/** @} endgroup geda-list-object */
