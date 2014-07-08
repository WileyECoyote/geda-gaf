#include <config.h>

#include "libgeda_priv.h"

/*!
 *  \brief Create an Empty Object
 *  \par Function Description
 *  Create and return an empty <b>Object</b> object with sensible defaults
 *  for its properties.
 *
 *  \returns the newly created Object.
 *
 *  \todo rethink block below that is set in gschem but used in libgeda.
 */
Object *s_object_new (int type, char const *name)
{
  return geda_object_new(type, name);
}

void s_object_add(Object *parent, Object *child){

  /* if the object is on a page then add the child */
  Page *page = geda_object_get_page(parent);
  if (page && (GEDA_IS_PAGE(page))) {
    s_page_append_object(page, child);
  }

  o_attrib_add(parent, child);
}
/*! \brief Remove an Object
 *  \par Function Description
 *  This function does not
 */
/*
 * Note: WEH (11/04/13): Modified to add conditional for s_conn_remove_object
 * to else clause of if page member, because the connections would be removed
 * by pre_object_remove if the object was on a page, Also added check for NULL
 * conn_list since there is no point in making the call if no connections exist.
 *
 */
void
s_object_release(Object *o_current)
{
  if (GEDA_IS_OBJECT(o_current)) {

    /* If currently attached to a page, remove it from the page */
    if (o_current->page != NULL) {
      s_page_remove_object (o_current->page, o_current);
    }
    else if ( o_current->conn_list != NULL ) {
      s_conn_remove_object (o_current);
    }

    if (o_current->attached_to != NULL) {
      /* do the actual remove */
      o_attrib_remove(&o_current->attached_to->attribs, o_current);
    }
    o_attrib_detach_all (o_current);

    if (o_current->complex) {

      if (o_current->complex->prim_objs) {
        s_object_release_objects (o_current->complex->prim_objs);
        o_current->complex->prim_objs = NULL;
      }
    }

    geda_object_weakref_notify(o_current);
    geda_object_unref(o_current);

    o_current=NULL; /* misc clean up */
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *   deletes everything including the GList
 */
void
s_object_release_objects(GList *list)
{
  Object *o_current = NULL;
  GList  *ptr;

  ptr = g_list_last(list);

  /* do the delete backwards */
  while(ptr != NULL) {
    o_current = (Object *) ptr->data;
    s_object_release(o_current);
    ptr = g_list_previous (ptr);
  }

  g_list_free(list);

}
void s_object_set_page_changed (Object *obj)
{
  Page *page = geda_object_get_page (obj);
  if (page != NULL){
    page->CHANGED = TRUE;
  }
}
