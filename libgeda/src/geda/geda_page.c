/* -*- geda_page.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 4, 2013
 */
/*! \file geda_page.c
 *  \brief Geda Page Object is derived from the base GObject Class
 */

/** \defgroup geda-toplevel Geda Top Level
 * @{
 * \brief Implmentation of #GedaToplevel Class
 * \par
 *  A Geda Page Data Structure is use to manage information related
 *  to a single opened document.
 *
 * \class GedaPage geda_page.h "include/libgeda/geda_page.h"
 */

#include "../../../config.h"

#include <libgeda_priv.h>

static GObjectClass *geda_page_parent_class = NULL;

static int global_pid = 0; /* Global integer for Page Indentification */

static GList *new_page_hooks = NULL;

/* List of pointers to GedaPage instances */
static GList *list_of_pages = NULL;

typedef struct {
  NewPageFunc func;
  void *data;
} NewPageHook;

typedef struct {
  ConnsChangedFunc func;
  void *data;
} ConnsChangedHook;

/*! \brief Internal Function to Call Register Page Hooks
 *  \par Function Description
 *
 */
static void call_new_page_hook (void *hook, void *page)
{
  NewPageHook *h = (NewPageHook*) hook;
  Page *p = (Page*) page;

  h->func (p, h->data);
}

/*! \brief Append New Page Hook List to this Page.
 * \par Function Description
 * Adds a callback hook \a notify_func to \a page. After a new
 * \a page is created, \a notify_func will be called with two
 * arguments: the \a page, and the \a user_data.
 *
 * \sa page_weak_unref
 *
 * \param [in] func      Page to add the Hook.
 * \param [in] data      Data to be passed to \a notify_func
 *
 */
void geda_page_append_new_hook (NewPageFunc func, void *data)
{
  NewPageHook *new_hook;

  new_hook = GEDA_MEM_ALLOC0(sizeof(NewPageHook));
  new_hook->func = func;
  new_hook->data = data;

  new_page_hooks = g_list_append (new_page_hooks, new_hook);
}

void geda_page_add_object (Page *page, GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {
    page->_object_list = g_list_append (page->_object_list, object);
  }
}

/*! \brief Search for an GedaObject given the sid.
 *  \par Function Description
 *  This functions returns the <b>GedaObject</b> that has the \a sid
 *  from the page object list or NULL if there is no such <b>GedaObject</b>.
 *
 *  \param [in] page    Pointer to a Page.
 *  \param [in] sid     The ID of the GedaObject to find.
 *
 *  \returns A pointer on the object found or NULL if not found.
 */
GedaObject *geda_page_get_object(Page *page, int sid)
{
  if (GEDA_IS_PAGE(page)) {

    const GList *iter = g_list_first(page->_object_list);

    while (iter != NULL) {

      GedaObject *object = (GedaObject *)iter->data;

      if (object->sid == sid) {
        return object;
      }
      NEXT(iter);
    }
  }
  return NULL;
}

/*!
 * \brief Remove an object from a Page
 * \par Function Description
 *  The function decreases the reference count of object. The object's page
 *  must be @page. Increase its reference count prior to calling this function
 *
 * \param [in] page    The Page from which the object is to be removed.
 * \param [in] object  The object to be removed.
 */
void geda_page_remove_object(Page *page, GedaObject *object)
{
  g_return_if_fail(GEDA_IS_PAGE(page));

  if (g_list_find(page->_object_list, object)) {
    page->_object_list = g_list_remove(page->_object_list, object);
  }
}

/*!
 * \brief Remove all objects from a Page object
 * \par Function Description
 *  The function derecments the reference for all objects on a Page and then
 *  sets the Page object list to NULL.
 *
 * \sa eda_page_remove_object
 *
 * \param [in]  page  The Page from which all objects are to be removed.
 */
void geda_page_remove_all_objects(Page *page)
{
    GList *iter;

    g_return_if_fail(GEDA_IS_PAGE(page));

    while ((iter = g_list_last(page->_object_list))) {

      GedaObject *object = GEDA_OBJECT(iter->data);

    if (object) {
        GEDA_UNREF (object);
      }
    }

    page->_object_list = NULL;
}

/*!
 * \brief GedaType instance initializer for Page
 * \par Function Description
 *  GedaType instance initializer for Page, initializes a new empty
 *  Page object by setting pointers to NULL and numbers to zero,
 *  the page PID variable is set to the next page index.
 *
 * \param [in] instance The Page being initializing.
 * \param [in] class    The class the instance is created for.
 */
static void geda_page_instance_init( GTypeInstance *instance, void *class)
{
  Page *page                      = (Page *)instance;
  page->pid                       = global_pid++;
  page->seq                       = -1;

  page->_object_list              = NULL;
  page->selection_list            = NULL;
  page->place_list                = NULL;
  page->object_lastplace          = NULL;

  page->major_changed_refdes      = NULL;

  page->CHANGED                   = 0;

  page->page_control              = 0;
  page->show_hidden_text          = 0;

  page->rendered_text_bounds_func = NULL;
  page->rendered_text_bounds_data = NULL;

  page->change_notify_funcs       = NULL;

  page->attribs_changed_hooks     = NULL;
  page->conns_changed_hooks       = NULL;

  page->weak_refs                 = NULL;

  /* Append page to list of valid page objects */
  list_of_pages = g_list_append(list_of_pages, instance);

  /* Call hooks */
  g_list_foreach (new_page_hooks, call_new_page_hook, page);
}

/*!
 * \brief Geda Page Object Dispose Function
 * \par Function Description
 *  This function removes all object from a page, de-references
 *  allocated objects and chain up to the parent's disposer.
 */
static void geda_page_dispose(GObject *object)
{
  GList *iter;
  Page *page = GEDA_PAGE(object);

  geda_page_remove_all_objects(page);

  if (page->major_changed_refdes) {
    iter = page->major_changed_refdes;
    while (iter) {
      GEDA_FREE(iter->data);
      iter = g_list_next(iter);
    }
    g_list_free(page->major_changed_refdes);
    page->major_changed_refdes = NULL;
  }

  /* Unreference change notification handlers */
  GEDA_UNREF (page->change_notify_funcs);

  for (iter = page->attribs_changed_hooks; iter != NULL; NEXT(iter)) {
    GEDA_FREE (iter->data);
  }
  g_list_free (page->attribs_changed_hooks);
  page->attribs_changed_hooks = NULL;

  for (iter = page->conns_changed_hooks; iter != NULL; NEXT(iter)) {
    GEDA_FREE (iter->data);
  }
  g_list_free (page->conns_changed_hooks);
  page->conns_changed_hooks = NULL;

  if (page->weak_refs) {

    for (iter = page->weak_refs; iter != NULL; iter = g_list_next (iter)) {
      g_free (iter->data);
    }
    g_list_free (page->weak_refs);
  }
  page->weak_refs = NULL;

  if (page->place_list) {
    g_list_free(page->place_list);
    page->place_list = NULL;
  }

  if (page->selection_list) {
    geda_list_remove_all(page->selection_list);
    geda_list_unref(page->selection_list);
    page->selection_list = NULL;
  }

  G_OBJECT_CLASS(geda_page_parent_class)->dispose(object);
}

/*!
 * \brief Geda Page Object Finalization Function
 * \par Function Description
 *  This function removes or releases all internal references
 *  and releases the memory allocated to the given Page data
 *  structure and then chain up to the parent's finalize
 *  handler.
 */
static void geda_page_finalize(GObject *object)
{
  Page *page = GEDA_PAGE(object);

  list_of_pages = g_list_remove(list_of_pages, object);

  if (page->_object_list) {
    g_list_free(page->_object_list);
    page->_object_list = NULL;
  }

  if (!g_list_length(list_of_pages)) {
    g_list_free(list_of_pages);
    list_of_pages = NULL;
  }

  if (page->filename) {
    GEDA_FREE(page->filename);
  }

  G_OBJECT_CLASS(geda_page_parent_class)->finalize(object);
}

/*!
 * \brief GedaType class initializer for Page
 * \par Function Description
 *  GedaType class initializer for Page. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in,out] class       A PageClass GedaObject
 * \param [in]     class_data  A Page data structure
 */
static void geda_page_class_init(void *class, void *class_data)
{
  GObjectClass *object_class = (GObjectClass*) class;

  geda_page_parent_class     = g_type_class_peek_parent(class);

  object_class->dispose      = geda_page_dispose;
  object_class->finalize     = geda_page_finalize;
}

/*!
 * \brief Function to retrieve Page's Type identifier.
 * \par Function Description
 *  Function to retrieve a #Page Type identifier. When first called,
 *  the function registers a #Page in the GedaPageType system to
 *  obtain an identifier that uniquely itentifies a Page and returns
 *  the unsigned integer value. The retained value is returned on all
 *  subsequent calls.
 *
 * \return GedaPageType identifier associated with Page.
 */
GedaPageType geda_page_get_type (void)
{
  static volatile GedaPageType geda_page_type = 0;

  if (g_once_init_enter (&geda_page_type)) {

    static const GTypeInfo info = {
      sizeof(PageClass),
      NULL,                          /* base_init           */
      NULL,                          /* base_finalize       */
      geda_page_class_init,          /* (GClassInitFunc)    */
      NULL,                          /* class_finalize      */
      NULL,                          /* class_data          */
      sizeof(Page),
      0,                             /* n_preallocs         */
      geda_page_instance_init        /* (GInstanceInitFunc) */
    };

    const char  *string;
    GedaPageType type;

    string = g_intern_static_string ("Page");
    type   = g_type_register_static (G_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_page_type, type);
  }

  return geda_page_type;
}

/*!
 * \brief Return True if object is Geda PagePage.
 * \par Function Description
 *  Returns true if the argument is a Geda Page object.
 *
 * \return boolean.
 */
bool is_a_geda_page (const Page *page)
{
  if (page) {
    return g_list_find(list_of_pages, page) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Returns a pointer to a new Page object.
 * \par Function Description
 *  Returns a pointer to a new Page object.
 *
 * \return pointer to the new Page object.
 */
Page *geda_page_new (void)
{
  Page *page;
  page = g_object_new (geda_page_get_type(), NULL);
  return page;
}

/*!
 * \brief Returns a pointer to a new Page object.
 * \par Function Description
 *  Returns a pointer to a new Page object.
 *
 * \return pointer to the new Page object.
 */
Page *geda_page_new_with_notify (void)
{
  Page *page;
  page = g_object_new( geda_page_get_type(), NULL);
  page->change_notify_funcs = geda_notify_list_new();

  return page;
}

/*!
 * \brief Decrement the GObject Reference Count
 * \par Function Description
 *  GedaPage are automatically referenced when added to a
 *  GedaToplevel and unreferenced when the page is removed,
 *  such as when the GedaToplevel object is destroyed.
 */
void geda_page_unref(Page *page)
{
  if (GEDA_IS_PAGE(page)) {
    if (page->toplevel) {
      geda_toplevel_unref (page->toplevel);
      page->toplevel = NULL;
    }
    g_object_unref (page);
  }
}

/*!
 * \brief Notify weak reference watchers that a structure is dead.
 * \par Function Description
 *  For each entry in \a weak_refs, call notify function with the dead
 *  pointer \a dead_ptr and the entry's specified user data, and free
 *  \a weak_refs. Should be called during destruction of an structure
 *  that allows weak references.
 *
 * \param [in] page  Pointer to Page Object being destroyed.
 */
void geda_page_weakref_notify (Page *page)
{
  if (GEDA_IS_PAGE(page)) {
    s_weakref_notify(page, page->weak_refs);
    page->weak_refs = NULL;
  }
}

/*!
 * \brief Add a weak reference watcher to a Page Object
 * \par Function Description
 *  Adds the weak reference callback \a notify_func to \a Page.
 *  When \a Page is destroyed, the \a notify_func will be called
 *  with two arguments: the \a Page, and the \a user_data.
 *
 * \note This function is for legacy purposes; since Page is
 *       now a GObject, just use g_object_weak_ref instead!
 *
 * \sa page_weak_unref
 *
 * \param [in,out] page       Page  to weak-reference.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 */
void geda_page_weak_ref (Page *page, WeakNotifyFunc notify_func, void *user_data)
{
  if (GEDA_IS_PAGE(page) && notify_func !=NULL) {
    page->weak_refs = s_weakref_add (page->weak_refs, notify_func, user_data);
  }
}

/*!
 * \brief Remove a weak reference watcher from a Page.
 * \par Function Description
 *  Removes the weak reference callback \a notify_func from \a Page.
 *
 * \sa page_weak_ref()
 *
 * \warning Do not use g_object_weak_unref instead!
 *
 * \param [in,out] page        Page to remove weak-reference function.
 * \param [in]     notify_func Notify function to search for.
 * \param [in]     user_data   Data to to search for.
 */
void geda_page_weak_unref (Page *page, WeakNotifyFunc notify_func, void *user_data)
{
  if (GEDA_IS_PAGE(page) && notify_func !=NULL) {
    page->weak_refs = s_weakref_remove (page->weak_refs, notify_func, user_data);
  }
}

/*!
 * \brief Add a weak pointer to a Page.
 * \par Function Description
 *  Adds the weak pointer at \a weak_pointer_loc to \a page. The
 *  value of \a weak_pointer_loc will be set to NULL when \a page is
 *  destroyed.
 *
 * \sa page_remove_weak_ptr
 *
 * \param [in,out] page          Page to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void geda_page_add_weak_ptr (Page *page, void *weak_pointer_loc)
{
  if (GEDA_IS_PAGE(page)) {
    g_object_add_weak_pointer ((GObject*)page, weak_pointer_loc);
  }
}

/*!
 * \brief Remove a weak pointer from an Page.
 * \par Function Description
 *  Removes the weak pointer at \a weak_pointer_loc from \a page.
 *
 * \sa page_add_weak_ptr()
 *
 * \param [in,out] page          Page to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void geda_page_remove_weak_ptr(Page *page, void *weak_pointer_loc)
{
  if (GEDA_IS_PAGE(page) && weak_pointer_loc !=NULL) {
    g_object_remove_weak_pointer ((GObject*)page, weak_pointer_loc);
  }
}

/*!
 * \brief Suspends Page Change Notifications
 * \par Function Description
 *  This function suspend notifications until the freeze count is
 *  reduced to zero.
 *
 * \sa geda_notify_list_freeze
 */
void geda_page_freeze_notify(Page *page)
{
  if (GEDA_IS_PAGE(page)) {
    geda_notify_list_freeze(page->change_notify_funcs);
  }
}

/*!
 * \brief Reduce the Freeze Notification Count
 * \par Function Description
 *  Causes the freeze count to be reduced by one.
 *
 * \sa geda_notify_list_thaw
 */
void geda_page_thaw_notify(Page *page)
{
  if (GEDA_IS_PAGE(page)) {
    geda_notify_list_thaw(page->change_notify_funcs);
  }
}

/*!
 * \brief Rename a Page
 * \par Function Description
 *  Replaces the filename string in \a page with \a newname and sets
 *  the page->CHANGED flag.
 *
 * \param [in,out] page     A valid Page object.
 * \param [in]     newname  New string name for page filename.
 *
 * \returns TRUE if the page was renamed, otherwise FALSE.
 *
 * \sa geda_page_set_filename
 */
int geda_page_rename(Page *page, const char *newname)
{
  bool result = FALSE;

  if (GEDA_IS_PAGE(page) && newname) {
    if (page->filename) {
      GEDA_FREE(page->filename);
    }
    page->filename = geda_utility_string_strdup(newname);
    page->CHANGED = TRUE;
    result = TRUE;
  }
  return result;
}

/*!
 * \brief Set the top level associated with a GedaPage
 * \par Function Description
 *  For now, the toplevel should only be set once, we do not have a
 *  low-level clone or copy method and the hooks are holding pointer
 *  to the toplevel.
 */
void geda_page_set_toplevel (Page *page, GedaToplevel *toplevel)
{
  g_return_if_fail (GEDA_IS_PAGE(page));

  if (page->toplevel) {
    g_object_unref (page->toplevel);
  }

  if (toplevel && GEDA_IS_TOPLEVEL(toplevel)) {
    page->toplevel = g_object_ref (toplevel);
  }
  else {
    page->toplevel = toplevel;
  }
}

/*!
 * \brief Retrieve the GedaToplevel associated with a Page object
 * \par Function Description
 *  Returns the toplevel member. Normally the toplevel member is
 *  set when the page is created with geda_struct_page_new or
 *  geda_struct_page_new_with_notify.
 */
GedaToplevel *geda_page_get_toplevel (Page *page)
{
  return GEDA_IS_PAGE(page) ? page->toplevel : NULL;
}

/*!
 * \brief Retrieve the CHANGED state of Page object
 * \par Function Description
 *  Returns the CHANGED member or -1 if \a page is not a valid
 *  Page object.
 */
int geda_page_get_changed (Page *page)
{
  return GEDA_IS_PAGE(page) ? page->CHANGED : -1;
}

/*!
 * \brief Set the CHANGED state of Page object
 * \par Function Description
 *  Sets the CHANGED member to the value of \a changed if \a page
 *  is a valid Page object.
 */
void geda_page_set_changed (Page *page, int changed)
{
  if (GEDA_IS_PAGE(page)) {
    page->CHANGED = changed;
  }
}

/*!
 * \brief Retrieve List of RefDes with Major Changes
 * \par Function Description
 *  Returns the major_changed_refdes member or NULL if \a page
 *  is not a valid Page object. The list itself could be NULL.
 */
GList *geda_page_get_changed_refdes (Page *page)
{
  return GEDA_IS_PAGE(page) ? page->major_changed_refdes : NULL;
}

/*!
 * \brief Get the filename property of the Page
 * \par Function Description
 *  Retrieves the filename member of the page object. The filename
 *  stored in the page is the full name, i.e. including the path.
 *  The returned string should not be modified.
 */
const char *geda_page_get_filename (Page *page)
{
  return GEDA_IS_PAGE(page) ? page->filename : NULL;
}

/*!
 * \brief Get a copy of the page filename property
 * \par Function Description
 *  Retrieves a copy of the filename of the page. The filename stored
 *  in the page is the full name, which includes the path. The returned
 *  string should be freed when no longer needed.
 */
char *geda_page_get_filename_dup (Page *page)
{
  return GEDA_IS_PAGE(page) ? geda_strdup(page->filename) : NULL;
}

/*!
 * \brief Set the filename associated with the Page object
 * \par Function Description
 *  Makes a copy of \a filename and stores the pointer in the page
 *  filename member.
 */
void geda_page_set_filename (Page *page, const char *filename)
{
  if (GEDA_IS_PAGE(page)) {
    if (page->filename) {
      GEDA_FREE(page->filename);
    }
    page->filename = geda_utility_string_strdup(filename);
  }
}

/*!
 * \brief Get the Page Index
 * \par Function Description
 *  The Page index is a unique integer value assigned when the
 *  page was created. Index are not reused during a given session
 *  so this index can be used to identify the page. The index is
 *  not the index of the page in the list of pages in the toplevel.
 */
int geda_page_get_pid (Page *page)
{
  return GEDA_IS_PAGE(page) ?  page->pid : -1;
}

/*!
 * \brief Retrieve the Place List Buffer
 * \par Function Description
 *  The Place List is a temporary list of one or more objects, which
 *  may or may not belong to the page that is used as a working buffer
 *  during editing operations.
 */
GList *geda_page_get_place_list (Page *page)
{
  return GEDA_IS_PAGE(page) ? page->place_list : NULL;
}

/*!
 * \brief Set the Place List in the PAge
 * \par Function Description
 *  This function assigns the list given by \a object_list to
 *  the page->place_list. This function does not release the
 *  current list.
 */
void geda_page_set_place_list (Page *page, GList *object_list)
{
  if (GEDA_IS_PAGE(page)) {
    page->place_list = object_list;
  }
}

/*!
 * \brief Retrieve the Selection List
 * \par Function Description
 *  The Selection is a GedaList of objects belonging to the page
 *  that have the "selected" flag set, which allow signals to be
 *  generated when the selection changes. This function returns
 *  the underlining double linked GList held by the GedaList,
 *  which is generally more useful during editing operations.
 */
GList *geda_page_get_selection_list(Page *page)
{
  return GEDA_IS_PAGE(page) ? geda_list_get_glist(page->selection_list) : NULL;
}

/** @} endgroup geda-page */

/*!
 * \brief Print Contents of Page Structure for Debugging
 * \par Function Description
 *  This function can be useful when debugging page objects.
 *  Prints assigned pointers and values in \a page.
 */
void geda_page_debug_print (Page *page)
{
  printf( "toplevel=%p, pid=%d, seq=%d\n", page->toplevel, page->pid, page->seq);

  printf( "filename=%s\n", page->filename);

  printf( "object    count=%d\n", g_list_length(page->_object_list));
  printf( "selection count=%d\n", g_list_length(page->selection_list->glist));
  printf( "place     count=%d\n", g_list_length(page->place_list));

  if (page->object_lastplace)
    printf( "object_lastplace=%s\n", page->object_lastplace->name);
  else
    printf( "last object is not set\n");

  printf( "major_changed_refdes count=%d\n", g_list_length(page->major_changed_refdes));

  printf( "CHANGED=%d\n", page->CHANGED);

  printf( "left=%d, right=%d, top=%d, bottom=%d\n", page->left, page->right, page->top, page->bottom);

  printf( "width=%d, height=%d\n", page->width, page->height);

  printf( "coord_aspectratio=%5.10f\n", page->coord_aspectratio);

  printf( "to_screen_x_constant=%5.10f, to_screen_y_constant=%5.10f\n", page->to_screen_x_constant, page->to_screen_y_constant);

  printf( "to_world_x_constant=%5.10f, to_world_y_constant=%5.10f\n", page->to_world_x_constant, page->to_world_y_constant);

  int x_res = (page->left - page->right) * page->to_screen_x_constant;
  int y_res = (page->bottom - page->top) * page->to_screen_y_constant;

  printf( "screen resolution x=%d, y=%d\n", x_res, y_res);

  printf( "show_hidden_text=%d\n", page->show_hidden_text);

  printf( "file_modified_time=%f\n", difftime(page->last_load_or_save_time, 0));

  printf( "saved_since_first_loaded=%d\n", page->saved_since_first_loaded);
  printf( "ops_since_last_backup=%d\n", page->ops_since_last_backup);
  printf( "do_autosave_backup=%d\n", page->do_autosave_backup);

  printf( "rendered_text_bounds_func=%p\n", page->rendered_text_bounds_func);
  printf( "rendered_text_bounds_data=%p\n", page->rendered_text_bounds_data);

  printf( "change_notify_funcs    count=%d\n", g_list_length(page->change_notify_funcs->glist));
  printf( "attribs_changed_hooks  count=%d\n", g_list_length(page->attribs_changed_hooks));
  printf( "conns_changed_hooks    count=%d\n", g_list_length(page->conns_changed_hooks));
  printf( "weak_refs              count=%d\n\n", g_list_length(page->weak_refs));
};
