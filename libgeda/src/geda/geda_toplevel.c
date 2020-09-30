/* -*- geda_toplevel.c -*-
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
/*! \file geda_toplevel.c
 *  \brief Geda Toplevel Object Class is derived from the base GObject Class
 */

/** \defgroup geda-toplevel Geda Top Level
 * @{
 * \brief Implmentation of #GedaToplevel Class
 * \par
 *  This module implements the geda-top-level class in libgeda.
 *  A GedaToplevel Data Structure contains information used to coordinate
 *  configuration and session information with applications such as default
 *  object properties and page hierarchy. Libgeda loads and manages all
 *  schematic and symbol documents using data stored in toplevel structures.
 *
 * \class GedaToplevel geda_toplevel.h "include/libgeda/geda_toplevel.h"
 */

#include "../../../config.h"

#include <stdio.h>

#include <libgeda_priv.h>

static GObjectClass *geda_toplevel_parent_class = NULL;

static GList *new_toplevel_hooks = NULL;

/* List of pointers to GedaToplevel instances */
static GList *list_of_toplevels = NULL;

typedef struct {
  NewToplevelFunc func;
  void *data;
} NewToplevelHook;


/*! \internal Function to Call Register Toplevel Hooks
 *  \par Function Description
 *
 */
static void call_new_toplevel_hook (void *hook, void *toplevel)
{
  NewToplevelHook *h = (NewToplevelHook*) hook;
  GedaToplevel *t    = (GedaToplevel*) toplevel;

  h->func (t, h->data);
}

/*!
 * \brief Append New TopLevel Hook List to a GedaToplevel.
 * \par Function Description
 *  Adds a callback hook \a func to \a toplevel. After a new
 *  \a toplevel is created, \a func will be called with two
 *  arguments: the \a toplevel, and the \a data.
 *
 * \sa geda_toplevel_weak_unref
 *
 * \param [in] func    Weak reference notify function.
 * \param [in] data      Data to be passed to \a func
 */
void geda_toplevel_append_new_hook (NewToplevelFunc func, void *data)
{
  NewToplevelHook *new_hook;

  new_hook = GEDA_MEM_ALLOC0 (sizeof(NewToplevelHook));
  new_hook->func = func;
  new_hook->data = data;

  new_toplevel_hooks = g_list_append (new_toplevel_hooks, new_hook);
}

/*!
 * \brief GedaType instance initializer for GedaToplevel
 * \par Function Description
 *  GedaType instance initializer for GedaToplevel,  initializes a
 *  new GedaToplevel object with sensible default properties.
 *
 * \param [in]  instance The GedaToplevel being initializing.
 * \param [in]  g_class  The class of the type the instance is created for.
 */
static void geda_toplevel_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaToplevel *toplevel           = (GedaToplevel*)instance;
  toplevel->open_flags             = F_OPEN_RC | F_OPEN_CHECK_BACKUP;
  toplevel->untitled_name          = NULL;
  toplevel->num_untitled           = 0;
  toplevel->bitmap_directory       = NULL;

  toplevel->width                  = DEFAULT_PAGE_WIDTH;
  toplevel->height                 = DEFAULT_PAGE_HEIGHT;

  toplevel->pages                  = geda_list_new();
  toplevel->page_current           = NULL;

  toplevel->bus_style              = default_bus_style;
  toplevel->line_style             = default_line_style;
  toplevel->net_style              = default_net_style;
  toplevel->pin_style              = default_pin_style;

  toplevel->thin_bus_width         = default_thin_bus_width;
  toplevel->thin_line_width        = default_thin_line_width;
  toplevel->thin_net_width         = default_thin_net_width;
  toplevel->thin_pin_width         = default_thin_pin_width;

  toplevel->thick_bus_width        = default_thick_bus_width;
  toplevel->thick_line_width       = default_thick_line_width;
  toplevel->thick_net_width        = default_thick_net_width;
  toplevel->thick_pin_width        = default_thick_pin_width;

  toplevel->image_color                    = FALSE;
  toplevel->invert_images                  = TRUE;

  toplevel->print_color                    = FALSE;
  toplevel->print_color_background         = 0;
  toplevel->print_orientation              = 0;
  toplevel->print_output_extents           = 0;
  toplevel->print_output_capstyle          = BUTT_CAP;

  toplevel->paper_width                    = 0;
  toplevel->paper_height                   = 0;

  toplevel->setpagedevice_orientation      = FALSE;
  toplevel->setpagedevice_pagesize         = FALSE;

  toplevel->postscript_prolog              = NULL;

  toplevel->net_consolidate                = TRUE;

  /* The following is an attempt at getting (deterministic) defaults */
  /* for the following variables */
  toplevel->attribute_promotion            = default_attribute_promotion;
  toplevel->promote_invisible              = default_promote_invisible;
  toplevel->keep_invisible                 = default_keep_invisible;

  toplevel->check_symbol_version           = TRUE;
  toplevel->make_backup_files              = default_make_backup_files;
  toplevel->show_full_path                 = default_show_full_path;

  toplevel->always_promote_attributes      = NULL;
  toplevel->attribute_offset               = DEFAULT_ATTRIBUTE_OFFSET;
  toplevel->attribute_font_size            = DEFAULT_ATTRIBUTE_SIZE;

  toplevel->component_groups               = NULL;

  toplevel->net_naming_priority            = 0;
  toplevel->hierarchy_traversal            = 0;
  toplevel->hierarchy_uref_mangle          = 0;
  toplevel->hierarchy_netname_mangle       = 0;
  toplevel->hierarchy_netattrib_mangle     = 0;
  toplevel->hierarchy_uref_separator       = NULL;
  toplevel->hierarchy_netname_separator    = NULL;
  toplevel->hierarchy_netattrib_separator  = NULL;
  toplevel->hierarchy_netattrib_order      = 0;
  toplevel->hierarchy_netname_order        = 0;
  toplevel->hierarchy_uref_order           = 0;
  toplevel->unnamed_netname                = NULL;
  toplevel->unnamed_busname                = NULL;

  toplevel->rendered_text_bounds_func      = NULL;
  toplevel->rendered_text_bounds_data      = NULL;

  toplevel->load_newer_backup_func         = NULL;
  toplevel->load_newer_backup_data         = NULL;

  toplevel->weak_refs                      = NULL;

  /* Auto-save interval */
  toplevel->auto_save_interval             = 0;
  toplevel->auto_save_timeout              = 0;

  /* Append toplevel to list of valid toplevel objects */
  list_of_toplevels = g_list_append(list_of_toplevels, instance);

  /* Call hooks */
  g_list_foreach (new_toplevel_hooks, call_new_toplevel_hook, toplevel);
}

/*!
 * \brief Geda Toplevel Object Finalization Function
 * \par Function Description
 *  This function removes or releases all internal references
 *  and releases the memory allocated to the given GedaToplevel
 *  data structure and then chain up to the parent's finalize
 *  handler.
 */
static void geda_toplevel_finalize(GObject *object)
{
  GedaToplevel *toplevel = (GedaToplevel*)object;
  GList *iter;

  list_of_toplevels = g_list_remove(list_of_toplevels, object);

  if (!g_list_length(list_of_toplevels)) {
    g_list_free(list_of_toplevels);
    list_of_toplevels = NULL;
  }

  if (toplevel->auto_save_timeout != 0) {
    /* Assume this works */
    g_source_remove (toplevel->auto_save_timeout);
  }

  GEDA_FREE(toplevel->untitled_name);
  GEDA_FREE(toplevel->bitmap_directory);
  GEDA_FREE(toplevel->postscript_prolog);

  GEDA_FREE(toplevel->hierarchy_uref_separator);
  GEDA_FREE(toplevel->hierarchy_netname_separator);
  GEDA_FREE(toplevel->hierarchy_netattrib_separator);
  GEDA_FREE(toplevel->unnamed_netname);
  GEDA_FREE(toplevel->unnamed_busname);

  if (toplevel->always_promote_attributes != NULL) {
    geda_glist_free_full(toplevel->always_promote_attributes, g_free);
    toplevel->always_promote_attributes = NULL;
  }

  if (toplevel->component_groups != NULL) {

    for (iter = toplevel->component_groups; iter != NULL; NEXT(iter)){
      GEDA_FREE(iter->data);
    }

    g_list_free(toplevel->component_groups);
    toplevel->component_groups = NULL;
  }

  if (toplevel->weak_refs) {

    for (iter = toplevel->weak_refs; iter != NULL; NEXT(iter)) {
      g_free (iter->data);
    }

    g_list_free (toplevel->weak_refs);

    toplevel->weak_refs = NULL;
  }

  ((GObjectClass*)geda_toplevel_parent_class)->finalize(object);
}

/*!
 * \brief GedaType class initializer for GedaToplevel
 * \par Function Description
 *  GedaType class initializer for GedaToplevel. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  klass       The GedaToplevel we are initializing
 * \param [in]  class_data  (unused)
 */
static void geda_toplevel_class_init (void *klass, void *class_data)
{
  GedaToplevelClass *class    = (GedaToplevelClass*)klass;
  GObjectClass *gobject_class = (GObjectClass*)class;
  geda_toplevel_parent_class  = g_type_class_peek_parent(class);
  gobject_class->finalize     = geda_toplevel_finalize;
}

/*!
 * \brief Function to retrieve GedaToplevel's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaToplevel Type identifier. When first called,
 *  the function registers a #GedaToplevel in the GedaTopType system to
 *  obtain an identifier that uniquely itentifies a GedaToplevel and returns
 *  the unsigned integer value. The retained value is returned on all
 *  subsequent calls.
 *
 * \returns GedaTopType identifier associated with GedaToplevel.
 */
GedaTopType geda_toplevel_get_type (void)
{
  static volatile GedaTopType geda_toplevel_type = 0;

  if (g_once_init_enter (&geda_toplevel_type)) {

    static const GTypeInfo info = {
      sizeof(GedaToplevelClass),
      NULL,                       /* base_init           */
      NULL,                       /* base_finalize       */
      geda_toplevel_class_init,   /* (GClassInitFunc)    */
      NULL,                       /* class_finalize      */
      NULL,                       /* class_data          */
      sizeof(GedaToplevel),
      0,                          /* n_preallocs         */
      geda_toplevel_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaTopType type;

    string = g_intern_static_string ("GedaToplevel");
    type   = g_type_register_static (G_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_toplevel_type, type);
  }

  return geda_toplevel_type;
}

/*!
 * \brief Returns a pointer to a new GedaToplevel object.
 * \par Function Description
 *  Create and return an empty TOPLEVEL object with sensible
 *  default properties.
 *
 * \returns pointer to the new GedaToplevel object.
 */
GedaToplevel *geda_toplevel_new (void) {
  return g_object_new(GEDA_TYPE_TOPLEVEL, NULL);
}

/*!
 * \brief Determine if object is Geda Toplevel Object.
 * \par Function Description
 *  Returns true if the argument is a GedaToplevel object. The
 *  \a toplevel is verified by looking for the pointer in list
 *  of GedaToplevels and returns false if not found. TRUE is
 *  returned when the pointer is found in the list of toplevels.
 *
 * \param [in] toplevel  Pointer to GedaToplevel Object
 *
 * \return boolean.
 */
bool is_a_geda_toplevel (GedaToplevel *toplevel)
{
  if (toplevel) {
    return g_list_find(list_of_toplevels, toplevel) ? TRUE : FALSE;
  }

  return FALSE;
}

/*!
 * \brief Set Bounds of Text Object using GedaToplevel Bound Function
 * \par Function Description
 *  If the rendered_text_bounds_func is set then rendered_text_bounds_func
 *  is called to set the bounds. When unset the bounds of \a o_current is
 *  set to zeros.
 *
 * \param [in]  toplevel   Pointer to GedaToplevel Object
 * \param [out] o_current  Object for which the bound is to be determined.
 */
bool geda_toplevel_set_text_bounds(GedaToplevel *toplevel, GedaObject *o_current)
{
  int result = 0;

  if (GEDA_IS_TOPLEVEL(toplevel)) {

    g_return_val_if_fail(GEDA_IS_TEXT(o_current), FALSE);

    int left   = 0;
    int top    = 0;
    int right  = 0;
    int bottom = 0;


    /* Check if toplevel render func is set */
    if (toplevel->rendered_text_bounds_func != NULL) {

      result = toplevel->rendered_text_bounds_func(toplevel->rendered_text_bounds_data,
                                                   o_current, &left, &top, &right, &bottom);
    }

    if (result) {
      o_current->left   = left;
      o_current->top    = top;
      o_current->right  = right;
      o_current->bottom = bottom;
    }
  }

  return result;
}

/*!
 * \brief Decrement Reference Count of Toplevel Object
 * \par Function Description
 *  Calls g_object_unref for the given top-level object, generally
 *  this destroys the object.
 *
 * \param [in] toplevel  Pointer to GedaToplevel Object
 */
void geda_toplevel_unref(GedaToplevel *toplevel)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));
  g_object_unref (toplevel);
}

/*!
 * \brief Notify weak reference watchers that a toplevel is dead.
 * \par Function Description
 *  For each entry in \a weak_refs, call notify function with the dead
 *  pointer \a dead_ptr and the entry's specified user data, and free
 *  \a weak_refs. Should be called during destruction of an structure
 *  that allows weak references.
 *
 * \param [in] toplevel  Pointer to GedaObject being destroyed.
 */
void geda_toplevel_weakref_notify (GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {
    s_weakref_notify(toplevel, toplevel->weak_refs);
    toplevel->weak_refs = NULL;
  }
}

/*!
 * \brief Add a weak reference watcher to a GedaToplevel Object
 * \par Function Description
 *  Adds the weak reference callback \a func to \a toplevel.
 *  When \a toplevel is destroyed, the \a func will be called
 *  with two arguments: the \a toplevel, and the \a data.
 *
 * \sa geda_toplevel_weak_unref
 *
 * \warning Do not use g_object_weak_ref instead!
 *
 * \param [in,out] toplevel  Toplevel to weak-reference.
 * \param [in] func          Weak reference notify function.
 * \param [in] data          Data to be passed to \a func.
 */
void geda_toplevel_weak_ref (GedaToplevel *toplevel, WeakNotifyFunc func, void *data)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  toplevel->weak_refs = s_weakref_add (toplevel->weak_refs, func, data);
}

/*!
 * \brief Remove a weak reference watcher from a GedaToplevel.
 * \par Function Description
 *  Removes the weak reference callback \a func from \a toplevel.
 *
 * \sa geda_toplevel_weak_ref()
 *
 * \warning Do not use g_object_weak_unref instead!
 *
 * \param [in,out] toplevel  Toplevel to remove weak-reference function.
 * \param [in]     func      Notify function to search for.
 * \param [in]     data      Data to be passed to \a func.
 */
void geda_toplevel_weak_unref (GedaToplevel *toplevel, WeakNotifyFunc func, void *data)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  toplevel->weak_refs = s_weakref_remove (toplevel->weak_refs, func, data);
}

/*!
 * \brief Add a weak pointer to a GedaToplevel.
 * \par Function Description
 *  Adds the weak pointer at \a weak_pointer_loc to \a toplevel. The
 *  value of \a weak_pointer_loc will be set to NULL when \a toplevel is
 *  destroyed.
 *
 * \sa geda_toplevel_remove_weak_ptr
 *
 * \param [in,out] toplevel      Toplevel to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void geda_toplevel_add_weak_ptr (GedaToplevel *toplevel, void *weak_pointer_loc)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  g_object_add_weak_pointer ((GObject*)toplevel,  weak_pointer_loc);
}

/*!
 * \brief Remove a weak pointer from an GedaToplevel Object.
 * \par Function Description
 *  Removes the weak pointer at \a weak_pointer_loc from \a toplevel.
 *
 * \sa geda_toplevel_add_weak_ptr()
 *
 * \param [in,out] toplevel      Toplevel to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void geda_toplevel_remove_weak_ptr (GedaToplevel *toplevel, void *weak_pointer_loc)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  g_object_remove_weak_pointer((GObject*)toplevel, weak_pointer_loc);
}

/* -------------------------------------------------------------- */


/*!
 * \brief Add Page list of pages of <B>toplevel</B>.
 * \par Function Description
 *  Increases reference count on the GedaPage object by one and
 *  adds \a new_page to the page list referenced by \a toplevel
 *
 * \param toplevel  The GedaToplevel object.
 * \param new_page  A GedaPage object.
 */
void geda_toplevel_add_page (GedaToplevel *toplevel, Page *new_page)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  /* append page to page list in toplevel */
  if (GEDA_IS_PAGE(new_page)) {

    if (!geda_list_is_in_list(toplevel->pages, new_page)) {
      geda_list_add_unique (toplevel->pages, g_object_ref(new_page));
    }

    new_page->seq = geda_list_length(toplevel->pages);
  }
}

/*!
 * \brief Get the Auto Save Interval
 * \par Function Description
 *  Returns the current auto save interval.
 *
 * \param [in] toplevel This toplevel
 */
int geda_toplevel_get_auto_save_interval  (GedaToplevel *toplevel)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), -1);

  return toplevel->auto_save_interval;
}

/*!
 * \brief Get the current page
 * \par Function Description
 *  This function returns a pointer the current Page object or
 *  NULL if the current page is not set or is invalid.
 *
 * \param [in,out] toplevel This toplevel
 */
Page *geda_toplevel_get_current_page (GedaToplevel *toplevel)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  if (GEDA_IS_PAGE(toplevel->page_current)) {
    return toplevel->page_current;
  }

  return NULL;
}

/*!
 * \brief Get the current open flags setting
 * \par Function Description
 *  This function returns the current value of open_flags in \a toplevel.
 *
 * \param [in] toplevel GedaToplevel object
 *
 * \returns current value of open_flags.
 */
int geda_toplevel_get_file_open_flags (GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {
    return toplevel->open_flags;
  }

  return F_OPEN_NONE;
}

/*!
 * \brief Get the Print with Color Toplevel Setting
 * \par Function Description
 *  This function returns the current value of image_color
 *  in \a toplevel. The image_color variable indicates the
 *  user preference to enable color when redendering while
 *  printing.
 *
 * \param [in] toplevel GedaToplevel object
 *
 * \returns current value of image_color.
 */
int geda_toplevel_get_image_color (GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {
    return toplevel->image_color;
  }

  return FALSE;
}

/*!
 * \brief Get the Print Inverted Images Toplevel Setting
 * \par Function Description
 *  This function returns the current value of invert_images
 *  in \a toplevel. The invert_images variable indicates the
 *  user preference to invert images when redendering while
 *  printing. Ttypically, this setting would only apply when
 *  image_color is FALSE, i.e. when printing black-and-white.
 *
 * \param [in] toplevel GedaToplevel object
 *
 * \returns current value of invert_images.
 */
int geda_toplevel_get_invert_images (GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {
    return toplevel->image_color;
  }

  return FALSE;
}

/*!
 * \brief Get the make backups setting
 * \par Function Description
 *  This function returns the current value of make_backup_files
 *  in \a toplevel.
 *
 * \param [in] toplevel GedaToplevel object
 */
int geda_toplevel_get_make_backups (GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {
    return toplevel->make_backup_files;
  }

  return FALSE;
}

/*!
 * \brief Get the Consolidate Net setting
 * \par Function Description
 *  This function returns the current value of net_consolidate
 *  in \a toplevel.
 *
 * \param [in] toplevel GedaToplevel object
 */
int geda_toplevel_get_net_consolidate (GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {
    return toplevel->net_consolidate;
  }

  return FALSE;
}

/*!
 * \brief Get Page given the Page Id
 * \par Function Description
 *  This function returns a pointer the Page object with the given
 *  page ID or NULL is no such Page exist in \a toplevel.
 *
 * \param [in] toplevel This toplevel
 * \param [in] page_id  The pid
 */
Page *geda_toplevel_get_page_by_id (GedaToplevel *toplevel, int page_id)
{
  const GList *pages, *iter;
  Page *found_page;

  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  found_page = NULL;

  pages = geda_toplevel_get_pages(toplevel);

  for (iter = pages; iter != NULL; NEXT(iter)) {

    Page *ptr = (Page*)iter->data;

    if (ptr->pid == page_id) {
      found_page = ptr;
      break;
    }
  }

  return found_page;
}

/*!
 * \brief Get the number of page in Toplevel Page list
 * \par Function Description
 *  Returns the number of pages in \a toplevel page list.
 *
 * \param [in] toplevel This toplevel
 */
int geda_toplevel_get_page_count (GedaToplevel *toplevel)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), 0);

  GList *pages = geda_list_get_glist(toplevel->pages);

  return pages ? g_list_length(pages) : 0;
}

/*!
 * \brief Get the Next page in GedaToplevel
 * \par Function Description
 *  This function returns a pointer to the next page in the \a toplevel
 *  list of pages, toplevel->pages, or NULL if the current page in the
 *  toplevel is the last page in the list or the current page is not set.
 *
 *  \param [in] toplevel This toplevel
 */
Page *geda_toplevel_get_page_down (GedaToplevel *toplevel)
{
  GList *iter;
  GList *list;
  Page  *page;

  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  list = geda_toplevel_get_pages(toplevel);

  /* Note that page_current is not validated here */
  if (toplevel->page_current) {
    iter = g_list_find(list, toplevel->page_current);
  }
  else {

    int len = geda_glist_length(list);

    if (len == 1 && GEDA_IS_PAGE(list->data)) {
      page = toplevel->page_current = list->data;
    }

    iter = NULL;
  }

  if (iter != NULL) {

    iter = iter->next;

    if (iter != NULL) {
      page = (Page*)iter->data;
    }
    else {
      page  = NULL;
    }
  }
  else {
    page  = NULL;
  }

  return page;
}

/*!
 * \brief Get the Page Up in GedaToplevel
 * \par Function Description
 *  This function returns a pointer to the previous page in the \a toplevel
 *  list of pages, toplevel->pages, or NULL if the current page in the
 *  toplevel is the first page in the list or the current page is not set.
 *
 * \param [in] toplevel This toplevel
 */
Page *geda_toplevel_get_page_up (GedaToplevel *toplevel)
{
  GList *iter;
  GList *list;
  Page  *page;

  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  list = geda_toplevel_get_pages(toplevel);

  /* Note that page_current is not validated here */
  if (toplevel->page_current) {
    iter = g_list_find(list, toplevel->page_current);
  }
  else {

    int len = geda_glist_length(list);

    if (len == 1 && GEDA_IS_PAGE(list->data)) {
      page = toplevel->page_current = list->data;
    }
    iter = NULL;
  }

  if (iter != NULL) {

    iter = iter->prev;

    if (iter != NULL) {
      page = (Page*)iter->data;
    }
    else {
      page = NULL;
    }
  }
  else {
    page = NULL;
  }

  return page;
}

/*!
 * \brief Get the List of all Pages in GedaToplevel
 * \par Function Description
 *  This function returns a pointer to the Gedalist of pages
 *  in the toplevel.
 *
 * \param [in] toplevel This toplevel
 *
 * \returns PageList of pages
 */
PageList *geda_toplevel_get_page_list (GedaToplevel *toplevel)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  return toplevel->pages;
}

/*!
 * \brief Get the untitled name in GedaToplevel
 * \par Function Description
 *  This function returns a pointer to a newly allocated string
 *  containging the default untitled name. The string should be
 *  released when no longer required.
 *
 * \param [in] toplevel This toplevel
 *
 * \returns copy of toplevel->untitled_name string
 */
char *geda_toplevel_get_untitled_name (GedaToplevel *toplevel)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  return geda_strdup(toplevel->untitled_name);
}

/*!
 * \brief Get if Page is the Current Page in Toplevel
 * \par Function Description
 *  Compares PID of \page to the PID of the current page if
 *  both are valid page objects and returns the result.
 *
 * \param [in] toplevel A toplevel object
 * \param [in] page     The page to be queried
 *
 * \returns TRUE if \a page is the current page, otherwise FALSE.
 */
bool geda_toplevel_is_current_page(GedaToplevel *toplevel, Page *page)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), FALSE);

  if (GEDA_IS_PAGE(page)) {

    if (toplevel->page_current) {
      return (toplevel->page_current->pid == page->pid);
    }
  }

  return FALSE;
}

/*!
 * \brief Geda Toplevel Move Page Down
 * \par Function Description
 *  Exchanges \a page with the next page in the list of pages in
 *  \a toplevel if a next page exist. Silently ignores if \a page
 *  is not a valid Page object or if \a page is not in the list
 *  of pages.
 *
 * \param [in] toplevel This toplevel
 * \param [in] page     The page to be moved
 *
 * \returns TRUE if \a page was moved
 */
bool geda_toplevel_move_page_down (GedaToplevel *toplevel, Page *page)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), FALSE);

  if (GEDA_IS_PAGE(page)) {

    GList *iter;
    GList *pages;

    pages = geda_list_get_glist(toplevel->pages);

    for (iter = pages; iter; iter = iter->next) {
      if (iter->data == page) {
        if (iter->next) {
          iter->data = iter->next->data;
          iter->next->data = page;
          return TRUE;
        }
      }
    }
  }

  return FALSE;
}

/*!
 * \brief Geda Toplevel Move Page Up
 * \par Function Description
 *  Exchanges \a page with the previous page in the list of pages
 *  in \a toplevel if a previous page exist. Silently ignores if
 *  \a page is not a valid Page object or if \a page is not in the
 *  list of pages.
 *
 * \param [in] toplevel This toplevel
 * \param [in] page     The page to be moved
 *
 * \returns TRUE if \a page was moved
 */
bool geda_toplevel_move_page_up (GedaToplevel *toplevel, Page *page)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), FALSE);

  if (GEDA_IS_PAGE(page)) {

    GList *iter;
    GList *pages;

    pages = geda_list_get_glist(toplevel->pages);

    for (iter = pages; iter; iter = iter->next) {
      if (iter->data == page) {
        if (iter->prev) {
          iter->data = iter->prev->data;
          iter->prev->data = page;
          return TRUE;
        }
      }
    }
  }

  return FALSE;
}

/*!
 * \brief Geda Toplevel Remove Page
 * \par Function Description
 *  Removes \a page from the list of pages in \a toplevel.
 *  Silently ignores if \a page is not a valid Page object
 *  or if \a page is not in the list.
 *
 * \param [in] toplevel This toplevel
 * \param [in] page     The page to be removed
 */
void geda_toplevel_remove_page (GedaToplevel *toplevel, Page *page)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  if (GEDA_IS_PAGE(page)) {

    if (geda_list_is_in_list(toplevel->pages, page)) {

      geda_list_remove (toplevel->pages, page);
      geda_page_unref (page);

      /* Check if removed page was page_current */

      if (toplevel->page_current == page) {
        toplevel->page_current = NULL;
      }
    }
  }
}

/*!
 * \brief Set Toplevel Auto Save Interval
 * \par Function Description
 *  Stores \a interval to \a toplevel.
 *
 * \param [in] toplevel This toplevel
 * \param [in] interval New auto-save interval in seconds
 */
bool geda_toplevel_set_auto_save_interval (GedaToplevel *toplevel, int interval)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), FALSE);

  toplevel->auto_save_interval = interval;

  return TRUE;
}

/*!
 * \brief Set Backup Loader Query Function in GedaToplevel object
 * \par Function Description
 *  Sets function to be call when a files is requested to be loaded
 *  and a newer backup file is detected.
 *
 * \param [in] toplevel  The GedaToplevel object being set
 * \param [in] func      Function to call if a backup is newer.
 * \param [in] ...       Optional data to be passed to the function.
 */
void geda_toplevel_set_bkloader_query_func (GedaToplevel *toplevel, void *func, ...)
{
  va_list argp;

  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  va_start (argp, func);
  toplevel->load_newer_backup_func = func;
  toplevel->load_newer_backup_data = va_arg(argp, void*);
  va_end (argp);
}

/*!
 * \brief Set the current page
 * \par Function Description
 *  Set the current page in \a toplevel to \a page.
 *
 * \param [in] toplevel GedaToplevel object
 * \param [in] page     Page object
 */
bool geda_toplevel_set_current_page (GedaToplevel *toplevel, Page *page)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), FALSE);

  toplevel->page_current = page;

  return TRUE;
}

/*!
 * \brief Set the Open Flags in a Toplevel
 * \par Function Description
 *  Set the current value of open_flags in \a toplevel to
 *  \a open_flags.
 *
 * \param [in,out] toplevel   GedaToplevel object
 * \param [in]     open_flags New value for open flags
 *
 * \returns previous value of open_flags.
 */
int geda_toplevel_set_file_open_flags (GedaToplevel *toplevel, int open_flags)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), 0);

  int old_flags;

  old_flags = toplevel->open_flags;

  toplevel->open_flags = open_flags;

  return old_flags;
}

/*!
 * \brief Set whether to make backup files
 * \par Function Description
 *  Set the current value of make_backup_files in \a toplevel to
 *  \a make_backups. When make_backups is TRUE,
 *
 * \param [in,out] toplevel     GedaToplevel object
 * \param [in]     make_backups If True backups will be created.
 *
 * \retval TRUE if the value was set, FALSE if \a toplevel is invalid
 *
 * \sa geda_file_save
 */
bool geda_toplevel_set_make_backups (GedaToplevel *toplevel, int make_backups)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), FALSE);

  toplevel->make_backup_files = make_backups;

  return TRUE;
}

/*!
 * \brief Set whether to Consolidate Nets
 * \par Function Description
 *  Set the value of net_consolidate in \a toplevel to \a consolidate.
 *  When net_consolidate is TRUE connected net segments will be combined
 *  when it makes sense to do so.
 *
 * \param [in,out] toplevel    GedaToplevel object
 * \param [in]     consolidate If True consolidations will be performed.
 */
void geda_toplevel_set_net_consolidate (GedaToplevel *toplevel, int consolidate)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  toplevel->net_consolidate = consolidate;
}

/*!
 * \brief Set the font-renderer-specific bounds function.
 * \par Function Description
 *  Set the function to be used to calculate text bounds for #Text
 *  Objects for all pages associated with the given #GedaToplevel.
 *  This allows a global page renderer function to be defined. If the
 *  function is not defined the renderer, and neither a Page level or
 *  Text Object level render is defined then the bounds of text can
 *  not be determined and world_get_text_bounds will return FALSE.
 *  A renderer must be define for at least one level. The order of
 *  precedence is Object level, then the Page level and lastly the
 *  GedaToplevel. Note that any previous setting is erased and
 *  passing NULL will disable rendering at this level.
 *
 * \sa geda_struct_page_set_bounds_func
 * \sa geda_text_object_set_rendered_bounds_func
 *
 * \param [in] toplevel  The GedaToplevel for which the render
 *                       function should be associated.
 * \param [in] func      Function to use.
 * \param [in] user_data User data to be passed to the function.
 */
void geda_toplevel_set_rendered_bounds_func(GedaToplevel       *toplevel,
                                            RenderedBoundsFunc  func,
                                            void               *user_data)
{
  g_return_if_fail(GEDA_IS_TOPLEVEL(toplevel));

  toplevel->rendered_text_bounds_func = func;
  toplevel->rendered_text_bounds_data = user_data;
}

/** @} endgroup geda-toplevel */
