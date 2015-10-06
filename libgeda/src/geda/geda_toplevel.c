/* -*- geda_toplevel.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Ales Hvezda
 * Copyright (C) 2013-2015 Wiley Edward Hill
 *
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
 * 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 4, 2013
 */
/*! \file geda_toplevel.c
 *  \brief Geda Toplevel Object Class derived from the base GObject Class
 */
/** \defgroup geda-toplevel Geda Toplevel Data Structure
 *  @{
 */
/*! \class GedaToplevel geda_toplevel.h "include/libgeda/geda_toplevel.h"
 *  \brief
 *  A GedaToplevel Data Structure contains information used to coordinate
 *  configuration and session information with applications such as default
 *  object properties and page hierarchy. Libgeda loads and manages all
 *  schematic and symbol documents using data stored in toplevel structures.
 */

#include <config.h>
#include <stdio.h>

#include "libgeda_priv.h"

static GObjectClass *geda_toplevel_parent_class = NULL;

static GList *new_toplevel_hooks = NULL;

typedef struct {
  NewToplevelFunc func;
  void *data;
} NewToplevelHook;


/*! \brief Internal Function to Call Register Toplevel Hooks
 *  \par Function Description
 *
 */
static void call_new_toplevel_hook (void *hook, void *toplevel)
{
  NewToplevelHook *h = (NewToplevelHook*) hook;
  GedaToplevel *t    = (GedaToplevel*) toplevel;

  h->func (t, h->data);
}

/*! \brief Append New TopLevel Hook List to a GedaToplevel.
 * \par Function Description
 * Adds a callback hook \a func to \a toplevel. After a new
 * \a toplevel is created, \a func will be called with two
 * arguments: the \a toplevel, and the \a data.
 *
 * \sa geda_toplevel_weak_unref
 *
 * \param [in] func    Weak reference notify function.
 * \param [in] data      Data to be passed to \a func
 *
 */
void geda_toplevel_append_new_hook (NewToplevelFunc func, void *data)
{
  NewToplevelHook *new_hook;

  new_hook = GEDA_MEM_ALLOC0 (sizeof(NewToplevelHook));
  new_hook->func = func;
  new_hook->data = data;

  new_toplevel_hooks = g_list_append (new_toplevel_hooks, new_hook);
}

/*! \brief GedaType instance initializer for GedaToplevel
 *
 *  \par Function Description
 *  GedaType instance initializer for GedaToplevel,  initializes a
 *  new GedaToplevel object with sensible default properties.
 *
 *  \param [in]  instance  The GedaToplevel being initialising.
 *  \param [in]  g_class   The class of the type the instance is created for.
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
  toplevel->print_output_type              = 0;
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

  toplevel->head_marker                    = GEDA_TYPE_TOPLEVEL;
  toplevel->tail_marker                    = toplevel->head_marker;

  /* Call hooks */
  g_list_foreach (new_toplevel_hooks, call_new_toplevel_hook, toplevel);

}

/*! \brief Geda Toplevel Object Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references
 *   and releases the memory allocated to the given GedaToplevel
 *   data structure and then chain up to the parent's finalize
 *   handler.
 */
static void geda_toplevel_finalize(GObject *object)
{
  GedaToplevel *toplevel = GEDA_TOPLEVEL(object);
  GList *iter;

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

  /* Delete all pages */
  s_page_delete_list (toplevel);

  /* Delete the page list */
  GEDA_UNREF (toplevel->pages);

  if (toplevel->always_promote_attributes != NULL) {
    g_list_foreach(toplevel->always_promote_attributes, (GFunc) g_free, NULL);
    g_list_free(toplevel->always_promote_attributes);
    toplevel->always_promote_attributes = NULL;
  }

  if (toplevel->component_groups != NULL) {
    for (iter = toplevel->component_groups; iter != NULL; iter = g_list_next(iter)){
      GEDA_FREE(iter->data);
    }

    g_list_free(toplevel->component_groups);
    toplevel->component_groups = NULL;
  }

  if (toplevel->weak_refs) {

    for (iter = toplevel->weak_refs; iter != NULL; iter = g_list_next (iter)) {
      g_free (iter->data);
    }
    g_list_free (toplevel->weak_refs);
  }
  toplevel->weak_refs = NULL;

  /* The object is no longer a GedaToplevel object */
  toplevel->head_marker = 1;
  toplevel->tail_marker = 0;

  G_OBJECT_CLASS(geda_toplevel_parent_class)->finalize(object);
}

/*! \brief GedaType class initializer for GedaToplevel
 *
 *  \par Function Description
 *  GedaType class initializer for GedaToplevel. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  class       The GedaToplevel we are initializing
 *  \param [in]  class_data  (unused)
 */
static void geda_toplevel_class_init (void *class, void *class_data)
{
  GedaToplevelClass *klass    = GEDA_TOPLEVEL_CLASS(class);
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
  geda_toplevel_parent_class  = g_type_class_peek_parent(klass);
  gobject_class->finalize     = geda_toplevel_finalize;

}

/*! \brief Function to retrieve GedaToplevel's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaToplevel Type identifier. When first called,
 *  the function registers a #GedaToplevel in the GedaType system to obtain
 *  an identifier that uniquely itentifies a GedaToplevel and returns the
 *  unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaToplevel.
 */
GedaType geda_toplevel_get_type (void)
{
  static GedaType geda_toplevel_type = 0;

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
    GedaType    type;

    string = g_intern_static_string ("GedaToplevel");
    type   = g_type_register_static (G_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_toplevel_type, type);
  }

  return geda_toplevel_type;
}

/*! \brief Returns a pointer to a new GedaToplevel object.
 *
 *  \par Function Description
 *  Returns a pointer to a new GedaToplevel object.
 *
 *  \return pointer to the new GedaToplevel object.
 */
GedaToplevel *geda_toplevel_new (void) {
  return g_object_new(GEDA_TYPE_TOPLEVEL, NULL);
}

/*! \brief Determine if object is Geda GedaToplevel Object.
 *  \par Function Description
 *  Returns true if the argument is a GedaToplevel object.
 *  This function use signatures embed in the structure
 *  to verify the object type as the gobject system appears
 *  unreliable and can return false results.
 *
 * \param [in] toplevel  Pointer to GedaToplevel Object
 *
 *  \return boolean.
 */
bool is_a_geda_toplevel (GedaToplevel *toplevel)
{
  return ((unsigned long)toplevel > 0x7FFFE) &&
  (GEDA_TYPE_TOPLEVEL == (toplevel->head_marker & toplevel->tail_marker));
}

bool
geda_toplevel_set_bounds(GedaToplevel *toplevel, Object *o_current)
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

/*! \brief Decrement Reference Count of Toplevel Object
 *  \par Function Description
 *  Calls g_object_unref for the given top-level object, generally
 *  this destroys the object.
 *
 * \param [in] toplevel  Pointer to GedaToplevel Object
 */
void
geda_toplevel_unref(GedaToplevel *toplevel)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));
  g_object_unref (toplevel);
}

/*! \brief Notify weak reference watchers that a toplevel is dead.
 * \par Function Description
 * For each entry in \a weak_refs, call notify function with the dead
 * pointer \a dead_ptr and the entry's specified user data, and free
 * \a weak_refs. Should be called during destruction of an structure
 * that allows weak references.
 *
 * \param [in] toplevel  Pointer to Object being destroyed.
 *
 */
void
geda_toplevel_weakref_notify (GedaToplevel *toplevel)
{
  if (GEDA_IS_TOPLEVEL(toplevel)) {
    s_weakref_notify(toplevel, toplevel->weak_refs);
    toplevel->weak_refs = NULL;
  }
}

/*! \brief Add a weak reference watcher to a GedaToplevel Object
 *
 *  \par Function Description
 *
 *   Adds the weak reference callback \a func to \a toplevel.
 * When \a toplevel is destroyed, the \a func will be called
 * with two arguments: the \a toplevel, and the \a data.
 *
 * \sa geda_toplevel_weak_unref
 *
 * \warning Do not use g_object_weak_ref instead!
 *
 * \param [in,out] toplevel  Toplevel to weak-reference.
 * \param [in] func          Weak reference notify function.
 * \param [in] data          Data to be passed to \a func.
 */
void
geda_toplevel_weak_ref (GedaToplevel *toplevel, WeakNotifyFunc func, void *data)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));
  toplevel->weak_refs = s_weakref_add (toplevel->weak_refs, func, data);
}

/*! \brief Remove a weak reference watcher from a GedaToplevel.
 * \par Function Description
 * Removes the weak reference callback \a func from \a toplevel.
 *
 * \sa geda_toplevel_weak_ref()
 *
 * \warning Do not use g_object_weak_unref instead!
 *
 * \param [in,out] toplevel  Toplevel to remove weak-reference function.
 * \param [in]     func      Notify function to search for.
 * \param [in]     data      Data to be passed to \a func.
 */
void
geda_toplevel_weak_unref (GedaToplevel *toplevel, WeakNotifyFunc func, void *data)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));
  toplevel->weak_refs = s_weakref_remove (toplevel->weak_refs, func, data);
}

/*! \brief Add a weak pointer to a GedaToplevel.
 * \par Function Description
 * Adds the weak pointer at \a weak_pointer_loc to \a toplevel. The
 * value of \a weak_pointer_loc will be set to NULL when \a toplevel is
 * destroyed.
 *
 * \sa geda_toplevel_remove_weak_ptr
 *
 * \param [in,out] toplevel      Toplevel to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
geda_toplevel_add_weak_ptr (GedaToplevel *toplevel, void *weak_pointer_loc)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));
  g_object_add_weak_pointer ((GObject*)toplevel,  weak_pointer_loc);
}

/*! \brief Remove a weak pointer from an GedaToplevel Object.
 * \par Function Description
 * Removes the weak pointer at \a weak_pointer_loc from \a toplevel.
 *
 * \sa geda_toplevel_add_weak_ptr()
 *
 * \param [in,out] toplevel      Toplevel to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
geda_toplevel_remove_weak_ptr (GedaToplevel *toplevel, void *weak_pointer_loc)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));
  g_object_remove_weak_pointer((GObject*)toplevel, weak_pointer_loc);
}

void geda_toplevel_add_page (GedaToplevel *toplevel, Page *page)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  /* append page to page list of toplevel */
  if (GEDA_IS_PAGE(page)) {
    geda_list_add_unique (toplevel->pages, page);
  }
}

Page *geda_toplevel_get_current_page (GedaToplevel *toplevel)
{
  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  return toplevel->page_current;
}

bool geda_toplevel_set_current_page (GedaToplevel *toplevel, Page *page)
{
  const GList *iter;
  bool  found_page;

  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), FALSE);

  found_page = FALSE;

  if (!page) {
    toplevel->page_current = NULL;
  }
  else if (GEDA_IS_PAGE(page)) {

    iter = geda_list_get_glist(toplevel->pages);

    while (iter != NULL) {

      if (iter->data == page) {
        toplevel->page_current = page;
        break;
      }
      NEXT(iter);
    }
  }

  return found_page;
}

Page *geda_toplevel_get_page (GedaToplevel *toplevel, int page_id)
{
  const GList *iter;
  Page *found_page;

  g_return_val_if_fail (GEDA_IS_TOPLEVEL(toplevel), NULL);

  found_page = NULL;

  for (iter = geda_list_get_glist(toplevel->pages); iter != NULL; NEXT(iter))
  {
    Page *ptr = (Page*)iter->data;

    if (ptr->pid == page_id) {
      found_page = ptr;
      break;
    }
  }

  return found_page;
}

void geda_toplevel_remove_page (GedaToplevel *toplevel, Page *page)
{
  g_return_if_fail (GEDA_IS_TOPLEVEL(toplevel));

  if (page && geda_list_is_in_list(toplevel->pages, page)) {
    geda_list_remove (toplevel->pages, page);
  }
}

/** @} endgroup geda-toplevel */
