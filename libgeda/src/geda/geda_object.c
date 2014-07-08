/* -*- geda_object.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2014 Ales Hvezda
 * Copyright (C) 2013-2014 Wiley Edward Hill
 *
 * Copyright (C) 2013-2014 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>
#include <ascii.h>
#include "libgeda_priv.h"
enum {
  OBJECT_TYPE = 1,
  OBJECT_ID,
  OBJECT_NAME,
  OBJECT_PARENT,
  OBJECT_SELECTABLE,
  OBJECT_NO_REDRAW,
  OBJECT_SELECTED,
  OBJECT_SHOW_NAME_VALUE,
  OBJECT_VISIBLE,
};

static GObjectClass *geda_object_parent_class = NULL;

/*! this is modified here and in o_list.c */
static int global_sid = 0; /* Global integer for Object Indentification */

static GList *new_object_hooks = NULL;

typedef struct {
  NewObjectFunc func;
  void *data;
} NewObjectHook;


/*! \brief Internal Function to Call Register Object Hooks
 *  \par Function Description
 *
 */
static void call_new_object_hook (void *hook, void *object)
{
  NewObjectHook *h = (NewObjectHook*) hook;
  Object *o = (Object*) object;

  h->func (o, h->data);
}

/*! \brief Append New Object Hook List to this Object.
 * \par Function Description
 * Adds a callback hook \a notify_func to \a object. After a new
 * \a object is created, \a notify_func will be called with two
 * arguments: the \a object, and the \a user_data.
 *
 * \sa object_weak_unref
 *
 * \param [in] func      notify function.
 * \param [in] data      Data to be passed to \a notify_func
 *
 */
void geda_object_append_new_hook (NewObjectFunc func, void *data)
{
  NewObjectHook *new_hook;

  new_hook = g_new0 (NewObjectHook, 1);
  new_hook->func = func;
  new_hook->data = data;

  new_object_hooks = g_list_append (new_object_hooks, new_hook);
}

/* BEGIN ------+-------+------ Property Handlers ------+-------+-------+-----*/

int geda_object_no_bounds (Object *o)
{
  fprintf(stderr, "ERROR: <geda_object_bound> bounds function not set, name=%s\n", o->name);
  return FALSE;
}
int geda_object_bounds(Object *object)
{
  ObjectClass *object_class = (ObjectClass*)G_OBJECT_GET_CLASS(object);

  return object_class ? object_class->bounds(object) : 0;
}

/*! \brief GedaObject property getter function
 *
 *  \par Function Description
 *  Getter function for GedaObject's properties,
 *
 *  \param [in]  gobject      The GedaObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
geda_object_get_property (GObject *gobject, unsigned int property_id,
                          GValue  *value,   GParamSpec  *pspec)
{
  Object *object = GEDA_OBJECT(gobject);

  switch (property_id)
  {
    case OBJECT_TYPE:
      g_value_set_schar (value, object->type);
      break;
    case OBJECT_ID:
      g_value_set_int (value, object->sid);
      break;
    case OBJECT_NAME:
      g_value_set_string (value, object->name);
      break;
    case OBJECT_PARENT:
      g_value_set_pointer (value, object->parent_object);
      break;
    case OBJECT_SELECTABLE:
      g_value_set_boolean (value, object->selectable);
      break;
    case OBJECT_NO_REDRAW:
      g_value_set_boolean (value, object->dont_redraw);
      break;
    case OBJECT_SELECTED:
      g_value_set_boolean (value, object->selected);
      break;
    case OBJECT_SHOW_NAME_VALUE:
      g_value_set_int (value, object->show_name_value);
      break;
    case OBJECT_VISIBLE:
      g_value_set_int (value, object->visibility);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, property_id, pspec);
      break;
  }
}

/*! \brief GedaObject property setter function
 *
 *  \par Function Description
 *  Setter function for GedaAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  gobject      The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
geda_object_set_property (GObject *gobject, unsigned int property_id,
                          const    GValue *value, GParamSpec *pspec)
{
  Object *object = GEDA_OBJECT(gobject);
  const char *name;
  switch (property_id)
  {
    case OBJECT_TYPE:
      object->type = g_value_get_schar (value);
      break;
    case OBJECT_NAME:
      name = g_value_get_string (value); //
      object->name = g_strdup_printf("%s.%d", name, object->sid);
      break;
    case OBJECT_PARENT:
      object->parent_object = g_value_get_pointer  (value);
      break;
    case OBJECT_SELECTABLE:
      object->selectable = g_value_get_boolean (value);
      break;
    case OBJECT_NO_REDRAW:
      object->dont_redraw = g_value_get_boolean (value);
      break;
    case OBJECT_SELECTED:
      object->selected = g_value_get_boolean (value);
      break;
    case OBJECT_SHOW_NAME_VALUE:
      object->show_name_value = g_value_get_int (value);
      break;
    case OBJECT_VISIBLE:
      object->visibility = g_value_get_int (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, property_id, pspec);
      break;
  }
}
/* END ----------------------- Property Handlers --------------------------- */

/* Non static but libgeda Private */
int geda_object_get_next_sid(void)
{
  return global_sid++;
}

/*! \brief GType instance initialiser for a Geda Object
 *
 *  \par Function Description
 *  GType instance initialiser for an Object, initializes a new empty
 *  Object by setting pointers to NULL and numbers to zero or default
 *  values, the object SID variable is set to the next Object index.
 *
 *  \param [in]  instance  The Object being initialising.
 *  \param [in]  g_class   The class of the type the instance is created for.
 */
static void geda_object_instance_init(GTypeInstance *instance, void *g_class)
{
  Object *object                 = (Object *)instance;

  /* setup sid */
  object->sid                    = global_sid++;
  object->type                   = 'E';

  /* The name will be set by the constructor */
  object->name                   = NULL;

  /* Not associate with a page, initially */
  object->page                   = NULL;

  /* Setup the bounding box */
  object->top                    = 0;
  object->left                   = 0;
  object->right                  = 0;
  object->bottom                 = 0;

  object->arc                    = NULL;
  object->box                    = NULL;
  object->bus                    = NULL;
  object->circle                 = NULL;
  object->complex                = NULL;
  object->line                   = NULL;
  object->net                    = NULL;
  object->path                   = NULL;
  object->picture                = NULL;
  object->pin                    = NULL;
  object->text                   = NULL;

  object->fill_options           = NULL;
  object->line_options           = NULL;

  object->tiles                  = NULL;

  object->conn_list              = NULL;

  object->parent_object          = NULL;

  /* Setup the color */
  object->color                  = DEFAULT_COLOR_INDEX;
  object->dont_redraw            = FALSE;
  object->selectable             = TRUE;
  object->selected               = FALSE;
  object->locked_color           = LOCK_COLOR;

  object->attribs                = NULL;
  object->attached_to            = NULL;
  object->copied_to              = NULL;
  object->show_name_value        = SHOW_NAME_VALUE;
  object->visibility             = VISIBLE;

  object->attrib_notify_freeze_count = 0;
  object->attrib_notify_pending      = 0;

  object->conn_notify_freeze_count   = 0;
  object->conn_notify_pending        = 0;

  object->weak_refs                  = NULL;

  object->head_marker                = GEDA_TYPE_OBJECT;
  object->tail_marker                = object->head_marker;

  /* Call hooks */
  g_list_foreach (new_object_hooks, call_new_object_hook, object);

}

/*! \brief Geda Object Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references
 *   and releases the memory allocated to the given Object
 *   data structure and then chain up to the parent's finalize
 *   handler.
 */
static void geda_object_finalize(GObject *gobject)
{
  Object *object = GEDA_OBJECT(gobject);
  GList *iter;

  if (object->name)
    GEDA_FREE(object->name);

  /* Should already be done */
  if (object->conn_list) {
    g_list_free(object->conn_list);
    object->conn_list = NULL;
  }

  if (object->tiles) {
    g_list_free(object->tiles);
    object->tiles = NULL;
  }

  if (object->weak_refs) {

    for (iter = object->weak_refs; iter != NULL; iter = g_list_next (iter)) {
      g_free (iter->data);
    }
    g_list_free (object->weak_refs);
  }
  object->weak_refs = NULL;

  G_OBJECT_CLASS( geda_object_parent_class )->finalize(gobject);
}

/*! \brief GType class initialiser for Object
 *
 *  \par Function Description
 *  GType class initialiser for Object. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class       The Object we are initialising
 *  \param [in]  class_data    (unused)
 */
static void geda_object_class_init(void *g_class, void *class_data)
{
  GParamSpec  *params;

  ObjectClass *class          = GEDA_OBJECT_CLASS( g_class );

  GObjectClass *gobject_class = G_OBJECT_CLASS( class );

  class->bounds               = geda_object_no_bounds;
  class->finalize             = geda_object_finalize;

  gobject_class->finalize     = class->finalize;
  gobject_class->set_property = geda_object_set_property;
  gobject_class->get_property = geda_object_get_property;

  geda_object_parent_class    = g_type_class_peek_parent( class );

  params = g_param_spec_char   ("type",
                              _("object-type"),
                              _("Character identifier corresponding to the type of object"),
                                 ASCII_CAPITAL_LETTER_A,
                                 ASCII_CAPITAL_LETTER_Z,
                                 ASCII_CAPITAL_LETTER_T,
                                (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

  g_object_class_install_property( gobject_class, OBJECT_TYPE, params);

  params = g_param_spec_int    ("sid",
                              _("sequence-identification"),
                              _("Sequence identifier created during object construction"),
                                 0, 0, 0,
                                 G_PARAM_READABLE);

  g_object_class_install_property( gobject_class, OBJECT_ID, params);

  params = g_param_spec_string ("name",
                              _("object-name"),
                              _("Unique name of this object"),
                                 NULL,
                                (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

  g_object_class_install_property( gobject_class, OBJECT_NAME, params);

  params = g_param_spec_pointer ("parent", _("Parent"),
                               _("Pointer to parent object - a Page or a Complex"),
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, OBJECT_PARENT, params);

  params = g_param_spec_boolean ("selectable", _("Selectable"),
                               _("Whether the label text can be selected with the mouse"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, OBJECT_SELECTABLE, params);

  params = g_param_spec_boolean ("redraw", _("Redraw"),
                               _("Whether the object redrawn, used for temporary objects"),
                                  TRUE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, OBJECT_NO_REDRAW, params);

  params = g_param_spec_boolean ("selected", _("Selected"),
                               _("Whether the object is currently selected"),
                                  TRUE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, OBJECT_SELECTED, params);

  params = g_param_spec_int    ("show-name-value",
                              _("Show-Name-Value-flag"),
                              _("Flag to indicate whether to show the name, value or both"),
                                 LEAVE_NAME_VALUE_ALONE,
                                 SHOW_NAME,
                                 SHOW_VALUE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, OBJECT_SHOW_NAME_VALUE, params);

  params = g_param_spec_int    ("visible", _("Visible"),
                               _("Whether the object is displayed, normaly text attributes"),
                                 -1,
                                  3,
                                 VISIBLE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, OBJECT_VISIBLE, params);
}

/*! \brief Function to retrieve Object's GType identifier.
 *
 *  \par Function Description
 *  Function to retrieve Object's type identifier.
 *  Upon first call, this registers the Object in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return the GType identifier associated with Object.
 */

unsigned int geda_gobject_get_type(void)
{
  static unsigned int type = 0;
  if (type == 0) {
    static const GTypeInfo info = {
      sizeof (ObjectClass),
      NULL,                            // base_init
      NULL,                            // base_finalize
      geda_object_class_init,          // class_init
      NULL,                            // class_finalize
      NULL,                            // class_data
      sizeof (Object),
      0,                               // n_preallocs
      geda_object_instance_init        // instance_init
    };
    type = g_type_register_static (G_TYPE_OBJECT, "Object", &info, 0);
  }
  return type;
}

/*! \brief Create a new Object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Object object.
 *
 *  \param [in] type      The object type; one of the OBJ_* constants.
 *  \param [in] name      A prefix for the object's session-unique name.
 *
 *  \return A pointer to the initialized object.
 */
Object *geda_object_new (int type, char const *name)
{
  Object *object = g_object_new( GEDA_TYPE_OBJECT,
                                "type", type,
                                "name", name,
                                 NULL);
  return object;
}

/*! \brief Determine if object is a Geda Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Object.
 *  All of the graphical and connection type Geda objects
 *  such as Circle, Lines, Pins, ... etc, are derived from
 *  the GedaObject base class. This function is intended
 *  to be used via the \a Macro #GEDA_IS_OBJECT, but only
 *  for convention, really makes no difference. Both of the
 *  following example produce exactly the same code:
 *
 *  example   if (is_a_geda_object(MyComplex))
 *
 *  example   if (GEDA_IS_OBJECT(MyComplex)) 
 *
 *  \return boolean.
 */
bool is_a_geda_object (Object *obj)
{
  return obj && (GEDA_TYPE_OBJECT == (obj->head_marker & obj->tail_marker));
}

/*! \todo Finish function documentation!!!
 *  \brief Decrement the GObject Reference Count
 *  \par Function Description
 *
 */
void geda_object_unref(Object *object)
{
  g_return_if_fail (GEDA_IS_OBJECT(object));

  g_object_unref ((GObject*)object);

}

/*! \brief Notify weak reference watchers that a structure is dead.
 * \par Function Description
 * For each entry in \a weak_refs, call notify function with the dead
 * pointer \a dead_ptr and the entry's specified user data, and free
 * \a weak_refs. Should be called during destruction of an structure
 * that allows weak references.
 *
 * \param [in] object  Pointer to Object being destroyed.
 *
 */
void
geda_object_weakref_notify (Object *object)
{
  if (GEDA_IS_OBJECT(object)) {
    s_weakref_notify(object, object->weak_refs);
    object->weak_refs = NULL;
  }
}

/*! \brief Add a weak reference watcher to an Object
 *
 *  \par Function Description
 *
 *   Adds the weak reference callback \a notify_func to \a Object.
 * When \a Object is destroyed, the \a notify_func will be called
 * with two arguments: the \a Object, and the \a user_data.
 *
 * \note This function is for legacy purposes; since Object is
 *       now a GObject, just use g_object_weak_ref instead!
 *
 * \sa object_weak_unref
 *
 * \param [in,out] object       Object  to weak-reference.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 */
void geda_object_weak_ref (Object *object, WeakNotifyFunc notify_func, void *user_data)
{
  if (GEDA_IS_OBJECT(object)) {
    object->weak_refs = s_weakref_add (object->weak_refs, notify_func, user_data);
  }
  else
    BUG_MSG("Object is not a GedaObject");
}

/*! \brief Remove a weak reference watcher from a Object.
 * \par Function Description
 * Removes the weak reference callback \a notify_func from \a Object.
 *
 * \sa object_weak_ref()
 *
 * \note This function is for legacy purposes; since Object is
 *       now a GObject, just use g_object_weak_unref instead!
 *
 * \param [in,out] object        Object to remove weak-reference function.
 * \param [in]     notify_func Notify function to search for.
 * \param [in]     user_data   Data to to search for.
 */
void geda_object_weak_unref (Object *object, WeakNotifyFunc notify_func, void *user_data)
{
  if (GEDA_IS_OBJECT(object)) {
    object->weak_refs = s_weakref_remove (object->weak_refs, notify_func, user_data);
  }
  else
    BUG_MSG("Object is not a GedaObject");
}

/*! \brief Add a weak pointer to a Object.
 * \par Function Description
 * Adds the weak pointer at \a weak_pointer_loc to \a object. The
 * value of \a weak_pointer_loc will be set to NULL when \a object is
 * destroyed.
 *
 * \sa object_remove_weak_ptr
 *
 * \param [in,out] object          Object to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void geda_object_add_weak_ptr (Object *object, void *weak_pointer_loc)
{
  g_return_if_fail (GEDA_IS_OBJECT(object));
  g_object_add_weak_pointer ((GObject*)object, weak_pointer_loc);
}

/*! \brief Remove a weak pointer from an Object.
 * \par Function Description
 * Removes the weak pointer at \a weak_pointer_loc from \a object.
 *
 * \sa object_add_weak_ptr()
 *
 * \param [in,out] object          Object to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void geda_object_remove_weak_ptr (Object *object, void *weak_pointer_loc)
{
  g_return_if_fail (GEDA_IS_OBJECT(object));
  g_object_remove_weak_pointer ((GObject*)object, weak_pointer_loc);
}

/*! \brief Get an object's parent Page.
 *
 * \par Function Description
 * Returns the Page structure which owns \a object. If \a object is
 * not currently associated with a Page, returns NULL. If \a object is
 * part of a compound object, recurses upward.
 *
 * \param [in] object    The Object for which to retrieve the parent Page.
 *
 * \return The Page which owns \a object or NULL.
 *
 * \sa s_page_append_object() s_page_append() s_page_remove()
 */
Page *geda_object_get_page (Object *object)
{
  if(GEDA_IS_OBJECT(object)) {

    if (GEDA_IS_PAGE(object->page)) {
      return object->page;
    }

    while (GEDA_IS_OBJECT(object->parent_object)) {
      object = object->parent_object;
      if (GEDA_IS_PAGE(object->page)) {
        return object->page;
      }
    }
  }
  else {
    BUG_MSG("Is not a GedaObject");
  }

  return NULL;
}