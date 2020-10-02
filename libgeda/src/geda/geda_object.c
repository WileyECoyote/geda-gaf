/* -*- geda_object.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 4, 2013
 */
/*! \file geda_object.c
 *  \brief GedaObject Base Class for all Geda Object types
 */

/** \defgroup geda-object GedaObject Base Class
 * @{
 * \brief Implmentation of #GedaObject Class
 * \par
 *  The Geda GedaObject base class provides the foundation for all Geda
 *  objects. Information such as page, object hierarchy, hooks and
 *  geometric bounds are stored in the base GedaObject class. GedaObject
 *  are derived from the base GObject Class
 *
 * \class GedaObject geda_object.h "include/libgeda/geda_object.h"
 */

#include "../../../config.h"

#include <libgeda_priv.h>

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

/*! Holds list of pointers to GedaObject instances */
static GHashTable *object_hash_table = NULL;

/*! Global integer for GedaObject Indentification */
static int global_sid = 0;

static GList *new_object_hooks = NULL;

typedef struct {
  NewObjectFunc func;
  void *data;
} NewGedaObjectHook;


/*!
 * \brief Internal Function to Call Register GedaObject Hooks
 * \par Function Description
 *
 */
static void call_new_object_hook (void *hook, void *object)
{
  NewGedaObjectHook *h = (NewGedaObjectHook*) hook;
  GedaObject *o = (GedaObject*) object;

  h->func (o, h->data);
}

/*!
 * \brief Append New GedaObject Hook List to this GedaObject.
 * \par Function Description
 *  Adds a callback hook \a notify_func to \a object. After a new
 *  \a object is created, \a notify_func will be called with two
 *  arguments: the \a object, and the \a user_data.
 *
 * \sa object_weak_unref
 *
 * \param [in] func      notify function.
 * \param [in] data      Data to be passed to \a notify_func
 *
 */
void geda_object_append_new_hook (NewObjectFunc func, void *data)
{
  NewGedaObjectHook *new_hook;

  new_hook = GEDA_MEM_ALLOC0 (sizeof(NewGedaObjectHook));
  new_hook->func = func;
  new_hook->data = data;

  new_object_hooks = g_list_append (new_object_hooks, new_hook);
}

/* BEGIN ------+-------+------ Property Handlers ------+-------+-------+-----*/

/*!
 * \internal Default object Bounds Function
 * \par Function Description
 *  When new GedaObjects are created this function is assigned to the
 *  virtual object_class->bounds method. Derivatives should over-ride
 *  this method during construction.
 */
static int geda_object_no_bounds (GedaObject *o)
{
  fprintf(stderr, "ERROR: <%s> bounds function not set <%s><%p>\n", __func__, o->name, o);
  return FALSE;
}

/*!
 * \brief
 * \par Function Description
 *  Calls the virtual method to return the actual bounds of the
 *  derived object. The method must have been over-ridden or the
 *  default method will output an error message.
 */
int geda_object_bounds(const GedaObject *object)
{
  GedaObjectClass *object_class = (GedaObjectClass*)G_OBJECT_GET_CLASS(object);

  return object_class ? object_class->bounds((GedaObject*)object) : 0;
}

/*!
 * \brief Get List of Attributes Attached to GedaObject
 * \par Function Description
 *  Returns the attribs list associated with the given object.
 *
 * \param [in]  object The GedaObject from which to get the attribute list.
 *
 * \return List of attached attributes.
 */
const GList *geda_object_get_attached (const GedaObject *object)
{
  if (is_a_geda_object(object)) {
    return object->attribs;
  }
  return NULL;
}

/*!
 * \brief Get GedaObject that a floating GedaObject is Attached to
 * \par Function Description
 *  Returns the parent associated with the given object if there
 *  is one. If \a object is not a floating object then the object
 *  could still be attached to a complex.
 *
 * \param [in] object The GedaObject from which to get the parent.
 *
 * \return GedaObject or NULL.
 */
GedaObject *geda_object_get_attached_to (const GedaObject *object)
{
  if (is_a_geda_object(object)) {
    return object->attached_to;
  }
  return NULL;
}

/*!
 * \brief Retrieve the Value of GedaObject Bounds Valid
 * \par Function Description
 *  Returns the value of the object->bounds_valid member.
 */
int geda_object_get_bounds_valid (const GedaObject *object) {
  if (is_a_geda_object(object)) {
    return object->bounds_valid;
  }
  return -0;
}

/*!
 * \brief Retrieve the Value of the GedaObject Color Index
 * \par Function Description
 *  Returns the value of the object->color member, this is the
 *  color index not the color value. The index refers to the
 *  index of a color in a color-map and is only relevant when
 *  rendering to a device such as the display or a printer.
 */
int geda_object_get_color (const GedaObject *object) {
  if (is_a_geda_object(object)) {
    return object->color;
  }
  return -0;
}

/*!
 * \brief Retrieve the Connection List from a GedaObject
 * \par Function Description
 *  The connection list is a double linked list of other objects
 *  electrically connected to this object. Hence, the connection
 *  only pertains to pin, nets, and buses.
 */
const GList *geda_object_get_conn_list (const GedaObject *object)
{
  if (is_a_geda_object(object)) {
    return object->conn_list;
  }
  return NULL;
}

/*!
 * \brief Retrieve the Value of the GedaObject Locked Color Index
 * \par Function Description
 *  Returns the value of the object->locked_color member, which holds the
 *  index of the locked color value, not the color value. The color index
 *  of the object is saved to the locked_color member when the object is
 *  locked so that the previous color can be restored when the object is
 *  unlocked.
 *
 * \sa geda_object_get_color
 */
int geda_object_get_locked_color (const GedaObject *object) {
  if (is_a_geda_object(object)) {
    return object->locked_color;
  }
  return -0;
}

/*!
 * \brief Get an object's parent Page.
 * \par Function Description
 *  Returns the Page structure which owns \a object. If \a object is
 *  not currently associated with a Page, returns NULL. If \a object is
 *  part of a compound object, recurses upward.
 *
 * \param [in] object    The GedaObject for which to retrieve the parent Page.
 *
 * \return The Page which owns \a object or NULL.
 *
 * \sa geda_struct_page_append_object() geda_struct_page_append() geda_struct_page_remove()
 */
Page *geda_object_get_page (const GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {

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

/*!
 * \brief Get the Selectable Flag from a GedaObject
 * \par Function Description
 *  Returns the selectable member of \a object.
 *
 * \sa geda_object_set_selectable
 */
bool geda_object_get_selectable (ConstObject *object) {
  if (is_a_geda_object(object)) {
    return object->selectable;
  }
  return FALSE;
}

/*!
 * \brief Get the Show Name Value setting from a GedaObject
 * \par Function Description
 *  Returns the show_name_value member of \a object.
 *
 * \sa geda_object_set_show_name_value
 */
int geda_object_get_show_name_value (ConstObject *object) {
  if (is_a_geda_object(object)) {
    return object->show_name_value;
  }
  return 0;
}

/*!
 * \brief Get the Visibility Flag from a GedaObject
 * \par Function Description
 *  Returns the visibility member of \a object.
 *
 * \sa geda_object_set_visibility
 */
int geda_object_get_visibility (ConstObject *object) {
  if (is_a_geda_object(object)) {
    return object->visibility;
  }
  return 0;
}

/*!
 * \brief GedaObject property getter function
 * \par Function Description
 *  Getter function for GedaObject's properties,
 *
 * \param [in]  gobject      The GedaObject whose properties we are getting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [out] value        The GValue in which to return the value of the property
 * \param [in]  pspec        A GParamSpec describing the property being got
 */
static void geda_object_get_property (GObject *gobject, unsigned int property_id,
                                      GValue  *value,   GParamSpec  *pspec)
{
  GedaObject *object = GEDA_OBJECT(gobject);

  switch (property_id)
  {
    case OBJECT_TYPE:
#if (( GLIB_MAJOR_VERSION <= 2 ) && ( GLIB_MINOR_VERSION <= 31 ))
      g_value_set_char (value, object->type);
#else
      g_value_set_schar (value, object->type);
#endif
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

/*!
 * \brief GedaObject property setter function
 * \par Function Description
 *  Setter function for GedaAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 * \param [in]  gobject      The GObject whose properties we are setting
 * \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 * \param [in]  value        The GValue the property is being set from
 * \param [in]  pspec        A GParamSpec describing the property being set
 */
static void geda_object_set_property (GObject *gobject, unsigned int property_id,
                                      const    GValue *value, GParamSpec *pspec)
{
  GedaObject *object = GEDA_OBJECT(gobject);
  const char *name;

  switch (property_id)
  {
    case OBJECT_TYPE:

#if (( GLIB_MAJOR_VERSION <= 2 ) && ( GLIB_MINOR_VERSION <= 31 ))
      object->type = g_value_get_char (value);
#else
      object->type = g_value_get_schar (value);
#endif
      break;

    case OBJECT_NAME:
      name = g_value_get_string (value); //
      object->name = geda_sprintf("%s.%d", name, object->sid);
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

/*!
 * \brief Retrieve Next Serial Identifier (SID)
 * \par Function Description
 *  Non static but libgeda Private. Each Libgeda object holds a session
 *  unique integer identifier that is assigned when the object is created.
 *  This function generates the identifier by incrementing and returning
 *  the global_sid variable static to this module.
 */
int geda_object_get_next_sid(void)
{
  return global_sid++;
}

/*!
 * \brief GedaType instance initializer for a Geda GedaObject
 * \par Function Description
 *  GedaType instance initializer for an GedaObject, initializes a new empty
 *  GedaObject by setting pointers to NULL and numbers to zero or default
 *  values, the object SID variable is set to the next GedaObject index.
 *
 * \param [in]  instance  The GedaObject being initializing.
 * \param [in]  g_class   The class of the type the instance is created for.
 */
static void geda_object_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaObject *object             = (GedaObject*)instance;

  /* setup sid */
  object->sid                    = geda_object_get_next_sid();
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

  if (!object_hash_table) {
    object_hash_table = g_hash_table_new (NULL, NULL);
  }

  g_hash_table_replace(object_hash_table, instance, instance);

  /* Call hooks */
  g_list_foreach (new_object_hooks, call_new_object_hook, object);
}

static void geda_object_dispose(GObject *gobject)
{
  GedaObject *object = (GedaObject*)(gobject);

  if (object->attribs) {
    g_list_free(object->attribs);
    object->attribs = NULL;
  }

  ((GObjectClass*)geda_object_parent_class)->dispose(gobject);
}

/*!
 * \brief Geda GedaObject Finalization Function
 * \par Function Description
 *  This function removes or releases all internal references
 *  and releases the memory allocated to the given GedaObject data
 *  structure and then chain up to the parent's finalize handler.
 */
static void geda_object_finalize(GObject *gobject)
{
  GedaObject *object = (GedaObject*)(gobject);

  if (object->name) {
    GEDA_FREE(object->name);
  }

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
     geda_object_weakref_notify(object);
  }

  if (g_hash_table_remove (object_hash_table, object)) {
    if (!g_hash_table_size (object_hash_table)) {
      g_hash_table_destroy (object_hash_table);
      object_hash_table = NULL;
    }
  }

  G_OBJECT_CLASS(geda_object_parent_class)->finalize(gobject);

  /* Return to the child's finalizer */
}

/*!
 * \brief GedaObjectType class initializer for GedaObject
 * \par Function Description
 *  GedaObjectType class initializer for #GedaObject, registers
 *  GObject signals and over-rides parent virtual class methods
 *  as needed.
 *
 * \param [in] klass      The GedaObject we are initializing
 * \param [in] class_data (unused)
 */
static void geda_object_class_init(void *klass, void *class_data)
{
  GParamSpec      *params;

  GedaObjectClass *class      = (GedaObjectClass*)klass;
  GObjectClass    *gclass     = (GObjectClass*)klass;

  class->bounds               = geda_object_no_bounds;
  class->finalize             = geda_object_finalize;

  gclass->finalize            = class->finalize;
  gclass->dispose             = geda_object_dispose;
  gclass->set_property        = geda_object_set_property;
  gclass->get_property        = geda_object_get_property;

  geda_object_parent_class    = g_type_class_peek_parent(class);

  params = g_param_spec_char   ("type",
                              _("object-type"),
                              _("Character identifier corresponding to the type of object"),
                                 ASCII_CAPITAL_LETTER_A,
                                 ASCII_CAPITAL_LETTER_Z,
                                 ASCII_CAPITAL_LETTER_T,
                                (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

  g_object_class_install_property( gclass, OBJECT_TYPE, params);

  params = g_param_spec_int    ("sid",
                              _("sequence-identification"),
                              _("Sequence identifier created during object construction"),
                                 0, 0, 0,
                                 G_PARAM_READABLE);

  g_object_class_install_property( gclass, OBJECT_ID, params);

  params = g_param_spec_string ("name",
                              _("object-name"),
                              _("Unique name of this object"),
                                 NULL,
                                (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

  g_object_class_install_property(gclass, OBJECT_NAME, params);

  params = g_param_spec_pointer ("parent", _("Parent"),
                               _("Pointer to parent object - a Page or a Complex"),
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gclass, OBJECT_PARENT, params);

  params = g_param_spec_boolean ("selectable", _("Selectable"),
                               _("Whether the label text can be selected with the mouse"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gclass, OBJECT_SELECTABLE, params);

  params = g_param_spec_boolean ("redraw", _("Redraw"),
                               _("Whether the object redrawn, used for temporary objects"),
                                  TRUE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gclass, OBJECT_NO_REDRAW, params);

  params = g_param_spec_boolean ("selected", _("Selected"),
                               _("Whether the object is currently selected"),
                                  TRUE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gclass, OBJECT_SELECTED, params);

  params = g_param_spec_int    ("show-name-value",
                              _("Show-Name-Value-flag"),
                              _("Flag to indicate whether to show the name, value or both"),
                                 LEAVE_NAME_VALUE_ALONE,
                                 SHOW_NAME,
                                 SHOW_VALUE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gclass, OBJECT_SHOW_NAME_VALUE, params);

  params = g_param_spec_int    ("visible", _("Visible"),
                               _("Whether the object is displayed, normaly text attributes"),
                                 -1,
                                  3,
                                 VISIBLE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gclass, OBJECT_VISIBLE, params);
}

/*!
 * \brief Function to retrieve GedaObject's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaObject Type identifier. When first called,
 *  the function registers a #GedaObject in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a GedaObject and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 * \return GedaObjectType identifier associated with GedaObject.
 */
GedaObjectType geda_object_get_type (void)
{
  static volatile GedaObjectType geda_object_type = 0;

  if (g_once_init_enter (&geda_object_type)) {

    static const GTypeInfo info = {
      sizeof(GedaObjectClass),
      NULL,                          /* base_init           */
      NULL,                          /* base_finalize       */
      geda_object_class_init,        /* (GClassInitFunc)    */
      NULL,                          /* class_finalize      */
      NULL,                          /* class_data          */
      sizeof(GedaObject),
      0,                             /* n_preallocs         */
      geda_object_instance_init      /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaObject");
    type   = g_type_register_static (G_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_object_type, type);
  }

  return geda_object_type;
}

/*!
 * \brief Create a new GedaObject.
 * \par Function Description
 *  GedaObject objects are generally not created directly, this function
 *  creates an object of the type specified by \a type, which results in
 *  the creation of a GedaObject base object. An alternative would be to
 *  just call the "new" constructor of the desired type.
 *
 * \param [in] type  The object type; one of the OBJ_* constants.
 *
 * \return A pointer to a new initialized object.
 */
GedaObject *geda_object_new (int type)
{
  GedaObject *object;

  switch (type) {
    case OBJ_ARC:
      object = geda_arc_new();
      break;

    case OBJ_BOX:
      object = geda_box_new();
      break;

    case OBJ_BUS:
      object = geda_bus_new();
      break;

    case OBJ_CIRCLE:
      object = geda_circle_new();
      break;

    case OBJ_COMPLEX:
      object = geda_complex_new();
      break;

    case OBJ_LINE:
      object = geda_line_new();
      break;

    case OBJ_NET:
      object = geda_net_new();
      break;

    case OBJ_PATH:
      object = geda_path_new();
      break;

    case OBJ_PICTURE:
      object = geda_picture_new();
      break;

    case OBJ_PIN:
      object = geda_pin_new();
      break;

    case OBJ_TEXT:
      object = geda_text_new();
      break;

    default:
      object = NULL;
  }

  return object;
}

/*!
 * \brief Determine if object is a Geda GedaObject.
 * \par Function Description
 *  Returns true if the argument is a Geda GedaObject.
 *  All of the graphical and connection type Geda objects
 *  such as Circle, Lines, Pins, ... etc, are derived from
 *  the GedaObject base class. This function is intended
 *  to be used via the \a Macro #GEDA_IS_OBJECT, but only
 *  for convention, really makes no difference. Both of the
 *  following examples produce exactly the same code:
 *
 *  example   if (is_a_geda_object(MyComplex))
 *
 *  example   if (GEDA_IS_OBJECT(MyComplex))
 *
 * \return boolean.
 */
bool is_a_geda_object (const void *object)
{
  if ((object != NULL) && (object_hash_table != NULL)) {
    return g_hash_table_lookup (object_hash_table, object) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Increment the GedaObject Reference Count
 * \par Function Description
 *  Increases the GedaObject's reference count by one.
 *
 * \param [in,out] object Pointer to a GedaObject.
 *
 * \returns void pointer to a GedaObject
 *
 * \sa geda_object_unref
 */
GedaObject *geda_object_ref(GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_OBJECT(object), NULL);
  return g_object_ref (object);
}

/*!
 * \brief Decrement the GedaObject Reference Count
 * \par Function Description
 *  Decreases the GedaObject's reference count.
 *
 * \param [in] object  Pointer to a GedaObject.
 *
 * \sa geda_object_ref
 */
void geda_object_unref(GedaObject *object)
{
  g_return_if_fail (GEDA_IS_OBJECT(object));
  g_object_unref (object);
}

/*!
 * \brief Notify weak reference watchers that a structure is dead.
 * \par Function Description
 *  For each entry in \a weak_refs, call notify function. This
 *  function will be called during destruction of the GedaObject.
 *
 * \param [in] object  Pointer to GedaObject being destroyed.
 */
void geda_object_weakref_notify (GedaObject *object)
{
  if (GEDA_IS_OBJECT(object)) {
    s_weakref_notify(object, object->weak_refs);
    object->weak_refs = NULL;
  }
}

/*!
 * \brief Add a weak reference watcher to an GedaObject
 * \par Function Description
 *  Adds the weak reference callback \a notify_func to \a GedaObject.
 *  When \a GedaObject is destroyed, the \a notify_func will be called
 *  with two arguments: the \a GedaObject, and the \a user_data.
 *
 * \sa object_weak_unref
 *
 * \param [in,out] object     GedaObject to weak-reference.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 */
void geda_object_weak_ref (GedaObject *object, WeakNotifyFunc notify_func, void *user_data)
{
  if (GEDA_IS_OBJECT(object)) {
    object->weak_refs = s_weakref_add (object->weak_refs, notify_func, user_data);
  }
  else {
    BUG_MSG("Object is not a GedaObject");
  }
}

/*!
 * \brief Remove a weak reference watcher from a GedaObject.
 * \par Function Description
 *  Removes the weak reference callback \a notify_func from \a GedaObject.
 *
 * \param [in,out] object      GedaObject to remove weak-reference function.
 * \param [in]     notify_func Notify function to search for.
 * \param [in]     user_data   Data to to search for.
 *
 * \sa object_weak_ref()
 */
void geda_object_weak_unref (GedaObject *object, WeakNotifyFunc notify_func, void *user_data)
{
  if (GEDA_IS_OBJECT(object)) {
    object->weak_refs = s_weakref_remove (object->weak_refs, notify_func, user_data);
  }
  else {
    BUG_MSG("Object is not a GedaObject");
  }
}

/*!
 * \brief Add a weak pointer to a GedaObject.
 * \par Function Description
 *  Adds the weak pointer at \a weak_pointer_loc to \a object. The
 *  value of \a weak_pointer_loc will be set to NULL when \a object
 *  is destroyed.
 *
 * \sa object_remove_weak_ptr
 *
 * \param [in,out] object        GedaObject to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void geda_object_add_weak_ptr (GedaObject *object, void *weak_pointer_loc)
{
  g_return_if_fail (GEDA_IS_OBJECT(object));
  object->weak_refs = s_weakref_add_ptr (object->weak_refs, weak_pointer_loc);
}

/*!
 * \brief Remove a weak pointer from an GedaObject.
 * \par Function Description
 *  Removes the weak pointer at \a weak_pointer_loc from \a object.
 *
 * \sa object_add_weak_ptr()
 *
 * \param [in,out] object        GedaObject to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void geda_object_remove_weak_ptr (GedaObject *object, void *weak_pointer_loc)
{
  g_return_if_fail (GEDA_IS_OBJECT(object));
  object->weak_refs = s_weakref_remove_ptr (object->weak_refs,
                                            weak_pointer_loc);
}

/*!
 * \brief Set the Bounds Valid Property of a GedaObject
 * \par Function Description
 *  Sets the bounds_valid to value of \a valid argument if \a object
 *  is valid.
 */
void geda_object_set_bounds_valid (GedaObject *object, int valid) {
  if (is_a_geda_object(object)) {
    object->bounds_valid = valid;
  }
}

/*!
 * \brief Set the Color Index of a GedaObject
 * \par Function Description
 *  The actual color used to render the object is depends on the color-map
 *  used by the program performing the rendering. Setting the color of a
 *  Complex object has no effect on the color of the compositing elements.
 */
void geda_object_set_color (GedaObject *object, int index) {
  if (is_a_geda_object(object)) {
    object->color = index;
  }
}

/*!
 * \brief Set the Locked Color Index of a GedaObject
 * \par Function Description
 *  The locked color can be used to store the index of the color of an enity
 *  when the objected is locked. This value can later be restored if and when
 *  the object is unlocked. Setting the locked color of a Complex object has
 *  no effect.
 */
void geda_object_set_locked_color (GedaObject *object, int index) {
  if (is_a_geda_object(object)) {
    if (index > BACKGROUND_COLOR) {
      object->locked_color = index;
    }
    else {
      BUG_IMSG("Refusing to set color index to value", index);
    }
  }
}

/*!
 * \brief Set the Page a GedaObject is on
 * \par Function Description
 *  Set the page member property, this is a low-level function
 *  that does not modify the page list.
 *
 * \sa geda_page_add_object.
 */
void geda_object_set_page (GedaObject *object, Page *page) {
  if (is_a_geda_object(object)) {
    object->page = GEDA_IS_PAGE(page) ? page : NULL;
  }
}

/*!
 * \brief Set the Selectable Flag of a GedaObject
 * \par Function Description
 *  The selectable flag is sometimes referred to as the "lock" flag but
 *  really is a NOT locked flag. When the selectable flag is set FALSE
 *  the object is NOT selectable, if the flag is not FALSE then an object
 *  can be selected.
 */
void geda_object_set_selectable (GedaObject *object, int state) {
  if (is_a_geda_object(object)) {
    object->selectable = state;
  }
}

/*!
 * \brief Set the Show Name Value property a GedaObject
 * \par Function Description
 *  Set the show_name_value member, this is a low-level function
 *  that does not check the snv argument.
 *
 * \sa geda_object_get_show_name_value
 */
void geda_object_set_show_name_value (GedaObject *object, int snv) {
  if (is_a_geda_object(object) && (snv >= 0)) {
    object->show_name_value = snv;
  }
}

/*!
 * \brief Set the visibility property a GedaObject
 * \par Function Description
 *  Set the visibility member property, this is a low-level function
 *  that does not check the visible argument.
 *
 * \sa geda_object_get_visibility
 */
void geda_object_set_visibility (GedaObject *object, int visible) {
  if (is_a_geda_object(object)) {
    object->visibility = visible;
  }
}

/** @} endgroup geda-object */
