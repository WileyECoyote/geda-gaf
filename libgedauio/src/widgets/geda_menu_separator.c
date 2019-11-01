
/*! \file geda_menu_separator.c
 *  \brief GedaMenuSeparator Class Module
 */

/** \defgroup geda-menu-separator GedaMenuSeparator Object
 * @{
 * \brief Implmentation of GedaMenuSeparator Class
 *  A GedaMenuSeparator is non-selectable GedaMenuItem with no event window or
 *  submenu widget, accel path, mnemonic, or label. Is always left justified
 *  and cannot be on a menubar.
 *
 * \class GedaMenuSeparator geda_menu_separator.h "include/geda_menu_separator.h"
 * \implements GedaMenuItem
 */

#include "../../../config.h"

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_separator.h"

/* Table of pointers to GedaMenuSeparator instances */
static GHashTable *separator_hash_table = NULL;

static void *geda_menu_separator_parent_class = NULL;

/*!
 * \brief gobject_class->finalize a GedaMenuSeparator object
 * \par Function Description
 *  Releases resources associated with the GedaMenuSeparator object.
 *  The object should not be referenced after this function
 *  is executes.
 */
static void
geda_menu_separator_finalize (GObject *object)
{
  if (g_hash_table_remove (separator_hash_table, object)) {
    if (!g_hash_table_size (separator_hash_table)) {
      g_hash_table_destroy (separator_hash_table);
      separator_hash_table = NULL;
    }
  }

  G_OBJECT_CLASS (geda_menu_separator_parent_class)->finalize (object);
}

/*!
 * \brief GedaMenuSeparator Class Initializer
 * \par Function Description
 *  Function is called to initialize the class instance.
 *
 * \param [in] klass  GedaMenuSeparatorClass Object
 * \param [in] data   GedaMenuSeparator structure associated with the class
 */
static void
geda_menu_separator_class_init (void *klass, void *data)
{
  GObjectClass       *object_class;
  GtkContainerClass  *container_class;

  object_class    = (GObjectClass*)klass;
  container_class = (GtkContainerClass*)klass;

  object_class->finalize = geda_menu_separator_finalize;

  container_class->child_type = NULL;

  geda_menu_separator_parent_class = g_type_class_peek_parent(klass);
}

/*!
 * \brief Type instance initializer for GedaMenuSeparator
 * \par Function Description
 *  Type instance initializer for GedaMenuSeparator, initializes a new empty
 *  GedaMenuSeparator object.
 *
 * \param [in] instance The GedaMenuSeparator structure being initialized,
 * \param [in] class    The GedaMenuSeparator class being initializing.
 */
static void
geda_menu_separator_instance_init(GTypeInstance *instance, void *class)
{
  if (!separator_hash_table) {
    separator_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (separator_hash_table, instance, instance);
}

/*!
 * \brief Retrieve GedaMenuSeparator's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaMenuSeparator Type identifier. When
 *  first called, the function registers a #GedaMenuSeparator in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaMenuSeparator and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaMenuSeparator.
 */
GedaType
geda_menu_separator_get_type (void)
{
  static volatile GedaType geda_menu_separator_type = 0;

  if (g_once_init_enter (&geda_menu_separator_type)) {

    static const GTypeInfo info = {
      sizeof(GedaMenuSeparatorClass),
      NULL,                             /* base_init           */
      NULL,                             /* base_finalize       */
      geda_menu_separator_class_init,   /* (GClassInitFunc)    */
      NULL,                             /* class_finalize      */
      NULL,                             /* class_data          */
      sizeof(GedaMenuSeparator),
      0,                                /* n_preallocs         */
      geda_menu_separator_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaMenuSeparator");
    type   = g_type_register_static (GEDA_TYPE_MENU_ITEM, string, &info, 0);

    g_once_init_leave (&geda_menu_separator_type, type);
  }

  return geda_menu_separator_type;
}

/*!
 * \brief Check if an object is a GedaMenuSeparator
 * \par Function Description
 *  Determines if \a separator is valid by verifying \a separator
 *  is included in the hash table of GedaMenuSeparator objects.
 *
 * \returns TRUE if \a separator is a valid GedaMenuSeparator
 */
bool
is_a_geda_menu_separator (GedaMenuSeparator *separator)
{
  if ((separator != NULL) && (separator_hash_table != NULL)) {
    return g_hash_table_lookup(separator_hash_table, separator) ? TRUE : FALSE;
  }
  return FALSE;
}

GtkWidget *
geda_menu_separator_new (void)
{
  return g_object_new (GEDA_TYPE_MENU_SEPARATOR, NULL);
}

/** @} geda-menu-separator */
