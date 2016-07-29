
/*! \file geda_menu_separator.c
 *  \brief GedaMenuSeparator Class Module
 */

/** \defgroup geda-menu-separator GedaMenuSeparator Object
 * @{
 * \brief Implmentation of GedaMenuSeparator Class
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

static void *geda_menu_separator_parent_class = NULL;

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
  GTK_CONTAINER_CLASS (klass)->child_type = NULL;
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
  GedaMenuSeparator *item = (GedaMenuSeparator*)instance;
  item->instance_type     = geda_menu_separator_get_type();
}

/*! \brief Retrieve GedaMenuSeparator's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaMenuSeparator Type identifier. When
 *  first called, the function registers a #GedaMenuSeparator in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaMenuSeparator and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaMenuSeparator.
 */
GedaType
geda_menu_separator_get_type (void)
{
  static GedaType geda_menu_separator_type = 0;

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

GtkWidget *
geda_menu_separator_new (void)
{
  return g_object_new (GEDA_TYPE_MENU_SEPARATOR, NULL);
}

/** @} geda-menu-separator */
