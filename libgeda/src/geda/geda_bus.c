/* -*- geda_bus.c -*-
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 18, 2013
 */
/*! \file geda_bus.c
 *  \brief Geda Bus Object Class derived from the GedaObject Class
 */
/** \defgroup geda-bus-object Geda Bus Object
 *  @{
 */
/*! \class GedaBus geda_bus.h "include/libgeda/geda_bus.h"
 *  \implements geda-object
 *  \brief This is an implementaion class for GEDA Bus Objects.
 *  A Geda Bus Object is similar to net objects but are intended to
 *  represent multiple conductors.
 */

#include <config.h>

#include <libgeda_priv.h>

static GObjectClass *geda_bus_parent_class = NULL;

/*! \brief Type instance initializer for GedaBus
 *
 *  \par Function Description
 *  Type instance initializer for GedaBus, initializes a new empty
 *  GedaBus object by setting pointers to NULL and numbers to zero.
 *
 *  \param [in] instance The Bus structure being initialized,
 *  \param [in] g_class  The Bus class we are initializing.
 */
static void geda_bus_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaBus    *bus            = (GedaBus*)instance;
  Line       *line           = &bus->parent_instance;
  GedaObject *object         = &line->parent_instance;

  bus->bus_ripper_direction  = 0;

  bus->bus_name              = NULL;

  object->bus                = bus;
  bus->line_width            = &line->line_options.line_width;
}

static void
geda_bus_dispose(GObject *object)
{

  G_OBJECT_CLASS(geda_bus_parent_class)->dispose(object);

}

/*! \brief GedaBus GedaObject Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references and
 *   releases the memory allocated to the given GedaBus data structure,
 *   then chain up to the parent's finalize handler after.
 */
static void geda_bus_finalize(GObject *object)
{
  GedaBus    *bus = GEDA_BUS(object);
  GedaObject *obj = GEDA_OBJECT(object);

  if (bus->bus_name)
    GEDA_FREE(bus->bus_name);

  /* The object is no longer a GedaBus */
  obj->bus = NULL;

  /* Finialize the parent GedaLine Class */
  GEDA_LINE_CLASS(geda_bus_parent_class)->finalize(object);
}

/*! \brief Type class initializer for a GedaBus
 *
 *  \par Function Description
 *  Type class initializer for Bus. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class      The Bus class we are initialising
 *  \param [in]  class_data   The Bus structure associated with the class
 */
static void geda_bus_class_init(void *g_class, void *class_data)
{
  GedaBusClass *class          = (GedaBusClass*)g_class;
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );

  geda_bus_parent_class        = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_bus_dispose;
  gobject_class->finalize      = geda_bus_finalize;
}

/*! \brief Function to retrieve GedaBus's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaBus Type identifier. When first called,
 *  the function registers a #GedaBus in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a GedaBus and returns
 *  the unsigned integer value. The retained value is returned on
 *  all Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with GedaBus.
 */
GedaObjectType geda_bus_get_type (void)
{
  static volatile GedaObjectType geda_bus_type = 0;

  if (g_once_init_enter (&geda_bus_type)) {

    static const GTypeInfo info = {
      sizeof(GedaBusClass),
      NULL,                  /* base_init           */
      NULL,                  /* base_finalize       */
      geda_bus_class_init,   /* (GClassInitFunc)    */
      NULL,                  /* class_finalize      */
      NULL,                  /* class_data          */
      sizeof(GedaBus),
      0,                     /* n_preallocs         */
      geda_bus_instance_init /* (GInstanceInitFunc) */
    };

    const char     *string;
    GedaObjectType  type;

    string = g_intern_static_string ("GedaBus");
    type   = g_type_register_static (GEDA_TYPE_LINE, string, &info, 0);

    g_once_init_leave (&geda_bus_type, type);
  }

  return geda_bus_type;
}

/*! \brief Returns a pointer to a new GedaBus object.
 *
 *  \par Function Description
 *  Returns a pointer to a new GedaBus object.
 *
 *  \return pointer to the new GedaBus object.
 */
GedaObject *geda_bus_new (void)
{
  GedaObject *bus = g_object_new(GEDA_TYPE_BUS,
                                 "type", OBJ_BUS,
                                 "name", "bus",
                                 NULL );
  return GEDA_OBJECT(bus);
}

/*! \brief Determine if object is a Geda Bus Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Bus object.
 *
 *  \return boolean.
 */
bool is_a_geda_bus_object (GedaBus *bus)
{
  return GEDA_IS_OBJECT(bus) && (((GedaObject*)bus)->type == OBJ_BUS);
}
/** @} endgroup geda-bus-object */
