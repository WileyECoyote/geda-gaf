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
/*! \class Bus geda_bus.h "include/libgeda/geda_bus.h"
 *  \implements geda-object
 *  \brief This is an implementaion class for GEDA Bus Objects.
 *  A Geda Bus Object is similar to net objects but are intended to
 *  represent multiple conductors.
 */

#include <config.h>

#include "libgeda_priv.h"

static GObjectClass *geda_bus_parent_class = NULL;

/*! \brief Type instance initializer for Bus
 *
 *  \par Function Description
 *  Type instance initializer for Bus, initializes a new empty
 *  Bus object by setting pointers to NULL and numbers to zero,
 *  the bus PID variable is set to the next bus index.
 *
 *  \param [in] instance The Bus structure being initialized,
 *  \param [in] g_class  The Bus class we are initializing.
 */
static void geda_bus_instance_init(GTypeInstance *instance, void *g_class)
{
  Bus    *bus                = (Bus*)instance;
  Line   *line               = &bus->parent_instance;
  Object *object             = &line->parent_instance;

  bus->bus_ripper_direction  = 0;

  bus->bus_name              = NULL;

  object->bus                = bus;
  bus->line_width            = &line->line_options.line_width;

  bus->head_marker           = GEDA_TYPE_BUS;
  bus->tail_marker           = bus->head_marker;
}

static void
geda_bus_dispose(GObject *object)
{

  G_OBJECT_CLASS(geda_bus_parent_class)->dispose(object);

}

/*! \brief Geda Bus Object Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references and
 *   releases the memory allocated to the given Bus data structure,
 *   invalidates the Bus's markers, then chain up to the parent's
 *   finalize handler after.
 */
static void geda_bus_finalize(GObject *object)
{
  Bus *bus = GEDA_BUS(object);

  if (bus->bus_name)
    GEDA_FREE(bus->bus_name);

  /* The object is no longer a GedaBus */
  bus->head_marker = 1;
  bus->tail_marker = 0;

  /* Finialize the parent GedaLine Class */
  GEDA_LINE_CLASS(geda_bus_parent_class)->finalize(object);
}

/*! \brief Type class initializer for Bus
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
  BusClass     *class          = (BusClass*)g_class;
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );

  geda_bus_parent_class        = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_bus_dispose;
  gobject_class->finalize      = geda_bus_finalize;

}

/*! \brief Function to retrieve Bus's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #Bus Type identifier. When first called,
 *  the function registers a #Bus in the GedaType system to obtain
 *  an identifier that uniquely itentifies a Bus and returns the
 *  unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaType identifier associated with Bus.
 */
GedaType geda_bus_get_type (void)
{
  static GedaType geda_bus_type = 0;

  if (g_once_init_enter (&geda_bus_type)) {

    static const GTypeInfo info = {
      sizeof(BusClass),
      NULL,                  /* base_init           */
      NULL,                  /* base_finalize       */
      geda_bus_class_init,   /* (GClassInitFunc)    */
      NULL,                  /* class_finalize      */
      NULL,                  /* class_data          */
      sizeof(Bus),
      0,                     /* n_preallocs         */
      geda_bus_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("Bus");
    type   = g_type_register_static (GEDA_TYPE_LINE, string, &info, 0);

    g_once_init_leave (&geda_bus_type, type);
  }

  return geda_bus_type;
}

/*! \brief Returns a pointer to a new Bus object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Bus object.
 *
 *  \return pointer to the new Bus object.
 */
Object *geda_bus_new (void)
{
  Object *bus = g_object_new( GEDA_TYPE_BUS,
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
bool is_a_geda_bus_object (Bus *bus)
{
  return GEDA_IS_OBJECT(bus) && (GEDA_TYPE_BUS == (bus->head_marker & bus->tail_marker));
}
/** @} endgroup geda-bus-object */
