/* -*- geda_bus.c -*-
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
 *  Date Contributed: November, 18, 2013
 */
/*! \file geda_bus.c
 *  \brief GedaBus Class Module
 */

/** \defgroup geda-bus-object GedaBus Object
 *  @{
 * \brief Implmentation of #GedaBus Class
 * \par
 *  A Geda Bus Object is similar to net objects but are intended to
 *  represent multiple conductors. The GedaBus class is derived from
 *  the GedaLine class.
 *
 * \class GedaBus geda_bus.h "include/libgeda/geda_bus.h"
 * \implements geda-line
 * \implements geda-object
 */

#include "../../../config.h"

#include <libgeda_priv.h>

enum {
  PROP_0,
  PROP_RIPPER_DIR,
};

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
  GedaLine   *line           = &bus->parent_instance;
  GedaObject *object         = &line->parent_instance;

  bus->ripper_direction  = 0;

  bus->bus_name              = NULL;

  object->bus                = bus;
  bus->line_width            = &line->line_options.line_width;
}

static void geda_bus_dispose(GObject *object)
{

  G_OBJECT_CLASS(geda_bus_parent_class)->dispose(object);

}

/*! \brief GedaBus Object Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references and
 *   releases the memory allocated to the given GedaBus data structure,
 *   then chain up to the parent's finalize handler after.
 */
static void geda_bus_finalize(GObject *object)
{
  GedaBus    *bus = (GedaBus*)object;
  GedaObject *obj = (GedaObject*)object;

  if (bus->bus_name)
    GEDA_FREE(bus->bus_name);

  /* The object is no longer a GedaBus */
  obj->bus = NULL;

  /* Finialize the parent GedaLine Class */
  ((GedaLineClass*)geda_bus_parent_class)->finalize(object);
}

static void get_property (GObject *object, unsigned int  prop_id,
                                           GValue       *value,
                                           GParamSpec   *pspec)

{
  GedaBus *bus = (GedaBus*)object;

  switch (prop_id)
  {
    case PROP_RIPPER_DIR:
      g_value_set_int (value, bus->ripper_direction);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
set_property (GObject *object, unsigned int  prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
  GedaBus *bus = (GedaBus*)object;

  switch (prop_id)
  {
    case PROP_RIPPER_DIR:
      bus->ripper_direction = g_value_get_int (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/*! \brief Type class initializer for a GedaBus
 *
 *  \par Function Description
 *  Type class initializer for Bus. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  klass       The Bus class we are initializing
 *  \param [in]  class_data  The Bus structure associated with the class
 */
static void geda_bus_class_init(void *klass, void *class_data)
{
  GedaBusClass *class         = (GedaBusClass*)klass;
  GObjectClass *object_class  = (GObjectClass*)klass;
  GParamSpec   *params;

  geda_bus_parent_class       = g_type_class_peek_parent(class);

  object_class->dispose       = geda_bus_dispose;
  object_class->finalize      = geda_bus_finalize;

  object_class->get_property  = get_property;
  object_class->set_property  = set_property;

  params = g_param_spec_int ("bus-ripper-direction",
                           _("Bus Ripper Direction"),
                           _("The bus ripper direction"),
                             0,
                             1,
                             0,
                             (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_RIPPER_DIR, params);
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

/*! \brief Determine if object is a GedaBus Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Bus object.
 *
 *  \return boolean.
 */
bool is_a_geda_bus (const GedaBus *bus)
{
  return GEDA_IS_OBJECT(bus) && (((GedaObject*)bus)->type == OBJ_BUS);
}

bool geda_bus_get_position (const GedaBus *bus, int *x, int *y)
{
  if (is_a_geda_bus(bus)) {

    GedaLine *line = (GedaLine*)bus;

    *x = line->x[0];
    *y = line->y[0];

    return TRUE;
  }
  return -0;
}

/*!
 * \brief Retrieve Bus Ripper Direction Property of a bus
 * \par Function Description
 *  Returns the value of the \a bus ripper_direction property
 *  if and only if \a bus is a valid GedaBus object.
 *
 * \return value of ripper_direction or -0 if \a bus is invalid.
 *
 * \sa geda_bus_set_ripper_direction
 */
int geda_bus_get_ripper_direction (const GedaBus *bus) {
  if (is_a_geda_bus(bus)) {
    return bus->ripper_direction;
  }
  return -0;
}

/*!
 * \brief Retrieve the First ordinate value of the Bus coordinates
 * \par Function Description
 *  Returns the first X value of \a bus if and only if \a bus is
 *  a valid GedaBus object.
 *
 * \return integer value of X0 if \a bus is invalid.
 */
int geda_bus_get_x0 (const GedaBus *bus) {
  if (is_a_geda_bus(bus)) {
    return GEDA_LINE(bus)->x[0];
  }
  return -0;
}

/*!
 * \brief Retrieve the Second ordinate value of the Bus coordinates
 * \par Function Description
 *  Returns the second X value of \a bus if and only if \a bus is
 *  a valid GedaBus object.
 *
 * \return integer value of X1 if \a bus is invalid.
 */
int geda_bus_get_x1 (const GedaBus *bus) {
  if (is_a_geda_bus(bus)) {
    return GEDA_LINE(bus)->x[1];
  }
  return -0;
}

/*!
 * \brief Retrieve the First Abscissa of the Bus coordinates
 * \par Function Description
 *  Returns the first Y value of \a bus if and only if \a bus is
 *  a valid GedaBus object.
 *
 * \return integer value of Y0 if \a bus is invalid.
 */
int geda_bus_get_y0 (const GedaBus *bus) {
  if (is_a_geda_bus(bus)) {
    return GEDA_LINE(bus)->y[0];
  }
  return -0;
}

/*!
 * \brief Retrieve the Second Abscissa of the Bus coordinates
 * \par Function Description
 *  Returns the second Y value of \a bus if and only if \a bus is
 *  a valid GedaBus object.
 *
 * \return integer value of Y1 if \a bus is invalid.
 */
int geda_bus_get_y1 (const GedaBus *bus) {
  if (is_a_geda_bus(bus)) {
    return GEDA_LINE(bus)->y[1];
  }
  return -0;
}

/*!
 * \brief Set the ripper-direction Property of a bus
 * \par Function Description
 *  Sets the value of the \a bus ripper_direction property
 *  if and only if \a bus is a valid GedaBus object.
 *
 * \sa geda_bus_get_ripper_direction
 */
void geda_bus_set_ripper_direction (GedaBus *bus, int dir) {
  if (is_a_geda_bus(bus)) {
    bus->ripper_direction = dir;
  }
}

/*!
 * \brief Set the First X coordinate of a GedaBus
 * \par Function Description
 *  Sets the first X of coordinate \a bus if \a bus is a valid
 *  GedaBus object, if \a bus is invalid then nothing is done.
 */
void geda_bus_set_x0 (GedaBus *bus, int x) {
  if (is_a_geda_bus(bus)) {
    GEDA_LINE(bus)->x[0] = x;
  }
}

/*!
 * \brief Set the Second X coordinate of a GedaBus
 * \par Function Description
 *  Sets the second X of coordinate \a bus if \a bus is a valid
 *  GedaBus object, if \a bus is invalid then nothing is done.
 */
void geda_bus_set_x1 (GedaBus *bus, int x) {
  if (is_a_geda_bus(bus)) {
    GEDA_LINE(bus)->x[1] = x;
  }
}

/*!
 * \brief Set the First Y coordinate of a GedaBus
 * \par Function Description
 *  Sets the first Y of coordinate \a bus if \a bus is a valid
 *  GedaBus object, if \a bus is invalid then nothing is done.
 */
void geda_bus_set_y0 (GedaBus *bus, int y) {
  if (is_a_geda_bus(bus)) {
    GEDA_LINE(bus)->y[0] = y;
  }
}

/*!
 * \brief Set the Second Y coordinate of a GedaBus
 * \par Function Description
 *  Sets the second Y of coordinate \a bus if \a bus is a valid
 *  GedaBus object, if \a bus is invalid then nothing is done.
 */
void geda_bus_set_y1 (GedaBus *bus, int y) {
  if (is_a_geda_bus(bus)) {
    GEDA_LINE(bus)->y[1] = y;
  }
}

/** @} endgroup geda-bus-object */
