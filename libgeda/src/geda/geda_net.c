/* -*- geda_net.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2016 Wiley Edward Hill
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
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
 *  Date Contributed: November, 18, 2013
 */
/*! \file geda_net.c
 *  \brief GedaNet Object Class Module
 */

/** \defgroup geda-net-object GedaNet Object
 *  @{
 * \brief Implmentation of #GedaNet Class
 * \par
 *  A GedaNet Object represent a conductor or wire, and is used to inter
 *  connect nodes. The GedaNet class is derived from the GedaLine class.
 *
 * \class GedaNet geda_net.h "include/libgeda/geda_net.h"
 * \implements geda-line
 * \implements geda-object
 */

#include "../../../config.h"

#include <libgeda_priv.h>

static GObjectClass *geda_net_parent_class = NULL;

/*! \brief GedaType instance initializer for Net
 *
 *  \par Function Description
 *  GedaType instance initializer for Net, initializes a new empty
 *  Net object by setting pointers to NULL and numbers to zero.
 *
 *  \param [in] instance The Net structure being initialized,
 *  \param [in] g_class  The Net class we are initializing.
 */
static void geda_net_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaNet    *net            = (GedaNet*)instance;
  GedaLine   *line           = &net->parent_instance;
  GedaObject *object         = &line->parent_instance;

  net->nid                   = -1;
  net->net_name_has_priority = FALSE;

  net->net_name              = NULL;
  net->pin_label             = NULL;

  net->connected_to          = NULL;

  /* Tracking total number of entities connected by this net */
  net->net_num_connected     = 0;
  net->valid_num_connected   = FALSE;

  net->prev                  = NULL;
  net->next                  = NULL;

  object->net                = net;

  net->line_width            = &line->line_options.line_width;
}

static void geda_net_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_net_parent_class)->dispose(object);

}

/*! \brief Geda Net GedaObject Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references and
 *   releases the memory allocated to the given Net data structure,
 *   invalidates the Net's markers, then chain up to the parent's
 *   finalize handler after.
 */
static void geda_net_finalize(GObject *object)
{
  GedaNet    *net = GEDA_NET(object);
  GedaObject *obj = GEDA_OBJECT(object);

  if(net->net_name)
    g_free(net->net_name);

  if(net->pin_label)
    g_free(net->pin_label);

  if(net->connected_to)
    g_free(net->connected_to);

  /* The object is no longer a GedaNet */
  obj->net = NULL;

  /* Finialize the parent GedaLine Class */
  GEDA_LINE_CLASS(geda_net_parent_class)->finalize(object);
}

/*! \brief GedaType class initializer for GedaNet
 *
 *  \par Function Description
 *  GedaType class initializer for GedaNet. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  klass       The GedaNet class we are initialising
 *  \param [in]  class_data  The GedaNet structure associated with the class
 */
static void geda_net_class_init(void *klass, void *class_data)
{
  GedaNetClass *class          = (GedaNetClass*)klass;
  GObjectClass *gobject_class  = (GObjectClass*)klass;

  geda_net_parent_class        = g_type_class_peek_parent(class);

  gobject_class->dispose       = geda_net_dispose;
  gobject_class->finalize      = geda_net_finalize;
}

/*! \brief Function to retrieve Net's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaNet Type identifier. When first called,
 *  the function registers a #GedaNet in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a GedaNet and returns
 *  the unsigned integer value. The retained value is returned on
 *  all Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with GedaNet.
 */
GedaObjectType geda_net_get_type (void)
{
  static volatile GedaObjectType geda_net_type = 0;

  if (g_once_init_enter (&geda_net_type)) {

    static const GTypeInfo info = {
      sizeof(GedaNetClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_net_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(GedaNet),
      0,                      /* n_preallocs         */
      geda_net_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaNet");
    type   = g_type_register_static (GEDA_TYPE_LINE, string, &info, 0);

    g_once_init_leave (&geda_net_type, type);
  }

  return geda_net_type;
}

/*! \brief Returns a pointer to a new GedaNet object.
 *
 *  \par Function Description
 *  Returns a pointer to a new GedaNet object.
 *
 *  \return pointer to the new GedaNet object.
 */
GedaObject *geda_net_new (void)
{
  GedaObject *net = g_object_new( GEDA_TYPE_NET,
                             "type", OBJ_NET,
                             "name", "net",
                              NULL );
  return GEDA_OBJECT(net);
}

/*! \brief Determine if an object is a GedaNet object.
 *
 *  \par Function Description
 *  Returns true if the argument is a GedaNet object.
 *
 *  \return boolean.
 */
bool is_a_geda_net (const GedaNet *net)
{
  return GEDA_IS_OBJECT(net) && (((GedaObject*)net)->type == OBJ_NET);
}

/*!
 * \brief Retrieve the Netname of a GedaNet
 * \par Function Description
 *  Returns the net_name of \a net if and only if \a net is a
 *  valid GedaNet object. The string should not be released or
 *  modified.
 *
 * \returns pointer to net_name member of \a net or NULL if invalid.
 */
const char *geda_net_get_netname (ConstObject *object)
{
  if (is_a_geda_net(object->net)) {
    return object->net->net_name;
  }
  return NULL;
}

/*!
 * \brief Retrieve the Pin Label of a GedaNet
 * \par Function Description
 *  Returns the pin_label of \a net if and only if \a net is a
 *  valid GedaNet object. The string should not be released or
 *  modified.
 *
 * \returns pointer to pin_label member of \a net or NULL if invalid.
 */
const char *geda_net_get_pin_label (ConstObject *object)
{
  if (is_a_geda_net(object->net)) {
    return object->net->pin_label;
  }
  return NULL;
}

/*!
 * \brief Retrieve the First ordinate value of the Net coordinates
 * \par Function Description
 *  Returns the first X value of \a net if and only if \a net is
 *  a valid GedaNet object.
 *
 * \returns integer value of X0 if \a net is invalid.
 */
int geda_net_get_x0 (const GedaNet *net) {
  if (is_a_geda_net(net)) {
    return GEDA_LINE(net)->x[0];
  }
  return -0;
}

/*!
 * \brief Retrieve the Second ordinate value of the Net coordinates
 * \par Function Description
 *  Returns the second X value of \a net if and only if \a net is
 *  a valid GedaNet object.
 *
 * \returns integer value of X1 if \a net is invalid.
 */
int geda_net_get_x1 (const GedaNet *net) {
  if (is_a_geda_net(net)) {
    return GEDA_LINE(net)->x[1];
  }
  return -0;
}

/*!
 * \brief Retrieve the First Abscissa of the Net coordinates
 * \par Function Description
 *  Returns the first Y value of \a net if and only if \a net is
 *  a valid GedaNet object.
 *
 * \returns integer value of Y0 if \a net is invalid.
 */
int geda_net_get_y0 (const GedaNet *net) {
  if (is_a_geda_net(net)) {
    return GEDA_LINE(net)->y[0];
  }
  return -0;
}

/*!
 * \brief Retrieve the Second Abscissa of the Net coordinates
 * \par Function Description
 *  Returns the second Y value of \a net if and only if \a net is
 *  a valid GedaNet object.
 *
 * \returns integer value of Y1 if \a net is invalid.
 */
int geda_net_get_y1 (const GedaNet *net) {
  if (is_a_geda_net(net)) {
    return GEDA_LINE(net)->y[1];
  }
  return -0;
}

/*!
 * \brief Set the Netname of a GedaNet
 * \par Function Description
 *  Sets the netname member of \a net if \a net is a valid GedaNet
 *  object. The netname property holds a pointer to a string containing
 *  the label of the pin to which the net is connected. if \a net is
 *  invalid then nothing is done.
 */
void geda_net_set_netname (ConstObject *object, const char *netname)
{
  if (is_a_geda_net(object->net)) {
    if (object->net->net_name) {
      g_free(object->net->net_name);
    }
    object->net->net_name = geda_strdup(netname);
  }
}

/*!
 * \brief Set the Pin Label of a GedaNet
 * \par Function Description
 *  Sets the pin label member of \a net if \a net is a valid GedaNet
 *  object. The pin label property holds a pointer to a string containing
 *  the label of the pin to which the net is connected. if \a net is
 *  invalid then nothing is done.
 */
void geda_net_set_pin_label (ConstObject *object, const char *pin_label)
{
  if (is_a_geda_net(object->net)) {
    if (object->net->pin_label) {
      g_free(object->net->pin_label);
    }
    object->net->pin_label = geda_strdup(pin_label);
  }
}

/*!
 * \brief Set the First X coordinate of a GedaNet
 * \par Function Description
 *  Sets the first X of coordinate \a net if \a net is a valid
 *  GedaNet object, if \a net is invalid then nothing is done.
 */
void geda_net_set_x0 (GedaNet *net, int x) {
  if (is_a_geda_net(net)) {
    GEDA_LINE(net)->x[0] = x;
  }
}

/*!
 * \brief Set the Second X coordinate of a GedaNet
 * \par Function Description
 *  Sets the second X of coordinate \a net if \a net is a valid
 *  GedaNet object, if \a net is invalid then nothing is done.
 */
void geda_net_set_x1 (GedaNet *net, int x) {
  if (is_a_geda_net(net)) {
    GEDA_LINE(net)->x[1] = x;
  }
}

/*!
 * \brief Set the First Y coordinate of a GedaNet
 * \par Function Description
 *  Sets the first Y of coordinate \a net if \a net is a valid
 *  GedaNet object, if \a net is invalid then nothing is done.
 */
void geda_net_set_y0 (GedaNet *net, int y) {
  if (is_a_geda_net(net)) {
    GEDA_LINE(net)->y[0] = y;
  }
}

/*!
 * \brief Set the Second Y coordinate of a GedaNet
 * \par Function Description
 *  Sets the second Y of coordinate \a net if \a net is a valid
 *  GedaNet object, if \a net is invalid then nothing is done.
 */
void geda_net_set_y1 (GedaNet *net, int y) {
  if (is_a_geda_net(net)) {
    GEDA_LINE(net)->y[1] = y;
  }
}
/** @} endgroup geda-net-object */
