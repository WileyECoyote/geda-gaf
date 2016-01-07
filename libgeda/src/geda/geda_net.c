/* -*- geda_net.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
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
 *  Date Contributed: November, 18, 2013
 */
/*! \file geda_net.c
 *  \brief Geda Net Object Class derived from the GedaObject Class
 */
/** \defgroup geda-net-object Geda Net Object
 *  @{
 */
/*! \class Net geda_net.h "include/libgeda/geda_net.h"
 *  \implements geda-object
 *  \brief This is an implementaion class for GEDA Net Objects.
 *  A Geda Net Object represent a conductor or wire, and is used to inter
 *  connect nodes.
 */

#include <config.h>

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
  Net    *net                = (Net*)instance;
  Line   *line               = &net->parent_instance;
  Object *object             = &line->parent_instance;

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

  net->head_marker           = GEDA_TYPE_NET;
  net->tail_marker           = net->head_marker;
}

static void
geda_net_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_net_parent_class)->dispose(object);

}

/*! \brief Geda Net Object Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references and
 *   releases the memory allocated to the given Net data structure,
 *   invalidates the Net's markers, then chain up to the parent's
 *   finalize handler after.
 */
static void geda_net_finalize(GObject *object)
{
  Net *net = GEDA_NET(object);

  if(net->net_name)
    g_free(net->net_name);

  if(net->pin_label)
    g_free(net->pin_label);

  if(net->connected_to)
    g_free(net->connected_to);

  /* The object is no longer a GedaNet */
  net->head_marker = 1;
  net->tail_marker = 0;

  /* Finialize the parent GedaLine Class */
  GEDA_LINE_CLASS(geda_net_parent_class)->finalize(object);
}

/*! \brief GedaType class initializer for Net
 *
 *  \par Function Description
 *  GedaType class initializer for Net. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class      The Net class we are initialising
 *  \param [in]  class_data   The Net structure associated with the class
 */
static void geda_net_class_init(void *g_class, void *class_data)
{
  NetClass     *class          = (NetClass*)g_class;
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );

  geda_net_parent_class        = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_net_dispose;
  gobject_class->finalize      = geda_net_finalize;

}

/*! \brief Function to retrieve Net's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #Net Type identifier. When first called,
 *  the function registers a #Net in the GedaType system to obtain
 *  an identifier that uniquely itentifies a Net and returns the
 *  unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaType identifier associated with Net.
 */
GedaType geda_net_get_type (void)
{
  static volatile GedaType geda_net_type = 0;

  if (g_once_init_enter (&geda_net_type)) {

    static const GTypeInfo info = {
      sizeof(NetClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_net_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(Net),
      0,                      /* n_preallocs         */
      geda_net_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("Net");
    type   = g_type_register_static (GEDA_TYPE_LINE, string, &info, 0);

    g_once_init_leave (&geda_net_type, type);
  }

  return geda_net_type;
}

/*! \brief Returns a pointer to a new Net object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Net object.
 *
 *  \return pointer to the new Net object.
 */
Object *geda_net_new (void)
{
  Object *net = g_object_new( GEDA_TYPE_NET,
                             "type", OBJ_NET,
                             "name", "net",
                              NULL );
  return GEDA_OBJECT(net);
}

/*! \brief Determine if object is a Geda Net Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Net object.
 *
 *  \return boolean.
 */
bool is_a_geda_net_object (Net *net)
{
  return GEDA_IS_OBJECT(net) &&
        (GEDA_TYPE_NET == (net->head_marker & net->tail_marker));
}
/** @} endgroup geda-net-object */