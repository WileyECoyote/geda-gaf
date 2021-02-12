/* -*- gschem_event.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 Wiley Edward Hill
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Contributing Author: Wiley Edward Hill
 * Date Contributed: November, 4, 2013
 */
/*!
 * \file gschem_event.c
 *
 * \brief
 */

#include "../../../config.h" /* for geda_types.h */

#include <glib.h>
#include <glib-object.h>
#include <gtk/gtk.h>

#include <geda/geda_types.h>
#include <gschem_event.h>

static GObjectClass *gschem_event_parent_class = NULL;

static void gschem_event_base_handler(void *w_current, int x, int y)
{
  fprintf(stderr, "%s, internal error",__func__);
}

static
int gschem_event_press_butt(GtkWidget *widget,
                            GdkEventButton *event,
                            void *w_current)
{
  fprintf(stderr, "%s, internal error",__func__);
  return(0);
}

static
int gschem_event_release_butt(GtkWidget *widget,
                              GdkEventButton *event,
                              void *w_current)
{
  fprintf(stderr, "%s, internal error",__func__);

  return(0);
}

/*! \brief GObject finalise handler for GschemEvent
 *
 *  \par Function Description
 *  release the memory allocated to the given GschemEvent data
 *  structure and then chain up to the parent's finalize handler.
 *
 *  \param [in] object  The GschemEvent object being finalized.
 */
static void gschem_event_finalize( GObject *object )
{
  //GschemEvent *handler = GSCHEM_EVENT(object);

  G_OBJECT_CLASS( gschem_event_parent_class )->finalize( object);
}

/*! \brief Type instance initialiser for GschemEvent
 *
 *  \par Function Description
 *  Type instance initialiser for GschemEvent.
 *
 *  \param [in]  instance   The GschemEvent we are initializing.
 *  \param [in]  g_class    The GschemEventClass we are initializing.
 */
static void gschem_event_instance_init(GTypeInstance *instance, void *g_class)
{
  GschemEvent *data = (GschemEvent*)instance;

  data->state = 0;

  data->wx = 0;
  data->wy = 0;

  data->press_hid   = 0;
  data->release_hid = 0;

  data->initializer    =  gschem_event_base_handler;
  data->resolver.func  =  gschem_event_base_handler;

  data->press_butt     =  gschem_event_press_butt;
  data->release_butt   =  gschem_event_release_butt;
}

/*! \brief Type class initialiser for GschemEvent
 *
 *  \par Function Description
 *  Type class initialiser for GschemEvent. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class      The GschemEventClass we are initializing
 *  \param [in]  g_class_data Not used
 */
static void gschem_event_class_init( void *g_class, void *g_class_data )
{
  GschemEventClass *klass      = GSCHEM_EVENT_CLASS(g_class);
  GObjectClass *gobject_class  = G_OBJECT_CLASS(klass);
  gschem_event_parent_class    = g_type_class_peek_parent(klass);

  gobject_class->finalize      = gschem_event_finalize;
}

/*! \brief Get/register GschemSelection type.
 */
GedaType gschem_event_get_type (void)
{
  static unsigned int type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemEventClass),
      NULL,                            /* base_init */
      NULL,                            /* base_finalize */
      gschem_event_class_init,         /* class_init */
      NULL,                            /* class_finalize */
      NULL,                            /* class_data */
      sizeof(GschemEvent),
      0,                               /* n_preallocs */
      gschem_event_instance_init       /* instance_init */
    };

    type = g_type_register_static (G_TYPE_OBJECT, "GschemEvent", &info, 0);
  }
  return type;
}

/*! \brief Create a new instanceof the GschemEvent
 *
 *  \return A new instanceof the GschemEvent
 */
GschemEvent*
gschem_event_new ()
{
  return g_object_new(GSCHEM_TYPE_EVENT, NULL);
}
