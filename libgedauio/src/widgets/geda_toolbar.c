/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_seperator.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: March 06, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_toolbar.h"

/**
 * \brief GedaToolbar - A Seperator for Menus and Toolbars
 * \par
 * A visual widget use to seperate items in meu and toolbars.
 *
 * \defgroup GedaToolbar Geda Toolbar
 * @{
 */

enum {
  PROP_0,
  PROP_ORIENTATION
};

static void geda_toolbar_set_property (GObject        *object,
                                         unsigned int    prop_id,
                                         const GValue   *value,
                                         GParamSpec     *pspec);
static void geda_toolbar_get_property (GObject        *object,
                                         unsigned int    prop_id,
                                         GValue         *value,
                                         GParamSpec     *pspec);

static void *geda_toolbar_parent_class = NULL;

static void
geda_toolbar_get_property (GObject     *object,
                           unsigned int prop_id,
                           GValue      *value,
                           GParamSpec  *pspec)
{
  GedaToolbar *toolbar = GEDA_TOOLBAR (object);
  switch (prop_id) {

    case PROP_ORIENTATION:
      g_value_set_int (value, toolbar->orientation);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_toolbar_set_property (GObject      *object,
                           unsigned int  prop_id,
                           const GValue *value,
                           GParamSpec   *pspec)
{
  GedaToolbar *toolbar = GEDA_TOOLBAR (object);
  switch (prop_id) {

    case PROP_ORIENTATION:
      toolbar->orientation = g_value_get_int (value);
      gtk_widget_queue_resize (GTK_WIDGET (object));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_toolbar_class_init(void *class, void *class_data)
{
  /*  (GedaToolbarClass *class) */
  GObjectClass *object_class = G_OBJECT_CLASS (class);
  GParamSpec   *params;

  object_class->set_property = geda_toolbar_set_property;
  object_class->get_property = geda_toolbar_get_property;

  geda_toolbar_parent_class = g_type_class_peek_parent (class);


  params = g_param_spec_int ("orientation",
                             "orientation",
                             "either horizontal or vertical",
                              0,
                              1,
                              1,
                              G_PARAM_READWRITE);

   g_object_class_install_property (object_class, PROP_ORIENTATION, params);
}

static void
geda_toolbar_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaToolbar *toolbar = (GedaToolbar*)instance;

  toolbar->instance_type = geda_toolbar_get_type();

}

/*!
 * \brief Function to retrieve GedaToolbar's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaToolbar Type identifier. When
 *  first called, the function registers a #GedaToolbar in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaToolbar and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaToolbar.
 */
GedaType geda_toolbar_get_type (void)
{
  static GedaType geda_toolbar_type = 0;

  if (g_once_init_enter (&geda_toolbar_type)) {

    static const GTypeInfo info = {
      sizeof(GedaToolbarClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_toolbar_class_init,         /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaToolbar),
      0,                               /* n_preallocs         */
      geda_toolbar_instance_init       /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaToolbar");
    type   = g_type_register_static (GTK_TYPE_TOOLBAR, string, &info, 0);

    g_once_init_leave (&geda_toolbar_type, type);
  }

  return geda_toolbar_type;
}

/*!
 * \brief Check if an object is a GedaToolbar
 * \par Function Description
 *  Ensures \a toolbar is a valid G_Object and compares signature
 *  to geda toolbar type.
 * \return TRUE if \a toolbar is a valid GedaToolbar
 */
bool
is_a_geda_toolbar (GedaToolbar *toolbar)
{
  if (G_IS_OBJECT(toolbar)) {
    return (geda_toolbar_get_type() == toolbar->instance_type);
  }
  return FALSE;
}

/*!
 * \brief Create New GedaToolbar
 * \par Function Description
 * Creates a new #GedaToolbar with the given orientation.
 *
 * \param [in] orientation the toolbar's orientation
 *
 * Return value: a new #GedaToolbar.
 */
GtkWidget *geda_toolbar_new (int orientation)
{
  return g_object_new (GEDA_TYPE_TOOLBAR,
                       "orientation", orientation,
                       NULL);
}

/** @} endgroup GedaToolbar */
