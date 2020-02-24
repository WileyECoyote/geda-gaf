/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_invisible.c
 *
 * GTK - The GIMP Toolkit
 *
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 * with modifications, December 30, 2018.
 */

#include "../../../config.h"

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include <gtk/gtk.h>

#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_invisible.h"
#include "../../include/gettext.h"

/**
 * \brief GedaInvisible - A Widget which is not displayed
 * \par
 * The #GedaInvisible widget is used used for reliable pointer
 * grabs and selection handling in the code for drag-and-drop
 * and is probably not very useful for application developers.
 * While similar to a GtkInvisible, GedaInvisible objects are
 * derived from GtkWindow, and NOT GtkWidget.
 *
 * \defgroup GedaInvisible Invisible Window Widget
 * @{
 */

struct _GedaInvisibleData
{
  GdkScreen    *screen;
  bool          has_user_ref_count;
};

enum {
  PROP_0,
  PROP_SCREEN,
  LAST_ARG
};

static void geda_invisible_destroy       (GtkObject         *widget);
static void geda_invisible_realize       (GtkWidget         *widget);
static void geda_invisible_show          (GtkWidget         *widget);
static void geda_invisible_size_allocate (GtkWidget         *widget,
                                          GtkAllocation     *allocation);
static void geda_invisible_set_property  (GObject           *object,
                                          unsigned int       prop_id,
                                          const GValue      *value,
                                          GParamSpec        *pspec);
static void geda_invisible_get_property  (GObject           *object,
                                          unsigned int       prop_id,
                                          GValue            *value,
                                          GParamSpec        *pspec);

static GObject *geda_invisible_constructor (GedaType               type,
                                            unsigned int           n_construct_properties,
                                            GObjectConstructParam *construct_params);

/* Table of pointers to GedaInvisible instances */
static GHashTable *invisible_hash_table = NULL;

static void *geda_invisible_parent_class = NULL;

/* The constructor is overriden here so that the invisible can be
 * realized on the correct screen after the screen property has been
 * set.
 */
static GObject*
geda_invisible_constructor (GedaType               type,
                            unsigned int           n_construct_properties,
                            GObjectConstructParam *construct_params)
{
  GObject *object;

  object = G_OBJECT_CLASS (geda_invisible_parent_class)->constructor (type,
                                                                     n_construct_properties,
                                                                     construct_params);

  gtk_widget_realize (GTK_WIDGET (object));

  return object;
}

static void geda_invisible_destroy (GtkObject *widget)
{
  GedaInvisible *invisible = GEDA_INVISIBLE (widget);
  GedaInvisibleData *priv = invisible->priv;

  if (priv->has_user_ref_count) {
    priv->has_user_ref_count = FALSE;
    g_object_unref (invisible);
  }

  GTK_OBJECT_CLASS (geda_invisible_parent_class)->destroy (widget);
}

/*!
 * \brief gobject_class->finalize a GedaInvisible object
 * \par Function Description
 *  Releases resources associated with the GedaInvisible object.
 *  The object should not be referenced after this function is
 *  executed.
 */
static void geda_invisible_finalize (GObject *object)
{
  if (g_hash_table_remove (invisible_hash_table, object)) {
    if (!g_hash_table_size (invisible_hash_table)) {
      g_hash_table_destroy (invisible_hash_table);
      invisible_hash_table = NULL;
    }
  }
  G_OBJECT_CLASS (geda_invisible_parent_class)->finalize (object);
}

static void geda_invisible_set_property  (GObject      *object,
                                          unsigned int  prop_id,
                                          const GValue *value,
                                          GParamSpec   *pspec)
{
  GedaInvisible *invisible = GEDA_INVISIBLE (object);

  switch (prop_id) {
    case PROP_SCREEN:
      geda_invisible_set_screen (invisible, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_invisible_get_property  (GObject      *object,
                                          unsigned int  prop_id,
                                          GValue       *value,
                                          GParamSpec   *pspec)
{
  GedaInvisible *invisible = GEDA_INVISIBLE (object);
  GedaInvisibleData *priv = invisible->priv;

  switch (prop_id) {
    case PROP_SCREEN:
      g_value_set_object (value, priv->screen);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_invisible_realize (GtkWidget *widget)
{
  GdkWindow     *parent;
  GdkWindow     *window;
  GdkWindowAttr  attributes;
  int attributes_mask;

  gtk_widget_set_realized (widget, TRUE);

  parent = gtk_widget_get_parent_window (widget);

  if (parent == NULL)
    parent = gdk_screen_get_root_window (gtk_widget_get_screen (widget));

  attributes.x = -100;
  attributes.y = -100;
  attributes.width = 10;
  attributes.height = 10;
  attributes.window_type = GDK_WINDOW_TEMP;
  attributes.wclass = GDK_INPUT_ONLY;
  attributes.override_redirect = TRUE;
  attributes.event_mask = gtk_widget_get_events (widget);

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_NOREDIR;

  window = gdk_window_new (parent, &attributes, attributes_mask);

  geda_set_widget_window (widget, window);

#if GTK_MAJOR_VERSION < 3

  GtkStyle *style;

  gdk_window_set_user_data (window, widget);

  style = geda_get_widget_style(widget);

  geda_set_widget_style (widget, gtk_style_attach (style, window));

#else

  gtk_widget_set_window (widget, window);
  gtk_widget_register_window (widget, window);

#endif
}

#if GTK_MAJOR_VERSION < 3

static void
geda_invisible_style_set (GtkWidget *widget, GtkStyle  *previous_style)
{
  /* Don't chain up to parent implementation */
}

#else

static void geda_invisible_style_set (GtkWidget *widget)
{
  /* Do not chain up to parent implementation */
}

#endif

static void geda_invisible_show (GtkWidget *widget)
{
  gtk_widget_set_visible (widget, TRUE);
  gtk_widget_map (widget);
}

static void
geda_invisible_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  gtk_widget_set_allocation (widget, allocation);
}

static void geda_invisible_class_init (void *class, void *data)
{
  GObjectClass   *object_class;
  GtkObjectClass *gtkobj_class;
  GtkWidgetClass *widget_class;

  gtkobj_class  = (GtkObjectClass*)class;
  object_class  = (GObjectClass*)class;
  widget_class  = (GtkWidgetClass*)class;

  gtkobj_class->destroy       = geda_invisible_destroy;

  object_class->set_property  = geda_invisible_set_property;
  object_class->get_property  = geda_invisible_get_property;
  object_class->constructor   = geda_invisible_constructor;
  object_class->finalize      = geda_invisible_finalize;

  widget_class->realize       = geda_invisible_realize;

#if GTK_MAJOR_VERSION < 3
  widget_class->style_set     = geda_invisible_style_set;
#else
  widget_class->style_updated = geda_invisible_style_set;
#endif

  widget_class->show          = geda_invisible_show;
  widget_class->size_allocate = geda_invisible_size_allocate;

  geda_invisible_parent_class = g_type_class_peek_parent(class);

  g_object_class_install_property (object_class,
                                   PROP_SCREEN,
                                   g_param_spec_object ("screen",
                                                      _("Screen"),
                                                      _("The screen where this window will be displayed"),
                                   GDK_TYPE_SCREEN,
                                   G_PARAM_READWRITE));
}

static void
geda_invisible_instance_init(GTypeInstance *instance, void *class)
{
  GedaInvisible     *invisible;
  GedaInvisibleData *priv;

  invisible = (GedaInvisible*)instance;

  priv = g_malloc0 (sizeof(GedaInvisibleData));

  gtk_widget_set_has_window (GTK_WIDGET (invisible), TRUE);

  g_object_ref_sink (invisible);

  priv->has_user_ref_count = TRUE;
  priv->screen = gdk_screen_get_default ();

  invisible->priv = priv;

  if (!invisible_hash_table) {
    invisible_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (invisible_hash_table, instance, instance);
}

/*!
 * \brief Retrieve GedaInvisible's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaInvisible Type identifier. When first
 *  called, the function registers a #GedaInvisible in the GedaType
 *  system to obtain an identifier that uniquely itentifies a GedaInvisible
 *  and returns the unsigned integer value. The retained value is returned
 *  on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaInvisible.
 */
GedaType geda_invisible_get_type (void)
{
  static volatile GedaType geda_invisible_type = 0;

  if (g_once_init_enter (&geda_invisible_type)) {

    static const GTypeInfo info = {
      sizeof(GedaInvisibleClass),
      NULL,                             /* base_init           */
      NULL,                             /* base_finalize       */
      geda_invisible_class_init,        /* (GClassInitFunc)    */
      NULL,                             /* class_finalize      */
      NULL,                             /* class_data          */
      sizeof(GedaInvisible),
      0,                                /* n_preallocs         */
      geda_invisible_instance_init      /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaInvisible");
    type   = g_type_register_static (GTK_TYPE_WINDOW, string, &info, 0);

    g_once_init_leave (&geda_invisible_type, type);
  }

  return geda_invisible_type;
}

/*!
 * \brief Check if an object is a GedaInvisible
 * \par Function Description
 *  Determines if \a invisible is valid by verifying \a invisible
 *  is included in the hash table of GedaInvisible objects.
 *
 * \returns TRUE if \a invisible is a valid GedaInvisible
 */
bool is_a_geda_invisible (GedaInvisible *invisible)
{
  if ((invisible != NULL) && (invisible_hash_table != NULL)) {
    return g_hash_table_lookup(invisible_hash_table, invisible) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Create a New GedaInvisible Object
 * \par Function Description
 *  Creates a new #GedaInvisible.
 *
 * \returns a new #GedaInvisible.
 */
GtkWidget *geda_invisible_new (void)
{
  return g_object_new (GEDA_TYPE_INVISIBLE, NULL);
}

/*!
 * \brief Create a New GedaInvisible Object for a Specified Screen
 * \par Function Description
 *  Creates a new #GedaInvisible object for a specified screen.
 *
 * \param[in] screen A GdkScreen for which the #GedaInvisible will be created.
 *
 * \returns a new #GedaInvisible object.
 */
GtkWidget *geda_invisible_new_for_screen (GdkScreen *screen)
{
  g_return_val_if_fail (GDK_IS_SCREEN (screen), NULL);

  return g_object_new (GEDA_TYPE_INVISIBLE, "screen", screen, NULL);
}

/*!
 * \brief Set the GedaInvisible Screen Property
 * \par Function Description
 *  Sets the GedaInvisible private screen where the #GedaInvisible
 *  object will receive event signals.
 *
 * \param[in] invisible Pointer to a #GedaInvisible
 * \param[in] screen    A GdkScreen
 */
void geda_invisible_set_screen (GedaInvisible *invisible, GdkScreen *screen)
{
  GedaInvisibleData *priv;
  GtkWidget *widget;
  GdkScreen *previous_screen;
  bool was_realized;

  g_return_if_fail (GEDA_IS_INVISIBLE (invisible));
  g_return_if_fail (GDK_IS_SCREEN (screen));

  priv = invisible->priv;

  if (screen == priv->screen)
    return;

  widget = GTK_WIDGET (invisible);

  previous_screen = priv->screen;
  was_realized = gtk_widget_get_realized (widget);

  if (was_realized)
    gtk_widget_unrealize (widget);

  priv->screen = screen;

  if (screen != previous_screen) {
    gtk_window_set_screen ((GtkWindow*)widget, screen);
    g_object_notify (G_OBJECT (invisible), "screen");
  }

  if (was_realized) {
    gtk_widget_realize (widget);
  }
}

/*!
 * \brief Retrieve the GedaInvisible Screen Property
 * \par Function Description
 *  Returns the GedaInvisible private screen member.
 *
 * \param[in] invisible  Pointer to a #GedaInvisible
 *
 * \returns the GdkScreen object associated with \a invisible
 */
GdkScreen *geda_invisible_get_screen (GedaInvisible *invisible)
{
  g_return_val_if_fail (GEDA_IS_INVISIBLE (invisible), NULL);

  return invisible->priv->screen;
}

/** @} endgroup GedaInvisible */
