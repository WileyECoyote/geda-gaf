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
#include "../../include/gettext.h"

/**
 * \brief GedaToolbar - A Seperator for Menus and Toolbars
 * \par
 * A visual widget use to separate items in menu and toolbars.
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

static GHashTable *toolbar_hash_table = NULL;

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
geda_toolbar_finalize (GObject *object)
{
  GedaToolbar *bar = GEDA_TOOLBAR (object);

  g_list_free(bar->children);
  bar->children = NULL;

  if (g_hash_table_remove (toolbar_hash_table, object)) {
    if (!g_hash_table_size (toolbar_hash_table)) {
      g_hash_table_destroy (toolbar_hash_table);
      toolbar_hash_table = NULL;
    }
  }

  G_OBJECT_CLASS (geda_toolbar_parent_class)->finalize (object);
}

/* widget_class->realize */
static void
geda_toolbar_box_realize (GtkWidget *widget)
{
  GTK_WIDGET_CLASS (geda_toolbar_parent_class)->realize (widget);
  gdk_window_set_type_hint (widget->window, GDK_WINDOW_TYPE_HINT_TOOLBAR);
}

static void
geda_toolbar_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  //GedaToolbar *toolbar = (GedaToolbar*)widget;

  GTK_WIDGET_CLASS (geda_toolbar_parent_class)->size_request(widget, requisition);
}

static void
geda_toolbar_class_init(void *class, void *class_data)
{
  /*  (GedaToolbarClass *class) */
  GObjectClass   *object_class;
  GtkWidgetClass *widget_class;
  GParamSpec     *params;

  object_class = (GObjectClass*)class;
  widget_class = (GtkWidgetClass*)class;

  object_class->finalize     = geda_toolbar_finalize;
  object_class->set_property = geda_toolbar_set_property;
  object_class->get_property = geda_toolbar_get_property;

  widget_class->realize      = geda_toolbar_box_realize;
  widget_class->size_request = geda_toolbar_size_request;

  geda_toolbar_parent_class = g_type_class_peek_parent (class);

  params = g_param_spec_int ("font-size",
                           _("Font Size"), /* nick name */
                           _("Set point size of the font for child widgets"), /* hint / blurb */
                              6,  /* Min value */
                              96, /* Max value */
                              10,  /* default_value */
                             (G_PARAM_READWRITE));

  gtk_widget_class_install_style_property (widget_class, params);
}

static void
geda_toolbar_instance_init(GTypeInstance *instance, void *g_class)
{
  //GedaToolbar *toolbar = (GedaToolbar*)instance;

  if (!toolbar_hash_table) {
    toolbar_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  //toolbar->children = NULL;

  g_hash_table_replace (toolbar_hash_table, instance, instance);
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
  static volatile GedaType geda_toolbar_type = 0;

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

static void
geda_toolbar_setup_label(GtkWidget *widget, GedaToolbar *bar)
{
  GtkWidget *box;
  GList *children;

  box = gtk_bin_get_child (GTK_BIN(widget));
  children = gtk_container_get_children (GTK_CONTAINER(box));

  while (children) {

    GtkWidget *child = children->data;

    if (GTK_IS_LABEL(child)) {

      PangoFontDescription *font_desc;
      GtkStyle *style;
      int bar_font_size;

      gtk_widget_style_get (GTK_WIDGET (bar),
                            "font-size", &bar_font_size,
                            NULL);

      style     = gtk_widget_get_style(child);
      font_desc = style->font_desc;

      pango_font_description_set_size (font_desc, bar_font_size * PANGO_SCALE);
      gtk_widget_modify_font(child, font_desc);
      gtk_widget_queue_resize_no_redraw(widget);
      break;
    }
    children = children->next;
  }

  bar->children = g_list_append(bar->children, widget);
}

/*!
 * \brief Check if an object is a GedaToolbar
 * \par Function Description
 *  Ensures \a toolbar is a valid G_Object and compares signature
 *  to geda toolbar type.
 *
 * \return TRUE if \a toolbar is a valid GedaToolbar
 */
bool
is_a_geda_toolbar (GedaToolbar *toolbar)
{
  if ((toolbar != NULL) && (toolbar_hash_table != NULL)) {
    return g_hash_table_lookup(toolbar_hash_table, toolbar) ? TRUE : FALSE;
  }
  return FALSE;
}

/* Any element type */
GtkWidget*
geda_toolbar_append_element (GedaToolbar        *toolbar,
                             GtkToolbarChildType type,
                             GtkWidget          *widget,
                             const char         *text,
                             const char         *tooltip_text,
                             const char         *tooltip_private_text,
                             GtkWidget          *icon,
                             GCallback           callback,
                             void               *user_data)
{
  GtkWidget *element;

  element = gtk_toolbar_append_element(GTK_TOOLBAR(toolbar), type,
                                       widget,
                                       text,
                                       tooltip_text,
                                       tooltip_private_text,
                                       GTK_WIDGET(icon), \
                                       (GtkSignalFunc) callback,
                                       user_data);

  geda_toolbar_setup_label(element, toolbar);

  return element;
}

/* Simple button items */
GtkWidget*
geda_toolbar_append_item (GedaToolbar     *toolbar,
                          const char      *text,
                          const char      *tooltip_text,
                          const char      *tooltip_private_text,
                          GtkWidget       *icon,
                          GCallback        callback,
                          void            *user_data)
{
  GtkWidget *button;

  button = gtk_toolbar_append_item (GTK_TOOLBAR(toolbar), text,
                                    tooltip_text, tooltip_private_text,
                                    GTK_WIDGET(icon),
                                    GTK_SIGNAL_FUNC(callback),
                                    user_data);

  geda_toolbar_setup_label(button, toolbar);

  return button;
}

/* Generic Widgets */
void
geda_toolbar_append_widget (GedaToolbar *bar,
                            GtkWidget   *widget,
                            const char  *tip_text,
                            const char  *tip_private)
{
  if (GTK_IS_WIDGET(widget)) {

    geda_toolbar_setup_label(widget, bar);

    gtk_toolbar_append_widget (GTK_TOOLBAR(bar), widget, tip_text, tip_private);
  }
}

/*!
 * \brief Create New GedaToolbar
 * \par Function Description
 * Creates a new #GedaToolbar with the given orientation.
 *
 * \param [in] orientation the toolbar's orientation
 *
 * \return a new #GedaToolbar.
 */
GtkWidget *geda_toolbar_new (int orientation)
{
  return g_object_new (GEDA_TYPE_TOOLBAR,
                       "orientation", orientation,
                       NULL);
}

bool
geda_toolbar_get_tooltips (GedaToolbar *toolbar)
{
  bool enabled;
  g_object_get (toolbar,"tooltips", &enabled, NULL);
  return enabled;
}

void
geda_toolbar_set_tooltips (GedaToolbar *toolbar, bool enable)
{
  /* Set GtkToolbar property, which does nothing */
  g_object_set (toolbar, "tooltips", enable, NULL);

  if (GEDA_TOOLBAR(toolbar)) {

    GList *iter;
    /* Loop thru children and set widget "has-tooltip" property */
    for (iter = toolbar->children; iter; iter = iter->next) {
      GtkWidget *widget = iter->data;
      gtk_widget_set_has_tooltip (widget, enable);
    }
  }
}

/** @} endgroup GedaToolbar */
