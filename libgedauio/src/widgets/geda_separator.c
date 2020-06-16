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

#include "../../include/geda_separator.h"

/**
 * \brief GedaSeparator - A Seperator for Menus and Toolbars
 * \par
 *  A visual widget used to separate items in status bars, tool bars
 *  and other widgets.
 *
 * \defgroup GedaSeparator Geda Separator
 * @{
 */

enum {
  PROP_0,
  PROP_ORIENTATION
};

static void geda_separator_set_property (GObject        *object,
                                         unsigned int    prop_id,
                                         const GValue   *value,
                                         GParamSpec     *pspec);
static void geda_separator_get_property (GObject        *object,
                                         unsigned int    prop_id,
                                         GValue         *value,
                                         GParamSpec     *pspec);

static void geda_separator_size_request (GtkWidget      *widget,
                                         GtkRequisition *requisition);
static bool geda_separator_expose       (GtkWidget      *widget,
                                         GdkEventExpose *event);

static void *geda_separator_parent_class = NULL;

/* Table of pointers to GedaSeparator instances */
static GHashTable *separator_hash = NULL;

#if GTK_MAJOR_VERSION < 3

static bool
geda_separator_expose (GtkWidget *widget, GdkEventExpose *event)
{
  GedaSeparator *seperator = (GedaSeparator*)widget;
  GtkStateType   state;
  GtkStyle       style;
  GdkWindow      window;
  bool wide_separators;
  int  separator_width;
  int  separator_height;

  if (!gtk_widget_is_drawable (widget))
    return FALSE;

  gtk_widget_style_get (widget,
                        "wide-separators",  &wide_separators,
                        "separator-width",  &separator_width,
                        "separator-height", &separator_height,
                        NULL);

  state  = gtk_widget_get_state (widget);
  style  = geda_get_widget_style (widget);
  window = geda_get_widget_window (widget);

  if (seperator->orientation == 0) { /* Horizontal */

    if (wide_separators) {
        gtk_paint_box (style, window, state, GTK_SHADOW_ETCHED_OUT,
                       &event->area, widget, "hseparator",
                       widget->allocation.x,
                       widget->allocation.y +
                      (widget->allocation.height - separator_height) / 2,
                       widget->allocation.width,
                       separator_height);
    }
    else {
        gtk_paint_hline (style, window, state,
                         &event->area, widget, "hseparator",
                         widget->allocation.x,
                         widget->allocation.x + widget->allocation.width - 1,
                         widget->allocation.y +
                        (widget->allocation.height - widget->style->ythickness) / 2);
    }
  }
  else {

    if (wide_separators) {
      gtk_paint_box (style, window, state, GTK_SHADOW_ETCHED_OUT,
                     &event->area, widget, "vseparator",
                     widget->allocation.x +
                    (widget->allocation.width - separator_width) / 2,
                     widget->allocation.y,
                     separator_width,
                     widget->allocation.height);
    }
    else {
      gtk_paint_vline (style,
                       widget->window, state,
                       &event->area, widget, "vseparator",
                       widget->allocation.y,
                       widget->allocation.y + widget->allocation.height - 1,
                       widget->allocation.x +
                      (widget->allocation.width - widget->style->xthickness) / 2);
    }
  }

  return FALSE;
}

static void
geda_separator_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GedaSeparator *separator = GEDA_SEPARATOR (widget);
  bool wide_separators;
  int  separator_width;
  int  separator_height;

  gtk_widget_style_get (widget,
                        "wide-separators",  &wide_separators,
                        "separator-width",  &separator_width,
                        "separator-height", &separator_height,
                        NULL);

  requisition->width  = 1;
  requisition->height = 1;

  if (separator->orientation == 0) {

    if (wide_separators)
      requisition->height = separator_height;
    else
      requisition->height = widget->style->ythickness;
  }
  else {

    if (wide_separators)
      requisition->width = separator_width;
    else
      requisition->width = widget->style->xthickness;
  }
}

#else /* !GTK_MAJOR_VERSION < 3 */

static void
geda_separator_get_preferred_size (GtkWidget *widget,
                                   int        orientation,
                                   int       *minimum,
                                   int       *natural)
{
  bool wide_sep;
  int  sep_width;
  int  sep_height;

  gtk_widget_style_get (widget,
                        "wide-separators",  &wide_sep,
                        "separator-width",  &sep_width,
                        "separator-height", &sep_height,
                        NULL);

  if (orientation == seperator->orientation) {

    *minimum = *natural = 1;
  }
  else if (orientation == 1) { /* VERTICAL*/

    *minimum = *natural = wide_sep ? sep_height : 1;
  }
  else {

    *minimum = *natural = wide_sep ? sep_width : 1;
  }
}

static void
geda_separator_get_preferred_width (GtkWidget *widget,
                                    int       *minimum,
                                    int       *natural)
{
  geda_separator_get_preferred_size (widget, 0, minimum, natural);
}

static void
geda_separator_get_preferred_height (GtkWidget *widget,
                                    int        *minimum,
                                    int        *natural)
{
  geda_separator_get_preferred_size (widget, 1 /* VERTICAL*/ , minimum, natural);
}

static bool
geda_separator_draw (GtkWidget *widget, cairo_t *cr)
{
  GedaSeparator *separator = GEDA_SEPARATOR (widget);

  GtkStyleContext *context;
  bool wide_separators;
  int  separator_width;
  int  separator_height;
  int  width, height;

  gtk_widget_style_get (widget,
                        "wide-separators",  &wide_separators,
                        "separator-width",  &separator_width,
                        "separator-height", &separator_height,
                        NULL);

  context = gtk_widget_get_style_context (widget);
  width   = gtk_widget_get_allocated_width (widget);
  height  = gtk_widget_get_allocated_height (widget);

  if (separator->orientation == 0) {

    if (wide_separators) {
      gtk_render_frame (context, cr,
                        0, (height - separator_height) / 2,
                        width, separator_height);
    }
    else {
      gtk_render_line (context, cr,
                       0, height / 2,
                       width - 1, height / 2);
    }
  }
  else {

    if (wide_separators) {
      gtk_render_frame (context, cr,
                        (width - separator_width) / 2, 0,
                        separator_width, height);
    }
    else {
      gtk_render_line (context, cr,
                       width / 2, 0,
                       width / 2, height - 1);
    }
  }

  return FALSE;
}

#endif

/*!
 * \brief gobject_class->finalize a GedaSeparator object
 * \par Function Description
 *  The object should not be referenced after this function executes.
 */
static void
geda_separator_finalize (GObject *object)
{
  if (g_hash_table_remove (separator_hash, object)) {
    if (!g_hash_table_size (separator_hash)) {
      g_hash_table_destroy (separator_hash);
      separator_hash = NULL;
    }
  }

  G_OBJECT_CLASS (geda_separator_parent_class)->finalize (object);
}

static void
geda_separator_get_property (GObject     *object,
                             unsigned int prop_id,
                             GValue      *value,
                             GParamSpec  *pspec)
{
  GedaSeparator *separator = GEDA_SEPARATOR (object);
  switch (prop_id) {

    case PROP_ORIENTATION:
      g_value_set_int (value, separator->orientation);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_separator_set_property (GObject      *object,
                             unsigned int  prop_id,
                             const GValue *value,
                             GParamSpec   *pspec)
{
  GedaSeparator *separator = GEDA_SEPARATOR (object);
  switch (prop_id) {

    case PROP_ORIENTATION:
      separator->orientation = g_value_get_int (value);
      gtk_widget_queue_resize (GTK_WIDGET (object));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}


static void
geda_separator_class_init(void *class, void *class_data)
{
  /*  (GedaSeparatorClass *class) */
  GObjectClass   *object_class = G_OBJECT_CLASS (class);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (class);

  object_class->finalize     = geda_separator_finalize;
  object_class->set_property = geda_separator_set_property;
  object_class->get_property = geda_separator_get_property;

#if GTK_MAJOR_VERSION < 3

  widget_class->size_request = geda_separator_size_request;
  widget_class->expose_event = geda_separator_expose;

#else

  widget_class->get_preferred_width  = geda_separator_get_preferred_width;
  widget_class->get_preferred_height = geda_separator_get_preferred_height;

  widget_class->draw = geda_separator_draw;

  gtk_widget_class_set_accessible_role (widget_class, ATK_ROLE_SEPARATOR);

#endif

  geda_separator_parent_class = g_type_class_peek_parent (class);

  GParamSpec *params;
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
geda_separator_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaSeparator *separator = (GedaSeparator*)instance;
  GtkWidget     *widget    = GTK_WIDGET (instance);

  gtk_widget_set_has_window (GTK_WIDGET (instance), FALSE);

  separator->orientation = 0;

  widget->requisition.width  = 1;
  widget->requisition.height = widget->style->ythickness;

  if (!separator_hash) {
    separator_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (separator_hash, instance, instance);
}

/*!
 * \brief Function to retrieve GedaSeparator's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaSeparator Type identifier. When
 *  first called, the function registers a #GedaSeparator in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaSeparator and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaSeparator.
 */
GedaType geda_separator_get_type (void)
{
  static volatile GedaType geda_separator_type = 0;

  if (g_once_init_enter (&geda_separator_type)) {

    static const GTypeInfo info = {
      sizeof(GedaSeparatorClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_separator_class_init,       /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaSeparator),
      0,                               /* n_preallocs         */
      geda_separator_instance_init     /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaSeparator");
    type   = g_type_register_static (GTK_TYPE_WIDGET, string, &info, 0);

    g_once_init_leave (&geda_separator_type, type);
  }

  return geda_separator_type;
}

/*!
 * \brief Check if an object is a GedaSeparator
 * \par Function Description
 *  Determines if \a separator is valid by verifying \a separator
 *  is included in the hash table of GedaSeparator objects.
 *
 * \return TRUE if \a separator is a valid GedaSeparator
 */
bool
is_a_geda_separator (GedaSeparator *separator)
{
  if ((separator != NULL) && (separator_hash != NULL)) {
    return g_hash_table_lookup(separator_hash, separator) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Create New GedaSeparator
 * \par Function Description
 * Creates a new #GedaSeparator with the given orientation.
 *
 * \param [in] orientation the separator's orientation
 *
 * Return value: a new #GedaSeparator.
 */
GtkWidget *geda_separator_new (int orientation)
{
  return g_object_new (GEDA_TYPE_SEPARATOR,
                       "orientation", orientation,
                       NULL);
}

GtkWidget *geda_hseparator_new (void)
{
  return g_object_new (GEDA_TYPE_SEPARATOR,
                       "orientation", 0,
                       NULL);
}

GtkWidget *geda_vseparator_new (void)
{
  return g_object_new (GEDA_TYPE_SEPARATOR,
                       "orientation", 1,
                       NULL);
}

/** @} endgroup GedaSeparator */
