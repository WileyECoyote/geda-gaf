/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_toolbar.c
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

#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_container.h"
#include "../../include/geda_toolbar.h"
#include "../../include/gettext.h"

/**
 * \brief GedaToolbar - A Toolbar Super Class
 * \par
 *  A GedaToolbar is a widget typically used to hold toolbar buttons.
 *  The GedaToolbar is derived from GtkToolbar class. GtkToolbar widgets
 *  ignore their GtkRcStyle->font_desc property, and hence do not enforce
 *  uniformity of labels within the children. GedaToolbar objects have a
 *  "font-size" property and each object that is added to the toolbar is
 *  interrogated and if a label is found, the font size of the label is
 *  set the value of GedaToolbar "font-size" property.
 *
 * \defgroup GedaToolbar Geda Toolbar Object
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

  ((GObjectClass*)geda_toolbar_parent_class)->finalize (object);
}

/*! \internal widget_class->realize */
static void
geda_toolbar_box_realize (GtkWidget *widget)
{
  GdkWindow *window;

  /* Chain up to parent class */
  ((GtkWidgetClass*)geda_toolbar_parent_class)->realize (widget);

  window = geda_get_widget_window (widget);

  gdk_window_set_type_hint (window, GDK_WINDOW_TYPE_HINT_TOOLBAR);
}

/*! \internal widget_class->size_request */
static void
geda_toolbar_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  //GedaToolbar *toolbar = (GedaToolbar*)widget;

  ((GtkWidgetClass*)geda_toolbar_parent_class)->size_request(widget, requisition);
}

static void
geda_toolbar_class_init (void *class, void *class_data)
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

  /*!
   * property "font-size": GedaToolbar::font-size
   * \brief
   *  Controls the point size of the font for labels in child widgets.
   */
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
geda_toolbar_instance_init (GTypeInstance *instance, void *g_class)
{
  if (!toolbar_hash_table) {
    toolbar_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (toolbar_hash_table, instance, instance);
}

/*!
 * \brief Function to retrieve GedaToolbar's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaToolbar Type identifier. When
 *  first called, the function registers a #GedaToolbar in the
 *  GType system to obtain an identifier that uniquely itentifies
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

/* Looks for a label nested in widget and when found, sets the font
 * size property of the widget to the same as GedaToolbar::font-size.
 */
static void geda_toolbar_setup_label(GtkWidget *widget, GedaToolbar *bar)
{
  GtkWidget *box;
  GList     *children;
  GList     *iter;

  box      = gtk_bin_get_child (GTK_BIN(widget));
  children = geda_container_get_children (box);

  /* Loop thru children and look for a label */
  for (iter = children; iter; iter = iter->next) {

    GtkWidget *child = iter->data;

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
  }

  g_list_free(children);

  bar->children = g_list_append(bar->children, widget);
}

/*!
 * \brief Check if an object is a GedaToolbar
 * \par Function Description
 *  Determines if \a toolbar is valid by verifying \a toolbar
 *  is included in the hash table of GedaToolbar objects.
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
geda_toolbar_append_element (GedaToolbar         *toolbar,
                             GedaToolbarChildType type,
                             GtkWidget           *widget,
                             const char          *text,
                             const char          *tooltip_text,
                             const char          *tooltip_private_text,
                             GtkWidget           *icon,
                             GCallback            callback,
                             void                *user_data)
{
  GtkWidget *element;

  element = gtk_toolbar_insert_element ((GtkToolbar*)toolbar,
                                        type,
                                        widget,
                                        text,
                                        tooltip_text,
                                        tooltip_private_text,
                                        (GtkWidget*)icon,
                                        callback,
                                        user_data,
                                        -1);

  geda_toolbar_setup_label (element, toolbar);

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

  button = gtk_toolbar_append_item ((GtkToolbar*)toolbar, text,
                                    tooltip_text, tooltip_private_text,
                                    (GtkWidget*)icon,
                                    G_CALLBACK(callback),
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

    gtk_toolbar_append_widget ((GtkToolbar*)bar, widget, tip_text, tip_private);
  }
}

/*!
 * \brief Create New GedaToolbar
 * \par Function Description
 *  Creates a new #GedaToolbar with the given orientation.
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

int geda_toolbar_get_orientation (GedaToolbar *toolbar)
{
  if (GEDA_IS_TOOLBAR(toolbar)) {
    return toolbar->orientation;
  }

  return -1;
}

void geda_toolbar_set_orientation (GedaToolbar *toolbar, int orientation)
{
  if (GEDA_IS_TOOLBAR(toolbar)) {
    if (orientation != toolbar->orientation) {

      toolbar->orientation = orientation;

      g_object_set (toolbar, "orientation", orientation, NULL);
    }
  }
}

/*!
 * \brief Get the GedaToolbar Style of property
 * \par Function Description
 *  Retrieves the current value of the "toolbar-style" property
 *  of the GedaToolbar.
 *
 * \param [in] toolbar Pointer to GedaToolbar object
 */
GtkToolbarStyle
geda_toolbar_get_style (GedaToolbar *toolbar)
{
  int style;

  g_object_get (toolbar, "toolbar-style", &style, NULL);

  return style;
}

/*!
 * \brief Set the GedaToolbar Style of property
 * \par Function Description
 *  Sets the "toolbar-style" property of the GedaToolbar, which
 *  controls visibility and arrangement of text and icons of the
 *  children. The style should be one of
 *  <DL>
 *    <DT>GTK_TOOLBAR_ICONS</DT>
 *    <DT>GTK_TOOLBAR_TEXT</DT>
 *    <DT>GTK_TOOLBAR_BOTH</DT>
 *    <DT>GTK_TOOLBAR_BOTH_HORIZ</DT>
 *  </DL>
 *
 * \param [in] toolbar Pointer to GedaToolbar object
 * \param [in] style   New toolbar style, see text
 */
void
geda_toolbar_set_style (GedaToolbar *toolbar, GtkToolbarStyle style)
{
  /* Set GtkToolbar style property */
  gtk_toolbar_set_style((GtkToolbar*)toolbar, style);
}

/*!
 * \brief Get Visibility of GedaToolbar tooltips
 * \par Function Description
 *  Retrieves the current value of the "has-tooltip" property
 *  of the GedaToolbar.
 *
 * \param [in] toolbar Pointer to GedaToolbar object
 */
bool
geda_toolbar_get_tooltips (GedaToolbar *toolbar)
{
  bool has_tooltip;
  g_object_get (toolbar, "has-tooltip", &has_tooltip, NULL);
  return has_tooltip;
}

/*!
 * \brief Set Visibility of GedaToolbar tooltips
 * \par Function Description
 *  Sets the "has-tooltip" property of the GedaToolbar and
 *  all children of the toolbar, which controls visibility
 *  of the tooltips.
 *
 * \param [in] toolbar Pointer to GedaToolbar object
 * \param [in] enable  visibility of tooltips
 */
void
geda_toolbar_set_tooltips (GedaToolbar *toolbar, bool enable)
{
  /* Set GedaToolbar property, which does nothing */
  g_object_set (toolbar, "has-tooltip", enable, NULL);

  if (GEDA_IS_TOOLBAR(toolbar)) {

    GList *iter;
    /* Loop thru children and set widget "has-tooltip" property */
    for (iter = toolbar->children; iter; iter = iter->next) {
      GtkWidget *widget = iter->data;
      gtk_widget_set_has_tooltip (widget, enable);
    }
  }
}

/** @} endgroup GedaToolbar */
