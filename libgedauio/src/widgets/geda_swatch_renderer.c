/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_swatch_renderer.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */
/*!
 * \file geda_swatch_renderer.c
 *
 * \brief A cell renderer for color swatches.
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include <gtk/gtk.h>

#include "../../include/geda_swatch_renderer.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/* The width of the border around the swatch, in pixels. */
#define SWATCH_BORDER_WIDTH (1.0)

/**
 * \brief GedaSwatchRenderer - A Button Widget for Color Swatches
 * \par
 * A GedaSwatchRenderer is a variant of GtkCellRendererText use for
 * redendering color swatches.
 *
 * \defgroup GedaSwatchRenderer Swatch Renderer
 * @{
 */

enum
{
  PROP_COLOR = 1,
  PROP_ENABLED
};

static void
get_property (GObject      *object,
              unsigned int  param_id,
              GValue       *value,
              GParamSpec   *pspec);

static void
set_property (GObject      *object,
              unsigned int  param_id,
              const GValue *value,
              GParamSpec   *pspec);

static void
render (GtkCellRenderer      *cell,
        GdkWindow            *window,
        GtkWidget            *widget,
        GdkRectangle         *background_area,
        GdkRectangle         *cell_area,
        GdkRectangle         *expose_area,
        GtkCellRendererState flags);


/*! \private
 *  \brief Get a property.
 *
 *  \brief [in]  object   The object with the property
 *  \brief [in]  param_id The id of the property
 *  \brief [out] value    The value of the property
 *  \brief [in]  pspec    The property param spec
 */
static void
get_property (GObject      *object,
              unsigned int  param_id,
              GValue       *value,
              GParamSpec   *pspec)
{
  GedaSwatchRenderer *swatch = GEDA_SWATCH_RENDERER (object);

  switch (param_id) {
    case PROP_COLOR: {

        GdkColor color;

        color.red   = swatch->color.red;
        color.green = swatch->color.green;
        color.blue  = swatch->color.blue;

        g_value_set_boxed (value, &color);
      }
      break;

    case PROP_ENABLED:
      g_value_set_boolean (value, swatch->enabled);

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
}

/*! \brief Render the swatch into the cell
 *  \par Function Description
 *
 *  \param [in] cell
 *  \param [in] window
 *  \param [in] widget
 *  \param [in] background_area
 *  \param [in] cell_area
 *  \param [in] expose_area
 *  \param [in] flags
 */
static void
render (GtkCellRenderer      *cell,
        GdkWindow            *window,
        GtkWidget            *widget,
        GdkRectangle         *background_area,
        GdkRectangle         *cell_area,
        GdkRectangle         *expose_area,
        GtkCellRendererState flags)
{
  GedaSwatchRenderer *swatch = GEDA_SWATCH_RENDERER (cell);

  if (swatch->enabled) {
    cairo_t *cr = gdk_cairo_create (window);
    double offset = SWATCH_BORDER_WIDTH / 2.0;

    if (expose_area) {
      gdk_cairo_rectangle (cr, expose_area);
      cairo_clip (cr);
    }

    cairo_move_to (cr,
                   (double) cell_area->x + offset,
                   (double) cell_area->y + offset);

    cairo_line_to (cr,
                   (double) cell_area->x + (double) cell_area->width - offset,
                   (double) cell_area->y + offset);

    cairo_line_to (cr,
                   (double) cell_area->x + (double) cell_area->width - offset,
                   (double) cell_area->y + (double) cell_area->height - offset);

    cairo_line_to (cr,
                   (double) cell_area->x + offset,
                   (double) cell_area->y + (double) cell_area->height - offset);

    cairo_close_path (cr);

    cairo_set_line_width (cr, SWATCH_BORDER_WIDTH);

    cairo_set_source_rgb (cr,
                          swatch->color.red   / 65535.0,
                          swatch->color.green / 65535.0,
                          swatch->color.blue  / 65535.0);

    cairo_fill_preserve (cr);

    cairo_set_source_rgb (cr, 0.0, 0.0, 0.0);

    cairo_stroke (cr);

    cairo_destroy (cr);
  }
}


/*! \private
 *  \brief Set the swatch color.
 *  \par Function Description
 *
 *  \param [in,out] swatch The swatch cell renderer
 *  \param [in]     color  The color of the swatch
 */
static void
set_color (GedaSwatchRenderer *swatch, const GdkColor *color)
{
  if (color) {
    swatch->color.red = color->red;
    swatch->color.green = color->green;
    swatch->color.blue = color->blue;
  }
}


/*! \private
 *  \brief Set a property.
 *  \par Function Description
 *
 *  \brief [in,out] object   The object with the property
 *  \brief [in]     param_id The id of the property
 *  \brief [in]     value    The value of the property
 *  \brief [in]     pspec    The property param spec
 */
static void
set_property (GObject      *object,
              unsigned int  param_id,
              const GValue *value,
              GParamSpec   *pspec)
{
  GedaSwatchRenderer *swatch = GEDA_SWATCH_RENDERER (object);

  switch (param_id) {
    case PROP_COLOR:
      set_color (swatch, g_value_get_boxed (value));
      break;

    case PROP_ENABLED:
      swatch->enabled =  g_value_get_boolean (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
}

/*! \brief Initialize swatch cell renderer class
 *  \par Function Description
 *  Called to initialize the class instance.
 *
 * \param [in] class A GedaSwatchRendererClass Object
 * \param [in] data  A GedaSwatchRenderer data structure
 */
static void
swatch_renderer_class_init (void *class, void *data)
{
  GObjectClass *object_class;
  GParamSpec   *params;

  GedaSwatchRendererClass *swatch = (GedaSwatchRendererClass*)class;

  object_class = (GObjectClass*)class;

  swatch->parent_class.parent_class.render = render;

  object_class->get_property = get_property;
  object_class->set_property = set_property;

  params = g_param_spec_boxed ("color",
                             _("Swatch Color"),
                             _("Swatch Color"),
                               GDK_TYPE_COLOR,
                               G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_COLOR, params);

  params = g_param_spec_boolean ("enabled",
                               _("Swatch Enabled"),
                               _("Swatch Enabled"),
                                 TRUE,                /* default value */
                                 G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_ENABLED, params);
}

/*! \brief Initialize new GedaSwatchRenderer data structure instance.
 *
 *  \par Function Description
 *  This function is call after the #GedaSwatchRendererClass is created
 *  to initialize the data structure.
 *
 *  \param [in,out] instance A GedaSwatchRenderer data structure
 *  \param [in]     class    A GedaSwatchRendererClass Object
 */
static void
swatch_renderer_instance_init (GTypeInstance *instance, void *class)
{
  GedaSwatchRenderer *swatch = (GedaSwatchRenderer*)instance;

  swatch->color.red   = 0;
  swatch->color.green = 0;
  swatch->color.blue  = 0;

  swatch->enabled = TRUE;
}

/*! \brief Function to retrieve GedaSwatchRenderer's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaSwatchRenderer Type identifier. When
 *  first called, the function registers a #GedaSwatchRenderer in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaSwatchRenderer and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaSwatchRenderer.
 */
GedaType geda_swatch_renderer_get_type (void)
{
  static volatile GedaType swatch_renderer_type = 0;

  if (g_once_init_enter (&swatch_renderer_type)) {

    static const GTypeInfo info = {
      sizeof(GedaSwatchRendererClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      swatch_renderer_class_init,      /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaSwatchRenderer),
      0,                               /* n_preallocs         */
      swatch_renderer_instance_init    /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaSwatchRenderer");
    type   = g_type_register_static (GTK_TYPE_CELL_RENDERER_TEXT, string, &info, 0);

    g_once_init_leave (&swatch_renderer_type, type);
  }

  return swatch_renderer_type;
}

/*! \brief Create a swatch cell renderer
 */
GedaSwatchRenderer*
geda_swatch_renderer_new(void)
{
  GedaSwatchRenderer *swatch = g_object_new (GEDA_TYPE_SWATCH_RENDERER, NULL);

  return swatch;
}
/** @} endgroup GedaSwatchRenderer */