/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */
/*!
 * \file geda_swatch_renderer.h
 *
 * \brief A cell renderer for color swatches.
 */

#define GEDA_TYPE_SWATCH_RENDERER          (geda_swatch_renderer_get_type())
#define GEDA_SWATCH_RENDERER(obj)          (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_SWATCH_RENDERER, GedaSwatchRenderer))
#define GEDA_SWATCH_RENDERER_CLASS(klass)  (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_SWATCH_RENDERER, GedaSwatchRendererClass))
#define IS_GEDA_SWATCH_RENDERER(obj)       (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEDA_TYPE_SWATCH_RENDERER))

typedef struct _GedaSwatchRendererClass GedaSwatchRendererClass;
typedef struct _GedaSwatchRenderer GedaSwatchRenderer;

struct _GedaSwatchRendererClass
{
  GtkCellRendererTextClass parent_class;
};

struct _GedaSwatchRenderer
{
  GtkCellRendererText parent;

  GdkColor color;
  bool enabled;
};

GType
geda_swatch_renderer_get_type ();

GedaSwatchRenderer*
geda_swatch_renderer_new ();
