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
 * \file gschem_swatch_renderer.h
 *
 * \brief A cell renderer for color swatches.
 */

#define GSCHEM_TYPE_SWATCH_RENDERER           (gschem_swatch_renderer_get_type())
#define GSCHEM_SWATCH_RENDERER(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_SWATCH_RENDERER, GschemSwatchRenderer))
#define GSCHEM_SWATCH_RENDERER_CLASS(klass)  (G_TYPE_CHECK_CLASS_CAST ((klass), GSCHEM_TYPE_SWATCH_RENDERER, GschemSwatchRendererClass))
#define IS_GSCHEM_SWATCH_RENDERER(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_SWATCH_RENDERER))

typedef struct _GschemSwatchRendererClass GschemSwatchRendererClass;
typedef struct _GschemSwatchRenderer GschemSwatchRenderer;

struct _GschemSwatchRendererClass
{
  GtkCellRendererTextClass parent_class;
};

struct _GschemSwatchRenderer
{
  GtkCellRendererText parent;

  GdkColor color;
  bool enabled;
};

GType
gschem_swatch_renderer_get_type ();

GschemSwatchRenderer*
gschem_swatch_renderer_new ();
