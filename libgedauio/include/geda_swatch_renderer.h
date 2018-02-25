/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_swatch_renderer.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2018 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
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

GedaType geda_swatch_renderer_get_type (void) GEDA_CONST;

GedaSwatchRenderer*
geda_swatch_renderer_new ();
