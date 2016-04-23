/* -*- C edapangorenderer.h indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-
 *
 * File: edapangorenderer.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedacairo - Rendering gEDA schematics with Cairo
 *
 * Copyright (C) 2010-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#ifndef __EDA_PANGO_RENDERER_H__
#define __EDA_PANGO_RENDERER_H__

#define EDA_TYPE_PANGO_RENDERER (eda_pango_renderer_get_type ())
#define EDA_PANGO_RENDERER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDA_TYPE_PANGO_RENDERER, EdaPangoRenderer))
#define EDA_IS_PANGO_RENDERER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDA_TYPE_PANGO_RENDERER))
#define EDA_PANGO_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDA_TYPE_PANGO_RENDERER), EdaPangoRendererClass)
#define EDA_IS_PANGO_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), EDA_TYPE_PANGO_RENDERER))
#define EDA_PANGO_RENDERER_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDA_TYPE_PANGO_RENDERER, EdaPangoRendererClass))

typedef struct _EdaPangoRenderer EdaPangoRenderer;
typedef struct _EdaPangoRendererClass EdaPangoRendererClass;
typedef struct _EdaPangoRendererData EdaPangoRendererData;

struct _EdaPangoRendererClass
{
  PangoRendererClass parent_class;
};

struct _EdaPangoRenderer
{
  PangoRenderer parent_instance;

  /* Private members */
  EdaPangoRendererData *priv;
};

#ifdef __cplusplus
extern "C" {        /* c++ do not mangle function names */
#endif

GedaType eda_pango_renderer_get_type   (void) GEDA_CONST;
PangoRenderer *eda_pango_renderer_new  (cairo_t *cr) WARN_UNUSED;

void eda_pango_renderer_update         (EdaPangoRenderer *renderer, cairo_t *cr);
void eda_pango_renderer_show_layout    (EdaPangoRenderer *renderer,
                                        PangoLayout *pl);

/* ---------------------------------------- */

typedef struct _EdaPangoAttrOverbar EdaPangoAttrOverbar;
struct _EdaPangoAttrOverbar
{
  PangoAttribute attr;
  bool overbar;
};

PangoAttrClass *eda_pango_attr_overbar_get_class (void) GEDA_CONST;

PangoAttribute *eda_pango_attr_overbar_new       (bool overbar) WARN_UNUSED;

bool            eda_is_pango_attr_overbar        (PangoAttribute *attr);
bool            eda_pango_parse_overbars         (const char *overbar_text,
                                                  int length,
                                                  PangoAttrList **attr_list,
                                                  char **text);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* !__EDA_PANGO_RENDERER_H__ */
