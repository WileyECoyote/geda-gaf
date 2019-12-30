/* -*- C edapangorenderer.c indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-
 *
 * File: edapangorenderer.c
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

#include <config.h>

#include <math.h>
#include <string.h>
#include <glib.h>
#include <glib-object.h>
#include <cairo.h>
#include <pango/pangocairo.h>
#include <geda/geda.h>

#include "../include/edapangorenderer.h"

/* We don't use gettext */
#define _(x) (x)

#define MAGIC_OVERBAR_POS_CONSTANT 0.8

enum {
  PROP_CAIRO_CONTEXT = 1,
};

struct _EdaPangoRendererData
{
  cairo_t *cr;
  bool overbar;
};

static GObject *eda_pango_renderer_constructor (GType type,
                                                unsigned int n_construct_properties,
                                                GObjectConstructParam *construct_params);
static void eda_pango_renderer_set_property    (GObject *object, unsigned int prop_id,
                                                const GValue *value,
                                                GParamSpec *pspec);
static void eda_pango_renderer_get_property    (GObject *object, unsigned int prop_id,
                                                GValue *value, GParamSpec *pspec);
static void eda_pango_renderer_finalize        (GObject *object);

static void eda_pango_renderer_draw_glyphs    (PangoRenderer *renderer,
                                               PangoFont *font,
                                               PangoGlyphString *glyphs,
                                               int x, int y);
static void eda_pango_renderer_draw_rectangle (PangoRenderer *renderer,
                                               PangoRenderPart part,
                                               int x, int y,
                                               int width, int height);
static void eda_pango_renderer_draw_error_underline (PangoRenderer *renderer,
                                                     int x, int y,
                                                     int width, int height);
static void eda_pango_renderer_part_changed (PangoRenderer *renderer,
                                             PangoRenderPart part);
static void eda_pango_renderer_begin        (PangoRenderer *renderer);
static void eda_pango_renderer_end          (PangoRenderer *renderer);
static void eda_pango_renderer_prepare_run  (PangoRenderer *renderer,
                                             PangoLayoutRun *run);

static GObjectClass *eda_pango_renderer_parent_class = NULL;

/* ---------------------------------------- */

static PangoAttribute *eda_pango_attr_overbar_copy (const PangoAttribute *attr);
static bool eda_pango_attr_overbar_compare         (const PangoAttribute *attr1,
                                                    const PangoAttribute *attr2);

/* ---------------------------------------- */

static GObject *
eda_pango_renderer_constructor (GType type,
                                unsigned int n_construct_properties,
                                GObjectConstructParam *construct_params)
{
  GObject      *object;
  GObjectClass *parent_object_class;

  parent_object_class = G_OBJECT_CLASS (eda_pango_renderer_parent_class);
  object = parent_object_class->constructor (type, n_construct_properties,
                                             construct_params);

  object = EDA_IS_PANGO_RENDERER (object) ? object : NULL;

  return object;
}

static void
eda_pango_renderer_set_property (GObject *object, unsigned int property_id,
                                 const GValue *value, GParamSpec *pspec)
{
  EdaPangoRenderer *renderer = EDA_PANGO_RENDERER (object);
  switch (property_id) {
  case PROP_CAIRO_CONTEXT:

    if (renderer->priv->cr != NULL) {
      cairo_destroy (renderer->priv->cr);   /* Is decrement */
    }

    renderer->priv->cr = (cairo_t*)g_value_get_pointer (value);

    if (renderer->priv->cr != NULL) {
      cairo_reference (renderer->priv->cr);
    }
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
eda_pango_renderer_get_property (GObject *object, unsigned int property_id,
                                 GValue *value, GParamSpec *pspec)
{
  EdaPangoRenderer *renderer = EDA_PANGO_RENDERER (object);
  switch (property_id) {
  case PROP_CAIRO_CONTEXT:
    g_value_set_pointer (value, renderer->priv->cr);
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
eda_pango_renderer_finalize (GObject *object)
{
  EdaPangoRenderer *renderer = EDA_PANGO_RENDERER (object);
  G_OBJECT_CLASS (eda_pango_renderer_parent_class)->finalize (object);

  if (renderer->priv->cr != NULL) {
    cairo_destroy (renderer->priv->cr);
  }

  g_free(renderer->priv);
}

static void
eda_pango_renderer_draw_glyphs (PangoRenderer *renderer,
                                PangoFont *font,
                                PangoGlyphString *glyphs,
                                int x, int y)
{
  EdaPangoRenderer *eda_renderer = EDA_PANGO_RENDERER (renderer);
  cairo_t *cr = eda_renderer->priv->cr;

  if (eda_renderer->priv->overbar) {

    PangoFontMetrics *metrics;
    PangoRectangle    glyphs_extents;

    double rx, ry;
    double rwidth;
    double rheight;

    pango_glyph_string_extents (glyphs, font, NULL, &glyphs_extents);

    rx      = x / PANGO_SCALE;
    ry      = (y - glyphs_extents.height * MAGIC_OVERBAR_POS_CONSTANT) / PANGO_SCALE;
    rwidth  = glyphs_extents.width / PANGO_SCALE;

    /* Make the thickness the same as for the font's underline */
    metrics = pango_font_get_metrics (font, NULL);
    rheight = pango_font_metrics_get_underline_thickness (metrics) / PANGO_SCALE;

    pango_font_metrics_unref (metrics);

    /* Allow the overbar to fade out as it becomes < 1px high */
    if (rheight > 1.0) {
      rheight = floor (rheight);
    }

    /* The +1 on width is a hack to ensure hinting doesn't sometimes
     * cause the overbars to be broken by a 1px gap if the overbar
     * spans multiple calls to this function. */
    rwidth += 1.0;

    cairo_rectangle (cr, floor(rx), floor(ry), floor(rwidth), rheight);
    cairo_fill (cr);
  }

  /* Now draw the actual characters */
  cairo_move_to (cr, (double) x / PANGO_SCALE, (double) y / PANGO_SCALE);
  pango_cairo_show_glyph_string (cr, font, glyphs);
}

static void
eda_pango_renderer_draw_rectangle (PangoRenderer *renderer,
                                   PangoRenderPart part,
                                   int x, int y, int width, int height)
{
  EdaPangoRenderer *eda_renderer = EDA_PANGO_RENDERER (renderer);
  cairo_t *cr = eda_renderer->priv->cr;

  cairo_rectangle (cr, (double) x / PANGO_SCALE, (double) y / PANGO_SCALE,
                   (double) width / PANGO_SCALE, (double) height / PANGO_SCALE);
  cairo_fill (cr);
}

static void
eda_pango_renderer_draw_error_underline (PangoRenderer *renderer,
                                   int x, int y, int width, int height)
{
  cairo_t *cr = EDA_PANGO_RENDERER (renderer)->priv->cr;
  pango_cairo_show_error_underline (cr, (double) x / PANGO_SCALE,
                                    (double) y / PANGO_SCALE,
                                    (double) width / PANGO_SCALE,
                                    (double) height / PANGO_SCALE);
}

static void
eda_pango_renderer_part_changed (PangoRenderer *renderer,
                                 PangoRenderPart part)
{
}

static void
eda_pango_renderer_begin (PangoRenderer *renderer)
{
}

static void
eda_pango_renderer_end (PangoRenderer *renderer)
{
}

static void
eda_pango_renderer_prepare_run (PangoRenderer *renderer,
                                PangoLayoutRun *run)
{
  EdaPangoRenderer *eda_renderer = EDA_PANGO_RENDERER (renderer);
  bool overbar = FALSE;
  GSList *l;

  for (l = run->item->analysis.extra_attrs; l != NULL; l = g_slist_next (l)) {

    if (eda_is_pango_attr_overbar ((PangoAttribute*)l->data)) {
      EdaPangoAttrOverbar *attr = (EdaPangoAttrOverbar*)l->data;
      overbar = attr->overbar;
    }
  }

  if (eda_renderer->priv->overbar != overbar) {
    pango_renderer_part_changed (renderer, PANGO_RENDER_PART_FOREGROUND);
    eda_renderer->priv->overbar = overbar;
  }

  PANGO_RENDERER_CLASS (eda_pango_renderer_parent_class)->prepare_run (renderer,
                                                                       run);
}

static void
eda_pango_renderer_class_init(void *g_class, void *class_data)
{
  EdaPangoRendererClass *class         = (EdaPangoRendererClass*)g_class;
  GObjectClass          *object_class  = G_OBJECT_CLASS (class);
  PangoRendererClass    *parent_class  = PANGO_RENDERER_CLASS (class);

  /* Register functions with base class */
  object_class->constructor          = eda_pango_renderer_constructor;
  object_class->set_property         = eda_pango_renderer_set_property;
  object_class->get_property         = eda_pango_renderer_get_property;
  object_class->finalize             = eda_pango_renderer_finalize;

  /* Register functions with parent class */
  parent_class->draw_glyphs          = eda_pango_renderer_draw_glyphs;
  parent_class->draw_rectangle       = eda_pango_renderer_draw_rectangle;
  parent_class->draw_error_underline = eda_pango_renderer_draw_error_underline;
  parent_class->part_changed         = eda_pango_renderer_part_changed;
  parent_class->begin                = eda_pango_renderer_begin;
  parent_class->end                  = eda_pango_renderer_end;
  parent_class->prepare_run          = eda_pango_renderer_prepare_run;

  eda_pango_renderer_parent_class    = g_type_class_peek_parent (class);

  /* Install properties */
  g_object_class_install_property (object_class, PROP_CAIRO_CONTEXT,
                                   g_param_spec_pointer ("cairo-context",
                                                         _("Cairo context"),
                                                         _("The Cairo context for rendering"),
                                                         G_PARAM_READWRITE
                                                         | G_PARAM_STATIC_NAME
                                                         | G_PARAM_STATIC_NICK
                                                         | G_PARAM_STATIC_BLURB));
}

static void
eda_pango_renderer_instance_init (GTypeInstance *instance, void *g_class)
{
  EdaPangoRenderer *renderer = (EdaPangoRenderer*)instance;

  renderer->priv = g_malloc0 (sizeof(EdaPangoRendererData));
}

/*!
 * \brief Retrieve EdaPangoRenderer's Type identifier.
 * \par Function Description
 *  Function to retrieve an #EdaPangoRenderer Type identifier. When
 *  first called, the function registers a #EdaPangoRenderer in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a EdaPangoRenderer and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with EdaPangoRenderer.
 */
GedaType eda_pango_renderer_get_type (void)
{
  static GedaType eda_pango_renderer_type = 0;

  if (g_once_init_enter (&eda_pango_renderer_type)) {

    static const GTypeInfo info = {
      sizeof(EdaPangoRendererClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      eda_pango_renderer_class_init,   /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(EdaPangoRenderer),
      0,                               /* n_preallocs         */
      eda_pango_renderer_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("EdaPangoRenderer");
    type   = g_type_register_static (PANGO_TYPE_RENDERER, string, &info, 0);

    g_once_init_leave (&eda_pango_renderer_type, type);
  }

  return eda_pango_renderer_type;
}

/*!
 * \brief Create a New EdaPangoRenderer
 * \par Function Description
 *  Returns a new EdaPangoRenderer object with the given Cairo Context.
 */
PangoRenderer *eda_pango_renderer_new (cairo_t *cr)
{
  return g_object_new (EDA_TYPE_PANGO_RENDERER, "cairo-context", cr, NULL);
}

/*!
 * \brief Update EdaPangoRenderer Cairo Context
 * \par Function Description
 *  Updates the EdaPangoRenderer object's Cairo Context property.
 */
void eda_pango_renderer_update (EdaPangoRenderer *renderer, cairo_t *cr)
{
  g_object_set (renderer, "cairo-context", cr, NULL);
}

/*!
 * \brief Show the Pango Layout
 * \par Function Description
 *  Draws layout given by \a pl using the PangoRenderer pointed
 *  to in the EdaPangoRenderer.
 *
 * \param [in] renderer EdaPangoRenderer object.
 * \param [in] pl       PangoLayout to be drawn.
 */
void
eda_pango_renderer_show_layout (EdaPangoRenderer *renderer, PangoLayout *pl)
{
  g_return_if_fail (EDA_IS_PANGO_RENDERER (renderer));
  g_return_if_fail (PANGO_IS_LAYOUT (pl));

  PangoRectangle logical_rect;
  int width;
  int height;

  /* Get XY of the left baseline edge from layout coordinates */
  pango_layout_get_extents (pl, NULL, &logical_rect);

  /* Convert USC to Pango units? */
  width  = logical_rect.width / PANGO_SCALE;
  height = logical_rect.height / PANGO_SCALE;

  pango_renderer_draw_layout (PANGO_RENDERER (renderer), pl, width, height);
}

/* ---------------------------------------- */
/*!
 * \internal Make a copy of an EdaPangoAttrOverbar Attribute structure
 * \par Function Description
 *  Copy method for the custom PangoAttrClass EdaPangoAttrOverbar.
 */
static PangoAttribute *
eda_pango_attr_overbar_copy (const PangoAttribute *attr)
{
  const EdaPangoAttrOverbar *a = (const EdaPangoAttrOverbar*)attr;
  return eda_pango_attr_overbar_new (a->overbar);
}

/*!
 * \internal Compare EdaPangoAttrOverbar Attribute structures
 * \par Function Description
 *  Compare method for the custom PangoAttrClass EdaPangoAttrOverbar.
 */
static bool
eda_pango_attr_overbar_compare (const PangoAttribute *attr1,
                                const PangoAttribute *attr2)
{
  const EdaPangoAttrOverbar *a1 = (const EdaPangoAttrOverbar*)attr1;
  const EdaPangoAttrOverbar *a2 = (const EdaPangoAttrOverbar*)attr2;
  return (a1->overbar == a2->overbar);
}

/*! \todo Finish function documentation!!!
 *  \brief Get an EdaPangoAttrOverbar Class
 *  \par Function Description
 */
PangoAttrClass *
eda_pango_attr_overbar_get_class ()
{
  static PangoAttrClass klass = { 0,
                                  eda_pango_attr_overbar_copy,
                                  (void (*)(PangoAttribute *)) g_free,
                                  eda_pango_attr_overbar_compare };

  if (!klass.type) {
    klass.type = pango_attr_type_register ("EdaPangoAttrOverbar");
  }

  return &klass;
}

/*! \todo Finish function documentation!!!
 *  \brief Create a New EdaPangoAttrOverbar
 *  \par Function Description
 *
 */
PangoAttribute *eda_pango_attr_overbar_new (bool overbar)
{
  EdaPangoAttrOverbar *result = g_malloc (sizeof(EdaPangoAttrOverbar));
  result->attr.klass = eda_pango_attr_overbar_get_class ();
  result->overbar = overbar;
  return (PangoAttribute *) result;
}

/*!
 * \brief Determine if an object is an EdaPangoAttrOverbar
 * \par Function Description
 *  Returns TRUE if \a attr is an EdaPangoAttrOverbar object.
 */
bool eda_is_pango_attr_overbar (PangoAttribute *attr)
{
  g_return_val_if_fail (attr != NULL, FALSE);
  g_return_val_if_fail (attr->klass != NULL, FALSE);

  return attr->klass->type == eda_pango_attr_overbar_get_class()->type;
}

/*!
 * \brief Parse overbar_text
 * \par Function Description
 *  Iterates over \a overbar_text and stores the string without the
 *  delimiter to \a text along with a list of PangoAttributes to be
 *  applied to the text to \a attr_list. The attribute list should
 *  be disposed using pango_attr_list_unref once the pango layout
 *  has been set.
 *
 * \param [in]  overbar_text The overbar text to be Parsed
 * \param [in]  length       Lenght of overbar_text or -1 if NULL terminated
 * \param [out] attr_list    Location to store the PangoAttrList
 * \param [out] text         Location to store the output string
 */
bool
eda_pango_parse_overbars (const char *overbar_text,  int    length,
                          PangoAttrList **attr_list, char **text)
{
  char       *out_ptr;
  char       *overbar_start = NULL;
  char       *overbar_end   = NULL;
  const char *escape_start  = NULL;
  const char *in_ptr        = NULL;

  g_return_val_if_fail ((overbar_text != NULL), FALSE);
  g_return_val_if_fail ((attr_list != NULL), FALSE);
  g_return_val_if_fail ((text != NULL), FALSE);

  /* Create the attribute list */
  *attr_list = pango_attr_list_new ();

  /* We know the length of the output will be <= the length of the
   * input text.  So we just allocate a string of the same length.  If
   * length was given as -1, the input should be null-terminated, so
   * just use strlen. */
  if (length == -1) {
    length = strlen (overbar_text);
  }

  *text = g_malloc0 (length + 1);

  out_ptr = *text;

  /* todo: eliminate pointer math */
  for (in_ptr=overbar_text; (in_ptr - overbar_text) <= length; in_ptr++)
  {
    /* If we find an escape character and we are not already in an
     * escaped state, enter escaped state and don't add the current
     * character to the output. */
    if ((*in_ptr == '\\') && !escape_start) {
      escape_start = in_ptr;
      continue;
    }

    /* If the escaped character is '_', this is an overbar delimiter.
     * Enter or exit overbar state if appropriate. Otherwise, simply
     * append the character (which may have been escaped) to the output.
     */
    if (escape_start && (*in_ptr == '_')) {

      if (overbar_start) {
        overbar_end = out_ptr;
      } else {
        overbar_start = out_ptr;
      }
    }
    else {
      /* just append the character to the output */
      *out_ptr++ = *in_ptr;
    }

    escape_start = NULL;

    /* If we've previously found an overbar delimiter, and either we
     * find a null byte or another overbar delimiter, create an
     * overbar attribute for the intervening run of characters. */
    if (overbar_start && (overbar_end || (*in_ptr == '\0'))) {

      /* Create overbar attribute and add to attribute list */
      PangoAttribute *attr = eda_pango_attr_overbar_new (TRUE);
      attr->start_index    = overbar_start - *text;
      attr->end_index      = overbar_end - *text;

      pango_attr_list_insert (*attr_list, attr);

      /* Clear overbar start & end pointers */
      overbar_start = overbar_end = NULL;
    }

    /* If we encounter a null character before we were expecting it,
     * give up anyway. */
    if (*in_ptr == '\0') {
      break;
    }
  }

  return TRUE;
}
