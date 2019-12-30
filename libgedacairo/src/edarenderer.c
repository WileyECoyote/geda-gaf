/* -*- C edarenderer.c indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-
 *
 * File: edarenderer.c
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
#include <glib.h>
#include <glib-object.h>
#include <gdk/gdk.h>
#include <cairo.h>
#include <pango/pangocairo.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include <libgeda/libgeda.h>

#include "../include/edacairo.h"
#include "../include/edarenderer.h"
#include "../include/edapangorenderer.h"

/* We do not use gettext */
#define _(x) (x)

#define EdaFontOptions renderer->priv->font_options

enum {
  PROP_CAIRO_CONTEXT = 1,
  PROP_PANGO_CONTEXT,
  PROP_FONT_NAME,
  PROP_COLOR_MAP,
  PROP_OVERRIDE_COLOR,
  PROP_DRAW_GRIPS,
  PROP_GRIP_SIZE,
  PROP_GRIP_STROKE,
  PROP_GRIP_FILL,
  PROP_CIRCLE_GRIP_QUAD,
  PROP_JUNCTION_COLOR,
  PROP_JUNCTION_SIZE,
  PROP_ENDPOINT_COLOR,
  PROP_MARKER_COLOR,
  PROP_TEXT_MARKER_SIZE,
  PROP_TEXT_MARKER_THLD,

  PROP_RENDER_FLAGS,

  FLAG_HINTING         = EDA_RENDERER_FLAG_HINTING,
  FLAG_PICTURE_OUTLINE = EDA_RENDERER_FLAG_PICTURE_OUTLINE,
  FLAG_TEXT_HIDDEN     = EDA_RENDERER_FLAG_TEXT_HIDDEN,
  FLAG_TEXT_OUTLINE    = EDA_RENDERER_FLAG_TEXT_OUTLINE,
  FLAG_TEXT_ORIGIN     = EDA_RENDERER_FLAG_TEXT_ORIGIN,

  EDAR_GRIP_SQUARE,
  EDAR_GRIP_CIRCLE,
};

struct _EdaRendererData
{
  cairo_t          *cr;
  PangoContext     *pc;
  PangoLayout      *pl;
  EdaPangoRenderer *pr;
  int pc_from_cr;

  int               flags;
  char             *font_name;
  int               override_color;

  GArray           *color_map;

  cairo_font_options_t *font_options;

  /* Cache of font metrics for different font sizes. */
  //GHashTable *metrics_cache;
};

static inline bool EDA_RENDERER_CHECK_FLAG (EdaRenderer *r, int f)
{
  return r->priv->flags & f;
}
static inline void
EDA_RENDERER_SET_FLAG (EdaRenderer *r, int f, bool e) {
  if (e) { r->priv->flags |= f; } else { r->priv->flags &= ~f; }
}
static inline int
EDA_RENDERER_CAIRO_FLAGS (EdaRenderer *r) {
  return EDA_RENDERER_CHECK_FLAG (r, FLAG_HINTING) ? EDA_CAIRO_ENABLE_HINTS : 0;
}

static inline double
EDA_RENDERER_STROKE_WIDTH (EdaRenderer *r,  double line_width) {
  return fmax (line_width, MIN_LINE_WIDTH_THRESHOLD);
}

static GObject *eda_renderer_constructor (GType type,
                                          unsigned int n_construct_properties,
                                          GObjectConstructParam *construct_params);

static void eda_renderer_dispose         (GObject *object);
static void eda_renderer_finalize        (GObject *object);
static void eda_renderer_set_property    (GObject *object, unsigned int property_id,
                                          const GValue *value, GParamSpec *pspec);
static void eda_renderer_get_property    (GObject *object, unsigned int property_id,
                                          GValue *value, GParamSpec *pspec);
static void eda_renderer_update_contexts (EdaRenderer *renderer, cairo_t *new_cr,
                                          PangoContext *new_pc);

static void eda_renderer_set_color    (EdaRenderer *renderer, int color);
static int  eda_renderer_is_colorable (EdaRenderer *renderer, int color);
static int  eda_renderer_is_drawable  (EdaRenderer *renderer, GedaObject *object);
static int  eda_renderer_draw_hatch   (EdaRenderer *renderer, GedaObject *object);

static void eda_renderer_default_draw (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_list    (EdaRenderer *renderer, GList      *objects);
static void eda_renderer_draw_line    (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_pin     (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_net     (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_bus     (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_box     (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_arc     (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_circle  (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_path    (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_text    (EdaRenderer *renderer, GedaObject *object);

static int  eda_renderer_prepare_text       (EdaRenderer *renderer, const GedaObject *object);
static void eda_renderer_calc_text_position (EdaRenderer *renderer, const GedaObject *object,
                                             int descent, double *x, double *y);
static void eda_renderer_draw_picture (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_complex (EdaRenderer *renderer, GedaObject *object);

static void eda_renderer_default_draw_grips (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_grips_impl    (EdaRenderer *renderer, int type, int n_grips, ...);
static void eda_renderer_draw_arc_grips     (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_path_grips    (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_text_grips    (EdaRenderer *renderer, GedaObject *object);

static void eda_renderer_draw_junction_cue  (EdaRenderer *renderer, int x, int y,
                                             double width);
static void eda_renderer_draw_mid_cues      (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_end_cues      (EdaRenderer *renderer, GedaObject *object,
                                             int end);
static void eda_renderer_default_draw_cues  (EdaRenderer *renderer, GedaObject *object);
static void eda_renderer_draw_cues_list     (EdaRenderer *renderer, GList      *objects);

static int eda_renderer_default_get_user_bounds (EdaRenderer      *renderer,
                                                 const GedaObject *object,
                                                 int *left,   int *top,
                                                 int *right,  int *bottom);

static GObjectClass *eda_renderer_parent_class = NULL;

static GObject*
eda_renderer_constructor(GType type,
                         unsigned int n_construct_properties,
                         GObjectConstructParam *construct_params)
{
  GObject *object;
  GObjectClass *parent_object_class;

  parent_object_class = G_OBJECT_CLASS (eda_renderer_parent_class);
  object = parent_object_class->constructor (type, n_construct_properties,
                                             construct_params);

  return object;
}

static void
eda_renderer_dispose (GObject *object)
{
  EdaRenderer *renderer = (EdaRenderer *) object;

  if (renderer->priv->pc != NULL) {
    if (G_IS_OBJECT(renderer->priv->pc)) {
      pango_cairo_context_set_font_options (renderer->priv->pc, NULL);

#if (PANGO_VERSION_MAJOR == 1) && (PANGO_VERSION_MINOR < 34)
      pango_context_set_font_map(renderer->priv->pc, NULL);
#endif

      GEDA_UNREF (renderer->priv->pc);
    }
    renderer->priv->pc = NULL;
  }

  if (renderer->priv->pl != NULL) {
    if (G_IS_OBJECT(renderer->priv->pl)) {
      GEDA_UNREF (renderer->priv->pl);
    }
    renderer->priv->pl = NULL;
  }

  if (renderer->priv->pr != NULL) {
    if (G_IS_OBJECT(renderer->priv->pr)) {
      GEDA_UNREF (renderer->priv->pr);
    }
    renderer->priv->pr = NULL;
  }

  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_renderer_parent_class)->dispose (object);
}

static void
eda_renderer_finalize (GObject *object)
{
  EdaRenderer *renderer = (EdaRenderer *) object;
/*
  g_hash_table_destroy (renderer->priv->metrics_cache);
  renderer->priv->metrics_cache = NULL;
*/
  cairo_destroy (renderer->priv->cr);
  renderer->priv->cr = NULL;

  if (EdaFontOptions != NULL) {
    cairo_font_options_destroy(EdaFontOptions);
    EdaFontOptions = NULL;
  }

  GEDA_FREE (renderer->priv->font_name);

  GEDA_FREE (renderer->priv);

  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_renderer_parent_class)->finalize (object);
}

static void
eda_renderer_set_property (GObject *object, unsigned int property_id,
                           const GValue *value, GParamSpec *pspec)
{
  EdaRenderer *renderer = EDA_RENDERER (object);

  switch (property_id) {
  case PROP_CAIRO_CONTEXT:
    eda_renderer_update_contexts (renderer,
                                  (cairo_t*)g_value_get_pointer (value),
                                  NULL);
    break;

  case PROP_PANGO_CONTEXT:
    eda_renderer_update_contexts (renderer, NULL,
                                  PANGO_CONTEXT(g_value_get_pointer (value)));
    break;

  case PROP_FONT_NAME:
    eda_renderer_set_font_name(renderer, g_value_get_string(value));
    /* Clear font metrics cache */
    //g_hash_table_remove_all (renderer->priv->metrics_cache);
    break;

  case PROP_COLOR_MAP:
    renderer->priv->color_map      = g_value_get_pointer (value);
    break;

  case PROP_OVERRIDE_COLOR:
    renderer->priv->override_color = g_value_get_int (value);
    break;

  case PROP_DRAW_GRIPS:
    eda_renderer_set_draw_grips(renderer, g_value_get_int (value));
    break;

  case PROP_GRIP_SIZE:
    EDAR_GRIP_SIZE                 = g_value_get_double (value);
    break;

  case PROP_GRIP_STROKE: /* Grip Stroke Color */
    eda_renderer_set_grips_stroke_color(renderer, g_value_get_boxed (value));
    break;

  case PROP_GRIP_FILL:  /* Grip Fill Color */
    eda_renderer_set_grips_fill_color (renderer, g_value_get_boxed (value));
    break;

  case PROP_CIRCLE_GRIP_QUAD:
    EDAR_CIRCLE_GRIP_QUAD = g_value_get_int (value);
    break;

  case PROP_JUNCTION_COLOR:
    eda_renderer_set_junction_color (renderer, g_value_get_boxed (value));
    break;

  case PROP_JUNCTION_SIZE:
    EDAR_JUNCTION_SIZE             = g_value_get_int (value);
    break;

  case PROP_ENDPOINT_COLOR:
    eda_renderer_set_net_endpoint_color (renderer, g_value_get_boxed (value));
    break;

  case PROP_MARKER_COLOR:  /* Marker Stroke Color */
    eda_renderer_set_text_marker_color (renderer, g_value_get_boxed (value));
    break;

  case PROP_TEXT_MARKER_SIZE:
    EDAR_TEXT_MARKER_SIZE          = g_value_get_int (value);
    break;

  case PROP_TEXT_MARKER_THLD:
    EDAR_MARKER_THRESHOLD          = g_value_get_int (value);
    break;

  case PROP_RENDER_FLAGS:
    renderer->priv->flags          = g_value_get_int (value);
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
eda_renderer_get_property (GObject *object, unsigned int property_id,
                           GValue *value, GParamSpec *pspec)
{
  EdaRenderer *renderer = EDA_RENDERER (object);

  switch (property_id) {
  case PROP_CAIRO_CONTEXT:
    g_value_set_pointer (value, renderer->priv->cr);
    break;

  case PROP_PANGO_CONTEXT:
    g_value_set_pointer (value, renderer->priv->pc);
    break;
  case PROP_FONT_NAME:
    g_value_set_string (value, renderer->priv->font_name);
    break;

  case PROP_COLOR_MAP:
    g_value_set_pointer (value, renderer->priv->color_map);
    break;

  case PROP_OVERRIDE_COLOR:
    g_value_set_int (value, renderer->priv->override_color);
    break;

  case PROP_DRAW_GRIPS:
    g_value_set_int (value, eda_renderer_get_draw_grips(renderer));
    break;

  case PROP_GRIP_SIZE:
    g_value_set_double (value, EDAR_GRIP_SIZE);
    break;

  case PROP_GRIP_STROKE: /* Grip Stroke Color */
    g_value_set_boxed (value, &EDAR_GRIP_STROKE_COLOR);
    break;

  case PROP_GRIP_FILL: /* Grip Fill Color */
    g_value_set_boxed (value, &EDAR_GRIP_FILL_COLOR);
    break;

  case PROP_CIRCLE_GRIP_QUAD:
    g_value_set_int (value, EDAR_CIRCLE_GRIP_QUAD);
    break;

  case PROP_JUNCTION_COLOR:
    g_value_set_boxed (value, &EDAR_JUNCTION_COLOR);
    break;

  case PROP_JUNCTION_SIZE:
    g_value_set_int (value, EDAR_JUNCTION_SIZE);
    break;

  case PROP_ENDPOINT_COLOR:
    g_value_set_boxed (value, &EDAR_NET_ENDPOINT_COLOR);
    break;

  case PROP_MARKER_COLOR:  /* Marker Stroke Color */
    g_value_set_boxed (value, &EDAR_TEXT_MARKER_COLOR);
    break;

  case PROP_TEXT_MARKER_SIZE:
    g_value_set_int (value, EDAR_TEXT_MARKER_SIZE);
    break;

  case PROP_TEXT_MARKER_THLD:
    g_value_set_double (value, EDAR_MARKER_THRESHOLD);
    break;

  case PROP_RENDER_FLAGS:
    g_value_set_int (value, renderer->priv->flags);
    break;

  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
eda_renderer_update_contexts (EdaRenderer  *renderer,
                              cairo_t      *new_cr,
                              PangoContext *new_pc)
{

  /* First figure out what's invalidated */
  if (new_cr != NULL) {

    if (renderer->priv->cr) {
     cairo_destroy (renderer->priv->cr);
    }
    renderer->priv->cr = cairo_reference (new_cr);
  }
  else if (new_pc != NULL) {
    if (renderer->priv->pc != NULL) {
      pango_cairo_context_set_font_options (renderer->priv->pc, NULL);
      GEDA_UNREF (G_OBJECT (renderer->priv->pc));
      renderer->priv->pc = NULL;
    }
    if (PANGO_IS_LAYOUT(renderer->priv->pl)) {
      GEDA_UNREF (G_OBJECT (renderer->priv->pl));
      renderer->priv->pl = NULL;
    }
    renderer->priv->pc = g_object_ref (new_pc);
    renderer->priv->pc_from_cr = 0;
  }

  /* Now recreate anything necessary */
  if ((renderer->priv->pc == NULL) && (renderer->priv->cr != NULL)) {
    renderer->priv->pc = pango_cairo_create_context (renderer->priv->cr);
    pango_cairo_context_set_font_options (renderer->priv->pc, EdaFontOptions);
    renderer->priv->pc_from_cr = 1;
  }
  else if (renderer->priv->cr != NULL) {
    pango_cairo_update_context(new_cr, renderer->priv->pc);
  }

  if (renderer->priv->pc != NULL) {
    pango_cairo_context_set_resolution (renderer->priv->pc, 1000);
  }

  if ((renderer->priv->pl == NULL) && (renderer->priv->pc != NULL)) {
    renderer->priv->pl = pango_layout_new (renderer->priv->pc);
  }

  if ((renderer->priv->pr == NULL) && (renderer->priv->cr != NULL)) {
    renderer->priv->pr =
      (EdaPangoRenderer*)eda_pango_renderer_new (renderer->priv->cr);
  }
  else if (renderer->priv->cr != NULL) {
    eda_pango_renderer_update(renderer->priv->pr, renderer->priv->cr);
  }
}

/*!
 * \brief Return RGB color from string color name
 * \par Function Description
 *  Parses a textual specification of a color and fill in the <b>red</b>,
 *  <b>green</b>, and <b>blue</b> fields of a GdkColor structure. The color
 *  is <em>not</em> allocated. The string can be either one of the standard
 *  names (Taken from the X11 <b>rgb.txt</b> file), or a hex
 *  value in the form 'rgb' 'rrggbb' 'rrrgggbbb' or 'rrrrggggbbbb' where 'r',
 *  'g' and 'b' are hex digits of the red, green, and blue components of the
 *  color, respectively. (White in the four forms is 'fff' 'ffffff' 'fffffffff'
 *  and 'ffffffffffff')
 *  This function implements similar functionality to u_color_rgba_decode
 *  and gdk_color_parse() but accept a pointer to a <b>GEDA COLOR</b>structure
 *  as an argument but does not fully support the alpha channel. The alpha
 *  value is always set to 0xff.
 *
 * \param [in] spec  Pointer to string hex representation of color to parse
 * \param [in] color GEDA Color structure to receive RGB values.
 *
 * \returns TRUE on success, otherwise FALSE.
 */
bool
eda_renderer_parse_color (const char *spec, COLOR *color)
{
  PangoColor pango_color;
  bool result;

  if (pango_color_parse (&pango_color, spec)) {

      color->r = pango_color.red;
      color->g = pango_color.green;
      color->b = pango_color.blue;
      color->a = 0xff;

      result = TRUE;
  }
  else {
    result = FALSE;
  }
  return result;
}

/* ================================================================
 * Object DRAWING
 * ================================================================ */

static void
eda_renderer_draw_list (EdaRenderer *renderer, GList *objects)
{
  GList *iter;

  for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
    eda_renderer_draw (renderer, (GedaObject*)iter->data);
  }
}

/*!
 * \brief Draw GedaObject using Cairo Renderer
 * \par Function Description
 *  Wrapper that calls the virtual eda_renderer_draw_xxxxx routine
 *  associated with the GedaObject.
 *
 * \param [in] renderer Pointer to a EdaRenderer object.
 * \param [in] object   Pointer to a GedaObject.
 */
void
eda_renderer_draw (EdaRenderer *renderer, GedaObject *object)
{
  g_return_if_fail (EDA_IS_RENDERER(renderer));
  g_return_if_fail (object != NULL);
  EDA_RENDERER_GET_CLASS (renderer)->draw (renderer, object);
}

static void
eda_renderer_default_draw (EdaRenderer *renderer, GedaObject *object)
{
  g_return_if_fail (renderer->priv->cr != NULL);

  if (renderer->priv->color_map != NULL) {

    void (*draw_func)(EdaRenderer *, GedaObject *);

    if (!eda_renderer_is_drawable (renderer, object)) return;

    switch (object->type) {
      case OBJ_TEXT:        draw_func = eda_renderer_draw_text; break;
      case OBJ_COMPLEX:     draw_func = eda_renderer_draw_complex; break;
      case OBJ_PIN:         draw_func = eda_renderer_draw_pin; break;
      case OBJ_NET:         draw_func = eda_renderer_draw_net; break;
      case OBJ_BOX:         draw_func = eda_renderer_draw_box; break;
      case OBJ_CIRCLE:      draw_func = eda_renderer_draw_circle; break;
      case OBJ_LINE:        draw_func = eda_renderer_draw_line; break;
      case OBJ_ARC:         draw_func = eda_renderer_draw_arc; break;
      case OBJ_PATH:        draw_func = eda_renderer_draw_path; break;
      case OBJ_BUS:         draw_func = eda_renderer_draw_bus; break;
      case OBJ_PICTURE:     draw_func = eda_renderer_draw_picture; break;
      case OBJ_PLACEHOLDER: draw_func = eda_renderer_draw_complex; break;
      default:
        BUG_IMSG("unhandled case", object->type);
        return;
    }

    eda_renderer_set_color (renderer, object->color);
    draw_func (renderer, object);
  }
  else {

    /* We use a static here so we do not flood the console with so many
     * error, all saying the same thing, No color map, that everything
     * else is scrolled beyond the consoles buffer = vary annoying */
    static int color_map_error = 0;

    if (!color_map_error)
      BUG_MSG("renderer->priv->color_map != NULL");
      color_map_error++;
  }
}

static void
eda_renderer_set_color (EdaRenderer *renderer, int color)
{
  if (renderer->priv->override_color != -1) {
    color = renderer->priv->override_color;
  }

  if (color == -1) {
    BUG_IMSG("color = %d\n", color);
  }
  else {

    cairo_t *cr = renderer->priv->cr;

    eda_cairo_set_source_color (cr, color, renderer->priv->color_map);
  }
}

static int
eda_renderer_is_colorable (EdaRenderer *renderer, int color)
{
  GArray *map = renderer->priv->color_map;

  /* Check for override color */
  if (renderer->priv->override_color >= 0) {
    color = renderer->priv->override_color;
  }

  /* Do not draw if color index out of bounds of the color map */
  g_return_val_if_fail ((map != NULL), FALSE);
  g_return_val_if_fail ((color >= 0) || (color < map->len), FALSE);

  /* Otherwise, return enabled flag of object's color */
  return (&g_array_index (map, COLOR, color))->enabled;
}

static int
eda_renderer_is_drawable (EdaRenderer *renderer, GedaObject *object)
{
  int color = object->color;
  /* Always attempt to draw complex objects */
  if ((object->type == OBJ_COMPLEX) || (object->type == OBJ_PLACEHOLDER)) {
    return TRUE;
  }
  return eda_renderer_is_colorable (renderer, color);
}

static int
eda_renderer_draw_hatch (EdaRenderer *renderer, GedaObject *object)
{
  bool result;

  g_return_val_if_fail ((object->type == OBJ_ARC    ||
                         object->type == OBJ_BOX    ||
                         object->type == OBJ_CIRCLE ||
                         object->type == OBJ_PATH),
                         FALSE);
  result = FALSE;

  if (object->fill_options->fill_type != FILLING_HOLLOW) {

    if (object->fill_options->fill_type != FILL_SOLID) {

      cairo_t *cr = renderer->priv->cr;

      GArray *fill_lines;
      int     fill_width;
      int     index;

      fill_lines = geda_math_hatch_object(object);
      fill_width = object->fill_options->fill_width;

      /* Draw fill pattern */
      for (index = 0; index < fill_lines->len; index++) {

        LINE *line = &g_array_index (fill_lines, LINE, index);

        eda_cairo_line (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        END_NONE, fill_width,
                        line->x[0], line->y[0], line->x[1], line->y[1]);
      }

      eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        TYPE_SOLID, END_NONE,
                        EDA_RENDERER_STROKE_WIDTH (renderer, fill_width), -1, -1);

      g_array_free (fill_lines, TRUE);
    }
    else {
      result = TRUE;
    }
  }

  return result;
}

static void
eda_renderer_draw_complex (EdaRenderer *renderer, GedaObject *object)
{
  /* Recurse */
  eda_renderer_draw_list (renderer, object->complex->prim_objs);
}

static void
eda_renderer_draw_line (EdaRenderer *renderer, GedaObject *object)
{
  GedaLine *line = GEDA_LINE(object);
  LINE_OPTIONS *line_options = &line->line_options;

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  line_options->line_end, line_options->line_width,
                  line->x[0], line->y[0], line->x[1], line->y[1]);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    line_options->line_type,
                    line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, line_options->line_width),
                                                         line_options->line_length,
                                                         line_options->line_space);
}

static void
eda_renderer_draw_net (EdaRenderer *renderer, GedaObject *object)
{
  GedaLine *line = GEDA_LINE(object);
  LINE_OPTIONS *line_options = &line->line_options;

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, line_options->line_width,
                  line->x[0], line->y[0], line->x[1], line->y[1]);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_SQUARE,
                    EDA_RENDERER_STROKE_WIDTH (renderer,
                                               line_options->line_width),
                    -1, -1);
}

static void
eda_renderer_draw_bus (EdaRenderer *renderer, GedaObject *object)
{
  GedaLine     *line = GEDA_LINE(object);
  LINE_OPTIONS *line_options = &line->line_options;

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, line_options->line_width,
                  line->x[0], line->y[0], line->x[1], line->y[1]);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_ROUND,
                    EDA_RENDERER_STROKE_WIDTH (renderer,
                                               line_options->line_width),
                    -1, -1);
}

static void
eda_renderer_draw_pin (EdaRenderer *renderer, GedaObject *object)
{
  GedaLine *line = GEDA_LINE(object);
  LINE_OPTIONS *line_options = &line->line_options;

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, line_options->line_width,
                  line->x[0], line->y[0], line->x[1], line->y[1]);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_SQUARE,
                    EDA_RENDERER_STROKE_WIDTH (renderer,
                                               line_options->line_width),
                    -1, -1);
}

static void
eda_renderer_draw_arc (EdaRenderer *renderer, GedaObject *object)
{
  LINE_OPTIONS *line_options = object->line_options;

  /* Hatch arc */
  int fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of arc */

  eda_cairo_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 line_options->line_width,
                 object->arc->x,
                 object->arc->y,
                 object->arc->radius,
                 object->arc->start_angle,
                 object->arc->arc_sweep);

  if (fill_solid) {
    cairo_fill_preserve (renderer->priv->cr);
  }

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    line_options->line_type,
                    line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, line_options->line_width),
                    line_options->line_length,
                    line_options->line_space);
}

static void
eda_renderer_draw_box (EdaRenderer *renderer, GedaObject *object)
{
  cairo_t *cr = renderer->priv->cr;
  LINE_OPTIONS *line_options = object->line_options;

  /* Hatch box */
  int fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of box */
  eda_cairo_box (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 line_options->line_width,
                 object->box->lower_x, object->box->lower_y,
                 object->box->upper_x, object->box->upper_y);

  if (fill_solid) {
    cairo_fill_preserve (cr);
  }

  eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    line_options->line_type, line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, line_options->line_width),
                    line_options->line_length, line_options->line_space);
}

static void
eda_renderer_draw_circle (EdaRenderer *renderer, GedaObject *object)
{
  cairo_t *cr = renderer->priv->cr;
  LINE_OPTIONS *line_options = object->line_options;

  /* Hatch circle */
  int fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of circle */
  eda_cairo_arc (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 line_options->line_width,
                 object->circle->center_x, object->circle->center_y,
                 object->circle->radius, 0, 360);

  if (fill_solid) {
    cairo_fill_preserve (cr);
  }

  eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    line_options->line_type, line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, line_options->line_width),
                    line_options->line_length, line_options->line_space);
}

static void
eda_renderer_draw_path (EdaRenderer *renderer, GedaObject *object)
{
  cairo_t *cr = renderer->priv->cr;
  LINE_OPTIONS *line_options = object->line_options;

  /* Hatch path */
  int fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of path */
  eda_cairo_path (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  line_options->line_width, object->path->num_sections,
                  object->path->sections);

  if (fill_solid) {
    cairo_fill_preserve (cr);
  }

  eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    line_options->line_type, line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, line_options->line_width),
                    line_options->line_length, line_options->line_space);
}

static void
eda_renderer_draw_text (EdaRenderer *renderer, GedaObject *object)
{
  cairo_t *cr = renderer->priv->cr;

  g_return_if_fail (renderer->priv->pl != NULL);

  void text_as_outline_box () {

    eda_cairo_box (cr, EDA_RENDERER_CAIRO_FLAGS (renderer), 0,
                   object->left, object->bottom,
                   object->right, object->top);

    eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_SQUARE,
                      EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                      -1, -1);
  }

  /* First check if this is hidden text. */
  if (!geda_object_get_is_visible(object) &&
      !EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {
    return;
  }

  /* Also, check that we actually need to display a string */
  if (object->text->disp_string == NULL)
    return;

  /* If text outline mode is selected, draw an outline */
  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_OUTLINE)) {
    return text_as_outline_box();
  }

  /* Otherwise, actually draw the text */

  cairo_save (cr);

  if (eda_renderer_prepare_text (renderer, object)) {
    eda_pango_renderer_show_layout (renderer->priv->pr, renderer->priv->pl);
    cairo_restore (cr);
  }
  else {
    cairo_restore (cr);
    return;
  }

  /* Note: we must access visibility flag directly */
  if (object->visibility != 1) { /* If not normally visible text */

    /* Hidden text so check if showing a little "I". */

    if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {

      double marker_dist = EDAR_TEXT_MARKER_SIZE; /* Get User setting */
      double dummy = 0;

      /* Translate to marker distance to device distance. */
      cairo_user_to_device_distance (cr, &marker_dist, &dummy);

      /* If the marker distance is too small do not draw the marker. */

      if (marker_dist > EDAR_MARKER_THRESHOLD) {

        double x, y;

        gdk_cairo_set_source_color (cr, &EDAR_TEXT_MARKER_COLOR);

        /* Centre of marker is just below and to the right of the text
         * object's origin - FIXME: even if it falls on top of text. */
        x = object->text->x + 2 * EDAR_TEXT_MARKER_SIZE;
        y = object->text->y - 2 * EDAR_TEXT_MARKER_SIZE;

        /* Top */
        eda_cairo_line (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        END_NONE, 0,
                        x - EDAR_TEXT_MARKER_SIZE, y + EDAR_TEXT_MARKER_SIZE,
                        x + EDAR_TEXT_MARKER_SIZE, y + EDAR_TEXT_MARKER_SIZE);

        /* Vertical */
        eda_cairo_line (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        END_NONE, 0,
                        x + EDAR_MARKER_THRESHOLD, y + EDAR_TEXT_MARKER_SIZE,
                        x + EDAR_MARKER_THRESHOLD, y - EDAR_TEXT_MARKER_SIZE);

        /* Bottom */
        eda_cairo_line (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        END_NONE, 0,
                        x - EDAR_TEXT_MARKER_SIZE, y - EDAR_TEXT_MARKER_SIZE,
                        x + EDAR_TEXT_MARKER_SIZE, y - EDAR_TEXT_MARKER_SIZE);

        eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                          TYPE_SOLID, END_NONE,
                          EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                          -1, -1);
      }
    }
  }
}

/*
static int
eda_renderer_get_font_descent (EdaRenderer *renderer,
                               PangoFontDescription *desc)
{
  PangoFontMetrics *metrics;
  int size = pango_font_description_get_size (desc);
  int *key;
  int descent;

  / Lookup the font size in the metrics cache, and get the metrics
   / from there if available. Otherwise, calculate the metrics and
   / cache them.
  metrics = g_hash_table_lookup (renderer->priv->metrics_cache, &size);
  if (metrics == NULL) {
    metrics = pango_context_get_metrics (renderer->priv->pc, desc, NULL);
    key = g_new (int, 1);
    *key = size;
    g_hash_table_insert (renderer->priv->metrics_cache, key, metrics);
  }

  descent = pango_font_metrics_get_descent (metrics);

  //pango_font_metrics_unref(metrics);
  return descent ;
}
*/

/* Calculate position to draw text relative to text origin marker, in
 * world coordinates. */
static inline void
eda_renderer_calc_text_position (EdaRenderer         *renderer,
                                 const GedaObject    *object,
                                 int descent, double *x, double *y)
{
  PangoRectangle inked_rect, logical_rect;
  double y_lower, y_middle, y_upper;
  double x_left,  x_middle, x_right;

  pango_layout_get_extents (renderer->priv->pl, &inked_rect, &logical_rect);

  /*! \note Ideally, we would just be using font / logical metrics for vertical
   *        alignment, however this way seems to be more backward compatible
   *        with the old gschem rendering.
   *
   *        Lower alignment is at the baseline of the bottom text line, whereas
   *        middle and upper alignment is based upon the inked extents of the
   *        entire text block.
   */

  x_middle = -logical_rect.width / 2.0;

  if (object->text->angle == 180) {
    x_left   = -logical_rect.width;
    x_right  = 0;
    y_upper  = descent - logical_rect.height;            /* Baseline of bottom line */
    y_middle = -inked_rect.y - inked_rect.height / 2.0;  /* Middle of inked extents */
    y_lower  = -inked_rect.y;                            /* Top of inked extents */
  }
  else {
    x_left   = 0;
    x_right  = -logical_rect.width;
    y_upper  = -inked_rect.y;                      /* Top of inked extents */
    y_middle = y_upper - inked_rect.height / 2.0;  /* Middle of inked extents */
    y_lower  = descent - logical_rect.height;      /* Baseline of bottom line */
  }

  switch (object->text->alignment) {
    default:
      /* Fall through to LOWER_left case */
    case LOWER_LEFT:   /* 0 */ *y = y_lower;  *x = x_left;   break;
    case MIDDLE_LEFT:  /* 1 */ *y = y_middle; *x = x_left;   break;
    case UPPER_LEFT:   /* 2 */ *y = y_upper;  *x = x_left;   break;
    case LOWER_MIDDLE: /* 3 */ *y = y_lower;  *x = x_middle; break;
    case MIDDLE_MIDDLE:/* 4 */ *y = y_middle; *x = x_middle; break;
    case UPPER_MIDDLE: /* 5 */ *y = y_upper;  *x = x_middle; break;
    case LOWER_RIGHT:  /* 6 */ *y = y_lower;  *x = x_right;  break;
    case MIDDLE_RIGHT: /* 7 */ *y = y_middle; *x = x_right;  break;
    case UPPER_RIGHT:  /* 8 */ *y = y_upper;  *x = x_right;  break;
  }

  *x /= PANGO_SCALE;
  *y /= PANGO_SCALE;
}

static inline int
eda_renderer_prepare_text (EdaRenderer *renderer, const GedaObject *object)
{
  int    pango_size;
  int    points_size;
  int    descent;
  char  *draw_string;
  double dx, dy;

  PangoFontDescription *desc;
  PangoAttrList        *attrs;

  points_size = geda_text_object_get_size_in_points(object);

  pango_size = lrint (points_size * PANGO_SCALE);

  /* Set hinting as appropriate */
  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {
    cairo_font_options_set_hint_style (EdaFontOptions, CAIRO_HINT_STYLE_MEDIUM);
  }
  else {
    cairo_font_options_set_hint_style (EdaFontOptions, CAIRO_HINT_STYLE_NONE);
  }

  /* Set font name and size, and obtain descent metric */
  desc = pango_font_description_from_string (renderer->priv->font_name);

  pango_font_description_set_size (desc, pango_size);

  pango_layout_set_font_description (renderer->priv->pl, desc);

  descent = round ((EDAR_DESCENT_FACTOR * pango_size) + EDAR_DESCENT_OFFSET);
  //descent = eda_renderer_get_font_descent (renderer, desc);

  pango_font_description_free (desc);

  /* Extract text to display, Pango text attributes, and set up layout. */
  if (!eda_pango_parse_overbars (object->text->disp_string, -1,
                                 &attrs, &draw_string)) {
    return FALSE;
  }

  cairo_t *cr = renderer->priv->cr;

  pango_layout_set_text (renderer->priv->pl, draw_string, -1);

  pango_layout_set_attributes (renderer->priv->pl, attrs);

  pango_cairo_update_layout (cr, renderer->priv->pl);

  GEDA_FREE (draw_string);
  pango_attr_list_unref (attrs);

  /* Calculate text position. */
  eda_renderer_calc_text_position (renderer, object, descent, &dx, &dy);

  cairo_translate (cr, object->text->x, object->text->y);

  /* Special case turns upside-down text back upright */
  if (object->text->angle != 180) {
    cairo_rotate (cr, M_PI * object->text->angle / 180.);
  }

  cairo_scale (cr, 1, -1);
  cairo_translate (cr, dx, dy);

  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {
  /* NB: Shift the position by 0.5px to match the hinting applied to single
   *     pixel wide lines. This means the text will sit correctly on top of
   *     the grid lines, and ensures consistency with other lines when the
   *     page view is zoomed out. */
    dx = 0.5; dy = 0.5;
    cairo_device_to_user_distance (cr, &dx, &dy);
    cairo_translate (cr, dx, dy);
  }

  /* tell Pango to re-layout the text with the new transformation matrix */
  pango_layout_context_changed (renderer->priv->pl);

  pango_cairo_update_layout (cr, renderer->priv->pl);

  return TRUE;
}

static void
eda_renderer_draw_picture (EdaRenderer *renderer, GedaObject *object)
{
  cairo_t *cr = renderer->priv->cr;
  double width,  orig_width;
  double height, orig_height;

  GdkPixbuf *pixbuf;
  bool missing;

  g_return_if_fail (GEDA_IS_PICTURE (object));

  /* Get a pixbuf, or, failing that, a fallback image. */
  if (object->picture->pixbuf && GDK_IS_PIXBUF(object->picture->pixbuf))
  {
    pixbuf  = g_object_ref (object->picture->pixbuf);
    missing = object->picture->missing;
  }
  else {

    /* do not flood the console with redundant messages */
    static int pixbuf_error = 1;

    if (pixbuf_error) {
      BUG_MSG("picture->pixbuf invalid");
      pixbuf_error--;
    }

    pixbuf  = geda_picture_object_get_fallback_pixbuf ();
    missing = TRUE;

    if (pixbuf) {
      g_object_ref (pixbuf);
    }
    else {

      /* If no pixbuf was found, fall back to drawing an outline */
      if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_PICTURE_OUTLINE)) {

        eda_cairo_box (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                       0,
                       object->picture->lower_x, object->picture->lower_y,
                       object->picture->upper_x, object->picture->upper_y);
        eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                          TYPE_SOLID, END_SQUARE,
                          EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                          -1, -1);

      }
      return;
    }
  }

  cairo_save (cr);

  width  = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  if ((object->picture->angle == 90) || (object->picture->angle == 270))
  {
    orig_width  = height;
    orig_height = width;
  }
  else {
    orig_width  = width;
    orig_height = height;
  }

  cairo_translate (cr,
                   object->picture->upper_x, object->picture->upper_y);

  cairo_scale (cr,
               fabs (object->picture->upper_x - object->picture->lower_x) / orig_width,
             - fabs (object->picture->upper_y - object->picture->lower_y) / orig_height);

  /* Evil magic translates picture origin to the right position for a given rotation */
  switch (object->picture->angle) {
    case 0:                                                                    break;
    case 90:   cairo_translate (cr, 0,          orig_height);  break;
    case 180:  cairo_translate (cr, orig_width, orig_height);  break;
    case 270:  cairo_translate (cr, orig_width, 0          );  break;
  }

  cairo_rotate (cr, -object->picture->angle * M_PI / 180.);

  if (object->picture->mirrored) {
    cairo_translate (cr, width, 0);
    cairo_scale (cr, -1, 1);
  }

  gdk_cairo_set_source_pixbuf (cr, pixbuf, 0,0);

  cairo_rectangle (cr, 0, 0, width, height);

  cairo_clip (cr);
  cairo_paint (cr);

  if (missing) { /* Add some useful text */

    char *filename;
    char *err_msg;

    filename  = object->picture->filename;
    err_msg = geda_sprintf (_("Error loading: %s"), filename);
    cairo_set_font_size(cr, 6);

    /* add the text */
    cairo_move_to( cr, 3, height * 0.95);
    cairo_set_source_rgba(cr, 0,0,0,1);
    cairo_show_text(cr, err_msg);
    GEDA_FREE(err_msg);
  }

  cairo_restore (cr);
  GEDA_UNREF (pixbuf);
}

/** \defgroup eda-renderer-draw-grips EdaRenderer Draw Grips
 *  @{
 */
/* ================================================================
 * GRIP DRAWING
 * ================================================================
 */

/*!
 * \brief Draw Grips for List of GedaObject using Cairo Renderer
 * \par Function Description
 *  Wrapper that calls eda_renderer_draw_grips for each member of list.
 *
 * \param [in] renderer Pointer to a EdaRenderer object.
 * \param [in] list     List of Objects for which grips are to be drawn.
 */
void
eda_renderer_draw_grips_list (EdaRenderer *renderer, GList *list)
{
  if (renderer->draw_grips) {
    GList *iter;
    for (iter = list; iter != NULL; iter = g_list_next (iter)) {
      eda_renderer_draw_grips (renderer, (GedaObject*) iter->data);
    }
  }
}

/*!
 * \brief Draw Grips for GedaObject using Cairo Renderer
 * \par Function Description
 *  Calls virtual draw_grips routine for \a object.
 *
 * \param [in] renderer Pointer to a EdaRenderer object.
 * \param [in] object   GedaObject for which grips are to be drawn.
 */
void
eda_renderer_draw_grips (EdaRenderer *renderer, GedaObject *object)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));

  if (renderer->draw_grips == FALSE)
    return;

  if (!eda_renderer_is_drawable (renderer, object))
    return;

  EDA_RENDERER_GET_CLASS (renderer)->draw_grips (renderer, object);
}

static void
eda_renderer_default_draw_grips (EdaRenderer *renderer, GedaObject *object)
{
  /* Note renderer was validated in eda_renderer_draw_grips */
  g_return_if_fail (object != NULL);
  g_return_if_fail (renderer->priv->cr != NULL);

  switch (object->type) {
    case OBJ_LINE:
    case OBJ_NET:
    case OBJ_BUS:
    case OBJ_PIN:
      eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 2,
                                    object->line->x[0], object->line->y[0],
                                    object->line->x[1], object->line->y[1]);
      break;

    case OBJ_BOX:
      eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 4,
                                    object->box->upper_x, object->box->upper_y,
                                    object->box->lower_x, object->box->upper_y,
                                    object->box->upper_x, object->box->lower_y,
                                    object->box->lower_x, object->box->lower_y);
      break;

    case OBJ_ARC:
      eda_renderer_draw_arc_grips (renderer, object);
      break;

    case OBJ_CIRCLE:
      switch (EDAR_CIRCLE_GRIP_QUAD) {
        case 1: /* Grip at right side */
          eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 1,
                                        object->circle->center_x + object->circle->radius,
                                        object->circle->center_y);
        case 2: /* Grip at top */
          eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 1,
                                        object->circle->center_x,
                                        object->circle->center_y + object->circle->radius);
        case 3: /* Grip at left side */
          eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 1,
                                        object->circle->center_x - object->circle->radius,
                                        object->circle->center_y);
        case 4: /* Grip at bottom */
          eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 1,
                                        object->circle->center_x,
                                        object->circle->center_y); // - object->circle->radius);
      }
      break;

    case OBJ_PATH:
      eda_renderer_draw_path_grips (renderer, object);
      break;

    case OBJ_TEXT:
      if(renderer->text_origin_marker)
        eda_renderer_draw_text_grips (renderer, object);
      break;

    case OBJ_PICTURE:
      eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 4,
                                    object->picture->upper_x, object->picture->upper_y,
                                    object->picture->lower_x, object->picture->upper_y,
                                    object->picture->upper_x, object->picture->lower_y,
                                    object->picture->lower_x, object->picture->lower_y);
      break;

    case OBJ_COMPLEX:
      if (renderer->draw_complex_grips) {
        eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 1,
                                      object->complex->x,
                                      object->complex->y);
      }
    case OBJ_PLACEHOLDER:
      /* No grips */
      break;

    default:
      BUG_IMSG("unhandled case <%c>\n", object->type);
  }
}

static void
eda_renderer_draw_grips_impl (EdaRenderer *renderer, int type, int n_grips, ...)
{
  cairo_t *cr = renderer->priv->cr;
  va_list coordinates;
  int i;

  va_start (coordinates, n_grips);
  for (i = 0; i < n_grips; i++) {

    int x = va_arg (coordinates, int);
    int y = va_arg (coordinates, int);

    switch (type) {
      case EDAR_GRIP_SQUARE:
        eda_cairo_center_box (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                              0, 0, x, y,
                              EDAR_GRIP_SIZE,
                              EDAR_GRIP_SIZE);
        break;
      case EDAR_GRIP_CIRCLE:
        eda_cairo_center_arc (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                              0, 0, x, y,
                              EDAR_GRIP_SIZE,
                              0, 360);
        break;
      default:
        BUG_IMSG("unhandled case <%d>\n", type);
    }

    gdk_cairo_set_source_color (cr, &EDAR_GRIP_FILL_COLOR);
    cairo_fill_preserve (cr);

    gdk_cairo_set_source_color (cr, &EDAR_GRIP_STROKE_COLOR);
    eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_NONE,
                      0, -1, -1);
  }
  va_end (coordinates);
}

/* Draw Grips for an Arc
 * An arc has three grips:
 * <DL>
 *   <DT>*</DT><DD>one at the center that allows changes on the
 *                 radius - at (<B>x</B>,<B>y</B>).
 *   <DT>*</DT><DD>one at the start of the arc - at (<B>x1</B>,<B>y1</B>).
 *   <DT>*</DT><DD>one at the end of the arc - at (<B>x2</B>,<B>y2</B>).
 */
static void
eda_renderer_draw_arc_grips (EdaRenderer *renderer, GedaObject *object)
{
  double radius, start_angle, arc_sweep;
  int x1, y1, x2, y2, x3, y3;

  x1 = object->arc->x;
  y1 = object->arc->y;

  radius      = (double)object->arc->radius;
  start_angle = (double)object->arc->start_angle;
  arc_sweep   = (double)object->arc->arc_sweep;

  x2 = x1 + radius * cos ( start_angle              * M_PI / 180);
  y2 = y1 + radius * sin ( start_angle              * M_PI / 180);
  x3 = x1 + radius * cos ((start_angle + arc_sweep) * M_PI / 180);
  y3 = y1 + radius * sin ((start_angle + arc_sweep) * M_PI / 180);

  eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 3,
                                x1, y1, /* center */
                                x2, y2, /* start_angle */
                                x3, y3); /* arc_sweep */
}

static void
eda_renderer_draw_path_grips (EdaRenderer *renderer, GedaObject *object)
{
  cairo_t *cr = renderer->priv->cr;
  int i, last_x = 0, last_y = 0, next_x, next_y;

  for (i = 0; i < object->path->num_sections; i++) {



    PATH_SECTION *section = object->path->sections + i;

    if (section->code != PATH_END) {
      next_x = section->x3;
      next_y = section->y3;
    }

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point lines */
      eda_cairo_line (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      END_NONE, 0,
                      last_x, last_y, section->x1, section->y1);
      eda_cairo_line (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      END_NONE, 0,
                      next_x, next_y, section->x2, section->y2);
      eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        TYPE_SOLID, END_NONE,
                        EDA_RENDERER_STROKE_WIDTH (renderer, 0), -1, -1);
      /* Two control point grips */
      eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_CIRCLE, 2,
                                    section->x1, section->y1,
                                    section->x2, section->y2);
      /* Deliberately fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      last_x = next_x;
      last_y = next_y;
      /* One control point grip */
      eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 1,
                                    section->x3, section->y3);
      break;
    case PATH_END:
      break;
    }
  }
}

static void
eda_renderer_draw_text_grips (EdaRenderer *renderer, GedaObject *object)
{
  cairo_t *cr        = renderer->priv->cr;
  double dummy       = 0;
  double marker_dist = EDAR_TEXT_MARKER_SIZE;

  int x = object->text->x;
  int y = object->text->y;

  /* First check if this is hidden text. */
  if (!geda_object_get_is_visible(object) &&
      !EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {
    return;
  }

  /* If the text marker is too tiny, don't draw it. */
  cairo_user_to_device_distance (cr, &marker_dist, &dummy);

  if (marker_dist < EDAR_MARKER_THRESHOLD)
    return;

  gdk_cairo_set_source_color (cr, &EDAR_TEXT_MARKER_COLOR);

  eda_cairo_line (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,
                  x - EDAR_TEXT_MARKER_SIZE, y - EDAR_TEXT_MARKER_SIZE,
                  x + EDAR_TEXT_MARKER_SIZE, y + EDAR_TEXT_MARKER_SIZE);

  eda_cairo_line (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,
                  x - EDAR_TEXT_MARKER_SIZE, y + EDAR_TEXT_MARKER_SIZE,
                  x + EDAR_TEXT_MARKER_SIZE, y - EDAR_TEXT_MARKER_SIZE);

  eda_cairo_stroke (cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_NONE,
                    EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                    -1, -1);
}
/** @} eda-renderer-draw-grips */

/** \defgroup eda-renderer-draw-cue EdaRenderer Draw Cue
 *  @{
 */
/* ================================================================
 * CUE DRAWING
 * ================================================================ */

static void
eda_renderer_draw_junction_cue (EdaRenderer *renderer, int x, int y, double width)
{
  double radius = width / 2.0;

  eda_cairo_center_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        width, -1, x, y, radius, 0, 360);

  gdk_cairo_set_source_color (renderer->priv->cr, &EDAR_JUNCTION_COLOR);

  cairo_fill (renderer->priv->cr);
}

static void
eda_renderer_draw_mid_cues (EdaRenderer *renderer, GedaObject *object)
{
  GList *iter;

  for (iter = object->conn_list; iter != NULL; iter = g_list_next (iter)) {

    CONN *conn = (CONN *) iter->data;

    if (conn->type == CONN_MIDPOINT) {
      eda_renderer_draw_junction_cue (renderer, conn->x, conn->y, EDAR_JUNCTION_SIZE);
    }
  }
}

static void
eda_renderer_draw_end_cues (EdaRenderer *renderer, GedaObject *object, int end)
{
  int x          = object->line->x[end];
  int y          = object->line->y[end];
  int conn_count = 0;
  int conn_type  = CONN_ENDPOINT;
  int is_bus;
  GList *iter;

  /* Should never be at the unconnectable end of a pin */
  g_return_if_fail ((object->type != OBJ_PIN) || (object->pin->whichend == end));

  /* Check whether the current object is a bus or bus pin */
  is_bus = ((object->type == OBJ_BUS) ||
           ((object->type == OBJ_PIN) &&
            (object->pin->node_type == PIN_BUS_NODE)));

  for (iter = object->conn_list; iter != NULL; iter = g_list_next (iter)) {

    CONN *conn = (CONN *) iter->data;

    if ((conn->x != x) || (conn->y != y))
      continue;

    /* Check whether the connected object is a bus or bus pin */
    is_bus |= ((conn->other_object->type == OBJ_BUS) ||
              ((conn->other_object->type == OBJ_PIN) &&
               (conn->other_object->pin->node_type == PIN_BUS_NODE)));

    if (conn->type == CONN_MIDPOINT) {
      /* If it's a mid-line connection, we can stop already. */
      conn_type = CONN_MIDPOINT;
      break;
    }

    conn_count++;
  }

  /* Draw a midpoint, if necessary */
  if ((conn_type == CONN_MIDPOINT) ||
     ((object->type == OBJ_NET) &&
      (conn_count > 1)))
  {
    eda_renderer_draw_junction_cue (renderer, x, y, is_bus);
    return;
  }

  /* The only artifacts not drawn are end point cues */

  gdk_cairo_set_source_color (renderer->priv->cr, &EDAR_NET_ENDPOINT_COLOR);

  switch (object->type) {
    case OBJ_NET:
    case OBJ_PIN:
      /* If less than one thing was connected to this end of the net
       * segment or pin, draw box cue */
      if (conn_count > 0) break;

      eda_cairo_center_box (renderer->priv->cr,
                            EDA_RENDERER_CAIRO_FLAGS (renderer),
                            -1, -1, x, y, CUE_BOX_SIZE, CUE_BOX_SIZE);
      cairo_fill (renderer->priv->cr);
      break;

    case OBJ_BUS:
      break;
    default:
      BUG_IMSG("unhandled case <%c>\n", object->type);
  }
}

static void
eda_renderer_default_draw_cues (EdaRenderer *renderer, GedaObject *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (renderer->priv->cr != NULL);

  switch (object->type) {
  case OBJ_LINE:
  case OBJ_BOX:
  case OBJ_ARC:
  case OBJ_CIRCLE:
  case OBJ_PATH:
  case OBJ_TEXT:
  case OBJ_PICTURE:
    break;

  case OBJ_COMPLEX:
  case OBJ_PLACEHOLDER:
    /* Recurse */
    eda_renderer_draw_cues_list (renderer, object->complex->prim_objs);
    break;

  case OBJ_NET:
  case OBJ_BUS:
    eda_renderer_draw_mid_cues (renderer, object);
    eda_renderer_draw_end_cues (renderer, object, 0);
    eda_renderer_draw_end_cues (renderer, object, 1);
    break;

  case OBJ_PIN:
    if ((object->pin->whichend == 1) || (object->pin->whichend == 0))
      eda_renderer_draw_end_cues (renderer, object, object->pin->whichend);
    else
      BUG_IMSG("pin->whichend is invalid=%d \n", object->pin->whichend);
    break;

  default:
    BUG_IMSG("unhandled case <%c>\n", object->type);
  }
}

static void
eda_renderer_draw_cues_list (EdaRenderer *renderer, GList *objects)
{
  GList *iter;

  for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
    eda_renderer_draw_cues (renderer, (GedaObject*) iter->data);
  }
}

/*!
 * \brief Draw Object Cues
 * \par Function Description
 *  Front-end for drawing the cues of a single object. Calls the
 *  virtual renderer class member draw_cues.
 */
void
eda_renderer_draw_cues (EdaRenderer *renderer, GedaObject *object)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  EDA_RENDERER_GET_CLASS (renderer)->draw_cues (renderer, object);
}

/** @} eda-renderer-draw-cue */

/** \defgroup eda-renderer-bounds EdaRenderer Bouds Routines
 *  @{
 */

/* ================================================================
 * RENDERED BOUNDS
 * ================================================================ */

/*!
 * \brief Get user bounds of a single object
 * \par Function Description
 *  Front-end for retrieving the bounds of a single object. Calls the
 *  virtual renderer class member user_bounds.
 */
int
eda_renderer_get_user_bounds (EdaRenderer      *renderer,
                              const GedaObject *object,
                              int *left,   int *top,
                              int *right,  int *bottom)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), FALSE);

  return EDA_RENDERER_GET_CLASS (renderer)->user_bounds (renderer, object,
                                                         left,     top,
                                                         right,    bottom);
}

/*!
 * \brief Get user bounds of a GedaObject
 * \par Function Description
 *  Calculates and returns the bounds of GedaObjects.
 *
 * \param [in]  renderer Pointer to a EdaRenderer object.
 * \param [in]  object   Pointer to a GedaObject.
 * \param [out] left     Pointer to integer location for the left bounds
 * \param [out] top      Pointer to integer location for the top bounds
 * \param [out] right    Pointer to integer location for the right bounds
 * \param [out] bottom   Pointer to integer location for the bottom bounds
 *
 * \returns TRUE if the bounds was set.
 */
int
eda_renderer_default_get_user_bounds (EdaRenderer      *renderer,
                                      const GedaObject *object,
                                      int *left,   int *top,
                                      int *right,  int *bottom)
{
  g_return_val_if_fail ((object != NULL), FALSE);
  g_return_val_if_fail ((renderer->priv->cr != NULL), FALSE);

  switch (object->type) {
  case OBJ_TEXT:
    return eda_renderer_get_text_user_bounds (renderer, object,
                                              left, top, right, bottom);
  case OBJ_LINE:
  case OBJ_BOX:
  case OBJ_ARC:
  case OBJ_CIRCLE:
  case OBJ_PATH:
  case OBJ_PICTURE:
  case OBJ_COMPLEX:
  case OBJ_PLACEHOLDER:
  case OBJ_NET:
  case OBJ_BUS:
  case OBJ_PIN:
    if (geda_object_bounds(object)) {
      *left    = object->left;
      *top     = object->top;
      *right   = object->right;
      *bottom  = object->bottom;
      return TRUE;
    }
    return FALSE;

  default:
    BUG_IMSG("object->type=%c\n", object->type);
    g_return_val_if_reached (FALSE);
  }
}

/*!
 * \brief Get user bounds of a GedaText object
 * \par Function Description
 *  Calculates and returns the bounds of text represented by the
 *  GedaText object.
 *
 * \param [in]  renderer Pointer to a EdaRenderer object.
 * \param [in]  object   Pointer to a GedaObject.
 * \param [out] left     Pointer to integer location for the left bounds
 * \param [out] top      Pointer to integer location for the top bounds
 * \param [out] right    Pointer to integer location for the right bounds
 * \param [out] bottom   Pointer to integer location for the bottom bounds
 *
 * \returns TRUE if the bounds was set.
 */
int
eda_renderer_get_text_user_bounds (EdaRenderer      *renderer,
                                   const GedaObject *object,
                                   int *left,   int *top,
                                   int *right,  int *bottom)

{
  int ret_val = FALSE;
  int visible;

  visible = object->visibility != INVISIBLE;

  /* First check if this is hidden text. */
  if (visible || EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {

    /* Also, check that we actually need to display a string */
    if (object->text->disp_string != NULL) {

      cairo_t *cr = renderer->priv->cr;

      cairo_save (cr);

      /* Set up the text and check it worked. */
      if (eda_renderer_prepare_text (renderer, object)) {

        if (PANGO_IS_LAYOUT(renderer->priv->pl)) {

          PangoRectangle inked_rect; /* logical_rect; */

          /* Figure out the bounds, send them back. Note that Pango thinks
           * in device coordinates, but we need world coordinates. */
          pango_layout_get_pixel_extents (renderer->priv->pl, &inked_rect, NULL);

          double dleft   = (double) inked_rect.x;
          double dtop    = (double) inked_rect.y;
          double dright  = (double) inked_rect.x + inked_rect.width;
          double dbottom = (double) inked_rect.y + inked_rect.height;

          /* Does it does make sense to describe bounds in terms of 14
           * decimal places? Or even 2 decimal place? */
          cairo_user_to_device (cr, &dleft,  &dtop);
          cairo_user_to_device (cr, &dright, &dbottom);

          cairo_restore (cr);

          cairo_device_to_user (cr, &dleft,  &dtop);
          cairo_device_to_user (cr, &dright, &dbottom);

          /* Gid rid of all the zeros Cairo just passed on the stack */
          *left   = lrint(dleft);
          *top    = lrint(dtop);
          *right  = lrint(dright);
          *bottom = lrint(dbottom);

          /* If not normal visible text, account for the little "I" */
          if (object->visibility != 1) {

            double offset = EDAR_TEXT_MARKER_SIZE + 5;
            unsigned int alignment = object->text->alignment;

            if ((alignment == LOWER_LEFT) ||
                (alignment == LOWER_MIDDLE) ||
                (alignment == LOWER_RIGHT))
            {
              *bottom = *bottom - offset;
            }

            /* someday, MIDDLE_MIDDLE UPPER_MIDDLE MIDDLE_LEFT UPPER_LEFT */
            if ((alignment == UPPER_RIGHT) ||
                (alignment == MIDDLE_RIGHT) ||
                (alignment == LOWER_RIGHT))
            {
              int    size;
              double adjustment;

              size       = object->text->size;
              adjustment = 1.5 * size  + (2 * offset / 3);
              *right     = *right + adjustment;
            }
          }
          ret_val = TRUE;
        }
        else {
          BUG_MSG("Invalid pango_layout");
          cairo_restore (cr);
        }
      }
      else {
        cairo_restore (cr);
      }
    }
  }

  return ret_val;
}

/** @} eda-renderer-bounds */

/*!
 * \brief Type class initializer for EdaRenderer
 * \par Function Description
 *  Type class initializer for EdaRenderer. Overrides parent virtual class
 *  methods as needed and register GObject signals.
 *
 * \param [in]  g_class      The EdaRenderer class being initialized
 * \param [in]  class_data   EdaRenderer structure associated with the class
 */
static void
eda_renderer_class_init(void *g_class, void *class_data)
{
  EdaRendererClass *class         = (EdaRendererClass*)g_class;
  GObjectClass     *gobject_class = G_OBJECT_CLASS (class);
  GParamSpec       *params;
  GParamFlags       param_flags;

  /* Register functions with base class */
  gobject_class->constructor  = eda_renderer_constructor;
  gobject_class->dispose      = eda_renderer_dispose;
  gobject_class->finalize     = eda_renderer_finalize;
  gobject_class->set_property = eda_renderer_set_property;
  gobject_class->get_property = eda_renderer_get_property;

  /* Install default implementations of virtual public methods */
  class->draw        = eda_renderer_default_draw;
  class->draw_grips  = eda_renderer_default_draw_grips;
  class->draw_cues   = eda_renderer_default_draw_cues;
  class->user_bounds = eda_renderer_default_get_user_bounds;

  eda_renderer_parent_class = g_type_class_peek_parent (class);

  /* Install properties */
  param_flags = (G_PARAM_READWRITE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK |
                 G_PARAM_STATIC_BLURB);

  params = g_param_spec_pointer ("cairo-context",
                               _("Cairo context"),
                               _("The Cairo context for rendering"),
                                  param_flags);

  g_object_class_install_property (gobject_class, PROP_CAIRO_CONTEXT, params);

  params = g_param_spec_pointer ("pango-context",
                               _("Pango context"),
                               _("The Pango context for text rendering"),
                                  param_flags);

  g_object_class_install_property (gobject_class, PROP_PANGO_CONTEXT, params);

  params = g_param_spec_string ("font-name",
                              _("Font name"),
                              _("The name of the font to use for text rendering"),
                                 EDAR_DEFAULT_FONT_NAME,
                                 param_flags);

  g_object_class_install_property (gobject_class, PROP_FONT_NAME, params);

  params = g_param_spec_pointer ("color-map",
                               _("Color map"),
                               _("Map for determining colors from color indices"),
                                  param_flags);

  g_object_class_install_property (gobject_class, PROP_COLOR_MAP, params);

  params = g_param_spec_int ("override-color",
                           _("Override color"),
                           _("Index of color to force used for all drawing."),
                             -1, MAX_COLORS, -1,
                              param_flags);

  g_object_class_install_property (gobject_class, PROP_OVERRIDE_COLOR, params);

  params = g_param_spec_int ("render-flags",
                           _("Rendering  Flags"),
                           _("Flags controlling rendering"),
                              0,
                              FLAG_HINTING | FLAG_TEXT_ORIGIN,
                              EDA_RENDERER_FLAG_HINTING,
                              param_flags);

  g_object_class_install_property (gobject_class, PROP_RENDER_FLAGS, params);

  params = g_param_spec_double ("draw-grips",
                              _("Draw grips"),
                              _("Controls if grips should be drawn"),
                                 0, 1, 0,
                                 param_flags);

  g_object_class_install_property (gobject_class, PROP_DRAW_GRIPS, params);

  params = g_param_spec_double ("grip-size",
                              _("Grip size"),
                              _("Size in user coordinates to draw grips"),
                                 0, G_MAXDOUBLE, EDAR_DEFAULT_GRIP_SIZE,
                                 param_flags);

  g_object_class_install_property (gobject_class, PROP_GRIP_SIZE, params);

  params = g_param_spec_boxed ("grips-stroke",
                             _("Grip Stroke Color"),
                             _("GDK color to use when rendering strokes for grips"),
                                GDK_TYPE_COLOR,
                                param_flags);

  g_object_class_install_property (gobject_class, PROP_GRIP_STROKE, params);

  params = g_param_spec_boxed ("grips-fill",
                             _("Grip Fill Color"),
                             _("GDK color to use when rendering background of grips"),
                                GDK_TYPE_COLOR,
                                param_flags);

  g_object_class_install_property (gobject_class, PROP_GRIP_FILL, params);

   /*! property "circle-grip-quadrant": EdaRenderer::circle-grip-quadrant
   *  \brief Sets or gets circle-grip-quadrant for a EdaRenderer.
   *  \par
   *   Controls quadrant of circle where the grip are to be drawn.
   */
  params = g_param_spec_int ("circle-grip-quadrant",
                           _("Circle Grip Quadrant"),
                           _("Controls where grips are drawn on circles"),
                              1,
                              4,
                              EDAR_DEFAULT_CIRCLE_GRIP_QUAD,
                              param_flags);

  g_object_class_install_property (gobject_class, PROP_CIRCLE_GRIP_QUAD, params);

  params = g_param_spec_boxed ("junction-color",
                             _("Junction Color"),
                             _("GDK color to use when rendering Junctions"),
                                GDK_TYPE_COLOR,
                                param_flags);

  g_object_class_install_property (gobject_class, PROP_JUNCTION_COLOR, params);

  params = g_param_spec_int ("junction-size",
                           _("Junction size"),
                           _("Size to draw junction cue points"),
                              0, 999, 10,
                              param_flags);

  g_object_class_install_property (gobject_class, PROP_JUNCTION_SIZE, params);

  params = g_param_spec_boxed ("net-endpoint-color",
                             _("Net Endpoint Color"),
                             _("GDK color to use when rendering Net and Pin endpoints"),
                                GDK_TYPE_COLOR,
                                param_flags);

  g_object_class_install_property (gobject_class, PROP_ENDPOINT_COLOR, params);

  params = g_param_spec_boxed ("text-marker-color",
                             _("Text Marker Color"),
                             _("GDK color to use when rendering text markers"),
                                GDK_TYPE_COLOR,
                                param_flags);

  g_object_class_install_property (gobject_class, PROP_MARKER_COLOR, params);

  params = g_param_spec_int ("text-marker-size",
                           _("Text Marker Size"),
                           _("Size to draw text markers."),
                              EDAR_MIN_TEXT_MARKER_SIZE,
                              EDAR_MAX_TEXT_MARKER_SIZE,
                              EDAR_DEFAULT_TEXT_MARKER_SIZE,
                              param_flags);

  g_object_class_install_property (gobject_class, PROP_TEXT_MARKER_SIZE, params);

  params = g_param_spec_double ("text-marker-threshold",
                              _("Text Marker Threshold"),
                              _("The threshold to draw text markers"),
                                 EDAR_MIN_MARKER_DIST_THLD,
                                 EDAR_MAX_MARKER_DIST_THLD,
                                 EDAR_DEFAULT_MARKER_DIST_THLD,
                                 param_flags);

  g_object_class_install_property (gobject_class, PROP_TEXT_MARKER_THLD, params);
}

/*!
 * \brief Type instance initializer for EdaRenderer
 * \par Function Description
 *  Type instance initializer for EdaRenderer, initializes a new empty
 *  EdaRenderer object.
 *
 * \param [in] instance The EdaRenderer structure being initialized,
 * \param [in] g_class  The EdaRenderer class we are initializing.
 */
static void
eda_renderer_instance_init(GTypeInstance *instance, void *g_class)
{
  EdaRenderer *renderer = (EdaRenderer*)instance;

  renderer->priv = GEDA_MEM_ALLOC0 (sizeof(EdaRendererData));

  EdaFontOptions = cairo_font_options_create();

  /* Setup default options */
  if (renderer->priv->font_name == NULL) {
    renderer->priv->font_name = geda_strdup (EDAR_DEFAULT_FONT_NAME);
  }

  cairo_font_options_set_antialias(EdaFontOptions, CAIRO_ANTIALIAS_GOOD);
  cairo_font_options_set_hint_metrics (EdaFontOptions, CAIRO_HINT_METRICS_OFF);

  renderer->priv->override_color = -1;

  EDAR_CIRCLE_GRIP_QUAD    = EDAR_DEFAULT_CIRCLE_GRIP_QUAD;
  EDAR_GRIP_SIZE           = EDAR_DEFAULT_GRIP_SIZE;
  EDAR_JUNCTION_SIZE       = EDAR_DEFAULT_JUNCTION_SIZE;
  EDAR_TEXT_MARKER_SIZE    = EDAR_DEFAULT_TEXT_MARKER_SIZE;
  EDAR_MARKER_THRESHOLD    = EDAR_DEFAULT_MARKER_DIST_THLD;

  //TODO
  //eda_renderer_parse_color (EDAR_DEFAULT_GRIP_STROKE_COLOR, &EDAR_GRIP_STROKE_COLOR);
  gdk_color_parse(EDAR_DEFAULT_GRIP_STROKE_COLOR,  &EDAR_GRIP_STROKE_COLOR);
  gdk_color_parse(EDAR_DEFAULT_GRIP_FILL_COLOR,    &EDAR_GRIP_FILL_COLOR);
  gdk_color_parse(EDAR_DEFAULT_JUNCTION_COLOR,     &EDAR_JUNCTION_COLOR);
  gdk_color_parse(EDAR_DEFAULT_ENDPOINT_COLOR,     &EDAR_NET_ENDPOINT_COLOR);
  gdk_color_parse(EDAR_DEFAULT_TEXT_MARKER_COLOR,  &EDAR_TEXT_MARKER_COLOR);

  /* Font metrics are expensive to compute, so we need to cache them.
  renderer->priv->metrics_cache =
    g_hash_table_new_full (g_int_hash, g_int_equal, g_free,
                           (GDestroyNotify) pango_font_metrics_unref);  */
}

/*!
 * \brief Function to retrieve EdaRenderer's Type identifier.
 * \par Function Description
 *  Function to retrieve a #EdaRenderer Type identifier. When
 *  first called, the function registers a #EdaRenderer in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a EdaRenderer and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with EdaRenderer.
 */
GedaType eda_renderer_get_type (void)
{
  static GedaType eda_renderer_type = 0;

  if (g_once_init_enter (&eda_renderer_type)) {

    static const GTypeInfo info = {
      sizeof(EdaRendererClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      eda_renderer_class_init,         /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(EdaRenderer),
      0,                               /* n_preallocs         */
      eda_renderer_instance_init       /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("EdaRenderer");
    type   = g_type_register_static (G_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&eda_renderer_type, type);
  }

  return eda_renderer_type;
}

/* ================================================================
 * MISCELLANEOUS (CREATION, DESTRUCTION, ACCESSORS)
 * ================================================================ */

/*!
 * \brief Get a New EdaRenderer Object
 * \par Function Description
 *  Creates and returns a new EdaRenderer instance
 *
 */
EdaRenderer *
eda_renderer_new (cairo_t *cr, PangoContext *pc)
{
  return g_object_new (EDA_TYPE_RENDERER,
                       "cairo-context", cr,
                       "pango-context", pc,
                       NULL);
}

/*!
 * \brief Destroy a EdaRenderer Object
 * \par Function Description
 *  Decrements the reference of \a renderer
 */
void eda_renderer_destroy (EdaRenderer *renderer)
{
  if (G_IS_OBJECT(renderer)) {
    g_object_unref (renderer);
  }
  else {
    BUG_MSG("Bad pointer to EdaRenderer, is it aleady dead?");
  }
}

/*!
 * \brief Get the active EdaRenderer Cairo Context
 * \par Function Description
 *  Function to retrieve the current Cairo Context.
 */
cairo_t *eda_renderer_get_cairo_context (EdaRenderer *renderer)
{
  cairo_t *cr;
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  g_object_get (renderer, "cairo-context", &cr, NULL);
  return cr;
}

/*!
 * \brief Set the active EdaRenderer Cairo Context
 * \par Function Description
 *  Function to retrieve the current Cairo Context.
 */
void
eda_renderer_set_cairo_context (EdaRenderer *renderer, cairo_t *cr)
{
  if (EDA_IS_RENDERER (renderer)) {
    eda_renderer_update_contexts(renderer, cr, NULL);
  }
}

/*!
 * \brief Get the active EdaRenderer Cairo Flags
 * \par Function Description
 *  Function to retrieve the current Cairo Flags.
 */
int
eda_renderer_get_cairo_flags (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), 0);
  return EDA_RENDERER_CAIRO_FLAGS (renderer);
}

/*!
 * \brief Get the current EdaRenderer Color Map Array
 * \par Function Description
 *  Function to retrieve the current color-map Array.
 */
GArray*
eda_renderer_get_color_map (EdaRenderer *renderer)
{
  GArray *map = NULL;
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  g_object_get (G_OBJECT (renderer), "color-map", &map, NULL);
  return map;
}

/*!
 * \brief Set the EdaRenderer Color Map Array
 * \par Function Description
 *  Sets the color-map Array.
 */
void
eda_renderer_set_color_map (EdaRenderer *renderer, GArray *map)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (renderer, "color-map", map, NULL);
}

/*!
 * \brief Get the EdaRenderer draw grips Property
 * \par Function Description
 *  Function to retrieve the current draw-grips property. Grips
 *  will not be drawn if the draw-grips property is not set.
 */
int eda_renderer_get_draw_grips (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return renderer->draw_grips;
}

/*!
 * \brief Set the EdaRenderer draw grips Property
 * \par Function Description
 *  Function to set the draw-grips property. When the draw-grips
 *  property is set, grips will be drawn.
 */
void eda_renderer_set_draw_grips (EdaRenderer *renderer, bool draw)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  renderer->draw_grips = draw ? 1 : 0;
}

/*!
 * \brief Get the EdaRenderer draw complex grips Property
 * \par Function Description
 *  Function to retrieve the current draw-complex-grips property.
 *  Grips will not be drawn if the draw-complex-grips property is
 *  not set.
 */
int eda_renderer_get_draw_complex_grips (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return renderer->draw_complex_grips ;
}

/*!
 * \brief Set the EdaRenderer draw complex grips Property
 * \par Function Description
 *  Function to set the draw-complex-grips property. When the
 *  draw-complex-grips property is set, grips will be drawn on
 *  complex object if draw-grips is enabled.
 */
void eda_renderer_set_draw_complex_grips (EdaRenderer *renderer, bool draw)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  renderer->draw_complex_grips  = draw ? 1 : 0;
}

/*!
 * \brief Get the current EdaRenderer Font Name Property
 * \par Function Description
 *  Function to retrieve the current font-name property.
 */
const char*
eda_renderer_get_font_name(EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (renderer->priv->font_name);
}

/*!
 * \brief Set the EdaRenderer Font Name Property
 * \par Function Description
 *  Sets the font-name property.
 */
void
eda_renderer_set_font_name(EdaRenderer *renderer, const char *font_name)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (font_name);
  g_return_if_fail (strlen(font_name) > 1);

  GEDA_FREE(renderer->priv->font_name);
  renderer->priv->font_name = geda_utility_string_strdup (font_name);
}

/*!
 * \brief Set EdaRenderer Flags
 * \par Function Description
 *  Function to set the EdaRenderer flags.
 */
bool
eda_renderer_set_flags (EdaRenderer *renderer, int flags)
{
  if (EDA_IS_RENDERER (renderer))
    renderer->priv->flags = flags;
  else
    return FALSE;
  return TRUE;
}

/*!
 * \brief Is EdaRenderer Flag Set
 * \par Function Description
 *  Function to check whether a flag bit is set.
 */
bool
eda_renderer_mask_flags (EdaRenderer *renderer, int flags)
{
  if (EDA_IS_RENDERER (renderer))
    renderer->priv->flags |= flags;
  else
    return FALSE;
  return TRUE;
}

/*!
 * \brief Get the current EdaRenderer Flags
 * \par Function Description
 *  Function to retrieve the current EdaRenderer Flags
 */
int
eda_renderer_get_flags (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return renderer->priv->flags;
}

/*!
 * \brief Get the EdaRenderer Hinting Enabled Property
 * \par Function Description
 *  Function to retrieve the current hinting property.
 */
bool
eda_renderer_get_hinting_enabled (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), FALSE);
  return EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING);
}

/*!
 * \brief Get the current EdaRenderer Override Color Index Property
 * \par Function Description
 *  Function to retrieve the current override-color property.
 */
int
eda_renderer_get_override_color_index (EdaRenderer *renderer)
{
  return renderer->priv->override_color;
}

/*!
 * \brief Set the EdaRenderer Override Color Index Property
 * \par Function Description
 *  Sets the override-color property.
 */
void
eda_renderer_set_override_color_index (EdaRenderer *renderer, int color_index)
{
  renderer->priv->override_color = color_index;
}

/*!
 * \brief Get the current EdaRenderer Grip Circle Quadrant Property
 * \par Function Description
 *  Function to retrieve the current circle-grip-quadrant property.
 */
int
eda_renderer_get_circle_grip_quad (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return EDAR_CIRCLE_GRIP_QUAD;
}

/*!
 * \brief Set the EdaRenderer Grip Circle Quadrant Property
 * \par Function Description
 *  Sets the circle-grip-quadrant property.
 */
void
eda_renderer_set_circle_grip_quad (EdaRenderer *renderer, int quad)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  if (quad > 0 && quad < 5) {
    EDAR_CIRCLE_GRIP_QUAD = quad;
  }
}

/*!
 * \brief Get the current EdaRenderer Grip Size Property
 * \par Function Description
 *  Function to retrieve the current grip-size property.
 */
double
eda_renderer_get_grips_size (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return EDAR_GRIP_SIZE;
}

/*!
 * \brief Set the EdaRenderer Grip Size Property
 * \par Function Description
 *  Sets the grip-size property.
 */
void
eda_renderer_set_grips_size (EdaRenderer *renderer, double new_size)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (renderer, "grip-size", new_size, NULL);
}

/*!
 * \brief Get the current EdaRenderer Grip Stroke Color Property
 * \par Function Description
 *  Function to retrieve the current grips-stroke-color property.
 */
const GdkColor*
eda_renderer_get_grips_stroke_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_GRIP_STROKE_COLOR;
}

/*!
 * \brief Set the EdaRenderer Grip Stroke Color Property
 * \par Function Description
 *  Sets the grips-stroke-color property.
 */
void
eda_renderer_set_grips_stroke_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_GRIP_STROKE_COLOR.red   = color->red;
  EDAR_GRIP_STROKE_COLOR.green = color->green;
  EDAR_GRIP_STROKE_COLOR.blue  = color->blue;
}

/*!
 * \brief Get the current EdaRenderer Grips Fill Color Property
 * \par Function Description
 *  Function to retrieve the current grips-fill-color property.
 */
const GdkColor*
eda_renderer_get_grips_fill_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_GRIP_FILL_COLOR;
}

/*!
 * \brief Set the EdaRenderer Grips Fill Color Property
 * \par Function Description
 *  Sets the grips-fill-color property.
 */
void
eda_renderer_set_grips_fill_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_GRIP_FILL_COLOR.red   = color->red;
  EDAR_GRIP_FILL_COLOR.green = color->green;
  EDAR_GRIP_FILL_COLOR.blue  = color->blue;
}

/*!
 * \brief Get the current EdaRenderer Junction Color Property
 * \par Function Description
 *  Function to retrieve the current junction-color property.
 */
const GdkColor*
eda_renderer_get_junction_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_JUNCTION_COLOR;
}

/*!
 * \brief Set the EdaRenderer Junction Color Property
 * \par Function Description
 *  Sets the junction-color property.
 */
void
eda_renderer_set_junction_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_JUNCTION_COLOR.red   = color->red;
  EDAR_JUNCTION_COLOR.green = color->green;
  EDAR_JUNCTION_COLOR.blue  = color->blue;
}

/*!
 * \brief Get the current EdaRenderer Junction Size Property
 * \par Function Description
 *  Function to retrieve the current junction-size property.
 */
int
eda_renderer_get_junction_size (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return EDAR_JUNCTION_SIZE;
}

/*!
 * \brief Set the EdaRenderer Junction Size Property
 * \par Function Description
 *  Sets the junction-size property.
 */
void
eda_renderer_set_junction_size (EdaRenderer *renderer, int new_size)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (renderer, "junction-size", new_size, NULL);
}

/*!
 * \brief Get the current EdaRenderer Marker Threshold Property
 * \par Function Description
 *  Function to retrieve the current text-marker-threshold property.
 */
double
eda_renderer_get_marker_threshold (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return EDAR_MARKER_THRESHOLD;
}

/*!
 * \brief Set the EdaRenderer Marker Threshold Property
 * \par Function Description
 *  Sets the text-marker-threshold property.
 */
void
eda_renderer_set_marker_threshold (EdaRenderer *renderer, double threshold)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (renderer, "text-marker-threshold", threshold, NULL);
}

/*!
 * \brief Get the current EdaRenderer Net Endpoint Color Property
 * \par Function Description
 *  Function to retrieve the current net-endpoint-color property.
 */
const GdkColor*
eda_renderer_get_net_endpoint_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_NET_ENDPOINT_COLOR;
}

/*!
 * \brief Set the EdaRenderer Net Endpoint Color Property
 * \par Function Description
 *  Sets the net-endpoint-color property.
 */
void
eda_renderer_set_net_endpoint_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_NET_ENDPOINT_COLOR.red   = color->red;
  EDAR_NET_ENDPOINT_COLOR.green = color->green;
  EDAR_NET_ENDPOINT_COLOR.blue  = color->blue;
}

/*!
 * \brief Get the current EdaRenderer Text Marker Color Property
 * \par Function Description
 *  Function to retrieve the current text marker color property.
 */
const GdkColor*
eda_renderer_get_text_marker_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_TEXT_MARKER_COLOR;
}

/*!
 * \brief Set the EdaRenderer Text Marker Color Property
 * \par Function Description
 *  Sets the text marker color property.
 */
void
eda_renderer_set_text_marker_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_TEXT_MARKER_COLOR.red   = color->red;
  EDAR_TEXT_MARKER_COLOR.green = color->green;
  EDAR_TEXT_MARKER_COLOR.blue  = color->blue;
}

/*!
 * \brief Get the current EdaRenderer Text Marker Size Property
 * \par Function Description
 *  Function to retrieve the current text-marker-size property. The
 *  Text Marker is the little "x" drawn at the insertion coordinates
 *  of the text object.
 */
int
eda_renderer_get_text_marker_size (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return EDAR_TEXT_MARKER_SIZE;
}

/*!
 * \brief Set the EdaRenderer Text Marker Size Property
 * \par Function Description
 *  Sets the text-marker-size property.
 * \see eda_renderer_get_text_marker_size
 */
void
eda_renderer_set_text_marker_size (EdaRenderer *renderer, int new_size)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (renderer, "text-marker-size", new_size, NULL);
}
