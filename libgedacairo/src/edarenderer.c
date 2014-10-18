/* gEDA - GPL Electronic Design Automation
 * libgedacairo - Rendering gEDA schematics with Cairo
 * Copyright (C) 2010-2014 gEDA Contributors (see ChangeLog for details)
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
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

#include "edarenderer.h"

#include "edacairo.h"
#include "edapangorenderer.h"

/* We don't use gettext */
#define _(x) (x)

#define EDAR_DEFAULT_GRIP_SIZE             100
#define EDAR_DEFAULT_JUNCTION_SIZE          50
#define EDAR_DEFAULT_TEXT_MARKER_SIZE       15
#define EDAR_MIN_TEXT_MARKER_SIZE            5
#define EDAR_MAX_TEXT_MARKER_SIZE          100

#define EDAR_DEFAULT_FONT_NAME          "Arial"

#define EDAR_DESCENT_FACTOR 3.01728024042074
#define EDAR_DESCENT_OFFSET -886.034560480839

#define EDAR_DEFAULT_GRIP_STROKE_COLOR  "orange"
#define EDAR_DEFAULT_GRIP_FILL_COLOR    "black"
#define EDAR_DEFAULT_TEXT_MARKER_COLOR  "gray"
#define EDAR_DEFAULT_JUNCTION_COLOR     "yellow"
#define EDAR_DEFAULT_ENDPOINT_COLOR     "red"

#define EDAR_DEFAULT_OVERRIDE_COLOR_INDEX 1

#define EdaFontOptions renderer->priv->font_options

enum {
  PROP_CAIRO_CONTEXT = 1,
  PROP_PANGO_CONTEXT,
  PROP_FONT_NAME,
  PROP_COLOR_MAP,
  PROP_OVERRIDE_COLOR,
  PROP_GRIP_SIZE,
  PROP_GRIP_STROKE,
  PROP_GRIP_FILL,
  PROP_JUNCTION_COLOR,
  PROP_JUNCTION_SIZE,
  PROP_ENDPOINT_COLOR,
  PROP_MARKER_COLOR,
  PROP_TEXT_MARKER_SIZE,

  PROP_RENDER_FLAGS,

  FLAG_HINTING         = EDA_RENDERER_FLAG_HINTING,
  FLAG_PICTURE_OUTLINE = EDA_RENDERER_FLAG_PICTURE_OUTLINE,
  FLAG_TEXT_HIDDEN     = EDA_RENDERER_FLAG_TEXT_HIDDEN,
  FLAG_TEXT_OUTLINE    = EDA_RENDERER_FLAG_TEXT_OUTLINE,
  FLAG_TEXT_ORIGIN     = EDA_RENDERER_FLAG_TEXT_ORIGIN,

  EDAR_GRIP_SQUARE,
  EDAR_GRIP_CIRCLE,
};

struct _EdaRendererPrivate
{
  cairo_t          *cr;
  PangoContext     *pc;
  PangoLayout      *pl;
  EdaPangoRenderer *pr;
  int pc_from_cr;

  unsigned int      flags;
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
static inline unsigned int
EDA_RENDERER_CAIRO_FLAGS (EdaRenderer *r) {
  return EDA_RENDERER_CHECK_FLAG (r, FLAG_HINTING) ? EDA_CAIRO_ENABLE_HINTS : 0;
}
static inline double
EDA_RENDERER_STROKE_WIDTH (EdaRenderer *r,  double line_width) {
  return fmax (line_width, MIN_LINE_WIDTH_THRESHOLD);
}

static GObject *eda_renderer_constructor (GedaType type,
                                          unsigned int n_construct_properties,
                                          GObjectConstructParam *construct_params);

static void eda_renderer_finalize (GObject *object);
static void eda_renderer_dispose  (GObject *object);
static void eda_renderer_set_property (GObject *object, unsigned int property_id,
                                       const GValue *value, GParamSpec *pspec);
static void eda_renderer_get_property (GObject *object, unsigned int property_id,
                                       GValue *value, GParamSpec *pspec);
static void eda_renderer_update_contexts (EdaRenderer *renderer, cairo_t *new_cr,
                                          PangoContext *new_pc);

static void eda_renderer_set_color    (EdaRenderer *renderer, int color);
static int  eda_renderer_is_drawable_color (EdaRenderer *renderer, int color, int use_override);
static int  eda_renderer_is_drawable  (EdaRenderer *renderer, Object *object);
static int  eda_renderer_draw_hatch   (EdaRenderer *renderer, Object *object);

static void eda_renderer_default_draw (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_list    (EdaRenderer *renderer, GList *objects);
static void eda_renderer_draw_line    (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_pin     (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_net     (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_bus     (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_box     (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_arc     (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_circle  (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_path    (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_text    (EdaRenderer *renderer, Object *object);

static int  eda_renderer_prepare_text       (EdaRenderer *renderer, Object *object);
static void eda_renderer_calc_text_position (EdaRenderer *renderer, Object *object,
                                             int descent, double *x, double *y);
static void eda_renderer_draw_picture (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_complex (EdaRenderer *renderer, Object *object);

static void eda_renderer_default_draw_grips (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_grips_impl    (EdaRenderer *renderer, int type, int n_grips, ...);
static void eda_renderer_draw_arc_grips     (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_path_grips    (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_text_grips    (EdaRenderer *renderer, Object *object);

static void eda_renderer_draw_junction_cue  (EdaRenderer *renderer, int x, int y,
                                             double width);
static void eda_renderer_draw_mid_cues      (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_end_cues      (EdaRenderer *renderer, Object *object,
                                             int end);
static void eda_renderer_default_draw_cues  (EdaRenderer *renderer, Object *object);
static void eda_renderer_draw_cues_list     (EdaRenderer *renderer, GList *objects);

static int eda_renderer_default_get_user_bounds (EdaRenderer *renderer, Object *object,
                                                 int *left,   int *top,
                                                 int *right,  int *bottom);

G_DEFINE_TYPE (EdaRenderer, eda_renderer, G_TYPE_OBJECT);

/*! \brief Function to retrieve EdaRendererFlags's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve EdaRendererFlags's Type identifier. Upon
 *  first call, this registers the EdaRendererFlags in the GedaType
 *  system.  Subsequently it returns the saved value from its first
 *  execution.
 *
 *  \return the Type identifier associated with EdaRendererFlags.
 */
unsigned int
eda_renderer_flags_get_type ()
{
  static const GFlagsValue values[] = {
    {FLAG_HINTING,         "hinting",         _("Enable hinting")},
    {FLAG_PICTURE_OUTLINE, "picture-outline", _("Picture outlines")},
    {FLAG_TEXT_HIDDEN,     "text-hidden",     _("Hidden text")},
    {FLAG_TEXT_OUTLINE,    "text-outline",    _("Text outlines")},
    {FLAG_TEXT_ORIGIN,     "text-origin",     _("Text origins")},
    {0, 0, 0},
  };
  static unsigned int flags_type = 0;
  if (flags_type == 0) {
    flags_type = g_flags_register_static ("EdaRendererFlags", values);
  }
  return flags_type;
}

static void
eda_renderer_class_init (EdaRendererClass *class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);
  GParamSpec   *params;
  GParamFlags   param_flags;

  g_type_class_add_private (gobject_class, sizeof (EdaRendererPrivate));

  /* Register functions with base class */
  gobject_class->constructor  = eda_renderer_constructor;
  gobject_class->finalize     = eda_renderer_finalize;
  gobject_class->dispose      = eda_renderer_dispose;
  gobject_class->set_property = eda_renderer_set_property;
  gobject_class->get_property = eda_renderer_get_property;

  /* Install default implementations of virtual public methods */
  class->draw        = eda_renderer_default_draw;
  class->draw_grips  = eda_renderer_default_draw_grips;
  class->draw_cues   = eda_renderer_default_draw_cues;
  class->user_bounds = eda_renderer_default_get_user_bounds;

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

  params =g_param_spec_pointer ("color-map",
                              _("Color map"),
                              _("Map for determining colors from color indices"),
                                 param_flags);

  g_object_class_install_property (gobject_class, PROP_COLOR_MAP, params);

  params =g_param_spec_int ("override-color",
                          _("Override color"),
                          _("Index of color to force used for all drawing."),
                            -1, MAX_COLORS, -1,
                             param_flags);

  g_object_class_install_property (gobject_class, PROP_OVERRIDE_COLOR, params);

  params = g_param_spec_flags ("render-flags",
                             _("Rendering  Flags"),
                             _("Flags controlling rendering"),
                                EDA_TYPE_RENDERER_FLAGS,
                                FLAG_HINTING | FLAG_TEXT_ORIGIN,
                                param_flags);

  g_object_class_install_property (gobject_class, PROP_RENDER_FLAGS, params);

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

  params = g_param_spec_boxed ("junction-color",
                             _("Junction Color"),
                             _("GDK color to use when rendering Junctions"),
                                GDK_TYPE_COLOR,
                                param_flags);

  g_object_class_install_property (gobject_class, PROP_JUNCTION_COLOR, params);

  params =g_param_spec_int ("junction-size",
                          _("Junction size"),
                          _("Size to draw junction cue points."),
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

  params =g_param_spec_int ("text-marker-size",
                          _("Text Marker size"),
                          _("Size to draw text markers."),
                             EDAR_MIN_TEXT_MARKER_SIZE,
                             EDAR_MAX_TEXT_MARKER_SIZE,
                             EDAR_DEFAULT_TEXT_MARKER_SIZE,
                             param_flags);

  g_object_class_install_property (gobject_class, PROP_TEXT_MARKER_SIZE, params);
}

static void
eda_renderer_init (EdaRenderer *renderer)
{

  renderer->priv = G_TYPE_INSTANCE_GET_PRIVATE (renderer,
                                                EDA_TYPE_RENDERER,
                                                EdaRendererPrivate);

  EdaFontOptions = cairo_font_options_create();

  /* Setup default options */
  if (renderer->priv->font_name == NULL) {
    renderer->priv->font_name = u_string_strdup (EDAR_DEFAULT_FONT_NAME);
  }

  renderer->priv->override_color = -1;
  EDAR_GRIP_SIZE           = EDAR_DEFAULT_GRIP_SIZE;
  EDAR_JUNCTION_SIZE       = EDAR_DEFAULT_JUNCTION_SIZE;
  EDAR_TEXT_MARKER_SIZE    = EDAR_DEFAULT_TEXT_MARKER_SIZE;

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

static GObject *
eda_renderer_constructor (GedaType type,
                          unsigned int n_construct_properties,
                          GObjectConstructParam *construct_params) {
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
    GEDA_UNREF (renderer->priv->pc);
    renderer->priv->pc = NULL;
  }

  if (PANGO_IS_LAYOUT(renderer->priv->pl)) {
    GEDA_UNREF (renderer->priv->pl);
    renderer->priv->pl = NULL;
  }

  if (renderer->priv->pr != NULL) {
    GEDA_UNREF (renderer->priv->pr);
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
  renderer->priv->font_name = NULL;

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
                                  (cairo_t *) g_value_get_pointer (value),
                                  NULL);
    break;

  case PROP_PANGO_CONTEXT:
    eda_renderer_update_contexts (renderer, NULL,
                                  PANGO_CONTEXT (g_value_get_pointer (value)));
    break;

  case PROP_FONT_NAME:
    if (renderer->priv->font_name != NULL)
      GEDA_FREE (renderer->priv->font_name);
    renderer->priv->font_name = g_value_dup_string (value);
    /* Clear font metrics cache */
    //g_hash_table_remove_all (renderer->priv->metrics_cache);
    break;

  case PROP_COLOR_MAP:
    renderer->priv->color_map      = g_value_get_pointer (value);
    break;

  case PROP_OVERRIDE_COLOR:
    renderer->priv->override_color = g_value_get_int (value);
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

  case PROP_RENDER_FLAGS:
    renderer->priv->flags          = g_value_get_flags (value);
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

  case PROP_GRIP_SIZE:
    g_value_set_double (value, EDAR_GRIP_SIZE);
    break;

  case PROP_GRIP_STROKE: /* Grip Stroke Color */
    g_value_set_boxed (value, &EDAR_GRIP_STROKE_COLOR);
    break;

  case PROP_GRIP_FILL: /* Grip Fill Color */
    g_value_set_boxed (value, &EDAR_GRIP_FILL_COLOR);
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

  case PROP_RENDER_FLAGS:
    g_value_set_flags (value, renderer->priv->flags);
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
      GEDA_UNREF (G_OBJECT (renderer->priv->pc));
      renderer->priv->pc = NULL;
    }
    if (PANGO_IS_LAYOUT(renderer->priv->pl)) {
      GEDA_UNREF (G_OBJECT (renderer->priv->pl));
      renderer->priv->pl = NULL;
    }
    renderer->priv->pc = g_object_ref (G_OBJECT (new_pc));
    renderer->priv->pc_from_cr = 0;
  }

  /* Now recreate anything necessary */
  if ((renderer->priv->pc == NULL) && (renderer->priv->cr != NULL)) {
    renderer->priv->pc = pango_cairo_create_context (renderer->priv->cr);
    cairo_font_options_set_antialias(EdaFontOptions, CAIRO_ANTIALIAS_GOOD);
    pango_cairo_context_set_font_options (renderer->priv->pc, EdaFontOptions);
    renderer->priv->pc_from_cr = 1;
  }
  else if (renderer->priv->cr != NULL) {
    pango_cairo_update_context(new_cr, renderer->priv->pc);
  }

  if ((renderer->priv->pl == NULL) && (renderer->priv->pc != NULL)) {
    renderer->priv->pl = pango_layout_new (renderer->priv->pc);
  }

  if ((renderer->priv->pr == NULL) && (renderer->priv->cr != NULL)) {
    renderer->priv->pr =
      (EdaPangoRenderer *) eda_pango_renderer_new (renderer->priv->cr);
  }
  else if (renderer->priv->cr != NULL) {
    eda_pango_renderer_update(renderer->priv->pr, renderer->priv->cr);
  }

}

/* ================================================================
 * Object DRAWING
 * ================================================================ */

static void
eda_renderer_draw_list (EdaRenderer *renderer, GList *objects)
{
  GList *iter;

  for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
    eda_renderer_draw (renderer, (Object *) iter->data);
  }
}

void
eda_renderer_draw (EdaRenderer *renderer, Object *object)
{
  g_return_if_fail (EDA_IS_RENDERER(renderer));
  EDA_RENDERER_GET_CLASS (renderer)->draw (renderer, object);
}

static void
eda_renderer_default_draw (EdaRenderer *renderer, Object *object)
{
  static int color_map_error = 0;

  void (*draw_func)(EdaRenderer *, Object *);

  g_return_if_fail (object != NULL);
  g_return_if_fail (renderer->priv->cr != NULL);
  g_return_if_fail (renderer->priv->pl != NULL);

  if (renderer->priv->color_map != NULL) {

    if (!eda_renderer_is_drawable (renderer, object)) return;

    switch (object->type) {
      case OBJ_LINE:        draw_func = eda_renderer_draw_line; break;
      case OBJ_NET:         draw_func = eda_renderer_draw_net; break;
      case OBJ_BUS:         draw_func = eda_renderer_draw_bus; break;
      case OBJ_PIN:         draw_func = eda_renderer_draw_pin; break;
      case OBJ_BOX:         draw_func = eda_renderer_draw_box; break;
      case OBJ_ARC:         draw_func = eda_renderer_draw_arc; break;
      case OBJ_CIRCLE:      draw_func = eda_renderer_draw_circle; break;
      case OBJ_PATH:        draw_func = eda_renderer_draw_path; break;
      case OBJ_TEXT:        draw_func = eda_renderer_draw_text; break;
      case OBJ_PICTURE:     draw_func = eda_renderer_draw_picture; break;

      case OBJ_COMPLEX:
      case OBJ_PLACEHOLDER: draw_func = eda_renderer_draw_complex; break;

      default:
        g_return_if_reached ();
    }

    eda_renderer_set_color (renderer, object->color);
    draw_func (renderer, object);
  }
  else {
    /* We use a static here so we do not flood the console with so many
     * error, all saying the same thing, No color map, that everything
     * else is scrolled beyond the consoles buffer = vary annoying */
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
  eda_cairo_set_source_color (renderer->priv->cr, color,
                              renderer->priv->color_map);
}

static int
eda_renderer_is_drawable_color (EdaRenderer *renderer, int color,
                                int use_override)
{
  GArray *map = renderer->priv->color_map;
  /* Check for override color */
  if ((renderer->priv->override_color >= 0) && use_override) {
    color = renderer->priv->override_color;
  }

  /* If color index out of color map bounds, don't draw */
  g_return_val_if_fail ((map != NULL), FALSE);
  g_return_val_if_fail ((color >= 0) || (color < map->len), FALSE);

  /* Otherwise, return enabled flag of object's color */
  return (&g_array_index (map, COLOR, color))->enabled;
}

static int
eda_renderer_is_drawable (EdaRenderer *renderer, Object *object)
{
  int color = object->color;
  /* Always attempt to draw complex objects */
  if ((object->type == OBJ_COMPLEX) || (object->type == OBJ_PLACEHOLDER)) {
    return TRUE;
  }
  return eda_renderer_is_drawable_color (renderer, color, TRUE);
}

static int eda_renderer_draw_hatch (EdaRenderer *renderer, Object *object)
{
  GArray *fill_lines;
  LINE   *line;
  int     fill_width;
  int     index;
  bool    result;

  g_return_val_if_fail ((object->type == OBJ_ARC    ||
                         object->type == OBJ_BOX    ||
                         object->type == OBJ_CIRCLE ||
                         object->type == OBJ_PATH),
                         FALSE);
  result = FALSE;

  if (object->fill_options->fill_type != FILLING_HOLLOW) {

    if (object->fill_options->fill_type != FILL_SOLID) {

      fill_lines = m_hatch_object(object);
      fill_width = object->fill_options->fill_width;

      /* Draw fill pattern */
      for (index = 0; index < fill_lines->len; index++) {

        line = &g_array_index (fill_lines, LINE, index);

        eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        END_NONE, fill_width,
                        line->x[0], line->y[0], line->x[1], line->y[1]);
      }

      eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
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
eda_renderer_draw_complex (EdaRenderer *renderer, Object *object)
{
  /* Recurse */
  eda_renderer_draw_list (renderer, object->complex->prim_objs);
}

static void eda_renderer_draw_line (EdaRenderer *renderer, Object *object)
{
  Line *line = GEDA_LINE(object);

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  object->line_options->line_end, object->line_options->line_width,
                  line->x[0], line->y[0], line->x[1], line->y[1]);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_options->line_type,
                    object->line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_options->line_width),
                                                         object->line_options->line_length,
                                                         object->line_options->line_space);
}

static void eda_renderer_draw_net (EdaRenderer *renderer, Object *object)
{
  Line *line = GEDA_LINE(object);

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, object->line_options->line_width,
                  line->x[0], line->y[0], line->x[1], line->y[1]);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_SQUARE,
                    EDA_RENDERER_STROKE_WIDTH (renderer,
                                               object->line_options->line_width),
                    -1, -1);
}

static void eda_renderer_draw_bus (EdaRenderer *renderer, Object *object)
{
  Line *line = GEDA_LINE(object);

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, object->line_options->line_width,
                  line->x[0], line->y[0], line->x[1], line->y[1]);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_SQUARE,
                    EDA_RENDERER_STROKE_WIDTH (renderer,
                                               object->line_options->line_width),
                    -1, -1);
}

static void eda_renderer_draw_pin (EdaRenderer *renderer, Object *object)
{
  Line *line = GEDA_LINE(object);

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, object->line_options->line_width,
                  line->x[0], line->y[0], line->x[1], line->y[1]);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_SQUARE,
                    EDA_RENDERER_STROKE_WIDTH (renderer,
                                               object->line_options->line_width),
                    -1, -1);
}

static void eda_renderer_draw_box (EdaRenderer *renderer, Object *object)
{
  int fill_solid = FALSE;

  /* Hatch box */
  fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of box */
  eda_cairo_box (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 object->line_options->line_width,
                 object->box->lower_x, object->box->lower_y,
                 object->box->upper_x, object->box->upper_y);

  if (fill_solid)
    cairo_fill_preserve (renderer->priv->cr);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_options->line_type, object->line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_options->line_width),
                    object->line_options->line_length, object->line_options->line_space);
}

static void eda_renderer_draw_arc (EdaRenderer *renderer, Object *object)
{
  eda_cairo_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 object->line_options->line_width,
                 object->arc->x,
                 object->arc->y,
                 object->arc->width / 2,
                 object->arc->start_angle,
                 object->arc->end_angle);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_options->line_type,
                    object->line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_options->line_width),
                    object->line_options->line_length,
                    object->line_options->line_space);
}

static void eda_renderer_draw_circle (EdaRenderer *renderer, Object *object)
{
  int fill_solid = FALSE;

  /* Hatch circle */
  fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of circle */
  eda_cairo_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 object->line_options->line_width,
                 object->circle->center_x, object->circle->center_y,
                 object->circle->radius, 0, 360);

  if (fill_solid) cairo_fill_preserve (renderer->priv->cr);

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_options->line_type, object->line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_options->line_width),
                    object->line_options->line_length, object->line_options->line_space);
}

static void
eda_renderer_draw_path (EdaRenderer *renderer, Object *object)
{
  int fill_solid = FALSE;

  /* Hatch path */
  fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of path */
  eda_cairo_path (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  object->line_options->line_width, object->path->num_sections,
                  object->path->sections);

  if (fill_solid) cairo_fill_preserve (renderer->priv->cr);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_options->line_type, object->line_options->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_options->line_width),
                    object->line_options->line_length, object->line_options->line_space);
}

static void
eda_renderer_draw_text (EdaRenderer *renderer, Object *object)
{
  double x, y;
  double dummy = 0;
  double marker_dist = EDAR_TEXT_MARKER_SIZE;

  void text_as_outline_box () {
    eda_cairo_box (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer), 0,
                   object->left, object->bottom,
                   object->right, object->top);
    eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_SQUARE,
                      EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                      -1, -1);
  }

  /* First check if this is hidden text. */
  if (!o_get_is_visible(object) && !EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {
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
  cairo_save (renderer->priv->cr);
  if (eda_renderer_prepare_text (renderer, object)) {
    eda_pango_renderer_show_layout (renderer->priv->pr, renderer->priv->pl);
    cairo_restore (renderer->priv->cr);
  }
  else {
    cairo_restore (renderer->priv->cr);
    return;
  }

  /* Note: we must access visibility flag directly */
  if (object->visibility != 1) { /* If not normal visible text */

    /*  We are showing hidden text so draw a little "I". */

    /* If the text marker is too tiny, don't draw it. */
    if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {
      cairo_user_to_device_distance (renderer->priv->cr, &marker_dist, &dummy);
      if (marker_dist < 1) return;
    }

    gdk_cairo_set_source_color (renderer->priv->cr, &EDAR_TEXT_MARKER_COLOR);

    /* Centre of marker is just below and to the right of the text
     * object's origin. */
    x = object->text->x + 2 * EDAR_TEXT_MARKER_SIZE;
    y = object->text->y - 2 * EDAR_TEXT_MARKER_SIZE;

    eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    END_NONE, 0,  /* Top */
                    x - EDAR_TEXT_MARKER_SIZE, y + EDAR_TEXT_MARKER_SIZE,
                    x + EDAR_TEXT_MARKER_SIZE, y + EDAR_TEXT_MARKER_SIZE);
    eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    END_NONE, 0,  /* Vertical */
                    x, y + EDAR_TEXT_MARKER_SIZE,
                    x, y - EDAR_TEXT_MARKER_SIZE);
    eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    END_NONE, 0,  /* Bottom */
                    x - EDAR_TEXT_MARKER_SIZE, y - EDAR_TEXT_MARKER_SIZE,
                    x + EDAR_TEXT_MARKER_SIZE, y - EDAR_TEXT_MARKER_SIZE);
    eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_NONE,
                      EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                      -1, -1);
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

  return pango_font_metrics_get_descent (metrics);
  descent = pango_font_metrics_get_descent (metrics);

  return descent ;
}
*/
static int
eda_renderer_prepare_text (EdaRenderer *renderer, Object *object)
{
  double points_size, dx, dy;
  int    size, descent;
  char  *draw_string;

  PangoFontDescription *desc;
  PangoAttrList        *attrs;

  points_size = o_text_get_font_size_in_points (object);
  size = lrint (points_size * PANGO_SCALE);

  /* Set hinting as appropriate */
  cairo_font_options_set_hint_metrics (EdaFontOptions, CAIRO_HINT_METRICS_OFF);

  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {
    cairo_font_options_set_hint_style (EdaFontOptions, CAIRO_HINT_STYLE_MEDIUM);
  }
  else {
    cairo_font_options_set_hint_style (EdaFontOptions, CAIRO_HINT_STYLE_NONE);
  }

  pango_cairo_context_set_resolution (renderer->priv->pc, 1000);

  /* Set font name and size, and obtain descent metric */
  desc = pango_font_description_from_string (renderer->priv->font_name);

  pango_font_description_set_size (desc, size);

  pango_layout_set_font_description (renderer->priv->pl, desc);

  descent = round ((EDAR_DESCENT_FACTOR * size) + EDAR_DESCENT_OFFSET);
  //descent = eda_renderer_get_font_descent (renderer, desc);

  pango_font_description_free (desc);

  /* Extract text to display and Pango text attributes, and then set
   * up layout. */
  if (!eda_pango_parse_overbars (object->text->disp_string, -1,
                                 &attrs, &draw_string)) {
    return FALSE;
  }

  pango_layout_set_text (renderer->priv->pl, draw_string, -1);
  pango_layout_set_attributes (renderer->priv->pl, attrs);
  GEDA_FREE (draw_string);
  pango_attr_list_unref (attrs);

  /* Calculate text position. */
  eda_renderer_calc_text_position (renderer, object, descent, &dx, &dy);

  cairo_translate (renderer->priv->cr, object->text->x, object->text->y);

  /* Special case turns upside-down text back upright */
  if (object->text->angle != 180) {
    cairo_rotate (renderer->priv->cr, M_PI * object->text->angle / 180.);
  }

  cairo_scale (renderer->priv->cr, 1, -1);
  cairo_translate (renderer->priv->cr, dx, dy);

  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {
  /* NB: Shift the position by 0.5px to match the hinting applied to single
   *     pixel wide lines. This means the text will sit correctly on top of
   *     the grid lines, and ensures consistency with other lines when the
   *     page view is zoomed out. */
    dx = 0.5; dy = 0.5;
    cairo_device_to_user_distance (renderer->priv->cr, &dx, &dy);
    cairo_translate (renderer->priv->cr, dx, dy);
  }

  /* tell Pango to re-layout the text with the new transformation matrix */
  pango_layout_context_changed (renderer->priv->pl);

  pango_cairo_update_layout (renderer->priv->cr, renderer->priv->pl);
  return TRUE;
}

/* Calculate position to draw text relative to text origin marker, in
 * world coordinates. */
static void
eda_renderer_calc_text_position (EdaRenderer *renderer, Object *object,
                                 int descent, double *x, double *y)
{
  PangoRectangle inked_rect, logical_rect;
  double temp;
  double y_lower, y_middle, y_upper;
  double x_left, x_middle, x_right;

  pango_layout_get_extents (renderer->priv->pl,
                            &inked_rect, &logical_rect);

  x_left = 0;
  x_middle = -logical_rect.width / 2.0;
  x_right = -logical_rect.width;

  /*! \note Ideally, we would be using just font / logical metrics for vertical
   *        alignment, however this way seems to be more backward compatible
   *        with the old gschem rendering.
   *
   *        Lower alignment is at the baseline of the bottom text line, whereas
   *        middle and upper alignment is based upon the inked extents of the
   *        entire text block.
   */
  y_upper  = -inked_rect.y;                     /* Top of inked extents */
  y_middle = y_upper - inked_rect.height / 2.;  /* Middle of inked extents */
  y_lower  = descent - logical_rect.height;     /* Baseline of bottom line */

  /* Special case flips attachment point to opposite corner when
   * the text is rotated to 180 degrees, since the drawing code
   * does not rotate the text to be shown upside down.
   */
  if (object->text->angle == 180) {
    temp = y_lower; y_lower = y_upper; y_upper = temp;
    temp = x_left;  x_left  = x_right; x_right = temp;
  }

  switch (object->text->alignment) {
    default:
      /* Fall through to LOWER_left case */
    case LOWER_LEFT:    *y = y_lower;  *x = x_left;   break;
    case MIDDLE_LEFT:   *y = y_middle; *x = x_left;   break;
    case UPPER_LEFT:    *y = y_upper;  *x = x_left;   break;
    case LOWER_MIDDLE:  *y = y_lower;  *x = x_middle; break;
    case MIDDLE_MIDDLE: *y = y_middle; *x = x_middle; break;
    case UPPER_MIDDLE:  *y = y_upper;  *x = x_middle; break;
    case LOWER_RIGHT:   *y = y_lower;  *x = x_right;  break;
    case MIDDLE_RIGHT:  *y = y_middle; *x = x_right;  break;
    case UPPER_RIGHT:   *y = y_upper;  *x = x_right;  break;
  }

  *x /= PANGO_SCALE;
  *y /= PANGO_SCALE;
}

static void
eda_renderer_draw_picture (EdaRenderer *renderer, Object *object)
{
  int swap_wh;
  double orig_width, orig_height;
  GdkPixbuf *pixbuf;

  /* Get a pixbuf, or, failing that, a fallback image. */
  pixbuf = g_object_ref (object->picture->pixbuf);
  if (pixbuf == NULL) pixbuf = o_picture_get_fallback_pixbuf (NULL /* FIXME */);

  /* If no pixbuf was found, fall back to drawing an outline */
  if (pixbuf == NULL || EDA_RENDERER_CHECK_FLAG (renderer,
                                                 FLAG_PICTURE_OUTLINE)) {
    eda_cairo_box (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                   0,
                   object->picture->lower_x, object->picture->lower_y,
                   object->picture->upper_x, object->picture->upper_y);
    eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_SQUARE,
                      EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                      -1, -1);
    return;
  }

  g_return_if_fail (GDK_IS_PIXBUF (pixbuf));

  cairo_save (renderer->priv->cr);

  swap_wh = ((object->picture->angle == 90) || (object->picture->angle == 270));
  orig_width  = swap_wh ? gdk_pixbuf_get_height (object->picture->pixbuf)
                        : gdk_pixbuf_get_width (object->picture->pixbuf);
  orig_height = swap_wh ? gdk_pixbuf_get_width (object->picture->pixbuf)
                        : gdk_pixbuf_get_height (object->picture->pixbuf);

  cairo_translate (renderer->priv->cr,
                   object->picture->upper_x, object->picture->upper_y);
  cairo_scale (renderer->priv->cr,
               fabs (object->picture->upper_x - object->picture->lower_x) / orig_width,
               - fabs (object->picture->upper_y - object->picture->lower_y) / orig_height);

  /* Evil magic translates picture origin to the right position for a given rotation */
  switch (object->picture->angle) {
    case 0:                                                                    break;
    case 90:   cairo_translate (renderer->priv->cr, 0,          orig_height);  break;
    case 180:  cairo_translate (renderer->priv->cr, orig_width, orig_height);  break;
    case 270:  cairo_translate (renderer->priv->cr, orig_width, 0          );  break;
  }

  cairo_rotate (renderer->priv->cr, -object->picture->angle * M_PI / 180.);
  if (object->picture->mirrored) {
    cairo_translate (renderer->priv->cr, gdk_pixbuf_get_width (pixbuf), 0);
    cairo_scale (renderer->priv->cr, -1, 1);
  }

  gdk_cairo_set_source_pixbuf (renderer->priv->cr,
                               object->picture->pixbuf, 0,0);
  cairo_rectangle (renderer->priv->cr, 0, 0,
                   gdk_pixbuf_get_width (object->picture->pixbuf),
                   gdk_pixbuf_get_height (object->picture->pixbuf));

  cairo_clip (renderer->priv->cr);
  cairo_paint (renderer->priv->cr);

  cairo_restore (renderer->priv->cr);
  GEDA_UNREF (pixbuf);
}

/* ================================================================
 * GRIP DRAWING
 * ================================================================ */

void eda_renderer_draw_grips_list (EdaRenderer *renderer, GList *objects)
{
  GList *iter;
  if(renderer->draw_grips) {
    for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
      eda_renderer_draw_grips (renderer, (Object *) iter->data);
    }
  }
}

void
eda_renderer_draw_grips (EdaRenderer *renderer, Object *object)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));

  EDA_RENDERER_GET_CLASS (renderer)->draw_grips (renderer, object);
}

static void
eda_renderer_default_draw_grips (EdaRenderer *renderer, Object *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (renderer->priv->cr != NULL);

  if(renderer->draw_grips == FALSE)
    return;

  if (!eda_renderer_is_drawable (renderer, object))
    return;

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
      /* Grip at bottom right of containing square */
      eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 1,
                                    object->circle->center_x + object->circle->radius,
                                    object->circle->center_y - object->circle->radius);
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
    case OBJ_PLACEHOLDER:
      /* No grips */
      break;
    default:
      g_return_if_reached ();
  }
}

static void
eda_renderer_draw_grips_impl (EdaRenderer *renderer, int type, int n_grips, ...)
{
  va_list coordinates;
  int i;

  va_start (coordinates, n_grips);
  for (i = 0; i < n_grips; i++) {
    int x = va_arg (coordinates, int);
    int y = va_arg (coordinates, int);

    switch (type) {
      case EDAR_GRIP_SQUARE:
        eda_cairo_center_box (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                              0, 0, x, y,
                              EDAR_GRIP_SIZE,
                              EDAR_GRIP_SIZE);
        break;
      case EDAR_GRIP_CIRCLE:
        eda_cairo_center_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                              0, 0, x, y,
                              EDAR_GRIP_SIZE,
                              0, 360);
        break;
      default:
        g_return_if_reached ();
    }

    gdk_cairo_set_source_color (renderer->priv->cr, &EDAR_GRIP_FILL_COLOR);
    cairo_fill_preserve (renderer->priv->cr);

    gdk_cairo_set_source_color (renderer->priv->cr, &EDAR_GRIP_STROKE_COLOR);
    eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_NONE,
                      0, -1, -1);
  }
  va_end (coordinates);
}

static void
eda_renderer_draw_arc_grips (EdaRenderer *renderer, Object *object)
{
  double radius, start_angle, end_angle;
  int x1, y1, x2, y2, x3, y3;

  /*
   * An arc has three grips:
   * <DL>
   *   <DT>*</DT><DD>one at the center that allows changes on the
   *                 radius - at (<B>x</B>,<B>y</B>).
   *   <DT>*</DT><DD>one at the start of the arc - at (<B>x1</B>,<B>y1</B>).
   *   <DT>*</DT><DD>one at the end of the arc - at (<B>x2</B>,<B>y2</B>).
   */

  x1 = object->arc->x;
  y1 = object->arc->y;

  radius      = object->arc->width / 2.;
  start_angle = object->arc->start_angle;
  end_angle   = object->arc->end_angle;

  x2 = x1 + radius * cos ( start_angle              * M_PI / 180);
  y2 = y1 + radius * sin ( start_angle              * M_PI / 180);
  x3 = x1 + radius * cos ((start_angle + end_angle) * M_PI / 180);
  y3 = y1 + radius * sin ((start_angle + end_angle) * M_PI / 180);

  eda_renderer_draw_grips_impl (renderer, EDAR_GRIP_SQUARE, 3,
                                x1, y1, /* center */
                                x2, y2, /* start_angle */
                                x3, y3); /* end_angle */
}

static void
eda_renderer_draw_path_grips (EdaRenderer *renderer, Object *object)
{
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
      eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      END_NONE, 0,
                      last_x, last_y, section->x1, section->y1);
      eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      END_NONE, 0,
                      next_x, next_y, section->x2, section->y2);
      eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
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
eda_renderer_draw_text_grips (EdaRenderer *renderer, Object *object)
{
  double dummy = 0;
  double marker_dist = EDAR_TEXT_MARKER_SIZE;
  int x = object->text->x;
  int y = object->text->y;

  /* First check if this is hidden text. */
  if (!o_get_is_visible(object) &&
      !EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {
    return;
  }

  /* If the text marker is too tiny, don't draw it. */
  cairo_user_to_device_distance (renderer->priv->cr, &marker_dist, &dummy);
  if (marker_dist < 1) return;

  gdk_cairo_set_source_color (renderer->priv->cr, &EDAR_TEXT_MARKER_COLOR);

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,
                  x - EDAR_TEXT_MARKER_SIZE, y - EDAR_TEXT_MARKER_SIZE,
                  x + EDAR_TEXT_MARKER_SIZE, y + EDAR_TEXT_MARKER_SIZE);
  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,
                  x - EDAR_TEXT_MARKER_SIZE, y + EDAR_TEXT_MARKER_SIZE,
                  x + EDAR_TEXT_MARKER_SIZE, y - EDAR_TEXT_MARKER_SIZE);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_NONE,
                    EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                    -1, -1);
}

/* ================================================================
 * CUE DRAWING
 * ================================================================ */

static void
eda_renderer_draw_junction_cue (EdaRenderer *renderer, int x, int y, double width)
{
  double radius = width / 2;
/*
  if (!eda_renderer_is_drawable_color (renderer, EDAR_JUNCTION_COLOR, 1)) {
    return;
  }
*/
  eda_cairo_center_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        width, -1, x, y, radius, 0, 360);

  gdk_cairo_set_source_color (renderer->priv->cr, &EDAR_JUNCTION_COLOR);

  cairo_fill (renderer->priv->cr);
}

static void
eda_renderer_draw_mid_cues (EdaRenderer *renderer, Object *object)
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
eda_renderer_draw_end_cues (EdaRenderer *renderer, Object *object, int end)
{
  int x = object->line->x[end], y = object->line->y[end];
  int conn_count = 0;
  int conn_type = CONN_ENDPOINT;
  int is_bus = FALSE;
  GList *iter;

  /* We should never be at the unconnectable end of a pin */
  g_return_if_fail ((object->type != OBJ_PIN) || (object->pin->whichend == end));

  /* Check whether the current object is a bus or bus pin */
  is_bus = ((object->type == OBJ_BUS) || ((object->type == OBJ_PIN)
         && (object->pin->node_type == PIN_BUS_NODE)));

  for (iter = object->conn_list; iter != NULL; iter = g_list_next (iter)) {
    CONN *conn = (CONN *) iter->data;
    if ((conn->x != x) || (conn->y != y)) continue;

    /* Check whether the connected object is a bus or bus pin */
    is_bus |= ((conn->other_object->type == OBJ_BUS)
               || ((conn->other_object->type == OBJ_PIN)
                   && (conn->other_object->pin->node_type == PIN_BUS_NODE)));

    if (conn->type == CONN_MIDPOINT) {
      /* If it's a mid-line connection, we can stop already. */
      conn_type = CONN_MIDPOINT;
      break;
    }

    conn_count++;
  }

  /* Draw a midpoint, if necessary */
  if ((conn_type == CONN_MIDPOINT)
      || ((object->type == OBJ_NET) && (conn_count > 1))) {
    eda_renderer_draw_junction_cue (renderer, x, y, is_bus);
    return;
  }

  /* Only things left to be drawn are end point cues */

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
    g_return_if_reached ();
  }
}

static void
eda_renderer_default_draw_cues (EdaRenderer *renderer, Object *object)
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
    g_return_if_reached ();
  }
}

static void
eda_renderer_draw_cues_list (EdaRenderer *renderer, GList *objects)
{
  GList *iter;

  for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
    eda_renderer_draw_cues (renderer, (Object *) iter->data);
  }
}

void
eda_renderer_draw_cues (EdaRenderer *renderer, Object *object)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  EDA_RENDERER_GET_CLASS (renderer)->draw_cues (renderer, object);
}

/* ================================================================
 * RENDERED BOUNDS
 * ================================================================ */

int
eda_renderer_get_user_bounds (EdaRenderer *renderer, Object *object,
                              int *left,     int *top,
                              int *right,    int *bottom)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), FALSE);

  return EDA_RENDERER_GET_CLASS (renderer)->user_bounds (renderer, object,
                                                         left, top,
                                                         right, bottom);
}

int
eda_renderer_default_get_user_bounds (EdaRenderer *renderer, Object *object,
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
    /* No rendered bounds available for most Object types. */
    return FALSE;
  default:
    BUG_IMSG("object->type=%c\n", object->type);
    g_return_val_if_reached (FALSE);
  }
}

int
eda_renderer_get_text_user_bounds (EdaRenderer *renderer, Object *object,
                                   int         *left,     int *top,
                                   int         *right,    int *bottom)
{
  PangoRectangle inked_rect; /* logical_rect; */
  int adjustment;
  int ret_val = FALSE;
  int visible;

  visible = o_get_is_visible(object);

  /* First check if this is hidden text. */
  if ( visible || EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {

    /* Also, check that we actually need to display a string */
    if (object->text->disp_string != NULL) {

      cairo_save (renderer->priv->cr);

      /* Set up the text and check it worked. */
      if (eda_renderer_prepare_text (renderer, object)) {

        if (PANGO_IS_LAYOUT(renderer->priv->pl)) {

          /* Figure out the bounds, send them back. Note that Pango thinks
           * in device coordinates, but we need world coordinates. */
          pango_layout_get_pixel_extents (renderer->priv->pl, &inked_rect, NULL);


          double dleft   = (double) inked_rect.x;
          double dtop    = (double) inked_rect.y;
          double dright  = (double) inked_rect.x + inked_rect.width;
          double dbottom = (double) inked_rect.y + inked_rect.height;

          /* Does it does make sense to describe bounds in terms of 14
           * decimal places? Or even 2 decimal place? */
          cairo_user_to_device (renderer->priv->cr, &dleft,  &dtop);
          cairo_user_to_device (renderer->priv->cr, &dright, &dbottom);

          cairo_restore (renderer->priv->cr);

          cairo_device_to_user (renderer->priv->cr, &dleft,  &dtop);
          cairo_device_to_user (renderer->priv->cr, &dright, &dbottom);

          *left   = lrint(dleft);
          *top    = lrint(dtop);
          *right  = lrint(dright);
          *bottom = lrint(dbottom);

          /* If not normal visible text, account for the little "I" */
          if (object->visibility != 1) {
             adjustment = 2 * EDAR_TEXT_MARKER_SIZE + MIN_LINE_WIDTH_THRESHOLD;
            *bottom = *bottom - adjustment;
          }

          ret_val = TRUE;
        }
        else {
          BUG_MSG("Invalid pango_layout");
        }
      }
    }
  }

#ifdef DEBUG_RENDER_TEXT
  else
    fprintf(stderr, "skippping %s\n", object->text->disp_string);
#endif

  return ret_val;
}


/* ================================================================
 * MISCELLANEOUS (CREATION, DESTRUCTION, ACCESSORS)
 * ================================================================ */

EdaRenderer *
eda_renderer_new (cairo_t *cr, PangoContext *pc)
{
  return g_object_new (EDA_TYPE_RENDERER,
                       "cairo-context", cr,
                       "pango-context", pc,
                       NULL);
}

void
eda_renderer_destroy (EdaRenderer *renderer)
{
  if (G_IS_OBJECT(renderer)) {
    g_object_unref (G_OBJECT (renderer));
  }
  else {
    BUG_MSG("Bad pointer to EdaRenderer, is it aleady dead?");
  }
}

cairo_t *
eda_renderer_get_cairo_context (EdaRenderer *renderer)
{
  cairo_t *cr;
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  g_object_get (G_OBJECT (renderer), "cairo-context", &cr, NULL);
  return cr;
}

bool eda_renderer_get_hinting_enabled (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), FALSE);
  return EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING);
}

GArray * eda_renderer_get_color_map (EdaRenderer *renderer)
{
  GArray *map = NULL;
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  g_object_get (G_OBJECT (renderer), "color-map", &map, NULL);
  return map;
}
void
eda_renderer_set_color_map (EdaRenderer *renderer, GArray *map)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (G_OBJECT (renderer), "color-map", map, NULL);
}

const char *eda_renderer_get_font_name(EdaRenderer *renderer)
{
  return (renderer->priv->font_name);
}
void eda_renderer_set_font_name(EdaRenderer *renderer, const char *fontname)
{
  GEDA_FREE(renderer->priv->font_name);
  renderer->priv->font_name = u_string_strdup (fontname);
}

bool eda_renderer_set_flags (EdaRenderer *renderer, int flags)
{
  if (EDA_IS_RENDERER (renderer))
    renderer->priv->flags = flags;
  else
    return FALSE;
  return TRUE;
}
bool eda_renderer_mask_flags (EdaRenderer *renderer, int flags)
{
  if (EDA_IS_RENDERER (renderer))
    renderer->priv->flags |= flags;
  else
    return FALSE;
  return TRUE;
}
int eda_renderer_get_flags (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return renderer->priv->flags;
}
int eda_renderer_get_cairo_flags (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), 0);
  return EDA_RENDERER_CAIRO_FLAGS (renderer);
}

int eda_renderer_get_override_color_index (EdaRenderer *renderer)
{
  return renderer->priv->override_color;
}
void
eda_renderer_set_override_color_index (EdaRenderer *renderer, int color_index)
{
  renderer->priv->override_color = color_index;
}
double eda_renderer_get_grips_size (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return EDAR_GRIP_SIZE;
}
void eda_renderer_set_grips_size (EdaRenderer *renderer, double new_size)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (G_OBJECT (renderer), "grip-size", new_size, NULL);
}

const
GdkColor *eda_renderer_get_grips_stroke_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_GRIP_STROKE_COLOR;
}
void
eda_renderer_set_grips_stroke_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_GRIP_STROKE_COLOR.red   = color->red;
  EDAR_GRIP_STROKE_COLOR.green = color->green;
  EDAR_GRIP_STROKE_COLOR.blue  = color->blue;
}
const
GdkColor *eda_renderer_get_grips_fill_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_GRIP_FILL_COLOR;
}

void
eda_renderer_set_grips_fill_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_GRIP_FILL_COLOR.red   = color->red;
  EDAR_GRIP_FILL_COLOR.green = color->green;
  EDAR_GRIP_FILL_COLOR.blue  = color->blue;
}

const
GdkColor *eda_renderer_get_junction_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_JUNCTION_COLOR;
}
void
eda_renderer_set_junction_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_JUNCTION_COLOR.red   = color->red;
  EDAR_JUNCTION_COLOR.green = color->green;
  EDAR_JUNCTION_COLOR.blue  = color->blue;
}
int eda_renderer_get_junction_size (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return EDAR_JUNCTION_SIZE;
}
void eda_renderer_set_junction_size (EdaRenderer *renderer, int new_size)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (G_OBJECT (renderer), "junction-size", new_size, NULL);
}

const
GdkColor *eda_renderer_get_net_endpoint_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_NET_ENDPOINT_COLOR;
}

void
eda_renderer_set_net_endpoint_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_NET_ENDPOINT_COLOR.red   = color->red;
  EDAR_NET_ENDPOINT_COLOR.green = color->green;
  EDAR_NET_ENDPOINT_COLOR.blue  = color->blue;
}

const
GdkColor *eda_renderer_get_text_marker_color (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  return (const GdkColor*) &EDAR_TEXT_MARKER_COLOR;
}

void
eda_renderer_set_text_marker_color (EdaRenderer *renderer, GdkColor* color)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (color != NULL);

  EDAR_TEXT_MARKER_COLOR.red   = color->red;
  EDAR_TEXT_MARKER_COLOR.green = color->green;
  EDAR_TEXT_MARKER_COLOR.blue  = color->blue;
}
int eda_renderer_get_text_marker_size (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), -1);
  return EDAR_TEXT_MARKER_SIZE;
}
void eda_renderer_set_text_marker_size (EdaRenderer *renderer, int new_size)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (G_OBJECT (renderer), "text-marker-size", new_size, NULL);
}
