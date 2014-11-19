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

#ifndef __EDA_RENDERER_H__
#define __EDA_RENDERER_H__

G_BEGIN_DECLS

/*! \def MIN_MARKER_DISTENCE
  The minimum text marker size, "x" smaller then this value are not drawn
*/
#define EDAR_MARKER_DIST_THREASHOLD        2.3

#define EDAR_DEFAULT_GRIP_SIZE             100
#define EDAR_DEFAULT_JUNCTION_SIZE          50
#define EDAR_DEFAULT_TEXT_MARKER_SIZE       15
#define EDAR_MIN_TEXT_MARKER_SIZE            5
#define EDAR_MAX_TEXT_MARKER_SIZE          100

#define EDAR_DEFAULT_FONT_NAME          DEFAULT_FONT_NAME

#define EDAR_DESCENT_FACTOR    3.01728024042074
#define EDAR_DESCENT_OFFSET -886.034560480839

#define EDAR_DEFAULT_GRIP_STROKE_COLOR  "orange"
#define EDAR_DEFAULT_GRIP_FILL_COLOR    "black"
#define EDAR_DEFAULT_TEXT_MARKER_COLOR  "gray"
#define EDAR_DEFAULT_JUNCTION_COLOR     "yellow"
#define EDAR_DEFAULT_ENDPOINT_COLOR     "red"

#define EDAR_DEFAULT_OVERRIDE_COLOR_INDEX 1

/* These macros are used to help reduce lines lengths */
#define EDAR_GRIP_SIZE          renderer->grip_size
#define EDAR_GRIP_STROKE_COLOR  renderer->grip_stroke_color
#define EDAR_GRIP_FILL_COLOR    renderer->grip_fill_color

#define EDAR_JUNCTION_COLOR     renderer->junction_color
#define EDAR_JUNCTION_SIZE      renderer->junction_size
#define EDAR_NET_ENDPOINT_COLOR renderer->net_endpoint_color
#define EDAR_TEXT_MARKER_COLOR  renderer->text_marker_color
#define EDAR_TEXT_MARKER_SIZE   renderer->text_marker_size

#define EDA_TYPE_RENDERER (eda_renderer_get_type ())
#define EDA_RENDERER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDA_TYPE_RENDERER, EdaRenderer))
#define EDA_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDA_TYPE_RENDERER, EdaRendererClass))
#define EDA_IS_RENDERER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDA_TYPE_RENDERER))
#define EDA_IS_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EDA_TYPE_RENDERER))
#define EDA_RENDERER_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDA_TYPE_RENDERER, EdaRendererClass))

typedef struct _EdaRendererClass EdaRendererClass;
typedef struct _EdaRenderer EdaRenderer;
typedef struct _EdaRendererPrivate EdaRendererPrivate;

struct _EdaRendererClass
{
  GObjectClass parent_class;

  /* Virtual public methods */
  void (*draw)         (EdaRenderer *renderer, Object *object);
  void (*draw_grips)   (EdaRenderer *renderer, Object *object);
  void (*draw_cues)    (EdaRenderer *renderer, Object *object);
  int  (*user_bounds)  (EdaRenderer *renderer, Object *object,
                        int *left,  int *top,
                        int *right, int *bottom);
};

struct _EdaRenderer
{
  GObject parent_instance;

  /* Public members */
  int      draw_grips;         /* controls if grips are enabled or not */
  double   grip_size;
  GdkColor grip_stroke_color;
  GdkColor grip_fill_color;

  /* TODO int      min_width; */

  int      junction_size;
  GdkColor junction_color;     /* The stroke color to be used for rendering junctions */
  GdkColor net_endpoint_color; /* The stroke color to be used for net/pin end points */

  int      text_origin_marker; /* controls if text origin marker is displayed or not */
  int      text_marker_size;   /* controls the size of text origin markers */
  GdkColor text_marker_color;  /* The stroke color to be used for text origin marker */

  /* Private members */
  EdaRendererPrivate *priv;
};

#define EDA_TYPE_RENDERER_FLAGS (eda_renderer_flags_get_type ())

typedef enum _EdaRendererFlags EdaRendererFlags;

enum _EdaRendererFlags
{
  /* Should hinting be enabled? */
  EDA_RENDERER_FLAG_HINTING         = 1 << 0,

  /* Should picture outlines be drawn instead of raster? */
  EDA_RENDERER_FLAG_PICTURE_OUTLINE = 1 << 1,

  /* Should hidden text be drawn? */
  EDA_RENDERER_FLAG_TEXT_HIDDEN     = 1 << 2,

  /* Should text outlines be drawn instead of glyphs? */
  EDA_RENDERER_FLAG_TEXT_OUTLINE    = 1 << 3,

  /* Should text origin markers be drawn? */
  EDA_RENDERER_FLAG_TEXT_ORIGIN     = 1 << 4,

};

GedaType eda_renderer_get_type       (void) G_GNUC_CONST;
GedaType eda_renderer_flags_get_type (void) G_GNUC_CONST;

EdaRenderer *eda_renderer_new     (cairo_t *cr, PangoContext *pc) G_GNUC_WARN_UNUSED_RESULT;
void         eda_renderer_destroy (EdaRenderer *renderer);

void eda_renderer_draw            (EdaRenderer *renderer, Object *object);
void eda_renderer_draw_grips      (EdaRenderer *renderer, Object *object);
void eda_renderer_draw_grips_list (EdaRenderer *renderer, GList *objects);
void eda_renderer_draw_cues       (EdaRenderer *renderer, Object *object);

int  eda_renderer_get_user_bounds (EdaRenderer *renderer, Object *object,
                                   int *left,   int *top,
                                   int *right,  int *bottom);

int  eda_renderer_get_text_user_bounds (EdaRenderer *renderer, Object *object,
                                        int *left,   int *top,
                                        int *right,  int *bottom);

GArray  *eda_renderer_get_color_map (EdaRenderer *renderer);
void     eda_renderer_set_color_map (EdaRenderer *renderer, GArray *map);

cairo_t *eda_renderer_get_cairo_context (EdaRenderer *renderer);
int      eda_renderer_get_cairo_flags   (EdaRenderer *renderer);

const
char    *eda_renderer_get_font_name     (EdaRenderer *renderer);
void     eda_renderer_set_font_name     (EdaRenderer *renderer, const char *name);

bool     eda_renderer_set_flags   (EdaRenderer *renderer, int flags);
int      eda_renderer_get_flags   (EdaRenderer *renderer);
bool     eda_renderer_mask_flags  (EdaRenderer *renderer, int flags);

int      eda_renderer_get_override_color_index (EdaRenderer *renderer);
void     eda_renderer_set_override_color_index (EdaRenderer *renderer,
                                                int          color_index);

double   eda_renderer_get_grips_size    (EdaRenderer *renderer);
void     eda_renderer_set_grips_size    (EdaRenderer *renderer,
                                         double       new_size);
const
GdkColor* eda_renderer_get_grips_stroke_color (EdaRenderer *renderer);
void      eda_renderer_set_grips_stroke_color (EdaRenderer *renderer,
                                               GdkColor    *color);
const
GdkColor* eda_renderer_get_grips_fill_color   (EdaRenderer *renderer);
void      eda_renderer_set_grips_fill_color   (EdaRenderer *renderer,
                                               GdkColor    *color);
const
GdkColor* eda_renderer_get_junction_color     (EdaRenderer *renderer);
void      eda_renderer_set_junction_color     (EdaRenderer *renderer,
                                               GdkColor*    color);
int       eda_renderer_get_junction_size      (EdaRenderer *renderer);
void      eda_renderer_set_junction_size      (EdaRenderer *renderer,
                                               int          new_size);
const
GdkColor* eda_renderer_get_net_endpoint_color (EdaRenderer *renderer);
void      eda_renderer_set_net_endpoint_color (EdaRenderer *renderer,
                                               GdkColor    *color);
const
GdkColor* eda_renderer_get_text_marker_color  (EdaRenderer *renderer);
void      eda_renderer_set_text_marker_color  (EdaRenderer *renderer,
                                               GdkColor    *color);
int       eda_renderer_get_text_marker_size   (EdaRenderer *renderer);
void      eda_renderer_set_text_marker_size   (EdaRenderer *renderer,
                                               int new_size);
G_END_DECLS

#endif /* !__EDA_RENDERER_H__ */
