#include <gschem.h>
#include <geda_draw.h>
#include <o_draw.h>   /* after geda_draw.h */

#include <geda_debug.h>

static void
o_draw_arc (GschemToplevel *w_current, GedaDrawData *draw_data)
{

}
static void
o_draw_circle (GschemToplevel *w_current, GedaDrawData *draw_data)
{
}
static void
o_draw_box (GschemToplevel *w_current, GedaDrawData *draw_data)
{

}

static void
o_draw_line (GschemToplevel *w_current, GedaDrawData *draw_data)
{
  Line *oline;
  int sx1, sy1, sx2, sy2;

  if (GEDA_IS_LINE(draw_data->object)) {

    oline = draw_data->object->line;
    WORLDtoSCREEN (w_current, oline->x[0],  oline->y[0], &sx1,  &sy1);
    WORLDtoSCREEN (w_current, oline->x[1],  oline->y[1], &sx2,  &sy2);
    geda_draw_line (draw_data, sx1, sy1, sx2, sy2);
  }
}

static void
o_draw_bus (GschemToplevel *w_current, GedaDrawData *draw_data)
{
  o_draw_line (w_current, draw_data);
}

static void
o_draw_net (GschemToplevel *w_current, GedaDrawData *draw_data)
{
  Line *oline;
  int sx1, sy1, sx2, sy2;

  if (GEDA_IS_LINE(draw_data->object)) {

    oline = draw_data->object->line;
    WORLDtoSCREEN (w_current, oline->x[0],  oline->y[0], &sx1,  &sy1);
    WORLDtoSCREEN (w_current, oline->x[1],  oline->y[1], &sx2,  &sy2);
    geda_draw_net (draw_data, sx1, sy1, sx2, sy2);
  }
}
static void
o_draw_path (GschemToplevel *w_current, GedaDrawData *draw_data)
{

}
static void
o_draw_pin (GschemToplevel *w_current, GedaDrawData *draw_data)
{
  o_draw_line (w_current, draw_data);
}
static void
o_draw_picture (GschemToplevel *w_current, GedaDrawData *draw_data)
{

}

static void
o_draw_text (GschemToplevel *w_current, GedaDrawData *draw_data)
{
  Text *otext;
  int   sx, sy;

  /* First check if this is visible */
  if ( o_get_is_visible(draw_data->object)) {

    draw_data->color.red    = draw_data->color.red   * 65535;
    draw_data->color.green  = draw_data->color.green * 65535;
    draw_data->color.blue   = draw_data->color.blue * 65535;
    draw_data->font_name    = DEFAULT_FONT_NAME;

    otext = (Text*)draw_data->object;

    WORLDtoSCREEN (w_current, otext->x,  otext->y, &sx,  &sy);
    geda_draw_text (draw_data, sx, sy);
  }
}

static void
o_draw_complex (GschemToplevel *w_current, GedaDrawData *draw_data)
{
  GList    *iter;

  iter = draw_data->object->complex->prim_objs;

  while (iter) {
    draw_data->object = iter->data;
    GdkColor *color = x_color_get_color_from_index(draw_data->object->color);
    o_draw_object(w_current, draw_data, color);
    iter = iter->next;
  }
}

void
o_draw_object (GschemToplevel *w_current, GedaDrawData *draw_data, GdkColor *c)
{
  void (*draw_func)(GschemToplevel *w_current, GedaDrawData *draw_data);

  g_return_if_fail (draw_data->object != NULL);

  switch (draw_data->object->type) {
    case OBJ_LINE:        draw_func = o_draw_line; break;
    case OBJ_NET:         draw_func = o_draw_net; break;
    case OBJ_BUS:         draw_func = o_draw_bus; break;
    case OBJ_PIN:         draw_func = o_draw_pin; break;
    case OBJ_BOX:         draw_func = o_draw_box; break;
    case OBJ_ARC:         draw_func = o_draw_arc; break;
    case OBJ_CIRCLE:      draw_func = o_draw_circle; break;
    case OBJ_PATH:        draw_func = o_draw_path; break;
    case OBJ_TEXT:        draw_func = o_draw_text; break;
    case OBJ_PICTURE:     draw_func = o_draw_picture; break;
    case OBJ_COMPLEX:
    case OBJ_PLACEHOLDER: draw_func = o_draw_complex;

    break;

    default:
      g_return_if_reached ();
  }

  draw_data->color.red    = c->red   / 65535;
  draw_data->color.green  = c->green / 65535;
  draw_data->color.blue   = c->blue  / 65535;
  draw_data->color.flags = DoRed | DoGreen | DoBlue;

  draw_func (w_current, draw_data);
}