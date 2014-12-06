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
  Circle *ocircle;
  int scx, scy;
  int radius;

  if (GEDA_IS_CIRCLE(draw_data->object)) {
    ocircle = draw_data->object->circle;
    radius = pix_x(w_current, draw_data->object->circle->radius);
    WORLDtoSCREEN (w_current, ocircle->center_x,  ocircle->center_y, &scx,  &scy);
    geda_draw_circle (draw_data, scx, scy, radius);
  }
}

static void
o_draw_box (GschemToplevel *w_current, GedaDrawData *draw_data)
{
  Box *obox;
  int sx1, sy1, sx2, sy2;
  int width, height;

  if (GEDA_IS_BOX(draw_data->object)) {

    obox = draw_data->object->box;
    WORLDtoSCREEN (w_current, obox->upper_x,  obox->upper_y, &sx1,  &sy1);
    WORLDtoSCREEN (w_current, obox->lower_x,  obox->lower_y, &sx2,  &sy2);
    width  = sx2 - sx1;
    height = sy2 - sy1;
    geda_draw_box (draw_data, sx1, sy1, width, height);
  }
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
    case OBJ_LINE:        draw_func = o_draw_line;    break;
    case OBJ_NET:         draw_func = o_draw_net;     break;
    case OBJ_BUS:         draw_func = o_draw_bus;     break;
    case OBJ_PIN:         draw_func = o_draw_pin;     break;
    case OBJ_BOX:         draw_func = o_draw_box;     break;
    case OBJ_ARC:         draw_func = o_draw_arc;     break;
    case OBJ_CIRCLE:      draw_func = o_draw_circle;  break;
    case OBJ_PATH:        draw_func = o_draw_path;    break;
    case OBJ_TEXT:        draw_func = o_draw_text;    break;
    case OBJ_PICTURE:     draw_func = o_draw_picture; break;
    case OBJ_COMPLEX:
    case OBJ_PLACEHOLDER: draw_func = o_draw_complex;
      break;

    default:
      g_return_if_reached ();
  }

  draw_data->color.red    = c->red;
  draw_data->color.green  = c->green;
  draw_data->color.blue   = c->blue;

  draw_func (w_current, draw_data);
}