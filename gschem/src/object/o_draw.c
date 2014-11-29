#include <gschem.h>
#include <geda_debug.h>

#include <geda_draw.h>

void
o_draw_text (GschemToplevel *w_current, Object *object,  GdkColor *c)
{
  Text    *otext;
  edaColor color;

  int sx, sy;

  /* First check if this is visible */
  if ( o_get_is_visible(object)) {
    otext = (Text*)object;
    color.r = c->red;
    color.g = c->green;
    color.b = c->blue;
    color.a = 255;
    WORLDtoSCREEN (w_current, otext->x,  otext->y, &sx,  &sy);
    geda_draw_text (w_current->drawable, object, &color, sx, sy);
  }
}