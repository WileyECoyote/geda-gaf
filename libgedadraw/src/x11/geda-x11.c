/* -lX11 -lXft `pkg-config --cflags freetype2 */

#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#define Picture xcrap

#include<unistd.h>
#include<X11/Xlib.h>
#include<X11/Xutil.h>
#include<X11/Xft/Xft.h>

#undef Complex
#undef Picture

#include <libgeda/libgeda.h>
#include <geda_draw.h>

static void XSetColor(GedaDrawData *draw_data)
{
  draw_data->color.flags = DoRed | DoGreen | DoBlue;

  draw_data->colormap = DefaultColormap(draw_data->display, draw_data->screen);

  XAllocColor(draw_data->display, draw_data->colormap, &draw_data->color);

  XSetForeground(draw_data->display,draw_data->gc,draw_data->color.pixel);

  return;
}

static inline int
LINE_WIDTH (int line_width) {
  return max (line_width, MIN_LINE_WIDTH_THRESHOLD) / 12;
}

static inline unsigned int
set_line_attributes(GedaDrawData *draw_data, XGCValues *gcvals, int total)
{
  Display *display = draw_data->display;
  Object  *object  = draw_data->object;

  char dash_list[6];
  int  dash_offset;
  int  num_dash;
  int  length;
  int  space;
  int  length_factor;
  int  space_factor;

  switch (object->line_options->line_end) {
    case END_NONE:   gcvals->cap_style = CapButt;   break;
    case END_SQUARE: gcvals->cap_style = CapProjecting; break;
    case END_ROUND:  gcvals->cap_style = CapRound;  break;
    default:         gcvals->cap_style = CapButt;   break;
  }

  if ( object->line_options->line_type == TYPE_SOLID) {
    gcvals->line_style = LineSolid;
  }
  else {

    dash_offset = 0;
    length = object->line_options->line_length;
    space  = object->line_options->line_space;

    if ( length < total / 20) {
      length_factor = length > 0 ? length : 100;
      space_factor  = space > 0 ? space : 50;
    }
    else {
      length_factor = length > 2 ? length / 2 : 50;
      space_factor  = space > 2 ? space / 2 : 25;
    }

    length_factor = length_factor ? length_factor : length;

    switch (object->line_options->line_type) {

      case TYPE_DOTTED:
        dash_list[0]      = 1;
        dash_list[1]      = space_factor;
        num_dash          = 2;
        XSetDashes(display, draw_data->gc, dash_offset, dash_list, num_dash);
        gcvals->cap_style = CapRound;
        gcvals->line_style = LineOnOffDash;
        break;

      case TYPE_DASHED:
        dash_list[0]      = length_factor;
        dash_list[1]      = space_factor;
        num_dash          = 2;
        XSetDashes(display, draw_data->gc, dash_offset, dash_list, num_dash);
        gcvals->line_style = LineOnOffDash;
        break;

      case TYPE_CENTER:
        dash_list[0]      = length_factor * 9;
        dash_list[1]      = space_factor  / 2;
        dash_list[2]      = length_factor / 3;
        dash_list[3]      = space_factor  / 2;
        num_dash          = 4;
        XSetDashes(display, draw_data->gc, dash_offset, dash_list, num_dash);
        gcvals->line_style = LineOnOffDash;
        break;

      case TYPE_PHANTOM:
        dash_list[0]      = length_factor * 6;
        dash_list[1]      = space_factor  / 2;
        dash_list[2]      = length_factor / 3;
        dash_list[3]      = space_factor  / 2;
        dash_list[4]      = length_factor / 3;
        dash_list[5]      = space_factor  / 2;
        num_dash          = 6;
        XSetDashes(display, draw_data->gc, dash_offset, dash_list, num_dash);
        gcvals->line_style = LineOnOffDash;
        break;

      default:
        g_warn_if_reached ();
        gcvals->line_style = LineSolid;
        break;
    }
  }

  gcvals->line_width = LINE_WIDTH(object->line_options->line_width);

  return GCCapStyle | GCLineStyle | GCLineWidth;
}

int geda_x11_draw_box (GedaDrawData *draw_data, int x, int y, int width, int height)
{
  Display *display = draw_data->display;
  XGCValues gcvals;
  unsigned int bits;
  int length;

  XSetColor(draw_data);

  length = min(width, height);

  bits = set_line_attributes(draw_data, &gcvals, length);

  XChangeGC(display, draw_data->gc, bits, &gcvals);

  XDrawRectangle(draw_data->display, draw_data->drawable, draw_data->gc, x, y, width, height);

  return 0;
}

int geda_x11_draw_circle (GedaDrawData *draw_data, int cx, int cy, int radius)
{
  Display *display = draw_data->display;
  XGCValues gcvals;

  int x, y;
  int circum;

  //int angle1, angle2;
  unsigned int half_radius;
  unsigned int bits;

  XSetColor(draw_data);

  circum = m_circumference(radius);
  bits   = set_line_attributes(draw_data, &gcvals, circum);

  XChangeGC(display, draw_data->gc, bits, &gcvals);

  half_radius = radius / 2;
  x           = cx - half_radius;
  y           = cy - half_radius;

  XDrawArc(display, draw_data->drawable, draw_data->gc, x, y, radius, radius, 0, 360*64);

  return 0;
}

int geda_x11_draw_line (GedaDrawData *draw_data, int x1, int y1, int x2, int y2)
{
  Display *display = draw_data->display;
  XGCValues gcvals;
  unsigned int bits;
  int length;

  XSetColor(draw_data);

  length = m_line_length(draw_data->object->line);

  bits = set_line_attributes(draw_data, &gcvals, length);

  XChangeGC(display, draw_data->gc, bits, &gcvals);

  XDrawLine(draw_data->display, draw_data->drawable, draw_data->gc, x1, y1, x2, y2);

  return 0;
}

int geda_x11_draw_net (GedaDrawData *draw_data, int x1, int y1, int x2, int y2)
{
  Display *display = draw_data->display;
  XGCValues gcvals;

  XSetColor(draw_data);

  gcvals.line_width = LINE_WIDTH(draw_data->object->line_options->line_width);

  XChangeGC(display, draw_data->gc, GCLineWidth, &gcvals);

  XDrawLine(draw_data->display, draw_data->drawable, draw_data->gc, x1, y1, x2, y2);

  return 0;
}

int geda_x11_draw_text (GedaDrawData *draw_data, int x, int y)
{
  Display      *display;
  Window        window;
  Text         *o_text;
  char         *font_string;
  const char   *font_name;
  const char   *string;

  int length;
  int screen;
  int size;
  //int ytext, wtext;

#ifdef HAVE_XFT
  XftFont      *font;
  XftDraw      *xftdraw;
  XftColor      xftcolor;
  XGlyphInfo    extents;
  XRenderColor xrcolor;
#else
  XFontStruct  *font;
  XGCValues     gcvals;

#endif

  if (GEDA_IS_TEXT(draw_data->object)) {

    window  = draw_data->drawable;
    display = draw_data->display;
    screen  = draw_data->screen;

    font   = NULL;
    o_text = (Text*)draw_data->object;
    size   = (o_text->size / draw_data->scale) * 10;
    string = o_text->disp_string;
    length = strlen(string);

    if (draw_data->font_name) {
      font_name = draw_data->font_name;
    }
    else {
#ifdef HAVE_XFT
      font_name = "morpheus";
#else
      font_name = "helvetica";
#endif
    }

#ifdef HAVE_XFT
      font_string = u_string_sprintf("%s-%d", font_name, size);
#else
      font_string = u_string_sprintf("-*-%s-medium-r-normal--%d-*-*-*-p-100-iso8859-1", font_name, size);
#endif

#ifdef HAVE_XFT

    /* set up font */
    font = XftFontOpenName( display, screen, font_string);
    if (font) {

      xftdraw = XftDrawCreate(display, window, DefaultVisual(display,0), DefaultColormap(display,0));

      /* Set up the color */
      xrcolor.red   = draw_data->color.red;
      xrcolor.green = draw_data->color.green;
      xrcolor.blue  = draw_data->color.blue;
      xrcolor.alpha = 255;

      /* Allocate drawable */
      XftColorAllocValue(display, DefaultVisual(display,0),DefaultColormap(display,0),&xrcolor,&xftcolor);

      /* position and size of top window (XSizeHints) */
      XftTextExtents8( display, font, (XftChar8 *)string, length, &extents );
      //ytext = extents.height - extents.y;
      //wtext = extents.width - extents.x;

      /* Draw the test */
      XftDrawString8(xftdraw, &xftcolor, font, x, y , (XftChar8 *)string, length);

      /* Release resoures */
      XftDrawDestroy(xftdraw);
      XftColorFree(display,DefaultVisual(display,0),DefaultColormap(display,0),&xftcolor);
#else
    /* set up font */
    font = XLoadQueryFont( display, font_string);

    if (font) {

      XSetColor(draw_data);
      //ytext = font->max_bounds.ascent + font->max_bounds.descent;
      //wtext = font->max_bounds.width / 2 + XTextWidth( font, string, length + 4 );

      XDrawString(draw_data->display, draw_data->drawable, draw_data->gc, x, y, string, length);
#endif
    }
    XFlush(display);
    GEDA_FREE(font_string);
  }
  return 0;
}