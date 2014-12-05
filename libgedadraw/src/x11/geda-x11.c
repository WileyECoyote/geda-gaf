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

static unsigned long buildColor(GedaDrawData *draw_data)
{
  int red   = ((draw_data->color.red*255)%256)<<16;
  int green = ((draw_data->color.green*255)%256)<<8;
  int blue  =  (draw_data->color.blue*255)%256;

  return red + green + blue;
}

static inline int
LINE_WIDTH (int line_width) {
  return max (line_width, MIN_LINE_WIDTH_THRESHOLD) / 12;
}

static inline unsigned int
set_line_attributes(GedaDrawData *draw_data, XGCValues *gcvals)
{
  Object *object = draw_data->object;

  int  dash_offset;
  int  num_dash;
  char dash_list[6];

  switch (object->line_options->line_end) {
    case END_NONE:   gcvals->cap_style = CapButt;   break;
    case END_SQUARE: gcvals->cap_style = CapProjecting; break;
    case END_ROUND:  gcvals->cap_style = CapRound;  break;
    default:         gcvals->cap_style = CapButt;   break;
  }

  dash_offset = 0;

  switch (object->line_options->line_type) {

    default:
      g_warn_if_reached ();
      /* Fall through */

    case TYPE_SOLID:
      gcvals->line_style = LineSolid;
      break;

    case TYPE_DOTTED:
      dash_list[0]      = 1;
      dash_list[1]      = object->line_options->line_space;
      num_dash          = 2;
      XSetDashes(draw_data->display, draw_data->gc, dash_offset, dash_list, num_dash);
      gcvals->cap_style = CapRound;
      gcvals->line_style = LineOnOffDash;
      break;

    case TYPE_DASHED:
      dash_list[0]      = object->line_options->line_length;
      dash_list[1]      = object->line_options->line_space;
      num_dash          = 2;
      XSetDashes(draw_data->display, draw_data->gc, dash_offset, dash_list, num_dash);
      gcvals->line_style = LineOnOffDash;
      break;

    case TYPE_CENTER:
      dash_list[0]      = object->line_options->line_length * 10;
      dash_list[1]      = object->line_options->line_space  / 2;
      dash_list[2]      = object->line_options->line_length / 3;
      dash_list[3]      = object->line_options->line_space  / 2;
      num_dash          = 4;
      XSetDashes(draw_data->display, draw_data->gc, dash_offset, dash_list, num_dash);
      gcvals->line_style = LineOnOffDash;
      break;

    case TYPE_PHANTOM:
      dash_list[0]      = object->line_options->line_length * 10;
      dash_list[1]      = object->line_options->line_space  / 2;
      dash_list[2]      = object->line_options->line_length / 3;
      dash_list[3]      = object->line_options->line_space  / 2;
      dash_list[4]      = object->line_options->line_length / 3;
      dash_list[5]      = object->line_options->line_space  / 2;
      num_dash          = 6;
      XSetDashes(draw_data->display, draw_data->gc, dash_offset, dash_list, num_dash);
      gcvals->line_style = LineOnOffDash;
      break;
  }

  gcvals->line_width = LINE_WIDTH(object->line_options->line_width);

  return GCCapStyle | GCLineStyle | GCLineWidth;
}

int geda_x11_draw_line (GedaDrawData *draw_data, int x1, int y1, int x2, int y2)
{
  Display *display = draw_data->display;
  XGCValues gcvals;
  unsigned int bits;

  XSetForeground(display, draw_data->gc, buildColor(draw_data));

  bits = set_line_attributes(draw_data, &gcvals);

  XChangeGC(display, draw_data->gc, bits, &gcvals);

  XDrawLine(draw_data->display, draw_data->drawable, draw_data->gc, x1, y1, x2, y2);

  return 0;
}

int geda_x11_draw_net (GedaDrawData *draw_data, int x1, int y1, int x2, int y2)
{
  Display *display = draw_data->display;
  XGCValues gcvals;

  XSetForeground(display, draw_data->gc, buildColor(draw_data));

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

      XSetForeground(display, draw_data->gc, buildColor(draw_data));

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