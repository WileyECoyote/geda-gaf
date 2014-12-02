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

static int buildColor(GedaDrawData *draw_data)
{
  int red   = ((draw_data->color.red*255)%256)<<16;
  int green = ((draw_data->color.green*255)%256)<<8;
  int blue  =  (draw_data->color.blue*255)%256;

  return red + green + blue;
} 

static inline int
LINE_WIDTH (int line_width) {
  return max (line_width, MIN_LINE_WIDTH_THRESHOLD) / 2;
}

int geda_x11_draw_line (GedaDrawData *draw_data, int x1, int y1, int x2, int y2)
{
  Display *display = draw_data->display;
  Object *object   = draw_data->object;
  XGCValues gcvals;

  gcvals.line_width = LINE_WIDTH(object->line_options->line_width);
  XChangeGC(draw_data->display, draw_data->gc, GCLineWidth, &gcvals);
  XSetForeground(display, draw_data->gc, buildColor(draw_data)); 
//fprintf (stderr, "from (%d,%d) to (%d,%d)\n", x1, y1, x2, y2);
  XDrawLine(draw_data->display, draw_data->drawable, draw_data->gc, x1, y1, x2, y2);
  return 0;
}

int geda_x11_draw_text (GedaDrawData *draw_data, int x, int y)
{
  Display      *display;
  Window        window;
  Text         *o_text;
  const char   *string;

  int length;
  int screen;
  int size;
  int ytext, wtext;

#ifdef HAVE_XFT
  XftFont      *font;
  XftDraw      *xftdraw;
  XftColor      xftcolor;
  XGlyphInfo    extents;
#else
  XGCValues     gcvals;
  XFontStruct  *font;
#endif
  XRenderColor xrcolor;

  if (GEDA_IS_TEXT(draw_data->object)) {

    window  = draw_data->drawable;
    display = draw_data->display;
    screen  = draw_data->screen;

    font   = NULL;
    o_text = (Text*)draw_data->object;
    size   = o_text->size;
    string = o_text->disp_string;
    length = strlen(string);

    /* set up font */
#ifdef HAVE_XFT
    font = XftFontOpenName( display, screen, "morpheus-10" );
#else
    font = XLoadQueryFont( display, "-*-helvetica-medium-r-normal--10-*-*-*-p-100-iso8859-1" );
#endif

    if (!font) return 1;

    xftdraw = XftDrawCreate(display, window, DefaultVisual(display,0), DefaultColormap(display,0));

    xrcolor.red   = draw_data->color.red;
    xrcolor.green = draw_data->color.green;
    xrcolor.blue  = draw_data->color.blue;
    xrcolor.alpha = 255;

    XftColorAllocValue(display, DefaultVisual(display,0),DefaultColormap(display,0),&xrcolor,&xftcolor);

    /* position and size of top window (XSizeHints) */
#ifdef HAVE_XFT
    XftTextExtents8( display, font, (XftChar8 *)string, length, &extents );
    ytext = extents.height - extents.y;
    wtext = extents.width - extents.x;
#else
    ytext = font->max_bounds.ascent + font->max_bounds.descent;
    wtext = font->max_bounds.width / 2 + XTextWidth( font, text, strlen(text) + 4 );
#endif   

    XftDrawString8(xftdraw, &xftcolor, font, x, y , (XftChar8 *)string, length);
    //fprintf(stderr, "Draw text=<%s>, (%d,%d)\n", string, x , y);
    XFlush(display);

    XftDrawDestroy(xftdraw);
    XftColorFree(display,DefaultVisual(display,0),DefaultColormap(display,0),&xftcolor);
  }
  return 0;
}