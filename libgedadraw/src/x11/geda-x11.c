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

int geda_x11_draw_text (GdkWindow *gdk_window, Object *object, edaColor *color, int x, int y)
{
  Display      *display;
  Window        window;
  Text         *o_text;
  const char   *string;
  
  int length;
  int screen;
  int size;
  int                     ytext, wtext;

#ifdef HAVE_XFT
  XftFont                 *font;
  XftDraw                 *xftdraw;
  XRenderColor            xrcolor;
  XftColor                xftcolor;
  XGlyphInfo              extents;
#else
  GC                      gc;
  XGCValues               gcvals;
  XFontStruct             *font;
#endif

  if (GEDA_IS_TEXT(object)) {
    
    window  = GDK_DRAWABLE_XID (gdk_window);
    display = GDK_DRAWABLE_XDISPLAY(gdk_window);
    screen  = DefaultScreen(display);
    
    XMapWindow(display,window);
    
    font   = NULL;
    o_text = (Text*)object;
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
    
    xftdraw = XftDrawCreate(display,window, DefaultVisual(display,0), DefaultColormap(display,0));

    xrcolor.red   = color->r;
    xrcolor.green = color->g;
    xrcolor.blue  = color->b;
    xrcolor.alpha = color->a;
    XftColorAllocValue(display,DefaultVisual(display,0),DefaultColormap(display,0),&xrcolor,&xftcolor);

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
    fprintf(stderr, "Draw text=<%s>, (%d,%d)\n", string, x , y);
    XFlush(display);
    
    XftDrawDestroy(xftdraw);
    XftColorFree(display,DefaultVisual(display,0),DefaultColormap(display,0),&xftcolor);
  }
  return 0;
}