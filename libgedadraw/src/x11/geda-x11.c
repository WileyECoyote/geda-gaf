/* -lX11 -lXft `pkg-config --cflags freetype2 */

#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include<unistd.h>
#include<X11/Xlib.h>
#include<X11/Xutil.h>
#include<X11/Xft/Xft.h>

int geda_x11_draw_text (GdkWindow *gdk_window)
{
  Display      *display;
  Window        window;
  XftFont      *font;
  XftDraw      *xftdraw;
  XRenderColor  xrcolor;
  XftColor      xftcolor;
  int           screen;
 
  window  = GDK_DRAWABLE_XID (gdk_window);
  display = GDK_DRAWABLE_XDISPLAY(gdk_window);
  screen  = DefaultScreen(display);

  XMapWindow(display,window);

  font = NULL; /* added 6/16 */

  //font = XftFontOpenName(display,0,"NorthKorea-50"); // how to check if this is good ?
  font = XftFontOpenName(display,0,""); /* added 6/16 */
  if (!font) return 1;

  xftdraw = XftDrawCreate(display,window,DefaultVisual(display,0),DefaultColormap(display,0));

  xrcolor.red  =65535;
  xrcolor.green=0;
  xrcolor.blue =0;
  xrcolor.alpha=65535;
  XftColorAllocValue(display,DefaultVisual(display,0),DefaultColormap(display,0),&xrcolor,&xftcolor);

  XftDrawString8(xftdraw, &xftcolor, font, 20,70 , (XftChar8 *)"Joe Dalton", 10);

  XFlush(display);

  sleep(2);

  XftDrawDestroy(xftdraw);
  XftColorFree(display,DefaultVisual(display,0),DefaultColormap(display,0),&xftcolor);

  return 0;
}