/* -*- C geda_x11.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: geda_x11.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 * Date: Dec, 25, 2014
 * Contributing Author: Wiley Edward Hill
 *
*/
/*!
 * \file geda_x11.c
 * \brief Libgedadraw Drawing routines for X11.
 *
 * \remarks This module is under development
 *
 * -lX11 -lXft `pkg-config --cflags freetype2
 */
#include <string>
#include <cmath>

#include <gdk/gdk.h>

#define Picture xpicture

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include <X11/Xft/Xft.h>

#include <cairo-xlib.h>
#undef Complex
#undef Picture

#include <libgeda/libgeda.h>

#include <geda_draw.h>

inline std::string
EdaX11Render::GetFontString(int size) {

  char *tmp_string = u_string_sprintf(font_format.c_str(), size);

  std::string str = tmp_string;

  GEDA_FREE(tmp_string);

  return str.data();
}

/* Used run-time by the text renderer funtion to determine if the
 * current font needs to be updated */
bool
EdaX11Render::QueryCurrentFont (const char *font_name, int size)
{
  int   new_size;
  bool  update;

  update = false;

  if (font_name == NULL) {
    font_name = font_family.c_str();
  }
  else {
    update = true;
  }

  new_size = rint((size / scale) * FONT_SIZE_FACTOR);

  //fprintf(stderr, "scale <%f> size <%d>, new_size <%d>, font_size=<%d>\t", scale, size, new_size, font_size);

  if (font_size != new_size || update) {

    font_size = new_size;

    std::string new_string = GetFontString(new_size);

    if (!font_string.compare(new_string)) {
      update = false;
    }
    else {
      font_string = new_string;
      update = true;
    }
  }

  //fprintf(stderr, "font string=%s, update=%d\n", font_string.c_str(), update);
  return update;
}
/*
 * This routine returns True if the font string is a properly formed
 * XLFD styled font name with a pixel size, point size, and average
 * width (fields 7,8, and 12) are "0".
 */
bool EdaX11Render::IsScalableFont(char *name)
{
  int i, field;
  bool anwser;

  if ((name == NULL) || (name[0] != '-')) {
    anwser = false;
  }
  else {

    anwser = true;

    for(i = field = 0; name[i] != '\0' && field <= 14; i++) {
      if (name[i] == '-') {
        field++;
        if ((field == 7) || (field == 8) || (field == 12)) {
          if ((name[i+1] != '0') || (name[i+2] != '-')) {
            anwser = false;
            break;
          }
        }
      }
    }

    if (anwser) {
      if (field != 14) {
        anwser = false;
      }
    }
  }
  return anwser;
}

#if DEBUG
int EdaX11Render::XSetColorRed(void)
{
  int status;
  XColor red;
  Colormap colormap = DefaultColormap(display, screen);
  status = XAllocNamedColor(display, colormap, "red", &red, &red);
  if (status)
    XSetForeground(display, gc, red.pixel);
  return status;
}
#endif

/* Call from all drawing routines to set line properties,
 * i.e. cap-style, style and width. The total (length)
 * parameter is used to adjust the dash-length and spaces
 * for dash, center and phantom styles so tht short lines
 * look better when the specified dash-length exceeds 20%
 * of the segment length. Could be improved further but is
 * better than what we do with Cairo renderer.
 */
unsigned int
EdaX11Render::SetLineAttributes(XGCValues *gcvals, int total)
{
  char dash_list[6];
  int  dash_offset;
  int  num_dash;
  int  length;
  int  space;
  int  length_factor;
  int  space_factor;
  bool success;

  if (GEDA_IS_OBJECT(object)) {

    switch (object->line_options->line_end) {
      case END_NONE:   gcvals->cap_style = CapButt;   break;
      case END_SQUARE: gcvals->cap_style = CapProjecting; break;
      case END_ROUND:  gcvals->cap_style = CapRound;  break;
      default:         gcvals->cap_style = CapButt;   break;
    }

    if (object->line_options->line_type == TYPE_SOLID) {
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
          XSetDashes(display, gc, dash_offset, dash_list, num_dash);
          gcvals->cap_style = CapRound;
          gcvals->line_style = LineOnOffDash;
          break;

        case TYPE_DASHED:
          dash_list[0]      = length_factor;
          dash_list[1]      = space_factor;
          num_dash          = 2;
          XSetDashes(display, gc, dash_offset, dash_list, num_dash);
          gcvals->line_style = LineOnOffDash;
          break;

        case TYPE_CENTER:
          dash_list[0]      = length_factor * 9;
          dash_list[1]      = space_factor  / 2;
          dash_list[2]      = length_factor / 3;
          dash_list[3]      = space_factor  / 2;
          num_dash          = 4;
          XSetDashes(display, gc, dash_offset, dash_list, num_dash);
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
          XSetDashes(display, gc, dash_offset, dash_list, num_dash);
          gcvals->line_style = LineOnOffDash;
          break;

        default:
          g_warn_if_reached ();
          gcvals->line_style = LineSolid;
          break;
      }
    }

    gcvals->line_width = GetLineWidth(object->line_options->line_width);
    success = true;
  }
  else {
    success = false;
  }
  return success ? GCCapStyle | GCLineStyle | GCLineWidth : 0;
}

void EdaX11Render::DrawBezierCurve (XPoint *points)
{
  double A, B, C, D, E, F, G, H;  /* The Coefficients */
  double time;
  double step;

  int from_x, from_y, to_x, to_y;

  /* Calculation Coefficients */
  int x0 = points[0].x;
  int y0 = points[0].y;
  int x1 = points[1].x;
  int y1 = points[1].y;
  int x2 = points[2].x;
  int y2 = points[2].y;
  int x3 = points[3].x;
  int y3 = points[3].y;

  A = x3 - 3 * x2  + 3 * x1 - x0;
  B = 3 * x2 - 6 * x1 + 3 * x0;
  C = 3 * x1 - 3 * x0;
  D = x0;
  E = y3 - 3 * y2 + 3 * y1 - y0;
  F = 3 * y2 - 6 * y1 + 3 * y0;
  G = 3 * y1 - 3 * y0;
  H = y0;

  from_x = x0;
  from_y = y0;
  step   = BEZIER_STEP;

  /* Step from 0 to 1 in BEZIER_STEP increments and
   * calculate X,Y and draw a line from previous point */
  for(time = step; time <= 1; time = time + step) {

    to_x = ((( A * time ) + B ) * time + C ) * time + D;
    to_y = ((( E * time ) + F ) * time + G ) * time + H;

    XDrawLine(display, drawable, gc, from_x, from_y, to_x, to_y);

#if DEBUG
    XSetColorRed();
    XDrawPoint(display, drawable, gc, from_x, from_y);
#endif

    from_x = to_x;
    from_y = to_y;
  }
}

void
EdaX11Render::TextAlignSetBounds (int length, int sx, int sy, int *x_left, int *y_lower)
{
  Text       *o_text = object->text;
  const char *string = o_text->disp_string;

  int s_ascent;
  int s_descent;

  int width;
  long unsigned int eol_sp;

  int s_left;
  int s_lower;

  int w_ascent;
  int w_descent;
  int w_height;

  int w_left;
  int w_right;
  int w_bottom;
  int w_top;

#ifdef HAVE_XFT

  XGlyphInfo  extents;

  XftTextExtents8 (display, font, (XftChar8 *)string, length, &extents);
  s_ascent  = font->ascent;
  s_descent = font->descent;
  width     = extents.width;
  eol_sp    = EDA_DEFAULT_EOL_SP;

#else

  s_ascent  = font->max_bounds.ascent;
  s_descent = font->max_bounds.descent;
  width     = XTextWidth (font, string, length);

  /* additional end-of-line spacing */
  if (!XGetFontProperty(font, XA_END_SPACE, &eol_sp)) {
    eol_sp = EDA_DEFAULT_EOL_SP;
  }

#endif

  w_ascent  = rint (s_ascent * scale);
  w_descent = rint (s_descent * scale);
  w_height  = w_ascent + w_descent;

  switch (o_text->alignment) {
    default:
    case LOWER_LEFT:
      s_left   = sx;
      s_lower  = sy;
      w_bottom = rint (s_lower * scale) - w_descent;
      w_top    = w_bottom + w_ascent;
      break;

    case MIDDLE_LEFT:
      s_left   = sx;
      s_lower  = sy + ( s_ascent - s_descent ) / 2 ;
      w_bottom = rint ((sy - EDA_DEFAULT_LEADING) * scale) - w_height / 2;
      w_top    = w_bottom + w_ascent;
      break;

    case UPPER_LEFT:
      s_left   = sx;
      s_lower  = sy + s_ascent - s_descent + EDA_DEFAULT_LEADING;
      w_bottom = rint ((sy - EDA_DEFAULT_LEADING) * scale) - w_ascent;
      w_top    = rint (sy * scale);
      break;

    case LOWER_MIDDLE:
      s_left   = sx - width / 2;
      s_lower  = sy;
      w_bottom = rint (s_lower * scale) - w_descent;
      w_top    = w_bottom + w_ascent;
      break;

    case MIDDLE_MIDDLE:
      s_left   = sx - width / 2;
      s_lower  = sy + ( s_ascent - s_descent ) / 2 ;
      w_bottom = rint ((sy - EDA_DEFAULT_LEADING) * scale) - w_height / 2;
      w_top    = w_bottom + w_ascent;
      break;

    case UPPER_MIDDLE:
      s_left   = sx - width / 2;
      s_lower  = sy + s_ascent - s_descent + EDA_DEFAULT_LEADING;
      w_bottom = rint ((sy - EDA_DEFAULT_LEADING) * scale) - w_ascent;
      w_top    = rint (sy * scale);
      break;

    case LOWER_RIGHT:
      s_left   = sx - width - eol_sp;
      s_lower  = sy;
      w_bottom = rint (s_lower * scale) - w_descent;
      w_top    = w_bottom + w_ascent;
      break;

    case MIDDLE_RIGHT:
      s_left   = sx - width - eol_sp;
      s_lower  = sy + ( s_ascent - s_descent ) / 2 ;
      w_bottom = rint ((sy - EDA_DEFAULT_LEADING) * scale) - w_height / 2;
      w_top    = w_bottom + w_ascent;
      break;

    case UPPER_RIGHT:
      s_left   = sx - width - eol_sp;
      s_lower  = sy + s_ascent - s_descent + EDA_DEFAULT_LEADING;
      w_bottom = rint ((sy - EDA_DEFAULT_LEADING) * scale) - w_ascent;
      w_top    = rint (sy * scale);
  }

  w_left    = rint (s_left * scale);
  w_right   = w_left + rint ((width + eol_sp) * scale);

  object->left   = w_left;
  object->right  = w_right;
  object->top    = w_top;
  object->bottom = w_bottom;

  *x_left  = s_left;
  *y_lower = s_lower;
}

/*---------------------- Begin Public Drawing Routines ----------------------*/

/* Only God knows what xorg developers were thinking, obviously the developer
 * were confused */
void EdaX11Render::
geda_x11_draw_arc (int cx, int cy, int radius, int start_angle, int sweep)
{
  XGCValues gcvals;

  int angle1, angle2;
  int length;
  int x_tweek;
  int y_tweek;

  unsigned long bits;
  unsigned int  width, height;

  length = m_arc_length (radius, sweep);
  bits   = SetLineAttributes (&gcvals, length);

  if (bits) {
    XChangeGC (display, gc, bits, &gcvals);

    angle1  = start_angle * 64;
    angle2  = sweep * 64;

    width   = radius * 2;
    height  = radius * 2;

    x_tweek = cx - radius;
    y_tweek = cy - radius;

    XDrawArc (display, drawable, gc, x_tweek, y_tweek, width, height, angle1, angle2);
  }

  return;
}

void EdaX11Render::geda_x11_draw_box (int x, int y, int width, int height)
{
  XGCValues gcvals;
  unsigned  long bits;
  int       length;

  length = min(width, height);

  bits = SetLineAttributes(&gcvals, length);

  if (bits) {
    XChangeGC (display, gc, bits, &gcvals);
    XDrawRectangle (display, drawable, gc, x, y, width, height);
  }

  return;
}

void EdaX11Render::geda_x11_draw_circle (int cx, int cy, int radius)
{
  XGCValues gcvals;

  int x, y;
  int circum;
  int axe;
  unsigned long bits;

  circum = m_circumference(radius);
  bits   = SetLineAttributes(&gcvals, circum);

  if (bits) {

    XChangeGC(display, gc, bits, &gcvals);

    x    = cx - radius;
    y    = cy - radius;
    axe  = radius * 2;
    XDrawArc(display, drawable, gc, x, y, axe, axe, 0, 360*64);

  }

  return;
}

void EdaX11Render::geda_x11_draw_line (int x1, int y1, int x2, int y2)
{
  XGCValues gcvals;
  unsigned  long bits;
  int       length;

  length = o_line_length(object);

  bits = SetLineAttributes(&gcvals, length);

  if (bits) {
    XChangeGC(display, gc, bits, &gcvals);
    XDrawLine(display, drawable, gc, x1, y1, x2, y2);
  }

  return;
}

void EdaX11Render::geda_x11_draw_net (int x1, int y1, int x2, int y2)
{
  XGCValues gcvals;

  if (GEDA_IS_OBJECT(object)) {
    gcvals.line_width = GetLineWidth(object->line_options->line_width);
    XChangeGC(display, gc, GCLineWidth, &gcvals);
    XDrawLine(display, drawable, gc, x1, y1, x2, y2);
  }
  return;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"

void EdaX11Render::geda_x11_draw_path (int nsections, PATH_SECTION *sections)
{
  XGCValues gcvals;
  unsigned long bits;
  int x0, y0;
  int from_x, from_y, to_x, to_y;

  XPoint points[4];

  //int length;
  int i;

  //length = m_line_length(object->line);

  bits = SetLineAttributes(&gcvals, 400);

  if (bits) {

    XChangeGC(display, gc, bits, &gcvals);

    for (i = 0; i < nsections; i++) {

      PATH_SECTION *section = sections + i;

      switch (section->code) {
        case PATH_MOVETO:
          to_x   = section->x3;
          to_y   = section->y3;
          break;

        case PATH_MOVETO_OPEN:
          to_x   = section->x3;
          to_y   = section->y3;
          from_x = -1;
          from_y = -1;
          if (i == 0) {
            x0   = to_x;
            y0   = to_y;
          }
          break;

        case PATH_CURVETO:
          points[0].x = from_x;
          points[0].y = from_y;
          points[1].x = section->x1;
          points[1].y = section->y1;
          points[2].x = section->x2;
          points[2].y = section->y2;
          points[3].x = section->x3;
          points[3].y = section->y3;
          DrawBezierCurve (&points[0]);
          from_x   = section->x3;
          from_y   = section->y3;
          continue;

        case PATH_LINETO:
          to_x   = section->x3;
          to_y   = section->y3;
          break;

        case PATH_END:
          to_x   = x0;
          to_y   = y0;
          break;

        default:
          continue;
      }
      if (!(from_x == -1 && from_y == -1)) {
        XDrawLine(display, drawable, gc, from_x, from_y, to_x, to_y);
      }
      from_x = to_x;
      from_y = to_y;
    }
  }
  return;
}
#pragma GCC diagnostic pop

void EdaX11Render::geda_x11_draw_text (int x, int y)
{
  Text         *o_text;
  const char   *string;

  int x_left;
  int y_lower;
  int length;

  if (GEDA_IS_TEXT(object)) {

    o_text    = object->text;
    string    = o_text->disp_string;
    length    = strlen(string);

    /* set up font */
    if (QueryCurrentFont (NULL, o_text->size)) {

#ifdef HAVE_XFT

      font = XftFontOpenName (display, screen, font_string.c_str());

    }

    if (font) {

      /* Set up the color */
      if (xrcolor.red != color.red || xrcolor.green != color.green || xrcolor.blue != color.blue)
      {
        XftColorFree(display, visual, colormap, &xftcolor);

        xrcolor.red   = color.red;
        xrcolor.green = color.green;
        xrcolor.blue  = color.blue;
        xrcolor.alpha = 255;

        /* Allocate Color */
        XftColorAllocValue(display, visual, colormap, &xrcolor, &xftcolor);
      }

      TextAlignSetBounds (length, x, y, &x_left, &y_lower);

      /* Draw the text */
      XftDrawString8(xftdraw, &xftcolor, font, x_left, y_lower, (XftChar8 *)string, length);

    }

#else

      font = XLoadQueryFont(display, font_string.c_str());
    }

    if (font) {

      XSetFont(display, gc, font->fid);
      TextAlignSetBounds (length, x, y, &x_left, &y_lower);
      XDrawString(display, drawable, gc, x_left, y_lower, string, length);

    }

#endif

    XFlush(display);
  }
  return;
}

void EdaX11Render::
geda_x11_draw_set_color (unsigned short red, unsigned short green, unsigned short blue)
{
  color.red   = red;
  color.green = green;
  color.blue  = blue;
  color.flags = DoRed | DoGreen | DoBlue;

  if (0 == colormap) {
    colormap = DefaultColormap(display, screen);
  }

  XAllocColor(display, colormap, &color);

  XSetForeground(display, gc, color.pixel);

  return;
}

int EdaX11Render::
geda_x11_draw_get_text_bounds (int *left, int *top,  int *right, int *bottom)
{
  Text       *o_text;
  const char *string;

  int length;
  int result;

  int sx;
  int sy;
  int s_left;
  int s_bottom;

  if (GEDA_IS_TEXT(object)) {

    o_text   = object->text;
    string   = o_text->disp_string;
    length   = strlen(string);
    sx       = rint (o_text->x / scale);
    sy       = rint (o_text->y / scale);

    QueryCurrentFont (NULL, o_text->size);

#ifdef HAVE_XFT

    font     = XftFontOpenName (display, screen, font_string.c_str());

#else

    font     = XLoadQueryFont(display, font_string.c_str());

#endif

    if (font) {

      TextAlignSetBounds (length, sx, sy, &s_left, &s_bottom);

     *left     = object->left;
     *right    = object->right;
     *bottom   = object->bottom;
     *top      = object->top;

      result = true;
    }
    else {
      result = false;
    }
  }
  else {
    result = false;
  }
  return result;
}

int EdaX11Render::geda_x11_draw_get_font_name (char *font_name, int size_of_buffer)
{
  int length;

  length = font_family.length();
  length = font_family.copy(font_name, size_of_buffer, 0);
  font_name[length] = '\0';

  return length;
}

void EdaX11Render::geda_x11_draw_set_font_name (const char *font_name)
{
  char* tmp_string;

  if (font_name == NULL ) {
    font_name  = DEFAULT_FONT_NAME;
  }

  font_family  = font_name;

#if HAVE_XFT
  tmp_string   = u_string_sprintf("%s,", font_name);
#else
  tmp_string   = u_string_sprintf("-*-%s", font_name);
#endif

  font_format  = tmp_string;

#if HAVE_XFT
  font_format  = font_format + "%d";
#else
  font_format  = font_format + "-medium-r-normal--%d-0-0-0-p-0-iso10646-1";
#endif

  font_string  = GetFontString(font_size);

  GEDA_FREE(tmp_string);
}

void EdaX11Render::geda_x11_draw_set_font (const char *font_name, int size)
{
  font_size = size;
  geda_x11_draw_set_font_name(font_name);
}

bool EdaX11Render::geda_x11_draw_get_font_list(const char *pattern, GArray *listing)
{
  bool result;
  int  index;

#ifdef HAVE_XFT

  FcFontSet *font_set;

  font_set = XftListFonts (display, screen, XFT_SCALABLE, XftTypeBool, True,
                           NULL,
                           XFT_FAMILY, XFT_STYLE,
                           NULL);

  if (font_set) {

    for (index = 0; index < font_set->nfont; ++index) {

      FcChar8   *family, *style;
      FcPattern *pattern = font_set->fonts[index];

      if (FcPatternGetString(pattern, FC_FAMILY, 0, &family) == FcResultMatch &&
          FcPatternGetString(pattern, FC_STYLE, 0, &style) == FcResultMatch)
      {
        char *name = u_string_sprintf("-%s-%s", family, style);
        g_array_append_val (listing, name);
      }
    }

    FcFontSetDestroy(font_set);

    result = index > 0 ? True : False;
  }
  else {
    result = false;
  }

#else

  char **font_list;
  int  maxnames = 4096;
  int  count;

  font_list = XListFonts (display, pattern, maxnames, &count);

  for (index = 0; index < count; index++) {
    char *name = u_string_strdup(font_list[index]);
    g_array_append_val (listing, name);
  }
  XFreeFontNames(font_list);

  result = count > 0 ? True : False;

#endif

  return result;
}

void EdaX11Render::geda_x11_draw_set_surface(cairo_t *cr, double scale_factor)
{
  if (surface != NULL) {
    cairo_surface_destroy (surface);
    XFreeGC(display, gc);
  }

  surface  = cairo_get_target (cr);
  cairo_surface_reference (surface);

  drawable = cairo_xlib_surface_get_drawable (surface);
  display  = cairo_xlib_surface_get_display (surface);
  screen   = *(int*)cairo_xlib_surface_get_screen (surface);
  gc       = XCreateGC (display, drawable, 0, 0 );
  scale    = scale_factor == 0 ? 1 : scale_factor; /* not allowed to be zero */

  if (0 == visual) {
    visual = cairo_xlib_surface_get_visual (surface);
  }

  if ( 0 == colormap) {
    colormap = DefaultColormap(display, screen);
  }

#ifdef HAVE_XFT

  if (xftdraw != NULL) {
    XftDrawChange (xftdraw, drawable);
  }
  else {
    xftdraw = XftDrawCreate(display, drawable, visual, colormap);
  }

#endif
}

EdaX11Render::~EdaX11Render () {

  /* Release resoures */

#ifdef HAVE_XFT

  XftColorFree(display, visual, colormap, &xftcolor);

  if (xftdraw != NULL) {
    XftDrawDestroy(xftdraw);
  }
  xftdraw = NULL;

#else
  if (font) {
    XFreeFont(display, font);
  }

#endif

  if (surface != NULL) {
    cairo_surface_destroy (surface);
  }
  surface = NULL;

  XFreeGC(display, gc);
}

/*! \brief EdaX11Render Class Constructor
 *  \par Function Description
 *  This is the constructor for the EdaX11Render class. The construction calls
 * initialize() to set variables, then becomeDaemon. If becomeDaemon is
 * not successful the program is terminated. If program successful transforms
 * the a PID file is created and the signal handle is setup.
 *
 */
EdaX11Render::EdaX11Render (const char *font_name) {


  font_size =  -1;
  scale     = 1.0;
  colormap  =   0;
  font      = NULL;
  object    = NULL;
  surface   = NULL;
  visual    = NULL;

#ifdef HAVE_XFT

  xftdraw   = NULL;

#endif

  geda_x11_draw_set_font_name(font_name);

  return;
}
