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
 * Date: Nov, 27, 2014
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

#include <gdk/gdk.h>

#define Picture xpicture

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xft/Xft.h>
#include <cairo-xlib.h>
#undef Complex
#undef Picture

#include <libgeda/libgeda.h>

#include <geda_draw.h>

/* Used run-time by the text renderer funtion to determine if the
 * current font needs to be updated */
bool
EdaX11Render::QueryCurrentFont (const char *font_name, int size)
{
  char *tmp_string;
  int   new_size;
  bool  update;

  tmp_string = NULL;
  update     = false;

  if (font_name == NULL) {
    font_name = font_family.c_str();
  }
  else {
    update = true;
  }

#if HAVE_XFT
  new_size    = (size / scale) * FONT_SIZE_FACTOR;
#else
  new_size    = size < 8 ? 8 : size;
#endif

  if (font_size != new_size || update) {

    font_size = new_size;

#if HAVE_XFT
    tmp_string = u_string_sprintf("%s-%d", font_name, new_size);
#else
    tmp_string = u_string_sprintf("-%s charter-medium-i-normal--0-0-0-0-p-0-iso8859-1", font_name);
#endif

  }
  else {
    tmp_string = NULL;
  }

  if (tmp_string) {

    if (!font_string.compare(tmp_string)) {
      update  = false;
    }
    else {
      font_string = tmp_string;
      update = true;
    }

    GEDA_FREE(tmp_string);
  }

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

  int length;
/*
  int ytext, wtext;

*/
  if (GEDA_IS_TEXT(object)) {

    o_text    = object->text;
    string    = o_text->disp_string;
    length    = strlen(string);

#ifdef HAVE_XFT

    /* set up font */
    if (geda_x11_draw_query_free (NULL, o_text->size)) {
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

      /* Draw the text */
      XftDrawString8(xftdraw, &xftcolor, font, x, y , (XftChar8 *)string, length);

    }
#else

    /* set up font */
    int new_size     = (o_text->size / scale) * FONT_SIZE_FACTOR;
    char *tmp_string = u_string_sprintf(font_string.c_str(), new_size);
    //fprintf(stderr,"using: %s\n", tmp_string);
    //if (!font) {
      font = XLoadQueryFont(display, tmp_string);
    //}

    if (font) {

      XSetFont(display, gc, font->fid);
      XDrawString(display, drawable, gc, x, y, string, length);

    }
    GEDA_FREE(tmp_string);
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

#if HAVE_XFT
    font_name  = "morpheus";
#else
    font_name  = DEFAULT_FONT_NAME;
#endif

  }

  font_family = font_name;

#if HAVE_XFT
  font_size   = font_size < 8 ? 8 : font_size;
  tmp_string  = u_string_sprintf("%s-%d", font_name, font_size);
#else
  tmp_string  = u_string_sprintf("-*-%s", font_name);
#endif

  font_string = tmp_string;

#ifndef HAVE_XFT
  font_string  = font_string + "-medium-r-normal--%d-0-0-0-p-0-iso10646-1";
#endif

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
  int  maxnames = 256;
  int  count;
  int  index;
  char **font_list;

  font_list = XListFonts (display, pattern, maxnames, &count);

  for (index = 0; index < count; index++) {
    char *name = u_string_strdup(font_list[index]);
    g_array_append_val (listing, name);
  }
  XFreeFontNames(font_list);

  result = true;

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
  scale    = scale_factor;

  if ( 0 == visual) {
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
