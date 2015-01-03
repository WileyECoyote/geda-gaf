/* -*- C geda_x11.hpp indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: geda_x11.hpp
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
 * \file geda_x11.hpp
 * \brief Header file for X11 Drawing routines in Libgedadraw.
 *
 * \remarks This module is under development
 *
 */
#define Picture xPicture

#include<X11/Xlib.h>
#include<X11/Xutil.h>
#include<X11/Xft/Xft.h>

#undef Complex
#undef Picture

#define geda_draw_set_surface     geda_x11_draw_set_surface
#define geda_draw_set_color       geda_x11_draw_set_color
#define geda_draw_get_font_name   geda_x11_draw_get_font_name
#define geda_draw_set_font        geda_x11_draw_set_font
#define geda_draw_get_font_list   geda_x11_draw_get_font_list
#define geda_draw_get_text_bounds geda_x11_draw_get_text_bounds

#define geda_draw_arc        geda_x11_draw_arc
#define geda_draw_box        geda_x11_draw_box
#define geda_draw_circle     geda_x11_draw_circle
#define geda_draw_line       geda_x11_draw_line
#define geda_draw_net        geda_x11_draw_net
#define geda_draw_path       geda_x11_draw_path
#define geda_draw_picture    geda_x11_draw_picture
#define geda_draw_text       geda_x11_draw_text

/* Drawable implementation for X11 */

typedef enum
{
  EDA_X11_FORMAT_NONE,
  EDA_X11_FORMAT_EXACT_MASK,
  EDA_X11_FORMAT_ARGB_MASK,
  EDA_X11_FORMAT_ARGB
} EdaX11Format;

class EdaX11Render
{

private:

  cairo_surface_t *surface;
  Visual          *visual;

  int world_width;
  int world_height;

protected:

#ifdef HAVE_XFT
  XftFont      *font;
  XftColor      xftcolor;
  XftDraw      *xftdraw;
  XRenderColor  xrcolor;

  XftFont      *CreateXftFont     (void);

#else
  XFontStruct  *font;
#endif

  double        scale;

  GHashTable   *font_cache;
  int           font_size;
  int           font_weight;
  int           font_slant;

  std::string   font_family;
  std::string   font_format;
  std::string   font_string;

  inline int GetLineWidth (int line_width) {
    return max (line_width, MIN_LINE_WIDTH_THRESHOLD) / 12;
  }

  inline std::string GetFontString(int size);

  void         CreateFontHash     (void);
  void         HashSetFont        (void);

  EdaRotation  GetRotation        (int angle);
  EdaX11Format GetImageFormat     (XRenderPictFormat **format, XRenderPictFormat **mask);
  XImage      *Pixbuf2Ximage      (GdkPixbuf *pixbuf);

  bool         IsScalableFont     (char *name);
  bool         QueryCurrentFont   (const char *font_name, int size);
  void         TextAlignSetBounds (int length, int x, int y, int *x_left, int *y_lower);

  void         DrawBezierCurve    (XPoint *points);
  unsigned int SetLineAttributes  (XGCValues *gcvals, int total);

public:

  GC            gc;
  Display      *display;
  Drawable      drawable;

  int           screen;
  Object       *object;

  XColor        color;
  Colormap      colormap;

  EdaX11Render(const char *font_name, int size);
 ~EdaX11Render();

  void geda_x11_draw_set_surface     (cairo_t *cr, double scale_factor);
  void geda_x11_draw_set_color       (unsigned short red, unsigned short green, unsigned short blue);

  void geda_x11_draw_set_font        (const char *font_name, int size);
  bool geda_x11_draw_get_font_list   (const char *pattern, GArray *listing);
  void geda_x11_draw_set_font_name   (const char *font_name);
  int  geda_x11_draw_get_font_name   (char *font_name, int size_of_buffer);
  int  geda_x11_draw_get_font_slant  (const char *font_descr);
  int  geda_x11_draw_get_font_weight (const char *font_descr);
  int  geda_x11_draw_get_text_bounds (int *left,  int *top, int *right, int *bottom);

  void geda_x11_draw_arc          (int cx, int cy, int radius, int start_angle, int end_angle);
  void geda_x11_draw_box          (int x, int y, int width, int height);
  void geda_x11_draw_circle       (int x, int y, int radius);
  void geda_x11_draw_line         (int x1, int y1, int x2, int y2);
  void geda_x11_draw_net          (int x1, int y1, int x2, int y2);
  void geda_x11_draw_path         (int num_sections, PATH_SECTION *sections);
  void geda_x11_draw_picture      (int x, int y, int width, int height);
  void geda_x11_draw_text         (int x, int y);
};
