/* -*- C geda_win32.hpp indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: geda_win32.hpp
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: Nov, 27, 2014
 * Contributing Author: Wiley Edward Hill
 *
*/
/*!
 * \file geda_win32.hpp
 * \brief Header file for X11 Drawing routines in Libgedadraw.
 *
 * \remarks This module is under development
 *
 * \warning config.h MUST be included before this file because offsets
 *          to class members will change if HAVE_XFT is defined!
 */

#define geda_draw_set_surface     geda_win32_draw_set_surface
#define geda_draw_set_color       geda_win32_draw_set_color
#define geda_draw_get_font_name   geda_win32_draw_get_font_name
#define geda_draw_set_font        geda_win32_draw_set_font
#define geda_draw_get_font_list   geda_win32_draw_get_font_list
#define geda_draw_get_text_bounds geda_win32_draw_get_text_bounds

#define geda_draw_arc        geda_win32_draw_arc
#define geda_draw_box        geda_win32_draw_box
#define geda_draw_circle     geda_win32_draw_circle
#define geda_draw_line       geda_win32_draw_line
#define geda_draw_net        geda_win32_draw_net
#define geda_draw_path       geda_win32_draw_path
#define geda_draw_picture    geda_win32_draw_picture
#define geda_draw_text       geda_win32_draw_text

#define geda_draw_rubber_box geda_win32_draw_rubber_box

/* Drawable implementation for Win32 */

#define FONT_SLANT_ROMAN		    0
#define FONT_SLANT_ITALIC		    100
#define FONT_SLANT_OBLIQUE	    110

#define FONT_WEIGHT_THIN		    0
#define FONT_WEIGHT_EXTRALIGHT	40
#define FONT_WEIGHT_ULTRALIGHT	FONT_WEIGHT_EXTRALIGHT
#define FONT_WEIGHT_LIGHT		    50
#define FONT_WEIGHT_DEMILIGHT	  55
#define FONT_WEIGHT_SEMILIGHT	  FONT_WEIGHT_DEMILIGHT
#define FONT_WEIGHT_BOOK		    75
#define FONT_WEIGHT_REGULAR	    80
#define FONT_WEIGHT_NORMAL	    FONT_WEIGHT_REGULAR
#define FONT_WEIGHT_MEDIUM	    100
#define FONT_WEIGHT_DEMIBOLD	  180
#define FONT_WEIGHT_SEMIBOLD	  FONT_WEIGHT_DEMIBOLD
#define FONT_WEIGHT_BOLD		    200
#define FONT_WEIGHT_EXTRABOLD	  205
#define FONT_WEIGHT_ULTRABOLD	  FONT_WEIGHT_EXTRABOLD
#define FONT_WEIGHT_BLACK		    210
#define FONT_WEIGHT_HEAVY		    FONT_WEIGHT_BLACK
#define FONT_WEIGHT_EXTRABLACK	215
#define FONT_WEIGHT_ULTRABLACK	FONT_WEIGHT_EXTRABLACK

typedef enum
{
  EDA_X11_FORMAT_NONE,
  EDA_X11_FORMAT_EXACT_MASK,
  EDA_X11_FORMAT_ARGB_MASK,
  EDA_X11_FORMAT_ARGB
} EdaPicFormat;

typedef unsigned long	Colormap;
typedef unsigned long	Display;
typedef unsigned long	Drawable;
typedef unsigned long	Font;
typedef unsigned long	GC;
typedef unsigned long	PictFormat;
typedef unsigned long	Pixmap;


typedef struct XPoint {
     short x, y;
} XPoint;

typedef struct XColor {
	unsigned long pixel;			/* pixel value */
	unsigned short red, green, blue;	/* rgb values */
	char flags;				/* DoRed, DoGreen, DoBlue */	
	char pad;
} XColor;

typedef struct XImage {
	    int    width;
	    int    height;
      int    bytes_per_line;
      int    byte_order;
	    char  *data;
} XImage;

typedef struct {
	    short   red;
	    short   redMask;
	    short   green;
	    short   greenMask;
	    short   blue;
	    short   blueMask;
	    short   alpha;
	    short   alphaMask;
} RenderDirectFormat;

typedef struct {
	    PictFormat	        id;
	    int			            type;
	    int			            depth;
	    RenderDirectFormat	direct;
	    Colormap		        colormap;
} RenderPictFormat;

typedef struct XGCValues {

    int function;

    /* logical operation */

    unsigned long plane_mask;/* plane mask */

    unsigned long foreground;/* foreground pixel */

    unsigned long background;/* background pixel */

    int line_width;

    /* line width (in pixels) */

    int line_style;

    /* LineSolid, LineOnOffDash, LineDoubleDash */

    int cap_style;

    /* CapNotLast, CapButt, CapRound, CapProjecting */

    int join_style;

    /* JoinMiter, JoinRound, JoinBevel */

    int fill_style;

    /* FillSolid, FillTiled, FillStippled FillOpaqueStippled*/

    int fill_rule;

    /* EvenOddRule, WindingRule */

    int arc_mode;

    /* ArcChord, ArcPieSlice */

    Pixmap tile;

    /* tile pixmap for tiling operations */

    Pixmap stipple;

    /* stipple 1 plane pixmap for stippling */

    int ts_x_origin;

    /* offset for tile or stipple operations */

    int ts_y_origin;

    Font font;

    /* default text font for text operations */

    int subwindow_mode;

    /* ClipByChildren, IncludeInferiors */

    bool graphics_exposures;

    /* boolean, should exposures be generated */

    int clip_x_origin;

    /* origin for clipping */

    int clip_y_origin;

    Pixmap clip_mask;

    /* bitmap clipping; other calls for rects */

    int dash_offset;

    /* patterned/dashed line information */

    char dashes; 
} XGCValues; 

class EdaX11Render
{

private:

  cairo_t         *context;
  cairo_surface_t *surface;
  //Visual          *visual;

  int world_width;
  int world_height;

protected:

  cairo_font_face_t  *font;

  double        scale;

  GHashTable   *font_cache;
  int           font_size;
  int           font_weight;
  int           font_slant;

  std::string   font_family;
  std::string   font_format;
  std::string   font_string;

  inline int GetLineWidth (int line_width) {

    //return max (line_width, MIN_LINE_WIDTH_THRESHOLD) / ((1 / scale) + 0.14) + 1.35;
    return max (line_width, MIN_LINE_WIDTH_THRESHOLD) * scale + 0.15;
  }

  inline std::string GetFontString(int size);

  void         CreateFontHash     (void);
  void         HashSetFont        (void);

  EdaRotation  GetRotation        (int angle);
  EdaPicFormat GetImageFormat     (RenderPictFormat **format, RenderPictFormat **mask);
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

  int           event_x;
  int           event_y;
  int           screen;
  GedaObject   *object;

  XColor        color;
  Colormap      colormap;

  EdaX11Render (const char *font_name, int size);
 ~EdaX11Render ();

  void geda_win32_draw_set_surface     (cairo_t *cr, double scale_factor);
  void geda_win32_draw_set_color       (unsigned short red, unsigned short green, unsigned short blue);

  void geda_win32_draw_set_font        (const char *font_name, int size);
  bool geda_win32_draw_get_font_list   (const char *pattern, GArray *listing);
  void geda_win32_draw_set_font_name   (const char *font_name);
  int  geda_win32_draw_get_font_name   (char *font_name, int size_of_buffer);
  int  geda_win32_draw_get_font_slant  (const char *font_descr);
  int  geda_win32_draw_get_font_weight (const char *font_descr);
  int  geda_win32_draw_get_text_bounds (int *left,  int *top, int *right, int *bottom);

  void geda_win32_draw_arc          (int cx, int cy, int radius, int start_angle, int end_angle);
  void geda_win32_draw_box          (int x, int y, int width, int height);
  void geda_win32_draw_circle       (int x, int y, int radius);
  void geda_win32_draw_line         (int x1, int y1, int x2, int y2);
  void geda_win32_draw_net          (int x1, int y1, int x2, int y2);
  void geda_win32_draw_path         (int num_sections, PATH_SECTION *sections);
  void geda_win32_draw_picture      (int x, int y, int width, int height);
  void geda_win32_draw_text         (int x, int y);

  void geda_win32_draw_rubber_box   (int x, int y, int width, int height);
};
