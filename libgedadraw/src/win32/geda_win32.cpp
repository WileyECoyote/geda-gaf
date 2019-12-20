/* -*- C geda_win32.c indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-
 *
 * File: geda_win32.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014-2016 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: Dec, 25, 2014
 * Contributing Author: Wiley Edward Hill
 *
*/
/*!
 * \file geda_win32.c
 * \brief Libgedadraw Drawing routines for X11.
 *
 * \remarks This module is under development
 *
 * -lX11 -lXft `pkg-config --cflags freetype2
 */

/* Not optional! See include/geda_win32.hpp (included by geda_draw.h),
 * except for distcheck, using ../../config.h is quicker work around
 * for issue with automake using the in-src configuration for the
 * distcheck rather than having to reconfigure, see libgedadraw/BUGS
 */
#include "../../../config.h"

#include <string>
#include <cmath>

#include <gdk/gdk.h>

#define Picture xpicture


#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda_colors.h>
#include <geda_draw.h>
#include <cairo-win32.h>
#include <windows.h>

/*! \todo Finish function documentation!!!
 *  \brief  Get Rotation
 *  \par Function Description
 *
 */
EdaRotation EdaX11Render::
GetRotation(int angle)
{
  int rotation;

  switch (angle) {
    case 0:    rotation = EDA_ROTATE_NONE;             /* =   0 */ break;
    case 90:   rotation = EDA_ROTATE_COUNTERCLOCKWISE; /* =  90 */ break;
    case 180:  rotation = EDA_ROTATE_UPSIDEDOWN;       /* = 180 */ break;
    case 270:  rotation = EDA_ROTATE_CLOCKWISE;        /* = 270 */ break;
    default:   rotation = EDA_ROTATE_NONE;             /* =   0 */ break;
  }
  return (EdaRotation)rotation;
}

/*! \todo Finish function documentation!!!
 *  \brief Get Image Format
 *  \par Function Description
 * Code for accelerated alpha compositing using the RENDER extension.
 * It's a bit long because there are lots of possibilities for
 * what's the fastest depending on the available picture formats,
 * whether we can used shared pixmaps, etc.
 */
EdaPicFormat EdaX11Render::
GetImageFormat (RenderPictFormat **format, RenderPictFormat **mask)
{
  /* Look for a 32-bit xRGB and Axxx formats that exactly match the
   * in memory data format. We can use them as pixmap and mask
   * to deal with non-premultiplied data.
   */


  return EDA_X11_FORMAT_NONE;
}

/*! \todo Finish function documentation!!!
 *  \brief Pixbuf to Ximage
 *  \par Function Description
 *
 */
XImage *EdaX11Render::
Pixbuf2Ximage (GdkPixbuf *pixbuf)
{
  RenderPictFormat *dest_format;
  RenderPictFormat *mask_format;

  EdaPicFormat format_type = GetImageFormat (&dest_format, &mask_format);

  XImage *ximage;

  int depth        = 32; //cairo_xlib_surface_get_depth (surface); /* 32 works fine with depth = 24 */
  int format       = 1; //ZPixmap;
  int width        = gdk_pixbuf_get_width  (pixbuf);
  int height       = gdk_pixbuf_get_height (pixbuf);
  int stride       = gdk_pixbuf_get_rowstride (pixbuf);
  int chan         = gdk_pixbuf_get_n_channels (pixbuf);
  int has_alpha    = gdk_pixbuf_get_has_alpha (pixbuf);

  int bitmap_pad;  /* 32 for 24 and 32 bpp, 16, for 15&16 */

  switch (depth) {
    case 32:
    case 24: bitmap_pad = 32; break;
    case 16:
    case 15: bitmap_pad = 16; break;
    default: bitmap_pad = 0;  break;
  }

  ximage = NULL; //XCreateImage (display, visual, depth, format, 0, 0, width, height, bitmap_pad, 0);

  if (ximage) {

    ximage->data = (char *) malloc(height * ximage->bytes_per_line);

    if (ximage->data) {

      unsigned char *src_buf        = gdk_pixbuf_get_pixels (pixbuf);
      unsigned char *dest_buf       = (unsigned char*)ximage->data;

      unsigned int   src_rowstride;
      unsigned int   dest_rowstride;
      int y;

      if (has_alpha && format_type == EDA_X11_FORMAT_EXACT_MASK) {
        format_type = EDA_X11_FORMAT_ARGB_MASK;
      }

      switch (format_type) {

        case EDA_X11_FORMAT_EXACT_MASK:

          for (y = 0; y < height; y++) {

            unsigned char *i = src_buf;
            int x;

            for (x = 0; x < width; x++) {

              unsigned long rgba = 0;

              switch (chan) {
                case 1:
                  rgba = ((0xFF << 24) | (*i << 16) | (*i << 8) | *i);
                  i++;
                  break;
                case 3:
                  rgba = ((0xFF << 24) | (i[0] << 16) | (i[1] << 8) | i[2]);
                  i += 3;
                  break;
                case 4:
                  rgba = ((i[3] << 24) | (i[2] << 16) | (i[1] << 8) | i[0]);
                  i += 4;
                  break;
                default:
                  fprintf (stderr, "%s: channel not supported <%d>\n", __func__, chan);
                  break;
              }
              //XPutPixel (ximage, x, y, rgba);
            }
            src_buf += stride;
          }
          break;

        case EDA_X11_FORMAT_ARGB_MASK:

          src_rowstride  =  dest_rowstride = stride;

          for (y = 0; y < height; y++) {

            unsigned char *row = src_buf + y * src_rowstride;

            if (((unsigned long)row & 3) != 0) {

              unsigned char *p = row;
              unsigned long *q = (unsigned long *)(dest_buf + y * dest_rowstride);
              unsigned char *end = p + 4 * width;

              while (p < end) {
                *q = (p[3] << 24) | (p[0] << 16) | (p[1] << 8) | p[2];
                p += 4;
                q++;
              }
            }
            else {

              unsigned long *p = (unsigned long *)row;
              unsigned long *q = (unsigned long *)(dest_buf + y * dest_rowstride);
              unsigned long *end = p + width;

#if G_BYTE_ORDER == G_LITTLE_ENDIAN

              if (ximage->byte_order == GDK_LSB_FIRST) { /* ABGR => ARGB */

                while (p < end) {
                  *q = ( (*p & 0xff00ff00) | ((*p & 0x000000ff) << 16) | ((*p & 0x00ff0000) >> 16));
                  q++;
                  p++;
                }
              }
              else { /* ABGR => BGRA */

                while (p < end) {
                  *q = (((*p & 0xff000000) >> 24) | ((*p & 0x00ffffff) << 8));
                  q++;
                  p++;
                }
              }
#else /* G_BYTE_ORDER == G_BIG_ENDIAN */
              if (ximage->byte_order == GDK_LSB_FIRST) { /* RGBA => BGRA */

                while (p < end) {
                  *q = ( (*p & 0x00ff00ff) | ((*p & 0x0000ff00) << 16) |  ((*p & 0xff000000) >> 16));
                  q++;
                  p++;
                }
              }
              else { /* RGBA => ARGB */

                while (p < end) {
                  *q = (((*p & 0xffffff00) >> 8) | ((*p & 0x000000ff) << 24));
                  q++;
                  p++;
                }
              }
#endif /* G_BYTE_ORDER*/
            }
          }
          break;

        case EDA_X11_FORMAT_ARGB:

          src_rowstride  =  dest_rowstride = stride;

          for (y = 0; y < height; y++) {
            unsigned char *p = (src_buf  + y * src_rowstride);
            unsigned char *q = (dest_buf + y * dest_rowstride);
            unsigned char *end = p + 4 * width;
            unsigned int t1,t2,t3;

#define MULT(d,c,a,t) G_STMT_START { t = c * a; d = ((t >> 8) + t) >> 8; } G_STMT_END

            if (ximage->byte_order == GDK_LSB_FIRST) {

              while (p < end) {
                MULT(q[0], p[2], p[3], t1);
                MULT(q[1], p[1], p[3], t2);
                MULT(q[2], p[0], p[3], t3);
                q[3] = p[3];
                p += 4;
                q += 4;
              }
            }
            else {

              while (p < end) {

                q[0] = p[3];
                MULT(q[1], p[0], p[3], t1);
                MULT(q[2], p[1], p[3], t2);
                MULT(q[3], p[2], p[3], t3);
                p += 4;
                q += 4;
              }
            }
#undef MULT
          }
          break;

        case EDA_X11_FORMAT_NONE:
        default:
          fprintf (stderr, "%s: unexpected format type <%d>\n", __func__, format_type);
          break;
      }
    }
    else {
      fprintf (stderr, "%s: out of memory (%d x %d)\n", __func__, width, height);
      ximage = NULL;
    }
  }
  else {
    fprintf (stderr, "%s: XCreateImage failed\n", __func__);
    ximage = NULL;
  }
  return ximage;
}

/*
#if GCC_DIAGNOSTIC_AWARE
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-fpermissive"
#endif
*/

/*! \todo Finish function documentation!!!
 *  \brief FontHashDestroyer
 *  \par Function Description
 *
 */
void
FontHashDestroyer (void *key, void *data, void *display)
{
  g_free(key);
/*
#ifndef HAVE_XFT

  XFontStruct *font = (XFontStruct*)data;

  if (font) {
    XFreeFont((Display*)display, font);
  }

#endif
*/
}
/*
#if GCC_DIAGNOSTIC_AWARE
#pragma GCC diagnostic pop
#endif
*/

#ifdef HAVE_XFT

XftFont *EdaX11Render::
CreateXftFont(void)
{
   XftFont   *xftfont;
   FcPattern *pattern;
   FcPattern *match;

    //double sina, cosa;
    int fs;

    //const char *family = "ubuntu";
    const char *family  = font_family.c_str();

    fs = font_size > 0 ? font_size : 10;

//fprintf (stderr, "Matching = %s with weight %d and size %d\n", family, font_weight, fs);

    pattern = FcPatternCreate();
    FcPatternAddString  (pattern, FC_FAMILY, (FcChar8*)family);
    FcPatternAddInteger (pattern, FC_PIXEL_SIZE, fs);
    FcPatternAddInteger (pattern, FC_WEIGHT, font_weight);
    FcPatternAddBool(pattern, FC_MINSPACE, 1);

    if (font_slant != EDA_SLANT_NONE) {
      FcPatternAddInteger (pattern, FC_SLANT, font_slant);
    }

    /*
    if(angle) {
        FcMatrix mx;
        Draw::SinCos(angle, sina, cosa);
        mx.xx = cosa;
        mx.xy = -sina;
        mx.yx = sina;
        mx.yy = cosa;
        FcPatternAddMatrix(pattern, FC_MATRIX, &mx);
    }*/

    FcResult result;

    match = XftFontMatch(display, screen, pattern, &result);

    xftfont = XftFontOpenPattern(display, match);
    FcPatternDestroy(pattern);
    return xftfont;
}
#endif

/*! \todo Finish function documentation!!!
 *  \brief CreateFontHash
 *  \par Function Description
 *
 */
void EdaX11Render::
CreateFontHash (void)
{
  font_cache = g_hash_table_new ( g_str_hash, g_str_equal);
}

/*! \todo Finish function documentation!!!
 *  \brief HashSetFont
 *  \par Function Description
 *
 */
void EdaX11Render::
HashSetFont (void)
{

#ifdef HAVE_XFT

  font = (XftFont*)g_hash_table_lookup (font_cache, font_string.c_str());

#else

  font = (cairo_font_face_t*)g_hash_table_lookup (font_cache, font_string.c_str());

#endif

  if (!font) {

    char *tmp_string = geda_utility_string_strdup(font_string.c_str());

#ifdef HAVE_XFT

    //font = XftFontOpenName (display, screen, font_string.c_str());
    font = CreateXftFont();

#else

    LOGFONTW logfontw;

    font = cairo_win32_font_face_create_for_logfontw(&logfontw);

#endif

    if (font) {
      g_hash_table_insert (font_cache, tmp_string, font);
    }
    else {

#ifdef DEBUG
      fprintf(stderr, "%s did not get a font for %s\n", __func__, font_string.c_str());
#endif

      GEDA_FREE(tmp_string);
    }
  }
}

inline std::string EdaX11Render::
GetFontString(int size) {

  char *tmp_string = geda_sprintf(font_format.c_str(), size);

  std::string str = tmp_string;

  GEDA_FREE(tmp_string);

  return str.data();
}

/*! \todo Finish function documentation!!!
 *  \brief QueryCurrentFont
 *  \par Function Description
 *  Used run-time by the text renderer funtion to determine if the
 *  current font needs to be updated
 */
bool EdaX11Render::
QueryCurrentFont (const char *font_name, int size)
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

  new_size = rint(size * scale * FONT_SIZE_FACTOR);

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

  return update;
}

/*! \todo Finish function documentation!!!
 *  \brief IsScalableFont
 *  \par Function Description
 * This routine returns True if the font string is a properly formed
 * XLFD styled font name with a pixel size, point size, and average
 * width (fields 7,8, and 12) are "0".
 */
bool EdaX11Render::
IsScalableFont(char *name)
{
  bool anwser;

  if ((name == NULL) || (name[0] != '-')) {
    anwser = false;
  }
  else {

    int i, field;

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
int EdaX11Render::
XSetColorRed(void)
{
  int status;
  XColor red;
  Colormap colormap = DefaultColormap(display, screen);
  status = 0; // XAllocNamedColor(display, colormap, "red", &red, &red);
  if (status)
    XSetForeground(display, gc, red.pixel);
  return status;
}
#endif

typedef enum
{
  CapNotLast,
  CapButt,
  CapRound,
  CapProjecting
} RenderCapStyle;

typedef enum
{
  LineSolid,
  LineOnOffDash,
  LineDoubleDash
} RenderLineStyle;

/*! \todo Finish function documentation!!!
 *  \brief SetLineAttributes
 *  \par Function Description
 * Called from all drawing routines to set line properties,
 * i.e. cap-style, style and width. The total (length)
 * parameter is used to adjust the dash-length and spaces
 * for dash, center and phantom styles so tht short lines
 * look better when the specified dash-length exceeds 20%
 * of the segment length. Could be improved further but is
 * better than what we do with Cairo renderer.
 */
unsigned int EdaX11Render::
SetLineAttributes(XGCValues *gcvals, int total)
{
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

      char dash_list[6];
      int  dash_offset;
      int  num_dash;
      int  length;
      int  space;
      int  length_factor;
      int  space_factor;

      dash_offset = 0;
      length      = object->line_options->line_length;
      space       = object->line_options->line_space;

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
          //XSetDashes(display, gc, dash_offset, dash_list, num_dash);
          gcvals->cap_style = CapRound;
          gcvals->line_style = LineOnOffDash;
          break;

        case TYPE_DASHED:
          dash_list[0]      = length_factor;
          dash_list[1]      = space_factor;
          num_dash          = 2;
          //XSetDashes(display, gc, dash_offset, dash_list, num_dash);
          gcvals->line_style = LineOnOffDash;
          break;

        case TYPE_CENTER:
          dash_list[0]      = length_factor * 9;
          dash_list[1]      = space_factor  / 2;
          dash_list[2]      = length_factor / 3;
          dash_list[3]      = space_factor  / 2;
          num_dash          = 4;
          //XSetDashes(display, gc, dash_offset, dash_list, num_dash);
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
          //XSetDashes(display, gc, dash_offset, dash_list, num_dash);
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

  return 1; //success ? GCCapStyle | GCLineStyle | GCLineWidth : 0;
}

/*! \todo Finish function documentation!!!
 *  \brief DrawBezierCurve
 *  \par Function Description
 *
 */
void EdaX11Render::
DrawBezierCurve (XPoint *points)
{
  double A, B, C, D, E, F, G, H;  /* The Coefficients */
  double time;
  double step;

  double from_x, from_y;

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
    
  cairo_move_to (context, from_x, from_y);

  /* Step from 0 to 1 in BEZIER_STEP increments and
   * calculate X,Y and draw a line from previous point */
  for (time = step; time <= 1; time = time + step) {

    double to_x, to_y;

    to_x = ((( A * time ) + B ) * time + C ) * time + D;
    to_y = ((( E * time ) + F ) * time + G ) * time + H;

    //XDrawLine(display, drawable, gc, from_x, from_y, to_x, to_y);
    cairo_line_to (context, to_x, to_y);

#if DEBUG
    XSetColorRed();
    XDrawPoint(display, drawable, gc, from_x, from_y);
#endif

    from_x = to_x;
    from_y = to_y;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief TextAlignSetBounds
 *  \par Function Description
 *
 */
void EdaX11Render::
TextAlignSetBounds (int length, int sx, int sy, int *x_left, int *y_lower)
{
  GedaText   *o_text = object->text;
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
/*
  static int five = 5;
  if (five) {
    fprintf(stderr, "%s, max_advance_width %d\t", font_string.c_str(), font->max_advance_width);
    fprintf(stderr, "extents.width %d, length %d: %s\n", width, length, string);
    five--;
  }
  */
#else

  s_ascent  = 0; //font->max_bounds.ascent;
  s_descent = 0; //font->max_bounds.descent;
  width     = 0; //XTextWidth (font, string, length);

  /* additional end-of-line spacing */
  //if (!XGetFontProperty(font, XA_END_SPACE, &eol_sp)) {
    eol_sp = EDA_DEFAULT_EOL_SP;
  //}

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

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_arc
 *  \par Function Description
 *  Only God knows what xorg developers were thinking, obviously the developer
 *  were confused
 */
void EdaX11Render::
geda_win32_draw_arc (int cx, int cy, int radius, int start_angle, int sweep)
{
  unsigned long bits;
  int           length;
  XGCValues     gcvals;

  GedaArc *arc;

  arc = (GedaArc*)geda_arc_object_new (3, 0, 0, radius, 0, sweep);

  length = geda_math_arc_length (arc);
  bits   = SetLineAttributes (&gcvals, length);

  if (bits) {

    unsigned int width, height;
    int angle1, angle2;
    int x_tweek;
    int y_tweek;

    //XChangeGC (display, gc, bits, &gcvals);

    angle1  = start_angle * 64;
    angle2  = sweep * 64;

    width   = radius * 2;
    height  = radius * 2;

    x_tweek = cx - radius;
    y_tweek = cy - radius;

    //XDrawArc (display, drawable, gc, x_tweek, y_tweek, width, height, angle1, angle2);
    cairo_arc (context, x_tweek, y_tweek, radius, angle1, angle2);
  }

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_box
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_box (int x, int y, int width, int height)
{
  XGCValues gcvals;
  unsigned  long bits;
  int       length;

  length = min(width, height);
  bits   = SetLineAttributes(&gcvals, length);

  if (bits) {
    //XChangeGC (display, gc, bits, &gcvals);
    //XDrawRectangle (display, drawable, gc, x, y, width, height);
    cairo_rectangle (context, x, y, width, height);
  }

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_circle
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_circle (int cx, int cy, int radius)
{
  unsigned  long bits;
  XGCValues gcvals;
  int       circum;

  circum = geda_math_circle_circumference(radius);
  bits   = SetLineAttributes(&gcvals, circum);

  if (bits) {

    int x, y;
    //int axe;

    //XChangeGC(display, gc, bits, &gcvals);

    x    = cx - radius;
    y    = cy - radius;
    //axe  = radius * 2;
    //XDrawArc(display, drawable, gc, x, y, axe, axe, 0, 360*64);
    cairo_arc (context, x, y, radius, 0, 2 * M_PI);
  }

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_line
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_line (int x1, int y1, int x2, int y2)
{
  XGCValues gcvals;
  unsigned  long bits;
  int       length;

  length = geda_line_object_length(object);

  bits = SetLineAttributes(&gcvals, length);

  if (bits) {
    //XChangeGC(display, gc, bits, &gcvals);
    //XDrawLine(display, drawable, gc, x1, y1, x2, y2);
    cairo_move_to (context, x1, x1);
    cairo_line_to (context, x2, x2);
  }

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_net
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_net (int x1, int y1, int x2, int y2)
{
  XGCValues gcvals;

  if (GEDA_IS_OBJECT(object)) {
    gcvals.line_style = LineSolid;
    gcvals.line_width = GetLineWidth(object->line_options->line_width);
    //XChangeGC(display, gc, GCLineWidth | GCLineStyle, &gcvals);
    //XDrawLine(display, drawable, gc, x1, y1, x2, y2);
    cairo_move_to (context, x1, x1);
    cairo_line_to (context, x2, x2);
  }
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_path
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_path (int nsections, PATH_SECTION *sections)
{
  unsigned long bits;
  XGCValues     gcvals;

  bits = SetLineAttributes(&gcvals, 400);

  if (bits) {

    int x0, y0;
    int from_x, from_y, to_x, to_y;
    int i;

    x0 = y0 = 0;
    from_x = from_y = -1;

    XPoint points[4];

    //XChangeGC(display, gc, bits, &gcvals);

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
        //XDrawLine(display, drawable, gc, from_x, from_y, to_x, to_y);
        cairo_move_to (context, from_x, from_y);
        cairo_line_to (context, to_x, to_y);
      }
      from_x = to_x;
      from_y = to_y;
    }
  }
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_picture
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_picture (int x, int y, int width, int height)
{
  if (width < 0)  width  = abs(width);
  if (height < 0) height = abs(height);

  if (GEDA_IS_PICTURE(object)) {

    GdkPixbuf   *pixbuf;
    GedaPicture *o_pic;

    bool  mirror;
    int   angle;

    o_pic  = object->picture;

    if (o_pic && GDK_IS_PIXBUF(o_pic->pixbuf)) {
      pixbuf = o_pic->pixbuf;
      angle  = o_pic->angle;
      mirror = o_pic->mirrored;
    }
    else {
      pixbuf = geda_picture_object_get_fallback_pixbuf ();
      angle  = 0;
      mirror = FALSE;
    }

    if (!pixbuf) {
      GedaObject *save_obj = object;
      object = geda_box_object_new(JUNCTION_COLOR, 0, 0, 0, 0);
      geda_win32_draw_box (x, y, width, height);
      g_object_unref(object);
      object = save_obj;
    }
    else {

      GdkPixbuf *pixbuf1; /* Scaled version of  o_pic->pixbuf     */
      GdkPixbuf *pixbuf2; /* tmp used when rotating and mirroring */
      GdkPixbuf *pixbuf3; /* what need to be drawn on the screen  */

      /* The object->picture->pixel is a pointer to the as read-in pixel
       * buffer and needs to be rescaled to the instance insertion size */
      if (o_pic && ((o_pic->angle == 90) || (o_pic->angle == 270))) {
        pixbuf1 = gdk_pixbuf_scale_simple (pixbuf, height, width, GDK_INTERP_BILINEAR);
      }
      else {
        pixbuf1 = gdk_pixbuf_scale_simple (pixbuf, width, height, GDK_INTERP_BILINEAR);
      }

      /* Adjust for rotation and mirroring */

      if (!angle && !mirror) {                            /* No adjustment required */
        pixbuf3 = pixbuf1; g_object_ref (pixbuf1);
      }
      else if (angle && !mirror) {                        /* Rotation required  */
        pixbuf3 = gdk_pixbuf_rotate_simple (pixbuf1, GetRotation(angle));
      }
      else if (!angle && mirror) {                        /* Mirroring required */
        pixbuf3 = gdk_pixbuf_flip (pixbuf1, TRUE);
      }
      else /* (mirror && angle) note: do flip 1st */ {    /* Mirror and Rotate */
        pixbuf2 = gdk_pixbuf_flip (pixbuf1, TRUE);
        pixbuf3 = gdk_pixbuf_rotate_simple (pixbuf2, GetRotation(angle));
        g_object_unref (pixbuf2);
      }

      XImage *ximage = Pixbuf2Ximage (pixbuf3);

      //XPutImage(display, drawable, gc, ximage, 0, 0, x, y, ximage->width, ximage->height);

      g_object_unref (pixbuf3);
      g_object_unref (pixbuf1);
    }
  }
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_text
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_text (int x, int y)
{
  if (GEDA_IS_TEXT(object)) {

    GedaText   *o_text;
    const char *string;

    int x_left;
    int y_lower;
    int length;

    o_text    = object->text;
    string    = o_text->disp_string;
    length    = strlen(string);

    /* set up font */
    if (QueryCurrentFont (NULL, o_text->size)) {
        HashSetFont();
    }

#ifdef HAVE_XFT
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

    if (font) {
      //XSetFont(display, gc, font->fid);
      TextAlignSetBounds (length, x, y, &x_left, &y_lower);
      //XDrawString(display, drawable, gc, x_left, y_lower, string, length);
      cairo_text_path (context, string);
    }

#endif

    //XFlush(display);
  }
  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_set_color
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_set_color (unsigned short red, unsigned short green, unsigned short blue)
{
  color.red   = red;
  color.green = green;
  color.blue  = blue;
  color.flags = 4; //DoRed | DoGreen | DoBlue;

  if (0 == colormap) {
    colormap = 0; //DefaultColormap(display, screen);
  }

  //XAllocColor(display, colormap, &color);

  //XSetForeground(display, gc, color.pixel);

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_get_font_slant
 *  \par Function Description
 *
 */
int EdaX11Render::
geda_win32_draw_get_font_slant (const char *font_descr)
{
  int slant;

  if (geda_stristr (font_descr, "ital") >= 0) {          /* Italic */
    slant = FONT_SLANT_ITALIC;
  }
  else if (geda_stristr (font_descr, "obli") >= 0) {     /* Oblique */
    slant = FONT_SLANT_OBLIQUE;
  }
  else if (geda_stristr (font_descr, "roma") >= 0) {     /* Roman */
    slant = FONT_SLANT_ROMAN;
  }
  else {
    slant = EDA_SLANT_NONE;
  }

  return slant;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_get_font_weight
 *  \par Function Description
 *
 */
int EdaX11Render::
geda_win32_draw_get_font_weight (const char *font_descr)
{
  int weight;

  // integer: Light, medium, demibold, bold or black, etc
  if (geda_stristr (font_descr, "regu") >= 0) {        /* Regular */
    weight = FONT_WEIGHT_REGULAR;
  }
  else if (geda_stristr (font_descr, "medi") >= 0) {   /* Medium */
    weight = FONT_WEIGHT_MEDIUM;
  }
  else if (geda_stristr (font_descr, "old") >= 0) {    /* Bold, which one? */

    if (geda_stristr (font_descr, "demi") >= 0) {      /* bad choice of characters */
      weight = FONT_WEIGHT_DEMIBOLD;
    }
    else if (geda_stristr (font_descr, "semi") >= 0) { /* Another wizard */
      /* FONT_WEIGHT_DEMIBOLD == FONT_WEIGHT_SEMIBOLD */
      weight = FONT_WEIGHT_DEMIBOLD;
    }
    else {
      weight = FONT_WEIGHT_BOLD;
    }
  }
  else if (geda_stristr (font_descr, "ight") >= 0) {    /* light */

    if (geda_stristr (font_descr, "extr") >= 0) {       /* Extra Light */
      weight = FONT_WEIGHT_EXTRALIGHT;
    }
    else if (geda_stristr (font_descr, "ultr") >= 0) {  /* Ultra Light */
      weight = FONT_WEIGHT_ULTRALIGHT;
    }
    else {
      weight = FONT_WEIGHT_LIGHT;
    }
  }
  else if (geda_stristr (font_descr, "boo") >= 0) {     /* Book */
    weight = FONT_WEIGHT_BOOK;
  }
  else if (geda_stristr (font_descr, "heav") >= 0) {    /* Heavy */
    /* FONT_WEIGHT_HEAVY == FONT_WEIGHT_BLACK */
    weight = FONT_WEIGHT_BLACK;
  }
  else if (geda_stristr (font_descr, "blac") >= 0) {    /* Black, which one?  */

    if (geda_stristr (font_descr, "extr") >= 0) {       /* Extra Black */
      weight = FONT_WEIGHT_EXTRABLACK;
    }
    else if (geda_stristr (font_descr, "ultr") >= 0) {  /* Ultra Black */
    /* FONT_WEIGHT_ULTRABLACK == FONT_WEIGHT_EXTRABLACK */
      weight = FONT_WEIGHT_EXTRABLACK;
    }
    else {
      weight = FONT_WEIGHT_BLACK;
    }

  }
  else {
    weight = FONT_WEIGHT_NORMAL;
  }

  return weight;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_get_text_bounds
 *  \par Function Description
 *
 */
int EdaX11Render::
geda_win32_draw_get_text_bounds (int *left, int *top,  int *right, int *bottom)
{
  int result;

  if (GEDA_IS_TEXT(object)) {

    GedaText   *o_text;
    const char *string;

    int length;
    int sx;
    int sy;
    int s_left;
    int s_bottom;

    o_text   = object->text;
    string   = o_text->disp_string;
    length   = strlen(string);
    sx       = rint (o_text->x / scale);
    sy       = rint (o_text->y / scale);

    if (QueryCurrentFont (NULL, o_text->size)) {

      if (display && (screen > 0)) {
        HashSetFont();
      }
      else {

#ifdef HAVE_XFT

      font   = XftFontOpenName (display, screen, font_string.c_str());

#else

     cairo_font_face_t *font_face;
     cairo_matrix_t *font_matrix;
     cairo_matrix_t *ctm;
     cairo_font_options_t *options;

     font = (cairo_font_face_t*) cairo_scaled_font_create (font_face, font_matrix, ctm, options);

#endif

      }
    }

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

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_get_font_name
 *  \par Function Description
 *
 */
int EdaX11Render::
geda_win32_draw_get_font_name (char *font_name, int size_of_buffer)
{
  int length;

  //length = font_family.length();
  length = font_family.copy(font_name, size_of_buffer, 0);
  font_name[length] = '\0';

  return length;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_set_font_name
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_set_font_name (const char *font_name)
{
  char *tmp_string;

  if (font_name == NULL ) {
    font_name  = DEFAULT_FONT_NAME;
  }

#if HAVE_XFT

  int   index;
  char  strBuffer[64];
  const char *str;

  memset(&strBuffer[0], 0, sizeof(strBuffer));

  str   = NULL;
  index = 0;

  /* Replace first dash with NULL */
  while (font_name[index]) {

    if (font_name[index] == ASCII_MINUS) {
      strBuffer[index] = '\0';
      index++;
      str = &font_name[index]; /* set a pointer to char after dash */
      break;
    }

    strBuffer[index] = font_name[index];
    index++;
  }

  font_family = &strBuffer[0];

  if (str) {
    font_weight = geda_win32_draw_get_font_weight(str);
    font_slant  = geda_win32_draw_get_font_slant(str);
  }
  else {

    font_weight = FONT_WEIGHT_NORMAL;
    font_slant  = EDA_SLANT_NONE;
  }

  tmp_string    = geda_sprintf("%s,", font_name);

#else

  font_family   = font_name;
  tmp_string    = geda_sprintf("-*-%s", font_name);

#endif

  font_format   = tmp_string;

#if HAVE_XFT

  font_format   = font_format + "%d";

#else

  font_format   = font_format + "-medium-r-normal--%d-0-0-0-p-0-iso10646-1";

#endif

  font_string   = GetFontString(font_size);

  GEDA_FREE(tmp_string);
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_set_font
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_set_font (const char *font_name, int size)
{
  font_size = size;
  geda_win32_draw_set_font_name(font_name);
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_get_font_list
 *  \par Function Description
 *
 */
bool EdaX11Render::
geda_win32_draw_get_font_list(const char *pattern, GArray *listing)
{
  bool result;

#ifdef HAVE_XFT

  FcFontSet *font_set = NULL;
/*
  font_set = XftListFonts (display, screen, XFT_SCALABLE, XftTypeBool, True,
                           NULL,
                           XFT_FAMILY, XFT_STYLE,
                           NULL);
*/
  if (font_set) {

    int  index;

    for (index = 0; index < font_set->nfont; ++index) {

      FcChar8   *family, *style;
      FcPattern *pattern = font_set->fonts[index];

      if (FcPatternGetString(pattern, FC_FAMILY, 0, &family) == FcResultMatch &&
          FcPatternGetString(pattern, FC_STYLE, 0, &style) == FcResultMatch)
      {
        char *name = geda_sprintf("-%s-%s", family, style);
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
  int    maxnames = 4096;
  int    count;
  int    index;

  //font_list = XListFonts (display, pattern, maxnames, &count);

  for (index = 0; index < count; index++) {
    char *name = geda_utility_string_strdup(font_list[index]);
    g_array_append_val (listing, name);
  }
  //XFreeFontNames(font_list);

  result = count > 0 ? TRUE : FALSE;

#endif

  return result;
}

/*! \todo Finish function documentation!!!
 *  \brief geda_win32_draw_set_surface
 *  \par Function Description
 *
 */
void EdaX11Render::
geda_win32_draw_set_surface(cairo_t *cr, double scale_factor)
{
  if (surface != NULL) {
    cairo_surface_destroy (surface);
    //XFreeGC(display, gc);
  }

  surface  = cairo_get_target (cr);
  cairo_surface_reference (surface);

  //drawable = cairo_xlib_surface_get_drawable (surface);
  //display  = cairo_xlib_surface_get_display (surface);
  //screen   = *(int*)cairo_xlib_surface_get_screen (surface);
  //gc       = XCreateGC (display, drawable, 0, 0 );
  scale    = scale_factor == 0 ? 1 : scale_factor; /* not allowed to be zero */
/*
  if (0 == visual) {
    visual = cairo_xlib_surface_get_visual (surface);
  }

  if ( 0 == colormap) {
    colormap = DefaultColormap(display, screen);
  }
*/
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

#endif

  g_hash_table_foreach (font_cache, FontHashDestroyer, display);
  g_hash_table_destroy (font_cache);

  font_cache = NULL;

  if (surface != NULL) {
    cairo_surface_destroy (surface);
  }

  surface = NULL;

  //XFreeGC(display, gc);
}

/*! \brief EdaX11Render Class Constructor
 *  \par Function Description
 *  This is the constructor for the EdaX11Render class. The construction calls
 * initialize() to set variables, then becomeDaemon. If becomeDaemon is
 * not successful the program is terminated. If program successful transforms
 * the a PID file is created and the signal handle is setup.
 *
 */
EdaX11Render::EdaX11Render (const char *font_name, int size) {

  scale     = 1.0;
  screen    =  -1;
  colormap  =   0;
  font_size =  -1;
  font      = NULL;
  object    = NULL;
  surface   = NULL;
  //visual    = NULL;

#ifdef HAVE_XFT

  xftdraw   = NULL;

#endif

  CreateFontHash();

  geda_win32_draw_set_font (font_name, size);

  return;
}
