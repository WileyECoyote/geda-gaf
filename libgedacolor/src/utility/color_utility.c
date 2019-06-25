/* -*- C indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-*/
/*
 * File: color_utility.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedacolor - gEDA's Extension library for Color
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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
 */

#include <config.h>

#include <gdk/gdk.h>

#include <libgeda/libgeda.h>

#include "../../include/geda_lumins.h"
#include "../../include/globals.h"
#include "../../include/libgedacolor.h"
#include "../../include/gettext_priv.h"
#include <geda_debug.h>

/*!
 * \brief Decode a hexadecimal RGB or RGBA color code.
 * \par Function Description
 * Accepts a hexadecimal color code \a rgba of either the form "#RRGGBB"
 * or "#RRGGBBAA", and parses it to extract the numerical color values,
 * placing them in the the char pointers passed as arguments. If the
 * six-digit form is used, the alpha channel is set to full opacity.
 * If an error occurs during parsing, the return values are set to
 * solid white.
 *
 * Note that this function implements similar functionality to
 * gdk_color_parse(). This function support the alpha channel and
 * should be used to parse color strings from gEDA configuration
 *
 * \todo Use GError mechanism to give more specific error messages.
 *
 * \param [in]  rgba Color code to parse.
 * \param [out] r    Location to store red value.
 * \param [out] g    Location to store green value.
 * \param [out] b    Location to store blue value.
 * \param [out] a    Location to store alpha value.
 *
 *  \returns #TRUE on success, #FALSE on failure.
 */
bool geda_color_utility_decode_rgba (const char *rgba,
                                     uint8 *r, uint8 *g, uint8 *b, uint8 *a)
{
  int len, i, ri, gi, bi, ai;
  char c;

  /* Default to solid white */
  *r = 0xff; *g = 0xff; *b = 0xff; *a = 0xff;

  /* Check that the string is a valid length and starts with a '#' */
  len = strlen (rgba);

  if ((len != 9 && len != 7) || rgba[0] != '#')
    return FALSE;

  /* Check we only have [0-9a-fA-F] */
  for (i = 1; i < len; i++) {

    c = rgba[i];

    if ((c < '0' || c > '9') && (c < 'a' || c > 'f') && (c < 'A' || c > 'F'))
      return FALSE;
  }

  /* Use sscanf to extract values */
  c = sscanf (rgba + 1, "%2x%2x%2x", &ri, &gi, &bi);
  if (c != 3)
    return FALSE;
  *r = (uint8) ri; *g = (uint8) gi; *b = (uint8) bi;

  if (len == 9) {
    c = sscanf (rgba + 7, "%2x", &ai);
    if (c != 1)
      return FALSE;
    *a = (uint8) ai;
  }

  return TRUE;
}

/*!
 * \brief Encode a hexadecimal RGB or RGBA color code.
 * \par Function Description
 *  Encodes four color components into either the form "#RRGGBB" or
 *  "#RRGGBBAA". The shorter form is used when the alpha component is
 *  0xff.
 *
 * \param [in] r Red component.
 * \param [in] g Green component.
 * \param [in] b Blue component.
 * \param [in] a Alpha component.
 *
 * \returns A newly allocated string containing the encoded string.
 */
char *geda_color_utility_encode_rgba (uint8 r, uint8 g, uint8 b, uint8 a)
{
  if (a < 0xff) {
    return geda_sprintf("#%02x%02x%02x%02x",
                           (int) r, (int) g, (int) b, (int) a);
  }
  else {
    return geda_sprintf("#%02x%02x%02x",
                           (int) r, (int) g, (int) b);
  }
}

/*!
 * \brief Return pointer to hexidecimal string name of color
 * \par Function Description
 *  The function obtains the RGB color at the given index position
 *  and calls library function geda_color_utility_encode_rgba to
 *  obtain the a pointer to hex string name of the color. This
 *  function is similar to geda_color_utility_encode_rgba but does
 *  not return a value if the given color in not enabled.
 *
 * \sa geda_color_utility_encode_rgba
 */
char *geda_color_utility_get_hex(COLOR *c)
{
  if (c->enabled) {
    return geda_color_utility_encode_rgba (c->r, c->g, c->b, c->a);
  }

  /* Did not find a color, but there still might be more */
  return (NULL);
}

/*!
 * \brief Return distance between colors
 * \par Function Description
 *  Calculates the distance squared between colors *c1 and *c2.
 *  RGB values scaled
 */
long u_color_dist(COLOR *c1, COLOR *c2)
{
    long r, g, b;

    /* distance components between *c1 & *c2 */
    r = c1->r - c2->r;
    g = c1->g - c2->g;
    b = c1->b - c2->b;

    /* distance squared */
    return Yred2 * r * r + Ygre2 * g * g + Yblu2 * b * b;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *geda_color_utility_lookup_name(COLOR *c1, GError **err)
{
  GArray *color_table = geda_color_get_standard_names();
  int     index;
  int     max_colors;

  char *name = NULL;

  max_colors = color_table->len;

  index = 0;

#if DEBUG
fprintf(stderr, "%s: looking for %02x%02x%02x\n", __func__,(int)c1->r, (int)c1->g, (int)c1->b);
#endif

  while (index >= 0 && index < max_colors) {

    ColorElement *record;
    COLOR c2;

    record = &g_array_index(color_table, ColorElement, index);

    c2.r = record->r;
    c2.g = record->g;
    c2.b = record->b;

    if (c2.r == c1->r && c2.g == c1->g && c2.b == c1->b) {

      if (record->name) {
        name = geda_utility_string_strdup(record->name);
      }
      else {

        if (!err) {
          fprintf(stderr, "%s: I see RED\n", __func__);
        }
        else {
          g_set_error (err, EDA_ERROR, EDA_ERROR_NUM_ERRORS, "I see RED");
        }
      }
      break;
    }
    index++;
  }

  g_array_free(color_table, TRUE);
  return name;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *geda_color_utility_postscript(int color)
{
  COLOR c;

  if (color >= MAX_COLORS) {
    fprintf(stderr,_("Color index out of range"));
    return NULL;
  }

  c = print_colors[color];

  if ((c.a == 0) || !c.enabled) {
    return NULL;
  }
  else {
    return geda_sprintf ("%.3f %.3f %.3f",
                            (double) c.r/255.0,
                            (double) c.g/255.0,
                            (double) c.b/255.0);
  }
}
