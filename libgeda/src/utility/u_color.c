/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#include <geda_standard.h>

#include "libgeda_priv.h"
#include <geda_debug.h>

/*! \brief Decode a hexadecimal RGB or RGBA color code.
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
bool u_color_rgba_decode (const char *rgba,
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

/*! \brief Encode a hexadecimal RGB or RGBA color code.
 *  \par Function Description
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
char *u_color_rgba_encode (uint8 r, uint8 g, uint8 b, uint8 a)
{
  if (a < 0xff)
    return u_string_sprintf("#%02x%02x%02x%02x",
                           (int) r, (int) g, (int) b, (int) a);
  else
    return u_string_sprintf("#%02x%02x%02x",
                           (int) r, (int) g, (int) b);
}

/*! \brief Return pointer to hexidecimal string name of color
 *  \par Function Description
 *  The function obtains the RGB color at the given index
 *  position and calls library function u_color_rgba_encode
 *  to obtain the a pointer to hex string name of the color.
 *  This function is similar to u_color_rgba_encode but does
 *  not return a value if the given color in not enabled.
 *
 *  \sa u_color_rgba_encode
 */
char *u_color_get_hex(COLOR *c)
{
  if (c->enabled) {
    return u_color_rgba_encode (c->r, c->g, c->b, c->a);
  }

  /* didn't find a color, but there still might be more */
  return(NULL);
}

/*! \brief Return distance between colors
 *  \par Function Description
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

char *
u_color_lookup_colorname(COLOR *c1, GError **err)
{
  GArray *color_table = s_color_get_standard_names();
  int     index;
  int     max_colors;

  char *name = NULL;

  max_colors = color_table->len;

  index = 0;

  while (index >= 0 && index < max_colors) {

    ColorElement *record;
    COLOR c2;

    record = &g_array_index(color_table, ColorElement, index);

    c2.r = record->r;
    c2.g = record->g;
    c2.b = record->b;

    if (c2.r == c1->r && c2.g == c1->g && c2.b == c1->b) {

      if (record->name) {
        name = u_string_strdup(record->name);
      }
      else {

        if (!err) {
          fprintf(stderr, "%s: I see RED\n", __func__);
        }
        else {
          g_set_error (err, G_FILE_ERROR, EDA_ERROR_NUM_ERRORS, "I see RED");
        }
      }
      break;
    }
    index++;
  }

  g_array_free(color_table, TRUE);
  return name;
}

