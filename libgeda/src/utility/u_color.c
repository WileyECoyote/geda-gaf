/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>
#include <missing.h>

#include <stdio.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

COLOR print_colors[MAX_COLORS];

/* See defines in geda_colors.h */

static COLOR default_colors[] = {
  RGB_WHITE,      /*  0: background         */
  RGB_BLACK,      /*  1: pin                */
  RGB_BLACK,      /*  2: net-endpoint       */
  RGB_BLACK,      /*  3: graphic            */
  RGB_BLACK,      /*  4: net                */
  RGB_BLACK,      /*  5: attribute          */
  RGB_BLACK,      /*  6: logic-bubble       */
  RGB_BLACK,      /*  7: dots-grid          */
  RGB_BLACK,      /*  8: detached-attribute */
  RGB_BLACK,      /*  9: text               */
  RGB_BLACK,      /* 10: bus                */
  RGB_GRAY,       /* 11: select             */
  RGB_GRAY,       /* 12: bounding-box       */
  RGB_GRAY,       /* 13: zoom-box           */
  RGB_GRAY,       /* 14: stroke             */
  RGB_BLACK,      /* 15: lock               */
  RGB_NOCOLOR,    /* 16: output-background  */
  RGB_NOCOLOR,    /* 17: freestyle1         */
  RGB_NOCOLOR,    /* 18: freestyle2         */
  RGB_NOCOLOR,    /* 19: freestyle3         */
  RGB_NOCOLOR,    /* 20: freestyle4         */
  RGB_BLACK,      /* 21: junction           */
  RGB_GRAY,       /* 22: mesh-grid-major    */
  RGB_NOCOLOR,    /* 23: mesh-grid-minor    */
  RGB_ENDMAP
};

/*! \brief Initialises the color subsystem
 *  \par Function Description
 *  At the moment, just initialises the print color map.
 */
void u_color_init(void)
{
  u_color_map_defaults (print_colors);
}

/*! \brief Initialise a color map to B&W
 *  \par Function Description
 *  Initialises a color map to a simple default: black features on a
 *  white background, with "special" colors as gray.
 *
 *  \warning \a map must be have length of at least #MAX_COLORS.
 *
 *  \param map Color map to initialise.
 */
void u_color_map_defaults (COLOR *map)
{
  int i;
  bool reached_end = FALSE;
  COLOR c;
  for (i = 0; i < MAX_COLORS; i++) {
    if (reached_end) {
      map[i].enabled = FALSE;
      continue;
    }
    c = default_colors[i];
    if (c.a == 0) { /* Check for end of default map */
      reached_end = TRUE;
      i--;
      continue;
    }
    map[i] = c;
  }
}

/* \brief Decode a hexadecimal RGB or RGBA color code.
 * \par Function Description
 * Accepts a hexadecimal color code \a rgba of either the form #RRGGBB
 * or #RRGGBBAA, and parses it to extract the numerical color values,
 * placing them in the the #guchar pointers passed as arguments. If
 * the six-digit form is used, the alpha channel is set to full
 * opacity. If an error occurs during parsing, the return values are
 * set to solid white.
 *
 * Note that this function implements similar functionality to
 * gdk_color_parse(). However, for consistency, <em>only</em> this
 * function should be used to parse color strings from gEDA
 * configuration files, as gdk_color_parse() does not support the
 * alpha channel.
 *
 * \todo Use GError mechanism to give more specific error messages.
 *
 * \param [in]  rgba Colour code to parse.
 * \param [out] r    Location to store red value.
 * \param [out] g    Location to store green value.
 * \param [out] b    Location to store blue value.
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
    if ((c < '0' || c > '9')
        && (c < 'a' || c > 'f')
        && (c < 'A' || c > 'F'))
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

/* \brief Encode a hexadecimal RGB or RGBA color code.
 * \par Function Description
 * Encodes four colour components into either the form #RRGGBB or
 * #RRGGBBAA. The shorter form is used when the alpha component is
 * 0xff.
 *
 * \param [in] r Red component.
 * \param [in] g Green component.
 * \param [in] b Blue component.
 * \returns A newly allocated string containing the encoded string.
 */
char *u_color_rgba_encode (uint8 r, uint8 g, uint8 b, uint8 a)
{
  if (a < 0xff)
    return g_strdup_printf("#%02x%02x%02x%02x",
                           (int) r, (int) g, (int) b, (int) a);
  else
    return g_strdup_printf("#%02x%02x%02x",
                           (int) r, (int) g, (int) b);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *u_color_ps_string(int color)
{
  COLOR c;

  if (color >= MAX_COLORS) {
    g_warning (_("Color index out of range"));
    return NULL;
  }

  c = print_colors[color];

  if ((c.a == 0) || !c.enabled) {
    return NULL;
  } else {
    return g_strdup_printf ("%.3f %.3f %.3f",
                            (double) c.r/255.0,
                            (double) c.g/255.0,
                            (double) c.b/255.0);
  }
}
