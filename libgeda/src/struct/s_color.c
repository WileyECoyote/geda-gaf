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
  RGB_BLACK,      /* 17: junction           */
  RGB_GRAY,       /* 18: mesh-grid-major    */
  RGB_NOCOLOR,    /* 19: mesh-grid-minor    */
  RGB_BLACK,      /* 20: freestyle0         */
  RGB_BLACK,      /* 21: freestyle1         */
  RGB_BLACK,      /* 22: freestyle2         */
  RGB_BLACK,      /* 23: freestyle3         */
  RGB_BLACK,      /* 24: freestyle4         */
  RGB_BLACK,      /* 25: freestyle5         */
  RGB_BLACK,      /* 26: freestyle6         */
  RGB_BLACK,      /* 27: freestyle7         */
  RGB_BLACK,      /* 28: freestyle8         */
  RGB_BLACK,      /* 29: freestyle9         */
  RGB_ENDMAP
};

/*! \brief Get Print Color Map
 *  \par Function Documentation
 *   Returns a pointer to a new Garray containing a copy of the
 *   current color-map allocations.
 *
 *  \note The returned color-map should be freed using g_array_free.
 */
GArray *s_color_get_print_color_map()
{
  GArray* color_map;
  color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  color_map = g_array_append_vals (color_map, print_colors, MAX_COLORS);
  return color_map;
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
void s_color_map_defaults (COLOR *map)
{
  bool  reached_end = FALSE;
  COLOR c;
  int   i;

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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_color_postscript_string(int color)
{
  COLOR c;

  if (color >= MAX_COLORS) {
    fprintf(stderr,_("Color index out of range"));
    return NULL;
  }

  c = print_colors[color];

  if ((c.a == 0) || !c.enabled) {
    return NULL;
  } else {
    return u_string_sprintf ("%.3f %.3f %.3f",
                            (double) c.r/255.0,
                            (double) c.g/255.0,
                            (double) c.b/255.0);
  }
}

/*! \brief Initialises the color subsystem
 *  \par Function Description
 *  At the moment, just initialises the print color map.
 */
void s_color_init(void)
{
  s_color_map_defaults (print_colors);
}
