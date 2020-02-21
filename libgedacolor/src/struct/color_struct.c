/* -*- C indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-*/
/*
 * File: color_struct.c
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
 *
 * Contributing Author: Wiley Edward Hill
 * Date Contributed: September, 15, 2015
 */

#include <config.h>

#include <geda/geda.h>
#include <geda/geda_idefines.h>       /* for MAX_COLORS */

#include <gdk/gdk.h>

#include "../../include/geda_color.h"
#include "../../include/geda_colors.h"
#include "../../include/globals.h"
#include "../../include/gettext_priv.h"
#include <geda_debug.h>

extern COLOR display_colors[MAX_COLORS];
extern COLOR outline_colors[MAX_COLORS];

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

/*! \struct stdcolors
 *  \brief Standard Color Name Table
 *  \par
 *  The following table represents mostly standardized color names,
 *  many of which correspond to names in the X.org file X11/rgb.txt.
 */
static ColorElement stdcolors [] =
{
  { 0x00, 0x00, 0x00, "black"},
  { 0x00, 0x00, 0x80, "navy blue"},            /* Navy */
  { 0x00, 0x00, 0x8B, "dark blue"},
  { 0x00, 0x00, 0xCD, "medium blue"},
  { 0x00, 0x00, 0xFF, "blue"},
  { 0x00, 0x64, 0x00, "dark green"},
  { 0x00, 0x80, 0x00, "web green"},
  { 0x00, 0x80, 0x80, "teal"},
  { 0x00, 0x8B, 0x00, "green4"},
  { 0x00, 0x8B, 0x8B, "dark cyan"},
  { 0x00, 0xBF, 0xFF, "deep sky blue"},
  { 0x00, 0xCE, 0xD1, "dark turquoise"},
  { 0x00, 0xFA, 0x9A, "medium spring green"},
  { 0x00, 0xFF, 0x00, "green"},
  { 0x00, 0xFF, 0x7F, "spring green"},
  { 0x00, 0xFF, 0xFF, "aqua"},                 /* Cyan */
  { 0x00, 0xFF, 0xFF, "cyan"},                 /* Aqua */
  { 0x19, 0x19, 0x70, "midnight blue"},
  { 0x1E, 0x90, 0xFF, "dodger blue"},
  { 0x20, 0xB2, 0xAA, "light sea green"},
  { 0x22, 0x8B, 0x22, "forest green"},
  { 0x2E, 0x8B, 0x57, "sea green"},
  { 0x2F, 0x4F, 0x4F, "dark slate gray"},      /* Dark Slate Grey */
  { 0x32, 0xCD, 0x32, "lime green"},           /* X11 Green, Lime */
  { 0x3C, 0xB3, 0x71, "medium sea green"},
  { 0x40, 0xE0, 0xD0, "turquoise"},
  { 0x41, 0x69, 0xE1, "royal blue"},
  { 0x46, 0x82, 0xB4, "steel blue"},
  { 0x48, 0x3D, 0x8B, "dark slate blue"},
  { 0x48, 0xD1, 0xCC, "medium turquoise"},
  { 0x4B, 0x00, 0x82, "indigo"},
  { 0x55, 0x6B, 0x2F, "dark olive green"},
  { 0x5F, 0x9E, 0xA0, "cadet blue"},
  { 0x64, 0x95, 0xED, "cornflower"},
  { 0x66, 0x33, 0x99, "rebecca purple"},
  { 0x66, 0xCD, 0xAA, "medium aquamarine"},
  { 0x69, 0x69, 0x69, "dim gray"},             /* Dim Grey */
  { 0x6A, 0x5A, 0xCD, "slate blue"},
  { 0x6B, 0x8E, 0x23, "olive drab"},
  { 0x70, 0x80, 0x90, "slate gray"},           /* Slate Grey */
  { 0x77, 0x88, 0x99, "light slate gray"},     /* Light Slate Grey */
  { 0x7B, 0x68, 0xEE, "medium slate blue"},
  { 0x7C, 0xFC, 0x00, "lawn green"},
  { 0x7F, 0x00, 0x00, "web maroon"},
  { 0x7F, 0x00, 0x7F, "web purple"},
  { 0x7F, 0xFF, 0x00, "chartreuse"},
  { 0x7F, 0xFF, 0xD4, "aquamarine"},
  { 0x80, 0x00, 0x00, "maroon"},
  { 0x80, 0x80, 0x00, "olive"},
  { 0x80, 0x80, 0x80, "web gray"},             /* Web Grey */
  { 0x87, 0xCE, 0xEB, "sky blue"},
  { 0x87, 0xCE, 0xFA, "light sky blue"},
  { 0x8A, 0x2B, 0xE2, "blue violet"},
  { 0x8B, 0x00, 0x00, "dark red"},
  { 0x8B, 0x00, 0x8B, "dark magenta"},
  { 0x8B, 0x45, 0x13, "saddle brown"},
  { 0x8F, 0xBC, 0x8F, "dark sea green"},
  { 0x90, 0xEE, 0x90, "light green"},
  { 0x93, 0x70, 0xDB, "medium purple"},
  { 0x94, 0x00, 0xD3, "dark violet"},
  { 0x98, 0xFB, 0x98, "pale green"},
  { 0x99, 0x32, 0xCC, "dark orchid"},
  { 0x9A, 0xCD, 0x32, "yellow green"},
  { 0xA0, 0x20, 0xF0, "purple"},               /* X11 Purple */
  { 0xA0, 0x52, 0x2D, "sienna"},
  { 0xA5, 0x2A, 0x2A, "brown"},
  { 0xA9, 0xA9, 0xA9, "dark gray"},            /* Dark Grey */
  { 0xAD, 0xD8, 0xE6, "light blue"},
  { 0xAD, 0xFF, 0x2F, "green yellow"},
  { 0xAF, 0xEE, 0xEE, "pale turquoise"},
  { 0xB0, 0x30, 0x60, "rich maroon"},          /* X11 Maroon */
  { 0xB0, 0xC4, 0xDE, "light steel blue"},
  { 0xB0, 0xE0, 0xE6, "powder blue"},
  { 0xB2, 0x22, 0x22, "firebrick"},
  { 0xB8, 0x86, 0x0B, "dark goldenrod"},
  { 0xBA, 0x55, 0xD3, "medium orchid"},
  { 0xBC, 0x8F, 0x8F, "rosy brown"},
  { 0xBD, 0xB7, 0x6B, "dark khaki"},
  { 0xBE, 0xBE, 0xBE, "gray"},                 /* Grey, X11 Gray, X11 Grey */
  { 0xBF, 0xFF, 0x00, "lime"},                 /* also known Lemon lime */
  { 0xC0, 0xC0, 0xC0, "silver"},
  { 0xC7, 0x15, 0x85, "medium violet red"},
  { 0xCD, 0x5C, 0x5C, "indian red"},
  { 0xCD, 0x85, 0x3F, "peru"},
  { 0xD1, 0xE2, 0x31, "pear"},
  { 0xD2, 0x69, 0x1E, "chocolate"},
  { 0xD2, 0xB4, 0x8C, "tan"},
  { 0xD3, 0xD3, 0xD3, "light gray"},           /* Light Grey */
  { 0xD8, 0xBF, 0xD8, "thistle"},
  { 0xDA, 0x70, 0xD6, "orchid"},
  { 0xDA, 0xA5, 0x20, "goldenrod"},
  { 0xDB, 0x70, 0x93, "pale violet red"},
  { 0xDC, 0x14, 0x3C, "crimson"},
  { 0xDC, 0xDC, 0xDC, "gainsboro"},
  { 0xDD, 0xA0, 0xDD, "plum"},
  { 0xDE, 0xB8, 0x87, "burlywood"},
  { 0xE0, 0xFF, 0xFF, "light cyan"},
  { 0xE6, 0xE6, 0xFA, "lavender"},
  { 0xE9, 0x96, 0x7A, "dark salmon"},
  { 0xE9, 0xFF, 0xDB, "nyanza"},
  { 0xEE, 0x82, 0xEE, "violet"},
  { 0xEE, 0xE8, 0xAA, "pale goldenrod"},
  { 0xEF, 0xDE, 0xCD, "almond"},
  { 0xF0, 0x80, 0x80, "light coral"},
  { 0xF0, 0xE6, 0x8C, "khaki"},
  { 0xF0, 0xF8, 0xFF, "alice blue"},
  { 0xF0, 0xFF, 0xF0, "honeydew"},
  { 0xF0, 0xFF, 0xFF, "azure"},
  { 0xF4, 0xA4, 0x60, "sandy brown"},
  { 0xF5, 0xDE, 0xB3, "wheat"},
  { 0xF5, 0xF5, 0xDC, "beige"},
  { 0xF5, 0xF5, 0xF5, "white smoke"},
  { 0xF5, 0xFF, 0xFA, "mint cream"},
  { 0xF8, 0xF4, 0xFF, "magnolia"},
  { 0xF8, 0xF8, 0xFF, "ghost white"},
  { 0xFA, 0x80, 0x72, "salmon"},
  { 0xFA, 0xEB, 0xD7, "antique white"},
  { 0xFA, 0xF0, 0xE6, "linen"},
  { 0xFA, 0xFA, 0xD2, "light goldenrod"},
  { 0xFD, 0xF5, 0xE6, "old lace"},
  { 0xFF, 0x00, 0x00, "red"},
  { 0xFF, 0x00, 0xFF, "fuchsia"},              /* Magenta */
  { 0xFF, 0x00, 0xFF, "magenta"},              /* Fuchsia */
  { 0xFF, 0x14, 0x93, "deep pink"},
  { 0xFF, 0x45, 0x00, "orange red"},
  { 0xFF, 0x63, 0x47, "tomato"},
  { 0xFF, 0x69, 0xB4, "hot pink"},
  { 0xFF, 0x7F, 0x50, "coral"},
  { 0xFF, 0x8C, 0x00, "dark orange"},
  { 0xFF, 0xA0, 0x7A, "light salmon"},
  { 0xFF, 0xA5, 0x00, "orange"},
  { 0xFF, 0xB6, 0xC1, "light pink"},
  { 0xFF, 0xC0, 0xCB, "pink"},
  { 0xFF, 0xD7, 0x00, "gold"},
  { 0xFF, 0xDA, 0xB9, "peach puff"},
  { 0xFF, 0xDE, 0xAD, "navajo white"},
  { 0xFF, 0xE4, 0xB5, "moccasin"},
  { 0xFF, 0xE4, 0xC4, "bisque"},
  { 0xFF, 0xE4, 0xE1, "misty rose"},
  { 0xFF, 0xE5, 0xB4, "peach"},
  { 0xFF, 0xEB, 0xCD, "blanched almond"},
  { 0xFF, 0xEF, 0xD5, "papaya whip"},
  { 0xFF, 0xF0, 0xF5, "lavender blush"},
  { 0xFF, 0xF5, 0xEE, "seashell"},
  { 0xFF, 0xF8, 0xDC, "cornsilk"},
  { 0xFF, 0xFA, 0xCD, "lemon chiffon"},
  { 0xFF, 0xFA, 0xF0, "floral white"},
  { 0xFF, 0xFA, 0xFA, "snow"},
  { 0xFF, 0xFF, 0x00, "yellow"},
  { 0xFF, 0xFF, 0xE0, "light yellow"},
  { 0xFF, 0xFF, 0xF0, "ivory"},
  { 0xFF, 0xFF, 0xFF, "white"},
  { 0,0,0, NULL}
};

/*!
 * \brief Get Table of Standard Color Names
 * \par Function Documentation
 *  Returns a pointer to a new Garray containing a copy of the
 *  stdcolors allocations.
 *
 * \returns color_table, the table should be freed using g_array_free.
 */
GArray *geda_color_get_standard_names(void)
{
  GArray *color_table;

  color_table = g_array_sized_new (FALSE, FALSE, sizeof(ColorElement), G_N_ELEMENTS(stdcolors));
  color_table = g_array_append_vals (color_table, stdcolors, G_N_ELEMENTS(stdcolors));

  return color_table;
}

/*!
 * \brief Initialize a color map to B&W
 * \par Function Description
 *  Initializes a color map to simple defaults: black features
 *  on a white background, with "special" colors as gray.
 *
 * \warning \a map must have length of at least #MAX_COLORS.
 *
 * \param map Color map to initialize.
 */
static void geda_color_struct_map_defaults (COLOR *map)
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

/*!
 * \brief Initializes the color system for the application.
 * \par Function Documentation
 *  Initializes color maps to default values.
 */
void geda_color_struct_init(void)
{
  geda_color_struct_map_defaults (display_colors);
  geda_color_struct_map_defaults (outline_colors);
  geda_color_struct_map_defaults (print_colors);

}

/*!
 * \brief Frees memory used by the color system.
 * \par Function Documentation
 *  This function does nothing.
 */
void geda_color_struct_release_resources(void)
{

}
