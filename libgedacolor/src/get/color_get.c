/* -*- C indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-*/
/*
 * File: color_get.c
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

#include <glib.h>
#include <gdk/gdk.h>

#include <libgeda/libgeda.h>

#include "../../include/geda_colors.h"
#include "../../include/globals.h"
#include "../../include/libgedacolor.h"
#include "../../include/private.h"
#include "../../include/gettext_priv.h"
#include <geda_debug.h>

extern COLOR display_colors[MAX_COLORS];
extern COLOR outline_colors[MAX_COLORS];

char *geda_color_get_color_name(int index, GArray *cmap, GError **err)
{
  int limit;



  if (cmap) {                       /* Find end of cmap */
    limit = cmap->len;
  }
  else {                            /* Use the print_colors map */
    limit = MAX_COLORS;
  }

  /* Check if index is with bounds of print_colors */
  if ((index < 0) || (index >= limit)) {

    const char *inval_index = _("Color index out of range");

    if (!err) {
      fprintf (stderr, "%s: %i\n", inval_index, index);
    }
    else {
      g_set_error (err, EDA_ERROR, EDA_ERROR_NUM_ERRORS,
                       "%s: %i\n", inval_index, index);
    }
  }
  else {

    COLOR *color;

    if (cmap) {                       /* Find end of cmap */
      color = &g_array_index (cmap, COLOR, index);
    }
    else {                            /* get offet in print_colors map */
      color = &print_colors[index];
    }

    return geda_color_utility_lookup_name (color);

  }
  return NULL;
}

/*! \brief Get the Default Color Index for an Object
 *  \par Function Description
 *
 */
int geda_color_get_object_default (char type)
{
  struct default_color_index_t {
    char type;
    int  index;
  };

  struct default_color_index_t default_color_data[] = {
    COMPLEX_COLOR_INDEX,
    NET_COLOR_INDEX,
    TEXT_COLOR_INDEX,
    PIN_COLOR_INDEX,
    LINE_COLOR_INDEX,
    CIRCLE_COLOR_INDEX,
    BUS_COLOR_INDEX,
    ARC_COLOR_INDEX,
    BOX_COLOR_INDEX,
    PICTURE_COLOR_INDEX,
    PATH_COLOR_INDEX,
    { '\0', -1 }
  };

  struct default_color_index_t *table;

  for (table = default_color_data; table->type != '\0'; table++)
    if( table->type == type)
      return table->index;

  return table->index;
}

/*! \brief Get Display Color Map
 *  \par Function Documentation
 *   Returns a pointer to a new Garray containing a copy of the
 *   current color-map allocations.
 *
 *  \note the returned color-map MUST be freed using g_array_free.
 */
GArray *geda_color_get_display_map(void)
{
  GArray *color_map;

  color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  color_map = g_array_append_vals (color_map, display_colors, MAX_COLORS);
  return color_map;
}

/*! \brief Get Display Outline Color Map
 *  \par Function Documentation
 *   Returns a pointer to a new Garray containing a copy of the
 *   current color-map allocations.
 *
 *  \note the returned color-map MUST be freed using g_array_free.
 */
GArray *geda_color_get_outline_map(void)
{
  GArray *color_map;

  color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  color_map = g_array_append_vals (color_map, outline_colors, MAX_COLORS);

  return color_map;
}

/*! \brief Get Print Color Map
 *  \par Function Documentation
 *   Returns a pointer to a new Garray containing a copy of the
 *   current color-map allocations.
 *
 *  \note The returned color-map should be freed using g_array_free.
 */
GArray *geda_color_get_print_map(void)
{
  GArray *color_map;

  color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  color_map = g_array_append_vals (color_map, print_colors, MAX_COLORS);

  return color_map;
}

/*! \brief Get a Print color given an index
 *  \par Function Description
 *  Similar to geda_color_utility_postscript but checks to
 *  insures the print_colors maps has been initialized.
 */
char *geda_color_get_print_color (int color)
{
  COLOR c;
  static bool once = 0;

  if (color >= MAX_COLORS) {
    fprintf(stderr,_("Color index out of range"));
    return NULL;
  }

  if (!print_cmap_flag && !once) {
    geda_color_struct_init ();
    once = TRUE;
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