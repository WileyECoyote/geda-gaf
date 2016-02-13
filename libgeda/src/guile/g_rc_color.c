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
#include <config.h>

#include <libgeda_priv.h>
#include <geda_debug.h>

/**   \defgroup Libgeda-RC-Color Libgeda RC Color
 *  @{\par
 *     This Group contains utility functions used when processing
 *     color map data in RC files.
 *    \ingroup (Libgeda-RC-Utilities)
 */

/*!
 * \brief Color Map to Scheme
 * \par Function Description
 *  This is a helper for "get" color map Scheme API handlers;
 *  display-color-map, print-color-map, outline-color-map.
 *  assimilates a list of pairs of integer color indexes
 *  and the string hex representation of the color for the
 *  corresponding index in \a map.
 *
 * \returns list of color (index . string) pairs
 */
SCM g_rc_color_map_to_scm (const COLOR *map)
{
  SCM result = SCM_EOL;
  int i;

  for (i = MAX_COLORS - 1; i >= 0; i--) {

    SCM color_val = SCM_BOOL_F;

    if (map[i].enabled) {
      COLOR c    = map[i];
      char *rgba = u_color_rgba_encode (c.r, c.g, c.b, c.a);
      color_val  = scm_from_utf8_string (rgba);
      g_free (rgba);
    }
    result = scm_cons (scm_list_2 (scm_from_int (i), color_val), result);
  }
  return result;
}

/** @} endgroup Libgeda-RC-Color */
