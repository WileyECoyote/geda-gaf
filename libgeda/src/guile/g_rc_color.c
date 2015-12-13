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
#include <missing.h>

#include <stdio.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

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

/*!
 * \brief Color Map from Scheme
 * \par Function Description
 * Common routine to processes a color-map entry in an RC file.
 * Each member pair of list and converts the hex string to RGB
 * data and sets the associated slot in the given color map
 * table the resulting RGA values.
 *
 * \sa g_rc_print_color_map
 */
void
g_rc_color_map_from_scm (COLOR *map, SCM lst, const char *scheme_proc_name)
{
  SCM curr               = lst;
  SCM wrong_type_arg_sym = scm_from_utf8_symbol ("wrong-type-arg");
  SCM proc_name          = scm_from_utf8_string (scheme_proc_name);

  while (curr != SCM_EOL) {

    char *rgba;
    COLOR c = {0x00, 0x00, 0x00, FALSE};
    int i;

    SCM scm_entry;
    SCM s;

    scm_entry = scm_car (curr);

    /* Check map entry has correct type */
    if (!scm_is_true (scm_list_p (scm_entry)) ||
        (scm_to_int (scm_length (scm_entry)) != 2))
    {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Color map entry must be a two-element list")),
                     SCM_EOL, scm_list_1 (scm_entry));
    }

      /* Check color index has correct type, and extract it */
      s = scm_car (scm_entry);
    if (!scm_is_integer (s)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Index in color map entry must be an integer")),
                     SCM_EOL, scm_list_1 (s));
    }
    i = scm_to_int (s);

    /* Check color index is within bounds. If it's out of bounds, it's
     * legal, but warn & ignore it.
     *
     * FIXME one day we will have dynamically-expanding colorspace.
     * One day. */
    if ((i < 0) || (i >= MAX_COLORS)) {
      fprintf (stderr, "Color map index out of bounds: %i\n", i);
    }
    else {

      /* If color value is #F, disable color */
      s = scm_cadr (scm_entry);

      if (scm_is_false (s)) {
        map[i].enabled = FALSE;
      }
      else {

        bool result;

        /* Otherwise, we require a string */
        s = scm_cadr (scm_entry);

        if (!scm_is_string (s)) {
          scm_error_scm (wrong_type_arg_sym, proc_name,
                         scm_from_utf8_string (_("Value in color map entry must be #f or a string")),
                         SCM_EOL, scm_list_1 (s));
        }

        rgba = scm_to_utf8_string (s);

        result = u_color_rgba_decode (rgba, &c.r, &c.g, &c.b, &c.a);

        if (!result) {
          fprintf (stderr, "Invalid color map value: %s\n", rgba);
        }
        else {
          map[i] = c;
          map[i].enabled = TRUE;
        }
        free(rgba);
      }
    }

    /* Get next element in map */
    curr = scm_cdr (curr);
  }

  scm_remember_upto_here_2 (wrong_type_arg_sym, proc_name);
}

/** @} endgroup Libgeda-RC-Color */
