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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */
#include <config.h>
#include <missing.h>

#include <stdio.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

SCM
s_color_map_to_scm (const COLOR *map)
{
  SCM result = SCM_EOL;
  int i;
  for (i = MAX_COLORS - 1; i >= 0; i--) {
    SCM color_val = SCM_BOOL_F;
    if (map[i].enabled) {
      COLOR c = map[i];
      char *rgba = s_color_rgba_encode (c.r, c.g, c.b, c.a);
      color_val = scm_from_utf8_string (rgba);
      g_free (rgba);
    }
    result = scm_cons (scm_list_2 (scm_from_int (i), color_val), result);
  }
  return result;
}

/*!
 * \warning This function should ONLY be called from Scheme procedures.
 */
void
s_color_map_from_scm (COLOR *map, SCM lst, const char *scheme_proc_name)
{
  SCM curr = lst;
  SCM wrong_type_arg_sym = scm_from_utf8_symbol ("wrong-type-arg");
  SCM proc_name = scm_from_utf8_string (scheme_proc_name);
  while (curr != SCM_EOL) {
    int i;
    char *rgba;
    SCM s;
    COLOR c = {0x00, 0x00, 0x00, FALSE};
    bool result;
    SCM entry = scm_car (curr);

    /* Check map entry has correct type */
    if (!scm_is_true (scm_list_p (entry))
        || (scm_to_int (scm_length (entry)) != 2)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Color map entry must be a two-element list")),
                     SCM_EOL, scm_list_1 (entry));
    }

    /* Check color index has correct type, and extract it */
    s = scm_car (entry);
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
      g_critical ("Color map index out of bounds: %i\n", i);
      goto color_map_next;
    }

    /* If color value is #F, disable color */
    s = scm_cadr (entry);
    if (scm_is_false (s)) {
      map[i].enabled = FALSE;
      goto color_map_next;
    }

    /* Otherwise, we require a string */
    s = scm_cadr (entry);
    if (!scm_is_string (s)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Value in color map entry must be #f or a string")),
                     SCM_EOL, scm_list_1 (s));
    }
    rgba = scm_to_utf8_string (s);

    result = s_color_rgba_decode (rgba, &c.r, &c.g, &c.b, &c.a);

    /* FIXME should we generate a Guile error if there's a problem here? */
    if (!result) {
      g_critical ("Invalid color map value: %s\n", rgba);
    } else {
      map[i] = c;
      map[i].enabled = TRUE;
    }

  color_map_next:
    /* Go to next element in map */
    curr = scm_cdr (curr);
  }
  scm_remember_upto_here_2 (wrong_type_arg_sym, proc_name);
}
