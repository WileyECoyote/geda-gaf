/* -*- C indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-*/
/*
 * File: color_guile.c
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

/*! \file color_guile.c
 *  \brief Process Scheme initialization file data.
 *
 * Contains functions to parse Scheme initialization (RC) files.
 */

#include <config.h>

#include <math.h>

#include <gdk/gdk.h>

#include <libgeda/libgeda.h>

#include "../../include/globals.h"
#include "../../include/libgedacolor.h"
#include "../../include/private.h"
#include "../../include/gettext_priv.h"
#include <geda_debug.h>

/*! \brief Load and Evaluate a Color Map Scheme
 *  \par Function Description
 *  Use to load a color map file.
 *
 *  \note \a Could actually be any scheme file
 *
 *  \param inputfile Full name of file including path.
 */
bool geda_color_guile_load_scheme (const char *inputfile)
{
  int result = FALSE;

  if (inputfile && g_file_test(inputfile, G_FILE_TEST_EXISTS)) {

    GError *err = NULL;

    result = g_evaluate_scheme_file (inputfile, &err);

    if (err != NULL) {
      const char *err_load = _("Error loading");
      u_log_message("%s %s, %s\n", err_load, inputfile, strerror(errno));
      g_clear_error (&err);
    }
  }

  return result;
}

/**   \defgroup Libgedacolor-RC-Color Libgedacolor RC Color
 *  @{\brief
 *     This group contains utility functions used when processing
 *     color map data in RC files.
 */

/*!
 * \brief Color Map to Scheme
 * \par Function Description
 *
 * \returns list of color (index . string) pairs
 * \sa geda_color_guile_print_map
 */
SCM geda_color_guile_map_to_scm (const COLOR *map)
{
  SCM result = SCM_EOL;
  int i;
  for (i = MAX_COLORS - 1; i >= 0; i--) {
    SCM color_val = SCM_BOOL_F;
    if (map[i].enabled) {
      COLOR c = map[i];
      char *rgba = geda_color_utility_encode_rgba (c.r, c.g, c.b, c.a);
      color_val = scm_from_utf8_string (rgba);
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
 * \sa geda_color_guile_map_to_scm geda_color_guile_print_map
 */
void
geda_color_guile_map_from_scm (COLOR *map, SCM lst, const char *scheme_proc_name)
{
  SCM curr               = lst;
  SCM wrong_type_arg_sym = scm_from_utf8_symbol ("wrong-type-arg");
  SCM proc_name          = scm_from_utf8_string (scheme_proc_name);

  while (curr != SCM_EOL) {

    COLOR c = {0x00, 0x00, 0x00, FALSE};
    char *key;
    int   i;

    SCM scm_entry;
    SCM s;

    scm_entry = scm_car (curr);

    /* Check map entry has correct type */
    if (!scm_is_true (scm_list_p (scm_entry)) ||
        (scm_to_int (scm_length (scm_entry)) != 2))
    {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Color map entries must be a two-element list")),
                     SCM_EOL, scm_list_1 (scm_entry));
    }

      /* Check color key is correct type, and extract it */
    s = scm_car (scm_entry);

    if (!scm_is_string (s)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Key in color map entry must be a string")),
                     SCM_EOL, scm_list_1 (s));
    }

    key = scm_to_utf8_string(s);

    i = geda_color_key_get_index (key);

    /* Check color index is within bounds.
     * If index is out of bounds warn & ignore it. */
    if (i < 0) {
      fprintf (stderr, "%s: %s\n", _("Invalid key in color map"), key);
    }
    else {

      /* If color value is #F, disable color */
      s = scm_cadr (scm_entry);

      if (scm_is_false (s)) {
        map[i].enabled = FALSE;
      }
      else {

        char *rgba;
        bool  result;

        /* Otherwise, we require a string */
        if (!scm_is_string (s)) {
          scm_error_scm (wrong_type_arg_sym, proc_name,
                         scm_from_utf8_string (_("Value in color map entry must be #f or a string")),
                         SCM_EOL, scm_list_1 (s));
        }

        rgba   = scm_to_utf8_string (s);
        result = geda_color_utility_decode_rgba (rgba, &c.r, &c.g, &c.b, &c.a);

#if DEBUG
fprintf(stderr, "%s %s=%d <%s>\n",__func__, key, i, rgba);
#endif

        if (!result) {
          fprintf (stderr, "%s: %s\n", _("Invalid color map value"), rgba);
        }
        else {
          map[i] = c;
          map[i].enabled = TRUE;
        }
        free(rgba);
      }
    }
    free(key);

    /* Get next element in map */
    curr = scm_cdr (curr);
  }

  scm_remember_upto_here_2 (wrong_type_arg_sym, proc_name);
}

/** @} endgroup Libgedacolor-RC-Color */

/**   \defgroup Libgedacolor-RC-Handlers Libgedacolor RC Handlers
 *  @{\par
 *  Contains functions to parse initialization (RC) files.
 */

/*! \brief Handles the display-color-map SCM keyword.
 *  \par Function Description
 *  Converts lisp variable while processing configuration data for the
 *  display-color-map RC entry to a color map data used when displaying
 *  colors.
 *
 *  \param [in] scm_map The color map to use
 *
 *  \returns SCM_BOOL_T always.
 */
SCM geda_color_guile_display_map (SCM scm_map)
{
  const char *me = "display-color-map";

  if (scm_is_eq (scm_map, SCM_UNDEFINED)) {
    return geda_color_guile_map_to_scm (display_colors);
  }

  SCM_ASSERT (scm_is_true (scm_list_p (scm_map)), scm_map, SCM_ARG1, me);

  display_cmap_flag = 1;

  geda_color_guile_map_from_scm (display_colors, scm_map, me);

  geda_color_x11_free();

  geda_color_x11_allocate();

  return SCM_BOOL_T;
}

/*! \brief Handles the display-outline-color-map SCM keyword.
 *  \par Function Description
 *  Converts lisp variable while processing configuration data for the
 *  display-outline-color-map RC entry to a color map data to be used
 *  for display-outline mode.
 *
 *  \param [in] scm_map The color map to use
 *
 *  \returns SCM_BOOL_T always.
 */
SCM geda_color_guile_outline_map (SCM scm_map)
{
  if (scm_is_eq (scm_map, SCM_UNDEFINED)) {
    return geda_color_guile_map_to_scm (outline_colors);
  }

  SCM_ASSERT (scm_is_true (scm_list_p (scm_map)),
              scm_map, SCM_ARG1, "display-outline-color-map");

  outline_cmap_flag = 1;

  geda_color_guile_map_from_scm (outline_colors, scm_map, "display-outline-color-map");

  return SCM_BOOL_T;
}

/*! \brief Handles the print-color-map SCM keyword.
 *  \par Function Description
 *  Specify a  color map to be used for printing.
 *
 *  \param [in] scm_map The color map to use
 *
 *  \returns SCM_BOOL_T always.
 */
SCM geda_color_guile_print_map (SCM scm_map)
{
  if (scm_map == SCM_UNDEFINED) {
    return geda_color_guile_map_to_scm (print_colors);
  }

  SCM_ASSERT (scm_is_true (scm_list_p (scm_map)),
              scm_map, SCM_ARG1, "print-color-map");

  geda_color_guile_map_from_scm (print_colors, scm_map, "print-color-map");

  print_cmap_flag = 1;

  return SCM_BOOL_T;
}

/*! \brief */
static struct gsubr_t guile_func_table[] = {
  { "display-color-map",          0, 1, 0, geda_color_guile_display_map },
  { "display-outline-color-map",  0, 1, 0, geda_color_guile_outline_map },
  { "print-color-map",            0, 1, 0, geda_color_guile_print_map },
  { NULL,                         0, 0, 0, NULL } };

/*! \brief Register all libgeda functions with scheme.
 *  \par Function Description
 *  Creates g_subr_t objects to make g_rc_* functions that are defined
 *  in g_rc.c visible to Scheme.
 */
void geda_color_guile_register_handlers (void)
{
  struct gsubr_t *tmp = guile_func_table;

  while (tmp->name != NULL) {
    scm_c_define_gsubr (tmp->name, tmp->req, tmp->opt, tmp->rst, tmp->func);
    tmp++;
  }
}
/** @} endgroup Libgedacolor-RC-Handlers */
