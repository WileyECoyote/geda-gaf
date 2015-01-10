/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: g_util.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2011-2015 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2012-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */

/*! \file g_util.c
 * \brief Scheme utility functions
 */
/************************ REVISION HISTORY *************************
;; Who |   When   |  What (Why)
;; ------------------------------------------------------------------
;; PB  | ??/??/11 |  Inital release.
;; ------------------------------------------------------------------
;; WEH | 10/15/12 | Added u_string_int2str, u_string_scm2c function, u_string_strequal
;;                | (so that these generic functions could be used
;;                | shared rather than defined locally in a module.
;;                | Update address for Free Software Foundation.
;; ------------------------------------------------------------------
;; WEH | 01/18/13 | Relocated generic string stuff to libgeda (so code
;;                | could be used by all geda apps.
;; WEH | 03/18/13 | Added function g_list_find_string.

*/

#include <config.h>

#include "gschem.h"

/*! \brief Launch default application for a URI.
 * \par Function Description
 * Launches the default application associated with \a uri_s on the
 * host platform.  Raises an error on failure.
 *
 * \note Scheme API: Implements the %show-uri procedure in the (gschem
 * core util) module.
 *
 * \sa x_show_uri().
 *
 * \param uri_s  URI to launch viewer for.
 * \return undefined value.
 */
SCM_DEFINE (show_uri, "%show-uri", 1, 0, 0, (SCM uri_s),
            "Show a URI in the associated default application")
{
  /* Check that we were passed a string. */
  if (scm_is_string (uri_s), uri_s, SCM_ARG1, s_show_uri) {

    const char *uri = scm_to_utf8_string (uri_s);

    if (!x_show_uri (uri)) {
      u_log_message( _("Could not launch URI %s\n"), uri);
    }
  }
  return SCM_UNDEFINED;
}

/*! \brief Create the (gschem core util) Scheme module.
 * \par Function Description
 * Defines procedures in the (gschem core util) module. The module can
 * be accessed using (use-modules (gschem core util)).
 */
static void init_module_gschem_core_util ()
{
  /* Register the functions */
  #include "g_util.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_show_uri, NULL);
}
/*!
 * \brief Initialise miscellaneous gschem utility procedures.
 * \par Function Description
 * Registers some Scheme utility procedures for e.g. accessing
 * miscellaneous system services.  Should only be called by
 * main_prog().
 */
void g_init_util ()
{
  /* Define the (gschem core util) module */
  scm_c_define_module ("gschem core util",
                       init_module_gschem_core_util,
                       NULL);
}

/*! \brief Copy a Tree Iter
 *  \par Function Description
 *  This function will set all pointers target to the values in source.
 *
 */
void g_copy_tree_iter(GtkTreeIter *source, GtkTreeIter *target)
{
  target->stamp       = source->stamp;
  target->user_data   = source->user_data;
  target->user_data2  = source->user_data2;
  target->user_data3  = source->user_data3;
  return;
}

/* Why was this not already included in <gtktreemodel.c> ? */
bool g_tree_model_iter_previous (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    GtkTreePath *path;
    gboolean ret;

    path = gtk_tree_model_get_path (tree_model, iter);
    ret = gtk_tree_path_prev (path);
    if (ret != FALSE)
      gtk_tree_model_get_iter (tree_model, iter, path);

    gtk_tree_path_free (path);
    return ret;
}

