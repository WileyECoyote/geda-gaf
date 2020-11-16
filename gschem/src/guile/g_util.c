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
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * PB  | ??/??/11 |  Inital release.
 * ------------------------------------------------------------------
 * WEH | 01/18/13 | Relocated generic string stuff to libgeda (so code
 *                | could be used by all geda apps.
 * ------------------------------------------------------------------
 * WEH | 03/18/13 | Added function g_list_find_string.
 * ------------------------------------------------------------------
*/

#include "../../include/gschem.h"
#include <libguile.h>             /* for doxygen */

/*!
 * \brief SCM API Launch default application for a URI.
 * \par Function Description
 *  Launches the default application associated with \a uri_s on the
 *  host platform.  Raises an error on failure.
 *
 * \note Scheme API: Implements the %show-uri procedure in the (gschem
 *       core util) module.
 *
 * \sa x_show_uri().
 *
 * \param uri_s  URI to launch viewer for.
 * \return undefined value.
 */
SCM_DEFINE (show_uri, "%show-uri", 1, 0, 0, (SCM uri_s),
            "Show a URI in the associated default application")
{
  char *uri;

  /* Check that we were passed a string. */
  SCM_ASSERT (scm_is_string (uri_s), uri_s, SCM_ARG1, s_show_uri);

  uri = scm_to_utf8_string (uri_s);

  if (!x_show_uri (uri)) {
     geda_log( "%s <%s>\n", _("Could not launch URI"), uri);
  }

  free (uri);

  return SCM_UNDEFINED;
}

/*!
 * \brief SCM API Create the (gschem core util) Scheme module
 * \par Function Description
 *  Defines procedures in the (gschem core util) module. The module can
 *  be accessed using (use-modules (gschem core util)).
 */
static void init_module_gschem_core_util (void *data)
{
  /* Register the functions */
  #include "g_util.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_show_uri, NULL);
}

/*!
 * \brief SCM API Initialize miscellaneous gschem utility procedures.
 * \par Function Description
 *  Registers some Scheme utility procedures for e.g. accessing
 *  miscellaneous system services.  Should only be called by
 *  main_prog().
 */
void g_init_util (void)
{
  /* Define the (gschem core util) module */
  scm_c_define_module ("gschem core util",
                       init_module_gschem_core_util,
                       NULL);
}
