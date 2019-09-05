/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2019 gEDA Contributors (see ChangeLog for details)
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

/*!
 * \file scheme_version.c
 * \brief Scheme API procedures for working with Libgeda EDA version list.
 */

#include "../../../config.h"
#include "../../../version.h"

#include "../../include/libgeda_priv.h"
#include "../../include/libgedaguile_priv.h"

/*!
 * \brief Returns Libgeda EDA version list.
 * \par Function Description
 * Returns the list consisting of Scheme strings representing
 * PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION, and
 * PACKAGE_GIT_COMMIT macro strings.
 */
SCM_DEFINE (libgeda_version, "%libgeda-version", 0, 0, 0,
            (),
            "Return Libgeda EDA version string list.")
{
  return scm_list_4 (scm_from_utf8_string (PREPEND_VERSION_STRING),
                     scm_from_utf8_string (PACKAGE_DOTTED_VERSION),
                     scm_from_utf8_string (PACKAGE_DATE_VERSION),
                     scm_from_utf8_string (PACKAGE_GIT_COMMIT));
}

/*!
 * \brief Create the geda core version Scheme module.
 * \par Function Description
 *  Defines procedures in the (geda core version) module. The module
 *  can be accessed using (use-modules (geda core version)).
 */
static void
init_module_libgeda_core_version (void *unused)
{
  /* Register the functions */
  #include "scheme_version.x"

  /* Add the function to the module's public definitions. */
  scm_c_export (s_libgeda_version,
                NULL);
}

/*!
 * \brief Initialize Libgeda Scheme API version procedures
 * \par Function Description
 *  Registers some core Scheme procedures for this module. Should
 *  only be called by edascm_init().
 */
void edascm_init_version ()
{
  /* Define the (geda core version) module */
  scm_c_define_module ("geda core version",
                       (void (*)(void*)) init_module_libgeda_core_version,
                       NULL);
}
