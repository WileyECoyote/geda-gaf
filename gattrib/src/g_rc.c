/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2015 Stuart D. Brorson.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*! \file
 *
 * \brief RC-file specific functions
 *
 * RC-file specific functions for Scheme. At the moment it only
 * contains a function to test the version number of the program.
 */

#include "../../version.h"

#include <gattrib.h>
#include "../include/g_rc.h"
#include "../include/i_vars.h"     /* This holds all the guile variable defs */
#include <geda_debug.h>

/*------------------------------------------------------------------*/
/*! \brief Test the version of gattrib and gEDA/gaf
 *
 * \param scm_version Version being tested
 * \returns false if incorrect version, true if OK
 */
SCM g_rc_gattrib_version(SCM scm_version)
{
  char *version;
  SCM ret = SCM_BOOL_T;

  SCM_ASSERT (scm_is_string (scm_version), scm_version,
              SCM_ARG1, "gattrib-version");

  version = scm_to_utf8_string (scm_version);

  if (g_ascii_strcasecmp (version, PACKAGE_DATE_VERSION) != 0) {

    SCM   rc_filename;
    char *sourcefile;

    sourcefile  = NULL;

    rc_filename = g_rc_parse_rc_filename ();

    if (rc_filename == SCM_BOOL_F) {
      rc_filename = scm_from_utf8_string ("unknown");
    }

    sourcefile  = scm_to_utf8_string (rc_filename);

    scm_dynwind_free (sourcefile);

    const char *running = _("This is gEDA/gattrib version");
    const char *have    = _("but you have a version");
    const char *please  = _("Please be sure that you have the latest rc file");

    fprintf(stderr, "%s [%s%s.%s],\n", running, PREPEND_VERSION_STRING,
                                                PACKAGE_DOTTED_VERSION,
                                                PACKAGE_DATE_VERSION);

    fprintf(stderr, "%s [%s] gattribrc %s \"%s\"\n",
                    have, version, _("file"), sourcefile);

    fprintf(stderr, "%s.\n", please);
    ret = SCM_BOOL_F;
  }

  free (version);
  return ret;
}

/*! \brief This function processes the sort-components RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the sort-components RC entry.
 */
SCM g_rc_sort_components(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("sort-components",
            default_sort_components, mode_table);
}

/*! \brief This function processes the tearoff-menus RC entry.
 *  \par Function Description
 *  C function processes configuration data for the tearoff-menus
 *  RC entry.
 */
SCM g_rc_tearoff_menus(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("tearoff-menus",
            default_tearoff_menus, mode_table);
}
