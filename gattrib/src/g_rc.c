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

#include "../include/gattrib.h"
#include "../include/g_rc.h"
#include "../include/i_vars.h"     /* This holds all the guile variable defs */

/*!
 * \brief Test the version of gattrib and gEDA/gaf
 * \par Function Description
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

  if (strcmp (version, PACKAGE_DATE_VERSION) != 0) {

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

/*!
 * \brief This function processes the hide-columns RC entry.
 * \par Function Description
 *  This function reads the string list from the hide-columns
 *  configuration parameter and converts the list into a GList.
 *  The GList is stored in the global default_hide_columns variable.
 */
SCM g_rc_hide_columns(SCM stringlist)
{
  GList *list=NULL;
  int    length, i;

  SCM_ASSERT(scm_list_p(stringlist), stringlist, SCM_ARG1, "scm_is_list failed");
  length = scm_ilength(stringlist);

  /* If the command is called multiple times, remove the old list before
     recreating it */
  geda_glist_free_full(default_hide_columns, g_free);

  scm_dynwind_begin(0);
  scm_dynwind_unwind_handler(geda_gslist_free_all, (void*)&list, 0);

  /* convert the scm list into a GList */
  for (i=0; i < length; i++) {

    char  *attr;
    char *str;
    SCM elem = scm_list_ref(stringlist, scm_from_int(i));

    SCM_ASSERT(scm_is_string(elem), elem, SCM_ARG1, "list element is not a string");

    str = scm_to_utf8_string(elem);
    attr = geda_strdup(str);
    free(str);
    list = g_list_prepend(list, attr);
  }

  scm_dynwind_end();

  default_hide_columns = g_list_reverse(list);

  return SCM_BOOL_T;
}

/*!
 * \brief This function processes the sort-components RC entry.
 * \par Function Description
 *  C function to capture lisp variable while processing
 *  configuration data for the sort-components RC entry.
 */
SCM g_rc_sort_components(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("sort-components", default_sort_components, mode_table);
}

/*!
 * \brief This function processes the tearoff-menus RC entry.
 * \par Function Description
 *  C function processes configuration data for the tearoff-menus
 *  RC entry.
 */
SCM g_rc_tearoff_menus(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("tearoff-menus", default_tearoff_menus, mode_table);
}
