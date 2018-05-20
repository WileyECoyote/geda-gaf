/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: g_rc.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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
 * MA 02111-1301 USA
 */

#include <version.h>

#include <libgeda/libgeda.h>
#include "../include/globals.h"
#include "../include/i_vars.h"
#include "../include/gettext.h"

/*! \brief This function processes the RC version information
 *  \par Function Description
 *       This function processes the version string in the rc file and
 *       compares the value to the current program version. A message
 *       is issued to standard error if the versions do not match,
 *
 *  \returns SCM_TRUE if versions match else FALSE
 */
SCM g_rc_gsymcheck_version(SCM scm_version)
{
  char *version;
  SCM ret = SCM_BOOL_T;

  SCM_ASSERT (scm_is_string (scm_version), scm_version,
              SCM_ARG1, "gsymcheck-version");

  scm_dynwind_begin (0);
  version = scm_to_utf8_string (scm_version);
  scm_dynwind_free (version);

  if (g_ascii_strcasecmp (version, PACKAGE_DATE_VERSION) != 0) {

    SCM rc_filename;
    char *sourcefile;

    sourcefile  = NULL;
    rc_filename = g_rc_parse_rc_filename ();

    if (rc_filename == SCM_BOOL_F) {
      rc_filename = scm_from_utf8_string ("unknown");
    }

    sourcefile  = scm_to_utf8_string (rc_filename);

    scm_dynwind_free (sourcefile);

    const char *running = _("This is gEDA/gsymcheck version");
    const char *have    = _("but you have a version");
    const char *please  = _("Please be sure that you have the latest rc file");

    fprintf(stderr, "%s [%s%s.%s],\n", running, PREPEND_VERSION_STRING,
                                                PACKAGE_DOTTED_VERSION,
                                                PACKAGE_DATE_VERSION);

    fprintf(stderr, "%s [%s] gsymcheckrc %s \"%s\"\n",
                    have, version, _("file"), sourcefile);

    fprintf(stderr, "%s.\n", please);

    ret = SCM_BOOL_F;
  }
  else {
    ret = SCM_BOOL_T;
  }

  scm_dynwind_end();

  return ret;
}

/*! \brief This function processes the known-devices RC entry.
 *  \par Function Description
 *  This function reads the string list from the known-devices
 *  configuration parameter and converts the list into a GList.
 *  The GList is stored in the global default_known_devices variable.
 */
SCM g_rc_known_devices(SCM stringlist)
{
  GList *list=NULL;
  int    length, i;

  SCM_ASSERT(scm_list_p(stringlist), stringlist, SCM_ARG1, "scm_is_list failed");
  length = scm_ilength(stringlist);

  /* If the command is called multiple times, remove the old list before
     recreating it */
  geda_glist_free_full(default_known_devices, g_free);

  scm_dynwind_begin(0);
  scm_dynwind_unwind_handler(geda_gslist_free_all, (void*)&list, 0);

  /* convert the scm list into a GList */
  for (i=0; i < length; i++) {

    char  *attr;
    char *str;
    SCM elem = scm_list_ref(stringlist, scm_from_int(i));

    SCM_ASSERT(scm_is_string(elem), elem, SCM_ARG1, "list element is not a string");

    str = scm_to_utf8_string(elem);
    attr = geda_utility_string_strdup(str);
    free(str);
    list = g_list_prepend(list, attr);
  }

  scm_dynwind_end();

  default_known_devices = g_list_reverse(list);

  return SCM_BOOL_T;

}

/*! \brief This function processes the valid-attributes RC entry.
 *  \par Function Description
 *  This function reads the string list from the valid-attributes
 *  configuration parameter and converts the list into a GList.
 *  The GList is stored in the global default_valid_attributes variable.
 */
SCM g_rc_valid_attributes(SCM stringlist)
{
  GList *list=NULL;
  int    length, i;

  SCM_ASSERT(scm_list_p(stringlist), stringlist, SCM_ARG1, "scm_is_list failed");
  length = scm_ilength(stringlist);

  /* If the command is called multiple times, remove the old list before
     recreating it */
  geda_glist_free_full(default_valid_attributes, g_free);

  scm_dynwind_begin(0);
  scm_dynwind_unwind_handler(geda_gslist_free_all, (void*)&list, 0);

  /* convert the scm list into a GList */
  for (i=0; i < length; i++) {

    char  *attr;
    char *str;
    SCM elem = scm_list_ref(stringlist, scm_from_int(i));

    SCM_ASSERT(scm_is_string(elem), elem, SCM_ARG1, "list element is not a string");

    str = scm_to_utf8_string(elem);
    attr = geda_utility_string_strdup(str);
    free(str);
    list = g_list_prepend(list, attr);
  }

  scm_dynwind_end();

  default_valid_attributes = g_list_reverse(list);

  return SCM_BOOL_T;

}

/*************************** GUILE end done *********************************/
