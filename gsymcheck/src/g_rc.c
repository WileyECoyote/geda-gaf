/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: g_rc.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
 *
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

  version = scm_to_utf8_string (scm_version);
  if (g_ascii_strcasecmp(version, PACKAGE_DATE_VERSION) != 0) {
    fprintf(stderr, _(
      "You are running gEDA/gaf version [%s%s.%s],\n"
      "but you have a version [%s] gsymcheckrc file:\n[%s]\n"
      "Please be sure that you have the latest rc file.\n"),
      PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION,
      version, rc_filename);
    ret = SCM_BOOL_F;
  }

  free (version);
  return ret;
}

/*! \brief This function processes the component dialog RC entry.
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
  g_list_foreach(default_valid_attributes, (GFunc)g_free, NULL);
  g_list_free(default_valid_attributes);

  scm_dynwind_begin(0);
  scm_dynwind_unwind_handler(geda_utility_gslist_free_all, (void *) &list, 0);

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
