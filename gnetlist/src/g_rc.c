/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "../../config.h"
#include "../../version.h"

#include <gnetlist.h>
#include "../include/i_vars.h"
#include <geda_debug.h>

/*! \brief This function processes the RC version information
 *  \par Function Description
 *       This function processes the version string in the rc file and
 *       compares the value to the current program version. A message
 *       is issued to standard error if the versions do not match,
 *
 *  \returns SCM_TRUE if versions match else FALSE
 */
SCM g_rc_gnetlist_version(SCM scm_version)
{
  char *version;
  SCM ret = SCM_BOOL_T;

  SCM_ASSERT (scm_is_string (scm_version), scm_version,
              SCM_ARG1, "gnetlist-version");

  version = scm_to_utf8_string (scm_version);
  if (strcmp (version, PACKAGE_DATE_VERSION) != 0) {
    fprintf(stderr,
            "You are running gEDA/gnetlist version [%s%s.%s],\n",
            PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION,
            PACKAGE_DATE_VERSION);
    fprintf(stderr,
            "but you have a version [%s] gnetlistrc file:\n\"%s\"\n",
            version, rc_filename);
    fprintf(stderr,
            "Please be sure that you have the latest rc file.\n");
    ret = SCM_BOOL_F;
  }

  free (version);
  return ret;
}

/*! \brief Process net-naming-priority Keyword
 *  \par Function Description
 *   Indirectly returns value of configuration setting in
 *   default_net_naming_priority based on the string option
 *   in RC file for the net-naming-priority keyword.
 */
SCM g_rc_net_naming_priority(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {NETATTRIB_ATTRIBUTE, "netattrib"},
    {NETNAME_ATTRIBUTE, "netname"}
  };

  RETURN_G_RC_MODE("net-naming-priority",
                    default_net_naming_priority, mode_table);
}

/*! \brief Process hierarchy-traversal Keyword
 *  \par Function Description
 *   Indirectly returns value of configuration setting in
 *   default_hierarchy_traversal based on the string option
 *   in RC file for the hierarchy-traversal keyword.
 */
SCM g_rc_hierarchy_traversal(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("hierarchy-traversal",
                    default_hierarchy_traversal, mode_table);
}

/*! \brief Process hierarchy-uref-mangle Keyword
 *  \par Function Description
 *   Indirectly returns value of configuration setting in
 *   default_hierarchy_uref_mangle based on the string option
 *   in RC file for the hierarchy-uref-mangle keyword.
 */
SCM g_rc_hierarchy_uref_mangle(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("hierarchy-uref-mangle",
                    default_hierarchy_uref_mangle, mode_table);
}

/*! \brief Process hierarchy-netname-mangle Keyword
 *  \par Function Description
 *   Indirectly returns value of configuration setting in
 *   default_hierarchy_netname_mangle based on the string
 *   option in RC file for the hierarchy-netname-mangle
 *   keyword.
 */
SCM g_rc_hierarchy_netname_mangle(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("hierarchy-netname-mangle",
                    default_hierarchy_netname_mangle, mode_table);
}

/*! \brief Process hierarchy-netattrib-mangle Keyword
 *  \par Function Description
 *   Indirectly returns value of configuration setting in
 *   g_rc_hierarchy_netattrib_mangle based on the string
 *   option in RC file for the hierarchy-netattrib-mangle
 *   keyword.
 */
SCM g_rc_hierarchy_netattrib_mangle(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("hierarchy-netattrib-mangle",
                    default_hierarchy_netattrib_mangle, mode_table);
}

/*! \brief Process hierarchy-netname-separator Keyword
 *  \par Function Description
 *  This function sets default_hierarchy_netname_separator to
 *  be the string argument to the hierarchy-netname-separator
 *  keyword in the RC file.
 */
SCM g_rc_hierarchy_netname_separator(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "hierarchy-netname-separator");

  GEDA_FREE(default_hierarchy_netname_separator);

  default_hierarchy_netname_separator = geda_string_scm2c (name);

  return SCM_BOOL_T;
}

/*! \brief Process hierarchy-netattrib-separator Keyword
 *  \par Function Description
 *  This function sets default_hierarchy_netattrib_separator to
 *  be the string argument to the hierarchy-netattrib-separator
 *  keyword in the RC file.
 */
SCM g_rc_hierarchy_netattrib_separator(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "hierarchy-netattrib-separator");

  GEDA_FREE(default_hierarchy_netattrib_separator);

  default_hierarchy_netattrib_separator = geda_string_scm2c (name);

  return SCM_BOOL_T;
}

/*! \brief Process hierarchy-uref-separator Keyword
 *  \par Function Description
 *  This function sets default_hierarchy_uref_separator to
 *  be the string argument to the hierarchy-uref-separator
 *  keyword in the RC file.
 */
SCM g_rc_hierarchy_uref_separator(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "hierarchy-uref-separator");

  GEDA_FREE(default_hierarchy_uref_separator);

  default_hierarchy_uref_separator = geda_string_scm2c (name);

  return SCM_BOOL_T;
}

/*! \brief Process hierarchy-netattrib-order Keyword
 *  \par Function Description
 *   Indirectly returns value of configuration setting in
 *   default_hierarchy_netattrib_order based on the string
 *   option in RC file for the hierarchy-netattrib-order
 *   keyword.
 */
SCM g_rc_hierarchy_netattrib_order(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {PREPEND, "prepend"},
    {APPEND, "append"}
  };

  RETURN_G_RC_MODE("hierarchy-netattrib-order",
                    default_hierarchy_netattrib_order, mode_table);
}

/*! \brief Process hierarchy-netname-order Keyword
 *  \par Function Description
 *   Indirectly returns value of configuration setting in
 *   default_hierarchy_netname_order based on the string
 *   option in RC file for the hierarchy-netname-order
 *   keyword.
 */
SCM g_rc_hierarchy_netname_order(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {PREPEND, "prepend"},
    {APPEND, "append"}
  };

  RETURN_G_RC_MODE("hierarchy-netname-order",
                    default_hierarchy_netname_order, mode_table);
}

/*! \brief Process hierarchy-uref-order Keyword
 *  \par Function Description
 *   Indirectly returns value of configuration setting in
 *   default_hierarchy_uref_order based on the string
 *   option in RC file for the hierarchy-uref-order
 *   keyword.
 */
SCM g_rc_hierarchy_uref_order(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {PREPEND, "prepend"},
    {APPEND, "append"}
  };

  RETURN_G_RC_MODE("hierarchy-uref-order",
                    default_hierarchy_uref_order, mode_table);
}

/*! \brief Process unamed-netname Keyword
 *  \par Function Description
 *  This function sets default_unnamed_netname to be the string
 *  argument to the unamed-netname keyword in the RC file.
 */
SCM g_rc_unnamed_netname(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "unamed-netname");

  GEDA_FREE(default_unnamed_netname);

  default_unnamed_netname = geda_string_scm2c (name);

  return SCM_BOOL_T;
}

/*! \brief Process unamed-busname Keyword
 *  \par Function Description
 *  This function sets default_unnamed_busname to be the string
 *  argument to the unamed-busname keyword in the RC file.
 */
SCM g_rc_unnamed_busname(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "unamed-busname");

  GEDA_FREE(default_unnamed_busname);

  default_unnamed_busname = geda_string_scm2c (name);

  return SCM_BOOL_T;
}

/*************************** GUILE end done *********************************/

