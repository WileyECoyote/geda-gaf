/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: i_vars.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#include "../../config.h"

#include <gnetlist.h>
#include <geda_debug.h>

#define DEFAULT_HIERARCHY_NETNAME_SEPARATOR "/"
#define DEFAULT_HIERARCHY_NETATTRIB_SEPARATOR "/"
#define DEFAULT_HIERARCHY_UREF_SEPARATOR "/"
#define DEFAULT_UNNAMED_NETNAME "unnamed_net"
#define DEFAULT_UNNAMED_BUSNAME "unnamed_bus"

int   default_net_naming_priority           = NETATTRIB_ATTRIBUTE;
int   default_hierarchy_traversal           = TRUE;
int   default_hierarchy_uref_mangle         = TRUE;
int   default_hierarchy_netname_mangle      = TRUE;
int   default_hierarchy_netattrib_mangle    = TRUE;
int   default_hierarchy_netattrib_order     = APPEND;
int   default_hierarchy_netname_order       = APPEND;
int   default_hierarchy_uref_order          = APPEND;
char *default_hierarchy_netname_separator   = NULL;
char *default_hierarchy_netattrib_separator = NULL;
char *default_hierarchy_uref_separator      = NULL;
char *default_unnamed_netname               = NULL;
char *default_unnamed_busname               = NULL;

/*!
 * \brief Set Variables from Defaults after reading RC
 * \par Function Description
 *  This function assigns default values for top-level variables. The default
 *  variables are declared extern in i_var.h so that the variables maybe set
 *  by RC handlers.
 */
void i_vars_set(GedaToplevel *pr_current)
{
  /* initialize the toplevels varibles */
  geda_iface_vars_set(pr_current);

  /* set non-strings to default values*/
  pr_current->net_naming_priority        = default_net_naming_priority;
  pr_current->hierarchy_traversal        = default_hierarchy_traversal;
  pr_current->hierarchy_uref_mangle      = default_hierarchy_uref_mangle;
  pr_current->hierarchy_netname_mangle   = default_hierarchy_netname_mangle;
  pr_current->hierarchy_netattrib_mangle = default_hierarchy_netattrib_mangle;
  pr_current->hierarchy_netattrib_order  = default_hierarchy_netattrib_order;
  pr_current->hierarchy_netname_order    = default_hierarchy_netname_order;
  pr_current->hierarchy_uref_order       = default_hierarchy_uref_order;

  if (pr_current->hierarchy_uref_mangle == FALSE) {
    if (pr_current->hierarchy_uref_separator) {
      strcpy(pr_current->hierarchy_uref_separator, "/");
    }
    else {
      pr_current->hierarchy_uref_separator = geda_utility_string_strdup("/");
    }
  }

  if (!default_hierarchy_netname_separator) {
    default_hierarchy_netname_separator =
    geda_utility_string_strdup (DEFAULT_HIERARCHY_NETNAME_SEPARATOR);
  }

  if (!default_hierarchy_netattrib_separator) {
    default_hierarchy_netattrib_separator =
    geda_utility_string_strdup (DEFAULT_HIERARCHY_NETATTRIB_SEPARATOR);
  }

  if (!default_hierarchy_uref_separator) {
    default_hierarchy_uref_separator =
    geda_utility_string_strdup (DEFAULT_HIERARCHY_UREF_SEPARATOR);
  }

  if (!default_unnamed_netname) {
    default_unnamed_netname =
    geda_utility_string_strdup (DEFAULT_UNNAMED_NETNAME);
  }

  if (!default_unnamed_busname) {
    default_unnamed_busname =
    geda_utility_string_strdup (DEFAULT_UNNAMED_BUSNAME);
  }

  INIT_STR(pr_current, hierarchy_netname_separator,
           default_hierarchy_netname_separator);

  INIT_STR(pr_current, hierarchy_netattrib_separator,
           default_hierarchy_netattrib_separator);

  INIT_STR(pr_current, hierarchy_uref_separator,
           default_hierarchy_uref_separator);

  INIT_STR(pr_current, unnamed_netname, default_unnamed_netname);

  INIT_STR(pr_current, unnamed_busname, default_unnamed_busname);

}

/*!
 * \brief Setup gnetlist default configuration.
 * \par Function Description
 *  Populate the default configuration context with compiled-in
 *  defaults.
 */
void
i_vars_init_gnetlist_defaults(void)
{
  EdaConfig *cfg = eda_config_get_default_context ();

  /* This is the default name used for nets for which the user has
   * set no explicit name via the netname= or net= attributes. */
  eda_config_set_string (cfg, "gnetlist", "default-net-name", "unnamed_net");

  /* This is the default name used for buses for which the user has set
   * no explicit name via the netname= or net= attributes. */
  eda_config_set_string (cfg, "gnetlist", "default-bus-name", "unnamed_bus");

  /* By default, hierarchy processing is enabled. */
  eda_config_set_boolean (cfg, "gnetlist", "traverse-hierarchy", TRUE);

  /* By default, net= attributes beats netname= attributes. */
  eda_config_set_integer (cfg, "gnetlist", "net-naming-priority", NETATTRIB_ATTRIBUTE);

  /* Maybe should get the system contexts and then assign settings
   * to default_xxx_xxx variable before the rc files are read so
   * that the user can over-ride in local files. as it is variables
   * such as pr_current->hierarchy_traversal are set here and can
   * never be changed with an rc file, i.e. silly gaf must be used.
   */
}

/*!
 * \brief Release resources for global strings
 * \par Function Description
 *  Called during shutdown to free the global strings.
 */
void i_vars_free_strings (void)
{
  GEDA_FREE(default_hierarchy_netname_separator);
  GEDA_FREE(default_hierarchy_netattrib_separator);
  GEDA_FREE(default_hierarchy_uref_separator);
  GEDA_FREE(default_unnamed_netname);
  GEDA_FREE(default_unnamed_busname);
}
