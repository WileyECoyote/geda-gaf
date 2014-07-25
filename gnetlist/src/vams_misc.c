/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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

#include <config.h>
#include <missing.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include <geda_debug.h>

SCM vams_get_attribs_list (Object *object)
{

  Object *o_current;
  GList  *a_iter;
  Object *a_current;
  int     val;

  char* found_name = NULL;
  SCM list         = SCM_EOL;
  o_current        = object;

  /* search outside the symbol (attached attributes only) */
  a_iter = o_current->attribs;
  while(a_iter != NULL) {
    a_current = a_iter->data;
    if (a_current->text && a_current->text->string) {
      val = o_attrib_get_name_value (a_current, &found_name, NULL);

      if (val) {
        list = scm_cons (scm_from_utf8_string (found_name), list);
      }

      GEDA_FREE (found_name);
    }
    a_iter = g_list_next (a_iter);
  }

  return list;
}

SCM vams_get_package_attributes(SCM scm_uref)
{
  NETLIST *nl_current;
  char *uref;

  SCM_ASSERT(scm_is_string (scm_uref), scm_uref, SCM_ARG1,
             "gnetlist:vams-get-package-attributes");

  uref = scm_to_utf8_string (scm_uref);

  /* here is where you make it multi page aware */
  nl_current = netlist_head;

  /* search for the first instance */
  /* through the entire list */
  while(nl_current != NULL) {

    if (nl_current->component_uref &&
        strcmp(nl_current->component_uref, uref) == 0) {
      free (uref);
      return vams_get_attribs_list (nl_current->object_ptr);
    }
    nl_current = nl_current->next;
  }

  free (uref);
  return SCM_EOL;
}
