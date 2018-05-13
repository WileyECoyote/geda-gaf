/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/*
 * File: vams_misc.c
 *
 * gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 *
 */

#include "../../config.h"
#include <gnetlist.h>
#include <geda_debug.h>

/*!
 * \brief VAMS get attribute list
 * \ingroup (gnetlist-SCM-API)
 * \par Function Description
 *  Returns a scheme list of attribute names from the list of attributes
 *  <b>attached</b> to \a object.
 */
SCM vams_get_attribs_list (GedaObject *object)
{
  const GList *a_iter;
  SCM   list = SCM_EOL;

  /* search outside the symbol (attached attributes only) */
  a_iter = geda_object_get_attached(object);

  while (a_iter != NULL) {

    GedaObject *a_current = a_iter->data;

    if (a_current->text && a_current->text->string) {

      char *found_name;

      if (geda_attrib_object_get_name_value (a_current, &found_name, NULL)) {
        list = scm_cons (scm_from_utf8_string (found_name), list);
      }

      GEDA_FREE (found_name);
    }
    a_iter = g_list_next (a_iter);
  }

  return list;
}

/*!
 * \brief vams get package attributes
 * \par Function Description
 * \ingroup (gnetlist-SCM-API)
 *  Retrieves a list of the names of attributes attached to the
 *  symbol given by \a scm_uref at schematic level.
 *
 *  Alias gnetlist:vams-get-package-attributes
 *
 * \param [in] scm_uref string reference designator.
 *
 * \return list of lists of pairs of names
 */
SCM vams_get_package_attributes(SCM scm_uref)
{
  NETLIST *nl_iter;
  char    *uref;

  SCM_ASSERT(scm_is_string (scm_uref), scm_uref, SCM_ARG1,
             "gnetlist:vams-get-package-attributes");

  uref = scm_to_utf8_string (scm_uref);

  /* here is where we make it multi page aware */
  nl_iter = netlist_head;

  /* search through the entire list for the first instance */
  while (nl_iter != NULL) {

    if (nl_iter->component_uref) {
      if (strcmp(nl_iter->component_uref, uref) == 0) {
        free (uref);
        return vams_get_attribs_list (nl_iter->object_ptr);
      }
    }
    nl_iter = nl_iter->next;
  }

  free (uref);
  return SCM_EOL;
}
