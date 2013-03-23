/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2011-2012 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2012 gEDA Contributors (see ChangeLog for details)
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

/*! \file g_util.c
 * \brief Scheme utility functions
 */
/************************ REVISION HISTORY *************************
;; Who |   When   |  What (Why)
;; ------------------------------------------------------------------
;; PB  | ??/??/11 |  Inital release.
;; ------------------------------------------------------------------
;; WEH | 10/15/12 | Added int2str, scm_2_cstring function, strequal
;;                | (so that these generic functions could be used
;;                | shared rather than defined locally in a module.
;;                | Update address for Free Software Foundation.
;; ------------------------------------------------------------------
;; WEH | 01/18/13 | Relocated generic string stuff to libgeda (so code
;;                | could be used by all geda apps.
;; WEH | 03/18/13 | Added function g_list_find_string.

*/

#include <config.h>

#include "gschem.h"

/*! \brief Launch default application for a URI.
 * \par Function Description
 * Launches the default application associated with \a uri_s on the
 * host platform.  Raises an error on failure.
 *
 * \note Scheme API: Implements the %show-uri procedure in the (gschem
 * core util) module.
 *
 * \sa x_show_uri().
 *
 * \param uri_s  URI to launch viewer for.
 * \return undefined value.
 */
SCM_DEFINE (show_uri, "%show-uri", 1, 0, 0, (SCM uri_s),
            "Show a URI in the associated default application")
{
  /* Check that we were passed a string. */
  if (scm_is_string (uri_s), uri_s, SCM_ARG1, s_show_uri) {

    const char *uri = scm_to_utf8_string (uri_s);

    if (!x_show_uri (uri)) {
      s_log_message( _("Could not launch URI %s\n"), uri);
    }
  }
  return SCM_UNDEFINED;
}

/*! \brief Create the (gschem core util) Scheme module.
 * \par Function Description
 * Defines procedures in the (gschem core util) module. The module can
 * be accessed using (use-modules (gschem core util)).
 */
static void init_module_gschem_core_util ()
{
  /* Register the functions */
  #include "g_util.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_show_uri, NULL);
}
/*!
 * \brief Initialise miscellaneous gschem utility procedures.
 * \par Function Description
 * Registers some Scheme utility procedures for e.g. accessing
 * miscellaneous system services.  Should only be called by
 * main_prog().
 */
void g_init_util ()
{
  /* Define the (gschem core util) module */
  scm_c_define_module ("gschem core util",
                       init_module_gschem_core_util,
                       NULL);
}

/*! \brief Detect item in list
 *
 * Look for item in the list.
 *
 * \param list pointer to the STRING_LIST struct
 * \param item string to search for
 * \returns 0 if absent, 1 if present
 */
int g_list_find_string(GList *list, char *str) {

  int len;
  int index = -1;
  char *ptr;

  /* return -1 if list is empty  */
  len = g_list_length(list);
  if ( len != 0 ) {

    for (index = 0; index < len; index++) {

      ptr = g_list_nth_data(list, index);
      if (ptr == NULL ) {
        index = -1;
        break;
      }

      if (strcmp(ptr, str) == 0) {
      /* Found item already in list.  return index. */
        break;
      }
    }
  }
  if (index == len)
    index = -1;     /* item was not in the list, so return -2 */

  return index;

}

/*! \brief Free a Glist of Strings
 *  \par Function Description
 *  This function will free all strings in a glist.
 *
 */
void g_list_free_string(void *data)
{
  GList *iter, *glst = *((GList **) data);

  for (iter = glst; iter != NULL; iter = g_list_next (iter)) {
    g_free (iter->data);
  }
  g_list_free (glst);
}

/*! \brief Free a Glist completely
 *  \par Function Description
 *  This function will free all of the data in a glist.
 *
 */
GList* g_list_clear(GList* list){

  if (list != NULL ) {

    g_list_foreach(list, (GFunc)g_free, NULL);
    lambda (const char* data)
    {
      list = g_list_remove( list, data);
      return FALSE;
    }
    foreach (list);

    g_list_free(list);
    list = NULL;
  }
  return list;
}

/*! \brief Copy a Tree Iter
 *  \par Function Description
 *  This function will set all pointers target to the values in source.
 *
 */
void g_copy_tree_iter(GtkTreeIter *source, GtkTreeIter *target)
{
  target->stamp       = source->stamp;
  target->user_data   = source->user_data;
  target->user_data2  = source->user_data2;
  target->user_data3  = source->user_data3;
  return;
}



