/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
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
 *
 * Date: August, 27, 2014
 * Contributing Author: Wiley Edward Hill
 */

#include <geda_standard.h>

#include "libgeda_priv.h"

#include <geda_debug.h>

/*! \brief Detect item in double linked list
 *
 * Look for item in the list.
 *
 * \param list pointer to the STRING_LIST struct
 * \param str  string to search for
 *
 * \returns 0 if absent, 1 if present
 */
int u_glist_find_string(GList *list, char *str) {

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
void u_glist_free_strings(void *data)
{
  GList *iter, *glst = *((GList **) data);

  for (iter = glst; iter != NULL; iter = g_list_next (iter)) {
    GEDA_FREE (iter->data);
  }
  g_list_free (glst);
}

/*! \brief Free a Glist completely
 *  \par Function Description
 *  This function will free all of the data in a glist.
 *
 */
GList* u_glist_clear(GList* list){

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

/*! \brief Detect item in single linked list
 *
 * Look for \a str in the \a list.
 *
 * \param list pointer to the STRING_LIST struct
 * \param str  string to search for
 *
 * \returns 0 if absent, 1 if present
 */
int u_gslist_find_string(GSList *list, char *str) {

  int len;
  int index = -1;
  char *ptr;

  /* return -1 if list is empty  */
  len = g_slist_length(list);
  if ( len != 0 ) {

    for (index = 0; index < len; index++) {

      ptr = g_slist_nth_data(list, index);
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

/*! \brief Free a GSlist of Strings
 *  \par Function Description
 *  This function will free all strings in a gslist.
 *
 */
void u_gslist_free_strings(void *data)
{
  GSList *iter;

  for (iter = data; iter != NULL; iter = g_slist_next (iter)) {
    GEDA_FREE (iter->data);
  }
  g_slist_free (data);
}

/*! \brief Free a GSlist completely
 *  \par Function Description
 *  This function will free all of the data in a gslist.
 *
 */
GSList* u_gslist_clear(GSList* list){

  if (list != NULL ) {

    g_slist_foreach(list, (GFunc)g_free, NULL);
    lambda (const char* data)
    {
      list = g_slist_remove( list, data);
      return FALSE;
    }
    mapcar (list);

    g_slist_free(list);
    list = NULL;
  }
  return list;
}