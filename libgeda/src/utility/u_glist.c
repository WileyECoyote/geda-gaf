/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2014-2016 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2014-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: August, 27, 2014
 * Contributing Author: Wiley Edward Hill
 */

#include "../../../config.h"
#include <libgeda_priv.h>

#include <geda_debug.h>

/*! U0201
 * \brief Free a Glist completely
 * \par Function Description
 *  This function will free all of the data in a glist but
 *  does not free the glist.
 *
 */
GList *geda_utility_glist_clear(GList *list)
{
  if (list != NULL ) {

    lambda (char *data) {
      list = g_list_remove (list, data);
      g_free(data);
      return FALSE; /* return from lambda */
    }
    foreach (list);
  }
  return list;
}

/*! U0202
 * \brief Detect item in double linked list
 * \par Function Description
 * Look for str item in the list.
 *
 * \param list pointer to the double linked list of strings
 * \param str  string to search for
 *
 * \returns index where first occurence of the string was found
 *          -1 if not found or
 *          -2 if either if the argument is NULL
 */
int geda_utility_glist_find_string(GList *list, const char *str)
{
  int index = -2;

  if ((list != NULL) && (str != NULL)) {

    GList *iter;
    int found = index = 0;

    for (iter = list; iter != NULL; iter = iter->next) {

      if (iter->data && geda_utility_string_strequal(iter->data, str)) {
        found = 1;
        break;
      }
      index++;
    }

    if (!found)
      index = -1;     /* item was not in the list, so return -1 */
  }

  return index;
}

/*!
 * \brief Free a Glist of Pointers
 * \par Function Description
 *  This function will free all data referenced by pointers in a glist
 *  and \a list.
 */
void geda_utility_glist_free_all(void *list)
{
  GList *iter;

  for (iter = list; iter != NULL; iter = iter->next) {
    if (iter->data)
      g_free(iter->data);
  }
  g_list_free (list);
}

/*!
 * \brief Free a Glist Full
 * \par Function Description
 *  This function provides the same functionality as g_list_free_full
 *  which is not avaliable until glib 2.28.
 */
void geda_utility_glist_free_full (GList *list, GDestroyNotify free_func)
{
  g_list_foreach (list, (GFunc) free_func, NULL);
  g_list_free (list);
}

/*!
 * \brief Compare strings in Glist to string.
 * \par Function Description
 *  Returns TRUE if the string is an element of the GLIST. Provides
 *  functionality similar to geda_utility_glist_find_string except
 *  that geda_utility_glist_find_string returns the index, which
 *  could be index zero.
 *
 * \param [in] list   GList containing strings to be search
 * \param [in] string pointer the string to search for
 *
 * \retval TRUE if string is in the GLIST data, otherwise FALSE.
 *
 * \sa geda_utility_glist_find_string
 */
bool geda_utility_glist_str_inlist(GList *list, const char *string)
{
  bool   answer = FALSE;
  GList *iter;

  for (iter = list; iter; iter = iter->next){

    if (geda_utility_string_strequal(iter->data, string)) {
      answer = TRUE;
      break;
    }
  }
  return answer;
}

/*!
 * \brief Compare strings in Glist to string, ignoring case
 * \par Function Description
 *  Returns TRUE if there is an equivalent string element in
 *  the GLIST.
 *
 * \param [in] list   A GList containing strings to be search
 * \param [in] string pointer the string to search for
 *
 * \retval TRUE if equivalent string is found in GLIST,
 *              otherwise FALSE.
 */
bool geda_utility_glist_stri_inlist(GList *list, const char *string)
{
  bool answer = FALSE;

  if (string) {

    GList *iter;

    for (iter = list; iter; iter = iter->next){

      if (!geda_utility_string_stricmp(iter->data, string)) {
        answer = TRUE;
        break;
      }
    }
  }

  return answer;
}

/*------------------------ gslist utilities ----------------------*/

/*!
 * \brief Free a GSlist completely
 * \par Function Description
 *  This function will free all of the data in a gslist but
 *  does not free \a list.
 */
GSList *geda_utility_gslist_clear(GSList *list)
{
  if (list != NULL ) {

    g_slist_foreach(list, (GFunc)g_free, NULL);

    lambda (const char* data) {
      list = g_slist_remove( list, data);
      return FALSE;
    }
    mapcar (list);
  }
  return list;
}

/*!
 * \brief Detect item in single linked list
 * \par Function Description
 *  Look for \a str in the \a list.
 *
 * \param list pointer to the STRING_LIST struct
 * \param str  string to search for
 *
 * \returns 0 if absent, 1 if present
 */
int geda_utility_gslist_find_string(GSList *list, const char *str)
{
  if ((list == NULL) || (str == NULL))
    return -2;

  int len;
  int index = -1;

  /* return -1 if list is empty  */
  len = g_slist_length(list);

  if (len != 0) {

    for (index = 0; index < len; index++) {

      char *ptr = g_slist_nth_data(list, index);

      if (ptr == NULL) {
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
    index = -1;     /* item was not in the list, so return -1 */

  return index;
}

/*!
 * \brief Free a GSlist of Pointers
 * \par Function Description
 *  This function will free data referenced by each pointer in a gslist
 *  and the gslist.
 */
void geda_utility_gslist_free_all(void *list)
{
  GSList *iter;

  for (iter = list; iter != NULL; iter = iter->next) {
    if (iter->data)
      g_free(iter->data);
  }
  g_slist_free (list);
}

/*!
 * \brief Free a GSlist Full
 * \par Function Description
 *  This function provides the same functionality as g_slist_free_full
 *  which is not avaliable until glib 2.28.
 */
void geda_utility_gslist_free_full (GSList *list, GDestroyNotify free_func)
{
  g_slist_foreach (list, (GFunc)free_func, NULL);
  g_slist_free (list);
}

/*!
 * \brief Is strings in GSlist.
 * \par Function Description
 *  Returns TRUE if the string is an element of the GLIST. Provides
 *  functionality similar to geda_utility_glist_find_string except
 *  that geda_utility_glist_find_string returns the index, which
 *  could be index zero.
 *
 * \param [in] list*  GSList containing strings to be search
 * \param [in] string pointer the string to search for
 *
 * \retval TRUE if string is in the GSLIST data, otherwise FALSE.
 *
 * \sa geda_utility_gslist_find_string
 */
bool geda_utility_gslist_str_inlist(GSList *list, const char *string)
{
  bool    answer = FALSE;
  GSList *iter;

  for (iter = list; iter; iter = iter->next){

    if (geda_utility_string_strequal(iter->data, string)) {
      answer = TRUE;
      break;
    }
  }
  return answer;
}

/*!
 * \brief Compare strings in GSlist to string, ignoring case
 * \par Function Description
 *  Returns TRUE if there is an equivalent string element in
 *  the GSLIST.
 *
 * \param [in] list   A GSList containing strings to be search
 * \param [in] string pointer the string to search for
 *
 * \retval TRUE if equivalent string is found in \a list data,
 *              otherwise FALSE.
 */
bool geda_utility_gslist_stri_inlist(GSList *list, const char *string)
{
  bool    answer = FALSE;
  GSList *iter;

  for (iter = list; iter; iter = iter->next){

    if (!geda_utility_string_stricmp(iter->data, string)) {
      answer = TRUE;
      break;
    }
  }
  return answer;
}
