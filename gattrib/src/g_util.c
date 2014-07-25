/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 *
 * Copyright (C) 2012-2014 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Date: November, 17, 2012
 * Contributing Author: Wiley Edward Hill
 */

#include <gattrib.h>
#include <geda_debug.h>

/*-------------------------- g_list_helper -----------------------*/
/*! \brief Compare strings in Glist to string.
 *
 *  \par Function Description
 *  Returns TRUE if the string is an element of the GLIST.
 *
 *  \param [in] list*  GList containing strings to be search
 *  \param [in] string pointer the string to search for
 *
 *  \retval TRUE if string is in GLIST data, otherwise FALSE.
 */
bool g_list_str_inlist(GList *list, char *string)
{
  bool answer = FALSE;
  char* str;
  int i, len;

  len = g_list_length(list);

  for(i=0; i<len; i++){
    str = (char*) g_list_nth_data(list, i);
    if (strequal( str, string)) {
      answer = TRUE;
      break;
    }
  }
  return answer;
}
/*! \brief Compare strings in Glist to string, ignoring case
 *
 *  \par Function Description
 *  Returns TRUE if there is an equivalent string element in
 *  the GLIST.
 *
 *  \param [in] list   A GList containing strings to be search
 *  \param [in] string pointer the string to search for
 *
 *  \retval TRUE if equivalent string is found in GLIST,
 *               otherwise FALSE.
 */
bool g_list_stri_inlist(GList *list, char *string)
{
  bool answer = FALSE;
  char* str;
  int i, len;

  len = g_list_length(list);

  for(i=0; i<len; i++){
    str = (char*) g_list_nth_data(list, i);
    if (stricmp( str, string)==0) {
      answer = TRUE;
      break;
    }
  }
  return answer;
}









