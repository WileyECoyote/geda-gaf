/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2012 gEDA Contributors (see ChangeLog for details)
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

/*! \file f_utilities.c
 *  \brief utilility file functions
 */

#include <config.h>
#include <geda.h>
#include <libgeda.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

/*! /comment: The functions in f_basic.c are (mostly) application
 *  specific so this file was created for basic non-application
 *  specific routines.
 */

/*! \brief Return pointer filename extension
 *  \par Function description
 *  Returns a pointer to the characters after the period or
 *  NULL if the is no period in the filename.
 *
 *  \param [in] filename The filename to search.
 *  \return offset if found, otherwise NULL.
 */
const char *get_filename_ext(const char *filename) {
    const char *dot = strrchr(filename, '.');
    if(!dot || dot == filename) return "";
    return dot + 1;
}
/* warning: MUST not be const char */
void remove_ext_from_basename(char *filename) {

  int i = 0;
  int n = 0;
  int len = 0;

  while(filename[len] != '\0') { len++; } /* get length of filename */

  for(i = len; i > -1; i--) {             /* look for extension working backwards */
     if(filename[i] == '.') {
       n = i;                             /* char # of exension */
       break;
    }
  }  
  if (n > 0)
    for(i = n; i < len; i++) {            /* starting with the '.'  */
      filename[i] = '\0';
    }
}






