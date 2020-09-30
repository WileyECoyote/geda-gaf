/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */
#include "../../../config.h"

#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda_priv.h>

#include <geda_debug.h>

#define MAX_ATTRIBS	128

/*! \brief */
struct st_attrib_names {
  char *attrib_name;
};

/*! \brief */
static int attrib_index=0;

/*! \brief */
/* and eventually make this unlimited */
/* hack hack */
static struct st_attrib_names attrib[MAX_ATTRIBS];

/*!
 * \brief Add a Name to the List of Library Attributes Names
 * \par Function Description
 *  Adds \a new_attrib to the attrib st_attrib_names structure.
 */
int geda_struct_attrib_add_entry(char *new_attrib)
{
  if (new_attrib == NULL || *new_attrib == 0) {
    return(-1);
  }

  if (attrib_index >= MAX_ATTRIBS) {
    return(-1);
  }

  attrib[attrib_index].attrib_name = geda_utility_string_strdup (new_attrib);

  attrib_index++;
  return(attrib_index);
}

/*!
 * \brief Clears the Library List of Attributes Names
 * \par Function Description
 *  Allows application to clears the current attribute list
 *  so that new list can be created.
 */
void geda_struct_attrib_clear()
{
  geda_struct_attrib_free();
}

/*!
 * \brief Return count of Attributes
 * \par Function Description
 *  Return integer count of the number of stored attribute
 *  strings.
 */
int geda_struct_attrib_count( void )
{
  return(attrib_index);
}

/*!
 * \brief Release Resourcs associated with List of Attributes
 * \par Function Description
 *  Releases the resources with the string in the attribute list.
 */
void geda_struct_attrib_free()
{
  int i;

  for (i = 0; i < attrib_index; i++) {
     GEDA_FREE(attrib[i].attrib_name);
  }

  attrib_index=0;
}

/*!
 * \brief Print the List of Library Attributes Names
 * \par Function Description
 *  Outputs the list attribute names to standard out.
 */
void geda_struct_attrib_print()
{
  int i;

  for (i = 0; i < attrib_index; i++) {
    printf("%s\n", attrib[i].attrib_name);
  }
}

/*!
 * \brief Check if Name exist in the List of Attributes Names
 * \par Function Description
 * \retval true for unique, zero for duplication
 */
int geda_struct_attrib_uniq(char *name)
{
  int i;

  for (i = 0; i < attrib_index; i++) {
    if (strcmp(attrib[i].attrib_name, name) == 0) {
      return(0);
    }
  }

  return(1);
}

/*!
 * \brief Initialize the Library List of Attributes Names
 * \par Function Description
 *  Initialize the attrib st_attrib_names structure wtih NULLs.
 */
void geda_struct_attrib_init()
{
  int i;
  for (i = 0; i < MAX_ATTRIBS; i++) {
    attrib[i].attrib_name = NULL;
  }
  attrib_index=0;
}

/*!
 * \brief Get and Entry from List of Attributes Names given an Index
 * \par Function Description
 * \returns the attribute name at the given index.
 */
char *geda_struct_attrib_get(int index)
{
  if (index < attrib_index) {
    return(attrib[index].attrib_name);
  }

  return(NULL);
}
