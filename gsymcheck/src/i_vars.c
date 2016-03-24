/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_vars.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

#include <libgeda/libgeda.h>

#include "../include/struct.h"

GList *default_valid_attributes = NULL;

/*! \brief Release Resources in i_vars
 *  \par Function Description
 *
 */
void i_vars_release_all(void)
{
  geda_glist_free_all(default_valid_attributes);
  default_valid_attributes = NULL;
}

/*! \brief Assign the List of Valid Attribute in the working structure
 *  \par Function Description
 *  Sets the valid_attributes pointer in \a s_current to either
 *  the GList pointed to by default_valid_attributes or a glist
 *  created using the strings in the internal standard_attributes
 *  structure. If the RC file is processed as expected this function
 *  just set a pointer to a pointer. If the internal strings are
 *  used then default_valid_attributes will also be set to the same
 *  GList so that which ever list is used, the list will be freed
 *  in i_vars_release_all.
 *
 *  \param [in] s_current Pointer to new SYMCHECK structure
 */
void i_vars_set_valid_attributes(SYMCHECK *s_current)
{
  if (default_valid_attributes) {
    s_current->valid_attributes = default_valid_attributes;
  }
  else {

    /* Fall back to hard-coded valid attribute list */

    GList *attributes_list = NULL;

    char *standard_attributes[] = {
                                   "device",
                                   "footprint",
                                   "numslots",
                                   "refdes",
                                   "author",
                                   "description",
                                   "documentation",
                                   "symversion",
                                   "dist-license",
                                   "use-license",
                                   "graphical",
                                   "net",
                                   "netname",
                                   "slot",
                                   "slotdef",
                                   "value",
                                   "comment",
                                   "footprints",
                                   "model-name",
                                   "file",
                                   "pins",
                                   "spice-type",
                                   NULL};

    int i;

    for (i = 0; standard_attributes[i] != NULL; i++) {
      char *string = geda_strdup(standard_attributes[i]);
      attributes_list = g_list_append(attributes_list, string);
    }

    /* write pointer to working structure */
    s_current->valid_attributes = attributes_list;

    /* And saved to default to be release at exit */
    default_valid_attributes = attributes_list;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_vars_set(GedaToplevel *pr_current)
{
  i_vars_libgeda_set(pr_current);

}
