/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_vars.c
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
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

#include "../../config.h"

#include <libgeda/libgeda.h>

#include "../include/struct.h"

GList *default_known_devices = NULL;
GList *default_valid_attributes = NULL;

/*!
 * \brief Release Resources in i_vars
 * \par Function Description
 *  Releases resources associated with i_vars, specifically, release
 *  lists and containing string allocations.
 */
void i_vars_release_all(void)
{
  geda_glist_free_all(default_known_devices);
  default_known_devices = NULL;

  geda_glist_free_all(default_valid_attributes);
  default_valid_attributes = NULL;
}

/*! \brief Assign the List of Valid Attribute in the working structure
 *  \par Function Description
 *  Sets the known_device pointer in \a s_current to either the GList
 *  pointed to by default_known_devices or a glist created using the
 *  strings in the internal known_devices structure. If the RC file
 *  is processed as expected this function just set a pointer to a
 *  pointer. If the internal strings are used then default_known_devices
 *  will also be set to the same GList so that which ever list is used,
 *  the list will be freed in i_vars_release_all.
 *
 *  \param [in] s_current Pointer to new SYMCHECK structure
 */
void i_vars_set_known_devices(SYMCHECK *s_current)
{
  if (default_known_devices) {
    s_current->known_devices = default_known_devices;
  }
  else {

    /* Fall back to hard-coded valid attribute list */

    GList *known_device_list = NULL;

    char *known_devices[] = { "none",
                              "RESISTOR",
                              "CAPACITOR",
                              "POLARIZED_CAPACITOR",
                              "COIL",
                              "INDUCTOR",
                              "DIODE",
                              "PMOS_TRANSISTOR",
                              "NMOS_TRANSISTOR",
                              "PNP_TRANSISTOR",
                              "NPN_TRANSISTOR",
                              "PFET_TRANSISTOR",
                              "NFET_TRANSISTOR",
                              "MESFET_TRANSISTOR",
                              "TESTPOINT",
                              "VOLTAGE_SOURCE",
                              "CURRENT_SOURCE",
                              "ZENER",
                              NULL};

    int i;

    for (i = 0; known_devices[i] != NULL; i++) {
      char *string = geda_strdup(known_devices[i]);
      known_device_list = g_list_append(known_device_list, string);
    }

    /* write pointer to working structure */
    s_current->known_devices = known_device_list;

    /* And saved to default to be release at exit */
    default_known_devices = known_device_list;
  }
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
                                   "manufacturer",
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

/*!
 * \brief Set gsymcheck Top Level Variables
 * \par Function Description
 *  Calls libgeda::geda_iface_vars_set to initialize the \a pr_current
 *  members. gsymcheck does not use an application top-level like gschem,
 *  per-se, but does use a SYMCHECK record which is initialized on a per
 *  symbol basis by s_symstruct_init, to record and track statistics.
 *
 * \sa i_vars_set_known_devices i_vars_set_valid_attributes
 */
void i_vars_set(GedaToplevel *pr_current)
{
  geda_iface_vars_set(pr_current);

  geda_toplevel_set_file_open_flags(pr_current, F_OPEN_NONE);
}
