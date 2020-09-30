/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: struct.h
 *
 * gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
 *
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 */

/* sym check structures (gsymcheck) */
typedef struct st_symcheck SYMCHECK;

/* gsymcheck structure */
struct st_symcheck {

  GList *known_devices;
  GList *valid_attributes;

  /* unused for now */
  int unattached_attribs;

  /* info / warning / error messages */
  GList *info_messages;
  GList *warning_messages;
  GList *error_messages;

  /* Influential Symbol Properties */
  const char *filename;              /* Not freed, points to page */

  int   graphical_symbol;
  int   has_directive;               /* Set if at least one found */

  /* description= check */
  int   missing_descr_attrib;
  char *description_attribute;
  int   duplicate_descr_attribute;

  /* device= check */
  char *device_attribute;
  int   device_attribute_incorrect;
  int   duplicate_device_attrib;
  int   multiple_device_attrib;

  /* documentation = check */
  int   missing_doc_attrib;          /* Not implemented */
  char *documentation_attribute;     /* Not implemented */
  int   duplicate_doc_attribute;     /* Not implemented */
  int   multiple_doc_attrib;         /* Not implemented */

  /* pinseq= check */
  int missing_pinseq_attrib;
  int multiple_pinseq_attrib;
  int duplicate_pinseq_attrib;

  /* pinnumber= check */
  int missing_pinnumber_attrib;
  int multiple_pinnumber_attrib;
  int duplicate_pinnumber_attrib;

  /* slotting checks */
  int missing_numslots_attrib;
  int slotting_errors;

  /* old pin#=# and slot#=# checks */
  int found_oldpin_attrib;
  int found_oldslot_attrib;

  /* net, bus, connection checks */
  int found_net;
  int found_bus;
  int found_connection;

  /* obsolete attribute checks */
  /* int found_label; */
  /* int found_uref; */

  /* forbidden attributes */
  /* int found_name; */
  /* int found_type; */

  /* misc attributes */
  int found_footprint;
  int found_refdes;

  /* number of pins */
  int numpins;
  /* number of net pins */
  int numnetpins;
  /* number of slots */
  int numslots;
  /* number of distinct slot pins */
  int numslotpins;

  /* total error counter */
  int error_count;

  /* total warning counter */
  int warning_count;

  /* pintype= check */
  int missing_pintype_attrib;
  int multiple_pintype_attrib;
  int duplicate_pintype_attrib;

};


