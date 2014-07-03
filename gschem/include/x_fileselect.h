/* -*- x_fileselect-h -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
 *
 *  Date: October, 18, 2013
 *  Contributing Author: Wiley Edward Hill
 */

typedef  enum {
  FILTER_NONE,
  FILTER_SCHEMATIC,
  FILTER_SYMBOL,
  FILTER_BOTH
} IDE_FILTER;

typedef struct _filter_reg_t GschemFileFilterDataDef;

struct _filter_reg_t {
  IDE_FILTER id;
  const char *name;
  const char *const *pattern;

};

#define GSCHEM_FILTER_SCHEMATIC  \
  { FILTER_SCHEMATIC,  "Schematics", (const char* const []) \
  {SCHEMATIC_FILTER, NULL}, }

#define GSCHEM_FILTER_SYMBOL     \
  { FILTER_SYMBOL,     "Symbols",    (const char* const []) \
  { SYMBOL_FILTER, NULL} }

#define GSCHEM_FILTER_BOTH       \
  { FILTER_BOTH,      "Schematics and symbols", (const char * const []) \
  { SCHEMATIC_FILTER, SYMBOL_FILTER, NULL} }

#define GSCHEM_FILTER_NONE       \
  { FILTER_NONE,      "All files", (const char * const []) \
  { "*", NULL} }

#define GSCHEM_NO_MORE_FILTERS   \
  { 0,  NULL, NULL }

