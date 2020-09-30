/* C
 *  file:o_color.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * Copyright (C) 2011-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02111-1301 USA
*/

#include "../../../config.h"

#include <o_types.h>
#include <geda_colors.h>

#include <geda_debug.h>

/*!
 * \brief Get the Default Color Index for a GedaObject
 * \par Function Description
 *  Returns the default color index for the given enity \a type.
 *  or -1 if type is invalid.
 */
int geda_object_color_get_default (char type)
{
  struct default_color_index_t {
    char type;
    int  index;
  };

  struct default_color_index_t default_color_data[] = {
    COMPLEX_COLOR_INDEX,
    NET_COLOR_INDEX,
    TEXT_COLOR_INDEX,
    PIN_COLOR_INDEX,
    LINE_COLOR_INDEX,
    CIRCLE_COLOR_INDEX,
    BUS_COLOR_INDEX,
    ARC_COLOR_INDEX,
    BOX_COLOR_INDEX,
    PICTURE_COLOR_INDEX,
    PATH_COLOR_INDEX,
    { '\0', -1 }
  };

  struct default_color_index_t *table;

  for (table = default_color_data; table->type != '\0'; table++) {
    if( table->type == type) {
      return table->index;
    }
  }

  return table->index;
}
