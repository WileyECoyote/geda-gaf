/* -*- x_dnd.h -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2014 Ales Hvezda
 * Copyright (C) 2013-2014 Wiley Edward Hill
 *
 * Copyright (C) 2013-2014 gEDA Contributors (see ChangeLog for details)
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
 *  Date: September, 27, 2013
 *  Contributing Author: Wiley Edward Hill
 */

#ifndef H_GSCHEM_DRAGnDROP_H
#define H_GSCHEM_DRAGnDROP_H

#define DND_FILE_LEADER "file:\x2F\x2f\0"

#define DND_NIL "nil"

enum {
      DND_TARGET_NONE,
      DND_TARGET_TEXT,
      DND_TARGET_STRING,
      DND_TARGET_PLAIN_TEXT,
      DND_TARGET_UTF8_STRING,
      DND_TARGET_OBJECTS,
};

typedef enum { DROPPED_ON_CANVAS, DROPPED_ON_COORD} DropLocation;

#define GSCHEM_TARGET_NONE \
        { "", 0,  DND_TARGET_NONE }

#define GSCHEM_TARGET_TEXT \
        { "TEXT", 0,  DND_TARGET_TEXT }

#define GSCHEM_TARGET_STRING \
        { "STRING", 0, DND_TARGET_STRING }

#define GSCHEM_TARGET_TEXT_PLAIN \
        { "text/plain", 0, DND_TARGET_PLAIN_TEXT }

#define GSCHEM_TARGET_UTF8_STRING \
        { "UTF8_STRING", 0, DND_TARGET_UTF8_STRING }

#define GSCHEM_TARGET_OBJECTS \
        { GEDA_TYPE_OBJECTS, 0, DND_TARGET_OBJECTS }


#endif /* H_GSCHEM_DRAGnDROP */
