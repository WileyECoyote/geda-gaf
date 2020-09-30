/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_dnd.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: September, 27, 2013
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */
/*!
 * \file x_dnd.h
 *
 * \brief header for the Drag & Drop module
 */

#ifndef H_GSCHEM_DRAGnDROP_H
#define H_GSCHEM_DRAGnDROP_H

#ifdef OS_WIN32
#define DND_FILE_LEADER "file:\x2F\x2f\x2f\0"
#else
#define DND_FILE_LEADER "file:\x2F\x2f\0"
#endif

#define DND_NIL "nil"

enum {
      DND_TARGET_NONE,
      DND_TARGET_TEXT,
      DND_TARGET_STRING,
      DND_TARGET_PLAIN_TEXT,
      DND_TARGET_URI_LIST,
      DND_TARGET_UTF8_STRING,
      DND_TARGET_OBJECTS,
};

typedef enum {
      DROPPED_ON_CANVAS,
      DROPPED_ON_COORD,
      DROPPED_ON_PAGESEL
} DropLocation;

#define GSCHEM_TARGET_NONE \
        { "", 0,  DND_TARGET_NONE }

#define GSCHEM_TARGET_TEXT \
        { "TEXT", 0,  DND_TARGET_TEXT }

#define GSCHEM_TARGET_STRING \
        { "STRING", 0, DND_TARGET_STRING }

#define GSCHEM_TARGET_TEXT_PLAIN \
        { "text/plain", 0, DND_TARGET_PLAIN_TEXT }

#define GSCHEM_TARGET_URI_LIST \
        { "text/uri-list", 0, DND_TARGET_URI_LIST }

#define GSCHEM_TARGET_UTF8_STRING \
        { "UTF8_STRING", 0, DND_TARGET_UTF8_STRING }

#define GSCHEM_TARGET_OBJECTS \
        { GEDA_TYPE_OBJECTS, 0, DND_TARGET_OBJECTS }


#endif /* H_GSCHEM_DRAGnDROP */
