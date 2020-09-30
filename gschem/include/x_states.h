/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_states.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */
/*!
 * \file x_states.h
 *
 * \brief Enumerates States
 */

#ifndef X_STATES_H
#define X_STATES_H

#undef NONE

/* NOTE: when adding states, also update i_status_string() function */

enum x_states {
  NONE,             /* 0 */
  DESELECT,         /* 1 */
  STARTDESELECT,    /* 2 */
  SELECT,           /* 3 */
  STARTSELECT,      /* 4 */
  GRIPS,            /* 5 */
  SBOX,             /* 6 */
  PAN,              /* 7 */
  DRAGMOVE,         /* 8 */
  COPYMODE,         /* 9 */
  COMPMODE,         /* 10 */
  ZOOMBOX,          /* 11 */
  NETMODE,          /* 12 */
  PINMODE,          /* 13 */
  LINEMODE,         /* 14 */
  BOXMODE,          /* 15 */
  CIRCLEMODE,       /* 16 */
  TEXTMODE,         /* 17 */
  ARCMODE,          /* 18 */
  PATHMODE,         /* 19 */
  PICTUREMODE,      /* 20 */
  BUSMODE,          /* 21 */
  MCOPYMODE,        /* 22 */
  MOVEMODE,         /* 23 */
  ENDDNDSTR,        /* 24 */
  ENDDND_MOVE_OBJ,  /* 25 */
  ENDDND_COPY_OBJ,  /* 26 */
  ENDROTATE,        /* 27 */
  ENDOFFSET,        /* 28 */
  ENDMIRROR,        /* 29 */
  STARTDND,         /* 30 */
  PASTEMODE,        /* 31 */
  STARTEXTEND,      /* 32 */
  EXTEND,           /* 33 */
  ENDEXTEND,        /* 34 */
  STARTBREAK,       /* 35 */
  ENDBREAK,         /* 36 */
};

#endif
