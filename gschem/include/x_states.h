/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2015 Ales Hvezda
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
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef X_STATES_H
#define X_STATES_H

#undef NONE

/* NOTE: when adding states, also update i_status_string() function */

enum x_states {
  NONE,             /* 0 */
  SELECT,           /* 1 */
  DESELECT,         /* 2 */
  SBOX,             /* 3 */
  MOVE,             /* 4 */
  PAN,              /* 5 */
  STARTSELECT,      /* 6 */
  STARTDESELECT,    /* 7 */
  STARTMOVE,        /* 8 */
  COPYMODE,         /* 9 */
  STARTDND,         /* 10 */
  COMPMODE,         /* 11 */
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
  ENDMOVE,          /* 23 */
  ENDDNDSTR,        /* 24 */
  ENDDND_MOVE_OBJ,  /* 25 */
  ENDDND_COPY_OBJ,  /* 26 */
  ENDROTATE,        /* 27 */
  ENDMIRROR,        /* 28 */
  ZOOMBOX,          /* 29 */
  STARTPASTE,       /* 30 */
  ENDPASTE,         /* 31 */
  GRIPS,            /* 32 */
  STARTEXTEND,      /* 33 */
  EXTEND,           /* 34 */
  ENDEXTEND,        /* 35 */
  STARTBREAK,       /* 36 */
  ENDBREAK,         /* 37 */
};

#endif
