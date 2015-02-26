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
  PAN,              /* 4 */
  STARTSELECT,      /* 5 */
  STARTDESELECT,    /* 6 */
  DRAGMOVE,         /* 7 */
  COPYMODE,         /* 8 */
  COMPMODE,         /* 9 */
  ZOOMBOX,          /* 10 */
  NETMODE,          /* 11 */
  PINMODE,          /* 12 */
  LINEMODE,         /* 13 */
  BOXMODE,          /* 14 */
  CIRCLEMODE,       /* 15 */
  TEXTMODE,         /* 16 */
  ARCMODE,          /* 17 */
  PATHMODE,         /* 18 */
  PICTUREMODE,      /* 19 */
  BUSMODE,          /* 10 */
  MCOPYMODE,        /* 21 */
  MOVEMODE,         /* 22 */
  ENDDNDSTR,        /* 23 */
  ENDDND_MOVE_OBJ,  /* 24 */
  ENDDND_COPY_OBJ,  /* 25 */
  ENDROTATE,        /* 26 */
  ENDMIRROR,        /* 27 */
  STARTDND,         /* 28 */
  STARTPASTE,       /* 29 */
  ENDPASTE,         /* 00 */
  GRIPS,            /* 30 */
  STARTEXTEND,      /* 31 */
  EXTEND,           /* 32 */
  ENDEXTEND,        /* 33 */
  STARTBREAK,       /* 34 */
  ENDBREAK,         /* 35 */
};

#endif
