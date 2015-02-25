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
  COPY,             /* 5 */
  PAN,              /* 6 */
  STARTSELECT,      /* 7 */
  STARTDESELECT,    /* 8 */
  STARTMOVE,        /* 9 */
  STARTCOPY,        /* 10 */
  STARTDND,         /* 11 */
  DRAWCOMP,         /* 12 */
  NETMODE,          /* 13 */
  PINMODE,          /* 14 */
  LINEMODE,         /* 15 */
  BOXMODE,          /* 16 */
  CIRCLEMODE,       /* 17 */
  TEXTMODE,         /* 18 */
  ARCMODE,          /* 19 */
  PATHMODE,         /* 20 */
  PICTUREMODE,      /* 21 */
  BUSMODE,          /* 22 */
  ENDCOMP,          /* 23 */
  ENDCOPY,          /* 24 */
  ENDMOVE,          /* 25 */
  ENDDNDSTR,        /* 26 */
  ENDDND_MOVE_OBJ,  /* 27 */
  ENDDND_COPY_OBJ,  /* 28 */
  ENDROTATE,        /* 29 */
  ENDMIRROR,        /* 30 */
  ZOOMBOX,          /* 31 */
  STARTPASTE,       /* 32 */
  ENDPASTE,         /* 33 */
  GRIPS,            /* 34 */
  STARTEXTEND,      /* 35 */
  EXTEND,           /* 36 */
  ENDEXTEND,        /* 37 */
  STARTBREAK,       /* 38 */
  ENDBREAK,         /* 39 */
  MCOPY,            /* 40 */
  STARTMCOPY,       /* 41 */
  ENDMCOPY,         /* 42 */
};

#endif
