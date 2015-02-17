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
  ZOOM,             /* 6 */
  PAN,              /* 7 */
  STARTSELECT,      /* 8 */
  STARTDESELECT,    /* 9 */
  STARTMOVE,        /* 10 */
  STARTCOPY,        /* 11 */
  STARTDND,         /* 12 */
  DRAWCOMP,         /* 13 */
  NETMODE,          /* 14 */
  PINMODE,          /* 15 */
  LINEMODE,         /* 16 */
  BOXMODE,          /* 17 */
  CIRCLEMODE,       /* 18 */
  ARCMODE,          /* 19 */
  PATHMODE,         /* 21 */
  PICTUREMODE,      /* 22 */
  BUSMODE,          /* 20 */
  ENDCOMP,          /* 23 */
  ENDCOPY,          /* 24 */
  ENDMOVE,          /* 25 */
  ENDDNDSTR,        /* 26 */
  ENDDND_MOVE_OBJ,  /* 27 */
  ENDDND_COPY_OBJ,  /* 28 */
  DRAWTEXT,         /* 29 */
  ENDTEXT,          /* 30 */
  ENDROTATE,        /* 31 */
  ENDMIRROR,        /* 32 */
  ZOOMBOXSTART,     /* 33 */
  ZOOMBOXEND,       /* 34 */
  STARTPASTE,       /* 35 */
  ENDPASTE,         /* 36 */
  GRIPS,            /* 37 */
  STARTEXTEND,      /* 38 */
  EXTEND,           /* 39 */
  ENDEXTEND,        /* 40 */
  STARTBREAK,       /* 41 */
  ENDBREAK,         /* 42 */
  MCOPY,            /* 43 */
  STARTMCOPY,       /* 44 */
  ENDMCOPY,         /* 45 */
};

#endif
