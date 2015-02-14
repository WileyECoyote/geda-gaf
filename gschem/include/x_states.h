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
  STARTDRAWNET,     /* 13 */
  DRAWNET,          /* 14 */
  NETCONT,          /* 15 */
  DRAWCOMP,         /* 16 */
  DRAWLINE,         /* 17 */
  DRAWPIN,          /* 18 */
  DRAWARC,          /* 19 */
  DRAWBOX,          /* 20 */
  DRAWCIRCLE,       /* 21 */
  ENDCOMP,          /* 22 */
  ENDCOPY,          /* 23 */
  ENDMOVE,          /* 24 */
  ENDLINE,          /* 25 */
  ENDARC,           /* 26 */
  ENDBOX,           /* 27 */
  ENDCIRCLE,        /* 28 */
  ENDPIN,           /* 29 */
  ENDDNDSTR,        /* 30 */
  ENDDND_MOVE_OBJ,  /* 31 */
  ENDDND_COPY_OBJ,  /* 32 */
  DRAWTEXT,         /* 33 */
  ENDTEXT,          /* 34 */
  ENDROTATE,        /* 35 */
  ENDMIRROR,        /* 36 */
  ZOOMBOXSTART,     /* 37 */
  ZOOMBOXEND,       /* 38 */
  DRAWBUS,          /* 39 */
  BUSCONT,          /* 40 */
  STARTDRAWBUS,     /* 41 */
  STARTPASTE,       /* 42 */
  ENDPASTE,         /* 43 */
  GRIPS,            /* 44 */
  STARTEXTEND,      /* 45 */
  EXTEND,           /* 46 */
  ENDEXTEND,        /* 47 */
  MCOPY,            /* 48 */
  STARTMCOPY,       /* 49 */
  ENDMCOPY,         /* 50 */
  DRAWPATH,         /* 51 */
  PATHCONT,         /* 52 */
  ENDPATH,          /* 53 */
  DRAWPICTURE,      /* 54 */
  ENDPICTURE,       /* 55 */
};

#endif
