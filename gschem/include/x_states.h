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
  STARTPAN,         /* 13 */
  STARTDRAWNET,     /* 14 */
  DRAWNET,          /* 15 */
  NETCONT,          /* 16 */
  DRAWCOMP,         /* 17 */
  DRAWLINE,         /* 18 */
  DRAWPIN,          /* 19 */
  DRAWARC,          /* 20 */
  DRAWBOX,          /* 21 */
  DRAWCIRCLE,       /* 22 */
  ENDCOMP,          /* 23 */
  ENDCOPY,          /* 24 */
  ENDMOVE,          /* 25 */
  ENDLINE,          /* 26 */
  ENDARC,           /* 27 */
  ENDBOX,           /* 28 */
  ENDCIRCLE,        /* 29 */
  ENDPIN,           /* 30 */
  ENDDNDSTR,        /* 31 */
  ENDDND_MOVE_OBJ,  /* 32 */
  ENDDND_COPY_OBJ,  /* 33 */
  DRAWTEXT,         /* 34 */
  ENDTEXT,          /* 35 */
  ENDROTATE,        /* 36 */
  ENDMIRROR,        /* 37 */
  ZOOMBOXSTART,     /* 38 */
  ZOOMBOXEND,       /* 39 */
  DRAWBUS,          /* 40 */
  BUSCONT,          /* 41 */
  STARTDRAWBUS,     /* 42 */
  STARTPASTE,       /* 43 */
  ENDPASTE,         /* 44 */
  GRIPS,            /* 45 */
  STARTEXTEND,      /* 46 */
  EXTEND,           /* 47 */
  ENDEXTEND,        /* 48 */
  MCOPY,            /* 49 */
  STARTMCOPY,       /* 50 */
  ENDMCOPY,         /* 51 */
  DRAWPATH,         /* 52 */
  PATHCONT,         /* 53 */
  ENDPATH,          /* 54 */
  DRAWPICTURE,      /* 55 */
  ENDPICTURE,       /* 56 */
};

#endif
