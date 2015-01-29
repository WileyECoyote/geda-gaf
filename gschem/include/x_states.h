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
  DRAWLINE,         /* 3 */
  SBOX,             /* 4 */
  DRAWBOX,          /* 5 */
  MOVE,             /* 6 */
  COPY,             /* 7 */
  DRAWCIRCLE,       /* 8 */
  ZOOM,             /* 9 */
  PAN,              /* 10 */
  DRAWNET,          /* 11 */
  NETCONT,          /* 12 */
  STARTPAN,         /* 13 */
  STARTDESELECT,    /* 14 */
  STARTSELECT,      /* 15 */
  STARTMOVE,        /* 16 */
  STARTCOPY,        /* 17 */
  STARTDND,         /* 18 */
  DRAWPIN,          /* 19 */
  DRAWARC,          /* 20 */
  STARTDRAWNET,     /* 21 */
  DRAWCOMP,         /* 22 */
  ENDCOMP,          /* 23 */
  ENDCOPY,          /* 24 */
  ENDMOVE,          /* 25 */
  ENDLINE,          /* 26 */
  ENDBOX,           /* 27 */
  ENDARC,           /* 28 */
  ENDCIRCLE,        /* 29 */
  ENDPIN,           /* 30 */
  ENDDNDSTR,        /* 31 */
  ENDDND_MOVE_OBJ,  /* 32 */
  ENDDND_COPY_OBJ,  /* 33 */
  DRAWTEXT,         /* 34 */
  ENDTEXT,          /* 35 */
  ENDROTATEP,       /* 36 */
  ENDMIRROR,        /* 37 */
  ZOOMBOXSTART,     /* 38 */
  ZOOMBOXEND,       /* 39 */
  STARTROUTENET,    /* 40 */
  ENDROUTENET,      /* 41 */
  MOUSEPAN,         /* 42 */
  DRAWBUS,          /* 43 */
  BUSCONT,          /* 44 */
  STARTDRAWBUS,     /* 45 */
  STARTPASTE,       /* 46 */
  ENDPASTE,         /* 47 */
  GRIPS,            /* 48 */
  STARTEXTEND,      /* 49 */
  EXTEND,           /* 50 */
  ENDEXTEND,        /* 51 */
  DRAWPICTURE,      /* 52 */
  ENDPICTURE,       /* 53 */
  MCOPY,            /* 54 */
  STARTMCOPY,       /* 55 */
  ENDMCOPY,         /* 56 */
  DRAWPATH,         /* 57 */
  PATHCONT,         /* 58 */
  ENDPATH,          /* 59 */
};

#endif
