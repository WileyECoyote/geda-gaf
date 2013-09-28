/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
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
  NONE,           /* 0 */
  SELECT,         /* 1 */
  DESELECT,       /* 2 */
  DRAWLINE,       /* 3 */
  DRAWBOX,        /* 4 */
  MOVE,           /* 5 */
  COPY,           /* 6 */
  DRAWCIRCLE,     /* 7 */
  ZOOM,           /* 8 */
  PAN,            /* 9 */
  DRAWNET,        /* 10 */
  NETCONT,        /* 11 */
  DRAWPIN,        /* 12 */
  DRAWARC,        /* 13 */
  STARTDRAWNET,   /* 14 */
  DRAWCOMP,       /* 15 */
  SBOX,           /* 16 */
  STARTPAN,       /* 17 */
  STARTDESELECT,  /* 18 */
  STARTSELECT,    /* 19 */
  STARTCOPY,      /* 20 */
  STARTMOVE,      /* 21 */
  ENDCOPY,        /* 22 */
  ENDMOVE,        /* 23 */
  ENDLINE,        /* 24 */
  ENDBOX,         /* 25 */
  ENDCIRCLE,      /* 26 */
  ENDARC,         /* 27 */
  ENDPIN,         /* 28 */
  ENDCOMP,        /* 29 */
  DRAWTEXT,       /* 30 */
  ENDTEXT,        /* 31 */
  ENDROTATEP,     /* 32 */
  ENDMIRROR,      /* 33 */
  ZOOMBOXSTART,   /* 34 */
  ZOOMBOXEND,     /* 35 */
  STARTROUTENET,  /* 36 */
  ENDROUTENET,    /* 37 */
  MOUSEPAN,       /* 38 */
  DRAWBUS,        /* 39 */
  BUSCONT,        /* 40 */
  STARTDRAWBUS,   /* 41 */
  STARTPASTE,     /* 42 */
  ENDPASTE,       /* 43 */
  GRIPS,          /* 44 */
  DRAWPICTURE,    /* 45 */
  ENDPICTURE,     /* 46 */
  MCOPY,          /* 47 */
  STARTMCOPY,     /* 48 */
  ENDMCOPY,       /* 49 */
  DRAWPATH,       /* 50 */
  PATHCONT,       /* 51 */
  ENDPATH,        /* 52 */
};

#endif
