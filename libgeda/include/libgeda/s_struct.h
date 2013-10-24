/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02111-1301 USA
 */

#ifndef SIMPLE_STRUCT_H
#define SIMPLE_STRUCT_H

/* gschem structures (gschem) */
typedef struct st_arc          ARC;
typedef struct st_bezier       BEZIER;
typedef struct st_bounds       BOUNDS;
typedef struct st_box          BOX;
typedef struct st_bus_ripper   BUS_RIPPER;

typedef struct st_circle       CIRCLE;
typedef struct st_color        COLOR;
typedef struct st_line         LINE;
typedef struct st_point        POINT;        /* intentionally out of order */
typedef struct st_path_section PATH_SECTION;
typedef struct st_path         PATH;
typedef struct st_transform    TRANSFORM;
typedef struct st_TextBuffer   TextBuffer;     /* For Managed text buffers */
typedef struct st_text         TEXT;

/*------------------------------------------------------------------
 *                             ARC
 *------------------------------------------------------------------*/
struct st_arc {
  int x, y; /* world */

  int width;
  int height;

  int start_angle;
  int end_angle;
};

#define ARC_CENTER 0
#define ARC_RADIUS 1
#define ARC_START_ANGLE 2
#define ARC_END_ANGLE 3

/*------------------------------------------------------------------
 *                           BEZIER
 *------------------------------------------------------------------*/
struct st_bezier {
  int x[4];
  int y[4];
};

/*------------------------------------------------------------------
 *                           BOUNDS
 *------------------------------------------------------------------*/
struct st_bounds {
  int min_x;
  int min_y;
  int max_x;
  int max_y;
};

/*------------------------------------------------------------------
 *                             BOX
 *------------------------------------------------------------------*/
struct st_box {
  /* upper is considered the origin */
  int upper_x, upper_y; /* world */     
  int lower_x, lower_y;
};

#define BOX_UPPER_LEFT 0
#define BOX_LOWER_RIGHT 1
#define BOX_UPPER_RIGHT 2
#define BOX_LOWER_LEFT 3

/*------------------------------------------------------------------
 *                          BUS_RIPPER
 *------------------------------------------------------------------*/
/*! \brief This structure is used in gschem to add rippers when drawing
 *          nets it is never stored in any object, it is only temporary.
 */
struct st_bus_ripper
{
  int x[2];
  int y[2];
};

/*------------------------------------------------------------------
 *                            CIRCLE
 *------------------------------------------------------------------*/
struct st_circle {
  /* shouldn't these just x & y like the rest of the world? */
  int center_x, center_y; /* world */
  int radius;
};

#define CIRCLE_CENTER 0
#define CIRCLE_RADIUS 1

/*------------------------------------------------------------------
 *                            COLOR
 *------------------------------------------------------------------*/
struct st_color {
  uint8_t r, g, b, a;
  bool enabled;
};

/*------------------------------------------------------------------
 *                            LINE
 *------------------------------------------------------------------*/
struct st_line {
  int x[2];
  int y[2];
};

/*------------------------------------------------------------------
 *                           POINT
 *------------------------------------------------------------------*/
struct st_point {
  int x;
  int y;
};

/*------------------------------------------------------------------
 *                            PATH
 *------------------------------------------------------------------*/
#define LINE_END1 0
#define LINE_END2 1

typedef enum {
    PATH_MOVETO,
    PATH_MOVETO_OPEN,
    PATH_CURVETO,
    PATH_LINETO,
    PATH_END
} PATH_CODE;

struct st_path_section {
  PATH_CODE code;
  int x1;
  int y1;
  int x2;
  int y2;
  int x3;
  int y3;
};

struct st_path {
  PATH_SECTION *sections; /* Bezier path segments  */
  int num_sections;       /* Number with data      */
  int num_sections_max;   /* Number allocated      */
};

/*------------------------------------------------------------------
 *                            TEXT
 *------------------------------------------------------------------*/
struct st_text {
  int x, y;               /* world origin */

  char *string;           /* text stuff */
  char *disp_string;
  int length;
  int size;
  int alignment;        
  int angle;
};

/*------------------------------------------------------------------
 *                          TextBuffer
 *------------------------------------------------------------------*/
struct st_TextBuffer
{
  const char *buffer;
  unsigned int size;

  char *line;
  unsigned int linesize;

  unsigned int offset;
};
#define TEXT_BUFFER_LINE_SIZE 1024

/*------------------------------------------------------------------
 *                           TRANSFORM
 *------------------------------------------------------------------*/
/*! \brief  A structure to store a 2D affine transform.
 *  \par   The transforms get stored in a 3x3 matrix. Code assumes the
 *         bottom row to remain constant at [0 0 1].
 */
struct st_transform {
  double m[2][3];         /* m[row][column] */
};

/* -- Miscellaneous Structures without defines here -- */
/* used by the rc loading mechanisms */
typedef struct {
  int   m_val;
  char *m_str;
} vstbl_entry;

/* Used by g_rc_parse_handler() */
typedef void (*ConfigParseErrorFunc)(GError **, void *);

#endif
