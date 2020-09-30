/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_struct.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#ifndef STRUCT_STRUCT_H
#define STRUCT_STRUCT_H

/* Structures for grapical shapes and objects */
typedef struct st_bezier        BEZIER;
typedef struct st_box           RECTANGLE;
typedef struct st_object_color  COLOR;
typedef struct st_fill_options  FILL_OPTIONS;
typedef struct st_line          LINE;
typedef struct st_line_options  LINE_OPTIONS;
typedef struct st_point         GedaPoint;
typedef struct st_path_section  PATH_SECTION;

/* Structures for user interface objects */
typedef struct st_color         edaColor;
typedef struct st_color_element ColorElement;
typedef struct st_menu_item     MENU;

/* Structures for mathmatical stuff */
typedef struct st_bounds        BOUNDS;
typedef struct st_transform     TRANSFORM;

/* Non-categorized structures */
typedef struct st_bus_ripper    BUS_RIPPER;

typedef struct st_TextBuffer    TextBuffer;     /* For Managed text buffers */

/** ********** structures for grapical shapes and objects ***********/

/*------------------------------------------------------------------
 *                             ARC
 *------------------------------------------------------------------*/
struct st_arc {
  int x, y; /* world */

  int width;
  int height;

  int start_angle;
  int arc_sweep;
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
 *                            CIRCLE
 *------------------------------------------------------------------*/
struct st_circle {
  /* shouldn't these just be x & y like the rest of the world? */
  int center_x, center_y; /* world */
  int radius;
};

#define CIRCLE_CENTER 0
#define CIRCLE_RADIUS 1

/*------------------------------------------------------------------
 *                         HATCH_OPTIONS
 *------------------------------------------------------------------*/
struct st_fill_options {
  OBJECT_FILLING fill_type;
  int            fill_width;
  int            fill_angle1;
  int            fill_pitch1;
  int            fill_angle2;
  int            fill_pitch2;
};

/*------------------------------------------------------------------
 *                            LINE
 *------------------------------------------------------------------*/
struct st_line {
  int x[2];
  int y[2];
};

#define LINE_END1 0
#define LINE_END2 1

/*------------------------------------------------------------------
 *                         LINE_OPTIONS
 *------------------------------------------------------------------*/
struct st_line_options {
  LINE_END  line_end;
  LINE_TYPE line_type;
  int       line_width;
  int       line_space;
  int       line_length;
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
  int   length;
  int   size;
  int   alignment;
  int   angle;
};

/** ************ Structures for user interface objects **************/

/*------------------------------------------------------------------
 *                            MENU
 *------------------------------------------------------------------*/
/*! \brief */
struct st_menu_item {
  char *menu_name;
  char *menu_action;
  char *menu_icon;
  char *menu_tooltip;
};

/*------------------------------------------------------------------
 *                         RC FILE ENTRY
 *------------------------------------------------------------------*/
/* used by the rc loading mechanisms */
typedef struct {
  int   m_val;
  char *m_str;
} vstbl_entry;


/* -- Miscellaneous Structures without defines here -- */

/** ************ Structures for mathmatical stuff *******************/

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
 *                           TRANSFORM
 *------------------------------------------------------------------*/
/*! \brief  A structure to store a 2D affine transform.
 *  \par   The transforms get stored in a 3x3 matrix. Code assumes the
 *         bottom row to remain constant at [0 0 1].
 */
struct st_transform {
  double m[2][3];         /* m[row][column] */
};

/** ****************** Non Categorized Structures *******************/

/*------------------------------------------------------------------
 *                          BUS_RIPPER
 *------------------------------------------------------------------*/
/*! \remarks
 *   This structure is used in gschem to add rippers when drawing
 *   nets, it is never stored in any object, it is only temporary.
 */
struct st_bus_ripper
{
  int x[2];
  int y[2];
};

/*------------------------------------------------------------------
 *                            COLOR
 *------------------------------------------------------------------*/
struct st_color {
    double r;
    double g;
    double b;
    double a;
};

struct st_object_color {
  unsigned char r, g, b, a;
  bool enabled;
};

/* Color element structure */
struct st_color_element {
  unsigned char  r, g, b;
  const char *name;
};

/*------------------------------------------------------------------
 *                          TextBuffer
 *------------------------------------------------------------------*/
struct st_TextBuffer
{
  const char   *buffer;
  unsigned int  size;

  char         *line;
  unsigned int  linesize;

  unsigned int  offset;
  unsigned int  line_count; /* line counter for error reporting */
};
#define TEXT_BUFFER_LINE_SIZE 1024

#endif /* STRUCT_STRUCT_H */
