/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*!
 * \file geda_py_struct.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedathon - gEDA's Python API Extension library
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 17, 2013
 */

#ifndef _GEDA_PY_STRUCT_H
#define _GEDA_PY_STRUCT_H

#pragma once

#include <libgeda/s_struct.h>

typedef struct st_py_page      PageObject;
typedef struct st_py_color     ColorObject;
typedef struct st_py_object    GedaObject;

typedef struct st_py_arc       ArcObject;
typedef struct st_py_box       BoxObject;
typedef struct st_py_bus       BusObject;
typedef struct st_py_circle    CircleObject;
typedef struct st_py_complex   ComplexObject;
typedef struct st_py_line      LineObject;
typedef struct st_py_missing   MissingObject;
typedef struct st_py_net       NetObject;
typedef struct st_py_path      PathObject;
typedef struct st_py_picture   PictureObject;
typedef struct st_py_pin       PinObject;
typedef struct st_py_text      TextObject;

struct st_py_page {
    PyObject_HEAD
    int  pid;
    PyObject *filename;
};

struct st_py_color {
    PyObject_HEAD
    unsigned char r;
    unsigned char g;
    unsigned char b;
    unsigned char a;
};
struct st_py_object {

    PyObject_HEAD

    bool      dirty;
    PyObject *name;

 /* Database management fields, read-only to clients */
    int  type;               /* Object type code */
    int  sid;                /* sequence ID */
    int  pid;                /* page ID */
    int  selected;

 /* Text Attributes of the components */
    PyObject *attributes;

 /* read-writable fields */
    bool auto_attributes;
    int  locked;             /* object selectable flag */
};

struct st_py_arc {

    GedaObject object;

 /* Arc specific data members */
    int x;
    int y;
    int radius;
    int start_angle;
    int arc_sweep;

 /* Generic Graphical Attributes Applicable to Arcs */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */

 /* Hatching */
    int  fill_type;
    int  fill_width;
    int  fill_angle1;
    int  fill_pitch1;
    int  fill_angle2;
    int  fill_pitch2;

 /* Line-Type */
    int  line_end;
    int  line_type;
    int  line_width;
    int  line_space;
    int  line_length;
};

struct st_py_box {

    GedaObject object;

 /* Box specific data members */
    int upper_x;    /* world units */
    int upper_y;
    int lower_x;
    int lower_y;

 /* Generic Graphical Attributes Applicable to Boxes */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */

 /* Hatching */
    int  fill_type;
    int  fill_width;
    int  fill_angle1;
    int  fill_pitch1;
    int  fill_angle2;
    int  fill_pitch2;

 /* Line-Type */
    int  line_end;
    int  line_type;
    int  line_width;
    int  line_space;
    int  line_length;
};

struct st_py_bus {

    GedaObject object;

 /* Bus specific data members */
    bool dirty_name;

    PyObject *bus_name;

    int  x[2];
    int  y[2];

    int  direction;

 /* Generic Graphical Attributes Applicable to Bus */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */

 /* Line-Type */
    int  line_width;
};

struct st_py_circle {

    GedaObject object;

 /* Circle specific data members */
    int center_x;   /* world units */
    int center_y;
    int radius;

 /* Generic Graphical Attributes Applicable to Circles */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */

 /* Hatching */
    int  fill_type;
    int  fill_width;
    int  fill_angle1;
    int  fill_pitch1;
    int  fill_angle2;
    int  fill_pitch2;

 /* Line-Type */
    int  line_end;
    int  line_type;
    int  line_width;
    int  line_space;
    int  line_length;
};

struct st_py_complex {

    GedaObject object;

 /* Complex specific data members */
    bool dirty_name;

    PyObject *filename;
    bool      embedded;      /* is embedded component? */

    int   x;                 /* world origin */
    int   y;
    int   angle;             /* orientation in degrees */
    bool  mirror;

    PyObject *pin_objs;      /* Pin Primitive objects */
    PyObject *prim_objs;     /* Primitive objects */

};

struct st_py_line {

    GedaObject object;

 /* Complex specific data members */
    int x[2];
    int y[2];

 /* Generic Graphical Attributes Applicable to Lines */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */

 /* Line-Type */
    int  line_end;
    int  line_type;
    int  line_width;
    int  line_space;
    int  line_length;
};

struct st_py_missing {

    GedaObject object;

    PyObject *filename;
    int       x;
    int       y;        /* world origin */
    int       angle;    /* orientation in degrees */
    int       mirror;
};

struct st_py_net {

    GedaObject object;

    bool dirty_name;

 /* Database  management fields, read-only to clients */
    int  nid;          /* net ID */

    PyObject *net_name;
    int x[2];
    int y[2];

    int  net_num_connected;
    bool valid_num_connected;

 /* Generic Graphical Attributes Applicable to Nets */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */

 /* Line-Type */
    int  line_width;
};

struct st_py_path {

    GedaObject object;

    bool dirty_string;

    PyObject *path_string;
    PyObject *sections; /* Bezier path segments  */
    int num_sections;       /* Number with data      */
    int num_sections_max;   /* Number allocated      */

 /* Generic Graphical Attributes Applicable to Paths */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */

 /* Hatching */
    int  fill_type;
    int  fill_width;
    int  fill_angle1;
    int  fill_pitch1;
    int  fill_angle2;
    int  fill_pitch2;

 /* Line-Type */
    int  line_end;
    int  line_type;
    int  line_width;
    int  line_space;
    int  line_length;
};

struct st_py_picture {

    GedaObject object;

    PyObject    *filename;
    PyObject    *pixel_buffer;
    unsigned int file_length;
    float        ratio;
    int          angle;
    bool         mirror;
    bool         embedded;

 /* upper is considered the origin */
    int  upper_x;
    int  upper_y;
    int  lower_x;
    int  lower_y;
};

struct st_py_pin {
    //TODO: should have a netname
    GedaObject object;

 /* Pin specific admistrative data members */
    bool dirty_label;
    bool dirty_number;
    bool dirty_electrical;
    bool dirty_mechanical;

 /* Pin specific exposed data members */

 /* read-only fields */
    int       cid;       /* sid of complex to which this pin belongs */
    int       nid;       /* net ID, < 0 then the pin is not connected  */

 /* read-write fields */
    int       sequence;
    int       x[2];
    int       y[2];
    int       whichend;  /* which end gets "connected", either 0 or 1 */

    PIN_ELECT   elect_type;
    PIN_MECH    mech_type;
    PIN_NODE    node_type;      /* either NET or BUS */;

    PyObject *label;
    PyObject *number;
    PyObject *electrical;
    PyObject *mechanical;

 /* Generic Graphical Attributes Applicable to Paths */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */

 /* Line-Type */
    int  line_width;
};

struct st_py_text {

    GedaObject object;

    int cid;            /* sid of complex to which this text is an attribute */

 /* Text specific admistrative */
    bool dirty_text;

 /* Text specific data members */

    PyObject *string;        /* text stuff */
    PyObject *disp_string;

    int  x;
    int  y;                  /* world origin */

    int  size;
    int  alignment;
    int  angle;

    int  show;
    bool visible;

 /* Generic Graphical Attributes Applicable to Text */
    COLOR color;
    COLOR locked_color;  /* Locked color (used to save) */
};
#endif
