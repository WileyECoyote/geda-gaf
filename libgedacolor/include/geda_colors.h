/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_colors.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * MA 02111-1301 USA
 */

/*! \file geda_colors.h
 *  \brief define some constants for the colors
 *  \sa scheme/color-map.scm utility/u_color.c
 */

#ifndef _COLORS_H_INCL
#define _COLORS_H_INCL

#define BACKGROUND_COLOR                0
#define PIN_COLOR                       1
#define NET_ENDPOINT_COLOR              2
#define GRAPHIC_COLOR                   3
#define NET_COLOR                       4
#define ATTRIBUTE_COLOR                 5
#define LOGIC_BUBBLE_COLOR              6
#define DOTS_GRID_COLOR                 7
#define DETACHED_ATTRIBUTE_COLOR        8
#define TEXT_COLOR                      9
#define BUS_COLOR                       10
#define SELECT_COLOR                    11
#define BOUNDINGBOX_COLOR               12
#define ZOOM_BOX_COLOR                  13
#define STROKE_COLOR                    14
#define LOCK_COLOR                      15
#define OUTPUT_BACKGROUND_COLOR         16
#define JUNCTION_COLOR                  17
#define MESH_GRID_MAJOR_COLOR           18
#define MESH_GRID_MINOR_COLOR           19
#define FREESTYLE0_COLOR                20
#define FREESTYLE1_COLOR                21
#define FREESTYLE2_COLOR                22
#define FREESTYLE3_COLOR                23
#define FREESTYLE4_COLOR                24
#define FREESTYLE5_COLOR                25
#define FREESTYLE6_COLOR                26
#define FREESTYLE7_COLOR                27
#define FREESTYLE8_COLOR                28
#define FREESTYLE9_COLOR                29

#define DEFAULT_COLOR_INDEX             GRAPHIC_COLOR

#define DEFAULT_ARC_COLOR_INDEX         GRAPHIC_COLOR
#define DEFAULT_BOX_COLOR_INDEX         GRAPHIC_COLOR
#define DEFAULT_BUS_COLOR_INDEX         BUS_COLOR
#define DEFAULT_CIRCLE_COLOR_INDEX      GRAPHIC_COLOR
#define DEFAULT_COMPLEX_COLOR_INDEX     GRAPHIC_COLOR
#define DEFAULT_LINE_COLOR_INDEX        GRAPHIC_COLOR
#define DEFAULT_NET_COLOR_INDEX         NET_COLOR
#define DEFAULT_PATH_COLOR_INDEX        GRAPHIC_COLOR
#define DEFAULT_PICTURE_COLOR_INDEX     GRAPHIC_COLOR
#define DEFAULT_PIN_COLOR_INDEX         PIN_COLOR
#define DEFAULT_TEXT_COLOR_INDEX        TEXT_COLOR

#define ARC_COLOR_INDEX      { OBJ_ARC,     DEFAULT_ARC_COLOR_INDEX     }
#define BOX_COLOR_INDEX      { OBJ_BOX,     DEFAULT_BOX_COLOR_INDEX     }
#define BUS_COLOR_INDEX      { OBJ_BUS,     DEFAULT_BUS_COLOR_INDEX     }
#define COMPLEX_COLOR_INDEX  { OBJ_COMPLEX, DEFAULT_COMPLEX_COLOR_INDEX }
#define CIRCLE_COLOR_INDEX   { OBJ_CIRCLE,  DEFAULT_CIRCLE_COLOR_INDEX  }
#define LINE_COLOR_INDEX     { OBJ_LINE,    DEFAULT_LINE_COLOR_INDEX    }
#define NET_COLOR_INDEX      { OBJ_NET,     DEFAULT_NET_COLOR_INDEX     }
#define TEXT_COLOR_INDEX     { OBJ_TEXT,    DEFAULT_TEXT_COLOR_INDEX    }
#define PATH_COLOR_INDEX     { OBJ_PATH,    DEFAULT_PATH_COLOR_INDEX    }
#define PIN_COLOR_INDEX      { OBJ_PIN,     DEFAULT_PIN_COLOR_INDEX     }
#define PICTURE_COLOR_INDEX  { OBJ_PICTURE, DEFAULT_PICTURE_COLOR_INDEX }

#define RGB_NOCOLOR   {0xff, 0xff, 0xff, 0xff, FALSE}
#define RGB_BLACK     {0x00, 0x00, 0x00, 0xff, TRUE}
#define RGB_BLUE      {0x00, 0x00, 0xff, 0xff, TRUE}
#define RGB_CYAN      {0x00, 0xff, 0xff, 0xff, TRUE} /* aqua */
#define RGB_GRAY      {0x88, 0x88, 0x88, 0xff, TRUE}
#define RGB_GREY      {0x88, 0x88, 0x88, 0xff, TRUE}
#define RGB_GREEN     {0x00, 0xff, 0x00, 0xff, TRUE}
#define RGB_MAGENTA   {0xff, 0x00, 0xff, 0xff, TRUE} /* fuchsia */
#define RGB_ORANGE    {0xff, 0xa5, 0xff, 0xff, TRUE}
#define RGB_RED       {0xff, 0x00, 0x00, 0xff, TRUE}
#define RGB_YELLOW    {0xff, 0xff, 0x00, 0xff, TRUE}
#define RGB_WHITE     {0xff, 0xff, 0xff, 0xff, TRUE}
#define RGB_ENDMAP    {0x00, 0x00, 0x00, 0x00, FALSE}
#endif
