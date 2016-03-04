/* -*- C indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-*/
/*
 * File: color_key.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedacolor - gEDA's Extension library for Color
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Contributing Author: Wiley Edward Hill
 * Date Contributed: September, 15, 2015
 */

#include <config.h>

#include <gdk/gdk.h>

#include <geda/geda_standard.h>

#include <geda/geda_idefines.h>       /* for MAX_COLORS */

#include "../../include/geda_color.h" /* for st_object_color */
#include "../../include/globals.h"
#include "../../include/gettext_priv.h"
#include <geda_debug.h>

/* See defines in geda_colors.h */
const char *color_keys [] = {
  "background",         /* 0 */
  "pin",                /* 1 */
  "net-endpoint",       /* 2 */
  "graphic",            /* 3 */
  "net",                /* 4 */
  "attribute",          /* 5 */
  "logic-bubble",       /* 6 */
  "dots-grid",          /* 7 */
  "detached-attribute", /* 8 */
  "text",               /* 9 */
  "bus",                /* 10 */
  "select",             /* 11 */
  "bounding-box",       /* 12 */
  "zoom-box",           /* 13 */
  "stroke",             /* 14 */
  "lock",               /* 15 */
  "output-background",  /* 16 */
  "junction",           /* 17 */
  "mesh-grid-major",    /* 18 */
  "mesh-grid-minor",    /* 19 */
  "freestyle0",         /* 20 */
  "freestyle1",         /* 21 */
  "freestyle2",         /* 22 */
  "freestyle3",         /* 23 */
  "freestyle4",         /* 24 */
  "freestyle5",         /* 25 */
  "freestyle6",         /* 26 */
  "freestyle7",         /* 27 */
  "freestyle8",         /* 28 */
  "freestyle9",         /* 29 */
   NULL
};

/*! \brief Get Table of Standard Color Names
 *  \par Function Documentation
 *   Returns a pointer to a new Garray containing a copy of the
 *   stdcolors allocations.
 *
 *  \returns color_table, the table should be freed using g_array_free.
 */
int geda_color_key_get_index(const char *name)
{
  int index;

  if (name) {

    static int last = -1;
    int trip = 0;

    index = last < sizeof (color_keys) / sizeof (color_keys[0]) - 2 ? ++last : 0;

    while (color_keys[index]) {

      if (strcmp(color_keys[index],name) == 0) {
        break;
      }

      index++;

      if (!color_keys[index] && last) {
        if (trip) {
          index = -1;
          break;
        }
        /* background becomes last on the list is okay */
        index = 0;
        trip  = 1;
      }
    }

    last = !(index - last) ? index : 0;
  }
  else {
    index = -1;
  }

  return index;
}


/* ! \brief Initializes the color system for the application.
 *  \par Function Documentation
 *
 *  Initializes color maps to default values.
 */
/*
void geda_color_key_init(void)
{

}
*/
/* ! \brief Frees memory used by the color system.
 *  \par Function Documentation
 *  This function frees the colors from colormap along with
 *  \b black and \b white.
 */
/*
void geda_color_key_release_resources(void)
{

}
*/