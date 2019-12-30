/* -*- C indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*-*/
/*
 * File: color_x11.c
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
 */
/*!
 * \file color_x11.c
 * \brief X11 Color Module
 * Contains routines for setting color schemes and manipulating
 * Color Arrays used in gEDA applications.
 */

#include <config.h>

#include <gdk/gdk.h>

#include <libgeda/libgeda.h>

#include "../../include/globals.h"
#include "../../include/libgedacolor.h"
#include "../../include/private.h"
#include "../../include/gettext_priv.h"
#include <geda_debug.h>

static GdkColor white;
static GdkColor black;

static GdkColor *x_display_colors[MAX_COLORS];
static GdkColor *x_outline_colors[MAX_COLORS];

static GdkColormap *x_colormap = NULL;

extern COLOR display_colors[MAX_COLORS];
extern COLOR outline_colors[MAX_COLORS];

static void inline x11_color_invalid_index(int index)
{
  u_log_message("%s: %d\n", _("Tried to get an invalid color"), index);
}

/*!
 * \brief Allocate system colors
 * \par Function Documentation
 *  Allocates system colors found in color maps. Called after initialization
 *  of the colormaps.
 */
void geda_color_x11_allocate (void)
{
  int error;
  int i;
  COLOR c;

  void log_allocation_error(const char *map) {

    const char *_allocate = _("Could not allocate");
    const char *_color    = _("color");

    u_log_message ("%s %s %s %i!\n", _allocate, map, _color, i);
  }

  gdk_color_parse ("black", &black);

  if (!gdk_colormap_alloc_color (x_colormap, &black, FALSE, TRUE)) {
    const char *err_msg  = _("Could not allocate the color");
    fprintf (stderr, "%s %s!\n", err_msg, _("black"));
    exit (-1);
  }

  gdk_color_parse ("white", &white);

  if (!gdk_colormap_alloc_color (x_colormap, &white, FALSE, TRUE)) {
    const char *err_msg  = _("Could not allocate the color");
    fprintf (stderr, "%s %s!\n", err_msg, _("white"));
    exit (-1);
  }

  for (i = 0; i < MAX_COLORS; i++) {

    if (display_colors[i].enabled) {

      c = display_colors[i];

      /* Interpolate 8-bpp colours into 16-bpp GDK color space
       * ignoring transparency because GDK does not support. */
      x_display_colors[i]->red   = c.r + (c.r<<8);
      x_display_colors[i]->green = c.g + (c.g<<8);
      x_display_colors[i]->blue  = c.b + (c.b<<8);

      error = gdk_colormap_alloc_color (x_colormap, x_display_colors[i], FALSE, TRUE);

      if (error == FALSE) {
        log_allocation_error(_("display"));
      }
    }

    if (outline_colors[i].enabled) {

      c = outline_colors[i];

      /* Interpolate 8-bpp colours into 16-bpp GDK color space. N.b.
       * ignore transparency because GDK does not understand it. */
      x_outline_colors[i]->red   = c.r + (c.r<<8);
      x_outline_colors[i]->green = c.g + (c.g<<8);
      x_outline_colors[i]->blue  = c.b + (c.b<<8);

      error = gdk_colormap_alloc_color (x_colormap, x_outline_colors[i], FALSE, TRUE);

      if (error == FALSE) {
        log_allocation_error(_("outline"));
      }
    }
  }
}

/*!
 * \brief Get Pointer to GdkColor
 * \par Function Documentation
 *  Returns a pointer to the active GdkColor object given the color index.
 */
GdkColor *geda_color_x11_color_from_index(int index)
{
  if ((index < 0) || (index >= MAX_COLORS) || (x_display_colors[index] == NULL)) {
    x11_color_invalid_index(index);
    return (&white);
  }
  else {
    return (x_display_colors[index]);
  }
}


/*!
 * \brief Get RGBA color for display given a color Index
 * \par Function Description
 *
 * \return display color at index.
 */
COLOR *geda_color_x11_display_lookup (int color)
{
  if (color < 0 || color >= MAX_COLORS || !display_colors[color].enabled) {
    x11_color_invalid_index(color);
    return &display_colors[DEFAULT_COLOR_INDEX];
  }
  else {
    return &display_colors[color];
  }
}

/*!
 * \brief Get if color is enabled or disabled
 * \par Function Description
 * \note is only relavent to display color
 *
 * \returns the enable flag corresponding to \a color
 */
bool geda_color_x11_get_state (int color)
{
  return display_colors[color].enabled;
}

/*!
 * \brief Set state of a color enabled or disabled
 * \par Function Description
 * \note is only relavent to display color
 */
void geda_color_x11_set_state (int color, int state)
{
  display_colors[color].enabled = state != FALSE;
}

/*!
 * \brief Frees colors used by the color system.
 * \par Function Documentation
 *  This function frees colors in the X11 colormap along with
 *  \b black and \b white.
 */
void geda_color_x11_free (void)
{
  if (x_colormap) {

    int i;

    gdk_colormap_free_colors (x_colormap, &black, 1);
    gdk_colormap_free_colors (x_colormap, &white, 1);

    for (i = 0; i < MAX_COLORS; i++) {

      if (display_colors[i].enabled) {
        gdk_colormap_free_colors (x_colormap, x_display_colors[i], 1);
      }
      if (outline_colors[i].enabled) {
        gdk_colormap_free_colors (x_colormap, x_outline_colors[i], 1);
      }
    }
  }
}

/*!
 * \brief Initializes the color system for the application.
 * \par Function Documentation
 *  Initializes color maps to default values.
 */
void geda_color_x11_init (void)
{
  int i;

  x_colormap = gdk_colormap_get_system ();

  for (i = 0; i < MAX_COLORS; i++) {
    x_display_colors[i] = (GdkColor*) GEDA_MEM_ALLOC(sizeof(GdkColor));
    x_outline_colors[i] = (GdkColor*) GEDA_MEM_ALLOC(sizeof(GdkColor));
  }
}

/*!
 * \brief Loads and executes a color map scheme
 * \par Function Description
 *  This function executes a color map scm file after verifying accessibility.
 *  The file must be referenced relative to the path returned by geda-rc-path.
 *  The current colors are free and the new color allocated.
 */
int geda_color_x11_load_scheme(char *scheme) {

  char *inputfile;
  int   result;

  inputfile = geda_file_get_data_filespec(scheme);

  if (inputfile) {

    geda_color_x11_free();

    if (geda_color_guile_load_scheme(inputfile)) {

      u_log_message("%s: %s\n", _("Allocating color scheme"), scheme);
      geda_color_x11_allocate();
      result = TRUE;
    }
    else {
      u_log_message ("%s: %s\n", _("Something went wrong, check"), scheme);
      result = FALSE;
    }
  }
  else {
    const char *err_msg = _("Could not locate file");
    u_log_message ("<libgedacolor> %s: %s\n", err_msg, scheme);
    result = FALSE;
  }

  GEDA_FREE(inputfile);

  return result;
}


/*!
 * \brief Release resources memory used by X11 color system.
 * \par Function Documentation
 *  Releases resources allocated by geda_color_x11_init.
 */
void geda_color_x11_release_resources (void)
{
  int i;

  geda_color_x11_free();

  for (i = 0; i < MAX_COLORS; i++) {

    free (x_display_colors[i]);
    x_display_colors[i] = NULL;

    free(x_outline_colors[i]);
    x_outline_colors[i] = NULL;
  }

  if (GDK_IS_COLORMAP(x_colormap)) {
   g_object_unref(x_colormap);
  }
}
