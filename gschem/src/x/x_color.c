/* -*- C x_color.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file x_color.c
 * \brief Color Module
 * Contains routines for setting color schemes and manipulating Color
 * Arrays used in Gschem.
 *
 */

#include "gschem.h"
#include <geda_debug.h>

COLOR display_colors[MAX_COLORS];
COLOR outline_colors[MAX_COLORS];

static GdkColor* gdk_colors[MAX_COLORS];
static GdkColor* gdk_outline_colors[MAX_COLORS];

static GdkColormap *colormap = NULL;

static void inline x_color_invalid_index(int index)
{
  u_log_message(_("Tried to get an invalid color: %d\n"), index);
}

/*! \brief Initializes the color system for the application.
 *  \par Function Documentation
 *
 *  Initializes color maps to default values.
 */
void
x_color_init (void)
{
  colormap = gdk_colormap_get_system ();

  /* Initialize default color maps */
  s_color_map_defaults (display_colors);
  s_color_map_defaults (outline_colors);
}

/*! \brief Frees memory used by the color system.
 *  \par Function Documentation
 *  This function frees the colors from colormap along with
 *  \b black and \b white.
 */
void
x_color_free (void)
{
  int i;

  gdk_colormap_free_colors (colormap, &black, 1);
  gdk_colormap_free_colors (colormap, &white, 1);

  for (i = 0; i < MAX_COLORS; i++) {
    if (display_colors[i].enabled)
      gdk_colormap_free_colors (colormap, gdk_colors[i], 1);
    if (outline_colors[i].enabled)
      gdk_colormap_free_colors (colormap, gdk_outline_colors[i], 1);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void x_color_allocate (void)
{
  int error;
  int i;
  COLOR c;

  const char *err_allocate_s1   = _("Could not allocate the color %s!\n");
  const char *err_allocate_s1i1 = _("Could not allocate %s color %i!\n");

  gdk_color_parse ("black", &black);
  if (!gdk_colormap_alloc_color (colormap, &black, FALSE, TRUE)) {
    fprintf (stderr, err_allocate_s1, _("black"));
    exit (-1);
  }

  gdk_color_parse ("white", &white);

  if (!gdk_colormap_alloc_color (colormap, &white, FALSE, TRUE)) {
    fprintf (stderr, err_allocate_s1, _("white"));
    exit (-1);
  }

  for (i = 0; i < MAX_COLORS; i++) {

    if (display_colors[i].enabled) {

      gdk_colors[i] = (GdkColor *) GEDA_MEM_ALLOC(sizeof(GdkColor));

      c = display_colors[i];

      /* Interpolate 8-bpp colours into 16-bpp GDK color
       * space. N.b. ignore transparency because GDK doesn't
       * understand it. */
      gdk_colors[i]->red   = c.r + (c.r<<8);
      gdk_colors[i]->green = c.g + (c.g<<8);
      gdk_colors[i]->blue  = c.b + (c.b<<8);

      error = gdk_color_alloc(colormap, gdk_colors[i]);

      if (error == FALSE) {
        u_log_message (err_allocate_s1i1, _("display"), i);
      }
    }
    else {
      gdk_colors[i] = NULL;
    }

    if (outline_colors[i].enabled) {

      gdk_outline_colors[i] = (GdkColor *) GEDA_MEM_ALLOC(sizeof(GdkColor));

      c = outline_colors[i];

      /* Interpolate 8-bpp colours into 16-bpp GDK color space. N.b.
       * ignore transparency because GDK does not understand it. */
      gdk_outline_colors[i]->red   = c.r + (c.r<<8);
      gdk_outline_colors[i]->green = c.g + (c.g<<8);
      gdk_outline_colors[i]->blue  = c.b + (c.b<<8);

      error = gdk_color_alloc(colormap, gdk_outline_colors[i]);

      if (error == FALSE) {
        u_log_message (err_allocate_s1i1, _("outline"), i);
      }
    }
    else {
      gdk_outline_colors[i] = NULL;
    }
  }
}

/*! \brief Get Pointer to GdkColor
 *  \par Function Documentation
 *   Returns a pointer to the active GdkColor object given the geda
 *   color index.
 */
GdkColor *x_color_get_color_from_index(int index)
{
  if ((index < 0) || (index >= MAX_COLORS) || (gdk_colors[index] == NULL)) {
    x_color_invalid_index(index);
    return(&white);
  }
  else {
    return(gdk_colors[index]);
  }
}

/*! \brief Get Display Color Map
 *  \par Function Documentation
 *   Returns a pointer to a new Garray containing a copy of the
 *   current color-map allocations.
 *
 *  \note the returned color-map MUST be freed using g_array_free.
 */
GArray *x_color_get_display_color_map()
{
  GArray *color_map;

  color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  color_map = g_array_append_vals (color_map, display_colors, MAX_COLORS);
  return color_map;
}

/*! \brief Get Display Outline Color Map
 *  \par Function Documentation
 *   Returns a pointer to a new Garray containing a copy of the
 *   current color-map allocations.
 *
 *  \note the returned color-map MUST be freed using g_array_free.
 */
GArray *x_color_get_outline_color_map()
{
  GArray *color_map;

  color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  color_map = g_array_append_vals (color_map, outline_colors, MAX_COLORS);

  return color_map;
}

/*!
 *  \brief Get RGBA color for display given a color Index
 *  \par Function Description
 *  \return display color at index.
 *  \todo function name should include the word "display"
 */
COLOR *x_color_lookup (int color)
{
  if (color < 0 || color >= MAX_COLORS || !display_colors[color].enabled) {
    x_color_invalid_index(color);
    return &display_colors[DEFAULT_COLOR_INDEX];
  }
  else {
    return &display_colors[color];
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
bool x_color_get_state (int color)
{
  return display_colors[color].enabled;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_color_set_state (int color, int state)
{
  display_colors[color].enabled = state != FALSE;
}

bool x_color_display_enabled (int index)
{
  return (gdk_colors[index] != NULL);
}

/*! \brief Loads and executes a color map scheme
 *  \par Function Description
 *       This function executes a color map scm file after
 *       verifying accessibility. The file must be referenced
 *       relative to the path returned by geda-rc-path. The
 *       current colors are free and the new color allocated.
 */
int x_color_load_scheme(char *scheme) {

  char *inputfile;
  int   result;

  inputfile = f_get_data_filespec(scheme);

  if (inputfile) {

    x_color_free();

    if (s_color_load_scheme(inputfile)) {
      q_log_message(_("Allocating new color scheme: %s\n"), scheme);
      x_color_allocate();
      result = TRUE;
    }
    else {
      u_log_message (_("Something went wrong, check:%s\n"), scheme);
      result = FALSE;
    }
  }
  else {
    u_log_message (_("%s: Could not locate file:%s\n"), __func__,scheme);
    result = FALSE;
  }

  GEDA_FREE(inputfile);

  return result;
}
