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

/*! \brief Initializes the color system for the application.
 *  \par Function Documentation
 *
 *  Initialises the color maps to defaults.
 */
void
x_color_init (void)
{
  colormap = gdk_colormap_get_system ();

  /* Initialise default color maps */
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

  gdk_color_parse ("black", &black);
  if (!gdk_colormap_alloc_color (colormap, &black, FALSE, TRUE)) {
    fprintf (stderr, _("Could not allocate the color %s!\n"), _("black"));
    exit (-1);
  }

  gdk_color_parse ("white", &white);

  if (!gdk_colormap_alloc_color (colormap, &white, FALSE, TRUE)) {
    fprintf (stderr, _("Could not allocate the color %s!\n"), _("white"));
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
        g_error (_("Could not allocate display color %i!\n"), i);
      }
    }
    else {
      gdk_colors[i] = NULL;
    }

    if (outline_colors[i].enabled) {

      gdk_outline_colors[i] = (GdkColor *) GEDA_MEM_ALLOC(sizeof(GdkColor));

      c = outline_colors[i];

      /* Interpolate 8-bpp colours into 16-bpp GDK color
       * space. N.b. ignore transparency because GDK doesn't
       * understand it. */
      gdk_outline_colors[i]->red   = c.r + (c.r<<8);
      gdk_outline_colors[i]->green = c.g + (c.g<<8);
      gdk_outline_colors[i]->blue  = c.b + (c.b<<8);

      error = gdk_color_alloc(colormap, gdk_outline_colors[i]);

      if (error == FALSE) {
        g_error (_("Could not allocate outline color %i!\n"), i);
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
 *   gschem color index.
 */
GdkColor *x_color_get_color_from_index(int index)
{
  if ((index < 0) || (index >= MAX_COLORS) || (gdk_colors[index] == NULL)) {
    u_log_message (_("Tried to get an invalid color index: %d\n"), index);
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
  GArray* color_map;
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
  GArray* color_map;
  color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  color_map = g_array_append_vals (color_map, outline_colors, MAX_COLORS);
  return color_map;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  \todo function name should include the word "display"
 */
COLOR *x_color_lookup (int color)
{
  if (color < 0 || color >= MAX_COLORS || !display_colors[color].enabled) {
    fprintf(stderr, _("Tried to get an invalid color: %d\n"), color);
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
int x_color_load_scheme(char* scheme) {

  char *strBuffer;
  char *inputfile;
  char *rc_path;
  int   result = FALSE;

  SCM s_result;

  strBuffer = GEDA_MEM_ALLOC( MAX_FILE ); /* be 255 */

  if (strBuffer) {
    rc_path = u_string_scm2c("geda-rc-path");
    inputfile = u_string_concat (rc_path, DIR_SEPARATOR_S, scheme, NULL);
    free(rc_path);
    if ((access (inputfile, R_OK)) == 0) {
      x_color_free();
      strcpy(strBuffer, "(load \"");
      strcat(strBuffer, inputfile);
      strcat(strBuffer, "\")");
      scm_dynwind_begin (0);
        scm_dynwind_free(inputfile);
        scm_dynwind_free(strBuffer);
        s_result = g_scm_c_eval_string_protected(strBuffer);
      scm_dynwind_end ();
      if ((result = scm_is_true(s_result)) ? 1 : 0) {
        q_log_message(_("Allocatating new color scheme\n"));
        x_color_allocate();
      }
    }
    else {
      GEDA_FREE(strBuffer);
      u_log_message (_("%s: Could not locate file:%s\n"), __func__,scheme);
    }
  }
  else {
    u_log_message(_("%s: Memory allocation error\n"), __func__);
  }
  return result;
}
