/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: g_rc.c
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */
/* Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 10/17/12 |  Reordered sequenced functions and added catagory markers
 *                |  to match i_var.c (to better oganize the source code.)
 *                |  Replaced most input string literals with macros defined
 *                |  in gschem_sdefines.h ( so strings could be syncronized
 *                |  with output routines in keywords.h)
 *                |  Added check_and_convert_scm_integer and associated macro
 *                |  (in order to stream-line coding and eliminate SCM_ASSERT
 *                |  for integers. Most functions were returning SCM_BOOL_T
 *                |  even when assertions failed, and this cause much havoc)
 *                |  Added g_rc_map_keys function (in order to by-pass functions
 *                |  in gschem.scm)
 * ------------------------------------------------------------------
 * WEH | 12/02/12 |  Renamed function g_rc_autoplace_attributes_grid to g_rc_
 *                |  attribute_placement_grid, added function call to x_
 *                |  settings_set_scm_int to set the definition of autoplace-
 *                |  attributes-grid
 * ------------------------------------------------------------------
 * WEH | 12/30/12 |  invert-images
 * ------------------------------------------------------------------
 * WEH | 01/06/14 |  Relocated g_rc_bus_ripper_symname from libgeda to gschem
 *                |  (so that all the bus ripper variable would be in the same
 *                |  module), changed ripper Rotation and Type values to strings
 *                |  defined in sdefines.h
 * ------------------------------------------------------------------
 * WEH | 03/03/14 |  Revised g_rc_parse_gtkrc() to include log messages using
 *                |  v_log_message and q_log_message
 *
 */

#include <config.h>
#include "version.h"
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#include <stdio.h>

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#include <ctype.h>

#include "gschem.h"
#include "ascii.h"

/*! \brief Loads gtk rc files for pararsing
 *
 *  \par Function Description
 *       This function calls gtk_rc_parse to arse the our system and user
 *       initialization files.
 */
void g_rc_parse_gtkrc()
{
  char *filename;

  /* System */
  filename = g_build_filename (f_path_sys_config (), "gschem-gtkrc", NULL);
  if (access(filename, R_OK) == 0) {
    v_log_message("Processing %s\n", filename);
    gtk_rc_parse (filename);
  }
  else {
    v_log_message("Skipping %s, %s\n", filename, strerror(errno));
  }
  GEDA_FREE (filename);

  /* User */
  filename = g_build_filename (f_path_user_config (), "gschem-gtkrc", NULL);
  if (access(filename, R_OK) == 0) {
    v_log_message("Processing %s\n", filename);
    gtk_rc_parse (filename);
  }
  else {
    v_log_message("Skipping %s, %s\n", filename, strerror(errno));
  }
  GEDA_FREE (filename);
}

/*! \brief This function is used to validate and convert integers from scm to c type.
 *  \par Function Description
 *       Utility function to be used by functions reading scm interger type input data.
 *
 */
#define ICHECK(value, min, max, default, var_name) \
        check_and_convert_scm_integer( value, min, max, default, var_name)
int check_and_convert_scm_integer( SCM val2chk, int min_value, int max_value,
                                   int default_value, char* keyword) {

  int val;

  int above() {
    fprintf (stderr, _("Bad Value [%d], check %s entry in rc file\n"), val, keyword);
    fprintf (stderr, _("Continuing with maximum value=[%d]\n"), max_value);
    return max_value; /* maximum value */
  }
  int below() {
    fprintf (stderr, _("Bad Value [%d], check %s entry in rc file\n"), val, keyword);
    fprintf (stderr, _("Continuing with minimum value=[%d]\n"), min_value);
    return min_value;   /* minimum value */
  }
  int the_default() {
    fprintf (stderr, _("Continuing with default value=[%d]\n"), default_value);
    return default_value; /* default value*/
  }
  if (scm_is_integer(val2chk)) {
    val = scm_to_int (val2chk);
    if (val < min_value) { below(); }
    else {
      if (max_value > 0) {
        if (val > max_value) { above(); }
      }
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check %s entry in rc file\n"), keyword);
    the_default();
  }
  return val;
}

/*! \brief This function processes the RC version information
 *  \par Function Description
 *       This function processes the version string in the rc and
 *       compares the value to the current program version.
 *
 *       Return value: TRUE if versions match else FALSE
 */
SCM g_rc_gschem_version(SCM scm_version)
{
  SCM ret;
  char *version;
  SCM rc_filename;
  char *sourcefile;

  SCM_ASSERT (scm_is_string (scm_version), scm_version,
              SCM_ARG1, "gschem-version");

  scm_dynwind_begin (0);
  version = scm_to_utf8_string (scm_version);
  scm_dynwind_free (version);

  if (g_ascii_strcasecmp (version, PACKAGE_DATE_VERSION) != 0) {
    sourcefile = NULL;
    rc_filename = g_rc_rc_filename ();
    sourcefile = scm_to_utf8_string (rc_filename);
    scm_dynwind_free (sourcefile);

    fprintf(stderr,
          _("You are running gEDA/gaf version [%s%s.%s],\n"),
            PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION,
            PACKAGE_DATE_VERSION);

    fprintf(stderr,
          _("but you have a version [%s] gschemrc file:\n[%s]\n"),
            version, sourcefile);

    fprintf(stderr,
          _("Please be sure that you have the latest rc file.\n"));

    ret = SCM_BOOL_F;
  } else {
    ret = SCM_BOOL_T;
  }
  scm_dynwind_end();
  return ret;
}

/* Color Related */
extern COLOR display_colors[MAX_COLORS];
extern COLOR outline_colors[MAX_COLORS];

/*! \brief This function processes the display-color-map RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the display-color-map RC entry.
 *
 */
SCM g_rc_display_color_map (SCM scm_map)
{
  if (scm_map == SCM_UNDEFINED) {
    return s_color_map_to_scm (display_colors);
  }

  SCM_ASSERT (scm_is_true (scm_list_p (scm_map)),
              scm_map, SCM_ARG1, "display-color-map");
  cmap_flag = 1;
  s_color_map_from_scm (display_colors, scm_map, "display-color-map");
  return SCM_BOOL_T;
}

/*! \brief This function processes the display-outline-color-map RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the display-outline-color-map RC entry.
 *
 */
SCM g_rc_display_outline_color_map (SCM scm_map)
{
  if (scm_map == SCM_UNDEFINED) {
    return s_color_map_to_scm (outline_colors);
  }

  SCM_ASSERT (scm_is_true (scm_list_p (scm_map)),
              scm_map, SCM_ARG1, "display-outline-color-map");
  cmap_flag = cmap_flag * -1;
  s_color_map_from_scm (outline_colors, scm_map, "display-outline-color-map");
  return SCM_BOOL_T;
}

/* Display Sub-System */

/*! (category "Display")
 *  (sub-category "Render")
 *  \brief This function processes the render-adaptor RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the render-adaptor RC entry.
 */
SCM g_rc_render_adaptor(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {CAIRO_ADAPTOR,  RC_RENDERER_OPTION_CAIRO},
    {X11_ADAPTOR,    RC_RENDERER_OPTION_X11}
  };

  RETURN_G_RC_MODE("render-adaptor",
                    default_render_adaptor,
                    2);
}

/*! (category "Display")
 *  (sub-category "Cario")
 *  \brief This function processes the anti-aliasing RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the anti-aliasing RC entry.
 */
SCM g_rc_anti_aliasing(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {CAIRO_ANTIALIAS_DEFAULT,  RC_STR_ANTIALIAS_DEFAULT},
    {CAIRO_ANTIALIAS_NONE,     RC_STR_ANTIALIAS_NONE},
    {CAIRO_ANTIALIAS_GRAY,     RC_STR_ANTIALIAS_GRAY},
    {CAIRO_ANTIALIAS_SUBPIXEL, RC_STR_ANTIALIAS_SUBPIXEL},
    {CAIRO_ANTIALIAS_FAST,     RC_STR_ANTIALIAS_FAST},
    {CAIRO_ANTIALIAS_GOOD,     RC_STR_ANTIALIAS_GOOD},
    {CAIRO_ANTIALIAS_BEST,     RC_STR_ANTIALIAS_BEST}
  };

  RETURN_G_RC_MODE("anti-aliasing",
                    default_anti_aliasing,
                    7);
}

/*! \brief This function processes the draw-grips RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for draw-grips RC entry.
 *
 */
SCM g_rc_draw_grips(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("draw-grips",
                   default_draw_grips,
                   2);
}

/*! \brief This function processes the grip-size RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the grip-size RC entry.
 */
SCM g_rc_grips_pixel_size (SCM size)
{
  default_grip_size = ICHECK(size, MIN_GRIP_SIZE, MAX_GRIP_SIZE,
                                   DEFAULT_GRIP_SIZE, "grip-size");
  return SCM_BOOL_T;
}

/*! (category "Display")
 *  (sub-category "Grid")
 *  \brief This function processes the grid-mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the grid-mode RC entry.
 */
SCM g_rc_grid_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {GRID_NONE, RC_STR_NONE},
    {GRID_DOTS, RC_STR_DOTS},
    {GRID_MESH, RC_STR_MESH}
  };

  RETURN_G_RC_MODE("grid-mode",
                   default_grid_mode,
                   3);
}

/*! \brief This function processes the dots-grid-minor-color RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variables while
 *       processing configuration data for the dots-grid-minor-color RC
 *       entry. The pixel field of the structure for default values is
 *       set to a value other than 88, to indicate the fields have been
 *       set from rc values.
 *
 */
SCM g_rc_dots_grid_minor_color (SCM red, SCM green, SCM blue)
{
  int r = ICHECK(red, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "dots-grid-minor-color");

  int g = ICHECK(green, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "dots-grid-minor-color");

  int b = ICHECK(blue, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "dots-grid-minor-color");

 /* The pixel field is not used for color, but we use as flag */
  default_dots_grid_minor_color.pixel = 99;
  default_dots_grid_minor_color.red   = r;
  default_dots_grid_minor_color.green = g;
  default_dots_grid_minor_color.blue  = b;

  return SCM_BOOL_T;
}

/*! \brief This function processes the dots-grid-major-color RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variables while
 *       processing configuration data for the dots-grid-major-color RC
 *       entry. The pixel field of the structure for default values is
 *       set to a value other than 88, to indicate the fields have been
 *       set from rc values.
 *
 */
SCM g_rc_dots_grid_major_color (SCM red, SCM green, SCM blue)
{
  int r = ICHECK(red, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "dots-grid-major-color");

  int g = ICHECK(green, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "dots-grid-major-color");

  int b = ICHECK(blue, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "dots-grid-major-color");

 /* The pixel field is not used for color, but we use as flag */
  default_dots_grid_major_color.pixel = 99;
  default_dots_grid_major_color.red   = r;
  default_dots_grid_major_color.green = g;
  default_dots_grid_major_color.blue  = b;

  return SCM_BOOL_T;
}

/*! \brief This function processes the dots-grid-dot-size RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the dots-grid-dot-size RC entry.
 */
SCM g_rc_dots_grid_dot_size (SCM dotsize)
{
  default_dots_grid_dot_size = ICHECK(dotsize, MIN_GRID_DOT_SIZE, -1,
                               DEFAULT_GRID_DOT_SIZE, "dots-grid-dot-size");
  return SCM_BOOL_T;
}

/*! \brief This function processes the dots_grid_mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the dots_grid_mode RC entry.
 */
SCM g_rc_dots_grid_mode (SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {DOTS_GRID_VARIABLE_MODE, RC_STR_DOTS_MODE_VARIABLE },
    {DOTS_GRID_FIXED_MODE,    RC_STR_DOTS_MODE_FIXED }
  };

  RETURN_G_RC_MODE ("dots-grid-mode",
                    default_dots_grid_mode,
                    2);
}

/*! \brief This function processes the dots-grid-threshold RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the dots-grid-threshold RC
 *       entry.
 *
 */
SCM g_rc_dots_grid_threshold (SCM threshold)
{
  default_dots_grid_threshold = ICHECK(threshold,
                                       MIN_GRID_DOT_THRESHOLD,
                                       MAX_GRID_DOT_THRESHOLD,
                                       DEFAULT_GRID_DOT_THRESHOLD,
                                       "dots-grid-threshold");
  return SCM_BOOL_T;
}

/*! \brief This function processes the dots-grid-minor-alpha RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variables while
 *       processing configuration data for the dots-grid-minor-alpha RC
 *       entry.
 *
 */
SCM g_rc_dots_grid_minor_alpha (SCM percent)
{
  default_dots_grid_minor_alpha = ICHECK(percent,
                                         MIN_GRID_ALPHA,
                                         MAX_GRID_ALPHA,
                                         DEFAULT_GRID_MINOR_ALPHA,
                                         "dots-grid-minor-alpha");
  return SCM_BOOL_T;
}

/*! \brief This function processes the dots-grid-major-alpha RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variables while
 *       processing configuration data for the dots-grid-minor-alpha RC
 *       entry.
 *
 */
SCM g_rc_dots_grid_major_alpha (SCM percent)
{
  default_dots_grid_major_alpha = ICHECK(percent,
                                         MIN_GRID_ALPHA,
                                         MAX_GRID_ALPHA,
                                         DEFAULT_GRID_MAJOR_ALPHA,
                                         "dots-grid-major-alpha");
  return SCM_BOOL_T;
}

/*! \brief This function processes the mesh-grid-threshold RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the mesh-grid-threshold RC
 *       entry.
 */
SCM g_rc_mesh_grid_threshold (SCM threshold)
{
  default_mesh_grid_threshold = ICHECK(threshold,
                                MIN_GRID_MESH_THRESHOLD,
                                MAX_GRID_DOT_THRESHOLD,
                                DEFAULT_GRID_MESH_THRESHOLD,
                                "mesh-grid-threshold");
  return SCM_BOOL_T;
}

/*! \brief This function processes the mesh-line-width-factor RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the mesh-line-width-factor RC
 *       entry.
 *
 */
SCM g_rc_mesh_line_width_factor (SCM width)
{
  default_mesh_line_width_factor = ICHECK(width,
                                        MIN_MESH_LINE_WIDTH_FACTOR,
                                        MAX_MESH_LINE_WIDTH_FACTOR,
                                        DEFAULT_MESH_LINE_WIDTH_FACTOR,
                                       "mesh-line-width-factor");
  return SCM_BOOL_T;
}

/*! \brief This function processes the mesh-grid-minor-alpha RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variables while
 *       processing configuration data for the mesh-grid-minor-alpha RC
 *       entry.
 *
 */
SCM g_rc_mesh_grid_minor_alpha (SCM percent)
{
  default_mesh_grid_minor_alpha = ICHECK(percent,
                                         MIN_GRID_ALPHA,
                                         MAX_GRID_ALPHA,
                                         DEFAULT_GRID_MINOR_ALPHA,
                                         "mesh-grid-minor-alpha");
  return SCM_BOOL_T;
}

/*! \brief This function processes the mesh-grid-major-alpha RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variables while
 *       processing configuration data for the mesh-grid-minor-alpha RC
 *       entry.
 *
 */
SCM g_rc_mesh_grid_major_alpha (SCM percent)
{
  default_mesh_grid_major_alpha = ICHECK(percent,
                                         MIN_GRID_ALPHA,
                                         MAX_GRID_ALPHA,
                                         DEFAULT_GRID_MAJOR_ALPHA,
                                         "mesh-grid-major-alpha");
  return SCM_BOOL_T;
}

/*! \brief This function processes the mesh-grid-minor-color RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variables while
 *       processing configuration data for the mesh-grid-minor-color RC
 *       entry. The pixel field of the structure for default values is
 *       set to a value other than 88, to indicate the fields have been
 *       set from rc values.
 *
 */
SCM g_rc_mesh_grid_minor_color (SCM red, SCM green, SCM blue)
{
  int r = ICHECK(red, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "mesh-grid-minor-color");

  int g = ICHECK(green, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "mesh-grid-minor-color");

  int b = ICHECK(blue, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "mesh-grid-minor-color");

 /* The pixel field is not used for color, but we use as a flag */
  default_mesh_grid_minor_color.pixel = 99;
  default_mesh_grid_minor_color.red   = r;
  default_mesh_grid_minor_color.green = g;
  default_mesh_grid_minor_color.blue  = b;

  return SCM_BOOL_T;
}

/*! \brief This function processes the mesh-grid-major-color RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variables while
 *       processing configuration data for the mesh-grid-minor-color RC
 *       entry. The pixel field of the structure for default values is
 *       set to a value other than 88, to indicate the fields have been
 *       set from rc values.
 *
 */
SCM g_rc_mesh_grid_major_color (SCM red, SCM green, SCM blue)
{
  int r = ICHECK(red, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "mesh-grid-major-color");

  int g = ICHECK(green, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "mesh-grid-major-color");

  int b = ICHECK(blue, 0, MAX_GRID_COLOR, DEFAULT_GRID_COLOR,
                "mesh-grid-major-color");

 /* The pixel field is not used for color, but we use as a flag */
  default_mesh_grid_major_color.pixel = 99;
  default_mesh_grid_major_color.red   = r;
  default_mesh_grid_major_color.green = g;
  default_mesh_grid_major_color.blue  = b;

  return SCM_BOOL_T;
}

/*! \brief This function processes the object-clipping RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the object-clipping RC entry.
 */
SCM g_rc_object_clipping(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED}
  };

  RETURN_G_RC_MODE("object-clipping",
                   default_object_clipping,
                   2);
}

/*! \brief This function processes the warp_cursor RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the warp_cursor RC entry.
 */
SCM g_rc_warp_cursor(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("warp-cursor",
                   default_warp_cursor,
                   2);
}

/*! \brief This function processes the window-size RC entry.
 *  \par Function Description
 *       function to dynamically process configuration data
 *       for the window-size RC entry. This sets the Windows
 *       X-Y size parameters "width" and "height".
 */
SCM g_rc_window_size(SCM width, SCM height)
{
  default_window_height = ICHECK( height,
                                  MIN_WINDOW_HEIGHT,
                                  MAX_WINDOW_HEIGHT,
                                  DEFAULT_WINDOW_HEIGHT,
                                  "window-size");

  default_window_width  = ICHECK( width,
                                  MIN_WINDOW_WIDTH,
                                  MAX_WINDOW_WIDTH,
                                  DEFAULT_WINDOW_WIDTH,
                                 "window-size");
  return SCM_BOOL_T;
}

/*! \brief This function processes the world-size RC entry.
 *  \par Function Description
 *       function to dynamically process configuration data
 *       for the world-size RC entry. This sets the World
 *       size parameters "init_right" and "init_bottom".
 *
 *  \param [in] width
 *  \param [in] height
 *  \param [in] border
 *
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_world_size(SCM width, SCM height, SCM border)
#define FUNC_NAME "world-size"
{
  int i_width, i_height, i_border;
  int i_right, i_bottom;

  SCM_ASSERT (SCM_NIMP (width) && SCM_REALP (width), width,
              SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (height) && SCM_REALP (height), height,
              SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (border) && SCM_REALP (border), border,
              SCM_ARG3, FUNC_NAME);

  /* yes this is legit, we are casing the resulting double to an int */
  i_width  = (int) (scm_to_double (width)  * MILS_PER_INCH);
  i_height = (int) (scm_to_double (height) * MILS_PER_INCH);
  i_border = (int) (scm_to_double (border) * MILS_PER_INCH);

  m_papersize_to_world(i_width, i_height, i_border, &i_right, &i_bottom);

#if DEBUG
  printf("%d %d\n", i_width, i_height);
  printf("%d %d\n", i_right, i_bottom);
#endif

  default_world_right  = i_right;
  default_world_bottom = i_bottom;

  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*! \brief This function processes the zoom-gain RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the zoom-gain RC entry.
 */
SCM g_rc_zoom_gain(SCM gain)
{
  int val;

  if (scm_is_integer(gain)) {
    val = scm_to_int (gain);
   /* Allow negative numbers in case the user wishes to reverse scroll
    * direction, but don't allow zero gain as this would result in a
    * division-by-zero error */
    if (val == 0) {
     fprintf(stderr, _("Invalid value, zoom-gain can not be zero, check entry in rc file\n"));
     val = DEFAULT_ZOOM_GAIN; /* assign default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check zoom-gain entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_ZOOM_GAIN);
    val = DEFAULT_ZOOM_GAIN; /* assign default */
  }

  default_zoom_gain = val;

  return SCM_BOOL_T;
}

/*! \brief This function processes the zoom-with-pan RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the zoom-with-pan RC entry.
 *
 */
SCM g_rc_zoom_with_pan(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE,  RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED}
  };

  RETURN_G_RC_MODE("zoom-with-pan",
                   default_zoom_with_pan,
                   2);
}

/* ----- Image Related ----- */
/*! \brief This function processes the image-color RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for image-color RC entry.
 */
SCM g_rc_image_color(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("image-color",
                   default_image_color,
                   2);
}
/*! \brief This function processes the image-color RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for invert-images RC entry.
 */
SCM g_rc_invert_images(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("invert-images",
                   default_invert_images,
                   2);
}
/* ----- Log related ----- */
/*! \brief This function processes log-enable information RC entry.
 * \par Function Description
 *       This function processes the entry for the log-enable
 *       string in the initialization rc file. This function accepts
 *       either string or integer type arguments.
 *
 */
SCM g_rc_logging(SCM mode)
{
  if(scm_is_string(mode)) {
    static const vstbl_entry mode_table[] = {
      {TRUE , RC_STR_ENABLED },
      {FALSE, RC_STR_DISABLED},
    };

    RETURN_G_RC_MODE("logging",
	    	      default_logging,
                      2);
  } else {
    if (scm_is_integer(mode)) {
      int val;

      val = scm_to_int (mode);
      default_logging = val;
    }
    else
      fprintf (stderr, _("Invalid type assignment, check logging entry in rc file\n"));
  }
  return SCM_BOOL_T;
}
/*! \brief This function processes log-destiny information
 *
 *  \par Function Description
 *       This function processes the entry for the log-destiny
 *       string in the initialization rc file
 *
 */
SCM g_rc_log_destiny(SCM mode)
{
  /* These are defined in topsrc/include/geda.h */
  static const vstbl_entry mode_table[] = {
    {CONSOLE_WINDOW     , RC_STR_DESTINY_WINDOW },
    {STDOUT_TTY         , RC_STR_DESTINY_TTY },
    {BOTH_CONWIN_STDOUT , RC_STR_DESTINY_BOTH }
  };

  RETURN_G_RC_MODE("log-destiny",
                   default_log_destiny,
                   3);
}
/*! \brief This function processes the console-window RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the console-window
 *       RC entry.
 */
SCM g_rc_console_window(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {MAP_ON_STARTUP, "startup" },     /* depreciate */
    {MAP_LATER     , "later"   },     /* depreciate */
    {MAP_ON_STARTUP, RC_STR_ENABLED },
    {MAP_LATER     , RC_STR_DISABLED }
  };

  RETURN_G_RC_MODE("console-window",
                   default_console_window,
                   4);
}
/*! \brief This function processes the console-window-type RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the console-window-type
 *       RC entry.
 */
SCM g_rc_console_window_type(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRANSIENT, RC_STR_CONWIN_TRANSIENT },
    {DECORATED, RC_STR_CONWIN_DECORATED }
  };

  RETURN_G_RC_MODE("console-window-type",
                   default_console_window_type,
                   2);
}
/* ----- Miscellaneous ----- */
/*! \brief This function processes the action-feedback-mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the action-feedback-mode RC entry.
 */
SCM g_rc_action_feedback_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {OUTLINE    , RC_STR_FEEDBACK_OUTLINE  },
    {BOUNDINGBOX, RC_STR_FEEDBACK_BOUNDBOX }
  };

  RETURN_G_RC_MODE("action-feedback-mode",
                   default_action_feedback_mode,
                   2);
}
/*! \brief This function processes the add-attribute-offset RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the add-attribute-offset RC entry.
 */
SCM g_rc_add_attribute_offset(SCM offset)
{
  default_add_attribute_offset = ICHECK( offset, 0, -1,
                                 DEFAULT_ATTRIBUTE_OFFSET,
                                  "add-attribute-offset");
  return SCM_BOOL_T;
}
/*! \brief This function processes the add-menu RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the add-menu items.
 */
SCM g_rc_add_menu(SCM scm_menu_name, SCM scm_menu_items)
{
  char *menu_name;

  SCM_ASSERT (scm_is_string (scm_menu_name), scm_menu_name, SCM_ARG1, "add-menu");

  SCM_ASSERT (SCM_NIMP (scm_menu_items) && SCM_CONSP (scm_menu_items), scm_menu_items, SCM_ARG2, "add-menu");

  menu_name = scm_to_utf8_string (scm_menu_name);

  i_menu_add_entry(menu_name, scm_menu_items);
  free (menu_name);

  return SCM_BOOL_T;
}
/*! \brief This function processes the map-keys RC entry.
 *  \par Function Description
 *       This procedure is functionally equivalent to the
 *       global-set-key function but uses a slightly different
 *       syntax and bypasses the routines in gschem.scm.
 */
SCM g_rc_map_keys(SCM keys, SCM action)
{
  char *action_str;
  char *keys_str;
  char scm_expr[128];

  SCM s_result;

  SCM_ASSERT (scm_is_string (keys), keys, SCM_ARG1, "map-keys");
  SCM_ASSERT (scm_is_string (action), action, SCM_ARG2, "map-keys");

  action_str = scm_to_utf8_string(action);
  keys_str = scm_to_utf8_string(keys);

  strcpy(scm_expr, "(bind-keys! %global-keymap \"");
  strcat(scm_expr, keys_str);
  strcat(scm_expr, "\" '");
  strcat(scm_expr, action_str);
  strcat(scm_expr, ")");

  s_result = g_scm_c_eval_string_protected(scm_expr);

  free(action_str);
  free(keys_str);

  return s_result;

}
/*! \brief This function processes the auto-load-last RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the auto-load-last RC entry.
 */
SCM g_rc_auto_load_last(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("auto-load-last",
                   default_auto_load_last,
                   2);
}
/*! \brief This function processes the auto-save-interval RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the auto-save-interval RC entry.
 */
SCM g_rc_auto_save_interval(SCM seconds)
{
  default_auto_save_interval = ICHECK( seconds, 0, -1,
                               DEFAULT_SAVE_INTERVAL,
                               "auto_save_interval");
  return SCM_BOOL_T;
}
/*! \brief This function processes the attribute-name RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the attribute-name RC entry.
 */
SCM g_rc_attribute_name(SCM scm_path)
{
  char *path;
  SCM ret;

  SCM_ASSERT (scm_is_string (scm_path), scm_path,
              SCM_ARG1, "attribute-name");

  path = scm_to_utf8_string (scm_path);

  /* not unique? */
  if (!s_attrib_uniq(path)) {
    ret = SCM_BOOL_F;
  } else {
    s_attrib_add_entry (path);
    ret = SCM_BOOL_T;
  }

  free(path);
  return ret;
}
/*! \brief This function processes the autoplace-attributes-grid RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the autoplacement-grid  RC entry.
 *       The keyword is used to set the value of autoplace-attributes-grid.
 */
SCM g_rc_attribute_placement_grid(SCM offset)
{
  default_attribute_placement_grid = ICHECK( offset,
                                     MIN_AUTOPLACE_GRID,
                                     MAX_AUTOPLACE_GRID,
                                     DEFAULT_ATTRIB_PLACE_GRID,
                                     "attribute-placement-grid");

   x_settings_set_scm_int("autoplace-attributes-grid", default_attribute_placement_grid );

  return SCM_BOOL_T;
}

/*! \brief This function processes the component dialog RC entry.
 *  \par Function Description
 *  This function reads the string list from the component-dialog-attributes
 *  configuration parameter and converts the list into a GList.
 *  The GList is stored in the global default_component_select_attrlist variable.
 */
SCM g_rc_component_dialog_attributes(SCM stringlist)
{
  int length, i;
  GList *list=NULL;
  char *attr;

  SCM_ASSERT(scm_list_p(stringlist), stringlist, SCM_ARG1, "scm_is_list failed");
  length = scm_ilength(stringlist);

  /* If the command is called multiple times, remove the old list before
     recreating it */
  g_list_foreach(default_component_select_attrlist, (GFunc)g_free, NULL);
  g_list_free(default_component_select_attrlist);

  scm_dynwind_begin(0);
  scm_dynwind_unwind_handler(u_glist_free_strings, (void *) &list, 0);

  /* convert the scm list into a GList */
  for (i=0; i < length; i++) {
    char *str;
    SCM elem = scm_list_ref(stringlist, scm_from_int(i));

    SCM_ASSERT(scm_is_string(elem), elem, SCM_ARG1, "list element is not a string");

    str = scm_to_utf8_string(elem);
    attr = u_string_strdup(str);
    free(str);
    list = g_list_prepend(list, attr);
  }

  scm_dynwind_end();

  default_component_select_attrlist = g_list_reverse(list);

  return SCM_BOOL_T;

}
/*! \brief This function processes the continue-component-place RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the continue-component-place RC entry.
 */
SCM g_rc_continue_component_place(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("continue-component-place",
                   default_continue_component_place,
                   2);
}
/*! \brief This function processes the embed-components RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the embed-components RC entry.
 */
SCM g_rc_embed_components(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED}
  };

  RETURN_G_RC_MODE("embed-components",
                   default_embed_components,
                   2);
}
/*! \brief This function processes the enforce-hierarchy RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the enforce-hierarchy RC entry.
 */
SCM g_rc_enforce_hierarchy(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("enforce-hierarchy",
                   default_enforce_hierarchy,
                   2);
}

/*! \brief This function processes the force_boundingbox RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for force_boundingbox RC entry.
 */
SCM g_rc_force_boundingbox(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE,  RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED  }
  };

  RETURN_G_RC_MODE("force-boundingbox",
                   default_force_boundingbox,
                   2);
}

/*! \brief This function is for setting the keyboardpan_gain.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the keyboardpan-gain RC entry.
 */
SCM g_rc_keyboardpan_gain(SCM gain)
{
  default_keyboardpan_gain = ICHECK( gain, MIN_KEYBOARD_GAIN,
                                     MAX_KEYBOARD_GAIN,
                                     DEFAULT_KEYBOARD_GAIN,
                                     "keyboardpan_gain");
  return SCM_BOOL_T;
}
/*! \brief This function processes the magnetic-net-mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the magnetic-net-mode RC entry.
 */
SCM g_rc_magnetic_net_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("magnetic-net-mode",
                   default_magnetic_net_mode,
                   2);
}
/*! \brief This function processes the netconn-rubberband RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the netconn-rubberband RC entry.
 */
SCM g_rc_netconn_rubberband(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("netconn-rubberband",
                   default_netconn_rubberband,
                   2);
}

/*! \brief This function processes the select-slack-pixels RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the select-slack-pixels RC entry.
 */
SCM g_rc_select_slack_pixels(SCM pixels)
{
  default_select_slack_pixels = ICHECK( pixels, 0, -1,
                                DEFAULT_SLACK_PIXELS,
                                "select-slack-pixels");
  return SCM_BOOL_T;
}
/*! \brief This function processes the snap-size RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the snap-size RC entry.
 */
SCM g_rc_snap_size(SCM size)
{
  int val;

  if (scm_is_integer(size)) {
    val = scm_to_int (size);
    if (val == 0) {
     fprintf(stderr, _("Invalid value, snap-size can not be zero, check entry rc file\n"));
     val = DEFAULT_SNAP_SIZE; /* assign default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check snap-size entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_SNAP_SIZE);
    val = DEFAULT_SNAP_SIZE; /* assign default */
  }

  default_snap_size = val;

  return SCM_BOOL_T;
}
/*! \brief This function processes the sort-component-library RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the sort-component-library RC entry.
 */
SCM g_rc_sort_component_library(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("sort_component_library",
                   default_sort_component_library,
                   2);
}

/* ----- Nets and Routing ----- */
/*! \brief This function processes the net-consolidate RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the net-consolidate RC entry.
 */
SCM g_rc_net_consolidate(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("net-consolidate",
                   default_net_consolidate,
                   2);
}
/*! \brief This function processes the net-endpoint-mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the net-endpoint-mode RC entry.
 */
SCM g_rc_net_endpoint_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {NET_NONE,   RC_STR_NET_NONE},
    {EMPTY_BOX,  RC_STR_EMPTY_BOX},
    {FILLED_BOX, RC_STR_FILLED_BOX}
  };

  RETURN_G_RC_MODE("net-endpoint-mode",
                   default_net_endpoint_mode,
                   3);
}

/*! \brief This function processes the net-midpoint-mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the net-midpoint-mode RC entry.
 */
SCM g_rc_net_midpoint_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {NET_NONE,   RC_STR_NET_NONE},
    {EMPTY_BOX,  RC_STR_EMPTY_BOX},
    {FILLED_BOX, RC_STR_FILLED_BOX},
    {FILLED_BOX, "filled"}           /* depreciate */

  };

  RETURN_G_RC_MODE("net-midpoint-mode",
                   default_net_midpoint_mode,
                   4);
}

/*! \brief This function processes the net-direction-mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the net-direction-mode RC entry.
 */
SCM g_rc_net_direction_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED}
  };

  RETURN_G_RC_MODE("net-direction-mode",
                   default_net_direction_mode,
                   2);
}
/*! \brief This function processes the net-selection_mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the net-selection_mode RC entry.
 *
 */
SCM g_rc_net_selection_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {NET_NONE,        RC_STR_DISABLED},
    {NET_SELECT_NET,  "enabled_net"}, /* depreciate */
    {NET_SELECT_ALL , "enabled_all"}, /* depreciate */
    {NET_SELECT_NET,  RC_STR_NET_NET},   /* = "net" */
    {NET_SELECT_ALL,  RC_STR_NET_ALL}    /* = "all" */
  };

  RETURN_G_RC_MODE("net-selection-mode",
                   default_net_selection_mode,
                   5);
}

/* Net Ripper */

/*! \brief This function processes the bus-ripper-rotation RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the bus-ripper-rotation RC entry.
 */
SCM g_rc_bus_ripper_rotation(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {SYMMETRIC,     RC_STR_RIP_SYMMETRIC },
    {NON_SYMMETRIC, RC_STR_RIP_NON_SYMMETRIC  }
  };

  RETURN_G_RC_MODE("bus-ripper-rotation",
                   default_bus_ripper_rotation,
                   2);
}
/*! \brief This function processes the bus-ripper-size RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the bus-ripper-size RC entry.
 */
SCM g_rc_bus_ripper_size(SCM size)
{
  int val;

  if (scm_is_integer(size)) {
    val = scm_to_int (size);
    if (val == 0) {
     fprintf(stderr, _("Invalid value, bus-ripper-size can not be zero, check entry rc file\n"));
     val = DEFAULT_RIPPER_SIZE; /* assign default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check bus-ripper-size entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_RIPPER_SIZE);
    val = DEFAULT_RIPPER_SIZE; /* assign default */
  }

  default_bus_ripper_size = val;
  return SCM_BOOL_T;
}
/*! \brief This function processes the bus-ripper-type RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the bus-ripper-type RC entry.
 */
SCM g_rc_bus_ripper_type(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {COMP_BUS_RIPPER, RC_STR_RIP_COMPONENT },
    {NET_BUS_RIPPER,  RC_STR_RIP_NET }
  };

  RETURN_G_RC_MODE("bus-ripper-type",
                   default_bus_ripper_type,
                   2);
}
/*! \brief This function processes the bus-ripper-symname RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the bus-ripper-symname RC entry.
 *
 *  \param [in] scmsymname
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_bus_ripper_symname(SCM scmsymname)
{
  char *temp;

  SCM_ASSERT (scm_is_string (scmsymname), scmsymname,
              SCM_ARG1, "bus-ripper-symname");

  GEDA_FREE(default_bus_ripper_symname);

  temp = scm_to_utf8_string (scmsymname);
  default_bus_ripper_symname = u_string_strdup (temp);
  free (temp);

  return SCM_BOOL_T;
}

/* Pointer Device, aka Mouse stuff */

/*! \brief This function processes the fast-mousepan RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the fast-mousepan RC entry.
 * Depreciate!
 */
SCM g_rc_fast_mousepan(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("fast-mousepan",
                   default_fast_mousepan,
                   2);
}

/*! \brief This function processes the drag-can-move RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the drag-can-move RC entry.
 * Depreciate!
 */
SCM g_rc_drag_can_move(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE,  RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED  }
  };

  RETURN_G_RC_MODE("drag-can-move",
                   default_drag_can_move,
                   2);
}

/*! \brief This function processes the middle-button RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the middle-button RC entry.
 *  Depreciate!
 */
SCM g_rc_middle_button(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {MOUSE_MIDDLE_STROKE, RC_STR_MID_STROKE},
    {MOUSE_MIDDLE_REPEAT, RC_STR_MID_REPEAT},
    {MOUSE_MIDDLE_ACTION, RC_STR_MID_ACTION},
    {MOUSE_MIDDLE_PAN,    RC_STR_MID_MOUSEPAN},
  };

  RETURN_G_RC_MODE("middle-button",
                   default_middle_button,
                   4);
}
/*! \brief This function processes the mousepan-gain RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the mousepan-gain RC entry.
 * Depreciate!
 */
SCM g_rc_mousepan_gain(SCM gain)
{
  default_mousepan_gain = ICHECK(gain, 0, -1, DEFAULT_MOUSEPAN_GAIN,
                         "mousepan-gain");
  return SCM_BOOL_T;
}

/*! \brief This function processes the scroll-wheel RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the scroll-wheel RC entry.
 * Depreciate!
 */
/* Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 09/05/12 |  Reversed logical, scroll_wheel = 0 = GTK,
 *                                     scroll_wheel = 1 = Classic
 *                   (because this seems more intuitive.)
*/
SCM g_rc_scroll_wheel(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {SCROLL_WHEEL_GTK,     RC_STR_SCROLL_GTK},
    {SCROLL_WHEEL_CLASSIC, RC_STR_SCROLL_CLASSIC}
  };

  RETURN_G_RC_MODE("scroll-wheel",
                   default_scroll_wheel,
                   2);
}

/*! \brief This function processes the pointer-hscroll RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the pointer-hscroll RC entry.
 * Depreciate!
 */
SCM g_rc_pointer_hscroll(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("pointer_hscroll",
                   default_pointer_hscroll,
                   2);
}

/*! \brief This function processes the third-button RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the third-button RC entry.
 * Depreciate!
 */
SCM g_rc_third_button(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {POPUP_ENABLED   , RC_STR_3RD_POPUP},
    {MOUSEPAN_ENABLED, RC_STR_3RD_PAN},
  };

  RETURN_G_RC_MODE("third-button",
                   default_third_button,
                   2);
}

/* ----- Print Related ----- */

/*! \brief This function processes the image-size RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the image-size RC entry.
 */
SCM g_rc_image_size(SCM width, SCM height)
{
  /* yes this is legit, we are casting the resulting double to an int */

  default_image_width = ICHECK(width, MIN_PAPER_DIMENSION, -1,
                         DEFAULT_PAPER_WIDTH,
                         "image_size");

  default_image_height = ICHECK(height, MIN_PAPER_DIMENSION, -1,
                         DEFAULT_PAPER_HEIGHT,
                         "image_size");

  return SCM_BOOL_T;
}
/*! \brief This function processes the paper-size RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the paper-size RC entry.
 */
SCM g_rc_paper_size(SCM width, SCM height)
#define FUNC_NAME "paper-size"
{
  SCM_ASSERT (SCM_NIMP (width) && SCM_REALP (width), width,
              SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (height) && SCM_REALP (height), height,
              SCM_ARG2, FUNC_NAME);

  /* yes this is legit, we are casting the resulting double to an int */
  default_paper_width  = (int) (scm_to_double (width)  * MILS_PER_INCH);
  default_paper_height = (int) (scm_to_double (height) * MILS_PER_INCH);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*! \brief This function processes the paper-sizes RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the paper-sizes RC entry. Note this
 *       is plural from the previous entry.
 */
SCM g_rc_paper_sizes(SCM scm_papername, SCM scm_width, SCM scm_height)
#define FUNC_NAME "paper-sizes"
{
  int width;
  int height;
  char *papername;
  SCM ret;

  SCM_ASSERT (scm_is_string (scm_papername), scm_papername,
              SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (scm_width) && SCM_REALP (scm_width), scm_width,
              SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (scm_height) && SCM_REALP (scm_height), scm_height,
              SCM_ARG3, FUNC_NAME);

  width  = (int) (scm_to_double (scm_width)  * MILS_PER_INCH);
  height = (int) (scm_to_double (scm_height) * MILS_PER_INCH);
  papername = scm_to_utf8_string (scm_papername);

  if (!s_papersizes_uniq(papername)) {
    ret = SCM_BOOL_F;
  } else {
    s_papersizes_add_entry(papername, width, height);
    ret = SCM_BOOL_T;
  }

  free(papername);
  return ret;
}

#undef FUNC_NAME
/*! \brief This function processes the print-command RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the print-command RC entry.
 */
SCM g_rc_print_command(SCM scm_command)
#define FUNC_NAME "print-command"
{
  char *command;

  SCM_ASSERT (scm_is_string (scm_command), scm_command,
              SCM_ARG1, FUNC_NAME);

  command = scm_to_utf8_string (scm_command);

  GEDA_FREE (default_print_command);
  default_print_command = u_string_strdup (command);
  free (command);

  return SCM_BOOL_T;
}
#undef FUNC_NAME
/*! \brief This function processes the output-type RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the output-type RC entry.
 *  \todo this keyword needs a better name ...
 */
SCM g_rc_output_type(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {WINDOW,  "current window" },
    {EXTENTS, "limits" },  /* deprecated */
    {EXTENTS, "extents" },
    {EXTENTS_NOMARGINS, "extents no margins" },
  };

  RETURN_G_RC_MODE("output-type",
                   default_print_output_type,
                   4);
}
/*! \brief This function processes the output-orientation RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the output-orientation RC entry.
 */
SCM g_rc_output_orientation(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {PORTRAIT , "portrait" },
    {LANDSCAPE, "landscape"},
  };

  RETURN_G_RC_MODE("output-orientation",
                   default_print_orientation,
                   2);
}
/*! \brief This function processes the output-color RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the output-color RC entry.
 */
SCM g_rc_output_color(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  /* this variable is inconsistantly named with the rest */
  RETURN_G_RC_MODE("output-color",
                   default_print_color,
                   2);
}
/*! \brief This function processes the output-capstyle RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the output-capstyle RC entry.
 */
SCM g_rc_output_capstyle(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {BUTT_CAP ,  "butt" },
    {ROUND_CAP , "round" },
    {SQUARE_CAP, "square"},
  };

  RETURN_G_RC_MODE("output-capstyle",
                   default_print_output_capstyle,
                   3);
}
/*! \brief This function processes the setpagedevice-orientation RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the setpagedevice-orientation RC entry.
 */
SCM g_rc_setpagedevice_orientation(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("setpagedevice-orientation",
                   default_setpagedevice_orientation,
                   2);
}
/*! \brief This function processes the setpagedevice-pagesize RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the setpagedevice-pagesize RC entry.
 */
SCM g_rc_setpagedevice_pagesize(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("setpagedevice-pagesize",
                   default_setpagedevice_pagesize,
                   2);
}

/* ----------------- System -----------------*/

/** \defgroup System-User-RC-Options RC keywork Handler Functions
 *  @{ \par
 *      These function are registered with Guile and are
 *      called when ever a System type key-word is encountered
 *      when processing RC files.
 */

/*! \brief This function processes the file-preview RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the file-preview RC entry.
 */
SCM g_rc_file_preview(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  /* this variable is inconsistantly named with the rest */
  RETURN_G_RC_MODE("file-preview",
                   default_file_preview,
                   2);
}
/*! \brief This function processes the handleboxes RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the handleboxes RC entry.
 */
SCM g_rc_handleboxes(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("handleboxes",
                   default_handleboxes,
                   2);
}
/*! \brief This function processes the raise-dialog-boxes-on-expose RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the raise-dialog-boxes-on-expose RC entry.
 */
SCM g_rc_raise_dialog_boxes_on_expose(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("raise-dialog-boxes-on-expose",
                   default_raise_dialog_boxes,
                   2);
}

/*! \brief This function processes the save-ui-settings RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the save_settings RC entry.
 */
SCM g_rc_save_ui_settings(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("save-ui-settings",
                   default_save_ui_settings,
                   2);
}

/*! \brief This function processes the toolbars RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the toolbars RC entry.
 */
SCM g_rc_toolbars(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("toolbars",
                   default_toolbars,
                   2);
}
/*! \brief This function processes the toolbars-mode RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the toolbars-mode RC entry.
 */
SCM g_rc_toolbars_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TOOLBAR_SHOW_ICONS, RC_STR_TB_ICONS },
    {TOOLBAR_SHOW_TEXT,  RC_STR_TB_TEXT  },
    {TOOLBAR_SHOW_BOTH,  RC_STR_TB_BOTH  },
    {TOOLBAR_SHOW_HORIZ, RC_STR_TB_HORIZ },
    {TOOLBAR_RETENTION,  RC_STR_TB_LAST  }
  };

  RETURN_G_RC_MODE("toolbars-mode",
                   default_toolbars_mode,
                   5);
}

/* --------------- Scrollbar ----------------*/

/** \defgroup RC-Scrollbar-Options Scrollbar Options RC Functions
 *  @{
 */

/*! \brief This function processes the scrollbars RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the scrollbars RC entry.
 */
SCM g_rc_scrollbars(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("scrollbars",
                   default_scrollbars,
                   2);
}

/*! \brief This function processes the scrollbar-update RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the scrollbar-update RC entry.
 */
SCM g_rc_scrollbar_update(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {DISPLAY_CONTINUOUS, RC_STR_BARS_CONTINUOUS },
    {DISPLAY_DELAYED,    RC_STR_BARS_DELAYED }
  };

 RETURN_G_RC_MODE("scrollbar-update",
                   default_scrollbar_update,
                   2);
}

/*! \brief This function processes the scrollbars-visible RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the scrollbars-visible RC entry.
 */
SCM g_rc_scrollbars_visible(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("scrollbars-visible",
                   default_scrollbars_visible,
                   2);
}

/*! \brief This function processes the scrollpan_steps RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the scrollpan_steps RC entry.
 * Depreciated!
 */
SCM g_rc_scrollpan_steps(SCM steps)
{
  int val;

  if (scm_is_integer(steps)) {
    val = scm_to_int (steps);
   /* Allow negative numbers in case the user wishes to reverse scroll
    * direction, but don't allow zero steps as this would result in a
    * division-by-zero error */
    if (val == 0) {
     fprintf(stderr, _("Can not scroll 0 steps, invalid scrollpan-steps, check entry in rc file\n"));
     val = DEFAULT_SCROLLPAN_STEPS; /* default */
    }
  }
  else {
    fprintf (stderr, _("Invalid type assignment, check scrollpan-steps entry in rc file\n"));
    fprintf (stderr, _("Continuing with default value=[%d]\n"), DEFAULT_SCROLLPAN_STEPS);
    val = DEFAULT_SCROLLPAN_STEPS; /* default value*/
  }

  default_scrollpan_steps = val;

  return SCM_BOOL_T;
}

/** @} END Group RC-Scrollbar-Options Functions */

/* ----- Text Related ----- */
/*! \brief This function processes the text-case RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the text-case RC entry.
 */
SCM g_rc_text_case(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {LOWER_CASE, RC_STR_TEXT_LOWER},
    {UPPER_CASE, RC_STR_TEXT_UPPER},
    {BOTH_CASES, RC_STR_TEXT_BOTH }
  };

  RETURN_G_RC_MODE("text-case",
                   default_text_case,
                   3);
}
/*! \brief This function processes the text-display_zoomfactor RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the text_display_zoomfactor RC entry.
 */
SCM g_rc_text_display_zoomfactor(SCM zoomfactor)
{
  int val;

  SCM_ASSERT (scm_is_integer (zoomfactor), zoomfactor,
              SCM_ARG1, "test-display-zoom-factor");

  val = scm_to_int (zoomfactor);
  if (val < MIN_TEXT_ZOOM) {
    fprintf(stderr,
            _("Invalid zoomfactor [%d] passed to %s\n"),
            val,
            "text-display-zoom-factor");
    val = DEFAULT_TEXT_ZOOM; /* default */
  }

  default_text_display_zoomfactor = val;

  return SCM_BOOL_T;
}
/*! \brief This function processes the text-feedback RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the text_feedback RC entry.
 */
SCM g_rc_text_feedback(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {ONLY_WHEN_READABLE, RC_STR_TXT_READABLE},
    {ALWAYS_FEEDBACK,    RC_STR_TXT_ALWAYS  }
  };

  RETURN_G_RC_MODE("text-feedback",
                   default_text_feedback,
                   2);
}
/*! \brief This function processes the text-origin-marker RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the text_origin_marker RC entry.
 */
SCM g_rc_text_origin_marker(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("text-origin-marker",
                   default_text_origin_marker,
                   2);
}
/*! \brief This function processes the text-marker-size RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the text_origin_marker RC entry.
 */
SCM g_rc_text_marker_size(SCM size)
{
  default_text_marker_size = ICHECK( size, MIN_TEXT_MARKER_SIZE,
                                           MAX_TEXT_MARKER_SIZE,
                                           DEFAULT_TEXT_MARKER_SIZE,
                                           "text-marker-size");

  return SCM_BOOL_T;
}
/*! \brief This function processes the text-size RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the text-size RC entry.
 */
SCM g_rc_text_size(SCM size)
{
  default_text_size = ICHECK( size, MIN_TEXT_SIZE, -1,
                      DEFAULT_TEXT_SIZE, "text-size");

  return SCM_BOOL_T;
}
/* ----- Undo System ----- */
/*! \brief This function processes the undo-control RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the undo-control RC entry.
 */
SCM g_rc_undo_control(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED},
  };

  RETURN_G_RC_MODE("undo-control", default_undo_control, 2);
}
/*! \brief This function processes the undo-levels RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the undo-levels RC entry.
 */
SCM g_rc_undo_levels(SCM levels)
{
  default_undo_levels = ICHECK(levels, 0, -1, DEFAULT_UNDO_LEVELS,
                         "undo-levels");
  return SCM_BOOL_T;
}

/*! \brief This function processes the undo-panzoom RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the undo-panzoom RC entry.
 */
SCM g_rc_undo_panzoom(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED}
  };

  RETURN_G_RC_MODE("undo-panzoom", default_undo_panzoom, 2);
}

/*! \brief This function processes the undo-preserve RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the undo-preserve RC entry.
 */
SCM g_rc_undo_preserve(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , RC_STR_ENABLED },
    {FALSE, RC_STR_DISABLED}
  };

  RETURN_G_RC_MODE("undo-preserve", default_undo_preserve, 2);
}

/*! \brief This function processes the undo-type RC entry.
 *  \par Function Description
 *       C function to dynamically convert lisp variable while
 *       processing configuration data for the undo-type RC entry.
 */
SCM g_rc_undo_type(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {UNDO_NONE  , RC_STR_UNDO_NONE},
    {UNDO_DISK  , RC_STR_UNDO_DISK},
    {UNDO_MEMORY, RC_STR_UNDO_MEMORY}
  };

  RETURN_G_RC_MODE("undo-type",
                   default_undo_type,
                   3);
}

/** @} end group System-User-RC-Options */
