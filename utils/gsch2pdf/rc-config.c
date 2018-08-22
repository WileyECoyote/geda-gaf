/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2014 Ales Hvezda
 * Copyright (C) 2002-2014 gEDA Contributors (see ChangeLog for details)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if  not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "../include/common.h"
#include "print-settings.h"
#include "rc-config.h"

#include <geda_debug.h>

static PrintSettings *settings = NULL;

static SCM rc_config_font(SCM string)
{
  char *font_string = scm_to_locale_string(string);

  print_settings_set_font(settings, font_string);

  free(font_string);

  return SCM_BOOL_T;
}

static SCM rc_config_junction_size_bus(SCM size)
{
  print_settings_set_junction_size_bus(settings, scm_to_double(size));

  return SCM_BOOL_T;
}

static SCM rc_config_junction_size_net(SCM size)
{
  print_settings_set_junction_size_net(settings, scm_to_double(size));

  return SCM_BOOL_T;
}

static SCM rc_config_output_orientation(SCM orientation)
{
  char *orientation_string = scm_to_locale_string(orientation);

  if (geda_stricmp(orientation_string, "portrait") == 0) {
    print_settings_set_print_orientation(settings, PRINT_ORIENTATION_PORTRAIT);
  }
  else if (geda_stricmp(orientation_string, "landscape") == 0) {
    print_settings_set_print_orientation(settings, PRINT_ORIENTATION_LANDSCAPE);
  }
  else {
    print_settings_set_print_orientation(settings, PRINT_ORIENTATION_OTHER);
  }

  free(orientation_string);

  return SCM_BOOL_T;
}

static SCM rc_config_page_align(SCM halign, SCM valign)
{
  print_settings_set_page_align_horizontal(settings, scm_to_double(halign));
  print_settings_set_page_align_vertical(settings, scm_to_double(valign));

  return SCM_BOOL_T;
}

static SCM rc_config_page_margins(SCM left, SCM top, SCM right, SCM bottom)
{
  print_settings_set_page_margin_left(settings, scm_to_double(left));
  print_settings_set_page_margin_top(settings, scm_to_double(top));
  print_settings_set_page_margin_right(settings, scm_to_double(right));
  print_settings_set_page_margin_bottom(settings, scm_to_double(bottom));

  return SCM_BOOL_T;
}

static SCM rc_config_paper_size(SCM width, SCM height)
{
  print_settings_set_paper_width(settings, scm_to_double(width));
  print_settings_set_paper_height(settings, scm_to_double(height));

  return SCM_BOOL_T;
}

/*! \brief Initialize this module
 */
void rc_config_init()
{
  scm_c_define_gsubr("font", 1, 0, 0, rc_config_font);
  scm_c_define_gsubr("junction-size-bus", 1, 0, 0, rc_config_junction_size_bus);
  scm_c_define_gsubr("junction-size-net", 1, 0, 0, rc_config_junction_size_net);
  scm_c_define_gsubr("output-orientation", 1, 0, 0, rc_config_output_orientation);
  scm_c_define_gsubr("page-align", 2, 0, 0, rc_config_page_align);
  scm_c_define_gsubr("page-margins", 4, 0, 0, rc_config_page_margins);
  scm_c_define_gsubr("paper-size", 2, 0, 0, rc_config_paper_size);
}


/*! \brief Set the object to store settings from the rc file
 *
 *  \param [in] print_settings The object to store print settings in.
 */
void rc_config_set_print_settings(PrintSettings *print_settings)
{
  settings = print_settings;
}

