/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */
#include "../../../config.h"
#include <stdio.h>

#include <libgeda_priv.h>

#include <geda_debug.h>

char  *default_bitmap_directory          = NULL;
char  *default_log_directory             = NULL;
char  *default_postscript_prolog         = NULL;
char  *default_untitled_name             = NULL;

GList *default_always_promote_attributes = NULL;
GList *default_component_groups          = NULL;

int   default_attribute_promotion        = TRUE;
int   default_promote_invisible          = FALSE;
int   default_keep_invisible             = TRUE;

int   default_check_symbol_version       = TRUE;
int   default_component_style            = RC_NIL;
int   default_enable_style_zero          = TRUE;

int   default_make_backup_files          = TRUE;
int   default_show_full_path             = FALSE;

/* Net Styles*/
int   default_bus_style        = DEFAULT_BUS_STYLE;
int   default_line_style       = DEFAULT_LINE_STYLE;
int   default_net_style        = DEFAULT_NET_STYLE;
int   default_pin_style        = DEFAULT_PIN_STYLE;

int   default_thick_bus_width  = DEFAULT_THICK_BUS_WIDTH;
int   default_thick_line_width = DEFAULT_THICK_LINE_WIDTH;
int   default_thick_net_width  = DEFAULT_THICK_NET_WIDTH;
int   default_thick_pin_width  = DEFAULT_THICK_PIN_WIDTH;

int   default_thin_bus_width   = DEFAULT_THIN_BUS_WIDTH;
int   default_thin_line_width  = DEFAULT_THIN_LINE_WIDTH;
int   default_thin_net_width   = DEFAULT_THIN_NET_WIDTH;
int   default_thin_pin_width   = DEFAULT_THIN_PIN_WIDTH;

int   default_line_end         = END_NONE;
int   default_line_type        = TYPE_SOLID;
int   default_line_width       = 0;
int   default_line_length      = 100;
int   default_line_space       = 100;

int   default_fill_type        = FILLING_HOLLOW;
int   default_fill_width       = 0;
int   default_fill_pitch1      = 50;
int   default_fill_angle1      = 45;
int   default_fill_pitch2      = 50;
int   default_fill_angle2      = 135;

/*!
 * \brief Initialize variables in a GedaToplevel object
 * \par Function Description
 *  This function will initialize variables to default values,
 *  which may have been assigned values by RC handlers.
 *
 * \param [out] toplevel  The GedaToplevel object to be updated.
 */
void geda_iface_vars_set(GedaToplevel *toplevel)
{
  GList *iter;

  toplevel->attribute_promotion   = default_attribute_promotion;
  toplevel->promote_invisible     = default_promote_invisible;
  toplevel->keep_invisible        = default_keep_invisible;

  toplevel->check_symbol_version  = default_check_symbol_version;
  toplevel->make_backup_files     = default_make_backup_files;
  toplevel->show_full_path        = default_show_full_path;

  toplevel->bus_style             = default_bus_style;
  toplevel->net_style             = default_net_style;
  toplevel->pin_style             = default_pin_style;
  toplevel->line_style            = default_line_style;

  toplevel->thick_bus_width       = default_thick_bus_width;
  toplevel->thick_line_width      = default_thick_line_width;
  toplevel->thick_net_width       = default_thick_net_width;
  toplevel->thick_pin_width       = default_thick_pin_width;

  toplevel->thin_bus_width        = default_thin_bus_width;
  toplevel->thin_line_width       = default_thin_line_width;
  toplevel->thin_net_width        = default_thin_net_width;
  toplevel->thin_pin_width        = default_thin_pin_width;

  toplevel->default_line_end      = default_line_end;
  toplevel->default_line_width    = default_line_width;
  toplevel->default_line_space    = default_line_space;

  toplevel->default_fill_width    = default_fill_width;
  toplevel->default_fill_pitch1   = default_fill_pitch1;
  toplevel->default_fill_angle1   = default_fill_angle1;
  toplevel->default_fill_pitch2   = default_fill_pitch2;
  toplevel->default_fill_angle2   = default_fill_angle2;

  /* copy the always_promote_attributes list from the default */
  geda_glist_free_full(toplevel->always_promote_attributes, g_free);

  toplevel->always_promote_attributes = g_list_copy(default_always_promote_attributes);

  for (iter = default_component_groups; iter != NULL; iter = g_list_next(iter))
  {
    toplevel->component_groups = g_list_append(toplevel->component_groups,
                                               geda_strdup(iter->data));
  }

  for (iter = toplevel->always_promote_attributes; iter != NULL;
       iter = g_list_next(iter)) {
    iter->data = geda_utility_string_strdup(iter->data);
  }

  /* Cannot free the default* strings here since new windows */
  /* need them */
  INIT_STR(toplevel, untitled_name,      DEFAULT_UNTITLED_NAME);
  INIT_STR(toplevel, bitmap_directory,   DEFAULT_BITMAP_DIRECTORY);
  INIT_STR(toplevel, postscript_prolog,  DEFAULT_POSTSCRIPT_PROLOG);
}


/*!
 * \brief Free default names
 * \par Function Description
 *  This function will free all of the default variables for libgeda.
 */
void geda_iface_vars_freenames()
{
  GList *iter;

  GEDA_FREE(default_bitmap_directory);
  GEDA_FREE(default_log_directory)
  GEDA_FREE(default_postscript_prolog);
  GEDA_FREE(default_untitled_name);

  geda_glist_free_full(default_always_promote_attributes, g_free);

  default_always_promote_attributes = NULL;

  for (iter = default_component_groups; iter != NULL; iter = g_list_next(iter)){
    GEDA_FREE(iter->data);
  }

  g_list_free(default_component_groups);
  default_component_groups = NULL;
}
