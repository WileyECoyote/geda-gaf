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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "../../../config.h"
#include <stdio.h>

#include <libgeda_priv.h>

/*! \brief */
static struct gsubr_t rc_func_table[] = {
  { "eval-protected",             1, 1, 0, g_evaluate_scm_protected },
  { "eval-string-protected",      1, 0, 0, g_evaluate_scm_string_protected },

  { "component-groups",           1, 0, 0, g_rc_component_groups },
  { "component-library",          1, 1, 0, g_rc_component_library },
  { "component-library-command",  3, 0, 0, g_rc_component_library_command },
  { "component-library-funcs",    3, 0, 0, g_rc_component_library_funcs },
  { "component-search-directory", 1, 1, 0, g_rc_component_search_directory },
  { "source-library",             1, 0, 0, g_rc_source_library },
  { "source-library-search",      1, 0, 0, g_rc_source_library_search },

  { "reset-component-library",    0, 0, 0, g_rc_reset_component_library },
  { "reset-source-library",       0, 0, 0, g_rc_reset_source_library },

  { "net-style",                  1, 0, 0, g_rc_net_style },
  { "bus-style",                  1, 0, 0, g_rc_bus_style },
  { "pin-style",                  1, 0, 0, g_rc_pin_style },
  { "line-style",                 1, 0, 0, g_rc_line_style },

  { "thick-bus-width",            1, 0, 0, g_rc_thick_bus_width },
  { "thick-line-width",           1, 0, 0, g_rc_thick_line_width },
  { "thick-net-width",            1, 0, 0, g_rc_thick_net_width },
  { "thick-pin-width",            1, 0, 0, g_rc_thick_pin_width },
  { "thin-bus-width",             1, 0, 0, g_rc_thin_bus_width },
  { "thin-line-width",            1, 0, 0, g_rc_thin_line_width },
  { "thin-net-width",             1, 0, 0, g_rc_thin_net_width },
  { "thin-pin-width",             1, 0, 0, g_rc_thin_pin_width },

  { "attribute-promotion",        0, 1, 0, g_rc_attribute_promotion },
  { "promote-invisible",          0, 1, 0, g_rc_promote_invisible },
  { "keep-invisible",             0, 1, 0, g_rc_keep_invisible },
  { "always-promote-attributes",  1, 0, 0, g_rc_always_promote_attributes },

  { "bitmap-directory",           0, 1, 0, g_rc_bitmap_directory },
  { "log-directory",              1, 0, 0, g_rc_log_directory },
  { "scheme-directory",           1, 0, 0, g_rc_scheme_directory },

  { "check-symbol-version",       1, 0, 0, g_rc_check_symbol_version },
  { "log-time",                   1, 0, 0, g_rc_log_time },
  { "postscript-prolog",          1, 0, 0, g_rc_postscript_prolog },
  { "make-backup-files",          1, 0, 0, g_rc_make_backup_files },
  { "rc-filename",                0, 0, 0, g_rc_parse_rc_filename },
  { "rc-config",                  0, 0, 0, g_rc_parse_rc_config },
  { "show-full-path",             1, 0, 0, g_rc_show_full_path },
  { "untitled-name",              1, 0, 0, g_rc_untitled_name },
  { NULL,                         0, 0, 0, NULL } };

/*! \brief Register all libgeda functions with scheme.
 *  \par Function Description
 *  Creates g_subr_t objects to make g_rc_* functions that are defined
 *  in g_rc.c visible to Scheme.
 */
void g_register_rc_handlers (void)
{
  struct gsubr_t *tmp = rc_func_table;

  while (tmp->name != NULL) {
    scm_c_define_gsubr (tmp->name, tmp->req, tmp->opt, tmp->rst, tmp->func);
    tmp++;
  }
}

/*! \brief Register some libgeda directories with Scheme.
 * \par Function Description
 * Ensures that the default gEDA Scheme directory is added to the
 * Guile load path.
 */
void g_register_libgeda_dirs (void)
{
  char *scheme_dir;

  scheme_dir = g_build_filename (geda_sys_data_path (), "scheme", NULL);

  g_rc_scheme_directory (scm_from_utf8_string (scheme_dir));

  GEDA_FREE (scheme_dir);
}
