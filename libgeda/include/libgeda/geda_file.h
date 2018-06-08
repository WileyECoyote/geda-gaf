/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_file.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 Wiley Edward Hill
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: March, 06, 2016
 */
/*! \file geda_file.h "libgeda/geda_file.h"
 *  \brief Libgeda macros for file modules.
 *  Alias macros for public functions in the src/files modules.
 *  These are used interchangeably with the function name, and
 *  are offered as "shorter" alternate version.
 */

#ifndef __GEDA_FILE_MAC__
#define __GEDA_FILE_MAC__

/* f_file.c */
#define geda_close_file           geda_file_close
#define geda_file_has_autosave    geda_file_has_active_autosave
#define geda_open_file            geda_file_open
#define geda_open_flags           geda_file_open_flags
#define geda_remove_backup_file   geda_file_remove_backup
#define geda_save_file            geda_file_save

/* f_get.c */
#define geda_get_autosave_name    geda_file_get_autosave_filename
#define geda_get_basename         geda_file_get_basename
#define geda_get_basename_dup     geda_file_get_basename_dup
#define geda_get_bitmap_spec      geda_file_get_bitmap_filespec
#define geda_get_data_spec        geda_file_get_data_filespec
#define geda_get_dir_list         geda_file_get_dir_list_files
#define geda_get_file_contents    geda_file_get_contents
#define geda_get_extension        geda_file_get_filename_ext
#define geda_get_format_header    geda_file_get_format_header
#define geda_get_file_name        geda_file_get_name

#define geda_is_path_absolute     geda_file_get_is_path_absolute

/* f_path.c */
#define geda_create_path          geda_file_path_create
#define geda_free_path            geda_file_path_free
#define geda_get_dirname          geda_file_path_get_dirname
#define geda_sys_data_path        geda_file_path_sys_data
#define geda_sys_doc_path         geda_file_path_sys_doc
#define geda_sys_config_path      geda_file_path_sys_config
#define geda_user_config_path     geda_file_path_user_config

/* f_print.c */
#define geda_print_file           geda_file_print_file
#define geda_print_command        geda_file_print_command;
#define geda_print_stream         geda_file_print_stream
#define geda_set_print_type       geda_file_print_set_type

/* f_sys.c */
#define geda_copy_file            geda_file_copy
#define geda_ckmod                geda_file_sys_ckmod
#define geda_cmp_file_mod_time    geda_file_sys_cmp_mod_time
#define geda_follow_symlinks      geda_file_sys_follow_symlinks
#define geda_normalize_filename   geda_file_sys_normalize_name
#define geda_remove_file          geda_file_sys_remove
#define geda_remove_extension     geda_file_sys_remove_extension

#endif /* __GEDA_FILE_MAC__ */
