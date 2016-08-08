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
 * published by the Free Software Foundation; either version 3 of
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
 *  \todo functions names in the file subdirectories are still
          using f_prefix
 */

#ifndef __GEDA_FILE_MAC__
#define __GEDA_FILE_MAC__

/* f_file.c */
#define geda_close_file           f_close
#define geda_file_has_autosave    f_has_active_autosave
#define geda_open_file            f_open
#define geda_open_flags           f_open_flags
#define geda_remove_backup_file   f_remove_backup_file
#define geda_save_file            f_save

/* f_get.c */
#define geda_get_autosave_name    f_get_autosave_filename
#define geda_get_basename         f_get_basename
#define geda_get_basename_dup     f_get_basename_dup
#define geda_get_bitmap_spec      f_get_bitmap_filespec
#define geda_get_data_spec        f_get_data_filespec
#define geda_get_dir_list         f_get_dir_list_files
#define geda_get_file_contents    f_get_file_contents
#define geda_get_extension        f_get_filename_ext
#define geda_get_format_header    f_get_format_header
#define geda_is_path_absolute     f_get_is_path_absolute

/* f_path.c */
#define geda_create_path          f_path_create
#define geda_free_path            f_path_free
#define geda_get_dirname          f_path_get_dirname
#define geda_sys_data_path        f_path_sys_data
#define geda_sys_doc_path         f_path_sys_doc
#define geda_sys_config_path      f_path_sys_config
#define geda_user_config_path     f_path_user_config

/* f_print.c */
#define geda_print_file           f_print_file
#define geda_print_command        f_print_command;
#define geda_print_stream         f_print_stream
#define geda_set_print_type       f_print_set_type

/* f_sys.c */
#define geda_copy_file            f_sys_copy
#define geda_cmp_file_mod_time    f_sys_cmp_mod_time
#define geda_follow_symlinks      f_sys_follow_symlinks
#define geda_normalize_name       f_sys_normalize_name
#define geda_remove_file          f_sys_remove
#define geda_remove_extension     f_sys_remove_extension

#endif /* __GEDA_FILE_MAC__ */
