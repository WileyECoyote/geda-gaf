/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_utility.h
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
 *  Date Contributed: March, 6th, 2016
 */
/*! \file geda_utility.h "libgeda/geda_utility.h"
 *  \brief Libgeda macros for utility modules.
 *
 */

#ifndef __GEDA_UTILITIES_MAC__
#define __GEDA_UTILITIES_MAC__

#define geda_glist_concat(l1, l2) g_list_concat (l1, (GList*)l2)
#define geda_glist_length(l)      (l ? g_list_length ((GList*)l) : -1)

/* u_utility.c */
#define geda_expand_env_variable  geda_utility_expand_env_variable
#define geda_print_object         geda_utility_program_print_object

/* u_glist.c */
#define geda_clear_glist          geda_utility_glist_clear
#define geda_glist_find_string    geda_utility_glist_find_string
#define geda_glist_free_all       geda_utility_glist_free_all
#define geda_glist_free_full      geda_utility_glist_free_full
#define geda_glist_str_inlist     geda_utility_glist_str_inlist
#define geda_glist_stri_inlist    geda_utility_glist_stri_inlist

#define geda_clear_gslist         geda_utility_gslist_clear
#define geda_gslist_find_string   geda_utility_gslist_find_string
#define geda_gslist_free_all      geda_utility_gslist_free_all
#define geda_gslist_free_full     geda_utility_gslist_free_full
#define geda_gslist_str_inlist    geda_utility_gslist_str_inlist
#define geda_gslist_stri_inlist   geda_utility_gslist_stri_inlist

/* u_log.c */
#define geda_close_log            geda_utility_log_close
#define geda_get_log_time         geda_utility_log_get_log_time
#define geda_get_quiet_mode       geda_utility_log_get_quiet_mode
#define geda_get_verbose_mode     geda_utility_log_get_verbose_mode
#define geda_init_log             geda_utility_log_init
#define geda_log_q                geda_utility_log_quite
#define geda_read_log             geda_utility_log_read
#define geda_set_default_logger   geda_utility_log_set_default_handler
#define geda_set_log_time         geda_utility_log_set_log_time
#define geda_set_quiet_mode       geda_utility_log_set_quiet_mode
#define geda_set_log_update_func  geda_utility_log_set_update_func
#define geda_set_verbose_mode     geda_utility_log_set_verbose_mode
#define geda_log_s                geda_utility_log_system
#define geda_log_v                geda_utility_log_verbose

/* Seems out of place, but maybe appropriate */
#define geda_log(format...)   g_log (G_LOG_DOMAIN, G_LOG_LEVEL_MESSAGE, format)
#define geda_log_w(format...) g_log (G_LOG_DOMAIN, G_LOG_LEVEL_WARNING, format)

/* u_string.c */
#define geda_strconcat            geda_utility_string_concat
#define geda_get_utf8             geda_utility_string_get_valid_utf8
#define geda_string_int2str       geda_utility_string_int2str
#define geda_string_isalnum       geda_utility_string_isalnum
#define geda_string_istr          geda_utility_string_istr
#define geda_string_parse_xy      geda_utility_string_parse_xy
#define geda_remove_last_newline  geda_utility_string_remove_last_nl
#define geda_remove_newline       geda_utility_string_remove_nl
#define geda_string_scm2c         geda_utility_string_scm2c
#define geda_sort_string_array    geda_utility_string_sort_array
#define geda_strsplit             geda_utility_string_split
#define geda_sprintf              geda_utility_string_sprintf
#define geda_strdup               geda_utility_string_strdup
#define geda_strequal             geda_utility_string_strequal
#define geda_stricmp              geda_utility_string_stricmp
#define geda_stristr              geda_utility_string_stristr
#define geda_strisubst            geda_utility_string_strisubst
#define geda_strncmpi             geda_utility_string_strncmpi
#define geda_strndup              geda_utility_string_strndup
#define geda_strsize              geda_utility_string_strsize
#define geda_strstr_rep           geda_utility_string_strstr_rep
#define geda_strsubst             geda_utility_string_strsubst
#define geda_strtrim              geda_utility_string_strtrim
#define geda_word_count           geda_utility_string_word_count

/* u_program.c */
#define geda_program_backtrace    geda_utility_program_backtrace
#define geda_malloc               geda_utility_program_mem_alloc
#define geda_calloc               geda_utility_program_mem_calloc
#define geda_free                 geda_utility_program_mem_free
#define geda_set_memory_vtable    geda_utility_program_mem_set_vtable

/* u_refdes.c */
#define geda_get_ieee_refdes      geda_utility_refdes_get_ieee
#define geda_get_spice_refdes     geda_utility_refdes_get_spice
#define geda_get_standard_refdes  geda_utility_refdes_get_standard
#define geda_reset_refdes         geda_utility_refdes_reset
#define geda_refdes_get_numeric   geda_utility_refdes_return_numeric
#define geda_refdes_get_prefix    geda_utility_refdes_return_prefix

#endif /* __GEDA_UTILITIES_MAC__ */
