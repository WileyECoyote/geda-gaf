/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_hierarchy.h
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
/*! \file geda_hierarchy.h "libgeda/geda_hierarchy.h"
 *  \brief Libgeda macros for geda_struct_hierarchy module.
 */

#ifndef __GEDA_HIERARCHY_MAC__
#define __GEDA_HIERARCHY_MAC__

#define geda_hierarchy_down_single    geda_struct_hierarchy_down_single
#define geda_hierarchy_down_symbol    geda_struct_hierarchy_down_symbol
#define geda_hierarchy_find_up_page   geda_struct_hierarchy_find_up_page
#define geda_hierarchy_traverse_pages geda_struct_hierarchy_traverse_pages
#define geda_hierarchy_print_page     geda_struct_hierarchy_print_page
#define geda_hierarchy_find_prev_page geda_struct_hierarchy_find_prev_page
#define geda_hierarchy_find_next_page geda_struct_hierarchy_find_next_page

#endif /* __GEDA_FILE_MAC__ */
