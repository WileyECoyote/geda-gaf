/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_object_list.h
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
 *  Date Contributed: April, 20th, 2016
 */
/*! \file geda_object_list.h "libgeda/geda_object_list.h"
 *  \brief Libgeda macros for o_list.c modules.
 *
 */

#ifndef __GEDA_OBJECT_LIST_MAC__
#define __GEDA_OBJECT_LIST_MAC__

#define geda_copy_list         geda_object_list_copy_all
#define geda_mirror_list       geda_object_list_mirror
#define geda_rotate_list       geda_object_list_rotate
#define geda_translate_list    geda_object_list_translate
#define geda_list_set_color    geda_object_list_set_color

#endif /* __GEDA_OBJECT_LIST_MAC__ */
