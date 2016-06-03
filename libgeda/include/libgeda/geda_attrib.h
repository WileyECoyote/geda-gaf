/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_attrib.h
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
 *  Date Contributed: April, 4th, 2016
 */
/*! \file geda_attrib.h "libgeda/geda_attrib.h"
 *  \brief Libgeda macros for o_attrib modules.
 *
 */

#ifndef __GEDA_ATTRIB_MAC__
#define __GEDA_ATTRIB_MAC__

#define geda_attrib_add                        geda_attrib_object_add
#define geda_attrib_append_changed_hook        geda_attrib_object_append_changed_hook
#define geda_attrib_attach                     geda_attrib_object_attach
#define geda_attrib_attach_list                geda_attrib_object_attach_list
#define geda_attrib_detach                     geda_attrib_object_detach
#define geda_attrib_detach_all                 geda_attrib_object_detach_all
#define geda_attrib_first_attrib_by_name       geda_attrib_object_first_attrib_by_name
#define geda_attrib_freeze_hooks               geda_attrib_object_freeze_hooks
#define geda_attrib_get_name_value             geda_attrib_object_get_name_value
#define geda_attrib_is_attached_to             geda_attrib_object_is_attached_to
#define geda_attrib_is_inherited               geda_attrib_object_is_inherited
#define geda_attrib_new_attached               geda_attrib_object_new_attached
#define geda_attrib_print                      geda_attrib_object_print
#define geda_attrib_remove                     geda_attrib_object_remove
#define geda_attrib_return_attribs             geda_attrib_object_return_attribs
#define geda_attrib_search_attached_by_name    geda_attrib_object_search_attached_by_name
#define geda_attrib_search_floating_by_name    geda_attrib_object_search_floating_by_name
#define geda_attrib_search_inherited_by_name   geda_attrib_object_search_inherited_by_name
#define geda_attrib_search_object_by_name      geda_attrib_object_search_object_by_name
#define geda_attrib_search_for_string          geda_attrib_object_search_object_string
#define geda_attrib_set_integer_value          geda_attrib_object_set_integer_value
#define geda_attrib_set_value                  geda_attrib_object_set_value
#define geda_attrib_string_get_name_value      geda_attrib_object_string_get_name_value
#define geda_attrib_thaw_hooks                 geda_attrib_object_thaw_hooks

#endif /* __GEDA_ATTRIB_MAC__ */
