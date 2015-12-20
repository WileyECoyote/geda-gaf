/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Note this file intentionally does not have a wrapper!
 * If header is included by <libgeda/libgeda.h>
*/

/** \defgroup geda-globals Global Defines and Enumerators
 *
 * \note    To include explicit references to gEDA globals the include path
 *          should be in the doxygen INPUT variable in the .dox file, using
 *          INCLUDE_PATH does not seem to do anything.
 *
 * \remarks If this file gets included twice then it most likely means the
 *          inclusion of headers in the source file in not setup correctly!
 *
 * @{\par This contains constants used through-out the gEDA suite
 */

#include <missing.h>

#include <geda_system.h>
#include <geda_idefines.h>
#include <geda_sdefines.h>
#include <geda_types.h>
#include <geda_bitmaps.h>
#include <geda_macros.h>
#include <geda_enum.h>
#include <geda_struct.h>
#include <geda_wrap.h>

/** @} endgroup geda-globals */
