/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_debug.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill
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
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: July, 25, 2014
 */
#ifndef __GEDA_DEBUG__
#  define __GEDA_DEBUG__

#define EMBED_BREAKPOINT  asm volatile ("int3;")

#define OBJECT_NAME(p) G_IS_OBJECT(p) ? G_OBJECT_TYPE_NAME(p): "not a gobject"

#define SET_DEBUG_DND_EVENTS  0
#define SET_DEBUG_EVENTS      0
#define SET_DEBUG_GRID        0
#define SET_DEBUG_IMAGING     0
#define SET_DEBUG_STATUS      0
#define SET_DEBUG_SENSITIVITY 0
#define SET_DEBUG_TOOLBARS    0
#define SET_DEBUG_UNDO        0

/* libgeda*/
#define SET_DEBUG_CONNS       0
#define SET_DEBUG_TILES       0

/* libgedauio*/
#define SET_DEBUG_LIBGEDAUIO  0

#define SET_DEBUG_GEDA_ENTRY  0

#define SET_DMALLOC           0

/* ------------------------------------------------------ */
#define DEBUG_DND_EVENTS  ( SET_DEBUG_DND_EVENTS  || DEBUG )
#define DEBUG_EVENTS      ( SET_DEBUG_EVENTS      || DEBUG )
#define DEBUG_GRID        ( SET_DEBUG_GRID        || DEBUG )
#define DEBUG_IMAGING     ( SET_DEBUG_IMAGING     || DEBUG )
#define DEBUG_STATUS      ( SET_DEBUG_STATUS      || DEBUG )
#define DEBUG_SENSITIVITY ( SET_DEBUG_SENSITIVITY || DEBUG_STATUS )
#define DEBUG_TOOLBARS    ( SET_DEBUG_TOOLBARS    || DEBUG )
#define DEBUG_UNDO        ( SET_DEBUG_UNDO        || DEBUG )

/* libgeda*/
#define DEBUG_CONNS       ( SET_DEBUG_CONNS       || DEBUG )
#define DEBUG_TILES       ( SET_DEBUG_TILES       || DEBUG )

/* libgedauio*/
#define DEBUG_LIBGEDAUIO  ( SET_DEBUG_LIBGEDAUIO  || DEBUG )

#define DEBUG_GEDA_ENTRY  ( SET_DEBUG_GEDA_ENTRY  || DEBUG_LIBGEDAUIO)

#ifdef HAVE_LIBDMALLOC
#    include <dmalloc.h>
#endif

#endif /* __GEDA_DEBUG__ */

