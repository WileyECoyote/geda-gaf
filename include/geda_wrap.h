/* -*- C header indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * file: geda_wrap.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2015 Wiley Edward Hill
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
 * 02110-1301 USA
 */

/* ------------------------------------------------------------------ */

#ifndef __GEDA_WRAP__
#define __GEDA_WRAP__

/** \file geda_wrap.h
 *
 *  \brief Global Wrapper Utility Macros
 *
 *   \defgroup geda-wrapper-macros Global Wrappper Macros
 * @{\par This group defines utility macros for wrapping functions
 *        in external librarie.
 *   \ingroup (geda-globals)
 */

#define GEDA_OBJECT_GET_DATA(object, key) \
    g_object_get_data (G_OBJECT(object), key)

#define GEDA_OBJECT_SET_DATA(object, data,  key) \
    g_object_set_data (G_OBJECT(object), key, data);

/** @} endgroup geda-wrapper-macros */

#endif
