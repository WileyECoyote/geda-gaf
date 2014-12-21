/* -*- C geda_draw.h indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: geda_draw.h
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
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
 *
 * Date: Dec, 25, 2014
 * Contributing Author: Wiley Edward Hill
 *
*/
/*!
 * \file geda_draw.c
 * \brief Header file for Drawing routines in Libgedadraw.
 *
 * \remarks This module is under development
 *
 */
#ifndef __GEDA_DRAW_H__
#define __GEDA_DRAW_H__

/* */
#ifdef HAVE_XFT
#  define FONT_SIZE_FACTOR     13.6
#else
#  define FONT_SIZE_FACTOR     19
#endif
#define EDA_DEFAULT_EOL_SP    4
#define EDA_DEFAULT_LEADING   4


#define BEZIER_STEP 0.0025

#include "geda_x11.hpp"

#endif /* __GEDA_DRAW_H__ */
