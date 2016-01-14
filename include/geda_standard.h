/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_standard.h
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
/*!
 * \file geda_standard.h
 * \brief Include standard C library headers
 * \par
 *  This file includes commonly used standard library headers and is used
 *  to reduce clutter so that specific inclusion required for source files
 *  are not obscured.
 */
#ifndef __GEDA_STD_INC__
#  define __GEDA_STD_INC__

#  ifdef HAVE_CONFIG_H
#    include <config.h>
#  endif

#  include <ascii.h>

#  include <stdio.h>

#  ifdef HAVE_STRING_H
#     include <string.h>
#  endif

#  ifdef HAVE_STDLIB_H
#    include <stdlib.h>
#  endif

#  ifdef HAVE_UNISTD_H
#     include <unistd.h>
#  endif

#  ifdef HAVE_ERRNO_H
#     include <errno.h>
#  endif

#endif /* __GEDA_STD_INC__ */
