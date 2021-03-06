/* -*- C header file: geda_struct.h indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/* ------------------------------------------------------------------ */

#ifndef __GEDA_STRUCTURES__
#define __GEDA_STRUCTURES__

/** \file geda_struct.h
 *
 *  \brief Global Structure Definitions
 *
 *   \defgroup geda-global-structures Global Structure Constants
 * @{\par This group defines global structures constants
 *   \ingroup geda-globals
 */

typedef struct _GedaRefDes GedaRefDes;

struct _GedaRefDes
{
  const char *designator;
  const char *description;
};

/** @} endgroup geda-global-structures */

#endif
