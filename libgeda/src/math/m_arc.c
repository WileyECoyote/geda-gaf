/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2014 Wiley Edward Hill
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

/*! \file m_arc.c
 *
 *  \brief Low-level mathmatical functions for arcs
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"

#include <geda_debug.h>

/*! \brief Calculates the length of an Arc sector
 * point on the perimeter or interior of the circle.
 *
 *  \param [in] radius  The radius of the arc.
 *  \param [in] sweep   The included angle.
 *
 *  \return The length of the sector.
 */
double  m_arc_length (int radius, int sweep)
{
  return 2 * M_PI * radius * (sweep / 360);
}
