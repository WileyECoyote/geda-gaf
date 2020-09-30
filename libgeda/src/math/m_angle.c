/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2019 gEDA Contributors (see ChangeLog for details)
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
/*!
 * \file m_angle.c
 * \brief Functions for working with angles
 */

/** \defgroup math-angle-proc Math Procedures for Angles
 * @{
 * \brief Math Related Procedures for Angles
 */

#include "../../../config.h"

#include <math.h>
#include <stdio.h>

#include "../../include/libgeda_priv.h"

/*!
 * \brief Checks if an angle is [0,360)
 * \par Function Description
 *
 * \param [in] angle the angle in degrees
 *
 * \retval TRUE if the angle is [0,360)
 */
bool geda_math_angle_is_normal (int angle)
{
  return ((0 <= angle) && (angle < 360));
}

/*!
 * \brief Checks if an angle is orthographic
 * \par Function Description
 *
 * \param [in] angle the angle in degrees
 *
 * \retval TRUE if the angle is a multiple of 90 degrees
 */
bool geda_math_angle_is_ortho (int angle)
{
  return ((angle % 90) == 0);
}

/*!
 * \brief Make an angle orthographic
 * \par Function Description
 *  Snaps the angle to the nearest 90 degrees
 *
 * \param [in] angle the angle in degrees
 * \return the orthographic angle
 */
int geda_math_angle_make_ortho (int angle)
{
  return round (angle / 90.0) * 90;
}

/*!
 * \brief Normalize an angle to [0,360)
 * \par Function Description
 *
 * \param [in] angle the angle in degrees
 *
 * \return the normalized angle inside [0,360)
 */
int geda_math_angle_normalize (int angle)
{
  if (angle < 0) {
    angle = 360 - (-angle % 360);
  }
  if (angle >= 360) {
    angle %= 360;
  }

  return angle;
}

/** @} endgroup math-angle-proc */
