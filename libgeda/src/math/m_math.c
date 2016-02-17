/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>

#include <stdio.h>
#include <time.h>
#include <math.h>

#include <libgeda_priv.h>

/*! \brief Convert Degrees to Radian
 *  \par Function Description
 *  This function converts angular \a degrees into radian measure.
 *
 *  \param [in] degrees degrees to convert
 *
 *  \returns radians conversion as a double
 *
 *  \sa m_radians_to_degrees
 */
double m_degrees_to_radians(double degrees)
{
  return (double)degrees * M_PI / 180.0;
}

/*! \brief Calculate the distance between two points
 *
 *  \par Function Description
 *  This function calculates the distance between two points defined
 *  by the (\a x1, \a y1) and (\a x2, \a y2) parameters.
 *
 *  \param [in]  x1  x-value of the first point
 *  \param [in]  y1  y-value of the first point
 *  \param [in]  x2  x-value of the second point
 *  \param [in]  y2  y-value of the second point
 *
 *  \return the distance
 */
double m_distance(int x1, int y1, int x2, int y2)
{

#if HAVE_HYPOT

  return hypot((x1-x2), (y1-y2));

#else

  return sqrt((x1-x2) * (x1-x2) + (y1-y2) * (y1-y2));

#endif

}

/*! \brief Convert Paper size to World coordinates.
 *  \par Function Description
 *  This function takes the paper size and converts it to
 *  world coordinates. It supports landscape with a fixed aspect ratio.
 *
 *  \param [in]  width   Paper width. (units?)
 *  \param [in]  height  Paper height. (units?)
 *  \param [in]  border  Paper border size. (units?)
 *  \param [out] right   Right world coordinate. (units?)
 *  \param [out] bottom  Bottom world coordinate. (units?)
 *
 *  \todo Support more modes than just landscape only mode.
 */
void m_papersize_to_world(int width, int height, int border, int *right, int *bottom)
{
  float aspect;

  aspect = (float) width / (float) height;

#if DEBUG
  printf("%f\n", aspect);
#endif

  if (aspect < 1.333333333) {
    /* is this lrint really needed? */
#ifdef HAVE_LRINT
    *right = lrint (width+border + ((height+border)*1.33333333 - (width+border)));
#else
    *right = (int) width+border +
      ((height+border)*1.33333333 - (width+border));
#endif
    *bottom = height+border;
  } else {
    *right = (int) width+border;
    *bottom = (int) height+border + ((width+border)/1.33333333 - (height+border));
  }

#if DEBUG
  aspect = (float) *right / (float) *bottom;
  printf("%f\n", aspect);
#endif

}

/*! \brief Convert Radians to Degrees
 *  \par Function Description
 *  This function converts radian measure into angular \a degrees.
 *
 *  \param [in] radians measure to convert
 *
 *  \returns degrees conversion as a double
 *
 *  \sa m_degrees_to_radians
 */
double m_radians_to_degrees(double radians)
{
  return (double)radians * 180.0 / M_PI;
}

/*! \brief Return a randon Number between given limits.
 *  \par Function Description
 *  This function intentionally does not seed the generator.
 *  Seems to work just fine.
 *
 *  \param [in]  min_num The smallest value.
 *  \param [in]  max_num The largest value.
 *
 *  \returns random integer
 */
int m_random_number (int min_num, int max_num)
{
  int result  = 0;
  int low_num = 0;
  int hi_num  = 0;

  if (min_num < max_num) {
    low_num = min_num;
    hi_num  = max_num + 1; /* this is done to include max_num in output */
  }
  else {
    low_num = max_num + 1; /* this is done to include max_num in output */
    hi_num  = min_num;
  }

  result = (rand()%(hi_num-low_num))+low_num;
  return result;
}

/*! \brief Rotate a point by an arbitrary angle.
 *  \par Function Description
 *  This function will rotate a point coordinate by an arbitrary angle
 *  and return the new coordinate in the newx and newy parameters.
 *
 *  \param [in]  x      Input point x coordinate.
 *  \param [in]  y      Input point y coordinate.
 *  \param [in]  angle  Angle to rotate in degrees.
 *  \param [out] newx   Output point x coordinate.
 *  \param [out] newy   Output point y coordinate.
 */
void m_rotate_point(int x, int y, int angle, int *newx, int *newy)
{
  double cos_theta, sin_theta;
  double rad;

  rad = angle*M_PI/180;

  cos_theta = cos(rad);
  sin_theta = sin(rad);

  *newx = x * cos_theta - y * sin_theta;
  *newy = x * sin_theta + y * cos_theta;
}

/*! \brief Rotate point in 90 degree increments only.
 *  \par Function Description
 *  This function takes a point coordinate and rotates it by
 *  90 degrees at a time.  The new point coordinate is returned
 *  in newx and newy.
 *
 *  \param [in]  x      Input point x coordinate.
 *  \param [in]  y      Input point y coordinate.
 *  \param [in]  angle  Angle to rotate by (90 degree increments only).
 *  \param [out] newx   Output point x coordinate.
 *  \param [out] newy   Output point y coordinate.
 */
void m_rotate_point_90(int x, int y, int angle, int *newx, int *newy)
{
  double costheta=1;
  double sintheta=0;

  /* I could have used sine/cosine for this, but I want absolute
   * accuracy */
  switch(angle) {

    case(0):
      *newx = x;
      *newy = y;
      return;
      break;

    case(90):
      costheta = 0;
      sintheta = 1;
      break;

    case(180):
      costheta = -1;
      sintheta = 0;
      break;

    case(270):
      costheta = 0;
      sintheta = -1;
      break;
  }

  *newx = x * costheta - y * sintheta;
  *newy = x * sintheta + y * costheta;
}
