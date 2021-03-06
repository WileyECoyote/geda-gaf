
/* -*- test_math.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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
 *  Date Contributed: November, 29nd, 2019
 */

#include "../../config.h"

#include <libgeda.h>
#include <math.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/math/m_math.c"

/*! \file test_math.c
 *  \brief Tests for geda math functions
 *  \par
 *   This module includes tests for functions in src/math.c.
 *
 *  Test Identifiers:  M  01  88  88
 *                     ^   ^   ^   ^
 *    group-code ______|   |   |   |
 *                         |   |   |
 *    Module/File No. _____|   |   |
 *                             |   |
 *    Function Number _________|   |
 *                                 |
 *    Tests Number ________________|
 *
 *  See tests/README for more details on the nomenclature for test identifiers.
 *
 *      M0101    geda_math_degrees_to_radians
 *      M0102    geda_math_distance
 *      M0103    geda_math_papersize_to_world
 *      M0104    geda_math_random_number
 *      M0105    geda_math_radians_to_degrees
 *      M0106    geda_math_rotate_point
 *      M0107    geda_math_rotate_point_90
 */

int check_math_degrees_to_radians ()
{
  int result = 0;

  double value;

  /* === Function 01:  geda_math_degrees_to_radians === */

  value = geda_math_degrees_to_radians(0.0);

  if (value != 0.0) {
    fprintf(stderr, "FAILED: (M010101-0.0) value (%f)\n", value);
    result++;
  }

  value = geda_math_degrees_to_radians(90.0);

  value = round(value * 100) / 100;

  if (abs (value - 1.570) > .001) {
    fprintf(stderr, "FAILED: (M010101-90) value (%f)\n", value);
    result++;
  }

  value = geda_math_degrees_to_radians(180.0);

  value = round(value * 100) / 100;

  if (abs (value - 3.140) > .001) {
    fprintf(stderr, "FAILED: (M010101-180) value (%f)\n", value);
    result++;
  }

  return result;
}

int check_math_distance ()
{
  int result = 0;

  double distance;

  /* === Function 02:  geda_math_distance === */

  distance = geda_math_distance(0, 3, 4, 0);

  if (distance != 5.0) {
    fprintf(stderr, "FAILED: (M010201-A) distance (%f)\n", distance);
    result++;
  }

  distance = geda_math_distance(0, 4, 3, 0);

  if (distance != 5.0) {
    fprintf(stderr, "FAILED: (M010201-B) distance (%f)\n", distance);
    result++;
  }

  return result;
}

int check_math_papersize_to_world ()
{
  int result = 0;

  int right;
  int bottom;

  int width, height, border;

  /* === Function 03:  geda_math_papersize_to_world === */

  width  = 120000;
  height = 90000;
  border = 1000;

  geda_math_papersize_to_world(width, height, border, &right, &bottom);

  if (right != 121000) {
    fprintf(stderr, "FAILED: (M010301) right (%d)\n", right);
    result++;
  }

  if (bottom != 90750) {
    fprintf(stderr, "FAILED: (M010302) bottom (%d)\n", bottom);
    result++;
  }

  return result;
}

int check_math_random_number ()
{
  int result = 0;
  int lower, random, upper;

  /* === Function 04: geda_math_random_number === */

  lower = 0;
  upper = 10;

  random = geda_math_random_number (lower, upper);

  if (random < 0 || random > 10) {
    fprintf(stderr, "FAILED: (M0103401) random=(%d)\n", random);
    result++;
  }

  return result;
}


int main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (setjmp(point) == 0) {
    result += check_math_degrees_to_radians();
  }
  else {
    fprintf(stderr, "Caught signal checking degrees_to_radians in %s\n\n", MUT);
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_math_distance();
  }
  else {
    fprintf(stderr, "Caught signal checking math_distance in %s\n\n", MUT);
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_math_papersize_to_world();
  }
  else {
    fprintf(stderr, "Caught signal checking papersize_to_world in %s\n\n", MUT);
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_math_random_number();
  }
  else {
    fprintf(stderr, "Caught signal checking random_number in %s\n\n", MUT);
    return 1;
  }

  return result;
}
