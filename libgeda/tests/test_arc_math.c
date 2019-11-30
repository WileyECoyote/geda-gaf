/* -*- test_arc_math.c -*-
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
 *  Date Contributed: April, 26th, 2019
 */

#include "../../config.h"

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>

#include <math.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/math/m_arc.c"

/** \defgroup test-arc-math Test Arc Math Module
 * @{
 * \brief Group 4 src/math/m_arc.c geda_math_arc_
 *  Group 4 == Module/File No.
 */

/*
 *  Test Identifiers:  M  03  88  88
 *                     ^   ^   ^   ^
 *    group-code ______|   |   |   |
 *                         |   |   |
 *    Module/File No. _____|   |   |
 *                             |   |
 *    Function No._____________|   |
 *                                 |
 *    Tests Number ________________|
 *
 *  See tests/README for more details on the nomenclature for test identifiers.
 *
 *      M0301    geda_math_arc_chord
 *      M0302    geda_math_arc_length
 *      M0303    geda_math_arc_includes_point
 */

int check_math_arc_chord ()
{
  int result = 0;

  GedaObject *object;
  LINE line;

  object = geda_arc_object_new (3, 1000, 1000, 1000, 0, 90);

  geda_math_arc_chord (object->arc, &line);

  if (line.x[0] != 2000 || line.y[0] != 1000) {
    fprintf(stderr, "FAILED: (M030101A) chord pt1 (%d, %d)\n", line.x[0], line.y[0]);
    result++;
  }

  if (line.x[1] != 1000 || line.y[1] != 2000) {
    fprintf(stderr, "FAILED: (M030101A) chord pt2 (%d, %d)\n", line.x[1], line.y[1]);
    result++;
  }

  object = geda_arc_object_new (3, 1000, 1000, 1000, 90, 90);

  geda_math_arc_chord (object->arc, &line);

  if (line.x[0] != 1000 || line.y[0] != 2000) {
    fprintf(stderr, "FAILED: (M030102A) chord pt1 (%d, %d)\n", line.x[0], line.y[0]);
    result++;
  }

  if (line.x[1] != 0 || line.y[1] != 1000) {
    fprintf(stderr, "FAILED: (M030102A) chord pt2 (%d, %d)\n", line.x[1], line.y[1]);
    result++;
  }

  object = geda_arc_object_new (3, 1000, 1000, 1000, 180, 90);

  geda_math_arc_chord (object->arc, &line);

  if (line.x[0] != 0 || line.y[0] != 1000) {
    fprintf(stderr, "FAILED: (M030103A) chord pt1 (%d, %d)\n", line.x[0], line.y[0]);
    result++;
  }

  if (line.x[1] != 1000 || line.y[1] != 0) {
    fprintf(stderr, "FAILED: (M030103A) chord pt2 (%d, %d)\n", line.x[1], line.y[1]);
    result++;
  }

  object = geda_arc_object_new (3, 1000, 1000, 1000, 270, 90);

  geda_math_arc_chord (object->arc, &line);

  if (line.x[0] != 1000 || line.y[0] != 0) {
    fprintf(stderr, "FAILED: (M030104A) chord pt1 (%d, %d)\n", line.x[0], line.y[0]);
    result++;
  }

  if (line.x[1] != 2000 || line.y[1] != 1000) {
    fprintf(stderr, "FAILED: (M030104A) chord pt2 (%d, %d)\n", line.x[1], line.y[1]);
    result++;
  }

  object = geda_arc_object_new (3, 1000, 1000, 1000, 0, 225);

  geda_math_arc_chord (object->arc, &line);

  if (line.x[0] != 2000 || line.y[0] != 1000) {
    fprintf(stderr, "FAILED: (M030105A) chord pt1 (%d, %d)\n", line.x[0], line.y[0]);
    result++;
  }

  if (line.x[1] != 293 || line.y[1] != 293) {
    fprintf(stderr, "FAILED: (M030105A) chord pt2 (%d, %d)\n", line.x[1], line.y[1]);
    result++;
  }

  return result;
}

int check_math_arc_length ()
{
  GedaObject *object;
  int result = 0;
  double length;

  object = geda_arc_object_new (3, 100, 100, 18, 0, 2);

  length = geda_math_arc_length (object->arc);

  length = trunc(length * 1000000);

  if (length - 628318.0 != 0) {
    fprintf(stderr, "FAILED: (M030201) <%p> length=%.5f\n", object->arc, length);
    result++;
  }

  geda_arc_object_set_arc_sweep(object, 135);

  length = geda_math_arc_length (object->arc);

  length = trunc(length * 10000);

  if (length - 424115.0 != 0) {
    fprintf(stderr, "FAILED: (M030202) <%p> length=%.5f\n", object->arc, length);
    result++;
  }

  return result;
}

int check_math_arc_includes_point ()
{
  GedaObject *object;
  GedaPoint   point;
  int result = 0;
  int answer;

  object = geda_arc_object_new (3, 1000, 1000, 500, 0, 90);

  point.x = 0;
  point.y = 0;

  /* geda_math_arc_includes_point */
  answer = geda_math_arc_includes_point(object->arc, &point);

  if (answer) {
    fprintf(stderr, "FAILED: (M030301A) _math_arc_includes_point\n");
    result++;
  }

  /* Center point */
  point.x = 1000;
  point.y = 1000;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (answer) {
    fprintf(stderr, "FAILED: (M030301B) _math_arc_includes_point\n");
    result++;
  }

  /* End point 1 */
  point.x = 1500;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (!answer) {
    fprintf(stderr, "FAILED: (M030301C) _math_arc_includes_point\n");
    result++;
  }

  /* End point 2 */
  point.x = 1000;
  point.y = 1500;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (!answer) {
    fprintf(stderr, "FAILED: (M030301D) _math_arc_includes_point\n");
    result++;
  }

  /* Inside sweep, inside arc */
  point.x = 1200;
  point.y = 1200;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (answer) {
    fprintf(stderr, "FAILED: (M030301E) _math_arc_includes_point\n");
    result++;
  }

  /* Inside sweep, outside arc */
  point.x = 1358;
  point.y = 1358;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (answer) {
    fprintf(stderr, "FAILED: (M030301F) _math_arc_includes_point\n");
    result++;
  }

  /* midpoint 1354, with fuzzy +/- 3 */
  point.x = 1351;
  point.y = 1351;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (!answer) {
    fprintf(stderr, "FAILED: (M030301G) _math_arc_includes_point\n");
    result++;
  }

  point.x = 1357;
  point.y = 1357;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (!answer) {
    fprintf(stderr, "FAILED: (M030301H) _math_arc_includes_point\n");
    result++;
  }

  /* Outside sweep, inside arc */
  point.x = 1400;
  point.y = 900;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (answer) {
    fprintf(stderr, "FAILED: (M030301J) _math_arc_includes_point\n");
    result++;
  }

  /* Outside sweep, beyond arc */
  point.x = 1600;
  point.y = 900;

  answer = geda_math_arc_includes_point(object->arc, &point);

  if (answer) {
    fprintf(stderr, "FAILED: (M030301K) _math_arc_includes_point\n");
    result++;
  }

  return result;
}

/* geda_math_arc_shortest_distance */
int check_math_arc_shortest_distance (void)
{
  int result = 0;
  int i;

  for (i = 0; i < 10; i++) {

    GedaObject *object;

    int j;
    int x = geda_random_number (0, 120000);
    int y = geda_random_number (0, 80000);

    object = geda_arc_object_new (3, x, y, 500, 0, 90);

    for (j = 0; j < 10; j++) {

      int nx;
      int ny;

      int px = geda_random_number (0, 120000);
      int py = geda_random_number (0, 80000);

      double shortest;

      shortest = geda_math_arc_shortest_distance (object->arc, px, py, 0);

      if (geda_arc_object_get_nearest_point (object, px, py, &nx, &ny)) {

        int dx;
        int dy;

        dx = px - nx;
        dy = py - ny;

        if (dy) {

          double nearest = hypot (dx, dy);

          int id1 = rint(shortest);
          int id2 = rint(nearest);

          if ((id1 - id2) > 1) {
            fprintf(stderr, "FAILED: (%d,%d) %f arc_shortest_distance\n", px, py, shortest);
            result++;
          }
        }
        else {
          fprintf(stderr, "skipped shortest_distance\n");
        }
      }
    }

    g_object_unref (object);
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
    result += check_math_arc_chord();
  }
  else {
    fprintf(stderr, "Caught signal checking arc_chord in %s\n\n", MUT);
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_math_arc_length();
  }
  else {
    fprintf(stderr, "Caught signal checking arc_length in %s\n\n", MUT);
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_math_arc_includes_point();
  }
  else {
    fprintf(stderr, "Caught signal checking arc_includes_point in %s\n\n", MUT);
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_math_arc_shortest_distance();
  }
  else {
    fprintf(stderr, "Caught signal checking arc_shortest_distance in %s\n\n", MUT);
    return 1;
  }

  return result;
}
