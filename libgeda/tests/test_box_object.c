/* -*- test_box_object.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 *  Date Contributed: April, 26th, 2016
 */

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>
#include <math.h>
#include "test-suite.h"

/*! \file test_box_object.c
 *  \brief Tests for o_box_object.c module
 *  \par
 *  This module provides basic unit tests for functions in the geda_box_object_
 *  module.
 */

#define TOBJECT "GedaBox"

/** \defgroup test-object-geda-arc Test GEDA Box object Module
 * @{
 * \brief Group 4 src/object/o_box_object.c geda_box_object_
 *  Group 4 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  04  88, 88
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
 *      O0401    geda_box_object_copy
 *               geda_box_object_get_nearest_point
 *      O0403    geda_box_object_get_position
 *               geda_box_object_modify
 *               geda_box_object_modify_all
 *               geda_box_object_mirror
 *      O0407    geda_box_object_new
 *
 *      O0418    geda_box_object_read
 *               geda_box_object_rotate
 *               geda_box_object_shortest_distance
 *      O0421    geda_box_object_to_buffer
 *               geda_box_object_translate
 */

int
check_construction ()
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int c  = m_random_number ( 0, MAX_COLORS - 1);
    int x1 = m_random_number ( 0,     119900);
    int y1 = m_random_number ( 0,      79900);
    int x2 = m_random_number (x1 + 1, 120000);
    int y2 = m_random_number (y1 + 1,  80000);

    GedaObject *object0 = geda_box_object_new(c, x1, y1, x2, y2);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O040701A) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_BOX(object0->box)) {
      fprintf(stderr, "FAILED: (O040701B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;   /* terminate loop if fail */
    }
    else {

      GedaBox *box  = object0->box;
      int      fail = 0;
      int      value;

      value = geda_object_get_color (object0);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-A) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      if (box->upper_x - x1) {
        fprintf(stderr, "FAILED: (O040701X1) box upper_x %d != %d\n", box->upper_x, x1);
        fail++;
      }

      if (box->upper_y - y1) {
        fprintf(stderr, "FAILED: (O040701Y1) box upper_y %d != %d\n", box->upper_y, y1);
        fail++;
      }

      if (box->lower_x - x2) {
        fprintf(stderr, "FAILED: (O040701X2) box lower_x %d != %d\n", box->lower_x, x2);
        fail++;
      }

      if (box->lower_y - y2) {
        fprintf(stderr, "FAILED: (O040701Y2) box lower_y %d != %d\n", box->lower_y, y2);
        fail++;
      }
      if (fail) {
        result++;
        break;
      }
    }
    g_object_unref (object0);
  }
  return result;
}

int
check_accessors ()
{
  int count;
  int result = 0;

  for (count = 0; count < 3; count++) {

    int c  = m_random_number ( 0, MAX_COLORS - 1);
    int x1 = m_random_number ( 0,       119800);
    int y2 = m_random_number ( 0,        79800);
    int x2 = m_random_number (x1 + 100, 120000);
    int y1 = m_random_number (y2 + 100,  80000);

    GedaObject *object0 = geda_box_object_new(c, x1, y1, x2, y2);

    int px, py;
    int fail;

    fail = 0;

    if (geda_box_object_get_position (NULL, &px, &py)) {
      fprintf(stderr, "FAILED: (O040300) object NULL\n");
      fail++;
    }

    if (!geda_box_object_get_position (object0, &px, &py)) {
      fprintf(stderr, "FAILED: (O040301A) box x1=%d, y1=%d, x2=%d, y2=%d\n", x1, y1, x2, y2);
      fail++;
    }

    if (px - x1) {
      fprintf(stderr, "FAILED: (O040301B) box %d != %d\n", px, x1);
      fail++;
    }

    if (py - y2) {
      fprintf(stderr, "FAILED: (O040301C) box %d != %d\n", py, y2);
      fail++;
    }
    if (fail) {
      result++;
    }
    g_object_unref (object0);
  }
  return result;
}

int
check_serialization ()
{
  int  count;
  int  converted;
  int  result;
  unsigned version;

  result    = 0;
  converted = sscanf (PACKAGE_DATE_VERSION, "%u", &version);

  if (!converted) {
    fprintf(stderr, "File %s, <%s>: could not scan version", __FILE__, __func__);
    version=19700101;
    result++;
  }

  for (count = 0; count < 10; count++) {

    int c  = m_random_number ( 0, MAX_COLORS - 1);
    int x1 = m_random_number ( 0,       119800);
    int y2 = m_random_number ( 0,        79800);
    int x2 = m_random_number (x1 + 100, 120000);
    int y1 = m_random_number (y2 + 100,  80000);

    GedaObject *object0 = geda_box_object_new(c, x1, y1, x2, y2);


    char *buffer0 = geda_box_object_to_buffer (object0);
    g_object_unref (object0);

    if (!buffer0) {
      fprintf(stderr, "FAILED: (O042101A) New GedaObject Failed\n");
      result++;
      break;
    }

    GedaObject *object1 = geda_box_object_read (buffer0,
                                                version,
                                                FILEFORMAT_VERSION,
                                                NULL);

    if (!GEDA_IS_OBJECT(object1)) {
      fprintf(stderr, "FAILED: (O041801A) Read GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_BOX(object1->box)) {
      fprintf(stderr, "FAILED: (O041801B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      GedaBox *box  = object1->box;
      int      fail = 0;
      int      value;

      value = geda_object_get_color (object1);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-B) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      if (box->upper_x - x1) {
        fprintf(stderr, "FAILED: (O0421/O0418X1) box upper_x %d != %d\n", box->upper_x, x1);
        fail++;
      }

      if (box->upper_y - y1) {
        fprintf(stderr, "FAILED: (O0421/O0418Y1) box upper_y %d != %d\n", box->upper_y, y1);
        fail++;
      }

      if (box->lower_x - x2) {
        fprintf(stderr, "FAILED: (O0421/O0418X2) box lower_x %d != %d\n", box->lower_x, x2);
        fail++;
      }

      if (box->lower_y - y2) {
        fprintf(stderr, "FAILED: (O0421/O0418Y2) box lower_y %d != %d\n", box->lower_y, y2);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to read/write %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t  color: %d\n", c);
        fprintf(stderr, "\tupper_x: %d\n", x1);
        fprintf(stderr, "\tupper_y: %d\n", y1);
        fprintf(stderr, "\tlower_x: %d\n", x2);
        fprintf(stderr, "\tlower_y: %d\n", y2);

        result = result + fail;
        break;
      }

      char *buffer1 = geda_box_object_to_buffer (object1);
      g_object_unref (object1);

      if (strcmp (buffer0, buffer1)) {
        fprintf(stderr, "FAILED: (O042101B) %s buffer mismatch\n", TOBJECT);
        result++;
        break;
      }

      g_free (buffer0);
      g_free (buffer1);
    }
  }
  return result;
}

int
check_query()
{
  int  count;
  int result = 0;

  int dum = 1;
  if (geda_box_object_get_nearest_point(NULL, dum, dum, &dum, &dum)) {
    fprintf(stderr, "FAILED: (O040200) box_get_nearest_point NULL\n");
    result++;
  }

  for (count = 0; count < 3; count++) {

    int c   = m_random_number ( 0, MAX_COLORS - 1);
    int x1  = m_random_number ( 0,       119800);
    int y2  = m_random_number ( 0,        79800);
    int x2  = m_random_number (x1 + 100, 120000);
    int y1  = m_random_number (y2 + 100,  80000);
    int off = m_random_number (     100,   1000);

    GedaObject *object = geda_box_object_new(c, x1, y1, x2, y2);
    GedaBox    *box    = object->box;

    /* Quad 1 = Outside North East */
    int qx = x2 + off;
    int qy = y1 + off;

    int  nx, ny;
    int  fail;

    fail = 0;

    /* O0402 geda_box_object_get_nearest_point */

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040201A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x2 - nx || y1 - ny) {
      fprintf(stderr, "FAILED: (O040201B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 12 = Outside North */
    qx = (x2 + x1) / 2;
    qy = y1 + off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040202A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (qx - nx || y1 - ny) {
      fprintf(stderr, "FAILED: (O040202B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 2 = Outside North West*/
    qx = x1 - off;
    qy = y1 + off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040203A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x1 - nx || y1 - ny) {
      fprintf(stderr, "FAILED: (O040203B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 23 = Outside West side */
    qx = x1 - off;
    qy = (y1 + y2) / 2;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040204A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x1 - nx || qy - ny) {
      fprintf(stderr, "FAILED: (O040204B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 3 = Outside South West */
    qx = x1 - off;
    qy = y2 - off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040205A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x1 - nx || y2 - ny) {
      fprintf(stderr, "FAILED: (O040205B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 34 = Outside South */
    qx = (x2 + x1) / 2;
    qy = y2 - off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040206A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (qx - nx || y2 - ny) {
      fprintf(stderr, "FAILED: (O040206B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 4 = Outside South East */
    qx = x2 + off;
    qy = y2 - off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040207A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x2 - nx || y2 - ny) {
      fprintf(stderr, "FAILED: (O040207B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 4 = Outside East */
    qx = x2 + off;
    qy = (y1 + y2) / 2;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040208A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x2 - nx || qy - ny) {
      fprintf(stderr, "FAILED: (O040208B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Check inside on diagonals, these should not report valid results */

    /* Quad 1 = Inside North East */
    qx = x2 - 100;
    qy = y1 - 100;
    nx = 0;
    ny = 0;
    if (geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040209) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }

    /* Quad 2 = Inside North West */
    qx = x1 + 100;
    qy = y1 - 100;
    nx = 0;
    ny = 0;
    if (geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040210) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }

    /* Quad 3 = Inside South West */
    qx = x1 + 100;
    qy = y2 + 100;
    nx = 0;
    ny = 0;
    if (geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040211) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }

    /* Quad 4 = Inside South East */
    qx = x2 - 100;
    qy = y2 + 100;
    nx = 0;
    ny = 0;
    if (geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O040212) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }

    if (fail) {
      fprintf(stderr, "Conditions:\n");
      fprintf(stderr, "Offset %d\n", off);
      fprintf(stderr, "box upper_x=%d, x1=%d\n", box->upper_x, x1);
      fprintf(stderr, "box upper_y=%d, y1=%d\n", box->upper_y, y1);
      fprintf(stderr, "box lower_x=%d, x2=%d\n", box->lower_x, x2);
      fprintf(stderr, "box lower_y=%d, y2=%d\n", box->lower_y, y2);
      result++;
    }

    g_object_unref (object);
  }

  /* O0420 geda_box_object_shortest_distance */

  double nodist = geda_box_object_shortest_distance(NULL, 10, 10, FALSE);
  if (nodist != G_MAXDOUBLE) {
    fprintf(stderr, "FAILED: (O042000) box_shortest_distance NULL\n");
    result++;
  }

  for (count = 0; count < 3; count++) {

    int c   = m_random_number ( 0, MAX_COLORS - 1);
    int x1  = m_random_number ( 0,       119800);
    int y2  = m_random_number ( 0,        79800);
    int x2  = m_random_number (x1 + 100, 120000);
    int y1  = m_random_number (y2 + 100,  80000);
    int off = m_random_number (     100,   1000);

    GedaObject *object = geda_box_object_new(c, x1, y1, x2, y2);
    GedaBox    *box    = object->box;

    /* Quad 1 = Outside North East */
    int qx = x2 + off;
    int qy = y1;

    double distance;
    double hypotenuse = hypot(off, off);
    int  fail;

    fail = 0;

    /* O0420 geda_box_object_shortest_distance */
    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042001) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    qx = x2;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042002) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    qx = x2 + off;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != hypotenuse) {
      fprintf(stderr, "FAILED: (O042003) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 12 = North */
    qx = (x2 + x1) / 2;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042004) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 2 = Outside North West straight up from top left corner */
    qx = x1;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042005) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 2 = Outside North West diagonal from top left corner */
    qx = x1 - off;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != hypotenuse) {
      fprintf(stderr, "FAILED: (O042006) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 2 = Outside North West straight left from top left corner */
    qx = x1 - off;
    qy = y1;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042007) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 23 = Outside West side */
    qx = x1 - off;
    qy = (y1 + y2) / 2;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042008) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 3 = Outside South West, straight left from lower left corner */
    qx = x1 - off;
    qy = y2;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042009) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    /* Quad 3 = Outside South West, diagonal from from lower left corner */
    qx = x1 - off;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != hypotenuse) {
      fprintf(stderr, "FAILED: (O042010) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 3 = Outside South West, straight down from lower left corner */
    qx = x1;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042011) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 34 = Outside South straight down from middle of bottom side */
    qx = (x2 + x1) / 2;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042012) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 4 = Outside South East, straight down from lower right corner */
    qx = x2;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042013) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 4 = Outside South East, diagonal from from lower right corner */
    qx = x2 + off;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != hypotenuse) {
      fprintf(stderr, "FAILED: (O042014) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 4 = Outside South East, straight right from lower right corner */
    qx = x2 + off;
    qy = y2;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O042015) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    if (fail) {
      fprintf(stderr, "Conditions:\n");
      fprintf(stderr, "Offset %d\n", off);
      fprintf(stderr, "box upper_x=%d, x1=%d\n", box->upper_x, x1);
      fprintf(stderr, "box upper_y=%d, y1=%d\n", box->upper_y, y1);
      fprintf(stderr, "box lower_x=%d, x2=%d\n", box->lower_x, x2);
      fprintf(stderr, "box lower_y=%d, y2=%d\n", box->lower_y, y2);
      result++;
    }

    g_object_unref (object);
  }
  return result;
}

int
check_transformer()
{
  int result = 0;
  int count;

  geda_box_object_modify(NULL, 0, 0, BOX_UPPER_LEFT);

  geda_box_object_modify_all(NULL, 0, 0, 0, 0);

  geda_box_object_mirror(NULL, 0, 0);

  geda_box_object_rotate(NULL, 0, 0, 0);

  geda_box_object_translate(NULL, 0, 0);

  for (count = 0; count < 10; count++) {

    int fail = 0;

    int x1  = m_random_number ( 0, 119800);
    int y2  = m_random_number ( 0, 79800);
    int x2  = x1 + 1000;
    int y1  = y2 + 1000;
    int off = m_random_number (10, 100);

    GedaObject *object = geda_box_object_new(3, x1, y1, x2, y2);
    GedaBox    *box    = object->box;

    int nx = x1 + off;
    int ny = y1 + off;

    /* O0404 geda_box_object_modify */

    geda_box_object_modify(object, nx, ny, BOX_UPPER_LEFT);

    if (box->upper_x - nx || box->upper_y - ny) {
      fprintf(stderr, "FAILED: (O040401) box nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    nx = x1 + off;
    ny = y2 + off;

    geda_box_object_modify(object, nx, ny, BOX_LOWER_LEFT);

    if (box->upper_x - nx || box->lower_y - ny) {
      fprintf(stderr, "FAILED: (O040402) box nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    nx = x2 + off;
    ny = y1 + off;

    geda_box_object_modify(object, nx, ny, BOX_UPPER_RIGHT);

    if (box->lower_x - nx || box->upper_y - ny) {
      fprintf(stderr, "FAILED: (O040403) box nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    nx = x2 + off;
    ny = y2 + off;

    geda_box_object_modify(object, nx, ny, BOX_LOWER_RIGHT);

    if (box->lower_x - nx || box->lower_y - ny) {
      fprintf(stderr, "FAILED: (O040404) box nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    /* O0405 geda_box_object_modify_all */

    geda_box_object_modify_all(object, x1, y1, x2, y2);

    if (box->upper_x - x1 || box->upper_y - y1 ||
        box->lower_x - x2 || box->lower_y - y2)
    {
      fprintf(stderr, "FAILED: (O040501) geda_box_object_modify_all\n");
      fail++;
    }

    /* O0406 geda_box_object_mirror */
    geda_box_object_mirror(object, x2, (y1 + y2) /2);

    if (box->upper_x - x2 || box->upper_y - y1) {
      fprintf(stderr, "FAILED: (O040601) geda_box_object_mirror\n");
      fail++;
    }

    /* O0405 geda_box_object_modify_all */

    geda_box_object_modify_all(object, x1, y1, x2, y2);

    if (box->upper_x - x1 || box->upper_y - y1 ||
        box->lower_x - x2 || box->lower_y - y2)
    {
      fprintf(stderr, "FAILED: (O040502) geda_box_object_modify_all\n");
      fail++;
    }

    /* O0419 geda_box_object_rotate */
    geda_box_object_rotate(object, x1, y1, 90);

    if (box->lower_x - x2 || box->lower_y - y1) {
      fprintf(stderr, "FAILED: (O041901) geda_box_object_mirror\n");
      fail++;
    }

    /* O0421 geda_box_object_translate */
    geda_box_object_translate(object, -1000, off);
    if (box->lower_x - x1 || box->lower_y - y1 - off) {
      fprintf(stderr, "FAILED: (O042101) geda_box_object_mirror\n");
      fail++;
    }

    if (fail) {
      fprintf(stderr, "Conditions:\n");
      fprintf(stderr, "Offset %d\n", off);
      fprintf(stderr, "box upper_x=%d, x1=%d\n", box->upper_x, x1);
      fprintf(stderr, "box upper_y=%d, y1=%d\n", box->upper_y, y1);
      fprintf(stderr, "box lower_x=%d, x2=%d\n", box->lower_x, x2);
      fprintf(stderr, "box lower_y=%d, y2=%d\n", box->lower_y, y2);
      result++;
      break;
    }
    g_object_unref (object);
  }

  return result;
}

int
main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (setjmp(point) == 0) {
    result = check_construction();
  }
  else {
    fprintf(stderr, "Caught signal in constructors %s\n\n", __FILE__);
    return 1;
  }

  if (!result) {

    if (setjmp(point) == 0) {
      result = check_accessors();
    }
    else {
      fprintf(stderr, "Caught signal checking accessors in object/o_box_object.c\n\n");
      return 1;
    }

    if (setjmp(point) == 0) {
      result += check_serialization();
    }
    else {
      fprintf(stderr, "Caught signal checking serialization in src/object/o_box_object.c\n\n");
      return 1;
    }

    if (setjmp(point) == 0) {
      result += check_query();
    }
    else {
      fprintf(stderr, "Caught signal during query in src/object/o_box_object.c\n\n");
      return 1;
    }

    if (setjmp(point) == 0) {
      result += check_transformer();
    }
    else {
      fprintf(stderr, "Caught signal in transformers for src/object/o_box_object.c\n\n");
      return 1;
    }
  }
  else {
    fprintf(stderr, "discontinuing checks for object/o_box_object.c\n\n");
  }

  return result;
}
