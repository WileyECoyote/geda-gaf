/* -*- test_box_object.c -*-
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
 *  Date Contributed: April, 26th, 2016
 */

#include "../../config.h"

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

/*! \def MUT Module Under Tests */
#define MUT "src/object/o_box_object.c"

#define TOBJECT "GedaBox"

/** \defgroup test-object-geda-box Test GEDA Box object Module
 * @{
 * \brief Group 4 src/object/o_box_object.c geda_box_object_
 *  Group 4 == Module/File No.
 * \par
 *
 *  Test Identifiers:  O  04  88  88
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
 *      O0402    geda_box_object_get_end_cap
 *      O0403    geda_box_object_get_fill_angle1
 *      O0404    geda_box_object_get_fill_angle2
 *      O0405    geda_box_object_get_fill_pitch1
 *      O0406    geda_box_object_get_fill_pitch2
 *      O0407    geda_box_object_get_fill_type
 *      O0408    geda_box_object_get_fill_width
 *      O0409    geda_box_object_get_line_length
 *      O0410    geda_box_object_get_line_space
 *      O0411    geda_box_object_get_line_type
 *      O0412    geda_box_object_get_line_width
 *      O0413    geda_box_object_get_lower_x
 *      O0414    geda_box_object_get_lower_y
 *      O0415    geda_box_object_get_nearest_point
 *      O0416    geda_box_object_get_position
 *      O0417    geda_box_object_get_upper_x
 *      O0418    geda_box_object_get_upper_y
 *      O0419    geda_box_object_mirror
 *      O0420    geda_box_object_modify
 *      O0421    geda_box_object_modify_all
 *      O0422    geda_box_object_new
 *
 *               geda_box_object_print
 *               geda_box_object_print_center
 *               geda_box_object_print_dashed
 *               geda_box_object_print_dotted
 *               geda_box_object_print_filled
 *               geda_box_object_print_hatch
 *               geda_box_object_print_mesh
 *               geda_box_object_print_phantom
 *               geda_box_object_print_solid
 *      O0432    geda_box_object_read
 *      O0433    geda_box_object_rotate
 *       O0434    geda_box_object_scale
 *      O0435    geda_box_object_set_end_cap
 *      O0436    geda_box_object_set_fill_angle1
 *      O0437    geda_box_object_set_fill_angle2
 *      O0438    geda_box_object_set_fill_pitch1
 *      O0439    geda_box_object_set_fill_pitch2
 *      O0440    geda_box_object_set_fill_type
 *      O0441    geda_box_object_set_fill_width
 *      O0442    geda_box_object_set_line_length
 *      O0443    geda_box_object_set_line_space
 *      O0444    geda_box_object_set_line_type
 *      O0445    geda_box_object_set_line_width
 *      O0446    geda_box_object_set_lower_x
 *      O0447    geda_box_object_set_lower_y
 *      O0448    geda_box_object_set_upper_x
 *      O0449    geda_box_object_set_upper_y
 *      O0450    geda_box_object_shortest_distance
 *      O0451    geda_box_object_to_buffer
 *      O0452    geda_box_object_translate
 */

int check_construction (void)
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,     119900);
    int y1 = geda_random_number ( 0,      79900);
    int x2 = geda_random_number (x1 + 1, 120000);
    int y2 = geda_random_number (y1 + 1,  80000);

    GedaObject *object0 = geda_box_object_new(c, x1, y1, x2, y2);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O042201A) New GedaObject Failed\n");
      result++;
      break;   /* terminate loop if fail */
    }

    if (!GEDA_IS_BOX(object0->box)) {
      fprintf(stderr, "FAILED: (O042201B) sub-pointer not a %s\n", TOBJECT);
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
        fprintf(stderr, "FAILED: (O042201X1) box upper_x %d != %d\n", box->upper_x, x1);
        fail++;
      }

      if (box->upper_y - y1) {
        fprintf(stderr, "FAILED: (O042201Y1) box upper_y %d != %d\n", box->upper_y, y1);
        fail++;
      }

      if (box->lower_x - x2) {
        fprintf(stderr, "FAILED: (O042201X2) box lower_x %d != %d\n", box->lower_x, x2);
        fail++;
      }

      if (box->lower_y - y2) {
        fprintf(stderr, "FAILED: (O042201Y2) box lower_y %d != %d\n", box->lower_y, y2);
        fail++;
      }

      GedaObject *object1 = geda_box_object_copy(object0);

      if (!GEDA_IS_OBJECT(object1)) {
        fprintf(stderr, "FAILED: (O040101A) geda_box_object_copy\n");
        result++;
      }
      else {

        GedaBox *box2  = object1->box;

        value = geda_object_get_color (object1);
        if (value - c) {
          fprintf(stderr, "FAILED: _get_color (%s-A) %d != %d\n", TOBJECT, value, c);
          fail++;
        }

        if (box2->upper_x - x1) {
          fprintf(stderr, "FAILED: (O040101X1) box upper_x %d != %d\n", box->upper_x, x1);
          fail++;
        }

        if (box2->upper_y - y1) {
          fprintf(stderr, "FAILED: (O040101Y1) box upper_y %d != %d\n", box->upper_y, y1);
          fail++;
        }

        if (box2->lower_x - x2) {
          fprintf(stderr, "FAILED: (O040101X2) box lower_x %d != %d\n", box->lower_x, x2);
          fail++;
        }

        if (box2->lower_y - y2) {
          fprintf(stderr, "FAILED: (O040101Y2) box lower_y %d != %d\n", box->lower_y, y2);
          fail++;
        }
      }

      if (fail) {
        result++;
        break;
      }

      g_object_unref (object1);
    }
    g_object_unref (object0);
  }
  return result;
}

int check_accessors (void)
{
  int count;
  int result = 0;

  if (geda_box_object_get_end_cap(NULL)) {
    fprintf(stderr, "FAILED: (O040200) %s line end not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_fill_angle1(NULL)) {
    fprintf(stderr, "FAILED: (O040300) %s fill_angle1 not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_fill_angle2(NULL)) {
    fprintf(stderr, "FAILED: (O040400) %s fill_angle2 not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_fill_pitch1(NULL)) {
    fprintf(stderr, "FAILED: (O040500) %s fill_pitch1 not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_fill_pitch2(NULL)) {
    fprintf(stderr, "FAILED: (O040600) %s fill_pitch2 not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_fill_type(NULL)) {
    fprintf(stderr, "FAILED: (O040700) %s fill_type not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_fill_width(NULL)) {
    fprintf(stderr, "FAILED: (O040800) %s fill_width not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_line_length(NULL)) {
    fprintf(stderr, "FAILED: (O040900) %s line length not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_line_space(NULL)) {
    fprintf(stderr, "FAILED: (O041000) %s line space not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_line_type(NULL)) {
    fprintf(stderr, "FAILED: (O041100) %s line x type zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_line_width(NULL)) {
    fprintf(stderr, "FAILED: (O041200) %s line width not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_lower_x(NULL)) {
    fprintf(stderr, "FAILED: (O041300) %s lower x not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_lower_y(NULL)) {
    fprintf(stderr, "FAILED: (O041400) %s lower y not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_upper_x(NULL)) {
    fprintf(stderr, "FAILED: (O041700) %s upper x not zero\n", TOBJECT);
    result++;
  }

  if (geda_box_object_get_upper_y(NULL)) {
    fprintf(stderr, "FAILED: (O041800) %s upper y not zero\n", TOBJECT);
    result++;
  }

  for (count = 0; count < 3; count++) {

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       119800);
    int y2 = geda_random_number ( 0,        79800);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y1 = geda_random_number (y2 + 100,  80000);

    /* Line type options */
    int e = geda_random_number (END_NONE, END_ROUND);
    int t = geda_random_number (TYPE_SOLID, TYPE_PHANTOM);
    int l = geda_random_number (5, 500);
    int p = geda_random_number (5, 500);
    int w = geda_random_number (0, 500);

    /* Filling options */
    int ft  = geda_random_number (FILLING_HOLLOW, FILLING_HATCH);
    int fw  = geda_random_number (0, 100);
    int fa1 = geda_random_number (0, 180);
    int fp1 = geda_random_number (0, 500);
    int fa2 = geda_random_number (0, 180);
    int fp2 = geda_random_number (0, 500);

    GedaObject *object0 = geda_box_object_new(c, 21, 31, 41, 51);

    int px, py;
    int fail;
    int value;

    fail = 0;

    /* Check line type properties */

    geda_box_object_set_end_cap (object0, e);

    value = object0->line_options->line_end;
    if (value - e) {
      fprintf(stderr, "FAILED: (O043501) %d != %d\n", value, e);
      fail++;
    }

    value = geda_box_object_get_end_cap(object0);

    if (value - e) {
      fprintf(stderr, "FAILED: (O040201) %d != %d\n", value, e);
      fail++;
    }

    geda_box_object_set_line_length (object0, l);

    value = object0->line_options->line_length;
    if (value - l) {
      fprintf(stderr, "FAILED: (O044201) %d != %d\n", value, l);
      fail++;
    }

    value = geda_box_object_get_line_length(object0);

    if (value - l) {
      fprintf(stderr, "FAILED: (O040901) %d != %d\n", value, l);
      fail++;
    }

    geda_box_object_set_line_space (object0, p);

    value = object0->line_options->line_space;
    if (value - p) {
      fprintf(stderr, "FAILED: (O044301) %d != %d\n", value, p);
      fail++;
    }

    value = geda_box_object_get_line_space(object0);

    if (value - p) {
      fprintf(stderr, "FAILED: (O041001) %d != %d\n", value, p);
      fail++;
    }

    geda_box_object_set_line_type (object0, t);

    value = object0->line_options->line_type;
    if (value - t) {
      fprintf(stderr, "FAILED: (O044401) %d != %d\n", value, t);
      fail++;
    }

    value = geda_box_object_get_line_type(object0);

    if (value - t) {
      fprintf(stderr, "FAILED: (O041101) %d != %d\n", value, t);
      fail++;
    }

    geda_box_object_set_line_width (object0, w);

    value = object0->line_options->line_width;
    if (value - w) {
      fprintf(stderr, "FAILED: (O044501) %d != %d\n", value, w);
      fail++;
    }

    value = geda_box_object_get_line_width(object0);

    if (value - w) {
      fprintf(stderr, "FAILED: (O041201) %d != %d\n", value, w);
      fail++;
    }

    /* Check Filling properties */

    geda_box_object_set_fill_angle1 (object0, fa1);

    value = object0->fill_options->fill_angle1;
    if (value - fa1) {
      fprintf(stderr, "FAILED: (O043601) %d != %d\n", value, fa1);
      fail++;
    }

    value = geda_box_object_get_fill_angle1(object0);

    if (value - fa1) {
      fprintf(stderr, "FAILED: (O040301) %d != %d\n", value, fa1);
      fail++;
    }

    geda_box_object_set_fill_angle2 (object0, fa2);


    value = object0->fill_options->fill_angle2;
    if (value - fa2) {
      fprintf(stderr, "FAILED: (O043701) %d != %d\n", value, fa2);
      fail++;
    }

    value = geda_box_object_get_fill_angle2(object0);

    if (value - fa2) {
      fprintf(stderr, "FAILED: (O040401) %d != %d\n", value, fa2);
      fail++;
    }

    geda_box_object_set_fill_pitch1 (object0, fp1);

    value = object0->fill_options->fill_pitch1;
    if (value - fp1) {
      fprintf(stderr, "FAILED: (O043801) %d != %d\n", value, fp1);
      fail++;
    }

    value = geda_box_object_get_fill_pitch1(object0);

    if (value - fp1) {
      fprintf(stderr, "FAILED: (O040501) %d != %d\n", value, fp1);
      fail++;
    }

    geda_box_object_set_fill_pitch2 (object0, fp2);

    value = object0->fill_options->fill_pitch2;
    if (value - fp2) {
      fprintf(stderr, "FAILED: (O043901) %d != %d\n", value, fp2);
      fail++;
    }

    value = geda_box_object_get_fill_pitch2(object0);

    if (value - fp2) {
      fprintf(stderr, "FAILED: (O040601) %d != %d\n", value, fp2);
      fail++;
    }

    geda_box_object_set_fill_type (object0, ft);

    value = object0->fill_options->fill_type;
    if (value - ft) {
      fprintf(stderr, "FAILED: (O044001) %d != %d\n", value, ft);
      fail++;
    }

    value = geda_box_object_get_fill_type(object0);

    if (value - ft) {
      fprintf(stderr, "FAILED: (O040701) %d != %d\n", value, ft);
      fail++;
    }

    geda_box_object_set_fill_width (object0, fw);

    value = object0->fill_options->fill_width;
    if (value - fw) {
      fprintf(stderr, "FAILED: (O044101) %d != %d\n", value, fw);
      fail++;
    }

    value = geda_box_object_get_fill_width(object0);

    if (value - fw) {
      fprintf(stderr, "FAILED: (O040801) %d != %d\n", value, fw);
      fail++;
    }

    /* Check Coordinate properties */

    geda_box_object_set_lower_x (object0, x1);

    value = object0->box->lower_x;
    if (value - x1) {
      fprintf(stderr, "FAILED: (O044601) lower_x %d != %d\n", value, x1);
      fail++;
    }

    value = geda_box_object_get_lower_x (object0);
    if (value - x1) {
      fprintf(stderr, "FAILED: (O041301) lower_y %d != %d\n", value, x1);
      fail++;
    }

    geda_box_object_set_lower_y (object0, y1);

    value = object0->box->lower_y;
    if (value - y1) {
      fprintf(stderr, "FAILED: (O044701) lower_y %d != %d\n", value, y1);
      fail++;
    }

    value = geda_box_object_get_lower_y (object0);
    if (value - y1) {
      fprintf(stderr, "FAILED: (O041401) lower_y %d != %d\n", value, y1);
      fail++;
    }

    geda_box_object_set_upper_x (object0, x2);

    value = object0->box->upper_x;
    if (value - x2) {
      fprintf(stderr, "FAILED: (O044801) upper_x %d != %d\n", value, x2);
      fail++;
    }

    value = geda_box_object_get_upper_x (object0);
    if (value - x2) {
      fprintf(stderr, "FAILED: (O041701) upper_x %d != %d\n", value, x2);
      fail++;
    }

    geda_box_object_set_upper_y (object0, y2);

    value = object0->box->upper_y;
    if (value - y2) {
      fprintf(stderr, "FAILED: (O044901) upper_y %d != %d\n", value, y2);
      fail++;
    }

    value = geda_box_object_get_upper_y (object0);
    if (value - y2) {
      fprintf(stderr, "FAILED: (O041801) upper_y %d != %d\n", value, y2);
      fail++;
    }


    if (!geda_box_object_get_position (object0, &px, &py)) {
      fprintf(stderr, "FAILED: (O041601A) box x1=%d, y1=%d, x2=%d, y2=%d\n", x1, y1, x2, y2);
      fail++;
    }

    if (px - x1) {
      fprintf(stderr, "FAILED: (O041601B) box %d != %d\n", px, x1);
      fail++;
    }

    if (py - y2) {
      fprintf(stderr, "FAILED: (O041601C) box %d != %d\n", py, y2);
      fail++;
    }
    if (fail) {
      result++;
    }
    g_object_unref (object0);
  }
  return result;
}

int check_serialization (void)
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

    int c  = geda_random_number ( 0, MAX_COLORS - 1);
    int x1 = geda_random_number ( 0,       119800);
    int y2 = geda_random_number ( 0,        79800);
    int x2 = geda_random_number (x1 + 100, 120000);
    int y1 = geda_random_number (y2 + 100,  80000);

    GedaObject *object0 = geda_box_object_new(c, x1, y1, x2, y2);


    char *buffer0 = geda_box_object_to_buffer (object0);
    g_object_unref (object0);

    if (!buffer0) {
      fprintf(stderr, "FAILED: (O045101A) New GedaObject Failed\n");
      result++;
      break;
    }

    GedaObject *object1 = geda_box_object_read (buffer0,
                                                version,
                                                FILEFORMAT_VERSION,
                                                NULL);

    if (!GEDA_IS_OBJECT(object1)) {
      fprintf(stderr, "FAILED: (O043201A) Read GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_BOX(object1->box)) {
      fprintf(stderr, "FAILED: (O043201B) sub-pointer not a %s\n", TOBJECT);
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
        fprintf(stderr, "FAILED: (O0451/O0432X1) box upper_x %d != %d\n", box->upper_x, x1);
        fail++;
      }

      if (box->upper_y - y1) {
        fprintf(stderr, "FAILED: (O0451/O0432Y1) box upper_y %d != %d\n", box->upper_y, y1);
        fail++;
      }

      if (box->lower_x - x2) {
        fprintf(stderr, "FAILED: (O0451/O0432X2) box lower_x %d != %d\n", box->lower_x, x2);
        fail++;
      }

      if (box->lower_y - y2) {
        fprintf(stderr, "FAILED: (O0451/O0432Y2) box lower_y %d != %d\n", box->lower_y, y2);
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
        fprintf(stderr, "FAILED: (O045201B) %s buffer mismatch\n", TOBJECT);
        fprintf(stderr, "buffer0=%s\n", buffer0);
        fprintf(stderr, "buffer1=%s\n", buffer1);
        result++;
        break;
      }

      g_free (buffer0);
      g_free (buffer1);
    }
  }
  return result;
}

int check_query(void)
{
  int  count;
  int result = 0;

  int dum = 1;

  /* _new(int color, int x1, int y1, int x2, int y2) */
  GedaObject *object = geda_box_object_new(3, 100, 200, 300, 400);

  /* bounds_valid should NOT be set */
  if (object->bounds_valid) {
    fprintf(stderr, "FAILED: (O042202A) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Virtual geda_box_bounds  === */
  if (!geda_object_bounds(object)) {
    fprintf(stderr, "FAILED: (O042202B) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* bounds_valid should be set */
  if (!object->bounds_valid) {
    fprintf(stderr, "FAILED: (O042202C) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Function 15: geda_box_object_get_nearest_point  === */

  if (geda_box_object_get_nearest_point(NULL, dum, dum, &dum, &dum)) {
    fprintf(stderr, "FAILED: (O041500) box_get_nearest_point NULL\n");
    result++;
  }

  /* === Function 16: geda_bus_object_get_position  === */

  if (geda_box_object_get_position (NULL, &dum, &dum)) {
    fprintf(stderr, "FAILED: (O041600A) object NULL\n");
    result++;
  }

  g_object_unref (object);

  for (count = 0; count < 3; count++) {

    int c   = geda_random_number ( 0, MAX_COLORS - 1);
    int x1  = geda_random_number ( 0,       119800);
    int y2  = geda_random_number ( 0,        79800);
    int x2  = geda_random_number (x1 + 100, 120000);
    int y1  = geda_random_number (y2 + 100,  80000);
    int off = geda_random_number (     100,   1000);

    GedaObject *object = geda_box_object_new(c, x1, y1, x2, y2);
    GedaBox    *box    = object->box;

    /* Quad 1 = Outside North East */
    int qx = x2 + off;
    int qy = y1 + off;

    int  nx, ny;
    int  fail;

    fail = 0;

    /* O0415 geda_box_object_get_nearest_point */

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041501A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x2 - nx || y1 - ny) {
      fprintf(stderr, "FAILED: (O041501B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 12 = Outside North */
    qx = (x2 + x1) / 2;
    qy = y1 + off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041502A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (qx - nx || y1 - ny) {
      fprintf(stderr, "FAILED: (O041502B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 2 = Outside North West*/
    qx = x1 - off;
    qy = y1 + off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041503A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x1 - nx || y1 - ny) {
      fprintf(stderr, "FAILED: (O041503B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 23 = Outside West side */
    qx = x1 - off;
    qy = (y1 + y2) / 2;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041504A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x1 - nx || qy - ny) {
      fprintf(stderr, "FAILED: (O041504B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 3 = Outside South West */
    qx = x1 - off;
    qy = y2 - off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041505A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x1 - nx || y2 - ny) {
      fprintf(stderr, "FAILED: (O041505B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 34 = Outside South */
    qx = (x2 + x1) / 2;
    qy = y2 - off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041506A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (qx - nx || y2 - ny) {
      fprintf(stderr, "FAILED: (O041506B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 4 = Outside South East */
    qx = x2 + off;
    qy = y2 - off;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041507A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x2 - nx || y2 - ny) {
      fprintf(stderr, "FAILED: (O041507B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Quad 4 = Outside East */
    qx = x2 + off;
    qy = (y1 + y2) / 2;
    nx = 0;
    ny = 0;

    if (!geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041508A) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }
    else if (x2 - nx || qy - ny) {
      fprintf(stderr, "FAILED: (O041508B) box qx=%d, qy=%d, nx=%d, ny=%d\n", qx, qy, nx, ny);
      fail++;
    }

    /* Check inside on diagonals, these should not report valid results */

    /* Quad 1 = Inside North East */
    qx = x2 - 100;
    qy = y1 - 100;
    nx = 0;
    ny = 0;
    if (geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041509) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }

    /* Quad 2 = Inside North West */
    qx = x1 + 100;
    qy = y1 - 100;
    nx = 0;
    ny = 0;
    if (geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041510) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }

    /* Quad 3 = Inside South West */
    qx = x1 + 100;
    qy = y2 + 100;
    nx = 0;
    ny = 0;
    if (geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041511) box qx=%d, qy=%d\n", qx, qy);
      fail++;
    }

    /* Quad 4 = Inside South East */
    qx = x2 - 100;
    qy = y2 + 100;
    nx = 0;
    ny = 0;
    if (geda_box_object_get_nearest_point(object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O041512) box qx=%d, qy=%d\n", qx, qy);
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

  /* O0450 geda_box_object_shortest_distance */

  double nodist = geda_box_object_shortest_distance(NULL, 10, 10, FALSE);
  if (nodist != G_MAXDOUBLE) {
    fprintf(stderr, "FAILED: (O045000) box_shortest_distance NULL\n");
    result++;
  }

  for (count = 0; count < 3; count++) {

    int c   = geda_random_number ( 0, MAX_COLORS - 1);
    int x1  = geda_random_number ( 0,       119800);
    int y2  = geda_random_number ( 0,        79800);
    int x2  = geda_random_number (x1 + 100, 120000);
    int y1  = geda_random_number (y2 + 100,  80000);
    int off = geda_random_number (     100,   1000);

    GedaObject *object = geda_box_object_new(c, x1, y1, x2, y2);
    GedaBox    *box    = object->box;

    /* Quad 1 = Outside North East */
    int qx = x2 + off;
    int qy = y1;

    double distance;
    double hypotenuse = hypot(off, off);
    int  fail;

    fail = 0;

    /* O0450 geda_box_object_shortest_distance */
    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045001) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    qx = x2;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045002) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    qx = x2 + off;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != hypotenuse) {
      fprintf(stderr, "FAILED: (O045003) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 12 = North */
    qx = (x2 + x1) / 2;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045004) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 2 = Outside North West straight up from top left corner */
    qx = x1;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045005) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 2 = Outside North West diagonal from top left corner */
    qx = x1 - off;
    qy = y1 + off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != hypotenuse) {
      fprintf(stderr, "FAILED: (O045006) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 2 = Outside North West straight left from top left corner */
    qx = x1 - off;
    qy = y1;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045007) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 23 = Outside West side */
    qx = x1 - off;
    qy = (y1 + y2) / 2;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045008) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 3 = Outside South West, straight left from lower left corner */
    qx = x1 - off;
    qy = y2;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045009) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    /* Quad 3 = Outside South West, diagonal from from lower left corner */
    qx = x1 - off;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != hypotenuse) {
      fprintf(stderr, "FAILED: (O045010) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 3 = Outside South West, straight down from lower left corner */
    qx = x1;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045011) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 34 = Outside South straight down from middle of bottom side */
    qx = (x2 + x1) / 2;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045012) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 4 = Outside South East, straight down from lower right corner */
    qx = x2;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045013) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 4 = Outside South East, diagonal from from lower right corner */
    qx = x2 + off;
    qy = y2 - off;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != hypotenuse) {
      fprintf(stderr, "FAILED: (O045014) box qx=%d, qy=%d,", qx, qy);
      fprintf(stderr, " distance=%f\n", distance);
      fail++;
    }

    /* Quad 4 = Outside South East, straight right from lower right corner */
    qx = x2 + off;
    qy = y2;

    distance = geda_box_object_shortest_distance(object, qx, qy, FALSE);

    if (distance != off) {
      fprintf(stderr, "FAILED: (O045015) box qx=%d, qy=%d,", qx, qy);
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

int check_transformer(void)
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

    int x1  = geda_random_number ( 0, 119800);
    int y2  = geda_random_number ( 0, 79800);
    int x2  = x1 + 1000;
    int y1  = y2 + 1000;
    int off = geda_random_number (10, 100);

    GedaObject *object = geda_box_object_new(3, x1, y1, x2, y2);
    GedaBox    *box    = object->box;

    int nx = x1 + off;
    int ny = y1 + off;

    /* O0420 geda_box_object_modify */

    geda_box_object_modify(object, nx, ny, BOX_UPPER_LEFT);

    if (box->upper_x - nx || box->upper_y - ny) {
      fprintf(stderr, "FAILED: (O042001) box nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    nx = x1 + off;
    ny = y2 + off;

    geda_box_object_modify(object, nx, ny, BOX_LOWER_LEFT);

    if (box->upper_x - nx || box->lower_y - ny) {
      fprintf(stderr, "FAILED: (O042002) box nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    nx = x2 + off;
    ny = y1 + off;

    geda_box_object_modify(object, nx, ny, BOX_UPPER_RIGHT);

    if (box->lower_x - nx || box->upper_y - ny) {
      fprintf(stderr, "FAILED: (O042003) box nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    nx = x2 + off;
    ny = y2 + off;

    geda_box_object_modify(object, nx, ny, BOX_LOWER_RIGHT);

    if (box->lower_x - nx || box->lower_y - ny) {
      fprintf(stderr, "FAILED: (O042004) box nx=%d, ny=%d\n", nx, ny);
      fail++;
    }

    /* O0421 geda_box_object_modify_all */

    geda_box_object_modify_all(object, x1, y1, x2, y2);

    if (box->upper_x - x1 || box->upper_y - y1 ||
        box->lower_x - x2 || box->lower_y - y2)
    {
      fprintf(stderr, "FAILED: (O042101) geda_box_object_modify_all\n");
      fail++;
    }

    /* O0419 geda_box_object_mirror */
    geda_box_object_mirror(object, x2, (y1 + y2) /2);

    if (box->upper_x - x2 || box->upper_y - y1) {
      fprintf(stderr, "FAILED: (O041901) geda_box_object_mirror\n");
      fail++;
    }

    /* O0421 geda_box_object_modify_all */

    geda_box_object_modify_all(object, x1, y1, x2, y2);

    if (box->upper_x - x1 || box->upper_y - y1 ||
        box->lower_x - x2 || box->lower_y - y2)
    {
      fprintf(stderr, "FAILED: (O042102) geda_box_object_modify_all\n");
      fail++;
    }

    /* O0433 geda_box_object_rotate */
    geda_box_object_rotate(object, x1, y1, 90);

    if (box->lower_x - x2 || box->lower_y - y1) {
      fprintf(stderr, "FAILED: (O043301) geda_box_object_rotate\n");
      fail++;
    }

    /* O0452 geda_box_object_translate */
    geda_box_object_translate(object, -1000, off);
    if (box->lower_x - x1 || box->lower_y - y1 - off) {
      fprintf(stderr, "FAILED: (O045201) geda_box_object_translate\n");
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

/** @} endgroup test-object-geda-box */

int main (int argc, char *argv[])
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
      fprintf(stderr, "Caught signal checking serialization in %s\n\n", MUT);
      return 1;
    }

    if (setjmp(point) == 0) {
      result += check_query();
    }
    else {
      fprintf(stderr, "Caught signal during query in %s\n\n", MUT);
      return 1;
    }

    if (setjmp(point) == 0) {
      result += check_transformer();
    }
    else {
      fprintf(stderr, "Caught signal in transformers for %s\n\n", MUT);
      return 1;
    }
  }
  else {
    fprintf(stderr, "discontinuing checks for %s\n\n", MUT);
  }

  return result;
}
