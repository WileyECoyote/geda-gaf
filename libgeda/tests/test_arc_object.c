/* -*- test_arc_object.c -*-
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
 *  Date Contributed: March, 6th, 2016
 */

#include "../../config.h"

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>

#include <math.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/object/o_arc_object.c"

#define TOBJECT "GedaArc"

#define USE_RANDOM_NUMBERS 1

/** \defgroup test-object-geda-arc Test GEDA Arc object Module
 * @{
 * \brief Group 2 src/object/o_arc_object.c geda_arc_object_
 *  Group 2 == Module/File No.
 */

/*
 *  Test Identifiers:  O  02  88  88
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
 *      O0201    geda_arc_object_copy
 *      O0202    geda_arc_object_get_arc_sweep
 *      O0203    geda_arc_object_get_center_x
 *      O0204    geda_arc_object_get_center_y
 *      O0205    geda_arc_object_get_end_cap
 *      O0206    geda_arc_object_get_fill_angle1
 *      O0207    geda_arc_object_get_fill_angle2
 *      O0208    geda_arc_object_get_fill_pitch1
 *      O0209    geda_arc_object_get_fill_pitch2
 *      O0210    geda_arc_object_get_fill_type
 *      O0211    geda_arc_object_get_fill_width
 *      O0212    geda_arc_object_get_line_length
 *      O0213    geda_arc_object_get_line_space
 *      O0214    geda_arc_object_get_line_type
 *      O0215    geda_arc_object_get_line_width
 *      O0216    geda_arc_object_get_nearest_point
 *      O0217    geda_arc_object_get_position
 *      O0218    geda_arc_object_get_radius
 *      O0219    geda_arc_object_get_start_angle
 *      O0220    geda_arc_object_mirror
 *      O0221    geda_arc_object_modify
 *      O0222    geda_arc_object_new
 *      O0223    geda_arc_object_print
 *      O0224    geda_arc_object_print_center
 *      O0225    geda_arc_object_print_dashed
 *      O0226    geda_arc_object_print_dotted
 *      O0227    geda_arc_object_print_phantom
 *      O0228    geda_arc_object_print_solid
 *      O0229    geda_arc_object_read
 *      O0230    geda_arc_object_rotate
 *      O0231    geda_arc_object_scale
 *      O0232    geda_arc_object_set_arc_sweep
 *      O0233    geda_arc_object_set_center_x
 *      O0234    geda_arc_object_set_center_y
 *      O0235    geda_arc_object_set_end_cap
 *      O0236    geda_arc_object_set_fill_angle1
 *      O0237    geda_arc_object_set_fill_angle2
 *      O0238    geda_arc_object_set_fill_pitch1
 *      O0239    geda_arc_object_set_fill_pitch2
 *      O0240    geda_arc_object_set_fill_type
 *      O0241    geda_arc_object_set_fill_width
 *      O0242    geda_arc_object_set_line_length
 *      O0243    geda_arc_object_set_line_space
 *      O0244    geda_arc_object_set_line_type
 *      O0245    geda_arc_object_set_line_width
 *      O0246    geda_arc_object_set_radius
 *      O0247    geda_arc_object_set_start_angle
 *      O0248    geda_arc_object_shortest_distance
 *      O0249    geda_arc_object_to_buffer
 *      O0250    geda_arc_object_translate
 *      O0251    geda_arc_object_within_sweep
 */

int check_construction (void)
{
  int count;
  int result = 0;

  for (count = 0; count < 10; count++) {

    int a = geda_random_number (0, 359);
    int c = geda_random_number (0, MAX_COLORS - 1);
    int r = geda_random_number (5, 20000);
    int s = geda_random_number (1, 359);
    int x = geda_random_number (0, 120000);
    int y = geda_random_number (0, 80000);

    GedaObject *object0 = geda_arc_object_new (c, x, y, r, a, s);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O022201A) New GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_ARC(object0->arc)) {
      fprintf(stderr, "FAILED: (O022201B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int fail;
      int value;

      fail = 0;

      /* Note: Check with NULL argument is performed in check_accessors */

      value = geda_arc_object_get_start_angle (object0);
      if (value - a) {
        fprintf(stderr, "FAILED: (O022201C) start angle %d != %d\n", value, a);
        fail++;
      }

      value = geda_object_get_color (object0);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-A) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      value = geda_arc_object_get_radius (object0);
      if (value - r) {
        fprintf(stderr, "FAILED: (O022201D) radius %d != %d\n", value, r);
        fail++;
      }

      value = geda_arc_object_get_arc_sweep (object0);
      if (value - s) {
        fprintf(stderr, "FAILED: (O022201E) arc sweep %d != %d\n", value, s);
        fail++;
      }

      value = geda_arc_object_get_center_x(object0);
      if (value - x) {
        fprintf(stderr, "FAILED: (O022201F) center x %d != %d\n", value, x);
        fail++;
      }

      value = geda_arc_object_get_center_y(object0);
      if (value - y) {
        fprintf(stderr, "FAILED: (O022201G) center y %d != %d\n", value, y);
        fail++;
      }

      if (!fail) {

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

        /* Set line type properties */

        object0->line_options->line_end    = e;
        object0->line_options->line_type   = t;
        object0->line_options->line_length = l;
        object0->line_options->line_space  = p;
        object0->line_options->line_width  = w;

        /* Set Filling properties */

        object0->fill_options->fill_type   = ft;
        object0->fill_options->fill_angle1 = fa1;
        object0->fill_options->fill_angle2 = fa2;
        object0->fill_options->fill_pitch1 = fp1;
        object0->fill_options->fill_pitch2 = fp2;
        object0->fill_options->fill_width  = fw;

        geda_arc_object_set_fill_width (object0, fw);

        GedaObject *object1 = geda_arc_object_copy (object0);

        g_object_unref (object0);

        if (!GEDA_IS_OBJECT(object1)) {
          fprintf(stderr, "FAILED: (O020101A) New GedaObject Failed\n");
          fail++;
          break;
        }

        if (!GEDA_IS_ARC(object1->arc)) {
          fprintf(stderr, "FAILED: (O020101B) sub-pointer not a %s\n", TOBJECT);
          fail++;
          break;
        }
        else {

          value = object1->arc->start_angle;
          if (value - a) {
            fprintf(stderr, "FAILED: (O020101C) start angle %d != %d\n", value, a);
            fail++;
          }

          value = geda_object_get_color (object1);
          if (value - c) {
            fprintf(stderr, "FAILED: _get_color (%s-B) %d != %d\n", TOBJECT, value, c);
            fail++;
          }

          value = object1->arc->radius;
          if (value - r) {
            fprintf(stderr, "FAILED: (O020101D) radius %d != %d\n", value, r);
            fail++;
          }

          value = object1->arc->arc_sweep;
          if (value - s) {
            fprintf(stderr, "FAILED: (O020101E) arc sweep %d != %d\n", value, s);
            fail++;
          }

          value = object1->arc->x;
          if (value - x) {
            fprintf(stderr, "FAILED: (O020101F) center x %d != %d\n", value, x);
            fail++;
          }

          value = object1->arc->y;
          if (value - y) {
            fprintf(stderr, "FAILED: (O020101G) center y %d != %d\n", value, y);
            fail++;
          }

          /* Check line type properties */

          value = object1->line_options->line_end;
          if (value - e) {
            fprintf(stderr, "FAILED: (O020101H) %d != %d\n", value, e);
            fail++;
          }

          value = object1->line_options->line_type;
          if (value - t) {
            fprintf(stderr, "FAILED: (O020101J) %d != %d\n", value, t);
            fail++;
          }

          value = object1->line_options->line_length;
          if (value - l) {
            fprintf(stderr, "FAILED: (O020101K) %d != %d\n", value, l);
            fail++;
          }

          value = object1->line_options->line_space;
          if (value - p) {
            fprintf(stderr, "FAILED: (O020101L) %d != %d\n", value, p);
            fail++;
          }

          value = object1->line_options->line_width;
          if (value - w) {
            fprintf(stderr, "FAILED: (O020101M) %d != %d\n", value, w);
            fail++;
          }

          /* Check Filling properties */

          value = object1->fill_options->fill_type;
          if (value - ft) {
            fprintf(stderr, "FAILED: (O020101N) %d != %d\n", value, ft);
            fail++;
          }

          value = object1->fill_options->fill_angle1;
          if (value - fa1) {
            fprintf(stderr, "FAILED: (O020101P) %d != %d\n", value, fa1);
            fail++;
          }

          value = object1->fill_options->fill_angle2;
          if (value - fa2) {
            fprintf(stderr, "FAILED: (O020101Q) %d != %d\n", value, fa2);
            fail++;
          }

          value = object1->fill_options->fill_pitch1;
          if (value - fp1) {
            fprintf(stderr, "FAILED: (O020101R) %d != %d\n", value, fp1);
            fail++;
          }

          value = object1->fill_options->fill_pitch2;
          if (value - fp2) {
            fprintf(stderr, "FAILED: (O020101S) %d != %d\n", value, fp2);
            fail++;
          }

          value = object1->fill_options->fill_width;
          if (value - fw) {
            fprintf(stderr, "FAILED: (O020101T) %d != %d\n", value, fw);
            fail++;
          }
        }

        if (G_IS_OBJECT(object1)) {
          g_object_unref (object1);
        }
      }
      else {
        g_object_unref (object0);
      }

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to get or set %d %s propert%s\n", fail, TOBJECT,                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tstart angle: %d\n", a);
        fprintf(stderr, "\t     radius: %d\n", r);
        fprintf(stderr, "\t  arc sweep: %d\n", s);
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);

        result = fail;
        break;
      }
    }
  }
  return result;
}

int check_accessors (void)
{
  int count;
  int result = 0;

  if (geda_arc_object_get_arc_sweep(NULL)) {
    fprintf(stderr, "FAILED: (O020200) %s arc sweep not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_center_x(NULL)) {
    fprintf(stderr, "FAILED: (O020300) %s center x not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_center_y(NULL)) {
    fprintf(stderr, "FAILED: (O020400) %s center y not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_radius(NULL)) {
    fprintf(stderr, "FAILED: (O021800) %s radius not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_start_angle(NULL)) {
    fprintf(stderr, "FAILED: (O021900) %s start_angle not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_end_cap(NULL)) {
    fprintf(stderr, "FAILED: (O020500) %s end_cap not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_line_length(NULL)) {
    fprintf(stderr, "FAILED: (O021200) %s line_length not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_line_space(NULL)) {
    fprintf(stderr, "FAILED: (O021300) %s line_space not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_line_type(NULL)) {
    fprintf(stderr, "FAILED: (O021400) %s line_type not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_line_width(NULL)) {
    fprintf(stderr, "FAILED: (O021500) %s line_width not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_fill_angle1(NULL)) {
    fprintf(stderr, "FAILED: (O020600) %s fill_angle1 not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_fill_angle2(NULL)) {
    fprintf(stderr, "FAILED: (O020700) %s fill_angle2 not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_fill_pitch1(NULL)) {
    fprintf(stderr, "FAILED: (O020800) %s fill_pitch1 not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_fill_pitch2(NULL)) {
    fprintf(stderr, "FAILED: (O020900) %s fill_pitch2 not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_fill_type(NULL)) {
    fprintf(stderr, "FAILED: (O021000) %s fill_type not zero\n", TOBJECT);
    result++;
  }

  if (geda_arc_object_get_fill_width(NULL)) {
    fprintf(stderr, "FAILED: (O021100) %s fill_width not zero\n", TOBJECT);
    result++;
  }

  for (count = 0; count < 10; count++) {

    GedaObject *object0 = geda_arc_object_new (11, 21, 31, 41, 51, 61);

    if (!GEDA_IS_OBJECT(object0)) {
      fprintf(stderr, "FAILED: (O022201H) New GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_ARC(object0->arc)) {
      fprintf(stderr, "FAILED: (O022201J) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int fail;
      int value;

      fail = 0;

#if USE_RANDOM_NUMBERS

      int a = geda_random_number (0, 359);
      int c = geda_random_number (0, MAX_COLORS - 1);
      int r = geda_random_number (5, 20000);
      int s = geda_random_number (1, 359);
      int x = geda_random_number (0, 120000);
      int y = geda_random_number (0, 80000);

#else
      int a = 17;
      int c = 0;
      int r = 14761;
      int s = 33;
      int x = 87101;
      int y = 46399;
#endif

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

      geda_arc_object_set_start_angle (object0, a);
      geda_set_object_color (object0, c);
      geda_arc_object_set_radius (object0, r);
      geda_arc_object_set_arc_sweep (object0, s);
      geda_arc_object_set_center_x (object0, x);
      geda_arc_object_set_center_y (object0, y);

      value = geda_arc_object_get_start_angle (object0);
      if (value - a) {
        fprintf(stderr, "FAILED: (O024701) start angle %d != %d\n", value, a);
        fail++;
      }

      value = geda_object_get_color (object0);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-C) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      value = geda_arc_object_get_radius (object0);
      if (value - r) {
        fprintf(stderr, "FAILED: (O024601) radius %d != %d\n", value, r);
        fail++;
      }

      value = geda_arc_object_get_arc_sweep (object0);
      if (value - s) {
        fprintf(stderr, "FAILED: (O023201) arc sweep %d != %d\n", value, s);
        fail++;
      }

      value = geda_arc_object_get_center_x(object0);
      if (value - x) {
        fprintf(stderr, "FAILED: (O023301) center x %d != %d\n", value, x);
        fail++;
      }

      value = geda_arc_object_get_center_y(object0);
      if (value - y) {
        fprintf(stderr, "FAILED: (O023401) center y %d != %d\n", value, y);
        fail++;
      }

      /* Check line type properties */

      geda_arc_object_set_end_cap (object0, e);

      value = object0->line_options->line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: (O023501) %d != %d\n", value, e);
        fail++;
      }

      value = geda_arc_object_get_end_cap(object0);

      if (value - e) {
        fprintf(stderr, "FAILED: (O020501) %d != %d\n", value, e);
        fail++;
      }

      geda_arc_object_set_line_length (object0, l);

      value = object0->line_options->line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: (O024201) %d != %d\n", value, l);
        fail++;
      }

      value = geda_arc_object_get_line_length(object0);

      if (value - l) {
        fprintf(stderr, "FAILED: (O021201) %d != %d\n", value, l);
        fail++;
      }

      geda_arc_object_set_line_space (object0, p);

      value = object0->line_options->line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: (O024301) %d != %d\n", value, p);
        fail++;
      }

      value = geda_arc_object_get_line_space(object0);

      if (value - p) {
        fprintf(stderr, "FAILED: (O021301) %d != %d\n", value, p);
        fail++;
      }

      geda_arc_object_set_line_type (object0, t);

      value = object0->line_options->line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: (O024401) %d != %d\n", value, t);
        fail++;
      }

      value = geda_arc_object_get_line_type(object0);

      if (value - t) {
        fprintf(stderr, "FAILED: (O021401) %d != %d\n", value, t);
        fail++;
      }

      geda_arc_object_set_line_width (object0, w);

      value = object0->line_options->line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: (O024501) %d != %d\n", value, w);
        fail++;
      }

      value = geda_arc_object_get_line_width(object0);

      if (value - w) {
        fprintf(stderr, "FAILED: (O021501) %d != %d\n", value, w);
        fail++;
      }

      /* Check Filling properties */

      geda_arc_object_set_fill_angle1 (object0, fa1);

      value = object0->fill_options->fill_angle1;
      if (value - fa1) {
        fprintf(stderr, "FAILED: (O023601) %d != %d\n", value, fa1);
        fail++;
      }

      value = geda_arc_object_get_fill_angle1(object0);

      if (value - fa1) {
        fprintf(stderr, "FAILED: (O020601) %d != %d\n", value, fa1);
        fail++;
      }

      geda_arc_object_set_fill_angle2 (object0, fa2);


      value = object0->fill_options->fill_angle2;
      if (value - fa2) {
        fprintf(stderr, "FAILED: (O023701) %d != %d\n", value, fa2);
        fail++;
      }

      value = geda_arc_object_get_fill_angle2(object0);

      if (value - fa2) {
        fprintf(stderr, "FAILED: (O020701) %d != %d\n", value, fa2);
        fail++;
      }

      geda_arc_object_set_fill_pitch1 (object0, fp1);

      value = object0->fill_options->fill_pitch1;
      if (value - fp1) {
        fprintf(stderr, "FAILED: (O023801) %d != %d\n", value, fp1);
        fail++;
      }

      value = geda_arc_object_get_fill_pitch1(object0);

      if (value - fp1) {
        fprintf(stderr, "FAILED: (O020801) %d != %d\n", value, fp1);
        fail++;
      }

      geda_arc_object_set_fill_pitch2 (object0, fp2);

      value = object0->fill_options->fill_pitch2;
      if (value - fp2) {
        fprintf(stderr, "FAILED: (O023901) %d != %d\n", value, fp2);
        fail++;
      }

      value = geda_arc_object_get_fill_pitch2(object0);

      if (value - fp2) {
        fprintf(stderr, "FAILED: (O020901) %d != %d\n", value, fp2);
        fail++;
      }

      geda_arc_object_set_fill_type (object0, ft);

      value = object0->fill_options->fill_type;
      if (value - ft) {
        fprintf(stderr, "FAILED: (O024001) %d != %d\n", value, ft);
        fail++;
      }

      value = geda_arc_object_get_fill_type(object0);

      if (value - ft) {
        fprintf(stderr, "FAILED: (O021001) %d != %d\n", value, ft);
        fail++;
      }

      geda_arc_object_set_fill_width (object0, fw);

      value = object0->fill_options->fill_width;
      if (value - fw) {
        fprintf(stderr, "FAILED: (O024101) %d != %d\n", value, fw);
        fail++;
      }

      value = geda_arc_object_get_fill_width(object0);

      if (value - fw) {
        fprintf(stderr, "FAILED: (O021101) %d != %d\n", value, fw);
        fail++;
      }

      g_object_unref (object0);

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tstart angle: %d\n", a);
        fprintf(stderr, "\t     radius: %d\n", r);
        fprintf(stderr, "\t  arc sweep: %d\n", s);
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);

        result = result + fail;
        break;
      }
    }
  }
  return result;
}

int
check_serialization (void)
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

    int a = geda_random_number (0, 359);
    int c = geda_random_number (0, MAX_COLORS - 1);
    int r = geda_random_number (5, 20000);
    int s = geda_random_number (1, 359);
    int x = geda_random_number (0, 120000);
    int y = geda_random_number (0, 80000);

    GedaObject *object0 = geda_arc_object_new (c, x, y, r, a, s);

    char *buffer0 = geda_arc_object_to_buffer (object0);
    g_object_unref (object0);

    if (!buffer0) {
      fprintf(stderr, "FAILED: (O024901A) New GedaObject Failed\n");
      result++;
      break;
    }

    GedaObject *object1 = geda_arc_object_read (buffer0,
                                                version,
                                                FILEFORMAT_VERSION,
                                                NULL);

    if (!GEDA_IS_OBJECT(object1)) {
      fprintf(stderr, "FAILED: (O022901A) Read GedaObject Failed\n");
      result++;
      break;
    }

    if (!GEDA_IS_ARC(object1->arc)) {
      fprintf(stderr, "FAILED: (O022901B) sub-pointer not a %s\n", TOBJECT);
      result++;
      break;
    }
    else {

      int fail;
      int value;

      fail = 0;

      value = geda_arc_object_get_start_angle (object1);
      if (value - a) {
        fprintf(stderr, "FAILED: (O0249/O0229A) start angle %d != %d\n", value, a);
        fail++;
      }

      value = geda_object_get_color (object1);
      if (value - c) {
        fprintf(stderr, "FAILED: _get_color (%s-D) %d != %d\n", TOBJECT, value, c);
        fail++;
      }

      value = geda_arc_object_get_radius (object1);
      if (value - r) {
        fprintf(stderr, "FAILED: (O0249/O0229B) radius %d != %d\n", value, r);
        fail++;
      }

      value = geda_arc_object_get_arc_sweep (object1);
      if (value - s) {
        fprintf(stderr, "FAILED: (O0249/O0229C) arc sweep %d != %d\n", value, s);
        fail++;
      }

      value = geda_arc_object_get_center_x(object1);
      if (value - x) {
        fprintf(stderr, "FAILED: (O0249/O0229D) center x %d != %d\n", value, x);
        fail++;
      }

      value = geda_arc_object_get_center_y(object1);
      if (value - y) {
        fprintf(stderr, "FAILED: (O0249/O0229E) center y %d != %d\n", value, y);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "Test Function: %s, in loop index %d\n", __func__, count);
        fprintf(stderr, "failed to read/write %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tstart angle: %d\n", a);
        fprintf(stderr, "\t     radius: %d\n", r);
        fprintf(stderr, "\t  arc sweep: %d\n", s);
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);

        result = result + fail;
        break;
      }

      char *buffer1 = geda_arc_object_to_buffer (object1);
      g_object_unref (object1);

      if (strcmp (buffer0, buffer1)) {
        fprintf(stderr, "FAILED: (O024901B) %s buffer mismatch\n", TOBJECT);
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

int query_nearest_Q13_90 (GedaObject *object)
{
  int result = 0;

  int nx;
  int ny;

  int a = geda_arc_object_get_start_angle (object);
  int x = geda_arc_object_get_center_x (object);
  int y = geda_arc_object_get_center_y (object);
  int r = geda_arc_object_get_radius (object);
  int s = geda_arc_object_get_arc_sweep (object);

  /* Starting point of the Arc */
  int spx;
  int spy;

  /* Ending point of the Arc */
  int epx;
  int epy;

  switch (a) {
    case 0:
      spx = x + r;
      spy = y;
      epx = x;
      epy = y + r;
      break;
    case 180:
      spx = x - r;
      spy = y;
      epx = x;
      epy = y - r;
      break;
    default:
     fprintf(stderr, "FAILED: %s unexpected start angle <%d>\n", __func__, a);
     result++;
     return result;
  }

  /* Try nearest the center point */
  if (geda_arc_object_get_nearest_point (object, x, y, &nx, &ny)) {
    fprintf(stderr, "FAILED: (O021601-Q13-90) nearest arc center point\n");
    result++;
  }

  nx = 0;
  ny = 0;

  int qx = 0;
  int qy = 0;

  switch (a) {
    case 0:
      qy = y - 10;  /* South 10 units */
      break;
    case 180:
      qy = y + 10;  /* North 10 units */
  }

  /* Q1 below, Q3 above */
  /* Sweep X about, the starting Y should return the starting point */
  for (qx = spx - 5; qx < spx + 10; qx = qx + 5) {
    if (!geda_arc_object_get_nearest_point (object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O021602-Q13-90SA) nearest arc object point\n");
      result++;
      break;
    }
    else {
      if ((nx - spx > 1) || (ny - spy > 1)) {
        fprintf(stderr, "\nFAILED: (O021602-Q13-90SB) nearest arc object point ((%d,%d)\n", nx, ny);
        fprintf(stderr, "with input conditions     (  x=%d,\t  y=%d,\t  r=%d,\t  a=%d)\n", x, y, r, a);
        fprintf(stderr, "calculated starting point (spx=%d,\tspy=%d,\tepx=%d,\tepy=%d)\n", spx, spy, epx, epy);
        fprintf(stderr, "query point               ( qx=%d,\t qy=%d)\n\n", qx, qy);
        result++;
        break;
      }
    }
  }

  nx = 0;
  ny = 0;

  switch (a) {
    case 0:
      qx = epx - 10;  /* West 10 units */
      break;
    case 180:
      qx = epx + 10;  /* East 10 units */
  }

  /* Q1 west, Q3 east */
  /* Sweep Y about the ending X should return the ending point */
  for (qy = epy - 5; qy < epy + 10; qy = qy + 5) {
    if (!geda_arc_object_get_nearest_point (object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O021603-Q13-90EA) nearest arc object point\n");
      result++;
      break;
    }
    else {
      if ((nx - epx > 1) || (ny - epy > 1)) {
        fprintf(stderr, "\nFAILED: (O021603-Q13-90EB) nearest arc object point ((%d,%d)\n", nx, ny);
        fprintf(stderr, "with input conditions  (  x=%d,\t  y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
        fprintf(stderr, "calculated end-point   (epx=%d,\tepy=%d)\n", epx, epy);
        fprintf(stderr, "query point            ( qx=%d,\t qy=%d)\n\n", qx, qy);
        result++;
        break;
      }
    }
  }

  nx = 0;
  ny = 0;

  /* Convert arc sweep to radian measure */
  double sweep = s * M_PI / 180.0;
  double angle = a * M_PI / 180.0;

  /* Get the first point of the sagitta of this Arc */
  int sag_px = (spx + epx) / 2;
  int sag_py = (spy + epy) / 2;

  /* So the midpoint of the arc will be ... */
  int mpx = x + rint(r * cos(angle + sweep/2));
  int mpy = y + rint(r * sin(angle + sweep/2));

  /* Which will be the point on the arc nearest the 1st point of the sagitta  */

  if (!geda_arc_object_get_nearest_point (object, sag_px, sag_py, &nx, &ny)) {
    fprintf(stderr, "FAILED: (O021604-Q13-90A) nearest arc object point\n");
    result++;
  }
  else {
    if  ((nx - mpx > 1) || (ny - mpy > 1)) {
      fprintf(stderr, "\nFAILED: (O021604-Q13-90B) nearest arc object point ((%d,%d)\n", nx, ny);
      fprintf(stderr, "with input conditions (     x=%d,\t      y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
      fprintf(stderr, " calculated midpoint  (   mpx=%d,\t    mpy=%d)\n", mpx, mpy);
      fprintf(stderr, "calculated end-point  (   spx=%d,\t    spy=%d)\n", spx, spy);
      fprintf(stderr, "calculated end-point  (   epx=%d,\t    epy=%d)\n", epx, epy);
      fprintf(stderr, " query point          (sag_px=%d,\t sag_py=%d)\n\n", sag_px, sag_py);
      result++;
    }
  }

  nx = 0;
  ny = 0;

  /* And if we just use the mid-point then ...*/
  if (!geda_arc_object_get_nearest_point (object, mpx, mpy, &nx, &ny)) {
    fprintf(stderr, "FAILED: (O021605-Q13-90A) nearest arc object point\n");
    result++;
  }
  else {
    if ((nx - mpx > 1) || (ny - mpy > 1)) {
      fprintf(stderr, "\nFAILED: (O021605-Q13-90B) nearest arc object point ((%d,%d)\n", nx, ny);
      fprintf(stderr, "with input conditions (  x=%d,\t   y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
      fprintf(stderr, " calculated midpoint  (mpx=%d,\t mpy=%d)\n", mpx, mpy);
      fprintf(stderr, " query point          (mpx=%d,\t mpy=%d)\n\n", mpx, mpy);
      result++;
    }
  }

  nx = 0;
  ny = 0;

  /* Finally check on the exterior/far-side of the bulge */
  if (!geda_arc_object_get_nearest_point (object, mpx + 5, mpy + 5, &nx, &ny)) {
    fprintf(stderr, "FAILED: (O021606-Q13-90A) nearest arc object point\n");
    fprintf(stderr, "with input conditions (  x=%d,\t   y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
    fprintf(stderr, " calculated midpoint  (mpx=%d,\t mpy=%d)\n", mpx, mpy);
    fprintf(stderr, " query point          (mpx=%d,\t mpy=%d)\n\n", mpx + 5, mpy + 5);
    fprintf(stderr, " returned INVALID     ( nx=%d,\t  ny=%d)\n\n", nx, ny);
    result++;
  }
  else {
    if ((nx - mpx > 1) || (ny - mpy > 1)) {
      fprintf(stderr, "\nFAILED: (O021606-Q13-90B) nearest arc object point ((%d,%d)\n", nx, ny);
      fprintf(stderr, "with input conditions (  x=%d,\t   y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
      fprintf(stderr, " calculated midpoint  (mpx=%d,\t mpy=%d)\n\n", mpx, mpy);
      result++;
    }
  }

  return result;
}

int query_nearest_Q24_90 (GedaObject *object)
{
  int result = 0;

  int nx;
  int ny;

  int a = geda_arc_object_get_start_angle (object);
  int x = geda_arc_object_get_center_x (object);
  int y = geda_arc_object_get_center_y (object);
  int r = geda_arc_object_get_radius (object);
  int s = geda_arc_object_get_arc_sweep (object);

  /* Starting point of the Arc */
  int spx;
  int spy;

  /* Ending point of the Arc */
  int epx;
  int epy;

  switch (a) {
    case 90:
      spx = x;
      spy = y + r;
      epx = x - r;
      epy = y;
      break;
    case 270:
      spx = x;
      spy = y - r;
      epx = x + r;
      epy = y;
      break;
    default:
     fprintf(stderr, "FAILED: %s unexpected start angle <%d>\n", __func__, a);
     result++;
     return result;
  }

  /* Try nearest the center point */
  if (geda_arc_object_get_nearest_point (object, x, y, &nx, &ny)) {
    fprintf(stderr, "FAILED: (O021601-Q24-90) nearest arc center point\n");
    result++;
  }

  nx = 0;
  ny = 0;

  int qx = 0;
  int qy = 0;

  switch (a) {
    case 90:
      qy = epy - 10;  /* South 10 units */
      break;
    case 270:
      qy = epy + 10;  /* North 10 units */
  }

  /* Q2 below, Q4 above ending point for X = {-5, 0, +5}  */
  /* Sweep X below the ending Y should return the end point */
  for (qx = epx - 5; qx < epx + 10; qx = qx + 5) {
    if (!geda_arc_object_get_nearest_point (object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O021602-Q24-90EA) nearest arc object point\n");
      result++;
      break;
    }
    else {
      if ((nx - epx > 1) || (ny - epy > 1)) {
        fprintf(stderr, "\nFAILED: (O021602-Q24-90EB) nearest arc object point ((%d,%d)\n", nx, ny);
        fprintf(stderr, "with input conditions (  x=%d,\t  y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
        fprintf(stderr, "calculated end point  (epx=%d,\tepy=%d)\n", epx, epy);
        fprintf(stderr, "query point           ( qx=%d,\t qy=%d)\n\n", qx, qy);
        result++;
        break;
      }
    }
  }

  nx = 0;
  ny = 0;

  switch (a) {
    case 90:
      qx = spx + 10;  /* East 10 units */
      break;
    case 270:
      qx = spx - 10;  /* West 10 units */
  }

  /* Q2 East, Q4 West of start point for Y = {-5, 0, +5} */
  /* Sweep Y about the starting X should return the starting point */
  for (qy = spy - 5; qy < spy + 10; qy = qy + 5) {
    if (!geda_arc_object_get_nearest_point (object, qx, qy, &nx, &ny)) {
      fprintf(stderr, "FAILED: (O021603-Q24-90SA) nearest arc object point\n");
      result++;
      break;
    }
    else {
      if ((nx - spx > 1) || (ny - spy > 1)) {
        fprintf(stderr, "\nFAILED: (O021603-Q24-90SB) nearest arc object point ((%d,%d)\n", nx, ny);
        fprintf(stderr, "with input conditions  (  x=%d,\t  y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
        fprintf(stderr, "calculated start-point (spx=%d,\tspy=%d)\n", spx, spy);
        fprintf(stderr, "query point            ( qx=%d,\t qy=%d)\n\n", qx, qy);
        result++;
        break;
      }
    }
  }

  nx = 0;
  ny = 0;

  /* Convert arc sweep to radian measure */
  double sweep = s * M_PI / 180.0;
  double angle = a * M_PI / 180.0;

  /* Get the first point of the sagitta of this Arc */
  int sag_px = (spx + epx) / 2;
  int sag_py = (spy + epy) / 2;

  /* So the midpoint of the arc will be ... */
  int mpx = x + rint(r * cos(angle + sweep / 2));
  int mpy = y + rint(r * sin(angle + sweep / 2));

  /* Which will be the point on the arc nearest the 1st point of the sagitta  */

  if (!geda_arc_object_get_nearest_point (object, sag_px, sag_py, &nx, &ny)) {
    fprintf(stderr, "FAILED: (O021604-Q24-90A) nearest arc object point\n");
    result++;
  }
  else {
    if ((nx - mpx > 1) || (ny - mpy > 1)) {
      fprintf(stderr, "\nFAILED: (O021604-Q24-90B) nearest arc object point ((%d,%d)\n", nx, ny);
      fprintf(stderr, "with input conditions (     x=%d,\t      y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
      fprintf(stderr, " calculated end-point (   spx=%d,\t    spy=%d)\n", spx, spy);
      fprintf(stderr, " calculated end-point (   epx=%d,\t    epy=%d)\n", epx, epy);
      fprintf(stderr, " calculated midpoint  (   mpx=%d,\t    mpy=%d)\n", mpx, mpy);
      fprintf(stderr, " query point          (sag_px=%d,\t sag_py=%d)\n\n", sag_px, sag_py);
      result++;
    }
  }

  nx = 0;
  ny = 0;

  /* And if we just use the mid-point then ... */
  if (!geda_arc_object_get_nearest_point (object, mpx, mpy, &nx, &ny)) {
    fprintf(stderr, "FAILED: (O021605-Q24-90A) nearest arc object point\n");
    result++;
  }
  else {
    if ((nx - mpx > 1) || (ny - mpy > 1)) {
      fprintf(stderr, "\nFAILED: (O021605-Q24-90B) nearest arc object point ((%d,%d)\n", nx, ny);
      fprintf(stderr, "with input conditions (     x=%d,\t      y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
      fprintf(stderr, " calculated midpoint  (mpx=%d,\t mpy=%d)\n", mpx, mpy);
      fprintf(stderr, " query point          (mpx=%d,\t mpy=%d)\n\n", mpx, mpy);
      result++;
    }
  }

  nx = 0;
  ny = 0;

  /* Finally check on the exterior/far-side of the bulge */
  if (!geda_arc_object_get_nearest_point (object, mpx - 5, mpy + 5, &nx, &ny)) {
    fprintf(stderr, "FAILED: (O021606-Q24-90A) nearest arc object point\n");
    fprintf(stderr, "with input conditions (  x=%d,\t   y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
    fprintf(stderr, " calculated midpoint  (mpx=%d,\t mpy=%d)\n", mpx, mpy);
    fprintf(stderr, " query point          (mpx=%d,\t mpy=%d)\n\n", mpx + 5, mpy + 5);
    fprintf(stderr, " returned INVALID     ( nx=%d,\t  ny=%d)\n\n", nx, ny);
    result++;
  }
  else {
    if ((nx - mpx > 1) || (ny - mpy > 1)) {
      fprintf(stderr, "\nFAILED: (O021606-Q24-90B) nearest arc object point ((%d,%d)\n", nx, ny);
      fprintf(stderr, "with input conditions (     x=%d,\t      y=%d,\t r=%d,\t a=%d)\n", x, y, r, a);
      fprintf(stderr, " calculated midpoint  (   mpx=%d,\t    mpy=%d)\n\n", mpx, mpy);
      result++;
    }
  }

  return result;
}

/* Is somewhat of a kludge; get distance to a random point (px,py)
 * then gets the nearest point on the arc and compares D(px,py) to
 * the distance from (px,py) to the (nx,ny). If the rounded integer
 * differ by less than 1 then result is considered correct. This
 * assume previously tested geda_arc_object_get_nearest_point passed.
 */
int query_nearest_shortest_distance (GedaObject *object)
{
  int result = 0;

  int nx;
  int ny;

  int px = geda_random_number (0, 120000);
  int py = geda_random_number (0, 80000);

  double shortest;

  shortest = geda_arc_object_shortest_distance (object, px, py, 1);

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
        fprintf(stderr, "FAILED: (O024801) (%d,%d) %f incorrect\n", px, py, shortest);
        result++;
      }
    }
    else {
      fprintf(stderr, "skipped shortest_distance\n");
    }
  }

  return result;
}

int check_query (void)
{
  int result = 0;
  int dx, dy;

  /* _new(int color, int x, int y, int radius, int start_angle, int arc_sweep) */
  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  /* bounds_valid should NOT be set */
  if (object->bounds_valid) {
    fprintf(stderr, "FAILED: (O022202A) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Virtual geda_arc_bounds  === */
  if (!geda_object_bounds(object)) {
    fprintf(stderr, "FAILED: (O022202B) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* bounds_valid should be set */
  if (!object->bounds_valid) {
    fprintf(stderr, "FAILED: (O022202C) %s bounds_valid %d\n", TOBJECT, object->bounds_valid);
    result++;
  }

  /* === Function 05: geda_arc_object_get_nearest_point NULL === */

  if (geda_arc_object_get_nearest_point (NULL, 0, 0, &dx, &dy)) {
    fprintf(stderr, "FAILED: (O021600) arc_object_get_nearest_point NULL\n");
    result++;
  }

  /* === Function 17: geda_arc_object_get_position NULL === */

  if (geda_arc_object_get_position(NULL, &dx, &dx)) {
    fprintf(stderr, "FAILED: (O021700A) %s get_position NULL not FALSE\n", TOBJECT);
    result++;
  }
    result++;
  }

  /* === Function 25: geda_arc_object_shortest_distance NULL === */
  if (G_MAXDOUBLE != geda_arc_object_shortest_distance(NULL, dx, dx, 0)) {
    fprintf(stderr, "FAILED: (O024800) %s answer not G_MAXDOUBLE\n", TOBJECT);
    result++;
  }

  /* === Function 28: geda_arc_object_within_sweep NULL === */

  if (geda_arc_object_within_sweep(NULL, dx, dx)) {
    fprintf(stderr, "FAILED: (O025100) %s answer not FALSE\n", TOBJECT);
    result++;
  }

  int count;

  for (count = 0; count < 10; count++) {

#if USE_RANDOM_NUMBERS
    int x = geda_random_number ( 0, 120000);
    int y = geda_random_number ( 0, 80000);
    int r = geda_random_number (10, 5000);
#else
    int x = 43766; //1000;
    int y = 63418; //1000;
    int r = 10;    //100;
#endif

    geda_arc_object_set_radius (object, r);
    geda_arc_object_set_center_x (object, x);
    geda_arc_object_set_center_y (object, y);

  /* === Function 05: geda_arc_object_get_nearest_point === */

    /* insure starting angle is reset after looping */
    geda_arc_object_set_start_angle(object, 0);

    result += query_nearest_Q13_90(object);

    geda_arc_object_set_start_angle(object, 90);

    result += query_nearest_Q24_90(object);

    geda_arc_object_set_start_angle(object, 180);

    result += query_nearest_Q13_90(object);

    geda_arc_object_set_start_angle(object, 270);

    result += query_nearest_Q24_90(object);

  /* === Function 06: geda_arc_object_get_position === */

    geda_arc_object_get_position(object, &dx, &dy);

    if ((dx != x) || (dy != y)) {
      fprintf(stderr, "FAILED: (O021701) (%d,%d) != (%d,%d)\n", x, y, dx, dy);
      result++;
    }

  /* === Function 25: geda_arc_object_shortest_distance === */

    result += query_nearest_shortest_distance(object);

  /* === Function 28: geda_arc_object_within_sweep === */

    /* see test_geda_arc_within_sweep */

#if !USE_RANDOM_NUMBERS
    break;
#endif

  }

  g_object_unref (object);

  return result;
}

int check_transformer (void)
{
  int result = 0;

  /* === Function 20: geda_arc_object_mirror NULL === */
  if (setjmp(point) == 0) {
    geda_arc_object_mirror (NULL, 0, 0);
  }
  else {
    fprintf(stderr, "FAILED: (O022000) geda_arc_object_mirror NULL\n");
    result++;
  }

  /* === Function 21: geda_arc_object_modify NULL === */
  if (setjmp(point) == 0) {
    geda_arc_object_modify (NULL, 0, 0, 2);
  }
  else {
    fprintf(stderr, "FAILED: (O022100) geda_arc_object_modify NULL\n");
    result++;
  }

  /* === Function 30: geda_arc_object_rotate NULL === */

  if (setjmp(point) == 0) {
    geda_arc_object_rotate (NULL, 0, 0, 180);
  }
  else {
    fprintf(stderr, "FAILED: (O023000) geda_arc_object_rotate NULL\n");
    result++;
  }

  /* === Function 31: geda_arc_object_scale NULL === */

  if (setjmp(point) == 0) {
    geda_arc_object_scale (NULL, 0);
  }
  else {
    fprintf(stderr, "FAILED: (O023100) geda_arc_object_scale NULL\n");
    result++;
  }

  /* === Function 50: geda_arc_object_translate NULL === */

  if (setjmp(point) == 0) {
    geda_arc_object_translate (NULL, 0, 0);
  }
  else {
    fprintf(stderr, "FAILED: (O025000) geda_arc_object_translate NULL\n");
    result++;
  }

  /* Now check the same function with actual arguments */

    /* _new(int color, int x, int y, int radius, int start_angle, int arc_sweep) */
  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  int count;

  for (count = 0; count < 10; count++) {

#if USE_RANDOM_NUMBERS
    int x = geda_random_number (0, 120000);
    int y = geda_random_number (0, 80000);
    int r = geda_random_number (5, 5000);
#else
    int x = 1050; //1000;
    int y = 6000; //1000;
    int r = 3233; //100;
#endif

    geda_arc_object_set_radius (object, r);
    geda_arc_object_set_center_x (object, x);
    geda_arc_object_set_center_y (object, y);

    geda_arc_object_set_start_angle(object, 0);

    int dx, dy;

    /* === Function 09: geda_arc_object_mirror === */

    geda_arc_object_mirror (object, x + r, y);

    geda_arc_object_get_position(object, &dx, &dy);
    if ((dx - x - 2 * r) || (dy - y)) {
      fprintf(stderr, "FAILED: (O022001) (%d,%d) != (%d,%d)\n", x, y, dx, dy);
      result++;
    }

    int ang = geda_arc_object_get_start_angle(object);
    if (ang != 180) {
      fprintf(stderr, "FAILED: (O022002) %d != 180\n", ang);
      result++;
    }

    /* === Function 21: geda_arc_object_modify === */

    geda_arc_object_modify(object, x, y, ARC_CENTER);

    geda_arc_object_get_position(object, &dx, &dy);
    if ((dx - x) || (dy - y)) {
      fprintf(stderr, "FAILED: (O022101) ARC_CENTER (%d,%d) != (%d,%d)\n", x, y, dx, dy);
      result++;
    }

    geda_arc_object_modify(object, r + 100, 100, ARC_RADIUS);

    int rad = geda_arc_object_get_radius(object);
    if (rad - r - 100) {
      fprintf(stderr, "FAILED: (O022102) ARC_RADIUS <%d>\n", rad);
      result++;
    }

    geda_arc_object_modify(object, 90, 100, ARC_START_ANGLE);

    ang = geda_arc_object_get_start_angle(object);
    if (ang != 90) {
      fprintf(stderr, "FAILED: (O022103) ARC_START_ANGLE %d != 90\n", ang);
      result++;
    }

    geda_arc_object_modify(object, 180, 100, ARC_END_ANGLE);

    ang = geda_arc_object_get_arc_sweep(object);
    if (ang != 180) {
      fprintf(stderr, "FAILED: (O022104) ARC_END_ANGLE %d != 180\n", ang);
      result++;
    }

    /* === Function 30: geda_arc_object_rotate === */

    geda_arc_object_rotate(object, x, y, 270);

    ang = geda_arc_object_get_start_angle(object);
    if (ang != 0) {
      fprintf(stderr, "FAILED: (O023001) start angle %d != 0\n", ang);
      result++;
    }

    /* === Function 31: geda_arc_object_scale NULL === */

    if (setjmp(point) == 0) {
      geda_arc_object_scale (object, 10);
    }
    else {
      int rad = geda_arc_object_get_radius (object);
      int sb  = r * 10;
      fprintf(stderr, "FAILED: (O023101) geda_arc_object_scale %d != %d\n", rad, sb);
      result++;
    }

    /* Reset the radius back to the generated value, not really needed */
    geda_arc_object_set_radius (object, r);

    /* === Function 50: geda_arc_object_translate === */

    int tx = -x + 100;
    int ty = -y + 100;

    geda_arc_object_translate(object, tx, ty);

    geda_arc_object_get_position(object, &dx, &dy);

    if ((dx - 100) || (dy - 100)) {
      fprintf(stderr, "FAILED: (O025001) (%d,%d) != (%d,%d)\n", tx, ty, dx, dy);
      result++;
    }

#if !USE_RANDOM_NUMBERS
    break;
#endif

  }

  g_object_unref (object);

  return result;
}

/** @} endgroup test-object-geda-arc */

int
main (int argc, char *argv[])
{
  int result = 0;
  int subtotal = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (setjmp(point) == 0) {
    result = check_construction();
  }
  else {
    fprintf(stderr, "Caught signal in constructors in %s\n\n", MUT);
    return 1;
  }

  if (result) {
    fprintf(stderr, "Check constructors in %s\n\n", MUT);
  }
  else {

    if (setjmp(point) == 0) {
      result = check_accessors();
    }
    else {
      fprintf(stderr, "Caught signal in accessors in %s\n\n", MUT);
      return 1;
    }
    if (result) {
      fprintf(stderr, "Check accessors in %s\n\n", MUT);
    }
  }

  if (!result) {

    if (setjmp(point) == 0) {
      subtotal = check_serialization();
    }
    else {
      fprintf(stderr, "Caught serialization in %s\n\n", MUT);
      return 1;
    }
    if (subtotal) {
      fprintf(stderr, "Check serialization in %s\n\n", MUT);
      result = result + subtotal;
    }

    if (setjmp(point) == 0) {
      subtotal = check_query();
    }
    else {
      fprintf(stderr, "Caught signal in query in %s\n\n", MUT);
      return 1;
    }
    if (subtotal) {
      fprintf(stderr, "Check query functions in %s\n\n", MUT);
      result = result + subtotal;
    }

    if (!result) {
      if (setjmp(point) == 0) {
        subtotal = check_transformer();
      }
      else {
        fprintf(stderr, "Caught signal in transformers in %s\n\n", MUT);
        return 1;
      }
      if (subtotal) {
        fprintf(stderr, "Check transformers in %s\n\n", MUT);
        result = result + subtotal;
      }
    }
    else {
      fprintf(stderr, "skipping transformation checks %s\n\n", MUT);
    }
  }
  else {
    fprintf(stderr, "discontinuing checks %s\n\n", MUT);
  }

  return result;
}
