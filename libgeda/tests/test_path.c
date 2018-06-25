/* -*- test_path.c -*-
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
 *  Date Contributed: February, 5th, 2016
 */

#include "../../config.h"

#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaPath"

/*! \file test_path.c
 *  \brief Tests for geda_path.c module
 */

int test_path (void)
{
  int result = 0;

  GedaObject *object = geda_path_new();

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s: is a GedaObject Failed\n", TOBJECT);
    result++;
  }

  /* The one with the Not operator is the one being tested */

  if (GEDA_IS_ARC(object)) {
    fprintf(stderr, "%s matched type GedaArc\n", TOBJECT);

    result++;
  }

  if (GEDA_IS_BOX(object)) {
    fprintf(stderr, "%s matched type GedaBox\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_BUS(object)) {
    fprintf(stderr, "%s matched type GedaBus\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_CIRCLE(object)) {
    fprintf(stderr, "%s matched type GedaCircle\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_COMPLEX(object)) {
    fprintf(stderr, "%s matched type GedaComplex\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s matched type GedaLine\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_NET(object)) {
    fprintf(stderr, "%s: matched type GedaNet\n", TOBJECT);
    result++;
  }

  if (!GEDA_IS_PATH(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
    result++;
  }

  if (GEDA_IS_PICTURE(object)) {
    fprintf(stderr, "%s matched type GedaPicture\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_PIN(object)) {
    fprintf(stderr, "%s matched type GedaPin\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_TEXT(object)) {
    fprintf(stderr, "%s matched type GedaText\n", TOBJECT);
    result++;
  }

  GedaPath *path = object->path;

  if (!GEDA_IS_PATH(path)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_PATH) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_PATH);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_PATH(object)) {
    fprintf(stderr, "%s was not destroyed\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s parent was not destroyed\n", TOBJECT);
    result++;
  }

  return result;
}

int
check_properties (void)
{
  int result = 0;

  GedaObject *object = geda_path_new();

  if (!GEDA_IS_PATH(object->path)) {
    fprintf(stderr, "is a %s Failed line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaPath *path = object->path;

    int count;
    int fail;

    fail = 0;

    for (count = 0; count < 10; count++) {

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

      /* Check line type properties */
      g_object_set(path, "end-cap",     e,
                         "line-type",   t,
                         "line-width",  w,
                         "line-space",  p,
                         "line-length", l,
                          NULL);

      int re, rt, rw, rp, rl;

      g_object_get(path, "end-cap",     &re,
                         "line-type",   &rt,
                         "line-width",  &rw,
                         "line-space",  &rp,
                         "line-length", &rl,
                          NULL);

      if (e - re) {
        fprintf(stderr, "FAILED: %s get/set end-cap property <%d>\n", TOBJECT, re);
        fail++;
      }

      if (t - rt) {
        fprintf(stderr, "FAILED: %s get/set line-type property <%d>\n", TOBJECT, rt);
        fail++;
      }

      if (w - rw) {
        fprintf(stderr, "FAILED: %s get/set line-width property <%d>\n", TOBJECT, rw);
        fail++;
      }

      if (p - rp) {
        fprintf(stderr, "FAILED: %s get/set line-space property <%d>\n", TOBJECT, rp);
        fail++;
      }

      if (l - rl) {
        fprintf(stderr, "FAILED: %s get/set line-length property <%d>\n", TOBJECT, rl);
        fail++;
      }

      /* Check Filling properties */
      g_object_set(path, "fill-type",   ft,
                         "fill-width",  fw,
                         "fill-angle1", fa1,
                         "fill-pitch1", fp1,
                         "fill-angle2", fa2,
                         "fill-pitch2", fp2,
                          NULL);

      int rft, rfw, rfa1, rfp1, rfa2, rfp2;

      g_object_get(path, "fill-type",   &rft,
                         "fill-width",  &rfw,
                         "fill-angle1", &rfa1,
                         "fill-pitch1", &rfp1,
                         "fill-angle2", &rfa2,
                         "fill-pitch2", &rfp2,
                          NULL);

      if (ft - rft) {
        fprintf(stderr, "FAILED: %s get/set fill-type property <%d>\n", TOBJECT, rft);
        fail++;
      }

      if (fw - rfw) {
        fprintf(stderr, "FAILED: %s get/set fill-width property <%d>\n", TOBJECT, rfw);
        fail++;
      }

      if (fa1 - rfa1) {
        fprintf(stderr, "FAILED: %s get/set fill-angle1 property <%d>\n", TOBJECT, rfa1);
        fail++;
      }

      if (fp1 - rfp1) {
        fprintf(stderr, "FAILED: %s get/set fill-pitch1 property <%d>\n", TOBJECT, fp1);
        fail++;
      }

      if (fa2 - rfa2) {
        fprintf(stderr, "FAILED: %s get/set fill-angle2 property <%d>\n", TOBJECT, rfa2);
        fail++;
      }

      if (fp2 - rfp2) {
        fprintf(stderr, "FAILED: %s get/set fill-pitch1 property <%d>\n", TOBJECT, fp2);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");

        result = fail;
        break;
      }
    }
  }

  return result;
}

int
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result = test_path();
  result += check_properties();

  return result;
}
