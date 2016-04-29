/* -*- test_arc.c -*-
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
 *  Date Contributed: January, 30th, 2016
 */

#include <libgeda.h>

#define TOBJECT "GedaArc"

/*! \file test_arc.c
 *  \brief Tests for geda_arc.c module
 *  \par
 *  This module provides basic unit tests for construction and destruction
 *  of GedaArc objects, type checking is also tested to insure intergration
 *  with other object types drived from the same base class, i.e. GedaObject.
 */

int test_geda_arc_within_sweep(GedaArc *arc)
{
  int result = 0;

  /* === Function: geda_arc_within_sweep === */

  int x, y;

  geda_arc_set_center_x (arc, 1000);
  geda_arc_set_center_y (arc, 1000);
  geda_arc_set_radius (arc, 100);
  geda_arc_set_start_angle (arc, 0);
  geda_arc_set_arc_sweep (arc, 90);

  if (geda_arc_within_sweep(NULL, 1000, 1000)) {
    fprintf(stderr, "FAILED: geda_arc_within_sweep NULL\n");
    result++;
  }

  /* Note: geda_arc_within_sweep inside for loops ==  TRUE */

  for (x = 1000; x < 1250; x = x + 50) {
    for (y = 1000; y < 1200; y = y + 50) {
      if (!geda_arc_within_sweep(arc, x, y)) {
        fprintf(stderr, "FAILED: SA0AS90F geda_arc_within_sweep (%d,%d)\n", x, y);
        result++;
      }
    }
  }

  /* Below */
  /* x = 1200 */
  y = m_random_number (0, 999);
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA0AS90B geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* Left */
  x = m_random_number (0, 999);
  y = 1000;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA0AS90L geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South West */
  y = m_random_number (0, 999);
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA0AS90SW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South West other side of origin */
  x = -1 * y;
  y = -1 * x;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA0AS90SW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  geda_arc_set_start_angle (arc, 90);

  for (x = 1000; x > 750; x = x - 50) {
    for (y = 1000; y < 1200; y = y + 50) {
      if (!geda_arc_within_sweep(arc, x, y)) {
        fprintf(stderr, "FAILED: SA90AS90F geda_arc_within_sweep (%d,%d)\n", x, y);
        result++;
      }
    }
  }

 /* Right */
  x = m_random_number (1001, 5000);
  y = 1000;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA90AS90R geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* Below */
  x = 1000;
  y = m_random_number (0, 999);
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA90AS90B geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South East */
  x = m_random_number (0, 999);
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA90AS90SE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  geda_arc_set_start_angle (arc, 180);

  for (x = 1000; x > 750; x = x - 50) {
    for (y = 1000; y > 750; y = y - 50) {
      if (!geda_arc_within_sweep(arc, x, y)) {
        fprintf(stderr, "FAILED: SA180AS90F geda_arc_within_sweep (%d,%d)\n", x, y);
        result++;
      }
    }
  }

  /* Right */
  x = m_random_number (1001, 5000);
  y = 1000;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA180AS90R geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* Above */
  x = 1000;
  y = m_random_number (1001, 5000);
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA180AS90A geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* North West */
  x =  m_random_number (1001, 5000);
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA180AS90NW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  geda_arc_set_start_angle (arc, 0);
  geda_arc_set_arc_sweep (arc, 180);

  int e = m_random_number (1000, 10000);
  int w = -1 * e;
  int n = m_random_number (1000, 10000);
  int s = -1 * n;

  /* North East ++ */
  x = n ; y = e;
  if (!geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA0AS180NE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* North West -+ */
  x = w;
  if (!geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA0AS180NW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South West -- */
  y = s;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA0AS180SW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South East +- */
  x = e;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA0AS180SE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  geda_arc_set_start_angle (arc, 90);

  /* South East +- */
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA90AS180SE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* North East ++ */
  x = e;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA90AS180NE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* North West -+ */
  x = w;
  if (!geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA90AS180NW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South West -- */
  y = s;
  if (!geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA90AS180SW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  geda_arc_set_start_angle (arc, 180);

  e = m_random_number (1000, 10000);
  w = -1 * e;
  n = m_random_number (1000, 10000);
  s = -1 * n;

  /* South West -- */
  x = w;
  y = s;
  if (!geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA180AS180SW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South East +- */
  x = e;
  if (!geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA180AS180SE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* North East ++ */
  y = n;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA180AS180NE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* North West -+ */
  x = w;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA180AS180NW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  geda_arc_set_start_angle (arc, 270);
  /* North West -+ */
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA270AS180NW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South West -- */
  y = s;
  if (geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA270AS180SW geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* South East +- */
  x = e;
  if (!geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA270AS180SE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  /* North East ++ */
  y = n;
  if (!geda_arc_within_sweep(arc, x, y)) {
    fprintf(stderr, "FAILED: SA270AS180NE geda_arc_within_sweep (%d,%d)\n", x, y);
    result++;
  }

  return result;
}

int test_arc (void)
{
  int result = 0;

  GedaObject *object = geda_arc_new();

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s: is a GedaObject Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* The one with the Not operator is the one being tested */

  if (!GEDA_IS_ARC(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  if (GEDA_IS_PATH(object)) {
    fprintf(stderr, "%s matched type GedaPath\n", TOBJECT);
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

  GedaArc *arc = object->arc;

  if (!GEDA_IS_ARC(arc)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_ARC) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_ARC);
    result++;
  }
  else {

    int count;
    int fail;

    fail = 0;

    for (count = 0; count < 100; count++) {

      int a = m_random_number (0, 359);
      int r = m_random_number (5, 20000);
      int s = m_random_number (1, 359);
      int x = m_random_number (0, 120000);
      int y = m_random_number (0, 80000);

      int dx = m_random_number (0, 1000);
      int dy = m_random_number (0, 1000);

      int cx, cy, value;

      geda_arc_set_arc_sweep (arc, s);
      geda_arc_set_center_x (arc, x);
      geda_arc_set_center_y (arc, y);
      geda_arc_set_radius (arc, r);
      geda_arc_set_start_angle (arc, a);

      geda_arc_get_position (arc, &cx, &cy);
      geda_arc_set_position (arc, cx - dx, cy - dy);

      value = geda_arc_get_start_angle(arc);
      if (value - a) {
        fprintf(stderr, "FAILED: %s get/set start angle <%d>\n", TOBJECT, value);
        fail++;
      }

      value = geda_arc_get_center_x(arc);
      if (value - x + dx) {
        fprintf(stderr, "FAILED: %s get/set center x <%d>\n", TOBJECT, value);
        fail++;
      }

      value = geda_arc_get_center_y(arc);
      if (value - y + dy) {
        fprintf(stderr, "FAILED: %s get/set center y <%d>\n", TOBJECT, value);
        fail++;
      }

      value = geda_arc_get_radius(arc);
      if (value - r) {
        fprintf(stderr, "FAILED: %s get/set radius <%d>\n", TOBJECT, value);
        fail++;
      }

      value = geda_arc_get_arc_sweep(arc);
      if ( s - value) {
        fprintf(stderr, "FAILED: %s get/set arc sweep <%d>\n", TOBJECT, value);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tstart angle: %d\n", a);
        fprintf(stderr, "\t     radius: %d\n", r);
        fprintf(stderr, "\t  arc sweep: %d\n", s);
        fprintf(stderr, "\t   center x: %d\n", x);
        fprintf(stderr, "\t   center y: %d\n", y);
        fprintf(stderr, "\t    offsets: dx=%d, dy=%d\n", dx, dy);

        result = result + fail;
        break;
      }
    }
  }

  /* If can construct, get and set properties then */
  if (!result) {
    result = test_geda_arc_within_sweep(arc);
  }

  g_object_unref(object);

  if (GEDA_IS_ARC(object)) {
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
test_arc_properties (void)
{
  int result = 0;

  GedaObject *object = geda_arc_new();

  if (!GEDA_IS_ARC(object->arc)) {
    fprintf(stderr, "is a %s Failed line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaArc *arc = object->arc;

    int count;
    int fail;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int a = m_random_number (0, 359);
      int r = m_random_number (5, 20000);
      int s = m_random_number (1, 359);
      int x = m_random_number (0, 120000);
      int y = m_random_number (0, 80000);
      int e = m_random_number (END_NONE, END_ROUND);
      int t = m_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int w = m_random_number (0, 500);
      int p = m_random_number (0, 500);
      int l = m_random_number (0, 500);

      g_object_set(arc, "center-x",    x,
                        "center-y",    y,
                        "radius",      r,
                        "start-angle", a,
                        "arc-sweep",   s,
                        NULL);

      int ra, rr, rs, rx, ry;

      g_object_get(arc, "center-x",    &rx,
                        "center-y",    &ry,
                        "radius",      &rr,
                        "start-angle", &ra,
                        "arc-sweep",   &rs,
                        NULL);

      if (a - ra) {
        fprintf(stderr, "FAILED: %s get/set start angle property <%d>\n", TOBJECT, ra);
        fail++;
      }

      if (x - rx) {
        fprintf(stderr, "FAILED: %s get/set center x property <%d>\n", TOBJECT, rx);
        fail++;
      }

      if (y - ry) {
        fprintf(stderr, "FAILED: %s get/set center y property <%d>\n", TOBJECT, ry);
        fail++;
      }

      if (r - rr) {
        fprintf(stderr, "FAILED: %s get/set radius property <%d>\n", TOBJECT, rr);
        fail++;
      }

      if ( s - rs) {
        fprintf(stderr, "FAILED: %s get/set arc sweep property <%d>\n", TOBJECT, rs);
        fail++;
      }

      /* line type properties */
      g_object_set(arc, "end-cap",     e,
                        "line-type",   t,
                        "line-width",  w,
                        "line-space",  p,
                        "line-length", l,
                        NULL);

      int re, rt, rw, rp, rl;

      g_object_get(arc, "end-cap",     &re,
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

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
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

int
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = test_arc();

  result += test_arc_properties();

  if (result) {
    fprintf(stderr, "Check module geda_arc.c");
  }

  return (result > 0);
}
