#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaArc"

/*! \file test_arc.c
 *  \brief Tests for geda_arc.c module
 *  \par
 *  This module provides basic unit tests for construction and destruction
 *  of GedaArc objects, type checking is also tested to insure intergration
 *  with other object types drived from the same base class, i.e. GedaObject.
 */

int test_arc (void)
{
  int result = 0;

  GedaObject *object = geda_arc_new();

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s: is a GedaObject Failed\n", TOBJECT);
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

      value = geda_arc_get_arc_sweep(arc);
      if ( s - value ) {
        fprintf(stderr, "FAILED: %s get/set arc sweep <%d>\n", TOBJECT, value);
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

      value = geda_arc_get_start_angle(arc);
      if (value - a) {
        fprintf(stderr, "FAILED: %s get/set start angle <%d>\n", TOBJECT, value);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s properties\n", fail, TOBJECT);
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
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result = test_arc();

  return (result > 0);
}
