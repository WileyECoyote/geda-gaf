#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaCircle"

/*! \file test_circle.c
 *  \brief Tests for geda_circle.c module
 */

int check_circle (void)
{
  int result = 0;

  GedaObject *object = geda_circle_new();

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

  if (!GEDA_IS_CIRCLE(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  GedaCircle *circle = object->circle;

  if (!GEDA_IS_CIRCLE(circle)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_CIRCLE) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_CIRCLE);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_CIRCLE(object)) {
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
check_accessors (void)
{
  int result = 0;

  GedaObject *object = geda_circle_new();

  if (!GEDA_IS_CIRCLE(object->circle)) {
    fprintf(stderr, "is a %s Failed at line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaCircle *circle = object->circle;

    int count;
    int fail;
    int value;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x  = m_random_number ( 0,   105000);
      int y  = m_random_number ( 0,    65000);
      int r  = m_random_number ( 100,  15000);

      /* Line type options
      int e = m_random_number (END_NONE, END_ROUND);
      int t = m_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = m_random_number (0, 500);
      int p = m_random_number (0, 500);
      int w = m_random_number (0, 500); */

      /* Filling options
      int ft  = m_random_number (FILLING_HOLLOW, FILLING_HATCH);
      int fw  = m_random_number (0, 100);
      int fa1 = m_random_number (0, 180);
      int fp1 = m_random_number (0, 500);
      int fa2 = m_random_number (0, 180);
      int fp2 = m_random_number (0, 500); */

      geda_circle_set_center_x(circle, x);

      value = circle->center_x;
      if (value - x) {
        fprintf(stderr, "FAILED: geda_circle_set_center_x %d != %d\n", value, x);
        fail++;
      }

      value = geda_circle_get_center_x(circle);
      if (value - x) {
        fprintf(stderr, "FAILED: geda_circle_get_center_x %d != %d\n", value, x);
        fail++;
      }

      geda_circle_set_center_y(circle, y);

      value = circle->center_y;
      if (value - y) {
        fprintf(stderr, "FAILED: geda_circle_set_center_y %d != %d\n", value, y);
        fail++;
      }

      value = geda_circle_get_center_y(circle);
      if (value - y) {
        fprintf(stderr, "FAILED: geda_circle_get_center_y %d != %d\n", value, y);
        fail++;
      }

      geda_circle_set_radius(circle, r);

      value = circle->radius;
      if (value - r) {
        fprintf(stderr, "FAILED: geda_circle_set_radius %d != %d\n", value, r);
        fail++;
      }

      value = geda_circle_get_radius(circle);
      if (value - r) {
        fprintf(stderr, "FAILED: geda_circle_get_radius %d != %d\n", value, r);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tcenter-x: %d\n", x);
        fprintf(stderr, "\tcenter-y: %d\n", y);
        fprintf(stderr, "\t  radius: %d\n", r);

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

  result = check_circle();

  result += check_accessors();

  return result > 0;
}
