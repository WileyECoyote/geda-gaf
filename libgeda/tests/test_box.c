#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaBox"

/*! \file test_box.c
 *  \brief Tests for geda_box.c module
 */

int test_box (void)
{
  int result = 0;

  GedaObject *object = geda_box_new();

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s: is a GedaObject Failed\n", TOBJECT);
    result++;
  }

  /* The one with the Not operator is the one being tested */

  if (GEDA_IS_ARC(object)) {
    fprintf(stderr, "%s matched type GedaArc\n", TOBJECT);

    result++;
  }

  if (!GEDA_IS_BOX(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  GedaBox *box = object->box;

  if (!GEDA_IS_BOX(box)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_BOX) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_BOX);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_BOX(object)) {
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
test_box_properties (void)
{
  int result = 0;

  GedaObject *object = geda_box_new();

  if (!GEDA_IS_BOX(object->box)) {
    fprintf(stderr, "is a %s Failed line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaBox *box = object->box;

    int count;
    int fail;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x1  = m_random_number ( 0,       119800);
      int y2  = m_random_number ( 0,        79800);
      int x2  = m_random_number (x1 + 100, 120000);
      int y1  = m_random_number (y2 + 100,  80000);

      g_object_set(box, "upper-x", x1,
                        "upper-y", y1,
                        "lower-x", x2,
                        "lower-y", y2,
                        NULL);

      int rx1, ry1, rx2, ry2;

      g_object_get(box, "upper-x", &rx1,
                        "upper-y", &ry1,
                        "lower-x", &rx2,
                        "lower-y", &ry2,
                        NULL);

      if (x1 - rx1) {
        fprintf(stderr, "FAILED: %s get/set upper-x property <%d>\n", TOBJECT, rx1);
        fail++;
      }

      if (y1 - ry1) {
        fprintf(stderr, "FAILED: %s get/set upper-y property <%d>\n", TOBJECT, ry1);
        fail++;
      }

      if (x2 - rx2) {
        fprintf(stderr, "FAILED: %s get/set lower-x property <%d>\n", TOBJECT, rx2);
        fail++;
      }

      if (y2 - ry2) {
        fprintf(stderr, "FAILED: %s get/set lower-y property <%d>\n", TOBJECT, ry2);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tupper-x: %d\n", x1);
        fprintf(stderr, "\tupper-y: %d\n", y1);
        fprintf(stderr, "\tlower-x: %d\n", x2);
        fprintf(stderr, "\tlower-y: %d\n", y2);

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

  result  = test_box();
  result += test_box_properties();

  if (result) {
    fprintf(stderr, "Check module geda_box.c");
  }

  return result;
}
