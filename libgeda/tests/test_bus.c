#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaBus"

/*! \file test_bus.c
 *  \brief Tests for geda_bus.c module
 */

int test_bus (void)
{
  int result = 0;

  GedaObject *object = geda_bus_new();

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

  if (!GEDA_IS_BUS(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  /* GedaBus objects are derived from GedaLine object class */
  if (!GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s is a GedaLine Failed\n", TOBJECT);
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

  GedaBus  *bus  = object->bus;
  GedaLine *line = object->line;

  if (!GEDA_IS_BUS(bus)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (!GEDA_IS_LINE(line)) {
    fprintf(stderr, "%s sub-pointer is a GedaLine\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_BUS) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_BUS);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_BUS(object)) {
    fprintf(stderr, "%s was not destroyed\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s parent was not destroyed\n", TOBJECT);
    result++;
  }

  if (GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "%s grand-parent was not destroyed\n", TOBJECT);
    result++;
  }

  return result;
}

int
test_bus_properties (void)
{
  int result = 0;

  GedaObject *object = geda_bus_new();

  if (!GEDA_IS_BUS(object->bus)) {
    fprintf(stderr, "is a %s Failed line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaBus *bus = object->bus;

    int count;
    int fail;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x1 = m_random_number ( 0,       119800);
      int y2 = m_random_number ( 0,        79800);
      int x2 = m_random_number (x1 + 100, 120000);
      int y1 = m_random_number (y2 + 100,  80000);
      int d  = m_random_number (0, 1);

      g_object_set(bus, "bus-ripper-direction", d,
                        "first-x",  x1,
                        "first-y",  y1,
                        "second-x", x2,
                        "second-y", y2,
                        NULL);

      int rd, rx1, ry1, rx2, ry2;

      g_object_get(bus, "bus-ripper-direction", &rd,
                        "first-x",  &rx1,
                        "first-y",  &ry1,
                        "second-x", &rx2,
                        "second-y", &ry2,
                        NULL);

      if (d - rd) {
        fprintf(stderr, "FAILED: %s get/set bus-ripper-direction property <%d>\n", TOBJECT, rd);
        fail++;
      }

      if (x1 - rx1) {
        fprintf(stderr, "FAILED: %s get/set first x property <%d>\n", TOBJECT, rx1);
        fail++;
      }

      if (y1 - ry1) {
        fprintf(stderr, "FAILED: %s get/set first y property <%d>\n", TOBJECT, ry1);
        fail++;
      }

      if (x2 - rx2) {
        fprintf(stderr, "FAILED: %s get/set second x property <%d>\n", TOBJECT, rx2);
        fail++;
      }

      if (y2 - ry2) {
        fprintf(stderr, "FAILED: %s get/set second y property <%d>\n", TOBJECT, ry2);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\tdirection: %d\n", d);
        fprintf(stderr, "\tfirst-x: %d\n",  x1);
        fprintf(stderr, "\tfirst-y: %d\n",  y1);
        fprintf(stderr, "\tsecond-x: %d\n", x2);
        fprintf(stderr, "\tsecond-y: %d\n", y2);

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

  result  = test_bus();
  result += test_bus_properties();

  if (result) {
    fprintf(stderr, "Check module geda_bus.c");
  }

  return result;
}
