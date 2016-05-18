#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaLine"

/*! \file test_line.c
 *  \brief Tests for geda_line.c module
 */

int test_line (void)
{
  int result = 0;

  GedaObject *object = geda_line_new();

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

  if (!GEDA_IS_LINE(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  GedaLine *line = object->line;

  if (!GEDA_IS_LINE(line)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_LINE) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_LINE);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_LINE(object)) {
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

  GedaObject *object = geda_line_new();

  if (!GEDA_IS_LINE(object->line)) {
    fprintf(stderr, "is a %s Failed at line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaLine *line = object->line;

    int count;
    int fail;
    int value;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x0 = m_random_number (0,  115000);
      int y0 = m_random_number (0,  75000);
      int x1 = m_random_number (x0, 120000);
      int y1 = m_random_number (y0, 80000);

      /* Line type options */
      int e = m_random_number (END_NONE, END_ROUND);
      int t = m_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int w = m_random_number (0, 500);
      int p = m_random_number (0, 500);
      int l = m_random_number (0, 500);

      g_object_set(line, "first-x",    x0,
                         "first-y",    y0,
                         "second-x",   x1,
                         "second-y",   y1,
                          NULL);

      int rx0, ry0, rx1, ry1;

      g_object_get(line, "first-x",    &rx0,
                         "first-y",    &ry0,
                         "second-x",   &rx1,
                         "second-y",   &ry1,
                          NULL);

      value = line->x[0];
      if (value - x0) {
        fprintf(stderr, "FAILED: %s set first x property %d != %d\n", TOBJECT, value, x0);
        fail++;
      }
      else if (rx0 - x0) {
        fprintf(stderr, "FAILED: %s get first x property %d != %d\n", TOBJECT, rx0, x0);
        fail++;
      }

      value = line->y[0];
      if (value - y0) {
        fprintf(stderr, "FAILED: %s set first y property %d != %d\n", TOBJECT, value, y0);
        fail++;
      }
      else if (ry0 - y0) {
        fprintf(stderr, "FAILED: %s get first y property %d != %d\n", TOBJECT, rx0, y0);
        fail++;
      }

      value = line->x[1];
      if (value - x1) {
        fprintf(stderr, "FAILED: %s set second x property %d != %d\n", TOBJECT, value, x1);
        fail++;
      }
      else if (rx1 - x1) {
        fprintf(stderr, "FAILED: %s get second x property %d != %d\n", TOBJECT, rx0, x1);
        fail++;
      }

      value = line->y[1];
      if (value - y1) {
        fprintf(stderr, "FAILED: %s set second y property %d != %d\n", TOBJECT, value, y1);
        fail++;
      }
      else if (ry1 - y1) {
        fprintf(stderr, "FAILED: %s get second y property %d != %d\n", TOBJECT, rx1, y1);
        fail++;
      }

      /* Check line type properties */

      g_object_set(line, "end-cap",     e,
                         "line-type",   t,
                         "line-width",  w,
                         "line-space",  p,
                         "line-length", l,
                          NULL);

      int re, rt, rw, rp, rl;

      g_object_get(line, "end-cap",     &re,
                         "line-type",   &rt,
                         "line-width",  &rw,
                         "line-space",  &rp,
                         "line-length", &rl,
                          NULL);

      value = line->line_options.line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: %s set end-cap property %d != %d\n", TOBJECT, value, e);
        fail++;
      }
      else if (re - e) {
        fprintf(stderr, "FAILED: %s get end-cap property %d != %d\n", TOBJECT, re, e);
        fail++;
      }

      value = line->line_options.line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: %s set line-type property %d != %d\n", TOBJECT, value, t);
        fail++;
      }
      else if (rt - t) {
        fprintf(stderr, "FAILED: %s get line-type property %d != %d\n", TOBJECT, rt, t);
        fail++;
      }

      value = line->line_options.line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: %s set line-width property %d != %d\n", TOBJECT, value, w);
        fail++;
      }
      else if (rw - w) {
        fprintf(stderr, "FAILED: %s get line-width property %d != %d\n", TOBJECT, rw, w);
        fail++;
      }

      value = line->line_options.line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: %s set line-space property %d != %d\n", TOBJECT, value, p);
        fail++;
      }
      else if (rp - p) {
        fprintf(stderr, "FAILED: %s get line-space property %d != %d\n", TOBJECT, rp, p);
        fail++;
      }

      value = line->line_options.line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: %s set line-length property %d != %d\n", TOBJECT, value, l);
        fail++;
      }
      else if (rl - l) {
        fprintf(stderr, "FAILED: %s get line-length property %d != %d\n", TOBJECT, rl, l);
        fail++;
      }

      if (fail) {

        fprintf(stderr, "FAILED: to get or set %d %s propert%s\n", fail, TOBJECT,
                fail > 1 ? "ies" : "y");
        fprintf(stderr, "Conditions:\n");
        fprintf(stderr, "\t      x0: %d\n", x0);
        fprintf(stderr, "\t      y0: %d\n", y0);
        fprintf(stderr, "\t      x1: %d\n", x1);
        fprintf(stderr, "\t      y1: %d\n", y1);

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

  result = test_line();

  result += check_properties();

  return result;
}
