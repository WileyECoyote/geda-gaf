#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaNet"

/*! \file tests_net.c
 *  \brief Tests for geda_net.c module
 */

int tests_net (void)
{
  int result = 0;

  GedaObject *object = geda_net_new();

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
    fprintf(stderr, "%s: matched type GedaBus\n", TOBJECT);
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

  if (!GEDA_IS_NET(object)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
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

  GedaNet  *net  = object->net;
  GedaLine *line = object->line;

  if (!GEDA_IS_NET(net)) {
    fprintf(stderr, "sub-pointer is a %s Failed\n", TOBJECT);
    result++;
  }
  else if (!GEDA_IS_LINE(line)) {
    fprintf(stderr, "%s sub-pointer is a GedaLine\n", TOBJECT);
    result++;
  }
  else if (object->type != OBJ_NET) {
    fprintf(stderr, "%s type not %c\n", TOBJECT, OBJ_NET);
    result++;
  }

  g_object_unref(object);

  if (GEDA_IS_NET(object)) {
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
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result = tests_net();

  return result;
}
