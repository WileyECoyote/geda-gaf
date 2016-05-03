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

      /* Line type options */
      int e = m_random_number (END_NONE, END_ROUND);
      int t = m_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int w = m_random_number (0, 500);
      int p = m_random_number (0, 500);
      int l = m_random_number (0, 500);

      /* Filling options */
      int ft  = m_random_number (FILLING_HOLLOW, FILLING_HATCH);
      int fw  = m_random_number (0, 100);
      int fa1 = m_random_number (0, 180);
      int fp1 = m_random_number (0, 500);
      int fa2 = m_random_number (0, 180);
      int fp2 = m_random_number (0, 500);

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

      /* Check line type properties */
      g_object_set(box, "end-cap",     e,
                        "line-type",   t,
                        "line-width",  w,
                        "line-space",  p,
                        "line-length", l,
                        NULL);

      int re, rt, rw, rp, rl;

      g_object_get(box, "end-cap",     &re,
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
      g_object_set(box, "fill-type",   ft,
                        "fill-width",  fw,
                        "fill-angle1", fa1,
                        "fill-pitch1", fp1,
                        "fill-angle2", fa2,
                        "fill-pitch2", fp2,
                        NULL);

      int rft, rfw, rfa1, rfp1, rfa2, rfp2;

      g_object_get(box, "fill-type",   &rft,
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
test_box_accessors (void)
{
  int result = 0;

  GedaObject *object = geda_box_new();

  if (!GEDA_IS_BOX(object->box)) {
    fprintf(stderr, "is a %s Failed at line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    GedaBox *box = object->box;

    int count;
    int fail;
    int value;

    fail = 0;

    for (count = 0; count < 10; count++) {

      int x1  = m_random_number ( 0,       119800);
      int y2  = m_random_number ( 0,        79800);
      int x2  = m_random_number (x1 + 100, 120000);
      int y1  = m_random_number (y2 + 100,  80000);

      /* Line type options */
      int e = m_random_number (END_NONE, END_ROUND);
      int t = m_random_number (TYPE_SOLID, TYPE_PHANTOM);
      int l = m_random_number (0, 500);
      int p = m_random_number (0, 500);
      int w = m_random_number (0, 500);

      /* Filling options */
      //int ft  = m_random_number (FILLING_HOLLOW, FILLING_HATCH);
      //int fw  = m_random_number (0, 100);
      //int fa1 = m_random_number (0, 180);
      //int fp1 = m_random_number (0, 500);
      //int fa2 = m_random_number (0, 180);
      //int fp2 = m_random_number (0, 500);

      geda_box_set_lower_x(box, x1);

      value = box->lower_x;
      if (value - x1) {
        fprintf(stderr, "FAILED: geda_box_set_lower_x %d != %d\n", value, x1);
        fail++;
      }

      value = geda_box_get_lower_x(box);
      if (value - x1) {
        fprintf(stderr, "FAILED: geda_box_get_lower_x %d != %d\n", value, x1);
        fail++;
      }

      geda_box_set_lower_y(box, y1);

      value = box->lower_y;
      if (value - y1) {
        fprintf(stderr, "FAILED: geda_box_set_lower_y %d != %d\n", value, x1);
        fail++;
      }

      value = geda_box_get_lower_y(box);
      if (value - y1) {
        fprintf(stderr, "FAILED: geda_box_get_lower_y %d != %d\n", value, x1);
        fail++;
      }


      geda_box_set_upper_x(box, x2);

      value = box->upper_x;
      if (value - x2) {
        fprintf(stderr, "FAILED: geda_box_set_upper_x %d != %d\n", value, x2);
        fail++;
      }

      value = geda_box_get_upper_x(box);
      if (value - x2) {
        fprintf(stderr, "FAILED: geda_box_get_upper_x %d != %d\n", value, x2);
        fail++;
      }

      geda_box_set_upper_y(box, y2);

      value = box->upper_y;
      if (value - y2) {
        fprintf(stderr, "FAILED: geda_box_set_upper_y %d != %d\n", value, y2);
        fail++;
      }

      value = geda_box_get_upper_y(box);
      if (value - y2) {
        fprintf(stderr, "FAILED: geda_box_get_upper_y %d != %d\n", value, y2);
        fail++;
      }

      /* Check line type properties */

      geda_box_set_end_cap (box, e);

      value = box->line_options.line_end;
      if (value - e) {
        fprintf(stderr, "FAILED: geda_box_set_end_cap %d != %d\n", value, e);
        fail++;
      }

      value = geda_box_get_end_cap(box);

      if (value - e) {
        fprintf(stderr, "FAILED: geda_box_get_end_cap %d != %d\n", value, e);
        fail++;
      }

      geda_box_set_line_type (box, t);

      value = box->line_options.line_type;
      if (value - t) {
        fprintf(stderr, "FAILED: geda_box_set_line_type %d != %d\n", value, t);
        fail++;
      }

      value = geda_box_get_line_type(box);

      if (value - t) {
        fprintf(stderr, "FAILED: geda_box_get_line_type %d != %d\n", value, t);
        fail++;
      }

      geda_box_set_line_length (box, l);

      value = box->line_options.line_length;
      if (value - l) {
        fprintf(stderr, "FAILED: geda_box_set_line_length %d != %d\n", value, l);
        fail++;
      }

      value = geda_box_get_line_length(box);

      if (value - l) {
        fprintf(stderr, "FAILED: geda_box_get_line_length %d != %d\n", value, l);
        fail++;
      }

      geda_box_set_line_space (box, p);

      value = box->line_options.line_space;
      if (value - p) {
        fprintf(stderr, "FAILED: geda_box_set_line_space %d != %d\n", value, p);
        fail++;
      }

      value = geda_box_get_line_space(box);

      if (value - p) {
        fprintf(stderr, "FAILED: geda_box_get_line_space %d != %d\n", value, p);
        fail++;
      }

      geda_box_set_line_width (box, w);

      value = box->line_options.line_width;
      if (value - w) {
        fprintf(stderr, "FAILED: geda_box_set_line_width %d != %d\n", value, w);
        fail++;
      }

      value = geda_box_get_line_width(box);

      if (value - w) {
        fprintf(stderr, "FAILED: geda_box_get_line_width %d != %d\n", value, w);
        fail++;
      }

      /* Check Filling properties */

      // ft, fw, fa1, fp1, fa2, fp2;

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
  result += test_box_accessors();

  if (result) {
    fprintf(stderr, "Check module geda_box.c");
  }

  return result;
}
