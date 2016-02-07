#include <libgeda.h>
#include <version.h>

void
check_construction ()
{
  int count;
  GedaToplevel *toplevel = s_toplevel_new ();

  for (count = 0; count < 1000; count++) {

    int center_x    = g_test_rand_int ();
    int center_y    = g_test_rand_int ();
    int color       = g_test_rand_int_range (0, MAX_COLORS - 1);
    int radius      = g_test_rand_int_range (0, G_MAXINT);
    int start_angle = g_test_rand_int_range (0, 359);
    int sweep_angle = g_test_rand_int_range (0, 360);

    GedaObject *object0 = geda_arc_object_new (toplevel,
                                               color,
                                               center_x,
                                               center_y,
                                               radius,
                                               start_angle,
                                               sweep_angle);

    g_assert (object0 != NULL);
    g_assert_cmpint (OBJ_ARC, ==, object0->type);

    g_assert_cmpint (center_x,    ==, geda_arc_object_get_center_x (object0));
    g_assert_cmpint (center_y,    ==, geda_arc_object_get_center_y (object0));
    g_assert_cmpint (color,       ==, geda_object_get_color (object0));
    g_assert_cmpint (radius,      ==, geda_arc_object_get_radius (object0));
    g_assert_cmpint (start_angle, ==, geda_arc_object_get_start_angle (object0));
    g_assert_cmpint (sweep_angle, ==, geda_arc_object_get_sweep_angle (object0));

    GedaObject *object1 = geda_arc_object_copy (toplevel, object0);

    g_assert (object1 != NULL);
    g_assert (object1 != object0);
    g_assert_cmpint (OBJ_ARC, ==, object1->type);

    s_delete_object (toplevel, object0);

    g_assert_cmpint (center_x,    ==, geda_arc_object_get_center_x (object1));
    g_assert_cmpint (center_y,    ==, geda_arc_object_get_center_y (object1));
    g_assert_cmpint (color,       ==, geda_object_get_color (object1));
    g_assert_cmpint (radius,      ==, geda_arc_object_get_radius (object1));
    g_assert_cmpint (start_angle, ==, geda_arc_object_get_start_angle (object1));
    g_assert_cmpint (sweep_angle, ==, geda_arc_object_get_sweep_angle (object1));

    s_delete_object (toplevel, object1);
  }

  s_toplevel_delete (toplevel);
}

void
check_accessors ()
{
  int count;
  GedaToplevel *toplevel = s_toplevel_new ();

  for (count = 0; count < 1000; count++) {

    int center_x    = g_test_rand_int ();
    int center_y    = g_test_rand_int ();
    int color       = g_test_rand_int_range (0, MAX_COLORS - 1);
    int radius      = g_test_rand_int_range (0, G_MAXINT);
    int start_angle = g_test_rand_int_range (0, 359);
    int sweep_angle = g_test_rand_int_range (0, 360);

    GedaObject *object0 = geda_arc_object_new (toplevel,
                                               color,
                                               center_x,
                                               center_y,
                                               radius,
                                               start_angle,
                                               sweep_angle);

    g_assert (object0 != NULL);
    g_assert_cmpint (OBJ_ARC, ==, object0->type);

    center_x    = g_test_rand_int ();
    center_y    = g_test_rand_int ();
    color       = g_test_rand_int_range (0, MAX_COLORS - 1);
    radius      = g_test_rand_int_range (0, G_MAXINT);
    start_angle = g_test_rand_int_range (0, 359);
    sweep_angle = g_test_rand_int_range (0, 360);

    o_set_color (toplevel, object0, color);
    geda_arc_object_set_center_x (object0, center_x);
    geda_arc_object_set_center_y (object0, center_y);

    geda_arc_object_set_radius (object0, radius);
    geda_arc_object_set_start_angle (object0, start_angle);
    geda_arc_object_set_sweep_angle (object0, sweep_angle);

    g_assert_cmpint (center_x,    ==, geda_arc_object_get_center_x (object0));
    g_assert_cmpint (center_y,    ==, geda_arc_object_get_center_y (object0));
    g_assert_cmpint (color,       ==, geda_object_get_color (object0));
    g_assert_cmpint (radius,      ==, geda_arc_object_get_radius (object0));
    g_assert_cmpint (start_angle, ==, geda_arc_object_get_start_angle (object0));
    g_assert_cmpint (sweep_angle, ==, geda_arc_object_get_sweep_angle (object0));

    s_delete_object (toplevel, object0);
  }

  s_toplevel_delete (toplevel);
}

void
check_serialization ()
{
  GedaToplevel *toplevel;
  int           count;
  int           converted;
  unsigned int  version;

  toplevel = s_toplevel_new ();

  converted = sscanf (PACKAGE_DATE_VERSION, "%u", &version);
  g_assert_cmpuint (converted, ==, 1);

  for (count = 0; count < 1000; count++) {

    int center_x    = g_test_rand_int ();
    int center_y    = g_test_rand_int ();
    int color       = g_test_rand_int_range (0, MAX_COLORS - 1);
    int radius      = g_test_rand_int_range (0, G_MAXINT);
    int start_angle = g_test_rand_int_range (0, 359);
    int sweep_angle = g_test_rand_int_range (0, 360);

    GedaObject *object0 = geda_arc_object_new (toplevel,
                                               color,
                                               center_x,
                                               center_y,
                                               radius,
                                               start_angle,
                                               sweep_angle);

    g_assert (object0 != NULL);

    char *buffer0 = geda_arc_object_to_buffer (object0);

    s_delete_object (toplevel, object0);
    g_assert (buffer0 != NULL);

    GedaObject *object1 = o_arc_read (toplevel,
                                      buffer0,
                                      version,
                                      FILEFORMAT_VERSION,
                                      NULL);

    g_assert (object1 != NULL);

    g_assert_cmpint (center_x,    ==, geda_arc_object_get_center_x (object1));
    g_assert_cmpint (center_y,    ==, geda_arc_object_get_center_y (object1));
    g_assert_cmpint (color,       ==, geda_object_get_color (object1));
    g_assert_cmpint (radius,      ==, geda_arc_object_get_radius (object1));
    g_assert_cmpint (start_angle, ==, geda_arc_object_get_start_angle (object1));
    g_assert_cmpint (sweep_angle, ==, geda_arc_object_get_sweep_angle (object1));

    char *buffer1 = geda_arc_object_to_buffer (object1);

    s_delete_object (toplevel, object1);
    g_assert (buffer1 != NULL);

    g_assert_cmpstr (buffer0, ==, buffer1);
    g_free (buffer0);
    g_free (buffer1);
  }

  s_toplevel_delete (toplevel);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/geda/libgeda/arc_object/construction",
                   check_construction);

  g_test_add_func ("/geda/libgeda/arc_object/check_accessors",
                   check_accessors);

  g_test_add_func ("/geda/libgeda/arc_object/serialization",
                   check_serialization);

  return g_test_run ();
}
