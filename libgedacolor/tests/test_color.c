#include <glib.h>
#include <gdk/gdk.h>

#include <geda/geda_types.h>
#include <geda_color.h>

#include <libgedacolor.h>

int check_display_dark_colors (void)
{
  int result = 0;

  GArray *color_map = geda_color_get_display_map();

  if (!color_map) {
    fprintf(stderr, "%s: FAILED: geda_color_get_display_map\n", __func__);
    result++;
  }
  else {

    char *name = geda_color_get_color_name(3, color_map, NULL);

    if (!name) {
      fprintf(stderr, "%s: FAILED: geda_color_get_color_name\n", __func__);
      result++;
    }
    else {
      if (!strcmp(name,"green") == 0) {
        fprintf(stderr, "%s: FAILED: index 3 is not green <%s>\n", __func__, name);
        result++;
      }
      g_free(name);
    }

    int index;

    index = geda_color_key_get_index("freestyle0");      /* 20 */
    if (index != 20) {
      fprintf(stderr, "FAILED: %s freestyle0 <%d>\n", __func__, index);
      result++;
    }

    index = geda_color_key_get_index("mesh-grid-minor"); /* 19 */
    if (index != 19) {
      fprintf(stderr, "FAILED: %s mesh-grid-minor <%d>\n", __func__, index);
      result++;
    }

    index = geda_color_key_get_index("freestyle9");      /* 29 */
    if (index != 29) {
      fprintf(stderr, "FAILED: %s freestyle9 <%d>\n", __func__, index);
      result++;
    }

    index = geda_color_key_get_index("background");      /* 0 */
    if (index != 0) {
      fprintf(stderr, "FAILED: %s background <%d>\n", __func__, index);
      result++;
    }
  }

  return result;
}

int check_display_light_colors (void)
{
  int result = 0;

  GArray *color_map = geda_color_get_display_map();

  if (!color_map) {
    fprintf(stderr, "%s: FAILED: geda_color_get_display_map\n", __func__);
    result++;
  }
  else {

    char *name = geda_color_get_color_name(3, color_map, NULL);

    if (!name) {
      fprintf(stderr, "%s: FAILED: geda_color_get_color_name <%s>\n", __func__, name);
      result++;
    }
    else {
      if (!strcmp(name,"green4") == 0) {
        fprintf(stderr, "%s: FAILED: index 3 is not green <%s>\n", __func__, name);
        result++;
      }
      g_free(name);
    }
  }

  return result;
}

int
main (int argc, char *argv[])
{
  int result = 0;

  if (libgedacolor_init(&argc, argv)) {

    geda_color_load_display_scheme("../etc/display-colormap-darkbg");

    result = check_display_dark_colors();

    geda_color_load_display_scheme("../etc/display-colormap-lightbg");

    result += check_display_light_colors();

    libgedacolor_release();
  }
  else {
    fprintf (stderr, "Host configuration not supported\n");
  }
  return result;
}
