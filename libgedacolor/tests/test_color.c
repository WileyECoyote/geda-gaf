#include <glib.h>
#include <gdk/gdk.h>

#include <geda/geda_types.h>
#include <geda_color.h>

#include <libgedacolor.h>

#define MESH_GRID_MAJOR_RED 0xB7
#define MESH_GRID_MAJOR_GRN 0x13
#define MESH_GRID_MAJOR_BLU 0xA7

#define MESH_GRID_MINOR_RED 0xA6
#define MESH_GRID_MINOR_GRN 0xA3
#define MESH_GRID_MINOR_BLU 0x0D

int check_dark_display_colors (void)
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

    index = geda_color_key_get_index("mesh-grid-major"); /* 18 */
    if (index != 18) {
      fprintf(stderr, "FAILED: %s mesh-grid-minor <%d>\n", __func__, index);
      result++;
    }
    else {

      GdkColor *color;

      color = geda_color_x11_color_from_index(index);

      if (!color) {
        fprintf(stderr, "FAILED: %s color_from_index <%d>\n", __func__, index);
        result++;
      }
      else {

        if (color->red != MESH_GRID_MAJOR_RED) {
          fprintf(stderr, "FAILED: %s MESH_GRID_MAJOR_RED <%u>\n", __func__, color->red);
        }

        if (color->red != MESH_GRID_MAJOR_GRN) {
          fprintf(stderr, "FAILED: %s MESH_GRID_MAJOR_GRN <%u>\n", __func__, color->green);
        }

        if (color->red != MESH_GRID_MAJOR_BLU) {
          fprintf(stderr, "FAILED: %s MESH_GRID_MAJOR_BLU <%u>\n", __func__, color->blue);
        }
      }
    }

    index = geda_color_key_get_index("mesh-grid-minor"); /* 19 */
    if (index != 19) {
      fprintf(stderr, "FAILED: %s mesh-grid-minor <%d>\n", __func__, index);
      result++;
    }
    else {

      GdkColor *color;

      color = geda_color_x11_color_from_index(index);

      if (!color) {
        fprintf(stderr, "FAILED: %s color_from_index <%d>\n", __func__, index);
        result++;
      }
      else {

        if (color->red != MESH_GRID_MINOR_RED) {
          fprintf(stderr, "FAILED: %s MESH_GRID_MINOR_RED <%u>\n", __func__, color->red);
        }

        if (color->red != MESH_GRID_MINOR_GRN) {
          fprintf(stderr, "FAILED: %s MESH_GRID_MINOR_GRN <%u>\n", __func__, color->green);
        }

        if (color->red != MESH_GRID_MINOR_BLU) {
          fprintf(stderr, "FAILED: %s MESH_GRID_MINOR_BLU <%u>\n", __func__, color->blue);
        }
      }
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

int check_light_display_colors (void)
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
        fprintf(stderr, "%s: FAILED: index 3 is not green4 <%s>\n", __func__, name);
        result++;
      }
      g_free(name);
    }
  }

  return result;
}

int check_bw_display_colors (void)
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
      if (!strcmp(name,"black") == 0) {
        fprintf(stderr, "%s: FAILED: index 3 is not black <%s>\n", __func__, name);
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

  char *srcdir = getenv("srcdir");

  if (srcdir) {
    if (chdir(srcdir)) {
      fprintf (stderr, "check environment variable <srcdir>\n");
      return 0;
    }
  }

  if (libgedacolor_init(&argc, argv)) {

    geda_color_load_display_scheme("../etc/display-colormap-darkbg");

    result = check_dark_display_colors();

    geda_color_load_display_scheme("../etc/display-colormap-lightbg");

    result += check_light_display_colors();

    geda_color_load_display_scheme("../etc/display-colormap-bw");

    result += check_bw_display_colors();

    libgedacolor_release();
  }
  else {
    fprintf (stderr, "Host configuration not supported\n");
  }
  return result;
}
