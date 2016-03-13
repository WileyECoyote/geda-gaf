#include <glib.h>
#include <gtk/gtk.h>
#include <geda/geda.h>
#include <geda_accel_label.h>
#include <geda_imagemenuitem.h>

#define TWIDGET "GedaAccelLabel"

/*! \file test_accel_label.c
 *  \brief Tests for accel_label.c module
 */

int check_construction (void)
{
  int result = 0;

  const char *label_string   = "GedaAccelLabel";
  const char *multikey_accel = "GA";

  GtkWidget *menu_item = geda_image_menu_item_new_with_label("GedaMenuItem");

  //GtkWidget *widget = geda_accel_label_new("GedaAccelLabel");

  GtkWidget *widget = g_object_new (GEDA_TYPE_ACCEL_LABEL,
                                    "use-underline", TRUE,
                                    "xalign", 0.0,
                                    "visible", TRUE,
                                    "parent", menu_item,
                                    "label", label_string,
                                    "accel-string", multikey_accel,
                                    NULL);

  if (!GEDA_IS_ACCEL_LABEL(widget)) {
    fprintf(stderr, "FAILED: is a %s\n", TWIDGET);
    result++;
  }

  g_object_unref(widget);

  if (GEDA_IS_ACCEL_LABEL(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TWIDGET);
    result++;
  }

  g_object_unref(menu_item);

  return result;
}

int
main (int argc, char *argv[])
{
  int result = 0;
  int subtotal = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  subtotal = check_construction();
  if (subtotal) {
    fprintf(stderr, "Check constructors in src/widgets/accel_label.c");
    result   = subtotal;
    subtotal = 0;
  }

  return result;
}
