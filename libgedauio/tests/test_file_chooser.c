#include <glib.h>
#include <gtk/gtk.h>
#include <geda/geda.h>
#include <geda_file_chooser.h>

#define TDIALOG "GedaFileChooser"

/*! \file test_file_chooser.c
 *  \brief Tests for geda_file_chooser.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_file_chooser_new(NULL, 0);

  if (!GEDA_IS_FILE_CHOOSER(widget)) {
    fprintf(stderr, "FAILED: is a %s\n", TDIALOG);
    result++;
  }

  g_object_unref(widget);

  if (GEDA_IS_FILE_CHOOSER(widget)) {
    fprintf(stderr, "FAILED %s destruction\n", TDIALOG);
    result++;
  }

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
    fprintf(stderr, "Check constructors in src/dialogs/geda_file_chooser.c");
    result   = subtotal;
    subtotal = 0;
  }

  return result;
}
