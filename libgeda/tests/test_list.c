#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaList"

/*! \file test_list.c
 *  \brief Tests for geda_list.c module
 */

int check_methods (void)
{
  int result = 0;

  /* === Function 01: geda_list_new previously tested === */

  GedaList *geda_list = geda_list_new();

  if (!GEDA_IS_LIST(geda_list)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
    result++;
  }
  /* === Function 02: geda_list_add === */
  /* void geda_list_add (GedaList *list, void  *item) */

  /* === Function 03: geda_list_add_glist === */
  /* void geda_list_add_glist ( GedaList *list, GList *items ) */

  /* === Function 04: geda_list_add_glist_unique === */
  /* void geda_list_add_glist_unique (GedaList *list, GList *items) */

  /* === Function 05: geda_list_add_unique === */
  /* bool geda_list_add_unique (GedaList *list, void  *item) */

  /* === Function 06: geda_list_add_unique_string === */
  /* bool geda_list_add_unique_string (GedaList *list, char  *text) */

  /* === Function 07: geda_list_copy_glist === */
  /* GList *geda_list_copy_glist (GedaList *list) */

  /* === Function 08: geda_list_find === */
  /* void *geda_list_find (GedaList *list, void *item) */

  /* === Function 09: geda_glist_is_homogeneous_objects === */
  /* int geda_glist_is_homogeneous_objects (GList *list) */

  /* === Function 10: geda_list_is_in_list === */
  /* bool geda_list_is_in_list (GedaList *list, void *item) */

  /* === Function 11: geda_list_remove === */
  /* void geda_list_remove (GedaList *list, void *item) */

  /* === Function 11: geda_list_unref === */
  /* void geda_list_unref (GedaList *list) */

  /* === Function 11: geda_list_remove_all === */
  /* void geda_list_remove_all (GedaList *list) */

  g_object_unref(geda_list);

  return result;
}

int check_construction (void)
{
  int result = 0;

  /* === Function 01: geda_list_new === */

  GedaList *geda_list = geda_list_new();

  /* === Check get_type && is_a_whatever === */

  if (GEDA_IS_LIST(NULL)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
    result++;
  }

  if (!GEDA_IS_LIST(geda_list)) {
    fprintf(stderr, "is a %s Failed in %s\n", TOBJECT, __func__);
    result++;
  }

  g_object_unref(geda_list);

  if (GEDA_IS_LIST(geda_list)) {
    fprintf(stderr, "%s was not destroyed\n", TOBJECT);
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
    fprintf(stderr, "Check constructors in src/geda/geda_list.c\n\n");
    result   = subtotal;
    subtotal = 0;
  }

  if (!result) {

    subtotal = check_methods();
    if (subtotal) {
      fprintf(stderr, "Check methods in src/geda/geda_list.c\n\n");
      result = result + subtotal;
    }

  }
  else {
    fprintf(stderr, "discontinuing checks src/geda/geda_list.c\n\n");
  }

  return result;
}
