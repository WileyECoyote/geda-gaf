/* -*- test_list.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: March, 22nd, 2016
 */

#include "../../config.h"

#include <glib.h>
#include <libgeda.h>

#define TOBJECT "GedaList"

/*! \file test_list.c
 *  \brief Tests for geda_list.c module
 */

int check_methods (void)
{
  int i, count, nl, nu, result = 0;

  char *str;

  GList *l_list,*u_list;

  static char *lower[] = {
                          "a",
                          "b",
                          "n",
                          "d",
                          "e",
                          "f",
                          NULL};

  static char *upper[] = {
                          "A",
                          "B",
                          "C",
                          "D",
                          "E",
                          NULL};

  static char letter_g = 'g';
  static char letter_k = 'k';

  l_list = NULL;

  for (i = 0; lower[i] != NULL; i++) {
    l_list = g_list_append(l_list, lower[i]);
  }

  nl = i;

  u_list = NULL;

  for (i = 0; upper[i] != NULL; i++) {
    u_list = g_list_append(u_list, upper[i]);
  }

  nu = i;

  /* === Function 01: geda_list_new previously tested === */

  GedaList *geda_list1 = geda_list_new();

  /* === Function 02: geda_list_add === */

  /* void geda_list_add (GedaList *list, void  *item) */

  i = 0;

  for (i = 0; upper[i] != NULL; i++) {
    geda_list_add(geda_list1, upper[i]);
  }

  if (geda_list_length(geda_list1) != nu) {
    fprintf(stderr, "FAILED: geda_list_add != %d\n", nu);
    result++;
  }

  /* === Function 03: geda_list_add_glist === */

  /* void geda_list_add_glist ( GedaList *list, GList *items ) */

  GedaList *geda_list2 = geda_list_new();

  geda_list_add_glist(geda_list2, l_list);

  if (geda_list_length(geda_list2) != nl) {
    fprintf(stderr, "FAILED: geda_list_add_glist line <%d>:", __LINE__);
    fprintf(stderr, " %d != %d\n", geda_list_length(geda_list1), nl);
    result++;
  }

  geda_list_add_glist(geda_list1, geda_list_get_glist(geda_list2));

  g_object_unref(geda_list2);

  count = nl + nu;

  if (geda_list_length(geda_list1) != count) {
    fprintf(stderr, "FAILED: geda_list_add_glist line <%d>:", __LINE__);
    fprintf(stderr, " %d != %d\n", geda_list_length(geda_list1), count);
    result++;
  }

  /* === Function 04: geda_list_add_glist_unique === */

  /* void geda_list_add_glist_unique (GedaList *list, GList *items) */

  geda_list_add_glist_unique(geda_list1, l_list);

  if (geda_list_length(geda_list1) != count) {
    fprintf(stderr, "FAILED: geda_list_add_glist_unique != %d:", count);
    fprintf(stderr, " %d != %d\n", geda_list_length(geda_list1), count);
    result++;
  }

  /* === Function 05: geda_list_add_unique === */

  /* bool geda_list_add_unique (GedaList *list, void  *item) */

  str = lower[geda_random_number (0, nl - 1)];

  geda_list_add_unique(geda_list1, str);

  if (geda_list_length(geda_list1) != count) {
    fprintf(stderr, "FAILED: geda_list_add_unique\n");
    fprintf(stderr, " %d != %d\n", geda_list_length(geda_list1), count);
    result++;
  }

  /* === Function 06: geda_list_add_unique_string === */

  /* bool geda_list_add_unique_string (GedaList *list, char  *text) */

  geda_list_add_unique_string(geda_list1, &letter_g);

  count++;

  if (geda_list_length(geda_list1) != count) {
    fprintf(stderr, "FAILED: geda_list_add_unique_string line <%d>:",__LINE__);
    fprintf(stderr, " %d != %d\n", geda_list_length(geda_list1), count);
    result++;
  }

  geda_list_add_unique_string(geda_list1, "E");

  if (geda_list_length(geda_list1) != count) {
    fprintf(stderr, "FAILED: geda_list_add_unique_string line <%d>:",__LINE__);
    fprintf(stderr, " %d != %d\n", geda_list_length(geda_list1), count);
    result++;
  }

  /* === Function 07: geda_list_copy_glist === */

  /* GList *geda_list_copy_glist (GedaList *list) */

  GList *list = geda_list_copy_glist(geda_list1);

  if (g_list_length(list) != count) {
    fprintf(stderr, "FAILED: geda_list_copy_glist line <%d>:",__LINE__);
    fprintf(stderr, " %d != %d\n", g_list_length(list), count);
    result++;
  }

  g_list_free(list);

  /* === Function 08: geda_list_find === */

  /* void *geda_list_find (GedaList *list, void *item) */

  char *answer;

  answer = geda_list_find(geda_list1, &letter_g);

  if (!answer) {
    fprintf(stderr, "FAILED: geda_list_find line <%d>:",__LINE__);
    result++;
  }

  answer = geda_list_find(geda_list1, &letter_k);

  if (answer) {
    fprintf(stderr, "FAILED: geda_list_find line <%d>:",__LINE__);
    result++;
  }

  /* === Function 09: geda_list_is_homogeneous_objects === */

  /* int geda_list_is_homogeneous_objects (GList *list) */

  if (geda_list_is_homogeneous_objects(geda_list1)) {
    fprintf(stderr, "FAILED: geda_list_is_homogeneous_objects <%d>:",__LINE__);
    result++;
  }

  geda_list2 = geda_list_new();

  GedaObject *object1 = geda_arc_new();

  geda_list_add(geda_list2, object1);

  GedaObject *object2 = geda_arc_new();

  geda_list_add(geda_list2, object2);

  if (!geda_list_is_homogeneous_objects(geda_list2)) {
    fprintf(stderr, "FAILED: geda_list_is_homogeneous_objects <%d>:",__LINE__);
    result++;
  }

  GedaObject *object3 = geda_box_new();

  geda_list_add(geda_list2, object3);

  if (geda_list_is_homogeneous_objects(geda_list2)) {
    fprintf(stderr, "FAILED: geda_list_is_homogeneous_objects <%d>:",__LINE__);
    result++;
  }

  /* === Function 10: geda_list_is_in_list === */

  /* bool geda_list_is_in_list (GedaList *list, void *item) */

  GedaObject *object4 = geda_circle_new();

  if (geda_list_is_in_list(geda_list2, object4)) {
    fprintf(stderr, "FAILED: geda_list_is_in_list <%d>:",__LINE__);
    result++;
  }

  geda_list_add_unique(geda_list2, object4);

  if (!geda_list_is_in_list(geda_list2, object3)) {
    fprintf(stderr, "FAILED: geda_list_is_in_list <%d>:",__LINE__);
    result++;
  }

  if (!geda_list_is_in_list(geda_list2, object4)) {
    fprintf(stderr, "FAILED: geda_list_is_in_list <%d>:",__LINE__);
    result++;
  }

  /* === Function 11: geda_list_prepend === */

  GedaList *geda_list3 = geda_list_new();

  GedaObject *object5 = geda_arc_new();

  geda_list_add(geda_list3, object4);

  geda_list_prepend(geda_list3, object5);

  if (geda_list3->glist->data != object5) {
    fprintf(stderr, "FAILED: geda_list_prepend <%d>:",__LINE__);
    result++;
  }

  g_object_unref(object5);
  g_object_unref(geda_list3);

  /* === Function 12: geda_list_remove === */

  /* void geda_list_remove (GedaList *list, void *item) */

  geda_list_remove(geda_list2, object3);

  if (geda_list_is_in_list(geda_list2, object3)) {
    fprintf(stderr, "FAILED: geda_list_remove <%d>:",__LINE__);
    result++;
  }

  if (!geda_list_is_in_list(geda_list2, object4)) {
    fprintf(stderr, "FAILED: geda_list_remove <%d>:",__LINE__);
    result++;
  }

  /* === Function 13: geda_list_unref === */

  /* void geda_list_unref (GedaList *list) */

   geda_list_unref(geda_list1);

  if (GEDA_IS_LIST(geda_list1)) {
    fprintf(stderr, "FAILED: geda_list_unref <%d>:",__LINE__);
    result++;
  }

  /* === Function 14: geda_list_remove_all === */

  /* void geda_list_remove_all (GedaList *list) */

  geda_list_remove_all(geda_list2);

  if (geda_list_length(geda_list2)) {
    fprintf(stderr, "FAILED: geda_list_remove_all\n");
    result++;
  }

  g_list_free(l_list);
  g_list_free(u_list);

  g_object_unref(object1);
  g_object_unref(object2);
  g_object_unref(object3);
  g_object_unref(object4);

  g_object_unref(geda_list2);

  return result;
}

int check_construction (void)
{
  int result = 0;

  /* === Function 01: geda_list_new === */

  GedaList *geda_list = geda_list_new();

  /* === Check get_type && is_a_whatever === */

  if (GEDA_IS_LIST(NULL)) {
    fprintf(stderr, "is a %s FAILED: in %s\n", TOBJECT, __func__);
    result++;
  }

  if (!GEDA_IS_LIST(geda_list)) {
    fprintf(stderr, "is a %s FAILED: in %s\n", TOBJECT, __func__);
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
