/* -*- test_object_list.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 *  Date Contributed: October, 6th, 2016
 */

#include <libgeda.h>
#include <prototype_priv.h>

#include "test-suite.h"

/*! \file test_object_list.c
 *  \brief Tests for o_list.c module
 *  \par
 *  This module provides basic unit tests for functions in the object_list
 *  module.
 */

/** \defgroup test-object-list Test GEDA Object List Module
 * @{
 * \brief Group 12 src/object/o_list.c geda_object_list
 *  Group 12 == Module/File No.
 *
 * \par
 *
 *  Test Identifiers:  O  12  88  88
 *                     ^   ^   ^   ^
 *    group-code ______|   |   |   |
 *                         |   |   |
 *    Module/File No. _____|   |   |
 *                             |   |
 *    Function No._____________|   |
 *                                 |
 *    Tests Number ________________|
 *
 *  See tests/README for more details on the nomenclature for test identifiers.
 *
 *      O1201    geda_object_list_copy_all
 *      O1203    geda_object_list_find_attrib_by_name
 *      O1203    geda_object_list_find_floating
 *       O1204   geda_object_list_mirror
 *       O1205   geda_object_list_rotate
 *       O1206   geda_object_list_scale
 *       O1207   geda_object_list_set_color
 *       O1208   geda_object_list_translate
 */

int notify_attribute;

int
check_list_copy_all (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=1");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=2");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=3");
  GedaObject *attrib4 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "D=4");

  geda_struct_page_append_object(page, object);

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);

  geda_struct_page_append_list(page, list);

  geda_set_object_selected(attrib3);

  /* start a new list */
  list = g_list_append(NULL, attrib4);

  /* === Function 01: geda_object_list_copy_all  === */

  list = geda_copy_list(geda_struct_page_get_objects(page), list);

  int cnt = g_list_length(list);

  if (cnt != 5) {
    fprintf(stderr, "FAILED: (O120101) object_list_copy_all <%d>\n", cnt);
    result++;
  }

  GedaObject *obj = g_list_nth_data (list, 0);

  if (obj != attrib4) {
    fprintf(stderr, "FAILED: (O120102A) object_list_copy_all 0 <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120102B) object_list_copy_all 1 <%p>\n", obj);
    result++;
  }

  obj = g_list_nth_data (list, 1);

  if (!GEDA_IS_OBJECT(obj)) {
    fprintf(stderr, "FAILED: (O120103A) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120103B) object_list_copy_all <%p>\n", obj);
    result++;
  }

  obj = g_list_nth_data (list, 2);

  if (!GEDA_IS_OBJECT(obj)) {
    fprintf(stderr, "FAILED: (O120104A) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120104B) object_list_copy_all <%p>\n", obj);
    result++;
  }

  obj = g_list_nth_data (list, 3);

  if (!GEDA_IS_OBJECT(obj)) {
    fprintf(stderr, "FAILED: (O120105A) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120105B) object_list_copy_all <%p>\n", obj);
    result++;
  }

  obj = g_list_nth_data (list, 4);

  if (!GEDA_IS_OBJECT(obj)) {
    fprintf(stderr, "FAILED: (O120106A) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120106B) object_list_copy_all <%p>\n", obj);
    result++;
  }

  return result;
}

int
check_list_find_by_name (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");
  GedaObject *attrib4 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=d");
  GedaObject *found;

  geda_struct_page_append_object(page, object);

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);
  list = g_list_append(list, attrib4);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  /* === Function 02: geda_object_list_find_attrib_by_name  === */

  found = geda_find_attrib_by_name (object->attribs, "A", 0);

  if (found != attrib1) {
    fprintf(stderr, "FAILED: (O120201) find_attrib_by_name\n");
    result++;
  }

  found = geda_find_attrib_by_name (object->attribs, "B", 0);

  if (found != attrib2) {
    fprintf(stderr, "FAILED: (O120202) find_attrib_by_name\n");
    result++;
  }

  found = geda_find_attrib_by_name (object->attribs, "A", 1);

  if (found != attrib4) {
    fprintf(stderr, "FAILED: (O120203) find_attrib_by_name\n");
    result++;
  }

  notify_attribute = 0;

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_list_find_floating (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");

  geda_struct_page_append_object(page, object);

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);

  /* === Function 03: geda_object_list_find_floating  === */

  GList *floating;

  floating = geda_list_find_floating (list);

  if (!floating) {
    fprintf(stderr, "FAILED: (O120301A) geda_object_list_find_floating\n");
    result++;
  }
  else if (g_list_length(floating) != 3) {
    fprintf(stderr, "FAILED: (O120301B) geda_object_list_find_floating\n");
    g_list_free(floating);
    result++;
  }

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  floating = geda_list_find_floating (object->attribs);

  if (floating) {
    fprintf(stderr, "FAILED: (O120302A) geda_object_list_find_floating\n");
    result++;
  }
  else if (g_list_length(floating) != 0) {
    fprintf(stderr, "FAILED: (O120302B) geda_object_list_find_floating\n");
    result++;
  }

  notify_attribute = 0;

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

GedaToplevel *setup_new_toplevel(void)
{
  GedaToplevel *toplevel;
  Page         *page;

  toplevel = geda_toplevel_new ();
  page     = geda_struct_page_new (toplevel, "test_attrib_object.log");

  geda_toplevel_add_page(toplevel, page);

  geda_toplevel_set_current_page(toplevel, page);

  return toplevel;
}

int
main (int argc, char *argv[])
{
  GedaToplevel *toplevel;

  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  toplevel = setup_new_toplevel ();

  if (setjmp(point) == 0) {
    result += check_list_copy_all(toplevel);
  }
  else {
    fprintf(stderr, "Caught signal checking geda_object_list_copy_all\n\n");
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_list_find_by_name(toplevel);
  }
  else {
    fprintf(stderr, "Caught signal checking geda_object_list_find_attrib_by_name\n\n");
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_list_find_floating(toplevel);
  }
  else {
    fprintf(stderr, "Caught signal checking geda_object_list_find_floating\n\n");
    return 1;
  }

  g_object_unref(toplevel);

  return result;
}