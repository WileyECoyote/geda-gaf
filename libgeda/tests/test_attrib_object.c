/* -*- test_attrib_object.c -*-
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
 *  Date Contributed: March, 16th, 2016
 */

#include <libgeda.h>
#include <prototype_priv.h>
#include <version.h>
#include <geda_colors.h>

#include "test-suite.h"

/*! \file test_attrib_object.c
 *  \brief Tests for o_attrib.c module
 *  \par
 *  This module provides basic unit tests for functions in the attrib_object
 *  module.
 */

/** \defgroup test-attrib-object Test GEDA attrib object Module
 * @{
 * \brief Group 3 src/object/o_attrib.c geda_attrib_object_
 *  Group 3 == Module/File No.
 */

/*
 *  Test Identifiers:  O  03  88  88
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
 *      O0301   geda_attrib_object_add
 *      O0302   geda_attrib_object_append_changed_hook
 *      O0303   geda_attrib_object_attach
 *      O0304   geda_attrib_object_attach_list
 *      O0305   geda_attrib_object_detach
 *      O0306   geda_attrib_object_detach_all
 *      O0307   geda_attrib_object_first_attrib_by_name
 *      O0308   geda_attrib_object_freeze_hooks
 *      O0309   geda_attrib_object_get_name_value
 *      O0310   geda_attrib_object_is_attached_to
 *      O0311   geda_attrib_object_is_inherited
 *      O0312   geda_attrib_object_new_attached
 *      O0313   geda_attrib_object_print
 *      O0314   geda_attrib_object_read
 *      O0315   geda_attrib_object_remove
 *      O0316   geda_attrib_object_return_attribs
 *      O0317   geda_attrib_object_search_attached_by_name
 *      O0318   geda_attrib_object_search_floating_by_name
 *      O0319   geda_attrib_object_search_inherited_by_name
 *      O0320   geda_attrib_object_search_object_by_name
 *      O0321   geda_attrib_object_search_object_string
 *      O0322   geda_attrib_object_set_integer_value
 *      O0323   geda_attrib_object_set_value
 *      O0324   geda_attrib_object_string_get_name_value
 *      O0325   geda_attrib_object_thaw_hooks
 */

int notify_attribute;

static void
test_attrib_object_notify (GedaToplevel *toplevel, GedaObject *object)
{
  if (GEDA_IS_TOPLEVEL(toplevel))
    notify_attribute++;

  if (GEDA_IS_OBJECT(object))
    notify_attribute++;
}

int
check_attrib_add(GedaToplevel *toplevel)
{
  int result = 0;

  GList      *list;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  geda_struct_page_append_object(page, object);

  /* === Function 01: geda_attrib_object_add === */

  geda_attrib_add(object, NULL);

  list = object->attribs;

  if (list != NULL) {
    fprintf(stderr, "FAILED: (O030100) geda_attrib_object_add\n");
    result++;
  }

  /* The functionality of the change-notify feature is not checked
   * here since geda_attrib_append_changed_hook is function #2 */

  geda_attrib_add(object, attrib);

  list = object->attribs;

  if (list == NULL) {
    fprintf(stderr, "FAILED: (O030101A) geda_attrib_object_add\n");
    result++;
  }
  else if (list->data != attrib) {
    fprintf(stderr, "FAILED: (O030101B) geda_attrib_object_add\n");
    result++;
  }

  geda_struct_page_remove_object (page, object);
  g_object_unref (object);

  return result;
}

int
check_append_attrib_changed_hook(GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  geda_struct_page_append_object(page, object);

  /* === Function 02: geda_attrib_object_append_changed_hook === */

  geda_attrib_append_changed_hook (page, (AttribsChangedFunc) NULL, toplevel);

  if (page->attribs_changed_hooks) {
    fprintf(stderr, "FAILED: (O030200) geda_attrib_append_changed_hook\n");
    result++;
  }

  /* Note leaving connected */
  geda_attrib_append_changed_hook (page, (AttribsChangedFunc) test_attrib_object_notify,
                                         toplevel);

  notify_attribute = 0;

  geda_attrib_add(object, attrib);

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O030201) geda_attrib_append_changed_hook\n");
    result++;
  }

  notify_attribute = 0;

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_attach (GedaToplevel *toplevel)
{
  int result = 0;
  int color;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object1 = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib  = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  geda_struct_page_append_object(page, object1);

  /* === Function 03: geda_attrib_object_attach === */

  notify_attribute = 0;

  geda_attrib_attach(object1, NULL, FALSE);

  if (notify_attribute != 0) {
    fprintf(stderr, "FAILED: (O030300) geda_attrib_object_attach\n");
    result++;
  }

  notify_attribute = 0;

  /* Note FALSE = do not modify color */
  geda_attrib_attach(object1, attrib, FALSE);

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O030301A) geda_attrib_object_attach\n");
    result++;
  }

  notify_attribute = 0;

  color = geda_object_get_color(attrib);
  if (color != 3) {
    fprintf(stderr, "FAILED: (O030301B) geda_attrib_object_attach color <%d>\n", color);
    result++;
  }

  attrib = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");

  /* Note TRUE = set attribute color */
  geda_attrib_attach(object1, attrib, TRUE);

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O030302A) geda_attrib_object_attach\n");
    result++;
  }

  notify_attribute = 0;

  color = geda_object_get_color(attrib);
  if (color != ATTRIBUTE_COLOR) {
    fprintf(stderr, "FAILED: (O030302B) geda_attrib_object_attach color <%d>\n", color);
    result++;
  }

  /* Try to attach the same object again */
  geda_attrib_attach(object1, attrib, TRUE);

  if (notify_attribute != 0) { /* Should not notify since no change made */
    fprintf(stderr, "FAILED: (O030303) geda_attrib_object_attach\n");
    result++;
  }

  notify_attribute = 0;

  GedaObject *object2 = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  /* Try to attach a non-text object */
  geda_attrib_attach(object1, object2, FALSE);

  if (notify_attribute != 0) { /* Should not notify since no change made */
    fprintf(stderr, "FAILED: (O030304) geda_attrib_object_attach\n");
    result++;
  }

  geda_struct_page_append_object(page, object2);

  /* Try to attach last attrib to object2 when already attached to object1 */
  geda_attrib_attach(object2, attrib, FALSE);

  if (notify_attribute != 0) { /* Should not notify since no change made */
    fprintf(stderr, "FAILED: (O030305) geda_attrib_object_attach\n");
    result++;
  }

  if (g_list_length(object1->attribs) != 2) { /* Just checking count here */
    fprintf(stderr, "FAILED: (O030306) geda_attrib_object_attach\n");
    result++;
  }

  geda_struct_page_remove_object (page, object2);
  geda_struct_page_remove_object (page, object1);

  g_object_unref (object2);
  g_object_unref (object1);

  return result;
}

int
check_attrib_attach_list (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");

  geda_struct_page_append_object(page, object);

  /* === Function 04: geda_attrib_object_attach_list === */

  geda_attrib_attach_list(object, NULL, FALSE);

  GList *list;

  list = g_list_append(NULL, attrib2);
  list = g_list_remove(list, attrib2);

  geda_attrib_attach_list(object, list, FALSE);

  list = g_list_append(list, attrib1);
  list = g_list_append(list, attrib2);

  /* Note TRUE = set attribute color */
  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  if (notify_attribute != 4) {
    fprintf(stderr, "FAILED: (O030401) geda_attrib_object_attach_list\n");
    result++;
  }

  notify_attribute = 0;

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_detach (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");

  geda_struct_page_append_object(page, object);
  geda_struct_page_append_object(page, attrib1);

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);

  /* Note TRUE = set attribute color */
  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  notify_attribute = 0;

  /* === Function 05: geda_attrib_object_detach === */

  geda_attrib_detach(attrib2);

  /* attrib2 is floating so there should be no notification */
  if (notify_attribute != 0) {
    fprintf(stderr, "FAILED: (O030501) geda_attrib_object_detach\n");
    result++;
  }
  notify_attribute = 0;

  geda_attrib_detach(attrib1);

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O030502A) geda_attrib_object_detach\n");
    result++;
  }
  notify_attribute = 0;

  if (g_list_length(object->attribs) != 1) { /* Just checking count here */
    fprintf(stderr, "FAILED: (O030502B) geda_attrib_object_detach\n");
    result++;
  }

  list = object->attribs;

  if (list->data != attrib3) { /* The only one still atached */
    fprintf(stderr, "FAILED: (O030503) geda_attrib_object_detach\n");
    result++;
  }

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_detach_all (GedaToplevel *toplevel)
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

  geda_attrib_attach_list(object, list, FALSE);

  notify_attribute = 0;

  /* === Function 06: geda_attrib_object_detach_all === */

  geda_attrib_detach_all (object);

  if (object->attribs) {
    fprintf(stderr, "FAILED: (O030601) geda_attrib_object_detach_all\n");
    result++;
  }

  /* All of the attribute were floating so there should be no notification */
  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O030602) geda_attrib_object_detach_all %d\n", notify_attribute);
    result++;
  }
  notify_attribute = 0;

  geda_struct_page_append_object(page, attrib1);          /* Add one attrib to the page */

  geda_attrib_attach_list(object, list, FALSE); /* reattach the attributes */

  notify_attribute = 0;

  geda_attrib_detach_all (object);

  if (notify_attribute != 4) {
    fprintf(stderr, "FAILED: (O030603) geda_attrib_object_detach_all\n");
    result++;
  }
  notify_attribute = 0;

  geda_struct_page_remove_object (page, object);
  g_list_free(list);
  g_object_unref (object);

  return result;
}

int
check_find_first_attrib_by_name (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=c");
  GedaObject *found;

  geda_struct_page_append_object(page, object);

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  /* === Function 07: geda_attrib_object_first_attrib_by_name  === */

  found = geda_attrib_first_attrib_by_name (object, "A");

  if (!found) {
    fprintf(stderr, "FAILED: (O030701A) geda_attrib_object_first_attrib_by_name\n");
    result++;
  }
  else if (found != attrib1) {
    fprintf(stderr, "FAILED: (O030701B) geda_attrib_object_first_attrib_by_name\n");
    result++;
  }

  notify_attribute = 0;

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_freeze_hooks (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  geda_struct_page_append_object(page, object);

  /* === Function 08: geda_attrib_object_freeze_hooks  === */

  geda_attrib_freeze_hooks(object);

  notify_attribute = 0;

  geda_attrib_attach(object, attrib1, FALSE);

  if (notify_attribute != 0) {
    fprintf(stderr, "FAILED: (O030801) geda_attrib_object_freeze_hooks\n");
    result++;
  }

  notify_attribute = 0;

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_get_attrib_name_value(GedaToplevel *toplevel)

{
  int result = 0;

  char *name;
  char *value;

  GedaObject *attrib = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  /* === Function 10: geda_attrib_object_get_name_value  === */

  if (geda_attrib_get_name_value(NULL, NULL, NULL)) {
    fprintf(stderr, "FAILED: (O030900A) geda_attrib_object_get_name_value\n");
    result++;
  }

  if (!geda_attrib_get_name_value(attrib, NULL, NULL)) {
    fprintf(stderr, "FAILED: (O030900B) geda_attrib_object_get_name_value\n");
    result++;
  }

  if (!geda_attrib_get_name_value(attrib, &name, NULL)) {
    fprintf(stderr, "FAILED: (O030901A) geda_attrib_object_get_name_value\n");
    result++;
  }
  else if (strncmp(name, "A", 1)) {
    fprintf(stderr, "FAILED: (O030901B) geda_attrib_object_get_name_value <%s>\n", name);
    result++;
  }

  GEDA_FREE(name);

  if (!geda_attrib_get_name_value(attrib, NULL, &value)) {
    fprintf(stderr, "FAILED: (O030902A) geda_attrib_object_get_name_value\n");
    result++;
  }
  else if (strncmp(value, "a", 1)) {
    fprintf(stderr, "FAILED: (O030902B) geda_attrib_object_get_name_value <%s>\n", value);
    result++;
  }

  GEDA_FREE(value);

  if (!geda_attrib_get_name_value(attrib, &name, &value)) {
    fprintf(stderr, "FAILED: (O030903A) geda_attrib_object_get_name_value\n");
    result++;
  }
  else if (strncmp(name, "A", 1) || strncmp(value, "a", 1)) {
    fprintf(stderr, "FAILED: (O030903B) geda_attrib_object_get_name_value <%s=%s>\n", name, value);
    result++;
  }

  GEDA_FREE(name);
  GEDA_FREE(value);

  g_object_unref (attrib);

  return result;
}

int
check_attrib_is_attached_to (GedaToplevel *toplevel)
{
  int result = 0;

  GedaObject *object1 = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *object2 = geda_box_object_new(3, 10, 20, 30, 40);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  /* === Function 10: geda_attrib_object_is_attached_to  === */

  if (geda_attrib_is_attached_to(NULL, NULL)) {
    fprintf(stderr, "FAILED: (O031000A) geda_attrib_object_is_attached_to\n");
    result++;
  }

  if (geda_attrib_is_attached_to(object1, NULL)) {
    fprintf(stderr, "FAILED: (O031000A) geda_attrib_object_is_attached_to\n");
    result++;
  }

  geda_attrib_attach(object1, attrib1, FALSE);

  if (!geda_attrib_is_attached_to(attrib1, object1)) {
    fprintf(stderr, "FAILED: (O031001) geda_attrib_object_is_attached_to\n");
    result++;
  }

  if (geda_attrib_is_attached_to( attrib1, object2)) {
    fprintf(stderr, "FAILED: (O031002) geda_attrib_object_is_attached_to\n");
    result++;
  }

  g_object_unref (object1);
  g_object_unref (object2);

  return result;
}

int
check_attrib_is_inherited (GedaToplevel *toplevel)
{
  int result = 0;

  GedaObject *object1 = geda_complex_new();

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  /* === Function 11: geda_attrib_object_is_inherited  === */

  if (geda_attrib_is_inherited(NULL)) {
    fprintf(stderr, "FAILED: (O031100A) geda_attrib_object_is_attached_to\n");
    result++;
  }

  if (geda_attrib_is_inherited(attrib1)) {
    fprintf(stderr, "FAILED: (O031101) geda_attrib_object_is_attached_to\n");
    result++;
  }

  attrib1->parent_object = object1;

  /* Note did not bother adding attrib1 to parent's attribute list */
  if (!geda_attrib_is_inherited(attrib1)) {
    fprintf(stderr, "FAILED: (O031102) geda_attrib_object_is_attached_to\n");
    result++;
  }

  g_object_unref (object1);

  return result;
}

int
check_new_attached_attrib(GedaToplevel *toplevel)

{
  int result = 0;

  GedaObject *attrib;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object1 = geda_complex_new ();
  GedaObject *object2 = geda_complex_new ();

  geda_struct_page_append_object(page, object2);

  /* === Function 12: geda_attrib_object_new_attached  === */

  /* Yes this does produce an attribute, "unknown=empty" */
  attrib = geda_attrib_new_attached (NULL, NULL, NULL, 0, 0);

  if (!GEDA_IS_OBJECT(attrib)) {
    fprintf(stderr, "FAILED: (O031200A) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (!GEDA_IS_TEXT(attrib)) {
    fprintf(stderr, "FAILED: (O031200B) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (attrib->page) {
    fprintf(stderr, "FAILED: (O031200C) geda_attrib_object_new_attached\n");
    result++;
  }
  g_object_unref (attrib);

  attrib = geda_attrib_new_attached (NULL, "A", "a", 1, 1);

  if (!GEDA_IS_OBJECT(attrib)) {
    fprintf(stderr, "FAILED: (O031201A) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (!GEDA_IS_TEXT(attrib)) {
    fprintf(stderr, "FAILED: (O031201B) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (attrib->page) {
    fprintf(stderr, "FAILED: (O031201C) geda_attrib_object_new_attached\n");
    result++;
  }
  g_object_unref (attrib);

  attrib = geda_attrib_new_attached (object1, "A", "a", 1, 1);

  if (!GEDA_IS_OBJECT(attrib)) {
    fprintf(stderr, "FAILED: (O031202A) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (!GEDA_IS_TEXT(attrib)) {
    fprintf(stderr, "FAILED: (O031202B) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (attrib->page) {
    fprintf(stderr, "FAILED: (O031202C) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (!geda_attrib_object_is_attached_to(attrib, object1)) {
    fprintf(stderr, "FAILED: (O031202D) geda_attrib_object_new_attached");
    result++;
  }
  g_object_unref (attrib);

  attrib = geda_attrib_new_attached (object2, "A", "a", 1, 1);

  if (!GEDA_IS_OBJECT(attrib)) {
    fprintf(stderr, "FAILED: (O031203A) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (!GEDA_IS_TEXT(attrib)) {
    fprintf(stderr, "FAILED: (O031203B) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (attrib->page != page) {
    fprintf(stderr, "FAILED: (O031203C) geda_attrib_object_new_attached\n");
    result++;
  }
  else if (!geda_attrib_object_is_attached_to(attrib, object2)) {
    fprintf(stderr, "FAILED: (O031203D) geda_attrib_object_new_attached\n");
    result++;
  }
  g_object_unref (attrib);

  geda_struct_page_remove_object (page, attrib); /* now floating */
  geda_struct_page_remove_object (page, object2);

  g_object_unref (object1);
  g_object_unref (object2);

  return result;
}

int
check_attrib_print (GedaToplevel *toplevel)
{
  int result = 0;

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);

   /* === Function 13: geda_attrib_object_print  === */
  geda_attrib_print (NULL);

  geda_attrib_print (list);

  g_list_free(list);

  return result;
}

int
check_attrib_read (GedaToplevel *toplevel)
{
  int  converted;
  int  result;
  unsigned version;

  result    = 0;
  converted = sscanf (PACKAGE_DATE_VERSION, "%u", &version);

  if (!converted) {
    fprintf(stderr, "File %s, <%s>: could not scan version", __FILE__, __func__);
    version=19700101;
    result++;
  }

  GedaObject *object1 = geda_complex_new ();

  const char *buffer0 = "T 250 1650 5 8 1 1 0 6 1\n"
                        "pinnumber=3\n"
                        "T 450 1550 9 10 1 1 0 0 1\n"
                        "pinlabel=COMP\n"
                        "T 250 1650 5 8 0 1 0 6 1\n"
                        "pinseq=3\n"
                        "T 450 1525 5 8 0 1 0 2 1\n"
                        "pintype=pas\n"
                        "}\n";

  TextBuffer *tb = geda_struct_textbuffer_new (buffer0, strlen(buffer0));

  /* === Function 14: geda_attrib_object_read  === */

  GList *list = geda_attrib_object_read (toplevel,
                                         object1,
                                         tb,
                                         version,
                                         FILEFORMAT_VERSION,
                                         NULL);

  if (!list) {
    fprintf(stderr, "FAILED: (O031401A) geda_attrib_object_read\n");
    result++;
  }
  else {

    const GList *a_iter = geda_object_get_attached(object1);

    if (!a_iter) {
      fprintf(stderr, "FAILED: (O031401B) geda_attrib_object_read\n");
      result++;
    }
    else {

      int cnt = geda_glist_length(a_iter);

      if (cnt != 4) {
        fprintf(stderr, "FAILED: (O031401C) geda_attrib_object_read <%d>\n", cnt);
        result++;
      }

      /* \note: geda_attrib_object_read uses geda_text_object_read so along as
       *        as text objects are returned, any errors within the data
       *        would be on geda_text_object_read not geda_attrib_object_read.
       */

      GedaObject *attrib1 = a_iter->data;

      if (!GEDA_IS_TEXT(attrib1)) {
        fprintf(stderr, "FAILED: (O031401D1) geda_attrib_object_read\n");
        result++;
      }
      else {

        char *str = attrib1->text->string;

        if (!str || strcmp(str, "pinnumber=3") != 0) {
          fprintf(stderr, "FAILED: (O031401D2) geda_attrib_object_read <%s>\n", str);
          result++;
        }
      }

      a_iter = a_iter->next;

      GedaObject *attrib2 = a_iter->data;

      if (!GEDA_IS_TEXT(attrib2)) {
        fprintf(stderr, "FAILED: (O031401D1) geda_attrib_object_read\n");
        result++;
      }
      else {

        char *str = attrib2->text->string;

        if (!str || strcmp(str, "pinlabel=COMP") != 0) {
          fprintf(stderr, "FAILED: (O031401D2) geda_attrib_object_read <%s>\n", str);
          result++;
        }
      }
    }
  }

  geda_struct_textbuffer_free (tb);
  g_object_unref (object1);
  g_list_free(list);

  return result;
}

int
check_attrib_remove (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_box_object_new(3, 10, 20, 30, 40);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");

  geda_struct_page_append_object(page, object);

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  /* === Function 15: geda_attrib_object_remove  === */

  notify_attribute = 0;

  geda_attrib_remove(&object->attribs, attrib3);

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O031501A) geda_attrib_object_remove\n");
    result++;
  }
  else {

    GedaObject *found = geda_attrib_first_attrib_by_name (object, "C");

    if (found) {
      fprintf(stderr, "FAILED: (O031501B) geda_attrib_object_remove\n");
      result++;
    }
  }

  notify_attribute = 0;

  geda_attrib_remove(&object->attribs, attrib1);

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O031502A) geda_attrib_object_remove\n");
    result++;
  }
  else {

    GedaObject *found = geda_attrib_first_attrib_by_name (object, "A");

    if (found) {
      fprintf(stderr, "FAILED: (O031502B) geda_attrib_object_remove\n");
      result++;
    }
  }

  notify_attribute = 0;

  list = object->attribs;

  if (g_list_length(list) != 1) {
    fprintf(stderr, "FAILED: (O031503A) geda_attrib_object_remove\n");
    result++;
  }
  else {
    GedaObject *attrib4 = list->data;
    if (attrib4 != attrib2) {
      fprintf(stderr, "FAILED: (O031503B) geda_attrib_object_remove\n");
      result++;
    }
  }

  geda_struct_page_remove_object (page, object);

  g_object_unref (attrib1); /* Not attached so explicitly destroy */
  g_object_unref (attrib3);
  g_object_unref (object);

  return result;
}

int
check_return_attribs (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_complex_new();

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "FA=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "FB=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "FC=c");
  GedaObject *attrib4 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=1");
  GedaObject *attrib5 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=2");
  GedaObject *attrib6 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=3");

  geda_struct_page_append_object(page, object);

  geda_complex_append (object->complex, attrib1);
  geda_complex_append (object->complex, attrib2);
  geda_complex_append (object->complex, attrib3);

  GList *list;

  list = g_list_append(NULL, attrib4);
  list = g_list_append(list, attrib5);
  list = g_list_append(list, attrib6);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  notify_attribute = 0;

  /* === Function 16: geda_attrib_object_return_attribs  === */

  list = geda_attrib_return_attribs (object);

  if (!list) {
    fprintf(stderr, "FAILED: (O031601A) geda_attrib_object_return_attribs\n");
    result++;
  }
  else if (g_list_length(list) != 6) {
    fprintf(stderr, "FAILED: (O031601B) geda_attrib_object_return_attribs\n");
    result++;
  }

  /* First member is the first floating attribute */
  GedaObject *attrib4R = list->data;

  if (!attrib4R) {
    fprintf(stderr, "FAILED: (O031602A) geda_attrib_object_return_attribs\n");
    result++;
  }
  else if (attrib4R != attrib4) {
    fprintf(stderr, "FAILED: (O031602B) geda_attrib_object_return_attribs\n");
    result++;
  }

  /* Forth member is the first attached attribute */
  GedaObject *attrib1R = g_list_nth_data(list, 3);

  if (!attrib1R) {
    fprintf(stderr, "FAILED: (O031603A) geda_attrib_object_return_attribs\n");
    result++;
  }
  else if (attrib1R != attrib1) {
    fprintf(stderr, "FAILED: (O031603B) geda_attrib_object_return_attribs\n");
    result++;
  }

  g_list_free(list);

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_search_attached_by_name (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=c");

  geda_struct_page_append_object(page, object);

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  /* === Function 17: geda_attrib_object_search_attached_by_name  === */

  char *value = geda_attrib_search_attached_by_name (object, "A", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O031701A) geda_attrib_object_search_attached_by_name\n");
    result++;
  }
  else if (strcmp(value, "a")) {
    fprintf(stderr, "FAILED: (O031701B) geda_attrib_object_search_attached_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    result++;
  }
  g_free(value);
  value = NULL;

  value = geda_attrib_search_attached_by_name (object, "A", 1);

  if (!value) {
    fprintf(stderr, "FAILED: (O031702A) geda_attrib_object_search_attached_by_name\n");
    result++;
  }
  else if (strcmp(value, "b")) {
    fprintf(stderr, "FAILED: (O031702B) geda_attrib_object_search_attached_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    result++;
  }
  g_free(value);
  value = NULL;

    value = geda_attrib_search_attached_by_name (object, "A", 2);

  if (!value) {
    fprintf(stderr, "FAILED: (O031703A) geda_attrib_object_search_attached_by_name\n");
    result++;
  }
  else if (strcmp(value, "c")) {
    fprintf(stderr, "FAILED: (O031703B) geda_attrib_object_search_attached_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    result++;
  }
  g_free(value);
  value = NULL;

  notify_attribute = 0;

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_search_floating_by_name (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_complex_new();

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");
  GedaObject *attrib4 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=1");
  GedaObject *attrib5 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=2");
  GedaObject *attrib6 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=3");
  GedaObject *attrib7 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "D=4");

  geda_struct_page_append_object(page, object);

  geda_complex_append (object->complex, attrib1);
  geda_complex_append (object->complex, attrib2);
  geda_complex_append (object->complex, attrib3);

  GList *list;

  list = g_list_append(NULL, attrib4);
  list = g_list_append(list, attrib5);
  list = g_list_append(list, attrib6);
  list = g_list_append(list, attrib7);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  notify_attribute = 0;

  /* === Function 18: geda_attrib_object_search_floating_by_name  === */
  char *value = geda_attrib_search_floating_by_name (NULL, "A", 0);

  value = geda_attrib_search_floating_by_name (object->complex->prim_objs, "A", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O031801A) geda_attrib_object_search_floating_by_name\n");
    result++;
  }
  else if (strcmp(value, "a")) {
    fprintf(stderr, "FAILED: (O031801B) geda_attrib_object_search_floating_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  value = geda_attrib_search_floating_by_name (object->complex->prim_objs, "B", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O031802A) geda_attrib_object_search_floating_by_name\n");
    result++;
  }
  else if (strcmp(value, "b")) {
    fprintf(stderr, "FAILED: (O031802B) geda_attrib_object_search_floating_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  value = geda_attrib_search_floating_by_name (object->complex->prim_objs, "C", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O031803A) geda_attrib_object_search_floating_by_name\n");
    result++;
  }
  else if (strcmp(value, "c")) {
    fprintf(stderr, "FAILED: (O031803B) geda_attrib_object_search_floating_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

    /* Should not return and attribute since is not floating */
  value = geda_attrib_search_floating_by_name (object->complex->prim_objs, "D", 0);

  if (value) {
    fprintf(stderr, "FAILED: (O031804) geda_attrib_object_search_floating_by_name\n");
    g_free(value);
    value = NULL;
    result++;
  }

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_search_inherited_by_name (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_complex_new();

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");
  GedaObject *attrib4 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=1");
  GedaObject *attrib5 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=2");
  GedaObject *attrib6 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=3");
  GedaObject *attrib7 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "D=4");

  geda_struct_page_append_object(page, object);

  geda_complex_append (object->complex, attrib1);
  geda_complex_append (object->complex, attrib2);
  geda_complex_append (object->complex, attrib3);

  GList *list;

  list = g_list_append(NULL, attrib4);
  list = g_list_append(list, attrib5);
  list = g_list_append(list, attrib6);
  list = g_list_append(list, attrib7);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  notify_attribute = 0;

  /* === Function 19: geda_attrib_object_search_inherited_by_name  === */
  char *value = geda_attrib_search_inherited_by_name (NULL, "A", 0);

  value = geda_attrib_search_inherited_by_name (object, "A", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O031901A) geda_attrib_object_search_inherited_by_name\n");
    result++;
  }
  else if (strcmp(value, "a")) {
    fprintf(stderr, "FAILED: (O031901B) geda_attrib_object_search_inherited_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  value = geda_attrib_object_search_inherited_by_name (object, "B", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O031902A) geda_attrib_object_search_inherited_by_name\n");
    result++;
  }
  else if (strcmp(value, "b")) {
    fprintf(stderr, "FAILED: (O031902B) geda_attrib_object_search_inherited_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  value = geda_attrib_object_search_inherited_by_name (object, "C", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O031903A) geda_attrib_object_search_inherited_by_name\n");
    result++;
  }
  else if (strcmp(value, "c")) {
    fprintf(stderr, "FAILED: (O031903B) geda_attrib_object_search_inherited_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  /* Should not return and attribute since is not inherited */
  value = geda_attrib_object_search_inherited_by_name (object, "D", 0);

  if (value) {
    fprintf(stderr, "FAILED: (O031904A) geda_attrib_object_search_inherited_by_name\n");
    g_free(value);
    value = NULL;
    result++;
  }

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_search_object_by_name (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_complex_new();

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");
  GedaObject *attrib4 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=1");
  GedaObject *attrib5 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=2");
  GedaObject *attrib6 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=3");
  GedaObject *attrib7 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "D=4");

  geda_struct_page_append_object(page, object);

  geda_complex_append (object->complex, attrib1);
  geda_complex_append (object->complex, attrib2);
  geda_complex_append (object->complex, attrib3);

  GList *list;

  list = g_list_append(NULL, attrib4);
  list = g_list_append(list, attrib5);
  list = g_list_append(list, attrib6);
  list = g_list_append(list, attrib7);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);

  notify_attribute = 0;

  /* === Function 20: geda_attrib_object_search_object_by_name  === */
  char *value = geda_attrib_search_object_by_name (NULL, "A", 0);

  /* Note the attached are search first! */
  value = geda_attrib_search_object_by_name (object, "A", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O032001A) geda_attrib_object_search_object_by_name\n");
    result++;
  }
  else if (strcmp(value, "1")) {
    fprintf(stderr, "FAILED: (O032001B) geda_attrib_object_search_object_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  value = geda_attrib_search_object_by_name (object, "A", 1);

  if (!value) {
    fprintf(stderr, "FAILED: (O032002A) geda_attrib_object_search_object_by_name\n");
    result++;
  }
  else if (strcmp(value, "a")) {
    fprintf(stderr, "FAILED: (O032002B) geda_attrib_object_search_object_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  value = geda_attrib_search_object_by_name (object, "C", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O032003A) geda_attrib_object_search_object_by_name\n");
    result++;
  }
  else if (strcmp(value, "3")) {
    fprintf(stderr, "FAILED: (O032003B) geda_attrib_object_search_object_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  value = geda_attrib_search_object_by_name (object, "D", 0);

  if (!value) {
    fprintf(stderr, "FAILED: (O032004A) geda_attrib_object_search_object_by_name\n");
    result++;
  }
  else if (strcmp(value, "4")) {
    fprintf(stderr, "FAILED: (O032004B) geda_attrib_object_search_object_by_name");
    fprintf(stderr, " returned <%s>\n", value);
    g_free(value);
    value = NULL;
    result++;
  }

  /* No such attribute exist */
  value = geda_attrib_search_object_by_name (object, "E", 0);

  if (value) {
    fprintf(stderr, "FAILED: (O032005) geda_attrib_object_search_object_by_name\n");
    result++;
  }

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_search_object_for_string  (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_complex_new();

  GedaObject *attrib1 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");
  GedaObject *attrib4 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=1");
  GedaObject *attrib5 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "B=2");
  GedaObject *attrib6 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=3");
  GedaObject *attrib7 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "D=4");

  geda_struct_page_append_object(page, object);

  geda_complex_append (object->complex, attrib1);
  geda_complex_append (object->complex, attrib2);
  geda_complex_append (object->complex, attrib3);

  GList *list;

  list = g_list_append(NULL, attrib4);
  list = g_list_append(list, attrib5);
  list = g_list_append(list, attrib6);
  list = g_list_append(list, attrib7);

  geda_attrib_attach_list(object, list, FALSE);

  g_list_free(list);
  list = NULL;

  geda_struct_page_append_object(page, object);
  geda_struct_page_append_object(page, attrib1);

  notify_attribute = 0;

  /* === Function 21: geda_attrib_object_search_object_string  === */

  list = geda_attrib_search_for_string(object, "A=", 0);

  if (!list) {
    fprintf(stderr, "FAILED: (O032101A) geda_attrib_object_search_object_string\n");
    result++;
  }
  else {

    int value = g_list_length (list);

    if (value - 2) {
      fprintf(stderr, "FAILED: (O032101B) geda_attrib_object_search_object_string\n");
      result++;
    }
    g_list_free(list);
  }

  list = NULL;

  list = geda_attrib_search_for_string(object, "A=a", 1);

  if (!list) {
    fprintf(stderr, "FAILED: (O032102A) geda_attrib_object_search_object_string\n");
    result++;
  }
  else {

    int value = g_list_length (list);

    if (value - 1) {
      fprintf(stderr, "FAILED: (O032102B) geda_attrib_object_search_object_string\n");
      result++;
    }
    g_list_free(list);
  }

  list = NULL;

  list = geda_attrib_search_for_string(object, "F=", 0);

  if (list) {
    fprintf(stderr, "FAILED: (O032103) geda_attrib_object_search_object_string\n");
    result++;
  }

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_set_integer_value(GedaToplevel *toplevel)

{
  int result = 0;
  int count;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  geda_struct_page_append_object(page, object);
  geda_struct_page_append_object(page, attrib);

  /* Note FALSE = do not modify color */
  geda_attrib_attach(object, attrib, FALSE);

  notify_attribute = 0;

  /* === Function 22: geda_attrib_object_set_integer_value  === */

  geda_attrib_set_integer_value(NULL, "B", 3);

  for (count = 0; count < 10; count++) {

    int i = geda_random_number (0, 9000000);

    geda_attrib_set_integer_value(attrib, "B", i);

    if (notify_attribute != 2) {
      fprintf(stderr, "FAILED: (O032201A) geda_attrib_object_set_integer_value %d\n",notify_attribute);
      result++;
    }

    notify_attribute = 0;

    char *name;
    char *value;

    if (!geda_attrib_get_name_value(attrib, &name, &value)) {
      fprintf(stderr, "FAILED: (O032201B) geda_attrib_object_set_integer_value\n");
      result++;
    }
    else {

      char *val = geda_sprintf("%d", i); /* get what the answer should be */
      int   len = strlen(val);

      if (strncmp(name, "B", 1) || strncmp(value, val, len)) {
        fprintf(stderr, "FAILED: (O032201C) geda_attrib_object_set_integer_value <%s=%s>\n", name, value);
        result++;
      }
      g_free(val);
    }

    GEDA_FREE(name);
    GEDA_FREE(value);
  }

  notify_attribute = 0;

  geda_struct_page_remove_object (page, attrib);
  geda_struct_page_remove_object (page, object);
  g_object_unref (object);

  return result;
}

int
check_attrib_set_value(GedaToplevel *toplevel)

{
  int result = 0;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  geda_struct_page_append_object(page, object);
  geda_struct_page_append_object(page, attrib);

  /* Note FALSE = do not modify color */
  geda_attrib_attach(object, attrib, FALSE);

  notify_attribute = 0;

  /* === Function 23: geda_attrib_object_set_value  === */

  geda_attrib_set_value(NULL, "B", "");

  geda_attrib_set_value(attrib, "B", "2");

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O032301A) geda_attrib_object_set_value %d\n",notify_attribute);
    result++;
  }

  char *name;
  char *value;

  if (!geda_attrib_get_name_value(attrib, &name, &value)) {
    fprintf(stderr, "FAILED: (O032301B) geda_attrib_object_set_value\n");
    result++;
  }
  else if (strncmp(name, "B", 1) || strncmp(value, "2", 1)) {
    fprintf(stderr, "FAILED: (O032301C) geda_attrib_object_set_value <%s=%s>\n", name, value);
    result++;
  }

  GEDA_FREE(name);
  GEDA_FREE(value);

  geda_struct_page_remove_object (page, attrib);
  geda_struct_page_remove_object (page, object);
  g_object_unref (object);

  return result;
}

int
check_string_get_name_value(GedaToplevel *toplevel)

{
  int result = 0;

  char *name;
  char *value;

  /* === Function 24: geda_attrib_object_string_get_name_value  === */

  if (geda_attrib_string_get_name_value (NULL, NULL, NULL)) {
    fprintf(stderr, "FAILED: (O032400) geda_utility_string_get_name_value\n");
    result++;
  }

  if (geda_attrib_string_get_name_value ("", NULL, NULL)) {
    fprintf(stderr, "FAILED: (O032401) geda_utility_string_get_name_value\n");
    result++;
  }

  if (geda_attrib_string_get_name_value ("=", NULL, NULL)) {
    fprintf(stderr, "FAILED: (O032402) geda_utility_string_get_name_value\n");
    result++;
  }

  if (geda_attrib_string_get_name_value ("A=", NULL, NULL)) {
    fprintf(stderr, "FAILED: (O032403) geda_utility_string_get_name_value\n");
    result++;
  }

  if (geda_attrib_string_get_name_value ("=B", NULL, NULL)) {
    fprintf(stderr, "FAILED: (O032404) geda_utility_string_get_name_value\n");
    result++;
  }

  if (geda_attrib_string_get_name_value ("A= B", NULL, NULL)) {
    fprintf(stderr, "FAILED: (O032405) geda_utility_string_get_name_value\n");
    result++;
  }

  if (geda_attrib_string_get_name_value ("A =B", NULL, NULL)) {
    fprintf(stderr, "FAILED: (O032406) geda_utility_string_get_name_value\n");
    result++;
  }

  if (geda_attrib_string_get_name_value ("A B", NULL, NULL)) {
    fprintf(stderr, "FAILED: (O032407) geda_utility_string_get_name_value\n");
    result++;
  }

  if (!geda_attrib_string_get_name_value ("A=B", &name, &value)) {
    fprintf(stderr, "FAILED: (O032408A) geda_utility_string_get_name_value\n");
    result++;
  }
  else if (strcmp(name, "A")) {
    fprintf(stderr, "FAILED: (O032408B) geda_utility_string_get_name_value\n");
    result++;
  }
  else if (strcmp(value, "B")) {
    fprintf(stderr, "FAILED: (O032408C) geda_utility_string_get_name_value\n");
    result++;
  }

  if (!geda_attrib_string_get_name_value ("ABABABABAB=BABABABABA", &name, &value)) {
    fprintf(stderr, "FAILED: (O032409A) geda_utility_string_get_name_value\n");
    result++;
  }
  else if (strcmp(name, "ABABABABAB")) {
    fprintf(stderr, "FAILED: (O032409B) geda_utility_string_get_name_value\n");
    result++;
  }
  else if (strcmp(value, "BABABABABA")) {
    fprintf(stderr, "FAILED: (O032409C) geda_utility_string_get_name_value\n");
    result++;
  }

  if (!geda_attrib_string_get_name_value ("A B=B A", &name, &value)) {
    fprintf(stderr, "FAILED: (O032410A) geda_utility_string_get_name_value\n");
    result++;
  }
  else if (strcmp(name, "A B")) {
    fprintf(stderr, "FAILED: (O032410B) geda_utility_string_get_name_value\n");
    result++;
  }
  else if (strcmp(value, "B A")) {
    fprintf(stderr, "FAILED: (O032410C) geda_utility_string_get_name_value\n");
    result++;
  }

  return result;
}

int
check_attrib_thaw_hooks (GedaToplevel *toplevel)
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

  geda_attrib_freeze_hooks(object);

  geda_attrib_attach(object, attrib1, FALSE);
  geda_attrib_attach(object, attrib2, FALSE);

  /* === Function 25: geda_attrib_object_thaw_hooks  === */

  notify_attribute = 0;

  geda_attrib_thaw_hooks(object);

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O032501) geda_attrib_object_freeze_hooks <%d>\n", notify_attribute);
    result++;
  }

  geda_attrib_freeze_hooks(object);

  geda_attrib_detach_all (object);

  geda_attrib_freeze_hooks(object);
  geda_attrib_freeze_hooks(object);

  geda_attrib_attach_list(object, list, FALSE);
  g_list_free(list);

  notify_attribute = 0;

  geda_attrib_thaw_hooks(object);

  if (notify_attribute != 0) {
    fprintf(stderr, "FAILED: (O032502) geda_attrib_object_freeze_hooks <%d>\n", notify_attribute);
    result++;
  }

  geda_attrib_thaw_hooks(object);

  if (notify_attribute != 0) {
    fprintf(stderr, "FAILED: (O032503) geda_attrib_object_freeze_hooks <%d>\n", notify_attribute);
    result++;
  }

  geda_attrib_thaw_hooks(object);

  if (notify_attribute != 2) {
    fprintf(stderr, "FAILED: (O032504) geda_attrib_object_freeze_hooks <%d>\n", notify_attribute);
    result++;
  }

  geda_struct_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

/** @} endgroup test-attrib-object */

GedaToplevel *setup_new_toplevel(void)
{
  GedaToplevel *toplevel;
  Page         *page;

  toplevel = geda_toplevel_new ();
  page     = geda_struct_page_new (toplevel, "test_attrib_object.log");

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
    result = check_attrib_add(toplevel);
  }
  else {
    fprintf(stderr, "Caught signal checking geda_attrib_object_add\n\n");
    result++;
  }

  if (!result) {

    if (setjmp(point) == 0) {
      result = check_append_attrib_changed_hook(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_append_changed_hook\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_attach(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_attach\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_detach(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking check_attrib_detach\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_detach_all(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking check_attrib_detach_all\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_find_first_attrib_by_name(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_first_attrib_by_name\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_freeze_hooks(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_freeze_hooks\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_get_attrib_name_value(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_get_name_value\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_is_attached_to(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_is_attached_to\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_is_inherited(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_is_inherited\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_new_attached_attrib(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_new_attached\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_print(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_print\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_read(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_read\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_remove(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_remove\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_return_attribs(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_return_attribs\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_search_attached_by_name(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_search_attached_by_name\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_search_floating_by_name(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_search_floating_by_name\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_search_inherited_by_name(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_search_inherited_by_name\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_search_object_by_name(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_search_object_by_name\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_search_object_for_string(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_search_object_string\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_set_integer_value(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_set_integer_value\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_set_value(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_set_value\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_string_get_name_value(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_string_get_name_value\n\n");
      result++;
    }

    if (setjmp(point) == 0) {
      result += check_attrib_thaw_hooks(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_thaw_hooks\n\n");
      result++;
    }
  }
  else {
    fprintf(stderr, "discontinuing checks for src/object/o_attrib\n\n");
  }

  g_object_unref(toplevel);

  return result;
}
