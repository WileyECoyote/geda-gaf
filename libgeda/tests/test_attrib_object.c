/* -*- test_attrib_object.c -*-
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
 *  Date Contributed: March, 16th, 2016
 */

#include <libgeda.h>
#include <prototype_priv.h>
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
 *  Test Identifiers:  O  03  88, 88
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
 *              geda_attrib_object_get_name_value
 *              geda_attrib_object_is_attached_to
 *              geda_attrib_object_is_inherited
 *              geda_attrib_object_new_attached
 *              geda_attrib_object_print
 *              geda_attrib_object_remove
 *              geda_attrib_object_return_attribs
 *              geda_attrib_object_search_attached_by_name
 *              geda_attrib_object_search_floating_by_name
 *              geda_attrib_object_search_inherited_by_name
 *              geda_attrib_object_search_object_by_name
 *              geda_attrib_object_set_integer_value
 *              geda_attrib_object_set_value
 *              geda_attrib_object_string_get_name_value
 *              geda_attrib_object_thaw_hooks
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
check_add(GedaToplevel *toplevel)

{
  int result = 0;

  GList      *list;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  s_page_append_object(page, object);

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

  s_page_remove_object (page, object);
  g_object_unref (object);

  return result;
}

int
check_append_changed_hook(GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  s_page_append_object(page, object);

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

  s_page_remove_object (page, object);

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

  GedaObject *attrib  = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  s_page_append_object(page, object1);

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

  attrib = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");

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

  s_page_append_object(page, object2);

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

  s_page_remove_object (page, object2);
  s_page_remove_object (page, object1);

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

  GedaObject *attrib1 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");

  s_page_append_object(page, object);

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

  s_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_detach (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");

  s_page_append_object(page, object);
  s_page_append_object(page, attrib1);

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

  s_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_detach_all (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "B=b");
  GedaObject *attrib3 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "C=c");

  s_page_append_object(page, object);

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

  s_page_append_object(page, attrib1);          /* Add one attrib to the page */

  geda_attrib_attach_list(object, list, FALSE); /* reattach the attributes */

  notify_attribute = 0;

  geda_attrib_detach_all (object);

  if (notify_attribute != 4) {
    fprintf(stderr, "FAILED: (O030603) geda_attrib_object_detach_all\n");
    result++;
  }
  notify_attribute = 0;

  s_page_remove_object (page, object);
  g_list_free(list);
  g_object_unref (object);

  return result;
}

int
check_attrib_find_first_by_name (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");
  GedaObject *attrib2 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=b");
  GedaObject *attrib3 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=c");
  GedaObject *found;

  s_page_append_object(page, object);

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

  s_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

int
check_attrib_freeze_hooks (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = o_text_new(3, 0, 0, 0, 0, 10, 1, 1, "A=a");

  s_page_append_object(page, object);

  /* === Function 08: geda_attrib_object_freeze_hooks  === */

  geda_attrib_freeze_hooks(object);

  notify_attribute = 0;

  geda_attrib_attach(object, attrib1, FALSE);

  if (notify_attribute != 0) {
    fprintf(stderr, "FAILED: (O030801) geda_attrib_object_freeze_hooks\n");
    result++;
  }

  notify_attribute = 0;

  s_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

  /* === Function 11: geda_attrib_is_attached_to             geda_attrib_object_is_attached_to  === */
  /* === Function 12: geda_attrib_is_inherited               geda_attrib_object_is_inherited  === */
  /* === Function 13: geda_attrib_new_attached               geda_attrib_object_new_attached  === */
  /* === Function 14: geda_attrib_print                      geda_attrib_object_print  === */
  /* === Function 15: geda_attrib_remove                     geda_attrib_object_remove  === */
  /* === Function 16: geda_attrib_return_attribs             geda_attrib_object_return_attribs  === */
  /* === Function 17: geda_attrib_search_attached_by_name    geda_attrib_object_search_attached_by_name  === */
  /* === Function 18: geda_attrib_search_floating_by_name    geda_attrib_object_search_floating_by_name  === */
  /* === Function 19: geda_attrib_search_inherited_by_name   geda_attrib_object_search_inherited_by_name  === */
  /* === Function 20: geda_attrib_search_object_by_name      geda_attrib_object_search_object_by_name  === */
  /* === Function 21: geda_attrib_set_integer_value          geda_attrib_object_set_integer_value  === */
  /* === Function 22: geda_attrib_set_value                  geda_attrib_object_set_value  === */
  /* === Function 23: geda_attrib_string_get_name_value      geda_attrib_object_string_get_name_value  === */
  /* === Function 24: geda_attrib_thaw_hooks                 geda_attrib_object_thaw_hooks  === */

/** @} endgroup test-attrib-object */

GedaToplevel *setup_new_toplevel(void)
{
  GedaToplevel *toplevel;
  Page         *page;

  toplevel = geda_toplevel_new ();
  page     = s_page_new (toplevel, "test_attrib_object.log");

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
    result = check_add(toplevel);
  }
  else {
    fprintf(stderr, "Caught signal checking geda_attrib_object_add\n\n");
    result++;
  }

  if (!result) {

    if (setjmp(point) == 0) {
      result = check_append_changed_hook(toplevel);
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
      result += check_attrib_find_first_by_name(toplevel);
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

  }
  else {
    fprintf(stderr, "discontinuing checks for src/object/o_attrib\n\n");
  }

  g_object_unref(toplevel);

  return result;
}
