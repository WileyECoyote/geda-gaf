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
 *              geda_attrib_object_attach
 *              geda_attrib_object_attach_list
 *              geda_attrib_object_detach
 *              geda_attrib_object_detach_all
 *              geda_attrib_object_find_floating
 *              geda_attrib_object_find_attrib_by_name
 *              geda_attrib_object_first_attrib_by_name
 *              geda_attrib_object_freeze_hooks
 *              geda_attrib_object_get_attached
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
  notify_attribute = 0;

  if (GEDA_IS_TOPLEVEL(toplevel))
    notify_attribute++;

  if (GEDA_IS_OBJECT(object))
    notify_attribute++;
}

int
check_add(GedaToplevel *toplevel)

{
  int result = 0;

  Page       *page   = geda_toplevel_get_current_page(toplevel);

  GedaObject *object = geda_arc_object_new (3, 10, 20, 33, 0, 90);

  GedaObject *attrib = o_text_new(3, 0, 0, 0,0, 10, 1, 1, "A=a");

  s_page_append_object(page, object);

  /* === Function 01: geda_attrib_object_add === */

  /* The functionality of the change-notify feature is not check
   * here since geda_attrib_append_changed_hook if function #2 */

  geda_attrib_add(object, attrib);

  GList *list = object->attribs;

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

  GedaObject *attrib = o_text_new(3, 0, 0, 0,0, 10, 1, 1, "A=a");

  s_page_append_object(page, object);

  /* === Function 02: geda_attrib_object_append_changed_hook === */

  /* Note leaving connected */
  geda_attrib_append_changed_hook (page, (AttribsChangedFunc) test_attrib_object_notify,
                                         toplevel);
  geda_attrib_add(object, attrib);

  if (notify_attribute < 2) {
    fprintf(stderr, "FAILED: (O030201) geda_attrib_append_changed_hook\n");
    result++;
  }

  notify_attribute = 0;

  s_page_remove_object (page, object);

  g_object_unref (object);

  return result;
}

/* geda_attrib_attach                     geda_attrib_object_attach */
/* geda_attrib_attach_list                geda_attrib_object_attach_list */
/* geda_attrib_detach                     geda_attrib_object_detach */
/* geda_attrib_detach_all                 geda_attrib_object_detach_all */
/* geda_attrib_find_floating              geda_attrib_object_find_floating */
/* geda_attrib_find_attrib_by_name        geda_attrib_object_find_attrib_by_name */
/* geda_attrib_first_attrib_by_name       geda_attrib_object_first_attrib_by_name */
/* geda_attrib_freeze_hooks               geda_attrib_object_freeze_hooks */
/* geda_attrib_get_attached               geda_attrib_object_get_attached */
/* geda_attrib_get_name_value             geda_attrib_object_get_name_value */
/* geda_attrib_is_attached_to             geda_attrib_object_is_attached_to */
/* geda_attrib_is_inherited               geda_attrib_object_is_inherited */
/* geda_attrib_new_attached               geda_attrib_object_new_attached */
/* geda_attrib_print                      geda_attrib_object_print */
/* geda_attrib_remove                     geda_attrib_object_remove */
/* geda_attrib_return_attribs             geda_attrib_object_return_attribs */
/* geda_attrib_search_attached_by_name    geda_attrib_object_search_attached_by_name */
/* geda_attrib_search_floating_by_name    geda_attrib_object_search_floating_by_name */
/* geda_attrib_search_inherited_by_name   geda_attrib_object_search_inherited_by_name */
/* geda_attrib_search_object_by_name      geda_attrib_object_search_object_by_name */
/* geda_attrib_set_integer_value          geda_attrib_object_set_integer_value */
/* geda_attrib_set_value                  geda_attrib_object_set_value */
/* geda_attrib_string_get_name_value      geda_attrib_object_string_get_name_value */
/* geda_attrib_thaw_hooks                 geda_attrib_object_thaw_hooks */

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
  }

  if (!result) {

    if (setjmp(point) == 0) {
      result = check_append_changed_hook(toplevel);
    }
    else {
      fprintf(stderr, "Caught signal checking geda_attrib_object_append_changed_hook\n\n");
    }


  }
  else {
    fprintf(stderr, "discontinuing checks for src/object/o_attrib\n\n");
  }

  g_object_unref(toplevel);

  return result;
}
