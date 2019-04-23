/* -*- test_object.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2017 gEDA Contributors (see ChangeLog for details)
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
 *  Date Contributed: January, 30th, 2016
 */

#include "../../config.h"

#include <libgeda.h>
#include <geda_colors.h>

#define TOBJECT "GedaObject"

#define IMAGE_FILE "../docs/logo_256x101.png"
#define IMAGE_WIDTH  256
#define IMAGE_HEIGHT 101

/*! \file test_object.c
 *  \brief Tests for geda_object.c module
 *  \par
 *  This module provides basic unit tests for construction and destruction
 *  of the fundamental GedaObject class objects. The GedaObject class is the
 *  base class for:
 *
 *  <DL>
 *    <DT>GedaArc</DT>
 *    <DT>GedaBox</DT>
 *    <DT>GedaBus</DT>
 *    <DT>GedaCircle</DT>
 *    <DT>GedaComplex</DT>
 *    <DT>GedaLine</DT>
 *    <DT>GedaNet</DT>
 *    <DT>GedaPath</DT>
 *    <DT>GedaPicture</DT>
 *    <DT>GedaPin</DT>
 *    <DT>GedaText</DT>
 *  </DL>
 *
 *  Testing is limited to low-level checks because the objects themselves
 *  are fundamental objects that are implemented and manipulated with higher
 *  level procedural modules which assign realistic properties to the objects.
 *  Additionally, a test module exist for each of the above object types and
 *  the tests within those module includes some checks of the base class. One
 *  example of this is setting and retrieving the color index property.
 */

void weak_notify_func(void *obj, void *user_data)
{
  unsigned *notified = user_data;

  *notified += 1;
}

void weak_notify_func2(void *obj, void *user_data)
{
  unsigned *notified = user_data;

  *notified += 2;
}

void weak_notify_func3(void *obj, void *user_data)
{
  unsigned *notified = user_data;

  *notified += 3;
}

int check_object (void)
{
  int result = 0;

  GedaObject *object;
  unsigned    notified;

  /* -------------------------- GedaArc ------------------------- */

  object = geda_object_new (OBJ_ARC);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaArc is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_ARC(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  GedaArc *arc = (GedaArc*)object->arc;

  geda_object_add_weak_ptr (object, (void*)&arc);

  geda_object_weak_ref (object, weak_notify_func, &notified);

  geda_object_ref(object);

  notified = 0;

  geda_object_unref(object);

  if (notified) {
    fprintf(stderr, "Failed: %s notified GedaArc <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaArc <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  if (arc) {
    fprintf(stderr, "Failed: %s weak_ptr <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* -------------------------- GedaBox ------------------------- */

  object = geda_object_new (OBJ_BOX);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaBox is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_BOX(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  GedaBox *box = (GedaBox*)object->box;

  geda_object_add_weak_ptr (object, (void*)&box);

  geda_object_weak_ref (object, weak_notify_func, &notified);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaBox <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  if (box) {
    fprintf(stderr, "Failed: %s weak_ptr <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* -------------------------- GedaBus ------------------------- */

  object = geda_object_new (OBJ_BUS);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaBus is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_BUS(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  GedaBus *bus = (GedaBus*)object->bus;

  geda_object_add_weak_ptr (object, (void*)&bus);

  geda_object_weak_ref (object, weak_notify_func, &notified);

  geda_object_remove_weak_ptr (object, &bus);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaBus <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  if (!bus) {
    fprintf(stderr, "Failed: %s remove_weak_ptr <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* ------------------------- GedaCircle ----------------------- */

  object = geda_object_new (OBJ_CIRCLE);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaCircle is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_CIRCLE(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_weak_ref (object, weak_notify_func, &notified);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaCircle <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* ------------------------ GedaComplex ----------------------- */

  object = geda_object_new (OBJ_COMPLEX);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaComplex is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_COMPLEX(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_weak_ref (object, weak_notify_func, &notified);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaComplex <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* -------------------------- GedaLine ------------------------ */

  object = geda_object_new (OBJ_LINE);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaLine is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_LINE(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_weak_ref (object, weak_notify_func, &notified);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaLine <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* -------------------------- GedaNet ------------------------- */

  object = geda_object_new (OBJ_NET);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaNet is a %s <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_NET(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_weak_ref (object, weak_notify_func, &notified);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaNet <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* ------------------------- GedaPath ------------------------- */

  object = geda_object_new (OBJ_PATH);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaPath is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_PATH(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_weak_ref (object, weak_notify_func, &notified);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaPath <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* ------------------------ GedaPicture ----------------------- */

  object = geda_object_new (OBJ_PICTURE);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaPicture is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_PICTURE(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_weak_ref (object, weak_notify_func, &notified);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaPicture <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* -------------------------- GedaPin ------------------------- */

  object = geda_object_new (OBJ_PIN);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaPin is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_PIN(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_weak_ref (object, weak_notify_func, &notified);

  notified = 0;

  geda_object_unref(object);

  if (!notified) {
    fprintf(stderr, "Failed: %s to notify GedaPin <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* ------------------------- GedaText ------------------------- */

  object = geda_object_new (OBJ_TEXT);

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaText is %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else if (!GEDA_IS_TEXT(object)) {
    fprintf(stderr, "%s Failed <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_weak_ref (object, weak_notify_func, &notified);
  geda_object_weak_ref (object, weak_notify_func2, &notified);
  geda_object_weak_ref (object, weak_notify_func3, &notified);

  geda_object_weak_unref(object, weak_notify_func2, &notified);

  notified = 0;

  geda_object_unref(object);

  if (notified - 4) {
    fprintf(stderr, "Failed: %s to notify GedaText <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  return result;
}

int check_accessors ()
{
  int result = 0;

  GedaObject *object0, *object1, *object2;

  object1 = geda_complex_new();
  object2 = geda_text_new();

  /* === Function: geda_object_get_attached === */

  const GList *list;

  /* The list should be NULL because nothing has been attached to object1 */
  list = geda_object_get_attached(object1);

  if (list) {
    fprintf(stderr, "Failed: get_attached %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_get_attached_to === */

  /* Check that the Text object has no parent */
  object0 = geda_object_get_attached_to(object2);

  if (object0) {
    fprintf(stderr, "Failed: get_attached_to %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* Attach the Text object to the Complex */
  geda_attrib_object_attach (object1, object2, TRUE);

  /* === Function: geda_object_get_attached Reprise === */

  list = geda_object_get_attached(object1);

  if (!list) {
    fprintf(stderr, "Failed: get_attached %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    /* Check that object in the list is the attached Text object */
    if (list->data != object2) {
      fprintf(stderr, "Failed: get_attached %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }
  }

  /* === Function: geda_object_get_attached_to === */

  /* Check that the Text object parent is object1 */
  object0 = geda_object_get_attached_to(object2);

  if (!object0) {
    fprintf(stderr, "Failed: get_attached_to %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (object0 != object1) {
      fprintf(stderr, "Failed: get_attached_to %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }
  }

  /* === Function: geda_object_get_color === */

  int color;

  color = geda_object_get_color(object2);

  if (color != ATTRIBUTE_COLOR) {
    fprintf(stderr, "Failed: get_color %s line <%d> color <%d>\n", TOBJECT, __LINE__, color);
    result++;
  }

  /* === Function: geda_object_get_conn_list === */

  if (geda_object_get_conn_list(NULL)) {
    fprintf(stderr, "Failed: get_conn_list %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  if (geda_object_get_conn_list(object1)) {
    fprintf(stderr, "Failed: get_conn_list %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  object0 = geda_complex_new();

  list = g_list_append(NULL, object0);

  object1->conn_list = (GList*)list;

  if (!geda_object_get_conn_list(object1)) {
    fprintf(stderr, "Failed: get_conn_list %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    const GList *conn_list;

    conn_list = geda_object_get_conn_list(object1);

    if (conn_list != list) {
      fprintf(stderr, "Failed: get_conn_list %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }
  }

  g_list_free((GList*)list);
  object1->conn_list = NULL;

  /* === Function: geda_object_get_locked_color === */

  int locked_color;

  locked_color = geda_object_get_locked_color(object2);

  if (locked_color != LOCK_COLOR) {
    fprintf(stderr, "Failed: get_color %s line <%d> locked color=<%d>\n", TOBJECT, __LINE__, locked_color);
    result++;
  }

  /* === Function: geda_object_set_locked_color === */

  geda_object_set_locked_color(object2, color);

  locked_color = geda_object_get_locked_color(object2);

  if (locked_color != ATTRIBUTE_COLOR) {
    fprintf(stderr, "Failed: get_color %s line <%d> locked color <%d>\n", TOBJECT, __LINE__, locked_color);
    result++;
  }

  geda_object_set_locked_color(object2, -1);

  locked_color = geda_object_get_locked_color(object2);

  if (locked_color != ATTRIBUTE_COLOR) {
    fprintf(stderr, "Failed: get_color %s line <%d> locked color <%d>\n", TOBJECT, __LINE__, locked_color);
    result++;
  }

  GedaToplevel *toplevel;
  Page *page, *o_page;

  toplevel = geda_toplevel_new ();

  geda_toplevel_set_file_open_flags(toplevel, F_OPEN_RESTORE_CWD);
  geda_toplevel_set_make_backups(toplevel, 0);

  page = geda_struct_page_new (toplevel, NULL);

  /* === Function: geda_object_get_page === */

  if (geda_object_get_page(object0)) {
    fprintf(stderr, "Failed: get_page %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_set_page === */

  /* This is a low-level setter so the page is not updated here */
  geda_object_set_page(object0, page);

  o_page = geda_object_get_page(object0);

  if (o_page != page) {
    fprintf(stderr, "Failed: set_page %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_set_page(object0, NULL);

  if (geda_object_get_page(object0)) {
    fprintf(stderr, "Failed: set_page %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* This also destroys the page */
  g_object_unref(toplevel);

  /* === Function: geda_object_set_color === */

  /* low-level setter accepts any value */
  geda_object_set_color (object0, -1);

  color = geda_object_get_color(object0);

  if (color + 1) {
    fprintf(stderr, "Failed: set_color %s line <%d> color <%d>\n", TOBJECT, __LINE__, color);
    result++;
  }

  geda_object_set_color (object0, 82);

  color = geda_object_get_color(object0);

  if (color != 82) {
    fprintf(stderr, "Failed: set_color %s line <%d> color <%d>\n", TOBJECT, __LINE__, color);
    result++;
  }

  geda_object_set_color (object0, ATTRIBUTE_COLOR);

  /* === Function: geda_object_get_selectable === */

  if (!geda_object_get_selectable(object0)) {
    fprintf(stderr, "Failed: get_selectable %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_set_selectable === */

  geda_object_set_selectable(object0, FALSE);

  if (geda_object_get_selectable(object0)) {
    fprintf(stderr, "Failed: set_selectable %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* Reset selectable */
  geda_object_set_selectable(object0, TRUE);

  /* === Function: geda_object_get_bounds_valid === */

  if (geda_object_get_bounds_valid(object0)) {
    fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_set_bounds_valid === */

  geda_object_set_bounds_valid(object0, TRUE);

  if (!geda_object_get_bounds_valid(object0)) {
    fprintf(stderr, "Failed: set_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_get_visibility === */

  /* Objects are visible by default */
  if (!geda_object_get_visibility(object0)) {
    fprintf(stderr, "Failed: get_visibility %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_set_visibility === */

  geda_object_set_visibility(object0, INVISIBLE);

  if (geda_object_get_visibility(object0)) {
    fprintf(stderr, "Failed: set_visibility %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_get_show_name_value === */

  /* SHOW_NAME_VALUE, or 0, is the default */
  if (geda_object_get_show_name_value(object0)) {
    fprintf(stderr, "Failed: get_show_name_value %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_set_show_name_value === */

  geda_object_set_show_name_value(object0, SHOW_VALUE);

  if (geda_object_get_show_name_value(object0) != SHOW_VALUE) {
    fprintf(stderr, "Failed: set_show_name_value %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_set_show_name_value(object0, SHOW_NAME);

  if (geda_object_get_show_name_value(object0) != SHOW_NAME) {
    fprintf(stderr, "Failed: set_show_name_value %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_set_show_name_value(object0, -1);

  if (geda_object_get_show_name_value(object0) != SHOW_NAME) {
    fprintf(stderr, "Failed: set_show_name_value %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_unref(object1);
  geda_object_unref(object2);

  return result;
}

int check_methods (void)
{
  int result = 0;

  GedaObject *object;

  /* === Function: geda_object_bounds === */

  /* ------------------------- GedaArc -------------------------- */

  object = geda_arc_object_new (3, 1000, 1000, 100, 0, 90);

  if (!geda_object_bounds(object)) {
    fprintf(stderr, "Failed: arc object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (!geda_object_get_bounds_valid(object)) {
      fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }

    if (object->left != 1000) {
      fprintf(stderr, "Failed: arc object_bounds %s; left <%d>\n", TOBJECT, object->left);
      result++;
    }

    if (object->right != 1100) {
      fprintf(stderr, "Failed: arc object_bounds %s; right <%d>\n", TOBJECT, object->right);
      result++;
    }

    if (object->top != 1000) {
      fprintf(stderr, "Failed: arc object_bounds %s; top <%d>\n", TOBJECT, object->top);
      result++;
    }

    if (object->bottom != 1100) {
      fprintf(stderr, "Failed: arc object_bounds %s; bottom <%d>\n", TOBJECT, object->bottom);
      result++;
    }
  }
  geda_object_unref(object);

  /* ------------------------- GedaBox -------------------------- */

  object = geda_box_object_new (3, 1000, 1000, 100, 100);

  if (!geda_object_bounds(object)) {
    fprintf(stderr, "Failed: box object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (!geda_object_get_bounds_valid(object)) {
      fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }

    if (object->left != 100) {
      fprintf(stderr, "Failed: box object_bounds %s; left <%d>\n", TOBJECT, object->left);
      result++;
    }

    if (object->right != 1000) {
      fprintf(stderr, "Failed: box object_bounds %s; right <%d>\n", TOBJECT, object->right);
      result++;
    }

    if (object->top != 100) {
      fprintf(stderr, "Failed: box object_bounds %s; top <%d>\n", TOBJECT, object->top);
      result++;
    }

    if (object->bottom != 1000) {
      fprintf(stderr, "Failed: box object_bounds %s; bottom <%d>\n", TOBJECT, object->bottom);
      result++;
    }
  }
  geda_object_unref(object);

  /* ------------------------- GedaBus -------------------------- */

  object = geda_bus_object_new (3, 100, 100, 200, 200, 0);

  if (!geda_object_bounds(object)) {
    fprintf(stderr, "Failed: bus object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (!geda_object_get_bounds_valid(object)) {
      fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }

    if (object->left != 100) {
      fprintf(stderr, "Failed: bus object_bounds %s; left <%d>\n", TOBJECT, object->left);
      result++;
    }

    if (object->right != 200) {
      fprintf(stderr, "Failed: bus object_bounds %s; right <%d>\n", TOBJECT, object->right);
      result++;
    }

    if (object->top != 100) {
      fprintf(stderr, "Failed: bus object_bounds %s; top <%d>\n", TOBJECT, object->top);
      result++;
    }

    if (object->bottom != 200) {
      fprintf(stderr, "Failed: bus object_bounds %s; bottom <%d>\n", TOBJECT, object->bottom);
      result++;
    }
  }
  geda_object_unref(object);

  /* ------------------------- GedaCircle ----------------------- */

  object = geda_circle_object_new (3, 1000, 1000, 500);

  if (!geda_object_bounds(object)) {
    fprintf(stderr, "Failed: circle object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (!geda_object_get_bounds_valid(object)) {
      fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }

    if (object->left != 500) {
      fprintf(stderr, "Failed: circle object_bounds %s; left <%d>\n", TOBJECT, object->left);
      result++;
    }

    if (object->right != 1500) {
      fprintf(stderr, "Failed: circle object_bounds %s; right <%d>\n", TOBJECT, object->right);
      result++;
    }

    if (object->top != 500) {
      fprintf(stderr, "Failed: circle object_bounds %s; top <%d>\n", TOBJECT, object->top);
      result++;
    }

    if (object->bottom != 1500) {
      fprintf(stderr, "Failed: circle object_bounds %s; bottom <%d>\n", TOBJECT, object->bottom);
      result++;
    }
  }
  geda_object_unref(object);

  /* ------------------------- GedaComplex ---------------------- */

  object = geda_complex_new();

  GList *prim_objs;

  prim_objs = g_list_append(NULL, geda_box_object_new (3, 1000, 1000, 100, 100));
  prim_objs = g_list_append(prim_objs, geda_circle_object_new (3, 1000, 1000, 500));

  geda_complex_set_prim_objs (object->complex, prim_objs);

  if (!geda_object_bounds(object)) {
    fprintf(stderr, "Failed: complex object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (!geda_object_get_bounds_valid(object)) {
      fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }

    if (object->left != 100) {
      fprintf(stderr, "Failed: complex object_bounds %s; left <%d>\n", TOBJECT, object->left);
      result++;
    }

    if (object->right != 1500) {
      fprintf(stderr, "Failed: complex object_bounds %s; right <%d>\n", TOBJECT, object->right);
      result++;
    }

    if (object->top != 100) {
      fprintf(stderr, "Failed: complex object_bounds %s; top <%d>\n", TOBJECT, object->top);
      result++;
    }

    if (object->bottom != 1500) {
      fprintf(stderr, "Failed: complex object_bounds %s; bottom <%d>\n", TOBJECT, object->bottom);
      result++;
    }
  }
  geda_object_unref(object);

  /* ------------------------- GedaLine ------------------------- */

  object = geda_line_object_new (3, 100, 100, 300, 300);

  if (!geda_object_bounds(object)) {
    fprintf(stderr, "Failed: line object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (!geda_object_get_bounds_valid(object)) {
      fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }

    if (object->left != 100) {
      fprintf(stderr, "Failed: line object_bounds %s; left <%d>\n", TOBJECT, object->left);
      result++;
    }

    if (object->right != 300) {
      fprintf(stderr, "Failed: line object_bounds %s; right <%d>\n", TOBJECT, object->right);
      result++;
    }

    if (object->top != 100) {
      fprintf(stderr, "Failed: line object_bounds %s; top <%d>\n", TOBJECT, object->top);
      result++;
    }

    if (object->bottom != 300) {
      fprintf(stderr, "Failed: line object_bounds %s; bottom <%d>\n", TOBJECT, object->bottom);
      result++;
    }
  }
  geda_object_unref(object);

  /* ------------------------- GedaNet -------------------------- */

  object = geda_net_object_new (3, 200, 250, 300, 350);

  if (!geda_object_bounds(object)) {
    fprintf(stderr, "Failed: line object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (!geda_object_get_bounds_valid(object)) {
      fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }

    if (object->left != 200) {
      fprintf(stderr, "Failed: net object_bounds %s; left <%d>\n", TOBJECT, object->left);
      result++;
    }

    if (object->right != 300) {
      fprintf(stderr, "Failed: net object_bounds %s; right <%d>\n", TOBJECT, object->right);
      result++;
    }

    if (object->top != 250) {
      fprintf(stderr, "Failed: net object_bounds %s; top <%d>\n", TOBJECT, object->top);
      result++;
    }

    if (object->bottom != 350) {
      fprintf(stderr, "Failed: net object_bounds %s; bottom <%d>\n", TOBJECT, object->bottom);
      result++;
    }
  }
  geda_object_unref(object);

  /* ------------------------- GedaPicture ---------------------- */

  object = geda_picture_object_new(NULL, 0, IMAGE_FILE, 100, 200,
                                                        100 + IMAGE_WIDTH,
                                                        100 - IMAGE_HEIGHT,
                                                        0, FALSE, FALSE);

  if (!geda_object_bounds(object)) {
    fprintf(stderr, "Failed: picture object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }
  else {

    if (!geda_object_get_bounds_valid(object)) {
      fprintf(stderr, "Failed: get_bounds_valid %s line <%d>\n", TOBJECT, __LINE__);
      result++;
    }

    if (object->left != 100) {
      fprintf(stderr, "Failed: picture object_bounds %s; left <%d>\n", TOBJECT, object->left);
      result++;
    }

    if (object->right != 100 + IMAGE_WIDTH) {
      fprintf(stderr, "Failed: picture object_bounds %s; right <%d>\n", TOBJECT, object->right);
      result++;
    }

    if (object->top != 100 - IMAGE_HEIGHT) {
      fprintf(stderr, "Failed: picture object_bounds %s; top <%d>\n", TOBJECT, object->top);
      result++;
    }

    if (object->bottom != 200) {
      fprintf(stderr, "Failed: picture object_bounds %s; bottom <%d>\n", TOBJECT, object->bottom);
      result++;
    }
  }
  geda_object_unref(object);

  /* ------------------------- GedaText ------------------------- */

  object = geda_text_object_new(9, 100, 100, 1, 0, 10, 1, 0, "Tests");

  /* No renderer has been set so bounds cannot be determined */
  if (geda_object_bounds(object)) {
    fprintf(stderr, "Failed: line object_bounds %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  geda_object_unref(object);

  return result;
}

int main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = check_object();

  result += check_accessors();

  result += check_methods();

  if (result) {
    fprintf(stderr, "Check module geda_object.c");
  }

  return (result > 0);
}
