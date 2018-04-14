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

#include <libgeda.h>
#include <geda_colors.h>

#define TOBJECT "GedaObject"

/*! \file test_object.c
 *  \brief Tests for geda_object.c module
 *  \par
 *  This module provides basic unit tests for construction and destruction
 *  of GedaObjects.
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

  object = geda_arc_new();

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

  object = geda_box_new();

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

  object = geda_bus_new();

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

  object = geda_circle_new();

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

  object = geda_complex_new();

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

  object = geda_line_new();

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

  object = geda_net_new();

  if (!GEDA_IS_OBJECT(object)) {
    fprintf(stderr, "Failed: GedaNet is %s line <%d>\n", TOBJECT, __LINE__);
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

  object = geda_path_new();

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

  object = geda_picture_new();

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

  object = geda_pin_new();

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

  object = geda_text_new();

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

int
check_accessors ()
{
  int result = 0;

  GedaObject *object0, *object1, *object2;

  object1 = geda_complex_new();
  object2 = geda_text_new();

  /* === Function: geda_object_get_attached === */

  const GList *list;

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
    fprintf(stderr, "Failed: get_color %s line <%d>\n", TOBJECT, __LINE__);
    result++;
  }

  /* === Function: geda_object_get_conn_list === */
  /* === Function: geda_object_get_page === */

  /* === Function: geda_object_set_color === */
  /* === Function: geda_object_set_selectable === */

  geda_object_unref(object1);
  geda_object_unref(object2);

  return result;
}

int
check_properties (void)
{
  int result = 0;

  /* -------------------------- GedaArc ------------------------- */

    /* === Function: geda_object_bounds === */

  return result;
}

int
main (int argc, char *argv[])
{
  int result = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  result  = check_object();

  result += check_properties();

  result += check_accessors();

  if (result) {
    fprintf(stderr, "Check module geda_object.c");
  }

  return (result > 0);
}
