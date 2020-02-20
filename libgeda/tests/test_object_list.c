/* -*- test_object_list.c -*-
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
 *  Date Contributed: October, 6th, 2016
 */

#include "../../config.h"

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
 *      O1204    geda_object_list_mirror
 *      O1205    geda_object_list_rotate
 *       O1206   geda_object_list_scale
 *       O1207   geda_object_list_set_color
 *       O1208   geda_object_list_translate
 */

int notify_attribute;

/*!
 * \brief Test geda_object_list_copy_all
 * \par Methodology
 * A. Create 7 objects:
 *    object 0 is an arc, aka non-text object.
 *    objects 1-6 are text attribute objects; attribx.
 *
 * B. Attach 2 attributes, 5 & 6, directly to Arc object 0 and append
 *    Arc object 0 to a page object. The first is attached with the set
 *    color flag, the second is not.
 *
 * C. Attached 3 of the remaining 4 text attributes, and one of the
 *    floating attributes attached to the arc, attrib6, to the page.
 *    Set one of the 3, attrib3, and child of the arc, attrib6, "selected".
 *
 * D. Create a new list and add the 1 remaining text attribute, attrib4,
 *    that is not on the page nor attached to any object, to the list.
 *
 * E. Use geda_object_list_copy_all to append the list of objects on the
 *    page to the new list containing only member, attrib4.
 *
 * Expected results:
 *
 * R1: There should be 6 members in the resulting list, attrib4, a copy
 *     of the arc object, a copy of attrib1, attrib2, and attrib3, and
 *     a copy of attrib6. Only the count is checked here.
 *
 * R2: The "selected" state of the orginal objects should not be changed;
 *     The arc and attributes attrib1 and attrib2 should not be selected
 *     and attrib3 and attrib6 should still be selected.
 *
 * R3: attrib4 should still be the first member of the resulting list
 *     and should not be set selected.
 *
 * R4: The 2 attributes, attrib5 and attrib6, attached to the arc should
 *     still be attached. The color property of attrib5 and attrib6 should
 *     not be changed.
 *
 * R5: The remaining 5 members of the list should be "copies" of the
 *     objects added to the page, starting with the arc, and none of
 *     the items should be set selected.
 *
 * R6: The new arc should have one attachment, a copy of attrib6.
 */
int
check_list_copy_all (GedaToplevel *toplevel)
{
  int result = 0;

  Page       *page    = geda_toplevel_get_current_page(toplevel);

  GedaObject *object  = geda_arc_object_new (0, 10, 20, 33, 0, 90);

  GedaObject *attrib1 = geda_text_object_new(1, 0, 0, 0, 0, 10, 1, 1, "A=1");
  GedaObject *attrib2 = geda_text_object_new(2, 0, 0, 0, 0, 10, 1, 1, "B=2");
  GedaObject *attrib3 = geda_text_object_new(3, 0, 0, 0, 0, 10, 1, 1, "C=3");
  GedaObject *attrib4 = geda_text_object_new(4, 0, 0, 0, 0, 10, 1, 1, "D=4");
  GedaObject *attrib5 = geda_text_object_new(5, 0, 0, 0, 0, 10, 1, 1, "E=5");
  GedaObject *attrib6 = geda_text_object_new(6, 0, 0, 0, 0, 10, 1, 1, "F=6");

  geda_attrib_object_attach (object, attrib5, TRUE);
  geda_attrib_object_attach (object, attrib6, FALSE);

  geda_struct_page_append_object(page, object);

  GList *list;

  list = g_list_append(NULL, attrib1);
  list = g_list_append(list, attrib2);
  list = g_list_append(list, attrib3);
  list = g_list_append(list, attrib6);

  geda_struct_page_append_list(page, list);

  geda_set_object_selected(attrib3);
  geda_set_object_selected(attrib6);

  /* start a new list */
  list = g_list_append(NULL, attrib4);

  /* === Function 01: geda_object_list_copy_all  === */

  list = geda_copy_list(geda_struct_page_get_objects(page), list);

  /* R1: Check that the list contains the correct number of items */
  int cnt = g_list_length(list);

  if (cnt != 6) {
    fprintf(stderr, "FAILED: (O120101) object_list_copy_all <%d>\n", cnt);
    result++;
  }

  /* R2A: Check if original objects have been set selected */

  if (geda_object_get_is_selected(object)) {
    fprintf(stderr, "FAILED: (O120102A) <%s> should not be selected\n", object->name);
    result++;
  }

  if (geda_object_get_is_selected(attrib1)) {
    fprintf(stderr, "FAILED: (O120102B) <%s> should not be selected\n", attrib1->name);
    result++;
  }

  if (geda_object_get_is_selected(attrib2)) {
    fprintf(stderr, "FAILED: (O120102C) <%s> should not be selected\n", attrib2->name);
    result++;
  }

  /* R2B: Check if attrib3 or attrib6 has been unselected */

  if (!geda_object_get_is_selected(attrib3)) {
    fprintf(stderr, "FAILED: (O120102D) <%s> should be selected\n", attrib3->name);
    result++;
  }

  if (!geda_object_get_is_selected(attrib6)) {
    fprintf(stderr, "FAILED: (O120102E) <%s> should be selected\n", attrib3->name);
    result++;
  }

  /* R3: Check list, the first object was already in the list */

  GedaObject *obj = g_list_nth_data (list, 0);

  if (obj != attrib4) {
    fprintf(stderr, "FAILED: (O120103A) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120103B) object_list_copy_all <%d>\n", obj->selected);
    result++;
  }

  const GList *attached;

  /* R4: The original arc object should still have 2 attachments */

  attached = geda_object_get_attached(object);

  cnt = geda_glist_length(attached);

  if (cnt != 2) {
    fprintf(stderr, "FAILED: (O120104A) object_list_copy_all <%d>\n", cnt);
    result++;
  }
  else {

    GedaObject *bute;

    bute = attached->data;

    if (bute != attrib5) {
      fprintf(stderr, "FAILED: (O120104B1) object_list_copy_all <%p>\n", bute);
      result++;
    }

    if (bute->color != attrib5->color) {
      fprintf(stderr, "FAILED: (O120104B2) object_list_copy_all <%d>\n", bute->color);
      result++;
    }

    attached = attached->next;
    bute     = attached->data;

    if (bute != attrib6) {
      fprintf(stderr, "FAILED: (O120104C1) object_list_copy_all <%p>\n", bute);
      result++;
    }

    if (bute->color != attrib6->color) {
      fprintf(stderr, "FAILED: (O120104C2) object_list_copy_all <%d>\n", bute->color);
      result++;
    }
  }

  /* R5: Remaining members of list are copies of the originals starting
   *     with the arc object */
  obj = g_list_nth_data (list, 1);

  if (!GEDA_IS_ARC(obj)) {
    fprintf(stderr, "FAILED: (O120105A1) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else {

    if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120105A2) object_list_copy_all <%p>\n", obj);
    result++;
    }

    /* Defer checking attachment to copied arc */
    attached = geda_object_get_attached(obj);
  }

  obj = g_list_nth_data (list, 2); /* Copy of attrib1 */

  if (!GEDA_IS_TEXT(obj)) {
    fprintf(stderr, "FAILED: (O120105B1) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120105B2) object_list_copy_all <%d>\n", obj->selected);
    result++;
  }
  else if (obj->text->string[0] != 'A') {
    fprintf(stderr, "FAILED: (O120105B3) object_list_copy_all <%c>\n", obj->text->string[0]);
    result++;
  }

  obj = g_list_nth_data (list, 3); /* Copy of attrib2 */

  if (!GEDA_IS_TEXT(obj)) {
    fprintf(stderr, "FAILED: (O120105C1) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120105C2) object_list_copy_all <%d>\n", obj->selected);
    result++;
  }
  else if (obj->text->string[0] != 'B') {
    fprintf(stderr, "FAILED: (O120105C3) object_list_copy_all <%c>\n", obj->text->string[0]);
    result++;
  }

  obj = g_list_nth_data (list, 4); /* Copy of attrib3 */

  if (!GEDA_IS_TEXT(obj)) {
    fprintf(stderr, "FAILED: (O120105D1) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120105D2) object_list_copy_all <%d>\n", obj->selected);
    result++;
  }
  else if (obj->text->string[0] != 'C') {
    fprintf(stderr, "FAILED: (O120105C3) object_list_copy_all <%c>\n", obj->text->string[0]);
    result++;
  }

  obj = g_list_nth_data (list, 5); /* Copy of attrib6 */

  if (!GEDA_IS_TEXT(obj)) {
    fprintf(stderr, "FAILED: (O120105E1) object_list_copy_all <%p>\n", obj);
    result++;
  }
  else if (geda_object_get_is_selected(obj)) {
    fprintf(stderr, "FAILED: (O120105E2) object_list_copy_all <%d>\n", obj->selected);
    result++;
  }
  else {

    GedaObject *bute;

    if (obj->text->string[0] != 'F') {
      fprintf(stderr, "FAILED: (O120105C3) object_list_copy_all <%c>\n", obj->text->string[0]);
      result++;
    }

    /* R6A: Check that there is only one attachment to the new arc */
    cnt = geda_glist_length(attached);

    if (cnt - 1) {
      fprintf(stderr, "FAILED: (O120106A) object_list_copy_all <%d>\n", cnt);
      result++;
    }
    else {

      /* Recall the attribute attached to the new arc */
      bute = attached->data;

      /* R6B: Attached attribute and last member of the list should be one in the same */
      if (bute != obj) {
        fprintf(stderr, "FAILED: (O120106B) object_list_copy_all <%p>\n", bute);
        result++;
      }
    }
  }

  g_object_unref (attrib4);
  g_object_unref (attrib5);

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

static const char path_string[] = "M1900,13100,L 8400,16100,z";

int
check_object_list_mirror (GedaToplevel *toplevel)
{
  int result = 0;

  int x1 = 100;
  int y1 = 100;
  int x2 = 200;
  int y2 = 200;
  int r  = 100;

  GedaObject *object1  = geda_arc_object_new (1, x1, y1, r, 0, 90);
  GedaObject *object2  = geda_box_object_new(2, x1, y1, x2, y2);
  GedaObject *object3  = geda_bus_object_new(3, x1, y1, x2, y2, 0);
  GedaObject *object4  = geda_circle_object_new(4, x1, y1, r);
  GedaObject *object5  = geda_complex_new();
  GedaObject *object6  = geda_line_object_new(6, x1, y1, x2, y2);
  GedaObject *object7  = geda_net_object_new (7, x1, y1, x2, y2);
  GedaObject *object8  = geda_path_object_new(8, &path_string[0]);
  GedaObject *object9  = geda_picture_new();
  GedaObject *object10 = geda_pin_object_new(10, x1, y1, x2, y2, 0, 0);
  GedaObject *object11 = geda_text_object_new(11, x1, y1, 0, 0, 10, 1, 0, "tests");

  geda_picture_object_modify_all (object9, x1, y1, x2, y2);

  GList *list;

  list = g_list_append(NULL, object1);
  list = g_list_append(list, object2);
  list = g_list_append(list, object3);
  list = g_list_append(list, object4);
  list = g_list_append(list, object5);
  list = g_list_append(list, object6);
  list = g_list_append(list, object7);
  list = g_list_append(list, object8);
  list = g_list_append(list, object9);
  list = g_list_append(list, object10);
  list = g_list_append(list, object11);

  /* === Function 04: geda_object_list_mirror  === */
  geda_object_list_mirror(list, x1, y1);

  if (!geda_complex_get_is_mirror(object5->complex)) {
    fprintf(stderr, "FAILED: (O120405) geda_object_list_mirror\n");
    result++;
  }

  if (!object9->picture->mirrored) {
    fprintf(stderr, "FAILED: (O120409) geda_object_list_mirror\n");
    result++;
  }

  if (object11->text->alignment != 6) {
    int a = object11->text->alignment;
    fprintf(stderr, "FAILED: (O120411) geda_object_list_mirror %d\n", a);
    result++;
  }

  g_object_unref (object1);
  g_object_unref (object2);
  g_object_unref (object3);
  g_object_unref (object4);
  g_object_unref (object5);
  g_object_unref (object6);
  g_object_unref (object7);
  g_object_unref (object8);
  g_object_unref (object9);
  g_object_unref (object10);
  g_object_unref (object11);

  return result;
}

int
check_object_list_rotate (GedaToplevel *toplevel)
{
  int result = 0;

  int x1 = 100;
  int y1 = 100;
  int x2 = 200;
  int y2 = 200;
  int r  = 100;

  GedaObject *object1  = geda_arc_object_new (1, x1, y1, r, 0, 90);
  GedaObject *object2  = geda_box_object_new(2, x1, y1, x2, y2);
  GedaObject *object3  = geda_bus_object_new(3, x1, y1, x2, y2, 0);
  GedaObject *object4  = geda_circle_object_new(4, x1, y1, r);
  GedaObject *object5  = geda_complex_new();
  GedaObject *object6  = geda_line_object_new(6, x1, y1, x2, y2);
  GedaObject *object7  = geda_net_object_new (7, x1, y1, x2, y2);
  GedaObject *object8  = geda_path_object_new(8, &path_string[0]);
  GedaObject *object9  = geda_picture_new();
  GedaObject *object10 = geda_pin_object_new(10, x1, y1, x2, y2, 0, 0);
  GedaObject *object11 = geda_text_object_new(11, x1, y1, 0, 0, 10, 1, 0, "tests");

  geda_complex_set_angle (object5->complex, 0);
  geda_complex_set_x (object5->complex, x1);
  geda_complex_set_y (object5->complex, y1);

  geda_picture_object_modify_all (object9, x1, y1, x2, y2);

  GList *list;

  list = g_list_append(NULL, object1);
  list = g_list_append(list, object2);
  list = g_list_append(list, object3);
  list = g_list_append(list, object4);
  list = g_list_append(list, object5);
  list = g_list_append(list, object6);
  list = g_list_append(list, object7);
  list = g_list_append(list, object8);
  list = g_list_append(list, object9);
  list = g_list_append(list, object10);
  list = g_list_append(list, object11);

  /* === Function 05: geda_object_list_rotate  === */
  geda_object_list_rotate(list, x1, y1, 90);

  int angle;
  int x3, x4;
  int y3, y4;

  /* === object1->arc === */

  angle = geda_arc_get_start_angle (object1->arc);

  if (angle != 90) {
    fprintf(stderr, "FAILED: (O120501) geda_object_list_rotate (%d)\n", angle);
    result++;
  }

  /* === object2->box === */

  g_object_get(object2->box, "upper-x", &x3, NULL);

  if (x3) {
    fprintf(stderr, "FAILED: (O120502A) geda_object_list_rotate <%d>\n", x3);
    result++;
  }

  /* === object3->bus === */

  g_object_get(object3->bus, "first-x",  &x3, NULL);


  if (x1 - x3) {
    fprintf(stderr, "FAILED: (O120503) geda_object_list_rotate <%d>\n", x3);
    result++;
  }

  /* === object5->complex === */

  angle = geda_complex_get_angle(object5->complex);

  if (angle != 90) {
    fprintf(stderr, "FAILED: (O120505) geda_object_list_rotate (%d)\n", angle);
    result++;
  }

  /* === object6->line === */

  g_object_get(object6->line, "second-x", &x4, NULL);


  if (x4) {
    fprintf(stderr, "FAILED: (O120506) geda_object_list_rotate <%d>\n", x4);
    result++;
  }

  /* === object7->net === */

  g_object_get(object7->net, "second-y", &y4, NULL);

  /* Both Y's should be 200 */
  if (y2 - y4) {
    fprintf(stderr, "FAILED: (O120507) geda_object_list_rotate <%d>\n", y4);
    result++;
  }

  /* === object8->path === */

  char *spath;

  /* M -12900,1900nL -15900,8400\nz>*/

  spath = geda_struct_path_string_from_path (object8->path);

  if (strncmp(spath, "M -12900", 8)) {
    fprintf(stderr, "FAILED: (O120508) geda_object_list_rotate <%s>\n", spath);
    result++;
  }

  free(spath);

  /* === object9->picture === */

  angle = geda_picture_get_angle(object9->picture);

  if (angle != 90) {
    fprintf(stderr, "FAILED: (O120509) geda_object_list_rotate (%d)\n", angle);
    result++;
  }

  /* === object10->pin === */

  g_object_get(object3->bus, "first-y",  &y3, NULL);

  if (y1 - y3) {
    fprintf(stderr, "FAILED: (O120510) geda_object_list_rotate <%d>\n", y3);
    result++;
  }

  /* === object11->text === */
  angle = geda_text_get_angle(object11->text);

  if (angle != 90) {
    fprintf(stderr, "FAILED: (O120511) geda_object_list_rotate (%d)\n", angle);
    result++;
  }

  /* check geda_object_list_rotate with a negative angle */
  geda_object_list_rotate(list, x1, y1, -90);

  /* === object1->arc === */

  angle = geda_arc_get_start_angle (object1->arc);

  if (angle != 0) {
    fprintf(stderr, "FAILED: (O120512) geda_object_list_rotate (%d)\n", angle);
    result++;
  }

  /* === object5->complex === */

  angle = geda_complex_get_angle(object5->complex);

  if (angle != 0) {
    fprintf(stderr, "FAILED: (O120516) geda_object_list_rotate (%d)\n", angle);
    result++;
  }

  /* === object6->line === */

  g_object_get(object6->line, "second-x", &x4, NULL);

  /* Second X should be at original X2=200 */
  if (x2 - x4) {
    fprintf(stderr, "FAILED: (O120517) geda_object_list_rotate <%d>\n", x4);
    result++;
  }

  /* === object7->net === */

  g_object_get(object7->net, "second-y", &y4, NULL);

  /* Second Y should be at original Y2=200 */
  if (y2 - y4) {
    fprintf(stderr, "FAILED: (O120518) geda_object_list_rotate <%d>\n", y4);
    result++;
  }

  g_object_unref (object1);
  g_object_unref (object2);
  g_object_unref (object3);
  g_object_unref (object4);
  g_object_unref (object5);
  g_object_unref (object6);
  g_object_unref (object7);
  g_object_unref (object8);
  g_object_unref (object9);
  g_object_unref (object10);
  g_object_unref (object11);

  return result;
}

int
check_object_list_scale (GedaToplevel *toplevel)
{
  int result = 0;

  int x1 = 100;
  int y1 = 100;
  int x2 = 200;
  int y2 = 200;
  int r  = 100;

  GedaObject *object1  = geda_arc_object_new (1, x1, y1, r, 0, 90);
  GedaObject *object2  = geda_box_object_new(2, x2, y2, x1, y1);
  GedaObject *object3  = geda_bus_object_new(3, x1, y1, x2, y2, 0);
  GedaObject *object4  = geda_circle_object_new(4, x1, y1, r);
  GedaObject *object5  = geda_complex_new();
  GedaObject *object6  = geda_line_object_new(6, x1, y1, x2, y2);
  GedaObject *object7  = geda_net_object_new (7, x1, y1, x2, y2);
  GedaObject *object8  = geda_path_object_new(8, &path_string[0]);
  GedaObject *object9  = geda_picture_new();
  GedaObject *object10 = geda_pin_object_new(10, x1, y1, x2, y2, 0, 0);
  GedaObject *object11 = geda_text_object_new(11, x1, y1, 0, 0, 10, 1, 0, "tests");

  geda_complex_set_angle (object5->complex, 0);
  geda_complex_set_x (object5->complex, x1);
  geda_complex_set_y (object5->complex, y1);

  geda_picture_object_modify_all (object9, x1, y1, x2, y2);

  GList *list;

  list = g_list_append(NULL, object1);
  list = g_list_append(list, object2);
  list = g_list_append(list, object3);
  list = g_list_append(list, object4);
  list = g_list_append(list, object5);
  list = g_list_append(list, object6);
  list = g_list_append(list, object7);
  list = g_list_append(list, object8);
  list = g_list_append(list, object9);
  list = g_list_append(list, object10);
  list = g_list_append(list, object11);

  /* === Function 06: geda_object_list_scale  === */

  geda_object_list_scale(list, 10, 10);

  /* === object1->arc === */

  int rad = geda_arc_get_radius (object1->arc);

  if (rad != r * 10) {
    fprintf(stderr, "FAILED: (O120601) geda_object_list_scale (%d)\n", rad);
    result++;
  }

  /* === object2->box === */

  int sx1 = geda_box_object_get_lower_x(object2);
  int sy1 = geda_box_object_get_lower_y(object2);
  int sx2 = geda_box_object_get_upper_x(object2);
  int sy2 = geda_box_object_get_upper_y(object2);

  if (sx1 + 350 || sx2 - 650) {
    fprintf(stderr, "FAILED: (O120602X) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  if (sy1 + 350 || sy2 - 650) {
    fprintf(stderr, "FAILED: (O120602Y) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  /* === object3->bus === */

  sx1 = geda_bus_object_get_x1(object3);
  sy1 = geda_bus_object_get_y1(object3);
  sx2 = geda_bus_object_get_x2(object3);
  sy2 = geda_bus_object_get_y2(object3);

  if (sx1 - 1000 || sx2 - 2000) {
    fprintf(stderr, "FAILED: (O120603X) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  if (sy1 - 1000 || sy2 - 2000) {
    fprintf(stderr, "FAILED: (O120603Y) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  /* === object4->circle  === */

  rad = geda_circle_object_get_radius(object4);

  if (rad != r * 10) {
    fprintf(stderr, "FAILED: (O120604) geda_object_list_scale (%d)\n", rad);
    result++;
  }

  /* === object5->complex  === */

  /* geda_object_list_scale ignores complexes, so it's a no tests. */

  /* === object6->line  === */

  sx1 = geda_line_object_get_x1(object6);
  sy1 = geda_line_object_get_y1(object6);
  sx2 = geda_line_object_get_x2(object6);
  sy2 = geda_line_object_get_y2(object6);

  if (sx1 - 1000 || sx2 - 2000) {
    fprintf(stderr, "FAILED: (O120606X) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  if (sy1 - 1000 || sy2 - 2000) {
    fprintf(stderr, "FAILED: (O120606Y) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  /* === object7->net  === */

  sx1 = geda_line_object_get_x1(object7);
  sy1 = geda_line_object_get_y1(object7);
  sx2 = geda_line_object_get_x2(object7);
  sy2 = geda_line_object_get_y2(object7);

  if (sx1 - 1000 || sx2 - 2000) {
    fprintf(stderr, "FAILED: (O120607X) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  if (sy1 - 1000 || sy2 - 2000) {
    fprintf(stderr, "FAILED: (O120607Y) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  /* === object8->path === */

  /* geda_object_list_scale does not yet scale path objects. */

  /* === object9->picture === */

  sx1 = geda_picture_object_get_lower_x(object9);
  sy1 = geda_picture_object_get_lower_y(object9);
  sx2 = geda_picture_object_get_upper_x(object9);
  sy2 = geda_picture_object_get_upper_y(object9);

  /* (650, -350), (-350, 650) */
  if (sx1 - 650 || sx2 + 350) {
    fprintf(stderr, "FAILED: (O120609X) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

  if (sy1 + 350 || sy2 - 650) {
    fprintf(stderr, "FAILED: (O120609Y) geda_object_list_scale: ");
    fprintf(stderr, "(%d, %d),(%d, %d)\n", sx1, sy1, sx2, sy2);
    result++;
  }

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

/** @} endgroup test-object-list */

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

  if (setjmp(point) == 0) {
    result += check_object_list_mirror(toplevel);
  }
  else {
    fprintf(stderr, "Caught signal checking geda_object_list_mirror\n\n");
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_object_list_rotate(toplevel);
  }
  else {
    fprintf(stderr, "Caught signal checking geda_object_list_rotate\n\n");
    return 1;
  }

  if (setjmp(point) == 0) {
    result += check_object_list_scale(toplevel);
  }
  else {
    fprintf(stderr, "Caught signal checking geda_object_list_scale\n\n");
    return 1;
  }

  g_object_unref(toplevel);

  return result;
}
