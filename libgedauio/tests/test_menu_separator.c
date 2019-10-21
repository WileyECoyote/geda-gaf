/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_menu_separator.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of the
 * License.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: November, 10, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>

#include "../include/geda_bulb.h"
#include "../include/geda_menu_item.h"
#include "../include/geda_menu_separator.h"

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_menu_separator.c"

#define TWIDGET "GedaMenuSeparator"

/*! \file test_menu_separator.c
 *  \brief Tests for geda_menu_separator.c module
 */

int check_construction (void)
{
  int result = 0;

  unsigned int bad_address = 0x7FFF0;

  /* Check instance identifier when no instances exist */

  if (GEDA_IS_MENU_SEPERATOR(&bad_address)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  GtkWidget *widget = geda_menu_separator_new();

  /* Check instance identifier with NULL */

  if (GEDA_IS_MENU_SEPERATOR(NULL)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* Check instance identifier with pointer to low memory location */

  if (GEDA_IS_MENU_SEPERATOR(&bad_address)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* Check instance identifier with another type of object */

  GtkWidget *bulb_widget = geda_bulb_new(NULL);

  if (GEDA_IS_MENU_SEPERATOR(bulb_widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(bulb_widget); /* Sink reference to the tmp bulb */
  g_object_unref(bulb_widget);    /* Destroy bulb widget */

  /* Check instance identifier with a valid object */

  if (!GEDA_IS_MENU_SEPERATOR(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    GtkWidget *widget2 = geda_menu_separator_new();

    /* Check instance identifier of parent class */
    if (!GEDA_IS_MENU_ITEM(widget)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    g_object_ref_sink(widget); /* Sink reference to menu_seperator */
    g_object_unref(widget);    /* Does not destroy widget */

    /* Check that child instance was destroyed */
    if (GEDA_IS_MENU_SEPERATOR(widget)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* Check that parent instance was destroyed */
    if (GEDA_IS_MENU_ITEM(widget)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    /* Check 2nd instance is still valid */
    if (!GEDA_IS_MENU_ITEM(widget2)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    if (G_IS_OBJECT(widget)) {
      g_object_ref_sink(widget); /* Sink reference to menu_seperator */
      g_object_unref(widget);    /* Does not destroy widget */
    }
  }

  return result;
}

int check_accessors(void)
{
  int result = 0;

  GedaMenuItem *menu_item;
  GtkWidget    *widget;

  widget    = geda_menu_separator_new();
  menu_item = GEDA_MENU_ITEM(widget);

  if (!menu_item) {
    fprintf(stderr, "FAILED: line <%d> GEDA_MENU_ITEM %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    if (geda_menu_item_is_selectable (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; selectable\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_is_widget_selectable (widget)) {
      fprintf(stderr, "FAILED: line <%d> %s; widget selectable\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_event_window (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; event window\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_submenu_widget (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; submenu widget\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_accel_path (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; accel path\n", __LINE__, TWIDGET);
      result++;
    }

    char c = geda_menu_item_get_mnemonic(menu_item);

    if (c != -1) {
      fprintf(stderr, "FAILED: line <%d> %s; mnemonic <%c>\n", __LINE__, TWIDGET, c);
      result++;
    }

    if (geda_menu_item_get_right_justified (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; right justified\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_toggle_size (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; toggle size\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_from_menubar (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; from menubar\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_label (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; get label\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_label_widget (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; label widget\n", __LINE__, TWIDGET);
      result++;
    }

    int direct = geda_menu_item_get_submenu_direction (menu_item);

    if (direct != GTK_DIRECTION_RIGHT) {
      fprintf(stderr, "FAILED: line <%d> %s; submenu direction <%d>\n", __LINE__, TWIDGET, direct);
      result++;
    }

    if (geda_menu_item_get_submenu_placement (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; get label\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_use_underline (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; use underline\n", __LINE__, TWIDGET);
      result++;
    }

    if (geda_menu_item_get_reserve_indicator (menu_item)) {
      fprintf(stderr, "FAILED: line <%d> %s; reserve indicator\n", __LINE__, TWIDGET);
      result++;
    }
  }

  g_object_ref_sink(widget); /* Sink reference to menu_seperator */
  g_object_unref(widget);    /* Does not destroy widget */

  return result;
}

int
main (int argc, char *argv[])
{
  int result = 0;

  SETUP_SIGSEGV_HANDLER;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (gtk_init_check(&argc, &argv)) {

    if (setjmp(point) == 0) {
      result = check_construction();
    }
    else {
      fprintf(stderr, "Caught signal checking constructors in %s\n\n", MUT);
    }

    if (!result) {

      if (setjmp(point) == 0) {
        result = check_accessors();
      }
      else {
        fprintf(stderr, "Caught signal checking accessors in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
