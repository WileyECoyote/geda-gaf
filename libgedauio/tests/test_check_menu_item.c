/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_check_menu_item.c
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
 * Date: May, 14, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_check_menu_item.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_check_menu_item.c"

#define TWIDGET "GedaCheckMenuItem"

/*! \file test_check_menu_item.c
 *  \brief Tests for geda_check_menu_item.c module
 */

int check_construction (void)
{
  int result = 0;

  /* geda_check_menu_item_new */

  GtkWidget *widget = geda_check_menu_item_new();

  if (!GEDA_IS_CHECK_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_check_menu_item_new_with_label */

  widget = geda_check_menu_item_new_with_label(NULL);

  if (!GEDA_IS_CHECK_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  /* geda_check_menu_item_new_with_mnemonic */

  widget = geda_check_menu_item_new_with_mnemonic("<b>Tortoise</b>");

  if (!GEDA_IS_CHECK_MENU_ITEM(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to menu_item */
  g_object_unref(widget);    /* Does not destroy widget */

  return result;
}

int
check_accessors ()
{
  int result = 0;

  GedaCheckMenuItem *check_menu_item;

  GtkWidget *widget = geda_check_menu_item_new();

  check_menu_item = GEDA_CHECK_MENU_ITEM(widget);

  /* active property */

  geda_check_menu_item_set_active(check_menu_item, FALSE);

  /* geda_check_menu_item_get_active */
  if (geda_check_menu_item_get_active (check_menu_item)) {
    fprintf(stderr, "FAILED: line <%d> get/set active %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* geda_check_menu_item_set_active */
  geda_check_menu_item_set_active(check_menu_item, TRUE);

  if (!geda_check_menu_item_get_active (check_menu_item)) {
    fprintf(stderr, "FAILED: line <%d> get/set active %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* draw_as_radio property */

  geda_check_menu_item_set_draw_as_radio(check_menu_item, FALSE);

  /* geda_check_menu_item_get_draw_as_radio */
  if (geda_check_menu_item_get_draw_as_radio (check_menu_item)) {
    fprintf(stderr, "FAILED: line <%d> get/set draw_as_radio %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* geda_check_menu_item_set_draw_as_radio */
  geda_check_menu_item_set_draw_as_radio(check_menu_item, TRUE);

  if (!geda_check_menu_item_get_draw_as_radio (check_menu_item)) {
    fprintf(stderr, "FAILED: line <%d> get/set draw_as_radio %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* inconsistent property */

  geda_check_menu_item_set_inconsistent(check_menu_item, FALSE);

  /* geda_check_menu_item_get_inconsistent */
  if (geda_check_menu_item_get_inconsistent (check_menu_item)) {
    fprintf(stderr, "FAILED: line <%d> get/set inconsistent %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* geda_check_menu_item_set_inconsistent */
  geda_check_menu_item_set_inconsistent(check_menu_item, TRUE);

  if (!geda_check_menu_item_get_inconsistent (check_menu_item)) {
    fprintf(stderr, "FAILED: line <%d> get/set inconsistent %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* show_toggle property */

  geda_check_menu_item_set_show_toggle(check_menu_item, FALSE);

  /* geda_check_menu_item_get_show_toggle */
  if (geda_check_menu_item_get_show_toggle (check_menu_item)) {
    fprintf(stderr, "FAILED: line <%d> get/set show_toggle %s\n", __LINE__, TWIDGET);
    result++;
  }

  /* geda_check_menu_item_set_show_toggle */
  geda_check_menu_item_set_show_toggle(check_menu_item, TRUE);

  if (!geda_check_menu_item_get_show_toggle (check_menu_item)) {
    fprintf(stderr, "FAILED: line <%d> get/set show_toggle %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

static int toggled = 0;
static int draw_indicator = 0;

static void check_menu_item_toggled (GedaCheckMenuItem *check_menu_item)
{
  toggled++;
}

static void check_menu_draw_indicator (GedaCheckMenuItem *check_menu_item,
                                       GdkRectangle      *area)
{
  draw_indicator++;
}

int
check_overides ()
{
  int result = 0;

  GedaCheckMenuItemClass *check_menu_item_class;

  GtkWidget *widget = geda_check_menu_item_new();

  check_menu_item_class = GEDA_CHECK_MENU_ITEM_GET_CLASS(widget);

  check_menu_item_class->toggled        = check_menu_item_toggled;
  check_menu_item_class->draw_indicator = check_menu_draw_indicator;

  toggled = 0;

  geda_check_menu_item_toggled(GEDA_CHECK_MENU_ITEM(widget));

  if (!toggled) {
    fprintf(stderr, "FAILED: line <%d> overide toggled %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!draw_indicator) {
    fprintf(stderr, "FAILED: line <%d> overide draw_indicator %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

  return result;
}

static void
on_toggled (GedaCheckMenuItem *check_menu_item, void *nothing)
{
  toggled++;
}

int
check_signals ()
{
  int result = 0;

  GtkWidget *widget = geda_check_menu_item_new();

  toggled = 0;

  g_signal_connect (widget, "toggled", G_CALLBACK (on_toggled), NULL);

  /* geda_check_menu_item_toggled */
  geda_check_menu_item_toggled(GEDA_CHECK_MENU_ITEM(widget));

  if (!toggled) {
    fprintf(stderr, "FAILED: line <%d> toggled %s\n", __LINE__, TWIDGET);
    result++;
  }

  g_object_ref_sink(widget); /* Sink reference to entry widget */
  g_object_unref(widget);    /* Destroy the widget */

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

      if (setjmp(point) == 0) {
        result += check_signals();
      }
      else {
        fprintf(stderr, "Caught signal checking signals in %s\n\n", MUT);
        return 1;
      }
    }
  }
  return result;
}
