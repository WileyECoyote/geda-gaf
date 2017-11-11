/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_menu_button.c
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
 * Date: Aug, 23, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "../../config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_menu_button.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_menu_button.c"

#define TWIDGET "GedaMenuButton"

/*! \file test_menu_button.c
 *  \brief Tests for geda_menu_button.c module
 */

int check_construction (void)
{
  int result = 0;

  /* geda_menu_button_new */

  GtkWidget *widget = geda_menu_button_new(NULL, NULL);

  if (!GEDA_IS_MENU_BUTTON(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    if (!GTK_IS_EVENT_BOX(widget)) {
      fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
      result++;
    }

    g_object_ref_sink(widget); /* Sink reference to menu_item */
    g_object_unref(widget);    /* Does not destroy widget */
  }

  /* geda_menu_button_new_from_stock */

  widget = geda_menu_button_new_from_stock(NULL);

  if (GEDA_IS_MENU_BUTTON(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  widget = geda_menu_button_new_from_stock("edit-clear");

  if (!GEDA_IS_MENU_BUTTON(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

    g_object_ref_sink(widget); /* Sink reference to menu_item */
    g_object_unref(widget);    /* Does not destroy widget */
  }

  return result;
}

int
check_accessors ()
{
  int result = 0;

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
      result++;
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
