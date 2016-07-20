/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_menu_shell.c
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
 * Date: June, 20, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_menu_bar.h>
#include <geda_menu_shell.h>

#include "test-suite.h"

/*! \def MUT Module Under Tests */
#define MUT "src/widgets/geda_menu_bar.c"

#define TWIDGET "GedaMenuBar"

/*! \file test_menu_bar.c
 *  \brief Tests for geda_menu_bar.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_menu_bar_new();

  if (!GEDA_IS_MENU_BAR(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }

  if (!GEDA_IS_MENU_SHELL(widget)) {
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

  GtkWidget *widget = geda_menu_bar_new();

  if (!GEDA_IS_MENU_BAR(widget)) {
    fprintf(stderr, "FAILED: line <%d> is a %s\n", __LINE__, TWIDGET);
    result++;
  }
  else {

  /* -------------------- pack_direction -------------------- */

    PackDirection pack_dir;

    GedaMenuBar *menubar = GEDA_MENU_BAR(widget);

    pack_dir = geda_menu_bar_get_pack_direction (menubar);

    if (pack_dir != PACK_DIRECTION_LTR) { /* Default value */
      fprintf(stderr, "FAILED: %s default pack_direction <%d>\n", TWIDGET, pack_dir);
      result++;
    }

    geda_menu_bar_set_pack_direction (menubar, PACK_DIRECTION_RTL);

    pack_dir = geda_menu_bar_get_pack_direction (menubar);

    if (pack_dir != PACK_DIRECTION_RTL) { /* Default value */
      fprintf(stderr, "FAILED: %s pack direction <%d>\n", TWIDGET, pack_dir);
      result++;
    }

    pack_dir = geda_menu_bar_get_child_pack_direction (menubar);

    if (pack_dir != PACK_DIRECTION_LTR) { /* Default value */
      fprintf(stderr, "FAILED: %s default child pack_direction <%d>\n", TWIDGET, pack_dir);
      result++;
    }

    geda_menu_bar_set_child_pack_direction (menubar, PACK_DIRECTION_RTL);

    pack_dir = geda_menu_bar_get_child_pack_direction (menubar);

    if (pack_dir != PACK_DIRECTION_RTL) { /* Default value */
      fprintf(stderr, "FAILED: %s child pack direction <%d>\n", TWIDGET, pack_dir);
      result++;
    }

    g_object_ref_sink(widget); /* Sink reference to the widget */
    g_object_unref(widget);    /* Destroy the widget */
  }

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
