/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=2 sw=2 sts=2: */
/*
 * File: test_menu.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
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
 * Date: June, 16, 2016
 * Contributing Author: Wiley Edward Hill
 */

#include "config.h"

#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda_menu.h>
#include <geda_menu_shell.h>

#define TWIDGET "GedaMenu"

/*! \file test_menu.c
 *  \brief Tests for geda_menu.c module
 */

int check_construction (void)
{
  int result = 0;

  GtkWidget *widget = geda_menu_new();

  if (!GEDA_IS_MENU(widget)) {
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
main (int argc, char *argv[])
{
  int result = 0;
  int subtotal = 0;

  /* Initialize gobject */
#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 36 ))
  g_type_init();
#endif

  if (argc > 1) {

     if (strcmp(argv[1], "-m") ==0) {
       fprintf(stderr, "%s: The Yahoos are comming!", argv[0]);
     }
     else {
       fprintf(stderr, "%s: unknown option: <%s>", argv[0], argv[1]);
       result++;
     }
  }
  else {

    if (gtk_init_check(&argc, &argv)) {

      subtotal = check_construction();
      if (subtotal) {
        fprintf(stderr, "Check constructors in src/widgets/geda_menu.c");
        result   = subtotal;
        subtotal = 0;
      }
    }
  }
  return result;
}
