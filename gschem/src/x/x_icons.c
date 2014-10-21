/* -*- C x_icons.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_icons.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 * Date: Jan, 31, 2013
 * Contributing Author: Wiley Edward Hill
 *
 */

/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ---------------|--------------------------------------------------
 * WEH | 01/31/13 |  Inital release, new file.
 * ---------------|--------------------------------------------------
 * WEH | 09/10/13 | Added more icons, seperate geda &  gschem (to
 *                | reduce the lists to smaller sizes, in anticipation
 *                | adding more icons).
 *                | to support routines not using embed labels.
 * ---------------|--------------------------------------------------
 * WEH | 09/16/13 | Relocated _set_default_icon from x_window to this
 *                | module (improvement to general code organization).
 *                | Added new function x_icons_add_search_path from
 *                | master branch ([with new name] as feature update).
 *                | Consolidated icon intialization to new function
 *                | x_icons_initialize (for better code organization).
 * ---------------|--------------------------------------------------
 * WEH | 07/20/14 | Added Doxygen grouping wrapper for this module (to
 *                | to improve source documentation).
 */
/*!
 * \file x_icons.c
 * \brief General Program Module for Icons and Bitmaps
 */

#include <gschem.h>
#include <geda_debug.h>

/** \defgroup Gschem-Icons Gschem-Icons
 *  @{
 *  \par This module contains routines for icons images
 *  in support of Gschem's GUI subsystems such as menus and toolbars.
 *  This support primarily consist of a Gtk icon factory.
 */

static GtkIconFactory* gschem_factory;

const char* IDS_GEDA_ICONS[] = {  /* Menu Icons Strings*/
  "geda-arc",           "geda-arc-edit",      "geda-analysis",     "geda-autonum-blue",
  "geda-autonum-green", "geda-autonum-red",   "geda-bed",          "geda-box",
  "geda-bus",           "geda-circle",        "geda-circles",      "geda-close-all",
  "geda-calculate",     "geda-component",     "geda-copy",         "geda-design",
  "geda-design1",       "geda-design2",       "geda-design3",      "geda-design4",
  "geda-display",       "geda-draft",         "geda-grid-dot",     "geda-grid-mesh",
  "geda-icprint",       "geda-inbed",
  "geda-line",          "geda-line-type",     "geda-lock",         "geda-net",
  "geda-marker",        "geda-magnet",        "geda-mesh",         "geda-mirror",
  "geda-molecule",      "geda-move",          "geda-multi",        "geda-nand",
  "geda-nand3",         "geda-new",           "geda-node",         "geda-npn",
  "geda-open-recent",   "geda-path",          "geda-pin",          "geda-pin-type",
  "geda-plot",          "geda-probe",         "geda-probe2",       "geda-save-image",
  "geda-schem2",        "geda-schem",         "geda-scope2",       "geda-scopehairs",
  "geda-scope",         "geda-show-nets",     "geda-simulate",     "geda-sinx",
  "geda-slot",          "geda-snap-off",      "geda-wave",         "geda-waves",
  "geda-snap-on",       "geda-spectrum",      "geda-text-editor",  "geda-rotate",
  "geda-unlock",        "geda-unselect-all",  "geda-zoom-box",     "geda-zoom-pan",
  "geda-zoom-selection",
  NULL
};

const char* IDS_GSCHEM_ICONS[] = {
  "gschem-bus",              "gschem-invert",            "gschem-net",
  "gschem-select",           "gschem-select-all",        "gschem-unselect",
  NULL
};

const char* IDS_THEME_ICONS[] = {
  "gschem-attribute-attach", "gschem-attribute-detach",  "gschem-clone",
  "gschem-datasheet",        "gschem-deselect",
  "gschem-insert-arc",       "gschem-insert-attribute",  "gschem-insert-box",
  "gschem-insert-bus",       "gschem-insert-circle",     "gschem-insert-line",
  "gschem-insert-net",       "gschem-insert-path",       "gschem-insert-symbol",
  "gschem-insert-pin",       "gschem-insert-text",       "gschem-move",
  "gschem-multi-clone",      "gschem-show-both",         "gschem-show-value",
  "gschem-show-name",        "gschem-unselect-all",      "gschem-zoom-extents",
  "gschem-zoom-fit",         "gschem-zoom-in",           "gschem-zoom-out",
  "gschem-zoom-selection",
  NULL
};


/*! \brief Setup default icon for GTK windows
 *
 *  \par Function Description
 *  Sets the default window icon by name, to be found in the current icon
 *  theme path.
 *
 *  \note The default icon name is \#defined in sdefines.h as
 *        GSCHEM_THEME_ICON_NAME.
 */
void x_icons_set_default_icon (const char* icon_name)
{
  gtk_window_set_default_icon_name( icon_name );
}

/*! \brief Setup icon search paths.
 * \par Function Description
 * Add the icons installed by gschem to the search path for the
 * default icon theme, so that they can be automatically found by GTK.
 */
void x_icons_add_search_path (const char *path)
{
  char *icon_path;

  g_return_if_fail (f_path_sys_data () != NULL);

  icon_path = g_build_filename (f_path_sys_data (), path, NULL);
  gtk_icon_theme_append_search_path (gtk_icon_theme_get_default (),
                                     icon_path);

  GEDA_FREE (icon_path);
}

/*! \brief Stock Icon Factory
 *
 *  \par Function Description
 *  This function sets up a GTK Icon Factory
 */
static void x_icons_setup_factory()
{
  GtkIconSet *icon_set;
  GdkPixbuf  *pixbuf;
  GError     *err;

  const char *icon_name;
  char       *filename;
  char       *pathname;
  int index;

  err       = NULL;
  icon_name = NULL;

  gschem_factory = gtk_icon_factory_new ();
  gtk_icon_factory_add_default (gschem_factory);

  for ( index = 0; IDS_GEDA_ICONS[index] != NULL; index++ ) {

    icon_name = IDS_GEDA_ICONS[index];

    filename = g_strconcat (icon_name, ".png", NULL);
    pathname = g_build_filename (f_path_sys_data (), "bitmap", filename, NULL);
    GEDA_FREE(filename);
    if(pathname) {
      if( g_file_test(pathname, G_FILE_TEST_EXISTS) &&
        ( access(pathname, R_OK) == 0)) {
        pixbuf = gdk_pixbuf_new_from_file(pathname, &err);
        if(!err) {
          icon_set = gtk_icon_set_new_from_pixbuf(pixbuf);
          gtk_icon_factory_add (gschem_factory, icon_name, icon_set);
        }
        else {
          u_log_message("Warning, Error reading image file: %s\n", err->message);
          g_clear_error (&err);
          err = NULL;
        }
      }
      else { /* file non existence or not accessible */
        u_log_message("Warning, Error accessing image file: %s\n", pathname);
      }
      GEDA_FREE(pathname);
    }
  }

  for ( index = 0; IDS_GSCHEM_ICONS[index] != NULL; index++ ) {

    icon_name = IDS_GSCHEM_ICONS[index];

    filename = g_strconcat (icon_name, ".png", NULL);
    pathname = g_build_filename (f_path_sys_data (), "bitmap", filename, NULL);
    GEDA_FREE(filename);
    if(pathname) {
      if( g_file_test(pathname, G_FILE_TEST_EXISTS) &&
        ( access(pathname, R_OK) == 0)) {
        pixbuf = gdk_pixbuf_new_from_file(pathname, &err);
        if(!err) {
          icon_set = gtk_icon_set_new_from_pixbuf(pixbuf);
          gtk_icon_factory_add (gschem_factory, icon_name, icon_set);
        }
        else {
          u_log_message("Warning, Error reading image file: %s\n", err->message);
          g_clear_error (&err);
          err = NULL;
        }
      }
      else { /* file non existence or not accessible */
        u_log_message("Warning, Error accessing image file: %s\n", pathname);
      }
      GEDA_FREE(pathname);
    }
  }
}

/*! \brief Setup icon search paths.
 * \par Function Description
 * Add the icons installed by gschem to the search path for the
 * default icon theme, so that they can be automatically found by GTK.
 */
void x_icons_initialize (void)
{
  x_icons_add_search_path ("icons");

  x_icons_set_default_icon(GSCHEM_THEME_ICON_NAME);

  x_icons_setup_factory();

}
/** @} endgroup Gschem-Icons */