/* -*- C x_icons.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_icons.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * WEH | 09/10/13 | Added more icons, separate geda &  gschem (to
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

#define ACTION_ICON_THEME_22_PATH "icons/hicolor/22x22/actions"

/** \defgroup Gschem-Icons Gschem Icons
 *  @{
 *  \brief Gschem icon module
 *  \par
 *  This module contains routines for icons images in support of
 *  Gschem's GUI subsystems such as menus and toolbars. The support
 *  primarily consist of a Gtk icon factory.
 */

static GtkIconFactory *gschem_factory;

const char *IDS_GEDA_ICONS[] = {  /* Menu Icons Strings */
  "gaf-add-attribute",  "gaf-bom",            "gaf-demote",         "gaf-hierarchy-sch",
  "gaf-hierarchy-sym",  "gaf-hierarchy-up",   "gaf-pdf",            "gaf-promote",
  "gaf-see-notes",      "gaf-tools",
  "geda-arc",           "geda-arc-edit",      "geda-analysis",      "geda-autonum-blue",
  "geda-autonum-green", "geda-autonum-red",   "geda-bed",           "geda-box",
  "geda-bus",           "geda-check-grn",     "geda-check-org",     "geda-check-red",
  "geda-circle",        "geda-circles",       "geda-close",         "geda-close-all",
  "geda-calculate",     "geda-component",     "geda-copy",          "geda-demote",
  "geda-deslect",       "geda-design",        "geda-design1",       "geda-design2",
  "geda-design3",       "geda-design4",       "geda-display",       "geda-display-color",
  "geda-draft",         "geda-film-roll",     "geda-find",          "geda-find-attribute",
  "geda-grid-dot",      "geda-grid-mesh",     "geda-icprint",       "geda-inbed",
  "geda-inspect-grn",   "geda-invisible",     "geda-lightning",     "geda-line",
  "geda-line-type",     "geda-lock",          "geda-net",           "geda-marker",
  "geda-magnet",        "geda-mesh",          "geda-mirror",        "geda-molecule",
  "geda-move",          "geda-multi",         "geda-name-tag",      "geda-name-value",
  "geda-nand",          "geda-nand3",         "geda-new",           "geda-node",
  "geda-npn",           "geda-offset",        "geda-open-recent",   "geda-path",
  "geda-pin",           "geda-pin-type",      "geda-plot",          "geda-properties",
  "geda-probe",         "geda-probe2",        "geda-promote",       "geda-prompt",
  "geda-redo",          "geda-rotate-left",
  "geda-save-image",    "geda-schem2",        "geda-schem",         "geda-scope2",
  "geda-scopehairs",    "geda-scope",         "geda-show-nets",     "geda-simulate",
  "geda-sinx",          "geda-slot",          "geda-snap",          "geda-snap-off",
  "geda-snap-on",       "geda-spectrum",      "geda-text-editor",   "geda-tools",
  "geda-translate",     "geda-undo",          "geda-unlock",        "geda-unselect-all",
  "geda-value",         "geda-verilog-blue",  "geda-verilog-grn",   "geda-view-redraw",
  "geda-wave",          "geda-waves",         "geda-xml",           "geda-zoom-box",
  "geda-zoom-extents",  "geda-zoom-in",       "geda-zoom-limits",   "geda-zoom-out",
  "geda-zoom-pan",      "geda-zoom-selection","git-logo",           "guile-logo",
  NULL
};

/* These have png file extension */
const char *IDS_GSCHEM_ICONS[] = {
  "gschem-array",            "gschem-bus",              "gschem-comp",
  "gschem-invert",           "gschem-net",
  "gschem-page-man",         "gschem-print-document",   "gschem-print-preview",
  "gschem-select",           "gschem-select-all",       "gschem-text",
  "gschem-transistor",       "gschem-unselect",
  NULL
};

/* These have xpm file extension */
const char *IDS_GSCHEM_XCONS[] = {
  "gschem_copy",             "gschem_delete",         "gschem_edit",
  "gschem_mirror",           "gschem_move",           "gschem_new",
  "gschem_open",             "gschem_redo",           "gschem_rotate",
  "gschem_save",             "gschem-save-as",        "gschem_undo",
  NULL
};

const char *IDS_THEME_ICONS_22[] = {
  "attribute-attach",        "attribute-detach",         "attribute-reset",
  "attribute-visibility",    "break",
  "clone",                   "close-path",               "deselect",
  "extend",
  "insert-arc",              "insert-attribute",         "insert-box",
  "insert-bus",              "insert-circle",            "insert-line",
  "insert-net",              "insert-path",              "insert-pin",
  "insert-symbol",           "insert-text",              "multi-clone",
  "process-stop",            "select",                   "show-both",
  "show-hidden",             "show-inherited",           "show-name",
  "show-netnames",           "show-value",               "symbol-datasheet",
  "unselect-all",            "zoom-extents",             "zoom-fit",
  "zoom-in",                 "zoom-limits",              "zoom-mag",
  "zoom-out",                "zoom-selection",
  NULL
};

/*!
 * \brief Check if Icon is in the Factory
 * \par Function Description
 *  Wrapper for gtk_icon_factory_lookup.
 *
 * \param [in] icon_id String name of the icon, like "gaf-pdf"
 *
 * \retval TRUE if the icon was found, otherwise FALSE.
 */
bool x_icons_factory_lookup (const char *icon_id)
{
  if (icon_id) {

    GtkIconSet *bs;

    bs = gtk_icon_factory_lookup (gschem_factory, icon_id);

    return bs != NULL;
  }

  return FALSE;
}

/*!
 * \brief Get Factory Icon Widget given Action string and Size
 * \par Function Description
 *  The \a action must be known to i_command_get_action_icon.
 *  Attempts to retrieve the icon from the local factory, if
 *  the icon is not found then checks if is a stock gtk icon.
 *
 *  example:
 *
 *  GtkWidget *icon;
 *
 *  icon = x_icons_get_factory_icon("add-circle", TB_SMALL_ICON);
 */
GtkWidget *x_icons_get_action_icon (const char *action, int size)
{
  GtkWidget  *image;

  char *icon_id = i_command_get_action_icon (action);

  if (icon_id) {

    GtkIconSet *icon_set;

    icon_set = gtk_icon_factory_lookup(gschem_factory, icon_id);

    if (icon_set) {
      image = gtk_image_new_from_stock(icon_id, size);
    }
    else {

      if (strncmp(icon_id, "gtk-",4) == 0) {
        image = gtk_image_new_from_stock (icon_id, size);
      }
      else {
        image = gtk_image_new_from_icon_name (icon_id, size);
      }
    }

    free (icon_id);
  }
  else {
    image = NULL;
  }

  return image;
}

/*!
 * \brief Get Factory Icon Widget given string Id and Size
 * \par Function Description
 *  Wrapper for gtk_icon_factory_lookup and gtk_image_new_from_icon_set.
 *
 *  example:
 *
 *  GtkWidget *icon;
 *
 *  icon = x_icons_get_factory_icon("geda-wave", TB_SMALL_ICON);
 */
GtkWidget *x_icons_get_factory_icon (const char *icon_id, int size)
{
  GtkWidget  *image;

  if (icon_id) {

    GtkIconSet *icon_set;

    icon_set = gtk_icon_factory_lookup (gschem_factory, icon_id);

    if (icon_set) {
      image = gtk_image_new_from_icon_set  (icon_set, size);
    }
    else {
      image = NULL;
    }
  }
  else {
    image = NULL;
  }

  return image;
}

/*!
 * \brief Setup default icon for GTK windows
 * \par Function Description
 *  Sets the default window icon by name, to be found in the current
 *  icon theme path.
 *
 * \note The default icon name is defined in sdefines.h as
 *       GSCHEM_THEME_ICON_NAME.
 */
void x_icons_set_default_icon (const char* icon_name)
{
  gtk_window_set_default_icon_name(icon_name);
}

/*!
 * \brief Setup icon search paths
 * \par Function Description
 * Add the icons installed by gschem to the search path for the
 * default icon theme, so that they can be automatically found
 * by GTK (even though Gtk will not look for them).
 */
void x_icons_add_search_path (const char *path)
{
  char *icon_path;

  g_return_if_fail (geda_sys_data_path () != NULL);

  icon_path = g_build_filename (geda_sys_data_path(), path, NULL);

  gtk_icon_theme_append_search_path (gtk_icon_theme_get_default(), icon_path);

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

  const char *err_warn_read   = _("Warning, Error reading image file");
  const char *err_warn_access = _("Warning, Error accessing image file");

  err       = NULL;
  icon_name = NULL;

  gschem_factory = gtk_icon_factory_new ();
  gtk_icon_factory_add_default (gschem_factory);

  for (index = 0; IDS_GEDA_ICONS[index] != NULL; index++) {

    icon_name = IDS_GEDA_ICONS[index];

    filename = geda_strconcat (icon_name, ".png", NULL);
    pathname = geda_file_get_bitmap_filespec (filename);
    GEDA_FREE(filename);
    if (pathname) {
      if (g_file_test(pathname, G_FILE_TEST_EXISTS) &&
         (access(pathname, R_OK) == 0))
      {
        pixbuf = gdk_pixbuf_new_from_file(pathname, &err);
        if(!err) {
          icon_set = gtk_icon_set_new_from_pixbuf(pixbuf);
          gtk_icon_factory_add (gschem_factory, icon_name, icon_set);
          GEDA_UNREF(pixbuf);
        }
        else {
           geda_log("%s: %s\n", err_warn_read, err->message);
          g_clear_error (&err);
          err = NULL;
        }
      }
      else
      {
        /* file non existence or not accessible */
         geda_log("%s: %s\n", err_warn_access, pathname);
      }
      GEDA_FREE(pathname);
    }
  }

  for (index = 0; IDS_GSCHEM_ICONS[index] != NULL; index++) {

    icon_name = IDS_GSCHEM_ICONS[index];

    filename = geda_strconcat (icon_name, ".png", NULL);
    pathname = geda_file_get_bitmap_filespec (filename);
    GEDA_FREE(filename);

    if(pathname) {
      if (g_file_test(pathname, G_FILE_TEST_EXISTS) &&
         (access(pathname, R_OK) == 0))
      {
        pixbuf = gdk_pixbuf_new_from_file(pathname, &err);
        if(!err) {
          icon_set = gtk_icon_set_new_from_pixbuf(pixbuf);
          gtk_icon_factory_add (gschem_factory, icon_name, icon_set);
          GEDA_UNREF(pixbuf);
        }
        else {
           geda_log("%s: %s\n", err_warn_read, err->message);
          g_clear_error (&err);
          err = NULL;
        }
      }
      else
      {
        /* file non existence or not accessible */
         geda_log("%s: %s\n", err_warn_access, pathname);
      }
      GEDA_FREE(pathname);
    }
  }

  for (index = 0; IDS_GSCHEM_XCONS[index] != NULL; index++) {

    icon_name = IDS_GSCHEM_XCONS[index];

    filename = geda_strconcat (icon_name, ".xpm", NULL);
    pathname = geda_file_get_bitmap_filespec (filename);
    GEDA_FREE(filename);

    if(pathname) {
      if (g_file_test(pathname, G_FILE_TEST_EXISTS) &&
         (access(pathname, R_OK) == 0))
      {
        pixbuf = gdk_pixbuf_new_from_file(pathname, &err);
        if(!err) {
          icon_set = gtk_icon_set_new_from_pixbuf(pixbuf);
          gtk_icon_factory_add (gschem_factory, icon_name, icon_set);
          GEDA_UNREF(pixbuf);
        }
        else {
          u_log_message("%s: %s\n", err_warn_read, err->message);
          g_clear_error (&err);
          err = NULL;
        }
      }
      else
      {
        /* file non existence or not accessible */
        u_log_message("%s: %s\n", err_warn_access, pathname);
      }
      GEDA_FREE(pathname);
    }
  }

  for ( index = 0; IDS_THEME_ICONS_22[index] != NULL; index++ ) {

    icon_name = IDS_THEME_ICONS_22[index];

    filename = geda_strconcat (icon_name, ".png", NULL);
    pathname = g_build_filename (geda_sys_data_path (), ACTION_ICON_THEME_22_PATH, filename, NULL);
    GEDA_FREE(filename);

    if (pathname) {
      if (g_file_test(pathname, G_FILE_TEST_EXISTS) &&
         (access(pathname, R_OK) == 0))
      {
        pixbuf = gdk_pixbuf_new_from_file(pathname, &err);
        if(!err) {
          icon_set = gtk_icon_set_new_from_pixbuf(pixbuf);
          gtk_icon_factory_add (gschem_factory, icon_name, icon_set);
          GEDA_UNREF(pixbuf);
        }
        else {
          u_log_message("%s: %s\n", err_warn_read, err->message);
          g_clear_error (&err);
          err = NULL;
        }
      }
      else
      {
        /* file non existence or not accessible */
        u_log_message("%s: %s\n", err_warn_access, pathname);
      }
      GEDA_FREE(pathname);
    }
  }
}

static void x_icons_remove_icons_from_factory(void)
{
  GtkIconSet *icon_set;
  int index;

  for (index = 0; IDS_GEDA_ICONS[index] != NULL; index++) {
    icon_set =  gtk_icon_factory_lookup_default (IDS_GEDA_ICONS[index]);
    if (icon_set) {
      gtk_icon_set_unref (icon_set);
    }
  }

  for (index = 0; IDS_GSCHEM_ICONS[index] != NULL; index++) {
    icon_set =  gtk_icon_factory_lookup_default (IDS_GSCHEM_ICONS[index]);
    if (icon_set) {
      gtk_icon_set_unref (icon_set);
    }
  }

  for (index = 0; IDS_THEME_ICONS_22[index] != NULL; index++) {
    icon_set =  gtk_icon_factory_lookup_default (IDS_THEME_ICONS_22[index]);
    if (icon_set) {
      gtk_icon_set_unref (icon_set);
    }
  }
}

static void x_icons_shutdown_factory(void * user_data)
{
  if (gschem_factory) {

    /* This will go to the console, because the log system is already down */
    v_log_message(_("Shutting down icon factory\n"));

    GEDA_REF(gschem_factory);

    x_icons_remove_icons_from_factory();

    gtk_icon_factory_remove_default(gschem_factory);

    GEDA_UNREF(gschem_factory);

    gtk_icon_theme_set_search_path (gtk_icon_theme_get_default(), NULL, 0);
  }
}

/*! \brief Setup icon search paths.
 * \par Function Description
 * Add the icons installed by gschem to the search path for the
 * default icon theme, so that they can be automatically found by GTK.
 */
void x_icons_initialize (void)
{
  v_log_message(_("Initializing icon factory\n"));

  x_icons_add_search_path ("icons");

  x_icons_set_default_icon(GSCHEM_THEME_ICON_NAME);

  x_icons_setup_factory();

  gschem_atexit(x_icons_shutdown_factory, NULL);

}
/** @} endgroup Gschem-Icons */
