/* C
 * File: x_icons.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors (see ChangeLog for details)
 *
 * Copyright (C) 2013 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
 * USA
 *
 * Date: Jan, 31, 2013
 * Contributing Author: Wiley Edward Hill
 *
*/


#include <gschem.h>

static GtkIconFactory* gschem_factory;

const char* IDS_GEDA_ICONS[] = {  /* Menu Icons Strings*/
  "geda-arc", "geda-box", "geda-circles", "geda-copy", "geda-line",
  "geda-mirror", "geda-move", "geda-multi", "geda-pin", "geda-rotate",
  "geda-select", "gschem-bus", "gschem-net",
  NULL
};

/*! \brief Stock Icon Factory
 *
 *  \par Function Description
 *  This function handles callbacks for all non-toogle type toolbar
 * buttons, the function retrieves the action from the button widget
 * and pass the action to i_command_process.
 */
void x_icons_setup_factory()
{
  GtkIconSet *icon_set;
  GdkPixbuf  *pixbuf;
  GError     *err = NULL;

  const char* icon_name = NULL;
  char *filename;
  char *pathname;
  int index;

  gschem_factory = gtk_icon_factory_new ();
  gtk_icon_factory_add_default (gschem_factory);

  for ( index = 0; IDS_GEDA_ICONS[index] != NULL; index++ ) {

    icon_name = IDS_GEDA_ICONS[index];

    filename = g_strconcat (icon_name, ".png", NULL);
    pathname = g_build_filename (s_path_sys_data (), "bitmap", filename, NULL);
    g_free(filename);
    if(pathname) {
      if( g_file_test(pathname, G_FILE_TEST_EXISTS) &&
        ( access(pathname, R_OK) == 0)) {
        pixbuf = gdk_pixbuf_new_from_file(pathname, &err);
        if(!err) {
          icon_set = gtk_icon_set_new_from_pixbuf(pixbuf);
          gtk_icon_factory_add (gschem_factory, icon_name, icon_set);
        }
        else {
          s_log_message("Warning, Error reading image file: %s\n", err->message);
          g_clear_error (&err);
          err = NULL;
        }
      }
      else { /* file non existence or not accessible */
        s_log_message("Warning, Error accessing image file: %s\n", pathname);
      }
      g_free(pathname);
    }

  }

}