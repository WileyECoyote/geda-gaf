/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_file_chooser.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2018 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 *
 *  Date: August, 05, 2014
 *  Contributing Author: Wiley Edward Hill
 */

#ifndef __GEDA_FILE_CHOOSER_H__
#define __GEDA_FILE_CHOOSER_H__

#include "geda_uio_macros.h"  /* For convenience of includer, not geda_file_chooser.c */

#define FILE_CHOOSER_CONFIG_GROUP  "file-chooser"
#define FILE_CHOOSER_CONFIG_FILTER "filter"

/*!
 * FileChooserAction:
 * #FILE_CHOOSER_ACTION_OPEN: Indicates open mode.
 * The file chooser will only let the user pick an existing file.
 * #FILE_CHOOSER_ACTION_SAVE: Indicates save mode.
 * The file chooser will let the user pick an existing file, or type in a new
 * filename.
 * #FILE_CHOOSER_ACTION_SELECT_FOLDER: Indicates select folder mode.
 * The file chooser will let the user pick an existing folder.
 * #FILE_CHOOSER_ACTION_CREATE_FOLDER: Indicates create folder mode.
 * The file chooser will let the user name an existing or new folder.
 *
 * Describes whether a #GedaFileChooser is being used to open existing files
 * or to save to a possibly new file.
 */
typedef enum
{
  FILE_CHOOSER_ACTION_OPEN,
  FILE_CHOOSER_ACTION_SAVE,
  FILE_CHOOSER_ACTION_SELECT_FOLDER,
  FILE_CHOOSER_ACTION_CREATE_FOLDER
} FileChooserAction;

/* GedaFileChooser is a widget that displays a file chooser dialog */

#define GEDA_TYPE_FILE_CHOOSER            (geda_file_chooser_get_type ())
#define GEDA_FILE_CHOOSER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_FILE_CHOOSER, GedaFileChooser))
#define GEDA_FILE_CHOOSER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),   GEDA_TYPE_FILE_CHOOSER, GedaFileChooserClass))
#define GEDA_IS_FILE_CHOOSER(obj)         (is_a_geda_file_chooser((GedaFileChooser*)(obj)))
#define GEDA_IS_FILE_CHOOSER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_FILE_CHOOSER))
#define GEDA_FILE_CHOOSER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_FILE_CHOOSER, GedaFileChooserClass))

typedef struct _GedaFileChooser        GedaFileChooser;
typedef struct _GedaFileChooserClass   GedaFileChooserClass;

struct _GedaFileChooser {

  GtkFileChooserDialog parent;
  GtkWidget           *filter_button;
           int         filter_index;
  unsigned int         handler;
};

struct _GedaFileChooserClass {

  GtkFileChooserDialogClass  parent_class;

  /* Action signals */
  void (* filter_changed)   (GedaFileChooser *chooser);
  void (* geometry_save)    (GedaFileChooser *chooser, char *group);
  void (* geometry_restore) (GedaFileChooser *chooser, char *group);
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType      geda_file_chooser_get_type         (void) GEDA_CONST;
bool          is_a_geda_file_chooser             (GedaFileChooser   *chooser);

GtkWidget    *geda_file_chooser_new              (void              *parent,
                                                  FileChooserAction  action);
GtkWidget    *geda_file_chooser_dialog_new_full  (const char        *title,
                                                  void              *parent,
                                                  FileChooserAction  action,
                                                  const char        *first_button_text,
                                                  ...);

GtkEntry     *geda_file_chooser_get_entry          (GtkWidget *chooser);
char         *geda_file_chooser_get_entry_text     (GtkWidget *chooser);
void          geda_file_chooser_set_entry_text     (GtkWidget *chooser, const char *text);

int           geda_file_chooser_get_filter         (GtkWidget *chooser);
void          geda_file_chooser_set_filter         (GtkWidget *chooser, int value);

char         *geda_file_chooser_get_filename       (GtkWidget *chooser);
bool          geda_file_chooser_set_filename       (GtkWidget *chooser, const char *name);

GSList       *geda_file_chooser_get_filenames      (GtkWidget *chooser);

char         *geda_file_chooser_get_current_folder (GtkWidget *chooser);
void          geda_file_chooser_set_current_folder (GtkWidget *chooser, const char *folder);

void          geda_file_chooser_set_current_name   (GtkWidget *chooser, const char *folder);

GtkWidget    *geda_file_chooser_get_extra_widget   (GtkWidget *chooser);
void          geda_file_chooser_set_extra_widget   (GtkWidget *chooser, GtkWidget *extra);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_FILE_CHOOSER_H__ */
