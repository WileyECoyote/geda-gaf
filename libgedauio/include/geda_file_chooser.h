/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2014 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 *  Date: August, 05, 2014
 *  Contributing Author: Wiley Edward Hill
 */

#ifndef __GEDA_FILE_CHOOSER_H__
#define __GEDA_FILE_CHOOSER_H__

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

G_BEGIN_DECLS

/* GedaFileChooser is a widget that displays a file chooser dialog */

#define GEDA_TYPE_FILE_CHOOSER            (geda_file_chooser_get_type ())
#define GEDA_FILE_CHOOSER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_FILE_CHOOSER, GedaFileChooser))
#define GEDA_FILE_CHOOSER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),   GEDA_TYPE_FILE_CHOOSER, GedaFileChooserClass))
#define GEDA_IS_FILE_CHOOSER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEDA_TYPE_FILE_CHOOSER))
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

GedaType      geda_file_chooser_get_type         (void) G_GNUC_CONST;
GtkWidget    *geda_file_chooser_new              (GtkWidget         *parent,
                                                  FileChooserAction  action);
GtkWidget    *geda_file_chooser_dialog_new_full  (const char       *title,
                                                  GtkWindow        *parent,
                                                  FileChooserAction action,
                                                  const char       *first_button_text,
                                                  ...);

GtkEntry     *geda_file_chooser_get_entry          (GtkWidget *chooser);
char         *geda_file_chooser_get_entry_text     (GtkWidget *chooser);

int           geda_file_chooser_get_filter         (GtkWidget *chooser);
void          geda_file_chooser_set_filter         (GtkWidget *chooser, int value);

char         *geda_file_chooser_get_filename       (GtkWidget *chooser);
void          geda_file_chooser_set_filename       (GtkWidget *chooser, const char *name);

GSList       *geda_file_chooser_get_filenames      (GtkWidget *chooser);

char         *geda_file_chooser_get_current_folder (GtkWidget *chooser);
void          geda_file_chooser_set_current_folder (GtkWidget *chooser, const char *folder);

void          geda_file_chooser_set_current_name   (GtkWidget *chooser, const char *folder);

GtkWidget    *geda_file_chooser_get_extra_widget   (GtkWidget *chooser);
void          geda_file_chooser_set_extra_widget   (GtkWidget *chooser, GtkWidget *extra);




G_END_DECLS

#endif /* __GEDA_FILE_CHOOSER_H__ */
