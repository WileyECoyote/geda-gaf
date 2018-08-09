/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_image_chooser.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2018 Wiley Edward Hill
 * Copyright (C) 2014-2018 gEDA Contributors (see ChangeLog for details)
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
 * Date: December, 20, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_IMAGE_CHOOSER_H__
#define __GEDA_IMAGE_CHOOSER_H__

#include <geda_uio_macros.h> /* For convenience of includer, not geda_image_chooser.c */

#define IMAGE_CHOOSER_CONFIG_GROUP   "image-chooser"
#define IMAGE_CHOOSER_CONFIG_FILTER  "image-filter"
#define IMAGE_CHOOSER_CONFIG_PREVIEW "preview-enable"
#define IMAGE_CHOOSER_CONFIG_PVSIZE  "preview-size"

#define DEFAULT_CHOOSER_PREVIEW_SIZE 300
#define MIN_CHOOSER_PREVIEW_SIZE     100
#define MAX_CHOOSER_PREVIEW_SIZE     1000

/*!
 * ImageChooserAction:
 * #IMAGE_CHOOSER_ACTION_OPEN: Indicates open mode.
 * The file chooser will only let the user pick an existing file.
 * #IMAGE_CHOOSER_ACTION_SAVE: Indicates save mode.
 * The file chooser will let the user pick an existing file, or type in a new
 * filename.
 * #IMAGE_CHOOSER_ACTION_SELECT_FOLDER: Indicates select folder mode.
 * The file chooser will let the user pick an existing folder.
 * #IMAGE_CHOOSER_ACTION_CREATE_FOLDER: Indicates create folder mode.
 * The file chooser will let the user name an existing or new folder.
 *
 * Describes whether a #GedaImageChooser is being used to open existing files
 * or to save to a possibly new file.
 */
typedef enum
{
  IMAGE_CHOOSER_ACTION_OPEN,
  IMAGE_CHOOSER_ACTION_SAVE,
  IMAGE_CHOOSER_ACTION_SELECT_FOLDER,
  IMAGE_CHOOSER_ACTION_CREATE_FOLDER
} ImageChooserAction;

/* GedaImageChooser is a widget that displays a file chooser
 * dialog with filters for various image type support by gEDA */

#define GEDA_TYPE_IMAGE_CHOOSER            (geda_image_chooser_get_type ())
#define GEDA_IMAGE_CHOOSER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_IMAGE_CHOOSER, GedaImageChooser))
#define GEDA_IMAGE_CHOOSER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass),   GEDA_TYPE_IMAGE_CHOOSER, GedaImageChooserClass))
#define GEDA_IS_IMAGE_CHOOSER(obj)         (is_a_geda_image_chooser((GedaImageChooser*)(obj)))
#define GEDA_IS_IMAGE_CHOOSER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_IMAGE_CHOOSER))
#define GEDA_IMAGE_CHOOSER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_IMAGE_CHOOSER, GedaImageChooserClass))

typedef struct _GedaImageChooser        GedaImageChooser;
typedef struct _GedaImageChooserClass   GedaImageChooserClass;

struct _GedaImageChooser {

  GtkFileChooserDialog  parent;
  GtkAdjustment        *adjustment;
  GtkWidget            *extra;
  GtkWidget            *slider;
  GtkWidget            *popup;
  GtkWidget            *preview;
  GtkWidget            *preview_chechbox;
  GtkWidget            *filter_button;
           int          filter_index;
  unsigned long         handler;
           int          default_preview_size;
           int          max_preview_size;
           int          min_preview_size;
           int          preview_size;
           int          previous_size;        /* New flag in chooser_adjust_size */
           bool         preview_enabled;
           bool         zoom_mode;
           int          zoom_save;
           bool         mouse_down;
};

struct _GedaImageChooserClass {

  GtkFileChooserDialogClass  parent_class;

  /* Action signals */
  void (* filter_changed)   (GedaImageChooser *chooser);
  void (* geometry_save)    (GedaImageChooser *chooser, char *group);
  void (* geometry_restore) (GedaImageChooser *chooser, char *group);
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType      geda_image_chooser_get_type        (void) GEDA_CONST;
bool          is_a_geda_image_chooser            (GedaImageChooser  *chooser);

GtkWidget    *geda_image_chooser_new             (void              *parent,
                                                  ImageChooserAction action);
GtkWidget    *geda_image_chooser_dialog_new_full (const char        *title,
                                                  void              *parent,
                                                  ImageChooserAction action,
                                                  const char        *first_button_text,
                                                  ...);

GtkEntry     *geda_image_chooser_get_entry          (GtkWidget *chooser);
char         *geda_image_chooser_get_entry_text     (GtkWidget *chooser);

int           geda_image_chooser_get_filter         (GtkWidget *chooser);
void          geda_image_chooser_set_filter         (GtkWidget *chooser, int value);

char         *geda_image_chooser_get_filename       (GtkWidget *chooser);
void          geda_image_chooser_set_filename       (GtkWidget *chooser, const char *name);

GSList       *geda_image_chooser_get_filenames      (GtkWidget *chooser);

char         *geda_image_chooser_get_current_folder (GtkWidget *chooser);
void          geda_image_chooser_set_current_folder (GtkWidget *chooser, const char *folder);

void          geda_image_chooser_set_current_name   (GtkWidget *chooser, const char *folder);

void          geda_image_chooser_append_extra       (GtkWidget *chooser, GtkWidget *widget);
void          geda_image_chooser_prepend_extra      (GtkWidget *dialog, GtkWidget *widget);

GtkWidget    *geda_image_chooser_get_extra_widget   (GtkWidget *chooser);
void          geda_image_chooser_set_extra_widget   (GtkWidget *chooser, GtkWidget *extra);

void          geda_image_chooser_set_preview_active (GtkWidget *chooser, bool state);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_IMAGE_CHOOSER_H__ */
