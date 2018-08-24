/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_font_dialog.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2018 by Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2013-2018 gEDA Contributors (see ChangeLog for details)
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
 */

#ifndef DEFAULT_FONT_NAME
#define DEFAULT_FONT_NAME "ARIAL 10"
#endif
#ifndef DEFAULT_FONT_SIZE
#define DEFAULT_FONT_SIZE 10
#endif

#define PREVIEW_TEXT _("abcdefghijk ABCDEFGHIJK")

/* This is the initial and maximum height of the preview entry (it expands
   when large font sizes are selected). Initial height is also the minimum. */
#define INITIAL_PREVIEW_HEIGHT 44
#define MAX_PREVIEW_HEIGHT 300

/* These are the sizes of the font, style & size lists. */
#define FONT_LIST_HEIGHT 136
#define FONT_LIST_WIDTH  190
#define FONT_STYLE_LIST_WIDTH 170
#define FONT_SIZE_LIST_WIDTH 60

#ifndef __GEDA_FONT_DIALOG_H__
#define __GEDA_FONT_DIALOG_H__

#define GEDA_TYPE_FONT_DIALOG            (geda_font_dialog_get_type ())
#define GEDA_FONT_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_FONT_DIALOG, GedaFontDialog))
#define GEDA_FONT_DIALOG_CLASS(class)    (G_TYPE_CHECK_CLASS_CAST ((class),  GEDA_TYPE_FONT_DIALOG, GedaFontDialogClass))
#define GEDA_IS_FONT_DIALOG(obj)         (is_a_geda_font_dialog((GedaFontDialog*)(obj)))
#define GEDA_IS_FONT_DIALOG_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class),  GEDA_TYPE_FONT_DIALOG))
#define GEDA_FONT_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_FONT_DIALOG, GedaFontDialogClass))

typedef struct _GedaFontDialog       GedaFontDialog;
typedef struct _GedaFontDialogClass  GedaFontDialogClass;

struct _GedaFontDialog
{
  GtkDialog parent_instance;

  GtkWidget *font_entry;
  GtkWidget *family_list;

  GtkWidget *style_entry;
  GtkWidget *style_list;

  GtkWidget *size_entry;
  GtkWidget *size_list;

  GtkWidget *preview_entry;

  GtkBox    *main_vbox;
  GtkBox    *action_area;

  /*< public >*/
  GtkWidget *ok_button;
  GtkWidget *cancel_button;

  unsigned long family_handler;
  unsigned long face_handler;
  unsigned long size_handler;
  unsigned long preview_handler;

  /* If the user changes the width of the dialog, we turn auto-shrink off.
   * (Unused now, autoshrink doesn't mean anything anymore -Yosh)
   */
  bool auto_resize;
  bool show_preview;

  int dialog_width;

  PangoFontDescription *font_desc;
  GdkFont              *font;      /* Cache for gdk_font_selection_get_font */
  PangoFontFamily      *family;    /* Current family */
  PangoFontFace        *face;      /* Current face */
  PangoFontMap         *font_map;
  char                 *default_font;
  int                   font_size;

};

struct _GedaFontDialogClass
{
  GtkDialogClass parent_class;
};

#define PFD PangoFontDescription
#define PFY PangoFontFamily
#define PFF PangoFontFace

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_font_dialog_get_type           (void) GEDA_CONST;
bool       is_a_geda_font_dialog               (GedaFontDialog *font_dialog);

GtkWidget  *geda_font_dialog_new                (void);
GtkWidget  *geda_font_dialog_new_with_font_name (const char     *font_name);
GtkWidget  *geda_font_dialog_new_with_title     (const char     *title);
GtkWidget  *geda_font_dialog_new_with_window    (const char     *title,
                                                 GtkWindow      *parent);

GdkFont    *geda_font_dialog_get_font           (GedaFontDialog *dialog);
bool        geda_font_dialog_set_font           (GedaFontDialog *dialog,
                                                 GdkFont        *font);

PFD        *geda_font_dialog_get_font_desc      (GedaFontDialog *dialog);
void        geda_font_dialog_set_font_desc      (GedaFontDialog *dialog,
                                                 const PFD      *font_desc);

char       *geda_font_dialog_get_font_name      (GedaFontDialog *dialog);
bool        geda_font_dialog_set_font_name      (GedaFontDialog *dialog,
                                                 const char     *fontname);

const PFY  *geda_font_dialog_get_font_family    (GedaFontDialog *dialog);
const PFF  *geda_font_dialog_get_font_face      (GedaFontDialog *dialog);

int         geda_font_dialog_get_font_size      (GedaFontDialog *dialog);
void        geda_font_dialog_set_font_size      (GedaFontDialog *dialog,
                                                 int             new_size);

const char *geda_font_dialog_get_preview_text   (GedaFontDialog *dialog);
bool        geda_font_dialog_set_preview_text   (GedaFontDialog *dialog,
                                                 const char     *text);

bool        geda_font_dialog_get_show_preview   (GedaFontDialog *dialog);
void        geda_font_dialog_set_show_preview   (GedaFontDialog *dialog,
                                                 bool            show_preview);

#define geda_font_dialog_set_title(d, t) gtk_window_set_title ((GtkWindow*)d, t)
#define geda_font_dialog_get_title(d)    gtk_window_get_title ((GtkWindow*)d)

#ifdef __cplusplus
}
#endif /* __cplusplus */

#undef PFD
#undef PFY
#undef PFF

#endif /* __GEDA_FONT_DIALOG_H__ */
