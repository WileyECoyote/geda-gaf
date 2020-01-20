/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_font_button.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2018 Wiley Edward Hill
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

#ifndef __GEDA_FONT_BUTTON_H__
#define __GEDA_FONT_BUTTON_H__

#if (GTK_MAJOR_VERSION < 3) && !defined GTK_DISABLE_SINGLE_INCLUDES

#include <gtk/gtkbutton.h>

#else

#include <gtk/gtk.h>

#endif

#ifndef DEFAULT_FONT_NAME
#define DEFAULT_FONT_NAME "ARIAL 10"
#endif
#ifndef DEFAULT_FONT_SIZE
#define DEFAULT_FONT_SIZE 10
#define MIN_FONT_SIZE      6
#define MAX_FONT_SIZE    256
#endif

#define DEFAULT_PREVIEW_TEXT _("abcdefghijk ABCDEFGHIJK")

/* GedaFontButton is a button widget that allow user to select font properties. */

#define GEDA_TYPE_FONT_BUTTON             (geda_font_button_get_type ())
#define GEDA_FONT_BUTTON(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_FONT_BUTTON, GedaFontButton))
#define GEDA_FONT_BUTTON_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_FONT_BUTTON, GedaFontButtonClass))
#define GEDA_IS_FONT_BUTTON(obj)          (is_a_geda_font_button((GedaFontButton*)(obj)))
#define GEDA_IS_FONT_BUTTON_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_FONT_BUTTON))
#define GEDA_FONT_BUTTON_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_FONT_BUTTON, GedaFontButtonClass))

typedef struct _GedaFontButton      GedaFontButton;
typedef struct _GedaFontButtonClass GedaFontButtonClass;
typedef struct _GedaFontButtonData  GedaFontButtonData;

struct _GedaFontButton {

  GtkButton button;

  /*< private >*/
  GedaFontButtonData    *priv;

  PangoFontDescription  *font_desc;
  PangoFontFace         *font_face;

  char *title;

};

struct _GedaFontButtonClass {
  GtkButtonClass parent_class;

  /* font_set signal is emitted when font is chosen */
  void (* font_set ) (GedaFontButton *gfp);
  void (* size_set ) (GedaFontButton *gfp);
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType    geda_font_button_get_type         (void) GEDA_CONST;
bool        is_a_geda_font_button             (GedaFontButton *font_button);

GtkWidget  *geda_font_button_new              (void);
GtkWidget  *geda_font_button_new_with_font    (const char     *fontname);

const char *geda_font_button_get_title        (GedaFontButton *font_button);
void        geda_font_button_set_title        (GedaFontButton *font_button,
                                               const char     *title);
bool        geda_font_button_get_use_font     (GedaFontButton *font_button);
void        geda_font_button_set_use_font     (GedaFontButton *font_button,
                                               bool            use_font);
const char *geda_font_button_get_font_name    (GedaFontButton *font_button);
bool        geda_font_button_set_font_name    (GedaFontButton *font_button,
                                               const char     *fontname);
bool        geda_font_button_get_use_size     (GedaFontButton *font_button);
void        geda_font_button_set_use_size     (GedaFontButton *font_button,
                                               bool            use_size);
bool        geda_font_button_get_show_size    (GedaFontButton *font_button);
void        geda_font_button_set_show_size    (GedaFontButton *font_button,
                                               bool            show_size);
char       *geda_font_button_get_ascii_size   (GedaFontButton *font_button);
int         geda_font_button_get_size         (GedaFontButton *font_button);
void        geda_font_button_set_size         (GedaFontButton *font_button,
                                               int             font_size);
bool        geda_font_button_get_show_style   (GedaFontButton *font_button);
void        geda_font_button_set_show_style   (GedaFontButton *font_button,
                                               bool            show_style);
bool        geda_font_button_get_show_preview (GedaFontButton *font_button);
void        geda_font_button_set_show_preview (GedaFontButton *font_button,
                                               bool            enable);
const char *geda_font_button_get_preview_text (GedaFontButton *font_button);
void        geda_font_button_set_preview_text (GedaFontButton *font_button,
                                               const char     *text);

const PangoFontDescription *geda_font_button_get_font_desc (GedaFontButton *gfp);
void  geda_font_button_set_font_desc (GedaFontButton *font_button,
                                      PangoFontDescription * pfd);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_FONT_BUTTON_H__ */
