/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
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
 */

#ifndef __GSCHEM_FONT_BUTTON_H__
#define __GSCHEM_FONT_BUTTON_H__

#include <gtk/gtkbutton.h>

#ifndef DEFAULT_FONT_NAME
#define DEFAULT_FONT_NAME "ARIAL 10"
#endif
#ifndef DEFAULT_FONT_SIZE
#define DEFAULT_FONT_SIZE 10
#define MIN_FONT_SIZE      6
#define MAX_FONT_SIZE    256
#endif

#define DEFAULT_PREVIEW_TEXT _("abcdefghijk ABCDEFGHIJK")

G_BEGIN_DECLS

/* GschemFontButton is a button widget that allow user to select font properties. */

#define GSCHEM_TYPE_FONT_BUTTON             (gschem_font_button_get_type ())
#define GSCHEM_FONT_BUTTON(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_FONT_BUTTON, GschemFontButton))
#define GSCHEM_FONT_BUTTON_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_FONT_BUTTON, GschemFontButtonClass))
#define GSCHEM_IS_FONT_BUTTON(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_FONT_BUTTON))
#define GSCHEM_IS_FONT_BUTTON_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass),  GSCHEM_TYPE_FONT_BUTTON))
#define GSCHEM_FONT_BUTTON_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_FONT_BUTTON, GschemFontButtonClass))

typedef struct _GschemFontButton        GschemFontButton;
typedef struct _GschemFontButtonClass   GschemFontButtonClass;
typedef struct _GschemFontButtonPrivate GschemFontButtonPrivate;

struct _GschemFontButton {

  GtkButton button;

  /*< private >*/
  GschemFontButtonPrivate *priv;

  PangoFontDescription    *font_desc;
  PangoFontFace           *font_face;

  char *title;

};

struct _GschemFontButtonClass {
  GtkButtonClass parent_class;

  /* font_set signal is emitted when font is chosen */
  void (* font_set ) (GschemFontButton *gfp);
  void (* size_set ) (GschemFontButton *gfp);
};

GType         gschem_font_button_get_type       (void) G_GNUC_CONST;
GtkWidget    *gschem_font_button_new            (void);
GtkWidget    *gschem_font_button_new_with_font  (const char       *fontname);

const char   *gschem_font_button_get_title      (GschemFontButton *font_button);
void          gschem_font_button_set_title      (GschemFontButton *font_button,
                                                  const char       *title);
bool          gschem_font_button_get_use_font   (GschemFontButton *font_button);
void          gschem_font_button_set_use_font   (GschemFontButton *font_button,
                                                 bool              use_font);
const char   *gschem_font_button_get_font_name  (GschemFontButton *font_button);
bool          gschem_font_button_set_font_name  (GschemFontButton *font_button,
                                                 const char       *fontname);
bool          gschem_font_button_get_use_size   (GschemFontButton *font_button);
void          gschem_font_button_set_use_size   (GschemFontButton *font_button,
                                                 bool              use_size);
bool          gschem_font_button_get_show_size  (GschemFontButton *font_button);
void          gschem_font_button_set_show_size  (GschemFontButton *font_button,
                                                 bool              show_size);
char         *gschem_font_button_get_ascii_size (GschemFontButton *font_button);
int           gschem_font_button_get_size       (GschemFontButton *font_button);
void          gschem_font_button_set_size       (GschemFontButton *font_button,
                                                 int               font_size);
bool          gschem_font_button_get_show_style (GschemFontButton *font_button);
void          gschem_font_button_set_show_style (GschemFontButton *font_button,
                                                 bool              show_style);
bool        gschem_font_button_get_show_preview (GschemFontButton *font_button);
void        gschem_font_button_set_show_preview (GschemFontButton *font_button,
                                                 bool              enable);
const char* gschem_font_button_get_preview_text (GschemFontButton *font_button);
void        gschem_font_button_set_preview_text (GschemFontButton *font_button,
                                                 const char *text);

const PangoFontDescription *gschem_font_button_get_font_desc (GschemFontButton *gfp);
void  gschem_font_button_set_font_desc (GschemFontButton *font_button,
                                        PangoFontDescription * pfd);

G_END_DECLS

#endif /* __GSCHEM_FONT_BUTTON_H__ */
