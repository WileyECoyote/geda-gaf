/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_toolbar.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016-2018 gEDA Contributors (see ChangeLog for details)
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
 * Date: May 27th, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_TOOLBAR_H__
#define __GEDA_TOOLBAR_H__

#define GEDA_TYPE_TOOLBAR            (geda_toolbar_get_type ())
#define GEDA_TOOLBAR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_TOOLBAR, GedaToolbar))
#define GEDA_TOOLBAR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_TOOLBAR, GedaToolbarClass))
#define GEDA_IS_TOOLBAR(obj)         (is_a_geda_toolbar((GedaToolbar*)(obj)))
#define GEDA_IS_TOOLBAR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_TOOLBAR))
#define GEDA_TOOLBAR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_TOOLBAR, GedaToolbarClass))

typedef enum
{
  GEDA_TOOLBAR_CHILD_SPACE,
  GEDA_TOOLBAR_CHILD_BUTTON,
  GEDA_TOOLBAR_CHILD_TOGGLEBUTTON,
  GEDA_TOOLBAR_CHILD_RADIOBUTTON,
  GEDA_TOOLBAR_CHILD_WIDGET
} GedaToolbarChildType;

typedef struct _GedaToolbar       GedaToolbar;
typedef struct _GedaToolbarClass  GedaToolbarClass;

struct _GedaToolbar
{
  GtkToolbar parent;

  GList     *children;

  int        orientation;
};

struct _GedaToolbarClass
{
  GtkToolbarClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType        geda_toolbar_get_type            (void) GEDA_CONST;
bool            is_a_geda_toolbar                (GedaToolbar *toolbar);

GtkWidget      *geda_toolbar_new                 (int orientation);

/* Any element type */
GtkWidget      *geda_toolbar_append_element      (GedaToolbar         *toolbar,
                                                  GedaToolbarChildType type,
                                                  GtkWidget           *widget,
                                                  const char          *text,
                                                  const char          *tooltip_text,
                                                  const char          *tooltip_private_text,
                                                  GtkWidget           *icon,
                                                  GCallback            callback,
                                                  void                *user_data);

/* Simple button items */
GtkWidget      *geda_toolbar_append_item         (GedaToolbar     *toolbar,
                                                  const char      *text,
                                                  const char      *tooltip_text,
                                                  const char      *tooltip_private_text,
                                                  GtkWidget       *icon,
                                                  GCallback        callback,
                                                  void            *user_data);

/* Generic Widgets */
void            geda_toolbar_append_widget       (GedaToolbar     *toolbar,
                                                  GtkWidget       *widget,
                                                  const char      *tooltip_text,
                                                  const char      *tooltip_private_text);

int             geda_toolbar_get_orientation     (GedaToolbar     *toolbar);
void            geda_toolbar_set_orientation     (GedaToolbar     *toolbar,
                                                  int              orientation);

GtkToolbarStyle geda_toolbar_get_style           (GedaToolbar     *toolbar);
void            geda_toolbar_set_style           (GedaToolbar     *toolbar,
                                                  GtkToolbarStyle  style);

bool            geda_toolbar_get_tooltips        (GedaToolbar     *toolbar);
void            geda_toolbar_set_tooltips        (GedaToolbar     *toolbar,
                                                  bool             enable);

/* ---------- inline Widget version ---------- */

inline bool
geda_toolbar_widget_get_tooltips (GtkWidget *toolbar)
{
  return geda_toolbar_get_tooltips((GedaToolbar*)toolbar);
}

inline void
geda_toolbar_widget_set_tooltips (GtkWidget *toolbar, bool enable)
{
  geda_toolbar_set_tooltips((GedaToolbar*)toolbar, enable);
}

inline GtkToolbarStyle
geda_toolbar_widget_get_style (GtkWidget *toolbar)
{
  return geda_toolbar_get_style((GedaToolbar*)toolbar);
}

inline void
geda_toolbar_widget_set_style (GtkWidget *toolbar, GtkToolbarStyle  style)
{
  geda_toolbar_set_style((GedaToolbar*)toolbar, style);
}


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_TOOLBAR_H__ */
