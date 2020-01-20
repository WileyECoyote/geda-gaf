/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_dockbox.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2018 by Wiley Edward Hill <wileyhill@gmail.com>
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

#ifndef __GEDA_DOCK_BOX_H__
#define __GEDA_DOCK_BOX_H__

#if (GTK_MAJOR_VERSION < 3) && !defined GTK_DISABLE_SINGLE_INCLUDES

#include <gtk/gtkbox.h>

#else

#include <gtk/gtk.h>

#endif

#define GEDA_TYPE_DOCK_BOX            (geda_dock_box_get_type ())
#define GEDA_DOCK_BOX(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_DOCK_BOX, GedaDockBox))
#define GEDA_DOCK_BOX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_DOCK_BOX, GedaDockBoxClass))
#define GEDA_IS_DOCK_BOX(obj)         (is_a_geda_dock_box((GedaDockBox*)(obj)))
#define GEDA_IS_DOCK_BOX_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_DOCK_BOX))
#define GEDA_DOCK_BOX_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_DOCK_BOX, GedaDockBoxClass))

typedef struct _GedaDockBox       GedaDockBox;
typedef struct _GedaDockBoxClass  GedaDockBoxClass;

struct _GedaDockBox
{
  GtkBox  box;
  GList  *docks;        /* List of boxes */

  int orientation;      /* orientation of children */
};

struct _GedaDockBoxClass
{
  GtkBoxClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType        geda_dock_box_get_type               (void) GEDA_CONST;
bool            is_a_geda_dock_box                   (GedaDockBox *dockbox);

GtkWidget      *geda_dock_box_new                    (int orientation);

void            geda_dock_box_add                    (GedaDockBox *dockbox,
                                                      GtkWidget   *child,
                                                      int          location);

void            geda_dock_widget_add                 (GtkWidget   *dockbox,
                                                      GtkWidget   *child,
                                                      int          location);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif  /* __GEDA_DOCK_BOX_H__ */
