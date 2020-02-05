/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_handlebox.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2018 by Wiley Edward Hill <wileyhill@gmail.com>
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

#ifndef __GEDA_HANDLE_BOX_H__
#define __GEDA_HANDLE_BOX_H__

#if (GTK_MAJOR_VERSION < 3) && !defined GTK_DISABLE_SINGLE_INCLUDES

#include <gtk/gtkbin.h>

#else

#include <gtk/gtk.h>

#endif

#define GEDA_TYPE_HANDLE_BOX            (geda_handle_box_get_type ())
#define GEDA_HANDLE_BOX(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_HANDLE_BOX, GedaHandleBox))
#define GEDA_HANDLE_BOX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_HANDLE_BOX, GedaHandleBoxClass))
#define GEDA_IS_HANDLE_BOX(obj)         (is_a_geda_handle_box((GedaHandleBox*)(obj)))
#define GEDA_IS_HANDLE_BOX_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_HANDLE_BOX))
#define GEDA_HANDLE_BOX_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_HANDLE_BOX, GedaHandleBoxClass))

typedef struct _GedaHandleBox       GedaHandleBox;
typedef struct _GedaHandleBoxClass  GedaHandleBoxClass;
typedef struct _GedaHandleBoxData   GedaHandleBoxData;

struct _GedaHandleBox
{
  GtkBin bin;

  GdkWindow     *bin_window;	/* parent window for children */
  GdkWindow     *float_window;
  GtkShadowType  shadow_type;
  int            handle_size;
  unsigned int   handle_position : 2;
  unsigned int   float_window_mapped : 1;
  unsigned int   child_detached : 1;
  unsigned int   in_drag : 1;
  unsigned int   shrink_on_detach : 1;

  signed int     snap_edge : 3; /* -1 == unset */

  /* Variables used during a drag
   */
  int            deskoff_x; /* Offset between root relative coords */
  int            deskoff_y; /* and deskrelative coords             */

  GtkOrientation dock_orientation;

  GtkAllocation  attach_allocation;
  GtkAllocation  float_allocation;

  GedaHandleBoxData *priv;
};

struct _GedaHandleBoxClass
{
  GtkBinClass parent_class;

  void (*child_attached) (GedaHandleBox **handlebox, GtkWidget *child);
  void (*child_detached) (GedaHandleBox **handlebox, GtkWidget *child);

};

#ifdef __cplusplus
extern "C" {
#endif

GedaType        geda_handle_box_get_type             (void) GEDA_CONST;
bool            is_a_geda_handle_box                 (GedaHandleBox *handlebox);

GtkWidget      *geda_handle_box_new                  (void);
void            geda_handle_box_dock                 (GedaHandleBox *handlebox);
bool            geda_handle_box_get_child_detached   (GedaHandleBox *handlebox);
void            geda_handle_box_set_handle_position  (GedaHandleBox *handlebox, GtkPositionType position);
GtkPositionType geda_handle_box_get_handle_position  (GedaHandleBox *handlebox);
void            geda_handle_box_set_shadow_type      (GedaHandleBox *handlebox, GtkShadowType type);
GtkShadowType   geda_handle_box_get_shadow_type      (GedaHandleBox *handlebox);
void            geda_handle_box_set_shrink_on_detach (GedaHandleBox *handlebox, bool shrink);
bool            geda_handle_box_get_shrink_on_detach (GedaHandleBox *handlebox);
void            geda_handle_box_set_snap_edge        (GedaHandleBox *handlebox, GtkPositionType edge);
GtkPositionType geda_handle_box_get_snap_edge        (GedaHandleBox *handlebox);
GtkToolbar     *geda_handle_box_get_toolbar          (GedaHandleBox *handlebox);
void            geda_handle_box_set_toolbar          (GedaHandleBox *handlebox, GtkWidget *toolbar);

void            geda_handle_widget_set_handle_position  (GtkWidget *widget, GtkPositionType  position);
void            geda_handle_widget_set_shadow_type      (GtkWidget *widget, GtkShadowType    type);
void            geda_handle_widget_set_shrink_on_detach (GtkWidget *widget, bool shrink);
void            geda_handle_widget_set_snap_edge        (GtkWidget *widget, GtkPositionType  edge);
void            geda_handle_widget_set_toolbar          (GtkWidget *widget, GtkWidget *toolbar);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif  /* __GEDA_HANDLE_BOX_H__ */
