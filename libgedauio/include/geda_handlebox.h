/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_handlebox.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2015 by Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */

#ifndef __GEDA_HANDLE_BOX_H__
#define __GEDA_HANDLE_BOX_H__

#include <gtk/gtkbin.h>

G_BEGIN_DECLS

#define GEDA_TYPE_HANDLE_BOX            (geda_handle_box_get_type ())
#define GEDA_HANDLE_BOX(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_HANDLE_BOX, GedaHandleBox))
#define GEDA_HANDLE_BOX_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_HANDLE_BOX, GedaHandleBoxClass))
#define GEDA_IS_HANDLE_BOX(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEDA_TYPE_HANDLE_BOX))
#define GEDA_IS_HANDLE_BOX_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_HANDLE_BOX))
#define GEDA_HANDLE_BOX_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_HANDLE_BOX, GedaHandleBoxClass))


typedef struct _GedaHandleBox       GedaHandleBox;
typedef struct _GedaHandleBoxClass  GedaHandleBoxClass;

struct _GedaHandleBox
{
  GtkBin bin;

  GdkWindow     *bin_window;	/* parent window for children */
  GdkWindow     *float_window;
  GtkShadowType  shadow_type;
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

  GtkOrientation  dock_orientation;

  GtkAllocation   attach_allocation;
  GtkAllocation   float_allocation;
};

struct _GedaHandleBoxClass
{
  GtkBinClass parent_class;

  void (*child_attached) (GedaHandleBox **handlebox, GtkWidget *child);
  void (*child_detached) (GedaHandleBox **handlebox, GtkWidget *child);

};

GedaType        geda_handle_box_get_type             (void) GEDA_CONST;
GtkWidget*      geda_handle_box_new                  (void);
void            geda_handle_box_dock                 (GedaHandleBox *handlebox);
void            geda_handle_box_set_shadow_type      (GedaHandleBox *handlebox, GtkShadowType    type);
GtkShadowType   geda_handle_box_get_shadow_type      (GedaHandleBox *handlebox);
void            geda_handle_box_set_handle_position  (GedaHandleBox *handlebox, GtkPositionType  position);
GtkPositionType geda_handle_box_get_handle_position  (GedaHandleBox *handlebox);
void            geda_handle_box_set_snap_edge        (GedaHandleBox *handlebox, GtkPositionType  edge);
GtkPositionType geda_handle_box_get_snap_edge        (GedaHandleBox *handlebox);
bool            geda_handle_box_get_child_detached   (GedaHandleBox *handlebox);

G_END_DECLS

#endif  /* __GEDA_HANDLE_BOX_H__ */
