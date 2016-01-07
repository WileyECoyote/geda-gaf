/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_gtk_compat.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
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
 *
 * Date: December 29, 2015
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 * Includes macros from gtkextra-compat.h by:
 *
 * Adrian E. Feiguin <feiguin@ifir.edu.ar>
 *
 * The file is used to implement work-arounds for compatibility
 * between gtk versions and geda code.
 */

#ifndef __GEDA_GTK_COMPAT_H__
#define __GEDA_GTK_COMPAT_H__

#include <gtk/gtk.h>

#if GTK_MAJOR_VERSION < 3

#if !GTK_CHECK_VERSION(2,20,0)

    /* before V2.20 */

#  define gtk_widget_get_realized GTK_WIDGET_REALIZED
#  define gtk_widget_get_mapped GTK_WIDGET_MAPPED

#  define gtk_widget_get_requisition(widget, requisitionptr) \
        *(requisitionptr) = GTK_WIDGET(widget)->requisition

#   define gtk_widget_set_realized_true(widget)  \
        GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED)
#   define gtk_widget_set_realized_false(widget)  \
        GTK_WIDGET_UNSET_FLAGS(widget, GTK_REALIZED)

#   define gtk_widget_set_mapped_true(widget)  \
        GTK_WIDGET_SET_FLAGS(widget, GTK_MAPPED)
#   define gtk_widget_set_mapped_false(widget)  \
        GTK_WIDGET_UNSET_FLAGS(widget, GTK_MAPPED)

#else

    /* from V2.20 */

#   define gtk_widget_set_realized_true(widget)  \
		gtk_widget_set_realized(widget, TRUE)
#   define gtk_widget_set_realized_false(widget)  \
		gtk_widget_set_realized(widget, FALSE)

#   define gtk_widget_set_mapped_true(widget)  \
		gtk_widget_set_mapped(widget, TRUE)
#   define gtk_widget_set_mapped_false(widget)  \
		gtk_widget_set_mapped(widget, FALSE)

#endif

/*! \def gtk_widget_get_allocated_height Not in Gtk < 3 */
#define gtk_widget_get_allocated_height(widget) (((GtkWidget *) (widget))->allocation.height)

/*! \def gtk_widget_get_allocated_width Not in Gtk < 3 */
#define gtk_widget_get_allocated_width(widget)  (((GtkWidget *) (widget))->allocation.width)

/*! \def geda_get_child_widget Get Child Bin widget Gtk < 3 */
#define geda_get_child_widget(w) GTK_BIN(w)->child

/*! \def geda_get_widget_allocation Get Pointer to Allocation */
#define geda_get_widget_allocation(w) &(GTK_WIDGET(w)->allocation)

#define geda_get_widget_requisition(w) &(GTK_WIDGET(w)->requisition)

#define geda_get_widget_window(w) GTK_WIDGET(w)->window

#else /* GTK >= 3 */

/*! \def geda_get_child_widget Get Child Bin widget Gtk >= 3*/
#define geda_get_child_widget(w) gtk_bin_get_child (GTK_BIN(w))

#define geda_get_widget_allocation(w) \
  ({ GtkAllocation a; gtk_widget_get_allocation (GTK_WIDGET(w), &a); &a; })

#define geda_get_widget_requisition(w) \
  ({ GtkRequisition r; gtk_widget_get_preferred_size (GTK_WIDGET(w), NULL, &r); &r; })

#define geda_get_widget_window(w) gtk_widget_get_window (GTK_WIDGET(w))

/* Gtk[VH]Box */
#   define geda_compat_box_new(orientation, homogeneous, spacing) \
        g_object_new(GTK_TYPE_BOX, \
                     "orientation", (orientation), \
                     "homogeneous", (homogeneous), \
                     "spacing", (spacing), \
                     NULL)

#   define gtk_vbox_new(homogeneous, spacing) \
        geda_compat_box_new(GTK_ORIENTATION_VERTICAL, (homogeneous), (spacing))

#   define gtk_hbox_new(homogeneous, spacing) \
        geda_compat_box_new(GTK_ORIENTATION_HORIZONTAL, (homogeneous), (spacing))

/* Gtk[VH]ButtonBox */
#   define gtk_vbutton_box_new()    gtk_button_box_new(GTK_ORIENTATION_VERTICAL)
#   define gtk_hbutton_box_new()    gtk_button_box_new(GTK_ORIENTATION_HORIZONTAL)

/* Gtk[VH]Separator */
#   define gtk_vseparator_new() gtk_separator_new(GTK_ORIENTATION_VERTICAL)
#   define gtk_hseparator_new() gtk_separator_new(GTK_ORIENTATION_HORIZONTAL)

/* Gtk[VH]Paned */
#   define gtk_vpaned_new() gtk_paned_new(GTK_ORIENTATION_VERTICAL)
#   define gtk_hpaned_new() gtk_paned_new(GTK_ORIENTATION_HORIZONTAL)

/* Gtk[VH]Scrollbar */
#   define gtk_vscrollbar_new(adj)  gtk_scrollbar_new(GTK_ORIENTATION_VERTICAL, (adj))
#   define gtk_hscrollbar_new(adj)  gtk_scrollbar_new(GTK_ORIENTATION_HORIZONTAL, (adj))

#endif

#endif /* __GEDA_GTK_COMPAT_H__ */
