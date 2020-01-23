/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_gtk_compat.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2018 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2018 gEDA Contributors (see ChangeLog for details)
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

/*! \def geda_adjustment_new Gtk < 3 oddly returns a GtkObject */
#define geda_adjustment_new (void*)gtk_adjustment_new

#ifndef GtkTooltips
#define GtkTooltips void
#endif

#ifndef GtkNotebookPage
#define GtkNotebookPage GtkWidget
#endif

#ifndef gdk_cursor_destroy
#define gdk_cursor_destroy gdk_cursor_unref
#endif

#if !GTK_CHECK_VERSION(3, 0, 0)

/* Map GtkStateFlags to GtkStateType */
#ifndef GTK_STATE_FLAG_NORMAL
#define GTK_STATE_FLAG_NORMAL GTK_STATE_NORMAL
#endif
#ifndef GTK_STATE_FLAG_ACTIVE
#define GTK_STATE_FLAG_ACTIVE GTK_STATE_ACTIVE
#endif
#ifndef GTK_STATE_FLAG_PRELIGHT
#define GTK_STATE_FLAG_PRELIGHT GTK_STATE_PRELIGHT
#endif

#ifndef GTK_STATE_FLAG_SELECTED
#define GTK_STATE_FLAG_SELECTED GTK_STATE_SELECTED
#endif

#ifndef GTK_STATE_FLAG_INSENSITIVE
#define GTK_STATE_FLAG_INSENSITIVE GTK_STATE_INSENSITIVE

#endif /* End if !GTK_CHECK_VERSION(3, 0, 0) */

/* The remaining GtkStateFlags:
 *
 *      GTK_STATE_FLAG_INCONSISTENT = 1 << 4,
 *      GTK_STATE_FLAG_FOCUSED      = 1 << 5,
 *      GTK_STATE_FLAG_BACKDROP     = 1 << 6,
 *      GTK_STATE_FLAG_DIR_LTR      = 1 << 7,
 *      GTK_STATE_FLAG_DIR_RTL      = 1 << 8
 *
 * do not have a Gtk2 equivalent and should not be passed
 * to Gtk2 functions.
 */

#define gtk_box_new(orientation, spacing) (orientation == GTK_ORIENTATION_HORIZONTAL \
        ? gtk_hbox_new (FALSE, spacing)\
        : gtk_vbox_new (FALSE, spacing))
#endif

# define geda_compat_box_new(orientation, homogeneous, spacing) \
        g_object_new(GTK_TYPE_BOX, \
                     "orientation", (orientation), \
                     "homogeneous", (homogeneous), \
                     "spacing", (spacing), \
                     NULL)


#if (GTK_MAJOR_VERSION < 3) && !defined GSEAL_ENABLE

#ifndef HAVE_GTK_WINDOW_GROUP_GET_CURRENT_GRAB
#  ifndef __MINGW32__

static inline GtkWidget *
gtk_window_group_get_current_grab (GtkWindowGroup *window_group)
{
  if (!window_group->grabs)
    return NULL;
  return GTK_WIDGET(window_group->grabs->data);
}

#  endif
#endif

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

/*! \def gdk_screen_get_monitor_workarea Not avaliable until Gtk 3.4 */
#define gdk_screen_get_monitor_workarea gdk_screen_get_monitor_geometry

/*! \def gtk_widget_get_allocated_height Not in Gtk < 3 */
#define gtk_widget_get_allocated_height(widget) (((GtkWidget*) (widget))->allocation.height)

/*! \def gtk_widget_get_allocated_width Not in Gtk < 3 */
#define gtk_widget_get_allocated_width(widget) (((GtkWidget*) (widget))->allocation.width)

/*! \def gtk_dialog_get_action_area Not in Gtk < 3 */
#define gtk_dialog_get_action_area(dialog) (GTK_DIALOG(dialog)->action_area)

/*! \def gtk_dialog_get_content_area Not in Gtk < 3 */
#define gtk_dialog_get_content_area(dialog) (GTK_DIALOG(dialog)->vbox)

/*! \def gtk_widget_reset_style Not in Gtk < 3 */
#define gtk_widget_reset_style gtk_widget_reset_rc_styles

/*! \def geda_get_accel_group_is_locked Get GtkAccelGroup lock_count > 0 Gtk < 3*/
#define geda_get_accel_group_is_locked(grp) ((grp)->lock_count > 0)

/*! \def geda_get_child_widget Get Child Bin widget Gtk < 3 */
#define geda_get_child_widget(w) (void*)((GtkBin*)w)->child

/*! \def geda_get_focus_widget Get Child Focus widget Gtk < 3*/
#define geda_get_focus_widget(w) (void*)((GtkWindow*)w)->focus_widget

/*! \def geda_get_widget_allocation Get Pointer to Allocation  Gtk < 3 */
#define geda_get_widget_allocation(w) &(GTK_WIDGET(w)->allocation)

#define geda_get_widget_in_destruction(w) GTK_OBJECT_FLAGS (w) & GTK_IN_DESTRUCTION

/*! \def geda_get_widget_parent Get parent from widget Gtk < 3 */
#define geda_get_widget_parent(w) (void*)((GtkWidget*)w)->parent

/*! \def geda_get_widget_requisition Get Pointer to requisition  Gtk < 3 */
#define geda_get_widget_requisition(w) &(GTK_WIDGET(w)->requisition)

/*! \def geda_get_widget_style Get style from widget Gtk < 3 */
#define geda_get_widget_style(w) (void*)((GtkWidget*)w)->style

/*! \def geda_get_widget_window Get Pointer to window  Gtk < 3 */
#define geda_get_widget_window(w) GTK_WIDGET(w)->window

#define geda_device_grab_remove(w,p) gtk_grab_remove(GTK_WIDGET(w))

#define geda_get_container_border_width(w) ((GtkContainer*)w)->border_width

#define geda_toggle_button_get_active(tb) GTK_TOGGLE_BUTTON(tb)->active

#define geda_toggle_button_set_active(tb, a) GTK_TOGGLE_BUTTON(tb)->active = a

#else /* GTK >= 3 */

#define GtkObject GtkWidget

/*! \def geda_get_accel_group_is_locked Get GtkAccelGroup lock_count > 0 Gtk >= 3*/
#define geda_get_accel_group_is_locked(g) gtk_accel_group_get_is_locked ((GtkAccelGroup*)g)

/*! \def geda_get_child_widget Get Child Bin widget Gtk >= 3*/
#define geda_get_child_widget(w) (void*)gtk_bin_get_child ((GtkBin*)w)

/*! \def geda_get_focus_widget Get Child Focus widget Gtk >= 3*/
#define geda_get_focus_widget(w) (void*)gtk_window_get_focus ((GtkWindow*)w)

/*! \def geda_get_widget_allocation Get Pointer to Allocation  Gtk >= 3 */
#define geda_get_widget_allocation(w) \
  ({ GtkAllocation a; gtk_widget_get_allocation (GTK_WIDGET(w), &a); &a; })

#if defined (gtk_widget_in_destruction)
#define geda_get_widget_in_destruction(w) gtk_widget_in_destruction(GTK_WIDGET(w))
#else
#define geda_get_widget_in_destruction(w) FALSE
#endif

/*! \def geda_get_widget_parent Get parent from widget Gtk >= 3 */
#define geda_get_widget_parent(w) gtk_widget_get_parent (GTK_WIDGET(w))

/*! \def geda_get_widget_requisition Get Pointer to requisition  Gtk >= 3 */
#if defined (gtk_widget_get_preferred_size)
#define geda_get_widget_requisition(w) \
  ({ GtkRequisition r; gtk_widget_get_preferred_size (GTK_WIDGET(w), NULL, &r); &r; })
#else
/* Handle Gtk == 2 and GSEAL_ENABLE is defined */
#define geda_get_widget_requisition(w) \
  ({ GtkRequisition r; gtk_widget_get_requisition (GTK_WIDGET(w), &r); &r; })
#endif

#if (GTK_MAJOR_VERSION < 3)
/* Handle Gtk == 2 and GSEAL_ENABLE is defined */
/*! \def geda_get_widget_style Get style from widget Gtk < 3 */
#define geda_get_widget_style(w) (void*)gtk_widget_get_style((GtkWidget*)w)
#endif

/*! \def geda_get_widget_window Get Pointer to window  Gtk >= 3 */
#define geda_get_widget_window(w) gtk_widget_get_window (GTK_WIDGET(w))

/* Gtk[VH]Box */
#   define gtk_vbox_new(homogeneous, spacing) \
        geda_compat_box_new(GTK_ORIENTATION_VERTICAL, (homogeneous), (spacing))

#   define gtk_hbox_new(homogeneous, spacing) \
        geda_compat_box_new(GTK_ORIENTATION_HORIZONTAL, (homogeneous), (spacing))

#   define geda_device_grab_remove(w,p) gtk_device_grab_remove(GTK_WIDGET(w),p)

/* Gtk[VH]ButtonBox */
#   define gtk_vbutton_box_new() gtk_button_box_new(GTK_ORIENTATION_VERTICAL)
#   define gtk_hbutton_box_new() gtk_button_box_new(GTK_ORIENTATION_HORIZONTAL)

/* Gtk[VH]Separator */
#   define gtk_vseparator_new() geda_separator_new(GTK_ORIENTATION_VERTICAL)
#   define gtk_hseparator_new() geda_separator_new(GTK_ORIENTATION_HORIZONTAL)

/* Gtk[VH]Paned */
#   define gtk_vpaned_new() gtk_paned_new(GTK_ORIENTATION_VERTICAL)
#   define gtk_hpaned_new() gtk_paned_new(GTK_ORIENTATION_HORIZONTAL)

/* Gtk[VH]Scrollbar */
#   define gtk_vscrollbar_new(adj) gtk_scrollbar_new(GTK_ORIENTATION_VERTICAL, (adj))
#   define gtk_hscrollbar_new(adj) gtk_scrollbar_new(GTK_ORIENTATION_HORIZONTAL, (adj))

#   define geda_get_container_border_width(w) gtk_container_get_border_width((GtkContainer*)w)

#if !defined gtk_dialog_set_has_separator
#   define gtk_dialog_set_has_separator(obj, setting);
#endif

#define geda_toggle_button_get_active(tb) gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(tb))

#define geda_toggle_button_set_active(tb, a) \
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tb), a)


#endif

#endif /* __GEDA_GTK_COMPAT_H__ */
