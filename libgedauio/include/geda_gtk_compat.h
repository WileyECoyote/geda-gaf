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

/*! \def geda_get_widget_allocation Get Pointer to Allocation */
#define geda_get_widget_allocation(w) &(GTK_WIDGET(w)->allocation);

#define geda_get_widget_requisition(w) &(GTK_WIDGET(w)->requisition);

#else

#define geda_get_widget_allocation(w) \
  ({ GtkAllocation a; gtk_widget_get_allocation (GTK_WIDGET(w), &a); &a; })

#  define geda_get_widget_requisition(w) \
  ({ GtkRequisition r; gtk_widget_get_preferred_size (GTK_WIDGET(w), NULL, &r); &r; })

#endif

#endif /* __GEDA_GTK_COMPAT_H__ */
