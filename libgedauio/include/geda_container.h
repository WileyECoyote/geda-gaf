/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_container.h
 *
 * libgedauio - gEDA's library for User Interface Objects
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2017-2018 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Date: December 31, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */

#ifdef __cplusplus
extern "C" {
#endif

GList  *geda_container_focus_sort (GtkContainer     *container,
                                   GList            *children,
                                   GtkDirectionType  direction,
                                   GtkWidget        *old_focus);

#define geda_container_add(c, w) gtk_container_add((GtkContainer*)c, (GtkWidget*)w)
#define geda_container_remove(c, w) gtk_container_remove((GtkContainer*)c, (GtkWidget*)w)

#define geda_container_get_children(c) gtk_container_get_children((GtkContainer*)c)

#define geda_set_container_border_width(c, w) gtk_container_set_border_width((GtkContainer*)c, w)
#define geda_container_set_focus_chain(c, fc) gtk_container_set_focus_chain ((GtkContainer*)c, fc);

#define geda_container_forall(c, f, d) gtk_container_forall ((GtkContainer*)c, (GtkCallback)f, (void*)d)
#define geda_container_foreach(c, f, d) gtk_container_forall ((GtkContainer*)c, (GtkCallback)f, (void*)d)

#define geda_container_propagate_expose(c, w, e) gtk_container_propagate_expose ((GtkContainer*)c, (GtkWidget*)w, e)

#ifdef __cplusplus
}
#endif /* __cplusplus */