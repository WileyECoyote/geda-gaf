/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_tree.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2015-2018 Wiley Edward Hill
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
 * Date: January 10, 2015
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
#ifndef __GEDA_TREE_H__
#define __GEDA_TREE_H__

#if (GTK_MAJOR_VERSION < 3) && !defined GTK_DISABLE_SINGLE_INCLUDES

#include <gtk/gtktreeview.h>
#include <gtk/gtktreemodel.h>

#else

#include <gtk/gtk.h>

#endif

/* Utility functions */
void          geda_tree_copy_iter                (GtkTreeIter *iter1, GtkTreeIter *iter2);
bool          geda_tree_model_iter_previous      (GtkTreeModel *tree_model, GtkTreeIter *iter);

#endif /* __GEDA_TREE_H__ */