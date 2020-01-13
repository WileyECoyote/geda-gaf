/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_tree_view.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2016 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Date: September, 18, 2014
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_TREE_VIEW_H__
#define __GEDA_TREE_VIEW_H__

#if (GTK_MAJOR_VERSION < 3) && !defined GTK_DISABLE_SINGLE_INCLUDES

#include <gtk/gtktreeview.h>
#include <gtk/gtktreemodel.h>

#else

#include <gtk/gtk.h>

#endif

#include "geda_tree.h"

#define GEDA_TYPE_TREE_VIEW            (geda_tree_view_get_type ())
#define GEDA_TREE_VIEW(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_TREE_VIEW, GedaTreeView))
#define GEDA_TREE_VIEW_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_TREE_VIEW, GedaTreeViewClass))
#define GEDA_IS_TREE_VIEW(obj)         (is_a_geda_tree_view((GedaTreeView*)obj))
#define GEDA_IS_TREE_VIEW_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_TREE_VIEW))
#define GEDA_TREE_VIEW_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),   GEDA_TYPE_TREE_VIEW, GedaTreeViewClass))

typedef struct _GedaTreeView        GedaTreeView;
typedef struct _GedaTreeViewClass   GedaTreeViewClass;

struct _GedaTreeView
{
  GtkTreeView parent_instance;
};

struct _GedaTreeViewClass
{
  GtkTreeViewClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType      geda_tree_view_get_type            (void) GEDA_CONST;
bool          is_a_geda_tree_view                (GedaTreeView *tree_view);

GtkWidget    *geda_tree_view_new                 (void);
GtkWidget    *geda_tree_view_new_with_model      (GtkTreeModel *model);

int           geda_tree_view_row_get_visibility  (GtkTreeView *tree_view, GtkTreeIter *iter, bool fully_visible);
int           geda_tree_view_row_make_visible    (GtkTreeView *tree_view, GtkTreeIter *iter, bool center);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif  /* __GEDA_TREE_VIEW_H__ */
