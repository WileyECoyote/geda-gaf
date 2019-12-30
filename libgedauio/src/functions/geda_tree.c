/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_trees.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2015 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Date: January 10, 2015
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif
#include <geda/geda.h>

#include <geda_tree.h>

/*! \brief Copy a Tree Iter
 *  \par Function Description
 *  This function will set all pointers target to the values in source.
 *
 */
void geda_tree_copy_iter(GtkTreeIter *source, GtkTreeIter *target)
{
  target->stamp       = source->stamp;
  target->user_data   = source->user_data;
  target->user_data2  = source->user_data2;
  target->user_data3  = source->user_data3;

  return;
}

/*! \brief Get the Previous item in a Tree Model
 *  \par Function Description
 *
 * \remark Probably should have been included in gtktreemodel.c
 */
bool geda_tree_model_iter_previous (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    GtkTreePath *path;
    bool ret;

    path = gtk_tree_model_get_path (tree_model, iter);
    ret  = gtk_tree_path_prev (path);

    if (ret != FALSE) {
      gtk_tree_model_get_iter (tree_model, iter, path);
    }

    gtk_tree_path_free (path);

    return ret;
}
