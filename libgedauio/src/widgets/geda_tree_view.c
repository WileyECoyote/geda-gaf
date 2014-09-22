/* -*- C geda_tree_view.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: geda_tree_view.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2014 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA
 *
 * Date: September, 18, 2014
 * Contributing Author: Wiley Edward Hill
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <geda.h>

#include <glib.h>
#include <gtk/gtk.h>

#include "geda_tree_view.h"
#include "gettext.h"
#include <geda_debug.h>

/**
 * \brief GedaTreeView - A Button Widget for Menus
 * \par
 * A GedaTreeView is a variant of GedaTreeView with additonal function
 * support.
 *
 * \defgroup GedaTreeView Geda Tree View Widget
 * @{
 */

static GObjectClass *geda_tree_view_parent_class = NULL;


static void
geda_tree_view_class_init (GedaTreeViewClass *klass)
{
  //GObjectClass   *gobject_class;

  geda_tree_view_parent_class = g_type_class_peek_parent (klass);

  //gobject_class = G_OBJECT_CLASS (klass);

}

/*! \brief Initialize GedaTreeView data structure.
 *
 *  \par Function Description
 *  Function is call after the GedaTreeViewClass is created
 *  to initialize the data structure.
 *
 * \param [in] tree_view A GedaTreeView object (structure)
 */
static void geda_tree_view_init (GedaTreeView *tree_view)
{

}

/*! \brief Function to retrieve GedaTreeView's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve GedaTreeView's Type identifier.
 *  On the first call, this registers the GedaTreeView in the GedaType
 *  system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return GedaType identifier associated with GedaTreeView.
 */
GedaType geda_tree_view_get_type ()
{
  static GedaType geda_tree_view_type = 0;

  if (!geda_tree_view_type) {
    static const GTypeInfo geda_tree_view_info = {
      sizeof(GedaTreeViewClass),
      NULL, /* base_init      */
      NULL, /* base_finalize  */
      (GClassInitFunc) geda_tree_view_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data     */
      sizeof(GedaTreeView),
      0,    /* n_preallocs    */
      (GInstanceInitFunc) geda_tree_view_init, /* instance_init */
    };

    geda_tree_view_type = g_type_register_static (GTK_TYPE_TREE_VIEW,
                                                  "GedaTreeView",
                                                  &geda_tree_view_info, 0);
  }

  return geda_tree_view_type;
}

/*! \brief Create a New GedaTreeView
 *
 *  \par Function Description
 *  This function creates and returns a new GedaTreeView
 *
 * Return value: a new GedaTreeView
 *
 */
GtkWidget *
geda_tree_view_new (void)
{
  return g_object_new (GTK_TYPE_TREE_VIEW, NULL);
}

/*! \brief Create a New GedaTreeView with a given Model
 *
 *  \par Function Description
 * Creates a new #GedaTreeView widget with the model initialized to
 * the given \a model.
 *
 * Returns: A newly created #GedaTreeView widget.
 */
GtkWidget *
geda_tree_view_new_with_model (GtkTreeModel *model)
{
  return g_object_new (GTK_TYPE_TREE_VIEW, "model", model, NULL);
}

/* WEH: Credit for the next two functions goes to The Geeqie Team, I added
 * the test for iter.stamp to surpress gtk's non-sense.
 *
 * (SLIK) SimpLIstic sKin functions
 * (C) 2004 John Ellis
 * Copyright (C) 2004 - 2014 The Geeqie Team
 *
 * Author: John Ellis   http://geeqie.sourceforge.net/
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */
/*! \brief Get visibility of Row in GtkTreeView
 *  \par Function Description
 *  This function is primarily used by the next function to determine if
 *  a row in a scrollable view is centered.
 */
int
geda_tree_view_row_get_visibility (GtkTreeView *tree_view,
                                   GtkTreeIter *iter,
                                   bool         fully_visible)
{
  GtkTreeModel *model;
  GtkTreePath  *path, *start_path, *end_path;
  bool valid;
  int  ret = 0;

  valid = ( iter->stamp != 0 ? TRUE : FALSE);

  if (valid) {

    if (!gtk_tree_view_get_visible_range (tree_view, &start_path, &end_path))
      return -1;

    /* we will most probably scroll down, needed for tree_view_row_make_visible */
    model = gtk_tree_view_get_model(tree_view);
    path = gtk_tree_model_get_path(model, iter);

    if (fully_visible) {
      if (gtk_tree_path_compare(path, start_path) <= 0) {
        ret = -1;
      }
      else if (gtk_tree_path_compare (path, end_path) >= 0) {
        ret = 1;
      }
    }
    else {
      if (gtk_tree_path_compare (path, start_path) < 0) {
        ret = -1;
      }
      else
        if (gtk_tree_path_compare (path, end_path) > 0) {
          ret = 1;
        }
    }

    gtk_tree_path_free (path);
    gtk_tree_path_free (start_path);
    gtk_tree_path_free (end_path);
  }
  else ret = -1;
  return ret;
}

/*! \brief Make Tree Row Visible in GtkTreeView
 *  \par Function Description
 *  This function provides a reliable method to adjust tree views to
 *  positions. GTK lacks comparable functionality. Don't bother with
 *  gtk_tree_view_set_cursor.
 */
int
geda_tree_view_row_make_visible(GtkTreeView *tree_view,
                                GtkTreeIter *iter,
                                bool         center)
{
  GtkTreePath *path;
  int visible;
  bool valid;

  valid = ( iter->stamp != 0 ? TRUE : FALSE);

  if (valid) {
  visible = geda_tree_view_row_get_visibility (tree_view, iter, TRUE);

  path = gtk_tree_model_get_path(gtk_tree_view_get_model (tree_view), iter);
  if (center && visible != 0) {
    gtk_tree_view_scroll_to_cell (tree_view, path, NULL, TRUE, 0.5, 0.0);
  }
  else
    if (visible < 0) {
      gtk_tree_view_scroll_to_cell (tree_view, path, NULL, TRUE, 0.0, 0.0);
    }
    else if (visible > 0) {
      gtk_tree_view_scroll_to_cell (tree_view, path, NULL, TRUE, 1.0, 0.0);
    }

  gtk_tree_path_free (path);

  }
  else {
    visible = -1;
  }
  return visible;
}

/** @} end group GedaTreeView */