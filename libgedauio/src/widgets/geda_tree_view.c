/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_tree_view.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2014-2015 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 2 of the
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
 * Date: September, 18, 2014
 * Contributing Author: Wiley Edward Hill
 *
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <geda/geda.h>

#include <glib.h>
#include <gtk/gtk.h>

#include "../../include/geda_tree_view.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaTreeView - A Button Widget for Menus
 * \par
 * A GedaTreeView is a variant of GtkTreeView with additonal function
 * support.
 *
 * \defgroup GedaTreeView Geda Tree View Widget
 * @{
 */

static GObjectClass *geda_tree_view_parent_class = NULL;

static GHashTable *tree_view_hash = NULL;

static void
geda_tree_view_finalize (GObject *object)
{
  if (g_hash_table_remove (tree_view_hash, object)) {
    if (!g_hash_table_size (tree_view_hash)) {
      g_hash_table_destroy (tree_view_hash);
      tree_view_hash = NULL;
    }
  }

  G_OBJECT_CLASS (geda_tree_view_parent_class)->finalize (object);
}

/*!
 * \brief GedaTreeView Class Initializer
 * \par Function Description
 *  Function is called to initialize the class instance.
 *
 * \param [in] class A GedaTreeViewClass Object
 * \param [in] data  A GedaTreeView data structure
 */
static void
geda_tree_view_class_init (void *class, void *data)
{
  GObjectClass      *object_class    = G_OBJECT_CLASS (class);
  //GedaTreeViewClass *tree_view_class = (GedaTreeViewClass)class;

  object_class->finalize      = geda_tree_view_finalize;

  geda_tree_view_parent_class = g_type_class_peek_parent (class);
}

/*!
 * \brief Initialize new GedaTreeView data structure instance.
 * \par Function Description
 *  This function is call after the GedaTreeViewClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaTreeView data structure
 * \param [in] class     A GedaTreeViewClass Object
 */
static void
geda_tree_view_instance_init (GTypeInstance *instance, void *class)
{
  //GedaTreeView *tree_view  = (GedaTreeView*)instance;

  if (!tree_view_hash) {
    tree_view_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (tree_view_hash, instance, instance);
}

/*!
 * \brief Function to retrieve GedaTreeView's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaTreeView Type identifier. When first
 *  called this function registers a #GedaTreeView in the GedaType
 *  system to obtain an unique itentifier for a GedaTreeView and
 *  returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \returns GedaType identifier associated with a GedaTreeView.
 */
GedaType geda_tree_view_get_type (void)
{
  static volatile GedaType geda_tree_view_type = 0;

  if (g_once_init_enter (&geda_tree_view_type)) {

    static const GTypeInfo geda_tree_view_info = {
      sizeof(GedaTreeViewClass),
      NULL,                                      /* base_init      */
      NULL,                                      /* base_finalize  */
      geda_tree_view_class_init,                 /* (GClassInitFunc) */
      NULL,                                      /* class_finalize */
      NULL,                                      /* class_data     */
      sizeof(GedaTreeView),
      0,                                         /* n_preallocs    */
      geda_tree_view_instance_init               /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaTreeView");
    type   = g_type_register_static (GTK_TYPE_TREE_VIEW, string,
                                    &geda_tree_view_info, 0);

    g_once_init_leave (&geda_tree_view_type, type);
  }

  return geda_tree_view_type;
}

/*!
 * \brief Check if an object is a GedaTreeView
 * \par Function Description
 *  Determines if \a tree_view is valid by verifying \a tree_view
 *  is included in the hash table of GedaTreeView objects.
 *
 * \returns TRUE if \a tree_view is a valid GedaTreeView
 */
bool
is_a_geda_tree_view (GedaTreeView *tree_view)
{
  if ((tree_view != NULL) && (tree_view_hash != NULL)) {
    return g_hash_table_lookup(tree_view_hash, tree_view) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Create a New GedaTreeView
 * \par Function Description
 *  This function creates and returns a new GedaTreeView
 *
 * \returns a new GedaTreeView
 */
GtkWidget *
geda_tree_view_new (void)
{
  return g_object_new (GTK_TYPE_TREE_VIEW, NULL);
}

/*!
 * \brief Create a New GedaTreeView with a given Model
 * \par Function Description
 * Creates a new #GedaTreeView widget with the model initialized to
 * the given \a model.
 *
 * \returns a newly created #GedaTreeView widget.
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
/*!
 * \brief Get visibility of Row in GtkTreeView
 *  \par Function Description
 *  This function is primarily used by the next function to determine if
 *  a row in a scrollable view is centered.
 */
int
geda_tree_view_row_get_visibility (GtkTreeView *tree_view,
                                   GtkTreeIter *iter,
                                   bool         fully_visible)
{
  bool valid;
  int  visible = 0;

  valid = ( iter->stamp != 0 ? TRUE : FALSE);

  if (valid) {

    GtkTreeModel *model;
    GtkTreePath  *path, *start_path, *end_path;

    if (!gtk_tree_view_get_visible_range (tree_view, &start_path, &end_path))
      return -1;

    /* we will most probably scroll down, needed for tree_view_row_make_visible */
    model = gtk_tree_view_get_model(tree_view);
    path = gtk_tree_model_get_path(model, iter);

    if (fully_visible) {
      if (gtk_tree_path_compare(path, start_path) <= 0) {
        visible = -1;
      }
      else if (gtk_tree_path_compare (path, end_path) >= 0) {
        visible = 1;
      }
    }
    else {
      if (gtk_tree_path_compare (path, start_path) < 0) {
        visible = -1;
      }
      else
        if (gtk_tree_path_compare (path, end_path) > 0) {
          visible = 1;
        }
    }

    gtk_tree_path_free (path);
    gtk_tree_path_free (start_path);
    gtk_tree_path_free (end_path);
  }
  else {
    visible = -1;
  }
  return visible;
}

/*!
 * \brief Make Tree Row Visible in GtkTreeView
 * \par Function Description
 *  This function provides a reliable method to adjust tree views to
 *  positions. GTK lacks comparable functionality. Don't bother with
 *  gtk_tree_view_set_cursor.
 */
int
geda_tree_view_row_make_visible(GtkTreeView *tree_view,
                                GtkTreeIter *iter,
                                bool         center)
{
  int visible;
  bool valid;

  valid = ( iter->stamp != 0 ? TRUE : FALSE);

  if (valid) {

    GtkTreePath *path;

    visible = geda_tree_view_row_get_visibility (tree_view, iter, TRUE);

    path = gtk_tree_model_get_path(gtk_tree_view_get_model (tree_view), iter);

    if (center && visible != 0) {
      gtk_tree_view_scroll_to_cell (tree_view, path, NULL, TRUE, 0.5, 0.0);
    }
    else {
      if (visible < 0) {
        gtk_tree_view_scroll_to_cell (tree_view, path, NULL, TRUE, 0.0, 0.0);
      }
      else if (visible > 0) {
        gtk_tree_view_scroll_to_cell (tree_view, path, NULL, TRUE, 1.0, 0.0);
      }
    }

    gtk_tree_path_free (path);

  }
  else {
    visible = -1;
  }

  return visible;
}

/** @} end group GedaTreeView */