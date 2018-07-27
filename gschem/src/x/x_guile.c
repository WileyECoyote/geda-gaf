/* -*- C indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_guile.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill
 * Copyright (C) 2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#include "../../include/gschem.h"
#include "../../include/x_dialog.h"

#ifndef QUOTE_SYMBOL
#define QUOTE_SYMBOL(symbol) #symbol
#endif

/** \defgroup Guile-Dialog  Guile Dialog
 *  @{ \memberof Systemic-Dialogs
*/

#define COL_PATH 0

/* Adds path argument to the guile load-path */
static void prepend_guile_path(const char *path)
{
  SCM sym_begin       = scm_from_utf8_symbol ("begin");
  SCM sym_cons        = scm_from_utf8_symbol ("cons");
  SCM sym_set_x       = scm_from_utf8_symbol ("set!");
  SCM sym_load_path   = scm_from_utf8_symbol ("%load-path");

  SCM s_load_expr = SCM_EOL;

  s_load_expr =
          scm_cons (scm_list_3 (sym_set_x,
                                sym_load_path,
                                scm_list_3 (sym_cons,
                                            scm_from_locale_string (path),
                                            sym_load_path)),
                    s_load_expr);

  /* Ensure Scheme expressions can be passed straight to eval */
  s_load_expr = scm_cons (sym_begin, scm_reverse_x (s_load_expr, SCM_UNDEFINED));
  scm_gc_protect_object (s_load_expr);

  g_evaluate_scm_protected (s_load_expr, scm_current_module ());
}

/* Remove str argument from the guile load-path */
static void remove_from_guile_path(const char *str)
{
  SCM s_path_expr;
  SCM s_load_path;
  SCM s_load_path_var;
  SCM s_load_expr;

  SCM sym_begin       = scm_from_utf8_symbol ("begin");
  SCM sym_delv        = scm_from_utf8_symbol ("delv");
  SCM sym_load_path   = scm_from_utf8_symbol ("%load-path");
  SCM sym_set_x       = scm_from_utf8_symbol ("set!");

  s_load_path_var = scm_c_lookup ("%load-path");
  s_load_path     = scm_variable_ref (s_load_path_var);
  s_path_expr     = SCM_EOL;

  int i;
  int  scm_search_len = (int) scm_ilength (s_load_path);

  for (i = 0 ; i < scm_search_len; i++) {

    char *path;
    SCM elem = scm_list_ref(s_load_path, scm_from_int(i));

    path = scm_to_utf8_string(elem);

    if (!strcmp(path, str)) {
      s_path_expr = scm_list_3 (sym_delv, elem, sym_load_path);
      free(path);
      break;
    }

    free(path);
  }

  s_load_expr =
          scm_cons (scm_list_3 (sym_set_x,
                                sym_load_path,
                                s_path_expr),
                    SCM_EOL);

  /* Ensure Scheme expressions can be passed straight to eval */
  s_load_expr = scm_cons (sym_begin, scm_reverse_x (s_load_expr, SCM_UNDEFINED));
  scm_gc_protect_object (s_load_expr);

  g_evaluate_scm_protected (s_load_expr, scm_current_module ());
}

/* Compares the current load-path to the contents of the list-store
 * in the TreeView; strings in the tree that are not in the load-path
 * are passed to prepend_guile_path(), strings in the load-path but
 * are not in the TreeView are passed to remove_from_guile_path()
 */
static void check_update_guile_path (GtkWidget *ThisDialog)
{
  GList        *path_list;
  GtkTreeView  *treeview;
  GtkTreeModel *treemodel;
  GtkTreeIter   iter;

  int next;

  path_list = GEDA_OBJECT_GET_DATA(ThisDialog, "path_list");
  treeview  = GEDA_OBJECT_GET_DATA(ThisDialog, "treeview");
  treemodel = gtk_tree_view_get_model (treeview);
  next      = gtk_tree_model_get_iter_first (treemodel, &iter);

  /* Find strings in tree not currently in the Guile path */
  while (next) {

    char *str  = NULL;

    /* Iterate through the list store, reading each row. */
    gtk_tree_model_get (treemodel, &iter, 0, &str, -1);

    if (geda_glist_find_string(path_list, str) == -1) {
      prepend_guile_path(str);
    }

    next = gtk_tree_model_iter_next (treemodel, &iter);
  }

  while (path_list) {

     char *path;
     bool  found;

     found = FALSE;
     path  = path_list->data;

     next  = gtk_tree_model_get_iter_first (treemodel, &iter);

     /* Find Guile path in not in the tree */
     while (next) {

       char *str  = NULL;

       /* Retrieve the path string from this row */
       gtk_tree_model_get (treemodel, &iter, 0, &str, -1);

       /* Check if this row is the path */
       if (geda_strequal(path, str)) {
         found = TRUE;
         break;
       }
       next = gtk_tree_model_iter_next (treemodel, &iter);
     }

     /* If path not found in the tree then remove from load path */
     if (!found) {
       remove_from_guile_path(path);
     }

     path_list = path_list->next;
  }
}

/* Releases the glist of string used to store the load-path
 * and then the dialog itself
 */
static void x_dialog_guile_finalize(GtkWidget *ThisDialog)
{
  GList *list;

  list = GEDA_OBJECT_GET_DATA(ThisDialog, "path_list");

  geda_glist_free_all(list);

  GEDA_OBJECT_SET_DATA (ThisDialog, NULL, "path_list");

  gtk_widget_destroy(ThisDialog);
}

static bool is_valid_path(const char *path)
{
  if (path != NULL) {
    return g_file_test (path, G_FILE_TEST_IS_DIR);
  }
  return FALSE;
}

/* Handles "Add" response; prompts for a string and if the string
 * is a valid path the string is prepended to the TreeView
 */
static void x_dialog_guile_response_add(GtkWidget *ThisDialog)
{
  char *string;

  string = geda_dialog_get_string(_("Guile Path"),
                                  _("Specify new path to search:"),
                                     NULL);
  if (is_valid_path(string)) {

    GList *path_list;

    path_list = GEDA_OBJECT_GET_DATA(ThisDialog, "path_list");

    /* Guile will not allow duplicates path but does not generate
     * and error, check if path is already in tree so that we do
     * not add a path already in the tree */
    if (geda_glist_find_string(path_list, string) == -1) {

      GtkTreeView  *treeview;
      GtkListStore *liststore;
      GtkTreeIter   iter;

      treeview  = GEDA_OBJECT_GET_DATA(ThisDialog, "treeview");
      liststore = (GtkListStore*)gtk_tree_view_get_model (treeview);

      gtk_list_store_prepend (liststore, &iter);
      gtk_list_store_set (liststore, &iter, COL_PATH, string, -1);
    }
    else {

      const char *msg = _("Attempting to duplicate path");

      geda_log("%s: %s\n", msg, string);
    }
  }
  else if (string != NULL) {

    const char *msg = _("Invalid path");

    geda_log("%s: %s\n", msg, string);
  }

  GEDA_FREE(string);
}

/* This function checks if the user is attempting to remove
 * the guild system libary, package or site path from the
 * load-path. These are not allowed to be removed.
 */
static bool can_remove_path(const char *path)
{
  SCM s_sys_path;
  char *sys_path;

  bool can_remove;

  s_sys_path = scm_sys_library_dir ();
  sys_path   = scm_to_utf8_string (s_sys_path);
  can_remove = strstr (path, sys_path) == NULL;
  free(sys_path);

  if (can_remove) {
    s_sys_path = scm_sys_package_data_dir ();
    sys_path   = scm_to_utf8_string (s_sys_path);
    can_remove = strstr (path, sys_path) == NULL;
    free(sys_path);
  }

  if (can_remove) {
    s_sys_path = scm_sys_site_dir ();
    sys_path   = scm_to_utf8_string (s_sys_path);
    can_remove = strstr (path, sys_path) == NULL;
    free(sys_path);
  }

  return can_remove;
}

/* Handles "Remove" response; remove the current TreeView selection */
static void x_dialog_guile_response_remove(GtkWidget *ThisDialog)
{
  GtkTreeSelection *selection;
  GtkTreeModel     *treemodel;
  GtkTreeView      *treeview;
  GtkTreeIter       iter;

  treeview  = GEDA_OBJECT_GET_DATA(ThisDialog, "treeview");
  selection = gtk_tree_view_get_selection (treeview);
  treemodel = NULL;

  if (gtk_tree_selection_get_selected (selection, &treemodel, &iter)) {

    char *str = NULL;

    /* Retrieve the path string from the selected row */
    gtk_tree_model_get (treemodel, &iter, 0, &str, -1);

    if (can_remove_path(str)) {
      gtk_list_store_remove ((GtkListStore *)treemodel, &iter);
    }
    else {

      const char *msg = _("Refusing to remove path");

      geda_log("%s: %s\n", msg, str);
    }
  }
}

/*! \brief response function for the translate dialog
 *  \par Function Description
 *  Handles user action based on responses. The APPLY and REJECT response,
 *  aka, Add and Remove, only apply changes to treeview in the dialog. The
 *  actual changes are not applied to the load path until the used clicked
 *  the "Okay" button.
 */
static void x_dialog_guile_response(GtkWidget      *ThisDialog,
                                    int             response,
                                    GschemToplevel *w_current)
{
  switch (response) {
  case GEDA_RESPONSE_ACCEPT:
    check_update_guile_path(ThisDialog);

  case GEDA_RESPONSE_CANCEL:
  case GEDA_RESPONSE_DELETE_EVENT:
    x_dialog_guile_finalize(ThisDialog);
    break;

  case GEDA_RESPONSE_APPLY: /* Add button */
    x_dialog_guile_response_add(ThisDialog);
    break;

  case GEDA_RESPONSE_REJECT: /* Remove button */
    x_dialog_guile_response_remove(ThisDialog);
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }
}

static GtkTreeModel *create_and_fill_model (GtkWidget *ThisDialog, SCM s_load_path)
{
  GList         *path_list;
  GtkListStore  *store;
  GtkTreeIter    iter;

  path_list = NULL;
  store     = gtk_list_store_new (1, G_TYPE_STRING);

  int i;
  int  scm_search_len = (int) scm_ilength (s_load_path);

  for (i = 0 ; i < scm_search_len; i++) {

    char *path;
    SCM elem = scm_list_ref(s_load_path, scm_from_int(i));

    path = scm_to_utf8_string(elem);

    /* Append a row and fill in the path */
    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter, COL_PATH, path, -1);

    path_list = g_list_append(path_list, geda_strdup(path));

    free(path);
  }

  /* Store the current guile path in the dialog */
  GEDA_OBJECT_SET_DATA (ThisDialog, path_list, "path_list");

  return (GtkTreeModel*)store;
}

/*! \brief Adds a treeview for displaying path string.
 *  \par Function Description
 *  This function adds a treeview and caption to display the content
 *  of the current guile search paths
 *
 *  \param [in] ThisDialog  Passed to create_and_fill_model().
 *  \param [in] s_load_path SCM list of paths.
 *
 *  \returns A pointer on the GtkHBox to add to dialog.
 */
static GtkWidget *x_guile_dialog_path_list (GtkWidget *ThisDialog, SCM s_load_path)
{
  GtkCellRenderer *renderer;
  GtkTreeModel    *model;
  GtkTreeView     *treeview;
  GtkWidget       *hbox, *scrolled_window;

  /* place the treeview and its caption into a box */
  hbox = g_object_new (GTK_TYPE_HBOX,
                       /* GtkBox */
                       "homogeneous", TRUE,
                       "spacing",     8,
                       NULL);

  /* list guile search path */

  /* create a scrolled window container for a treeview */
  scrolled_window = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                  /* GtkScrolledWindow */
                                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                  "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                  "shadow-type",  GTK_SHADOW_ETCHED_IN,
                                  NULL);

  treeview = (GtkTreeView*)gtk_tree_view_new ();

  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                           "editable", FALSE, NULL);

  gtk_tree_view_insert_column_with_attributes (treeview,
                                               -1,
                                               "Path",
                                               renderer,
                                               "text", COL_PATH,
                                               NULL);

  model = create_and_fill_model(ThisDialog, s_load_path);

  gtk_tree_view_set_model (treeview, model);

  /* The tree view has acquired its own reference to the model,
   * so we can drop ours. That way the model will */

  g_object_unref (model);

  geda_container_add (scrolled_window, treeview);

  gtk_box_pack_start (GTK_BOX (hbox), scrolled_window, TRUE, TRUE, 0);

  GEDA_OBJECT_SET_DATA (ThisDialog, treeview, "treeview");

  return hbox;
}

/*! \brief Create the Guile dialog
 *  \par Function Description
 *  Create the dialog to translate symbols.
 */
void x_guile_dialog (GschemToplevel *w_current)
{
  GtkWidget  *ThisDialog;
  GtkWidget  *hbox;
  GtkWidget  *vbox;

  const char *version;
        char *guile_ver;
        char *title;

  version   = _("Version");

  guile_ver = scm_to_utf8_string(scm_version());

  title     = geda_sprintf("Guile (%s %s)", version, guile_ver, NULL);

  GEDA_FREE(guile_ver);

  ThisDialog = gschem_dialog_new_with_buttons(title,
                                              w_current->main_window,
                                              GTK_DIALOG_MODAL,
                                              "x-guile", w_current,
                                              GTK_STOCK_ADD,
                                              GEDA_RESPONSE_APPLY,
                                              GTK_STOCK_REMOVE,
                                              GEDA_RESPONSE_REJECT,
                                              GTK_STOCK_CANCEL,
                                              GEDA_RESPONSE_CANCEL,
                                              GTK_STOCK_OK,
                                              GEDA_RESPONSE_ACCEPT,
                                              NULL);
  free(title);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response(GTK_DIALOG(ThisDialog),
                                  GEDA_RESPONSE_ACCEPT);

  vbox = GTK_DIALOG(ThisDialog)->vbox;

  SCM s_load_path;
  SCM s_load_path_var;

  s_load_path_var = scm_c_lookup ("%load-path");
  s_load_path     = scm_variable_ref (s_load_path_var);

  hbox = x_guile_dialog_path_list(ThisDialog, s_load_path);

  gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

  gtk_widget_show_all (ThisDialog);

  g_signal_connect (G_OBJECT (ThisDialog), "response",
                    G_CALLBACK (x_dialog_guile_response),
                    w_current);
}

/***************** End of Guile dialog box ***********************/

/** @} End Group Guile-Dialog */
