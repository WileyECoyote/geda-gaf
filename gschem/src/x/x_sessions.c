/* C
 * File: x_sessions.c
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 * Date: July, 13, 2014
 * Contributing Author: Wiley Edward Hill
 *
 */

#include <config.h>

#include <gschem.h>
#include <geda_dialogs.h>
#include "x_dialog.h"

#define TreeSelection GtkTreeSelection

enum {
  COLUMN_NAME,
  COLUMN_COUNT,
  NUM_COLUMNS
};

/*! \brief Update tree model of <B>Session</B>'s treeview.
 *  \par Function Description
 *  Updates the tree model of <B>Session</B>\'s treeview.
 *
 *  Right now, each time it is called, it rebuilds all the model from the
 *  list of pages passed in.
 *  It is a recursive function to populate the tree store
 *
 *  \param [in] record  PageList of pages for this toplevel.
 */
static void session_tree_update (GtkWidget *dialog)
{
  GArray        *sessions;
  Session       *record;
  GtkTreeView   *treeview;
  GtkTreeIter    iter;
  GtkTreeModel  *model;
  int            index;

  treeview = gtk_object_get_data(GTK_OBJECT(dialog), "tree-view");
  model    = gtk_tree_view_get_model (treeview);

  /* wipe out every thing in the store */
  gtk_tree_store_clear (GTK_TREE_STORE (model));

  if (i_sessions_is_enabled()) { /* if session data exist */

    sessions = i_sessions_get_sessions();

    for(index = 0; index < sessions->len; index++) {

      record = &g_array_index(sessions, Session, index);

      /* add the record to the store */
      gtk_tree_store_append (GTK_TREE_STORE (model), &iter, NULL);

      gtk_tree_store_set (GTK_TREE_STORE (model), &iter,
                          COLUMN_NAME, record->session_name,
                          COLUMN_COUNT, record->page_count, -1);
    }
  }
}

static GtkTreeSelection*
session_dialog_get_selection(GschemDialog *dialog)
{
  GtkWidget  *treeview;

  treeview = gtk_object_get_data(GTK_OBJECT(dialog), "tree-view");
  return gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
}

static void
on_rename_butt_clicked (GtkWidget *button, void *user_data)
{
  GschemDialog      *Dialog    = (GschemDialog*)user_data;
  GschemToplevel    *w_current = Dialog->w_current;
  GtkTreeSelection  *selection;
  GtkTreeModel      *model;
  GtkTreeIter        iter;
  const char        *old_name;
        char        *new_name;

  new_name = geda_dialog_get_string("Rename Session", "Specify new name for Session:");

  if (new_name != NULL) { /* If  user did not cancel */

    selection = session_dialog_get_selection(Dialog);

    if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

      gtk_tree_model_get (model, &iter, COLUMN_NAME, &old_name, -1);

      if (i_sessions_rename_session(w_current, old_name, new_name)) {
          gtk_tree_store_set (GTK_TREE_STORE (model), &iter,
                              COLUMN_NAME, new_name, -1);
      }
    }
    GEDA_FREE(new_name);
  }
}

static void
on_delete_butt_clicked (GtkWidget *button, void *user_data)
{
  GschemDialog      *Dialog    = (GschemDialog*)user_data;
  GschemToplevel    *w_current = Dialog->w_current;
  GtkTreeSelection  *selection;
  GtkTreeModel      *model;
  GtkTreeIter        iter;
  const char        *name;

  selection = session_dialog_get_selection(Dialog);

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    gtk_tree_model_get (model, &iter, COLUMN_NAME, &name, -1);

    i_sessions_delete_session(w_current, name);
    gtk_tree_store_remove (GTK_TREE_STORE (model), &iter);
  }
}

static void
on_export_butt_clicked (GtkWidget *button, void *user_data)
{
  GschemDialog      *Dialog    = (GschemDialog*)user_data;
  GtkTreeSelection  *selection;
  GtkTreeModel      *model;
  GtkTreeIter        iter;
  const char        *name;
  char              *filename = NULL;

  selection = session_dialog_get_selection(Dialog);

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    gtk_tree_model_get (model, &iter, COLUMN_NAME, &name, -1);

    filename = gschem_filesel_dialog("Select export file", name, FSB_SAVE);

    if(filename != NULL) { /* if user did not cancel */
      i_sessions_export_session(name, filename);
      q_log_message(_("Exported session %s to %s\n"), name, filename);
      GEDA_FREE(filename);
    }
  }
}

static void
on_open_butt_clicked(GtkButton *button, void *user_data)
{
  GschemDialog      *Dialog    = (GschemDialog*)user_data;
  GschemToplevel    *w_current = Dialog->w_current;
  GtkTreeSelection  *selection;
  GtkTreeModel      *model;
  GtkTreeIter        iter;
  const char        *name;

  selection = session_dialog_get_selection(Dialog);

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    gtk_tree_model_get (model, &iter, COLUMN_NAME, &name, -1);
    if (i_sessions_open_session(w_current, name)) {

      g_signal_emit_by_name (GTK_DIALOG (Dialog), "response",
                             GTK_RESPONSE_REJECT,
                             NULL);
    }
  }
}

/*! \brief Page Manager Dialog Tree View Page Selected
 *  \par Function Description
 *  This function is called whenever a row is selected in the
 *  treeview. Buttons are are not active, buttons are enabled
 *  and signals connected.
 *
 *  \note This function is not used by the Open-Session dialog
 */
static void
manage_session_selection_changed (GtkTreeSelection *selection,
                                    void *user_data)
{
  GtkWidget *Dialog = (GtkWidget*)user_data;
  GtkWidget *button;

  button = gtk_object_get_data(GTK_OBJECT(Dialog), "rename-butt");

  if(!gtk_widget_get_sensitive (button)) {

    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_rename_butt_clicked),
                      Dialog);

    button = gtk_object_get_data(GTK_OBJECT(Dialog), "delete-butt");
    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_delete_butt_clicked),
                      Dialog);

    button = gtk_object_get_data(GTK_OBJECT(Dialog), "export-butt");
    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_export_butt_clicked),
                      Dialog);

    button = gtk_object_get_data(GTK_OBJECT(Dialog), "open-butt");
    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_open_butt_clicked),
                      Dialog);
  }
}

/*! \brief Response function for Session dialogs
 *  \par Function Description
 *  This is a response function called when the used selects one
 *  of the action bottons, either CLOSE or APPLY.
 *
 *  \param [in]   Dialog    ptr to the dialog widget
 *  \param [in]   response  int signal indicating which button
 *  \param [in]   nothing   Not used
 */
void
x_sessions_response(GtkWidget *Dialog, int response, void *nothing)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy (Dialog);
    break;
  case GTK_RESPONSE_ACCEPT:
    break;
  default:
    BUG_IMSG("strange signal %d\n", response);
  }

  i_set_state (w_current, SELECT);

}

/*! \brief Emit GTK_RESPONSE_REJECT response signal */
static void on_close_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data), "response",
                           GTK_RESPONSE_REJECT,
                           user_data);
}

static void
callback_session_startup_toggled(GtkToggleButton *button, void *nothing)
{
  bool state;

  state = gtk_toggle_button_get_active(button);
  i_sessions_set_show_at_startup(state);
}

/*! \brief Creates Action Area for the  for Session dialogs.
 *  \par Function Description
 *  We create our own "Action Area", because; 1.) GTK's entire
 * concept of an action area is more of a hindrance then it is
 * useful, and 2.) We need access to the apply button widget.
 */
static GtkWidget*
create_action_area (GschemDialog *ThisDialog, GtkWidget *parent)
{
  GtkWidget  *action_hbox  = NULL;
  GtkWidget  *checkbutton;
  const char *open_tip;
  const char *startup_tip;
  int         startup;

  open_tip     = _("Loads the selected session");
  startup_tip  = _("Show the Open Session dialog at startup, over-rides auto-load-last feature");

  /* Create a Horizontal Box for everything to go into */
  NEW_HCONTROL_BOX(parent, action, DIALOG_H_SPACING);

  checkbutton = gtk_check_button_new_with_mnemonic (_("Show at startup"));
  g_object_set (checkbutton, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (action_hbox), checkbutton, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text(checkbutton, startup_tip);
  startup = i_sessions_get_show_at_startup();
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton), startup);



  /* Create and connect the Close and Apply Buttons */
  GtkWidget *close_butt = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
  GtkWidget *open_butt  = gtk_button_new_from_stock (GTK_STOCK_OPEN);

  SetWidgetSize (close_butt, DIALOG_BUTTON_HSIZE, DIALOG_BUTTON_VSIZE);
  SetWidgetSize (open_butt, DIALOG_BUTTON_HSIZE, DIALOG_BUTTON_VSIZE);

  gtk_box_pack_end (GTK_BOX (action_hbox), close_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_box_pack_end (GTK_BOX (action_hbox), open_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_widget_set_sensitive (open_butt, FALSE);
  gtk_widget_set_tooltip_text(open_butt, open_tip);

  g_signal_connect (checkbutton, "toggled",
                    G_CALLBACK (callback_session_startup_toggled),
                    NULL);

  g_signal_connect (close_butt, "clicked",
                    G_CALLBACK (on_close_butt_clicked),
                    ThisDialog);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(ThisDialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_REJECT,
                                          -1);

  //gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog), GTK_RESPONSE_ACCEPT);

  GSCHEM_HOOKUP_OBJECT(ThisDialog, open_butt,  "open-butt");
  GSCHEM_HOOKUP_OBJECT(ThisDialog, close_butt, "close-butt");

  return action_hbox;
}

/*! \brief Returns a Session treeview for Session dialogs.
 *  \par Function Description
 *  This function adds a treeview and caption to display the content
 *  of the dialog model of pages with unsaved changes.
 *
 *  The treeview displays the page names with check boxes.
 *
 *  \param [in] dialog The dialog.
 *  \returns A pointer on the GtkVBox to add to dialog.
 */
static GtkWidget*
x_sessions_get_treeview (GtkWidget *Dialog)
{

  GtkTreeViewColumn *column;
  GtkCellRenderer   *renderer;
  GtkTreeModel      *store;
  GtkWidget         *scrolled_window;
  GtkWidget         *treeview;
  GtkTreeSelection  *selection;

  /* create the model for the treeview */
  store = (GtkTreeModel*)gtk_tree_store_new (NUM_COLUMNS,
                                             G_TYPE_STRING, /* name */
                                             G_TYPE_INT);   /* Count */

  /* create a scrolled window for the treeview */
  scrolled_window = GTK_WIDGET (g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                             "border-width",       DIALOG_BORDER_WIDTH,
                             "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                             "vscrollbar-policy",    GTK_POLICY_ALWAYS,
                             "shadow-type",       GTK_SHADOW_ETCHED_IN,
                                                                  NULL));

  /* create model for treeview and populate */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "model",           store,
                                       "enable-search",   FALSE,
                                       "headers-visible", TRUE,
                                       "rules-hint",      TRUE,
                                       NULL));

  /* first column: session name */
  renderer = GTK_CELL_RENDERER (g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                                             "editable", FALSE,
                                              NULL));

  column = GTK_TREE_VIEW_COLUMN (g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                              "title", _("Session Name"),
                                              "min-width", 300,
                                              "resizable", TRUE,
                                               NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_NAME);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* second column: Count */
  renderer = GTK_CELL_RENDERER (g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                                             "editable", FALSE,
                                              NULL));

  column = GTK_TREE_VIEW_COLUMN (g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                              "title", _("Count"),
                                              "sizing", GTK_TREE_VIEW_COLUMN_FIXED,
                                               NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_COUNT);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_window), treeview);

  gtk_widget_show_all (scrolled_window);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

  /* store pointer to treeview in Dialog */
  GSCHEM_HOOKUP_OBJECT (Dialog, treeview, "tree-view");

  return scrolled_window;
}

void x_sessions_manage_dialog(GschemToplevel *w_current)
{
  GtkWidget  *ThisDialog;
  GtkWidget  *action_area;
  GtkWidget  *main_vbox;
  GtkWidget  *table;
  GtkWidget  *scrollable;
  GtkWidget  *hbox;
  GtkWidget  *button;

  GtkTreeSelection  *selection;

  const char *rename_tip;
  const char *delete_tip;
  const char *export_tip;

  rename_tip   = _("Rename the selected session");
  delete_tip   = _("Delete the selected session");
  export_tip   = _("Export the selected session to a project file");

  ThisDialog = NEW_GSCHEM_DIALOG(_("Manage Sessions"),
                                    GSCHEM_DIALOG_MODAL,
                                   "manage-sessions",
                                    w_current);
  /* dialog initialization */
  g_object_set (G_OBJECT (ThisDialog),
                "window-position", GTK_WIN_POS_NONE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                "has-separator",   TRUE,
                NULL);

  main_vbox = GTK_DIALOG (ThisDialog)->vbox;
  g_object_set (main_vbox, "visible", TRUE, NULL);

  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,/* GtkContainer */
                                   "border-width", 5,
                                   "homogeneous",  FALSE,
                                   "spacing",      12,
                                   NULL));

  gtk_container_add (GTK_CONTAINER (main_vbox), hbox);
  g_object_set (hbox, "visible", TRUE, NULL);

  scrollable = x_sessions_get_treeview(ThisDialog);

  /* add the scrolled window to the dialog box */
  gtk_container_add (GTK_CONTAINER (hbox), scrollable);
  g_object_set (scrollable, "visible", TRUE, NULL);

  table = gtk_table_new (5, 1, FALSE);
  gtk_table_set_row_spacings (GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_box_pack_start(GTK_BOX (hbox), table, FALSE, FALSE, 0);
  g_object_set               (table, "visible", TRUE, NULL);

  button = gtk_button_new_with_mnemonic (_("_Rename"));
  g_object_set (button, "visible", TRUE, NULL);
  gtk_table_attach(GTK_TABLE(table), button, 0,1,0,1, GTK_FILL,0,0,0);
  gtk_widget_set_sensitive (button, FALSE);
  gtk_widget_set_tooltip_text(button, rename_tip);
  GSCHEM_HOOKUP_OBJECT (ThisDialog, button, "rename-butt");

  button = gtk_button_new_with_mnemonic (_("_Delete"));
  g_object_set (button, "visible", TRUE, NULL);
  gtk_table_attach(GTK_TABLE(table), button, 0,1,1,2, GTK_FILL,0,0,0);
  gtk_widget_set_sensitive (button, FALSE);
  gtk_widget_set_tooltip_text(button, delete_tip);
  GSCHEM_HOOKUP_OBJECT (ThisDialog, button, "delete-butt");

  button = gtk_button_new_with_mnemonic (_("_Export"));
  g_object_set (button, "visible", TRUE, NULL);
  gtk_table_attach(GTK_TABLE(table), button, 0,1,2,3, GTK_FILL,0,0,0);
  gtk_widget_set_sensitive (button, FALSE);
  gtk_widget_set_tooltip_text(button, export_tip);
  GSCHEM_HOOKUP_OBJECT (ThisDialog, button, "export-butt");

  action_area = create_action_area (GSCHEM_DIALOG(ThisDialog),
                                    (GtkWidget*) main_vbox);
  gtk_widget_show_all (action_area);

  selection = session_dialog_get_selection(GSCHEM_DIALOG(ThisDialog));

  g_signal_connect (selection, "changed",
                    G_CALLBACK (manage_session_selection_changed),
                    ThisDialog);

  g_signal_connect (G_OBJECT (ThisDialog), "response",
                    G_CALLBACK (x_sessions_response),
                    ThisDialog);

  session_tree_update (ThisDialog);

  gtk_widget_show (ThisDialog);
}

/*! \brief Open Session Dialog Treeview button Press Event
 *  \par Function Description
 *  This function is called when the user clicks a mouse button while
 *  over a treeview row. If the event was a "double-click" then a
 *  on_open_butt_clicked is called.
 *
 *  \note This function is not used by the Manage-Sessions dialog
 */
static bool
callback_treeview_button_pressed (GtkWidget      *widget,
                                  GdkEventButton *event,
                                  void           *user_data)
{
  GschemDialog  *ThisDialog = GSCHEM_DIALOG(user_data);
  bool           retval     = FALSE;
  GtkWidget     *button;
  TreeSelection *selection;
  GtkTreeModel  *model;
  GtkTreeIter    iter;

  selection = session_dialog_get_selection(ThisDialog);

  /* If something is selected */
  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    /* If was a double-left click */
    if (event->type == GDK_2BUTTON_PRESS && event->button == 1) {
      button = gtk_object_get_data(GTK_OBJECT(ThisDialog), "open-butt");
      on_open_butt_clicked (GTK_BUTTON(button), ThisDialog);
      retval = TRUE;
    }
  }
  return retval;
}
static void
open_session_selection_changed (GtkTreeSelection *selection,
                                            void *user_data)
{
  GtkWidget *Dialog = (GtkWidget*)user_data;
  GtkWidget *button;

  button = gtk_object_get_data(GTK_OBJECT(Dialog), "open-butt");

  /* If not already,  then enable the open button */
  if(!gtk_widget_get_sensitive (button)) {

    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_open_butt_clicked),
                      Dialog);
  }
}
void x_sessions_open_dialog(GschemToplevel *w_current)
{
  GtkWidget     *ThisDialog;
  GtkWidget     *action_area;
  GtkWidget     *main_vbox;
  GtkWidget     *scrollable;
  GtkWidget     *treeview;
  TreeSelection *selection;

  ThisDialog = NEW_GSCHEM_DIALOG(_("Open Session"),
                                    GSCHEM_DIALOG_MODAL,
                                   "manage-sessions",
                                    w_current);
  /* dialog initialization */
  g_object_set (G_OBJECT (ThisDialog),
                "window-position", GTK_WIN_POS_NONE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                "has-separator",   TRUE,
                NULL);

  main_vbox = GTK_DIALOG (ThisDialog)->vbox;
  g_object_set (main_vbox, "visible", TRUE, NULL);

  scrollable = x_sessions_get_treeview(ThisDialog);

  /* add the scrolled window to the dialog vbox */
  gtk_container_add (GTK_CONTAINER (main_vbox), scrollable);
  g_object_set (scrollable, "visible", TRUE, NULL);

  action_area = create_action_area (GSCHEM_DIALOG(ThisDialog),
                                   (GtkWidget*) main_vbox);

  gtk_widget_show_all (action_area);

  treeview = gtk_object_get_data(GTK_OBJECT(ThisDialog), "tree-view");
  selection = session_dialog_get_selection(GSCHEM_DIALOG(ThisDialog));

  g_signal_connect (selection, "changed",
                    G_CALLBACK (open_session_selection_changed),
                    ThisDialog);

  g_signal_connect (treeview,
                    "button-press-event",
                    G_CALLBACK (callback_treeview_button_pressed),
                    ThisDialog);

  g_signal_connect (G_OBJECT (ThisDialog), "response",
                    G_CALLBACK (x_sessions_response),
                    ThisDialog);

  session_tree_update (ThisDialog);

  gtk_widget_show (ThisDialog);
}
void x_sessions_new_dialog (GschemToplevel *w_current)
{
  char *name;

  name = geda_dialog_get_string("New Session", "Specify new name for Session:");

  if (name != NULL) { /* If  user did not cancel */
    i_sessions_new_session (w_current, name);
    GEDA_FREE(name);
  }
}
void x_sessions_save_as_dialog (GschemToplevel *w_current)
{
  char *name;

  name = geda_dialog_get_string("Save Session As", "Specify new name for Session:");

  if (name != NULL) { /* If  user did not cancel */
    i_sessions_save_session (w_current, name);
    GEDA_FREE(name);
  }
}

#undef TreeSelection