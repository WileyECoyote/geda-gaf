/* -*- C x_sessions.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
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
/*!
 * \file x_sessions.c
 * \brief Sessions Module
 */

#include <gschem.h>
#include <x_dialog.h>

#include <geda_dialogs.h>

#include <geda_debug.h>

/** \defgroup Gschem-Session-Dialogs Sessions Dialogs
 *  @{
 *  \ingroup Gschem-Session-System Standard-Dialogs
 *  \par This group contains core Routines for Sessions Dialogs.
 *   This group contains routines for 4 dialogs to support Gschem
 *   Sessions, the Sessions dialogs are:
 *
 *              1. Manage Sessions
 *              2. Open Session
 *              3. New Session
 *              4. Save Session As
 */

#define TreeSelection GtkTreeSelection

enum {
  COLUMN_NAME,
  COLUMN_COUNT,
  NUM_COLUMNS
};

/*!
 * \brief Populate tree model of <B>Session</B>'s treeview.
 * \par Function Description
 *  This function updates the tree model of <B>Session</B>\'s
 *  treeview by getting a pointer to the main Session GArray
 *  and appends a row to the tree-store for each record in the
 *  the array.
 *
 * \param [in] dialog  This Dialog object.
 */
static void session_tree_update (GtkWidget *dialog)
{
  GtkTreeStore *store;
  GtkTreeView  *treeview;

  treeview = GEDA_OBJECT_GET_DATA (dialog, "tree-view");
  store    = GTK_TREE_STORE(gtk_tree_view_get_model (treeview));

  /* wipe out every thing in the store */
  gtk_tree_store_clear (store);

  if (i_sessions_is_enabled()) { /* if session data exist */

    GArray *sessions;
    int     index;

    sessions = i_sessions_get_sessions();

    for (index = 0; index < sessions->len; index++) {

      GtkTreeIter iter;

      Session *record = &g_array_index(sessions, Session, index);

      /* add the record to the store */
      gtk_tree_store_append (store, &iter, NULL);

      gtk_tree_store_set (store, &iter,
                          COLUMN_NAME, record->session_name,
                          COLUMN_COUNT, record->page_count, -1);
    }
  }
}

/*!
 * \par Session Dialog helper to return the current selection
 *  in a sessions tree-view, used by both the Open Session and
 *  Manage Sessions dialogs
 */
static GtkTreeSelection *session_dialog_get_selection(GschemDialog *dialog)
{
  GtkTreeView *treeview;

  treeview = GEDA_OBJECT_GET_DATA (dialog, "tree-view");

  return gtk_tree_view_get_selection (treeview);
}

/*!
 * \brief Callback for the Rename Button on Manage Sessions dialog
 * \par Function Description
 *  Displays a simple dialog with a test entry field to accept a
 *  new name for the session. If the dialog is not canceled then
 *  the current session name is extracted from the tree selection
 *  and both strings are passed to i_sessions_rename_session. If
 *  successful, then the tree-view is updated.
 *
 * \param [in]   button    ptr to the button widget, not used
 * \param [in]   user_data ptr to the dialog object
 *
 * \sa on_delete_butt_clicked, on_export_butt_clicked
 */
static void on_rename_butt_clicked (GtkWidget *button, void *user_data)
{
  GschemDialog     *Dialog    = (GschemDialog*)user_data;
  GtkTreeSelection *selection;
  GtkTreeModel     *model;
  GtkTreeIter       iter;

  selection = session_dialog_get_selection(Dialog);

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    const char *old_name;
    char       *new_name;

    gtk_tree_model_get (model, &iter, COLUMN_NAME, &old_name, -1);

    new_name = geda_dialog_get_string(_("Rename Session"),
                                      _("Specify new name for Session:"),
                                         old_name);

    if (new_name != NULL) { /* If  user did not cancel */

      GschemToplevel *w_current = Dialog->w_current;

      if (i_sessions_rename_session(w_current, old_name, new_name)) {
        gtk_tree_store_set ((GtkTreeStore*)model, &iter,
                             COLUMN_NAME, new_name, -1);
      }
    }
    GEDA_FREE(new_name);
  }
}

/*!
 * \brief Callback for the Delete Button on Manage Sessions dialog
 * \par Function Description
 *  This is function called when the user activates the Export
 *  on the Manage Dialog. The function extracts the session name
 *  from the selection and displays a "Save As" dialog. If the
 *  user continues, the string returned from the dialog and the
 *  session named are passed to i_sessions_export_session.
 *
 * \param [in]   button    ptr to the button widget, not used
 * \param [in]   user_data ptr to the dialog object
 *
 * \sa on_delete_butt_clicked, on_rename_butt_clicked
 */
static void on_delete_butt_clicked (GtkWidget *button, void *user_data)
{
  GschemDialog      *Dialog    = (GschemDialog*)user_data;
  GschemToplevel    *w_current = Dialog->w_current;
  GtkTreeSelection  *selection;
  GtkTreeModel      *model;
  GtkTreeIter        iter;
  const char        *name;

  selection = session_dialog_get_selection(Dialog);

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    const char *msg = _("Confirm delete session");
    int response;

    gtk_tree_model_get (model, &iter, COLUMN_NAME, &name, -1);

    response = x_dialog_confirmation (msg, GTK_MESSAGE_QUESTION, FALSE);

    if (response == GEDA_RESPONSE_YES) {
      i_sessions_delete_session(w_current, name);
      gtk_tree_store_remove ((GtkTreeStore*)model, &iter);
    }
  }
}

/*!
 * \brief Callback for Export Button on Manage Sessions dialog
 * \par Function Description
 *  This function is called when the user activates the Export
 *  on the Manage Dialog. The function extracts the session name
 *  from the selection and displays a "Save As" dialog. If the
 *  user continues, the string returned from the dialog and the
 *  session named are passed to i_sessions_export_session.
 *
 * \param [in]   button    ptr to the button widget, not used
 * \param [in]   user_data ptr to the dialog object
 *
 * \sa on_rename_butt_clicked, on_export_butt_clicked
 */
static void on_export_butt_clicked (GtkWidget *button, void *user_data)
{
  GschemDialog      *Dialog    = (GschemDialog*)user_data;
  GtkTreeSelection  *selection;
  GtkTreeModel      *model;
  GtkTreeIter        iter;
  const char        *name;

  selection = session_dialog_get_selection(Dialog);

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    char *filename;

    gtk_tree_model_get (model, &iter, COLUMN_NAME, &name, -1);

    filename = x_dialog_select_file(NULL, _("Select export file"), name, FSB_SAVE);

    if (filename != NULL) { /* if user did not cancel */
      i_sessions_export_session(name, filename);
      geda_log_q("%s %s %s %s\n", _("Session"), name, _("exported to"), filename);
      GEDA_FREE(filename);
    }
  }
}

/*!
 * \brief Callback for Open Button on Session dialogs
 * \par Function Description
 *  This is function called when the user activates the Open
 *  button or double click on a tree row on the Open Dialog.
 *  The function extracts the session name from the selected
 *  model and passes the string to i_sessions_open_session.
 *  This function is shared by both the Manage Sessions and
 *  the Open Session dialogs.
 *
 * \note The Manage Session is not setup to respond to double
 *       click events.
 *
 * \param [in]   button    ptr to the button widget, not used
 * \param [in]   user_data ptr to the dialog object
 */
static void on_open_butt_clicked(GtkButton *button, void *user_data)
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

      g_signal_emit_by_name (Dialog, "response", GEDA_RESPONSE_REJECT, NULL);
    }
  }
}

/*!
 * \brief Page Manager Dialog Tree View Page Selected
 * \par Function Description
 *  This function is called whenever a row is selected in the
 *  treeview. Buttons are are not active, buttons are enabled
 *  and signals connected.
 *
 * \note This function is not used by the Open-Session dialog
 */
static void manage_session_selection_changed (GtkTreeSelection *selection,
                                                          void *user_data)
{
  GtkWidget *Dialog = (GtkWidget*)user_data;
  GtkWidget *button;

  button = GEDA_OBJECT_GET_DATA (Dialog, "rename-butt");

  if (!gtk_widget_get_sensitive (button)) {

    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_rename_butt_clicked),
                      Dialog);

    button = GEDA_OBJECT_GET_DATA (Dialog, "delete-butt");
    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_delete_butt_clicked),
                      Dialog);

    button = GEDA_OBJECT_GET_DATA (Dialog, "export-butt");
    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_export_butt_clicked),
                      Dialog);

    button = GEDA_OBJECT_GET_DATA (Dialog, "open-butt");
    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_open_butt_clicked),
                      Dialog);
  }
}

/** \defgroup Manage-Session-Dialog Manage Session Dialog
 *  @{
 *  \ingroup Gschem-Session-Dialogs
 *  \image html manage_sessions_dialog.png
 *  \image latex manage_sessions_dialog.png
 *  \par
 *   This group contains Routines for the Manage Sessions Dialog.
 *
 *  \note The first two function are common to both the Manage Sessions
 *        and Open Session dialogs.
 */

/*!
 * \brief Response function for Session dialogs
 * \ingroup Open-Session-Dialog
 * \par Function Description
 *  This is a response function called when the used selects one
 *  of the action bottons, either CLOSE or APPLY. This function
 *  is shared by both the Manage Sessions and the Open Session
 *  dialogs.
 *
 * \param [in] Dialog    ptr to the dialog widget
 * \param [in] response  int signal indicating which button
 * \param [in] nothing   Not used
 */
void x_sessions_response(GtkWidget *Dialog, int response, void *nothing)
{
  GschemToplevel *w_current = GSCHEM_DIALOG(Dialog)->w_current;

  switch (response) {
  case GEDA_RESPONSE_REJECT:
  case GEDA_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy (Dialog);
    break;

  case GEDA_RESPONSE_ACCEPT:
    break;

  default:
    BUG_IMSG ("unhandled case", response);
  }

  i_status_set_state (w_current, SELECT);
}

/*!
 * \brief Emit GEDA_RESPONSE_REJECT response signal
 * \ingroup Open-Session-Dialog
 * \par Function Description
 *  This function is common to both the Manage Sessions and the
 *  Open Session dialogs.
 */
static void on_close_butt_clicked(GtkButton *button, void *data)
{
  g_signal_emit_by_name (data, "response", GEDA_RESPONSE_REJECT, data);
}

/*!
 * \brief Manage Sessions dialog "Display at Startup" check button Toggled.
 * \par Function Description
 *  Executes whenever the check-button changes state. The function
 *  passes the state to i_sessions_set_show_at_startup function to
 *  update the configuration setting with the new value.
 */
static void callback_session_startup_toggled(GtkToggleButton *button,
                                                        void *nothing)
{
  bool state;

  state = gtk_toggle_button_get_active(button);

  i_sessions_set_show_at_startup(state);
}

/*!
 * \brief Manage Sessions Auto Update check button Toggled.
 * \par Function Description
 *  Executes whenever the check-button changes state. The function
 *  passes set the toplevel variable to the value of the state of
 *  the widget, either enabled or disabled.
 */
static void callback_session_auto_toggled (GtkToggleButton *button, void *data)
{
  GschemToplevel *w_current = data;

  w_current->auto_sessions =  gtk_toggle_button_get_active(button);
}

/*!
 * \brief Creates Action Area for the  for Manage Sessions dialog.
 * \par Function Description
 *  We create our own "Action Area", because; 1.) GTK's entire
 *  concept of an action area is more of a hindrance then it is
 *  useful, and 2.) We need access to the apply button widget.
 */
static GtkWidget *create_action_area (GschemDialog *ThisDialog,
                                         GtkWidget *parent)
{
  GschemToplevel *w_current = ThisDialog->w_current;
  GtkWidget  *action_hbox   = NULL;
  GtkWidget  *auto_check_butt;
  GtkWidget  *startup_checkbutt;
  const char *open_tip;
  const char *startup_tip;
  const char *update_tip;
  bool        show_image;
  int         butt_width;
  int         startup;

  open_tip    = _("Load the selected session");
  startup_tip = _("Show the Open Session dialog at start-up, over-rides auto-load-last feature");
  update_tip  = _("Automatically add and remove files from sessions");

  /* Create a Horizontal Box for everything to go into */
  NEW_HCONTROL_BOX(parent, action, DIALOG_H_SPACING);

  startup_checkbutt = gtk_check_button_new_with_mnemonic (_("Show at start-up"));
  g_object_set (startup_checkbutt, "visible", TRUE, NULL);
  gtk_box_pack_start ((GtkBox*)action_hbox, startup_checkbutt, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text(startup_checkbutt, startup_tip);
  startup = i_sessions_get_show_at_startup();
  SetToggleState (startup_checkbutt, startup);

  auto_check_butt = gtk_check_button_new_with_mnemonic (_("Auto update"));
  g_object_set (auto_check_butt, "visible", TRUE, NULL);
  gtk_box_pack_start ((GtkBox*)action_hbox, auto_check_butt, FALSE, FALSE, 0);
  gtk_widget_set_tooltip_text(auto_check_butt, update_tip);
  SetToggleState (auto_check_butt, w_current->auto_sessions);

  /* Create and connect the Close and Apply Buttons */
  GtkWidget *close_butt = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
  GtkWidget *open_butt  = gtk_button_new_from_stock (GTK_STOCK_OPEN);

  g_object_get (gtk_widget_get_settings ((GtkWidget*)close_butt),
                "gtk-button-images", &show_image, NULL);

  if (show_image) {
    butt_width = DIALOG_BUTTON_HSIZE + 24;
  }
  else {
    butt_width = DIALOG_BUTTON_HSIZE;
  }

  SetWidgetSize (close_butt, butt_width, DIALOG_BUTTON_VSIZE);
  SetWidgetSize (open_butt, butt_width, DIALOG_BUTTON_VSIZE);

  gtk_box_pack_end ((GtkBox*)action_hbox, close_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_box_pack_end ((GtkBox*)action_hbox, open_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_widget_set_sensitive (open_butt, FALSE);
  gtk_widget_set_tooltip_text(open_butt, open_tip);

  g_signal_connect (startup_checkbutt, "toggled",
                    G_CALLBACK (callback_session_startup_toggled),
                    NULL);

  g_signal_connect (auto_check_butt, "toggled",
                    G_CALLBACK (callback_session_auto_toggled),
                    w_current);

  g_signal_connect (close_butt, "clicked",
                    G_CALLBACK (on_close_butt_clicked),
                    ThisDialog);

  /* Set the alternative button order (ok, cancel, help) for other systems
  gtk_dialog_set_alternative_button_order((GtkDialog*)ThisDialog,
                                          GEDA_RESPONSE_ACCEPT,
                                          GEDA_RESPONSE_REJECT,
                                          -1);
 */
  GEDA_HOOKUP_OBJECT(ThisDialog, open_butt,  "open-butt");
  GEDA_HOOKUP_OBJECT(ThisDialog, close_butt, "close-butt");

  return action_hbox;
}

/*!
 * \brief Returns a Session treeview for the  Manage Sessions dialog.
 * \par Function Description
 *  This function adds a treeview and caption to display the content
 *  of the dialog model of pages with unsaved changes.
 *
 *  The treeview displays the page names with check boxes.
 *
 * \param [in] Dialog The dialog.
 *
 * \returns A pointer on the GtkVBox to add to dialog.
 */
static GtkWidget *x_sessions_get_treeview (GtkWidget *Dialog)
{
  GtkTreeViewColumn *column;
  GtkCellRenderer   *renderer;
  GtkTreeModel      *store;
  GtkTreeView       *treeview;
  GtkWidget         *scrolled_window;
  GtkTreeSelection  *selection;

  /* create the model for the treeview */
  store = (GtkTreeModel*)gtk_tree_store_new (NUM_COLUMNS,
                                             G_TYPE_STRING, /* name */
                                             G_TYPE_INT);   /* Count */

  /* create a scrolled window for the treeview */
  scrolled_window = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                  "border-width",       DIALOG_BORDER_WIDTH,
                                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                  "vscrollbar-policy",    GTK_POLICY_ALWAYS,
                                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                                  NULL);

  /* create model for treeview and populate */
  treeview = g_object_new (GTK_TYPE_TREE_VIEW,
                           /* GtkTreeView */
                           "model",           store,
                           "enable-search",   FALSE,
                           "headers-visible", TRUE,
                           "rules-hint",      TRUE,
                           NULL);

  /* first column: session name */
  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                           "editable", FALSE,
                           NULL);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         "title", _("Session Name"),
                         "min-width", 300,
                         "resizable", TRUE,
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_NAME);
  gtk_tree_view_append_column (treeview, column);

  /* second column: Count */
  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                           "editable", FALSE,
                           NULL);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         "title", _("File Count"),
                         "sizing", GTK_TREE_VIEW_COLUMN_FIXED,
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_COUNT);
  gtk_tree_view_append_column (treeview, column);

  /* add the treeview to the scrolled window */
  geda_container_add (scrolled_window, treeview);

  gtk_widget_show_all (scrolled_window);

  selection = gtk_tree_view_get_selection (treeview);
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

  /* store pointer to treeview in Dialog */
  GEDA_HOOKUP_OBJECT (Dialog, treeview, "tree-view");

  return scrolled_window;
}

/*!
 * \brief Manage Sessions Dialog
 * \par Function Description
 *  Creates and displays the Manage Sessions Dialog.
 *
 * \note This function shares the reponse callback and other
 *       routines with the Open Session dialog
 *
 * \sa x_sessions_open_dialog
 *
 * \param w_current Pointer to GschemToplevel object
 */
void x_sessions_manage_dialog(GschemToplevel *w_current)
{
  GtkWidget  *ThisDialog;
  GtkWidget  *action_area;
  GtkWidget  *main_vbox;
  GtkWidget  *scrollable;
  GtkWidget  *hbox;
  GtkWidget  *button;
  GtkTable   *table;

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
  g_object_set (ThisDialog,
                "window-position", GTK_WIN_POS_NONE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                "has-separator",   FALSE,
                NULL);

  main_vbox = GTK_DIALOG (ThisDialog)->vbox;
  gtk_widget_show (main_vbox);

  hbox = g_object_new (GTK_TYPE_HBOX,/* GtkContainer */
                       "border-width", 5,
                       "homogeneous",  FALSE,
                       "spacing",      12,
                       NULL);

  geda_container_add (main_vbox, hbox);
  gtk_widget_show (hbox);

  scrollable = x_sessions_get_treeview(ThisDialog);

  /* Add the scrolled window to the dialog box */
  geda_container_add (hbox, scrollable);
  g_object_set (scrollable, "visible", TRUE, NULL);

  table = (GtkTable*)gtk_table_new (5, 1, FALSE);
  gtk_table_set_row_spacings (table, DIALOG_V_SPACING);
  gtk_table_set_col_spacings (table, DIALOG_H_SPACING);
  gtk_box_pack_start         (GTK_BOX (hbox), (GtkWidget*)table, FALSE, FALSE, 0);
  gtk_widget_show            ((GtkWidget*)table);

  button = gtk_button_new_with_mnemonic (_("_Rename"));
  gtk_widget_show            (button);
  gtk_widget_set_sensitive   (button, FALSE);
  gtk_widget_set_tooltip_text(button, rename_tip);
  gtk_table_attach           (table, button, 0,1,0,1, GTK_FILL,0,0,0);
  GEDA_HOOKUP_OBJECT         (ThisDialog, button, "rename-butt");

  button = gtk_button_new_with_mnemonic (_("_Delete"));
  gtk_widget_show            (button);
  gtk_widget_set_sensitive   (button, FALSE);
  gtk_widget_set_tooltip_text(button, delete_tip);
  gtk_table_attach           (table, button, 0,1,1,2, GTK_FILL,0,0,0);
  GEDA_HOOKUP_OBJECT         (ThisDialog, button, "delete-butt");

  button = gtk_button_new_with_mnemonic (_("_Export"));
  gtk_widget_show            (button);

  gtk_widget_set_sensitive   (button, FALSE);
  gtk_widget_set_tooltip_text(button, export_tip);
  gtk_table_attach           (table, button, 0,1,2,3, GTK_FILL,0,0,0);
  GEDA_HOOKUP_OBJECT         (ThisDialog, button, "export-butt");

  action_area = create_action_area ((GschemDialog*)ThisDialog, main_vbox);

  gtk_widget_show_all (action_area);

  selection = session_dialog_get_selection((GschemDialog*)ThisDialog);

  g_signal_connect (selection, "changed",
                    G_CALLBACK (manage_session_selection_changed),
                    ThisDialog);

  g_signal_connect (ThisDialog, "response",
                    G_CALLBACK (x_sessions_response),
                    ThisDialog);

  session_tree_update (ThisDialog);

  gtk_widget_show (ThisDialog);
}

/** @} end group Manage-Session-Dialog */

/** \defgroup Open-Session-Dialog Open Session Dialog
 *  @{
 *  \ingroup Gschem-Session-Dialogs
 *  \todo image html open_sessions_dialog.png
 *  \todo image latex open_sessions_dialog.png
 *  \par
 *   This group contains Routines for the Open Session Dialog.
 *
 *  \note x_sessions_response and on_close_butt_clicked in the
 *        Manage Sessions dialog are also callbacks for the Open
 *        Sessions dialog.
 */

/*!
 * \brief Open Session Dialog Treeview button Press Event
 * \par Function Description
 *  This function is called when the user clicks a mouse button while
 *  over a treeview row. If the event was a "double-click" then a
 *  on_open_butt_clicked is called.
 *
 * \note This function is not used by the Manage-Sessions dialog
 */
static bool callback_treeview_button_pressed (GtkWidget      *widget,
                                              GdkEventButton *event,
                                              void           *user_data)
{
  GschemDialog  *ThisDialog = GSCHEM_DIALOG(user_data);
  bool           retval     = FALSE;
  TreeSelection *selection;
  GtkTreeModel  *model;
  GtkTreeIter    iter;

  selection = session_dialog_get_selection(ThisDialog);

  /* If something is selected */
  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    /* If was a double-left click */
    if (event->type == GDK_2BUTTON_PRESS && event->button == 1) {
      GtkButton *button = GEDA_OBJECT_GET_DATA(ThisDialog, "open-butt");
      on_open_butt_clicked (button, ThisDialog);
      retval = TRUE;
    }
  }
  return retval;
}

/*! \brief Open Session Dialog Treeview Selection Event
 *  \par Function Description
 *  This function is called when the user select anything in the
 *  session treeview, in which case this routine enables and sets
 *  up the callback for the Open button
 *
 *  \note This function is not used by the Manage-Sessions dialog
 */
static void open_session_selection_changed (GtkTreeSelection *selection,
                                                        void *user_data)
{
  GtkWidget *Dialog = (GtkWidget*)user_data;
  GtkWidget *button;

  button = GEDA_OBJECT_GET_DATA (Dialog, "open-butt");

  /* If not already,  then enable the open button */
  if (!gtk_widget_get_sensitive (button)) {

    gtk_widget_set_sensitive (button, TRUE);
    g_signal_connect (button, "clicked",
                      G_CALLBACK (on_open_butt_clicked),
                      Dialog);
  }
}

/*!
 * \brief Open Session Dialog
 * \par Function Description
 *  Creates and displays the Manage Sessions Dialog. If no file
 *  names are provided on the command-line, and the configuration
 *  variable sessions-at-startup is TRUE, and data exist for a least
 *  one session, then this Dialog is presented during program start-
 *  up to give user a change to select a session from a list of saved
 *  sessions. A Session is just a collection of opened documents.
 *
 * \note This function shares the reponse callback and other
 *       routines with the Manage Session dialog
 *
 * \sa x_sessions_open_dialog
 *
 * \param w_current Pointer to GschemToplevel object
 */
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
  g_object_set (ThisDialog,
                "window-position", GTK_WIN_POS_NONE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                "has-separator",   FALSE,
                NULL);

  main_vbox = GTK_DIALOG (ThisDialog)->vbox;
  g_object_set (main_vbox, "visible", TRUE, NULL);

  scrollable = x_sessions_get_treeview(ThisDialog);

  /* add the scrolled window to the dialog vbox */
  geda_container_add (main_vbox, scrollable);
  g_object_set (scrollable, "visible", TRUE, NULL);

  action_area = create_action_area (GSCHEM_DIALOG(ThisDialog),
                                   (GtkWidget*) main_vbox);

  gtk_widget_show_all (action_area);

  treeview  = GEDA_OBJECT_GET_DATA(ThisDialog, "tree-view");
  selection = session_dialog_get_selection(GSCHEM_DIALOG(ThisDialog));

  g_signal_connect (selection, "changed",
                    G_CALLBACK (open_session_selection_changed),
                    ThisDialog);

  g_signal_connect (treeview,
                    "button-press-event",
                    G_CALLBACK (callback_treeview_button_pressed),
                    ThisDialog);

  g_signal_connect (ThisDialog, "response",
                    G_CALLBACK (x_sessions_response),
                    ThisDialog);

  session_tree_update (ThisDialog);

  gtk_widget_show (ThisDialog);
}

/** @} end group Open-Session-Dialog */

/** \defgroup New-Session-Dialog New Session Dialog
 *  @{
 *  \ingroup Gschem-Session-Dialogs
 *  \par
 *   This group contains a single function for the New Session Dialog.
 */

/*!
 * \brief Create a New Session Dialog
 * \par Function Description
 *  Displays a simple dialog with a test entry field to accept
 *  a name for the new session. If the dialog is not canceled
 *  then i_sessions_new_session is called to create the session
 *  passing the text from the entry widget.
 *
 * \param w_current Pointer to #GschemToplevel Object
 *
 * \sa x_sessions_save_as_dialog
 */
void x_sessions_new_dialog (GschemToplevel *w_current)
{
  char *name;

  name = geda_dialog_get_string(_("New Session"),
                                _("Specify new name for Session:"), NULL);

  if (name != NULL) { /* If  user did not cancel */
    i_sessions_new_session (w_current, name);
    GEDA_FREE(name);
  }
}

/** @} end group New-Session-Dialog */

/** \defgroup Save-As-Session-Dialog Save-As Session Dialog
 *  @{
 *  \ingroup Gschem-Session-Dialogs
 *  \par
 *   This group contains a single function for the Save-As Session Dialog.
 */

/*!
 * \brief Save As Session Dialog
 * \par Function Description
 *  Displays a simple dialog with a test entry field to accept
 *  a name for the session. If the dialog is not canceled then
 *  i_sessions_save_session is called to create the session
 *  passing the text from the entry widget.
 *
 * \param w_current Pointer to #GschemToplevel Object
 *
 * \sa i_sessions_save_session, x_sessions_save_as_dialog
 */
void x_sessions_save_as_dialog (GschemToplevel *w_current)
{
  char *name;

  name = geda_dialog_get_string(_("Save Session As"), _("Specify new name for Session:"), NULL);

  if (name != NULL) { /* If  user did not cancel */
    i_sessions_save_session (w_current, name);
    GEDA_FREE(name);
  }
}

/** @} end group Save-As-Session-Dialog */

/*!
 * \brief Save Session Configuration Settings and Auto Session
 * \par Function Description
 *  This function is called at shutdown to preserve the state of
 *  the auto_sessions variable to the configuration system. The
 *  function first saves the current session if appropriate.
 *  The auto_sessions variable is restored by #i_sessions_init.
 *
 * \param w_current Pointer to #GschemToplevel Object
 *
 * \sa i_sessions_init
 */
void x_sessions_save_settings (GschemToplevel *w_current)
{
  EdaConfig  *cfg = eda_config_get_user_context ();
  const char *group_name = SESSIONS_CONFIG_GROUP;

  if (w_current->auto_sessions > 0 && w_current->session_name) {
    i_sessions_save_session (w_current, NULL);
  }

  eda_config_set_boolean (cfg, group_name, "auto-update-sessions",
                          w_current->auto_sessions);
}

/** @} endgroup Gschem-Session-Dialogs */
#undef TreeSelection