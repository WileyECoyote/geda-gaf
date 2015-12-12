/* -*- C x_pagesel.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * MA 02110-1301 USA
 */
/*!
 * \file x_pagesel.c
 * \brief Page Manager Dialog
 */

#include <config.h>

#include <gschem.h>
#include <i_actions.h>
#include <x_dialog.h>

#include <geda_debug.h>

#define ThisDialog pagesel

/** \defgroup Page-Select-Dialog Page Select Dialog
 *  @{
 *  \ingroup (Standard-Dialogs)
 *  \image html page_man_dialog.png
 *  \image latex page_man_dialog.png
 */

/* Enumerate Control IDs */
typedef enum {
       ShowFullName,

} ControlID;

static GschemDialogClass *pagesel_parent_class = NULL;

static WidgetStringData DialogStrings[] = {
  { "ShowFullNameSwitch",  "Show full names", "Enable or disable displaying of paths in file names"},
        { NULL, NULL, NULL},
};

static void x_pagesel_callback_response (GtkDialog *d, int r, void *log);

/*! \brief Open the page manager dialog.
 *  \par Function Description
 *  Opens the page manager dialog for <B>toplevel</B> if it is not already.
 *  In this last case, it raises the dialog.
 *
 *  \param [in] w_current  The GschemToplevel object to open page manager for.
 */
void x_pagesel_open (GschemToplevel *w_current)
{
  if (w_current->pswindow == NULL) {

    w_current->pswindow = GTK_WIDGET (g_object_new (TYPE_PAGESEL,
                                                    /* GschemDialog */
                                                    "settings-name",   IDS_PAGE_MANAGER,
                                                    "gschem-toplevel", w_current,
                                                    NULL));

    g_signal_connect (w_current->pswindow,
                      "response",
                      G_CALLBACK (x_pagesel_callback_response),
                      w_current);

    gtk_widget_show (w_current->pswindow);
  }
  else {
    gtk_window_present(GTK_WINDOW(w_current->pswindow));
  }

}

/*! \brief Close the page manager dialog.
 *  \par Function Description
 *  Closes the page manager dialog associated with <B>toplevel</B>.
 *
 *  \param [in] w_current  The GschemToplevel object to close page manager for.
 */
void x_pagesel_close (GschemToplevel *w_current)
{
  if (w_current->pswindow) {
    if (IS_PAGESEL (w_current->pswindow)) {
      gtk_widget_destroy (w_current->pswindow);
    }
    else {
      BUG_MSG ("pswindow is wrong object");
    }
  }
}

/*! \brief Update the list and status of <B>toplevel</B>'s pages.
 *  \par Function Description
 *  Updates the list and status of <B>toplevel</B>\'s pages if the page
 *  manager dialog is opened.
 *
 *  \param [in] w_current  The GschemToplevel object to update.
 */
void x_pagesel_update (GschemToplevel *w_current)
{
  if (w_current->pswindow) {
    if (IS_PAGESEL (w_current->pswindow)) {
      pagesel_update (PAGESEL (w_current->pswindow));
    }
    else {
      BUG_MSG ("pswindow is wrong object");
    }
  }
}

/*! \brief Callback for page manager response.
 *  \par Function Description
 *  Handles response <B>arg1</B> of the page manager dialog <B>dialog</B>.
 *
 *  \param [in] dialog   GtkDialog that issues callback.
 *  \param [in] response Response argument of page manager dialog.
 *  \param [in] data     Pointer to relevant GschemToplevel structure.
 */
static
void x_pagesel_callback_response (GtkDialog *dialog, int response, void *data)
{
  GschemToplevel *w_current;

  w_current = (GschemToplevel*)data;

  switch (response) {
      case GEDA_RESPONSE_DELETE_EVENT:
      case GEDA_RESPONSE_CLOSE:
        if (IS_PAGESEL (w_current->pswindow)) {
          gtk_widget_destroy (GTK_WIDGET (dialog));
        }
        else {
          BUG_MSG ("pswindow is wrong object");
        }
        break;
      case GEDA_RESPONSE_REFRESH:
        pagesel_update (PAGESEL (dialog));
        break;
      default:
        BUG_IMSG("unhandled case <%d>", response);
  }
}

enum {
  COLUMN_PAGE,
  COLUMN_NAME,
  COLUMN_CHANGED,
  COLUMN_SPACER,
  NUM_COLUMNS
};

static void pagesel_class_init (PageselClass *class);
static void pagesel_instance_init       (Pagesel *pagesel);
static void pagesel_popup_menu (Pagesel *pagesel, GdkEventButton *event);

/*! \brief Page Manager Dialog Tree View Page Selected
 *  \par Function Description
 *  This function is called whenever a row is selected in the
 *  treeview. The page associated with the selected row is set
 *  to be the Current page.
 */
static void
pagesel_callback_selection_changed (GtkTreeSelection *selection,
                                    void             *user_data)
{
  GtkTreeModel   *model;
  GtkTreeIter     iter;
  Pagesel        *pagesel = (Pagesel*)user_data;
  GschemToplevel *w_current;
  Page           *page;

  if (!gtk_tree_selection_get_selected (selection, &model, &iter)) {
    return;
  }

  w_current = GSCHEM_DIALOG (pagesel)->w_current;

  gtk_tree_model_get (model, &iter, COLUMN_PAGE, &page, -1);

  /* Since setting the current page may call x_pagesel_update(), which
   * might change the current page selection, make sure we do nothing
   * if the newly-selected page is already the current page. */
  if (page != w_current->toplevel->page_current) {
    x_window_set_current_page (w_current, page);
  }
}

/*! \brief Page Manager Dialog Treeview button Press Event
 *  \par Function Description
 *  This function is called when the user clicks a mouse button while
 *  over a treeview row. If the event was a "right-click" then a
 *  a the pagesel_popup_menu () function is called to present a menu.
 */
static bool pagesel_callback_button_pressed (GtkWidget      *widget,
                                             GdkEventButton *event,
                                             void           *user_data)
{
  Pagesel *pagesel = (Pagesel*)user_data;
  bool ret_val;

  if (event->type == GDK_BUTTON_PRESS) {
    if (event->button == 2) {
      GschemToplevel *w_current = GSCHEM_DIALOG(pagesel)->w_current;
      gtk_window_present(GTK_WINDOW(w_current->main_window));
      ret_val = FALSE;
    }
    else if (event->button == 3) {
      pagesel_popup_menu (pagesel, event);
      ret_val = TRUE;
    }
    else {
      ret_val = FALSE;
    }
  }
  else {
    ret_val = FALSE;
  }

  return ret_val;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static bool pagesel_callback_popup_menu (GtkWidget *widget,
                                         void      *user_data)
{
  Pagesel *pagesel = (Pagesel*)user_data;

  pagesel_popup_menu (pagesel, NULL);

  return TRUE;
}

#define DEFINE_POPUP_CALLBACK(name, action) \
static void \
pagesel_callback_popup_##name (GtkMenuItem *menuitem, void *pagesel) { \
  i_command_process(GSCHEM_DIALOG (pagesel)->w_current, \
                    action, 0, NULL, ID_ORIGIN_MENU); \
}

DEFINE_POPUP_CALLBACK (new_page,     ACTION(FILE_NEW))
DEFINE_POPUP_CALLBACK (open_page,    ACTION(FILE_OPEN))
DEFINE_POPUP_CALLBACK (save_page,    ACTION(FILE_SAVE))
DEFINE_POPUP_CALLBACK (close_page,   ACTION(PAGE_CLOSE))
DEFINE_POPUP_CALLBACK (discard_page, ACTION(PAGE_DISCARD))

/*! \brief Popup context-sensitive menu.
 *  \par Function Description
 *  Pops up a context-sensitive menu.
 *
 *  <B>event</B> can be NULL if the popup is triggered by a key binding
 *  instead of a mouse click.
 *
 *  \param [in] pagesel  The Pagesel object.
 *  \param [in] event    Mouse click event info.
 */
static void pagesel_popup_menu (Pagesel        *pagesel,
                                GdkEventButton *event)
{
  GtkTreePath *path;
  GtkWidget   *menu;

  struct menuitem_t {
    char *label;
    GCallback callback;
  };

  struct menuitem_t menuitems[] = {
    { N_("New Page"),     G_CALLBACK (pagesel_callback_popup_new_page)     },
    { N_("Goto Page..."), G_CALLBACK (pagesel_callback_popup_open_page)    },
    { "-",                NULL                                             },
    { N_("Save Page"),    G_CALLBACK (pagesel_callback_popup_save_page)    },
    { N_("Close Page"),   G_CALLBACK (pagesel_callback_popup_close_page)   },
    { N_("Discard Page"), G_CALLBACK (pagesel_callback_popup_discard_page) },
    { NULL,               NULL                                             }};
  struct menuitem_t *tmp;

  if (event != NULL &&
      gtk_tree_view_get_path_at_pos (pagesel->treeview,
                                     (int)event->x,
                                     (int)event->y,
                                     &path, NULL, NULL, NULL)) {
    GtkTreeSelection *selection;
    selection = gtk_tree_view_get_selection (pagesel->treeview);
    gtk_tree_selection_unselect_all (selection);
    gtk_tree_selection_select_path (selection, path);
    gtk_tree_path_free (path);
  }

  /* create the context menu */
  menu = gtk_menu_new();
  for (tmp = menuitems; tmp->label != NULL; tmp++) {
    GtkWidget *menuitem;
    if (g_ascii_strcasecmp (tmp->label, "-") == 0) {
      menuitem = gtk_separator_menu_item_new ();
    }
    else {
      menuitem = gtk_menu_item_new_with_label (_(tmp->label));
      g_signal_connect (menuitem,
                        "activate",
                        tmp->callback,
                        pagesel);
    }
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  }
  gtk_widget_show_all (menu);
  /* make menu a popup menu */
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
}

/*! \brief Handler for the notify::gschem-toplevel signal of GschemDialog
 *
 *  \par Function Description
 *  When the gschem-toplevel property is set on the parent GschemDialog,
 *  we should update the pagesel dialog.
 *
 *  \param [in] gobject    Object which received the signal.
 *  \param [in] arg1       GParamSpec of the property which changed
 *  \param [in] user_data  user data set when the signal handler was connected.
 */
static void notify_gschem_toplevel_cb (GObject    *gobject,
                                       GParamSpec *arg1,
                                       void       *user_data)
{
  Pagesel *pagesel = PAGESEL( gobject );
  pagesel_update( pagesel );
}

/*! \brief Sets data for a particular cell of the In Use treeview.
 *  \par Function Description
 *  This function determines what data is to be displayed in the cells
 *  in the filename column, dynamically, so the pointer to the string
 *  can be advanced past the path portion based on user preference.
 */
static void
pagesel_treeview_set_cell_filename (GtkTreeViewColumn *tree_column,
                                    GtkCellRenderer   *cell,
                                    GtkTreeModel      *tree_model,
                                    GtkTreeIter       *iter,
                                    void              *data)
{
  Page *page;

  gtk_tree_model_get (tree_model, iter, COLUMN_PAGE, &page, -1);

  /* Is important to check the Page object here because if a page was closed
   * or discarded via the popup menu this function will be called before the
   * tree is updated, in which case we do not have an answer, so we take the
   * ostrich approach, that is, we turn a blind-eye and do nothing, seems to
   * work.
   */
  if (GEDA_IS_PAGE(page)) {

    Pagesel    *pagesel = data;
    GtkWidget  *widget  = GET_EDA_OBJECT(ShowFullName);
    int         state   = GET_SWITCH_STATE(widget);

    const char *filename;

    filename = state ? page->filename : f_get_basename(page->filename);
    g_object_set ((GObject*)cell, "text", filename, NULL);
  }
}

/*! \brief Regenerate attribute list when the visibility
 *         setting  changes and toggle switch image
 *  \par Function Description: This function changes images for
 *       show_inherited switch to the opposite state, i.e. if ON
 *       use OFF image and if OFF use ON image. The function then
 *       calls multiattrib_update to update the attribute list.
 */
static void pagesel_show_fullnames_toggled (GtkWidget *widget,
                                            Pagesel   *pagesel)
{
  TOGGLE_SWITCH(widget);
  pagesel_update(pagesel);
  gtk_tree_view_columns_autosize(pagesel->treeview);
  return;
}

static void
pagesel_callback_refresh_clicked (GtkButton *RefreshButt, void *user_data)
{
  gtk_dialog_response (GTK_DIALOG (user_data), GEDA_RESPONSE_REFRESH);
}

static void
pagesel_callback_close_clicked (GtkButton *CloseButt, void *user_data)
{
  gtk_dialog_response (GTK_DIALOG (user_data), GEDA_RESPONSE_CLOSE);
}

/*! \brief Geda Box Object Finalization Function
 *  \par Function Description
 *   Save the user preference to configuration system.
 */
static void pagesel_finalize(GObject *object)
{
  Pagesel    *pagesel = PAGESEL(object);
  GtkWidget  *widget  = GET_EDA_OBJECT(ShowFullName);
  EdaConfig  *cfg     = eda_config_get_user_context();
  const char *group   = IDS_PAGE_MANAGER;
  const char *key     = "full-names";
        int   value   = GET_SWITCH_STATE(widget);

  eda_config_set_boolean (cfg, group, key, value);

  /* Chain up to parent Class */
  G_OBJECT_CLASS(pagesel_parent_class)->finalize(object);
}

/*! \brief Function to retrieve pagesel's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve pagesel's Type identifier. On the first call,
 *  this registers the pagesel in the GedaTypesystem.  Subsequently
 *  the functions returns the saved value from its first execution.
 *
 *  \return the Type identifier associated with pagesel.
 */
GedaType pagesel_get_type()
{
  static GedaType pagesel_type = 0;

  if (!pagesel_type) {
    static const GTypeInfo pagesel_info = {
      sizeof(PageselClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) pagesel_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(Pagesel),
      0,    /* n_preallocs */
      (GInstanceInitFunc) pagesel_instance_init
    };

    pagesel_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                           "Pagesel",
                                           &pagesel_info, 0);
  }

  return pagesel_type;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void pagesel_class_init (PageselClass *class)
{
  GObjectClass *gobject_class  = G_OBJECT_CLASS (class);

  pagesel_parent_class         = g_type_class_peek_parent (class);

  gobject_class->finalize      = pagesel_finalize;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void pagesel_instance_init (Pagesel *pagesel)
{
  GtkWidget *scrolled_win, *treeview, *label;
  GtkTreeModel *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection  *selection;

  EdaConfig  *cfg   = eda_config_get_user_context ();
  const char *group = IDS_PAGE_MANAGER;
  const char *key   = "full-names";

  int full_names;

  i_var_restore_group_boolean(cfg, group, key, &full_names, TRUE);

  /* dialog initialization */
  g_object_set (G_OBJECT (pagesel),
                /* GtkContainer */
                "border-width",    DIALOG_BORDER_WIDTH,
                /* GtkWindow */
                "title",           _("Page Manager"),
                "default-height",  180,
                "default-width",   250,
                "modal",           FALSE,
                "window-position", GTK_WIN_POS_MOUSE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

  /* create the model for the treeview */
  store = (GtkTreeModel*)gtk_tree_store_new (NUM_COLUMNS,
                                             G_TYPE_POINTER,  /* page */
                                             G_TYPE_STRING,   /* name */
                                             G_TYPE_BOOLEAN,  /* changed */
                                             G_TYPE_POINTER);

  /* create a scrolled window for the treeview */
  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkContainer */
                  "border-width",      DIALOG_BORDER_WIDTH,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_ALWAYS,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));

  /* create the treeview */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "model",      store,
                                       "rules-hint", TRUE,
                                       NULL));

  g_signal_connect (treeview, "button-press-event",
                    G_CALLBACK (pagesel_callback_button_pressed),
                    pagesel);

  g_signal_connect (treeview,
                    "popup-menu",
                    G_CALLBACK (pagesel_callback_popup_menu),
                    pagesel);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (pagesel_callback_selection_changed),
                    pagesel);

  /* ---------------------- first column: page name ---------------------- */

  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                  /* GtkCellRendererText */
                  "editable", FALSE,
                  NULL));

  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Filename"),
                  "min-width", COLUMN_NAME_MIN_WIDTH,
                  "sizing",GTK_TREE_VIEW_COLUMN_AUTOSIZE,
                  "resizable", TRUE,
                  NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_NAME);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           pagesel_treeview_set_cell_filename,
                                           pagesel, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* --------------------- second column: changed  ---------------------- */
  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
                  /* GtkCellRendererToggle */
                  "activatable", FALSE,
                  NULL));

  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Changed"),
                  "min-width", COLUMN_CHANGED_MIN_WIDTH,
                  NULL));

  gtk_tree_view_column_pack_start (column, renderer, FALSE);
  gtk_tree_view_column_add_attribute (column, renderer, "active", COLUMN_CHANGED);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* --------------------- third column is a spacer ---------------------- */
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "expand", TRUE,
                  "resizable", FALSE,
                  "sizing",GTK_TREE_VIEW_COLUMN_FIXED,
                  NULL));

  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), treeview);

  /* set treeview of pagesel */
  pagesel->treeview = GTK_TREE_VIEW (treeview);

  /* add the scrolled window to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (pagesel)->vbox), scrolled_win, TRUE, TRUE, 0);

  gtk_widget_show_all (scrolled_win);

  /* add a label below the scrolled window */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkLabel */
                                    "label", _("Right click on rows for more options..."),
                                    NULL));

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (ThisDialog)->vbox), label, FALSE, FALSE, 5);
  gtk_widget_show (label);

  /* -------------------- Setup the Action/Button Area ------------------- */
  GtkWidget *action_hbox;
  GtkWidget *switch_vbox;
  GtkWidget *butt_hbox;
  GtkWidget *ShowFullNameSwitch;
  GtkWidget *fresh_butt;
  GtkWidget *close_butt;
  GtkWidget *alignment;
  GtkDialog *Dialog;

  Dialog = (GtkDialog*)ThisDialog;

  /* Remove Gtk action area from the dialog and don't re-use it */
  action_hbox = Dialog->action_area;
  gtk_container_remove(GTK_CONTAINER(Dialog->vbox),GTK_WIDGET(action_hbox));

  action_hbox = gtk_hbox_new(FALSE, 0);
  g_object_set (action_hbox, "visible", TRUE, NULL);
  gtk_box_pack_end (GTK_BOX (Dialog->vbox), action_hbox, FALSE, FALSE, 0);

  /* Replace the action_area with the new container */
  Dialog->action_area = action_hbox;

  /* Create and add container for filename option toggle switch */
  switch_vbox = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start (GTK_BOX (action_hbox), switch_vbox, FALSE, FALSE, 0);

  /* Setup the full-name option toggle switch */
  ShowFullNameSwitch = NULL;

  /* Create a new Toggle Switch widget */
  EDA_SWITCH( (GTK_WIDGET(ThisDialog)), switch_vbox, ShowFullName, 0, full_names);
  gtk_widget_show_all(switch_vbox); /* set every widget in container visible */

  /* Setup callback for Toggle Switch widget */
  GEDA_CALLBACK_SWITCH (ShowFullName, pagesel_show_fullnames_toggled, ThisDialog)

  /* Create and add alignment container to hold the button container */
  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        "right-padding", 0,
                                        "left-padding",  50,
                                        "xscale",        1.0,
                                        "yscale",        0.0,
                                        "xalign",        1.0,
                                        "yalign",        0.5,
                                        NULL));

  g_object_set (alignment, "visible", TRUE, NULL);
  gtk_box_pack_end (GTK_BOX (action_hbox), alignment, TRUE, TRUE, 0);

  /* Create a Horizontal Box for the buttons to go into */
  butt_hbox = gtk_hbox_new(FALSE, 0);
  g_object_set (butt_hbox, "visible", TRUE, NULL);
  gtk_container_add (GTK_CONTAINER (alignment), butt_hbox);

  /* Create and connect the Close and Refresh Buttons */
  fresh_butt = gtk_button_new_from_stock (GTK_STOCK_REFRESH);
  close_butt = gtk_button_new_from_stock (GTK_STOCK_CLOSE);

  g_signal_connect( ThisDialog, "notify::gschem-toplevel",
                    G_CALLBACK( notify_gschem_toplevel_cb ), NULL );

  g_signal_connect (fresh_butt,
                    "clicked",
                    G_CALLBACK (pagesel_callback_refresh_clicked),
                    pagesel);

  g_signal_connect (close_butt,
                    "clicked",
                    G_CALLBACK (pagesel_callback_close_clicked),
                    pagesel);

  g_object_set (close_butt, "visible", TRUE, NULL);
  g_object_set (fresh_butt, "visible", TRUE, "can-default", TRUE, NULL);

  gtk_box_pack_end (GTK_BOX (butt_hbox), close_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_box_pack_end (GTK_BOX (butt_hbox), fresh_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_dialog_set_default_response (GTK_DIALOG (ThisDialog), GEDA_RESPONSE_REFRESH);
  gtk_widget_grab_default (fresh_butt);
}

/*! \brief Update tree model of <B>pagesel</B>'s treeview.
 *  \par Function Description
 *  Updates the tree model of <B>pagesel</B>\'s treeview.
 *
 *  Right now, each time it is called, it rebuilds all the model from the
 *  list of pages passed in.
 *  It is a recursive function to populate the tree store
 *
 *  \param [in] model   GtkTreeModel to update.
 *  \param [in] parent  GtkTreeIter pointer to tree root.
 *  \param [in] pages   PageList of pages for this toplevel.
 *  \param [in] page    The Page object to update tree model from.
 */
static void add_page (GtkTreeModel *model, GtkTreeIter *parent,
                      PageList     *pages, Page        *page)
{
  GtkTreeIter iter;
  Page  *p_current;
  GList *p_iter;

  /* add the page to the store */
  gtk_tree_store_append (GTK_TREE_STORE (model), &iter, parent);

  gtk_tree_store_set (GTK_TREE_STORE (model),
                      &iter,
                      COLUMN_PAGE, page,
                      COLUMN_CHANGED, geda_page_get_changed(page),
                      -1);

  /* search a page that has a up field == p_current->pid */
  for ( p_iter = geda_list_get_glist( pages );
        p_iter != NULL;
        p_iter = g_list_next( p_iter ) ) {

    p_current = (Page *)p_iter->data;
    if (p_current->hierarchy_up == page->pid) {
      add_page (model, &iter, pages, p_current);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Recursive function to select the current page in the treeview
 *
 */
static
void select_page(GtkTreeView *treeview, GtkTreeIter *parent, Page *page)
{
  GtkTreeModel *treemodel = gtk_tree_view_get_model (treeview);
  GtkTreeIter iter;
  Page *p_current;

  if (!gtk_tree_model_iter_children (treemodel, &iter, parent)) {
    return;
  }

  do {
    gtk_tree_model_get (treemodel, &iter, COLUMN_PAGE, &p_current, -1);
    if (p_current == page) {
      gtk_tree_view_expand_all (treeview);
      gtk_tree_selection_select_iter (gtk_tree_view_get_selection (treeview),
                                      &iter);
      return;
    }

    select_page (treeview, &iter, page);

  } while (gtk_tree_model_iter_next (treemodel, &iter));

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void pagesel_update (Pagesel *pagesel)
{

  GschemToplevel *w_current;
  GedaToplevel   *toplevel;
  Page           *p_current;
  GtkTreeModel   *model;
  GList          *iter;

  w_current = GSCHEM_DIALOG (pagesel)->w_current;
  toplevel  = w_current->toplevel;

  model     = gtk_tree_view_get_model (pagesel->treeview);

  /* wipe out every thing in the store */
  gtk_tree_store_clear (GTK_TREE_STORE (model));
  /* now rebuild */
  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    int pid;

    p_current = (Page *)iter->data;
    pid       = p_current->hierarchy_up;

    /* find every page that is not a hierarchy-down of another page */
    if (pid < 0 || s_page_search_by_page_id (toplevel->pages, pid) == NULL)
    {
      add_page (model, NULL, toplevel->pages, p_current);
    }
  }

  /* select the current page in the treeview */
  select_page (pagesel->treeview, NULL, toplevel->page_current);
}

/** @} endgroup Page-Select-Dialog */

#undef ThisDialog
