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

#include "gschem.h"
#include "x_dialog.h"

#include <geda_debug.h>

static void x_pagesel_callback_response (GtkDialog *dialog,
                                         int        arg1,
                                         void      *user_data);

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
    w_current->pswindow = GTK_WIDGET (g_object_new (TYPE_PageSEL,
                                                    /* GschemDialog */
                                                    "settings-name", "pagesel",
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
      pagesel_update (PageSEL (w_current->pswindow));
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
 *  \param [in] dialog     GtkDialog that issues callback.
 *  \param [in] arg1       Response argument of page manager dialog.
 *  \param [in] user_data  Pointer to relevant GschemToplevel structure.
 */
static
void x_pagesel_callback_response (GtkDialog *dialog, int arg1, void *user_data)
{
  GschemToplevel *w_current;

  w_current = (GschemToplevel*)user_data;

  switch (arg1) {
      case PageSEL_RESPONSE_UPDATE:
        pagesel_update (PageSEL (dialog));
        break;
      case GTK_RESPONSE_DELETE_EVENT:
      case PageSEL_RESPONSE_CLOSE:
        if (IS_PAGESEL (w_current->pswindow)) {
          gtk_widget_destroy (GTK_WIDGET (dialog));
        }
        else {
          BUG_MSG ("pswindow is wrong object");
        }
        break;
      default:
        BUG_IMSG("unhandled case <%d>", arg1);
  }
}

enum {
  COLUMN_Page,
  COLUMN_NAME,
  COLUMN_CHANGED,
  NUM_COLUMNS
};


static void pagesel_class_init (PageselClass *class);
static void pagesel_init       (Pagesel *pagesel);

static void pagesel_popup_menu (Pagesel *pagesel,
                                GdkEventButton *event);

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

  gtk_tree_model_get (model, &iter, COLUMN_Page, &page, -1);

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

  if (event->type == GDK_BUTTON_PRESS  &&  event->button == 3) {
    pagesel_popup_menu (pagesel, event);
    ret_val = TRUE;
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

#define DEFINE_POPUP_CALLBACK(name, action)                       \
static void                                                       \
pagesel_callback_popup_ ## name (GtkMenuItem *menuitem,           \
                                 void *user_data)              \
{                                                                 \
  i_callback_ ## action (GSCHEM_DIALOG (user_data)->w_current, 0, NULL); \
}

DEFINE_POPUP_CALLBACK (new_page,     file_new)
DEFINE_POPUP_CALLBACK (open_page,    file_open)
DEFINE_POPUP_CALLBACK (save_page,    file_save)
DEFINE_POPUP_CALLBACK (close_page,   page_close)
DEFINE_POPUP_CALLBACK (discard_page, page_discard)


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
    { NULL,               NULL                                             } };
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
    } else {
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
 *
 *  When the gschem-toplevel property is set on the parent GschemDialog,
 *  we should update the pagesel dialog.
 *
 *  \param [in] gobject    the object which received the signal.
 *  \param [in] arg1      the GParamSpec of the property which changed
 *  \param [in] user_data  user data set when the signal handler was connected.
 */
static void notify_gschem_toplevel_cb (GObject    *gobject,
                                       GParamSpec *arg1,
                                       void       *user_data)
{
  Pagesel *pagesel = PageSEL( gobject );

  pagesel_update( pagesel );
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
unsigned int pagesel_get_type()
{
  static unsigned int pagesel_type = 0;

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
      (GInstanceInitFunc) pagesel_init,
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
static void pagesel_class_init (PageselClass *klass)
{
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void pagesel_init (Pagesel *pagesel)
{
  GtkWidget *scrolled_win, *treeview, *label;
  GtkTreeModel *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  /* dialog initialization */
  g_object_set (G_OBJECT (pagesel),
                /* GtkContainer */
                "border-width",    0,
                /* GtkWindow */
                "title",           _("Page Manager"),
                "default-height",  180,
                "default-width",   515,
                "modal",           FALSE,
                "window-position", GTK_WIN_POS_NONE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

  /* create the model for the treeview */
  store = (GtkTreeModel*)gtk_tree_store_new (NUM_COLUMNS,
                                             G_TYPE_POINTER,  /* page */
                                             G_TYPE_STRING,   /* name */
                                             G_TYPE_BOOLEAN); /* changed */

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
  /*   - first column: page name */
  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                  /* GtkCellRendererText */
                  "editable", FALSE,
                  NULL));
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Filename"),
                  "min-width", 400,
                  "resizable", TRUE,
                  NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_NAME);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*   - second column: changed */
  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
                  /* GtkCellRendererToggle */
                  "activatable", FALSE,
                  NULL));
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Changed"),
                  "sizing", GTK_TREE_VIEW_COLUMN_FIXED,
                  NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "active", COLUMN_CHANGED);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), treeview);
  /* set treeview of pagesel */
  pagesel->treeview = GTK_TREE_VIEW (treeview);

  /* add the scrolled window to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (pagesel)->vbox), scrolled_win,
                      TRUE, TRUE, 0);
  gtk_widget_show_all (scrolled_win);

  /* add a label below the scrolled window */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkLabel */
                                    "label", _("Right click on the filename for more options..."),
                                    NULL));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (pagesel)->vbox), label,
                      FALSE, TRUE, 5);
  gtk_widget_show (label);

  /* now add buttons in the action area */
  gtk_dialog_add_buttons (GTK_DIALOG (pagesel),
                          /*  - update button */
                          GTK_STOCK_REFRESH, PageSEL_RESPONSE_UPDATE,
                          /*  - close button */
                          GTK_STOCK_CLOSE,   PageSEL_RESPONSE_CLOSE,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(pagesel),
					  PageSEL_RESPONSE_UPDATE,
					  PageSEL_RESPONSE_CLOSE,
					  -1);

  g_signal_connect( pagesel, "notify::gschem-toplevel",
                    G_CALLBACK( notify_gschem_toplevel_cb ), NULL );
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
  Page *p_current;
  GList *p_iter;

  /* add the page to the store */
  gtk_tree_store_append (GTK_TREE_STORE (model), &iter, parent);

  gtk_tree_store_set (GTK_TREE_STORE (model),
                      &iter,
                      COLUMN_Page, page,
                      COLUMN_NAME, page->filename,
                      COLUMN_CHANGED, page->CHANGED,
                      -1);

  /* search a page that has a up field == p_current->pid */
  for ( p_iter = geda_list_get_glist( pages );
        p_iter != NULL;
        p_iter = g_list_next( p_iter ) ) {

    p_current = (Page *)p_iter->data;
    if (p_current->up == page->pid) {
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
static void select_page(GtkTreeView *treeview,
			GtkTreeIter *parent, Page *page)
{
  GtkTreeModel *treemodel = gtk_tree_view_get_model (treeview);
  GtkTreeIter iter;
  Page *p_current;

  if (!gtk_tree_model_iter_children (treemodel, &iter, parent)) {
    return;
  }

  do {
    gtk_tree_model_get (treemodel, &iter,
                        COLUMN_Page, &p_current,
                        -1);
    if (p_current == page) {
      gtk_tree_view_expand_all (treeview);
      gtk_tree_selection_select_iter (
        gtk_tree_view_get_selection (treeview),
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
/*
  if (!IS_PAGESEL (pagesel)) {
    BUG_MSG ("pagesel is wrong object");
    return;
  }
*/
  w_current = GSCHEM_DIALOG (pagesel)->w_current;
  toplevel  = w_current->toplevel;

  model     = gtk_tree_view_get_model (pagesel->treeview);

  /* wipe out every thing in the store */
  gtk_tree_store_clear (GTK_TREE_STORE (model));
  /* now rebuild */
  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (Page *)iter->data;
    /* find every page that is not a hierarchy-down of another page */
    if (p_current->up < 0 ||
        s_page_search_by_page_id (toplevel->pages,
                                  p_current->up) == NULL) {
      add_page (model, NULL, toplevel->pages, p_current);
    }
  }

  /* select the current page in the treeview */
  select_page (pagesel->treeview, NULL, toplevel->page_current);
}

