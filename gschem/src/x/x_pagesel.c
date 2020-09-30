/* -*- C x_pagesel.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
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

#include "../../include/gschem.h"
#include "../../include/i_actions.h"
#include "../../include/x_dialog.h"
#include "../../include/x_dnd.h"

#include <geda_debug.h>

#define ThisDialog pagesel

/** \defgroup Page-Select-Dialog Page Select Dialog
 *  @{
 *  \ingroup Standard-Dialogs
 *  \image html page_man_dialog.png
 *  \image latex page_man_dialog.png
 */

/* Enumerate Control IDs */
typedef enum {
       AutoHeight,
       ShowFullName
} ControlID;

static GschemDialogClass *pagesel_parent_class = NULL;

static WidgetStringData DialogStrings[] = {
  { "AutoHeightSwitch",    N_("Auto Height"), N_("Enable or disable Page Select dialog auto height")},
  { "ShowFullNameSwitch",  N_("Full names"), N_("Enable or disable displaying of paths in file names")},
  { NULL, NULL, NULL},
};

enum
{
  TARGET_STRING,
  TARGET_URL
};

static GtkTargetEntry dnd_target_list[] =
{
  { "STRING",        0, TARGET_STRING },
  { "text/plain",    0, TARGET_STRING },
  { "text/uri-list", 0, TARGET_URL },
};

static unsigned int dnd_ntargets = G_N_ELEMENTS (dnd_target_list);

static void select_page                 (GtkTreeView *treeview,
                                         GtkTreeIter *parent,
                                         Page        *page);
static void x_pagesel_callback_response (GtkDialog   *dialog,
                                         int          response,
                                         void        *log);
static void pagesel_auto_height       (Pagesel     *pagesel);

/*!
 * \brief Open the page manager dialog.
 * \par Function Description
 *  Opens the page manager dialog for <B>toplevel</B> if it is not already.
 *  In this last case, it raises the dialog.
 *
 * \param [in] w_current  The GschemToplevel object to open page manager for.
 */
void x_pagesel_open (GschemToplevel *w_current)
{
  if (w_current->pswindow == NULL) {

    w_current->pswindow = g_object_new (TYPE_PAGESEL,
                                        /* GschemDialog */
                                        "settings-name",   IDS_PAGE_MANAGER,
                                        "gschem-toplevel", w_current,
                                        NULL);

    g_signal_connect (w_current->pswindow,
                      "response",
                      G_CALLBACK (x_pagesel_callback_response),
                      w_current);

    gtk_widget_show (w_current->pswindow);

    pagesel_auto_height((Pagesel*)w_current->pswindow);
  }
  else {
    gtk_window_present(GTK_WINDOW(w_current->pswindow));
  }
}

/*!
 * \brief Close the page manager dialog.
 * \par Function Description
 *  Closes the page manager dialog associated with <B>toplevel</B>.
 *
 * \param [in] w_current  The GschemToplevel object to close page manager for.
 */
void x_pagesel_close (GschemToplevel *w_current)
{
  /* Check if PageSelect dialog active */
  if (w_current->pswindow) {

    /* Validate the pointer to the Dialog */
    if (IS_PAGESEL (w_current->pswindow)) {
      gtk_widget_destroy (w_current->pswindow);
    }
    else {
      BUG_MSG ("pswindow is wrong object");
    }
  }
}

/*!
 * \brief Idle Update Page Select Dialog
 * \par Function Description
 *  Calls pagesel_update and then decrements reference on the Pagesel
 *  dialog that was added in x_pagesel_update to insure dialog was not
 *  destroyed whilst waiting for an idle state.
 *
 * \param [in] w_current  A GschemToplevel object.
 */
static int x_pagesel_idle_update (GschemToplevel *w_current)
{
  Pagesel *pagesel = PAGESEL(w_current->pswindow);

  if (pagesel) {
    pagesel_update (pagesel);
    g_object_unref(pagesel);
  }
  return FALSE;
}

/*!
 * \brief Update the Page Select Dialog.
 * \par Function Description
 *  Spawns thread to call x_pagesel_idle_update when loop is idle.
 *
 * \param [in] w_current  A GschemToplevel object.
 */
void x_pagesel_update (GschemToplevel *w_current)
{
  /* Check if PageSelect dialog active */
  if (w_current->pswindow) {

    /* Validate the pointer to the Dialog */
    if (IS_PAGESEL (w_current->pswindow)) {

      /* Add a reference to the object */
      g_object_ref(w_current->pswindow);
      gschem_threads_idle_add (x_pagesel_idle_update, w_current);
    }
    else {
      BUG_MSG ("pswindow is wrong object");
    }
  }
}

/*!
 * \brief Callback for page manager response.
 * \par Function Description
 *  Handles response <B>arg1</B> of the page manager dialog <B>dialog</B>.
 *
 * \param [in] dialog   GtkDialog that issues callback.
 *
 * \param [in] response Response argument of page manager dialog.
 * \param [in] data     Pointer to relevant GschemToplevel structure.
 */
static void
x_pagesel_callback_response (GtkDialog *dialog, int response, void *data)
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
      BUG_IMSG ("unhandled case", response);
  }
}

enum {
  COLUMN_PAGE,
  COLUMN_NAME,
  COLUMN_CHANGED,
  COLUMN_SPACER,
  NUM_COLUMNS
};

static void pagesel_class_init    (void *class, void *class_data);
static void pagesel_instance_init (GTypeInstance *instance, void *class);
static void pagesel_popup_menu    (Pagesel *pagesel, GdkEventButton *event);

/*!
 * \brief Get the Heigth of the Page Manager dialog Action Area
 * \par Function Description
 *  Helper for pagesel_auto_height to retrieve the heigth of the
 *  acion area, which is an hbox widget.
 */
static void pagesel_get_action_height (Pagesel *pagesel)
{
  GtkWidget    *action_hbox;
  GtkAllocation allocation;
  unsigned int  border;

  action_hbox = GTK_DIALOG(pagesel)->action_area;

  gtk_widget_get_allocation(action_hbox, &allocation);

  border = geda_get_container_border_width (action_hbox);

  pagesel->action_height = allocation.height + (border << 1);
}

/*!
 * \brief Get Tree-View Row Heigth in Page Manager dialog
 * \par Function Description
 *  Helper for pagesel_auto_height to retrieve the heigth of the
 *  rows in the tree-view in the PageSel dialog. A pointer to the
 *  file name cell renderer was retained during initialization so
 *  that the cell heigth could be determine, cell heigth as reported
 *  by gtk_cell_renderer_get_size does not include the spacing
 *  between rows but is included in the row_height.
 */
static void pagesel_get_row_height (Pagesel *pagesel)
{
  GdkRectangle cell_area;
  int          x_offset;
  int          y_offset;
  int          width;
  int          height;

  gtk_cell_renderer_get_size (pagesel->renderer,
                              GTK_WIDGET (pagesel->treeview),
                              &cell_area,
                              &x_offset,
                              &y_offset,
                              &width,
                              &height);

  pagesel->row_height = height + 2;
}

/*!
 * \brief Page Manager Auto Resize Dialog Heigth
 * \par Function Description
 *  This function adjust the height of the Page Select Dialog based
 *  on the number of pages if auto_height is TRUE. When the dialog
 *  is manually sized to the miniumnm height the tree-view shows 3
 *  rows with the header displayed so the value of the constant
 *  #PAGESEL_ROWS_THRESHOLD was choosen to be 4. The height of dialog
 *  components such as the tree row height could not be determine
 *  during initialization, so these are resolved the first time the
 *  function executes with auto_height enabled. Note this function
 *  is called by the callback for the AutoHeight toggle switch.
 */
static void pagesel_auto_height(Pagesel *pagesel)
{
  GschemToplevel *w_current;
  GtkWidget      *toggle;
  bool            auto_height;
  int width, height;
  int pcount;

  toggle      = GET_EDA_OBJECT(AutoHeight);
  auto_height = GET_SWITCH_STATE(toggle);

  if (!auto_height) {
    return;
  }

  /* Determine the mysterious tree-row row_height */
  if (!pagesel->row_height ) {
    pagesel_get_row_height (pagesel);
  }

  /* Get height of action area if have not already done so */
  if (!pagesel->action_height ) {
    pagesel_get_action_height (pagesel);
  }

  gtk_window_get_size (GTK_WINDOW (pagesel), &width, &height);

  w_current = GSCHEM_DIALOG (pagesel)->w_current;

  pcount = geda_toplevel_get_page_count(w_current->toplevel);

  /* Check number of pages less than min threshold */
  if (pcount < PAGESEL_ROWS_THRESHOLD) {

    /* Pages less than min threshold, check if dialog needs to shrink */
    if (height > PAGESEL_MIN_HEIGHT) {
      gtk_window_resize(GTK_WINDOW(pagesel), width, PAGESEL_MIN_HEIGHT);
    }
  }
  else {

    int action_height;
    int row_height;
    int preferred;

    action_height = pagesel->action_height;
    row_height    = pagesel->row_height;

    /* Calculate the height to display all pages, note PAGESEL_ROWS_THRESHOLD
     * is subtracted from the page count because these rows are already
     * accounted for in PAGESEL_MIN_HEIGHT */
    preferred = PAGESEL_MIN_HEIGHT + action_height +
                ((pcount - PAGESEL_ROWS_THRESHOLD) * row_height);

    if (height < preferred) {

      GdkScreen    *screen;
      int screen_height, window_x, window_y;
      int below, bottom;

      screen  = gtk_widget_get_screen (GTK_WIDGET (pagesel));

      screen_height = gdk_screen_get_height(screen);

      gtk_window_get_position (GTK_WINDOW (pagesel), &window_x, &window_y);

      bottom = window_y + height;

      below  = screen_height - bottom;

      if (below > preferred - height) {
        /* definitely shrink */
        gtk_window_resize(GTK_WINDOW(pagesel), width, preferred);
      }
      else {

        /* Adjust the height to use all of the usable y below the dialog */
        int new_bottom = bottom + below - height;
        gtk_window_resize(GTK_WINDOW(pagesel), width, new_bottom);
      }
    }
    else {

      /* The dialog is taller than it needs to be to display the list of
       * opened pages, if the "extra" height is more than one row then
       * shrink the dialog */
      if (height - preferred > pagesel->row_height) {
        gtk_window_resize(GTK_WINDOW(pagesel), width, preferred);
      }
    }
  }
}

/*!
 * \brief Page Manager Dialog Tree View Page Selected
 * \par Function Description
 *  This function is called whenever a row is selected in the
 *  treeview. The page associated with the selected row is set
 *  to be the Current page. geda_tree_view_row_make_visible is
 *  called to ensure the current page is visible in the tree.
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

  geda_tree_view_row_make_visible (pagesel->treeview, &iter, TRUE);
}

/*!
 * \brief Page Manager Dialog Treeview button Press Event
 * \par Function Description
 *  This function is called when the user clicks a mouse button when
 *  the pointer is over a treeview row. If the event was a "right-
 *  click" then the pagesel_popup_menu function is called to present
 *  a menu of options. If the event was a middle-mouse click then the
 *  main window is raised to the foreground and this is convenient for
 *  users using the Pagesel dialog to switch between pages while other
 *  application windows are present.
 */
static bool
pagesel_callback_button_pressed (GtkWidget      *widget,
                                 GdkEventButton *event,
                                 void           *user_data)
{
  Pagesel *pagesel = (Pagesel*)user_data;
  bool ret_val;

  if (event->type == GDK_BUTTON_PRESS) {
    if (event->button == 2) {
      GschemToplevel *w_current = GSCHEM_DIALOG(pagesel)->w_current;
      gtk_window_present(w_current->main_window);
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

/*!
 * \brief Callback for keyboard initiated Pop-up Menu
 * \par Function Description
 * Called when "pop-up-menu" is emitted on the tree-view, which
 * occurs when users initiated the pop-up menu using a keyboard
 * short-cut such as Shift-F10, this callback is not associated
 * with mouse button events.
 * Calls pagesel_popup_menu to create the context pop-up menu.
 */
static bool
pagesel_callback_popup_menu (GtkWidget *widget, void *user_data)
{
  Pagesel *pagesel = (Pagesel*)user_data;

  pagesel_popup_menu (pagesel, NULL);

  return TRUE;
}

/*!
 * \brief Callback Page Select Dialog context Popup Page Down
 * \par Function Description
 *  This function calls gschem_toplevel_move_page_down with the page
 *  of the current selection in the PageSel tree view, if the page
 *  is moved, then pagesel_update is called to refresh the tree-view.
 *  Rather than using the current page, this routine retrieves the
 *  page from the selection to avoid a potential race condition with
 *  pagesel_callback_selection_changed having called x_window_set_
 *  current_page.
 *
 *  \param [in] menuitem   The Popup Menu Item selected (not used).
 *  \param [in] user_data  The Pagesel object.
 */
static void
pagesel_callback_popup_down  (GedaMenuItem *menuitem, void *user_data)
{
  GschemToplevel   *w_current;
  Pagesel          *pagesel;
  Page             *page;

  GtkTreeSelection *selection;
  GtkTreeModel     *model;
  GtkTreeIter       iter;

  pagesel   = (Pagesel*)user_data;

  selection = gtk_tree_view_get_selection (pagesel->treeview);

  if (!gtk_tree_selection_get_selected (selection, &model, &iter)) {
    return;
  }

  w_current = GSCHEM_DIALOG (pagesel)->w_current;

  gtk_tree_model_get (model, &iter, COLUMN_PAGE, &page, -1);

  if (gschem_toplevel_move_page_down(w_current, page)) {
    pagesel_update (pagesel);
  }
}

/*!
 * \brief Callback Page Select Dialog context Popup Page Up
 * \par Function Description
 *  This function calls gschem_toplevel_move_page_up with the page
 *  of the current selection in the PageSel tree view, if the page
 *  is moved up, then pagesel_update is called to refresh the tree
 *  view. Rather than using the current page, this routine retrieves
 *  the page from the selection to avoid a potential race condition
 *  with pagesel_callback_selection_changed having called x_window_
 *  set_current_page.
 *
 *  \param [in] menuitem   The Popup Menu Item selected (not used).
 *  \param [in] user_data  The Pagesel object.
 */
static void
pagesel_callback_popup_up (GedaMenuItem *menuitem, void *user_data)
{
  GschemToplevel   *w_current;
  Pagesel          *pagesel;
  Page             *page;

  GtkTreeSelection *selection;
  GtkTreeModel     *model;
  GtkTreeIter       iter;

  pagesel   = (Pagesel*)user_data;

  selection = gtk_tree_view_get_selection (pagesel->treeview);

  if (!gtk_tree_selection_get_selected (selection, &model, &iter)) {
    return;
  }

  w_current = GSCHEM_DIALOG (pagesel)->w_current;

  gtk_tree_model_get (model, &iter, COLUMN_PAGE, &page, -1);

  if (gschem_toplevel_move_page_up(w_current, page)) {
    pagesel_update (pagesel);
  }
}

#define DEFINE_POPUP_CALLBACK(name, action) \
static void \
pagesel_callback_popup_##name (GedaMenuItem *menuitem, void *pagesel) { \
  i_command_process(GSCHEM_DIALOG (pagesel)->w_current, \
                    action, 0, NULL, ID_ORIGIN_MENU); \
}

DEFINE_POPUP_CALLBACK (new_blank,    ACTION(FILE_NEW))
DEFINE_POPUP_CALLBACK (new_page,     ACTION(PAGE_NEW))
DEFINE_POPUP_CALLBACK (open_page,    ACTION(FILE_OPEN))
DEFINE_POPUP_CALLBACK (save_page,    ACTION(FILE_SAVE))
DEFINE_POPUP_CALLBACK (close_page,   ACTION(PAGE_CLOSE))
DEFINE_POPUP_CALLBACK (discard_page, ACTION(PAGE_DISCARD))
DEFINE_POPUP_CALLBACK (revert_page,  ACTION(PAGE_REVERT))

/*!
 * \internal Creates the context pop-up menu displayed when the
 *  user right-clicks on the tree, which also actives the item
 *  (if not already active), and consequently the page (on the
 *  canvas). The pop-up widget is retained in pagesel->popup and
 *  re-used until the dialog is closed.
 */
static GtkWidget *pagesel_create_popup_menu (Pagesel *pagesel)
{
  GtkWidget *menu;

  struct menuitem_t {
    char *label;
    GCallback callback;
  };

  struct menuitem_t menuitems[] = {
    { N_("Move Up"),      G_CALLBACK (pagesel_callback_popup_up)           },
    { N_("Move Down"),    G_CALLBACK (pagesel_callback_popup_down)         },
    { "-",                NULL                                             },
    { N_("New Blank"),    G_CALLBACK (pagesel_callback_popup_new_blank)    },
    { N_("New Page"),     G_CALLBACK (pagesel_callback_popup_new_page)     },
    { N_("Open Page..."), G_CALLBACK (pagesel_callback_popup_open_page)    },
    { "-",                NULL                                             },
    { N_("Save Page"),    G_CALLBACK (pagesel_callback_popup_save_page)    },
    { N_("Close Page"),   G_CALLBACK (pagesel_callback_popup_close_page)   },
    { N_("Discard Page"), G_CALLBACK (pagesel_callback_popup_discard_page) },
    { N_("Revert Page"),  G_CALLBACK (pagesel_callback_popup_revert_page)  },
    { NULL,               NULL                                             }};

  struct menuitem_t *tmp;

  /* create the context menu */
  menu = geda_menu_new();

  for (tmp = menuitems; tmp->label != NULL; tmp++) {

    GtkWidget *menuitem;

    if (strncmp (tmp->label, "-", 1) == 0) {
      menuitem = geda_menu_separator_new ();
    }
    else {
      menuitem = geda_menu_item_new_with_label (_(tmp->label));
      g_signal_connect (menuitem, "activate", tmp->callback, pagesel);
    }
    geda_menu_shell_append (GEDA_MENU_SHELL (menu), menuitem);
  }

  /* Save for destruction when dialog is closed */
  pagesel->popup = menu;

  return menu;
}

/*!
 * \brief Popup context-sensitive menu.
 * \par Function Description
 *  Pops up a context-sensitive menu.
 *
 *  <B>event</B> can be NULL if the popup is triggered by a key
 *  binding instead of a mouse click.
 *
 * \param [in] pagesel  The Pagesel object.
 * \param [in] event    Mouse click event info.
 */
static void
pagesel_popup_menu (Pagesel *pagesel, GdkEventButton *event)
{
  GtkTreePath *path;
  GtkWidget   *menu;

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

  if (!pagesel->popup) {
    menu = pagesel_create_popup_menu (pagesel);
  }
  else {
    menu = pagesel->popup;
  }

  gtk_widget_show_all (menu);

  /* make menu a popup menu */
  geda_menu_popup (GEDA_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                   gdk_event_get_time ((GdkEvent*)event));
}

/*!
 * \brief Handler for the notify::gschem-toplevel signal of GschemDialog
 * \par Function Description
 *  When the gschem-toplevel property is set on the parent GschemDialog,
 *  we should update the pagesel dialog.
 *
 * \param [in] gobject  Object which received the signal.
 * \param [in] arg1     GParamSpec of the property which changed
 * \param [in] nothing  user data not set.
 */
static void
notify_gschem_toplevel_cb (GObject *gobject, GParamSpec *arg1, void *nothing)
{
  Pagesel *pagesel = PAGESEL(gobject);
  pagesel_update( pagesel );
}

/*!
 * \brief Sets data for a particular cell of the In Use treeview.
 * \par Function Description
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

    filename = state ? page->filename : geda_file_get_basename(page->filename);
    g_object_set (cell, "text", filename, NULL);
  }
}

/*!
 * \brief Show full file name toggle switch responder
 * \par Function Description
 *  Toggles the switch state and updates the tree view.
 */
static void
pagesel_auto_height_toggled (GtkWidget *widget, Pagesel *pagesel)
{
  TOGGLE_SWITCH(widget);
  pagesel_auto_height(pagesel);
  return;
}

/*!
 * \brief Show full file name toggle switch responder
 * \par Function Description
 *  Toggles the switch state and updates the tree view.
 */
static void
pagesel_show_fullnames_toggled (GtkWidget *widget, Pagesel *pagesel)
{
  TOGGLE_SWITCH(widget);
  pagesel_update(pagesel);
  gtk_tree_view_columns_autosize(pagesel->treeview);
  return;
}

/*!
 * \brief Callback Refresh button clicked on Page Select Dialog
 * \par Function Description
 *  Causes Gtk to issue a GEDA_RESPONSE_REFRESH "response" on the dialog.
 */
static void
pagesel_callback_refresh_clicked (GtkButton *RefreshButt, void *user_data)
{
  gtk_dialog_response (GTK_DIALOG (user_data), GEDA_RESPONSE_REFRESH);
}

/*!
 * \brief Callback Close button clicked on Page Select Dialog
 * \par Function Description
 *  Causes Gtk to issue a GEDA_RESPONSE_CLOSE "response" on the dialog.
 */
static void
pagesel_callback_close_clicked (GtkButton *CloseButt, void *user_data)
{
  gtk_dialog_response (GTK_DIALOG (user_data), GEDA_RESPONSE_CLOSE);
}

/*!
 * \brief Callback Drag Received on the Tree View widget
 * \par Function Description
 *  Provides basis support for Drag&Drop on the Page Select Dialog. The
 *  received string is extracted and passed to x_dnd_receive_string for
 *  evaluation and processing. If the received string ends with "sym"
 *  and was dropped on a row that is a schematic then the page is first
 *  selected and the x argument passed to x_dnd_receive_string is set
 *  TRUE and this allows users to drag symbols onto the PageSel Dialog
 *  and into a specific schematic (if the symbol is known). If the
 *  symbol is dropped on a blank area, or on another open symbol page
 *  then the symbol file is opened for editing as a separate page.
 *
 * \note Re-allocation of the string would not be required if someone
 *       did not errantly add line feeds and carriage returns to the
 *       string that is received.
 */
static void
pagesel_dnd_drag_receive(GtkWidget *widget, GdkDragContext   *context, int x, int y,
                                            GtkSelectionData *selection_data,
                                            unsigned int      target_type,
                                            unsigned int      time,
                                            void             *userdata)
{
  GschemToplevel *w_current;

  Pagesel *pagesel = PAGESEL(userdata);
  w_current = GSCHEM_DIALOG (pagesel)->w_current;

  /* Deal with what we are given from source */
  if ((selection_data != NULL) &&
     (gtk_selection_data_get_length(selection_data) >= 0))
  {
    GtkTreePath         *path;
    const unsigned char *buffer;
    const char          *ptr;
    const char          *ext;
          char          *string;

    int tail;
    int len;
    int xf = 0;

    /* Get pointer to the data */
#if GTK_CHECK_VERSION(2,14,0)
    buffer = gtk_selection_data_get_data (selection_data);
#else
    buffer = selection_data->data;
#endif

    ptr    = (const char*)buffer;
    string = geda_utility_string_strdup(ptr);
    len    = strlen(string);

    /* Some lame file managers append line feeds and carriage
     * returns characters so replace these with nulls */
    for(tail = 0; tail < len; tail++) {
      if (string[tail] == '\n')
        string[tail] = '\0';
      if (string[tail] == '\r')
        string[tail] = '\0';
    }

    /* check if string could be a symbol file */
    ext = geda_file_get_filename_ext(string);

    if (ext && strcmp (ext, SYMBOL_FILE_SUFFIX) == 0) {

      GtkTreeView *tree_view;
      int tx, ty;

      tree_view = pagesel->treeview;

      gtk_tree_view_convert_widget_to_tree_coords (tree_view, x, y, &tx, &ty);

      /* Check is symbol was dropped on a schmatic */
      if (gtk_tree_view_get_path_at_pos (tree_view, tx, ty, &path, NULL, NULL, NULL))
      {
        GtkTreeModel *treemodel;
        GtkTreeIter   iter;
        Page         *page;

        page      = NULL;
        treemodel = gtk_tree_view_get_model (tree_view);

        if (gtk_tree_model_get_iter (treemodel, &iter, path)) {
          gtk_tree_model_get (treemodel, &iter, COLUMN_PAGE, &page, -1);
        }

        /* do not need path anymore */
        gtk_tree_path_free (path);

        if (page && !geda_struct_page_is_symbol_file(page)) {

          /* Dropped a symbol on a schmatic */
          select_page (tree_view, NULL, page);

          /* set flag indicating add symbol to page */
          xf = 1;
        }
      }
    }

    x_dnd_receive_string(w_current, xf, 0, string, DROPPED_ON_PAGESEL);

    /* Check if dialog should grow */
    pagesel_auto_height((Pagesel*)w_current->pswindow);

    GEDA_FREE(string);
  }
}

/*!
 * \brief Callback Query Tooltip in the Page Select Dialog TreeView
 * \par Function Description
 *  Sets the tool-tip text to the page file name if the pointer is
 *  over a valid row when not displaying the file names with paths.
 *
 * \returns TRUE if the tip was set, otherwise FALSE;
 */
static bool
pagesel_callback_query_tooltip(GtkWidget  *widget, int x, int y,
                               bool        keyboard_mode,
                               GtkTooltip *tooltip,
                               void       *user_data)
{
  Pagesel   *pagesel = user_data;
  GtkWidget *toggle  = GET_EDA_OBJECT(ShowFullName);
  int        state   = GET_SWITCH_STATE(toggle);

  if (!state) {

    GtkTreePath  *path;
    GtkTreeView  *tree_view; /* is also widget */

    int tx, ty;

    tree_view = pagesel->treeview;

    gtk_tree_view_convert_widget_to_tree_coords (tree_view, x, y, &tx, &ty);

    if (gtk_tree_view_get_path_at_pos (tree_view, tx, ty,
                                      &path, NULL, NULL, NULL))
    {
      GtkTreeModel *tree_model;
      GtkTreeIter   iter;
      Page         *page;

      page       = NULL;
      tree_model = gtk_tree_view_get_model (tree_view);

      if (gtk_tree_model_get_iter (tree_model, &iter, path)) {
        gtk_tree_model_get (tree_model, &iter, COLUMN_PAGE, &page, -1);
      }

      gtk_tree_path_free (path);

      if (page) {
        gtk_tooltip_set_text(tooltip, page->filename);
        return TRUE;
      }
    }
  }
  return FALSE;
}

/*!
 * \brief Geda Box Object Finalization Function
 * \par Function Description
 *  Save user preferences to the configuration system.
 */
static void pagesel_finalize(GObject *object)
{
  Pagesel    *pagesel = PAGESEL(object);
  EdaConfig  *cfg     = eda_config_get_user_context();
  GtkWidget  *widget;
  const char *group   = IDS_PAGE_MANAGER;
  const char *key;
        int   value;

  key    = "full-names";
  widget = GET_EDA_OBJECT(ShowFullName);
  value  = GET_SWITCH_STATE(widget);

  eda_config_set_boolean (cfg, group, key, value);

  key    = "auto-height";
  widget = GET_EDA_OBJECT(AutoHeight);
  value  = GET_SWITCH_STATE(widget);

  eda_config_set_boolean (cfg, group, key, value);

  if (pagesel->popup) {
    g_object_ref(pagesel->popup);
    gtk_widget_destroy(pagesel->popup);
    g_object_ref_sink(G_OBJECT(pagesel->popup));
    g_object_unref(pagesel->popup);
    pagesel->popup = NULL;
  }

  /* Chain up to parent Class */
  G_OBJECT_CLASS(pagesel_parent_class)->finalize(object);
}

/*!
 * \brief Function to retrieve pagesel's Type identifier.
 * \par Function Description
 *  Function to retrieve pagesel's Type identifier. On the first call,
 *  this registers the pagesel in the GedaTypesystem.  Subsequently
 *  the functions returns the saved value from its first execution.
 *
 * \return the Type identifier associated with pagesel.
 */
GedaType pagesel_get_type (void)
{
  static GedaType pagesel_type = 0;

  if (!pagesel_type) {
    static const GTypeInfo pagesel_info = {
      sizeof(PageselClass),
      NULL,                  /* base_init */
      NULL,                  /* base_finalize */
      pagesel_class_init,    /* (GClassInitFunc)  */
      NULL,                  /* class_finalize */
      NULL,                  /* class_data */
      sizeof(Pagesel),
      0,                     /* n_preallocs */
      pagesel_instance_init  /* (GInstanceInitFunc) */
    };

    pagesel_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                           "Pagesel",
                                           &pagesel_info, 0);
  }

  return pagesel_type;
}

bool is_a_pagesel (Pagesel *pagesel)
{
  if (G_IS_OBJECT(pagesel)) {
    return (pagesel_get_type() == pagesel->instance_type);
  }
  return FALSE;
}

/*!
 * \brief Pagesel Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class       Pagesel class we are initializing
 * \param [in]  class_data  Pagesel structure associated with the class
 */
static void pagesel_class_init (void *class, void *class_data)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);

  pagesel_parent_class        = g_type_class_peek_parent (class);

  gobject_class->finalize     = pagesel_finalize;
}

/*!
 * \brief Initialize new GedaAction data structure instance.
 * \par Function Description
 *  This function is call after the GedaActionClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaAction data structure
 * \param [in] class     A GedaActionClass Object
 */
static void pagesel_instance_init (GTypeInstance *instance, void *class)
{
  GtkWidget         *scrolled_win, *label;
  GtkTreeModel      *store;
  GtkCellRenderer   *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection  *selection;
  GtkTreeView       *tree_view;
  Pagesel           *pagesel;

  EdaConfig  *cfg   = eda_config_get_user_context ();
  const char *group = IDS_PAGE_MANAGER;
  const char *key   = "full-names";

  int full_names;
  int auto_height;

  pagesel = (Pagesel*)instance;

  pagesel->instance_type = pagesel_get_type();
  pagesel->popup         = NULL;

  i_var_restore_group_boolean(cfg, group, key, &full_names, TRUE);

  key = "auto-height";

  i_var_restore_group_boolean(cfg, group, key, &auto_height, TRUE);

  /* dialog initialization */
  g_object_set (pagesel,
                /* GtkContainer */
                "border-width",    DIALOG_BORDER_WIDTH,
                /* GtkWindow */
                "title",           _("Page Manager"),
                "default-height",  220,
                "default-width",   280,
                "modal",           FALSE,
                "window-position", GTK_WIN_POS_MOUSE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                /* GtkDialog */
                "has-separator",   FALSE, /* Action Area is Replaced */
                NULL);

  /* create the model for the TreeView */
  store = (GtkTreeModel*)gtk_tree_store_new (NUM_COLUMNS,
                                             G_TYPE_POINTER,  /* page object */
                                             G_TYPE_STRING,   /* file name */
                                             G_TYPE_BOOLEAN,  /* changed */
                                             G_TYPE_POINTER);

  /* create a scrolled window for the TreeView */
  scrolled_win = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                               /* GtkContainer */
                               "border-width",      DIALOG_BORDER_WIDTH,
                               /* GtkScrolledWindow */
                               "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                               "vscrollbar-policy", GTK_POLICY_ALWAYS,
                               "shadow-type",       GTK_SHADOW_ETCHED_IN,
                               NULL);

  /* create the TreeView */
  tree_view = g_object_new (GTK_TYPE_TREE_VIEW,
                            /* GtkTreeView */
                            "model",             store,
                            "rules-hint",        TRUE,
                            NULL);

  g_signal_connect (tree_view, "button-press-event",
                    G_CALLBACK (pagesel_callback_button_pressed),
                    pagesel);

  g_signal_connect (tree_view, "popup-menu",
                    G_CALLBACK (pagesel_callback_popup_menu),
                    pagesel);

  selection = gtk_tree_view_get_selection (tree_view);
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

  g_signal_connect (selection, "changed",
                    G_CALLBACK (pagesel_callback_selection_changed),
                    pagesel);

  /* ---------------------- first column: page name ---------------------- */

  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                           /* GtkCellRendererText */
                           "editable", FALSE,
                           NULL);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         /* GtkTreeViewColumn */
                         "title",    _("Filename"),
                         "min-width", COLUMN_NAME_MIN_WIDTH,
                         "sizing",    GTK_TREE_VIEW_COLUMN_AUTOSIZE,
                         "resizable", TRUE,
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_NAME);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           pagesel_treeview_set_cell_filename,
                                           pagesel, NULL);
  gtk_tree_view_append_column (tree_view, column);

  /* Save pointer to the Name cell renderer in pagesel structure */
  pagesel->renderer = renderer;

  /* Which is used to determine the row height later */
  pagesel->row_height = 0;

  /* --------------------- second column: changed  ---------------------- */
  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
                           /* GtkCellRendererToggle */
                           "activatable", FALSE,
                           NULL);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         /* GtkTreeViewColumn */
                         "title", _("Changed"),
                         "min-width", COLUMN_CHANGED_MIN_WIDTH,
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, FALSE);
  gtk_tree_view_column_add_attribute (column, renderer, "active", COLUMN_CHANGED);
  gtk_tree_view_append_column (tree_view, column);

  /* --------------------- third column is a spacer ---------------------- */
  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         /* GtkTreeViewColumn */
                         "expand", TRUE,
                         "resizable", FALSE,
                         "sizing",GTK_TREE_VIEW_COLUMN_FIXED,
                         NULL);

  gtk_tree_view_append_column (tree_view, column);

  /* add the tree view to the scrolled window */
  geda_container_add (scrolled_win, tree_view);

  /* add the scrolled window to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (pagesel)->vbox), scrolled_win, TRUE, TRUE, 0);

  gtk_widget_show_all (scrolled_win);

  /* add a label below the scrolled window */
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkLabel */
                        "label", _("Right click on rows for more options..."),
                        NULL);

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (ThisDialog)->vbox), label, FALSE, FALSE, 5);
  gtk_widget_show (label);

  /* Save pointer to the TreeView in pagesel structure */
  pagesel->treeview = tree_view;

  /* ------------------ setup tooltips for the tree View ----------------- */

  gtk_widget_set_has_tooltip (GTK_WIDGET (tree_view), TRUE);

  g_signal_connect (tree_view, "query-tooltip",
                    G_CALLBACK (pagesel_callback_query_tooltip), pagesel);

  /* -------------------- Setup the Drag & Drop Support ------------------- */

  gtk_drag_dest_set(GTK_WIDGET(tree_view), GTK_DEST_DEFAULT_ALL,
                    dnd_target_list, dnd_ntargets,
                    GDK_ACTION_COPY|GDK_ACTION_MOVE|GDK_ACTION_LINK);

  g_signal_connect(tree_view, "drag_data_received",
                   G_CALLBACK(pagesel_dnd_drag_receive), pagesel);

  /* -------------------- Setup the Action/Button Area ------------------- */
  GtkWidget *action_hbox;
  GtkWidget *switch_vbox;
  GtkWidget *butt_hbox;
  GtkWidget *AutoHeightSwitch;
  GtkWidget *ShowFullNameSwitch;
  GtkWidget *fresh_butt;
  GtkWidget *okay_butt;
  GtkWidget *alignment;
  GtkDialog *Dialog;

  Dialog = (GtkDialog*)ThisDialog;

  /* Remove Gtk action area from the dialog and don't re-use it */
  action_hbox = Dialog->action_area;
  geda_container_remove(Dialog->vbox, action_hbox);

  HD_ACTION_SEPARATOR (Dialog->vbox);

  action_hbox = gtk_hbox_new(FALSE, 0);
  g_object_set (action_hbox, "visible", TRUE, NULL);
  gtk_box_pack_end (GTK_BOX (Dialog->vbox), action_hbox, FALSE, FALSE, 0);

  /* Replace the action_area with the new container */
  Dialog->action_area = action_hbox;

  /* Create and add container for filename option toggle switch */
  switch_vbox = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_start (GTK_BOX (action_hbox), switch_vbox, FALSE, FALSE, 0);

  /* Setup the option toggle switches */
  AutoHeightSwitch   = NULL;
  ShowFullNameSwitch = NULL;

  /* Create a new Toggle Switch widget for ShowFullNames */
  EDA_SWITCH(switch_vbox, ShowFullName, 0, full_names);

  /* Setup callback for Toggle Switch widget */
  GEDA_CALLBACK_SWITCH (ShowFullName, pagesel_show_fullnames_toggled, ThisDialog)

    /* Create a new Toggle Switch widget for AutoHeight */
  EDA_SWITCH(switch_vbox, AutoHeight, 0, auto_height);
  gtk_widget_show_all(switch_vbox); /* set every widget in container visible */

  /* Setup callback for Toggle Switch widget */
  GEDA_CALLBACK_SWITCH (AutoHeight, pagesel_auto_height_toggled, ThisDialog)

  /* Create and add alignment container to hold the button container */
  alignment = g_object_new (GTK_TYPE_ALIGNMENT,
                            "right-padding", 0,
                            "left-padding",  50,
                            "xscale",        1.0,
                            "yscale",        0.0,
                            "xalign",        1.0,
                            "yalign",        0.5,
                            NULL);

  g_object_set (alignment, "visible", TRUE, NULL);
  gtk_box_pack_end (GTK_BOX (action_hbox), alignment, TRUE, TRUE, 0);

  /* Create a Horizontal Box for the buttons to go into */
  butt_hbox = gtk_hbox_new(FALSE, 0);
  g_object_set (butt_hbox, "visible", TRUE, NULL);
  geda_container_add (alignment, butt_hbox);

  /* Create and connect the Close and Refresh Buttons */
  fresh_butt = gtk_button_new_from_stock (GTK_STOCK_REFRESH);
  okay_butt = gtk_button_new_from_stock (GTK_STOCK_OK);

  g_signal_connect( ThisDialog, "notify::gschem-toplevel",
                    G_CALLBACK( notify_gschem_toplevel_cb ), NULL);

  g_signal_connect (fresh_butt,
                    "clicked",
                    G_CALLBACK (pagesel_callback_refresh_clicked),
                    pagesel);

  g_signal_connect (okay_butt,
                    "clicked",
                    G_CALLBACK (pagesel_callback_close_clicked),
                    pagesel);

  g_object_set (okay_butt, "visible", TRUE, NULL);
  g_object_set (fresh_butt, "visible", TRUE, "can-default", TRUE, NULL);

  gtk_box_pack_end (GTK_BOX (butt_hbox), okay_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_box_pack_end (GTK_BOX (butt_hbox), fresh_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_widget_set_size_request(okay_butt, DIALOG_BUTTON_HSIZE + 50, -1);

  gtk_dialog_set_default_response (Dialog, GEDA_RESPONSE_REFRESH);
  gtk_widget_grab_default (fresh_butt);

  pagesel->action_height = 0;
}

/*!
 * \brief Update tree model of <B>pagesel</B>'s treeview.
 * \par Function Description
 *  Updates the tree model of <B>pagesel</B>\'s treeview.
 *
 *  Right now, each time it is called, it rebuilds all the model from the
 *  list of pages passed in.
 *  It is a recursive function to populate the tree store
 *
 * \param [in] model   GtkTreeModel to update.
 * \param [in] parent  GtkTreeIter pointer to tree root.
 * \param [in] pages   PageList of pages for this toplevel.
 * \param [in] page    The Page object to update tree model from.
 */
static void
add_page(GtkTreeModel *model, GtkTreeIter *parent, PageList *pages, Page *page)
{
  GList *p_iter;
  GtkTreeIter iter;

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

    Page *p_current = (Page *)p_iter->data;

    if (p_current->hierarchy_up == page->pid) {
      add_page (model, &iter, pages, p_current);
    }
  }
}

/*!
 * \internal
 * \brief Select the current page in the Pagesel Dialog treeview
 * \par Function Description
 *  Recursive function to select the current page in the treeview
 */
static void
select_page(GtkTreeView *treeview, GtkTreeIter *parent, Page *page)
{
  Page         *p_current;
  GtkTreeModel *treemodel;
  GtkTreeIter   iter;

  treemodel = gtk_tree_view_get_model (treeview);

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

/*!
 * \brief Updates the TreeView on the Pagesel Dialog
 * \par Function Description
 *  This function completely rebuilds the model for the TreeView
 *  each time the function is called, thus updating the list of
 *  pages displayed in the Dialog and the status pages.
 *
 *  This function is declared in the header x_pagesel.h and is called
 *  from i_window_idle_notify_dialogs directly because i_window_idle_
 *  notify_dialogs is ran in an idle thread. Other modules call
 *  x_pagesel_update, which calls x_pagesel_idle_update to create a
 *  new thread to call this function.
 *
 *  Three functions call this procedure directly and they are all
 *  callback routines, i.e. members of Pagesel:
 *  \par
 *  <DL>
 *    <DT>notify_gschem_toplevel_cb</DT>
 *    <DT>pagesel_show_fullnames_toggled</DT>
 *    <DT>x_pagesel_callback_response</DT>
 *  </DL>
 */
void
pagesel_update (Pagesel *pagesel)
{
  GschemToplevel *w_current;
  GedaToplevel   *toplevel;
  Page           *p_current;
  GtkTreeModel   *model;
  GList          *iter;
  GList          *pages;

  w_current = GSCHEM_DIALOG (pagesel)->w_current;
  toplevel  = w_current->toplevel;

  model     = gtk_tree_view_get_model (pagesel->treeview);

  /* wipe out every thing in the store */
  gtk_tree_store_clear (GTK_TREE_STORE (model));

  pages = geda_list_get_glist(toplevel->pages);

  /* now rebuild */
  for (iter = pages; iter != NULL; iter = iter->next) {

    int pid;

    p_current = (Page *)iter->data;
    pid       = p_current->hierarchy_up;

    /* find every page that is not a hierarchy-down of another page */
    if (pid < 0 || geda_struct_page_search_by_page_id (toplevel->pages, pid) == NULL)
    {
      add_page (model, NULL, toplevel->pages, p_current);
    }
  }

  /* select the current page in the treeview */
  select_page (pagesel->treeview, NULL, toplevel->page_current);
}

/** @} endgroup Page-Select-Dialog */

#undef ThisDialog
