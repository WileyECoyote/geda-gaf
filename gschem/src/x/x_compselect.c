/* -*- C x_compselect.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details..
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file x_compselect.c
 * \brief A dialog box for selecting Component Symbols
 */

#include <ctype.h>

#include <gschem.h>

#include <gschem_xdefines.h>            /* Define dialog default internal spacing */
#include <gschem_dialog.h>              /* Definition of base Dialog Class */

#include <geda/geda_dialog_controls.h>  /* Macros for Dialogs */
#include <geda_widgets.h>               /* Switches use geda_labels */

#include <x_compselect.h>
#include <geda_keysyms.h>
#include <geda_debug.h>

/**  \defgroup Component-Select-Dialog Component Select Dialog Implementation
 *  @{
 *  \ingroup Component-Dialog Editing-Dialogs
 *  \par
 *  This group contains routines for the Component Select dialog. The
 *  Component Select Dialog implements Component Select Dialog Class
 *  and is used to select symbols to be inserted into schematics.
 */

/*! \def COMPSELECT_FILTER_INTERVAL
 *  \brief The time interval between request and actual filtering
 *
 *  This constant is the time-lag between user modifications in
 *  the filter entry and the actual evaluation of the filter which
 *  ultimately updates the display. This helps reduce the frequency
 *  of evaluation of the filter as the user types.
 *
 *  Unit is milliseconds.
 */
#define COMPSELECT_FILTER_INTERVAL 250

#define MASK compselect->style_flag

typedef enum {
  IN_USE_TAB=0,
  STD_TAB,
  MAN_TAB,
  SIM_TAB,
  LOCAL_TAB
} DialogTabs;

const char *IDS_COMPSELECT_TABS[] = {
  "In Use", "Std", "Manf", "Sim", "Local", /* Tab Name Strings*/
  NULL
};

const char *IDS_CATEGORIES[] = {
  "",
  "Standard", "Manufacturers", "Simulation", "Local", /* Tab Name Strings*/
  NULL
};

/*! \typedef IDE_CS_ControlID Enumerated Control IDs
 *  \memberof Compselect
 */
typedef enum IDE_CS_ControlID {
       FilterEntry,
       ClearButton,
       RefreshAllViews,
       CollapseButton,
       ExpandButton,
       Continue,
       SortLibrary,
       ShowGroups,
       SubGroups
} ControlID;

static WidgetStringData DialogStrings[] = {
  { "entry_filter",       "Filter:",       "Enter a filter string"},
  { "clear-button",       "Clear",         "Clear the filter entry"},
  { "refresh-button",     "Refresh all",   "Refresh all views"},
  { "collapse_button",    "Collapse all",  "Close all Library containers"},
  { "expand_button",      "Expand all",    "Expand all Library containers"},
  { "ContinueSwitch",     "Continue",      "Enable or diable continuation mode"},
  { "SortLibrarySwitch",  "Sort",          "If this is enabled then the component library will be sorted in alphanumeric order.\n This option is cosmetic and will not alter the component search order (latest added gets scanned first)."},
  { "ShowGroupsSwitch",   "Groups",        "Enable or disable group folders in tree views"},
  { "SubGroupsSwitch",    "Assort",        "Enable or disable subfolder with in group categories"},
  { NULL, NULL, NULL},
};

static GtkWidget *tree_view_popup_menu;

typedef enum  { ExpandFolder,
                ExpandAll,
                CloseFolder,
                RefreshView,
                RescanLibs,
                StylesMenu,
                ShowToolTips,
                ToolTipsOn,
                ToolTipsOff

} IDE_CS_Popup_items; /* Enumerators to reference the string below: */

static char *popup_items[]={ N_("Open"),
                             N_("Open all"),
                             N_("Close"),
                             N_("Refresh"),
                             N_("Rescan"),
                             N_("Styles"),
                             N_("Tooltips"),
                             N_("On"),
                             N_("Off"),
                             NULL
};

static char *popup_tips[]={  N_("Expand one level"),
                             N_("Expand all sub-folders"),
                             N_("Close this folder"),
                             N_("Refresh this view"),
                             N_("Rescan the libraries"),
                             N_("Set styles options"),
                             N_("Tooltip settings"),
                             N_("Enable tooltips"),
                             N_("Disable tooltips"),
                             NULL
};

enum
{
  REFRESH,
  LAST_SIGNAL
};

static unsigned int signals[LAST_SIGNAL] = { 0 };

/*!
 * \brief Component Selection Dialog Response Handler
 * \par Function Description
 *  This function handles the response <B>arg1</B> of the component
 *  selection dialog <B>dialog</B>.
 *
 *  Parameter <B>user_data</B> is a pointer on the relevant toplevel
 *  structure.
 *
 * \param [in] dialog    The component selection dialog.
 * \param [in] response  The response ID.
 * \param [in] user_data A pointer on the GschemToplevel environment.
 */
static void
x_compselect_callback_response(GtkDialog *dialog, int response, void *user_data)
{
  Compselect     *compselect = (Compselect*)dialog;
  GschemToplevel *w_current  = (GschemToplevel *)user_data;
  GedaToplevel   *toplevel   = w_current->toplevel;

  switch (response) {

    case COMPSELECT_RESPONSE_PLACE: {
      /* Check and resolve current state */
      if (w_current->event_state == COMPMODE) {

        if (toplevel->page_current->place_list != NULL) {

          /* Delete the component which was being placed */
          if (w_current->rubber_visible) {
            o_place_invalidate_rubber (w_current, FALSE);
          }
          w_current->rubber_visible = FALSE;
          geda_struct_place_free_place_list(toplevel);
        }
      }
      else if (w_current->event_state != SELECT && w_current->inside_action)
      {
        /* Cancel whatever other action is currently in progress */
        o_redraw_cleanstates (w_current);
      }

      CompselectBehavior behavior;

      CLibSymbol *symbol = NULL;

      /* Get pointer to selected symbol and behavior combo state */
      /* (Both of which could have been saved in dialog structure) */
      g_object_get (compselect, "symbol",   &symbol,
                                "behavior", &behavior, NULL);

      if (symbol == NULL) {
        /* If there is no symbol selected, switch to SELECT mode */
        w_current->event_state = SELECT;
      }
      else {

        w_current->include_complex  = FALSE;
        w_current->embed_components = FALSE;

        if (behavior == COMPSELECT_BEHAVIOR_EMBED) {
          w_current->embed_components = TRUE;
        }
        else if (behavior == COMPSELECT_BEHAVIOR_INCLUDE) {
          w_current->include_complex = TRUE;
        }

        /* Otherwise set the new symbol to place */
        o_complex_start (w_current, symbol, COMPMODE);
        i_status_show_msg(w_current, "Place Component");
      }
      break;
    }

    case COMPSELECT_RESPONSE_HIDE:
      /* Response when clicking on the "Ok" button, If there is no
       * component in the complex place list, set the current one */

      if (toplevel->page_current->place_list == NULL) {
        gtk_dialog_response (GTK_DIALOG (compselect),
                             COMPSELECT_RESPONSE_PLACE);
      }

      /* Hide the Component Select Dialog */
      g_object_set (compselect, "hidden", TRUE, NULL);
      break;

    case GEDA_RESPONSE_CLOSE:
    case GEDA_RESPONSE_DELETE_EVENT:
      if (GTK_WIDGET (dialog) == w_current->cswindow) {
        if (ThisDialog->style_menu_widgets) {
          g_slist_free(ThisDialog->style_menu_widgets);
        }
        /* gtk_widget_destroy(tree_view_popup_menu); */
        if (GTK_IS_DIALOG(dialog)) {
          gtk_widget_destroy (GTK_WIDGET (dialog));
        }
      }

      if (w_current->event_state == COMPMODE) {

        /* If user clicked on symbol in the tree and did not place, there
         * could be a component in the place list, so check and release */
        geda_struct_place_free_place_list(toplevel);

        /* Cannot wait for base class to do this*/
        w_current->cswindow = NULL;

      }

      w_current->include_complex = FALSE;
      i_event_stop_action_handler (w_current);
      break;

    default:
      /* Do nothing, in case there's another handler function
       * which can handle the response ID received. */
      break;
  }
}

/*! \brief Opens a component selection dialog.
 *  \par Function Description
 *  This function creates the Component Chooser Dialog for <B>toplevel</B>
 *  if the dialog does not already exist. In this latter case, This function
 *  only raises the dialog.
 *
 *  \param [in] w_current  The GschemToplevel environment data.
 */
void x_compselect_open (GschemToplevel *w_current)
{
  GtkWidget  *ThisDialog;
  Compselect *ActiveDialog;
  GedaObject *o_current;

  ThisDialog = w_current->cswindow;

  if (ThisDialog == NULL) {

    ThisDialog = g_object_new (TYPE_COMPSELECT, /* GschemDialog */
                               "parent", w_current->main_window,
                               "settings-name",  DialogSettings,
                               "gschem-toplevel", w_current,
                                NULL);

    g_signal_connect (ThisDialog, "response",
                      G_CALLBACK (x_compselect_callback_response),
                      w_current);

    gtk_widget_show (ThisDialog);
    w_current->cswindow = ThisDialog;

  }
  else {
    gtk_window_present (GTK_WINDOW (ThisDialog));
  }

  ActiveDialog = COMPSELECT (ThisDialog);

  GTK_EDITITABLE(ActiveDialog->entry_filter);

  if (strcmp (GetEntryText (ActiveDialog->entry_filter), "") != 0) {
    gtk_widget_grab_focus ((GtkWidget*) ActiveDialog->entry_filter);
  }

  o_current = o_select_return_first_object(w_current);

  if (GEDA_IS_COMPLEX(o_current)) {

    char *sym_name;

    sym_name = geda_complex_get_filename(o_current->complex);

    g_object_set(ThisDialog, "symbol", sym_name, NULL);
  }
}

/** @} end group Component-Select-Dialog */

/** \defgroup Component-Dialog-Class Component Select Dialog Class
 *  @{
 *  \ingroup Component-Dialog
 *  \par
 *  Definition of the Component Select dialog Class. The Component
 *  Select Dialog Class is derived from #GschemDialogClass.
 */

/*!
 * \brief Unselect selection in the active tree view.
 * \par Function Description
 *  This function deselects the selection in the active viewtree
 *  <B>toplevel</B>.
 *
 * \param [in] w_current  The GschemToplevel environment data.
 */
void x_compselect_deselect (GschemToplevel *w_current)
{
  Compselect *compselect = COMPSELECT (w_current->cswindow);

  if (compselect != NULL) {

    switch (compselect->active_tab) {
      case IN_USE_TAB:
        gtk_tree_selection_unselect_all (
          gtk_tree_view_get_selection (compselect->inusetreeview));
        break;
      case STD_TAB:
        gtk_tree_selection_unselect_all (
          gtk_tree_view_get_selection (compselect->stdtreeview));
        break;
      case MAN_TAB:
        gtk_tree_selection_unselect_all (
          gtk_tree_view_get_selection (compselect->mantreeview));
        break;
      case SIM_TAB:
        gtk_tree_selection_unselect_all (
          gtk_tree_view_get_selection (compselect->simtreeview));
        break;
      case LOCAL_TAB:
        gtk_tree_selection_unselect_all (
          gtk_tree_view_get_selection (compselect->localtreeview));
        break;
      default:
        BUG_MSG("ignoring Invalid Tab Id");
    }
  }
}

enum {
  PROP_SYMBOL=1,
  PROP_BEHAVIOR,
  PROP_HIDDEN,
  PROP_VIEW,
};

enum {
  ATTRIBUTE_COLUMN_NAME = 0,
  ATTRIBUTE_COLUMN_VALUE,
  NUM_ATTRIBUTE_COLUMNS
};

/* Define the column containing the symbol data */
#define IU_DATA_COLUMN 0
enum {
  LVC_ROW_TYPE = 0,
  LVC_ROW_DATA,
  LVC_TOOLTIP_TEXT,
  NUM_LV_COLUMNS
};

static GtkWidget *ContinueSwitch;
static GtkWidget *SortLibrarySwitch;
static GtkWidget *SubGroupsSwitch;
static GtkWidget *ShowGroupsSwitch;

static GObjectClass *compselect_parent_class = NULL;

static void     compselect_class_init  (void *class, void *class_data);
static GObject *compselect_constructor (GType    type,
                                        unsigned int n_construct_properties,
                                        GObjectConstructParam *construct_params);
static void compselect_finalize        (GObject *object);
static void compselect_set_property    (GObject *object,
                                        unsigned int property_id,
                                        const GValue *value,
                                        GParamSpec *pspec);
static void compselect_get_property    (GObject *object,
                                        unsigned int property_id,
                                        GValue *value,
                                        GParamSpec *pspec);

/*!
 * \brief Get Active Tree View.
 * \par Function Description
 *  This function returns pointer to the active viewtree
 *  <B>toplevel</B>.
 *
 * \param [in] compselect This Dialog.
 */
static GtkTreeView *get_active_tree_view (Compselect *compselect)
{
  GtkTreeView  *view;

  if (compselect == NULL)
    return NULL;

  switch (compselect->active_tab) {
  case IN_USE_TAB:
    view = compselect->inusetreeview;
    break;
  case STD_TAB:
    view = compselect->stdtreeview;
    break;
  case MAN_TAB:
    view = compselect->mantreeview;
    break;
  case SIM_TAB:
    view = compselect->simtreeview;
    break;
  case LOCAL_TAB:
    view = compselect->localtreeview;
    break;
  default:
    view = NULL;
  }
  return view;
}

/*!
 * \brief Sets data for a particular cell of the In Use treeview.
 * \par Function Description
 *  This function determines what data is to be displayed in the
 *  "in use" symbol selection view. The model is a list of symbols.
 *  geda_struct_clib_symbol_get_name() is called to get the text
 *  to display.
 */
static void
inuse_treeview_set_cell_data (GtkTreeViewColumn *tree_column,
                              GtkCellRenderer   *cell,
                              GtkTreeModel      *tree_model,
                              GtkTreeIter       *iter,
                              void              *data)
{
  CLibSymbol *symbol;

  gtk_tree_model_get (tree_model, iter, IU_DATA_COLUMN, &symbol, -1);
  g_object_set (cell, "text", geda_struct_clib_symbol_get_name (symbol), NULL);
}

/*!
 * \brief Returns whether a treeview node contains symbol data.
 * \par Function Description
 *  This function returns TRUE if the integer stored in column LVC_ROW_TYPE
 *  at the given (row) \a iter is not zero. A zero in this columns indicates
 *  the row contains a source (folder or category) data, otherwise the row
 *  is for a symbol record.
 */
static inline bool
is_symbol_record (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
  if (iter->stamp != 0) {

    unsigned result;

    gtk_tree_model_get (tree_model, iter, LVC_ROW_TYPE, &result, -1);

    return (result != 0);
  }

  return FALSE;
}

/*!
 * \brief Sets data for a particular cell in Library treeviews.
 * \par Function Description
 *  This function determines what data is to be displayed in the
 *  selection selection view.
 *
 *  The top level of the model contains sources, and the next symbols.
 *  geda_struct_clib_source_get_name() or geda_struct_clib_symbol_get_name()
 *  as appropriate is called to get the text to display.
 */
static void
lib_treeview_set_cell_data (GtkTreeViewColumn *tree_column,
                            GtkCellRenderer   *cell,
                            GtkTreeModel      *tree_model,
                            GtkTreeIter       *iter,
                            void              *data)
{
  CLibSource *source;
  CLibSymbol *symbol;
  const char *text;
  const char *ptr;

  if (is_symbol_record(tree_model, iter)) {

    gtk_tree_model_get (tree_model, iter, LVC_ROW_DATA, &symbol, -1);
    text = geda_struct_clib_symbol_get_name (symbol);

  }
  else { /* Must be a source. */

    gtk_tree_model_get (tree_model, iter, LVC_ROW_DATA, &source, -1);
    text = geda_struct_clib_source_get_name (source);
    ptr  = strstr(source->name, "/");       /* look for a slash */

    if (ptr!= NULL) {
      text = ptr + 1;
    }
  }
  g_object_set (cell, "text", text, NULL);
}

/*!
 * \brief Determines visibility of items of the library treeview.
 * \par Function Description
 *  This is the function used to filter entries of the component
 *  selection tree.
 *
 * \param [in] model The current selection in the treeview.
 * \param [in] iter  An iterator on a component or folder in the tree.
 * \param [in] data  The component selection dialog.
 *
 * \returns TRUE if item should be visible, FALSE otherwise.
 */
static bool
lib_model_filter_visible_func (GtkTreeModel *model,
                               GtkTreeIter  *iter,
                               void         *data)
{
  Compselect *compselect = (Compselect*)data;
  const char *text;

  bool  ret;

  if (!IS_COMPSELECT (data)) {
    BUG_MSG("Dialog->NULL");
    return FALSE;
  }

  if (GTK_IS_ENTRY(compselect->entry_filter)) {

     text = GetEntryText (compselect->entry_filter);
     if (geda_strequal (text, "")) {
       return TRUE;
     }
  }
  else {
    return TRUE;     /* Isn't this an error condition ? */
  }

  /* If this is a source, only display if there are children */
  if (gtk_tree_model_iter_has_child (model, iter)) {

    GtkTreeIter iter2;

    ret = FALSE;

    if (gtk_tree_model_iter_children (model, &iter2, iter))

      do {

        if (lib_model_filter_visible_func (model, &iter2, data)) {
          ret = TRUE;
          break;
        }
      } while (gtk_tree_model_iter_next (model, &iter2));
  }
  else {

    CLibSymbol *sym;

    const char *compname;
          char *compname_upper, *text_upper, *pattern;

    gtk_tree_model_get (model, iter, LVC_ROW_DATA, &sym, -1);

    compname       = geda_struct_clib_symbol_get_name (sym);
    /* Do a case insensitive uppercase comparison */
    compname_upper = g_ascii_strup (compname, -1);
    text_upper     = g_ascii_strup (text, -1);
    pattern        = geda_strconcat ("*", text_upper, "*", NULL);
    ret            = g_pattern_match_simple (pattern, compname_upper);

    GEDA_FREE (compname_upper);
    GEDA_FREE (text_upper);
    GEDA_FREE (pattern);
  }

  return ret;
}

/*!
 * \brief Component row activated - double-click handler:
 * \par Function Description
 *  This functions handles row activation of a component row as a
 *  convenience to the user, the row expands or colapses any node
 *  with children.
 *
 * \param [in] tree_view The component treeview.
 * \param [in] path      The GtkTreePath to the activated row.
 * \param [in] column    The GtkTreeViewColumn in which the activation occurred.
 * \param [in] dialog    The component selection dialog.
 */
static void
compselect_tree_row_activated (GtkTreeView       *tree_view,
                               GtkTreePath       *path,
                               GtkTreeViewColumn *column,
                               void              *dialog)
{
  GtkTreeModel *model;
  GtkTreeIter   iter;

  model = gtk_tree_view_get_model (tree_view);
  gtk_tree_model_get_iter (model, &iter, path);

  if (is_symbol_record(model, &iter)) {
    gtk_dialog_response (GTK_DIALOG (dialog), COMPSELECT_RESPONSE_HIDE);
  }
  else if (gtk_tree_view_row_expanded (tree_view, path)) {
    gtk_tree_view_collapse_row (tree_view, path);
  }
  else {
    gtk_tree_view_expand_row (tree_view, path, FALSE);
  }
}

/*!
 * \brief Open Rows in Tree in Tree-View
 * \par Function Description
 *  This function is not call directly.
 *
 * \param [in] tree_view  Pointer to a library tree-view widget
 * \param [in] expand_all
 */
static void
compselect_tree_open_rows (GtkTreeView *tree_view, bool expand_all)
{
  GtkTreeModel     *model = NULL;
  GtkTreeIter       iter;
  GtkTreeSelection *selection;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));

  if (gtk_tree_selection_get_selected(selection, &model, &iter)) {

    GtkTreePath *path = gtk_tree_model_get_path (model, &iter);

    gtk_tree_view_expand_row (tree_view, path, expand_all);

    gtk_tree_path_free(path);
  }
}

/*!
 * \brief Open A Row in Tree call-back for Tree-View pop-up menu
 * \par Function Description
 *  This function performs the same functionality as double-clicking on a
 *  closed parent row in a tree-view but is enacted from the right context
 *  menu. The function calls the compselect_tree_open_rows function above
 *  with a value of FALSE for the expand-all argument. This option is only
 *  included on the right menu for the sake of completeness.
 *
 * \param [in] menu_widget  Un-used menu item widget on the pop-up menu
 * \param [in] tree_view    Pointer to the tree-view widget when popup activated
 */
static void
compselect_open_tree_row (GtkWidget *menu_widget, GtkTreeView *tree_view)
{
  compselect_tree_open_rows (tree_view, FALSE);
}

/*!
 * \brief Open All Rows in Tree call-back for Tree-View pop-up menu
 * \par Function Description
 *  The function calls the compselect_tree_open_rows function above with a
 *  value of TRUE for the expand-all argument. This is the same functionality
 *  as pressing the Expand All button widget.
 *
 * \param [in] menu_widget  Un-used menu item widget on the pop-up menu
 * \param [in] tree_view    Pointer to the tree-view widget when popup activated
 */
static void
compselect_open_tree_rows (GtkWidget *menu_widget, GtkTreeView *tree_view)
{
  compselect_tree_open_rows (tree_view, TRUE);
}

/*!
 * \internal csSearchRecord
 * \par
 *  Statically allocated structure used by compselect_open_to_symbol
 *  in order to pass two pointers to compselect_foreach_func using
 *  the gtk_tree_model_foreach function.
 */
typedef struct csSearchRecord {
  Compselect *dialog;
  const char *sym_name;
} csSearchRecord;

/*!
 * \brief Search for Compselect Tree Row with Symbol Name
 * \par Function Description
 *  GtkTreeModelForeachFunc function used to search for model row
 *  containing symbol name stored in the data record, if found the
 *  tree is expanded and the row made visible.
 *
 * \param [in] model  GtkTreeModel for the active notebook tab
 * \param [in] path   GtkTreePath to the current row data
 * \param [in] iter   GtkTreeIter to the current row data
 * \param [in] data   csSearchRecord
 */
static bool compselect_foreach_func (GtkTreeModel *model,
                                     GtkTreePath  *path,
                                     GtkTreeIter  *iter,
                                     void         *data)
{
  csSearchRecord *SearchRecord = data;

  if (is_symbol_record(model, iter)) {

    CLibSymbol *symbol;
    const char *sym_name;
    const char *ptr_sym_name;

    gtk_tree_model_get (model, iter, LVC_ROW_DATA, &symbol, -1);

    ptr_sym_name = geda_struct_clib_symbol_get_name(symbol);
    sym_name = SearchRecord->sym_name;

    if (ptr_sym_name && (strcmp(ptr_sym_name, sym_name) == 0)) {

      GtkTreeView *tree_view;

      tree_view = get_active_tree_view (SearchRecord->dialog);

      path = gtk_tree_model_get_path (model, iter);
      gtk_tree_view_expand_to_path(tree_view, path);
      gtk_tree_view_set_cursor (tree_view, path, NULL, FALSE);
      geda_tree_view_row_make_visible (tree_view, iter, TRUE);
      gtk_tree_path_free(path);
      return TRUE;
    }
  }

  return FALSE; /* do not stop traversing model, call us with next row */
}

/*!
 * \brief Open Compselect Tree Row to Symbol Name
 * \par Function Description
 *  This function handles the set "symbol" property, which is used at
 *  launch in an attempt to open the tree to the component selected.
 *  This function only succeeds if the component selected is a member
 *  of the active notebook tab.
 *
 * \param [in] compselect  Pointer to the Compselect dialog
 * \param [in] sym_name    Name of the symbol including extension
 *
 * \todo consider adding a flag to csSearchRecord to check for success
 *       and query other models on failure.
 */
static void
compselect_open_to_symbol (Compselect *ThisDialog, const char *sym_name)
{
  if (sym_name) {

    GtkTreeModel *model;
    GtkTreeView  *tree_view;

    csSearchRecord SearchRecord;

    SearchRecord.dialog   = ThisDialog;
    SearchRecord.sym_name = sym_name;

    tree_view = get_active_tree_view (ThisDialog);
    model     = gtk_tree_view_get_model (tree_view);

    gtk_tree_model_foreach(model, compselect_foreach_func, &SearchRecord);
  }
}

/*!
 * \brief Close Row in Tree call-back for Tree-View pop-up menu
 * \par Function Description
 *  This function performs the same functionality as double-clicking on an
 *  opened parent row in a tree-view but is initiated from the right context
 *  menu to allows closing of large tree folders without scrolling up to
 *  the parent or using the collapse all function.
 *
 * \param [in] menu_widget  Un-used menu item widget on the pop-up menu
 * \param [in] tree_view    Pointer to the tree-view widget when popup activated
 */
static void
compselect_close_tree_row (GtkWidget *menu_widget, GtkTreeView *tree_view)
{
  GtkTreeModel     *model = NULL;
  GtkTreeIter       iter;
  GtkTreeSelection *selection;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));

  if (gtk_tree_selection_get_selected(selection, &model, &iter)) {

    GtkTreePath *path = gtk_tree_model_get_path (model, &iter);

    if (is_symbol_record(model, &iter)) {
      gtk_tree_path_up (path);
    }

    gtk_tree_view_collapse_row (tree_view, path);

    gtk_tree_path_free(path);
  }
}

/*!
 * \brief Update the model of the attributes treeview
 * \par Function Description
 *  This function acquires the toplevel attributes from the preview
 *  widget and puts them into the model of the <b>attrtreeview</b> widget.
 *
 * \param [in] compselect       The dialog compselect
 * \param [in] preview_toplevel The toplevel of the preview widget
 */
void
compselect_update_attributes_model (Compselect   *compselect,
                                    GedaToplevel *preview_toplevel)
{
  GtkListStore      *model;
  GtkTreeIter        iter;
  GtkTreeViewColumn *column;

  GList *o_iter, *o_attrlist, *filter_list;

  char   *name, *value;

  model = (GtkListStore*) gtk_tree_view_get_model (compselect->attrtreeview);

  gtk_list_store_clear (model);

  /*! \note: Invalidate the width for the attribute value column, so the
   * column is  re-sized based on the new data being shown. Symbols with
   * long values are common, and we don't want having viewed those forcing
   * a h-scroll bar on symbols with short valued attributes.
   *
   * We might also consider invalidating the attribute name columns,
   * however that gives an inconsistent column division when swithing
   * between symbols, which doesn't look nice. For now, assume that
   * the name column can keep the max width gained whilst previewing.
   */
  column = gtk_tree_view_get_column (compselect->attrtreeview,
                                     ATTRIBUTE_COLUMN_VALUE);
  gtk_tree_view_column_queue_resize (column);

  if (preview_toplevel->page_current == NULL) {
    return;
  }

  o_attrlist =
    geda_list_find_floating (geda_struct_page_get_objects (preview_toplevel->page_current));

  filter_list = GSCHEM_DIALOG (compselect)->w_current->component_select_attrlist;

  if (filter_list != NULL && strcmp (filter_list->data, "*") == 0) {

    /* display all attributes in alphabetical order */
    o_attrlist = g_list_sort(o_attrlist, (GCompareFunc)geda_text_object_strcmp);

    for (o_iter = o_attrlist; o_iter != NULL; o_iter = g_list_next (o_iter)) {

      GedaObject *o_current = o_iter->data;

      geda_attrib_object_get_name_value (o_current, &name, &value);
      gtk_list_store_append (model, &iter);
      gtk_list_store_set (model, &iter, 0, name, 1, value, -1);
      GEDA_FREE (name);
      GEDA_FREE (value);
    }
  }
  else {

    GList  *listiter;

    /* Display only attribute that are in the filter list */
    for (listiter = filter_list; listiter != NULL; NEXT(listiter)) {

      for (o_iter = o_attrlist; o_iter != NULL; o_iter = g_list_next (o_iter)) {

        GedaObject *o_current = o_iter->data;

        if (geda_attrib_object_get_name_value (o_current, &name, &value)) {

          if (strcmp (name, listiter->data) == 0) {
            gtk_list_store_append (model, &iter);
            name[0] = toupper(name[0]);
            gtk_list_store_set (model, &iter, 0, name, 1, value, -1);
          }
          GEDA_FREE (name);
          GEDA_FREE (value);
        }
      }
    }
  }
  g_list_free (o_attrlist);
}

/*!
 * \brief Handles changes in the treeview selection.
 * \par Function Description
 *  This is the callback function called every time the user select
 *  a row in any component treeview of the dialog.
 *
 *  If the selection is not a selection of a component (a directory
 *  name), it does nothing. Otherwise it retrieves the <B>CLibSymbol</B>
 *  from the model.
 *
 *  If a symbol was selected, this function emits the <B>response</B>
 *  signal on the dialog so that the selected component can be placed
 *  whilst the dialog remains visible.
 *
 * \param [in] selection The current selection in the treeview.
 * \param [in] user_data The component selection dialog.
 */
static void
compselect_cb_tree_selection_changed (GtkTreeSelection *selection,
                                      void             *user_data)
{
  GtkTreeModel *model;
  GtkTreeIter   iter;
  Compselect   *compselect = (Compselect*)user_data;

  char *buffer          = NULL;
  bool is_symbol        = FALSE;

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    is_symbol = is_symbol_record(model, &iter);

    if (is_symbol) {

      const CLibSymbol *sym = NULL;
      GtkTreeView  *view;

      /* get pointer to tree that triggered signal */
      view = gtk_tree_selection_get_tree_view (selection);

      if (view == compselect->inusetreeview) {
        gtk_tree_model_get (model, &iter, IU_DATA_COLUMN, &sym, -1);
      }
      else {
        gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &sym, -1);
      }

      buffer = geda_struct_clib_symbol_get_data (sym);
    }
  }

  if (buffer) {

    /* update the preview with new symbol data */
    g_object_set (compselect->preview,
                  "buffer", buffer,
                  "active", (buffer != NULL),
                  NULL);

    /* update the attributes with the toplevel of the preview widget*/
    if (compselect->attrtreeview != NULL) {
      compselect_update_attributes_model (compselect,
                                          compselect->preview->
                                          preview_window->toplevel);
    }

    if (is_symbol) {

      /* signal a component has been selected to parent of dialog */
      g_signal_emit_by_name (compselect,
                             "response",
                             COMPSELECT_RESPONSE_PLACE,
                             NULL);
    }

    GEDA_FREE (buffer);
  }
}

/*!
 * \brief Apply Filter to supplied View.
 * \par Function Description
 *  This function applies the filtering of components in the treeview of
 *  the dialog based on the text in the search entry. The applying_filter
 *  variable is used here to prevent expanding when there is no text. This
 *  was added when the notebook sheets were added because the filtering is
 *  is applied to multiple tree views on an as needed basis, aka, only when
 *  the user switches Tab's with text in the filter entry widget.
 *
 *  The function is called whenever a filter timeout occurs or when the
 *  user changes tabs.
 *
 * \note The filtering done here is based on text entered by the user and
 *       has nothing to do with filtering based on style settings.
 *
 * \param [in] Dialog The component selection dialog Object.
 * \param [in] view   The Active Tree view to operate on.
 */
static void
compselect_apply_filter_tree_view (Compselect *Dialog, GtkTreeView *view)
{

  if (view != Dialog->inusetreeview) {

    GtkTreeModel *model;

    model = gtk_tree_view_get_model (view);

    if (model != NULL) {
      gtk_tree_model_filter_refilter ((GtkTreeModelFilter*) model);
    }

    if (Dialog->applying_filter) {
      gtk_tree_view_expand_all (view);
    }
  }
}

/*!
 * \brief Requests re-evaluation of the filter.
 * \par Function Description
 *  This is the timeout function for the filtering of component in the tree
 *  view of the dialog. The function checks for the presents of text in the
 *  entry widget and calls the preceeding function, compselect_apply_filter
 *  _tree_view, to apply the filter if there is text set in the entry. The
 *  function sets the boolean variable applying_filter, which is used to
 *  control if trees should be expanded or not.
 *
 * \note The timeout source that this callback is attached to is removed
 *       after the function executes, because the function return FALSE.
 *
 * \param [in] data The component selection dialog.
 * \returns FALSE to remove the timeout.
 */
static bool compselect_filter_timeout (void *data)
{
  Compselect  *Dialog = COMPSELECT(data);
  GtkTreeView *view;

  /* resets the source id in compselect */
  Dialog->filter_timeout = 0;

  view = get_active_tree_view (Dialog);

  if (strcmp (GetEntryText(Dialog->entry_filter), "") != 0) {
    /* filter text not-empty */
    /* Set Flag before applying filter */
    Dialog->applying_filter = TRUE;
    compselect_apply_filter_tree_view (Dialog, view);
  }
  else { /* if was filtering then */
    /* filter text is empty, collapse expanded tree */
    Dialog->applying_filter=FALSE;
    gtk_tree_view_collapse_all (view);
  }

  /* return FALSE to remove the source */
  return FALSE;
}

/*!
 * \brief Callback function for the changed signal of the filter entry.
 * \par Function Description
 *  This function monitors changes in the entry filter of the dialog.
 *  Specifically, this function manages the sensitivity of the clear
 *  button of the entry depending on its contents and also requests
 *  an update of the component list by re-evaluating filter at every
 *  change.
 *
 * \param [in] editable  The filter text entry.
 * \param [in] user_data The component selection dialog.
 */
static void
compselect_callback_filter_entry_changed (GtkEditable *editable,
                                          void        *user_data)
{
  Compselect *compselect = COMPSELECT (user_data);
  GtkWidget  *button;
  bool        sensitive;
  const char *text;

  /* turns button off if filter entry is empty otherwise turns it on */
  button = GTK_WIDGET (compselect->button_clear);

  text = GetEntryText(compselect->entry_filter);

  sensitive = text && (strcmp (text,"") != 0);

  if (gtk_widget_is_sensitive (button) != sensitive) {
    gtk_widget_set_sensitive (button, sensitive);
  }

  compselect->applying_filter = sensitive;

  /* Cancel any pending update of the component list filter */
  if (compselect->filter_timeout != 0) {
    g_source_remove (compselect->filter_timeout);
  }

  /* Schedule an update of the component list filter in
   * COMPSELECT_FILTER_INTERVAL milliseconds */
  compselect->filter_timeout = g_timeout_add (COMPSELECT_FILTER_INTERVAL,
                                              compselect_filter_timeout,
                                              compselect);
}

/*!
 * \brief Callback when a Switch is toggled on the Component Select Dialog
 * \par Function Description:
 *  This function changes images for switches that are toggled. The image
 *  is set to the opposite state, i.e. if ON use OFF image and if OFF use ON
 *  image. The function then checks which switches was toggled and set the
 *  variable associated with the switch. If the ShowGroups switch is turned
 *  off then the Sub-groups switch is disabled because it does not make sense
 *  to list every component in the library in a single column.
 *
 * \param [in]  widget  ptr to the switch, aka toggle-button, widget
 * \param [in]  Dialog  ptr to the dialog widget
 */
static void
compselect_cb_switch_toggled(GtkWidget *widget, GschemDialog *Dialog)
{
  GschemToplevel *w_current;
  Compselect     *ThisDialog;

  /* Change the Switch image */
  TOGGLE_SWITCH(widget);

  w_current = Dialog->w_current;
  ThisDialog = COMPSELECT (w_current->cswindow);

  if (widget == ContinueSwitch) {
    w_current->continue_component_place = GET_SWITCH_STATE(ContinueSwitch);
  }
  else {

    if (widget == SortLibrarySwitch) {
      w_current->sort_component_library = GET_SWITCH_STATE(SortLibrarySwitch);
    }
    else {

      if (widget == ShowGroupsSwitch) {

        ThisDialog->show_groups = GET_SWITCH_STATE(ShowGroupsSwitch);

        /* if 1st level groups is off then 2nd level MUST be on */
        if (ThisDialog->show_groups == FALSE) {
          ThisDialog->subgroups = TRUE;
          SetSwitch(SubGroups, TRUE);
        }

        /* Update Widget sensitivities */
        gtk_widget_set_sensitive (SubGroupsSwitch, ThisDialog->show_groups);
      }
      else {

        if (widget == SubGroupsSwitch) {
          ThisDialog->subgroups = GET_SWITCH_STATE(SubGroupsSwitch);
        }
      }
    }
    g_signal_emit (ThisDialog, signals[REFRESH], 0);
  }

  return;
}

/*!
 * \brief Sets toggle item state without emitting signal
 * \par Function Description:
 *  This function sets the active state of a toggle widget while blocking
 *  signals if the widget has the ID of it's signal handler embedded,
 *  otherwise the toggle widget is set without blocking the signal.
 */
static void
compselect_set_item_active(GtkWidget *widget, bool state) {

  if (GTK_IS_WIDGET(widget)) {

    unsigned long handler;

    handler = (unsigned long) GEDA_OBJECT_GET_DATA(widget, "handler");

    if (handler) {
      g_signal_handler_block (widget, handler);       /* disable signal */
      g_object_set (widget, "active", state, NULL);   /* set the value */
      g_signal_handler_unblock (widget, handler);     /* re-enable signal */
    }
    else { /* No handler ID so just set without blocking */
      g_object_set (widget, "active", state, NULL);   /* set the value */
    }
  }
}

/*!
 * \brief Handles changes of the Style settings.
 * \par Function Description
 *  This is call-back function for the style menu toggle widgets. This
 *  functions establishes the flag mask that will be used when loading
 *  the tree-views, compselect->style_flag. In other functions through
 *  out this module this same variable is aliased as #MASK (primarily to
 *  reduce line length).
 *
 *  Since this function is only used as a call-back for toggle widgets the
 *  mask must have changed. Therefore we emits a <B>refresh</B> signal for
 *  the  tree-views once the mask has been updated.
 *
 * \param [in] button We must find this button in our GSList of widgets.
 * \param [in] compselect  The component selection dialog.
 */
static void
compselect_toggle_style(GedaCheckMenuItem *button, Compselect *compselect)
{
  GSList *iter;
  int state;
  int index;

  /* Get the current state of the toggle button that called us */
  g_object_get (G_OBJECT (button), "active", &state, NULL);

  /* Determine which button was toggled */
  index = g_slist_index(compselect->style_menu_widgets, button);

  if (index == COMPSELECT_STYLE_NONE && state) {

    /* The "None" box was checked so un-check all the check boxes, except
     * NONE, the second assignment of iter causes the first element to be
     * skipped, which is "None", which was activated by the user */
    for (iter = compselect->style_menu_widgets, iter = g_slist_next (iter);
         iter != NULL; iter = g_slist_next (iter)) {
      compselect_set_item_active(iter->data, FALSE);
    }
    compselect->style_flag = COMPSELECT_STYLE_NONE;  /* Be Zero */
  }
  else {

    if (index == 9 && state) { /* The "All" box was pressed check */

      /* Un-check the check "none" box */
      compselect_set_item_active(compselect->style_menu_widgets->data, FALSE);
      /* Check all the other boxes except NONE*/
      for (iter = compselect->style_menu_widgets,
        iter = g_slist_next (iter); iter != NULL; iter = g_slist_next (iter)) {
        compselect_set_item_active(iter->data, TRUE);
      }
      compselect->style_flag = COMPSELECT_STYLE_ALL; /* Be 255 */
    }
    else {

       GtkWidget *all_butt;
       unsigned int mask = 1;

       /* decrement index because 0 was resolved above */
       --index;

       if (state) {
         mask = mask << index; /* bit shift left */
         compselect->style_flag = compselect->style_flag + mask;
       }
       else {
         mask = mask << index; /* bit shift right */
         compselect->style_flag = compselect->style_flag - mask;
       }

       /* Set the state of the All check box */
       state = (compselect->style_flag == COMPSELECT_STYLE_ALL);
       all_butt = g_slist_nth_data(compselect->style_menu_widgets, 9);
       compselect_set_item_active(all_butt, state);

       /* Set the state of the None check box */
       state = (compselect->style_flag == COMPSELECT_STYLE_NONE);
       compselect_set_item_active(compselect->style_menu_widgets->data, (state));
    }
  }

  g_signal_emit (ThisDialog, signals[REFRESH], 0);
}

/*!
 * \brief Callback for Style toggler on the popup menu.
 * \par Function Description
 *  This is call-back function for the style toggle widgets on the right
 *  button mouse menu. The function retrieves the index from the widget
 *  and then get the address of the equivalent "main" style toggle widget.
 *  The "main" style toggle widget is set unblocked so the preceding
 *  call-back handler is takes care of establishing the new mask and
 *  signaling for a refresh.
 *
 * \param [in] button  A menu item toggle button widget on the popup submenu.
 * \param [in] compselect  The component selection dialog.
 */
static void
compselect_popup_toggle_style(GedaCheckMenuItem *button, Compselect *compselect)
{
  int  i;
  GedaCheckMenuItem *real_butt;

  i = (int)(long)GEDA_OBJECT_GET_DATA(button, "index");
  real_butt = g_slist_nth_data(compselect->style_menu_widgets, i);

  geda_check_menu_item_set_active(real_butt, geda_check_menu_item_get_active(button));

}

/*!
 * \brief Handles changes of behavior.
 * \par Function Description
 *  This function is called every time the value of the option menu
 *  for behaviors is modified.
 *
 *  It emits the dialog's <B>apply</B> signal to let the parent know
 *  that the requested behavior for the next adding of a component has
 *  been changed.
 *
 * \param [in] optionmenu The behavior option menu.
 * \param [in] user_data  The component selection dialog.
 */
static void
compselect_callback_behavior_changed (GedaOptionMenu *optionmenu,
                                      void          *user_data)
{
  Compselect *compselect = (Compselect*)user_data;
  GtkWidget *menuitem;
  int menu_choice;

  menuitem = geda_menu_widget_get_active(geda_option_menu_get_menu(optionmenu));

  menu_choice = (int)(long)GEDA_OBJECT_GET_DATA(menuitem, "behaviors");

  switch(menu_choice) {
    case COMPSELECT_BEHAVIOR_REFERENCE:
      g_object_set (optionmenu, "tooltip-text",
                    _("Default is to reference the component"), NULL);
      break;
    case COMPSELECT_BEHAVIOR_EMBED:
      g_object_set (optionmenu, "tooltip-text",
                    _("Embed component in schematic"), NULL);
      break;
    case COMPSELECT_BEHAVIOR_INCLUDE:
      g_object_set (optionmenu, "tooltip-text",
                    _("Include component as individual objects"), NULL);
  }

  g_signal_emit_by_name (compselect,
                         "response",
                         COMPSELECT_RESPONSE_PLACE,
                         NULL);
}

/*!\brief Create the tree model for the "In Use" view.
 * \par Function Description
 * Creates a straightforward list of symbols which are currently in
 * use, using geda_toplevel_struct_get_symbols().
 */
static GtkTreeModel*
compselect_create_inuse_tree_model (Compselect *compselect)
{
  GschemToplevel *w_current;
  GtkListStore   *store;
  GList          *symlist;
  GList          *iter;
  GtkTreeIter     tree;

  w_current = GSCHEM_DIALOG (compselect)->w_current;

  store = (GtkListStore *) gtk_list_store_new (1, G_TYPE_POINTER);

  symlist = geda_toplevel_struct_get_symbols (w_current->toplevel);

  for (iter = symlist; iter != NULL; iter = g_list_next (iter)) {

    gtk_list_store_append (store, &tree);

    gtk_list_store_set (store, &tree, 0, iter->data, -1);
  }
  g_list_free (symlist);

  return (GtkTreeModel*)store;
}

/*!
 * \brief Get Library Source List for Model for use in Library treeviews.
 * \par Function Description
 *  This function filters the list of available sources for each ViewTree.
 *  The function returns a glist of sources in the category associated
 *  with the supplied data set enumerator. This has nothing to do with
 *  the filter entry. The data is based on the Data and is used for all
 *  library views but not the IN-USE Tab.
 *
 * \param [in] w_current  GschemToplevel environment data.
 * \param [in] data_set   Enumerator associated with the TAB for the TreeView.
 *
 * \returns [out] Ptr to Glist of selected sources matching the category.
 */
GList *
compselect_get_tree_sources(GschemToplevel *w_current, DialogTabs data_set)
{
  GList *all_sources;
  GList *selected_sources;
  GList *src_iter;

  /* Get list of all sources */
  all_sources = geda_struct_clib_get_sources (w_current->sort_component_library != 0);

  /* Initialize a list to receive the sources for this group  */
  selected_sources = NULL;

  /* Loop through all sources and look for source for this group */
  for (src_iter = all_sources; src_iter; src_iter = src_iter->next) {

    CLibSource *source = src_iter->data; /* Retrieve a single source */

    /* Check if this source belongs in this group, i.e view TAB */
    if (source->category != NULL &&
       (strcmp(source->category, IDS_CATEGORIES[data_set]) == 0)) {

      /* Then add this to the list*/
      selected_sources = g_list_append (selected_sources, source);
    }
  }

  g_list_free (all_sources);

  return selected_sources;
}

/*!
 * \brief Check symbol name for style inclusion
 * \par Function Description
 *  Compares integer value after dash in the symbol name to see
 *  if the symbol should be included based on the style flag mask.
 */
static bool
compselect_filter_check_style(Compselect *compselect, const char *sym_name)
{
  const    char *ptr;
  unsigned char  code[2];

  bool result;

  result  = TRUE;
  code[1] = ASCII_NUL;
  ptr     = sym_name;                 /* set pointer to address of 1st char */

  while (*ptr != ASCII_NUL) ptr++;    /* increment to the end of string */

  ptr = ptr - 6;                      /* decrement to where dash should be */

  if (*ptr == ASCII_HYPHEN_MINUS) {   /* and if it's a dash then */

    int flag;

    ++ptr;                            /* increment to the number */
    code[0] = *ptr;                   /* retrieve the char value */
    flag    = code[0] & 0x0F;         /* convert askii to number */

    if (flag > 0  && flag < 9) {      /* only consider 1 to 8    */

      unsigned int mask = 1;

      mask = mask << (flag - 1);                /* raise by power of 2 */
      if (!(compselect->style_flag & mask)) {   /* and compare */
        result = FALSE;
      }
    }
  }
  return result;
}

/*!
 * \brief Create the Tree Model for "Library" views.
 * \par Function Description
 *  Creates a TreeStore with 3 columns, the first column is a boolean
 *  value indicating whether the second column contains data for a
 *  symbol or a source. If the first column contains TRUE then the
 *  row is for a symbol entry, if FALSE then the data in the second
 *  column is a pointer to a CLibSource record.
 *
 *  Data supplied by compselect_get_tree_sources is added to a new
 *  TreeStore. The data is monitored looking for group changes,
 *  indicating the different symbol sub-directory.
 *
 *  A TreeModel containing the TreeStore is return after connecting
 *  associating the model with the visibility filter.
 *
 *  In theory, we could nest any number of levels using this technique
 *  but this implementation only looks for 1 slash.
 */
static GtkTreeModel *
compselect_create_lib_tree_model (Compselect *compselect, DialogTabs data_set)
{
  GschemToplevel  *w_current;
  GtkTreeModel    *model;
  GtkTreeStore    *store;
  GtkTreeIter      parent;
  GtkTreeIter      tree_iter, tree_iter2;

  GList *group_names;
  GList *sources, *src_iter;
  GList *symlist, *sym_iter;
  CLibSource      *source;

  bool   bypassing;
  bool   at_boundary;

  char  *tooltip_text;
  char  *previous_grp;
  int    sym_count;

  w_current = GSCHEM_DIALOG (compselect)->w_current;

  store = (GtkTreeStore*)gtk_tree_store_new (NUM_LV_COLUMNS,
                                             G_TYPE_BOOLEAN,
                                             G_TYPE_POINTER,
                                             G_TYPE_STRING);

  /* populate component store */
  sources = compselect_get_tree_sources (w_current, data_set);

  group_names = w_current->toplevel->component_groups;

    /* Set flag for "special" folders */
  bypassing   = (data_set == LOCAL_TAB) || (data_set == SIM_TAB);

  previous_grp = "";

  for (src_iter = sources; src_iter != NULL; NEXT (src_iter)) {

    source       = src_iter->data;
    symlist      = source->symbols;
    sym_count    = g_list_length (symlist);
    at_boundary  = g_ascii_strcasecmp (previous_grp, source->group) == 0;

    if ((compselect->show_groups == FALSE) &&
        (geda_utility_glist_find_string(group_names, source->group) > -1))
    {
      bypassing  = TRUE;
    }

    if (!bypassing) {

      /* Might eliminate 1 of these, they alway seemed to occur in pairs */
      if (at_boundary) {

        /* At the start of a new group, either add it */
        if (compselect->subgroups == TRUE) {
          gtk_tree_store_append (store, &tree_iter, &parent);
        }
        else { /* or load symbols directly under parent */
          goto load_symbols;
        }
      }
      else {
        gtk_tree_store_append (store, &tree_iter, NULL);
        geda_tree_copy_iter(&tree_iter, &parent);
      }
    }
    else { /* Not Nesting a Group */

      if (sym_count > 0) {
        gtk_tree_store_append (store, &tree_iter, NULL);
      }
      else { /* is empty "special" folder with no files so */
        continue;
      }
    }

    /* Save a ptr to the group name for the next iteration */
    previous_grp = source->group;

    if (sym_count > 1) {
      /* Add tool tip to the source row */
      tooltip_text = geda_sprintf("%s contains %d symbols", source->name, sym_count);
    }
    else {
      tooltip_text = geda_sprintf("%s Group", source->name);
    }

    gtk_tree_store_set (store, &tree_iter,
                        LVC_ROW_TYPE, FALSE,
                        LVC_ROW_DATA, source,
                        LVC_TOOLTIP_TEXT, tooltip_text, -1);

    GEDA_FREE(tooltip_text);

load_symbols: /* It Works! */

    for (sym_iter = symlist; sym_iter != NULL;
         sym_iter = g_list_next (sym_iter)) {

      const char *sym_name = geda_struct_clib_symbol_get_name(sym_iter->data);

      /* if a directory only has one symbol file and the file name is
       * "placeholder.sym" then we don't display the symbol, the file
       * is being used to over-ride the switches and force a group to
       * always be on the treeview list */
      if (sym_count == 1) {
        if (geda_strncmpi (sym_name, "placeholder.sym", 15) == 0)
          continue;
      }

      if (MASK != COMPSELECT_STYLE_ALL) {
        if (!compselect_filter_check_style(compselect, sym_name))
          continue;
      }

      gtk_tree_store_append (store, &tree_iter2, &tree_iter);
      gtk_tree_store_set (store, &tree_iter2,
                          LVC_ROW_TYPE, TRUE,
                          LVC_ROW_DATA, sym_iter->data, -1);
    }
  }

  g_list_free (sources);

  /* rather than returning a list store to each caller we will just create
   * a new model and add the Store since this is what the caller was going
   * to do anyways, then we can setup the filter func for the caller */
  model = g_object_new (GTK_TYPE_TREE_MODEL_FILTER,
                        "child-model", store,
                        "virtual-root", NULL, NULL);

  gtk_tree_model_filter_set_visible_func ((GtkTreeModelFilter*)model,
                                          lib_model_filter_visible_func,
                                          compselect,
                                          NULL);
  GEDA_UNREF (store);

  return model;
}

/*!
 * \brief Handles a click on the clear button.
 * \par Function Description
 *  This is the callback function called every time the user press the
 *  clear filter button. This function resets the filter entry, which
 *  indirectly causes re-evaluation of the filter on the list of symbols
 *  (because the entry "changed" is gets triggered), and this updates
 *  the active display view. To insure all of the views get updated, each
 *  library is manually updated.
 *
 * \param [in] button    The clear button
 * \param [in] user_data The component selection dialog.
 */
static void
compselect_callback_clear_filter_clicked (GtkButton *button,
                                          void      *user_data)
{
  Compselect *Dialog = COMPSELECT (user_data);

  /* clears text in text entry for filter */
  SetEntryText (Dialog->entry_filter, "");
  Dialog->applying_filter = FALSE;

  compselect_apply_filter_tree_view (Dialog, Dialog->stdtreeview);
  compselect_apply_filter_tree_view (Dialog, Dialog->mantreeview);
  compselect_apply_filter_tree_view (Dialog, Dialog->simtreeview);
  compselect_apply_filter_tree_view (Dialog, Dialog->localtreeview);

}

/*!
 * \brief Refresh all Library Tree-Views Signal Handler.
 * \par Function Description
 *  This functions is called by compselect_callback_refresh_views
 *  to rebuild all trees view models. Currently, this functions
 *  is not called directly because the cursor position is not
 *  maintained. The function cause models in all tree-views to
 *  be rebuilt.
 */
static void
compselect_refresh_tree_views (Compselect *compselect)
{
  GtkTreeModel *model;

  /* inline function to update models in trees */
  inline
  void set_tree_view_model(GtkTreeView  *tree_view, DialogTabs data_set) {

    GtkTreeSelection *selection;

    GEDA_UNREF (gtk_tree_view_get_model (tree_view));

    model = compselect_create_lib_tree_model (compselect, data_set);

    /* Block handling selection updated for duration of model changes */
    selection = gtk_tree_view_get_selection (tree_view);
    g_signal_handlers_block_by_func (selection,
                                     compselect_cb_tree_selection_changed,
                                     compselect);

    /* Update the view model with signals blocked */
    gtk_tree_view_set_model (tree_view, model);

    /* Unblock & fire handler for stdtreeview selection */
    g_signal_handlers_unblock_by_func (selection,
                                       compselect_cb_tree_selection_changed,
                                       compselect);
  }

  /* Refresh the Standard Library TreeView */
  set_tree_view_model(compselect->stdtreeview, STD_TAB);

  /* Refresh the Manufactures Library TreeView */
  set_tree_view_model(compselect->mantreeview, MAN_TAB);

  /* Refresh the Local Library TreeView */
  set_tree_view_model(compselect->localtreeview, LOCAL_TAB);

  /* Refresh the Simulation Library TreeView */
  set_tree_view_model(compselect->simtreeview, SIM_TAB);

  /* Refresh the "In Use" view */
  GEDA_UNREF (gtk_tree_view_get_model (compselect->inusetreeview));
  model = compselect_create_inuse_tree_model (compselect);

  /* Here we can update the model without blocking signals
   * as this is the (final) tree view we are updating */
  gtk_tree_view_set_model (compselect->inusetreeview, model);

}

/*!
 * \brief On-demand Refresh all Library Tree-Views.
 * \par Function Description
 * The function checks for a selection in the active tree-view and if
 * an item, either a container or a symbol, is selected then associated
 * names are saved. compselect_refresh_tree_views is called to rebuild all
 * tree models. If a selection had previosuly existed, an attempt is made
 * to locate the container and or symbol and restore the cursor and scroll
 * view to the same item.
 *
 * This function servers as a "front-end" to compselect_refresh_tree_views,
 * which is called in the middle section between "save" and "restore".
 * This function is also used by signal handlers, call-back functions, and
 * the "on-demand" button to rebuild all treeview models.
 *
 */
static void
compselect_callback_refresh_views (GtkWidget *widget, void *user_data)
{
  Compselect *compselect = COMPSELECT (user_data);

  GtkTreeView *tree_view;
  GtkTreeModel  *model;
  GtkTreeIter     iter;
  GtkTreeIter     parent;
  GtkTreePath      *path;
  GtkTreeSelection *selection;

  bool   at_boundary;
  bool   do_restore;
  bool   was_expanded = FALSE;

  CLibSource  *source;
  CLibSymbol  *symbol;

  char        *gp_src_name = NULL;
  char        *src_name    = NULL;
  char        *sym_name    = NULL;

  tree_view = get_active_tree_view (compselect);
  selection = gtk_tree_view_get_selection(tree_view);

  /* Check if something is selected and if so then save the name of
   * the containing folder and the symbol if there is one */
  if (gtk_tree_selection_get_selected(selection, &model, &iter)) {

    if (is_symbol_record(model, &iter)) {

      symbol = NULL;

      gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &symbol, -1);

      const char *ptr_sym_name = geda_struct_clib_symbol_get_name(symbol);

      sym_name = geda_utility_string_strdup(ptr_sym_name);

      gtk_tree_model_iter_parent (model, &parent, &iter);
      was_expanded = TRUE;
      at_boundary  = FALSE;

    }
    else {
      path         = gtk_tree_model_get_path (model, &iter);
      was_expanded = gtk_tree_view_row_expanded (tree_view, path);
      gtk_tree_path_free(path);
      geda_tree_copy_iter(&iter, &parent);
      at_boundary  = TRUE;
    }

    source = NULL;

    gtk_tree_model_get (model, &parent, LVC_ROW_DATA, &source, -1);

    const char *ptr_src_name = geda_struct_clib_source_get_name(source);

    src_name = geda_utility_string_strdup(ptr_src_name);

    if (gtk_tree_model_iter_parent (model, &iter, &parent)) {
      source = NULL;
      gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &source, -1);
      ptr_src_name = geda_struct_clib_source_get_name(source);
      gp_src_name = geda_utility_string_strdup(ptr_src_name);
    }

    do_restore = TRUE;
  }
  else {
    at_boundary = FALSE;
    do_restore = FALSE;
  }

  /* Check of flag set to rescan the libraries for symbols */
  if (compselect->rescan_lib) {
    geda_struct_clib_refresh ();
    compselect->rescan_lib = FALSE;
  }

  /* Rebuild the Tree Model */
  compselect_refresh_tree_views (compselect);

  if (do_restore) {

    bool valid;
    bool found = FALSE;

    model = gtk_tree_view_get_model(tree_view);

    /* Get the first iter in the list */
    valid = gtk_tree_model_get_iter_first (model, &iter);

    if (gp_src_name) { /* 1st Look for Grand Parent */

      while (valid) {

        if (!is_symbol_record(model, &iter)) {

          source = NULL;

          gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &source, -1);

          const char *ptr_src_name = geda_struct_clib_source_get_name(source);

          if (ptr_src_name && (strcmp(ptr_src_name, gp_src_name) == 0)) {

            /* found grand parent */
            path = gtk_tree_model_get_path (model, &iter);

            gtk_tree_view_expand_row (tree_view, path, FALSE);
            gtk_tree_view_set_cursor (tree_view, path, NULL, FALSE);

            /* Set iter to 1st child */
            gtk_tree_path_down(path);
            gtk_tree_model_get_iter(model, &iter, path);
            gtk_tree_path_free(path);
            break;
          }
        }
        valid = gtk_tree_model_iter_next (model, &iter) && iter.stamp;
      }
      GEDA_FREE (gp_src_name);
    }

    valid = (iter.stamp != 0 ? TRUE : FALSE);

    if (!valid) valid = gtk_tree_model_get_iter_first (model, &iter);

    while (valid) {                               /* Look for Parent */

      if (!is_symbol_record(model, &iter)) {

        source = NULL;

        gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &source, -1);

        const char *ptr_src_name = geda_struct_clib_source_get_name(source);

        if (ptr_src_name && (strcmp(ptr_src_name, src_name) == 0)) {

          /* found the folder so check if was expanded and set cursor */
          path = gtk_tree_model_get_path (model, &iter);

          if (was_expanded) { /* expand folder is previously expanded */
            gtk_tree_view_expand_to_path(tree_view, path);
          }

          gtk_tree_view_set_cursor (tree_view, path, NULL, FALSE);

          /* Set iter to 1st child for next section */
          gtk_tree_path_down(path);
          gtk_tree_model_get_iter(model, &iter, path);
          gtk_tree_path_free(path);
          break;
        }
      }
      valid = gtk_tree_model_iter_next (model, &iter) && iter.stamp;
    }

    /* We could continue looking even if the current style is set to
     * none but we would have to check the symbol name first and make
     * sure it did not have a style, aka a dash number */
    if (!at_boundary && compselect->style_flag) {

      bool parent_is_valid;

      if (valid) {
        /* Save iter in case we do not find symbol on 1st pass */
        parent_is_valid = gtk_tree_model_iter_parent(model, &parent, &iter);
      }
      else {
        parent_is_valid = FALSE;
        valid = gtk_tree_model_get_iter_first (model, &iter);
      }

      /* Look for Symbol - 1st pass */
      while (valid) {

        if (is_symbol_record(model, &iter)) {

          gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &symbol, -1);

          const char *ptr_sym_name = geda_struct_clib_symbol_get_name(symbol);

          if (ptr_sym_name && (strcmp(ptr_sym_name, sym_name) == 0)) {
            path = gtk_tree_model_get_path (model, &iter);
            gtk_tree_view_expand_to_path(tree_view, path);
            gtk_tree_view_set_cursor (tree_view, path, NULL, FALSE);
            gtk_tree_path_free(path);
            found = TRUE;
            break;
          }
        }
        valid = gtk_tree_model_iter_next (model, &iter) && iter.stamp;
      }

      if (!found && parent_is_valid) {

        /* remove "-n.sym" from what we are looking for */
        char *short_name = geda_strndup(sym_name, strlen(sym_name) - 6);

        /* Look for Symbol - 2nd pass, set iter to 1st child of saved parent */
        valid = gtk_tree_model_iter_children(model, &iter, &parent);

        while (valid) {

          if (is_symbol_record(model, &iter)) {

            gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &symbol, -1);

            const char *ptr_sym_name = geda_struct_clib_symbol_get_name(symbol);

            if (strstr(ptr_sym_name, short_name) == 0) {
              path = gtk_tree_model_get_path (model, &iter);
              gtk_tree_view_expand_to_path(tree_view, path);
              gtk_tree_view_set_cursor (tree_view, path, NULL, FALSE);
              gtk_tree_path_free(path);
              break;
            }
          }
          valid = gtk_tree_model_iter_next (model, &iter) && iter.stamp;
        }
        GEDA_FREE (short_name);
      }
    }

    if (valid) {
      geda_tree_view_row_make_visible (tree_view, &iter, TRUE);
    }
  } /* End if do_restore */
  GEDA_FREE (src_name);
  GEDA_FREE (sym_name);
}

/*!
 * \brief Refresh all Library Tree-Views Signal Handler.
 * \par Function Description
 *  This function is activate when the "refresh" signal is emitted
 *  on the dialog from any internal widgets. The signal is emitted when
 *  a style is changed.
 */
static void
compselect_on_refresh_tree_views (Compselect *compselect)
{
  compselect_callback_refresh_views (NULL, compselect);
}

/*!
 * \brief Refresh the Active Tree View.
 * \par Function Description
 *  This function causes the model for the active tree-view to be
 *  rebuilt.
 */
static void
compselect_refresh_inuse_view (GtkWidget *widget, void *user_data)
{
  Compselect *compselect = COMPSELECT (user_data);

  GtkTreeView  *tree_view;
  GtkTreeModel *model;

  tree_view = compselect->inusetreeview;
  GEDA_UNREF (gtk_tree_view_get_model (tree_view));
  model = compselect_create_inuse_tree_model (compselect);
  gtk_tree_view_set_model (compselect->inusetreeview, model);

}

/*!
 * \brief Rescan component libraries and update treeViews.
 * \par Function Description
 *  Requests a rescan of the component library in order to pick up any
 *  new symbols, and calls compselect_refresh_tree_views to refresh
 *  the tree views.
 */
static void
compselect_callback_rescan_libraries (GtkButton *button, void *user_data)
{
  Compselect  *compselect = COMPSELECT (user_data);

  gtk_widget_set_sensitive (compselect->filter_hbox, FALSE);

  /* Set flag to rescan the libraries for symbols */
  compselect->rescan_lib = TRUE;
  compselect_callback_refresh_views (NULL, user_data);

  gtk_widget_set_sensitive (compselect->filter_hbox, TRUE);
}

/*!
 * \brief Collase All button callback.
 * \par Function Description
 *  Called when user presses the Collapse All button to close all opened
 *  rows in the active view-tree.
 *
 *  TODO: The standard call back does not have the parameter in the order
 *  need to call the gtk_tree_view_collapse_all function directly. Isn't
 *  there an optional callback protocol?
 */
static void
compselect_callback_collapse_all(GtkButton *button, void *user_data)
{
  Compselect  *compselect = COMPSELECT (user_data);
  GtkTreeView *view       = get_active_tree_view (compselect);

  if (GTK_IS_TREE_VIEW(view))
    gtk_tree_view_collapse_all (view);
  else
    BUG_MSG("bad TreeView");
}

/*!\brief Expand All button callback.
 * \par Function Description
 * Called when user presses the Expand All button to open all parent
 * rows in the active view-tree.
 *
 * TODO: The standard call back does not have the parameters in the order
 * needed to call the gtk_tree_view_collapse_all function directly. Isn't
 * there an optional callback protocol?
 */
static void
compselect_callback_expand_all(GtkButton *button, void *user_data)
{
  Compselect  *compselect = COMPSELECT (user_data);
  GtkTreeView *view       = get_active_tree_view (compselect);

  if (GTK_IS_TREE_VIEW(view))
    gtk_tree_view_expand_all (view);
  else
    BUG_MSG("bad TreeView");
}

/*!
 * \brief Emit GEDA_RESPONSE_CLOSE signal.
 * \par Function Description
 *  This function is called when the Close button on the Component
 *  Select dialog is pressed.
 */
static void
compselect_on_close_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data),
                           "response",
                           GEDA_RESPONSE_CLOSE,
                           user_data);
}

/*!
 * \brief Emit COMPSELECT_RESPONSE_HIDE signal.
 * \par Function Description
 *  This function is called when the Okay button on the Component
 *  Select dialog is pressed.
 */
static void
compselect_on_okay_butt_clicked(GtkButton *button, void *user_data)
{
    g_signal_emit_by_name (GTK_DIALOG (user_data),
                           "response",
                           COMPSELECT_RESPONSE_HIDE,
                           user_data);
}

/*!
 * \brief Callback on Component Select Dialog change TAB in notebook.
 * \par Function Description
 *  This function is called when ever a TAB sheet is selected in order
 *  to set the page number for use by other functions in this module and
 *  then calls compselect_apply_filter_tree_view to update the active
 *  treeview widget.
 */
static void
compselect_on_notebook_switch_page (GtkNotebook     *notebook,
                                    GtkNotebookPage *page,
                                    unsigned int     page_num,
                                    Compselect      *dialog)
{
  dialog->active_tab = page_num;

  if (dialog->applying_filter) {
    compselect_apply_filter_tree_view (dialog, get_active_tree_view(dialog));
  }

  if (page_num == IN_USE_TAB) {
    compselect_refresh_inuse_view (NULL, dialog);
  }

  return;
}

/*!
 * \brief Creates the treeview for the "In Use" view.
 * \par Function Description
 *  This function create a new Treeview Widget Assembly. The assembly
 *  contains a scrollable Treeview nested inside a vertical box object.
 *
 * \returns vBox widget Assembly
 */
static GtkWidget *
compselect_create_inuse_treeview (Compselect *compselect)
{
  GtkWidget         *scrolled_win, *treeview, *hbox, *button;
  GtkTreeModel      *model;
  GtkTreeSelection  *selection;
  GtkCellRenderer   *renderer;
  GtkTreeViewColumn *column;

  model = compselect_create_inuse_tree_model (compselect);

  /* vertical box for component selection and search entry */
  GTK_NEW_vBOX(inuse, FALSE, DEFAULT_DIALOG_SPACING);
  geda_set_container_border_width(inuse_vbox, DIALOG_BORDER_WIDTH);

  /* Create a scrolled window to accomodate the treeview */
  scrolled_win = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                               /* GtkContainer */
                               "border-width", DIALOG_BORDER_WIDTH,
                               /* GtkScrolledWindow */
                               "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                               "vscrollbar-policy", GTK_POLICY_ALWAYS,
                               "shadow-type",       GTK_SHADOW_ETCHED_IN,
                               NULL);

  /* Create the treeview */
  treeview = g_object_new (GTK_TYPE_TREE_VIEW,
                           /* GtkTreeView */
                           "model",      model,
                           "rules-hint", TRUE,
                           "headers-visible", FALSE,
                           NULL);

  /* Connect callback to selection */
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (compselect_cb_tree_selection_changed),
                    compselect);

  /* Insert a column for symbol name */
  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TEXT, /* GtkCellRendererText */
                           "editable", FALSE,
                           NULL);

  column =  g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,  /* GtkTreeViewColumn */
                          "title", _("Components"),
                          NULL);

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           inuse_treeview_set_cell_data,
                                           NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* Add the treeview to the scrolled window */
  geda_container_add (scrolled_win, treeview);

  /* set the inuse treeview of compselect */
  compselect->inusetreeview = GTK_TREE_VIEW (treeview);

  /* Add the scrolled window for directories to the vertical box */
  PACK_START (inuse_vbox, scrolled_win, TRUE, TRUE, 0);

  /* -- refresh button area -- */
  hbox = g_object_new (GTK_TYPE_HBOX,
                       /* GtkBox */
                       "homogeneous", FALSE,
                       "spacing",     3,
                       NULL);

  /* create the refresh button */
  button = g_object_new (GTK_TYPE_BUTTON,
                         /* GtkWidget */
                         "sensitive", TRUE,
                         /* GtkButton */
                         "relief",    GTK_RELIEF_NONE,
                         NULL);

  geda_container_add (button,
                      gtk_image_new_from_stock (GTK_STOCK_REFRESH,
                                                GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the refresh button to the horizontal box at the end */
  gtk_box_pack_end (GTK_BOX (hbox), button, FALSE, FALSE, 0);

  g_signal_connect (button,
                    "clicked",
                    G_CALLBACK (compselect_refresh_inuse_view),
                    compselect);

  /* Add the refresh button area to the vertical box */
  PACK_START(inuse_vbox, hbox, FALSE, FALSE, 0);

  return inuse_vbox;
}

/*!
 * \brief Tool Tips On Callback Handler for Popup Mouse Context Menu
 * \par Function Description
 *  This function sets the LVC_TOOLTIP_TEXT column to be the tooltip
 *  string for each library treeview and is used to enable tooltips
 *  in the tree views.
 *
 * \param [in] menu_item is menu widget
 * \param [in] compselect Pointer Compselect dialog structure
 */
static void
compselect_menu_tooltips_on(GedaMenuItem *menu_item, Compselect *compselect)
{
  gtk_tree_view_set_tooltip_column (compselect->stdtreeview,   LVC_TOOLTIP_TEXT);
  gtk_tree_view_set_tooltip_column (compselect->mantreeview,   LVC_TOOLTIP_TEXT);
  gtk_tree_view_set_tooltip_column (compselect->localtreeview, LVC_TOOLTIP_TEXT);
  gtk_tree_view_set_tooltip_column (compselect->simtreeview,   LVC_TOOLTIP_TEXT);
}

/*!
 * \brief Tool Tips Off Callback Handler for Popup Mouse Context Menu
 * \par Function Description
 *  This function sets the tooltip column to -1 for each library treeview
 *  and is used to disable tooltips in the tree views if the use so chooses.
 *
 * \param [in] menu_item is menu widget
 * \param [in] compselect Pointer Compselect dialog structure
 */
static void
compselect_menu_tooltips_off(GedaMenuItem *menu_item, Compselect *compselect)
{
  gtk_tree_view_set_tooltip_column (compselect->stdtreeview,   -1);
  gtk_tree_view_set_tooltip_column (compselect->mantreeview,   -1);
  gtk_tree_view_set_tooltip_column (compselect->localtreeview, -1);
  gtk_tree_view_set_tooltip_column (compselect->simtreeview,   -1);
}

/*!
 * \brief Create and Setup Popup Mouse Menu for the View Trees
 * \par Function Description
 *  This function constructs a pop-up menu for the Library Tree Views
 *  settting sensitivity on menu choices based on Tree-View position
 *  and the state of the containing Tree-View.
 *
 * \param [in] compselect Pointer Compselect dialog structure
 * \param [in] treeview   widget
 *
 * TODO: Should not have recreate the popup menu everytime the user
 *       right-clicks.
 */
static GtkWidget*
compselect_build_view_menu(Compselect *compselect, GtkWidget *treeview)
{
  GtkWidget *menu;
  GtkWidget *submenu;
  GtkWidget *menuitem;

  bool is_symbol;

  GtkTreeIter iter;
  GtkTreeModel *model;
  GtkTreeSelection *selection;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
  if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
    is_symbol = is_symbol_record(model, &iter);
  }
  else {
    is_symbol = FALSE;
  }

  menu = geda_menu_new();

  menuitem = geda_menu_item_new_with_label(_(popup_items[ExpandFolder]));
  if (!is_symbol) {
    gtk_widget_set_tooltip_text (menuitem, _(popup_tips[ExpandFolder]));
    gtk_widget_set_sensitive(menuitem, TRUE);
    gtk_widget_set_can_focus(menuitem, TRUE);
    g_signal_connect(menuitem, "activate",
                    (void*)compselect_open_tree_row,
                    (void*)treeview);
  }
  else {
    gtk_widget_set_sensitive(menuitem, FALSE);
    gtk_widget_set_can_focus(menuitem, FALSE);
  }
  geda_menu_append (menu, menuitem);

  menuitem = geda_menu_item_new_with_label(_(popup_items[ExpandAll]));
  if (!is_symbol) {
    gtk_widget_set_tooltip_text (menuitem, _(popup_tips[ExpandAll]));
    gtk_widget_set_sensitive(menuitem, TRUE);
    gtk_widget_set_can_focus(menuitem, TRUE);
    g_signal_connect(menuitem,"activate",
                    (void*)compselect_open_tree_rows,
                    (void*)treeview);
  }
  else {
    gtk_widget_set_sensitive(menuitem, FALSE);
    gtk_widget_set_can_focus(menuitem, FALSE);
  }
  geda_menu_append (menu, menuitem);

  /* The Close Menu Option */
  menuitem = geda_menu_item_new_with_label(_(popup_items[CloseFolder]));
  gtk_widget_set_tooltip_text (menuitem, _(popup_tips[CloseFolder]));
  gtk_widget_set_sensitive(menuitem, TRUE);
  gtk_widget_set_can_focus(menuitem, TRUE);
  g_signal_connect(menuitem,"activate",
                  (void*) compselect_close_tree_row,
                  (void*) treeview);
  geda_menu_append (menu, menuitem);

  /* The Refresh Menu Option */
  menuitem = geda_menu_item_new_with_label(_(popup_items[RefreshView]));
  gtk_widget_set_tooltip_text (menuitem, _(popup_tips[RefreshView]));
  g_signal_connect(menuitem,"activate",
                   G_CALLBACK (compselect_callback_refresh_views),
                   compselect);

  geda_menu_append (menu, menuitem);

  /* The Rescan Menu Option */
  menuitem = geda_menu_item_new_with_label(_(popup_items[RescanLibs]));
  gtk_widget_set_tooltip_text (menuitem, _(popup_tips[RescanLibs]));
  g_signal_connect(menuitem,"activate",
                   G_CALLBACK (compselect_callback_rescan_libraries),
                   compselect);

  geda_menu_append (menu, menuitem);

  /* The Styles submenu */

  GSList *iter2;             /* Use to index the "main" styles menu */
  int     index;             /* Use by callback to index "main" widget */

  /* Note: the handler is not used for the popup menu widgets */
  menuitem = geda_menu_item_new_with_label(_(popup_items[StylesMenu]));

  submenu = geda_menu_new ();
  geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menuitem), submenu);
  geda_menu_append (menu, menuitem);

  index = 0;

  for (iter2 = compselect->style_menu_widgets;
       iter2 != NULL;
       iter2 = g_slist_next (iter2)) {

    GedaCheckMenuItem *check_item;  /* Ptr to "main" style widget */

    bool           active;   /* state of the "main" style widget */
    unsigned long  handler;  /* Signal handler of this submenu item */
    const char    *label;    /* Ptr to label of the "main" style widget */

    check_item = iter2->data;
    label      = geda_menu_item_get_label (GEDA_MENU_ITEM(check_item));
    active     = geda_check_menu_item_get_active (check_item);
    menuitem   = geda_check_menu_item_new_with_mnemonic (label);

    geda_check_menu_item_set_state (menuitem, active);
    geda_check_menu_item_set_as_radio (menuitem, FALSE);
    geda_menu_append (menu, menuitem);

    handler = GTK_CALLBACK_TOGGLED (menuitem,
                                    compselect_popup_toggle_style,
                                    compselect);

    GEDA_OBJECT_SET_DATA(menuitem, (void*)handler, "handler");
    GEDA_OBJECT_SET_DATA(menuitem, (void*)(long)index, "index");
    index++;
  }

  /* tooltip submenu */
  menuitem = geda_menu_item_new_with_mnemonic (_(popup_items[ShowToolTips]));

  geda_menu_append (menu, menuitem);

  submenu = geda_menu_new ();
  geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM (menuitem), submenu);

  /* Create "On" menuitem and attach to the submenu */
  menuitem = geda_image_menu_item_new_with_label (_(popup_items[ToolTipsOn]));
  gtk_widget_set_tooltip_text (menuitem, _(popup_tips[ToolTipsOn]));
  geda_menu_append (submenu, menuitem);

  /* Create "Off" menuitem and attach to the submenu */
  menuitem = geda_image_menu_item_new_with_label (_(popup_items[ToolTipsOff]));
  gtk_widget_set_tooltip_text (menuitem, _(popup_tips[ToolTipsOff]));
  geda_menu_append (submenu, menuitem);

  g_signal_connect (menuitem, "activate",
                    G_CALLBACK (compselect_menu_tooltips_on),
                    compselect);

  g_signal_connect (menuitem, "activate",
                    G_CALLBACK (compselect_menu_tooltips_off),
                    compselect);

  gtk_widget_show_all (menu);

  return (menu);
}

/*!
 * \brief ViewTree Mouse Button Constructor
 * \par Function Description
 *  This function checks if mouse botton press and when the 3rd button is
 *  released the compselect_build_view_menu function is called to create
 *  the mouse menu.
 *
 * \param [in] treeview   The TreeView widget when user "right-clicked"
 * \param [in] event      Mouse Button event record
 * \param [in] compselect Pointer to This dialog
 */
void
compselect_view_popup_menu (GtkWidget      *treeview,
                            GdkEventButton *event,
                            Compselect     *compselect)
{
  if (GEDA_IS_MENU(tree_view_popup_menu)) {
    gtk_widget_destroy(GTK_WIDGET(tree_view_popup_menu));
    g_object_ref_sink(tree_view_popup_menu);
    g_object_unref(tree_view_popup_menu);
    tree_view_popup_menu = NULL;
  }


  tree_view_popup_menu = compselect_build_view_menu(compselect, treeview);

  /* Tell GTK to do the menu we just created, Note: event can be NULL here
   * when called from compselect_view_onPopupMenu;
   * gdk_event_get_time() accepts a NULL argument */
  geda_menu_popup(GEDA_MENU(tree_view_popup_menu), NULL, NULL, NULL, NULL,
                 (event != NULL) ? event->button : 0,
                  gdk_event_get_time((GdkEvent*)event));
}

/*!
 * \brief ViewTree Mouse Button Call Back
 * \par Function Description
 *  This function checks the mouse botton press and when the 3rd button
 *  is released the build_menu function is called to create the mouse
 *  menu.
 *
 * \param [in] treeview   The treeview widget when user "right-clicked"
 * \param [in] event      Mouse Button event record
 * \param [in] compselect Pointer to This dialog
 */
bool
compselect_view_onButtonPressed (GtkWidget      *treeview,
                                 GdkEventButton *event,
                                 Compselect     *compselect)
{
    /* single click with the right mouse button? */
    if (event->type == GDK_BUTTON_PRESS  &&  event->button == 3) {

      compselect_view_popup_menu(treeview, event, compselect);

      return TRUE; /* we handled this */
    }

    return FALSE; /* we did not handle this */
}

bool
compselect_view_onPopupMenu (GtkWidget *treeview, Compselect *compselect)
{
    compselect_view_popup_menu(treeview, NULL, compselect);

    return TRUE; /* we handled this */
}

bool
compselect_search_treeview (GtkTreeModel *model,
                            int           column,
                            const char   *key,
                            GtkTreeIter  *iter,
                            void         *search_data)
{
    CLibSymbol *sym;

    gtk_tree_model_get (model, iter, LVC_ROW_DATA, &sym, -1);
    return (geda_utility_string_stristr (sym->name, key) >= 0);
}

/*!
 * \brief Creates the treeview for each notebook Library Tab */
static GtkWidget *
compselect_create_treeview_box (Compselect   *compselect,
                                GtkTreeView **viewtree,
                                DialogTabs    data_set)
{
  GtkWidget         *scrolled_win;
  GtkTreeView       *treeview;
  GtkTreeModel      *model;
  GtkTreeSelection  *selection;
  GtkCellRenderer   *renderer;
  GtkTreeViewColumn *column;

  /* This macro creates a vbox and adds the suffix _vbox, this is
   * the vbox we will put everthing in and the widget we return */
  GTK_NEW_vBOX(view, FALSE, DEFAULT_DIALOG_SPACING);

  geda_set_container_border_width(view_vbox, DIALOG_BORDER_WIDTH);

  model = compselect_create_lib_tree_model (compselect, data_set);

  scrolled_win = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                               /* GtkContainer */
                               "border-width", 5,
                               /* GtkScrolledWindow */
                               "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                               "vscrollbar-policy", GTK_POLICY_ALWAYS,
                               "shadow-type",       GTK_SHADOW_ETCHED_IN,
                               NULL);

  /* create the treeview */
  treeview = g_object_new (GTK_TYPE_TREE_VIEW,
                           /* GtkTreeView */
                           "model",      model,
                           "rules-hint", TRUE,
                           "headers-visible", FALSE,
                           NULL);

  gtk_tree_view_set_tooltip_column(treeview, LVC_TOOLTIP_TEXT);

  /* insert a column to treeview for library/symbol name */
  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                           /* GtkCellRendererText */
                           "editable", FALSE,
                           NULL);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         /* GtkTreeViewColumn */
                         "title", _("Components"),
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, TRUE);

  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           lib_treeview_set_cell_data,
                                           NULL, NULL);

  gtk_tree_view_set_search_column((GtkTreeView*)treeview, LVC_ROW_DATA);

  gtk_tree_view_set_search_equal_func ((GtkTreeView*) treeview,
                                       compselect_search_treeview,
                                       NULL,
                                       NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);

  /* Add the treeview to the scrolled window */
  geda_container_add (scrolled_win, treeview);

  /* Add the scrolled window for directories to the vertical box */
  PACK_START(view_vbox, scrolled_win, TRUE, TRUE, 0);

  g_signal_connect (treeview,
                    "row-activated",
                    G_CALLBACK (compselect_tree_row_activated),
                    compselect);

  /* connect callback to selection */
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (compselect_cb_tree_selection_changed),
                    compselect);

  g_signal_connect(treeview, "button-press-event",
                   (GCallback) compselect_view_onButtonPressed, compselect);

  g_signal_connect(treeview, "popup-menu",
                   (GCallback) compselect_view_onPopupMenu, compselect);

  /* Save pointer to treeview in compselect structure */
  *viewtree = treeview;

  return view_vbox;
}

/*!
 * \brief Creates the treeview widget for the attributes
 */
static GtkWidget*
compselect_create_attributes_treeview (Compselect *compselect)
{
  GtkWidget         *attrtreeview;
  GtkWidget         *scrolled_win;
  GtkListStore      *model;
  GtkCellRenderer   *renderer;
  GtkTreeViewColumn *column;

  model = gtk_list_store_new (NUM_ATTRIBUTE_COLUMNS,
                              G_TYPE_STRING, G_TYPE_STRING);

  attrtreeview = g_object_new (GTK_TYPE_TREE_VIEW,
                               /* GtkTreeView */
                               "model",      model,
                               "headers-visible", FALSE,
                               "rules-hint", TRUE,
                               NULL);

  /* two columns for name and value of the attributes */
  renderer = g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                           "editable", FALSE,
                           NULL);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         "title", _("Name"),
                         "resizable", TRUE,
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, TRUE);

  gtk_tree_view_column_add_attribute (column, renderer, "text",
                                      ATTRIBUTE_COLUMN_NAME);

  gtk_tree_view_append_column (GTK_TREE_VIEW (attrtreeview), column);

  column = g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                         "title", _("Value"),
                         "resizable", TRUE,
                         NULL);

  gtk_tree_view_column_pack_start (column, renderer, TRUE);

  gtk_tree_view_column_add_attribute (column, renderer, "text",
                                      ATTRIBUTE_COLUMN_VALUE);

  gtk_tree_view_append_column (GTK_TREE_VIEW (attrtreeview), column);

  scrolled_win =  g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                /* GtkContainer */
                                "border-width", 5,
                                /* GtkScrolledWindow */
                                "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                "vscrollbar-policy", GTK_POLICY_ALWAYS,
                                "shadow-type", GTK_SHADOW_ETCHED_IN,
                                NULL);

  geda_container_add (scrolled_win, attrtreeview);

  compselect->attrtreeview = GTK_TREE_VIEW (attrtreeview);

  return scrolled_win;
}

/*!
 *\brief Creates the filter area on the Component Select Dialog
 */
static GtkWidget *compselect_create_filter_area (Compselect *compselect)
{
  GtkWidget *label, *hbox,  *entry;
  GtkWidget *collapse_butt, *expand_butt;
  GtkWidget *clear_butt,    *refresh_butt;
  /*GtkWidget *top_butt,    *bottom_butt;*/

    /* -- filter area -- */
  hbox = g_object_new (GTK_TYPE_HBOX,
                       /* GtkBox */
                       "homogeneous", FALSE,
                       "spacing",     3,
                       NULL);

  /* create the entry label */
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign", 0.0,
                        /* GtkLabel */
                        "label", _LABEL(FilterEntry),
                        NULL);

  /* Add the search label to the filter area */
  PACK_START(hbox, label, FALSE, FALSE, 0);

  /* create the text entry for filter in components */
  entry = geda_entry_new();

  /* Add the filter entry to the filter area */
  PACK_START(hbox, entry,TRUE, TRUE, 0);

  /* set filter entry of compselect */
  compselect->entry_filter = (GedaEntry*)entry;

  /* and init the event source for component filter */
  compselect->filter_timeout = 0;

  /* create the Clear button for filter entry */
  clear_butt = g_object_new (GTK_TYPE_BUTTON,
                             /* GtkWidget */
                             "sensitive", FALSE,
                             /* GtkButton */
                             "relief",    GTK_RELIEF_NONE,
                             NULL);

  gtk_widget_set_tooltip_text(clear_butt, _TOOLTIP(ClearButton));

  geda_container_add (clear_butt,
                      gtk_image_new_from_stock (GTK_STOCK_CLEAR,
                                                GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the clear button to the filter area */
  PACK_START(hbox, clear_butt, FALSE, FALSE, 0);

  /* Save a pointer to the clear button in compselect */
  compselect->button_clear = GTK_BUTTON (clear_butt);

  /* create the refresh button */
  refresh_butt = g_object_new (GTK_TYPE_BUTTON,
                               /* GtkWidget */
                               "sensitive", TRUE,
                               /* GtkButton */
                               "relief",    GTK_RELIEF_NONE,
                               NULL);

  gtk_widget_set_tooltip_text(refresh_butt, _TOOLTIP(RefreshAllViews));

  geda_container_add (refresh_butt,
                      gtk_image_new_from_stock (GTK_STOCK_REFRESH,
                                                GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the refresh button to the filter area */
  PACK_START(hbox, refresh_butt, FALSE, FALSE, 0);

   /* create the collapse button */
  collapse_butt = g_object_new (GTK_TYPE_BUTTON,
                                /* GtkWidget */
                                "sensitive", TRUE,
                                /* GtkButton */
                                "relief",    GTK_RELIEF_NONE,
                                NULL);

  gtk_widget_set_tooltip_text(collapse_butt, _TOOLTIP(CollapseButton));

  geda_container_add (collapse_butt,
                      gtk_image_new_from_stock (GTK_STOCK_GO_UP,
                                                GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the collapse button to the filter area */
  PACK_START(hbox, collapse_butt, FALSE, FALSE, 0);

    /* create the expand  button */
  expand_butt = g_object_new (GTK_TYPE_BUTTON,
                              /* GtkWidget */
                              "sensitive", TRUE,
                              /* GtkButton */
                              "relief",    GTK_RELIEF_NONE,
                              NULL);

  gtk_widget_set_tooltip_text(expand_butt, _TOOLTIP(ExpandButton));

  geda_container_add (expand_butt,
                      gtk_image_new_from_stock (GTK_STOCK_GO_DOWN,
                                                GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the expand button to the filter area */
  PACK_START(hbox, expand_butt, FALSE, FALSE, 0);

  /* -- Setup the Callbacks for the Entry Area */
  g_signal_connect (entry,
                    "changed",
                    G_CALLBACK (compselect_callback_filter_entry_changed),
                    compselect);

  g_signal_connect (clear_butt,
                    "clicked",
                    G_CALLBACK (compselect_callback_clear_filter_clicked),
                    compselect);

  g_signal_connect (refresh_butt,
                    "clicked",
                    G_CALLBACK (compselect_callback_refresh_views),
                    compselect);

  g_signal_connect (collapse_butt,
                    "clicked",
                    G_CALLBACK (compselect_callback_collapse_all),
                    compselect);

  g_signal_connect (expand_butt,
                    "clicked",
                    G_CALLBACK (compselect_callback_expand_all),
                    compselect);

  return hbox;
}

/*!
 * \brief Create the style menu for the menu button in the Action Area
 */
static GedaMenu *
compselect_create_styles_menu (Compselect *ThisDialog)
{
  GedaMenu  *menu;
  GtkWidget *menuitem;
  GSList    *widget_list;
  int        i;

  struct styles {
    char *str;
    CompselectStyle style;
  } types[] = { { N_("None"),      COMPSELECT_STYLE_NONE },
                { N_("Style 1"),   COMPSELECT_STYLE1 },
                { N_("Style 2"),   COMPSELECT_STYLE2 },
                { N_("Style 3"),   COMPSELECT_STYLE3 },
                { N_("Style 4"),   COMPSELECT_STYLE4 },
                { N_("Style 5"),   COMPSELECT_STYLE5 },
                { N_("Style 6"),   COMPSELECT_STYLE6 },
                { N_("Style 7"),   COMPSELECT_STYLE7 },
                { N_("Style 8"),   COMPSELECT_STYLE8 },
                { N_("All"),       COMPSELECT_STYLE_ALL }
              };

  widget_list = NULL;

  menu  = GEDA_MENU (geda_menu_new ());

  for (i = 0; i < sizeof(types) / sizeof(struct styles); i++) {

    unsigned long handler;
    bool state;

    menuitem = geda_check_menu_item_new_with_mnemonic (_(types[i].str));
    geda_menu_append (menu, menuitem);

    GEDA_OBJECT_SET_DATA(menuitem, (void*)(long)types[i].style, "style");

    /* Maybe shouldn't bother since this will be over-riden
     * when dialog is restored */
    state = ((MASK & types[i].style) == types[i].style);

    g_object_set (menuitem, "active", state, NULL);

    /* Save pointer to widget */
    widget_list = g_slist_append(widget_list, menuitem);

    gtk_widget_show (menuitem);

    /* Setup callback for when a menu item is selected */
    handler= GTK_CALLBACK_TOGGLED (menuitem,
                                   compselect_toggle_style,
                                   ThisDialog);

    GEDA_OBJECT_SET_DATA(menuitem, (void*)handler, "handler");
  }

  gtk_widget_set_tooltip_text(menuitem, _("Enable all styles"));
  gtk_widget_set_tooltip_text(widget_list->data, _("Uncheck all styles"));

  if (MASK) /* If any value other than 0 then uncheck "None" */
    compselect_set_item_active(widget_list->data, FALSE);

  ThisDialog->style_menu_widgets = widget_list;

  return(menu);
}

/*!
 * \brief Create the menu of behaviors for the Component Select dialog
 * \par Function Description
 *  This function creates and returns a <B>GtkComboBox</B> for
 *  selecting the behavior when a component is added to the sheet.
 */
static GtkWidget *
compselect_create_behaviors_menu (void)
{
  GtkWidget *menu;
  GSList    *group;
  int        i;

  struct behaviors {
    char *str;
    CompselectBehavior behavior;
  } types[] = { { N_("Reference"),  COMPSELECT_BEHAVIOR_REFERENCE },
                { N_("Embed"),      COMPSELECT_BEHAVIOR_EMBED },
                { N_("Include"),    COMPSELECT_BEHAVIOR_INCLUDE }
              };

  menu  = geda_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof(types) / sizeof(struct behaviors); i++) {

    GtkWidget *menuitem;

    menuitem = geda_radio_menu_item_new_with_label (group, _(types[i].str));

    group = geda_radio_menu_item_group (GEDA_RADIO_MENU_ITEM (menuitem));

    geda_menu_append (menu, menuitem);

    GEDA_OBJECT_SET_DATA(menuitem, (void*)(long)types[i].behavior, "behaviors");
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*!
 * \brief GschemDialog "geometry_save" class method over-ride
 * \par Function Description
 *  Chain up to our parent's method to save the dialog's size and
 *  position, then save the dialog's current internal geometry.
 *
 * \param [in] dialog     The GschemDialog to save the geometry of.
 * \param [in] cfg        A Geda configuration object.
 * \param [in] group_name Group name in the key file to store the data under.
 */
static void
compselect_geometry_save (GschemDialog *dialog,
                          EdaConfig    *cfg,
                          char         *group_name)
{
  int  tmp_int;
  bool tmp_bool;
  unsigned int flags;

  /* Call the parent's geometry_save method */
  GSCHEM_DIALOG_CLASS (compselect_parent_class)->
  geometry_save (dialog, cfg, group_name);

  /* get position of the horizontal divider between the panes */
  tmp_int = gtk_paned_get_position (GTK_PANED (COMPSELECT (dialog)->hpaned));
  eda_config_set_integer (cfg, IDS_COMP_SELECT, "hpaned", tmp_int);

  /* get position of the verticla divider between the panes */
  tmp_int = gtk_paned_get_position (GTK_PANED (COMPSELECT (dialog)->vpaned));
  eda_config_set_integer (cfg, IDS_COMP_SELECT, "vpaned", tmp_int);

  /* save the active workbook tab */
  tmp_int = gtk_notebook_get_current_page (COMPSELECT (dialog)->notebook);
  eda_config_set_integer (cfg, IDS_COMP_SELECT, "source-tab", tmp_int);

  tmp_bool =COMPSELECT (dialog)->show_tips;
  eda_config_set_boolean (cfg, IDS_COMP_SELECT, "showtips", tmp_bool);

  tmp_bool =COMPSELECT (dialog)->show_groups;
  eda_config_set_boolean (cfg, IDS_COMP_SELECT, "groups", tmp_bool);

  tmp_bool =COMPSELECT (dialog)->subgroups;
  eda_config_set_boolean (cfg, IDS_COMP_SELECT, "subgroups", tmp_bool);

  tmp_bool =COMPSELECT (dialog)->do_sort;
  eda_config_set_boolean (cfg, IDS_COMP_SELECT, "sort", tmp_bool);

  /* Save the user current style mask */
  flags  = COMPSELECT (dialog)->style_flag;
  eda_config_set_integer (cfg, IDS_COMP_SELECT, "style", flags);
}

/*!
 * \brief GschemDialog "geometry_restore" class method handler
 * \par Function Description
 *  Chain up to our parent's method to restore the dialog's size and
 *  position, then restore the dialog's current internal geometry.
 *  This function also sets the style menu toggle widgets based on the
 *  flag mask, which was retrieved before the dialog was contructed.
 *
 * \remark This function is executed post contruction, essentially right
 *  before when the gtk_widget_show is applied to the entire window.
 *
 * \param [in] dialog     The GschemDialog to restore the geometry of.
 * \param [in] cfg        A Geda configuration object.
 * \param [in] group_name Group name in the key file to store the data under.
 */
static void
compselect_geometry_restore (GschemDialog *dialog,
                             EdaConfig    *cfg,
                             char         *group_name)
{
  Compselect *ThisDialog;
  int         tmp_int;

  ThisDialog = COMPSELECT (dialog);

  CompselectStyle types[] = { COMPSELECT_STYLE_NONE,
                              COMPSELECT_STYLE1,
                              COMPSELECT_STYLE2,
                              COMPSELECT_STYLE3,
                              COMPSELECT_STYLE4,
                              COMPSELECT_STYLE5,
                              COMPSELECT_STYLE6,
                              COMPSELECT_STYLE7,
                              COMPSELECT_STYLE8,
                              COMPSELECT_STYLE_ALL };

  /* Call the parent's geometry_restore method */
  GSCHEM_DIALOG_CLASS (compselect_parent_class)->
    geometry_restore (dialog, cfg, group_name);

  tmp_int = eda_config_get_integer (cfg, IDS_COMP_SELECT, "hpaned", NULL);
  if (tmp_int > 0)
    gtk_paned_set_position (GTK_PANED (ThisDialog->hpaned), tmp_int);

  tmp_int = eda_config_get_integer (cfg, IDS_COMP_SELECT, "vpaned", NULL);
  if (tmp_int > 0)
    gtk_paned_set_position (GTK_PANED (ThisDialog->vpaned), tmp_int);

  tmp_int = eda_config_get_integer (cfg, IDS_COMP_SELECT, "source-tab", NULL);
  if (tmp_int >= 0)
    gtk_notebook_set_current_page (ThisDialog->notebook, tmp_int);

  int i;

  /* skip over "None" & set active if bit is set */
  for (i = 1; i < G_N_ELEMENTS(types) ; i++) {

    GtkWidget *menuitem;
    bool state;

    menuitem = g_slist_nth_data(compselect->style_menu_widgets ,i);
    state    = ((MASK & types[i]) == types[i]);

    compselect_set_item_active(menuitem, state);
  }

  if (MASK) /* If any value other than 0 then uncheck "None" */
    compselect_set_item_active(compselect->style_menu_widgets->data, FALSE);

}

/*!
 * \brief Retrived Component Select dialog settings
 * \par Function Description
 *  This function is used to retrieve the previous settings such as the
 *  flag mask for the style menu before the dialog is contructed.
 *
 * \param [in] Dialog The Compselect Dialog widget.
 */
static void
compselect_settings_restore (Compselect *Dialog)
{
  EdaConfig *cfg      = eda_config_get_user_context ();

  Dialog->style_flag  = eda_config_get_integer (cfg, IDS_COMP_SELECT, "style", NULL);
  Dialog->show_groups = eda_config_get_boolean (cfg, IDS_COMP_SELECT, "groups", NULL);
  Dialog->subgroups   = eda_config_get_boolean (cfg, IDS_COMP_SELECT, "subgroups", NULL);
  Dialog->show_tips   = eda_config_get_boolean (cfg, IDS_COMP_SELECT, "showtips", NULL);
  Dialog->do_sort     = eda_config_get_boolean (cfg, IDS_COMP_SELECT, "sort", NULL);
}

#undef MASK

static void
compselect_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  if (gtk_widget_is_drawable(widget)) {

    Compselect *compselect = (Compselect*)widget;

    gtk_widget_queue_draw(compselect->hpaned);
  }
  GTK_WIDGET_CLASS(compselect_parent_class)->size_request(widget, requisition);
}

static void
compselect_style_set (GtkWidget *widget, GtkStyle *previous)
{
  Compselect *ThisDialog = COMPSELECT (widget);

  int ifocus;

  gtk_widget_style_get (widget, "focus-filter", &ifocus, NULL);

  if (ifocus) {
    gtk_widget_grab_focus (GTK_WIDGET (ThisDialog->entry_filter));
  }
}

/*!
 * \internal Helper for compselect_constructor
 * \brief Creates the Action Area on the Component Select Dialog
 */
static GtkWidget*
compselect_create_action_area (Compselect *ThisDialog, GtkWidget *parent, int mode)
{
  GtkWidget *action_hbox = NULL;
  GtkWidget *optionmenu  = NULL;

  GedaMenuButton *stylemenu = NULL;

  /* Create a Horizontal Box for everything to go into */
  action_hbox = gtk_hbox_new(FALSE, 0);

  /* ---- style Menu ---- */
  stylemenu = GEDA_MENU_BUTTON (geda_menu_button_new(NULL, _("Rescan")));

  if (stylemenu) {

    compselect->menu = compselect_create_styles_menu (ThisDialog);

    geda_menu_button_set_menu(stylemenu, (GtkWidget*)compselect->menu);

    geda_menu_button_set_tooltip_text(stylemenu, _("Rescan component libraries"));

    geda_menu_button_set_arrow_tooltip_text(stylemenu, _("Set style filter"));

    g_signal_connect (stylemenu, "clicked",
                      G_CALLBACK (compselect_callback_rescan_libraries),
                      ThisDialog);

    gtk_widget_show_all ((GtkWidget*)stylemenu);

    SetWidgetSize(stylemenu, 125, DIALOG_BUTTON_VSIZE);

    ThisDialog->style_menu = stylemenu;

    /* Add the combobox to the horizontal action box */
    PACK_START(action_hbox, ThisDialog->style_menu, FALSE, FALSE, 10);

  }
  else {
    ThisDialog->style_menu = NULL;
  }

  /* ---- behavior Menu ---- */
  /* Create and Save a pointer to the behavior menu widget */
  optionmenu = geda_option_menu_new ();

  if (GEDA_IS_OPTION_MENU (optionmenu)) {

    geda_option_menu_set_menu((GedaOptionMenu*)optionmenu,
                               compselect_create_behaviors_menu());

    g_signal_connect (optionmenu, "changed",
                      G_CALLBACK (compselect_callback_behavior_changed),
                      ThisDialog);

    gtk_widget_show_all (optionmenu);

    SetWidgetSize(optionmenu, 165, DIALOG_BUTTON_VSIZE);

    ThisDialog->behavior_menu = ((GedaOptionMenu*)optionmenu);

    /* Add the combobox to the horizontal action box */
    PACK_START(action_hbox, ThisDialog->behavior_menu, FALSE, FALSE, 10);

  }
  else {
    ThisDialog->behavior_menu = NULL;
  }

  ContinueSwitch = NULL;

  /* Create Toggle Switch widgets and put inside the horizontal options box*/
  EDA_SWITCH(action_hbox, Continue, 5, mode);

  /* Setup callback for Switch widgets */
  GEDA_CALLBACK_SWITCH (Continue, compselect_cb_switch_toggled, ThisDialog)

  /* Create and connect the Close and Okay Buttons */
  GtkWidget *close_butt = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
  GtkWidget *okay_butt  = gtk_button_new_from_stock (GTK_STOCK_OK);

  SetWidgetSize(close_butt, DIALOG_BUTTON_HSIZE, DIALOG_BUTTON_VSIZE);
  SetWidgetSize(okay_butt,  DIALOG_BUTTON_HSIZE, DIALOG_BUTTON_VSIZE);

  g_signal_connect (close_butt,
                    "clicked",
                    G_CALLBACK (compselect_on_close_butt_clicked),
                    ThisDialog);

  g_signal_connect (okay_butt,
                    "clicked",
                    G_CALLBACK (compselect_on_okay_butt_clicked),
                    ThisDialog);

  gtk_box_pack_end (GTK_BOX (action_hbox), okay_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  gtk_box_pack_end (GTK_BOX (action_hbox), close_butt, FALSE, FALSE,
                    DIALOG_H_SPACING);

  return action_hbox;
}

/*!
 * \brief  Compselect Dialog Contructor
 * \par Function Description
 *  This is the main function for construction of the Compselect
 *  dialog box. Due to the expanse of this dialog, other previously
 *  defined functions are called to create portions. Those function
 *  above have "create-" in the function name.
 *
 * \param [in] type                   The GedaType type
 * \param [in] n_construct_properties The number of inital parameters?
 * \param [in] construct_params       Inital g_object property values
 */
static GObject*
compselect_constructor (GType                  type,
                        unsigned int           n_construct_properties,
                        GObjectConstructParam *construct_params)
{
  GschemToplevel *w_current;
  GObject        *object;
  Compselect     *ThisDialog;

  GtkNotebook *notebook;
  GtkWidget   *hpaned, *vpaned;
  GtkWidget   *notebook_tab  = NULL;
  GtkWidget   *preview       = NULL;
  GtkWidget   *alignment     = NULL;
  GtkWidget   *frame         = NULL;
  GtkWidget   *action_area   = NULL;
  GtkVBox     *main_vbox     = NULL;
  GtkWidget   *label         = NULL;

  SortLibrarySwitch        = NULL;
  ShowGroupsSwitch         = NULL;

  /* chain up to constructor of parent class */
  object = G_OBJECT_CLASS (compselect_parent_class)->
           constructor (type, n_construct_properties, construct_params);

  ThisDialog = (Compselect*) (object);
  w_current  = GSCHEM_DIALOG (ThisDialog)->w_current;

  /* Initialize the hidden property */
  ThisDialog->hidden      = FALSE;
  ThisDialog->rescan_lib  = FALSE;

  ThisDialog->style_menu_widgets = NULL;

  /* Retrieve previous settings */
  compselect_settings_restore(ThisDialog);

  main_vbox = GTK_VBOX(GTK_DIALOG (ThisDialog)->vbox);

  /* dialog initialization */
  g_object_set (object,
                /* GtkWindow */
                "title",           _(DialogTitle),
                "default-height",  300,
                "default-width",   400,
                NULL);

  ThisDialog->stdtreeview   = NULL;
  ThisDialog->mantreeview   = NULL;
  ThisDialog->simtreeview   = NULL;
  ThisDialog->localtreeview = NULL;

  /* vertical pane containing preview and attributes */
  vpaned = gtk_vpaned_new ();

  /* horizontal pane containing selection and preview */
  hpaned = gtk_hpaned_new ();
  g_object_set(hpaned, "border-width", 5, NULL);

  ThisDialog->vpaned = vpaned;
  ThisDialog->hpaned = hpaned;

  /* Create a vertical box to hold the notebook and the filter area */
  GTK_NEW_vBOX(left, FALSE, DEFAULT_DIALOG_SPACING);

  /* notebook for library and inuse views */
  notebook = g_object_new (GTK_TYPE_NOTEBOOK, NULL);
  ThisDialog->notebook = (GtkNotebook*)notebook;

  /* Note" The order we create the notebook tabs is important */
  notebook_tab = compselect_create_inuse_treeview (ThisDialog);
  label = geda_label_new (_(IDS_COMPSELECT_TABS[IN_USE_TAB]));
  gtk_notebook_append_page (notebook, notebook_tab, label);

  notebook_tab = compselect_create_treeview_box (ThisDialog,
                                                 &ThisDialog->stdtreeview,
                                                 STD_TAB);
  label = geda_label_new (_(IDS_COMPSELECT_TABS[STD_TAB]));
  gtk_notebook_append_page (notebook, notebook_tab, label);

  notebook_tab = compselect_create_treeview_box (ThisDialog,
                                                 &ThisDialog->mantreeview,
                                                 MAN_TAB);
  label = geda_label_new (_(IDS_COMPSELECT_TABS[MAN_TAB]));
  gtk_notebook_append_page (notebook, notebook_tab, label);

  notebook_tab = compselect_create_treeview_box (ThisDialog,
                                                 &ThisDialog->simtreeview,
                                                 SIM_TAB);
  label = geda_label_new (_(IDS_COMPSELECT_TABS[SIM_TAB]));
  gtk_notebook_append_page (notebook, notebook_tab, label);

  notebook_tab = compselect_create_treeview_box (ThisDialog,
                                                 &ThisDialog->localtreeview,
                                                 LOCAL_TAB);
  label = geda_label_new (_(IDS_COMPSELECT_TABS[LOCAL_TAB]));
  gtk_notebook_append_page (notebook, notebook_tab, label);
  PACK_START(left_vbox, notebook, TRUE, TRUE, 0);

  ThisDialog->filter_hbox = compselect_create_filter_area (ThisDialog);
  gtk_widget_show_all (ThisDialog->filter_hbox);

  PACK_START(left_vbox, ThisDialog->filter_hbox, FALSE, FALSE, 0);

  gtk_paned_pack1 (GTK_PANED (hpaned), left_vbox, TRUE, FALSE);

  /* -- preview area -- */
  frame = g_object_new (GTK_TYPE_FRAME,
                        /* GtkFrame */
                        "label", _("Preview"),
                        NULL);

  alignment = g_object_new (GTK_TYPE_ALIGNMENT,
                            /* GtkAlignment */
                            "border-width",   5,
                            "xscale",         1.0,
                            "yscale",         1.0,
                            "xalign",         0.5,
                            "yalign",         0.5,
                            NULL);

  preview = g_object_new (GSCHEM_TYPE_PREVIEW,
                          /* Preview */
                          "active", FALSE,
                          NULL);

  geda_container_add (alignment, preview);
  geda_container_add (frame, alignment);

  /* save pointer to preview frame widget in compselect */
  ThisDialog->preview = GSCHEM_PREVIEW (preview);

  gtk_paned_pack1 (GTK_PANED (vpaned), frame, FALSE, FALSE);

  /* only create the attribute treeview if there are elements in the
     component_select_attrlist */
  if (w_current->component_select_attrlist == NULL) {
    ThisDialog->attrtreeview = NULL;
  }
  else {

    GtkWidget *attributes;

    frame = g_object_new (GTK_TYPE_FRAME,
                          /* GtkFrame */
                          "label", _("Attributes"), NULL);

    attributes = compselect_create_attributes_treeview (ThisDialog);

    gtk_paned_pack2 (GTK_PANED (vpaned), frame, FALSE, FALSE);
    geda_container_add (frame, attributes);
  }

  gtk_paned_pack2 (GTK_PANED (hpaned), vpaned, FALSE, FALSE);

  /* Add the hpaned to the dialog vbox */
  PACK_START(main_vbox, hpaned, TRUE, TRUE, 0)

  gtk_widget_show_all (hpaned);

  /* Create a horizontal box for options controls */
  GTK_NEW_hBOX(opts, FALSE, DEFAULT_DIALOG_SPACING);

  /* Create Toggle Switch widgets and put inside the horizontal options box*/
  EDA_SWITCH(opts_hbox, SortLibrary, 5, ThisDialog->do_sort);
  EDA_SWITCH(opts_hbox, ShowGroups,  5, ThisDialog->show_groups);
  EDA_SWITCH(opts_hbox, SubGroups,   5, ThisDialog->subgroups);

  /* Setup callback for Switch widgets */
  GEDA_CALLBACK_SWITCH (SortLibrary, compselect_cb_switch_toggled, ThisDialog)
  GEDA_CALLBACK_SWITCH (ShowGroups,  compselect_cb_switch_toggled, ThisDialog)
  GEDA_CALLBACK_SWITCH (SubGroups,   compselect_cb_switch_toggled, ThisDialog)

  /* Add the horizontal options box to the main vertical box */
  PACK_START(main_vbox, opts_hbox, FALSE, FALSE, 0)

  int mode = w_current->continue_component_place;

  /* Remove Gtk action area from the dialog and don't re-use it */
  action_area = GTK_DIALOG(ThisDialog)->action_area;
  geda_container_remove(main_vbox, action_area);

  action_area = compselect_create_action_area (compselect, (GtkWidget*)main_vbox, mode);
  gtk_box_pack_end (GTK_BOX (main_vbox), action_area, FALSE, FALSE, 0);

  /* Replace the action_area with the new container */
  GTK_DIALOG(ThisDialog)->action_area = action_area;

  gtk_widget_show_all (action_area);

  g_signal_connect (notebook, "switch-page",
                    G_CALLBACK (compselect_on_notebook_switch_page),
                    ThisDialog);
  return object;
}

static void
compselect_dispose (GObject *object)
{
  if (tree_view_popup_menu != NULL) {
    gtk_widget_destroy(GTK_WIDGET(tree_view_popup_menu));
    g_object_ref_sink(tree_view_popup_menu);
    g_object_unref(tree_view_popup_menu);
    tree_view_popup_menu = NULL; /* Is static to the module */
  }

  G_OBJECT_CLASS (compselect_parent_class)->dispose (object);
}

static void
compselect_finalize (GObject *object)
{
  Compselect *ThisDialog = COMPSELECT (object);

  if (ThisDialog->filter_timeout != 0) {

    g_source_remove (ThisDialog->filter_timeout);
    ThisDialog->filter_timeout = 0;
  }

  G_OBJECT_CLASS (compselect_parent_class)->finalize (object);
}

/*!
 * \brief  Get Properties of the Compselect Dialog
 * \par Function Description
 *  This function handles the gobject properties request.
 *
 * \param [in]  object  Pointer to Compselect dialog structure
 * \param [in]  epid    The enumerated property ID
 * \param [out] value   The variable that will be set to the property value
 * \param [in]  pspec   The parameter specifications for the property
 */
static void
compselect_get_property (GObject     *object,
                         unsigned int epid,
                         GValue      *value,
                         GParamSpec  *pspec)
{
  Compselect *compselect = COMPSELECT (object);
  GtkWidget  *menuitem;

  switch (epid) {
      case PROP_SYMBOL:
        {
          GtkTreeModel *model;
          GtkTreeIter   iter;
          CLibSymbol   *symbol    = NULL;
          int           is_symbol = FALSE;

          struct {
            GtkTreeView *tree_view;
            unsigned int column;
          } tab_lookup [] = {
            {compselect->inusetreeview, IU_DATA_COLUMN},
            {compselect->stdtreeview,   LVC_ROW_DATA},
            {compselect->mantreeview,   LVC_ROW_DATA},
            {compselect->simtreeview,   LVC_ROW_DATA},
            {compselect->localtreeview, LVC_ROW_DATA},
          };

          if (compselect->active_tab < 5) {
            int active = compselect->active_tab;
            if (gtk_tree_selection_get_selected (
                gtk_tree_view_get_selection (tab_lookup[active].tree_view), &model, &iter))
            {
              if (is_symbol_record (model, &iter)) {
                gtk_tree_model_get (model, &iter, tab_lookup[active].column, &symbol, -1);
                is_symbol = TRUE;
              }
            }
          }

          if (is_symbol) {
            g_value_set_pointer (value, symbol);
          }

          break;
        }
      case PROP_BEHAVIOR:
        menuitem = geda_menu_widget_get_active (
                     geda_option_menu_get_menu(compselect->behavior_menu));
        g_value_set_enum (value, (int)(long)GEDA_OBJECT_GET_DATA (menuitem, "behaviors"));
        break;

      case PROP_HIDDEN:
        g_value_set_boolean (value, compselect->hidden);
        break;

      case PROP_VIEW:
        g_value_set_int (value, compselect->active_tab);
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, epid, pspec);
  }
}

/*!
 * \brief  Set Properties of the Compselect Dialog
 * \par Function Description
 *  This function handled the gobject properties setters.
 *
 * \param [in] object  Pointer to Compselect dialog structure
 * \param [in] epid    The enumerated property ID
 * \param [in] value   The value to set the property
 * \param [in] pspec   The parameter specifications for the property
 */
static void
compselect_set_property (GObject      *object,
                         unsigned int  epid,
                         const GValue *value,
                         GParamSpec   *pspec)
{
  Compselect *compselect = COMPSELECT (object);

  switch (epid) {
    case PROP_SYMBOL:
      compselect_open_to_symbol(compselect, g_value_get_pointer (value));
      break;

    case PROP_BEHAVIOR:
      geda_option_menu_set_history(compselect->behavior_menu,
                                   g_value_get_enum (value));
      break;

    case PROP_HIDDEN:
      compselect->hidden = g_value_get_boolean (value);
      if (compselect->hidden)
        gtk_widget_hide ((GtkWidget*)compselect);
      else
        gtk_window_present ((GtkWindow*)compselect);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, epid, pspec);
  }
}

/*!
 * \brief  Compselect Dialog Class Initialization
 * \par Function Description
 *  This is the class initializer function before construction of
 *  the Compselect dialog.
 */
static void
compselect_class_init (void *class, void *class_data)
{
  GParamSpec *params;

  CompselectClass   *compselect_class    = (CompselectClass*)class;
  GObjectClass      *object_class        = (GObjectClass*)class;
  GschemDialogClass *gschem_dialog_class = (GschemDialogClass*)class;
  GtkWidgetClass    *widget_class        = (GtkWidgetClass*)class;

  gschem_dialog_class->geometry_save     = compselect_geometry_save;
  gschem_dialog_class->geometry_restore  = compselect_geometry_restore;

  object_class->constructor   = compselect_constructor;
  object_class->dispose       = compselect_dispose;
  object_class->finalize      = compselect_finalize;
  object_class->get_property  = compselect_get_property;
  object_class->set_property  = compselect_set_property;

  widget_class->size_request  = compselect_size_request;
  widget_class->style_set     = compselect_style_set;

  compselect_parent_class     = g_type_class_peek_parent (class);

  compselect_class->refresh   = compselect_on_refresh_tree_views;

  params = g_param_spec_pointer ("symbol", "", "", G_PARAM_READWRITE);
  g_object_class_install_property (object_class, PROP_SYMBOL, params);

  params = g_param_spec_enum ("behavior", "", "",
                              COMPSELECT_TYPE_BEHAVIOR,
                              COMPSELECT_BEHAVIOR_REFERENCE,
                              G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_BEHAVIOR, params);

  params = g_param_spec_boolean ("hidden", "", "", FALSE, G_PARAM_READWRITE);
  g_object_class_install_property (object_class, PROP_HIDDEN, params);

  params = g_param_spec_int ("view",
                           _("active view"),
                           _("Active sheet of the notebook"),
                             IN_USE_TAB, LOCAL_TAB, IN_USE_TAB,
                             G_PARAM_READABLE);

  g_object_class_install_property (object_class, PROP_VIEW, params);

  /*!
  * CompselectClass:focus-filter:
  * Sets the initial focus to the filter entry widget.
  */
  params = g_param_spec_boolean ("focus-filter",
                               _("Focus Filter"),
                               _("When true, initial focus will be the filter entry, otherwise the Component Tree"),
                                 FALSE,
                                 G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  signals[REFRESH] =  g_signal_new ("refresh",
                                    TYPE_COMPSELECT,
                                    G_SIGNAL_RUN_FIRST,
                                    G_STRUCT_OFFSET (CompselectClass, refresh),
                                    NULL, NULL,
                                    geda_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);
}

/*!
 * \brief Initialize new Compselect data structure instance.
 * \par Function Description
 *  This function is call after the CompselectClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A Compselect data structure
 * \param [in] class     A CompselectClass Object
 */
static void compselect_instance_init(GTypeInstance *instance, void *class)
{
  Compselect *dialog;

  dialog = (Compselect*)instance;

  dialog->instance_type = compselect_get_type();
}

/*!
 * \brief Function to retrieve Compselect's Type identifier.
 * \par Function Description
 *  Function to retrieve a #Compselect Type identifier. When
 *  first called, the function registers a #Compselect in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a Compselect and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with Compselect.
 */
GedaType compselect_get_type (void)
{
  static GedaType compselect_type = 0;

  if (g_once_init_enter (&compselect_type)) {

    static const GTypeInfo info = {
      sizeof(CompselectClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      compselect_class_init,           /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(Compselect),
      0,                               /* n_preallocs         */
      compselect_instance_init         /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("Compselect");
    type   = g_type_register_static (GSCHEM_TYPE_DIALOG, string, &info, 0);

    g_once_init_leave (&compselect_type, type);
  }

  return compselect_type;
}

/*!
 * \brief Check if an object is a Compselect
 * \par Function Description
 *  Ensures dialog is a valid G_Object and compares signature
 *  to compselect dialog type.
 * \return TRUE if \a view is a valid Compselect
 */
bool is_a_compselect (Compselect *dialog)
{
  if (G_IS_OBJECT(dialog)) {
    return (compselect_get_type() == dialog->instance_type);
  }
  return FALSE;
}

GedaType compselect_behavior_get_type (void)
{
  static GedaType etype = 0;

  if (etype == 0) {
    static const GEnumValue values[] = {
      { COMPSELECT_BEHAVIOR_REFERENCE,
      "COMPSELECT_BEHAVIOR_REFERENCE", "reference" },
      { COMPSELECT_BEHAVIOR_EMBED,
      "COMPSELECT_BEHAVIOR_EMBED",     "embed" },
      { COMPSELECT_BEHAVIOR_INCLUDE,
      "COMPSELECT_BEHAVIOR_INCLUDE",   "include" },
      { 0, NULL, NULL }
    };

    etype = g_enum_register_static ("CompselectBehavior", values);
  }

  return etype;
}

/** @} end group Component-Dialog-Class */
