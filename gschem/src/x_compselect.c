/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"
#include <gdk/gdkkeysyms.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include <gschem_xdefines.h>            /* Define dialog default internal spacing */
#include <gschem_dialog.h>
#include <geda_dialog_controls.h>       /* Macros for Dialogs */

#include "x_preview.h"
#include "x_compselect.h"

/*! \def COMPSELECT_FILTER_INTERVAL
 *  \brief The time interval between request and actual filtering
 *
 *  This constant is the time-lag between user modifications in the
 *  filter entry and the actual evaluation of the filter which
 *  ultimately update the display. It helps reduce the frequency of
 *  evaluation of the filter as user types.
 *
 *  Unit is milliseconds.
 */
#define COMPSELECT_FILTER_INTERVAL 250

typedef enum {
  IN_USE_TAB=0,
  STD_TAB,
  MAN_TAB,
  SIM_TAB,
  LOCAL_TAB
} DialogTabs;

const char* IDS_COMPSELECT_TABS[] = {
  "In Use", "Std", "Manf", "Sim", "Local", /* Tab Name Strings*/
  NULL
};

const char* IDS_CATEGORIES[] = {
  "",
  "Standard", "Manufacturers", "Simulation", "Local", /* Tab Name Strings*/
  NULL
};

/* Enumerate Control IDs */
typedef enum {
       SortLibrary,
       ShowGroups,
       SubGroups,
} ControlID;

static WidgetStringData DialogStrings[] = {
  { "SortLibrarySwitch",  "Sort", "If this is enabled then the component library will be sorted in alphanumeric order.\n This option is cosmetic and will not alter the component search order (latest added gets scanned first)."},
  { "ShowGroupsSwitch",   "Groups", "Enable or disable Group Folders in treee views"},
  { "SubGroupsSwitch",    "Assort", "Enable or disable subfolder with in group Categories"},
  { NULL, NULL, NULL},
};

/*! \brief Process the response returned by the component selection dialog.
 *  \par Function Description
 *  This function handles the response <B>arg1</B> of the component
 *  selection dialog <B>dialog</B>.
 *
 *  Parameter <B>user_data</B> is a pointer on the relevant toplevel
 *  structure.
 *
 *  \param [in] dialog    The component selection dialog.
 *  \param [in] arg1      The response ID.
 *  \param [in] user_data A pointer on the GSCHEM_TOPLEVEL environment.
 */
static void
x_compselect_callback_response(GtkDialog *dialog, int arg1, gpointer user_data)
{
  Compselect *compselect = (Compselect*)dialog;
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *)user_data;
  TOPLEVEL *toplevel = w_current->toplevel;

  switch (arg1) {
      case COMPSELECT_RESPONSE_PLACE: {
        CLibSymbol *symbol = NULL;
        CompselectBehavior behavior;

        g_object_get (compselect,
                      "symbol", &symbol,
                      "behavior", &behavior,
                      NULL);

        w_current->include_complex = w_current->embed_components = 0;
        switch (behavior) {
            case COMPSELECT_BEHAVIOR_REFERENCE:
              break;
            case COMPSELECT_BEHAVIOR_EMBED:
              w_current->embed_components   = 1;
              break;
            case COMPSELECT_BEHAVIOR_INCLUDE:
              w_current->include_complex = 1;
              break;
            default:
              fprintf(stderr, "x_compselect_callback_response: OOPS!: unknown behavior\n");
        }

        if (w_current->event_state == ENDCOMP) {
          /* Delete the component which was being placed */
          if (w_current->rubber_visible)
            o_place_invalidate_rubber (w_current, FALSE);
          w_current->rubber_visible = 0;
          s_delete_object_glist (toplevel,
                                 toplevel->page_current->place_list);
          toplevel->page_current->place_list = NULL;
        } else {
          /* Cancel whatever other action is currently in progress */
          o_redraw_cleanstates (w_current);
        }

        if (symbol == NULL) {
          /* If there is no symbol selected, switch to SELECT mode */
          w_current->event_state = SELECT;
        } else {
          /* Otherwise set the new symbol to place */
          o_complex_prepare_place (w_current, symbol);
        }
        break;
      }

      case COMPSELECT_RESPONSE_HIDE:
        /* Response when clicking on the "hide" button */

        /* If there is no component in the complex place list, set the current one */
        if (toplevel->page_current->place_list == NULL) {
          gtk_dialog_response (GTK_DIALOG (compselect),
                               COMPSELECT_RESPONSE_PLACE);
        }

        /* Hide the Component Select Dialog */
        g_object_set (G_OBJECT (compselect), "hidden", TRUE, NULL);
        break;

      case GTK_RESPONSE_CLOSE:
      case GTK_RESPONSE_DELETE_EVENT:
        if (GTK_WIDGET (dialog) == w_current->cswindow){
          gtk_widget_destroy (GTK_WIDGET (dialog));
          w_current->cswindow = NULL;
        }
        if (w_current->event_state == ENDCOMP) {

          /* Cancel the place operation currently in progress */
          o_redraw_cleanstates (w_current);

          /* return to the default state */
          i_set_state (w_current, SELECT);
        }
        break;

      default:
        /* Do nothing, in case there's another handler function which
           can handle the response ID received. */
        break;
  }
}

/*! \brief Opens a component selection dialog.
 *  \par Function Description
 *  This function creates the Component Chooser Dialog for  <B>toplevel</B>
 *  if the dialog does not already exist. In this latter case, it only
 *  raises the dialog.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL environment data.
 */
void x_compselect_open (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget   *ThisDialog;
  Compselect  *ActiveDialog;

  ThisDialog = w_current->cswindow;
  if ( ThisDialog == NULL) {

    ThisDialog = g_object_new (TYPE_COMPSELECT, /* GschemDialog */
                               "settings-name",  DialogSettings,
                               "gschem-toplevel", w_current,
                                NULL);

    g_signal_connect (ThisDialog,
                      "response",
                      G_CALLBACK (x_compselect_callback_response),
                      w_current);

    gtk_window_set_transient_for (GTK_WINDOW (ThisDialog),
                                  GTK_WINDOW (w_current->main_window));

    gtk_widget_show (ThisDialog);
    w_current->cswindow = ThisDialog;

  } else {
    gtk_window_present (GTK_WINDOW (ThisDialog));
  }

  ActiveDialog = COMPSELECT (ThisDialog);

  GTK_EDITITABLE(ActiveDialog->entry_filter);

  if (strcmp (gtk_entry_get_text (ActiveDialog->entry_filter), "") != 0) {
    gtk_widget_grab_focus ((GtkWidget*) ActiveDialog->entry_filter);
  }
}

/*! \brief Unselect selection in the active tree view.
 *  \par Function Description
 *  This function deselects the selection in the active viewtree
 *  <B>toplevel</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL environment data.
 */
void x_compselect_deselect (GSCHEM_TOPLEVEL *w_current)
{
  Compselect *compselect = COMPSELECT (w_current->cswindow);

  if (compselect == NULL)
    return;

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
    fprintf(stderr, "x_compselect_deselect: ignoring Invalid Tab Id\n");
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

static GtkWidget *SortLibrarySwitch;
static GtkWidget *SubGroupsSwitch;
static GtkWidget *ShowGroupsSwitch;

static GObjectClass *compselect_parent_class = NULL;

static void compselect_class_init      (CompselectClass *class);
static GObject *compselect_constructor (GType type,
                                        guint n_construct_properties,
                                        GObjectConstructParam *construct_params);
static void compselect_finalize        (GObject *object);
static void compselect_set_property    (GObject *object,
                                        guint property_id,
                                        const GValue *value,
                                        GParamSpec *pspec);
static void compselect_get_property    (GObject *object,
                                        guint property_id,
                                        GValue *value,
                                        GParamSpec *pspec);

/*! \brief Get Active Tree View.
 *  \par Function Description
 *  This function returns pointer to the active viewtree
 *  <B>toplevel</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL environment data.
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

/*! \brief Sets data for a particular cell of the In Use treeview.
 *  \par Function Description
 *  This function determines what data is to be displayed in the
 *  "in use" symbol selection view.
 *
 *  The model is a list of symbols. s_clib_symbol_get_name() is called
 *  to get the text to display.
 */
static void
inuse_treeview_set_cell_data (GtkTreeViewColumn *tree_column,
                              GtkCellRenderer   *cell,
                              GtkTreeModel      *tree_model,
                              GtkTreeIter       *iter,
                              gpointer           data)
{
  CLibSymbol *symbol;

  gtk_tree_model_get (tree_model, iter, IU_DATA_COLUMN, &symbol, -1);
  g_object_set ((GObject*)cell, "text", s_clib_symbol_get_name (symbol), NULL);
}

/*! \brief Sets data for a particular cell in Library treeviews.
 *  \par Function Description
 *  This function determines what data is to be displayed in the
 *  selection selection view.
 *
 *  The top level of the model contains sources, and the next symbols.
 *  s_clib_source_get_name() or s_clib_symbol_get_name() as
 *  appropriate is called to get the text to display.
 */
static void
lib_treeview_set_cell_data (GtkTreeViewColumn *tree_column,
                            GtkCellRenderer   *cell,
                            GtkTreeModel      *tree_model,
                            GtkTreeIter       *iter,
                            gpointer           data)
{
  CLibSource *source;
  CLibSymbol *symbol;
  const char *text;
  const char *ptr;
  bool        is_symbol;

  gtk_tree_model_get (tree_model, iter, LVC_ROW_TYPE, &is_symbol, -1);

  if (is_symbol) {

    gtk_tree_model_get (tree_model, iter, LVC_ROW_DATA, &symbol, -1);
    text = s_clib_symbol_get_name (symbol);

  }
  else { /* Must be a source. */

    gtk_tree_model_get (tree_model, iter, LVC_ROW_DATA, &source, -1);
    text = s_clib_source_get_name (source);
    ptr  = strstr(source->name, "/");       /* look for a slash */
    if( ptr!= NULL) {
      text = ptr + 1;
    }

  }
  g_object_set ((GObject*)cell, "text", text, NULL);
}

/*! \brief Determines visibility of items of the library treeview.
 *  \par Function Description
 *  This is the function used to filter entries of the component
 *  selection tree.
 *
 *  \param [in] model The current selection in the treeview.
 *  \param [in] iter  An iterator on a component or folder in the tree.
 *  \param [in] data  The component selection dialog.
 *  \returns TRUE if item should be visible, FALSE otherwise.
 */
static bool lib_model_filter_visible_func (GtkTreeModel *model,
                                           GtkTreeIter  *iter,
                                           gpointer      data)
{
  Compselect *compselect = (Compselect*)data;
  CLibSymbol *sym;
  const char *compname;
  const char *text;
  char *compname_upper, *text_upper, *pattern;
  bool  ret;

  if(!IS_COMPSELECT (data)) {
    fprintf(stderr, "lib_model_filter_visible_func: need ptr to Dialog\n");
    return FALSE;
  }

  if (GTK_IS_ENTRY(compselect->entry_filter)) {
     text = gtk_entry_get_text (compselect->entry_filter);

     if (g_ascii_strcasecmp (text, "") == 0) {
       return TRUE;
     }
  }
  else
    return TRUE;

  /* If this is a source, only display if there are children */
  if (gtk_tree_model_iter_has_child (model, iter)) {

    GtkTreeIter iter2;

    gtk_tree_model_iter_children (model, &iter2, iter);
    ret = FALSE;
    do {
      if (lib_model_filter_visible_func (model, &iter2, data)) {
        ret = TRUE;
        break;
      }
    } while (gtk_tree_model_iter_next (model, &iter2));
  }
  else {

    gtk_tree_model_get (model, iter, LVC_ROW_DATA, &sym, -1);

    compname = s_clib_symbol_get_name (sym);
    /* Do a case insensitive comparison, converting the strings
       to uppercase */
    compname_upper = g_ascii_strup (compname, -1);
    text_upper = g_ascii_strup (text, -1);
    pattern = g_strconcat ("*", text_upper, "*", NULL);
    ret = g_pattern_match_simple (pattern, compname_upper);
    g_free (compname_upper);
    g_free (text_upper);
    g_free (pattern);
  }

  return ret;
}

/*! \brief Component row activated - double-click handler:
 *  \par Function Description
 * This functions handles row activation of a component row as a
 * convenince to the user, the row expands or colapses any node
 * with children.
 *
 *  \param [in] tree_view The component treeview.
 *  \param [in] path      The GtkTreePath to the activated row.
 *  \param [in] column    The GtkTreeViewColumn in which the activation occurred.
 *  \param [in] user_data The component selection dialog.
 */
static void tree_row_activated (GtkTreeView *tree_view, GtkTreePath *path,
                                GtkTreeViewColumn *column, gpointer dialog)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  bool is_symbol;

  model = gtk_tree_view_get_model (tree_view);
  gtk_tree_model_get_iter (model, &iter, path);

  /* get column 0 data into variable is_symbol */
  gtk_tree_model_get ( model, &iter, LVC_ROW_TYPE, &is_symbol, -1);

  if (is_symbol) {
    gtk_dialog_response (GTK_DIALOG (dialog), COMPSELECT_RESPONSE_HIDE);
    return;
  }

  if (gtk_tree_view_row_expanded (tree_view, path))
    gtk_tree_view_collapse_row (tree_view, path);
  else
    gtk_tree_view_expand_row (tree_view, path, FALSE);
}

/*! \brief GCompareFunc to sort an text object list by the object strings
 */
static int sort_object_text (OBJECT *a, OBJECT *b)
{
  return strcmp (a->text->string, b->text->string);
}

/*! \brief Update the model of the attributes treeview
 *  \par Function Description
 *  This function takes the toplevel attributes from the preview widget and
 *  puts them into the model of the <b>attrtreeview</b> widget.
 *  \param [in] compselect       The dialog compselect
 *  \param [in] preview_toplevel The toplevel of the preview widget
 */
void
update_attributes_model (Compselect *compselect, TOPLEVEL *preview_toplevel)
{
  GtkListStore *model;
  GtkTreeIter iter;
  GtkTreeViewColumn *column;
  GList *listiter, *o_iter, *o_attrlist, *filter_list;
  char *name, *value;
  OBJECT *o_current;

  model = (GtkListStore*) gtk_tree_view_get_model (compselect->attrtreeview);
  gtk_list_store_clear (model);

  /* Invalidate the column width for the attribute value column, so
   * the column is re-sized based on the new data being shown. Symbols
   * with long values are common, and we don't want having viewed those
   * forcing a h-scroll-bar on symbols with short valued attributes.
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

  o_attrlist = o_attrib_find_floating_attribs (
                              s_page_objects (preview_toplevel->page_current));

  filter_list = GSCHEM_DIALOG (compselect)->w_current->component_select_attrlist;

  if (filter_list != NULL && strcmp (filter_list->data, "*") == 0) {
    /* display all attributes in alphabetical order */
    o_attrlist = g_list_sort (o_attrlist, (GCompareFunc) sort_object_text);
    for (o_iter = o_attrlist; o_iter != NULL; o_iter = g_list_next (o_iter)) {
      o_current = o_iter->data;
      o_attrib_get_name_value (o_current, &name, &value);
      gtk_list_store_append (model, &iter);
      gtk_list_store_set (model, &iter, 0, name, 1, value, -1);
      g_free (name);
      g_free (value);
    }
  } else {
    /* display only attribute that are in the filter list */
    for (listiter = filter_list;
         listiter != NULL;
         listiter = g_list_next (listiter)) {
      for (o_iter = o_attrlist; o_iter != NULL; o_iter = g_list_next (o_iter)) {
        o_current = o_iter->data;
        if (o_attrib_get_name_value (o_current, &name, &value)) {
          if (strcmp (name, listiter->data) == 0) {
            gtk_list_store_append (model, &iter);
            gtk_list_store_set (model, &iter, 0, name, 1, value, -1);
          }
          g_free (name);
          g_free (value);
        }
      }
    }
  }
  g_list_free (o_attrlist);
}

/*! \brief Handles changes in the treeview selection.
 *  \par Function Description
 *  This is the callback function that is called every time the user
 *  select a row in any component treeview of the dialog.
 *
 *  If the selection is not a selection of a component (a directory
 *  name), it does nothing. Otherwise it retrieves the #CLibSymbol
 *  from the model.
 *
 *  It then emits the dialog's <B>apply</B> signal to let its parent
 *  know that a component has been selected.
 *
 *  \param [in] selection The current selection in the treeview.
 *  \param [in] user_data The component selection dialog.
 */
static void
cs_callback_tree_selection_changed (GtkTreeSelection *selection,
                                    gpointer          user_data)
{
  GtkTreeView  *view;
  GtkTreeModel *model;
  GtkTreeIter   iter;
  Compselect   *compselect = (Compselect*)user_data;

  const CLibSymbol *sym = NULL;
  char *buffer          = NULL;
  bool is_symbol        = FALSE;

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    gtk_tree_model_get ( model, &iter, LVC_ROW_TYPE, &is_symbol, -1);

    if(is_symbol) {

      /* get pointer to tree that triggered signal */
      view = gtk_tree_selection_get_tree_view (selection);

      if ( view == compselect->inusetreeview ) {
        gtk_tree_model_get (model, &iter, IU_DATA_COLUMN, &sym, -1);
      }
      else {
        gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &sym, -1);
      }
      buffer = s_clib_symbol_get_data (sym);
    }

  }

  /* update the preview with new symbol data */
  g_object_set (compselect->preview,
                "buffer", buffer,
                "active", (buffer != NULL),
                NULL);

  /* update the attributes with the toplevel of the preview widget*/
  if (compselect->attrtreeview != NULL)
    update_attributes_model (compselect,
                             compselect->preview->
                                         preview_w_current->toplevel);

  if(is_symbol) {
    /* signal a component has been selected to parent of dialog */
    g_signal_emit_by_name (compselect,
                           "response",
                           COMPSELECT_RESPONSE_PLACE,
                           NULL);
  }
  g_free (buffer);

}

/*! \brief Apply Filter to supplied View.
 *  \par Function Description
 *  This function applies the filtering of components in the treeview of
 *  the dialog based in the text in the search entry. The applying_filter
 *  variable is used here to prevent expanding when there is not text. This
 *  was added when the notebook sheets were added because the filtering is
 *  is apply to multible tree views on an as needed basis, aka, only when
 *  the user switches Tab's with text in the filter entry widget.
 *
 *  The the function is called when ever a filter timeout
 * occurs or when the user changes tabs.
 *
 *  \param [in] data The component selection dialog.
 *  \param [in] data The Active Tree view to operate on.
 */
static void cs_apply_filter_tree_view (Compselect *Dialog,
                                       GtkTreeView *view)
{
  GtkTreeModel *model;
  if(view != Dialog->inusetreeview) {
   model = gtk_tree_view_get_model (view);
   if (model != NULL) {
     gtk_tree_model_filter_refilter ((GtkTreeModelFilter*) model);
   }
   if ( Dialog->applying_filter)
     gtk_tree_view_expand_all (view);
  }
}
/*! \brief Requests re-evaluation of the filter.
 *  \par Function Description
 *  This is the timeout function for the filtering of component in the
 *  tree of the dialog. The function check for the presents of text in
 *  the entry widget and call the preceeding function to apply if there
 *  is text. The function sets the boolean variable applying_filter, which
 *  is used to control if the trees should be expand or not.
 *
 *  The timeout this callback is attached to is removed after the
 *  function, because the function return FALSE.
 *
 *  \param [in] data The component selection dialog.
 *  \returns FALSE to remove the timeout.
 */
static bool compselect_filter_timeout (gpointer data)
{
  Compselect  *Dialog = COMPSELECT (data);
  GtkTreeView *view;

  /* resets the source id in compselect */
  Dialog->filter_timeout = 0;

  view = get_active_tree_view (Dialog);

  if (strcmp (gtk_entry_get_text (Dialog->entry_filter), "") != 0) {
    /* filter text not-empty */
    /* Set Flag before applying filter */
    Dialog->applying_filter=TRUE;
    cs_apply_filter_tree_view (Dialog, view);
  }
  else { /* if was filtering then */
    /* filter text is empty, collapse expanded tree */
    Dialog->applying_filter=FALSE;
    gtk_tree_view_collapse_all (view);
  }

  /* return FALSE to remove the source */
  return FALSE;
}

/*! \brief Callback function for the changed signal of the filter entry.
 *  \par Function Description
 *  This function monitors changes in the entry filter of the dialog.
 *
 *  It specifically manages the sensitivity of the clear button of the
 *  entry depending on its contents. It also requests an update of the
 *  component list by re-evaluating filter at every changes.
 *
 *  \param [in] editable  The filter text entry.
 *  \param [in] user_data The component selection dialog.
 */
static void
compselect_callback_filter_entry_changed (GtkEditable *editable,
                                          gpointer  user_data)
{
  Compselect *compselect = COMPSELECT (user_data);
  GtkWidget *button;
  bool  sensitive;
  const char* text;

  /* turns button off if filter entry is empty otherwise*/
  /* turns it on  */
  button    = GTK_WIDGET (compselect->button_clear);

  text = gtk_entry_get_text (compselect->entry_filter);

  sensitive = (g_ascii_strcasecmp (text,"") != 0);

  if (GTK_WIDGET_IS_SENSITIVE (button) != sensitive) {
    gtk_widget_set_sensitive (button, sensitive);
  }

  compselect->applying_filter = sensitive;

  /* Cancel any pending update of the component list filter */
  if (compselect->filter_timeout != 0)
    g_source_remove (compselect->filter_timeout);

  /* Schedule an update of the component list filter in
   * COMPSELECT_FILTER_INTERVAL milliseconds */
  compselect->filter_timeout = g_timeout_add (COMPSELECT_FILTER_INTERVAL,
                                              compselect_filter_timeout,
                                              compselect);
}

/*! \brief Callback when a Switch is toggled on the Component Select Dialog
 *  \par Function Description:
 *   This function changes images for switches that are toggled. The image
 * is set to the opposite state, i.e. if ON use OFF image and if OFF use ON
 * image. The function then checks which switches was toggled and set the
 * variable associated with the switch. If the ShowGroups switch is turned
 * off then the Sub-groups switch is disabled because it does not make sense
 * to list every component in the library in a single column.
 *
 *  \param [in]  switch  ptr to the switch, aka toggle-button, widget
 *  \param [in]  Dialog  ptr to the dialog widget
 *
 * TODO: Currently this function does call for a refresh so the change is
 * not visible until the user clicks the refresh button.
 */
static void
cs_callback_switch_toggled(GtkWidget *Switch, GschemDialog *Dialog)
{
  GSCHEM_TOPLEVEL *w_current;
  Compselect      *ThisDialog;

  /* Changed the Switch image */
  TOGGLE_SWITCH(Switch);

  w_current = Dialog->w_current;
  ThisDialog = COMPSELECT (w_current->cswindow);

  if(Switch == SortLibrarySwitch) {
    w_current->sort_component_library = GET_SWITCH_STATE(SortLibrarySwitch);
  }
  else {
    if(Switch == ShowGroupsSwitch) {
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
      if(Switch == SubGroupsSwitch) {
        ThisDialog->subgroups = GET_SWITCH_STATE(SubGroupsSwitch);
      }
    }
  }

  return;
}

/*! \brief Handles changes of behavior.
 *  \par Function Description
 *  This function is called every time the value of the option menu
 *  for behaviors is modified.
 *
 *  It emits the dialog's <B>apply</B> signal to let the parent know
 *  that the requested behavior for the next adding of a component has
 *  been changed.
 *
 *  \param [in] optionmenu The behavior option menu.
 *  \param [in] user_data  The component selection dialog.
 */
static void
compselect_callback_behavior_changed (GtkOptionMenu *optionmenu,
                                      gpointer user_data)
{
  Compselect *compselect = (Compselect*)user_data;

  GtkWidget *menuitem;
  int menu_choice;

  menuitem = gtk_menu_get_active ( GTK_MENU
            (gtk_option_menu_get_menu (GTK_OPTION_MENU (optionmenu))));
  menu_choice = GPOINTER_TO_INT(
             gtk_object_get_data (GTK_OBJECT (menuitem), "behaviors"));

  switch(menu_choice) {
    case COMPSELECT_BEHAVIOR_REFERENCE:
      g_object_set (G_OBJECT (optionmenu), "tooltip-text",
                    "Default is to reference the component", NULL);
      break;
    case COMPSELECT_BEHAVIOR_EMBED:
      g_object_set (G_OBJECT (optionmenu), "tooltip-text",
                    "Embed component in schematic", NULL);
      break;
    case COMPSELECT_BEHAVIOR_INCLUDE:
      g_object_set (G_OBJECT (optionmenu), "tooltip-text",
                    "Include component as individual objects", NULL);
  }

  g_signal_emit_by_name (compselect,
                         "response",
                         COMPSELECT_RESPONSE_PLACE,
                         NULL);
}

/*! \brief Callback on Component Select Dialog change TAB in notebook.
 *  \par Function Description
 *  This function is called when ever a TAB sheet is selected. The
 * set the page number for use by other functions in this module and
 * then call cs_apply_filter_tree_view tp update the avtive
 * treeview widget.
 *
 */
static void
on_notebook_switch_page (GtkNotebook *notebook, GtkNotebookPage *page,
                         guint        page_num, Compselect *Dialog)
{
  Dialog->active_tab = page_num;
  if( Dialog->applying_filter)
    cs_apply_filter_tree_view (Dialog, get_active_tree_view(Dialog));

  return;
}

/*!\brief Create the tree model for the "In Use" view.
 * \par Function Description
 * Creates a straightforward list of symbols which are currently in
 * use, using s_toplevel_get_symbols().
 */
static GtkTreeModel* create_inuse_tree_model (Compselect *compselect)
{
  GSCHEM_TOPLEVEL *w_current;
  GtkListStore *store;
  GList *symlist, *iter;
  GtkTreeIter tree;

  w_current = GSCHEM_DIALOG (compselect)->w_current;

  store = (GtkListStore *) gtk_list_store_new (1, G_TYPE_POINTER);

  symlist = s_toplevel_get_symbols (w_current->toplevel);

  for (iter = symlist; iter != NULL; iter = g_list_next (iter)) {

    gtk_list_store_append (store, &tree);

    gtk_list_store_set (store, &tree, 0, iter->data, -1);
  }

  g_list_free (symlist);

  return (GtkTreeModel*)store;
}

/*!\brief Get Library Source List for Model for use in Library treeviews.
 * \par Function Description
 * This function filters the list of available sources for each ViewTree.
 * The function returns a glist of sources in the category associated
 * with the supplied data set enumerator. This has nothing to do with
 * the filter entry. The data is based on the Data and is used for all
 * library views but not the IN-USE Tab.
 *
 *  \param [in] w_current  GSCHEM_TOPLEVEL environment data.
 *  \param [in] data_set   Enumerator assocated with the TAB for the TreeView.
 *
 *  \returns [out] Ptr to Glist of selected sources matching the category.
 */
GList *get_tree_sources(GSCHEM_TOPLEVEL *w_current, Compselect *compselect,
                        DialogTabs data_set)
{

  GList *all_sources;
  GList *selected_sources;
  GList *src_iter;
  CLibSource *source;

  /* Get list of all sources */
  all_sources = s_clib_get_sources (w_current->sort_component_library != 0);

  /* Initialize a list to receive the sources for this group  */
  selected_sources = NULL;

  /* Loop through all sources and look for source for this group */
  for (src_iter = all_sources; src_iter != NULL; src_iter = g_list_next (src_iter)) {

    source =  src_iter->data; /* Retrieve a single source */

    /* Check if this source belongs in this group, i.e view TAB */
    if ( source->category != NULL &&
       ( strcmp(source->category, IDS_CATEGORIES[data_set]) == 0)) {

      /* Then add this to the list*/
      selected_sources = g_list_append (selected_sources, source);
    }
  }
  g_list_free (all_sources);

  return selected_sources;
}

/*!\brief Create the Tree Model for "Library" views.
 * \par Function Description
 * Creates a TreeStore with 2 columns, the first column is a boolean
 * value indicating whether the second column contains data for a
 * symbol or a source. If the first column contains TRUE then the
 * row is for a symbol entry, if FALSE then the data in the second
 * column is a pointer to a CLibSource record.
 *
 * Data supplied by get_tree_sources is added to a new TreeStore.
 * The data is monitored and if a source name contains a forward
 * slash, indicating a sub-directory in a symbol folder, then that
 * source row is added under the previous parent entry.
 *
 * A TreeModel containing the TreeStore is return after connecting
 * associating the model with the visibility filter.
 *
 * In theory, we could nest number of levels using this technique
 * but this implementation only looks for 1 slash.
 *
 */
static GtkTreeModel* create_lib_tree_model (Compselect *compselect,
                                            DialogTabs data_set)
{
  GSCHEM_TOPLEVEL *w_current;
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
  bool   have_parent;

  char  *tooltip_text;
  char  *previous_grp;
  char  *sym_name;
  int    sym_count;

  w_current = GSCHEM_DIALOG (compselect)->w_current;

  store = (GtkTreeStore*)gtk_tree_store_new (NUM_LV_COLUMNS,
                                             G_TYPE_BOOLEAN,
                                             G_TYPE_POINTER,
                                             G_TYPE_STRING );

  /* populate component store */
  sources = get_tree_sources (w_current, compselect, data_set);

  group_names = w_current->toplevel->component_groups;

    /* Set flag for "special" folders */
  bypassing   = ( data_set == LOCAL_TAB);
  bypassing  |= ( data_set == SIM_TAB);

  previous_grp = "";

  for (src_iter = sources; src_iter != NULL; src_iter = g_list_next (src_iter)) {

    source    =  src_iter->data;
    symlist   = source->symbols;
    sym_count = g_list_length(symlist);

    at_boundary  = (g_ascii_strcasecmp (previous_grp, source->group) == 0);
    have_parent  = gtk_tree_store_iter_is_valid (store, &parent);

    if ( (compselect->show_groups == FALSE) &&
         (g_list_find_string(group_names, source->group) > -1 )) {
      bypassing  = TRUE;
    }

    if ( !bypassing) {
      /* Might eliminate 1 of these, they alway seemed to occur in pairs */
      if ( at_boundary &&  have_parent ) {
        /* At the start of a new group, either add it */
        if (compselect->subgroups == TRUE) {
          gtk_tree_store_append (store, &tree_iter, &parent);
        }
        else /* or load symbols directly under parent */
          goto load_symbols;
      }
      else {
        gtk_tree_store_append (store, &tree_iter, NULL);
        g_copy_tree_iter(&tree_iter, &parent);
      }
    }
    else { /* Not Nesting a Group */
      if (sym_count > 0) {
          gtk_tree_store_append (store, &tree_iter, NULL);
      }
      else { /* is empty "special" folder with not files so */
        continue;
      }
    }

    /* Save a ptr to the group name for the next iteration */
    previous_grp = source->group;

    if (sym_count > 1) {
      /* Add tool tip to the source row */
      tooltip_text = g_strdup_printf("%s contains %d symbols", source->name, sym_count);
    }
    else
      tooltip_text = g_strdup_printf("%s Group", source->name);

    gtk_tree_store_set (store, &tree_iter,
                        LVC_ROW_TYPE, FALSE,
                        LVC_ROW_DATA, source,
                        LVC_TOOLTIP_TEXT, tooltip_text, -1);

    g_free(tooltip_text);

load_symbols: /* It Works! */

    for (sym_iter = symlist; sym_iter != NULL; sym_iter = g_list_next (sym_iter)) {

      /* if a directory only has one symbol file and the file name is
       * "placeholder.sym" then we don't display the symbol, the file
       * is being used to over-ride the switches and force a group to
       * always be on the treeview list */
      if (sym_count == 1) {
        sym_name = (char*) s_clib_symbol_get_name(sym_iter->data);
        if (g_ascii_strcasecmp ( "placeholder.sym", sym_name) == 0)
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
  model = (GtkTreeModel *) g_object_new (GTK_TYPE_TREE_MODEL_FILTER,
                                         "child-model", store,
                                         "virtual-root", NULL, NULL);

  gtk_tree_model_filter_set_visible_func ((GtkTreeModelFilter*)model,
                                          lib_model_filter_visible_func,
                                          compselect,
                                          NULL);
  g_object_unref (store);

  return model;
}

/*! \brief Handles a click on the clear button.
 *  \par Function Description
 *  This is the callback function called every time the user press the
 *  clear filter button. This function resets the filter entry, which
 *  indirectly causes re-evaluation of the filter on the list of symbols
 *  (because the entry "changed" is gets triggered), and this updates
 *  the active display view. To insure all of the views get updated, each
 *  library is manually updated.
 *
 *  \param [in] button    The clear button
 *  \param [in] user_data The component selection dialog.
 */
static void
compselect_callback_clear_filter_clicked (GtkButton *button,
                                          gpointer   user_data)
{
  Compselect *Dialog = COMPSELECT (user_data);

  /* clears text in text entry for filter */
  gtk_entry_set_text (Dialog->entry_filter, "");
  Dialog->applying_filter = FALSE;

  cs_apply_filter_tree_view (Dialog, Dialog->stdtreeview);
  cs_apply_filter_tree_view (Dialog, Dialog->mantreeview);
  cs_apply_filter_tree_view (Dialog, Dialog->simtreeview);
  cs_apply_filter_tree_view (Dialog, Dialog->localtreeview);

}

/*!\brief On-demand refresh of the component library.
 * \par Function Description
 * Requests a rescan of the component library in order to pick up any
 * new symbols, and updates each view on the component selector Dialog.
 */
static void
compselect_callback_refresh_libraries (GtkButton *button, gpointer user_data)
{
  Compselect *compselect = COMPSELECT (user_data);
  GtkTreeModel *model;

  /* inline function to update models in trees */
  inline void set_tree_view_model(GtkTreeView  *tree_view, DialogTabs data_set) {

    GtkTreeSelection *selection;

    g_object_unref (gtk_tree_view_get_model ( tree_view ));

    model = create_lib_tree_model (compselect, data_set);

    /* Block handling selection updated for duration of model changes */
    selection = gtk_tree_view_get_selection (tree_view);
    g_signal_handlers_block_by_func (selection,
                                     cs_callback_tree_selection_changed,
                                     compselect);

   /* Update the view model with signals blocked */
   gtk_tree_view_set_model (tree_view, model);

   /* Unblock & fire handler for stdtreeview selection */
   g_signal_handlers_unblock_by_func (selection,
                                     cs_callback_tree_selection_changed,
                                     compselect);
  }

  /* Rescan the libraries for symbols */
  s_clib_refresh ();

  /* Refresh the Standard Library TreeView */
  set_tree_view_model(compselect->stdtreeview, STD_TAB);

  /* Refresh the Manufactures Library TreeView */
  set_tree_view_model(compselect->mantreeview, MAN_TAB);

  /* Refresh the Local Library TreeView */
  set_tree_view_model(compselect->localtreeview, LOCAL_TAB);

  /* Refresh the Simulation Library TreeView */
  set_tree_view_model(compselect->simtreeview, SIM_TAB);

  /* Refresh the "In Use" view */
  g_object_unref (gtk_tree_view_get_model (compselect->inusetreeview));
  model = create_inuse_tree_model (compselect);

  /* Here we can update the model without blocking signals
   * as this is the (final) tree view we are updating */
  gtk_tree_view_set_model (compselect->inusetreeview, model);

}

/*!\brief Collase All button callback.
 * \par Function Description
 * Called when user presses the Collapse All button to close all opened
 * rows in the active view-tree.
 *
 * TODO: The standard call back does not have the parameter in the order
 * need to call the gtk_tree_view_collapse_all function directly. Isn't
 * there an optional callback protocol?
 */
static void
compselect_callback_collapse_all(GtkButton *button, gpointer user_data)
{
  Compselect  *compselect = COMPSELECT (user_data);
  GtkTreeView *view       = get_active_tree_view (compselect);

  if(GTK_IS_TREE_VIEW(view))
    gtk_tree_view_collapse_all (view);
  else
    fprintf(stderr, "compselect_callback_collapse_all, bad TreeView\n");
}

/*!\brief Expand All button callback.
 * \par Function Description
 * Called when user presses the Expand All button to open all parent
 * rows in the active view-tree.
 *
 * TODO: The standard call back does not have the parameter in the order
 * need to call the gtk_tree_view_collapse_all function directly. Isn't
 * there an optional callback protocol?
 */
static void
compselect_callback_expand_all(GtkButton *button, gpointer user_data)
{
  Compselect  *compselect = COMPSELECT (user_data);
  GtkTreeView *view       = get_active_tree_view (compselect);

  if(GTK_IS_TREE_VIEW(view))
    gtk_tree_view_expand_all (view);
  else
    fprintf(stderr, "compselect_callback_expand_all, bad TreeView\n");
}

/*! \brief Creates the treeview for the "In Use" view.
 * \par Function Description
 * This function create a new Treeview Widget Assembly. The assembly
 * contains a scrollable Treeview nested inside a vertical box object.
 *
 * \returns vBox widget Assembly
 */
static GtkWidget* create_inuse_treeview (Compselect *compselect)
{
  GtkWidget *scrolled_win, *treeview, *hbox, *button;
  GtkTreeModel      *model;
  GtkTreeSelection  *selection;
  GtkCellRenderer   *renderer;
  GtkTreeViewColumn *column;

  model = create_inuse_tree_model (compselect);

  /* vertical box for component selection and search entry */
  GTK_NEW_vBOX(inuse, FALSE, DEFAULT_DIALOG_SPACING);
  gtk_container_set_border_width( GTK_CONTAINER(inuse_vbox),
                                  DIALOG_BORDER_SPACING);

  /* Create a scrolled window to accomodate the treeview */
  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkContainer */
                  "border-width", 5,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_ALWAYS,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));

  /* Create the treeview */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "model",      model,
                                       "rules-hint", TRUE,
                                       "headers-visible", FALSE,
                                       NULL));

  /* Connect callback to selection */
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (cs_callback_tree_selection_changed),
                    compselect);

  /* Insert a column for symbol name */
  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TEXT, /* GtkCellRendererText */
                  "editable", FALSE,
                  NULL));

  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,  /* GtkTreeViewColumn */
                  "title", _("Components"),
                  NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           inuse_treeview_set_cell_data,
                                           NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* Add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), treeview);
  /* set the inuse treeview of compselect */
  compselect->inusetreeview = GTK_TREE_VIEW (treeview);

  /* Add the scrolled window for directories to the vertical box */
  PACK_BOX ( inuse_vbox, scrolled_win, TRUE, TRUE, 0);

  /* -- refresh button area -- */
  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                          /* GtkBox */
                                          "homogeneous", FALSE,
                                          "spacing",     3,
                                          NULL));

  /* create the refresh button */
  button = GTK_WIDGET (g_object_new (GTK_TYPE_BUTTON,
                                     /* GtkWidget */
                                     "sensitive", TRUE,
                                     /* GtkButton */
                                     "relief",    GTK_RELIEF_NONE,
                                     NULL));

  gtk_container_add (GTK_CONTAINER (button),
                     gtk_image_new_from_stock (GTK_STOCK_REFRESH,
                                            GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the refresh button to the horizontal box at the end */
  gtk_box_pack_end (GTK_BOX (hbox), button, FALSE, FALSE, 0);

  g_signal_connect (button,
                    "clicked",
                    G_CALLBACK (compselect_callback_refresh_libraries),
                    compselect);

  /* Add the refresh button area to the vertical box */
  PACK_BOX( inuse_vbox, hbox, FALSE, FALSE, 0);

  return inuse_vbox;
}

/*! \brief Creates the treeview for each notebook Library Tab */
static GtkWidget *create_treeview_box (Compselect   *compselect,
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

  gtk_container_set_border_width(GTK_CONTAINER(view_vbox),
                                 DIALOG_BORDER_SPACING);

  model  = create_lib_tree_model (compselect, data_set);

  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkContainer */
                  "border-width", 5,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_ALWAYS,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));

  /* create the treeview */
  treeview = GTK_TREE_VIEW (g_object_new (GTK_TYPE_TREE_VIEW,
                                          /* GtkTreeView */
                                          "model",      model,
                                          "rules-hint", TRUE,
                                          "headers-visible", FALSE,
                                          NULL));

  gtk_tree_view_set_tooltip_column(treeview, LVC_TOOLTIP_TEXT);

  /* insert a column to treeview for library/symbol name */
  renderer = GTK_CELL_RENDERER ( g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                                               /* GtkCellRendererText */
                                               "editable", FALSE,
                                               NULL));

  column = GTK_TREE_VIEW_COLUMN ( g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                                /* GtkTreeViewColumn */
                                                "title", _("Components"),
                                                NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);

  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           lib_treeview_set_cell_data,
                                           NULL, NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* Add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), GTK_WIDGET (treeview));

  /* Add the scrolled window for directories to the vertical box */
  PACK_BOX( view_vbox, scrolled_win, TRUE, TRUE, 0);

  g_signal_connect (treeview,
                    "row-activated",
                    G_CALLBACK (tree_row_activated),
                    compselect);

  /* connect callback to selection */
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (cs_callback_tree_selection_changed),
                    compselect);

  /* Save pointer to treeview in compselect structure */
  *viewtree = treeview;

  return view_vbox;
}

/*! \brief Creates the treeview widget for the attributes
 */
static GtkWidget *create_attributes_treeview (Compselect *compselect)
{
  GtkWidget *attrtreeview, *scrolled_win;
  GtkListStore *model;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  model = gtk_list_store_new (NUM_ATTRIBUTE_COLUMNS,
                              G_TYPE_STRING, G_TYPE_STRING);

  attrtreeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                           /* GtkTreeView */
                                           "model",      model,
                                           "headers-visible", FALSE,
                                           "rules-hint", TRUE,
                                           NULL));

  /* two columns for name and value of the attributes */
  renderer = GTK_CELL_RENDERER (g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                                              "editable", FALSE,
                                              NULL));

  column = GTK_TREE_VIEW_COLUMN (g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                               "title", _("Name"),
                                               "resizable", TRUE,
                                               NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);

  gtk_tree_view_column_add_attribute (column, renderer, "text",
                                      ATTRIBUTE_COLUMN_NAME);

  gtk_tree_view_append_column (GTK_TREE_VIEW (attrtreeview), column);

  column = GTK_TREE_VIEW_COLUMN (g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                               "title", _("Value"),
                                               "resizable", TRUE,
                                               NULL));

  gtk_tree_view_column_pack_start (column, renderer, TRUE);

  gtk_tree_view_column_add_attribute (column, renderer, "text",
                                      ATTRIBUTE_COLUMN_VALUE);

  gtk_tree_view_append_column (GTK_TREE_VIEW (attrtreeview), column);

  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkContainer */
                  "border-width", 5,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_ALWAYS,
                  "shadow-type", GTK_SHADOW_ETCHED_IN,
                  NULL));

  gtk_container_add (GTK_CONTAINER (scrolled_win), attrtreeview);

  compselect->attrtreeview = GTK_TREE_VIEW (attrtreeview);

  return scrolled_win;
}

/*! \brief Creates the treeview for the "Library" view */
static GtkWidget *create_filter_area (Compselect *compselect)
{
  GtkWidget *label, *hbox,  *entry;
  GtkWidget *collapse_butt, *expand_butt;
  GtkWidget *clear_butt,    *refresh_butt;
  /*GtkWidget *top_butt,    *bottom_butt;*/

  DECLARE_TOOPTIPS;

    /* -- filter area -- */
  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                   /* GtkBox */
                                   "homogeneous", FALSE,
                                   "spacing",     3,
                                    NULL));

  /* create the entry label */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign", 0.0,
                                    /* GtkLabel */
                                    "label",  _("Filter:"),
                                    NULL));

  /* Add the search label to the filter area */
  PACK_BOX( hbox, label, FALSE, FALSE, 0);

  /* create the text entry for filter in components */
  entry = GTK_WIDGET (g_object_new (GTK_TYPE_ENTRY,
                                    /* GtkEntry */
                                    "text", "",
                                    NULL));

  /* Add the filter entry to the filter area */
  PACK_BOX( hbox, entry,TRUE, TRUE, 0);

  /* set filter entry of compselect */
  compselect->entry_filter = GTK_ENTRY (entry);

  /* and init the event source for component filter */
  compselect->filter_timeout = 0;

  /* create the Clear button for filter entry */
  clear_butt = GTK_WIDGET (g_object_new (GTK_TYPE_BUTTON,
                                         /* GtkWidget */
                                         "sensitive", FALSE,
                                         /* GtkButton */
                                         "relief",    GTK_RELIEF_NONE,
                                         NULL));

  gtk_widget_set_tooltip_text(clear_butt, _("Clear the filter entry"));

  gtk_container_add (GTK_CONTAINER (clear_butt),
                     gtk_image_new_from_stock (GTK_STOCK_CLEAR,
                                               GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the clear button to the filter area */
  PACK_BOX( hbox, clear_butt, FALSE, FALSE, 0);

  /* Save a pointer to the clear button in compselect */
  compselect->button_clear = GTK_BUTTON (clear_butt);

  /* create the refresh button */
  refresh_butt = GTK_WIDGET (g_object_new (GTK_TYPE_BUTTON,
                                           /* GtkWidget */
                                           "sensitive", TRUE,
                                           /* GtkButton */
                                           "relief",    GTK_RELIEF_NONE,
                                           NULL));

  gtk_widget_set_tooltip_text(refresh_butt, _("Refresh the active View"));

  gtk_container_add (GTK_CONTAINER (refresh_butt),
                     gtk_image_new_from_stock (GTK_STOCK_REFRESH,
                                            GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the refresh button to the filter area */
  PACK_BOX( hbox, refresh_butt, FALSE, FALSE, 0);

   /* create the collapse button */
  collapse_butt = GTK_WIDGET (g_object_new (GTK_TYPE_BUTTON,
                                           /* GtkWidget */
                                           "sensitive", TRUE,
                                           /* GtkButton */
                                           "relief",    GTK_RELIEF_NONE,
                                           NULL));

  gtk_widget_set_tooltip_text(collapse_butt, _("Close all Library containers"));

  gtk_container_add (GTK_CONTAINER (collapse_butt),
                     gtk_image_new_from_stock (GTK_STOCK_GO_UP,
                                    GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the collapse button to the filter area */
  PACK_BOX( hbox, collapse_butt, FALSE, FALSE, 0);

    /* create the expand  button */
  expand_butt = GTK_WIDGET (g_object_new (GTK_TYPE_BUTTON,
                                           /* GtkWidget */
                                           "sensitive", TRUE,
                                           /* GtkButton */
                                           "relief",    GTK_RELIEF_NONE,
                                           NULL));

  gtk_widget_set_tooltip_text(expand_butt, _("Expand all Library containers"));

  gtk_container_add (GTK_CONTAINER (expand_butt),
                     gtk_image_new_from_stock (GTK_STOCK_GO_DOWN,
                                    GTK_ICON_SIZE_SMALL_TOOLBAR));

  /* Add the expand button to the filter area */
  PACK_BOX( hbox, expand_butt, FALSE, FALSE, 0);

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
                    G_CALLBACK (compselect_callback_refresh_libraries),
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

/*! \brief Create a pin type  for the Pin Properties Editor dialog
 *  \par Function Description
 *  This function creates a GtkMenu with the different pin types.
 */
/*! \brief Create the menu of behaviors for the Component Select dialog
 *  \par Function Description
 *  This function creates and returns a <B>GtkComboBox</B> for
 *  selecting the behavior when a component is added to the sheet.
 */
static GtkWidget *create_behaviors_menu ( )
{
  GtkWidget *menu;
  GSList    *group;

  struct behaviors {
    char *str;
    CompselectBehavior behavior;
  } types[] = { { N_("Reference"),  COMPSELECT_BEHAVIOR_REFERENCE },
                { N_("Embed"),      COMPSELECT_BEHAVIOR_EMBED },
                { N_("Include"),    COMPSELECT_BEHAVIOR_INCLUDE }
              };

  int i;

  menu  = gtk_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof (types) / sizeof ( struct behaviors ); i++) {
    GtkWidget *menuitem;

    menuitem = gtk_radio_menu_item_new_with_label (group, _(types[i].str));

    group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));

    gtk_menu_append (GTK_MENU (menu), menuitem);

    gtk_object_set_data (GTK_OBJECT(menuitem), "behaviors",
                         GINT_TO_POINTER (types[i].behavior));
    gtk_widget_show (menuitem);
  }

  return(menu);
}

GType compselect_get_type ()
{
  static GType compselect_type = 0;

  if (!compselect_type) {
    static const GTypeInfo compselect_info = {
      sizeof (CompselectClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) compselect_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (Compselect),
      0,    /* n_preallocs */
      NULL  /* instance_init */
    };

    compselect_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                              "Compselect",
                                              &compselect_info, 0);
  }

  return compselect_type;
}

/*! \brief GschemDialog "geometry_save" class method handler
 *
 *  \par Function Description
 *  Chain up to our parent's method to save the dialog's size and
 *  position, then save the dialog's current internal geometry.
 *
 *  \param [in] dialog     The GschemDialog to save the geometry of.
 *  \param [in] key_file   The GKeyFile to save the geometry data to.
 *  \param [in] group_name Group name in the key file to store the data under.
 */
static void compselect_geometry_save (GschemDialog *dialog,
                                      GKeyFile *key_file, char *group_name)
{
  int position;

  /* Call the parent's geometry_save method */
  GSCHEM_DIALOG_CLASS (compselect_parent_class)->
    geometry_save (dialog, key_file, group_name);

  position = gtk_paned_get_position (GTK_PANED (COMPSELECT (dialog)->hpaned));
  g_key_file_set_integer (key_file, group_name, "hpaned", position);

  position = gtk_paned_get_position (GTK_PANED (COMPSELECT (dialog)->vpaned));
  g_key_file_set_integer (key_file, group_name, "vpaned", position);

  position = gtk_notebook_get_current_page (COMPSELECT (dialog)->notebook);
  g_key_file_set_integer (key_file, group_name, "source-tab", position);
}

/*! \brief GschemDialog "geometry_restore" class method handler
 *
 *  \par Function Description
 *  Chain up to our parent's method to restore the dialog's size and
 *  position, then restore the dialog's current internal geometry.
 *
 *  \param [in] dialog     The GschemDialog to restore the geometry of.
 *  \param [in] key_file   The GKeyFile to save the geometry data to.
 *  \param [in] group_name Group name in the key file to store the data under.
 */
static void compselect_geometry_restore (GschemDialog *dialog,
                                         GKeyFile *key_file,
                                         char *group_name)
{
  int position;

  /* Call the parent's geometry_restore method */
  GSCHEM_DIALOG_CLASS (compselect_parent_class)->
    geometry_restore (dialog, key_file, group_name);

  position = g_key_file_get_integer (key_file, group_name, "hpaned", NULL);
  if (position != 0)
    gtk_paned_set_position (GTK_PANED(COMPSELECT (dialog)->hpaned), position);

  position = g_key_file_get_integer (key_file, group_name, "vpaned", NULL);
  if (position != 0)
    gtk_paned_set_position (GTK_PANED(COMPSELECT (dialog)->vpaned), position);

  position = g_key_file_get_integer(key_file, group_name, "source-tab", NULL);
  gtk_notebook_set_current_page (COMPSELECT (dialog)->notebook, position);
}

static void
compselect_class_init (CompselectClass *klass)
{
  GParamSpec *params;

  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GschemDialogClass *gschem_dialog_class = GSCHEM_DIALOG_CLASS (klass);

  gschem_dialog_class->geometry_save    = compselect_geometry_save;
  gschem_dialog_class->geometry_restore = compselect_geometry_restore;

  gobject_class->constructor  = compselect_constructor;
  gobject_class->finalize     = compselect_finalize;
  gobject_class->set_property = compselect_set_property;
  gobject_class->get_property = compselect_get_property;

  compselect_parent_class = g_type_class_peek_parent (klass);

  params = g_param_spec_pointer ("symbol", "", "", G_PARAM_READABLE);
  g_object_class_install_property ( gobject_class, PROP_SYMBOL, params);

  params = g_param_spec_enum ("behavior", "", "",
                              COMPSELECT_TYPE_BEHAVIOR,
                              COMPSELECT_BEHAVIOR_REFERENCE,
                              G_PARAM_READWRITE);

  g_object_class_install_property ( gobject_class, PROP_BEHAVIOR, params);

  params = g_param_spec_boolean ("hidden", "", "", FALSE, G_PARAM_READWRITE);
  g_object_class_install_property ( gobject_class, PROP_HIDDEN, params);

  params = g_param_spec_int ("view", _("active view"),
                             _("Active sheet of the notebook widget"),
                             IN_USE_TAB, LOCAL_TAB, IN_USE_TAB,
                             G_PARAM_READABLE);
  g_object_class_install_property ( gobject_class, PROP_VIEW, params);

}

static GObject*
compselect_constructor (GType type,
                        guint n_construct_properties,
                        GObjectConstructParam *construct_params)
{
  GSCHEM_TOPLEVEL *w_current;
  GObject         *object;
  Compselect      *ThisDialog;

  GtkWidget *hpaned, *vpaned, *notebook, *attributes;
  GtkWidget *notebook_tab  = NULL;
  GtkWidget *preview       = NULL;
  GtkWidget *optionmenu    = NULL;
  GtkWidget *alignment     = NULL;
  GtkWidget *frame         = NULL;
  GtkWidget *filter_hbox   = NULL;
  GtkVBox   *main_vbox     = NULL;
  GtkWidget *label         = NULL;

  bool       do_sort;

  DECLARE_TOOPTIPS;

  /* chain up to constructor of parent class */
  object = G_OBJECT_CLASS (compselect_parent_class)->
    constructor (type, n_construct_properties, construct_params);
  ThisDialog = COMPSELECT (object);

  w_current = GSCHEM_DIALOG (ThisDialog)->w_current;

  main_vbox = GTK_VBOX(GTK_DIALOG (ThisDialog)->vbox);

  do_sort = w_current->sort_component_library;

  ThisDialog->show_groups = TRUE;
  ThisDialog->subgroups   = TRUE;

  SortLibrarySwitch  = NULL;
  ShowGroupsSwitch   = NULL;

  /* dialog initialization */
  g_object_set (object,
                /* GtkWindow */
                "title",           _(DialogTitle),
                "default-height",  300,
                "default-width",   400,
                NULL);

  /* vertical pane containing preview and attributes */
  vpaned = GTK_WIDGET (g_object_new (GTK_TYPE_VPANED, NULL));
  ThisDialog->vpaned = vpaned;

  /* horizontal pane containing selection and preview */
  hpaned = GTK_WIDGET (g_object_new (GTK_TYPE_HPANED,
                                    /* GtkContainer */
                                    "border-width", 5,
                                     NULL));
  ThisDialog->hpaned = hpaned;

  /* Create a vertical box to hold the notebook and the filter area */
  GTK_NEW_vBOX(left, FALSE, DEFAULT_DIALOG_SPACING);

  /* notebook for library and inuse views */
  notebook = GTK_WIDGET (g_object_new (GTK_TYPE_NOTEBOOK, NULL));
  ThisDialog->notebook = GTK_NOTEBOOK (notebook);

  /* Note" The order we create the notebook tabs is important */
  notebook_tab = create_inuse_treeview (ThisDialog);
  label = gtk_label_new (_(IDS_COMPSELECT_TABS[IN_USE_TAB]));
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), notebook_tab, label);

  notebook_tab = create_treeview_box (ThisDialog, &ThisDialog->stdtreeview, STD_TAB);
  label = gtk_label_new (_(IDS_COMPSELECT_TABS[STD_TAB]));
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), notebook_tab, label);

  notebook_tab = create_treeview_box (ThisDialog, &ThisDialog->mantreeview, MAN_TAB);
  label = gtk_label_new (_(IDS_COMPSELECT_TABS[MAN_TAB]));
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), notebook_tab, label);

  notebook_tab = create_treeview_box (ThisDialog, &ThisDialog->simtreeview, SIM_TAB);
  label = gtk_label_new (_(IDS_COMPSELECT_TABS[SIM_TAB]));
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), notebook_tab, label);

  notebook_tab = create_treeview_box (ThisDialog, &ThisDialog->localtreeview, LOCAL_TAB);
  label = gtk_label_new (_(IDS_COMPSELECT_TABS[LOCAL_TAB]));
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), notebook_tab, label);
  PACK_BOX( left_vbox, notebook, TRUE, TRUE, 0);

  filter_hbox = create_filter_area ( ThisDialog );
  gtk_widget_show_all (filter_hbox);

  PACK_BOX( left_vbox, filter_hbox, FALSE, FALSE, 0);

  gtk_paned_pack1 (GTK_PANED (hpaned), left_vbox, TRUE, FALSE);

  /* -- preview area -- */
  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                    /* GtkFrame */
                                    "label", _("Preview"),
                                    NULL));
  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        /* GtkAlignment */
                                        "border-width", 5,
                                        "xscale",         1.0,
                                        "yscale",         1.0,
                                        "xalign",         0.5,
                                        "yalign",         0.5,
                                        NULL));
  preview = GTK_WIDGET (g_object_new (TYPE_PREVIEW,
                                      /* Preview */
                                      "active", FALSE,
                                      NULL));

  gtk_container_add (GTK_CONTAINER (alignment), preview);
  gtk_container_add (GTK_CONTAINER (frame), alignment);

  /* save pointer to preview frame widget in compselect */
  ThisDialog->preview = PREVIEW (preview);

  gtk_paned_pack1 (GTK_PANED (vpaned), frame, FALSE, FALSE);

  /* only create the attribute treeview if there are elements in the
     component_select_attrlist */
  if (w_current->component_select_attrlist == NULL) {
    ThisDialog->attrtreeview = NULL;
  }
  else {
    frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                      /* GtkFrame */
                                      "label", _("Attributes"),
                                      NULL));
    attributes = create_attributes_treeview (ThisDialog);
    gtk_paned_pack2 (GTK_PANED (vpaned), frame, FALSE, FALSE);
    gtk_container_add (GTK_CONTAINER (frame), attributes);
  }

  gtk_paned_pack2 (GTK_PANED (hpaned), vpaned, FALSE, FALSE);

  /* Add the hpaned to the dialog vbox */
  PACK_BOX( main_vbox, hpaned, TRUE, TRUE, 0)

  gtk_widget_show_all (hpaned);

  /* Create a horizontal box for options controls */
  GTK_NEW_hBOX(opts, FALSE, DEFAULT_DIALOG_SPACING);

  /* Create Toggle Switch widgets and put inside the horizontal options box*/
  GEDA_SWITCH( (GTK_WIDGET(ThisDialog)), opts_hbox, SortLibrary, 5, do_sort);
  GEDA_SWITCH( (GTK_WIDGET(ThisDialog)), opts_hbox, ShowGroups,  5, ThisDialog->show_groups);
  GEDA_SWITCH( (GTK_WIDGET(ThisDialog)), opts_hbox, SubGroups,   5, ThisDialog->subgroups);

  /* Setup callback for Switch widgets */
  GEDA_CALLBACK_SWITCH (SortLibrary, cs_callback_switch_toggled, ThisDialog)
  GEDA_CALLBACK_SWITCH (ShowGroups,  cs_callback_switch_toggled, ThisDialog)
  GEDA_CALLBACK_SWITCH (SubGroups,   cs_callback_switch_toggled, ThisDialog)

  /* ---- behavior Menu ---- */
  /* Create and Save a pointer to the behavior menu widget */
  optionmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu), create_behaviors_menu ());
  g_signal_connect (optionmenu, "changed",
                    G_CALLBACK (compselect_callback_behavior_changed),
                    ThisDialog);

  gtk_widget_show_all (optionmenu);

  ThisDialog->behavior_menu = (GTK_OPTION_MENU(optionmenu));

   /* Add the combobox to the horizontal options box */
  PACK_BOX( opts_hbox, ThisDialog->behavior_menu, FALSE, FALSE, 10);

  /* Add the horizontal options box to the main vertical box */
  PACK_BOX( main_vbox, opts_hbox, FALSE, FALSE, 0)

  /* now add buttons in the action area */
  gtk_dialog_add_buttons (GTK_DIALOG (ThisDialog),
                          /*  - close button */
                          GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
                          GTK_STOCK_OK, COMPSELECT_RESPONSE_HIDE,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order (GTK_DIALOG (ThisDialog),
                                          COMPSELECT_RESPONSE_HIDE,
                                          GTK_RESPONSE_CLOSE,
                                          -1);

  /* Initialize the hidden property */
  ThisDialog->hidden = FALSE;

  g_signal_connect ((gpointer) notebook, "switch-page",
                    G_CALLBACK (on_notebook_switch_page),
                    ThisDialog);
  return object;
}

static void compselect_finalize (GObject *object)
{
  Compselect *ThisDialog = COMPSELECT (object);

  if (ThisDialog->filter_timeout != 0) {
    g_source_remove (ThisDialog->filter_timeout);
    ThisDialog->filter_timeout = 0;
  }

  G_OBJECT_CLASS (compselect_parent_class)->finalize (object);
}

static void compselect_set_property (GObject *object,
                                     guint property_id,
                                     const GValue *value,
                                     GParamSpec *pspec)
{
  Compselect *compselect = COMPSELECT (object);

  switch (property_id) {
    case PROP_BEHAVIOR:
      gtk_option_menu_set_history(compselect->behavior_menu,
                                  g_value_get_enum (value));
      break;
    case PROP_HIDDEN:
      compselect->hidden = g_value_get_boolean (value);
      if (compselect->hidden)
        gtk_widget_hide (GTK_WIDGET (compselect));
      else
        gtk_window_present (GTK_WINDOW (compselect));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

static void compselect_get_property (GObject *object,
                                     guint property_id,
                                     GValue *value,
                                     GParamSpec *pspec)
{
  Compselect *compselect = COMPSELECT (object);
  GtkWidget *menuitem;

  switch (property_id) {
      case PROP_SYMBOL:
        {
          GtkTreeModel *model;
          GtkTreeIter iter, parent;
          CLibSymbol *symbol = NULL;

          switch (compselect->active_tab) {
          case IN_USE_TAB:
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->inusetreeview),
                  &model,
                  &iter)) {
              gtk_tree_model_get (model, &iter, IU_DATA_COLUMN, &symbol, -1);
            }
            break;
          case STD_TAB:
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->stdtreeview),
                  &model,
                  &iter)
                && gtk_tree_model_iter_parent (model, &parent, &iter)) {
              gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &symbol, -1);
            }
            break;
          case MAN_TAB:
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->mantreeview),
                  &model,
                  &iter)
                && gtk_tree_model_iter_parent (model, &parent, &iter)) {
              gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &symbol, -1);
            }
            break;
          case SIM_TAB:
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->simtreeview),
                  &model,
                  &iter)
                && gtk_tree_model_iter_parent (model, &parent, &iter)) {
              gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &symbol, -1);
            }
            break;
          case LOCAL_TAB:
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->localtreeview),
                  &model,
                  &iter)
                && gtk_tree_model_iter_parent (model, &parent, &iter)) {
              gtk_tree_model_get (model, &iter, LVC_ROW_DATA, &symbol, -1);
            }
            break;
          default:
            fprintf(stderr, "compselect_get_property: OOPS!: unknown Tab\n");
          }

          g_value_set_pointer (value, symbol);
          break;
        }
      case PROP_BEHAVIOR:
        menuitem = gtk_menu_get_active( GTK_MENU(gtk_option_menu_get_menu(compselect->behavior_menu)));
        g_value_set_enum (value, GPOINTER_TO_INT(gtk_object_get_data (GTK_OBJECT (menuitem), "behaviors")));
        break;
      case PROP_HIDDEN:
        g_value_set_boolean (value, compselect->hidden);
        break;
      case PROP_VIEW:
        g_value_set_int (value, compselect->active_tab);
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

GType compselect_behavior_get_type (void)
{
  static GType etype = 0;

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
