/* C
;  File: x_settings_dialog.c
;;
;;; gEDA - GPL Electronic Design Automation
;;; gschem - gEDA Schematic Capture
;;; Copyright (C) 1998-2012 Ales Hvezda
;;; Copyright (C) 1998-2012 gEDA Contributors (see ChangeLog for details)
;;;
;;; Copyright (C) 2012 Wiley Edward Hill <wileyhill@gmail.com>
;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;
;;; Date: Aug, 17, 2012
;;; Contributing Author: Wiley Edward Hill
;;
*/
/************************ REVISION HISTORY *************************
;; Who |   When   |  What (Why)
;; ------------------------------------------------------------------
;; WEH | 09/17/12 |  Inital release.
;; ------------------------------------------------------------------
;;
;; To add a new variable or control:
;;
*/
/*! /Comment
 *
 * 1. The variable should be valid readable in the RC system, this is
 *    is not a requirement
 *
 * 2. Create the control
 *
 *      a.) Delare widget variable in the section Global Variables
 *      b.) Add the widget in the create_settings_dialog function.
 *      c.) Added a WidgetStringData record to struct in the file
 *          x_settings_dialog.h, see instructions at the top of the
 *          header.
 *      d.) Add any necassary support functions, to the responder
 *
 *      (At this point the control should be displayed properly.)
 *
 * 3. Initialize the value in the function load_settings_dialog 
 *
 * 4. Retrieve the value of the control in GatherSettings 
 *
 *      (At this point the widget should be functioning but the
 *       value will not be saved to disk.)
 *
 * 5. Add the keyword and keyword handlers to the list in the
 *    header file keywords.h
 *
 * 6. Implement a Writer function in the file x_settings.c
 *
 * 7. Varify:
 *
 *      a.) value changes when manually changed in the RC file.
 *      b.) values are properly written and formated correctly.
 *
*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include <guile/gh.h>

#include <gschem.h>
#include <ascii.h>

#include <gschem_dialog.h>
#include <x_dialog_controls.h>
#include <x_settings.h>
#include <x_settings_dialog.h>


/* ---------------  Function that Should Be Somewhere Else  ---------------- */

/* Why was this not already included in <gtktreemodel.c> */
bool gtk_tree_model_iter_previous (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
    GtkTreePath *path;
    gboolean ret;

    path = gtk_tree_model_get_path (tree_model, iter);
    ret = gtk_tree_path_prev (path);
    if (ret != FALSE)
      gtk_tree_model_get_iter (tree_model, iter, path);

    gtk_tree_path_free (path);
    return ret;
}

/* Could call this one gtkless_radio_group_get_active */
int gtk_radio_group_get_active(GSList *RadioGroupList) {
  GtkToggleButton *button;
  int length;
  int index;
  int active = -1;

  length = g_slist_length (RadioGroupList);

  for (index = 0; index < length; index++) {
     button = GTK_TOGGLE_BUTTON (g_slist_nth_data (RadioGroupList, index));
     if (button == NULL) return -1;
     if (gtk_toggle_button_get_active (button) == TRUE) {
        active = index;
        break;
     }
  }
  /* new buttons are *prepended* to the list, so buttons added as
   * first have last position in the list and using glist reverse
   * confuses gtk */
  return ((length - 1) - active);
}
void gtk_radio_group_set_active(GSList *RadioGroupList, int value)
{
  GtkToggleButton *button;
  int length;
  int index;
  int pos = GPOINTER_TO_UINT(value);

  length = g_slist_length (RadioGroupList);

  /* new buttons are *prepended* to the list, so buttons added as
   * first have last position in the list and using glist reverse
   * confuses gtk */

  index = (length - 1) - pos;

  if (index < 0 || index >= length) return;

  button = GTK_TOGGLE_BUTTON (g_slist_nth_data (RadioGroupList, index));
  if (button == NULL) return;

  if (gtk_toggle_button_get_active (button) == FALSE)
  {
    gtk_toggle_button_set_active (button, TRUE);
  }

  return;
}

/*! \brief function load_combo_str
 *  \par Function Description
 *  Loads GTK Combobox with strings from char array.
 *  @param[in]  combo  ptr to ComboBox to load, is not checked.
 *  @param[in]  list   ptr to array of strings
*/
void load_combo_str( GtkComboBox *combo, const char *list[])
{
  int i=0;
  while (list[i]) { gtk_combo_box_append_text (combo, _(list[i++]));}
}

/* --------------------------- Global Variables ---------------------------- */
/* \defgroup X_Settings_Dialog_Globals Global Variables
 *  @{
 */
gschem_rc_options rc_options={
	1,  /* display_color_map flag */
	0,  /* color_scheme_index */
	1,  /* display_outline_color_map flag */
	3,  /* window_size index */
	0,  /* custom_window_size flag */
	1,  /* world_size index */
	0,  /* custom_world_size flag */
        0,  /* titleblock_index */
	"gschem-colormap-darkbg", /* color_map_scheme file name [MAX_FILE]*/
	"",                       /* untitled_name[MAX_FILE] */
	""};                      /* titleblock_fname[64]; */

/* The Global Widgets */
GtkWidget *AddAttributeButt=NULL;
GtkWidget *RemoveAttributeButt=NULL;
GtkWidget *ClearAttributesButt=NULL;
GtkWidget *DefaultAttributesButt=NULL;
GtkWidget *ConfirmClearCheckBox=NULL;

/* The Combo Boxes */
GtkWidget *ColorMapSchemeCombo;
GtkWidget *DotGridModeCombo;
GtkWidget *LogWindowTypeCombo;
GtkWidget *MiddleButtonCombo;
GtkWidget *ThirdButtonCombo;
GtkWidget *TitleBlockCombo;
GtkWidget *UndoTypeCombo;

/* The one and only Text Entry Box */
GtkWidget *UntitledNameEntry;

/* The Radio Widgets */
  /* General TAB */
  DECLARE_RADIO_TRIAD (LogDestiny,   Window,   TTY,      Both);

  /* Edit Tab */
  DECLARE_RADIO_TRIAD (NetEndPoint,  None,     Filled,   Empty);
  DECLARE_RADIO_TRIAD (NetMidPoint,  None,     Filled,   Empty);
  DECLARE_RADIO_TRIAD (NetSelection, None,     Net,      All);

  /* Window Tab */
  DECLARE_RADIO_TRIAD (GridDotSize,   One,      Two,      Three);
  DECLARE_RADIO_TRIAD (GridMode,      None,     Dots,     Mesh);
//  DECLARE_RADIO_TRIAD (MeshThreshold, Three,    Four,     Five);
  DECLARE_QUAD_RADIO  (WindowSize,    W650H487, W900H650, W950H712, W1100H825);
  DECLARE_RADIO_TRIAD (WorldSize,     Large,    Medium,   Small);

  /* Text Tab */
  DECLARE_RADIO_TRIAD (CapsStyle,     Lower,    Upper,    Both);
  DECLARE_RADIO_TRIAD (TextFeedback,  Readable, Always,   Default);

  /* Styles Tab */
  DECLARE_RADIO_TRIAD (BusStyle,  None, Thin, Thick);
  DECLARE_RADIO_TRIAD (NetStyle,  None, Thin, Thick);
  DECLARE_RADIO_TRIAD (LineStyle, None, Thin, Thick);
  DECLARE_RADIO_TRIAD (PinStyle,  None, Thin, Thick);

  /* Attributes TAB */
  DECLARE_RADIO_TRIAD (DialogListAttributes,  All, None, List);

/* The Spinners */
GtkWidget *AttributeOffsetSpin;
GtkWidget *AutoPlacementGridSpin;
GtkWidget *AutoSaveIntervalSpin;
GtkWidget *DotGridThresholdSpin;
GtkWidget *KeyboardPanGainSpin;
GtkWidget *KeyboardPanGainSpin;
GtkWidget *MeshGridThresholdSpin;
GtkWidget *MousePanGainSpin;
GtkWidget *ScrollPanStepsSpin;
GtkWidget *SelectPixelsSpin;
GtkWidget *SnapSizeSpin;
GtkWidget *TextSizeSpin;
GtkWidget *TextSnapSizeSpin;
GtkWidget *TextZoomFactorSpin;
GtkWidget *ThickBusWidthSpin;
GtkWidget *ThickLineWidthSpin;
GtkWidget *ThickNetWidthSpin;
GtkWidget *ThickPinWidthSpin;
GtkWidget *ThinBusWidthSpin;
GtkWidget *ThinLineWidthSpin;
GtkWidget *ThinNetWidthSpin;
GtkWidget *ThinPinWidthSpin;
GtkWidget *UndoBufferSizeSpin;
GtkWidget *ZoomGainSpin;

/* The Switches */
GtkWidget *AutoLoadSwitch=NULL;
GtkWidget *AutoSaveSwitch=NULL;
GtkWidget *ClassicWheelSwitch=NULL;
GtkWidget *ConsolidateNetsSwitch=NULL;
GtkWidget *ContinuePlaceSwitch=NULL;
GtkWidget *DelayScrollingSwitch=NULL;
GtkWidget *DragMoveSwitch=NULL;
GtkWidget *DrawGripsSwitch=NULL;
GtkWidget *EmbedComponentsSwitch=NULL;
GtkWidget *EnableLogSwitch=NULL;
GtkWidget *EnableUndoSwitch=NULL;
GtkWidget *EnforceHierarchySwitch=NULL;
GtkWidget *FastMousePanSwitch=NULL;
GtkWidget *FeedbackModeSwitch=NULL;
GtkWidget *ForceBoundingBoxSwitch=NULL;
GtkWidget *FilePreviewSwitch=NULL;
GtkWidget *FriendlyColorMapSwitch=NULL;
GtkWidget *FriendlyOutlineMapSwitch=NULL;
GtkWidget *InitLogWindowSwitch=NULL;
GtkWidget *MagneticNetsSwitch=NULL;
GtkWidget *NetDirectionSwitch=NULL;
GtkWidget *NotifyEventsSwitch=NULL;
GtkWidget *ObjectClippingSwitch=NULL;
GtkWidget *RubberNetsSwitch=NULL;
GtkWidget *ScrollBarsSwitch=NULL;
GtkWidget *SortLibrarySwitch=NULL;
GtkWidget *SpareSwitchSwitch=NULL;
GtkWidget *TextOriginMarkerSwitch=NULL;
GtkWidget *UndoViewsSwitch=NULL;
GtkWidget *WarpCursorSwitch=NULL;
GtkWidget *ZoomPanSwitch=NULL;

GtkWidget *PotentialAttributesView=NULL;
GtkWidget *SelectedAttributesView=NULL;

/** @} END Group X_Settings_Dialog_Globals */

/* --------------------------- Support Functions --------------------------- */

/*! \defgroup X_Settings_Dialog_Support_Functions
 *  @{
          1. Inhibitor Support Functions
          2. Attributes Support ~ Group 1 & Group 2
          3. Button Support
          4. ComboBox Support
          5. Multi Widget Calllback Responders
*/
/* ---------------------- Inhibitor Support Functions ---------------------- */

/*!
 *   The Inhibitors enable and disable Widgets based on other selections.
 */
/*
	1. enable_attribute_list_controls	called in callback functions
	2. enable_color_map_controls
	3. enable_color_map_scheme
	4. enable_log_controls
	5. enable_undo_controls
	6. on_notebook_switch_page		is a callback handler
*/
/*! \brief Set Attribute list crontols based on the value of the
 *         component_select_attrlist.
 *  \par Function Description: This functions enables and disables buttons and
 *       a view tree based on weather list attributes are to be displayed on
 *       the component_select_attrlist filter list.
 */
static void enable_attribute_list_controls( bool state ){
  gtk_widget_set_sensitive (AddAttributeButt, state);
  gtk_widget_set_sensitive (RemoveAttributeButt, state);
  gtk_widget_set_sensitive (ClearAttributesButt, state);
  gtk_widget_set_sensitive (DefaultAttributesButt, state);
  gtk_widget_set_sensitive (SelectedAttributesView, state);
  return;
}
/** @brief enable_color_map_controls in X_Settings_Dialog_Support_Functions */
/*! \brief enables and disables FriendlyOutlineMapSwitch.
 *  \par Function Description
 *       Sets the state of the FriendlyOutlineMapSwitch. If enabling then
 *       enable ColorMapSchemeCombo if FriendlyOutlineMapSwitch is enabled.
 */
static void enable_color_map_controls( bool state ){
  gtk_widget_set_sensitive (FriendlyOutlineMapSwitch, state);
  if (!state) gtk_widget_set_sensitive (ColorMapSchemeCombo, FALSE);
  else
    if (GET_SWITCH_STATE (FriendlyOutlineMapSwitch))
      gtk_widget_set_sensitive (ColorMapSchemeCombo, TRUE);
}
/** @brief enable_color_map_scheme in X_Settings_Dialog_Support_Functions */
/*! \brief enables and disables ColorMapSchemeCombo.*/
static void enable_color_map_scheme( bool state ){
  gtk_widget_set_sensitive (ColorMapSchemeCombo, state);
}
/** @brief enable_log_controls in X_Settings_Dialog_Support_Functions */
/*! \brief enables and disables log related configuration options. */
static void enable_log_controls( bool state ){
  gtk_widget_set_sensitive (InitLogWindowSwitch, state);
  gtk_widget_set_sensitive (LogWindowTypeCombo, state);
  gtk_widget_set_sensitive (LogDestinyWindowRadio, state);
  gtk_widget_set_sensitive (LogDestinyTTYRadio, state);
  gtk_widget_set_sensitive (LogDestinyBothRadio, state);
}
/** @brief enable_undo_controls in X_Settings_Dialog_Support_Functions */
/*! \brief enables and disables undo related configuration options. */
static void enable_undo_controls( bool state ){
  gtk_widget_set_sensitive (UndoViewsSwitch, state);
  gtk_widget_set_sensitive (UndoTypeCombo, state);
  gtk_widget_set_sensitive (UndoBufferSizeSpin, state);
}
/** @brief on_notebook_switch_page in X_Settings_Dialog_Support_Functions */
/*! \brief Callback on TAB change.
 *  \par Function Description
 *       This function called when ever that TAB sheet is selected. This
 *       allows all sensitivities on sheet to be checked and set properly
 *       in case settings on one sheet effect setting on another.
 */
static void
on_notebook_switch_page (GtkNotebook *notebook, GtkNotebookPage *page,
                         guint        page_num, gpointer    user_data)
{
  bool state;
  switch ( page_num ) {
  case General:
    state = GET_SWITCH_STATE (AutoSaveSwitch);
      gtk_widget_set_sensitive (AutoSaveIntervalSpin, state);
    state = GET_SWITCH_STATE (EnableUndoSwitch);
      enable_undo_controls (state);
    state = GET_SWITCH_STATE (FriendlyColorMapSwitch);
      enable_color_map_controls (state);
    state = GET_SWITCH_STATE (FriendlyOutlineMapSwitch);
      enable_color_map_scheme(state);
    break;
  case Edit:
  case Pointer:
    break;
  case Window:
    if (gtk_combo_box_get_active (GTK_COMBO_BOX (DotGridModeCombo)) == DOTS_GRID_VARIABLE_MODE)
       gtk_widget_set_sensitive (DotGridThresholdSpin, FALSE);
    else
       gtk_widget_set_sensitive (DotGridThresholdSpin, TRUE);

    state = GET_SWITCH_STATE (ScrollBarsSwitch);
    gtk_widget_set_sensitive (DelayScrollingSwitch, state);
    break;
  case Text:
  case Styles:
    break;
  case Attributes:
    state = GET_SWITCH_STATE (DialogListAttributesListRadio);
    enable_attribute_list_controls (state);
    break;
  case Library:
    break;
  default:
    s_log_message("notebook_switch_page(): BAD_TAB ID %d\n", page_num);
  }

  return;
}

/* -------------------- Attributes Support Functions Group 1 ----------------*/
/*
   1. st_callback_selection_changed_view  Single click callback
   2. st_tree_row_activated               Double click callback
   3. connect_list_view                   Setup callbacks for list views
   4. initialize_tree_View                Initial widget setup
   5. add_to_list                         Called in load_tree_view_xxx
   6. load_tree_view_gl                   Load view from glist
   7. load_tree_view_str                  Load view from string array
*/
static void st_callback_selection_changed_view(GtkTreeSelection *selection,
                                               GtkWidget *Dialog)__attribute__((unused));

/*! \brief function st_callback_selection_changed_view
 *  \par Function Description
 *  This is a callback handler for attribute views and is called
 *  when an attribute is selected/single clicked.
 *  @param[in]  selection GtkTreeSelection set.
 *  @param[in]  Dialog    is really w_current.
 *
 * \remark
 *  This is disabled in the connect_list_view function, what to do?
 */
static void
st_callback_selection_changed_view(GtkTreeSelection *selection,
                                        GtkWidget *Dialog) 
{
  GtkTreeIter iter;
  GtkTreeModel *model;
  char *value;

  if (gtk_tree_selection_get_selected( selection, &model, &iter)) {
    gtk_tree_model_get(model, &iter, 0, &value,  -1);
    /* gtk_label_set_text( label, value); */
    s_log_message("Tree Widget selected:%s.\n", value);
    g_free(value);
  }
}

/*! \brief function st_tree_row_activated in X_Settings_Attribute
 *  \par Function Description
 *  This is a callback handler for attribute views and is called
 *  when an attribute is double clicked.
 *
 *  @param[in]  tree_view GtkTreeView (could be either one).
 *  @param[in]  path      Tree limb.
 *  @param[in]  column    There is only 1 text data column.
 *  @param[in]  Dialog    is really w_current.
 *
 * \remark
 *  This function does not do anything yet, could add/remove
 *  attribute and bypass buttons or what?
 */
static void
st_tree_row_activated (GtkTreeView *tree_view,
                       GtkTreePath *path,
                       GtkTreeViewColumn *column,
                       GtkWidget *Dialog)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  s_log_message("Row double-clicked\n");

  model = gtk_tree_view_get_model (tree_view);
  gtk_tree_model_get_iter (model, &iter, path);

  if (!gtk_tree_model_iter_has_child (model, &iter))
    return;

  if (gtk_tree_view_row_expanded (tree_view, path))
    gtk_tree_view_collapse_row (tree_view, path);
  else
    gtk_tree_view_expand_row (tree_view, path, FALSE);
}

/*! \brief function connect_list_view
 *  \par Function Description
 *  This sets up the the callbacks for the attribute views.
 *
 *  @param[in]  Dialog    is really w_current.
 *  @param[in]  selection GtkTreeSelection set.
 *
 * \remark
 *  Neither handler does anything useful, yet.
 */
static void
connect_list_view (GtkWidget *Dialog, GtkTreeView *TreeTextView)
{
  GtkTreeSelection *selection;

  selection = gtk_tree_view_get_selection(TreeTextView);
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

  /* connect callbacks
  g_signal_connect (selection,   "changed",
                    G_CALLBACK (st_callback_selection_changed_view),
                    Dialog);  */
  g_signal_connect (TreeTextView,"row-activated",
                    G_CALLBACK (st_tree_row_activated),
                    Dialog);
  return;
}

/*! \brief initialize_tree_View
 *  \par Function Description
 *   This is a generic function to setup a GTK Treeview with a single
 *   column of string/text data.
*/
void initialize_tree_View(GtkTreeView *list, int list_item,
                          int nColumns, int DataType)
{

  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkListStore *store;

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("List Items", renderer,
                                                    "text", list_item, NULL);

  gtk_tree_view_append_column(GTK_TREE_VIEW(list), column);

  store = gtk_list_store_new(nColumns, DataType);

  gtk_tree_view_set_model(GTK_TREE_VIEW(list), GTK_TREE_MODEL(store));

  g_object_unref(store);
}

/*  \par
 *
 *  Abstract:
 *
 *  There are two attribute list referenced in the initializations file.
 *  The first is a filter list used by the Add Component dialog. This
 *  list is stored in a Glist array. The second list is used by the Add
 *  Attributes routines and is stored in a static struc in Libgeda.
 *  Without overloads, we direct loading to one of the functions below
 *  to load Treeviews, the first for Glist, the second for arrays.
 *
 *  Note that if the filter list was commented out in the rc file, the
 *  first list was not read into the memory. A "default" filter list is
 *  maintained in an anonymous array of arrays, see load_tree_view_str
 *  function for more details.
 *  
*/
/*! \brief add_to_list 'in Tree View'
 *  \par Function Description
 *   This functions appends a string to a list in a given Treeview
*/
static void add_to_list(GtkTreeView *list, const char *str)
{
  GtkListStore *store;
  GtkTreeIter iter;
  gtk_tree_view_set_headers_visible (list, FALSE);
  store = GTK_LIST_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW(list)));
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, str, -1); /* Column 0 */
}
/*! \brief Load Tree View with string data from GList.
 *  \par Function Description
 *  This function does not actually load the data to the Liststore. The
 *  function retrieves a pointer for each (string) item in the Glist
 *  and calls add_to_list to store the item in the designated view.
*/
void load_tree_view_gl( GtkTreeView *TreeView, GList *list)
{
    lambda (const char* data)
    {
      add_to_list( TreeView, data);
      return FALSE;
    }
    foreach (list);
}
/*! \brief Load Tree View with string data from string array.
 *  \par Function Description
 *  This function passes each char* from an array of strings to
 *  the add_to_list function to store the item in the designated
 *  view.
 *      while (list[i]) { add_to_list(TreeView, list[i++]);}
 *
 *  The array of string can be NULL, in which case string are
 *  retieved using s_attrib_get.
*/
void load_tree_view_str( GtkTreeView *TreeView, const char *list[])
{
  int i=0;
  const char *string;

  const char* array ( int i) {
    if (list != NULL) /* if pointer is NULL, (not char) */
       return list[i];
    else
       return s_attrib_get(i);
  }

  string =  array(i);
  while (string != NULL) { add_to_list(TreeView, string);
    string = array(++i);
  }
}

/* -------------------- Attributes Support Functions Group 2 ----------------*/
/*
   1. GetAttributeFilterMode
   2. SaveAttributeFilterList       Save List in the Filter Viewtree
   3. SavePotentialAttributes       Save list all attibutes, is left Viewtree
   4. is_not_in_list                called by add_selected_attribute
   5. decrement_selected_attribute  Move selected attribute down in the list
   6. increment_selected_attribute  Move selected attribute up in the list
   7. add_selected_attribute        Add selected attribute up in the list
   8. remove_selected_attribute
   9. clear_attributes
  10. filter_list_set_default
*/

/*! \brief Function GetAttributeFilterMode
 *  \par Function Description: This is a Group 2 support function that
 *       returns the integer "setting" based on the state of the variable
 *       component_select_attrlist
 *
 *  \retval 0	= Filter All
 *          1	= No Filter
 *          2	= Filter List
 */
static int GetAttributeFilterMode(GSCHEM_TOPLEVEL *w_current) {

  char* data;

  data = g_list_nth_data (View2Data, 0);
  return data == NULL ? 1 : (data[0] == ASCII_ASTERISK ? 0 : 2 );
}

/*! \brief Function SaveAttributeFilterList
 *  \par Function Description: This is a Group 2 support function that
 *       clears the current attribute "filter" list, creates a new list
 *       or modified the old list based on the Dialog settings
 */
static int SaveAttributeFilterList(GSCHEM_TOPLEVEL *w_current) {
   GtkTreeModel *store = NULL;
   GtkTreeIter iter;

   char *str_new   = NULL;
   char *str_old   = NULL;
   int index       = 0;
   int list_length = 0;
   int next        = 0;

   switch (gtk_radio_group_get_active(DialogListAttributesRadioGroup)) {
     case 0:   /* ALL */
       g_list_free(View2Data);
       View2Data = g_list_append(NULL, "*");
       break;
     case 1:   /* NONE */
       g_list_free(View2Data);
       View2Data = NULL;
       break;
     case 2:   /* LIST */  
       store = gtk_tree_view_get_model (GTK_TREE_VIEW(SelectedAttributesView));

       /* Get the first iter in the list */
       next = gtk_tree_model_get_iter_first (store, &iter);

       list_length = g_list_length(View2Data);
       while (next)
       {
         /* Walk through the list, reading each row. */
         gtk_tree_model_get (store, &iter, 0, &str_new, -1);
         if (index < list_length) {
            str_old = g_list_nth_data (View2Data, index);
            if ( !strequal( str_old, str_new )) { /* update if they don't match */
              View2Data = g_list_remove (View2Data, str_old);
              View2Data = g_list_insert(View2Data, str_new, index);
            }
         }
         else {
           View2Data = g_list_append (View2Data, str_new);
         }
         next = gtk_tree_model_iter_next (store, &iter);
         index ++;
       }
       /* We went thru the new list and the index is still set, so if there
          are any more members in the old list they need to be removed */
       while (index < list_length) {
         str_old = g_list_nth_data (View2Data, index);
         View2Data = g_list_remove (View2Data, str_old);
         g_free (str_old);
         index ++;
       }
       break;
     default:
       s_log_message("SaveAttributeFilterList: DialogListAttributesRadioGroup returned bad ID\n");
   }
  /* Don't g_free (str_new) here because it's pointing
   * at a string somewhere and referenced in the glist */
   return index;
}

/*! \brief Function SavePotentialAttributes
 *  \par Function Description: This is a Group 2 support function that
 *       clears the current attribute list, gets the modified attribute
 *       list from the Treeview and stores new list. The dialog does
 *       not have provisions to add or remove attributes from this list
 *       so the only modification is the order of the attributes in the
 *       list.
 */
static int SavePotentialAttributes(GSCHEM_TOPLEVEL *w_current) {
  GtkTreeModel *store;
  GtkTreeIter iter;
  char *str_new;
  int next;
 
    s_attrib_init();

    store = gtk_tree_view_get_model (GTK_TREE_VIEW(PotentialAttributesView));

    /* Get the first iter in the list */
    next = gtk_tree_model_get_iter_first (store, &iter);
    while (next)
    {
    /* Walk through the list, reading each row. */
      gtk_tree_model_get (store, &iter, 0, &str_new, -1);
      s_attrib_add_entry(str_new);
      next = gtk_tree_model_iter_next (store, &iter);
    }
    return 0;
}

/*! \brief Function is_not_in_list
 *  \par Function Description: This is a Group 2 support function
 *       used to check is a given attribute is all in the filter
 *       Treeview.
 *
 *  @param[in]  GtkTreeView *list    Treeview to look in
 *  @param[in]  const char *str      str to look for
 */
static bool is_not_in_list(GtkTreeView *list, const char *str)
{
  GtkListStore *store;
  gboolean answer = FALSE;

  gboolean foreach_func (GtkTreeModel *model, GtkTreePath  *path,
                         GtkTreeIter  *iter,  gpointer      user_data)
  {
    gchar *attribute;

    /* Note: here we use 'iter' and not '&iter', because we did not allocate
     * the iter on the stack and are already getting the pointer to a tree iter */
    gtk_tree_model_get (model, iter, 0, &attribute, -1);

    answer = strequal( str, attribute);

    g_free(attribute); /* gtk_tree_model_get made copies of strings */
    return answer; /* stop walking the store if found, else call us with next row */
  }
  store = GTK_LIST_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW(list)));
  gtk_tree_model_foreach(GTK_TREE_MODEL(store), foreach_func, NULL);

  return !answer;
}

/*! \brief Function increment_selected_attribute
 *  \par Function Description: This is a Group 2 support function use to
 *       move an attribute up in the Potential (left) Treeview list. 
 */
static void increment_selected_attribute( void ){
  
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter1;
  GtkTreeIter *iter2=NULL;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(PotentialAttributesView));
  if (gtk_tree_selection_get_selected( selection, &model, &iter1)) {
    iter2 = gtk_tree_iter_copy (&iter1);
    if (gtk_tree_model_iter_previous (model, iter2))
    gtk_list_store_swap (GTK_LIST_STORE(model), &iter1, iter2);
  }
}

/*! \brief Function decrement_selected_attribute
 *  \par Function Description: This is a Group 2 support function use to
 *       move an attribute down in the Potential (left) Treeview list. 
*/
static void decrement_selected_attribute( void ){
  
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter1;
  GtkTreeIter *iter2=NULL;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(PotentialAttributesView));
  if (gtk_tree_selection_get_selected( selection, &model, &iter1)) {
    iter2 = gtk_tree_iter_copy (&iter1);
    if (gtk_tree_model_iter_next (model, iter2))
    gtk_list_store_swap (GTK_LIST_STORE(model), &iter1, iter2);
  }

}

/*! \brief Function add_selected_attribute
 *  \par Function Description: This is a Group 2 support function that
 *       adds the selected attribute from the left list to the right list
 *       but if the attribute is not already in the list. The attribute is
 *       inserted at the current selected position, or appends to end of
 *       of the filter list if there is no selection.
 */
static void add_selected_attribute( void ) {
  GtkTreeSelection *l_selection;
  GtkTreeSelection *r_selection;
  GtkListStore *l_store;
  GtkListStore *r_store;
  GtkTreeIter l_iter;
  GtkTreeIter r_iter;
  GtkTreeIter n_iter;
  char *value;

  /* Get selection for the Left list */
  l_selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(PotentialAttributesView));
  if (gtk_tree_selection_get_selected( l_selection, (GtkTreeModel**) &l_store, &l_iter)) {
    /* If a row was selected then l_selection, l_model, and l_iter were set so
       retrieve source string */
    gtk_tree_model_get(GTK_TREE_MODEL(l_store), &l_iter, 0, &value,  -1);
    if (is_not_in_list((GtkTreeView*) SelectedAttributesView, value)) { /* if not already in r list */

      /* check if selection in right list */
      r_selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(SelectedAttributesView));

      /* if there was a selection then r_iter set else GTK set r_iter to NULL */
      if (gtk_tree_selection_get_selected( r_selection, (GtkTreeModel**) &r_store, &r_iter))
        /* if r_iter = NULL then is appended to end but doens't seem to work */
        gtk_list_store_insert_before(r_store, &n_iter, &r_iter);
      else { /* so do this instead */
        r_store = GTK_LIST_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW (SelectedAttributesView)));
        gtk_list_store_append(r_store, &n_iter);
      }
      /* save the value to the right list */
      gtk_list_store_set(r_store, &n_iter, 0, value, -1); /* Column 0 */

    }
    g_free(value); /* Don't free unless we use it, less we be cursed */
  } /* endif there was an attribute selected in the left list */
}

/*! \brief Function remove_selected_attribute
 *  \par Function Description: This is a Group 2 support function that
 *       remove the selected attribute from the filter list.
 */
static void remove_selected_attribute( void ){
  
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(SelectedAttributesView));
  if (gtk_tree_selection_get_selected( selection, &model, &iter)) {
    gtk_list_store_remove (GTK_LIST_STORE (model),&iter);
   }
}

/*! \brief Function clear_attributes
 *  \par Function Description: This is a Group 2 support function that
 *       clears all of the attributes from the right Treeview, aka the
 *       filter list.
 */
static void clear_attributes( void ){

  GtkListStore *store;

  store = GTK_LIST_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW (SelectedAttributesView)));
  gtk_list_store_clear(store);

}

/*! \brief Function filter_list_set_default
 *  \par Function Description: This is a Group 2 support function that
 *       restore the filter list in the right Treeview using a pre-compiled
 *       string array.
 */
static void filter_list_set_default( void )
{
  int response = generic_confirm_dialog("Clear attributes and restore default filter list?");
  if (response) {
    clear_attributes();
    load_tree_view_str(GTK_TREE_VIEW (SelectedAttributesView), View2DefaultData);
  }
}

/* ------------------------ Button Support Functions ------------------------*/
/*! \brief Function butt_responder
 *  \par Function Description: This callback function is used to execute
 *       support functions that manipulate the Attribute Viewtrees.
 */
static
void butt_responder(GtkWidget *widget, GdkEventButton *event, ControlID *Control)
{
  if (GTK_IS_BUTTON(widget)) {

    int WhatHappend = event->type;
    int WhichButt = (int)(long*) Control;

    if(WhatHappend == GDK_BUTTON_PRESS) /* refraining switch (event->type) */
      switch ( WhichButt ) {
        case AddAttribute:
          add_selected_attribute();
          break;
        case RemoveAttribute:
          remove_selected_attribute();
          break;
        case ClearAttributes:
          /*  ignore, do not delete on a single-click */
          break;
        case IncreaseAttribute:
          increment_selected_attribute();
          break;
        case DecreaseAttribute:
          decrement_selected_attribute();
          break;
        case DefaultAttributes:
          filter_list_set_default();
          break;
        default:
          s_log_message("button_responder(): UKNOWN BUTTON ID: %d\n", WhichButt);
      } /* End Switch */
    else
      if (WhatHappend == GDK_2BUTTON_PRESS)
        switch ( WhichButt ) {
          case ClearAttributes:
            clear_attributes(); /* Now okay, confirmed with double-click */
            break;
          default:
            break;
        } /* End Switch */
  }
}
/* -------------------------- End Attributes Support ------------------------*/
/* ---------------------------- End Button Support --------------------------*/

/* ------------------------ ComboBox Support Functions ----------------------*/

/*! \brief Function combo_responder
 *  \par Function Description: This callback function is used to set the
 *       sensitivity of other controls based on combo-box selections.
 */
void combo_responder(GtkWidget *widget, gpointer data)
{
  int WhichComboBox = GPOINTER_TO_UINT (data);
/*
  int row;
  row = gtk_combo_box_get_active (GTK_COMBO_BOX (widget));
 */

  switch ( WhichComboBox ) {
  case TitleBlock:
    break;
  case ColorMapScheme:
    break;
  case DotGridMode:
    if (gtk_combo_box_get_active (GTK_COMBO_BOX (DotGridModeCombo)) == DOTS_GRID_VARIABLE_MODE)
       gtk_widget_set_sensitive (DotGridThresholdSpin, FALSE);
    else
       gtk_widget_set_sensitive (DotGridThresholdSpin, TRUE);
    break;
  case LogWindowType:
    break;
  case UndoType:
    break;
  case ThirdButton:
  case MiddleButton:
    break;
  default:
    s_log_message("combo_responder(): Warning, Uknown Combo Id: %d\n",WhichComboBox);
  }

 return;
}
/*! \brief setup_titleblock_combo loads combo box with list of title-blocks
 *  \par Function Description: This function allocates and arrays and calls
 *       get_titleblock_list to get a list of the sym files in the title-block
 *       folder, the name of files are appended to the combo-box without the
 *       .sym extension, after adding a "None" options. If the current title
         block is found then the combo is activated to this entry.
 *
 *  @param[in]  char *titleblock    ptr to name of current default titleblock.
 */
int setup_titleblock_combo( char *titleblock ){

  int i;
  int pos = -1;

  int number_of_buffers;
  char **strBuffer;

  /* Add option to disable automatic addition of a title-block */
  GTK_LOAD_COMBO (TitleBlock, "None");

  number_of_buffers = get_titleblock_cnt(); /* get count of files */

  strBuffer = malloc(number_of_buffers * sizeof(char *));
  if (strBuffer) {
     for (i=0; i<number_of_buffers; i++) {
       strBuffer[i] = malloc( MAX_FILENAME ); /* be 64 */
     }

     get_titleblock_list(strBuffer); /* get list of files */

     /* Maybe someone really smart can fix */
     sort_string_array(strBuffer, sizeof(strBuffer));

     remove_ext_from_basename(titleblock);

     i = 0; 
     while (i < number_of_buffers){
        if (strequal(titleblock, strBuffer[i])) pos = i;
        LOAD_STD_COMBO (TitleBlock, strBuffer[i++]);
     }
     if (pos >= 0) {
       pos++;               /* add 1 extra because we added "None"*/
       SetCombo (TitleBlock, pos); /* set the entry field */
     }

     for (i=0; i<number_of_buffers; i++) {
       free(strBuffer[i]);
     }
     free(strBuffer);
     strBuffer = NULL;
  }
  else
     s_log_message("setup_titleblock: Memory allocation error\n");

  return pos;
}
/* -------------------------- End Combo Box Support -------------------------*/

/* --------------------- Multi Control Callback Resonders -------------------*/

/*! \brief Function to toggle radio images
 *  \par Function Description: This function changes the images of
 *       controls created with create_geda_switch to the opposite
 *       state, i.e. if ON use OFF image and if OFF use ON image.
 */
static void
radio_responder(GtkWidget *widget,  gint response, ControlID *Control)
{
  bool state = GET_SWITCH_STATE (widget);

  if ((GTK_IS_BUTTON(widget)) && (state != TRUE)) {
    
    bulb_on(widget);

    switch ( response ) {
    /* General TAB */
      case LogDestinyWindow:
        bulb_off(LogDestinyTTYRadio); bulb_off(LogDestinyBothRadio);
        break;
      case LogDestinyTTY:
        bulb_off(LogDestinyWindowRadio); bulb_off(LogDestinyBothRadio);
        break;
      case LogDestinyBoth:
        bulb_off(LogDestinyWindowRadio); bulb_off(LogDestinyTTYRadio);
        break;
   /* Edit TAB */
      case NetEndPointNone:
        bulb_off(NetEndPointFilledRadio); bulb_off(NetEndPointEmptyRadio);
        break;
      case NetEndPointFilled:
        bulb_off(NetEndPointNoneRadio); bulb_off(NetEndPointEmptyRadio);
        break;
      case NetEndPointEmpty:
        bulb_off(NetEndPointNoneRadio); bulb_off(NetEndPointFilledRadio);
        break;
      case NetMidPointNone:
        bulb_off(NetMidPointFilledRadio); bulb_off(NetMidPointEmptyRadio);
        break;
      case NetMidPointFilled:
        bulb_off(NetMidPointNoneRadio); bulb_off(NetMidPointEmptyRadio);
        break;
      case NetMidPointEmpty:
        bulb_off(NetMidPointNoneRadio); bulb_off(NetMidPointFilledRadio);
        break;
      case NetSelectionNone:
        bulb_off(NetSelectionNetRadio); bulb_off(NetSelectionAllRadio);
        break;
      case NetSelectionNet:
        bulb_off(NetSelectionNoneRadio); bulb_off(NetSelectionAllRadio);
        break;
      case NetSelectionAll:
        bulb_off(NetSelectionNoneRadio); bulb_off(NetSelectionNetRadio);
        break;
  /* Styles TAB */
      case BusStyleNone:
        bulb_off(BusStyleThinRadio); bulb_off(BusStyleThickRadio);
        break;
      case BusStyleThin:
        bulb_off(BusStyleNoneRadio); bulb_off(BusStyleThickRadio);
        break;
      case BusStyleThick:
        bulb_off( BusStyleNoneRadio); bulb_off(BusStyleThinRadio);
        break;
      case NetStyleNone:
        bulb_off(NetStyleThinRadio); bulb_off(NetStyleThickRadio);
        break;
      case NetStyleThin:
        bulb_off(NetStyleNoneRadio); bulb_off(NetStyleThickRadio);
        break;
      case NetStyleThick:
        bulb_off( NetStyleNoneRadio); bulb_off(NetStyleThinRadio);
        break;
      case LineStyleNone:
        bulb_off(LineStyleThinRadio); bulb_off(LineStyleThickRadio);
        break;
      case LineStyleThin:
        bulb_off(LineStyleNoneRadio); bulb_off(LineStyleThickRadio);
        break;
      case LineStyleThick:
        bulb_off( LineStyleNoneRadio); bulb_off(LineStyleThinRadio);
        break;
      case PinStyleNone:
        bulb_off(PinStyleThinRadio); bulb_off(PinStyleThickRadio);
        break;
      case PinStyleThin:
        bulb_off(PinStyleNoneRadio); bulb_off(PinStyleThickRadio);
        break;
      case PinStyleThick:
        bulb_off( PinStyleNoneRadio); bulb_off(PinStyleThinRadio);
        break;
  /* Text TAB */
      case CapsStyleLower:
        bulb_off(CapsStyleUpperRadio); bulb_off(CapsStyleBothRadio);
        break;
      case CapsStyleUpper:
        bulb_off(CapsStyleLowerRadio); bulb_off(CapsStyleBothRadio);
        break;
      case CapsStyleBoth:
        bulb_off( CapsStyleLowerRadio); bulb_off(CapsStyleUpperRadio);
        break;
      case TextFeedbackDefault:
        bulb_off(TextFeedbackReadableRadio); bulb_off(TextFeedbackAlwaysRadio);
        break;
      case TextFeedbackReadable:
        bulb_off(TextFeedbackDefaultRadio); bulb_off(TextFeedbackAlwaysRadio);
        break;
      case TextFeedbackAlways:
        bulb_off(TextFeedbackDefaultRadio); bulb_off(TextFeedbackReadableRadio);
        break;
  /* Windows TAB */
      case GridDotSizeOne:
        bulb_off(GridDotSizeTwoRadio); bulb_off(GridDotSizeThreeRadio);
        break;
      case GridDotSizeTwo:
        bulb_off(GridDotSizeOneRadio); bulb_off(GridDotSizeThreeRadio);
        break;
      case GridDotSizeThree:
        bulb_off(GridDotSizeOneRadio); bulb_off(GridDotSizeTwoRadio);
        break;
      case GridModeNone:
        bulb_off(GridModeDotsRadio); bulb_off(GridModeMeshRadio);
        break;
      case GridModeDots:
        bulb_off(GridModeNoneRadio); bulb_off(GridModeMeshRadio);
        break;
      case GridModeMesh:
        bulb_off(GridModeNoneRadio); bulb_off(GridModeDotsRadio);
        break;
      case WindowSizeW650H487:       bulb_off(WindowSizeW900H650Radio);
        bulb_off(WindowSizeW950H712Radio); bulb_off(WindowSizeW1100H825Radio);
        break;
      case WindowSizeW900H650:       bulb_off(WindowSizeW650H487Radio);
        bulb_off(WindowSizeW950H712Radio); bulb_off(WindowSizeW1100H825Radio);
        break;
      case WindowSizeW950H712:       bulb_off(WindowSizeW650H487Radio);
        bulb_off(WindowSizeW900H650Radio); bulb_off(WindowSizeW1100H825Radio);
        break;
      case WindowSizeW1100H825:      bulb_off(WindowSizeW650H487Radio);
        bulb_off(WindowSizeW900H650Radio); bulb_off(WindowSizeW950H712Radio);
        break;
      case WorldSizeSmall:
        bulb_off(WorldSizeMediumRadio); bulb_off(WorldSizeLargeRadio);
        break;
      case WorldSizeMedium:
        bulb_off(WorldSizeSmallRadio); bulb_off(WorldSizeLargeRadio);
        break;
      case WorldSizeLarge:
        bulb_off(WorldSizeSmallRadio); bulb_off(WorldSizeMediumRadio);
        break;
      case DialogListAttributesAll:
        enable_attribute_list_controls(FALSE);
        bulb_off(DialogListAttributesNoneRadio); bulb_off(DialogListAttributesListRadio);
        break;
      case DialogListAttributesNone:
        enable_attribute_list_controls(FALSE);
        bulb_off(DialogListAttributesAllRadio); bulb_off(DialogListAttributesListRadio);
        break;
      case DialogListAttributesList:
        bulb_off(DialogListAttributesAllRadio); bulb_off(DialogListAttributesNoneRadio);
        enable_attribute_list_controls(TRUE);
        break;
      default:
        break;
    }
  }

  return;
}

/*! \brief Function to toggle switch images
 *  \par Function Description: This function changes the images of
 *       controls created with create_geda_switch to the opposite
 *       state, i.e. if ON use OFF image and if OFF use ON image.
 *       The functions enables or disables other widgets based on
 *       the state of the switch.
 */
static void switch_responder(GtkWidget *widget, gint response,  ControlID *Control)
{
   gboolean state = GET_SWITCH_STATE (widget);
   GtkWidget* SwitchImage = get_geda_switch_image( state);
   gtk_button_set_image(GTK_BUTTON (widget), SwitchImage);

   switch ( response ) {
   case AutoLoad:
     break;
   case AutoSave:
     gtk_widget_set_sensitive (AutoSaveIntervalSpin, state);
     break;
   case ClassicWheel:
   case ConsolidateNets:
   case ContinuePlace:
   case DelayScrolling:
   case DragMove:
   case DrawGrips:
   case EmbedComponents:
     break;
   case EnableLog:
     enable_log_controls (state );
     break;
   case EnableUndo:
     enable_undo_controls (state);
     break;
   case EnforceHierarchy:
   case FastMousePan:
   case FeedbackMode:
   case ForceBoundingBox:
   case FilePreview:
     break;
   case FriendlyColorMap:
     enable_color_map_controls (state);
     break;
   case FriendlyOutlineMap:
     enable_color_map_scheme(state);
     break;
   case InitLogWindow:
   case MagneticNets:
   case NetDirection:
   case NotifyEvents:
   case ObjectClipping:
   case RubberNets:
   case ScrollBars:
     gtk_widget_set_sensitive (DelayScrollingSwitch, state);
     break;
   case SortLibrary:
   case SpareSwitch:
   case TextOriginMarker:
   case UndoViews:
   case WarpCursor:
   case ZoomPan:
    break;
   default:
    s_log_message("toggle_switch(): UKNOWN SWITCH ID: %d\n", response);
   }

   return;
}
/* ------------------- End Multi Control Callback Resonders -----------------*/

/** @} END Group X_Settings_Dialog_Support_Functions */

/* \defgroup X_Settings_Dialog_Load_Variables
 *  @{
 */
/*! \brief Function load_settings_dialog
 *  \par Function Description: This function sets the value of controls when
 *       when the dialog begins based on the current settings, in so much as
 *       possible. Some configurations options are SCM scripts and we have
 *       to find a way to deal with them ...
 */
bool load_settings_dialog (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  extern gchar *x_color_get_name(int index);
  COLOR *cflag;

  /* variable pre-conditioning */

  /* Some variables have 0 base but do not includes all
   * counting numbers between 0 and the next value. The
   * groups for the radio widgets are zero based Glist.
   */

  int net_selection = w_current->net_selection_mode;
  net_selection = net_selection == 0 ? 0 : (net_selection -1);

  int dot_size = w_current->dots_grid_dot_size -1;

  /* For color-maps */
  if (abs(cmap_flag)  == 1) {
    rc_options.display_color_map = TRUE;
    if (cmap_flag < 0 )
      rc_options.display_outline_color_map = TRUE;
    else {
      rc_options.display_outline_color_map = FALSE;
    }
  }
  else {
    rc_options.display_color_map = FALSE;
    rc_options.display_outline_color_map = FALSE;
  }

/* Combo Boxes (7) */

 /* Using value for green gun @ index 10 . (both mapping and outline
    must be on if a color map was loaded)
    00 = default mapping also BW
    ff = dark 
    ee = light

    /TODO: Really should check all indexes
 */

  cflag = x_color_lookup(10); /* index of "bus" */
  if (cflag->g == 0xff) {
    rc_options.color_scheme_index = 0;
  }
  else {
    if (cflag->g == 0xee) {
      rc_options.color_scheme_index = 1;
    }
    else {
      if (cflag->g == 0)
        rc_options.color_scheme_index = 2;
      else {
        rc_options.color_scheme_index = 3;
      }
    }
  }
  SetCombo ( ColorMapScheme, rc_options.color_scheme_index);

  SetCombo ( DotGridMode, w_current->dots_grid_mode);
  SetCombo ( LogWindowType, log_window_type);
  SetCombo ( ThirdButton, w_current->third_button);
  SetCombo ( MiddleButton, w_current->middle_button);

#ifdef DEBUG
  LOAD_COMBO_STR( TitleBlock, DefaultTitleBlockList )
  SetCombo (TitleBlock, 1)
#else
  /* make C variable on stack */
  setup_titleblock_combo(scm_2_cstring( "default-titleblock" ));
  rc_options.titleblock_index = gtk_combo_box_get_active (GTK_COMBO_BOX (TitleBlockCombo));
#endif

  SetCombo ( UndoType, w_current->undo_type );

  strcpy(rc_options.untitled_name, w_current->toplevel->untitled_name);
  gtk_entry_set_text (GTK_ENTRY (UntitledNameEntry), _(rc_options.untitled_name));

/* The Switches Alphabetically (31) */

  SetSwitch(AutoLoad, auto_load_last);
  SetSwitch(AutoSave, toplevel->auto_save_interval);
  SetSwitch(EnableUndo, w_current->undo_control);
  SetSwitch(ClassicWheel, w_current->scroll_wheel);
  SetSwitch(ConsolidateNets, toplevel->net_consolidate);
  SetSwitch(ContinuePlace, w_current->continue_component_place);
  SetSwitch(DelayScrolling, w_current->scrollbar_update);
  SetSwitch(DragMove, w_current->drag_can_move);
  SetSwitch(DrawGrips, w_current->draw_grips);
  SetSwitch(EmbedComponents, w_current->embed_components);

  SetSwitch(EnableLog, logging);

  SetSwitch(EnforceHierarchy, w_current->enforce_hierarchy);
  SetSwitch(FastMousePan, w_current->fast_mousepan);

  SetSwitch(FeedbackMode, w_current->action_feedback_mode);              // was text_feedback
  SetSwitch(ForceBoundingBox, w_current->toplevel->force_boundingbox);   //was action_feedback_mode

  SetSwitch(FilePreview, w_current->file_preview);

  SetSwitch(FriendlyColorMap, rc_options.display_color_map);
  SetSwitch(FriendlyOutlineMap, rc_options.display_outline_color_map);

  SetSwitch(InitLogWindow, log_window);
  SetSwitch(MagneticNets, w_current->magnetic_net_mode);
  SetSwitch(NetDirection, w_current->net_direction_mode);
  SetSwitch(NotifyEvents, w_current->raise_dialog_boxes);
  SetSwitch(ObjectClipping, w_current->toplevel->object_clipping);
  SetSwitch(RubberNets, w_current->netconn_rubberband);
  SetSwitch(ScrollBars, w_current->scrollbars);
  SetSwitch(SortLibrary, w_current->sort_component_library);
  SetSwitch(SpareSwitch, FALSE);
  SetSwitch(TextOriginMarker, w_current->text_origin_marker);
  SetSwitch(UndoViews, w_current->undo_panzoom);
  SetSwitch(WarpCursor, w_current->warp_cursor);
  SetSwitch(ZoomPan, w_current->zoom_with_pan);

/* Radio (15) */
  // General TAB
  SetBulbGroup (LogDestiny, log_destiny);
  /// Edit Tab
  SetBulbGroup (NetEndPoint, w_current->net_endpoint_mode);
  SetBulbGroup (NetMidPoint, w_current->net_midpoint_mode);

  SetBulbGroup (NetSelection, net_selection);

  // Styles TAB
  SetBulbGroup ( BusStyle,  toplevel->bus_style);
  SetBulbGroup ( NetStyle,  toplevel->net_style);
  SetBulbGroup ( LineStyle, toplevel->line_style);
  SetBulbGroup ( PinStyle,  toplevel->pin_style);

  // Text TAB
  SetBulbGroup ( CapsStyle, w_current->text_case);
  SetBulbGroup ( TextFeedback, w_current->text_feedback);

  // Windows TAB
  SetBulbGroup ( GridDotSize, dot_size);
  SetBulbGroup ( GridMode, w_current->grid_mode);
  
  if ((default_window_width == 650) && (default_window_height == 487))
    rc_options.window_size = 0;
  else
    if ((default_window_width == 900) && (default_window_height == 650))
      rc_options.window_size = 1;
    else
      if ((default_window_width == 950) && (default_window_height == 712))
        rc_options.window_size = 2;
      else
        if ((default_window_width == 1100) && (default_window_height == 825))
          rc_options.window_size = 3;
        else
          rc_options.custom_window_size = 1;

  SetBulbGroup ( WindowSize, rc_options.window_size);

  if (default_init_right == 61 * 1000)
    rc_options.world_size = 0;
  else
    if (default_init_right == 121 * 1000)
      rc_options.world_size = 1;
    else
      if (default_init_right == 181 * 1000)
        rc_options.world_size = 2;
      else
        rc_options.custom_world_size = 1;

  SetBulbGroup ( WorldSize, rc_options.world_size);

/* Attributes TAB */
  SetBulbGroup(DialogListAttributes, GetAttributeFilterMode(w_current));

/* The Spin Controls Alphabetically (23) */
  SetSpin (AttributeOffset, w_current->add_attribute_offset);
  SetSpin (AutoPlacementGrid, w_current->autoplace_attributes_grid);
  SetSpin (AutoSaveInterval, w_current->toplevel->auto_save_interval);
  SetSpin (DotGridThreshold, w_current->dots_grid_fixed_threshold);
  SetSpin (KeyboardPanGain, w_current->keyboardpan_gain);
  SetSpin (MeshGridThreshold, w_current->mesh_grid_threshold);
  SetSpin (MousePanGain, w_current->mousepan_gain);
  SetSpin (ScrollPanSteps, w_current->scrollpan_steps);
  SetSpin (SelectPixels, w_current->select_slack_pixels);
  SetSpin (SnapSize, w_current->snap_size);
  SetSpin (TextSize, w_current->text_size);
  SetSpin (TextSnapSize,  w_current->snap_size);
  SetSpin (TextZoomFactor, w_current->text_display_zoomfactor);

  SetSpin (ThickBusWidth,  toplevel->thick_bus_width);
  SetSpin (ThickLineWidth, toplevel->thick_line_width);
  SetSpin (ThickNetWidth,  toplevel->thick_net_width);
  SetSpin (ThickPinWidth,  toplevel->thick_pin_width);

  SetSpin (ThinBusWidth,  toplevel->thin_bus_width);
  SetSpin (ThinLineWidth, toplevel->thin_line_width);
  SetSpin (ThinNetWidth,  toplevel->thin_net_width);
  SetSpin (ThinPinWidth,  toplevel->thin_pin_width);

  SetSpin (UndoBufferSize, w_current->undo_levels);
  SetSpin (ZoomGain, w_current->zoom_gain);

  return TRUE;
}

/** @} END Group X_Settings_Dialog_load_Variables */

/* \defgroup X_Settings_Dialog
 *  @{
 */
/* Note! Local Module Main, before "macrolization" the function
 * create_settings_dialog was > 148kbytes. The macros somewhat
 * obsure base coding but is much more manageable then a 150K
 * lines of gtk_xxx's and does not depend on Glade.
 */
GtkWidget*
create_settings_dialog (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *ThisDialog;
  GtkWidget *MainDialogVBox;
  GtkWidget *notebook;

  GtkWidget *IncreaseAttributeButt;
  GtkWidget *DecreaseAttributeButt;

  GtkWidget *dialog_action_area;
  GtkWidget *CancelButt;
  GtkWidget *SaveButt;
  GtkWidget *OkayButt;

  GtkTooltips *tooltips;
  tooltips = gtk_tooltips_new ();

  ThisDialog=NEW_STD_GSCHEM_DIALOG( DialogTitle, DialogSettings, w_current);
  MainDialogVBox = GTK_DIALOG (ThisDialog)->vbox;
  gtk_widget_show (MainDialogVBox);
  notebook = gtk_notebook_new ();
  gtk_widget_show (notebook);
  gtk_box_pack_start (GTK_BOX (MainDialogVBox), notebook, TRUE, TRUE, 0);

  { /*------------------- Start General TAB Contents -------------------*/
   GTK_START_TAB (General);
     VSECTION (GeneralTab_vbox, GeneralAutoOptions) /* GT Grp 1 Auto Options */
       HSECTION (GeneralAutoOptions_vbox, AutoOptionsRow1);   /* Grp 1 Row 1 */
           GTK_SWITCH (AutoOptionsRow1_hbox, AutoSave, 51, TRUE)
           GTK_NUMERIC_SPIN (AutoOptionsRow1_hbox, AutoSaveInterval, 48, 180, 60, 3600)
       HSECTION (GeneralAutoOptions_vbox, AutoOptionsRow2)    /* Grp 1 Row 2 */
           GTK_SWITCH(AutoOptionsRow2_hbox, AutoLoad, 50, TRUE);
           GTK_TEXT_ENTRY(AutoOptionsRow2_hbox, UntitledName, 48, DEFAULT_UNTITLED_NAME)
     VPSECTION (GeneralTab_vbox, GeneralOptions, 0)  /* GT Grp 2 General Options */
       HSECTION (GeneralOptions_vbox, GeneralOptionsRow1)     /* Grp 2 Row 1 */
           GTK_SWITCH(GeneralOptionsRow1_hbox, FilePreview, 18, TRUE);
           GTK_SWITCH(GeneralOptionsRow1_hbox, FriendlyColorMap, 120, TRUE);
       HSECTION (GeneralOptions_vbox, GeneralOptionsRow2)     /* Grp 2 Row 2 */
           GTK_SWITCH(GeneralOptionsRow2_hbox, NotifyEvents, 8, FALSE);
           GTK_SWITCH(GeneralOptionsRow2_hbox, FriendlyOutlineMap, 108, TRUE);
       HSECTION (GeneralOptions_vbox, GeneralOptionsRow3)     /* Grp 2 Row 3 */
           GTK_NEW_COMBO (GeneralOptionsRow3_hbox, TitleBlock, 200, 41);
           GTK_NEW_COMBO (GeneralOptionsRow3_hbox, ColorMapScheme, 150, 9);
              GTK_LOAD_COMBO (ColorMapScheme, "dark");
              GTK_LOAD_COMBO (ColorMapScheme, "light");
              GTK_LOAD_COMBO (ColorMapScheme, "BW");
              GTK_LOAD_COMBO (ColorMapScheme, "custom");
     HXYP_SEPERATOR (GeneralTab_vbox, Grp3, 10);
     CSECTION_OPTIONS(GeneralTab_vbox, Logging, -1, 10, H); /* GT Grp 3 Log Related */
       VSECTION (LoggingOptions_hbox, LogOptions);   /* Grp 3 Row 1 */
         GTK_SWITCH(LogOptions_vbox, EnableLog, 5, TRUE);
         GTK_SWITCH(LogOptions_vbox, InitLogWindow, 0, FALSE);
         GTK_NEW_COMBO (LogOptions_vbox, LogWindowType, 150, 5);
           GTK_LOAD_COMBO (LogWindowType, RC_STR_LOGWIN_DECORATED)
           GTK_LOAD_COMBO (LogWindowType, RC_STR_LOGWIN_TRANSIENT)
         GTK_V_BULB_TRIAD (LoggingOptions_hbox, LogDestiny, 50, Window, TTY, Both, Window);
     HXYP_SEPERATOR (GeneralTab_vbox, Grp4, 10);
     CSECTION_OPTIONS(GeneralTab_vbox, Undo, 70, 10, H); /* was GT Grp 4 Undo Related */
       VSECTION (UndoOptions_hbox, UndoToggleOptions);
         GTK_SWITCH(UndoToggleOptions_vbox, EnableUndo, 0, TRUE);
         GTK_SWITCH(UndoToggleOptions_vbox, UndoViews, 0, FALSE);
         VXP_SEPERATOR(UndoOptions_hbox, UndoToggleOptions, 60);
         VSECTION (UndoOptions_hbox, UndoExtraOptions);
           GTK_NUMERIC_SPIN (UndoExtraOptions_vbox, UndoBufferSize, 0, 10, 1, 99);
           GTK_NEW_COMBO (UndoExtraOptions_vbox, UndoType, 150, 5); //pad 39
             GTK_LOAD_COMBO (UndoType, RC_STR_UNDO_DISK)
             GTK_LOAD_COMBO (UndoType, RC_STR_UNDO_MEMORY)
     HXYP_SEPERATOR (GeneralTab_vbox, End, 10);
   GTK_END_TAB(General);

  } /*** END General TAB Contents ***/
 
  { /*------------------- Start Edit TAB Contents -------------------*/

   GTK_START_TAB (Edit);
     VSECTION (EditTab_vbox, EditOptions) /* ET Grp 1 Auto Options */
       HSECTION (EditOptions_vbox, EditOptionsRow1)  /* Grp 1 Row 1 */
         GTK_SWITCH(EditOptionsRow1_hbox, DrawGrips, 42, TRUE);
         GTK_NUMERIC_SPIN (EditOptionsRow1_hbox, SelectPixels, 91, 10, 0, 20);
       HSECTION (EditOptions_vbox, EditOptionsRow2)  /* Grp 1 Row 2 */
         GTK_SWITCH(EditOptionsRow2_hbox, FeedbackMode, 9, TRUE);
         GTK_NUMERIC_SPIN (EditOptionsRow2_hbox, KeyboardPanGain, 124, 20, 0, 100);
       HSECTION (EditOptions_vbox, EditOptionsRow3)   /* Grp 1 Row 3 */
         GTK_SWITCH(EditOptionsRow3_hbox, ContinuePlace, 0, TRUE);
         GTK_NUMERIC_SPIN (EditOptionsRow3_hbox, SnapSize, 175, 100, 0, 500);
       HSECTION (EditOptions_vbox, EditOptionsRow4)   /* Grp 1 Row 4 */
         GTK_SWITCH(EditOptionsRow4_hbox, ForceBoundingBox, 20, TRUE);
         GTK_SWITCH(EditOptionsRow4_hbox, SpareSwitch, 124, TRUE);
       HSECTION (EditOptions_vbox, EditOptionsRow5)    /* Grp 1 Row 4 */
         GTK_SWITCH(EditOptionsRow5_hbox, WarpCursor, 30, FALSE);
         GTK_SWITCH(EditOptionsRow5_hbox, ObjectClipping, 82, TRUE);
     HYP_SEPERATOR (EditTab_vbox, Grp2, 10);
     CSECTION_OPTIONS(EditTab_vbox, Nets, 20, DIALOG_V_SPACING, H); /*ET Grp 2 Edit Nets */
       VSECTION(NetsOptions_hbox, NetsGroup1) /* ET Grp 1 Net Selection Options */
         GTK_SWITCH(NetsGroup1_vbox, NetDirection, DEFAULT_WIDGET_SPACING, TRUE);
         GTK_SWITCH(NetsGroup1_vbox, MagneticNets, DEFAULT_WIDGET_SPACING, TRUE);
         GTK_SWITCH(NetsGroup1_vbox, RubberNets, DEFAULT_WIDGET_SPACING, TRUE);
         GTK_SWITCH(NetsGroup1_vbox, ConsolidateNets, 0, TRUE);
       VSECTION(NetsOptions_hbox, NetsGroup2) /* ET Grp 2 Net Selection Options */
         GTK_H_BULB_TRIAD( NetsGroup2_vbox, NetEndPoint,  None, Empty, Filled, Filled);
         GTK_H_BULB_TRIAD( NetsGroup2_vbox, NetMidPoint,  None, Empty, Filled, Filled);
         GTK_H_BULB_TRIAD( NetsGroup2_vbox, NetSelection, None, Net, All, Net);
   GTK_END_TAB(Edit);
  } /*** END Edit TAB Contents ***/

  { /*------------------- Start Pointer TAB Contents -------------------*/
   GTK_START_TAB (Pointer);
     VSECTION(PointerTab_vbox, PointerOptions) /* PT Solo Grp 1 */
       HSECTION (PointerOptions_vbox, PointerRow1)    /* Row 1 */
         GTK_SWITCH(PointerRow1_hbox, ZoomPan, 62, FALSE);
         GTK_NUMERIC_SPIN (PointerRow1_hbox, ZoomGain, 72, 20, 0, 100);
       HSECTION (PointerOptions_vbox, PointerRow2)    /* Row 1 */
         GTK_SWITCH(PointerRow2_hbox, FastMousePan, 41, FALSE);
         GTK_NUMERIC_SPIN (PointerRow2_hbox, MousePanGain, 38, 1, 1, 30);
       HSECTION (PointerOptions_vbox, PointerRow3)    /* Row 3 */
         GTK_SWITCH(PointerRow3_hbox, DragMove, 54, TRUE);
         GTK_NUMERIC_SPIN (PointerRow3_hbox, ScrollPanSteps, 0, 8, 0, 100);
       HSECTION (PointerOptions_vbox, PointerRow4)    /* Row 4 */
         GTK_SWITCH(PointerRow4_hbox, ClassicWheel, 8, TRUE);
         GTK_NEW_COMBO (PointerRow4_hbox, ThirdButton, 150, 20);
         gtk_widget_set_size_request (ThirdButtonCombo, 150, 20);
         GTK_LOAD_COMBO (ThirdButton, RC_STR_3RD_POPUP)
         GTK_LOAD_COMBO (ThirdButton, RC_STR_3RD_PAN)
       HSECTION (PointerOptions_vbox, PointerRow5)    /* Row 4 */
         GTK_NEW_COMBO (PointerRow5_hbox, MiddleButton, 150, 266);
         gtk_widget_set_size_request (MiddleButtonCombo, 150, 20);
         GTK_LOAD_COMBO (MiddleButton, RC_STR_MID_STROKE)
         GTK_LOAD_COMBO (MiddleButton, RC_STR_MID_REPEAT)
         GTK_LOAD_COMBO (MiddleButton, RC_STR_MID_ACTION)
         GTK_LOAD_COMBO (MiddleButton, RC_STR_MID_MOUSEPAN)
     HXYP_SEPERATOR (PointerTab_vbox, End, 10);
   GTK_END_TAB(Pointer);
  } /*** END Pointer TAB Contents ***/

  { /*------------------- Start Window TAB Contents -------------------*/

   GTK_START_TAB (Window);
     HSECTION (WindowTab_vbox, DisplaySizeOptions) /* WT Row 1 Display Size */
       GTK_V_QUAD_BULB(DisplaySizeOptions_hbox, WindowSize, 10, W650H487, W900H650, W950H712, W1100H825, W950H712)
       GTK_V_BULB_TRIAD( DisplaySizeOptions_hbox, WorldSize, 20, Small, Medium, Large, Medium);
     HD_SEPERATOR (WindowTab_vbox, Grp2);
       HPSECTION(WindowTab_vbox, GridOptions, DIALOG_V_SPACING) /* WT Row 2 */
         GTK_V_BULB_TRIAD( GridOptions_hbox, GridMode, 0, None, Dots, Mesh, Mesh);
         VPSECTION(GridOptions_hbox, GridDotOptions, 60) /* WT Row 2 Grp 2 Dot Grid Options */
           GTK_NEW_COMBO (GridDotOptions_vbox, DotGridMode, 150, DIALOG_V_SPACING);
           GTK_LOAD_COMBO (DotGridMode, RC_STR_DOTS_MODE_VARIABLE)
           GTK_LOAD_COMBO (DotGridMode, RC_STR_DOTS_MODE_FIXED)
           GTK_NUMERIC_SPIN (GridDotOptions_vbox, DotGridThreshold, DIALOG_V_SPACING, DEFAULT_GRID_DOT_SIZE, MIN_GRID_DOT_SIZE, MAX_GRID_DOT_THRESHOLD);
     HD_SEPERATOR (WindowTab_vbox, Grp3);
       HSECTION(WindowTab_vbox, MeshGridSizeOptions) /* WT Row 3 */
         GTK_V_BULB_TRIAD (MeshGridSizeOptions_hbox, GridDotSize, 0, One, Two, Three, One);
         GTK_NUMERIC_SPIN (MeshGridSizeOptions_hbox, MeshGridThreshold, 105, DEFAULT_GRID_MESH_THRESHOLD, MIN_GRID_MESH_THRESHOLD, MAX_GRID_MESH_THRESHOLD);
     HD_SEPERATOR (WindowTab_vbox, Grp4);
       HSECTION(WindowTab_vbox, WindowScrollOptions) /* WT Grp 4  */
         GTK_SWITCH(WindowScrollOptions_hbox, ScrollBars, 35, TRUE);
         GTK_SWITCH(WindowScrollOptions_hbox, DelayScrolling, 77, FALSE);
     HD_SEPERATOR (WindowTab_vbox, End);
   GTK_END_TAB(Window);
  } /*** END Window TAB Contents ***/

  { /*-------------------- Start Text TAB Contents --------------------*/

   GTK_START_TAB (Text);
     VSECTION(TextTab_vbox, TextOptionsGrp1);
       GTK_NUMERIC_SPIN (TextOptionsGrp1_vbox, TextSize, DIALOG_V_SPACING, DEFAULT_TEXT_SIZE, MIN_TEXT_SIZE, MAX_TEXT_SIZE);
       GTK_NUMERIC_SPIN (TextOptionsGrp1_vbox, TextZoomFactor, DIALOG_V_SPACING, 30, 0, 100);
       GTK_NUMERIC_SPIN (TextOptionsGrp1_vbox,  TextSnapSize, DIALOG_V_SPACING, 100, 0, 500);
       GTK_SWITCH(TextOptionsGrp1_vbox, TextOriginMarker, DIALOG_V_SPACING, TRUE);
     HD_SEPERATOR (TextTab_vbox, Grp2);
     HSECTION (TextTab_vbox, CapsStyleOptions)   /* TT Grp 2 Text Styles */
       GTK_V_BULB_TRIAD(CapsStyleOptions_hbox, CapsStyle, DIALOG_H_SPACING, Lower, Upper, Both, Both);
     HD_SEPERATOR (TextTab_vbox, Grp3);
     HSECTION (TextTab_vbox, TextOptionsGrp3) /* TT Grp 3 Feedback */
       GTK_V_BULB_TRIAD(TextOptionsGrp3_hbox, TextFeedback, DIALOG_H_SPACING, Readable, Always, Default, Readable);
   GTK_END_TAB(Text);
  } /***  END Text TAB Contents ***/

  { /*-------------------- Start Styles TAB Contents --------------------*/
   GTK_START_TAB (Styles);
     HD_SEPERATOR (StylesTab_vbox, Grp1);
     HSECTION(StylesTab_vbox, StylesRow1); /* ST Grp 1 Bus and Net */
       GTK_V_BULB_TRIAD( StylesRow1_hbox, BusStyle, 5, None, Thin, Thick, Thin);
       VSECTION (StylesRow1_hbox, BusWidths)  /* ST Grp 1 Bus Spinners */
         GTK_NUMERIC_SPIN (BusWidths_vbox, ThinBusWidth, DIALOG_V_SPACING +5, DEFAULT_THIN_BUS_WIDTH, 0, 100);
         GTK_NUMERIC_SPIN (BusWidths_vbox, ThickBusWidth, DIALOG_V_SPACING +5, DEFAULT_THICK_BUS_WIDTH, 0, 500);
       VXP_SEPERATOR (StylesRow1_hbox, Row1, 15);
       GTK_V_BULB_TRIAD( StylesRow1_hbox, NetStyle, 5, None, Thin, Thick, Thin);
       VPSECTION (StylesRow1_hbox, NetWidths, DIALOG_V_SPACING)  /* ST Grp 1 Net Spinners */
         GTK_NUMERIC_SPIN (NetWidths_vbox, ThinNetWidth, DIALOG_V_SPACING +5, DEFAULT_THIN_NET_WIDTH, 0, 100);
         GTK_NUMERIC_SPIN (NetWidths_vbox, ThickNetWidth, DIALOG_V_SPACING +5, DEFAULT_THICK_NET_WIDTH, 0, 500);
     HD_SEPERATOR (StylesTab_vbox, Grp2);
     HSECTION(StylesTab_vbox, StylesRow2); /* ST Grp 2 Lines and Pins */
       GTK_V_BULB_TRIAD( StylesRow2_hbox, LineStyle, 5, None, Thin, Thick, Thin);
       VSECTION (StylesRow2_hbox, LineWidths)  /* ST Grp 2 Lines Spinners */
         GTK_NUMERIC_SPIN (LineWidths_vbox, ThinLineWidth, DIALOG_V_SPACING +5, DEFAULT_THIN_LINE_WIDTH, 0, 100);
         GTK_NUMERIC_SPIN (LineWidths_vbox, ThickLineWidth, DIALOG_V_SPACING +5, DEFAULT_THICK_LINE_WIDTH, 0, 500);
       VXP_SEPERATOR (StylesRow2_hbox, Row2, 15);
       GTK_V_BULB_TRIAD( StylesRow2_hbox, PinStyle, 5, None, Thin, Thick, Thin);
       VPSECTION (StylesRow2_hbox, PinWidths, DIALOG_V_SPACING)  /* ST Grp 2 Pin Spinners */
         GTK_NUMERIC_SPIN (PinWidths_vbox, ThinPinWidth, DIALOG_V_SPACING +5, DEFAULT_THIN_PIN_WIDTH, 0, 100);
         GTK_NUMERIC_SPIN (PinWidths_vbox, ThickPinWidth, DIALOG_V_SPACING +5, DEFAULT_THICK_PIN_WIDTH, 0, 500);
     HD_SEPERATOR (StylesTab_vbox, End);
   GTK_END_TAB(Styles);
  } /*** END Styles TAB Contents ***/

  { /*------------------ Start Attribute TAB Contents ------------------*/
   GTK_START_TAB (Attributes);
    VSECTION (AttributesTab_vbox, AttributeDialogList);  /* Grp 1 Attribute List */
       GTK_HV_BULB_TRIAD (AttributeDialogList_vbox, DialogListAttributes, All, None, List, All);
       //GTK_ICALLBACK_RTRIAD (DialogListAttributes, attributes_radio_group, All, None, List)
       HSECTION (AttributeDialogList_vbox, AttributeLists); /* Still Grp 1 but now Row 2 */
         VPSECTION (AttributeLists_hbox, AttributeListing, 40); /* Still Grp 1, Row 2 Col 1*/
         GTK_NEW_SCROLL_OUT (AttributeListing_vbox, AttributesListWindow, 15, 190, 220, GTK_POLICY_AUTOMATIC)
           GTK_VIEW_TREE (AttributesListWindow, PotentialAttributes, View1Data, str, 180, -1);
             gtk_tree_view_set_enable_search (GTK_TREE_VIEW (PotentialAttributesView), TRUE);
           HSECTION(AttributeListing_vbox, VerticalListControls);
             GTK_STD_BUTTON (VerticalListControls_hbox, IncreaseAttribute)
             GTK_STD_BUTTON (VerticalListControls_hbox, DecreaseAttribute)
           VZSECTION (AttributeLists_hbox, HorizontalListControls, 115, 200);
             GTK_STD_BUTTON (HorizontalListControls_vbox, AddAttribute)
             GTK_STD_BUTTON (HorizontalListControls_vbox, RemoveAttribute)
             GTK_STD_BUTTON (HorizontalListControls_vbox, ClearAttributes)
             GTK_STD_BUTTON (HorizontalListControls_vbox, DefaultAttributes)
         GTK_NEW_SCROLL_OUT (AttributeLists_hbox, SelectedAttributesWindow, 35, 190, -1, GTK_POLICY_AUTOMATIC)
         GTK_VIEW_TREE (SelectedAttributesWindow, SelectedAttributes, View2Data, gl, 180, -1)
           gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (SelectedAttributesView), FALSE);
       HYP_SEPERATOR (AttributesTab_vbox, Grp2, 5);
       HSECTION(AttributesTab_vbox,  AttributeOptions); /* Grp 2 but now Row 2 */
         GTK_NUMERIC_SPIN (AttributeOptions_hbox, AttributeOffset, 25, 50, 0, 300);
         GTK_NUMERIC_SPIN (AttributeOptions_hbox, AutoPlacementGrid, 25, 1, 0, 300);
       HYP_SEPERATOR (AttributesTab_vbox, End, 5);
   GTK_END_TAB(Attributes);

  } /*** END Attribute TAB Contents ***/

  { /*-------------------- Start Library TAB Contents --------------------*/

   GTK_START_TAB (Library);
     VSECTION(LibraryTab_vbox, LibraryOptionsRow1)
       GTK_SWITCH(LibraryOptionsRow1_vbox, EnforceHierarchy, DIALOG_V_SPACING, FALSE);
       GTK_SWITCH(LibraryOptionsRow1_vbox, EmbedComponents, DIALOG_V_SPACING, FALSE);
       GTK_SWITCH(LibraryOptionsRow1_vbox, SortLibrary, DIALOG_V_SPACING, FALSE);
     HXYP_SEPERATOR (LibraryTab_vbox, End, 10);
   GTK_END_TAB(Library);
  } /*** END Library TAB Contents ***/

  dialog_action_area = GTK_DIALOG (ThisDialog)->action_area;
  gtk_widget_show (dialog_action_area);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area), GTK_BUTTONBOX_END);

  CancelButt = gtk_button_new_from_stock ("gtk-cancel");
  gtk_widget_show (CancelButt);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), CancelButt, GTK_RESPONSE_CANCEL);
  GTK_WIDGET_SET_FLAGS (CancelButt, GTK_CAN_DEFAULT);
  gtk_tooltips_set_tip (tooltips, CancelButt, _("Close without changing any settings"), NULL);

  SaveButt = gtk_button_new_with_mnemonic (_("Save"));
  gtk_widget_show (SaveButt);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), SaveButt, GTK_RESPONSE_APPLY);
  GTK_WIDGET_SET_FLAGS (SaveButt, GTK_CAN_DEFAULT);
  gtk_tooltips_set_tip (tooltips, SaveButt, _("Close and Write settings to disk"), NULL);

  OkayButt = gtk_button_new_from_stock ("gtk-ok");
  gtk_widget_show (OkayButt);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), OkayButt, GTK_RESPONSE_OK);
  GTK_WIDGET_SET_FLAGS (OkayButt, GTK_CAN_DEFAULT);
  gtk_tooltips_set_tip (tooltips, OkayButt, _("Change settings and close but do not write settings to storage.."), NULL);

  /* Store pointers to widgets, for use by lookup_widget(). */
  GTK_HOOKUP_OBJECT_NO_REF (ThisDialog, ThisDialog, "ThisDialog");
  GTK_HOOKUP_OBJECT_NO_REF (ThisDialog, MainDialogVBox, "MainDialogVBox");
  GTK_HOOKUP_OBJECT (ThisDialog, notebook, "notebook");
  GTK_HOOKUP_OBJECT_NO_REF (ThisDialog, dialog_action_area, "dialog_action_area");
  GTK_HOOKUP_OBJECT (ThisDialog, CancelButt, "CancelButt");
  GTK_HOOKUP_OBJECT (ThisDialog, SaveButt, "SaveButt");
  GTK_HOOKUP_OBJECT (ThisDialog, OkayButt, "OkayButt");
  GTK_HOOKUP_OBJECT_NO_REF (ThisDialog, tooltips, "tooltips");

  gtk_widget_grab_default (CancelButt);

  g_signal_connect ((gpointer) notebook, "switch-page",
                    G_CALLBACK (on_notebook_switch_page),
                    NULL);
  return ThisDialog;
}
/** @} END Group X_Settings_Dialog */

/* \defgroup X_Settings_Dialog_Unload_Variables
 *  @{
 */
/*! \brief Post Dialog procedure to retrieves values in dialog controls.
 *  \par Function Description
 *       This function retrieves and saves the values from all widgets. The
 *       values are saved to either the memory variables from which they were
 *       loaded or to the rc_options structure.
 */
void GatherSettings(GSCHEM_TOPLEVEL *w_current) {

  TOPLEVEL *toplevel = w_current->toplevel;
  int tmp_int;
  char *tmpstr;

  /** @brief function change_default_titleblock in GatherSettings */
  void change_default_titleblock() {
    char expr [MAX_FILENAME] = "(define default-titleblock \"";
         strcat(expr, rc_options.titleblock_fname );
         strcat(expr, SYMBOL_FILE_DOT_SUFFIX);
         strcat(expr, "\")" );
         g_scm_c_eval_string_protected(expr);
  }
  /* Next line assigns a pointer to a char field in a GTK Entry widget - do not free it! */
  tmpstr = (char *)gtk_entry_get_text (GTK_ENTRY (UntitledNameEntry));
  strcpy(rc_options.untitled_name, tmpstr );
  strcpy(toplevel->untitled_name, rc_options.untitled_name);

/* Combo Boxes (7) */

  w_current->dots_grid_mode = gtk_combo_box_get_active (GTK_COMBO_BOX (DotGridModeCombo));
  log_window_type           = gtk_combo_box_get_active (GTK_COMBO_BOX (LogWindowTypeCombo));
  w_current->undo_type      = gtk_combo_box_get_active (GTK_COMBO_BOX (UndoTypeCombo));
  w_current->middle_button  = gtk_combo_box_get_active (GTK_COMBO_BOX (MiddleButtonCombo));
  w_current->third_button   = gtk_combo_box_get_active (GTK_COMBO_BOX (ThirdButtonCombo));

  tmp_int = gtk_combo_box_get_active (GTK_COMBO_BOX (ColorMapSchemeCombo));
  if (tmp_int != rc_options.color_scheme_index) { /* if user changed this settings */
    rc_options.color_scheme_index = tmp_int;
    switch ( tmp_int ) {
    case 0:
      x_load_color_scheme(DARK_COLOR_MAP);  /* call for load the Dark map */
      break;
    case 1:
      x_load_color_scheme(LIGHT_COLOR_MAP); /* call for load the Light map */
      break;
    case 2:
      x_load_color_scheme(BW_COLOR_MAP);    /* call for load the Blk/Wht map */
      break;
    case 3:
      x_load_color_scheme(CUSTOM_COLOR_MAP);  /* call to load the custom map */
      break;
    default:
      x_load_color_scheme(rc_options.color_map_scheme);  /* call for load custom map */
    }
  } /* else do nothing because the map did not change */

  tmp_int = gtk_combo_box_get_active (GTK_COMBO_BOX (TitleBlockCombo));

  if (tmp_int != rc_options.titleblock_index) {
    if (tmp_int != 0 ) { /* and if was not set to None */
      /* Next line gets a pointer to a dynamic char array - Must be freed! */
      tmpstr = gtk_combo_box_get_active_text (GTK_COMBO_BOX (TitleBlockCombo));
      strcpy(rc_options.titleblock_fname, tmpstr); /* save the filename */
      strcat(rc_options.titleblock_fname, SYMBOL_FILE_DOT_SUFFIX);
      g_free(tmpstr);
      change_default_titleblock();
    }
    else {
      strcpy(rc_options.titleblock_fname, ""); /* write empty quote */
      scm_eval_string(scm_from_utf8_string("(define default-titleblock \"\")"));
    }
    rc_options.titleblock_index = -1; /* set flag to indicate new default_titleblock */
  }
/* The Switches Alphabetically (31) */
             auto_load_last             = GET_SWITCH_STATE (AutoLoadSwitch);
  w_current->continue_component_place   = GET_SWITCH_STATE (ContinuePlaceSwitch);
  w_current->drag_can_move              = GET_SWITCH_STATE (DragMoveSwitch);
  w_current->draw_grips                 = GET_SWITCH_STATE (DrawGripsSwitch);
  w_current->embed_components           = GET_SWITCH_STATE (EmbedComponentsSwitch);
  w_current->enforce_hierarchy          = GET_SWITCH_STATE (EnforceHierarchySwitch);
  w_current->fast_mousepan              = GET_SWITCH_STATE (FastMousePanSwitch);
  w_current->action_feedback_mode       = GET_SWITCH_STATE (FeedbackModeSwitch);
   toplevel->force_boundingbox          = GET_SWITCH_STATE (ForceBoundingBoxSwitch);

  w_current->file_preview               = GET_SWITCH_STATE (FilePreviewSwitch);

  rc_options.display_color_map          = GET_SWITCH_STATE (FriendlyColorMapSwitch);
  rc_options.display_outline_color_map  = GET_SWITCH_STATE (FriendlyOutlineMapSwitch);

  w_current->magnetic_net_mode          = GET_SWITCH_STATE (MagneticNetsSwitch);
   toplevel->net_consolidate            = GET_SWITCH_STATE (ConsolidateNetsSwitch);
  w_current->net_direction_mode         = GET_SWITCH_STATE (NetDirectionSwitch);

  w_current->netconn_rubberband         = GET_SWITCH_STATE (RubberNetsSwitch);
             logging                    = GET_SWITCH_STATE (EnableLogSwitch);
             log_window                 = GET_SWITCH_STATE (InitLogWindowSwitch);
   toplevel->object_clipping            = GET_SWITCH_STATE (ObjectClippingSwitch);
  w_current->raise_dialog_boxes         = GET_SWITCH_STATE (NotifyEventsSwitch);
  w_current->scrollbars                 = GET_SWITCH_STATE (ScrollBarsSwitch);
  w_current->scrollbar_update           = GET_SWITCH_STATE (DelayScrollingSwitch);
  w_current->scroll_wheel               = GET_SWITCH_STATE (ClassicWheelSwitch);
  w_current->sort_component_library     = GET_SWITCH_STATE (SortLibrarySwitch);
  w_current->text_origin_marker         = GET_SWITCH_STATE (TextOriginMarkerSwitch);
  w_current->undo_control               = GET_SWITCH_STATE (EnableUndoSwitch);
  w_current->undo_panzoom               = GET_SWITCH_STATE (UndoViewsSwitch);
  w_current->warp_cursor                = GET_SWITCH_STATE (WarpCursorSwitch);
  w_current->zoom_with_pan              = GET_SWITCH_STATE (ZoomPanSwitch);

/* The Spin Controls Alphabetically (23) */
  w_current->add_attribute_offset       =GET_SPIN_IVALUE (AttributeOffsetSpin);
  w_current->autoplace_attributes_grid  =GET_SPIN_IVALUE (AutoPlacementGridSpin);
                                tmp_int = GET_SWITCH_STATE (AutoSaveSwitch);
   toplevel->auto_save_interval         = tmp_int == 0 ? 0 : GET_SPIN_IVALUE (AutoSaveIntervalSpin);
  w_current->dots_grid_fixed_threshold  =GET_SPIN_IVALUE (DotGridThresholdSpin);
  w_current->keyboardpan_gain           =GET_SPIN_IVALUE (KeyboardPanGainSpin);
  w_current->mesh_grid_threshold        =GET_SPIN_IVALUE (MeshGridThresholdSpin);
  w_current->mousepan_gain              =GET_SPIN_IVALUE (MousePanGainSpin);
  w_current->scrollpan_steps            =GET_SPIN_IVALUE (ScrollPanStepsSpin);
  w_current->select_slack_pixels        =GET_SPIN_IVALUE (SelectPixelsSpin);
  w_current->snap_size        		=GET_SPIN_IVALUE (SnapSizeSpin);
  w_current->text_size                  =GET_SPIN_IVALUE (TextSizeSpin);
/* w_current->toplevel->snap_size        =GET_SPIN_IVALUE (TextSnapSizeSpin); */
  w_current->text_display_zoomfactor    =GET_SPIN_IVALUE (TextZoomFactorSpin);

   toplevel->thick_bus_width            =GET_SPIN_IVALUE (ThickBusWidthSpin);
   toplevel->thick_line_width           =GET_SPIN_IVALUE (ThickLineWidthSpin); 
   toplevel->thick_net_width            =GET_SPIN_IVALUE (ThickNetWidthSpin); 
   toplevel->thick_pin_width            =GET_SPIN_IVALUE (ThickPinWidthSpin); 

   toplevel->thin_bus_width             =GET_SPIN_IVALUE (ThinBusWidthSpin); 
   toplevel->thin_line_width            =GET_SPIN_IVALUE (ThinLineWidthSpin); 
   toplevel->thin_net_width             =GET_SPIN_IVALUE (ThinNetWidthSpin); 
   toplevel->thin_pin_width             =GET_SPIN_IVALUE (ThinPinWidthSpin);

  w_current->undo_levels                =GET_SPIN_IVALUE (UndoBufferSizeSpin);
  w_current->zoom_gain                  =GET_SPIN_IVALUE (ZoomGainSpin);

/* The Radio Widgets Alphabetically (13) */
   toplevel->bus_style = gtk_radio_group_get_active(BusStyleRadioGroup);

/* Temporarily deviating from alphabetical sequence */
  toplevel->line_style = gtk_radio_group_get_active(LineStyleRadioGroup);
  toplevel->net_style  = gtk_radio_group_get_active(NetStyleRadioGroup);
  toplevel->pin_style  = gtk_radio_group_get_active(PinStyleRadioGroup);

/* Resume alphabetical sequence */
                                tmp_int = gtk_radio_group_get_active(GridDotSizeRadioGroup);
  w_current->dots_grid_dot_size         = 1 + tmp_int;

  w_current->grid_mode                  = gtk_radio_group_get_active(GridModeRadioGroup);

  log_destiny                           = gtk_radio_group_get_active(LogDestinyRadioGroup);
  w_current->net_endpoint_mode          = gtk_radio_group_get_active(NetEndPointRadioGroup);
  w_current->net_midpoint_mode          = gtk_radio_group_get_active(NetMidPointRadioGroup);
                                tmp_int = gtk_radio_group_get_active(NetSelectionRadioGroup);
  w_current->net_selection_mode         = tmp_int == NET_NONE ? NET_NONE : (tmp_int +1);
  w_current->text_case                  = gtk_radio_group_get_active(CapsStyleRadioGroup);
                                tmp_int = gtk_radio_group_get_active(TextFeedbackRadioGroup);
  w_current->text_feedback              = tmp_int == 2 ? ONLY_WHEN_READABLE : tmp_int;

  rc_options.window_size                = gtk_radio_group_get_active(WindowSizeRadioGroup);
  rc_options.world_size                 = gtk_radio_group_get_active(WorldSizeRadioGroup);

/* Retrieve data from Liststores in Views */
   SavePotentialAttributes(w_current);
   SaveAttributeFilterList(w_current); //SelectedAttributes

}
/** @} END Group X_Settings_Dialog_Unload_Variables */

