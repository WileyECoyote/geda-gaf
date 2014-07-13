/* C
 * File: x_settings_dialog.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2014 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2012-2014 gEDA Contributors (see ChangeLog for details)
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
 * Date: Aug, 17, 2012
 * Contributing Author: Wiley Edward Hill
 *
*/
/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 09/17/12 |  Inital release.
 * ------------------------------------------------------------------
 * WEH | 12/02/12 |  Added call to x_settings_set_scm_int in function
 *                |  GatherSettings (to support renaming of autoplace-
 *                |  attribute-grid to attribute_placement_grid).
 * ------------------------------------------------------------------
 * WEH | 12/04/12 |  Added switch for EnableColorImaging
 * ------------------------------------------------------------------
 * WEH | 12/30/12 |  Changed "Log" to "Console"
 * ------------------------------------------------------------------
 * WEH | 01/06/13 | Added spinner RipperSize, switches RipperRotation
 *                | RipperType, and combo RipperSymbol, (to extend
 *                | functionality)
 * ------------------------------------------------------------------
 * WEH | 07/20/13 | Added Font Name Combo (to extend functionality)
 * ------------------------------------------------------------------
 * WEH | 09/20/13 | Added PointerCurso Combo (to extend functionality)
 * ------------------------------------------------------------------
 * WEH | 09/25/13 | Added GripStrokeColor, GripFillColor,TextMarkerColor,
 *                | TextOriginMarker, TextMarkerSize, JunctionColor,
 *                | TextMarkerSize and JunctionSize, NetEndpointColor,
 *                | ScrollBarsVisible
 * ------------------------------------------------------------------
 * WEH | 03/13/14 | Fix: case JunctionColor did not have break in
 *                | color_butt_responder, oops.
 *
*/
/*! \remarks To add a new variable or control:
 *
 * 1. The variable should be valid and readable in the RC system, but
 *    this is not a requirement to add the widget.
 *
 * 2. Create the control
 *
 *      a.) Delare widget variable in the section Global Variables
 *      b.) Add the widget in the create_settings_dialog function.
 *      c.) Added a WidgetStringData record to struct in the file
 *          x_settings_dialog.h, see instructions at the top of the
 *          header.
 *      d.) Add any necassary support functions and code to the
 *          responder, i.e. the existing callback function
 *      e.) If there is an error such as "Unknown button Id" then
 *          there is a generic/common responder with a switch/case
 *          that does not have a case for the new control being
 *          added, see step 2.d.
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
 *    (At this point the new variable and control should be displayed
 *     and working properly.)
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

#include <gschem_xdefines.h>            /* Define dialog default internal spacing */
#include <gschem_dialog.h>              /* Definition the base Dialog Class */
#include <geda_dialog_controls.h>       /* Macros for Dialogs */
#include <geda_widgets.h>               /* Switches use geda_labels */
#include <x_settings.h>                 /* Common Declarations and Enumerators */
#include <x_settings_dialog.h>          /* Dialog String Data */

const char* IDS_FONT_NAMES[] = {  /* Menu Icons Strings*/
  DEFAULT_FONT_NAME, "Courier New", "Helvetica", "Monospace", "Tahoma", "Verdana",
  NULL
};

/* ---------------  Functions that Should Be Somewhere Else  --------------- */

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
  /* new buttons are *prepended* to the list, so buttons added first
   * in the last positions in the list and using glist reverse
   * confuses gtk */
  return ((length - 1) - active);
}

/* You would have thought . . . */
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
        0,  /* ripper_symbol_index */
        "gschem-colormap-darkbg", /* color_map_scheme file name [MAX_FILE]*/
        "",                       /* untitled_name[MAX_FILE] */
        "",                       /* titleblock_fname[64]; */
        ""};                      /* ripper_symbol_fname [MAX_FILE]*/

/* The Global Widgets */
static GtkWidget *AddAttributeButt=NULL;
static GtkWidget *RemoveAttributeButt=NULL;
static GtkWidget *ClearAttributesButt=NULL;
static GtkWidget *DefaultAttributesButt=NULL;
//static GtkWidget *ConfirmClearCheckBox=NULL;

/* The Color Buttons */
static GtkWidget *GripStrokeColorButt;
static GtkWidget *GripFillColorButt;
static GtkWidget *JunctionColorButt;
static GtkWidget *NetEndpointColorButt;
static GtkWidget *TextMarkerColorButt;

/* The Combo Boxes */
static GtkWidget *ColorMapSchemeCombo;
static GtkWidget *DotGridModeCombo;
static GtkWidget *ConsoleWindowTypeCombo;
static GtkWidget *PointerCursorCombo;
static GtkWidget *MiddleButtonCombo;
static GtkWidget *ThirdButtonCombo;
static GtkWidget *TitleBlockCombo;
static GtkWidget *UndoTypeCombo;
static GtkWidget *FontNameCombo;
static GtkWidget *RipperSymbolCombo;

/* The one and only Text Entry Box */
static GtkWidget *UntitledNameEntry;

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
static GtkWidget *AttributeOffsetSpin;
static GtkWidget *AutoPlacementGridSpin;
static GtkWidget *AutoSaveIntervalSpin;
static GtkWidget *DotGridThresholdSpin;
static GtkWidget *GripPixelSizeSpin;
static GtkWidget *JunctionSizeSpin;
static GtkWidget *KeyboardPanGainSpin;
static GtkWidget *KeyboardPanGainSpin;
static GtkWidget *MeshGridThresholdSpin;
static GtkWidget *MousePanGainSpin;
static GtkWidget *RipperSizeSpin;
static GtkWidget *ScrollPanStepsSpin;
static GtkWidget *SelectPixelsSpin;
static GtkWidget *SnapSizeSpin;
static GtkWidget *TextMarkerSizeSpin;
static GtkWidget *TextSizeSpin;
static GtkWidget *TextZoomFactorSpin;
static GtkWidget *ThickBusWidthSpin;
static GtkWidget *ThickLineWidthSpin;
static GtkWidget *ThickNetWidthSpin;
static GtkWidget *ThickPinWidthSpin;
static GtkWidget *ThinBusWidthSpin;
static GtkWidget *ThinLineWidthSpin;
static GtkWidget *ThinNetWidthSpin;
static GtkWidget *ThinPinWidthSpin;
static GtkWidget *UndoBufferSizeSpin;
static GtkWidget *ZoomGainSpin;

/* The Switches */
static GtkWidget *AutoLoadSwitch=NULL;
static GtkWidget *AutoSaveSwitch=NULL;
static GtkWidget *ClassicWheelSwitch=NULL;
static GtkWidget *ConsolidateNetsSwitch=NULL;
static GtkWidget *ContinuePlaceSwitch=NULL;
static GtkWidget *DelayScrollingSwitch=NULL;
static GtkWidget *DragMoveSwitch=NULL;
static GtkWidget *DrawGripsSwitch=NULL;
static GtkWidget *EmbedComponentsSwitch=NULL;
static GtkWidget *EnableColorImagingSwitch=NULL;
static GtkWidget *EnableLogSwitch=NULL;
static GtkWidget *EnableUndoSwitch=NULL;
static GtkWidget *EnforceHierarchySwitch=NULL;
static GtkWidget *FastMousePanSwitch=NULL;
static GtkWidget *FeedbackModeSwitch=NULL;
static GtkWidget *ForceBoundingBoxSwitch=NULL;
static GtkWidget *FilePreviewSwitch=NULL;
static GtkWidget *FriendlyColorMapSwitch=NULL;
static GtkWidget *FriendlyOutlineMapSwitch=NULL;
static GtkWidget *InitConsoleWindowSwitch=NULL;
static GtkWidget *InvertImagesSwitch=NULL;
static GtkWidget *MagneticNetsSwitch=NULL;
static GtkWidget *NetDirectionSwitch=NULL;
static GtkWidget *NotifyEventsSwitch=NULL;
static GtkWidget *ObjectClippingSwitch=NULL;
static GtkWidget *PointerHScrollSwitch=NULL;
static GtkWidget *RipperRotationSwitch=NULL;
static GtkWidget *RipperTypeSwitch=NULL;
static GtkWidget *RubberNetsSwitch=NULL;
static GtkWidget *ScrollBarsSwitch=NULL;
static GtkWidget *ScrollBarsVisibleSwitch=NULL;
static GtkWidget *SortLibrarySwitch=NULL;
static GtkWidget *TextOriginMarkerSwitch=NULL;
static GtkWidget *UndoViewsSwitch=NULL;
static GtkWidget *WarpCursorSwitch=NULL;
static GtkWidget *ZoomPanSwitch=NULL;

static GtkWidget *PotentialAttributesView=NULL;
static GtkWidget *SelectedAttributesView=NULL;

/** @} END Group X_Settings_Dialog_Globals */

/* --------------------------- Support Functions --------------------------- */

/*! \defgroup X_Settings_Dialog_Support_Functions X Settings Dialog Support Functions
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
        1. enable_attribute_list_controls        called in callback functions
        2. enable_color_map_controls
        3. enable_color_map_scheme
        4. enable_log_controls
        5. enable_undo_controls
        6. on_notebook_switch_page                is a callback handler
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
  gtk_widget_set_sensitive (InitConsoleWindowSwitch, state);
  gtk_widget_set_sensitive (ConsoleWindowTypeCombo, state);
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
 *       This function is called when ever a TAB sheet is selected. This
 *       allows all sensitivities on sheet to be checked and set properly
 *       in case settings on one sheet effect setting on another.
 */
static void
on_notebook_switch_page (GtkNotebook *notebook, GtkNotebookPage *page,
                         guint        page_num, gpointer    user_data)
{
  bool state;
  switch ( page_num ) {
  case GeneralPref:
    state = GET_SWITCH_STATE (AutoSaveSwitch);
      gtk_widget_set_sensitive (AutoSaveIntervalSpin, state);
    state = GET_SWITCH_STATE (EnableUndoSwitch);
      enable_undo_controls (state);
    state = GET_SWITCH_STATE (FriendlyColorMapSwitch);
      enable_color_map_controls (state);
    state = GET_SWITCH_STATE (FriendlyOutlineMapSwitch);
      enable_color_map_scheme(state);
    break;
  case EditPref:
    state = GET_SWITCH_STATE (DrawGripsSwitch);
      gtk_widget_set_sensitive (GripPixelSizeSpin,   state);
      gtk_widget_set_sensitive (GripStrokeColorButt, state);
      gtk_widget_set_sensitive (GripFillColorButt,   state);
    break;
  case PointerPref:
    break;
  case WindowPref:
    if (gtk_combo_box_get_active (GTK_COMBO_BOX (DotGridModeCombo)) == DOTS_GRID_VARIABLE_MODE)
       gtk_widget_set_sensitive (DotGridThresholdSpin, FALSE);
    else
       gtk_widget_set_sensitive (DotGridThresholdSpin, TRUE);

    state = GET_SWITCH_STATE (ScrollBarsSwitch);
    gtk_widget_set_sensitive (ScrollBarsVisibleSwitch, state);
    gtk_widget_set_sensitive (DelayScrollingSwitch, state);
    break;
  case TextPref:
  case StylesPref:
    state = GET_SWITCH_STATE (RipperTypeSwitch);
    gtk_widget_set_sensitive (RipperSymbolCombo, state);
    break;
  case AttributesPref:
    state = GET_SWITCH_STATE (DialogListAttributesListRadio);
    enable_attribute_list_controls (state);
    break;
  case LibraryPref:
    break;
  default:
    BUG_IMSG( "Unknown TAB Id", page_num);
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
    GEDA_FREE(value);
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
 * \remarks
 *  This function does not do anything yet, could add/remove
 *  attribute and bypass buttons or what?
 */
static void
st_tree_row_activated (GtkTreeView       *tree_view,
                       GtkTreePath       *path,
                       GtkTreeViewColumn *column,
                       GtkWidget         *Dialog)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

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
 *  @param[in]  Dialog       is really w_current.
 *  @param[in]  TreeTextView A Treeview widge.
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

  GEDA_UNREF(store);
}

/*  \par
 *
 *  Abstract:
 *
 *  There are two attribute list referenced in the initialization file.
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
 *  The array of string can be NULL, in which case strings are
 *  retieved using s_attrib_get. The only "list" passed to this
 *  function is the default list that is used when the configuration
 *  data is missing.
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
 *       component_select_attrlist, which should not be confused with the
 *       state indicated in the dialog (after the user has changed/clicked
 *       the radio/bulb widgets)
 *
 *  \retval 0        = Filter All    // rc entry had an
 *          1        = No Filter     // rc entry had empty list
 *          2        = Filter List   // rc entry had and actual list
 */
static int GetAttributeFilterMode(GschemToplevel *w_current) {

  char* data;

  data = g_list_nth_data (View2Data, 0);  /* 0 could also be the last */
  /* return 1 if empty, else return 0 if char is asterisk, else return 2 */
  return data == NULL ? 1 : (data[0] == ASCII_ASTERISK ? 0 : 2 );
}

/*! \brief Function SaveAttributeFilterList
 *  \par Function Description: This is a Group 2 support function that
 *       clears the current attribute "filter" list, creates a new list
 *       or modified the old list based on the Dialog settings
 */
static int SaveAttributeFilterList(GschemToplevel *w_current) {
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
       /* We went thru the new list and index is still set, so if there
          are any more members in the old list they need to be removed */
       while (index < list_length) {
         str_old = g_list_nth_data (View2Data, index);
         View2Data = g_list_remove (View2Data, str_old);
         GEDA_FREE (str_old);
         index ++;
       }
       break;
     default:
       BUG_MSG("DialogListAttributesRadioGroup returned bad ID\n");
   }
  /* Don't GEDA_FREE (str_new) here because it's pointing
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
static int SavePotentialAttributes(GschemToplevel *w_current) {
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
 *       used to check is a given attribute is already in the filter
 *       Treeview.
 *
 *  @param[in]  list  The Treeview list to look in
 *  @param[in]  str   String to look for
 */
static bool is_not_in_list(GtkTreeView *list, const char *str)
{
  GtkListStore *store;
  gboolean answer = FALSE;

  gboolean foreach_func (GtkTreeModel *model, GtkTreePath  *path,
                         GtkTreeIter  *iter,  gpointer      user_data)
  {
    char *attribute;

    /* Note: here we use 'iter' and not '&iter', because we did not allocate
     * the iter on the stack and are already getting the pointer to a tree iter */
    gtk_tree_model_get (model, iter, 0, &attribute, -1);

    answer = strequal( str, attribute);

    GEDA_FREE(attribute); /* gtk_tree_model_get made copies of strings */
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
    if (g_tree_model_iter_previous (model, iter2))
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
 *       but only if the attribute is not already in the list. The attribute
 *       is inserted at the current selected position, or appends to end of
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

      /* check if there is a selection in right list */
      r_selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(SelectedAttributesView));

      /* if there was a selection then r_iter set else GTK set r_iter to NULL */
      if (gtk_tree_selection_get_selected( r_selection, (GtkTreeModel**) &r_store, &r_iter))
        /* if r_iter = NULL then is suppose to be appended to end but doens't seem to work */
        gtk_list_store_insert_before(r_store, &n_iter, &r_iter);
      else { /* so do this instead */
        r_store = GTK_LIST_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW (SelectedAttributesView)));
        gtk_list_store_append(r_store, &n_iter);
      }
      /* save the value to the right list */
      gtk_list_store_set(r_store, &n_iter, 0, value, -1); /* Column 0 */

    }
    GEDA_FREE(value); /* Don't free unless we use it, less we be cursed */
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
  int response = gschem_confirm_dialog("Clear attributes and restore default filter list?", GTK_MESSAGE_INFO);
  if (response == GTK_RESPONSE_YES) {
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
          BUG_IMSG( "Unknown button Id", WhichButt);
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

/*! \defgroup Popup-Menu Popmenu to restore default colors
 *  @{
*/

static GtkWidget *popup_menu; /* Seems safer to use a global for this */

/*! \brief Restore Default Color Setting - Display Menu item responder
 *  \par Function Description: Called when once of the two menu items
 *       on the "restore default color" mini menu is selected. If
*/
static void
color_button_popup_menu_callback (GtkMenuItem *item, gpointer data)
{
  bool restore_default_color;
  int  color_index;
  GtkColorButton *button;

  restore_default_color = (int)(long*)data;

  if (restore_default_color) {
    color_index = GPOINTER_TO_INT( g_object_get_data(G_OBJECT(item), "color-index"));
    button      = g_object_get_data(G_OBJECT(item), "color-button");
    gtk_color_button_set_color(button, x_get_color(color_index));
  }

  gtk_widget_destroy(popup_menu);
}

/*! \brief Restore Default Color Setting - Display Menu
 *  \par Function Description: We need used to be able to restore
 *   default colors, since the Color Selector Dialog is a child of
 *   button, only display after the button pressed, we can not easily
 *   add a "restore default" button to the action area, so instead we
 *   have the user right click on the color button and display a "mini"
 *   menu with just two choices, restore defaults or cancel. This function
 *   creates that menu and sets up the callback to the preceding function
 *   after embedding the pertinent data in the menu item.
*/
static void default_color_button_popup (GtkColorButton *button, GdkEventButton *event, int index)
{
  GtkWidget   *item;

  if (popup_menu) {
    gtk_object_destroy(GTK_OBJECT(popup_menu));
    popup_menu = NULL;
  }
  popup_menu = gtk_menu_new ();

  item = gtk_image_menu_item_new_with_label (_("Restore default"));

  g_object_set_data(G_OBJECT(item), "color-index",  GINT_TO_POINTER(index));

  g_object_set_data (G_OBJECT(item), "color-button", button);

  g_signal_connect (G_OBJECT (item), "activate",
                    G_CALLBACK (color_button_popup_menu_callback), GINT_TO_POINTER (1));

  gtk_menu_shell_append (GTK_MENU_SHELL (popup_menu), item);

  item = gtk_image_menu_item_new_with_label (_("Cancel"));

  g_signal_connect (G_OBJECT (item), "activate",
                    G_CALLBACK (color_button_popup_menu_callback), GINT_TO_POINTER (0));

  gtk_menu_shell_append (GTK_MENU_SHELL (popup_menu), item);

  gtk_widget_show_all (popup_menu);

  gtk_menu_popup(GTK_MENU(popup_menu), NULL, NULL, NULL, NULL, event->button, event->time);

}

/*! \brief Restore Default Color Setting - Check button events
 *  \par Function Description: This is callback function for all of the
 *   color buttons used in the Preferences/Settings dialog. This function
 *   intercepts mouse button press events, looking for "right clicks" on
 *   a color button, passing the all other events to the regular button
 *   event handler. If a the event was a "button 3 down" (but not release)
 *   the preceding function is called, passing the pertinent data, to
 *   display a pop-up menu.
 */
static
bool color_butt_responder(GtkWidget *widget, GdkEventButton *event, ControlID *Control)
{

  bool resolved = FALSE;

  if (GTK_IS_COLOR_BUTTON(widget)) {

    int WhatHappend = event->type;
    int WhichButt   = (int)(long*) Control;
    int color_index = -1;

    if (event->button == 3) /* only interest in right button down */
      if (WhatHappend == GDK_BUTTON_PRESS) {
       resolved = TRUE;
       switch ( WhichButt ) {
         case GripStrokeColor:
            color_index = SELECT_COLOR;
           break;
         case GripFillColor:
            color_index = BACKGROUND_COLOR;
           break;
         case NetEndpointColor:
            color_index = NET_ENDPOINT_COLOR;
           break;
         case TextMarkerColor:
            color_index = LOCK_COLOR;
           break;
         case JunctionColor:
            color_index = JUNCTION_COLOR;
           break;
         default:
            BUG_IMSG( "Unknown button Id", WhichButt);
      } /* End Switch */
      if(color_index != -1) {
        default_color_button_popup((GtkColorButton *)widget, event, color_index);
      }
    }
  }
  return resolved;
}

/* ---------------------------- End Button Support --------------------------*/
/*! @} endgroup Popup-Menu */

/* ------------------------ ComboBox Support Functions ----------------------*/

/*! \brief Function combo_responder
 *  \par Function Description: This callback function is used to set the
 *       sensitivity of other controls based on combo-box selections.
 */
void combo_responder(GtkWidget *widget, gpointer data)
{
  int WhichComboBox = (int)(long*) data;
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
  case ConsoleWindowType:
    break;
  case UndoType:
    break;
  case PointerCursor:
  case ThirdButton:
  case MiddleButton:
  case FontName:
  case RipperSymbol:
    break;
  default:
    BUG_IMSG( "Unknown Combo Id", WhichComboBox);
  }

 return;
}

#ifndef DEBUG
/*! \brief setup_titleblock_combo loads combo box with list of title-blocks
 *  \par Function Description: This function allocates and arrays and calls
 *       get_titleblock_list to get a list of the sym files in the title-block
 *       folder, the name of files are appended to the combo-box without the
 *       .sym extension, after adding a "None" options. If the current title
         block is found then the combo is activated to this entry.
 *
 *  @param[in] titleblock  ptr to name of current default titleblock.
 */
static
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
    BUG_MSG("Memory allocation error");

  return pos;
}
#endif

/*! \brief Loads Font Name Combo Box and Set Active
 *  \par Function Description:
 *   This function up loads font name strings into the FontName
 *   combobox. If one of strings matches the given font name,
 *   then that entry is set to be the active combo entry, other
 *   wise the first entry is set to be the active string.
 *
 *  @param[in] cur_font ptr to name of current font.
 */
void setup_font_name_combo(char* cur_font) {

  int current;
  int index;
  const char* pfont;

  current = 0;
  for ( index = 0; IDS_FONT_NAMES[index] != NULL; index++ ) {

    pfont = IDS_FONT_NAMES[index];
    GTK_LOAD_COMBO (FontName, pfont);
    if ( cur_font && strequal(cur_font, pfont)) {
     current = index;
    }
  }
  gtk_combo_box_set_active((GtkComboBox *)FontNameCombo, current);

}

void setup_ripper_symbol_combo(char* cur_name) {

  strcpy(rc_options.ripper_symbol_fname, cur_name);

  if (strequal(rc_options.ripper_symbol_fname, DEFAULT_BUS_RIPPER_SYMNAME))
    rc_options.ripper_symbol_index = 0;
  else
    if (strequal(rc_options.ripper_symbol_fname, SECOND_BUS_RIPPER_SYMNAME))
      rc_options.ripper_symbol_index = 1;
    else {
      LOAD_STD_COMBO(RipperSymbol, rc_options.ripper_symbol_fname);
      rc_options.ripper_symbol_index = 2;
    }

  gtk_combo_box_set_active((GtkComboBox *)RipperSymbolCombo, rc_options.ripper_symbol_index);

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
static void switch_responder(GtkWidget *widget, int response,  ControlID *Control)
{
   bool state = GET_SWITCH_STATE (widget);
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
     gtk_widget_set_sensitive (GripPixelSizeSpin,   state);
     gtk_widget_set_sensitive (GripStrokeColorButt, state);
     gtk_widget_set_sensitive (GripFillColorButt,   state);
     break;
   case EmbedComponents:
   case EnableColorImaging:
     gtk_widget_set_sensitive (InvertImagesSwitch, state);
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
   case InitConsoleWindow:
   case InvertImages:
   case MagneticNets:
   case NetDirection:
   case NotifyEvents:
   case ObjectClipping:
   case PointerHScroll:
   case RipperRotation:
     break;
   case RipperType:
     gtk_widget_set_sensitive (RipperSymbolCombo, state);
     break;
   case RubberNets:
     break;
   case ScrollBars:
     gtk_widget_set_sensitive (ScrollBarsVisibleSwitch, state);
     gtk_widget_set_sensitive (DelayScrollingSwitch,    state);
     break;
   case SortLibrary:
     break;
   case TextOriginMarker:
     gtk_widget_set_sensitive (TextMarkerSizeSpin,  state);
     gtk_widget_set_sensitive (TextMarkerColorButt, state);
     break;
   case UndoViews:
   case WarpCursor:
   case ZoomPan:
    break;
   default:
    BUG_IMSG( "Unknown switch Id", response);
   }

   return;
}
/* ------------------- End Multi Control Callback Resonders -----------------*/

/** @} END Group X_Settings_Dialog_Support_Functions */

/* \defgroup X_Settings_Dialog_Load_Variables
 *  @{
 */
/*! \brief Function load_settings_dialog
 *  \par Function Description: This function sets the value of controls after
 *       the dialog is created based on the current settings, in so much as
 *       possible. Some configurations options are SCM scripts and we have
 *       to find a way to deal with them ...
 */
bool load_settings_dialog (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  EdaConfig    *cfg      = eda_config_get_user_context ();
  const char   *group    = IVAR_CONFIG_GROUP;

  char *tmpstr;
  extern char *x_color_get_name(int index);
  const GdkColor* color;
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

  color = eda_renderer_get_grips_stroke_color (w_current->renderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(GripStrokeColorButt),  color);

  color = eda_renderer_get_grips_fill_color (w_current->renderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(GripFillColorButt),    color);

  color = eda_renderer_get_net_endpoint_color (w_current->renderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(NetEndpointColorButt), color);

  color = eda_renderer_get_text_marker_color (w_current->renderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(TextMarkerColorButt),  color);

  color = eda_renderer_get_junction_color (w_current->renderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(JunctionColorButt),    color);

/* Combo Boxes (7) */

 /* Using value for green gun @ index 10 to identify which color map
  * was loaded. (both mapping and outline must be on if a color map
  * was loaded)

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
        rc_options.color_scheme_index = 3; /* The custom map */
      }
    }
  }
  SetCombo ( ColorMapScheme, rc_options.color_scheme_index);

  SetCombo ( DotGridMode,   w_current->dots_grid_mode);
  SetCombo ( ConsoleWindowType, console_window_type);
  SetCombo ( ThirdButton,   w_current->third_button);
  SetCombo ( PointerCursor, w_current->drawing_pointer);
  SetCombo ( MiddleButton,  w_current->middle_button);

#ifdef DEBUG
  LOAD_COMBO_STR( TitleBlock, DefaultTitleBlockList );
  SetCombo (TitleBlock, 1);
#else
  /* make C variable on stack */
  tmpstr = eda_config_get_string (cfg, group, "default-titleblock", NULL);
  setup_titleblock_combo(tmpstr);
  GEDA_FREE (tmpstr);
#endif

  SetCombo ( UndoType, w_current->undo_type );

  tmpstr = eda_config_get_string (cfg, group, "default-font-name", NULL);
  setup_font_name_combo(tmpstr);
  GEDA_FREE (tmpstr);

  setup_ripper_symbol_combo(w_current->bus_ripper_symname);

  tmpstr = eda_config_get_string (cfg, group, "default-filename", NULL);
  SetEntryText( UntitledNameEntry, tmpstr );
  GEDA_FREE (tmpstr);

/* The Switches Alphabetically (31) */

  SetSwitch(AutoLoad, auto_load_last);
  SetSwitch(AutoSave, toplevel->auto_save_interval);
  SetSwitch(EnableUndo, w_current->undo_control);
  SetSwitch(ClassicWheel, w_current->scroll_wheel);
  SetSwitch(ConsolidateNets, toplevel->net_consolidate);
  SetSwitch(ContinuePlace, w_current->continue_component_place);
  SetSwitch(DelayScrolling, w_current->scrollbar_update);
  SetSwitch(DragMove, w_current->drag_can_move);
  SetSwitch(DrawGrips, w_current->renderer->draw_grips);
  SetSwitch(EmbedComponents, w_current->embed_components);
  SetSwitch(EnableColorImaging, toplevel->image_color);
  SetSwitch(EnableLog, logging);

  SetSwitch(EnforceHierarchy, w_current->enforce_hierarchy);
  SetSwitch(FastMousePan, w_current->fast_mousepan);

  SetSwitch(FeedbackMode, w_current->action_feedback_mode);    /* was text_feedback */
  SetSwitch(ForceBoundingBox, w_current->force_boundingbox);   /* was action_feedback_mode */

  SetSwitch(FilePreview, w_current->file_preview);

  SetSwitch(FriendlyColorMap, rc_options.display_color_map);
  SetSwitch(FriendlyOutlineMap, rc_options.display_outline_color_map);

  SetSwitch(InitConsoleWindow, console_window);
  SetSwitch(InvertImages, toplevel->invert_images);
  SetSwitch(MagneticNets, w_current->magnetic_net_mode);
  SetSwitch(NetDirection, w_current->net_direction_mode);
  SetSwitch(NotifyEvents, w_current->raise_dialog_boxes);
  SetSwitch(ObjectClipping, w_current->object_clipping);

  SetSwitch(PointerHScroll, w_current->pointer_hscroll);

  SetSwitch(RipperRotation, w_current->bus_ripper_rotation);
  SetSwitch(RipperType, w_current->bus_ripper_type);

  SetSwitch(RubberNets, w_current->netconn_rubberband);
  SetSwitch(ScrollBars, w_current->scrollbars);
  SetSwitch(ScrollBarsVisible, w_current->scrollbars_visible);
  SetSwitch(SortLibrary, w_current->sort_component_library);
  SetSwitch(TextOriginMarker, w_current->renderer->text_origin_marker);
  SetSwitch(UndoViews, w_current->undo_panzoom);
  SetSwitch(WarpCursor, w_current->warp_cursor);
  SetSwitch(ZoomPan, w_current->zoom_with_pan);

/* Radio (15) */
  /* General TAB */
  SetBulbGroup (LogDestiny, log_destiny);
  /* Edit Tab */
  SetBulbGroup (NetEndPoint, w_current->net_endpoint_mode);
  SetBulbGroup (NetMidPoint, w_current->net_midpoint_mode);

  SetBulbGroup (NetSelection, net_selection);

  /* Styles TAB  */
  SetBulbGroup ( BusStyle,  toplevel->bus_style);
  SetBulbGroup ( NetStyle,  toplevel->net_style);
  SetBulbGroup ( LineStyle, toplevel->line_style);
  SetBulbGroup ( PinStyle,  toplevel->pin_style);

  /* Text TAB  */
  SetBulbGroup ( CapsStyle, w_current->text_case);
  SetBulbGroup ( TextFeedback, w_current->text_feedback);

  /* Windows TAB  */
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

  if (default_world_right == 61 * 1000)
    rc_options.world_size = 0;
  else
    if (default_world_right == 121 * 1000)
      rc_options.world_size = 1;
    else
      if (default_world_right == 181 * 1000)
        rc_options.world_size = 2;
      else
        rc_options.custom_world_size = 1;

  SetBulbGroup ( WorldSize, rc_options.world_size);

/* Attributes TAB */
  SetBulbGroup(DialogListAttributes, GetAttributeFilterMode(w_current));

/* The Spin Controls Alphabetically (23) */
  SetSpin (AttributeOffset, w_current->add_attribute_offset);
  SetSpin (AutoPlacementGrid, w_current->attribute_placement_grid);
  SetSpin (AutoSaveInterval, w_current->toplevel->auto_save_interval);
  SetSpin (DotGridThreshold, w_current->dots_grid_fixed_threshold);
  SetSpin (GripPixelSize, w_current->grip_pixel_size);
  SetSpin (JunctionSize, w_current->renderer->junction_size);
  SetSpin (KeyboardPanGain, w_current->keyboardpan_gain);
  SetSpin (MeshGridThreshold, w_current->mesh_grid_threshold);
  SetSpin (MousePanGain, w_current->mousepan_gain);
  SetSpin (RipperSize, w_current->bus_ripper_size);
  SetSpin (ScrollPanSteps, w_current->scrollpan_steps);
  SetSpin (SelectPixels, w_current->select_slack_pixels);
  SetSpin (SnapSize, w_current->snap_size);
  SetSpin (TextMarkerSize, w_current->renderer->text_marker_size);
  SetSpin (TextSize, w_current->text_size);
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
 * obsures base coding but is much more manageable then 100K+
 * lines of gtk_xxx's and does not depend on Glade.
 */
GtkWidget*
create_settings_dialog (GschemToplevel *w_current)
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

  PangoFontDescription *FontDescription;
  FontDescription = pango_font_description_from_string("Monospace");
  pango_font_description_set_absolute_size(FontDescription, 10);

  ThisDialog=NEW_STD_GSCHEM_DIALOG( DialogTitle, DialogSettings, w_current);
  MainDialogVBox = GTK_DIALOG (ThisDialog)->vbox;
  g_object_set ( MainDialogVBox, "visible", TRUE, NULL);
  notebook = gtk_notebook_new ();
  g_object_set ( notebook, "visible", TRUE, NULL);
  gtk_box_pack_start (GTK_BOX (MainDialogVBox), notebook, FALSE, FALSE, 0);

  { /*------------------- Start General TAB Contents -------------------*/
   GTK_START_TAB (GeneralPref);
     VSECTION (GeneralPrefTab_vbox, GeneralAutoOptions) /* GT Grp 1 Auto Options */
       HSECTION (GeneralAutoOptions_vbox, AutoOptionsRow1);   /* Grp 1 Row 1 */
           GTK_SWITCH (AutoOptionsRow1_hbox, AutoSave, 51, TRUE)
           GTK_NUMERIC_SPIN (AutoOptionsRow1_hbox, AutoSaveInterval, 30, 180, 60, 3600)
       HSECTION (GeneralAutoOptions_vbox, AutoOptionsRow2)    /* Grp 1 Row 2 */
           GTK_SWITCH(AutoOptionsRow2_hbox, AutoLoad, 7, TRUE);
           GTK_TEXT_ENTRY(AutoOptionsRow2_hbox, UntitledName, 30, DEFAULT_UNTITLED_NAME)
     VPSECTION (GeneralPrefTab_vbox, GeneralOptions, 0)  /* GT Grp 2 General Options */
       HSECTION (GeneralOptions_vbox, GeneralOptionsRow1)     /* Grp 2 Row 1 */
           GTK_SWITCH(GeneralOptionsRow1_hbox, FilePreview, 18, TRUE);
           GTK_NEW_COMBO (GeneralOptionsRow1_hbox, TitleBlock, 200, 49);
       HSECTION (GeneralOptions_vbox, GeneralOptionsRow2)     /* Grp 2 Row 2 */
           GTK_SWITCH(GeneralOptionsRow2_hbox, EnableColorImaging, 7, FALSE);
           GTK_SWITCH(GeneralOptionsRow2_hbox, FriendlyColorMap, 114, TRUE);
       HSECTION (GeneralOptions_vbox, GeneralOptionsRow3)     /* Grp 2 Row 3 */
           GTK_SWITCH(GeneralOptionsRow3_hbox, InvertImages, 8, TRUE);
           GTK_SWITCH(GeneralOptionsRow3_hbox, FriendlyOutlineMap, 101, TRUE);
      HSECTION (GeneralOptions_vbox, GeneralOptionsRow4)     /* Grp 2 Row 4 */
           GTK_NEW_COMBO (GeneralOptionsRow4_hbox, ColorMapScheme, 150, 27);
              GTK_LOAD_COMBO (ColorMapScheme, "dark");
              GTK_LOAD_COMBO (ColorMapScheme, "light");
              GTK_LOAD_COMBO (ColorMapScheme, "BW");
              GTK_LOAD_COMBO (ColorMapScheme, "custom");
     HXYP_SEPERATOR (GeneralPrefTab_vbox, Grp3, 10);
     CSECTION_OPTIONS(GeneralPrefTab_vbox, Logging, -1, 10, H); /* GT Grp 3 Log Related */
       VSECTION (LoggingOptions_hbox, LogOptions);   /* Grp 3 Row 1 */
         GTK_SWITCH(LogOptions_vbox, EnableLog, 5, TRUE);
         GTK_SWITCH(LogOptions_vbox, InitConsoleWindow, 0, FALSE);
         GTK_NEW_COMBO (LogOptions_vbox, ConsoleWindowType, 150, 5);
           GTK_LOAD_COMBO (ConsoleWindowType, RC_STR_CONWIN_DECORATED)
           GTK_LOAD_COMBO (ConsoleWindowType, RC_STR_CONWIN_TRANSIENT)
         GTK_V_BULB_TRIAD (LoggingOptions_hbox, LogDestiny, 10, Window, TTY, Both, Window);
     HXYP_SEPERATOR (GeneralPrefTab_vbox, Grp4, 10);
     CSECTION_OPTIONS(GeneralPrefTab_vbox, Undo, 63, 5, H); /* was GT Grp 4 Undo Related */
       VSECTION (UndoOptions_hbox, UndoToggleOptions);
         GTK_SWITCH(UndoToggleOptions_vbox, EnableUndo, 0, TRUE);
         GTK_SWITCH(UndoToggleOptions_vbox, UndoViews, 0, FALSE);
         VXP_SEPERATOR(UndoOptions_hbox, UndoToggleOptions, 60);
         VSECTION (UndoOptions_hbox, UndoExtraOptions);
           GTK_NUMERIC_SPIN (UndoExtraOptions_vbox, UndoBufferSize, 0, 10, 1, 99);
           GTK_NEW_COMBO (UndoExtraOptions_vbox, UndoType, 150, 5);
             GTK_LOAD_COMBO (UndoType, RC_STR_UNDO_DISK)
             GTK_LOAD_COMBO (UndoType, RC_STR_UNDO_MEMORY)
     HXYP_SEPERATOR (GeneralPrefTab_vbox, End, 10);
   GTK_END_TAB(GeneralPref);

  } /*** END General TAB Contents ***/

  { /*------------------- Start Edit TAB Contents -------------------*/

   GTK_START_TAB (EditPref);
     VSECTION (EditPrefTab_vbox, EditOptions) /* ET Grp 1 Auto Options */
       GEDA_FRAME (EditOptions_vbox, Grips, -1, 110, 0.05, 0.2, 5)
         VSECTION (Grips_hbox, GripOptions)  /* Grp 1 Row 1 */
           HSECTION ( GripOptions_vbox, GripOptionsRow1)  /* Grp 1 Row 3 */
             GTK_SWITCH(GripOptionsRow1_hbox, DrawGrips, 34, TRUE);
             GTK_NUMERIC_SPIN (GripOptionsRow1_hbox, GripPixelSize, 45, 10, MIN_GRIP_PIXELS, MAX_GRIP_PIXELS);
           HSECTION (GripOptions_vbox, GripOptionsRow2)  /* Grp 1 Row 2 */
             GEDA_COLOR_BUTTON (GripOptionsRow2_hbox, GripStrokeColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, 34)
             GEDA_COLOR_BUTTON (GripOptionsRow2_hbox, GripFillColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, 28)
       HSECTION (EditOptions_vbox, EditOptionsRow3)  /* Grp 1 Row 3 */
         GTK_SWITCH(EditOptionsRow3_hbox, FeedbackMode, 12, TRUE);
         GTK_NUMERIC_SPIN (EditOptionsRow3_hbox, KeyboardPanGain, 13, 20, MIN_KEYBOARD_GAIN, MAX_KEYBOARD_GAIN);
       HSECTION (EditOptions_vbox, EditOptionsRow4)   /* Grp 1 Row 4 */
         GTK_SWITCH(EditOptionsRow4_hbox, ContinuePlace, 2, TRUE);
         GTK_NUMERIC_SPIN (EditOptionsRow4_hbox, SnapSize, 22, 100, MIN_SNAP_SIZE, MAX_SNAP_SIZE);
       HSECTION (EditOptions_vbox, EditOptionsRow5)   /* Grp 1 Row 5 */
         GTK_SWITCH(EditOptionsRow5_hbox, ForceBoundingBox, 22, TRUE);
         GTK_NUMERIC_SPIN (EditOptionsRow5_hbox, SelectPixels, 22, 10, 0, 20);
       HSECTION (EditOptions_vbox, EditOptionsRow6)    /* Grp 1 Row 6 */
         GTK_SWITCH(EditOptionsRow6_hbox, NotifyEvents, 12, TRUE);
         GTK_SWITCH(EditOptionsRow6_hbox, ObjectClipping, 0, TRUE);
     HYP_SEPERATOR (EditPrefTab_vbox, Grp2, 10);
     CSECTION_OPTIONS(EditPrefTab_vbox, Nets, 20, DIALOG_V_SPACING, H); /*ET Grp 2 Edit Nets */
       VSECTION(NetsOptions_hbox, NetsGroup1) /* ET Grp 1 Net Selection Options */
         GTK_SWITCH(NetsGroup1_vbox, NetDirection, DEFAULT_WIDGET_SPACING, TRUE);
         GTK_SWITCH(NetsGroup1_vbox, MagneticNets, DEFAULT_WIDGET_SPACING, TRUE);
         GTK_SWITCH(NetsGroup1_vbox, RubberNets, DEFAULT_WIDGET_SPACING, TRUE);
         GTK_SWITCH(NetsGroup1_vbox, ConsolidateNets, 0, TRUE);
       VSECTION(NetsOptions_hbox, NetsGroup2) /* ET Grp 2 Net Selection Options */
         GTK_H_BULB_TRIAD( NetsGroup2_vbox, NetEndPoint,  None, Empty, Filled, Filled);
         GTK_H_BULB_TRIAD( NetsGroup2_vbox, NetMidPoint,  None, Empty, Filled, Filled);
         GTK_H_BULB_TRIAD( NetsGroup2_vbox, NetSelection, None, Net, All, Net);
   GTK_END_TAB(EditPref);
  } /*** END Edit TAB Contents ***/

  { /*------------------- Start Pointer TAB Contents -------------------*/
   GTK_START_TAB (PointerPref);
     VSECTION(PointerPrefTab_vbox, PointerOptions) /* PT Solo Grp 1 */
       HSECTION (PointerOptions_vbox, PointerRow1)    /* Row 1 */
         GTK_SWITCH(PointerRow1_hbox, ZoomPan, 18, FALSE);
         GTK_NUMERIC_SPIN (PointerRow1_hbox, ZoomGain, 67, 20, 0, 80);
       HSECTION (PointerOptions_vbox, PointerRow2)    /* Row 1 */
         GTK_SWITCH(PointerRow2_hbox, FastMousePan, 18, FALSE);
         GTK_NUMERIC_SPIN (PointerRow2_hbox, MousePanGain, 22, 1, 1, 30);
       HSECTION (PointerOptions_vbox, PointerRow3)    /* Row 3 */
         GTK_SWITCH(PointerRow3_hbox, DragMove, 18, TRUE);
         GTK_NUMERIC_SPIN (PointerRow3_hbox, ScrollPanSteps, 11, 8, 0, 80);
       HSECTION (PointerOptions_vbox, PointerRow4)    /* Row 4 */
         GTK_SWITCH(PointerRow4_hbox, ClassicWheel, 40, TRUE);
         GTK_NEW_COMBO (PointerRow4_hbox, PointerCursor,  150, 13);
           LOAD_COMBO_STR( PointerCursor, CursorStrings );
         gtk_widget_set_size_request (PointerCursorCombo, 150, 31);
       HSECTION (PointerOptions_vbox, PointerRow5)    /* Row 4 */
         GTK_SWITCH(PointerRow5_hbox, PointerHScroll, 8, FALSE);
         GTK_NEW_COMBO (PointerRow5_hbox, MiddleButton,   150, 12);
         gtk_widget_set_size_request (MiddleButtonCombo,  150, 31);
         GTK_LOAD_COMBO (MiddleButton, RC_STR_MID_STROKE)
         GTK_LOAD_COMBO (MiddleButton, RC_STR_MID_REPEAT)
         GTK_LOAD_COMBO (MiddleButton, RC_STR_MID_ACTION)
         GTK_LOAD_COMBO (MiddleButton, RC_STR_MID_MOUSEPAN)
       HSECTION (PointerOptions_vbox, PointerRow6)    /* Row 4 */
         GTK_SWITCH(PointerRow6_hbox, WarpCursor, 30, FALSE);
         GTK_NEW_COMBO (PointerRow6_hbox, ThirdButton,  150, 0);
         gtk_widget_set_size_request (ThirdButtonCombo, 150, 31);
         GTK_LOAD_COMBO (ThirdButton, RC_STR_3RD_POPUP)
         GTK_LOAD_COMBO (ThirdButton, RC_STR_3RD_PAN)
     HXYP_SEPERATOR (PointerPrefTab_vbox, End, 10);

   GTK_END_TAB(PointerPref);
  } /*** END Pointer TAB Contents ***/

  { /*------------------- Start Window TAB Contents -------------------*/

   GTK_START_TAB (WindowPref);
     HSECTION (WindowPrefTab_vbox, DisplaySizeOptions) /* WT Row 1 Display Size */
       GTK_V_QUAD_BULB(DisplaySizeOptions_hbox, WindowSize, 10, W650H487, W900H650, W950H712, W1100H825, W950H712)
       GTK_V_BULB_TRIAD( DisplaySizeOptions_hbox, WorldSize, 20, Small, Medium, Large, Medium);
     HD_SEPERATOR (WindowPrefTab_vbox, Grp2);
       HPSECTION(WindowPrefTab_vbox, GridOptions, DIALOG_V_SPACING) /* WT Row 2 */
         GTK_V_BULB_TRIAD( GridOptions_hbox, GridMode, 0, None, Dots, Mesh, Mesh);
         VPSECTION(GridOptions_hbox, GridDotOptions, 50) /* WT Row 2 Grp 2 Dot Grid Options */
           GTK_NEW_COMBO (GridDotOptions_vbox, DotGridMode, 150, DIALOG_V_SPACING);
           GTK_LOAD_COMBO (DotGridMode, RC_STR_DOTS_MODE_VARIABLE)
           GTK_LOAD_COMBO (DotGridMode, RC_STR_DOTS_MODE_FIXED)
           GTK_NUMERIC_SPIN (GridDotOptions_vbox, DotGridThreshold, DIALOG_V_SPACING, DEFAULT_GRID_DOT_SIZE, MIN_GRID_DOT_SIZE, MAX_GRID_DOT_THRESHOLD);
     HD_SEPERATOR (WindowPrefTab_vbox, Grp3);
       HSECTION(WindowPrefTab_vbox, MeshGridSizeOptions) /* WT Row 3 */
         GTK_V_BULB_TRIAD (MeshGridSizeOptions_hbox, GridDotSize, 8, One, Two, Three, One);
         GTK_NUMERIC_SPIN (MeshGridSizeOptions_hbox, MeshGridThreshold, 0, DEFAULT_GRID_MESH_THRESHOLD, MIN_GRID_MESH_THRESHOLD, MAX_GRID_MESH_THRESHOLD);
     HD_SEPERATOR (WindowPrefTab_vbox, Grp4);
       GEDA_FRAME (WindowPrefTab_vbox, Scrolling, -1, 58, 0.05, 0.2, 10)
         GTK_SWITCH(Scrolling_hbox, ScrollBars,        DIALOG_H_SPACING + 10, TRUE);
         GTK_SWITCH(Scrolling_hbox, ScrollBarsVisible, DIALOG_H_SPACING + 10, TRUE);
         GTK_SWITCH(Scrolling_hbox, DelayScrolling,    DIALOG_H_SPACING + 10, FALSE);
   GTK_END_TAB(WindowPref);
  } /*** END Window TAB Contents ***/

  { /*-------------------- Start Text TAB Contents --------------------*/

   GTK_START_TAB (TextPref);
     VSECTION(TextPrefTab_vbox, TextOptionsGrp1); /* TT Grp 1 Text Options */
       HSECTION (TextOptionsGrp1_vbox, TextOptionsRow1)   /* TT Grp 1 Row 1 Text Styles */
         GTK_NUMERIC_SPIN (TextOptionsRow1_hbox, TextSize, 9, DEFAULT_TEXT_SIZE, MIN_TEXT_SIZE, MAX_TEXT_SIZE);
         GTK_NEW_COMBO (TextOptionsRow1_hbox, FontName, 160, DIALOG_V_SPACING);
       HSECTION (TextOptionsGrp1_vbox, TextOptionsRow2)   /* TT Grp 1 Row 1 Text Styles */
         GTK_NUMERIC_SPIN (TextOptionsRow2_hbox, TextZoomFactor, 9, DEFAULT_TEXT_ZOOM, MIN_TEXT_ZOOM, MAX_TEXT_ZOOM);
       GEDA_FRAME (TextOptionsGrp1_vbox, Markers, -1, 110, 0.3, 0.2, DIALOG_H_SPACING)
         VSECTION (Markers_hbox, MarkerOptions)  /* Grp 1 Row 1 */
           HSECTION ( MarkerOptions_vbox, MarkerOptionsRow3)  /* Grp 1 Row 3 */
             GTK_SWITCH(MarkerOptionsRow3_hbox, TextOriginMarker, DIALOG_V_SPACING, TRUE);
           HSECTION (MarkerOptions_vbox, MarkerOptionsRow4)  /* Grp 1 Row 2 */
             GTK_NUMERIC_SPIN (MarkerOptionsRow4_hbox, TextMarkerSize, DIALOG_H_SPACING, DEFAULT_TEXT_MARKER_SIZE, MIN_TEXT_MARKER_SIZE, MAX_TEXT_MARKER_SIZE);
             GEDA_COLOR_BUTTON (MarkerOptionsRow4_hbox, TextMarkerColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, 0)
     HD_SEPERATOR (TextPrefTab_vbox, Grp2);
     HSECTION (TextPrefTab_vbox, CapsStyleOptions)   /* TT Grp 2 Text Styles */
       GTK_V_BULB_TRIAD(CapsStyleOptions_hbox, CapsStyle, DIALOG_H_SPACING, Lower, Upper, Both, Both);
     HD_SEPERATOR (TextPrefTab_vbox, Grp3);
     HSECTION (TextPrefTab_vbox, TextOptionsGrp3) /* TT Grp 3 Feedback */
       GTK_V_BULB_TRIAD(TextOptionsGrp3_hbox, TextFeedback, DIALOG_H_SPACING, Readable, Always, Default, Readable);
   GTK_END_TAB(TextPref);
  } /***  END Text TAB Contents ***/

  { /*-------------------- Start Styles TAB Contents --------------------*/
   GTK_START_TAB (StylesPref);
     HD_SEPERATOR (StylesPrefTab_vbox, Grp1);
     HSECTION(StylesPrefTab_vbox, StylesRow1); /* ST Grp 1 Bus and Net */
       GTK_V_BULB_TRIAD( StylesRow1_hbox, BusStyle, 5, None, Thin, Thick, Thin);
       VSECTION (StylesRow1_hbox, BusWidths)  /* ST Grp 1 Bus Spinners */
         GTK_NUMERIC_SPIN (BusWidths_vbox, ThinBusWidth, DIALOG_V_SPACING +5, DEFAULT_THIN_BUS_WIDTH, 0, 100);
         GTK_NUMERIC_SPIN (BusWidths_vbox, ThickBusWidth, DIALOG_V_SPACING +5, DEFAULT_THICK_BUS_WIDTH, 0, 500);
       VXP_SEPERATOR (StylesRow1_hbox, Row1, 15);
       GTK_V_BULB_TRIAD( StylesRow1_hbox, NetStyle, 5, None, Thin, Thick, Thin);
       VPSECTION (StylesRow1_hbox, NetWidths, DIALOG_V_SPACING)  /* ST Grp 1 Net Spinners */
         GTK_NUMERIC_SPIN (NetWidths_vbox, ThinNetWidth, DIALOG_V_SPACING +5, DEFAULT_THIN_NET_WIDTH, 0, 100);
         GTK_NUMERIC_SPIN (NetWidths_vbox, ThickNetWidth, DIALOG_V_SPACING +5, DEFAULT_THICK_NET_WIDTH, 0, 500);
     HD_SEPERATOR (StylesPrefTab_vbox, Grp2);
     HSECTION(StylesPrefTab_vbox, StylesRow2); /* ST Grp 2 Lines and Pins */
       GTK_V_BULB_TRIAD( StylesRow2_hbox, LineStyle, 5, None, Thin, Thick, Thin);
       VSECTION (StylesRow2_hbox, LineWidths)  /* ST Grp 2 Lines Spinners */
         GTK_NUMERIC_SPIN (LineWidths_vbox, ThinLineWidth, DIALOG_V_SPACING +5, DEFAULT_THIN_LINE_WIDTH, 0, 100);
         GTK_NUMERIC_SPIN (LineWidths_vbox, ThickLineWidth, DIALOG_V_SPACING +5, DEFAULT_THICK_LINE_WIDTH, 0, 500);
       VXP_SEPERATOR (StylesRow2_hbox, Row2, 15);
       GTK_V_BULB_TRIAD( StylesRow2_hbox, PinStyle, 5, None, Thin, Thick, Thin);
       VPSECTION (StylesRow2_hbox, PinWidths, DIALOG_V_SPACING)  /* ST Grp 2 Pin Spinners */
         GTK_NUMERIC_SPIN (PinWidths_vbox, ThinPinWidth, DIALOG_V_SPACING +5, DEFAULT_THIN_PIN_WIDTH, 0, 100);
         GTK_NUMERIC_SPIN (PinWidths_vbox, ThickPinWidth, DIALOG_V_SPACING +5, DEFAULT_THICK_PIN_WIDTH, 0, 500);
     HD_SEPERATOR (StylesPrefTab_vbox, Grp3);      /* Ripper Options */
       HSECTION(StylesPrefTab_vbox, StylesRow3);     /* ST Grp 2 Lines and Pins */
         GTK_SWITCH(StylesRow3_hbox, RipperType, 30, FALSE);
         GTK_NEW_COMBO (StylesRow3_hbox, RipperSymbol, 0, 0);
         gtk_widget_set_size_request (RipperSymbolCombo, 180, 31);
         GTK_LOAD_COMBO (RipperSymbol, DEFAULT_BUS_RIPPER_SYMNAME)
         GTK_LOAD_COMBO (RipperSymbol, SECOND_BUS_RIPPER_SYMNAME)
       HSECTION(StylesPrefTab_vbox, StylesRow4);     /* ST Grp 2 Lines and Pins */
         GTK_SWITCH(StylesRow4_hbox, RipperRotation, 18, FALSE);
         GTK_NUMERIC_SPIN (StylesRow4_hbox, RipperSize, 30, 200, 0, 500);
     HD_SEPERATOR (StylesPrefTab_vbox, Grp4);        /* Junction Options */
       GEDA_FRAME (StylesPrefTab_vbox, Junctions, -1, 58, 0.05, 0.2, 10)
         GTK_NUMERIC_SPIN (Junctions_hbox, JunctionSize, 12, DEFAULT_JUNCTION_SIZE, MIN_JUNCTION_SIZE, MAX_JUNCTION_SIZE);
         GEDA_COLOR_BUTTON (Junctions_hbox, JunctionColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, DIALOG_H_SPACING)
         GEDA_COLOR_BUTTON (Junctions_hbox, NetEndpointColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, DIALOG_H_SPACING)
     HD_SEPERATOR (StylesPrefTab_vbox, End);

   GTK_END_TAB(StylesPref);
  } /*** END Styles TAB Contents ***/

  { /*------------------ Start Attribute TAB Contents ------------------*/
   GTK_START_TAB (AttributesPref);
    VSECTION (AttributesPrefTab_vbox, AttributeDialogList);  /* Grp 1 Attribute List */
       GTK_HV_BULB_TRIAD (AttributeDialogList_vbox, DialogListAttributes, All, None, List, All);
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
       HYP_SEPERATOR (AttributesPrefTab_vbox, Grp2, 5);
       HSECTION(AttributesPrefTab_vbox,  AttributeOptions); /* Grp 2 but now Row 2 */
         GTK_NUMERIC_SPIN (AttributeOptions_hbox, AttributeOffset, 25, 50, 0, 300);
         GTK_NUMERIC_SPIN (AttributeOptions_hbox, AutoPlacementGrid, 25, 1, 0, 300);
       HYP_SEPERATOR (AttributesPrefTab_vbox, End, 5);
   GTK_END_TAB(AttributesPref);

  } /*** END Attribute TAB Contents ***/

  { /*-------------------- Start Library TAB Contents --------------------*/

   GTK_START_TAB (LibraryPref);
     VSECTION(LibraryPrefTab_vbox, LibraryOptionsRow1)
       GTK_SWITCH(LibraryOptionsRow1_vbox, EnforceHierarchy, DIALOG_V_SPACING, FALSE);
       GTK_SWITCH(LibraryOptionsRow1_vbox, EmbedComponents, DIALOG_V_SPACING, FALSE);
       GTK_SWITCH(LibraryOptionsRow1_vbox, SortLibrary, DIALOG_V_SPACING, FALSE);
     HXYP_SEPERATOR (LibraryPrefTab_vbox, End, 10);
   GTK_END_TAB(LibraryPref);
  } /*** END Library TAB Contents ***/

  dialog_action_area = GTK_DIALOG (ThisDialog)->action_area;
  g_object_set (dialog_action_area, "visible", TRUE, NULL);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area), GTK_BUTTONBOX_END);

  CancelButt = gtk_button_new_from_stock ("gtk-cancel");
  g_object_set (CancelButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), CancelButt, GTK_RESPONSE_CANCEL);
  gtk_widget_set_can_default(CancelButt, TRUE);
  gtk_widget_set_tooltip_text (CancelButt, _("Close without changing any settings"));

  SaveButt = gtk_button_new_with_mnemonic (_("Save"));
  g_object_set (SaveButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), SaveButt, GTK_RESPONSE_APPLY);
  gtk_widget_set_can_default(SaveButt, TRUE);
  gtk_widget_set_tooltip_text (SaveButt, _("Close and Write settings to disk"));

  OkayButt = gtk_button_new_from_stock ("gtk-ok");
  g_object_set (OkayButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), OkayButt, GTK_RESPONSE_OK);
  gtk_widget_set_can_default(OkayButt, TRUE);
  gtk_widget_set_tooltip_text ( OkayButt, _("Change settings and close but do not write settings to storage.."));

  /* Store pointers to widgets, for use by get_widget_data(). */
  GTK_HOOKUP_OBJECT_NO_REF (ThisDialog, ThisDialog, "ThisDialog");
  GTK_HOOKUP_OBJECT_NO_REF (ThisDialog, MainDialogVBox, "MainDialogVBox");
  GTK_HOOKUP_OBJECT (ThisDialog, notebook, "notebook");
  GTK_HOOKUP_OBJECT_NO_REF (ThisDialog, dialog_action_area, "dialog_action_area");
  GTK_HOOKUP_OBJECT (ThisDialog, CancelButt, "CancelButt");
  GTK_HOOKUP_OBJECT (ThisDialog, SaveButt, "SaveButt");
  GTK_HOOKUP_OBJECT (ThisDialog, OkayButt, "OkayButt");

  gtk_widget_grab_default (CancelButt);

  g_signal_connect ((gpointer) notebook, "switch-page",
                    G_CALLBACK (on_notebook_switch_page),
                    NULL);
  return ThisDialog;
}
/** @} END Group X_Settings_Dialog */

int x_settings_lookup_cursor(int offset) {
  return DrawingCursorsInt[offset];
}

/*! \defgroup X_Settings_Dialog_Unload_Variables Load Variables X Settings Dialog
 *  @{
 */

/*! \brief Post Dialog procedure to retrieves values in dialog controls.
 *  \par Function Description
 *       This function retrieves and saves the values from all widgets. The
 *       values are saved to either the memory variables from which they were
 *       loaded or to the rc_options structure.
 */
void GatherSettings(GschemToplevel *w_current) {

  GedaToplevel  *toplevel = w_current->toplevel;
  EdaConfig     *cfg      = eda_config_get_user_context ();
  const char    *group    = IVAR_CONFIG_GROUP;

  int         tmp_int;
  const char *tmpstr;
  GdkColor    color;

  /* Next line assigns a pointer to a char field in a GTK Entry widget - do not free it! */
  tmpstr = GetEntryText(UntitledNameEntry);

  eda_config_set_string (cfg, group, "default-filename", tmpstr);

/* The Color Buttons */
  gtk_color_button_get_color(GTK_COLOR_BUTTON(GripStrokeColorButt), &color);
  eda_renderer_set_grips_stroke_color (w_current->renderer, &color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(GripFillColorButt), &color);
  eda_renderer_set_grips_fill_color (w_current->renderer, &color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(NetEndpointColorButt), &color);
  eda_renderer_set_net_endpoint_color (w_current->renderer, &color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(TextMarkerColorButt), &color);
  eda_renderer_set_text_marker_color (w_current->renderer, &color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(JunctionColorButt), &color);
  eda_renderer_set_junction_color (w_current->renderer, &color);

/* Combo Boxes (10) */

  w_current->dots_grid_mode = gtk_combo_box_get_active (GTK_COMBO_BOX (DotGridModeCombo));
  console_window_type       = gtk_combo_box_get_active (GTK_COMBO_BOX (ConsoleWindowTypeCombo));
  w_current->undo_type      = gtk_combo_box_get_active (GTK_COMBO_BOX (UndoTypeCombo));
  w_current->middle_button  = gtk_combo_box_get_active (GTK_COMBO_BOX (MiddleButtonCombo));
  w_current->third_button   = gtk_combo_box_get_active (GTK_COMBO_BOX (ThirdButtonCombo));
  tmp_int                   = gtk_combo_box_get_active (GTK_COMBO_BOX (PointerCursorCombo));

  if (tmp_int != w_current->drawing_pointer) {
    int pointer_id = DrawingCursorsInt[tmp_int];   /* get the cursor id from our table */
    w_current->drawing_pointer = tmp_int;      /* Save the index with table offset factor*/
    x_window_set_cursor(w_current, pointer_id);
  }

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

  tmpstr = gtk_combo_box_get_active_text (GTK_COMBO_BOX (TitleBlockCombo));
  eda_config_set_string (cfg, group, "default-titleblock", tmpstr);

  tmp_int = gtk_combo_box_get_active (GTK_COMBO_BOX (RipperSymbolCombo));
  if (tmp_int != rc_options.ripper_symbol_index) {
    GEDA_FREE(w_current->bus_ripper_symname);
    w_current->bus_ripper_symname =
    geda_strdup(gtk_combo_box_get_active_text(GTK_COMBO_BOX (RipperSymbolCombo)));
    strcpy(rc_options.ripper_symbol_fname, w_current->bus_ripper_symname); /* save the filename */
  }

  tmpstr = gtk_combo_box_get_active_text (GTK_COMBO_BOX (FontNameCombo));
  eda_config_set_string (cfg, group, "default-font-name", tmpstr);
  eda_renderer_set_font_name(w_current->renderer, tmpstr);
  /* Don't free the font name string, belongs to the dialog control */

/* The Switches Alphabetically (31) */
             auto_load_last             = GET_SWITCH_STATE (AutoLoadSwitch);
  w_current->bus_ripper_rotation        = GET_SWITCH_STATE (RipperRotationSwitch);
  w_current->bus_ripper_type            = GET_SWITCH_STATE (RipperTypeSwitch);
  w_current->continue_component_place   = GET_SWITCH_STATE (ContinuePlaceSwitch);
  w_current->drag_can_move              = GET_SWITCH_STATE (DragMoveSwitch);
  w_current->renderer->draw_grips       = GET_SWITCH_STATE (DrawGripsSwitch);
  w_current->embed_components           = GET_SWITCH_STATE (EmbedComponentsSwitch);
   toplevel->image_color                = GET_SWITCH_STATE (EnableColorImagingSwitch);
   toplevel->invert_images              = GET_SWITCH_STATE (InvertImagesSwitch);
  w_current->enforce_hierarchy          = GET_SWITCH_STATE (EnforceHierarchySwitch);
  w_current->fast_mousepan              = GET_SWITCH_STATE (FastMousePanSwitch);
  w_current->action_feedback_mode       = GET_SWITCH_STATE (FeedbackModeSwitch);
  w_current->force_boundingbox          = GET_SWITCH_STATE (ForceBoundingBoxSwitch);

  w_current->file_preview               = GET_SWITCH_STATE (FilePreviewSwitch);

  rc_options.display_color_map          = GET_SWITCH_STATE (FriendlyColorMapSwitch);
  rc_options.display_outline_color_map  = GET_SWITCH_STATE (FriendlyOutlineMapSwitch);

  w_current->magnetic_net_mode          = GET_SWITCH_STATE (MagneticNetsSwitch);
   toplevel->net_consolidate            = GET_SWITCH_STATE (ConsolidateNetsSwitch);
  w_current->net_direction_mode         = GET_SWITCH_STATE (NetDirectionSwitch);

  w_current->netconn_rubberband         = GET_SWITCH_STATE (RubberNetsSwitch);
             logging                    = GET_SWITCH_STATE (EnableLogSwitch);
             console_window             = GET_SWITCH_STATE (InitConsoleWindowSwitch);
  w_current->object_clipping            = GET_SWITCH_STATE (ObjectClippingSwitch);
  w_current->raise_dialog_boxes         = GET_SWITCH_STATE (NotifyEventsSwitch);
  w_current->scrollbars                 = GET_SWITCH_STATE (ScrollBarsSwitch);

             tmp_int                    = GET_SWITCH_STATE (ScrollBarsVisibleSwitch);
  if (tmp_int != w_current->scrollbars_visible) {
    w_current->scrollbars_visible       =  tmp_int;
    g_object_set (w_current->v_scrollbar, "visible", tmp_int, NULL);
    g_object_set (w_current->h_scrollbar, "visible", tmp_int, NULL);
  }
  w_current->scrollbar_update           = GET_SWITCH_STATE (DelayScrollingSwitch);
  w_current->scroll_wheel               = GET_SWITCH_STATE (ClassicWheelSwitch);
  w_current->sort_component_library     = GET_SWITCH_STATE (SortLibrarySwitch);
  w_current->pointer_hscroll            = GET_SWITCH_STATE (PointerHScrollSwitch);
  w_current->renderer->text_origin_marker = GET_SWITCH_STATE (TextOriginMarkerSwitch);
  w_current->undo_control               = GET_SWITCH_STATE (EnableUndoSwitch);
  w_current->undo_panzoom               = GET_SWITCH_STATE (UndoViewsSwitch);
  w_current->warp_cursor                = GET_SWITCH_STATE (WarpCursorSwitch);
  w_current->zoom_with_pan              = GET_SWITCH_STATE (ZoomPanSwitch);

/* The Spin Controls Alphabetically (23) */
  w_current->add_attribute_offset       = GET_SPIN_IVALUE (AttributeOffsetSpin);
  w_current->attribute_placement_grid   = GET_SPIN_IVALUE (AutoPlacementGridSpin);
  x_settings_set_scm_int("autoplace-attributes-grid", w_current->attribute_placement_grid);

                                tmp_int = GET_SWITCH_STATE (AutoSaveSwitch);
   toplevel->auto_save_interval         = tmp_int == 0 ? 0 : GET_SPIN_IVALUE (AutoSaveIntervalSpin);
  w_current->bus_ripper_size            = GET_SPIN_IVALUE (RipperSizeSpin);
  w_current->dots_grid_fixed_threshold  = GET_SPIN_IVALUE (DotGridThresholdSpin);
  w_current->grip_pixel_size            = GET_SPIN_IVALUE (GripPixelSizeSpin);
  w_current->keyboardpan_gain           = GET_SPIN_IVALUE (KeyboardPanGainSpin);
  w_current->mesh_grid_threshold        = GET_SPIN_IVALUE (MeshGridThresholdSpin);
  w_current->mousepan_gain              = GET_SPIN_IVALUE (MousePanGainSpin);
  w_current->scrollpan_steps            = GET_SPIN_IVALUE (ScrollPanStepsSpin);
  w_current->select_slack_pixels        = GET_SPIN_IVALUE (SelectPixelsSpin);
  w_current->snap_size                  = GET_SPIN_IVALUE (SnapSizeSpin);
  w_current->renderer->text_marker_size = GET_SPIN_IVALUE (TextMarkerSizeSpin);
  w_current->text_size                  = GET_SPIN_IVALUE (TextSizeSpin);
  w_current->text_display_zoomfactor    = GET_SPIN_IVALUE (TextZoomFactorSpin);

   toplevel->thick_bus_width            = GET_SPIN_IVALUE (ThickBusWidthSpin);
   toplevel->thick_line_width           = GET_SPIN_IVALUE (ThickLineWidthSpin);
   toplevel->thick_net_width            = GET_SPIN_IVALUE (ThickNetWidthSpin);
   toplevel->thick_pin_width            = GET_SPIN_IVALUE (ThickPinWidthSpin);

   toplevel->thin_bus_width             = GET_SPIN_IVALUE (ThinBusWidthSpin);
   toplevel->thin_line_width            = GET_SPIN_IVALUE (ThinLineWidthSpin);
   toplevel->thin_net_width             = GET_SPIN_IVALUE (ThinNetWidthSpin);
   toplevel->thin_pin_width             = GET_SPIN_IVALUE (ThinPinWidthSpin);

  w_current->undo_levels                = GET_SPIN_IVALUE (UndoBufferSizeSpin);
  w_current->zoom_gain                  = GET_SPIN_IVALUE (ZoomGainSpin);

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
   SaveAttributeFilterList(w_current); /* SelectedAttributes */

}

/** @} END Group X_Settings_Dialog_Unload_Variables */
