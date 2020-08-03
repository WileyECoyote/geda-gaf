/* -*- C x_settings_dialog.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * File: x_settings_dialog.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2018 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2012-2018 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
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
 * ---------------|--------------------------------------------------
 * WEH | 09/17/12 |  Inital release.
 * ---------------|--------------------------------------------------
 * WEH | 12/02/12 |  Added call to x_settings_set_scm_int in function
 *                |  GatherSettings (to support renaming of autoplace-
 *                |  attribute-grid to attribute_placement_grid).
 * ---------------|--------------------------------------------------
 * WEH | 12/04/12 |  Added switch for EnableColorImaging
 * ---------------|--------------------------------------------------
 * WEH | 12/30/12 |  Changed "Log" to "Console"
 * ---------------|--------------------------------------------------
 * WEH | 01/06/13 | Added spinner RipperSize, switches RipperRotation
 *                | RipperType, and combo RipperSymbol, (to extend
 *                | functionality)
 * ---------------|--------------------------------------------------
 * WEH | 07/20/13 | Added Font Name Combo (to extend functionality)
 * ---------------|--------------------------------------------------
 * WEH | 09/20/13 | Added PointerCurso Combo (to extend functionality)
 * ---------------|--------------------------------------------------
 * WEH | 09/25/13 | Added GripStrokeColor, GripFillColor,TextMarkerColor,
 *                | TextOriginMarker, TextMarkerSize, JunctionColor,
 *                | TextMarkerSize and JunctionSize, NetEndpointColor,
 *                | ScrollBarsVisible
 * ---------------|--------------------------------------------------
 * WEH | 03/13/14 | Fix: case JunctionColor did not have break in
 *                | color_butt_responder, oops.
 * ---------------|--------------------------------------------------
 * WEH | 07/20/14 | Make-over for Doxygen Documentation/Comments in order
 *                | to improve source documentation.
 * ---------------|--------------------------------------------------
 * WEH | 11/14/14 | Add call x_menu_set_togglable if drag_can_move changed
 *                | in GatherSettings.
 * ---------------|--------------------------------------------------
 * WEH | 11/18/14 | Add "FreeMono" to IDS_FONT_NAMES
 * ---------------|--------------------------------------------------
 * WEH | 12/11/14 | Pass w_current to setup_font_name_combo and use dynamically
 *                  list of font names. Remove static IDS_FONT_NAMES
 * ---------------|--------------------------------------------------
 * WEH | 12/13/14 | Add on_change_renderer, revise setup_font_name_combo to
 *                | include X11 scalable fonts.
 * ---------------|--------------------------------------------------
 * WEH | 03/13/15 | Add spinner for text marker threshold as a double.
 *                | Annotated notes to instructions for clarification.
 * ---------------|--------------------------------------------------
 * WEH | 08/01/15 | Remove include <guile/gh.h>, (<ref> libgeda::u_string.c)
 * ---------------|--------------------------------------------------
 * WEH | 10/02/15 | Add missing FontNameCombo to combo_responder
 * ---------------|--------------------------------------------------
 * WEH | 04/10/16 | Adjust Renderer, AntiAlias  ConsoleWindowType and
 *                | DotGridMode Combo width and padding.
 * ------------------------------------------------------------------
 * WEH | 08/21/18 | verify geda_combo_box_get_active_text returned
 *                | a pointer.
 * ------------------------------------------------------------------
 * WEH | 06/19/19 | Substituted geda_combo_widget_get_active_text for
 *                | geda_combo_box_get_active_text.
 * ------------------------------------------------------------------
 */

/*!
 * \file x_settings_dialog.c
 * \brief A dialog box for setting program preferences.
 *
 * \remarks To add a new variable or control:
 *
 * 1. The variable should be valid and readable in either the RC system,
 *    or the key-file system, but this is not a requirement to add the
 *    widget.
 *
 * 2. Create the control
 *
 *      a.) Declare widget variable in the section Global Variables
 *      b.) Add the widget in the create_settings_dialog function,
 *          i.e. where the widget should appear in the dialog.
 *      c.) Added a WidgetStringData record to struct in the file
 *          x_settings_dialog.h, see instructions at the top of the
 *          header.
 *      d.) Add any necessary support functions and code to the
 *          responder, i.e. the existing callback function. note
 *          that spinners do not generally have a callback.
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
 *       value will not be saved to rc generated files.)
 *
 *    \note The setting should be saved some where, some how. This
 *          could be within a function such as dialog code or in
 *          generalized groups, see x_settings_save_settings and
 *          x_window_save_settings.
 *
 *    \note The older method of generating a scheme file in the
 *          user configuration directory only works if the keyword
 *          appears in user-gschemrc.scm. Steps 5 and 6 are only
 *          necessary to have the keyword appear in the generated
 *          files.
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
 * \note: The main responder for the Settings dialog is not in
 *        this modules, see x_settings.c
*/

#include <sys/types.h>
#include <ctype.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "../../include/gschem.h"
#include "../../include/gschem_xdefines.h" /* Define dialog default internal spacing */
#include "../../include/gschem_dialog.h"   /* Definition the base Dialog Class */

#include "../../../include/geda/geda_stat.h"
#include "../../../include/geda/geda_dialog_controls.h" /* Macros for Dialogs */
#include "../../../include/geda_debug.h"

#include <geda_widgets.h>                  /* Switches use geda_labels */

/** \defgroup Preferences-Dialog Preferences Dialog
 *  @{
 *  \ingroup Settings-Dialogs
 *
 *  \par This group contains routines for the Preferences dialog.
 *
 *  \image html preferences_dialog.png
 *  \image latex preferences_dialog.png
 */

#include "../../include/x_settings.h"        /* Common Declarations and Enumerators */
#include "../../include/x_settings_dialog.h" /* Dialog String Data */

/* ---------------  Functions that Should Be Somewhere Else  --------------- */

/* Could call this one gtkless_radio_group_get_active */
int gtk_radio_group_get_active(GSList *RadioGroupList) {

  int active;
  int index;
  int length;

  active = -1;
  length = g_slist_length (RadioGroupList);

  for (index = 0; index < length; index++) {

     GtkToggleButton *button;

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

/* --------------------------- Global Variables ---------------------------- */

/** \defgroup Preferences-Dialog-Globals Global Variables
 *  @{
 *  \memberof (Preferences-Dialog)
 *  \par
 *   This group contains global variables for the Settings Dialog.
 *  \note The variables are only global to this module, not gschem
 */

/** \struct rc_options x_settings.h
 *  \par
 *   This structure allows temporary editing of configuration items to be changed
 *   on the Settings Dialog without changing toplevel members. In general, do not
 *   use unless required. Initial values should be set in load_settings_dialog and
 *   then manipulated by the dialog function. The final value should be retrieved
 *   in GatherSettings.
 *
 *  \note The values loaded into the structure here are defaults.
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
        0,  /* render_adaptor */
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
static GtkWidget *MeshMinorColorButt;
static GtkWidget *MeshMajorColorButt;
static GtkWidget *NetEndpointColorButt;
static GtkWidget *TextMarkerColorButt;

/* The Combo Boxes */
static GtkWidget *AntiAliasCombo;
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
static GtkWidget *RendererCombo;

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
static GtkWidget *MeshGridWidthSpin;
static GtkWidget *MousePanGainSpin;
static GtkWidget *RipperSizeSpin;
static GtkWidget *ScrollPanStepsSpin;
static GtkWidget *SelectPixelsSpin;
static GtkWidget *SnapSizeSpin;
static GtkWidget *TextMarkerSizeSpin;
static GtkWidget *TextMarkerThldSpin;
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
static GtkWidget *AutoLoadLastSwitch=NULL;
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

/** @} endgroup Preferences-Dialog-Globals */

/* --------------------------- Support Functions --------------------------- */

/** \defgroup Preferences-Dialog-Support Settings Dialog Support Functions
 *  @{
 *  \memberof (Preferences-Dialog)
 *  \par
 *   Support functions for the Settings dialog are subgrouped into
 *   categories as follows:
 *
 *        1. Inhibitor Support Functions
 *        2. Attributes Support ~ Group 1 & Group 2
 *        3. Button Support
 *        4. ComboBox Support
 *        5. Multi Widget Calllback Responders
 */

/* ---------------------- Inhibitor Support Functions ---------------------- */

/** \defgroup Preferences-Dialog-Inhibitors Settings Dialog Inhibitors Functions
 *  @{
 *  \memberof (Preferences-Dialog-Support)
 *  \par
 *   The Inhibitors enable and disable Widgets based on other selections.
 *   This groups contains the following functions:
 *
 *      1. enable_attribute_list_controls        called in callback functions
 *      2. enable_color_map_controls
 *      3. enable_color_map_scheme
 *      4. enable_log_controls
 *      5. enable_undo_controls
 *      6. on_notebook_switch_page               is a callback handler
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
                         unsigned int page_num, void            *user_data)
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
    if (GetGedaCombo (DotGridMode) == DOTS_GRID_VARIABLE_MODE)
       gtk_widget_set_sensitive (DotGridThresholdSpin, FALSE);
    else
       gtk_widget_set_sensitive (DotGridThresholdSpin, TRUE);

    state = GET_SWITCH_STATE (ScrollBarsSwitch);
    gtk_widget_set_sensitive (ScrollBarsVisibleSwitch, state);
    gtk_widget_set_sensitive (DelayScrollingSwitch, state);
    break;
  case RenderPref:
    break;
  case StylesPref:
    state = GET_SWITCH_STATE (RipperTypeSwitch);
    gtk_widget_set_sensitive (RipperSymbolCombo, state);
  case TextPref:
    break;
  case AttributesPref:
    /* List controls are only enabled if the List radio is active */
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

/** @} endgroup Preferences-Dialog-Inhibitors */

/** \defgroup Preferences-Dialog-Attributes Attribute Support Functions
 *  @{
 *  \memberof (Preferences-Dialog-Support)
 *  \par
 *   Settings dialog attribute support functions are divided into two sub-groups.
 */

/* -------------------- Attributes Support Functions Group 1 ----------------*/

/*!
 *  \defgroup Preferences-Dialog-Attributes-Group-1 Attribute Support Group 1 Functions
 *  @{
 *  \memberof (Preferences-Dialog-Attributes)
 *  \par
 *   The functions to support attribute settings in group 1 are:
 *
 *      1. st_callback_selection_changed_view  Single click callback
 *      2. st_tree_row_activated               Double click callback
 *      3. connect_list_view                   Setup callbacks for list views
 *      4. initialize_tree_View                Initial widget setup
 *      5. add_to_list                         Called in load_tree_view_xxx
 *      6. load_tree_view_gl                   Load view from glist
 *      7. load_tree_view_str                  Load view from string array
*/

static void st_callback_selection_changed_view(GtkTreeSelection *selection,
                                               GtkWidget *Dialog)__attribute__((unused));

/*! \brief function st_callback_selection_changed_view
 *  \par Function Description
 *  This is a callback handler for attribute views and is called
 *  when an attribute is selected/single clicked.
 *
 *
 *  \param[in]  selection GtkTreeSelection set.
 *  \param[in]  Dialog    is really w_current.
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
 *   This is a callback handler for attribute views and is called
 *   when an attribute is double clicked.
 *
 *  \param[in]  tree_view GtkTreeView (could be either one).
 *  \param[in]  path      Tree limb.
 *  \param[in]  column    There is only 1 text data column.
 *  \param[in]  Dialog    is really w_current.
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
 *   This sets up the callbacks for the attribute views.
 *
 *  \param[in]  Dialog       is really w_current.
 *  \param[in]  TreeTextView A Treeview widge.
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
  g_signal_connect (TreeTextView, "row-activated",
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

/*! \remarks:
 *
 *  There are two attribute list referenced in the initialization file.
 *  The first is a filter list used by the Add Component dialog. This
 *  list is stored in a Glist array. The second list is used by the Add
 *  Attributes routines and is stored in a static structure in Libgeda.
 *  Without overloads, we direct loading to one of the functions below
 *  to load Treeviews, the first for Glist, the second for arrays.
 *
 *  Note that if the filter list was commented out in the rc file, the
 *  first list was not read into the memory. A "default" filter list is
 *  maintained in an anonymous array of arrays, see load_tree_view_str
 *  function for more details.
 *
 * \sa load_tree_view_str
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
    lambda (const char *data)
    {
      add_to_list( TreeView, data);
      return FALSE;
    }
    foreach (list);
}

/*!
 * \brief Load Tree View with string data from string array.
 * \par Function Description
 *  This function passes each char *from an array of strings to
 *  the add_to_list function to store the item in the designated
 *  view.
 *
 * \code
 *      while (list[i]) { add_to_list(TreeView, list[i++]);}
 * \endcode
 *
 *  The array of string can be NULL, in which case the strings are
 *  retrieved using geda_struct_attrib_get. The only "list" passed to
 *  this function is the default list that is used when configuration
 *  data is missing.
*/
void load_tree_view_str( GtkTreeView *TreeView, const char *list[])
{
  int i = 0;
  const char *string;

  const char *array ( int i) {
    if (list != NULL) /* if pointer is NULL, (not char) */
       return list[i];
    else
       return geda_struct_attrib_get(i);
  }

  string =  array(i);
  while (string != NULL) { add_to_list(TreeView, string);
    string = array(++i);
  }
}

/** @} endgroup Preferences-Dialog-Attributes-Group-1 */

/* -------------------- Attributes Support Functions Group 2 ----------------*/

/*!
 *  \defgroup Preferences-Dialog-Attributes-Group-2 Attribute Support Group 2 Functions
 *  @{
 *  \memberof (Preferences-Dialog-Attributes)
 *  \par
 *   The second group of functions to support attribute settings are:
 *   -
 *      1. GetAttributeFilterMode
 *      2. SaveAttributeFilterList
 *      3. SavePotentialAttributes
 *      4. is_not_in_list
 *      5. decrement_selected_attribute
 *      6. increment_selected_attribute
 *      7. add_selected_attribute
 *      8. remove_selected_attribute
 *      9. clear_attributes
 *     10. filter_list_set_default
 *
*/

/*!
 * \brief Preferences Dialog GetAttributeFilterMode
 * \par Function Description:
 * This is Group 2 support function returns the integer "setting" based
 * on the state of the variable component_select_attrlist, which should
 * not be confused with the state indicated in the dialog (after the user
 * has changed/clicked the radio/bulb widgets)
 *
 *  \retval 0        = Filter All    // rc entry had an asterisk
 *          1        = No Filter     // rc entry was an empty list
 *          2        = Filter List   // rc entry had an actual list
 */
static int GetAttributeFilterMode(GschemToplevel *w_current) {

  char *data;

  data = g_list_nth_data (View2Data, 0);  /* 0 could also be the last */
  /* return 1 if empty, else return 0 if char is asterisk, else return 2 */
  return data == NULL ? 1 : (data[0] == ASCII_ASTERISK ? 0 : 2 );
}

/*!
 * \brief Preferences Dialog Save List in the Filter Viewtree
 * \par Function Description:
 *  This is Group 2 support function called from GatherSettings to save
 *  the current attribute "filter" list.
 */
static int SaveAttributeFilterList(GschemToplevel *w_current) {

  GtkTreeModel *store    = NULL;
  GList        *new_list = NULL;
  char         *str_new  = NULL;
  int           index    = 0;
  int           next     = 0;
  int           list_len = 0;
  bool          update   = FALSE;

  GtkTreeIter iter;

  list_len = View2Data ? g_list_length(View2Data) : 0;

  switch (gtk_radio_group_get_active(DialogListAttributesRadioGroup)) {
     case 0:   /* ALL */
       if (list_len == 1) {
         char *c = View2Data->data;
         if (c[0] != ASCII_ASTERISK) {
           new_list = g_list_append(NULL, geda_strdup("*"));
           update = TRUE;
         }
       }
       else {
         new_list = g_list_append(NULL, geda_strdup("*"));
         update = TRUE;
       }
       break;

     case 1:   /* NONE */
       if (list_len != 0) {
         update = TRUE;
       }
       break;

     case 2:   /* LIST */
       store = gtk_tree_view_get_model (GTK_TREE_VIEW(SelectedAttributesView));

       /* Get the first iter in the list */
       next = gtk_tree_model_get_iter_first (store, &iter);

       while (next) {

         /* Walk through the list, reading each row. */
         gtk_tree_model_get (store, &iter, 0, &str_new, -1);
         new_list = g_list_append (new_list, str_new);
         next = gtk_tree_model_iter_next (store, &iter);
         index ++;
       }

       /* Update if the lists are not the same length */
       if (new_list && g_list_length(new_list) != list_len) {
         update = TRUE;
       }
       else if (!new_list) {
         /* User, for whatever reason removed all items, is still valid */
         update = TRUE;
       }
       else {

         GList *iter1, *iter2;

         iter1 = View2Data;
         iter2 = new_list;

         while (iter1) {
           char *old_str, *new_str;
           old_str = iter1->data;
           new_str = iter2->data;
           if (!strcmp(new_str, old_str)) {
             update = TRUE;
             break;
           }
           NEXT(iter1);
           NEXT(iter2);
         }
       }
       break;

     default:
       BUG_MSG("DialogListAttributesRadioGroup returned bad ID\n");
  }

  if (update) {

    if (View2Data != default_component_select_attrlist) {
      geda_glist_free_full (View2Data, g_free);
    }

    View2Data = new_list;
  }

  /* Don't GEDA_FREE (str_new) here because it's pointing
   * at a string somewhere and referenced in the glist */
  return index;
}

/*!
 * \brief Preferences Dialog Save list of attributes in left Viewtree
 * \par Function Description:
 *  This is a Group 2 support function to clear the current attribute
 *  list, gets the modified attribute list from the Treeview and stores
 *  the new list. The dialog does not have provisions to add or remove
 *  attributes from this list so the only modification is the order of
 *  the attributes in the list.
 */
static int SavePotentialAttributes(GschemToplevel *w_current) {

  GtkTreeModel *store;
  GtkTreeIter iter;
  int next;

  geda_struct_attrib_clear();

  store = gtk_tree_view_get_model (GTK_TREE_VIEW(PotentialAttributesView));

  /* Get the first iter in the list */
  next = gtk_tree_model_get_iter_first (store, &iter);

  while (next) {

    char *str_new;

    /* Walk through the list, reading each row. */
    gtk_tree_model_get (store, &iter, 0, &str_new, -1);
    geda_struct_attrib_add_entry(str_new);
    g_free(str_new);
    next = gtk_tree_model_iter_next (store, &iter);
  }
  return 0;
}

/*!
 * \brief Preferences Dialog add_selected_attribute Helper
 * \par Function Description:
 *  This is a Group 2 support function to check if a given attribute
 *  is already in the filter Treeview.
 *
 *  \param[in]  list  The Treeview list to look in
 *  \param[in]  str   String to look for
 */
static bool is_not_in_list(GtkTreeView *list, const char *str)
{
  GtkListStore *store;
  bool answer = FALSE;

  bool foreach_func (GtkTreeModel *model, GtkTreePath  *path,
                     GtkTreeIter  *iter,  void *      user_data)
  {
    char *attribute;

    /* Note: here we use 'iter' and not '&iter', because we did not allocate
     * the iter on the stack and are already getting the pointer to a tree iter */
    gtk_tree_model_get (model, iter, 0, &attribute, -1);

    answer = geda_strequal( str, attribute);

    GEDA_FREE(attribute); /* gtk_tree_model_get made copies of strings */
    return answer; /* stop walking the store if found, else call us with next row */
  }

  store = GTK_LIST_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW(list)));
  gtk_tree_model_foreach(GTK_TREE_MODEL(store), foreach_func, NULL);

  return !answer;
}

/*!
 * \brief Preferences Dialog Move selected attribute up in the list
 * \par Function Description:
 *  This is a Group 2 support function to move an attribute up in the
 *  Potential (left) Treeview list.
 */
static void increment_selected_attribute( void ){

  GtkTreeIter       iter1;
  GtkTreeModel     *model;
  GtkTreeSelection *selection;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(PotentialAttributesView));

  if (gtk_tree_selection_get_selected (selection, &model, &iter1)) {

    GtkTreeIter *iter2 = gtk_tree_iter_copy (&iter1);

    if (geda_tree_model_iter_previous (model, iter2)) {
      gtk_list_store_swap (GTK_LIST_STORE(model), &iter1, iter2);
    }
  }
}

/*!
 * \brief Preferences Dialog Move selected attribute down in the list
 * \par Function Description:
 *  This is a Group 2 support function to move an attribute down in the
 *  Potential (left) Treeview list.
*/
static void decrement_selected_attribute( void ) {

  GtkTreeIter       iter1;
  GtkTreeModel     *model;
  GtkTreeSelection *selection;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(PotentialAttributesView));

  if (gtk_tree_selection_get_selected (selection, &model, &iter1)) {

    GtkTreeIter *iter2 = gtk_tree_iter_copy (&iter1);

    if (gtk_tree_model_iter_next (model, iter2)) {
      gtk_list_store_swap (GTK_LIST_STORE(model), &iter1, iter2);
    }
  }
}

/*!
 * \brief Preferences Dialog Add selected attribute up in the list
 * \par Function Description:
 *  This is a Group 2 support function to add the selected attribute from
 *  the left list to the right list but only if the attribute is not already
 *  in the list. The attribute is inserted at the current selected position,
 *  or appends to end of the filter list if there is no selection.
 */
static void add_selected_attribute( void ) {

  GtkTreeSelection *l_selection;
  GtkListStore     *l_store;
  GtkTreeIter       l_iter;

  /* Get selection for the Left list */
  l_selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(PotentialAttributesView));

  if (gtk_tree_selection_get_selected( l_selection, (GtkTreeModel**) &l_store, &l_iter))
  {
    char *value;

    /* If a row was selected then l_selection, l_model, and l_iter were set so
       retrieve source string */
    gtk_tree_model_get(GTK_TREE_MODEL(l_store), &l_iter, 0, &value,  -1);

    if (is_not_in_list((GtkTreeView*) SelectedAttributesView, value)) { /* if not already in r list */

      GtkTreeSelection *r_selection;
      GtkListStore     *r_store;
      GtkTreeIter       r_iter;
      GtkTreeIter       n_iter;

      /* check if there is a selection in right list */
      r_selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(SelectedAttributesView));

      /* if there was a selection then r_iter set else GTK set r_iter to NULL */
      if (gtk_tree_selection_get_selected( r_selection, (GtkTreeModel**) &r_store, &r_iter))
      {
        /* if r_iter = NULL then is suppose to be appended to end but doens't seem to work */
        gtk_list_store_insert_before(r_store, &n_iter, &r_iter);
      }
      else
      {
        /* so do this instead */
        r_store = GTK_LIST_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW (SelectedAttributesView)));
        gtk_list_store_append(r_store, &n_iter);
      }
      /* save the value to the right list */
      gtk_list_store_set(r_store, &n_iter, 0, value, -1); /* Column 0 */

    }
    GEDA_FREE(value); /* Don't free unless we used it, less we be cursed */
  } /* endif there was an attribute selected in the left list */
}

/*!
 * \brief Preferences Dialog remove_selected_attribute
 * \par Function Description:
 *  This is a Group 2 support function to remove the selected attribute
 *  from the filter list.
 */
static void remove_selected_attribute( void ) {

  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeIter iter;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(SelectedAttributesView));

  if (gtk_tree_selection_get_selected( selection, &model, &iter)) {
    gtk_list_store_remove (GTK_LIST_STORE (model),&iter);
  }
}

/*!
 * \brief Preferences Dialog clear_attributes
 * \par Function Description:
 *  This is a Group 2 support function to clear all of the attributes
 *  from the right Treeview, aka the filter list.
 */
static void clear_attributes( void ){

  GtkListStore *store;

  store = GTK_LIST_STORE(gtk_tree_view_get_model (GTK_TREE_VIEW (SelectedAttributesView)));
  gtk_list_store_clear(store);
}

/*!
 * \brief Preferences Dialog filter_list_set_default
 * \par Function Description:
 *  This is a Group 2 support function to restore the filter list
 *  in the right Treeview using a pre-compiled string array.
 */
static void filter_list_set_default( void )
{
  const char *question = _("Clear attributes and restore default filter list?");
  int response = x_dialog_confirmation(question, GTK_MESSAGE_INFO, FALSE);
  if (response == GEDA_RESPONSE_YES) {
    clear_attributes();
    load_tree_view_str(GTK_TREE_VIEW (SelectedAttributesView), View2DefaultData);
  }
}

/** @} endgroup Preferences-Dialog-Attributes-Group-2 */
/** @} endgroup Preferences-Dialog-Attributes */

/* ------------------------ Button Support Functions ------------------------*/

/*!
 *  \defgroup Preferences-Dialog-Buttons Settings Dialog Button Support Functions
 *  @{
 *  \memberof (Preferences-Dialog-Support)
 *  \par
 *  Settings dialog button support functions are divided into two sub-groups.
 */

/* --------------------- Button Support Functions Group 1 -------------------*/

/*!
 *  \defgroup Preferences-Dialog-Buttons-Group-1 Button Support Group 1 Functions
 *  @{
 *  \memberof (Preferences-Dialog-Buttons)
 *  \weakgroup Preferences-Dialog-Attributes
 *  \par
 *   The only member of this groups is a button responder to handler signals
 *   generated from buttons on the Attributes TAB.
 *
 *      1. butt_responder
*/

/*! \brief Preferences Dialog butt_responder
 *  \par Function Description: This callback function response to signals
 *       generated from buttons on the Attribute TAB, (the ones between the
 *       Viewtrees).
 */
static
void butt_responder(GtkWidget *widget, GdkEventButton *event, ControlID *Control)
{
  if (GTK_IS_BUTTON(widget)) {

    int WhatHappend = event->type;
    int WhichButt = (int)(long)Control;

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

/** @} endgroup Preferences-Dialog-Buttons-Group-1 */

/* -------------------------- End Attributes Support ------------------------*/

/* --------------------- Button Support Functions Group 2 -------------------*/
/*!
 *  \defgroup Preferences-Dialog-Buttons-Group-2 Button Support Group 2 Functions
 *  @{
 *  \memberof (Preferences-Dialog-Buttons)
 *  \par
 *   Currently, the remainder of the buttons on the Settings dialog are for
 *   setting color preferences. These color buttons have a supporting utility
 *   menu accessible from the right mouse button that can be used to restore
 *   default colors. Popup menu support for the color buttons are all in
 *   Preferences-Dialog-Buttons-Group-2
*/
/*!
 *  \defgroup Preferences-Dialog-Color-Popup Color Button Popup Functions
 *  @{
 *  \memberof (Preferences-Dialog-Buttons-Group-2)
 *  \par
 *   The functions to support color buttons are:
 *
 *      1. color_button_popup_menu_callback
 *      2. default_color_button_popup
 *      3. color_butt_responder
*/

static GtkWidget *popup_menu; /* Seems safer to use a global for this */

/*! \brief Restore Default Color Setting - Display Menu item responder
 *  \par Function Description
 *   Called when once of the two menu items on the "restore default
 *   color" mini menu is selected.
 *
 *  \remarks to add a color button \sa color_butt_responder
*/
static void
color_button_popup_menu_callback (GedaMenuItem *item, void * data)
{
  bool restore_default_color;

  restore_default_color = (int)(long)data;

  if (restore_default_color) {

    int color_index;
    GtkColorButton *button;

    color_index = (int)(long)(GEDA_OBJECT_GET_DATA(item, "color-index"));
    button      = GEDA_OBJECT_GET_DATA(item, "color-button");

    gtk_color_button_set_color(button, geda_color_x11_color_from_index(color_index));
  }

  gtk_widget_destroy(popup_menu);
}

static bool color_button_popup_destroy(GtkWidget *widget, void *data)
{
  if (GEDA_IS_MENU(popup_menu)) {
    g_object_unref(popup_menu);
    popup_menu = NULL;
  }
  return FALSE;
}

/*! \brief Restore Default Color Setting - Display Menu
 *  \par Function Description
 *   We need used to be able to restore default colors, since the Color
 *   Selector Dialog is a child of button, only display after the button
 *   pressed, we can not easily add a "restore default" button to the
 *   action area, so instead we have the user right click on the color
 *   button and display a "mini" menu with just two choices, restore
 *   defaults or cancel. This function creates that menu and sets up
 *   the callback to the preceding function after embedding the
 *   pertinent data in the menu item.
 *
 *  \remarks to add a color button \sa color_butt_responder
*/
static void default_color_button_popup (GtkColorButton *button, GdkEventButton *event, int index)
{
  GtkWidget *item;

  color_button_popup_destroy(NULL, NULL);

  popup_menu = geda_menu_new ();

  item = geda_image_menu_item_new_with_label (_("Restore default"));

  GEDA_OBJECT_SET_DATA(item, (void*)(long)(index), "color-index");

  GEDA_OBJECT_SET_DATA(item, button, "color-button");

  g_signal_connect (item, "activate",
                    G_CALLBACK (color_button_popup_menu_callback), (void*)(long) (1));

  geda_menu_append (popup_menu, item);

  item = geda_image_menu_item_new_with_label (_("Cancel"));

  g_signal_connect (item, "activate",
                    G_CALLBACK (color_button_popup_menu_callback), (void*)(long) (0));

  geda_menu_append (popup_menu, item);

  gtk_widget_show_all (popup_menu);

  geda_menu_popup(GEDA_MENU(popup_menu), NULL, NULL, NULL, NULL, event->button, event->time);

  g_timeout_add_seconds (5, (GSourceFunc)color_button_popup_destroy, NULL);
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
    int WhichButt   = (int)(long)Control;

    if (event->button == 3) { /* only interest in right button down */

      if (WhatHappend == GDK_BUTTON_PRESS) {

        int color_index = -1;

        resolved = TRUE;

        switch ( WhichButt ) {
         case GripStrokeColor:
           color_index = SELECT_COLOR;
           break;
         case GripFillColor:
           color_index = BACKGROUND_COLOR;
           break;
         case MeshMinorColor:
           color_index = MESH_GRID_MINOR_COLOR;
           break;
         case MeshMajorColor:
           color_index = MESH_GRID_MAJOR_COLOR;
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
  }
  return resolved;
}

/** @} endgroup Preferences-Dialog-Color-Popup */

/* ---------------------------- End Button Support --------------------------*/

/** @} endgroup Preferences-Dialog-Buttons-Group-2 */
/** @} endgroup Preferences-Dialog-Buttons */

/* ------------------------ ComboBox Support Functions ----------------------*/
/*!
 *  \defgroup Preferences-Dialog-ComboBox Settings Dialog ComboBox Support Functions
 *  @{
 *  \memberof (Preferences-Dialog-Support)
 *  \par
 *   The functions to support ComboBox on the settings dialog:
 *
 *      1. combo_responder
 *      2. setup_titleblock_combo
 *      3. setup_font_name_combo
 *      4. on_change_renderer
 *      5. setup_ripper_symbol_combo
*/

/*! \brief Preferences Dialog combo_responder
 *  \par Function Description: This callback function is used to set the
 *       sensitivity of other controls based on combo-box selections.
 */
void combo_responder(GtkWidget *widget, void * data)
{
  int WhichComboBox = (int)(long)data;
/*
  int row;
  row = gtk_combo_box_get_active (GTK_COMBO_BOX (widget));
 */

  switch ( WhichComboBox ) {
  case TitleBlock:              /* GEDA_COMBO_BOX_TEXT */
    break;
  case DotGridMode:
    if (GetGedaCombo (DotGridMode) == DOTS_GRID_VARIABLE_MODE)
       gtk_widget_set_sensitive (DotGridThresholdSpin, FALSE);
    else
       gtk_widget_set_sensitive (DotGridThresholdSpin, TRUE);
    break;
  case ConsoleWindowType:
  case PointerCursor:
  case MiddleButton:
  case ThirdButton:
  case UndoType:
  case FontName:              /* GEDA_COMBO_BOX_TEXT */
  case RipperSymbol:
  case Renderer:
  case AntiAlias:
  case ColorMapScheme:
    break;
  default:
    BUG_IMSG( "Unknown Combo Id", WhichComboBox);
  }

 return;
}

#ifndef DEBUG /* When debugging the combo is loaded with static data */

/*! \brief setup_titleblock_combo loads combo box with list of title-blocks
 *
 *  \par Function Description: This function allocates and arrays and calls
 *   get_titleblock_list to get a list of the sym files in the title-block
 *   folder, the name of files are appended to the combo-box without the
 *   .sym extension, after adding a "None" options. If the current title
     block is found then the combo is activated to this entry.
 *
 *  \param[in] titleblock  ptr to name of current default titleblock.
 */
static
int setup_titleblock_combo( char *titleblock ) {

  char **strBuffer;
  int    number_of_buffers;
  int    pos = -1;

  /* Add option to disable automatic addition of a title-block */
  LOAD_GEDA_TEXT_COMBO (TitleBlock, "None");

  number_of_buffers = get_titleblock_cnt(); /* get count of files */

  strBuffer = malloc(number_of_buffers * sizeof(char *));

  if (strBuffer) {

    int i;


    for (i=0; i<number_of_buffers; i++) {
       strBuffer[i] = malloc( MAX_FILENAME ); /* be 64 */
     }

     get_titleblock_list(strBuffer); /* get list of files */

     /* Maybe someone really smart can fix */


     geda_remove_extension(titleblock);

     i = 0;
     while (i < number_of_buffers){
        if (geda_utility_string_strequal(titleblock, strBuffer[i])) pos = i;
        LOAD_GEDA_TEXT_COMBO (TitleBlock, strBuffer[i++]);
     }
     if (pos >= 0) {
       pos++;               /* add 1 extra because we added "None"*/
       SetGedaCombo (TitleBlock, pos); /* set the entry field */
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

static int
cmp_families (const void *a, const void *b)
{
  const char *a_name = pango_font_family_get_name (*(PangoFontFamily **)a);
  const char *b_name = pango_font_family_get_name (*(PangoFontFamily **)b);

  return g_utf8_collate (a_name, b_name);
}

/*! \brief Loads Font Name Combo Box and Set Active
 *  \par Function Description:
 *   This function up-loads font name strings into the FontName combobox
 *   based on the value in rc_options.render_adaptor. The list returned by
 *   Pango is used when Cairo is the renderer, when libgedadraw is used,
 *   the list returned by x_draw_get_font_list is filtered to retrieve the
 *   font provider and the font family name into a temporary buffer where
 *   both are capitalized for aesthetics. Only unique entry are added to
 *   the combobox. If one of strings matches the given font name, then that
 *   entry is set to be the active combo entry, otherwise the combo entry
 *   will be blank.
 *
 *  \param [in] w_current   The GschemToplevel object
 *  \param [in] cur_font    ptr to name of current font (to match)
 */
void setup_font_name_combo(GschemToplevel *w_current, char *cur_font) {

  GedaList   *font_list;
  GList      *iter;
  const char *pfont;
  int         current;
  int         index;
  int         n_families;

  font_list = geda_list_new();

  if (rc_options.render_adaptor == CAIRO_ADAPTOR) {

    PangoContext     *context;
    PangoFontFamily **families;

    context = gtk_widget_get_pango_context ( GTK_WIDGET (w_current->drawing_area));
    pango_context_list_families (context, &families, &n_families);
    qsort (families, n_families, sizeof(PangoFontFamily*), cmp_families);

    /* Load the output list with data from Pango */
    for (index = 0; index < n_families; index++) {
      pfont = pango_font_family_get_name (families[index]);

#if DEBUG
      bool is_mono =  pango_font_family_is_monospace (families[index]);
      fprintf(stderr, "font <%s> monospace <%d>\n", pfont, is_mono);
#endif

      geda_list_add_unique_string (font_list, geda_strdup(pfont));
    }
    GEDA_FREE (families);
  }
  else { /* Load glist from libgedadraw supplied list */

    GArray *fonts;
    char    strBuffer[256];


#ifdef WITH_LIBGEDADRAW

    fonts = x_draw_get_font_list(NULL);

#else

    fonts = NULL;

#endif

    if (fonts) {

      /* Index thru all fonts strings in the array */
      for(index = 0; index < fonts->len; index++) {

        char *family   = NULL;
        char *provider = NULL;
        char *ptr;
        int   pos, length;

        pfont = g_array_index (fonts, char*, index);

        length = strlen(pfont);

        if (length > sizeof(strBuffer))
          length = sizeof(strBuffer);

        ptr = strncpy(&strBuffer[0], pfont, length);

        pos = 0;

        if (strBuffer[0] == ASCII_MINUS) {

          provider = ++ptr;

          if (islower(strBuffer[1])) {
            strBuffer[1] = strBuffer[1] ^ 0x20;
          }
          while (++pos < length) {
            if (strBuffer[pos] == ASCII_MINUS) {
              strBuffer[pos] = '\0';
              pos++;
              break;
            }
          }
        }

        if (strBuffer[0] != ASCII_MINUS || !family) {
          family = &strBuffer[pos];
          if (islower(strBuffer[pos])) {
            strBuffer[pos] = strBuffer[pos] ^ 0x20;
          }
          while (++pos < length) {
            if (strBuffer[pos] == ASCII_MINUS) {
              strBuffer[pos] = '\0';
              ++pos;
              break;
            }
            /* Look for spaces in family */
            if (strBuffer[pos] == ASCII_SPACE) {
              if (islower(strBuffer[pos + 1])) {
                strBuffer[pos + 1] = strBuffer[pos + 1] ^ 0x20; /* Capitalize first character */
              }
            }
          }
          strBuffer[pos] = '\0';
        }

        ptr = NULL;

        if(provider && family) {

#ifdef HAVE_XFT
          ptr = geda_sprintf("%s-%s", provider, family);
#else
          ptr = geda_sprintf("%s, %s", provider, family);
#endif

        }
        else if (family) {
          ptr = geda_utility_string_strdup(family);
        }

        if (ptr) {
          geda_list_add_unique_string (font_list, ptr);
        }
      }                                                    /* Next font string in array */
      g_array_free(fonts, TRUE);
    }
    font_list->glist = g_list_sort (font_list->glist, (GCompareFunc)strcmp);
  }                                                      /* else was for libgedadraw */

  /* Load the FontName Combo from the Glist and look for cur_font */
  current = -1;
  index   = 0;
  for (iter = geda_list_get_glist(font_list); iter; iter = iter->next) {

    pfont = iter->data;

    char *gtk_blunder = strstr(pfont, "&");

    if (gtk_blunder) {

      *gtk_blunder = '-';

    }

    LOAD_GEDA_TEXT_COMBO (FontName, pfont);

    /* current < 0 here means do not keep looking, but we got to keep loading */
    if ( current < 0 && cur_font && geda_strequal(cur_font, pfont)) {
      current = index;
    }
    index++;
  }

  if (current < 0) { /* If we did not find exact match, try harder */

    char *reduced;

    index   = 0;

#ifdef WITH_LIBGEDADRAW

    reduced = x_draw_strip_font_provider(cur_font);

#else

    reduced = NULL;

#endif


    if (reduced) {

      for (iter = geda_list_get_glist(font_list); iter; iter = iter->next)
      {
        const char *needle;
        const char *haystack;

        pfont = iter->data;

        /* The longer string needs to be the haystack */
        if (strlen(pfont) > strlen(reduced)) {
          haystack = pfont; needle = reduced;
        }
        else {
          haystack = reduced; needle = pfont;
        }

        if (pfont && geda_stristr(haystack, needle) >= 0) {
          current = index;
          break;
        }

        index++;
      }
      GEDA_FREE (reduced);
    }
  }

  geda_list_free_full (font_list);
  g_object_unref(font_list);

  geda_combo_box_set_active((GedaComboBox *)FontNameCombo, current);

}

/* TODO: This should be moved to the Combo responder once the GTK Combo
 *       are converted to GedaCombo's */
static void
on_change_renderer (GtkWidget *widget, void *user_data)
{
  GtkComboBox      *combo      = (GtkComboBox*) widget; // callee */
  GedaComboBoxText *font_combo = (GedaComboBoxText*) FontNameCombo;
  GschemToplevel   *w_current  = user_data;
  EdaConfig        *cfg        = eda_config_get_user_context ();
  const char       *group      = IVAR_CONFIG_GROUP;
  char             *name_now;
  char             *prev_name;

  /* Set which render is to be used in the temporary block */
  rc_options.render_adaptor = gtk_combo_box_get_active(combo);

  /* Don't free the font name, the string belongs to the dialog control */
  name_now = geda_combo_box_text_get_active_text (font_combo);

  /* Preserve the current font name */
  if (rc_options.render_adaptor == CAIRO_ADAPTOR) { /* name_now is for X11 */
    eda_config_set_string (cfg, group, "last-draw-font", name_now);
    prev_name = i_var_get_global_config_string (cfg, "last-cairo-font");
  }
  else { /* name_now is for Cairo */
    eda_config_set_string (cfg, group, "last-cairo-font", name_now);
    prev_name = i_var_get_global_config_string (cfg, "last-draw-font");
  }

  /* Empty out the */
  geda_combo_box_text_remove_all(font_combo);

  if (prev_name != NULL) {
    setup_font_name_combo(w_current, prev_name);
    GEDA_FREE (prev_name);
  }
  else {
    /* There was no preserved value, pass the current font name and let
     * setup_font_name_combo() sort out the new font name */
    setup_font_name_combo(w_current, name_now);
  }

  GEDA_FREE (name_now); /* Free the string from libgedauio */
  GEDA_FREE (prev_name);
}

/*! \brief Set Ripper Symbol Name Combo Box Active
 *  \par Function Description:
 *   The RipperSymbol combo-box was load when the dialog was created,
 *   this function copies the passed symbol name to the temporary
 *   structure and sets the corresponding combo-box index active.
 *
 *  \param [in] cur_name  ptr to name of ripper symbol name (to match)
 */
void setup_ripper_symbol_combo(char *cur_name) {

  strcpy(rc_options.ripper_symbol_fname, cur_name);

  if (geda_strequal(rc_options.ripper_symbol_fname, DEFAULT_BUS_RIPPER_SYMNAME))
    rc_options.ripper_symbol_index = 0;
  else {
    if (geda_strequal(rc_options.ripper_symbol_fname, SECOND_BUS_RIPPER_SYMNAME))
      rc_options.ripper_symbol_index = 1;
    else {
      LOAD_STD_COMBO(RipperSymbol, rc_options.ripper_symbol_fname);
      rc_options.ripper_symbol_index = 2;
    }
  }

  gtk_combo_box_set_active((GtkComboBox *)RipperSymbolCombo, rc_options.ripper_symbol_index);

}

/** @} endgroup Preferences-Dialog-ComboBox */

/* -------------------------- End Combo Box Support -------------------------*/

/* --------------------- Multi Control Callback Resonders -------------------*/

/*!
 *  \defgroup Preferences-Dialog-MultiWidget-Responders Multi-Widget Support Functions
 *  @{
 *  \memberof (Preferences-Dialog-Support)
 *  \par
 *  The functions in this groups are callback responders to handle signal from
 *  radio buttons and switches (check) buttons on all Settings dialog TABs.
 *
 *      1. radio_responder
 *      2. switch_responder
*/

/*! \brief Preferences Dialog to toggle radio images on the Preferences Dialog
 *  \par Function Description: This function changes the images of
 *       controls created with create_geda_switch to the opposite
 *       state, i.e. if ON use OFF image and if OFF use ON image.
 */
static void
radio_responder(GtkWidget *widget,  int response, ControlID *Control)
{
  bool state = GET_SWITCH_STATE (widget);

  if ((GTK_IS_BUTTON(widget)) && (state != TRUE)) {

    x_dialog_set_bulb_on(widget);

    switch ( response ) {
    /* General TAB */
      case LogDestinyWindow:
        x_dialog_set_bulb_off(LogDestinyTTYRadio); x_dialog_set_bulb_off(LogDestinyBothRadio);
        break;
      case LogDestinyTTY:
        x_dialog_set_bulb_off(LogDestinyWindowRadio); x_dialog_set_bulb_off(LogDestinyBothRadio);
        break;
      case LogDestinyBoth:
        x_dialog_set_bulb_off(LogDestinyWindowRadio); x_dialog_set_bulb_off(LogDestinyTTYRadio);
        break;
   /* Edit TAB */
      case NetEndPointNone:
        x_dialog_set_bulb_off(NetEndPointFilledRadio); x_dialog_set_bulb_off(NetEndPointEmptyRadio);
        break;
      case NetEndPointFilled:
        x_dialog_set_bulb_off(NetEndPointNoneRadio); x_dialog_set_bulb_off(NetEndPointEmptyRadio);
        break;
      case NetEndPointEmpty:
        x_dialog_set_bulb_off(NetEndPointNoneRadio); x_dialog_set_bulb_off(NetEndPointFilledRadio);
        break;
      case NetMidPointNone:
        x_dialog_set_bulb_off(NetMidPointFilledRadio); x_dialog_set_bulb_off(NetMidPointEmptyRadio);
        break;
      case NetMidPointFilled:
        x_dialog_set_bulb_off(NetMidPointNoneRadio); x_dialog_set_bulb_off(NetMidPointEmptyRadio);
        break;
      case NetMidPointEmpty:
        x_dialog_set_bulb_off(NetMidPointNoneRadio); x_dialog_set_bulb_off(NetMidPointFilledRadio);
        break;
      case NetSelectionNone:
        x_dialog_set_bulb_off(NetSelectionNetRadio); x_dialog_set_bulb_off(NetSelectionAllRadio);
        break;
      case NetSelectionNet:
        x_dialog_set_bulb_off(NetSelectionNoneRadio); x_dialog_set_bulb_off(NetSelectionAllRadio);
        break;
      case NetSelectionAll:
        x_dialog_set_bulb_off(NetSelectionNoneRadio); x_dialog_set_bulb_off(NetSelectionNetRadio);
        break;
  /* Styles TAB */
      case BusStyleNone:
        x_dialog_set_bulb_off(BusStyleThinRadio); x_dialog_set_bulb_off(BusStyleThickRadio);
        break;
      case BusStyleThin:
        x_dialog_set_bulb_off(BusStyleNoneRadio); x_dialog_set_bulb_off(BusStyleThickRadio);
        break;
      case BusStyleThick:
        x_dialog_set_bulb_off( BusStyleNoneRadio); x_dialog_set_bulb_off(BusStyleThinRadio);
        break;
      case NetStyleNone:
        x_dialog_set_bulb_off(NetStyleThinRadio); x_dialog_set_bulb_off(NetStyleThickRadio);
        break;
      case NetStyleThin:
        x_dialog_set_bulb_off(NetStyleNoneRadio); x_dialog_set_bulb_off(NetStyleThickRadio);
        break;
      case NetStyleThick:
        x_dialog_set_bulb_off( NetStyleNoneRadio); x_dialog_set_bulb_off(NetStyleThinRadio);
        break;
      case LineStyleNone:
        x_dialog_set_bulb_off(LineStyleThinRadio); x_dialog_set_bulb_off(LineStyleThickRadio);
        break;
      case LineStyleThin:
        x_dialog_set_bulb_off(LineStyleNoneRadio); x_dialog_set_bulb_off(LineStyleThickRadio);
        break;
      case LineStyleThick:
        x_dialog_set_bulb_off( LineStyleNoneRadio); x_dialog_set_bulb_off(LineStyleThinRadio);
        break;
      case PinStyleNone:
        x_dialog_set_bulb_off(PinStyleThinRadio); x_dialog_set_bulb_off(PinStyleThickRadio);
        break;
      case PinStyleThin:
        x_dialog_set_bulb_off(PinStyleNoneRadio); x_dialog_set_bulb_off(PinStyleThickRadio);
        break;
      case PinStyleThick:
        x_dialog_set_bulb_off( PinStyleNoneRadio); x_dialog_set_bulb_off(PinStyleThinRadio);
        break;
  /* Text TAB */
      case CapsStyleLower:
        x_dialog_set_bulb_off(CapsStyleUpperRadio); x_dialog_set_bulb_off(CapsStyleBothRadio);
        break;
      case CapsStyleUpper:
        x_dialog_set_bulb_off(CapsStyleLowerRadio); x_dialog_set_bulb_off(CapsStyleBothRadio);
        break;
      case CapsStyleBoth:
        x_dialog_set_bulb_off( CapsStyleLowerRadio); x_dialog_set_bulb_off(CapsStyleUpperRadio);
        break;
      case TextFeedbackDefault:
        x_dialog_set_bulb_off(TextFeedbackReadableRadio); x_dialog_set_bulb_off(TextFeedbackAlwaysRadio);
        break;
      case TextFeedbackReadable:
        x_dialog_set_bulb_off(TextFeedbackDefaultRadio); x_dialog_set_bulb_off(TextFeedbackAlwaysRadio);
        break;
      case TextFeedbackAlways:
        x_dialog_set_bulb_off(TextFeedbackDefaultRadio); x_dialog_set_bulb_off(TextFeedbackReadableRadio);
        break;
  /* Windows TAB */
      case GridDotSizeOne:
        x_dialog_set_bulb_off(GridDotSizeTwoRadio); x_dialog_set_bulb_off(GridDotSizeThreeRadio);
        break;
      case GridDotSizeTwo:
        x_dialog_set_bulb_off(GridDotSizeOneRadio); x_dialog_set_bulb_off(GridDotSizeThreeRadio);
        break;
      case GridDotSizeThree:
        x_dialog_set_bulb_off(GridDotSizeOneRadio); x_dialog_set_bulb_off(GridDotSizeTwoRadio);
        break;
      case GridModeNone:
        x_dialog_set_bulb_off(GridModeDotsRadio); x_dialog_set_bulb_off(GridModeMeshRadio);
        break;
      case GridModeDots:
        x_dialog_set_bulb_off(GridModeNoneRadio); x_dialog_set_bulb_off(GridModeMeshRadio);
        break;
      case GridModeMesh:
        x_dialog_set_bulb_off(GridModeNoneRadio); x_dialog_set_bulb_off(GridModeDotsRadio);
        break;
      case WindowSizeW650H487:       x_dialog_set_bulb_off(WindowSizeW900H650Radio);
        x_dialog_set_bulb_off(WindowSizeW950H712Radio); x_dialog_set_bulb_off(WindowSizeW1100H825Radio);
        break;
      case WindowSizeW900H650:       x_dialog_set_bulb_off(WindowSizeW650H487Radio);
        x_dialog_set_bulb_off(WindowSizeW950H712Radio); x_dialog_set_bulb_off(WindowSizeW1100H825Radio);
        break;
      case WindowSizeW950H712:       x_dialog_set_bulb_off(WindowSizeW650H487Radio);
        x_dialog_set_bulb_off(WindowSizeW900H650Radio); x_dialog_set_bulb_off(WindowSizeW1100H825Radio);
        break;
      case WindowSizeW1100H825:      x_dialog_set_bulb_off(WindowSizeW650H487Radio);
        x_dialog_set_bulb_off(WindowSizeW900H650Radio); x_dialog_set_bulb_off(WindowSizeW950H712Radio);
        break;
      case WorldSizeSmall:
        x_dialog_set_bulb_off(WorldSizeMediumRadio); x_dialog_set_bulb_off(WorldSizeLargeRadio);
        break;
      case WorldSizeMedium:
        x_dialog_set_bulb_off(WorldSizeSmallRadio); x_dialog_set_bulb_off(WorldSizeLargeRadio);
        break;
      case WorldSizeLarge:
        x_dialog_set_bulb_off(WorldSizeSmallRadio); x_dialog_set_bulb_off(WorldSizeMediumRadio);
        break;
  /* Attributes TAB */
      case DialogListAttributesAll:
        enable_attribute_list_controls(FALSE);
        x_dialog_set_bulb_off(DialogListAttributesNoneRadio); x_dialog_set_bulb_off(DialogListAttributesListRadio);
        break;
      case DialogListAttributesNone:
        enable_attribute_list_controls(FALSE);
        x_dialog_set_bulb_off(DialogListAttributesAllRadio); x_dialog_set_bulb_off(DialogListAttributesListRadio);
        break;
      case DialogListAttributesList:
        x_dialog_set_bulb_off(DialogListAttributesAllRadio); x_dialog_set_bulb_off(DialogListAttributesNoneRadio);
        enable_attribute_list_controls(TRUE);
        break;
      default:
        break;
    }
  }

  return;
}

/*! \brief Preferences Dialog to toggle switch images
 *  \par Function Description: This function changes the images of
 *       controls created with create_geda_switch to the opposite
 *       state, i.e. if ON use OFF image and if OFF use ON image.
 *       The functions enables or disables other widgets based on
 *       the state of the switch.
 */
static void switch_responder(GtkWidget *widget, int response,  ControlID *Control)
{
   bool state = GET_SWITCH_STATE (widget);
   GtkWidget *SwitchImage = get_geda_switch_image( state);
   gtk_button_set_image(GTK_BUTTON (widget), SwitchImage);

   switch ( response ) {
   case AutoLoadLast:
     break;

   case AutoSave:
     gtk_widget_set_sensitive (AutoSaveIntervalSpin, state);
     break;

   case ClassicWheel:
   case ConsolidateNets:
   case ContinuePlace:
   case DelayScrolling:
   case DragMove:
     break;

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
   case ScrollBarsVisible:
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

/** @} endgroup Preferences-Dialog-MultiWidget-Responders */

/* ------------------- End Multi Control Callback Resonders -----------------*/

/** @} endgroup Preferences-Dialog-Support */

/** \defgroup Preferences-Dialog-Loader Settings Dialog Loader
 *  @{
 *  \memberof (Preferences-Dialog-Support)
 */

/*! \brief Upload values into the Settings Dialog
 *  \par Function Description: This function sets the value of controls after
 *  the dialog has been created based on the current settings, in so much as
 *  possible. Some configurations options are SCM scripts and we have to find
 *  a way to deal with them ...
 */
bool load_settings_dialog (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  EdaConfig    *cfg      = eda_config_get_user_context();
  const char   *group    = IVAR_CONFIG_GROUP;

  char  *tmpstr;
  const  GdkColor *color;
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

  color = eda_renderer_get_grips_stroke_color (CairoRenderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(GripStrokeColorButt), color);

  color = eda_renderer_get_grips_fill_color (CairoRenderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(GripFillColorButt), color);

  color = &w_current->mesh_grid_minor_color;
  gtk_color_button_set_color(GTK_COLOR_BUTTON(MeshMinorColorButt), color);

  color = &w_current->mesh_grid_major_color;
  gtk_color_button_set_color(GTK_COLOR_BUTTON(MeshMajorColorButt), color);

  color = eda_renderer_get_net_endpoint_color (CairoRenderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(NetEndpointColorButt), color);

  color = eda_renderer_get_text_marker_color (CairoRenderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(TextMarkerColorButt), color);

  color = eda_renderer_get_junction_color (CairoRenderer);
  gtk_color_button_set_color(GTK_COLOR_BUTTON(JunctionColorButt), color);

/* Combo Boxes (7) */

 /* Using value for green gun @ index 10 to identify which color map
  * was loaded. (both mapping and outline must be on if a color map
  * was loaded)

    00 = default mapping also BW
    ff = dark
    ee = light

    TODO: Really should check all indexes
 */
  cflag = geda_color_x11_display_lookup(10); /* index of "bus" */
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

  SetGedaCombo (DotGridMode,       w_current->dots_grid_mode);
  SetGedaCombo (ConsoleWindowType, console_window_type);
  SetGedaCombo (ThirdButton,       w_current->third_button);
  SetGedaCombo (PointerCursor,     w_current->drawing_pointer);
  SetGedaCombo (MiddleButton,      w_current->middle_button);

#ifdef DEBUG
  LOAD_GEDA_TEXT_COMBO( TitleBlock, DefaultTitleBlockList );
  SetGedaCombo (TitleBlock, 1);
#else
  /* make C variable on stack */
  tmpstr = eda_config_get_string (cfg, group, "default-titleblock", NULL);
  setup_titleblock_combo(tmpstr);
  GEDA_FREE (tmpstr);
#endif

  SetGedaCombo (UndoType, w_current->undo_type);

  /* Note: This should be set before calling setup_font_name_combo */
  rc_options.render_adaptor = w_current->render_adaptor;

  tmpstr = eda_config_get_string (cfg, group, "default-font-name", NULL);
  setup_font_name_combo(w_current, tmpstr);
  GEDA_FREE (tmpstr);

  setup_ripper_symbol_combo(w_current->bus_ripper_symname);

  SetCombo ( Renderer, w_current->render_adaptor );
  SetCombo ( AntiAlias, w_current->anti_aliasing );

  tmpstr = eda_config_get_string (cfg, group, "default-filename", NULL);
  SetEntryText( UntitledNameEntry, tmpstr );
  GEDA_FREE (tmpstr);

/* The Switches Alphabetically (31) */

  SetSwitch(AutoLoadLast, auto_load_last);
  SetSwitch(AutoSave, geda_toplevel_get_auto_save_interval(toplevel));
  SetSwitch(EnableUndo, w_current->undo_control);
  SetSwitch(ClassicWheel, w_current->scroll_wheel);
  SetSwitch(ConsolidateNets, geda_toplevel_get_net_consolidate(toplevel));
  SetSwitch(ContinuePlace, w_current->continue_component_place);
  SetSwitch(DelayScrolling, w_current->scrollbar_update);
  SetSwitch(DragMove, w_current->drag_can_move);
  SetSwitch(DrawGrips, CairoRenderer->draw_grips);
  SetSwitch(EmbedComponents, w_current->embed_components);
  SetSwitch(EnableColorImaging, geda_toplevel_get_image_color(toplevel));
  SetSwitch(EnableLog, logging);

  SetSwitch(EnforceHierarchy, w_current->enforce_hierarchy);
  SetSwitch(FastMousePan, w_current->fast_mousepan);

  SetSwitch(FeedbackMode, w_current->action_feedback_mode);    /* was text_feedback */
  SetSwitch(ForceBoundingBox, w_current->force_boundingbox);   /* was action_feedback_mode */

  SetSwitch(FilePreview, w_current->file_preview);

  SetSwitch(FriendlyColorMap, rc_options.display_color_map);
  SetSwitch(FriendlyOutlineMap, rc_options.display_outline_color_map);

  SetSwitch(InitConsoleWindow, console_window);
  SetSwitch(InvertImages, geda_toplevel_get_invert_images(toplevel));
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
  SetSwitch(TextOriginMarker, CairoRenderer->text_origin_marker);
  SetSwitch(UndoViews, w_current->undo_panzoom);
  SetSwitch(WarpCursor, w_current->warp_cursor);
  SetSwitch(ZoomPan, w_current->zoom_with_pan);

/* Radios (15) */
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

  SetBulbGroup (WorldSize, rc_options.world_size);

/* Attributes TAB */
  SetBulbGroup(DialogListAttributes, GetAttributeFilterMode(w_current));

/* The Spin Controls - Alphabetically (24) */
  SetSpin (AttributeOffset, w_current->add_attribute_offset);
  SetSpin (AutoPlacementGrid, w_current->attribute_placement_grid);
  SetSpin (AutoSaveInterval, geda_toplevel_get_auto_save_interval(toplevel));
  SetSpin (DotGridThreshold, w_current->dots_grid_threshold);
  SetSpin (GripPixelSize, w_current->grip_size);
  SetSpin (JunctionSize, CairoRenderer->junction_size);
  SetSpin (KeyboardPanGain, w_current->keyboardpan_gain);
  SetSpin (MeshGridThreshold, w_current->mesh_grid_threshold);
  SetSpin (MeshGridWidth, w_current->mesh_line_width_factor);
  SetSpin (MousePanGain, w_current->mousepan_gain);
  SetSpin (RipperSize, w_current->bus_ripper_size);
  SetSpin (ScrollPanSteps, w_current->scrollpan_steps);
  SetSpin (SelectPixels, w_current->select_slack_pixels);
  SetSpin (SnapSize, w_current->snap_size);
  SetSpin (TextMarkerSize, CairoRenderer->text_marker_size);
  SetSpin (TextMarkerThld, CairoRenderer->text_marker_threshold);
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

  g_signal_connect (RendererCombo, "changed",
                    G_CALLBACK (on_change_renderer),
                    w_current);

  return TRUE;
}

/** @} endgroup Preferences-Dialog-Loader */

/** \defgroup Preferences-Dialog-Creator Settings Dialog Creator
 *  @{
 *  \memberof (Preferences-Dialog)
 * \remarks This is the "Local Module Main" routine, before "macrolization"
 * the function create_settings_dialog was > 148kbytes without supporting
 * functions. The macros somewhat obsures base coding but is much more
 * manageable then 100K+ lines of gtk_xxx's and does not depend on Glade.
 */
GtkWidget *create_settings_dialog (GschemToplevel *w_current)
{
  GtkWidget *ThisDialog;
  GtkWidget *MainDialogVBox;
  GtkWidget *notebook;

  GtkWidget *dialog_action_area;
  GtkWidget *CancelButt;
  GtkWidget *SaveButt;
  GtkWidget *OkayButt;

/*
  PangoFontDescription *FontDescription;
  FontDescription = pango_font_description_from_string("Monospace");
  pango_font_description_set_absolute_size(FontDescription, 10);
*/
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
           GTK_SWITCH (AutoOptionsRow1_hbox, AutoSave, 48, TRUE)
           GTK_NUMERIC_SPIN (AutoOptionsRow1_hbox, AutoSaveInterval, 38, 180, 60, 3600)
       HSECTION (GeneralAutoOptions_vbox, AutoOptionsRow2)    /* Grp 1 Row 2 */
           GTK_SWITCH(AutoOptionsRow2_hbox, AutoLoadLast, 7, TRUE);
           GTK_TEXT_ENTRY(AutoOptionsRow2_hbox, UntitledName, 30, DEFAULT_UNTITLED_NAME)
     VPSECTION (GeneralPrefTab_vbox, GeneralOptions, 0)  /* GT Grp 2 General Options */
       HSECTION (GeneralOptions_vbox, GeneralOptionsRow1)     /* Grp 2 Row 1 */
           GTK_SWITCH(GeneralOptionsRow1_hbox, FilePreview, 18, TRUE);
           GEDA_NEW_TEXT_ENTRY_COMBO (GeneralOptionsRow1_hbox, TitleBlock, 200, 49);
     HXYP_SEPARATOR (GeneralPrefTab_vbox, Grp3, 10);
     CSECTION_OPTIONS(GeneralPrefTab_vbox, Logging, -1, 10, H); /* GT Grp 3 Log Related */
       VSECTION (LoggingOptions_hbox, LogOptions);   /* Grp 3 Row 1 */
         GTK_SWITCH(LogOptions_vbox, EnableLog, 5, TRUE);
         GTK_SWITCH(LogOptions_vbox, InitConsoleWindow, 0, FALSE);
         GEDA_NEW_TEXT_COMBO (LogOptions_vbox, ConsoleWindowType, 170, 5);
           LOAD_GEDA_TEXT_COMBO (ConsoleWindowType, RC_STR_CONWIN_DECORATED)
           LOAD_GEDA_TEXT_COMBO (ConsoleWindowType, RC_STR_CONWIN_TRANSIENT)
         GTK_V_BULB_TRIAD (LoggingOptions_hbox, LogDestiny, 10, Window, TTY, Both, Window);
     HXYP_SEPARATOR (GeneralPrefTab_vbox, Grp4, 10);
     CSECTION_OPTIONS(GeneralPrefTab_vbox, Undo, 63, 5, H); /* was GT Grp 4 Undo Related */
       VSECTION (UndoOptions_hbox, UndoToggleOptions);
         GTK_SWITCH(UndoToggleOptions_vbox, EnableUndo, 0, TRUE);
         GTK_SWITCH(UndoToggleOptions_vbox, UndoViews, 0, FALSE);
         VXP_SEPERATOR(UndoOptions_hbox, UndoToggleOptions, 60);
         VSECTION (UndoOptions_hbox, UndoExtraOptions);
           GTK_NUMERIC_SPIN (UndoExtraOptions_vbox, UndoBufferSize, 0, 10, 1, 99);
           GEDA_NEW_TEXT_COMBO (UndoExtraOptions_vbox, UndoType, 150, 5);
             LOAD_GEDA_TEXT_COMBO (UndoType, RC_STR_UNDO_NONE)
             LOAD_GEDA_TEXT_COMBO (UndoType, RC_STR_UNDO_DISK)
             LOAD_GEDA_TEXT_COMBO (UndoType, RC_STR_UNDO_MEMORY)
     HXYP_SEPARATOR (GeneralPrefTab_vbox, End, 10);
   GTK_END_TAB(GeneralPref);

  } /*** END General TAB Contents ***/

  { /*------------------- Start Edit TAB Contents -------------------*/

   GTK_START_TAB (EditPref);
     VSECTION (EditPrefTab_vbox, EditOptions) /* ET Grp 1 Auto Options */
       GEDA_FRAME (EditOptions_vbox, Grips, -1, 110, 0.05, 0.2, 5)
         VSECTION (Grips_hbox, GripOptions)  /* Grp 1 Row 1 */
           HSECTION ( GripOptions_vbox, GripOptionsRow1)  /* Grp 1 Row 3 */
             GTK_SWITCH(GripOptionsRow1_hbox, DrawGrips, 34, TRUE);
             GTK_NUMERIC_SPIN (GripOptionsRow1_hbox, GripPixelSize, 45, 10, MIN_GRIP_SIZE, MAX_GRIP_SIZE);
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
         GTK_NUMERIC_SPIN (EditOptionsRow5_hbox, SelectPixels, 21, 10, 0, 20);
       HSECTION (EditOptions_vbox, EditOptionsRow6)    /* Grp 1 Row 6 */
         GTK_SWITCH(EditOptionsRow6_hbox, NotifyEvents, 12, TRUE);
         GTK_SWITCH(EditOptionsRow6_hbox, ObjectClipping, 0, TRUE);
     HYP_SEPARATOR (EditPrefTab_vbox, Grp2, 10);
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
         GTK_NUMERIC_SPIN (PointerRow1_hbox, ZoomGain, 63, 20, 0, 80);
       HSECTION (PointerOptions_vbox, PointerRow2)    /* Row 1 */
         GTK_SWITCH(PointerRow2_hbox, FastMousePan, 18, FALSE);
         GTK_NUMERIC_SPIN (PointerRow2_hbox, MousePanGain, 22, 1, 1, 30);
       HSECTION (PointerOptions_vbox, PointerRow3)    /* Row 3 */
         GTK_SWITCH(PointerRow3_hbox, DragMove, 18, TRUE);
         GTK_NUMERIC_SPIN (PointerRow3_hbox, ScrollPanSteps, 11, 8, 0, 80);
       HSECTION (PointerOptions_vbox, PointerRow4)    /* Row 4 */
         GTK_SWITCH(PointerRow4_hbox, ClassicWheel, 40, TRUE);
         GEDA_NEW_TEXT_COMBO (PointerRow4_hbox, PointerCursor, 150, 13);
           LOAD_GEDA_COMBO_STR( PointerCursor, CursorStrings );
         gtk_widget_set_size_request (PointerCursorCombo, 150, 31);
       HSECTION (PointerOptions_vbox, PointerRow5)    /* Row 4 */
         GTK_SWITCH(PointerRow5_hbox, PointerHScroll, 8, FALSE);
         GEDA_NEW_TEXT_COMBO (PointerRow5_hbox, MiddleButton, 150, 13);
         gtk_widget_set_size_request (MiddleButtonCombo,  150, 31);
         LOAD_GEDA_TEXT_COMBO (MiddleButton, RC_STR_MID_STROKE);
         LOAD_GEDA_TEXT_COMBO (MiddleButton, RC_STR_MID_REPEAT);
         LOAD_GEDA_TEXT_COMBO (MiddleButton, RC_STR_MID_ACTION);
         LOAD_GEDA_TEXT_COMBO (MiddleButton, RC_STR_MID_MOUSEPAN);
         LOAD_GEDA_TEXT_COMBO (MiddleButton, RC_STR_MID_MOUSEPOP);
       HSECTION (PointerOptions_vbox, PointerRow6)    /* Row 4 */
         GTK_SWITCH(PointerRow6_hbox, WarpCursor, 30, FALSE);
         GEDA_NEW_TEXT_COMBO (PointerRow6_hbox, ThirdButton,  150, 1);
         gtk_widget_set_size_request (ThirdButtonCombo, 150, 31);
         LOAD_GEDA_TEXT_COMBO (ThirdButton, _(RC_STR_3RD_POPUP));
         LOAD_GEDA_TEXT_COMBO (ThirdButton, _(RC_STR_3RD_PAN));
     HXYP_SEPARATOR (PointerPrefTab_vbox, End, 10);

   GTK_END_TAB(PointerPref);
  } /*** END Pointer TAB Contents ***/

  { /*------------------- Start Window TAB Contents -------------------*/

   GTK_START_TAB (WindowPref);
     VSECTION (WindowPrefTab_vbox, DisplaySizeOptions) /* WT Row 1 Display Size */
       GTK_H_QUAD_BULB(DisplaySizeOptions_vbox, WindowSize, 10, W650H487, W900H650, W950H712, W1100H825, W950H712);
       GTK_H_BULB_TRIAD( DisplaySizeOptions_vbox, WorldSize, Small, Medium, Large, Medium);
     HD_SEPARATOR (WindowPrefTab_vbox, Grp2);
       HPSECTION(WindowPrefTab_vbox, GridOptions, DIALOG_V_SPACING) /* WT Row 2 */
         GTK_V_BULB_TRIAD( GridOptions_hbox, GridMode, 0, None, Dots, Mesh, Mesh);
         VPSECTION(GridOptions_hbox, GridDotOptions, 50); /* WT Row 2 Grp 2 Dot Grid Options */
           GEDA_NEW_TEXT_COMBO (GridDotOptions_vbox, DotGridMode, 160, DIALOG_V_SPACING);
           LOAD_GEDA_TEXT_COMBO (DotGridMode, RC_STR_DOTS_MODE_VARIABLE);
           LOAD_GEDA_TEXT_COMBO (DotGridMode, RC_STR_DOTS_MODE_FIXED);
           GTK_NUMERIC_SPIN (GridDotOptions_vbox, DotGridThreshold, DIALOG_V_SPACING, DEFAULT_GRID_DOT_SIZE, MIN_GRID_DOT_SIZE, MAX_GRID_DOT_THRESHOLD);
     HD_SEPARATOR (WindowPrefTab_vbox, Grp3);
       HSECTION(WindowPrefTab_vbox, MeshGridSizeOptions) /* WT Row 3 */
         GTK_V_BULB_TRIAD (MeshGridSizeOptions_hbox, GridDotSize, 8, One, Two, Three, One);
         VPSECTION(MeshGridSizeOptions_hbox, GridMeshOptions, 50); /* WT Row 2 Grp 2 Dot Grid Options */
         GTK_NUMERIC_SPIN (GridMeshOptions_vbox, MeshGridThreshold, 0, DEFAULT_GRID_MESH_THRESHOLD, MIN_GRID_MESH_THRESHOLD, MAX_GRID_MESH_THRESHOLD);
         GTK_NUMERIC_SPIN (GridMeshOptions_vbox, MeshGridWidth, 0, DEFAULT_MESH_LINE_WIDTH_FACTOR, MIN_MESH_LINE_WIDTH_FACTOR, MAX_MESH_LINE_WIDTH_FACTOR);
         GEDA_COLOR_BUTTON (GridMeshOptions_vbox, MeshMinorColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, 0);
         GEDA_COLOR_BUTTON (GridMeshOptions_vbox, MeshMajorColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, 0);
     HD_SEPARATOR (WindowPrefTab_vbox, Grp4);
       GEDA_FRAME (WindowPrefTab_vbox, Scrolling, -1, 60, 0.05, 0.2, 10)
         GTK_SWITCH(Scrolling_hbox, ScrollBars,        DIALOG_H_SPACING + 10, TRUE);
         GTK_SWITCH(Scrolling_hbox, ScrollBarsVisible, DIALOG_H_SPACING + 10, TRUE);
         GTK_SWITCH(Scrolling_hbox, DelayScrolling,    DIALOG_H_SPACING + 10, FALSE);
   GTK_END_TAB(WindowPref);
  } /*** END Window TAB Contents ***/

  { /*-------------------- Start Render TAB Contents --------------------*/
   GTK_START_TAB (RenderPref);
     HSECTION(RenderPrefTab_vbox, RenderOptionsRow1); /* ST Grp 1 */
       GTK_NEW_COMBO (RenderOptionsRow1_hbox, Renderer, 10, 6);
           gtk_widget_set_size_request (RendererCombo, 120, 31);
           GTK_LOAD_COMBO (Renderer, RC_RENDERER_OPTION_CAIRO);
           GTK_LOAD_COMBO (Renderer, RC_RENDERER_OPTION_X11);
       GTK_NEW_COMBO (RenderOptionsRow1_hbox, AntiAlias, 10, 74);
           gtk_widget_set_size_request (AntiAliasCombo, 160, 31);
           GTK_LOAD_COMBO (AntiAlias, RC_STR_ANTIALIAS_DEFAULT);
           GTK_LOAD_COMBO (AntiAlias, RC_STR_ANTIALIAS_NONE);
           GTK_LOAD_COMBO (AntiAlias, RC_STR_ANTIALIAS_GRAY);
           GTK_LOAD_COMBO (AntiAlias, RC_STR_ANTIALIAS_SUBPIXEL);
           GTK_LOAD_COMBO (AntiAlias, RC_STR_ANTIALIAS_FAST);
           GTK_LOAD_COMBO (AntiAlias, RC_STR_ANTIALIAS_GOOD);
           GTK_LOAD_COMBO (AntiAlias, RC_STR_ANTIALIAS_BEST);
     HSECTION (RenderPrefTab_vbox, RenderOptionsRow2)     /* Grp 2 Row 2 */
       GTK_SWITCH(RenderOptionsRow2_hbox, EnableColorImaging, 7, FALSE);
       GTK_SWITCH(RenderOptionsRow2_hbox, FriendlyColorMap, 87, TRUE);
     HSECTION (RenderPrefTab_vbox, RenderOptionsRow3)     /* Grp 2 Row 3 */
       GTK_SWITCH(RenderOptionsRow3_hbox, InvertImages, 8, TRUE);
       GTK_SWITCH(RenderOptionsRow3_hbox, FriendlyOutlineMap, 76, TRUE);
     HSECTION (RenderPrefTab_vbox, RenderOptionsRow4)     /* Grp 2 Row 4 */
       GTK_NEW_COMBO (RenderOptionsRow4_hbox, ColorMapScheme, 150, 60);
           GTK_LOAD_COMBO (ColorMapScheme, "dark");
           GTK_LOAD_COMBO (ColorMapScheme, "light");
           GTK_LOAD_COMBO (ColorMapScheme, "BW");
           GTK_LOAD_COMBO (ColorMapScheme, "custom");
  GTK_END_TAB(RenderPref);
  } /***  END Text TAB Contents ***/

  { /*-------------------- Start Styles TAB Contents --------------------*/
   GTK_START_TAB (StylesPref);
     HD_SEPARATOR (StylesPrefTab_vbox, Grp1);
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
     HD_SEPARATOR (StylesPrefTab_vbox, Grp2);
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
     HD_SEPARATOR (StylesPrefTab_vbox, Grp3);      /* Ripper Options */
       HSECTION(StylesPrefTab_vbox, StylesRow3);     /* ST Grp 2 Lines and Pins */
         GTK_SWITCH(StylesRow3_hbox, RipperType, 30, FALSE);
         GTK_NEW_COMBO (StylesRow3_hbox, RipperSymbol, 0, 0);
         gtk_widget_set_size_request (RipperSymbolCombo, 180, 31);
         GTK_LOAD_COMBO (RipperSymbol, DEFAULT_BUS_RIPPER_SYMNAME)
         GTK_LOAD_COMBO (RipperSymbol, SECOND_BUS_RIPPER_SYMNAME)
       HSECTION(StylesPrefTab_vbox, StylesRow4);     /* ST Grp 2 Lines and Pins */
         GTK_SWITCH(StylesRow4_hbox, RipperRotation, 19, FALSE);
         GTK_NUMERIC_SPIN (StylesRow4_hbox, RipperSize, 31, 200, 0, 500);
     HD_SEPARATOR (StylesPrefTab_vbox, Grp4);        /* Junction Options */
       GEDA_FRAME (StylesPrefTab_vbox, Junctions, -1, 60, 0.05, 0.2, 10)
         GTK_NUMERIC_SPIN (Junctions_hbox, JunctionSize, 12, DEFAULT_JUNCTION_SIZE, MIN_JUNCTION_SIZE, MAX_JUNCTION_SIZE);
         GEDA_COLOR_BUTTON (Junctions_hbox, JunctionColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, DIALOG_H_SPACING)
         GEDA_COLOR_BUTTON (Junctions_hbox, NetEndpointColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, DIALOG_H_SPACING)
     HD_SEPARATOR (StylesPrefTab_vbox, End);

   GTK_END_TAB(StylesPref);
  } /*** END Styles TAB Contents ***/

  { /*-------------------- Start Text TAB Contents --------------------*/

   GTK_START_TAB (TextPref);
     VSECTION(TextPrefTab_vbox, TextOptionsGrp1); /* TT Grp 1 Text Options */
       HSECTION (TextOptionsGrp1_vbox, TextOptionsRow1)   /* TT Grp 1 Row 1 Text Styles */
         GTK_NUMERIC_SPIN (TextOptionsRow1_hbox, TextSize, 9, DEFAULT_TEXT_SIZE, MIN_TEXT_SIZE, MAX_TEXT_SIZE);
         GEDA_NEW_TEXT_ENTRY_COMBO (TextOptionsRow1_hbox, FontName, 325, DIALOG_V_SPACING);
       HSECTION (TextOptionsGrp1_vbox, TextOptionsRow2)   /* TT Grp 1 Row 1 Text Styles */
         GTK_NUMERIC_SPIN (TextOptionsRow2_hbox, TextZoomFactor, 9, DEFAULT_TEXT_ZOOM, MIN_TEXT_ZOOM, MAX_TEXT_ZOOM);
       GEDA_FRAME (TextOptionsGrp1_vbox, Markers, -1, 110, 0.3, 0.2, DIALOG_H_SPACING);
         VSECTION (Markers_hbox, MarkerOptions)  /* Grp 1 Row 1 */
           HSECTION ( MarkerOptions_vbox, MarkerOptionsRow3)  /* Grp 1 Row 3 */
             GTK_SWITCH(MarkerOptionsRow3_hbox, TextOriginMarker, DIALOG_V_SPACING, TRUE);
             GEDA_COLOR_BUTTON (MarkerOptionsRow3_hbox, TextMarkerColor, COLOR_BUTTON_HSIZE, COLOR_BUTTON_VSIZE, 58);
           HSECTION (MarkerOptions_vbox, MarkerOptionsRow4)  /* Grp 1 Row 2 */
             GTK_NUMERIC_SPIN (MarkerOptionsRow4_hbox, TextMarkerSize, 2, DEFAULT_TEXT_MARKER_SIZE, MIN_TEXT_MARKER_SIZE, MAX_TEXT_MARKER_SIZE);
             GTK_NUMERIC_SPIN (MarkerOptionsRow4_hbox, TextMarkerThld, 2, DEFAULT_TEXT_MARKER_THLD/10.0, MIN_TEXT_MARKER_THLD/10.0, MAX_TEXT_MARKER_THLD/10.0);
             SetupSpinner(TextMarkerThldSpin, 1, 0.1, 1.0);
     HD_SEPARATOR (TextPrefTab_vbox, Grp2);
     HSECTION (TextPrefTab_vbox, CapsStyleOptions)   /* TT Grp 2 Text Styles */
       GTK_V_BULB_TRIAD(CapsStyleOptions_hbox, CapsStyle, DIALOG_H_SPACING, Lower, Upper, Both, Both);
     HD_SEPARATOR (TextPrefTab_vbox, Grp3);
     HSECTION (TextPrefTab_vbox, TextOptionsGrp3) /* TT Grp 3 Feedback */
       GTK_V_BULB_TRIAD(TextOptionsGrp3_hbox, TextFeedback, DIALOG_H_SPACING, Readable, Always, Default, Readable);
   GTK_END_TAB(TextPref);
  } /***  END Text TAB Contents ***/

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
             GtkWidget *IncreaseAttributeButt;
             GtkWidget *DecreaseAttributeButt;
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
       HYP_SEPARATOR (AttributesPrefTab_vbox, Grp2, 5);
       HSECTION(AttributesPrefTab_vbox,  AttributeOptions); /* Grp 2 but now Row 2 */
         GTK_NUMERIC_SPIN (AttributeOptions_hbox, AttributeOffset, 25, 50, 0, 300);
         GTK_NUMERIC_SPIN (AttributeOptions_hbox, AutoPlacementGrid, 25, 1, 0, 300);
       HYP_SEPARATOR (AttributesPrefTab_vbox, End, 5);
   GTK_END_TAB(AttributesPref);

  } /*** END Attribute TAB Contents ***/

  { /*-------------------- Start Library TAB Contents --------------------*/

   GTK_START_TAB (LibraryPref);
     VSECTION(LibraryPrefTab_vbox, LibraryOptionsRow1)
       GTK_SWITCH(LibraryOptionsRow1_vbox, EnforceHierarchy, DIALOG_V_SPACING, FALSE);
       GTK_SWITCH(LibraryOptionsRow1_vbox, EmbedComponents, DIALOG_V_SPACING, FALSE);
       GTK_SWITCH(LibraryOptionsRow1_vbox, SortLibrary, DIALOG_V_SPACING, FALSE);
     HXYP_SEPARATOR (LibraryPrefTab_vbox, End, 10);
   GTK_END_TAB(LibraryPref);
  } /*** END Library TAB Contents ***/

  dialog_action_area = GTK_DIALOG (ThisDialog)->action_area;
  g_object_set (dialog_action_area, "visible", TRUE, NULL);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area), GTK_BUTTONBOX_END);

  CancelButt = gtk_button_new_from_stock ("gtk-cancel");
  g_object_set (CancelButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), CancelButt, GEDA_RESPONSE_CANCEL);
  gtk_widget_set_can_default(CancelButt, TRUE);
  gtk_widget_set_tooltip_text (CancelButt, _("Close without changing any settings"));

  SaveButt = gtk_button_new_with_mnemonic (_("Save"));
  g_object_set (SaveButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), SaveButt, GEDA_RESPONSE_APPLY);
  gtk_widget_set_can_default(SaveButt, TRUE);
  gtk_widget_set_tooltip_text (SaveButt, _("Close and Write settings to disk"));

  OkayButt = gtk_button_new_from_stock ("gtk-ok");
  g_object_set (OkayButt, "visible", TRUE, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (ThisDialog), OkayButt, GEDA_RESPONSE_OK);
  gtk_widget_set_can_default(OkayButt, TRUE);
  gtk_widget_set_tooltip_text ( OkayButt, _("Change settings and close but do not write settings to storage"));

  /* Store pointers to widgets, for use by get_widget_data(). */
  GEDA_OBJECT_SET_DATA (ThisDialog, ThisDialog, "ThisDialog");
  GEDA_OBJECT_SET_DATA (ThisDialog, MainDialogVBox, "MainDialogVBox");
  GEDA_OBJECT_SET_DATA (ThisDialog, dialog_action_area, "dialog_action_area");

  GEDA_HOOKUP_OBJECT (ThisDialog, notebook,   "notebook");
  GEDA_HOOKUP_OBJECT (ThisDialog, CancelButt, "CancelButt");
  GEDA_HOOKUP_OBJECT (ThisDialog, SaveButt,   "SaveButt");
  GEDA_HOOKUP_OBJECT (ThisDialog, OkayButt,   "OkayButt");

  gtk_widget_grab_default (CancelButt);

  g_signal_connect (notebook, "switch-page",
                    G_CALLBACK (on_notebook_switch_page),
                    NULL);

  return ThisDialog;
}

/** @} endgroup Preferences-Dialog-Creator */

/** \defgroup Preferences-Dialog-Unloader Settings Dialog Unloader
 *  @{
 *  \memberof (Preferences-Dialog)
 *  \defgroup Preferences-Dialog-Unloader-Utilities Settings Dialog Unloader
 *  @{
 *  \memberof (Preferences-Dialog-Unloader)
 */
int x_settings_lookup_cursor(int offset) {
  return DrawingCursorsInt[offset];
}

/** @} endgroup Preferences-Dialog-Unloader-Utilities */

/*! \brief Post Dialog procedure to retrieves values in dialog controls.
 *  \par Function Description
 *   This function retrieves and saves the values from all widgets. The
 *   values are saved to either the memory variables from which they were
 *   loaded or to the rc_options structure. In some cases, the new setting
 *   is compared to the old setting and only updated when changed, this
 *   primarily applies to strings.
 *
 *   \sa configure_dialog_response
 */
void GatherSettings(GschemToplevel *w_current) {

  GedaToplevel  *toplevel = w_current->toplevel;
  EdaConfig     *cfg      = eda_config_get_user_context ();
  const char    *group    = IVAR_CONFIG_GROUP;

  int      tmp_int;
  char    *tmpstr;
  GdkColor color;

  eda_config_set_string (cfg, group, "default-filename", GetEntryText(UntitledNameEntry));

/* The Color Buttons */

  gtk_color_button_get_color(GTK_COLOR_BUTTON(MeshMinorColorButt),
                             &w_current->mesh_grid_minor_color);
  gtk_color_button_get_color(GTK_COLOR_BUTTON(MeshMajorColorButt),
                             &w_current->mesh_grid_major_color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(GripStrokeColorButt), &color);
  eda_renderer_set_grips_stroke_color (CairoRenderer, &color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(GripFillColorButt), &color);
  eda_renderer_set_grips_fill_color (CairoRenderer, &color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(NetEndpointColorButt), &color);
  eda_renderer_set_net_endpoint_color (CairoRenderer, &color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(TextMarkerColorButt), &color);
  eda_renderer_set_text_marker_color (CairoRenderer, &color);

  gtk_color_button_get_color(GTK_COLOR_BUTTON(JunctionColorButt), &color);
  eda_renderer_set_junction_color (CairoRenderer, &color);

/* Combo Boxes (10) */

  w_current->dots_grid_mode = geda_combo_box_text_widget_get_active (DotGridModeCombo);
  tmp_int                   = geda_combo_box_text_widget_get_active (ConsoleWindowTypeCombo);

  if (tmp_int != console_window_type) {
    console_window_type = tmp_int;
    x_console_update_decorated(w_current);
  }

  w_current->undo_type      = geda_combo_box_text_widget_get_active (UndoTypeCombo);

  if (w_current->undo_type == UNDO_NONE) {
    w_current->undo_control = FALSE;
  }

  w_current->middle_button  = GetGedaCombo (MiddleButton);
  w_current->third_button   = GetGedaCombo (ThirdButton);
  tmp_int                   = GetGedaCombo (PointerCursor);

  if (tmp_int != w_current->drawing_pointer) {
    int pointer_id = DrawingCursorsInt[tmp_int];   /* get the cursor id from our table */
    w_current->drawing_pointer = tmp_int;      /* Save the index with table offset factor*/
    i_window_set_cursor(w_current, pointer_id);
  }

  tmp_int = gtk_combo_box_get_active (GTK_COMBO_BOX (ColorMapSchemeCombo));
  if (tmp_int != rc_options.color_scheme_index) { /* if user changed this settings */
    rc_options.color_scheme_index = tmp_int;
    switch ( tmp_int ) {
    case 0:
      geda_color_load_display_scheme(DARK_DISPLAY_MAP);  /* call for load the Dark map */
      break;
    case 1:
      geda_color_load_display_scheme(LIGHT_DISPLAY_MAP); /* call for load the Light map */
      break;
    case 2:
      geda_color_load_display_scheme(BW_DISPLAY_MAP);    /* call for load the Blk/Wht map */
      break;
    case 3:
      geda_color_load_display_scheme(CUSTOM_DISPLAY_MAP);/* call to load the custom map */
      break;
    default:
      geda_color_load_display_scheme(rc_options.color_map_scheme);  /* call for load custom map */
    }
  } /* else do nothing because the map did not change */

  tmpstr = geda_combo_widget_get_active_text (TitleBlockCombo);

  eda_config_set_string (cfg, group, "default-titleblock", tmpstr);

  g_free(tmpstr);

  tmp_int = gtk_combo_box_get_active (GTK_COMBO_BOX (RipperSymbolCombo));
  if (tmp_int != rc_options.ripper_symbol_index) {
    GEDA_FREE(w_current->bus_ripper_symname);
    w_current->bus_ripper_symname =
    geda_utility_string_strdup(gtk_combo_box_get_active_text(GTK_COMBO_BOX (RipperSymbolCombo)));
    strcpy(rc_options.ripper_symbol_fname, w_current->bus_ripper_symname); /* save the filename */
  }

  w_current->render_adaptor   = gtk_combo_box_get_active (GTK_COMBO_BOX (RendererCombo));
  w_current->anti_aliasing    = gtk_combo_box_get_active (GTK_COMBO_BOX (AntiAliasCombo));

  tmpstr = geda_combo_widget_get_active_text (FontNameCombo);

  if (tmpstr && tmpstr[0]) {
    eda_config_set_string (cfg, group, "default-font-name", tmpstr);

    if (w_current->render_adaptor == CAIRO_ADAPTOR) {
      eda_renderer_set_font_name(CairoRenderer, tmpstr);
    }

#ifdef WITH_LIBGEDADRAW

    else {
      x_draw_set_font (tmpstr, GET_SPIN_IVALUE (TextSizeSpin));
    }

#endif

    g_free(tmpstr);
  }

/* The Switches Alphabetically (31) */
             auto_load_last             = GET_SWITCH_STATE (AutoLoadLastSwitch);
  w_current->bus_ripper_rotation        = GET_SWITCH_STATE (RipperRotationSwitch);
  w_current->bus_ripper_type            = GET_SWITCH_STATE (RipperTypeSwitch);
  w_current->continue_component_place   = GET_SWITCH_STATE (ContinuePlaceSwitch);
  w_current->drag_can_move              = GET_SWITCH_STATE (DragMoveSwitch);

  CairoRenderer->draw_grips             = GET_SWITCH_STATE (DrawGripsSwitch);
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

  if (tmp_int != w_current->scrollbars_visible ) {
    w_current->scrollbars_visible       =  tmp_int;
    x_window_set_scroll_visibility(w_current);
  }

  w_current->scrollbar_update           = GET_SWITCH_STATE (DelayScrollingSwitch);
  w_current->scroll_wheel               = GET_SWITCH_STATE (ClassicWheelSwitch);
  w_current->sort_component_library     = GET_SWITCH_STATE (SortLibrarySwitch);
  w_current->pointer_hscroll            = GET_SWITCH_STATE (PointerHScrollSwitch);
  CairoRenderer->text_origin_marker     = GET_SWITCH_STATE (TextOriginMarkerSwitch);
  w_current->undo_control               = GET_SWITCH_STATE (EnableUndoSwitch);
  w_current->undo_panzoom               = GET_SWITCH_STATE (UndoViewsSwitch);
  w_current->warp_cursor                = GET_SWITCH_STATE (WarpCursorSwitch);
  w_current->zoom_with_pan              = GET_SWITCH_STATE (ZoomPanSwitch);

/* The Spin Controls Alphabetically (23) */
  w_current->add_attribute_offset       = GET_SPIN_IVALUE (AttributeOffsetSpin);
  w_current->attribute_placement_grid   = GET_SPIN_IVALUE (AutoPlacementGridSpin);
  x_settings_set_scm_int("autoplace-attributes-grid", w_current->attribute_placement_grid);

  /* auto save interval */ {

    /* Save the current auto_save_interval */
    int old_auto_save = geda_toplevel_get_auto_save_interval(toplevel);
    int new_auto_save;
                                tmp_int = GET_SWITCH_STATE (AutoSaveSwitch);
                          new_auto_save = tmp_int == 0 ? 0 : GET_SPIN_IVALUE (AutoSaveIntervalSpin);

    geda_toplevel_set_auto_save_interval(toplevel, new_auto_save);

     /* Check if Auto save was enabled, i.e. from 0 -> >0 */
    if (!old_auto_save && new_auto_save) {
      geda_struct_page_autosave_init(toplevel);
    }
  }

  w_current->bus_ripper_size            = GET_SPIN_IVALUE (RipperSizeSpin);
  w_current->dots_grid_threshold        = GET_SPIN_IVALUE (DotGridThresholdSpin);
  w_current->grip_size                  = GET_SPIN_IVALUE (GripPixelSizeSpin);
  CairoRenderer->junction_size          = GET_SPIN_IVALUE (JunctionSizeSpin);
  w_current->keyboardpan_gain           = GET_SPIN_IVALUE (KeyboardPanGainSpin);
  w_current->mesh_grid_threshold        = GET_SPIN_IVALUE (MeshGridThresholdSpin);
  w_current->mesh_line_width_factor     = GET_SPIN_IVALUE (MeshGridWidthSpin);
  w_current->mousepan_gain              = GET_SPIN_IVALUE (MousePanGainSpin);
  w_current->scrollpan_steps            = GET_SPIN_IVALUE (ScrollPanStepsSpin);
  w_current->select_slack_pixels        = GET_SPIN_IVALUE (SelectPixelsSpin);
  w_current->snap_size                  = GET_SPIN_IVALUE (SnapSizeSpin);
  CairoRenderer->text_marker_size       = GET_SPIN_IVALUE (TextMarkerSizeSpin);

  CairoRenderer->text_marker_threshold  = GET_SPIN_DVALUE (TextMarkerThldSpin);
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
/** @} endgroup Preferences-Dialog-Unloader */
/** @} endgroup X_Settings_Dialog_Unload_Variables */
/** @} endgroup Preferences-Dialog */
