/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_settings_dialog.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2020 Wiley Edward Hill <wileyhill@gmail.com>
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: Aug, 22, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
*/
/*!
 * \file x_settings_dialog.h
 *
 * \brief Header file for construction of the Setting Dialog
 */
/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 09/17/12 |  Inital release.
 * ------------------------------------------------------------------
 * WEH | 12/04/12 |  Added switch for EnableColorImaging
 * ------------------------------------------------------------------
 * WEH | 12/30/12 |  Changed "Log" to "Console"
 * ------------------------------------------------------------------
 * WEH | 01/06/13 |  Added strings for new ripper controls, (new)
 * ------------------------------------------------------------------
*/

#ifndef __X_SETTINGS_DIALOG_H__
#define __X_SETTINGS_DIALOG_H__

/*************************** CAUTION! ******************************/
/*
 * String data for Labels and ToolTips in the string arrays are
 * referenced using enumerated Control ID's. To add or remove a
 * control:
 *
 *    1.) Be sure to add an enumerator in the ControlID array in the
 *        file x_settings.h
 *
 *    2.) Add the string text to the SAME position in DialogStrings
 *        below.
 *
 *    3.) Reference the string with the enumerators using the
 *        macros in geda_dialog_controls.h
 */

/* These Macro were used during development so "dummy" data,
 * in the AttributeList arrays, could be substituted for the
 * real data. Turned out to be "handy" and View2Data seems
 * less obsure then w_current->component_select_attrlist.
 */
#define View1Data NULL
#define View2Data w_current->component_select_attrlist

#define View1DefaultData DefaultAttributeList
#define View2DefaultData DefaultFilterList

#define TitleBlockData DefaultTitleBlockList

static WidgetStringData DialogTabData[] = {
        { "GeneralTab",        N_("General"),      N_("General program options")},
        { "EditTab",           N_("Edit"),         N_("Editing preferences")},
        { "PointerTab",        N_("Pointer"),      N_("Mouse related configuration options")},
        { "WindowTab",         N_("Window"),       N_("Window sizes and grid options")},
        { "RenderTab",         N_("Render"),       N_("Renderer options")},
        { "StylesTab",         N_("Styles"),       N_("Styles")},
        { "TextTab",           N_("Text"),         N_("Text options")},
        { "AttributesTab",     N_("Attributes"),   N_("Attributes")},
        { "LibraryTab",        N_("Library"),      N_("Library related preferences")},
};

WidgetStringData DialogStrings[] = {

  /* 5 String for Section Labels */
        { "ConsoleLabel",         "Console Log",  "Console and Logging Options"},
        { "UndoLabel",            "   Undo",      "Undo Options"},
        { "NetsLabel",            "  Net",        "Net Options"},

  /* Button Controls */
    /* Edit TAB */
        { "Select Grip Color",         "Grip Color:",     "Set the color to be used when drawing grips"},
        { "Select Grip Fill Color",    "Grips Fill:",     "Set the background color for grips"},
        { "Select Net Endpoint Color", "Endpoint Color:", "Set the color on net endpoints"},

    /* Attributes TAB */
        { "AddAttributeButt",     "Add",          "Add selected attribute to the filter list on the Select Components Dialog"},
        { "RemoveAttributeButt",  "Remove",       "Remove the selected attribute from the filter list"},
        { "ClearAtrributesButt",  "Clear",        "Double click to clear all attributes from the filter list"},
        { "DefaultAttributeButt", "Default",      "Restore default filter list of attributes to display"},
        { "IncreaseAttributeButt","Up",           "Increase selected attribute in the Add Attribute list"},
        { "DecreaseAttributeButt","Down",         "Decrease selected attribute in the Add Attribute list"},

    /* Window TAB */
        { "Select Minor Mesh Color",  " Minor Mesh Color:",   "Set the mesh minor grid color"},
        { "Select Major Mesh Color",  " Major Mesh Color:",   "Set the mesh major grid color"},

    /* Styles TAB */
        { "Select Junction Color",  "Cue Color:",    "Set the color for junction cues"},

    /* Text TAB */
        { "Select Text Marker Color", "Color:",         "Set the color for text markers"},

  /* String for Combo Controls  */
        { "TitleBlockCombo",        "Titleblock:",      "Name of Default Titleblock"},
        { "DotGridModeCombo",       "  Dot Grid Mode:", "With variable mode, the dotted grid spacing changes depending on the zoom factor. In the fixed mode, the grid always represents the same number of units as the snap-spacing. The density of the dotted grid can be controlled using the dots-grid-threshold."},
        { "ConsoleWindowTypeCombo", "  Window:",        "Controls if the console window is a transient or decorated as a normal window. The window manager is responsible for doing the decoration."},
        { "PointerCursorCombo",     "    Cursor:",      "Select the preferred Pointer Cursor Style"},
        { "MiddleButtonCombo",      "Middle Button:",   "Controls if the middle mouse button draws strokes, repeats the last command, does an action (move and copy (pressing the ALT key) are supported) on a single objects, or if it does the mouse panning."},
        { "ThirdButtonCombo",       "Third Button:",    "Controls if the third mouse button displays a popup menu or performs panning"},
        { "UndoTypeCombo",          "Undo Type:",       "Controls which Undo System configuration. The default is to use the disk as the storing medium (i.e. after every action the undo information is stored to disk). The other mechanism uses only memory. The disk mechanism is slower but allows undoing even after a system crash."},
        { "FontNameCombo",          "Font:",            "Select the Default Font"},
        { "RipperSymbolCombo",      "Symbol:",          "Specify the symbol name to be used if ripper type is \"component\"."},
        { "RendererCombo",          "     Renderer:",    "Choose which rendering system to use."},
        { "AntiAliasCombo",         "AntiAlias",        "Choose the Anti Alias level for the rendered."},
        { "ColorMapSchemeCombo",    "  Scheme:",        "Choose which color scheme should be load at startup."},

  /* 1 String for Edit Controls Label */
        { "UntitledNameEntry",	        "Untitled Name:",	"Initial name for new drawings."},

  /* Strings for Radio Controls by Alphabetical TAB order */
    /* General TAB */
        { "LogDestinyLabel",            "Log Output",   "Log Output"},
        { "LogDestinyWindowRadio",      "Window",	"Write log output to the gEDA console Window"},
        { "LogDestinyTTYRadio",	        "TTY",		"Write log output to the tty console"},
        { "LogDestinyBothRadio",	"Both",		"Write log output to both the gEDA and the TTY console"},

    /* Edit TAB */
        { "NetEndPointLabel",           " End Point:",  "Not implemented yet. Set End Point markers for Selected nets."},
        { "NetEndPointNoneRadio",       "None",		"None"},
        { "NetEndPointEmptyRadio",      "Empty",	"Empty box"},
        { "NetEndPointFilledRadio",     "Filled",	"Filled box mode"},

        { "NetMidPointLabel",           " Mid Point:",  "Not implemented yet. Set Mid Point markers for Selected nets."},
        { "NetMidPointNoneRadio",       "None",		"None"},
        { "NetMidPointEmptyRadio",      "Empty",	"Empty box"},
        { "NetMidPointFilledRadio",     "Filled",	"Filled box mode"},

        { "NetSelectionLabel",          "  Selection:", "Controls how many net segments are selected when you click at a net."},
        { "NetSelectionNoneRadio",      "None",		"Controls how many net segments are selected when you click at a net."},
        { "NetSelectionNetRadio",       "Net  ",	"The mode defines the maximum search depth for the net selection mode."},
        { "NetSelectionAllRadio",       "All",		"Everything connect to the selection."},

    /* Styles TAB */
        { "BusStyleLabel",	  "  Bus",		"Bus Style"},
        { "BusStyleDefaultRadio", "None",		"Setting Bus style to None disables the Bus width style."},
        { "BusStyleThinRadio",	  "Thin",		"Use Thin Bus style"},
        { "BusStyleThickRadio",	  "Thick",		"Use Thick Bus style"},

        { "LineStyleLabel",	  "Lines",		"Line Style"},
        { "LineStyleDefaultRadio","None",		"Setting Line style to None disables the Line width style."},
        { "LineStyleThinRadio",	  "Thin",		"Use Thin Line style"},
        { "LineStyleThickRadio",  "Thick",		"Use Thick Line style"},

        { "NetStyleLabel",	  " Net",		"Net Style"},
        { "NetStyleDefaultRadio", "None",		"Setting Net style to None disables the Net width style."},
        { "NetStyleThinRadio",	  "Thin",		"Use Thin Net style"},
        { "NetStyleThickRadio",	  "Thick",		"Use Thick Net style"},

        { "PinStyleLabel",	  "Pins",		"Pin Style"},
        { "PinStyleDefaultRadio", "None",		"Setting Pin style to None disables the Pin width style."},
        { "PinStyleThinRadio",	  "Thin",		"Use Thin Pin style"},
        { "PinStyleThickRadio",	  "Thick",		"Use Thick Pin style"},

    /* Text TAB */
        { "CapsStyleLabel",	  "  Caps Style",	"Sets the default caps style used for the input of text."},
        { "CapsStyleLowerRadio",  "Lower",		"Specifies that all inputed text is in lowercase."},
        { "CapsStyleUpperRadio",  "Upper",		"Specifies that all inputed text is in uppercase."},
        { "CapsStyleBothRadio",	  "Both",		"specifies that all inputed text is used as is with no case conversion."},

        { "TextFeedbackLabel",	       "   Feedback:",		"Controls if text is drawn when doing an xor action (like copy/move)."},
        { "TextFeedbackReadableRadio", "Only-when-readable",	"Only-when-readable"},
        { "TextFeedbackAlwaysRadio",   "Always",		"Always provide feedback"},
        { "TextFeedbackDefaultRadio",  "Default",		"Default feedback mode"},

    /* Windows TAB */
        { "GridDotSizeRadio",           "Grid Dot Size:",	"dots-grid-dot-size controls the size of the grid dots in the dots grid display. The units are in pixels."},
        { "GridDotSizeOneRadio",        "1",			"The default (min) value of 1 is the best performing as the grid dot size is rendered as a single pixel."},
        { "GridDotSizeTwoRadio",        "2",			"Values of 2 and 3 are good values to try if the default grid dot size is too small for your tastes."},
        { "GridDotSizeThree",           "3",			"Anything larger than 3 is probably too large."},

        { "GridMode",                   "      Grid Mode:",     "Controls the grid display mode"},
        { "GridModeNoneRadio",          "None",                 "Startup with the grid turned off"},
        { "GridModeDotsRadio",          "Dots",                 "Startup with Dots grid mode"},
        { "GridModeMeshRadio",          "Mesh",                 "Startup with Mesh grid mode"},

        { "WindowSizeRadio",            "Window Size:",	"Specifies the size of the drawing area window. The width and height are specified in pixels and do not include the menu bars and scrollbars so the window will be larger than the specified values."},
        { "WindowSizeH650W487Radio",    "650 x 487",		"Good size for 800x600"},
        { "WindowSizeH900W650Radio",    "900 x 650",		"Good size for 1024x768"},
        { "WindowSizeH950W712Radio",    "950 x 712",		"Good size for 1152x864"},
        { "WindowSizeH1100W825Radio",   "1100 x 825",		"Good size for 1280x1024"},
        { "WorldSizeRadio",             "   World Size:",	    "Specifies the size of the world and a border in world space units this is not the paper size."},
        { "WorldSizeSmallRadio",        "60.0 x 45.0",	 "A small World"},
        { "WorldSizeMediumRadio",       "120.0 x 90.0",	 "Default normal World size"},
        { "WorldSizeLargeRadio",        "180.0 x 135.0",  "Large World size"},

    /* Attributes TAB */
        { "DialogListAttributesRadio",     "Dialog Attributes:", "Options for the attributes list in the Select Component dialog."},
        { "DialogListAttributesAllRadio",  "All",		"Show all attributes in the Select Component dialog."},
        { "DialogListAttributesNoneRadio", "None",		"Do not show attributes in the Select Component dialog."},
        { "DialogListAttributesListRadio", "List",		"Show listed attributes as sorted as they appear in the list below."},

  /* Spinner Controls in Alphabetical order */
        { "AttributeOffsetSpin",        "Offset Factor:",	"Controls a offset which is added to the location of text items that are added to an object as an attribute. This offset is added when certain conditions occur."},
        { "AutoPlacementGridSpin",      "Autoplacement Grid:",   "When placing components, snaps attributes to the nearest point on the grid within the specified value"},
        { "AutoSaveIntervalSpin",       "   Save Interval:",	"The unit for interval is seconds."},
        { "DotGridThresholdSpin",       "Fixed Threshold:",	"The dots-grid-threshold specifies the minimum number of pixels grid-spacing for the grid to be displayed. Using this parameter you can control the density of the displayed grid (smaller numbers will cause the grid to be drawn denser). This mode is only used when grid-mode is fixed."},
        { "GripPixelSizeSpin",          "Grip Size:",	        "Sets the size of the grips."},
        { "JunctionSizeSpin",           "Node Size:",            "Controls the junction \"cues\" size of drawing nodes"},
        { "KeyboardPanGainSpin",        " Keyboard Gain:",	"Controls how much the display pans when using the keyboard cursor keys. A larger value provides greater pan distance when pressing the cursor keys while a smaller value provides a smoother but smaller pan distance when moving the cursor keys."},
        { "MeshGridThresholdSpin",      "   Mesh Threshold:",    "The mesh-grid-display-threshold specifies the minimum line pitch for the grid to be displayed. Using this parameter you can control maximum density of the displayed grid before the minor then major grid-lines are switched off."},
        { "MeshGridWidthSpin    ",      "  Mesh Line Width:",    "The line width used for the mesh grid display."},
        { "MousePanGainSpin",           "     Mouse Pan Gain:",	"Controls how much the display pans when using mousepan. A larger value provides greater pan distance when moving the mouse while a smaller value provides a smoother but smaller pan distance when moving the mouse."},
        { "RipperSizeSpin",             "   Ripper Size:",       "Sets the size of the auto bus rippers."},
        { "ScrollPanStepsSpin",         "    Scroll Pan Steps:", "Controls the number of scroll pan events required to traverse the viewed schematic area. Larger numbers mean more scroll steps are required to pan across the viewed area and giving finer control over positioning."},
        { "SelectPixelsSpin",           "Select Pixels:",       "Controls how many pixels around an object can still be clicked as part of that object. A larger value gives greater ease in selecting small or narrow objects."},
        { "SnapSizeSpin",               "     Snap Size:",      "Sets the default spacing which objects snaps to."},
        { "TextMarkerSizeSpin",         "  Size:",              "Sets the default size of text origin markers."},
        { "TextMarkerThldSpin",         "Threshold:",           "Lateral threshold distance between text and markers at which point the markers drawn"},
        { "TextSizeSpin",               "     Text Size:",      "Sets the default text font size."},
        { "TextZoomFactorSpin",         "   Zoom Factor:",      "Sets the zoomfactor number (~150 being the most zoomed out factor)(zoom factors get smaller as you zoom in) at which text is displayed completely (not a line). This is only valid if Feedback is set to \"only-when-readable\""},
        { "ThickBusWidthSpin",          "Thick",                "Set the width, in mils, of the Thick Buss."},
        { "ThickLineWidthSpin",         "Thick",                "Set the width, in mils, of the Thick Lines."},
        { "ThickNetWidthSpin",          "Thick",                "Set the width, in mils, of the Thick Nets."},
        { "ThickPinWidthSpin",          "Thick",                "Set the width, in mils, of the Thick Pins."},
        { "ThinBusWidthSpin",           " Thin",                "Set the width, in mils, of the Thin Buss."},
        { "ThinLineWidthSpin",          " Thin",                "Set the width, in mils, of the Thin Lines."},
        { "ThinNetWidthSpin",           " Thin",                "Set the width, in mils, of the Think Nets."},
        { "ThinPinWidthSpin",           " Thin",                "Set the width, in mils, of the Thin Pins."},
        { "UndoBufferSizeSpin",         "     Buffer Size:",    "Determines the number of levels of undo. Basically this number decides how many backup schematics are saved on disk."},
        { "ZoomGainSpin",               "      Zoom Gain:",     "Controls the percentage size increase when zooming into the page. Un-zooming uses the inverse factor such that a zoom in / zoom out pair will return the schematic to the same size."},

    /* Strings for Switch Controls */
        { "AutoLoadLastSwitch",         "    Auto Last:",	"Enable or disable automatic loading of the last edited schematic."},
        { "AutoPanSwitch",              "Auto Pan:",      "Controls if ."},
        { "AutoSaveSwitch",             "Auto Save:",		"Controls if a backup copy is made every interval. Note that the backup copy is made when you make some change to the schematic and there were more than \"interval\" seconds from the last auto-save."},
        { "ClassicWheelSwitch",         "Classic Wheel:",	"Controls binding of the scroll wheel. With no modifier keys, the gschem default \"Classic\" style  maps scrolling to zoom, + CTRL -> x-axis pan, + SHIFT -> y-axis pan. GTK style changes the behavior to be more like other GTK applications, no modifier -> y-axis pan, + CTRL -> zoom, + SHIFT -> x-axis pan"},
        { "ConsolidateNets",            "Consolidate:",	        "Controls if the net consolidation code is used when schematics are read in written to disk and when nets are being drawn (does not consolidate when things are being copied or moved yet). Net consolidation is the connection of nets which can be combined into one. Note: scrollbars must be enable for this to work properly."},
        { "ContinuePlaceSwitch",        "Continue Place:",	"If this enabled then multiple instances of the same component can be placed immediately without having to click on the name or Apply in the Component Place dialog box. If this is disabled then only one component can be placed (the user must then press Apply in the dialog box to place multiple instances of the same component)."},
        { "DelayScrollingSwitch",       "Delay:",               "Specifies the behavior of the scrollbars in the main window. When disabled the display is redrawn as you move the scrollbars. When enabled the display is redrawn once you stop moving the scrollbars"},
        { "DragMoveSwitch",             "      Drag Move:",     "If enabled the drag movement over selected objects can move the objects."},
        { "DrawGripsSwitch",            "Draw Grips:",	        "Controls if the editing grips are drawn when selecting objects."},
        { "EmbedComponentsSwitch",      " Embed Components:",	"Determines if the newly placed components are embedded in the schematic or if only the filename is specified and the component is searched for instead. If it is enabled then all new components will be embedded otherwise they are not embedded. This can be controlled on the fly during run-time with the \"Embed Component\" check-box on the select component dialog box."},
        { "EnableColorImaging",         "Color Imaging:",       "Controls if png images are color (enabled) or black/white (disabled). If enabled, images will be generated bases on the active \"Display\" color map, which could be B&W."},
        { "EnableLogSwitch",            "  Enable:",		"Determines if the logging mechanism is enabled or disabled."},
        { "EnableUndoSwitch",           " Enable:",		"Enabled or disable Undo function."},
        { "EnforceHierarchySwitch",     "Enforce Hierarchy:",	"Controls if the movement between hierarchy levels (of the same underlying schematics) is allowed or not.If this is enabled then the user cannot (without using the page manager) move between hierarchy levels otherwise if enabled the user sees all the hierarchy levels as being flat."},
        { "FastMousePanSwitch",         "     Fast Mouse:",     "Controls if text is drawn properly or if a simplified version (a line which represents the text string) is drawn during mouse pan. Drawing a simple line speeds up mouse panning a lot with big schematics."},
        { "FeedbackModeSwitch",         "Feedback Mode:",	"Disable sets Action Feedback to outline to get an outline of the selection. Enable to get a bounding box of the selection. For slow computer or video sub-system, use boundingbox to  improve performance during editing operations."},
        { "ForceBoundingBoxSwitch",     "Bounding Box:",	"Controls if the entire bounding box of a symbol is used when figuring out which end of the pin is considered the active port. Enable this when gschem is guessing incorrectly."},
        { "FilePreviewSwitch",          "File Preview:",	"Controls if the preview area in the File Open/Save As and Component dialog boxes is enabled by default or not."},
        { "FriendlyColorMapSwitch",     "   Color Map:",	"Make the gschem color maps more user-friendly"},
        { "FriendlyOutlineMapSwitch",   "  Outline Map:",       "Make the gschem Outline color maps more user-friendly"},
        { "InitLogWindowSwitch",        "Start Up:",		"Controls if the log message window is mapped when gschem is started up."},
        { "InvertImagesSwitch",         "Invert Images:",       "Controls if export black & white images are reversed to black on white (enabled) or not, black on white (disabled)"},
        { "MagneticNetsSwitch",         "   Magnetic:",		"Controls the initial setting of the magnetic net mode. The magnetic net mode marks a possible connection that is close to the current cursor position."},
        { "NetDirectionSwitch",         "  Direction:",		"Controls if the net direction mode is used. This mode tries to guess the best continuation direction of a L-shape net when adding a net."},
        { "NotifyEventsSwitch",         "Notify Events:",	"Controls if dialog boxes are raised whenever an expose event happens."},
        { "ObjectClippingSwitch",       "Object Clipping:",	"Determines if the object clipping code is executed or not."},
        { "PointerHScroll",             "Horizontal Scroll",    "This option enables or disables pointer horizontal scroll events so that integrated middle button can be utilized without interference."},
        { "RipperRotationSwitch",       "Ripper Rotation:",     "Determines how the bus ripper symbol is rotated when it is auto added to a schematic, either \"symmetric\" (On) or \"non-symmetric\" (Off)."},
        { "RipperTypeSwitch",           "   Ripper Type:",      "Sets the bus ripper type, either a plain \"net\" (Off) or a \"component\" (On)"},
        { "RubberNetsSwitch",           " Rubberband:",	        "Controls if net connections are maintained when you move a connecting component or net."},
        { "ScrollBarsSwitch",           "Enable:",	        "Controls if the scrolling is enabled or disabled. If you disable scrolling, both the scrollbars and the mouse scroll wheel will be disable."},
        { "ScrollBarsVisibleSwitch",    "Bars:",                "Controls if horizontal and vertical scrollbars are visible."},
        { "SortLibrarySwitch",          "     Sort Library:",	"If this is enabled then the component library will be sorted in alphanumeric order. This option is cosmetic and will not alter the component search order (latest added gets scanned first)."},
        { "TextOriginMarkerSwitch",     "Origin:",	        "Controls if the text origin markers are displayed or not."},
        { "UndoViewsSwitch",            "  Views:",		    "Controls if pan or zoom commands are saved in the undo list. If this is enabled then a pan or zoom command will be considered a command and can be undone. If this is false then panning and zooming is not saved in the undo list and cannot be undone. Note the current view port information is saved for every command so the display will change to the view port before a command is executed."},
        { "WarpCursorSwitch",           "   Warp Cursor:",	"Controls if the cursor is warped (or moved) when you zoom in and out."},
        { "ZoomPanSwitch",              "       Zoom Pan:",     "Sets the zoom in and zoom out functions to pan the display and then zoom. Basically zoom in / out where the mouse pointer sits on the display."},
    /* View Controls */
        { "PotentialAttributesView",    "Potential Attributes", "Add these to the list on the right"},
        { "SelectedAttributesView",     "Selected Attributes", "Display these on the Add Components Dialog"},
        { NULL, NULL, NULL},
};

const char *DefaultAttributeList[] = {
"footprint", "value", "refdes", "source", "model-name", "model", "net",
"device", "pinnumber", "pinseq", "pintype", "pinlabel", "numslots","slot",
"slotdef", "graphical","description","documentation","symversion", "comment",
"author", "dist-license","use-license","file", NULL
};

const char *DefaultFilterList[] = {
"device", "description", "footprint", "comment", "net", "model", "model-name",
"file", "value", "numslots", "slotdef", "slot", "documentation", "symversion",
"author", "use-license", "dist-license", NULL};

const char *DefaultTitleBlockList[] = {
"title-B.sym", "title-A4.sym", "cvstitle.sym", "cvstitleblock-1.sym",
"title-A0.sym", "title-A1.sym", "title-A2.sym", "title-A3.sym", "title-A4.sym",
"title-bordered-A.sym", "title-bordered-A0.sym", "title-bordered-A1.sym",
"title-bordered-A2.sym", "title-bordered-A3.sym", "title-bordered-A4.sym",
"title-bordered-B.sym", "title-bordered-C.sym", "title-bordered-D.sym",
"title-bordered-E.sym",
"title-C.sym", "title-D.sym", "title-E.sym",
NULL};

int DrawingCursorsInt[] =
{
  -2,
  GDK_CURSOR_IS_PIXMAP,
  GDK_X_CURSOR,
  GDK_ARROW,
  GDK_CENTER_PTR,
  GDK_CROSS,
  GDK_CROSSHAIR,
  GDK_DIAMOND_CROSS,
  GDK_DOT,
  GDK_DOTBOX,
  GDK_DRAFT_LARGE,
  GDK_DRAFT_SMALL,
  GDK_LEFT_PTR,
  GDK_RIGHT_PTR,
  GDK_TARGET,
  GDK_TCROSS,
  GDK_TOP_LEFT_ARROW,
};

const char *CursorStrings[] =
{
  N_("System"),
  N_("Custom"),
  N_("X cursor"),
  N_("Arrow"),
  N_("Center Pointer"),
  N_("Cross"),
  N_("Crosshair"),
  N_("Diamond Cross"),
  N_("Dot"),
  N_("Dot Box"),
  N_("Draft Large"),
  N_("Draft Small"),
  N_("Left Pointer"),
  N_("Righ Pointer"),
  N_("Target"),
  N_("T Cross"),
  N_("Top Left Arrow"),
  NULL
};
#endif /* __X_SETTINGS_DIALOG_H__ */
