/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_settings.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Date: Aug, 17, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */
/*!
 * \file x_settings.h
 *
 * \brief  header used by the Setting module
 */
/************************ REVISION HISTORY *************************
 * Who |   When   | What (Why)
 * ------------------------------------------------------------------
 * WEH | 09/17/12 | Inital release.
 * ------------------------------------------------------------------
 * WEH | 12/04/12 | Added function x_settings_set_scm_int, revised
 * ------------------------------------------------------------------
 * WEH | 01/06/13 | Added RC_RIPPER_xxx_STRINGS (new). Revise  gschem_
 *                | structure for Ripper name string.
 * ------------------------------------------------------------------
 * WEH | 09/20/13 | Added PointerCursor Combo (to extend functionality)
 * ------------------------------------------------------------------
 * WEH | 09/25/13 | Added GripStrokeColor, GripFillColor,TextMarkerColor,
 *                | TextOriginMarker, TextMarkerSize, JunctionColor,
 *                | TextMarkerSize and JunctionSize, NetEndpointColor,
 *                | ScrollBarsVisible
 * ------------------------------------------------------------------
 * WEH | 03/13/15 | Added TextMarkerThld Spinner (to support new settings
 *     |          | variable)
 * ------------------------------------------------------------------
*/
#ifndef __X_SETTINGS_H__
#define __X_SETTINGS_H__

#define ThisDialog SettingsDialog
#define DialogTitle "Configure gschem"
#define DialogSettings IDS_CONFIG_SETTINGS

#define ControlID EnumeratedSettingsControl
#define Combo_Responder combo_responder
#define Butt_Responder butt_responder
#define Color_Butt_Responder color_butt_responder
#define Radio_Responder radio_responder
#define Switch_Responder switch_responder

struct rc_7_strings_t
{
   const char *zero;
   const char *one;
   const char *two;
   const char *three;
   const char *four;
   const char *five;
   const char *six;
   const char *seven;
};

/* The following defines are for saving settings to an RC file, this is
 * not required for key-file variables. If following instructions to add
 * a widget to the preference dialog, then skip over these defines to the
 * enumerator ControlID
 */
#define RC_BOOL_STRINGS(WhichOne) ((WhichOne) ? RC_STR_ENABLED : RC_STR_DISABLED)
#define RC_RENDER_ADAPTOR_STRINGS struct rc_7_strings_t string_table = {RC_RENDERER_OPTION_CAIRO, RC_RENDERER_OPTION_X11, NULL, NULL};
#define RC_ANTI_ALIASING_STRINGS struct rc_7_strings_t string_table = {RC_STR_ANTIALIAS_DEFAULT, RC_STR_ANTIALIAS_NONE, \
                                                                       RC_STR_ANTIALIAS_GRAY, RC_STR_ANTIALIAS_SUBPIXEL, \
                                                                       RC_STR_ANTIALIAS_FAST, RC_STR_ANTIALIAS_GOOD, \
                                                                       RC_STR_ANTIALIAS_BEST, NULL};
#define RC_GRID_MODE_STRINGS struct rc_7_strings_t string_table = {RC_STR_NONE, RC_STR_DOTS, RC_STR_MESH, NULL};
#define RC_DOTS_GRID_MODE_STRINGS struct rc_7_strings_t string_table = {RC_STR_DOTS_MODE_VARIABLE, RC_STR_DOTS_MODE_FIXED, NULL, NULL};
#define RC_BARS_UPDATE_STRINGS struct rc_7_strings_t string_table = {RC_STR_BARS_CONTINUOUS, RC_STR_BARS_DELAYED, NULL, NULL};
#define RC_LOG_DESTINY_STRINGS struct rc_7_strings_t string_table = {RC_STR_DESTINY_WINDOW, RC_STR_DESTINY_TTY, RC_STR_DESTINY_BOTH, NULL};
#define RC_CONSOLE_WINTYPE_STRINGS struct rc_7_strings_t string_table = {RC_STR_CONWIN_DECORATED, RC_STR_CONWIN_TRANSIENT, NULL, NULL};
#define RC_ACTION_FEEDBACK_STRINGS struct rc_7_strings_t string_table = {RC_STR_FEEDBACK_OUTLINE, RC_STR_FEEDBACK_BOUNDBOX, NULL, NULL};

#define RC_WINDOW_SIZE_STRINGS struct rc_7_strings_t string_table = {RC_STR_WINDOW_W650H487, RC_STR_WINDOW_W900H650, \
                                                                     RC_STR_WINDOW_W950H712, RC_STR_WINDOW_W1100H825};

#define RC_WORLD_SIZE_STRINGS struct rc_7_strings_t string_table =  {RC_STR_WORLD_SMALL, RC_STR_WORLD_MEDIUM, RC_STR_WORLD_LARGE, NULL};

#define RC_NET_MAKER_STRINGS struct rc_7_strings_t string_table = {RC_STR_NET_NONE, RC_STR_EMPTY_BOX, RC_STR_FILLED_BOX, NULL};
#define RC_NET_SELECTION_STRINGS struct rc_7_strings_t string_table = {RC_STR_DISABLED, RC_STR_NET_NET, RC_STR_NET_ALL, NULL};
#define RC_STYLES_STRINGS struct rc_7_strings_t string_table = {RC_STR_STYLE_NONE, RC_STR_STYLE_THIN, RC_STR_STYLE_THICK, NULL};

#define RC_MIDDLE_MOUSE_STRINGS struct rc_7_strings_t string_table = {RC_STR_MID_STROKE, RC_STR_MID_REPEAT, \
                                                                   RC_STR_MID_ACTION, RC_STR_MID_MOUSEPAN, RC_STR_MID_MOUSEPOP};
#define RC_3RD_BUTT_STRINGS struct rc_7_strings_t string_table = {RC_STR_3RD_POPUP, RC_STR_3RD_PAN, NULL, NULL};
#define RC_SCROLL_STRINGS struct rc_7_strings_t string_table = {RC_STR_SCROLL_GTK, RC_STR_SCROLL_CLASSIC, NULL, NULL};

#define RC_TEXT_CASE_STRINGS struct rc_7_strings_t string_table = {RC_STR_TEXT_LOWER, RC_STR_TEXT_UPPER, RC_STR_TEXT_BOTH };
#define RC_TXT_FEEDBACK_STRINGS struct rc_7_strings_t string_table = {RC_STR_TXT_READABLE, RC_STR_TXT_ALWAYS, NULL, NULL};
#define RC_UNDO_TYPE_STRINGS struct rc_7_strings_t string_table = {RC_STR_UNDO_DISK, RC_STR_UNDO_MEMORY, NULL, NULL};

#define RC_RIPPER_ROTATION_STRINGS struct rc_7_strings_t string_table = {RC_STR_RIP_SYMMETRIC, RC_STR_RIP_NON_SYMMETRIC, NULL, NULL};
#define RC_RIPPER_TYPE_STRINGS     struct rc_7_strings_t string_table = {RC_STR_RIP_NET, RC_STR_RIP_COMPONENT, NULL, NULL};

/* This structure is initialized in x_settings_dialog.c, see rc_options */
typedef struct
{
   int display_color_map;
   int color_scheme_index;
   int display_outline_color_map; /* Must determine the status of these options */
   int window_size;
   int custom_window_size;
   int world_size;
   int custom_world_size;
   int titleblock_index;
   int ripper_symbol_index;
   int render_adaptor;
   char color_map_scheme[MAX_FILENAME];
   char untitled_name[MAX_FILENAME];
   char titleblock_fname[MAX_FILENAME];
   char ripper_symbol_fname[MAX_FILENAME];
}  gschem_rc_options;

extern gschem_rc_options rc_options;

typedef enum {
       GeneralPref,
       EditPref,
       PointerPref,
       WindowPref,
       RenderPref,
       StylesPref,
       TextPref,
       AttributesPref,
       LibraryPref,
} DialogTabs;

typedef enum {
/* Section Labels */
        Logging,
        Undo,
        Nets,

/* Button Controls */
    /* Edit TAB */
        GripStrokeColor,
        GripFillColor,
        NetEndpointColor,

    /* Attributes TAB */
        AddAttribute,
        RemoveAttribute,
        ClearAttributes,
        DefaultAttributes,
        IncreaseAttribute,
        DecreaseAttribute,

    /* Window TAB */
        MeshMinorColor,
        MeshMajorColor,

    /* Styles TAB */
        JunctionColor,  /* 13 */

    /* Text TAB */
        TextMarkerColor,

/* 12 Combo Controls  */
        TitleBlock,
        DotGridMode,
        ConsoleWindowType,
        PointerCursor,
        MiddleButton,
        ThirdButton,
        UndoType,
        FontName,
        RipperSymbol,
        Renderer,
        AntiAlias,
        ColorMapScheme,

/* 1 Edit Control Ids  */
        UntitledName,

/* Radio Control by Alphabetical TAB order */
  /* General TAB */
        LogDestiny,
        LogDestinyWindow,
        LogDestinyTTY,
        LogDestinyBoth,

  /* Edit TAB */
        NetEndPoint,
        NetEndPointNone,
        NetEndPointEmpty,
        NetEndPointFilled,

        NetMidPoint,
        NetMidPointNone,
        NetMidPointEmpty,
        NetMidPointFilled,

        NetSelection,
        NetSelectionNone,
        NetSelectionNet,
        NetSelectionAll,

  /* Styles TAB */
        BusStyle,  BusStyleNone,  BusStyleThin,  BusStyleThick,
        LineStyle, LineStyleNone, LineStyleThin, LineStyleThick,
        NetStyle,  NetStyleNone,  NetStyleThin,  NetStyleThick,
        PinStyle,  PinStyleNone,  PinStyleThin,  PinStyleThick,

  /* Text TAB */
        CapsStyle,            CapsStyleLower,
        CapsStyleUpper,       CapsStyleBoth,
        TextFeedback,         TextFeedbackReadable,
        TextFeedbackAlways,  TextFeedbackDefault,

  /* Windows TAB */
        GridDotSize,       GridDotSizeOne,
        GridDotSizeTwo,    GridDotSizeThree,
        GridMode,          GridModeNone, GridModeDots, GridModeMesh,
        WindowSize, WindowSizeW650H487, WindowSizeW900H650,
                    WindowSizeW950H712, WindowSizeW1100H825,
        WorldSize, WorldSizeSmall,  WorldSizeMedium, WorldSizeLarge,

  /* Attributes TAB */
        DialogListAttributes, DialogListAttributesAll,
        DialogListAttributesNone, DialogListAttributesList,

/* Spinner Controls in Alphabetical order (not TAB) */
        AttributeOffset,
        AutoPlacementGrid,
        AutoSaveInterval,
        DotGridThreshold,
        GripPixelSize,
        JunctionSize,
        KeyboardPanGain,
        MeshGridThreshold,
        MeshGridWidth,
        MousePanGain,
        RipperSize,
        ScrollPanSteps,
        SelectPixels,
        SnapSize,
        TextMarkerSize,
        TextMarkerThld,
        TextSize,
        TextZoomFactor,
        ThickBusWidth,
        ThickLineWidth,
        ThickNetWidth,
        ThickPinWidth,
        ThinBusWidth,
        ThinLineWidth,
        ThinNetWidth,
        ThinPinWidth,
        UndoBufferSize,
        ZoomGain,

/* Text for Switch Controls */
        AutoLoadLast,
        AutoSave,
        ClassicWheel,
        ConsolidateNets,
        ContinuePlace,
        DelayScrolling,
        DragMove,
        DrawGrips,
        EmbedComponents,
        EnableColorImaging,
        EnableLog,
        EnableUndo,
        EnforceHierarchy,
        FastMousePan,
        FeedbackMode,
        ForceBoundingBox,
        FilePreview,
        FriendlyColorMap,
        FriendlyOutlineMap,
        InitConsoleWindow,
        InvertImages,
        MagneticNets,
        NetDirection,
        NotifyEvents,
        ObjectClipping,
        PointerHScroll,
        RipperRotation,
        RipperType,
        RubberNets,
        ScrollBars,
        ScrollBarsVisible,
        SortLibrary,
        TextOriginMarker,
        UndoViews,
        WarpCursor,
        ZoomPan,
/* View Ids */
        PotentialAttributes,
        SelectedAttributes,
} ControlID;

/* Function Prototypes */

/* Defined in x_settings.c */

void configure_dialog_response(GtkWidget *w, gint response, GschemToplevel *w_current);

int  get_titleblock_cnt(void);
bool get_titleblock_list(char **Buffer);

int  generate_rc(GschemToplevel *w_current, const char *rcname);

/* Defined in x_settings_dialog.c */
bool gtk_tree_model_iter_previous (GtkTreeModel *tree_model, GtkTreeIter *iter);
int  gtk_radio_group_get_active(GSList *RadioGroupList);

void initialize_tree_View(GtkTreeView *list, int list_item, int nColumns, int DataType);
void load_tree_view_gl( GtkTreeView *TreeView, GList *list);
void load_tree_view_str( GtkTreeView *TreeView, const char *list[]);

GtkWidget* create_settings_dialog (GschemToplevel *w_current);
bool load_settings_dialog (GschemToplevel *w_current);
void GatherSettings(GschemToplevel *w_current);

#endif /* __X_SETTINGS_H__ */
