/* C header
 *  File: x_toolbar.h
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2014 Ales Hvezda
 *
 * Copyright (C) 2013 Wiley Edward Hill <wileyhill@gmail.com>
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
 * MA 02111-1301 USA
 *
 * Date: January, 31, 2013
 * Contributing Author: Wiley Edward Hill
 *
 */
/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 01/31/13 |  Inital release.
 * ------------------------------------------------------------------
 *
 * ------------------------------------------------------------------
 */
#ifndef __X_TOOLBARS_H__
#define __X_TOOLBARS_H__

typedef enum { tb_Add,  tb_Attribute, tb_Edit,      tb_Grid_Snap,
               tb_Page, tb_Select,    tb_Standard,  tb_Zoom

} ID_GSCHEM_Toolbar;

typedef struct {
  GedaHandleBox   *handlebox;
  GtkToolbar      *toolbar;
} ToolBarInfo;

typedef struct {

  GSList *toolbar_slist;       /* Single-linked list of all toolbars */

  /* The are Single-linked list of button grouped for sensitivity */
  GSList *can_paste;
  GSList *can_undo;
  GSList *can_redo;
  GSList *have_pages;
  GSList *complex_object;
  GSList *some_object;    /* List of widgets on toolbars to set if some object is selected */
  GSList *text_object;

  GSList *toolbar_radio_list;  /* Single-linked list of mode radios on toolbars */

  /* Radio Drawing Mode Widgets on the Standard Bar */
  GtkWidget *toolbar_arc;
  GtkWidget *toolbar_box;
  GtkWidget *toolbar_bus;
  GtkWidget *toolbar_circle;
  GtkWidget *toolbar_line;
  GtkWidget *toolbar_net;
  GtkWidget *toolbar_path;
  GtkWidget *toolbar_pin;
  GtkWidget *toolbar_select;
  GtkWidget *toolbar_deselect;
  GtkWidget *toolbar_none;

  /* Radio Widgets on the Grid_Snap Bar */
  GtkWidget *toolbar_dot;
  GtkWidget *toolbar_mesh;
  GtkWidget *toolbar_off;

} ToolBarWidgets;

/* ---------------------- Toolbar tooltip strings ---------------------- */
   /* Standard Toolbar*/
#define TBTS_FILE_NEW           "Create a new file"
#define TBTS_FILE_OPEN          "Open file"
#define TBTS_FILE_SAVE          "Save file"
#define TBTS_FILE_SAVE_AS       "Save the file to different name or location"
#define TBTS_FILE_CLOSE         "Close the current file"

#define TBTS_FILE_PRINT         "Open the Print Dialog"
#define TBTS_FILE_WRITE_PDF     "Create PDF document"

#define TBTS_EDIT_CB_CUT        "Cut selection to the clipboard"
#define TBTS_EDIT_CB_COPY       "Copy selection to the clipboard"
#define TBTS_EDIT_CB_PASTE      "Paste selection from the clipboard"

#define TBTS_EDIT_UNDO          "Undo the last operation"
#define TBTS_EDIT_REDO          "Redo the last undo"

#define TBTS_EDIT_SELECT        "Activate Select mode"
#define TBTS_EDIT_DESELECT      "Activate UnSelect mode"
#define TBTS_EDIT_DESELECT_ALL  "Unselect everything"

#define TBTS_EDIT_SELECT_ALL    "Select all objects in the database"
#define TBTS_EDIT_INVERT        "Invert the current selection set"

#define TBTS_ADD_COMPONENT      "Add Component...\nSelect library and component from list, move the mouse into main window, click to place\nRight mouse button to cancel"
#define TBTS_ADD_NET            "Add Nets mode\nRight mouse button to cancel"
#define TBTS_ADD_BUS            "Add Buses mode\nRight mouse button to cancel"
#define TBTS_ADD_ATTRIB         "Add Attribute..."
#define TBTS_ADD_TEXT           "Add Text..."

  /* Add Toolbar */
#define TBTS_ADD_LINE           "Add line"
#define TBTS_ADD_BOX            "Add Box"
#define TBTS_ADD_CIRCLE         "Add Circle"
#define TBTS_ADD_ARC            "Add Arc"
#define TBTS_ADD_PATH           "Add Path"
#define TBTS_ADD_PIN            "Add Pin"
#define TBTS_ADD_PICTURE        "Insert an image"

  /* Page Toolbar */
#define TBTS_PAGE_PREV          "Switch to the previous page"
#define TBTS_PAGE_NEXT          "Switch to the next page"
#define TBTS_PAGE_NEW           "Create a new page"
#define TBTS_PAGE_MANAGER       "Open the Page Manager"

#define TBTS_DOWN_SCHEMATIC     "Lower schematic hierarchy"
#define TBTS_DOWN_SYMBOL        "Lower symbol hierarchy"
#define TBTS_HIERARCHY_UP       "Elevate hierarchy"

#define TBTS_VIEW_DOCUMENT      "View component documentation"

  /* Zoom Toolbar */
#define TBTS_VIEW_REDRAW        "Redraw current display"
#define TBTS_VIEW_PAN           "Zoom Pan"
#define TBTS_VIEW_BOX           "Zoom Window"
#define TBTS_VIEW_SELECTED      "Zoom to selection"
#define TBTS_VIEW_EXTENTS       "Zoom to extents"
#define TBTS_VIEW_ZOOM_IN       "Zoom In"
#define TBTS_VIEW_ZOOM_OUT      "Zoom Out"
#define TBTS_VIEW_ZOOM_ALL      "Zoom to Limits"

  /* Edit Toolbar */
#define TBTS_EDIT_COPY          "Copy selection"
#define TBTS_EDIT_MCOPY         "Make multible copies of selection"
#define TBTS_EDIT_MOVE          "Move objects"
#define TBTS_EDIT_ROTATE        "Rotate objects"
#define TBTS_EDIT_MIRROR        "Mirror objects"

#define TBTS_EDIT_ATTRIB        "Edit Attribute properties"
#define TBTS_EDIT_COLOR         "Change Colors"

#define TBTS_EDIT_TEXT          "Edit text properties"
#define TBTS_EDIT_SLOT          "Edit Slot number of complex"
#define TBTS_EDIT_PIN           "Open the Pin Editor"
#define TBTS_EDIT_LINE          "Edit line type"
#define TBTS_EDIT_FILL          "Edit Hatch pattern"
#define TBTS_EDIT_ARC           "Edit Arc parameters"

#define TBTS_EDIT_LOCK          "Lock Objects"
#define TBTS_EDIT_UNLOCK        "Unlock Objects"

  /* Attribute Toolbar */
#define TBTS_ATTRIB_ATTACH      "Attach selected attribute"
#define TBTS_ATTRIB_DETACH      "Dettach selected attribute"
#define TBTS_ATTRIB_VALUE       "Set selected value visible"
#define TBTS_ATTRIB_NAME        "Set selected name visible"
#define TBTS_ATTRIB_BOTH        "Set selected name and value visible"
#define TBTS_ATTRIB_VISIBILITY  "Toggle Visibilty"
#define TBTS_VIEW_HIDDEN        "Toggle hidden text attributes"
#define TBTS_VIEW_INHERITED     "Toggle hidden inherited text attributes"
#define TBTS_ATTRIB_FIND        "Find attribute"
#define TBTS_ATTRIB_HIDE        "Hide selected attribute"
#define TBTS_ATTRIB_SHOW        "Show a specific attribute value"

#define TBTS_OPT_GRID_DOT       "Set the Grid Display to Dots Mode"
#define TBTS_OPT_GRID_MESH      "Set the Grid Display to Mesh Mode"
#define TBTS_OPT_GRID_OFF       "Turn the display grid off"

#define TBTS_OPT_SNAP_UP        "Increase the Snap size"
#define TBTS_OPT_SNAP_DOWN      "Descrease the Snap size"
#define TBTS_OPT_SNAP_SIZE      "Open the Snap Size dialog"
#define TBTS_OPT_SNAP_OFF       "Turn Snap mode off"
#define TBTS_OPT_SNAP_ON        "Turn Snap mode on"

#define TBTS_OPT_SETTINGS       "Set configuration preferences"

#define TBTS_TOOLS_AUTONUM      "Open Auto Number dialog"
#define TBTS_TOOLS_TRANSLATE    "Translate component positions"
#define TBTS_TOOLS_UPDATE       "Reload Component from library"

#endif /* __X_TOOLBARS_H__ */



