/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_toolbar.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill <wileyhill@gmail.com>
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 * Date: January, 31, 2013
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */
/*!
 * \file x_toolbars.h
 * \brief header file for the toolbar module
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

/*! \brief ToolBar Macro with GSCHEM_TOPLEVEL *w_current call-back data */
#define GSCHEM_TOOLBAR_BUTTON( bar, name) \
   GtkWidget *name##_button __attribute__ ((unused)); /* maybe mv comment for tb devel */ \
   GEDA_TOOLBAR_BUTTON(bar##_Toolbar, TB_ICON_IMAGE(name), name##_button, \
   _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), x_toolbars_execute, w_current) \
   GEDA_OBJECT_SET_DATA (name##_button, (void*)TB_ACTION(name), "action"); \
   GEDA_TOOLBAR_BUTTON_ATK(bar##_Toolbar, name##_button, TB_TOOLTIP (name), TB_ACTION(name)) \
   gtk_widget_show(name##_button);

   /*! \brief ToolBar Macro with GSCHEM_TOPLEVEL *w_current call-back data */
#define GSCHEM_TOOLBAR_BUTTON_FUNC( bar, name, func) \
   GtkWidget *name##_button __attribute__ ((unused)); /* maybe mv comment for tb devel */ \
   GEDA_TOOLBAR_BUTTON(bar##_Toolbar, TB_ICON_IMAGE(name), name##_button, \
   _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), func, w_current) \
   GEDA_OBJECT_SET_DATA (name##_button, (void*)TB_ACTION(name), "action"); \
   GEDA_TOOLBAR_BUTTON_ATK(bar##_Toolbar, name##_button, TB_TOOLTIP (name), TB_ACTION(name)) \
   gtk_widget_show(name##_button);

/*! These enumerations are attached to each toolbar object with the "BarId"
 *  string, these are used to save and restore toolbar states and reference
 *  menu strings, see IDS_Menu_Toolbar_Toggles in x_menu.c.
 */
typedef enum { tb_Add,  tb_Attribute, tb_Edit,     tb_Grid_Snap, tb_Modify,
               tb_Page, tb_Select,    tb_Standard, tb_Symbol,    tb_Zoom

} ID_GSCHEM_Toolbar;

typedef struct {
  GedaHandleBox *handlebox;
  GedaToolbar   *toolbar;
} ToolBarInfo;

typedef struct {

  GSList *toolbar_slist;       /* Single-linked list of all toolbars */

  /* The are Single-linked list of button grouped for sensitivity */
  GSList *any_object;    /* List of widgets on toolbars to set if some object is selected */
  GSList *can_paste;
  GSList *can_undo;
  GSList *can_redo;
  GSList *can_hatch;
  GSList *can_edit_line;
  GSList *complex_selected;
  GSList *multi_pages;
  GSList *pin_selected;
  GSList *text_selected;

  GSList *toolbar_radio_list;  /* Single-linked list of mode radios on toolbars */

  /* Drawing Mode Radio Widgets on the Standard Bar */
  GtkWidget *toolbar_arc;
  GtkWidget *toolbar_box;
  GtkWidget *toolbar_bus;
  GtkWidget *toolbar_circle;
  GtkWidget *toolbar_line;
  GtkWidget *toolbar_net;
  GtkWidget *toolbar_path;
  GtkWidget *toolbar_pic;
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
#define TBTS_FILE_OPEN          "Open an existing schematic or symbol file"
#define TBTS_FILE_SAVE          "Save the current document"
#define TBTS_FILE_SAVE_AS       "Save the current document to a new name or location"
#define TBTS_FILE_CLOSE         "Close the current document"

#define TBTS_FILE_PRINT         "Open the Print Dialog"
#define TBTS_FILE_WRITE_PDF     "Create PDF document"

#define TBTS_EDIT_CB_CUT        "Cut the current selection to the system clipboard"
#define TBTS_EDIT_CB_COPY       "Copy the current selection to the system clipboard"
#define TBTS_EDIT_CB_PASTE      "Paste the contents of the system clipboard"

#define TBTS_EDIT_UNDO          "Undo the last operation"
#define TBTS_EDIT_REDO          "Redo the last undo"

#define TBTS_EDIT_SELECT        "Activate Select mode"
#define TBTS_EDIT_DESELECT      "Activate Deselect mode"
#define TBTS_EDIT_DESELECT_ALL  "Unselect everything"

#define TBTS_EDIT_SELECT_ALL    "Select all objects"
#define TBTS_EDIT_INVERT        "Invert the current selection set"

#define TBTS_ADD_COMPONENT      "Insert a library symbol"
#define TBTS_ADD_NET            "Add Nets mode\nRight mouse button to cancel"
#define TBTS_ADD_BUS            "Add Buses mode\nRight mouse button to cancel"
#define TBTS_ADD_ATTRIB         "Add attribute"
#define TBTS_ADD_TEXT           "Add text"

  /* Add Toolbar */
#define TBTS_ADD_LINE           "Add a line"
#define TBTS_ADD_BOX            "Add a box"
#define TBTS_ADD_CIRCLE         "Add a circle"
#define TBTS_ADD_ARC            "Create an arc"
#define TBTS_ADD_PATH           "Add a path"
#define TBTS_ADD_PIN            "Add a pin"
#define TBTS_ADD_PICTURE        "Insert an image"

  /* Page Toolbar */
#define TBTS_PAGE_FIRST         "Go to the first page"
#define TBTS_PAGE_UP            "Switch to the previous page"
#define TBTS_PAGE_DOWN          "Switch to the next page"
#define TBTS_PAGE_LAST          "Go to the last page"
#define TBTS_PAGE_NEW           "Create a new page"
#define TBTS_PAGE_MANAGER       "Open the Page Manager"

#define TBTS_DOWN_SCHEMATIC     "Lower schematic hierarchy"
#define TBTS_DOWN_SYMBOL        "Lower symbol hierarchy"
#define TBTS_HIERARCHY_UP       "Elevate hierarchy"

#define TBTS_VIEW_DOCUMENT      "View component documentation"
#define TBTS_VIEW_NETNAMES      "Toggle visibility of net name attributes"

  /* Zoom Toolbar */
#define TBTS_VIEW_REDRAW        "Redraw current display"
#define TBTS_VIEW_PAN           "Zoom Pan"
#define TBTS_VIEW_BOX           "Zoom Window"
#define TBTS_VIEW_SELECTED      "Zoom to selection"
#define TBTS_VIEW_EXTENTS       "Zoom to extents"
#define TBTS_VIEW_ZOOM_IN       "Zoom In"
#define TBTS_VIEW_ZOOM_OUT      "Zoom Out"
#define TBTS_VIEW_ZOOM_ALL      "Zoom to Limits"

  /* Symbol Toolbar */
#define TBTS_EDIT_PROPERTIES    "Edit Component properties"
#define TBTS_TOOLS_TRANSLATE    "Translate component positions"
#define TBTS_SELECT_LOCK        "Lock Objects"
#define TBTS_SELECT_UNLOCK      "Unlock Objects"
#define TBTS_TOOLS_UPDATE       "Reload Component from library"

  /* Edit Toolbar */
#define TBTS_EDIT_COPY          "Copy selection"
#define TBTS_EDIT_MCOPY         "Make multiple copies of selection"
#define TBTS_EDIT_MOVE          "Move objects"
#define TBTS_EDIT_MIRROR        "Mirror objects"
#define TBTS_EDIT_OFFSET        "Offset selected objects"
#define TBTS_EDIT_ROTATE_LEFT   "Rotate objects left"
#define TBTS_EDIT_ARRAY         "Create and array of objects"
#define TBTS_EDIT_BREAK         "Break an object into pieces"
#define TBTS_EDIT_EXTEND        "Project linear objects to other objects"

  /* Modify Toolbar */
#define TBTS_EDIT_ATTRIB        "Edit Object Attributes"
#define TBTS_EDIT_COLOR         "Open the Color Editor Dialog"

#define TBTS_EDIT_TEXT          "Edit text properties"
#define TBTS_EDIT_SLOT          "Edit Slot number of complex"
#define TBTS_EDIT_PIN           "Open the Pin Editor"
#define TBTS_EDIT_LINE          "Edit line type"
#define TBTS_EDIT_FILL          "Edit Hatch pattern"
#define TBTS_EDIT_ARC           "Edit Arc parameters"

  /* Attribute Toolbar */
#define TBTS_ATTRIB_ATTACH      "Attach selected attributes to symbol"
#define TBTS_ATTRIB_DETACH      "Dettach selected attribute"
#define TBTS_ATTRIB_VALUE       "Set selected value visible"
#define TBTS_ATTRIB_NAME        "Set selected name visible"
#define TBTS_ATTRIB_BOTH        "Set selected name and value visible"
#define TBTS_ATTRIB_VISIBILITY  "Toggle attribute visibility"
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

#endif /* __X_TOOLBARS_H__ */
