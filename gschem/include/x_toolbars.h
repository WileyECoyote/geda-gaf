/* C header
 *  File: x_toolbar.h
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
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

#endif /* __X_TOOLBARS_H__ */



