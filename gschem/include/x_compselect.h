/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_compselect.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 */
/*!
 * \file x_compselect.h
 *
 * \brief header for the Component Selection Dialog module
 * The Component-Dialog Group is used to show images of the Dialog
 * in the Doxygen generated source documnetation, before the other
 * stuff related to the Component Select Dialog.
 */

/** \defgroup Component-Dialog Component Selection Dialog
 *  @{
 *  \ingroup Standard-Dialogs
 *  \image html component_select_dialog.png
 *  \image latex component_select_dialog.png
 *  @} end group Component-Dialog
 */

/*! \class Compselect x_compselect.h "x_compselect.h"
 *  \brief Component Selection Dialog
 *  \par
 *  Component Selection Dialog ...
 */

#ifndef __X_COMPSELECT_H__
#define __X_COMPSELECT_H__

#define ThisDialog compselect
#define DialogTitle "Select Component..."
#define DialogSettings IDS_COMP_SELECT

typedef enum {
  COMPSELECT_BEHAVIOR_REFERENCE,
  COMPSELECT_BEHAVIOR_EMBED,
  COMPSELECT_BEHAVIOR_INCLUDE
} CompselectBehavior;

typedef enum {
  COMPSELECT_STYLE_NONE =   0,
  COMPSELECT_STYLE1     =   1,
  COMPSELECT_STYLE2     =   2,
  COMPSELECT_STYLE3     =   4,
  COMPSELECT_STYLE4     =   8,
  COMPSELECT_STYLE5     =  16,
  COMPSELECT_STYLE6     =  32,
  COMPSELECT_STYLE7     =  64,
  COMPSELECT_STYLE8     = 128,
  COMPSELECT_STYLE_ALL  = 255
} CompselectStyle;

/* Response IDs for special dialog buttons */
typedef enum {
  COMPSELECT_RESPONSE_PLACE   = 1,
  COMPSELECT_RESPONSE_HIDE    = 2,
  COMPSELECT_RESPONSE_REFRESH = 3
} CompselectResponseType;

GedaType compselect_behavior_get_type (void);
#define COMPSELECT_TYPE_BEHAVIOR  (compselect_behavior_get_type ())

/*
 * Compselect
 */

#define TYPE_COMPSELECT           (compselect_get_type())
#define COMPSELECT(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_COMPSELECT, Compselect))
#define COMPSELECT_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_COMPSELECT, CompselectClass))
#define IS_COMPSELECT(obj)        (is_a_compselect((Compselect*)(obj)))
#define COMPSELECT_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), TYPE_COMPSELECT, CompselectClass))

typedef struct _CompselectClass CompselectClass;
typedef struct _Compselect      Compselect;

struct _CompselectClass {

  GschemDialogClass parent_class;

  unsigned int behavior_changed_signal_id;

  void (* refresh)   (Compselect *compselect);
};

struct _Compselect {

  GschemDialog    parent_instance;
  GedaType        instance_type;

  GtkWidget      *hpaned, *vpaned;
  GtkWidget      *filter_hbox;

  GtkTreeView    *attrtreeview;
  GtkTreeView    *inusetreeview;
  GtkTreeView    *stdtreeview;
  GtkTreeView    *mantreeview;
  GtkTreeView    *simtreeview;
  GtkTreeView    *localtreeview;

  GtkNotebook    *notebook;
  GschemPreview  *preview;
  GedaEntry      *entry_filter;
  GtkButton      *button_clear;
  unsigned int    filter_timeout;
  bool            applying_filter;

  GtkTooltips    *tooltips;
  bool            show_tips;

  GedaOptionMenu *behavior_menu;

  GedaMenuButton *style_menu;
  GedaMenu       *menu;

  GSList         *style_menu_widgets;
  unsigned int    style_flag;

  bool hidden;
  bool do_sort;
  bool show_groups;
  bool subgroups;
  bool rescan_lib;
  int  active_tab;
};

GedaType compselect_get_type (void) GEDA_CONST;
bool     is_a_compselect     (Compselect *dialog);

#endif /* __X_COMPSELECT_H__ */
