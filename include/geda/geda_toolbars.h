/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: toolbars.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
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
 */
/* ------------------------------------------------------------------
 * WEH | 05/02/15 | Modify ToolbarStringData structure; remove members
 *                | Widget and Private, add member icon (to hold a pointer
 *                | to the image widget for the button.)
 * ------------------------------------------------------------------
 * WEH | 08/12/15 | Modify cast is macro GET_TOOLBAR_ID; change long pointer
 *                | long.
 * ------------------------------------------------------------------
 * WEH | 02/01/18 | Replace GTK_SIGNAL_FUNC with G_CALLBACK.
*/

/* ------------------------------------------------------------------ */

#ifndef __GEDA__TOOLBARS__
#define __GEDA__TOOLBARS__

/*! \struct ToolbarStringData
 *  \brief  String data record for creating toolbar items
 *  \par
 *  action:
 *           1. Must be unadulterated action string
 *              a. string is loaded into widget as "action" data, which is later
 *                 retrieved by func (x_toolbars_execute) when the button is clicked.
 *              b. Primary icon is determined from the action string
 */
typedef struct
{
   const char *action;
   const char *Label;
   const char *Tip;
   const char *icon_id; /* Default and Backup Icon name */
   int         iflag;   /* Flag mask use to describe the icon source */
   GtkWidget  *icon;
} ToolbarStringData;

typedef struct
{
   char *Widget;
   int   ButtonId;
   int   ToolBarId;
   char *stock_id;

} ToolbarItem;

typedef struct
{
   int  ToolBarId;

} ToolbarConfig;

#define TB_SMALL_ICON GTK_ICON_SIZE_SMALL_TOOLBAR

#define TB_ACTION(member)     ToolbarStrings[member].action
#define TB_LABEL(member)      ToolbarStrings[member].Label
#define TB_TOOLTIP(member)    ToolbarStrings[member].Tip
#define TB_ICON_NAME(member)  ToolbarStrings[member].icon_id
#define TB_ICON_IMAGE(member) ToolbarStrings[member].icon

#define TB_BUTTON(member) member##_button

#define GET_TOOLBAR_ID(obj) (int)(long) g_object_get_data ((GObject*)obj, "BarId");
#define SET_TOOLBAR_ID(obj, bar_id) g_object_set_data((GObject*)obj, "BarId", (void*)(long)(bar_id));
#define GET_TOOLBAR_WC(obj) (long) g_object_get_data ((GObject*)obj, "WinData");
#define SET_TOOLBAR_WC(obj, win_cur) g_object_set_data((GObject*)obj, "WinData", win_cur);

#define SET_TOOLBAR_ORIENTATION(bar, orient) \
     gtk_orientable_set_orientation (GTK_ORIENTABLE (bar), GTK_ORIENTATION_##orient);

/*------------------------------------------------------------------
 * GTK Toolbar includes: stuff for dealing with Toolbars.
 *------------------------------------------------------------------*/
#define GEDA_TOOLBAR_BUTTON_ATK(bar, button, tip, action) \
   { \
     AtkObject *atk_obj = gtk_widget_get_accessible(button); \
     char *str = geda_strconcat("Geda-toolbar-", action, "-button", NULL); \
     gtk_widget_set_name (button, str); \
           str = g_strdelimit(str, "-", ' ' ); \
     atk_object_set_name (atk_obj, _(str)); \
     atk_object_set_description(atk_obj, tip); \
     g_free(str); \
   }

#define GEDA_PACK_DOCKBOX(parent, bar, where) \
  geda_dock_box_add((GedaDockBox*)parent, bar, where); \
  gtk_widget_show(bar);

#define GEDA_PACK_TOOLBOX( parent, bar) \
  gtk_box_pack_start (GTK_BOX (parent), bar, FALSE, FALSE, 0); \
  gtk_widget_show(bar);

/* ------------------------- Entry Level -------------------------------- */
/*! \brief ToolBar Macro with GSCHEM_TOPLEVEL *w_current call-back data */
#define TOOLBAR_GEDA_BUTTON( bar, name, type, icon, func, data) \
   GtkWidget *name##_button __attribute__ ((unused)); /* maybe mv comment for tb devel */ \
   TOOLBAR_BUTTON_DATA( bar##_Toolbar, name, type, icon, name##_button, func, data)

/*! \brief ToolBar Macro with Enumerated Control call-back data */
#define TOOLBAR_STD_BUTTON( bar, name, type, icon, func) \
   GtkWidget *name##_button; \
   TOOLBAR_BUTTON( bar##_Toolbar, name, type, icon, name##_button, func, name)

/*! \brief ToolBar Macro with Enumerated Control and Assumed call-back func */
#define TOOLBAR_STD_FUNC( bar, name, type, icon) \
   GtkWidget *name##_button; \
   TOOLBAR_BUTTON( bar##_Toolbar, name, type, icon, name##_button, callBack_##bar##Bar, name)

/* --------------------------- 1st Level -------------------------------- */
/*! \brief 1st LeveL Intermediate ToolBar Macro with Enumerated data */
#define TOOLBAR_BUTTON( bar, name, type, icon, button, func, data) { \
        TOOLBAR_BUTTON_##type (bar, name, ENUM, icon, button, func, data) \
        GEDA_TOOLBAR_BUTTON_ATK(bar, button, TB_TOOLTIP (name), TB_ACTION(name)) \
        gtk_widget_show (button); \
}

/*! \brief 1st LeveL Intermediate ToolBar Macro with *w_current data */
#define TOOLBAR_BUTTON_DATA( bar, name, type, icon, button, func, data) { \
        TOOLBAR_BUTTON_##type (bar, name, GEDA, icon, button, func, data) \
        GEDA_TOOLBAR_BUTTON_ATK(bar, button, TB_TOOLTIP (name), TB_ACTION(name)) \
        gtk_widget_show (button); \
}

/* --------------------------- 2nd Level -------------------------------- */
/*! \brief 2nd Level Intermediate ToolBar Macro with Pixmap Icon */
#define TOOLBAR_BUTTON_PIX(bar, name, next, icon, button, func, data) \
   tmp_toolbar_icon = create_pixmap (icon); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, tmp_toolbar_icon, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_ACTION(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Stock Icon */
#define TOOLBAR_BUTTON_STK(bar, name, next, icon, button, func, data) \
   tmp_toolbar_icon = gtk_image_new_from_stock(STOCK_MAP(icon), TB_SMALL_ICON); \
   next##_TOOLBAR_BUTTON(bar, tmp_toolbar_icon, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_ACTION(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Local Pixmap Icon */
#define TOOLBAR_BUTTON_LOCAL_PIX(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = create_pixmap (icon); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_ACTION(name));

#define TOOLBAR_BUTTON_LOCAL_STR(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = create_pixmap (TB_ICON_NAME(name)); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_ACTION(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Local Factory Icon */
#define TOOLBAR_BUTTON_LOCAL_FAC(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_stock(TB_ICON_NAME(name), TB_SMALL_ICON); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_ACTION(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Action Theme Icon */
#define TOOLBAR_BUTTON_THEME(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_icon_name(TB_ICON_NAME(name), GTK_ICON_SIZE_BUTTON); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_ACTION(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Local Stock Icon */
#define TOOLBAR_BUTTON_LOCAL_STK(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_stock(STOCK_MAP(icon), TB_SMALL_ICON); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_ICON_NAME (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_ACTION(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Item Data Struct */
#define TOOLBAR_BUTTON_LOCAL_ALT(bar, name, next, icon, button, func, data) \
   ToolbarItem name##_ToolbarItem; /* Allocate a Data Structure */ \
   name##_ToolbarItem.stock_id =  STOCK_MAP(icon); \
   name##_ToolbarItem.ButtonId =  name; \
   GtkWidget *name##_ICN = get_stock_alt_pixmap (w_current, &name##_ToolbarItem); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_TOOLTIP (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_ACTION(name));

/* --------------------------- 3rd Level -------------------------------- */
#define ENUM_TOOLBAR_BUTTON(bar, icon, button, txt, tip, priv, func, data) \
   g_object_set (icon, "visible", TRUE, NULL); \
   button = geda_toolbar_append_item(GEDA_TOOLBAR(bar), txt, \
                                     tip, priv, \
                                     GTK_WIDGET(icon), /* GtkWidget */ \
                                     G_CALLBACK(func), \
                                     (void*)(unsigned int)data); /* ptr to IDS_xxxx_Toolbar enumerator */

#define GEDA_TOOLBAR_BUTTON(bar, icon, button, txt, tip, priv, func, data) \
   g_object_set (icon, "visible", TRUE, NULL); \
   button = geda_toolbar_append_item(GEDA_TOOLBAR(bar), txt, \
                                     tip, priv, \
                                     GTK_WIDGET(icon), /* GtkWidget */ \
                                     G_CALLBACK(func), \
                                     data); /* MUST be PTR to something */

#ifdef ToolBar_Radio_Responder

#define TOOLBAR_GSCHEM_RADIO( bar, var, grp, name, data) \
    TOOLBAR_GSCHEM_RADIO_STK( bar, var, grp, name, TB_ICON_NAME(name), ToolBar_Radio_Responder, data)
#else
#define TOOLBAR_GSCHEM_RADIO( bar, var, grp, name, func, data) \
    TOOLBAR_GSCHEM_RADIO_STK( bar, var, grp, name, TB_ICON_NAME(name), func, data)
#endif

#define TOOLBAR_GSCHEM_RADIO_STK( bar, var, grp, name, icon, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_stock(icon, TB_SMALL_ICON); \
   TOOLBAR_GSCHEM_RADIO_ELEMENT ( bar##_Toolbar, name##_ICN, var, grp, name, func, data)

#define TOOLBAR_GSCHEM_RADIO_ELEMENT( bar, icon, var, grp, name, func, data) \
   var = geda_toolbar_append_element(GEDA_TOOLBAR(bar), \
                                     GEDA_TOOLBAR_CHILD_RADIOBUTTON, \
                                     grp, \
                                   _(TB_LABEL (name)), \
                                   _(TB_TOOLTIP (name)), \
                                     TB_ACTION(name), \
                                     GTK_WIDGET(icon), \
                                    (GCallback) func, \
                                     data); \
   g_object_set_data ((GObject*) var, "action", (void*)TB_ACTION(name)); \
   { \
     AtkObject *atk_obj = gtk_widget_get_accessible(var); \
     char *str = geda_strconcat("Geda-toolbar-button-", TB_ACTION(name), "-mode", NULL); \
     gtk_widget_set_name (var, str); \
           str = g_strdelimit(str, "-", ' ' ); \
     atk_object_set_name (atk_obj, _(str)); \
     atk_object_set_description(atk_obj,_(TB_TOOLTIP (name))); \
     g_free(str); \
   }

#endif /* __GEDA__TOOLBARS__ */

