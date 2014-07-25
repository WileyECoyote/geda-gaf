/* C header                                           -*- toolbars.h -*-
 * file: toolbars.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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
 * MA 02110-1301 USA
 */

/*!
 * \file
 * \brief Global variable declarations
 *
 * \section
 *
 */
/* ------------------------------------------------------------------ */

#ifndef __GEDA__TOOLBARS__
#define __GEDA__TOOLBARS__

typedef struct
{
   const char *Widget;
   const char *Label;
   const char *Tip;
   const char *Private;
} ToolbarStringData;

typedef struct
{
   char *Widget;
   int  ButtonId;
   int  ToolBarId;
   char *stock_id;

} ToolbarItem;

typedef struct
{
   int  ToolBarId;

} ToolbarConfig;

#define TB_WIDGET(member)    ToolbarStrings[member].Widget
#define TB_LABEL(member)     ToolbarStrings[member].Label
#define TB_TOOLTIP(member)   ToolbarStrings[member].Tip
#define TB_PRIVATE(member)   ToolbarStrings[member].Private
#define TB_ICON_NAME(member) ToolbarStrings[member].Private

#define TB_BUTTON(member) member##_button

#define GET_TOOLBAR_ID(obj) (int)(long*) g_object_get_data(G_OBJECT(obj), "BarId");
#define SET_TOOLBAR_ID(obj, bar_id) g_object_set_data(G_OBJECT(obj), "BarId", GINT_TO_POINTER(bar_id));
#define GET_TOOLBAR_WC(obj) (int)(long*) g_object_get_data(G_OBJECT(obj), "WinData");
#define SET_TOOLBAR_WC(obj, win_cur) g_object_set_data(G_OBJECT(obj), "WinData", GINT_TO_POINTER(win_cur));

/*------------------------------------------------------------------
 * GTK Toolbar includes: stuff for dealing with Toolbars.
 *------------------------------------------------------------------*/
#define GEDA_TOOLBAR_BUTTON_ATK(bar, button, tip, action) \
   { \
     AtkObject *atk_obj = gtk_widget_get_accessible(button); \
     char *str = g_strconcat("Geda-toolbar-", action, "-button", NULL); \
     gtk_widget_set_name (button, str); \
           str = g_strdelimit(str, "-", ' ' ); \
     atk_object_set_name (atk_obj, _(str)); \
     atk_object_set_description(atk_obj, tip); \
     g_free(str); \
   }

#define GEDA_PACK_TOOLBOX( parent, bar) \
  gtk_box_pack_start (GTK_BOX (parent), bar, FALSE, FALSE, 0); \
  g_object_set (bar, "visible", TRUE, NULL); \
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
        GEDA_TOOLBAR_BUTTON_ATK(bar, button, TB_TOOLTIP (name), TB_WIDGET(name)) \
        g_object_set (button, "visible", TRUE, NULL); \
}

/*! \brief 1st LeveL Intermediate ToolBar Macro with *w_current data */
#define TOOLBAR_BUTTON_DATA( bar, name, type, icon, button, func, data) { \
        TOOLBAR_BUTTON_##type (bar, name, GEDA, icon, button, func, data) \
        GEDA_TOOLBAR_BUTTON_ATK(bar, button, TB_TOOLTIP (name), TB_WIDGET(name)) \
        g_object_set (button, "visible", TRUE, NULL); \
}

/* --------------------------- 2nd Level -------------------------------- */
/*! \brief 2nd Level Intermediate ToolBar Macro with Pixmap Icon */
#define TOOLBAR_BUTTON_PIX(bar, name, next, icon, button, func, data) \
   tmp_toolbar_icon = create_pixmap (icon); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, tmp_toolbar_icon, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_WIDGET(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Stock Icon */
#define TOOLBAR_BUTTON_STK(bar, name, next, icon, button, func, data) \
   tmp_toolbar_icon = gtk_image_new_from_stock(STOCK_MAP(icon), GTK_ICON_SIZE_SMALL_TOOLBAR); \
   next##_TOOLBAR_BUTTON(bar, tmp_toolbar_icon, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_WIDGET(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Local Pixmap Icon */
#define TOOLBAR_BUTTON_LOCAL_PIX(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = create_pixmap (icon); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_WIDGET(name));

#define TOOLBAR_BUTTON_LOCAL_STR(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = create_pixmap (TB_ICON_NAME(name)); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_WIDGET(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Local Factory Icon */
#define TOOLBAR_BUTTON_LOCAL_FAC(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_stock(TB_ICON_NAME(name), GTK_ICON_SIZE_SMALL_TOOLBAR); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_WIDGET(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Action Theme Icon */
#define TOOLBAR_BUTTON_THEME(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_icon_name(TB_ICON_NAME(name), GTK_ICON_SIZE_BUTTON); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_WIDGET(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Local Stock Icon */
#define TOOLBAR_BUTTON_LOCAL_STK(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_stock(STOCK_MAP(icon), GTK_ICON_SIZE_SMALL_TOOLBAR); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_WIDGET(name));

/*! \brief 2nd Level Intermediate ToolBar Macro with Item Data Struct */
#define TOOLBAR_BUTTON_LOCAL_ALT(bar, name, next, icon, button, func, data) \
   ToolbarItem name##_ToolbarItem; /* Allocate a Data Structure */ \
   name##_ToolbarItem.stock_id =  STOCK_MAP(icon); \
   name##_ToolbarItem.ButtonId =  name; \
   GtkWidget *name##_ICN = get_stock_alt_pixmap (w_current, &name##_ToolbarItem); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_TOOLTIP (name)), func, data) \
   g_object_set_data ((GObject*) button, "action", (void*)TB_WIDGET(name));

/* --------------------------- 3rd Level -------------------------------- */
#define ENUM_TOOLBAR_BUTTON(bar, icon, button, txt, tip, priv, func, data) \
   g_object_set (icon, "visible", TRUE, NULL); \
   button = gtk_toolbar_append_item( GTK_TOOLBAR(bar), txt, \
                                     tip, priv, \
                                     GTK_WIDGET(icon), /* GtkWidget */ \
                                     GTK_SIGNAL_FUNC(func), \
                                     GUINT_TO_POINTER (data)); /* ptr to IDS_xxxx_Toolbar enumerator */

#define GEDA_TOOLBAR_BUTTON(bar, icon, button, txt, tip, priv, func, data) \
   g_object_set (icon, "visible", TRUE, NULL); \
   button = gtk_toolbar_append_item( GTK_TOOLBAR(bar), txt, \
                                     tip, priv, \
                                     GTK_WIDGET(icon), /* GtkWidget */ \
                                     GTK_SIGNAL_FUNC(func), \
                                     data); /* MUST be PTR to something */

#ifdef ToolBar_Radio_Responder

#define TOOLBAR_GSCHEM_RADIO( bar, var, grp, name, data) \
    TOOLBAR_GSCHEM_RADIO_STK( bar, var, grp, name, TB_ICON_NAME(name), ToolBar_Radio_Responder, data)
#else
#define TOOLBAR_GSCHEM_RADIO( bar, var, grp, name, func, data) \
    TOOLBAR_GSCHEM_RADIO_STK( bar, var, grp, name, TB_ICON_NAME(name), func, data)
#endif

#define TOOLBAR_GSCHEM_RADIO_STK( bar, var, grp, name, icon, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_stock(icon, GTK_ICON_SIZE_SMALL_TOOLBAR); \
   TOOLBAR_GSCHEM_RADIO_ELEMENT ( bar##_Toolbar, name##_ICN, var, grp, name, func, data)

#define TOOLBAR_GSCHEM_RADIO_ELEMENT( bar, icon, var, grp, name, func, data) \
   var = gtk_toolbar_append_element(GTK_TOOLBAR(bar), \
                                    GTK_TOOLBAR_CHILD_RADIOBUTTON, \
                                    grp, \
                                   _(TB_LABEL (name)), \
                                   _(TB_TOOLTIP (name)), \
                                     TB_WIDGET(name), \
                                     GTK_WIDGET(icon), \
                                    (GtkSignalFunc) func, \
                                     data); \
   g_object_set_data ((GObject*) var, "action", (void*)TB_WIDGET(name)); \
   { \
     AtkObject *atk_obj = gtk_widget_get_accessible(var); \
     char *str = g_strconcat("Geda-toolbar-button-", TB_WIDGET(name), "-mode", NULL); \
     gtk_widget_set_name (var, str); \
           str = g_strdelimit(str, "-", ' ' ); \
     atk_object_set_name (atk_obj, _(str)); \
     atk_object_set_description(atk_obj,_(TB_TOOLTIP (name))); \
     g_free(str); \
   }
#endif

