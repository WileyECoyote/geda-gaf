/* -*- toolbars-h -*-
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2013 Wiley Edward Hill
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*!
 * \file
 * \brief Global variable declarations
 *
 * \section
 *
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

#define TB_WIDGET(member)  ToolbarStrings[member].Widget
#define TB_LABEL(member)   ToolbarStrings[member].Label
#define TB_TOOLTIP(member) ToolbarStrings[member].Tip
#define TB_PRIVATE(member) ToolbarStrings[member].Private
#define TB_ICON_NAME(member) ToolbarStrings[member].Private

#define GET_TOOLBAR_ID(obj) (int)(long*) g_object_get_data(G_OBJECT(obj), "BarId");
#define SET_TOOLBAR_ID(obj, bar_id) g_object_set_data(G_OBJECT(obj), "BarId", GINT_TO_POINTER(bar_id));

/*------------------------------------------------------------------
 * GTK Toolbar includes: stuff for dealing with Toolbars.
 *------------------------------------------------------------------*/

/* ------------------------- Entry Level -------------------------------- */
/*! \brief ToolBar Macros with GSCHEM_TOPLEVEL *w_current call-back data */
#define TOOLBAR_GEDA_BUTTON( bar, name, type, icon, func, data) \
   GtkWidget *name##_button; \
   TOOLBAR_BUTTON_DATA( bar##_Toolbar, name, type, icon, name##_button, func, data)

/*! \brief ToolBar Macros with Enumerated Control call-back data */
#define TOOLBAR_STD_BUTTON( bar, name, type, icon, func) \
   GtkWidget *name##_button; \
   TOOLBAR_BUTTON( bar##_Toolbar, name, type, icon, name##_button, func, name)

/*! \brief ToolBar Macros with Enumerated Control and Assumed call-back func */
#define TOOLBAR_STD_FUNC( bar, name, type, icon) \
   GtkWidget *name##_button; \
   TOOLBAR_BUTTON( bar##_Toolbar, name, type, icon, name##_button, callBack_##bar##Bar, name)

/* --------------------------- 1st Level -------------------------------- */
/*! \brief 1st LeveL Intermediate ToolBar Macros with Enumerated data */
#define TOOLBAR_BUTTON( bar, name, type, icon, button, func, data) { \
        TOOLBAR_BUTTON_##type (bar, name, ENUM, icon, button, func, data) \
}

/*! \brief 1st LeveL Intermediate ToolBar Macros with *w_current data */
#define TOOLBAR_BUTTON_DATA( bar, name, type, icon, button, func, data) { \
        TOOLBAR_BUTTON_##type (bar, name, GEDA, icon, button, func, data) \
}

/* --------------------------- 2nd Level -------------------------------- */
/*! \brief 2nd Level Intermediate ToolBar Macros with Pixmap Icon */
#define TOOLBAR_BUTTON_PIX(bar, name, next, icon, button, func, data) \
   tmp_toolbar_icon = create_pixmap (icon); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, tmp_toolbar_icon, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data)

/*! \brief 2nd Level Intermediate ToolBar Macros with Stock Icon */
#define TOOLBAR_BUTTON_STK(bar, name, next, icon, button, func, data) \
   tmp_toolbar_icon = gtk_image_new_from_stock(STOCK_MAP(icon), GTK_ICON_SIZE_SMALL_TOOLBAR); \
   next##_TOOLBAR_BUTTON(bar, tmp_toolbar_icon, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data)

/*! \brief 2nd Level Intermediate ToolBar Macros with Local Pixmap Icon */
#define TOOLBAR_BUTTON_LOCAL_PIX(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = create_pixmap (icon); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data)

#define TOOLBAR_BUTTON_LOCAL_STR(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = create_pixmap (TB_ICON_NAME(name)); /* from file */ \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data)

/*! \brief 2nd Level Intermediate ToolBar Macros with Local Stock Icon */
#define TOOLBAR_BUTTON_LOCAL_STK(bar, name, next, icon, button, func, data) \
   GtkWidget *name##_ICN = gtk_image_new_from_stock(STOCK_MAP(icon), GTK_ICON_SIZE_SMALL_TOOLBAR); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_PRIVATE (name)), func, data)

/*! \brief 2nd Level Intermediate ToolBar Macros with Item Data Struct */
#define TOOLBAR_BUTTON_LOCAL_ALT(bar, name, next, icon, button, func, data) \
   ToolbarItem name##_ToolbarItem; /* Allocate a Data Structure */ \
   name##_ToolbarItem.stock_id =  STOCK_MAP(icon); \
   name##_ToolbarItem.ButtonId =  name; \
   GtkWidget *name##_ICN = get_stock_alt_pixmap (&name##_ToolbarItem); \
   next##_TOOLBAR_BUTTON(bar, name##_ICN, button, _(TB_LABEL (name)), _(TB_TOOLTIP (name)), _(TB_TOOLTIP (name)), func, data)

/* --------------------------- 3rd Level -------------------------------- */
#define ENUM_TOOLBAR_BUTTON(bar, icon, button, txt, tip, priv, func, data) \
   button = gtk_toolbar_append_item( GTK_TOOLBAR(bar), txt, \
                                     tip, priv, \
                                     GTK_WIDGET(icon), /* GtkWidget */ \
                                     GTK_SIGNAL_FUNC(func), \
                                     GUINT_TO_POINTER (data)); /* ptr to IDS_xxxx_Toolbar enumerator */

#define GEDA_TOOLBAR_BUTTON(bar, icon, button, txt, tip, priv, func, data) \
   button = gtk_toolbar_append_item( GTK_TOOLBAR(bar), txt, \
                                     tip, priv, \
                                     GTK_WIDGET(icon), /* GtkWidget */ \
                                     GTK_SIGNAL_FUNC(func), \
                                     data); /* MUST be PTR to something */
#endif
