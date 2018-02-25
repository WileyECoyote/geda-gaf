/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_tearoff_menu_item.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016-2018 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 *
 * Date: March 31, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */

#ifndef __GEDA_TEAROFF_MENU_ITEM_H__
#define __GEDA_TEAROFF_MENU_ITEM_H__

#include "geda_menu_item.h"

#define GEDA_TYPE_TEAROFF_MENU_ITEM            (geda_tearoff_menu_item_get_type ())
#define GEDA_TEAROFF_MENU_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_TEAROFF_MENU_ITEM, GedaTearoffMenuItem))
#define GEDA_TEAROFF_MENU_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_TEAROFF_MENU_ITEM, GedaTearoffMenuItemClass))
#define GEDA_IS_TEAROFF_MENU_ITEM(obj)         (is_a_geda_tearoff_menu_item((GedaTearoffMenuItem*)(obj)))
#define GEDA_IS_TEAROFF_MENU_ITEM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_TEAROFF_MENU_ITEM))
#define GEDA_TEAROFF_MENU_ITEM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_TEAROFF_MENU_ITEM, GedaTearoffMenuItemClass))

typedef struct _GedaTearoffMenuItem      GedaTearoffMenuItem;
typedef struct _GedaTearoffMenuItemData  GedaTearoffMenuItemData;
typedef struct _GedaTearoffMenuItemClass GedaTearoffMenuItemClass;

struct _GedaTearoffMenuItem
{
  GedaMenuItem menu_item;

  /*< private >*/
  GedaTearoffMenuItemData *priv;
};

struct _GedaTearoffMenuItemClass
{
  GedaMenuItemClass parent_class;

};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_tearoff_menu_item_get_type (void) GEDA_CONST;
GtkWidget *geda_tearoff_menu_item_new      (void);
bool       is_a_geda_tearoff_menu_item     (GedaTearoffMenuItem  *tearoff_menu_item);

bool       geda_tearoff_menu_is_active     (GtkWidget *menu_item);
bool       geda_tearoff_menu_is_torn       (GtkWidget *menu_item);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_TEAROFF_MENU_ITEM_H__ */
