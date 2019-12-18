/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_image_menu_item.h
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
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_IMAGE_MENU_ITEM_H__
#define __GEDA_IMAGE_MENU_ITEM_H__

#include "geda_menu_enum.h"
#include "geda_menu_item.h"

#define GEDA_TYPE_IMAGE_MENU_ITEM            (geda_image_menu_item_get_type ())
#define GEDA_IMAGE_MENU_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_IMAGE_MENU_ITEM, GedaImageMenuItem))
#define GEDA_IMAGE_MENU_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_IMAGE_MENU_ITEM, GedaImageMenuItemClass))
#define GEDA_IS_IMAGE_MENU_ITEM(obj)         (is_a_geda_image_menu_item((GedaImageMenuItem*)(obj)))
#define GEDA_IS_IMAGE_MENU_ITEM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_IMAGE_MENU_ITEM))
#define GEDA_IMAGE_MENU_ITEM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_IMAGE_MENU_ITEM, GedaImageMenuItemClass))

typedef struct _GedaImageMenuItem      GedaImageMenuItem;
typedef struct _GedaImageMenuItemClass GedaImageMenuItemClass;
typedef struct _GedaImageMenuItemData  GedaImageMenuItemData;

struct _GedaImageMenuItem
{
  GedaMenuItem  menu_item;

  GtkWidget    *image;

  char         *label;
  unsigned int  use_stock  : 1;
  unsigned int  show_image : 1;

  GedaImageMenuItemData *priv;
};

struct _GedaImageMenuItemClass
{
  GedaMenuItemClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_image_menu_item_get_type              (void) GEDA_CONST;
bool       is_a_geda_image_menu_item                  (GedaImageMenuItem *image_menu_item);

GtkWidget *geda_image_menu_item_new                   (void);
GtkWidget *geda_image_menu_item_new_with_label        (const char        *label);
GtkWidget *geda_image_menu_item_new_with_mnemonic     (const char        *label);
GtkWidget *geda_image_menu_item_new_from_stock        (const char        *stock_id,
                                                       GtkAccelGroup     *accel_group);
GtkAccelGroup
          *geda_image_menu_item_get_accel_group       (GedaImageMenuItem *image_menu_item);
void       geda_image_menu_item_set_accel_group       (GedaImageMenuItem *image_menu_item,
                                                       GtkAccelGroup     *accel_group);
GtkWidget *geda_image_menu_item_get_image             (GedaImageMenuItem *image_menu_item);
void       geda_image_menu_item_set_image             (GedaImageMenuItem *image_menu_item,
                                                       GtkWidget         *image);
bool       geda_image_menu_item_get_show_image        (GedaImageMenuItem *image_menu_item);
void       geda_image_menu_item_set_show_image        (GedaImageMenuItem *image_menu_item,
                                                       bool               always_show);
bool       geda_image_menu_item_get_use_stock         (GedaImageMenuItem *image_menu_item);
void       geda_image_menu_item_set_use_stock         (GedaImageMenuItem *image_menu_item,
                                                       bool               use_stock);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_IMAGE_MENU_ITEM_H__ */
