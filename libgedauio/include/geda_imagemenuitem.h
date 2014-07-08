/* GTK - The GIMP Toolkit
 * Copyright (C) Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 */

#ifndef __GEDA_IMAGE_MENU_ITEM_H__
#define __GEDA_IMAGE_MENU_ITEM_H__

#include <gtk/gtkmenuitem.h>

G_BEGIN_DECLS

#define GEDA_TYPE_IMAGE_MENU_ITEM            (geda_image_menu_item_get_type ())
#define GEDA_IMAGE_MENU_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_IMAGE_MENU_ITEM, GedaImageMenuItem))
#define GEDA_IMAGE_MENU_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_IMAGE_MENU_ITEM, GedaImageMenuItemClass))
#define GEDA_IS_IMAGE_MENU_ITEM(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEDA_TYPE_IMAGE_MENU_ITEM))
#define GEDA_IS_IMAGE_MENU_ITEM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_IMAGE_MENU_ITEM))
#define GEDA_IMAGE_MENU_ITEM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_IMAGE_MENU_ITEM, GedaImageMenuItemClass))


typedef struct _GedaImageMenuItem       GedaImageMenuItem;
typedef struct _GedaImageMenuItemClass  GedaImageMenuItemClass;

struct _GedaImageMenuItem
{
  GtkMenuItem   menu_item;
  GtkWidget    *image;

  char         *label;
  unsigned int  use_stock  : 1;
  unsigned int  show_image : 1;

};

struct _GedaImageMenuItemClass
{
  GtkMenuItemClass parent_class;
};

GType	   geda_image_menu_item_get_type              (void) G_GNUC_CONST;
GtkWidget* geda_image_menu_item_new                   (void);
GtkWidget* geda_image_menu_item_new_with_label        (const char        *label);
GtkWidget* geda_image_menu_item_new_with_mnemonic     (const char        *label);
GtkWidget* geda_image_menu_item_new_from_stock        (const char        *stock_id,
                                                       GtkAccelGroup     *accel_group);
void       geda_image_menu_item_set_show_image        (GedaImageMenuItem *image_menu_item,
                                                       bool               always_show);
bool       geda_image_menu_item_get_show_image        (GedaImageMenuItem *image_menu_item);
void       geda_image_menu_item_set_image             (GedaImageMenuItem *image_menu_item,
                                                       GtkWidget         *image);
GtkWidget* geda_image_menu_item_get_image             (GedaImageMenuItem *image_menu_item);
void       geda_image_menu_item_set_use_stock         (GedaImageMenuItem *image_menu_item,
						       bool               use_stock);
bool       geda_image_menu_item_get_use_stock         (GedaImageMenuItem *image_menu_item);
void       geda_image_menu_item_set_accel_group       (GedaImageMenuItem *image_menu_item, 
						       GtkAccelGroup     *accel_group);

G_END_DECLS

#endif /* __GEDA_IMAGE_MENU_ITEM_H__ */
