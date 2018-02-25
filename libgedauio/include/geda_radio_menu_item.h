/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_radio_menu_item.h
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
 * Date: June 16, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
/*! \class GedaRadioMenuItem geda_radio_menu_item.h "libgedauio/geda_radio_menu_item.h"
 *  \brief Class for GedaRadioMenuItem Objects.
 *
 *  GedaRadioMenuItem is a derivative of the GedaCheckMenuItem class.
 */

#ifndef __GEDA_RADIO_MENU_ITEM_H__
#define __GEDA_RADIO_MENU_ITEM_H__

#include "geda_check_menu_item.h"

#define GEDA_TYPE_RADIO_MENU_ITEM             (geda_radio_menu_item_get_type ())
#define GEDA_RADIO_MENU_ITEM(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_RADIO_MENU_ITEM, GedaRadioMenuItem))
#define GEDA_RADIO_MENU_ITEM_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_RADIO_MENU_ITEM, GedaRadioMenuItemClass))
#define GEDA_IS_RADIO_MENU_ITEM(obj)          (is_a_geda_radio_menu_item((GedaRadioMenuItem*)(obj)))
#define GEDA_IS_RADIO_MENU_ITEM_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_RADIO_MENU_ITEM))
#define GEDA_RADIO_MENU_ITEM_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_RADIO_MENU_ITEM, GedaRadioMenuItemClass))


typedef struct _GedaRadioMenuItem       GedaRadioMenuItem;
typedef struct _GedaRadioMenuItemClass  GedaRadioMenuItemClass;

struct _GedaRadioMenuItem
{
  GedaCheckMenuItem check_menu_item;

  GSList           *group;
};

struct _GedaRadioMenuItemClass
{
  GedaCheckMenuItemClass parent_class;

  /* Signals */
  void (*group_changed) (GedaRadioMenuItem *radio_menu_item);

};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_radio_menu_item_get_type                      (void) GEDA_CONST;
bool       is_a_geda_radio_menu_item                          (GedaRadioMenuItem  *radio_menu_item);

GtkWidget *geda_radio_menu_item_new                           (GSList             *group);
GtkWidget *geda_radio_menu_item_new_from_widget               (GtkWidget          *group);
GtkWidget *geda_radio_menu_item_new_with_label                (GSList             *group,
                                                               const char         *label);
GtkWidget *geda_radio_menu_item_new_with_label_from_widget    (GtkWidget          *group,
                                                               const char         *label);
GtkWidget *geda_radio_menu_item_new_with_mnemonic             (GSList             *group,
                                                               const char         *label);
GtkWidget *geda_radio_menu_item_new_with_mnemonic_from_widget (GtkWidget          *group,
                                                               const char         *label);

GSList*    geda_radio_menu_item_get_group                     (GedaRadioMenuItem  *radio_menu_item);
void       geda_radio_menu_item_set_group                     (GedaRadioMenuItem  *radio_menu_item,
                                                               GSList             *group);

#define geda_radio_menu_item_group(w) geda_radio_menu_item_get_group((GedaRadioMenuItem*)w)

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_RADIO_MENU_ITEM_H__ */
