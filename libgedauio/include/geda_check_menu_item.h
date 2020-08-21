/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_check_menu_item.h
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
/*! \class GedaCheckMenuItem geda_check_menu_item.h "libgedauio/geda_check_menu_item.h"
 *  \brief Class for GedaCheckMenuItem Objects.
 *
 *  GedaCheckMenuItem is a derivative of the GedaMenuItem class
 *  specialized for representation and manipulation of togglable
 *  check menu item widgets.
 */

#ifndef __GEDA_CHECK_MENU_ITEM_H__
#define __GEDA_CHECK_MENU_ITEM_H__

#include "geda_menu_item.h"

#define GEDA_TYPE_CHECK_MENU_ITEM            (geda_check_menu_item_get_type ())
#define GEDA_CHECK_MENU_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_CHECK_MENU_ITEM, GedaCheckMenuItem))
#define GEDA_CHECK_MENU_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_CHECK_MENU_ITEM, GedaCheckMenuItemClass))
#define GEDA_IS_CHECK_MENU_ITEM(obj)         (is_a_geda_check_menu_item((GedaCheckMenuItem*)(obj)))
#define GEDA_IS_CHECK_MENU_ITEM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_CHECK_MENU_ITEM))
#define GEDA_CHECK_MENU_ITEM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_CHECK_MENU_ITEM, GedaCheckMenuItemClass))


typedef struct _GedaCheckMenuItem       GedaCheckMenuItem;
typedef struct _GedaCheckMenuItemClass  GedaCheckMenuItemClass;

struct _GedaCheckMenuItem
{
  GedaMenuItem menu_item;

  unsigned int active : 1;
  unsigned int always_show_toggle : 1;
  unsigned int inconsistent : 1;
  unsigned int draw_as_radio : 1;
};

struct _GedaCheckMenuItemClass
{
  GedaMenuItemClass parent_class;

  void (* toggled)        (GedaCheckMenuItem *check_menu_item);
  void (* draw_indicator) (GedaCheckMenuItem *check_menu_item,
                           GdkRectangle      *area);

};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_check_menu_item_get_type	  (void) GEDA_CONST;
bool       is_a_geda_check_menu_item              (GedaCheckMenuItem *check_menu_item);

GtkWidget *geda_check_menu_item_new               (void);
GtkWidget *geda_check_menu_item_new_with_label    (const char        *label);
GtkWidget *geda_check_menu_item_new_with_mnemonic (const char        *label);

bool       geda_check_menu_item_get_active        (GedaCheckMenuItem *check_menu_item);
void       geda_check_menu_item_set_active        (GedaCheckMenuItem *check_menu_item,
                                                   bool               is_active);

bool       geda_check_menu_item_get_draw_as_radio (GedaCheckMenuItem *check_menu_item);
void       geda_check_menu_item_set_draw_as_radio (GedaCheckMenuItem *check_menu_item,
                                                   bool               draw_as_radio);

bool       geda_check_menu_item_get_inconsistent  (GedaCheckMenuItem *check_menu_item);
void       geda_check_menu_item_set_inconsistent  (GedaCheckMenuItem *check_menu_item,
                                                   bool               setting);
bool       geda_check_menu_item_get_show_toggle   (GedaCheckMenuItem *menu_item);
void       geda_check_menu_item_set_show_toggle   (GedaCheckMenuItem *menu_item,
                                                   bool               always);

void       geda_check_menu_item_toggled           (GedaCheckMenuItem *check_menu_item);

#define    geda_check_menu_item_set_as_radio(i,r) geda_check_menu_item_set_draw_as_radio ((GedaCheckMenuItem*)(i),(r))
#define    geda_check_menu_item_set_state(i,s) geda_check_menu_item_set_active ((GedaCheckMenuItem*)(i),(s))

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_CHECK_MENU_ITEM_H__ */
