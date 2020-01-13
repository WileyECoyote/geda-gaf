/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menu_item.h
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
/*! \class GedaMenuItem geda_menu_item.h "libgedauio/geda_menu_item.h"
 *  \brief Class for GedaMenuItem Objects.
 *
 *  GedaMenuItem is a derivative of the GtkBin class.
 */

#ifndef __GEDA_MENU_ITEM_H__
#define __GEDA_MENU_ITEM_H__

#if (GTK_MAJOR_VERSION < 3) && !defined GTK_DISABLE_SINGLE_INCLUDES

#include <gtk/gtkbin.h>

#else

#include <gtk/gtk.h>

#endif

#include "geda_menu.h"
#include "geda_menu_enum.h"

#define MENU_POPUP_TIME_KEY "menu-exact-popup-time"

#define GEDA_TYPE_MENU_ITEM            (geda_menu_item_get_type ())
#define GEDA_MENU_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_MENU_ITEM, GedaMenuItem))
#define GEDA_MENU_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GEDA_TYPE_MENU_ITEM, GedaMenuItemClass))
#define GEDA_IS_MENU_ITEM(obj)         (is_a_geda_menu_item((GedaMenuItem*)(obj)))
#define GEDA_IS_MENU_ITEM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GEDA_TYPE_MENU_ITEM))
#define GEDA_MENU_ITEM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GEDA_TYPE_MENU_ITEM, GedaMenuItemClass))

typedef struct _GedaMenuItem        GedaMenuItem;
typedef struct _GedaMenuItemClass   GedaMenuItemClass;
typedef struct _GedaMenuItemPrivate GedaMenuItemPrivate;

struct _GedaMenuItem
{
  GtkBin bin;

  /*< private >*/
  GedaMenuItemPrivate *priv;
};

/*!
 * \class GedaMenuItemClass include/geda_menu_item.h "geda_menu_item.h"
 * \brief GedaMenuItem for Menu Objects.
 * parent_class: The parent class.
 * hide_on_activate: If %TRUE, then we should always hide the menu when
 *                   the %GedaMenuItem is activated. Otherwise, it is up
 *                   to the caller.
 * activate: Signal emitted when the item is activated.
 * activate_item: Signal emitted when the item is activated, but also
 *                if the menu item has a submenu.
 * toggle_size_request:
 * toggle_size_allocate:
 * set_label: Sets text on the #GedaMenuItem label
 * get_label: Gets text from the #GedaMenuItem label
 * select: Signal emitted when the item is selected.
 * deselect: Signal emitted when the item is deselected.
 */
struct _GedaMenuItemClass
{
  GtkBinClass parent_class;

  /*< public >*/

  /* If the following flag is true, then we should always
   * hide the menu when the MenuItem is activated. Otherwise,
   * it is up to the caller. For instance, when navigating
   * a menu with the keyboard, <Space> doesn't hide, but
   * <Return> does.
   */
  unsigned int hide_on_activate : 1;

  void (* activate)             (GedaMenuItem *menu_item);
  void (* activate_item)        (GedaMenuItem *menu_item);
  void (* toggle_size_request)  (GedaMenuItem *menu_item,
                                 int          *requisition);
  void (* toggle_size_allocate) (GedaMenuItem *menu_item,
                                 int           allocation);
  void (* set_label)            (GedaMenuItem *menu_item,
                                 const char   *label);
  const char * (* get_label)    (GedaMenuItem *menu_item);

  void (* select)               (GedaMenuItem *menu_item);
  void (* deselect)             (GedaMenuItem *menu_item);

  /*< private >*/
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType    geda_menu_item_get_type              (void)          GEDA_CONST;
bool        is_a_geda_menu_item                  (GedaMenuItem  *menu_item);

bool        geda_menu_item_is_selectable         (GedaMenuItem  *menu_item);
bool        geda_menu_item_is_widget_selectable  (GtkWidget     *widget);

GtkWidget  *geda_menu_item_new                   (void);

GtkWidget  *geda_menu_item_new_with_label        (const char    *label);

GtkWidget  *geda_menu_item_new_with_mnemonic     (const char    *label);

GdkWindow  *geda_menu_item_get_event_window      (GedaMenuItem  *menu_item);

void        geda_menu_item_set_submenu           (GedaMenuItem  *menu_item,
                                                  GedaMenu      *submenu);

void        geda_menu_item_set_submenu_widget    (GedaMenuItem  *menu_item,
                                                  GtkWidget     *submenu);

GtkWidget  *geda_menu_item_get_submenu_widget    (GedaMenuItem  *menu_item);

void        geda_menu_item_select                (GedaMenuItem  *menu_item);

void        geda_menu_item_deselect              (GedaMenuItem  *menu_item);

void        geda_menu_item_activate              (GedaMenuItem  *menu_item);

void        geda_menu_item_activate_item         (GedaMenuItem  *menu_item);

void        geda_menu_item_toggle_size_request   (GedaMenuItem  *menu_item,
                                                  int           *requisition);

void        geda_menu_item_toggle_size_allocate  (GedaMenuItem  *menu_item,
                                                  int            allocation);

void        geda_menu_item_set_accel_path        (GedaMenuItem  *menu_item,
                                                  const char    *accel_path);

const char *geda_menu_item_get_accel_path        (GedaMenuItem  *menu_item);

unsigned short
            geda_menu_item_get_accel_width       (GedaMenuItem  *menu_item);

char        geda_menu_item_get_mnemonic          (GedaMenuItem  *menu_item);

void        geda_menu_item_set_mnemonic          (GedaMenuItem  *menu_item,
                                                  char           mnemonic);

void        geda_menu_item_set_right_justified   (GedaMenuItem  *menu_item,
                                                  bool           right_justified);

bool        geda_menu_item_get_right_justified   (GedaMenuItem  *menu_item);

unsigned short
            geda_menu_item_get_toggle_size       (GedaMenuItem  *menu_item);

bool        geda_menu_item_get_from_menubar      (GedaMenuItem  *menu_item);

void        geda_menu_item_set_label             (GedaMenuItem  *menu_item,
                                                  const char    *label);

const char   *geda_menu_item_get_label                (GedaMenuItem     *menu_item);

GtkWidget    *geda_menu_item_get_label_widget         (GedaMenuItem     *menu_item);


unsigned int  geda_menu_item_get_submenu_direction    (GedaMenuItem     *menu_item);

SubmenuPlacement
              geda_menu_item_get_submenu_placement    (GedaMenuItem     *menu_item);

void          geda_menu_item_set_submenu_placement    (GedaMenuItem     *menu_item,
                                                       SubmenuPlacement  placement);

void        geda_menu_item_set_use_underline          (GedaMenuItem     *menu_item,
                                                       bool              setting);

bool        geda_menu_item_get_use_underline          (GedaMenuItem     *menu_item);

void        geda_menu_item_set_related_action         (GedaMenuItem     *menu_item,
                                                       GtkAction        *action);

void        geda_menu_item_set_reserve_indicator      (GedaMenuItem     *menu_item,
                                                       bool              reserve);

bool        geda_menu_item_get_reserve_indicator      (GedaMenuItem     *menu_item);



bool        geda_menu_item_get_show_submenu_indicator (GedaMenuItem     *menu_item);

void        geda_menu_item_set_show_submenu_indicator (GedaMenuItem     *menu_item,
                                                       bool              show);

void        geda_menu_item_refresh_accel_path         (GedaMenuItem     *menu_item,
                                                       const char       *prefix,
                                                       GtkAccelGroup    *accel_group,
                                                       int               group_changed);

void        geda_menu_item_popup_submenu              (GedaMenuItem     *menu_item,
                                                       bool              with_delay);
void        geda_menu_item_popdown_submenu            (GedaMenuItem     *menu_item);

void        geda_menu_item_set_use_action_appearance  (GedaMenuItem     *menu_item,
                                                       bool              use_appearance);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MENU_ITEM_H__ */
