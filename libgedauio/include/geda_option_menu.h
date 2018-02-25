/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_option_menu.h
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
 * Date: June 11, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_OPTION_MENU_H__
#define __GEDA_OPTION_MENU_H__

#define GEDA_TYPE_OPTION_MENU            (geda_option_menu_get_type ())
#define GEDA_OPTION_MENU(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_OPTION_MENU, GedaOptionMenu))
#define GEDA_OPTION_MENU_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_OPTION_MENU, GedaOptionMenuClass))
#define GEDA_IS_OPTION_MENU(obj)         (is_a_geda_option_menu((GedaOptionMenu*)(obj)))
#define GEDA_IS_OPTION_MENU_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_OPTION_MENU))
#define GEDA_OPTION_MENU_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_OPTION_MENU, GedaOptionMenuClass))

typedef struct _GedaOptionMenu       GedaOptionMenu;
typedef struct _GedaOptionMenuClass  GedaOptionMenuClass;

struct _GedaOptionMenu
{
  GtkButton button;

  GtkWidget *menu;
  GtkWidget *menu_item;

  uint16 width;
  uint16 height;
};

struct _GedaOptionMenuClass
{
  GtkButtonClass parent_class;

  void (*changed) (GedaOptionMenu *option_menu);
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType   geda_option_menu_get_type      (void) GEDA_CONST;
bool       is_a_geda_option_menu          (GedaOptionMenu *option_menu);

GtkWidget *geda_option_menu_new           (void);
GtkWidget *geda_option_menu_get_menu      (GedaOptionMenu *option_menu);
void       geda_option_menu_set_menu      (GedaOptionMenu *option_menu,
                                           GtkWidget      *menu);
void       geda_option_menu_remove_menu   (GedaOptionMenu *option_menu);
int        geda_option_menu_get_history   (GedaOptionMenu *option_menu);
void       geda_option_menu_set_history   (GedaOptionMenu *option_menu,
                                           unsigned int    index);

GtkWidget *geda_option_widget_get_menu    (GtkWidget      *option_menu);
void       geda_option_widget_set_menu    (GtkWidget      *option_menu,
                                           GtkWidget      *menu);
void       geda_option_widget_remove_menu (GtkWidget      *option_menu);
int        geda_option_widget_get_history (GtkWidget      *option_menu);
void       geda_option_widget_set_history (GtkWidget      *option_menu,
                                           unsigned int    index);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_OPTION_MENU_H__ */
