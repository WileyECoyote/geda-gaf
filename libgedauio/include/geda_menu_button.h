/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menu_button.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2018 Wiley Edward Hill
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
 * Date: March 31, 2013
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */
/*! \class GedaMenuButton geda_menu_button.h "libgedauio/geda_menu_button.h"
 *  \brief Class for GedaMenuButton Objects.
 *
 *  GedaMenuButton is a derivative of the GtkEventBox class.
 */

#ifndef __GEDA_MENU_BUTTON_H__
#define __GEDA_MENU_BUTTON_H__

#include <gtk/gtk.h>

#define GEDA_TYPE_MENU_BUTTON             (geda_menu_button_get_type ())
#define GEDA_MENU_BUTTON(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_MENU_BUTTON, GedaMenuButton))
#define GEDA_MENU_BUTTON_CONST(obj)       (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_MENU_BUTTON, GedaMenuButton const))
#define GEDA_MENU_BUTTON_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_MENU_BUTTON, GedaMenuButtonClass))
#define GEDA_IS_MENU_BUTTON(obj)          (is_a_geda_menu_button((GedaMenuButton*)(obj)))
#define GEDA_IS_MENU_BUTTON_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_MENU_BUTTON))
#define GEDA_MENU_BUTTON_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_MENU_BUTTON, GedaMenuButtonClass))

typedef struct _GedaMenuButton      GedaMenuButton;
typedef struct _GedaMenuButtonClass GedaMenuButtonClass;
typedef struct _GedaMenuButtonData  GedaMenuButtonData;

struct _GedaMenuButton
{
  GtkEventBox  parent;

  GdkWindow   *event_window;

  char        *label_text;

  unsigned int activate_timeout;;
  unsigned int constructed;
  unsigned int in_button;
  unsigned int button_down;
  unsigned int relief;
  unsigned int menu_relief;
  unsigned int use_underline;
  unsigned int use_stock;
  unsigned int depressed;
  unsigned int depress_on_activate;
  unsigned int focus_on_click;

  GedaMenuButtonData *priv;
};

struct _GedaMenuButtonClass
{
  GtkEventBoxClass parent_class;

  void (* pressed)   (GedaMenuButton *button);
  void (* released)  (GedaMenuButton *button);
  void (* clicked)   (GedaMenuButton *button);
  void (* enter)     (GedaMenuButton *button);
  void (* leave)     (GedaMenuButton *button);
  void (* activate)  (GedaMenuButton *button);
  void (* show_menu) (GedaMenuButton *button);
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType        geda_menu_button_get_type             (void) GEDA_CONST;
bool            is_a_geda_menu_button                 (GedaMenuButton *menu_button);

GtkWidget      *geda_menu_button_new                  (GtkWidget      *icon_widget,
                                                       const char     *label);
GtkWidget      *geda_menu_button_new_from_icon_name   (const char     *icon_name);
GtkWidget      *geda_menu_button_new_from_stock       (const char     *stock_id);
GtkWidget      *geda_menu_button_new_with_mnemonic    (const char     *label);

void            geda_menu_button_set_style            (GedaMenuButton *button,
                                                       GtkStyle       *new_style);
void            geda_menu_button_set_relief           (GedaMenuButton *button,
                                                       GtkReliefStyle  newstyle);
GtkReliefStyle  geda_menu_button_get_relief           (GedaMenuButton *button);
void            geda_menu_arrow_set_relief            (GedaMenuButton *button,
                                                       GtkReliefStyle  newstyle);
GtkReliefStyle  geda_menu_arrow_get_relief            (GedaMenuButton *button);

void            geda_menu_button_set_label            (GedaMenuButton *button,
                                                       const char     *label);
const char*     geda_menu_button_get_label            (GedaMenuButton *button);

void            geda_menu_button_set_use_underline     (GedaMenuButton *button,
                                                       bool      use_underline);
bool            geda_menu_button_get_use_underline     (GedaMenuButton *button);

void            geda_menu_button_set_use_stock         (GedaMenuButton *button,
                                                       bool      use_stock);
bool            geda_menu_button_get_use_stock         (GedaMenuButton *button);

void            geda_menu_button_set_focus_on_click    (GedaMenuButton *button,
                                                       bool      focus_on_click);
bool            geda_menu_button_get_focus_on_click    (GedaMenuButton *button);
void            geda_menu_button_set_alignment         (GedaMenuButton *button,
                                                       float     xalign,
                                                       float     yalign);
void            geda_menu_button_get_alignment         (GedaMenuButton *button,
                                                       float    *xalign,
                                                       float    *yalign);
void            geda_menu_button_set_image             (GedaMenuButton *button,
                                                        GtkWidget      *image);
GtkWidget*      geda_menu_button_get_image             (GedaMenuButton *button);
void            geda_menu_button_set_image_position    (GedaMenuButton *button,
                                                        GtkPositionType position);
GtkPositionType geda_menu_button_get_image_position    (GedaMenuButton *button);

GdkWindow*      geda_menu_button_get_event_window      (GedaMenuButton *button);

void            geda_menu_button_set_menu              (GedaMenuButton *button,
                                                        GtkWidget      *menu);
GtkWidget      *geda_menu_button_get_menu              (GedaMenuButton *button);

void            geda_menu_button_set_tooltip_text      (GedaMenuButton *button,
                                                        const char     *tip_text);
void            geda_menu_button_set_arrow_tooltip     (GedaMenuButton *button,
                                                        GtkTooltips    *tooltips,
                                                        const char     *tip_text,
                                                        const char     *tip_private);

void            geda_menu_button_set_arrow_tooltip_text   (GedaMenuButton *button,
                                                           const char     *text);
void            geda_menu_button_set_arrow_tooltip_markup (GedaMenuButton *button,
                                                           const char     *markup);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_MENU_BUTTON_H__ */
/* ex:set ts=8 noet: */