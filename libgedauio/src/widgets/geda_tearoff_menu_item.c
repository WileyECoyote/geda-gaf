/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_tearoff_menu_item.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: March 31, 2016
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_menu_enum.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_tearoff_menu_item.h"

/**
 * \brief GedaTearoffMenuItem - A Widget for Menus
 * \par
 * A menu widget which can torn from the menu.
 *
 * \defgroup GedaTearoffMenuItem Geda Tearoff Menu Item
 * @{
 * A #GedaTearoffMenuItem is a special menu item which is used to
 * tear off and reattach the menu.
 *
 * When the menu is shown normally, the #GedaTearoffMenuItem is drawn as a
 * dotted line indicating that the menu can be torn off. Activating the line
 * causes the menu to be torn off and displayed in seperte window as a tear
 * off menu.
 *
 * When the menu is shown as a tearoff menu, the #GedaTearoffMenuItem is drawn
 * as a dotted line which has a left pointing arrow graphic indicating that
 * the tearoff menu can be reattached.  Activating it will erase the tearoff
 * menu window.
 *
 * \sa #GedaMenu
 */

#define ARROW_SIZE 10
#define TEAR_LENGTH 5
#define BORDER_SPACING  3

enum {
  TORN_OFF,
  LAST_SIGNAL
};

struct _GedaTearoffMenuItemData
{
  unsigned int torn_off : 1;
};

#if GTK_MAJOR_VERSION < 3

static void geda_tearoff_menu_item_size_request         (GtkWidget      *widget,
                                                         GtkRequisition *requisition);
static int  geda_tearoff_menu_item_expose               (GtkWidget      *widget,
                                                         GdkEventExpose *event);
#else

static void geda_tearoff_menu_item_get_preferred_width  (GtkWidget      *widget,
                                                         int            *minimum,
                                                         int            *natural);
static void geda_tearoff_menu_item_get_preferred_height (GtkWidget      *widget,
                                                         int            *minimum,
                                                         int            *natural);
static bool geda_tearoff_menu_item_draw                 (GtkWidget      *widget,
                                                         cairo_t        *cr);

#endif

static void geda_tearoff_menu_item_activate             (GedaMenuItem   *menu_item);
static void geda_tearoff_menu_item_parent_set           (GtkWidget      *widget,
                                                         GtkWidget      *previous);

static unsigned int  geda_tearoff_signals[ LAST_SIGNAL ] = { 0 };

G_DEFINE_TYPE (GedaTearoffMenuItem, geda_tearoff_menu_item, GEDA_TYPE_MENU_ITEM)

/*!
 * \brief Create a New GedaTearoffMenuItem
 * \par Function Description
 * Creates a new #GedaTearoffMenuItem.
 *
 * \returns a new #GedaTearoffMenuItem.
 */
GtkWidget*
geda_tearoff_menu_item_new (void)
{
  return g_object_new (GEDA_TYPE_TEAROFF_MENU_ITEM, NULL);
}

static void
geda_tearoff_menu_item_class_init (GedaTearoffMenuItemClass *klass)
{
  GtkWidgetClass    *widget_class;
  GedaMenuItemClass *menu_item_class;

  widget_class    = (GtkWidgetClass*)klass;
  menu_item_class = (GedaMenuItemClass*)klass;

#if GTK_MAJOR_VERSION < 3
  widget_class->expose_event = geda_tearoff_menu_item_expose;
  widget_class->size_request = geda_tearoff_menu_item_size_request;


#else
  widget_class->draw                 = geda_tearoff_menu_item_draw;
  widget_class->get_preferred_width  = geda_tearoff_menu_item_get_preferred_width;
  widget_class->get_preferred_height = geda_tearoff_menu_item_get_preferred_height;

  gtk_widget_class_set_accessible_role (widget_class, ATK_ROLE_TEAR_OFF_MENU_ITEM);

#endif

  widget_class->parent_set           = geda_tearoff_menu_item_parent_set;

  menu_item_class->activate = geda_tearoff_menu_item_activate;

  geda_tearoff_signals[ TORN_OFF ] = g_signal_new ("torn-off",
                                       G_OBJECT_CLASS_TYPE(klass),
                                       0     /*signal_flags */,
                                       0     /*class_offset */,
                                       NULL, /* accumulator */
                                       NULL, /* accu_data */
                                       g_cclosure_marshal_VOID__VOID,
                                       G_TYPE_NONE,
                                       0);   /* n_params */
}

static void
geda_tearoff_menu_item_init (GedaTearoffMenuItem *tearoff_menu_item)
{
  tearoff_menu_item->priv = g_malloc0 (sizeof(GedaTearoffMenuItemData));
}

#if GTK_MAJOR_VERSION < 3

static void
geda_tearoff_menu_item_size_request (GtkWidget      *widget,
                                     GtkRequisition *requisition)
{
  requisition->width  = (GTK_CONTAINER (widget)->border_width +
                         widget->style->xthickness + BORDER_SPACING) * 2;

  requisition->height = (GTK_CONTAINER (widget)->border_width +
                         widget->style->ythickness) * 2;

  if (GEDA_IS_MENU (widget->parent) && GEDA_MENU (widget->parent)->torn_off)
  {
    requisition->height += ARROW_SIZE;
  }
  else
  {
    requisition->height += widget->style->ythickness + 4;
  }
}

static void
geda_tearoff_menu_item_paint (GtkWidget *widget, GdkRectangle *area)
{
  GedaMenuItem  *menu_item;
  GtkShadowType shadow_type;
  int  width, height;
  int  x, y;
  int  right_max;
  GtkArrowType arrow_type;
  GtkTextDirection direction;

  if (gtk_widget_is_drawable (widget)) {

    menu_item = GEDA_MENU_ITEM (widget);

    direction = gtk_widget_get_direction (widget);

    x = widget->allocation.x + GTK_CONTAINER (menu_item)->border_width;
    y = widget->allocation.y + GTK_CONTAINER (menu_item)->border_width;
    width = widget->allocation.width - GTK_CONTAINER (menu_item)->border_width * 2;
    height = widget->allocation.height - GTK_CONTAINER (menu_item)->border_width * 2;
    right_max = x + width;

    if (widget->state == GTK_STATE_PRELIGHT) {

      int  selected_shadow_type;

      gtk_widget_style_get (widget,
                            "selected-shadow-type", &selected_shadow_type,
                            NULL);
      gtk_paint_box (widget->style,
                     widget->window,
                     GTK_STATE_PRELIGHT,
                     selected_shadow_type,
                     area, widget, "menuitem",
                     x, y, width, height);
    }
    else
      gdk_window_clear_area (widget->window, area->x, area->y, area->width, area->height);

    if (GEDA_IS_MENU (widget->parent) && GEDA_MENU (widget->parent)->torn_off)
    {
      int arrow_x;
      int toggle_size;

      toggle_size = geda_menu_item_get_toggle_size(menu_item);

      if (widget->state == GTK_STATE_PRELIGHT)
        shadow_type = GTK_SHADOW_IN;
      else
        shadow_type = GTK_SHADOW_OUT;

      if (toggle_size > ARROW_SIZE) {

        if (direction == GTK_TEXT_DIR_LTR) {
          arrow_x = x + (toggle_size - ARROW_SIZE)/2;
          arrow_type = GTK_ARROW_LEFT;
        }
        else {
          arrow_x = x + width - toggle_size + (toggle_size - ARROW_SIZE)/2;
          arrow_type = GTK_ARROW_RIGHT;
        }
        x += toggle_size + BORDER_SPACING;
      }
      else {

        if (direction == GTK_TEXT_DIR_LTR) {
          arrow_x = ARROW_SIZE / 2;
          arrow_type = GTK_ARROW_LEFT;
        }
        else {
          arrow_x = x + width - 2 * ARROW_SIZE + ARROW_SIZE / 2;
          arrow_type = GTK_ARROW_RIGHT;
        }
        x += 2 * ARROW_SIZE;
      }

      gtk_paint_arrow (widget->style, widget->window,
                       widget->state, shadow_type,
                       NULL, widget, "tearoffmenuitem",
                       arrow_type, FALSE,
                       arrow_x, y + height / 2 - 5,
                       ARROW_SIZE, ARROW_SIZE);
    }

    while (x < right_max) {

      int  x1, x2;

      if (direction == GTK_TEXT_DIR_LTR) {
        x1 = x;
        x2 = MIN (x + TEAR_LENGTH, right_max);
      }
      else {
        x1 = right_max - x;
        x2 = MAX (right_max - x - TEAR_LENGTH, 0);
      }

      gtk_paint_hline (widget->style, widget->window, GTK_STATE_NORMAL,
                       NULL, widget, "tearoffmenuitem",
                       x1, x2, y + (height - widget->style->ythickness) / 2);
      x += 2 * TEAR_LENGTH;
    }
  }
}

static int
geda_tearoff_menu_item_expose (GtkWidget *widget, GdkEventExpose *event)
{
  geda_tearoff_menu_item_paint (widget, &event->area);

  return FALSE;
}
#else
static void
geda_tearoff_menu_item_get_preferred_width (GtkWidget *widget,
                                            int       *minimum,
                                            int       *natural)
{
  GtkStyleContext *context;
  unsigned int border_width;
  GtkBorder padding;
  GtkStateFlags state;

  context = gtk_widget_get_style_context (widget);
  state = gtk_widget_get_state_flags (widget);

  gtk_style_context_get_padding (context, state, &padding);
  border_width = gtk_container_get_border_width (GTK_CONTAINER (widget));

  *minimum = *natural = (border_width + BORDER_SPACING) * 2 + padding.left + padding.right;
}

static void
geda_tearoff_menu_item_get_preferred_height (GtkWidget *widget,
                                             int       *minimum,
                                             int       *natural)
{
  GtkStyleContext *context;
  GtkBorder padding;
  GtkStateFlags state;
  GtkWidget *parent;
  unsigned int border_width;

  context = gtk_widget_get_style_context (widget);
  state   = gtk_widget_get_state_flags (widget);

  gtk_style_context_get_padding (context, state, &padding);

  border_width = gtk_container_get_border_width (GTK_CONTAINER (widget));

  *minimum = *natural = (border_width * 2) + padding.top + padding.bottom;

  parent = gtk_widget_get_parent (widget);

  if (GEDA_IS_MENU (parent) && GEDA_MENU (parent)->torn_off) {

    *minimum += ARROW_SIZE;
    *natural += ARROW_SIZE;
  }
  else {

    *minimum += padding.top + 4;
    *natural += padding.top + 4;
  }
}

static bool
geda_tearoff_menu_item_draw (GtkWidget *widget,
                            cairo_t   *cr)
{
  GedaMenuItem     *menu_item;
  GtkStateFlags    state;
  GtkStyleContext *context;
  GtkBorder padding;
  int  x, y, width, height;
  int  right_max;
  unsigned int border_width;
  GtkTextDirection direction;
  GtkWidget *parent;
  gdouble angle;

  menu_item    = GEDA_MENU_ITEM (widget);
  context      = gtk_widget_get_style_context (widget);
  direction    = gtk_widget_get_direction (widget);
  state        = gtk_widget_get_state_flags (widget);

  border_width = gtk_container_get_border_width (GTK_CONTAINER (menu_item));
  x            = border_width;
  y            = border_width;
  width        = gtk_widget_get_allocated_width (widget) - border_width * 2;
  height       = gtk_widget_get_allocated_height (widget) - border_width * 2;
  right_max    = x + width;

  gtk_style_context_save (context);
  gtk_style_context_set_state (context, state);
  gtk_style_context_get_padding (context, state, &padding);

  if (state & GTK_STATE_FLAG_PRELIGHT) {

    gtk_render_background (context, cr, x, y, width, height);
    gtk_render_frame (context, cr, x, y, width, height);
  }

  parent = gtk_widget_get_parent (widget);

  if (GEDA_IS_MENU (parent) && GEDA_MENU (parent)->torn_off) {

    int  arrow_x;

    if (menu_item->toggle_size > ARROW_SIZE) {

      if (direction == GTK_TEXT_DIR_LTR) {

        arrow_x = x + (menu_item->toggle_size - ARROW_SIZE)/2;
        angle = (3 * G_PI) / 2;
      }
      else {

        arrow_x = x + width - menu_item->toggle_size + (menu_item->toggle_size - ARROW_SIZE)/2;
        angle = G_PI / 2;
      }
      x += menu_item->toggle_size + BORDER_SPACING;
    }
    else {

      if (direction == GTK_TEXT_DIR_LTR) {

        arrow_x = ARROW_SIZE / 2;
        angle = (3 * G_PI) / 2;
      }
      else {

        arrow_x = x + width - 2 * ARROW_SIZE + ARROW_SIZE / 2;
        angle = G_PI / 2;
      }
      x += 2 * ARROW_SIZE;
    }

    gtk_render_arrow (context, cr, angle,
                      arrow_x, height / 2 - 5,
                      ARROW_SIZE);
  }

  while (x < right_max) {

    int  x1, x2;

    if (direction == GTK_TEXT_DIR_LTR) {

      x1 = x;
      x2 = MIN (x + TEAR_LENGTH, right_max);
    }
    else {

      x1 = right_max - x;
      x2 = MAX (right_max - x - TEAR_LENGTH, 0);
    }

    gtk_render_line (context, cr,
                     x1, y + (height - padding.bottom) / 2,
                     x2, y + (height - padding.bottom) / 2);
    x += 2 * TEAR_LENGTH;
  }

  gtk_style_context_restore (context);

  return FALSE;
}
#endif

static void
geda_tearoff_menu_item_activate (GedaMenuItem *menu_item)
{
  GtkWidget *parent;

  parent = gtk_widget_get_parent (GTK_WIDGET (menu_item));

  if (GEDA_IS_MENU (parent)) {

    GedaMenu *menu = GEDA_MENU (parent);

    gtk_widget_queue_resize (GTK_WIDGET (menu_item));
    geda_menu_set_tearoff_state (GEDA_MENU (parent), !geda_menu_get_tearoff_state (menu));
  }
}

static void
tearoff_state_changed (GedaMenu *menu, GParamSpec *pspec, void *data)
{
  GedaTearoffMenuItem     *tearoff_menu_item = GEDA_TEAROFF_MENU_ITEM (data);
  GedaTearoffMenuItemData *priv              = tearoff_menu_item->priv;

  priv->torn_off = geda_menu_get_tearoff_state (menu);

  if (priv->torn_off) {
    g_signal_emit(tearoff_menu_item, geda_tearoff_signals[ TORN_OFF ], 0);
  }
}

static void
geda_tearoff_menu_item_parent_set (GtkWidget *widget, GtkWidget *previous)
{
  GedaTearoffMenuItem     *tearoff_menu_item = GEDA_TEAROFF_MENU_ITEM (widget);
  GedaTearoffMenuItemData *priv              = tearoff_menu_item->priv;
  GedaMenu                *menu;
  GtkWidget               *parent;

  parent = gtk_widget_get_parent (widget);
  menu   = GEDA_IS_MENU (parent) ? GEDA_MENU (parent) : NULL;

  if (previous) {
    g_signal_handlers_disconnect_by_func (previous,
                                          tearoff_state_changed,
                                          tearoff_menu_item);
  }

  if (menu) {

    priv->torn_off = geda_menu_get_tearoff_state (menu);
    g_signal_connect (menu, "notify::tearoff-state",
                      G_CALLBACK (tearoff_state_changed),
                      tearoff_menu_item);
  }
}

/*!
 * \brief Get the active state of a GedaTearoffMenuItem
 * \par Function Description
 *  Returns the value of menu->tearoff_active.
 *
 * \return value of menu->tearoff_active
 */
bool geda_tearoff_menu_is_active (GtkWidget *menu)
{
  if (GEDA_IS_TEAROFF_MENU_ITEM (menu)) {
    return (GEDA_MENU(menu)->tearoff_active);
  }
  return FALSE;
}

/*!
 * \brief Get the tear-off state of a GedaTearoffMenuItem
 * \par Function Description
 *  Returns the value of torn_off property.
 *
 * \return value of priv->torn_off
 */
bool geda_tearoff_menu_is_torn (GtkWidget *menu)
{
  if (GEDA_IS_TEAROFF_MENU_ITEM (menu)) {

    GedaTearoffMenuItem *tearoff_menu_item = GEDA_TEAROFF_MENU_ITEM (menu);
    GedaTearoffMenuItemData *priv = tearoff_menu_item->priv;
    return priv->torn_off;
  }
  return FALSE;
}

/** @} endgroup GedaTearoffMenuItem */
