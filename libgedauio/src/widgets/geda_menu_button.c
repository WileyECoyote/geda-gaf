/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_menu_button.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill <wileyhill@gmail.com>
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
 * Date: March 31, 2013
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */
/*! \file geda_menu_button.c
 *  \brief GedaMenuButton Class Module
 */

/** \defgroup geda-menu-button GedaMenuButton Object
 * @{
 * \brief GedaMenuButton - A Button Widget for Menus
 * \par
 *  A GedaMenuButton is a button object used on toolbars or menus.
 *  GedaMenuButton is a replacement for the GedaMenuButton because the
 *  GedaMenuButton has a display issue related to the "themeing" and
 *  pre-light, forgot the details. And GedaMenuButton uses a GtkLabel,
 *  which valgrind reports as a memory leak after GTK over-rides the
 *  font to LibFontConfig's best match of the default "theme" font but
 *  neither Pango nor LibFontConfig free the (I think) description.
 *
 * \class GedaMenuButton geda_menu_button.h "include/geda_menu_button.h"
 * \implements GtkEventBox
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include <glib.h>
#include <gtk/gtk.h>

#include "../../include/geda_container.h"
#include "../../include/geda_marshal.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_menu_button.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/* Time out before giving up on getting a key release when animating the
 * close button.
 */
#define ACTIVATE_TIMEOUT 250

struct _GedaMenuButtonData
{
  GtkWidget *button;
  GtkWidget *arrow;
  GtkWidget *arrow_button;
  GtkWidget *box;
  GtkWidget *menu;

  unsigned int has_grab;
  uint32       grab_time;

  GtkAction   *action;
};

static void geda_menu_button_dispose (GObject *object);
static void geda_menu_button_destroy (GtkObject *object);

static int  geda_menu_deactivate_cb(GedaMenuShell *menu_shell, GedaMenuButton *button);

static bool arrow_button_press_event_cb               (GtkWidget         *widget,
                                                       GdkEventButton    *event,
                                                       GedaMenuButton    *button);
static void arrow_button_toggled_cb                   (GtkToggleButton   *togglebutton,
                                                       GedaMenuButton    *button);

static void geda_menu_button_buildable_interface_init (GtkBuildableIface *iface);
static void geda_menu_button_buildable_add_child      (GtkBuildable      *buildable,
                                                       GtkBuilder        *builder,
                                                       GObject           *child,
                                                       const char        *type);

enum
{
  PRESSED,
  RELEASED,
  CLICKED,
  ENTER,
  LEAVE,
  ACTIVATE,
  SHOW_MENU,
  LAST_SIGNAL
};

enum
{
  PROP_0,
  PROP_FOCUS_ON_CLICK,
  PROP_IMAGE,
  PROP_IMAGE_POSITION,
  PROP_LABEL,
  PROP_MENU,
  PROP_RELIEF,
  PROP_MENU_RELIEF,
  PROP_USE_STOCK,
  PROP_USE_UNDERLINE,
};

static unsigned int signals[LAST_SIGNAL] = { 0 };

static void *geda_menu_button_parent_class = NULL;

static GtkBuildableIface *parent_buildable_iface;

static GHashTable *menu_button_hash = NULL;

/* BEGIN ------+-------+-------^- GtkBuildable ^-------+-------+-------+-----*/

static void
geda_menu_button_buildable_add_child (GtkBuildable *buildable,
                                      GtkBuilder   *builder,
                                      GObject      *child,
                                      const char   *type)
{
  if (type && strcmp (type, "menu") == 0) {
    geda_menu_button_set_menu (GEDA_MENU_BUTTON (buildable), GTK_WIDGET (child));
  }
  else {
    parent_buildable_iface->add_child (buildable, builder, child, type);
  }
}

static void
geda_menu_button_buildable_interface_init (GtkBuildableIface *iface)
{
  parent_buildable_iface = g_type_interface_peek_parent (iface);
  iface->add_child       = geda_menu_button_buildable_add_child;
}

/* END -------------------------- GtkBuildable ----------------------------- */

/* BEGIN ------+-------+------ Property Handlers ------+-------+-------+-----*/

/*!
 * \brief Set GedaMenuButton Widget Style
 * \par Function Description
 *  Sets the widget style property for both the button and the arrow.
 */
void
geda_menu_button_set_style (GedaMenuButton *button, GtkStyle *new_style)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  gtk_widget_set_style( GTK_WIDGET (button->priv->button), new_style);
  gtk_widget_set_style( GTK_WIDGET (button->priv->arrow_button), new_style);
}

/*!
 * \brief Set GedaMenuButton Relief Property
 * \par Function Description
 *  Sets the relief of the button, \a new_relief should be one of:
 *
 *  <DL>
 *    <DT><B>GTK_RELIEF_NORMAL</B></DT>
 *    <DT><B>GTK_RELIEF_HALF</B></DT>
 *    <DT><B>GTK_RELIEF_NONE</B></DT>
 *  </DL>
 */
void
geda_menu_button_set_relief (GedaMenuButton *button, GtkReliefStyle new_relief)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  button->relief = new_relief;
  gtk_button_set_relief (GTK_BUTTON (button->priv->button), new_relief);
}

/*!
 * \brief Get GedaMenuButton Relief Property
 * \par Function Description
 *  Retrieves the GtkReliefStyle used by the GedaMenuButton object.
 *
 * \return relief style setting
 */
GtkReliefStyle
geda_menu_button_get_relief (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), GTK_RELIEF_NORMAL);
  return button->relief;
}

/*!
 * \brief Set GedaMenuButton Arrow Relief Property
 * \par Function Description
 *  Sets the relief of the button, \a new_relief should be one of:
 *
 *  <DL>
 *    <DT><B>GTK_RELIEF_NORMAL</B></DT>
 *    <DT><B>GTK_RELIEF_HALF</B></DT>
 *    <DT><B>GTK_RELIEF_NONE</B></DT>
 *  </DL>
 */
void
geda_menu_arrow_set_relief (GedaMenuButton *button, GtkReliefStyle new_relief)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  button->menu_relief = new_relief;
  gtk_button_set_relief (GTK_BUTTON (button->priv->arrow_button), new_relief);

}

/*!
 * \brief Get the GedaMenuButton Arrow Relief Property
 * \par Function Description
 *  Retrieves the GtkReliefStyle used by the GedaMenuButton arrow object.
 *
 * \return arrow relief style setting
 */
GtkReliefStyle
geda_menu_arrow_get_relief (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), GTK_RELIEF_NORMAL);

  return button->menu_relief;

}

/*!
 * \brief Set the Label Text on a GedaMenuButton
 * \par Function Description
 *  Sets the text of the label of the button to value pointed to by
 *  label. This text is also used to select the stock item if
 *  geda_menu_button_set_use_stock() is used.
 *
 *  This will also clear any previously set labels.
 */
void geda_menu_button_set_label (GedaMenuButton *button, const char *label)
{
  g_return_if_fail ( GEDA_IS_MENU_BUTTON (button) );
  gtk_button_set_label( GTK_BUTTON(button->priv->button), label);
}

/*!
 * \brief Get the Label Text on a GedaMenuButton
 * \par Function Description
 *  This function retrieves the text from the label of the button.
 *  If the label text has not been set the return value will be NULL.
 *  This will could be the case if an empty menu button was created
 *  with gtk_button_new() to use as a container.
 *
 *  \returns The text of the label widget. This string is owned
 *           by the widget and must not be modified or freed.
 */
const char *geda_menu_button_get_label (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), NULL);
  return gtk_button_get_label(GTK_BUTTON(button->priv->button));
}

/*!
 * \brief Get the Label Text on a GedaMenuButton
 * \par Function Description
 *  If true, an underline in the text of the button label indicates
 *  the next character should be used for the mnemonic accelerator key.
 *
 * \param [in] button        a GedaMenuButton
 * \param [in] use_underline TRUE if underlines in the text indicate mnemonics
 */
void
geda_menu_button_set_use_underline (GedaMenuButton *button, bool use_underline)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_use_underline(GTK_BUTTON(button->priv->button), use_underline);
}

/*!
 * \brief Get GedaMenuButton Mnemonic Interpretation Setting
 * \par Function Description
 *  Returns whether an embedded underline in the button label indicates
 *  a mnemonic. See gtk_button_set_use_underline ().
 *
 * \param [in] button a GedaMenuButton
 *
 * \retval %TRUE if an embedded underline in the button label
 *          indicates the mnemonic accelerator keys.
 */
bool
geda_menu_button_get_use_underline (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), FALSE);
  return gtk_button_get_use_underline( GTK_BUTTON(button->priv->button));
}

/*!
 * \brief Set a GedaMenuButton to use stock item set by label text
 * \par Function Description
 * If TRUE, the label set on the button is used as a stock id to
 * select the stock item for the button.
 */
void geda_menu_button_set_use_stock (GedaMenuButton *button, bool use_stock)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_use_stock(GTK_BUTTON(button->priv->button), use_stock);
}

/*!
 * \brief Query to determine if GedaMenuButton uses stock item
 * \par Function Description
 *   Returns whether the button label is a stock item.
 *
 * \param [in] button a GedaMenuButton
 *
 * \retval TRUE if the button label is used to select a stock item
 *         instead of being used directly as the label text.
 */
bool geda_menu_button_get_use_stock (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), FALSE);
  return gtk_button_get_use_stock (GTK_BUTTON(button->priv->button));
}

/*!
 * \brief Sets whether the GedaMenuButton primary  button  will grap focus
 * \par Function Description
 *  Sets whether the button will grab focus when it is clicked with the mouse.
 *  Making mouse clicks not grab focus is useful in places like toolbars where
 *  you don't want the keyboard focus removed from the main area of the
 *  application.
 *
 * \param [in] button         a GedaMenuButton
 * \param [in] focus_on_click %TRUE if widget should grab focus
 */
void
geda_menu_button_set_focus_on_click (GedaMenuButton *button, bool focus_on_click)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_focus_on_click( GTK_BUTTON (button->priv->button) ,
                                 focus_on_click);
}

/*!
 * \brief Returns whether a GedaMenuButton grabs focus.
 * \par Function Description
 *  This function returns whether the main button grabs focus when
 *  the button is clicked with the mouse.
 *
 * \sa gtk_button_set_focus_on_click().
 *
 * \retval TRUE  if the button grabs focus when it is clicked
 *               with the mouse.
 */
bool geda_menu_button_get_focus_on_click (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), FALSE);
  return gtk_button_get_focus_on_click( GTK_BUTTON (button->priv->button));
}

/*!
 * \brief Sets the alignment of the child in a GedaMenuButton
 * \par Function Description
 * Sets the alignment of the child. This property has no effect unless
 * the child is a GtkMisc or a GtkAligment.
 *
 * \param [in] button  a GedaMenuButton
 * \param [in] xalign  the horizontal position of the child, 0.0 is left aligned,
 *                     1.0 is right aligned
 * \param [in] yalign  the vertical position of the child, 0.0 is top aligned,
 *                     1.0 is bottom aligned
 */
void
geda_menu_button_set_alignment (GedaMenuButton *button,
                                float           xalign,
                                float           yalign)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_alignment( GTK_BUTTON (button->priv->button),
                                        xalign, yalign);

}

/*!
 * \brief Gets the alignment of the child in a GedaMenuButton
 * \par Function Description
 *  Gets the alignment of the child in the button.
 *
 * \param [in]  button  a GedaMenuButton
 *
 * \param [out] xalign  return location for horizontal alignment
 * \param [out] yalign  return location for vertical alignment
 *
 */
void
geda_menu_button_get_alignment (GedaMenuButton *button,
                                         float *xalign,
                                         float *yalign)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  return gtk_button_get_alignment( GTK_BUTTON (button->priv->button),
                                   xalign, yalign);
}

/*!
 * \brief Sets the Image of the GedaMenuButton
 * \par Function Description
 * Set the image of button to the given widget. Note that
 * it depends on the GtkSettings:gtk-button-images setting whether
 * the image will be displayed or not, it is not necessary to call
 * gtk_widget_show() on image separately.
 *
 * \param [in] button The GedaMenuButton
 * \param [in] image  a widget to set as the image for the button
 */
void geda_menu_button_set_image (GedaMenuButton *button, GtkWidget *image)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_image (GTK_BUTTON (button->priv->button), image);
}

/*!
 * \brief Gets the GedaMenuButton Image object
 * \par Function Description
 *  Gets the widget that is currenty set as the image of button.
 *  This may have been explicitly set by gtk_button_set_image()
 *  or constructed by gtk_button_new_from_stock().
 *
 * \param [in] button The GedaMenuButton
 *
 * \returns a GtkWidget or %NULL in case there is no image
 */
GtkWidget *geda_menu_button_get_image (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), NULL);
  return gtk_button_get_image (GTK_BUTTON (button->priv->button));
}

/*!
 * \brief Sets the Image Position in a GedaMenuButton
 * \par Function Description
 *  Sets the position of the image relative to the text
 *  inside the button.
 */
void geda_menu_button_set_image_position (GedaMenuButton *button, GtkPositionType position)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_image_position ( GTK_BUTTON (button->priv->button), position);
}

/*!
 * \brief Gets the GedaMenuButton Image Position
 * \par Function Description
 *  Gets the position of the image relative to the text
 *  inside the button.
 *
 * \returns the position
 */
GtkPositionType
geda_menu_button_get_image_position (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), GTK_POS_LEFT);
  return gtk_button_get_image_position (GTK_BUTTON (button->priv->button));
}

/*!
 * \brief Gets the GedaMenuButton Event Window
 * \par Function Description
 * Returns the button's event window if it is realized, %NULL otherwise.
 * This function should be rarely needed.
 *
 * \returns the button's event window.
 */
GdkWindow *geda_menu_button_get_event_window (GedaMenuButton *button)
{
  GdkWindow *window;

  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), NULL);

  //window =  gtk_button_get_event_window (GTK_BUTTON (button->priv->button));
  window = gtk_widget_get_window(GTK_WIDGET(button->priv->button));

  return window;
}

/*!
 * \brief  Set Properties of a GedaMenuButton
 * \par Function Description
 *  This function handled the gobject properties setters.
 *
 * \param [in] object   a GedaMenuButton
 * \param [in] prop_id  The enumerated property ID
 * \param [in] value    The value to set the property
 * \param [in] pspec    The parameter specifications for the property
 */
static void geda_menu_button_set_property (GObject      *object,
                                           unsigned int  prop_id,
                                           const GValue *value,
                                           GParamSpec   *pspec)
{
  GedaMenuButton     *button = GEDA_MENU_BUTTON (object);
  GedaMenuButtonData *priv   = button->priv;
  GtkButton          *butt   = GTK_BUTTON(priv->button);

  switch (prop_id) {
    case PROP_FOCUS_ON_CLICK:
      gtk_button_set_focus_on_click (butt, g_value_get_boolean (value));
      break;

    case PROP_IMAGE:
      gtk_button_set_image (butt, (GtkWidget *) g_value_get_object (value));
      break;

    case PROP_IMAGE_POSITION:
      gtk_button_set_image_position (butt, g_value_get_enum (value));
      break;

    case PROP_LABEL:
      geda_menu_button_set_label (button, g_value_get_string (value));
      break;

    case PROP_MENU:
      geda_menu_button_set_menu (button, g_value_get_object (value));
      break;

    case PROP_RELIEF:
      button->relief = g_value_get_enum (value);
      geda_menu_button_set_relief (button, button->relief);
      break;

    case PROP_MENU_RELIEF:
      button->menu_relief = g_value_get_enum (value);
      geda_menu_arrow_set_relief (button, button->menu_relief);
      break;

    case PROP_USE_STOCK:
      button->use_stock = g_value_get_boolean (value);
      gtk_button_set_use_stock (butt, button->use_stock);
      break;

    case PROP_USE_UNDERLINE:
      button->use_underline = g_value_get_boolean (value);
      gtk_button_set_use_underline (butt, button->use_underline);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/*!
 * \brief  Get Properties of a GedaMenuButton.
 * \par Function Description
 *  This function handled the gobject properties request.
 *
 * \param [in]  object   a GedaMenuButton
 * \param [in]  prop_id  The enumerated property ID
 * \param [out] value    The variable that will be set to the property value
 * \param [in]  pspec    The parameter specifications for the property
 */
static void geda_menu_button_get_property (GObject     *object,
                                           unsigned int prop_id,
                                           GValue      *value,
                                           GParamSpec  *pspec)
{
  GedaMenuButton     *button = GEDA_MENU_BUTTON (object);
  GedaMenuButtonData *priv   = button->priv;
  GtkButton          *butt   = GTK_BUTTON(priv->button);

  switch (prop_id) {

    case PROP_FOCUS_ON_CLICK:
      g_value_set_boolean (value, butt->focus_on_click);
      break;

    case PROP_IMAGE:
      g_value_set_object (value, (GObject*) gtk_button_get_image(butt));
      break;

    case PROP_IMAGE_POSITION:
      g_value_set_enum (value, gtk_button_get_image_position(butt));
      break;

    case PROP_LABEL:
      g_value_set_string (value, butt->label_text);
      break;

    case PROP_MENU:
      g_value_set_object (value, button->priv->menu);
      break;

    case PROP_RELIEF:
      g_value_set_enum (value, button->relief);
      break;

    case PROP_MENU_RELIEF:
      g_value_set_enum (value, button->menu_relief);
      break;

    case PROP_USE_UNDERLINE:
      g_value_set_boolean (value, butt->use_underline);
      break;

    case PROP_USE_STOCK:
      g_value_set_boolean (value, butt->use_stock);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/* END ----------------------- Property Handlers --------------------------- */

/* BEGIN ------+-------+-------^    Emitters   ^-------+-------+-------+-----*/

/*!
 * \brief  GedaMenuButton (Main) Button "pressed" Signal Emitter.
 * \par Function Description
 *  This function requests the "pressed" signal be emitted from the Widget.
 *
 * \param [in] button a GedaMenuButton
 *
 */
void geda_menu_button_pressed (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[PRESSED], 0);
}

/*!
 * \brief  GedaMenuButton (Main) Button "released" Signal Emitter.
 * \par Function Description
 *  This function requests the "released" signal be emitted from the Widget.
 *
 * \param [in] button a GedaMenuButton
 */
void geda_menu_button_released (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[RELEASED], 0);
}

/*!
 * \brief  GedaMenuButton (Main) Button "clicked" Signal Emitter.
 * \par Function Description
 *  This function requests the "clicked" signal be emitted from the Widget.
 *
 * \param [in] button a GedaMenuButton
 */
void geda_menu_button_clicked (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[CLICKED], 0);
}

/*!
 * \brief  GedaMenuButton "enter" Signal Emitter.
 * \par Function Description
 *  This function requests the "enter" signal be emitted from
 *  the Widget, which occurs when the mouse enters the widget's
 *  boundaries.
 *
 * \param [in] button a GedaMenuButton
 */
void geda_menu_button_enter (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[ENTER], 0);
}

/*!
 * \brief  GedaMenuButton "leave" Signal Emitter.
 * \par Function Description
 *   This function requests the "leave" signal be emitted from
 *   the Widget, which occurs when the mouse leaves the widget's
 *   boundaries.
 *
 * \param [in] button a GedaMenuButton
 */
void geda_menu_button_leave (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[LEAVE], 0);
}

/* END --------+-------+-------^    Emitters   ^-------+-------+-------+-----*/

/* BEGIN ------+-------+------- Signal Handlers -------+-------+-------+-----*/

/*!
 * \brief  GedaMenuButton Update State Signal Handler.
 * \par Function Description
 *  This function updates the visual appearance of the button.
 *
 * \param [in] button a GedaMenuButton
 */
static void geda_menu_button_update_state (GedaMenuButton *button)
{
  bool depressed;
  GtkStateType new_state;

  if (button->activate_timeout)
    depressed = button->depress_on_activate;
  else
    depressed = button->in_button && button->button_down;

  if (button->in_button && (!button->button_down || !depressed))
    new_state = GTK_STATE_PRELIGHT;
  else
    new_state = depressed ? GTK_STATE_ACTIVE : GTK_STATE_NORMAL;

  gtk_widget_set_state (GTK_WIDGET (button), new_state);
}

/*!
 * \brief  GedaMenuButton Process the "clicked" Signal internally.
 * \par Function Description
 *  This function is called when the "clicked" signal is received
 *  from the button. The function checks and executes the action
 *  if an action exist.
 *
 *  TODO: Add action property handler?
 *
 * \param [in] button a GedaMenuButton
 */
static void geda_menu_button_button_clicked (GedaMenuButton *button)
{
  GedaMenuButtonData *priv = button->priv;

  if (priv->action) {
    gtk_action_activate (priv->action);
  }
}

/*!
 * \brief  GedaMenuButton Process the "pressed" Signal internally.
 * \par Function Description
 *  This function is called when the "pressed" signal is received
 *  from the button to update the visual appearance if the "down
 *  time" has expired.
 *
 * \param [in] button a GedaMenuButton
 */
static void geda_menu_button_button_pressed (GedaMenuButton *button)
{
  if (button->activate_timeout) {
    return;
  }

  button->button_down = TRUE;
  geda_menu_button_update_state (button);
}

/*!
 * \brief  GedaMenuButton Process the "released" Signal internally.
 * \par Function Description
 *  This function is called when the "released" signal is received
 *  from the button to generate the "clicked" signal and to update
 *  the visual appearance if the "down time" has expired.
 *
 * \param [in] button a GedaMenuButton
 */
static void geda_menu_button_button_released (GedaMenuButton *button)
{
  if (button->button_down) {

    button->button_down = FALSE;

    if (button->activate_timeout) {
      return;
    }

    if (button->in_button) {
      geda_menu_button_clicked (button);
    }

    geda_menu_button_update_state (button);
  }
}

static void geda_menu_button_finish_activate (GedaMenuButton *button, bool do_it)
{
  GedaMenuButtonData *priv = button->priv;
  GtkWidget          *widget = GTK_WIDGET (button);

  g_source_remove (button->activate_timeout);
  button->activate_timeout = 0;

  if (priv->has_grab) {
    GdkDisplay *display = gtk_widget_get_display(widget);
    gdk_display_keyboard_ungrab (display, priv->grab_time);
  }

  gtk_grab_remove (widget);

  button->button_down = FALSE;

  geda_menu_button_update_state (button);
  gtk_widget_queue_draw (GTK_WIDGET (button));

  if (do_it) {
    geda_menu_button_clicked (button);
  }
}

static bool button_activate_timeout (void * data)
{
  geda_menu_button_finish_activate (data, TRUE);

  return FALSE;
}

static void geda_menu_button_activate (GedaMenuButton *button)
{
  GedaMenuButtonData *priv   = button->priv;
  GtkWidget          *widget = GTK_WIDGET (button);

  uint32 time;

  if (gtk_widget_get_realized (widget) && !button->activate_timeout) {

    time = gtk_get_current_event_time ();

    if (gdk_keyboard_grab (button->event_window, TRUE, time) ==
        GDK_GRAB_SUCCESS)
    {
      priv->has_grab = TRUE;
      priv->grab_time = time;
    }

    gtk_grab_add (widget);

    button->activate_timeout = gdk_threads_add_timeout (ACTIVATE_TIMEOUT,
                                                        button_activate_timeout,
                                                        button);
    button->button_down = TRUE;
    geda_menu_button_update_state (button);
    gtk_widget_queue_draw (GTK_WIDGET (button));
  }
}

/* BEGIN ------+-------+--- Widget Class Over-rides ---+-------+-------+-----*/

static bool geda_menu_button_button_press_event (GtkWidget *widget,
                                                 GdkEventButton *event)
{
  if (event->type == GDK_BUTTON_PRESS) {

    GedaMenuButton *button;

    button = GEDA_MENU_BUTTON (widget);

    if (button->focus_on_click && !gtk_widget_has_focus (widget))
      gtk_widget_grab_focus (widget);

    if (event->button == 1)
      geda_menu_button_released (button);
  }

  return TRUE;
}

static bool geda_menu_button_grab_broken_event (GtkWidget *widget,
                                                GdkEventGrabBroken *event)
{
  GedaMenuButton *button = GEDA_MENU_BUTTON (widget);

  /* Simulate a button release without the pointer in the button */
  if (button->button_down) {

    bool save_in = button->in_button;

    button->in_button = FALSE;

    geda_menu_button_released (button);

    if (save_in != button->in_button) {

      button->in_button = save_in;
      geda_menu_button_update_state (button);
    }
  }

  return TRUE;
}

static bool geda_menu_button_key_release_event (GtkWidget *widget, GdkEventKey *event)
{
  GedaMenuButton *button = GEDA_MENU_BUTTON (widget);

  if (button->activate_timeout) {

      geda_menu_button_finish_activate (button, TRUE);
      return TRUE;
  }
  else if (GTK_WIDGET_CLASS (geda_menu_button_parent_class)->key_release_event)
    return GTK_WIDGET_CLASS (geda_menu_button_parent_class)->key_release_event (widget, event);
  else
    return FALSE;
}

static bool geda_menu_button_enter_notify (GtkWidget *widget, GdkEventCrossing *event)
{
  GedaMenuButton *button;
  GtkWidget      *event_widget;

  button       = GEDA_MENU_BUTTON (widget);
  event_widget = gtk_get_event_widget ((GdkEvent*) event);

  if ((event_widget == widget) && (event->detail != GDK_NOTIFY_INFERIOR))
  {
    button->in_button = TRUE;
    geda_menu_button_enter (button);
  }

  return FALSE;
}

static bool geda_menu_button_leave_notify (GtkWidget *widget, GdkEventCrossing *event)
{
  GedaMenuButton *button;
  GtkWidget      *event_widget;

  button       = GEDA_MENU_BUTTON (widget);
  event_widget = gtk_get_event_widget ((GdkEvent*) event);

  if ((event_widget == widget) &&
      (event->detail != GDK_NOTIFY_INFERIOR) &&
      (gtk_widget_get_sensitive (event_widget)))
  {
      button->in_button = FALSE;
      geda_menu_button_leave (button);
  }

  return FALSE;
}

static void geda_menu_button_state_changed (GtkWidget *widget, GtkStateType  previous_state)
{
  GedaMenuButton     *button = GEDA_MENU_BUTTON (widget);
  GedaMenuButtonData *priv   = button->priv;

  if (!gtk_widget_is_sensitive (widget) && priv->menu) {
      geda_menu_shell_deactivate (GEDA_MENU_SHELL (priv->menu));
  }
}

/* BEGIN ------+------ Internal callbacks for main button -----+-------+-----*/

static void geda_menu_main_button_pressed (GtkButton *main_button, GedaMenuButton *button)
{
  geda_menu_button_pressed (button);
}

static void geda_menu_main_button_released (GtkButton *main_button, GedaMenuButton *button)
{
  geda_menu_button_released (button);
}

/* BEGIN ------+-------+-------^  Destructors  ^-------+-------+-------+-----*/

static void geda_menu_button_dispose (GObject *object)
{
  /*GedaMenuButton *button = GEDA_MENU_BUTTON (object);*/
  G_OBJECT_CLASS (geda_menu_button_parent_class)->dispose (object);
}

static void geda_menu_button_finalize (GObject *object)
{
  GedaMenuButton *button = GEDA_MENU_BUTTON (object);

  if (g_hash_table_remove (menu_button_hash, object)) {
    if (!g_hash_table_size (menu_button_hash)) {
      g_hash_table_destroy (menu_button_hash);
      menu_button_hash = NULL;
    }
  }

  GEDA_FREE (button->label_text);

  GEDA_FREE(button->priv);

  G_OBJECT_CLASS (geda_menu_button_parent_class)->finalize (object);
}

static void geda_menu_button_destroy (GtkObject *object)
{
  GedaMenuButton *button = GEDA_MENU_BUTTON (object);
  GedaMenu       *menu   = (GedaMenu*)button->priv->menu;

  if (menu) {

      g_signal_handlers_disconnect_by_func (menu,
                                            geda_menu_deactivate_cb,
                                            button);
      geda_menu_detach (menu);

      g_signal_handlers_disconnect_by_func (button->priv->arrow_button,
                                            arrow_button_toggled_cb,
                                            button);
      g_signal_handlers_disconnect_by_func (button->priv->arrow_button,
                                            arrow_button_press_event_cb,
                                            button);

      g_signal_handlers_disconnect_by_func (button->priv->button,
                                            geda_menu_main_button_pressed,
                                            button);
      g_signal_handlers_disconnect_by_func (button->priv->button,
                                            geda_menu_main_button_released,
                                            button);

      g_signal_handlers_disconnect_by_func (button,
                                            geda_menu_button_button_pressed,
                                            NULL);

      g_signal_handlers_disconnect_by_func (button,
                                            geda_menu_button_button_released,
                                            NULL);;

  }

#if GTK_MAJOR_VERSION < 3

  GTK_OBJECT_CLASS (geda_menu_button_parent_class)->destroy (object);

#else

  GTK_WIDGET_CLASS (geda_menu_button_parent_class)->destroy (object);

#endif

}

/* END -------------------------- Destructors ------------------------------ */

/* BEGIN ------+-------+-------^ Constructors  ^-------+-------+-------+-----*/

/*!
 * \brief Type instance initializer for GedaMenuButton
 * \par Function Description
 *  Type instance initializer for GedaMenuButton, initializes a new empty
 *  GedaMenuButton object.
 *
 * \param [in] instance The GedaMenuButton structure being initialized,
 * \param [in] class    The GedaMenuButton class we are initializing.
 */
static void geda_menu_button_init (GTypeInstance *instance, void *class)
{
  GedaMenuButton *button;
  GtkWidget      *box;
  GtkWidget      *arrow;
  GtkWidget      *arrow_button;
  GtkWidget      *main_button;

  button       = (GedaMenuButton*)instance;
  button->priv = GEDA_MEM_ALLOC0 (sizeof(GedaMenuButtonData));

  box          = gtk_hbox_new (FALSE, 0);
  main_button  = gtk_button_new ();

  arrow_button = gtk_toggle_button_new ();
  arrow        = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_NONE);

  geda_container_add (box, main_button);

  geda_container_add (arrow_button, arrow);

  gtk_box_pack_end (GTK_BOX (box), arrow_button, FALSE, FALSE, 0);

  /* the arrow button is insentive until we set a menu */
  gtk_widget_set_sensitive (arrow_button, FALSE);

  gtk_widget_show_all (box);

  geda_container_add (button, box);

  button->priv->button       = main_button;
  button->priv->arrow        = arrow;
  button->priv->arrow_button = arrow_button;
  button->priv->box          = box;

  g_signal_connect (arrow_button, "toggled", G_CALLBACK (arrow_button_toggled_cb), button);

  g_signal_connect (arrow_button, "button-press-event", G_CALLBACK (arrow_button_press_event_cb), button);

  g_signal_handlers_disconnect_by_func (button, geda_menu_main_button_pressed, button);
  g_signal_handlers_disconnect_by_func (button, geda_menu_main_button_released, button);

  g_signal_connect (main_button,  "pressed",  G_CALLBACK (geda_menu_main_button_pressed), button);
  g_signal_connect (main_button,  "released", G_CALLBACK (geda_menu_main_button_released), button);

  if (!menu_button_hash) {
    menu_button_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (menu_button_hash, instance, instance);
}

/* END ------------------------- Constructors  ------------------------------*/

/* BEGIN ------+-------+-------  Initialization -------+-------+-------+-----*/

/*!
 * \brief GedaMenuButton Class Initializer
 * \par Function Description
 *  Function is called to initialize the class instance.
 *
 * \param [in] class    A GedaMenuButtonClass Object
 * \param [in] class_data GedaMenuButton structure associated with the class
 */
static void geda_menu_button_class_init (void *class, void *class_data)
{
  GedaMenuButtonClass *menu_button_class;
  GObjectClass        *object_class;
  GtkWidgetClass      *widget_class;

  menu_button_class = (GedaMenuButtonClass*)class;
  object_class      = (GObjectClass*)class;
  widget_class      = (GtkWidgetClass*)class;

  object_class->finalize           = geda_menu_button_finalize;
  object_class->dispose            = geda_menu_button_dispose;
  object_class->get_property       = geda_menu_button_get_property;
  object_class->set_property       = geda_menu_button_set_property;

  widget_class->button_press_event = geda_menu_button_button_press_event;
  widget_class->grab_broken_event  = geda_menu_button_grab_broken_event;
  widget_class->key_release_event  = geda_menu_button_key_release_event;

  widget_class->enter_notify_event = geda_menu_button_enter_notify;
  widget_class->leave_notify_event = geda_menu_button_leave_notify;
  widget_class->state_changed      = geda_menu_button_state_changed;

  menu_button_class->pressed       = geda_menu_button_button_pressed;
  menu_button_class->released      = geda_menu_button_button_released;
  menu_button_class->clicked       = geda_menu_button_button_clicked;
  menu_button_class->enter         = geda_menu_button_update_state;
  menu_button_class->leave         = geda_menu_button_update_state;
  menu_button_class->activate      = geda_menu_button_activate;

#if GTK_MAJOR_VERSION < 3

  GtkObjectClass *gtk_object_class;

  gtk_object_class                 = (GtkObjectClass*)class;
  gtk_object_class->destroy        = geda_menu_button_destroy;

#else

  widget_class->destroy            = geda_menu_button_destroy;

#endif

  geda_menu_button_parent_class    = g_type_class_peek_parent (class);

  g_object_class_install_property (object_class,
                                   PROP_MENU,
                                   g_param_spec_object ("menu",
                                                      _("Menu"),
                                                      _("The drop-down menu"),
                                                        GTK_TYPE_MENU,
                                                        G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_LABEL,
                                   g_param_spec_string ("label",
                                                      _("Label"),
                                                      _("Text of the label widget inside the button, if the button contains a label widget"),
                                                        NULL,
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class,
                                   PROP_USE_STOCK,
                                   g_param_spec_boolean ("use-stock",
                                                       _("Use stock"),
                                                       _("If set, the label is used to pick a stock item instead of being displayed"),
                                                        FALSE,
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
  /* set whether to grab focus when it is clicked with the mouse.*/
  g_object_class_install_property (object_class,
                                   PROP_FOCUS_ON_CLICK,
                                   g_param_spec_boolean ("focus-on-click",
                                                       _("Focus on click"),
                                                       _("Whether the button grabs focus when it is clicked with the mouse"),
                                                         TRUE,
                                                         G_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_RELIEF,
                                   g_param_spec_enum ("relief",
                                                    _("Border relief"),
                                                    _("The border relief style"),
                                                      GTK_TYPE_RELIEF_STYLE,
                                                      GTK_RELIEF_NORMAL,
                                                      G_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_MENU_RELIEF,
                                   g_param_spec_enum ("menu-relief",
                                                    _("Arrow Border relief"),
                                                    _("The arrow border relief style"),
                                                      GTK_TYPE_RELIEF_STYLE,
                                                      GTK_RELIEF_NONE,
                                                      G_PARAM_READWRITE));

  /* The child widget to appear next to the button text. */
  g_object_class_install_property (object_class,
                                   PROP_IMAGE,
                                   g_param_spec_object ("image",
                                                      _("Image widget"),
                                                      _("Child widget to appear next to the button text"),
                                                        GTK_TYPE_WIDGET,
                                                        G_PARAM_READWRITE));

  /* The position of the image relative to the text inside the button.*/
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_POSITION,
                                   g_param_spec_enum ("image-position",
                                                    _("Image position"),
                                                    _("The position of the image relative to the text"),
                                                      GTK_TYPE_POSITION_TYPE,
                                                      GTK_POS_LEFT,
                                                      G_PARAM_READWRITE));

 /*! GtkButton::default-border:
   * \par
  * The "default-border" style property defines the extra space to add
  * around a button that can become the default widget of its window.
  * For more information about default widgets, see gtk_widget_grab_default().
  */

  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_boxed ("default-border",
                                                             _("Default Spacing"),
                                                             _("Extra space to add for GTK_CAN_DEFAULT buttons"),
                                                               GTK_TYPE_BORDER,
                                                               G_PARAM_READABLE));

  /* default-outside-border:
   *
   * The "default-outside-border" style property defines the extra outside
   * space to add around a button that can become the default widget of its
   * window. Extra outside space is always drawn outside the button border.
   * For more information about default widgets, see gtk_widget_grab_default().
   */
  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_boxed ("default-outside-border",
                                                             _("Default Outside Spacing"),
                                                             _("Extra space to add for GTK_CAN_DEFAULT buttons that is always drawn outside the border"),
                                                               GTK_TYPE_BORDER,
                                                               G_PARAM_READABLE));

  /* inner-border:
   *
   * Sets the border between the button edges and child.
   */
  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_boxed ("inner-border",
                                                             _("Inner Border"),
                                                             _("Border between button edges and child."),
                                                               GTK_TYPE_BORDER,
                                                               G_PARAM_READABLE));
  /* signals */

  GedaType type = geda_menu_button_get_type();

  /* Button::pressed:
   * button: the object that received the signal
   *
   * Emitted when the button is pressed.
   * (Use the GtkWidget::button-press-event signal.)
   */
  signals[PRESSED] =  g_signal_new ("pressed", type,
                                    G_SIGNAL_RUN_FIRST,
                                    G_STRUCT_OFFSET (GedaMenuButtonClass, pressed),
                                    NULL, NULL,
                                    geda_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  /* Button::released:
   * button: the object that received the signal
   *
   * Emitted when the button is released.
   * (Use the GtkWidget::button-release-event signal.)
   */
  signals[RELEASED] = g_signal_new ("released", type,
                                    G_SIGNAL_RUN_FIRST,
                                    G_STRUCT_OFFSET (GedaMenuButtonClass, released),
                                    NULL, NULL,
                                    geda_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  /* Button::clicked:
   * button: the object that received the signal
   *
   * Emitted when the button is pressed.
   * (Use the GtkWidget::button-press-event signal.)
   */
  signals[CLICKED] =  g_signal_new ("clicked", type,
                                    G_SIGNAL_RUN_FIRST,
                                    G_STRUCT_OFFSET (GedaMenuButtonClass, clicked),
                                    NULL, NULL,
                                    geda_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  /* Button::enter:
   * button: the object that received the signal
   *
   * Emitted when the pointer enters the button.
   * (Use the GtkWidget::enter-notify-event signal.)
   */
  signals[ENTER] = g_signal_new ("enter", type,
                                 G_SIGNAL_RUN_FIRST,
                                 G_STRUCT_OFFSET (GedaMenuButtonClass, enter),
                                 NULL, NULL,
                                 geda_marshal_VOID__VOID,
                                 G_TYPE_NONE, 0);

  /* Button::leave:
   * button: the object that received the signal
   *
   * Emitted when the pointer leaves the button.
   * (Use the GtkWidget::leave-notify-event signal.)
   */
  signals[LEAVE] = g_signal_new ("leave", type,
                                 G_SIGNAL_RUN_FIRST,
                                 G_STRUCT_OFFSET (GedaMenuButtonClass, leave),
                                 NULL, NULL,
                                 geda_marshal_VOID__VOID,
                                 G_TYPE_NONE, 0);

  /*!
   * GtkButton::activate:
   * \par
   * The activate signal from GtkButton is an action signal and
   * emitting it causes the button to animate press then release.
   * Applications should never connect to this signal, but use the
   * GedaMenuButton::clicked signal.
   *
   * param widget: the object which received the signal.
   */
  signals[ACTIVATE] = g_signal_new ("activate", type,
                                    G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
                                    G_STRUCT_OFFSET (GedaMenuButtonClass, activate),
                                    NULL, NULL,
                                    geda_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  widget_class->activate_signal = signals[ACTIVATE];

  /*! GedaMenuButton::show-menu:
   * \par
   * The ::show-menu signal is emitted before the menu is shown.
   *
   * It can be used to populate the menu on demand, using
   * geda_menu_button_get_menu().

   * Note that even if you populate the menu dynamically in this way,
   * you must set an empty menu on the #GedaMenuButton beforehand,
   * since the arrow is made insensitive if the menu is not set.
   *
   * param button: the object on which the signal is emitted.
   */
  signals[SHOW_MENU] = g_signal_new ("show-menu", type,
                         G_SIGNAL_RUN_FIRST,
                         G_STRUCT_OFFSET (GedaMenuButtonClass, show_menu),
                         NULL, NULL,
                         geda_marshal_VOID__VOID,
                         G_TYPE_NONE, 0);
}

/*!
 * \brief Retrieve GedaMenuButton's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaMenuButton Type identifier. When
 *  first called, the function registers a #GedaMenuButton in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaMenuButton and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaMenuButton.
 */
GedaType geda_menu_button_get_type (void)
{
  static volatile GedaType geda_menu_button_type = 0;

  if (g_once_init_enter (&geda_menu_button_type)) {

    static const GTypeInfo info = {
      sizeof(GedaMenuButtonClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_menu_button_class_init,     /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaMenuButton),
      0,                               /* n_preallocs         */
      geda_menu_button_init            /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaMenuButton");
    type   = g_type_register_static (GTK_TYPE_EVENT_BOX, string, &info, 0);

    const GInterfaceInfo interface_info = {
      (GInterfaceInitFunc) geda_menu_button_buildable_interface_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_BUILDABLE, &interface_info);

    g_once_init_leave (&geda_menu_button_type, type);
  }

  return geda_menu_button_type;
}

/*!
 * \brief Check if an object is a GedaMenuButton
 * \par Function Description
 *  Determines if \a menu_button is valid by verifying \a menu_button
 *  is included in the hash table of GedaMenuButton objects.
 *
 * \return TRUE if \a menu_button is a valid GedaMenuButton
 */
bool is_a_geda_menu_button (GedaMenuButton *menu_button)
{
  if ((menu_button != NULL) && (menu_button_hash != NULL)) {
    return g_hash_table_lookup(menu_button_hash, menu_button) ? TRUE : FALSE;
  }
  return FALSE;
}

/* END ------------------------- Initialization ---------------------------- */

/* BEGIN ------+-------+-------^    Sub-Menu   ^-------+-------+-------+-----*/

/*! \internal Callback for the "deactivate" signal on the pop-up menu.
 *  This is used to unset the state of the toggle button when the pop-up
 *  menu disappears.
 */
static int geda_menu_deactivate_cb (GedaMenuShell *menu_shell, GedaMenuButton *button)
{
  GedaMenuButtonData *priv = button->priv;

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->arrow_button), FALSE);

  return TRUE;
}

static void menu_detacher (GtkWidget *widget, GedaMenu *menu)
{
  GedaMenuButtonData *priv = GEDA_MENU_BUTTON (widget)->priv;

  g_return_if_fail (priv->menu == (GtkWidget*) menu);

  priv->menu = NULL;
}

/* Helper call by:
 *   arrow_button_toggled_cb
 *   arrow_button_press_event_cb
 */
static void popup_menu_under_arrow (GedaMenuButton *button, GdkEventButton *event)
{
  GedaMenuButtonData *priv = button->priv;

  g_signal_emit (button, signals[SHOW_MENU], 0);

  if (!priv->menu)
    return;

  geda_menu_popup (GEDA_MENU(priv->menu), NULL, NULL,
                   NULL,
                   button,
                   event ? event->button : 0,
                   event ? event->time : gtk_get_current_event_time ());
}

/*!
 * \brief On Arrow Button "toggled" Callback
 * \par Function Description
 *  This function is called when the arrow_button was toggled
 *  by the user.
 *
 * \param[in] togglebutton Is the drop-down arrow button
 * \param[in] button       GedaMenuButton.
 */
static void arrow_button_toggled_cb (GtkToggleButton *togglebutton,
                                     GedaMenuButton  *button)
{
  GedaMenuButtonData *priv = button->priv;

  if (!priv->menu)
    return;

  if (gtk_toggle_button_get_active (togglebutton) &&
     !gtk_widget_get_visible (GTK_WIDGET(priv->menu)))
  {
      /* we get here only when the menu is activated by a key
       * press, so that we can select the first menu item */
      popup_menu_under_arrow (button, NULL);
      geda_menu_shell_select_first (GEDA_MENU_SHELL(priv->menu), FALSE);
  }
}

/*!
 * \brief On Arrow Button "button-press-event" Callback
 * \par Function Description
 *  This function is called when the arrow_button was selected
 *  using a pointer button press.
 *
 * \param[in] widget  The drop-down arrow togglebutton
 * \param[in] event   GdkEventButton event structure.
 * \param[in] button  GedaMenuButton.
 */
static bool arrow_button_press_event_cb (GtkWidget      *widget,
                                         GdkEventButton *event,
                                         GedaMenuButton *button)
{
  if (event->button == 1) {

      popup_menu_under_arrow (button, event);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(widget), TRUE);

      return TRUE;
  }
  else  {
      return FALSE;
  }
}

/*!
 * \brief Set the Menu Widget for GedaMenuButton object
 * \par Function Description
 *  Sets the GedaMenu that is popped up when the user clicks on the arrow.
 *  If menu is NULL, the arrow button becomes insensitive.
 *
 * \param [in] button a #GedaMenuButton
 * \param [in] menu   the GedaMenu associated with #GedaMenuButton
 */
void geda_menu_button_set_menu (GedaMenuButton *button, GtkWidget *menu)
{
  GedaMenuButtonData *priv;

  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  g_return_if_fail (GEDA_IS_MENU (menu) || menu == NULL);

  priv = button->priv;

  if (priv->menu != menu) {

    if (priv->menu && gtk_widget_get_visible (GTK_WIDGET (priv->menu)))
      geda_menu_shell_deactivate (GEDA_MENU_SHELL (priv->menu));

    if (priv->menu) {

      g_signal_handlers_disconnect_by_func (priv->menu,
                                            geda_menu_deactivate_cb,
                                            button);
      geda_menu_detach ((GedaMenu*)priv->menu);
    }

    priv->menu = menu;

    if (priv->menu) {

      geda_menu_attach_to_widget ((GedaMenu*)priv->menu, GTK_WIDGET (button),
                                 menu_detacher);

      gtk_widget_set_sensitive (priv->arrow_button, TRUE);

      g_signal_connect (priv->menu, "deactivate",
                        G_CALLBACK (geda_menu_deactivate_cb), button);
    }
    else {
      gtk_widget_set_sensitive (priv->arrow_button, FALSE);
    }
  }

  GEDA_OBJECT_NOTIFY (button, "menu");
}

/*!
 * \brief Get the Menu Widget for GedaMenuButton object
 * \par Function Description
 *  Gets the GedaMenu associated with #GedaMenuButton.
 *
 * \param [in] button a #GedaMenuButton
 *
 * \returns the GedaMenu associated with #GedaMenuButton
 */
GtkWidget *geda_menu_button_get_menu (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), NULL);

  return GTK_WIDGET (button->priv->menu);
}
/* END -------------------------    Sub-Menu   ----------------------------- */

/* BEGIN ------+-------+-------^    Tooltips   ^-------+-------+-------+-----*/

/*!
 * \brief Set GedaMenuButton tooltip Text
 * \par Function Description
 *  Wrapper for gtk_widget_set_tooltip_text that accept a
 *  GedaMenuButton as an argument.
 */
void geda_menu_button_set_tooltip_text (GedaMenuButton *button,
                                        const char *tip_text)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_widget_set_tooltip_text((GtkWidget*) button,tip_text);
}

/*!
 * \brief Set GedaMenuButton Arrow tooltip
 * \par Function Description
 *  Sets the tooltip for the arrow of the GedaMenuButton.
 *
 * \param [in] button      A GedaMenuButton Object
 * \param [in] tooltips    GtkTooltips object
 * \param [in] tip_text    text to be used as tooltip text for the button
 * \param [in] tip_private additional information that may be useful to the user
 */
void geda_menu_button_set_arrow_tooltip (GedaMenuButton *button,
                                         GtkTooltips    *tooltips,
                                         const char     *tip_text,
                                         const char     *tip_private)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_tooltips_set_tip (tooltips, button->priv->arrow_button, tip_text,
                        tip_private);
}

/*!
 * \brief Set GedaMenuButton Arrow tooltip Text
 * \par Function Description
 * Sets the tooltip text to be used as tooltip for the arrow button
 * which pops up the menu.
 *
 * \param [in] button  A GedaMenuButton Object
 * \param [in] text    text to be used as tooltip text for the arrow button
 */
void geda_menu_button_set_arrow_tooltip_text(GedaMenuButton *button,
                                             const char     *text)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_widget_set_tooltip_text (button->priv->arrow_button, text);
}

/*!
 * \brief Set GedaMenuButton Arrow tooltip Markup
 * \par Function Description
 * Sets the tooltip markup text to be used as tooltip for the arrow
 * button which pops up the menu.
 *
 * \param [in] button  A GedaMenuButton Object
 * \param [in] markup  text with markup to be used as tooltip text
 */
void geda_menu_button_set_arrow_tooltip_markup (GedaMenuButton *button,
                                                const char     *markup)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_widget_set_tooltip_markup (button->priv->arrow_button, markup);
}

/* END -------------------------    Tooltips   ----------------------------- */

/*-----+-------+-------+-------^-------+-------^-------+-------+-------+-----*/

/*!
 * \brief New GedaMenuButton with Icon and Label
 * \par Function Description
 *  Creates a new GedaMenuButton using \a icon_widget as the icon
 *  and \a label as the label.
 *
 * \returns the new GedaMenuButton as a Widget
 */
GtkWidget *geda_menu_button_new(GtkWidget *icon_widget, const char *label)
{
  GtkWidget *button;

  button = g_object_new (GEDA_TYPE_MENU_BUTTON, "border-width", 0,
                                                "menu-relief", GTK_RELIEF_NONE,
                                                NULL);
  if (label)
    geda_menu_button_set_label((GedaMenuButton*)button, label);

  if (icon_widget)
     geda_menu_button_set_image((GedaMenuButton*)button, icon_widget);

  return button;
}

/*!
 * \brief New GedaMenuButton with Icon Image
 * \par Function Description
 * Creates a new Menu Button containing an icon from the current icon
 * theme. If the icon name isn’t known, a “broken image” icon will be
 * displayed instead. If the current icon theme is changed, the icon
 * will be updated appropriately.
 *
 * This is a convenience wrapper for gtk_image_new_from_icon_name and
 * geda_menu_button_new.
 *
 * \param [in] icon_name  Name of a known icon image
 *
 * \returns a new #GedaMenuButton displaying the themed icon
 */
GtkWidget *geda_menu_button_new_from_icon_name (const char *icon_name)
{
  GtkWidget *button;
  GtkWidget *image;

  image  = gtk_image_new_from_icon_name (icon_name, GTK_ICON_SIZE_MENU);
  button = g_object_new (GEDA_TYPE_MENU_BUTTON, "image", image, NULL);

  return button;
}

/*!
 * \brief New GedaMenuButton from Stock Id
 * \par Function Description
 * Creates a new GedaMenuButton. The new GedaMenuButton will contain
 * an icon and label from the stock item indicated by \a stock_id.
 *
 * \param [in] stock_id  Name of a stock item
 *
 * \returns the new GedaMenuButton as a Widget
 */
GtkWidget *geda_menu_button_new_from_stock (const char *stock_id)
{
  GtkWidget *button;

  g_return_val_if_fail (stock_id != NULL, NULL);

  button = g_object_new (GEDA_TYPE_MENU_BUTTON, "stock-id", stock_id,
                                                "border-width", 0,
                                                "menu-relief", GTK_RELIEF_NONE,
                                                 NULL);
  return button;
}

/*!
 * \brief New GedaMenuButton with Mnemonic Label
 * \par Function Description
 *  Creates a new GedaMenuButton using \a label as the label.
 *
 * \returns the new GedaMenuButton as a Widget
 */
GtkWidget *geda_menu_button_new_with_mnemonic(const char *label)
{
  return g_object_new (GEDA_TYPE_MENU_BUTTON, "label", label,
                                              "menu-relief", GTK_RELIEF_NONE,
                                              "use-underline", TRUE,  NULL);
}

/** @} end group geda-menu-button */
