/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Date: March 31, 2013
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <geda.h>

#include <glib.h>
#include <string.h>

#include <gtk/gtk.h>
#include <gtk/gtkprivate.h>

#include "geda_menu_button.h"

#include "gettext.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/**
 * \brief GedaMenuButton - A Button Widget for Menus
 * \par
 * A GedaMenuButton is a Button objects are used on toolbars or menus. The
 * GedaMenuButton is a replacement for the GtkMenuButton because the GtkMenu
 * Button has a display issue related to the "themeing" and pre-light, forgot
 * the details. And GtkMenuButton uses a GtkLabel, which valgrind reports as
 * a memory leak after GTK over-rides the font to LibFontConfig's best match
 * of the default "theme" font but neither Pango nor LibFontConfig free the
 * (I think) description.
 *
 * \defgroup GedaMenuButton Menu Button
 * @{
 */

/* Time out before giving up on getting a key release when animating the close button.
 */
#define ACTIVATE_TIMEOUT 250

#define GEDA_MENU_BUTTON_GET_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE ((obj), GEDA_TYPE_MENU_BUTTON, GedaMenuButtonPrivate))

struct _GedaMenuButtonPrivate
{
  GtkWidget *button;
  GtkWidget *arrow;
  GtkWidget *arrow_button;
  GtkWidget *box;
  GtkWidget *menu;

  unsigned int has_grab;
  guint32      grab_time;

  GtkAction   *action;
};

static const GtkBorder default_default_border         = { 1, 1, 1, 1 };
static const GtkBorder default_default_outside_border = { 0, 0, 0, 0 };
static const GtkBorder default_inner_border           = { 1, 1, 1, 1 };

static void geda_menu_button_dispose (GObject *object);
static void geda_menu_button_destroy (GtkObject *object);

static int  geda_menu_deactivate_cb(GtkMenuShell *menu_shell, GedaMenuButton *button);

static bool arrow_button_press_event_cb        (GtkWidget         *widget,
                                                       GdkEventButton    *event,
                                                       GedaMenuButton *button);
static void arrow_button_toggled_cb                   (GtkToggleButton   *togglebutton,
                                                       GedaMenuButton *button);

static void geda_menu_button_buildable_interface_init (GtkBuildableIface   *iface);
static void geda_menu_button_buildable_add_child      (GtkBuildable        *buildable,
                                                       GtkBuilder          *builder,
                                                       GObject             *child,
                                                       const char          *type);

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

static GtkBuildableIface *parent_buildable_iface;

G_DEFINE_TYPE_WITH_CODE (GedaMenuButton, geda_menu_button, GTK_TYPE_EVENT_BOX,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_BUILDABLE,
                                                geda_menu_button_buildable_interface_init))

/* BEGIN ------+-------+------ Property Handlers ------+-------+-------+-----*/
void
geda_menu_button_set_style (GedaMenuButton *button, GtkStyle *new_style)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  gtk_widget_set_style( GTK_WIDGET (button->priv->button), new_style);
  gtk_widget_set_style( GTK_WIDGET (button->priv->arrow_button), new_style);
}

void
geda_menu_button_set_relief (GedaMenuButton *button, GtkReliefStyle new_relief)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  button->relief = new_relief;
  gtk_button_set_relief (GTK_BUTTON (button->priv->button), new_relief);
}

GtkReliefStyle
geda_menu_button_get_relief (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), GTK_RELIEF_NORMAL);
  return button->relief;
}
void
geda_menu_arrow_set_relief (GedaMenuButton *button, GtkReliefStyle new_relief)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  button->menu_relief = new_relief;
  gtk_button_set_relief (GTK_BUTTON (button->priv->arrow_button), new_relief);

}

GtkReliefStyle
geda_menu_arrow_get_relief (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), GTK_RELIEF_NORMAL);

  return button->menu_relief;

}

/*! \brief Set the Label Text on a GedaMenuButton.
 *  \par Function Description
 * Sets the text of the label of the button to value pointed to by
 * label. This text is also used to select the stock item if
 * geda_menu_button_set_use_stock() is used.
 *
 * This will also clear any previously set labels.
 */
void geda_menu_button_set_label (GedaMenuButton *button, const char *label)
{
  g_return_if_fail ( GEDA_IS_MENU_BUTTON (button) );
  gtk_button_set_label( GTK_BUTTON(button->priv->button), label);
}

/*! \brief Get the Label Text on a GedaMenuButton.
 *  \par Function Description
 * This function retrieves the text from the label of the button.
 * If the label text has not been set the return value will be NULL.
 * This will could be the case if an empty menu button was created
 * with gtk_button_new() to use as a container.
 *
 * Return value: The text of the label widget. This string is owned
 * by the widget and must not be modified or freed.
 */
const char *geda_menu_button_get_label (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), NULL);
  return gtk_button_get_label(GTK_BUTTON(button->priv->button));
}

/*! \brief Get the Label Text on a GedaMenuButton.
 *
 *  \par Function Description
 * If true, an underline in the text of the button label indicates
 * the next character should be used for the mnemonic accelerator key.
 *
 * \param [in] button: a GedaMenuButton
 * \param [in] @use_underline: %TRUE if underlines in the text indicate mnemonics
 */
void
geda_menu_button_set_use_underline (GedaMenuButton *button, bool use_underline)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_use_underline(GTK_BUTTON(button->priv->button), use_underline);
}

/*! \brief Get GedaMenuButton Mnemonic Interpretation Setting.
 *  \par Function Description
 *
 * Returns whether an embedded underline in the button label indicates a
 * mnemonic. See gtk_button_set_use_underline ().
 *
 * \param [in] button: a GedaMenuButton
 *
 * Return value: %TRUE if an embedded underline in the button label
 *               indicates the mnemonic accelerator keys.
 **/
bool
geda_menu_button_get_use_underline (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), FALSE);
  return gtk_button_get_use_underline( GTK_BUTTON(button->priv->button));
}

/*! \brief Set a GedaMenuButton to use stock item set by label text.
 *  \par Function Description
 * If %TRUE, the label set on the button is used as a stock id to
 * select the stock item for the button.
 */
void geda_menu_button_set_use_stock (GedaMenuButton *button, bool use_stock)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_use_stock(GTK_BUTTON(button->priv->button), use_stock);
}

/*! \brief Query to determine if GedaMenuButton uses stock item.
 *  \par Function Description
 *   Returns whether the button label is a stock item.
 *
 * \param [in] button: a GedaMenuButton
 *
 * Return value: %TRUE if the button label is used to
 *               select a stock item instead of being
 *               used directly as the label text.
 */
bool geda_menu_button_get_use_stock (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), FALSE);
  return gtk_button_get_use_stock (GTK_BUTTON(button->priv->button));
}

/*! \brief Sets whether the GedaMenuButton primary  button  will grap focus.
 *  \par Function Description
 * Sets whether the button will grab focus when it is clicked with the mouse.
 * Making mouse clicks not grab focus is useful in places like toolbars where
 * you don't want the keyboard focus removed from the main area of the
 * application.
 *
 * \param [in] button: a GedaMenuButton
 * \param [in] focus_on_click: %TRUE if widget should grab focus
 */
void
geda_menu_button_set_focus_on_click (GedaMenuButton *button, bool focus_on_click)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_focus_on_click( GTK_BUTTON (button->priv->button) ,
                                 focus_on_click);
}

/*! \brief Returns whether a GedaMenuButton grabs focus.
 *  \par Function Description
 *   This function returns whether the main button grabs focus when
 * the button is clicked with the mouse.
 * See gtk_button_set_focus_on_click().
 *
 * Return value: %TRUE if the button grabs focus when it is clicked
 *                with the mouse.
 */
bool geda_menu_button_get_focus_on_click (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), FALSE);
  return gtk_button_get_focus_on_click( GTK_BUTTON (button->priv->button));
}

/*! \brief Sets the alignment of the child in a GedaMenuButton.
 *  \par Function Description
 * Sets the alignment of the child. This property has no effect unless
 * the child is a GtkMisc or a GtkAligment.
 *
 * \param [in] button: a GedaMenuButton
 * \param [in] xalign: the horizontal position of the child, 0.0 is left aligned,
 *                     1.0 is right aligned
 * \param [in] yalign: the vertical position of the child, 0.0 is top aligned,
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

/*! \brief  Gets the alignment of the child in a GedaMenuButton.
 *  \par Function Description
 *   Gets the alignment of the child in the button.
 *
 * \param [in] button:  button: a GedaMenuButton
 *
 * \param [out] xalign: return location for horizontal alignment
 * \param [out] yalign: return location for vertical alignment
 *
 */
void
geda_menu_button_get_alignment (GedaMenuButton *button,
                          float    *xalign,
                          float    *yalign)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  return gtk_button_get_alignment( GTK_BUTTON (button->priv->button),
                                   xalign, yalign);
}

/*! \brief Sets the alignment of the child in a GedaMenuButton.
 *  \par Function Description
 *
 * Set the image of button to the given widget. Note that
 * it depends on the GtkSettings:gtk-button-images setting whether the
 * image will be displayed or not, you don't have to call
 * gtk_widget_show() on image yourself.
 *
 * \param [in] button: The GedaMenuButton
 * \param [in] image:  a widget to set as the image for the button

 */
void
geda_menu_button_set_image (GedaMenuButton *button, GtkWidget *image)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_image (GTK_BUTTON (button->priv->button), image);
}

/*! \brief Sets the alignment of the child in a GedaMenuButton.
 *  \par Function Description
 * Gets the widget that is currenty set as the image of button.
 * This may have been explicitly set by gtk_button_set_image()
 * or constructed by gtk_button_new_from_stock().
 *
 * \param [in] button: The GedaMenuButton
 *
 * Return value: a GtkWidget or %NULL in case there is no image
 */
GtkWidget *geda_menu_button_get_image (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), NULL);
  return gtk_button_get_image (GTK_BUTTON (button->priv->button));
}

/*! \brief Sets the alignment of the child in a GedaMenuButton.
 *  \par Function Description
 *
 * Sets the position of the image relative to the text
 * inside the button.
 */
void
geda_menu_button_set_image_position (GedaMenuButton *button, GtkPositionType position)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_button_set_image_position ( GTK_BUTTON (button->priv->button), position);
}

/*! \brief Sets the alignment of the child in a GedaMenuButton.
 *  \par Function Description
 *
 * Gets the position of the image relative to the text
 * inside the button.
 *
 * Return value: the position
 *
 */
GtkPositionType
geda_menu_button_get_image_position (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), GTK_POS_LEFT);
  return gtk_button_get_image_position (GTK_BUTTON (button->priv->button));
}

/*! \brief Sets the alignment of the child in a GedaMenuButton.
 *  \par Function Description
 *
 * Returns the button's event window if it is realized, %NULL otherwise.
 * This function should be rarely needed.
 *
 * Return value:  button's event window.
 *
 */
GdkWindow*
geda_menu_button_get_event_window (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), NULL);
  return gtk_button_get_event_window (GTK_BUTTON (button->priv->button));
}

/*! \brief  Set Properties of a GedaMenuButton.
 *  \par Function Description
 *   This function handled the gobject properties setters.
 *
 * \param [in] object:  a GedaMenuButton
 * \param [in] prop_id: The enumerated property ID
 * \param [in] value:   The value to set the property
 * \param [in] pspec:   The parameter specifications for the property
 */
static void
geda_menu_button_set_property (GObject      *object,
                               unsigned int  prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
  GedaMenuButton *button      = GEDA_MENU_BUTTON (object);
  GedaMenuButtonPrivate *priv = button->priv;
  GtkButton *butt             = GTK_BUTTON(priv->button);

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
/*! \brief  Get Properties of a GedaMenuButton.
 *  \par Function Description
 *   This function handled the gobject properties request.
 *
 * \param [in] object:    a GedaMenuButton
 * \param [in] prop_id:   The enumerated property ID
 * \param [out] value:    The variable that will be set to the property value
 * \param [in] pspec:     The parameter specifications for the property
 */
static void
geda_menu_button_get_property (GObject    *object,
                               guint       prop_id,
                               GValue     *value,
                               GParamSpec *pspec)
{
  GedaMenuButton *button      = GEDA_MENU_BUTTON (object);
  GedaMenuButtonPrivate *priv = button->priv;
  GtkButton *butt             = GTK_BUTTON(priv->button);

  switch (prop_id)
    {
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

/*! \brief  GedaMenuButton (Main) Button "pressed" Signal Emitter.
 *  \par Function Description
 *   This function requests the "pressed" signal be emitted from the Widget.
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
void geda_menu_button_pressed (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[PRESSED], 0);
}

/*! \brief  GedaMenuButton (Main) Button "released" Signal Emitter.
 *  \par Function Description
 *   This function requests the "released" signal be emitted from the Widget.
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
void geda_menu_button_released (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[RELEASED], 0);
}

/*! \brief  GedaMenuButton (Main) Button "clicked" Signal Emitter.
 *  \par Function Description
 *   This function requests the "clicked" signal be emitted from the Widget.
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
void geda_menu_button_clicked (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[CLICKED], 0);
}

/*! \brief  GedaMenuButton "enter" Signal Emitter.
 *  \par Function Description
 *   This function requests the "enter" signal be emitted from
 *   the Widget, which occurs when the mouse enters the widget's
 *   boundaries.
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
void geda_menu_button_enter (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[ENTER], 0);
}

/*! \brief  GedaMenuButton "leave" Signal Emitter.
 *  \par Function Description
 *   This function requests the "leave" signal be emitted from
 *   the Widget, which occurs when the mouse leaves the widget's
 *   boundaries.
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
void geda_menu_button_leave (GedaMenuButton *button)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));

  g_signal_emit (button, signals[LEAVE], 0);
}

/* END --------+-------+-------^    Emitters   ^-------+-------+-------+-----*/

/* BEGIN ------+-------+------- Signal Handlers -------+-------+-------+-----*/

/*! \brief  GedaMenuButton Update State Signal Handler.
 *  \par Function Description
 *   This function updates the visual appearance of the button.
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
static void
geda_menu_button_update_state (GedaMenuButton *button)
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

/*! \brief  GedaMenuButton Process the "clicked" Signal internally.
 *  \par Function Description
 *   This function is called with the "clicked" is received
 *   from the button. The function check and executes the action
 *   if an action exist.
 *
 *  TODO: Add action property handler?
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
static void
geda_menu_button_button_clicked (GedaMenuButton *button)
{
  GedaMenuButtonPrivate *priv = GEDA_MENU_BUTTON_GET_PRIVATE (button);

  if (priv->action)
    gtk_action_activate (priv->action);

}

/*! \brief  GedaMenuButton Process the "pressed" Signal internally.
 *  \par Function Description
 *   This function is called with the "pressed" signal is received
 *   from the button to update the visual appearance if the "down
 *   time" has expired.
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
static void
geda_menu_button_button_pressed (GedaMenuButton *button)
{

  if (button->activate_timeout)
    return;

  button->button_down = TRUE;
  geda_menu_button_update_state (button);
}

/*! \brief  GedaMenuButton Process the "released" Signal internally.
 *  \par Function Description
 *   This function is called with the "released" signal is received
 *   from the button to generate the "clicked" signal and to update
 *   the visual appearance if the "down time" has expired.
 *
 * \param [in] button:       a GedaMenuButton
 *
 */
static void
geda_menu_button_button_released (GedaMenuButton *button)
{

  if (button->button_down)
    {
      button->button_down = FALSE;

      if (button->activate_timeout)
        return;

      if (button->in_button)
        geda_menu_button_clicked (button);

      geda_menu_button_update_state (button);
    }
}

static void
geda_menu_button_finish_activate (GedaMenuButton *button, bool do_it)
{
  GtkWidget *widget = GTK_WIDGET (button);
  GedaMenuButtonPrivate *priv;

  priv = GEDA_MENU_BUTTON_GET_PRIVATE (button);

  g_source_remove (button->activate_timeout);
  button->activate_timeout = 0;

  if (priv->has_grab)
    {
      gdk_display_keyboard_ungrab (gtk_widget_get_display (widget),
                                   priv->grab_time);
    }
  gtk_grab_remove (widget);

  button->button_down = FALSE;

  geda_menu_button_update_state (button);
  gtk_widget_queue_draw (GTK_WIDGET (button));

  if (do_it)
    geda_menu_button_clicked (button);
}

static bool
button_activate_timeout (gpointer data)
{
  geda_menu_button_finish_activate (data, TRUE);

  return FALSE;
}

static void
geda_menu_button_activate (GedaMenuButton *button)
{
  GtkWidget *widget = GTK_WIDGET (button);
  GedaMenuButtonPrivate *priv;
  guint32 time;

  priv = GEDA_MENU_BUTTON_GET_PRIVATE (button);

  if (gtk_widget_get_realized (widget) && !button->activate_timeout)
    {
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

static bool
geda_menu_button_button_press_event (GtkWidget *widget, GdkEventButton *event)
{
  GedaMenuButton *button;

  if (event->type == GDK_BUTTON_PRESS)
    {
      button = GEDA_MENU_BUTTON (widget);

      if (button->focus_on_click && !gtk_widget_has_focus (widget))
        gtk_widget_grab_focus (widget);

      if (event->button == 1)
        geda_menu_button_released (button);
    }

  return TRUE;
}

static bool
geda_menu_button_grab_broken_event (GtkWidget *widget, GdkEventGrabBroken *event)
{
  GedaMenuButton *button = GEDA_MENU_BUTTON (widget);
  bool save_in;

  /* Simulate a button release without the pointer in the button */
  if (button->button_down)
    {
      save_in = button->in_button;
      button->in_button = FALSE;
      geda_menu_button_released (button);
      if (save_in != button->in_button)
        {
          button->in_button = save_in;
          geda_menu_button_update_state (button);
        }
    }

  return TRUE;
}

static bool
geda_menu_button_key_release_event (GtkWidget *widget, GdkEventKey *event)
{
  GedaMenuButton *button = GEDA_MENU_BUTTON (widget);

  if (button->activate_timeout)
    {
      geda_menu_button_finish_activate (button, TRUE);
      return TRUE;
    }
  else if (GTK_WIDGET_CLASS (geda_menu_button_parent_class)->key_release_event)
    return GTK_WIDGET_CLASS (geda_menu_button_parent_class)->key_release_event (widget, event);
  else
    return FALSE;
}

static bool
geda_menu_button_enter_notify (GtkWidget *widget, GdkEventCrossing *event)
{
  GedaMenuButton *button;
  GtkWidget *event_widget;

  button = GEDA_MENU_BUTTON (widget);
  event_widget = gtk_get_event_widget ((GdkEvent*) event);

  if ((event_widget == widget) &&
      (event->detail != GDK_NOTIFY_INFERIOR))
    {
      button->in_button = TRUE;
      geda_menu_button_enter (button);
    }

  return FALSE;
}

static bool
geda_menu_button_leave_notify (GtkWidget *widget, GdkEventCrossing *event)
{
  GedaMenuButton *button;
  GtkWidget *event_widget;

  button = GEDA_MENU_BUTTON (widget);
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
static void
geda_menu_button_state_changed (GtkWidget *widget, GtkStateType  previous_state)
{
  GedaMenuButton *button = GEDA_MENU_BUTTON (widget);
  GedaMenuButtonPrivate *priv = button->priv;

  if (!gtk_widget_is_sensitive (widget) && priv->menu)
    {
      gtk_menu_shell_deactivate (GTK_MENU_SHELL (priv->menu));
    }
}

/* BEGIN ------+------ Internal callbacks for main button -----+-------+-----*/

static void
geda_menu_main_button_pressed (GtkButton *main_button, GedaMenuButton *button)
{
  geda_menu_button_pressed (button);
}
static void
geda_menu_main_button_released (GtkButton *main_button, GedaMenuButton *button)
{
  geda_menu_button_released (button);
}

/* BEGIN ------+-------+-------^  Destructors  ^-------+-------+-------+-----*/

static void geda_menu_button_dispose (GObject *object)
{
  /*GedaMenuButton *button = GEDA_MENU_BUTTON (object);*/
  G_OBJECT_CLASS (geda_menu_button_parent_class)->dispose (object);
}

static void
geda_menu_button_destroy (GtkObject *object)
{
  GedaMenuButton *button;
  GtkMenu   *menu;

  button = GEDA_MENU_BUTTON (object);

  menu = (GtkMenu*) button->priv->menu;

  if (menu)
    {
      g_signal_handlers_disconnect_by_func (menu,
                                            geda_menu_deactivate_cb,
                                            button);
      gtk_menu_detach (menu);

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

  GTK_OBJECT_CLASS (geda_menu_button_parent_class)->destroy (object);
}

/* END -------------------------- Destructors ------------------------------ */

/* BEGIN ------+-------+-------^ Constructors  ^-------+-------+-------+-----*/

static void geda_menu_button_init (GedaMenuButton *button)
{
  GtkWidget *box;
  GtkWidget *arrow;
  GtkWidget *arrow_button;
  GtkWidget *main_button;

  button->priv = G_TYPE_INSTANCE_GET_PRIVATE (button,
                                              GEDA_TYPE_MENU_BUTTON,
                                              GedaMenuButtonPrivate);

  box = gtk_hbox_new (FALSE, 0);

  main_button = gtk_button_new ();

  gtk_container_add (GTK_CONTAINER(box), main_button);

  arrow_button = gtk_toggle_button_new ();
  arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_NONE);
  gtk_container_add (GTK_CONTAINER (arrow_button), arrow);

  gtk_box_pack_end (GTK_BOX (box), arrow_button, FALSE, FALSE, 0);

  /* the arrow button is insentive until we set a menu */
  gtk_widget_set_sensitive (arrow_button, FALSE);

  gtk_widget_show_all (box);

  gtk_container_add (GTK_CONTAINER (button), box);

  button->priv->button          = main_button;
  button->priv->arrow           = arrow;
  button->priv->arrow_button    = arrow_button;
  button->priv->box             = box;

  g_signal_connect (arrow_button, "toggled", G_CALLBACK (arrow_button_toggled_cb), button);

  g_signal_connect (arrow_button, "button-press-event", G_CALLBACK (arrow_button_press_event_cb), button);

  g_signal_handlers_disconnect_by_func (button, geda_menu_main_button_pressed, button);
  g_signal_handlers_disconnect_by_func (button, geda_menu_main_button_released, button);

  g_signal_connect (main_button,  "pressed",  G_CALLBACK (geda_menu_main_button_pressed), button);
  g_signal_connect (main_button,  "released", G_CALLBACK (geda_menu_main_button_released), button);
}

/* END ------------------------- Constructors  ------------------------------*/

/* BEGIN ------+-------+-------  Initialization -------+-------+-------+-----*/

static void geda_menu_button_class_init (GedaMenuButtonClass *klass)
{
  GtkObjectClass   *gtk_object_class;
  GObjectClass     *object_class;
  GtkWidgetClass   *widget_class;

  gtk_object_class      = (GtkObjectClass *)klass;
  object_class          = (GObjectClass*)klass;
  widget_class          = (GtkWidgetClass*)klass;

  gtk_object_class->destroy            = geda_menu_button_destroy;

  object_class->dispose                = geda_menu_button_dispose;
  object_class->get_property           = geda_menu_button_get_property;
  object_class->set_property           = geda_menu_button_set_property;

  widget_class->button_press_event     = geda_menu_button_button_press_event;
  widget_class->grab_broken_event      = geda_menu_button_grab_broken_event;
  widget_class->key_release_event      = geda_menu_button_key_release_event;

  widget_class->enter_notify_event     = geda_menu_button_enter_notify;
  widget_class->leave_notify_event     = geda_menu_button_leave_notify;
  widget_class->state_changed          = geda_menu_button_state_changed;

  klass->pressed                       = geda_menu_button_button_pressed;
  klass->released                      = geda_menu_button_button_released;
  klass->clicked                       = geda_menu_button_button_clicked;
  klass->enter                         = geda_menu_button_update_state;
  klass->leave                         = geda_menu_button_update_state;
  klass->activate                      = geda_menu_button_activate;

  g_object_class_install_property (object_class,
                                   PROP_MENU,
                                   g_param_spec_object ("menu",
                                                      _("Menu"),
                                                      _("The dropdown menu"),
                                                        GTK_TYPE_MENU,
                                                        GTK_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_LABEL,
                                   g_param_spec_string ("label",
                                                      _("Label"),
                                                      _("Text of the label widget inside the button, if the button contains a label widget"),
                                                        NULL,
                                                        GTK_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (object_class,
                                   PROP_USE_STOCK,
                                   g_param_spec_boolean ("use-stock",
                                                       _("Use stock"),
                                                       _("If set, the label is used to pick a stock item instead of being displayed"),
                                                        FALSE,
                                                        GTK_PARAM_READWRITE | G_PARAM_CONSTRUCT));
  /* set whether to grab focus when it is clicked with the mouse.*/
  g_object_class_install_property (object_class,
                                   PROP_FOCUS_ON_CLICK,
                                   g_param_spec_boolean ("focus-on-click",
                                                       _("Focus on click"),
                                                       _("Whether the button grabs focus when it is clicked with the mouse"),
                                                         TRUE,
                                                         GTK_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_RELIEF,
                                   g_param_spec_enum ("relief",
                                                    _("Border relief"),
                                                    _("The border relief style"),
                                                      GTK_TYPE_RELIEF_STYLE,
                                                      GTK_RELIEF_NORMAL,
                                                      GTK_PARAM_READWRITE));

  g_object_class_install_property (object_class,
                                   PROP_MENU_RELIEF,
                                   g_param_spec_enum ("menu-relief",
                                                    _("Arrow Border relief"),
                                                    _("The arrow border relief style"),
                                                      GTK_TYPE_RELIEF_STYLE,
                                                      GTK_RELIEF_NONE,
                                                      GTK_PARAM_READWRITE));

  /* The child widget to appear next to the button text. */
  g_object_class_install_property (object_class,
                                   PROP_IMAGE,
                                   g_param_spec_object ("image",
                                                      _("Image widget"),
                                                      _("Child widget to appear next to the button text"),
                                                        GTK_TYPE_WIDGET,
                                                        GTK_PARAM_READWRITE));

  /* The position of the image relative to the text inside the button.*/
  g_object_class_install_property (object_class,
                                   PROP_IMAGE_POSITION,
                                   g_param_spec_enum ("image-position",
                                                    _("Image position"),
                                                    _("The position of the image relative to the text"),
                                                      GTK_TYPE_POSITION_TYPE,
                                                      GTK_POS_LEFT,
                                                      GTK_PARAM_READWRITE));

 /*! default-border:
  *
  * The "default-border" style property defines the extra space to add
  * around a button that can become the default widget of its window.
  * For more information about default widgets, see gtk_widget_grab_default().
  */

  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_boxed ("default-border",
                                                             _("Default Spacing"),
                                                             _("Extra space to add for GTK_CAN_DEFAULT buttons"),
                                                               GTK_TYPE_BORDER,
                                                               GTK_PARAM_READABLE));

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
                                                               GTK_PARAM_READABLE));

  /* inner-border:
   *
   * Sets the border between the button edges and child.
   */
  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_boxed ("inner-border",
                                                              _("Inner Border"),
                                                              _("Border between button edges and child."),
                                                               GTK_TYPE_BORDER,
                                                               GTK_PARAM_READABLE));

  /* Button::pressed:
   * button: the object that received the signal
   *
   * Emitted when the button is pressed.
   * (Use the GtkWidget::button-press-event signal.)
   */
  signals[PRESSED] =  g_signal_new ("pressed",
                                    G_OBJECT_CLASS_TYPE (object_class),
                                    G_SIGNAL_RUN_FIRST,
                                    G_STRUCT_OFFSET (GedaMenuButtonClass, pressed),
                                    NULL, NULL,
                                    g_cclosure_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  /* Button::released:
   * button: the object that received the signal
   *
   * Emitted when the button is released.
   * (Use the GtkWidget::button-release-event signal.)
   */
  signals[RELEASED] = g_signal_new ("released",
                                    G_OBJECT_CLASS_TYPE (object_class),
                                    G_SIGNAL_RUN_FIRST,
                                    G_STRUCT_OFFSET (GedaMenuButtonClass, released),
                                    NULL, NULL,
                                    g_cclosure_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  /* Button::clicked:
   * button: the object that received the signal
   *
   * Emitted when the button is pressed.
   * (Use the GtkWidget::button-press-event signal.)
   */
  signals[CLICKED] =  g_signal_new ("clicked",
                                    G_OBJECT_CLASS_TYPE (object_class),
                                    G_SIGNAL_RUN_FIRST,
                                    G_STRUCT_OFFSET (GedaMenuButtonClass, clicked),
                                    NULL, NULL,
                                    g_cclosure_marshal_VOID__VOID,
                                    G_TYPE_NONE, 0);

  /* Button::enter:
   * button: the object that received the signal
   *
   * Emitted when the pointer enters the button.
   * (Use the GtkWidget::enter-notify-event signal.)
   */
  signals[ENTER] = g_signal_new ("enter",
                                 G_OBJECT_CLASS_TYPE (object_class),
                                 G_SIGNAL_RUN_FIRST,
                                 G_STRUCT_OFFSET (GedaMenuButtonClass, enter),
                                 NULL, NULL,
                                 g_cclosure_marshal_VOID__VOID,
                                 G_TYPE_NONE, 0);

  /* Button::leave:
   * button: the object that received the signal
   *
   * Emitted when the pointer leaves the button.
   * (Use the GtkWidget::leave-notify-event signal.)
   */
  signals[LEAVE] = g_signal_new ("leave",
                                 G_OBJECT_CLASS_TYPE (object_class),
                                 G_SIGNAL_RUN_FIRST,
                                 G_STRUCT_OFFSET (GedaMenuButtonClass, leave),
                                 NULL, NULL,
                                 g_cclosure_marshal_VOID__VOID,
                                 G_TYPE_NONE, 0);

  /*GtkButton::activate:
   * @widget: the object which received the signal.
   *
   * The ::activate signal on GtkButton is an action signal and
   * emitting it causes the button to animate press then release.
   * Applications should never connect to this signal, but use the
   * GedaMenuButton::clicked signal.
   */
  signals[ACTIVATE] = g_signal_new ("activate",
                        G_OBJECT_CLASS_TYPE (object_class),
                        G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
                        G_STRUCT_OFFSET (GedaMenuButtonClass, activate),
                        NULL, NULL,
                        g_cclosure_marshal_VOID__VOID,
                        G_TYPE_NONE, 0);

  widget_class->activate_signal = signals[ACTIVATE];

  /* GedaMenuButton::show-menu:
   * button: the object on which the signal is emitted
   *
   * The ::show-menu signal is emitted before the menu is shown.
   *
   * It can be used to populate the menu on demand, using
   * geda_menu_button_get_menu().

   * Note that even if you populate the menu dynamically in this way,
   * you must set an empty menu on the #GedaMenuButton beforehand,
   * since the arrow is made insensitive if the menu is not set.
   */
  signals[SHOW_MENU] = g_signal_new ("show-menu",
                         G_OBJECT_CLASS_TYPE (klass),
                         G_SIGNAL_RUN_FIRST,
                         G_STRUCT_OFFSET (GedaMenuButtonClass, show_menu),
                         NULL, NULL,
                         g_cclosure_marshal_VOID__VOID,
                         G_TYPE_NONE, 0);

  g_type_class_add_private (object_class, sizeof (GedaMenuButtonPrivate));
}

/* END ------------------------- Initialization ---------------------------- */

/* BEGIN ------+-------+-------^    Sub-Menu   ^-------+-------+-------+-----*/

/* Callback for the "deactivate" signal on the pop-up menu.
 * This is used so that we unset the state of the toggle button
 * when the pop-up menu disappears.
 */
static int
geda_menu_deactivate_cb (GtkMenuShell *menu_shell, GedaMenuButton *button)
{
  GedaMenuButtonPrivate *priv = button->priv;

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->arrow_button), FALSE);

  return TRUE;
}

static void menu_detacher (GtkWidget *widget, GtkMenu *menu)
{
  GedaMenuButtonPrivate *priv = GEDA_MENU_BUTTON (widget)->priv;

  g_return_if_fail (priv->menu == (GtkWidget*) menu);

  priv->menu = NULL;
}

static void
popup_menu_under_arrow (GedaMenuButton *button, GdkEventButton *event)
{
  GedaMenuButtonPrivate *priv = button->priv;

  g_signal_emit (button, signals[SHOW_MENU], 0);

  if (!priv->menu)
    return;

  gtk_menu_popup (GTK_MENU(priv->menu), NULL, NULL,
                  NULL,
                  button,
                  event ? event->button : 0,
                  event ? event->time : gtk_get_current_event_time ());
}

static void
arrow_button_toggled_cb (GtkToggleButton   *togglebutton,
                         GedaMenuButton *button)
{
  GedaMenuButtonPrivate *priv = button->priv;

  if (!priv->menu)
    return;

  if (gtk_toggle_button_get_active (togglebutton) &&
      !gtk_widget_get_visible (GTK_WIDGET (priv->menu)))
    {
      /* we get here only when the menu is activated by a key
       * press, so that we can select the first menu item */
      popup_menu_under_arrow (button, NULL);
      gtk_menu_shell_select_first (GTK_MENU_SHELL (priv->menu), FALSE);
    }
}

static bool
arrow_button_press_event_cb (GtkWidget         *widget,
                                    GdkEventButton    *event,
                                    GedaMenuButton *button)
{
  if (event->button == 1)
    {
      popup_menu_under_arrow (button, event);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget), TRUE);

      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

static void
geda_menu_button_buildable_add_child (GtkBuildable *buildable,
                                      GtkBuilder   *builder,
                                      GObject      *child,
                                      const char   *type)
{
  if (type && strcmp (type, "menu") == 0)
    geda_menu_button_set_menu (GEDA_MENU_BUTTON (buildable), GTK_WIDGET (child));
  else
    parent_buildable_iface->add_child (buildable, builder, child, type);
}

static void
geda_menu_button_buildable_interface_init (GtkBuildableIface *iface)
{
  parent_buildable_iface = g_type_interface_peek_parent (iface);
  iface->add_child = geda_menu_button_buildable_add_child;
}

/*! \brief Set the Menu Widget for GedaMenuButton object
 *
 *  \par Function Description
 * Sets the GtkMenu that is popped up when the user clicks on the arrow.
 * If menu is NULL, the arrow button becomes insensitive.
 *
 *  \param [in] button: a #GedaMenuButton
 *  \param [in] menu:   the GtkMenu associated with #GedaMenuButton
 *
 */
void
geda_menu_button_set_menu (GedaMenuButton *button, GtkWidget *menu)
{
  GedaMenuButtonPrivate *priv;

  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  g_return_if_fail (GTK_IS_MENU (menu) || menu == NULL);

  priv = button->priv;

  if (priv->menu != menu)
    {
      if (priv->menu && gtk_widget_get_visible (GTK_WIDGET (priv->menu)))
        gtk_menu_shell_deactivate (GTK_MENU_SHELL (priv->menu));

      if (priv->menu)
        {
          g_signal_handlers_disconnect_by_func (priv->menu,
                                                geda_menu_deactivate_cb,
                                                button);
          gtk_menu_detach ((GtkMenu*)priv->menu);
        }

      priv->menu = menu;

      if (priv->menu)
        {
          gtk_menu_attach_to_widget ((GtkMenu*)priv->menu, GTK_WIDGET (button),
                                     menu_detacher);

          gtk_widget_set_sensitive (priv->arrow_button, TRUE);

          g_signal_connect (priv->menu, "deactivate",
                            G_CALLBACK (geda_menu_deactivate_cb), button);
        }
      else
       gtk_widget_set_sensitive (priv->arrow_button, FALSE);
    }

  g_object_notify (G_OBJECT (button), "menu");
}

/*! \brief Get the Menu Widget for GedaMenuButton object
 *
 *  \par Function Description
 *
 * Gets the GtkMenu associated with #GedaMenuButton.
 *
 *  \param [in] button: a #GedaMenuButton
 *
 * Return value: the GtkMenu associated with #GedaMenuButton
 *
 **/
GtkWidget *geda_menu_button_get_menu (GedaMenuButton *button)
{
  g_return_val_if_fail (GEDA_IS_MENU_BUTTON (button), NULL);

  return GTK_WIDGET (button->priv->menu);
}
/* END -------------------------    Sub-Menu   ----------------------------- */

/* BEGIN ------+-------+-------^    Tooltips   ^-------+-------+-------+-----*/

void geda_menu_button_set_tooltip_text (GedaMenuButton *button,
                                        const char *tip_text)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_widget_set_tooltip_text((GtkWidget*) button,tip_text);
}

void geda_menu_button_set_arrow_tooltip (GedaMenuButton *button,
                                         GtkTooltips    *tooltips,
                                         const char     *tip_text,
                                         const char     *tip_private)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_tooltips_set_tip (tooltips, button->priv->arrow_button, tip_text,
                        tip_private);
}

void geda_menu_button_set_arrow_tooltip_text(GedaMenuButton *button,
                                             const char     *text)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_widget_set_tooltip_text (button->priv->arrow_button, text);
}

void geda_menu_button_set_arrow_tooltip_markup (GedaMenuButton *button,
                                                const char     *markup)
{
  g_return_if_fail (GEDA_IS_MENU_BUTTON (button));
  gtk_widget_set_tooltip_markup (button->priv->arrow_button, markup);
}

/* END -------------------------    Tooltips   ----------------------------- */

/*-----+-------+-------+-------^-------+-------^-------+-------+-------+-----*/
GedaMenuButton *geda_menu_button_new_from_stock (const char *stock_id)
{
  GedaMenuButton *button;

  g_return_val_if_fail (stock_id != NULL, NULL);

  button = g_object_new (GEDA_TYPE_MENU_BUTTON, "stock-id", stock_id,
                                                "border-width", 0,
                                                "menu-relief", GTK_RELIEF_NONE,
                                                 NULL);

  return button;
}

GedaMenuButton*
geda_menu_button_new(GtkWidget *icon_widget, const char *label)
{
  GedaMenuButton *button;

  button = g_object_new (GEDA_TYPE_MENU_BUTTON, "border-width", 0,
                                                "menu-relief", GTK_RELIEF_NONE,
                                                NULL);

  if (label)
    geda_menu_button_set_label(button, label);

  if (icon_widget)
     geda_menu_button_set_image(button, icon_widget);

  return button;
}

GedaMenuButton *geda_menu_button_new_with_mnemonic(const char *label)
{
  return g_object_new (GEDA_TYPE_MENU_BUTTON, "label", label,
                       "menu-relief", GTK_RELIEF_NONE,
                       "use-underline", TRUE,  NULL);;
}
/*! @} end group GedaMenuButton */