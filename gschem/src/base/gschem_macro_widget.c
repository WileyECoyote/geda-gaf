/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Contributing Author: Edward Hennessy
 * Date Contributed: November 30th, 2013
 */
/*!
 * \file gschem_macro_widget.c
 *
 * \brief A widget for entering macros
 */

#include <gschem.h>
#include <geda_widgets.h>

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#include <gettext.h>

/** \defgroup Gschem-Macro-Widget Gschem Macro Widget
 * @{
 * \brief #GschemMacroWidget Class Implmentation
 * \par
 *  This module implements the macro entry widget in gschem. The
 *  macro widget appears when the user activate the item, normally
 *  using the \<SHIFT\>colon key and contains an entry field, and two
 *  buttons; Evaluate and Cancel.
 */

enum
{
  PROP_0,
  PROP_LABEL_TEXT,
  PROP_MACRO_STRING
};

/* Function Prototypes */
static void
activate_entry (GtkWidget *entry, GschemMacroWidget *widget);

static void
click_cancel (GtkWidget *button, GschemMacroWidget *widget);

static void
click_evaluate (GtkWidget *entry, GschemMacroWidget *widget);

static void
realize (GtkWidget *widget);

static void
unrealize (GtkWidget *widget);

static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec);

static void
gschem_macro_widget_class_init (void *g_class, void *g_class_data);

static void
gschem_macro_widget_instance_init (GTypeInstance *instance, void *g_class);

static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec);

static void
notify_entry_text (GtkWidget *entry, GParamSpec *pspec, GschemMacroWidget *widget);


static void *gschem_macro_widget_parent_class = NULL;


/* Callback; called when the user presses enter in the entry widget
 */
static void
activate_entry (GtkWidget *entry, GschemMacroWidget *widget)
{
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {

    if (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) > 0) {
      gtk_info_bar_response (GTK_INFO_BAR (widget), GEDA_RESPONSE_OK);
    }
    else {
      gtk_info_bar_response (GTK_INFO_BAR (widget), GEDA_RESPONSE_CANCEL);
    }
  }
}

/* Callback; called when the user clicks the cancel button
 */
static void
click_cancel (GtkWidget *button, GschemMacroWidget *widget)
{
  gtk_info_bar_response (GTK_INFO_BAR (widget), GEDA_RESPONSE_CANCEL);
}

/* Callback; called when the user clicks the evaluate button
 */
static void
click_evaluate (GtkWidget *entry, GschemMacroWidget *widget)
{
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) > 0) {
      gtk_info_bar_response (GTK_INFO_BAR (widget), GEDA_RESPONSE_OK);
    }
  }
}

static void
realize (GtkWidget *widget)
{
  GschemMacroWidget *gmw = GSCHEM_MACRO_WIDGET (widget);

 /* Note: We do not need the "activate" signal here because the geda-entry
   * widget generates a "process-entry" signal instead. */
  GEDA_SIGNAL_CONNECT (gmw->entry, "process-entry",
                       G_CALLBACK (activate_entry), gmw);

  GEDA_SIGNAL_CONNECT (gmw->cancel_button, "clicked",
                       G_CALLBACK (click_cancel),
                       gmw);

  GEDA_SIGNAL_CONNECT (gmw->evaluate_button, "clicked",
                       G_CALLBACK (click_evaluate),
                       gmw);

  GEDA_SIGNAL_CONNECT (gmw->entry, "notify::text",
                       G_CALLBACK (notify_entry_text),
                       gmw);

  ((GtkWidgetClass*)gschem_macro_widget_parent_class)->realize (widget);
}

static void
unrealize (GtkWidget *widget)
{
  GschemMacroWidget *gmw = GSCHEM_MACRO_WIDGET (widget);

  g_signal_handlers_disconnect_by_func (gmw->entry,
                                        activate_entry,
                                        gmw);

  g_signal_handlers_disconnect_by_func (gmw->cancel_button,
                                        click_cancel,
                                        gmw);

  g_signal_handlers_disconnect_by_func (gmw->evaluate_button,
                                        click_evaluate,
                                        gmw);

  g_signal_handlers_disconnect_by_func (gmw->entry,
                                        notify_entry_text,
                                        gmw);

  ((GtkWidgetClass*)gschem_macro_widget_parent_class)->unrealize (widget);
}

/*!
 * \brief Get a property
 * \par Function Description
 * \param [in]     object
 * \param [in]     param_id
 * \param [in,out] value
 * \param [in]     pspec
 */
static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec)
{
  GschemMacroWidget *gmw    = GSCHEM_MACRO_WIDGET (object);
  GtkWidget         *widget = (GtkWidget*)gmw;

  switch (param_id) {
    case PROP_LABEL_TEXT:
      g_value_set_string (value, gschem_macro_widget_get_label_text (widget));
      break;

    case PROP_MACRO_STRING:
      g_value_set_string (value, gschem_macro_widget_get_macro_string (widget));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

/*!
 * \brief Initialize GschemMacroWidget class
 * \par Function Description
 * \param [in]  class       The GschemMacroWidgetClass to be initialized
 * \param [in]  class_data  (unused)
 */
static void gschem_macro_widget_class_init (void *class, void *class_data)
{
  GObjectClass   *gobject_class = (GObjectClass*)class;
  GtkWidgetClass *widget_class  = (GtkWidgetClass*)class;

  gschem_macro_widget_parent_class = g_type_class_peek_parent (class);

  gobject_class->get_property = get_property;
  gobject_class->set_property = set_property;

  widget_class->realize    = realize;
  widget_class->unrealize  = unrealize;

  g_object_class_install_property (gobject_class,
                                   PROP_LABEL_TEXT,
                                   g_param_spec_string ("label-text",
                                                      _("Label Text"),
                                                      _("Text to be displayed for the Macro label"),
                                                      _("Macro:"),
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (gobject_class,
                                   PROP_MACRO_STRING,
                                   g_param_spec_string ("macro-string",
                                                      _("Macro String"),
                                                      _("Macro String"),
                                                        "",
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

/*!
 * \brief Get the entry
 * \par Function Description
 * \param [in] widget This GschemMacroWidget
 * \return The entry
 */
GtkWidget *gschem_macro_widget_get_entry (GtkWidget *widget)
{
  GtkWidget *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
   if (GSCHEM_IS_MACRO_WIDGET(widget)) {
     ret_val = ((GschemMacroWidget*)widget)->entry;
   }
   else {
     BUG_MSG("widget is not a GschemMacroWidget");
   }
  }
  return ret_val;
}

/*!
 * \brief Get the label text
 * \par Function Description
 * \param [in] widget This GschemMacroWidget
 * \return The label text
 */
const char *gschem_macro_widget_get_label_text (GtkWidget *widget)
{
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
   if (GSCHEM_IS_MACRO_WIDGET(widget)) {
     GschemMacroWidget *gmw = (GschemMacroWidget*)widget;
     ret_val = geda_label_widget_get_text (gmw->label);
   }
   else {
     BUG_MSG("widget is not a GschemMacroWidget");
   }
  }
  return ret_val;
}

/*!
 * \brief Get the macro string
 * \par Function Description
 * \param [in] widget This GschemMacroWidget
 * \return The macro string
 */
const char *gschem_macro_widget_get_macro_string (GtkWidget *widget)
{
  const char *ret_val = NULL;

  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
   if (GSCHEM_IS_MACRO_WIDGET(widget)) {
     GschemMacroWidget *gmw = (GschemMacroWidget*)widget;
     ret_val = geda_entry_widget_get_text (gmw->entry);
   }
   else {
     BUG_MSG("widget is not a GschemMacroWidget");
   }
  }
  return ret_val;
}

/*!
 * \brief Initialize GschemMacroWidget instance
 * \par Function Description
 * \param [in,out] instance The GschemMacroWidget being initialized.
 * \param [in]     class  The class of the type the instance is created for.
 */
static void gschem_macro_widget_instance_init(GTypeInstance *instance, void *class)
{
  GschemMacroWidget *widget;

  GtkWidget *action;
  GtkWidget *button_box;
  GtkWidget *content;

  widget  = (GschemMacroWidget*)instance;
  action  = gtk_info_bar_get_action_area (GTK_INFO_BAR (widget));
  content = gtk_info_bar_get_content_area (GTK_INFO_BAR (widget));

  widget->instance_type = gschem_macro_widget_get_type();

  gtk_widget_set_no_show_all (GTK_WIDGET (widget), TRUE);

  widget->label = geda_visible_label_new (NULL);
  gtk_box_pack_start (GTK_BOX (content), widget->label, FALSE, FALSE, 0);

  widget->entry = geda_entry_new_visible ();
  gtk_box_pack_start (GTK_BOX (content), widget->entry, TRUE, TRUE, 0);

  button_box = gtk_hbutton_box_new ();
  gtk_widget_show (button_box);
  gtk_box_pack_start (GTK_BOX (content), button_box, FALSE, FALSE, 0);

  widget->evaluate_button = gtk_button_new_with_label (_("Evaluate"));
  gtk_widget_set_sensitive (widget->evaluate_button, FALSE);
  gtk_widget_show (widget->evaluate_button);
  gtk_box_pack_start (GTK_BOX (button_box), widget->evaluate_button, FALSE, FALSE, 0);

  widget->cancel_button = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  gtk_widget_show (widget->cancel_button);
  gtk_box_pack_start (GTK_BOX (button_box), widget->cancel_button, FALSE, FALSE, 0);

  gtk_widget_set_no_show_all (action, TRUE);

  g_object_set (action, "visible", FALSE, NULL);
}

/*!
 * \brief Get/register GschemMacroWidget type.
 * \par Function Description
 */
GedaType gschem_macro_widget_get_type (void)
{
  static GedaType macro_widget_type = 0;

  if (g_once_init_enter (&macro_widget_type)) {

    static const GTypeInfo info = {
      sizeof(GschemMacroWidgetClass),
      NULL,                             /* base_init           */
      NULL,                             /* base_finalize       */
      gschem_macro_widget_class_init,   /* (GClassInitFunc)    */
      NULL,                             /* class_finalize      */
      NULL,                             /* class_data          */
      sizeof(GschemMacroWidget),
      0,                                /* n_preallocs         */
      gschem_macro_widget_instance_init /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GschemMacroWidget");
    type   = g_type_register_static (GTK_TYPE_INFO_BAR, string, &info, 0);

    g_once_init_leave (&macro_widget_type, type);
  }

  return macro_widget_type;
}

/*!
 * \brief Check if Obect is GschemMacroWidget
 * \par Function Description
 *  Compares unsigned long/int to at instance_type offset to the
 *  value stored by gschem_macro_widget_get_type if widget is a
 *  g_object.
 *
 * \param [in] widget Object to be tested.
 *
 * \returns TRUE if \a widget is a bona fided GschemMacroWidget
 */
bool is_a_gschem_macro_widget (GschemMacroWidget *widget)
{
  if (G_IS_OBJECT(widget)) {
    return (gschem_macro_widget_get_type() == widget->instance_type);
  }
  return FALSE;
}

GtkWidget *gschem_macro_widget_new(void)
{
  return g_object_new (GSCHEM_TYPE_MACRO_WIDGET, NULL);
}

/*!
 * \brief Set the label text
 * \par Function Description
 * \param [in,out] widget This GschemMacroWidget
 * \param [in]     text   The label text
 */
void gschem_macro_widget_set_label_text (GtkWidget *widget, const char *text)
{
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
   if (GSCHEM_IS_MACRO_WIDGET(widget)) {
     GschemMacroWidget *gmw = (GschemMacroWidget*)widget;
     geda_label_widget_set_text (gmw->label, text);
     GEDA_OBJECT_NOTIFY (widget, "label-text");
   }
   else {
     BUG_MSG("widget is not a GschemMacroWidget");
   }
  }
}

/*!
 * \brief Set the macro string
 * \par Function Description
 * \param [in,out] widget This GschemMacroWidget
 * \param [in]     str    The macro string
 */
void gschem_macro_widget_set_macro_string (GtkWidget *widget, const char *str)
{
  if (widget == NULL) {
    BUG_MSG("widget is NULL");
  }
  else {
    if (GSCHEM_IS_MACRO_WIDGET(widget)) {
      GschemMacroWidget *gmw = (GschemMacroWidget*)widget;
      geda_entry_widget_set_text (gmw->entry, str);
      GEDA_OBJECT_NOTIFY (widget, "macro-string");
    }
    else {
      BUG_MSG("widget is not a GschemMacroWidget");
    }
  }
}

/*!
 * \brief Update the sensitivity of the evaluate button
 * \par Function Description
 */
static void notify_entry_text (GtkWidget *entry, GParamSpec *pspec, GschemMacroWidget *widget)
{
  g_return_if_fail (widget != NULL);

  gtk_widget_set_sensitive (widget->evaluate_button,
                            (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) > 0));
}


/*!
 * \brief Set a gobject property
 * \par Function Description
 */
static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec)
{
  GtkWidget *widget = (GtkWidget*)GSCHEM_MACRO_WIDGET (object);

  switch (param_id) {
    case PROP_LABEL_TEXT:
      gschem_macro_widget_set_label_text (widget, g_value_get_string (value));
      break;

    case PROP_MACRO_STRING:
      gschem_macro_widget_set_macro_string (widget, g_value_get_string (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

/** @} endgroup Gschem-Macro-Widget */
