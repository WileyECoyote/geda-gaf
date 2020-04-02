/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_toggle_action.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2012-2015 Ales Hvezda
 * Copyright (C) 2012-2015 gEDA Contributors (see ChangeLog for details)
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
 * Date: December 25, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 * \Note: WEH: We do all this for the Multi-Key Characters in the
 *             Menus, (because We really Like them, otherwise we
 *             could have just used the stock gtk_toggle_action.)
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <geda/geda.h>

#include <glib.h>
#include <gtk/gtk.h>

#include "../../include/geda_toggle_action.h"
#include "../../include/geda_accel_label.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_check_menu_item.h"
#include "../../include/gettext.h"
#include <geda_debug.h>

/**
 * \brief GedaToggleAction - A Button Widget for Menus
 * \par
 * A GedaToggleAction is a variant of GedaAction. GedaToggleAction and
 * GedaAction objects allow menu items with multi-key assignments, their
 * Gtk counterparts do not.
 *
 * \defgroup GedaToggleAction Menu Toggle Button
 * @{
 */

enum
{
  TOGGLED,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_DRAW_AS_RADIO,
  PROP_ACTIVE
};

static void geda_toggle_action_activate     (GtkAction       *action);

static void geda_toggle_action_set_property (GObject      *object,
                                             unsigned int  property_id,
                                             const GValue *value,
                                             GParamSpec   *pspec);

static void geda_toggle_action_get_property (GObject      *object,
                                             unsigned int  property_id,
                                             GValue       *value,
                                             GParamSpec   *pspec);

static GObjectClass *geda_toggle_action_parent_class = NULL;

static GHashTable *toggle_action_hash = NULL;

static unsigned int action_signals[LAST_SIGNAL] = { 0 };

/*!
 * \brief GObject finalise handler
 * \par Function Description
 *  Just before the GedaToggleAction GObject is finalized, free
 *  allocated data and chain up to the parent's finalize handler.
 *
 * \param [in] object The GObject being finalized.
 */
static void
geda_toggle_action_finalize (GObject *object)
{
  //GedaToggleAction *action = (GedaToggleAction*)object;

  if (g_hash_table_remove (toggle_action_hash, object)) {
    if (!g_hash_table_size (toggle_action_hash)) {
      g_hash_table_destroy (toggle_action_hash);
      toggle_action_hash = NULL;
    }
  }

  ((GObjectClass*)geda_toggle_action_parent_class)->finalize (object);
}

/*!
 * \brief GObject property getter function
 * \par Function Description
 *  Getter function for GedaToggleAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 * \param [in]  object       The GObject whose properties we are getting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [out] value        The GValue in which to return the value of the property
 * \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
geda_toggle_action_get_property (GObject *object, unsigned int property_id,
                                 GValue  *value,  GParamSpec  *pspec)
{
  GedaToggleAction *action = (GedaToggleAction*)object;

  switch (property_id) {

    case PROP_DRAW_AS_RADIO:
      g_value_set_boolean (value, geda_toggle_action_get_draw_as_radio (action));
      break;

    case PROP_ACTIVE:
      g_value_set_boolean (value, geda_toggle_action_get_active (action));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/*!
 * \brief GObject property setter function
 * \par Function Description
 *  Setter function for GedaToggleAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 * \param [in]  object       The GObject whose properties we are setting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [in]  value        The GValue the property is being set from
 * \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
geda_toggle_action_set_property (GObject *object,
                                 unsigned int property_id,
                                 const GValue *value, GParamSpec *pspec)
{
  GedaToggleAction *action = (GedaToggleAction*)object;

  switch (property_id) {

    case PROP_DRAW_AS_RADIO:
      geda_toggle_action_set_draw_as_radio (action, g_value_get_boolean (value));
      break;

    case PROP_ACTIVE:
      geda_toggle_action_set_active (action, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

static void
geda_toggle_action_activate (GtkAction *action)
{
  GedaToggleAction *toggle_action = (GedaToggleAction*)action;

  toggle_action->active = !toggle_action->active;

  g_object_notify (G_OBJECT (action), "active");

  geda_toggle_action_toggled (toggle_action);
}

static GtkWidget *
geda_toggle_action_create_menu_item (GtkAction *action)
{
  GedaToggleAction *toggle_action = (GedaToggleAction*)action;

  return g_object_new (GEDA_TYPE_CHECK_MENU_ITEM,
                       "draw-as-radio", toggle_action->draw_as_radio,
                       NULL);
}

static void
geda_toggle_action_connect_proxy (GtkAction *action, GtkWidget *proxy)
{
  GedaAction *toggler = (GedaAction*)action;

  /* Override the type of label widget used with the menu item */
  if (GEDA_IS_MENU_ITEM (proxy)) {

    GtkWidget *label;

    label = geda_get_child_widget(proxy);

    /* make sure label is a GedaAccelLabel */
    if (label && !GEDA_IS_ACCEL_LABEL(label)) {
      gtk_container_remove ((GtkContainer*)proxy, label);
      label = NULL;
    }

    if (label == NULL) {

      char *label_string;

      g_object_get (action, "label", &label_string, NULL);

      g_object_new (GEDA_TYPE_ACCEL_LABEL,
                    "use-underline", TRUE,
                    "xalign", 0.0,
                    "parent", proxy,
                    "label", label_string,
                    "accel-string", toggler->multikey_accel,
                    NULL);
      g_free(label_string);
    }
    else {

      g_object_set (label,
                    "use-underline", TRUE,
                    "xalign", 0.0,
                    "accel-string", toggler->multikey_accel,
                    NULL);
    }

    gtk_widget_show(label);
  }

  /* Let the parent class do its work now we've fiddled with the label */
  ((GtkActionClass*)geda_toggle_action_parent_class)->connect_proxy (action, proxy);
}


/*!
 * \brief GedaToggleAction Class Initializer
 * \par Function Description
 *  Called to initialize the class instance.
 *
 * \note GtkActionClass is the parent of the GtkToggleActionClass
 *       from which we are derived. the GtkToggleActionClass has
 *       no member connect_proxy.
 *
 * \param [in] class A GedaToggleActionClass Object
 * \param [in] data  A GedaToggleAction data structure
 */
static void
geda_toggle_action_class_init (void *class, void *data)
{
  GObjectClass   *object_class;
  GtkActionClass *action_class;
  GParamSpec     *params;

  geda_toggle_action_parent_class = g_type_class_peek_parent (class);

  object_class = (GObjectClass*)class;
  action_class = (GtkActionClass*)class;

  object_class->finalize     = geda_toggle_action_finalize;
  object_class->set_property = geda_toggle_action_set_property;
  object_class->get_property = geda_toggle_action_get_property;

  action_class->activate          = geda_toggle_action_activate;
  action_class->menu_item_type    = GEDA_TYPE_CHECK_MENU_ITEM;
  action_class->toolbar_item_type = GTK_TYPE_TOGGLE_TOOL_BUTTON;
  action_class->connect_proxy     = geda_toggle_action_connect_proxy;
  action_class->create_menu_item  = geda_toggle_action_create_menu_item;

  ((GedaToggleActionClass*)class)->toggled = NULL;

  /**
   * GedaToggleAction:draw-as-radio:
   *
   * Whether the proxies for this action look like radio action proxies.
   *
   * This is an appearance property and thus only applies if
   * GtkActivatable:use-action-appearance is %TRUE.
   */

  params = g_param_spec_boolean ("draw-as-radio",
                               _("Create the same proxies as a radio action"),
                               _("Whether the proxies for this action look like radio action proxies"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_DRAW_AS_RADIO, params);

  /**
   * GedaToggleAction:active:
   *
   * If the toggle action should be active in or not.
   */
  params = g_param_spec_boolean ("active",
                               _("Active"),
                               _("If the toggle action should be active in or not"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_ACTIVE, params);

  action_signals[TOGGLED] =
    g_signal_new ("toggled",
                  G_OBJECT_CLASS_TYPE (class),
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (GedaToggleActionClass, toggled),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);
}

/*!
 * \brief Initialize new GedaToggleAction data structure instance.
 * \par Function Description
 *  This function is call after the GedaToggleActionClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaToggleAction data structure
 * \param [in] class     A GedaToggleActionClass Object
 */
static void
geda_toggle_action_instance_init (GTypeInstance *instance, void *class)
{
  GedaToggleAction *action = (GedaToggleAction*)instance;

  if (!toggle_action_hash) {
    toggle_action_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  action->active = FALSE;
  action->draw_as_radio = FALSE;

  g_hash_table_replace (toggle_action_hash, instance, instance);
}

/*!
 * \brief Function to retrieve GedaToggleAction's Type identifier.
 * \par Function Description
 *  Function to retrieve #GedaToggleAction Type identifier. On the first
 *  call, this registers the #GedaToggleAction in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 * \return GedaType identifier associated with a GedaToggleAction.
 */
GedaType geda_toggle_action_get_type (void)
{
  static volatile GedaType toggle_action_type = 0;

  if (g_once_init_enter (&toggle_action_type)) {

    static const GTypeInfo geda_toggle_action_info = {
      sizeof(GedaToggleActionClass),
      NULL,                                      /* base_init      */
      NULL,                                      /* base_finalize  */
      geda_toggle_action_class_init,             /* (GClassInitFunc) */
      NULL,                                      /* class_finalize */
      NULL,                                      /* class_data     */
      sizeof(GedaToggleAction),
      0,                                         /* n_preallocs    */
      geda_toggle_action_instance_init           /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaToggleAction");
    type   = g_type_register_static (GEDA_TYPE_ACTION, string,
                                    &geda_toggle_action_info, 0);

    g_once_init_leave (&toggle_action_type, type);
  }

  return toggle_action_type;
}

bool is_a_geda_toggle_action (GedaToggleAction *action)
{
  if ((action != NULL) && (toggle_action_hash != NULL)) {
    return g_hash_table_lookup(toggle_action_hash, action) ? TRUE : FALSE;
  }

  return FALSE;
}

/*!
 * \brief Create a New GedaToggleAction
 * \par Function Description
 *  This function creates and returns a new GedaToggleAction
 *
 * \param [in] name           A unique name for the action
 * \param [in] label          Label displayed in menu items and on buttons, or %NULL
 * \param [in] tooltip        A tooltip for the action, or %NULL
 * \param [in] icon_id        Stock icon to display in widgets representing the action, or %NULL
 * \param [in] multikey_accel The accel char string
 *
 * Return value: a new GedaToggleAction
 */
GedaToggleAction *
geda_toggle_action_new (const char *name,
                        const char *label,
                        const char *tooltip,
                        const char *icon_id,
                        const char *multikey_accel)
{
  GedaToggleAction *action;

  g_return_val_if_fail (name != NULL, NULL);

  if (multikey_accel != NULL) {
    action = g_object_new (GEDA_TYPE_TOGGLE_ACTION, "name", name,
                                                    "label", label,
                                                    "tooltip", tooltip,
                                                    "stock-id", icon_id,
                                                    "multikey-accel", multikey_accel,
                                                    NULL);
  }
  else {
    action = g_object_new (GEDA_TYPE_TOGGLE_ACTION, "name", name,
                                                    "label", label,
                                                    "tooltip", tooltip,
                                                    "stock-id", icon_id,
                                                    NULL);
  }
  return action;
}

/*!
 * \brief Emit "toggled" on a GedaToggleAction object
 * \par Function Description
 *  Emits the "toggled" signal on the toggle action.
 *
 * \param [in] action  the action object
 */
void
geda_toggle_action_toggled (GedaToggleAction *action)
{
  g_return_if_fail (GEDA_IS_TOGGLE_ACTION (action));

  g_signal_emit (action, action_signals[TOGGLED], 0);
}

/*!
 * \brief Retrieve the active status from a GedaToggleAction
 * \par Function Description
 *  Returns the checked state of the toggle action.
 *
 * \param [in] action the action object
 *
 * \returns the checked state of the toggle action
 */
bool
geda_toggle_action_get_active (GedaToggleAction *action)
{
  g_return_val_if_fail (GEDA_IS_TOGGLE_ACTION (action), FALSE);

  return action->active;
}

/*!
 * \brief Set GedaToggleAction active status
 * \par Function Description
 *  Sets the checked state on the toggle action.
 *
 * \param [in] action     the action object
 * \param [in] is_active  whether the action should be checked or not
 */
void
geda_toggle_action_set_active (GedaToggleAction *action, bool is_active)
{
  g_return_if_fail (GEDA_IS_TOGGLE_ACTION (action));

  is_active = is_active != FALSE;

  if (action->active != is_active) {
    gtk_action_activate ((GtkAction*)action);
  }
}

/**
 * geda_toggle_action_get_draw_as_radio:
 *
 * \param [in] action  the action object
 *
 * Returns whether the action should have proxies like a radio action.
 *
 * Returns: whether the action should have proxies like a radio action.
 */
bool
geda_toggle_action_get_draw_as_radio (GedaToggleAction *action)
{
  g_return_val_if_fail (GEDA_IS_TOGGLE_ACTION (action), FALSE);

  return action->draw_as_radio;
}

/**
 * geda_toggle_action_set_draw_as_radio:
 *
 * \param [in] action         the action object
 * \param [in] draw_as_radio  whether the action should have proxies like a radio
 *    action
 *
 * Sets whether the action should have proxies like a radio action.
 */
void
geda_toggle_action_set_draw_as_radio (GedaToggleAction *action, bool draw_as_radio)
{
  g_return_if_fail (GEDA_IS_TOGGLE_ACTION (action));

  draw_as_radio = draw_as_radio != FALSE;

  if (action->draw_as_radio != draw_as_radio) {

    action->draw_as_radio = draw_as_radio;

    g_object_notify (G_OBJECT (action), "draw-as-radio");
  }
}

/** @} end group GedaToggleAction */
