/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_toggle_action.h
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
 * version 3 of the License, or (at your option) any later version.
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
#include "config.h"
#endif

#include <geda/geda.h>

#include <glib.h>
#include <gtk/gtk.h>
#include <gtk/gtkprivate.h>

#include "geda_toggle_action.h"
#include "geda_accel_label.h"
#include "gettext.h"
#include <geda_debug.h>

/**
 * \brief GedaToggleAction - A Button Widget for Menus
 * \par
 * A GedaToggleAction is a variant of GtkToggleAction, similar to GedaAction
 * objects. GedaToggleAction and GedaAction allow menu items with multi-key
 * assignments, their Gtk counterparts do not.
 *
 * \defgroup GedaToggleAction Menu Toggle Button
 * @{
 */

enum {
  PROP_0,
  PROP_DRAW_AS_RADIO,
  PROP_ACTIVE,
  PROP_MULTIKEY_ACCEL
};

//static char *ptr_multikey_accel = NULL;
static GObjectClass *geda_toggle_action_parent_class = NULL;

static void geda_toggle_action_set_property (GObject      *object,
                                             unsigned int  property_id,
                                             const GValue *value,
                                             GParamSpec   *pspec);

static void geda_toggle_action_get_property (GObject      *object,
                                             unsigned int  property_id,
                                             GValue       *value,
                                             GParamSpec   *pspec);

/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the GedaToggleAction GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void geda_toggle_action_finalize (GObject *object)
{
  GedaToggleAction *action = GEDA_TOGGLE_ACTION (object);

  if (action->multikey_accel) {
    g_free (action->multikey_accel);
  }
  G_OBJECT_CLASS (geda_toggle_action_parent_class)->finalize (object);
}

/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GedaToggleAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
geda_toggle_action_set_property (GObject *object,
                                 unsigned int property_id,
                                 const GValue *value, GParamSpec *pspec)
{
  GedaToggleAction *action = GEDA_TOGGLE_ACTION (object);

  if(property_id == PROP_MULTIKEY_ACCEL) {
    if (action->multikey_accel != NULL) {
      g_free (action->multikey_accel);
    }
    action->multikey_accel = g_value_dup_string (value);
  }
}

/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GedaToggleAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
geda_toggle_action_get_property (GObject *object, unsigned int property_id,
                                 GValue  *value,  GParamSpec  *pspec)
{
  GedaToggleAction *action = GEDA_TOGGLE_ACTION (object);

  if(property_id == PROP_MULTIKEY_ACCEL)
    g_value_set_string (value, action->multikey_accel);
}

static void
geda_toggle_action_connect_proxy (GtkAction *action, GtkWidget *proxy)
{
  GedaToggleAction *toggler = GEDA_TOGGLE_ACTION (action);
  char *label_string;

  /* Override the type of label widget used with the menu item */
  if (GTK_IS_MENU_ITEM (proxy)) {
    GtkWidget *label;

    label = GTK_BIN (proxy)->child;

    /* make sure label is a GedaAccelLabel */
    if (label && !GEDA_IS_ACCEL_LABEL (label)) {
      gtk_container_remove (GTK_CONTAINER (proxy), label);
      label = NULL;
    }

    if (label == NULL) {
      g_object_get (action, "label", &label_string, NULL);
      g_object_new (GEDA_TYPE_ACCEL_LABEL,
                    "use-underline", TRUE,
                    "xalign", 0.0,
                    "visible", TRUE,
                    "parent", proxy,
                    "label", label_string,
                    "accel-string", toggler->multikey_accel,
                    NULL);
      g_free(label_string);
    }
  }

  /* Let the parent class do its work now we've fiddled with the label */
  GTK_ACTION_CLASS ( geda_toggle_action_parent_class)->connect_proxy (action, proxy);
}

/*! \brief GedaToggleAction Class Initializer
 *  \par Function Description
 *  Called to initialize the class instance.
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

  object_class = G_OBJECT_CLASS (class);
  action_class = GTK_ACTION_CLASS (class);

  action_class->connect_proxy = geda_toggle_action_connect_proxy;

  object_class->finalize     = geda_toggle_action_finalize;
  object_class->set_property = geda_toggle_action_set_property;
  object_class->get_property = geda_toggle_action_get_property;

  params = g_param_spec_string ("multikey-accel",
                              _("Multikey Accelerator"),
                              _("Multikey Accelerator"),
                                 NULL,
                                 G_PARAM_READWRITE);

  g_object_class_install_property(object_class, PROP_MULTIKEY_ACCEL, params);

}

/*! \brief Initialize new GedaToggleAction data structure instance.
 *
 *  \par Function Description
 *  This function is call after the GedaToggleActionClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaToggleAction data structure
 * \param [in] class     A GedaToggleActionClass Object
 */
static void
geda_toggle_action_instance_init (GTypeInstance *instance, void *class)
{
  //GedaToggleAction *action = (GedaToggleAction*)instance;
}

/*! \brief Function to retrieve GedaToggleAction's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve #GedaToggleAction Type identifier. On the first
 *  call, this registers the #GedaToggleAction in the GedaType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return GedaType identifier associated with a GedaToggleAction.
 */
GedaType geda_toggle_action_get_type (void)
{
  static GedaType toggle_action_type = 0;

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
    type   = g_type_register_static (GTK_TYPE_TOGGLE_ACTION, string,
                                    &geda_toggle_action_info, 0);

    g_once_init_leave (&toggle_action_type, type);
  }

  return toggle_action_type;
}

/*! \brief Create a New GedaToggleAction
 *
 *  \par Function Description
 *  This function creates and returns a new GedaToggleAction
 *
 * \param [in] name:     A unique name for the action
 * \param [in] label:    The label displayed in menu items and on buttons, or %NULL
 * \param [in] tooltip:  A tooltip for the action, or %NULL
 * \param [in] icon_id:  The stock icon to display in widgets representing the action, or %NULL
 * \param [in] multikey_accel: The accel char string
 *
 * Return value: a new GedaToggleAction
 *
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

  if (multikey_accel != NULL)
    action = g_object_new (GEDA_TYPE_TOGGLE_ACTION, "name", name,
                                                    "label", label,
                                                    "tooltip", tooltip,
                                                    "stock-id", icon_id,
                                                    "multikey-accel", multikey_accel,
                                                    NULL);
  else
    action = g_object_new (GEDA_TYPE_TOGGLE_ACTION, "name", name,
                                                    "label", label,
                                                    "tooltip", tooltip,
                                                    "stock-id", icon_id,
                                                    NULL);
  return action;
}

/** @} end group GedaToggleAction */