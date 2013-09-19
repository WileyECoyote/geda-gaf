/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 09/16/13 | Separated call to g_param_spec_string from
 *                | g_object_class_install_property. Setup data with
 *                | new function gschem_action_label_init. Added property
 *                | PROP_ICON_ID and associated functions. Set property
 *                | handlers back to switch/case. Added GInstanceInitFunc.
 *                | (The gschem_action_set_icon_name checks both the
 *                | theme-able search path and our icon factory for icons).
 */
#include <config.h>

#include <gtk/gtk.h>
#include "widgets/gschem_action.h"
#include "widgets/gschem_accel_label.h"
#include "gettext.h"

enum {
  PROP_MULTIKEY_ACCEL = 1,
  PROP_ICON_ID,
};

static GObjectClass *gschem_action_parent_class = NULL;

/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the GschemAction GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void gschem_action_finalize (GObject *object)
{
  GschemAction *action = GSCHEM_ACTION (object);

  g_free (action->multikey_accel);
  g_free (action->icon_name);

  G_OBJECT_CLASS (gschem_action_parent_class)->finalize (object);
}


/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GschemAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
gschem_action_set_property (GObject *object, unsigned int property_id,
                            const    GValue *value, GParamSpec *pspec)
{
  GschemAction *action = GSCHEM_ACTION (object);

  switch (property_id)
  {
    case PROP_MULTIKEY_ACCEL:
      g_free (action->multikey_accel);
      action->multikey_accel = g_strdup (g_value_get_string (value));
      break;
    case PROP_ICON_ID:
      gschem_action_set_icon_name(action, g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GschemAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
gschem_action_get_property (GObject *object, unsigned int property_id,
                            GValue  *value,  GParamSpec   *pspec)
{
  GschemAction *action = GSCHEM_ACTION (object);

  switch (property_id)
  {
    case PROP_MULTIKEY_ACCEL:
      g_value_set_string (value, action->multikey_accel);
      break;
    case PROP_ICON_ID:
      g_value_set_string (value, action->icon_name);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

static void
gschem_action_connect_proxy (GtkAction *action, GtkWidget *proxy)
{
  GschemAction *gs_action = GSCHEM_ACTION (action);
  char *label_string;

  /* Override the type of label widget used with the menu item */
  if (GTK_IS_MENU_ITEM (proxy)) {
    GtkWidget *label;

    label = GTK_BIN (proxy)->child;

    /* make sure label is a GschemAccelLabel */
    if (label && !GSCHEM_IS_ACCEL_LABEL (label)) {
      gtk_container_remove (GTK_CONTAINER (proxy), label);
      label = NULL;
    }

    if (label == NULL) {
      g_object_get (action, "label", &label_string, NULL);
      g_object_new (GSCHEM_TYPE_ACCEL_LABEL,
                    "use-underline", TRUE,
                    "xalign", 0.0,
                    "visible", TRUE,
                    "parent", proxy,
                    "label", label_string,
                    "accel-string", gs_action->multikey_accel,
                    NULL);
    }
  }

  /* Let the parent class do its work now we've fiddled with the label */
  GTK_ACTION_CLASS (gschem_action_parent_class)->connect_proxy (action, proxy);
}


/*! \brief GType class initialiser for GschemAction
 *
 *  \par Function Description
 *  GType class initialiser for GschemAction. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  klass       The GschemActionClass we are initialising
 */
static void gschem_action_class_init (GschemActionClass *klass)
{
  GParamSpec     *params;
  GObjectClass   *gobject_class   = G_OBJECT_CLASS (klass);
  GtkActionClass *gtkaction_class = GTK_ACTION_CLASS (klass);

  gtkaction_class->connect_proxy  = gschem_action_connect_proxy;

  gobject_class->finalize         = gschem_action_finalize;
  gobject_class->set_property     = gschem_action_set_property;
  gobject_class->get_property     = gschem_action_get_property;

  gschem_action_parent_class      = g_type_class_peek_parent (klass);

  params = g_param_spec_string ("multikey-accel",
                              _("multikey-accelerator"),
                              _("A string with characters in positions key characters"),
                                 NULL,
                               (G_PARAM_WRITABLE));

  g_object_class_install_property( gobject_class, PROP_MULTIKEY_ACCEL, params);

  params = g_param_spec_string ("icon-id",
                              _("icon-identification"),
                              _("String name of the icon image"),
                                 NULL,
                               (G_PARAM_WRITABLE));

  g_object_class_install_property( gobject_class, PROP_ICON_ID, params);

}
static void gschem_action_init (GschemAction *action)
{
  action->multikey_accel = NULL;
  action->icon_name      = NULL;
}
/*! \brief Function to retrieve GschemAction's GType identifier.
 *
 *  \par Function Description
 *  Function to retrieve GschemAction's GType identifier.
 *  Upon first call, this registers the GschemAction in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return the GType identifier associated with GschemAction.
 */
GType gschem_action_get_type ()
{
  static GType gschem_action_type = 0;

  if (!gschem_action_type) {
    static const GTypeInfo gschem_action_info = {
      sizeof(GschemActionClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) gschem_action_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(GschemAction),
      0,    /* n_preallocs */
      (GInstanceInitFunc) gschem_action_init, /* instance_init */
    };

    gschem_action_type = g_type_register_static (GTK_TYPE_ACTION,
                                                 "GschemAction",
                                                 &gschem_action_info, 0);
  }

  return gschem_action_type;
}


/*! /brief Creates a new GschemAction object
 *
 * /par Function Descriptions
 *
 * Creates a new GschemAction object.
 *
 * /param [in] name            A unique name for the action
 * /param [in] label           The label displayed in menu items and on buttons, or NULL
 * /param [in] tooltip         A tooltip for the action, or NULL
 * /param [in] icon_id         The icon to display in widgets representing the action, or NULL
 * /param [in] multikey_accel  The (potentially) multi-key accelerator used for this action
 *
 * /returns A new GschemAction
 */
GschemAction *gschem_action_new (const char *name,
                                 const char *label,
                                 const char *tooltip,
                                 const char *icon_id,
                                 const char *multikey_accel)
{
  g_return_val_if_fail (name != NULL, NULL);

  return g_object_new (GSCHEM_TYPE_ACTION,
                       "name", name,
                       "label", label,
                       "tooltip", tooltip,
                       "icon-id", icon_id,
                       "multikey-accel", multikey_accel,
                       NULL);
}

const char *gschem_action_get_icon_name (GschemAction *action)
{
  g_return_val_if_fail (GSCHEM_IS_ACTION (action), NULL);
  return action->icon_name;
}

void gschem_action_set_icon_name (GschemAction *action,
                                   const char  *icon_name)
{
  GtkStockItem stock_info;

  g_return_if_fail (GSCHEM_IS_ACTION (action));

  g_free (action->icon_name);
  action->icon_name = NULL;

  action->icon_name = g_strdup (icon_name);

  if (icon_name != NULL) {
    if ( gtk_stock_lookup(icon_name, &stock_info)) {
      gtk_action_set_stock_id (GTK_ACTION(action), icon_name);
    }
    else if ( gtk_icon_factory_lookup_default(icon_name)) {
      gtk_action_set_stock_id (GTK_ACTION(action), icon_name);
    }
    else {
      gtk_action_set_icon_name (GTK_ACTION(action), icon_name);
    }
  }
}