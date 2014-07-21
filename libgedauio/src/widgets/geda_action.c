/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
 *                | new function geda_action_label_init. Added property
 *                | PROP_ICON_ID and associated functions. Set property
 *                | handlers back to switch/case. Added GInstanceInitFunc.
 *                | (The geda_action_set_icon_name checks both the
 *                | theme-able search path and our icon factory for icons).
 * WEH | 03/11/14 | Renamed class geda instead of gschem and relocated code
 *                | to libgedauio (to declutter gschems src and more
 *                | importantly to make available to all geda-gaf programs.
 */
#include <config.h>

#include <geda.h>

#include <gtk/gtk.h>

#include "geda_action.h"
#include "geda_accel_label.h"
#include "geda_imagemenuitem.h"
#include "gettext.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/**
 * \brief GedaAction Action - An Action Widget for Menus
 * \par
 * A GedaAction is a variant of GtkAction but use a GedaAccelLabel insead
 * of a GtkAccelLabel and this allow menu items with multi-key assignments.
 *
 * \defgroup GedaAction Action Object
 * @{
 */

enum {
  PROP_MULTIKEY_ACCEL = 1,
  PROP_ICON_ID,
};

static GObjectClass *geda_action_parent_class = NULL;

/*! \brief GObject finalize handler
 *
 *  \par Function Description
 *  Just before the GtkAction GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void geda_action_finalize (GObject *object)
{
  GedaAction *action = GEDA_ACTION (object);

  if(action->multikey_accel) {
    g_free (action->multikey_accel);
  }
  g_free (action->icon_name);

  G_OBJECT_CLASS (geda_action_parent_class)->finalize (object);
}


/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GedaAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
geda_action_set_property (GObject *object, unsigned int property_id,
                          const    GValue *value, GParamSpec *pspec)
{
  GedaAction *action = GEDA_ACTION (object);

  switch (property_id)
  {
    case PROP_MULTIKEY_ACCEL:
      if(action->multikey_accel) {
        g_free (action->multikey_accel);
      }
      action->multikey_accel = g_value_dup_string (value);
      break;
    case PROP_ICON_ID:
      geda_action_set_icon_name(action, g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GedaAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
geda_action_get_property (GObject *object, unsigned int property_id,
                          GValue  *value,  GParamSpec   *pspec)
{
  GedaAction *action = GEDA_ACTION (object);

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
geda_action_connect_proxy (GtkAction *action, GtkWidget *proxy)
{
  GedaAction *gs_action = GEDA_ACTION (action);
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
                    "accel-string", gs_action->multikey_accel,
                    NULL);
      g_free(label_string);
    }
  }

  /* Let the parent class do its work now we've fiddled with the label */
  GTK_ACTION_CLASS (geda_action_parent_class)->connect_proxy (action, proxy);
}

/*! \brief Type class initialiser for GedaAction
 *
 *  \par Function Description
 *  Type class initialiser for GedaAction. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  klass       The GedaActionClass we are initialising
 */
static void geda_action_class_init (GedaActionClass *klass)
{
  GParamSpec     *params;
  GObjectClass   *gobject_class   = G_OBJECT_CLASS (klass);
  GtkActionClass *gtkaction_class = GTK_ACTION_CLASS (klass);

  gtkaction_class->connect_proxy  = geda_action_connect_proxy;

  gobject_class->finalize         = geda_action_finalize;
  gobject_class->set_property     = geda_action_set_property;
  gobject_class->get_property     = geda_action_get_property;

  geda_action_parent_class      = g_type_class_peek_parent (klass);

  params = g_param_spec_string ("multikey-accel",
                              _("multikey-accelerator"),
                              _("A string with characters in positions key characters"),
                                 NULL,
                               (G_PARAM_READWRITE));

  g_object_class_install_property( gobject_class, PROP_MULTIKEY_ACCEL, params);

  params = g_param_spec_string ("icon-id",
                              _("icon-identification"),
                              _("String name of the icon image"),
                                 NULL,
                               (G_PARAM_READWRITE));

  g_object_class_install_property( gobject_class, PROP_ICON_ID, params);
/*
  params = g_param_spec_string ("icon-id",
                              _("icon-identification"),
                              _("String name of the icon image"),
                                 NULL,
                               (G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
                                   PROP_TOOLBAR_STYLE,
                                   g_param_spec_enum ("toolbar-style",
                                                      P_("Toolbar Style"),
                                                      P_("How to draw the toolbar"),
                                                      GTK_TYPE_TOOLBAR_STYLE,
                                                      DEFAULT_TOOLBAR_STYLE,
                                                      GTK_PARAM_READWRITE));
*/
}

/*! \brief Initialize GedaAction data structure.
 *
 *  \par Function Description
 *  Function tois call after the GedaActionClass is created
 *  to initialize the data structure.
 *
 * \param [in] action A GedaAction object (structure)
 */
static void geda_action_init (GedaAction *action)
{
  action->multikey_accel = NULL;
  action->icon_name      = NULL;
}

/*! \brief Function to retrieve GedaAction's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve GedaAction's Type identifier.
 *  Upon first call, this registers the GedaAction in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return the Type identifier associated with GedaAction.
 */
unsigned int geda_action_get_type ()
{
  static unsigned int geda_action_type = 0;

  if (!geda_action_type) {
    static const GTypeInfo geda_action_info = {
      sizeof(GedaActionClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) geda_action_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(GedaAction),
      0,    /* n_preallocs */
      (GInstanceInitFunc) geda_action_init, /* instance_init */
    };

    geda_action_type = g_type_register_static (GTK_TYPE_ACTION,
                                                 "GedaAction",
                                                 &geda_action_info, 0);
  }

  return geda_action_type;
}

/*! \brief Creates a new Menu Item with an Action Object
 *  \par Function Description
 *  This function creates a geda menu item widget that proxies
 *  for the given action.
 *
 * \param [in] action A GedaAction object
 *
 * \returns: GtkWidget pointer to a new action menu item.
 */
GtkWidget *
geda_action_create_menu_item (GedaAction *action)
{
  GtkWidget *menu_item;
  GtkAction *parent_action;

  g_return_val_if_fail (GEDA_IS_ACTION (action), NULL);

  parent_action = (GtkAction*)action;

  menu_item = geda_image_menu_item_new ();

  gtk_activatable_set_use_action_appearance (GTK_ACTIVATABLE (menu_item), TRUE);
  gtk_activatable_set_related_action (GTK_ACTIVATABLE (menu_item), parent_action);

  return menu_item;
}

/*! \brief Creates a new Menu Item with an Action Object from Widget
 *
 *  \par Function Description
 *  This function is a convience funtion to type cast the source
 *  action from a Widget to an Action [widget].
 *
 * \param [in] widget A GedaAction object
 *
 * \returns: GtkWidget geda_action_create_menu_item().
 */
GtkWidget *
geda_action_widget_create_menu_item (GtkWidget *widget)
{

  return geda_action_create_menu_item(GEDA_ACTION (widget));
}

/*! \brief Creates a new GedaAction object
 *  \par Function Description
 *
 * Creates a new GedaAction object.
 *
 * \param [in] name            A unique name for the action
 * \param [in] label           The label displayed in menu items and on buttons, or NULL
 * \param [in] tooltip         A tooltip for the action, or NULL
 * \param [in] icon_id         The icon to display in widgets representing the action, or NULL
 * \param [in] multikey_accel  The (potentially) multi-key accelerator used for this action
 *
 * \returns A new GedaAction
 */
GedaAction *geda_action_new (const char *name,
                             const char *label,
                             const char *tooltip,
                             const char *icon_id,
                             const char *multikey_accel)
{
  GedaAction *action;

  g_return_val_if_fail (name != NULL, NULL);

  if (multikey_accel != NULL)
    action = g_object_new (GEDA_TYPE_ACTION, "name", name,
                                             "label", label,
                                             "tooltip", tooltip,
                                             "stock-id", icon_id,
                                             "multikey-accel", multikey_accel,
                                             NULL);
  else
    action = g_object_new (GEDA_TYPE_ACTION, "name", name,
                                             "label", label,
                                             "tooltip", tooltip,
                                             "stock-id", icon_id,
                                             NULL);
  return action;
}

/*! \brief GedaAction Get Icon Name
 *  \par Function Description
 *
 * Returns the name of the icon asociated with the GedaAction
 * object.
 *
 * \param [in] action A GedaAction object
 *
 * \returns const char* to the icon name;
 */
const char *geda_action_get_icon_name (GedaAction *action)
{
  g_return_val_if_fail (GEDA_IS_ACTION (action), NULL);
  return action->icon_name;
}

/*! \brief GedaAction Set Icon Name
 *  \par Function Description
 *
 * Set the name of the icon asociated with the GedaAction
 * object.
 *
 * \param [in] action    A GedaAction object
 * \param [in] icon_name Point to string containing the icon name
 *
 * \returns const char* to the icon name;
 */
void geda_action_set_icon_name (GedaAction *action,
                                const char *icon_name)
{
  GtkStockItem stock_info;
  GtkAction   *parent_action;

  g_return_if_fail (GEDA_IS_ACTION (action));

  parent_action = (GtkAction*)action;

  g_free (action->icon_name);
  action->icon_name = NULL;

  action->icon_name = g_strdup (icon_name);

  if (icon_name != NULL) {
    if ( gtk_stock_lookup(icon_name, &stock_info)) {
      gtk_action_set_stock_id (parent_action, icon_name);
    }
    else if ( gtk_icon_factory_lookup_default(icon_name)) {
      gtk_action_set_stock_id (parent_action, icon_name);
    }
    else {
      gtk_action_set_icon_name (parent_action, icon_name);
    }
  }
}
/** @} end group GedaAction */
