/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_action.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This Library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
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
 * WEH | 03/11/14 | Include relative path in local includes, revise function
 *                | geda_action_create_menu_item to process GedaToggleAction
 *                | widget for convenience.
 * WEH | 09/04/16 | Revise geda_action_connect_proxy to set properties of
 *                | existent GedaAccelLabels and reduce scope of variable.
 *                | Revise geda_action_create_menu_item to use g_object_set
 *                | instead of gtk_activatable_set_use_action_appearance
 *                | to set the "use-action-appearance" property.
 * WEH | 09/04/16 | Implement object identication system using a hash table.
 * WEH | 04/21/17 | Eliminate call to gtk_activatable_set_related_action in
 *                | geda_action_create_menu_item, set "related-action" in the
 *                | same g_object_set as "use-action-appearance". Replace
 *                | g_type_check_instance_cast with static cast.
 *
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

#include "../../include/geda_action.h"
#include "../../include/geda_accel_label.h"
#include "../../include/geda_check_menu_item.h"
#include "../../include/geda_image_menu_item.h"
#include "../../include/geda_toggle_action.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaAction - An Action Widget for Menus
 * \par
 * A #GedaAction is a variant of GtkAction but use a GedaAccelLabel insead
 * of a GtkAccelLabel and this allow menu items with multi-key assignments.
 *
 * \defgroup GedaAction Action Object
 * @{
 */

enum {
  PROP_MULTIKEY_ACCEL = 1,
  PROP_ICON_ID,
};

static void *geda_action_parent_class = NULL;

static GHashTable *action_hash_table = NULL;

/** \defgroup geda-action-govo GedaAction GObject Virtual Overrides
  * @{
  */

/*!
 * \brief GObject finalize handler
 * \par Function Description
 *  Called just before the Action object is destroyed to release
 *  resources. Chains up to the parent's finalize handler.
 *
 * \param [in] object The GObject being finalized.
 */
static void geda_action_finalize (GObject *object)
{
  GedaAction *action = (GedaAction*)object;

  if (g_hash_table_remove (action_hash_table, object)) {
    if (!g_hash_table_size (action_hash_table)) {
      g_hash_table_destroy (action_hash_table);
      action_hash_table = NULL;
    }
  }

  if (action->multikey_accel) {
    g_free (action->multikey_accel);
  }

  if (action->icon_name) {
    g_free (action->icon_name);
  }

  ((GObjectClass*)geda_action_parent_class)->finalize (object);
}

/*!
 * \brief GObject property setter function
 * \par Function Description
 *  Setter function for GedaAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 * \param [in]  object       The GObject whose properties we are setting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [in]  value        The GValue the property is being set from
 * \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
geda_action_set_property (GObject *object, unsigned int property_id,
                          const    GValue *value, GParamSpec *pspec)
{
  GedaAction *action = (GedaAction*)object;

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

/*!
 * \brief GObject property getter function
 * \par Function Description
 *  Getter function for GedaAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 * \param [in]  object       The GObject whose properties we are getting
 * \param [in]  property_id  The numeric id. under which the property was
 *                           registered with g_object_class_install_property()
 * \param [out] value        The GValue in which to return the value of the property
 * \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
geda_action_get_property (GObject *object, unsigned int property_id,
                          GValue  *value,  GParamSpec   *pspec)
{
  GedaAction *action = (GedaAction*)object;

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

/** @} geda-action-govo */

/** \defgroup geda-action-pcvo GedaAction Parent Class Virtual Overrides
  * @{
  */

static void
geda_action_connect_proxy (GtkAction *action, GtkWidget *proxy)
{
  /* Override the type of label widget used with the menu item */
  if (GEDA_IS_MENU_ITEM (proxy)) {

    GedaAction *geda_action;
    GtkWidget  *label;

    geda_action = (GedaAction*)action;
    label       = geda_get_child_widget(proxy);

    /* Ensure label is a GedaAccelLabel */
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
                    "visible", TRUE,
                    "parent", proxy,
                    "label", label_string,
                    "accel-string", geda_action->multikey_accel,
                    NULL);
      g_free(label_string);
    }
    else {

      g_object_set (label,
                    "use-underline", TRUE,
                    "xalign", 0.0,
                    "visible", TRUE,
                    "accel-string", geda_action->multikey_accel,
                    NULL);
    }
  }

  /* Let the parent class do its work now we've fiddled with the label */
  ((GtkActionClass*)geda_action_parent_class)->connect_proxy (action, proxy);
}

/** @} geda-action-pcvo */

/*!
 * \brief GedaAction Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class       GedaAction class we are initializing
 * \param [in]  class_data  GedaAction structure associated with the class
 */
static void
geda_action_class_init(void *class, void *class_data)
{
  GParamSpec     *params;
  GObjectClass   *object_class    = (GObjectClass*) class;
  GtkActionClass *gtkaction_class = (GtkActionClass*) class;

  gtkaction_class->connect_proxy  = geda_action_connect_proxy;

  object_class->finalize          = geda_action_finalize;
  object_class->set_property      = geda_action_set_property;
  object_class->get_property      = geda_action_get_property;

  geda_action_parent_class        = g_type_class_peek_parent (class);

  params = g_param_spec_string ("multikey-accel",
                              _("multikey accelerator"),
                              _("A string with characters in positions key characters"),
                                 NULL,
                               (G_PARAM_READWRITE));

  g_object_class_install_property( object_class, PROP_MULTIKEY_ACCEL, params);

  params = g_param_spec_string ("icon-id",
                              _("icon identification"),
                              _("String name of the icon image"),
                                 NULL,
                               (G_PARAM_READWRITE));

  g_object_class_install_property (object_class, PROP_ICON_ID, params);
}

/*!
 * \brief Initialize new GedaAction data structure instance.
 * \par Function Description
 *  This function is call after the GedaActionClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaAction data structure
 * \param [in] class     A GedaActionClass Object
 */
static void
geda_action_instance_init (GTypeInstance *instance, void *class)
{
  GedaAction *action     = (GedaAction*)instance;

  action->multikey_accel = NULL;
  action->icon_name      = NULL;

  if (!action_hash_table) {
    action_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (action_hash_table, instance, instance);
}

/*!
 * \brief Function to retrieve GedaAction's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaAction Type identifier. When
 *  first called, the function registers a #GedaAction in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaAction and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaAction.
 */
GedaType geda_action_get_type (void)
{
  static volatile GedaType geda_action_type = 0;

  if (g_once_init_enter (&geda_action_type)) {

    static const GTypeInfo info = {
      sizeof(GedaActionClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_action_class_init,          /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaAction),
      0,                               /* n_preallocs         */
      geda_action_instance_init        /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaAction");
    type   = g_type_register_static (GTK_TYPE_ACTION, string, &info, 0);

    g_once_init_leave (&geda_action_type, type);
  }

  return geda_action_type;
}

/*!
 * \brief Check if an object is a GedaAction
 * \par Function Description
 *  Determines if \a action is valid by verifying \a action is
 *  included in the hash table of GedaAction objects.
 *
 * \return TRUE if \a action is a valid GedaAction
 */
bool
is_a_geda_action (GedaAction *action)
{
  if ((action != NULL) && (action_hash_table != NULL)) {
    return g_hash_table_lookup(action_hash_table, action) ? TRUE : FALSE;
  }

  return FALSE;
}

/*!
 * \brief Creates a new Menu Item with an Action Object
 * \par Function Description
 *  This function creates a geda menu item widget that proxies
 *  for the given action.
 *
 *  As a convenience, this function also accepts GedaToggleAction
 *  as the \a action argument but this may require a cast.
 *
 * \param [in] action A GedaAction object
 *
 * \returns GtkWidget pointer to a new action menu item.
 */
GtkWidget *geda_action_create_menu_item (GedaAction *action)
{
  GtkWidget *menu_item;

  if (GEDA_IS_TOGGLE_ACTION(action)) {

    menu_item = geda_check_menu_item_new ();

  }
  else if (GEDA_IS_ACTION (action)) {

    menu_item = geda_image_menu_item_new ();

  }

  else {
    fprintf(stderr, "%s: Error: invalid action\n",__func__);
    menu_item = NULL;
  }

  if (menu_item) {

    GedaMenuItem *item = (GedaMenuItem*)menu_item;

    geda_menu_item_set_use_action_appearance(item, TRUE);
    geda_menu_item_set_related_action(item, (GtkAction*)action);

    action->action_name = gtk_action_get_name((GtkAction*)action);
  }

  return menu_item;
}

/*!
 * \brief Creates a new Menu Item with an Action Object from Widget
 * \par Function Description
 *  This function is a convience funtion to type cast the source
 *  action from a Widget to an Action [widget].
 *
 * \param [in] widget A GedaAction object
 *
 * \returns GtkWidget geda_action_create_menu_item().
 */
GtkWidget*
geda_action_widget_create_menu_item (GtkWidget *widget)
{
  return geda_action_create_menu_item((GedaAction*)widget);
}

/*!
 * \brief Creates a new GedaAction object
 * \par Function Description
 *  Creates a new GedaAction object.
 *
 * \param [in] name            A unique name for the action
 * \param [in] label           The label displayed in menu items and on buttons, or NULL
 * \param [in] tooltip         A tooltip for the action, or NULL
 * \param [in] icon_id         The icon to display in widgets representing the action, or NULL
 * \param [in] multikey_accel  The (potentially) multi-key accelerator used for this action
 *
 * \returns A new GedaAction
 */
GedaAction *
geda_action_new (const char *name,
                 const char *label,
                 const char *tooltip,
                 const char *icon_id,
                 const char *multikey_accel)
{
  GedaAction *action;

  g_return_val_if_fail (name != NULL, NULL);

  if (multikey_accel != NULL) {
    action = g_object_new (GEDA_TYPE_ACTION, "name", name,
                                             "label", label,
                                             "tooltip", tooltip,
                                             "stock-id", icon_id,
                                             "multikey-accel", multikey_accel,
                                             NULL);
  }
  else {
    action = g_object_new (GEDA_TYPE_ACTION, "name", name,
                                             "label", label,
                                             "tooltip", tooltip,
                                             "stock-id", icon_id,
                                             NULL);
  }

  return action;
}

/*!
 * \brief Activate a GedaAction
 * \par Function Description
 *  Calls base class to emits the "activate" signal on the \a action,
 *  if it is not insensitive. This gets called by the proxy widgets
 *  when they get activated and can also be used to manually activate
 *  an action.
 *
 * \param [in] action A GedaAction object
 */
void
geda_action_activate (GedaAction *action)
{
  gtk_action_activate ((GtkAction*)action);
}

/*!
 * \brief GedaAction Get Action Name
 * \par Function Description
 *  Returns the name of the action associated with the GedaAction object.
 *
 * \param [in] action A GedaAction object, which could be a GedaToggleAction
 *
 * \returns const char * to the action name;
 */
const char *geda_action_get_action_name (GedaAction *action)
{
  if (GEDA_IS_ACTION (action)) {
    return action->action_name;
  }

  return NULL;
}

/*!
 * \brief GedaAction Get Icon Name
 * \par Function Description
 *  Returns the name of the icon associated with the GedaAction object.
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

/*!
 * \brief GedaAction Set Icon Name
 * \par Function Description
 *  Set the name of the icon associated with the GedaAction
 *  object.
 *
 * \param [in] action    A GedaAction object
 * \param [in] icon_name Point to string containing the icon name
 *
 * \returns const char* to the icon name;
 */
void geda_action_set_icon_name (GedaAction *action, const char *icon_name)
{
  GtkStockItem stock_info;
  GtkAction   *parent_action;

  g_return_if_fail (GEDA_IS_ACTION (action));

  parent_action = (GtkAction*)action;

  if (action->icon_name) {
    g_free (action->icon_name);
  }

  action->icon_name = geda_strdup (icon_name);

  if (icon_name != NULL) {
    if (gtk_stock_lookup(icon_name, &stock_info)) {
      gtk_action_set_stock_id (parent_action, icon_name);
    }
    else if (gtk_icon_factory_lookup_default(icon_name)) {
      gtk_action_set_stock_id (parent_action, icon_name);
    }
    else {
      gtk_action_set_icon_name (parent_action, icon_name);
    }
  }
}

/*!
 * \brief Disconnect accelerator from a GedaAction
 * \par Function Description
 *  Disconnects \a action from any accelerator group with which
 *  the action is associated, if the action is not associated
 *  with an accelerator group then nothing is done.
 *
 * \param [in] action A GedaAction object
 */
void geda_action_disconnect_accelerator (GedaAction *action)
{
  g_return_if_fail (GTK_IS_ACTION(action));

  gtk_action_disconnect_accelerator((GtkAction*)action);
}

/*!
 * \brief Synchronize Visibility of Proxy widget with GedaAction
 * \par Function Description
 *  Set the visibility of \a proxy to the visibility of \a action.
 *  If \a action is NULL, set the visibility of \a proxy to the
 *  visibility of the related activatable if set, otherwise the
 *  visibility of \a proxy is set to visible.
 *
 * \param [in] action A GedaAction object
 * \param [in] proxy  menu_item widget
 * \param [in] empty  flag to over-ride visible if "hide-if-empty"
 *                    property is set on related actions.
 */
void
geda_action_sync_menu_visible(GedaAction *action, GtkWidget *proxy, bool empty)
{
  GtkAction *object;

  bool hide_if_empty;
  bool visible;

  g_return_if_fail (GTK_IS_WIDGET(proxy));

  /* A Menu object for a popup does not have to have an action */
  if (action == NULL) {
    object = gtk_activatable_get_related_action (GTK_ACTIVATABLE(proxy));
  }
  else {
    object = NULL;
  }

  if (object) {
    g_object_get(object, "hide-if-empty", &hide_if_empty,
                         "visible", &visible, NULL);
  }
  else {
    hide_if_empty = TRUE;
    visible       = TRUE;
  }

  if (visible && !(empty && hide_if_empty)) {
    gtk_widget_show (proxy);
  }
  else {
    gtk_widget_hide (proxy);
  }
}

/** @} end group GedaAction */
