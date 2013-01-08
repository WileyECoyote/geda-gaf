/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1912-2013 Ales Hvezda
 * Copyright (C) 1912-2013 gEDA Contributors (see ChangeLog for details)
 *
 * Date: December 25, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
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
 * \Note: We do all this for the Multi Keys Characters in the Menus, (because
 *        We really Like them,) otherwise we could have just used the stock
 *        gtk_toggle_action.
 */
#include <config.h>
#include <geda.h>

#include <glib.h>
#include <gtk/gtk.h>
#include <gtk/gtkprivate.h>

#include <widgets.h>

enum {
  PROP_0,
  PROP_DRAW_AS_RADIO,
  PROP_ACTIVE,
  PROP_MULTIKEY_ACCEL
};

static char *ptr_multikey_accel = NULL;
static GObjectClass *gschem_toggle_action_parent_class = NULL;

static void gschem_toggle_action_set_property (GObject      *object,
                                               unsigned int  property_id,
                                               const GValue *value,
                                               GParamSpec   *pspec);
static void gschem_toggle_action_get_property (GObject      *object,
                                               unsigned int  property_id,
                                               GValue       *value,
                                               GParamSpec   *pspec);

/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the GschemAction GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void gschem_toggle_action_finalize (GObject *object)
{
  g_free (ptr_multikey_accel);
  G_OBJECT_CLASS (gschem_toggle_action_parent_class)->finalize (object);
}

/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GschemToggleAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
gschem_toggle_action_set_property (GObject *object,
                                   unsigned int property_id,
                                   const GValue *value, GParamSpec *pspec)
{
  if(property_id == PROP_MULTIKEY_ACCEL) {
    g_free (ptr_multikey_accel);
    ptr_multikey_accel = g_strdup (g_value_get_string (value));
  }
}

/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GschemToggleAction's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
gschem_toggle_action_get_property (GObject *object, unsigned int property_id,
                                   GValue  *value,  GParamSpec  *pspec)
{
  if(property_id == PROP_MULTIKEY_ACCEL)
    g_value_set_string (value, ptr_multikey_accel);
}

static void
gschem_toggle_action_connect_proxy (GtkAction *action, GtkWidget *proxy)
{
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
                    "accel-string", ptr_multikey_accel,
                    NULL);
    }
  }

  /* Let the parent class do its work now we've fiddled with the label */
  GTK_ACTION_CLASS ( gschem_toggle_action_parent_class)->connect_proxy (action, proxy);
}

static void
gschem_toggle_action_class_init (GschemToggleActionClass *klass)
{
  GObjectClass   *gobject_class;
  GtkActionClass *action_class;

  gschem_toggle_action_parent_class = g_type_class_peek_parent (klass);

  gobject_class = G_OBJECT_CLASS (klass);
  action_class  = GTK_ACTION_CLASS (klass);

  action_class->connect_proxy = gschem_toggle_action_connect_proxy;

  gobject_class->finalize     = gschem_toggle_action_finalize;
  gobject_class->set_property = gschem_toggle_action_set_property;
  gobject_class->get_property = gschem_toggle_action_get_property;

  g_object_class_install_property ( gobject_class,
                                    PROP_MULTIKEY_ACCEL,
                                    g_param_spec_string ("multikey-accel",
                                    "",
                                    "",
                                    NULL,
                                    G_PARAM_READWRITE));

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
GType gschem_toggle_action_get_type ()
{
  static GType gschem_toggle_action_type = 0;

  if (!gschem_toggle_action_type) {
    static const GTypeInfo gschem_toggle_action_info = {
      sizeof(GschemToggleActionClass),
      NULL, /* base_init      */
      NULL, /* base_finalize  */
      (GClassInitFunc) gschem_toggle_action_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data     */
      sizeof(GschemToggleAction),
      0,    /* n_preallocs    */
      NULL, /* instance_init  */
    };

    gschem_toggle_action_type = g_type_register_static (GTK_TYPE_TOGGLE_ACTION,
                                                        "GschemToggleAction",
                                                        &gschem_toggle_action_info, 0);
  }

  return gschem_toggle_action_type;
}
/**
 * gschem_toggle_action_new:
 * @name: A unique name for the action
 * @label: (allow-none): The label displayed in menu items and on buttons, or %NULL
 * @tooltip: (allow-none): A tooltip for the action, or %NULL
 * @stock_id: The stock icon to display in widgets representing the action, or %NULL
 *
 * Return value: a new GschemToggleAction
 *
 * Since: 2.4
 */
GschemToggleAction *
gschem_toggle_action_new (const char *name,
                          const char *label,
                          const char *tooltip,
                          const char *stock_id,
                          const char *multikey_accel)
{
  g_return_val_if_fail (name != NULL, NULL);
  ptr_multikey_accel = (char *)multikey_accel;
  return g_object_new (GSCHEM_TYPE_TOGGLE_ACTION, "name", name,
                                                  "label", label,
                                                  "tooltip", tooltip,
                                                  "stock-id", stock_id,
                                                  "multikey-accel", multikey_accel,
                                                   NULL);
}

#define __GSCHEM_TOGGLE_ACTION_C__
