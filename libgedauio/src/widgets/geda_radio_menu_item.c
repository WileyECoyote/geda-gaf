/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*! \file geda_radio_menu_item.c
 *  \brief GedaRadioMenuItem Class Module
 */

/** \defgroup geda-radio-menu-item Geda Radio Menu Item
 * @{
 * \brief Implmentation of GedaRadioMenuItem Class
 *
 * \class GedaRadioMenuItem geda_radio_menu_item.h "include/geda_radio_menu_item.h"
 * \implements GedaCheckMenuItem
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_uio_functions.h"
#include "../../include/geda_accel_label.h"
#include "../../include/geda_menu_enum.h"
#include "../../include/geda_label.h"
#include "../../include/geda_radio_menu_item.h"
#include "../../include/gettext.h"

enum {
  PROP_0,
  PROP_GROUP
};

//static void geda_radio_menu_item_destroy        (GtkObject      *object);
static void geda_radio_menu_item_activate       (GedaMenuItem   *menu_item);
static void geda_radio_menu_item_set_property   (GObject        *object,
                                                 unsigned int    prop_id,
                                                 const GValue   *value,
                                                 GParamSpec     *pspec);
static void geda_radio_menu_item_get_property   (GObject        *object,
                                                 unsigned int    prop_id,
                                                 GValue         *value,
                                                 GParamSpec     *pspec);

static unsigned int group_changed_signal = 0;

static void *geda_radio_menu_item_parent_class = NULL;

static GHashTable *radio_menu_hash = NULL;

/* menu_item_class->activate */
static void geda_radio_menu_item_activate (GedaMenuItem *menu_item)
{
  GedaRadioMenuItem *radio_menu_item = GEDA_RADIO_MENU_ITEM (menu_item);
  GedaCheckMenuItem *check_menu_item = GEDA_CHECK_MENU_ITEM (menu_item);
  GedaCheckMenuItem *tmp_menu_item;
  GtkAction         *action;
  GSList            *tmp_list;
  bool               toggled;

  action = gtk_activatable_get_related_action (GTK_ACTIVATABLE (menu_item));

  if (action && geda_menu_item_get_submenu_widget (menu_item) == NULL) {
    gtk_action_activate (action);
  }

  toggled = FALSE;

  if (check_menu_item->active) {

    tmp_menu_item = NULL;
    tmp_list = radio_menu_item->group;

    while (tmp_list) {

      tmp_menu_item = tmp_list->data;
      tmp_list = tmp_list->next;

      if (tmp_menu_item->active && (tmp_menu_item != check_menu_item)) {
        break;
      }

      tmp_menu_item = NULL;
    }

    if (tmp_menu_item) {

      toggled = TRUE;
      check_menu_item->active = !check_menu_item->active;
    }
  }
  else {

    toggled = TRUE;
    check_menu_item->active = !check_menu_item->active;

    tmp_list = radio_menu_item->group;

    while (tmp_list) {

      tmp_menu_item = tmp_list->data;
      tmp_list = tmp_list->next;

      if (tmp_menu_item->active && (tmp_menu_item != check_menu_item))
      {
        geda_menu_item_activate (GEDA_MENU_ITEM (tmp_menu_item));
        break;
      }
    }
  }

  if (toggled) {

    geda_check_menu_item_toggled (check_menu_item);
  }

  gtk_widget_queue_draw (GTK_WIDGET (radio_menu_item));
}

static void geda_radio_menu_item_set_property (GObject      *object,
                                               unsigned int  prop_id,
                                               const GValue *value,
                                               GParamSpec   *pspec)
{
  GedaRadioMenuItem *radio_menu_item;

  radio_menu_item = GEDA_RADIO_MENU_ITEM (object);

  switch (prop_id) {

      GSList *slist;

    case PROP_GROUP:
      if (G_VALUE_HOLDS_OBJECT (value))
        slist = geda_radio_menu_item_get_group ((GedaRadioMenuItem*) g_value_get_object (value));
      else
        slist = NULL;
      geda_radio_menu_item_set_group (radio_menu_item, slist);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void geda_radio_menu_item_get_property (GObject      *object,
                                               unsigned int  prop_id,
                                               GValue       *value,
                                               GParamSpec   *pspec)
{
  switch (prop_id) {

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_radio_menu_item_destroy (GtkObject *object)
{
  GedaRadioMenuItem *radio_menu_item     = GEDA_RADIO_MENU_ITEM (object);
  GtkWidget         *old_group_singleton = NULL;
  GSList            *tmp_list;
  bool               was_in_group;

  was_in_group = radio_menu_item->group && radio_menu_item->group->next;

  radio_menu_item->group = g_slist_remove (radio_menu_item->group,
                                           radio_menu_item);

  if (radio_menu_item->group && !radio_menu_item->group->next) {
    old_group_singleton = radio_menu_item->group->data;
  }

  tmp_list = radio_menu_item->group;

  while (tmp_list) {

      GedaRadioMenuItem *tmp_menu_item;

      tmp_menu_item        = tmp_list->data;
      tmp_list             = tmp_list->next;
      tmp_menu_item->group = radio_menu_item->group;
  }

  /* this radio menu item is no longer in the group */
  radio_menu_item->group = NULL;

  if (old_group_singleton) {
    g_signal_emit (old_group_singleton, group_changed_signal, 0);
  }

  if (was_in_group) {
    g_signal_emit (radio_menu_item, group_changed_signal, 0);
  }

#if GTK_MAJOR_VERSION < 3

  GTK_OBJECT_CLASS (geda_radio_menu_item_parent_class)->destroy (object);

#else

  GTK_WIDGET_CLASS (geda_radio_menu_item_parent_class)->destroy (object);

#endif
}

static void geda_radio_menu_item_finalize (GObject *object)
{
  //GedaRadioMenuItem *radio_menu_item = GEDA_RADIO_MENU_ITEM (object);

  if (g_hash_table_remove (radio_menu_hash, object)) {
    if (!g_hash_table_size (radio_menu_hash)) {
      g_hash_table_destroy (radio_menu_hash);
      radio_menu_hash = NULL;
    }
  }

  G_OBJECT_CLASS (geda_radio_menu_item_parent_class)->finalize (object);
}

/*!
 * \brief GedaRadioMenuItem Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class       GedaRadioMenuItemClass class being initializes
 * \param [in]  class_data  GedaRadioMenuItem structure associated with the class
 */
static void geda_radio_menu_item_class_init(void *class, void *class_data)
{
  GObjectClass      *gobject_class;
  GedaMenuItemClass *menu_item_class;
  GParamSpec        *params;

  gobject_class   = G_OBJECT_CLASS (class);
  menu_item_class = GEDA_MENU_ITEM_CLASS (class);

  gobject_class->finalize     = geda_radio_menu_item_finalize;
  gobject_class->set_property = geda_radio_menu_item_set_property;
  gobject_class->get_property = geda_radio_menu_item_get_property;

#if GTK_MAJOR_VERSION < 3

  GtkObjectClass *object_class;

  object_class = (GtkObjectClass*)class;

  object_class->destroy = geda_radio_menu_item_destroy;

#else

  GtkWidgetClass *widget_class;

  widget_class = (GtkWidgetClass*)class;

  widget_class->destroy = geda_radio_menu_item_destroy;

#endif

  /*!
   * property GedaRadioMenuItem::group
   * The radio menu item whose group this widget belongs to.
   */
  params = g_param_spec_object ("group",
                              _("Group"),
                              _("The radio menu item whose group this widget belongs to."),
                                 GEDA_TYPE_RADIO_MENU_ITEM,
                                 G_PARAM_WRITABLE);

  g_object_class_install_property (gobject_class, PROP_GROUP, params);

  menu_item_class->activate = geda_radio_menu_item_activate;

  geda_radio_menu_item_parent_class = g_type_class_peek_parent(class);

  /*!
   * property GedaRadioMenuItem::group-changed:
   * Emitted when the group of radio menu items that a radio menu item
   * belongs to changes. This is emitted when a radio menu item switches
   * from being alone to being part of a group of 2 or more menu items,
   * or vice-versa, and when a button is moved from one group of 2 or
   * more menu items to a different one, but not when the composition
   * of the group that a menu item belongs to changes.
   */
  group_changed_signal = g_signal_new ("group-changed",
                                       GEDA_TYPE_RADIO_MENU_ITEM,
                                       G_SIGNAL_RUN_FIRST,
                                       G_STRUCT_OFFSET (GedaRadioMenuItemClass, group_changed),
                                       NULL, NULL,
                                       geda_marshal_VOID__VOID,
                                       G_TYPE_NONE, 0);
}

/*!
 * \brief Initialize new GedaRadioMenuItem data structure instance.
 * \par Function Description
 *  This function is called after the GedaRadioMenuItemClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaRadioMenuItem data structure
 * \param [in] class     A GedaRadioMenuItem Object
 */
static void geda_radio_menu_item_instance_init(GTypeInstance *instance, void *class)
{
  GedaRadioMenuItem *radio_menu_item = (GedaRadioMenuItem*)instance;

  radio_menu_item->group = g_slist_prepend (NULL, radio_menu_item);

  if (!radio_menu_hash) {
    radio_menu_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (radio_menu_hash, instance, instance);

  geda_check_menu_item_set_draw_as_radio ((GedaCheckMenuItem*)radio_menu_item, TRUE);
}

/*!
 * \brief Retrieve GedaRadioMenuItem's Type identifier.
 * \par Function Description
 *  Function to retrieve a GedaRadioMenuItemType identifier. When
 *  first called, the function registers a #GedaRadioMenuItem in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaRadioMenuItem and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaRadioMenuItem.
 */
GedaType geda_radio_menu_item_get_type (void)
{
  static volatile GedaType geda_radio_menu_item_type = 0;

  if (g_once_init_enter (&geda_radio_menu_item_type)) {

    static const GTypeInfo info = {
      sizeof(GedaRadioMenuItemClass),
      NULL,                                /* base_init           */
      NULL,                                /* base_finalize       */
      geda_radio_menu_item_class_init,     /* (GClassInitFunc)    */
      NULL,                                /* class_finalize      */
      NULL,                                /* class_data          */
      sizeof(GedaRadioMenuItem),
      0,                                   /* n_preallocs         */
      geda_radio_menu_item_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaRadioMenuItem");
    type   = g_type_register_static (GEDA_TYPE_CHECK_MENU_ITEM, string, &info, 0);

    g_once_init_leave (&geda_radio_menu_item_type, type);
  }

  return geda_radio_menu_item_type;
}

/*!
 * \brief Check if an object is a GedaRadioMenuItem
 * \par Function Description
 *  Determines if \a radio_menu_item is valid by verifying \a radio_menu_item
 *  is included in the hash table of GedaRadioMenuItem objects.
 *
 * \return TRUE if \a radio_menu_item is a valid GedaRadioMenuItem
 */
bool is_a_geda_radio_menu_item (GedaRadioMenuItem *radio_menu_item)
{
  if ((radio_menu_item != NULL) && (radio_menu_hash != NULL)) {
    return g_hash_table_lookup(radio_menu_hash, radio_menu_item) ? TRUE : FALSE;
  }

  return FALSE;
}

/*!
 * \brief Create a New GedaRadioMenuItem Object
 * \par Function Description
 *  Creates a new GedaRadioMenuItem adding it to \a group, which could
 *  be NULL.
 *
 * \param[in] group optional group the radio menu item is to be a member of
 *
 * \returns The new #GedaRadioMenuItem
 */
GtkWidget *geda_radio_menu_item_new (GSList *group)
{
  GedaRadioMenuItem *radio_menu_item;

  radio_menu_item = g_object_new (GEDA_TYPE_RADIO_MENU_ITEM, NULL);

  geda_radio_menu_item_set_group (radio_menu_item, group);

  return GTK_WIDGET (radio_menu_item);
}

/*!
 * \brief Create a New GedaRadioMenuItem Object in the same group as widget
 * \par Function Description
 *  Creates a new #GedaRadioMenuItem adding it to the same group as \a group.
 *
 * \param[in] group group the radio menu item is to be a member of
 *
 * \returns The new #GedaRadioMenuItem
 */
GtkWidget *geda_radio_menu_item_new_from_widget (GtkWidget *group)
{
  GSList *list = NULL;

  if (GEDA_IS_CHECK_MENU_ITEM(group)) {
    list = geda_radio_menu_item_get_group ((GedaRadioMenuItem*)group);
  }

  return geda_radio_menu_item_new (list);
}

/*!
 * \brief Create a New GedaRadioMenuItem Object with a given Label
 * \par Function Description
 * Creates a new #GedaRadioMenuItem whose child is a simple GedaLabel.
 *
 * \param[in] group group the radio menu item is to be a member of
 * \param[in] label the text for the label
 *
 * \returns A new #GedaRadioMenuItem
 */
GtkWidget *geda_radio_menu_item_new_with_label (GSList *group, const char *label)
{
  GtkWidget *radio_menu_item;
  GtkWidget *accel_label;

  radio_menu_item = geda_radio_menu_item_new (group);
  accel_label     = geda_accel_label_new (label);

  gtk_misc_set_alignment (GTK_MISC (accel_label), 0.0, 0.5);
  geda_container_add (radio_menu_item, accel_label);
  geda_accel_label_set_accel_widget (GEDA_ACCEL_LABEL (accel_label), radio_menu_item);
  gtk_widget_show (accel_label);

  return radio_menu_item;
}

/*!
 * \brief Create a New GedaRadioMenuItem Object with a given Mnemonic Label
 * \par Function Description
 * Creates a new #GedaRadioMenuItem containing a label. The label
 * will be created using geda_label_new_with_mnemonic(), so underscores
 * in \a label indicate the mnemonic for the menu item.
 *
 * \param[in] group group the radio menu item is to be a member of
 * \param[in] label the text of the button, with an underscore in front
 *                  of the mnemonic character and must not be NULL.
 *
 * \returns a new #GedaRadioMenuItem
 */
GtkWidget *geda_radio_menu_item_new_with_mnemonic (GSList *group, const char *label)
{
  GtkWidget *radio_menu_item;
  GtkWidget *accel_label;

  radio_menu_item = geda_radio_menu_item_new (group);
  accel_label     = g_object_new (GEDA_TYPE_ACCEL_LABEL, NULL);

  geda_label_set_mnemonic_text ((GedaLabel*)accel_label, label);
  gtk_misc_set_alignment ((GtkMisc*)accel_label, 0.0, 0.5);

  geda_container_add (radio_menu_item, accel_label);
  geda_accel_label_set_accel_widget ((GedaAccelLabel*)accel_label, radio_menu_item);
  gtk_widget_show (accel_label);

  return radio_menu_item;
}

/*!
 * \brief Create a New GedaRadioMenuItem with a given Mnemonic Label from widget
 * \par Function Description
 * Creates a new GedaRadioMenuItem containing a label. The label will be
 * created using geda_label_new_with_mnemonic(), so underscores in label
 * indicate the mnemonic for the menu item.
 *
 * The new #GedaRadioMenuItem is added to the same group as \a group.
 *
 * \param[in] group group the radio menu item is to be a member of
 * \param[in] label the text of the button, with an underscore in front
 *                  of the mnemonic character
 *
 * \returns The new #GedaRadioMenuItem
 */
GtkWidget *geda_radio_menu_item_new_with_mnemonic_from_widget (GtkWidget  *group,
                                                               const char *label)
{
  GSList *list = NULL;

  g_return_val_if_fail (GEDA_IS_RADIO_MENU_ITEM (group), NULL);

  if (group) {
    list = geda_radio_menu_item_get_group ((GedaRadioMenuItem*)group);
  }

  return geda_radio_menu_item_new_with_mnemonic (list, label);
}

/*!
 * \brief Create a New GedaRadioMenuItem Object with a given Label from widget
 * \par Function Description
 * Creates a new GedaRadioMenuItem whose child is a simple GedaLabel.
 * The new #GedaRadioMenuItem is added to the same group as \a group.
 *
 * \param[in] group group the radio menu item is to be a member of
 * \param[in] label the text for the label
 *
 * \returns The new #GedaRadioMenuItem
 */
GtkWidget *geda_radio_menu_item_new_with_label_from_widget (GtkWidget  *group,
                                                            const char *label)
{
  GSList *list = NULL;

  g_return_val_if_fail (GEDA_IS_RADIO_MENU_ITEM (group), NULL);

  if (group) {
    list = geda_radio_menu_item_get_group ((GedaRadioMenuItem*)group);
  }

  return geda_radio_menu_item_new_with_label (list, label);
}

/*!
 * \brief Get the Group associated with a GedaRadioMenuItem
 * \par Function Description
 * Returns the group to which the radio menu item belongs, as a GList
 * of #GedaRadioMenuItem. The list belongs to the widget and should not
 * be freed.
 *
 * \param[in] radio_menu_item  The GedaRadioMenuItem
 *
 * \returns the group of \a radio_menu_item
 */
GSList *geda_radio_menu_item_get_group (GedaRadioMenuItem *radio_menu_item)
{
  g_return_val_if_fail (GEDA_IS_RADIO_MENU_ITEM (radio_menu_item), NULL);

  return radio_menu_item->group;
}

/*!
 * \brief Set the Group associated with a GedaRadioMenuItem
 * \par Function Description
 * Sets the group to which the radio menu item belongs.
 *
 * \param[in]     radio_menu_item The GedaRadioMenuItem
 * \param[in,out] group           The group
 */
void geda_radio_menu_item_set_group (GedaRadioMenuItem *radio_menu_item,
                                     GSList            *group)
{
  GtkWidget *old_group_singleton = NULL;
  GtkWidget *new_group_singleton = NULL;

  g_return_if_fail (GEDA_IS_RADIO_MENU_ITEM (radio_menu_item));

  if (group) {
    g_return_if_fail (!g_slist_find (group, radio_menu_item));
  }

  /* if item is currently in a group */
  if (radio_menu_item->group) {

    GSList *old_group;
    GSList *iter;

    /* remove and update the old group */
    old_group = g_slist_remove (radio_menu_item->group, radio_menu_item);

    if (old_group && !old_group->next) {
      old_group_singleton = g_object_ref (old_group->data);
    }

    for (iter = old_group; iter; iter = iter->next) {

      GedaRadioMenuItem *tmp_item;

      tmp_item        = iter->data;

      tmp_item->group = old_group;
    }
  }

  if (group && !group->next) {
    new_group_singleton = g_object_ref (group->data);
  }

  radio_menu_item->group = g_slist_prepend (group, radio_menu_item);

  if (group) {

    GSList *slist;

    for (slist = group; slist; slist = slist->next) {

      GedaRadioMenuItem *tmp_item;

      tmp_item = slist->data;

      tmp_item->group = radio_menu_item->group;
    }
  }
  else {

    GEDA_CHECK_MENU_ITEM (radio_menu_item)->active = TRUE;
    /* gtk_widget_set_state (GTK_WIDGET (radio_menu_item), GTK_STATE_ACTIVE);
     */
  }

  g_object_ref (radio_menu_item);

  GEDA_OBJECT_NOTIFY (radio_menu_item, "group");

  g_signal_emit (radio_menu_item, group_changed_signal, 0);

  if (old_group_singleton) {
    g_signal_emit (old_group_singleton, group_changed_signal, 0);
    g_object_unref (old_group_singleton);
  }

  if (new_group_singleton) {
    g_signal_emit (new_group_singleton, group_changed_signal, 0);
    g_object_unref (new_group_singleton);
  }

  g_object_unref (radio_menu_item);
}

/** @} geda-radio-menu-item */
