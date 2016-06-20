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

/** \defgroup geda-radio-menu-item GedaRadioMenuItem Object
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
#include "../../include/geda_menu_enum.h"
#include "../../include/geda_radio_menu_item.h"
#include "../../include/gettext.h"

enum {
  PROP_0,
  PROP_GROUP
};


static void geda_radio_menu_item_destroy        (GtkObject      *object);
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

G_DEFINE_TYPE (GedaRadioMenuItem, geda_radio_menu_item, GEDA_TYPE_CHECK_MENU_ITEM)

GtkWidget*
geda_radio_menu_item_new (GSList *group)
{
  GedaRadioMenuItem *radio_menu_item;

  radio_menu_item = g_object_new (GEDA_TYPE_RADIO_MENU_ITEM, NULL);

  geda_radio_menu_item_set_group (radio_menu_item, group);

  return GTK_WIDGET (radio_menu_item);
}

static void
geda_radio_menu_item_set_property (GObject      *object,
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

static void
geda_radio_menu_item_get_property (GObject      *object,
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

void
geda_radio_menu_item_set_group (GedaRadioMenuItem *radio_menu_item,
                                GSList            *group)
{
  GtkWidget *old_group_singleton = NULL;
  GtkWidget *new_group_singleton = NULL;

  g_return_if_fail (GEDA_IS_RADIO_MENU_ITEM (radio_menu_item));
  g_return_if_fail (!g_slist_find (group, radio_menu_item));

  if (radio_menu_item->group) {

    GSList *slist;

    radio_menu_item->group = g_slist_remove (radio_menu_item->group, radio_menu_item);

    if (radio_menu_item->group && !radio_menu_item->group->next)
      old_group_singleton = g_object_ref (radio_menu_item->group->data);

    for (slist = radio_menu_item->group; slist; slist = slist->next) {

      GedaRadioMenuItem *tmp_item;

      tmp_item = slist->data;

      tmp_item->group = radio_menu_item->group;
    }
  }

  if (group && !group->next)
    new_group_singleton = g_object_ref (group->data);

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

  g_object_notify (G_OBJECT (radio_menu_item), "group");
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


/*!
 * \brief geda_radio_menu_item_new_with_label
 * \par Function Description
 * Creates a new #GedaRadioMenuItem whose child is a simple #GtkLabel.
 *
 * \param[in] group group the radio menu item is to be a member of
 * \param[in] label the text for the label
 *
 * \returns A new #GedaRadioMenuItem
 */
GtkWidget*
geda_radio_menu_item_new_with_label (GSList *group, const char *label)
{
  GtkWidget *radio_menu_item;
  GtkWidget *accel_label;

  radio_menu_item = geda_radio_menu_item_new (group);
  accel_label     = geda_accel_label_new (label);

  gtk_misc_set_alignment (GTK_MISC (accel_label), 0.0, 0.5);
  gtk_container_add (GTK_CONTAINER (radio_menu_item), accel_label);
  geda_accel_label_set_accel_widget (GEDA_ACCEL_LABEL (accel_label), radio_menu_item);
  gtk_widget_show (accel_label);

  return radio_menu_item;
}


/*!
 * \brief geda_radio_menu_item_new_with_mnemonic
 * \par Function Description
 * Creates a new #GedaRadioMenuItem containing a label. The label
 * will be created using geda_label_new_with_mnemonic(), so underscores
 * in @label indicate the mnemonic for the menu item.
 *
 * \param[in] group group the radio menu item is to be a member of
 * \param[in] label the text of the button, with an underscore in front
 *                  of the mnemonic character
 *
 * \returns a new #GedaRadioMenuItem
 */
GtkWidget*
geda_radio_menu_item_new_with_mnemonic (GSList *group, const char *label)
{
  GtkWidget *radio_menu_item;
  GtkWidget *accel_label;

  radio_menu_item = geda_radio_menu_item_new (group);
  accel_label     = g_object_new (GEDA_TYPE_ACCEL_LABEL, NULL);

  geda_label_set_text_with_mnemonic (GEDA_LABEL (accel_label), label);
  gtk_misc_set_alignment (GTK_MISC (accel_label), 0.0, 0.5);

  gtk_container_add (GTK_CONTAINER (radio_menu_item), accel_label);
  geda_accel_label_set_accel_widget (GEDA_ACCEL_LABEL (accel_label), radio_menu_item);
  gtk_widget_show (accel_label);

  return radio_menu_item;
}

/*!
 * \brief geda_radio_menu_item_new_from_widget
 * \par Function Description
 *  Creates a new #GedaRadioMenuItem adding it to the same group as @group.
 *
 * \param[in] group group the radio menu item is to be a member of
 *
 * \returns The new #GedaRadioMenuItem
 */
GtkWidget *
geda_radio_menu_item_new_from_widget (GedaRadioMenuItem *group)
{
  GSList *list = NULL;

  if (group) {
    list = geda_radio_menu_item_get_group (group);
  }

  return geda_radio_menu_item_new (list);
}

/*!
 * \brief geda_radio_menu_item_new_with_mnemonic_from_widget
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
GtkWidget *
geda_radio_menu_item_new_with_mnemonic_from_widget (GedaRadioMenuItem *group,
                                                    const char        *label)
{
  GSList *list = NULL;

  g_return_val_if_fail (GEDA_IS_RADIO_MENU_ITEM (group), NULL);

  if (group) {
    list = geda_radio_menu_item_get_group (group);
  }

  return geda_radio_menu_item_new_with_mnemonic (list, label);
}

/*!
 * \brief geda_radio_menu_item_new_with_label_from_widget
 * \par Function Description
 * Creates a new GedaRadioMenuItem whose child is a simple GedaLabel.
 * The new #GedaRadioMenuItem is added to the same group as \a group.
 *
 * \param[in] group group the radio menu item is to be a member of
 * \param[in] label: the text for the label
 *
 * \returns The new #GedaRadioMenuItem
 */
GtkWidget *
geda_radio_menu_item_new_with_label_from_widget (GedaRadioMenuItem *group,
                                                 const char        *label)
{
  GSList *list = NULL;

  g_return_val_if_fail (GEDA_IS_RADIO_MENU_ITEM (group), NULL);

  if (group) {
    list = geda_radio_menu_item_get_group (group);
  }

  return geda_radio_menu_item_new_with_label (list, label);
}

/*!
 * \brief geda_radio_menu_item_get_group
 * \par Function Description
 * Returns the group to which the radio menu item belongs, as a #GList
 * of #GedaRadioMenuItem. The list belongs to the widget and should not
 * be freed.
 *
 * \param[in] radio_menu_item  The GedaRadioMenuItem
 *
 * \returns the group of @radio_menu_item
 */
GSList*
geda_radio_menu_item_get_group (GedaRadioMenuItem *radio_menu_item)
{
  g_return_val_if_fail (GEDA_IS_RADIO_MENU_ITEM (radio_menu_item), NULL);

  return radio_menu_item->group;
}

static void
geda_radio_menu_item_class_init (GedaRadioMenuItemClass *klass)
{
  GObjectClass *gobject_class;
  GtkObjectClass *object_class;
  GedaMenuItemClass *menu_item_class;

  gobject_class   = G_OBJECT_CLASS (klass);
  object_class    = GTK_OBJECT_CLASS (klass);
  menu_item_class = GEDA_MENU_ITEM_CLASS (klass);

  gobject_class->set_property = geda_radio_menu_item_set_property;
  gobject_class->get_property = geda_radio_menu_item_get_property;

  /*!
   * \property GedaRadioMenuItem::group
   * The radio menu item whose group this widget belongs to.
   */
  g_object_class_install_property (gobject_class,
                                   PROP_GROUP,
                                   g_param_spec_object ("group",
                                                      _("Group"),
                                                      _("The radio menu item whose group this widget belongs to."),
                                   GEDA_TYPE_RADIO_MENU_ITEM,
                                   G_PARAM_WRITABLE));

  object_class->destroy = geda_radio_menu_item_destroy;

  menu_item_class->activate = geda_radio_menu_item_activate;

  /*!
   * GedaRadioMenuItem::group-changed:
   * Emitted when the group of radio menu items that a radio menu item belongs
   * to changes. This is emitted when a radio menu item switches from
   * being alone to being part of a group of 2 or more menu items, or
   * vice-versa, and when a button is moved from one group of 2 or
   * more menu items ton a different one, but not when the composition
   * of the group that a menu item belongs to changes.
   */
  group_changed_signal = g_signal_new ("group-changed",
                                       G_OBJECT_CLASS_TYPE (object_class),
                                       G_SIGNAL_RUN_FIRST,
                                       G_STRUCT_OFFSET (GedaRadioMenuItemClass, group_changed),
                                       NULL, NULL,
                                       geda_marshal_VOID__VOID,
                                       G_TYPE_NONE, 0);
}

static void
geda_radio_menu_item_init (GedaRadioMenuItem *radio_menu_item)
{
  radio_menu_item->group = g_slist_prepend (NULL, radio_menu_item);
  geda_check_menu_item_set_draw_as_radio (GEDA_CHECK_MENU_ITEM (radio_menu_item), TRUE);
}

static void
geda_radio_menu_item_destroy (GtkObject *object)
{
  GedaRadioMenuItem *radio_menu_item     = GEDA_RADIO_MENU_ITEM (object);
  GtkWidget         *old_group_singleton = NULL;
  GedaRadioMenuItem *tmp_menu_item;
  GSList            *tmp_list;
  bool               was_in_group;

  was_in_group = radio_menu_item->group && radio_menu_item->group->next;

  radio_menu_item->group = g_slist_remove (radio_menu_item->group,
                                           radio_menu_item);
  if (radio_menu_item->group && !radio_menu_item->group->next)
    old_group_singleton = radio_menu_item->group->data;

  tmp_list = radio_menu_item->group;

  while (tmp_list) {

      tmp_menu_item = tmp_list->data;
      tmp_list = tmp_list->next;

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

  GTK_OBJECT_CLASS (geda_radio_menu_item_parent_class)->destroy (object);
}

static void
geda_radio_menu_item_activate (GedaMenuItem *menu_item)
{
  GedaRadioMenuItem *radio_menu_item = GEDA_RADIO_MENU_ITEM (menu_item);
  GedaCheckMenuItem *check_menu_item = GEDA_CHECK_MENU_ITEM (menu_item);
  GedaCheckMenuItem *tmp_menu_item;
  GtkAction         *action;
  GSList            *tmp_list;
  bool               toggled;

  action = gtk_activatable_get_related_action (GTK_ACTIVATABLE (menu_item));

  if (action && geda_menu_item_get_submenu (menu_item) == NULL) {
    gtk_action_activate (action);
  }

  toggled = FALSE;

  if (check_menu_item->active) {

    tmp_menu_item = NULL;
    tmp_list = radio_menu_item->group;

    while (tmp_list) {

      tmp_menu_item = tmp_list->data;
      tmp_list = tmp_list->next;

      if (tmp_menu_item->active && (tmp_menu_item != check_menu_item))
        break;

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

/** @} geda-radio-menu-item */
