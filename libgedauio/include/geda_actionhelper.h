/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_actionhelper.h
 *
 * Copyright © 2012 Canonical Limited
 *
 * This Library is free software; you can redistribute it and or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 3 of the License, or (at your
 * option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Authors: Ryan Lortie <desrt@desrt.ca>
 */

#ifndef __GTK_ACTION_HELPER_H__
#define __GTK_ACTION_HELPER_H__

#include <gtk/gtkapplication.h>
#include <gtk/gtkactionable.h>

#define GTK_TYPE_ACTION_HELPER     (gtk_action_helper_get_type ())
#define GTK_ACTION_HELPER(inst)    (G_TYPE_CHECK_INSTANCE_CAST ((inst), GTK_TYPE_ACTION_HELPER, GtkActionHelper))
#define GTK_IS_ACTION_HELPER(inst) (G_TYPE_CHECK_INSTANCE_TYPE ((inst), GTK_TYPE_ACTION_HELPER))

typedef struct _GtkActionHelper                             GtkActionHelper;

typedef enum
{
  GTK_ACTION_HELPER_ROLE_NORMAL,
  GTK_ACTION_HELPER_ROLE_TOGGLE,
  GTK_ACTION_HELPER_ROLE_RADIO
} GtkActionHelperRole;

G_GNUC_INTERNAL
GedaType                gtk_action_helper_get_type                      (void);

G_GNUC_INTERNAL
GtkActionHelper *       gtk_action_helper_new                           (GtkActionable   *widget);

G_GNUC_INTERNAL
GtkActionHelper *       gtk_action_helper_new_with_application          (GtkApplication  *application);

G_GNUC_INTERNAL
void                    gtk_action_helper_set_action_name               (GtkActionHelper *helper,
                                                                         const gchar     *action_name);
G_GNUC_INTERNAL
void                    gtk_action_helper_set_action_target_value       (GtkActionHelper *helper,
                                                                         GVariant        *action_target);
G_GNUC_INTERNAL
const gchar *           gtk_action_helper_get_action_name               (GtkActionHelper *helper);
G_GNUC_INTERNAL
GVariant *              gtk_action_helper_get_action_target_value       (GtkActionHelper *helper);

G_GNUC_INTERNAL
GtkActionHelperRole     gtk_action_helper_get_role                      (GtkActionHelper *helper);
G_GNUC_INTERNAL
gboolean                gtk_action_helper_get_enabled                   (GtkActionHelper *helper);
G_GNUC_INTERNAL
gboolean                gtk_action_helper_get_active                    (GtkActionHelper *helper);

G_GNUC_INTERNAL
void                    gtk_action_helper_activate                      (GtkActionHelper *helper);

#endif /* __GTK_ACTION_HELPER_H__ */
