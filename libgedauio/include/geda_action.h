/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_action.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2013-2018 Wiley Edward Hill
 * Copyright (C) 2013-2018 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 */

#ifndef __GEDA_ACTION_H__
#define __GEDA_ACTION_H__

#define GEDA_TYPE_ACTION           (geda_action_get_type())
#define GEDA_ACTION(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_ACTION, GedaAction))
#define GEDA_ACTION_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_ACTION, GedaActionClass))
#define GEDA_IS_ACTION(obj)        (is_a_geda_action((GedaAction*)(obj)))
#define GEDA_ACTION_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_ACTION, GedaActionClass))

typedef struct _GedaAction      GedaAction;
typedef struct _GedaActionClass GedaActionClass;

struct _GedaAction
{
  GtkAction parent_instance;

  const char *action_name;
        char *icon_name;
        char *multikey_accel;
};

struct _GedaActionClass
{
  GtkActionClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType    geda_action_get_type                 (void) GEDA_CONST;
bool        is_a_geda_action                     (GedaAction   *action);

GedaAction *geda_action_new                      (const char   *name,
                                                  const char   *label,
                                                  const char   *tooltip,
                                                  const char   *icon_id,
                                                  const char   *multikey_accel);
void        geda_action_activate                 (GedaAction   *action);
const char *geda_action_get_action_name          (GedaAction   *action);
const char *geda_action_get_icon_name            (GedaAction   *action);
void        geda_action_set_icon_name            (GedaAction   *action,
                                                  const char   *icon_name);

GtkWidget  *geda_action_create_menu_item         (GedaAction   *action);
GtkWidget  *geda_action_widget_create_menu_item  (GtkWidget    *widget);

void        geda_action_disconnect_accelerator   (GedaAction   *action);

void        geda_action_sync_menu_visible        (GedaAction   *action,
                                                  GtkWidget    *proxy,
                                                  bool          empty);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_ACTION_H__ */
