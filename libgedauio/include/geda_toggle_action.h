/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_toggle_action.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 1998-2018 gEDA Contributors (see ChangeLog for details)
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
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 */

#ifndef __GEDA_TOGGLE_ACTION_H__
#define __GEDA_TOGGLE_ACTION_H__

#if (GTK_MAJOR_VERSION < 3) && !defined GTK_DISABLE_SINGLE_INCLUDES

#include <gtk/gtkaction.h>

#else

#include <gtk/gtk.h>

#endif

#include "geda_action.h"       /* only because is current dir, otherwise inclusion belongs in src */

#define GEDA_TYPE_TOGGLE_ACTION            (geda_toggle_action_get_type ())
#define GEDA_TOGGLE_ACTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_TOGGLE_ACTION, GedaToggleAction))
#define GEDA_TOGGLE_ACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_TOGGLE_ACTION, GedaToggleActionClass))
#define GEDA_IS_TOGGLE_ACTION(obj)         (is_a_geda_toggle_action((GedaToggleAction*)(obj)))
#define GEDA_IS_TOGGLE_ACTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_TOGGLE_ACTION))
#define GEDA_TOGGLE_ACTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),   GEDA_TYPE_TOGGLE_ACTION, GedaToggleActionClass))

typedef struct _GedaToggleAction        GedaToggleAction;
typedef struct _GedaToggleActionPrivate GedaToggleActionPrivate;
typedef struct _GedaToggleActionClass   GedaToggleActionClass;

struct _GedaToggleAction
{
  GedaAction parent_instance;

  unsigned int active : 1;
  unsigned int draw_as_radio : 1;
};

struct _GedaToggleActionClass
{
  GedaActionClass parent_class;

  void (* toggled) (GedaToggleAction *action);
};

#ifdef __cplusplus
extern "C" {
#endif

GedaType          geda_toggle_action_get_type    (void) GEDA_CONST;
bool              is_a_geda_toggle_action        (GedaToggleAction *toggle_action);

GedaToggleAction *geda_toggle_action_new         (const char *name,
                                                  const char *label,
                                                  const char *tooltip,
                                                  const char *stock_id,
                                                  const char *multikey_accel);

void geda_toggle_action_toggled           (GedaToggleAction *action);
bool geda_toggle_action_get_active        (GedaToggleAction *action);
void geda_toggle_action_set_active        (GedaToggleAction *action,
                                           bool              is_active);


bool geda_toggle_action_get_draw_as_radio (GedaToggleAction *action);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif  /* __GEDA_TOGGLE_ACTION_H__ */
