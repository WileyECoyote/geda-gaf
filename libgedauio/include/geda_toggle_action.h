/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_toggle_action.h
 *
 * gEDA - GPL Electronic Design Automation
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
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

#ifndef __GEDA_TOGGLE_ACTION_H__
#define __GEDA_TOGGLE_ACTION_H__

#include <gtk/gtkaction.h>
#include "geda_action.h"       /* only because is current dir, otherwise inclusion belongs in src */

BEGIN_DECLS

#define GEDA_TYPE_TOGGLE_ACTION            (geda_toggle_action_get_type ())
#define GEDA_TOGGLE_ACTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_TOGGLE_ACTION, GedaToggleAction))
#define GEDA_TOGGLE_ACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_TOGGLE_ACTION, GedaToggleActionClass))
#define GEDA_IS_TOGGLE_ACTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEDA_TYPE_TOGGLE_ACTION))
#define GEDA_IS_TOGGLE_ACTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_TOGGLE_ACTION))
#define GEDA_TOGGLE_ACTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj),   GEDA_TYPE_TOGGLE_ACTION, GedaToggleActionClass))

typedef struct _GedaToggleAction        GedaToggleAction;
typedef struct _GedaToggleActionPrivate GedaToggleActionPrivate;
typedef struct _GedaToggleActionClass   GedaToggleActionClass;

struct _GedaToggleAction
{
  GtkToggleAction parent_instance;
  char *multikey_accel;
};

struct _GedaToggleActionClass
{
  GtkToggleActionClass parent_class;
};

GedaType          geda_toggle_action_get_type    (void) GEDA_CONST;

GedaToggleAction *geda_toggle_action_new         (const char *name,
                                                  const char *label,
                                                  const char *tooltip,
                                                  const char *stock_id,
                                                  const char *multikey_accel);

END_DECLS

#endif  /* __GEDA_TOGGLE_ACTION_H__ */
