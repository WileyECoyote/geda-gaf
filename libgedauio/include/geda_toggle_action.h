/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
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
 */

#ifndef __GEDA_TOGGLE_ACTION_H__
#define __GEDA_TOGGLE_ACTION_H__

#include <gtk/gtkaction.h>
#include "geda_action.h"       /* only because is current dir, otherwise inclusion belongs in src */

G_BEGIN_DECLS

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

unsigned int      geda_toggle_action_get_type    (void) G_GNUC_CONST;

GedaToggleAction *geda_toggle_action_new         (const char *name,
                                                  const char *label,
                                                  const char *tooltip,
                                                  const char *stock_id,
                                                  const char *multikey_accel);

G_END_DECLS

#endif  /* __GEDA_TOGGLE_ACTION_H__ */
