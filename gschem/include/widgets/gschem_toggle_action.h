/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

#ifndef __GSCHEM_TOGGLE_ACTION_H__
#define __GSCHEM_TOGGLE_ACTION_H__

#include <gtk/gtkaction.h>
#include "gschem_action.h"       /* only because is current dir, otherwise inclusion belongs in src */

G_BEGIN_DECLS

#define GSCHEM_TYPE_TOGGLE_ACTION            (gschem_toggle_action_get_type ())
#define GSCHEM_TOGGLE_ACTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_TOGGLE_ACTION, GtkToggleAction))
#define GSCHEM_TOGGLE_ACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GSCHEM_TYPE_TOGGLE_ACTION, GtkToggleActionClass))
#define GSCHEM_IS_TOGGLE_ACTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_TOGGLE_ACTION))
#define GSCHEM_IS_TOGGLE_ACTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GSCHEM_TYPE_TOGGLE_ACTION))
#define GSCHEM_TOGGLE_ACTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), GSCHEM_TYPE_TOGGLE_ACTION, GtkToggleActionClass))

typedef struct _GschemToggleAction        GschemToggleAction;
typedef struct _GschemToggleActionPrivate GschemToggleActionPrivate;
typedef struct _GschemToggleActionClass   GschemToggleActionClass;

struct _GschemToggleAction
{
  GtkToggleAction parent_instance;
  char *multikey_accel;
};

struct _GschemToggleActionClass
{
  GtkToggleActionClass parent_class;
};

GType               gschem_toggle_action_get_type    (void) G_GNUC_CONST;

GschemToggleAction *gschem_toggle_action_new         (const char *name,
                                                      const char *label,
                                                      const char *tooltip,
                                                      const char *stock_id,
                                                      const char *multikey_accel);

G_END_DECLS

#endif  /* __GSCHEM_TOGGLE_ACTION_H__ */
