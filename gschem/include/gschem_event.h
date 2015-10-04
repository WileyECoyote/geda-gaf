/* -*- gschem_event.h -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: June, 23, 2015
 *
 */
/*!
 * \file gschem_event.h
 *
 * \brief
 */

#define GSCHEM_TYPE_EVENT           (gschem_event_get_type())
#define GSCHEM_EVENT(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_EVENT, GschemEvent))
#define GSCHEM_EVENT_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_EVENT, GschemEventClass))
#define GSCHEM_IS_EVENT(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_EVENT))
#define GSCHEM_EVENT_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_EVENT, GschemEventClass))

typedef struct _GschemEventClass GschemEventClass;
typedef struct _GschemEvent GschemEvent;

struct _GschemEventClass
{
  GObjectClass parent;

  void (*finalize) (GObject *object);
};

struct _GschemEvent
{
  GObject parent;

  int state;

  int wx;
  int wy;

  unsigned int press_hid;
  unsigned int release_hid;

  void (*initializer)    (void *, int, int);

  union { void (*adder) (void *, int, int);  void (*paster) (void *); void * func;} resolver;

  int  (*press_butt)     (GtkWidget *widget, GdkEventButton *, void *);
  int  (*release_butt)   (GtkWidget *widget, GdkEventButton *, void *);
};

GedaType      gschem_event_get_type(void);

GschemEvent  *gschem_event_new (void);
