/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_event.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include <gschem.h>
#include "x_window.h"

#include <geda_debug.h>

typedef bool (*GschemDrawEvent) (GtkWidget*, void*, GschemToplevel*);

struct event_reg_t {
  char            *detailed_signal;
  unsigned long    hid;
  int              block;
  GschemDrawEvent  handler;
};

static struct event_reg_t drawing_area_events[] = {
  { "expose_event",         0, 0, GSE_HANDLER( x_event_expose          )},
  { "button_press_event",   0, 0, GSE_HANDLER( x_event_button_pressed  )},
  { "button_release_event", 0, 0, GSE_HANDLER( x_event_button_released )},
  { "motion_notify_event",  0, 0, GSE_HANDLER( x_event_motion          )},
  { "configure_event",      0, 0, GSE_HANDLER( x_event_configure       )},
  { "key_press_event",      0, 0, GSE_HANDLER( x_event_key             )},
  { "key_release_event",    0, 0, GSE_HANDLER( x_event_key             )},
  { "scroll_event",         0, 0, GSE_HANDLER( x_event_scroll          )},
  {  NULL,                  0, 0, NULL }
};

void i_event_block_buttons (GschemToplevel *w_current)
{
  i_event_block_handler (w_current, BUTTON_PRESS_HANDLER);
  i_event_block_handler (w_current, BUTTON_RELEASE_HANDLER);
}

void i_event_unblock_buttons (GschemToplevel *w_current)
{
  i_event_unblock_handler (w_current, BUTTON_PRESS_HANDLER);
  i_event_unblock_handler (w_current, BUTTON_RELEASE_HANDLER);
}

void i_event_block_handler (GschemToplevel *w_current, EventHandler id)
{
  if (!drawing_area_events[id].block) {
    g_signal_handler_block (DrawingArea, drawing_area_events[id].hid);
  }
  drawing_area_events[id].block++;
}

void i_event_unblock_handler (GschemToplevel *w_current, EventHandler id)
{
  if (drawing_area_events[id].block) {
    drawing_area_events[id].block--;
    if (!drawing_area_events[id].block) {
      g_signal_handler_unblock (DrawingArea, drawing_area_events[id].hid);
    }
  }
}

/*! \brief Setup X-Events Event Handlers
 *  \par Function Description
 *   This function configures events for the Drawing Area and connects
 *   callback functions in x_event.c for both the Drawing Area and the
 *   Main window. Lastly, x_dnd_setup_event_handlers is called to setup
 *   Drag-and-Drop events for the drawing area.
 */
void i_event_setup_handlers (GschemToplevel *w_current)
{

  struct event_reg_t *tmp;

  /* is the configure event type missing here? hack */
  gtk_widget_set_events (DrawingArea,
                         GDK_BUTTON_PRESS_MASK        |
                         GDK_BUTTON_RELEASE_MASK      |
                         GDK_EXPOSURE_MASK            |
                         GDK_KEY_PRESS_MASK           |
                         GDK_POINTER_MOTION_MASK      |
                         GDK_POINTER_MOTION_HINT_MASK |
                         GDK_SCROLL_MASK              |
                         GDK_VISIBILITY_NOTIFY_MASK);

  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++) {
    tmp->hid = g_signal_connect (DrawingArea, tmp->detailed_signal,
                                 G_CALLBACK(tmp->handler), w_current);
  }

  x_dnd_setup_event_handlers(w_current);
  x_event_governor(w_current);
}
