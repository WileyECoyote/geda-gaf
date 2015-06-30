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

#include "gschem.h"
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
                         GDK_SCROLL_MASK              |
                         GDK_VISIBILITY_NOTIFY_MASK);

  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++) {
    tmp->hid = g_signal_connect (DrawingArea, tmp->detailed_signal,
                                 G_CALLBACK(tmp->handler), w_current);
  }

  x_dnd_setup_event_handlers(w_current);
  x_event_governor(w_current);
}


/* ----------------------- Setup Adder Event Handlers ---------------------- */

static void i_event_adder_disconnect_events (GschemToplevel *w_current)
{
  GschemEvent *event = w_current->action_event;

  if (event->press_hid) {
    g_signal_handler_disconnect (DrawingArea, event->press_hid);
    event->press_hid = 0;
  }
  if (event->release_hid) {
    g_signal_handler_disconnect (DrawingArea, event->release_hid);
    event->release_hid = 0;
  }
}

static void i_event_adder_enable_events(GschemToplevel *w_current)
{
  GschemEvent *event = w_current->action_event;

  i_event_block_buttons (w_current);

  i_event_adder_disconnect_events(w_current);

  event->press_hid   = g_signal_connect (DrawingArea, "button_press_event",
                                         G_CALLBACK(event->press_butt),
                                         w_current);
  event->release_hid = g_signal_connect (DrawingArea, "button_release_event",
                                         G_CALLBACK(event->release_butt),
                                         w_current);
}

static void i_event_end_action_handler(GschemToplevel *w_current)
{
  i_event_adder_disconnect_events(w_current);
  i_event_unblock_buttons (w_current);

  w_current->action_event->state = 0;

  i_status_action_stop(w_current);
}

/* ---------------------- Button Event Adder Handlers ---------------------- */
static
int i_event_adder_pressed(GtkWidget *widget, GdkEventButton *event, GschemToplevel *w_current)
{
  GschemEvent *action = w_current->action_event;

  if (w_current->event_state == action->state) {

    w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
    w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
    w_current->ALTKEY     = (event->state & GDK_MOD1_MASK)    ? 1 : 0;

    if (event->button == 1) {

      int  x, y;
      int  w_x, w_y;

      SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

      w_x = snap_grid (w_current, x);
      w_y = snap_grid (w_current, y);

      if (w_current->inside_action) {

        action->resolver.adder(w_current, w_x, w_y); /* aka o_shape_end */

        if (event->type == GDK_2BUTTON_PRESS) {

          if (w_current->event_state == PATHMODE) {
            action->resolver.adder(w_current, w_x, w_y);
          }
          else {
            i_event_end_action_handler(w_current);
            i_status_set_state(w_current, SELECT);
          }
        }
      }
      else {

        /* Not inside action so let instigator initialize variables */
        action->initializer(w_current, w_x, w_y);

        i_status_action_start(w_current);
        w_current->rubber_visible = TRUE;
      }
    }
    else if (event->button == 2) {

      if (w_current->event_state == PICTUREMODE) {

        if (w_current->current_pixbuf != NULL) {
          GEDA_UNREF(w_current->current_pixbuf);
          w_current->current_pixbuf = NULL;
        }

        GEDA_FREE(w_current->pixbuf_filename);
      }
    }
    else if (event->button == 3) {

      if (w_current->rubber_visible) {
        w_current->rubber_visible = FALSE;
        o_invalidate_rubber (w_current);
      }

      switch (w_current->event_state) {

        case(NETMODE):
          o_net_reset (w_current);
          break;

        case(PINMODE):
        case(LINEMODE):
        case(BOXMODE):
        case(CIRCLEMODE):
        case(ARCMODE):
        case(BUSMODE):
        case(PATHMODE):
        case(PICTUREMODE):
          i_event_end_action_handler(w_current);
          i_status_set_state(w_current, SELECT);
          break;

        default:
          /* Not an Adder event; do nothing */
          break;
      }
    }
  }
  else {
    BUG_MSG("w_current->event_state != action->state");
  }
  return(0);
}

static int i_event_adder_released(GtkWidget      *widget,
                                  GdkEventButton *event,
                                  GschemToplevel *w_current)
{
  GschemEvent *action = w_current->action_event;

  if (w_current->event_state == action->state) {

    if (event->button == 1) {

      if (w_current->inside_action) {

        if (w_current->event_state == PATHMODE) {

          int w_x, w_y;
          int unsnapped_wx, unsnapped_wy;

          /* Capture where in the World this event occurred */
          SCREENtoWORLD (w_current, (int) event->x, (int) event->y,
                         &unsnapped_wx, &unsnapped_wy);

          w_x = snap_grid (w_current, unsnapped_wx);
          w_y = snap_grid (w_current, unsnapped_wy);
          o_path_end (w_current, w_x, w_y);
        }
      }
    }
    //else if (event->button == 3) {}
  }
  else {
    BUG_MSG("w_current->event_state != action->state");
  }
  return(0);
}


/* ---------------------- Button Event Paster Handlers ---------------------- */
static
int i_event_paster_pressed(GtkWidget *widget, GdkEventButton *event, GschemToplevel *w_current)
{
  GschemEvent *action = w_current->action_event;

  if (w_current->event_state == action->state) {

    w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
    w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
    w_current->ALTKEY     = (event->state & GDK_MOD1_MASK)    ? 1 : 0;

    if (event->button == 1) {

      int  x, y;
      int  w_x, w_y;

      SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

      w_x = snap_grid (w_current, x);
      w_y = snap_grid (w_current, y);

      if (w_current->inside_action) {

        if (Current_PlaceList != NULL) {
          w_current->second_wx = w_x;
          w_current->second_wy = w_y;

          action->resolver.paster(w_current); /* o_???_end */

        }
      }
      else {

        i_status_action_start(w_current);
        w_current->rubber_visible = TRUE;
      }
    }
    else if (event->button == 3) {

      if (w_current->rubber_visible) {
        w_current->rubber_visible = FALSE;
        o_invalidate_rubber (w_current);
      }

      i_callback_cancel(w_current, 0, NULL);

    }
  }
  else {
    BUG_MSG("w_current->event_state != action->state");
  }
  return(0);
}

static int i_event_paster_released(GtkWidget      *widget,
                                  GdkEventButton *event,
                                  GschemToplevel *w_current)
{
  GschemEvent *action = w_current->action_event;

  if (w_current->event_state == action->state) {

    if (event->button == 1) {

      switch (w_current->event_state) {

        case(DRAGMOVE):  /* 8 */
          if (Current_PlaceList != NULL) {
            o_move_end(w_current);
          }

        case(MOVEMODE):  /* 23 */
          if (w_current->drag_event) {
            gdk_event_free(w_current->drag_event);
            w_current->drag_event = NULL;
          }
          break;

        case STARTDND:   /* 30 */
          w_current->dnd_state = NONE;
          if (w_current->drag_event) {
            gdk_event_free(w_current->drag_event);
            w_current->drag_event = NULL;
          }
          break;
      }
    }
    else  if (event->button == 2) {

      if (!w_current->inside_action) {
        BUG_MSG("Oops, Not inside an action!");
      }
      else {

        int rotate;

        rotate = (w_current->event_state == DRAGMOVE  ||
                  w_current->event_state == COMPMODE  ||
                  w_current->event_state == MOVEMODE  ||
                  w_current->event_state == COPYMODE  ||
                  w_current->event_state == MCOPYMODE ||
                  w_current->event_state == PASTEMODE ||
                  w_current->event_state == TEXTMODE);

        if (rotate) {

          if (w_current->event_state == MOVEMODE ||
              w_current->event_state == DRAGMOVE)
          {
            o_move_invalidate_rubber (w_current, FALSE);
          }
          else {
            o_place_invalidate_rubber (w_current, FALSE);
          }
          w_current->rubber_visible = FALSE;

          o_place_rotate(w_current);

          if (w_current->event_state == COMPMODE) {
            o_complex_place_changed_run_hook (w_current);
          }

          if (w_current->event_state == MOVEMODE ||
              w_current->event_state == DRAGMOVE)
          {
            o_move_invalidate_rubber (w_current, TRUE);
          }
          else {
            o_place_invalidate_rubber (w_current, TRUE);
          }
          w_current->rubber_visible = TRUE;
        }
      }
    }
    //else if (event->button == 3) {}
  }
  else {
    BUG_IMSG("action->state != w_current->event_state",w_current->event_state);
  }
  return(0);
}
/* ----------------------------------------------------------------- */

void i_event_cancel_action_handler(GschemToplevel *w_current)
{
  GschemEvent *event = w_current->action_event;

  if (event->state) {
    i_event_end_action_handler(w_current);
    i_status_set_state(w_current, SELECT);
    o_invalidate_all(w_current);
  }
}

void i_event_start_adder_handler (GschemToplevel *w_current,
                                  ActionInit      ifunc,
                                  ActionAdder     rfunc)
{
  GschemEvent *event;

  i_status_action_start(w_current);

  if (w_current->action_event->state) {
    i_event_end_action_handler(w_current);
  }

  event                = w_current->action_event;
  event->state         = w_current->event_state;

  event->initializer   = (void*)ifunc;
  event->resolver.func = (void*)rfunc;

  event->press_butt    = (void*)i_event_adder_pressed;
  event->release_butt  = (void*)i_event_adder_released;

  i_event_adder_enable_events(w_current);
}

void i_event_start_paster_handler (GschemToplevel *w_current,
                                   ActionPaster    rfunc)
{
  GschemEvent *event;

  if (w_current->action_event->state) {
    i_event_end_action_handler(w_current);
  }

  event                = w_current->action_event;
  event->state         = w_current->event_state;

  event->resolver.func = (void*)rfunc;

  event->press_butt    = (void*)i_event_paster_pressed;
  event->release_butt  = (void*)i_event_paster_released;

  i_event_adder_enable_events(w_current);

  i_status_action_start(w_current);
}

void i_event_stop_action_handler(GschemToplevel *w_current)
{
  GschemEvent *event = w_current->action_event;

  if (event->state) {
    i_event_end_action_handler(w_current);
    i_status_action_stop(w_current);
    i_status_set_state(w_current, SELECT);
  }
}
