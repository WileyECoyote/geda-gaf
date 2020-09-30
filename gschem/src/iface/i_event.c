/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_event.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
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
#include <x_window.h>

#include <geda_debug.h>

/** \defgroup I-Events Interface Event Module
 *  @{
 *  \brief Primary Canvas Event Module
 *  \par
 *   This module contains routines to setup the primary event handlers
 *   for the user interface. Two of the handlers routines are located
 *   in this module; i_event_enter and i_event_leave, the remainder are
 *   in x_window.c. This module also contains routines to support local
 *   event handlers by temporarily block the primary button press and
 *   release event handlers. This reduces the complexity of the primary
 *   event handlers, and more significantly, improves performances by
 *   localizing events to specific operations. Routines utilizing local
 *   handlers need only supply a initializer, called when a press event
 *   occurs, and resolver function that is called when a release event
 *   occurs. Resolver functions typically terminate the local event
 *   handling.
 */

typedef bool (*GschemDrawEvent) (GtkWidget*, void*, GschemToplevel*);

struct event_reg_t {
  char            *detailed_signal;
  unsigned long    hid;
  int              block;
  GschemDrawEvent  handler;
};

int i_event_enter(GtkWidget *widget, GdkEventCrossing *event,
                                     GschemToplevel   *w_current);
int i_event_leave(GtkWidget *widget, GdkEventCrossing *event,
                                     GschemToplevel   *w_current);

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

static struct event_reg_t page_view_events[] = {
  { "enter_notify_event",   0, 0, GSE_HANDLER( i_event_enter )},
  { "leave_notify_event",   0, 0, GSE_HANDLER( i_event_leave )},
  {  NULL,                  0, 0, NULL }
};

/*!
 * \brief Convenience function to Block the Button Event Handlers
 * \par Function Description
 *   This function blocks both the BUTTON_PRESS_HANDLER and the
 *   BUTTON_RELEASE_HANDLER event handlers.
 *
 * \param [in] w_current  The GschemToplevel object.
 *
 * \sa i_event_unblock_handler
 */
void
i_event_block_buttons (GschemToplevel *w_current)
{
  i_event_block_handler (w_current, BUTTON_PRESS_HANDLER   /* Enum 1 */);
  i_event_block_handler (w_current, BUTTON_RELEASE_HANDLER /* Enum 2 */);
}

/*!
 * \brief Convenience function to Unblock the Button Event Handlers
 * \par Function Description
 *  This function unblocks both the BUTTON_PRESS_HANDLER and the
 *  BUTTON_RELEASE_HANDLER event handlers.
 *
 * \param [in] w_current  The GschemToplevel object.
 *
 * \sa i_event_unblock_handler
 */
void
i_event_unblock_buttons (GschemToplevel *w_current)
{
  i_event_unblock_handler (w_current, BUTTON_PRESS_HANDLER   /* Enum 1 */);
  i_event_unblock_handler (w_current, BUTTON_RELEASE_HANDLER /* Enum 2 */);
}

/*!
 * \brief Global utility function to Block an Event Handler
 * \par Function Description
 *  This function block event handler identified by the
 *  #EID_EVENT_HANDLERS id.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] id         Enumerated ID of handler to block.
 *
 * \sa i_event_unblock_handler i_event_block_buttons
 */
void
i_event_block_handler (GschemToplevel *w_current, EventHandler id)
{
  if (!drawing_area_events[id].block) {
    g_signal_handler_block (DrawingArea, drawing_area_events[id].hid);
  }
  drawing_area_events[id].block++;
}

/*!
 * \brief Global utility function to Unblock an Event Handler
 * \par Function Description
 *  This function unblocks a previously blocked event handler indentify
 *  by the #EID_EVENT_HANDLERS id.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] id         Enumerated ID of handler to unblock.
 *
 * \sa i_event_block_handler i_event_unblock_buttons
 */
void
i_event_unblock_handler (GschemToplevel *w_current, EventHandler id)
{
  if (drawing_area_events[id].block) {
    drawing_area_events[id].block--;
    if (!drawing_area_events[id].block) {
      g_signal_handler_unblock (DrawingArea, drawing_area_events[id].hid);
    }
  }
}

/*!
 * \brief Enter Drawing Canvas Event Handler
 * \par Function Description
 *  Called when the pointers enters the DrawingArea widget. Currently,
 *  this function serves to deactivate sources added to the main loop
 *  using g_timeout_add by i_pan_auto, essentialy this function causes
 *  auto-pan scrolling to stop when the pointer is brought back on the
 *  canvas.
 *
 * \sa i_event_leave i_pan_auto
 */
int
i_event_enter(GtkWidget *widget, GdkEventCrossing *event,
                                 GschemToplevel   *w_current)
{
  if (w_current->inside_action) {

    switch (w_current->event_state) {
      case NETMODE:
      case BUSMODE:
        w_current->doing_pan = 0;
        break;

      default:
        break;
    }
  }

  return(0);
}

/*!
 * \brief Leave Drawing Canvas Event Handler
 * \par Function Description
 *  Called when the pointers leaves the DrawingArea widget. This
 *  function initiates auto-panning inside a NETMODE or BUSMODE
 *  action by calling i_pan_auto.
 *
 * \sa i_event_enter i_pan_auto
 */
int
i_event_leave(GtkWidget *widget, GdkEventCrossing *event,
                                 GschemToplevel   *w_current)
{
  if (event->mode == GDK_CROSSING_NORMAL) {
    if (w_current->inside_action) {

      switch (w_current->event_state) {
        case NETMODE:
        case BUSMODE:
          i_pan_auto(w_current, event);
          break;

        default:
          break;
      }
    }
  }
  return(0);
}

/*!
 * \brief Setup X-Events Event Handlers
 * \par Function Description
 *  This function configures events for the Drawing Area and connects
 *  callback functions in x_event.c for both the Drawing Area and the
 *  Main window. Lastly, x_dnd_setup_event_handlers is called to setup
 *  Drag-and-Drop events for the drawing area.
 */
void i_event_setup_handlers (GschemToplevel *w_current)
{
  struct event_reg_t *tmp;

  /* is the configure event type missing here? hack */
  gtk_widget_set_events (DrawingArea,
                         GDK_BUTTON_PRESS_MASK        |
                         GDK_BUTTON_RELEASE_MASK      |
                         GDK_ENTER_NOTIFY_MASK        |
                         GDK_EXPOSURE_MASK            |
                         GDK_KEY_PRESS_MASK           |
                         GDK_LEAVE_NOTIFY_MASK        |
                         GDK_POINTER_MOTION_MASK      |
                         GDK_SCROLL_MASK              |
                         GDK_VISIBILITY_NOTIFY_MASK);

  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++) {
    tmp->hid = g_signal_connect (DrawingArea, tmp->detailed_signal,
                                 G_CALLBACK(tmp->handler), w_current);
  }

  for (tmp = page_view_events; tmp->detailed_signal != NULL; tmp++) {
    tmp->hid = g_signal_connect (DrawingArea, tmp->detailed_signal,
                                 G_CALLBACK(tmp->handler), w_current);
  }

  x_dnd_setup_event_handlers(w_current);
  x_event_governor(w_current);
}

/* ----------------------- Setup Adder Event Handlers ---------------------- */

/*!
 * \brief Disable Action Event Handlers
 * \par Function Description
 * \internal Function disables the Action Event Handlers and invalidates
 *  the handler ids.
 *
 * \sa i_event_start_adder_handler i_event_start_paster_handler
 */
static void
i_event_adder_disconnect_events (GschemToplevel *w_current)
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

/*!
 * \brief Enable Action Event Handlers
 * \par Function Description
 * \internal Function blocks the current button event handlers and
 *  enables the signals to be sent to the Action Event Handler.
 *
 * \sa i_event_start_adder_handler i_event_start_paster_handler
 */
static void
i_event_action_enable_events(GschemToplevel *w_current)
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

/*!
 * \brief Stop the Action Event Handler
 * \par Function Description
 * \internal Function disconnects the Action Event Handler and unblock
 *  the previous event handler. This function is the only functions to
 *  set the action_event->state = FALSE. This function is similar to
 *  i_event_cancel_action_handler except that this function does not
 *  set SELECT mode.
 *
 * \sa i_event_stop_action_handler, i_event_cancel_action_handler,
 *     i_event_start_adder_handler, i_event_start_paster_handler,
 *     i_event_adder_pressed, i_event_adder_released, x_dnd_source_leave
 */
void i_event_end_action_handler(GschemToplevel *w_current)
{
  w_current->action_event->state = 0;

  i_event_adder_disconnect_events(w_current);
  i_event_unblock_buttons (w_current);

  i_status_action_stop(w_current);
}

/* ---------------------- Button Event Adder Handlers ---------------------- */

/*!
 * \brief Adder Action button Press events
 * \par Function Description
 * \internal handler for button press events when performing
 *  input operations for new objects.
 */
static int
i_event_adder_pressed(GtkWidget *widget, GdkEventButton *event,
                                         GschemToplevel *w_current)
{
  GschemEvent *action = w_current->action_event;

  void erase_rubber (void) {
    if (w_current->rubber_visible) {
      w_current->rubber_visible = FALSE;
      o_invalidate_rubber (w_current);
    }
  }

  if (w_current->event_state == action->state) {

    /* Note that these keys are also updated in x_event_motion */
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

      switch (w_current->event_state) {
        case(NETMODE):
          if (o_net_reset (w_current)) {
            erase_rubber();
            break;
          }
        case(PINMODE):
        case(LINEMODE):
        case(BOXMODE):
        case(CIRCLEMODE):
        case(ARCMODE):
        case(BUSMODE):
        case(PICTUREMODE):
          erase_rubber();
          i_event_end_action_handler(w_current);
          i_status_set_state(w_current, SELECT);
          break;

        case(PATHMODE):
          /* If path as been started, display context menu */
          if (w_current->temp_path) {
            x_menu_display_path_popup(w_current, event);
          }
          else {
            /* cancel path mode like other add modes */
            erase_rubber();
            i_event_end_action_handler(w_current);
            i_status_set_state(w_current, SELECT);
          }
        default:
          /* Not an Adder event; do nothing */
          break;
      }
    }
  }
  else {
    BUG_MSG("w_current->event_state != action->state");
    i_event_cancel_action_handler (w_current);
  }
  return(0);
}

/*!
 * \brief Adder Action button Release events
 * \par Function Description
 * \internal handler for button release events when performing
 *  input operations for new objects.
 */
static int
i_event_adder_released(GtkWidget *widget, GdkEventButton *event,
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
  }
  else {
    BUG_MSG("w_current->event_state != action->state");
    i_event_cancel_action_handler (w_current);
  }
  return(0);
}


/* ---------------------- Button Event Paster Handlers ---------------------- */

/*!
 * \brief Paster Action button Press events
 * \par Function Description
 * \internal handler for button press events when performing
 *  paste operations.
 */
static int
i_event_paster_pressed(GtkWidget *widget, GdkEventButton *event,
                                          GschemToplevel *w_current)
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

        if (geda_struct_place_get_place_list(w_current->toplevel)) {

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

          o_place_rotate(w_current, 90);

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
    i_event_cancel_action_handler (w_current);
  }
  return(0);
}

/*!
 * \brief Paster Action button release events
 * \par Function Description
 * \internal handler for button release events when performing
 *  paste operations.
 */
static int
i_event_paster_released(GtkWidget      *widget,
                        GdkEventButton *event,
                        GschemToplevel *w_current)
{
  GschemEvent *action = w_current->action_event;

  if (w_current->event_state == action->state) {

    if (event->button == 1) {

      switch (w_current->event_state) {

        case(DRAGMOVE):  /* 8 */
          if (geda_struct_place_get_place_list(w_current->toplevel)) {
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
  }
  else {
    BUG_IMSG("action->state != w_current->event_state",w_current->event_state);
    i_event_cancel_action_handler (w_current);
  }
  return(0);
}

/* ----------------------------------------------------------------- */

/*!
 * \brief Cancel Action Event Handler
 * \par Function Description
 *  Terminates the Action Event Handler and sets the
 *  event state to SELECT mode, invalidate all.
 *
 * \sa i_event_stop_action_handler
 */
void i_event_cancel_action_handler(GschemToplevel *w_current)
{
  GschemEvent *event = w_current->action_event;

  if (event->state) {
    i_event_end_action_handler(w_current);
    o_invalidate_all(w_current);
  }
  i_status_set_state(w_current, SELECT);
}

/*!
 * \brief Start the Adder Action Event Handler
 * \par Function Description
 *  The Adder Event handler is used to handle button events when
 *  the user is creating objects for event->state:
 * \par
 *  <DL>
 *    <DT>NETMODE</DT>
 *    <DT>PINMODE</DT>
 *    <DT>LINEMODE</DT>
 *    <DT>BOXMODE</DT>
 *    <DT>CIRCLEMODE</DT>
 *    <DT>ARCMODE</DT>
 *    <DT>PATHMODE</DT>
 *    <DT>PICTUREMODE</DT>
 *    <DT>BUSMODE</DT>
 *  </DL>
 */
void
i_event_start_adder_handler (GschemToplevel *w_current,
                             ActionInit      ifunc,
                             ActionAdder     rfunc)
{
  GschemEvent *event;

  if (w_current->action_event->state) {
    i_event_end_action_handler(w_current);
  }

  i_status_action_start(w_current);

  event                = w_current->action_event;
  event->state         = w_current->event_state;

  event->initializer   = (void*)ifunc;
  event->resolver.func = (void*)rfunc;

  event->press_butt    = (void*)i_event_adder_pressed;
  event->release_butt  = (void*)i_event_adder_released;

  i_event_action_enable_events(w_current);
}

/*!
 * \brief Start the Paster Action Event Handler
 * \par Function Description
 *  The Paster Event handler is used to handle button events when
 *  the user is creating objects for event->state:
 * \par
 *  <DL>
 *    <DT>COPYMODE</DT>
 *    <DT>COMPMODE</DT>
 *    <DT>TEXTMODE</DT>
 *    <DT>MCOPYMODE</DT>
 *    <DT>MOVEMODE</DT>
 *    <DT>ENDDND_MOVE_OBJ</DT>
 *    <DT>ENDDND_COPY_OBJ</DT>
 *    <DT>STARTDND</DT>
 *    <DT>PASTEMODE</DT>
 *  </DL>
 */
void
i_event_start_paster_handler (GschemToplevel *w_current, ActionPaster rfunc)
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

  i_event_action_enable_events(w_current);

  i_status_action_start(w_current);
}

/*!
 * \brief Stop the Action Event Handler
 * \par Function Description
 *  Terminates the Action Event Handler and sets the
 *  event state to SELECT mode.
 *
 * \sa i_event_cancel_action_handler
 */
void
i_event_stop_action_handler(GschemToplevel *w_current)
{
  GschemEvent *event = w_current->action_event;

  if (event->state) {
    i_event_end_action_handler(w_current);
  }

  i_status_set_state(w_current, SELECT);
}

/*! \brief Close main window callback
 *  \par Function Description
 *  When invoked (via signal delete_event), closes the current window
 *  when the user clicks the close button on the window which sends a
 *  DELETE signal to the app
 */
bool i_event_close_wm (GschemToplevel* w_current, GdkEvent *event, GtkWidget *widget)
{
  x_window_close(w_current);

  /* Stop further propagation of the delete_event signal for window:
   * If user has canceled the close the window should obviously not
   * be destroyed, otherwise window has already been destroyed and
   * there is nothing more to do
   */
  return TRUE;
}

/** @} endgroup I-Events */
