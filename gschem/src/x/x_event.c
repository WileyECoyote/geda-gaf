/* -*- C x_event.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2017 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file x_event.c
 * \brief Main Window Auxiliary Module for Event Handlers
 */

#include "../../include/gschem.h"
#include "../../include/x_window.h"

#include <geda_keysyms.h>
#include <geda_debug.h>

#ifdef PERFORMANCE
#include <geda_diagnostics.h>
#include <valgrind/callgrind.h>
#endif

/** \defgroup X-Events X-Event Module
 *  @{
 *  \brief Primary Canvas Event Module
 *  \par
 *   This module contains the primary event handlers for the drawing
 *   canvas and are connected by i_event_setup_handlers. This module
 *   does not directly handle drag-n-drop or notification events but
 *   does support drag-n-drop events; x_event_motion stores copies of
 *   drag events in the toplevel for the Drag-N-Drop module. Button
 *   event handlers can be temporarily suspended by secondary event
 *   handlers in other modules by utilizing routines in the i_event
 *   module.
 *
 *   Specifically, functions in this module handle the following
 *   events:
 *  <DL>
 *    <DT><B>"expose_event"</B></DT>
 *    <DT><B>"button_press_event"</B></DT>
 *    <DT><B>"button_release_event"</B></DT>
 *    <DT><B>"motion_notify_event"</B></DT>
 *    <DT><B>"configure_event"</B></DT>
 *    <DT><B>"key_press_event"</B></DT>
 *    <DT><B>"key_release_event"</B></DT>
 *    <DT><B>"scroll_event"</B></DT>
 *  </DL>
 */

/* used by mouse pan */
int start_pan_x, start_pan_y;
int throttle = 0;

/* used for the stroke stuff */
#ifdef HAVE_LIBSTROKE
static int DOING_STROKE = FALSE;
#endif /* HAVE_LIBSTROKE */

/* Threaded to repeat the previous action */
static bool x_event_idle_repeat_last (void *w_current)
{
  i_command_process(w_current, "repeat-last", 0, NULL, ID_ORIGIN_MOUSE);
  return FALSE;
}

/*!
 * \brief Button Press Event Handler
 * \par Function Description
 *  This function is called each time a mouse button is pressed. The
 *  routine checks which button triggered the event and whether a key
 *  -- SHIFT, CONTROL or ALT, was pressed when the press event occurred.
 *  The routine also check for a "double" click event, which GDK isolates
 *  for us. The appropriate action is performed based on the current state
 *  of the program. Enumerated state are defined in the file x_states.h.
 *
 * \param [in] widget    The drawing area which received the signal.
 * \param [in] event     The GdkEventButton event structure.
 * \param [in] w_current The toplevel environment as user data.
 *
 * \returns FALSE to propagate the event further.
 */
int x_event_button_pressed(GtkWidget      *widget,
                           GdkEventButton *event,
                           GschemToplevel *w_current)
{
  GList *list;

  int  w_x, w_y;
  int  unsnapped_wx, unsnapped_wy;

#if DEBUG_EVENTS
  printf("pressed button %d\n", event->button);
  printf("event state: %d\n", event->state);
  printf("event type: %d\n", event->type);
  printf("w_current state: %d\n", w_current->event_state);
  printf("Selection is:\n");
  o_selection_print_all(Current_Selection);
  printf("\n");
#endif

  SCREENtoWORLD (w_current, (int) event->x, (int) event->y,
                 &unsnapped_wx, &unsnapped_wy);
  w_x = snap_grid (w_current, unsnapped_wx);
  w_y = snap_grid (w_current, unsnapped_wy);

  /* Check for "double click" */
  if (event->type == GDK_2BUTTON_PRESS) {
    switch (w_current->event_state) {
      case (STARTSELECT):
      case (SELECT):
        list = geda_list_get_glist(Current_Selection);
        o_edit_objects (w_current, list, ID_ORIGIN_EVENT);
        i_status_set_state(w_current, SELECT);
        return(0);

      default:
        break;
    }
  }

  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK)    ? 1 : 0;

  if (event->button == GDK_BUTTON_PRIMARY) {

      /* Huge switch statement to evaluate state transitions */

      switch (w_current->event_state) {

        case (SELECT):
          o_select_start(w_current, unsnapped_wx, unsnapped_wy);
          break;

        case (NETMODE):
          o_net_start (w_current, w_x, w_y);
          break;

        case (PINMODE):
          o_pin_start (w_current, w_x, w_y);
          break;

        case (LINEMODE):
          o_line_start (w_current, w_x, w_y);
          break;

        case (BOXMODE):
          o_box_start (w_current, w_x, w_y);
          break;

        case (CIRCLEMODE):
          o_circle_start (w_current, w_x, w_y);
          break;

        case (ARCMODE):
          o_arc_start (w_current, w_x, w_y);
          break;
        case (PATHMODE):
          o_path_start (w_current, w_x, w_y);
          break;

        case (PICTUREMODE):
          o_picture_start (w_current, w_x, w_y);
          break;

        case (BUSMODE):
          o_bus_start (w_current, w_x, w_y);
          break;

        case (ZOOMBOX):
          if (!w_current->inside_action) {
            i_zoom_world_box_start(w_current, unsnapped_wx, unsnapped_wy);
          }
          break;

        case (MOVEMODE):
          o_move_start(w_current, w_x, w_y);
          break;

        case (COPYMODE):
          o_copy_start (w_current, w_x, w_y);
          break;

        case (MCOPYMODE):
          o_copy_multiple_start (w_current, w_x, w_y);
          break;

        case (PASTEMODE):
          o_buffer_paste_start (w_current, w_x, w_y);
          break;

        case(DESELECT):
          w_current->event_state = STARTDESELECT;
          break;

        case(ENDROTATE):
          list = geda_list_get_glist(Current_Selection);
          o_edit_rotate_world(w_current, w_x, w_y, 90, list);
          i_status_set_state(w_current, SELECT);
          break;

        case(ENDOFFSET):
          list = geda_list_get_glist(Current_Selection);
          o_edit_offset_world(w_current, w_x, w_y, list);
          break;

        case(ENDMIRROR):
          list = geda_list_get_glist(Current_Selection);
          o_edit_mirror_world(w_current, w_x, w_y, list);
          i_status_set_state(w_current, SELECT);
          break;

        case(PAN):
          i_pan_world(w_current, w_x, w_y);
          i_status_set_state(w_current, SELECT);
          break;

        case(STARTBREAK):
          i_status_set_state(w_current, o_break_start(w_current, unsnapped_wx, unsnapped_wy));
          break;

        case(ENDBREAK):
          if(!o_break_end (w_current, unsnapped_wx, unsnapped_wy)) {
            i_status_set_state(w_current, SELECT);
          }
          break;

        case(STARTEXTEND):
          i_status_set_state(w_current, o_extend_start(w_current, w_x, w_y));
          break;

        case EXTEND:
        case(ENDEXTEND):
          if(!o_extend_end (w_current, w_x, w_y)) {
            i_status_set_state(w_current, SELECT);
          }

        default:
          break;
    }
  }
  else if (event->button == 2) {

    /* try this out and see how it behaves */
    if (!w_current->inside_action) {

      switch (w_current->middle_button) {

        case(MOUSE_MIDDLE_ACTION):

          /* Do not search if shift key is pressed */
          if (!w_current->SHIFTKEY) {
            o_find_object(w_current, unsnapped_wx, unsnapped_wy, TRUE);
          }

          if (!o_select_is_selection(w_current)) {
            /* This means the above find did not find anything */
            i_status_action_stop(w_current);
            i_status_set_state(w_current, SELECT);
          }
          else { /* Only Copy and Move are supported */
            if (w_current->ALTKEY) {
              o_copy_start(w_current, w_x, w_y);
            }
            else {
              o_move_start_drag(w_current, w_x, w_y);
            }
          }
          break;

        case(MOUSE_MIDDLE_REPEAT):
          w_current->pointer_sx = event->x;
          w_current->pointer_sy = event->y;
          g_idle_add (x_event_idle_repeat_last, w_current);
          break;

#ifdef HAVE_LIBSTROKE
        case(MOUSE_MIDDLE_STROKE):
          DOING_STROKE=TRUE;
          break;
#endif /* HAVE_LIBSTROKE */

        case(MOUSE_MIDDLE_PAN):
          w_current->doing_pan     = TRUE;
          start_pan_x              = (int) event->x;
          start_pan_y              = (int) event->y;
          throttle                 = 0;
          break;

        case(MOUSE_MIDDLE_POPUP):
          i_status_update_sensitivities(w_current);  /* update menus before popup  */
          x_menu_display_main_popup(w_current, event);
          break;
      }
    }
  }
  else if (event->button == 3) {

    if (w_current->rubber_visible) {
        w_current->rubber_visible = FALSE;
        o_invalidate_rubber (w_current);
    }

    if (!w_current->inside_action) {

      if (w_current->third_button == POPUP_ENABLED) {
        i_status_update_sensitivities(w_current);  /* update menus before popup  */
        x_menu_display_main_popup(w_current, event);
      }
      else {

        if (!w_current->SHIFTKEY && w_current->third_button_cancel) {
          switch (w_current->event_state) {
            case(DESELECT):
            case(SELECT):
              break;
            case(STARTEXTEND):
              i_status_set_state(w_current, SELECT);
              break;
            default:
              i_callback_cancel(w_current, 0, NULL);
          }
        }

        w_current->doing_pan   = TRUE;
        start_pan_x            = (int) event->x;
        start_pan_y            = (int) event->y;
        throttle               = 0;
      }
    }
    else if (w_current->SHIFTKEY &&
             w_current->inside_action &&
             w_current->third_button == MOUSEPAN_ENABLED)
    {
      w_current->doing_pan     = TRUE;
      start_pan_x              = (int) event->x;
      start_pan_y              = (int) event->y;
      throttle                 = 0;
    }
    else {

      switch (w_current->event_state) {

        case(ENDOFFSET):
          i_status_action_stop(w_current);
          i_status_set_state(w_current, SELECT);
          break;

        default:
          i_callback_cancel(w_current, 0, NULL);
          break;
      }
    }
  }

  return(0);
}

/*!
 * \brief Button Release Event Handler
 * \par Function Description
 *   This function is called each time a mouse button is released. The
 *  routine checks which button triggered the event and whether a key
 *  -- SHIFT, CONTROL or ALT, was pressed when the release event occurred
 *  and performs the approiate action based on the current state of the
 *  program. Enumerated state are defined in the file x_states.h.
 *
 * \param [in] widget    The drawing area which received the signal.
 * \param [in] event     The GdkEventButton event structure.
 * \param [in] w_current The toplevel environment as user data.
 *
 * \returns FALSE to propagate the event further.
 */
bool x_event_button_released (GtkWidget      *widget,
                              GdkEventButton *event,
                              GschemToplevel *w_current)
{
  int unsnapped_wx, unsnapped_wy;

#if DEBUG_EVENTS
  printf("%s: entry! %d \n", __func__, w_current->event_state);
#endif

  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK)    ? 1 : 0;

  /* Capture where in the World this event occurred */
  SCREENtoWORLD (w_current, (int) event->x, (int) event->y,
                             &unsnapped_wx, &unsnapped_wy);

  if (event->button == 1) {

    GedaObject *object;

    /* Switch statement to evaluate state transitions */

    switch (w_current->event_state) {
      case(DESELECT):
      case(SELECT):
        /* do nothing - is almost same as not having a case */
        break;

      case(GRIPS):
        o_grips_end(w_current);
        i_status_set_state(w_current, SELECT);
        break;

      case(SBOX):
        o_select_box_end(w_current, unsnapped_wx, unsnapped_wy);
        i_status_set_state(w_current, SELECT);
        break;

      case(STARTDESELECT):
        object = o_find_selected_object(w_current, unsnapped_wx, unsnapped_wy);
        if (object) {
          if (!w_current->CONTROLKEY) {
            o_select_object(w_current, object, SINGLE, 1);
          }
        }
        break;

      case(STARTSELECT):
        o_select_end(w_current, unsnapped_wx, unsnapped_wy);
        i_status_set_state (w_current, SELECT);
        break;

      case(ENDOFFSET):
        i_status_action_start(w_current);
        break;
    }

    if (w_current->inside_action) {

      if (w_current->event_state == ZOOMBOX) {
        i_zoom_world_box_end(w_current, unsnapped_wx, unsnapped_wy);
        i_status_set_state(w_current, SELECT);
      }
    }

    if (w_current->render_adaptor == X11_ADAPTOR) {
      o_invalidate_all (w_current);
    }
  }
  else if (event->button == 2) {

    if (w_current->doing_pan) {
      w_current->doing_pan = FALSE;
      o_invalidate_all (w_current);
      if (w_current->undo_panzoom) {
        o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
      }
    }
    else {

      switch (w_current->middle_button) {
/*
        case(MOUSE_MIDDLE_ACTION):
          if (w_current->inside_action && Current_PlaceList) {
            switch (w_current->event_state) {
              case(DRAGMOVE):
                o_move_end(w_current);
                break;

              case(COPYMODE):
                o_copy_end(w_current);
                i_status_set_state(w_current, SELECT);
                break;
            }
          }
*/
#ifdef HAVE_LIBSTROKE
        case(MOUSE_MIDDLE_STROKE):
            DOING_STROKE = FALSE;
            x_stroke_translate_and_execute (w_current);
            break;
#endif /* HAVE_LIBSTROKE */

        case(MOUSE_MIDDLE_PAN):
          if (w_current->doing_pan) {
            w_current->doing_pan=FALSE;
            o_invalidate_all (w_current);
            if (w_current->undo_panzoom) {
              o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
            }
          }
          break;

        case(MOUSE_MIDDLE_POPUP):
          break;

          break;
        default:
          break;
      }
    }
  }
  else if (event->button == 3) {

    if (w_current->doing_pan) { /* just for ending a mouse pan */
      w_current->doing_pan = FALSE;
      o_invalidate_all (w_current);
      if (w_current->undo_panzoom) {
        o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
      }
    }
    else {

      i_status_action_stop(w_current);
      i_status_set_state(w_current, SELECT);

    }
  }

#if DEBUG_EVENTS
  printf("%s: exit! %d \n", __func__, w_current->event_state);
#endif

  return(FALSE);
} /* End Function x_event_button_released*/

/*!
 * \brief Updates GSCHEM TOPLEVEL when drawing area is configured.
 * \par Function Description
 *  This is the callback function connected to the configure event of
 *  the drawing area of the main window in order to update the size of
 *  the backingstore for the associated toplevel structure (creates a
 *  new pixmap) and re-pans each of its pages to keep their contents
 *  centered in the drawing area.
 *
 *  When the window is maximized, the zoom of every page is changed to
 *  best fit the previously displayed area of the page in the new
 *  area. Otherwise the current zoom level is left unchanged.
 *
 * \param [in] widget    The drawing area which received the signal.
 * \param [in] event     The event structure of signal configure-event.
 * \param [in] w_current The toplevel environment as user data.
 *
 * \returns FALSE to propagate the event further.
 */
bool x_event_configure (GtkWidget         *widget,
                        GdkEventConfigure *event,
                        GschemToplevel    *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int old_screen_width,  old_screen_height;
  int new_screen_width,  new_screen_height;

  if (toplevel == NULL) {
    BUG_MSG ("toplevel == NULL");
    return FALSE;
  }

  old_screen_width  = w_current->screen_width;
  old_screen_height = w_current->screen_height;
  new_screen_width  = event->width;
  new_screen_height = event->height;

  if (new_screen_width == old_screen_width &&
      new_screen_height == old_screen_height)
  {
    /* the size of the drawing area has not changed */
    /* nothing to do here */

    return FALSE;
  }

  /* update the GschemToplevel with new size of drawing area */
  w_current->screen_width  = new_screen_width;
  w_current->screen_height = new_screen_height;

  /* if the current page has been setup */
  if (toplevel->page_current != NULL) {

    GdkWindow *window;
    GList     *iter;
    GList     *pages;

    double relative_zoom_factor = 1.0;

    window = geda_get_widget_window(gtk_widget_get_toplevel (widget));

    /* in the case the user has maximised the window (hence the */
    /* configure event) fit the view by playing with zoom level */
    if (gdk_window_get_state (window) & GDK_WINDOW_STATE_MAXIMIZED) {

      double width_ratio, height_ratio;

      /* tweak relative_zoom to better fit page in maximized window */
      width_ratio  = ((double)new_screen_width)  / ((double)old_screen_width);
      height_ratio = ((double)new_screen_height) / ((double)old_screen_height);

      /* keep smallest ratio as relative zoom factor when panning */
      if (width_ratio < height_ratio) {
        relative_zoom_factor = width_ratio;
      }
      else {
        relative_zoom_factor = height_ratio;
      }
    }

    pages = geda_toplevel_get_pages(toplevel);

    /* re-pan each page of the GedaToplevel */
    for (iter = pages; iter != NULL; iter  = iter->next) {

      double cx, cy;

      Page  *p_current = (Page*)iter->data;

      /* doing this the aspect ratio is kept when changing (hw)*/
      cx = ((double)(p_current->left + p_current->right))  / 2;
      cy = ((double)(p_current->top  + p_current->bottom)) / 2;

      i_pan_world_general (w_current, p_current, cx, cy, relative_zoom_factor, I_PAN_DONT_REDRAW);
    }

    /* redraw the current page and update UI */
    o_invalidate_all (w_current);
    x_scrollbars_update (w_current);
  }

  return FALSE;
}

/*!
 * \brief On event Expose
 * \par Function Description
 *  The expose event in Gtk is equivalent to the OnDraw in MS Windows,
 *  except it's not just a hook, we actually have do the drawing. We
 *  don't do in any drawing here, the function creates a temporary Cairo
 *  drawable context and calls o_redraw_rectangle() to do the actual
 *  drawing. The temporary drawable is destroyed and the original
 *  restored.
 */
int x_event_expose (GtkWidget      *widget,
                    GdkEventExpose *event,
                    GschemToplevel *w_current)
{

#if DEBUG_EVENTS
  const char  *name = gtk_widget_get_name (widget);
  printf ("%s, exposing %s <%p>\n", __func__, name, widget);
#endif

  cairo_t *save_cr;

  save_cr = w_current->cr;

  gdk_window_begin_paint_region(widget->window, event->region);

  w_current->cr = gdk_cairo_create(widget->window);

  cairo_set_antialias(w_current->cr, w_current->anti_aliasing);

  gdk_cairo_rectangle (w_current->cr, &(event->area));

  cairo_clip (w_current->cr);

  x_grid_repaint_background (w_current, &(event->area));

  x_grid_draw_grid_region (w_current, &(event->area));

  o_redraw_rectangle (w_current, &(event->area));

  gdk_window_end_paint(widget->window);

  cairo_destroy (w_current->cr);

  w_current->cr = save_cr;

  return FALSE;
}

/*This function also raises any open dialogs to the foreground if the
 *toplevel raise_dialog_boxes is TRUE
 */
static bool x_event_raise_dialogs(void *user_data)
{
  GschemToplevel *w_current = (GschemToplevel*)user_data;

  /* raise the dialog boxes if this feature is enabled */
  if (w_current->raise_dialog_boxes) {
    x_dialog_raise_all(w_current);
  }

  return (w_current->raise_dialog_boxes != 0);
}

void x_event_governor(GschemToplevel *w_current)
{
  if (w_current->raise_dialog_boxes > 0) {
    g_timeout_add (w_current->raise_dialog_boxes, x_event_raise_dialogs, w_current);
  }
}

/*!
 * \brief Get a snapped pointer position in world coordinates
 * \par Function Description
 *  Queries GTK for the mouse location in world coordinates,
 *  then snaps it to the grid.
 *
 * \param [in]  w_current  The GschemToplevel object.
 * \param [out] wx         Return location for the snapped X coordinate.
 * \param [out] wy         Return location for the snapped Y coordiante.
 */
static inline
void x_event_get_snapped_pointer (GschemToplevel *w_current, int *wx, int *wy)
{
  int sx, sy;
  int unsnapped_wx, unsnapped_wy;

  gtk_widget_get_pointer (DrawingArea, &sx, &sy);

  SCREENtoWORLD   (w_current, sx, sy, &unsnapped_wx, &unsnapped_wy);
  *wx = snap_grid (w_current, unsnapped_wx);
  *wy = snap_grid (w_current, unsnapped_wy);
}

/*!
 * \brief Callback to handle key events in the drawing area.
 * \par Function Description
 *  GTK+ callback function (registered in x_window_setup_draw_events())
 *  which handles key press and release events from the GTK+ system.
 *
 * \param [in] widget the widget that generated the event
 * \param [in] event the event itself
 * \param w_current the toplevel environment
 *
 * \returns TRUE if the event has been handled.
 */
bool x_event_key (GtkWidget      *widget,
                  GdkEventKey    *event,
                  GschemToplevel *w_current)
{
  bool retval      = FALSE;
  int  control_key = 0;
  int  pressed;
  int  wx, wy;

#if DEBUG_EVENTS
  printf("%s: Pressed key %i.\n", _func__, event->keyval);
#endif

  /* update the state of the modifiers */
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK)    ? 1 : 0;
  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK)   ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;

  pressed = (event->type == GDK_KEY_PRESS) ? 1 : 0;

  switch (event->keyval) {
    case GDK_Alt_L:
    case GDK_Alt_R:
      w_current->ALTKEY = pressed;
      break;

    case GDK_Shift_L:
    case GDK_Shift_R:
      w_current->SHIFTKEY = pressed;
      break;

    case GDK_Control_L:
    case GDK_Control_R:
      control_key = 1;
      w_current->CONTROLKEY = pressed;
      break;
  }

  /* Switch statement to evaluate state transitions. Jump to
   * end_key label to escape the state evaluation rather
   * than returning from the function directly. */

  if (w_current->inside_action && control_key) {

    x_event_get_snapped_pointer (w_current, &wx, &wy);

    switch (w_current->event_state) {

      case LINEMODE:
        o_line_motion (w_current, wx, wy);
        break;

      case NETMODE:
        o_net_motion (w_current, wx, wy);
        break;

      case PATHMODE:
        o_path_motion (w_current, wx, wy);
        break;

      case BUSMODE:
        o_bus_motion (w_current, wx, wy);
        break;

      case DRAGMOVE:
      case MOVEMODE:
        o_move_motion (w_current, wx, wy);
        break;

      case COPYMODE:
      case MCOPYMODE:
        o_place_motion (w_current, wx, wy);

      default:
        break;
    }
  }

  if (pressed)
    retval = g_keys_execute (w_current, event) ? TRUE : FALSE;

  return retval;
}

/*!
 * \brief Drawing Area Pointer Motion Callback Handler
 * \par Function Description
 *  This function is called by the underling window context event
 *  dispatcher when ever the mouse pointer position changes.
 *
 * \remarks Motion events are not swapped like buttons, i.e. this
 *          is the only motion event handler!
 */
bool x_event_motion (GtkWidget      *widget,
                     GdkEventMotion *event,
                     GschemToplevel *w_current)
{
  int unsnapped_wx, unsnapped_wy;
  int w_x, w_y;

  GdkDisplay *display;
  GdkEvent   *test_event;

  g_return_val_if_fail ((w_current != NULL), 0);

#if DEBUG_EVENTS
  printf("MOTION!\n");
#endif

#ifdef HAVE_LIBSTROKE
  if (DOING_STROKE == TRUE) {
    x_stroke_record (w_current, event->x, event->y);
    return(0);
  }
#endif /* HAVE_LIBSTROKE */

  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK)    ? 1 : 0;

  /* Werner: Skip the motion event if there are other motion events
   * in the gdk event queue but only if event is the same event and
   * no buttons or modifier keys changed*/
  display = gdk_drawable_get_display (event->window);

  if ((test_event = gdk_display_get_event(display)) != NULL) {

    int skip_event = 0;

    if (test_event->type == GDK_MOTION_NOTIFY &&
       ((GdkEventMotion*)test_event)->state == event->state)
    {
      skip_event++;
    }

    gdk_event_put(test_event); /* put it back in front of the queue */
    gdk_event_free(test_event);

    if (skip_event) {
      return 0;
    }
  }

  SCREENtoWORLD (w_current, (int) event->x, (int) event->y,
                                  &unsnapped_wx, &unsnapped_wy);

  w_x = snap_grid (w_current, unsnapped_wx);
  w_y = snap_grid (w_current, unsnapped_wy);

  /* If visible update the Coord Dialog */
  if (w_current->cowindow) {
    x_dialog_coord_update_display(w_current, (int)event->x, (int)event->y, w_x, w_y);
  }

  /* Update coordinates display on the status bar*/
  i_status_update_coordinates(w_current, w_x, w_y);

  if (w_current->doing_pan) {

    int pdiff_x = (event->x - start_pan_x) * w_current->mousepan_gain;
    int pdiff_y = (event->y - start_pan_y) * w_current->mousepan_gain;

    if (!(throttle % 5)) {

      i_pan_world_mouse(w_current, pdiff_x, pdiff_y);

      start_pan_x = (int) event->x;
      start_pan_y = (int) event->y;
    }
    throttle++;
    return(0);
  }

  if (w_current->inside_action) {

    if (Current_PlaceList != NULL) {

      switch (w_current->event_state) {
        case (COMPMODE)  :
        case (COPYMODE)  :
        case (MCOPYMODE) :
        case (PASTEMODE) :
        case (TEXTMODE)  : o_place_motion (w_current, w_x, w_y); break;
        default: break;
      }
    }
    else {

        switch (w_current->event_state) {

        case(NETMODE)    :  o_net_motion     (w_current, w_x, w_y); break;
        case(PINMODE)    :  o_pin_motion     (w_current, w_x, w_y); break;
        case(LINEMODE)   :  o_line_motion    (w_current, w_x, w_y); break;
        case(BOXMODE)    :  o_box_motion     (w_current, w_x, w_y); break;
        case(CIRCLEMODE) :  o_circle_motion  (w_current, w_x, w_y); break;
        case(ARCMODE)    :  o_arc_motion     (w_current, w_x, w_y); break;
        case(PATHMODE)   :  o_path_motion    (w_current, w_x, w_y); break;
        case(PICTUREMODE):  o_picture_motion (w_current, w_x, w_y); break;
        case(BUSMODE)    :  o_bus_motion     (w_current, w_x, w_y); break;
        case(GRIPS)      :  o_grips_motion   (w_current, w_x, w_y); break;
        case(SBOX)       :  o_select_box_motion (w_current, unsnapped_wx, unsnapped_wy); break;
        case(ZOOMBOX)    :  i_zoom_world_box_motion (w_current, unsnapped_wx, unsnapped_wy);
        default: break;
      }
    }
  }
  else {

    switch (w_current->event_state) {
      case(NETMODE):
        if (w_current->magnetic_net_mode) {
          o_net_start_magnetic(w_current, w_x, w_y); break;
        }
      default: break;
    }
  }

  switch (w_current->event_state) {

    case(STARTSELECT):
      if (o_select_motion (w_current, unsnapped_wx, unsnapped_wy)) {

        /* Start moving the selected object(s) */
        o_move_start_drag(w_current, w_x, w_y);
        if (w_current->drag_event) {
          gdk_event_free(w_current->drag_event);
        }
        w_current->drag_event = gdk_event_copy((GdkEvent*)event);
      }
      else {
        break;
      }

    case(DRAGMOVE):
    case(MOVEMODE):
      if (w_current->inside_action) {
        o_move_motion (w_current, w_x, w_y);
      }
      break;
  }

  return(0);
}

/*!
 * \brief Callback for Window Scroll Events.
 * \par Function Description
 *  This function is called with the drawing window receives a scroll
 *  event signal. Typically the signal originated from a pointing device.
 *
 * \note These signals do not originate from the scrollbars.
 *
 * \param [in] widget    The DrawingArea scrollable widget.
 * \param [in] event     The event record, likely from the mouse.
 * \param [in] w_current The toplevel object.
 */
bool x_event_scroll (GtkWidget      *widget,
                     GdkEventScroll *event,
                     GschemToplevel *w_current)
{
  bool pan_xaxis = FALSE;
  bool pan_yaxis = FALSE;
  bool zoom      = FALSE;

  GtkAdjustment *adjust;

  g_return_val_if_fail ((w_current != NULL), 0);

  /* update the state of the modifiers */
  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK) ? 1 : 0;

  if (w_current->scroll_wheel == SCROLL_WHEEL_CLASSIC) {
    /* Classic gschem behaviour */
    zoom      = !w_current->CONTROLKEY && !w_current->SHIFTKEY;
    pan_yaxis = !w_current->CONTROLKEY &&  w_current->SHIFTKEY;
    pan_xaxis =  w_current->CONTROLKEY && !w_current->SHIFTKEY;
  }
  else {
    /* GTK style behaviour */
    zoom      =  w_current->CONTROLKEY && !w_current->SHIFTKEY;
    pan_yaxis = !w_current->CONTROLKEY && !w_current->SHIFTKEY;
    pan_xaxis = !w_current->CONTROLKEY &&  w_current->SHIFTKEY;
  }

  /* If the user has a left/right scroll wheel, check if x-axis scrolling is enabled */
  if (event->direction == GDK_SCROLL_LEFT ||
      event->direction == GDK_SCROLL_RIGHT) {
      zoom      = FALSE;
      pan_yaxis = FALSE;
      pan_xaxis = w_current->pointer_hscroll;
  }

  /* You must have scrollbars enabled if you want to use the scroll wheel to pan */
  if (!w_current->scrollbars) {
    pan_xaxis = FALSE;
    pan_yaxis = FALSE;
  }

  int pan_direction  = 1;
  int zoom_direction = ZOOM_IN_DIRECTIVE;

  switch (event->direction) {
    case GDK_SCROLL_UP:
    case GDK_SCROLL_LEFT:
      pan_direction  = -1;
      zoom_direction = ZOOM_IN_DIRECTIVE;
      break;
    case GDK_SCROLL_DOWN:
    case GDK_SCROLL_RIGHT:
      pan_direction  =  1;
      zoom_direction = ZOOM_OUT_DIRECTIVE;
      break;
  }

  if (zoom) {
    i_zoom_world(w_current, zoom_direction, ID_ORIGIN_MOUSE, 0);
  }

  if (pan_xaxis) {

    adjust = gtk_range_get_adjustment(HorizontalScrollRange);
    gtk_adjustment_set_value(adjust, min(adjust->value + pan_direction *
                                        (adjust->page_increment /
                                         w_current->scrollpan_steps),
                                         adjust->upper - adjust->page_size));
  }

  if (pan_yaxis) {

    adjust = gtk_range_get_adjustment(VerticalScrollRange);
    gtk_adjustment_set_value(adjust, min(adjust->value + pan_direction *
                                        (adjust->page_increment /
                                         w_current->scrollpan_steps),
                                         adjust->upper - adjust->page_size));
  }

  if (w_current->undo_panzoom && (zoom || pan_xaxis || pan_yaxis)) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  }

  return 1;
}

/*!
 * \brief Horizontal Scrollbar Value Changed Event Handler
 * \par Function Description
 *  This function is called when the value of the horizontal scrollbar
 *  widget has been changed. If the value changed because the user moved
 *  the slider then the page geometry is updated. If the value was changed
 *  by some other means, such as the mouse wheel or keyboard, then the page
 *  geometry has already been updated, so as long as the slider value matches
 *  the page geometry this function does nothing.
 */
void x_event_hschanged (GtkAdjustment *adjust, GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  if (w_current->scrollbars) {

    GedaToplevel *toplevel;
    int current_left;
    int new_left;

    toplevel     = w_current->toplevel;
    current_left = toplevel->page_current->left;
    new_left     = (int) adjust->value;

    if (new_left - current_left) {

      int new_right;

      new_right  = toplevel->page_current->right - (current_left - new_left);

      toplevel->page_current->right = new_right;
      toplevel->page_current->left  = new_left;

      o_invalidate_all (w_current);
    }
  }
}

/*!
 * \brief Vertical Scrollbar Value Changed Event Handler
 * \par Function Description
 *  This function is called when the value of the vertical scrollbar
 *  widget has been changed. If the value changed because the user moved
 *  the slider then the page geometry is updated. If the value was changed
 *  by some other means, such as the mouse wheel or keyboard, then the page
 *  geometry has already been updated, so as long as the slider value matches
 *  the page geometry this function does nothing.
 */
void x_event_vschanged (GtkAdjustment *adjust, GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  if (w_current->scrollbars) {

    GedaToplevel *toplevel;

    int current_bottom;
    int new_bottom;

    toplevel       = w_current->toplevel;
    current_bottom = toplevel->page_current->bottom;
    new_bottom     = w_current->world_bottom - (int) adjust->value;

    if (new_bottom - current_bottom) {

      int new_top;

      new_top  = toplevel->page_current->top - (current_bottom - new_bottom);

      toplevel->page_current->top    = new_top;
      toplevel->page_current->bottom = new_bottom;

      o_invalidate_all (w_current);

#if DEBUG_EVENTS
      printf("vrange %f %f\n",  adjust->lower, adjust->upper);
      printf("vvalue %f\n",     adjust->value);
      printf("actual: %d %d\n", toplevel->page_current->top,
      toplevel->page_current->bottom);
#endif
    }
  }
}

/** @} end group X-Events */
