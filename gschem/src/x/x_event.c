/* -*- C x_event.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file x_event.c
 * \brief Main Window Auxiliary Module for Event Handlers
 */

#include "gschem.h"
#include "x_window.h"

#include <gdk/gdkkeysyms.h>

#include <geda_debug.h>

/* used by mouse pan */
int start_pan_x, start_pan_y;
int throttle = 0;

/* used for the stroke stuff */
#ifdef HAVE_LIBSTROKE
static int DOING_STROKE = FALSE;
#endif /* HAVE_LIBSTROKE */

/*! \brief Button Press Event Handler
 *  \par Function Description
 *   This function is called each time a mouse button is pressed. The
 *  routine checks which button triggered the event and whether a key
 *  -- SHIFT, CONTROL or ALT, was pressed when the press event occured.
 * The routine also check for a "double" click event, which GDK isolates
 * for us. The approiate action is performed based on the current state
 * of the program. Enumerated state are defined in the file x_states.h.
 */
int x_event_button_pressed(GtkWidget      *widget,
                           GdkEventButton *event,
                           GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int  w_x, w_y;
  int  unsnapped_wx, unsnapped_wy;

#if DEBUG_EVENTS
  printf("pressed button %d\n", event->button);
  printf("event state: %d\n", event->state);
  printf("event type: %d\n", event->type);
  printf("w_current state: %d\n", w_current->event_state);
  printf("Selection is:\n");
  o_selection_print_all((Top_Selection));
  printf("\n");
#endif

  SCREENtoWORLD (w_current, (int) event->x, (int) event->y,
                 &unsnapped_wx, &unsnapped_wy);
  w_x = snap_grid (w_current, unsnapped_wx);
  w_y = snap_grid (w_current, unsnapped_wy);

  if (event->type == GDK_2BUTTON_PRESS &&
     (w_current->event_state == STARTSELECT ||
      w_current->event_state == SELECT))
  {
    if (o_select_is_selection (w_current)) {
      o_edit_objects (w_current, geda_list_get_glist( Top_Selection ), ID_ORIGIN_EVENT);
      i_status_set_state(w_current, SELECT);
      return(0);
    }
  }

  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK)    ? 1 : 0;

  /* Huge switch statement to evaluate state transitions */

  if (event->button == GDK_BUTTON_PRIMARY) {

    switch(w_current->event_state) {
      case(DESELECT):
        w_current->event_state = STARTDESELECT;
        break;
      case(SELECT):

        /* look for grips or fall through if not enabled */
        if (!o_grips_start(w_current, unsnapped_wx, unsnapped_wy)) {

          /* now go into normal SELECT */
          w_current->event_state = STARTSELECT;
          w_current->first_wx = w_current->second_wx = unsnapped_wx;
          w_current->first_wy = w_current->second_wy = unsnapped_wy;
        }
        else {
          /* a grip was found */
          w_current->event_state   = GRIPS;
          w_current->inside_action = TRUE;
        }
        break;

      case(STARTCOPY):
        if (o_select_is_selection(w_current)) {
          o_copy_start(w_current, w_x, w_y);
          w_current->event_state   = COPY;
          w_current->inside_action = TRUE;
        }
        break;

      case(STARTMCOPY):
        if (o_select_is_selection(w_current)) {
          o_copy_start(w_current, w_x, w_y);
          w_current->event_state   = MCOPY;
          w_current->inside_action = TRUE;
        }
        break;

      case(STARTMOVE):
        if (o_select_is_selection(w_current)) {
          o_move_start(w_current, w_x, w_y);
          w_current->event_state   = MOVE;
          w_current->inside_action = TRUE;
        }
        break;

      case(STARTPASTE):
        o_buffer_paste_start(w_current, w_x, w_y, w_current->buffer_number);
        w_current->event_state   = ENDPASTE;
        w_current->inside_action = TRUE;
        break;

      case(DRAWLINE):
        o_line_start(w_current, w_x, w_y);
        w_current->event_state   = ENDLINE;
        w_current->inside_action = TRUE;
        break;

      case(ENDLINE):
        o_line_end(w_current, w_x, w_y);
        w_current->inside_action = FALSE;
        w_current->event_state   = DRAWLINE;
        break;

      case DRAWPATH:
        o_path_start (w_current, w_x, w_y);
        w_current->event_state   = ENDPATH;
        w_current->inside_action = TRUE;
        break;

      case PATHCONT:
        o_path_continue (w_current, w_x, w_y);
        w_current->event_state   = ENDPATH;
        w_current->inside_action = TRUE;
        break;

      case(DRAWBOX):
        o_box_start(w_current, w_x, w_y);
        w_current->event_state   = ENDBOX;
        w_current->inside_action = TRUE;
        break;

      case(ENDBOX):
        o_box_end(w_current, w_x, w_y);
        w_current->inside_action = FALSE;
        w_current->event_state   = DRAWBOX;
        break;

      case(DRAWPICTURE):
        o_picture_start(w_current, w_x, w_y);
        w_current->event_state   = ENDPICTURE;
        w_current->inside_action = TRUE;
        break;

      case(ENDPICTURE):
        o_picture_end(w_current, w_x, w_y);
        w_current->inside_action = FALSE;
        w_current->event_state   = DRAWPICTURE;
        break;

      case(DRAWCIRCLE):
        o_circle_start(w_current, w_x, w_y);
        w_current->event_state   = ENDCIRCLE;
        w_current->inside_action = TRUE;
        break;

      case(ENDCIRCLE):
        o_circle_end(w_current, w_x, w_y);
        w_current->inside_action = FALSE;
        w_current->event_state   = DRAWCIRCLE;
        break;

      case(DRAWARC):
        o_arc_start(w_current, w_x, w_y);
        w_current->event_state   = ENDARC;
        w_current->inside_action = TRUE;
        break;

      case(ENDARC):
        o_arc_end1(w_current, w_x, w_y);
        w_current->inside_action = FALSE;
        w_current->event_state   = DRAWARC;
        break;

      case(DRAWPIN):
        o_pin_start(w_current, w_x, w_y);
        w_current->event_state   = ENDPIN;
        w_current->inside_action = TRUE;
        break;

      case(ENDPIN):
        o_pin_end(w_current, w_x, w_y);
        w_current->inside_action = FALSE;
        w_current->event_state   = DRAWPIN;
        break;

      case(STARTDRAWNET):  /*! \todo change state name? */
        o_net_start(w_current, w_x, w_y);
        w_current->event_state   = DRAWNET;
        w_current->inside_action = TRUE;
        break;

      case(STARTDRAWBUS):
        o_bus_start(w_current, w_x, w_y);
        w_current->inside_action = TRUE;
        w_current->event_state   = DRAWBUS;
        break;

      case(DRAWNET):
      case(NETCONT):
        /* Only continue the net if net end worked */
        if (o_net_end(w_current, w_x, w_y)) {
          o_net_start(w_current, w_current->first_wx, w_current->first_wy);
          w_current->event_state = NETCONT;
        }
        else { /* cleanup and start a new net */
          o_net_invalidate_rubber (w_current);
          o_net_reset(w_current);
          i_status_set_state(w_current, STARTDRAWNET);
          w_current->inside_action = FALSE;
        }
        break;

      case(DRAWBUS):
      case(BUSCONT):
        /* Only continue the net if net end worked */
        if (o_bus_end(w_current, w_x, w_y)) {
          o_bus_start(w_current, w_current->first_wx, w_current->first_wy);
          w_current->event_state=BUSCONT;
        }
        else {
          w_current->inside_action = FALSE;
          i_status_set_state(w_current, STARTDRAWBUS);
        }
        break;
      case(ENDCOMP):
        o_place_end(w_current, w_x, w_y, w_current->continue_component_place,
                    NULL, "%add-objects-hook");
        if (!w_current->continue_component_place) {
          w_current->inside_action = FALSE;
          i_status_set_state(w_current, SELECT);
        }
        break;

      case(ENDPASTE):
        o_place_end(w_current, w_x, w_y, FALSE, NULL, "%paste-objects-hook");
        w_current->inside_action = FALSE;
        i_status_set_state(w_current, SELECT);
        break;

      case(ENDROTATEP):
        o_edit_rotate_world(w_current, w_x, w_y, 90,
                            geda_list_get_glist(Current_Selection));

        w_current->inside_action = FALSE;
        i_status_set_state(w_current, SELECT);
        break;

      case(ENDMIRROR):
        o_edit_mirror_world(w_current, w_x, w_y,
                            geda_list_get_glist(Current_Selection));

        w_current->inside_action = FALSE;
        i_status_set_state(w_current, SELECT);
        break;

      case(ENDTEXT):
        o_place_end(w_current, w_x, w_y, FALSE, NULL, "%add-objects-hook");
        w_current->inside_action = FALSE;
        i_status_set_state(w_current, SELECT);
        break;

      case(STARTPAN):
        i_pan_world(w_current, w_x, w_y);
        i_status_set_state(w_current, SELECT);
        break;

      case(ZOOMBOXSTART):
        o_redraw_cleanstates(w_current);
        i_zoom_world_box_start(w_current, unsnapped_wx, unsnapped_wy);
        w_current->inside_action = TRUE;
        i_status_set_state(w_current, ZOOMBOXEND);
        break;

      case(STARTEXTEND):
        i_status_set_state(w_current, o_extend_start(w_current, w_x, w_y));
        break;

      case EXTEND:
      case(ENDEXTEND):
        if(!o_extend_end (w_current, w_x, w_y)) {
          i_status_set_state(w_current, SELECT);
        }
        break;
    }
  }
  else if (event->button == 2) {

    if (w_current->event_state == DRAWPICTURE) {

      if (w_current->current_pixbuf != NULL) {
        GEDA_UNREF(w_current->current_pixbuf);
        w_current->current_pixbuf = NULL;
      }

      GEDA_FREE(w_current->pixbuf_filename);
    }

    /* try this out and see how it behaves */
    if (w_current->inside_action) {
      if (!(w_current->event_state == ENDCOMP  ||
        w_current->event_state     == ENDTEXT  ||
        w_current->event_state     == ENDMOVE  ||
        w_current->event_state     == ENDCOPY  ||
        w_current->event_state     == ENDMCOPY ||
        w_current->event_state     == ENDPASTE )) {
        i_callback_cancel(w_current, 0, NULL);
        }
    }
    else {

      switch(w_current->middle_button) {

        case(MOUSE_MIDDLE_ACTION):
          /* Only Copy and Move are supported */
          /* Do not search if shift key is depresed */
          if (!w_current->SHIFTKEY) {
            o_find_object(w_current, unsnapped_wx, unsnapped_wy, TRUE);
          }

          if (!o_select_is_selection(w_current)) {
            /* This means the above find did not find anything */
            w_current->inside_action = FALSE;
            i_status_set_state(w_current, SELECT);
          }
          else {
            if (w_current->ALTKEY) {
              o_copy_start(w_current, w_x, w_y);
              w_current->inside_action = TRUE;
              i_status_set_state(w_current, COPY);
            }
            else {
              o_move_start(w_current, w_x, w_y);
              w_current->inside_action = TRUE;
              i_status_set_state(w_current, MOVE);
            }
          }
          break;

        case(MOUSE_MIDDLE_REPEAT):
          w_current->pointer_sx = event->x;
          w_current->pointer_sy = event->y;
          i_command_process(w_current, "repeat-last", 0, NULL, ID_ORIGIN_MOUSE);
          break;

#ifdef HAVE_LIBSTROKE
        case(MOUSE_MIDDLE_STROKE):
          DOING_STROKE=TRUE;
          break;
#endif /* HAVE_LIBSTROKE */

        case(MOUSE_MIDDLE_PAN):
          //w_current->event_state   = MOUSEPAN; /* start */
          //w_current->inside_action = TRUE;
          w_current->doing_pan     = TRUE;
          start_pan_x              = (int) event->x;
          start_pan_y              = (int) event->y;
          throttle                 = 0;
          break;
      }
    }
  }
  else if (event->button == 3) {

    if (!w_current->inside_action) {
      if (w_current->third_button == POPUP_ENABLED) {
        i_status_update_sensitivities(w_current);  /* update menus before popup  */
        x_menu_display_popup(w_current, event);
      }
      else {
        //w_current->event_state   = MOUSEPAN; /* start */
        //w_current->inside_action = TRUE;
        w_current->doing_pan   = TRUE;
        start_pan_x            = (int) event->x;
        start_pan_y            = (int) event->y;
        throttle               = 0;
      }
    }
    else if (w_current->inside_action && w_current->third_button == MOUSEPAN_ENABLED){
      w_current->doing_pan     = TRUE;
      start_pan_x              = (int) event->x;
      start_pan_y              = (int) event->y;
      throttle                 = 0;
    }
    else {

      switch (w_current->event_state) {
        case(NETCONT):

        case(STARTDRAWNET):
        case(DRAWNET):
          w_current->inside_action = FALSE;
          i_status_set_state (w_current, STARTDRAWNET);
          o_net_invalidate_rubber (w_current);
          o_net_reset (w_current);
          break;

        case(STARTDRAWBUS):
        case(DRAWBUS):
        case(BUSCONT):
          w_current->inside_action = FALSE;
          i_status_set_state (w_current, STARTDRAWBUS);
          o_bus_invalidate_rubber (w_current);
          break;

        case(DRAWPIN):
        case(ENDPIN):
          w_current->inside_action = FALSE;
          i_status_set_state(w_current, DRAWPIN);
          o_pin_invalidate_rubber (w_current);
          break;

        case(DRAWLINE):
        case(ENDLINE):
          w_current->inside_action = FALSE;
          i_status_set_state(w_current, DRAWLINE);
          o_line_invalidate_rubber (w_current);
          break;

        case DRAWPATH:
        case PATHCONT:
        case ENDPATH:
          w_current->inside_action = FALSE;
          i_status_set_state (w_current, DRAWPATH);
          o_path_invalidate_rubber (w_current);
          break;

        case(DRAWBOX):
        case(ENDBOX):
          w_current->inside_action = FALSE;
          i_status_set_state(w_current, DRAWBOX);
          o_box_invalidate_rubber (w_current);
          break;

        case(DRAWPICTURE):
        case(ENDPICTURE):
          w_current->inside_action = FALSE;
          i_status_set_state(w_current, DRAWPICTURE);
          o_picture_invalidate_rubber (w_current);
          break;

        case(DRAWCIRCLE):
        case(ENDCIRCLE):
          w_current->inside_action = FALSE;
          i_status_set_state(w_current, DRAWCIRCLE);
          o_circle_invalidate_rubber (w_current);
          break;

        case(DRAWARC):
        case(ENDARC):
          w_current->inside_action = FALSE;
          i_status_set_state(w_current, DRAWARC);
          o_arc_invalidate_rubber (w_current);
          break;

        default:
          i_callback_cancel(w_current, 0, NULL);
          break;
      }
    }
  }

  return(0);
}

/*! \brief Button Release Event Handler
 *  \par Function Description
 *   This function is called each time a mouse button is released. The
 *  routine checks which button triggered the event and whether a key
 *  -- SHIFT, CONTROL or ALT, was pressed when the release event occured
 *  and performs the approiate action based on the current state of the
 *  program. Enumerated state are defined in the file x_states.h.
 */
bool x_event_button_released (GtkWidget      *widget,
                              GdkEventButton *event,
                              GschemToplevel *w_current)
{
  Object *object;
  int w_x, w_y;
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

    /* Huge switch statement to evaluate state transitions. Jump to
     * end_button_released label to escape the state evaluation rather
     * than returning from the function directly. */

    switch(w_current->event_state) {
      case(DESELECT):
      case(SELECT):
        /* do nothing - is almost same as not having a case */
        break;
      case(MOVE):
        w_current->event_state = ENDMOVE;
        break;

      case(COPY):
        w_current->event_state = ENDCOPY;
        break;

      case(MCOPY):
        w_current->event_state = ENDMCOPY;
        break;
      case(GRIPS):
        o_grips_end(w_current);
        w_current->inside_action = FALSE;
        i_status_set_state(w_current, SELECT);
        break;
      case(ENDMOVE):
        if (w_current->drag_event) {
          gdk_event_free(w_current->drag_event);
          w_current->drag_event = NULL;
        }
        o_move_end(w_current);
        w_current->inside_action = FALSE;
        i_status_set_state(w_current, SELECT);
        break;

      case(ENDCOPY):
        o_copy_end(w_current);
        w_current->inside_action = FALSE;
        i_status_set_state(w_current, SELECT);
        break;

      case(ENDMCOPY):
        o_copy_multiple_end(w_current);
        /* having this stay in copy was driving me nuts*/
        w_current->inside_action = TRUE;
        /* Keep the state and the inside_action, as the copy has not finished. */
        i_status_set_state(w_current, ENDMCOPY);
        o_undo_savestate(w_current, UNDO_ALL);
        break;

      case(SBOX):
        o_select_box_end(w_current, unsnapped_wx, unsnapped_wy);
        w_current->inside_action = FALSE;
        i_status_set_state(w_current, SELECT);
        break;

      case(ZOOMBOXEND):
        i_zoom_world_box_end(w_current, unsnapped_wx, unsnapped_wy);
        w_current->inside_action = FALSE;
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

        /* first look for grips */
        if (!o_grips_start(w_current, unsnapped_wx, unsnapped_wy)) {

          /* look for objects to select, TRUE = add to page selection */
          o_find_object(w_current, unsnapped_wx, unsnapped_wy, TRUE);
          w_current->event_state   = SELECT;
          w_current->inside_action = FALSE;
        }
        else {  /* a grip was found */
          w_current->event_state   = GRIPS;
          w_current->inside_action = TRUE;
        }
        break;

      case ENDPATH:
        w_x = snap_grid (w_current, unsnapped_wx);
        w_y = snap_grid (w_current, unsnapped_wy);
        if (o_path_end (w_current, w_x, w_y)) {
          w_current->event_state   = PATHCONT;
          w_current->inside_action = TRUE;
        } else {
          w_current->event_state   = DRAWPATH;
          w_current->inside_action = FALSE;
        }
        break;

      case STARTDND:
        w_current->dnd_state = NONE;
        if (w_current->drag_event) {
          gdk_event_free(w_current->drag_event);
          w_current->drag_event = NULL;
        }
        break;
    }

    if (w_current->render_adaptor == X11_ADAPTOR) {
      o_invalidate_all (w_current);
    }
  }
  else if (event->button == 2) {
    if (w_current->doing_pan) {
      w_current->doing_pan=FALSE;
      o_invalidate_all (w_current);
      if (w_current->undo_panzoom) {
        o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
      }
      /* this needs to be REDONE because if you mouse pan, you will
       * be thrown out of the current mode. not good */
      //w_current->inside_action = FALSE;
      //i_status_set_state(w_current, SELECT);
    }
    else if (w_current->inside_action) {
      if (w_current->event_state == ENDCOMP  ||
        w_current->event_state == ENDTEXT  ||
        w_current->event_state == ENDMOVE  ||
        w_current->event_state == ENDCOPY  ||
        w_current->event_state == ENDMCOPY ||
        w_current->event_state == ENDPASTE )
      {
        if (w_current->event_state == ENDMOVE) {
          o_move_invalidate_rubber (w_current, FALSE);
        }
        else {
          o_place_invalidate_rubber (w_current, FALSE);
        }
        w_current->rubber_visible = FALSE;

        o_place_rotate(w_current);

        if (w_current->event_state == ENDCOMP) {
          o_complex_place_changed_run_hook (w_current);
        }

        if (w_current->event_state == ENDMOVE) {
          o_move_invalidate_rubber (w_current, TRUE);
        }
        else {
          o_place_invalidate_rubber (w_current, TRUE);
        }
        w_current->rubber_visible = TRUE;
      }
    }
    else {

      switch(w_current->middle_button) {

        case(MOUSE_MIDDLE_ACTION):

          switch(w_current->event_state) {

            case(MOVE):
              o_move_end(w_current);
              w_current->inside_action = FALSE;
              i_status_set_state(w_current, SELECT);
              break;

            case(COPY):
              o_copy_end(w_current);
              w_current->inside_action = FALSE;
              i_status_set_state(w_current, SELECT);
              break;
          }
          break;

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
            /* this needs to be REDONE because if you mouse pan, you will
             * be thrown out of the current mode. not good */
            //w_current->inside_action = FALSE;
            //i_status_set_state(w_current, SELECT);
          }
          break;

        default:
          break;
      }
    }
  }
  else if (event->button == 3) {

    if (w_current->doing_pan) { /* just for ending a mouse pan */
      w_current->doing_pan=FALSE;
      o_invalidate_all (w_current);

      if (w_current->undo_panzoom) {
        o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
      }
      /* This needs to be REDONE, if user mouse pans, the user will be
       * thrown out of the current mode. Not good */
      //w_current->inside_action = FALSE;
      //i_status_set_state(w_current, SELECT);
    }
    else {
      w_current->inside_action = FALSE;
      i_status_set_state(w_current, SELECT);
    }
  }

#if DEBUG_EVENTS
  printf("%s: exit! %d \n", __func__, w_current->event_state);
#endif

  return(FALSE);
} /* End Function x_event_button_released*/

/*! \brief Updates GSCHEM TOPLEVEL when drawing area is configured.
 *  \par Function Description
 *  This is the callback function connected to the configure event of
 *  the drawing area of the main window.
 *
 *  It updates the size of the backingstore for the associated
 *  toplevel structure (creates a new pixmap) and re-pans each of its
 *  pages to keep their contents centered in the drawing area.
 *
 *  When the window is maximised, the zoom of every page is changed to
 *  best fit the previously displayed area of the page in the new
 *  area. Otherwise the current zoom level is left unchanged.
 *
 *  \param [in] widget    The drawing area which received the signal.
 *  \param [in] event     The event structure of signal configure-event.
 *  \param [in] w_current The toplevel environment as user data.
 *  \returns FALSE to propagate the event further.
 */
bool x_event_configure (GtkWidget         *widget,
                        GdkEventConfigure *event,
                        GschemToplevel    *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  GList *iter;
  GList *pages;
  Page  *old_page_current, *p_current;
  int    old_screen_width,  old_screen_height;
  int    new_screen_width,  new_screen_height;

  double relative_zoom_factor = 1.0;

  if (toplevel == NULL) {
    BUG_MSG ("toplevel == NULL");
    return FALSE;
  }

  /* if the current page has been setup */
  if (toplevel->page_current != NULL) {

    old_screen_width  = w_current->screen_width;
    old_screen_height = w_current->screen_height;
    new_screen_width  = event->width;
    new_screen_height = event->height;

    if (old_screen_width  == new_screen_width &&
        old_screen_height == new_screen_height)
    {
      /* the size of the drawing area has not changed */
      /* nothing to do here */
      return FALSE;
    }

    w_current->drawable = w_current->window;

    /* update the GschemToplevel with new size of drawing area */
    w_current->screen_width  = new_screen_width;
    w_current->screen_height = new_screen_height;

    /* in the case the user has maximised the window (hence the */
    /* configure event) fit the view by playing with zoom level */
    if (gdk_window_get_state (
       (gtk_widget_get_toplevel (
        widget))->window) & GDK_WINDOW_STATE_MAXIMIZED)
    {
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

    /* save current page */
    old_page_current = toplevel->page_current;

    pages = geda_list_get_glist(toplevel->pages);

    /* re-pan each page of the GedaToplevel */
    for (iter = pages; iter != NULL; iter  = iter->next) {

      double cx, cy;
      p_current = (Page*)iter->data;

      /* doing this the aspect ratio is kept when changing (hw)*/
      cx = ((double)(p_current->left + p_current->right))  / 2;
      cy = ((double)(p_current->top  + p_current->bottom)) / 2;
      s_page_goto (toplevel, p_current);
      i_pan_world_general (w_current, cx, cy, relative_zoom_factor, I_PAN_DONT_REDRAW);

    }

    /* restore current page to saved value */
    s_page_goto (toplevel, old_page_current);

    /* redraw the current page and update UI */
    o_invalidate_all (w_current);
    x_scrollbars_update (w_current);
  }
  return FALSE;
}

/*This function also raises any open dialogs to the foreground if the
 *toplevel raise_dialog_boxes is TRUE
 */
bool x_event_raise_dialogs(void *user_data)
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
  if (w_current->raise_dialog_boxes > 0)
  g_timeout_add (w_current->raise_dialog_boxes, x_event_raise_dialogs, w_current);
}

/*! \brief On event Expose
 *  \par Function Description
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
  fprintf (stderr, "x_event_expose, exposing %s <%p>\n", name, widget);
#endif

  if (w_current != NULL) {

    cairo_t      *save_cr;

    save_cr = w_current->cr;

    gdk_window_begin_paint_region(widget->window, event->region );

    w_current->cr = gdk_cairo_create( widget->window );

    cairo_set_antialias(w_current->cr, w_current->anti_aliasing);

    gdk_cairo_rectangle (w_current->cr, &(event->area));
    cairo_clip (w_current->cr);

    x_grid_repaint_background (w_current, &(event->area));

    x_grid_draw_grid_region (w_current, &(event->area));

    o_redraw_rectangle (w_current, &(event->area));

    gdk_window_end_paint(widget->window);

    cairo_destroy (w_current->cr);

    w_current->cr = save_cr;
  }

  return FALSE;
}

/*! \brief Get a snapped pointer position in world coordinates
 *
 *  \par Function Description
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

/*! \brief Callback to handle key events in the drawing area.
 *  \par Function Description
 * GTK+ callback function (registered in x_window_setup_draw_events() ) which
 * handles key press and release events from the GTK+ system.
 *
 * \param [in] widget the widget that generated the event
 * \param [in] event the event itself
 * \param w_current the toplevel environment
 * \returns TRUE if the event has been handled.
 */
bool x_event_key (GtkWidget      *widget,
                  GdkEventKey    *event,
                  GschemToplevel *w_current)
{
  bool retval      = FALSE;
  int  control_key = 0;
  int  shift_key   = 0;
  int  pressed;
  int  wx, wy;

#if DEBUG_EVENTS
  printf("x_event_key_pressed: Pressed key %i.\n", event->keyval);
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
      shift_key = 1;
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

  switch (w_current->event_state) {
    case ENDLINE:
      if (control_key) {
        x_event_get_snapped_pointer (w_current, &wx, &wy);
        o_line_motion (w_current, wx, wy);
      }
      break;
    case STARTDRAWNET:
      if (control_key) {
        x_event_get_snapped_pointer (w_current, &wx, &wy);
        o_net_start_magnetic(w_current, wx, wy);
      }
      break;
    case DRAWNET:
    case NETCONT:
      if (shift_key || control_key) {
        x_event_get_snapped_pointer (w_current, &wx, &wy);
        o_net_motion (w_current, wx, wy);
      }
      break;
    case DRAWBUS:
    case BUSCONT:
      if (control_key) {
        x_event_get_snapped_pointer (w_current, &wx, &wy);
        o_bus_motion (w_current, wx, wy);
      }
      break;
    case ENDMOVE:
      if (control_key) {
        x_event_get_snapped_pointer (w_current, &wx, &wy);
        o_move_motion (w_current, wx, wy);
      }
      break;
    case ENDCOMP:   /* FIXME: This state shouldn't respond to modifier keys */
    case ENDPASTE:  /* FIXME: This state shouldn't respond to modifier keys */
    case ENDTEXT:   /* FIXME: This state shouldn't respond to modifier keys */
    case ENDCOPY:
    case ENDMCOPY:
      if (control_key) {
        x_event_get_snapped_pointer (w_current, &wx, &wy);
        o_place_motion (w_current, wx, wy);
      }
      break;
  }

  if (pressed)
    retval = g_keys_execute (w_current, event) ? TRUE : FALSE;

  return retval;
}

/*! \brief Drawing Area Pointer Motion Callback Handler
 *  \par Function Description
 *  This function is call by the underling window context event
 *  dispatcher when ever the mouse pointer position changes.
 */
bool x_event_motion (GtkWidget      *widget,
                     GdkEventMotion *event,
                     GschemToplevel *w_current)
{
  int skip_event = 0;
  int unsnapped_wx, unsnapped_wy;
  int w_x, w_y;

  GdkEvent *test_event;

  g_return_val_if_fail ((w_current != NULL), 0);

  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK)    ? 1 : 0;

#if DEBUG_EVENTS
  /*  printf("MOTION!\n");*/
#endif

#ifdef HAVE_LIBSTROKE
  if (DOING_STROKE == TRUE) {
    x_stroke_record (w_current, event->x, event->y);
    return(0);
  }
#endif /* HAVE_LIBSTROKE */

  /* skip the moving event if there are other moving events in the
   * gdk event queue (Werner) but only skip the event if is the same
   * event and no buttons or modifier keys changed*/
  if ((test_event = gdk_event_get()) != NULL) {
    if (test_event->type == GDK_MOTION_NOTIFY
      && ((GdkEventMotion *) test_event)->state == event->state) {
        skip_event = 1;
      }
      gdk_event_put(test_event); /* put it back in front of the queue */
      gdk_event_free(test_event);
    if (skip_event == 1)
      return 0;
  }

  SCREENtoWORLD (w_current, (int) event->x, (int) event->y,
                                  &unsnapped_wx, &unsnapped_wy);

  w_x = snap_grid (w_current, unsnapped_wx);
  w_y = snap_grid (w_current, unsnapped_wy);

  /* If visible update the Coord Dialog */
  if (w_current->cowindow) {
    x_dialog_coord_update_display(w_current, (int) event->x, (int) event->y);
  }

  /* Update coordinates display on the status bar*/
  i_status_update_coordinates(w_current, w_x, w_y);

  //if (w_current->third_button == MOUSEPAN_ENABLED ||      w_current->middle_button == MOUSE_MIDDLE_PAN) {
    //if((w_current->event_state == MOUSEPAN) && w_current->inside_action) {
    if(w_current->doing_pan) {

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
  //}

  /* Huge switch statement to evaluate state transitions. Jump to end_motion
   * label to escape the state evaluation rather than returning from the
   * function directly. */
  switch(w_current->event_state) {

    case(SELECT):
      /* do nothing */
      break;

    case(GRIPS):
      o_grips_motion(w_current, w_x, w_y);
      break;

    case(STARTSELECT):
      if ( (!w_current->drag_can_move) || (w_current->drag_can_move &&
         (!o_find_selected_object(w_current, w_current->first_wx, w_current->first_wy))))
      {
        if (o_select_box_start(w_current, unsnapped_wx, unsnapped_wy)) {
          w_current->event_state = SBOX;
          w_current->inside_action = 1;
        }
        break;
      }
      else {
        /* If the shift or control keys are pressed, that means the user
         * definitely wants to drag out a selection box.  Otherwise, if
         * there is not a selected object under the cursor, look for one
         * that could be selected and start moving it.
         */
        if (w_current->SHIFTKEY ||
            w_current->CONTROLKEY ||
           (!o_find_selected_object(w_current, w_current->first_wx, w_current->first_wy) &&
           (!o_find_object(w_current, w_current->first_wx, w_current->first_wy, TRUE) ||
            !o_select_is_selection(w_current)))
           )
        {
          if (o_select_box_start(w_current, unsnapped_wx, unsnapped_wy)) {
            w_current->event_state = SBOX;
            w_current->inside_action = 1;
          }
          break;
        }
        else
        {
          /* Start moving the selected object(s) */
          o_move_start(w_current, w_x, w_y);
          w_current->event_state = ENDMOVE;
          w_current->inside_action = 1;
          if (w_current->drag_event) {
            gdk_event_free(w_current->drag_event);
          }
          w_current->drag_event = gdk_event_copy( (GdkEvent*)event);
          /* Fall through bottom of case to finish the move */
        }
      }
      /* Fall through to handle move */
      case(ENDMOVE):
      case(MOVE):
        if (w_current->inside_action) {
          o_move_motion (w_current, w_x, w_y);
        }
        break;

      case(ENDLINE):
        if (w_current->inside_action)
          o_line_motion (w_current, w_x, w_y);
        break;

      case PATHCONT:
      case ENDPATH:
        if (w_current->inside_action)
          o_path_motion (w_current, w_x, w_y);
        break;
      case(ENDBOX):
        if (w_current->inside_action)
          o_box_motion ( w_current, w_x, w_y);
        break;

      case(ENDPICTURE):
        if (w_current->inside_action)
          o_picture_motion ( w_current, w_x, w_y);
        break;

      case(ENDCIRCLE):
        if (w_current->inside_action)
          o_circle_motion (w_current, w_x, w_y);
        break;

      case(ENDARC):
        if (w_current->inside_action) {
          w_current->which_grip = ARC_RADIUS;
          o_arc_motion (w_current, w_x, w_y);
        }
        break;

      case(STARTDRAWNET):
        if(w_current->magnetic_net_mode == 1) {
          o_net_start_magnetic(w_current, w_x, w_y);
        }
        break;

      case(DRAWNET):
      case(NETCONT):
        if (w_current->inside_action)
          o_net_motion (w_current, w_x, w_y);
        break;

      case(DRAWBUS):
      case(BUSCONT):
        if (w_current->inside_action)
          o_bus_motion (w_current, w_x, w_y);
        break;

      case(ENDPIN):
        if (w_current->inside_action)
          o_pin_motion (w_current, w_x, w_y);
        break;

      case(COPY):
      case(MCOPY):
      case(ENDCOPY):
      case(ENDMCOPY):
      case(ENDCOMP):
      case(ENDPASTE):
      case(ENDTEXT):
        o_place_motion (w_current, w_x, w_y);
        break;

      case(SBOX):
        if (w_current->inside_action)
          o_select_box_motion (w_current, unsnapped_wx, unsnapped_wy);
        break;

      case(ZOOMBOXEND):
        if (w_current->inside_action)
          i_zoom_world_box_motion (w_current, unsnapped_wx, unsnapped_wy);
        break;

  }

  return(0);
}

/*! \brief Callback for Window Scroll Events.
 *  \par Function Description
 * This function is called with the drawing window receives a scroll
 * event signal. Typically the signal originated from a pointing device.
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_event_hschanged (GtkAdjustment *adjust, GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int current_left;
  int new_left;

  g_return_if_fail (w_current != NULL);

  if (w_current->scrollbars) {
    current_left = toplevel->page_current->left;
    new_left     = (int) adjust->value;

    toplevel->page_current->left = new_left;
    toplevel->page_current->right =
    toplevel->page_current->right -
    (current_left - new_left);

    o_invalidate_all (w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_event_vschanged (GtkAdjustment *adjust, GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int current_bottom;
  int new_bottom;

  g_return_if_fail (w_current != NULL);

  if (w_current->scrollbars) {
    current_bottom = toplevel->page_current->bottom;
    new_bottom     = w_current->world_bottom - (int) adjust->value;

    toplevel->page_current->bottom = new_bottom;
    toplevel->page_current->top =
    toplevel->page_current->top -
    (current_bottom - new_bottom);

#if DEBUG_EVENTS
    printf("vrange %f %f\n",  adjust->lower, adjust->upper);
    printf("vvalue %f\n",     adjust->value);
    printf("actual: %d %d\n", toplevel->page_current->top,
                              toplevel->page_current->bottom);
#endif

    o_invalidate_all (w_current);
  }
}
