/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_callbacks.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#include <geda_debug.h>

#define I_DO_DECLARE
#include <i_command.h>

#include <geda_debug.h>

/*! \brief */
#define DELIMITERS ", "

/*! \def DEFINE_I_CALLBACK
 * Every i_callback functions have the same footprint
 */
#define DEFINE_I_CALLBACK(name) void i_callback_ ## name I_CALLBACK_ARGUMENTS

/*! \section callback-intro Callback Functions
 *
 * Don't use the widget parameter in these callback function, or do some
 * checking since these routines are usually called with widget = NULL,
 * data = 0 (will be w_current hack)
 */

/*! \brief Zoom Pan initiated by Keyboard Hotkey
 *  \par Function Description
 *  This is a callback function for the Zoom Pan Hotkey action.
 */
DEFINE_I_CALLBACK(view_pan_hotkey)
{
  if (w_current != NULL) {

    int wx, wy;

    if (!i_window_get_pointer_position(w_current, FALSE, &wx, &wy))
      return;

    i_pan_world(w_current, wx, wy);

    if (w_current->undo_panzoom) {
      o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
    }
  }
}

/*! \brief Callback function that moves the viewport to the left.
 *  \par Function Description
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_left)
{
  if (w_current != NULL) {
    i_pan_world_mouse(w_current, w_current->keyboardpan_gain, 0);
  }
}

/*! \brief Callback function that moves the viewport to the right.
 *  \par Function Description
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_right)
{
  if (w_current != NULL) {
    /* yes, that's a negative sign there */
    i_pan_world_mouse(w_current, -w_current->keyboardpan_gain, 0);
  }

}

/*! \brief Callback function that moves the viewport up.
 *  \par Function Description
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_up)
{
  if (w_current != NULL) {
    i_pan_world_mouse(w_current, 0, w_current->keyboardpan_gain);
  }
}

/*! \brief Callback function that moves the viewport down.
 *  \par Function Description
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_down)
{
  if (w_current != NULL) {
    /* yes, that's a negative sign there */
    i_pan_world_mouse(w_current, 0, -w_current->keyboardpan_gain);
  }
}

/*! \brief Cancel Everthing
 *  \par Function Description
 *
 *  \note Do NOT use the widget parameter in this one since it is being
 *        called with a null.
 */
DEFINE_I_CALLBACK(cancel)
{
  switch (w_current->event_state) {
    case COMPMODE:

      if (w_current->cswindow) {

        /* user hit escape key when placing components */

        /* Undraw any outline of the place list */
        o_place_invalidate_rubber (w_current, FALSE);
        w_current->rubber_visible = FALSE;

        /* De-select the lists in the component selector */
        x_compselect_deselect (w_current);

        /* Present the component selector again */
        g_object_set (w_current->cswindow, "hidden", FALSE, NULL);
      }
      break;

    case MOVEMODE:
    case DRAGMOVE:

      if (w_current->inside_action) {

        /* If we're cancelling while inside a move action, free the place
         * list and destroy the stretch_list */
          o_move_cancel (w_current);
      }
      break;

    case GRIPS:
      /* If we're cancelling from a grip action, call the specific cancel
       * routine to reset the visibility of the object being modified */
      o_grips_cancel (w_current);

    case SELECT:
    case STARTSELECT:
      o_select_cancel_events(w_current);
      break;

    default:
      break;

  }

  if (w_current->primary_selection) {
    g_list_free (w_current->primary_selection);
    w_current->primary_selection = NULL;
  }

  if (o_select_is_selection (w_current)) {
    o_select_unselect_all (w_current);
  }

  /* Check and release objects in the toplevel place list */
  geda_struct_place_free_place_list(w_current->toplevel);

  /* Clear the key guile command-sequence */
  g_keys_reset (w_current);

  /* Reset the action event handler */
  i_event_cancel_action_handler (w_current);
}
