/* -*- C o_place.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_place.c
 * \brief Low-level module for placing objects
 */
#include <gschem.h>
#include <geda_debug.h>

/*! \brief Start process to place objects
 *  \par Function Description
 *  This function starts the process of interactively placing objects.
 *  The objects may be objects being copied or objects recieved from
 *  the clibboard or from drag-and-drop. Temporary outline maybe used
 *  during the process.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
bool o_place_start (GschemToplevel *w_current, int w_x, int w_y)
{
  bool result;

  if (Current_Page->place_list) {

    int count = g_list_length(Current_Page->place_list);

    if (count > 0) {

#if DEBUG || DEBUG_DND_EVENTS || DEBUG_PASTE || DEBUG_PLACE
    printf("%s: place_list has %d objects\n", __func__, count);
#endif

      w_current->second_wx = w_x;
      w_current->second_wy = w_y;

      o_place_invalidate_rubber (w_current, TRUE);
      w_current->rubber_visible = TRUE;
      result = TRUE;
    }
    else {
      u_log_message (_("Buffer is empty, nothing to place\n"));
      w_current->inside_action = FALSE;
      i_status_set_state(w_current, SELECT);
      result = FALSE;
    }
  }
  else {
    i_status_set_state(w_current, SELECT);
    result = FALSE;
  }
  return w_current->inside_action = result;
}

/*! \brief Finalize objects being Placed
 *
 *  \par Function Description
 *   Handles x_event_button_pressed for ENDCOMP, ENDPASTE ENDTEXT events,
 *   and o_copy_end, o_copy_multiple_end and x_dnd_receive_string_sym.
 */
void
o_place_end (GschemToplevel *w_current, int w_x, int w_y,
             int continue_placing, GList **ret_new_objects, const char* hook_name)
{
  GedaToplevel *toplevel       = w_current->toplevel;
  GList        *object_list    = NULL;
  GList        *connected_list = NULL;
  GList        *iter;
  Object       *o_current;
  Page         *p_current;

  int w_diff_x, w_diff_y;

  if (w_current->inside_action) {

    /* Turn off flag */
    w_current->rubber_visible = FALSE;

    /* Calc final object positions */
    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    w_diff_x = w_current->second_wx - w_current->first_wx;
    w_diff_y = w_current->second_wy - w_current->first_wy;

    if (continue_placing) {
      /* Make a copy of the place list if we want to keep it afterwards */
      object_list = o_list_copy_all (Current_PlaceList, object_list);
    }
    else {
      /* Otherwise just take it */
      object_list = Current_PlaceList;
      Current_PlaceList = NULL;
    }

    if (ret_new_objects != NULL) {
      *ret_new_objects = g_list_copy (object_list);
    }

    o_list_translate(object_list, w_diff_x, w_diff_y);

    /* Attach each item onto the page's object list. Update object
     * connectivity and add the new objects to the selection list.*/
    p_current = toplevel->page_current;

    for (iter = object_list; iter != NULL; NEXT(iter)) {

      o_current = iter->data;                      /* Get pointer to object */

      o_current->page = NULL;                      /* Remove old references */

      s_page_append_object (p_current, o_current); /* Append to current page */

      s_conn_update_object (o_current);            /* Update connectivity */

      connected_list = s_conn_return_others (connected_list, o_current);
    }

    if (hook_name != NULL) {
      g_run_hook_object_list (w_current, hook_name, object_list);
    }

    o_invalidate_glist (w_current, connected_list);
    g_list_free (connected_list);
    connected_list = NULL;

    o_invalidate_glist (w_current, object_list);  /* only redraw new objects */
    g_list_free (object_list);

    o_undo_savestate (w_current, UNDO_ALL);
    i_status_update_sensitivities (w_current);
    w_current->inside_action = continue_placing;
  }
  else {
    BUG_MSG("Not inside an action!");
  }
}

/*! \brief Handle Erasing and Redrawing of rubber outlines for objects
 *
 *  \par Function Description
 *  This function handles motion events for rubber outlines when placing
 *  objects.
 */
void o_place_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->inside_action) {
    if (w_current->rubber_visible) {
      o_place_invalidate_rubber (w_current, FALSE);
    }
    w_current->second_wx = w_x;
    w_current->second_wy = w_y;
    o_place_invalidate_rubber (w_current, TRUE);
    w_current->rubber_visible = 1;
  }
}

/*! \brief Invalidate bounding box or outline for Object placement
 *
 *  \par Function Description
 *  This function invalidates the bounding box where objects would be
 *  drawn by o_place_draw_rubber()
 *
 * The function applies manhatten mode constraints to the coordinates
 * before drawing if the CONTROL key is recording as being pressed in
 * the w_current structure.
 *
 * The "drawing" parameter is used to indicate if this drawing should
 * immediately use the selected feedback mode and positioning constraints.
 *
 * With drawing=TRUE, the selected conditions are used immediately,
 * otherwise the conditions from the last drawing operation are used,
 * saving the new state for next time.
 *
 * This function should be called with drawing=TRUE when starting a
 * rubberbanding operation and when otherwise refreshing the rubberbanded
 * outline (e.g. after a screen redraw). For any undraw operation, should
 * be called with drawing=FALSE, ensuring that the undraw XOR matches the
 * mode and constraints of the corresponding "draw" operation.
 *
 * If any mode / constraint changes are made between a undraw, redraw XOR
 * pair, the latter (draw) operation must be called with drawing=TRUE. If
 * no mode / constraint changes were made between the pair, it is not
 * harmful to call the draw operation with "drawing=FALSE".
 *
 *  \param [in] w_current   GschemToplevel which we're drawing for.
 *  \param [in] drawing     Set to FALSE for undraw operations to ensure
 *                            matching conditions to a previous draw operation.
 */
void o_place_invalidate_rubber (GschemToplevel *w_current, int drawing)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int diff_x, diff_y;
  int left,     top,   bottom,   right;
  int s_left, s_top, s_bottom, s_right;

  if (toplevel->page_current->place_list != NULL) {

    /* If drawing is true, then don't worry about the previous drawing
     * method and movement constraints, use with the current settings */
    if (drawing) {
      /* Ensure we set this to flag there is "something" supposed to be
       * drawn when the invaliate call below causes an expose event. */
      w_current->last_drawb_mode = w_current->action_feedback_mode;
      w_current->drawbounding_action_mode = (w_current->CONTROLKEY)
      ? CONSTRAINED : FREE;
    }

    /* Calculate delta of X-Y positions from buffer's origin */
    diff_x = w_current->second_wx - w_current->first_wx;
    diff_y = w_current->second_wy - w_current->first_wy;

    /* Adjust the coordinates according to the movement constraints */

    /* Need to update the w_current->{first,second}_w{x,y} coords even
     * though we're only invalidating because the move rubberband code
     * (which may execute right after this function) expects these
     * coordinates to be correct.
     */
    if (w_current->drawbounding_action_mode == CONSTRAINED) {
      if (abs (diff_x) >= abs (diff_y)) {
        w_current->second_wy = w_current->first_wy;
        diff_y = 0;
      }
      else {
        w_current->second_wx = w_current->first_wx;
        diff_x = 0;
      }
    }

    /* Get bounds of the drawing to be done */
    if (o_get_bounds_list (Current_PlaceList, &left, &top, &right, &bottom))
    {

      WORLDtoSCREEN (w_current, left  + diff_x, top    + diff_y, &s_left, &s_top);
      WORLDtoSCREEN (w_current, right + diff_x, bottom + diff_y, &s_right, &s_bottom);

      o_invalidate_rectangle (w_current, s_left, s_top, s_right, s_bottom);
    }
    else {
      BUG_TRACE("Error No bounds");
    }
  }
  else {
    BUG_MSG("page_current->place_list is NULL");
  }
}

/*! \brief Draw a bounding box or outline for Object placement
 *
 *  \par Function Description
 *  This function draws either the Object in the place list
 *  or a rectangle around their bounding box, depending upon the
 *  currently selected w_current->action_feedback_mode. This takes the
 *  value BOUNDINGBOX or OUTLINE.
 *
 * The function applies manhatten mode constraints to the coordinates
 * before drawing if the CONTROL key is recording as being pressed in
 * the w_current structure.
 *
 * The "drawing" parameter is used to indicate if this drawing should
 * immediately use the selected feedback mode and positioning constraints.
 *
 * With drawing=TRUE, the selected conditions are used immediately,
 * otherwise the conditions from the last drawing operation are used,
 * saving the new state for next time.
 *
 * This function should be called with drawing=TRUE when starting a
 * rubberbanding operation and when otherwise refreshing the rubberbanded
 * outline (e.g. after a screen redraw). For any undraw operation, should
 * be called with drawing=FALSE, ensuring that the undraw XOR matches the
 * mode and constraints of the corresponding "draw" operation.
 *
 * If any mode / constraint changes are made between a undraw, redraw XOR
 * pair, the latter (draw) operation must be called with drawing=TRUE. If
 * no mode / constraint changes were made between the pair, it is not
 * harmful to call the draw operation with "drawing=FALSE".
 *
 *  \param [in] w_current   GschemToplevel which we're drawing for.
 *  \param [in] drawing     Set to FALSE for undraw operations to ensure
 *                            matching conditions to a previous draw operation.
 */
void o_place_draw_rubber (GschemToplevel *w_current, int drawing)
{
  GedaToplevel *toplevel = w_current->toplevel;
  cairo_t *cr = eda_renderer_get_cairo_context (CairoRenderer);
  int diff_x, diff_y;

  g_return_if_fail (toplevel->page_current->place_list != NULL);

  /* If drawing is true, then don't worry about the previous drawing
   * method and movement constraints, use with the current settings */
  if (drawing) {
    w_current->last_drawb_mode = w_current->action_feedback_mode;
    w_current->drawbounding_action_mode = (w_current->CONTROLKEY)
                                            ? CONSTRAINED : FREE;
  }

  /* Calculate delta of X-Y positions from buffer's origin */
  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  /* Adjust the coordinates according to the movement constraints */
  if (w_current->drawbounding_action_mode == CONSTRAINED ) {
    if (abs(diff_x) >= abs(diff_y)) {
      w_current->second_wy = w_current->first_wy;
      diff_y = 0;
    }
    else {
      w_current->second_wx = w_current->first_wx;
      diff_x = 0;
    }
  }

  /* Translate the cairo context to the required offset before drawing. */
  cairo_save (cr);
  cairo_translate (cr, diff_x, diff_y);

  /* Draw with the appropriate mode */
  if (w_current->last_drawb_mode == BOUNDINGBOX) {

    GArray *map = eda_renderer_get_color_map (CairoRenderer);
    int flags   = eda_renderer_get_cairo_flags (CairoRenderer);

    int left, top, bottom, right;

    /* Find the bounds of the drawing to be done */
    o_get_bounds_list (Current_PlaceList, &left, &top, &right, &bottom);

    /* Draw box outline */
    eda_cairo_box (cr, flags, 0, left, top, right, bottom);

    eda_cairo_set_source_color (cr, BOUNDINGBOX_COLOR, map);

    eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, 0, -1, -1);
  }
  else {
    GList *iter;
    for (iter = Place_List; iter != NULL; NEXT(iter)) {
      eda_renderer_draw (CairoRenderer, (Object *) iter->data);
    }
  }
  cairo_restore (cr);
}

/*! \brief Mirror the objects being placed
 *
 *  \par Function Description
 *  This function erases the objects in the place list, mirrors
 *  them, runs %mirror-objects-hook, and redraws the objects after
 *  mirroring.
 *
 *  \param [in] w_current   The GschemToplevel object.
 */
void o_place_mirror (GschemToplevel *w_current)
{
  GList *list;

  if ((list = Current_PlaceList)) {

    int wx = w_current->first_wx;
    int wy = w_current->first_wy;

    o_place_invalidate_rubber (w_current, FALSE);

    o_list_mirror(list, wx, wy);

    /* Run mirror-objects-hook */
    g_run_hook_object_list (w_current, "%mirror-objects-hook", list);

    o_place_invalidate_rubber (w_current, TRUE);

  }
}

/*! \brief Rotate objects being placed
 *
 *  \par Function Description
 *   Passes list of objects being placed to o_list_rotate
 *   with and angle of 90.
 */
void o_place_rotate (GschemToplevel *w_current)
{
  GList *list;

  if ((list = Current_PlaceList)) {

    int wx = w_current->first_wx;
    int wy = w_current->first_wy;

    o_place_invalidate_rubber (w_current, FALSE);
    o_list_rotate (list, wx, wy, 90);

    /* Run rotate-objects-hook */
    g_run_hook_object_list (w_current, "%rotate-objects-hook", list);

    o_place_invalidate_rubber (w_current, TRUE);

  }
}
