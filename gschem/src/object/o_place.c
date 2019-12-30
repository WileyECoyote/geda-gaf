/* -*- C o_place.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2017 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
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
/** \defgroup Place-Operations Place Operations
 *  @{
 *  \ingroup Editing-Operations
 *  \par This group contains routines for Placing Operations.
 *  \image html buffers.png
 *  \image latex buffers.png
 */

#include <gschem.h>
#include <geda_debug.h>

/*!
 * \brief Start process to place objects
 * \par Function Description
 *  This function starts the process of interactively placing objects.
 *  The objects may be objects being copied or objects received from
 *  the clibboard or from drag-and-drop. Temporary outline maybe used
 *  during the process.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        Current x coordinate of pointer in world units.
 * \param [in] w_y        Current y coordinate of pointer in world units.
 */
bool o_place_start (GschemToplevel *w_current, int w_x, int w_y)
{
  bool result;

  GList *list = geda_struct_place_get_place_list(w_current->toplevel);

  if (list) {

    int count = g_list_length(list);

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
      geda_log (_("Buffer is empty, nothing to place\n"));
      i_status_set_state(w_current, SELECT);
      result = FALSE;
    }
  }
  else {
    i_status_set_state(w_current, SELECT);
    result = FALSE;
  }

  i_status_update_action_state(w_current, result);

  return result;
}

/*!
 * \brief Finalize objects being Placed
 * \par Function Description
 *  Handles x_event_button_pressed for ENDCOMP, ENDPASTE ENDTEXT events,
 *  and o_copy_end, o_copy_multiple_end and x_dnd_receive_string_sym.
 */
void
o_place_end (GschemToplevel *w_current, int continue_placing, GList **ret_new_objects, Hooker id)
{
  if (w_current->inside_action) {

    GList *object_list;
    GList *connected_list = NULL;
    GList *iter;
    GList *place_list;
    Page  *p_current;
    int    w_diff_x, w_diff_y;

    /* Turn off flag */
    w_current->rubber_visible = FALSE;

    /* Calculate final object positions */
    w_diff_x = w_current->second_wx - w_current->first_wx;
    w_diff_y = w_current->second_wy - w_current->first_wy;

    if (w_current->CONTROLKEY) {
      if (abs (w_diff_x) >= abs (w_diff_y)) {
        w_diff_y = 0;
      }
      else {
        w_diff_x = 0;
      }
    }

    p_current  = gschem_toplevel_get_current_page (w_current);
    place_list = geda_page_get_place_list(p_current);

    if (continue_placing) {
      /* Make a copy of the place list if we want to keep it afterwards */
      object_list = geda_copy_list (place_list, NULL);
    }
    else {
      /* Otherwise just take it */
      object_list = place_list;
      geda_page_set_place_list(p_current, NULL);
    }

    if (ret_new_objects != NULL) {
      *ret_new_objects = g_list_copy (object_list);
    }

    /* Attach each item onto the page's object list. Update object
     * connectivity and add the new objects to the selection list.*/
    for (iter = object_list; iter != NULL; NEXT(iter)) {

      GedaObject *o_current = iter->data;              /* Get pointer to object */

      geda_object_translate(o_current, w_diff_x, w_diff_y);

      o_current->page = NULL;                      /* Remove old references */

      geda_struct_page_append_object (p_current, o_current); /* Append to current page */

      geda_struct_conn_update_object (o_current);            /* Update connectivity */

      connected_list = geda_struct_conn_return_others (connected_list, o_current);
    }

    if (id != INVALID_HOOK) {

      /* Check if a complex is being included piecewise, in which case
       * the add-default-pin-attributes maybe ran on each pin and this
       * will reset pin labels to "unknown", see bug #1091071 */
      if (w_current->include_complex) {

        GList *hook_list = NULL;

        /* Create a new list without pins */
        for (iter = object_list; iter != NULL; NEXT(iter)) {

          GedaObject *o_current = iter->data;

          if (o_current->type != OBJ_PIN) {
            hook_list = g_list_append(hook_list, o_current);
          }
        }

        g_hook_run_object_list (w_current, id, hook_list);
        g_list_free (hook_list);

      }
      else {
        g_hook_run_object_list (w_current, id, object_list);
      }
    }

    o_invalidate_list (w_current, connected_list);
    o_invalidate_list (w_current, object_list);  /* only redraw new objects */

    i_status_update_action_state(w_current, continue_placing);
    i_status_update_sensitivities (w_current);

    g_list_free (connected_list);
    g_list_free (object_list);
  }
  else {
    BUG_MSG("Not inside an action!");
  }
}

/*!
 * \brief Handle Erasing and Redrawing of rubber outlines for objects
 * \par Function Description
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
    w_current->rubber_visible = TRUE;
  }
}

/*!
 * \brief Invalidate bounding box or outline for Object placement
 * \par Function Description
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
 *  \param [in] w_current   GschemToplevel which we are drawing for.
 *  \param [in] drawing     Set to FALSE for undraw operations to ensure
 *                            matching conditions to a previous draw operation.
 */
void o_place_invalidate_rubber (GschemToplevel *w_current, int drawing)
{
  GedaToplevel *toplevel   = w_current->toplevel;
  GList        *place_list = geda_page_get_place_list(toplevel->page_current);

  int left,     top,   bottom,   right;
  int s_left, s_top, s_bottom, s_right;

  if (place_list != NULL) {

    int diff_x, diff_y;

    /* If drawing is true, then do not worry about the previous drawing
     * method and movement constraints, use with the current settings */
    if (drawing) {
      /* Ensure we set this to flag there is "something" supposed to be
       * drawn when the invaliate call below causes an expose event. */
      w_current->last_drawb_mode = w_current->action_feedback_mode;
      w_current->drawbounding_action_mode = (w_current->CONTROLKEY) ? CONSTRAINED : FREE;
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
    if (geda_object_get_bounds_list (place_list, &left, &top, &right, &bottom))
    {
      WORLDtoSCREEN (w_current, left  + diff_x, top    + diff_y, &s_left, &s_top);
      WORLDtoSCREEN (w_current, right + diff_x, bottom + diff_y, &s_right, &s_bottom);

      o_invalidate_rectangle (w_current, s_left, s_top, s_right, s_bottom);
    }
    else {

      BUG_TRACE("Error No bounds");

#if DEBUG

      if (place_list != NULL) {
        GList *iter;
        int len = g_list_length(place_list);
        fprintf(stderr, "place_list=%p contains <%d> objects", place_list, len);

        if (len > 0) {
          for (iter = place_list; iter != NULL; NEXT(iter)) {
            GedaObject *o_current = iter->data;  /* Get pointer to object */
            geda_utility_print_object(o_current);
          }
        }
      }
      else {
        BUG_MSG("place list is NULL");
      }

#endif

    }
  }
}

/*!
 * \brief Draw a bounding box or outline for Object placement
 * \par Function Description
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
 * \param [in] w_current   GschemToplevel which we're drawing for.
 * \param [in] drawing     Set to FALSE for undraw operations to ensure
 *                         matching conditions to a previous draw operation.
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
    geda_object_get_bounds_list (Current_PlaceList, &left, &top, &right, &bottom);

    /* Draw box outline */
    eda_cairo_box (cr, flags, 0, left, top, right, bottom);

    eda_cairo_set_source_color (cr, BOUNDINGBOX_COLOR, map);

    eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, 0, -1, -1);
  }
  else {
    GList *iter;
    for (iter = Place_List; iter != NULL; NEXT(iter)) {
      eda_renderer_draw (CairoRenderer, (GedaObject*) iter->data);
    }
  }
  cairo_restore (cr);
}

/*!
 * \brief Mirror the objects being placed
 * \par Function Description
 *  This function erases the objects in the place list, mirrors
 *  them, runs %mirror-objects-hook, and redraws the objects after
 *  mirroring.
 *
 * \param [in] w_current   The GschemToplevel object.
 */
void o_place_mirror (GschemToplevel *w_current)
{
  GList *list;

  if ((list = Current_PlaceList)) {

    int wx = w_current->first_wx;
    int wy = w_current->first_wy;

    o_place_invalidate_rubber (w_current, FALSE);

    geda_object_list_mirror(list, wx, wy);

    /* Run mirror-objects-hook */
    g_hook_run_object_list (w_current, MIRROR_OBJECTS_HOOK, list);

    o_place_invalidate_rubber (w_current, TRUE);

  }
}

/*!
 * \brief Rotate objects being placed
 * \par Function Description
 *  Passes list of objects being placed to geda_rotate_list
 *  with and angle of 90.
 */
void o_place_rotate (GschemToplevel *w_current, int angle)
{
  GList *list;

  if ((list = Current_PlaceList)) {

    int wx = w_current->first_wx;
    int wy = w_current->first_wy;

    o_place_invalidate_rubber (w_current, FALSE);

    geda_rotate_list (list, wx, wy, angle);

    if (w_current->ALTKEY) {

      GList *alist;

      alist = geda_object_get_objects_by_type(list, OBJ_TEXT);

      if (alist) {

        int text_angle = -1 * angle;

        geda_rotate_list (alist, 0, 0, text_angle);

        g_list_free(alist);
      }
    }

    /* Run rotate-objects-hook */
    g_hook_run_object_list (w_current, ROTATE_OBJECTS_HOOK, list);

    o_place_invalidate_rubber (w_current, TRUE);
  }
}

/** @} endgroup Place-Operations */
