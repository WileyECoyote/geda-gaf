/* -*- C o_move.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_move.c
 * \brief Low-level module for moving objects
 */
/** \defgroup Move-Operations Move Operations
 *  @{
 *  \ingroup Editing-Operations
 *
 *  \par This group contains routines for Moving Objects.
 */

#include <gschem.h>
#include <geda_debug.h>

/** \defgroup Stretch Stretch While Dragging during Move Operations
 *  @{
 */

/*!
 * \brief Add object to Stretch "Semi-Select" List
 * \ingroup Stretch
 * \par Function Description
 *  Used to pickup pins nets, and bus enpoints when parent objects
 *  are moved. Adds \a object to \a list if \a object is not already
 *  in \a list
 */
static GList *o_move_stretch_add (GList *list, GedaObject *object, int whichone)
{
  GList   *s_iter;
  STRETCH *s_new;

  /* Check if the object is already in the stretch list */
  for (s_iter = list; s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *s_current = s_iter->data;
    if (s_current->object->sid == object->sid) {
      return list;
    }
  }

  s_new = GEDA_MEM_ALLOC (sizeof(STRETCH));
  s_new->object = object;
  s_new->whichone = whichone;

  return g_list_append (list, s_new);
}


/*!
 * \brief Test if a STRETCH structure points at a given Object
 * \ingroup Stretch
 * \par Function Description
 *  Compares if (STRETCH *)a->object == (GedaObject*)b
 *
 * \param [in] a  The STRETCH structure
 * \param [in] b  The Object to test for
 * \returns 0 if STRETCH *a points toGedaObject *b, otherwise 1.
 */
static int find_object (const void *a, const void *b)
{
  return (((STRETCH*)a)->object == (GedaObject*)b) ? 0 : 1;
}


/*!
 * \brief Remove object from Stretch "Semi-Select" List
 * \ingroup Stretch
 * \par Function Description
 */
static GList *o_move_stretch_remove (GList *list, GedaObject *object)
{
  GList *item;

  g_return_val_if_fail (object != NULL, list);

  item = g_list_find_custom (list, object, find_object);
  GEDA_FREE (item->data);

  return g_list_delete_link (list, item);
}

#if DEBUG_STRETCH

/*!
 * \brief Debug utility function for Stretch routines
 * \ingroup Stretch
 * \par Function Description
 *  Prints name of members to the standard out.
 */
static void o_move_stretch_print_all (GList *list)
{
  GList *iter;

  printf("START printing stretch ********************\n");
  for (iter = list; iter != NULL; iter = g_list_next (iter)) {
    STRETCH *s_current = iter->data;

    if (s_current->object) {
      printf("Object: %s\n", s_current->object->name);
    } else {
      printf("Object is NULL\n");
    }

    printf("which one: %d\n", s_current->whichone);
  }
  printf("DONE printing stretch ********************\n\n");
}

#endif

/*!
 * \brief Free Stretch Allocations
 * \ingroup Stretch
 * \par Function Description
 *  This function is used to free the memory associated with
 *  the scretch routines.
 */
static void o_move_stretch_destroy_all (GList *list)
{
  geda_glist_free_full (list, g_free);
}

/** @} END Group Stretch */

/*!
 * \brief Low-Level Move Object
 * \par Function Description
 *  This is calls geda_object_translate to reposition the object
 *  (diff_x, diff_y) from the current location. If object is an
 *  electrical object any connection re first remove and then
 *  reconnected after the object is moved.
 */
void o_move_end_lowlevel (GschemToplevel *w_current,
                          GedaObject     *object,
                          int diff_x, int diff_y)
{
  switch (object->type) {

    case (OBJ_NET):
    case (OBJ_BUS):
    case (OBJ_PIN):
      geda_struct_conn_remove_object (object);
      geda_object_translate (object, diff_x, diff_y);
      geda_struct_conn_update_linear_object(object);
      break;

    default:
      geda_object_translate (object, diff_x, diff_y);
      break;
  }
}

/*! \brief Finalize Move Operation
 *  \par Function Description
 *
 */
void o_move_end(GschemToplevel *w_current)
{
  GedaObject *object = o_select_return_first_object(w_current);

  if (!object) {
    /* This is an error condition hack */
    i_status_set_state(w_current, SELECT);
  }
  else {

    GedaToplevel *toplevel = w_current->toplevel;

    GedaObject *sub_object;
    GList      *iter;
    GList      *selection_iter;
    GList      *selection_list;
    GList      *rubbernet_objects = NULL;
    int         diff_x, diff_y;

    diff_x = w_current->second_wx - w_current->first_wx;
    diff_y = w_current->second_wy - w_current->first_wy;

    o_move_invalidate_rubber (w_current, FALSE);
    w_current->rubber_visible = FALSE;

    if (w_current->CONTROLKEY) {
      if (abs (diff_x) >= abs (diff_y)) {
        diff_y = 0;
      }
      else {
        diff_x = 0;
      }
    }

    if (w_current->netconn_rubberband) {
      o_move_end_rubberband (w_current, diff_x, diff_y, &rubbernet_objects);
    }

    /* Unset the dont_redraw flag on rubberbanded objects.
     * We set this above, in o_move_start(). */
    for (iter = w_current->stretch_list; iter != NULL; NEXT(iter)) {
      STRETCH *stretch = iter->data;
      stretch->object->dont_redraw = FALSE;
    }

    selection_iter = selection_list = geda_toplevel_struct_get_selection(toplevel);

    while (selection_iter != NULL) {

      object = (GedaObject*) selection_iter->data;

      if (object == NULL) {
        BUG_MSG("NULL object!");
        return;
      }

      if (GEDA_IS_COMPLEX(object)) {

        /* From geda_complex_object_translate */
        object->complex->x = object->complex->x + diff_x;
        object->complex->y = object->complex->y + diff_y;

        iter = geda_complex_get_prim_objs(object->complex);

        while (iter != NULL) {
          sub_object = (GedaObject*)iter->data;
          o_move_end_lowlevel (w_current, sub_object, diff_x, diff_y);
          NEXT(iter);
        }
      }
      else{
        o_move_end_lowlevel (w_current, object, diff_x, diff_y);
      }
      NEXT(selection_iter);
    }

    /* Remove the undo saved in o_move_start */
    o_undo_remove_last_undo(w_current);

    /* Draw the objects that were moved */
    o_invalidate_list (w_current, selection_list);

    /* Draw the connected nets/buses that were also changed */
    o_invalidate_list (w_current, rubbernet_objects);

    /* Call move-objects-hook for moved objects and changed connected
     * nets/buses */
    GList *object_list = g_list_concat (Place_List, rubbernet_objects);

    g_hook_run_object_list (w_current, MOVE_OBJECTS_HOOK, object_list);

    o_undo_savestate(w_current, UNDO_ALL);

    o_move_stretch_destroy_all (w_current->stretch_list);
    w_current->stretch_list = NULL;

    g_list_free (object_list);

    Place_List = NULL;
  }

  i_event_stop_action_handler (w_current);

}

/*!
 * \brief Gschem Cancel Move Operation
 * \par Function Description
 * Cancels the move operation by canceling the action (event) handler,
 * destorying the stretch_list, and free the placelist.
 */
void o_move_cancel (GschemToplevel *w_current)
{
  GList *s_iter;

  /* Unset the dont_redraw flag on rubberbanded objects.
   * We set this above, in o_move_start(). */
  for (s_iter = w_current->stretch_list; s_iter != NULL; NEXT(s_iter))
  {
    STRETCH *stretch = s_iter->data;
    stretch->object->dont_redraw = FALSE;
  }

  i_event_stop_action_handler (w_current);

  o_move_stretch_destroy_all (w_current->stretch_list);

  w_current->stretch_list = NULL;

  geda_struct_place_free_place_list(w_current->toplevel);

  o_undo_callback(w_current, UNDO_ACTION);
}

/*!
 * \brief Invalidate Temporary drawing artifacts While Moving
 * \par Function Description
 *  Calls o_place_invalidate_rubber to invalidate to objects and
 *  then and invalidate any nets being stretched.
 */
void o_move_invalidate_rubber (GschemToplevel *w_current, int drawing)
{
  GList *s_iter;

  int x1, y1, x2, y2;

  o_place_invalidate_rubber (w_current, drawing);

  if (w_current->netconn_rubberband) {

    for (s_iter = w_current->stretch_list; s_iter != NULL; NEXT(s_iter))
    {
      int dx1, dx2, dy1, dy2;

      STRETCH    *s_current = s_iter->data;
      GedaObject *object    = s_current->object;

      switch (object->type) {
        case (OBJ_NET):
        case (OBJ_BUS):
          if (s_current->whichone == 0) {
            dx1 = w_current->second_wx - w_current->first_wx;
            dy1 = w_current->second_wy - w_current->first_wy;
            dx2 = dy2 = 0;
          }
          else {
            dx1 = dy1 = 0;
            dx2 = w_current->second_wx - w_current->first_wx;
            dy2 = w_current->second_wy - w_current->first_wy;
          }

          WORLDtoSCREEN (w_current, object->line->x[0] + dx1,
                                    object->line->y[0] + dy1, &x1, &y1);
          WORLDtoSCREEN (w_current, object->line->x[1] + dx2,
                                    object->line->y[1] + dy2, &x2, &y2);

          o_invalidate_rectangle (w_current, x1, y1, x2, y2);
      }
    }
  }
}

/*!
 * \brief Handle motion during a move operation, resnapping if necessary
 * \par Function Description
 * Handle movement during a move operation, by updating the global
 * candidate transformation parameters.  The \a w_x and \b w_y
 * parameters are the incremental translation to be handled.
 *
 * This function mostly exists to implement the "resnapping" logic,
 * which destructively puts objects back onto the grid during a move
 * operation, when specific criteria are met.
 *
 * \param w_current  Global gschem state structure.
 * \param w_x        X-axis translation
 * \param w_y        Y-axis translation
 */
void o_move_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  Page  *page      = gschem_toplevel_get_current_page(w_current);
  GList *selection = geda_list_get_glist(page->selection_list);

  /* realign the object if we are in resnap mode */
  if (selection != NULL && w_current->snap == SNAP_RESNAP) {

    GedaObject *object;

    if (g_list_length(selection) > 1) {

      GList  *s_current = selection;

      /* The object that objects are supposed to be attached to */
      GedaObject *attached = NULL;
      object = NULL;

      /* Search selection for an object that is not attached
       * to any other object and also check whether everything in the list
       * that *is* attached as an attribute is attached to the same object. */
      while (s_current) {

        GedaObject *candidate = (GedaObject*)s_current->data;

        if (candidate->attached_to == NULL) {

          /* If the object is *not* attached as an attribute, then check
           * whether we previously found an independent object. If we did
           * do not perform snapping, so give up. */
          if (object == NULL) {
            object = candidate;
          }
          else if (candidate != object) {
            object = NULL;
            break; /* Give up */
          }
        }
        else {

          /* If the object is attached as an attribute, then check if the
           * object is attached as an attribute of the same object as
           * everything else is. If not, do not snapping, so give up. */
          if (!attached) {
            attached = candidate->attached_to;
          }
          else if (attached != candidate->attached_to) {
            break; /* Give up */
          }
        }
        NEXT(s_current);
      }

      if (attached != NULL && attached != object) {
        object = NULL;
      }
    }
    else { /* single object */
      object = (GedaObject*) selection->data;
    }

    /* manipulate w_x and w_y in a way that will lead to a position
       of the object that is aligned with the grid */
    if (object) {

      int object_x, object_y;

      if (geda_object_get_position(object, &object_x, &object_y)) {
        w_x += snap_grid (w_current, object_x) - object_x;
        w_y += snap_grid (w_current, object_y) - object_y;
      }
    }
  }

  o_move_invalidate_rubber (w_current, FALSE);
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  o_move_invalidate_rubber (w_current, TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_draw_rubber (GschemToplevel *w_current, int drawing)
{
  GList *s_iter;
  int diff_x, diff_y;

  o_place_draw_rubber (w_current, drawing);

  if (!w_current->netconn_rubberband)
    return;

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  for (s_iter = w_current->stretch_list; s_iter != NULL; NEXT(s_iter))
  {
    STRETCH    *s_current = s_iter->data;
    GedaObject *object    = s_current->object;
    int         whichone  = s_current->whichone;

    /* We can only stretch nets and buses */
    switch (object->type) {
      case OBJ_NET:
      case OBJ_BUS:
        break;
      default:
      continue;
    }

    g_return_if_fail ((whichone >= 0) && (whichone < 2));

    /* Apply stretch */
    object->line->x[whichone] += diff_x;
    object->line->y[whichone] += diff_y;

    /* Draw stretched object */
    eda_renderer_draw (CairoRenderer, object);

    /* Restore original geometry */
    object->line->x[whichone] -= diff_x;
    object->line->y[whichone] -= diff_y;
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static int o_move_return_whichone(GedaObject *object, int x, int y)
{
  if (object->line->x[0] == x && object->line->y[0] == y) {
    return (0);
  }

  if (object->line->x[1] == x && object->line->y[1] == y) {
    return (1);
  }

  v_log_message(_("Could not resolve which end of pin to connect\n"));

  return (-1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_move_check_endpoint(GschemToplevel *w_current, GedaObject *object)
{
  GedaToplevel *toplevel = w_current->toplevel;
  const GList  *cl_current;

  for (cl_current = geda_object_get_conn_list(object); cl_current; NEXT(cl_current))
  {
    GedaObject *other;
    CONN       *c_current;
    int         whichone;

    c_current = (CONN*)cl_current->data;
    other     = c_current->other_object;

    if (other == NULL)
      continue;

    /* really make sure that the object is not selected */
    if (other->selected)
      continue;

    /* Catch pins, whos parent object is selected. */
    if (other->parent_object != NULL && other->parent_object->selected)
      continue;

    if (c_current->type != CONN_ENDPOINT &&
        (c_current->type != CONN_MIDPOINT ||
         c_current->other_whichone == -1))
      continue;

    if (object->type == OBJ_PIN && other->type == OBJ_PIN) {

      GedaObject *new_net;

      /* other object is a pin, insert a net */
      new_net = geda_net_object_new (NET_COLOR,
                                     c_current->x, c_current->y,
                                     c_current->x, c_current->y);
      geda_struct_page_append_object (toplevel->page_current, new_net);

      /* FIXME:This new net object is only picked up for stretching later,
       * somewhat of a kludge. If the move operation is canceled, these
       * new 0 length nets are removed by the "undo" operation invoked.
       */
    }

    /* Only attempt to stretch nets and buses */
    if (other->type != OBJ_NET && other->type != OBJ_BUS)
      continue;

    whichone = o_move_return_whichone (other, c_current->x, c_current->y);

#if DEBUG
    printf ("FOUND: %s type: %d, whichone: %d, x,y: %d %d\n",
            other->name, c_current->type,
            whichone, c_current->x, c_current->y);

    printf("other x,y: %d %d\n", c_current->x, c_current->y);
    printf("type: %d return: %d real: [ %d %d ]\n",
           c_current->type, whichone, c_current->whichone,
           c_current->other_whichone);
#endif

    if (whichone == 0 || whichone == 1) {
      w_current->stretch_list = o_move_stretch_add (w_current->stretch_list,
                                                    other,
                                                    whichone);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_prep_rubberband(GschemToplevel *w_current)
{
  GList      *s_current;
  GedaObject *object;
  GList      *iter;

  for (s_current = geda_list_get_glist (Current_Selection);
       s_current != NULL; NEXT(s_current)) {

    object = s_current->data;

    if (object == NULL)
      continue;

    switch (object->type) {
      case (OBJ_NET):
      case (OBJ_PIN):
      case (OBJ_BUS):
        o_move_check_endpoint (w_current, object);
        break;

      case (OBJ_COMPLEX):
      case (OBJ_PLACEHOLDER):
        for (iter = object->complex->prim_objs; iter != NULL; NEXT(iter))
        {
          GedaObject *o_current = iter->data;

          if (o_current->type == OBJ_PIN) {
            o_move_check_endpoint (w_current, o_current);
          }
        }
        break;
    }
  }
}

/*!
 * \brief
 * \par Function Description
 *  Helper function for o_move_end_rubberband to check for a
 *  zero length displacement.
 */
static int o_move_zero_length(GedaObject *object)
{
#if DEBUG
  printf("x: %d %d y: %d %d\n",
         object->line->x[0], object->line->x[1],
         object->line->y[0], object->line->y[1]);
#endif

  if (object->line->x[0] == object->line->x[1] &&
      object->line->y[0] == object->line->y[1]) {
    return TRUE;
  }

  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_end_rubberband (GschemToplevel *w_current,
                            int w_dx, int w_dy,
                            GList** objects)
{
  GList *s_iter, *s_iter_next;

  for (s_iter = w_current->stretch_list; s_iter != NULL; s_iter = s_iter_next)
  {
    STRETCH *s_current = s_iter->data;
    GedaObject *object = s_current->object;
    int whichone = s_current->whichone;

    /* Store this now, since we may delete the current item */
    s_iter_next = s_iter->next;

    if (object->type == OBJ_NET ||
        object->type == OBJ_BUS) {

      /* remove the object's connections */
      geda_struct_conn_remove_object (object);

      object->line->x[whichone] += w_dx;
      object->line->y[whichone] += w_dy;

      if (o_move_zero_length (object)) {
        w_current->stretch_list =
          o_move_stretch_remove (w_current->stretch_list, object);
        o_delete (w_current, object);
        continue;
      }

      geda_struct_tile_update_object (object);
      geda_struct_conn_update_object (object);
     *objects = g_list_append (*objects, object);
    }
  }
}

/*!
 * \brief Start a Move Action
 * \par Function Description
 *  If objects are selected, prepares the toplevel for a move operation
 *  and causes the selection to be add to the place list. Generates an
 *  error if a stretch_list exist, subsequently canceling the current
 *  state to abort any on-going actions.
 */
static
bool o_move_real_start(GschemToplevel *w_current, int w_x, int w_y)
{
  int status = FALSE;
  GList *s_iter;

  /* Make sure stretch_list == NULL */
  if (w_current->stretch_list != NULL) {
    g_critical ("%s: w_current->stretch_list == NULL\n", __func__);
    //if (w_current->inside_action) {
    i_callback_cancel (w_current, 0, NULL);
    //}
    return FALSE;
  }

  if (o_select_is_selection (w_current)) {

    GList *selection_list;

    /* Save the current state. When rotating the selection when
     * moving, we have to come back to here */
    o_undo_savestate(w_current, UNDO_ALL);

    w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

    w_current->first_wx = w_current->second_wx = w_x;
    w_current->first_wy = w_current->second_wy = w_y;

    selection_list = geda_list_get_glist (Current_Selection);

    o_invalidate_list (w_current, selection_list);

    if (w_current->netconn_rubberband) {

      o_move_prep_rubberband(w_current);

      /* Set the do not_redraw flag on rubberbanded objects and invalidate
       * them. This ensures that they are not drawn (in their un-stretched
       * position) during screen updates. */
      for (s_iter = w_current->stretch_list; s_iter != NULL; NEXT(s_iter))
      {
        STRETCH *stretch = s_iter->data;
        stretch->object->dont_redraw = TRUE;
        o_invalidate_object (w_current, stretch->object);
      }
    }

    o_select_move_to_place_list(w_current);

    o_move_invalidate_rubber (w_current, TRUE);

    status = TRUE;
  }

  i_status_update_action_state(w_current, status);

  return status;
}

/*!
 * \brief Start Move Event
 * \par Function Description
 *  Called from event handlers to possiably initiate a move action.
 *  If the action is to be performed the main button event handler
 *  is replaced ith the paste event handler.
 */
void o_move_start(GschemToplevel *w_current, int w_x, int w_y)
{
  if (o_move_real_start(w_current, w_x, w_y)) {
    i_status_set_state(w_current, MOVEMODE);
    i_event_start_paster_handler(w_current, o_move_end);
  }
}

/*!
 * \brief Start Drag Move Event
 * \par Function Description
 *  Called from event handlers to possiably initiate a drag-move.
 *  If the move action is to be performed the main button event
 *  handler is replaced ith the paste event handler.
 */
void o_move_start_drag(GschemToplevel *w_current, int w_x, int w_y)
{
  if (o_move_real_start(w_current, w_x, w_y)) {
    i_status_set_state(w_current, DRAGMOVE);
    i_event_start_paster_handler(w_current, o_move_end);
  }
}

/** @} endgroup Move-Operations */
