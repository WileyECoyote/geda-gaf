/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include <geda_debug.h>

/** \defgroup Stretch Stretch While Dragging during Move Operations
 *  @{
 */

/*! \brief Add object to Stretch "Semi-Select" List
 *
 *  \ingroup Stretch
 *  \par Function Description
 *  Used to pickup pins nets, and bus enpoints when parent objects
 * are moved. Adds \a object to \a list if \a object is not already
 * in \a list
 *
 */
GList *o_move_stretch_add (GList *list, Object *object, int whichone)
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

  s_new = GEDA_MEM_ALLOC (sizeof (STRETCH));
  s_new->object = object;
  s_new->whichone = whichone;

  return g_list_append (list, s_new);
}


/*! \brief Test if a STRETCH structure points at a given Object
 *
 *  \ingroup Stretch
 *  \par Function Description
 *  Compares if (STRETCH *)a->object == (Object *)b
 *
 * \param [in] a  The STRETCH structure
 * \param [in] b  The Object to test for
 * \returns 0 if STRETCH *a points to Object *b, otherwise 1.
 */
static int find_object (gconstpointer a, gconstpointer b)
{
  return (((STRETCH *)a)->object == (Object *)b) ? 0 : 1;
}


/*! \brief Remove object from Stretch "Semi-Select" List
 *  \ingroup Stretch
 *  \par Function Description
 *
 */
GList *o_move_stretch_remove (GList *list, Object *object)
{
  GList *item;

  g_return_val_if_fail (object != NULL, list);

  item = g_list_find_custom (list, object, find_object);
  GEDA_FREE (item->data);

  return g_list_delete_link (list, item);
}


/*! \brief Debug utility function for Stretch routines
 *
 *  \ingroup Stretch
 *  \par Function Description
 *   Prints name of members to the standard out.
 *
 */
void o_move_stretch_print_all (GList *list)
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

/*! \brief Free Stretch Allocations
 *
 *  \ingroup Stretch
 *  \par Function Description
 *  This function is used to free the memory assocaited with
 *  the scretch routines.
 *  TODO: WEH USed u_glist_clear insead and eliminate this
 *        function.
 */
void o_move_stretch_destroy_all (GList *list)
{
  g_list_foreach (list, (GFunc)g_free, NULL);
  g_list_free (list);
}

/** @} END Group Python_API_Library_Internal */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_start(GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GList *s_iter;

  g_return_if_fail (w_current->stretch_list == NULL);

  if (o_select_is_selection (w_current)) {

    /* Save the current state. When rotating the selection when
     * moving, we have to come back to here */
    o_undo_savestate(w_current, UNDO_ALL);
    w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;
    w_current->event_state = MOVE;

    w_current->first_wx = w_current->second_wx = w_x;
    w_current->first_wy = w_current->second_wy = w_y;

    o_invalidate_glist (w_current, geda_list_get_glist (Top_Selection));

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
    w_current->inside_action = 1;

    o_move_invalidate_rubber (w_current, TRUE);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_end_lowlevel (GschemToplevel *w_current,
                         Object *object,
                         int diff_x, int diff_y)
{
  switch (object->type) {

    case (OBJ_NET):
    case (OBJ_BUS):
    case (OBJ_PIN):
      s_conn_remove_object (object);
      o_translate_world    (diff_x, diff_y, object);
      s_conn_update_linear_object(object);
      break;

    default:
      o_translate_world (diff_x, diff_y, object);
      break;
  }
}

/*! \brief Finalize Move Operation
 *  \par Function Description
 *
 */
void o_move_end(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  Object *object;
  Object *sub_object;
  GList  *selection_list = NULL;
  GList  *rubbernet_objects = NULL;
  GList  *iter;

  int diff_x, diff_y;

  object = o_select_return_first_object(w_current);

  if (!object) {
    /* actually this is an error condition hack */
    w_current->inside_action = 0;
    i_status_set_state(w_current, SELECT);
    return;
  }

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  o_move_invalidate_rubber (w_current, FALSE);
  w_current->rubber_visible = 0;

  if (w_current->netconn_rubberband) {
    o_move_end_rubberband (w_current, diff_x, diff_y, &rubbernet_objects);
  }

  /* Unset the dont_redraw flag on rubberbanded objects.
   * We set this above, in o_move_start(). */
  for (iter = w_current->stretch_list; iter != NULL; NEXT(iter)) {
    STRETCH *stretch = iter->data;
    stretch->object->dont_redraw = FALSE;
  }

  selection_list = Current_Selection->glist;

  while (selection_list != NULL) {

    object = (Object *) selection_list->data;

    if (object == NULL) {
      BUG_MSG("NULL object in o_move_end!");
      return;
    }

    if (GEDA_IS_COMPLEX(object)) {

        /* this next block of code is from */
        /* o_complex_world_translate_world */
        object->complex->x = object->complex->x + diff_x;
        object->complex->y = object->complex->y + diff_y;

        iter = g_list_first( object->complex->prim_objs);
        while (iter != NULL) {
          sub_object = (Object *)iter->data;
          o_move_end_lowlevel (w_current, sub_object, diff_x, diff_y);
          NEXT(iter);
        }

        object->w_bounds_valid_for = NULL;

    }
    else{
      o_move_end_lowlevel (w_current, object, diff_x, diff_y);
    }
    NEXT(selection_list);
  }

  /* Remove the undo saved in o_move_start */
  o_undo_remove_last_undo(w_current);

  /* Draw the objects that were moved */
  o_invalidate_glist (w_current, geda_list_get_glist (Top_Selection));

  /* Draw the connected nets/buses that were also changed */
  o_invalidate_glist (w_current, rubbernet_objects);

  /* Call move-objects-hook for moved objects and changed connected
   * nets/buses */
  GList *moved_list = g_list_concat (Place_List,
                                     rubbernet_objects);
  Place_List = NULL;
  rubbernet_objects = NULL;
  g_run_hook_object_list (w_current, "%move-objects-hook", moved_list);
  g_list_free (moved_list);

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);

  o_move_stretch_destroy_all (w_current->stretch_list);
  w_current->stretch_list = NULL;

}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

  s_place_free_place_list(w_current->toplevel);

  o_move_stretch_destroy_all (w_current->stretch_list);
  w_current->stretch_list = NULL;

  w_current->inside_action = 0;
  i_status_set_state (w_current, SELECT);

  o_undo_callback(w_current, UNDO_ACTION);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_invalidate_rubber (GschemToplevel *w_current, int drawing)
{
  GList *s_iter;
  int dx1, dx2, dy1, dy2;
  int x1, y1, x2, y2;

  o_place_invalidate_rubber (w_current, drawing);
  if (w_current->netconn_rubberband) {

    for (s_iter = w_current->stretch_list; s_iter != NULL; NEXT(s_iter))
    {
      STRETCH *s_current = s_iter->data;
      Object *object = s_current->object;

      switch (object->type) {
        case (OBJ_NET):
        case (OBJ_BUS):
          if (s_current->whichone == 0) {
            dx1 = w_current->second_wx - w_current->first_wx;
            dy1 = w_current->second_wy - w_current->first_wy;
            dx2 = dy2 = 0;
          } else {
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GList *selection, *s_current;
  Object *object;
  int object_x, object_y;
  bool resnap = FALSE;

  selection = geda_list_get_glist( Top_Selection );

  /* realign the object if we are in resnap mode */
  if (selection != NULL && w_current->snap == SNAP_RESNAP) {

    if (g_list_length(selection) > 1) {
      /* find an object that is not attached to any other object */
      for (s_current = selection; s_current != NULL; NEXT(s_current)) {
        if (((Object *) s_current->data)->attached_to == NULL) {
          object = (Object *) s_current->data;
          resnap = TRUE;
          break;
        }
      }

      /* Only resnap single elements. This is also the case if
         the selection contains one object and all other object
         elements are attributes of the object element.*/
      for (s_current = selection; s_current != NULL && resnap == TRUE; NEXT(s_current))
      {
        if (!(object == (Object *) s_current->data)
            && !o_attrib_is_attached((Object *) s_current->data, object)) {
          resnap = FALSE;
        }
      }
    }
    else { /* single object */
      resnap = TRUE;
      object = (Object *) selection->data;
    }

    /* manipulate w_x and w_y in a way that will lead to a position
       of the object that is aligned with the grid */
    if (resnap) {
      if (o_get_position(&object_x, &object_y, object)) {
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
void
o_move_draw_rubber (GschemToplevel *w_current, int drawing)
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
    STRETCH *s_current = s_iter->data;
    Object *object = s_current->object;
    int whichone = s_current->whichone;

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
    eda_renderer_draw (w_current->renderer, object);

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
int o_move_return_whichone(Object * object, int x, int y)
{
  if (object->line->x[0] == x && object->line->y[0] == y) {
    return (0);
  }

  if (object->line->x[1] == x && object->line->y[1] == y) {
    return (1);
  }

  fprintf(stderr,
          _("DOH! tried to find the whichone, but didn't find it!\n"));
  return (-1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_check_endpoint(GschemToplevel *w_current, Object * object)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GList *cl_current;
  CONN *c_current;
  Object *other;
  int whichone;

  if (!object)
  return;

  if (object->type != OBJ_NET &&
      object->type != OBJ_PIN &&
      object->type != OBJ_BUS) {
    fprintf(stderr, _("Got a non line object in o_move_check_endpoint\n"));
    return;
  }

  for (cl_current = object->conn_list; cl_current != NULL; NEXT(cl_current))
  {
    c_current = (CONN *) cl_current->data;
    other = c_current->other_object;

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

      Object *new_net;
      /* other object is a pin, insert a net */
      new_net = o_net_new (NET_COLOR,
                           c_current->x, c_current->y,
                           c_current->x, c_current->y);
      s_page_append_object (toplevel->page_current, new_net);

      /* FIXME:This new net object is only picked up for stretching later,
       * somewhat of a kludge. If the move operation is cancelled, these
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
      w_current->stretch_list =
        o_move_stretch_add (w_current->stretch_list, other, whichone);
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
  GedaToplevel *toplevel = w_current->toplevel;
  GList *s_current;
  Object *object;
  Object *o_current;
  GList *iter;

  for (s_current = geda_list_get_glist (Top_Selection);
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
          o_current = iter->data;

          if (o_current->type == OBJ_PIN) {
            o_move_check_endpoint (w_current, o_current);
          }
        }
        break;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_move_zero_length(Object * object)
{
#if DEBUG
  printf("x: %d %d y: %d %d\n",
         object->line->x[0], object->line->x[1],
         object->line->y[0], object->line->y[1]);
#endif

  if (object->line->x[0] == object->line->x[1] &&
      object->line->y[0] == object->line->y[1]) {
    return TRUE;
  } else {
    return FALSE;
  }
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
    Object *object = s_current->object;
    int whichone = s_current->whichone;

    /* Store this now, since we may delete the current item */
    s_iter_next = s_iter->next;

    if (object->type == OBJ_NET ||
        object->type == OBJ_BUS) {

      /* remove the object's connections */
      s_conn_remove_object (object);

      object->line->x[whichone] += w_dx;
      object->line->y[whichone] += w_dy;

      if (o_move_zero_length (object)) {
        w_current->stretch_list =
          o_move_stretch_remove (w_current->stretch_list, object);
        o_delete (w_current, object);
        continue;
      }

      object->w_bounds_valid_for = NULL;
      s_tile_update_object (object);
      s_conn_update_object (object);
      *objects = g_list_append (*objects, object);
    }
  }
}
