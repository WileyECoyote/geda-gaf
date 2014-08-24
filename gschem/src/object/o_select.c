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
 * Foundation, Inc., 51 Franklin Street, Boston, MA, 02110-1301 USA
 */
/*! \note The code in this file is sometimes not obvious, especially
 * o_select_object (which implements the selection of objects either
 * when doing a single or multi select)
 *
 * Also, there are cases where it looks like there is redundant code, which
 * could be removed/merged, but I purposely didn't do so to keep the code
 * readable
 *
 * the count == 0 stuff really only applies to when you are coming from a
 * multi select case
 */

#include <gschem.h>
#include <geda_debug.h>

typedef enum { DESELECT_HOOK, SELECT_HOOK} HOOKS;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
static void
o_select_run_hooks(GschemToplevel *w_current, Object *o_current, HOOKS which_hook)
{
#if DEBUG || DEBUG_HOOKS || DEBUG_SELECT
  fprintf(stderr, "o_select_run_hooks: begin\n");
#endif

  switch (which_hook) {

    /* If flag == 0, then we are deselecting something. */
    case DESELECT_HOOK:
      g_run_hook_object (w_current, "%deselect-objects-hook", o_current);
      break;

      /* If flag == 1, then we are selecting something. */
    case SELECT_HOOK:
      g_run_hook_object (w_current, "%select-objects-hook", o_current);
      break;

    default:
      g_critical("Internal Error Detected: <o_select_run_hooks> unhandled case\n");
  }

#if DEBUG || DEBUG_HOOKS || DEBUG_SELECT
  fprintf(stderr, "o_select_run_hooks: exit\n");
#endif
}

/*! \brief Select or Unselect an Object
 *  \par Function Description
 *     This function adds or removes an object from the current selection
 *
 *  \param [in] w_current  A pointer to a GschemToplevel object.
 *  \param [in] o_current  The Object of interest.
 *  \param [in] type       Is more of a mode flag.
 *  \param [in] count      Number of objects.
 *
 *  \note
 *  type can be either SINGLE meaning selection is a single mouse click
 *      or it can be MULTIPLE meaning selection is a selection box
 */
void
o_select_object(GschemToplevel *w_current, Object *o_current,
                int type, int count)
{
  SELECTION *selection = Current_Selection;
  bool       removing_obj;
  int        CONTROLKEY;
  int        SHIFTKEY;

#if DEBUG
  printf("Object id: %d\n", o_current->sid);
  o_selection_print_all(Current_Selection);
#endif

  SHIFTKEY     = w_current->SHIFTKEY;

  removing_obj = FALSE;

  /* If in "deselect mode" reverse OUR state of CONTROLKEY */
  if (w_current->event_state == STARTDESELECT)
    CONTROLKEY = !w_current->CONTROLKEY;
  else
    CONTROLKEY = w_current->CONTROLKEY;

  switch(o_current->selected) {

    case(FALSE):          /* object not selected */

      switch(SHIFTKEY) {  /* shift key pressed? */

        case(TRUE):          /* shift key pressed  */
          /* just fall through  */
          break; /* Or not? */

        case(FALSE):

          /* condition: first object being added  */
          /* condition: control key not pressed   */
          /* condition: for both multiple and single object added */
          /* result: remove all objects from selection */
          if (count == 0 && !CONTROLKEY) {
            o_select_unselect_all(w_current);
          }
          break;

      } /* End Switch shift key */

      /* object not selected, so add it to the selection list */
      o_select_run_hooks (w_current, o_current, SELECT_HOOK);
      o_selection_add    (selection, o_current);
      break;

    case(TRUE):  /* object was already selected */

      if (SHIFTKEY) { /* shift key pressed ? */

          /* condition: not doing multi-selection  */
          /*     then : remove object from selection */
          if (type != MULTIPLE) {
            o_select_run_hooks (w_current, o_current, DESELECT_HOOK);
            o_selection_remove (selection, o_current);
            removing_obj = TRUE;
          }

      }
      else { /* shift key not pressed */

          /* condition: doing multiple */
          /* condition: first object being added */
          /* condition: control key not pressed  */
          /*   then 1 : remove all objects from selection */
          /*        2 : add object to selection */
          if (type == MULTIPLE && count == 0 && !CONTROLKEY) {
            o_select_unselect_all (w_current);
            o_select_run_hooks    (w_current, o_current, SELECT_HOOK);
            o_selection_add       (selection, o_current);
          }

          /* condition: doing single object add */
          /* condition: control key not pressed */
          /* 1st objective: remove all objects from selection */
          /* 2nd objective: add object to selection list */
          if (type == SINGLE && !CONTROLKEY) {
            o_select_unselect_all (w_current);
            o_select_run_hooks    (w_current, o_current, SELECT_HOOK);
            o_selection_add       (selection, o_current);
          }

          if (CONTROLKEY) {
            o_select_run_hooks    (w_current, o_current, DESELECT_HOOK);
            o_selection_remove    (selection, o_current);
            removing_obj = TRUE;
          }

      }
      break;
  } /* End Switch object selected */

  /* do the attributes */
  if ( TRUE == removing_obj) {
    /* Remove the invisible attributes from the object list as well,
     * so they don't remain selected without the user knowing. */
     o_attrib_deselect_invisible (w_current, selection, o_current);
  }
  else {
    /* If the type is MULTIPLE (meaning a select box was/is being used),
     * only select invisible attributes on objects. Otherwise attributes
     * will be "double selected", causing them to remain unselected if
     * using invert-selection (CONTROLKEY is pressed) */
    if ( MULTIPLE == type ) {
      o_attrib_select_invisible (w_current, selection, o_current);
    }
    else {

      /* Select all attributes of the object for a single click select */
      o_attrib_add_selected (w_current, selection, o_current);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_select_box_start(GschemToplevel *w_current, int w_x, int w_y)
{
  int diff_x, diff_y;

  diff_x = abs(w_current->first_wx - w_x);
  diff_y = abs(w_current->first_wy - w_y);

  /* if we are still close to the button press location,
   *     then don't enter the selection box mode */
  if (SCREENabs (w_current, max(diff_x, diff_y)) < 10) {
    return FALSE;
  }

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  return TRUE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_end(GschemToplevel *w_current, int w_x, int w_y)
{
  o_select_box_invalidate_rubber (w_current);
  w_current->rubber_visible = FALSE;

  o_select_box_search(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->rubber_visible)
    o_select_box_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_select_box_invalidate_rubber (w_current);
  w_current->rubber_visible = TRUE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void o_select_box_invalidate_rubber (GschemToplevel *w_current)
{
  int x1, y1, x2, y2;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  o_invalidate_rect (w_current, x1, y1, x2, y1);
  o_invalidate_rect (w_current, x1, y1, x1, y2);
  o_invalidate_rect (w_current, x2, y1, x2, y2);
  o_invalidate_rect (w_current, x1, y2, x2, y2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_draw_rubber (GschemToplevel *w_current)
{
  o_box_draw_rubber (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_search(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object *o_current=NULL;
  int count = 0; /* object count */
  int SHIFTKEY = w_current->SHIFTKEY;
  int CONTROLKEY = w_current->CONTROLKEY;
  int left, right, top, bottom;
  const GList *iter;

  left = min(w_current->first_wx, w_current->second_wx);
  right = max(w_current->first_wx, w_current->second_wx);
  top = min(w_current->first_wy, w_current->second_wy);
  bottom = max(w_current->first_wy, w_current->second_wy);

  iter = s_page_get_objects (toplevel->page_current);
  while (iter != NULL) {
    o_current = iter->data;
    /* only select visible objects */
    //if (o_get_is_visible (o_current) || toplevel->page_current->show_hidden_text) {
    if (o_get_is_visible (o_current)) {
      int cleft, ctop, cright, cbottom;

      if ( world_get_single_object_bounds(o_current,
        &cleft, &ctop, &cright, &cbottom) &&
        cleft >= left &&
        cright <= right &&
        ctop >= top &&
        cbottom <= bottom ) {

        o_select_object(w_current, o_current, MULTIPLE, count);
      count++;
        }
    }
    iter = g_list_next (iter);
  }

  /* if there were no objects to be found in select box, count will be */
  /* zero, and you need to deselect anything remaining (except when the */
  /* shift or control keys are pressed) */
  if (count == 0 && !SHIFTKEY && !CONTROLKEY) {
    o_select_unselect_all (w_current);
  }
  i_status_update_sensitivities(w_current);

}

/*! \brief Select all nets connected to the current net
 *  \par Depending on the state of the w_current->net_selection_mode variable
 *   and the net_selection_state of the current net this function will either
 *   select the single net, all directly connected nets or all nets connected
 *   with netname labels.
 *  \param [in] w_current  Pointer to GschemToplevel struct.
 *  \param [in] o_net      Pointer to a single net object
 */
void o_select_connected_nets(GschemToplevel *w_current, Object* o_net)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object *o_current;
  const   GList *o_iter;
          GList *iter1;
  GList  *netstack     = NULL;
  GList  *netnamestack = NULL;
  GList  *netnameiter;
  char   *netname;
  int     count;

  if (o_net->type != OBJ_NET) {
    BUG_MSG("Object is not a Net type\n");
    return;
  }

  if (!o_net->selected) {
    w_current->net_selection_state = 1;
  }

  /* the current net is the startpoint for the stack */
  netstack = g_list_prepend(netstack, o_net);

  count = 0;
  while (1) {
    netnameiter = g_list_last(netnamestack);
    for (iter1  = g_list_last(netstack);
         iter1 != NULL;
         iter1  = g_list_previous(iter1), count++) {
      o_current = iter1->data;
      if (o_current->type == OBJ_NET &&
        (!o_current->selected || count == 0)) {
        o_select_object (w_current, o_current, SINGLE, count);
        if (w_current->net_selection_state > 1) {
          /* collect nets */
          netstack = g_list_concat(s_conn_return_others(NULL, o_current),
                                   netstack);
        }
        if (w_current->net_selection_state > 2) {
          /* collect netnames */
          netname = o_attrib_search_object_attribs_by_name (o_current,
                                                            "netname", 0);
          if (netname != NULL) {
            if (g_list_find_custom(netnamestack, netname,
                                  (GCompareFunc) strcmp) == NULL) {
              netnamestack = g_list_append(netnamestack, netname);
            }
            else {
              GEDA_FREE(netname);
            }
          }
        }
      }
    }
    g_list_free(netstack);
    netstack = NULL;

    if (netnameiter == g_list_last(netnamestack))
      break; /* no new netnames in the stack --> finished */

      /* get all the nets of the stacked netnames */
    for (o_iter  = s_page_get_objects (toplevel->page_current);
         o_iter != NULL;
         o_iter  = g_list_next (o_iter)) {
      o_current  = o_iter->data;
      if (o_current->type == OBJ_TEXT
        && o_current->attached_to != NULL) {
        if (o_current->attached_to->type == OBJ_NET) {
          netname =
          o_attrib_search_object_attribs_by_name (o_current->attached_to,
                                                  "netname", 0);
          if (netname != NULL) {
            if (g_list_find_custom (netnamestack,
              netname, (GCompareFunc) strcmp) != NULL) {
              netstack = g_list_prepend(netstack, o_current->attached_to);
            }
            GEDA_FREE(netname);
          }
        }
      }
    }
  }

  w_current->net_selection_state += 1;
  if (w_current->net_selection_state > w_current->net_selection_mode)
    w_current->net_selection_state = 1;

  for (iter1 = netnamestack; iter1 != NULL; iter1 = g_list_next(iter1))
    GEDA_FREE(iter1->data);
  g_list_free(netnamestack);
}

/*! \brief Get the Number of Currently Selected Objects
 *  \par Function Description
 *   Returns the selection set count. Does not check validity
 *   of objects, this should have been done when the objects
 *   were added, nor does this function varify that the object
 *   or objects still exist.
 *
 * \returns count of the selection list..
 */
int o_select_get_count(GschemToplevel *w_current)
{
  return g_list_length(w_current->Top_Selection->glist);
}

/*! \brief Check if any Currently Select Objects
 *  \par Function Description
 *
 * \returns TRUE if the selection list is not empty,
 *          FALSE if the selection list is empty,
 */
bool o_select_is_selection(GschemToplevel *w_current)
{
  return (w_current->Top_Selection->glist != FALSE);
}
/*! \brief UnSelect All Objects
 *  \par Function Description
 *   Removes all members from the toplevel selection list
 *  and calls the run hook function, after the object(s)
 *  have been removed, pass a list of the object that were
 *  removed.
 *
 * \remarks WEH: Could it help to have a freeze/thaw wrapper
 *          around the do loop if count was many?
 */
void o_select_unselect_all(GschemToplevel *w_current)
{
  GedaToplevel  *toplevel  = w_current->toplevel;
  SELECTION *selection = Top_Selection;
  GList     *removed;
  GList     *iter;
  Object    *object;
  int        count;

  if (selection->glist != NULL) {

    removed = g_list_copy(selection->glist);

    count   = g_list_length(removed);

    if (count > 1) {
      iter = g_list_first(removed);
      do {
        object = iter->data;
        o_selection_remove(selection, object );
        NEXT(iter);
      } while (iter);
      /* Call hooks with list of removed objects */
      g_run_hook_object_list(w_current, "%deselect-objects-hook", removed);
    }
    else {
      object = removed->data;
      if (GEDA_IS_OBJECT(object)) {
        o_selection_remove(selection, object);
        g_run_hook_object(w_current, "%deselect-objects-hook", object);
      }
    }
    g_list_free(removed);
  }
}

/*! \brief Selects all visible objects on the current page.
 * \par Function Description
 * Clears any existing selection, then selects everything visible and
 * unlocked on the current page, and any attached attributes whether
 * visible or invisible..
 *
 * \param w_current  Pointer to a GschemToplevel structure.
 */
void
o_select_visible_unlocked (GschemToplevel *w_current)
{
  GedaToplevel  *toplevel  = w_current->toplevel;
  SELECTION *selection = Top_Selection;
  const GList *iter;
  GList *added;

  o_select_unselect_all (w_current);
  for (iter = s_page_get_objects (toplevel->page_current);
       iter != NULL;
       iter = g_list_next (iter)) {
    Object *obj = (Object *) iter->data;

    /* Skip invisible objects. */
    if (!o_get_is_visible (obj) && !toplevel->page_current->show_hidden_text)
      continue;

    /* Skip locked objects. */
    if (!obj->selectable) continue;

      /* Add object to selection. */
      /*! \bug We can't call o_select_object() because it
       * behaves differently depending on the state of
       * w_current->SHIFTKEY and w_current->CONTROLKEY, which may well
       * be set if this function is called via a keystroke
       * (e.g. Ctrl-A). */
       o_selection_add (selection, obj);

    /* Add any attributes of object to selection as well. */
    o_attrib_add_selected (w_current, selection, obj);
  }

  /* Run hooks for all items selected */
  added = geda_list_get_glist (selection);
  if (added != NULL) {
    g_run_hook_object_list (w_current, "%select-objects-hook", added);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_select_move_to_place_list(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GList *selection;
  GList *selection_copy;

  /* remove the old place list if it exists */
  s_object_release_objects(toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  selection = geda_list_get_glist( Top_Selection );
  selection_copy = g_list_copy( selection );
  toplevel->page_current->place_list = selection_copy;
}

/*! \brief Create a list of selected object of the given type
 *  \par Function Description
 *  This is a general utility function that is like get selection except
 *  this function returns a list containing only objects of the specified
 *  type.
 *
 *  \param w_current pointer to GschemToplevel context
 *  \param otype     An Object type Object, not checked.
 *
 *  \returns Glist* list of selected object or NULL is no object of the
 *                  specified type are selected.
 *
 *  \note Caller should g_list_free returned list
 */
GList*
o_select_get_list_selected(GschemToplevel *w_current, char otype)
{
  GList  *selection, *iter, *list = NULL;
  Object *object;

  /* Get the current selection list, alias Current_Selection->glist */
  selection = geda_list_get_glist(Current_Selection);
  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
      object = (Object *) iter->data;
      if ( object->type == otype)
        list = g_list_append (list, object);
  }
  return list;
}

/*! \brief Return the First Selected Object
 *  \par Function Description
 * This is a wrapper for o_selection_return_first_object.
 * This function always looks at the current page selection list
 *
 *  \param w_current Pointer to a Gschem Toplevel object
 */
Object *o_select_return_first_object(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  if (! ( w_current &&
          toplevel->page_current &&
          geda_list_get_glist( Top_Selection )
        )
     )
    return NULL;
  else
    return (Object *) g_list_first( geda_list_get_glist(Top_Selection))->data;
}
