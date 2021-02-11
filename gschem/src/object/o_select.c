/* -*- C o_select.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file o_select.c
 * \brief Low-level module for Selecting objects
 * \note The code in this file is sometimes not obvious, especially
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

/*!
 * \brief Run Hook Selection Changed
 * \par Function Description
 *  Passes \a o_current to g_hook_run_object for deselection or selection
 *  depending on \a which_hook.
 */
static void
o_select_run_hooks(GschemToplevel *w_current, GedaObject *o_current, EID_SCM_HOOKS which_hook)
{
  if (which_hook == DESELECT_OBJECTS_HOOK) {

    /* then we are deselecting something. */
    w_current->which_object = o_current;
    g_hook_run_object (w_current, DESELECT_OBJECTS_HOOK, o_current);

  }
  else { /* else we are selecting something. */
    g_hook_run_object (w_current, SELECT_OBJECTS_HOOK, o_current);
  }
}

/*!
 * \brief Select or Unselect an Object
 * \par Function Description
 *  Adds or removes an object from the current selection
 *
 * \param [in] w_current  A pointer to a GschemToplevel object,
 * \param [in] o_current  The Object of interest,
 * \param [in] type       Is more of a mode flag,
 * \param [in] count      Number of objects.
 *
 * \note type can be either SINGLE meaning selection is a single mouse
 *       click or it can be MULTIPLE meaning selection is a selection
 *       box.
 */
void
o_select_object(GschemToplevel *w_current, GedaObject *o_current, int type, int count)
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

  w_current->which_object = o_current;

  switch (o_current->selected) {

    case(FALSE):                /* object not selected */

      switch (SHIFTKEY) {       /* shift key pressed? */

        case(TRUE):             /* shift key pressed  */
          /* just fall through  */
          break;                /* WEH: Or not? */

        case(FALSE):

          /* condition: first object being added  */
          /* condition: control key not pressed   */
          /* condition: for both multiple and single object added */
          /* result: remove all objects from selection */
          if (count == 0 && !CONTROLKEY) {
            o_select_unselect_all(w_current);
          }
          break;

      }                        /* End Switch shift key */

      /* object not selected, so add it to the selection list */
      o_select_run_hooks (w_current, o_current, SELECT_OBJECTS_HOOK);
      geda_object_selection_add    (selection, o_current);
      break;

    case(TRUE):                /* object was already selected */

      if (SHIFTKEY) {          /* shift key pressed ? */

          /* condition: not doing multi-selection  */
          /*     then : remove object from selection */
          if (type != MULTIPLE) {
            o_select_run_hooks (w_current, o_current, DESELECT_OBJECTS_HOOK);
            geda_object_selection_remove (selection, o_current);
            removing_obj = TRUE;
          }

      }
      else {                   /* shift key not pressed */

          /* condition: doing multiple */
          /* condition: first object being added */
          /* condition: control key not pressed  */
          /*   then 1 : remove all objects from selection */
          /*        2 : add object to selection */
          if (type == MULTIPLE && count == 0 && !CONTROLKEY) {
            o_select_unselect_all (w_current);
            o_select_run_hooks (w_current, o_current, SELECT_OBJECTS_HOOK);
            geda_object_selection_add    (selection, o_current);
          }

          /* condition: doing single object add */
          /* condition: control key not pressed */
          /* 1st objective: remove all objects from selection */
          /* 2nd objective: add object to selection list */
          if (type == SINGLE && !CONTROLKEY) {
            o_select_unselect_all (w_current);
            o_select_run_hooks (w_current, o_current, SELECT_OBJECTS_HOOK);
            geda_object_selection_add    (selection, o_current);
          }

          if (CONTROLKEY) {
            o_select_run_hooks (w_current, o_current, DESELECT_OBJECTS_HOOK);
            geda_object_selection_remove (selection, o_current);
            removing_obj = TRUE;
          }

      }
      break;
  }                            /* End Switch object selected */

  /* do the attributes */
  if (TRUE == removing_obj) {

    /* Remove the invisible attributes from the object list as well,
     * so they don't remain selected without the user knowing. */
     o_attrib_deselect_invisible (w_current, selection, o_current);

     /* If nothing else to "deselect", then goto select mode */
     if (g_list_length(selection->glist) == 0) {
       i_status_set_state (w_current, SELECT);
     }
  }
  else {
    /* If the type is MULTIPLE (meaning a select box was/is being used),
     * only select invisible attributes on objects. Otherwise attributes
     * will be "double selected", causing them to remain unselected if
     * using invert-selection (CONTROLKEY is pressed) */
    if (MULTIPLE == type ) {
      o_attrib_select_invisible (w_current, selection, o_current);
    }
    else {

      /* Select all attributes of the object for a single click select */
      o_attrib_attached_2_selection (w_current, selection, o_current);
    }
  }
}

/*!
 * \brief Add Objects in List to Selection
 * \par Function Description
 *  Adds items in the list to the current selection set.
 *
 * \sa o_select_move_to_place_list
 *
 * \note see comment for o_select_visible_unlocked regarding
 *       geda_object_selection_add
 */
void o_select_add_list(GschemToplevel *w_current, GList *list)
{
  GedaToplevel *toplevel  = w_current->toplevel;
  SELECTION    *selection = toplevel->page_current->selection_list;
  GList        *iter      = list;

  while (iter) {
    GedaObject *object = iter->data;
    w_current->which_object = object;
    geda_object_selection_add (selection, object);
    o_select_run_hooks(w_current, object, SELECT_OBJECTS_HOOK);
    iter = iter->next;
  }
}

/*!
 * \brief Add Object to Selection without BS
 * \par Function Description
 *  Adds object to the current selection list.
 *
 * \sa o_select_move_to_place_list
 *
 * \note see comment for o_select_visible_unlocked regarding
 *       geda_object_selection_add
 */
void o_select_add_object(GschemToplevel *w_current, GedaObject *object)
{
  GedaToplevel *toplevel  = w_current->toplevel;
  SELECTION    *selection = toplevel->page_current->selection_list;

  if (GEDA_IS_OBJECT(object)) {
    w_current->which_object = object;
    geda_object_selection_add (selection, object);
    o_select_run_hooks(w_current, object, SELECT_OBJECTS_HOOK);
  }
  else {
    BUG_MSG("Invalid object");
  }
}

/*!
 * \brief Start the process of selection
 * \par Function Description
 *  Chooses the way of how to start the selection process. If no
 *  grip was found at the given coordinates the function toggles
 *  the current state into the STARTSELECT mode in order to define
 *  what to do farther. Otherwise, it switches on the GRIPS mode
 *  for working with the grip found.
 *
 *  The function is intended to be called by pressing the left
 *  mouse button.
 *
 * \param [in] w_current The GschemToplevel structure.
 * \param [in] wx        The unsnapped X coordinate.
 * \param [in] wy        The unsnapped Y coordinate.
 */
void o_select_start (GschemToplevel *w_current, int wx, int wy)
{
  if (!o_grips_start(w_current, wx, wy)) {
    /* now go into normal SELECT */
    w_current->first_wx = w_current->second_wx = wx;
    w_current->first_wy = w_current->second_wy = wy;
    i_status_set_state (w_current, w_current->event_state = STARTSELECT);
  }
}

/*!
 * \brief End the process of selection
 * \par Function Description
 *  Finishes the process of selection if the \a o_select_start()
 *  or \a o_select_motion() functions haven't defined other
 *  functions to finish it. If no grip was found at the given
 *  coordinates the function tries to find an object under the
 *  mouse pointer and select it.  Otherwise, it switches on the
 *  GRIPS mode for working with the grip found.
 *
 *  The function is intended to be called by releasing the left
 *  mouse button.
 *
 * \param [in] w_current The GschemToplevel structure.
 * \param [in] wx        The world X coordinate.
 * \param [in] wy        The world Y coordinate.
 */
void o_select_end (GschemToplevel *w_current, int wx, int wy)
{
  o_find_object(w_current, wx, wy, TRUE);
  i_status_action_stop(w_current);
}

/*!
 * \brief Determine whether objects have to be selected or moved
 * \par Function Description
 *  Checks if the shift or control keys are pressed, (that means
 *  the user definitely wants to drag out a selection box), or
 *  there are no selected objects under the cursor. In that case
 *  the function starts drawing the selection box. Otherwise, it
 *  looks for the objects that have been or could be selected and
 *  starts moving them.
 *
 *  The function is intended to be called by motion of the mouse
 *  while the left mouse button is pressed.
 *
 * \param [in] w_current The GschemToplevel structure.
 * \param [in] wx        The world X coordinate.
 * \param [in] wy        The world Y coordinate.
 */
bool o_select_motion (GschemToplevel *w_current, int wx, int wy)
{
  GedaObject *selected;

  bool result;

  int x1 = w_current->first_wx;
  int y1 = w_current->first_wy;

  selected = o_find_selected_object(w_current, x1, y1);

  if (!w_current->drag_can_move || !selected) {
    if (o_select_box_start(w_current, wx, wy)) {
      w_current->event_state = SBOX;
    }
    result = FALSE;
  }
  else {
    /* If a shift or control key is pressed, that means the user
     * definitely wants to drag out a selection box.  Otherwise, if
     * there is not a selected object under the cursor, look for one
     * that could be selected and start moving it.
     */
    if (w_current->SHIFTKEY   ||
        w_current->CONTROLKEY ||
       (!selected &&
       (!o_find_object(w_current, x1, y1, TRUE) ||
        !o_select_is_selection(w_current))))
    {
      if (o_select_box_start(w_current, wx, wy)) {
        w_current->event_state = SBOX;
      }
      result = FALSE;
    }
    else
    {
      result = TRUE;
    }
  }

  return result;
}

/*!
 * \brief Start Windowed/Box Selection
 * \par Function Description
 *  Similar to other "event" start routines, this function is used to
 *  capture the cursor position at the on-set of a boxed selection
 *  operation.
 *
 * \param [in] w_current The GschemToplevel object,
 * \param [in] w_x       Snapped X coordinate in World units,
 * \param [in] w_y       Snapped Y coordinate in World units.
 *
 * \todo Reeks box-selection-threashold
 */
int o_select_box_start(GschemToplevel *w_current, int w_x, int w_y)
{
  int status;
  int dx, dy;

  dx = abs(w_current->first_wx - w_x);
  dy = abs(w_current->first_wy - w_y);

  /* if the pointer is still close to the button press location,
   * then don't enter the selection box mode */
  if (SCREENabs (w_current, max(dx, dy)) < 10) {
    i_status_action_stop(w_current);
    status = FALSE;
  }
  else {
    i_status_action_start(w_current);
    w_current->second_wx = w_x;
    w_current->second_wy = w_y;
    status = TRUE;
  }

  return status;
}

/*!
 * \brief End Window/Box Selection Callback
 * \par Function Description
 *  Invalidates the drawn temporary selection box and invokes selection
 *  search routines.
 *
 * \param [in] w_current The GschemToplevel object,
 * \param [in] wx        Unsnapped X coordinate in World units,
 * \param [in] w_y       Unsnapped Y coordinate in World units.
 */
void
o_select_box_end(GschemToplevel *w_current, int wx, int w_y)
{
  o_select_box_invalidate_rubber (w_current);
  w_current->rubber_visible = FALSE;

  o_select_box_search(w_current);
  i_status_action_stop(w_current);
}

/*!
 * \brief Window/Box Selection Pointer Motion Callback
 * \par Function Description
 *  Invalidates the old temporary selection box, aka rubber, and causes
 *  new rubber to be drawn. This has the effect of drawing a temporary
 *  box while dragging the edge.
 *
 * \param [in] w_current  The GschemToplevel object,
 * \param [in] us_wx      Unsnapped X coordinate in World units,
 * \param [in] us_wy      Unsnapped Y coordinate in World units.
 */
void
o_select_box_motion (GschemToplevel *w_current, int us_wx, int us_wy)
{
  if (w_current->rubber_visible) {
    o_select_box_invalidate_rubber (w_current);
  }

  w_current->second_wx = us_wx;
  w_current->second_wy = us_wy;

  o_select_box_invalidate_rubber (w_current);
  w_current->rubber_visible = TRUE;
}

/*!
 * \brief Invalidate Temporary drawing artifacts for Box Selection
 * \par Function Description
 *  Note the similarity to o_box_invalidate_rubber()
 *
 * \param [in] w_current  The GschemToplevel object.
 */
void
o_select_box_invalidate_rubber (GschemToplevel *w_current)
{
  int x1, y1, x2, y2;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  o_invalidate_rectangle (w_current, x1, y1, x2, y1);
  o_invalidate_rectangle (w_current, x1, y1, x1, y2);
  o_invalidate_rectangle (w_current, x2, y1, x2, y2);
  o_invalidate_rectangle (w_current, x1, y2, x2, y2);
}

/*!
 * \brief Draw Temporary Box for Windowed Selection Operation
 * \par Function Description
 *  Since the functionality is the same as drawing rubber for a box
 *  object, this function calls the box object functions to draw the
 *  temporary box used during the windowing selection operation.
 */
void
o_select_box_draw_rubber (GschemToplevel *w_current)
{
  o_box_draw_rubber (w_current);
}

/*!
 * \brief Search Windowed Region for Object Selection
 * \par Function Description
 *  Calls o_select_object for objects bounded by the windowed/box
 *  region
 *
 * \param [in] w_current  The GschemToplevel object.
 *
 * \todo primitive
 */
void
o_select_box_search(GschemToplevel *w_current)
{
  GedaToplevel *toplevel  = w_current->toplevel;

  int count      = 0; /* object count */
  int SHIFTKEY   = w_current->SHIFTKEY;
  int CONTROLKEY = w_current->CONTROLKEY;

  int left, right, top, bottom;
  const GList *iter;

  left   = min(w_current->first_wx, w_current->second_wx);
  right  = max(w_current->first_wx, w_current->second_wx);
  top    = min(w_current->first_wy, w_current->second_wy);
  bottom = max(w_current->first_wy, w_current->second_wy);

  iter   = geda_struct_page_get_objects (toplevel->page_current);

  while (iter != NULL) {

    GedaObject *o_current = iter->data;

    /* only select visible objects */
      if (geda_object_get_is_visible (o_current)) {

        int cleft, ctop, cright, cbottom;

        if (geda_object_get_bounds(o_current, &cleft, &ctop, &cright, &cbottom) &&
            cleft   >= left  &&
            cright  <= right &&
            ctop    >= top   &&
            cbottom <= bottom)
        {
          o_select_object(w_current, o_current, MULTIPLE, count);
          count++;
        }
      }
    iter = iter->next;
  }

  /* if there were no objects to be found in select box, count will be */
  /* zero, and we need to deselect anything remaining (except when the */
  /* shift or control keys are pressed) */
  if (count == 0 && !SHIFTKEY && !CONTROLKEY) {
    o_select_unselect_all (w_current);
  }

  i_status_update_sensitivities(w_current);
}

/*!
 * \brief Select all nets connected to the current net
 * \par Function Description
 *  Depending on the state of the w_current->net_selection_mode variable
 *  and the net_selection_state of the current net this function will either
 *  select the single net, all directly connected nets or all nets connected
 *  with netname labels.
 *
 * \param [in] w_current  Pointer to GschemToplevel struct.
 * \param [in] o_net      Pointer to a single net object
 */
void
o_select_connected_nets(GschemToplevel *w_current, GedaObject *o_net)
{
  GedaToplevel *toplevel = w_current->toplevel;

  GList  *netstack     = NULL;
  GList  *netnamestack = NULL;

  const   GList *all_objects;
  const   GList *o_iter;
          GList *iter1;
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

    GList  *netnameiter;

    netnameiter = g_list_last(netnamestack);

    for (iter1 = g_list_last(netstack); iter1 != NULL;
         iter1 = iter1->prev, count++)
    {
      GedaObject *o_current = iter1->data;

      if (o_current->type == OBJ_NET && (!o_current->selected || count == 0))
      {
        o_select_object (w_current, o_current, SINGLE, count);

        if (w_current->net_selection_state > 1) {

          /* collect nets */
          netstack = g_list_concat(geda_struct_conn_return_others(NULL, o_current),
                                   netstack);
        }

        if (w_current->net_selection_state > 2) {

          /* collect netnames */
          netname = geda_attrib_search_object_by_name (o_current, "netname", 0);

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
    all_objects = geda_struct_page_get_objects (toplevel->page_current);

    for (o_iter = all_objects; o_iter != NULL; o_iter = o_iter->next) {

      GedaObject *o_current  = o_iter->data;

      if (o_current->type == OBJ_TEXT && o_current->attached_to != NULL) {

        if (o_current->attached_to->type == OBJ_NET) {

          GedaObject *parent = o_current->attached_to;

          netname = geda_attrib_search_object_by_name (parent, "netname", 0);

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

  if (w_current->net_selection_state > w_current->net_selection_mode) {
    w_current->net_selection_state = 1;
  }

  for (iter1 = netnamestack; iter1 != NULL; iter1 = iter1->next) {
    GEDA_FREE(iter1->data);
  }
  g_list_free(netnamestack);
}

/*!
 * \brief Get the Number of Currently Selected Objects
 * \par Function Description
 *  Returns the selection set count. Does not check validity of objects,
 *  this should have been done when the objects were added, nor does this
 *  function varify that the object or objects still exist.
 *
 * \returns count of the selection list..
 */
int
o_select_get_count(GschemToplevel *w_current)
{
  Page *page = gschem_toplevel_get_current_page(w_current);

  return !page ? 0 : g_list_length(geda_page_get_selection(page));
}

/*!
 * \brief Check if any Currently Select Objects
 * \par Function Description
 *
 * \returns TRUE if the selection list is not empty,
 *          FALSE if the selection list is empty,
 */
bool
o_select_is_selection(GschemToplevel *w_current)
{
  Page *page = gschem_toplevel_get_current_page(w_current);

  return !page ? FALSE : (geda_page_get_selection(page) != NULL);
}

/*!
 * \brief UnSelect All Objects
 * \par Function Description
 *  Removes all members from the toplevel selection list and calls
 *  the run hook function, after the object(s) have been removed,
 *  pass a list of the object that were removed.
 *
 * \remarks WEH: Could it help to have a freeze/thaw wrapper
 *          around the do loop if count was many?
 */
void
o_select_unselect_all(GschemToplevel *w_current)
{
  if (o_select_is_selection(w_current)) {

    Page      *p_current;
    SELECTION *selection;

    p_current = gschem_toplevel_get_current_page(w_current);
    selection = p_current->selection_list;

    if (g_list_length(geda_list_get_glist (selection)) > 1) {

      if (geda_object_selection_unselect_all(selection) > 0) {

        GList *list = geda_list_get_glist (selection);

        g_hook_run_object_list (w_current, DESELECT_OBJECTS_HOOK, list);
      }

      geda_list_remove_all(selection);
    }
    else {

      GedaObject *object = geda_list_get_glist (selection)->data;

      if (geda_object_selection_remove(selection, object) == 1) {
        o_select_run_hooks (w_current, object, DESELECT_OBJECTS_HOOK);
      }
    }
  }
}

/*!
 * \brief Selects all visible objects on the current page
 * \par Function Description
 *  Clears any existing selection, then selects everything visible and
 *  unlocked on the current page, and any attached attributes whether
 *  visible or invisible..
 *
 * \param [in] w_current  Pointer to a GschemToplevel structure
 */
void
o_select_visible_unlocked (GschemToplevel *w_current)
{
  GedaToplevel  *toplevel  = w_current->toplevel;
  SELECTION     *selection = toplevel->page_current->selection_list;
  const GList   *all_objects;
  const GList   *iter;
        GList   *added;

  o_select_unselect_all (w_current);

  all_objects = geda_struct_page_get_objects (toplevel->page_current);

  for (iter = all_objects; iter != NULL; iter = iter->next) {

    GedaObject *obj = (GedaObject*) iter->data;

    /* Skip invisible objects. */
    if (!geda_object_get_is_visible (obj) && !toplevel->page_current->show_hidden_text)
      continue;

    /* Skip locked objects. */
    if (!obj->selectable) continue;

      /* Add object to selection. */
      /*! \internal We can't call o_select_object() because it behaves
       *  differently depending on the state of w_current->SHIFTKEY
       *  and w_current->CONTROLKEY, which may well be set if this
       *  function is called via a keystroke (e.g. Ctrl-A). */
       geda_object_selection_add (selection, obj);

    /* Add any attributes of object to selection as well. */
    o_attrib_attached_2_selection (w_current, selection, obj);
  }

  /* Run hooks for all items selected */
  added = geda_list_get_glist (selection);

  if (added != NULL) {
    g_hook_run_object_list (w_current, SELECT_OBJECTS_HOOK, added);
  }
}

/*!
 * \brief Move Selection to Place List
 * \par Function Description
 *  Releases objects currently in the place list and copies
 *  the current selection list to the place list
 */
void
o_select_move_to_place_list(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  GList *selection;
  GList *selection_copy;

  /* remove the old place list if it exists */
  geda_struct_object_release_objects(toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  /* get selection list and copy to the place list */
  selection      = geda_list_get_glist(toplevel->page_current->selection_list);
  selection_copy = g_list_copy(selection);

  toplevel->page_current->place_list = selection_copy;
}

/*!
 * \brief Create list of selected objects of the given type
 * \par Function Description
 *  This is a general utility function that is like get selection except
 *  this function returns a list containing only objects of the specified
 *  type.
 *
 * \param w_current pointer to GschemToplevel context
 * \param otype     An Object type Object, not checked.
 *
 * \returns Glist* list of selected object or NULL is no object of the
 *                 specified type are selected.
 *
 * \note Caller should g_list_free returned list
 */
GList*
o_select_get_list_selected(GschemToplevel *w_current, char otype)
{
  GList  *iter;
  GList  *list;
  GList  *selection;     /* will be obtimized out */

  list = NULL;

  /* Get the current selection list, alias Current_Selection->glist */
  selection = geda_list_get_glist(Current_Selection);

  for (iter = selection; iter != NULL; iter = iter->next) {

    GedaObject *object = (GedaObject*) iter->data;

    if ( object->type == otype) {
      list = g_list_append (list, object);
    }
  }
  return list;
}

/*!
 * \brief Return the First Selected Object
 * \par Function Description
 *  This is a wrapper for o_selection_get_first_object, which
 *  references the <B>current page</B> selection list utilizing
 *  gschem_toplevel_get_current_page, which validates w_current
 *  and w_current->toplevel.
 *
 * \param w_current Pointer to a Gschem Toplevel object
 */
GedaObject * o_select_return_first_object(GschemToplevel *w_current)
{
  Page *page = gschem_toplevel_get_current_page(w_current);

  if (!page)
    return NULL;

  return geda_object_selection_get_first(page->selection_list);
}

static unsigned press_hid;
static unsigned release_hid;
static void (* verb) (GschemToplevel *w_current);

/*!
 * \brief Secondary Selection button Press events callback
 * \par Function Description
 * \internal Callback for button Press events when selecting
 *  objects for the secondary selection set.
 */
static int
o_select_press_butt(GtkWidget *widget, GdkEventButton *event,
                                       GschemToplevel *w_current)
{
  if (event->button == 1) {

    int  x, y;

    if (w_current->event_state == SELECT) {

      i_status_action_start(w_current);

      SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

      w_current->first_wx      = w_current->second_wx = x;
      w_current->first_wy      = w_current->second_wy = y;
      w_current->event_state   = STARTSELECT;
    }
  }
  return(0);
}

/*!
 * \brief Secondary Selection button Release events callback
 * \par Function Description
 * \internal Callback for button release events when selecting
 *  objects for the secondary selection set.
 */
static int
o_select_release_butt(GtkWidget *widget, GdkEventButton *event,
                                         GschemToplevel *w_current)
{
  if (event->button == 1) {

    int  x, y;

    SCREENtoWORLD (w_current, (int) event->x, (int) event->y, &x, &y);

    if (w_current->event_state == STARTSELECT) {
      if (o_find_object(w_current, x, y, TRUE)) {
        w_current->event_state = SELECT;
      }
    }
    else if (w_current->event_state == SBOX) {
      o_select_box_end(w_current, x, y);
      w_current->event_state = SELECT;
    }
  }
  else if (event->button == 3) {
    o_select_cancel_events(w_current);
    if (verb) {
      verb(w_current);
      verb = NULL;
    }
  }
  return(0);
}

/*!
 * \brief Cancel Secondary Selection Handler
 * \par Function Description
 *  Disconnects signals from the Secondary Selection Handler and unblock
 *  the previous button event handlers.
 *
 * \param [in] w_current  The GschemToplevel object.
 *
 * \sa i_event_stop_action_handler
 */
void o_select_cancel_events(GschemToplevel *w_current)
{
  if (press_hid) {
    g_signal_handler_disconnect (w_current->drawing_area, press_hid);
    press_hid = 0;
  }

  if (release_hid) {
    g_signal_handler_disconnect (w_current->drawing_area, release_hid);
    release_hid = 0;
  }

  i_event_unblock_buttons (w_current);
}

/*!
 * \brief Enable Secondary Selection Event Handler
 * \par Function Description
 * \internal Function blocks the current button event handlers and
 *  enables the signals to be sent to the Secondary Selection Event
 *  Handler.
 *
 * \param [in] w_current  The GschemToplevel object.
 *
 * \sa o_select_connect_selector
 */
static void o_select_enable_events(GschemToplevel *w_current)
{
  i_event_block_buttons (w_current);

  press_hid   = g_signal_connect (w_current->drawing_area,
                                  "button_press_event",
                                  G_CALLBACK(o_select_press_butt),
                                  w_current);

  release_hid = g_signal_connect (w_current->drawing_area,
                                  "button_release_event",
                                  G_CALLBACK(o_select_release_butt),
                                  w_current);
}

/*!
 * \brief Connect Secondary Selection Event Handler
 * \par Function Description
 *  This function allow routines to let user pick a second set of objects.
 *  The current select should be stored to w_current->primary_selection
 *  prior to calling this function. The secondary selector event handler
 *  cancels the mode after the user finished the selection, restoring the
 *  normal event handler.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] func       Current x coordinate of pointer in world units.
 */
void o_select_connect_selector (GschemToplevel *w_current, geda_predicator func)
{
  o_select_cancel_events(w_current);
  o_select_enable_events(w_current);

  verb = func;
  w_current->event_state = SELECT;
}
