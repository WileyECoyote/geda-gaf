/* -*- C o_find.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_find.c
 * \brief Low-level module for finding strings in Text objects
 */
#include <gschem.h>
#include <geda_debug.h>

/*! \brief Tests a if a given Object was hit at a given set of coordinates
 *
 *  \par Function Description
 *  Tests a if a given Object was hit at a given set of coordinates. If an
 *  object is not selectable (e.g. it is locked), or it is invisible and
 *  not being rendered, this function will return FALSE.
 *
 *  \param [in] w_current         The GschemToplevel object.
 *  \param [in] object            The Object being hit-tested.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] w_slack           The slack applied to the hit-test.
 *
 *  \returns TRUE if the Object was hit, otherwise FALSE.
 */
static bool is_object_hit (GschemToplevel *w_current, Object *object,
                           int w_x, int w_y, int w_slack)
{
  int left, top, right, bottom;

  if (!object->selectable)
    return FALSE;

  /* We can't hit invisible text objects unless show_hidden_text is active. */
  //if (!o_get_is_visible (object) && !Current_Page->show_hidden_text)
  if (!o_get_is_visible (object))
    return FALSE;

  /* Do a coarse test first to avoid computing distances for objects ouside
   * of the hit range.
   */
  if (!o_get_world_bounds(object, &left, &top, &right, &bottom) ||
      !o_get_is_inside_region(left  - w_slack, top    - w_slack,
                              right + w_slack, bottom + w_slack,
                              w_x, w_y))
    return FALSE;

  return (o_get_shortest_distance (object, w_x, w_y) < w_slack);
}

/*! \brief Tests a if a given Object was hit at a given set of coordinates
 *  \par Function Description
 *  Tests a if a given Object was hit at a given set of coordinates. If so,
 *  processes selection changes as appropriate for the object and passed
 *  flag. Saves a pointer to the found object so future find operations
 *  resume after this object.
 *
 *  \param [in] w_current         The GschemToplevel object.
 *  \param [in] object            The Object being hit-tested.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] w_slack           The slack applied to the hit-test.
 *  \param [in] change_selection  Whether to select the found object or not.
 *  \returns TRUE if the Object was hit, otherwise FALSE.
 *
 *  \remark WEH 07/23/13: This function is ONLY called by the o_find_object
 *  function below.
 */
static bool
find_single_object (GschemToplevel *w_current, Object *object,
                    int w_x, int w_y, int w_slack,
                    int change_selection)
{
  if (!is_object_hit (w_current, object, w_x, w_y, w_slack))
    return FALSE;

  if (change_selection) {
    if (object->type == OBJ_NET && w_current->net_selection_mode)
      o_select_connected_nets (w_current, object);
    else
      o_select_object (w_current, object, SINGLE, 0); /* 0 is count */
  }

  w_current->toplevel->page_current->object_lastplace = object;
  i_status_update_sensitivities (w_current);
  return TRUE;
}

/*! \brief Find an Object at a given set of coordinates
 *
 *  \par Function Description
 *  Tests for ObjectS hit at a given set of coordinates. If
 *  change_selection is TRUE, it updates the page's selection.
 *
 *  Find operations resume searching after the last object which was
 *  found, so multiple find operations at the same point will cycle
 *  through any objects on top of each other at this location.
 *
 *  \param [in] w_current         The GschemToplevel object.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] change_selection  Whether to select the found object or not.
 *  \returns TRUE if the object was hit at the given coordinates,
 *           otherwise FALSE.
 */
bool o_find_object (GschemToplevel *w_current, int w_x, int w_y,
                        bool change_selection)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int w_slack;
  const GList *iter = NULL;

  w_slack = WORLDabs (w_current, w_current->select_slack_pixels);

  /* Decide whether to iterate over all object or start at the last
     found object. If there is more than one object below the
     (w_x/w_y) position, this will select the next object below the
     position point. You can change the selected object by clicking
     at the same place multiple times. */
  if (toplevel->page_current->object_lastplace != NULL) {
    /* NB: g_list_find doesn't declare its input const, so we cast */
    iter = g_list_find ((GList *)s_page_get_objects (toplevel->page_current),
                        toplevel->page_current->object_lastplace);
    iter = g_list_next (iter);
  }

  /* do first search (if we found any objects after the last found object) */
  while (iter != NULL) {
    Object *o_current = iter->data;
    if (find_single_object (w_current, o_current,
                            w_x, w_y, w_slack, change_selection)) {
      return TRUE;
    }
    iter = g_list_next (iter);
  }

  /* now search from the beginning up until the object_lastplace */
  for (iter = s_page_get_objects (toplevel->page_current);
       iter != NULL; iter = g_list_next (iter)) {
    Object *o_current = iter->data;
    if (find_single_object (w_current, o_current,
                            w_x, w_y, w_slack, change_selection)) {
      return TRUE;
    }
    /* break once we've inspected up to where we started the first loop */
    if (o_current == toplevel->page_current->object_lastplace)
      break;
  }

  /* didn't find anything.... reset lastplace */
  toplevel->page_current->object_lastplace = NULL;

  /* Deselect everything if change_selection flag is True AND
   * the shift key or the control isn't pressed */
  if (change_selection && ( !(w_current->SHIFTKEY || w_current->CONTROLKEY))) {
    o_select_unselect_all (w_current);
  }

  i_status_update_sensitivities(w_current);
  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
Object *o_find_selected_object (GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int w_slack = WORLDabs (w_current, w_current->select_slack_pixels);
  GList *s_current;

  for (s_current = geda_list_get_glist (toplevel->page_current->selection_list);
       s_current != NULL; s_current = g_list_next (s_current)) {
    Object *o_current = s_current->data;

    if (is_object_hit (w_current, o_current, w_x, w_y, w_slack))
      return o_current;
  }

  return NULL;
}
