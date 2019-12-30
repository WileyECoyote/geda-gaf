/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: o_find.c
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 */
/*!
 * \file o_find.c
 * \brief Low-level module for finding Objects on the Current Page
 */
#include <gschem.h>
#include <geda_debug.h>

/*!
 * \brief Tests a if a given Object was hit at a given set of coordinates
 * \par Function Description
 *  Tests a if a given Object was hit at a given set of coordinates.
 *
 * \param [in] w_current    The GschemToplevel object
 * \param [in] object       The Object being hit-tested
 * \param [in] wx           The X coordinate to test (in world coords)
 * \param [in] wy           The Y coordinate to test (in world coords)
 * \param [in] w_slack      The slack applied to the hit-test
 *
 * \returns TRUE if the Object was hit, otherwise FALSE.
 */
inline static bool o_find_is_object_hit (GschemToplevel *w_current,
                                         GedaObject *object,
                                         int wx, int wy, int w_slack)
{
  int left, top, right, bottom;

  /* Do a coarse test first to avoid computing distances for objects ouside
   * of the hit range.
   */
  if (!geda_object_get_bounds(object, &left, &top, &right, &bottom) ||
      !geda_object_get_is_inside_region(left  - w_slack, top    - w_slack,
                                        right + w_slack, bottom + w_slack,
                                        wx, wy))
    return FALSE;

  /* Else */
  return (geda_object_get_shortest_distance_full (object, wx, wy, FALSE) < w_slack);
}

/*!
 * \brief Disposition objects found by o_find_object
 * \par Function Description
 *  Handles dispositing of any found objects by o_find_object. The object
 *  is added to or replaces the current selection depending on the mode
 *  flag, if \a mode is add or replace the object is not selected. If
 *  the selection is modified sensitivities are updated.
 *
 * \param [in] w_current    The GschemToplevel object
 * \param [in] object       The Object being hit-tested
 * \param [in] mode         Whether to select the found object or not
 *
 * \returns TRUE if the Object was hit, otherwise FALSE.
 */
static void o_find_disposition_object (GschemToplevel *w_current,
                                       GedaObject     *object,
                                       int mode)
{
  if (mode == SELECTION_REPLACE) {

    if (geda_get_object_type(object) == OBJ_NET &&
        w_current->net_selection_mode) {
      o_select_connected_nets (w_current, object);
    }
    else {
      o_select_object (w_current, object, SINGLE, 0); /* 0 is count */
    }
    i_status_update_sensitivities(w_current);
  }
  else if (mode == SELECTION_ADD) {
    o_select_add_object(w_current, object);
    i_status_update_sensitivities(w_current);
  }

  Current_Page->object_lastplace = object;
}

/*!
 * \brief Find an Object at a given set of coordinates
 * \par Function Description
 *  Tests for ObjectS hit at a given set of coordinates. Objects that
 *  are not selectable (e.g. it is locked), or are invisible are not
 *  tested. The object is neither added nor appended to the current
 *  selection and sensitivities are not updated.
 *
 * \param [in] w_current   The GschemToplevel object
 * \param [in] x           The X coordinate to test (in world coords)
 * \param [in] y           The Y coordinate to test (in world coords)
 *
 * \returns TRUE if the Object was hit, otherwise FALSE.
 *
 * \sa o_find_object
 */
GedaObject *o_find_get_hit (GschemToplevel *w_current, int x, int y)
{
  GList      *iter;
  GedaObject *object = NULL;
  int        w_slack = WORLDabs (w_current, w_current->select_slack_pixels);

  for (iter = geda_struct_page_get_objects (Current_Page); iter; NEXT(iter)) {

   GedaObject *o_current = iter->data;

    if (o_current->selectable && o_current->visibility > 0) {

      if (o_find_is_object_hit (w_current, o_current, x, y, w_slack)) {
        Current_Page->object_lastplace = o_current;
        object = o_current;
        break;
      }
    }
  }

  return object;
}

/*!
 * \brief Find an Object at a given set of coordinates
 * \par Function Description
 *  Tests for objects hit at a given set of coordinates. Objects that
 *  are not selectable (e.g. it is locked), or are invisible are not
 *  tested. The Find operations resume searching after the last object
 *  which was found, so multiple find operations at the same point will
 *  cycle through any objects on top of each other near a given location.
 *
 * \param [in] w_current    The GschemToplevel object
 * \param [in] wx           The X coordinate to test (in world coords)
 * \param [in] wy           The Y coordinate to test (in world coords)
 * \param [in] mode         Whether to select the found object or not
 *
 * \returns TRUE if the object was hit at the given coordinates,
 *          otherwise FALSE.
 *
 * \sa o_find_disposition_object
 */
bool o_find_object (GschemToplevel *w_current, int wx, int wy, int mode)
{
  const GList *iter = NULL;
  const GList *list;

  bool  found;
  int   w_slack;

  found   = FALSE;
  list    = geda_struct_page_get_objects (Current_Page);
  w_slack = WORLDabs (w_current, w_current->select_slack_pixels);

  /* Decide whether to iterate over all object or start at the last
     found object. If there is more than one object below the (wx,wy)
     this will select the next object below the position point, users
     can change the selected object by clicking at the same place
     multiple times. */
  if (Current_Page->object_lastplace != NULL) {

    iter = list;

    while (iter) {

      if (iter->data == Current_Page->object_lastplace) {

        iter = iter->next;     /* Skip over this object, is last */

        /* Start searching from remainder of list */
        while (iter) {

          GedaObject *object = iter->data;

          if (object->visibility && (object->selectable || w_current->ALTKEY))
          {
            if (o_find_is_object_hit (w_current, object, wx, wy, w_slack)) {
              o_find_disposition_object (w_current, object, mode);
              found = TRUE;
              break;           /* Break-out from inner while loop */
            }
          }
          iter = iter->next;
        }
        break;                 /* Break-out from outer while loop */
      }
      iter = iter->next;
    }
  }

  /* If not found start Search from the beginning until the object_lastplace */
  if (!found) {

    for (iter = list; iter; iter = iter->next) {

      GedaObject *object = iter->data;

      if (object->visibility && (object->selectable || w_current->ALTKEY))
      {
        if (o_find_is_object_hit (w_current, object, wx, wy, w_slack)) {
          o_find_disposition_object (w_current, object, mode);
          found = TRUE;
          break;           /* Break-out from for loop found == TRUE */
        }
      }

      /* Break once we have inspected up to where we started the first loop */
      if (object == Current_Page->object_lastplace) {
        break;                /* Break-out from for loop found == FALSE */
      }
    }
  }

  if (!found) {

    /* We did not find anything so reset lastplace */
    Current_Page->object_lastplace = NULL;

    /* Deselect everything if selection mode flag is SELECTION_REPLACE
     * AND the shift key or the control is not pressed */
    if (mode == SELECTION_REPLACE) {
      if (!(w_current->SHIFTKEY || w_current->CONTROLKEY)) {
        o_select_unselect_all (w_current);
        i_status_update_sensitivities(w_current);
      }
    }
  }

  return found;
}

/*!
 * \brief Determine if Coordinates within Object's Insertion Grip Region
 * \par Function Description
 *  Returns TRUE if the given coordinates are within the grip region
 *  about the insertion point of the object.
 *  Currently, this is only relavent to drag-moving circles.
 */
inline static bool o_find_is_object_grip_hit (GschemToplevel *w_current,
                                              GedaObject     *object,
                                              int wx, int wy)
{
  bool hit;
  int owx, owy;

  hit = FALSE;

  if (geda_object_get_position(object, &owx, &owy)) {

    int xmin, ymin, xmax, ymax;

    register int half_size = gschem_toplevel_get_grips_half_size (w_current);

    xmin = owx - half_size;
    xmax = owx + half_size;

    ymin = owy - half_size;
    ymax = owy + half_size;

    return geda_object_get_is_inside_region(xmin, ymin, xmax, ymax, wx, wy);
  }

  return hit;
}

/*!
 * \brief Find Selected Object at a given set of coordinates
 * \par Function Description
 *  Return first object in the current selection that can be hit at the
 *  given coordinates or NULL if no such object is found.
 */
GedaObject *o_find_selected_object (GschemToplevel *w_current, int wx, int wy)
{
  int w_slack = WORLDabs (w_current, w_current->select_slack_pixels);
  GList *iter;

  for (iter = geda_list_get_glist (Current_Selection); iter; NEXT(iter)) {

    GedaObject *o_current = iter->data;

    if (o_find_is_object_hit (w_current, o_current, wx, wy, w_slack)) {
      return o_current;
    }

    if (o_find_is_object_grip_hit (w_current, o_current, wx, wy)) {
      return o_current;
    }
  }

  return NULL;
}
