/* -*- C o_invalidate.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_invalidate.c
 * \brief Low-level module invalidate object and regions
 */

#include <gschem.h>
#include <geda_debug.h>

#define INVALIDATE_MARGIN 1

/*!
 * \brief Invalidate Temporary drawing artifacts
 * \par Function Description
 *  Calls appropriate handler if inside an action in order
 *  to remove temporary drawing objects from the screen.
 *  Note that there is no o_text_invalidate_rubber, text
 *  is entered into a dialog entry and is not "drawn".
 *
 * \return TRUE is something erased, otherwise FALSE.
 */
int
o_invalidate_rubber (GschemToplevel *w_current)
{
  if (!w_current->inside_action)
    return(FALSE);

  switch(w_current->event_state) {

    case ( NETMODE ):
      o_net_invalidate_rubber (w_current);
      break;

    case ( PINMODE ):
      o_pin_invalidate_rubber (w_current);
      break;

    case ( LINEMODE):
      o_line_invalidate_rubber (w_current);
      break;

    case ( BOXMODE ):
      o_box_invalidate_rubber (w_current);
      break;

    case ( CIRCLEMODE ):
      o_circle_invalidate_rubber (w_current);
      break;

    case ( ARCMODE ):
      o_arc_invalidate_rubber (w_current);
      break;

    case ( PATHMODE ):
      o_path_invalidate_rubber (w_current);
      break;

    case ( PICTUREMODE ):
      o_picture_invalidate_rubber (w_current);
      break;

    case ( BUSMODE ):
      o_bus_invalidate_rubber (w_current);
      break;

    default:
      return (FALSE);
      break;
  }

  return (TRUE);
}

/*!
 * \brief Invalidates a rectangular region of the on screen drawing area
 * \par Function Description
 *  Given a pair of (x,y) coordinates in SCREEN units, invalidate the
 *  rectangular on-screen drawing area which has those two coordinate
 *  pairs as opposite corners of its region. This will cause that region
 *  to be blitted from the back-buffer once the mainloop reaches idle.
 *
 *  A margin, INVALIDATE_MARGIN is added to the invalidated region as
 *  a hack workaround for rounding errors which may occur in the
 *  WORLD -> SCREEN coordinate transform. This margin may also be used
 *  to expand the invalidated region if anti-aliased drawing is ever
 *  used.
 *
 *  A further, larger margin is added to account for invalidating the
 *  size occupied by an object's grips.
 *
 *  If the GschemToplevel in question is not rendering to a GDK_WINDOW,
 *  (e.g. image export), this function call is a no-op. A test is used:
 *  GDK_IS_WINDOW(), which should be safe since in either case,
 *  w_current->window is a GObject. This is really a _HACK_,
 *  and should be fixed with a re-worked drawing model.
 *
 *  \param [in] w_current  The GschemToplevel who's drawing area is being
 *                         invalidated.
 *  \param [in] x1         X coord for corner 1 (SCREEN units)
 *  \param [in] y1         Y coord for corner 1 (SCREEN units)
 *  \param [in] x2         X coord for corner 2 (SCREEN units)
 *  \param [in] y2         Y coord for corner 2 (SCREEN units)
 */
void
o_invalidate_rectangle (GschemToplevel *w_current,
                        int x1, int y1, int x2, int y2)
{
  GdkRectangle rect;

#if DEBUG
  fprintf(stderr, "%s x1 %d, y1 %d, x2 %d, y2 %d\n", __func__, x1, y1, x2, y2);
#endif

  /* Ensure we only invalidate GdkWindows - probably wasting time here */
  if (GDK_IS_WINDOW(w_current->window)) {

    int grip_half_size;
    int cue_half_size;
    int bloat;

    grip_half_size = gschem_toplevel_get_grips_half_size (w_current);

    cue_half_size  = SCREENabs (w_current, CUE_BOX_SIZE);

    bloat = MAX (grip_half_size, cue_half_size) + INVALIDATE_MARGIN;

    rect.x = MIN(x1, x2) - bloat;
    rect.y = MIN(y1, y2) - bloat;

    rect.width  = 1 + abs( x1 - x2 ) + (bloat << 1);
    rect.height = 1 + abs( y1 - y2 ) + (bloat << 1);

    gdk_window_invalidate_rect (w_current->window, &rect, FALSE);
  }
}

/*!
 * \brief Invalidate the whole on-screen area
 * \par Function Description
 *  This function calls gdk_window_invalidate_rect() with a rect
 *  of NULL, causing the entire drawing area to be invalidated.
 *
 * \param [in] w_current  The GschemToplevel object.
 */
void
o_invalidate_all (GschemToplevel *w_current)
{
  if (w_current && GDK_IS_WINDOW(w_current->window)) {
    gdk_window_invalidate_rect (w_current->window, NULL, FALSE);
  }
}

/*!
 * \brief Invalidate on-screen area for an object
 * \par Function Description
 *  This function calls o_invalidate_rectangle(), after validating the
 *  bounds of the passed Object, converted to screen coordinates.
 *
 * \sa o_invalidate_force
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] object     The Object invalidated on screen.
 */
void
o_invalidate_object (GschemToplevel *w_current, GedaObject *object)
{
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;

  if (geda_object_get_bounds(object, &left,  &top, &right, &bottom)) {

    WORLDtoSCREEN (w_current, left, top, &s_left, &s_top);
    WORLDtoSCREEN (w_current, right, bottom, &s_right, &s_bottom);
    o_invalidate_rectangle (w_current, s_left, s_top, s_right, s_bottom);
  }
  else {
    if (geda_object_get_is_visible(object)) {
      fprintf(stderr, "%s could not get bounds <%s>\n", __func__, object->name);
    }
  }
}

/*!
 * \brief Force invalidate on-screen area for an object
 * \par Function Description
 *  This function calls o_invalidate_rectangle() with the bounds of the
 *  passed Object, converted to screen coordinates.
 *
 * \sa o_invalidate_object
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] object     The Object invalidated on screen.
 */
void
o_invalidate_force(GschemToplevel *w_current, GedaObject *object)
{
  int s_left, s_top, s_bottom, s_right;

  WORLDtoSCREEN (w_current, object->left,  object->top,    &s_left, &s_top);
  WORLDtoSCREEN (w_current, object->right, object->bottom, &s_right, &s_bottom);
  o_invalidate_rectangle (w_current, s_left, s_top, s_right, s_bottom);
}

/*!
 * \brief Invalidate on-screen area for a GList of objects
 * \par Function Description
 *  This function calls o_invalidate_rectangle() with the bounds of the
 *  passed GList, converted to screen coordinates.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] list       The glist objects invalidated on screen.
 */
void
o_invalidate_list (GschemToplevel *w_current, GList *list)
{
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;

  geda_set_object_list_invalid (list);

  if (geda_object_get_bounds_list (list, &left,  &top, &right, &bottom)) {
    WORLDtoSCREEN (w_current, left,  top,    &s_left, &s_top);
    WORLDtoSCREEN (w_current, right, bottom, &s_right, &s_bottom);
    o_invalidate_rectangle (w_current, s_left, s_top, s_right, s_bottom);
  }
}
