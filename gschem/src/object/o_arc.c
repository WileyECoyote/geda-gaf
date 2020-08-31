/* -*- C o_arc.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_arc.c
 * \brief Low-level module for manipulating Arc objects
 */

#include <gschem.h>
#include <math.h>
#include <geda_debug.h>

/*!
 * \brief Draw arc from GschemToplevel object.
 * \par Function Description
 *  This function draws an arc from the variables in the GschemToplevel
 *  structure <B>*w_current</B>.
 *  The center of the arc is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>), its radius equal to <B>w_current->distance</B>,
 *  and the start and end angle are given by <B>w_current->second_wx</B> and
 *  <B>w_current->second_wy</B>.
 *
 * \param [in] w_current  The GschemToplevel object.
 */
void o_arc_draw_rubber (GschemToplevel *w_current)
{
  double rad_angle;
  int rdx, rdy;
  double wwidth = 0;

  int x1 = w_current->first_wx;
  int y1 = w_current->first_wy;
  int x2 = w_current->second_wx;
  int y2 = w_current->second_wy;

  cairo_t *cr       = eda_renderer_get_cairo_context (CairoRenderer);
  GArray *color_map = eda_renderer_get_color_map (CairoRenderer);
  int flags         = eda_renderer_get_cairo_flags (CairoRenderer);

  eda_cairo_arc (cr, flags, wwidth, x1, y1, w_current->distance, x2, y2);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);

  /* draw the radius line */
  rad_angle = ((double) x2) * M_PI / 180;
  rdx = (double) w_current->distance * cos (rad_angle);
  rdy = (double) w_current->distance * sin (rad_angle);

  eda_cairo_line (cr, flags, END_NONE, wwidth, x1, y1, x1 + rdx, y1 + rdy);

  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}

/*!
 * \brief End the input of an arc.
 * \par Function Description
 *  This function ends the input of the radius of the arc. The
 *  (<B>w_x</B>,<B>w_y</B>) point in world coords, is the other
 *  end of the radius segment. The distance between this point
 *  and the center is the radius of the arc. The center of the
 *  arc is at (<B>w_current->first_wx</B>, <B>w_current->first_wy</B>)
 *  and the radius is <B>w_current->distance</B>, was computed
 *  in o_arc_motion.
 *
 *  The two angles needs to be input to fully define the arc.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        (unused)
 * \param [in] w_y        (unused)
 */
static void o_arc_end1(GschemToplevel *w_current, int w_x, int w_y)
{
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  i_status_action_stop(w_current);
  w_current->rubber_visible = FALSE;

  /* ack! zero length radius */
  if (w_current->distance != 0) {

    /* Open dialog to input the start and end angle */
    x_dialog_edit_arc_angle(w_current, NULL);

  }
}

/*!
 * \brief Ends the process of arc input.
 * \par Function Description
 *  The #o_arc_end4() function ends the process of the input of an arc.
 *  <B>start_angle</B> and <B>arc_sweep</B> are the start and end angle
 *  of the arc in degrees. The partial internal representation of the arc,
 *  i.e. the center and the radius of the arc, are converted in world units.
 *  A new object is created and linked to the object list.
 *
 * \param [in] w_current    The GschemToplevel object.
 * \param [in] radius       Radius of the arc
 * \param [in] start_angle  Start of angle in degrees.
 * \param [in] arc_sweep    End of angle in degrees.
 */
void o_arc_end4(GschemToplevel *w_current, int radius, int start_angle, int arc_sweep)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GedaObject   *new_obj;

  /* create, initialize and link the new arc object */
  new_obj = geda_arc_object_new (GRAPHIC_COLOR,
                                 w_current->first_wx, w_current->first_wy,
                                 radius, start_angle, arc_sweep);
  new_obj->line_options->line_width =  geda_object_style_get_line_width(toplevel);
  geda_struct_page_append_object (toplevel->page_current, new_obj);

  /* Call add-objects-hook */
  g_hook_run_object (w_current, ADD_OBJECT_HOOK, new_obj);

  o_undo_savestate_object(w_current, UNDO_ALL, new_obj);

  w_current->first_wx = -1;
  w_current->first_wy = -1;
  w_current->distance =  0;
}

/*!
 * \brief Initialize Variables to input a new Arc
 * \par Function Description
 *  This function initializes variables to input a new Arc. Parameters for the
 *  Arc are stored in the <B>w_current</B> toplevel structure. <B>w_x</B> and
 *  <B>w_y</B> are current coordinates of the pointer in world coordinates.
 *
 * \param [in] w_current  The GschemToplevel object
 * \param [in] w_x        Current x coordinate of pointer in world
 * \param [in] w_y        Current y coordinate of pointer in world
 */
static void o_arc_init(GschemToplevel *w_current, int w_x, int w_y)
{
  /* set the center of the arc */
  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  /* set the radius */
  w_current->distance = 0;

  /* set the start and end angles */
  w_current->second_wx      = w_current->second_wy = 0;

  w_current->rubber_visible = TRUE;
  w_current->which_grip     = ARC_RADIUS;
}

/*!
 * \brief Invalidate Temporary drawing artifacts for Arc objects
 * \par Function Description
 *  Retrieves coordinates from top-level and invalidate the bounding
 *  region of a Arc object.
 */
void o_arc_invalidate_rubber (GschemToplevel *w_current)
{
  int wx, wy, cx, cy, radius;

  wx = w_current->first_wx;
  wy = w_current->first_wy;

  WORLDtoSCREEN (w_current, wx, wy, &cx, &cy);
  radius = SCREENabs (w_current, w_current->distance);

  o_invalidate_rectangle (w_current, cx - radius, cy - radius,
                                     cx + radius, cy + radius);
}

/*!
 * \brief Draw an arc using one angle modification.
 * \par Function Description
 *  This function draws an arc according to its internal representation
 *  and allows the modification of one of its angle. The start or end
 *  angle of the arc is updated according to <B>whichone</B> with the angle
 *  that the current pointer and the arc center are making with the horizontal.
 *
 *  The previous temporary arc is erased, the angle is then computed
 *  and updated and finally a new temporary arc with the new angle is drawn.
 *
 *  The arc is internally described by :
 *  <DL>
 *    <DT>*</DT><DD>(<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) as
 *                   its center.
 *    <DT>*</DT><DD><B>w_current->distance</B> as its radius.
 *    <DT>*</DT><DD><B>w_current->second_wx</B> and <B>w_current->second_wx</B> as its
 *                  start and end angle respectively.
 *  </DL>
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        Current x coordinate of pointer in world units.
 * \param [in] w_y        Current y coordinate of pointer in world units.
 *
 *  <B>whichone</B> can have one of the following values:
 *  <DL>
 *    <DT>ARC_RADIUS</DT>
 *    <DD>at the center of the arc. This grip is used to modify
 *        the radius of the arc.
 *    <DT>ARC_START_ANGLE</DT>
 *    <DD>at one end of the arc. It corresponds to the starting
 *        angle of the arc.
 *    <DT>ARC_END_ANGLE</DT>
 *    <DD>at the other end of the arc. It corresponds to the
 *        ending angle of the arc.
 *  </DL>
 */
void o_arc_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  int grip = w_current->which_grip;
  int diff_x, diff_y;

  /* erase the previous temporary arc */
  if (w_current->rubber_visible)
    o_arc_invalidate_rubber (w_current);

  if (grip == ARC_RADIUS) {

    /* The radius is taken as the largest distance on the x and y
     * axis between the center of the arc and the mouse position.
     */
    diff_x = abs(w_current->first_wx - snap_grid (w_current, w_x));
    diff_y = abs(w_current->first_wy - snap_grid (w_current, w_y));

    w_current->distance = max(diff_x, diff_y);
  }
  else if ((grip == ARC_START_ANGLE) || (grip == ARC_END_ANGLE)) {

    int angle_deg;

    /* compute the angle */
    diff_x = w_x - w_current->first_wx;
    diff_y = w_y - w_current->first_wy;
    angle_deg = atan2 (diff_y, diff_x) * 180 / M_PI;

    /* set the start or end angle with this angle */
    switch(grip) {
    case ARC_START_ANGLE:
      w_current->second_wx = (angle_deg + 360) % 360;
      break;

    case ARC_END_ANGLE:
      w_current->second_wy = (((angle_deg + 360) % 360) -
      w_current->second_wx + 360) % 360;

      if (w_current->which_object->arc->arc_sweep < 0) {
        w_current->second_wy = w_current->second_wy - 360;
      }

      if (w_current->second_wy == 0) {
        w_current->second_wy = 360;
      }
    }
  }

  /* Draw the new temporary arc */
  o_arc_invalidate_rubber (w_current);
  w_current->rubber_visible = TRUE;
}

/*!
 * \brief Start process to input a new arc.
 * \par Function Description
 *  This function starts the process to input a new arc. Parameters for
 *  this arc are put into/extracted from the <B>w_current</B> toplevel structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the pointer in screen unit.
 *
 *  First step of the arc input is to set the radius of the arc. The center
 *  of the arc is kept in (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>).
 *  The radius of the arc is in <B>w_current->distance</B>.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        Current x coordinate of pointer in world units.
 * \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_arc_start(GschemToplevel *w_current, int w_x, int w_y)
{
  o_arc_init(w_current, w_x, w_y);

  i_event_start_adder_handler(w_current, o_arc_init, o_arc_end1);
}