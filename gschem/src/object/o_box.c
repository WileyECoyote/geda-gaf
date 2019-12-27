/* -*- C o_box.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_box.c
 * \brief Low-level module for manipulating Box objects
 */

#include <gschem.h>
#include <gschem_macros.h>
#include <geda_debug.h>

/*! \brief Initialize Variables to input a new box
 *  \par Function Description
 *  This function initializes variables to input a new box. Parameters for the
 *  box are put into/extracted from the <B>w_current</B> toplevel structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the pointer in world
 *  coordinates.
 *
 *  The first step is to input one corner of the box. The point given by
 *  (<B>w_x</B>,<B>w_y</B>) is snapped to the grid and saved as the first
 *  corner in <B>w_current->first_wx</B> and <B>w_current->first_wy</B>.
 *
 *  The other corner will be saved in (<B>w_current->second_wx</B>,
 *  <B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] w_x        Current x coordinate of pointer in world
 *  \param [in] w_y        Current y coordinate of pointer in world
 */
static void o_box_init(GschemToplevel *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe box */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  /* start to draw the box */
  w_current->rubber_visible = TRUE;
}

/*! \brief Draw temporary box
 *  \par Function Description
 *  This function draws the box from the variables in the GschemToplevel
 *  structure <B>*w_current</B>.
 *  One corner of the box is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and the second corner is at
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  \param [in] w_current  The GschemToplevel object.
 */
void o_box_draw_rubber (GschemToplevel *w_current)
{

    int width = geda_object_style_get_line_width(w_current->toplevel);
  cairo_t *cr = eda_renderer_get_cairo_context (CairoRenderer);
  GArray *color_map = eda_renderer_get_color_map (CairoRenderer);
  int flags = eda_renderer_get_cairo_flags (CairoRenderer);

  eda_cairo_box (cr, flags, width, w_current->first_wx, w_current->first_wy,
                 w_current->second_wx, w_current->second_wy);
  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, width, -1, -1);
}

/*! \brief End the input of a box.
 *  \par Function Description
 *  This function ends the input of the second corner of a box.
 *  The (<B>w_x</B>,<B>w_y</B>) point is set to be this second corner. The box is
 *  then defined by (<B>w_current->first_wx</B>,<B>w_current->first_wy</B> and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *  <B>w_x</B> and <B>w_y</B> are in screen unit.
 *
 *  The temporary box is erased ; a new box object is allocated, initialized
 *  and linked to the object list ; The object is finally drawn on the
 *  current sheet.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] w_x        Current x coordinate of pointer in world units
 *  \param [in] w_y        Current y coordinate of pointer in world units
 */
static void o_box_end(GschemToplevel *w_current, int w_x, int w_y)
{
  int  box_width, box_height;
  int  box_left,  box_top;

  i_status_action_stop(w_current);
  w_current->rubber_visible = FALSE;

  /* get the last coords of the pointer */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  box_width  = GET_BOX_WIDTH (w_current);
  box_height = GET_BOX_HEIGHT(w_current);
  box_left   = GET_BOX_LEFT  (w_current);
  box_top    = GET_BOX_TOP   (w_current);

  /* boxes with null width or height are not allowed */
  if ((box_width == 0) || (box_height == 0)) {

    /* cancel the object creation */
    o_box_invalidate_rubber (w_current);
    w_current->first_wx  = (-1);
    w_current->first_wy  = (-1);
    w_current->second_wx = (-1);
    w_current->second_wy = (-1);
  }
  else {

    GedaToplevel *toplevel = w_current->toplevel;
    GedaObject   *new_obj;

    /* create the object */
    new_obj = geda_box_object_new (GRAPHIC_COLOR,
                                   box_left,  box_top,
                                   box_left + box_width,
                                   box_top -  box_height);

    new_obj->line_options->line_width =  geda_object_style_get_line_width(toplevel);

    geda_struct_page_append_object (toplevel->page_current, new_obj);

#if DEBUG
    printf("coords: %d %d %d %d\n", box_left, box_top, box_width, box_height);
#endif

    /* Call add-objects-hook */
    g_hook_run_object (w_current, ADD_OBJECT_HOOK, new_obj);

    o_undo_savestate_object(w_current, UNDO_ALL, new_obj);

    w_current->first_wx  = (-1);
    w_current->first_wy  = (-1);
    w_current->second_wx = (-1);
    w_current->second_wy = (-1);
  }
}

/*! \brief Invalidate Temporary drawing artifacts for Box objects
 *  \par Function Description
 *   Get coordinates from top-level and invalidate the bounding
 *   region of a Box object.
 */
void o_box_invalidate_rubber (GschemToplevel *w_current)
{
  int x1, y1, x2, y2;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  o_invalidate_rectangle (w_current, x1, y1, x2, y1);
  o_invalidate_rectangle (w_current, x1, y1, x1, y2);
  o_invalidate_rectangle (w_current, x2, y1, x2, y2);
  o_invalidate_rectangle (w_current, x1, y2, x2, y2);
}

/*! \brief Draw temporary box while dragging edge.
 *  \par Function Description
 *  This function is used to draw the box while dragging one of its edge or
 *  angle. It erases the previous temporary box drawn before, and draws a new
 *  updated one. <B>w_x</B> and <B>w_y</B> are the new position of the mobile point,
 *  ie the mouse.
 *
 *  The old values are inside the <B>w_current</B> pointed structure. Old width,
 *  height and left and top values are recomputed by the corresponding macros.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_box_motion (GschemToplevel *w_current, int w_x, int w_y)
{

  if (w_current->inside_action == 0) {
    BUG_MSG("Not inside action");
  }
  else {

    /* erase the previous temporary box if it is visible */
    if (w_current->rubber_visible) {
      o_box_invalidate_rubber (w_current);
    }

    /*
     * New values are fixed according to the <B>w_x</B> and <B>w_y</B>
     * parameters. These are saved in <B>w_current</B> pointed structure
     * as new temporary values. The new box is then drawn.
     */

    /* update the coords of the corner */
    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    /* draw the new temporary box */
    o_box_invalidate_rubber (w_current);
    w_current->rubber_visible = 1;
  }
}

/*! \brief Start process to input a new box
 *  \par Function Description
 *  This function starts the process to input a new box.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] w_x        Current x coordinate of pointer in world
 *  \param [in] w_y        Current y coordinate of pointer in world
 */
void o_box_start(GschemToplevel *w_current, int w_x, int w_y)
{
  o_box_init(w_current, w_x, w_y);

  i_event_start_adder_handler(w_current, o_box_init, o_box_end);
}
