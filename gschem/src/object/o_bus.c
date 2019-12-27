/* -*- C o_bus.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_bus.c
 * \brief Low-level module for manipulating Bus objects
 */
#include <gschem.h>
#include <geda_debug.h>


/*!
 * \brief draw a rubber bus segment
 * \par Function Description
 *  This function draws a bus segment from the point (<B>first_wx</B>,
 *  <B>first_wy</B>) to the point (<B>second_wx</B>,<B>second_wy</B>)
 *  from the <B>GschemToplevel</B> structure.
 *
 *  The function can be used to draw or erase the rubber bus on the screen.
 *
 *  \param [in] w_current  The GschemToplevel object
 */
void o_bus_draw_rubber (GschemToplevel *w_current)
{
  int width = geda_object_style_get_bus_width(w_current->toplevel);

  cairo_t *cr = eda_renderer_get_cairo_context (CairoRenderer);
  GArray *color_map = eda_renderer_get_color_map (CairoRenderer);
  int flags = eda_renderer_get_cairo_flags (CairoRenderer);

  eda_cairo_line (cr, flags, END_NONE, width,
                  w_current->first_wx,  w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);
  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_ROUND, width, -1, -1);
}

/*!
 * \brief finish a bus drawing action
 * \par Function Description
 *  This function finishes a net drawing action. The function draws
 *  a bus from the point (<B>first_wx</B>,<B>first_wy</B>) to
 *  (<B>second_wx</B>,<B>second_wy</B>). Both points are taken from
 *  the <B>GschemToplevel</B> structure.
 *
 *  The function returns TRUE if a bus object has been created and
 *  FALSE if no bus object has been created.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        (unused)
 * \param [in] w_y        (unused)
 */
static void o_bus_end(GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int color;

  if (w_current->override_bus_color == -1) {
    color = BUS_COLOR;
  }
  else {
    color = w_current->override_bus_color;
  }

  /* set flag erase the rubber bus */
  w_current->rubber_visible = FALSE;

  /* don't allow zero length bus */
  /* this ends the bus drawing behavior we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy))
  {
    i_status_action_stop(w_current);
  }
  else
  {
    GedaObject *new_obj;
    GList      *prev_conn_objects;

    new_obj = geda_bus_object_new(color,
                                  w_current->first_wx, w_current->first_wy,
                                  w_current->second_wx, w_current->second_wy,
                                  0);

    new_obj->line_options->line_width = geda_object_style_get_bus_width(toplevel);
    geda_struct_page_append_object (toplevel->page_current, new_obj);

    /* connect the new bus to the other busses */
    prev_conn_objects = geda_struct_conn_return_others (NULL, new_obj);
    o_invalidate_list (w_current, prev_conn_objects);
    g_list_free (prev_conn_objects);

    /* Call add-objects-hook */
    g_hook_run_object (w_current, ADD_OBJECT_HOOK, new_obj);

    o_undo_savestate_object(w_current, UNDO_ALL, new_obj);

    w_current->first_wx = w_current->second_wx;
    w_current->first_wy = w_current->second_wy;
  }
}

/*!
 * \brief Initialize Variables to input a new bus
 * \par Function Description
 *  This function initializes variables to input a new bus object. The
 *  start point is stored in <B>first_wx</B>, <B>first_wy</B> in the
 *  <B>GschemToplevel</B> structure.
 *
 * \param [in] w_current  The GschemToplevel object
 * \param [in] w_x        Current x coordinate of pointer in world
 * \param [in] w_y        Current y coordinate of pointer in world
 */
static void o_bus_init(GschemToplevel *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe box */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  /* start to draw the box */
  w_current->rubber_visible = TRUE;
}

/*!
 * \brief Invalidate Temporary drawing artifacts for Bus objects
 * \par Function Description
 *  Get coordinates from top-level and invalidate the bounding
 *  region of a GedaBus object.
 *
 * \param [in] w_current  The GschemToplevel object
 */
void o_bus_invalidate_rubber (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;
  int min_x, min_y, max_x, max_y;
  int bloat = 0;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  if (toplevel->bus_style == STYLE_THICK ) {
    bloat = SCREENabs (w_current, toplevel->thick_bus_width) / 2;
  }

  min_x = min (x1, x2) - bloat;
  max_x = max (x1, x2) + bloat;
  min_y = min (y1, y2) - bloat;
  max_y = max (y1, y2) + bloat;

  o_invalidate_rectangle (w_current, min_x, min_y, max_x, max_y);
}

/*!
 * \brief draw the bus rubber when creating a bus
 * \par Function Description
 *  This function draws a bus rubber from the point
 *  (<B>first_wx</B>,<B>first_wy</B>) from the <B>GschemToplevel
 *  </B> structure to the input parameter (<B>w_x</B>, <B>w_y</B>).
 *
 *  The function stores creates an non-orthogonal bus segment if the
 *  CONTROLKEY is pressed. The coordinates of the second rubber bus
 *  point is stored as (<B>second_wx</B>,<B>second_wy</B>) in the
 *  <B>GschemToplevel</B> structure.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        current x position in world units
 * \param [in] w_y        current y position in world units
 */
void o_bus_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->inside_action == 0) {
    BUG_MSG("Not inside action");
  }
  else {

    if (w_current->rubber_visible)
      o_bus_invalidate_rubber (w_current);

    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    /* If you press the control key then you can draw non-ortho bus */
    if (!w_current->CONTROLKEY) {

      int diff_x, diff_y;

      diff_x = abs(w_current->second_wx - w_current->first_wx);
      diff_y = abs(w_current->second_wy - w_current->first_wy);

      if (diff_x >= diff_y) {
        w_current->second_wy = w_current->first_wy;
      }
      else {
        w_current->second_wx = w_current->first_wx;
      }
    }

    o_bus_invalidate_rubber (w_current);
    w_current->rubber_visible = 1;
  }
}

/*!
 * \brief Start input for a new bus object
 * \par Function Description
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        the x position in world coords
 * \param [in] w_y        the y position in world coords
 */
void o_bus_start(GschemToplevel *w_current, int w_x, int w_y)
{
  o_bus_init(w_current, w_x, w_y);

  i_event_start_adder_handler(w_current, o_bus_init, o_bus_end);
}