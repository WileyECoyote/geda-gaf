/* -*- C o_line.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_line.c
 * \brief Low-level module for manipulating Line objects
 */
#include <gschem.h>
#include <geda_debug.h>

/*! \brief Draw line from GschemToplevel object.
 *  \par Function Description
 *  This function draws a line with an exclusive or function over the
 *  sheet. The color of the line is <B>SELECT_COLOR</B>. The line is
 *  described by the two points (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GschemToplevel object.
 */
void o_line_draw_rubber (GschemToplevel *w_current)
{
  int width = geda_object_style_get_line_width(w_current->toplevel);
  cairo_t *cr = eda_renderer_get_cairo_context (CairoRenderer);
  GArray *color_map = eda_renderer_get_color_map (CairoRenderer);
  int flags = eda_renderer_get_cairo_flags (CairoRenderer);

  eda_cairo_line (cr, flags, END_NONE, width,
                  w_current->first_wx, w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, width, -1, -1);
}

/*! \brief End the input of a line.
 *  \par Function Description
 *  This function ends the process of interactively adding a line to the
 *  current sheet.
 *
 *  It first erases the last temporary line displayed, calculates the
 *  corresponding world coordinates of the two ends of the line and finally
 *  adds a new initialized line object to the list of object of the current
 *  sheet.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_line_end(GschemToplevel *w_current, int w_x, int w_y)
{
  /* Don't bother.. the real object is invalidated, its in the same place */
  i_status_action_stop(w_current);

  w_current->rubber_visible = FALSE;

  /* don't allow zero length lines */
  if ((w_current->first_wx - w_current->second_wx) ||
      (w_current->first_wy - w_current->second_wy))
  {
    GedaToplevel *toplevel = w_current->toplevel;
    GedaObject   *new_obj;

    /* create the line object and draw it */
    new_obj = geda_line_object_new (GRAPHIC_COLOR,
                          w_current->first_wx, w_current->first_wy,
                          w_current->second_wx, w_current->second_wy);
    new_obj->line_options->line_width = geda_object_style_get_line_width(toplevel);

    geda_struct_page_append_object (toplevel->page_current, new_obj);

    /* Call add-objects-hook */
    g_hook_run_object (w_current, ADD_OBJECT_HOOK, new_obj);

    o_undo_savestate_object(w_current, UNDO_ALL, new_obj);
  }

}

/*! \brief Initialize Variables to input a new line.
 *  \par Function Description
 *  This function initialize variables to interactively add a new line to
 *  the current sheet.
 *
 *  During the process, the line is internally represented by the two ends
 *  of the line as (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>)
 *  and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  A temporary line is drawn during the process with the selection color
 *  and changed according to the position of the mouse pointer.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_init(GschemToplevel *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe line */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  w_current->rubber_visible = TRUE;
}

/*! \brief Invalidate Temporary drawing artifacts for Line objects
 *  \par Function Description
 *   Get coordinates from top-level and invalidate the bounding
 *   region of a Line object.
 *
 *  \param [in] w_current  The GschemToplevel object
 */
void o_line_invalidate_rubber (GschemToplevel *w_current)
{
  int x1, y1, x2, y2;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  o_invalidate_rectangle (w_current, x1, y1, x2, y2);
}

/*! \brief Draw temporary line while dragging end.
 *  \par Function Description
 *  This function manages the erase/update/draw process of temporary line
 *  when modifying one end of the line.
 *  The line is described by four <B>*w_current</B> variables : the first end
 *  of the line is (<B>first_wx</B>,<B>first_wy</B>), the second end is
 *  (<B>second_wx</B>,<B>second_wy</B>).
 *  The first end is constant. The second end is updated to the (<B>w_x</B>,<B>w_y</B>).
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->inside_action) {

    if (w_current->rubber_visible)

      o_line_invalidate_rubber (w_current);

    /*
     * The coordinates of the moving end of the line are updated. Its new
     * coordinates are in <B>w_x</B> and <B>w_y</B> parameters and saved to
     * <B>w_current->second_wx</B> and <B>w_current->second_wy</B> respectively.
     */
    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    /* if the control key was pressed then draw ortho lines */
    if (w_current->CONTROLKEY) {

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

    o_line_invalidate_rubber (w_current);
    w_current->rubber_visible = TRUE;
  }
  else {
    BUG_MSG ("Not inside action");
  }
}

/*! \brief Start the process to input a new line.
 *  \par Function Description
 *  This function starts the process of interactively adding a line to the
 *  current sheet.
 *
 *  During all the process, the line is internally represented by the two ends
 *  ends of the line as (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>)
 *  and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  A temporary line is drawn during the process with the selection color
 *  and changed according to the position of the mouse pointer.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_start(GschemToplevel *w_current, int w_x, int w_y)
{
  o_line_init(w_current, w_x, w_y);

  i_event_start_adder_handler(w_current, o_line_init, o_line_end);
}
