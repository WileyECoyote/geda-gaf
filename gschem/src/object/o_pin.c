/* -*- C o_pin.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_pin.c
 * \brief Low-level module for manipulating Pin objects
 */
#include <gschem.h>
#include <geda_debug.h>

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_start(GschemToplevel *w_current, int w_x, int w_y)
{
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_end(GschemToplevel *w_current, int x, int y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  Object       *new_obj;
  int           color;

  if (w_current->inside_action == 0) {
    BUG_MSG("Not inside action\n");
  }
  else {

    if (w_current->override_pin_color == -1) {
      color = PIN_COLOR;
    }
    else {
      color = w_current->override_pin_color;
    }

    /* undraw rubber line */
    /* o_pin_invalidate_rubber (w_current); */
    w_current->rubber_visible = 0;

    /* don't allow zero length pins */
    if ((w_current->first_wx == w_current->second_wx) &&
        (w_current->first_wy == w_current->second_wy))
    {
      return;
    }

    new_obj = o_pin_new(color,
                        w_current->first_wx, w_current->first_wy,
                        w_current->second_wx, w_current->second_wy,
                        PIN_NET_NODE, 0);

    new_obj->line_options->line_width = o_style_get_pin_width(toplevel, PIN_NET_NODE);

    s_page_append_object (toplevel->page_current, new_obj);

    /* Call add-objects-hook */
    g_run_hook_object (w_current, "%add-objects-hook", new_obj);

    o_undo_savestate(w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->inside_action == 0) {
    BUG_MSG("Not inside action");
  }
  else {

    /* erase the rubberpin if it is visible */
    if (w_current->rubber_visible)
      o_pin_invalidate_rubber (w_current);

    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    /* decide whether to draw the pin vertical or horizontal */
    if (abs(w_current->second_wx - w_current->first_wx)
      >= abs(w_current->second_wy - w_current->first_wy)) {
      w_current->second_wy = w_current->first_wy;
      } else {
        w_current->second_wx = w_current->first_wx;
      }

      o_pin_invalidate_rubber (w_current);
    w_current->rubber_visible = 1;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void o_pin_invalidate_rubber (GschemToplevel *w_current)
{
  int x1, y1, x2, y2;
  int min_x, min_y, max_x, max_y;
  int bloat = 0;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  /* Pins are always first created as net pins, use net pin width */
  bloat = o_style_get_pin_width(w_current->toplevel, PIN_NET_NODE);
  bloat = SCREENabs (w_current, bloat / 2);

  min_x = min (x1, x2) - bloat;
  max_x = max (x1, x2) + bloat;
  min_y = min (y1, y2) - bloat;
  max_y = max (y1, y2) + bloat;

  o_invalidate_rectangle (w_current, min_x, min_y, max_x, max_y);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_draw_rubber (GschemToplevel *w_current)
{
  int size = 0;

  /* Pins are always first created as net pins, use net pin width */
  size = o_style_get_pin_width(w_current->toplevel, PIN_NET_NODE);

  cairo_t *cr = eda_renderer_get_cairo_context (CairoRenderer);
  GArray *color_map = eda_renderer_get_color_map (CairoRenderer);
  int flags = eda_renderer_get_cairo_flags (CairoRenderer);

  eda_cairo_line (cr, flags, END_NONE, size,
                  w_current->first_wx, w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, size, -1, -1);
}
