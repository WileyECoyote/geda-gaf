/* -*- C o_net.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_net.c
 * \brief Low-level module for manipulating Net objects
 */

#include <gschem.h>
#include <math.h>
#include <geda_debug.h>

/* magnetic options */
/* half size of the magnetic marker on the screen. */
#define MAGNETIC_HALFSIZE 6

/* define how far the cursor could be to activate magnetic */
#define MAGNETIC_PIN_REACH 50
#define MAGNETIC_NET_REACH 20
#define MAGNETIC_BUS_REACH 30

/* weighting factors to tell that a pin is more important than a net */
#define MAGNETIC_PIN_WEIGHT 5.0
#define MAGNETIC_NET_WEIGHT 2.0
#define MAGNETIC_BUS_WEIGHT 3.0

/* Bit definitions for the four quardrants of the direction guessing */
#define QUADRANT1  0x01
#define QUADRANT2  0x02
#define QUADRANT3  0x04
#define QUADRANT4  0x08

/*! \brief set the start point of a new net
 *  \par Function Description
 *  This function sets the start point of a new net at the position of the
 *  cursor. If we have a visible magnetic marker, we use that instead of
 *  the cursor position
 */
static void o_net_continue(GschemToplevel *w_current, int w_x, int w_y)
{
  i_status_action_start(w_current);

  if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {
    w_current->first_wx = w_current->magnetic_wx;
    w_current->first_wy = w_current->magnetic_wy;
  }
  else {
    w_current->first_wx = w_x;
    w_current->first_wy = w_y;
  }

  w_current->second_wx = w_current->third_wx = w_current->first_wx;
  w_current->second_wy = w_current->third_wy = w_current->first_wy;

  if (w_current->first_wx != snap_grid (w_current, w_current->first_wx) ||
      w_current->first_wy != snap_grid (w_current, w_current->first_wy))
       geda_log(_("Warning: Starting net off grid coordinate\n"));

  if (w_current->net_direction_mode) {
    o_net_guess_direction(w_current, w_current->first_wx, w_current->first_wy);
  }
}

/*! \brief Find Closest Connection Location
 *  \par Function Description
 *  This function calculates the distance to all connectable objects
 *  and searches the closest connection point.
 *  It searches for pins, nets and busses.
 *
 *  The connection point is stored in GschemToplevel->magnetic_wx and
 *  GschemToplevel->magnetic_wy. If no connection is found. Both variables
 *  are set to -1.
 */
static void o_net_find_magnetic(GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GedaObject   *o_current;
  GedaObject   *o_magnetic;
  GList        *objectlists, *iter1, *iter2;

  int    x1, x2, y1, y2, min_x, min_y, w_magnetic_reach;
  double min_dist, min_best, dist1, dist2;
  double weight, min_weight;
  int    magnetic_reach;

  o_magnetic = NULL;
  min_best   = min_x = min_y = 0;
  min_weight = 0;

  /* max distance of all the different reaches */
  magnetic_reach   = max(MAGNETIC_PIN_REACH, MAGNETIC_NET_REACH);
  magnetic_reach   = max(magnetic_reach, MAGNETIC_BUS_REACH);
  w_magnetic_reach = WORLDabs (w_current, magnetic_reach);

  /* calculate the reachable region */
  x1 = w_x - w_magnetic_reach;
  y1 = w_y - w_magnetic_reach;
  x2 = w_x + w_magnetic_reach;
  y2 = w_y + w_magnetic_reach;

  /* get the objects of the tiles within reach */
  objectlists = geda_struct_tile_get_objectlists(toplevel->page_current, x1, y1, x2, y2);

  for (iter1 = objectlists; iter1 != NULL; NEXT(iter1)) {

    for (iter2 = (GList*) iter1->data; iter2 != NULL; NEXT(iter2)) {

      int left, top, right, bottom;
      int o_type;

      o_current = (GedaObject*) iter2->data;

      if (!geda_object_get_bounds(o_current, &left, &top, &right, &bottom) ||
          !visible (w_current, left, top, right, bottom))
      {
        continue; /* skip invisible objects */
      }

      o_type = geda_get_object_type(o_current);

      if (o_type == OBJ_PIN) {

        min_x = o_current->line->x[o_current->pin->whichend];
        min_y = o_current->line->y[o_current->pin->whichend];

#if HAVE_HYPOT
        min_dist = hypot(w_x - min_x, w_y - min_y);
#else
       min_dist = sqrt((double) (w_x - min_x) * (w_x - min_x)
                     + (double) (w_y - min_y) * (w_y - min_y));
#endif

        weight = min_dist / MAGNETIC_PIN_WEIGHT;
      }
      else if (o_type == OBJ_NET || o_type == OBJ_BUS) {

        /* we have 3 possible points to connect:
         *   2 endpoints and 1 midpoint point */
        x1 = o_current->line->x[0];
        y1 = o_current->line->y[0];
        x2 = o_current->line->x[1];
        y2 = o_current->line->y[1];
        /* endpoint tests */

#if HAVE_HYPOT
        dist1 = hypot((double) (w_x - x1), (w_y - y1));
        dist2 = hypot((double) (w_x - x2), (w_y - y2));
#else
        dist1 = sqrt((double) (w_x - x1) * (w_x - x1)
                   + (double) (w_y - y1) * (w_y - y1));
        dist2 = sqrt((double) (w_x - x2) * (w_x - x2)
                   + (double) (w_y - y2) * (w_y - y2));
#endif
        if (dist1 < dist2) {
          min_x = x1;
          min_y = y1;
          min_dist = dist1;
        }
        else {
          min_x = x2;
          min_y = y2;
          min_dist = dist2;
        }

        /* midpoint tests */
        if ((x1 == x2)  /* vertical net */
          && ((y1 >= w_y && w_y >= y2)
          || (y2 >= w_y && w_y >= y1)))
        {
          if (abs(w_x - x1) < min_dist) {
            min_dist = abs(w_x - x1);
            min_x = x1;
            min_y = w_y;
          }
        }

        if ((y1 == y2)  /* horitontal net */
          && ((x1 >= w_x && w_x >= x2)
          || (x2 >= w_x && w_x >= x1)))
        {
          if (abs(w_y - y1) < min_dist) {
            min_dist = abs(w_y - y1);
            min_x = w_x;
            min_y = y1;
          }
        }

        if (o_type == OBJ_BUS) {
          weight = min_dist / MAGNETIC_BUS_WEIGHT;
        }
        else { /* OBJ_NET */
          weight = min_dist / MAGNETIC_NET_WEIGHT;
        }
      }
      else { /* neither pin nor net or bus */
        continue;
      }

      if (o_magnetic == NULL || weight < min_weight) {

        min_best   = min_dist;
        min_weight = weight;
        o_magnetic = o_current;
        w_current->magnetic_wx = min_x;
        w_current->magnetic_wy = min_y;
      }
    }
  }

  /* check whether we found an object and if it's close enough */
  if (o_magnetic != NULL) {

    switch (geda_get_object_type(o_magnetic)) {
      case (OBJ_PIN): magnetic_reach = MAGNETIC_PIN_REACH; break;
      case (OBJ_NET): magnetic_reach = MAGNETIC_NET_REACH; break;
      case (OBJ_BUS): magnetic_reach = MAGNETIC_BUS_REACH; break;
    }

    if (min_best > WORLDabs (w_current, magnetic_reach)) {
      w_current->magnetic_wx = -1;
      w_current->magnetic_wy = -1;
    }
  }
  else {
    w_current->magnetic_wx = -1;
    w_current->magnetic_wy = -1;
  }

  g_list_free(objectlists);
}

/*! \brief calcutates the net route to the magnetic marker
 *  \par Function Description
 *  Depending on the two rubbernet lines from start to last and from
 *  last to second, the 3 coordinates are manipulated to find
 *  a way to the magnetic marker.
 */
static void o_net_finish_magnetic(GschemToplevel *w_current)
{
  int primary_zero_length, secondary_zero_length;

  primary_zero_length = ((w_current->first_wx == w_current->second_wx)
  && (w_current->first_wy == w_current->second_wy));

  secondary_zero_length = ((w_current->second_wx == w_current->third_wx)
  && (w_current->second_wy == w_current->third_wy));

  if (!primary_zero_length && secondary_zero_length) {
    if (w_current->first_wx == w_current->second_wx) {
      /* expand vertical line to magnetic_wy */
      w_current->second_wy = w_current->magnetic_wy;
    }
    else if (w_current->first_wy == w_current->second_wy) {
      /* expand horitontal line to vertical to magnetic_wx */
      w_current->second_wx = w_current->magnetic_wx;
    }
    /* connect to magnetic */
    w_current->third_wx = w_current->magnetic_wx;
    w_current->third_wy = w_current->magnetic_wy;
  }

  if (primary_zero_length && !secondary_zero_length) {

    /* move second line to the first (empty line) */
    w_current->first_wx = w_current->second_wx;
    w_current->first_wy = w_current->second_wy;

    if (w_current->second_wx == w_current->third_wx) {
      /* expand vertical line to magnetic_wy */
      w_current->second_wy = w_current->magnetic_wy;
    }
    else if (w_current->second_wy == w_current->third_wy) {
      /* expand horitontal line to magnetic_wx */
      w_current->second_wx = w_current->magnetic_wx;
    }
    /* connect to magnetic */
    w_current->third_wx = w_current->magnetic_wx;
    w_current->third_wy = w_current->magnetic_wy;
  }

  if (!primary_zero_length && !secondary_zero_length) {

    /* expand line in both directions */
    if (w_current->first_wx == w_current->second_wx) {
      w_current->second_wy = w_current->magnetic_wy;
    }
    else {
      w_current->second_wx = w_current->magnetic_wx;
    }
    w_current->third_wx = w_current->magnetic_wx;
    w_current->third_wy = w_current->magnetic_wy;
  }
}

/*! \brief guess the best direction for the next net drawing action
 *  \par Function Description
 *  This function checks all connectable objects at a starting point.
 *  It determines the best drawing direction for each quadrant of the
 *  possible net endpoint.
 *
 *  The directions are stored in the GschemToplevel->net_direction variable
 *  as a bitfield.
 */
void o_net_guess_direction(GschemToplevel *w_current, int wx, int wy)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int up=0, down=0, left=0, right=0;
  int x1, y1, x2, y2;
  int xmin, ymin, xmax, ymax;
  int orientation;
  GList *objectlists, *iter1, *iter2;
  GedaObject *o_current;

  int *current_rules;

  objectlists = geda_struct_tile_get_objectlists(toplevel->page_current, wx, wy, wx, wy);

  for (iter1 = objectlists; iter1 != NULL; iter1 = g_list_next(iter1)) {

    for (iter2 = (GList*) iter1->data; iter2 != NULL; iter2 = g_list_next(iter2)) {

      /* badness values       {OVERWRITE, ORTHO, CONTINUE} */
      const int pin_rules[] = {100, 50, 0};
      const int bus_rules[] = {90, 0, 40};
      const int net_rules[] = {80, 30, 0};

      o_current = (GedaObject*) iter2->data;

      if ((orientation = geda_net_object_orientation(o_current)) == NEITHER)
        continue;

      switch (o_current->type) {
        case OBJ_NET:
          current_rules = (int*) net_rules;
          break;

        case OBJ_PIN:
          current_rules = (int*) pin_rules;
          break;

        case OBJ_BUS:
          current_rules = (int*) bus_rules;
          break;

        default:
          BUG_IMSG("unhandled case", o_current->type);
          return;
      }

      x1 = o_current->line->x[0];
      x2 = o_current->line->x[1];
      y1 = o_current->line->y[0];
      y2 = o_current->line->y[1];

      xmin = min(x1, x2);
      ymin = min(y1, y2);
      xmax = max(x1, x2);
      ymax = max(y1, y2);

      if (orientation == HORIZONTAL && wy == y1) {
        if (wx == xmin) {
          up    = max(up, current_rules[1]);
          down  = max(down, current_rules[1]);
          right = max(right, current_rules[0]);
          left  = max(left, current_rules[2]);
        }
        else if (wx == xmax) {
          up    = max(up, current_rules[1]);
          down  = max(down, current_rules[1]);
          right = max(right, current_rules[2]);
          left  = max(left, current_rules[0]);
        }
        else if (xmin < wx && wx < xmax) {
          up    = max(up, current_rules[1]);
          down  = max(down, current_rules[1]);
          right = max(right, current_rules[0]);
          left  = max(left, current_rules[0]);
        }
        else {
          continue;
        }
      }
      if (orientation == VERTICAL && wx == x1) {
        if (wy == ymin) {
          up    = max(up, current_rules[0]);
          down  = max(down, current_rules[2]);
          right = max(right, current_rules[1]);
          left  = max(left, current_rules[1]);
        }
        else if (wy == ymax) {
          up    = max(up, current_rules[2]);
          down  = max(down, current_rules[0]);
          right = max(right, current_rules[1]);
          left  = max(left, current_rules[1]);
        }
        else if (ymin < wy && wy < ymax) {
          up    = max(up, current_rules[0]);
          down  = max(down, current_rules[0]);
          right = max(right, current_rules[1]);
          left  = max(left, current_rules[1]);
        }
        else {
          continue;
        }
      }
    }
  }

  w_current->net_direction = 0;
  w_current->net_direction |= up >= right ? 0 : QUADRANT1;
  w_current->net_direction |= up >= left ? 0 : QUADRANT2;
  w_current->net_direction |= down >= left ? 0 : QUADRANT3;
  w_current->net_direction |= down >= right ? 0 : QUADRANT4;

#if 0
  printf("%s: up=%d down=%d left=%d right=%d direction=%d\n", __func__,
  up, down, left, right, w_current->net_direction);
#endif
  g_list_free(objectlists);
}

/*! \brief finish a net drawing action
 * \par Function Description
 * This function finishes the drawing of a net. If we have a visible
 * magnetic marker, we use that instead of the current cursor
 * position.
 *
 * The rubber nets are removed, the nets and cues are drawn and the
 * net is added to the GedaToplevel structure.
 *
 * The function returns TRUE if it has drawn a net, FALSE otherwise.
 */
static void o_net_end(GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int color;
  int primary_zero_length, secondary_zero_length;
  int found_primary_connection = FALSE;
  int save_wx, save_wy;

  GList *prev_conn_objects;
  GedaObject *new_net = NULL;

  /* Save a list of added objects to run the %add-objects-hook later */
  GList *added_objects = NULL;

  o_net_invalidate_rubber (w_current);

  if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {
    o_net_finish_magnetic(w_current);
  }

  w_current->rubber_visible = FALSE;

  /* See if either of the nets are zero length.  We'll only add */
  /* the non-zero ones */
  primary_zero_length = (w_current->first_wx == w_current->second_wx) &&
                        (w_current->first_wy == w_current->second_wy);

  secondary_zero_length = (w_current->second_wx == w_current->third_wx) &&
                          (w_current->second_wy == w_current->third_wy);

  /* If both nets are zero length abort the net drawing mode */
  if ( primary_zero_length && secondary_zero_length ) {
    return;
  }

  save_wx = w_current->third_wx;
  save_wy = w_current->third_wy;

  if (w_current->override_net_color == -1) {
    color = NET_COLOR;
  }
  else {
    color = w_current->override_net_color;
  }

  if (w_current->third_wx != snap_grid (w_current, w_current->third_wx) ||
      w_current->third_wy != snap_grid (w_current, w_current->third_wy))
       geda_log(_("Warning: Ending net off grid coordinate\n"));

  if (!primary_zero_length ) {

      /* create primary net */
      new_net = geda_net_object_new(color,
                                    w_current->first_wx, w_current->first_wy,
                                    w_current->second_wx, w_current->second_wy);

      new_net->line_options->line_width = geda_object_style_get_net_width(toplevel);
      geda_struct_page_append_object (toplevel->page_current, new_net);

      added_objects = g_list_prepend (added_objects, new_net);

      /* conn stuff */
      /* LEAK CHECK 1 */
      prev_conn_objects = geda_struct_conn_return_others (NULL, new_net);
      o_net_add_busrippers (w_current, new_net, prev_conn_objects);
      g_list_free (prev_conn_objects);

#if DEBUG
      printf("primary:\n");
      geda_struct_conn_print(geda_object_get_conn_list(new_net));
#endif

      /* Go off and search for valid connection on this newly created net */
      found_primary_connection = geda_struct_conn_net_search(new_net, 1,
                                                   new_net->conn_list);
      if (found_primary_connection) {

        /* if a net connection is found, reset start point of next net */
        save_wx = w_current->second_wx;
        save_wy = w_current->second_wy;
      }
  }

  /* If the second net is not zero length, add it as well */
  /* Also, a valid net connection from the primary net was not found */
  if (!secondary_zero_length && !found_primary_connection) {

      /* Add secondary net */
      new_net = geda_net_object_new(color,
                                    w_current->second_wx, w_current->second_wy,
                                    w_current->third_wx, w_current->third_wy);

      new_net->line_options->line_width = geda_object_style_get_net_width(toplevel);
      geda_struct_page_append_object (toplevel->page_current, new_net);

      added_objects = g_list_prepend (added_objects, new_net);

      /* conn stuff */
      /* LEAK CHECK 2 */
      prev_conn_objects = geda_struct_conn_return_others (NULL, new_net);
      o_net_add_busrippers (w_current, new_net, prev_conn_objects);
      g_list_free (prev_conn_objects);

#if DEBUG
      geda_struct_conn_print(geda_object_get_conn_list(new_net));
#endif

  }

  /* Call add-objects-hook */
  if (added_objects != NULL) {
    g_hook_run_object_list (w_current, ADD_OBJECT_HOOK, added_objects);
    g_list_free (added_objects);
  }

  o_undo_savestate_object(w_current, UNDO_ALL, new_net);

  w_current->first_wx = save_wx;
  w_current->first_wy = save_wy;

  /* Continue net drawing */
  o_net_continue(w_current, w_current->first_wx, w_current->first_wy);
}

/*! \brief draw rubbernet lines to the gc
 *  \par Function Description
 *  This function draws the rubbernets to the graphic context
 */
void o_net_draw_rubber(GschemToplevel *w_current )
{
  cairo_t *cr       = eda_renderer_get_cairo_context (CairoRenderer);
  GArray *color_map = eda_renderer_get_color_map (CairoRenderer);
  int flags         = eda_renderer_get_cairo_flags (CairoRenderer);
  int size          = geda_object_style_get_net_width (w_current->toplevel);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);

  if (w_current->magnetic_net_mode) {

    if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {

      int w_magnetic_halfsize;

      w_magnetic_halfsize = max (4 * size,
                                 WORLDabs (w_current, MAGNETIC_HALFSIZE));

      eda_cairo_arc (cr, flags, size,
                     w_current->magnetic_wx, w_current->magnetic_wy,
                     w_magnetic_halfsize, 0, 360);
    }
  }

  /* Primary line */
  eda_cairo_line (cr, flags, END_NONE, size,
                  w_current->first_wx,  w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);

  /* Secondary line */
  eda_cairo_line (cr, flags, END_NONE, size,
                     w_current->second_wx, w_current->second_wy,
                     w_current->third_wx,  w_current->third_wy);

  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, size, -1, -1);
}

/*! \brief erase and redraw the rubber lines when drawing a net
 *  \par Function Description
 *  This function draws the rubber net lines when drawing a net.
 */
void o_net_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->inside_action) {

    int ortho;

    /* Orthognal mode enabled when Control Key is NOT pressed or
     *    if we are using magnetic mode */
    ortho = !w_current->CONTROLKEY || w_current->magnetic_net_mode;

    if (w_current->rubber_visible) {
      o_net_invalidate_rubber (w_current);
    }

    if (w_current->magnetic_net_mode) {

      if (w_current->CONTROLKEY) {
        /* set the magnetic marker position to current xy if the
         *    controlkey is pressed. Thus the net will not connect to
         *    the closest net if we finish the net drawing */
        w_current->magnetic_wx = w_x;
        w_current->magnetic_wy = w_y;
      }
      else {
        o_net_find_magnetic(w_current, w_x, w_y);
      }
    }

    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    /* In orthogonal mode secondary line is the same as the first */
    if (!ortho) {
      w_current->third_wx = w_current->second_wx;
      w_current->third_wy = w_current->second_wy;
    }
    else {

      /* If control key pressed then draw non-ortho nets */

      int horizontal, quadrant;

      if (w_current->second_wy > w_current->first_wy) {
        quadrant = w_current->second_wx > w_current->first_wx ? QUADRANT1 :
                                                                QUADRANT2;
      }
      else {
        quadrant = w_current->second_wx > w_current->first_wx ? QUADRANT4 :
                                                                QUADRANT3;
      }

      horizontal = w_current->net_direction & quadrant;

      if (!w_current->SHIFTKEY)
        horizontal = !horizontal;

      /* calculate the co-ordinates necessary to draw the lines*/
      /* Pressing the shift key will cause the vertical and horizontal lines to switch places */
      if ( horizontal ) {
        w_current->second_wy = w_current->first_wy;
        w_current->third_wx = w_current->second_wx;
        w_current->third_wy = w_y;
      } else {
        w_current->second_wx = w_current->first_wx;
        w_current->third_wx = w_x;
        w_current->third_wy = w_current->second_wy;
      }
    }

    o_net_invalidate_rubber (w_current);
    w_current->rubber_visible = TRUE;
  }
  else {
    BUG_MSG("Not inside action");
  }
}

/*! \brief Invalidate Temporary drawing artifacts for Net objects
 *  \par Function Description
 *   Get coordinates from top-level and invalidate the bounding
 *   region of a Net object. The bounding region is compensated
 *   for line width and grips.
 */
void o_net_invalidate_rubber (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  int size = 0;
  int bloat;
  int magnetic_x, magnetic_y;
  int first_x, first_y, second_x, second_y, third_x, third_y;
  int x1, y1, x2, y2;

  WORLDtoSCREEN (w_current, w_current->magnetic_wx, w_current->magnetic_wy,
                 &magnetic_x, &magnetic_y);
  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy,
                 &first_x, &first_y);
  WORLDtoSCREEN (w_current, w_current->third_wx, w_current->third_wy,
                 &third_x, &third_y);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy,
                 &second_x, &second_y);

  size  = geda_object_style_get_net_width(toplevel);
  size  = max (size, 0);
  bloat = size / 2;

  if (w_current->magnetic_net_mode) {

    if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {

      int magnetic_halfsize;

      magnetic_halfsize = max (4 * size, MAGNETIC_HALFSIZE);

      o_invalidate_rectangle (w_current, magnetic_x - magnetic_halfsize,
                              magnetic_y - magnetic_halfsize,
                              magnetic_x + magnetic_halfsize,
                              magnetic_y + magnetic_halfsize);
    }
  }

  x1 = min (first_x, second_x) - bloat;
  x2 = max (first_x, second_x) + bloat;
  y1 = min (first_y, second_y) - bloat;
  y2 = max (first_y, second_y) + bloat;

  o_invalidate_rectangle (w_current, x1, y1, x2, y2);

  x1 = min (second_x, third_x) - bloat;
  x2 = max (second_x, third_x) + bloat;
  y1 = min (second_y, third_y) - bloat;
  y2 = max (second_y, third_y) + bloat;

  o_invalidate_rectangle (w_current, x1, y1, x2, y2);
}


/*!
 * \brief Add Bus Rippers to a Net Object
 * \par Function Description
 *
 * \returns TRUE if something was added, otherwise FALSE
 */
int o_net_add_busrippers(GschemToplevel *w_current,
                         GedaObject     *net_obj,
                         GList          *prev_conn_objects)

{
  CONN   *found_conn  = NULL;
  GList  *cl_current;
  double  distance1, distance2;
  double  length;
  int     color;
  int     otherone;
  int     ripper_count = 0;
  int     sign;
  int     first, second;
  int     made_changes  = FALSE;
  const int ripper_size = w_current->bus_ripper_size;
  int complex_angle     = 0;
  BUS_RIPPER rippers[2];

  if (!GEDA_IS_LINE(net_obj)) {
    return(FALSE);
  }

  length = geda_line_object_length(net_obj);

  if (!prev_conn_objects) {
    return(FALSE);
  }

  if (length <= ripper_size) {
    return(FALSE);
  }

  if (w_current->override_net_color == -1) {
    color = NET_COLOR;
  }
  else {
    color = w_current->override_net_color;
  }

  /* check for a bus connection and draw rippers if so */
  cl_current = prev_conn_objects;

  while (cl_current != NULL) {

    GedaObject *ukn_object = cl_current->data;

    if (GEDA_IS_BUS(ukn_object)) {

      const GList *cl_current2;

      GedaBus  *bus_object  = (GedaBus*)ukn_object;
      GedaLine *line_object = (GedaLine*)bus_object;

      int bus_orientation = geda_bus_object_orientation(ukn_object);
      int net_orientation = geda_net_object_orientation(net_obj);

      /* find the CONN structure which is associated with this object */
      cl_current2 = geda_object_get_conn_list(net_obj);

      while (cl_current2 != NULL) {

        CONN *tmp_conn = (CONN*) cl_current2->data;

        if (tmp_conn && tmp_conn->other_object &&
          ((GedaObject*)tmp_conn->other_object == ukn_object))
        {

          found_conn = tmp_conn;
          break;
        }

        cl_current2 = cl_current2->next;
      }

      if (!found_conn) {
        return(FALSE);
      }

      otherone = !found_conn->whichone;

      /* now deal with the found connection */
      if (bus_orientation == HORIZONTAL && net_orientation == VERTICAL) {

        sign = bus_object->ripper_direction;
        if (!sign) {
          if (line_object->x[0] < line_object->x[1]) {
            first = 0;
            second = 1;
          }
          else {
            first = 1;
            second = 0;
          }

          distance1 = abs(line_object->x[first] -
                          net_obj->line->x[found_conn->whichone]);
          distance2 = abs(line_object->x[second] -
                          net_obj->line->x[found_conn->whichone]);

          if (distance1 <= distance2) {
            sign = 1;
          }
          else {
            sign = -1;
          }
          bus_object->ripper_direction = sign;
        }

        /* printf("hor sign: %d\n", sign); */

        if (net_obj->line->y[otherone] < line_object->y[0]) {
          /* new net is below bus */
          /*printf("below\n");*/

          if (ripper_count >= 2) {
            /* try to exit gracefully */
            BUG_MSG("Tried to add more than two bus rippers.\n");
            made_changes = FALSE;
            break;
          }

          if (w_current->bus_ripper_rotation == NON_SYMMETRIC) {
            /* non-symmetric */
            if (sign == 1) {
              complex_angle = 0;
            }
            else {
              complex_angle = 90;
            }
          }
          else {
            /* symmetric */
            complex_angle = 0;
          }

          net_obj->line->y[found_conn->whichone] -= ripper_size;
          geda_object_set_bounds_valid(net_obj, FALSE);

          rippers[ripper_count].x[0] = net_obj->line->x[found_conn->whichone];
          rippers[ripper_count].y[0] = net_obj->line->y[found_conn->whichone];

          rippers[ripper_count].x[1] = net_obj->line->x[found_conn->whichone]
                                       + sign * ripper_size;
          rippers[ripper_count].y[1] = net_obj->line->y[found_conn->whichone]
                                       + ripper_size;
          ripper_count++;
          /* printf("done\n"); */
          made_changes++;

        }
        else {
          /* new net is above bus */
          /* printf("above\n"); */

          if (ripper_count >= 2) {
            /* try to exit gracefully */
            BUG_MSG("Tried to add more than two bus rippers.\n");
            made_changes = FALSE;
            break;
          }

          if (w_current->bus_ripper_rotation == NON_SYMMETRIC) {
            /* non-symmetric */
            if (sign == 1) {
              complex_angle = 270;
            }
            else {
              complex_angle = 180;
            }
          }
          else {
            /* symmetric */
            complex_angle = 180;
          }

          net_obj->line->y[found_conn->whichone] += ripper_size;
          geda_object_set_bounds_valid(net_obj, FALSE);

          rippers[ripper_count].x[0] = net_obj->line->x[found_conn->whichone];
          rippers[ripper_count].y[0] = net_obj->line->y[found_conn->whichone];

          rippers[ripper_count].x[1] = net_obj->line->x[found_conn->whichone]
                                       + sign * ripper_size;
          rippers[ripper_count].y[1] = net_obj->line->y[found_conn->whichone]
                                       - ripper_size;
          ripper_count++;

            /* printf("done\n"); */
          made_changes++;
        }
      }
      else if (bus_orientation == VERTICAL && net_orientation == HORIZONTAL) {

        sign = bus_object->ripper_direction;

        if (!sign) {
          if (line_object->y[0] < line_object->y[1]) {
            first = 0;
            second = 1;
          }
          else {
            first = 1;
            second = 0;
          }

          distance1 = abs(line_object->y[first] -
                          net_obj->line->y[found_conn->whichone]);
          distance2 = abs(line_object->y[second] -
                          net_obj->line->y[found_conn->whichone]);

          if (distance1 <= distance2) {
            sign = 1;
          }
          else {
            sign = -1;
          }
          bus_object->ripper_direction = sign;
        }
        /* printf("ver sign: %d\n", sign); */

        if (net_obj->line->x[otherone] < line_object->x[0]) {
          /* new net is to the left of the bus */
          /* printf("left\n"); */

          if (ripper_count >= 2) {
            /* try to exit gracefully */
            BUG_MSG("Tried to add more than two bus rippers.\n");
            made_changes = FALSE;
            break;
          }

          if (w_current->bus_ripper_rotation == NON_SYMMETRIC) {
            /* non-symmetric */
            if (sign == 1) {
              complex_angle = 0;
            }
            else {
              complex_angle = 270;
            }
          }
          else {
            /* symmetric */
            complex_angle = 270;
          }

          net_obj->line->x[found_conn->whichone] -= ripper_size;
          geda_object_set_bounds_valid(net_obj, FALSE);

          rippers[ripper_count].x[0] = net_obj->line->x[found_conn->whichone];
          rippers[ripper_count].y[0] = net_obj->line->y[found_conn->whichone];

          rippers[ripper_count].x[1] = net_obj->line->x[found_conn->whichone]
                                       + ripper_size;
          rippers[ripper_count].y[1] = net_obj->line->y[found_conn->whichone]
                                       + sign * ripper_size;
          ripper_count++;

          made_changes++;
        }
        else {
          /* new net is to the right of the bus */
          /* printf("right\n"); */

          if (ripper_count >= 2) {
            /* try to exit gracefully */
            BUG_MSG("Tried to add more than two bus rippers.\n");
            made_changes = FALSE;
            break;
          }

          if (w_current->bus_ripper_rotation == NON_SYMMETRIC) {
            /* non-symmetric */
            if (sign == 1) {
              complex_angle = 90;
            }
            else {
              complex_angle = 180;
            }
          }
          else {
            /* symmetric */
            complex_angle = 90;
          }

          net_obj->line->x[found_conn->whichone] += ripper_size;
          geda_object_set_bounds_valid(net_obj, FALSE);

          rippers[ripper_count].x[0] = net_obj->line->x[found_conn->whichone];
          rippers[ripper_count].y[0] = net_obj->line->y[found_conn->whichone];
          rippers[ripper_count].x[1] = net_obj->line->x[found_conn->whichone]
                                       - ripper_size;
          rippers[ripper_count].y[1] = net_obj->line->y[found_conn->whichone]
                                       + sign * ripper_size;
          ripper_count++;

          made_changes++;
        }
      }
    }

    cl_current = cl_current->next;
  }

  if (made_changes) {

    GedaToplevel     *toplevel  = w_current->toplevel;
    const CLibSymbol *rippersym = NULL;
    int   i;

    geda_struct_conn_remove_object (net_obj);

    if (w_current->bus_ripper_type == COMP_BUS_RIPPER) {

      GList *symlist;

      symlist = geda_struct_clib_search (w_current->bus_ripper_symname, CLIB_EXACT);

      if (symlist != NULL) {
        rippersym = (CLibSymbol *) symlist->data;
      }
      g_list_free (symlist);
    }

    for (i = 0; i < ripper_count; i++) {

      GedaObject *new_obj;

      if (w_current->bus_ripper_type == NET_BUS_RIPPER) {
        new_obj = geda_net_object_new(color,rippers[i].x[0], rippers[i].y[0],
                            rippers[i].x[1], rippers[i].y[1]);
        new_obj->line_options->line_width =  geda_object_style_get_net_width(toplevel);
        geda_struct_page_append_object (toplevel->page_current, new_obj);
      }
      else {

        char *name = w_current->bus_ripper_symname;

        if (rippersym != NULL) {
          new_obj = geda_complex_object_new (toplevel, rippers[i].x[0],
                                                       rippers[i].y[0],
                                                       complex_angle, 0,
                                                       rippersym, name, 1);
          geda_struct_page_append_list (toplevel->page_current,
                              geda_complex_object_promote_attribs (toplevel, new_obj));
          geda_struct_page_append_object (toplevel->page_current, new_obj);
        }
        else {
           geda_log("%s \"%s\"\n",
                      _("Bus ripper symbol was not found in any component library"),
                         name);
        }
      }
    }

    geda_struct_conn_update_linear_object (net_obj);
    return(TRUE);
  }

  return(FALSE);
}

/*! \brief Reset all variables used for net drawing
 *  \par Function Description
 *  This function resets all variables in GschemToplevel that are used
 *  for net drawing. This function should be called when escaping from
 *  a net drawing action or before entering it.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *
 *  \return state of inside_action on entry
 */
bool o_net_reset(GschemToplevel *w_current)
{
  int was_action = w_current->inside_action;

  o_net_invalidate_rubber (w_current);

  i_status_action_stop(w_current);

  w_current->first_wx       = w_current->first_wy    = -1;
  w_current->second_wx      = w_current->second_wy   = -1;
  w_current->third_wx       = w_current->third_wy    = -1;
  w_current->magnetic_wx    = w_current->magnetic_wy = -1;

  w_current->rubber_visible = FALSE;

  return was_action;
}

/*! \brief Start the process to input new Net objects.
 *  \par Function Description
 *  This function starts the process of interactively adding Nets to the
 *  current sheet.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Initial x coordinate of pointer in world units.
 *  \param [in] w_y        Initial y coordinate of pointer in world units.
 */
void o_net_start(GschemToplevel *w_current, int w_x, int w_y)
{
  o_net_continue(w_current, w_x, w_y);

  i_event_start_adder_handler(w_current, o_net_continue, o_net_end);
}

/*! \brief callback function to draw a net marker in magnetic mode
 *  \par Function Description
 *  If the mouse is moved, this function is called to update the
 *  position of the magnetic marker.
 *  If the controllkey is pressed the magnetic marker follows the mouse.
 */
void o_net_start_magnetic(GschemToplevel *w_current, int w_x, int w_y)
{
  o_net_invalidate_rubber (w_current);

  if (w_current->CONTROLKEY) {
    w_current->magnetic_wx = w_x;
    w_current->magnetic_wy = w_y;
  }
  else {
    o_net_find_magnetic(w_current, w_x, w_y);
  }

  o_net_invalidate_rubber (w_current);
  w_current->rubber_visible = TRUE;
}
