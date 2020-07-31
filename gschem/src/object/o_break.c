/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: o_break.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2015 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This Library is free software; you can redistribute it and or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; version 3 of the
 * License.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this Library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 *
 *  Date: February, 14, 2015
 *  Contributing Author: Wiley Edward Hill
 */
/*!
 * \file o_break.c
 * \brief Break Operations Implementatiom Module
 */

#include <gschem.h>
#include <math.h>

/** \defgroup Break-Operations Break Operations
 *  @{
 *  \ingroup Editing-Operations
 *  \par This group contains routines for Breaking Breakable objects.
 *  \image html break.png
 *  \image latex break.png
 */

/*!
 * \brief Is an object Breakable
 * \par Function Description
 *  Returns true of \a object is a type of object that is supported
 *  by the Break module.
 *
 * \returns TRUE if object is breakable, otherwise FALSE.
 */
static bool o_break_is_breakable (GedaObject *object)
{
  int anwser;

  if (GEDA_IS_OBJECT(object)) {
    switch (object->type) {
      case OBJ_ARC:
      case OBJ_BOX:
      case OBJ_PATH:
      case OBJ_LINE:
      case OBJ_NET:
      case OBJ_BUS:
      case OBJ_CIRCLE:
        anwser = TRUE;
        break;
      default:
        anwser = FALSE;
    }
  }
  else {
    anwser = FALSE;
  }
  return anwser;
}

/*!
 * \brief Break an Arc
 * \par Function Description
 *  If both points specified in \a w_current are on the Arc object,
 *  then creates two degenerate Arcs that are the remnants of what
 *  remains of the original arc. The starting and ending points are
 *  the break points in the counterclockwise direction.
 *
 * \returns TRUE if the task was completed, otherwise FALSE.
 *
 * \remark \a object Must be a Arc object and is not checked!
 */
static bool o_break_arc(GschemToplevel *w_current, GedaObject *object)
{
  bool result = FALSE;
  GedaPoint point1;
  GedaPoint point2;

  point1.x = w_current->first_wx;
  point1.y = w_current->first_wy;

  if (geda_arc_includes_point(object->arc, &point1)) {

    point2.x = w_current->second_wx;
    point2.y = w_current->second_wy;

    if (geda_arc_includes_point(object->arc, &point2)) {

      GedaObject *new_obj;
      double  radians;
      int  color;
      int  cx;
      int  cy;
      int  radius;
      int  angle_1;
      int  angle_2;
      int  arc_sweep1;
      int  arc_sweep2;
      int  end_angle;
      int  start_angle1;
      int  start_angle2;

      color   = object->color;
      cx      = object->arc->x;
      cy      = object->arc->y;
      radius  = object->arc->radius;

      /* Get angle to first point */
      radians = atan2((point1.y - cy), (point1.x - cx));

      /* If negative, make the angle positive */
      while (radians < 0) {
        radians += 2 * M_PI;
      }
      angle_1 = geda_math_radians_to_degrees(radians);

      /* Get angle to second point */
      radians = atan2((point2.y - cy), (point2.x - cx));

      /* If negative, make the angle positive */
      while (radians < 0) {
        radians += 2 * M_PI;
      }

      angle_2 = geda_math_radians_to_degrees(radians);

      start_angle1 = object->arc->start_angle;
      end_angle    = object->arc->arc_sweep + start_angle1;

      if (angle_2 > angle_1){
        arc_sweep1   = angle_1 - start_angle1;
        arc_sweep2   = end_angle - angle_2;
        start_angle2 = angle_2;
      }
      else {
        arc_sweep1   = angle_2 - start_angle1;
        arc_sweep2   = end_angle - angle_1;
        start_angle2 = angle_1;
      }

      if (arc_sweep1) {

        new_obj = geda_arc_object_new (color, cx, cy, radius, start_angle1, arc_sweep1);

        /* Set line options of arc to the values used by the old arc */
        geda_set_object_line_options(new_obj, object->line_options);

        /* Add the new Arc to the page */
        geda_struct_page_append_object (Current_Page, new_obj);
      }

      if (arc_sweep2) {

        new_obj = geda_arc_object_new (color, cx, cy, radius, start_angle2, arc_sweep2);

        /* Set line options of arc to the values used by the old arc */
        geda_set_object_line_options(new_obj, object->line_options);

        /* Add the new Arc to the page */
        geda_struct_page_append_object (Current_Page, new_obj);
      }

      /* Delete the old Arc */
      o_delete(w_current, object);

      result = TRUE;
    }
    else
      v_log_message(_("Point 2 missed x=%d, y=%d\n"), point2.x, point2.y);
  }
  else {
    v_log_message(_("Point 1 missed x=%d, y=%d\n"), point1.x, point1.y);
  }

  return result;
}

/*!
 * \brief Break a Box
 * \par Function Description
 *  If both points specified in \a w_current are on the box object,
 *  then creates a degenerate path whose vertices are the remnants
 *  of what remains of the box. The starting and ending points are
 *  the break points and in all cases, the replacement path is not
 *  closed.
 *
 * \returns TRUE if the task was completed, otherwise FALSE.
 *
 * \remark \a object Must be a GedaBox object and is not checked!
 *
 * \image html break_box.png
 * \image latex break_box.png
 */
static bool o_break_box(GschemToplevel *w_current, GedaObject *object)
{
  GedaBox *box = object->box;
  bool  result = FALSE;
  GedaPoint point1;
  GedaPoint point2;
  int   corn1;
  int   corn2;
  int   side1;
  int   side2;

  int  left   = /* min */ box->upper_x < box->lower_x ? box->upper_x : box->lower_x;
  int  bottom = /* min */ box->upper_y < box->lower_y ? box->upper_y : box->lower_y;
  int  right  = /* max */ box->upper_x > box->lower_x ? box->upper_x : box->lower_x;
  int  top    = /* max */ box->upper_y > box->lower_y ? box->upper_y : box->lower_y;

  /* Internal function to determine which side of the box a point is on,
   * if any. Note left is side 1, then continue counter-clockwise. */
  int get_side (GedaPoint *point) {
    int side = 0;
    if (point->x == left && (point->y < top || point->y > bottom)) {
      side = 1;
    }
    else if (point->y == bottom && (point->x < right || point->x > left)) {
      side = 2;
    }
    else if (point->x == right && (point->y < top || point->y > bottom)) {
      side = 3;
    }
    else if (point->y == top && (point->x < right || point->x > left)) {
      side = 4;
    }
    return side;
  }

  /* Internal function to check if a point is a vertex of the box.
   * Note bottom left is 1, then continue counter-clockwise. */
  int get_corner (GedaPoint *point) {
    int corner = 0;
    if (point->x == left && point->y == bottom) {
      corner = 1;
    }
    else if (point->y == bottom && point->x == right) {
      corner = 2;
    }
    else if (point->x == right &&  point->y== top) {
      corner = 3;
    }
    else if (point->y == top && point->x == left) {
      corner = 4;
    }
    return corner;
  }

  /* Internal wrapper for get_side and get_corner to set cornX
   * and sideX by reference */
  inline void checkpoint (GedaPoint *point, int *side, int *corner) {
    *side = get_side(point);
    if (*side == 0) {
      *corner = get_corner(point);
    }
    else {
      *corner = 0;
    }
  }

  point1.x = snap_grid (w_current, w_current->first_wx);
  point1.y = snap_grid (w_current, w_current->first_wy);

  checkpoint(&point1, &side1, &corn1);

  if (side1 || corn1) { /* if a side or a corner then 1 hit box */

    point2.x = snap_grid (w_current, w_current->second_wx);
    point2.y = snap_grid (w_current, w_current->second_wy);

    checkpoint(&point2, &side2, &corn2);

    if (side2 || corn2) { /* if a side or a corner then 2 hit box */

      /* Both points landed on box, we're good to go,... */

      GArray     *vertices;
      GedaObject *new_path;
      GedaPoint   points[5];
      GedaPoint  *start;
      GedaPoint  *end;
      int     done;
      int     index;
      int     stop;

      /* Determine the starting and ending points, normally we would use
       * the second point and keep vertices until we reach the first point,
       * but if both points are on the same side then it should be obvious
       * the user wants to "break" the box between the points, regardless
       * of which was first. The following chain of conditionals tests for
       * these situations */
      if (point1.x == left && point2.x == left) {
        if (point2.y > point1.y) {
          start = &point1;
          end   = &point2;
        }
        else {
          start = &point2;
          end   = &point1;
        }
      }
      else if (point1.y == bottom && point2.y == bottom) {
        if (point1.x > point2.x) {
          start = &point1;
          end   = &point2;
        }
        else {
          start = &point2;
          end   = &point1;
        }
      }
      else if (point1.x == right && point2.x == right) {
        if (point2.y > point1.y) {
          start = &point2;
          end   = &point1;
        }
        else {
          start = &point1;
          end   = &point2;
        }
      }
      else if (point1.y == top && point2.y == top) {
        if (point2.x > point1.x) {
          start = &point1;
          end   = &point2;
        }
        else {
          start = &point2;
          end   = &point1;
        }
      }
      else { /* Points are not on the same side of box */
        start = &point2;
        end   = &point1;
      }

      /* Update the index flags */
      checkpoint(start, &side1, &corn1);
      checkpoint(end,   &side2, &corn2);

      /* Set the starting index */
      if (corn1) {
        index = corn1 + 1;
      }
      else {
        index = side1;
      }

      /* Use magic to calculate the final index */
      if (corn2) {
        side2 = corn1 - 1;
      }

      /* The number of negative stops will be side1 - 1 */
      stop = side2 - side1;

      /* stop = 0 if same side, so must rotate completely */

      if (stop < 1) {       /* add the potion */
        stop = stop + 4;    /* in just the right amount */
      }

      /* Create a table of the box's vertices
       *  1   left    bottom
       *  2   right   bottom
       *  3   right   top
       *  4   left    top
       */
      points[1].x = left;
      points[1].y = bottom;
      points[2].x = right;
      points[2].y = bottom;
      points[3].x = right;
      points[3].y = top;
      points[4].x = left;
      points[4].y = top;

      vertices = g_array_new (FALSE, FALSE, sizeof(GedaPoint));

      /* Add the starting point */
      g_array_append_val (vertices, *start);

      done = 0; /* is the vertex counter, not boolean! */

      do {                /* gather vertices, while */

        if (index == 5) { /* rotating around in table data */
          index = 1;
        }

        g_array_append_val (vertices, points[index]);
        done++;
        index++;

      } while (done != stop); /* until vertex count is not stop (because) */

      g_array_append_val (vertices, *end);  /* there is one more vertex */

      new_path = geda_path_object_new_from_polygon(vertices, object->color);

      /* Set line options of the path to the values used by the box */
      geda_set_object_line_options(new_path, object->line_options);

      /* Delete the box */
      o_delete(w_current, object);

      /* Add the new path to the page */
      geda_struct_page_append_object (Current_Page, new_path);

      g_array_free (vertices, TRUE);

      result = TRUE;
    }
  }
  return result;
}

/*!
 * \brief Break a Circle
 * \par Function Description
 *  If both points specified in \a w_current are on the Circle object,
 *  then creates an arc whose angles correspond to the starting and
 *  ending points of the new arc, in the counterclockwise direction.
 *
 * \returns TRUE if the task was completed, otherwise FALSE.
 *
 * \remark \a object Must be a Circle object and is not checked!
 */
static bool o_break_circle(GschemToplevel *w_current, GedaObject *object)
{
  GedaPoint point1;
  GedaPoint point2;

  bool result = FALSE;

  point1.x = w_current->first_wx;
  point1.y = w_current->first_wy;

  if (geda_circle_includes_point(object->circle, &point1)) {

    point2.x = w_current->second_wx;
    point2.y = w_current->second_wy;

    if (geda_circle_includes_point(object->circle, &point2)) {

      GedaObject *new_obj;
      double      radians;
      int  color;
      int  cx;
      int  cy;
      int  radius;
      int  arc_sweep;
      int  start_angle;
      int  end_angle;
      int  first_angle;

      color   = object->color;
      cx      = object->circle->center_x;
      cy      = object->circle->center_y;
      radius  = object->circle->radius;

      /* Get angle to first point */
      radians = atan2((point1.y - cy), (point1.x - cx));

      /* If negative, make the angle positive */
      while (radians < 0) {
        radians += 2 * M_PI;
      }
      start_angle = geda_math_radians_to_degrees(radians);

      /* Get angle to second point */
      radians = atan2((point2.y - cy), (point2.x - cx));

      /* If negative, make the angle positive */
      while (radians < 0) {
        radians += 2 * M_PI;
      }

      end_angle = geda_math_radians_to_degrees(radians);

      if (end_angle > start_angle){
        first_angle = end_angle;
        arc_sweep   = 360 - end_angle + start_angle;
      }
      else {
        first_angle = start_angle;
        arc_sweep   = end_angle - start_angle;
      }

      new_obj = geda_arc_object_new (color, cx, cy, radius, first_angle, arc_sweep);

      /* Set line options of the path to the values used by the old path */
      geda_set_object_line_options(new_obj, object->line_options);

      /* Add the new Arc to the page */
      geda_struct_page_append_object (Current_Page, new_obj);

      /* Delete the Circle */
      o_delete(w_current, object);

      result = TRUE;
    }
    else
      v_log_message(_("Point 2 missed x=%d, y=%d\n"), point2.x, point2.y);
  }
  else
    v_log_message(_("Point 1 missed x=%d, y=%d\n"), point1.x, point1.y);

  return result;
}

/*!
 * \brief Break a Path
 * \par Function Description
 *  If both points specified in \a w_current are on the Path object,
 *  then creates a degenerate path whose vertices are the remnants
 *  of what remains of the path. The starting and ending points are
 *  the break points and in all cases, the replacement path is not
 *  closed. If the original path was not closed then two paths are
 *  generated as a result of the break.
 *
 * \returns TRUE if the task was completed, otherwise FALSE.
 *
 * \remark \a object Must be a Path object and is not checked!
 */
static bool o_break_path(GschemToplevel *w_current, GedaObject *object)
{
  bool      result = FALSE;
  bool      closed;
  GArray   *points;
  GedaLine  segments[2];
  GedaPoint point1;
  GedaPoint point2;
  int       vertex1;
  int       vertex2;
  int       segment1;
  int       segment2;

  points = g_array_new (FALSE, FALSE, sizeof(GedaPoint));
  closed = geda_struct_path_to_polygon (object->path, points);

  if (closed) {
    point1 = g_array_index (points, GedaPoint, points->len - 1);
    points = g_array_prepend_val (points, point1);
  }

#if DEBUG
  int i;
  for (i = 0; i < points->len; i++) {
    GedaPoint vertex = g_array_index (points, GedaPoint, i);
    fprintf(stderr, "vertex[%d] x=%d, y=%d)\n", i, vertex.x, vertex.y);
  }
#endif

  /* Internal wrapper to set node and segment by reference. Note that
   * returned segment corresponds to the index in points of the first
   * point of the segment that was hit and a returned node cooresponds
   * to the second point */
  void checkpoint (GedaPoint *point, int *segment, int *node, GedaLine *line) {

    int   i;
    GedaPoint vertex;
    GedaPoint snapped;

    /* Important: Initialize both values to Zero or the main routine
     * will not knows which, if either, was set */
    *segment = 0;
    *node    = 0;

    /* Get snap outside of loop */
    snapped.x = snap_grid (w_current, point->x);
    snapped.y = snap_grid (w_current, point->y);

    vertex = g_array_index (points, GedaPoint, 0);

    for(i = 1; i < points->len; i++) {

      bool  do_snap;
      GedaPoint tmp;  /* Point to check, is point possibly adjusted for snap */

      line->x[0] = vertex.x;
      line->y[0] = vertex.y;

      vertex = g_array_index (points, GedaPoint, i);

      line->x[1] = vertex.x;
      line->y[1] = vertex.y;

      /* Set flag to snap if horizontal or vertical segment */
      do_snap = line->x[0] == line->x[1] || line->y[0] == line->y[1];

      if (do_snap) {
        tmp.x = snapped.x;
        tmp.y = snapped.y;
      }
      else {
        tmp.x = point->x;
        tmp.y = point->y;
      }

      /* First check if point was the second end point */
      if (tmp.x == line->x[1] && tmp.y == line->y[1])  {
        point->x = tmp.x;
        point->y = tmp.y;
       *node = i;
        break;
      }

      /* Else check if point is some other point on the line segment */
      if (geda_line_includes_point(line, &tmp)) {
        point->x = tmp.x;
        point->y = tmp.y;
       *segment = i;
        break;
      }
    }
  }

  point1.x = w_current->first_wx;
  point1.y = w_current->first_wy;

  checkpoint(&point1, &segment1, &vertex1, &segments[0]);

  if (segment1 || vertex1) { /* if a segment or a vertex then 1 hit path */

    point2.x = w_current->second_wx;
    point2.y = w_current->second_wy;

    checkpoint(&point2, &segment2, &vertex2, &segments[1]);

    if (segment2 || vertex2) { /* if a segment or a vertex then 2 hit path */

#if DEBUG
  fprintf(stderr, "pre swap segment1[%d] vertex1[%d], segment2=[%d] vertex2=[%d]\n", segment1, vertex1, segment2, vertex2);
#endif

      /* Both points landed on the polygon, we're good to go,... */

      GArray     *vertices;
      GedaObject *new_path;
      GedaPoint  *start;
      GedaPoint  *end;
      int         done;
      int         index;
      int         stop;
      int         swap = TRUE;

      /* Determine the starting and ending points, normally we would start on
       * the second point and keep vertices until we reach the first point,
       * but if both points are on the same segment then it should be obvious
       * the user wants to "break" the path between the points, regardless of
       * which was first. The following conditional checks for this situation
       */
      if (segment1 == segment2) { /* If both points are on the same segment */

        GedaPoint  previous;
        double     distance1;
        double     distance2;

        previous  = g_array_index (points, GedaPoint, segment1 - 1);
        int Px    = previous.x;
        int Py    = previous.y;

#if HAVE_HYPOT
        distance1 = hypot((point1.x-Px), (point1.y-Py));
        distance2 = hypot((point2.x-Px), (point2.y-Py));
#else
        distance1 = sqrt((point1.x-Px) * (point1.x-Px) + (point1.y-Py) * (point1.y-Py));
        distance2 = sqrt((point2.x-Px) * (point2.x-Px) + (point2.y-Py) * (point2.y-Py));
#endif

        /* The point farthest from the previous is the starting point */
        if (distance2 > distance1) {
          start = &point2;
          end   = &point1;
        }
        else {
          start = &point1;
          end   = &point2;
          swap  = FALSE;
        }
      }
      else { /* Points are not on the same segment of the path */

        /* If path is open then the starting point is the greater,
         * only because this path is created first */
        if (closed || (segment2 > segment1)) {
          start = &point2;
          end   = &point1;
        }
        else {
          start = &point1;
          end   = &point2;
          swap  = FALSE;
        }
      }

      /* Update the index flags if points were swapped */
      if (swap) {
        int tmp_seg  = segment1;
        int tmp_node = vertex1;
        segment1     = segment2;
        vertex1      = vertex2;
        segment2     = tmp_seg;
        vertex2      = tmp_node;
      }

      /* Set the starting index */
      if (vertex1) {
        index = vertex1 + 1;
      }
      else {
        index = segment1;
      }

      /* Use magic to calculate the final index */
      if (vertex2) {
        segment2 = vertex1 - 1;
      }

      if (closed) {

        /* The number of negative stops will be segment1 - 1 */
        stop = segment2 - segment1;

        /* stop = 0 if same segment, so must rotate completely */
        if (stop < 1) {                   /* add the potion */
          stop = stop + points->len - 1;  /* in the right proportion */
        }
      }
      else {
        stop = points->len; /* Will break at end of table */
      }

      vertices = g_array_new (FALSE, FALSE, sizeof(GedaPoint));

      /* Add the starting point */
      g_array_append_val (vertices, *start);

#if DEBUG
  fprintf(stderr, "start x=%d, y=%d)\n", start->x, start->y);
#endif

      done = 0; /* is the vertex counter, not boolean! */

      do {                /* gather vertices, while */

        GedaPoint vertex;

        if (index == points->len) { /* rotating around in data table */
          if (closed) {
            index = 1;
          }
          else {
            break;                  /* Or not, path was open */
          }
        }

        vertex = g_array_index (points, GedaPoint, index);
        g_array_append_val (vertices, vertex);

#if DEBUG
  fprintf(stderr, "array 2 vertex[%d] x=%d, y=%d)\n", index, vertex.x, vertex.y);
#endif

        done++;
        index++;

      } while (done != stop); /* until vertex count is not stop (because) */

      if (closed) {
        g_array_append_val (vertices, *end);  /* there is one more vertex */
      }

      new_path = geda_path_object_new_from_polygon(vertices, object->color);

      /* Set line options of the path to the values used by the old path */
      geda_set_object_line_options(new_path, object->line_options);

      /* Add the new path to the page */
      geda_struct_page_append_object (Current_Page, new_path);

      /* If broke an open path then then need to add a second path
       * that was created as a result of the break */
      if (!closed) { /* Add path for vertices 0 thru (*end) point */

        g_array_free (vertices, TRUE);

        vertices = g_array_new (FALSE, FALSE, sizeof(GedaPoint));

        index = 0;
        if (segment1 == segment2) {
           stop  = segment1;
        }
        else {
          stop  = segment1 > segment2 ? segment2 : segment1;
        }

        while (index != stop) {
          GedaPoint vertex = g_array_index (points, GedaPoint, index);
          g_array_append_val (vertices, vertex);

#if DEBUG
  fprintf(stderr, "array 2 vertex[%d] x=%d, y=%d)\n", index, vertex.x, vertex.y);
#endif

          index++;
        }

        g_array_append_val (vertices, *end);  /* there is one more vertex */

        new_path = geda_path_object_new_from_polygon(vertices, object->color);

        /* Set line options of the path to the values used by the old path */
        geda_set_object_line_options(new_path, object->line_options);

        /* Add the new path to the page */
        geda_struct_page_append_object (Current_Page, new_path);
      }

      /* Delete the old path */
      o_delete(w_current, object);

      g_array_free (vertices, TRUE);

      result = TRUE;
    }
    else
      v_log_message(_("Point 2 missed x=%d, y=%d\n"), point2.x, point2.y);
  }
  else
    v_log_message(_("Point 1 missed x=%d, y=%d\n"), point1.x, point1.y);

  g_array_free (points, TRUE);

  return result;
}

/*!
 * \brief Determine if Linear boundary can bound a given Projectile
 * \par Function Description
 *  Determines if \a projectile can intersect a Linear. \a point is set
 *  to the intersection if point exist and does not already intersect.
 *
 * \returns TRUE or FALSE
 *
 * \remark boundary Must be a GedaLine object and is not checked!
 */
static bool o_break_line(GschemToplevel *w_current, GedaObject *object)
{
  bool  result = FALSE;
  bool  do_snap;
  GedaPoint point1;
  GedaPoint point2;

  do_snap = object->line->x[0] == object->line->x[1] ||
            object->line->y[0] == object->line->y[1];

  if (do_snap) {
    point1.x = snap_grid (w_current, w_current->first_wx);
    point1.y = snap_grid (w_current, w_current->first_wy);
  }
  else {
    point1.x = w_current->first_wx;
    point1.y = w_current->first_wy;
  }

  if (geda_line_includes_point(object->line, &point1) &&
     !geda_line_object_is_endpoint(object, &point1))
  {

    if (do_snap) {
      point2.x = snap_grid (w_current, w_current->second_wx);
      point2.y = snap_grid (w_current, w_current->second_wy);
    }
    else {
      point2.x = w_current->second_wx;
      point2.y = w_current->second_wy;
    }

    if (geda_line_includes_point(object->line, &point2) &&
       !geda_line_object_is_endpoint(object, &point2))
    {
      int end1 = geda_line_object_get_closest_endpoint(object, point1.x, point1.y);
      int end2 = !end1;

      GedaObject *new_line = geda_line_object_copy(object);

      /* Temporarily remove the object from the screen */
      object->dont_redraw = TRUE;
      o_invalidate_object (w_current, object);

      /* Modify the orginal line */
      object->line->x[end2] = point1.x;
      object->line->y[end2] = point1.y;

      /* Redraw the original line after modifications */
      object->dont_redraw = FALSE;
      o_invalidate_object (w_current, object);

      new_line->line->x[end1] = point2.x;
      new_line->line->y[end1] = point2.y;

      geda_struct_page_append_object (Current_Page, new_line);
      result = TRUE;
    }
  }

  return result;
}

/*!
 * \brief Determine if a Circular boundary can bound a given Projectile
 * \par Function Description
 *  Determines if \a projectile can intersect a Circle. \a point is set
 *  to the intersection if point exist and does not already intersect.
 *
 * \returns TRUE or FALSE
 *
 * \remark boundary Must be a Circle object and is not checked!
 */
static bool o_break_net(GschemToplevel *w_current, GedaObject *object)
{
  bool  result = FALSE;
  bool  do_snap;
  GedaPoint point1;
  GedaPoint point2;

  do_snap = object->line->x[0] == object->line->x[1] ||
            object->line->y[0] == object->line->y[1];

  if (do_snap) {
    point1.x = snap_grid (w_current, w_current->first_wx);
    point1.y = snap_grid (w_current, w_current->first_wy);
  }
  else {
    point1.x = w_current->first_wx;
    point1.y = w_current->first_wy;
  }

  if (geda_line_includes_point(object->line, &point1) &&
     !geda_line_object_is_endpoint(object, &point1))
  {
    if (do_snap) {
      point2.x = snap_grid (w_current, w_current->second_wx);
      point2.y = snap_grid (w_current, w_current->second_wy);
    }
    else {
      point2.x = w_current->second_wx;
      point2.y = w_current->second_wy;
    }

    if (geda_line_includes_point(object->line, &point2) &&
       !geda_line_object_is_endpoint(object, &point2))
    {
      int end1 = geda_line_object_get_closest_endpoint(object, point1.x, point1.y);
      int end2 = !end1;

      GedaObject *new_obj;

      if (object->type == OBJ_NET) {
        new_obj = geda_net_object_copy(object);
      }
      else {
        new_obj = geda_bus_object_copy(object);
      }

      /* Temporarily remove the object from the screen */
      object->dont_redraw = TRUE;
      o_invalidate_object (w_current, object);

      geda_struct_conn_remove_object (object);

      /* Modify the orginal line */
      object->line->x[end2] = point1.x;
      object->line->y[end2] = point1.y;

      /* Redraw the original object after modifications */
      object->dont_redraw = FALSE;

      geda_object_update (object);
      o_invalidate_object (w_current, object);

      new_obj->line->x[end1] = point2.x;
      new_obj->line->y[end1] = point2.y;

      geda_struct_page_append_object (Current_Page, new_obj);

      result = TRUE;
    }
  }

  return result;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void o_break_snap_object(GschemToplevel *w_current, GedaObject *object)
{
  GedaPoint point;
  double dist;
  double w_slack;
  int x, y;

  point.x = w_current->first_wx;
  point.y = w_current->first_wy;

  if (geda_object_get_nearest_point(object, point.x, point.y, &x, &y)) {

    dist    = geda_distance(x, y, point.x, point.y);
    w_slack = WORLDabs (w_current, w_current->select_slack_pixels) / 2.5;

    if (dist && (w_slack > dist)) {
      w_current->first_wx = x;
      w_current->first_wy = y;
    }
  }

  point.x = w_current->second_wx;
  point.y = w_current->second_wy;

  if (geda_object_get_nearest_point(object, point.x, point.y, &x, &y)) {

    dist    = geda_distance(x, y, point.x, point.y);
    w_slack = WORLDabs (w_current, w_current->select_slack_pixels) / 3;

    if (dist && (w_slack > dist)) {
      w_current->second_wx = x;
      w_current->second_wy = y;
    }
  }
}

/*!
 * \brief Determine if an Object can bound a given Projectile
 * \par Function Description
 *  Calls appropriate o_break_can_xx function based on the type
 *  of object the boundary represents. The selected intersection
 *  will be determined by the handler and returned in \a point if
 *  point is not NULL.
 *
 * \returns TRUE or FALSE
 */
static bool o_break_object(GschemToplevel *w_current, GedaObject *object)
{
  bool (*breaker)(GschemToplevel *, GedaObject *);

  o_break_snap_object(w_current, object);

  switch (object->type) {
    case OBJ_ARC:
      breaker = o_break_arc;    break;
    case OBJ_BOX:
      breaker = o_break_box;    break;
    case OBJ_PATH:
      breaker = o_break_path;   break;
    case OBJ_LINE:
      breaker = o_break_line;   break;
    case OBJ_NET:
    case OBJ_BUS:
      breaker = o_break_net;    break;
    case OBJ_CIRCLE:
      breaker = o_break_circle; break;
    default:
      return FALSE;
  }
  /* TODO Check for fill or hatch and warn user */
  return breaker (w_current, object);
}

/* ----------------------- Public Event Handlers ---------------------- */

/*!
 * \brief Start a Projection operation
 * \par Function Description
 */
int o_break_start(GschemToplevel *w_current, int w_x, int w_y)
{
  GList  *object_list;

  bool breakable;
  int  count;
  int  status;

  object_list = geda_list_get_glist (Current_Selection);

  count = g_list_length(object_list);

  if (count == 0) {

    GedaObject *o_current = o_find_get_hit (w_current, w_x, w_y);

    if (o_current) {
      if (o_break_is_breakable(o_current)) {
        breakable = TRUE;
      }
      else {
        status = SELECT;
      }
      o_select_object (w_current, o_current, SINGLE, 0);
    }
    else {
      w_current->first_wx   = -0;
      w_current->first_wy   = -0;
      status    = STARTBREAK;        /* call this again on next click */
      breakable = FALSE;
    }
  }
  else if (count == 1) {
    breakable = TRUE;
  }
  else {
    breakable = FALSE;
    status    = SELECT;
  }

  if (breakable) {
    w_current->first_wx    = w_x;
    w_current->first_wy    = w_y;
    status = ENDBREAK;
  }

  return status;
}

/*!
 * \brief Projection event Selection
 * \par Function Description
 *  This function is called after a button press event and one
 *  object had been previously selected to response to either
 *  BREAK or ENDBREAK events.
 */
int o_break_end (GschemToplevel *w_current, int x, int y)
{
  GList      *object_list;
  GedaObject *object;

  int status;

  w_current->second_wx = x;
  w_current->second_wy = y;

  object_list = geda_list_get_glist (Current_Selection);
  object      = object_list->data;

  if (o_break_object(w_current, object)) {
    geda_struct_object_set_page_changed(object);
    o_select_unselect_all (w_current);
    o_undo_savestate (w_current, UNDO_ALL);
    i_status_update_sensitivities(w_current);
    status = 0;
  }
  else {
    status = ENDBREAK;
  }

  return (status & 2);
}

/* ------------------------- Action Processor ------------------------- */

/*!
 * \brief Break Hot
 * \par Function Description
 *  Called to process one or more selected objects when Project mode was
 *  initiated using the keyboard or mouse. The direction each object is
 *  to be projected is based on cursor position relative to the objects,
 *  and need not be the same for each object. The closest boundary in the
 *  path of each projectile will be the target boundary.
 */
void o_break_hot (GschemToplevel *w_current, GList *object_list, int x, int y)
{
  int count;

  count = g_list_length(object_list);

  if (count == 1) {
    if (o_break_is_breakable(object_list->data)) {
      w_current->first_wx    = x;
      w_current->first_wy    = y;
      w_current->event_state = ENDBREAK;
    }
  }
}

/*!
 * \brief Projection Mode Activated, Interrogate Selection
 * \par Function Description
 *  This function is called at the beginning of a project operation
 *  to determine how to proceed based on the number and type of objects
 *  selected.
 */
int o_break_interrogate (GschemToplevel *w_current, GList *object_list)
{
  int count;
  int status;

  count = g_list_length(object_list);

  if (count == 0) {
    w_current->first_wx   = -0;
    w_current->first_wy   = -0;
    status = STARTBREAK;       /* call o_break_start on next click */
  }
  else if (count == 1) {       /* Can be boundary or a projectile */

    /* call o_break_end on next click if valid object selected */

    if (o_break_is_breakable(object_list->data)) {
      status = STARTBREAK;
    }
    else {
      status = SELECT; /* Why not w_current->event_state*/
    }
  }
  else {
    status = SELECT;
  }

  return status;
}

/** @} endgroup Break-Operations */
