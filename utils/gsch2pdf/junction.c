/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */

#include "common.h"

extern bool o_pin_get_position (int *x, int *y, GedaObject *object);

typedef struct st_sweep_event SWEEP_EVENT;
typedef struct st_sweep_status SWEEP_STATUS;

struct st_sweep_status
{
    int y1;
    LINE line;
};

struct st_sweep_event
{
    int y0;
    SWEEP_STATUS status;
};

static int compare_points(gconstpointer a, gconstpointer b)
{
  POINT *point_a = (POINT*) a;
  POINT *point_b = (POINT*) b;

  return (point_a->y - point_b->y);
}

/*! \brief Locate all the junctions on a schematic
 *
 *  \param [in] current the GedaToplevel object
 *  \param [in] objects The objects on the schematic
 *  \param [in,out] junctions A GArray of POINT to contain the coordinates
 *  of junctions.  This function appends new junctions to the GArray and leaves
 *  existing GArray contents unchanged.
 *  \param [in,out] unconnected A GArray of POINT to contain the coordinates
 *  of unconnected endpoints.  This function appends new endpoints to the GArray and leaves
 *  existing GArray contents unchanged.
 */
void junction_locate(const GList *objects, GArray *junctions, GArray *unconnected)
{
  const GList *node = objects;

  GArray *events = g_array_new(FALSE, FALSE, sizeof(SWEEP_EVENT));
  GArray *points = g_array_new(FALSE, FALSE, sizeof(POINT));
  GArray *status = g_array_new(FALSE, FALSE, sizeof(SWEEP_STATUS));

  while (node != NULL) {
    GedaObject *object = (GedaObject*) node->data;
    if (object->type == OBJ_NET) {
      SWEEP_EVENT event;
      POINT point;
      event.y0 = min(object->line->y[0], object->line->y[1]);
      event.status.y1 = max(object->line->y[0], object->line->y[1]);
      LINE sline;
      sline.x[0] = object->line->x[0];
      sline.y[0] = object->line->y[0];
      sline.x[1] = object->line->x[1];
      sline.y[1] = object->line->y[1];
      event.status.line = sline;
      g_array_append_val(events, event);
      point.x = object->line->x[0];
      point.y = object->line->y[0];
      g_array_append_val(points, point);
      point.x = object->line->x[1];
      point.y = object->line->y[1];
      g_array_append_val(points, point);
    }
    else if (object->type == OBJ_PIN) {
      POINT point;
      o_pin_get_position(&point.x, &point.y, object);
      g_array_append_val(points, point);
    }
    else if ((object->type == OBJ_COMPLEX) || (object->type == OBJ_PLACEHOLDER)) {
      const GList *node2 = object->complex->prim_objs;
      while (node2 != NULL) {
        GedaObject *object2 = (GedaObject*) node2->data;
        if (object2->type == OBJ_PIN) {
          POINT point2;
          o_pin_get_position(&point2.x, &point2.y, object2);
          g_array_append_val(points, point2);
        }
        node2 = g_list_next(node2);
      }
    }
    node = g_list_next(node);
  }

  g_array_sort(points, compare_points);

  while ( points->len > 0 ) {
    int count;
    int index;
    POINT current = g_array_index(points, POINT, 0);

    /* add new segments that intersect the sweep line */
    index = 0;
    while ( index < events->len ) {
      SWEEP_EVENT *event = &g_array_index(events, SWEEP_EVENT, index);
      if ( current.y >= event->y0 ) {
        SWEEP_STATUS st = event->status;
        g_array_append_val(status, st);
        g_array_remove_index(events, index);
      } else {
        index++;
      }
    }

    /* remove status no longer intersecting sweep line */
    index = status->len;
    while ( index-- > 0 ) {
      if ( current.y > g_array_index(status, SWEEP_STATUS, index).y1 ) {
        g_array_remove_index_fast(status, index);
      }
    }

    count = 0;

    /* test for endpoint intersections */
    for (index=0; index<points->len; index++) {
      POINT *point = &g_array_index(points, POINT, index);
      if (memcmp(&current, point, sizeof(POINT)) == 0) {
        count++;
      }
    }

    /* test for midpoint intersections */
    for (index=0; index<status->len; index++) {
      SWEEP_STATUS *st = &g_array_index(status, SWEEP_STATUS, index);
      if (st->line.x[0] == st->line.x[1]) {
        if (st->line.x[0] == current.x) {
          int ymin = min(st->line.y[0], st->line.y[1]);
          int ymax = max(st->line.y[0], st->line.y[1]);
          if ((current.y > ymin) && (current.y < ymax)) {
            count += 2;
          }
        }
      } else if (st->line.y[0] == st->line.y[1]) {
        if (st->line.y[0] == current.y) {
          int xmin = min(st->line.x[0], st->line.x[1]);
          int xmax = max(st->line.x[0], st->line.x[1]);
          if ((current.x > xmin) && (current.x < xmax)) {
            count += 2;
          }
        }
      }
    }

    if (count < 2) {
      if (unconnected != NULL) {
        g_array_append_val(unconnected, current);
      }
    } else if (count > 2) {
      if (junctions != NULL) {
        g_array_append_val(junctions, current);
      }
    }

    /* remove points from array */
    index = 0;
    while ( index < points->len ) {
      POINT *point = &g_array_index(points, POINT, index);
      if ( memcmp(&current, point, sizeof(POINT)) == 0 ) {
        g_array_remove_index(points, index);
      } else {
        index++;
      }
    }
  }

  g_array_free(events, TRUE);
  g_array_free(points, TRUE);
  g_array_free(status, TRUE);
}
