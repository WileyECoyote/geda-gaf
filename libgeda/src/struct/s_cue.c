/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/*
 * File: s_cue.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 *
 * Copyright (C) 1998-2016 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <math.h>                /* hypot function */

#include <libgeda_priv.h>

#include <geda_debug.h>

/*! \brief Get Locations for Junctions and Unconnected Cues
 *  \par Function Description
 *   This function populates the supplied GArrays with POINT locations of
 *   junction and unconnected points. This function appends new POINT to
 *   the GArrays and leaves existing GArray contents unchanged. Both array
 *   arguments are optional, but would only make sense to supply a least
 *   one array. The array argument for undisred points should be set to
 *   NULL.
 *
 *  \param [in]     objects     GedaObjects to get the cue points for
 *  \param [in,out] junctions   GArray of GedaPoint to contain the coordinates of junctions
 *  \param [in,out] unconnected GArray of GedaPoint to contain the coordinates of unconnected
 *                              endpoints.
 *
 *  example 1: geda_struct_cue_get_locations (list, junctions, noconnects);
 *
 *  example 2: geda_struct_cue_get_locations (list, junctions, NULL);
 *
 */
void geda_struct_cue_get_locations(const GList *objects, GArray *junctions,
                                                         GArray *unconnected)
{
  const GList    *iter;
        GedaPoint point;

  void add_end_cues (GedaObject *object, int end) {

    int conn_count = 0;
    int conn_type  = CONN_ENDPOINT;
    int x          = object->line->x[end], y = object->line->y[end];
    GList *iter;

    for (iter = object->conn_list; iter != NULL; iter = iter->next) {

      CONN *conn = (CONN *) iter->data;

      if ((conn->x != x) || (conn->y != y))
        continue;

      if (conn->type == CONN_MIDPOINT) {
        /* If it's a mid-line connection, we can stop already. */
        conn_type = CONN_MIDPOINT;
        break;
      }

      conn_count++;
    }

    /* Draw a midpoint, if necessary */
    if ((conn_type == CONN_MIDPOINT) ||
       ((object->type == OBJ_NET) && (conn_count > 1)))
    {
      if (junctions) {
        point.x = x;
        point.y = y;
        g_array_append_val(junctions, point);
      }
      return;
    }

    switch (object->type) {
      case OBJ_NET:
      case OBJ_PIN:
        if (conn_count > 0) break;
        if (unconnected) {
          point.x = x;
          point.y = y;
          g_array_append_val(unconnected, point);
        }
        break;

      case OBJ_BUS:
        break;
      default:
        break;
    }
  }

  void add_mid_cues (GedaObject *object) {

    if (junctions) {

      GList *iter;

      for (iter = object->conn_list; iter != NULL; iter = iter->next) {
        CONN *conn = (CONN *) iter->data;
        if (conn->type == CONN_MIDPOINT) {
          point.x = conn->x;
          point.y = conn->y;
          g_array_append_val(junctions, point);
        }
      }
    }
  }

  iter = objects;

  while (iter != NULL) {

    GedaObject *object = iter->data;

    if (object->type == OBJ_NET || object->type == OBJ_BUS) {
      add_mid_cues (object);
      add_end_cues (object, 0);
      add_end_cues (object, 1);
    }
    else if (object->type == OBJ_PIN) {
      add_end_cues (object, object->pin->whichend);
    }

    iter = iter->next;
  }
}

/*! \brief Write Fill-Box Cue to Postscript file
 *  \par Function Description
 *   Write postscript instructions to \a fp to draw an unconnected
 *   end-point, aka a small box, at the given coordinates.
 */
static void s_cue_postscript_fillbox(GedaToplevel *toplevel, FILE *fp,
                                     int x, int y)
{
  int offset;
  int offset2;

  /* hard coded values */
  offset = CUE_BOX_SIZE;
  offset2 = offset * 2;

  f_print_set_color(toplevel, fp, NET_ENDPOINT_COLOR);

  fprintf(fp, "%d %d %d %d fbox\n", offset2, offset2, x-offset, y-offset);
}

/*! \brief Write Junction to Postscript file
 *  \par Function Description
 *   Write postscript instructions to \a fp to draw a junction,
 *   aka a small circle, at the given coordinates.
 */
static void s_cue_postscript_junction (GedaToplevel *toplevel, FILE *fp,
                                       int x, int y, int bus_involved)
{
  int offset2;

  if (bus_involved) {
    offset2 = JUNCTION_CUE_SIZE_BUS;
  }
  else {
    offset2 = JUNCTION_CUE_SIZE_NET;
  }

  f_print_set_color(toplevel, fp, JUNCTION_COLOR);

  fprintf(fp, "newpath\n");
  fprintf(fp, "%d %d\n", x, y);
  fprintf(fp, "%d\n", offset2 / 2);
  fprintf(fp, "0 360 arc\n");
  fprintf(fp, "fill\n");
}

/*! \brief  Write Arrow to Postscript file
 *  \par Function Description
 *   Write postscript instructions to \a fp to draw an arrow,
 *   at given the coordinates, normally at the end of a net.
 */
static void s_cue_postscript_arrow (GedaToplevel *toplevel, FILE *fp,
                                    int x, int y, int dx, int dy)
{
  int offset = CUE_BOX_SIZE;

  f_print_set_color (toplevel, fp, JUNCTION_COLOR);

  fprintf (fp, "gsave\n");

  /* We compute a transformation so that the arrowhead is aligned
   * with the net segment. */
  double len = hypot (dx, dy);

  if (len != 0) {
    double c = dx / len;
    double s = dy / len;
    fprintf (fp, "[ %.2f %.2f %.2f %.2f %d %d ] concat\n", c, -s, s, c, x, y);
  }
  else {
    fprintf (fp, "%d %d translate\n", x, y); /* Translate to centre point of the arrow */
  }

  /* Now draw a simple arrowhead */
  fprintf (fp, "newpath\n");
  fprintf (fp, "%d %d moveto\n", -offset, -offset);
  fprintf (fp, "%d %d lineto\n", -offset, offset);
  fprintf (fp, "%d %d lineto\n", offset, 0);
  fprintf (fp, "closepath\n"
               "fill\n"
               "grestore\n");
}


/*! \brief Low-level Write Cue to Postscript file
 *  \par Function Description
 *   Wrapper for s_cue_postscript_arrow, s_cue_postscript_fillbox
 *   and s_cue_postscript_junction.
 */
static void s_cue_output_lowlevel(GedaToplevel *toplevel,
                                  GedaObject   *object,
                                  int           whichone,
                                  FILE         *fp,
                                  int           output_type)
{
  GList *cl_current;

  int x            = object->line->x[whichone];
  int y            = object->line->y[whichone];
  int type         = CONN_ENDPOINT;
  int count        = 0;
  int done         = FALSE;
  int bus_involved = FALSE;

  if (object->type == OBJ_BUS ||
     (object->type == OBJ_PIN && object->pin->node_type == PIN_BUS_NODE))
    bus_involved = TRUE;

  cl_current = object->conn_list;

  while (cl_current != NULL && !done) {

    CONN *conn = (CONN *) cl_current->data;

    if (conn->x == x && conn->y == y) {

      if (conn->other_object &&
         (conn->other_object->type == OBJ_BUS ||
         (conn->other_object->type == OBJ_PIN &&
          conn->other_object->pin->node_type == PIN_BUS_NODE)))
        bus_involved=TRUE;

      switch (conn->type) {

        case (CONN_ENDPOINT):
          count++;
          break;

        case (CONN_MIDPOINT):
          type  = CONN_MIDPOINT;
          done  = TRUE;
          count = 0;
          break;
      }
    }

    cl_current = g_list_next(cl_current);
  }

#if DEBUG
  printf("type: %d count: %d\n", type, count);
#endif

  if (type == CONN_ENDPOINT) {
    if (object->type == OBJ_NET || object->type == OBJ_PIN) {
      if (count < 1) {        /* Didn't find anything connected there */
        if ((object->type == OBJ_NET) && geda_net_object_is_fully_connected (object))
        {
          /* Probably connected, so draw friendly arrowhead. The
           * additional parameters are needed to allow the arrowhead
           * to be pointed in the direction of the net. */
          if (output_type == POSTSCRIPT) {
            s_cue_postscript_arrow (toplevel, fp, x, y,
                                    object->line->x[whichone] - object->line->x[!whichone],
                                    object->line->y[whichone] - object->line->y[!whichone]);
          }
        }
        else {
          s_cue_postscript_fillbox (toplevel, fp, x, y);
        }
      }
      else if (count >= 2) {
        if (output_type == POSTSCRIPT)
          s_cue_postscript_junction (toplevel, fp, x, y, bus_involved);
      }
    }
  }
}

/*! \brief Low-level Write Mid-Point Cue to Postscript file
 *  \par Function Description
 *   Wrapper for s_cue_postscript_junction.
 */
static void s_cue_output_lowlevel_midpoints(GedaToplevel *toplevel,
                                            GedaObject   *object,
                                            FILE         *fp,
                                            int           output_type)
{
  GList *cl_current;
  CONN  *conn;
  int    x, y;
  int    bus_involved = FALSE;

  if (object->type == OBJ_BUS)
    bus_involved = TRUE;

  cl_current = object->conn_list;

  while (cl_current != NULL) {
    conn = (CONN *) cl_current->data;

    if  (conn->type == CONN_MIDPOINT) {

      x = conn->x;
      y = conn->y;

      if (conn->other_object && conn->other_object->type == OBJ_BUS)
        bus_involved = TRUE;

      if (output_type == POSTSCRIPT) {
        s_cue_postscript_junction (toplevel, fp, x, y, bus_involved);
      }
    }

    cl_current = g_list_next(cl_current);
  }
}

/*! \brief Output cues for a single object
 *
 *  \par Function Description
 *  Cues are drawn on pins, nets and buses.
 *  Two types of cues are drawn:
 *   - endpoint cues (identifying unconnected ends of objects)
 *   - junction cues (identifying net/pin/bus junctions)
 *
 *  \param [in] toplevel   The GedaToplevel object
 *  \param [in] object     The GedaObject to output cues for
 *  \param [in] fp         The file handle to output to
 *  \param [in] type       The type of output being produced
 */
void geda_struct_cue_output_single(GedaToplevel *toplevel,
                                   GedaObject   *object,
                                   FILE         *fp,
                                   int           type)
{
  g_return_if_fail (object != NULL);

  switch (object->type) {
    case (OBJ_NET):
      s_cue_output_lowlevel(toplevel, object, 0, fp, type);
      s_cue_output_lowlevel(toplevel, object, 1, fp, type);
      break;
    case (OBJ_BUS):
      s_cue_output_lowlevel(toplevel, object, 0, fp, type);
      s_cue_output_lowlevel(toplevel, object, 1, fp, type);
      break;
    case (OBJ_PIN):
      s_cue_output_lowlevel(toplevel, object, object->pin->whichend, fp, type);
      break;
    default:
      return;
  }
  s_cue_output_lowlevel_midpoints(toplevel, object, fp, type);
}

/*! \brief Write All Cues to Postscript file
 *  \par Function Description
 *   Wrapper for geda_struct_cue_output_single and geda_struct_cue_output_all.
 */
void geda_struct_cue_output_all (GedaToplevel *toplevel, const GList *obj_list, FILE *fp, int type)
{
  const GList *iter = obj_list;

  while (iter != NULL) {

    GedaObject *o_current = (GedaObject*)iter->data;

    switch (o_current->type) {
      case (OBJ_NET):
      case (OBJ_BUS):
      case (OBJ_PIN):
        geda_struct_cue_output_single(toplevel, o_current, fp, type);
        break;

      case (OBJ_COMPLEX):
      case (OBJ_PLACEHOLDER):
        geda_struct_cue_output_all(toplevel, o_current->complex->prim_objs, fp, type);
        break;

    }
    iter = g_list_next (iter);
  }
}