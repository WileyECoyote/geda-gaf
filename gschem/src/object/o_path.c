/* -*- C o_path.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_path.c
 * \brief Low-level module for manipulating Path objects
 * \image html paths.png
 * \image latex paths.png
 */
#include <gschem.h>
#include <geda_debug.h>

#define NUM_BEZIER_SEGMENTS 100
#define NUM_BEZIER_SEGMENTS 100

/*! Default capacity of newly created path objects, in path sections. */
#define TEMP_PATH_DEFAULT_SIZE 8

typedef void (*FILL_FUNC) (GschemToplevel *w_current,
                           COLOR *color, GedaPath *path, int fill_width,
                           int angle1, int pitch1, int angle2, int pitch2);

/*!
 * \brief Calculate path bounding box for rubber purposes
 * \par Function Description
 *  Calculate the bounding box of \a path, returning its bounds in
 *  \a min_x, \a max_y, \a max_x and \a min_y.  If \a path is NULL,
 *  the Path object currently being edited is used, with any required
 *  control point changes applied.
 */
static void
path_rubber_bbox (GschemToplevel *w_current, GedaPath *path,
                  int *min_x, int *max_y, int *max_x, int *min_y)
{
  int new_x, new_y, whichone;
  int grip_no = 0;
  int i;

  if (w_current == NULL) {
    BUG_MSG ("w_current = NULL");
    return;
  }

  if (path == NULL)
    path = w_current->which_object->path;

  *min_x = G_MAXINT;  *max_x = G_MININT;
  *min_y = G_MAXINT;  *max_y = G_MININT;

  new_x = w_current->second_wx;
  new_y = w_current->second_wy;
  whichone = w_current->which_grip;

  for (i = 0; i <  path->num_sections; i++) {

    int x1, y1, x2, y2, x3, y3;
    PATH_SECTION *section = &path->sections[i];

    x1 = section->x1; y1 = section->y1;
    x2 = section->x2; y2 = section->y2;
    x3 = section->x3; y3 = section->y3;

    switch (section->code) {
      case PATH_CURVETO:
        /* Two control point grips */
        if (whichone == grip_no++) {
          x1 = new_x; y1 = new_y;
        }
        if (whichone == grip_no++) {
          x2 = new_x; y2 = new_y;
        }
        *min_x = MIN (*min_x, x1);  *min_y = MIN (*min_y, y1);
        *max_x = MAX (*max_x, x1);  *max_y = MAX (*max_y, y1);
        *min_x = MIN (*min_x, x2);  *min_y = MIN (*min_y, y2);
        *max_x = MAX (*max_x, x2);  *max_y = MAX (*max_y, y2);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        if (whichone == grip_no++) {
          x3 = new_x; y3 = new_y;
        }
        *min_x = MIN (*min_x, x3);  *min_y = MIN (*min_y, y3);
        *max_x = MAX (*max_x, x3);  *max_y = MAX (*max_y, y3);
        /* Fall through */
      case PATH_END:
        break;
    }
  }
}

/*!
 * \brief Add elements to the temporary Path.
 * \par Function Description
 *  Check if the temporary Path object used when interactively
 *  creating paths has room for additional sections. If not, doubles
 *  its capacity.
 *
 * \param [in] w_current   The GschemToplevel object.
 */
static void
path_expand (GschemToplevel *w_current)
{

  GedaPath *p = w_current->temp_path;
  if (p->num_sections == p->num_sections_max) {
    p->num_sections_max *= 2;
    p->sections = g_renew (PATH_SECTION, p->sections,
                           p->num_sections_max);
  }
}

/*!
 * \brief Add new sections to the temporary path while drawing.
 * \par Function Description
 *  Calculates the next section to be added to a path while drawing.
 *  The temporary slots in the GschemToplevel structure are used as
 *  follows:
 *   - first_wx and first_wy contain the location of the next point
 *     that will lie on the path
 *   - second_wx and second_wy contain the location of the next
 *     point's control point.
 *   - third_wx and third_wy contain the location of the previous
 *     point's control point.
 *   - temp_path is the new Path object (i.e. sequence of path
 *     sections that comprise the path drawn so far).
 *
 *  path_next_sections() adds up to two additional sections to the
 *  temporary path, and returns the number of sections added, on the
 *  basis that: a path starts with a MOVETO the first point; two cusp
 *  nodes (control points coincident with the node position) generate a
 *  LINETO section; and a path ends either whenever the user clicks on
 *  either the first or the current node.
 *
 * \param [in] w_current   The GschemToplevel object.
 *
 * \return the number of path sections added.
 */
static int
path_next_sections (GschemToplevel *w_current)
{

  bool cusp_point, cusp_prev, close_path, end_path, start_path;
  GedaPath *p;
  PATH_SECTION *section, *prev_section;
  int x1, y1, x2, y2, x3, y3;
  int save_num_sections;

  if (w_current == NULL) {
    BUG_MSG ("w_current = NULL");
    return 0;
  }

  if (w_current->temp_path == NULL || w_current->temp_path->sections == NULL) {
    BUG_MSG ("invalid temp_path or section");
    return 0;
  }

  x1 = w_current->first_wx;
  y1 = w_current->first_wy;
  x2 = w_current->second_wx;
  y2 = w_current->second_wy;
  x3 = w_current->third_wx;
  y3 = w_current->third_wy;
  p  = w_current->temp_path;

  save_num_sections = p->num_sections;

  /* Check whether the section that is being added is the initial
   * MOVETO.  This is detected if the path is currently empty. */
  start_path = (p->num_sections == 0);

  prev_section = start_path ? NULL : &p->sections[p->num_sections - 1];

  /* Check whether the point that is being added has a handle offset. */
  cusp_point = (w_current->first_wx == w_current->second_wx &&
                w_current->first_wy == w_current->second_wy);

  /* Check whether there's a leftover control handle from the previous
   * point. */
  cusp_prev = (!start_path &&
                prev_section->x3 == x3 &&
                prev_section->y3 == y3);

  /* Check whether the section that is being added closes the path.
   * This is detected if the location of the node is the same as the
   * location of the starting node, and there is at least one section
   * in the path in addition to the initial MOVETO section. */
  section = &p->sections[0];
  close_path = (!start_path
                && x1 == section->x3
                && y1 == section->y3);

  /* Check whether the section that is being added ends the path. This
   * is detected if the location of the node is the same as the
   * location of the previous node. */
  end_path = (!start_path
              && x1 == prev_section->x3
              && y1 == prev_section->y3);

  /* Create section */
  if (start_path) {
    /* At the start of the path, just create the initial MOVETO. */
    path_expand (w_current);
    section = &p->sections[p->num_sections++];
    section->code = PATH_MOVETO;
    section->x3 = x1;
    section->y3 = y1;

  }
  else if (!end_path) {
    path_expand (w_current);
    section = &p->sections[p->num_sections++];

    /* If there are two cusp points, then add a line segment. If the
     * path is being closed, closing the path adds an implicit line
     * segment. */
    if (cusp_prev && cusp_point && close_path) {
      section->code = PATH_END;

    } else if ((cusp_prev && cusp_point) || w_current->CONTROLKEY) {
      section->code = PATH_LINETO;
      section->x3 = x1;
      section->y3 = y1;

    }
    else {
      /* If there are one or more Bezier control points, the section
       * needs to be a CURVETO.  The control point of the current
       * point is mirrored about the point (i.e. the line is kept
       * continuous through the point). */
      section->code = PATH_CURVETO;
      section->x1   = x3;
      section->y1   = y3;
      section->x2   = x1 + (x1 - x2);
      section->y2   = y1 + (y1 - y2);
      section->x3   = x1;
      section->y3   = y1;

      if (close_path) {
        path_expand (w_current);
        section       = &p->sections[p->num_sections++];
        section->code = PATH_END;
      }
    }
  }
  /* Return the number of sections added */
  return p->num_sections - save_num_sections;
}

/*!
 * \brief Begin input for a new path node.
 * \par Function Description
 *  Re-enters path creation mode, saving the current pointer location
 *  as the location of the next path control point.
 */
void
o_path_continue (GschemToplevel *w_current, int w_x, int w_y)
{
  o_path_invalidate_rubber (w_current);

  w_current->first_wx  = w_current->second_wx;
  w_current->first_wy  = w_current->second_wy;
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_path_invalidate_rubber (w_current);
  i_status_action_start(w_current);
}

/*!
 * \brief Close Path in Progress.
 * \par Function Description
 *  Sets first and second coordinates in the top-level to be the
 *  third coordinates of the first section of the temp path in
 *  the top-level and then calls o_path_end passing the same
 *  coordinate so as to have the path closed by o_path_end,
 *  provided the path in progress has at least two sides.
 */
void
o_path_close (GschemToplevel *w_current)
{
  if (w_current->temp_path) {

    GedaPath *path = w_current->temp_path;

    if (path && path->num_sections > 2) {

      PATH_SECTION *section = &path->sections[0];

      w_current->first_wx = w_current->second_wx = section->x3;
      w_current->first_wy = w_current->second_wy = section->y3;

      o_path_end(w_current, section->x3, section->y3);
    }
  }
}

/*!
 * \brief Draw path creation preview.
 * \par Function Description
 *  Draw a preview of the path currently being drawn, including a
 *  helper line showing the control point of the node being drawn
 *  (if applicable).
 */
void
o_path_draw_rubber (GschemToplevel *w_current)
{
  EdaRenderer *renderer;
  GedaObject  *object;
  int added_sections = 0;

  renderer = CairoRenderer;

  /* Draw a helper for when we're dragging a control point */
  if (w_current->first_wx != w_current->second_wx
      || w_current->first_wy != w_current->second_wy) {
    double wwidth = 0;
    cairo_t *cr = eda_renderer_get_cairo_context (renderer);
    GArray *color_map = eda_renderer_get_color_map (renderer);
    int flags = eda_renderer_get_cairo_flags (renderer);

    eda_cairo_line (cr, flags, END_NONE, wwidth,
                    w_current->first_wx, w_current->first_wy,
                    w_current->second_wx, w_current->second_wy);

    eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
    eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
  }
  /* Now draw the rest of the path */

  /* Calculate any new sections */
  added_sections = path_next_sections (w_current);

  /* Setup a tmp object to pass the drawing routine */
   object = geda_path_new();

  object->type  = OBJ_PATH;
  object->color = SELECT_COLOR;
  object->line_options->line_width = 0; /* clamped to 1 pixel in circle_path */
  object->path  = w_current->temp_path;

  eda_renderer_draw (renderer, object);

  /* Get rid of temp object */
  GEDA_UNREF (object);

  /* Throw away the added sections again */
  w_current->temp_path->num_sections -= added_sections;
}

/*!
 * \brief Draw path from GschemToplevel object.
 * \par Function Description
 *  This function draws a path with an exclusive or function over the sheet.
 *  The color of the box is <B>SELECT_COLOR</B>. The path is described by the
 *  two points (<B>w_current->first_wx</B>, <B>w_current->first_wy</B>) and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 * \param [in] w_current  The GschemToplevel object.
 */
void
o_path_draw_rubber_grips (GschemToplevel *w_current)
{
  GedaObject *object;

  /* Setup a tmp object to pass the drawing routine */
  object = geda_path_new();

  object->type  = OBJ_PATH;
  object->color = SELECT_COLOR;
  object->line_options->line_width = 0; /* clamped to 1 pixel in circle_path */
  object->path  = w_current->temp_path;

  object->path = geda_struct_path_copy_modify (w_current->which_object->path, 0, 0,
                                               w_current->second_wx,
                                               w_current->second_wy,
                                               w_current->which_grip);

  eda_renderer_draw (CairoRenderer, object);

  /* Get rid of temp object */
  GEDA_UNREF(object);
}

/*!
 * \brief End the input of a path.
 * \par Function Description
 *  This function ends the process of interactively adding a path to the
 *  current sheet.
 *
 *  It first erases the last temporary path displayed, calculates the
 *  corresponding world coordinates of the two ends of the path and finally
 *  adds a new initialized path object to the list of object of the current
 *  sheet.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        (unused)
 * \param [in] w_y        (unused)
 */
void
o_path_end(GschemToplevel *w_current, int w_x, int w_y)
{
  GedaToplevel *toplevel;
  GedaPath     *path;
  PATH_SECTION *section;
  PATH_SECTION *prev_section;
  bool          result;

  if (w_current == NULL || w_current->toplevel == NULL) {
    BUG_MSG ("invalid pointer to top level");
    result = FALSE;
  }
  else if (w_current->temp_path == NULL) {
    BUG_MSG ("invalid section");
    result = FALSE;
  }
  else if (w_current->temp_path->sections == NULL) {
    BUG_MSG ("invalid section");
    result = FALSE;
  }
  else {

    bool close_path;
    bool end_path;
    bool start_path;
    int  x1, y1, x2, y2;

    o_path_invalidate_rubber (w_current);

    toplevel = w_current->toplevel;

    x1    = w_current->first_wx;
    y1    = w_current->first_wy;
    x2    = w_current->second_wx;
    y2    = w_current->second_wy;
    path  = w_current->temp_path;

    /* Check whether the section that is being added is the initial
     * MOVETO.  This is detected if the path is currently empty. */
    start_path   = (path->num_sections == 0);
    prev_section = start_path ? NULL : &path->sections[path->num_sections - 1];

    /* Check whether the section that is being added closes the path.
     * This is detected if the location of the node is the same as the
     * location of the starting node, and there is at least one section
     * in the path in addition to the initial MOVETO section. */
    section    = &path->sections[0];
    close_path = (!start_path && x1 == section->x3 && y1 == section->y3);

    /* Check whether the section that is being added ends the path. This
     * is detected if the location of the node is the same as the
     * location of the previous node. */
    end_path =(!start_path && x1 == prev_section->x3 && y1 == prev_section->y3);

    /* Add predicted next sections */
    path_next_sections (w_current);

    if (end_path || close_path) {

      /* Create a copy of the tmp path object */
      GedaObject *new_obj = geda_path_object_copy((GedaObject*)path);

      /* Add the New Path object to the page */
      geda_struct_page_append_object (toplevel->page_current, new_obj);
      g_hook_run_object (w_current, ADD_OBJECT_HOOK, new_obj);
      o_undo_savestate_object(w_current, UNDO_ALL, new_obj);

      w_current->rubber_visible = FALSE;

      /* Release tmp path and and clean up path drawing state */
      GEDA_UNREF (path);
      w_current->temp_path = NULL;
      w_current->first_wx  = -1;
      w_current->first_wy  = -1;
      w_current->second_wx = -1;
      w_current->second_wy = -1;
      w_current->third_wx  = -1;
      w_current->third_wy  = -1;

      result = FALSE;
    }
    else {
      /* Leave state as it is and continue path drawing... */
      /* Save the control point coordinates for the next section */
      w_current->third_wx = x2;
      w_current->third_wy = y2;

      result = TRUE;
    }
  }

  i_status_update_action_state(w_current, result);
}

/*!
 * \brief Initialize Variables to input a new path object.
 * \par Function Description
 *  This function initialize variables to input a new path to
 *  the current sheet by resetting the path creation state and
 *  enabling preview ("rubber") drawing.
 *
 *  For details of how #GschemToplevel fields are used during the
 *  path creation process, see path_next_sections().
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        Current x coordinate of pointer in world units.
 * \param [in] w_y        Current y coordinate of pointer in world units.
 */
static void
o_path_init(GschemToplevel *w_current, int w_x, int w_y)
{
  i_status_action_start(w_current);

  /* Reset path creation state */
  if (w_current->temp_path != NULL) {
    w_current->temp_path->num_sections = 0;
  }
  else {

    int size = sizeof(PATH_SECTION) * TEMP_PATH_DEFAULT_SIZE;

    GedaPath *path          = (GedaPath*)geda_path_new ();
    path->sections          = GEDA_MEM_ALLOC0 (size);
    path->num_sections      = 0;
    path->num_sections_max  = TEMP_PATH_DEFAULT_SIZE;
    w_current->temp_path    = path;
  }

  w_current->which_grip     = -1;
  w_current->first_wx       = w_x;
  w_current->first_wy       = w_y;
  w_current->second_wx      = w_x;
  w_current->second_wy      = w_y;
  w_current->third_wx       = w_x;
  w_current->third_wy       = w_y;

  /* Enable preview drawing */
  w_current->rubber_visible = TRUE;
}

/*!
 * \brief Invalidate current path creation screen region.
 * \par Function Description
 *  Invalidates the screen region occupied by the current path
 *  creation preview and control handle helpers.
 */
void
o_path_invalidate_rubber (GschemToplevel *w_current)
{
  int added_sections;
  int min_x, min_y, max_x, max_y;
  int x1, y1, x2, y2;

  /* Calculate any new sections */
  added_sections = path_next_sections (w_current);

  path_rubber_bbox (w_current, w_current->temp_path,
                    &min_x, &max_y, &max_x, &min_y);

  /* Expand the bounding box to include any control handles
   * that are currently being drawn. */
  min_x = MIN (min_x, w_current->second_wx);
  max_x = MAX (max_x, w_current->second_wx);
  min_y = MIN (min_y, w_current->second_wy);
  max_y = MAX (max_y, w_current->second_wy);

  WORLDtoSCREEN (w_current, min_x, max_y, &x1, &y1);
  WORLDtoSCREEN (w_current, max_x, min_y, &x2, &y2);
  o_invalidate_rectangle (w_current, x1, y1, x2, y2);

  w_current->temp_path->num_sections -= added_sections;
}

/*!
 * \brief Invalidate Temporary Path Grips
 * \par Function Description
 *  Retrieves bounds from path_rubber_bbox and invalidate
 *  the bounding region of a Path object.
 */
void
o_path_invalidate_rubber_grips (GschemToplevel *w_current)
{
  int min_x, min_y, max_x, max_y;
  int x1, y1, x2, y2;

  path_rubber_bbox (w_current, NULL,
                    &min_x, &max_y, &max_x, &min_y);

  WORLDtoSCREEN (w_current, min_x, max_y, &x1, &y1);
  WORLDtoSCREEN (w_current, max_x, min_y, &x2, &y2);
  o_invalidate_rectangle (w_current, x1, y1, x2, y2);
}

/*!
 * \brief Give feedback on path creation during mouse movement.
 * \par Function Description
 *  If the user is currently in the process of creating a path node
 *  (i.e. has mouse button pressed), moves the next node's control
 *  point.  If the user has not yet pressed the mouse button to start
 *  defining a path node, moves the next node's location and control
 *  point together.
 */
void
o_path_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  o_path_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* if the control key was pressed then draw ortho lines */
  if (w_current->CONTROLKEY) {

    int diff_x = abs(w_current->second_wx - w_current->first_wx);
    int diff_y = abs(w_current->second_wy - w_current->first_wy);

    if (diff_x >= diff_y) {
      w_current->second_wy = w_current->first_wy;
    }
    else {
      w_current->second_wx = w_current->first_wx;
    }
  }

  o_path_invalidate_rubber (w_current);
}

/*!
 * \brief Draw temporary path while dragging end.
 * \par Function Description
 *  This function manages the erase/update/draw process of temporary path
 *  when modifying one end of the path.
 *  The path is described by four <B>*w_current</B> variables : the first end
 *  of the path is (<B>first_wx</B>,<B>first_wy</B>), the second end is
 *  (<B>second_wx</B>,<B>second_wy</B>).
 *  The first end is constant. The second end is updated to the (<B>w_x</B>,<B>w_y</B>).
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        Current x coordinate of pointer in world units.
 * \param [in] w_y        Current y coordinate of pointer in world units.
 */
void
o_path_motion_grips (GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->rubber_visible)
    o_path_invalidate_rubber_grips (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_path_invalidate_rubber_grips (w_current);
  w_current->rubber_visible = 1;
}

/*!
 * \brief Start process to input a new path.
 * \par Function Description
 *  This function starts the process of interactively adding a path to
 *  the current sheet by resetting the path creation state and
 *  enabling preview ("rubber") drawing.
 *
 *  For details of how #GschemToplevel fields are used during the
 *  path creation process, see path_next_sections().
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        Current x coordinate of pointer in world units.
 * \param [in] w_y        Current y coordinate of pointer in world units.
 */
void
o_path_start(GschemToplevel *w_current, int w_x, int w_y)
{
  o_path_init(w_current, w_x, w_y);

  i_event_start_adder_handler(w_current, o_path_init, o_path_continue);
}

void o_path_undo (GschemToplevel *w_current)
{
  GedaPath *path;

  if (w_current->temp_path == NULL || w_current->temp_path->sections == NULL) {
    return;
  }

  path = w_current->temp_path;

  if (path->num_sections > 1) {

    PATH_SECTION *section;
    int w_x, w_y;

    path->num_sections--;

    section = &path->sections[path->num_sections];

    section->x1 = 0;
    section->y1 = 0;
    section->x2 = 0;
    section->y2 = 0;
    section->x3 = 0;
    section->y3 = 0;

    if (path->num_sections > 0) {
      section = &path->sections[path->num_sections - 1];
    }

    w_x = section->x3;
    w_y = section->y3;

    w_current->first_wx  = w_x;
    w_current->first_wy  = w_y;
    w_current->second_wx = w_x;
    w_current->second_wy = w_y;
    w_current->third_wx  = 0;
    w_current->third_wy  = 0;

    o_path_continue (w_current, w_x, w_y);
  }
}
