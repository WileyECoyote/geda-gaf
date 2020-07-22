/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 1998-2017 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors (see ChangeLog for details)
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

#include "../../../config.h"

#include <stdio.h>
#include <math.h>
#include <libgeda_priv.h>

#include <geda_debug.h>

/** \defgroup path-object-proc GedaPath Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaPath Objects
 */

typedef void (*DRAW_FUNC) (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                           int line_width, int length, int space,
                           int origin_x, int origin_y);


typedef void (*FILL_FUNC) (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                           int fill_width,
                           int angle1, int pitch1, int angle2, int pitch2,
                           int origin_x, int origin_y);

static void
geda_object_error(const char *func, const void *object, IDE_OBJECT_TYPE type)
{
  geda_error_object_argument(__FILE__, func, object, type);
}

static void
geda_path_object_error(const char *func, const void *object)
{
  geda_object_error(func, object, GEDA_OBJECT_PATH);
}

/*!
 * \brief Get Point on a GedaPath Nearest a Given Point
 * \par Function Description
 *  This function is intended to locate a point on a GedaPath object given
 *  a point \a x, \a y, that is on or about the vicinity of \a object. If
 *  True is returned, <B>nx</B> and <B>ny</B> are set in world unit to a point
 *  on the Path that is the closest point on the path to the point given by
 *  \a x, \a y. If the found point is within #NEAR_DISTANCE distance to a
 *  vertex of the path, the vertex is return as the result.
 *
 * \param [in]  object  Pointer to a GedaPath object
 * \param [in]  x       Integer x of point near or on the path
 * \param [in]  y       Integer y of point near or on the path
 * \param [out] nx      Integer pointer to resulting x value
 * \param [out] ny      Integer pointer to resulting y value
 *
 * \returns TRUE is the results are valid, FALSE if \a object was not a GedaPath.
 */
bool geda_path_object_get_nearest_point (const GedaObject *object, int x, int y, int *nx, int *ny)
{
  GedaPoint target;
  bool result;

#if DEBUG
  fprintf(stderr, "%s begin: x=%d, y=%d\n", __func__, x, y);
#endif

  /* Internal function to check if a point is near an endpoint and move
   * the point if the point is within a reasonably short distance */
  void check_endpoints(int *pt, int end1, int end2) {

    int delta;

    delta = abs(*pt - end1);

    if (delta < NEAR_DISTANCE) { /* If near 1st endpoint*/
      *pt = end1;
       return;
    }

    delta = abs(*pt - end2);

    if (delta < NEAR_DISTANCE) { /* If near 2nd endpoint*/
      *pt = end2;
    }
  }

  if (GEDA_IS_PATH(object)) {

    GArray   *points;
    GedaLine  segment;
    int       closed;

    points = g_array_new (FALSE, FALSE, sizeof(GedaPoint));
    closed = geda_struct_path_to_polygon (object->path, points);
    result = FALSE;

    if (closed) {
      target = g_array_index (points, GedaPoint, points->len - 1);
      points = g_array_prepend_val (points, target);
    }

    if (points->len > 0) {

      double shortest = G_MAXDOUBLE;
      int i = 1;
      GedaPoint vertex;

      vertex = g_array_index (points, GedaPoint, 0);

      while (i < points->len) {

        double   distance;
        GedaLine line;

        line.x[0] = vertex.x;
        line.y[0] = vertex.y;

        vertex = g_array_index (points, GedaPoint, i++);

        line.x[1] = vertex.x;
        line.y[1] = vertex.y;

        distance = geda_math_line_shortest_distance (&line, x, y);

        if (distance == 0.0) { /* Point is on a segment */
          target.x = x;
          target.y = y;
          check_endpoints(&target.x, line.x[0], line.x[1]);
          check_endpoints(&target.y, line.y[0], line.y[1]);
          result   = TRUE;      /* Set flag to not check within segment */
          break;
        }

        if (distance < shortest) {
          segment.x[0] = line.x[0];
          segment.y[0] = line.y[0];
          segment.x[1] = line.x[1];
          segment.y[1] = line.y[1];
          shortest   = distance;
        }
      }
    }
    g_array_free (points, TRUE);

    if (!result) {

      if (segment.x[0] == segment.x[1]) {  /* Segment is vertical */

        int ymin = segment.y[0] > segment.y[1] ? segment.y[1] : segment.y[0];
        int ymax = segment.y[0] > segment.y[1] ? segment.y[0] : segment.y[1];

        target.x = segment.x[0];

        if (y >= ymax) {
          target.y = ymax;
        }
        else if (y <= ymin) {
          target.y = ymin;
        }
        else {
          target.y = y;
          check_endpoints(&target.y, segment.y[0], segment.y[1]);
        }
      }
      else if (segment.y[0] == segment.y[1]) {  /* Segment is horizontal */

        int xmin = segment.x[0] > segment.x[1] ? segment.x[1] : segment.x[0];
        int xmax = segment.x[0] > segment.x[1] ? segment.x[0] : segment.x[1];

        target.y = segment.y[0];

        if (x >= xmax) {
          target.x = xmax;
        }
        else if (x <= xmin) {
          target.x = xmin;
        }
        else {
          target.x = x;
          check_endpoints(&target.x, segment.x[0], segment.x[1]);
        }
      }
      else { /* Segment is on non-zero angle*/

        double dx, dy;
        double m1, b1;
        double off;

        GedaPoint  point;

        dx  = segment.x[1] - segment.x[0];
        dy  = segment.y[1] - segment.y[0];

        m1  = dy / dx;
        b1  = segment.y[0] - m1 * segment.x[0];

        off = abs(m1 * x + b1 - y);

        if (off) {

          double ix, iy;
          double m2, b2;

          m2  = -1 / m1;
          b2  = y - (m2 * x);
          ix  = (b2 - b1) / (m1 - m2);
          iy  = m2 * ix + b2;

          point.x = ix;
          point.y = iy;

        }
        else {
          point.x = x;
          point.y = y;
        }

        /* The calculated point could still be off the line for two
         * reasons; the exact point can not be expressed using just
         * integers and rounding errors. The next section checks and
         * eliminates any such error but starting at x - 5 and sweeping
         * y over a range looking for the first point on the line
         * that is an exact hit (with integers)
         */

        /* Get the error offset from the orginal line segment */
        off = m1 * point.x + b1 - point.y;

        if (off) {

          int index = -5;

          do {
            int sweep_x = point.x + index;
            int sweep_y, start_y;

            start_y = point.y -2 + index++;

            for (sweep_y = start_y; sweep_y < start_y + 5; sweep_y++) {
              off = abs(m1 * sweep_x + b1 - sweep_y);
              if (!off)
                break;
            }
            if (!off) {
              point.x = sweep_x;
              point.y = sweep_y;
            }
            if (index == 15) {
              off = 0.0;          /* Just give up */
            }
          } while (off);
        }

        check_endpoints(&point.x, segment.x[0], segment.x[1]);
        check_endpoints(&point.y, segment.y[0], segment.y[1]);

        /* Check if the segment include the point */
        if (geda_math_line_includes_point(&segment, &point)) {

#if DEBUG
  fprintf(stderr, "%s calculated includes: x=%d, y=%d\n", __func__, point.x, point.y);
#endif
          target.x = point.x;
          target.y = point.y;
        }
        else {

          double first  = geda_distance (segment.x[0], segment.y[0], x, y);
          double second = geda_distance (segment.x[1], segment.y[1], x, y);
          int    index  = first < second ? 0 : 1;

          target.x = segment.x[index];
          target.y = segment.y[index];
        }
      }
      result = TRUE;
    }
  }
  else {
    geda_path_object_error(__func__, object);
    result = FALSE;
  }

  if (result) {
    *nx = target.x;
    *ny = target.y;
  }
  else {
    *nx = x;
    *ny = y;
  }

#if DEBUG
  fprintf(stderr, "%s exit: x=%d, y=%d\n", __func__, target.x, target.y);
#endif
  return result;
}

/*!
 * \brief Get position of the first path point
 * \par Function Description
 *  This function gets the position of the first point of an path object.
 *
 * \param [in]  object  GedaPath object whose position is to be returned
 * \param [out] x       pointer to save the x-position
 * \param [out] y       pointer to save the y-position
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_path_object_get_position (GedaObject *object, int *x, int *y)
{
  if (GEDA_IS_PATH(object)) {

    if (object->path->num_sections == 0)
      return FALSE;

    *x = object->path->sections[0].x3;
    *y = object->path->sections[0].y3;
    return TRUE;
  }

  geda_path_object_error(__func__, object);

  return FALSE;
}

/*!
 * \brief Create and add path GedaObject to list.
 * \par Function Description
 *  This function creates a new object representing a path.
 *  This object is added to the end of the list <B>object_list</B>
 *  pointed object belongs to.
 *  The path is described by its two ends - <B>x1</B>,<B>y1</B> and
 *  <B>x2</B>,<B>y2</B>.
 *  The <B>type</B> parameter must be equal to #OBJ_PATH.
 *  The <B>color</B> parameter corresponds to the color the path
 *  will be drawn with.
 *  The path shape is created by parsing \a path_string.
 *
 *  The structure describing the path is allocated and initialized
 *  with the parameters given to the function.
 *
 *  Both the path type and the filling type are set to default
 *  values : solid path type with a width of 0, and no filling.
 *  It can be changed after with the #geda_set_object_line_options() and
 *  #geda_set_object_fill_options().
 *
 * \param [in]     color        The path color.
 * \param [in]     path_string  The string representation of the path
 *
 * \return A pointer to the new end of the object list.
 */
GedaObject *geda_path_object_new (int color, const char *path_string)
{
  GedaObject *new_obj;
  GedaPath   *path;

  path = geda_struct_path_parse (path_string);
  new_obj = GEDA_OBJECT(path);
  new_obj->color = color;

  return new_obj;
}

/*!
 * \brief Create a New Path GedaObject from an Array of Points
 * \par Function Description
 *  This function creates a new path object using the vertices
 *  given in the array \a points and sets the color property
 *  of the object to \a color. This function essentially does
 *  the oposite of the function geda_struct_path_to_polygon.
 *
 * \param [in] points A GArray containing points
 * \param [in] color  Color index the path should be set to
 *
 * \return A pointer to the new GedaPath object.
 */
GedaObject *geda_path_object_new_from_polygon (GArray *points, int color)
{
  GedaObject *new_obj;
  GedaPath   *path;
  GedaPoint   first;
  GedaPoint   point;
  int     i;

  /* create the object */
  new_obj = geda_path_new ();
  path    = GEDA_PATH(new_obj);

  new_obj->color = color;

  path->num_sections     = points->len;
  path->num_sections_max = path->num_sections + 1;

  path->sections = g_malloc (sizeof(PATH_SECTION) * path->num_sections);

  point = g_array_index (points, GedaPoint, 0);

  path->sections->code   = PATH_MOVETO;
  path->sections->x3     = first.x = point.x;
  path->sections->y3     = first.y = point.y;

  for (i = 1; i < points->len; i++) {

    point = g_array_index (points, GedaPoint, i);
    PATH_SECTION *section = &path->sections[i];

    section->code   = PATH_LINETO;
    section->x3     = point.x;
    section->y3     = point.y;
  }

  if ((path->sections[i - 1].x3 == first.x) &&
       path->sections[i - 1].y3 == first.y)
  {
    path->sections[i].code = PATH_END;
  }

  return new_obj;
}

/*!
 * \brief Create a new path object.
 * \par Function Description
 *  This function creates and returns a new GedaObject representing
 *  a path using the path shape data stored in \a path_data. The
 *  \a path_data is subsequently owned by the returned GedaObject.
 *
 * \sa geda_path_object_new
 *
 * \param [in]     color        The path color
 * \param [in]     path_data    The #GedaPath data structure to use
 *
 * \return A pointer to the new end of the object list.
 */
GedaObject *geda_path_object_new_take_path (int color, GedaPath *path_data)
{
  GedaObject *new_obj;
  GedaPath   *path;

  /* create the object */
  new_obj = geda_path_new ();
  path = GEDA_PATH(new_obj);

  new_obj->color = color;

  path->num_sections     = path_data->num_sections;
  path->num_sections_max = path_data->num_sections_max;

  path->sections = g_malloc (sizeof(PATH_SECTION) * path->num_sections_max);

  path->sections->code   = path_data->sections->code;
  path->sections->x1     = path_data->sections->x1;
  path->sections->y1     = path_data->sections->y1;
  path->sections->x2     = path_data->sections->x2;
  path->sections->y2     = path_data->sections->y2;
  path->sections->x3     = path_data->sections->x3;
  path->sections->y3     = path_data->sections->y3;

  return new_obj;
}

/*!
 * \brief Create a copy of a path.
 * \par Function Description
 *  This function creates a verbatim copy of the
 *  object pointed by <B>\a o_current</B> describing a path. The new object
 *  is added at the end of the list following the <B>list_tail</B>
 *  parameter.
 *
 * \param [in]  o_current  Line GedaObject to copy.
 *
 * \return A new pointer to the end of the object list.
 */
GedaObject *geda_path_object_copy (const GedaObject *o_current)
{
  if (GEDA_IS_PATH(o_current)) {

    GedaObject *new_obj;
    GedaPath   *old_path;
    char       *path_string;

    old_path    = (GedaPath*)o_current;
    path_string = geda_struct_path_string_from_path (old_path);
    new_obj     = geda_path_object_new (o_current->color, path_string);

    GEDA_FREE (path_string);

    /* Copy the path line-type and filling options */
    geda_set_object_line_options (new_obj, &old_path->line_options);
    geda_set_object_fill_options (new_obj, &old_path->fill_options);

    /* calc the bounding box */
    new_obj->bounds_valid = FALSE;

    /* return the new tail of the object list */
    return new_obj;
  }

  geda_path_object_error(__func__, o_current);

  return NULL;
}

/*!
 * \brief Create path GedaObject from character string.
 * \par Function Description
 *  This function creates a path GedaObject from the character string <B>*buf</B>
 *  and a number of lines following that describing the path, read from <B>*tb</B>.
 *  A path is internally described by its two ends. A new object is allocated,
 *  initialized and added to the list of objects. The path type is set according
 *  to the values of the fields on the path. The current path format to describe
 *  a line is a space separated list of characters and numbers in plain ASCII on
 *  a single path. The meaning of each item is described in the file format
 *  documentation.
 *
 *  Depending on <B>*version</B>, the correct file format is considered.
 *  Currently two file format revisions are supported :
 *
 * \li the file format used until 20010704 release.
 * \li the file format used for the releases after 20010704.
 *
 * \param [in]  first_line      Character string with path description.
 * \param [in]  tb              Text buffer containing the path string.
 * \param [in]  release_ver     libgeda release version number.
 * \param [in]  fileformat_ver  libgeda file format version number.
 *
 * \param [out] err           A GError obejct
 *
 * \return A pointer to the new path object, or NULL on error;
 */
GedaObject *geda_path_object_read (const char *first_line,
                                   TextBuffer  *tb,
                                   unsigned int release_ver,
                                   unsigned int fileformat_ver,
                                   GError     **err)
{
  GedaObject *new_obj;
  char       *string;
  unsigned    allocated;

  char type;
  int color;
  int line_width, line_space, line_length;
  int line_end;
  int line_type;
  int fill_type, fill_width, angle1, pitch1, angle2, pitch2;
  int num_lines = 0;
  int i;

  /* Allocate enough space */
  if (sscanf (first_line, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
       &type, &color, &line_width, &line_end, &line_type,
       &line_length, &line_space, &fill_type, &fill_width, &angle1,
       &pitch1, &angle2, &pitch2, &num_lines) != 14) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse path object"));
    return NULL;
  }

  /* Checks if the required color is valid. */
  if (color < 0 || color > MAX_COLORS) {
    const char *msg = _("Found an invalid color");
    if (geda_object_show_buffer_err(msg, first_line)) {
      geda_log_w("%s: %d.\n", msg, color);
    }
    geda_log_w (_("Setting color to default color\n"));
    color = DEFAULT_PATH_COLOR_INDEX;
  }

  allocated = 0;
  string    = NULL;

  for (i = 0; i < num_lines; i++) {

    const char *line;

    line = geda_struct_textbuffer_next_line (tb);

    if (line == NULL) {
      GEDA_FREE(string);
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Unexpected end-of-file after %d lines"), i);
      return NULL;
    }

    size_t len = strlen(line);

    if (!allocated) {
      string = (char*)malloc(len + 1);
      strncpy(string, line, len);
      allocated = ++len;
      string[allocated - 1] = '\0';
    }
    else {

      char *buffer;

      allocated = allocated + len;

      buffer = (char*)realloc(string, allocated);

      if (!buffer)
        break;

      string = buffer;
      strncat(string, line, len);
    }
  }

  string = geda_utility_string_remove_last_nl(string);

  /* create a new path */
  new_obj = geda_path_object_new (color, string);
  GEDA_FREE (string);

  /* set the line options */
  new_obj->line_options->line_end    = line_end;
  new_obj->line_options->line_type   = line_type;
  new_obj->line_options->line_width  = line_width;
  new_obj->line_options->line_length = line_length;
  new_obj->line_options->line_space  = line_space;

  /* set the fill options */
  new_obj->fill_options->fill_type   = fill_type;
  new_obj->fill_options->fill_width  = fill_width;
  new_obj->fill_options->fill_angle1 = angle1;
  new_obj->fill_options->fill_angle2 = angle2;
  new_obj->fill_options->fill_pitch1 = pitch1;
  new_obj->fill_options->fill_pitch2 = pitch2;

  return new_obj;
}

/*!
 * \brief Create a character string representation of a path GedaObject.
 * \par Function Description
 *  The function formats a string in the buffer <B>*buff</B> to describe
 *  the path object <B>*object</B>.
 *
 * \param [in] object  path GedaObject to create string from.
 *
 * \return A pointer to the path GedaObject character string.
 *
 * \note Caller should GEDA_FREE returned character string.
 */
char *geda_path_object_to_buffer (GedaObject *object)
{
  int line_width, line_space, line_length;
  char *buf;
  int num_lines;
  LINE_END line_end;
  LINE_TYPE  line_type;
  OBJECT_FILLING fill_type;
  int fill_width, angle1, pitch1, angle2, pitch2;
  char *path_string;

  /* description of the line type */
  line_width  = object->line_options->line_width;
  line_end    = object->line_options->line_end;
  line_type   = object->line_options->line_type;
  line_length = object->line_options->line_length;
  line_space  = object->line_options->line_space;

  /* filling parameters */
  fill_type    = object->fill_options->fill_type;
  fill_width   = object->fill_options->fill_width;
  angle1       = object->fill_options->fill_angle1;
  pitch1       = object->fill_options->fill_pitch1;
  angle2       = object->fill_options->fill_angle2;
  pitch2       = object->fill_options->fill_pitch2;

  path_string = geda_struct_path_string_from_path (object->path);

  num_lines = geda_object_get_num_text_lines (path_string);
  buf = geda_sprintf ("%c %d %d %d %d %d %d %d %d %d %d %d %d %d\n%s",
                      object->type, object->color, line_width, line_end,
                      line_type, line_length, line_space, fill_type,
                      fill_width, angle1, pitch1, angle2, pitch2,
                      num_lines, path_string);
  GEDA_FREE (path_string);

  return buf;
}

/*!
 * \brief Modify control point location
 * \par Function Description
 *  This function modifies a control point location of the path object
 *  *object. The control point being modified is selected according to
 *  the whichone parameter.
 *
 *  The new position is given by <B>x</B> and <B>y</B>.
 *
 * \param [in,out] object    The path GedaObject
 * \param [in]     x         New x coordinate for the control point
 * \param [in]     y         New y coordinate for the control point
 * \param [in]     whichone  Which control point is being modified
 */
void geda_path_object_modify (GedaObject *object, int x, int y, int whichone)
{
  if (GEDA_IS_PATH(object)) {

    int i;
    int grip_no = 0;

    for (i = 0; i <  object->path->num_sections; i++) {

      PATH_SECTION *section = &object->path->sections[i];

      switch (section->code) {
        case PATH_CURVETO:
          /* Two control point grips */
          if (whichone == grip_no++) {
            section->x1 = x;
            section->y1 = y;
          }
          if (whichone == grip_no++) {
            section->x2 = x;
            section->y2 = y;
          }
          /* Fall through */
          case PATH_MOVETO:
          case PATH_MOVETO_OPEN:
          case PATH_LINETO:
            /* Destination point grip */
            if (whichone == grip_no++) {
              section->x3 = x;
              section->y3 = y;
            }
            break;
          case PATH_END:
            break;
      }
    }

    /* Update bounding box */
    object->bounds_valid = FALSE;
  }
  else {
    geda_path_object_error(__func__, object);
  }
}

/*!
 * \brief Mirror a Path.
 * \par Function Description
 *  This function mirrors the path from the point
 *  (<B>center_x</B>,<B>center_y</B>) in world unit.
 *
 * \param [in,out] object    Line GedaObject to mirror.
 * \param [in]     center_x  Origin x coordinate.
 * \param [in]     center_y  Origin y coordinate.
 */
void geda_path_object_mirror (GedaObject *object, int center_x, int center_y)
{
  if (GEDA_IS_PATH(object)) {

    int i;

    for (i = 0; i < object->path->num_sections; i++) {

      PATH_SECTION *section = &object->path->sections[i];

      switch (section->code) {
        case PATH_CURVETO:
          /* Two control point grips */
          section->x1 = 2 * center_x - section->x1;
          section->x2 = 2 * center_x - section->x2;
          /* Fall through */
          case PATH_MOVETO:
          case PATH_MOVETO_OPEN:
          case PATH_LINETO:
            /* Destination point grip */
            section->x3 = 2 * center_x - section->x3;
            break;
          case PATH_END:
            break;
      }
    }

    object->bounds_valid = FALSE;
  }
  else {
    geda_path_object_error(__func__, object);
  }
}

/*!
 * \brief Rotate Line GedaObject.
 * \par Function Description
 *  This function rotates the path described by  <B>*object</B> around
 *  the (<B>center_x</B>,<B>center_y</B>) point by <B>angle</B> degrees.
 *  The center of rotation is in world units.
 *
 * \param [in,out] object    Line GedaObject to rotate
 * \param [in]     center_x  Rotation center x coordinate
 * \param [in]     center_y  Rotation center y coordinate
 * \param [in]     angle     Rotation angle in degrees (See note below).
 */
void geda_path_object_rotate (GedaObject *object, int center_x, int center_y, int angle)
{
  if (GEDA_IS_PATH(object)) {

    PATH_SECTION *section;
    int i;

    for (i = 0; i < object->path->num_sections; i++) {
      section = &object->path->sections[i];

      switch (section->code) {
        case PATH_CURVETO:
          /* Two control point grips */
          section->x1 -= center_x; section->y1 -= center_y;
          section->x2 -= center_x; section->y2 -= center_y;
          geda_math_rotate_point_90 (section->x1, section->y1, angle, &section->x1, &section->y1);
          geda_math_rotate_point_90 (section->x2, section->y2, angle, &section->x2, &section->y2);
          section->x1 += center_x; section->y1 += center_y;
          section->x2 += center_x; section->y2 += center_y;
          /* Fall through */
          case PATH_MOVETO:
          case PATH_MOVETO_OPEN:
          case PATH_LINETO:
            /* Destination point grip */
            section->x3 -= center_x; section->y3 -= center_y;
            geda_math_rotate_point_90 (section->x3, section->y3, angle, &section->x3, &section->y3);
            section->x3 += center_x; section->y3 += center_y;
            break;
          case PATH_END:
            break;
      }
    }
    object->bounds_valid = FALSE;
  }
  else {
    geda_path_object_error(__func__, object);
  }
}

/*!
 * \brief Translate a path position by a delta.
 * \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the path
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 * \param [in,out] object     Line GedaObject to translate
 * \param [in]     dx         x distance to move
 * \param [in]     dy         y distance to move.
 */
void geda_path_object_translate (GedaObject *object, int dx, int dy)
{
  if (GEDA_IS_PATH(object)) {

    PATH_SECTION *section;
    int i;

    for (i = 0; i < object->path->num_sections; i++) {
      section = &object->path->sections[i];

      switch (section->code) {
        case PATH_CURVETO:
          section->x1 += dx;
          section->y1 += dy;
          section->x2 += dx;
          section->y2 += dy;
          /* Fall through */
          case PATH_MOVETO:
          case PATH_MOVETO_OPEN:
          case PATH_LINETO:
            section->x3 += dx;
            section->y3 += dy;
            break;
          case PATH_END:
            break;
      }
    }

    /* Update bounding box */
    object->bounds_valid = FALSE;
  }
  else {
    geda_path_object_error(__func__, object);
  }
}

/*!
 * \brief Print a solid PATH to Postscript document.
 * \par Function Description
 *  This function prints the outline of a path when a solid line type is
 *  required. The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> and <B>space</B> are ignored.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          FILE pointer to Postscript document
 * \param [in] path        The PATH object ot print
 * \param [in] line_width  PATH Line width
 * \param [in] length      Dashed line length
 * \param [in] space       Amount of space between dashes
 * \param [in] origin_x    Page x coordinate to place PATH GedaObject
 * \param [in] origin_y    Page y coordinate to place PATH GedaObject
 */
static void
geda_path_object_print_solid (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                              int line_width, int length, int space,
                              int origin_x, int origin_y)
{
  int i;

  for (i = 0; i < path->num_sections; i++) {

    PATH_SECTION *section = &path->sections[i];

    if (i > 0)
      fprintf (fp, " ");

    switch (section->code) {
      case PATH_MOVETO:
        fprintf (fp, "closepath");
        /* Fall through */
      case PATH_MOVETO_OPEN:
        fprintf (fp, "%i %i moveto",
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_CURVETO:
        fprintf (fp, "%i %i %i %i %i %i curveto",
                     section->x1 - origin_x, section->y1 - origin_y,
                     section->x2 - origin_x, section->y2 - origin_y,
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_LINETO:
        fprintf (fp, "%i %i lineto",
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_END:
        fprintf (fp, "closepath");
        break;
    }
  }

  fprintf (fp, " stroke\n");
}


/*!
 * \brief Print a dotted Path to Postscript document.
 * \par Function Description
 *  This function prints the outline of a path when a dotted line type is
 *  required. The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameter <B>length</B> is ignored.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          FILE pointer to Postscript document
 * \param [in] path        The GedaPath object to print
 * \param [in] line_width  Path Line width
 * \param [in] length      Dashed line length
 * \param [in] space       Amount of space between dashes
 * \param [in] origin_x    Page x coordinate to place Path GedaObject
 * \param [in] origin_y    Page y coordinate to place Path GedaObject
 */
static void
geda_path_object_print_dotted (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                     int line_width, int length, int space,
                     int origin_x, int origin_y)
{
  geda_path_object_print_solid (toplevel, fp, path, line_width,
                      length, space, origin_x, origin_y);
}


/*!
 * \brief Print a dashed Path to Postscript document.
 * \par Function Description
 *  This function prints the outline of a path when a dashed line type is
 *  required. The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          FILE pointer to Postscript document
 * \param [in] path        The GedaPath object to print
 * \param [in] line_width  Path Line width
 * \param [in] length      Dashed line length
 * \param [in] space       Amount of space between dashes
 * \param [in] origin_x    Page x coordinate to place Path GedaObject
 * \param [in] origin_y    Page y coordinate to place Path GedaObject
 */
static void
geda_path_object_print_dashed (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                     int line_width, int length, int space,
                     int origin_x, int origin_y)
{
  geda_path_object_print_solid (toplevel, fp, path, line_width,
                      length, space, origin_x, origin_y);
}


/*!
 * \brief Print centered line type Path to Postscript document.
 * \par Function Description
 *  This function prints the outline of a path when a centered line type is
 *  required. The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          FILE pointer to Postscript document
 * \param [in] path        The Path object to print
 * \param [in] line_width  Path Line width
 * \param [in] length      Dashed line length
 * \param [in] space       Amount of space between dashes
 * \param [in] origin_x    Page x coordinate to place Path GedaObject
 * \param [in] origin_y    Page y coordinate to place Path GedaObject
 */
static void
geda_path_object_print_center (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                     int line_width, int length,
                     int space, int origin_x, int origin_y)
{
  geda_path_object_print_solid (toplevel, fp, path, line_width,
                      length, space, origin_x, origin_y);
}


/*!
 * \brief Print phantom line type Path to Postscript document.
 * \par Function Description
 *  This function prints the outline of a path when a phantom line type is
 *  required. The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          FILE pointer to Postscript document
 * \param [in] path        The Path object to print
 * \param [in] line_width  GedaPath Line width
 * \param [in] length      Dashed line length
 * \param [in] space       Amount of space between dashes
 * \param [in] origin_x    Page x coordinate to place Path GedaObject
 * \param [in] origin_y    Page y coordinate to place Path GedaObject
 */
static void
geda_path_object_print_phantom (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                      int line_width, int length,
                      int space, int origin_x, int origin_y)
{
  geda_path_object_print_solid (toplevel, fp, path, line_width,
                                length, space, origin_x, origin_y);
}


/*!
 * \brief Print a solid pattern Path to Postscript document.
 * \par Function Description
 *  The function prints a filled path with a solid pattern. No outline is
 *  printed. The postscript file is defined by the file pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B> and <B>pitch1</B>, <B>angle2</B> and <B>pitch2</B>
 *  parameters are ignored in this functions but kept for compatibility
 *  with other fill functions.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          FILE pointer to Postscript document
 * \param [in] path        The Path object to print
 * \param [in] fill_width  GedaPath fill width (unused)
 * \param [in] angle1      (unused)
 * \param [in] pitch1      (unused)
 * \param [in] angle2      (unused)
 * \param [in] pitch2      (unused)
 * \param [in] origin_x    Page x coordinate to place Path GedaObject
 * \param [in] origin_y    Page y coordinate to place Path GedaObject
 */
static void
geda_path_object_print_filled (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                               int fill_width, int angle1, int pitch1, int angle2,
                               int pitch2, int origin_x, int origin_y)
{
  int i;

  for (i = 0; i < path->num_sections; i++) {
    PATH_SECTION *section = &path->sections[i];

    if (i > 0)
      fprintf (fp, " ");

    switch (section->code) {
      case PATH_MOVETO:
        fprintf (fp, "closepath");
        /* Fall through */
      case PATH_MOVETO_OPEN:
        fprintf (fp, "%i %i moveto",
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_CURVETO:
        fprintf (fp, "%i %i %i %i %i %i curveto",
                     section->x1 - origin_x, section->y1 - origin_y,
                     section->x2 - origin_x, section->y2 - origin_y,
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_LINETO:
        fprintf (fp, "%i %i lineto",
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_END:
        fprintf (fp, "closepath");
        break;
    }
  }

  fprintf (fp, " fill\n");
}

/*!
 * \brief Print a hatch pattern Path to Postscript document.
 * \par Function Description
 *  The function prints a hatched path. No outline is printed.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B>, <B>pitch1</B> parameters define the way the path
 *  has to be hatched.
 *  <B>angle2</B> and <B>pitch2</B> parameters are unused but kept for compatibility
 *  with other fill functions.
 *
 *  Negative or zero values for <B>pitch1</B> are not allowed.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          FILE pointer to Postscript document
 * \param [in] path        The Path object to print
 * \param [in] fill_width  GedaPath fill width
 * \param [in] angle1      Angle of hatch pattern
 * \param [in] pitch1      Pitch of hatch pattern
 * \param [in] angle2      (unused)
 * \param [in] pitch2      (unused)
 * \param [in] origin_x    Page x coordinate to place Path GedaObject
 * \param [in] origin_y    Page y coordinate to place Path GedaObject
 */
static void
geda_path_object_print_hatch (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                              int fill_width, int angle1, int pitch1, int angle2,
                              int pitch2, int origin_x, int origin_y)
{
  int i;
  GArray *lines;

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (fp != NULL);

  /* Avoid printing line widths too small */
  if (fill_width <= 1) fill_width = 2;

  lines = g_array_new (FALSE, FALSE, sizeof(LINE));
  fprintf(stderr, "geda_path_object_print_hatch: Calling geda_math_hatch_path with pitch=[%d]", pitch1);
  geda_math_hatch_path (path, angle1, pitch1, lines);

  for (i=0; i < lines->len; i++) {
    LINE *line = &g_array_index (lines, LINE, i);

    fprintf (fp,"%d %d %d %d %d line\n", line->x[0], line->y[0],
                                         line->x[1], line->y[1], fill_width);
  }

  g_array_free (lines, TRUE);
}

/*!
 * \brief Print a mesh pattern Path to Postscript document.
 * \par Function Description
 *  This function prints a meshed path. No outline is printed.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  Negative or zero values for <B>pitch1</B> and/or <B>pitch2</B> are
 *  not allowed.
 *
 *  All dimensions are in mils.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          FILE pointer to Postscript document
 * \param [in] path        The Path object to print
 * \param [in] fill_width  GedaPath fill width
 * \param [in] angle1      1st angle for mesh pattern
 * \param [in] pitch1      1st pitch for mesh pattern
 * \param [in] angle2      2nd angle for mesh pattern
 * \param [in] pitch2      2nd pitch for mesh pattern
 * \param [in] origin_x    Page x coordinate to place Path GedaObject
 * \param [in] origin_y    Page y coordinate to place Path GedaObject
 */
static void
geda_path_object_print_mesh (GedaToplevel *toplevel, FILE *fp, GedaPath *path,
                             int fill_width, int angle1, int pitch1, int angle2,
                             int pitch2, int origin_x, int origin_y)
{
  geda_path_object_print_hatch (toplevel, fp, path, fill_width,
                                angle1, pitch1, -1, -1, origin_x, origin_y);

  geda_path_object_print_hatch (toplevel, fp, path, fill_width,
                                angle2, pitch2, -1, -1, origin_x, origin_y);
}

/*!
 * \brief Print Path to Postscript document.
 * \par Function Description
 *  This function prints the path described by the <B>\a o_current</B>
 *  parameter to a Postscript document.
 *  The Postscript document is descibed by the file pointer <B>fp</B>.
 *
 * \param [in] toplevel   GedaToplevel object
 * \param [in] fp         FILE pointer to Postscript document
 * \param [in] o_current  Path GedaObject to write to document
 * \param [in] origin_x   Page x coordinate to place Path GedaObject
 * \param [in] origin_y   Page y coordinate to place Path GedaObject
 */
void
geda_path_object_print(GedaToplevel *toplevel, FILE *fp,
                       GedaObject   *o_current,
                       int origin_x, int origin_y)
{
  int line_width, length, space;
  DRAW_FUNC outl_func = NULL;

  g_return_if_fail(GEDA_IS_PATH(o_current));

  /*! \note
   *  Depending on the type of the line for this particular path, the
   *  appropriate function is chosen among #geda_path_object_print_solid(),
   *  #geda_path_object_print_dotted(), #geda_path_object_print_dashed(),
   *  #geda_path_object_print_center() and #geda_path_object_print_phantom().
   *
   *  The needed parameters for each of these type is extracted from the
   *  <B>\a o_current</B> object. Depending on the type, unused parameters are
   *  set to -1.
   *
   *  In the eventuality of a length and/or space null, the line is printed
   *  solid to avoid and endless loop produced by other functions in such a
   *  case.
   */
  /* 09/08/12 | W.E.Hill Modified algorithms to incorperate both THICK & THIN
   *            styles, and eliminated hard-coded integer values.
   */

  line_width = o_current->line_options->line_width;

  if (line_width < MIN_LINE_WIDTH_THRESHOLD) {

    line_width = geda_object_style_get_line_width(toplevel); /* 1st try updating style */

     if (line_width < MIN_LINE_WIDTH_THRESHOLD){
        line_width = MIN_LINE_WIDTH_THRESHOLD;        /* if STYLE_NONE  */
     }
  }

  length = o_current->line_options->line_length;
  space  = o_current->line_options->line_space;

  switch(o_current->line_options->line_type) {
    case TYPE_SOLID:
      length = -1; space  = -1;
      outl_func = geda_path_object_print_solid;
      break;

    case TYPE_DOTTED:
      length = -1;
      outl_func = geda_path_object_print_dotted;
      break;

    case TYPE_DASHED:
      outl_func = geda_path_object_print_dashed;
      break;

    case TYPE_CENTER:
      outl_func = geda_path_object_print_center;
      break;

    case TYPE_PHANTOM:
      outl_func = geda_path_object_print_phantom;
      break;

    case TYPE_ERASE:
      /* Unused for now, print it solid */
      length = -1; space  = -1;
      outl_func = geda_path_object_print_solid;
      break;
  }

  if((length == 0) || (space == 0)) {
    length = -1; space  = -1;
    outl_func = geda_path_object_print_solid;
  }

  f_print_set_color (toplevel, fp, o_current->color);

  f_print_set_line_width (fp, line_width);

  (*outl_func) (toplevel, fp, o_current->path, line_width,
                length, space, origin_x, origin_y);

  /*! \note
   *  If the filling type of the path is not <B>HOLLOW</B>, the appropriate
   *  function is chosen among #geda_path_object_print_filled(), #geda_path_object_print_mesh()
   *  and #geda_path_object_print_hatch(). The corresponding parameters are extracted
   *  from the <B>\a o_current</B> object and corrected afterward.
   *
   *  The case where <B>pitch1</B> and <B>pitch2</B> are null or negative is
   *  avoided as it leads to an endless loop in most of the called functions.
   *  In such a case, the path is printed filled. Unused parameters for each
   *  of these functions are set to -1 or any passive value.
   */
  if (o_current->fill_options->fill_type != FILLING_HOLLOW) {

    FILL_FUNC fill_func = NULL;

    int fill_width, angle1, pitch1, angle2, pitch2;

    fill_width = o_current->fill_options->fill_width;
    angle1     = o_current->fill_options->fill_angle1;
    pitch1     = o_current->fill_options->fill_pitch1;
    angle2     = o_current->fill_options->fill_angle2;
    pitch2     = o_current->fill_options->fill_pitch2;

    switch(o_current->fill_options->fill_type) {
      case FILL_SOLID:
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func  = geda_path_object_print_filled;
        break;

      case FILLING_MESH:
        fill_func = geda_path_object_print_mesh;
        break;

      case FILLING_HATCH:
        angle2 = -1; pitch2 = 1;
        fill_func = geda_path_object_print_hatch;
        break;

      case FILLING_VOID:
        /* Unused for now, print it filled */
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func  = geda_path_object_print_filled;
        break;

      case FILLING_HOLLOW:
        /* nop */
        break;

    }

    if ((pitch1 <= 0) || (pitch2 <= 0)) {
      angle1 = -1; pitch1 = 1;
      angle2 = -1; pitch2 = 1;
      fill_func = geda_path_object_print_filled;
    }

    if (fill_func) {
      (*fill_func) (toplevel, fp,
                    o_current->path, fill_width,
                    angle1, pitch1, angle2, pitch2, origin_x, origin_y);
    }
  }
}

/*!
 * \brief Determine the Shortest Distence to a Path Object
 * \par Function Description
 *  Calculates the distance between the given point and the closest
 *  point on the given path segment.
 *
 * \param [in] object       The path GedaObject
 * \param [in] x            The x coordinate of the given point
 * \param [in] y            The y coordinate of the given point
 * \param [in] force_solid  If true, force treating the object as solid
 *
 * \return The shortest distance from the object to the point. With an
 *         invalid parameter, this function returns G_MAXDOUBLE.
 */
double geda_path_object_shortest_distance (ConstObject *object, int x, int y, int force_solid)
{
  if (GEDA_IS_PATH(object)) {

    int solid;

    g_return_val_if_fail(GEDA_IS_PATH(object), 0.0);

    solid = force_solid || object->fill_options->fill_type != FILLING_HOLLOW;

    return geda_struct_path_shortest_distance (object->path, x, y, solid);

  }

  geda_path_object_error(__func__, object);

  return (G_MAXDOUBLE);
}

/** @} endgroup path-object-proc */
