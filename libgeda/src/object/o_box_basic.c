/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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

/*! \file o_box_basic.c
 *  \brief functions for the box object
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include <libgeda_priv.h>

/*! \brief Create a Box Object
 *  \par Function Description
 *  This function creates a new object representing a box.
 *
 *  The box is described by its upper left corner - <B>x1</B>, <B>y1</B> - and
 *  its lower right corner - <B>x2</B>, <B>y2</B>.
 *  The <B>type</B> parameter must be equal to <B>OBJ_BOX</B>. The <B>color</B>
 *  corresponds to the color the box will be drawn with.
 *  The <B>GedaObject</B> structure is allocated with the #geda_object_new()
 *  function. The structure describing the box is allocated and initialized
 *  with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default values : solid
 *  line type with a width of 0, and no filling. It can be changed after
 *  with the #o_set_line_options() and #o_set_fill_options().
 *
 *  \param [in]     color        Box border color.
 *  \param [in]     x1           Upper x coordinate.
 *  \param [in]     y1           Upper y coordinate.
 *  \param [in]     x2           Lower x coordinate.
 *  \param [in]     y2           Lower y coordinate.
 *
 *  \return The new GedaObject
 */
GedaObject *o_box_new(int color, int x1, int y1, int x2, int y2)
{
  Box    *box;
  GedaObject *new_obj;

  /* create the object */
  new_obj = geda_box_new();
  box     = GEDA_BOX(new_obj);

  new_obj->color = color;

  /* describe the box with its upper left and lower right corner */
  box->upper_x = x1;
  box->upper_y = y1;
  box->lower_x = x2;
  box->lower_y = y2;

  return new_obj;
}

/*! \brief Copy a box to a list.
 *
 *  \par Function Description
 *  The function #o_box_copy() creates a verbatim copy of the object
 *  pointed by <B>\a o_current</B> describing a box.
 *
 *  \param [in] o_current Box Object to copy.
 *
 *  \return The new GedaObject
 */
GedaObject *o_box_copy(GedaObject *o_current)
{
  GedaObject *new_obj;
  Box    *old_box;

  g_return_val_if_fail(GEDA_IS_BOX(o_current), NULL);

  old_box = GEDA_BOX(o_current);

  /* A new box object is created with #o_box_new().
   * Values for its fields are default and need to be modified. */
  new_obj = o_box_new (o_current->color, 0, 0, 0, 0);

  /* The dimensions of the new box are set with the ones of the original box.
   * The two boxes have the same line type and the same filling options.
   */
  new_obj->box->upper_x = old_box->upper_x;
  new_obj->box->upper_y = old_box->upper_y;
  new_obj->box->lower_x = old_box->lower_x;
  new_obj->box->lower_y = old_box->lower_y;

  o_set_line_options(new_obj, &old_box->line_options);
  o_set_fill_options(new_obj, &old_box->fill_options);

  new_obj->w_bounds_valid_for = NULL;

  return new_obj;
}

/*! \brief Modify a Box Object's coordinates.
 *
 * \par Function Description
 * Modifies the coordinates of all four corners of \a box, by setting
 * the box to the rectangle enclosed by the points (\a x1, \a y1) and
 * (\a x2, \a y2).
 *
 * \param [in,out] object   box #GedaObject to be modified.
 * \param [in]     x1       x coordinate of first corner of box.
 * \param [in]     y1       y coordinate of first corner of box.
 * \param [in]     x2       x coordinate of second corner of box.
 * \param [in]     y2       y coordinate of second corner of box,
 */
void o_box_modify_all (GedaObject *object, int x1, int y1, int x2, int y2)
{
  object->box->lower_x = (x1 > x2) ? x1 : x2;
  object->box->lower_y = (y1 > y2) ? y2 : y1;

  object->box->upper_x = (x1 > x2) ? x2 : x1;
  object->box->upper_y = (y1 > y2) ? y1 : y2;

  /* recalculate the world coords and bounds */
  object->w_bounds_valid_for = NULL;

}

/*! \brief Modify a Box Object's coordinates.
 *
 *  \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the box. The new coordinates of the corner identified by <B>whichone</B>
 *  are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate system.
 *  Screen coordinates and boundings are then updated.
 *
 *  \param [in,out] object     Box Object to be modified.
 *  \param [in]     x          x coordinate.
 *  \param [in]     y          y coordinate.
 *  \param [in]     whichone   coordinate to change.
 *
 *  \note
 *  <B>whichone</B> can take the following values:
 *  <DL>
 *    <DT>*</DT><DD>BOX_UPPER_LEFT
 *    <DT>*</DT><DD>BOX_LOWER_LEFT
 *    <DT>*</DT><DD>BOX_UPPER_RIGHT
 *    <DT>*</DT><DD>BOX_LOWER_RIGHT
 *  </DL>
 */
void o_box_modify(GedaObject *object, int x, int y, int whichone)
{
  int tmp;

  /* change the position of the selected corner */
  switch(whichone) {
    case BOX_UPPER_LEFT:
      object->box->upper_x = x;
      object->box->upper_y = y;
      break;

    case BOX_LOWER_LEFT:
      object->box->upper_x = x;
      object->box->lower_y = y;
      break;

    case BOX_UPPER_RIGHT:
      object->box->lower_x = x;
      object->box->upper_y = y;
      break;

    case BOX_LOWER_RIGHT:
      object->box->lower_x = x;
      object->box->lower_y = y;
      break;

    default:
      return;
  }

  /* need to update the upper left and lower right corners */
  if (object->box->upper_x > object->box->lower_x) {
    tmp                  = object->box->upper_x;
    object->box->upper_x = object->box->lower_x;
    object->box->lower_x = tmp;
  }

  if (object->box->upper_y < object->box->lower_y) {
    tmp                  = object->box->upper_y;
    object->box->upper_y = object->box->lower_y;
    object->box->lower_y = tmp;
  }

  /* recalculate the world coords and the boundings */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Create a box from a character string.
 *  \par Function Description
 *  This function gets the description of a box from the <B>*buf</B> character
 *  string.
 *
 *  Depending on <B>*version</B>, the correct file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 20000704 release
 *    <DT>*</DT><DD>the file format used for the releases after 2000704.
 *  </DL>
 *
 *  \param [in]     buf             Character string with box description.
 *  \param [in]     release_ver     libgeda release version number.
 *  \param [in]     fileformat_ver  libgeda file format version number.
 *
 *  \param [out] err                A GError object
 *
 *  \return The Box Object that was created, or NULL on error.
 */
GedaObject* o_box_read (const char buf[], unsigned int release_ver,
                    unsigned int fileformat_ver, GError **err)
{
  GedaObject *new_obj;
  char type;
  int x1, y1;
  int width, height;
  int d_x1, d_y1;
  int d_x2, d_y2;
  int color;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  int box_end;
  int box_type;
  int box_filling;

  if (release_ver <= VERSION_20000704) {

  /*! \note
   *  The old geda file format, i.e. releases 20000704 and older, does not
   *  handle the line type and the filling of the box object. They are set
   *  to default.
   */

    if (sscanf (buf, "%c %d %d %d %d %d\n",
        &type, &x1, &y1, &width, &height, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse box object"));
      return NULL;
    }

    box_width   = 0;
    box_end     = END_NONE;
    box_type    = TYPE_SOLID;
    box_length  = -1;
    box_space   = -1;

    box_filling = FILLING_HOLLOW;
    fill_width  = 0;
    angle1      = -1;
    pitch1      = -1;
    angle2      = -1;
    pitch2      = -1;

  }
  else {

    /*! \note
     *  The current line format to describe a box is a space separated list of
     *  characters and numbers in plain ASCII on a single line. The meaning of
     *  each item is described in the file format documentation.
     */
    if (sscanf (buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
                &type, &x1, &y1, &width, &height, &color,
                &box_width, &box_end, &box_type, &box_length,
                &box_space, &box_filling,
                &fill_width, &angle1, &pitch1, &angle2, &pitch2) != 17)
    {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse box object"));
      return NULL;
    }
  }

  if (width == 0 || height == 0) {
    u_log_message (_("Found a zero width/height box [ %c %d %d %d %d %d ]\n"),
                   type, x1, y1, width, height, color);
  }

  if (color < 0 || color > MAX_COLORS) {
    u_log_message (_("Found an invalid color [ %s ]\n"), buf);
    u_log_message (_("Setting color to default color\n"));
    color = DEFAULT_BOX_COLOR_INDEX;
  }

  /*! \note
   *  A box is internally described by its lower right and upper left corner
   *  whereas the line describe it with the lower left corner and the width
   *  and height.
   *
   *  A new object is allocated, initialized and added to the object list.
   *  Its filling and line type are set according to the values of the field
   *  on the line.
   */

  /* upper left corner of the box */
  d_x1 = x1;
  d_y1 = y1 + height; /* move box origin to top left */

  /* lower right corner of the box */
  d_x2 = x1 + width;  /* end points of the box */
  d_y2 = y1;

  /* create a new box */
  new_obj = o_box_new (color, d_x1, d_y1, d_x2, d_y2);

  /* set its line options */
  new_obj->line_options->line_end     = box_end;
  new_obj->line_options->line_type    = box_type;
  new_obj->line_options->line_width   = box_width;
  new_obj->line_options->line_length  = box_length;
  new_obj->line_options->line_space   = box_space;

  /* set its fill options */
  new_obj->fill_options->fill_type   = box_filling;
  new_obj->fill_options->fill_width  = fill_width;
  new_obj->fill_options->fill_angle1 = angle1;
  new_obj->fill_options->fill_angle2 = angle2;
  new_obj->fill_options->fill_pitch1 = pitch1;
  new_obj->fill_options->fill_pitch2 = pitch2;

  return new_obj;
}

/*! \brief Create a character string representation of a Box.
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe the
 *  box object <B>*object</B>.
 *  It follows the post-20000704 release file format that handle the line type
 *  and fill options.
 *
 *  \param [in] object  The Box Object to create string from.
 *  \return A pointer to the Box character string.
 *
 *  \warning
 *  Caller must GEDA_FREE returned character string.
 */
char *o_box_save(GedaObject *object)
{
  int x1, y1;
  int width, height;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  LINE_END box_end;
  LINE_TYPE  box_type;
  OBJECT_FILLING box_fill;
  Box  *box;
  char *buf;

  g_return_val_if_fail(GEDA_IS_BOX(object), NULL);
  box = GEDA_BOX(object);

  /*! \note
   *  A box is internally represented by its lower right and upper left corner
   *  whereas it is described in the file format as its lower left corner and
   *  its width and height.
   */

  /* calculate the width and height of the box */
  width  = abs(box->lower_x - box->upper_x);
  height = abs(box->upper_y - box->lower_y);

  /* calculate the lower left corner of the box */
  x1 = box->upper_x;
  y1 = box->upper_y - height; /* move the origin to 0, 0*/

  #if DEBUG
  printf("box: %d %d %d %d\n", x1, y1, width, height);
  #endif

  /* description of the line type for the outline */
  box_end    = object->line_options->line_end;
  box_width  = object->line_options->line_width;
  box_type   = object->line_options->line_type;
  box_length = object->line_options->line_length;
  box_space  = object->line_options->line_space;

  /* description of the filling of the box */
  box_fill   = object->fill_options->fill_type;
  fill_width = object->fill_options->fill_width;
  angle1     = object->fill_options->fill_angle1;
  pitch1     = object->fill_options->fill_pitch1;
  angle2     = object->fill_options->fill_angle2;
  pitch2     = object->fill_options->fill_pitch2;

  buf = u_string_sprintf("%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                         object->type,
                         x1, y1, width, height, object->color,
                         box_width, box_end, box_type, box_length, box_space,
                         box_fill,
                         fill_width, angle1, pitch1, angle2, pitch2);

  return(buf);
}

/*! \brief Mirror Box using WORLD coordinates.
 *
 *  \par Function Description
 *  This function mirrors the box from the point
 *  (<B>center_x</B>,<B>center_y</B>) in world unit.
 *
 *  The box is first translated to the origin, then mirrored and finally
 *  translated back at its previous position.
 *
 *  \param [in,out] object    Box Object to mirror
 *  \param [in]     center_x  Origin x coordinate in WORLD units
 *  \param [in]     center_y  Origin y coordinate in WORLD units
 */
void o_box_mirror(GedaObject *object, int center_x, int center_y)
{
  int newx1, newy1;
  int newx2, newy2;

  /* translate object to origin */
  object->box->upper_x -= center_x;
  object->box->upper_y -= center_y;
  object->box->lower_x -= center_x;
  object->box->lower_y -= center_y;

  /* mirror the corners */
  newx1 = -object->box->upper_x;
  newy1 =  object->box->upper_y;
  newx2 = -object->box->lower_x;
  newy2 =  object->box->lower_y;

  /* reorder the corners */
  object->box->upper_x = min(newx1,newx2);
  object->box->upper_y = max(newy1,newy2);
  object->box->lower_x = max(newx1,newx2);
  object->box->lower_y = min(newy1,newy2);

  /* translate back in position */
  object->box->upper_x += center_x;
  object->box->upper_y += center_y;
  object->box->lower_x += center_x;
  object->box->lower_y += center_y;

  /* recalc boundings and world coords */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Rotate Box Object using WORLD coordinates.
 *  \par Function Description
 *  The function #o_box_rotate() rotate the box described by
 *  <B>*object</B> around the (<B>center_x</B>, <B>center_y</B>) point by
 *  <B>angle</B> degrees.
 *  The center of rotation is in world unit.
 *
 *  \param [in,out]  object    Box Object to rotate
 *  \param [in]      center_x  Rotation center x coordinate in WORLD units
 *  \param [in]      center_y  Rotation center y coordinate in WORLD units
 *  \param [in]      angle     Rotation angle in degrees (See note below)

 *
 */
void o_box_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  int newx1, newy1;
  int newx2, newy2;

  /*! \note
   *  Only 90 degree multiple and positive angles are allowed.
   */

  /* angle must be positive */
  if (angle < 0) angle = -angle;
  /* angle must be a 90 multiple or no rotation performed */
  if ((angle % 90) != 0) return;

  /*! \note
   *  The center of rotation (<B>center_x</B>, <B>center_y</B>) is
   *  translated to the origin. The rotation of the upper left and lower right
   *  corner are then performed. Finally, the rotated box is translated back
   *  to its previous location.
   */
  /* translate object to origin */
  object->box->upper_x -= center_x;
  object->box->upper_y -= center_y;
  object->box->lower_x -= center_x;
  object->box->lower_y -= center_y;

  /* rotate the upper left corner of the box */
  m_rotate_point_90(object->box->upper_x, object->box->upper_y, angle,
                  &newx1, &newy1);

  /* rotate the lower left corner of the box */
  m_rotate_point_90(object->box->lower_x, object->box->lower_y, angle,
                  &newx2, &newy2);

  /* reorder the corners after rotation */
  object->box->upper_x = min(newx1,newx2);
  object->box->upper_y = max(newy1,newy2);
  object->box->lower_x = max(newx1,newx2);
  object->box->lower_y = min(newy1,newy2);

  /* translate object back to normal position */
  object->box->upper_x += center_x;
  object->box->upper_y += center_y;
  object->box->lower_x += center_x;
  object->box->lower_y += center_y;

  /* recalc boundings and world coords */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Translate a Box position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the box
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 *  \param [in,out] object     Box Object to translate
 *  \param [in]     dx         x distance to move
 *  \param [in]     dy         y distance to move
 */
void o_box_translate(GedaObject *object, int dx, int dy)
{
  /* Do world coords */
  object->box->upper_x = object->box->upper_x + dx;
  object->box->upper_y = object->box->upper_y + dy;
  object->box->lower_x = object->box->lower_x + dx;
  object->box->lower_y = object->box->lower_y + dy;

  /* recalc the screen coords and the bounding box */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Get Point on a Box Nearest a Given Point
 *  \par Function Description
 *  This function is intended to locate a point on a Box object given
 *  a point \a x, \a y, that is on or about the vicinity of \a object. If
 *  True is returned, <B>nx</B> and <B>ny</B> are set in world unit to a point
 *  on the box that is the closest point on the box to the point given by \a x, \a y.
 *
 *  \param [in]  object  Pointer to a Box object
 *  \param [in]  x       Integer x of point near or on the box
 *  \param [in]  y       Integer y of point near or on the box
 *  \param [out] nx      Integer pointer to resulting x value
 *  \param [out] ny      Integer pointer to resulting y value
 *
 *  \returns TRUE is the results are valid, FALSE if \a object was not a Box.
 */
bool o_box_get_nearest_point (GedaObject *object, int x, int y, int *nx, int *ny)
{
  Box *box;
  bool result;

  if (GEDA_IS_BOX(object)) {

    Line *closest;
    Line  segments[4];

    box    = object->box;
    result = FALSE;

    int  left   = /* min */ box->upper_x < box->lower_x ? box->upper_x : box->lower_x;
    int  bottom = /* min */ box->upper_y < box->lower_y ? box->upper_y : box->lower_y;
    int  right  = /* max */ box->upper_x > box->lower_x ? box->upper_x : box->lower_x;
    int  top    = /* max */ box->upper_y > box->lower_y ? box->upper_y : box->lower_y;

    if (left >= x) {

      *nx = left;

      if (y >= top) {
        *ny = top;
      }
      else if (bottom >= y) {
        *ny = bottom;
      }
      else {
        *ny = y;
      }
    }
    else if (x >= right) {

      *nx = right;

      if (y >= top) {
        *ny = top;
      }
      else if (bottom >= y) {
        *ny = bottom;
      }
      else {
        *ny = y;
      }
    }
    else if (y >= top) {
      *ny = top;
      *nx = x;
    }
    else if (bottom >= y) {
      *ny = bottom;
      *nx = x;
    }
    else { /* point is inside the box */

      double dl, dr, dt, db;

      /* Left Side */
      segments[0].x[0] = left;
      segments[0].y[0] = bottom;
      segments[0].x[1] = left;
      segments[0].y[1] = top;

      dl = m_line_shortest_distance (&segments[0], x, y);

      /* Right Side */
      segments[1].x[0] = right;
      segments[1].y[0] = bottom;
      segments[1].x[1] = right;
      segments[1].y[1] = top;

      dr = m_line_shortest_distance (&segments[1], x, y);

      /* Top Side */
      segments[2].x[0] = left;
      segments[2].y[0] = top;
      segments[2].x[1] = right;
      segments[2].y[1] = top;

      dt = m_line_shortest_distance (&segments[2], x, y);

      /* Bottom Side */
      segments[3].x[0] = left;
      segments[3].y[0] = bottom;
      segments[3].x[1] = right;
      segments[3].y[1] = bottom;

      db = m_line_shortest_distance (&segments[3], x, y);

      /* Check for corners */

      if (db == dl) {                       /* bottom left */
        *ny = bottom;
        *nx = left;
      }
      else if (dt == dl) {                  /* top left */
        *ny = top;
        *nx = left;
      }

      if (db == dr) {                       /* bottom right */
        *ny = bottom;
        *nx = right;
      }
      else if (dt == dr) {                  /* top right */
        *ny = top;
        *nx = right;
      }
      else {

        if (dl < db && dl < dt) {           /* left */
          closest = &segments[0];
        }
        else if (dr < db && dr < dt) {      /* right */
          closest = &segments[1];
        }
        else if (db > dt) {                 /* top */
          closest = &segments[2];
        }
        else {                              /* bottom */
          closest = &segments[3];
        }

        double dx, dy, ix, iy;
        double m1, m2, b1, b2;

        dx = closest->x[1] - closest->x[0];
        dy = closest->y[1] - closest->y[0];

        m1 = dy / dx;
        b1 = closest->y[0] - m1 * closest->x[0];
        m2 = -1 / m1;
        b2 = y - m2 * x;

        ix = (b2 - b1) / (m1 - m2);
        iy = m2 * ix + b2;

#ifdef HAVE_LRINT

        *nx = lrint(ix);
        *nx = lrint(iy);

#else

        *nx = ix + 0.5;
        *nx = iy + 0.5;

#endif
      }
    }
    result = TRUE;
  }
  else { /* was not an Box */
    result = FALSE;
  }

  if (!result) {
    *nx = x;
    *ny = y;
  }
  return result;
}

/*! \brief get the position of the left bottom point
 *  \par Function Description
 *  This function gets the position of the bottom left point of a box object.
 *
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
bool o_box_get_position (int *x, int *y, GedaObject *object)
{
  *x = min(object->box->lower_x, object->box->upper_x);
  *y = min(object->box->lower_y, object->box->upper_y);
  return TRUE;
}

/*! \brief Print Box to Postscript document.
 *  \par Function Description
 *  This function prints the box described by the <B>\a o_current</B>
 *  parameter to a Postscript document. It takes into account its line
 *  type and fill type.
 *  The Postscript document is descibed by the file pointer <B>fp</B>.
 *
 *  The validity of the <B>\a o_current</B> parameter is verified : a null pointer
 *  causes an error message and a return.
 *
 *  The description of the box is extracted from
 *  the <B>\a o_current</B> parameter :
 *  the coordinates of the box - upper left corner and width and
 *  height of the box -, its line type, its fill type.
 *
 *  The outline and the inside of the box are successively handled by two
 *  differend sets of functions.
 *
 *  \param [in] toplevel  The GedaToplevel object.
 *  \param [in] fp         FILE pointer to Postscript document.
 *  \param [in] o_current  Box Object to write to document.
 *  \param [in] origin_x   Page x coordinate to place Box Object.
 *  \param [in] origin_y   Page y coordinate to place Box Object.
 */
void o_box_print(GedaToplevel *toplevel, FILE *fp, GedaObject *o_current,
                 int origin_x, int origin_y)
{
  int x, y, width, height;
  int color;
  int line_width, capstyle, length, space;
  void (*outl_func)() = NULL;
  void (*fill_func)() = NULL;
  Box  *box;

  if (o_current == NULL) {
    printf("got null in o_box_print\n");
    return;
  }

  g_return_if_fail(GEDA_IS_BOX(o_current));
  box = GEDA_BOX(o_current);

  x = box->upper_x;
  y = box->upper_y;
  width  = abs(box->lower_x - box->upper_x);
  height = abs(box->lower_y - box->upper_y);

  color  = o_current->color;
  capstyle = o_get_capstyle (o_current->line_options->line_end);

  /*! \note
   *  Depending on the type of the line for this particular box, the
   *  appropriate function is chosen among #o_box_print_solid(),
   *  #o_box_print_dotted(), #o_box_print_dashed(),
   *  #o_box_print_center() and #o_box_print_phantom().
   *
   *  The needed parameters for each of these type is extracted from the
   *  <B>Box</B> object. Depending on the type, unused parameters are
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

  if (line_width < MIN_LINE_WIDTH_THRESHOLD)
    line_width = o_style_get_line_width(toplevel); /* 1st try updating style */

  if (line_width < MIN_LINE_WIDTH_THRESHOLD)
    line_width = MIN_LINE_WIDTH_THRESHOLD;        /* if STYLE_NONE  */

  length = o_current->line_options->line_length;
  space  = o_current->line_options->line_space;

  switch(o_current->line_options->line_type) {
    case(TYPE_SOLID):
      length = -1; space  = -1;
      outl_func = o_box_print_solid;
      break;

    case(TYPE_DOTTED):
      length = -1;
      outl_func = o_box_print_dotted;
      break;

    case(TYPE_DASHED):
      outl_func = o_box_print_dashed;
      break;

    case(TYPE_CENTER):
      outl_func = o_box_print_center;
      break;

    case(TYPE_PHANTOM):
      outl_func = o_box_print_phantom;
      break;

    case(TYPE_ERASE):
      /* Unused for now, print it solid */
      length = -1; space  = -1;
      outl_func = o_box_print_solid;
      break;
  }

  if ((length == 0) || (space == 0)) {
    length = -1; space  = -1;
    outl_func = o_box_print_solid;
  }

  (*outl_func)(toplevel, fp,
               x, y, width, height,
               color,
               line_width,
               capstyle,
               length, space,
               origin_x, origin_y);

  /*! \note
   *  If the filling type of the box is not <B>HOLLOW</B>, the appropriate
   *  function is chosen among #o_box_print_filled(), #o_box_print_mesh()
   *  and #o_box_print_hatch(). The corresponding parameters are extracted
   *  from the <B>\a o_current</B> object and corrected afterward.
   *
   *  The case where <B>pitch1</B> and <B>pitch2</B> are null or negative is
   *  avoided as it leads to an endless loop in most of the called functions.
   *  In such a case, the box is printed filled. Unused parameters for each of
   *  these functions are set to -1 or any passive value.
   */
  if (box->fill_options.fill_type != FILLING_HOLLOW) {

    int fill_width, angle1, pitch1, angle2, pitch2;

    fill_width = box->fill_options.fill_width;
    angle1     = box->fill_options.fill_angle1;
    pitch1     = box->fill_options.fill_pitch1;
    angle2     = box->fill_options.fill_angle2;
    pitch2     = box->fill_options.fill_pitch2;

    switch(box->fill_options.fill_type) {
      case(FILL_SOLID):
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func = o_box_print_filled;
        break;

      case(FILLING_MESH):
        fill_func = o_box_print_mesh;
        break;

      case(FILLING_HATCH):
        angle2 = -1; pitch2 = 1;
        fill_func = o_box_print_hatch;
        break;

      case(FILLING_VOID):
        /* Unused for now, print it filled */
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func = o_box_print_filled;
        break;
      case(FILLING_HOLLOW):
        /* nop */
        break;

    }

    if ((pitch1 <= 0) || (pitch2 <= 0)) {
      angle1 = -1; pitch1 = 1;
      angle2 = -1; pitch2 = 1;
      fill_func = o_box_print_filled;
    }

    (*fill_func)(toplevel, fp,
                 x, y, width, height,
                 color,
                 fill_width,
                 angle1, pitch1, angle2, pitch2,
                 origin_x, origin_y);
  }
}

/*! \brief Print a solid Box to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a solid line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> and <B>space</B> are ignored.
 *
 *  It uses the function #o_line_print_solid() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of Box.
 *  \param [in] y           Upper y coordinate of Box.
 *  \param [in] width       Width of Box.
 *  \param [in] height      Height of Box.
 *  \param [in] color       Box color.
 *  \param [in] line_width  Box Line width.
 *  \param [in] capstyle    Box Line capstyle.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place Box Object.
 *  \param [in] origin_y    Page y coordinate to place Box Object.
 */
void o_box_print_solid(GedaToplevel *toplevel, FILE *fp,
                       int x, int y,
                       int width, int height,
                       int color,
                       int line_width, int capstyle, int length, int space,
                       int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_solid(toplevel, fp,
                     x1, y1, x1 + width, y1,
                     color,
                     line_width, capstyle, length, space,
                     origin_x, origin_y);
  o_line_print_solid(toplevel, fp,
                     x1 + width, y1, x1 + width, y1 + height,
                     color,
                     line_width, capstyle, length, space,
                     origin_x, origin_y);
  o_line_print_solid(toplevel, fp,
                     x1 + width, y1 + height, x1, y1 + height,
                     color,
                     line_width, capstyle, length, space,
                     origin_x, origin_y);
  o_line_print_solid(toplevel, fp,
                     x1, y1 + height, x1, y1,
                     color,
                     line_width, capstyle, length, space,
                     origin_x, origin_y);
}

/*! \brief Print a dotted Box to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a dotted line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> is ignored.
 *
 *  It uses the function #o_line_print_dotted() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of Box.
 *  \param [in] y           Upper y coordinate of Box.
 *  \param [in] width       Width of Box.
 *  \param [in] height      Height of Box.
 *  \param [in] color       Box color.
 *  \param [in] line_width  Box Line width.
 *  \param [in] capstyle    Box Line capstyle.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place Box Object.
 *  \param [in] origin_y    Page y coordinate to place Box Object.
 */
void o_box_print_dotted(GedaToplevel *toplevel, FILE *fp,
                        int x, int y,
                        int width, int height,
                        int color,
                        int line_width, int capstyle, int length, int space,
                        int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_dotted(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_dotted(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_dotted(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_dotted(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
}

/*! \brief Print a dashed Box to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a dashed line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #o_line_print_dashed() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of Box.
 *  \param [in] y           Upper y coordinate of Box.
 *  \param [in] width       Width of Box.
 *  \param [in] height      Height of Box.
 *  \param [in] color       Box color.
 *  \param [in] line_width  Box Line width.
 *  \param [in] capstyle    Box Line capstyle.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place Box Object.
 *  \param [in] origin_y    Page y coordinate to place Box Object.
 */
void o_box_print_dashed(GedaToplevel *toplevel, FILE *fp,
                        int x, int y,
                        int width, int height,
                        int color,
                        int line_width, int capstyle, int length, int space,
                        int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);


  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_dashed(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_dashed(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_dashed(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_dashed(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
}

/*! \brief Print centered line type Box to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a centered line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #o_line_print_center() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of Box.
 *  \param [in] y           Upper y coordinate of Box.
 *  \param [in] width       Width of Box.
 *  \param [in] height      Height of Box.
 *  \param [in] color       Box color.
 *  \param [in] line_width  Box Line width.
 *  \param [in] capstyle    Box Line capstyle.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place Box Object.
 *  \param [in] origin_y    Page y coordinate to place Box Object.
 */
void o_box_print_center(GedaToplevel *toplevel, FILE *fp,
                        int x, int y,
                        int width, int height,
                        int color,
                        int line_width, int capstyle, int length, int space,
                        int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_center(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_center(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_center(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
  o_line_print_center(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, capstyle, length, space,
                      origin_x, origin_y);
}

/*! \brief Print phantom line type Box to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a phantom line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #o_line_print_phantom() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of Box.
 *  \param [in] y           Upper y coordinate of Box.
 *  \param [in] width       Width of Box.
 *  \param [in] height      Height of Box.
 *  \param [in] color       Box color.
 *  \param [in] line_width  Box Line width.
 *  \param [in] capstyle    Box Line capstyle.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place Box Object.
 *  \param [in] origin_y    Page y coordinate to place Box Object.
 */
void o_box_print_phantom(GedaToplevel *toplevel, FILE *fp,
                         int x, int y,
                         int width, int height,
                         int color,
                         int line_width, int capstyle, int length, int space,
                         int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_phantom(toplevel, fp,
                       x1, y1, x1 + width, y1,
                       color,
                       line_width, capstyle, length, space,
                       origin_x, origin_y);
  o_line_print_phantom(toplevel, fp,
                       x1 + width, y1, x1 + width, y1 + height,
                       color,
                       line_width, capstyle, length, space,
                       origin_x, origin_y);
  o_line_print_phantom(toplevel, fp,
                       x1 + width, y1 + height, x1, y1 + height,
                       color,
                       line_width, capstyle, length, space,
                       origin_x, origin_y);
  o_line_print_phantom(toplevel, fp,
                       x1, y1 + height, x1, y1,
                       color,
                       line_width, capstyle, length, space,
                       origin_x, origin_y);
}

/*! \brief Print a solid pattern Box to Postscript document.
 *  \par Function Description
 *  The function prints a filled box with a solid pattern. No outline is
 *  printed.
 *  The box is defined by the coordinates of its upper left corner in
 *  (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters. The postscript file is defined by the file
 *  pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B> and <B>pitch1</B>, <B>angle2</B> and <B>pitch2</B>
 *  parameters are ignored in this functions but kept for compatibility
 *  with other fill functions.
 *
 *  It uses the fbox postscript function defined in the prolog to
 *  specify a filled box.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of Box.
 *  \param [in] y           Upper y coordinate of Box.
 *  \param [in] width       Width of Box.
 *  \param [in] height      Height of Box.
 *  \param [in] color       Box color.
 *  \param [in] fill_width  Box fill width. (unused).
 *  \param [in] angle1      (unused).
 *  \param [in] pitch1      (unused).
 *  \param [in] angle2      (unused).
 *  \param [in] pitch2      (unused).
 *  \param [in] origin_x    Page x coordinate to place Box Object.
 *  \param [in] origin_y    Page y coordinate to place Box Object.
 */
void o_box_print_filled(GedaToplevel *toplevel, FILE *fp,
                        int x, int y,
                        int width, int height,
                        int color,
                        int fill_width,
                        int angle1, int pitch1,
                        int angle2, int pitch2,
                        int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y-height; /* move the origin to 0, 0*/
  fprintf(fp, "%d %d %d %d fbox\n",
	  width, height,
	  x1-origin_x, y1-origin_y);

}

/*! \brief Print a mesh pattern Box to Postscript document.
 *  \par Function Description
 *  This function prints a meshed box. No outline is printed. The box is
 *  defined by the coordinates of its upper left corner in (<B>x</B>,<B>y</B>) and
 *  its width and height given by the <B>width</B> and <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  The inside mesh is achieved by two successive call to the
 *  #o_box_print_hatch() function, given <B>angle1</B> and <B>pitch1</B> the first
 *  time and <B>angle2</B> and <B>pitch2</B> the second time.
 *
 *  Negative or null values for <B>pitch1</B> and/or <B>pitch2</B> are not allowed
 *  as it leads to an endless loop in #o_box_print_hatch().
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of Box.
 *  \param [in] y           Upper y coordinate of Box.
 *  \param [in] width       Width of Box.
 *  \param [in] height      Height of Box.
 *  \param [in] color       Box color.
 *  \param [in] fill_width  Box fill width.
 *  \param [in] angle1      1st angle for mesh pattern.
 *  \param [in] pitch1      1st pitch for mesh pattern.
 *  \param [in] angle2      2nd angle for mesh pattern.
 *  \param [in] pitch2      2nd pitch for mesh pattern.
 *  \param [in] origin_x    Page x coordinate to place Box Object.
 *  \param [in] origin_y    Page y coordinate to place Box Object.
 */
void o_box_print_mesh(GedaToplevel *toplevel, FILE *fp,
                      int x, int y,
                      int width, int height,
                      int color,
                      int fill_width,
                      int angle1, int pitch1,
                      int angle2, int pitch2,
                      int origin_x, int origin_y)
{
  o_box_print_hatch(toplevel, fp,
                    x, y, width, height,
                    color,
                    fill_width,
                    angle1, pitch1, -1, -1,
                    origin_x, origin_y);
  o_box_print_hatch(toplevel, fp,
                    x, y, width, height,
                    color,
                    fill_width,
                    angle2, pitch2, -1, -1,
                    origin_x, origin_y);

}

/*! \brief Print a hatch pattern Box to Postscript document.
 *  \par Function Description
 *  The function prints a hatched box. No outline is printed. The box is
 *  defined by the coordinates of its upper left corner in (<B>x</B>,<B>y</B>) and
 *  its width and height given by the <B>width</B> and <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B>, <B>pitch1</B> parameters define the way the box
 *  has to be hatched.
 *  <B>angle2</B> and <B>pitch2</B> parameters are unused but kept for compatibility
 *  with other fill functions.
 *
 *  Negative or null values for <B>pitch1</B> are not allowed as it leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of Box.
 *  \param [in] y           Upper y coordinate of Box.
 *  \param [in] width       Width of Box.
 *  \param [in] height      Height of Box.
 *  \param [in] color       Box color.
 *  \param [in] fill_width  Box fill width.
 *  \param [in] angle1      Angle of hatch pattern.
 *  \param [in] pitch1      Pitch of hatch pattern.
 *  \param [in] angle2      (unused).
 *  \param [in] pitch2      (unused).
 *  \param [in] origin_x    Page x coordinate to place Box Object.
 *  \param [in] origin_y    Page y coordinate to place Box Object.
 */
void o_box_print_hatch(GedaToplevel *toplevel, FILE *fp,
                       int x, int y,
                       int width, int height,
                       int color,
                       int fill_width,
                       int angle1, int pitch1,
                       int angle2, int pitch2,
                       int origin_x, int origin_y)
{
  Box box;
  int index;
  GArray *lines;

  g_return_if_fail(toplevel != NULL);
  g_return_if_fail(fp != NULL);

  f_print_set_color(toplevel, fp, color);

  /* Avoid printing line widths too small */
  if (fill_width <= 1) fill_width = 2;

  lines = g_array_new(FALSE, FALSE, sizeof(LINE));

  box.upper_x = x;
  box.upper_y = y;
  box.lower_x = x + width;
  box.lower_y = y - height;    /* Hmmm... */

  m_hatch_box(&box, angle1, pitch1, lines);

  for(index=0; index<lines->len; index++) {
    LINE *line = &g_array_index(lines, LINE, index);

    fprintf(fp,"%d %d %d %d %d %d line\n",
            line->x[0], line->y[0],
            line->x[1], line->y[1],
            fill_width, BUTT_CAP);
  }

  g_array_free(lines, TRUE);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the perimeter of the box.
 *
 *  \param [in] object       A box GedaObject.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double o_box_shortest_distance (GedaObject *object, int x, int y, int force_solid)
{
  int solid;

  g_return_val_if_fail (GEDA_IS_BOX(object), G_MAXDOUBLE);

  solid = force_solid || object->fill_options->fill_type != FILLING_HOLLOW;

  return m_box_shortest_distance (object->box, x, y, solid);
}

