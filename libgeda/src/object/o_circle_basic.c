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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*! \file o_circle_basic.c
 *  \brief functions for the circle object
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include <libgeda_priv.h>

/*! \brief Create and add circle Object to list.
 *  \par Function Description
 *  This function creates a new object representing a circle.
 *
 *  The circle is described by its center (<B>x</B>,<B>y</B>) and its radius
 *  <B>radius</B>.
 *  The <B>color</B> corresponds to the color the box will be drawn with.
 *
 *  The <B>GedaObject</B> structure is allocated with the#geda_object_new()
 *  function. The structure describing the circle is allocated and initialized
 *  with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default values : solid
 *  line type with a width of 0, and no filling. It can be changed after
 *  with#o_set_line_options() and#o_set_fill_options().
 *
 *  \param [in]     color        Circle line color.
 *  \param [in]     x            Center x coordinate.
 *  \param [in]     y            Center y coordinate.
 *  \param [in]     radius       Radius of new circle.
 *  \return A pointer to the new end of the object list.
 */
GedaObject*
o_circle_new(int color, int x, int y, int radius)
{
  GedaObject *new_obj;
  Circle *circle;

  new_obj = geda_circle_new();
  circle  = GEDA_CIRCLE(new_obj);

  new_obj->color = color;

  /* describe the circle with its center and radius */
  circle->center_x = x;
  circle->center_y = y;
  circle->radius   = radius;

  return new_obj;
}

/*! \brief Create a copy of a circle.
 *
 *  \par Function Description
 *  The function#o_circle_copy() creates a verbatim copy of the object
 *  pointed by <B>\a o_current</B> describing a circle.
 *
 *  \param [in]  o_current  Circle GedaObject to copy.
 *  \return The new GedaObject
 */
GedaObject *o_circle_copy(GedaObject *o_current)
{
  GedaObject *new_obj;
  Circle *old_circle;

  g_return_val_if_fail(GEDA_IS_CIRCLE(o_current), NULL);

  old_circle = GEDA_CIRCLE(o_current);

  /* A new circle object is created with#o_circle_new().
   * Values for its fields are default and need to be modified. */
  new_obj = o_circle_new (o_current->color, 0, 0, 0);

  /* The parameters of the new circle are set with the ones of the original
   * circle. The two circle have the same line type and filling options.
   */

  new_obj->circle->center_x = old_circle->center_x;
  new_obj->circle->center_y = old_circle->center_y;
  new_obj->circle->radius   = old_circle->radius;

  o_set_line_options(new_obj, &old_circle->line_options);
  o_set_fill_options(new_obj, &old_circle->fill_options);

  return new_obj;
}

/*! \brief Modify the description of a circle Object.
 *  \par Function Description
 *  This function modifies the description of the circle object <B>*object</B>
 *  depending on <B>whichone</B> that give the meaning of the <B>x</B> and <B>y</B>
 *  parameters.
 *
 *  If <B>whichone</B> is equal to <B>CIRCLE_CENTER</B>, the new center of the
 *  circle is given by (<B>x</B>,<B>y</B>) where <B>x</B> and <B>y</B> are in world units.
 *
 *  If <B>whichone</B> is equal to <B>CIRCLE_RADIUS</B>, the radius is given by
 *  <B>x</B> - in world units. <B>y</B> is ignored.
 *
 *  The bounding box of the circle object is updated after the modification of its
 *  parameters.
 *
 *  \param [in,out] object     Circle GedaObject to modify.
 *  \param [in]     x          New center x coordinate, or radius value.
 *  \param [in]     y          New center y coordinate.
 *                             Unused if radius is being modified.
 *  \param [in]     whichone   Which circle parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>CIRCLE_CENTER
 *    <DT>*</DT><DD>CIRCLE_RADIUS
 *  </DL>
 */
void o_circle_modify(GedaObject *object, int x, int y, int whichone)
{
  switch(whichone) {
    case CIRCLE_CENTER:
      /* modify the center of the circle */
      object->circle->center_x = x;
      object->circle->center_y = y;
      break;
    case CIRCLE_RADIUS:
      /* modify the radius of the circle */
      if (x == 0) {
        u_log_message(_("Null radius circles are not allowed\n"));
        return;
      }
      object->circle->radius = x;
      break;
    default:
      break;
  }

  /* recalculate the boundings */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Create circle Object from character string.
 *
 *  \par Function Description
 *  The#o_circle_read() function gets from the character string <B>*buff</B> the
 *  description of a circle.
 *
 *  Depending on <B>*version</B>, the right file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 2000704 release.
 *    <DT>*</DT><DD>the file format used for the releases after 20000704.
 *  </DL>
 *
 *  \param [in]  buf             Character string with circle description.
 *  \param [in]  release_ver     libgeda release version number.
 *  \param [in]  fileformat_ver  libgeda file format version number.
 *
 *  \param [out] err             A GError object
 *
 *  \return A pointer to the new circle object, or NULL on error.
 */
GedaObject *o_circle_read (const char buf[], unsigned int release_ver,
                                         unsigned int fileformat_ver,
                                         GError **err)
{
  GedaObject *new_obj;
  char type;
  int x1, y1;
  int radius;
  int color;
  int circle_width, circle_space, circle_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  int circle_end;
  int circle_type;
  int circle_fill;

  if(release_ver <= VERSION_20000704) {
    /*
     * The old geda file format, i.e. releases 20000704 and older, does not
     * handle the line type and the filling of the box object. They are set
     * to default.
     */
    if (sscanf(buf, "%c %d %d %d %d\n", &type, &x1, &y1, &radius, &color) != 5) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse circle object"));
      return NULL;
    }

    circle_width  = 0;
    circle_end    = END_NONE;
    circle_type   = TYPE_SOLID;
    circle_length = -1;
    circle_space  = -1;

    circle_fill   = FILLING_HOLLOW;
    fill_width    = 0;
    angle1        = -1;
    pitch1        = -1;
    angle2        = -1;
    pitch2        = -1;

  } else {

    /*
     * The current line format to describe a circle is a space separated
     * list of characters and numbers in plain ASCII on a single line. The
     * meaning of each item is described in the file format documentation.
     */
    if (sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
      &type, &x1, &y1, &radius, &color,
      &circle_width, &circle_end, &circle_type,
      &circle_length, &circle_space, &circle_fill,
      &fill_width, &angle1, &pitch1, &angle2, &pitch2) != 16) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse circle object"));
    return NULL;
      }
  }


  if (radius <= 0) {
    u_log_message(_("Found a zero or negative radius circle [ %c %d %d %d %d ]\n"),
                  type, x1, y1, radius, color);
    u_log_message (_("Setting radius to 0\n"));
    radius = 0;
  }

  if (color < 0 || color > MAX_COLORS) {
    u_log_message(_("Found an invalid color [ %s ]\n"), buf);
    u_log_message(_("Setting color to default color\n"));
    color = DEFAULT_CIRCLE_COLOR_INDEX;
  }

  /*
   * A circle is internally described by its center and its radius.
   *
   * A new object is allocated, initialized and added to the object list.
   * Its filling and line type are set according to the values of the field
   * on the line.
   */
  new_obj = o_circle_new(color, x1, y1, radius);

  new_obj->line_options->line_end     = circle_end;
  new_obj->line_options->line_type    = circle_type;
  new_obj->line_options->line_width   = circle_width;
  new_obj->line_options->line_length  = circle_length;
  new_obj->line_options->line_space   = circle_space;

  /* set its fill options */
  new_obj->fill_options->fill_type   = circle_fill;
  new_obj->fill_options->fill_width  = fill_width;
  new_obj->fill_options->fill_angle1 = angle1;
  new_obj->fill_options->fill_angle2 = angle2;
  new_obj->fill_options->fill_pitch1 = pitch1;
  new_obj->fill_options->fill_pitch2 = pitch2;
  return new_obj;
}

/*! \brief Create a character string representation of a circle Object
 *
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe the
 *  circle object <B>*object</B>.
 *  It follows the post-20000704 release file format that handle the line
 *  type and fill options.
 *
 *  \param [in] object  Circle GedaObject to create string from.
 *
 *  \return A pointer to the circle Object character string.
 *
 *  \note
 *  Caller must GEDA_FREE returned character string.
 *
 */
char *o_circle_save(GedaObject *object)
{
  int x,y;
  int radius;
  int circle_width, circle_space, circle_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  char *buf;
  LINE_END circle_end;
  LINE_TYPE  circle_type;
  OBJECT_FILLING circle_fill;

  g_return_val_if_fail(GEDA_IS_CIRCLE(object), NULL);

  /* circle center and radius */
  x = object->circle->center_x;
  y = object->circle->center_y;
  radius = object->circle->radius;

  /* line type parameters */
  circle_width = object->circle->line_options.line_width;
  circle_end   = object->circle->line_options.line_end;
  circle_type  = object->circle->line_options.line_type;
  circle_length= object->circle->line_options.line_length;
  circle_space = object->circle->line_options.line_space;

  /* filling parameters */
  circle_fill  = object->circle->fill_options.fill_type;
  fill_width   = object->circle->fill_options.fill_width;
  angle1       = object->circle->fill_options.fill_angle1;
  pitch1       = object->circle->fill_options.fill_pitch1;
  angle2       = object->circle->fill_options.fill_angle2;
  pitch2       = object->circle->fill_options.fill_pitch2;

  buf = u_string_sprintf("%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                         object->type, x, y, radius, object->color,
                         circle_width, circle_end, circle_type, circle_length,
                         circle_space, circle_fill,
                         fill_width, angle1, pitch1, angle2, pitch2);
  return(buf);
}

/*! \brief Mirror circle using WORLD coordinates.
 *
 *  \par Function Description
 *  This function recalculates the screen coords of the <B>\a o_current</B> pointed
 *  circle object from its world coords.
 *
 *  The circle coordinates and its bounding are recalculated as well as the
 *  GedaObject specific (line width, filling ...).
 *
 *  \param [in,out] object    Circle GedaObject to mirror.
 *  \param [in]     center_x  Origin x coordinate in WORLD units.
 *  \param [in]     center_y  Origin y coordinate in WORLD units.

 */
void o_circle_mirror(GedaObject *object, int center_x, int center_y)
{
  /* translate object to origin */
  object->circle->center_x -= center_x;
  object->circle->center_y -= center_y;

  /* mirror the center of the circle */
  object->circle->center_x = -object->circle->center_x;

  /* translate back in position */
  object->circle->center_x += center_x;
  object->circle->center_y += center_y;

  /* recalc boundings and screen coords */
  object->w_bounds_valid_for = NULL;

}

/*! \brief Rotate Circle GedaObject using WORLD coordinates
 *
 *  \par Function Description
 *  The function#o_circle_rotate() rotate the circle described by
 *  <B>*object</B> around the (<B>center_x</B>,<B>center_y</B>) point by
 *  angle <B>angle</B> degrees.
 *  The center of rotation is in world unit.
 *
 *  \param [in,out]  object    Circle GedaObject to rotate.
 *  \param [in]      center_x  Rotation center x coordinate in WORLD units.
 *  \param [in]      center_y  Rotation center y coordinate in WORLD units.
 *  \param [in]      angle     Rotation angle in degrees (See note below).

 */
void o_circle_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  int newx, newy;
  int x, y;

  /* Only 90 degree multiple and positive angles are allowed. */
  /* angle must be positive */
  if(angle < 0) angle = -angle;
  /* angle must be a 90 multiple or no rotation performed */
  if((angle % 90) != 0) return;

  /*
   * The center of rotation (<B>center_x</B>,<B>center_y</B>) is
   * translated to the origin. The rotation of the center around the origin
   * is then performed. Finally, the rotated circle is translated back to
   * its previous location.
   */

  /* translate object to origin */
  object->circle->center_x -= center_x;
  object->circle->center_y -= center_y;

  /* rotate the center of the circle around the origin */
  x = object->circle->center_x;
  y = object->circle->center_y;
  m_rotate_point_90(x, y, angle, &newx, &newy);
  object->circle->center_x = newx;
  object->circle->center_y = newy;

  /* translate back in position */
  object->circle->center_x += center_x;
  object->circle->center_y += center_y;

  object->w_bounds_valid_for = NULL;

}

/*! \brief Translate a circle position in WORLD coordinates by a delta
 *
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the circle
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 *  \param [in,out] object     Circle GedaObject to translate.
 */
void o_circle_translate(GedaObject *object, int dx, int dy)
{
  /* Do world coords */
  object->circle->center_x = object->circle->center_x + dx;
  object->circle->center_y = object->circle->center_y + dy;

  /* recalc the screen coords and the bounding box */
  object->w_bounds_valid_for = NULL;

}

/*! \brief Get Point on a Circle Nearest a Given Point
 *  \par Function Description
 *  This function is intended to locate a point on an Circle object given
 *  a point \a x, \a y, that is on or about the vicinity of the \a object.
 *  If True is returned, <B>nx</B> and <B>ny</B> are set world unit to a
 *  point on the circle that is the closest point on \a object to the point
 *  given by \a x, \a y.
 *
 *  \param [in]  object  Pointer to an Circle object
 *  \param [in]  x       Integer x of point near or on the circle
 *  \param [in]  y       Integer y of point near or on the circle
 *  \param [out] nx      Integer pointer to resulting x value
 *  \param [out] ny      Integer pointer to resulting y value
 *
 *  \returns TRUE is the results are valid, FALSE if \a object was not an
 *           Circle object, or if (<B>dx</B>,<B>dy</B>) is the centerpoint of
 *           the circle.
 */
bool o_circle_get_nearest_point (GedaObject *object, int x, int y, int *nx, int *ny)
{
  bool    result;


  if (GEDA_IS_CIRCLE(object)) {

    int cx, cy;

    cx = object->circle->center_x;
    cy = object->circle->center_y;

    /* If the point is the center, every point on the circle is equal
     * distance to the point, so answer is false */
    if ((y == cy) && (x == cx)) {
      result = FALSE;
    }
    else {

      int     x1, y1, x2, y2;
      double  dx, dy, r;
      double  A, B, C, D;

      volatile double  b;
      volatile double  m;

      r  = object->circle->radius;

      x1 = x;
      y1 = y;
      x2 = cx;
      y2 = cy;

      dx = x2 - x1;
      dy = y2 - y1;

      /* Get coefficients of quadratic */
      if (dx == 0) {                   /* In terms of Y, because X1 = X2 */

        /* Special vertical case: (x-cx)^2 + (y-cy)^2 = r^2, solve for y */

        A = 1;
        B = -2 * cy;
        C = (x1 * x1) - (2 * cx * x1) + (cx * cx) + (cy * cy) - (r * r);
      }
      else {                           /* In terms of X */

        /* Conventional: (x - cx)^2 + (mx + b - cy)^2 = r^2, solve for x */

        m  = dy / dx;
        b  = (-1 * m * x2) + y2;

        A = m * m + 1;
        B = 2 * ((m * b) - (m * cy) - cx);
        C = (cy * cy) + (cx * cx) - (r * r) - (2 * (b * cy)) + (b * b);
      }

      D = B * B - 4 * A * C;           /* The discriminant */

      /* The discriminant can not be negative */

#if HAVE_LRINT

      if (dx == 0) {      /* Vertical = special, find y first*/

       *nx = x1;          /* Line vertical so x is known */

        if (x > cx) {
         *ny = lrint((-1 * B + sqrt(D)) / (2 * A));
        }
        else {
         *ny = lrint((-1 * B - sqrt(D)) / (2 * A));
        }
      }
      else {              /* For all non-vertical line */

        double tmp_x;

        if (x > cx) {     /* Use positive root */

          tmp_x = (-1 * B + sqrt(D)) / (2 * A);

        }
        else {            /* Use negative root */

          tmp_x = (-1 * B - sqrt(D)) / (2 * A);

        }

       *nx = lrint(tmp_x);
       *ny = lrint(m * tmp_x + b); /* Must use non rounded x here */
      }

#else

      if (dx == 0) {      /* Vertical special, find y first*/

        *nx = x1;         /* Line vertical so x is known */

        if (x > cx) {     /* Use positive root */

          *ny = ((-1 * B + sqrt(D)) / (2 * A)) + 0.5;

        }
        else {

          *ny = ((-1 * B - sqrt(D)) / (2 * A)) + 0.5;

        }
      }
      else {             /* For all non-vertical line */

        double tmp_x;

        if (x > cx) {     /* Use positive root */

          tmp_x = (-1 * B + sqrt(D)) / (2 * A);

        }
        else {

          tmp_x = (-1 * B - sqrt(D)) / (2 * A);

        }
        *nx = tmp_x + 0.5;
        *ny = (m * tmp_x + b) + 0.5;  /* Must use non rounded x here */
      }

#endif

      result = TRUE;
    }
  }
  else { /* was not an Circle */
    result = FALSE;
  }

  if (!result) {
    *nx = x;
    *ny = y;
  }
  return result;
}

/*! \brief get the position of the center point
 *
 *  \par Function Description
 *  This function gets the position of the center point of a circle object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position

 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
bool o_circle_get_position (int *x, int *y, GedaObject *object)
{
  g_return_val_if_fail(GEDA_IS_CIRCLE(object), FALSE);
  *x = object->circle->center_x;
  *y = object->circle->center_y;
  return TRUE;
}

/*! \brief Print circle to Postscript document.
 *  \par Function Description
 *  This function prints the circle described by the <B>\a o_current</B>
 *  parameter to a Postscript document. It takes into account its line type
 *  and fill type.
 *  The Postscript document is descibed by the file pointer <B>fp</B>.
 *
 *  The validity of the <B>\a o_current</B> pointer is checked :
 *  a null pointer causes an error message and a return.
 *
 *  The description of the circle is extracted from the <B>\a o_current</B>
 *  parameter : the coordinates of the center of the circle, its radius,
 *  its line type, its fill type.
 *
 *  The outline and the inside of the circle are successively handled by
 *  two differend sets of functions.
 *
 *  \param [in] toplevel  The GedaToplevel object.
 *  \param [in] fp         FILE pointer to Postscript document.
 *  \param [in] o_current  Circle GedaObject to write to document.
 *  \param [in] origin_x   Page x coordinate to place circle Object.
 *  \param [in] origin_y   Page y coordinate to place circle Object.
 */
void o_circle_print(GedaToplevel *toplevel, FILE *fp, GedaObject *o_current,
                    int origin_x, int origin_y)
{
  int x, y, radius;
  int color;
  int circle_width, capstyle, length, space;
  void (*outl_func)() = NULL;

  g_return_if_fail(GEDA_IS_CIRCLE(o_current));

  x      = o_current->circle->center_x;
  y      = o_current->circle->center_y;
  radius = o_current->circle->radius;

  color  = o_current->color;
  capstyle = o_get_capstyle (o_current->line_options->line_end);

  /*
   * Depending on the type of the line for this particular circle, the
   * appropriate function is chosen among #o_circle_print_solid(),
   * #o_circle_print_dotted(),#o_circle_print_dashed(),
   * #o_circle_print_center() and#o_circle_print_phantom().
   *
   * The needed parameters for each of these type is extracted from the
   * <B>\a o_current</B> object. Depending on the type, unused parameters are
   * set to -1.
   *
   * In the eventuality of a length and/or space null, the line is
   * printed solid to avoid and endless loop produced by other functions
   * in such a case.
   */
  /* 09/08/12 | W.E.Hill Modified algorithms to incorperate both THICK & THIN
   *            styles, and eliminated hard-coded integer values.
   */

  circle_width = o_current->line_options->line_width;

  if(circle_width < MIN_LINE_WIDTH_THRESHOLD)
     circle_width = o_style_get_line_width(toplevel); /* 1st try updating style */

  if(circle_width < MIN_LINE_WIDTH_THRESHOLD)
     circle_width = MIN_LINE_WIDTH_THRESHOLD;         /* if STYLE_NONE  */

  length       = o_current->line_options->line_length;
  space        = o_current->line_options->line_space;

  switch(o_current->line_options->line_type) {
    case(TYPE_SOLID):
      length = -1; space  = -1;
      outl_func = o_circle_print_solid;
      break;

    case(TYPE_DOTTED):
      length = -1;
      outl_func = o_circle_print_dotted;
      break;

    case(TYPE_DASHED):
      outl_func = o_circle_print_dashed;
      break;

    case(TYPE_CENTER):
      outl_func = o_circle_print_center;
      break;

    case(TYPE_PHANTOM):
      outl_func = o_circle_print_phantom;
      break;

    case(TYPE_ERASE):
      /* Unused for now print it solid */
      length = -1; space  = -1;
      outl_func = o_circle_print_solid;
      break;
  }

  if((length == 0) || (space == 0)) {
    length = -1; space  = -1;
    outl_func = o_circle_print_solid;
  }

  (*outl_func)(toplevel, fp,
               x - origin_x, y - origin_y,
               radius,
               color,
               circle_width, capstyle, length, space,
               origin_x, origin_y);

  /*
   * If the filling type of the circle is not <B>HOLLOW</B>, the appropriate
   * function is chosen among #o_circle_print_filled(),# o_circle_print_mesh()
   * and #o_circle_print_hatch(). The corresponding parameters are extracted
   * from the <B>\a o_current</B> object and corrected afterward.
   *
   * The case where <B>pitch1</B> and <B>pitch2</B> are null or negative is
   * avoided as it leads to an endless loop in most of the called functions.
   * In such a case, the circle is printed filled. Unused parameters for
   * each of these functions are set to -1 or any passive value.
   */
  if(o_current->fill_options->fill_type != FILLING_HOLLOW) {

    void (*fill_func)();

    int fill_width, angle1, pitch1, angle2, pitch2;

    fill_width = o_current->fill_options->fill_width;
    angle1     = o_current->fill_options->fill_angle1;
    pitch1     = o_current->fill_options->fill_pitch1;
    angle2     = o_current->fill_options->fill_angle2;
    pitch2     = o_current->fill_options->fill_pitch2;

    switch(o_current->fill_options->fill_type) {
      case(FILL_SOLID):
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func = o_circle_print_filled;
        break;

      case(FILLING_MESH):
        fill_func = o_circle_print_mesh;
        break;

      case(FILLING_HATCH):
        angle2 = -1; pitch2 = 1;
        fill_func = o_circle_print_hatch;
        break;

      case(FILLING_VOID):          /* Unused for now, print it filled */
        angle1 = -1; pitch1 = 1;
        angle2 = -1; pitch2 = 1;
        fill_width = -1;
        fill_func = o_circle_print_filled;
        break;

      case(FILLING_HOLLOW):
      default:
        fill_func = NULL;
        break;
    }

    if((pitch1 <= 0) || (pitch2 <= 0)) {
      angle1 = -1; pitch1 = 1;
      angle2 = -1; pitch2 = 1;
      fill_func = o_circle_print_filled;
    }

    (*fill_func)(toplevel, fp,
                 x, y, radius,
                 color,
                 fill_width,
                 angle1, pitch1, angle2, pitch2,
                 origin_x, origin_y);
  }
}

/*! \brief Print a solid circle to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a circle when a solid line type
 *  is required. The circle is defined by its center in (<B>x</B>, <B>y</B>)
 *  and its radius in <B>radius</B>. It is printed with the color given
 *  in <B>color</B>.
 *  The parameters <B>length</B> and <B>space</B> are ignored.
 *
 *  It uses the function #o_arc_print_solid() to print the outline.
 *  Therefore it acts as an interface between the way a circle is defined
 *  and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel     The GedaToplevel object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x             Center x coordinate of circle.
 *  \param [in] y             Center y coordinate of circle.
 *  \param [in] radius        Circle radius.
 *  \param [in] color         Circle color.
 *  \param [in] circle_width  Width of circle.
 *  \param [in] capstyle      Capstyle of circle lines.
 *  \param [in] length        (unused).
 *  \param [in] space         (unused).
 *  \param [in] origin_x      Page x coordinate to place circle Object.
 *  \param [in] origin_y      Page y coordinate to place circle Object.
 */
void o_circle_print_solid(GedaToplevel *toplevel, FILE *fp,
                          int x, int y, int radius,
                          int color,
                          int circle_width, int capstyle, int length, int space,
                          int origin_x, int origin_y)
{
  o_arc_print_solid(toplevel, fp,
                    x, y, radius,
                    0, FULL_CIRCLE / 64,
                    color,
                    circle_width, BUTT_CAP, -1, -1,
                    origin_x, origin_y);

}

/*! \brief Print a dotted circle to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a circle when a dotted line
 *  type is required. The circle is defined by its center
 *  in (<B>x</B>, <B>y</B>) and its radius in <B>radius</B>. It is printed
 *  with the color given in <B>color</B>.
 *  The parameter <B>length</B> is ignored.
 *
 *  It uses the function #o_arc_print_dotted() to print the outline.
 *  Therefore it acts as an interface between the way a circle is
 *  defined and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel     The GedaToplevel object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x             Center x coordinate of circle.
 *  \param [in] y             Center y coordinate of circle.
 *  \param [in] radius        Circle radius.
 *  \param [in] color         Circle color.
 *  \param [in] circle_width  Width of circle.
 *  \param [in] capstyle      Capstyle of circle lines.
 *  \param [in] length        (unused).
 *  \param [in] space         Space between dots.
 *  \param [in] origin_x      Page x coordinate to place circle Object.
 *  \param [in] origin_y      Page y coordinate to place circle Object.
 */
void o_circle_print_dotted(GedaToplevel *toplevel, FILE *fp,
                           int x, int y, int radius,
                           int color,
                           int circle_width, int capstyle, int length, int space,
                           int origin_x, int origin_y)
{

  o_arc_print_dotted(toplevel, fp,
                     x, y, radius,
                     0, FULL_CIRCLE / 64,
                     color,
                     circle_width, capstyle, -1, space,
                     origin_x, origin_y);

}

/*! \brief Print a dashed circle to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a circle when a dashed line type
 *  is required. The circle is defined by its center in
 *  (<B>x</B>, <B>y</B>) and its radius in <B>radius</B>. It is printed with the
 *  color given in <B>color</B>.
 *
 *  It uses the function #o_arc_print_dashed() to print the outline.
 *  Therefore it acts as an interface between the way a circle is
 *  defined and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel     The GedaToplevel object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x             Center x coordinate of circle.
 *  \param [in] y             Center y coordinate of circle.
 *  \param [in] radius        Circle radius.
 *  \param [in] color         Circle color.
 *  \param [in] circle_width  Width of circle.
 *  \param [in] capstyle      Capstyle of circle lines.
 *  \param [in] length        Length of dashed lines.
 *  \param [in] space         Space between dashes.
 *  \param [in] origin_x      Page x coordinate to place circle Object.
 *  \param [in] origin_y      Page y coordinate to place circle Object.
 */
void o_circle_print_dashed(GedaToplevel *toplevel, FILE *fp,
                           int x, int y,
                           int radius,
                           int color,
                           int circle_width, int capstyle, int length, int space,
                           int origin_x, int origin_y)
{

  o_arc_print_dashed(toplevel, fp,
                     x, y, radius,
                     0, FULL_CIRCLE / 64,
                     color,
                     circle_width, capstyle, length, space,
                     origin_x, origin_y);

}

/*! \brief Print a centered line type circle to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a circle when a centered line
 *  type is required. The circle is defined by its center in
 *  (<B>x</B>, <B>y</B>) and its radius in <B>radius</B>. It is printed with the
 *  color given in <B>color</B>.
 *
 *  It uses the function #o_arc_print_center() to print the outline.
 *  Therefore it acts as an interface between the way a circle is
 *  defined and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel     The GedaToplevel object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x             Center x coordinate of circle.
 *  \param [in] y             Center y coordinate of circle.
 *  \param [in] radius        Circle radius.
 *  \param [in] color         Circle color.
 *  \param [in] circle_width  Width of circle.
 *  \param [in] capstyle      Capstyle of circle lines.
 *  \param [in] length        Length of dashed lines.
 *  \param [in] space         Space between dashes.
 *  \param [in] origin_x      Page x coordinate to place circle Object.
 *  \param [in] origin_y      Page y coordinate to place circle Object.
 */
void o_circle_print_center(GedaToplevel *toplevel, FILE *fp,
                           int x, int y,
                           int radius,
                           int color,
                           int circle_width, int capstyle, int length, int space,
                           int origin_x, int origin_y)
{

  o_arc_print_center(toplevel, fp,
                     x, y, radius,
                     0, FULL_CIRCLE / 64,
                     color,
                     circle_width, capstyle, length, space,
                     origin_x, origin_y);

}

/*! \brief Print a phantom line type circle to Postscript document.
 *
 *  \par Function Description
 *  This function prints the outline of a circle when a phantom line type
 *  is required. The circle is defined by its center in
 *  (<B>x</B>, <B>y</B>) and its radius in <B>radius</B>. It is printed with the
 *  color given in <B>color</B>.
 *
 *  It uses the function #o_arc_print_phantom() to print the outline.
 *  Therefore it acts as an interface between the way a circle is defined
 *  and the way an arc is defined.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel     The GedaToplevel object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x             Center x coordinate of circle.
 *  \param [in] y             Center y coordinate of circle.
 *  \param [in] radius        Circle radius.
 *  \param [in] color         Circle color.
 *  \param [in] circle_width  Width of circle.
 *  \param [in] capstyle      Capstyle of circle lines.
 *  \param [in] length        Length of dashed lines.
 *  \param [in] space         Space between dashes.
 *  \param [in] origin_x      Page x coordinate to place circle Object.
 *  \param [in] origin_y      Page y coordinate to place circle Object.
 */
void o_circle_print_phantom(GedaToplevel *toplevel, FILE *fp,
                            int x, int y, int radius, int color,
                            int circle_width, int capstyle, int length, int space,
                            int origin_x, int origin_y)
{

  o_arc_print_phantom(toplevel, fp,
                      x, y, radius,
                      0, FULL_CIRCLE / 64,
                      color,
                      circle_width, capstyle, length, space,
                      origin_x, origin_y);

}

/*! \brief Print a solid pattern circle to Postscript document
 *
 *  \par Function Description
 *  The function prints a filled circle with a solid pattern.
 *  No outline is printed.
 *  The circle is defined by the coordinates of its center in
 *  (<B>x</B>,<B>y</B>) and its radius given by the <B>radius</B> parameter.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B> and <B>pitch1</B>, <B>angle2</B>
 *  and <B>pitch2</B> parameters are ignored in this functions but
 *  kept for compatibility with other fill functions.
 *
 *  All dimensions are in mils (except <B>angle1</B> and <B>angle2</B> in degree).
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Center x coordinate of circle.
 *  \param [in] y           Center y coordinate of circle.
 *  \param [in] radius      Radius of circle.
 *  \param [in] color       Circle color.
 *  \param [in] fill_width  Circle fill width. (unused).
 *  \param [in] angle1      (unused).
 *  \param [in] pitch1      (unused).
 *  \param [in] angle2      (unused).
 *  \param [in] pitch2      (unused).
 *  \param [in] origin_x    Page x coordinate to place circle Object.
 *  \param [in] origin_y    Page y coordinate to place circle Object.
 */
void o_circle_print_filled(GedaToplevel *toplevel, FILE *fp,
              int x, int y, int radius,
              int color,
              int fill_width,
              int angle1, int pitch1,
              int angle2, int pitch2,
              int origin_x, int origin_y)
{
  f_print_set_color(toplevel, fp, color);

  fprintf(fp, "%d %d %d dot\n",
          x-origin_x, y-origin_y,
          radius);

}

/*! \brief Print a mesh pattern circle to Postscript document
 *
 *  \par Function Description
 *  This function prints a meshed circle. No outline is printed.
 *  The circle is defined by the coordinates of its center in
 *  (<B>x</B>,<B>y</B>) and its radius by the <B>radius</B> parameter.
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *
 *  The inside mesh is achieved by two successive call to the
 *  #o_circle_print_hatch() function, given <B>angle1</B> and <B>pitch1</B>
 *  the first time and <B>angle2</B> and <B>pitch2</B> the second time.
 *
 *  Negative or null values for <B>pitch1</B> and/or <B>pitch2</B> are
 *  not allowed as it leads to an endless loop in #o_circle_print_hatch().
 *
 *  All dimensions are in mils (except <B>angle1</B> and <B>angle2</B> in degree).
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Center x coordinate of circle.
 *  \param [in] y           Center y coordinate of circle.
 *  \param [in] radius      Radius of circle.
 *  \param [in] color       Circle color.
 *  \param [in] fill_width  Circle fill width.
 *  \param [in] angle1      1st angle for mesh pattern.
 *  \param [in] pitch1      1st pitch for mesh pattern.
 *  \param [in] angle2      2nd angle for mesh pattern.
 *  \param [in] pitch2      2nd pitch for mesh pattern.
 *  \param [in] origin_x    Page x coordinate to place circle Object.
 *  \param [in] origin_y    Page y coordinate to place circle Object.
 */
void o_circle_print_mesh(GedaToplevel *toplevel, FILE *fp,
                         int x, int y, int radius,
                         int color,
                         int fill_width,
                         int angle1, int pitch1,
                         int angle2, int pitch2,
                         int origin_x, int origin_y)
{
  o_circle_print_hatch(toplevel, fp,
                       x, y, radius,
                       color,
                       fill_width,
                       angle1, pitch1,
                       -1, -1,
                       origin_x, origin_y);
  o_circle_print_hatch(toplevel, fp,
                       x, y, radius,
                       color,
                       fill_width,
                       angle2, pitch2,
                       -1, -1,
                       origin_x, origin_y);

}

/*! \brief Print a hatch pattern circle to Postscript document.
 *  \par Function Description
 *  The function prints a hatched circle. No outline is printed.
 *  The circle is defined by the coordinates of its center in
 *  (<B>x</B>,<B>y</B>) and its radius by the <B>radius</B> parameter.
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *  <B>angle2</B> and <B>pitch2</B> parameters are ignored in this
 *  functions but kept for compatibility with other fill functions.
 *
 *  The only attribute of line here is its width from the parameter <B>width</B>.
 *
 *  Negative or null values for <B>pitch1</B> is not allowed as it
 *  leads to an endless loop.
 *
 *  All dimensions are in mils (except <B>angle1</B> is in degrees).
 *
 *  \param [in] toplevel   The GedaToplevel object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Center x coordinate of circle.
 *  \param [in] y           Center y coordinate of circle.
 *  \param [in] radius      Radius of circle.
 *  \param [in] color       Circle color.
 *  \param [in] fill_width  Circle fill width.
 *  \param [in] angle1      Angle for hatch pattern.
 *  \param [in] pitch1      Pitch for hatch pattern.
 *  \param [in] angle2      (unused).
 *  \param [in] pitch2      (unused).
 *  \param [in] origin_x    Page x coordinate to place circle Object.
 *  \param [in] origin_y    Page y coordinate to place circle Object.
 */
void o_circle_print_hatch(GedaToplevel *toplevel, FILE *fp,
                          int x, int y, int radius,
                          int color,
                          int fill_width,
                          int angle1, int pitch1,
                          int angle2, int pitch2,
                          int origin_x, int origin_y)
{
  Circle circle;
  int index;
  GArray *lines;

  g_return_if_fail(toplevel != NULL);
  g_return_if_fail(fp != NULL);

  f_print_set_color(toplevel, fp, color);

  /* Avoid printing line widths too small */
  if (fill_width <= 1) fill_width = 2;

  lines = g_array_new(FALSE, FALSE, sizeof(LINE));

  circle.center_x = x;
  circle.center_y = y;
  circle.radius   = radius;

  m_hatch_circle(&circle, angle1, pitch1, lines);

  for(index=0; index<lines->len; index++) {
    LINE *line = &g_array_index(lines, LINE, index);

    fprintf(fp,"%d %d %d %d %d %d line\n",
            line->x[0], line->y[0],
            line->x[1], line->y[1],
            fill_width, BUTT_CAP);
  }

  g_array_free(lines, TRUE);
}

/*! \brief Calculates shortest distance to a Circle
 *  \par Function Description
 *   Calculates the distance between the given point and the closest
 *   point on the perimeter of the circle.
 *
 *  \param [in] object       A circle Object.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *
 *  \return The shortest distance from point to \a object to the point or
 *          G_MAXDOUBLE if the parameters are invalid parameter.
 */
double o_circle_shortest_distance (GedaObject *object, int x, int y, int force_solid)
{
  int solid;

  g_return_val_if_fail (GEDA_IS_CIRCLE(object), G_MAXDOUBLE);

  solid = force_solid || object->fill_options->fill_type != FILLING_HOLLOW;

  return m_circle_shortest_distance (object->circle, x, y, solid);
}

