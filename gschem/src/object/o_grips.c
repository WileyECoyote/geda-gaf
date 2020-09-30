/* -*- C o_grips.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * \file o_grips.c
 * \brief Low-level module for Grip operations
 */

#include <math.h>

#include <gschem.h>
#include <geda_debug.h>

/*!
 * \brief Cancel process of modifying object with grip.
 * \par Function Description
 *  Helper function when canceling the process of modifying an object using
 *  a grip, which could occur if the user hits the ESC key before releasing
 *  the grip. This function resets the dont_redraw flag on the object which
 *  was being modified and invalidates the reference to the object and top-
 *  level which_grip variable.
 *
 * \param [in,out] w_current  The GschemToplevel object.
 */
void
o_grips_cancel(GschemToplevel *w_current)
{
  /* If object set then switch drawing of the object back on */
  if (w_current->which_object != NULL) {
    if (GEDA_IS_OBJECT(w_current->which_object)) {
      w_current->which_object->dont_redraw = FALSE;
    }
    w_current->which_object = NULL;
  }

  /* reset global variables */
  w_current->which_grip     = -1;
  w_current->rubber_visible = FALSE;
}

/*!
 * \brief Draw objects being grip maniuplated from GschemToplevel object.
 * \par Function Description
 *  This function draws the objects being grip manipulated.
 *
 * \param [in] w_current  The GschemToplevel object.
 */
void
o_grips_draw_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current->which_object != NULL);

  switch(w_current->which_object->type) {
    case OBJ_ARC:
      o_arc_draw_rubber (w_current);
      break;

    case OBJ_BOX:
      o_box_draw_rubber (w_current);
      break;

    case OBJ_PATH:
      o_path_draw_rubber_grips (w_current);
      break;

    case OBJ_PICTURE:
      o_picture_draw_rubber (w_current);
      break;

    case OBJ_CIRCLE:
      o_circle_draw_rubber (w_current);
      break;

    case OBJ_LINE:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:
      o_line_draw_rubber (w_current);
    break;

    default:
    return; /* error condition */
  }
}

#if 0
/*!
 * \brief Get half the width and height of grip in screen units.
 * \par Function Description
 *  This function returns a value half the size of the grip size
 *  in screen units, based on the objects width and the zoom factor.
 *  Between the threashold and 0, grippable object with non-zero
 *  width, the size returned is the greater of width - factor and
 *  <b>GRIP_SIZE_ZOOM1</b>. The value returned for "grippables" with
 *  zero width is GRIP_SIZE_ZOOM1. If object is NULL then have of
 *  #MAX_GRIP_SIZE is returned.
 *
 * \param [in] w_current  The GschemToplevel object.
 *
 * \return Half grip size in screen units.
 */
int
o_grips_compute_drawn_size (GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;

  int ret_size;
  int factor;

  factor   = (int)toplevel->page_current->to_world_x_constant;
  ret_size = gschem_toplevel_get_grips_size(w_current);

  if (factor < GRIP_ZOOM_THREASHOLD_1) {

     ret_size = ret_size * factor;

  }

  return ret_size >> 1;
}
#endif

/*!
 * \brief Check if pointer is inside the grip region.
 * \par Function Description
 *  This function checks if the point (<b>x</b>,<b>y</b>) is
 *  inside the grip centered at (<b>grip_x</b>,<b>grip_y</b>).
 *
 * \param [in]  x       Current x coordinate of pointer in world units.
 * \param [in]  y       Current y coordinate of pointer in world units.
 * \param [in]  grip_x  Current x coordinate of grip center in world units.
 * \param [in]  grip_y  Current y coordinate of grip center in world units.
 * \param [in]  size    Half the width of the grip square in world units.
 *
 * \return True / False whether the mouse pointer is inside the grip.
 */
static bool
o_grips_inside_grip( int x, int y, int grip_x, int grip_y, int size )
{
  int xmin, ymin, xmax, ymax;

  register int half_size = size;

  xmin = grip_x - half_size;
  xmax = grip_x + half_size;

  ymin = grip_y - half_size;
  ymax = grip_y + half_size;

  return geda_object_get_is_inside_region(xmin, ymin, xmax, ymax, x, y);
}

/*!
 * \brief Modify previously selected object according to mouse position.
 * \par Function Description
 *  This function modify the previously selected
 *  object according to the mouse position in <b>w_x</b> and <b>w_y</b>.
 *  The grip under modification is updated and the temporary object displayed.
 *
 *  The object under modification is <b>w_current->which_object</b> and
 *  the grip concerned is <b>w_current->which_grip</b>.
 *
 *  Depending on the object type, a specific function is used.
 *  It erases the temporary object, updates its internal representation,
 *  and draws it again.
 *
 * \param [in] w_current  The GschemToplevel object.
 * \param [in] w_x        Current x coordinate of pointer in world units.
 * \param [in] w_y        Current y coordinate of pointer in world units.
 */
void
o_grips_motion(GschemToplevel *w_current, int w_x, int w_y)
{
  if (w_current->inside_action == 0) {
    BUG_MSG("Not inside action");
  }
  else if (w_current->which_object == NULL) {
    BUG_MSG("which_object == NULL");
  }
  else {
    switch(w_current->which_object->type) {
      case OBJ_ARC:
        o_arc_motion (w_current, w_x, w_y);
        break;

      case OBJ_BOX:
        o_box_motion (w_current, w_x, w_y);
        break;

      case OBJ_PATH:
        o_path_motion_grips (w_current, w_x, w_y);
        break;

      case OBJ_PICTURE:
        o_picture_motion (w_current, w_x, w_y);
        break;

      case OBJ_CIRCLE:
        o_circle_motion (w_current, w_x, w_y);
        break;

      case OBJ_LINE:
      case OBJ_NET:
      case OBJ_PIN:
      case OBJ_BUS:
        o_line_motion (w_current, w_x, w_y);
        break;

      default:
        break; /* error condition */
    }
  }
}

/*!
 * \brief Check if pointer is inside arc grip.
 * \par Function Description
 *  This function checks if the pointer event occuring at (<b>x</b>,<b>y</b>) is
 *  inside one of the grips of an <b>\a o_current</b> pointed arc object. If so
 *  the <b>whichone</b> pointed integer is set to the number of this grip and
 *  the return pointer is a pointer on this object. If the point is not
 *  inside a grip the function returns a NULL pointer and the <b>whichone</b>
 *  pointed integer is unset.
 *
 *  An arc object has three grips :
 *  <DL>
 *    <DT>*</DT><DD>one at the center of the arc. This grip is used to modify
 *                  the radius of the arc. If this one is selected, the
 *                  <b>whichone</b> pointed integer is set to <b>ARC_RADIUS</b>.
 *    <DT>*</DT><DD>one at one end of the arc. It corresponds to the starting
 *                  angle of the arc. If this one is selected, the
 *                  <b>whichone</b> pointed integer is set to <b>ARC_START_ANGLE</b>.
 *    <DT>*</DT><DD>one at the other end of the arc. It corresponds to the
 *                  ending angle of the arc. If this one is selected, the
 *                  <b>whichone</b> pointed integer is set to <b>ARC_END_ANGLE</b>.
 *  </DL>
 *
 *  The <b>x</b> and <b>y</b> parameters are in world units.
 *
 *  The <b>size</b> parameter is the width (and height) of the square
 *  representing a grip in world units.
 *
 * \param [in]  w_current  The GschemToplevel object.
 * \param [in]  o_current  Arc Object to check.
 * \param [in]  x          Current x coordinate of pointer in world units.
 * \param [in]  y          Current y coordinate of pointer in world units.
 * \param [in]  size       Half the width of the grip square in world units.
 * \param [out] whichone   Which grip point is selected.
 *
 * \return Pointer to Object the grip is on, NULL otherwise.
 */
GedaObject*
o_grips_search_arc_world(GschemToplevel *w_current, GedaObject *o_current,
                                   int x, int y, int size, int *whichone)
{
  int centerx, centery, radius, start_angle, arc_sweep;
  double tmp;

  centerx     = o_current->arc->x;
  centery     = o_current->arc->y;
  radius      = o_current->arc->radius;
  start_angle = o_current->arc->start_angle;
  arc_sweep   = o_current->arc->arc_sweep;

  /* check the grip on the center of the arc */
  if (o_grips_inside_grip(x, y, centerx, centery, size)) {
    *whichone = ARC_RADIUS;
    return(o_current);
  }

  /* check the grip at the end angle of the arc */
  tmp = ((double) start_angle + arc_sweep) * M_PI / 180;
  if (o_grips_inside_grip(x, y,
                  centerx + radius * cos(tmp),
                  centery + radius * sin(tmp), size)) {
    *whichone = ARC_END_ANGLE;
    return(o_current);
  }

  /* check the grip at the start angle of the arc */
  tmp = ((double) start_angle) * M_PI / 180;
  if (o_grips_inside_grip(x, y,
                  centerx + radius * cos(tmp),
                  centery + radius * sin(tmp), size)) {
    *whichone = ARC_START_ANGLE;
    return(o_current);
  }

  return NULL;
}

/*!
 * \brief Check if pointer is inside box grip.
 * \par Function Description
 *  This function checks if the pointer event occuring at (<b>x</b>,<b>y</b>) is
 *  inside one of the grips of the <b>\a o_current</b> pointed box object.
 *  If so, the <b>whichone</b> pointed integer is set to the identifier of
 *  this grip and the returned pointer is a pointer on this object.
 *  If the point is not inside a grip the function returns a NULL pointer
 *  and the <b>whichone</b> pointed integer is unset.
 *
 *  A box object has four grips : one at each corner of the box. The
 *  identifiers of each corner are <b>BOX_UPPER_LEFT</b>,
 *  <b>BOX_UPPER_RIGHT</b>, <b>BOX_LOWER_LEFT</b> and <b>BOX_LOWER_RIGHT</b>.
 *
 *  The <b>x</b> and <b>y</b> parameters are in world units.
 *
 *  The <b>size</b> parameter is half the width (and half the height) of
 *  the square representing a grip in world units.
 *
 * \param [in]  w_current  The GschemToplevel object.
 * \param [in]  o_current  Box Object to check.
 * \param [in]  x          Current x coordinate of pointer in world units.
 * \param [in]  y          Current y coordinate of pointer in world units.
 * \param [in]  size       Half the width of the grip square in world units.
 * \param [out] whichone   Which grip point is selected.
 *
 * \return Pointer to Object the grip is on, NULL otherwise.
 */
GedaObject*
o_grips_search_box_world(GschemToplevel *w_current, GedaObject *o_current,
                                int x, int y, int size, int *whichone)
{
  /* inside upper left grip ? */
  if (o_grips_inside_grip(x, y,
                  o_current->box->upper_x,
                  o_current->box->upper_y, size)) {
    *whichone = BOX_UPPER_LEFT;
    return(o_current);
  }

  /* inside lower right grip ? */
  if (o_grips_inside_grip(x, y,
                  o_current->box->lower_x,
                  o_current->box->lower_y, size)) {
    *whichone = BOX_LOWER_RIGHT;
    return(o_current);
  }

  /* inside upper right grip ? */
  if (o_grips_inside_grip(x, y,
                  o_current->box->lower_x,
                  o_current->box->upper_y, size)) {
    *whichone = BOX_UPPER_RIGHT;
    return(o_current);
  }

  /* inside lower left grip ? */
  if (o_grips_inside_grip(x, y,
                  o_current->box->upper_x,
                  o_current->box->lower_y, size)) {
    *whichone = BOX_LOWER_LEFT;
    return(o_current);
  }

  return NULL;
}

/*! \brief Check if pointer is inside path grip.
 *  \par Function Description
 *  This function checks if the pointer event occuring at (<b>x</b>,<b>y</b>)
 *  is inside one of the grips of the <b>\a o_current</b> pointed path object.
 *  If so, the <b>whichone</b> pointed integer is set to the identifier of
 *  this grip and the returned pointer is a pointer on this object.
 *  If the point is not inside a grip the function returns a NULL pointer
 *  and the <b>whichone</b> pointed integer is unset.
 *
 *  A path object has four grips : one at each corner of the path.
 *  The identifiers of each corner are \a PICTURE_UPPER_LEFT,
 *  \a PICTURE_UPPER_RIGHT, \a PICTURE_LOWER_LEFT and
 *  \a PICTURE_LOWER_RIGHT.
 *
 *  The <b>x</b> and <b>y</b> parameters are in world units.
 *
 *  The <b>size</b> parameter is half the width (and half the height) of the
 *  square representing a grip in world units.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Picture Object to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *
 *  \return Pointer to Object the grip is on, NULL otherwise.
 */
GedaObject*
o_grips_search_path_world(GschemToplevel *w_current, GedaObject *o_current,
                                 int x, int y, int size, int *whichone)
{
  int i;
  int grip_no = 0;

  for (i = 0; i <  o_current->path->num_sections; i++) {

    PATH_SECTION *section = &o_current->path->sections[i];

    switch (section->code) {
    case PATH_CURVETO:
      /* inside first control grip ? */
      if (o_grips_inside_grip(x, y, section->x1, section->y1, size)) {
        *whichone = grip_no;
        return o_current;
      }
      grip_no ++;
      /* inside second control grip ? */
      if (o_grips_inside_grip(x, y, section->x2, section->y2, size)) {
        *whichone = grip_no;
        return o_current;
      }
      grip_no ++;
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* inside destination control grip ? */
      if (o_grips_inside_grip(x, y, section->x3, section->y3, size)) {
        *whichone = grip_no;
        return o_current;
      }
      grip_no ++;
      break;
    case PATH_END:
      break;
    }
  }

  return NULL;
}

/*! \brief Check if pointer is inside picture grip.
 *  \par Function Description
 *  This function checks if the pointer event occuring at (<b>x</b>,<b>y</b>)
 *  is inside one of the grips of the <b>\a o_current</b> pointed picture object.
 *  If so, the <b>whichone</b> pointed integer is set to the identifier of
 *  this grip and the returned pointer is a pointer on this object.
 *  If the point is not inside a grip the function returns a NULL pointer
 *  and the <b>whichone</b> pointed integer is unset.
 *
 *  A picture object has four grips : one at each corner of the picture.
 *  The identifiers of each corner are \a PICTURE_UPPER_LEFT,
 *  \a PICTURE_UPPER_RIGHT, \a PICTURE_LOWER_LEFT and
 *  \a PICTURE_LOWER_RIGHT.
 *
 *  The <b>x</b> and <b>y</b> parameters are in world units.
 *
 *  The <b>size</b> parameter is half the width (and half the height) of the
 *  square representing a grip in world units.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Picture Object to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to Object the grip is on, NULL otherwise.
 */
GedaObject*
o_grips_search_picture_world(GschemToplevel *w_current, GedaObject *o_current,
                                    int x, int y, int size, int *whichone)
{
  /* inside upper left grip ? */
  if (o_grips_inside_grip(x, y,
                  o_current->picture->upper_x,
                  o_current->picture->upper_y, size)) {
    *whichone = PICTURE_UPPER_LEFT;
    return(o_current);
  }

  /* inside lower right grip ? */
  if (o_grips_inside_grip(x, y,
                  o_current->picture->lower_x,
                  o_current->picture->lower_y, size)) {
    *whichone = PICTURE_LOWER_RIGHT;
    return(o_current);
  }

  /* inside upper right grip ? */
  if (o_grips_inside_grip(x, y,
                  o_current->picture->lower_x,
                  o_current->picture->upper_y, size)) {
    *whichone = PICTURE_UPPER_RIGHT;
    return(o_current);
  }

  /* inside lower left grip ? */
  if (o_grips_inside_grip(x, y,
                  o_current->picture->upper_x,
                  o_current->picture->lower_y, size)) {
    *whichone = PICTURE_LOWER_LEFT;
    return(o_current);
  }

  return NULL;
}

/*! \brief Check if pointer is inside circle grip.
 *  \par Function Description
 *  This function determines if the (<b>x</b>,<b>y</b>) point is inside one of
 *  the grip of the circle object <b>\a o_current</b>.
 *  It computes the area covered by each grip and check if (<b>x</b>,<b>y</b>)
 *  is in one of these areas.
 *  If the event occurred in one of the grip, a pointer on the object is
 *  returned and <b>*whichone</b> is set to the identifier of the grip.
 *  If not, the function returns a %NULL pointer and <b>*whichone</b>
 *  is unchanged.
 *
 *  The parameter <b>size</b> is half the size of the grip in world units.
 *
 *  A circle has four grips. Moving the middle-right grip changes the radius
 *  of the circle. The identifier of this grip is <b>CIRCLE_RADIUS</b>.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Circle Object to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *  \return Pointer to Object the grip is on, NULL otherwise.
 */
GedaObject*
o_grips_search_circle_world(GschemToplevel *w_current, GedaObject *o_current,
                                   int x, int y, int size, int *whichone)
{
  /* check the grip for radius */
  if (o_grips_inside_grip(x, y,
                  o_current->circle->center_x + o_current->circle->radius,
                  o_current->circle->center_y,
                  size)) {
    *whichone = CIRCLE_RADIUS;
    return(o_current);
  }

  return NULL;
}

GedaObject*
o_grips_search_complex_world(GschemToplevel *w_current, GedaObject *o_current,
                                   int x, int y, int size, int *whichone)
{
  /* Check the grip for insertion point */
  if (o_grips_inside_grip(x, y, o_current->complex->x,
                                o_current->complex->y, size)) {
    *whichone = COMPLEX_INSERTION;
    return(o_current);
  }

  return NULL;
}

/*! \brief Check if pointer is inside line grip.
 *  \par Function Description
 *  This function determines if the (<b>\a x</b>,<b>\a y</b>) point given by \a x
 *  \a y is inside one of the grips of the #Line object <b>\a o_current</b>,
 *  by computing the area covered by each grip and then checks if (<b>x</b>,
 *  <b>y</b>) is in one of these regions.
 *  If the event occurred in one of its grip, a pointer on the object is
 *  returned and <b>\a whichone</b> is set to the identifier of the grip. If
 *  not, the function returns %NULL pointer and <b>\a whichone</b> is unchanged.
 *
 *  The parameter <b>\a size</b> is half the size of the grip in world units.
 *
 *  \param [in]  w_current  The #GschemToplevel object.
 *  \param [in]  o_current  A #Line Object to check.
 *  \param [in]  x          Current x coordinate of pointer in world units.
 *  \param [in]  y          Current y coordinate of pointer in world units.
 *  \param [in]  size       Half the width of the grip square in world units.
 *  \param [out] whichone   Which grip point is selected.
 *
 *  \return Pointer to Object the grip is on, NULL otherwise.
 */
GedaObject*
o_grips_search_line_world(GschemToplevel *w_current, GedaObject *o_current,
                                 int x, int y, int size, int *whichone)
{
  /* check the grip on the end of line 1 */
  if (o_grips_inside_grip(x, y,
                  o_current->line->x[LINE_END1],
                  o_current->line->y[LINE_END1], size)) {
    *whichone = LINE_END1;
    return(o_current);
  }

  /* check the grip on the end of line 2 */
  if (o_grips_inside_grip(x, y,
                  o_current->line->x[LINE_END2],
                  o_current->line->y[LINE_END2], size)) {
    *whichone = LINE_END2;
    return(o_current);
  }

  return NULL;
}

/*! \brief Initialize grip motion process for an arc.
 *  \par Function Description
 *  This function initializes the grip motion process for an arc.
 *  From the <b>\a o_current</b> pointed object, it stores into the
 *  GschemToplevel structure the coordinates of the center, the radius
 *  and the two angle that describes an arc. These variables are used in
 *  the grip process.
 *
 *  The coordinates of the center of the arc on x- and y-axis are stored
 *  into the <b>first_wx</b> and <b>first_wy</b> fields of the GschemToplevel
 *  structure in screen units.
 *
 *  The radius of the center is stored into the <b>distance</b> field of
 *  the GschemToplevel structure in screen units.
 *
 *  The two angles describing the arc on a circle are stored into the
 *  <b>second_wx</b> for the starting angle and <b>second_wy</b> for the
 *  ending angle. These angles are expressed in degrees.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Arc Object to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 */
static void
o_grips_start_arc(GschemToplevel *w_current, GedaObject *o_current, int x, int y)
{
  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* describe the arc with GschemToplevel variables */
  /* center */
  w_current->first_wx  = o_current->arc->x;
  w_current->first_wy  = o_current->arc->y;
  /* radius */
  w_current->distance  = o_current->arc->radius;
  /* angles */
  w_current->second_wx = o_current->arc->start_angle;
  w_current->second_wy = o_current->arc->arc_sweep;

  /* draw the first temporary arc */
  w_current->rubber_visible = 1;
}

/*! \brief Initialize grip motion process for a box.
 *  \par Function Description
 *  This function initializes the grip motion process for a box. From the
 *  <b>\a o_current</b> pointed object, it stores into the GschemToplevel
 *  structure the .... These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the selected corner are put in
 *  (<b>w_current->second_wx</b>,<b>w_current->second_wx</b>).
 *
 *  The coordinates of the opposite corner go in
 *  (<b>w_current->first_wx</b>,<b>w_current->first_wy</b>). They are not suppose
 *  to change during the action.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Box Object to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 */
static void
o_grips_start_box(GschemToplevel *w_current, GedaObject *o_current, int x, int y)
{
  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* (second_wx, second_wy) is the selected corner */
  /* (first_wx, first_wy) is the opposite corner */
  switch(w_current->which_grip) {
    case BOX_UPPER_LEFT:
      w_current->second_wx = o_current->box->upper_x;
      w_current->second_wy = o_current->box->upper_y;
      w_current->first_wx  = o_current->box->lower_x;
      w_current->first_wy  = o_current->box->lower_y;
      break;
    case BOX_LOWER_RIGHT:
      w_current->second_wx = o_current->box->lower_x;
      w_current->second_wy = o_current->box->lower_y;
      w_current->first_wx  = o_current->box->upper_x;
      w_current->first_wy  = o_current->box->upper_y;
      break;
    case BOX_UPPER_RIGHT:
      w_current->second_wx = o_current->box->lower_x;
      w_current->second_wy = o_current->box->upper_y;
      w_current->first_wx  = o_current->box->upper_x;
      w_current->first_wy  = o_current->box->lower_y;
      break;
    case BOX_LOWER_LEFT:
      w_current->second_wx = o_current->box->upper_x;
      w_current->second_wy = o_current->box->lower_y;
      w_current->first_wx  = o_current->box->lower_x;
      w_current->first_wy  = o_current->box->upper_y;
      break;
    default:
      return; /* error */
  }

  /* draw the first temporary box */
  w_current->rubber_visible = 1;
}

/*!
 * \brief Check if point is inside grip.
 * \par Function Description
 *  This function is used to determine if the (<b>x</b>,<b>y</b>) point is
 *  inside a grip of one of the selected object on the current sheet.
 *  The selected object are in a list starting at
 *  <b>w_current->toplevel->page_current->selection2_head</b>.
 *  The <b>x</b> and <b>y</b> parameters are in world units.
 *  If the point is inside one grip, a pointer on the object it belongs to is
 *  returned and <b>*whichone</b> is set according to the position of the grip
 *  on the object.
 *  Else, <b>*whichone</b> is unchanged and the function returns %NULL.
 *
 *  A specific search function is provided for every kind of graphical object.
 *  The list of selected object is covered : each object is tested with the
 *  appropriate function.
 *
 * \param [in]  w_current  The GschemToplevel object.
 * \param [in]  x          Current x coordinate of pointer in world units.
 * \param [in]  y          Current y coordinate of pointer in world units.
 * \param [out] whichone   Which grip point is selected.
 *
 * \return Pointer to Object the grip is on, NULL otherwise.
 */
GedaObject*
o_grips_search_world(GschemToplevel *w_current, int x, int y, int *whichone)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GedaObject   *found    = NULL;
  GList        *s_current;

  int size;
  int w_size;

  if (!whichone) {
    return(NULL);
  }

  size   = gschem_toplevel_get_grips_half_size(w_current);
  w_size = (int)size * toplevel->page_current->to_world_x_constant + 3.5;

  s_current = geda_list_get_glist (toplevel->page_current->selection_list);

  while (s_current != NULL) {

    GedaObject *object = (GedaObject*)s_current->data;

    if (object) {

      switch(object->type) {
        case(OBJ_ARC):
          /* check the grips of the arc object */
          found = o_grips_search_arc_world(w_current, object,
                                           x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_BOX):
          /* check the grips of the box object */
          found = o_grips_search_box_world(w_current, object,
                                           x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_PATH):
          /* check the grips of the path object */
          found = o_grips_search_path_world(w_current, object,
                                            x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_PICTURE):
          /* check the grips of the picture object */
          found = o_grips_search_picture_world(w_current, object,
                                               x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_CIRCLE):
          /* check the grips of the circle object */
          found = o_grips_search_circle_world(w_current, object,
                                              x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_COMPLEX):
          /* check the grips of the complex object */
          found = o_grips_search_complex_world(w_current, object,
                                              x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        case(OBJ_LINE):
        case(OBJ_PIN):
        case(OBJ_NET):
        case(OBJ_BUS):
          /* check the grips of the line object */
          /* the function is the same for line, pin, net, bus */
          found = o_grips_search_line_world(w_current, object,
                                            x, y, w_size, whichone);
          if(found != NULL) return found;
          break;

        default:
          break;
      }
    }
    s_current = g_list_next(s_current);
  }

  return(NULL);
}

/*! \brief Initialize grip motion process for a path.
 *  \par Function Description
 *  This function initializes the grip motion process for a path.
 *  From the <b>\a o_current</b> pointed object, it stores into the
 *  GschemToplevel structure the ....
 *  These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the selected corner are put in
 *  (<b>w_current->second_wx</b>,<b>w_current->second_wy</b>).
 *
 *  The coordinates of the opposite corner go in
 *  (<b>w_current->first_wx</b>,<b>w_current->first_wy</b>). They are not
 *  suppose to change during the action.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Picture Object to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 */
static void
o_grips_start_path(GschemToplevel *w_current, GedaObject *o_current, int x, int y)
{
  PATH_SECTION *section;
  int i;
  int grip_no  = 0;
  int gx       = -1;
  int gy       = -1;
  int whichone = w_current->which_grip;

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  for (i = 0; i <  o_current->path->num_sections; i++) {
    section = &o_current->path->sections[i];

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      if (whichone == grip_no++) {
        gx = section->x1;
        gy = section->y1;
      }
      if (whichone == grip_no++) {
        gx = section->x2;
        gy = section->y2;
      }
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* Destination point grip */
      if (whichone == grip_no++) {
        gx = section->x3;
        gy = section->y3;
      }
      break;
    case PATH_END:
      break;
    }
  }

  w_current->first_wx = w_current->second_wx = gx;
  w_current->first_wy = w_current->second_wy = gy;

  /* draw the first temporary path */
  w_current->rubber_visible = 1;

}

/*! \brief Initialize grip motion process for a picture.
 *  \par Function Description
 *  This function initializes the grip motion process for a picture.
 *  From the <b>\a o_current</b> pointed object, it stores into the
 *  GschemToplevel structure the ....
 *  These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the selected corner are put in
 *  (<b>w_current->second_wx</b>,<b>w_current->second_wy</b>).
 *
 *  The coordinates of the opposite corner go in
 *  (<b>w_current->first_wx</b>,<b>w_current->first_wy</b>). They are not
 *  suppose to change during the action.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Picture Object to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 */
static void
o_grips_start_picture(GschemToplevel *w_current, GedaObject *o_current, int x, int y)
{
  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;
  w_current->current_pixbuf  = geda_picture_object_get_pixbuf (o_current);
  w_current->pixbuf_filename = geda_strdup (geda_picture_object_get_filename (o_current));

  w_current->pixbuf_wh_ratio = geda_picture_object_get_effective_ratio (o_current);

  /* (second_wx,second_wy) is the selected corner */
  /* (first_wx, first_wy) is the opposite corner */

  switch (w_current->which_grip) {
    case PICTURE_UPPER_LEFT:
      w_current->second_wx = o_current->picture->upper_x;
      w_current->second_wy = o_current->picture->upper_y;
      w_current->first_wx  = o_current->picture->lower_x;
      w_current->first_wy  = o_current->picture->lower_y;
      break;
    case PICTURE_LOWER_RIGHT:
      w_current->second_wx = o_current->picture->lower_x;
      w_current->second_wy = o_current->picture->lower_y;
      w_current->first_wx  = o_current->picture->upper_x;
      w_current->first_wy  = o_current->picture->upper_y;
      break;
    case PICTURE_UPPER_RIGHT:
      w_current->second_wx = o_current->picture->lower_x;
      w_current->second_wy = o_current->picture->upper_y;
      w_current->first_wx  = o_current->picture->upper_x;
      w_current->first_wy  = o_current->picture->lower_y;
      break;
    case PICTURE_LOWER_LEFT:
      w_current->second_wx = o_current->picture->upper_x;
      w_current->second_wy = o_current->picture->lower_y;
      w_current->first_wx  = o_current->picture->lower_x;
      w_current->first_wy  = o_current->picture->upper_y;
      break;
    default:
      return; /* error */
  }

#if DEBUG
fprintf(stderr, "\tfirst_wx  %d, first_wy  %d ", w_current->first_wx,  w_current->first_wy);
fprintf(stderr, "second_wx %d, second_wy %d\n", w_current->second_wx,  w_current->second_wy);
#endif

  /* draw the first temporary picture */
  o_picture_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Initialize grip motion process for a circle.
 *  \par Function Description
 *  This function initializes the grip motion process for a circle.
 *  From the <b>\a o_current</b> pointed object, it stores into the
 *  GschemToplevel structure the coordinate of the center and the radius.
 *  These variables are used in the grip process.
 *
 *  The function first erases the grips.
 *
 *  The coordinates of the center are put in
 *  (<b>w_current->first_wx</b>,<b>w_current->first_wy</b>). They are not suppose
 *  to change during the action.
 *
 *  The radius of the circle is stored in <b>w_current->distance</b>.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Circle Object to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 */
static void
o_grips_start_circle(GschemToplevel *w_current, GedaObject *o_current, int x, int y)
{

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* store circle center and radius in GschemToplevel structure */
  w_current->first_wx = o_current->circle->center_x;
  w_current->first_wy = o_current->circle->center_y;
  w_current->distance = o_current->circle->radius;

  /* draw the first temporary circle */
  w_current->rubber_visible = 1;
}

static void
o_grips_start_complex(GschemToplevel *w_current, GedaObject *o_current, int x, int y)
{

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* store circle center and radius in GschemToplevel structure */
  w_current->first_wx = geda_complex_get_x(o_current->complex);
  w_current->first_wy = geda_complex_get_y(o_current->complex);

  /* draw the first temporary complex */
  w_current->rubber_visible = 1;
}

/*! \brief Initialize grip motion process for a line.
 *  This function starts the move of one of the two grips of the
 *  line object <b>\a o_current</b>.
 *
 *  During the move of the grip, the line is described by
 *  (<b>w_current->first_wx</b>,<b>w_current->first_wy</b>) and
 *  (<b>w_current->second_wx</b>,<b>w_current->second_wy</b>).
 *
 *  The line end that corresponds to the moving grip is in
 *  (<b>w_current->second_wx</b>,<b>w_current->second_wy</b>).
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  o_current  Line Object to check.
 *  \param [in]  x          (unused)
 *  \param [in]  y          (unused)
 */
static void
o_grips_start_line(GschemToplevel *w_current, GedaObject *o_current, int x, int y)
{
  i_status_action_start(w_current);

  int whichone = w_current->which_grip;

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* describe the line with GschemToplevel variables */
  w_current->second_wx = o_current->line->x[whichone];
  w_current->second_wy = o_current->line->y[whichone];
  w_current->first_wx  = o_current->line->x[!whichone];
  w_current->first_wy  = o_current->line->y[!whichone];

  /* draw the first temporary line */
  w_current->rubber_visible = TRUE;

}

/*! \brief Start process of modifiying one grip.
 *  \par Function Description
 *  This function starts the process of modifying one grip of an object
 *  on the current sheet. The event occurred at (<b>w_x</b>,<b>w_y</b>) in
 *  world unit. If this position is related to a grip of an object, the
 *  function prepares the modification of this grip thanks to the user input.
 *
 *  The function returns <b>FALSE</b> if an error occurred or if no grip
 *  have been found under (<b>w_x</b>,<b>w_y</b>). It returns <b>TRUE</b> if a grip
 *  has been found and modification of the object has been started.
 *
 *  If a grip has been found, this function modifies the GschemToplevel
 *  variables <b>which_grip</b> and <b>which_object</b> with the identifier
 *  of the grip and the object it belongs to respectively.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  w_x        Current x coordinate of pointer in world units.
 *  \param [in]  w_y        Current y coordinate of pointer in world units.
 *
 *  \return FALSE if an error occurred or no grip was found, TRUE otherwise.
 */
bool
o_grips_start(GschemToplevel *w_current, int w_x, int w_y)
{
  bool result;

  if (eda_renderer_get_draw_grips(CairoRenderer)) {

    GedaObject *object;
    int whichone;

    /* search if there is a grip on a selected object at (w_x,w_y) */
    object = o_grips_search_world(w_current, w_x, w_y, &whichone);

    if (object != NULL) {

      void (*func) (GschemToplevel*, GedaObject*, int, int);

      w_current->which_grip   = whichone;
      w_current->which_object = object;

      /* Switch off drawing for the object being modified */
      object->dont_redraw = TRUE;
      o_invalidate_object (w_current, object);

      /* there is one */
      /* depending on its type, start the modification process */
      switch(object->type) {

        case(OBJ_ARC):
          func =  o_grips_start_arc;
          break;

        case(OBJ_BOX):
          func =  o_grips_start_box;
          break;

        case(OBJ_PATH):
          func = o_grips_start_path;
          break;

        case(OBJ_PICTURE):
          func = o_grips_start_picture;
          break;

        case(OBJ_CIRCLE):
          func = o_grips_start_circle;
          break;

        case(OBJ_COMPLEX):
          func = o_grips_start_complex;
          break;

        case(OBJ_LINE):
        case(OBJ_NET):
        case(OBJ_PIN):
        case(OBJ_BUS):
          func = o_grips_start_line; /* identical for line/net/pin/bus */
          break;

        default:
          /* object type unknown : error condition */
          func = NULL;
          result = FALSE;
          break;
      }

      if (func) {
        /* start the modification of a grip the object */
        (*func) (w_current, object, w_x, w_y);
        i_status_set_state (w_current, w_current->event_state = GRIPS);
        result = TRUE;
      }
    }
    else { /* An object was not found */
      result = FALSE;
    }
  }
  else { /* Grips are turned off */
    result = FALSE;
  }

  i_status_update_action_state(w_current, result);

  return result;
}

/*! \brief End process of modifying arc object with grip.
 *  \par Function Description
 *  This function ends the grips process specific to an arc object. It erases
 *  the old arc and write back to the object the new parameters of the arc.
 *  Depending on the grip selected and moved, the right fields are updated.
 *  The function handles the conversion from screen unit to world unit before
 *  updating and redrawing.
 *
 *  If the grip at the center of the arc has been moved - modifying the radius
 *  of the arc -, the new radius is calculated expressed in world unit (the
 *  center is unchanged) and updated with the function geda_arc_object_modify().
 *
 *  If one of the end of arc grip has been moved - modifying one of the
 *  angles describing the arc -, this angle is updated with the
 *  geda_arc_object_modify() function.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Arc Object to end modification on.
 */
static void
o_grips_end_arc(GschemToplevel *w_current, GedaObject *o_current)
{
  int arg1, arg2;

  /* erase the temporary arc */
  /* o_arc_invalidate_rubber (w_current); */

  /* determination of the parameters for geda_arc_object_modify() */
  switch (w_current->which_grip) {
    case ARC_RADIUS:
      /* get the radius from w_current */
      arg1 = w_current->distance;
      /* second parameter is not used */
      arg2 = -1;
      break;

    case ARC_START_ANGLE:
      /* get the start angle from w_current */
      arg1 = w_current->second_wx;
      /* second parameter is not used */
      arg2 = -1;
      break;

    case ARC_END_ANGLE:
      /* get the end angle from w_current */
      arg1 = w_current->second_wy;
      /* second parameter is not used */
      arg2 = -1;
      break;

    default:
      return;
  }

  /* modify the arc with the parameters determined above */
  geda_arc_object_modify(o_current, arg1, arg2, w_current->which_grip);
}

/*! \brief End process of modifying box object with grip
 *  \par Function Description
 *   Validates dimensions of the box before modifications are made.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] o_current  Box Object to end modification on
 */
static void
o_grips_end_box(GschemToplevel *w_current, GedaObject *o_current)
{
  int box_width, box_height;

  box_width  = GET_BOX_WIDTH (w_current);
  box_height = GET_BOX_HEIGHT(w_current);

  /* Check for invalid input: zero width/height boxes are not allowed */
  if ((box_width == 0) || (box_height == 0)) {
    o_box_invalidate_rubber (w_current);
    o_invalidate_object (w_current, o_current);
    return;
  }

  geda_box_object_modify(o_current, w_current->second_wx,
                                    w_current->second_wy,
                                    w_current->which_grip);
}

/*! \brief End process of modifying path object with a grip point
 *  \par Function Description
 *   This function just call the corresponding handler in the o_path
 *   module.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Picture Object to end modification on.
 */
static void
o_grips_end_path(GschemToplevel *w_current, GedaObject *o_current)
{
  geda_path_object_modify (o_current, w_current->second_wx,
                                      w_current->second_wy,
                                      w_current->which_grip);
}

/*! \brief End process of modifying picture object with grip.
 *  \par Function Description
 *   Validates dimensions of a picture object before invalidating the
 *   new rectangle region occupied by the picture object.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Picture Object to end modification on.
 */
static bool
o_grips_end_picture(GschemToplevel *w_current, GedaObject *o_current)
{
  int  width   = GET_PICTURE_WIDTH(w_current);
  int  height  = GET_PICTURE_HEIGHT(w_current);
  bool modified;

  /* Check for invalid input: zero width/height pictures are not allowed */
  if ((!width) || (!height)) {
    o_picture_invalidate_rubber (w_current);
    o_invalidate_object (w_current, o_current);
    modified = FALSE;
  }
  else {

    int new_upper_x = min(w_current->first_wx, w_current->second_wx);
    int new_upper_y = max(w_current->first_wy, w_current->second_wy);
    int new_lower_x = max(w_current->first_wx, w_current->second_wx);
    int new_lower_y = min(w_current->first_wy, w_current->second_wy);

    int old_upper_x = o_current->picture->upper_x;
    int old_upper_y = o_current->picture->upper_y;
    int old_lower_x = o_current->picture->lower_x;
    int old_lower_y = o_current->picture->lower_y;

#if DEBUG

fprintf(stderr, "Before: old_upper_x %d old_upper_y %d old_lower_x %d old_lower_y %d\n",
                         old_upper_x,   old_upper_y,   old_lower_x,   old_lower_y);
fprintf(stderr, "After:  new_upper_x %d new_upper_y %d new_lower_x %d new_lower_y %d\n",
                         new_upper_x,   new_upper_y,   new_lower_x,   new_lower_y);
#endif

    if ((new_upper_x - old_upper_x) +
        (new_upper_y - old_upper_y) +
        (new_lower_x - old_lower_x) +
        (new_lower_y - old_lower_y)) {

      if (w_current->CONTROLKEY) {
        o_picture_invalidate_rubber (w_current);
        geda_picture_object_modify (o_current, w_current->second_wx,
                                               w_current->second_wy,
                                               w_current->which_grip);
      }
      else {
        geda_picture_object_modify_all (o_current, w_current->first_wx,
                                                   w_current->first_wy,
                                                   w_current->second_wx,
                                                   w_current->second_wy);
      }
      modified = TRUE;
    }
    else {
      modified = FALSE;
    }
  }

  GEDA_FREE (w_current->pixbuf_filename);
  GEDA_UNREF (w_current->current_pixbuf);
  w_current->current_pixbuf = NULL;
  w_current->pixbuf_wh_ratio = 0;

  return modified;
}

/*! \brief End process of modifying circle object with grip.
 *  \par Function Description
 *  This function ends the process of modifying the radius of the circle
 *  object <b>*o_current</b>.
 *  The modified circle is finally normally drawn.
 *
 *  A circle with a null radius is not allowed. In this case, the process
 *  is stopped and the circle is left unchanged.
 *
 *  The last value of the radius is in <b>w_current->distance</b> in screen units.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Circle Object to end modification on.
 */
static void
o_grips_end_circle(GschemToplevel *w_current, GedaObject *o_current)
{
  /* don't allow zero radius circles
   * this ends the circle drawing behavior
   * we want this? hack */
  if (w_current->distance == 0) {
    o_circle_invalidate_rubber (w_current);
    o_invalidate_object (w_current, o_current);
    return;
  }

  /* modify the radius of the circle */
  geda_circle_object_modify(o_current, w_current->distance, -1, CIRCLE_RADIUS);
}

static void
o_grips_end_complex(GschemToplevel *w_current, GedaObject *o_current)
{
  if (w_current->distance == 0) {
    o_complex_invalidate_rubber (w_current, o_current);
    o_invalidate_object (w_current, o_current);
    return;
  }

  geda_complex_object_modify(o_current, w_current->second_wx, w_current->second_wy);
}

/*! \brief End process of modifying line object with grip.
 *  \par Function Description
 *  This function ends the process of modifying one end of the line
 *  object <b>*o_current</b>.
 *  This end is identified by <b>whichone</b>. The line object is modified
 *  according to the <b>whichone</b> parameter and the last position of the
 *  line end.
 *  The modified line is finally normally drawn.
 *
 *  A line with a null width, i.e. when both ends are identical, is not
 *  allowed. In this case, the process is stopped and the line unchanged.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Line Object to end modification on.
 */
static void
o_grips_end_line(GschemToplevel *w_current, GedaObject *o_current)
{
  /* don't allow zero length nets / lines / pins
   * this ends the net drawing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_box_invalidate_rubber (w_current);
    o_invalidate_object (w_current, o_current);
    return;
  }

  /* modify the right line end according to w_current->which_grip */
  geda_line_object_modify(o_current, w_current->second_wx,
                                     w_current->second_wy,
                                     w_current->which_grip);
}

/*! \brief End process of modifying net object with grip.
 *  \par Function Description
 *  This function ends the process of modifying one end of the net
 *  object <b>*o_current</b>.
 *  This end is identified by <b>whichone</b>. The line object is modified
 *  according to the <b>whichone</b> parameter and the last position of the
 *  line end.
 *  The connections to the modified net are checked and recreated if neccessary.
 *
 *  A net with zero length, i.e. when both ends are identical, is not
 *  allowed. In this case, the process is stopped and the line unchanged.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Net Object to end modification on.
 */
static void
o_grips_end_net(GschemToplevel *w_current, GedaObject *o_current)
{
  GList *connected_objects;

  /* don't allow zero length net
   * this ends the net drawing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate_object (w_current, o_current);
    return;
  }

  geda_struct_conn_remove_object (o_current);
  geda_net_object_modify (o_current, w_current->second_wx,
                                     w_current->second_wy,
                                     w_current->which_grip);
  geda_struct_conn_update_object (o_current);

  /* add bus rippers if necessary */
  connected_objects = geda_struct_conn_return_others (NULL, o_current);
  o_net_add_busrippers (w_current, o_current, connected_objects);
  g_list_free (connected_objects);
}

/*! \brief End process of modifying pin object with grip.
 *  \par Function Description
 *  This function ends the process of modifying one end of the pin
 *  object <b>*o_current</b>.
 *  This end is identified by <b>whichone</b>. The pin object is modified
 *  according to the <b>whichone</b> parameter and the last position of the
 *  pin end.
 *  The connections to the modified pin are checked and recreated if neccessary.
 *
 *  A pin with zero length, i.e. when both ends are identical, is not
 *  allowed. In this case, the process is stopped and the line unchanged.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  Net Object to end modification on.
 */
static void
o_grips_end_pin(GschemToplevel *w_current, GedaObject *o_current)
{
  /* don't allow zero length pin
   * this ends the pin changing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate_object (w_current, o_current);
    return;
  }

  geda_struct_conn_remove_object (o_current);
  geda_pin_object_modify (o_current, w_current->second_wx,
                                     w_current->second_wy,
                                     w_current->which_grip);
  geda_struct_conn_update_object (o_current);
}

/*! \brief End process of modifying bus object with grip.
 *  \par Function Description
 *  This function ends the process of modifying one end of the bus
 *  object <b>*o_current</b>.
 *  This end is identified by <b>whichone</b>. The line object is modified
 *  according to the <b>whichone</b> parameter and the last position of the
 *  bus end.
 *  The connections to the modified bus are checked and recreated if neccessary.
 *
 *  A bus with zero length, i.e. when both ends are identical, is not
 *  allowed. In this case, the process is stopped and the bus unchanged.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] o_current  bus Object to end modification on.
 */
static void
o_grips_end_bus(GschemToplevel *w_current, GedaObject *o_current)
{
  /* don't allow zero length bus
   * this ends the bus changing behavior
   * we want this? hack */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    o_invalidate_object (w_current, o_current);
    return;
  }

  geda_struct_conn_remove_object (o_current);
  geda_bus_object_modify (o_current, w_current->second_wx,
                                     w_current->second_wy,
                                     w_current->which_grip);
  geda_struct_conn_update_object (o_current);
}

/*! \brief End process of modifying object with grip.
 *  \par Function Description
 *  This function ends the process of modifying a parameter of an object
 *  with a grip.
 *  The temporary representation of the object is erased, the object is
 *  modified and finally drawn.
 *
 *  The object under modification is <b>w_current->which_object</b> and
 *  the grip concerned is <b>w_current->which_grip</b>.
 *
 *  Depending on the object type, a specific function is used. It erases
 *  the temporary object, updates the object and draws the modified object
 *  normally.
 *
 *  \param [in,out] w_current  The GschemToplevel object.
 */
void
o_grips_end(GschemToplevel *w_current)
{
  GedaObject *object;

  object = w_current->which_object;

  if (!object) {
    /* actually this is an error condition hack */
    i_status_set_state(w_current, SELECT);
  }
  else {

    int modified = TRUE;

    switch(object->type) {

      case(OBJ_ARC):
        /* modify an arc object */
        o_grips_end_arc(w_current, object);
        break;

      case(OBJ_BOX):
        /* modify a box object */
        o_grips_end_box(w_current, object);
        break;

      case(OBJ_PATH):
        /* modify a path object */
        o_grips_end_path(w_current, object);
        break;

      case(OBJ_PICTURE):
        /* modify a picture object */
        modified = o_grips_end_picture(w_current, object);
        break;

      case(OBJ_CIRCLE):
        /* modify a circle object */
        o_grips_end_circle(w_current, object);
        break;

      case(OBJ_COMPLEX):
        /* modify a complex object */
        o_grips_end_complex(w_current, object);
        break;

      case(OBJ_LINE):
        /* modify a line object */
        o_grips_end_line(w_current, object);
        break;

      case(OBJ_NET):
        /* modify a net object */
        o_grips_end_net(w_current, object);
        break;

      case(OBJ_PIN):
        /* modify a pin object */
        o_grips_end_pin(w_current, object);
        break;

      case(OBJ_BUS):
        /* modify a bus object */
        o_grips_end_bus(w_current, object);
        break;

      default:
        return;
    }

    /* Switch drawing of the object back on */
    object->dont_redraw = FALSE;
    o_invalidate_object (w_current, object);

    /* reset global variables */
    w_current->which_grip     = -1;
    w_current->which_object   = NULL;
    w_current->rubber_visible = FALSE;

    if (modified) {

      Page *p_current = gschem_toplevel_get_current_page(w_current);

      geda_page_set_changed(p_current, TRUE);;
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
  i_status_action_stop(w_current);
}
