/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: o_extend.c
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
 *  Date: January, 10, 2015
 *  Contributing Author: Wiley Edward Hill
 */
/*!
 * \file o_extend.c
 * \brief Extend/Project Operations Implementatiom Module
 */

#include <gschem.h>
#include <math.h>

/** \defgroup Extend-Operations Extend Operations
 *  @{
 *  \ingroup Editing-Operations
 *  \par This group contains routines for extending projectables.
 *  \image html projections.png
 *  \image latex projections.png
 */

typedef struct st_hit_record  hit_record;
typedef struct st_path_record path_record;

struct st_hit_record {
  GedaObject *object;    /* Pointer to object for this record   */
  int         hits;      /* Number of times object was hit (by projectiles) */
  double      distance;  /* geda_object_get_shortest_distance */
};

/* ----------------------------- Qualifiers --------------------------- */

/*! \brief Check if object is a valid boundary
 *  \par Function Description
 *  Returns true if \a object can serve as a boundary.
 */
static bool o_extend_is_valid_bounder (GedaObject *object)
{
  int anwser;

  if (GEDA_IS_OBJECT(object)) {
    switch (object->type) {
      case OBJ_ARC:
      case OBJ_BOX:
      case OBJ_PATH:
      case OBJ_LINE:
      case OBJ_NET:
      case OBJ_PIN:
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

/*! \brief Check if object is Projectable
 *  \par Function Description
 *  Projectiles are the three linear type objects; line, nets
 *  and buses.
 */

static bool o_extend_is_valid_projectile (GedaObject *object)
{
  int anwser;

  if (GEDA_IS_OBJECT(object)) {
    switch (object->type) {
      case OBJ_LINE:
      case OBJ_NET:
      case OBJ_BUS:
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

/* -------------------------- List Compilers -------------------------- */

/*!
 * \brief Get list of valid bounders from list of objects
 * \par Function Description
 *  Returns a new list containing pointers to object in \a list
 *  that are valid boundary objects.
 *
 * \remark  Returned list must be freed
 */
static GList *o_extend_get_valid_bounders (GList *list)
{
  GList *bounders = NULL;
  GList *iter     = list;

  while (iter) {

    GedaObject *object = iter->data;

    if (o_extend_is_valid_bounder(object)) {
      bounders = g_list_append(bounders, object);
    }
    iter = iter->next;
  }

  return bounders;
}

/*!
 * \brief Get list of non-projectable bounders from list of objects
 * \par Function Description
 *  Returns a new list containing pointers to object in \a list
 *  that are valid boundary objects nut not lines, nets, or buses.
 *
 * \remark  Returned list must be freed
 */
static GList *o_extend_get_nonlinear (GList *list)
{
  GList *bounders = NULL;
  GList *iter     = list;

  while (iter) {
    GedaObject *object = iter->data;
    if (GEDA_IS_OBJECT(object)) {
      switch (object->type) {
        case OBJ_ARC:
        case OBJ_BOX:
        case OBJ_PATH:
        case OBJ_PIN:
        case OBJ_CIRCLE:
          bounders = g_list_append(bounders, object);
          break;
        default:
          break;
      }
    }
    iter = iter->next;
  }

  return bounders;
}

/*!
 * \brief Get list of valid Projectiles from list of objects
 * \par Function Description
 *  Returns a new list containing pointer to object in \a list
 *  that are projectable objects, i.e. lines, nets, and buses.
 *
 * \remark  Returned list must be freed
 */
static GList *o_extend_get_projectiles (GedaObject *exclude, GList *list)
{
  GList *projectiles = NULL;
  GList *iter        = list;

  while (iter) {

    GedaObject *object = iter->data;

    if (object != exclude && o_extend_is_valid_projectile(object)) {
      projectiles = g_list_append(projectiles, object);
    }
    iter = iter->next;
  }

  return projectiles;
}

/* -------------------------- Determinators --------------------------- */

/*!
 * \brief Get bounder of two Linear objects
 * \par Function Description
 *  Determines which of two objects is the bounder. The other object
 *  must be projectable to the selected object.
 *
 * \returns bounder object or NULL if neither can not bound the other
 */
static GedaObject *o_extend_get_bounder_of_two_linears (GedaObject *object1,
                                                        GedaObject *object2)
{
  GedaObject *bounder;
  GedaPoint   point;


  if (geda_line_object_get_intersection(object1, object2, &point)) {

    bool has_slope1;
    bool has_slope2;
    bool included1;
    bool included2;

    has_slope1 = geda_object_get_has_slope(object1);
    has_slope2 = geda_object_get_has_slope(object2);

    included1 = FALSE;
    included2 = FALSE;

    if (has_slope1 && has_slope2) {

      /* check if object 1 includes the point */
      int x11 = min(object1->line->x[0], object1->line->x[1]);
      int x21 = max(object1->line->x[0], object1->line->x[1]);

      int y11 = min(object1->line->y[0],object1->line->y[1]);
      int y21 = max(object1->line->y[0],object1->line->y[1]);

      if ((x11 <= point.x && point.x <= x21) &&
          (y11 <= point.y && point.y <= y21))
      {
        included1 = TRUE;
      }

      /* check if object 2 includes the point */
      int x12 = min(object2->line->x[0],object2->line->x[1]);
      int x22 = max(object2->line->x[0],object2->line->x[1]);

      int y12 = min(object2->line->y[0],object2->line->y[1]);
      int y22 = max(object2->line->y[0],object2->line->y[1]);

      if ((x12 <= point.x && point.x <= x22) &&
          (y12 <= point.y && point.y <= y22))
      {
        included2 = TRUE;
      }
    }
    else if (has_slope1) { /* 2 is vertical at point.x */

      /* check if object 1 includes the point */
      int x11 = min(object1->line->x[0], object1->line->x[1]);
      int x21 = max(object1->line->x[0], object1->line->x[1]);

      if (x11 <= point.x && point.x <= x21) {
        included1 = TRUE;
      }

      /* check if object 2 includes the point */
      int y12 = min(object2->line->y[0],object2->line->y[1]);
      int y22 = max(object2->line->y[0],object2->line->y[1]);

      if (y12 <= point.y && point.y <= y22) {
        included2 = TRUE;
      }
    }
    else if (has_slope2) { /* 1 is vertical at point.x */

      /* check if object 1 includes the point */
      int y11 = min(object1->line->y[0],object1->line->y[1]);
      int y21 = max(object1->line->y[0],object1->line->y[1]);

      if (y11 <= point.y && point.y <= y21) {
        included1 = TRUE;
      }

      /* check if object 2 includes the point */
      int x12 = min(object2->line->x[0],object2->line->x[1]);
      int x22 = max(object2->line->x[0],object2->line->x[1]);

      if (x12 <= point.x && point.x <= x22) {
        included2 = TRUE;
      }
    }

    if (included1 & !included2) {
      bounder = object1;
    }
    else if (included2 & !included1) {
      bounder = object2;
    }
    else { /* both include the point so linears already intersect */
      bounder = NULL;
    }
  }
  else {  /* either parallel or vertical, linears do not intersect */
    bounder = NULL;
  }
  return bounder;
}

/*!
 * \brief Get which end of a projectile is cloest to a bounder
 * \par Function Description
 *  Determines which end point of \a projectile is closest to \a boundary.
 *
 * \returns 0 or 1
 */
static int
o_extend_get_closest_end(GedaObject *projectile, GedaObject *boundary)
{
  double dist1;
  double dist2;

  dist1 = geda_object_get_shortest_distance(boundary, projectile->line->x[0],
                                            projectile->line->y[0]);
  dist2 = geda_object_get_shortest_distance(boundary, projectile->line->x[1],
                                            projectile->line->y[1]);

  return dist1 < dist2 ? 0 : 1;
}

/* -------------------------- Discriminators -------------------------- */

/* Discriminating Resolvers, must be passed a point for Determination */

#if GCC_DIAGNOSTIC_AWARE
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif

/*!
 * \brief Determine if an Arc boundary can bound a given Projectile
 * \par Function Description
 *  Determines if \a projectile can intersect an Arc, \a point is set
 *  to the intersection if point exist and does not already intersect.
 *
 * \returns TRUE or FALSE
 *
 * \remark boundary Must be an Arc object and is not checked!
 */
static bool o_extend_can_arc_bound(GedaObject *boundary,
                                   GedaObject *projectile,
                                   int         which_end,
                                   char        direction,
                                   GedaPoint  *point)
{
  int    answer;
  int    included;
  int    x1, y1, x2, y2;
  int    cx, cy, r;
  double dx, dy;
  double A, B, C, D;
  double b;
  double m;

  GedaPoint pt1;
  GedaPoint pt2;
  GedaPoint pt3;

  x1 = projectile->line->x[!which_end];
  y1 = projectile->line->y[!which_end];
  x2 = projectile->line->x[which_end];
  y2 = projectile->line->y[which_end];

  cx = boundary->arc->x;
  cy = boundary->arc->y;
  r  = boundary->arc->radius;

  dx = x2 - x1;
  dy = y2 - y1;

  included = FALSE;

  /* Get coefficients of quadratic */
  if (dx == 0) {                  /* In terms of Y, because X1 = X2 */

    /* Special vertical case: (x-cx)^2 + (y-cy)^2 = r^2, solve for y */

    A = 1;
    B = -2 * cy;
    C = (x1 * x1) - (2 * cx * x1) + (cx * cx) + (cy * cy) - (r * r);
  }
  else {                          /* In terms of X, all but vertical */

    /* Conventional: (x - cx)^2 + (mx + b - cy)^2 = r^2, solve for x */

    m  = dy / dx;
    b  = (-1 * m * x2) + y2;

    A = m * m + 1;
    B = 2 * ((m * b) - (m * cy) - cx);
    C = (cy * cy) + (cx * cx) - (r * r) - (2 * (b * cy)) + (b * b);
  }

  D = B * B - 4 * A * C;          /* The discriminant */

  if (D < 0) {                    /* If negative discriminant */
    answer = FALSE;               /* the line does not intersect */
  }
  else {                          /* maybe, if not already crossing */

    double tmp_x, tmp_y;

    if (dx == 0) { /* Vertical = special, find y first*/

      if (direction & 2) {                             /* North */
        tmp_y = (-1 * B - sqrt(D)) / (2 * A);
      }
      else {                                           /* South = 'h' */
        tmp_y = (-1 * B + sqrt(D)) / (2 * A);
      }
      tmp_x = x1;
    }
    else {        /* For all non-vertical projectiles */

      if (direction & 1) {                             /* Easterly */
        tmp_x = (-1 * B - sqrt(D)) / (2 * A);
      }
      else {                                           /* Westward */
        tmp_x = (-1 * B + sqrt(D)) / (2 * A);
      }
      tmp_y = m * tmp_x + b;
    }

#ifdef HAVE_LRINT

    pt1.x = lrint(tmp_x);
    pt1.y = lrint(tmp_y);

#else

    pt1.x = tmp_x + 0.5;
    pt1.y = tmp_y + 0.5;

#endif

    if (D > 0) { /* Does the projectile intersect the circle twice? */

      double dist1, dist2;

      answer = FALSE;

      /* recall X2,Y2 is the "hot" point */
      dist1 = geda_distance(x2, y2, pt1.x, pt1.y);

      /* Calculate second intercept */

      if (dx == 0) {   /* Vertical special, find y first*/

        if (direction & 2) {                             /* North */
          tmp_y = (-1 * B + sqrt(D)) / (2 * A);
        }
        else {                                           /* South = 'h' */
          tmp_y = (-1 * B - sqrt(D)) / (2 * A);
        }
        tmp_x = x1;

      }
      else {        /* For all non-vertical projectiles */

        if (direction & 1) {                             /* Easterly */
          tmp_x = (-1 * B + sqrt(D)) / (2 * A);
        }
        else {                                           /* Westward */
          tmp_x = (-1 * B - sqrt(D)) / (2 * A);
        }
        tmp_y = m * tmp_x + b;

      }

#ifdef HAVE_LRINT

      pt2.x = lrint(tmp_x);
      pt2.y = lrint(tmp_y);

#else

      pt2.x = tmp_x + 0.5;
      pt2.y = tmp_y + 0.5;

#endif

      /* Get distance to second intercept */
      dist2 = geda_distance(x2, y2, pt2.x, pt2.y);

      if (dist1 < dist2) {
        included = !geda_line_includes_point(projectile->line, &pt1);
        answer   = included ? 2 : 0;
      }

      if (dist2 <= dist1 || !answer) {
        answer = !geda_line_includes_point(projectile->line, &pt2);
        answer = answer ? 3 : 0;
      }
    }
    else {

      /* Is tangent, easy anwser */
      answer = !geda_line_includes_point(projectile->line, &pt1);
    }
  }

  /* The preceding algorithms determined if the projectile intersected
   * the arc's circle and which point would be picked for a circle, now
   * check if the choosen point is on the arc and if not wheather the
   * other point is on the arc, and if so, not on the projectile.
   */
  if (answer == 1) {     /* tangent, check if arc contains point */
    if (geda_arc_includes_point(boundary->arc, &pt1)) {
      answer = TRUE;
      pt3.x = pt1.x;
      pt3.y = pt1.y;
    }
    else {
      answer = FALSE;
    }
  }
  else if (answer == 2) {  /* 2 points, first was picked above */
    if (geda_arc_includes_point(boundary->arc, &pt1)) {
      answer = TRUE;
      pt3.x = pt1.x;
      pt3.y = pt1.y;
    }
    else if (geda_arc_includes_point(boundary->arc, &pt2)) {
      /* Since the 1st point was picked we did not check pt2 */
      if (!geda_line_includes_point(projectile->line, &pt2)) {
        answer = TRUE;
        pt3.x = pt2.x;
        pt3.y = pt2.y;
      }
    }
    else {
      answer = FALSE;
    }
  }
  else if (answer == 3) {  /* 2 points, second was picked above */
    if (geda_arc_includes_point(boundary->arc, &pt2)) {
      answer = TRUE;
      pt3.x = pt2.x;
      pt3.y = pt2.y;
    }
    else if (included) { /* If 1st point include but not picked, check */
      if (geda_arc_includes_point(boundary->arc, &pt1)) {
        answer = TRUE;
        pt3.x = pt1.x;
        pt3.y = pt1.y;
      }
    }
    else {
      answer = FALSE;
    }
  }

  /* The optional point argument does not save time for arcs */
  if (answer && point) {
    point->x = pt3.x;
    point->y = pt3.y;
  }

  return answer;
}

#if GCC_DIAGNOSTIC_AWARE
#pragma GCC diagnostic pop
#endif

/*!
 * \brief Determine if a GedaBox boundary can bound a given Projectile
 * \par Function Description
 *  Determines if \a projectile can intersect a Box. \a point is set
 *  to the intersection if point exist and does not already intersect.
 *
 * \returns TRUE or FALSE
 *
 * \remark boundary Must be a GedaBox object and is not checked!
 * \image html projections2.png
 * \image latex projections2.png
 */
static bool o_extend_can_box_bound(GedaObject *boundary,
                                   GedaObject *projectile,
                                   int         which_end,
                                   char        direction,
                                   GedaPoint  *point)
{
  GedaBox  *box  = boundary->box;
  GedaLine *proj = projectile->line;
  bool      answer;
  double    slope;

  int  left   = /* min */ box->upper_x < box->lower_x ? box->upper_x : box->lower_x;
  int  bottom = /* min */ box->upper_y < box->lower_y ? box->upper_y : box->lower_y;
  int  right  = /* max */ box->upper_x > box->lower_x ? box->upper_x : box->lower_x;
  int  top    = /* max */ box->upper_y > box->lower_y ? box->upper_y : box->lower_y;

  int  x1     = projectile->line->x[which_end];
  int  y1     = projectile->line->y[which_end];

  inline bool east_bound() {

    bool   answer;

    if (y1 > bottom && y1 < top  &&  x1 < right) {

      answer = TRUE;

      /* if pointer to point then we must find intersection */
      if (point) {

        /* Since projectile is horizontal, we know the y */
        point->y = y1;

        /* we are only dealing with x's here */

        /* Point is to left, right, or inside of the box */
        if (x1 < left) {
          /* Is to the left, so target point is the box's left side */
          point->x = left;
        }
        else { /* < right) */
          /* Is to the Right, so target point is the box's Right side */
          point->x = right;
        }
      }
    }
    else if ((y1 == bottom || y1 == top) && x1 < left) {

      answer = TRUE;

      if (point) {
        point->x = left;
        point->y = y1;
      }
    }
    else {
      answer = FALSE;
    }

    return answer;
  }

  inline bool west_bound() {

    bool   answer;

    if (y1 > bottom && y1 < top && x1 > left) {

      answer = TRUE;

      /* if pointer to point then we must find intersection */
      if (point) {

        /* Since projectile is horizontal, we know the y */
        point->y = y1;                 /* is still arbitrary */

        /* we are only dealing with x's here */

        /* Point is to left, right, or inside of the box */
        if (x1 > right) {           /* if is to the left */
          /* target point is right side of box */
          point->x = right;
        }
        else { /* must be < right) */
          /* target point is the left side of box */
          point->x = left;
        }
      }
    }
    else if ((y1 == bottom || y1 == top)  && x1 > right) {

      /* Is to the left, so target point is the left side of box */
      answer = TRUE;
      if (point) {
        point->x = right;
        point->y = y1;
      }
    }
    else {
      answer = FALSE;
    }

    return answer;
  }

  inline bool north_bound() {

    bool   answer;

    if (x1 > left  && x1 < right && y1 < top) {

      answer = TRUE;

      /* if pointer to point then we must find intersection */
      if (point) {

        /* Since projectile is vertical, we know the x */
        point->x = x1;   /* is arbitrary */

        /* we are only dealing with y's here */

        /* Point is to below, above, or inside of the box */
        if (y1 < bottom) {
          /* Is below, so target point is the box's bottom side */
          point->y = bottom;
        }
        else { /* (projectile->line->y[which_end] < top) */
          /* Is inside, so target point is the box's top side */
          point->y = top;
        }
      }
    }
    else if ((x1 == right  || x1 == left) && y1 < bottom) {
      /* Is to the left, so target point is left side box */
      answer = TRUE;
      if (point) {
        point->x = x1;     /* is arbitrary */
        point->y = bottom;
      }
    }
    else {
      answer = FALSE;
    }

    return answer;
  }

  inline bool south_bound() {

    bool   answer;

    if (x1 > left  && x1 < right && y1 > bottom) {

      answer = TRUE;

      /* if pointer to point then we must find intersection */
      if (point) {

        /* Since projectile is vertical, we know the x */
        point->x = proj->x[0];   /* is arbitrary */

        /* we are only dealing with y's here */

        /* Point is to below, above, or inside of the box */
        if (proj->y[which_end] > top) {
          /* Is above, so target point is the box's top side */
          point->y = top;
        }
        else { /* (projectile->line->y[which_end] > bottom) */
          /* Is inside, so target point is the box's bottom side */
          point->y = bottom;
        }
      }
    }
    else if ((x1 == right  || x1 == left) && y1 > top) {
      /* Is to the left, so target point is the box's left side */
      answer = TRUE;
      if (point) {
        point->x = x1;
        point->y = top;
      }
    }
    else {
      answer = FALSE;
    }
    return answer;
  }

  answer = FALSE;

  /* If geda_line_object_get_slope returns true, the slope is valid */
  if (geda_line_object_get_slope(projectile, &slope)) {

    /* Within this conditional we use y1, but is arbitrary y's are equal */

    if (direction == 'a') {      /* projectile is east bound horizonal */

      answer = east_bound();
    }
    else if (direction == 'd') { /* projectile is west bound horizonal */

      answer = west_bound();

    }
    else { /* projectile is on an angle, y is not arbitrary */

      GedaLine  diagonal;
      GedaPoint tmp;

      /* If box is to bound, projectile must intersect a diagonal */

      diagonal.x[0] = left;
      diagonal.x[1] = right;

      /* Which diagonal depends on the direction of projectile */

      if (direction == 'c' || direction == 'l') {

        /* Ascending eastwardly || Descending westwardly */
        diagonal.y[0] = top;
        diagonal.y[1] = bottom;

      }
      else {  /* (direction == 'i' || direction == 'f') */

        /* Descending eastwardly || Ascending westwardly */
        diagonal.y[0] = bottom;
        diagonal.y[1] = top;
      }

      /* Check if projectile intersects chosen diagonal */
      if (geda_line_get_intersection(proj, &diagonal, &tmp)) {

        /* and the intersection must be within the bounds of the box */
        if (geda_object_get_is_inside_region(left, bottom, right, top, tmp.x, tmp.y)) {

          /* but not on the far side, aka not touching opposite side */
          switch (direction) {
            case 'c':
              answer = x1 < right && y1 < top;
              break;
            case 'f':
              answer = x1 > left && y1 < top;
              break;
            case 'l':
              answer = x1 > left && y1 > bottom;
              break;
            case 'i':
              answer = x1 < right && y1 > bottom;
              break;
            default:
              answer = FALSE;
          }
        }
      }
      else {
        answer = FALSE;
      }

      if (answer && point) { /* If caller requested point */

        bool   outside;
        double b, dx, dy;

        /* If the projectile's "hot" point is outside of the box then
         * the intersection will be the near-side of the box */
        outside = !geda_object_get_is_inside_region(left, bottom, right, top, x1, y1);

        b = (-1 * slope * x1) + y1;

        /* Pick the corner of interest */
        switch (direction) {

          case 'c':          /* outside can only hit left or bottom */
            if (outside) {                       /* If (x1,y1) out side of box */
              if (y1 >= bottom) {
                point->x = left;
                point->y = slope * left + b;
              }
              else if (x1 >= left) {
                point->y = bottom;
                point->x = (bottom - b) / slope;
              }                                  /* below and to left of box */
              else {
                dx = left - x1;
                dy = bottom - y1;
                if (slope > dy / dx) {
                  point->x = left;
                  point->y = slope * left + b;
                }
                else {
                  point->y = bottom;
                  point->x = (bottom - b) / slope;
                }
              }
            }
            else {                               /* inclusively inside of box */

              dx = right - x1;
              dy = top - y1;
              if (slope > dy / dx) {
                 point->y = top;
                 point->x = (top - b) / slope;
              }
              else {
                point->x = right;
                point->y = slope * right + b;
              }
            }
            break;

          case 'f':                     /* outside can only hit right or bottom */
             if (outside) {                       /* If (x1,y1) out side of box */
              if (y1 >= bottom) {
                point->x = right;
                point->y = slope * right + b;
              }
              else if (x1 <= right) {
                point->y = bottom;
                point->x = (bottom - b) / slope;
              }                                  /* below and to left of box */
              else {
                dx = right - x1;
                dy = bottom - y1;
                if (slope > dy / dx) {
                  point->y = bottom;
                  point->x = (bottom - b) / slope;
                }
                else {
                  point->x = right;
                  point->y = slope * right + b;
                }
              }
            }
            else {    /* inclusively inside of box, use The oposite corner */

              dx = left - x1;
              dy = top - y1;
              if (slope > dy / dx) {
                point->x = left;
                point->y = slope * left + b;
              }
              else {
                point->y = top;
                point->x = (top - b) / slope;
              }
            }
            break;

          case 'l':                       /* outside can only hit right or top */
            if (outside) {                       /* If (x1,y1) out side of box */
              if (y1 <= top) {
                point->x = right;
                point->y = slope * right + b;
              }
              else if (x1 <= right) {
                point->y = top;
                point->x = (top - b) / slope;
              }                                  /* below and to left of box */
              else {
                dx = right - x1;
                dy = top - y1;
                if (slope > dy / dx) {
                  point->x = right;
                  point->y = slope * right + b;
                }
                else {
                  point->y = top;
                  point->x = (top - b) / slope;
                }
              }
            }
            else {                  /* inclusively inside of box, use the oposite corner*/
              dx = left - x1;
              dy = bottom - y1;
              if (slope > dy / dx) {
                point->y = bottom;
                point->x = (bottom - b) / slope;
              }
              else {
                point->x = left;
                point->y = slope * left + b;
              }
            }
            break;

          case 'i':                        /* outside can only hit left or top */
            if (outside) {                       /* If (x1,y1) out side of box */
              if (y1 <= top) {
                point->x = left;
                point->y = slope * left + b;
              }
              else if (x1 >= left) {
                point->y = top;
                point->x = (top - b) / slope;
              }                                  /* below and to left of box */
              else {
                dx = left - x1;
                dy = top - y1;
                if (slope > dy / dx) {
                  point->y = top;
                  point->x = (top - b) / slope;
                }
                else {
                  point->x = left;
                  point->y = slope * left + b;
                }
              }
            }
            else {                               /* inclusively inside of box */
              dx = right - x1;
              dy = bottom - y1;
              if (slope > dy / dx) {
                point->x = right;
                point->y = slope * right + b;
              }
              else {
                point->y = bottom;
                point->x = (bottom - b) / slope;
              }
            }
            break;

          default:
            break;
        }
      }
    }
  }
  else { /* projectile is a vertical line, x's are arbitrary */

    if (direction == 'b') {       /* projectile is north bound */

      answer = north_bound();

    }
    else if (direction == 'h') {  /* if projectile is a south bound */

      answer = south_bound();
    }
  }

  return answer;
}

#if GCC_DIAGNOSTIC_AWARE
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif

/*!
 * \brief Determine if a Path boundary can bound a given Projectile
 * \par Function Description
 *  Determines if \a projectile can intersect a Path. \a point is set
 *  to the intersection if point exist and does not already intersect.
 *
 * \returns TRUE or FALSE
 *
 * \remark boundary Must be a Path object and is not checked!
 */
static bool o_extend_can_path_bound(GedaObject *boundary,
                                    GedaObject *projectile,
                                    int         which_end,
                                    char        direction,
                                    GedaPoint  *point)
{
  GArray   *points;
  GedaPoint target;
  bool      answer;
  int       closed;

  int x1 = projectile->line->x[which_end];
  int y1 = projectile->line->y[which_end];

  answer = FALSE;
  points = g_array_new (FALSE, FALSE, sizeof(GedaPoint));
  closed = geda_struct_path_to_polygon (boundary->path, points);

  if (points->len > 0) {

    double shortest = G_MAXDOUBLE;
    int i = 0;
    GedaPoint vertex;

    if (closed) {
      vertex = g_array_index (points, GedaPoint, points->len - 1);
    }
    else {
      vertex = g_array_index (points, GedaPoint, i++);
    }

    while (i < points->len) {

      GedaPoint intersect;
      GedaLine line;

      line.x[0] = vertex.x;
      line.y[0] = vertex.y;

      vertex = g_array_index (points, GedaPoint, i++);

      line.x[1] = vertex.x;
      line.y[1] = vertex.y;

      if (geda_line_get_intersection(projectile->line, &line, &intersect)) {

        if (!geda_line_includes_point(projectile->line, &intersect)) {

          if (geda_line_includes_point(&line, &intersect)) {

            double distance;

            int x2   = intersect.x;
            int y2   = intersect.y;

#if HAVE_HYPOT
            distance = hypot((x1-x2), (y1-y2));
#else
            distance = sqrt((x1-x2) * (x1-x2) + (y1-y2) * (y1-y2));
#endif

            if (distance != G_MAXDOUBLE && distance < shortest) {
              answer   = TRUE;
              target.x = intersect.x;
              target.y = intersect.y;
              shortest = distance;
            }
          }
        }
      }
    }
  }

  if (answer == TRUE && point != NULL) {
    point->x = target.x;
    point->y = target.y;
  }

  g_array_free (points, TRUE);

  return answer;
}

#if GCC_DIAGNOSTIC_AWARE
#pragma GCC diagnostic pop
#endif

/*!
 * \brief Determine if Linear boundary can bound a given Projectile
 * \par Function Description
 *  Determines if \a projectile can intersect a Linear. \a point is set
 *  to the intersection if point exist and does not already intersect.
 *
 * \returns TRUE or FALSE
 *
 * \remark boundary Must be a Linear object and is not checked!
 */
static bool o_extend_can_linear_bound(GedaObject *boundary,
                                      GedaObject *projectile,
                                      int         which_end,
                                      char        direction,
                                      GedaPoint  *point)
{
  bool  has_slope1;
  bool  has_slope2;
  bool  included1;
  bool  included2;
  bool  answer;
  GedaPoint pt;

  answer = geda_line_object_get_intersection(boundary, projectile, &pt);

  has_slope1 = geda_object_get_has_slope(projectile);
  has_slope2 = geda_object_get_has_slope(boundary);

  included1 = FALSE;
  included2 = FALSE;

  if (has_slope1 && has_slope2) {

    /* check if projectile includes the point */
    int x11 = min(projectile->line->x[0], projectile->line->x[1]);
    int x21 = max(projectile->line->x[0], projectile->line->x[1]);

    int y11 = min(projectile->line->y[0], projectile->line->y[1]);
    int y21 = max(projectile->line->y[0], projectile->line->y[1]);

    if ((x11 <= pt.x && pt.x <= x21) &&
        (y11 <= pt.y && pt.y <= y21))
    {
      included1 = TRUE;
    }

    /* check if boundary includes the point */
    int x12 = min(boundary->line->x[0],boundary->line->x[1]);
    int x22 = max(boundary->line->x[0],boundary->line->x[1]);

    int y12 = min(boundary->line->y[0],boundary->line->y[1]);
    int y22 = max(boundary->line->y[0],boundary->line->y[1]);

    if ((x12 <= pt.x && pt.x <= x22) &&
        (y12 <= pt.y && pt.y <= y22))
    {
      included2 = TRUE;
    }
  }
  else if (has_slope1) { /* boundary is vertical at point.x */

    /* check if projectile includes the point */
    int x11 = min(projectile->line->x[0], projectile->line->x[1]);
    int x21 = max(projectile->line->x[0], projectile->line->x[1]);

    if (x11 <= pt.x && pt.x <= x21) {
      included1 = TRUE;
    }

    /* check if boundary includes the point */
    int y12 = min(boundary->line->y[0],boundary->line->y[1]);
    int y22 = max(boundary->line->y[0],boundary->line->y[1]);

    if (y12 <= pt.y && pt.y <= y22) {
      included2 = TRUE;
    }
  }
  else if (has_slope2) { /* projectile is vertical at point.x */

    /* check if projectile includes the point */
    int y11 = min(projectile->line->y[0],projectile->line->y[1]);
    int y21 = max(projectile->line->y[0],projectile->line->y[1]);

    if (y11 <= pt.y && pt.y <= y21) {
      included1 = TRUE;
    }

    /* check if boundary includes the point */
    int x12 = min(boundary->line->x[0],boundary->line->x[1]);
    int x22 = max(boundary->line->x[0],boundary->line->x[1]);

    if (x12 <= pt.x && pt.x <= x22) {
      included2 = TRUE;
    }
  }

  answer = (answer && included2 && !included1);

  if (point && answer) {
    point->x = pt.x;
    point->y = pt.y;
  }

  return answer;
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
static bool o_extend_can_circle_bound(GedaObject *boundary,
                                      GedaObject *projectile,
                                      int         which_end,
                                      char        direction,
                                      GedaPoint  *point)
{
  bool   answer;
  int    x1, y1, x2, y2;
  int    cx, cy, r;
  double dx, dy;
  double A, B, C, D;

  volatile double b;
  volatile double m;

  x1 = projectile->line->x[!which_end];
  y1 = projectile->line->y[!which_end];
  x2 = projectile->line->x[which_end];
  y2 = projectile->line->y[which_end];

  cx = boundary->circle->center_x;
  cy = boundary->circle->center_y;
  r  = boundary->circle->radius;

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

  if (D < 0) {                     /* If negative discriminant */
    answer = FALSE;                /* the line does not intersect */
  }
  else {                           /* maybe, if not already crossing */

    GedaPoint pt1;
    GedaPoint pt2;

#ifdef HAVE_LRINT

    if (dx == 0) { /* Vertical = special, find y first*/

      if (direction & 2) {                             /* North */
        pt1.y = lrint((-1 * B - sqrt(D)) / (2 * A));
      }
      else {                                           /* South = 'h' */
        pt1.y = lrint((-1 * B + sqrt(D)) / (2 * A));
      }
      pt1.x = x1;
    }
    else {        /* For all non-vertical projectiles */

      double x;

      if (direction & 1) {                             /* Easterly */
        x = (-1 * B - sqrt(D)) / (2 * A);
      }
      else {                                           /* Westward */
        x = (-1 * B + sqrt(D)) / (2 * A);
      }

      pt1.x = lrint(x);
      pt1.y = lrint(m * x + b);

    }

#else

    if (dx == 0) { /* Vertical = special, find y first*/

      if (direction & 2) {                             /* North */
        pt1.y = ((-1 * B - sqrt(D)) / (2 * A)) + 0.5;
      }
      else {                                           /* South = 'h' */
        pt1.y = ((-1 * B + sqrt(D)) / (2 * A)) + 0.5;
      }
      pt1.x = x1;

    }
    else {        /* For all non-vertical projectiles */

      double x;

      if (direction & 1) {                             /* Easterly */
        x = (-1 * B - sqrt(D)) / (2 * A);
      }
      else {                                           /* Westward */
        x = (-1 * B + sqrt(D)) / (2 * A);
      }

      pt1.x = x + 0.5;
      pt1.y = (m * x + b) + 0.5;

    }

#endif

    if (D > 0) { /* Does the projectile intersect the circle twice? */

      double dist1, dist2;

      answer = FALSE;

      /* recall X2,Y2 is the "hot" point */
#if HAVE_HYPOT
      dist1 = hypot((x2-pt1.x), (y2-pt1.y));
#else
      dist1 = sqrt((x2-pt1.x) * (x2-pt1.x) + (y2-pt1.y) * (y2-pt1.y));
#endif
      /* Calculate second intercept */

#ifdef HAVE_LRINT

      if (dx == 0) {   /* Vertical special, find y first*/

        if (direction & 2) {                             /* North */
          pt2.y = lrint((-1 * B + sqrt(D)) / (2 * A));
        }
        else {                                           /* South = 'h' */
          pt2.y = lrint((-1 * B - sqrt(D)) / (2 * A));
        }
        pt2.x = x1;

      }
      else {        /* For all non-vertical projectiles */

        double x;

        if (direction & 1) {                             /* Easterly */
          x = (-1 * B + sqrt(D)) / (2 * A);
        }
        else {                                           /* Westward */
          x = (-1 * B - sqrt(D)) / (2 * A);
        }

        pt2.x = lrint(x);
        pt2.y = lrint(m * x + b);

      }

#else

      if (dx == 0) {   /* Vertical special, find y first*/

        if (direction & 2) {                             /* North */
          pt2.y = ((-1 * B + sqrt(D)) / (2 * A)) + 0.5;
        }
        else {                                           /* South = 'h' */
          pt2.y = ((-1 * B - sqrt(D)) / (2 * A)) + 0.5;
        }
        pt2.x = x1;

      }
      else {        /* For all non-vertical projectiles */

        double x;

        if (direction & 1) {                             /* Easterly */
          x = (-1 * B + sqrt(D)) / (2 * A);
        }
        else {                                           /* Westward */
          x = (-1 * B - sqrt(D)) / (2 * A);
        }

        pt2.x = x + 0.5;
        pt2.y = (m * x + b) + 0.5;
      }

#endif

      /* Get distance to second intercept */
#if HAVE_HYPOT
      dist2 = hypot((x2-pt2.x), (y2-pt2.y));
#else
      dist2 = sqrt((x2-pt2.x) * (x2-pt2.x) + (y2-pt2.y) * (y2-pt2.y));
#endif

      if (dist1 < dist2) {

        answer = !geda_line_includes_point(projectile->line, &pt1);

        if (answer && point) {
          point->x = pt1.x;
          point->y = pt1.y;
        }
      }

      if (dist2 <= dist1 || !answer) {

        answer = !geda_line_includes_point(projectile->line, &pt2);

        if (answer && point) {
          point->x = pt2.x;
          point->y = pt2.y;
        }
      }
    }
    else {

      /* Is tangent, easy anwser */
      answer = !geda_line_includes_point(projectile->line, &pt1);

      if (answer && point) {
        point->x = pt1.x;
        point->y = pt1.y;
      }
    }
  }
  return answer;
}

/*!
 * \brief Determine if an Object can bound a given Projectile
 * \par Function Description
 *  Calls appropriate o_extend_can_xx function based on the type
 *  of object the boundary represents. The selected intersection
 *  will be determined by the handler and returned in \a point if
 *  point is not NULL.
 *
 * \returns TRUE or FALSE
 */
static bool o_extend_can_bound(GedaObject *boundary,
                               GedaObject *projectile,
                               int         which_end,
                               char        direction,
                               GedaPoint  *point)
{
  bool (*discriminator)(GedaObject*, GedaObject*, int, char, GedaPoint*);

  switch (boundary->type) {
    case OBJ_ARC:
      discriminator = o_extend_can_arc_bound;    break;
    case OBJ_BOX:
      discriminator = o_extend_can_box_bound;    break;
    case OBJ_PATH:
      discriminator = o_extend_can_path_bound;   break;
    case OBJ_LINE:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:
      discriminator = o_extend_can_linear_bound; break;
    case OBJ_CIRCLE:
      discriminator = o_extend_can_circle_bound; break;
    default:
      return FALSE;
  }

  return discriminator (boundary, projectile, which_end, direction, point);
}

/*!
 * \brief Determine if Projectile can hit a given Target
 * \par Function Description
 *  This function is used as a wrapper for o_extend_can_bound when
 *  the distance is needed (to sort out which boundary to pick),
 *  rather than the actual point of intersection.
 *
 * \returns TRUE or FALSE
 */
static bool o_extend_can_hit_target(GedaObject *projectile,
                                    GedaObject *target,
                                    int         which_end,
                                    char        direction,
                                    double     *distance)
{
  bool   answer;
  GedaPoint  point;

  answer = o_extend_can_bound(target, projectile, which_end, direction, &point);

  if (answer) {

    GedaLine *line;

    line     = projectile->line;
   *distance = geda_distance(line->x[which_end], line->y[which_end], point.x, point.y);
  }

  return answer; /* In theory, there's a chance */
}

/* ---------------------------- Orientators --------------------------- */

/*!
 * \brief Get Orientation of Projectile
 * \par Function Description
 *  This function is used as a wrapper for o_extend_can_bound when
 *  the distance is needed (to sort out which boundary to pick),
 *  rather than the actual point of intersection. See documentation
 *  "projections.sch"
 *
 * \returns char assignment designating the direction
 */
char o_extend_get_direction(GedaObject *object, int which_end)
{
  char direction;

  if (object->line->y[0] == object->line->y[1]) { /* horizontal */

    if (object->line->x[0] < object->line->x[1]) {
      direction = which_end == 1 ? 'a' : 'd';
    }
    else {
      direction = which_end == 0 ? 'a' : 'd';
    }
  }
  else if (object->line->x[0] == object->line->x[1]) { /* horizontal */

    if (object->line->y[0] < object->line->y[1]) {
      direction = which_end == 1 ? 'b' : 'h';
    }
    else {
      direction = which_end == 0 ? 'b' : 'h';
    }
  }
  else{

    int x1, y1, x2, y2;

    x1 = object->line->x[!which_end];
    y1 = object->line->y[!which_end];

    x2 = object->line->x[which_end];
    y2 = object->line->y[which_end];

    if (x1 < x2 && y1 < y2) {
      direction = 'c';
    }
    else if (x1 < x2) { /* y1 must be > y2*/
      direction = 'i';
    }
    else if (x1 > x2 && y1 < y2) {
      direction = 'f';
    }
    else { /* x1 must be > x2 & y1 must be > y2*/
      direction = 'l';
    }
  }

  return direction;
}

/* -------------------------- Object Getters -------------------------- */

/*!
 * \brief Select Bounder from list of objects based on point and distance
 * \par Function Description
 *  Determine which object, if any, should be the bounder from a list of
 *  objects. For each potential bounder, all other members of \a list are
 *  considered as candidates as projectiles. If no \a point is specified,
 *  the bounder with the most hits is choosen, if there is a tie, then no
 *  bounder is choosen. If \a point is specified the closest object with
 *  a hit to the point is the bounder.
 *
 * \returns The choosen object
 *
 * \remark Note that all projectiles can also be bounders.
 */
GedaObject *o_extend_get_bounder (GList *list, const GedaPoint *point)
{
  GedaObject *bounder = NULL;
  GList      *iter    = list;
  int         count;

  count = g_list_length(list);

  if (count == 2) { /* Handle the simple case = two objects */

    GedaObject *object1;
    GedaObject *object2;

    bool valid1;
    bool valid2;

    object1 = list->data;
    iter    = iter->next;
    object2 = iter->data;

    valid1 = o_extend_is_valid_projectile(object1);
    valid2 = o_extend_is_valid_projectile(object2);

    if (valid1 && !valid2) {
      if (o_extend_is_valid_bounder(object2)) {
        bounder = object2;
      }
      else {
        /* 1 really is but 2 can not bound 1, since there are only two
         * objects, the selection set is not valid for projecting */
        bounder = NULL;
      }
    }
    else if (valid2 && !valid1) {
      if (o_extend_is_valid_bounder(object1)) {
        bounder = object1;
      }
      else {
        bounder = NULL; /* 2 really is but 1 can not bound 2 */
      }
    }
    else if (valid1 && valid2) {
      bounder = o_extend_get_bounder_of_two_linears(object1, object2);
    }
    else { /* neither is valid */
      bounder = NULL;
    }
  }
  else {

    GList *bounders = NULL;  /* List of valid objects that can bound  */
    GList *nonlinears;       /* sub-list of non-projectable bounders  */
    GList *projectiles;      /* list of objects that can be projected */

    int count_bounders;      /* length of the list bounders */

    int count_projectiles;   /* length of the list of projectiles */

    int most_hits;           /* index of record with the most hits */
    int num_records;         /* number of indexes, aka array size  */
    int index;               /* working index */

    hit_record *hit_records;

    most_hits   = 0;
    num_records = 0;
    bounders    = o_extend_get_valid_bounders(list);
    projectiles = o_extend_get_projectiles(NULL, list);

    count_bounders    = g_list_length(bounders);
    count_projectiles = g_list_length(projectiles);

    /* This is a question of logistics more than mathmatics */
    if (count_bounders - count_projectiles > 0) {

      int count_nonlinears;    /* length of the list nonlinears  */

      /* Get the list of objects that can not be projected */
      nonlinears       = o_extend_get_nonlinear(list);
      count_nonlinears = g_list_length(nonlinears);

      if (count_nonlinears == 1) {
        bounder = nonlinears->data; /* What else are we suppose to do? */
        hit_records = NULL;
      }
      else {

        GList *iter2;

        num_records = sizeof(struct st_hit_record) * count_bounders;
        hit_records = malloc(num_records);

        /* Create an array of hit counts */
        for (index = 0; index < count_bounders; index++) {

          int hits = 0;

          for (iter = projectiles; iter; iter = iter->next) {
            GedaObject *projectile = iter->data;
            for (iter2 = bounders; iter2; iter2 = iter2->next) {
              GedaObject *boundary = iter2->data;
              int  which_end = o_extend_get_closest_end(projectile, boundary);
              char direction = o_extend_get_direction(projectile, which_end);
              if (boundary != projectile) { /* Skip the object were checking */
                if (o_extend_can_bound(boundary, projectile, which_end, direction, NULL)) {
                  hits++;
                }
              }
            }
          }

         GedaObject *record = g_list_nth_data(bounders, index);

          hit_records[index].object = record;
          hit_records[index].hits   = hits;

          if (point) {
            hit_records[index].distance = geda_object_get_shortest_distance(record,
                                                                  point->x,
                                                                  point->y);
          }

          if (hits >= hit_records[most_hits].hits) {
            most_hits = index;
          }
        }
      }
      g_list_free(bounders);
      if (count_nonlinears) {
        g_list_free(nonlinears);
      }
    }
    else { /* count projectiles == count bounders, means all are linear */

      GList *projectiles;
      int    count_projectiles;

      projectiles       = o_extend_get_projectiles(NULL, list);
      count_projectiles = g_list_length(projectiles);

      num_records = sizeof(struct st_hit_record) * count_projectiles;
      hit_records = malloc(num_records);

      /* Create an array of hit counts */
      for (index = 0; index < count_projectiles; index++) {

        int hits = 0;

       GedaObject *projectile = g_list_nth_data(projectiles, index);

        for (iter = projectiles; iter; iter = iter->next) {
         GedaObject *boundary = iter->data;
          if (boundary != projectile) { /* Skip the object were checking */
            int  which_end = o_extend_get_closest_end(projectile, boundary);
            char direction = o_extend_get_direction(projectile, which_end);
            if (o_extend_can_bound(boundary, projectile, which_end, direction, NULL)) {
              hits++;
            }
          }
        }

        hit_records[index].object = projectile;
        hit_records[index].hits   = hits;

        if (point) {
          hit_records[index].distance = geda_object_get_shortest_distance(projectile,
                                                                point->x,
                                                                point->y);
        }

        if (hits >= hit_records[most_hits].hits) {
          most_hits = index;
        }
      }
    }

    if (hit_records) {

      /* We should have all we need to know in the array of hit_records to see
       * if there is a winner, aka a bounder. The choice depends on if a point
       * was given. If a point was given then the closest object with a hit to
       * the point is the bounder. If no point was given the object with the
       * most hits is the bounder, if there is a tie, then there is no bounder.
       */
      if (point) {

        /* Index to first record with a hit */
        for (index = 0; index < num_records; index++) {
          if (hit_records[index].hits > 0) {
            break;
          }
        }

        int shortest = index; /* Start from here */

        while (index < num_records) {
          if (hit_records[index].distance < hit_records[shortest].distance) {
            shortest = index;
          }
          index++;
        }

        /* We use [most_hits] to confirm at least one bonder was hit,
         * otherwise the previous loops would not have iterated */
        if (hit_records[most_hits].hits > 0) {
          bounder = hit_records[shortest].object; /* winner is closest bounder */
        }
      }
      else {

        if (hit_records[most_hits].hits > 0) { /* if there was a winner */
          bounder = hit_records[most_hits].object;
        }

        /* Check if another bounder had the same hit count */
        for (index = 0; index < num_records; index++) {
          if (index == most_hits) {
            continue; /* Is the number we are comparing to others */
          }
          if (hit_records[index].hits == hit_records[most_hits].hits) {
            bounder = NULL; /* Disqualify the winner, there was a tie */
            break;
          }
        }
      }

      free(hit_records);
    }
    g_list_free(projectiles);
  }

  return bounder;
}

/* ------------------------ Projector Processor ----------------------- */

/*!
 * \brief Project Linear to Boundary
 * \par Function Description
 *  Determine which end of the projectile is closest to the bounder
 *  and if o_extend_can_bound return TRUE, then change the previously
 *  selected point, the "which_end" to the returned coordinates.
 *
 * \returns True if the operation succeeded, otherwise false
 */
bool o_extend_object (GschemToplevel *w_current,
                      GedaObject     *projectile,
                      GedaObject     *bounder)
{
  GedaPoint point;
  char direction;
  int  result;
  int  which_end;
  int  x;
  int  y;

  x = w_current->second_wx;
  y = w_current->second_wy;

  //which_end = o_extend_get_closest_end(projectile, bounder);
  which_end = geda_line_object_get_closest_endpoint(projectile, x, y);

  direction = o_extend_get_direction(projectile, which_end);

  if (o_extend_can_bound(bounder, projectile, which_end, direction, &point))
  {
    projectile->line->x[which_end] = point.x;
    projectile->line->y[which_end] = point.y;

    if (projectile->type == OBJ_NET || projectile->type == OBJ_BUS) {
      geda_struct_tile_update_object (projectile);
      geda_struct_conn_update_object (projectile);
    }
    result = TRUE;
  }
  else {
    result = FALSE;
  }

  return result;
}

/* ------------------------ Project List Iterator --------------------- */

int o_extend_object_list (GschemToplevel *w_current,
                          GedaObject     *bounder,
                          GList          *projectiles)
{
  int    status = 0;
  GList *iter;

  for(iter = projectiles; iter; iter = iter->next) {

    GedaObject *projectile = iter->data;

    if (o_extend_object(w_current, projectile, bounder)) {
      o_invalidate_object(w_current, projectile);
      status++;
    }
  }

  return status;
}

/* --------------------- Blind Projector Processor -------------------- */

/*!
 * \brief Extend Linear without a known target
 * \par Function Description
 *  Determine which end of the \a projectile is to be used based on
 *  second_wx and second_wy.
 *
 * \returns 0 if not valid, 2 if valid projectile but no hits and 3 if
 *          the operation succeeded.
 */
int o_extend_blind (GschemToplevel *w_current, GedaObject *projectile)
{
  bool    ret_val;

  if (o_extend_is_valid_projectile (projectile)) {

    GList      *iter;
    GedaObject *target;
    double      shortest;
    char        direction;
    int         which_end;
    int         x;
    int         y;

    ret_val = 2;

    x = w_current->second_wx;
    y = w_current->second_wy;

    which_end = geda_line_object_get_closest_endpoint(projectile, x, y);

    direction = o_extend_get_direction(projectile, which_end);
    iter      = geda_struct_page_get_objects(Current_Page);
    shortest  = G_MAXDOUBLE;
    target    = NULL;

    while (iter) {

      double  distance;
     GedaObject *bounder;

      bounder = iter->data;

      if (bounder != projectile && o_extend_is_valid_bounder(bounder)) {

        int closest_end;

        closest_end = o_extend_get_closest_end(projectile, bounder);

        if (which_end == closest_end) {

          if (o_extend_can_hit_target(projectile, bounder, which_end,
                                      direction, &distance))
          {
            if (distance != G_MAXDOUBLE && distance < shortest) {
              target   = bounder;
              shortest = distance;

            }
          }
        }
      }
      iter = iter->next;
    }

    if (target != NULL) {

      GedaPoint point;

      if (o_extend_can_bound(target, projectile, which_end, direction, &point))
      {
        projectile->line->x[which_end] = point.x;
        projectile->line->y[which_end] = point.y;

        if (projectile->type == OBJ_NET || projectile->type == OBJ_BUS) {
          geda_struct_tile_update_object (projectile);
          geda_struct_conn_update_object (projectile);
        }

        o_invalidate_object(w_current, projectile);
        ret_val = 3;
      }
    }
  }
  else {
    ret_val = 0; /* Was not a valid projectile */
  }

  return ret_val;
}

/* ------------------- Blind Projector List Iterator ------------------ */

int o_extend_blind_list(GschemToplevel *w_current, GList *projectiles)
{
  int status;
  GList *iter;

  status = 0;

  for(iter = projectiles; iter; iter = iter->next) {

   GedaObject *projectile = iter->data;

    if (o_extend_blind(w_current, projectile) == 3) {
      status++;
    }
  }

  return status;
}

/* ----------------------- Public Event Handlers ---------------------- */

/*!
 * \brief Start a Projection operation
 * \par Function Description
 *  This function is called at the beginning of a extend operation to
 *  save x and y coordinates for the event and if an object can be hit
 *  an attempt is made to complete the operation, if a projection is
 *  not performed then the object is added to the current selection.
 *
 * \returns 1. EXTEND if the found object is a bounder, or a the
 *             object is projectile but can not be projected
 *             (because the algorithms did not find a target).
 *          2. STARTEXTEND if object was projected by o_extend
 *             _blind.
 *          3. SELECT to terminate Project mode because nothing
 *             was changed and the selection was not valid.
 */
int o_extend_start(GschemToplevel *w_current, int w_x, int w_y)
{
  int status;

 GedaObject *o_current = o_find_get_hit (w_current, w_x, w_y);

  if (o_current != NULL) {

    int result;

    w_current->first_wx = w_current->second_wx = w_x;
    w_current->first_wy = w_current->second_wy = w_y;

    result = o_extend_blind (w_current, o_current);

    if (result == 0) { /* Oops, not a projectile */

      if (o_extend_is_valid_bounder(o_current)) {
        status = EXTEND;
      }
      else {
        /* User clicked on an object that can not be projected
         * and can not bound, see we will abort projection mode
         * and fallback to select mode.
         */
        status = SELECT;
      }
      /* In either case, add object to current selection */
      o_select_object (w_current, o_current, SINGLE, 0); /* 0 is count */
    }
    else if (result == 2) { /* Is projectile, but nothing was hit */

      /* Since all projectiles can also be bounders, we will assume
       * o_current is going to be the bounder, add to selection
       * and change states
       */
      w_current->first_wx = w_current->second_wx = w_x = -1;
      w_current->first_wy = w_current->second_wx = w_x = -1;
      o_select_object (w_current, o_current, SINGLE, 0); /* 0 is count */
      status = EXTEND;
    }
    else if (result == 3) { /* Something was hit */

      geda_struct_object_set_page_changed (o_current);
      o_undo_savestate (w_current, UNDO_ALL);

      /* Was verb->noun so just stay in STARTEXTEND state */
      status = STARTEXTEND;
    }
    else {
      status = 0; /* Never get here */
    }
  }
  else { /* Maybe user missed object */
    status = STARTEXTEND;
  }

  return status;
}

/*!
 * \brief Projection event Selection
 * \par Function Description
 *  This function is called after a button press event and one
 *  object had been previously selected to response to either
 *  EXTEND or ENDEXTEND events.
 */
int o_extend_end (GschemToplevel *w_current, int x, int y)
{
  int     status;
  GedaObject *o_current = o_find_get_hit (w_current, x, y);

  if (o_current != NULL) {

    GList  *object_list;
    GedaObject *bounder;

    int count;

    object_list = geda_list_get_glist (Current_Selection);
    count       = g_list_length(object_list);
    status      = 0;

    w_current->second_wx     = x;
    w_current->second_wy     = y;

    /* Check if only one potential target */
    if (count == 1) {

      GedaObject *projectile;

      if (o_extend_is_valid_projectile(o_current)) {

        projectile = o_current;

        if (o_extend_is_valid_bounder (object_list->data)) {
          bounder = object_list->data;
          if(o_extend_object(w_current, projectile, bounder)) {
            o_invalidate_object(w_current, projectile);
            status = 3;  /* bit 1 to save state, bit 2 to stay in mode */
          }
        }
      }
      else if (o_extend_is_valid_bounder (o_current)) {

        bounder = o_current;

        if (o_extend_is_valid_projectile (object_list->data)) {
          projectile = object_list->data;
          status = o_extend_object(w_current, projectile, bounder);
          if (status) {
            o_invalidate_object(w_current, projectile);
          }
        }
      }
    }
    else {

      /* Can only get here if o_extend_selection returned false to
       * o_extend_interrogate because the current selection did not
       * have a bounder (but the selection must have contain multiple
       * objects or o_extend_selection would not have been called.)
       * Therefore, o_current must be a bounder or we do nothing */
      if (o_extend_is_valid_bounder (o_current)) {

        GList  *projectiles;

        bounder     = o_current;
        projectiles = o_extend_get_projectiles(bounder, object_list);

        if (projectiles) {
          status = o_extend_object_list(w_current, bounder, projectiles);
          status = status > 1 ? 1 : 0;
          g_list_free(projectiles);
        }
      }
    }
  }
  else {
    status = 2; /* stay in project mode, nothing to undo */
  }

  if (status & 1) {
    geda_struct_object_set_page_changed (o_current);
    o_undo_savestate (w_current, UNDO_ALL);
  }

  i_status_action_start(w_current);

  return (status & 2);
}

/* ------------------------- Action Processor ------------------------- */

/*!
 * \brief Project Selected objects
 * \par Function Description
 *  Called when multiple objects were selected when Project mode was
 *  initiated. Uses the object returned by o_extend_get_bounder as
 *  the boundary object and attempts to project all other objects to
 *  the boundary using o_extend_object if there was only one other
 *  object, or o_extend_object_list if ther were more than one other
 *  object selected. If the current selection does not contain a
 *  bounder, that is, a boundary for the other objects in the current
 *  selection then the state is indeterminate and the function returns
 *  true to indicate that o_extend_end needs to be called after the
 *  user selects another object.
 *
 *  \returns TRUE if operation should continue, otherwise FALSE.
 */
bool o_extend_selection (GschemToplevel *w_current, int count)
{
  GList  *object_list;
  GedaObject *bounder;
  int     status;

  object_list = geda_list_get_glist (Current_Selection);
  bounder     = o_extend_get_bounder(object_list, NULL);

  if (bounder) {

    if (count == 2) {

     GedaObject *projectile;

      if (g_list_nth_data(object_list,1) != bounder) {
        projectile = g_list_nth_data(object_list,1);
      }
      else {
        projectile = object_list->data;
      }

      if (o_extend_is_valid_projectile(projectile)) {
        /* returns TRUE is something modified */
        status = o_extend_object(w_current, projectile, bounder);
        if (status) {
          o_invalidate_object(w_current, projectile);
        }
      }
      else {
        status = 0; /* Invalid object in selection, do not continue */
      }
    }
    else {

      GList *projectiles;

      projectiles = o_extend_get_projectiles(bounder, object_list);

      if (projectiles) {
        /* returns count of modifications*/
        status = o_extend_object_list(w_current, bounder, projectiles);
        g_list_free(projectiles);
      }
      else {
        status = 0; /* Invalid object in selection, do not continue */
      }
    }
    if (status) {
      o_undo_savestate (w_current, UNDO_ALL);
      status = 0;   /* task completed, no need to continue */
    }
  }
  else {
    status = 1; /* Undeteminate, continue for now */
  }

  return status;
}

/*!
 * \brief Project Hot
 * \par Function Description
 *  Called to process one or more selected objects when Project mode was
 *  initiated using the keyboard or mouse. The direction each object is
 *  to be projected is based on cursor position relative to the objects,
 *  and need not be the same for each object. The closest boundary in the
 *  path of each projectile will be the target boundary.
 */
void o_extend_hot (GschemToplevel *w_current, GList *object_list, int x, int y)
{
  GList *projectiles = o_extend_get_projectiles(NULL, object_list);
  int    status;

  if (projectiles) {

    w_current->second_wx = x;
    w_current->second_wy = y;

    status = o_extend_blind_list(w_current, projectiles);

    g_list_free(projectiles);

    if (status) {

      Page *p_current = gschem_toplevel_get_current_page (w_current);

      geda_page_set_changed (p_current, TRUE);

      o_undo_savestate (w_current, UNDO_ALL);
    }
  }
  else {
    status = 0;
  }

  if (!status) {
    status = o_extend_interrogate (w_current, object_list);
    i_status_set_state(w_current, status);
  }
}

/*!
 * \brief Projection Mode Activated, Interrogate Selection
 * \par Function Description
 *  This function is called at the beginning of a project operation
 *  to determine how to proceed based on the number and type of objects
 *  selected.
 */
int o_extend_interrogate (GschemToplevel *w_current, GList *object_list)
{
  int count;
  int status;

  count = g_list_length(object_list);

  if (count == 0) {
    status = STARTEXTEND;     /* call o_extend_start on next click */
  }
  else if (count == 1) {      /* Can be boundary or a projectile */

    /* Call o_extend_end on next click if valid object selected */

    if (o_extend_is_valid_projectile(object_list->data)) {
      status = ENDEXTEND;
    }
    else if (o_extend_is_valid_bounder(object_list->data)) {
      status = EXTEND;
    }
    else {
      status = SELECT; /* Why not w_current->event_state*/
    }
  }
  else {
    if (o_extend_selection(w_current, count)) {
      status = ENDEXTEND;
    }
    else {
      status = SELECT;
    }
  }

  return status;
}

/** @} endgroup Extend-Operations */
