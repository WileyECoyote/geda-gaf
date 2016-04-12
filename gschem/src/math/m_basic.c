/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 51 Franklin Street, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include <math.h>
#include <geda_debug.h>

/*! \brief Convert x coordinate to mils
 *  \par Function Description
 *   Convert a x coordinate to mils.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] val        The x coordinate to convert
 *
 *  \return The coordinate value in mils.
 */
int mil_x (GschemToplevel *w_current, int val)
{
  int j;

  if ((w_current != NULL) &&
      (w_current->toplevel != NULL) &&
      (w_current->toplevel->page_current != NULL)) {

    int   left = w_current->toplevel->page_current->left;
    float x_constant =
          w_current->toplevel->page_current->to_world_x_constant;

    double fval;

    fval = val;

#ifdef HAVE_LRINT
    j = lrint(fval * x_constant + left);
#else
    j = (fval * x_constant + left) + 0.5;
#endif
  }
  else {
    fprintf(stderr, "ERROR! mil_x: detected NULL pointer\n");
    j = 0;
  }

  return(j);
}

/*! \brief Convert y coordinate to mils
 *  \par Function Description
 *   Convert a y coordinate to mils.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] val        The y coordinate to convert
 *
 *  \return The coordinate value in mils.
 */
int mil_y(GschemToplevel *w_current, int val)
{

  int j;
  if ((w_current != NULL) &&
      (w_current->toplevel != NULL) &&
      (w_current->toplevel->page_current != NULL)) {

    int   height = w_current->screen_height;
    int   top = w_current->toplevel->page_current->top;
    float y_constant =
          w_current->toplevel->page_current->to_world_y_constant;

    double fval;

    fval = height - val;

#ifdef HAVE_LRINT
    j = lrint(fval * y_constant + top);
#else
    j = (fval * y_constant + top) + 0.5;
#endif
  }
  else {
    fprintf(stderr, "ERROR! mil_y: detected NULL pointer\n");
    j = 0;
  }
  return(j);
}

/*! \brief Convert x coordinate to pixels
 *  \par Function Description
 *   Convert a x coordinate to pixels.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] val        The x coordinate to convert
 *
 *  \return The coordinate value in pixels.
 */
int pix_x (GschemToplevel *w_current, int val)
{
  int j;

  if ((w_current != NULL) &&
      (w_current->toplevel != NULL) &&
      (w_current->toplevel->page_current != NULL)) {

    float x_constant =
          w_current->toplevel->page_current->to_screen_x_constant;

    int   left = w_current->toplevel->page_current->left;

#ifdef HAVE_LRINT
    j = lrint(x_constant * (double)(val - left));
#else
    j = x_constant * (double)(val - left) + 0.5;
#endif

    /* this is a temp solution to fix the wrapping associated with */
    /* X coords being greater/less than than 2^15 */
    if (j >= 32768) {
      j = 32767;
    }
    else if (j <= -32768) {
      j = -32767;
    }
  }
  else {
    fprintf(stderr, "ERROR! pix_x: detected NULL pointer\n");
    j = 0;
  }

  return(j);
}

/*! \brief Convert a y coordinate to pixels
 *  \par Function Description
 *   Convert a y coordinate to pixels.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] val        The y coordinate to convert
 *
 *  \return The coordinate value in pixels.
 */
int pix_y(GschemToplevel *w_current, int val)
{
  int j;

  if ((w_current != NULL) &&
      (w_current->toplevel != NULL) &&
      (w_current->toplevel->page_current != NULL)) {

    float y_constant =
          w_current->toplevel->page_current->to_screen_y_constant;
    int   top = w_current->toplevel->page_current->top;
    int   height = w_current->screen_height;

#ifdef HAVE_LRINT
    j = lrint(height - (y_constant * (double)(val - top)));
#else
    j = (height - (y_constant * (double)(val - top))) + 0.5;
#endif

    /* This is a temp solution to fix the wrapping associated with */
    /* Y coords being greater/less than than 2^15 */
    if (j >= 32768) {
      j = 32767;
    }
    else if (j <= -32768) {
      j = -32767;
    }
  }
  else {
    fprintf(stderr, "ERROR! pix_y: detected NULL pointer\n");
    j = 0;
  }
  return(j);
}

/*! \brief Transform WORLD coordinates to SCREEN coordinates
 *  \par Function Description
 *   This function takes in WORLD x/y coordinates pair and
 *   transforms the values to SCREEN x/y coordinates.
 *
 *  \param [in]  w_current  The GschemToplevel object
 *  \param [in]  x          The x coordinate in WORLD units
 *  \param [in]  y          The y coordinate in WORLD units
 *  \param [out] px         The x coordinate in SCREEN units
 *  \param [out] py         The y coordinate in SCREEN units
 */
void WORLDtoSCREEN (GschemToplevel *w_current, int x, int y, int *px, int *py)
{

  *px = pix_x (w_current, x);
  *py = pix_y (w_current, y);
}

/*! \brief Transform SCREEN coordinates to WORLD coordinates
 *  \par Function Description
 *   This function takes in SCREEN x/y coordinates and
 *   transforms them to WORLD x/y coordinates.
 *
 *  \param [in]  w_current  The GschemToplevel object
 *  \param [in]  mx         The x coordinate in SCREEN units
 *  \param [in]  my         The y coordinate in SCREEN units
 *  \param [out] x          The x coordinate in WORLD units
 *  \param [out] y          The y coordinate in WORLD units
 *
 */
void SCREENtoWORLD (GschemToplevel *w_current, int mx, int my, int *x, int *y)
{
  *x = mil_x (w_current, mx);
  *y = mil_y (w_current, my);
}

/*! \brief Find the closest grid coordinate
 *  \par Function Description
 *   This function snaps the current input coordinate to the
 *   closest grid coordinate.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] input      The coordinate to snap
 *
 *  \return The closest grid coordinate to the input,
 *
 *  WEH: Tweeked to eliminate conditional, library call to abs
 *       and final multiplication instruction.
 */
int snap_grid(GschemToplevel *w_current, int input)
{
  int p, m, n;

  if (w_current->snap == SNAP_OFF || w_current->snap_size <= 0) {
    return(input);
  }

  /* WEH: This is equivalent to signed value = abs(input) but gets a
   *      sign mask instead of getting the sign using a conditional */
  register int value = input;
  register int sign  = value >> 31;     /* make a mask of the sign bit */

  value ^= sign;                        /* toggle the bits if value is negative */
  value -= sign;                        /* add one if value was negative */

  register int snap_grid = w_current->snap_size;

  p = value / snap_grid;
  m = value % snap_grid;
  n = p * snap_grid;
  if (m > snap_grid / 2) {
     n += snap_grid;
  }

#if DEBUG
  printf("p: %d\n", p);
  printf("m: %d\n", m);
  printf("m > snap_grid / 2: %d\n", (m > snap_grid / 2));
  printf("n: %d\n", n);
  printf("n*s: %d\n", n*sign); /* This won't work */
#endif

  /* Now put the sign back to what ever it was using the mask */
  n ^= sign;
  n -= sign;                        /* add one if value was negative */

  return n;
}

/*! \brief Get absolute SCREEN coordinate.
 *  \par Function Description
 *   Get absolute SCREEN coordinate.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] val        The coordinate to convert
 *
 *  \return The converted SCREEN coordinate.
 */
int SCREENabs(GschemToplevel *w_current, int val)
{
  if ((w_current == NULL) ||
      (w_current->toplevel == NULL) ||
      (w_current->toplevel->page_current == NULL))
    return 0;

  double f0,f1,f;

  int j;

  f0 = w_current->toplevel->page_current->left;
  f1 = w_current->toplevel->page_current->right;
  f  = w_current->screen_width / (f1 - f0);

#ifdef HAVE_LRINT
  j = lrint(f * (double)(val));
#else
  j = (f * (double)(val)) + 0.5;
#endif

  return(j);
}

/*! \brief Get absolute WORLD coordinate.
 *  \par Function Description
 *   Get absolute WORLD coordinate.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] val        The coordinate to convert
 *
 *  \return The converted WORLD coordinate.
 */
int WORLDabs(GschemToplevel *w_current, int val)
{
  double fw0,fw1,fw,fval;

  int j;

  fw1 = w_current->toplevel->page_current->right;
  fw0 = w_current->toplevel->page_current->left;
  fw  = w_current->screen_width;
  fval = val;

#ifdef HAVE_LRINT
  j = lrint(fval * (fw1 - fw0) / fw);
#else
  j = (fval * (fw1 - fw0) / fw) + 0.5;
#endif

  return(j);
}

/*! \brief */
typedef struct st_halfspace HALFSPACE;

/*! \brief */
struct st_halfspace {
  int left; /* these are booleans */
  int top;
  int right;
  int bottom;
};

/* \note
 * encode_halfspace and clip are part of the cohen-sutherland clipping
 * algorithm.  They are used to determine if an object is visible or not
 */
/*! \brief Encode WORLD coordinates as halfspace matrix.
 *  \par Function Description
 *   This function takes a point and checks if it is in the bounds of the
 *   current GedaToplevel object's page coordinates. It handles points with WORLD coordinates.
 *
 *  \param [in]  w_current  The GschemToplevel object.
 *  \param [in]  point      The point in WORLD coordinates to be checked.
 *  \param [out] halfspace  The created HALFSPACE structure.
 *
 *  \warning halfspace must be allocated before this function is called
 */
static void WORLDencode_halfspace (GschemToplevel *w_current,
                                   POINT *point, HALFSPACE *halfspace)
{
  halfspace->left   = point->x < w_current->toplevel->page_current->left;
  halfspace->right  = point->x > w_current->toplevel->page_current->right;
  halfspace->bottom = point->y > w_current->toplevel->page_current->bottom;
  halfspace->top    = point->y < w_current->toplevel->page_current->top;
}

/*! \brief Calculate the cliping region for a set of coordinates.
 *  \par Function Description
 *  This function will check the provided set of coordinates to see if
 *  they fall within a clipping region.  If they do the coordinates will
 *  be changed to reflect only the region no covered by the clipping window.
 *  All coordinates should be in WORLD units.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in,out] x1     x coordinate of the first screen point
 *  \param [in,out] y1     y coordinate of the first screen point
 *  \param [in,out] x2     x coordinate of the second screen point
 *  \param [in,out] y2     y coordinate of the second screen point
 *
 *  \return TRUE if coordinates are now visible, FALSE otherwise.
 */
int WORLDclip_change (GschemToplevel *w_current,
                      int *x1, int *y1, int *x2, int *y2)
{
  HALFSPACE half1, half2;
  HALFSPACE tmp_half;
  POINT tmp_point;
  POINT point1, point2;
  float slope;
  int done;
  int visible;
  int w_l, w_t, w_r, w_b;

  point1.x = *x1;
  point1.y = *y1;
  point2.x = *x2;
  point2.y = *y2;

  w_l = w_current->toplevel->page_current->left;
  w_t = w_current->toplevel->page_current->top;
  w_r = w_current->toplevel->page_current->right;
  w_b = w_current->toplevel->page_current->bottom;

  done = FALSE;
  visible = FALSE;

  do {

    int in1, in2;

    WORLDencode_halfspace (w_current, &point1, &half1);
    WORLDencode_halfspace (w_current, &point2, &half2);

#if DEBUG
    printf("starting loop\n");
    printf("1 %d %d %d %d\n", half1.left, half1.top, half1.right, half1.bottom);
    printf("2 %d %d %d %d\n", half2.left, half2.top, half2.right, half2.bottom);
#endif

    in1 = (!half1.left) &&
      (!half1.top) &&
      (!half1.right) &&
      (!half1.bottom);

    in2 = (!half2.left) &&
      (!half2.top) &&
      (!half2.right) &&
      (!half2.bottom);


    if (in1 && in2) { /* trivally accept */
      done = TRUE;
      visible = TRUE;
    } else if ( ((half1.left && half2.left) ||
                 (half1.right && half2.right)) ||
                ((half1.top && half2.top) ||
                 (half1.bottom && half2.bottom)) ) {
      done = TRUE; /* trivially reject */
      visible = FALSE;
    } else { /* at least one point outside */
      if (in1) {
        tmp_half = half1;
        half1 = half2;
        half2 = tmp_half;

        tmp_point = point1;
        point1 = point2;
        point2 = tmp_point;
      }

      if (point2.x == point1.x) { /* vertical line */
        if (half1.top) {
          point1.y = w_t;
        } else if (half1.bottom) {
          point1.y = w_b;
        }
      } else { /* not a vertical line */

        /* possible fix for alpha core dumping */
        /* assume the object is visible */
        if ((point2.x - point1.x) == 0) {
          return(TRUE);
        }

        slope = (float) (point2.y - point1.y) /
          (float) (point2.x - point1.x);

        /* possible fix for alpha core dumping */
        /* assume the object is visible */
        if (slope == 0.0) {
          return(TRUE);
        }

        if (half1.left) {
          point1.y = point1.y +
            (w_l - point1.x) * slope;
          point1.x = w_l;
        } else if (half1.right) {
          point1.y = point1.y +
            (w_r - point1.x) * slope;
          point1.x = w_r;
        } else if (half1.bottom) {
          point1.x = point1.x +
            (w_b - point1.y) / slope;
          point1.y = w_b;
        } else if (half1.top) {
          point1.x = point1.x +
            (w_t - point1.y) / slope;
          point1.y = w_t;
        }
      } /* end of not a vertical line */
    } /* end of at least one outside */
  } while (!done);

  /*printf("after: %d %d %d %d\n", point1.x, point1.y, point2.x, point2.y);*/
  *x1 = point1.x;
  *y1 = point1.y;
  *x2 = point2.x;
  *y2 = point2.y;
  return(visible);
}

/*! \brief Check if a set of coordinates are within a clipping region
 *  \par Function Description
 *  This function will check if the given set of coordinates
 *  are within a clipping region. No action will be taken to change
 *  the coordinates.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in,out] x1     x coordinate of the first screen point.
 *  \param [in,out] y1     y coordinate of the first screen point.
 *  \param [in,out] x2     x coordinate of the second screen point.
 *  \param [in,out] y2     y coordinate of the second screen point.
 *  \return TRUE if coordinates are now visible, FALSE otherwise.
 */
int clip_nochange (GschemToplevel *w_current, int x1, int y1, int x2, int y2)
{
  HALFSPACE half1, half2;
  HALFSPACE tmp_half;
  POINT tmp_point;
  POINT point1, point2;
  float slope;
  int done;
  int visible;
  int w_l, w_t, w_r, w_b;

  point1.x = x1;
  point1.y = y1;
  point2.x = x2;
  point2.y = y2;

  /*printf("before: %d %d %d %d\n", x1, y1, x2, y2);*/

  w_l = w_current->toplevel->page_current->left;
  w_t = w_current->toplevel->page_current->top;
  w_r = w_current->toplevel->page_current->right;
  w_b = w_current->toplevel->page_current->bottom;

  done = FALSE;
  visible = FALSE;

  do {

    int in1, in2;

    WORLDencode_halfspace (w_current, &point1, &half1);
    WORLDencode_halfspace (w_current, &point2, &half2);

#if DEBUG
    printf("starting loop\n");
    printf("1 %d %d %d %d\n", half1.left, half1.top, half1.right, half1.bottom);
    printf("2 %d %d %d %d\n", half2.left, half2.top, half2.right, half2.bottom);
#endif

    in1 = (!half1.left) &&
      (!half1.top) &&
      (!half1.right) &&
      (!half1.bottom);

    in2 = (!half2.left) &&
      (!half2.top) &&
      (!half2.right) &&
      (!half2.bottom);


    if (in1 && in2) { /* trivally accept */
      done = TRUE;
      visible = TRUE;
    } else if ( ((half1.left && half2.left) ||
                 (half1.right && half2.right)) ||
                ((half1.top && half2.top) ||
                 (half1.bottom && half2.bottom)) ) {
      done = TRUE; /* trivially reject */
      visible = FALSE;
    } else { /* at least one point outside */
      if (in1) {
        tmp_half = half1;
        half1 = half2;
        half2 = tmp_half;

        tmp_point = point1;
        point1 = point2;
        point2 = tmp_point;
      }

      if (point2.x == point1.x) { /* vertical line */
        if (half1.top) {
          point1.y = w_t;
        } else if (half1.bottom) {
          point1.y = w_b;
        }
      } else { /* not a vertical line */

        /* possible fix for alpha core dumping */
        /* assume the object is visible */
        if ((point2.x - point1.x) == 0) {
          return(TRUE);
        }

        slope = (float) (point2.y - point1.y) /
          (float) (point2.x - point1.x);

        /* possible fix for alpha core dumping */
        /* assume the object is visible */
        if (slope == 0.0) {
          return(TRUE);
        }

        if (half1.left) {
          point1.y = point1.y +
            (w_l - point1.x) * slope;
          point1.x = w_l;
        } else if (half1.right) {
          point1.y = point1.y +
            (w_r - point1.x) * slope;
          point1.x = w_r;
        } else if (half1.bottom) {
          point1.x = point1.x +
            (w_b - point1.y) / slope;
          point1.y = w_b;
        } else if (half1.top) {
          point1.x = point1.x +
            (w_t - point1.y) / slope;
          point1.y = w_t;
        }
      } /* end of not a vertical line */
    } /* end of at least one outside */
  } while (!done);

  return(visible);
}

/*! \brief Check if a bounding box is visible on the screen.
 *  \par Function Description
 *  This function checks if a given bounding box is visible on the screen.
 *
 *  WARNING: top and bottom are mis-named in world-coords,
 *  top is the smallest "y" value, and bottom is the largest.
 *  Be careful! This doesn't correspond to what you'd expect.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] wleft      Left coordinate of the bounding box.
 *  \param [in] wtop       Top coordinate of the bounding box.
 *  \param [in] wright     Right coordinate of the bounding box.
 *  \param [in] wbottom    Bottom coordinate of the bounding box.
 *  \return TRUE if bounding box is visible, FALSE otherwise
 */
int visible (GschemToplevel *w_current,
             int wleft, int wtop, int wright, int wbottom)
{
  int visible=FALSE;

  /* don't do object clipping if this is false */
  if (!w_current->object_clipping) {
    return(TRUE);
  }

  visible = clip_nochange (w_current, wleft, wtop, wright, wtop);

#if DEBUG
  printf("vis1 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange (w_current, wleft, wbottom, wright, wbottom);
  } else {
    return(visible);
  }

#if DEBUG
  printf("vis2 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange (w_current, wleft, wtop, wleft, wbottom);
  } else {
    return(visible);
  }

#if DEBUG
  printf("vis3 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange (w_current, wright, wtop, wright, wbottom);
  } else {
    return(visible);
  }

#if DEBUG
  printf("vis4 %d\n", visible);
#endif

#if DEBUG
  printf("%d %d %d\n", wleft, w_current->toplevel->page_current->top, wright);
  printf("%d %d %d\n", wtop, w_current->toplevel->page_current->top, wbottom);
  printf("%d %d %d\n", wleft, w_current->toplevel->page_current->right, wright);
  printf("%d %d %d\n", wtop, w_current->toplevel->page_current->bottom, wbottom);
#endif

  /*
   * now check to see if bounding box encompasses the entire viewport.
   * We only need to test if one point on the screen clipping boundary
   * is indide the bounding box of the object.
   */
  if (w_current->toplevel->page_current->left >= wleft  &&
      w_current->toplevel->page_current->left <= wright &&
      w_current->toplevel->page_current->top >= wtop    &&
      w_current->toplevel->page_current->top <= wbottom ) {
    visible = 1;
  }

#if DEBUG
  printf("vis5 %d\n", visible);
#endif

  return(visible);
}


/*! \brief Rounds numbers by a power of 10.
 *  \par Function Description
 *  This function will round numbers using a power of 10 method.
 *  For example:
 *                1235 rounds to 1000
 *                 670 rounds to  500
 *               0.234 rounds to  0.2
 *  integer values would be enough if there are no numbers smaller than 1 (hw)
 *
 *  \param [in] unrounded  The number to be rounded.
 *  \return The rounded number.
 */
/* rounds for example 1235 to 1000, 670 to 500, 0.234 to 0.2 ...
int would be enough if there are no numbers smaller 1 (hw)*/
double m_round_5_2_1(double unrounded)
{
  int digits;
  double betw_1_10;

  /*only using the automatic cast */
  digits = log10(unrounded);

  /* creates numbers between 1 and 10 */

  betw_1_10 = unrounded / pow(10,digits);

  if (betw_1_10 < 1.5) {
    return(pow(10,digits));
  }
  if (betw_1_10 > 1.4 && betw_1_10 < 3.5 ) {
    return(2*pow(10,digits));
  }
  if (betw_1_10 > 3.4 && betw_1_10 < 7.5 ) {
    return(5*pow(10,digits));
  }
  else {
    return(10*pow(10,digits));
  }

}
