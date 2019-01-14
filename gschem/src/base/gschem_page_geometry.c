/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 Ales Hvezda
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Contributing Author: Edward Hennessy
 * Date Contributed: November 16th, 2013
 */
/*!
 * \file gschem_page_geometry.c
 *
 * \brief
 */

#include <gschem.h>
#include <math.h>
#include <geda_debug.h>

/** \defgroup Gschem-Page-Geometry Gschem Page Geometry
 * @{
 * \brief #GschemPageGeometry Class Implmentation
 * \par
 *  This module implements a page geometry class in gschem.
 */

/* Function Prototype */

static void
update_constants (GschemPageGeometry *geometry);

/*!
 * \brief Copy a page geometry
 * \par Function Description
 * \param [in] geometry The page geometry to copy
 * \return An dynamically allocated copy of the geometry
 */
GschemPageGeometry*
gschem_page_geometry_copy (GschemPageGeometry *geometry)
{
  return (GschemPageGeometry*) g_memdup (geometry, sizeof(GschemPageGeometry));
}

/*!
 * \brief Free a page geometry
 * \par Function Description
 * \param [in] geometry The page geometry to free
 */
void
gschem_page_geometry_free (GschemPageGeometry *geometry)
{
  GEDA_FREE (geometry);
}

/*!
 * \brief Get the screen height in pixels.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The screen height in pixels.
 */
int
gschem_page_geometry_get_screen_height (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->screen_height;
}

/*!
 * \brief Get the screen width in pixels.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The screen width in pixels.
 */
int
gschem_page_geometry_get_screen_width (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->screen_width;
}

/*!
 * \brief Get/register the GschemPageGeometry type.
 * \par Function Description
 * \return The GschemPageGeometry type.
 */
GedaType
gschem_page_geometry_get_type (void)
{
  static GedaType type = 0;

  if (type == 0) {
    type = g_boxed_type_register_static ("GschemPageGeometry",
                                         (GBoxedCopyFunc) gschem_page_geometry_copy,
                                         (GBoxedFreeFunc) gschem_page_geometry_free);
  }

  return type;
}

/*!
 * \brief Get the bottom edge of the viewport in world coordinates.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The bottom edge of the viewport in world coordinates.
 */
int
gschem_page_geometry_get_viewport_bottom (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->viewport_bottom;
}

/*!
 * \brief Get the left edge of the viewport in world coordinates.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The left edge of the viewport in world coordinates.
 */
int
gschem_page_geometry_get_viewport_left (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->viewport_left;
}

/*!
 * \brief Get the right edge of the viewport in world coordinates.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The right edge of the viewport in world coordinates.
 */
int
gschem_page_geometry_get_viewport_right (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->viewport_right;
}

/*!
 * \brief Get the top edge of the viewport in world coordinates.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The top edge of the viewport in world coordinates.
 */
int
gschem_page_geometry_get_viewport_top (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->viewport_top;
}

/*!
 * \brief Get the top edge of the world in world coordinates.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The top edge of the world in world coordinates.
 */
int
gschem_page_geometry_get_world_bottom (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->world_top;
}

/*!
 * \brief Get the top edge of the world in world coordinates.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The top edge of the world in world coordinates.
 */
int
gschem_page_geometry_get_world_left (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->world_top;
}

/*!
 * \brief Get the top edge of the world in world coordinates.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The top edge of the world in world coordinates.
 */
int
gschem_page_geometry_get_world_right (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->world_top;
}

/*!
 * \brief Get the world to screen transformation matrix.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 * \return The world to screen transformation matrix.
 */
cairo_matrix_t*
gschem_page_geometry_get_world_to_screen_matrix (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, NULL);

  if (!geometry->world_to_screen_calculated) {
    cairo_matrix_init (&(geometry->world_to_screen_matrix),
                       (double) geometry->to_screen_x_constant,
                       0,
                       0,
                       - (double) geometry->to_screen_y_constant,
                       (- (double) geometry->viewport_left * geometry->to_screen_x_constant),
                       ((double) geometry->to_screen_y_constant * geometry->viewport_top + geometry->screen_height));

    geometry->world_to_screen_calculated = TRUE;
  }

  return &(geometry->world_to_screen_matrix);
}

/*!
 * \brief Get the top edge of the world in world coordinates.
 * \par Function Description
 * \param [in] geometry The GschemPageGeometry
 *
 * \return The top edge of the world in world coordinates.
 */
int
gschem_page_geometry_get_world_top (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->world_top;
}

/*!
 * \brief Convert a x coordinate to mils.
 * \par Function Description
 *  Convert a x coordinate to mils.
 *
 * \param [in] geometry   The GschemToplevel object
 * \param [in] value      The x coordinate to convert
 *
 * \return The coordinate value in mils.
 */
int
gschem_page_geometry_mil_x (GschemPageGeometry *geometry, int value)
{
  double i;
  double fval;
  int j;

  g_return_val_if_fail (geometry != NULL, 0);

  fval = value;
  i = fval * geometry->to_world_x_constant + geometry->viewport_left;

#ifdef HAVE_LRINT
  j = lrint(i);
#else
  j = i;
#endif

  return(j);
}

/*!
 * \brief Convert a y coordinate to mils
 * \par Function Description
 *  Convert a y coordinate to mils
 *
 * \param [in] geometry   The GschemToplevel object.
 * \param [in] value      The y coordinate to convert.
 *
 * \return The coordinate value in mils.
 */
int
gschem_page_geometry_mil_y(GschemPageGeometry *geometry, int value)
{
  double i;
  double fval;
  int j;

  g_return_val_if_fail (geometry != NULL, 0);

  fval = geometry->screen_height - value;
  i = fval * geometry->to_world_y_constant + geometry->viewport_top;

#ifdef HAVE_LRINT
  j = lrint(i);
#else
  j = i;
#endif

  return(j);
}

/*!
 * \brief Get page geometry for this view with way to many parameters
 * \par Function Description
 * \param [in] screen_width
 * \param [in] screen_height
 * \param [in] viewport_left
 * \param [in] viewport_top
 * \param [in] viewport_right
 * \param [in] viewport_bottom
 * \param [in] world_left
 * \param [in] world_top
 * \param [in] world_right
 * \param [in] world_bottom
 *
 * \return The page for the view
 */
GschemPageGeometry*
gschem_page_geometry_new_with_values (int screen_width,
                                      int screen_height,
                                      int viewport_left,
                                      int viewport_top,
                                      int viewport_right,
                                      int viewport_bottom,
                                      int world_left,
                                      int world_top,
                                      int world_right,
                                      int world_bottom)
{
  GschemPageGeometry *geometry = GEDA_MEM_ALLOC0 (sizeof(GschemPageGeometry));

  gschem_page_geometry_set_values (geometry,
                                   screen_width,
                                   screen_height,
                                   viewport_left,
                                   viewport_top,
                                   viewport_right,
                                   viewport_bottom);

  geometry->world_left   = world_left;
  geometry->world_top    = world_top;
  geometry->world_right  = world_right;
  geometry->world_bottom = world_bottom;

  return geometry;
}

/*!
 * \brief Pan and zoom the viewport
 * \par Function Description
 * \param [in,out] geometry
 * \param [in]     world_cx
 * \param [in]     world_cy
 * \param [in]     relativ_zoom_factor
 * \param [in]     flags
 */
void
gschem_page_geometry_pan_general(GschemPageGeometry *geometry,
                                 double world_cx,
                                 double world_cy,
                                 double relativ_zoom_factor,
                                 int flags)
{
  /* see libgeda/include/defines.h for flags */
  /*if the borders should be ignored always, remove, outcomment or changes
    the flags in the function-calls*/
  /*	flags |= I_PAN_IGNORE_BORDERS; */
  /* think it's better that the zoomfactor is defined as pix/mils
     this will be the same as w_current->page_current->to_screen_x/y_constant*/


  double zx, zy, zoom_old, zoom_new, zoom_min;

  g_return_if_fail (geometry != NULL);

#if DEBUG
  printf("i_pan_world_general(): world_cx=%f, world_cy=%f\n",world_cx, world_cy);
#endif

  /* calc minimum zoomfactors and choose the smaller one. They are equal
     if the aspectratio of the world is the same as the screen ratio */
  zx = (double) geometry->screen_width  / (geometry->world_right - geometry->world_left);
  zy = (double) geometry->screen_height / (geometry->world_bottom - geometry->world_top);
  zoom_min = zx < zy ? zx : zy;

#if DEBUG
  printf("  zx_min=%f, zy_min=%f , flags=%d\n ",zx, zy, flags);
#endif

  /* to_screen_x_constant and to_screen_y_constant are almost the same.
     lets use to_screen_y_constant */
  zoom_old = geometry->to_screen_y_constant;

  /* calc new zooming factor */
  /* check if there's a zoom_full (relativ_zoom_factor == -1) */
  if (relativ_zoom_factor <0)  {
    zoom_new = zoom_min;
  }
  else {

    int zoom_max = MAX_ZOOM_FACTOR;

    zoom_new = zoom_old * relativ_zoom_factor;
    zoom_new = zoom_new > zoom_max ? zoom_max : zoom_new;
    if (!(flags & I_PAN_IGNORE_BORDERS)) {
      zoom_new = zoom_new < zoom_min ? zoom_min : zoom_new;
    }
  }

  /* calculate the new visible area; adding 0.5 to round */
  geometry->viewport_left   = world_cx - (double) geometry->screen_width / 2 / zoom_new + 0.5;
  geometry->viewport_right  = world_cx + (double) geometry->screen_width / 2 / zoom_new + 0.5;
  geometry->viewport_top    = world_cy - (double) geometry->screen_height / 2 / zoom_new + 0.5;
  geometry->viewport_bottom = world_cy + (double) geometry->screen_height / 2 / zoom_new + 0.5;

  /* and put it back to the borders */
  if (!(flags & I_PAN_IGNORE_BORDERS)) {

    int diff;

    /* check right border */
    if (geometry->viewport_right > geometry->world_right) {
      geometry->viewport_left += geometry->world_right - geometry->viewport_right;
      geometry->viewport_right = geometry->world_right;
    }
    /* check left border */
    if (geometry->viewport_left < geometry->world_left) {
      geometry->viewport_right += geometry->world_left - geometry->viewport_left;
      geometry->viewport_left = geometry->world_left;
    }

    /* If there is any slack, center the view */
    diff = (geometry->viewport_right - geometry->viewport_left) - (geometry->world_right - geometry->world_left);
    if (diff > 0) {
      geometry->viewport_left -= diff / 2;
      geometry->viewport_right -= diff / 2;
    }

    /* check bottom border */
    if (geometry->viewport_bottom > geometry->world_bottom) {
      geometry->viewport_top += geometry->world_bottom - geometry->viewport_bottom;
      geometry->viewport_bottom = geometry->world_bottom;
    }
    /* check top border */
    if (geometry->viewport_top < geometry->world_top) {
      geometry->viewport_bottom += geometry->world_top - geometry->viewport_top;
      geometry->viewport_top = geometry->world_top;
    }

    /* If there is any slack, center the view */
    diff = (geometry->viewport_bottom - geometry->viewport_top) - (geometry->world_bottom - geometry->world_top);
    if (diff > 0) {
      geometry->viewport_top -= diff / 2;
      geometry->viewport_bottom -= diff / 2;
    }
  }
}

/*!
 * \brief Convert a x coordinate to pixels.
 * \par Function Description
 * \param [in] geometry The page geometry
 * \param [in] value    The x coordinate in mils
 *
 * \return The x coordinate in pixels
 */
int
gschem_page_geometry_pix_x (GschemPageGeometry *geometry, int value)
{
  double i;
  int j;

  g_return_val_if_fail (geometry != NULL, 0);

  i = geometry->to_screen_x_constant * (double)(value - geometry->viewport_left);

#ifdef HAVE_LRINT
  j = lrint(i);
#else
  j = i;
#endif

  /* this is a temp solution to fix the wrapping associated with */
  /* X coords being greater/less than than 2^15 */
  if (j >= 32768) {
    j = 32767;
  }
  else if (j <= -32768) {
    j = -32767;
  }

  return(j);
}

/*!
 * \brief Convert a y coordinate to pixels.
 * \par Function Description
 * \param [in] geometry The page geometry
 * \param [in] value    The y coordinate in mils
 *
 * \return The y coordinate in pixels
 */
int
gschem_page_geometry_pix_y (GschemPageGeometry *geometry, int value)
{
  double i;
  int j;

  g_return_val_if_fail (geometry != NULL, 0);

  i = geometry->screen_height - (geometry->to_screen_y_constant * (double)(value - geometry->viewport_top));

#ifdef HAVE_LRINT
  j = lrint(i);
#else
  j = i;
#endif

  /* this is a temp solution to fix the wrapping associated with */
  /* X coords being greater/less than than 2^15 */
  if (j >= 32768) {
    j = 32767;
  }
  else if (j <= -32768) {
    j = -32767;
  }

  return(j);
}

/*!
 * \brief Set the screen height in pixels
 * \par Function Description
 * \param [in,out] geometry      The GschemPageGeometry
 * \param [in]     screen_height The screen height in pixels.
 */
void
gschem_page_geometry_set_screen_height (GschemPageGeometry *geometry, int screen_height)
{
  g_return_if_fail (geometry != NULL);

  geometry->screen_height = screen_height;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}

/*!
 * \brief Set the screen width in pixels
 * \par Function Description
 * \param [in,out] geometry     The GschemPageGeometry
 * \param [in]     screen_width The screen width in pixels.
 */
void
gschem_page_geometry_set_screen_width (GschemPageGeometry *geometry, int screen_width)
{
  g_return_if_fail (geometry != NULL);

  geometry->screen_width = screen_width;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}

/*!
 * \brief Get page geometry for this view
 * \par Function Description
 * \param [in] geometry          A Gschem geometry Object
 * \param [in] screen_width
 * \param [in] screen_height
 * \param [in] viewport_left
 * \param [in] viewport_top
 * \param [in] viewport_right
 * \param [in] viewport_bottom
 *
 * \return The page for the view
 */
void
gschem_page_geometry_set_values (GschemPageGeometry *geometry,
                                 int screen_width,
                                 int screen_height,
                                 int viewport_left,
                                 int viewport_top,
                                 int viewport_right,
                                 int viewport_bottom)
{
  g_return_if_fail (geometry != NULL);
  g_return_if_fail (screen_width > 0);
  g_return_if_fail (screen_height > 0);
  g_return_if_fail (viewport_left != viewport_right);
  g_return_if_fail (viewport_top != viewport_bottom);

  geometry->screen_width  = screen_width;
  geometry->screen_height = screen_height;

  geometry->viewport_left   = MIN (viewport_left, viewport_right);
  geometry->viewport_top    = MIN (viewport_top, viewport_bottom);
  geometry->viewport_right  = MAX (viewport_left, viewport_right);
  geometry->viewport_bottom = MAX (viewport_top, viewport_bottom);

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}

/*!
 * \brief Set the bottom edge of the viewport in world coordinates
 * \par Function Description
 * \param [in,out] geometry The GschemPageGeometry
 * \param [in] viewport_bottom The bottom edge of the viewport in world coordinates.
 */
void
gschem_page_geometry_set_viewport_bottom (GschemPageGeometry *geometry, int viewport_bottom)
{
  g_return_if_fail (geometry != NULL);

  geometry->viewport_bottom = viewport_bottom;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}

/*!
 * \brief Set the left edge of the viewport in world coordinates
 * \par Function Description
 * \param [in,out] geometry The GschemPageGeometry
 * \param [in] viewport_left The left edge of the viewport in world coordinates.
 */
void
gschem_page_geometry_set_viewport_left (GschemPageGeometry *geometry, int viewport_left)
{
  g_return_if_fail (geometry != NULL);

  geometry->viewport_left = viewport_left;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}

/*!
 * \brief Set the right edge of the viewport in world coordinates
 * \par Function Description
 * \param [in,out] geometry The GschemPageGeometry
 * \param [in] viewport_right The right edge of the viewport in world coordinates.
 */
void
gschem_page_geometry_set_viewport_right (GschemPageGeometry *geometry, int viewport_right)
{
  g_return_if_fail (geometry != NULL);

  geometry->viewport_right = viewport_right;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}

/*!
 * \brief Set the top edge of the viewport in world coordinates
 * \par Function Description
 * \param [in,out] geometry The GschemPageGeometry
 * \param [in] viewport_top The top edge of the viewport in world coordinates.
 */
void
gschem_page_geometry_set_viewport_top (GschemPageGeometry *geometry, int viewport_top)
{
  g_return_if_fail (geometry != NULL);

  geometry->viewport_top = viewport_top;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}

/*!
 * \brief Zoom the viewport to the extents of the given objects
 * \par Function Description
 * \param [in,out] geometry  This GschemPageGeometry
 * \param [in]     list      The list of object to zoom extents
 * \param [in]     pan_flags
 */
void
gschem_page_geometry_zoom_extents (GschemPageGeometry *geometry, const GList *list, int pan_flags)
{
  int lleft, lright, ltop, lbottom;
  double zx, zy, relativ_zoom_factor;
  double world_pan_center_x,world_pan_center_y;

  g_return_if_fail (geometry != NULL);

  if (list == NULL) {
    return;
  }

  if (!geda_object_get_bounds_list (list, &lleft, &ltop, &lright, &lbottom)) {
    return;
  }

#if DEBUG
  printf("in i_zoom_world_extents:  left: %d, right: %d, top: %d, bottom: %d\n",
         lleft, lright, ltop, lbottom);
#endif

  /* Calc the necessary zoomfactor to show everything
   * Start with the windows width and height (minus a small padding in pixels),
   * then scale back to world coordinates with the to_screen_y_constant as the
   * initial page data may not have the correct aspect ratio. */
  zx = (double)(geometry->screen_width - 2 * ZOOM_EXTENTS_PADDING_PX) / (lright-lleft);
  zy = (double)(geometry->screen_height - 2 * ZOOM_EXTENTS_PADDING_PX) / (lbottom-ltop);

  /* choose the smaller one */
  relativ_zoom_factor = (zx < zy ? zx : zy) / geometry->to_screen_y_constant;

  /* get the center of the objects */
  world_pan_center_x = (double) (lright + lleft) / 2.0;
  world_pan_center_y = (double) (lbottom + ltop) / 2.0;

  /* and create the new window */
  gschem_page_geometry_pan_general (geometry,
                                    world_pan_center_x,
                                    world_pan_center_y,
                                    relativ_zoom_factor,
                                    pan_flags);
}

/*!
 * \brief Update the constants (coefficients) for calculations
 * \par Function Description
 * \param [in,out] geometry The GschemPageGeometry
 */
static void
update_constants (GschemPageGeometry *geometry)
{
  /* now do the constant setups */

  /* pix_x */
  geometry->to_screen_x_constant = (double)geometry->screen_width / (double)(geometry->viewport_right - geometry->viewport_left);

  /* pix_y */
  geometry->to_screen_y_constant = (double)geometry->screen_height / (double)(geometry->viewport_bottom - geometry->viewport_top);

  /* mil_x */
  geometry->to_world_x_constant = (double)(geometry->viewport_right - geometry->viewport_left) / (double)geometry->screen_width;

  /* mil_y */
  geometry->to_world_y_constant = (double)(geometry->viewport_bottom - geometry->viewport_top) / (double)geometry->screen_height;
}

/** @} endgroup Gschem-Page-Geometry */
