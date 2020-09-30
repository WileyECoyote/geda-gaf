/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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

/*! \file o_bus_object.c
 *  \brief functions for the bus object
 */

/** \defgroup geda-bus-object-proc GedaBus Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaBus Objects
 */

#include "../../../config.h"

#include <stdio.h>

#include <libgeda_priv.h>

static int  geda_bus_object_consolidate_segments (GedaObject *object) __attribute__((unused));
static void geda_bus_object_consolidate_lowlevel (GedaObject *object,  GedaObject *del_object, int orient) __attribute__((unused));

static void geda_object_error(const char *func, const void *object, IDE_OBJECT_TYPE type)
{
  geda_error_object_argument(__FILE__, func, object, type);
}

static void geda_bus_object_error(const char *func, const void *object)
{
  geda_object_error(func, object, GEDA_OBJECT_BUS);
}

/* \brief
 * \par Function Description
 * This function does the actual work of making one bus segment out of two
 * connected segments.
 * The second object (del_object) is the object that should be deleted.
 *
 * \todo This function is currently not used. Check it before using it
 */
static void
geda_bus_object_consolidate_lowlevel (GedaObject *object,
                                      GedaObject *del_object, int orient)
{
  int temp1, temp2;
  int final1, final2;
  int changed = 0;

#if DEBUG
  printf("o %d %d %d %d\n", object->line->x[0], object->line->y[0], object->line->x[1], object->line->y[1]);
  printf("d %d %d %d %d\n", del_object->line->x[0], del_object->line->y[0], del_object->line->x[1], del_object->line->y[1]);
#endif

  if (orient == HORIZONTAL) {

    temp1 = min(object->line->x[0], del_object->line->x[0]);
    temp2 = min(object->line->x[1], del_object->line->x[1]);

    final1 = min(temp1, temp2);

    temp1 = max(object->line->x[0], del_object->line->x[0]);
    temp2 = max(object->line->x[1], del_object->line->x[1]);

    final2 = max(temp1, temp2);

    object->line->x[0] = final1;
    object->line->x[1] = final2;
    changed=1;
  }

  if (orient == VERTICAL) {
    temp1 = min(object->line->y[0], del_object->line->y[0]);
    temp2 = min(object->line->y[1], del_object->line->y[1]);

    final1 = min(temp1, temp2);

    temp1 = max(object->line->y[0], del_object->line->y[0]);
    temp2 = max(object->line->y[1], del_object->line->y[1]);

    final2 = max(temp1, temp2);

    object->line->y[0] = final1;
    object->line->y[1] = final2;
    changed=1;
  }

#if DEBUG
  printf("fo %d %d %d %d\n", object->line->x[0], object->line->y[0], object->line->x[1], object->line->y[1]);
#endif

  /* Move any attributes from the deleted object*/
  if (changed && del_object->attribs != NULL) {

    /* Reassign the attached_to pointer on attributes from the del object */
    GList *a_iter = del_object->attribs;

    while (a_iter != NULL) {

      GedaObject *a_current = a_iter->data;

      a_current->attached_to = object;

      a_iter = g_list_next (a_iter);
    }

    object->attribs = g_list_concat (object->attribs, del_object->attribs);

    /* Don't free del_object->attribs as it's relinked into object's list */
    del_object->attribs = NULL;
  }
}

/* \brief
 * \par Function Description
 *
 * \todo Not Implemented Yet
 */
static int geda_bus_object_consolidate_segments (GedaObject *object)
{
  return(0);
}

/* \brief
 * \par Function Description
 *
 * \todo Not Implemented Yet
 */
void geda_bus_object_consolidate(GedaToplevel *toplevel, Page *page)
{
  const GList *iter;
  int status = 0;

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (page != NULL);

  iter = geda_struct_page_get_objects (page);

  while (iter != NULL) {

    GedaObject *o_current = (GedaObject *)iter->data;

    if (o_current->type == OBJ_BUS) {
      status = geda_bus_object_consolidate_segments(o_current);
    }

    if (status == -1) {
      iter = geda_struct_page_get_objects (page);
      status = 0;
    } else {
      iter = g_list_next (iter);
    }
  }
}

/*! O0502
 * \brief create a copy of a bus object
 * \par Function Description
 *  This function creates a copy of the bus object \a o_current.
 *
 * \param [in] o_current    The object that is copied
 *
 * \return a new bus object
 */
GedaObject *geda_bus_object_copy(const GedaObject *o_current)
{
  if (GEDA_IS_BUS(o_current)) {

    GedaObject *new_obj;

    new_obj = geda_bus_object_new (o_current->color,
                                   o_current->line->x[0], o_current->line->y[0],
                                   o_current->line->x[1], o_current->line->y[1],
                                   o_current->bus->ripper_direction);

    new_obj->line_options->line_width = *o_current->bus->line_width;

    return new_obj;
  }

  geda_bus_object_error(__func__, o_current);

  return NULL;
}

/*! O0503
 * \brief Get the direction of a bus
 * \par Function Description
 *  1 for right, -1 for left (horizontal bus) or
 *  1 for up, -1 for down (vertical bus).
 */
int geda_bus_object_get_direction(const GedaObject *object)
{
  int direction = 0;

  if (GEDA_IS_BUS(object)) {

    if (object->line->x[1] > object->line->x[0] &&
      object->line->y[1] == object->line->y[0] )
    {
      direction = 1;
    }
    else if (object->line->x[0] > object->line->x[1] &&
      object->line->y[1] == object->line->y[0])
    {
      direction = -1;
    }
    else if (object->line->x[0] == object->line->x[1] &&
      object->line->y[1] > object->line->y[0])
    {
      direction = 1;
    }
    else if (object->line->x[0] == object->line->x[1] &&
      object->line->y[0] > object->line->y[1])
    {
      direction = -1;
    }
  }
  else {
    geda_object_error(__func__, object, GEDA_OBJECT_BUS);
  }

  return direction;
}

/*! O0504
 * \brief Get the Bus ripper direction
 * \par Function Description
 *  This function gets returns the Bus ripper direction
 * \param [in] object The bus object
 * \return The ripper direction
 */
int geda_bus_object_get_ripper_direction (const GedaObject *object)
{
  if (GEDA_IS_BUS(object)) {
    return object->bus->ripper_direction;
  }

  geda_bus_object_error(__func__, object);

  return 0;
}

/*! O0505
 * \brief get the position of the first bus point
 * \par Function Description
 *  This function gets the position of the first point of a bus object.
 *
 * \param [out] x       pointer to the x-position
 * \param [out] y       pointer to the y-position
 * \param [in] object   The object to get the position.
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_bus_object_get_position(GedaObject *object, int *x, int *y)
{
  if (GEDA_IS_BUS(object)) {

    if (x) *x = object->line->x[0];

    if (y) *y = object->line->y[0];

    return (x || y) ? TRUE : FALSE;
  }

  geda_bus_object_error(__func__, object);

  return FALSE;
}

/*! O0506
 * \brief Get the x coordinate of first endpoint
 * \par Function Description
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 * \param [in] object The line
 * \return The x coordinate for the first endpoint
 */
int geda_bus_object_get_x1 (const GedaObject *object)
{
  if (GEDA_IS_BUS(object)) {
    return object->line->x[0];
  }

  geda_bus_object_error(__func__, object);

  return 0;
}

/*! O0507
 * \brief Get the x coordinate of second endpoint
 * \par Function Description
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 * \param [in] object The line
 *
 * \return The x coordinate for the second endpoint
 */
int geda_bus_object_get_x2 (const GedaObject *object)
{
  if (GEDA_IS_BUS(object)) {
    return object->line->x[1];
  }
  geda_bus_object_error(__func__, object);
  return 0;
}

/*! O0508
 * \brief Get the y coordinate of first endpoint
 * \par Function Description
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 * \param [in] object The line
 *
 * \return The y coordinate for the first endpoint
 */
int geda_bus_object_get_y1 (const GedaObject *object)
{
  if (GEDA_IS_BUS(object)) {
    return object->line->y[0];
  }
  geda_bus_object_error(__func__, object);
  return 0;
}

/*! O0509
 * \brief Get the y coordinate of second endpoint
 * \par Function Description
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 * \param [in] object The line
 *
 * \return The y coordinate for the second endpoint
 */
int geda_bus_object_get_y2 (const GedaObject *object)
{
  if (GEDA_IS_BUS(object)) {
    return object->line->y[1];
  }
  geda_bus_object_error(__func__, object);
  return 0;
}

/*! O0510
 * \brief mirror a bus object horizontaly at a centerpoint
 * \par Function Description
 *  This function mirrors a bus \a object horizontaly at the point
 *  (\a center_x, \a center_y).
 *
 * \param [in,out] object    The bus object
 * \param [in]     center_x  x-coord of the mirror position
 * \param [in]     center_y  y-coord of the mirror position

 */
void geda_bus_object_mirror(GedaObject *object, int center_x, int center_y)
{
  if (GEDA_IS_BUS(object)) {

    /* translate object to origin */
    geda_bus_object_translate(object, -center_x, -center_y);

    object->line->x[0] = -object->line->x[0];

    object->line->x[1] = -object->line->x[1];

    geda_bus_object_translate(object, center_x, center_y);
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/*! O0511
 * \brief modify one point of a bus object
 * \par Function Description
 *  This function modifies one point of a bus \a object. The point
 *  is specified by the \a whichone variable and the new coordinate
 *  is (\a x, \a y).
 *
 * \param [in] object     The bus Object to modify
 * \param [in] x          new x-coord of the bus point
 * \param [in] y          new y-coord of the bus point
 * \param [in] whichone   bus point to modify
 */
void geda_bus_object_modify(GedaObject *object, int x, int y, int whichone)
{
  if (GEDA_IS_BUS(object)) {

    object->line->x[whichone] = x;
    object->line->y[whichone] = y;

    geda_struct_tile_update_object(object);

    object->bounds_valid = FALSE;
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/*! O0512
 * \brief create a new bus object
 * \par Function Description
 *  This function creates and returns a new bus object.
 *
 * \param [in]  color The color of the bus
 * \param [in]  x1    x-coord of the first point
 * \param [in]  y1    y-coord of the first point
 * \param [in]  x2    x-coord of the second point
 * \param [in]  y2    y-coord of the second point
 * \param [in]  dir   direction of the bus rippers
 *
 * \return A new bus Object
 */
GedaObject *geda_bus_object_new(int color, int x1, int y1, int x2, int y2, int dir)
{
  GedaObject *new_obj;
  GedaBus    *bus;

  /* create the object */
  new_obj = geda_bus_new();
  bus     = GEDA_BUS(new_obj);

  new_obj->color = color;

  new_obj->line->x[0] = x1;
  new_obj->line->y[0] = y1;
  new_obj->line->x[1] = x2;
  new_obj->line->y[1] = y2;

  bus->ripper_direction = dir;

  return new_obj;
}

/*! O0513
 * \brief calculate the orientation of a bus object
 * \par Function Description
 *  This function calculates the orientation of a bus object.
 *
 * \param [in] object   The bus object
 * \return The orientation: HORIZONTAL, VERTICAL or NEITHER
 */
int geda_bus_object_orientation(const GedaObject *object)
{
  if (GEDA_IS_BUS(object)) {

    if (object->line->y[0] == object->line->y[1]) {
      return(HORIZONTAL);
    }

    if (object->line->x[0] == object->line->x[1]) {
      return(VERTICAL);
    }
  }
  else {
    geda_bus_object_error(__func__, object);
  }
  return(NEITHER);
}

/*!
 * \brief postscript print command for a bus object
 * \par Function Description
 *  This function writes the postscript command of the bus object \a o_current
 *  into the FILE \a fp points to.
 *
 * \param [in] toplevel    The GedaToplevel object
 * \param [in] fp          pointer to a FILE structure
 * \param [in] o_current   The GedaObject to print
 * \param [in] origin_x    x-coord of the postscript origin
 * \param [in] origin_y    y-coord of the postscript origin
 */
void geda_bus_object_print(GedaToplevel *toplevel,  FILE *fp,
                           GedaObject   *o_current, int   origin_x,
                                                    int   origin_y)
{
  if (GEDA_IS_BUS(o_current)) {

    int bus_width;
    int x1, y1;
    int x2, y2;

    if (o_current == NULL) {
      printf (_("null in geda_bus_object_print\n"));
      return;
    }

    f_print_set_color(toplevel, fp, o_current->color);

    bus_width = o_current->line_options->line_width;
    if(bus_width <= MIN_LINE_WIDTH_THRESHOLD) {
      /* 1st try updating the style */
      bus_width = geda_object_style_get_bus_width(toplevel);
    }

    if (bus_width < MIN_LINE_WIDTH_THRESHOLD) {
      /* if STYLE_NONE */
      bus_width = MIN_LINE_WIDTH_THRESHOLD;
    }

    x1 = o_current->line->x[0]-origin_x,
    y1 = o_current->line->y[0]-origin_y;
    x2 = o_current->line->x[1]-origin_x,
    y2 = o_current->line->y[1]-origin_y;

    fprintf(fp, "%d %d %d %d %d %d bus\n", x1,y1,x2,y2,bus_width,SQUARE_CAP);
  }
  else {
    geda_bus_object_error(__func__, o_current);
  }
}

/*! O0515
 * \brief read a bus object from a char buffer
 * \par Function Description
 *  This function reads a bus object from the buffer \a buf.
 *  If the bus object was read successfully, a new bus object is
 *  allocated and appended to the \a object_list.
 *
 * \param [in] buf            a text buffer (usually a line of a schematic file)
 * \param [in] release_ver    The release number gEDA
 * \param [in] fileformat_ver integer value of the file format
 * \param [out] err           A GError object
 *
 * \return The object list, or NULL on error.
 */
GedaObject *geda_bus_object_read(const char     buf[],
                                 unsigned int   release_ver,
                                 unsigned int   fileformat_ver,
                                 GError       **err)
{
  GedaObject *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int color;
  int ripper_dir;

  if (release_ver <= VERSION_20020825) {
    if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse bus object"));
      return NULL;
    }
    ripper_dir = 0;
  }
  else {
    if (sscanf (buf, "%c %d %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color,
      &ripper_dir) != 7) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse bus object"));
      return NULL;
    }
  }

  if (x1 == x2 && y1 == y2) {
    const char *msg = _("Found a bus with zero length");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: (%d, %d) (%d, %d).\n", msg, x1, y1, x2, y2);
    }
  }

  if (ripper_dir < -1 || ripper_dir > 1) {
    const char *msg = _("Found an invalid bus ripper direction");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d\n", msg, ripper_dir);
    }
    geda_log_w (_("Resetting direction to neutral (no direction)\n"));
    ripper_dir = 0;
  }

  if (color < 0 || color > MAX_COLORS) {
    const char *msg = _("Found an invalid color");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, color);
    }
    geda_log_w (_("Setting color to default color\n"));
    color = DEFAULT_BUS_COLOR_INDEX;
  }

  new_obj = geda_bus_object_new (color, x1, y1, x2, y2, ripper_dir);

  return new_obj;
}

/*! O0516
 * \brief rotate a bus object around a centerpoint
 * \par Function Description
 *  This function rotates a bus \a object around the point
 *  (\a center_x, \a center_y).
 *
 * \param [in] object    The bus object
 * \param [in] center_x  x-coord of the rotation center
 * \param [in] center_y  y-coord of the rotation center
 * \param [in] angle     The angle to rotate the bus object
 *
 * \note only steps of 90 degrees are allowed for the \a angle
 */
void geda_bus_object_rotate(GedaObject *object, int center_x,
                                                int center_y,
                                                int angle)
{
  if (GEDA_IS_BUS(object)) {

    int newx, newy;

    if (angle == 0)
      return;

    /* translate object to origin */
    geda_bus_object_translate(object, -center_x, -center_y);

    geda_math_rotate_point_90(object->line->x[0], object->line->y[0], angle,
                      &newx, &newy);

    object->line->x[0] = newx;
    object->line->y[0] = newy;

    geda_math_rotate_point_90(object->line->x[1], object->line->y[1], angle,
                      &newx, &newy);

    object->line->x[1] = newx;
    object->line->y[1] = newy;

    geda_bus_object_translate(object, center_x, center_y);
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/*! O0517
 * \brief Set the Bus ripper direction
 * \par Function Description
 *
 * \param [in,out] object    The bus object
 * \param [in]     direction The ripper direction
 */
void geda_bus_object_set_ripper_direction (GedaObject *object, int direction)
{
  if (GEDA_IS_BUS(object)) {
    object->bus->ripper_direction = direction;
    object->bounds_valid = FALSE;
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/*! O0518
 * \brief Set the x coordinate of first endpoint
 * \par Function Description
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 * \param [in,out] object The line
 * \param [in] x The new x coordinate for the first endpoint
 */
void geda_bus_object_set_x1 (GedaObject *object, int x)
{
  if (GEDA_IS_BUS(object)) {
    object->line->x[0] = x;
    object->bounds_valid = FALSE;
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/*! O0519
 * \brief Set the x coordinate of second endpoint
 * \par Function Description
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 * \param [in,out] object The line
 * \param [in] x The new x coordinate for the second endpoint
 */
void geda_bus_object_set_x2 (GedaObject *object, int x)
{
  if (GEDA_IS_BUS(object)) {
    object->line->x[1] = x;
    object->bounds_valid = FALSE;
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/*! O0520
 * \brief Set the y coordinate of first endpoint
 * \par Function Description
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 * \param [in,out] object The line
 * \param [in] y The new y coordinate for the first endpoint
 */
void geda_bus_object_set_y1 (GedaObject *object, int y)
{
  if (GEDA_IS_BUS(object)) {
    object->line->y[0] = y;
    object->bounds_valid = FALSE;
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/*! O0521
 * \brief Set the y coordinate of second endpoint
 * \par Function Description
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 * \param [in,out] object The line
 * \param [in] y The new y coordinate for the second endpoint
 */
void geda_bus_object_set_y2 (GedaObject *object, int y)
{
  if (GEDA_IS_BUS(object)) {
    object->line->y[1] = y;
    object->bounds_valid = FALSE;
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/*! O0522
 * \brief Create a string representation of the bus object
 * \par Function Description
 *  This function takes a bus \a object and return a string
 *  according to the file format definition.
 *
 * \note object was validated by geda_object_save_objects.
 *
 * \param [in] object  a bus Object
 *
 * \returns string representation of the bus Object, which should
 *          be freed at some point.
 */
char *geda_bus_object_to_buffer(GedaObject *object)
{
  if (GEDA_IS_BUS(object)) {
    int x1, x2, y1, y2;
    char *buf;

    x1 = object->line->x[0];
    y1 = object->line->y[0];
    x2 = object->line->x[1];
    y2 = object->line->y[1];

    buf = geda_sprintf("%c %d %d %d %d %d %d", object->type,
    x1, y1, x2, y2, object->color,
    object->bus->ripper_direction);
    return(buf);
  }
  geda_bus_object_error(__func__, object);
  return NULL;
}

/*! O0523
 * \brief move a bus object
 * \par Function Description
 *  This function changes the position of a bus \a object.
 *
 * \param [in] object   The bus Object to be moved
 * \param [in] dx       The x-distance to move the object
 * \param [in] dy       The y-distance to move the object
 */
void geda_bus_object_translate(GedaObject *object, int dx, int dy)
{
  if (GEDA_IS_BUS(object)) {

    /* Update world coords */
    object->line->x[0] = object->line->x[0] + dx;
    object->line->y[0] = object->line->y[0] + dy;
    object->line->x[1] = object->line->x[1] + dx;
    object->line->y[1] = object->line->y[1] + dy;

    /* Update bounding box */
    object->bounds_valid = FALSE;

    geda_struct_tile_update_object(object);
  }
  else {
    geda_bus_object_error(__func__, object);
  }
}

/** @} endgroup geda-bus-object-proc */
