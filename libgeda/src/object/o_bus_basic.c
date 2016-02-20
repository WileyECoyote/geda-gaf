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

/*! \file o_bus_basic.c
 *  \brief functions for the bus object
 */

#include <config.h>
#include <stdio.h>

#include <libgeda_priv.h>

static int  o_bus_consolidate_segments (GedaObject *object) __attribute__((unused));
static void o_bus_consolidate_lowlevel (GedaObject *object,  GedaObject *del_object, int orient) __attribute__((unused));

/*! \brief get the position of the first bus point
 *
 *  \par Function Description
 *  This function gets the position of the first point of a bus object.
 *
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
bool o_bus_get_position(int *x, int *y, GedaObject *object)
{
  g_return_val_if_fail(GEDA_IS_BUS(object), FALSE);

  *x = object->line->x[0];
  *y = object->line->y[0];

  return TRUE;
}

/*! \brief create a new bus object
 *
 *  \par Function Description
 *  This function creates and returns a new bus object.
 *
 *  \param [in]     color       The color of the bus
 *  \param [in]     x1          x-coord of the first point
 *  \param [in]     y1          y-coord of the first point
 *  \param [in]     x2          x-coord of the second point
 *  \param [in]     y2          y-coord of the second point
 *  \param [in]  bus_ripper_direction direction of the bus rippers
 *
 *  \return A new bus Object
 */
GedaObject *o_bus_new(int color, int x1, int y1, int x2, int y2, int bus_ripper_direction)
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

  bus->bus_ripper_direction = bus_ripper_direction;

  return new_obj;
}

/*! \brief read a bus object from a char buffer
 *
 *  \par Function Description
 *  This function reads a bus object from the buffer \a buf.
 *  If the bus object was read successfully, a new bus object is
 *  allocated and appended to the \a object_list.
 *
 *  \param [in] buf            a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver    The release number gEDA
 *  \param [in] fileformat_ver integer value of the file format
 *
 *  \param [out] err           A GError object
 *
 *  \return The object list, or NULL on error.
 */
GedaObject *o_bus_read(const char buf[], unsigned int release_ver,
                                     unsigned int fileformat_ver, GError **err)
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
  } else {
    if (sscanf (buf, "%c %d %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color,
      &ripper_dir) != 7) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse bus object"));
    return NULL;
      }
  }

  if (x1 == x2 && y1 == y2) {
    u_log_message (_("Found a zero length bus [ %c %d %d %d %d %d ]\n"),
                   type, x1, y1, x2, y2, color);
  }
/*
  if (toplevel->override_bus_color != -1) {
    color = toplevel->override_bus_color;
  }
*/
  if (color < 0 || color > MAX_COLORS) {
    u_log_message (_("Found an invalid color [ %s ]\n"), buf);
    u_log_message (_("Setting color to default color\n"));
    color = DEFAULT_BUS_COLOR_INDEX;
  }

  if (ripper_dir < -1 || ripper_dir > 1) {
    u_log_message (_("Found an invalid bus ripper direction [ %s ]\n"), buf);
    u_log_message (_("Resetting direction to neutral (no direction)\n"));
    ripper_dir = 0;
  }

  new_obj = o_bus_new (color, x1, y1, x2, y2, ripper_dir);

  return new_obj;
}

/*! \brief create a copy of a bus object
 *
 *  \par Function Description
 *  This function creates a copy of the bus object \a o_current.
 *
 *  \param [in] o_current    The object that is copied
 *
 *  \return a new bus object
 */
GedaObject *o_bus_copy(GedaObject *o_current)
{
  GedaObject *new_obj;

  g_return_val_if_fail(GEDA_IS_BUS(o_current), NULL);

  /* make sure you fix this in pin and bus as well */
  /* still doesn't work... you need to pass in the new values */
  /* or don't update and update later */
  /* I think for now I'll disable the update and manually update */
  new_obj = o_bus_new (o_current->color,
                       o_current->line->x[0], o_current->line->y[0],
                       o_current->line->x[1], o_current->line->y[1],
                       o_current->bus->bus_ripper_direction);

  new_obj->line_options->line_width = *o_current->bus->line_width;

  return new_obj;
}

/*! \brief Create a string representation of the bus object
 *
 *  \par Function Description
 *  This function takes a bus \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object  a bus Object
 *
 *  \return the string representation of the bus Object
 */
char *o_bus_save(GedaObject *object)
{
  int x1, x2, y1, y2;
  char *buf;

  g_return_val_if_fail(GEDA_IS_BUS(object), NULL);

  x1 = object->line->x[0];
  y1 = object->line->y[0];
  x2 = object->line->x[1];
  y2 = object->line->y[1];

  buf = geda_utility_string_sprintf("%c %d %d %d %d %d %d", object->type,
                         x1, y1, x2, y2, object->color,
                         object->bus->bus_ripper_direction);
  return(buf);
}

/*! \brief move a bus object
 *
 *  \par Function Description
 *  This function changes the position of a bus \a object.
 *
 *  \param [in] object       The bus Object to be moved
 *  \param [in] dx           The x-distance to move the object
 *  \param [in] dy           The y-distance to move the object
 */
void o_bus_translate(GedaObject *object, int dx, int dy)
{
  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;

  /* Update bounding box */
  object->w_bounds_valid_for = NULL;

  s_tile_update_object(object);

}

/*! \brief postscript print command for a bus object
 *
 *  \par Function Description
 *  This function writes the postscript command of the bus object \a o_current
 *  into the FILE \a fp points to.
 *
 *  \param [in] toplevel     The GedaToplevel object
 *  \param [in] fp           pointer to a FILE structure
 *  \param [in] o_current    The GedaObject to print
 *  \param [in] origin_x     x-coord of the postscript origin
 *  \param [in] origin_y     y-coord of the postscript origin
 */
void o_bus_print(GedaToplevel *toplevel, FILE *fp, GedaObject *o_current,
                 int origin_x, int origin_y)
{
  int bus_width;
  int x1, y1;
  int x2, y2;

  if (o_current == NULL) {
    printf (_("null in o_bus_print\n"));
    return;
  }

  f_print_set_color(toplevel, fp, o_current->color);

  bus_width = o_current->line_options->line_width;
  if(bus_width <= MIN_LINE_WIDTH_THRESHOLD)
    bus_width = o_style_get_bus_width(toplevel);  /* 1st try updating the style */
  if (bus_width < MIN_LINE_WIDTH_THRESHOLD) /* if STYLE_NONE */
    bus_width = MIN_LINE_WIDTH_THRESHOLD;

  x1 = o_current->line->x[0]-origin_x,
  y1 = o_current->line->y[0]-origin_y;
  x2 = o_current->line->x[1]-origin_x,
  y2 = o_current->line->y[1]-origin_y;

  fprintf(fp, "%d %d %d %d %d %d bus\n", x1,y1,x2,y2,bus_width,SQUARE_CAP);

}

/*! \brief rotate a bus object around a centerpoint
 *
 *  \par Function Description
 *  This function rotates a bus \a object around the point
 *  (\a center_x, \a center_y).
 *
 *  \param [in] object    The bus object
 *  \param [in] center_x  x-coord of the rotation center
 *  \param [in] center_y  y-coord of the rotation center
 *  \param [in] angle     The angle to rotate the bus object

 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void o_bus_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  int newx, newy;

  if (angle == 0)
  return;

  /* translate object to origin */
  o_bus_translate(object, -center_x, -center_y);

  m_rotate_point_90(object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  m_rotate_point_90(object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  o_bus_translate(object, center_x, center_y);
}

/*! \brief mirror a bus object horizontaly at a centerpoint
 *
 *  \par Function Description
 *  This function mirrors a bus \a object horizontaly at the point
 *  (\a center_x, \a center_y).
 *
 *  \param [in,out] object    The bus object
 *  \param [in]     center_x  x-coord of the mirror position
 *  \param [in]     center_y  y-coord of the mirror position

 */
void o_bus_mirror(GedaObject *object, int center_x, int center_y)
{
  /* translate object to origin */
  o_bus_translate(object, -center_x, -center_y);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  o_bus_translate(object, center_x, center_y);
}

/*! \brief calculate the orientation of a bus object
 *
 *  \par Function Description
 *  This function calculates the orientation of a bus object.
 *
 *  \param [in] object   The bus object
 *  \return The orientation: HORIZONTAL, VERTICAL or NEITHER
 */
int o_bus_orientation(GedaObject *object)
{
  if (object->line->y[0] == object->line->y[1]) {
    return(HORIZONTAL);
  }

  if (object->line->x[0] == object->line->x[1]) {
    return(VERTICAL);
  }

  return(NEITHER);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* 1 for right, -1 for left (horizontal bus)  1 for up, -1 for down (vertial bus) */
int o_bus_get_direction(GedaObject *object)
{
  int direction = 0;

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
  return direction;
}

/* \brief
 * \par Function Description
 * This function does the actual work of making one bus segment out of two
 * connected segments.
 * The second object (del_object) is the object that should be deleted.
 *
 * \todo This function is currently not used. Check it before using it
 */
static void o_bus_consolidate_lowlevel (GedaObject *object,
                                        GedaObject *del_object, int orient)
{
  int temp1, temp2;
  int final1, final2;
  int changed=0;
  GList *a_iter;
  GedaObject *a_current;

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
    a_iter = del_object->attribs;
    while (a_iter != NULL) {
      a_current = a_iter->data;
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
static int o_bus_consolidate_segments (GedaObject *object)
{

  return(0);
}

/* \brief
 * \par Function Description
 *
 * \todo Not Implemented Yet
 */
void o_bus_consolidate( void)
{

}

/*! \brief modify one point of a bus object
 *
 *  \par Function Description
 *  This function modifies one point of a bus \a object. The point
 *  is specified by the \a whichone variable and the new coordinate
 *  is (\a x, \a y).
 *
 *  \param [in] object     The bus Object to modify
 *  \param [in] x          new x-coord of the bus point
 *  \param [in] y          new y-coord of the bus point
 *  \param [in] whichone   bus point to modify
 */
void o_bus_modify(GedaObject *object, int x, int y, int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  s_tile_update_object(object);

  object->w_bounds_valid_for = NULL;
}
