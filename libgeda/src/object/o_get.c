/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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

/*! \file o_get.c
 *  \brief functions to get basic object properties
 *
 *  This file contains the code used to retrieve the properties ot
 *  relations of <b>Objects</b>. The object is the basic type of all
 *  elements stored in schematic and symbol files.
 *
 */

#include <config.h>
#include <stdio.h>

#include "libgeda_priv.h"

/*! \brief Get capstyle for printing of an object.
 *  \par Function Description
 *  This function gets the object's capstyle for printing from its line end.
 *  See LINE_END for information on valid line end values.
 *
 *  \param [in]     end        Line end value of the object
 *
 *  TODO: Change this function to use it also in gschem_cairo.c
 */
int o_get_capstyle (LINE_END end)
{
  switch(end) {
    case(END_NONE):   return BUTT_CAP; break;
    case(END_SQUARE): return SQUARE_CAP; break;
    case(END_ROUND):  return ROUND_CAP; break;
    default:          return BUTT_CAP; break;
  }
}

/*! \brief get #Object's fill properties
 *
 *  \par Function Description
 *  This function get's the #Object's fill options.
 *  See OBJECT_FILLING for information on valid fill types.
 *
 *  \param [in]   object    Object to read the properties
 *  \param [out]  type      OBJECT_FILLING type
 *  \param [out]  width     fill width.
 *  \param [out]  pitch1    cross hatch line distance
 *  \param [out]  angle1    cross hatch angle
 *  \param [out]  pitch2    cross hatch line distance
 *  \param [out]  angle2    cross hatch angle
 *
 *  \return TRUE on succes, FALSE otherwise
 *
 */
bool o_get_fill_options(Object *object,
                        OBJECT_FILLING *type, int *width,
                        int *pitch1, int *angle1,
                        int *pitch2, int *angle2)
{
  bool answer;

  if (GEDA_IS_CIRCLE(object) || GEDA_IS_ARC(object)    ||
    GEDA_IS_BOX(object)    || GEDA_IS_PATH(object))
  {

    *type   = object->fill_options->fill_type;
    *width  = object->fill_options->fill_width;
    *pitch1 = object->fill_options->fill_pitch1;
    *angle1 = object->fill_options->fill_angle1;
    *pitch2 = object->fill_options->fill_pitch2;
    *angle2 = object->fill_options->fill_angle2;

    return answer = TRUE;
  }
  else {
    answer = FALSE;
  }
  return answer;
}

/*! \brief Checks if an object is bus, or a bus pin
 *
 *  \par Function Description
 *  Checks if an object is a bus or a bus pin
 *
 *  \param object  The Object to test
 *
 *  \return TRUE if the object is a bus, or bus pin
 */
bool
o_get_is_bus_related (Object *object)
{
  return (GEDA_IS_BUS(object) || (GEDA_IS_PIN(object)
          && object->pin->node_type == PIN_BUS_NODE));
}

/*! \brief Query if object is selectable
 *
 *  \par Function Description
 *  Attribute getter for the selectable field within the object.
 *
 *  \param object   The Object structure to be queried
 *
 *  \return TRUE when VISIBLE, FALSE otherwise
 */
bool
o_get_is_selectable (Object *object)
{
  g_return_val_if_fail (GEDA_IS_OBJECT(object), FALSE);
  return object->selectable == TRUE;
}

/*! \brief Query if object is selected
 *
 *  \par Function Description
 *  Attribute getter for the selectable field within the object.
 *
 *  \param object   The Object structure to be queried
 *
 *  \return TRUE when the object is selected, FALSE otherwise
 */
bool
o_get_is_selected (Object *object)
{
  g_return_val_if_fail (GEDA_IS_OBJECT(object), FALSE);
  return object->selected == 1;
}

/*! \brief Query visibility of the object
 *
 *  \par Function Description
 *  Attribute getter for the visible field within the object.
 *
 *  \param object   The Object structure to be queried
 *
 *  \return TRUE when VISIBLE, FALSE otherwise
 */
bool
o_get_is_visible (Object *object)
{
  g_return_val_if_fail (object != NULL, FALSE);
  return object->visibility > 0;
}

/*! \brief Get line end using capstyle value
 *
 *  \par Function Description
 *  This function gets the object's line end value based on capstyle value
 *  used for Postscript printing.
 *  See also information on 'output-capstyle' gschem configuration option.
 *
 *  \param [in]     capstyle
 */
LINE_END o_get_line_end (int capstyle)
{
  switch(capstyle) {
    case(BUTT_CAP): return END_NONE; break;
    case(SQUARE_CAP): return END_SQUARE; break;
    case(ROUND_CAP): return END_ROUND; break;
    default: return END_NONE; break;
  }
}

/*! \brief get #Object's line properties
 *
 *  \par Function Description
 *  This function get's the #Object's line options.
 *  See LINE_END and LINE_TYPE for information on valid
 *  object end and type values.
 *
 *  \param [in]   object    Object to read the properties
 *  \param [out]  end       An LINE_END.
 *  \param [out]  type      An LINE_TYPE.
 *  \param [out]  width     Line width.
 *  \param [out]  length    Line length.
 *  \param [out]  space     Spacing between dashes/dots.
 *
 *  \return TRUE on succes, FALSE otherwise
 *
 */
bool o_get_line_options(Object *object,
                        LINE_END *end, LINE_TYPE *type,
                        int *width, int *length, int *space)
{
  bool result;

  if (GEDA_IS_LINE(object) || GEDA_IS_CIRCLE(object) ||
      GEDA_IS_ARC(object)  || GEDA_IS_BOX(object)    ||
      GEDA_IS_PATH(object))
  {

    *end    = object->line_options->line_end;
    *type   = object->line_options->line_type;
    *width  = object->line_options->line_width;
    *length = object->line_options->line_length;
    *space  = object->line_options->line_space;
     result = TRUE;
  }
  else {
    result = FALSE;
  }
  return result;
}

/*! \brief Get List of Objects in List by Object Type.
 *
 * \par Function Description
 * Returns a glist of objects, that are member of the given
 * glist that match the given type..
 *
 * \param [in] olist A GList of Objects to search
 * \param [in] type  The object type to search for
 *
 * \return list of Objects if found, or NULL if no member
 *         was the requested type or the input list was
 *         empty.
 */

GList*
o_get_objects_by_type (GList *olist, int type)
{
  GList  *objects = NULL;

  lambda (Object *object)
  {
    if (object->type == type) {
      objects = g_list_append(objects, object);
    }
    return FALSE;
  }
  foreach (olist);

  return objects;
}

/*! \brief Get the Page associated with an Object.
 *
 * \par Function Description
 * If \a object is on a page, the page is return, otherwise NULL
 * is returned.
 *
 * \param [in] object    The Object for which to get the Page Object.
 *
 * \return page Object which owns \a object, or NULL.
 */
Page *o_get_page (Object *object)
{
  return geda_object_get_page(object);
}


/*! \brief Get an object's containing complex object.
 *
 * \par Function Description
 * If \a object is part of a complex #Object, returns that
 * #Object. Otherwise, returns NULL.
 *
 * \param [in] object    The Object for which to get the containing Object.
 *
 * \return The complex Object which owns \a object, or NULL.
 */
Object *o_get_parent (Object *object)
{
  g_return_val_if_fail (GEDA_IS_OBJECT(object), NULL);

  if (object->parent_object != NULL) {
    return object->parent_object;
  }
  return NULL;
}

/*! \brief Get an object's containing object index.
 *
 * \par Function Description
 * If \a object is a sub object of another #Object, returns the
 * sid of the parent object. Otherwise, returns -1.
 *
 * \param [in] object    The Object for which to get the containing Object.
 *
 * \return The complex Object which owns \a object, or NULL.
 */
int o_get_parent_id (Object *object)
{
  g_return_val_if_fail (GEDA_IS_OBJECT(object), -1);

  int cid = -1;

  if (object->parent_object != NULL) {
    cid = object->parent_object->sid;
  }
  return cid;
}

/*! \brief get the base position of an object
 *
 *  \par Function Description
 *  This function gets the position of an object in world coordinates.
 *
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
bool o_get_position (int *x, int *y, Object *object)
{
  bool (*func) ( int*, int*, Object*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = o_line_get_position;    break;
      case OBJ_NET:     func = o_net_get_position;     break;
      case OBJ_BUS:     func = o_bus_get_position;     break;
      case OBJ_BOX:     func = o_box_get_position;     break;
      case OBJ_PICTURE: func = o_picture_get_position; break;
      case OBJ_CIRCLE:  func = o_circle_get_position;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = o_complex_get_position; break;
      case OBJ_TEXT:    func = o_text_get_position;    break;
      case OBJ_PATH:    func = o_path_get_position;    break;
      case OBJ_PIN:     func = o_pin_get_position;     break;
      case OBJ_ARC:     func = o_arc_get_position;     break;
      default:
        g_critical ("o_get_position: object %p has bad type '%c'\n",
                    object, object->type);
  }

  if (func != NULL) {
    return (*func) (x, y, object);
  }
  return FALSE;
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the given object. Allows forcing objects to solid.
 *
 *  \param [in] object       The given object.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  If an error occurs, this function returns
 *  G_MAXDOUBLE.
 */
double o_get_shortest_distance_full (Object *object, int x, int y, int force_solid)
{
  double shortest_distance = G_MAXDOUBLE;
  double (*func) (Object *, int, int, int) = NULL;

  g_return_val_if_fail (GEDA_IS_OBJECT(object), G_MAXDOUBLE);

  switch(object->type) {
    case OBJ_BUS:
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_LINE:        func = o_line_shortest_distance;     break;
    case OBJ_BOX:         func = o_box_shortest_distance;      break;
    case OBJ_PICTURE:     func = o_picture_shortest_distance;  break;
    case OBJ_CIRCLE:      func = o_circle_shortest_distance;   break;
    case OBJ_PLACEHOLDER:
    case OBJ_COMPLEX:     func = o_complex_shortest_distance;  break;
    case OBJ_TEXT:        func = o_text_shortest_distance;     break;
    case OBJ_PATH:        func = o_path_shortest_distance;     break;
    case OBJ_ARC:         func = o_arc_shortest_distance;      break;
  }

  if (func != NULL) {
    shortest_distance = (*func) (object, x, y, force_solid);
  }

  return shortest_distance;
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the given object.
 *
 *  \par Function Description
 *  If the distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  If an error occurs, this function returns G_MAXDOUBLE.
 *
 *  \param [in] object       The given object.
 *  \param [in] x            The x coordinate of the given point
 *  \param [in] y            The y coordinate of the given point
 *
 *  \return The shortest distance from the object to the point.
 */
double
o_get_shortest_distance (Object *object, int x, int y)
{
  return o_get_shortest_distance_full (object, x, y, FALSE);
}
