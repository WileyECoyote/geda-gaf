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

/*! \file o_get.c
 *  \brief functions to get basic object properties
 *
 *  This file contains the code used to retrieve the properties ot
 *  relations of <b>GedaObjects</b>. The object is the basic type of all
 *  elements stored in schematic and symbol files.
 */

#include "../../../config.h"

#include <stdio.h>
#include <ctype.h>

#include <libgeda_priv.h>
#include <geda_text.h>


/*!
 * \brief Get the Parent index an object is attached to
 * \par Function Description
 *  If \a object is a attached to another #GedaObject, returns the
 *  sid of the parent object. Otherwise, returns -1.
 *
 * \param [in] object The GedaObject for which to get the parent index.
 *
 * \return sid of the parent \a object is attached or -1 if none.
 */
int geda_object_get_attached_parent_id (GedaObject *object)
{
  int sid;

  if (geda_object_get_is_attached(object)) {
    sid = object->attached_to->sid;
  }
  else {
    sid = -1;
  }
  return sid;
}

/*!
 * \brief Return the bounds of the given object
 * \par Given an object, calculate the bounds coordinates.
 *
 * \param [in]  o_current The object to look the bounds for.
 * \param [out] rleft     pointer to the left coordinate of the object.
 * \param [out] rtop      pointer to the top coordinate of the object.
 * \param [out] rright    pointer to the right coordinate of the object.
 * \param [out] rbottom   pointer to the bottom coordinate of the object.
 *
 * \return If any bounds were found for the object
 * \retval 0 No bound was found
 * \retval 1 Bound was found
 */
int geda_object_get_bounds(ConstObject *o_current,
                           int *rleft,  int *rtop,
                           int *rright, int *rbottom)
{
  int result = 0;

  if (GEDA_IS_OBJECT(o_current)) {

    if (!o_current->bounds_valid) {

      result = geda_object_bounds(o_current);

      if (result) {

        *rleft   = o_current->left;
        *rtop    = o_current->top;
        *rright  = o_current->right;
        *rbottom = o_current->bottom;
      }
    }
    else {

      *rleft   = o_current->left;
      *rtop    = o_current->top;
      *rright  = o_current->right;
      *rbottom = o_current->bottom;

#if DEBUG
      geda_object_bounds(o_current);
      if (o_current->top != *rtop) {
        fprintf(stderr, "%s bounds_valid=%d\n", o_current->name, o_current->bounds_valid);
      }
#endif
      result = 1;
    }
  }
  else {
    BUG_MSG("Oops, Not a GedaObject");
  }

  return result;
}

/*!
 * \brief Return the bounds of the given GList of objects.
 * \par Given a list of objects, calculates the bounds coordinates.
 *
 * \param [in]  list   The list of objects to look the bounds for.
 * \param [out] left   pointer to the left coordinate of the object.
 * \param [out] top    pointer to the top coordinate of the object.
 * \param [out] right  pointer to the right coordinate of the object.
 * \param [out] bottom pointer to the bottom coordinate of the object.
 *
 * \return If any bounds were found for the list of objects
 * \retval 0 No bounds were found
 * \retval 1 Bound was found
 */
int geda_object_get_bounds_list(const GList *list, int *left, int *top, int *right, int *bottom)
{
  const GList *s_current;

  int rleft   = 0;
  int rtop    = 0;
  int rright  = 0;
  int rbottom = 0;
  int found   = 0;

  s_current = g_list_first((GList *)list);

  /* Find the first object with bounds, and set the bounds variables, then expand as necessary */
  while ( s_current != NULL ) {

    if (geda_object_get_bounds(s_current->data, &rleft, &rtop, &rright, &rbottom))
    {
      if ( found ) {
        *left   = min( *left, rleft );
        *top    = min( *top, rtop );
        *right  = max( *right, rright );
        *bottom = max( *bottom, rbottom );
      }
      else {
        *left   = rleft;
        *top    = rtop;
        *right  = rright;
        *bottom = rbottom;
        found   = 1;
      }
    }

    NEXT(s_current);
  }

  return found;
}

/*!
 * \brief Get capstyle for printing of an object.
 * \par Function Description
 *  This function gets the object's capstyle for printing from
 *  its line end. See #LINE_END for information on valid line
 *  end values.
 *
 *  \param [in] end  Line end value of the object
 */
int geda_object_get_capstyle (LINE_END end)
{
  switch(end) {
    case(END_NONE):   return BUTT_CAP; break;
    case(END_SQUARE): return SQUARE_CAP; break;
    case(END_ROUND):  return ROUND_CAP; break;
    default:          return BUTT_CAP; break;
  }
}

/*!
 * \brief get #GedaObject's fill properties
 * \par Function Description
 *  This function get's the #GedaObject's fill options.
 *  See OBJECT_FILLING for information on valid fill types.
 *
 * \param [in]   object    GedaObject to read the properties
 * \param [out]  type      OBJECT_FILLING type
 * \param [out]  width     fill width.
 * \param [out]  pitch1    cross hatch line distance
 * \param [out]  angle1    cross hatch angle
 * \param [out]  pitch2    cross hatch line distance
 * \param [out]  angle2    cross hatch angle
 *
 * \return TRUE on succes, FALSE otherwise
 */
bool geda_object_get_fill_options(GedaObject       *object,
                                  OBJECT_FILLING   *type, int *width,
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

    answer = TRUE;
  }
  else {
    answer = FALSE;
  }
  return answer;
}

/*!
 * \brief Get if Linear Object has Slope
 * \par Function Description
 *  Returns TRUE if \a object is derived from a GedaLine
 *  and has differing x coordinates, otherwise FALSE.
 */
bool geda_object_get_has_slope (GedaObject *object)
{
  bool answer;

  if (GEDA_IS_LINE(object)) {
    answer = object->line->x[0] != object->line->x[1];
  }
  else {
    answer = FALSE;
  }
  return answer;
}

/*!
 * \brief Checks if an object is attached to something
 * \par Function Description
 *  Noramally \a object would be a text attribute that is
 *  attached to another object but could be any type of
 *  GedaObject
 *
 * \param object  The GedaObject to test
 *
 * \return TRUE if attached to another object, other wise FALSE.
 */
bool geda_object_get_is_attached (GedaObject *object)
{
  return GEDA_IS_OBJECT(object) && GEDA_IS_OBJECT(object->attached_to);
}

/*!
 * \brief Checks if an object is bus, or a bus pin
 * \par Function Description
 *  Checks if an object is a bus or a bus pin
 *
 * \param object  The GedaObject to test
 *
 * \return TRUE if the object is a bus, or bus pin
 */
bool geda_object_get_bus_related (GedaObject *object)
{
  return (GEDA_IS_BUS(object) || (GEDA_IS_PIN(object)
          && object->pin->node_type == PIN_BUS_NODE));
}

/*!
 * \brief Checks if an object is embedded
 * \par Function Description
 *  Checks if an object is embedded or not. If object is
 *  a Complex or Picture then object->embedded is returned
 *  otherwise FALSE is returned,
 *
 * \param object  The GedaObject to test
 *
 * \return TRUE if the object is embedded
 */
bool geda_object_get_is_embedded (GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_OBJECT (object), FALSE);

  return ((GEDA_IS_COMPLEX(object) && (object->complex->is_embedded)) ||
          (GEDA_IS_PICTURE(object) && (object->picture->is_embedded)));
}

/*!
 * \brief Check if point is inside a region
 * \par Function Description
 *  This function takes a rectangular region and a point and checks
 *  if the point is located in the region or not.
 *
 * \param [in] xmin    Smaller x coordinate of the region.
 * \param [in] ymin    Smaller y coordinate of the region.
 * \param [in] xmax    Larger x coordinate of the region.
 * \param [in] ymax    Larger y coordinate of the region.
 * \param [in] x       x coordinate of the point to check.
 * \param [in] y       y coordinate of the point to check.
 * \return 1 if the point is inside the region, 0 otherwise.
 */
int geda_object_get_is_inside_region(int xmin, int ymin, int xmax, int ymax, int x, int y)
{
  return ((x >= xmin && x <= xmax && y >= ymin && y <= ymax) ? 1 : 0);
}

/*!
 * \brief Query if object is selectable
 * \par Function Description
 *  Attribute getter for the selectable field within the object.
 *
 * \param object   The GedaObject structure to be queried
 *
 * \return TRUE when selectable, otherwise FALSE.
 */
bool geda_object_get_is_selectable (GedaObject *object)
{
  return GEDA_IS_OBJECT(object) && (object->selectable);
}

/*!
 * \brief Query if object is selected
 * \par Function Description
 *  Attribute getter for the selectable field within the object.
 *
 * \param object   The GedaObject structure to be queried
 *
 * \return TRUE when the object is selected, FALSE otherwise
 */
bool geda_object_get_is_selected (GedaObject *object)
{
  return GEDA_IS_OBJECT(object) && (object->selected);
}

/*!
 * \brief Checks if an text string is valid attribute format
 * \par Function Description
 *  Check for the presents of an ASCII_EQUAL_SIGN without adjacent
 *  spaces characters in the text string.
 *
 * \param object  Text Object to test
 *
 * \return TRUE if valid, otherwise FALSE.
 */
bool geda_object_get_is_valid_attribute (GedaObject *object)
{
  bool result = FALSE;

  if (GEDA_IS_TEXT(object)) {

    if (object->text && object->text->string) {

      const char *ptr, *string;

      string = object->text->string;
      ptr    = strstr(string, "=");

      if (ptr) {

        int pos = ptr - string;

        if (pos && (pos != strlen(string) - 1)) {

          if (!isspace (string[pos - 1]) && (!isspace(string[pos + 1]))) {
            result = TRUE;
          }
        }
      }
    }
  }

  return result;
}

/*!
 * \brief Query visibility of the object
 * \par Function Description
 *  Attribute getter for the visible field within the object.
 *
 * \param object   The GedaObject structure to be queried
 *
 * \return TRUE when VISIBLE, FALSE otherwise
 *
 * \sa geda_object_get_visibility
 */
bool geda_object_get_is_visible (const GedaObject *object)
{
  return GEDA_IS_OBJECT(object) && (object->visibility > 0);
}

/*!
 * \brief Get line end using capstyle value
 * \par Function Description
 *  This function gets the object's line end value based on capstyle
 *  value used for Postscript printing.
 *
 * \sa geda_object_get_capstyle
 *
 * \param [in] capstyle
 */
LINE_END geda_object_get_line_cap_style (int capstyle)
{
  switch(capstyle) {
    case(BUTT_CAP): return END_NONE; break;
    case(SQUARE_CAP): return END_SQUARE; break;
    case(ROUND_CAP): return END_ROUND; break;
    default: return END_NONE; break;
  }
}

/*!
 * \brief get #GedaObject's line properties
 * \par Function Description
 *  This function get's the #GedaObject's line options.
 *  See #LINE_END and #LINE_TYPE for information on valid
 *  object end and type values.
 *
 * \param [in]   object    GedaObject to read the properties
 * \param [out]  end       An #LINE_END.
 * \param [out]  type      An #LINE_TYPE.
 * \param [out]  width     Line width.
 * \param [out]  length    Line length.
 * \param [out]  space     Spacing between dashes/dots.
 *
 * \return TRUE on succes, FALSE otherwise
 */
bool geda_object_get_line_options(GedaObject *object,
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

/*!
 * \brief Get the Point on an GedaObject Nearest a given Point
 * \par Function Description
 *  This function ia a wrapper for the 0_xxx_get_nearest functions.
 *
 * \param [in]  object  Pointer to the object of interest
 * \param [in]  x       Integer x of point near or on the object
 * \param [in]  y       Integer y of point near or on the object
 * \param [out] nx      Integer pointer to resulting x value
 * \param [out] ny      Integer pointer to resulting y value
 */
bool geda_object_get_nearest_point(const GedaObject *object, int x, int y, int *nx, int *ny)
{
  bool (*getter) (const GedaObject *, int, int, int *, int *);

  switch (object->type) {
    case OBJ_NET:
    case OBJ_BUS:
    case OBJ_PIN:
    case OBJ_LINE:    getter = geda_line_object_get_nearest_point;      break;
    case OBJ_BOX:     getter = geda_box_object_get_nearest_point;       break;
    case OBJ_PICTURE: getter = geda_picture_object_get_nearest_point;   break;
    case OBJ_CIRCLE:  getter = geda_circle_object_get_nearest_point;    break;
    case OBJ_PLACEHOLDER:
    case OBJ_COMPLEX: getter = geda_complex_object_get_nearest_point;   break;
    case OBJ_TEXT:    getter = geda_text_object_get_nearest_point;      break;
    case OBJ_PATH:    getter = geda_path_object_get_nearest_point;      break;
    case OBJ_ARC:     getter = geda_arc_object_get_nearest_point;       break;
    default:          getter = NULL;                                    break;
  }

  return (getter != NULL) ? (*getter) (object, x, y, nx, ny) : FALSE;
}

/*!
 * \brief Count the lines of a text string
 * \par Function Description
 *  This function just counts the number of lines that are
 *  in the \a string.
 *
 * \param [in] string  text string to count the lines
 *
 * \return the number of lines
 */
int geda_object_get_num_text_lines(const char *string)
{
  const char *aux;
        int   line_count = 0;

  if (string == NULL) {
    return 0;
  }

  /* if it's not null, then we have at least one line */
  line_count++;

  /* Count how many \n are in the string */
  aux = string;

  while (aux && ((uint32_t) (*aux) != 0) ) {

    uint32_t current_char = g_utf8_get_char_validated(aux, -1);

    if (current_char == '\n')
      line_count++;
    aux = g_utf8_find_next_char(aux, NULL);
  }

  return (line_count);
}

/*!
 * \brief Get pointer to an GedaObject's Attribute Value given the name
 * \par Function Description
 *  Returns a pointer to the value of a named attribute belonging to object,
 *  the string belongs to libgeda and must not be freed. The value returned
 *  is for the first attribute found with the given \a name.
 *
 * \param [in] object GedaObject whose attributes are to be searched
 * \param [in] name   The name of the attribute to search for
 *
 * \note Does not search floating attributes, only attributes directly
 *       attached, i.e. object->attribs.
 *
 * \return If objects is valid and has an attribute with a matching \a name
 *         then the value of the attribute is returned , otherwise NULL.
 */
const char *geda_object_get_attrib_value (GedaObject *object, const char *name)
{
  GedaObject *attrib;
  const char *value;

  if (GEDA_IS_OBJECT(object)) {

    if (object->attribs) {

      attrib = geda_find_attrib_by_name (object->attribs, name, 0);

      if (geda_object_get_is_valid_attribute(attrib)) {

         value  = attrib->text->string + 2;
         while (value && *(value - 1) != ASCII_EQUAL_SIGN) value++;

      }
      else {
        value = NULL;
      }
    }
    else {
      value = NULL;
    }
  }
  else {
    BUG_MSG("Invalid GEDA GedaObject");
    value = NULL;
  }
  return value;
}

/*!
 * \brief Get List of Objects in List by GedaObject Type.
 * \par Function Description
 *  Returns a glist of objects that are members of the \a olist matching
 *  the given \a type. The returned glist must be freed with g_list_free.
 *
 * \param [in] olist A GList of Objects to search
 * \param [in] type  The object type to search for
 *
 * \return list of Objects if found, or %NULL if no member was the requested
 *         type or the input list was empty.
 */
GList *geda_object_get_objects_by_type (const GList *olist, int type)
{
  GList  *objects = NULL;

  lambda (GedaObject *object)
  {
    if (object->type == type) {
      objects = g_list_append(objects, object);
    }
    return FALSE;
  }
  foreach (olist);

  return objects;
}

/*!
 * \brief Get an object's containing complex object.
 * \par Function Description
 *  If \a object is part of a complex #GedaObject, returns that
 *  #GedaObject. Otherwise, returns %NULL.
 *
 * \param [in] object  The GedaObject for which to get the containing GedaObject.
 *
 * \return The Objects which owns \a object, or %NULL.
 */
GedaObject *geda_object_get_parent (GedaObject *object)
{
  if (GEDA_IS_OBJECT(object) && GEDA_IS_OBJECT(object->parent_object))
  {
      return object->parent_object;
  }

  return NULL;
}

/*!
 * \brief Get an object's containing object index.
 * \par Function Description
 *  If \a object is a sub object of another #GedaObject, returns the
 *  sid of the parent object. Otherwise, returns -1.
 *
 * \param [in] object    The GedaObject for which to get the containing GedaObject.
 *
 * \return The complex Object which owns \a object, or %NULL.
 */
int geda_object_get_parent_id (GedaObject *object)
{
  int cid;

  if ((GEDA_IS_OBJECT(object) && GEDA_IS_OBJECT(object->parent_object)))
  {
      cid = object->parent_object->sid;
  }
  else {
    cid = -1;
  }
  return cid;
}

/*!
 * \brief get the base position of an object
 * \par Function Description
 *  This function gets the position of an object in world coordinates.
 *
 * \param [in]  object  Pointer to a #GedaObject
 * \param [out] x       pointer to the x-position,
 * \param [out] y       pointer to the y-position.
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_object_get_position (GedaObject *object, int *x, int *y)
{
  bool (*func) (GedaObject*, int*, int*) = NULL;

  switch (object->type) {
      case OBJ_TEXT:        func = geda_text_object_get_position;    break;
      case OBJ_COMPLEX:     func = geda_complex_object_get_position; break;
      case OBJ_PIN:         func = geda_pin_object_get_position;     break;
      case OBJ_NET:         func = geda_net_object_get_position;     break;
      case OBJ_BOX:         func = geda_box_object_get_position;     break;
      case OBJ_CIRCLE:      func = geda_circle_object_get_position;  break;
      case OBJ_LINE:        func = geda_line_object_get_position;    break;
      case OBJ_ARC:         func = geda_arc_object_get_position;     break;
      case OBJ_PATH:        func = geda_path_object_get_position;    break;
      case OBJ_BUS:         func = geda_bus_object_get_position;     break;
      case OBJ_PICTURE:     func = geda_picture_object_get_position; break;
      case OBJ_PLACEHOLDER: func = geda_complex_object_get_position; break;

      default:
        BUG_IMSG("object has bad type", object->type);
  }

  return (func != NULL) ? (*func) (object, x, y) : FALSE;
}

/*!
 * \brief Get the Shortest distance from point to Hatchable Object
 * \par Function Description
 *  Calculates the distance between the given point and the closest
 *  point on the given object. Allows forcing objects to solid. If
 *  the distance cannot be calculated or if an error occurs, this
 *  function returns a really large number (G_MAXDOUBLE).
 *
 * \param [in] object       The given object.
 * \param [in] x            The x coordinate of the given point.
 * \param [in] y            The y coordinate of the given point.
 * \param [in] force_solid  If true, force treating the object as solid.
 *
 * \return The shortest distance from the object to the point.
 */
double geda_object_get_shortest_distance_full (GedaObject *object, int x, int y, int force_solid)
{
  double shortest_distance;

  if(GEDA_IS_OBJECT(object)) {

    double (*func) (ConstObject *, int, int, int) = NULL;

    switch(object->type) {
      case OBJ_BUS:
      case OBJ_NET:
      case OBJ_PIN:
      case OBJ_LINE:        func = geda_line_object_shortest_distance;     break;
      case OBJ_BOX:         func = geda_box_object_shortest_distance;      break;
      case OBJ_PICTURE:     func = geda_picture_object_shortest_distance;  break;
      case OBJ_CIRCLE:      func = geda_circle_object_shortest_distance;   break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX:     func = geda_complex_object_shortest_distance;  break;
      case OBJ_TEXT:        func = geda_text_object_shortest_distance;     break;
      case OBJ_PATH:        func = geda_path_object_shortest_distance;     break;
      case OBJ_ARC:         func = geda_arc_object_shortest_distance;      break;
    }

    if (func != NULL) {
      shortest_distance = (*func) (object, x, y, force_solid);
    }
    else {
      shortest_distance = G_MAXDOUBLE;
    }
  }
  else {
    shortest_distance = G_MAXDOUBLE;
  }

  return shortest_distance;
}

/*!
 * \brief Get the Shortest distance from point to Object
 * \par Function Description
 *  Calculates the distance between the given point and the closest
 *  point on the given object. If the distance cannot be calculated,
 *  this function returns a really large number (G_MAXDOUBLE). If an
 *  error occurs, this function returns G_MAXDOUBLE.
 *
 * \param [in] object       The given object.
 * \param [in] x            The x coordinate of the given point
 * \param [in] y            The y coordinate of the given point
 *
 * \return The shortest distance from the object to the point.
 */
double geda_object_get_shortest_distance (GedaObject *object, int x, int y)
{
  return geda_object_get_shortest_distance_full (object, x, y, FALSE);
}
