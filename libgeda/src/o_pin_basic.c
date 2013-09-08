/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2013 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>

#include <stdio.h>
#include <math.h>
#include <ascii.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \file o_pin_basic.c
 *  \brief functions for the pin object
 */

/*! \brief calculate and return the boundaries of a pin object
 *  \par Function Description
 *  This function calculates the object boudaries of a pin \a object.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object    a pin object
 *  \param [out] left      the left world coord
 *  \param [out] top       the top world coord
 *  \param [out] right     the right world coord
 *  \param [out] bottom    the bottom world coord
 */
void world_get_pin_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top,
                          int *right, int *bottom)
{
  world_get_line_bounds( toplevel, object, left, top, right, bottom );
}

/*! \brief get the position of a whichend of the pin object
 *  \par Function Description
 *  This function gets the position of the whichend side of a pin object.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
bool o_pin_get_position (TOPLEVEL *toplevel, int *x, int *y, OBJECT *object)
{
  *x = object->line->x[object->whichend];
  *y = object->line->y[object->whichend];
  return TRUE;
}

/*! \brief create a new pin object
 *  \par Function Description
 *  This function creates and returns a new pin object.
 *
 *  \param [in]     toplevel    The TOPLEVEL object.
 *  \param [in]     type        The OBJECT type (usually OBJ_PIN)
 *  \param [in]     color       The color of the pin
 *  \param [in]     x1          x-coord of the first point
 *  \param [in]     y1          y-coord of the first point
 *  \param [in]     x2          x-coord of the second point
 *  \param [in]     y2          y-coord of the second point
 *  \param [in]     pin_type    type of pin
 *  \param [in]     whichend    The connectable end of the pin
 *  \return A new pin OBJECT
 */
OBJECT *o_pin_new(TOPLEVEL *toplevel,
                  char type, int color,
                  int x1, int y1, int x2, int y2, PIN_TYPE pin_type, int whichend)
{
  OBJECT *new_node;

  new_node = s_basic_new_object(type, "pin");
  new_node->color = color;

  new_node->line = (LINE *) g_malloc(sizeof(LINE));

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;

  o_pin_set_type (toplevel, new_node, pin_type);

  new_node->w_bounds_valid_for = NULL;

  new_node->whichend = whichend;

  return new_node;
}

/*! \brief read a pin object from a char buffer
 *  \par Function Description
 *  This function reads a pin object from the buffer \a buf.
 *  If the pin object was read successfully, a new pin object is
 *  allocated and appended to the \a object_list.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] buf          a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list, or NULL on error.
 */
OBJECT *o_pin_read (TOPLEVEL *toplevel, const char buf[],
                    unsigned int release_ver, unsigned int fileformat_ver, GError **err)
{
  OBJECT *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int color;
  int pin_type;
  int whichend;

  if (release_ver <= VERSION_20020825) {
    if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse pin object"));
      return NULL;
    }
    pin_type = PIN_TYPE_NET;
    whichend = -1;
  } else {
    if (sscanf (buf, "%c %d %d %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2,
      &color, &pin_type, &whichend) != 8) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse pin object"));
    return NULL;
      }
  }

  if (whichend == -1) {
    s_log_message (_("Found a pin which did not have the whichone field set.\n"
    "Verify and correct manually.\n"));
  } else if (whichend < -1 || whichend > 1) {
    s_log_message (_("Found an invalid whichend on a pin (reseting to zero): %d\n"),
                   whichend);
    whichend = 0;
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message (_("Found an invalid color [ %s ]\n"), buf);
    s_log_message (_("Setting color to default color\n"));
    color = DEFAULT_COLOR_INDEX;
  }

  if (toplevel->override_pin_color != -1) {
    color = toplevel->override_pin_color;
  }

  new_obj = o_pin_new (toplevel, type, color, x1, y1, x2, y2,
                       pin_type, whichend);

  return new_obj;
}

/*! \brief Create a string representation of the pin object
 *  \par Function Description
 *  This function takes a pin \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] toplevel  a TOPLEVEL structure
 *  \param [in] object  a pin OBJECT
 *  \return the string representation of the pin OBJECT
 */
char *o_pin_save(TOPLEVEL *toplevel, OBJECT *object)
{
  int x1, x2, y1, y2;
  int pin_type, whichend;
  char *buf;

  x1 = object->line->x[0];
  y1 = object->line->y[0];
  x2 = object->line->x[1];
  y2 = object->line->y[1];

  /* description of the pin */
  pin_type = object->pin_type;
  whichend = object->whichend;

  buf = g_strdup_printf("%c %d %d %d %d %d %d %d", object->type,
		   x1, y1, x2, y2, object->color, pin_type, whichend);
  return(buf);
}

/*! \brief move a pin object
 *  \par Function Description
 *  This function changes the position of a pin \a object.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] dx           The x-distance to move the object
 *  \param [in] dy           The y-distance to move the object
 *  \param [in] object       The pin OBJECT to be moved
 */
void o_pin_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object)
{
  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;

  /* Update bounding box */
  object->w_bounds_valid_for = NULL;

  s_tile_update_object(toplevel, object);
}

/*! \brief create a copy of a pin object
 *  \par Function Description
 *  This function creates a copy of the pin object \a o_current.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] o_current    The object that is copied
 *  \return a new pin object
 */
OBJECT *o_pin_copy(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;

  new_obj = o_pin_new (toplevel, OBJ_PIN, o_current->color,
                       o_current->line->x[0], o_current->line->y[0],
                       o_current->line->x[1], o_current->line->y[1],
                       o_current->pin_type,   o_current->whichend);

  return new_obj;
}

/*! \brief postscript print command for a pin object
 *  \par Function Description
 *  This function writes the postscript command of the pin object \a o_current
 *  into the FILE \a fp points to.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] fp           pointer to a FILE structure
 *  \param [in] o_current    The OBJECT to print
 *  \param [in] origin_x     x-coord of the postscript origin
 *  \param [in] origin_y     y-coord of the postscript origin
 */
void o_pin_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
		 int origin_x, int origin_y)
{
  int pin_width;
  int x1, y1;
  int x2, y2;

  if (o_current == NULL) {
    printf("got null in o_pin_print\n");
    return;
  }

  f_print_set_color(toplevel, fp, o_current->color);

  x1 = o_current->line->x[0] - origin_x;
  y1 = o_current->line->y[0] - origin_y;
  x2 = o_current->line->x[1] - origin_x;
  y2 = o_current->line->y[1] - origin_y;

  pin_width = o_current->line_width;
  if(pin_width < MIN_LINE_WIDTH_THRESHOLD)
     pin_width = o_style_get_pin_width(toplevel, PIN_TYPE_NET); /* 1st try updating pin style */
  if(pin_width < MIN_LINE_WIDTH_THRESHOLD)
     pin_width = MIN_LINE_WIDTH_THRESHOLD;        /* if STYLE_NONE  */

  fprintf(fp, "%d %d %d %d %d %d line\n",x1,y1,x2,y2,pin_width,toplevel->print_output_capstyle);

}

/*! \brief rotate a pin object around a centerpoint
 *  \par Function Description
 *  This function rotates a pin \a object around the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotat the pin object
 *  \param [in] object        The pin object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void o_pin_rotate_world(TOPLEVEL *toplevel, int world_centerx,
			int world_centery, int angle,
			OBJECT *object)
{
  int newx, newy;

  if (angle == 0)
    return;

  /* translate object to origin */
  o_pin_translate_world(toplevel, -world_centerx, -world_centery, object);

  rotate_point_90(object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  rotate_point_90(object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  o_pin_translate_world(toplevel, world_centerx, world_centery, object);
}

/*! \brief mirror a pin object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a pin \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the mirror position
 *  \param [in] world_centery y-coord of the mirror position
 *  \param [in] object        The pin object
 */
void o_pin_mirror_world(TOPLEVEL *toplevel,
			int world_centerx, int world_centery, OBJECT *object)
{
  /* translate object to origin */
  o_pin_translate_world(toplevel, -world_centerx, -world_centery, object);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  o_pin_translate_world(toplevel, world_centerx, world_centery, object);
}

/*! \brief modify one point of a pin object
 *  \par Function Description
 *  This function modifies one point of a pin \a object. The point
 *  is specified by the \a whichone variable and the new coordinate
 *  is (\a x, \a y).
 *
 *  \param toplevel   The TOPLEVEL object
 *  \param object     The pin OBJECT to modify
 *  \param x          new x-coord of the pin point
 *  \param y          new y-coord of the pin point
 *  \param whichone   pin point to modify
 *
 */
void o_pin_modify(TOPLEVEL *toplevel, OBJECT *object,
                  int x, int y, int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  object->w_bounds_valid_for = NULL;

  s_tile_update_object(toplevel, object);
}

/*! \brief guess the whichend of pins of object list
 *  \par Function Description
 *  This function determines the whichend of the pins in the \a object_list.
 *  In older libgeda file format versions there was no information about the
 *  active end of pins.
 *  This function calculates the bounding box of all pins in the object list.
 *  The side of the pins that are closer to the boundary of the box are
 *  set as active ends of the pins.
 *
 *  \param toplevel    A TOPLEVEL object
 *  \param object_list list of OBJECTs
 *  \param num_pins    pin count in the object list
 *
 */
void o_pin_update_whichend (TOPLEVEL *toplevel, GList *object_list, int num_pins)
{
  OBJECT *o_current;
  GList *iter;
  int top = 0, left = 0;
  int right = 0, bottom = 0;
  int d1, d2, d3, d4;
  int min0, min1;
  int min0_whichend, min1_whichend;
  int rleft, rtop, rright, rbottom;
  int found;

  if (object_list && num_pins) {
    if (num_pins == 1 || toplevel->force_boundingbox) {
      world_get_object_glist_bounds (toplevel, object_list,
                                    &left, &top, &right, &bottom);
    } else {
      found = 0;

      /* only look at the pins to calculate bounds of the symbol */
      iter = object_list;
      while (iter != NULL) {
        o_current = (OBJECT *)iter->data;
        if (o_current->type == OBJ_PIN) {
          world_get_single_object_bounds( toplevel, o_current, &rleft, &rtop, &rright, &rbottom);

          if ( found ) {
            left = min( left, rleft );
            top = min( top, rtop );
            right = max( right, rright );
            bottom = max( bottom, rbottom );
          } else {
            left = rleft;
            top = rtop;
            right = rright;
            bottom = rbottom;
            found = 1;
          }
        }
        iter = g_list_next (iter);
      }

    }
  } else {
    return;
  }

  iter = object_list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;
    /* Determine which end of the pin is on or nearest the boundary */
    if (o_current->type == OBJ_PIN && o_current->whichend == -1) {
      if (o_current->line->y[0] == o_current->line->y[1]) {
        /* horizontal */

        d1 = abs(o_current->line->x[0] - left);
        d2 = abs(o_current->line->x[1] - left);
        d3 = abs(o_current->line->x[0] - right);
        d4 = abs(o_current->line->x[1] - right);

        if (d1 <= d2) {
          min0 = d1;
          min0_whichend = 0;
        } else {
          min0 = d2;
          min0_whichend = 1;
        }

        if (d3 <= d4) {
          min1 = d3;
          min1_whichend = 0;
        } else {
          min1 = d4;
          min1_whichend = 1;
        }

        if (min0 <= min1) {
          o_current->whichend = min0_whichend;
        } else {
          o_current->whichend = min1_whichend;
        }

      } else if (o_current->line->x[0] == o_current->line->x[1]) {
        /* vertical */

        d1 = abs(o_current->line->y[0] - top);
        d2 = abs(o_current->line->y[1] - top);
        d3 = abs(o_current->line->y[0] - bottom);
        d4 = abs(o_current->line->y[1] - bottom);

        if (d1 <= d2) {
          min0 = d1;
          min0_whichend = 0;
        } else {
          min0 = d2;
          min0_whichend = 1;
        }

        if (d3 <= d4) {
          min1 = d3;
          min1_whichend = 0;
        } else {
          min1 = d4;
          min1_whichend = 1;
        }

        if (min0 <= min1) {
          o_current->whichend = min0_whichend;
        } else {
          o_current->whichend = min1_whichend;
        }
      }
    }
    iter = g_list_next (iter);
  }
}


/*! \brief Sets the type, and corresponding width of a pin
 *
 *  \par Function Description
 *  Sets the pin's type and width to a particular style.
 *
 *  \param [in] toplevel   The TOPLEVEL object
 *  \param [in] o_current  The pin OBJECT being modified
 *  \param [in] pin_type   The new type of this pin
 */
void o_pin_set_type (TOPLEVEL *toplevel, OBJECT *o_current, PIN_TYPE pin_type)
{
  switch (pin_type) {
    case PIN_TYPE_NET:
      o_current->pin_type = PIN_TYPE_NET;
      break;
    case PIN_TYPE_BUS:
      o_current->pin_type = PIN_TYPE_BUS;
      break;
    case PIN_TYPE_BUMP:
      o_current->pin_type = PIN_TYPE_BUMP;
      break;
    case PIN_TYPE_BALL:
      o_current->pin_type = PIN_TYPE_BALL;
      break;
    case PIN_TYPE_WEDGE:
      o_current->pin_type = PIN_TYPE_WEDGE;
      break;
    case PIN_TYPE_RIBBON:
      o_current->pin_type = PIN_TYPE_RIBBON;
      break;
    default:
      g_critical ("o_pin_set_type: invalid pin type! %i\n", pin_type);
      o_current->pin_type = PIN_TYPE_NET;
  }
  o_current->line_width = o_style_get_pin_width(toplevel, o_current->pin_type);
}

PIN_ATTRIBUTE o_pin_get_pintype_attribute(const char *pintype_str) {

  const char *types[] = { "in",  "out", "io",  "oc", "oe", "pas", "tp",
                          "tri", "clk", "pwr", NULL };

  PIN_ATTRIBUTE index;
  for (index = PIN_ATTRIB_IN; types[index] != NULL; index++) {
    if (strcmp(pintype_str, types[index]) == 0)
      break;
  }
  if(types[index] == NULL ) index = PIN_ATTRIB_VOID;
  return index;
}

char *o_pin_get_pintype_string(PIN_ATTRIBUTE attribute) {

  const char *types[] = { "in",  "out", "io",  "oc", "oe", "pas", "tp",
                          "tri", "clk", "pwr", NULL };

  return g_strdup(types[attribute]);
}
/*! \brief Retrieve Properties for a Pin Object
 *
 *  \par Function Description
 *  Gets the pin's properties.
 *
 *  \param [in]  object    OBJECT to read the properties
 *  \param [out] type      The PIN_TYPE.
 *  \param [out] number    The Pin Number associate with the object
 *  \param [out] sequence  The Pin Sequence Number
 *  \param [out] label     Ptr to the Pin Label String.
 *  \param [out] attribute The PIN_ATTRIBUTE
 *
 *  \return TRUE on succes, FALSE otherwise
 */
bool
o_pin_get_options(OBJECT *object, PIN_TYPE *type, int *number, int *sequence,
                  const char **label, PIN_ATTRIBUTE *attribute)
{
  bool result;
  GList *a_iter;
  OBJECT *a_current;
  const char *str;
  int length;
  char *ptr;

  char *pinnumber_str = NULL;
  char *pinseq_str = NULL;
  const char *pinlabel_str = NULL;
  char *pintype_str = NULL;

  result = (object->type == OBJ_PIN) ? TRUE : FALSE;

  if (result) {
    *type  = object->pin_type;
    a_iter = object->attribs;
    while (a_iter != NULL) {
      a_current = a_iter->data;
      str = a_current->text->string;
      if (str) {
        ptr = (char*) str;
        while (( *ptr != ASCII_NUL ) && ( *ptr != ASCII_EQUAL_SIGN )) { ptr++; } /* find "=" */
        if ( *ptr == ASCII_EQUAL_SIGN ) {
          length = ptr - str;
          ptr++;
          if(strncmp(str, "pinnumber", length) == 0 ) {
            pinnumber_str = ptr;
           *number = atoi(pinnumber_str);
          }
          else {
            if(strncmp(str, "pinseq", length) == 0) {
              pinseq_str = ptr;
             *sequence = atoi(pinseq_str);
            }
            else {
              if(strncmp(str, "pinlabel", length) == 0 ) {
                pinlabel_str = ptr;
               *label = pinlabel_str;
              }
              else {
                if(strncmp(str, "pintype", length) == 0) {
                  pintype_str = ptr;
                 *attribute = o_pin_get_pintype_attribute(pintype_str);
                }
              }
            }
          }
        }
      }
      else
         fprintf(stderr,"got null string\n");
      a_iter = g_list_next (a_iter);
    }
  }
  if(!pinnumber_str)
    *number = -1;

  if(!pinseq_str)
    *sequence = -1;

  if(!pinlabel_str)
    *label = NULL;

  if(!pintype_str)
    *attribute = PIN_ATTRIB_VOID;

  return result;
}

void o_pin_set_options(TOPLEVEL *toplevel, OBJECT *object, PIN_TYPE type,
                       int number, int sequence, const char *label_str,
                       PIN_ATTRIBUTE attribute)
{
  if (object != NULL && object->type == OBJ_PIN) {

    o_attrib_freeze_hooks(toplevel, object);
    s_conn_remove_object (toplevel, object);

    OBJECT *bute;
    object->pin_type = type;

    bute = o_attrib_first_attrib_by_name (object, "pinnumber");
    if(bute !=NULL) {
      o_attrib_set_value(bute, "pinnumber",  g_strdup_printf ("%d", number));
      o_text_recreate(toplevel, bute);
    } /*
    else {
      o_attrib_add(TOPLEVEL *toplevel, OBJECT *object, OBJECT *item)
    }
    */

    bute = o_attrib_first_attrib_by_name (object, "pinseq");
    if(bute !=NULL) {
      o_attrib_set_value(bute, "pinseq",  g_strdup_printf ("%d", sequence));
      o_text_recreate(toplevel, bute);
    }

    bute = o_attrib_first_attrib_by_name (object, "pinlabel");
    if(bute !=NULL) {
      o_attrib_set_value(bute, "pinlabel",  g_strdup(label_str));
      o_text_recreate(toplevel, bute);
    }

    bute = o_attrib_first_attrib_by_name (object, "pintype");
    if(bute !=NULL) {
      o_attrib_set_value(bute, "pintype",  o_pin_get_pintype_string(attribute));
      o_text_recreate(toplevel, bute);
    }
    s_conn_update_object (toplevel, object);
    o_attrib_thaw_hooks (toplevel, object);
  }

}
