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

#include "../../../config.h"

#include <stdio.h>

#include <libgeda_priv.h>
#include <geda_debug.h>

/*! \file o_pin_object.c
 *  \brief functions for the pin object
 */

/** \defgroup geda-pin-object-proc GedaPin Object Procedures
 * @{
 * \brief Procedures for Operations with #GedaPin Objects
 */

/*!
 * \brief Create a new pin object
 * \par Function Description
 *  This function creates and returns a new pin object.
 *
 * \param [in]     color       The color of the pin
 * \param [in]     x1          x-coord of the first point
 * \param [in]     y1          y-coord of the first point
 * \param [in]     x2          x-coord of the second point
 * \param [in]     y2          y-coord of the second point
 * \param [in]     node_type   type of pin
 * \param [in]     whichend    The connectable end of the pin
 *
 * \return A new pin Object
 */
GedaObject*
geda_pin_object_new(int color, int x1, int y1, int x2, int y2, PIN_NODE node_type, int whichend)
{
  GedaObject *new_obj;

  /* create the object */
  new_obj = geda_pin_new();

  new_obj->color = color;

  new_obj->line->x[0] = x1;
  new_obj->line->y[0] = y1;
  new_obj->line->x[1] = x2;
  new_obj->line->y[1] = y2;

  geda_pin_object_set_node_type (new_obj, node_type);

  new_obj->pin->whichend = whichend;

  return new_obj;
}

/*!
 * \brief create a copy of a pin object
 * \par Function Description
 *  This function creates a copy of the pin object \a o_current.
 *
 * \param [in] o_current    The object that is copied
 *
 * \return a new pin object
 */
GedaObject *geda_pin_object_copy(GedaObject *o_current)
{
  if (GEDA_IS_PIN(o_current)) {

    GedaObject *new_obj;
    GedaPin    *new_pin;

    new_obj = geda_pin_object_new (o_current->color,
                                   o_current->line->x[0],
                                   o_current->line->y[0],
                                   o_current->line->x[1],
                                   o_current->line->y[1],
                                   o_current->pin->node_type,
                                   o_current->pin->whichend);

    new_pin = GEDA_PIN(new_obj);

    if (o_current->pin->electrical)
      geda_pin_set_electrical(new_pin, o_current->pin->electrical);

    if (o_current->pin->label)
      geda_pin_set_label(new_pin, o_current->pin->label);

    if (o_current->pin->mechanical)
      geda_pin_set_mechanical(new_pin, o_current->pin->mechanical);

    new_obj->line_options->line_width = *o_current->pin->line_width;

    return new_obj;
  }
  return NULL;
}

/*!
 * \brief get the position of a whichend of the pin object
 * \par Function Description
 *  This function gets the position of the whichend side of a pin object.
 *
 * \param [in]  object  Pointer to a #GedaPin object
 * \param [out] x       pointer to the x-position
 * \param [out] y       pointer to the y-position
 *
 * \return TRUE if successfully determined the position, FALSE otherwise
 */
bool geda_pin_object_get_position (GedaObject *object, int *x, int *y)
{
  g_return_val_if_fail(GEDA_IS_PIN(object), FALSE);

  if (x) *x = object->line->x[object->pin->whichend];

  if (y) *y = object->line->y[object->pin->whichend];

  return (x || y) ? TRUE : FALSE;
}

/*!
 * \brief read a pin object from a char buffer
 * \par Function Description
 *  This function reads a pin object from the buffer \a buf.
 *  If the pin object was read successfully, a new pin object is
 *  allocated and appended to the \a object_list.
 *
 * \param [in] buf            a text buffer (usually a pin of a schematic file)
 * \param [in] release_ver    The release number gEDA
 * \param [in] fileformat_ver a integer value of the file format
 *
 * \param [out] err           A GError obejct
 *
 * \return The object list, or NULL on error.
 */
GedaObject *geda_pin_object_read (const char buf[], unsigned int release_ver,
                                      unsigned int fileformat_ver,
                                      GError **err)
{
  GedaObject *new_obj;
  char   type;
  int    x1, y1;
  int    x2, y2;
  int    color;
  int    node_type;
  int    whichend;

  if (release_ver <= VERSION_20020825) {
    if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6)
    {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse pin object"));
      return NULL;
    }
    node_type = PIN_NET_NODE;
    whichend = -1;
  }
  else {
    if (sscanf (buf, "%c %d %d %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2,
      &color, &node_type, &whichend) != 8)
    {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse pin object"));
      return NULL;
    }
  }

  /* TODO Should check zero length! */

  if (whichend == -1) {
    const char *msg = _("Found a pin which did not have the whichend field set");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s\n", msg);
    }
    geda_log_w (_("Verify and correct manually.\n"));
  }
  else if (whichend < -1 || whichend > 1) {
    const char *msg = _("Found an invalid whichend on a pin");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, whichend);
    }
    geda_log_w (_("Setting to zero\n"));
    whichend = 0;
  }

  if (color < 0 || color > MAX_COLORS) {
    const char *msg = _("Found an invalid color");
    if (geda_object_show_buffer_err(msg, buf)) {
      geda_log_w("%s: %d.\n", msg, color);
    }
    geda_log_w (_("Setting color to default color\n"));
    color = PIN_COLOR;
  }

  new_obj = geda_pin_object_new (color, x1, y1, x2, y2, node_type, whichend);

  return new_obj;
}

/*!
 * \brief Create a string representation of the pin object
 * \par Function Description
 *  This function takes a pin \a object and return a string
 *  according to the file format definition.
 *
 * \note object was validated by geda_object_save_objects
 *
 * \param [in] object  a pin Object
 *
 * \returns string representation of the pin Object
 */
char *geda_pin_object_save(GedaObject *object)
{
  int x1, x2, y1, y2;
  int node_type, whichend;
  char *buf;

  x1 = object->line->x[0];
  y1 = object->line->y[0];
  x2 = object->line->x[1];
  y2 = object->line->y[1];

  /* description of the pin */
  node_type = object->pin->node_type;
  whichend  = object->pin->whichend;

  buf = geda_sprintf("%c %d %d %d %d %d %d %d", object->type,
                          x1, y1, x2, y2, object->color, node_type, whichend);
  return(buf);
}

/*!
 * \brief mirror a pin object horizontaly at a centerpoint
 * \par Function Description
 *  This function mirrors a pin \a object horizontaly at the point
 *  (\a center_x, \a center_y).
 *
 * \param [in,out] object    The pin object
 * \param [in]     center_x  x-coord of the mirror position
 * \param [in]     center_y  y-coord of the mirror position
 */
void geda_pin_object_mirror(GedaObject *object, int center_x, int center_y)
{
  /* translate object to origin */
  geda_pin_object_translate(object, -center_x, -center_y);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  geda_pin_object_translate(object, center_x, center_y);
}

/*!
 * \brief Translate a pin object
 * \par Function Description
 *  This function changes the position of a pin \a object.
 *
 * \param [in] object       The pin Object to be moved
 * \param [in] dx           The x-distance to move the object
 * \param [in] dy           The y-distance to move the object
 */
void geda_pin_object_translate(GedaObject *object, int dx, int dy)
{
  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;

  /* Update bounding box */
  object->bounds_valid = FALSE;

  geda_struct_tile_update_object(object);
}

/*!
 * \brief postscript print command for a pin object
 * \par Function Description
 *  This function writes the postscript command of the pin object \a o_current
 *  into the FILE \a fp points to.
 *
 * \param [in] toplevel     The GedaToplevel object
 * \param [in] fp           pointer to a FILE structure
 * \param [in] o_current    The GedaObject to print
 * \param [in] origin_x     x-coord of the postscript origin
 * \param [in] origin_y     y-coord of the postscript origin
 */
void
geda_pin_object_print(GedaToplevel *toplevel, FILE *fp, GedaObject *o_current,
            int origin_x, int origin_y)
{
  int cap_style;
  int pin_width;
  int x1, y1;
  int x2, y2;

  if (o_current == NULL) {
    printf("got null in geda_pin_object_print\n");
    return;
  }

  f_print_set_color(toplevel, fp, o_current->color);

  x1 = o_current->line->x[0] - origin_x;
  y1 = o_current->line->y[0] - origin_y;
  x2 = o_current->line->x[1] - origin_x;
  y2 = o_current->line->y[1] - origin_y;

  pin_width = o_current->line_options->line_width;
  if(pin_width < MIN_LINE_WIDTH_THRESHOLD)
     pin_width = geda_object_style_get_pin_width(toplevel, PIN_NET_NODE); /* 1st try updating pin style */
  if(pin_width < MIN_LINE_WIDTH_THRESHOLD)
     pin_width = MIN_LINE_WIDTH_THRESHOLD;        /* if STYLE_NONE  */

  cap_style = toplevel->print_output_capstyle;

  fprintf(fp, "%d %d %d %d %d %d pin\n", x1,y1,x2,y2,pin_width,cap_style);
}

/*!
 * \brief rotate a pin object around a centerpoint
 * \par Function Description
 *  This function rotates a pin \a object around the point
 *  (\a center_x, \a center_y).
 *
 * \param [in,out] object    The pin object
 * \param [in]     center_x  x-coord of the rotation center
 * \param [in]     center_y  y-coord of the rotation center
 * \param [in]     angle     The angle to rotate the pin object
 *
 * \note only steps of 90 degrees are allowed for the \a angle
 */
void
geda_pin_object_rotate(GedaObject *object, int center_x, int center_y, int angle)
{
  int newx, newy;

  if (angle == 0)
    return;

  /* translate object to origin */
  geda_pin_object_translate(object, -center_x, -center_y);

  geda_math_rotate_point_90(object->line->x[0], object->line->y[0], angle, &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  geda_math_rotate_point_90(object->line->x[1], object->line->y[1], angle, &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  geda_pin_object_translate(object, center_x, center_y);
}

/*!
 * \brief modify one point of a pin object
 * \par Function Description
 *  This function modifies one point of a pin \a object. The point
 *  is specified by the \a whichone variable and the new coordinate
 *  is (\a x, \a y).
 *
 * \param object     The pin Object to modify
 * \param x          new x-coord of the pin point
 * \param y          new y-coord of the pin point
 * \param whichone   pin point to modify
 */
void geda_pin_object_modify(GedaObject *object, int x, int y, int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  object->bounds_valid = FALSE;

  geda_struct_tile_update_object(object);
}

/*!
 * \brief Normalize a Pin object
 * \par Function Description
 *  This function makes the connection point to be the second point.
 *
 * \param object      A GedaPin GedaObject
 */
void geda_pin_object_normalize(GedaObject *object)
{
  if (object && object->type == OBJ_PIN) {
    if (!object->pin->whichend) {
      int tmp_x = object->line->x[0];
      int tmp_y = object->line->y[0];
      object->line->x[0] = object->line->x[1];
      object->line->y[0] = object->line->y[1];
      object->line->x[1] = tmp_x;
      object->line->y[1] = tmp_y;
      object->pin->whichend = 1;
    }
  }
}

/*!
 * \brief guess the whichend of pins of object list
 * \par Function Description
 *  This function determines the whichend of the pins in the \a object_list.
 *  In older libgeda file format versions there was no information about the
 *  active end of pins.
 *  This function calculates the bounding box of all pins in the object list.
 *  The side of the pins that are closer to the boundary of the box are
 *  set as active ends of the pins.
 *
 * \param object_list list of Objects
 * \param num_pins    pin count in the object list
 */
void geda_pin_object_update_whichend (GList *object_list, int num_pins)
{
  GedaObject *o_current;
  GList *iter;
  int top   = 0, left   = 0;
  int right = 0, bottom = 0;
  int d1, d2, d3, d4;
  int min0, min1;
  int min0_whichend, min1_whichend;
  int rleft, rtop, rright, rbottom;

  if (object_list && num_pins) {

    if (num_pins == 1) {
      geda_object_get_bounds_list (object_list, &left, &top, &right, &bottom);
    }
    else {

      int found = 0;

      /* only look at the pins to calculate bounds of the symbol */
      iter = object_list;

      while (iter != NULL) {
        o_current = (GedaObject *)iter->data;
        if (o_current->type == OBJ_PIN) {
          geda_object_get_bounds(o_current, &rleft, &rtop, &rright, &rbottom);

          if (found) {
            left   = min (left, rleft);
            top    = min (top, rtop);
            right  = max (right, rright);
            bottom = max (bottom, rbottom);
          }
          else {
            left   = rleft;
            top    = rtop;
            right  = rright;
            bottom = rbottom;
            found  = 1;
          }
        }
        iter = iter->next;
      }
    }
  }
  else {
    return;
  }

  iter = object_list;
  while (iter != NULL) {

    o_current = (GedaObject *)iter->data;

    /* Determine which end of the pin is on or nearest the boundary */
    if (o_current->type == OBJ_PIN && o_current->pin->whichend == -1) {

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
          o_current->pin->whichend = min0_whichend;
        } else {
          o_current->pin->whichend = min1_whichend;
        }

      }
      else if (o_current->line->x[0] == o_current->line->x[1]) {
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
          o_current->pin->whichend = min0_whichend;
        } else {
          o_current->pin->whichend = min1_whichend;
        }
      }
    }
    iter = iter->next;
  }
}

/*!
 * \brief Sets the Electrical type of a GedaPin
 * \par Function Description
 *  Sets the pin's electrical type code and updates the pin's
 *  internal electrical description. Attributes assicated with
 *  the pins electrical properties are not updated. Applicated
 *  should manage such attributes separately or use the pin's
 *  internal description.
 *
 * \param [in] o_current  The pin Object being modified
 * \param [in] e_type     The electrical type code
 *
 * \note geda_pin_lookup_estring returns NULL if the code was not
 *       valid but the function do not report as an error and this
 *       allows applications to use custom electrical types. In such
 *       cases, the code is set to PIN_ELECT_VOID and the application
 *       must also maintain the internal electrical description.
 *
 * \return [out] TRUE     Standard electrical code
 *               FALSE    Custom type, i.e. code = PIN_ELECT_VOID
 */
bool geda_pin_object_set_elect_type (GedaObject *o_current, PIN_ELECT e_type)
{
  const char *e_str = geda_pin_lookup_estring(e_type);
  if (e_str) {
    o_current->pin->elect_type = e_type;
    geda_pin_set_electrical(o_current->pin, e_str);
  }
  return (e_str ? TRUE : FALSE);
}

/*!
 * \brief Sets the Mechanical type of a GedaPin
 * \par Function Description
 *  Sets the pin's mechanical type code and updates the pin's
 *  internal mechanical description. Attributes assicated with
 *  the pins mechanical properties are not updated. Applicated
 *  should manage such attributes separately or use the pin's
 *  internal description.
 *
 * \param [in] o_current  The pin Object being modified
 * \param [in] m_type     The mechanical type code
 *
 * \note geda_pin_lookup_estring returns NULL if the code was not
 *       valid but the function do not report as an error and this
 *       allows applications to use custom mechanical types. In such
 *       cases, the code is set to PIN_MECH_VOID and the application
 *       must also maintain the internal mechanical description.
 *
 * \return [out] TRUE     Standard mechanical code
 *               FALSE    Custom type, i.e. code = PIN_MECH_VOID
 */
bool geda_pin_object_set_mech_type (GedaObject *o_current, PIN_MECH m_type)
{
  const char *m_str = geda_pin_lookup_mstring(m_type);
  if (m_str) {
    o_current->pin->mech_type = m_type;
    geda_pin_set_mechanical(o_current->pin, m_str);
  }

  return (m_str ? TRUE : FALSE);
}

/*!
 * \brief Sets the node type, and corresponding width of a pin
 * \par Function Description
 *  Sets the pin's type and width to a particular style.
 *
 * \param [in] o_current  The pin Object being modified
 * \param [in] node_type   The new type of this pin
 */
void geda_pin_object_set_node_type (GedaObject *o_current, PIN_NODE node_type)
{
  switch (node_type) {
    case PIN_NET_NODE:
      o_current->pin->node_type = PIN_NET_NODE;
      break;

    case PIN_BUS_NODE:
      o_current->pin->node_type = PIN_BUS_NODE;
      break;

    default:
      BUG_IMSG ("invalid pin node type", node_type);
      o_current->pin->node_type = PIN_NET_NODE;
  }
}

/*!
 * \brief Retrieve Properties for a GedaPin Object
 * \par Function Description
 *  Gets the pin's properties.
 *
 * \param [in]  object    GedaObject to read the properties
 * \param [out] label     Ptr to Pin Label String.
 * \param [out] number    The Pin Number of the Pin object
 * \param [out] sequence  The Pin Sequence Number
 * \param [out] e_type    The Pin Electrical Attribute code
 * \param [out] m_type    The Pin Mechanical Attribute code
 * \param [out] n_type    The PIN_NODE type (Net or Bus type).
 *
 * \return TRUE on succes, FALSE otherwise
 */
bool geda_pin_object_get_attributes(GedaObject *object, const char **label,
                                                        const char **number,
                                                        int         *sequence,
                                                        PIN_ELECT   *e_type,
                                                        PIN_MECH    *m_type,
                                                        PIN_NODE    *n_type)
{
  bool        result;
  char       *pinnumber_str = NULL;
  char       *pinseq_str    = NULL;
  const char *pinlabel_str  = NULL;
  char       *pintype_str   = NULL;
  char       *mechtype_str  = NULL;

  result = (object->type == OBJ_PIN) ? TRUE : FALSE;

  if (result) {

    GList      *a_iter;

   *n_type = object->pin->node_type;    /* Update client's node type */

    a_iter = object->attribs;    /* Prime loop with Pin's attributes */

    while (a_iter != NULL) {

      GedaObject     *a_current;
      const char *str;

      a_current = a_iter->data;
      str       = a_current->text->string; /* get pointer attribute's text */

      if (str) {

        char *ptr;

        ptr = (char*) str;

        while (( *ptr != ASCII_NUL ) && ( *ptr != ASCII_EQUAL_SIGN )) { ptr++; } /* find "=" */

          if ( *ptr == ASCII_EQUAL_SIGN ) {

          int length;

          length = ptr - str;    /* is pointer offset to "=" */
          ptr++;                 /* pointer offset to first char of value */

          if(strncmp(str, "pinnumber", length) == 0 ) {
            pinnumber_str = ptr;
           *number = pinnumber_str;
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
                 *e_type = geda_pin_lookup_etype(pintype_str);
                }
                else {
                  if(strncmp(str, "mechtype", length) == 0) {
                    mechtype_str = ptr;
                   *m_type = geda_pin_lookup_mtype(mechtype_str);
                  }
                }
              }
            }
          }
        }
      }

      a_iter = a_iter->next;
    }
  }
  if(!pinnumber_str)
    *number = NULL;

  if(!pinseq_str)
    *sequence = -1;

  if(!pinlabel_str)
    *label = NULL;

  if(!pintype_str)
    *e_type = -1; /* was = PIN_ELECT_VOID */

  if(!mechtype_str)
    *m_type = -1;

  return result;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void geda_pin_object_set_attributes(GedaObject *object, const char *label_str,
                                                        const char *number,
                                                        int         sequence,
                                                        PIN_ELECT   e_type,
                                                        PIN_MECH    m_type,
                                                        PIN_NODE    n_type)
{
  if (object != NULL && object->type == OBJ_PIN) {

    GedaPin *pin = object->pin;

    geda_attrib_object_freeze_hooks(object);
    geda_struct_conn_remove_object (object);

    GedaObject *bute;
    pin->node_type = n_type;

    /* pin number */
    if (number && geda_pin_set_number(pin, number)) {
      bute = geda_attrib_first_attrib_by_name (object, "pinnumber");
      if(bute != NULL) {
        geda_attrib_object_set_value(bute, "pinnumber",  number);
        geda_text_object_recreate(bute);
      }
      else {
        bute = geda_pin_object_create_number_attrib (NULL, object, number, -1, -1);
        if (bute && object->page) {
          geda_struct_page_append_object(object->page, bute);
          bute->page = object->page;
        }
      }
    }

    /* pin sequence */
    char *str_seq = geda_sprintf ("%d", sequence);

    if (geda_pin_set_sequence(pin, str_seq)) {
      bute = geda_attrib_first_attrib_by_name (object, "pinseq");
      if(bute !=NULL) {
        geda_attrib_object_set_value(bute, "pinseq", str_seq);
        geda_text_object_recreate(bute);
      }
      else {
        bute = geda_pin_object_create_seq_attrib (NULL, object, sequence, -1, -1);
        if (bute && object->page) {
          geda_struct_page_append_object(object->page, bute);
          bute->page = object->page;
        }
      }
    }
    GEDA_FREE(str_seq);

#if DEBUG
    fprintf(stderr, "%s: pinlabel=<%s>\n", __func__, label_str);
#endif

    /* pin label */
    if (label_str && geda_pin_set_label(pin, label_str)) {
      bute = geda_attrib_first_attrib_by_name (object, "pinlabel");
      if(bute !=NULL && bute->type == OBJ_TEXT) {
        geda_attrib_object_set_value(bute, "pinlabel", (char*)label_str);
        geda_text_object_recreate(bute);
      }
      else {
        bute = geda_pin_object_create_label_attrib (NULL, object, label_str, -1, -1);
        if (bute && object->page) {
          geda_struct_page_append_object(object->page, bute);
          bute->page = object->page;
        }
      }
    }

    if ((int)e_type >= 0) {
      const char *electrical = geda_pin_lookup_estring(e_type);
      if (pin->electrical == NULL || (strcmp(pin->electrical, electrical) != 0))
        geda_pin_set_electrical(pin, electrical);
      bute = geda_attrib_first_attrib_by_name (object, "pintype");
      if (bute !=NULL) {
        geda_attrib_object_set_value(bute, "pintype", (char*)electrical);
        geda_text_object_recreate(bute);
      }
      else {
        bute = geda_pin_object_create_elect_attrib (NULL, object, electrical, -1, -1);
        if (bute && object->page) {
          geda_struct_page_append_object(object->page, bute);
          bute->page = object->page;
        }
      }
    }

    if ((int)m_type >= 0) {
      const char *mechanical = geda_pin_lookup_mstring(m_type);
      if (pin->mechanical == NULL || (strcmp(pin->mechanical, mechanical) != 0))
        geda_pin_set_mechanical(pin, mechanical);
      bute = geda_attrib_first_attrib_by_name (object, "mechtype");
      if(bute !=NULL) {
        geda_attrib_object_set_value(bute, "mechtype", (char*)mechanical);
        geda_text_object_recreate(bute);
      }
      else {
        bute = geda_pin_object_create_mech_attrib (NULL, object, mechanical, -1, -1);
        if (bute && object->page) {
          geda_struct_page_append_object(object->page, bute);
          bute->page = object->page;
        }
      }
    }
    geda_struct_conn_update_linear_object (object);
    geda_attrib_object_thaw_hooks (object);
  }
}

/*!
 * \brief Create a New Electrical Type Attribute for a GedaPin
 * \par Function Description
 *  This function creates a new text attribute for the pintype. If the
 *  optional top-level object is present, the attribute offset will be
 *  set based on global top-level settings, otherwise the text will be
 *  positioned at a fixed default offset relative to the X and Y arguments,
 *  if X and Y are both not less than zero, otherwise the offset will be
 *  relative to the active end of the given pin object. If descr string
 *  is NULL then NULL is returned. The text angle and justification will
 *  be set based on the orientation of the pin.
 *
 * \param [in] toplevel The GedaToplevel object, can be NULL
 * \param [in] object   The pin object for which the attribute was being added.
 * \param [in] descr    Normally a member of #e_strings
 * \param [in] x        Desired X location for the label
 * \param [in] y        Desired Y location for the label
 *
 * \returns Pointer to new pintype attribute or NULL if label was NULL
 *
 *  example: geda_pin_object_create_elect_attrib (NULL, object, "pas", -1, -1);
 *
 * \sa geda_pin_object_create_mech_attrib
 */
GedaObject *
geda_pin_object_create_elect_attrib(GedaToplevel *toplevel,
                                      GedaObject *object,
                                      const char *descr,
                                             int  x,
                                             int  y)
{
  char *text;
  int   align = -1;
  int   offset;
  int   size;
  int   x_pos, y_pos;

  if (descr == NULL && object->pin->electrical == NULL) {
    return NULL;
  }

  if (toplevel) { /* if was passed a toplevel configuration */
    offset = toplevel->attribute_offset;
    size   = toplevel->attribute_font_size;
  }
  else { /* fallback to compiled-in value */
    offset = DEFAULT_ATTRIBUTE_OFFSET;
    size   = DEFAULT_ATTRIBUTE_SIZE;
  }

  geda_pin_object_normalize(object);

  if ( x < 0 && y < 0 ){

    /* locate description relative to the connected end */
    x_pos = object->line->x[object->pin->whichend];
    y_pos = object->line->y[object->pin->whichend];

    /* Pin is horizontal*/
    if (object->line->y[0] == object->line->y[1]) {

        y_pos = y_pos - offset;

      /* left to right */
      if (object->line->x[1] > object->line->x[0]) {
        align = UPPER_LEFT;
      }
      /* right to left */
      else if (object->line->x[0] > object->line->x[1]) {
        align = UPPER_RIGHT;
      }
    }
    /* Pin is vertical */
    else if (object->line->x[0] == object->line->x[1]) {

      x_pos = x_pos + offset;
      align = UPPER_LEFT;
    }
  }
  else {
     x_pos = x;
     y_pos = y;
  }

  if ( align < 0 ) align = MIDDLE_MIDDLE;
  if ( x_pos < 0 ) x_pos = object->line->x[object->pin->whichend];
  if ( y_pos < 0 ) y_pos = object->line->y[object->pin->whichend];

  if (descr) {
    text = geda_utility_string_concat("pintype", "=", descr, NULL);
  }
  else {
    text = geda_utility_string_concat("pintype", "=", object->pin->electrical, NULL);
  }

  GedaObject *new_bute;

  new_bute = geda_text_object_new (ATTRIBUTE_COLOR, x_pos, y_pos, align, 0,
                         size, INVISIBLE, SHOW_VALUE, text);

  if (toplevel) {
    geda_text_object_set_rendered_bounds_func (new_bute,
                                    toplevel->rendered_text_bounds_func,
                                    toplevel->rendered_text_bounds_data);
  }

  geda_attrib_object_add(object, new_bute);

  GEDA_FREE(text);

  return new_bute;
}

/*!
 * \brief Create a New Pin Label Attribute for a GedaPin
 * \par Function Description
 *  This function creates a new text attribute for a pin label. If the
 *  optional top-level object is present, the attribute offset will be
 *  set based on global top-level settings, otherwise the text position
 *  will be a fixed default offset relative to the X and Y arguments, if
 *  X and Y are both not less than zero, otherwise the offset will be
 *  relative to the active end of the given pin object. The text angle
 *  and justification will be set based on the orientation of the pin.
 *
 * \param [in] toplevel The GedaToplevel object, can be NULL
 * \param [in] object   The pin object for which the attribute was being added.
 * \param [in] label    The attribute object being attached to the pin
 * \param [in] x        Desired X location for the label
 * \param [in] y        Desired Y location for the label
 *
 * \returns Pointer to new pinlabel attribute or NULL no label string is set
 *
 *  example: geda_pin_object_create_label_attrib (toplevel, object, label_str, -1, -1);
 *
 * \sa geda_pin_object_create_number_attrib geda_pin_object_create_seq_attrib
 */
GedaObject *
geda_pin_object_create_label_attrib(GedaToplevel *toplevel,
                                      GedaObject *object,
                                      const char *label,
                                             int  x,
                                             int  y)
{
  char *text;
  int   align = -1;
  int   angle = -1;
  int   offset;
  int   size;
  int   x_pos, y_pos;

  if (label == NULL && object->pin->label == NULL) {
    return NULL;
  }

  if (toplevel) { /* if was passed a toplevel configuration */
    offset = toplevel->attribute_offset;
    size   = toplevel->attribute_font_size;
  }
  else { /* fallback to compiled-in value */
    offset = DEFAULT_ATTRIBUTE_OFFSET;
    size   = DEFAULT_ATTRIBUTE_SIZE;
  }

  geda_pin_object_normalize(object);

  if ( x < 0 && y < 0 ){

    /* locate labels relative to the non-connected end */
    x_pos = object->line->x[!object->pin->whichend];
    y_pos = object->line->y[!object->pin->whichend];

    /* Pin is horizontal*/
    if (object->line->y[0] == object->line->y[1]) {

      angle = 0;

      /* left to right */
      if (object->line->x[1] > object->line->x[0]) {
        align = MIDDLE_RIGHT;
        x_pos = x_pos - offset;
      }
      /* right to left */
      else if (object->line->x[0] > object->line->x[1]) {
        align = MIDDLE_LEFT;
        x_pos = x_pos + offset;
      }
    }
    /* Pin is vertical */
    else if (object->line->x[0] == object->line->x[1]) {
      angle = 90;

      /* botttom to top */
      if (object->line->y[1] > object->line->y[0]) {
        align = MIDDLE_RIGHT;
        y_pos = y_pos - offset;
      }

      /* top to botttom */
      else if (object->line->y[0] > object->line->y[1]) {
        align = MIDDLE_LEFT;
        y_pos = y_pos + offset;
      }
    }
  }
  else {
     x_pos = x;
     y_pos = y;
  }

  if ( angle < 0 ) angle = 0;
  if ( align < 0 ) align = MIDDLE_MIDDLE;
  if ( x_pos < 0 ) x_pos = object->line->x[!object->pin->whichend];
  if ( y_pos < 0 ) y_pos = object->line->y[!object->pin->whichend];

  if (label)
    text = geda_utility_string_concat("pinlabel", "=", label, NULL);
  else
    text = geda_utility_string_concat("pinlabel", "=", object->pin->label, NULL);

  GedaObject *new_bute;

  new_bute = geda_text_object_new (ATTRIBUTE_COLOR, x_pos, y_pos, align, angle,
                         size, VISIBLE, SHOW_VALUE, text);

  if (toplevel) {
    geda_text_object_set_rendered_bounds_func (new_bute,
                                    toplevel->rendered_text_bounds_func,
                                    toplevel->rendered_text_bounds_data);
  }

  geda_attrib_object_add(object, new_bute);

  GEDA_FREE(text);

  return new_bute;
}

/*!
 * \brief Create a New Pin Mechanical Type Attribute for GedaObject
 * \par Function Description
 *  This function creates a new text attribute for the mechtype. If the
 *  optional top-level object is present, the attribute offset will be
 *  set based on global top-level settings, otherwise the text will be
 *  positioned at a fixed default offset relative to the X and Y arguments,
 *  if X and Y are both not less than zero, otherwise the offset will be
 *  relative to the active end of the given pin object. If descr string
 *  is NULL then NULL is returned. The text angle and justification will
 *  be set based on the orientation of the pin.
 *
 * \param [in] toplevel The GedaToplevel object, can be NULL
 * \param [in] object   The pin object for which the attribute was being added.
 * \param [in] descr    Normally a member of #m_strings
 * \param [in] x        Desired X location for the label
 * \param [in] y        Desired Y location for the label
 *
 * \returns Pointer to new mechtype attribute or NULL if label was NULL
 *
 *  example: geda_pin_object_create_mech_attrib (NULL, object, "lead", -1, -1);
 *
 * \sa geda_pin_object_create_elect_attrib
 */
GedaObject *
geda_pin_object_create_mech_attrib(GedaToplevel *toplevel,
                                     GedaObject *object,
                                     const char *descr,
                                            int  x,
                                            int  y)
{
  GedaObject *new_bute;
  char   *text;
  int     align = -1;
  int     offset;
  int     size;
  int     x_pos, y_pos;

  if (descr == NULL && object->pin->mechanical == NULL) {
    return NULL;
  }

  if (toplevel) { /* if was passed a toplevel configuration */
    offset = toplevel->attribute_offset;
    size   = toplevel->attribute_font_size;
  }
  else { /* fallback to compiled-in value */
    offset = DEFAULT_ATTRIBUTE_OFFSET;
    size   = DEFAULT_ATTRIBUTE_SIZE;
  }

  geda_pin_object_normalize(object);

  if ( x < 0 && y < 0 ){

    /* locate description relative to the non-connected end */
    x_pos = object->line->x[!object->pin->whichend];
    y_pos = object->line->y[!object->pin->whichend];

    /* Pin is horizontal*/
    if (object->line->y[0] == object->line->y[1]) {

        y_pos = y_pos - offset;

      /* left to right */
      if (object->line->x[1] > object->line->x[0]) {
        align = UPPER_LEFT;
      }
      /* right to left */
      else if (object->line->x[0] > object->line->x[1]) {
        align = UPPER_RIGHT;
      }
    }
    /* Pin is vertical */
    else if (object->line->x[0] == object->line->x[1]) {

      x_pos = x_pos + offset;
      align = UPPER_LEFT;
    }
  }
  else {
     x_pos = x;
     y_pos = y;
  }

  if ( align < 0 ) align = MIDDLE_MIDDLE;
  if ( x_pos < 0 ) x_pos = object->line->x[!object->pin->whichend];
  if ( y_pos < 0 ) y_pos = object->line->y[!object->pin->whichend];

  if (descr)
    text = geda_utility_string_concat("mechtype", "=", descr, NULL);
  else
    text = geda_utility_string_concat("mechtype", "=", object->pin->mechanical, NULL);

  new_bute = geda_text_object_new (ATTRIBUTE_COLOR, x_pos, y_pos, align, 0,
                         size, INVISIBLE, SHOW_VALUE, text);

  if (toplevel) {
    geda_text_object_set_rendered_bounds_func (new_bute,
                                    toplevel->rendered_text_bounds_func,
                                    toplevel->rendered_text_bounds_data);
  }

  geda_attrib_object_add(object, new_bute);

  GEDA_FREE(text);
  return new_bute;
}

/*!
 * \brief Create a New Number Attribute for a GedaPin
 * \par Function Description
 *  This function creates a new text attribute for a pin number. If the
 *  optional top-level object is present, the attribute offset will be
 *  set based on global top-level settings, otherwise the text position
 *  will be at a fixed default offset relative to the X and Y arguments,
 *  if X and Y are both not less than zero, otherwise the offset will
 *  be relative to the active end of the given pin object. If the number
 *  is less then zero then the pin number will be set to the count of
 *  pin objects attached to object. The text angle and justification
 *  will be set based on the orientation of the pin.
 *
 * \param [in] toplevel The GedaToplevel object, can be NULL
 * \param [in] object   The pin object for which the attribute was being added.
 * \param [in] number   Pointer to string value of the pin number
 * \param [in] x        Desired X location for the label
 * \param [in] y        Desired Y location for the label
 *
 * \returns Pointer to new pinnumber attribute or NULL if label was NULL
 *
 * example: geda_pin_object_create_number_attrib (toplevel, object, pnum, -1, -1);
 *
 * \sa geda_pin_object_create_label_attrib geda_pin_object_create_seq_attrib
 */
GedaObject *geda_pin_object_create_number_attrib(GedaToplevel *toplevel,
                                                   GedaObject *object,
                                                   const char *number,
                                                          int  x,
                                                          int  y)
{
  GedaObject     *new_bute;
  const char *str_num;
        char *text;
        char  s_val[6];

  int     align = -1;
  int     offset;
  int     size;
  int     x_pos, y_pos;


  if (toplevel) { /* if was passed a toplevel configuration */
    offset = toplevel->attribute_offset;
    size   = toplevel->attribute_font_size;
  }
  else { /* fallback to compiled-in value */
    offset = DEFAULT_ATTRIBUTE_OFFSET;
    size   = DEFAULT_ATTRIBUTE_SIZE;
  }

  geda_pin_object_normalize(object);

  if ( x < 0 && y < 0 ) {

    /* locate pin numbers relative to the non-connected end */
    x_pos = object->line->x[!object->pin->whichend];
    y_pos = object->line->y[!object->pin->whichend];

    /* Pin is horizontal */
    if (object->line->y[0] == object->line->y[1]) {

      y_pos = y_pos + offset;  /* numbers go above the pin */

      /* left to right */
      if (object->line->x[1] > object->line->x[0]) {
        align = LOWER_LEFT;
        x_pos = x_pos + offset;
      }
      /* right to left */
      else if (object->line->x[0] > object->line->x[1]) {
        align = LOWER_RIGHT;
        x_pos = x_pos - offset;
      }
    }
    /* Pin is vertical */
    else if (object->line->x[0] == object->line->x[1]) {

      x_pos = x_pos - offset;  /* numbers go on left side of pins */

      /* botttom to top  */
      if (object->line->y[1] > object->line->y[0]) {
        align = LOWER_RIGHT;
        y_pos = y_pos + offset;
      }

      /* top to botttom */
      else if (object->line->x[0] > object->line->x[1]) {
        align = UPPER_RIGHT;
        y_pos = y_pos - offset;
      }
    }
  }
  else {
     x_pos = x;
     y_pos = y;
  }

  if ( align < 0 ) align = MIDDLE_MIDDLE;
  if ( x_pos < 0 ) x_pos = object->line->x[!object->pin->whichend];
  if ( y_pos < 0 ) y_pos = object->line->y[!object->pin->whichend];

  if (number == NULL) {

    if (object->pin->number == NULL) {

      if(GEDA_IS_COMPLEX(object->parent_object)) {

        int value;

        value = g_list_length(object->parent_object->complex->pin_objs);
        geda_utility_string_int2str(value, s_val, 10);
        str_num = &s_val[0];
      }
      else {
        str_num = "1";
      }
    }
    else {
      str_num = object->pin->number;
    }
  }
  else {
    str_num = number;
  }

  text = geda_sprintf("pinnumber=%s", str_num);

  new_bute = geda_text_object_new (ATTRIBUTE_COLOR, x_pos, y_pos, align, 0,
                         size, VISIBLE, SHOW_VALUE, text);

  geda_attrib_object_add(object, new_bute);

  GEDA_FREE(text);

  return new_bute;
}

/*!
 * \brief Create a New Sequence Attribute for a GedaPin
 * \par Function Description
 *  This function creates a new text attribute for a pin sequence. If
 *  the optional top-level object is present, the attribute offset will
 *  be set based on global top-level settings, otherwise the text will be
 *  position at a fixed default offset relative to the X and Y arguments,
 *  if X and Y are both not less than zero, otherwise the offset will be
 *  relative to the active end of the given pin object. If label string
 *  is NULL then NULL is returned. The text angle and justification will
 *  be set based on the orientation of the pin.
 *
 * \param [in] toplevel The GedaToplevel object, can be NULL
 * \param [in] object   The pin object for which the attribute was being added.
 * \param [in] sequence Integer value of the pin sequence
 * \param [in] x        Desired X location for the label
 * \param [in] y        Desired Y location for the label
 *
 * \returns Pointer to new pinseq attribute or NULL if label was NULL
 *
 *  example: geda_pin_object_create_seq_attrib (NULL, object, seq, -1, -1);
 *
 * \sa geda_pin_object_create_label_attrib geda_pin_object_create_number_attrib
 */
GedaObject *geda_pin_object_create_seq_attrib(GedaToplevel *toplevel,
                                                GedaObject *object,
                                                       int  sequence,
                                                       int  x,
                                                       int  y)
{
  GedaObject *new_bute;
  char   *text;
  int     value;
  int     align = -1;
  int     offset;
  int     size;
  int     x_pos, y_pos;

  if (toplevel) { /* if was passed a toplevel configuration */
    offset = toplevel->attribute_offset;
    size   = toplevel->attribute_font_size;
  }
  else { /* fallback to compiled-in value */
    offset = DEFAULT_ATTRIBUTE_OFFSET;
    size   = DEFAULT_ATTRIBUTE_SIZE;
  }

  geda_pin_object_normalize(object);

  if ( x < 0 && y < 0 ) {

    /* locate pin numbers relative to the non-connected end */
    x_pos = object->line->x[!object->pin->whichend];
    y_pos = object->line->y[!object->pin->whichend];

    /* Pin is horizontal */
    if (object->line->y[0] == object->line->y[1]) {

      y_pos = y_pos + offset;  /* numbers go above the pin */

      /* left to right */
      if (object->line->x[1] > object->line->x[0]) {
        align = LOWER_LEFT;
        x_pos = x_pos + offset;
      }
      /* right to left */
      else if (object->line->x[0] > object->line->x[1]) {
        align = LOWER_RIGHT;
        x_pos = x_pos - offset;
      }
    }
    /* Pin is vertical */
    else if (object->line->x[0] == object->line->x[1]) {

      x_pos = x_pos - offset;  /* numbers go on left side of pins */

      /* botttom to top  */
      if (object->line->y[1] > object->line->y[0]) {
        align = LOWER_RIGHT;
        y_pos = y_pos + offset;
      }

      /* top to botttom */
      else if (object->line->x[0] > object->line->x[1]) {
        align = UPPER_RIGHT;
        y_pos = y_pos - offset;
      }
    }
  }
  else {
     x_pos = x;
     y_pos = y;
  }

  if ( align < 0 ) align = MIDDLE_MIDDLE;
  if ( x_pos < 0 ) x_pos = object->line->x[!object->pin->whichend];
  if ( y_pos < 0 ) y_pos = object->line->y[!object->pin->whichend];

  if (sequence < 1) {
    if (object->pin->sequence < 1) {
      if (object->pin->number == NULL) {
        if(GEDA_IS_COMPLEX(object->parent_object)) {
          value = g_list_length(object->parent_object->complex->pin_objs);
        }
        else {
          value = 1;
        }
      }
      else {
        object->pin->sequence = atoi(object->pin->number);
        value = object->pin->sequence;
      }
    }
    else {
      value = object->pin->sequence;
    }
  }
  else {
    value = sequence;
  }

  text = geda_sprintf("pinseq=%d", value);

  new_bute = geda_text_object_new (ATTRIBUTE_COLOR, x_pos, y_pos, align, 0,
                         size, INVISIBLE, SHOW_NAME_VALUE, text);

  geda_attrib_object_add(object, new_bute);

  GEDA_FREE(text);

  return new_bute;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GList *geda_pin_object_realize_attributes(GedaToplevel *toplevel, GedaObject *object)
{
  PIN_ELECT etype;
  PIN_MECH  mtype;
  PIN_NODE  ntype;  /* Does not get attribute but need for get_attributes */

  const char *label_str;
  const char *number;
        int   sequence;

  if (geda_pin_object_get_attributes(object, &label_str, &number, &sequence, &etype, &mtype, &ntype)) {

    GedaObject  *attrib;

    if (label_str == NULL) {
      attrib = geda_pin_object_create_label_attrib(toplevel, object, NULL, -1, -1);
      if (attrib && object->page) {
        geda_struct_page_append_object(object->page, attrib);
        attrib->page = object->page;
      }
    }
    if (number == NULL) {
      attrib = geda_pin_object_create_number_attrib(toplevel, object, NULL, -1, -1);
      if (attrib && object->page) {
        geda_struct_page_append_object(object->page, attrib);
        attrib->page = object->page;
      }
    }
    if (sequence < 0) {
      attrib = geda_pin_object_create_seq_attrib(toplevel, object, -1, -1, -1);
      if (attrib && object->page) {
        geda_struct_page_append_object(object->page, attrib);
        attrib->page = object->page;
      }
    }
    /* Note etype & mtype are enumerated, gcc does not eval -1 < 0, so cast to int */
    if ((int)etype < 0) {
      attrib = geda_pin_object_create_elect_attrib(toplevel, object, NULL, -1, -1);
      if (attrib && object->page) {
        geda_struct_page_append_object(object->page, attrib);
        attrib->page = object->page;
      }
    }
    if ((int)mtype < 0) {
      attrib = geda_pin_object_create_mech_attrib(toplevel, object, NULL, -1, -1);
      if (attrib && object->page) {
        geda_struct_page_append_object(object->page, attrib);
        attrib->page = object->page;
      }
    }
    return object->attribs;
  }

  return NULL;
}

/*!
 * \brief Update a GedaPin Object Property with Read Attribute GedaObject
 * \par Function Description
 *  This function is called from geda_attrib_object_read after a text
 *  attribute has been read in for pin object. The functions passes the
 *  attribute object to the GedaPin Object to set internal values.
 *
 * \param [in] o_pin   The pin object to which the attribute is being added.
 * \param [in] o_text  The attribute object being attached to the pin
 *
 * \note The pin object is not likely associated with a page or even attached
 *       to complex when this function is called by geda_attrib_object_read.
 */
void
geda_pin_object_update_read_property(GedaObject *o_pin, GedaObject *o_text)
{
  char *attrib;
  char *value;

  if (GEDA_IS_TEXT(o_text)) {
    if (geda_attrib_string_get_name_value(o_text->text->string, &attrib, &value)) {

      /* We skip the first 3 chars because "pin" as in "pinlabel" or
       * "pinnumber" is not part of the property name */
      if( strcmp(attrib, "pinnumber") == 0)
        g_object_set (o_pin,   attrib + 3, value, NULL);
      else if( strcmp(attrib, "pinlabel") == 0)
        g_object_set (o_pin,   attrib + 3, value, NULL);
      else if( strcmp(attrib, "pinseq") == 0)
        g_object_set (o_pin,  "sequence", value, NULL);
      else if( strcmp(attrib, "pintype") == 0)
        g_object_set (o_pin,  "pin-type", value, NULL);

      GEDA_FREE(attrib);
      GEDA_FREE(value);
      /* Note, not creating an attribute because geda_attrib_object_read already did */
    }
  }
}

/*!
 * \brief Retrieve a Pin Object Electrical Attributes
 * \par Function Description
 *  Returns the object->pin->electrical Attributes of \a object.
 *  The electrical
 */
const char *geda_pin_object_get_electrical(GedaObject *object)
{
  g_return_val_if_fail(GEDA_IS_PIN(object), NULL);

  return object->pin->electrical;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char *geda_pin_object_get_label(GedaObject *object)
{
  g_return_val_if_fail(GEDA_IS_PIN(object), NULL);

  return object->pin->label;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
const char *geda_pin_object_get_mechanical(GedaObject *object)
{
  g_return_val_if_fail(GEDA_IS_PIN(object), NULL);

  return object->pin->mechanical;
}

/** @} endgroup geda-pin-object-proc */
