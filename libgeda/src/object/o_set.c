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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

/*! \file o_basic.c
 *  \brief functions for the basic object type
 *
 *  This file contains the code used to manipulate <b>Objects</b>.
 *  The object is the basic type of all elements stored in schematic
 *  and symbol files.
 *
 *  The <b>Object</b> be extended to become concrete objects like a line,
 *  a pin, text, a circle or a picture. These extentions are substructures
 *  in the object struct.
 *  The <b>Subojects</b> are pictures (st_picture), paths (st_path), arcs (st_arc),
 *  a lines (st_line), boxes (st_box), circles (st_circle), texts (st_text) and
 *  a the complex type (st_complex).
 *
 *  Pins, nets and busses are types of line <b>Objects</b>.
 *
 *  The <b>Complex Objects</b> can be linked to many primary objects. If the <b>Complex
 *  Object</b> is a symbol, then the complex symbol contains all the pins,
 *  the text and the graphics.
 *
 *  \image html o_object_relations.png
 *  \image latex o_object_relations.pdf "object relations" width=14cm
 */

#include <config.h>
#include <stdio.h>

#include "libgeda_priv.h"


/*! \brief Set an #Object's line options.
 *  \par Function Description
 *  This function allows a line's end, type, width, length and space to be set.
 *  See LINE_END and LINE_TYPE for information on valid
 *  object end and type values.
 *
 *  \param [in,out] object       Object to set line options on.
 *  \param [in]     line_options A option data structure.
 *
 *  \todo Make space an unsigned int and check for a max value instead.
 *        If a max value is not required, then it would simplify the code.
 */
void o_set_line_options(Object *object, LINE_OPTIONS *line_options)
{
  g_return_if_fail( GEDA_IS_LINE(object)   ||
                    GEDA_IS_CIRCLE(object) ||
                    GEDA_IS_ARC(object)    ||
                    GEDA_IS_BOX(object)    ||
                    GEDA_IS_PATH(object));

  /* do some error checking / correcting */
  switch(line_options->line_type) {
    case(TYPE_DOTTED):
    if (line_options->line_space < 1) {
      line_options->line_space = default_line_space;
      u_log_message (_("Invalid space specified, setting to default=%d\n"),
                     default_line_space);
    }
    break;
    case(TYPE_DASHED):
    case(TYPE_CENTER):
    case(TYPE_PHANTOM):
    if (line_options->line_length < 1) {
      line_options->line_length = default_line_length;
      u_log_message (_("Invalid length specified, setting to default=%d\n"),
                     default_line_length);
    }
    if (line_options->line_space < 1) {
      line_options->line_space = default_line_space;
      u_log_message (_("Invalid space specified, setting to  default=%d\n"),
                     default_line_space);
    }
    break;
    default:

    break;
  }

  object->line_options->line_width  = line_options->line_width;
  object->line_options->line_end    = line_options->line_end;
  object->line_options->line_type   = line_options->line_type;
  object->line_options->line_length = line_options->line_length;
  object->line_options->line_space  = line_options->line_space;

}

/*! \brief Set #Object's fill options.
 *  \par Function Description
 *  This function allows an #Object's fill options to be configured.
 *  See OBJECT_FILLING for information on valid fill types.
 *
 *  \param [in,out]  object         Object to be updated.
 *  \param [in]      fill_options   OBJECT_FILLING type.
 *
 */
void
o_set_fill_options(Object *object, FILL_OPTIONS *fill_options)
{
  g_return_if_fail( GEDA_IS_BOX(object)    ||
                    GEDA_IS_CIRCLE(object) ||
                    GEDA_IS_ARC(object)    ||
                    GEDA_IS_PATH(object));

  object->fill_options->fill_type   = fill_options->fill_type;
  object->fill_options->fill_width  = fill_options->fill_width;
  object->fill_options->fill_pitch1 = fill_options->fill_pitch1;
  object->fill_options->fill_angle1 = fill_options->fill_angle1;
  object->fill_options->fill_pitch2 = fill_options->fill_pitch2;
  object->fill_options->fill_angle2 = fill_options->fill_angle2;
}

/*! \brief Change the color of an object
 *
 *  \par Function Description
 *  This function changes the color of an object.
 *
 *  \param [in] object    The Object to change color.
 *  \param [in] color     The new color.
 */
void o_set_color (Object *object, int color)
{
  if (GEDA_IS_OBJECT(object)) {

    object->color = color;

    if (object->type == OBJ_COMPLEX || object->type == OBJ_PLACEHOLDER)
      o_glist_set_color (object->complex->prim_objs, color);
  }
  else {
    BUG_TRACE ("object is not a GedaObject");
  }
}

/*! \brief Set visibility of the object.
 *  \par Function Description
 *  Set value of visibility field within the object.
 *  If resulting visibility value is changed,
 *  invalidate the bounds of the object and parent objects.
 *
 *  \param object     The #Object structure to be modified
 *  \param visibility Boolean desired state
 */
void
o_set_visibility (Object *object, int visibility)
{
  if(GEDA_IS_OBJECT(object)) {
    if (object->visibility != visibility) {
      object->visibility = visibility;
      o_bounds_invalidate (object);
    }
  }
  else {
    BUG_TRACE ("object is not a GedaObject");
  }
}

