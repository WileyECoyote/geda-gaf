/* C
 *  file:o_style.c
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * Copyright (C) 2011-2013 Wiley Edward Hill <wileyhill@gmail.com>
;;
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
;;
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
;;
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA7
*/
/*! \file o_style.c
 *  \brief functions to return width values based on style settings.
 *
 *  \remark: WEH: Styles system needs as revamp. These new functions
 *  were created to "temporarily" cleanup of the code as bit. The
 *  styles TAB in the new settings dialog could have variable widths
 *  adjustments for screen and printing, then there might not be any
 *  reason to have THIN and THICK styles any more.
 *
 */

#include "../../../config.h"
#include <stdio.h>

#include <libgeda_priv.h>

/*! \brief Return current bus width value
 *  \par Function Description
 *  This functions returns an integer value to be used as bus
 *  width based on the current bus style variable.
 *
 */
int geda_object_style_get_bus_width( GedaToplevel *toplevel )
{
  int width;

  if(toplevel->bus_style == STYLE_NONE) {
    width = DEFAULT_WIDTH_NONE;
  }
  else
  {
    if(toplevel->bus_style == STYLE_THIN) {
      width = toplevel->thin_bus_width;
    }
    else {
      if (toplevel->bus_style == STYLE_THICK) {
        width = toplevel->thick_bus_width;
      }
      else {
        width = MIN_LINE_WIDTH_THRESHOLD;
      }
    }
  }

  return width;
}
/*! \brief Return current line width value
 *  \par Function Description
 *  This functions returns an integer value to be used as line
 *  width based on the current line style variable.
 *
 */
int geda_object_style_get_line_width( GedaToplevel *toplevel ) {

  int width;

  if(toplevel->line_style == STYLE_NONE) {
    width = DEFAULT_WIDTH_NONE;
  }
  else
  {
    if(toplevel->line_style == STYLE_THIN) {
      width = toplevel->thin_line_width;
    }
    else {
      if (toplevel->line_style == STYLE_THICK) {
        width = toplevel->thick_line_width;
      }
      else {
        width = MIN_LINE_WIDTH_THRESHOLD;
      }
    }
  }
  return width;
}
/*! \brief Return current net width value
 *  \par Function Description
 *  This functions returns an integer value to be used as net
 *  width based on the current net style variable.
 *
 */
int geda_object_style_get_net_width( GedaToplevel *toplevel ) {

  int width;

  if(toplevel->net_style == STYLE_NONE) {
    width = DEFAULT_WIDTH_NONE;
  }
  else
  {
    if(toplevel->net_style == STYLE_THIN) {
      width = toplevel->thin_net_width;
    }
    else {
      if (toplevel->net_style == STYLE_THICK) {
        width = toplevel->thick_net_width;
      }
      else {
        width = MIN_LINE_WIDTH_THRESHOLD;
      }
    }
  }

  return width;
}
/*! \brief Return current pin width value
 *  \par Function Description
 *  This functions returns an integer value to be used as
 *  pin width based on the current style settings.
 *
 *  of the pin type.
 */
int geda_object_style_get_pin_width( GedaToplevel *toplevel, int type) {

  int width;

  if(toplevel->pin_style == STYLE_NONE) {
    if(type==PIN_NET_NODE) width = DEFAULT_THIN_PIN_WIDTH;
    else width = DEFAULT_THICK_PIN_WIDTH;
  }
  else
  {
    if(toplevel->pin_style == STYLE_THIN) {
      width = toplevel->thin_pin_width;
    }
    else {
      if (toplevel->pin_style == STYLE_THICK) {
        width = toplevel->thick_pin_width;
      }
      else { /* Should not get here! */
        width = MIN_LINE_WIDTH_THRESHOLD;
      }
    }
  }

  return width;
}
/*! \brief Set line_width of object based on style settings.
 *  \par Function Description
 *  This function will take an object and recalculate its
 *  position on the screen.
 *
 *  \param [in]     toplevel    The GedaToplevel object.
 *  \param [in,out] o_current   GedaObject to set.
 *
 */
void geda_object_style_set_line_width(GedaToplevel *toplevel, GedaObject *o_current)
{
  if (o_current != NULL) {
    switch(o_current->type) {
      case(OBJ_ARC):
      case(OBJ_BOX):
      case(OBJ_CIRCLE):
      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_BUS):
      case(OBJ_PATH):
        if(toplevel->line_style != STYLE_NONE)
          o_current->line_options->line_width = geda_object_style_get_line_width( toplevel );
        break;
      case(OBJ_PICTURE):
      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        break;
      case(OBJ_PIN):
        if(toplevel->pin_style != STYLE_NONE) {
          if(o_current->pin->node_type == PIN_NET_NODE)
            o_current->line_options->line_width = geda_object_style_get_pin_width( toplevel, PIN_NET_NODE);
          else
            o_current->line_options->line_width = geda_object_style_get_pin_width( toplevel, PIN_BUS_NODE);
        }
        break;
      case(OBJ_TEXT):
        break;
    }
  }
}
