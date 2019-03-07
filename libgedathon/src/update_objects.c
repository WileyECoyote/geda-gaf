/* C source                                       -*- update_objects.c -*- */
/*!
 * \file update_objects.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedathon - gEDA's Python API Extension library
 *
 * Copyright (C) 2013-2015 Wiley Edward Hill
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
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 17, 2013
 */

#include <Python.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

#include <geda_py_struct.h>
#include <geda_capsule.h>

static int
PyGeda_update_arc(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaArcObject *py_arc    = (PyGedaArcObject*)py_object;
  object->arc->x             = py_arc->x;
  object->arc->y             = py_arc->y;
  object->arc->radius        = py_arc->radius;
  object->arc->start_angle   = py_arc->start_angle;
  object->arc->arc_sweep     = py_arc->arc_sweep;

  //COLOR color;
  //COLOR locked_color;
  object->fill_options->fill_type   = py_arc->fill_type;
  object->fill_options->fill_width  = py_arc->fill_width;
  object->fill_options->fill_angle1 = py_arc->fill_angle1;
  object->fill_options->fill_pitch1 = py_arc->fill_pitch1;
  object->fill_options->fill_angle2 = py_arc->fill_angle2;
  object->fill_options->fill_pitch2 = py_arc->fill_pitch2;

  object->line_options->line_end   = py_arc->line_end;
  object->line_options->line_type  = py_arc->line_type;
  object->line_options->line_width = py_arc->line_width;
  object->line_options->line_space = py_arc->line_space;
  return 1;
}

static int
PyGeda_update_box(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaBoxObject *py_box          = (PyGedaBoxObject*)py_object;
  object->box->upper_x       = py_box->upper_x;
  object->box->upper_y       = py_box->upper_y;
  object->box->lower_x       = py_box->lower_x;
  object->box->lower_y       = py_box->lower_y;
  //COLOR color;
  //COLOR locked_color;
  object->fill_options->fill_type   = py_box->fill_type;
  object->fill_options->fill_width  = py_box->fill_width;
  object->fill_options->fill_angle1 = py_box->fill_angle1;
  object->fill_options->fill_pitch1 = py_box->fill_pitch1;
  object->fill_options->fill_angle2 = py_box->fill_angle2;
  object->fill_options->fill_pitch2 = py_box->fill_pitch2;

  object->line_options->line_end    = py_box->line_end;
  object->line_options->line_type   = py_box->line_type;
  object->line_options->line_width  = py_box->line_width;
  object->line_options->line_space  = py_box->line_space;
  return 1;
}

static int
PyGeda_update_bus(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaBusObject *py_bus          = (PyGedaBusObject*)py_object;
  object->line->x[0]         = py_bus->x[0];
  object->line->y[0]         = py_bus->y[0];
  object->line->x[1]         = py_bus->x[1];
  object->line->y[1]         = py_bus->y[1];
  //COLOR color;
  //COLOR locked_color;
  object->line_options->line_width = py_bus->line_width;
  return 1;
}

static int
PyGeda_update_circle(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaCircleObject *py_circle     = (PyGedaCircleObject*)py_object;
  object->circle->center_x          = py_circle->center_x;
  object->circle->center_y          = py_circle->center_y;
  object->circle->radius            = py_circle->radius;

  //COLOR color;
  //COLOR locked_color;
  object->fill_options->fill_type   = py_circle->fill_type;
  object->fill_options->fill_width  = py_circle->fill_width;
  object->fill_options->fill_angle1 = py_circle->fill_angle1;
  object->fill_options->fill_pitch1 = py_circle->fill_pitch1;
  object->fill_options->fill_angle2 = py_circle->fill_angle2;
  object->fill_options->fill_pitch2 = py_circle->fill_pitch2;

  object->line_options->line_end    = py_circle->line_end;
  object->line_options->line_type   = py_circle->line_type;
  object->line_options->line_width  = py_circle->line_width;
  object->line_options->line_space  = py_circle->line_space;
  return 1;
}

static int
PyGeda_update_complex(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaComplexObject *py_complex = (PyGedaComplexObject*)py_object;

  object->complex->is_embedded = py_complex->embedded;
  object->complex->x           = py_complex->x;
  object->complex->y           = py_complex->y;
  object->complex->angle       = py_complex->angle;
  object->complex->mirror      = py_complex->mirror;

  return 1;
}

static int
PyGeda_update_line(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaLineObject *py_line  = (PyGedaLineObject*)py_object;

  object->line->x[0]         = py_line->x[0];
  object->line->y[0]         = py_line->y[0];
  object->line->x[1]         = py_line->x[1];
  object->line->y[1]         = py_line->y[1];
  //COLOR color;
  //COLOR locked_color;
  object->line_options->line_end   = py_line->line_end;
  object->line_options->line_type  = py_line->line_type;
  object->line_options->line_width = py_line->line_width;
  object->line_options->line_space = py_line->line_space;
  return 1;
}

static int
PyGeda_update_net(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaNetObject *py_net    = (PyGedaNetObject*)py_object;

  object->line->x[0]         = py_net->x[0];
  object->line->y[0]         = py_net->y[0];
  object->line->x[1]         = py_net->x[1];
  object->line->y[1]         = py_net->y[1];
  //COLOR color;
  //COLOR locked_color;
  return 1;
}

static int
PyGeda_update_path(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaPathObject *py_path = (PyGedaPathObject*)py_object;

#ifdef DEBUG
  if (py_path->dirty_string) {
    const char *str = PyString_AsString(py_path->path_string);
    fprintf(stderr, "%s: string <%s> is dirty", __func__, str);
  }
#endif

  object->fill_options->fill_type   = py_path->fill_type;
  object->fill_options->fill_width  = py_path->fill_width;
  object->fill_options->fill_angle1 = py_path->fill_angle1;
  object->fill_options->fill_pitch1 = py_path->fill_pitch1;
  object->fill_options->fill_angle2 = py_path->fill_angle2;
  object->fill_options->fill_pitch2 = py_path->fill_pitch2;

  object->line_options->line_end   = py_path->line_end;
  object->line_options->line_type  = py_path->line_type;
  object->line_options->line_width = py_path->line_width;
  object->line_options->line_space = py_path->line_space;
  return 1;
}

static int
PyGeda_update_picture(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaPictureObject *py_picture  = (PyGedaPictureObject*)py_object;

  object->picture->angle           = py_picture->angle;
  object->picture->upper_x         = py_picture->upper_x;
  object->picture->upper_y         = py_picture->upper_y;
  object->picture->lower_x         = py_picture->lower_x;
  object->picture->lower_y         = py_picture->lower_y;
  return 1;
}

static int
PyGeda_update_pin(GedaObject *object, PyGedaObject *py_object )
{
  PyGedaPinObject *py_pin = (PyGedaPinObject*)py_object;
  const char *str;

  if (py_pin->dirty_electrical) {
    str = PyString_AsString(py_pin->electrical);
    geda_pin_set_electrical(object->pin, str);
    py_pin->dirty_electrical = 0;
  }

  if (py_pin->dirty_label) {
    str = PyString_AsString(py_pin->label);
    geda_pin_set_label(object->pin, str);
    py_pin->dirty_label = 0;
  }

  if (py_pin->dirty_label) {
    str = PyString_AsString(py_pin->label);
    geda_pin_set_label(object->pin, str);
    py_pin->dirty_label = 0;
  }

  if (py_pin->dirty_mechanical) {
    str = PyString_AsString(py_pin->mechanical);
    geda_pin_set_label(object->pin, str);
    py_pin->dirty_mechanical = 0;
  }

  object->pin->sequence            = py_pin->sequence;
  object->pin->whichend            = py_pin->whichend;   /* either 0 or 1 */
  object->pin->node_type           = py_pin->node_type;  /* either NET or BUS */;
  object->pin->mech_type           = py_pin->mech_type;
  object->pin->elect_type          = py_pin->elect_type;

  object->line->x[0]               = py_pin->x[0];
  object->line->y[0]               = py_pin->y[0];
  object->line->x[1]               = py_pin->x[1];
  object->line->y[1]               = py_pin->y[1];

  return 1;
}

static int
PyGeda_update_text(GedaObject *object, PyGedaObject *py_object)
{
  PyGedaTextObject *py_text        = (PyGedaTextObject*)py_object;

  object->text->x                  = py_text->x;
  object->text->y                  = py_text->y;
  object->text->size               = py_text->size;
  object->text->alignment          = py_text->alignment;
  object->text->angle              = py_text->angle;

  object->show_name_value          = py_text->show;
  object->visibility               = py_text->visible;

  if (py_text->dirty_text) {
    const char *str = PyString_AsString(py_text->string);
    geda_text_object_set_string(object, str);
    py_text->dirty_text = 0;
  }
  return 1;
}

/* This function checks the "dirty flags and calls the appropriate
 * object updater to synchronize the object with the current python
 * object. This allows the python object to be modified before the
 * object is committed to a page */
int
PyGeda_update_object(GedaObject *object, PyGedaObject *py_object )
{
  if (GEDA_IS_OBJECT(object)) {

    object->selectable = !py_object->locked;

    if (py_object->dirty) {

      py_object->dirty = 0;

      switch(object->type) {
        case OBJ_NET:
          return PyGeda_update_net(object, py_object);
        case OBJ_TEXT:
          return PyGeda_update_text(object, py_object);
        case OBJ_COMPLEX:
          return PyGeda_update_complex(object, py_object);
        case OBJ_PIN:
          return PyGeda_update_pin(object, py_object);
        case OBJ_CIRCLE:
          return PyGeda_update_circle(object, py_object);
        case OBJ_LINE:
          return PyGeda_update_line(object, py_object);
        case OBJ_BUS:
          return PyGeda_update_bus(object, py_object);
        case OBJ_BOX:
          return PyGeda_update_box(object, py_object);
        case OBJ_ARC:
          return PyGeda_update_arc(object, py_object);
        case OBJ_PATH:
          return PyGeda_update_path(object, py_object);
        case OBJ_PICTURE:
          return PyGeda_update_picture(object, py_object);
        default:
          break;
      }
    }
    return 1;
  }
  return 0;
}

/* ----------------------*   Attributes   *---------------------- */
static int
PyGeda_update_net_butes(GedaObject *object, PyGedaObject *py_object )
{
  //PyGedaNetObject *py_net          = (PyGedaNetObject*)py_object;
  return 1;
}
static int
PyGeda_update_pin_butes(GedaObject *object, PyGedaObject *py_object )
{
  //PyGedaPinObject *py_pin           = (PyGedaPinObject*)py_object;
  GedaObject *attrib;
  char       *value;

  inline bool is_number(char *str) {
    char *ptr = str;
    while (*ptr) {
      if (!isdigit(*ptr)) {
        return 0;
      }
      ptr++;
    }
    return 1;
  }

  attrib = geda_attrib_first_attrib_by_name (object, "pinnumber");
  if (attrib) {
    value = strstr(attrib->text->string, "=");
    value++;
    if (value != NULL) {
      if (strcmp(object->pin->number, value) != 0) {
        geda_attrib_object_set_value(attrib, "pinnumber", object->pin->number);
        geda_text_object_recreate(attrib);
      }
    }
  }

  attrib = geda_attrib_first_attrib_by_name (object, "pinseq");
  if (attrib) {

    value = strstr(attrib->text->string, "=");
    value++;

    if (is_number(value)) {

      int number = atoi(value);

      if (number != object->pin->sequence) {
        geda_attrib_object_set_integer_value(attrib, "pinseq", object->pin->sequence);
        geda_text_object_recreate(attrib);
      }
    }
  }

  attrib = geda_attrib_first_attrib_by_name (object, "pinlabel");

  if (attrib) {
    value = strstr(attrib->text->string, "=");
    value++;
    if (value != NULL) {
      if (strcmp(object->pin->label, value) != 0) {
        geda_attrib_object_set_value(attrib, "pinlabel", object->pin->label);
        geda_text_object_recreate(attrib);
      }
    }
  }

  attrib = geda_attrib_first_attrib_by_name (object, "pintype");

  if (attrib) {
    value = strstr(attrib->text->string, "=");
    value++;
    if (value != NULL) {
      if (strcmp(object->pin->electrical, value) != 0) {
        geda_attrib_object_set_value(attrib, "pintype", object->pin->electrical);
        geda_text_object_recreate(attrib);
      }
    }
  }

  attrib = geda_attrib_first_attrib_by_name (object, "mechtype");

  if (attrib) {
    value = strstr(attrib->text->string, "=");
    value++;
    if (value != NULL) {
      if (strcmp(object->pin->mechanical, value) != 0) {
        geda_attrib_object_set_value(attrib, "mechtype", object->pin->mechanical);
        geda_text_object_recreate(attrib);
      }
    }
  }
  return 1;
}

static int
PyGeda_update_complex_butes(GedaObject *object, PyGedaObject *py_object )
{
  GList *butes = NULL;
  int    count = 0;

  if (PyList_Check(py_object->attributes)) {

    int i;

    count = (int) PyList_GET_SIZE(py_object->attributes);

    for (i = 0; i < count ; i++) {

      GedaCapsule *capsule  = (GedaCapsule*)PyList_GET_ITEM(py_object->attributes, i);

      if (GEDA_IS_TEXT(capsule->object)) {

        GedaObject *attrib = capsule->object;

        butes  = g_list_append(butes, attrib);

#ifdef DEBUG
        fprintf(stderr, "%s: adding %s\n", __func__, attrib->text->string);
#endif

      }
      else {
        fprintf(stderr, "%s: invalid capsule contents\n", __func__);
      }
    }
  }

  if (count > 0) {
    g_list_free(object->attribs);
    object->attribs = butes;
  }

#ifdef DEBUG
  else {
    fprintf(stderr, "%s: no attributes to update\n", __func__);
  }
#endif

  while (butes != NULL) {
    GedaObject *o_attrib = (GedaObject*)butes->data;
    geda_text_object_recreate (o_attrib);
    NEXT (butes);
  }; /* wend*/

  return 1;
}

static int
PyGeda_update_bus_butes(GedaObject *object, PyGedaObject *py_object )
{
  //PyGedaBusObject *py_bus          = (PyGedaBusObject*)py_object;
  return 1;
}

/* Called to update attributes of an objects */
int
PyGeda_update_butes(GedaObject *object, PyGedaObject *py_object )
{
  if (PyGeda_update_object(object, py_object)) {
    switch(object->type) {
      case OBJ_NET:
        return PyGeda_update_net_butes(object, py_object);
      case OBJ_COMPLEX:
        return PyGeda_update_complex_butes(object, py_object);
      case OBJ_PIN:
        return PyGeda_update_pin_butes(object, py_object);
      case OBJ_BUS:
        return PyGeda_update_bus_butes(object, py_object);
      case OBJ_TEXT:
      case OBJ_CIRCLE:
      case OBJ_LINE:
      case OBJ_BOX:
      case OBJ_ARC:
      case OBJ_PATH:
      case OBJ_PICTURE:
        break;
    }
    return 1;
  }
  return 0;
}