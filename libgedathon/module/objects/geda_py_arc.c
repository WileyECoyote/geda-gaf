/* C source                                          -*- geda_py_arc.c -*- */
/*!
 * \file geda_py_arc.c
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
#include <structmember.h>

#include <geda/geda.h>
#include "../../include/geda_python.h"
#include "../../include/geda_py_struct.h"
#include "../../include/geda_py_object.h"
#include "../../include/geda_py_color.h"
#include "../../include/geda_py_docs.h"

static PyObject* arc_module;
static PyObject* geda_module;

static char PyGedaArcObject_doc[] = PyDoc_STR("Geda Arc: x, y, radius, start_angle, arc_sweep [, color]");

/* ------------------------- PyGedaArcObject Constructor ------------------------- */

static PyObject *Arc_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyGedaArcObject *self;
  struct { int r; int g; int b; int a; }
  default_color = DEFAULT_ARC_COLOR;

  self = (PyGedaArcObject*)(PyGedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    self->x           =  0;
    self->y           =  0;
    self->radius      =  1;
    self->start_angle =  0;
    self->arc_sweep   =  0;

 /* Generic Graphical Attributes Applicable to Arcs */
    self->color.r         =  default_color.r;
    self->color.g         =  default_color.g;
    self->color.b         =  default_color.b;
    self->color.a         =  default_color.a;

    self->locked_color.r  =  default_color.r;
    self->locked_color.g  =  default_color.g;
    self->locked_color.b  =  default_color.b;
    self->locked_color.a  =  default_color.a;

    self->line_end        =  END_NONE;
    self->line_type       =  TYPE_SOLID;
    self->line_width      =  0;
    self->line_space      =  0;
    self->line_length     =  0;
  }

  return (PyObject *)self;
}

/* ------------------------- PyGedaArcObject Initializer ------------------------- */
static int Arc_init(PyGedaArcObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_base_params;
  PyObject *py_name = NULL;
  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked",
                           "x", "y", "radius", "start_angle", "arc_sweep",
                           "line_end", "line_type", "line_width", "line_space",
                           "line_length", NULL};

  if (! PyArg_ParseTupleAndKeywords(args,
                                    kwds, "|Siiiiiiiiiiiiii:geda.Arc.__init__",
                                    kwlist,
                                    &py_name, &type, &pid, &sid, &locked,
                                    &self->x, &self->y, &self->radius,
                                    &self->start_angle, &self->arc_sweep,
                                    &self->line_end, &self->line_type,
                                    &self->line_width, &self->line_space,
                                    &self->line_length))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Arc object");
    return -1;
  }

  py_base_params = Py_BuildValue("Siiii", py_name, type, pid, sid, locked);
  if (PyGedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL) < 0)
    return -1;

  return 0;
}

static int PyGedaArcObject_print(PyGedaArcObject *arc, FILE *file, int flags)
{
  const char *name;
  int  radius, x, y, start_angle, arc_sweep;
  int  color_code;
  int  fill_type, fill_width, fill_angle1, fill_pitch1, fill_angle2, fill_pitch2;
  int  line_width, line_end, line_type, line_length, line_space;

  name  = PyString_AsString(arc->object.name);
  color_code = 1; /*arc->color*/

  /* radius, center and angles of the arc */
  radius      = arc->radius;
  x           = arc->x;
  y           = arc->y;
  start_angle = arc->start_angle;
  arc_sweep   = arc->arc_sweep;

  fill_type   = arc->fill_type;
  fill_width  = arc->fill_width;
  fill_angle1 = arc->fill_angle1;
  fill_pitch1 = arc->fill_pitch1;
  fill_angle2 = arc->fill_angle2;
  fill_pitch2 = arc->fill_pitch2;

  /* line type parameters */
  line_width  = arc->line_width;
  line_end    = arc->line_end;
  line_type   = arc->line_type;
  line_length = arc->line_length;
  line_space  = arc->line_space;

  /* Describe a arc with post-20000704 file parameters */
  fprintf(file, "<<%s> <%d %d> <radius=%d> <start-angle=%d> <end-angle=%d> <color=%d>",
                   name, x, y, radius, start_angle, arc_sweep, color_code);
  fprintf(file, " <line-type <width=%d> <end=%d> <type=%d> <dash=%d> <spaces=%d>>",
                   line_width, line_end, line_type, line_length, line_space);
  fprintf(file, " <fill-type <type=%d <width=%d> <angle1=%d> <pitch1=%d> <angle2=%d>>> <pitch2=%d>>",
                   fill_type, fill_width, fill_angle1, fill_pitch1, fill_angle2, fill_pitch2);
  return 0;
}

/* --------------------------- PyGedaArcObject Members --------------------------- */

static PyMemberDef Arc_members[] = {
  {"x",           T_INT, offsetof(PyGedaArcObject, x),           0, "Arc Center Abscissa"},
  {"y",           T_INT, offsetof(PyGedaArcObject, y),           0, "Arc Center Ordinate"},
  {"radius",      T_INT, offsetof(PyGedaArcObject, radius),      0, "Arc Radius"},
  {"start_angle", T_INT, offsetof(PyGedaArcObject, start_angle), 0, "Arc Start Angle"},
  {"arc_sweep",   T_INT, offsetof(PyGedaArcObject, arc_sweep),   0, "Arc End Angle"},
  {"end_type",    T_INT, offsetof(PyGedaArcObject, line_end),    0, "Endpoint style"},
  {"line_type",   T_INT, offsetof(PyGedaArcObject, line_type),   0, "Line type"},
  {"line_width",  T_INT, offsetof(PyGedaArcObject, line_width),  0, "Line width"},
  {"line_space",  T_INT, offsetof(PyGedaArcObject, line_space),  0, "Line space/gaps"},
  {"line_length", T_INT, offsetof(PyGedaArcObject, line_length), 0, "Line dash length"},
  {NULL}  /* Sentinel */
};

static int Arc_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
{
  char *name = PyString_AsString(key);
  char *str;
  int   index;
  int   result;
  int  *old_value;

  if (py_value == NULL) {
    PyErr_Format(PyExc_ValueError, "Cannot delete the %s attribute", name);
    return -1;
  }

  str = NULL;

  for (index = 0; Arc_members[index].name; index++){

    PyMemberDef *member = &Arc_members[index];

    str = member->name;

    if (!strcmp(str, name)) {
      old_value = (int*)((char *)obj + member->offset);
      break;
    }
    str = NULL;
  };

  if (str) {

    long long_val;
    int  new_value;

    if (!PyInt_Check(py_value)) {
      PyErr_Format(PyExc_TypeError, "The %s attribute must be an integer value", name);
      return -1;
    }

    long_val = PyInt_AsLong(py_value);

    if ((long_val == -1) && PyErr_Occurred()) {
      return -1;
    }

#if LONG_MAX != INT_MAX

    if (long_val >= INT_MIN && long_val <= INT_MAX) {
      new_value = long_val;
    }
    else {
      PyErr_SetString(PyExc_OverflowError, "Python int too large to convert to C int");
      return -1;
    }

#else

    new_value = long_val;

#endif

    /* No need to do anything if new value equals the old value */
    if (new_value != *old_value) {

      PyGedaObject *py_geda_object = (PyGedaObject*)obj;

     *old_value = new_value;

      py_geda_object->dirty = 1;

      if(py_geda_object->pid >= 0) {
        PyObject_CallMethod(geda_module, "refresh_attribs", "O", py_geda_object);
      }
    }

    result = 0;
  }
  else {
    /* no gotta, check with the base class,  */
    result = (PyGedaObjectClass())->tp_setattro(obj, key, py_value);
  }
  return result;
}

/* -------------------------- Begin Type Definition ------------------------ */


static PyTypeObject PyGedaArcObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                            /* ob_size,        not used, historical artifact for backward compatibility */
    "geda.Arc",                   /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(PyGedaArcObject),            /* tp_basicsize,   memory to allocate for this object */
    0,                            /* tp_itemsize*/
    (destructor)0,                /* tp_dealloc*/
    (printfunc)PyGedaArcObject_print,   /* tp_print*/
    0,                            /* tp_getattr*/
    0,                            /* tp_setattr*/
    0,                            /* tp_compare*/
    0,                            /* tp_repr*/
    0,                            /* tp_as_number*/
    0,                            /* tp_as_sequence*/
    0,                            /* tp_as_mapping*/
    0,                            /* tp_hash */
    0,                            /* tp_call*/
    0,                            /* tp_str*/
    0,                            /* tp_getattro*/
    (setattrofunc)Arc_set_int,    /* tp_setattro*/
    0,                            /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,          /* tp_flags*/
    PyGedaArcObject_doc,                /* tp_doc */
    0,                            /* tp_traverse */
    0,                            /* tp_clear */
    0,                            /* tp_richcompare */
    0,                            /* tp_weaklistoffset */
    0,                            /* tp_iter */
    0,                            /* tp_iternext */
    0,                            /* tp_methods */
    Arc_members,                  /* tp_members */
    0,                            /* tp_getset */
    0,                            /* tp_base, for portability, do not fill here*/
    0,                            /* tp_dict */
    0,                            /* tp_descr_get */
    0,                            /* tp_descr_set */
    0,                            /* tp_dictoffset */
    (initproc)Arc_init,           /* tp_init */
    0,                            /* tp_alloc */
    (newfunc)Arc_new,             /* tp_new */
};

PyMODINIT_FUNC initArc(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  PyGedaArcObjectType.tp_base = PyGedaObjectClass();

  if ( PyType_Ready(&PyGedaArcObjectType) < 0)
    return;

  arc_module = Py_InitModule3("Arc", NULL, "Creates an Arc object extension type.");

  if (arc_module == NULL)
    return;

  Py_INCREF(&PyGedaArcObjectType);
  PyModule_AddObject(arc_module, "Arc", (PyObject *)&PyGedaArcObjectType);
}

PyTypeObject *PyGedaArcClass(void)
{
  return &PyGedaArcObjectType;
}
