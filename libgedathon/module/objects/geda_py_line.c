/* C source                                         -*- geda_py_line.c -*- */
/*!
 * \file geda_py_line.c
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

static PyObject* line_module;
static PyObject* geda_module;

static char PyGedaLineObject_doc[] = PyDoc_STR("Geda Line: x1, y1, x2, y2 [, color]");

/* ------------------------- PyGedaLineObject Constructor ------------------------ */

static PyObject *
Line_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyGedaLineObject *self;
  struct { int r; int g; int b; int a; }
  default_color = DEFAULT_BUS_COLOR;

  self = (PyGedaLineObject*)(PyGedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    self->x[0] =  0;
    self->y[0] =  0;
    self->x[1] =  0;
    self->y[1] =  0;

 /* Generic Graphical Attributes Applicable to Linees */
    self->color.r         = default_color.r;
    self->color.g         = default_color.g;
    self->color.b         = default_color.b;
    self->color.a         = default_color.a;

    self->locked_color.r  = default_color.r;
    self->locked_color.g  = default_color.g;
    self->locked_color.b  = default_color.b;
    self->locked_color.a  = default_color.a;

 /* Line-Type Properties - only one for a line */
    self->line_end        = END_NONE;
    self->line_type       = TYPE_SOLID;
    self->line_width      = 0;
    self->line_space      = 0;
    self->line_length     = 0;
  }

  return (PyObject *)self;
}

/* ------------------------- PyGedaLineObject Initializer ------------------------ */

static int
Line_init(PyGedaLineObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_name = NULL;
  PyObject *py_base_params;
  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked",
                           "x1", "y1", "x2", "y2",
                           "end_type", "line_type", "line_width", "line_space",
                           "line_length", NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "|Siiiiiiiiiiiii:geda.Line.__init__", kwlist,
                                    &py_name, &type, &pid, &sid, &locked,
                                    &self->x[0], &self->y[0], &self->x[1], &self->y[1],
                                    &self->line_end, &self->line_type,
                                    &self->line_width, &self->line_space,
                                    &self->line_length))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Line object");
    return -1;
  }
  py_base_params = Py_BuildValue("Siiii", py_name, type, pid, sid, locked);
  if (PyGedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL) < 0)
    return -1;

  return 0;
}

static int
PyGedaLineObject_print(PyGedaLineObject *line, FILE *file, int flags)
{
  const char *name;
  int   x1, x2, y1, y2;
  int   color_code;
  int   line_width, line_end, line_type, line_length, line_space;

  name  = PyString_AsString(line->object.name);
  color_code = 1; /*line->color*/

  x1 = line->x[0];
  y1 = line->y[0];
  x2 = line->x[1];
  y2 = line->y[1];

  /* description of the line type */
  line_width = line->line_width;
  line_end   = line->line_end;
  line_type  = line->line_type;
  line_length= line->line_length;
  line_space = line->line_space;

  fprintf(file, "<<%s> <%d %d> <%d %d> <color=%d>",
                   name, x1, y1, x2, y2, color_code);
  fprintf(file, " <line-type <width=%d> <end=%d> <type=%d> <dash=%d> <spaces=%d>>",
                   line_width, line_end, line_type, line_length, line_space);
  return 0;
}

/* --------------------------- PyGedaLineObject Members -------------------------- */

static PyMemberDef Line_members[] = {
  {"x1",     T_INT, offsetof(PyGedaLineObject, x[0]),     0, "Line point 1 Abscissa"},
  {"y1",     T_INT, offsetof(PyGedaLineObject, y[0]),     0, "Line point 1 Ordinate"},
  {"x2",     T_INT, offsetof(PyGedaLineObject, x[1]),     0, "Line point 2 Abscissa"},
  {"y2",     T_INT, offsetof(PyGedaLineObject, y[1]),     0, "Line point 2 Ordinate"},

  /* Line-Type */
  {"line_type",   T_INT, offsetof(PyGedaLineObject, line_type),   0, "Line type"},
  {"line_width",  T_INT, offsetof(PyGedaLineObject, line_width),  0, "Line width"},
  {"end_type",    T_INT, offsetof(PyGedaLineObject, line_end),    0, "Endpoint style"},
  {"line_space",  T_INT, offsetof(PyGedaLineObject, line_space),  0, "Line space/gaps"},
  {"line_length", T_INT, offsetof(PyGedaLineObject, line_length), 0, "Line dash length"},
  {NULL}  /* Sentinel */
};

static int Line_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
{
  PyGedaObject  *py_geda_object = (PyGedaObject*)obj;
  PyMemberDef *member;

  char *name = PyString_AsString(key);
  char *str;
  int   index;
  int   result;
  int  *old_value;

  if (py_value == NULL) {
    PyErr_Format(PyExc_ValueError, "Cannot delete the %s attribute", name);
    return -1;
  }

  for (index = 0; Line_members[index].name; index++){

    member = &Line_members[index];

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
    if ( new_value != *old_value) {

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

/* ------------------------------ Begin Methods ---------------------------- */
static PyObject* Line_length(PyObject *self)
{
  PyGedaLineObject *line = (PyGedaLineObject*)self;
  int result;

#if HAVE_HYPOT
  result = hypot((line->x[0]-line->x[1]), (line->y[0]-line->y[1]));
#else
  result = sqrt(pow(line->x[0]-line->x[1],2)+pow(line->y[0]-line->y[1],2));
#endif

  return Py_BuildValue("i", result);
}

static PyMethodDef Line_methods[] = {
  {"length", (PyCFunction)Line_length,  METH_NOARGS,  line_length_docs},
  {NULL}  // Sentinel
};

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaLineObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                              /*ob_size,        not used, historical artifact for backward compatibility */
    "geda.Line",                    /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(PyGedaLineObject),             /* tp_basicsize,   memory to allocate for this object */
    0,                              /* tp_itemsize*/
    (destructor)0,                  /* tp_dealloc*/
    (printfunc)PyGedaLineObject_print,    /* tp_print*/
    0,                              /* tp_getattr*/
    0,                              /* tp_setattr*/
    0,                              /* tp_compare*/
    0,                              /* tp_repr*/
    0,                              /* tp_as_number*/
    0,                              /* tp_as_sequence*/
    0,                              /* tp_as_mapping*/
    0,                              /* tp_hash */
    0,                              /* tp_call*/
    0,                              /* tp_str*/
    0,                              /* tp_getattro*/
    (setattrofunc)Line_set_int,     /* tp_setattro*/
    0,                              /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,            /* tp_flags*/
    PyGedaLineObject_doc,                 /* tp_doc */
    0,                              /* tp_traverse */
    0,                              /* tp_clear */
    0,                              /* tp_richcompare */
    0,                              /* tp_weaklistoffset */
    0,                              /* tp_iter */
    0,                              /* tp_iternext */
    Line_methods,                   /* tp_methods */
    Line_members,                   /* tp_members */
    0,                              /* tp_getset */
    0,                              /* tp_base, for portability, do not fill here*/
    0,                              /* tp_dict */
    0,                              /* tp_descr_get */
    0,                              /* tp_descr_set */
    0,                              /* tp_dictoffset */
    (initproc)Line_init,             /* tp_init */
    0,                              /* tp_alloc */
    (newfunc)Line_new,               /* tp_new */
};

PyMODINIT_FUNC
initLine(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  PyGedaLineObjectType.tp_base = PyGedaObjectClass();

  if ( PyType_Ready(&PyGedaLineObjectType) < 0)
    return;

  line_module = Py_InitModule3("Line", NULL, "Creates a Line Object extension type.");

  if (line_module == NULL)
    return;

  Py_INCREF(&PyGedaLineObjectType);
  PyModule_AddObject(line_module, "Line", (PyObject *)&PyGedaLineObjectType);
}

PyTypeObject *PyGedaLineClass(void)
{
  return &PyGedaLineObjectType;
}
