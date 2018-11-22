/* C source                                          -*- geda_py_bus.c -*- */
/*!
 * \file geda_py_bus.c
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

static PyObject *bus_module;
static PyObject *geda_module;

static char PyGedaBusObject_doc[] = PyDoc_STR("Geda Bus: x1, y1, x2, y2 [, color]");

/* ------------------------- PyGedaBusObject Destructor -------------------------- */

static void PyGedaBusObject_dealloc(PyGedaBusObject *self)
{
  Py_XDECREF(self->bus_name);
  /* Don't dealloc self, the base class will do that */
  (PyGedaObjectClass())->tp_dealloc((PyObject*)self);
}

/* ------------------------- PyGedaBusObject Constructor ------------------------- */

static PyObject *Bus_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyGedaBusObject *self;
  struct { int r; int g; int b; int a; }
  default_color = DEFAULT_BUS_COLOR;

  self = (PyGedaBusObject*)(PyGedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    ALLOC_EMPTY_PY_STRING(bus_name)

    self->x[0] =  0;
    self->y[0] =  0;
    self->x[1] =  0;
    self->y[1] =  0;

 /* Generic Graphical Attributes Applicable to Buses */
    self->color.r         = default_color.r;
    self->color.g         = default_color.g;
    self->color.b         = default_color.b;
    self->color.a         = default_color.a;

    self->locked_color.r  = default_color.r;
    self->locked_color.g  = default_color.g;
    self->locked_color.b  = default_color.b;
    self->locked_color.a  = default_color.a;

 /* Line-Type Property - only one for a bus */
    self->line_width      = 0;
  }

  return (PyObject*)self;
}

/* ------------------------- PyGedaBusObject Initializer ------------------------- */

static int Bus_init(PyGedaBusObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_base_params;
  PyObject *py_name      = NULL;
  PyObject *py_bus_name  = NULL;
  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked",
                           "x1", "y1", "x2", "y2", "busname",
                            "direction", "line_width", NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "|SiiiiiiiiSii:geda.Bus.__init__", kwlist,
                                    &py_name, &type, &pid, &sid, &locked,
                                    &self->x[0], &self->y[0], &self->x[1], &self->y[1],
                                    &py_bus_name, &self->direction, &self->line_width))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Bus object");
    return -1;
  }

  SWAP_PY_TMP_OBJECT(bus_name)

  py_base_params = Py_BuildValue("Siiii", py_name, type, pid, sid, locked);
  if (PyGedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL) < 0)
    return -1;

  return 0;
}

static int PyGedaBusObject_print(PyGedaBusObject *bus, FILE *file, int flags)
{
  const char *name;
  const char *bus_name;

  int   x1, y1, x2, y2;
  int   color_code;
  int   direction;

  name  = PyString_AsString(bus->object.name);
  bus_name = PyString_AsString(bus->bus_name);


  x1 = bus->x[0];
  y1 = bus->y[0];
  x2 = bus->x[1];
  y2 = bus->y[1];

  color_code = 1; /*bus->color*/

  /* description of the pin */
  direction   = bus->direction;

  fprintf(file, "<<%s> <%d %d %d %d> <bus_name=%s> <color=%d> <direction=%d>>",
                 name, x1, y1, x2, y2, bus_name, color_code, direction);

  return 0;
}

/* --------------------------- PyGedaBusObject Members --------------------------- */

static PyMemberDef Bus_members[] = {
  {"x1",     T_INT, offsetof(PyGedaBusObject, x[0]),     0, "Bus point 1 Abscissa"},
  {"y1",     T_INT, offsetof(PyGedaBusObject, y[0]),     0, "Bus point 1 Ordinate"},
  {"x2",     T_INT, offsetof(PyGedaBusObject, x[1]),     0, "Bus point 2 Abscissa"},
  {"y2",     T_INT, offsetof(PyGedaBusObject, y[1]),     0, "Bus point 2 Ordinate"},

  {"direction",   T_INT, offsetof(PyGedaBusObject, direction),   0, "Bus ripper direction"},

  /* Line-Type */
  {"line_width",  T_INT, offsetof(PyGedaBusObject, line_width),  0, "Line width"},
  {NULL}  /* Sentinel */
};

static int Bus_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
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

  for (index = 0; Bus_members[index].name; index++){
    member = &Bus_members[index];
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

/* ------------------------ Begin Getters and Setters ---------------------- */

static PyObject *
Bus_get_busname(PyGedaBusObject *self, void *closure)
{
  Py_INCREF(self->bus_name);
  return self->bus_name;
}

static int Bus_set_busname(PyGedaBusObject *self, PyObject *value, void *closure)
{
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the bus name attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The bus name attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->bus_name);
  Py_INCREF(value);
  self->bus_name = value;

  self->dirty_name = 1;
  self->object.dirty = 1;
  if(self->object.pid >= 0)
    PyObject_CallMethod(geda_module, "refresh_attribs", "O", self);

  return 0;
}

static PyGetSetDef Bus_getseters[] = {
  {"bus_name", (getter)Bus_get_busname, (setter)Bus_set_busname, "bus_name_docs", NULL},
  {"net_name", (getter)Bus_get_busname, (setter)Bus_set_busname, "bus_name_docs", NULL},
  {NULL}  /* Sentinel */
};

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaBusObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                              /* ob_size,        not used, historical artifact for backward compatibility */
    "geda.Bus",                     /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(PyGedaBusObject),              /* tp_basicsize,   memory to allocate for this object */
    0,                              /* tp_itemsize */
    (destructor)PyGedaBusObject_dealloc,  /* tp_dealloc */
    (printfunc)PyGedaBusObject_print,     /* tp_print */
    0,                              /* tp_getattr */
    0,                              /* tp_setattr */
    0,                              /* tp_compare */
    0,                              /* tp_repr */
    0,                              /* tp_as_number */
    0,                              /* tp_as_sequence */
    0,                              /* tp_as_mapping */
    0,                              /* tp_hash */
    0,                              /* tp_call */
    0,                              /* tp_str */
    0,                              /* tp_getattro */
    (setattrofunc)Bus_set_int,      /* tp_setattro */
    0,                              /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,            /* tp_flags */
    PyGedaBusObject_doc,                  /* tp_doc */
    0,                              /* tp_traverse */
    0,                              /* tp_clear */
    0,                              /* tp_richcompare */
    0,                              /* tp_weaklistoffset */
    0,                              /* tp_iter */
    0,                              /* tp_iternext */
    0,                              /* tp_methods */
    Bus_members,                    /* tp_members */
    Bus_getseters,                  /* tp_getset */
    0,                              /* tp_base, for portability, do not fill here */
    0,                              /* tp_dict */
    0,                              /* tp_descr_get */
    0,                              /* tp_descr_set */
    0,                              /* tp_dictoffset */
    (initproc)Bus_init,             /* tp_init */
    0,                              /* tp_alloc */
    (newfunc)Bus_new,               /* tp_new */
};

PyMODINIT_FUNC initBus(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  PyGedaBusObjectType.tp_base = PyGedaObjectClass();

  if ( PyType_Ready(&PyGedaBusObjectType) < 0)
    return;

  bus_module = Py_InitModule3("Bus", NULL, "Creates a Bus object type.");

  if (bus_module == NULL)
    return;

  Py_INCREF(&PyGedaBusObjectType);
  PyModule_AddObject(bus_module, "Bus", (PyObject *)&PyGedaBusObjectType);
}

PyTypeObject *PyGedaBusClass(void)
{
  return &PyGedaBusObjectType;
}
