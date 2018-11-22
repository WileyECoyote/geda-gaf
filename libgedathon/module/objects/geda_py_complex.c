/* C source                                      -*- geda_py_complex.c -*- */
/*!
 * \file geda_py_complex.c
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

static PyObject *complex_module;
static PyObject *geda_module;

static char PyGedaComplexObject_doc[] = PyDoc_STR("Geda Complex: x, y [, angle [, color]");

/* -------------------- PyGedaComplexObject Destructor --------------------- */

static void PyGedaComplexObject_dealloc(PyGedaComplexObject* self)
{
  Py_XDECREF(self->filename);
  Py_XDECREF(self->pin_objs);
  Py_XDECREF(self->prim_objs);
  /* Don't dealloc self, the base class will do that */
  (PyGedaObjectClass())->tp_dealloc((PyObject*)self);
}

/* -------------------- PyGedaComplexObject Constructor -------------------- */

static PyObject *Complex_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyGedaComplexObject *self;

  self = (PyGedaComplexObject*)(PyGedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    ALLOC_EMPTY_PY_STRING(filename)

    self->embedded = 0;      /* is embedded component? */
    self->x        = 0;
    self->y        = 0;
    self->angle    = 0;      /* orientation in degrees */
    self->mirror   = 0;

    self->pin_objs   = NULL; /* Pin Primitive objects */
    self->prim_objs  = NULL; /* Primitive objects */

  }

  return (PyObject*)self;
}

/* -------------------- PyGedaComplexObject Initializer -------------------- */

static int
Complex_init(PyGedaComplexObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_base_params;
  PyObject *py_name       = NULL;
  PyObject *py_filename   = NULL;
  PyObject *py_attributes = NULL;
  PyObject *py_pin_objs   = NULL;
  PyObject *py_prim_objs  = NULL;

  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked",
                           "filename", "x", "y", "embedded", "angle", "mirror",
                           "pin_objs", "prim_objs", "attributes", NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "|SiiiiSiiiiiOOO:geda.Conplex.__init__",
                                   kwlist, &py_name, &type, &pid, &sid, &locked,
                                   &py_filename, &self->x, &self->y, &self->embedded,
                                   &self->angle, &self->mirror,
                                   &py_pin_objs,
                                   &py_prim_objs,
                                   &py_attributes))
  {
    PyErr_SetString(PyExc_TypeError,  "Error initializing new Complex object");
    return -1;
  }

  SWAP_PY_TMP_OBJECT(filename)
  SWAP_PY_TMP_OBJECT(pin_objs)
  SWAP_PY_TMP_OBJECT(prim_objs)

  py_base_params = Py_BuildValue("SiiiiO", py_name, type, pid, sid, locked, py_attributes);
  return PyGedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL);
}

static int
PyGedaComplexObject_print(PyGedaComplexObject *complex, FILE *file, int flags)
{
  const char *name;
  const char *filename;

  int  x        = complex->x;
  int  y        = complex->y;
  int  embedded = complex->embedded;
  int  angle    = complex->angle;
  int  mirror   = complex->mirror;

  name  = PyString_AsString(complex->object.name);
  filename = PyString_AsString(complex->filename);

  fprintf(file, "<<%s> <%d,%d> <filename=%s> <embedded=%d> <angle=%d> <mirror=%d>>",
                 name, x, y, filename, embedded, angle, mirror);
  return 0;
}

/* ---------------------- PyGedaComplexObject Members ---------------------- */

static PyMemberDef Complex_members[] = {
  {"x",        T_INT,  offsetof(PyGedaComplexObject, x),          0, "Abscissa of Complex object"},
  {"y",        T_INT,  offsetof(PyGedaComplexObject, y),          0, "Ordinate of Complex object"},
  {"angle",    T_INT,  offsetof(PyGedaComplexObject, angle),      0, "Angle of Complex object"},
  {"embedded", T_BOOL, offsetof(PyGedaComplexObject, embedded),   0, "Embedded Complex object"},
  {"mirror",   T_BOOL, offsetof(PyGedaComplexObject, mirror),     0, "Complex object is mirrored"},
  {NULL}  /* Sentinel */
};

static int Complex_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
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

  for (index = 0; Complex_members[index].name; index++) {
    member = &Complex_members[index];
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

    if (strcmp(name, "mirror") != 0) {
      if (!PyInt_Check(py_value)) {
        PyErr_Format(PyExc_TypeError, "The %s attribute must be an integer value", name);
        return -1;
      }
    }
    else {
      if (!PyBool_Check(py_value)) {
        PyErr_Format(PyExc_TypeError, "The %s attribute must be a boolean value", name);
        return -1;
      }
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
      *old_value = new_value;
      py_geda_object->dirty = 1;
      if (py_geda_object->pid >= 0) {
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

static PyObject *Complex_get_filename(PyGedaComplexObject *self, void *closure)
{
  Py_INCREF(self->filename);
  return self->filename;
}

static int
Complex_set_filename(PyGedaComplexObject *self, PyObject *value, void *closure)
{
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the filename attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The filename attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->filename);
  Py_INCREF(value);
  self->filename = value;

  self->dirty_name = 1;
  self->object.dirty = 1;

  if (self->object.pid >= 0)
    PyObject_CallMethod(geda_module, "refresh_attribs", "O", self);

  return 0;
}

static PyGetSetDef Complex_getseters[] = {
  {"filename", (getter)Complex_get_filename, (setter)Complex_set_filename, "object_name_docs", NULL},
  {NULL}  /* Sentinel */
};

/* ------------------------------ Begin Methods ---------------------------- */
static PyObject *
PyGedaComplexObject_reload(PyGedaComplexObject* self)
{
  return 0;
}

static PyMethodDef Complex_methods[] = {
  {"reload", (PyCFunction)PyGedaComplexObject_reload, METH_NOARGS,  "object_reload_docs"},
  {NULL, NULL, 0, NULL}
};

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaComplexObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                                  /* ob_size,        not used, historical artifact for backward compatibility */
    "geda.Complex",                     /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(PyGedaComplexObject),        /* tp_basicsize,   memory to allocate for this object */
    0,                                  /* tp_itemsize*/
    (destructor)PyGedaComplexObject_dealloc,  /* tp_dealloc*/
    (printfunc)PyGedaComplexObject_print,     /* tp_print*/
    0,                                  /* tp_getattr*/
    0,                                  /* tp_setattr*/
    0,                                  /* tp_compare*/
    0,                                  /* tp_repr*/
    0,                                  /* tp_as_number*/
    0,                                  /* tp_as_sequence*/
    0,                                  /* tp_as_mapping*/
    0,                                  /* tp_hash */
    0,                                  /* tp_call*/
    0,                                  /* tp_str*/
    0,                                  /* tp_getattro*/
    (setattrofunc)Complex_set_int,      /* tp_setattro*/
    0,                                  /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,                /* tp_flags*/
    PyGedaComplexObject_doc,            /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    Complex_methods,                    /* tp_methods */
    Complex_members,                    /* tp_members */
    Complex_getseters,                  /* tp_getset */
    0,                                  /* tp_base, for portability, do not fill here*/
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)Complex_init,             /* tp_init */
    0,                                  /* tp_alloc */
    (newfunc)Complex_new,               /* tp_new */
};

PyMODINIT_FUNC initComplex(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  PyGedaComplexObjectType.tp_base = PyGedaObjectClass();

  if ( PyType_Ready(&PyGedaComplexObjectType) < 0)
    return;

  complex_module = Py_InitModule3("Complex", NULL, "Creates a Complex object type.");

  if (complex_module == NULL)
    return;

  Py_INCREF(&PyGedaComplexObjectType);
  PyModule_AddObject(complex_module, "Complex", (PyObject *)&PyGedaComplexObjectType);
}

PyTypeObject *PyGedaComplexClass(void)
{
  return &PyGedaComplexObjectType;
}
