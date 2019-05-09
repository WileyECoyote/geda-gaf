/* C source                                          -*- geda_py_pin.c -*- */
/*!
 * \file geda_py_pin.c
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
#include "../../include/gettext.h"

static PyObject *pin_module;
static PyObject *geda_module;

static char PyGedaPinObject_doc[] = PyDoc_STR("Geda Pin: x1, y1, x2, y2 [, whichend [, number [, label [, etype [, mtype [, ntype ]]]]]]");

/* ----------------------- PyGedaPinObject Destructor ---------------------- */

static void PyGedaPinObject_dealloc(PyGedaPinObject *self)
{
  Py_XDECREF(self->electrical);
  Py_XDECREF(self->mechanical);
  Py_XDECREF(self->label);
  /* Don't dealloc self, the base class will do that */
  (PyGedaObjectClass())->tp_dealloc((PyObject*)self);
}

/* ---------------------- PyGedaPinObject Constructor ---------------------- */

static PyObject *Pin_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyGedaPinObject *self;
  struct { int r; int g; int b; int a; }
  default_color = DEFAULT_PIN_COLOR;

  self = (PyGedaPinObject*)(PyGedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    self->dirty_electrical = 0;
    self->dirty_label      = 0;
    self->dirty_mechanical = 0;
    self->dirty_number     = 0;

    ALLOC_EMPTY_PY_STRING(electrical)
    ALLOC_EMPTY_PY_STRING(label)
    ALLOC_EMPTY_PY_STRING(mechanical)
    ALLOC_EMPTY_PY_STRING(number)

    self->cid      = -1;
    self->nid      = -1;
    self->sequence = -1;

    self->whichend = 1; /* get connected graphically */

    self->x[0]     = 0;
    self->y[0]     = 0;
    self->x[1]     = 0;
    self->y[1]     = 0;

 /* Generic Graphical Attributes Applicable to Pins */
    self->color.r         = default_color.r;
    self->color.g         = default_color.g;
    self->color.b         = default_color.b;
    self->color.a         = default_color.a;

    self->locked_color.r  = default_color.r;
    self->locked_color.g  = default_color.g;
    self->locked_color.b  = default_color.b;
    self->locked_color.a  = default_color.a;

 /* Line-Type Property - only one for a pin */
    self->line_width      = 0;
  }

  return (PyObject*)self;
}

/* ---------------------- PyGedaPinObject Initializer ---------------------- */

static int Pin_init(PyGedaPinObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_base_params;
  PyObject *py_name       = NULL;
  PyObject *py_electrical = NULL;
  PyObject *py_mechanical = NULL;
  PyObject *py_label      = NULL;
  PyObject *py_number     = NULL;

  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked", "cid", "nid",
                           "electrical", "mechanical", "label", "number",
                           "sequence", "whichend",
                           "x1", "y1", "x2", "y2",
                           "elect_type", "mech_type", "node_type", "line_width",
                            NULL};

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "|SiiiiiiSSSSiiiiiiiiii:geda.Pin.__init__",
                                   kwlist, &py_name, &type, &pid, &sid, &locked,
                                   &self->cid, &self->nid,
                                   &py_electrical, &py_mechanical, &py_label, &py_number,
                                   &self->sequence, &self->whichend,
                                   &self->x[0], &self->y[0], &self->x[1], &self->y[1],
                                   &self->elect_type, &self->mech_type, &self->node_type,
                                   &self->line_width))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Pin object");
    return -1;
  }

  SWAP_PY_TMP_OBJECT(electrical)
  SWAP_PY_TMP_OBJECT(mechanical)
  SWAP_PY_TMP_OBJECT(label)
  SWAP_PY_TMP_OBJECT(number)

  py_base_params = Py_BuildValue("Siiii", py_name, type, pid, sid, locked);

  if (PyGedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL) < 0)
    return -1;

  /*! @note: Strings should NOT be dirty at this point! */
  self->object.dirty = 1;

  return 0;
}

static int PyGedaPinObject_print(PyGedaPinObject *pin, FILE *file, int flags)
{
  const char *elect;
  const char *label;
  const char *name;
  const char *number;

  int   x1, y1, x2, y2;
  int   color_code;
  int   node_type, whichend;
  int   ret_val = 1;

  elect  = PyString_AsString(pin->electrical);
  label  = PyString_AsString(pin->label);
  name   = PyString_AsString(pin->object.name);
  number = PyString_AsString(pin->number);

  x1 = pin->x[0];
  y1 = pin->y[0];
  x2 = pin->x[1];
  y2 = pin->y[1];

  color_code = 1; /*pin->color*/

  /* description of the pin */
  node_type  = pin->node_type;
  whichend   = pin->whichend;

  if (fprintf(file, "<<%s> <%d %d %d %d> <label=%s> <number=%s> <pin-type=%s> <color=%d> <node-type=%d> <connect=%d>>",
              name, x1, y1, x2, y2, label, number, elect, color_code, node_type, whichend))
    ret_val = 0;

  return ret_val;
}

/* ------------------------ PyGedaPinObject Members ------------------------ */

static PyMemberDef Pin_members[] = {
  {"cid",        T_INT, offsetof(PyGedaPinObject, cid),       RO, "Pin Complex Identifier"},
  {"nid",        T_INT, offsetof(PyGedaPinObject, nid),       RO, "Pin Net identifier"},
  {"sequence",   T_INT, offsetof(PyGedaPinObject, sequence),   0, "Pin Sequence index"},
  {"whichend",   T_INT, offsetof(PyGedaPinObject, whichend),   0, "Which point gets connect?"},

  {"x1",         T_INT, offsetof(PyGedaPinObject, x[0]),       0, "Pin point 1 Abscissa"},
  {"y1",         T_INT, offsetof(PyGedaPinObject, y[0]),       0, "Pin point 1 Ordinate"},
  {"x2",         T_INT, offsetof(PyGedaPinObject, x[1]),       0, "Pin point 2 Abscissa"},
  {"y2",         T_INT, offsetof(PyGedaPinObject, y[1]),       0, "Pin point 2 Ordinate"},

  /* Pin Type Codes */
  {"elect_type", T_INT, offsetof(PyGedaPinObject, elect_type), 0, "Pin Electrical type code"},
  {"mech_type",  T_INT, offsetof(PyGedaPinObject, mech_type),  0, "Pin Mechanical type code"},
  {"node_type",  T_INT, offsetof(PyGedaPinObject, node_type),  0, "Pin node type code"},

  /* Line-Type */
  {"line_width", T_INT, offsetof(PyGedaPinObject, line_width), 0, "Line width"},
  {NULL}  /* Sentinel */
};

static int Pin_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
{
  PyGedaObject  *py_geda_object = (PyGedaObject*)obj;
  PyMemberDef *member;

  char *name = PyString_AsString(key);
  char *str;
  int   index;
  int   result;
  int  *old_value;

  if (py_value == NULL) {
    PyErr_Format(PyExc_ValueError, _("Cannot delete the %s attribute"), name);
    return -1;
  }

  old_value = &index;

  for (index = 0; Pin_members[index].name; index++){
    member = &Pin_members[index];
    str = member->name;
    if (!strcmp(str, name)) {
      old_value = (int*)((char *)obj + member->offset);
      break;
    }
    str = NULL;
  };

  if (index < 2) {
    PyErr_Format(PyExc_ReferenceError, "%s attribute is READ-ONLY", name);
    return 0;
  }

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

static PyObject *Pin_get_elect(PyGedaPinObject *self, void *closure)
{
  Py_INCREF(self->electrical);
  return self->electrical;
}

static int Pin_set_elect(PyGedaPinObject *self, PyObject *value, void *closure)
{
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the electrical attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The electrical attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->electrical);
  Py_INCREF(value);
  self->electrical = value;
  self->dirty_electrical = 1;
  self->object.dirty = 1;

  return 0;
}

static PyObject *Pin_get_mech(PyGedaPinObject *self, void *closure)
{
  Py_INCREF(self->mechanical);
  return self->mechanical;
}

static int Pin_set_mech(PyGedaPinObject *self, PyObject *value, void *closure)
{
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the mechanical attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The mechanical attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->mechanical);
  Py_INCREF(value);
  self->mechanical = value;
  self->dirty_mechanical = 1;
  self->object.dirty = 1;

  return 0;
}

static PyObject *Pin_get_label(PyGedaPinObject *self, void *closure)
{
  Py_INCREF(self->label);
  return self->label;
}

static int Pin_set_label(PyGedaPinObject *self, PyObject *value, void *closure)
{
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the label attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The label attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->label);
  Py_INCREF(value);
  self->label = value;

  self->dirty_label = 1;
  self->object.dirty = 1;

  if (self->object.pid >= 0)
    PyObject_CallMethod(geda_module, "refresh_attribs", "O", self);

  return 0;
}

static PyObject *Pin_get_number(PyGedaPinObject *self, void *closure)
{
  Py_INCREF(self->number);
  return self->number;
}

static int Pin_set_number(PyGedaPinObject *self, PyObject *value, void *closure)
{
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the pin number attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The pin number attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->number);
  Py_INCREF(value);
  self->number = value;

  self->dirty_number = 1;
  self->object.dirty = 1;

  if (self->object.pid >= 0)
    PyObject_CallMethod(geda_module, "refresh_attribs", "O", self);

  return 0;
}

static PyGetSetDef Pin_getseters[] = {
  {"electrical", (getter)Pin_get_elect,  (setter)Pin_set_elect,  "pin_electrical_docs", NULL},
  {"mechanical", (getter)Pin_get_mech,   (setter)Pin_set_mech,   "pin_mechanical_docs", NULL},
  {"label",      (getter)Pin_get_label,  (setter)Pin_set_label,  "pin_label_docs",      NULL},
  {"number",     (getter)Pin_get_number, (setter)Pin_set_number, "pin_number_docs",     NULL},
  {NULL}  /* Sentinel */
};

/* ------------------------------ Begin Methods ---------------------------- */

static PyObject *PyGedaPinObject_change(PyGedaPinObject* self)
{
  Py_INCREF(self->electrical);
  return self->electrical;
}

static PyObject *PyGedaPinObject_connect(PyGedaPinObject* self)
{
  Py_INCREF(self->mechanical);
  return self->mechanical;
}

static PyObject *PyGedaPinObject_disconnect(PyGedaPinObject* self)
{
  Py_INCREF(self->label);
  return self->label;
}

static PyMethodDef Pin_methods[] = {
  {"change",     (PyCFunction)PyGedaPinObject_change,     METH_NOARGS, "pin_change_docs"},
  {"connect",    (PyCFunction)PyGedaPinObject_connect,    METH_NOARGS, "pin_connect_docs"},
  {"disconnect", (PyCFunction)PyGedaPinObject_disconnect, METH_NOARGS, "pin_disconnect_docs"},
  {NULL, NULL, 0, NULL}
};

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaPinObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                              /* ob_size,        not used, historical artifact for backward compatibility */
    "geda.Pin",                     /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(PyGedaPinObject),        /* tp_basicsize,   memory to allocate for this object */
    0,                              /* tp_itemsize*/
    (destructor)PyGedaPinObject_dealloc,  /* tp_dealloc*/
    (printfunc)PyGedaPinObject_print,     /* tp_print*/
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
    (setattrofunc)Pin_set_int,      /* tp_setattro*/
    0,                              /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,            /* tp_flags*/
    PyGedaPinObject_doc,            /* tp_doc */
    0,                              /* tp_traverse */
    0,                              /* tp_clear */
    0,                              /* tp_richcompare */
    0,                              /* tp_weaklistoffset */
    0,                              /* tp_iter */
    0,                              /* tp_iternext */
    Pin_methods,                    /* tp_methods */
    Pin_members,                    /* tp_members */
    Pin_getseters,                  /* tp_getset */
    0,                              /* tp_base, for portability, do not fill here*/
    0,                              /* tp_dict */
    0,                              /* tp_descr_get */
    0,                              /* tp_descr_set */
    0,                              /* tp_dictoffset */
    (initproc)Pin_init,             /* tp_init */
    0,                              /* tp_alloc */
    (newfunc)Pin_new,               /* tp_new */
};

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PyMODINIT_FUNC initPin(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  PyGedaPinObjectType.tp_base = PyGedaObjectClass();

  if (PyType_Ready(&PyGedaPinObjectType) < 0)
    return;

  pin_module = Py_InitModule3("Pin", NULL, "Creates a Pin object type.");

  if (pin_module == NULL)
    return;

  Py_INCREF(&PyGedaPinObjectType);
  PyModule_AddObject(pin_module, "Pin", (PyObject *)&PyGedaPinObjectType);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PyTypeObject *PyGedaPinClass(void)
{
  return &PyGedaPinObjectType;
}
