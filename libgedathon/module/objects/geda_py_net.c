/* C source                                          -*- geda_py_net.c -*- */
/*!
 * \file geda_py_net.c
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

#include <geda.h>
#include <geda_python.h>
#include <geda_py_struct.h>
#include <geda_py_object.h>
#include <geda_py_color.h>
#include <geda_py_docs.h>

static PyObject* net_module;
static PyObject* geda_module;

static char NetObject_doc[] = PyDoc_STR("Geda Net: x1, y1, x2, y2 [, net_name [, color]]");

static void
NetObject_dealloc(NetObject* self)
{
  Py_XDECREF(self->net_name);
  /* Don't dealloc self, the base class will do that */
  (GedaObjectClass())->tp_dealloc((PyObject*)self);
}

static PyObject *
Net_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  NetObject *self;
  struct { int r; int g; int b; int a; }
  default_color = DEFAULT_NET_COLOR;

  self = (NetObject*)(GedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    ALLOC_EMPTY_PY_STRING(net_name)

    self->x[0] =  0;
    self->y[0] =  0;
    self->x[1] =  0;
    self->y[1] =  0;

 /* Generic Graphical Attributes Applicable to Netes */
    self->color.r         = default_color.r;
    self->color.g         = default_color.g;
    self->color.b         = default_color.b;
    self->color.a         = default_color.a;

    self->locked_color.r  = default_color.r;
    self->locked_color.g  = default_color.g;
    self->locked_color.b  = default_color.b;
    self->locked_color.a  = default_color.a;

 /* Line-Type Property - only one for a net */
    self->line_width      = 0;
  }

  return (PyObject *)self;
}
static int
Net_init(NetObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_base_params;
  PyObject *py_name         = NULL;
  PyObject *py_net_name     = NULL;
  PyObject *tmp;

  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked", "nid",
                           "x1", "y1", "x2", "y2", "line_width", "netname",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "|SiiiiiiiiiiS:geda.Net.__init__", kwlist,
                                    &py_name, &type, &pid, &sid, &locked, &self->nid,
                                    &self->x[0], &self->y[0], &self->x[1], &self->y[1],
                                    &self->line_width, &py_net_name))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Net object");
    return -1;
  }

  SWAP_PY_TMP_OBJECT(net_name)

  py_base_params = Py_BuildValue("Siiii", py_name, type, pid, sid, locked);
  if (GedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL) < 0)
    return -1;

  return 0;
}

static int
NetObject_print(NetObject *net, FILE *file, int flags)
{
  const char *name;
  const char *net_name;

  int   x1, y1, x2, y2;
  int   color_code;
  int   connected, valid_connected, line_width;

  name     = PyString_AsString(net->object.name);
  net_name = PyString_AsString(net->net_name);

  x1 = net->x[0];
  y1 = net->y[0];
  x2 = net->x[1];
  y2 = net->y[1];

  color_code = 1; /*net->color*/

  connected       = net->net_num_connected;
  valid_connected = net->valid_num_connected;
  line_width      = net->line_width;

  fprintf(file, "<<%s> <%d %d %d %d> <net_name=%s> <color=%d> <connected=%d> <valid-connected=%d> <line-width=%d>>",
                name, x1, y1, x2, y2, net_name, color_code, connected, valid_connected, line_width);

  return 0;
}

/* --------------------------- NetObject Members --------------------------- */

static PyMemberDef Net_members[] = {
  {"nid",    T_INT, offsetof(NetObject,  nid),   RO, "Geda Net Node Identifier"},
  {"x1",     T_INT, offsetof(NetObject, x[0]),    0, "Net point 1 Abscissa"},
  {"y1",     T_INT, offsetof(NetObject, y[0]),    0, "Net point 1 Ordinate"},
  {"x2",     T_INT, offsetof(NetObject, x[1]),    0, "Net point 2 Abscissa"},
  {"y2",     T_INT, offsetof(NetObject, y[1]),    0, "Net point 2 Ordinate"},

  /* Line-Type */
  {"line_width",  T_INT, offsetof(NetObject, line_width),  0, "Line width"},
  {NULL}  /* Sentinel */
};

static int Net_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
{
  GedaObject  *py_geda_object = (GedaObject*)obj;
  PyMemberDef *member;

  char *name = PyString_AsString(key);
  char *str;
  int   index;
  int   result;
  int   new_value;
  int  *old_value;
  long  long_val;

  if (py_value == NULL) {
    PyErr_Format(PyExc_ValueError, "Cannot delete the %s attribute", name);
    return -1;
  }

  old_value = &index;

  for (index = 0; Net_members[index].name; index++){
    member = &Net_members[index];
    str = member->name;
    if (!strcmp(str, name)) {
      old_value = (int*)((char *)obj + member->offset);
      break;
    }
    str = NULL;
  };

  if (index < 1) {
    PyErr_Format(PyExc_ReferenceError, "%s attribute is READ-ONLY", name);
    return 0;
  }

  if (str) {
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
    result = (GedaObjectClass())->tp_setattro(obj, key, py_value);
  }
  return result;
}

/* ------------------------ Begin Getters and Setters ---------------------- */
static PyObject *
Net_get_netname(NetObject *self, void *closure)
{
  Py_INCREF(self->net_name);
  return self->net_name;
}

static int
Net_set_netname(NetObject *self, PyObject *value, void *closure)
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

  Py_DECREF(self->net_name);
  Py_INCREF(value);
  self->net_name = value;

  self->dirty_name = 1;
  self->object.dirty = 1;
  if(self->object.pid >= 0)
    PyObject_CallMethod(geda_module, "refresh_attribs", "O", self);

  return 0;
}

static PyGetSetDef Net_getseters[] = {
  {"net_name", (getter)Net_get_netname, (setter)Net_set_netname, "net_name_docs", NULL},
  {NULL}  /* Sentinel */
};

/* ------------------------------ Begin Methods ---------------------------- */

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject NetObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                              /* ob_size,        not used, historical artifact for backward compatibility */
    "geda.Net",                     /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(NetObject),              /* tp_basicsize,   memory to allocate for this object */
    0,                              /* tp_itemsize*/
    (destructor)NetObject_dealloc,  /* tp_dealloc*/
    (printfunc)NetObject_print,     /* tp_print*/
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
    (setattrofunc)Net_set_int,      /* tp_setattro*/
    0,                              /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,            /* tp_flags*/
    NetObject_doc,                  /* tp_doc */
    0,                              /* tp_traverse */
    0,                              /* tp_clear */
    0,                              /* tp_richcompare */
    0,                              /* tp_weaklistoffset */
    0,                              /* tp_iter */
    0,                              /* tp_iternext */
    0,                              /* tp_methods */
    Net_members,                    /* tp_members */
    Net_getseters,                  /* tp_getset */
    0,                              /* tp_base, for portability, do not fill here*/
    0,                              /* tp_dict */
    0,                              /* tp_descr_get */
    0,                              /* tp_descr_set */
    0,                              /* tp_dictoffset */
    (initproc)Net_init,             /* tp_init */
    0,                              /* tp_alloc */
    (newfunc)Net_new,               /* tp_new */
};

PyMODINIT_FUNC
initNet(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  NetObjectType.tp_base = GedaObjectClass();

  if ( PyType_Ready(&NetObjectType) < 0)
    return;

  net_module = Py_InitModule3("Net", NULL, "Creates a Net object type.");

  if (net_module == NULL)
    return;

  Py_INCREF(&NetObjectType);
  PyModule_AddObject(net_module, "Net", (PyObject *)&NetObjectType);
}
PyTypeObject *NetObjectClass(void)
{
  return &NetObjectType;
}
