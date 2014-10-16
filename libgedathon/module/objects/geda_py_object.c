/* C source                                       -*- geda_py_object.c -*- */
/*!
 * \file geda_py_object.c
 *
 * gEDA - GPL Electronic Design Automation
 * libgedathon - gEDA's Python API Extension library
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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
#include "structmember.h"

#include "geda.h"
#include "geda_python.h"
#include "geda_py_struct.h"
#include "geda_py_object.h"
#include "geda_py_docs.h"

static PyObject* object_module;
static PyObject* geda_module;

/* ------------------------- GedaObject Destructor ------------------------- */

static void
GedaObject_dealloc(GedaObject* self)
{
#if DEBUG
  fprintf(stderr, "GedaObject_dealloc: %s, id=%d\n", PyString_AsString(self->name), self->sid);
#endif
  Py_XDECREF(self->name);
  Py_XDECREF(self->attributes);
  self->ob_type->tp_free((PyObject*)self);
}

/* ------------------------- GedaObject Constructor ------------------------ */

static PyObject *
GedaObject_new(PyTypeObject *type, PyObject *args)
{
  GedaObject *self;

  self = (GedaObject *)type->tp_alloc(type, 0);

  if (self != NULL) {

    self->dirty            = 0;
    self->auto_attributes  = 1;

    ALLOC_EMPTY_PY_STRING(name)

    self->type        =  0;
    self->pid         = -1;
    self->sid         = -1;
    self->locked      =  0;
    self->selected    =  0;

    self->attributes = NULL; /* Text Attributes of the components */
  }

  return (PyObject *)self;
}

/* ------------------------- GedaObject Initializer ------------------------ */

static int
GedaObject_init(GedaObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_name = NULL;
  PyObject *py_attributes = NULL;

  PyObject *tmp;

  if (! PyArg_ParseTuple(args, "|SiiiiO!:geda.Object.__init__", &py_name, &self->type,
                         &self->pid, &self->sid, &self->locked, &PyList_Type, &py_attributes))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Geda object");
    return -1;
  }

  SWAP_PY_TMP_OBJECT(name)
  SWAP_PY_TMP_OBJECT(attributes)
  return 0;
}

PyObject *GedaObject_str(GedaObject *self)
{
  return self->name;
}

/* --------------------------- GedaObject Members -------------------------- */

static PyMemberDef GedaObject_members[] = {
  {"pid",      T_INT,  offsetof(GedaObject, pid),       RO, "Page Container Identifier"},
  {"sid",      T_INT,  offsetof(GedaObject, sid),       RO, "Geda Sequence Identifier"},
  {"type",     T_INT,  offsetof(GedaObject, type),      RO, "Geda Type Identifier"},

  {"selected",         T_BOOL, offsetof(GedaObject, selected),        0, "object selected attribute"},
  {"locked",           T_BOOL, offsetof(GedaObject, locked),          0, "object selectable attribute"},
  {"auto_attributes",  T_BOOL, offsetof(GedaObject, auto_attributes), 0, "Enable or disable auto attribute"},

  {NULL}  /* Sentinel */
};

static int Object_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
{
  GedaObject  *geda_object = (GedaObject*)obj;
  PyMemberDef *member;

  char *name  = PyString_AsString(key);
  int   result;
  int   index;

  /*Note: The setattrofunc in derived objects only set their integer types.
   *      The following return to PyObject_GenericSetAttr not only will set
   *      integers within this base class, but can also call the tp_getset's
   *      in the derived type to set string values. This is not obvious */
  result = PyObject_GenericSetAttr(obj, key, py_value);

  /* Check if key is member of this object, we don't set but we need to
   * know if one of our members is being set so we can set our flag */
  for (index = 0; GedaObject_members[index].name; index++){
    member = &GedaObject_members[index];
    if (!strcmp(member->name, name)) {
      geda_object->dirty = 1;
      if(geda_object->pid >= 0) {
        PyObject_CallMethod(geda_module, "sync_object", "O", geda_object);
      }
      break;
    }
  };

  return result;
}
/* ------------------------------ Begin Methods ---------------------------- */

static PyObject *
GedaObject_name(GedaObject* self)
{
  static PyObject *format = NULL;
  PyObject *args, *result;

  if (format == NULL) {
    format = PyString_FromString("%s");
                                 if (format == NULL)
                                   return NULL;
  }

  args = Py_BuildValue("O", self->name);
  if (args == NULL)
    return NULL;

  result = PyString_Format(format, args);
  Py_DECREF(args);

  return result;
}
static PyObject* go_add(PyObject *self, PyObject *args)
{
  PyObject *unknown;
  PyObject *bute;

  if(!PyArg_ParseTuple(args, "O!O!:geda.add_object",
                       GedaObjectClass(), &unknown,
                       TextObjectClass(), &bute))
  {
    return NULL;
  }

  return PyObject_CallMethod(geda_module, "add_object", "OO", self, args);
}
static PyObject* go_copy(PyObject *self, PyObject *args)
{
  int       dx = -1;
  int       dy = -1;

  PyArg_ParseTuple(args, "|ii:geda.add_objects, Object PyList", &dx, &dy);
  return PyObject_CallMethod(geda_module, "copy_object", "Oii", self, dx, dy);
}
static PyObject* go_delete(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "delete_object", "O", self);
}
static PyObject* go_rotate(PyObject *self, PyObject *args)
{
  return PyObject_CallMethod(geda_module, "rotate_object", "O", self, args);
}
static PyObject* go_select(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "select_object", "O", self);
}
static PyObject* go_unselect(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "unselect_object", "O", self);
}
/*add lock, unlock*/
static PyMethodDef GedaObject_methods[] = {
  {"name",     (PyCFunction)GedaObject_name, METH_NOARGS,  object_name_docs},
  {"add",      (PyCFunction)go_add,          METH_VARARGS, add_attrib_docs},
  {"copy",     (PyCFunction)go_copy,         METH_VARARGS, copy_object_docs},
  {"delete",   (PyCFunction)go_delete,       METH_NOARGS,  delete_object_docs},
  {"rotate",   (PyCFunction)go_rotate,       METH_VARARGS, rotate_object_docs},
  {"select",   (PyCFunction)go_select,       METH_NOARGS,  select_object_docs},
  {"unselect", (PyCFunction)go_unselect,     METH_NOARGS,  unselect_object_docs},
  {NULL, NULL, 0, NULL}
};

/* -------------------------- PageObject GetSeters ------------------------- */

static PyObject *
Geda_getbounds(GedaObject *self, void *closure)
{
  return PyObject_CallMethod(geda_module, "get_bounds", "O", self);
}

static PyGetSetDef GedaObject_getseters[] = {
  {"bounds", (getter)Geda_getbounds, (setter)NULL, "Bounds", NULL},
  {NULL}  /* Sentinel */
};
/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject GedaObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                              /* ob_size, not used, historical artifact for backward compatibility */
    "geda.GedaObject",              /* tp_name, default textual representation an object, used in some error messages*/
    sizeof(GedaObject),             /* tp_basicsize, memory to allocate for this object */
    0,                              /* tp_itemsize */
    (destructor)GedaObject_dealloc, /* tp_dealloc */
    0,                              /* tp_print */
    0,                              /* tp_getattr */
    0,                              /* tp_setattr */
    0,                              /* tp_compare */
    0,                              /* tp_repr */
    0,                              /* tp_as_number */
    0,                              /* tp_as_sequence */
    0,                              /* tp_as_mapping */
    0,                              /* tp_hash */
    0,                              /* tp_call */
    (reprfunc)GedaObject_str,       /* tp_str */
    0,                              /* tp_getattro */
    (setattrofunc)Object_set_int,   /* tp_setattro */
    0,                              /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,             /* tp_flags */
    "Geda objects",                 /* tp_doc */
    0,                              /* tp_traverse */
    0,                              /* tp_clear */
    0,                              /* tp_richcompare */
    0,                              /* tp_weaklistoffset */
    0,                              /* tp_iter */
    0,                              /* tp_iternext */
    GedaObject_methods,             /* tp_methods */
    GedaObject_members,             /* tp_members */
    GedaObject_getseters,           /* tp_getset */
    0,                              /* tp_base */
    0,                              /* tp_dict */
    0,                              /* tp_descr_get */
    0,                              /* tp_descr_set */
    0,                              /* tp_dictoffset */
    (initproc)GedaObject_init,      /* tp_init */
    0,                              /* tp_alloc */
    (newfunc)GedaObject_new,        /* tp_new */
};

#ifndef PyMODINIT_FUNC  /* declarations for DLL import/export */
#define PyMODINIT_FUNC void
#endif
PyMODINIT_FUNC
initGedaObject(PyObject *module)
{
  geda_module = module;
  if ( PyType_Ready(&GedaObjectType) < 0)
    return;

  object_module = Py_InitModule3("GedaObject", GedaObject_methods, "Base Geda Object extension type.");
  if (object_module == NULL)
    return;

  initArc(module);
  initBox(module);
  initBus(module);
  initCircle(module);
  initComplex(module);
  initLine(module);
  initNet(module);
  initPath(module);
  initPicture(module);
  initPin(module);
  initText(module);

  Py_INCREF(&GedaObjectType);
  PyModule_AddObject(object_module, "GedaObject", (PyObject *)&GedaObjectType);
}
PyTypeObject *GedaObjectClass(void)
{
  return &GedaObjectType;
}
