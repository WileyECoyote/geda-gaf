/* C source                                       -*- geda_py_object.c -*- */
/*!
 * \file geda_py_object.c
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
#include "../../include/geda_py_docs.h"

static PyObject *object_module;
static PyObject *geda_module;

/* ------------------------- PyGedaObject Destructor ------------------------- */

static void PyGedaObject_dealloc(PyGedaObject *self)
{
#if DEBUG
  fprintf(stderr, "PyGedaObject_dealloc: %s, id=%d\n", PyString_AsString(self->name), self->sid);
#endif
  Py_XDECREF(self->name);
  Py_XDECREF(self->attributes);
  self->ob_type->tp_free((PyObject*)self);
}

/* ------------------------- PyGedaObject Constructor ------------------------ */

static PyObject *PyGedaObject_new(PyTypeObject *type, PyObject *args)
{
  PyGedaObject *self;

  self = (PyGedaObject*)type->tp_alloc(type, 0);

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

  return (PyObject*)self;
}

/* ------------------------- PyGedaObject Initializer ------------------------ */

static int PyGedaObject_init(PyGedaObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_name = NULL;
  PyObject *py_attributes = NULL;

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

PyObject *PyGedaObject_str(PyGedaObject *self)
{
  return self->name;
}

/* --------------------------- PyGedaObject Members -------------------------- */

static PyMemberDef PyGedaObject_members[] = {
  {"pid",      T_INT,  offsetof(PyGedaObject, pid),       RO, "Page Container Identifier"},
  {"sid",      T_INT,  offsetof(PyGedaObject, sid),       RO, "Geda Sequence Identifier"},
  {"type",     T_INT,  offsetof(PyGedaObject, type),      RO, "Geda Type Identifier"},

  {"selected",         T_BOOL, offsetof(PyGedaObject, selected),        0, "object selected attribute"},
  {"locked",           T_BOOL, offsetof(PyGedaObject, locked),          0, "object selectable attribute"},
  {"auto_attributes",  T_BOOL, offsetof(PyGedaObject, auto_attributes), 0, "Enable or disable auto attribute"},

  {NULL}  /* Sentinel */
};

static int Object_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
{
  PyGedaObject *geda_object = (PyGedaObject*)obj;
  char         *name        = PyString_AsString(key);

  int result;
  int index;

  /*Note: The setattrofunc in derived objects only set their integer types.
   *      The following return to PyObject_GenericSetAttr not only will set
   *      integers within this base class, but can also call the tp_getset's
   *      in the derived type to set string values. This is not obvious */
  result = PyObject_GenericSetAttr(obj, key, py_value);

  /* Check if key is member of this object, we don't set but we need to
   * know if one of our members is being set so we can set our flag */
  for (index = 0; PyGedaObject_members[index].name; index++){

    PyMemberDef *member = &PyGedaObject_members[index];

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

static PyObject *PyGedaObject_name(PyGedaObject* self)
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

static PyObject *go_add(PyObject *self, PyObject *args)
{
  PyObject *unknown;
  PyObject *bute;

  if(!PyArg_ParseTuple(args, "O!O!:geda.add_object",
                       PyGedaObjectClass(), &unknown,
                       PyGedaTextClass(), &bute))
  {
    return NULL;
  }

  return PyObject_CallMethod(geda_module, "add_object", "OO", self, args);
}

static PyObject *go_copy(PyObject *self, PyObject *args)
{
  int       dx = -1;
  int       dy = -1;

  PyArg_ParseTuple(args, "|ii:geda.add_objects, Object PyList", &dx, &dy);
  return PyObject_CallMethod(geda_module, "copy_object", "Oii", self, dx, dy);
}

static PyObject *go_delete(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "delete_object", "O", self);
}

static PyObject *go_rotate(PyObject *self, PyObject *args)
{
  return PyObject_CallMethod(geda_module, "rotate_object", "O", self, args);
}

static PyObject *go_select(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "select_object", "O", self);
}

static PyObject *go_unselect(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "unselect_object", "O", self);
}

/*add lock, unlock*/
static PyMethodDef PyGedaObject_methods[] = {
  {"name",     (PyCFunction)PyGedaObject_name, METH_NOARGS,  object_name_docs},
  {"add",      (PyCFunction)go_add,          METH_VARARGS, add_attrib_docs},
  {"copy",     (PyCFunction)go_copy,         METH_VARARGS, copy_object_docs},
  {"delete",   (PyCFunction)go_delete,       METH_NOARGS,  delete_object_docs},
  {"rotate",   (PyCFunction)go_rotate,       METH_VARARGS, rotate_object_docs},
  {"select",   (PyCFunction)go_select,       METH_NOARGS,  select_object_docs},
  {"unselect", (PyCFunction)go_unselect,     METH_NOARGS,  unselect_object_docs},
  {NULL, NULL, 0, NULL}
};

/* -------------------------- PyGedaPageObject GetSeters ------------------------- */

static PyObject *Geda_getbounds(PyGedaObject *self, void *closure)
{
  return PyObject_CallMethod(geda_module, "get_bounds", "O", self);
}

static PyGetSetDef PyGedaObject_getseters[] = {
  {"bounds", (getter)Geda_getbounds, (setter)NULL, "Bounds", NULL},
  {NULL}  /* Sentinel */
};
/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                              /* ob_size, not used, historical artifact for backward compatibility */
    "geda.PyGedaObject",            /* tp_name, default textual representation an object, used in some error messages*/
    sizeof(PyGedaObject),           /* tp_basicsize, memory to allocate for this object */
    0,                              /* tp_itemsize */
    (destructor)PyGedaObject_dealloc, /* tp_dealloc */
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
    (reprfunc)PyGedaObject_str,     /* tp_str */
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
    PyGedaObject_methods,           /* tp_methods */
    PyGedaObject_members,           /* tp_members */
    PyGedaObject_getseters,         /* tp_getset */
    0,                              /* tp_base */
    0,                              /* tp_dict */
    0,                              /* tp_descr_get */
    0,                              /* tp_descr_set */
    0,                              /* tp_dictoffset */
    (initproc)PyGedaObject_init,    /* tp_init */
    0,                              /* tp_alloc */
    (newfunc)PyGedaObject_new,      /* tp_new */
};

#ifndef PyMODINIT_FUNC  /* declarations for DLL import/export */
#define PyMODINIT_FUNC void
#endif
PyMODINIT_FUNC initPyGedaObject(PyObject *module)
{
  geda_module = module;
  if ( PyType_Ready(&PyGedaObjectType) < 0)
    return;

  object_module = Py_InitModule3("PyGedaObject", PyGedaObject_methods, "Base Geda Object extension type.");
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

  Py_INCREF(&PyGedaObjectType);
  PyModule_AddObject(object_module, "PyGedaObject", (PyObject *)&PyGedaObjectType);
}

PyTypeObject *PyGedaObjectClass(void)
{
  return &PyGedaObjectType;
}
