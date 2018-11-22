/* C source                                         -*- geda_py_page.c -*- */
/*!
 * \file geda_py_page.c
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
#include "../../include/geda_py_page.h"
#include "../../include/geda_py_docs.h"

static PyObject *page_module;
static PyObject *geda_module;

/* ------------------------- PyGedaPageObject Destructor ------------------------- */

static void Page_dealloc(PyGedaPageObject *self)
{
  /* should we call MethodFunctions[CLOSE_METHOD].func? */
  Py_XDECREF(self->filename);
  self->ob_type->tp_free((PyObject*)self);
}

/* ------------------------- PyGedaPageObject Constructor ------------------------ */

static PyObject *Page_new(PyTypeObject *type, PyObject *args)
{
  PyGedaPageObject *self;

  self = (PyGedaPageObject *)type->tp_alloc(type, 0);

  if (self != NULL) {

    ALLOC_EMPTY_PY_STRING(filename)

    self->pid = -1;
  }

  return (PyObject*)self;
}

/* ------------------------- PyGedaPageObject Initializer ------------------------ */

static int Page_init(PyGedaPageObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_filename = NULL;

  static char *kwlist[] = {"filename", "pid", NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "|Si", kwlist,
                                    &py_filename, &self->pid))
    return -1;

  SWAP_PY_TMP_OBJECT(filename)

  return 0;
}

/* --------------------------- PyGedaPageObject Members -------------------------- */

static PyMemberDef Page_members[] = {
  {"pid",      T_INT,       offsetof(PyGedaPageObject, pid),      RO, "Page Identifier"},
  {"modified", T_INT,       offsetof(PyGedaPageObject, modified), RO, "Page modified flag"},
  {NULL}  /* Sentinel */
};

/* ------------------------------ Begin Methods ---------------------------- */

static PyObject *Page_filename(PyGedaPageObject *self)
{
  static PyObject *format = NULL;
  PyObject *args, *result;

  if (format == NULL) {
    format = PyString_FromString("%s");
    if (format == NULL)
      return NULL;
  }

  args = Py_BuildValue("O", self->filename);
  if (args == NULL)
    return NULL;

  result = PyString_Format(format, args);

  Py_DECREF(args);

  return result;
}

static PyObject *go_save(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "save_page", "O", self);
}

static PyObject *go_select(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "set_active_page", "O", self);
}

static PyObject *go_to_page(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "goto_page", "O", self);
}

static PyObject *go_close(PyObject *self)
{
  return PyObject_CallMethod(geda_module, "close_page", "O", self);
}

static PyObject *go_get_objects(PyObject *self, PyObject *args)
{
  return PyObject_CallMethod(geda_module, "get_objects", "O", self);
}

static PyObject *go_add(PyObject *self, PyObject *args)
{
  PyObject *object;

  if (!PyArg_ParseTuple(args, "O!:geda.add_object",
                        PyGedaObjectClass(), &object))
  {
    return NULL;
  }
  return PyObject_CallMethod(geda_module, "add_object", "OO", self, object);
}

static PyObject *go_delete(PyObject *self, PyObject *args)
{
  return PyObject_CallMethod(geda_module, "delete_object", "O", args);
}

static PyMethodDef Page_methods[] = {
  {"name",     (PyCFunction)Page_filename,  METH_NOARGS,  filename_docs},
  {"save",     (PyCFunction)go_save,        METH_NOARGS,  save_page_docs},
  {"select",   (PyCFunction)go_select,      METH_NOARGS,  set_active_page_docs},
  {"goto",     (PyCFunction)go_to_page,     METH_NOARGS,  goto_page_docs},
  {"close",    (PyCFunction)go_close,       METH_NOARGS,  close_page_docs},
  {"objects",  (PyCFunction)go_get_objects, METH_VARARGS, get_objects_docs},
  {"add",      (PyCFunction)go_add,         METH_VARARGS, add_object_docs},
  {"delete",   (PyCFunction)go_delete,      METH_VARARGS, delete_object_docs},
  {NULL}  /* Sentinel */
};

/* -------------------------- PyGedaPageObject GetSeters ------------------------- */

static PyObject *Page_getbounds(PyGedaPageObject *self, void *closure)
{
  return PyObject_CallMethod(geda_module, "get_bounds", "O", self);
}

static PyObject *Page_getfilename(PyGedaPageObject *self, void *closure)
{
    Py_INCREF(self->filename);
    return self->filename;
}

static int Page_setfilename(PyGedaPageObject *self, PyObject *value, void *closure)
{
  PyObject *py_status;
  long result;

  /* TODO: “closure” check for invalid characters for a filename */
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the file name attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError, "The file name attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->filename);
  Py_INCREF(value);
  self->filename = value;

  py_status = PyObject_CallMethod(geda_module, "rename_page", "OO", self,self->filename);

  result = PyInt_AsLong(py_status);
  if ((result == -1) && PyErr_Occurred()) {
      return -1;
  }

  self->modified = result;

  return 0;
}

static PyGetSetDef Page_getseters[] = {
  {"bounds", (getter)Page_getbounds, (setter)NULL, "Bounds", NULL},
  {"filename", (getter)Page_getfilename, (setter)Page_setfilename, "file name", NULL},
  {NULL}  /* Sentinel */
};

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaPageObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /* ob_size,        not used, historical artifact for backward compatibility */
    "geda.Page",               /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(PyGedaPageObject),  /* tp_basicsize,   memory to allocate for this object */
    0,                         /* tp_itemsize*/
    (destructor)Page_dealloc,  /* tp_dealloc*/
    0,                         /* tp_print*/
    0,                         /* tp_getattr*/
    0,                         /* tp_setattr*/
    0,                         /* tp_compare*/
    0,                         /* tp_repr*/
    0,                         /* tp_as_number*/
    0,                         /* tp_as_sequence*/
    0,                         /* tp_as_mapping*/
    0,                         /* tp_hash */
    0,                         /* tp_call*/
    0,                         /* tp_str*/
    0,                         /* tp_getattro*/
    0,                         /* tp_setattro*/
    0,                         /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT,        /* tp_flags*/
    "Geda Page object",        /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,                         /* tp_richcompare */
    0,                         /* tp_weaklistoffset */
    0,                         /* tp_iter */
    0,                         /* tp_iternext */
    Page_methods,              /* tp_methods */
    Page_members,              /* tp_members */
    Page_getseters,            /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    (initproc)Page_init,       /* tp_init */
    0,                         /* tp_alloc */
    (newfunc)Page_new,         /* tp_new */
};

#ifndef PyMODINIT_FUNC  /* declarations for DLL import/export */
#define PyMODINIT_FUNC void
#endif

PyMODINIT_FUNC initPage(PyObject *module)
{
  geda_module = module;

  if ( PyType_Ready(&PyGedaPageObjectType) < 0)
    return;

  page_module = Py_InitModule3("Page", Page_methods, "Geda Page Object type.");

  if (page_module == NULL)
    return;

  Py_INCREF(&PyGedaPageObjectType);
  PyModule_AddObject(page_module, "Page", (PyObject *)&PyGedaPageObjectType);
}

PyTypeObject *PyGedaPageClass(void)
{
  return &PyGedaPageObjectType;
}

/*
PyGedaPageObject *NewPyGedaPageObject(void)
{
  PyGedaPageObject *pypage;
  pypage = PyObject_New(PyGedaPageObject, &PyGedaPageObjectType);
  return pypage;
}
*/