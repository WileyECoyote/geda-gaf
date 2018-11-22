/* C source                                      -*- geda_py_picture.c -*- */
/*!
 * \file geda_py_picture.c
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

static PyObject *picture_module;
static PyObject *geda_module;

static char PyGedaPictureObject_doc[] = PyDoc_STR("Geda Picture: filename, x1, y1, x2, y2 [, angle [, mirror [, embedded]]]");

/* ----------------------- PyGedaPictureObject Destructor ------------------------ */

static void PyGedaPictureObject_dealloc(PyGedaPictureObject *self)
{
  Py_XDECREF(self->filename);
  Py_XDECREF(self->pixel_buffer);

  /* Don't dealloc self, the base class will do that */
  (PyGedaObjectClass())->tp_dealloc((PyObject*)self);
}

/* ----------------------- PyGedaPictureObject Constructor ----------------------- */

static PyObject *Picture_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyGedaPictureObject *self;

  self = (PyGedaPictureObject *)(PyGedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    ALLOC_EMPTY_PY_STRING(filename)

    self->file_length  = 0;
    self->ratio        = 0.0;
    self->angle        = 0;    /* orientation in degrees */
    self->mirror       = 0;
    self->embedded     = 0;    /* is embedded component? */

    self->upper_x      = 0;
    self->upper_y      = 0;
    self->lower_x      = 1;
    self->lower_y      = 1;
  }

  return (PyObject*)self;
}

/* ----------------------- PyGedaPictureObject Initializer ----------------------- */

static int
Picture_init(PyGedaPictureObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_base_params;
  PyObject *py_name          = NULL;
  PyObject *py_filename      = NULL;
  PyObject *py_pixel_buffer  = NULL;

  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked",
                           "filename", "file_length", "ratio",
                           "upper_x", "upper_y", "lower_x", "lower_y",
                           "angle", "mirror", "embedded", "pixel_buffer",
                           NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "|SiiiiSldiiiiiiiO:geda.Picture.__init__",
                                    kwlist, &py_name, &type, &pid, &sid, &locked,
                                    &py_filename, &self->file_length, &self->ratio,
                                    &self->upper_x, &self->upper_y,
                                    &self->lower_x, &self->lower_y,
                                    &self->angle, &self->mirror, &self->embedded,
                                    &py_pixel_buffer))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Picture object");
    return -1;
  }

  SWAP_PY_TMP_OBJECT(filename)
  SWAP_PY_TMP_OBJECT(pixel_buffer)

  py_base_params = Py_BuildValue("Siiii", py_name, type, pid, sid, locked);
  if (PyGedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL) < 0)
    return -1;

  return 0;
}

static int
PyGedaPictureObject_print(PyGedaPictureObject *picture, FILE *file, int flags)
{
  const char *name;
  const char *filename;

  int width  = abs(picture->lower_x - picture->upper_x);
  int height = abs(picture->upper_y - picture->lower_y);

  int x1 = picture->upper_x;
  int y1 = picture->upper_y - height; /* move the origin to 0, 0*/

  int   angle       = picture->angle;
  int   mirror      = picture->mirror;
  int   embedded    = picture->embedded;

  name     = PyString_AsString(picture->object.name);
  filename = PyString_AsString(picture->filename);

  fprintf(file, "<<%s> <filename=%s> <%d,%d> <width=%d> <height=%d> <embedded=%d> <angle=%d> <mirror=%d>>",
                 name, filename, x1, y1, width, height, embedded, angle, mirror);

  return 0;
}

static PyMemberDef Picture_members[] = {
  {"embedded",    T_BOOL,   offsetof(PyGedaPictureObject, embedded),    RO, "Picture embedded"},
  {"file_length", T_UINT,   offsetof(PyGedaPictureObject, file_length), RO, "Picture file length"},
  {"ratio",       T_DOUBLE, offsetof(PyGedaPictureObject, ratio),       RO, "Picture ratio"},
  {"angle",       T_INT,    offsetof(PyGedaPictureObject, angle),        0, "Picture angle"},
  {"mirror",      T_BOOL,   offsetof(PyGedaPictureObject, mirror),       0, "Picture mirror"},
  {"upper_x",     T_INT,    offsetof(PyGedaPictureObject, upper_x),      0, "Picture Upper Abscissa"},
  {"upper_y",     T_INT,    offsetof(PyGedaPictureObject, upper_y),      0, "Picture Upper Ordinate"},
  {"lower_x",     T_INT,    offsetof(PyGedaPictureObject, lower_x),      0, "Picture Lower Abscissa"},
  {"lower_y",     T_INT,    offsetof(PyGedaPictureObject, lower_y),      0, "Picture Lower Ordinate"},
  {NULL}  /* Sentinel */
};

static int Picture_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
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

  old_value = &index;

  for (index = 0; Picture_members[index].name; index++){
    member = &Picture_members[index];
    str = member->name;
    if (!strcmp(str, name)) {
      old_value = (int*)((char *)obj + member->offset);
      break;
    }
    str = NULL;
  };

  if (index < 3) {
    PyErr_Format(PyExc_ReferenceError, "%s attribute is READ-ONLY", name);
    return 0;
  }

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
      PyErr_SetString(PyExc_OverflowError,
                      "Python int too large to convert to C int");
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

static PyObject *PyGedaPictureObject_filename(PyGedaPictureObject* self)
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

static PyMethodDef Picture_methods[] = {
  {"filename", (PyCFunction)PyGedaPictureObject_filename, METH_NOARGS,  "object_name_docs"},
  /* reload? */
  {NULL, NULL, 0, NULL}
};

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaPictureObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size,       not used, historical artifact for backward compatibility */
    "geda.Picture",                    /* tp_name,       default textual representation our objects, used in some error messages*/
    sizeof(PyGedaPictureObject),             /* tp_basicsize,  memory to allocate for this object */
    0,                                 /* tp_itemsize*/
    (destructor)PyGedaPictureObject_dealloc, /* tp_dealloc*/
    (printfunc)PyGedaPictureObject_print,    /* tp_print*/
    0,                                 /* tp_getattr*/
    0,                                 /* tp_setattr*/
    0,                                 /* tp_compare*/
    0,                                 /* tp_repr*/
    0,                                 /* tp_as_number*/
    0,                                 /* tp_as_sequence*/
    0,                                 /* tp_as_mapping*/
    0,                                 /* tp_hash */
    0,                                 /* tp_call*/
    0,                                 /* tp_str*/
    0,                                 /* tp_getattro*/
    (setattrofunc)Picture_set_int,     /* tp_setattro*/
    0,                                 /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,               /* tp_flags*/
    PyGedaPictureObject_doc,                 /* tp_doc */
    0,                                 /* tp_traverse */
    0,                                 /* tp_clear */
    0,                                 /* tp_richcompare */
    0,                                 /* tp_weaklistoffset */
    0,                                 /* tp_iter */
    0,                                 /* tp_iternext */
    Picture_methods,                   /* tp_methods */
    Picture_members,                   /* tp_members */
    0,                                 /* tp_getset */
    0,                                 /* tp_base, for portability, do not fill here*/
    0,                                 /* tp_dict */
    0,                                 /* tp_descr_get */
    0,                                 /* tp_descr_set */
    0,                                 /* tp_dictoffset */
    (initproc)Picture_init,            /* tp_init */
    0,                                 /* tp_alloc */
    (newfunc)Picture_new,              /* tp_new */
};

PyMODINIT_FUNC initPicture(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  PyGedaPictureObjectType.tp_base = PyGedaObjectClass();

  if ( PyType_Ready(&PyGedaPictureObjectType) < 0)
    return;

  picture_module = Py_InitModule3("Picture", NULL, "Creates a Picture object type.");

  if (picture_module == NULL)
    return;

  Py_INCREF(&PyGedaPictureObjectType);
  PyModule_AddObject(picture_module, "Picture", (PyObject *)&PyGedaPictureObjectType);
}

PyTypeObject *PyGedaPictureClass(void)
{
  return &PyGedaPictureObjectType;
}
