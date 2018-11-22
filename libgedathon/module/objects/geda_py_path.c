/* C source                                         -*- geda_py_path.c -*- */
/*!
 * \file geda_py_path.c
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

static PyObject *path_module;
static PyObject *geda_module;

static char PyGedaPathObject_doc[] = PyDoc_STR("Geda Path: upper_x, upper_y, lower_x, lower_y [, color]");

/* ------------------------- PyGedaPathObject Destructor ------------------------- */

static void PyGedaPathObject_dealloc(PyGedaPathObject *self)
{
  Py_XDECREF(self->sections);
  /* Don't dealloc self, the base class will do that */
  (PyGedaObjectClass())->tp_dealloc((PyObject*)self);
}

/* ------------------------- PyGedaPathObject Constructor ------------------------ */

static PyObject *Path_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyGedaPathObject *self;
  struct { int r; int g; int b; int a; }
  default_color = DEFAULT_NET_COLOR;

  self = (PyGedaPathObject*)(PyGedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    self->num_sections      = 0;
    self->num_sections_max  = 0;

    self->sections          = NULL;   /* List of Path segments */

 /* Generic Graphical Attributes Applicable to Paths */
    self->color.r         = default_color.r;
    self->color.g         = default_color.g;
    self->color.b         = default_color.b;
    self->color.a         = default_color.a;

    self->locked_color.r  = default_color.r;
    self->locked_color.g  = default_color.g;
    self->locked_color.b  = default_color.b;
    self->locked_color.a  = default_color.a;

 /* Line-Type */
    self->line_end        = END_NONE;
    self->line_type       = TYPE_SOLID;
    self->line_width      = 0;
    self->line_space      = 0;
    self->line_length     = 0;

  }

  return (PyObject*)self;
}

/* ------------------------- PyGedaPathObject Initializer ------------------------ */

static int Path_init(PyGedaPathObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_base_params;
  PyObject *py_name = NULL;
  PyObject *py_path_string;
  PyObject *py_sections;
  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked",
                           "path-string",
                           "num_sections", "num_sections_max", "sections",
                           "fill_type", "fill_width", "fill_angle1",
                           "fill_pitch1", "fill_angle2", "fill_pitch2",
                           "end_type", "line_type",
                           "line_width", "line_space", "line_length",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "|SiiiiSiiO!iiiiiiiiiii:geda.Path.__init__", kwlist,
                                    &py_name, &type, &pid, &sid, &locked, &py_path_string,
                                    &self->num_sections, &self->num_sections_max,
                                    &PyList_Type, &py_sections,
                                    &self->fill_type, &self->fill_width, &self->fill_angle1,
                                    &self->fill_pitch1, &self->fill_angle2, &self->fill_pitch2,
                                    &self->line_end, &self->line_type, &self->line_width,
                                    &self->line_space, &self->line_length))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Path object");
    return -1;
  }

  SWAP_PY_TMP_OBJECT(path_string)
  SWAP_PY_TMP_OBJECT(sections)

  py_base_params = Py_BuildValue("Siiii", py_name, type, pid, sid, locked);
  if (PyGedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL) < 0)
    return -1;

  return 0;
}

static int PyGedaPathObject_print(PyGedaPathObject *path, FILE *file, int flags)
{
  const char *name;
  char       *path_string;

  int color_code;
  int fill_type, fill_width, fill_angle1, fill_pitch1, fill_angle2, fill_pitch2;
  int line_width, line_end, line_type, line_length, line_space;

  name         = PyString_AsString(path->object.name);
  path_string  = PyString_AsString(path->path_string);

  color_code = 1; /*path->color*/

  fill_type   = path->fill_type;
  fill_width  = path->fill_width;
  fill_angle1 = path->fill_angle1;
  fill_pitch1 = path->fill_pitch1;
  fill_angle2 = path->fill_angle2;
  fill_pitch2 = path->fill_pitch2;

  /* line type parameters */
  line_width  = path->line_width;
  line_end    = path->line_end;
  line_type   = path->line_type;
  line_length = path->line_length;
  line_space  = path->line_space;

  /* Describe a circle with post-20000704 file parameters */
  fprintf(file, "<<%s> <color=%d>", name, color_code);
  fprintf(file, " <line-type <width=%d> <end=%d> <type=%d> <dash=%d> <spaces=%d>>",
                   line_width, line_end, line_type, line_length, line_space);
  fprintf(file, " <fill-type <type=%d <width=%d> <angle1=%d> <pitch1=%d> <angle2=%d>>> <pitch2=%d>>",
                   fill_type, fill_width, fill_angle1, fill_pitch1, fill_angle2, fill_pitch2);
  fprintf(file, " <path_string <%s>>", path_string);

  return 0;
}

/* --------------------------- PyGedaPathObject Members -------------------------- */

static PyMemberDef Path_members[] = {
 /* Hatching */
  {"fill_type",   T_INT, offsetof(PyGedaPathObject, fill_type),   0, "Hatch fill type"},
  {"fill_width",  T_INT, offsetof(PyGedaPathObject, fill_width),  0, "Hatch line width"},
  {"fill_angle1", T_INT, offsetof(PyGedaPathObject, fill_angle1), 0, "Hatch fill angle"},
  {"fill_pitch1", T_INT, offsetof(PyGedaPathObject, fill_pitch1), 0, "Hatch fill pitch"},
  {"fill_angle2", T_INT, offsetof(PyGedaPathObject, fill_angle2), 0, "Mesh hatch second fill angle"},
  {"fill_pitch2", T_INT, offsetof(PyGedaPathObject, fill_pitch2), 0, "Mesh hatch second fill pitch"},

  /* Line-Type */
  {"end_type",    T_INT, offsetof(PyGedaPathObject, line_end),    0, "Endpoint style"},
  {"line_type",   T_INT, offsetof(PyGedaPathObject, line_type),   0, "Line type"},
  {"line_width",  T_INT, offsetof(PyGedaPathObject, line_width),  0, "Line width"},
  {"line_space",  T_INT, offsetof(PyGedaPathObject, line_space),  0, "Line space/gaps"},
  {"line_length", T_INT, offsetof(PyGedaPathObject, line_length), 0, "Line dash length"},
  {NULL}  /* Sentinel */
};

static int Path_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
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

  for (index = 0; Path_members[index].name; index++){
    member = &Path_members[index];
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

static PyObject *PyGedaPathObject_sections(PyGedaPathObject* self)
{
  static PyObject *format = NULL;
  PyObject *args, *result;

  if (format == NULL) {
    format = PyString_FromString("%i,%i,%i,%i,%i,%i,%i");
                                 if (format == NULL)
                                   return NULL;
  }

  args = Py_BuildValue("O", self->sections);
  if (args == NULL)
    return NULL;

  result = PyString_Format(format, args);
  Py_DECREF(args);

  return result;
}

static PyMethodDef Path_methods[] = {
  {"sections", (PyCFunction)PyGedaPathObject_sections, METH_NOARGS,  "object_name_docs"},
  /* modify? */
  {NULL, NULL, 0, NULL}
};

/* -------------------------- PyGedaPathObject GetSeters ------------------------- */


/* ------------------------ Begin Getters and Setters ---------------------- */
static PyObject *Path_get_string(PyGedaPathObject *self, void *closure)
{
  Py_INCREF(self->path_string);
  return self->path_string;
}

static int Path_set_string(PyGedaPathObject *self, PyObject *value, void *closure)
{
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the path-string attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The path-string attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->path_string);
  Py_INCREF(value);
  self->path_string = value;

  self->dirty_string = 1;
  self->object.dirty = 1;
  if(self->object.pid >= 0)
    PyObject_CallMethod(geda_module, "refresh_attribs", "O", self);

  return 0;
}

static PyGetSetDef Path_getseters[] = {
  {"string", (getter)Path_get_string, (setter)Path_set_string, "path_string_docs", NULL},
  {NULL}  /* Sentinel */
};

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaPathObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                                 /* ob_size,        not used, historical artifact for backward compatibility */
    "geda.Path",                       /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(PyGedaPathObject),          /* tp_basicsize,   memory to allocate for this object */
    0,                                 /* tp_itemsize*/
    (destructor)PyGedaPathObject_dealloc,    /* tp_dealloc*/
    (printfunc)PyGedaPathObject_print,       /* tp_print*/
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
    (setattrofunc)Path_set_int,        /* tp_setattro*/
    0,                                 /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,               /* tp_flags*/
    PyGedaPathObject_doc,              /* tp_doc */
    0,                                 /* tp_traverse */
    0,                                 /* tp_clear */
    0,                                 /* tp_richcompare */
    0,                                 /* tp_weaklistoffset */
    0,                                 /* tp_iter */
    0,                                 /* tp_iternext */
    Path_methods,                      /* tp_methods */
    Path_members,                      /* tp_members */
    Path_getseters,                    /* tp_getset */
    0,                                 /* tp_base, for portability, do not fill here*/
    0,                                 /* tp_dict */
    0,                                 /* tp_descr_get */
    0,                                 /* tp_descr_set */
    0,                                 /* tp_dictoffset */
    (initproc)Path_init,               /* tp_init */
    0,                                 /* tp_alloc */
    (newfunc)Path_new,                 /* tp_new */
};

PyMODINIT_FUNC initPath(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  PyGedaPathObjectType.tp_base = PyGedaObjectClass();

  if ( PyType_Ready(&PyGedaPathObjectType) < 0)
    return;

  path_module = Py_InitModule3("Path", NULL, "Create a new Path object type.");

  if (path_module == NULL)
    return;

  Py_INCREF(&PyGedaPathObjectType);
  PyModule_AddObject(path_module, "Path", (PyObject *)&PyGedaPathObjectType);
}

PyTypeObject *PyGedaPathClass(void)
{
  return &PyGedaPathObjectType;
}
