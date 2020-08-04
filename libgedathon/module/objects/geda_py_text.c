/* C source                                         -*- geda_py_text.c -*- */
/*!
 * \file geda_py_text.c
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
#include <geda/ascii.h>
#include <libgeda/defines.h>

#include "../../include/geda_python.h"
#include "../../include/geda_py_struct.h"
#include "../../include/geda_py_object.h"
#include "../../include/geda_py_color.h"
#include "../../include/geda_py_docs.h"
#include "../../include/gettext.h"

static PyObject *text_module;
static PyObject *geda_module;

static char PyGedaTextObject_doc[] = PyDoc_STR("Geda Text: string, x, y, [, size [, align [, angle]]]");

/* ---------------- Begin PyGedaTextObject Utility Functions --------------- */

static void update_disp_string (PyGedaTextObject *self)
{
  PyObject *py_disp_string = NULL;

  char *text  = PyString_AS_STRING(self->string);
  char *ptr;
  int   length;

  if (self->show == SHOW_NAME_VALUE) {
    py_disp_string = PyString_FromString(text);
  }
  else if (self->show == SHOW_NAME) {
    ptr = text;
    while (( *ptr != ASCII_NUL ) && ( *ptr != ASCII_EQUAL_SIGN )) { ptr++; } /* find "=" */
    if (*ptr == ASCII_EQUAL_SIGN ) {
      length = ptr - text;            /* is pointer offset to "=" */
      py_disp_string = PyString_FromStringAndSize(text, length);
    }
    else {
      py_disp_string = PyString_FromString(text);
    }
  }
  else if (self->show == SHOW_VALUE) {
    ptr = text;
    while (( *ptr != ASCII_NUL ) && ( *ptr != ASCII_EQUAL_SIGN )) { ptr++; } /* find "=" */
    if (*ptr == ASCII_EQUAL_SIGN ) {
      ptr++;            /* pointer offset to first char of value */
      py_disp_string = PyString_FromString(ptr);
    }
    else {
      py_disp_string = PyString_FromString(text);
    }
  }
  else {
    py_disp_string = PyString_FromString(text);
  }

  /* dereference old and save new string */
  SWAP_PY_TMP_OBJECT(disp_string);
}

/* ---------------------- PyGedaTextObject Destructor ---------------------- */

static void PyGedaTextObject_dealloc(PyGedaTextObject* self)
{
  Py_XDECREF(self->string);
  Py_XDECREF(self->disp_string);
  /* Don't dealloc self, the base class will do that */;
  (PyGedaObjectClass())->tp_dealloc((PyObject*)self);
}

/* ---------------------- PyGedaTextObject Constructor --------------------- */

static PyObject *Text_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyGedaTextObject *self;
  struct { int r; int g; int b; int a; }
  default_color = DEFAULT_TEXT_COLOR;

  self = (PyGedaTextObject*)(PyGedaObjectClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    ALLOC_EMPTY_PY_STRING(string)
    ALLOC_EMPTY_PY_STRING(disp_string)

    self->x         = 0;
    self->y         = 0;

    self->cid       = -1;
    self->size      = -1;
    self->alignment = -1;
    self->angle     = -1;

 /* Generic Graphical Attributes Applicable to Textes */
    self->color.r         = default_color.r;
    self->color.g         = default_color.g;
    self->color.b         = default_color.b;
    self->color.a         = default_color.a;

    self->locked_color.r  = default_color.r;
    self->locked_color.g  = default_color.g;
    self->locked_color.b  = default_color.b;
    self->locked_color.a  = default_color.a;
  }

  return (PyObject*)self;
}

/* ---------------------- PyGedaTextObject Initializer --------------------- */

static int Text_init(PyGedaTextObject *self, PyObject *args, PyObject *kwds)
{
  PyObject *py_base_params;
  PyObject *py_name        = NULL;
  PyObject *py_string      = NULL;
  PyObject *py_disp_string = NULL;
  int       type;
  int       pid;
  int       sid;
  int       locked;

  static char *kwlist[] = {"name", "type", "pid", "sid", "locked",
                           "cid", "string", "disp_string", "x", "y",
                           "size", "alignment", "angle",
                           "visible", "show",
                            NULL};

  if (! PyArg_ParseTupleAndKeywords(args, kwds, "|SiiiiiSSiiiiiii:geda.Text.__init__",
                                    kwlist, &py_name, &type, &pid, &sid, &locked,
                                    &self->cid, &py_string, &py_disp_string,
                                    &self->x, &self->y, &self->size,
                                    &self->alignment, &self->angle,
                                    &self->visible, &self->show))
  {
    PyErr_SetString(PyExc_TypeError, "Error initializing new Text object");
    return -1;
  }

  SWAP_PY_TMP_OBJECT(string)
  SWAP_PY_TMP_OBJECT(disp_string)

  py_base_params = Py_BuildValue("Siiii", py_name, type, pid, sid, locked);
  if (PyGedaObjectClass()->tp_init((PyObject *)self, py_base_params, NULL) < 0)
    return -1;

  return 0;
}

static int PyGedaTextObject_print(PyGedaTextObject *text, FILE *file, int flags)
{
  const char *name;
  const char *string;
  const char *dstring;

  int  color_code;
  int  size, align, angle;
  int  visible, show;

  name    = PyString_AsString(text->object.name);
  string  = PyString_AsString(text->string);
  dstring = PyString_AsString(text->disp_string);

  size    = text->size;
  align   = text->alignment;
  angle   = text->angle;

  visible = text->visible;
  show       = text->show;

  color_code = 1; /*text->color*/

  fprintf(file, "<<%s> <%d %d> <string=%s> <disp-string=%s> <color=%d>",
                  name, text->x, text->y, string, dstring, color_code);
  fprintf(file, " <size=%d> <align=%d> <angle=%d> <visible=%d> <show=%d>>",
                  size, align, angle, visible, show);

  return 0;
}

/* ------------------------ PyGedaTextObject Members ----------------------- */

static PyMemberDef Text_members[] = {
  {"cid",         T_INT, offsetof(PyGedaTextObject, cid),      RO, "Text Complex Identifier"},
  {"x",           T_INT, offsetof(PyGedaTextObject, x),         0, "Abscissa of Text insertion point"},
  {"y",           T_INT, offsetof(PyGedaTextObject, y),         0, "Ordinate of Text insertion point"},
  {"size",        T_INT, offsetof(PyGedaTextObject, size),      0, "Font Size"},
  {"alignment",   T_INT, offsetof(PyGedaTextObject, alignment), 0, "Text Justification Code"},
  {"angle",       T_INT, offsetof(PyGedaTextObject, angle),     0, "Text orientation"},
  {"visible",     T_INT, offsetof(PyGedaTextObject, visible),   0, "Text visibility flag"},
  {NULL}  /* Sentinel */
};

static int Text_set_int(PyObject *obj, PyObject *key, PyObject *py_value)
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

  for (index = 0; Text_members[index].name; index++){
    member = &Text_members[index];
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
      PyErr_SetString(PyExc_OverflowError, "Python integer too large to convert to C integer");
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

static int
PyGedaTextObject_set_text(PyGedaTextObject *self, PyObject *value, void *closure)
{
  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the string attribute");
    return -1;
  }

  if (! PyString_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The string attribute value must be a string");
    return -1;
  }

  Py_DECREF(self->string);
  Py_INCREF(value);
  self->string = value;

  self->dirty_text = 1;
  self->object.dirty = 1;
  if (self->object.pid >= 0) {
    PyObject_CallMethod(geda_module, "refresh_attribs", "O", self);
  }

  return 0;
}

static PyObject *PyGedaTextObject_get_text(PyGedaTextObject *self, void *closure)
{
  Py_INCREF(self->string);
  return self->string;
}

static int
PyGedaTextObject_set_show(PyGedaTextObject *self, PyObject *value, void *closure)
{
  long long_val;
  int  new_show;

  if (value == NULL) {
    PyErr_SetString(PyExc_TypeError, "Cannot delete the display code attribute");
    return -1;
  }
  if (!PyInt_Check(value)) {
    PyErr_SetString(PyExc_TypeError,
                    "The display code attribute value must be an integer");
    return -1;
  }

  long_val = PyInt_AsLong(value);

  if ((long_val == -1) && PyErr_Occurred())
    return -1;

  new_show = long_val;

  if (new_show != self->show) {

    self->show = new_show;
    self->object.dirty = 1;

    if (self->object.pid >= 0) {
      self->dirty_text = 1;        /* cause disp_string needs updating */
      PyObject_CallMethod(geda_module, "refresh_attribs", "O", self);
    }
    else { /* not on page, so we'll update disp_string ourself, but the */
           /* show setting is still dirty*/
      update_disp_string(self);
    }
  }
  return 0;
}

static PyObject *PyGedaTextObject_get_show(PyGedaTextObject *self, void *closure)
{
  return Py_BuildValue("i", self->show);
}

static PyGetSetDef Text_getseters[] = {
  {"string",  (getter)PyGedaTextObject_get_text, (setter)PyGedaTextObject_set_text, "text_string_docs", NULL},
  {"show",    (getter)PyGedaTextObject_get_show, (setter)PyGedaTextObject_set_show, "name value flag", NULL},
  {NULL}  /* Sentinel */
};

/* ------------------------------ Begin Methods ---------------------------- */

static PyObject *PyGedaTextObject_disp_string(PyGedaTextObject* self)
{
  Py_INCREF(self->disp_string);
  return self->disp_string;
}

static PyObject *PyGedaTextObject_name(PyGedaTextObject* self)
{
  PyObject *out_str = NULL;

  if (self->show == SHOW_NAME) {
    Py_INCREF(self->disp_string);
    out_str = self->disp_string;
  }
  else {

    char *ptr;
    char *nv = PyString_AsString(self->string); /* Name Value */
    ptr = nv;

    /* Find character "=" */
    while (( *ptr != ASCII_NUL ) && ( *ptr != ASCII_EQUAL_SIGN )) {
      ptr++;
    }

    if (*ptr == ASCII_EQUAL_SIGN ) {
      out_str = PyString_FromStringAndSize(nv, ptr - nv);
    }
  }
  return out_str;
}

static PyObject *PyGedaTextObject_value(PyGedaTextObject* self)
{
  PyObject *out_str = NULL;

  if (self->show == SHOW_VALUE) {
    Py_INCREF(self->disp_string);
    out_str = self->disp_string;
  }
  else {
    char *ptr;
    char *nv = PyString_AsString(self->string);
    ptr = nv;
    while (( *ptr != ASCII_NUL ) && ( *ptr != ASCII_EQUAL_SIGN )) { ptr++; } /* find "=" */
    if (*ptr == ASCII_EQUAL_SIGN ) {
      ptr++;            /* pointer offset to first char of value */
      out_str = PyString_FromString(ptr);
    }
  }

  return out_str;
}

static PyMethodDef Text_methods[] = {
  {"disp_string", (PyCFunction)PyGedaTextObject_disp_string, METH_NOARGS, "text_disp_string_docs"},
  {"name",        (PyCFunction)PyGedaTextObject_name,        METH_NOARGS, "text_name_docs"},
  {"value",       (PyCFunction)PyGedaTextObject_value,       METH_NOARGS, "text_value_docs"},
  {NULL, NULL, 0, NULL}
};

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject PyGedaTextObjectType = {
    PyObject_HEAD_INIT(NULL)
    0,                              /* ob_size,        not used, historical artifact for backward compatibility */
    "geda.Text",                    /* tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(PyGedaTextObject),       /* tp_basicsize,   memory to allocate for this object */
    0,                              /* tp_itemsize*/
    (destructor)PyGedaTextObject_dealloc, /* tp_dealloc*/
    (printfunc)PyGedaTextObject_print,    /* tp_print*/
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
    (setattrofunc)Text_set_int,     /* tp_setattro*/
    0,                              /* tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,            /* tp_flags*/
    PyGedaTextObject_doc,           /* tp_doc */
    0,                              /* tp_traverse */
    0,                              /* tp_clear */
    0,                              /* tp_richcompare */
    0,                              /* tp_weaklistoffset */
    0,                              /* tp_iter */
    0,                              /* tp_iternext */
    Text_methods,                   /* tp_methods */
    Text_members,                   /* tp_members */
    Text_getseters,                 /* tp_getset */
    0,                              /* tp_base, for portability, do not fill here*/
    0,                              /* tp_dict */
    0,                              /* tp_descr_get */
    0,                              /* tp_descr_set */
    0,                              /* tp_dictoffset */
    (initproc)Text_init,            /* tp_init */
    0,                              /* tp_alloc */
    (newfunc)Text_new,              /* tp_new */
};

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PyMODINIT_FUNC initText(PyObject *module)
{
  geda_module = module;

  /* Fill in the bass class */
  PyGedaTextObjectType.tp_base = PyGedaObjectClass();

  if (PyType_Ready(&PyGedaTextObjectType) < 0)
    return;

  text_module = Py_InitModule3("Text", NULL, "Creates an Text object type.");

  if (text_module == NULL)
    return;

  Py_INCREF(&PyGedaTextObjectType);
  PyModule_AddObject(text_module, "Text", (PyObject *)&PyGedaTextObjectType);
}

/*!
 * \brief Retrieve PyGedaTextClass Python Structure
 * \par Function Description
 *  Returns a pointer to the above PyGedaTextObjectType structure.
 */
PyTypeObject *PyGedaTextClass(void)
{
  return &PyGedaTextObjectType;
}
