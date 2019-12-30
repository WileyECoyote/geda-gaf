/* -*- Mode: C; c-basic-offset: 4 -*-
 * Gimp-Python - allows the writing of Gimp plugins in Python.
 * Copyright (C) 2005-2006  Manish Singh <yosh@gimp.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#define NO_IMPORT_PYGOBJECT

#include "../../include/geda_py_struct.h"
#include "../../include/geda_py_color.h"

static PyObject *
Color_new(PyTypeObject *type, PyObject *args, PyObject *kwds)

PyGedaColorObject_new(const PyGedaColorObject *rgb)
{
  PyGedaColorObject *self;
  self = (PyGedaComplexObject*)(PyGedaColorClass())->tp_new(type, args, kwds);

  if (self != NULL) {

    self->r = 0;
    self->g = 0;
    self->b = 0;
    self->a = 0;

  }

  return (PyObject*)self;
  //  return pyg_boxed_new(GIMP_TYPE_RGB, (void *)rgb, TRUE, TRUE);
}

static int rgb_init(PyGBoxed *self, PyObject *args, PyObject *kwargs)
{
    PyObject *r, *g, *b, *a = NULL;
    PyGedaColorObject rgb;
    static char *kwlist[] = { "r", "g", "b", "a", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "OOO|O:set", kwlist,
                                     &r, &g, &b, &a))
        return -1;

    SET_COLOR_MEMBER(r);
    SET_COLOR_MEMBER(g);
    SET_COLOR_MEMBER(b);

    if (a)
    SET_COLOR_MEMBER(a);
    else
        rgb.a = 1.0;

    self->gtype = GIMP_TYPE_RGB;
    self->free_on_dealloc = TRUE;
    self->boxed = g_boxed_copy(GIMP_TYPE_RGB, &rgb);

    return 0;
}

static PyObject *rgb_set(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyObject *r = NULL, *g = NULL, *b = NULL, *a = NULL;
    PyGedaColorObject tmprgb, *rgb;
    static char *kwlist[] = { "r", "g", "b", "a", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|OOOO:set", kwlist,
                              &r, &g, &b, &a))
        return NULL;

    if (!r && !g && !b && !a) {
     PyErr_SetString(PyExc_TypeError, "must provide r,g,b or a arguments");
     return NULL;
    }

    if ((r && (!g || !b)) ||
        (g && (!r || !b)) ||
        (b && (!r || !g))) {
     PyErr_SetString(PyExc_TypeError, "must provide all 3 r,g,b arguments");
     return NULL;
    }

    rgb = pyg_boxed_get(self, PyGedaColorObject);
    tmprgb = *rgb;

#define SET_MEMBER(m)     G_STMT_START {                         \
    if (PyInt_Check(m))                                      \
     tmprgb.m = (double) PyInt_AS_LONG(m) / 255.0;             \
    else if (PyFloat_Check(m))                              \
        tmprgb.m = PyFloat_AS_DOUBLE(m);                   \
    else {                                            \
     PyErr_SetString(PyExc_TypeError,                   \
                   #m " must be an int or a float");     \
     return NULL;                                      \
    }                                                  \
} G_STMT_END

    if (r) {
     SET_MEMBER(r);
     SET_MEMBER(g);
     SET_MEMBER(b);
    }

    if (a)
     SET_MEMBER(a);

#undef SET_MEMBER

    *rgb = tmprgb;

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *rgb_set_alpha(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyObject *py_a;
    PyGedaColorObject *rgb;
    static char *kwlist[] = { "a", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                                  "O:set_alpha", kwlist,
                              &py_a))
        return NULL;

    rgb = pyg_boxed_get(self, PyGedaColorObject);

    if (PyInt_Check(py_a))
     rgb->a = (double) PyInt_AS_LONG(py_a) / 255.0;
    else if (PyFloat_Check(py_a))
        rgb->a = PyFloat_AS_DOUBLE(py_a);
    else {
     PyErr_SetString(PyExc_TypeError, "a must be an int or a float");
     return NULL;
    }

    Py_INCREF(Py_None);

    return Py_None;
}

static PyObject *rgb_add(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyObject *color;
    bool with_alpha = FALSE;
    static char *kwlist[] = { "color", "with_alpha", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O!|i:add", kwlist,
                              &PyPyGedaColorObject_Type, &color, &with_alpha))
        return NULL;

    if (with_alpha)
     gimp_rgba_add(pyg_boxed_get(self, PyGedaColorObject),
                   pyg_boxed_get(color, PyGedaColorObject));
    else
     gimp_rgb_add(pyg_boxed_get(self, PyGedaColorObject),
                  pyg_boxed_get(color, PyGedaColorObject));

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject *rgb_subtract(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyObject *color;
    bool with_alpha = FALSE;
    static char *kwlist[] = { "color", "with_alpha", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O!|i:subtract", kwlist,
                              &PyPyGedaColorObject_Type, &color, &with_alpha))
        return NULL;

    if (with_alpha)
     gimp_rgba_subtract(pyg_boxed_get(self, PyGedaColorObject),
                      pyg_boxed_get(color, PyGedaColorObject));
    else
     gimp_rgb_subtract(pyg_boxed_get(self, PyGedaColorObject),
                     pyg_boxed_get(color, PyGedaColorObject));

    Py_INCREF(Py_None);

    return Py_None;
}

static PyObject *rgb_multiply(PyObject *self, PyObject *args, PyObject *kwargs)
{
    double factor;
    bool with_alpha = FALSE;
    static char *kwlist[] = { "factor", "with_alpha", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "d|i:multiply", kwlist,
                              &factor, &with_alpha))
        return NULL;

    if (with_alpha)
     gimp_rgba_multiply(pyg_boxed_get(self, PyGedaColorObject), factor);
    else
     gimp_rgb_multiply(pyg_boxed_get(self, PyGedaColorObject), factor);

    Py_INCREF(Py_None);

    return Py_None;
}

static PyObject *rgb_distance(PyObject *self, PyObject *args, PyObject *kwargs)
{
  PyObject *color;
  bool alpha = FALSE;
  double ret;
  static char *kwlist[] = { "color", "alpha", NULL };

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O!|i:distance", kwlist,
    &PyPyGedaColorObject_Type, &color, &alpha))
    return NULL;

  ret = gimp_rgb_distance(pyg_boxed_get(self, PyGedaColorObject),
                          pyg_boxed_get(color, PyGedaColorObject));


  return PyFloat_FromDouble(ret);
}

static PyObject *rgb_max(PyObject *self)
{
  return PyFloat_FromDouble(gimp_rgb_max(pyg_boxed_get(self, PyGedaColorObject)));
}

static PyObject *rgb_min(PyObject *self)
{
  return PyFloat_FromDouble(gimp_rgb_min(pyg_boxed_get(self, PyGedaColorObject)));
}

static PyObject *rgb_clamp(PyObject *self)
{
    gimp_rgb_clamp(pyg_boxed_get(self, PyGedaColorObject));

    Py_INCREF(Py_None);

    return Py_None;
}

static PyObject *rgb_gamma(PyObject *self, PyObject *args, PyObject *kwargs)
{
    double gamma;
    static char *kwlist[] = { "gamma", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "d:gamma", kwlist, &gamma))
        return NULL;

    gimp_rgb_gamma(pyg_boxed_get(self, PyGedaColorObject), gamma);

    Py_INCREF(Py_None);

    return Py_None;
}

static PyObject *rgb_luminance(PyObject *self)
{
    return PyFloat_FromDouble(gimp_rgb_luminance(pyg_boxed_get(self, PyGedaColorObject)));
}

static PyObject *rgb_composite(PyObject *self, PyObject *args, PyObject *kwargs)
{
  PyObject *color;
  int mode = GIMP_RGB_COMPOSITE_NORMAL;
  static char *kwlist[] = { "color", "mode", NULL };

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O!|i:composite", kwlist,
                                  &PyPyGedaColorObject_Type, &color, &mode))
    return NULL;

  if (mode < GIMP_RGB_COMPOSITE_NONE || mode > GIMP_RGB_COMPOSITE_BEHIND) {
    PyErr_SetString(PyExc_TypeError, "composite type is not valid");
    return NULL;
  }

  gimp_rgb_composite(pyg_boxed_get(self, PyGedaColorObject),
                     pyg_boxed_get(color, PyGedaColorObject),
                     mode);

  Py_INCREF(Py_None);

  return Py_None;
}

static PyObject *rgb_parse_name(PyObject *self, PyObject *args, PyObject *kwargs)
{
  char *name;
  int len;
  bool success;
  static char *kwlist[] = { "name", NULL };

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, "s#:parse_name", kwlist,
    &name, &len))
    return NULL;

  success = gimp_rgb_parse_name(pyg_boxed_get(self, PyGedaColorObject), name, len);

  if (!success) {
    PyErr_SetString(PyExc_ValueError, "unable to parse color name");
    return NULL;
  }

  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *rgb_parse_hex(PyObject *self, PyObject *args, PyObject *kwargs)
{
    char *hex;
    int len;
    bool success;
    static char *kwlist[] = { "hex", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "s#:parse_hex", kwlist,
                              &hex, &len))
        return NULL;

    success = gimp_rgb_parse_hex(pyg_boxed_get(self, PyGedaColorObject), hex, len);

    if (!success) {
     PyErr_SetString(PyExc_ValueError, "unable to parse hex value");
     return NULL;
    }

    Py_INCREF(Py_None);

    return Py_None;
}

static PyObject *rgb_parse_css(PyObject *self, PyObject *args, PyObject *kwargs)
{
    char *css;
    int   len;
    bool  success, with_alpha = FALSE;

    static char *kwlist[] = { "css", "with_alpha", NULL };

    if (!PyArg_ParseTupleAndKeywords(args, kwargs,
                              "s#|i:parse_css", kwlist,
                              &css, &len, &with_alpha))
        return NULL;

    if (with_alpha)
     success = gimp_rgba_parse_css(pyg_boxed_get(self, PyGedaColorObject), css, len);
    else
     success = gimp_rgb_parse_css(pyg_boxed_get(self, PyGedaColorObject), css, len);

    if (!success) {
     PyErr_SetString(PyExc_ValueError, "unable to parse CSS color");
     return NULL;
    }

    Py_INCREF(Py_None);

    return Py_None;
}

/* __getstate__ is not exposed */
static PyObject *rgb_getstate(PyObject *self)
{
    PyGedaColorObject *rgb;

    rgb = pyg_boxed_get(self, PyGedaColorObject);

    return Py_BuildValue("dddd", rgb->r, rgb->g, rgb->b, rgb->a);
}

static PyObject *rgb_reduce(PyObject *self)
{
    return Py_BuildValue("ON", self->ob_type, rgb_getstate(self));
}

static PyMethodDef rgb_methods[] = {
    { "set",        (PyCFunction)rgb_set,        METH_VARARGS|METH_KEYWORDS },
    { "set_alpha",  (PyCFunction)rgb_set_alpha,  METH_VARARGS|METH_KEYWORDS },
    { "add",        (PyCFunction)rgb_add,        METH_VARARGS|METH_KEYWORDS },
    { "subtract",   (PyCFunction)rgb_subtract,   METH_VARARGS|METH_KEYWORDS },
    { "multiply",   (PyCFunction)rgb_multiply,   METH_VARARGS|METH_KEYWORDS },
    { "distance",   (PyCFunction)rgb_distance,   METH_VARARGS|METH_KEYWORDS },
    { "max",        (PyCFunction)rgb_max,        METH_NOARGS },
    { "min",        (PyCFunction)rgb_min,        METH_NOARGS },
    { "clamp",      (PyCFunction)rgb_clamp,      METH_NOARGS },
    { "gamma",      (PyCFunction)rgb_gamma,      METH_VARARGS|METH_KEYWORDS },
    { "luminance",  (PyCFunction)rgb_luminance,  METH_NOARGS },
    { "composite",  (PyCFunction)rgb_composite,  METH_VARARGS|METH_KEYWORDS },
    { "parse_name", (PyCFunction)rgb_parse_name, METH_VARARGS|METH_KEYWORDS },
    { "parse_hex",  (PyCFunction)rgb_parse_hex,  METH_VARARGS|METH_KEYWORDS },
    { "parse_css",  (PyCFunction)rgb_parse_css,  METH_VARARGS|METH_KEYWORDS },
    { "__reduce__", (PyCFunction)rgb_reduce,     METH_NOARGS },
    { NULL, NULL, 0 }
};

#define MEMBER_ACCESSOR(m) \
static PyObject *                                            \
rgb_get_ ## m(PyObject *self, void *closure)                         \
{                                                       \
    return PyFloat_FromDouble(pyg_boxed_get(self, PyGedaColorObject)->m);             \
}                                                       \
static int                                                  \
rgb_set_ ## m(PyObject *self, PyObject *value, void *closure)             \
{                                                       \
    PyGedaColorObject *rgb = pyg_boxed_get(self, PyGedaColorObject);                   \
    if (value == NULL) {                                      \
     PyErr_SetString(PyExc_TypeError, "cannot delete value");     \
     return -1;                                            \
    }                                                       \
    else if (PyInt_Check(value))                              \
     rgb->m = (double) PyInt_AS_LONG(value) / 255.0;                   \
    else if (PyFloat_Check(value))                              \
        rgb->m = PyFloat_AS_DOUBLE(value);                         \
    else {                                                  \
     PyErr_SetString(PyExc_TypeError, "type mismatch");             \
     return -1;                                            \
    }                                                       \
    return 0;                                                  \
}

MEMBER_ACCESSOR(r);
MEMBER_ACCESSOR(g);
MEMBER_ACCESSOR(b);
MEMBER_ACCESSOR(a);

#undef MEMBER_ACCESSOR

static PyGetSetDef rgb_getsets[] = {
    { "r",     (getter)rgb_get_r, (setter)rgb_set_r },
    { "g",     (getter)rgb_get_g, (setter)rgb_set_g },
    { "b",     (getter)rgb_get_b, (setter)rgb_set_b },
    { "a",     (getter)rgb_get_a, (setter)rgb_set_a },
    { "red",   (getter)rgb_get_r, (setter)rgb_set_r },
    { "green", (getter)rgb_get_g, (setter)rgb_set_g },
    { "blue",  (getter)rgb_get_b, (setter)rgb_set_b },
    { "alpha", (getter)rgb_get_a, (setter)rgb_set_a },
    { NULL,    (getter)0, (setter)0 },
};

static Py_ssize_t rgb_length(PyObject *self)
{
    return 4;
}

static PyObject *rgb_getitem(PyObject *self, Py_ssize_t pos)
{
    PyGedaColorObject *rgb;
    double val;

    if (pos < 0)
        pos += 4;

    if (pos < 0 || pos >= 4) {
     PyErr_SetString(PyExc_IndexError, "index out of range");
     return NULL;
    }

    rgb = pyg_boxed_get(self, PyGedaColorObject);

    switch (pos) {
    case 0: val = rgb->r; break;
    case 1: val = rgb->g; break;
    case 2: val = rgb->b; break;
    case 3: val = rgb->a; break;
    default:
        fprintf(stderr, "%s: unhandled case <%d>\n",__func__, pos);
        return NULL;
    }

    return PyInt_FromLong(ROUND(CLAMP(val, 0.0, 1.0) * 255.0));
}

static int rgb_setitem(PyObject *self, Py_ssize_t pos, PyObject *value)
{
    if (pos < 0)
        pos += 4;

    if (pos < 0 || pos >= 4) {
     PyErr_SetString(PyExc_IndexError, "index out of range");
     return -1;
    }

    switch (pos) {
    case 0: return rgb_set_r(self, value, NULL);
    case 1: return rgb_set_g(self, value, NULL);
    case 2: return rgb_set_b(self, value, NULL);
    case 3: return rgb_set_a(self, value, NULL);
    default:
        fprintf(stderr, "%s: unhandled case <%d>\n",__func__, pos);
        return -1;
    }
}

static PyObject *rgb_slice(PyObject *self, Py_ssize_t start, Py_ssize_t end)
{
    PyTupleObject *ret;
    Py_ssize_t i;

    if (start < 0)
     start = 0;
    if (end > 4)
     end = 4;
    if (end < start)
     end = start;

    ret = (PyTupleObject *)PyTuple_New(end - start);
    if (ret == NULL)
     return NULL;

    for (i = start; i < end; i++)
     PyTuple_SET_ITEM(ret, i - start, rgb_getitem(self, i));

    return (PyObject *)ret;
}

static PySequenceMethods rgb_as_sequence = {
    rgb_length,
    (binaryfunc)0,
    0,
    rgb_getitem,
    rgb_slice,
    rgb_setitem,
    0,
    (objobjproc)0,
};

static PyObject *rgb_subscript(PyObject *self, PyObject *item)
{
  if (PyInt_Check(item)) {
    long i = PyInt_AS_LONG(item);
    return rgb_getitem(self, i);
  }
  else if (PyLong_Check(item)) {
    long i = PyLong_AsLong(item);
    if (i == -1 && PyErr_Occurred())
      return NULL;
    return rgb_getitem(self, i);
  }
  else if (PySlice_Check(item)) {

    Py_ssize_t start, stop, step, slicelength, cur, i;

    if (PySlice_GetIndicesEx((PySliceObject*)item, 4,
      &start, &stop, &step, &slicelength) < 0)
      return NULL;

    if (slicelength <= 0) {
      return PyTuple_New(0);
    }
    else {

      PyObject *ret = PyTuple_New(slicelength);

      if (!ret)
        return NULL;

      for (cur = start, i = 0; i < slicelength; cur += step, i++)
        PyTuple_SET_ITEM(ret, i, rgb_getitem(self, cur));

      return ret;
    }
  }
  else if (PyString_Check(item)) {
    char *s = PyString_AsString(item);

    if (g_ascii_strcasecmp(s, "r") == 0 ||
      g_ascii_strcasecmp(s, "red") == 0)
      return rgb_get_r(self, NULL);
    else if (g_ascii_strcasecmp(s, "g")  == 0 ||
      g_ascii_strcasecmp(s, "green") == 0)
      return rgb_get_g(self, NULL);
    else if (g_ascii_strcasecmp(s, "b")  == 0 ||
      g_ascii_strcasecmp(s, "blue") == 0)
      return rgb_get_b(self, NULL);
    else if (g_ascii_strcasecmp(s, "a")  == 0 ||
      g_ascii_strcasecmp(s, "alpha") == 0)
      return rgb_get_a(self, NULL);
    else {
      PyErr_SetObject(PyExc_KeyError, item);
      return NULL;
    }
  }
  else {
    PyErr_SetString(PyExc_TypeError,
                    "indices must be integers");
    return NULL;
  }
}

static PyMappingMethods rgb_as_mapping = {
    rgb_length,
    (binaryfunc)rgb_subscript,
    (objobjargproc)0
};

static long rgb_hash(PyObject *self)
{
    long ret = -1;

    PyObject *temp = rgb_getstate(self);
    if (temp != NULL) {
     ret = PyObject_Hash(temp);
     Py_DECREF(temp);
    }

    return ret;
}

static PyObject *rgb_richcompare(PyObject *self, PyObject *other, int op)
{
    PyGedaColorObject *c1, *c2;
    PyObject *ret;

    if (!PyGedaColorObject_check(other)) {
     PyErr_Format(PyExc_TypeError,
                  "can't compare %s to %s",
                  self->ob_type->tp_name, other->ob_type->tp_name);
     return NULL;
    }

    if (op != Py_EQ && op != Py_NE) {
     PyErr_SetString(PyExc_TypeError,
                   "can't compare color values using <, <=, >, >=");
     return NULL;
    }

    c1 = pyg_boxed_get(self, PyGedaColorObject);
    c2 = pyg_boxed_get(other, PyGedaColorObject);

    if ((c1->r == c2->r && c1->g == c2->g && c1->b == c2->b && c1->a == c2->a) == (op == Py_EQ))
     ret = Py_True;
    else
     ret = Py_False;

    Py_INCREF(ret);

    return ret;
}

static PyObject *rgb_pretty_print(PyObject *self, bool inexact)
{
    PyGedaColorObject *rgb;
    PyObject *ret = NULL;
    PyObject *r_f = NULL, *g_f = NULL, *b_f = NULL, *a_f = NULL;
    PyObject *r   = NULL, *g   = NULL, *b   = NULL, *a   = NULL;
    reprfunc repr;
    const char *prefix;

    if (inexact) {
     repr = PyObject_Str;
     prefix = "RGB ";
    }
    else {
     repr = PyObject_Repr;
     prefix = self->ob_type->tp_name;
    }

    rgb = pyg_boxed_get(self, PyGedaColorObject);

    if ((r_f = PyFloat_FromDouble(rgb->r)) == NULL) goto cleanup;
    if ((g_f = PyFloat_FromDouble(rgb->g)) == NULL) goto cleanup;
    if ((b_f = PyFloat_FromDouble(rgb->b)) == NULL) goto cleanup;
    if ((a_f = PyFloat_FromDouble(rgb->a)) == NULL) goto cleanup;

    if ((r = repr(r_f)) == NULL) goto cleanup;
    if ((g = repr(g_f)) == NULL) goto cleanup;
    if ((b = repr(b_f)) == NULL) goto cleanup;
    if ((a = repr(a_f)) == NULL) goto cleanup;

    ret = PyString_FromFormat("%s(%s, %s, %s, %s)",
                         prefix,
                         PyString_AsString(r),
                         PyString_AsString(g),
                         PyString_AsString(b),
                         PyString_AsString(a));

cleanup:
    Py_XDECREF(r); Py_XDECREF(g); Py_XDECREF(b); Py_XDECREF(a);
    Py_XDECREF(r_f); Py_XDECREF(g_f); Py_XDECREF(b_f); Py_XDECREF(a_f);

    return ret;
}

static PyObject *rgb_repr(PyObject *self)
{
    return rgb_pretty_print(self, FALSE);
}

static PyObject *rgb_str(PyObject *self)
{
    return rgb_pretty_print(self, TRUE);
}

PyTypeObject PyPyGedaColorObject_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                             /* ob_size */
    "geda.color",                  /* tp_name */
    sizeof(PyGedaCircleObject),    /* tp_basicsize */
    0,                             /* tp_itemsize */
    (destructor)0,                 /* tp_dealloc */
    (printfunc)0,                  /* tp_print */
    (getattrfunc)0,                /* tp_getattr */
    (setattrfunc)0,                /* tp_setattr */
    (cmpfunc)0,                    /* tp_compare */
    (reprfunc)rgb_repr,            /* tp_repr */
    (PyNumberMethods*)0,           /* tp_as_number */
    &rgb_as_sequence,              /* tp_as_sequence */
    &rgb_as_mapping,               /* tp_as_mapping */
    (hashfunc)rgb_hash,            /* tp_hash */
    (ternaryfunc)0,                /* tp_call */
    (reprfunc)rgb_str,             /* tp_str */
    (getattrofunc)0,               /* tp_getattro */
    (setattrofunc)0,               /* tp_setattro */
    (PyBufferProcs*)0,             /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,            /* tp_flags */
    NULL,                          /*! \todo Documentation string */
    (traverseproc)0,               /* tp_traverse */
    (inquiry)0,                    /* tp_clear */
    (richcmpfunc)rgb_richcompare,  /* tp_richcompare */
    0,                             /* tp_weaklistoffset */
    (getiterfunc)0,                /* tp_iter */
    (iternextfunc)0,               /* tp_iternext */
    rgb_methods,                   /* tp_methods */
    0,                             /* tp_members */
    rgb_getsets,                   /* tp_getset */
    NULL,                          /* tp_base */
    NULL,                          /* tp_dict */
    (descrgetfunc)0,               /* tp_descr_get */
    (descrsetfunc)0,               /* tp_descr_set */
    0,                             /* tp_dictoffset */
    (initproc)rgb_init,            /* tp_init */
    (allocfunc)0,                  /* tp_alloc */
    (newfunc)0,                    /* tp_new */
    (freefunc)0,                   /* tp_free */
    (inquiry)0                     /* tp_is_gc */
};

int
PyGedaColorObject_from_pyobject(PyObject *object, PyGedaColorObject *color)
{
    g_return_val_if_fail(color != NULL, FALSE);

    if (PyGedaColorObject_check(object)) {
        *color = *pyg_boxed_get(object, PyGedaColorObject);
        return 1;
    }
    else if (PyString_Check(object)) {
        if (gimp_rgb_parse_css (color, PyString_AsString(object), -1)) {
            return 1;
        }
        else {
            PyErr_SetString(PyExc_TypeError, "unable to parse color string");
            return 0;
        }
    }
    else if (PySequence_Check(object)) {
        PyObject *r, *g, *b, *a = NULL;

        if (!PyArg_ParseTuple(object, "OOO|O", &r, &g, &b, &a))
            return 0;

#define SET_MEMBER(m)     G_STMT_START {                         \
    if (PyInt_Check(m))                                      \
        color->m = (double) PyInt_AS_LONG(m) / 255.0;             \
    else if (PyFloat_Check(m))                              \
        color->m = PyFloat_AS_DOUBLE(m);                   \
    else {                                            \
     PyErr_SetString(PyExc_TypeError,                   \
                   #m " must be an int or a float");     \
     return 0;                                      \
    }                                                  \
} G_STMT_END

        SET_MEMBER(r);
        SET_MEMBER(g);
        SET_MEMBER(b);

        if (a)
            SET_MEMBER(a);
        else
            color->a = 1.0;

        gimp_rgb_clamp(color);

        return 1;
    }

    PyErr_SetString(PyExc_TypeError, "could not convert to PyGedaColorObject");
    return 0;
}
