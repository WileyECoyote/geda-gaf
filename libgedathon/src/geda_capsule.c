/* C source                                         -*- geda_capsule.c -*- */
/*!
 * \file geda_capsule.c
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

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>

#include <geda_py_struct.h>
#include <geda_capsule.h>

#include "../include/gettext.h"


/** \defgroup Python_API_Encapsulation Python API Encapsulation Functions
 *  @{
 */

/*!
 * \brief Retrieve Pointer to contents of a GedaCapsule
 * \par Function Description
 *  Returns a pointer to the containing GedaObject.
 */
void *GedaCapsule_GetPointer(PyObject *obj)
{
  GedaCapsule *capsule = (GedaCapsule*)obj;
  return capsule->object;
}

/*!
 * \brief Retrieve Pointer to Encapsulated Object Name
 * \par Function Description
 *  Returns a pointer to the name string, the string belongs to
 *  the GedaObject and must not be freed or modified.
 */
const char *GedaCapsule_GetName(PyObject *obj)
{
  GedaCapsule *capsule = (GedaCapsule*)obj;
  return capsule->name;
}

/*!
 * \brief Retrieve the Name of an encapsulated Object
 * \par Function Description
 *  Returnd a string containing a printable representation the
 *  containing object.
 */
static PyObject *GedaCapsule_repr(PyObject *obj)
{
  GedaCapsule *capsule = (GedaCapsule*)obj;
  return PyString_FromString(capsule->name);
}

static PyMemberDef GedaCapsule_members[] = {
  {"sid",    T_INT,    offsetof(GedaCapsule, sid),    RO, "Geda object sequence identifier"},
  {"type",   T_INT,    offsetof(GedaCapsule, type),   RO, "Geda object type identifier"},
  {"name",   T_STRING, offsetof(GedaCapsule, name),   RO, "Geda object name"},
  {"object", T_ULONG,  offsetof(GedaCapsule, object), RO, "Pointer to Geda object"},
  {NULL}  /* Sentinel */
};

/* ------------------------------ Begin Methods ---------------------------- */

/* -------------------------- Begin Type Definition ------------------------ */

static PyTypeObject GedaCapsuleType = {
    PyObject_HEAD_INIT(NULL)
    0,                              /*ob_size,        not used, historical artifact for backward compatibility */
    "geda.GedaCapsule",             /*tp_name,        default textual representation our objects, used in some error messages*/
    sizeof(GedaCapsule),            /*tp_basicsize,   memory to allocate for this object */
    0,                              /*tp_itemsize*/
    (destructor)0,                  /*tp_dealloc*/
    (printfunc)0,                   /*tp_print*/
    0,                              /*tp_getattr*/
    0,                              /*tp_setattr*/
    0,                              /*tp_compare*/
    (reprfunc)GedaCapsule_repr,     /*tp_repr*/
    0,                              /*tp_as_number*/
    0,                              /*tp_as_sequence*/
    0,                              /*tp_as_mapping*/
    0,                              /*tp_hash */
    0,                              /*tp_call*/
    0,                              /*tp_str*/
    0,                              /*tp_getattro*/
    0,                              /*tp_setattro*/
    0,                              /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT |
    Py_TPFLAGS_BASETYPE,            /*tp_flags*/
    "Geda Capsule object",          /* tp_doc */
    0,                              /* tp_traverse */
    0,                              /* tp_clear */
    0,                              /* tp_richcompare */
    0,                              /* tp_weaklistoffset */
    0,                              /* tp_iter */
    0,                              /* tp_iternext */
    0,                              /* tp_methods */
    GedaCapsule_members,            /* tp_members */
};

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PyTypeObject *GedaCapsuleClass(void)
{
  return &GedaCapsuleType;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
PyObject *GedaCapsule_New(void *obj)
{
  GedaCapsule *capsule;
  GedaObject  *object = (GedaObject*) obj;

  if(!GEDA_IS_OBJECT(object)) {
    const char *msg = _("called with invalid pointer");
    PyErr_Format(PyExc_ValueError, "GedaCapsule_New %s", msg);
    return NULL;
  }

  capsule = PyObject_NEW(GedaCapsule, &GedaCapsuleType);

  if (capsule == NULL) {
    return NULL;
  }

  capsule->sid    = object->sid;
  capsule->type   = object->type;
  capsule->name   = object->name;
  capsule->object = object;

#if DEBUG

  int size = GedaCapsuleType.tp_basicsize;
  fprintf(stderr, "GedaCapsule_New: capsule for <%s> capsule address=%p, size=%d, type=%d, ",
          object->name, capsule, size, capsule->type);
  fprintf(stderr, "gobject address=%p\n", object);

#endif

  return (PyObject *)capsule;
}

/*!
 * \brief Initialize the GedaCapsule module
 * \par Function Description
 *  Creates the GedaCapsule type and adds the type to the module.
 */
PyMODINIT_FUNC initGedaCapsule(void)
{
  PyObject *capsule_module;

  if (PyType_Ready(&GedaCapsuleType) < 0)
    return;

  capsule_module = Py_InitModule3("GedaCapsule", NULL, _("Creates a GedaCapsule object type."));

  if (capsule_module == NULL)
    return;

  Py_INCREF(&GedaCapsuleType);
  PyModule_AddObject(capsule_module, "GedaCapsule", (PyObject*)&GedaCapsuleType);
}

/** @} END Group Python_API_Encapsulation */
