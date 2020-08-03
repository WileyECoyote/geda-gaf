/* C source                                          -*- libgedathon.c -*- */
/*!
 * \file libgedathon.c
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

/* Library Options and Administration
 *
 * Page Level Properties and Operations
 */

#include <config.h>

#include <geda_py_compat.h>

#include <libgen.h>        /* dirname */
#include <dirent.h>
#include <errno.h>

#include <gdk-pixbuf/gdk-pixbuf.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <libgedacolor.h>
#include <geda/geda_stat.h>

#if ENABLE_NLS
#  ifdef HAVE_LOCALE_H
#    include <locale.h>
#  endif
#endif

#include "../include/gettext.h"

/* Only loading header here to get METHOD_COUNT because it is needed in
 * libgedathon, (so count doesn't has to be passed to the init function */
#include "../include/geda_module.h"

#include "../include/geda_py_struct.h"
#include "../include/geda_py_page.h"
#include "../include/geda_py_object.h"
#include "../include/geda_py_struct.h"
#include "../include/geda_capsule.h"
#include "../include/libgedathon.h"


int PyGeda_update_object(GedaObject *object, PyGedaObject *py_object);
int PyGeda_update_butes(GedaObject *object, PyGedaObject *py_object);

/* Set for reload in order to fill-in  the function table */
#define METHOD(token, arg) { #token, (GedaLibFunc) PyGeda_##token, 0 },

static GedaToplevel *toplevel;
static GList        *floating_objects;   /* List objects not on a page */

/* fill struct with pointer functions */
static PyGedaFunc FunctionTable[] = {
 { "error", NULL, 0},
 #include <geda_module.h>
};

/** \defgroup Python_API_Library_Internal Python API Internal Functions
 *  @{
 */

static inline void add_floating_object (GedaObject *new_object)
{
  if (GEDA_IS_OBJECT(new_object)) {
    floating_objects = g_list_append(floating_objects, new_object);
  }
}

static void destroy_all_floating_objects (void)
{
  GList *iter = g_list_first(floating_objects);

  while (iter != NULL) {

    GedaObject *object = (GedaObject*)iter->data;

    if (GEDA_IS_OBJECT(object)) {
      if (object->page == NULL) {
        geda_struct_object_release(object);
      }
    }
    NEXT(iter);
  }
  g_list_free(floating_objects);
  floating_objects = NULL;
}

GedaObject *get_floating_object (int sid)
{
  GList *iter = g_list_first(floating_objects);

  while (iter != NULL) {

    GedaObject *object = iter->data;

    if (object->sid == sid) {
      return object;
    }
    NEXT(iter);
  }
  return NULL;
}

GedaObject *retrieve_floating_object (int sid)
{
  GList *iter = g_list_first(floating_objects);

  while (iter != NULL) {

    GedaObject *object = iter->data;

    if (object->sid == sid) {
      floating_objects = g_list_remove(floating_objects, object);
      return object;
    }
    NEXT(iter);
  }
  return NULL;
}

/*!
 * \brief Append an Object to a PyList (of GedaCapules)
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function appends a GedaCapsule for the given PyGedaObject to an
 *  a PyList Object. If the first argument is NULL then a new PyList
 *  object is created and returned.
 *
 * \sa PyGeda_glist_2_pylist
 *
 * \param [in] pylist   Pointer PyList (of GedaCapsules)
 * \param [in] object   Pointer to a Libgeda Object
 *
 * \return [out] PyObject Pointer to PyList
 */
static PyObject*
PyGeda_append_2_pylist(PyObject *pylist, GedaObject *object) {

  PyObject *list;

  if (pylist == NULL) {
    list = PyList_New(0);
  }
  else {
    list = pylist;
  }

  if (GEDA_IS_OBJECT(object)) {
    PyObject *capsule = GedaCapsule_New(object);
    PyList_Append(list, capsule);
  }
  else {
    list = NULL;
  }
  return list;
}

/*!
 * \brief Create a PyList of GedaCapules
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a new PyList object and adds a GedaCapsule
 *  for each PyGedaObject in the given glist. GedaCapsules are similar
 *  to a PyCapsule, but are not derived from PyCapsule_Type, in that
 *  they contain a pointer to something, in this case the pointer with
 *  in the given glist, i.e. pointers to Libgeda Objects.
 *
 * \sa PyGeda_append_2_pylist
 *
 * \param [in] object_list Pointer to a GList of Libgeda Objects
 *
 * \return [out] PyObject  Pointer to new PyList
 */
static PyObject*
PyGeda_glist_2_pylist(GList *object_list)
{
  GList    *ptr;
  PyObject *objects;

  objects = PyList_New(0);
  ptr     = g_list_first(object_list);

  while (ptr != NULL) {

    PyObject *capsule = GedaCapsule_New(GEDA_OBJECT(ptr->data));

    if (capsule) {
      PyList_Append(objects, capsule);
    }
    else { /* Was error, should have been reported by encapsulator */
      break;
    }
    NEXT(ptr);
  }

  return objects;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaArcObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaBusObject initializer in order to create a
 *  Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaArcObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_arc_object_data(GedaObject *object)
{
  PyObject *data;

  char *name      = object->name;
  int   type      = object->type;
  int   pid       = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid       = object->sid;
  int   lock      = !object->selectable;

  int x           = object->arc->x;
  int y           = object->arc->y;
  int radius      = object->arc->radius;
  int start_angle = object->arc->start_angle;
  int arc_sweep   = object->arc->arc_sweep;

  int line_end    = object->line_options->line_end;
  int line_type   = object->line_options->line_type;
  int line_width  = object->line_options->line_width;
  int line_space  = object->line_options->line_space;
  int line_length = object->line_options->line_length;

  data = Py_BuildValue("siiiiiiiiiiiiii", name, type, pid, sid, lock,
                       x, y, radius, start_angle, arc_sweep,
                       line_end, line_type, line_width, line_space, line_length);
  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaBoxObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaBoxObject initializer in order to create a
 *  Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaBoxObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_box_object_data(GedaObject *object)
{
  PyObject *data;

  char *name      = object->name;
  int   type      = object->type;
  int   pid       = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid       = object->sid;
  int   lock      = !object->selectable;

  int   upper_x   = object->box->upper_x;
  int   upper_y   = object->box->upper_y;
  int   lower_x   = object->box->lower_x;
  int   lower_y   = object->box->lower_y;

  int fill_type   = object->fill_options->fill_type;
  int fill_width  = object->fill_options->fill_width;
  int fill_angle1 = object->fill_options->fill_angle1;
  int fill_pitch1 = object->fill_options->fill_pitch1;
  int fill_angle2 = object->fill_options->fill_angle2;
  int fill_pitch2 = object->fill_options->fill_pitch2;

  int line_end    = object->line_options->line_end;
  int line_type   = object->line_options->line_type;
  int line_width  = object->line_options->line_width;
  int line_space  = object->line_options->line_space;
  int line_length = object->line_options->line_length;

  data = Py_BuildValue("siiiiiiiiiiiiiiiiiii",  name, type, pid, sid, lock,
                       upper_x, upper_y, lower_x, lower_y,
                       fill_type, fill_width, fill_angle1, fill_pitch1,
                       fill_angle2, fill_pitch2,
                       line_end, line_type, line_width, line_space, line_length);

  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaBusObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaBusObject initializer in order to create a
 *  Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaBusObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_bus_object_data(GedaObject *object)
{
  PyObject *data;

  char *name        = object->name;
  int   type        = object->type;
  int   pid         = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid         = object->sid;
  int   lock        = !object->selectable;

  int   x1          = object->line->x[0];
  int   y1          = object->line->y[0];
  int   x2          = object->line->x[1];
  int   y2          = object->line->y[1];

  char *bus_name    = object->bus->bus_name;
  int   direction   = object->bus->ripper_direction;
  int   line_width  = object->line_options->line_width;

  object->bus->ripper_direction = direction;

  if (!bus_name) {
    bus_name = "";
  }

  data = Py_BuildValue("siiiiiiiisii", name, type, pid, sid, lock,
                       x1, y1, x2, y2, bus_name, direction, line_width);

  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaCircleObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaCircleObject initializer in order to create
 *  a Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaCircleObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_circle_object_data(GedaObject *object)
{
  PyObject *data;

  char *name      = object->name;
  int   type      = object->type;
  int   pid       = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid       = object->sid;
  int   lock      = !object->selectable;

  int   x         = object->circle->center_x;
  int   y         = object->circle->center_y;
  int   radius    = object->circle->radius;

  int fill_type   = object->fill_options->fill_type;
  int fill_width  = object->fill_options->fill_width;
  int fill_angle1 = object->fill_options->fill_angle1;
  int fill_pitch1 = object->fill_options->fill_pitch1;
  int fill_angle2 = object->fill_options->fill_angle2;
  int fill_pitch2 = object->fill_options->fill_pitch2;

  int line_end    = object->line_options->line_end;
  int line_type   = object->line_options->line_type;
  int line_width  = object->line_options->line_width;
  int line_space  = object->line_options->line_space;
  int line_length = object->line_options->line_length;

  data = Py_BuildValue("siiiiiiiiiiiiiiiiii", name, type, pid, sid, lock,
                       x, y, radius,
                       fill_type, fill_width, fill_angle1, fill_pitch1,
                       fill_angle2, fill_pitch2,
                       line_end, line_type, line_width, line_space, line_length);

  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaComplexObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaComplexObject initializer in order to create
 *  a Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaComplexObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_complex_object_data(GedaObject *object)
{
  PyObject *data;
  PyObject *py_attributes;
  PyObject *py_pin_objs;
  PyObject *py_prim_objs;

  char *name      = object->name;

  int   type      = object->type;
  int   pid       = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid       = object->sid;
  int   lock      = !object->selectable;

  char *basename  = object->complex->filename;
  int   embedded  = object->complex->is_embedded;
  int   x         = object->complex->x;
  int   y         = object->complex->y;
  int   angle     = object->complex->angle;
  int   mirror    = object->complex->mirror;

  if (object->complex->pin_objs) {
    py_pin_objs   = PyGeda_glist_2_pylist(object->complex->pin_objs);
  }
  else {
    py_pin_objs   = PyList_New(0);
  }

  if (object->complex->prim_objs) {
    py_prim_objs   = PyGeda_glist_2_pylist(object->complex->prim_objs);
  }
  else {
    py_prim_objs   = PyList_New(0);
  }

  if (object->attribs) {
    py_attributes   = PyGeda_glist_2_pylist(object->attribs);
  }
  else {
    py_attributes   = PyList_New(0);
  }

  data = Py_BuildValue("siiiisiiiiiOOO",  name, type, pid, sid, lock,
                        basename, x, y, embedded, angle, mirror,
                        py_pin_objs, py_prim_objs, py_attributes);
#if DEBUG
  fprintf(stderr, "gcod exit: name=%s, pid=%d, sid=%d, at (%d,%d) basename=%s)\n", name, pid, sid, x, y, basename);
#endif

  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaLineObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaLineObject initializer in order to create a
 *  Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaLineObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_line_object_data(GedaObject *object)
{
  PyObject *data;

  char *name      = object->name;
  int   type      = object->type;
  int   pid       = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid       = object->sid;
  int   lock      = !object->selectable;

  int   x1        = object->line->x[0];
  int   y1        = object->line->y[0];
  int   x2        = object->line->x[1];
  int   y2        = object->line->y[1];

  int line_end    = object->line_options->line_end;
  int line_type   = object->line_options->line_type;
  int line_width  = object->line_options->line_width;
  int line_space  = object->line_options->line_space;
  int line_length = object->line_options->line_length;

  data = Py_BuildValue("siiiiiiiiiiii",  name, type, pid, sid, lock,
                        x1, y1, x2, y2,
                        line_end, line_type, line_width, line_space, line_length);
  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaNetObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaNetObject initializer in order to create a
 *  Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaNetObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_net_object_data(GedaObject *object)
{
  PyObject *data;

  char *name      = object->name;
  int   type      = object->type;
  int   pid       = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid       = object->sid;
  int   lock      = !object->selectable;

  int   nid       = sid;

  int   x1        = object->line->x[0];
  int   y1        = object->line->y[0];
  int   x2        = object->line->x[1];
  int   y2        = object->line->y[1];

  int   width     = object->line_options->line_width;
  char *net_name  = object->net->net_name;

  if (!net_name)
    net_name = "";

#if DEBUG
  fprintf(stderr, "gpod: name=%s, pid=%d, sid=%d, net_name=<%s> nid=%d from (%d,%d) to (%d,%d)\n",
                         name, pid, sid, net_name, nid, x1, y1, x2, y2);
#endif

  data = Py_BuildValue("siiiiiiiiiis",  name, type, pid, sid, lock,
                        nid, x1, y1, x2, y2, width, net_name);
  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaPathObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaPathObject initializer in order to create a
 *  Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaPathObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_path_object_data(GedaObject *object)
{
  PyObject *data;
  PyObject *pyobject;
  PyObject *py_sections;

  char *name         = object->name;
  int   type         = object->type;
  int   pid          = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid          = object->sid;
  int   lock         = !object->selectable;

  char *path_string  = geda_struct_path_string_from_path(object->path);
  int   num_sections = object->path->num_sections;
  int   max_sections = object->path->num_sections_max;

  py_sections = PyList_New(0);

  pyobject = Py_BuildValue("i",  object->path->sections->code);
  PyList_Append(py_sections, pyobject);

  pyobject = Py_BuildValue("ii", object->path->sections->x1,
                                 object->path->sections->y1);
  PyList_Append(py_sections, pyobject);

  pyobject = Py_BuildValue("ii", object->path->sections->x2,
                                 object->path->sections->y2);
  PyList_Append(py_sections, pyobject);

  pyobject = Py_BuildValue("ii", object->path->sections->x3,
                                 object->path->sections->y3);
  PyList_Append(py_sections, pyobject);

  int fill_type      = object->fill_options->fill_type;
  int fill_width     = object->fill_options->fill_width;
  int fill_angle1    = object->fill_options->fill_angle1;
  int fill_pitch1    = object->fill_options->fill_pitch1;
  int fill_angle2    = object->fill_options->fill_angle2;
  int fill_pitch2    = object->fill_options->fill_pitch2;

  int line_end       = object->line_options->line_end;
  int line_type      = object->line_options->line_type;
  int line_width     = object->line_options->line_width;
  int line_space     = object->line_options->line_space;
  int line_length    = object->line_options->line_length;

  data = Py_BuildValue("siiiisiiOiiiiiiiiiii",  name, type, pid, sid, lock,
                       path_string, num_sections, max_sections, py_sections,
                       fill_type, fill_width, fill_angle1, fill_pitch1,
                       fill_angle2, fill_pitch2,
                       line_end, line_type, line_width, line_space, line_length);
  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaPictureObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaPictureObject initializer in order to create
 *  a Python object corresponding to the given  libgeda object. The
 *  parameters MUST be in the correct order or the Python object will
 *  not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaPictureObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_picture_object_data(GedaObject *object)
{
  PyObject  *data;
  PyObject  *py_pixbuf;
  GdkPixbuf *pixbuf;

  char  *name        = object->name;
  int    type        = object->type;
  int    pid         = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int    sid         = object->sid;
  int    lock        = !object->selectable;

  char  *filepath    = object->name;
         pixbuf      = object->picture->pixbuf;
  int    file_length = object->picture->file_length; /* unsigned int */
  double ratio       = object->picture->ratio;

  int    angle       = object->picture->angle;
  bool   mirrored    = object->picture->mirrored;
  bool   embedded    = object->picture->is_embedded;

  py_pixbuf = Py_BuildValue("B", pixbuf);

  int    upper_x    = object->picture->upper_x;
  int    upper_y    = object->picture->upper_y;
  int    lower_x    = object->picture->lower_x;
  int    lower_y    = object->picture->lower_y;

  data = Py_BuildValue("siiiisifiiiiiiiO", name, type, pid, sid, lock,
                        filepath, file_length, ratio,
                        upper_x, upper_y, lower_x, lower_y,
                        angle, mirrored, embedded, py_pixbuf);
  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaPinObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaPinObject initializer in order to create a
 *  Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaPinObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_pin_object_data(GedaObject *object)
{
  PyObject *data;

  char *name       = object->name;
  int   type       = object->type;
  int   pid        = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int   sid        = object->sid;
  int   lock       = !object->selectable;

  int   cid        = geda_object_get_parent_id(object);
  int   nid        = sid;

  char *elect_type = object->pin->electrical;
  char *mech_type  = object->pin->mechanical;
  char *label      = object->pin->label;

  char *number     = object->pin->number;
  int   sequence   = object->pin->sequence;
  int   whichend   = object->pin->whichend;

  int   x1         = object->line->x[0];
  int   y1         = object->line->y[0];
  int   x2         = object->line->x[1];
  int   y2         = object->line->y[1];

  PIN_ELECT e_type = object->pin->elect_type;
  PIN_MECH  m_type = object->pin->mech_type;
  PIN_NODE  n_type = object->pin->node_type;

  int line_width   = object->line_options->line_width;

  /* Check and handle missing attributes */
  if (!elect_type)
    elect_type = "";

  if (!mech_type)
    mech_type = "";

  if (!label)
    label = "";

  if (!number)
    number = "";

  data = Py_BuildValue("siiiiiissssiiiiiiiiii", name, type, pid, sid, lock,
                       cid, nid, elect_type, mech_type, label,
                       number, sequence, whichend, x1, y1, x2, y2,
                       e_type, m_type, n_type, line_width);

#if DEBUG
  fprintf(stderr, "gpod: name=%s, pid=%d, sid=%d, cid=%d, nid=%d from (%d,%d) to (%d,%d)\n", name, pid, sid, cid, nid, x1, y1, x2, y2);
  fprintf(stderr, "      label=%s, number=%s, sequence=%d, whichend=%d, line_width=%d\n", label, number, sequence, whichend, line_width);
  fprintf(stderr, "      e_type (%d=%s), m_type (%d=%s), n_type=%d,\n", e_type, elect_type, m_type, mech_type, n_type);
#endif

  return data;
}

/*!
 * \brief Get a data object to create a Python Geda PyGedaTextObject
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function creates a PyObject with the parameters used to
 *  pass to the PyGedaTextObject initializer in order to create a
 *  Python object corresponding to the given libgeda object. The
 *  parameters MUST be in the correct order or the Python object
 *  will not be created correctly, if at all.
 *
 * \param [in] object   Pointer to a Libgeda PyGedaTextObject
 *
 * \return [out] PyObject Pointer to parameter data object
 */
static PyObject*
get_text_object_data(GedaObject *object)
{
  PyObject *data;
  char     *name = object->name;
  int       type = object->type;
  int        pid = GEDA_IS_PAGE(object->page) ? object->page->pid : -1;
  int        sid = object->sid;
  char      *str = object->text->string;
  char     *dstr = object->text->disp_string;
  int       lock = !object->selectable;
  int        cid = geda_object_get_parent_id(object);
  int          x = object->text->x;
  int          y = object->text->y;
  int       size = object->text->size;
  int        vis = object->visibility;
  int       show = object->show_name_value;
  int      align = object->text->alignment;
  int      angle = object->text->angle;

  /* if text was not integral with complex check if attached */
  if (cid < 0) cid = geda_object_get_attached_parent_id(object);

  data = Py_BuildValue("siiiiissiiiiiii", name, type, pid, sid, lock, cid,
                       str, dstr, x, y, size, align, angle, vis, show);
  return data;
}

/*!
 * \brief Translate Color Objects
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This is a temporary function to translate PyPyGedaColorObject
 *  to Libgeda color index.
 *
 * \param [in] py_color      PyObject Color object
 * \param [in] default_color Integer the default color index
 *
 * \return [out] integer A Color index
 */
static int translate_color(PyObject *py_color, int default_color)
{
  int result = -1;

  if (py_color == NULL) {
    result = default_color;
  }
  else if (PyInt_Check(py_color)) {
    result = PyInt_AsLong(py_color);
  }
  else {
    result = default_color;
  }

  return result;
}

int alphasort (const struct dirent **a, const struct dirent **b)
{
  return strcoll ((*a)->d_name, (*b)->d_name);
}

typedef int (*filter_fp)(const char *path, const struct dirent *, void *);

int scan_dir(const char *dirname, struct dirent ***namelist,  filter_fp filter, void *baton)
{
    DIR *dirp = NULL;
    struct dirent **names = NULL;
    struct dirent *entry, *d;
    int names_len = 32;
    int results_len = 0;

    dirp = opendir(dirname);
    if (dirp == NULL) {
        goto fail;
    }

    names = malloc(sizeof(struct dirent *) * names_len);
    if (names == NULL) {
        goto fail;
    }

    while ((entry = readdir(dirp)) != NULL) {

       if (filter) {
         if ((*filter)(dirname, entry, baton) == FALSE) {
            continue;
         }
       }

        if (results_len >= names_len) {
            struct dirent **tmp_names = names;
            names_len *= 2;
            names = realloc(names, sizeof(struct dirent *) * names_len);
            if (names == NULL) {
                free(tmp_names);
                goto fail;
            }
        }

        d = malloc(sizeof(struct dirent));

        if (d == NULL) {
            goto fail;
        }

        memcpy(d, entry, sizeof(struct dirent));

        names[results_len] = d;
        results_len++;
    }

    closedir(dirp);
    *namelist = names;
    return results_len;

fail:
    if (dirp) {
        closedir(dirp);
    }

    if (names != NULL) {
        int i;
        for (i = 0; i < results_len; i++) {
            free(names[i]);
        }
        free(names);
    }
    return -1;
}

/*!
 * \brief Setup the Python API Symbol Library
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function is called by the initializer to setup the s_clib module
 *  in Libgeda using the data path returned from geda_sys_data_path().
 */
static void setup_source_library (void)
{
  struct dirent **namelist;
  struct dirent **nestedlist;
  char           *data_path;
  int n;

  data_path = geda_strconcat(geda_sys_data_path(), DIR_SEPARATOR_S, "sym", NULL);


  n = scan_dir(data_path, &namelist, NULL, alphasort);

  if (n < 0)
    fprintf(stderr, "libgedathon <setup_source_library> bad geda_sys_data_path, %s\n",strerror (errno));
  else {

    int i;

    while (n--) {
      if ((g_ascii_strcasecmp (namelist[n]->d_name, ".")  != 0) &&
          (g_ascii_strcasecmp (namelist[n]->d_name, "..") != 0) &&
          (namelist[n]->d_type == DT_DIR))
      {
        const char *name;
              char *fullpath;

        name     = namelist[n]->d_name;
        fullpath = g_build_filename (data_path, name, NULL);

        geda_struct_clib_add_directory (fullpath,name);

        i = scan_dir (fullpath, &nestedlist, NULL, alphasort);

        if (i > 0) {
          while (i--) {
            if ((g_ascii_strcasecmp (nestedlist[i]->d_name, ".")  != 0) &&
                (g_ascii_strcasecmp (nestedlist[i]->d_name, "..") != 0) &&
                (nestedlist[i]->d_type == DT_DIR))
            {
              const char *nested;
                    char *nestedpath;

              nested     = nestedlist[i]->d_name;
              nestedpath = g_build_filename (fullpath, nested, NULL);

              geda_struct_clib_add_directory (nestedpath, nested);

              free(nestedpath);
              free(nestedlist[i]);
            }
          }
          free(nestedlist);
        }
        free(fullpath);
      }
      free(namelist[n]);
    }
    free(namelist);
  }
  GEDA_FREE(data_path);
}

/* local macro used in the next function */
#define apa_list toplevel->always_promote_attributes

/*!
 * \brief initialize the Pythong API Library System
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This function must be called after the library is opened in order to
 *  initialize memory within libgedathon and libgeda. This function creates
 *  a new toplevel Object and sets basic settings in the toplevel. The
 *  modules function table is loaded with addresses of the API functions.
 *  This function is the only function symbolically exported from the
 *  Library.
 *
 * \param [in] user_table a API_FunctionTable to be loaded with the
 *                        addresses of API functions in this module.
 */
void initialize( API_FunctionTable *user_table)
{
  int i;

  libgeda_init(0, NULL);

#ifdef ENABLE_NLS

  bindtextdomain (LIBGEDATHON_GETTEXT_DOMAIN, LOCALEDIR);
  bind_textdomain_codeset(LIBGEDATHON_GETTEXT_DOMAIN, "UTF-8");

#endif

  toplevel = geda_toplevel_new();
  toplevel->open_flags = 0;

  setup_source_library();

  if (toplevel->always_promote_attributes == NULL ) {

    const char *promote_list[] = {  /* always-promote-attributes */
    "footprint", "device", "value", "model-name", NULL};

    for ( i = 0; promote_list[i]; i++) {
     apa_list = g_list_prepend(apa_list, geda_strdup(promote_list[i]));
    }
  }

  floating_objects = NULL;

  /* make a copy of the function table for the module */
  for ( i = 0; i < METHOD_COUNT; i++) {
   PyGedaFunc method     = FunctionTable[i];
   user_table[i]->name   = method.name;
   user_table[i]->func   = method.func;
   user_table[i]->status = method.status;
  }

  initGedaCapsule();

 return;
}
#undef apa_list

/** @} END Group Python_API_Library_Internal */

/** \defgroup Python_API_Library  Python API Functions
 *  @{
 */

/*!
 * \brief Shutdown library API
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function is called when the library is closed in order to
 *  clean close any opened files and release all dynamically allocated
 *  memory. All floating objects are destroyed and then libgeda is
 *  finalized.
 */
void PyGeda_shutdown(void)
{
  GList *iter = g_list_first(toplevel->pages->glist);

  while (iter) {

    Page *page = iter->data;

    if (page && (GEDA_IS_PAGE(page))) {
      PyGeda_close_page(page->pid);
    }
    NEXT(iter);
  }

  destroy_all_floating_objects();
  libgeda_release();
  geda_toplevel_struct_release(toplevel);
  return;
}

/*!
 * \brief Append the Symbol Library Search Path
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function checks for the directory of the given path string
 *  and calls geda_struct_clib_add_directory to append the directory
 *  to the library search path so that symbol files within the folder
 *  will be made available to the drawing.
 *
 * \return [out] integer result
 */
int PyGeda_append_symbol_path( const char *path )
{
  char *directory;
  int result;

  directory = geda_utility_expand_env_variable (path);
  result    = 0;

  /* is valid path? */
  if (g_file_test (directory, G_FILE_TEST_IS_DIR)) {

    char *namestr = basename(directory);

    if (geda_file_get_is_path_absolute (directory)) {
      result = geda_struct_clib_add_directory (directory, namestr) != NULL;
    }
    else {

      char *cwd = g_get_current_dir ();
      char *temp;

      if (*directory == '.' && *directory + 1 == '/' )
        temp = g_build_filename (cwd, directory + 2, NULL);
      else
        temp = g_build_filename (cwd, directory, NULL);

      result = geda_struct_clib_add_directory (temp, namestr) != NULL;

      GEDA_FREE(temp);
      GEDA_FREE(cwd);
    }
  }
  return result;
}

/* Helper for PyGeda_declare_local_sym */
static char *advance2char(const char *string)
{
  const char *ptr = string;
  while ((*ptr != ASCII_NUL) && ((*ptr == SPACE) || (*ptr == ASCII_HT)))++ptr;
  return (char*)ptr;
}

/*!
 * \brief Declare Local Symbols
 * \ingroup Python_API_Library_Internal
 * \par Function Description
 *  This is a utility function to create a "gafrc" file in the given
 *  directory in order for a document to have locally defined symbols.
 *
 * \param [in] directory The directory to be used in "gafrc" file
 *
 * \return [out] integer result=TRUE if file was created, otherwise the
 *                       system error number is returned
 */
int PyGeda_declare_local_sym( const char *directory )
{
  const char *subdir;
  int result = 0;
  FILE *fp;

  if (directory)
    subdir = directory;
  else
    subdir = "sym";

  /* Check if the subdirectory exist */
  if (!g_file_test (subdir, G_FILE_TEST_IS_DIR)) {

    /* Attempt to create the subdirectory, does not set error result */
    if (geda_create_path(subdir, S_IRWXU | S_IRWXG | S_IRWXO)) {
      const char *msg = "Warning: could not create";
      fprintf(stderr, "%s \"%s\", %s\n", msg, subdir, strerror(errno));
    }
  }

  if (!g_file_test ("gafrc", G_FILE_TEST_EXISTS)) {

    /* The RC file does not exist so create */

    fp = fopen("gafrc", "w");

    if (fp == NULL) {
      result = errno;
    }
    else {
      fprintf(fp, "(component-library \"./%s\")\n", subdir);
      fclose(fp);
      result = TRUE;
    }
  }
  else { /* the RC file does exist so check contents */

    fp = fopen("gafrc", "r");

    if (fp) {

      char   *buff;
      char   *oline;
      bool    found;
      size_t  len;
      size_t  olen;
      ssize_t read;

      buff  = NULL;
      len   = 0;
      found = FALSE;
      oline = geda_sprintf("(component-library \"./%s\")\n", subdir);
      olen  = strlen(oline);

      /* Check if line already in file */
      while ((read = getline(&buff, &len, fp)) != -1) {

        char *ptr = advance2char(&buff[0]);

        if (read > 21) {
          if (strncmp(ptr, oline, olen) == 0) {
            found=TRUE;
            break;
          }
        }
      }

      if (!found) {

        /* The line was not found in the existing file so close
         * the file and reopen in append mode */

        fclose(fp);
        fp = fopen("gafrc", "a");

        if (fp) {

          /* Append line, extra new-line here is intentional */
          fprintf(fp, "%s\n", oline);
          result = TRUE;
        }
        else {
          const char *msg = "Warning: could not write to";
          fprintf(stderr, "%s \"%s\", %s\n", msg, subdir, strerror(errno));
        }
      }
      else {
        result = TRUE; /* because file with line already exist */
      }

      free(buff);
      free(oline);
      fclose(fp);
    }
    else {
      const char *msg = "Warning: could not read";
      fprintf(stderr, "%s \"%s\", %s\n", msg, subdir, strerror(errno));
    }
  }

  return result;
}

/*!
 * \brief Gets a list of all currently opened pages
 * \ingroup Python_API_Library
 * \par Function Description
 *  Returns a list with the page info for each currently opened
 *  page, if there are no pages opened then the list will be an
 *  empty PyList;
 *
 * \return [out] PyObject list of page names and ID's
 */
PyObject *PyGeda_get_pages( void )
{
  PyObject *pages;
  GList    *iter;

  pages = PyList_New(0);
  iter  = g_list_first(toplevel->pages->glist);

  while (iter) {

    Page *page = iter->data;

    if (page && (GEDA_IS_PAGE(page))) {
      PyObject *page_info;
      page_info = Py_BuildValue("si", page->filename, page->pid);
      PyList_Append(pages, page_info);
    }
    NEXT(iter);
  }
  return pages;
}

/*!
 * \brief Gets the current page in the TopLevel
 * \ingroup Python_API_Library
 * \par Function Description
 *  Returns info for the current page in \a toplevel or NULL if there
 *  is no current page;
 *
 * \return [in] data PyObject page name and pid or NULL
 */
PyObject *PyGeda_get_active_page( void )
{
  Page *page;
  PyObject *page_info;

  page = geda_struct_page_get_current(toplevel);

  if (page && (GEDA_IS_PAGE(page))) {
    page_info = Py_BuildValue("si", page->filename, page->pid);
  }
  else {
    page_info = NULL;
  }
  return page_info;
}

/*!
 * \brief Set the current page in the TopLevel
 * \ingroup Python_API_Library
 * \par Function Description
 *  Changes the current page in \a toplevel to the page \a page.
 *
 * \param [in] pid   Integer, The new current page
 *
 * \returns True on success, otherwise FALSE
 */
int PyGeda_set_active_page( int pid )
{
  Page *page;
  page = geda_toplevel_get_page_by_id(toplevel, pid);
  return geda_struct_page_set_current (toplevel, page);
}

/*!
 * \brief Get is Page Modified
 * \ingroup Python_API_Library
 * \par Function Description
 *  Retrieves page object assocated with \a pid and returns
 *  page->CHANGED;
 *
 * \param [in] pid     Integer, ID of Page being queried
 *
 * \returns True if page has been modifiied and not save, FALSE
 *          if the page has been saved, -1 if there was an error,
 *          such as an invalid page id.
 */
int PyGeda_is_page_modified (int pid)
{
  Page   *page   = NULL;
  int     status = -1;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  if (page) {
    status = geda_page_get_changed(page);
  }
  return status;
}

/*!
 * \brief Changes the current page in toplevel
 * \ingroup Python_API_Library
 * \par Function Description
 *  Calls LibGeda function geda_struct_page_goto to change the current
 *  page referenced by \a toplevel and changes the current working
 *  directory to the directory associated with the page.
 *
 * \param [in] pid     Integer, The Page to go to
 *
 * \returns True on success, otherwise FALSE
 */
int PyGeda_goto_page( int pid )
{
  Page *page;
  page = geda_toplevel_get_page_by_id(toplevel, pid);
  return geda_struct_page_goto (page);
}

/*!
 * \brief Open a Page
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function attempts to create new PyGedaPageObject. The object
 *  represents the Page referenced by the given filename only if the
 *  file exist and is readable when attempting to open and existing file.
 *  If the filename does not exist then an empty page Object is created.
 *
 * \param [in] filename String, the file name of the page to open
 *
 * \return [out] PyObject A composite Python object containing the filename
 *                        and the assigned pid
 */
PyObject *PyGeda_open_page( const char *filename )
{
  Page *page;
  char *ptr;
  char  strbuff[MAX_PATH];

  g_return_val_if_fail (toplevel != NULL, Py_BuildValue(""));

  /* Generate unique untitled filename if none was specified */
  char *generate_untitled(void) {

    char *str;
    char *untitled;

    /* Get untitled-name prior to looping */
    if (!toplevel->untitled_name) {
      untitled = "untitled";            /* Set to fall-back name */
    }
    else {
      untitled = toplevel->untitled_name;  /* Set to string from config */
    }

    inline void unique_untitled(void) {

      char *tmp;
      char  s_val[3];

      /* Get DIR in buffer */
      ptr = str = getcwd (&strbuff[0], MAX_PATH - 1);

      /* Append a seperator onto the end of DIR */
      while (*ptr != '\0') {
        ++ptr; /* advance to end of string */
      }

       *ptr = DIR_SEPARATOR;     /* add separator */
      ++ptr;                     /* advance over separator */
       *ptr = '\0';              /* Add new NULL */

      /* Append untitled-name */
      str = strcat (str, untitled);

      /* Converted and append an integer to the string */
      tmp = geda_string_int2str (++toplevel->num_untitled, &s_val[0], 10);

      str = strcat (str, tmp);

      /* Append our file extension */
      str = strcat (str, SCHEMATIC_FILE_DOT_SUFFIX);
    }

    memset(&strbuff[0], '\0', sizeof(strbuff));
    unique_untitled ();
    while (g_file_test (str, G_FILE_TEST_EXISTS)) unique_untitled ();
    return str;
  }

  /* Create an empty page with optional filename */
  inline Page *empty_page( const char *name ) {

    char *fname;

    fname = geda_utility_string_strdup ( name ? name : generate_untitled() );
    page  = geda_struct_page_new (toplevel, fname);

    geda_struct_page_goto (page);
    GEDA_FREE (fname);
    return page;
  }

  /* If filename string has no length, NULL the pointer */
  if (filename && !strlen(filename)) {
    filename = NULL;
  }

  if (filename == NULL) {
    page = empty_page(NULL); /* and were done */
    filename = page->filename;
  }
  else {

    Page *old_current = toplevel->page_current; /* save fallback point */

    /* Recover by switching back to Old or a create blank */
    inline void resolve_2_recover( const char *name ) {
      /* There was an error, try go back to old page */
      if (old_current != NULL ) {
        geda_struct_page_goto (old_current);
        page = NULL;
      }
      else { /* There was error and no previous page */
        page = empty_page(name);
      }
    }

    if (g_file_test (filename, G_FILE_TEST_EXISTS)) {

      if (geda_is_path_absolute (filename)) {
        /* An existing filename was passed, see if already loaded */
        page = geda_struct_page_search (toplevel, filename);
      }
      else {

        char *cwd;
        char *fullname;

        cwd = getcwd(0, 0);

        fullname = geda_strconcat (cwd, DIR_SEPARATOR_S, filename, NULL);

        /*see if this file is already loaded */
        page = geda_struct_page_search (toplevel, fullname);

        geda_free (cwd);
        geda_free (fullname);
      }

      if (page == NULL ) {
        GError *err = NULL;
        /* Problem: geda_file_open needs a pointer to a page so we have to create
         * a page struct without knowing the file can be read. If an error
         * occurs then we have to delete this page but geda_struct_page_delete is
         * going to free the name, the one passed to us as a constant, so
         * we have to make a copy here for the maybe future page */
        page = geda_struct_page_new (toplevel, geda_strdup (filename));

        /* Try to load the file */
        if (!geda_file_open (toplevel, page, (char *) filename, &err)) {
          fprintf(stderr, "Error loading file:%s\n", err->message);
          g_error_free (err);
          geda_struct_page_delete (toplevel, page, FALSE);
          resolve_2_recover(NULL);
        }
      }
      else { /* File is already open, so make it the current page */
        geda_struct_page_goto (page);
        /* Fall through and return existing page */
      }
    }
    else {  /* File name specified but does not exist, check path */

      char *path;
      int   file_err;

      errno = 0;
      access (filename,  W_OK && F_OK);

      file_err = errno;                     /* save file error */
      path     = strcpy (&strbuff[0], filename);
      path     = dirname(path);             /* geda_get_dirname make copy */

      /* If the path is OK but no file then just create a new file */
      if ((access(path, W_OK && X_OK && F_OK) == 0) && (file_err == ENOENT)) {
        /* Filespec may not exist but user has authority to create */
        page = empty_page(filename);
      }
      else { /* Houston, we have problem */

        /* Filename was specified but path error, so we still do not
         * know if the base name is okay. Break down filespec and try
         * to sort out the problem:
         */
        if (errno == ENOENT) { /* 100% sure file_err == ENOENT */
          if (geda_create_path (path, S_IRWXU | S_IRWXG) == NO_ERROR ) {
            page = empty_page(filename);
            errno = NO_ERROR;
          }
          else {
            fprintf(stderr, "Path \"%s\": is not accessible: %s\n",
                    path, strerror(errno));
          }
        }

        if (errno != NO_ERROR) {

          const char *home_dir;

#ifdef OS_LINUX

          home_dir = g_getenv ("HOME");   /* does not allocate */

          if (!home_dir)
            home_dir = g_get_home_dir (); /* does not allocate */
#else
          home_dir = (char*)g_get_home_dir ();
#endif
          path = strcpy(&strbuff[0], home_dir);

          ptr  = (char*)filename;

          while (*ptr != '\0') ++ptr;     /* advance to end of argument */

          while (*ptr != DIR_SEPARATOR) --ptr;   /* backup to separator */

          path = strcat(path, ptr);

#if DEBUG
          perror(stderr, "filename:%s\n path:%s\n", path, filename);
#endif
          resolve_2_recover(path);
        }
      }
    }
  }

  PyObject *page_info;

  if (page) {
    page_info = Py_BuildValue("si", filename, page->pid);
  }
  else {
    page_info = Py_BuildValue("");
  }

  return page_info;
}

/*!
 * \brief Close a Page
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function calls the libgeda function geda_struct_page_delete to remove
 *  the page referenced by the ID argument from memory.
 *
 * \param [in] pid  Integer, the page id of the page to close
 *
 * \return [out] The pid of the assigned current page or -1 if there are
 *               no page opened after closing the argument page
 */
int PyGeda_close_page(int pid)
{
  Page *page;
  int   new_pid;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  /* remove page from toplevel list of page and free */
  geda_struct_page_delete (toplevel, page, TRUE);

  page = geda_toplevel_get_current_page(toplevel);

  /* Switch to a different page if we just removed the current */
  if (!page) {

    page = geda_toplevel_get_page_up(toplevel);

    /* If there is another page open then switch to it */
    if (!page) {
      new_pid = -1;
    }
    else {

      /* change to new_current */
      if (geda_struct_page_set_current(toplevel, page))
        new_pid = page->pid;
      else
        new_pid = -1;
    }
  }
  else {
    new_pid = geda_page_get_pid(page);
  }

  return new_pid;
}

/*!
 * \brief Start a New Page
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function calls the PyGeda_open_page after determining of a file
 *  current exist and if so, whether a back-up should made. If the optional
 *  over_write flag is FALSE then the setting of toplevel->make_backup_files
 *  will determine if any existing files should be backed up.
 *
 * \param [in] filename String, file name or NULL = "untitled"
 * \param [in] over_write  Integer, the page id of the page to be saved
 *
 * \return [out] PyObject result of calling PyGeda_open_page.
 */
PyObject *PyGeda_new_page( const char *filename, int over_write)
{
  char *fname = NULL;

  if (filename) {

    if (g_file_test (filename, G_FILE_TEST_EXISTS)) {

      char *command;

      if (toplevel->make_backup_files && !over_write) {
        command = geda_strconcat("mv ", filename, " ", filename, ".bak", NULL);
      }
      else {
        command = geda_strconcat("rm -f ", filename, NULL);
      }
      if (system(command) != 0)
        perror("system");
      GEDA_FREE(command);
    }
    fname = (char*)filename;
  }

  return PyGeda_open_page(fname);
}

/*!
 * \brief Rename Page
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function calls the libgeda object setter function to
 *  change the file name string.
 *
 * \param [in] pid      Integer, the page id of the page to rename
 * \param [in] filename String to set as the new file name
 *
 * \return [out] True on success, otherwise FALSE
 */
int PyGeda_rename_page (int pid, const char *filename)
{
  int status = 0;

  /* Check for empty string */
  if (strlen(filename) > 0) {

    Page *page;

    page = geda_toplevel_get_page_by_id(toplevel, pid);

    if (page && (GEDA_IS_PAGE(page))) {

      if (geda_is_path_absolute (filename)) {
        status = geda_page_rename(page, filename);
      }
      else {

        char *fname;
        char *ppath;

        ppath = geda_file_path_get_dirname(page->filename);

        fname = geda_strconcat (ppath, DIR_SEPARATOR_S, filename, NULL);

        status = geda_page_rename(page, fname);

        geda_free(fname);
        geda_free (ppath);
      }
    }
  }

  return status;
}

/*!
 * \brief Save Page
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function calls the libgeda function geda_save_file to save the
 *  page document referenced by the ID argument.
 *
 * \param [in] pid  Integer, the page id of the page to be saved
 *
 * \return True on success, otherwise FALSE.
 */
int
PyGeda_save_page( int pid )
{
  Page *page;
  int   status = 0;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  if (page && (GEDA_IS_PAGE(page))) {

    GError *err = NULL;

    if (!geda_save_file (toplevel, page, page->filename, &err)) {
      fprintf(stderr, "%s\n", err->message);
      g_clear_error (&err);
      status++;
    }
  }
  else {
    status++;
  }

  return status;
}

/*!
 * \brief Save Page As new file Name
 * \ingroup Python_API_Library
 * \par Function Description
 *  This is a convenience function that combines PyGeda_rename_page
 *  and PyGeda_save_page.
 *
 * \param [in] pid      Integer page id of the page to save
 * \param [in] filename String to set as the new file name
 *
 * \return [out] True on success, otherwise FALSE
 */
int PyGeda_save_page_as (int pid, const char *filename)
{
  int status;

  status = PyGeda_rename_page (pid, filename);

  return status ? !PyGeda_save_page(pid) : 0;
}

/*!
 * \brief Save All Pages
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function iterates through all Page objects in the argument list or all
 *  pages of the argument is NULL, and calls the libgeda function geda_save_file
 *  for each page document.
 *
 * \param [in] py_page_list A PyList Object
 *
 * \return [out] integer number of errors that occurred.
 */
int PyGeda_save_all_pages( PyObject *py_page_list )
{
  GError *err    = NULL;
  GList  *pages  = NULL;
  GList  *iter   = NULL;
  Page   *page   = NULL;
  int     status = 0;

  if (PyList_Check(py_page_list)) {

    int i;
    int count = (int) PyList_GET_SIZE(py_page_list);

    for (i = 0; i < count ; i++)
    {
      PyObject   *py_page;

      int         pid;

      py_page     = PyList_GET_ITEM(py_page_list, i);
      pid         = ((PyGedaPageObject*)py_page)->pid;
      page        = geda_toplevel_get_page_by_id(toplevel, pid);
      pages       = g_list_append(pages, page);
    }
  }
  else {
    pages = geda_list_copy_glist(toplevel->pages);
  }

  iter = g_list_first(pages);
  while (iter) {
    page = iter->data;
    if (page && (GEDA_IS_PAGE(page))) {
      if (!geda_save_file (toplevel, page, page->filename, &err)) {
        fprintf(stderr, "%s\n", err->message);
        g_clear_error (&err);
        status++;
      }
    }
    NEXT(iter);
  }

  g_list_free(pages);

  return status;
}

/*!
 * \brief Check is Object is a GedaCapsuleObject
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function returns True is the argument object is a
 *  GedaCapsule type, otherwise FALSE.
 *
 * \param [in] py_object  A PyObject to check
 *
 * \return [out] True of object is GedaCapsuleObject.
 */
int PyGeda_GedaCapsule_Type(PyObject *py_object)
{
  int answer = 0;

  if (py_object != NULL) {
    answer = PyObject_TypeCheck(py_object, GedaCapsuleClass());
  }

  return answer;
}

/*!
 * \brief Get Bounds of a GedaObject or All objects on a Page
 * \par Method Description
 *  This function provides a method to retrieve the bounds of an object
 *  or all objects on a page. To get the bounds of all objects on a page,
 *  pid should be a valid page identifier an sid should be any negative
 *  value. To get the bounds of a specific object, \a sid should be the
 *  sequence index of the object and \a pid should be a page identifier
 *  or -1 if the object has not been committed to a page. This is a low
 *  level library routine for the get-bounds module API, which handles
 *  setting \a pid and \a sid based on the object that was supplied to
 *  the module.
 *
 * \param [in] pid Interger Page identifier or -1
 * \param [in] sid Interger Sequence identifier of object or -1
 *
 * \return [out] A PyList of four integers; left, top, right, bottom,
 *               or and empty list if there is no bounds, or NULL if
 *               an error occurred.
 */
PyObject *PyGeda_get_bounds( int pid, int sid )
{
  Page     *page;
  PyObject *py_list;

  int left, top, right, bottom;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  if (sid < 0) {

    GList *list = geda_struct_page_get_objects(page);

    if (geda_object_get_bounds_list (list, &left, &top, &right, &bottom)) {
      py_list = Py_BuildValue("iiii",  left, top, right, bottom);
    }
    else {
      py_list = Py_BuildValue("()"); /* No objects so return an empty list */
    }
  }
  else {

    GedaObject *object = geda_struct_page_get_object(page, sid);

    /* If no object then check for object not yet committed to a page */
    if (!object) {
      object = get_floating_object(sid);
    }

    if (object) {
      if (geda_object_get_bounds (object, &left, &top, &right, &bottom)) {
        py_list = Py_BuildValue("iiii",  left, top, right, bottom);
      }
      else {
        py_list = Py_BuildValue("()"); /* Should never happen */
      }
    }
    else {
      py_list = NULL;
    }
  }

  return py_list;
}

/*!
 * \brief Get an Object from GedaCapsuleObject
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function returns the data to create a PyPyGedaObject from an object
 *  pointer to by the contents of the given GedaCapsule.
 *
 * \param [in] py_capsule   The PyObject container object
 *
 * \return [out] A PyGedaObject construction data.
 */
PyObject *PyGeda_get_object(PyObject *py_capsule)
{
  GedaObject *object;

  object = GedaCapsule_GetPointer(py_capsule);

  if (GEDA_IS_OBJECT(object)) {

    switch (object->type) {
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX:
        return get_complex_object_data(object);
      case OBJ_TEXT:
        return get_text_object_data(object);
      case OBJ_NET:
        return get_net_object_data(object);
      case OBJ_LINE:
        return get_line_object_data(object);
      case OBJ_PATH:
        return get_path_object_data(object);
      case OBJ_BOX:
        return get_box_object_data(object);
      case OBJ_PICTURE:
        return get_picture_object_data(object);
      case OBJ_CIRCLE:
        return get_circle_object_data(object);
      case OBJ_BUS:
        return get_bus_object_data(object);
      case OBJ_PIN:
        return get_pin_object_data(object);
      case OBJ_ARC:
        return get_arc_object_data(object);
      default:
        break;
    }
  }

  return NULL;
}

/*!
 * \brief Get a List Objects from an Object
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function returns a list of existing objects from another object. The
 *  source object can be a <p>Page</p> or a <p>PyGedaObject</p>. The returned
 *  list contains capsule items.  Encapsulation of objects is performed for
 *  efficiency and memory management. If real PyGedaObjects had to be created
 *  for an entire schematic containing numerous objects, the time required for
 *  Python to manage the memory would approach "hard-disk" access times. And
 *  when the list was later dereferenced, similar delays would occur while
 *  Python was performing garbage collection.
 *
 * \param [in] pid  Integer, the page id of the page from which to get the sub-objects
 * \param [in] sid  Integer, the PyGedaObject id of the Object from which to get the sub-objects
 *
 * \return [out] PyList list of GedaCapsule Objects or an empty list if the
 *               source object did not contain any objects.
 */
PyObject *PyGeda_get_objects( int pid, int sid )
{
  GList      *list;
  GedaObject *object;
  Page       *page;
  PyObject   *py_list;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  if (sid < 0) {
    list = geda_struct_page_get_objects (page);
  }
  else {

    object = geda_struct_page_get_object(page, sid);

    if (!object) {
      object = get_floating_object(sid);
    }

    if (object && GEDA_IS_COMPLEX(object)) {
      list = object->complex->prim_objs;
    }
    else {
      list = NULL;
    }
  }

  if (list) {
    py_list = PyGeda_glist_2_pylist(list);
  }
  else {
    py_list = Py_BuildValue("()");
  }

  return py_list;
}

/*!
 * \brief Add an Object to another Object
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function add an object to another object. The target object can be a
 *  Page or another object. The arguments are positional.
 *
 * \param [in] PyPage      The Pageobject or NULL if the object are being added
 *                         to py_object_A
 * \param [in] py_object_A The Geda object to receive the child or NULL if object
 *                         is being added to page given by parameter 1.
 * \param [in] py_object_B The PyGedaObject to be added
 *
 * \return True if successful, otherwise False.
 */
int PyGeda_add_object( PyObject *PyPage, PyObject *py_object_A, PyObject *py_object_B )
{
  PyGedaObject *geda_pyparent = (PyGedaObject*)py_object_A;
  PyGedaObject *geda_pyobject = (PyGedaObject*)py_object_B;
  GedaObject   *parent; /* Could be a solo object being added */
  GedaObject   *object;
  Page         *page;

  int pid;
  int sid;
  int status = 0;

  /* The module already checked that object B was a PyGedaObject */
  sid  = geda_pyobject->sid;

  /* If PyGedaPageObject then adding to a page */
  if (PyPage) {

    pid  = ((PyGedaPageObject*)PyPage)->pid;
    page = geda_toplevel_get_page_by_id(toplevel, pid);

    if (page && (GEDA_IS_PAGE(page))) {

      int count = 0;

      geda_pyobject->pid = pid; /* save page id to the pyobject */
      parent = retrieve_floating_object(sid);

      if (PyGeda_update_object(parent, geda_pyobject)) {

        geda_struct_page_append_object(page, parent);

        if (geda_pyobject->auto_attributes == TRUE) {
          if (parent->type == OBJ_PIN) {
            geda_pin_object_realize_attributes(toplevel, parent);
          }
        }

        /* Check if this object has attached attributes and add them too */
        if (parent->type == OBJ_COMPLEX) {

          GList  *butes = parent->attribs;

          while (butes != NULL) {

            object = (GedaObject*)butes->data;

            if (!object->page) {
              geda_struct_page_append_object(page, object);
            }
            NEXT (butes);
          }; /* wend*/
        }

        /* Check if this object has attributes from Python and add them too */
        PyObject *py_list = geda_pyobject->attributes;

        if (py_list) {

          int i;

          count = (int) PyList_GET_SIZE(py_list);

          for (i = 0; i < count ; i++) {

            GedaCapsule *py_capsule;

            py_capsule = (GedaCapsule*)PyList_GET_ITEM(py_list, i);

            object     = retrieve_floating_object(py_capsule->sid);

            if (object) {

              /* We get here when an object was added to a page and the object
               * had attributes and these "children" need to be referenced by
               * the parent object. Note that when the capsule was retrieved
               * they are removed from the the list of "floating" objects.
               */
              geda_struct_object_add_child(parent, object);

              if (geda_pyobject->auto_attributes == TRUE) {
                if (parent->type == OBJ_PIN) {
                  geda_pin_object_realize_attributes(toplevel, parent);
                }
              }
            }
          }
        }
      }
      status = count;
    }
  }
  else { /* Adding an attribute object to an object */

    pid  = geda_pyparent->pid;
    page = geda_toplevel_get_page_by_id(toplevel, pid);

    /* If the pyparent is already on a page then add child */
    if (page && (GEDA_IS_PAGE(page))) {

      parent = geda_page_get_object(page, geda_pyparent->sid);
      object = retrieve_floating_object(sid);

      if (PyGeda_update_object(object, geda_pyobject)) {
        geda_struct_object_add_child(parent, object);
      }
    }
    else { /* Parent is still in Que (and child is also) */

      /* Add to parent */
      object = get_floating_object(geda_pyobject->sid);

      geda_pyparent->attributes = PyGeda_append_2_pylist(geda_pyparent->attributes, object);

      if (geda_pyparent->attributes == NULL) { /* sanity check */
        fprintf(stderr, "PyGeda_add_object: Error appending child object\n");
      }
    }
    status = sid;
  }

  return status;
}

/*!
 * \brief Python API Library Add Objects to an Object
 * \par Function Description
 *  This function provides a method to add a list of existing object to another
 *  object. The target object can be a Page or another object. The arguments are
 *  positional.
 *
 * \param [in] PyPage       Pageobject or NULL if the object are being added to
 *                          py_object_A
 * \param [in] py_object_A  The Geda object to receive the objects or NULL if the
 *                          are being added to page given by parameter 1.
 * \param [in] py_object_B  PyList, the list of PyGedaObjects to be added
 *
 * \return True if successful, otherwise False.
 */
int PyGeda_add_objects( PyObject *PyPage, PyObject *py_object_A, PyObject *py_object_B )
{
  int i;
  int count;
  int status = 0;

  count = (int) PyList_GET_SIZE(py_object_B);

  for (i = 0; i < count; i++) {

    PyObject *geda_object = PyList_GET_ITEM(py_object_B, i);

    status += PyGeda_add_object(PyPage, py_object_A, geda_object);
  }

  return status;
}

/*!
 * \brief Copy an Object
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function copies an existing object. The Object does not have to be on
 *  a Page. The object argument must be a PyGedaObject, such as PyGedaComplexObject
 *  type, not a GedaCapsule object.
 *
 * \param [in] py_object PyObject, the PyGedaObject to be copied
 *
 * \param [in] dx  Integer X offset relative to the source object's location
 * \param [in] dy  Integer Y offset relative to the source object's location
 *
 * \note 1. The target offset arguments are mutually optional, either both must be
 *          provided or neither. If offset arguments are not provided the copy will
 *          coincide with the original. If only one of the offsets is set to -1 then
 *          this value will be interprted as a coordinate.
 *
 * \note 2. This is the only copy function in the Python_API_Library. The Pyobject
 *          types are derived from PyGedaObject, whose copy method utilizes the main
 *          geda.copy_object method which calls this API function.
 *
 * \return A GedaCapsule containing a reference to the new object.
 */
PyObject *PyGeda_copy_object( PyObject *py_object, int dx, int dy )
{
  PyObject     *py_capsule  = NULL;
  PyGedaObject *geda_object = (PyGedaObject*)py_object;
  int           pid         = geda_object->pid;
  int           sid;
  GedaObject   *src_object;
  GedaObject   *new_object  = NULL;
  Page         *page;

  GList  *dest_list = NULL;
  GList  *iter;

  sid  = geda_object->sid;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  if (page && (GEDA_IS_PAGE(page))) {

    src_object = geda_struct_page_get_object(page, sid);

    if (src_object) {

      new_object = geda_object_copy(src_object);

      geda_struct_page_append_object(page, new_object);

      if (src_object->attribs) {
        for (iter = src_object->attribs; iter != NULL; NEXT(iter)) {

          GedaObject *a_current = iter->data;
          GedaObject *a_new = geda_object_copy(a_current);

          geda_struct_page_append_object (page, a_new);

          dest_list = g_list_append(dest_list, a_new);

          geda_attrib_object_add(new_object, a_new);
        }
      }
      dest_list = g_list_prepend(dest_list, new_object);
    }
  }
  else {

    src_object = get_floating_object(sid);

    if (GEDA_IS_OBJECT(src_object)) {

      new_object = geda_object_copy(src_object);

      add_floating_object(new_object);

      if (src_object->attribs) {
        for (iter = src_object->attribs; iter != NULL; NEXT(iter)) {

          GedaObject *a_current = iter->data;
          GedaObject *a_new = geda_object_copy(a_current);

          dest_list = g_list_append(dest_list, a_new);

          add_floating_object(a_new);
          geda_attrib_object_add(new_object, a_new);
        }
      }
      dest_list = g_list_prepend(dest_list, new_object);
    }
  }

  if (dest_list) {

    if (dx != -1 && dy != -1) {
      geda_translate_list(dest_list, dx, dy);
    }

    py_capsule = GedaCapsule_New(new_object);

    g_list_free(dest_list);
  }

  return Py_BuildValue("(O)", py_capsule);
}

/*!
 * \brief Remove an Object from a Page
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function removes an existing object from the page which the object
 *  is associated. Removing an object does not destroy the object. The
 *  removed object is appended to the glist of floating_objects for future
 *  reference.
 *
 * \param [in] py_object    The Geda PyObject to be removed
 *
 * \return True if successful otherwise False, False would only
 *         be returned if an object did not exist.
 */
int PyGeda_remove_object( PyObject *py_object )
{
  PyGedaObject *geda_object = (PyGedaObject*)py_object;
  int           pid         = geda_object->pid;
  int           status      = -1;
  Page         *page;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  if (page && (GEDA_IS_PAGE(page))) {

    int sid  = geda_object->sid;

    const GedaObject *object = geda_struct_page_get_object(page, sid);

    if (object) {
      geda_struct_page_remove_object(page, (GedaObject*)object);
      add_floating_object((GedaObject*)object);
    }
    status = 0;
  }

  return status;
}

/*!
 * \brief Remove a PyList of Objects from a Page
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function removes a list of existing objects from a page which the
 *  objects are associated. The objects in the list do not have to be the
 *  same page.
 *
 * \param [in] pyobjects PyList Of Geda objects to be removed
 *
 * \return True if success otherwise False, False would only be
 *         returned if the page conatining the object did not exist.
 */
int PyGeda_remove_objects( PyObject *pyobjects )
{
  int i;
  int count;
  int status = 0;

  count = (int) PyList_GET_SIZE(pyobjects);

  for (i = 1; count < i; i++) {
    PyObject *geda_object = PyList_GET_ITEM(pyobjects, i);
    status += PyGeda_remove_object(geda_object);
  }

  return status;
}

/*!
 * \brief Delete an Object
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function deletes the given Object
 *
 * \param [in] py_object    The Geda PyObject to be deleted
 *
 * \return True if success otherwise False, False would only be
 *         returned if the page conatining the object did not exist.
 */
int PyGeda_delete_object( PyObject *py_object )
{
  PyGedaObject *geda_object = (PyGedaObject*)py_object;
  GedaObject   *object      = NULL;
  int           pid         = geda_object->pid;
  int           sid         = ((PyGedaObject*)geda_object)->sid;
  int           status      = 0;

  if (pid >= 0 ) {

    Page *page = geda_toplevel_get_page_by_id(toplevel, pid);

    if (GEDA_IS_PAGE(page)) {

      object = geda_page_get_object(page, sid);

      if (GEDA_IS_OBJECT(object)) {

        const char *name = PyString_AsString(geda_object->name);

        if (strcmp(object->name, name) == 0) {
          geda_struct_page_remove_object(page, object);
        }
      }
    }
    else {
      status = -1;
    }
  }
  else {
    /* Retrieving also removes object from floating list */
    object = retrieve_floating_object(sid);
  }

  if (GEDA_IS_OBJECT(object)) {
    geda_struct_object_release(object);
  }

  return status;
}

/*!
 * \brief Delete a List of Objects
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function calls PyGeda_delete_object for each object in the
 *  supplied list of pyobjects
 *
 * \param [in] objects PyObject of type PyList containing PyPyGedaObjects
 *
 * \return True if success otherwise False, False would only be returned
 *         if an object in the list did not exist.
 */
int PyGeda_delete_objects( PyObject *objects )
{
  int i;
  int count;
  int status = 0;

  count = (int) PyList_GET_SIZE(objects);
  for (i = 1; count < i; i++) {
    PyObject *geda_object = PyList_GET_ITEM(objects, i);
    status += PyGeda_delete_object(geda_object);
  }
  return status;
}

/*!
 * \brief Synchronize GedaObject with a PyGedaObject
 * \ingroup Python_API_Library
 * \par Function Description
 *  This function the update the values of Library's Object the with
 *  the value from the given PyObject
 *
 * \param [in] py_object    The Geda PyObject to be updated
 *
 * \return True if success otherwise False, False would only be returned
 *         if the page containing the object did not exist.
 */
int PyGeda_sync_object( PyObject *py_object )
{
  PyGedaObject *geda_object = (PyGedaObject*)py_object;
  GedaObject   *object      = NULL;
  int           pid         = geda_object->pid;
  int           sid         = ((PyGedaObject*)geda_object)->sid;
  int           status      = 0;

  if (pid >= 0 ) {

    Page *page = geda_toplevel_get_page_by_id(toplevel, pid);

    if (GEDA_IS_PAGE(page)) {
      object = geda_page_get_object(page, sid);
    }
    else {
      status = -1;
    }
  }
  else {
    object = retrieve_floating_object(sid);
  }

  if (GEDA_IS_OBJECT(object)) {
#if DEBUG
    fprintf(stderr, "PyGeda_sync_object: name=%s\n", object->name);
#endif
    status = PyGeda_update_object(object, geda_object);
  }
  return status;
}

/** \defgroup Python_API_Create_Functions  Python API Creation Functions
 *  @{
 */

/*!
 * \brief Python API Library Create a New Arc Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function provides a Arc object and returns the construction
 *  data required to produce a Python version of the object.
 *
 * \param [in] x           Integer center X location
 * \param [in] y           Integer center Y location
 * \param [in] radius      Integer radius of the arc sector
 * \param [in] start_angle Integer start angle of the sector
 * \param [in] arc_sweep   Integer ending angle of the sector
 * \param [in] py_color    PyObject color
 *
 * \return [out] PyGedaArcObject construction data.
 */
PyObject *PyGeda_new_arc ( int x, int y, int radius, int start_angle, int arc_sweep, PyObject *py_color)
{
  GedaObject *object;

  int color = translate_color(py_color, GRAPHIC_COLOR);

  object = geda_arc_object_new(color, x, y, radius, start_angle, arc_sweep);

  add_floating_object(object);

#if DEBUG
  fprintf(stderr, "PyGeda_new_arc: name=%s\n", object->name);
#endif
  return get_arc_object_data(object);
}

/*!
 * \brief Python API Library Create a New Box Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function provides a Box object and returns the construction
 *  data required to produce a Python version of the object.
 *
 * \param [in] lower_x  Integer lower X corner
 * \param [in] lower_y  Integer lower Y corner
 * \param [in] upper_x  Integer upper X corner
 * \param [in] upper_y  Integer upper Y corner
 * \param [in] py_color PyObject color
 *
 * \return [out] PyGedaBoxObject construction data.
 */
PyObject *PyGeda_new_box (int lower_x, int lower_y, int upper_x, int upper_y, PyObject *py_color)
{
  GedaObject *object;

  int color = translate_color(py_color, GRAPHIC_COLOR);

  /* The Y's are intentional reversed here to compensate for Libgeda, being
   * backwards, is cause X11 is upside down */
  object = geda_box_object_new(color, lower_x, upper_y, upper_x, lower_y);

  add_floating_object(object);

#if DEBUG
  fprintf(stderr, "PyGeda_new_box: name=%s\n", object->name);
#endif
  return get_box_object_data(object);
}

/*!
 * \brief Python API Library Create a New Bus Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function provides creates new Bus object and returns the object to
 *  get_net_object_data, so that the caller is returned the data to create a
 *  Python version of the object. If the busname argument is non-null then a
 *  "netname" attribute is attached to the object using the string pointed to
 *  by busname as the value. If the busname argument is NULL the object name
 *  is used as the netname attribute.
 *
 * \param [in] busname  string bus_name attribute (can be NULL)
 * \param [in] x1       integer from X location
 * \param [in] y1       integer from Y location
 * \param [in] x2       integer to X location
 * \param [in] y2       integer to Y location
 * \param [in] py_color PyObject color
 *
 * \return [out] PyGedaBusObject construction data.
 */
PyObject *PyGeda_new_bus (const char *busname, int x1, int y1, int x2, int y2, PyObject *py_color)
{
  GedaObject *object;

  int color = translate_color(py_color, BUS_COLOR);

  object = geda_bus_object_new(color, x1, y1, x2, y2, 0);

  if (busname) { /* then create a text attribute for netname */
    GedaObject *net_attrib;
    object->bus->bus_name  = geda_utility_string_strdup(busname);
    net_attrib = geda_attrib_object_new_attached(object, "netname", busname, INVISIBLE, SHOW_VALUE);
    add_floating_object(net_attrib);
  }
  else {
    object->bus->bus_name  = geda_utility_string_strdup(object->name);
  }

  object->bus->ripper_direction = geda_bus_object_get_direction(object);

  add_floating_object(object);

#if DEBUG
  fprintf(stderr, "PyGeda_new_bus: name=%s\n", object->name);
#endif
  return get_bus_object_data(object);
}

/*!
 * \brief Python API Library Create a New Circle Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function provides a new Circle object and returns data to
 *  create a Python version of the object.
 *
 * \param [in] x         Integer center X location
 * \param [in] y         Integer center Y location
 * \param [in] radius    Integer to X location
 *
 * \param [in] py_color  PyObject a color object (not implemented yet)
 *
 * \return [out] PyGedaCircleObject construction data.
 */
PyObject *PyGeda_new_circle( int x, int y, int radius, PyObject *py_color )
{
  GedaObject *object;

  object = geda_circle_object_new(GRAPHIC_COLOR, x, y, radius);

  add_floating_object(object);

#if DEBUG
  fprintf(stderr, "PyGeda_new_circle: name=%s\n", object->name);
#endif

  return get_circle_object_data(object);
}

/*!
 * \brief Python API Library Create a New Complex Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function creates new Complex object. The symbol must be known to
 *  the Library.
 *
 * \param [in] filename String  The base file name of the symbol without the extension
 * \param [in] x        Integer X insertion location
 * \param [in] y        Integer Y insertion location
 * \param [in] angle    Integer orientation property
 * \param [in] mirror   Integer property whether to mirror the symbol
 * \param [in] embed    Integer property whether to embed the symbol data
 *
 * \return [out] PyGedaComplexObject construction data.
 *
 * \note  Optional agruments with a value of -1 will be assigned default values.
 */
PyObject*
PyGeda_new_complex(const char *filename, int x, int y, int angle, int mirror, int embed)
{
  GedaObject *object;
  const       CLibSymbol *clib;
  char       *sym_file;

  int ang  = angle  < 0 ? 0 : angle;
  int mirr = mirror < 0 ? 0 : mirror;
  int emb  = embed  < 0 ? 0 : embed;

  sym_file = geda_strconcat(filename, SYMBOL_FILE_DOT_SUFFIX, NULL);
  clib     = geda_struct_clib_get_symbol_by_name (sym_file);

  if (clib != NULL) {
    object = geda_complex_object_new (toplevel, x, y, ang, mirr, clib, sym_file, TRUE);
  }
  else {
    object = NULL;
  }

  GEDA_FREE(sym_file);

  if (GEDA_IS_COMPLEX(object)) {

    object->attribs =  geda_complex_object_promote_attribs (toplevel, object);

    object->complex->is_embedded = emb;

    add_floating_object(object);
  }
  else {
    return NULL;
  }

#if DEBUG
  fprintf(stderr, "PyGeda_new_complex: name=<%s>, type=%i, sid=%d, basename=%s, emb=%d, x=%d, y=%d, angle=%d, mirror=%d\n",
          object->name, object->type, object->sid, basename, emb, x, y, angle, mirror);
#endif
  return get_complex_object_data(object);
}

/*!
 * \brief Python API Library Create a New Line Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function provides a new Line object and returns the data
 *  to create the Python version of the object.
 *
 * \param [in] x1           Integer from X location
 * \param [in] y1           Integer from Y location
 * \param [in] x2           Integer to X location
 * \param [in] y2           Integer to Y location
 * \param [in] py_color     PyObject color
 *
 * \return [out] PyGedaLineObject construction data.
 */
PyObject *PyGeda_new_line ( int x1, int y1, int x2, int y2, PyObject *py_color)
{
  GedaObject *object;

  int color = translate_color(py_color, GRAPHIC_COLOR);

  object = geda_line_object_new(color, x1, y1, x2, y2);

  add_floating_object(object);

#if DEBUG
  fprintf(stderr, "PyGeda_new_line: name=%s\n", object->name);
#endif

  return get_line_object_data(object);
}

/*!
 * \brief Python API Library Create a New Net Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function creates new Net object and returns the object via
 *  get_net_object_data, so that the caller is returned the data to create a
 *  Python version of the object. If the netname argument is non-null then a
 *  "netname" attribute is attached to the object using the string pointed to
 *  by netname as the value.
 *
 * \param [in] netname      String, net_name attribute (can be NULL)
 * \param [in] x1           Integer from X location
 * \param [in] y1           Integer from Y location
 * \param [in] x2           Integer to X location
 * \param [in] y2           Integer to Y location
 * \param [in] py_color     PyObject color
 *
 * \return [out] PyGedaNetObject construction data.
 */
PyObject *PyGeda_new_net (const char *netname, int x1, int y1, int x2, int y2, PyObject *py_color)
{
  GedaObject *object;

  int color = translate_color(py_color, NET_COLOR);

  object = geda_net_object_new(color, x1, y1, x2, y2);

  /* Set in the gobject so the memory will be freed later */
  if (netname) {
    /* Create a text attribute for netname */
    GedaObject *net_attrib;

    geda_net_set_netname(object, netname);

    net_attrib = geda_attrib_object_new_attached(object, "netname", netname, INVISIBLE, SHOW_VALUE);

    add_floating_object(net_attrib);
  }
  else {
    object->net->net_name  = geda_utility_string_strdup(object->name);
  }

  object->net->nid = object->sid;

  add_floating_object(object);

#if DEBUG
    fprintf(stderr, "PyGeda_new_net: name=%s, netname=%s\n", object->name,
                                              netname ? netname : "NULL");
#endif

  return get_net_object_data(object);
}

/*!
 * \brief Python API Library Create a New Path Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function creates new Path Object given a path string and
 *  returns necessary data via the get_path_object_data() to create
 *  a Python version of the Object.
 *
 * \param [in] path_string  String, the SVG path string
 *
 * \return [out] PyGedaPathObject construction data.
 */
PyObject *PyGeda_new_path (const char *path_string)
{
  GedaObject *object;

  object = geda_path_object_new(GRAPHIC_COLOR, path_string);

  if (GEDA_IS_PATH(object)) {
      add_floating_object(object);
  }
  else {
    return NULL;
  }

#if DEBUG
  fprintf(stderr, "PyGeda_new_path: name=%s\n", object->name);
#endif

  return get_path_object_data(object);
}

/*!
 * \brief Python API Library Create a New Picture Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function create a new Pin Object and returns using get_picture
 *  _object_data function in provide construction data to create the a
 *  Python version of the object.
 *
 * \param [in] filepath string  The file name of the image
 * \param [in] x1       integer lower left X location
 * \param [in] y1       integer lower left Y location
 * \param [in] x2       integer top right X location
 * \param [in] y2       integer top right Y location
 *
 *  optional: (will be assigned default values if < 0)
 *
 * \param [in] angle    integer orientation property
 * \param [in] mirror   integer property whether to mirror the image
 * \param [in] embed    integer property whether to embed the image data
 *
 * \return [out] PyGedaPictureObject construction data or NULL if an error occurred,
 *               such as the file did not exist or was not readable.
 */
PyObject*
PyGeda_new_picture (const char *filepath, int x1, int y1, int x2, int y2,
                                          int angle, int mirror, int embed)
{
  GedaObject *object;

  int ang  = angle  < 0 ? 0 : angle;
  int mirr = mirror < 0 ? 0 : mirror;
  int emb  = embed  < 0 ? 0 : embed;

  object = geda_picture_object_new (NULL, 0, filepath, x1, y1, x2, y2, ang, mirr, emb);

  if (GEDA_IS_PICTURE(object)) {
    add_floating_object(object);
  }
  else {
    return NULL;
  }

#if DEBUG
  fprintf(stderr, "PyGeda_new_picture: name=%s, file=%s\n", object->name, filepath);
#endif

  return get_picture_object_data(object);
}

/*!
 * \brief Python API Library Create a New Pin Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function create a new Pin Object and returns using get_pin_object
 *  _data function in provide construction data to create the a Python
 *  version of the object.
 *
 * \param [in] label  string  pin label attribute
 * \param [in] number   string  pin number attribute
 * \param [in] x1     integer from X location
 * \param [in] y1     integer from Y location
 * \param [in] x2     integer to X location
 * \param [in] y2     integer to Y location
 *
 * \param [in] whichend integer Which gets connected (either 0 or 1)
 *
 * \param [in] etype    integer electrical type attribute (formally pin type)
 * \param [in] mtype    integer mechanical type attribute
 * \param [in] ntype    integer node type property ( 0=normal, 1=bus type)
 *
 * \return [out] PyGedaPinObject construction data.
 */
PyObject*
PyGeda_new_pin (const char *label, const char *number, int x1, int y1, int x2, int y2,
                int whichend, int etype, int mtype, int ntype)
{
  GedaObject *object;

  int conn2 = whichend < 0 ? 0 : whichend;

  object = geda_pin_object_new(PIN_COLOR, x1, y1, x2, y2, 0, conn2);

  if (label) {
    object->pin->label = geda_utility_string_strdup(label);
  }
  else {
    object->pin->label = geda_utility_string_strdup("unknown");
  }

  if (etype < 0) {
    geda_pin_object_set_elect_type(object, PIN_ELECT_VOID);
  }
  else {
    geda_pin_object_set_elect_type(object, etype);
  }

  if (object->pin->electrical == NULL) {
    geda_pin_set_electrical(object->pin, "unknown");
  }

  if (mtype < 0) {
    geda_pin_object_set_mech_type(object, PIN_MECH_VOID);
  }
  else {
    geda_pin_object_set_mech_type(object, mtype);
  }
  if (object->pin->mechanical == NULL) {
    geda_pin_set_mechanical(object->pin, "unknown");
  }

  if (ntype < 0) {
     geda_pin_object_set_node_type(object, PIN_NET_NODE);
  }
  else {
    if (ntype == PIN_NET_NODE || ntype == PIN_BUS_NODE) {
      geda_pin_object_set_node_type(object, ntype);
    }
    else {
      geda_pin_object_set_node_type(object, PIN_NET_NODE);
    }
  }

  if (number) {
    object->pin->number = geda_utility_string_strdup(number);
  }
  else {
    object->pin->number = geda_utility_string_strdup("");
  }

  add_floating_object(object);

#if DEBUG
  int  elect_type = object->pin->elect_type;
  int  mech_type  = object->pin->mech_type;
  int  node_type  = object->pin->node_type;
  fprintf(stderr, "PyGeda_new_pin: name=%s, number=%s, elect_type=%d mech_type=%d, node_type=%d\n",
          object->name, object->pin->number, elect_type, mech_type, node_type);
#endif

  return get_pin_object_data(object);
}

/*!
 * \brief Python API Library Create a New Text Object
 * \ingroup Python_API_Create_Functions
 * \par Function Description
 *  This function provides a method to create a new Text Object.
 *
 * \param [in] text         String for the name property
 * \param [in] x            Integer X location
 * \param [in] y            Integer Y location
 *
 * \param [in] size         Integer font size property
 * \param [in] align        Integer alignment property
 * \param [in] angle        Integer orientation property
 * \param [in] py_color     PyObject color
 *
 *  \return [out] GedaPyGedaTextObject contruction data
 *
 *  \note  Optional agruments with a value of -1 will be assigned default values.
 */
PyObject*
PyGeda_new_text( const char *text, int x, int y, int size, int align, int angle, PyObject *py_color)
{
  GedaObject *object;

  int sze = size  < 0 ? DEFAULT_TEXT_SIZE : size;
  int alg = align < 0 ? LOWER_LEFT : align;
  int ang = angle < 0 ? 0 : angle;

  int color = translate_color(py_color, TEXT_COLOR);

  object = geda_text_object_new(color, x, y, alg, ang, sze, VISIBLE, SHOW_NAME_VALUE, text);

  add_floating_object(object);

#if DEBUG
  fprintf(stderr, "PyGeda_new_text: name=%s\n", object->name);
#endif

  return get_text_object_data(object);
}

/** @} END Group Python_API_Create_Functions */

/** \defgroup Python_API_Attribute_Functions  Python API Attribute Functions
 *  @{
 */

/*!
 * \brief Python API Library Create a New Attribute Text Object
 * \ingroup Python_API_Attribute_Functions
 * \par Function Description
 *  This function creates new attribute Object and returns the data to
 *  create a Python version of the Text Object.
 *
 * \param [in] name         String for the name property
 * \param [in] value        String for the Value property
 * \param [in] x            Integer X location
 * \param [in] y            Integer Y location
 *
 * optional: (will be assigned default values if < 0)
 *
 * \param [in] visible      Boolean visibility property
 * \param [in] show         Integer show-name-value property
 * \param [in] align        Integer alignment property
 * \param [in] angle        Integer orientation property
 * \param [in] py_color     PyObject color
 *
 * \return [out] GedaPyGedaTextObject contruction data
 */
PyObject*
PyGeda_new_attrib(const char *name, const char *value, int x, int y,
                  int visible, int show, int align, int angle, PyObject *py_color)
{
  GedaObject *object;
  char       *text;

  int size = DEFAULT_ATTRIBUTE_SIZE;

  int vis = visible < 0 ? INVISIBLE : visible;
  int shw = show    < 0 ? SHOW_NAME_VALUE : show;
  int alg = align   < 0 ? LOWER_LEFT : align;
  int ang = angle   < 0 ? 0 : angle;

  int color = translate_color(py_color, ATTRIBUTE_COLOR);

  text = geda_sprintf("%s=%s", name, value);

  object = geda_text_object_new(color, x, y, alg, ang, size, vis, shw, text);

  add_floating_object(object);

#if DEBUG
  fprintf(stderr, "PyGeda_new_text: name=%s\n", object->name);
#endif

  GEDA_FREE(text);
  return get_text_object_data(object);
}

/*!
 * \brief Python API Library Get Single Attribute Object by Name
 * \ingroup Python_API_Attribute_Functions
 * \par Function Description
 *  This function returns data for a single attribute associated with a given
 *  Gedaobject, when found. The returned data attribute may be attached or
 *  floating.
 *
 * \param [in] py_object    PyGedaObject whose attribute is to be returned
 * \param [in] name         string name of the attribute  to be returned
 *
 * \return [out] GedaPyGedaTextObject data if found or Py_None if an attribute was
 *               not found with the given name.
 */
PyObject *PyGeda_get_attrib(PyObject *py_object, const char *name)
{
  PyGedaObject *geda_object = (PyGedaObject*)py_object;
  int           pid         = geda_object->pid;
  int           sid         = geda_object->sid;
  GedaObject   *parent;
  PyObject     *py_data     = NULL;

  /* Get a pointer to the parent object */
  if (pid < 0) {
    parent = get_floating_object(sid);
  }
  else {

    Page *page;

    page   = geda_toplevel_get_page_by_id(toplevel, pid);
    parent = geda_struct_page_get_object(page, sid);
  }

  if (GEDA_IS_OBJECT(parent)) {

    GedaObject *attrib;

    attrib = geda_find_attrib_by_name (parent->attribs, name, 0);

    if (attrib){

#if DEBUG
      if (attrib->page == NULL && page != NULL) {
        fprintf(stderr, "PyGeda_get_attrib: <%s> child of <%s><%s> was missing pointer to page, pid=%d\n",
                attrib->name, parent->name, parent->complex->filename, pid);
        attrib->page = page;
      }
#endif

      py_data = get_text_object_data(attrib);
    }
    else {

      GList *all_butes = geda_attrib_return_attribs(parent);

      attrib = geda_find_attrib_by_name (all_butes, name, 0);

      if (attrib){

#if DEBUG
        if (attrib->page == NULL && page != NULL) {
          fprintf(stderr, "PyGeda_get_attrib: <%s> child of <%s><%s> was missing pointer to page, pid=%d\n",
                  attrib->name, parent->name, parent->complex->filename, pid);
          attrib->page = page;
        }
#endif

        py_data = get_text_object_data(attrib);
      }
    }
  }
  return py_data;
}

/*!
 * \brief Python API Library Get Attribute Objects
 * \ingroup Python_API_Attribute_Functions
 * \par Function Description
 *  This function provides a medthod to get all of the attributes attached to
 *  given Gedaobject and return a PyList data to contruct the attributes.
 *
 * \param [in] py_object    PyGedaObject whose attributes are to be returned
 *
 * \return [out] PyList of GedaPyGedaTextObject data for each attribute
 *               attached to Object.
 */
PyObject *PyGeda_get_attribs(PyObject *py_object)
{
  PyGedaObject *geda_object = (PyGedaObject*)py_object;
  int           pid         = geda_object->pid;
  int           sid         = geda_object->sid;
  PyObject     *output_list = Py_BuildValue("()");
  GedaObject   *parent;

  /* Get pointer to the parent object */
  if (pid < 0) {
    parent = get_floating_object(sid);
  }
  else {

    Page *page;

    page   = geda_toplevel_get_page_by_id(toplevel, pid);
    parent = geda_struct_page_get_object(page, sid);
  }

  if (GEDA_IS_OBJECT(parent)) {

    GList *attribs = parent->attribs;

    if (attribs){

      GList *iter;

      iter        = g_list_first(attribs);
      output_list = PyList_New(0);

      while(iter != NULL) {

        GedaObject *object;
        PyObject   *data;

        object = iter->data;

#if DEBUG
        if (object->page == NULL && page != NULL) {
          fprintf(stderr, "PyGeda_get_attribs: <%s> child of <%s><%s> was missing pointer to page, pid=%d\n",
                  object->name, parent->name, parent->complex->filename, pid);
        }
#endif
        data = get_text_object_data(object);
        PyList_Append(output_list, data);
        NEXT(iter);
      };
    }
  }

  return output_list;
}

/*!
 * \brief Python API Library Set Attribute Objects Value
 * \ingroup Python_API_Attribute_Functions
 * \par Function Description
 *  This function sets the value of an attribute given the name of the attribute
 *  OR the parent Object. If the Attribute being set is a floating type, then the
 *  attribute is promoted to the parent. If the Attribute does not exist, whether
 *  inherited or not, a new attribute is created and attached to the parent.
 *
 * \param [in] py_complex PyObject Complex parent of the attribute
 * \param [in] py_attrib  PyObject AttributeObject, aka PyGedaTextObject
 * \param [in] name       string name of the attribute to be set (or created)
 * \param [in] value      string value of the attribute
 * \param [in] ret_obj    integer flag, if True, then return attribute data
 *
 * \return GedaPyGedaTextObject construction data if the 4th argument is True,
 *         otherwise Py_None is returned.
 */
PyObject *PyGeda_set_attrib(PyObject *py_complex, PyObject *py_attrib,
                            const char *name, const char *value, int ret_obj)
{
  PyGedaObject *geda_complex = (PyGedaObject*)py_complex;
  PyGedaObject *geda_attrib  = (PyGedaObject*)py_attrib;
  int           pid          = -1;
  int           sid          = -1;
  GedaObject   *object;
  PyObject     *py_data      = NULL;

  if (py_complex) {
    pid = geda_complex->pid;
    sid = geda_complex->sid;
  }
  else if (py_attrib) {
    pid = geda_attrib->pid;
    sid = geda_attrib->sid;
  }
  if (sid != -1) {
    if (pid < 0) {
      object = get_floating_object(sid);
    }
    else {
      Page *page;
      page   = geda_toplevel_get_page_by_id(toplevel, pid);
      object = geda_struct_page_get_object(page, sid);
    }
    if (GEDA_IS_OBJECT(object)) {

      GedaObject *attrib;

      if (py_complex) {
        attrib = geda_find_attrib_by_name (object->attribs, name, 0);
      }
      else {
        attrib = object;
      }
      if (attrib) {
        geda_attrib_object_set_value(attrib, name, value);
      }
      else {
        GList * floaters = geda_attrib_return_attribs(object);
        attrib = geda_find_attrib_by_name (floaters, name, 0);
        if (attrib) {
          geda_attrib_object_set_value(attrib, name, value);
          geda_attrib_object_attach (object, attrib, TRUE);
        }
        else{
          attrib = geda_attrib_object_new_attached(object, name, value, VISIBLE, SHOW_VALUE);
        }
        g_list_free(floaters);
      }
      if (ret_obj) {
        py_data = get_text_object_data(attrib);
      }
      else {
        py_data = Py_BuildValue("");
      }
    }
  }

  return py_data;
}

/*!
 * \brief Python API Library Refresh PyPyGedaObject Attributes
 * \ingroup Python_API_Attribute_Functions
 * \par Function Description
 *  This updates attributes that were modified in Python scripts after a
 *  complex object associates with the attributes was placed (on a page).
 *
 * \param [in] py_object PyObject Complex parent of attributes to be updated
 *
 * \return status True on success, False if an error occurs.
 */
int PyGeda_refresh_attribs(PyObject *py_object)
{
  PyGedaObject *geda_object = (PyGedaObject*)py_object;
  int           pid         = geda_object->pid;
  int           status      = 1;
  Page         *page;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  if (page && (GEDA_IS_PAGE(page))) {

    const GedaObject *object;
    int sid;

    sid    = geda_object->sid;
    object = geda_struct_page_get_object(page, sid);

#if DEBUG
    fprintf(stderr, "PyGeda_refresh_attribs: <%s>\n", object->name);
#endif

    status = PyGeda_update_butes((GedaObject*)object, geda_object);

  } /* else invalid pointer page */
  return status;
}
/** @} END Group Python_API_Attribute_Functions */


/** \defgroup Python_API_Connections  Geda Python Module API Network Methods
 *  @{
 */
static GedaList *get_connected(GedaObject* o_net)
{
  GedaList *network;
  int       index;

  network = geda_list_new();

  /* the current net is the startpoint for the stack */
  geda_list_add(network, o_net);
  index = -1;
  do {

    GList *nets;

    index++;

    GedaObject *o_current = g_list_nth_data (network->glist, index);

    nets =  geda_struct_conn_return_others(NULL, o_current);

    geda_list_add_glist_unique(network, nets);

    g_list_free(nets);

  } while ( index != geda_list_length(network) - 1);

  return network;
}

/*!
 * \brief Python API Library Get Network
 * \ingroup Python_API_Connections
 * \par Function Description
 *  This function use the helper function interconnected to collect all
 *  geometrically connected "conductive" objects; Net, Buses and Pins,
 *  that are connected to the object with the \a sid on page \a pid.
 *
 * \param [in] pid    Integer, the page id of the page to search
 * \param [in] sid    Integer, the sequence id of Object whose connections are to found
 * \param [in] filter Interer, filter to indicate the type of objects to be returned
 *
 * \return PyList of all objects connected to sid, inclusive of the object
 *         itself, unless excluded by the filter, inclusive of the object
 *         itself, unless excluded by the filter.
 */
PyObject *PyGeda_get_network( int pid, int sid, int filter )
{
  GedaList *network;
  Page     *page;
  PyObject *py_list;

  network = NULL;

  page = geda_toplevel_get_page_by_id(toplevel, pid);

  if (sid >= 0) {

    GedaObject *object = geda_struct_page_get_object(page, sid);

    if (!object) {
      object = get_floating_object(sid);
    }

    if (object) {
      network = get_connected(object);
    }
  }

  if (network) {

    GList *list = network->glist;
    GList *tmp  = NULL;

    switch (filter) {
      case GEDA_FILTER_ALL:
        break;

      case GEDA_FILTER_BUS:
        list = geda_object_get_objects_by_type (list, OBJ_BUS);
        break;

      case GEDA_FILTER_NET:
        list = geda_object_get_objects_by_type (list, OBJ_NET);
        break;

      case GEDA_FILTER_PIN:
        list = geda_object_get_objects_by_type (list, OBJ_PIN);
        break;

      case GEDA_FILTER_NET | GEDA_FILTER_BUS:
        list = geda_object_get_objects_by_type (list, OBJ_NET);
        tmp  = geda_object_get_objects_by_type (list, OBJ_BUS);
        list = g_list_concat(list, tmp);
        break;

      case GEDA_FILTER_NET | GEDA_FILTER_PIN:
        list = geda_object_get_objects_by_type (list, OBJ_NET);
        tmp  = geda_object_get_objects_by_type (list, OBJ_PIN);
        list = g_list_concat(list, tmp);
        break;

      case GEDA_FILTER_BUS | GEDA_FILTER_PIN:
        list = geda_object_get_objects_by_type (list, OBJ_BUS);
        tmp  = geda_object_get_objects_by_type (list, OBJ_PIN);
        list = g_list_concat(list, tmp);
        break;

      default:
        filter = GEDA_FILTER_ALL;
        break;

    }

    py_list = PyGeda_glist_2_pylist(list);

    if (filter != GEDA_FILTER_ALL) {
      g_list_free(list);
      if (tmp) {
        g_list_free(tmp);
      }
    }

    GEDA_UNREF(network);
  }
  else {
    py_list = Py_BuildValue("()");
  }

  return py_list;
}

/* flag = 0 return unconnect, flag = 1 return junctions */
static void get_cue_locations_lowlevel(GList *list, PyObject *py_list, int flag)
{
  GArray   *junctions;
  GArray   *noconnects;
  GArray   *results;
  int       index;

  junctions  = g_array_new(FALSE, FALSE, sizeof(GedaPoint));
  noconnects = g_array_new(FALSE, FALSE, sizeof(GedaPoint));

  geda_struct_cue_get_locations (list, junctions, noconnects);

  if (flag) {
    results = junctions;
  }
  else {
    results = noconnects;
  }

  for (index = 0; index<results->len; index++) {

    PyObject  *py_pair;
    GedaPoint *point;

    point   = &g_array_index(results, GedaPoint, index);
    py_pair = Py_BuildValue("ii", point->x, point->y);
    PyList_Append(py_list, py_pair);
  }

  g_array_free(junctions, TRUE);
  g_array_free(noconnects, TRUE);

}

static PyObject *get_cue_locations(PyObject *py_objects, int flag)
{
  PyGedaObject *geda_object = NULL;
  PyObject     *py_list;

  int count;

  if (py_objects) {
    count = (int)PyList_GET_SIZE(py_objects);
  }
  else  {
    count = 0;
  }

  py_list = PyList_New(0);

  if (count > 0) {

    Page *page;
    int   pid;
    int   i;

    GList *list = NULL;
    geda_object = (PyGedaObject*)PyList_GET_ITEM(py_objects, 1);
    pid         = geda_object->pid;

    page = geda_toplevel_get_page_by_id(toplevel, pid);

    for (i = 1; i < count; i++) {

      GedaObject *object;

      geda_object = (PyGedaObject*)PyList_GET_ITEM(py_objects, i);
      object      = geda_struct_page_get_object(page, geda_object->sid);

      if (object) {
        list = g_list_append(list, object);
      }
    }

    count = g_list_length(list);

    if (count > 0) {
      get_cue_locations_lowlevel(list, py_list, flag);
    }
    g_list_free(list);
  }
  return py_list;
}

/*!
 * \brief Python API Library Get Coordinates of Junctions for Objects
 * \ingroup Python_API_Connections
 * \par Function Description
 *  This function provides an API to libgeda to obtain X-Y coordinates
 *  data of where junctions occur, normally where nets cross or meet.
 *  Junctions are used for illustion purposes.
 *
 * \param [in] py_objects The object whose juctions are to be returned
 *
 * \return [out] PyList of order integer pairs representing points
 *               where a junction occurs or an empty list if none
 *               where found.
 *  example:
 *
 *        junctions = geda.get_junctions(network)
 *        for point in junctions:
 *           circle = geda.new_circle(point[0], point[1], JUNCTION_SIZE)
 *           circle.fill_type = FILL_SOLID
 *           geda.add_object(schematic, circle)
 */
PyObject  *PyGeda_get_junctions (PyObject *py_objects)
{
  return get_cue_locations(py_objects, 1);
}

/*!
 * \brief Python API Library Get Coordinates of Unconnnected Objects
 * \ingroup Python_API_Connections
 * \par Function Description
 *  This function provides an API to libgeda to obtain X-Y coordinates
 *  data of unconnected object, normally pins and nets.
 *
 * \param [in] py_objects Must contain only PyGedaObjects to analyzed
 *
 * \return [out] PyList of order integer pairs representing points
 *               where objects are disconnected or an empty list if
 *               none where found.
 */
PyObject *PyGeda_get_unconnected (PyObject *py_objects)
{
  return get_cue_locations(py_objects, 0);
}

/** @} END Group Python_API_Connections */

/** @} END Group Python_API_Library */
