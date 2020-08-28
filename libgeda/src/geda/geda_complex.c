/* -*- geda_complex.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2015 Ales Hvezda
 * Copyright (C) 2013-2015 Wiley Edward Hill
 *
 * Copyright (C) 2013-2015 gEDA Contributors (see ChangeLog for details)
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 18, 2013
 */
/*! \file geda_complex.c
 *  \brief Geda Complex Object Class Module
 */

/** \defgroup geda-complex-object Geda Complex Object
 * @{
 * \brief Implmentation of #GedaComplex Class
 * \par
 *  A Geda Complex Object is a graphical symbol used to represent
 *  components or module information and are generally associated
 *  with another file. Information from Complexes, such as ordinary
 *  symbols, can optionally be embedded into schematics. A Complex
 *  is derived from the GedaObject base class.
 *
 * \class GedaComplex geda_complex.h "include/libgeda/geda_complex.h"
 * \implements geda-object
 */

#include "../../../config.h"

#include <libgeda_priv.h>

static GObjectClass *geda_complex_parent_class = NULL;

/*!
 * \brief Return the bounds of the given GedaComplex.
 * \par Given a list of objects, calculates the bounds coordinates.
 *
 * \param [in]  object The complex object whose bounds are to be determined.
 *
 * \return If any bounds were found for the list of objects
 * \retval 0 No bounds were found
 * \retval 1 Bound was found
 */
static int geda_complex_bounds(GedaObject *object)
{
  const GList     *iter;
  GedaObject      *sub_object;
  GedaObjectClass *object_class;

  int result = 0;

  g_return_val_if_fail (GEDA_IS_COMPLEX(object), FALSE);

  /* Find the first object with bounds, and set the bounds variables*/
  iter = object->complex->prim_objs;

  /* Loop thru sub objects, if any exist */
  while (!result && iter != NULL) {

    sub_object = (GedaObject *) iter->data;

    g_return_val_if_fail (GEDA_IS_OBJECT(sub_object), FALSE);

    object_class = (GedaObjectClass*)G_OBJECT_GET_CLASS(sub_object);
    result       = object_class->bounds(sub_object);

    NEXT(iter);
  }

  if (result) {

    int left,  top,  right,  bottom;

    left   = sub_object->left;
    top    = sub_object->top;
    right  = sub_object->right;
    bottom = sub_object->bottom;

    /* Check other objects with bounds and expand as necessary */
    while ( iter != NULL ) {

      sub_object   = (GedaObject*)iter->data;
      object_class = (GedaObjectClass*)G_OBJECT_GET_CLASS(sub_object);

      if (object_class->bounds(sub_object)) {
        left   = min( left,   sub_object->left );
        top    = min( top,    sub_object->top );
        right  = max( right,  sub_object->right );
        bottom = max( bottom, sub_object->bottom );
      }

      NEXT(iter);
    }

    /* Set bounds of the parent complex object */
    object->left   = left;
    object->top    = top;
    object->right  = right;
    object->bottom = bottom;

  }

  object->bounds_valid = TRUE;

  return result;
}

/*!
 * \brief GedaType instance initializer for GedaComplex
 * \par Function Description
 *  GedaType instance initializer for GedaComplex, initializes a new
 *  empty GedaComplex object by setting pointers to NULL and numbers
 *  to zero.
 *
 * \param [in] instance The GedaComplex structure being initialized,
 * \param [in] g_class  The GedaComplex class being initializing.
 */
static void geda_complex_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaComplex *complex  = (GedaComplex*)instance;
  GedaObject  *object   = &complex->parent_instance;

  complex->filename     = NULL;
  complex->is_embedded  = FALSE;  /* is embedded component? */

  complex->x            = 0;      /* world origin */
  complex->y            = 0;

  complex->angle        = 0;      /* orientation in degrees */
  complex->mirror       = 0;

  complex->pin_objs     = NULL;
  complex->prim_objs    = NULL;

  object->complex       = complex;
}

static void geda_complex_dispose(GObject *object)
{
  ((GObjectClass*)geda_complex_parent_class)->dispose(object);
}

/*!
 * \brief Geda Complex GedaObject Finalization Function
 * \par Function Description
 *  Releases all internal references and releases the memory allocated to
 *  the given Complex data structure and then chain's up to the parent's
 *  finalizer after invalidating the Complex.
 */
static void geda_complex_finalize(GObject *object)
{
  GedaComplex *complex = GEDA_COMPLEX(object);
  GedaObject  *obj     = GEDA_OBJECT(object);

  if (complex->filename) {
    GEDA_FREE(complex->filename);
  }

  if (complex->pin_objs) {
    g_list_free (complex->pin_objs);
    complex->pin_objs = NULL;
  }

  if (complex->prim_objs) {
    g_list_free (complex->prim_objs);
    complex->prim_objs = NULL;
  }

  /* The object is no longer a GedaComplex */
  obj->complex = NULL;

  /* Finialize the parent GedaObject Class */
  ((GedaObjectClass*)geda_complex_parent_class)->finalize(object);
}

/*!
 * \brief GedaType class initializer for GedaComplex
 * \par Function Description
 *  GedaType class initializer for GedaComplex. We override parents
 *  virtual class methods as needed and register GObject signals.
 *
 * \param [in]  klass       The GedaComplex class we are initializing
 * \param [in]  class_data  The Complex structure associated with the class
 */
static void geda_complex_class_init(void *klass, void *class_data)
{
  GedaComplexClass *class         = (GedaComplexClass*)klass;
  GObjectClass     *gobject_class = (GObjectClass*)class;
  GedaObjectClass  *geda_class    = (GedaObjectClass*)class;

  geda_complex_parent_class       = g_type_class_peek_parent(class);

  gobject_class->dispose          = geda_complex_dispose;
  gobject_class->finalize         = geda_complex_finalize;

  geda_class->bounds              = geda_complex_bounds;
}

/*!
 * \brief Function to retrieve Complex's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaComplex Type identifier. When first called,
 *  the function registers a #GedaComplex in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Complex and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 * \return GedaObjectType identifier associated with GedaComplex.
 */
GedaObjectType geda_complex_get_type (void)
{
  static volatile GedaObjectType geda_complex_type = 0;

  if (g_once_init_enter (&geda_complex_type)) {

    static const GTypeInfo info = {
      sizeof(GedaComplexClass),
      NULL,                      /* base_init           */
      NULL,                      /* base_finalize       */
      geda_complex_class_init,   /* (GClassInitFunc)    */
      NULL,                      /* class_finalize      */
      NULL,                      /* class_data          */
      sizeof(GedaComplex),
      0,                         /* n_preallocs         */
      geda_complex_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaComplex");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_complex_type, type);
  }

  return geda_complex_type;
}

/*!
 * \brief Determine if object is a Geda Complex GedaObject.
 * \par Function Description
 *  Returns true if the argument is a GedaComplex.
 *
 * \return boolean.
 */
bool is_a_geda_complex (const GedaComplex *cpx)
{
 if (GEDA_IS_OBJECT (cpx)) {
   GedaObject *obj = (GedaObject*)cpx;
   return (obj->type == OBJ_COMPLEX || obj->type == OBJ_PLACEHOLDER);
 }

 return FALSE;

}

/*!
 * \brief Returns a pointer to a new Complex object.
 * \par Function Description
 *  Returns a pointer to a new Complex object.
 *
 * \return pointer to the new Complex object.
 */
GedaObject *geda_complex_new (void)
{
  GedaObject *complex = g_object_new( GEDA_TYPE_COMPLEX,
                                 "type", OBJ_COMPLEX,
                                 "name", "complex",
                                 NULL );
  return GEDA_OBJECT(complex);
}

/*!
 * \brief Appends an Object to a Complex object
 * \par Function Description
 *  Appends \a object to the complex's list of prim_objs and sets the
 *  parent_object member of \a object to that of the complex's parent
 *  object. If \a object is a pin object then the pin is also appended
 *  to the complex's list of pin_objs.
 *
 * \note This method is for special purposes and probably not what you
 *       want! Object appended using this method are not saved with the
 *       schematic! To attach attributes to a complex, see function
 *       geda_attrib_object_attach!
 *
 * \return pointer to the new Complex object.
 */
bool geda_complex_append (GedaComplex *complex, GedaObject *object)
{
 if (is_a_geda_complex (complex)) {

   if (GEDA_IS_OBJECT (object) && !GEDA_IS_COMPLEX (object)) {

     if (GEDA_IS_PIN(object)) {
       complex->pin_objs = g_list_append(complex->pin_objs, object);
     }

     complex->prim_objs = g_list_append (complex->prim_objs, object);
     object->parent_object = &complex->parent_instance;
     return TRUE;
   }
 }

 return FALSE;
}

/*!
 * \brief Retrieve the Angle of a GedaComplex
 * \par Function Description
 *  Returns the value of the angle member or -0 if \a complex
 *  is not a valid GedaComplex object.
 */
int geda_complex_get_angle (const GedaComplex *complex)
{
  if (is_a_geda_complex(complex)) {
    return complex->angle;
  }
  return -0;
}

/*!
 * \brief Retrieve the Filename of a GedaComplex
 * \par Function Description
 *  Returns a pointer to the filename member of \a complex or NULL
 *  if \a complex is not a valid GedaComplex object. The returned
 *  string is owned by Libgeda and should not be freed.
 */
char *geda_complex_get_filename (const GedaComplex *complex)
{
  if (is_a_geda_complex(complex)) {
    return complex->filename;
  }
  return NULL;
}

/*!
 * \brief Get the Is Embedded Flag of a GedaComplex
 * \par Function Description
 *  Returns the value of the is_embedded member or FALSE
 *  if \a complex is not a valid GedaComplex object.
 */
bool geda_complex_get_is_embedded (const GedaComplex *complex)
{
  if (is_a_geda_complex(complex)) {
    return complex->is_embedded;
  }
  return FALSE;
}

/*!
 * \brief Get the mirror Property of the GedaComplex
 * \par Function Description
 *  Retrieves the mirror property of the complex.
 *
 * \sa geda_complex_set_is_mirror
 */
bool geda_complex_get_is_mirror (const GedaComplex *complex)
{
  if (is_a_geda_complex(complex)) {
    return complex->mirror;
  }
  return FALSE;
}

/*!
 * \brief Get the list of Pins from a GedaComplex object
 * \par Function Description
 *  Retrieves the pin_objs member. The pin_objs member is a double
 *  linked list of pin objects associated with complex.
 */
GList *geda_complex_get_pin_objs (const GedaComplex *complex)
{
  if (is_a_geda_complex(complex)) {
    return complex->pin_objs;
  }
  return NULL;
}

/*!
 * \brief Get the list of Pins from a GedaComplex object
 * \par Function Description
 *  Retrieves the prim_objs member. The prim_objs member is a double
 *  linked list of non-complex objects that make-up the complex.
 */
GList *geda_complex_get_prim_objs (const GedaComplex *complex)
{
  if (is_a_geda_complex(complex)) {
    return complex->prim_objs;
  }
  return NULL;
}

/*!
 * \brief Get the X insertion point of a GedaComplex
 * \par Function Description
 *  Retrieves the X insertion point of \a complex.
 */
int geda_complex_get_x (const GedaComplex *complex)
{
  if (is_a_geda_complex(complex)) {
    return complex->x;
  }
  return -0;
}

/*!
 * \brief Get the Y insertion point of a GedaComplex
 * \par Function Description
 *  Retrieves the Y insertion point of \a complex.
 */
int geda_complex_get_y (const GedaComplex *complex)
{
  if (is_a_geda_complex(complex)) {
    return complex->y;
  }
  return -0;
}

/*!
 * \brief Set the Angle of a GedaComplex
 * \par Function Description
 *  Sets the angle member to unchecked integer value of \a angle.
 */
void geda_complex_set_angle (GedaComplex *complex, int angle)
{
  if (is_a_geda_complex(complex)) {
    complex->angle = angle;
  }
}

/*!
 * \brief Set the Filename of a GedaComplex
 * \par Function Description
 *  Low level function to set the filename of the complex.
 */
void geda_complex_set_filename (GedaComplex *complex, const char *filename)
{
  if (is_a_geda_complex(complex)) {
    if (complex->filename) {
      GEDA_FREE(complex->filename);
    }
    complex->filename = geda_strdup(filename);
  }
}

/*!
 * \brief Set is_embedded Flag in GedaComplex
 * \par Function Description
 *  Sets the is_embedded property of the complex to the value
 *  of \a is_embedded. Setting the property does not result in
 *  the object being is_embedded.
 */
void geda_complex_set_is_embedded (GedaComplex *complex, bool is_embedded)
{
  if (is_a_geda_complex(complex)) {
    complex->is_embedded = is_embedded;
  }
}

/*!
 * \brief Set the mirror Property of a GedaComplex
 * \par Function Description
 *  Sets the mirror property of the complex to the value of \a is_mirror.
 *  Setting the property result in the object being mirrored when drawn.
 *
 * \note The value is not checked.
 *
 * \sa geda_complex_get_is_mirror
 */
void geda_complex_set_is_mirror (GedaComplex *complex, bool is_mirror)
{
  if (is_a_geda_complex(complex)) {
    complex->mirror = is_mirror;
  }
}

/*!
 * \brief Set GedaComplex list of Pin objects
 * \par Function Description
 *  Sets the pin_objs member.
 *
 * \param [in] complex   GedaComplex object.
 * \param [in] pin_objs  List of GedaPin objects.
 */
void geda_complex_set_pin_objs (GedaComplex *complex, GList *pin_objs)
{
  if (is_a_geda_complex(complex)) {
    if (complex->pin_objs) {
      g_list_free (complex->pin_objs);
    }
    complex->pin_objs = pin_objs;
  }
}

/*!
 * \brief Set GedaComplex list of primary objects
 * \par Function Description
 *  Sets the prim_objs member. The primary objects are the non-complex
 *  objects that make-up the complex.
 *
 * \param [in] complex   GedaComplex object.
 * \param [in] prim_objs List of GedaObjects.
 */
void geda_complex_set_prim_objs (GedaComplex *complex, GList *prim_objs)
{
  if (is_a_geda_complex(complex)) {
    if (complex->prim_objs) {
      g_list_free (complex->prim_objs);
    }
    complex->prim_objs = prim_objs;
  }
}

/*!
 * \brief Set the X coordinate value of a GedaComplex
 * \par Function Description
 *  Sets the X coordinate \a complex if \a complex is a valid
 *  GedaComplex, if \a complex is invalid then nothing is done.
 *
 * \sa geda_complex_set_y
 */
void geda_complex_set_x (GedaComplex *complex, int x)
{
  if (is_a_geda_complex(complex)) {
    complex->x = x;
  }
}

/*!
 * \brief Set the Y coordinate value of a GedaComplex
 * \par Function Description
 *  Sets the Y coordinate \a complex if \a complex is a valid
 *  GedaComplex, if \a complex is invalid then nothing is done.
 *
 * \sa geda_complex_set_x
 */
void geda_complex_set_y (GedaComplex *complex, int y)
{
  if (is_a_geda_complex(complex)) {
    complex->y = y;
  }
}

/** @} endgroup geda-complex-object */
