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

#include <config.h>

#include <libgeda_priv.h>

static GObjectClass *geda_complex_parent_class = NULL;

/*! \brief Return the bounds of the given GList of objects.
 *
 *  \par Given a list of objects, calculates the bounds coordinates.
 *
 *  \param [in]  object The complex object whose bounds are to be determined.

 *  \return If any bounds were found for the list of objects
 *  \retval 0 No bounds were found
 *  \retval 1 Bound was found
 */
int geda_complex_bounds(GedaObject *object)
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
    result = object_class->bounds(sub_object);

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
      sub_object   = (GedaObject *) iter->data;
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

  return result;
}

/*! \brief GedaType instance initializer for GedaComplex
 *
 *  \par Function Description
 *  GedaType instance initializer for GedaComplex, initializes a new
 *  empty GedaComplex object by setting pointers to NULL and numbers
 *  to zero.
 *
 *  \param [in] instance The GedaComplex structure being initialized,
 *  \param [in] g_class  The GedaComplex class being initializing.
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

static void
geda_complex_dispose(GObject *object)
{
  G_OBJECT_CLASS(geda_complex_parent_class)->dispose(object);
}

/*! \brief Geda Complex GedaObject Finalization Function
 *  \par Function Description
 *   Releases all internal references and releases the memory allocated to
 *   the given Complex data structure and then chain's up to the parent's
 *   finalizer after invalidating the Complex.
 */
static void geda_complex_finalize(GObject *object)
{
  GedaComplex *complex = GEDA_COMPLEX(object);
  GedaObject  *obj     = GEDA_OBJECT(object);

  if (complex->filename)
    GEDA_FREE(complex->filename);

  if (complex->pin_objs) {
    g_list_free (complex->pin_objs);
  }
  complex->pin_objs = NULL;

  if (complex->prim_objs) {
    g_list_free (complex->prim_objs);
  }
  complex->prim_objs = NULL;

  /* The object is no longer a GedaComplex */
  obj->complex = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_complex_parent_class)->finalize(object);
}

/*! \brief GedaType class initializer for GedaComplex
 *
 *  \par Function Description
 *  GedaType class initializer for GedaComplex. We override parents
 *  virtual class methods as needed and register GObject signals.
 *
 *  \param [in]  g_class      The GedaComplex class we are initializing
 *  \param [in]  class_data   The Complex structure associated with the class
 */
static void geda_complex_class_init(void *g_class, void *class_data)
{
  GedaComplexClass*class         = (GedaComplexClass*)g_class;
  GObjectClass    *gobject_class = G_OBJECT_CLASS(class);
  GedaObjectClass *geda_class    = GEDA_OBJECT_CLASS(class);

  geda_complex_parent_class      = g_type_class_peek_parent(class);

  gobject_class->dispose         = geda_complex_dispose;
  gobject_class->finalize        = geda_complex_finalize;

  geda_class->bounds             = geda_complex_bounds;
}

/*! \brief Function to retrieve Complex's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaComplex Type identifier. When first called,
 *  the function registers a #GedaComplex in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Complex and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with GedaComplex.
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

/*! \brief Returns a pointer to a new Complex object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Complex object.
 *
 *  \return pointer to the new Complex object.
 */
GedaObject *geda_complex_new (void)
{
  GedaObject *complex = g_object_new( GEDA_TYPE_COMPLEX,
                                 "type", OBJ_COMPLEX,
                                 "name", "complex",
                                 NULL );
  return GEDA_OBJECT(complex);
}

/*! \brief Determine if object is a Geda Complex GedaObject.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Complex object.
 *
 *  \return boolean.
 */
bool is_a_geda_complex_object (GedaComplex *cpx)
{
 if (GEDA_IS_OBJECT (cpx)) {
   GedaObject *obj = (GedaObject*)cpx;
   return (obj->type == OBJ_COMPLEX || obj->type == OBJ_PLACEHOLDER);
 }

 return FALSE;

}

/** @} endgroup geda-complex-object */
