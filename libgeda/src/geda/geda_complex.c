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
 *  \brief Geda Complex Object Class derived from the GedaObject Class
 */
/** \defgroup geda-complex-object Geda Complex Object
 *  @{
 */
/*! \class Complex geda_complex.h "include/libgeda/geda_complex.h"
 *  \implements geda-complex-object
 *  \brief This is an implementaion class for GEDA Complex.
 *  A Geda Complex Object is a graphical symbol used to represent
 *  components or module information and are generally associated
 *  with another file. Information from Complexes, such as ordinary
 *  symbols, can optionally be embedded into schematics.
 */
/** @} endgroup geda-complex-object */

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
int geda_complex_bounds(Object *object)
{
  const GList *iter;
  Object      *sub_object;
  ObjectClass *object_class;

  int result = 0;

  g_return_val_if_fail (GEDA_IS_COMPLEX(object), FALSE);

  /* Find the first object with bounds, and set the bounds variables*/
  iter = object->complex->prim_objs;

  /* Loop thru sub objects, if any exist */
  while (!result && iter != NULL) {

    sub_object = (Object *) iter->data;

    g_return_val_if_fail (GEDA_IS_OBJECT(sub_object), FALSE);

    object_class = (ObjectClass*)G_OBJECT_GET_CLASS(sub_object);
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
      sub_object = (Object *) iter->data;
      object_class = (ObjectClass*)G_OBJECT_GET_CLASS(sub_object);

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

/*! \brief GedaType instance initializer for Complex
 *
 *  \par Function Description
 *  GedaType instance initializer for Complex, initializes a new empty
 *  Complex object by setting pointers to NULL and numbers to zero.
 *
 *  \param [in] instance The Complex structure being initialized,
 *  \param [in] g_class  The Complex class we are initializing.
 */
static void geda_complex_instance_init(GTypeInstance *instance, void *g_class)
{
  Complex *complex      = (Complex*)instance;
  Object  *object       = &complex->parent_instance;

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

/*! \brief Geda Complex Object Finalization Function
 *  \par Function Description
 *   Releases all internal references and releases the memory allocated to
 *   the given Complex data structure and then chain's up to the parent's
 *   finalizer after invalidating the Complex's markers.
 */
static void geda_complex_finalize(GObject *object)
{
  Complex *complex = GEDA_COMPLEX(object);
  Object  *obj     = GEDA_OBJECT(object);

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

/*! \brief GedaType class initializer for Complex
 *
 *  \par Function Description
 *  GedaType class initializer for Complex. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class      The Complex class we are initializing
 *  \param [in]  class_data   The Complex structure associated with the class
 */
static void geda_complex_class_init(void *g_class, void *class_data)
{
  ComplexClass *class          = (ComplexClass*)g_class;
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );
  ObjectClass  *object_class   = GEDA_OBJECT_CLASS( class );

  geda_complex_parent_class    = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_complex_dispose;
  gobject_class->finalize      = geda_complex_finalize;

  object_class->bounds         = geda_complex_bounds;
}

/*! \brief Function to retrieve Complex's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #Complex Type identifier. When first called,
 *  the function registers a #Complex in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Complex and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with Complex.
 */
GedaObjectType geda_complex_get_type (void)
{
  static volatile GedaObjectType geda_complex_type = 0;

  if (g_once_init_enter (&geda_complex_type)) {

    static const GTypeInfo info = {
      sizeof(ComplexClass),
      NULL,                      /* base_init           */
      NULL,                      /* base_finalize       */
      geda_complex_class_init,   /* (GClassInitFunc)    */
      NULL,                      /* class_finalize      */
      NULL,                      /* class_data          */
      sizeof(Complex),
      0,                         /* n_preallocs         */
      geda_complex_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("Complex");
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
Object *geda_complex_new (void)
{
  Object *complex = g_object_new( GEDA_TYPE_COMPLEX,
                                 "type", OBJ_COMPLEX,
                                 "name", "complex",
                                 NULL );
  return GEDA_OBJECT(complex);
}

/*! \brief Determine if object is a Geda Complex Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Complex object.
 *
 *  \return boolean.
 */
bool is_a_geda_complex_object (Complex *cpx)
{
  return GEDA_IS_OBJECT(cpx) && (((Object*)cpx)->type == OBJ_COMPLEX);
}
