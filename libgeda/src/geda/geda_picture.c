/* -*- geda_picture.c -*-
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
/*! \file geda_picture.c
 *  \brief Geda Picture Object Class Module
 */

/** \defgroup geda-picture-object Geda Picture Object
 * @{
 * \brief Implmentation of #GedaPicture Class
 * \par
 *  This module implements GedaPicture Objects in libgeda. A GedaPicture
 *  object is a type of graphical object used to insert images such as
 *  graphs or photos. GedaPicture are derived from the GedaObject base
 *  class.
 *
 * \class GedaPicture geda_picture.h "include/libgeda/geda_picture.h"
 * \implements geda-object
 */

#include "../../../config.h"

#include <libgeda_priv.h>

static GObjectClass *geda_picture_parent_class = NULL;

/*!
 * \brief Get picture bounding rectangle.
 * \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and
 *  <B>bottom</B> parameters to the boundings of the picture object
 *  described in <B>*picture</B>.
 *
 * \param [in]  object     Picture GedaObject to read coordinates from.
 */
static int geda_picture_bounds(GedaObject *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE(object), FALSE);

  object->left   = min(object->picture->upper_x, object->picture->lower_x);
  object->top    = min(object->picture->upper_y, object->picture->lower_y);
  object->right  = max(object->picture->upper_x, object->picture->lower_x);
  object->bottom = max(object->picture->upper_y, object->picture->lower_y);

  object->bounds_valid = TRUE;

  return TRUE;
}

/*!
 * \brief GedaType instance initializer for Picture
 * \par Function Description
 *  GedaType instance initializer for Picture, initializes a new empty
 *  Picture object by setting pointers to NULL and numbers to zero.
 *
 * \param [in] instance The Picture structure being initialized,
 * \param [in]  g_class The Picture class we are initializing.
 */
static void geda_picture_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaPicture *picture  = (GedaPicture*)instance;
  GedaObject  *object   = &picture->parent_instance;

  picture->pixbuf       = NULL;
  picture->file_content = NULL;
  picture->file_length  = 0;

  picture->ratio        = 0.0;
  picture->filename     = NULL;
  picture->angle        = 0;
  picture->mirrored     = 0;
  picture->is_embedded  = 0;

  /* upper is considered the origin, world units */
  picture->upper_x      = 0;
  picture->upper_y      = 0;
  picture->lower_x      = 0;
  picture->lower_y      = 0;

  picture->height       = 0;
  picture->width        = 0;

  object->picture       = picture;
}

static void geda_picture_dispose(GObject *object)
{
  GedaPicture *pic = GEDA_PICTURE(object);

  if (pic->filename) {
    GEDA_FREE(pic->filename);
    pic->filename = NULL;
  }

  G_OBJECT_CLASS(geda_picture_parent_class)->dispose(object);
}

/*!
 * \brief Geda Picture GedaObject Finalization Function
 * \par Function Description
 *  This function removes or releases all internal references and
 *  releases the memory allocated to the given Picture structure,
 *  invalidates the Picture's markers, then chain up to the parent's
 *  finalizer.
 */
static void geda_picture_finalize(GObject *object)
{
  GedaPicture *pic = GEDA_PICTURE(object);
  GedaObject  *obj = GEDA_OBJECT(object);

  if (pic->file_content) {
    GEDA_FREE(pic->file_content);
    pic->file_content = NULL;
  }

  /* The object is no longer a GedaPicture */
  obj->picture = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_picture_parent_class)->finalize(object);
}

/*!
 * \brief GedaType class initializer for GedaPicture
 * \par Function Description
 *  GedaType class initializer for GedaPicture. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  klass       The GedaPicture class we are initializing
 * \param [in]  class_data  The Picture structure associated with the class
 */
static void geda_picture_class_init(void *klass, void *class_data)
{
  GedaPictureClass *class         = (GedaPictureClass*)klass;
  GObjectClass     *gobject_class = (GObjectClass*)klass;
  GedaObjectClass  *geda_class    = (GedaObjectClass*)klass;

  geda_picture_parent_class       = g_type_class_peek_parent(class);

  gobject_class->dispose          = geda_picture_dispose;
  gobject_class->finalize         = geda_picture_finalize;

  geda_class->bounds              = geda_picture_bounds;
}

/*!
 * \brief Function to retrieve Picture Type identifier.
 * \par Function Description
 *  Function to retrieve a #Picture Type identifier. When first called,
 *  the function registers a #Picture in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Picture and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 * \return GedaObjectType identifier associated with Picture.
 */
GedaObjectType geda_picture_get_type (void)
{
  static GedaObjectType geda_picture_type = 0;

  if (g_once_init_enter (&geda_picture_type)) {

    static const GTypeInfo info = {
      sizeof(GedaPictureClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_picture_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(GedaPicture),
      0,                      /* n_preallocs         */
      geda_picture_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaPicture");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_picture_type, type);
  }

  return geda_picture_type;
}

/*!
 * \brief Returns a pointer to a new Picture object.
 * \par Function Description
 *  Returns a pointer to a new Picture object.
 *
 * \return pointer to the new Picture object.
 */
GedaObject *geda_picture_new (void)
{
  GedaObject *picture = g_object_new( GEDA_TYPE_PICTURE,
                                 "type", OBJ_PICTURE,
                                 "name", "picture",
                                 NULL );
  return GEDA_OBJECT(picture);
}

/*!
 * \brief Determine if object is a Geda Picture GedaObject.
 * \par Function Description
 *  Returns true if the argument is a Geda Picture object.
 *
 * \return boolean.
 */
bool is_a_geda_picture (const GedaPicture *pic)
{
  return GEDA_IS_OBJECT(pic) && (((GedaObject*)pic)->type == OBJ_PICTURE);
}

/*!
 * \brief Retrieve the Angle of a GedaPicture
 * \par Function Description
 *  Returns the value of the angle member or -0 if \a pic
 *  is not a valid GedaPicture object.
 *
 * \return integer angle.
 */
int geda_picture_get_angle (const GedaPicture *pic)
{
  if (is_a_geda_picture(pic)) {
    return pic->angle;
  }
  return -0;
}

/*!
 * \brief Retrieve the As Loaded Height of a GedaPicture
 * \par Function Description
 *  Returns the value of the height member or -0 if \a pic
 *  is not a valid GedaPicture object.
 *
 * \return integer height.
 */
int geda_picture_get_height (const GedaPicture *pic)
{
  if (is_a_geda_picture(pic)) {
    return pic->height;
  }
  return -0;
}

/*!
 * \brief Get if Picture is Embedded
 * \par Function Description
 *  Returns the value of the is_embedded flag or -0 if \a pic
 *  is not a valid GedaPicture object.
 */
bool geda_picture_get_is_embedded (const GedaPicture *pic)
{
  if (is_a_geda_picture(pic)) {
    return pic->is_embedded;
  }
  return -0;
}

/*!
 * \brief Set the Angle of a GedaPicture
 * \par Function Description
 *  Low-level setter to set the value of the angle property
 *  if \a pic is a valid GedaPicture object. Silently ignores
 *  error if \a pic is not a valid GedaPicture.
 */
void geda_picture_set_angle (GedaPicture *pic, int angle)
{
  if (is_a_geda_picture(pic)) {
    pic->angle = angle;
  }
}

void geda_picture_set_height (GedaPicture *pic, int height)
{
  if (is_a_geda_picture(pic)) {
    pic->height = height;
  }
}

/*!
 * \brief Set GedaPicture is_embedded Flag
 * \par Function Description
 *  Low-level setter to set the is_embedded property if \a pic
 *  is a valid GedaPicture object. Setting this flag does not
 *  embed the picture. Silently ignores error if \a pic is not
 *  a valid GedaPicture.
 *
 * \sa geda_object_embed geda_object_unembed
 */
void geda_picture_set_is_embedded (GedaPicture *pic, bool is_embedded)
{
  if (is_a_geda_picture(pic)) {
    pic->is_embedded = is_embedded;
  }
}

/** @} endgroup geda-picture-object */
