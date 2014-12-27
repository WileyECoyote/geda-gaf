/* -*- geda_picture.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2014 Ales Hvezda
 * Copyright (C) 2013-2014 Wiley Edward Hill
 *
 * Copyright (C) 2013-2014 gEDA Contributors (see ChangeLog for details)
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
 *  Date Contributed: November, 18, 2013
 */
/*! \file geda_picture.c
 *  \brief Geda Picture Object Class derived from the GedaObject Class
 */
/** \defgroup geda-picture-object Geda Picture Object
 *  @{
 */
/*! \class Picture geda_picture.h "include/libgeda/geda_picture.h"
 *  \implements geda-object
 *  \brief This is an implementaion class for GEDA Picture Objects.
 *  A Geda Picture Object is a type graphical object used to insert images
 *  such as graphs or photos.
 */

#include <config.h>

#include "libgeda_priv.h"

G_DEFINE_TYPE (Picture, geda_picture, GEDA_TYPE_OBJECT);

/*! \brief Get picture bounding rectangle in WORLD coordinates.
 *  \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and
 *  <B>bottom</B> parameters to the boundings of the picture object
 *  described in <B>*picture</B> in WORLD units.
 *
 *  \param [in]  object     Picture Object to read coordinates from.
 */
int
geda_picture_bounds(Object *object)
{
  g_return_val_if_fail (GEDA_IS_PICTURE(object), FALSE);

  object->left   = min(object->picture->upper_x, object->picture->lower_x);
  object->top    = min(object->picture->upper_y, object->picture->lower_y);
  object->right  = max(object->picture->upper_x, object->picture->lower_x);
  object->bottom = max(object->picture->upper_y, object->picture->lower_y);

  return TRUE;
}

/*! \brief GedaType instance initialiser for Picture
 *
 *  \par Function Description
 *  GedaType instance initialiser for Picture, initializes a new empty
 *  Picture object by setting pointers to NULL and numbers to zero,
 *  the picture PID variable is set to the next picture index.
 *
 *  \param [in]  picture  The Picture instance being initialising.
 */
static void geda_picture_init(Picture *picture)
{
  Object *object        = &picture->parent_instance;

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

  object->picture       = picture;

  picture->head_marker  = GEDA_TYPE_PICTURE;
  picture->tail_marker  = picture->head_marker;
}

static void
geda_picture_dispose(GObject *object)
{
  Picture *pic = GEDA_PICTURE(object);
  if (pic->filename) {
    GEDA_FREE(pic->filename);
    pic->filename = NULL;
  }
  G_OBJECT_CLASS(geda_picture_parent_class)->dispose(object);

}

/*! \brief Geda Picture Object Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references
 *   and releases the memory allocated to the given Picture
 *   data structure and then chain up to the parent's finalize
 *   handler.
 */
static void geda_picture_finalize(GObject *object)
{
  //Object *obj = GEDA_OBJECT(object);
  GEDA_OBJECT_CLASS( geda_picture_parent_class )->finalize(object);

}

/*! \brief GedaType class initialiser for Picture
 *
 *  \par Function Description
 *  GedaType class initialiser for Picture. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  class       The Picture we are initialising
 */
static void geda_picture_class_init(PictureClass *class)
{
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );
  ObjectClass  *object_class   = GEDA_OBJECT_CLASS( class );

  geda_picture_parent_class    = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_picture_dispose;
  gobject_class->finalize      = geda_picture_finalize;

  object_class->bounds         = geda_picture_bounds;
}

/*! \brief Returns a pointer to a new Picture object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Picture object.
 *
 *  \return pointer to the new Picture object.
 */
Object *geda_picture_new (void)
{
  Object *picture = g_object_new( GEDA_TYPE_PICTURE,
                                 "type", OBJ_PICTURE,
                                 "name", "picture",
                                 NULL );
  return GEDA_OBJECT(picture);
}

/*! \brief Determine if object is a Geda Picture Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Picture object.
 *
 *  \return boolean.
 */
bool is_a_geda_picture_object (Picture *pic)
{
  return GEDA_IS_OBJECT(pic) && (GEDA_TYPE_PICTURE == (pic->head_marker & pic->tail_marker));
}
/** @} endgroup geda-picture-object */
