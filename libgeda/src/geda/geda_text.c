/* -*- geda_text.c -*-
 *
 * gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 *
 * Copyright (C) 2013-2014 Wiley Edward Hill
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 18, 2013
 */
/*! \file geda_text.c
 *  \brief Geda Text Object Class Module
 */

/** \defgroup geda-text-object Geda Text Object
 * @{
 * \brief Implmentation of #GedaText Class
 * \par
 *  This module implements GedaText Objects in libgeda. A GedaText Object
 *  is a type of graphical object that does not directly involve electrical
 *  interconnections. However, text with strings in the form of name=value
 *  are considered attributes, which may be used to describe the electrical
 *  properties of another object. GedaText are derived from the GedaObject
 *  class.
 *
 * \class GedaText geda_text.h "include/libgeda/geda_text.h"
 * \implements geda-object
 */

#include "../../../config.h"

#include <libgeda_priv.h>

static GObjectClass *geda_text_parent_class = NULL;

/*!
 * \brief Calculate and Return the Boundaries of a text object
 * \par Function Description
 *  This function attempts to call the bounds functions to calculate the
 *  object boundaries of a text object.
 *
 *  The object's bounds function is checked first, if the object's bounds
 *  function is not set then the object's page and Toplevel are checked.
 *  If either the Page or the Toplevel bounds function can determine the
 *  bounds the object's bounds function is set to the function that was
 *  successful.
 *
 * \param [in]  o_current a text object
 */
static int geda_text_bounds(GedaObject *o_current)
{
  /* Static was assigned by geda_text_class_init */
  GedaText *text = (GedaText*)o_current;

  int left   = 0;
  int top    = 0;
  int right  = 0;
  int bottom = 0;
  int result = 0;

  /* First check the text if Text object has a func */
  if (text->rendered_text_bounds_func != NULL) {
    result =
    text->rendered_text_bounds_func (o_current->text->rendered_text_bounds_data,
                                     o_current, &left, &top, &right, &bottom);
  }
  else {

    /* Text object must be associated with page */
    if (GEDA_IS_PAGE(o_current->page)) {

      Page *page = o_current->page;

      /* Check if page level render func is set */
      if (page->rendered_text_bounds_func != NULL) {

        result =
        page->rendered_text_bounds_func (page->rendered_text_bounds_data,
                                         o_current, &left, &top, &right, &bottom);

        if (result && !text->rendered_text_bounds_func) {
          text->rendered_text_bounds_func = page->rendered_text_bounds_func;
          text->rendered_text_bounds_data = page->rendered_text_bounds_data;
        }
      }
      else {

        g_return_val_if_fail(GEDA_IS_TOPLEVEL(page->toplevel), FALSE);

        GedaToplevel *toplevel = page->toplevel;

        /* Check if toplevel render func is set */
        if (toplevel->rendered_text_bounds_func != NULL) {

          result =
          toplevel->rendered_text_bounds_func(toplevel->rendered_text_bounds_data,
                                              o_current, &left, &top, &right, &bottom);
          if (result && !text->rendered_text_bounds_func) {
            text->rendered_text_bounds_func = toplevel->rendered_text_bounds_func;
            text->rendered_text_bounds_data = toplevel->rendered_text_bounds_data;
          }
        }
      }
    }
  }

  if (result) {
    o_current->left   = left;
    o_current->top    = top;
    o_current->right  = right;
    o_current->bottom = bottom;

    o_current->bounds_valid = TRUE;
  }

  return result;
}

/*!
 * \brief GedaType instance initializer for GedaText
 * \par Function Description
 *  GedaType instance initializer for GedaText, initializes a new empty
 *  GedaText object by setting pointers to NULL and numbers to zero.
 *
 * \param [in] instance The GedaText structure being initialized,
 * \param [in] g_class  The GedaText class we are initializing.
 */
static void geda_text_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaText   *text    = (GedaText*)instance;
  GedaObject *object  = &text->parent_instance;

  text->x             = 0; /* world origin */
  text->y             = 0;

  text->string        = NULL;
  text->disp_string   = NULL;

  text->length        = 0;
  text->size          = 0;
  text->alignment     = 0;
  text->angle         = 0;

  text->rendered_text_bounds_func  = NULL;
  text->rendered_text_bounds_data  = NULL;

  object->text                     = text;
  object->visibility               = INVISIBLE;
}

static void geda_text_dispose(GObject *object)
{
  GedaText *text = GEDA_TEXT(object);

  GEDA_FREE(text->string);
  GEDA_FREE(text->disp_string);

  G_OBJECT_CLASS(geda_text_parent_class)->dispose(object);

}

/*!
 * \brief GedaText Object Finalization Function
 * \par Function Description
 *  This function invalidates the GedaText object and then chains up to
 *  the parent's finalize handler. Once invalidated, GEDA_IS_TEXT will
 *  fail.
 */
static void geda_text_finalize(GObject *object)
{
  GedaObject *obj = GEDA_OBJECT(object);

  /* The object is no longer a GedaText */
  obj->text = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_text_parent_class)->finalize(object);
}

/*!
 * \brief GedaType class initializer for GedaText
 * \par Function Description
 *  GedaType class initializer for GedaText. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 * \param [in]  klass       The GedaText class we are initializing
 * \param [in]  class_data  The structure associated with the class
 */
static void geda_text_class_init(void *klass, void *class_data)
{
  GedaTextClass   *class         = (GedaTextClass*)klass;
  GObjectClass    *gobject_class = (GObjectClass*)klass;
  GedaObjectClass *geda_class    = (GedaObjectClass*)klass;

  geda_text_parent_class         = g_type_class_peek_parent(class);

  gobject_class->dispose         = geda_text_dispose;
  gobject_class->finalize        = geda_text_finalize;

  geda_class->bounds             = geda_text_bounds;
}

/*!
 * \brief Function to retrieve GedaText's Object Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaText Type identifier. When first called,
 *  the function registers a #GedaText in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Text and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 * \return GedaObjectType identifier associated with GedaText.
 */
GedaObjectType geda_text_get_type (void)
{
  static volatile GedaObjectType geda_text_type = 0;

  if (g_once_init_enter (&geda_text_type)) {

    static const GTypeInfo info = {
      sizeof(GedaTextClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_text_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(GedaText),
      0,                      /* n_preallocs         */
      geda_text_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("GedaText");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_text_type, type);
  }

  return geda_text_type;
}

/*!
 * \brief Returns a pointer to a new GedaText Object.
 * \par Function Description
 *  Returns a pointer to a new GedaText object.
 *
 * \return pointer to the new GedaText object.
 */
GedaObject *geda_text_new (void)
{
  GedaObject *text_object = g_object_new(GEDA_TYPE_TEXT,
                                         "type", OBJ_TEXT,
                                         "name", "text",
                                         NULL );
  return text_object;
}

/*!
 * \brief Determine if object is a Geda Text Object.
 * \par Function Description
 *  Returns true if the argument is a Geda Text object.
 *
 * \return boolean.
 */
bool is_a_geda_text (const GedaText *txt)
{
  return GEDA_IS_OBJECT(txt) && (((GedaObject*)txt)->type == OBJ_TEXT);
}

/*!
 * \brief Retrieve GedaText Alignment Property
 * \par Function Description
 *  Returns the value of \a txt alignment if and only if \a txt is
 *  a valid GedaText object.
 *
 * \return integer value of text alignment or LOWER_LEFT if \a txt is invalid.
 */
int geda_text_get_alignment (const GedaText *txt)
{
  return is_a_geda_text(txt) ? txt->alignment : LOWER_LEFT;
}

/*!
 * \brief Retrieve GedaText Angle Property
 * \par Function Description
 *  Returns the value of \a txt angle if and only if \a txt is
 *  a valid GedaText object.
 *
 * \return integer value of text angle or -0 if \a txt is invalid.
 */
int geda_text_get_angle (const GedaText *txt)
{
  return is_a_geda_text(txt) ? txt->angle : -0;
}

/*!
 * \brief Retrieve the GedaText Display String Property
 * \par Function Description
 *  Returns the value of \a txt string to be displayed if and only
 *  if \a txt is a valid GedaText object. The returned string is
 *  owned by the object and should not be freed.
 *
 * \return string or the NULL if \a txt is invalid.
 */
const char *geda_text_get_disp_string (const GedaText *txt)
{
  return is_a_geda_text(txt) ? txt->disp_string : NULL;
}

/*!
 * \brief Retrieve GedaText Size Property
 * \par Function Description
 *  Returns the value of \a txt size if and only if \a txt is
 *  a valid GedaText object.
 *
 * \return integer value of text size or the default size if \a txt is invalid.
 */
int geda_text_get_size (const GedaText *txt)
{
  return is_a_geda_text(txt) ? txt->size : DEFAULT_TEXT_SIZE;
}

/*!
 * \brief Retrieve the GedaText String Property
 * \par Function Description
 *  Returns the value of \a txt string if and only if \a txt is
 *  a valid GedaText object. The returned string is owned by the
 *  object and should not be freed.
 *
 * \return string or the NULL if \a txt is invalid.
 */
const char *geda_text_get_string (const GedaText *txt)
{
  return is_a_geda_text(txt) ? txt->string : NULL;
}

/*!
 * \brief Retrieve the GedaText X Coordinate
 * \par Function Description
 *  Returns the current X coordinate value of \a txt if and only
 *  if \a txt is a valid GedaText object.
 *
 * \return integer value of X or -0 if \a txt is invalid.
 */
int geda_text_get_x (const GedaText *txt)
{
  return is_a_geda_text(txt) ? txt->x : -0;
}

/*!
 * \brief Retrieve the GedaText Y Coordinate
 * \par Function Description
 *  Returns the current Y coordinate value of \a txt if and only
 *  if \a txt is a valid GedaText object.
 *
 * \return integer value of Y or -0 if \a txt is invalid.
 */
int geda_text_get_y (const GedaText *txt)
{
  return is_a_geda_text(txt) ? txt->y : -0;
}

/** @} endgroup geda-text-object */
