/* -*- geda_text.c -*-
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 *  Contributing Author: Wiley Edward Hill
 *  Date Contributed: November, 18, 2013
 */
/*! \file geda_text.c
 *  \brief Geda Text Object Class derived from the GedaObject Class
 */
/** \defgroup geda-text-object Geda Text Object
 *  @{
 */
/*! \class Text geda_text.h "include/libgeda/geda_text.h"
 *  \implements geda-object
 *  \brief This is an implementaion class for GEDA Text Objects.
 *  A Geda Text Object is a type of graphical object, do not directly involve
 *  electrical interconnections. However, text object with strings in the
 *  for of name=value are consider attributes, which may be used to describe
 *  the electrical properties of the another object.
 */

#include <config.h>

#include <libgeda_priv.h>

static GObjectClass *geda_text_parent_class = NULL;

/*! \brief Calculate and Return the Boundaries of a text object
 *
 *  \par Function Description
 *  This function attempts to calls the bounds functions to calculates
 *  the object boundaries of a text \a object.
 *
 *  The object's bounds function is checked first, if the object's bounds
 *  function is not set then the object's page and Toplevel are checked.
 *  If either the Page or the Toplevel bounds function can determine the
 *  bounds the object's bounds function is set to the function that was
 *  successful.
 *
 *  \param [in]  o_current a text object
 */
int
geda_text_bounds(Object *o_current)
{
  g_return_val_if_fail(GEDA_IS_TEXT(o_current), FALSE);

  GedaToplevel *toplevel;
  Text *text = GEDA_TEXT(o_current);
  Page *page;
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

      page = o_current->page;

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

        toplevel = page->toplevel;

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
  }
  return result;
}

/*! \brief GedaType instance initializer for Text
 *
 *  \par Function Description
 *  GedaType instance initializer for Text, initializes a new empty
 *  Text object by setting pointers to NULL and numbers to zero.
 *
 *  \param [in] instance The Text structure being initialized,
 *  \param [in] g_class  The Text class we are initializing.
 */
static void geda_text_instance_init(GTypeInstance *instance, void *g_class)
{
  Text   *text        = (Text*)instance;
  Object *object      = &text->parent_instance;

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

static void
geda_text_dispose(GObject *object)
{
  Text *text = GEDA_TEXT(object);

  GEDA_FREE(text->string);
  GEDA_FREE(text->disp_string);

  G_OBJECT_CLASS(geda_text_parent_class)->dispose(object);

}

/*! \brief Geda Text Object Finalization Function
 *  \par Function Description
 *   This function invalidates the Text's markers and then chains up to
 *   the parent's finalize handler. Once invalidated, GEDA_IS_TEXT will
 *   fail.
 */
static void geda_text_finalize(GObject *object)
{
  Object *obj  = GEDA_OBJECT(object);

  /* The object is no longer a GedaText */
  obj->text    = NULL;

  /* Finialize the parent GedaObject Class */
  GEDA_OBJECT_CLASS(geda_text_parent_class)->finalize(object);
}

/*! \brief GedaType class initializer for Text
 *
 *  \par Function Description
 *  GedaType class initializer for Text. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  g_class      The Text class we are initialising
 *  \param [in]  class_data   The Text structure associated with the class
 */
static void geda_text_class_init(void *g_class, void *class_data)
{
  TextClass    *class          = (TextClass*)g_class;
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );
  ObjectClass  *object_class   = GEDA_OBJECT_CLASS( class );

  geda_text_parent_class       = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_text_dispose;
  gobject_class->finalize      = geda_text_finalize;

  object_class->bounds         = geda_text_bounds;
}

/*! \brief Function to retrieve Text's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #Text Type identifier. When first called,
 *  the function registers a #Text in the GedaObjectType system to
 *  obtain an identifier that uniquely itentifies a Text and returns
 *  the unsigned integer value. The retained value is returned on all
 *  Subsequent calls.
 *
 *  \return GedaObjectType identifier associated with Text.
 */
GedaObjectType geda_text_get_type (void)
{
  static volatile GedaObjectType geda_text_type = 0;

  if (g_once_init_enter (&geda_text_type)) {

    static const GTypeInfo info = {
      sizeof(TextClass),
      NULL,                   /* base_init           */
      NULL,                   /* base_finalize       */
      geda_text_class_init,   /* (GClassInitFunc)    */
      NULL,                   /* class_finalize      */
      NULL,                   /* class_data          */
      sizeof(Text),
      0,                      /* n_preallocs         */
      geda_text_instance_init /* (GInstanceInitFunc) */
    };

    const char    *string;
    GedaObjectType type;

    string = g_intern_static_string ("Text");
    type   = g_type_register_static (GEDA_TYPE_OBJECT, string, &info, 0);

    g_once_init_leave (&geda_text_type, type);
  }

  return geda_text_type;
}

/*! \brief Returns a pointer to a new GedaText Object.
 *
 *  \par Function Description
 *  Returns a pointer to a new Text object.
 *
 *  \return pointer to the new Text object.
 */
Object *geda_text_new (void)
{
  Object *text = g_object_new( GEDA_TYPE_TEXT,
                               "type", OBJ_TEXT,
                               "name", "text",
                                NULL );
  return GEDA_OBJECT(text);
}

/*! \brief Determine if object is a Geda Text Object.
 *
 *  \par Function Description
 *  Returns true if the argument is a Geda Text object.
 *
 *  \return boolean.
 */
bool is_a_geda_text_object (Text *txt)
{
  return GEDA_IS_OBJECT(txt) && (((Object*)txt)->type == OBJ_TEXT);
}
/** @} endgroup geda-text-object */
