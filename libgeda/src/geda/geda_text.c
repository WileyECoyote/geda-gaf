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
 * 02110-1301 USA
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

#include "libgeda_priv.h"

G_DEFINE_TYPE (Text, geda_text, GEDA_TYPE_OBJECT);


/*! \brief calculate and return the boundaries of a text object
 *
 *  \par Function Description
 *  This function calculates the object boudaries of a text \a object.
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
  if (o_current->text->rendered_text_bounds_func != NULL) {
    result =
    text->rendered_text_bounds_func (o_current->text->rendered_text_bounds_data,
                                     o_current, &left, &top, &right, &bottom);
  }
  else {

    /* Text object must be associated with page */
    if(GEDA_IS_PAGE(o_current->page)) {
      page = o_current->page;

      /* Check if page level render func is set */
      if (page->rendered_text_bounds_func != NULL) {
        result =
        page->rendered_text_bounds_func (page->rendered_text_bounds_data,
                                         o_current, &left, &top, &right, &bottom);
      }
      else {
        g_return_val_if_fail(GEDA_IS_TOPLEVEL(page->toplevel), FALSE);
        toplevel = page->toplevel;

        /* Check if toplevel render func is set */
        if (toplevel->rendered_text_bounds_func != NULL) {
          result =
          toplevel->rendered_text_bounds_func(toplevel->rendered_text_bounds_data,
                                              o_current, &left, &top, &right, &bottom);
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

/*! \brief GedaType instance initialiser for Text
 *
 *  \par Function Description
 *  GedaType instance initialiser for Text, initializes a new empty
 *  Text object by setting pointers to NULL and numbers to zero,
 *  the text PID variable is set to the next text index.
 *
 *  \param [in]  text      The Text instance being initialising.
 */
static void geda_text_init(Text *text)
{
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

  text->head_marker                = GEDA_TYPE_TEXT;
  text->tail_marker                = text->head_marker;
}

static void
geda_text_dispose(GObject *object)
{
  Text *text = GEDA_TEXT(object);

  if (text->string) {
    GEDA_FREE(text->string);
    text->string = NULL;
  }
  if (text->disp_string) {
    GEDA_FREE(text->disp_string);
    text->disp_string = NULL;
  }

  G_OBJECT_CLASS(geda_text_parent_class)->dispose(object);

}

/*! \brief Geda Text Object Finalization Function
 *  \par Function Description
 *   This function removes or releases all internal references
 *   and releases the memory allocated to the given Text
 *   data structure and then chain up to the parent's finalize
 *   handler.
 */
static void geda_text_finalize(GObject *object)
{
  GEDA_OBJECT_CLASS( geda_text_parent_class )->finalize(object);
}

/*! \brief GedaType class initialiser for Text
 *
 *  \par Function Description
 *  GedaType class initialiser for Text. We override our parents
 *  virtual class methods as needed and register our GObject signals.
 *
 *  \param [in]  class       The Text we are initialising
 */
static void geda_text_class_init(TextClass *class)
{
  GObjectClass *gobject_class  = G_OBJECT_CLASS( class );
  ObjectClass  *object_class   = GEDA_OBJECT_CLASS( class );

  geda_text_parent_class       = g_type_class_peek_parent( class );

  gobject_class->dispose       = geda_text_dispose;
  gobject_class->finalize      = geda_text_finalize;

  object_class->bounds         = geda_text_bounds;
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
  return GEDA_IS_OBJECT(txt) && (GEDA_TYPE_TEXT == (txt->head_marker & txt->tail_marker));
}
/** @} endgroup geda-text-object */
