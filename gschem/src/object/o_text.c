/* -*- C o_text.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */
/*!
 * \file o_text.c
 * \brief Low-level module for manipulating Text objects
 */

#include <gschem.h>
#include <math.h>
#include <geda_debug.h>

/*!
 * \brief Get Bounds of text object
 * \par Function Description
 *  This function is used as a callback for text objects
 *  to determine the XY bounderies of the objects.
 */
int o_text_get_rendered_bounds (void *user_data, GedaObject *o_current,
                                int  *min_x, int *min_y,
                                int  *max_x, int *max_y)
{
  GschemToplevel *w_current = (GschemToplevel *) user_data;

  int result;
  int t = 0;
  int l = 0;
  int r = 0;
  int b = 0;

  g_return_val_if_fail ((w_current != NULL), FALSE);

  /* First check if this is hidden text. */
  if (geda_object_get_is_visible(o_current)) {

    if (w_current->render_adaptor == CAIRO_ADAPTOR) {

      EdaRenderer    *renderer;
      cairo_t        *cr;
      cairo_matrix_t  render_mtx;

      int render_flags = 0;

      cr = gdk_cairo_create (w_current->window);

      renderer = g_object_ref (CairoRenderer);

      g_object_set (renderer,
                    "cairo-context", cr,
                    "render-flags", render_flags,
                    NULL);

      /* Transform the cairo context to approximate world coordinates. */
      cairo_matrix_init (&render_mtx, 1, 0, 0, -1, -1, 1);
      cairo_set_matrix (cr, &render_mtx);

      /* Use the renderer to calculate text bounds */
      result = eda_renderer_get_text_user_bounds (renderer, o_current, &l, &t, &r, &b);

      /* Clean up */
      eda_renderer_destroy (renderer);
      cairo_destroy (cr);

      /* Round bounds to nearest integer */
      *min_x = min (l, r);
      *min_y = min (t, b);
      *max_x = max (l, r);
      *max_y = max (t, b);
    }
    else {

#ifdef WITH_LIBGEDADRAW

      result =  x_draw_set_text_bounds(o_current);

#else

      result =  FALSE;

#endif

      if (result) {
        *min_x = o_current->left;
        *max_x = o_current->right;
        *min_y = min (o_current->top, o_current->bottom);
        *max_y = max (o_current->top, o_current->bottom);
      }
    }
  }
  else {
    result = FALSE;
  }

#if DEBUG
  if (result)
     fprintf(stderr, " %s <type %d> left<%d>, right<%d>, top<%d>, bottom<%d>\n",
             __func__, w_current->render_adaptor, *min_x , *max_x, *max_y, *min_y);
#endif

  return result;
}

static void o_text_end (GschemToplevel *w_current)
{
  o_place_end(w_current, FALSE, NULL, ADD_OBJECT_HOOK);
  o_undo_savestate (w_current, UNDO_ALL);
  i_event_stop_action_handler (w_current);
}

/*!
 * \brief Prepare for Placement of New Text Object
 * \par Function Description
 *  Creates a new text object and adds the Text object to
 *  current place list after ensuring the place list is empty.
 */
void o_text_prepare_place(GschemToplevel *w_current, char *text)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GedaObject   *object;

  /* Insert the new object into the buffer at world coordinates (0,0).
   * It will be translated to the mouse coordinates during placement. */

  w_current->first_wx = 0;
  w_current->first_wy = 0;

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  object = geda_text_object_new (TEXT_COLOR,
                       0, 0, LOWER_LEFT, 0, /* zero is angle */
                       w_current->text_size,
                       /* has to be visible so we can place it */
                       /* visibility is set when we create the object */
                       VISIBLE, SHOW_NAME_VALUE, text);

  geda_text_object_set_rendered_bounds_func (object,
                                   o_text_get_rendered_bounds,
                                   w_current);

  /* Remove the old place list if it exists */
  geda_struct_place_free_place_list(toplevel);

  /* Add the new text object to the list */
  geda_struct_place_append_place_list (toplevel, object);

  i_status_set_state (w_current, TEXTMODE);
  i_event_start_paster_handler(w_current, o_text_end);
}

/*!
 * \brief Launch the Edit Text Dialog
 * \par Function Description
 *  This function calls x_dialog_edit_text() which could have been done
 *  in i_command do_edit_text or o_edit directly so maybe this function
 *  is just adding an unnecessary stack push and pops.
 */
void o_text_edit(GschemToplevel *w_current, GedaObject *o_current)
{
  if (o_current->type == OBJ_TEXT) {
    x_dialog_edit_text(w_current, o_current);
  }
}

/*!
 * \brief Complete Text Editing
 * \par Function Description
 *  This function is called by x_dialog_edit_text_ok if the OKAY button was
 *  pressed. There may or by not have been any changes so we will loop thru
 *  all of the selected objects and make changes when there is a difference.
 *  If we changed something we recreate that object and undate UNDO.
 *
 * \param [in] w_current  Ptr to Window specific data structure
 * \param [in] string     Ptr to new char string  or NULL if multiple selection
 * \param [in] text_align integer, The new text alignment
 * \param [in] text_color integer, The new text color
 * \param [in] text_size  integer, The new text size integer
 * \param [in] rotate     integer, The rotation angle
 */
/*
 * 02/27/13 WEH Added text_color, changed_something conditionals and documentation
 * 07/20/13 WEH Added rotation, eliminated numselect and len & add conditonal
 *          skip for values less then 0.
 */
void
o_text_edit_end(GschemToplevel *w_current, char *string, int text_align,
                int text_color, int text_size, int rotate)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GList  *s_current;
  bool    changed_something;
  bool    invalidated;

  /* skip over head */
  s_current = geda_list_get_glist( toplevel->page_current->selection_list );

  changed_something = FALSE;

  while(s_current != NULL) {

    GedaObject *object = (GedaObject*) s_current->data;

    if (object) {

      if (object->type == OBJ_TEXT) {


        invalidated = FALSE;

        /* Text string is only applicable if string has length */
        if ( string && strlen (string) != 0 ) {
          if (strcmp(object->text->string, string) != 0) {
            geda_text_object_set_string (object, string);
            changed_something = TRUE;
            invalidated = TRUE;
            /* handle slot= attribute, it's a special case */
            if (object->attached_to != NULL &&
              geda_strncmpi (string, "slot=", 5) == 0) {
              o_slot_end (w_current, object->attached_to, string);
            }
          }
        }

        /* \note: if the string was replaced then the old text was erased
         *        else if other changes are made, the old text may need to
         *        be erased BEFORE commiting the changes */

        /* Change Size */
        if( text_size >= 0 && object->text->size != text_size) {
          if (!invalidated && text_size < object->text->size) {
            /* New size is smaller, make sure old text gets erased */
            o_invalidate_object (w_current, object);
            invalidated = TRUE;
          }
          object->text->size = text_size;
          changed_something = TRUE;
        }

        /* Change Alignment */
        if(text_align >= 0 && object->text->alignment != text_align) {
          if (!invalidated) {
            /* Make sure text with old alignment gets erased */
            o_invalidate_object (w_current, object);
            invalidated = TRUE;
          }
          object->text->alignment = text_align;
          changed_something = TRUE;
        }

        /* Change Color */
        if( text_color >= 0 && object->color != text_color) {
          object->color = text_color;
          changed_something = TRUE;
        }

        /* Change Rotation */
        if (rotate >= 0 && object->text->angle != rotate) {
          if (!invalidated) {
            /* Make sure text with old text gets erased */
            o_invalidate_object (w_current, object);
            invalidated = TRUE;
          }
          object->text->angle = rotate;
          changed_something = TRUE;
        }

        if (changed_something) {
          geda_text_object_recreate(object);
        }
      }
    }

    s_current = g_list_next(s_current);
  }

  if (changed_something) {
    o_undo_savestate(w_current, UNDO_ALL);
  }

}

/*!
 * \brief Change Text String
 * \par Function Description
 *  This function is call by:
 *
 *     attrib_edit_dialog_ok
 *     ma_callback_edited_name
 *     ma_callback_edited_value
 *
 * \note
 *  The object passed in should be the REAL object, NOT any copy in any
 *  selection list
 */
void
o_text_change(GschemToplevel *w_current, GedaObject *object,
              char *string, int visibility, int show)
{
  if (object == NULL) {
    BUG_MSG("object == NULL");
  }
  else {
    if (object->type != OBJ_TEXT) {
      BUG_MSG("object != OBJ_TEXT");
    }
    else {

      geda_text_object_set_string (object, string);

      geda_set_object_visibility (object, visibility);
      object->show_name_value = show;
      geda_text_object_recreate(object);

      /* handle slot= attribute, it's a special case */
      if (object->attached_to != NULL &&
          geda_strncmpi (string, "slot=", 5) == 0)
      {
        o_slot_end (w_current, object->attached_to, string);
      }
    }
  }
}
