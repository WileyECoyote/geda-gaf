/* -*- C o_buffer.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2019 gEDA Contributors (see ChangeLog for details)
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
 * \file o_buffer.c
  * \brief Low-level module for manipulating buffers for the clip-board
 */
#include <gschem.h>
#include <geda_debug.h>

/*!
 * \brief Copy Current Selection to Buffer
 * \par Function Description
 *
 *  The function is used by both the "clipboard" copy and the
 *  "clipboard" cut routines. This function first deleted the
 *  contents of the assigned buffer before copying the current
 *  selection to the buffer.
 *
 * \param w_current A pointer to a GSCHEM top level object
 * \param buf_num   integer value of the buffer to use.
 */
static void selection_to_buffer(GschemToplevel *w_current, int buf_num)
{
  Page  *p_current;
  GList *s_current;

  p_current = gschem_toplevel_get_current_page (w_current);
  s_current = geda_list_get_glist(p_current->selection_list);

  if (object_buffer[buf_num] != NULL) {

    geda_struct_object_release_objects(object_buffer[buf_num]);

    /* NULL pointer so geda_copy_list does not append to the
     * list of objects that was just released.*/
    object_buffer[buf_num] = NULL;
  }

  object_buffer[buf_num] = geda_copy_list(s_current, object_buffer[buf_num]);
}

/*!
 * \brief Copy Selection to Buffer
 * \par Function Description
 *  This function calls selection_to_buffer to copy the current
 *  selection data to a buffer and then iterator over the objects
 *  in the buffer, calling geda_complex_object_reset_refdes for each
 *  complex in the copied buffer.
 *
 * \param w_current A pointer to a GSCHEM top level object
 * \param buf_num   integer value of the buffer to use.
 */
void o_buffer_copy(GschemToplevel *w_current, int buf_num)
{
  GList *iter;

  if (buf_num < 0 || buf_num >= MAX_BUFFERS) {
    BUG_IMSG ("Invalid buffer", buf_num);
  }
  else {
    selection_to_buffer (w_current, buf_num);
  }

  /* Get a pointer to the glist in the buffer */
  iter = object_buffer[buf_num];

  /* reset all the refdes in the copied buffer */
  while (iter != NULL) {

    GedaObject *object = (GedaObject*) iter->data;

    if (object->type == OBJ_COMPLEX || object->type == OBJ_PLACEHOLDER) {
      geda_complex_object_reset_refdes(object);
    }

    iter = g_list_next(iter);
  }
}

void o_buffer_clear(GschemToplevel *w_current, int buf_num)
{
  if (buf_num < 0 || buf_num >= MAX_BUFFERS) {
    BUG_IMSG ("Invalid buffer", buf_num);
  }
  else {

    geda_struct_object_release_objects(object_buffer[buf_num]);

    object_buffer[buf_num] = NULL;
  }
}

/*!
 * \brief Cut Selection to Buffer
 * \par Function Description
 *  This function calls selection_to_buffer to copy the current
 *  selection data to a buffer and then deleted the selected data.
 *
 * \param w_current A pointer to a GSCHEM top level object
 * \param buf_num   integer value of the buffer to use.
 */
void o_buffer_cut(GschemToplevel *w_current, int buf_num)
{
  if (buf_num < 0 || buf_num >= MAX_BUFFERS) {
    BUG_IMSG ("Invalid buffer", buf_num);
  }
  else {
    selection_to_buffer (w_current, buf_num);
    o_delete_selected(w_current);
    i_status_action_stop(w_current);
  }
}

static void o_buffer_paste_end (GschemToplevel *w_current)
{
  o_place_end(w_current, FALSE, NULL, PASTE_OBJECTS_HOOK);
  o_undo_savestate (w_current, UNDO_ALL);
  i_event_stop_action_handler (w_current);
}

/*!
 * \brief Paste Contents of Buffer into drawing
 * \par Function Description
 *  This function initiates the pasting of data by copying the buffer
 *  contents and updates the global state variable so the user can
 *  position/place the any objects that were in the buffer.
 */
bool o_buffer_paste_start(GschemToplevel *w_current, int w_x, int w_y)
{
  int result;
  int buf_num = w_current->buffer_number;

  Page *page = gschem_toplevel_get_current_page(w_current);

  if (page == NULL) {
    BUG_MSG ("Current_Page is NULL");
    result = FALSE;
  }
  else {

    if (buf_num < 0 || buf_num >= MAX_BUFFERS) {
      BUG_MSG ("invalid buffer_number");
      result = FALSE;
    }
    else {

      GedaToplevel *toplevel = w_current->toplevel;

      GList *place_list;
      int left, top, bottom, right;

      /* Cancel current place or draw action if it is being done */
      if (w_current->inside_action) {
        i_callback_cancel (w_current, 0, NULL);
      }

      w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

      /* Remove old place list and set from buffer content */
      place_list = geda_struct_place_set_place_list (toplevel, object_buffer[buf_num]);

#if DEBUG || DEBUG_DND_EVENTS || DEBUG_PASTE

      int dint = g_list_length(place_list);

      printf("%s: buffers has %d objects\n", __func__, dint);

#endif

      if (geda_object_get_bounds_list(place_list, &left, &top, &right, &bottom)) {

        const GList *iter;
        int x, y;

        /* Place objects into the buffer at the mouse origin, (w_x, w_y) */
        w_current->first_wx = w_x;
        w_current->first_wy = w_y;

        /* snap x and y to the grid, pointed out by Martin Benes */
        x = snap_grid (w_current, left);
        y = snap_grid (w_current, top);

        for (iter = place_list; iter != NULL; iter = iter->next) {
          GedaObject *o_current = iter->data;
          geda_object_translate(o_current, w_x - x, w_y - y);
        }

#if DEBUG || DEBUG_DND_EVENTS || DEBUG_PASTE
        printf("%s: calling o_place_start with %d objects\n", __func__, dint);
#endif

        result = o_place_start (w_current, w_x, w_y);
      }
      else { /* Buffer does not have objects to define its any bounds */
        result = FALSE;
      }
    }
  }

  if (result) {
    i_status_set_state(w_current, PASTEMODE);
    i_event_start_paster_handler(w_current, o_buffer_paste_end);
  }

  i_status_update_action_state(w_current, result);

  return result;
}

/*!
 * \brief Initialize the Buffers
 * \par Function Description
 *  This function just set each member of our global array of buffer
 *  pointers to NULL.
 */
void o_buffer_init(void)
{
  int i;

  for (i = 0 ; i < MAX_BUFFERS; i++) {
    object_buffer[i] = NULL;
  }
}

/*!
 * \brief Free All buffers
 * \par Function Description
 *  This function iterates over each buffer and deletes any
 *  found by calling geda_struct_object_release_objects.
 */
void o_buffer_free(GschemToplevel *w_current)
{
  int i;

  for (i = 0 ; i < MAX_BUFFERS; i++) {
    if (object_buffer[i]) {
      geda_struct_object_release_objects(object_buffer[i]);
      object_buffer[i] = NULL;
    }
  }
}
