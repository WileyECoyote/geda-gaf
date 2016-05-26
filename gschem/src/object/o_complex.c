/* -*- C o_complex.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
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
 * \file o_complex.c
 * \brief Low-level module for manipulating Complex objects
 */

#include <gschem.h>
#include <geda_file_chooser.h>
#include <geda_debug.h>

/*! \brief Export Complex to symbol file
 *  \par Function Description
 *  This function is provided to allow user to export embeded complexes
 *  but is not restricted to an embeded object, though it may not make
 *  sense to export a symbol that already exist as a file.
 *
 */
void o_complex_export(GschemToplevel *w_current, GedaObject *o_current)
{
  GtkWidget *dialog;

  dialog = geda_file_chooser_new (w_current->main_window,
                                  FILE_CHOOSER_ACTION_SAVE);

  geda_file_chooser_set_filter (dialog, FILTER_SYMBOL);
  geda_file_chooser_set_filename (dialog, o_current->complex->filename);

  gtk_widget_show (dialog);

  if (gtk_dialog_run ((GtkDialog*)dialog) == GEDA_RESPONSE_ACCEPT) {

    char   *filename;
    GError *err;
    GList  *list;

    list     = g_list_append(NULL, o_current);
    filename = geda_file_chooser_get_filename (dialog);

    if (!o_save (list, filename, &err)) {

      pango_error_dialog("Failed to export symbol:", err->message);
      g_clear_error (&err);

    }
    GEDA_FREE (filename);
  }
  gtk_widget_destroy (dialog);
}

static void o_complex_end (GschemToplevel *w_current)
{
  o_place_end(w_current, w_current->continue_component_place, NULL, ADD_OBJECT_HOOK);

  if (!w_current->continue_component_place) {
    i_event_stop_action_handler (w_current);
  }
  o_undo_savestate (w_current, UNDO_ALL);
}

/*!
 * \brief Prepare for Placement of New Complex Object
 * \par Function Description
 *  Creates a new Complex object and adds the object to #Current_PlaceList
 *  after ensuring the place list is empty.
 */
static bool o_complex_prepare_place(GschemToplevel *w_current, const CLibSymbol *sym)
{
  GedaToplevel *toplevel = w_current->toplevel;
  const char   *sym_name = s_clib_symbol_get_name (sym);
  GError       *err      = NULL;
  bool          success  = FALSE;

  /* remove the old place list if it exists */
  s_place_free_place_list(toplevel);

  /* Insert the new object into the buffer at world coordinates (0,0).
   * It will be translated to the mouse coordinates during placement. */

  w_current->first_wx = 0;
  w_current->first_wy = 0;

  if (w_current->include_complex) {

    GList *temp_list;
    char  *buffer;

    buffer    = s_clib_symbol_get_data (sym);
    temp_list = geda_object_read_buffer(toplevel, NULL, buffer, -1, sym_name, &err);

    GEDA_FREE (buffer);

    if (err) {
      /* If an error occurs here, we can assume that the preview also has
       * failed to load and the error message is displayed there. Therefore,
       * we ignore this error, but end the component insertion.
       */
      g_error_free(err);
      i_status_set_state (w_current, SELECT);
    }
    else {
      /* Take the added objects */
      s_place_set_place_list(toplevel, temp_list);
      success = TRUE;
    }
  }
  else { /* if (w_current->include_complex) {..} else { */

   GedaObject *new_object;

    new_object = geda_complex_object_new (toplevel, 0, 0, 0, 0, sym, sym_name, 1);

    if (new_object->type == OBJ_PLACEHOLDER) {

      /* If new object is a placeholder, load failed sort end action */
      s_object_release(new_object);
      i_status_set_state (w_current, SELECT);
    }
    else {

      GedaObject *o_current;

      GList *promoted   = geda_complex_object_promote_attribs (toplevel, new_object);

      Current_PlaceList = g_list_concat (Current_PlaceList, promoted);

      Current_PlaceList = g_list_append (Current_PlaceList, new_object);

      /* Flag the symbol as embedded if necessary */
      o_current = (g_list_last (Current_PlaceList))->data;

      if (w_current->embed_components) {
        o_current->complex->is_embedded = TRUE;
      }
      success = TRUE;
    }
  }

  return (w_current->inside_action = success);
}

/*! \brief Start Placement of New Complex Object
 *
 *  \par Function Description
 *  Calls o_complex_prepare_place to create a new Complex object and sets
 *  event state and handler on success.
 */
void o_complex_start(GschemToplevel *w_current, const CLibSymbol *sym, int state)
{
  if (o_complex_prepare_place (w_current, sym)) {
    i_status_set_state (w_current, state);
    i_event_start_paster_handler(w_current, o_complex_end);
  }
}

/*! \brief Run the complex place list changed hook
 *  \par Function Description
 *  The complex place list is usually used when placing new components
 *  in the schematic. This function should be called whenever that list
 *  is modified.
 *
 *  \param [in] w_current GschemToplevel structure.
 */
void o_complex_place_changed_run_hook(GschemToplevel *w_current) {

  GedaToplevel *toplevel = w_current->toplevel;

  if (toplevel->page_current->place_list != NULL) {

    /* Run the complex place list changed hook */
    if (scm_is_false(scm_hook_empty_p(complex_place_list_changed_hook))) {

      GList *iter = toplevel->page_current->place_list;

      scm_dynwind_begin (0);
      g_dynwind_window (w_current);
      while (iter) {
        SCM expr = scm_list_3 (scm_from_utf8_symbol ("run-hook"),
                               complex_place_list_changed_hook,
                               edascm_from_object ((GedaObject*) iter->data));

        g_scm_eval_protected (expr, scm_interaction_environment ());
        iter = iter->next;
      }
      scm_dynwind_end ();
    }
  }
}

/*!
 * \brief Translate All objects
 * \par Function Description
 *  Helper for the Translate Dialog to translate all objects on the
 *  current page, whether selected or not \a offset distance in both
 *  the x and y direction.
 *
 * \param [in] w_current    GschemToplevel structure
 * \param [in] offset       Integer distance to translate all objects
 * \param [in] zoom_extents Zoom to the extents if True
 *
 * \todo has nothing to do with complex objects
 */
void
o_complex_translate_all(GschemToplevel *w_current, int offset, bool zoom_extents)
{
  GedaToplevel *toplevel = w_current->toplevel;
  const GList  *object_list;

  int left, top, right, bottom;
  int x, y;

  object_list = s_page_get_objects (toplevel->page_current);

  /* first zoom extents */
  if (zoom_extents) {
    i_zoom_world_extents (w_current, object_list, I_PAN_DONT_REDRAW);
  }
  o_invalidate_all (w_current);

  o_get_bounds_list (object_list, &left,  &top, &right, &bottom);

  /*! \todo do we want snap grid here? */
  x = snap_grid (w_current, left);
  y = snap_grid (w_current, top);

  if (offset == 0) {
    u_log_message(_("Translating schematic [%d %d]\n"), -x, -y);
    geda_translate_list (object_list, -x, -y);
  }
  else {
    u_log_message(_("Translating schematic [%d %d]\n"), offset, offset);
    geda_translate_list (object_list, offset, offset);
  }

  if (zoom_extents) {
    i_zoom_world_extents (w_current, object_list, I_PAN_DONT_REDRAW);
  }

  if (!w_current->SHIFTKEY) {
    o_select_unselect_all(w_current);
  }

  o_invalidate_all (w_current);
  o_undo_savestate(w_current, UNDO_ALL);
  i_status_update_sensitivities(w_current);
}

/*! \brief Reposition attributes to original positions
 *  \par Function Description
 *  The functions iterates over the attributes attached to the
 *  given object and calls o_attrib_reset_position to reposition
 *  each attribute back to where the attribute was defined in the
 *  symbol file if the attribute was inherited.
 *
 *  \returns TRUE if an object was modified, otherwise FALSE.
 */
bool o_complex_reset_attrib_positions (GschemToplevel *w_current, GedaObject *o_current)
{
  int    modified = FALSE;
  GList *attributes = geda_attrib_return_attribs (o_current);
  GList *a_iter;

  for (a_iter = attributes; a_iter != NULL; a_iter = a_iter->next) {

   GedaObject *a_current = a_iter->data;

    if (!geda_attrib_is_inherited(a_current)) {
      if (o_attrib_reset_position(w_current, o_current, a_current)) {
        modified = TRUE;
      }
    }
  }
  g_list_free (attributes);
  return modified;
}
