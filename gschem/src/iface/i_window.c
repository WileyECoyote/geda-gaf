/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_window.c
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <gschem.h>
#include <gschem_dialog.h>
#include <x_window.h>

#include <geda_debug.h>

/*! \brief Idle Update Editing Dialogs Page Selection change
 *  \par Function Description
 *   Checks for eack of the editing dialog with selection trackers and
 *   updates the selection if the dialog is active.
 *
 *  \param [in] w_current  The GschemToplevel object
 *
 */
static bool i_window_idle_notify_dialogs (GschemToplevel *w_current)
{
  if (w_current->aawindow != NULL) { /* Arc Attrib Tracks */
    g_object_set (G_OBJECT (w_current->aawindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }
  if (w_current->clwindow != NULL) { /* Color Edit Tracks */
    g_object_set (G_OBJECT (w_current->clwindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }
  if (w_current->hpwindow != NULL) { /* Hatch Pattern Tracks */
    g_object_set (G_OBJECT (w_current->hpwindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }
  if (w_current->ltwindow != NULL) { /* Line Type Tracks */
    g_object_set (G_OBJECT (w_current->ltwindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }
  if (w_current->prwindow != NULL) { /* Prop edit Tracks */
    g_object_set (G_OBJECT (w_current->prwindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }
  if (w_current->ptwindow != NULL) { /* Pin Type Tracks */
    g_object_set (G_OBJECT (w_current->ptwindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }
  if (w_current->sewindow != NULL) { /* Slot Edit Tracks */
    g_object_set (G_OBJECT (w_current->sewindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }
  if (w_current->tewindow != NULL) { /* Text Edit Tracks */
    g_object_set (G_OBJECT (w_current->tewindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }
  if (w_current->aewindow != NULL) { /* Attribute Edit Tracks */
    g_object_set (G_OBJECT (w_current->aewindow), DIALOG_SELECTION_DATA,
                                                  Current_Selection, NULL);
  }

  x_multiattrib_update (w_current);
  x_pagesel_update (w_current);

  return FALSE;
}

void i_window_close_page (GschemToplevel *w_current)
{
  bool  can_close;
  Page *page;

  page = gschem_toplevel_get_current_page(w_current);

  if (geda_page_get_changed(page) > 0) {
    can_close = x_confirm_close_changed_page (w_current, page);
  }
  else {
    can_close = TRUE;
  }

  if (page && can_close) {
    q_log_message(_("Closing Page\n"));
    g_hook_run_page (w_current, CLOSE_PAGE_HOOK, page);
    x_window_close_page (w_current, page);
  }

  i_status_set_state(w_current, SELECT);
}

/*! \brief get the pointer position of a given GschemToplevel
 *  \par Function Description
 *  This function gets the pointer position of the drawing area of the
 *  current workspace <b>GschemToplevel</b>. The flag <b>snapped</b> specifies
 *  whether the pointer position should be snapped to the current grid.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] snapped    An option flag to specify the wished coords
 *  \param [out] wx        snapped/unsnapped world x coordinate
 *  \param [out] wy        snapped/unsnapped world y coordinate
 *
 *  \return Returns TRUE if the pointer position is inside the drawing area.
 *
 */
bool i_window_get_pointer_position (GschemToplevel *w_current,
                                   bool snapped, int *wx, int *wy)
{
  int sx, sy, x, y;

  gtk_widget_get_pointer(w_current->drawing_area, &sx, &sy);

  /* check if we are inside the drawing area */
  if (sx < 0 || sx >= w_current->screen_width  ||
      sy < 0 || sy >= w_current->screen_height) {
    return FALSE;
  }

  SCREENtoWORLD (w_current, sx, sy, &x, &y);

  if (snapped) {
    x = snap_grid (w_current, x);
    y = snap_grid (w_current, y);
  }

  *wx = x;
  *wy = y;

  return TRUE;
}

/*! \brief Do updates when the Current Page is Changed
 *  \par Function Description
 *  This function calls various functions in order to update the main
 *  window interface and dialogs that are linked to the page selection
 *  and send notifification to SCM hooks that the page has been changed.
 *
 *  \param [in] w_current  The GschemToplevel object
 */
void i_window_on_page_changed (GschemToplevel *w_current)
{
  Page *page;

  i_status_update_sensitivities (w_current);
  i_status_update_title (w_current);

  i_window_set_viewport_size (w_current);

  g_idle_add ((GSourceFunc)i_window_idle_notify_dialogs, w_current);

  page = gschem_toplevel_get_current_page (w_current);

  g_hook_run_page (w_current, CHANGE_PAGE_HOOK, page);
}

/*! \brief Revert Current Page back to File
 *  \par Function Description
 *   Attempts to reload the current page from dish into the current page
 *   by removing all of the object on the current page and reloading. If
 *   the file can not be reloaded the function resorts to using the undo
 *   system to restore the objects that were deleted.
 *
 *  \note 1. If the file is not modified, the user is not asked to confirm.
 *  \note 2. The page does not change position in the page list.
 *
 *  \param [in] w_current  The GschemToplevel object
 */
void i_window_revert_page (GschemToplevel *w_current)
{
  Page *page;
  int   answer;

  page = gschem_toplevel_get_current_page (w_current);

  if (geda_page_get_changed(page) > 0) {
    answer = x_dialog_confirmation (_("Really revert page?"), GTK_MESSAGE_QUESTION, FALSE);
  }
  else {
    answer = GEDA_RESPONSE_YES;
  }

  if (page && (answer == GEDA_RESPONSE_YES)) {

    GedaToplevel *toplevel;
    GError       *err;

    char *filename;
    int   page_control;
    int   up;

    err          = NULL;
    toplevel     = gschem_toplevel_get_geda_toplevel (w_current);

    /* save these for later */
    filename     = page->filename;
    page_control = page->page_control;
    up           = page->hierarchy_up;

    /* Just in case */
    o_undo_savestate(w_current, UNDO_ALL);

    geda_notify_list_freeze (page->change_notify_funcs);

    i_event_block_handler (w_current, EXPOSE_EVENT_HANDLER);

    /* Clear the selection list Note the we do not use o_select_unselect_all
     * here because all of the object are soon to be wiped-out */
    geda_list_remove_all(toplevel->page_current->selection_list);

    s_place_free_place_list (toplevel);

    s_page_delete_objects (page);

    toplevel->open_flags = F_OPEN_RESTORE_CWD;

    page->filename = NULL;

    if (f_open(toplevel, page, filename, &err)) {

    /* make sure we maintain the hierarchy info */
      page->page_control    = page_control;
      page->hierarchy_up    = up;

      geda_page_set_changed (page, FALSE);

      GEDA_FREE (filename);
    }
    else {

      const char *disk_err_msg;

      disk_err_msg = _("An error occurred during a File Revert operation: %s.");

      char *errmsg = geda_utility_string_sprintf (disk_err_msg, err->message);
      titled_pango_error_dialog(_("<b>File error.</b>"), errmsg, _("Revert failed"));
      GEDA_FREE(errmsg);
      g_error_free(err);

      /* Put the file name back */
      page->filename = filename;

      u_log_message(_("Error encountered during file operation <%s>\n"), filename);
      u_log_message(_("Recovery: undo last action\n"));

      /* Do error recovery */
      o_undo_callback(w_current, UNDO_ACTION);
    }
    x_window_set_current_page (w_current, page);
    i_event_unblock_handler (w_current, EXPOSE_EVENT_HANDLER);
    geda_notify_list_thaw (page->change_notify_funcs);
  }
}

void i_window_set_cursor(GschemToplevel *w_current, int cursor_id)
{

  GdkWindow *draw_window;

  draw_window = gtk_widget_get_window(DrawingArea);

  if(draw_window){
    if (w_current->cursor) {
      gdk_window_set_cursor(draw_window, NULL);
      if (w_current->cursor->ref_count > 0) {
        gdk_cursor_destroy(w_current->cursor);
      }
    }

    if (cursor_id >= 0) {
      w_current->cursor = gdk_cursor_new (cursor_id);
      gdk_window_set_cursor (draw_window, w_current->cursor);
    }
  }
}

void i_window_set_grid_type (GschemToplevel *w_current)
{
  x_grid_configure_variables (w_current);
  x_toolbars_set_grid_radio (w_current);
}

/*! \brief Set Pointer Position Relative to the Drawing Area
 *  \par Function Description
 *   This function sets the pointer position to relative
 *   screen coordinates off the given widget.
 *
 *  \param [in] w_current The GschemToplevel object
 *  \param [in] wx        Integer is abscissa in World units
 *  \param [in] wy        Integer is ordinate in World units
 *
 */
void i_window_set_pointer_position (GschemToplevel *w_current, int wx, int wy)
{
  int sx, sy;

  gtk_window_present (GTK_WINDOW(w_current->main_window));

  WORLDtoSCREEN (w_current,  wx, wy, &sx, &sy);

  /* set cursor position */
  i_pan_warp_cursor (w_current->drawing_area, sx, sy);

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] w_current   The GschemToplevel object
 *
 *  \note
 *  this is used during an open command to setup the correct sizes
 */
void i_window_set_viewport_size(GschemToplevel *w_current)
{
  GedaToplevel  *toplevel = w_current->toplevel;
  GtkAllocation *allocation;

  allocation = geda_get_widget_allocation(DrawingArea);

  /* of the actual win window (drawing_area) */
  w_current->screen_width  = allocation->width;
  w_current->screen_height = allocation->height;

#if DEBUG_EVENTS
  printf("manual: %d %d\n", w_current->screen_width, w_current->screen_height);
#endif

  /* need to do this every time the width / height change */
  x_window_setup_page(w_current, toplevel->page_current,
                      toplevel->page_current->left,
                      toplevel->page_current->right,
                      toplevel->page_current->top,
                      toplevel->page_current->bottom);

#if DEBUG_EVENTS
  float aspect = (float) w_current->screen_width / w_current->screen_height;
  printf("Window aspect: %f\n", aspect);
#endif
}

/*! \brief Interface for Toggling Visibility of Attributes
 *  \par Function Description
 *   Called by i_cmd_do_show_hidden and i_cmd_do_show_inherited to
 *   toggle visibility of attributes.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] scope      Boolean flag, TRUE for inherited
 */
void i_window_show_attributes (GschemToplevel *w_current, int scope)
{
  if (!w_current->inside_action) {

    GList *object_list;
    Page  *p_current;
    bool   show_status;

    object_list = NULL;
    p_current   = gschem_toplevel_get_current_page(w_current);

    if (o_select_is_selection (w_current)) {

      SELECTION *selection;

      selection   = s_page_get_selection (p_current);
      object_list = geda_list_get_glist (selection);
      show_status = FALSE;
    }
    else {
      object_list = s_page_get_objects (p_current);
      show_status = TRUE;
    }

    if (object_list) {
      if(o_edit_show_hidden (w_current, object_list, scope)) {
        o_undo_savestate (w_current, UNDO_ALL);
        if (show_status) {
          i_status_show_state(w_current, NULL); /* update screen status */
        }
      }
    }
  }
  else if (o_select_is_selection (w_current)) {

    GList *place_list = s_place_get_place_list(w_current->toplevel);

    if (place_list) {
      o_edit_show_hidden (w_current, place_list, scope);
    }
  }
}
