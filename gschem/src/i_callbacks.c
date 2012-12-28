/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2011 gEDA Contributors (see ChangeLog for details)
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
#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"
#include "x_menu.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief */
#define DELIMITERS ", "

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* every i_callback functions have the same footprint */
#define DEFINE_I_CALLBACK(name)				\
	void i_callback_ ## name(GSCHEM_TOPLEVEL* w_current,    \
			         unsigned int callback_action,	\
			         GtkWidget *widget)

#define DEFINE_TB_CALLBACK(name)                         \
        void i_callback_toolbar_ ## name(GtkWidget* widget, \
                                        GSCHEM_TOPLEVEL* w_current)

/*! \section callback-intro Callback Functions
 * right now, all callbacks except for the ones on the File menu have
 * the middle button shortcut. Let me (Ales) know if we should also
 * shortcut the File button
 */

/*! \section file-menu File Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 *  \todo This should be renamed to page_new perhaps...
 */
DEFINE_I_CALLBACK(file_new)
{
  PAGE *page;

  /* create a new page */
  page = x_window_open_page (w_current, NULL);
  x_window_set_current_page (w_current, page);
  s_log_message (_("New page created [%s]\n"), page->page_filename);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
DEFINE_TB_CALLBACK(file_new)
{
  if (!w_current->window) return;
  i_callback_file_new(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_new_window)
{
  GSCHEM_TOPLEVEL *new_window;
  PAGE *page;

  new_window = gschem_toplevel_new ();
  new_window->toplevel = s_toplevel_new ();

  new_window->toplevel->load_newer_backup_func = x_fileselect_load_backup;
  new_window->toplevel->load_newer_backup_data = new_window;

  o_text_set_rendered_bounds_func (new_window->toplevel,
                                   o_text_get_rendered_bounds, new_window);

  /* Damage notifications should invalidate the object on screen */
  o_add_change_notify (new_window->toplevel,
                       (ChangeNotifyFunc) o_invalidate,
                       (ChangeNotifyFunc) o_invalidate, new_window);

  x_window_setup (new_window);

  page = x_window_open_page (new_window, NULL);
  x_window_set_current_page (new_window, page);
  s_log_message (_("New Window created [%s]\n"), page->page_filename);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  \todo This should be renamed to page_open perhaps...
 */
DEFINE_I_CALLBACK(file_open)
{
    x_fileselect_open (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  \todo This should be renamed to page_open perhaps...
 */
DEFINE_TB_CALLBACK(file_open)
{
  if (!w_current->window) return;
  i_callback_file_open(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_script)
{
  setup_script_selector(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 */
DEFINE_I_CALLBACK(file_save)
{
  /*! \todo probably there should be a flag that says whether
   *   page_filename is derived from untitled_name or specified by
   *   a user. Some twisted people might name their files like
   *   untitled_name. :-)
   */
  if (strstr(w_current->toplevel->page_current->page_filename,
             w_current->toplevel->untitled_name)) {
    x_fileselect_save (w_current);
  } else {
    x_window_save_page (w_current,
                        w_current->toplevel->page_current,
                        w_current->toplevel->page_current->page_filename);
  }
}

/*! \brief Save File initiated by ToolBar Button
 *  \par Function Description
 *  This callback function for the Save File toolbar button. The function
 *  just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(file_save)
{
  if (!w_current->window) return;
  i_callback_file_save(w_current, 0, NULL);
}

/*! \brief Save All Files initiated by Menu Selection
 *  \par Function Description
 *
 *  \note
 *  The widget parameter in this function is the a ptr to the Menu Selection!
 */
DEFINE_I_CALLBACK(file_save_all)
{
  if (s_page_save_all(w_current->toplevel)) {
     i_set_state_msg(w_current, SELECT, _("Failed to Save All"));
  } else {
     i_set_state_msg(w_current, SELECT, _("Saved All"));
  }

  i_update_toolbar(w_current);
  x_pagesel_update (w_current);
  i_update_menus(w_current);
}

/*! \brief Save File As command initiated by Menu Selection
 *  \par Function Description
 *  Save the current file to disk.
 */
DEFINE_I_CALLBACK(file_save_as)
{
  x_fileselect_save (w_current);
}
/*! \brief Save File As command initiated by Tool-bar Button
 *  \par Function Description
 *  This is a callback function for the Save As tool-bar button. The function
 *  just calls the corresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the tool-bar button!
 */
DEFINE_TB_CALLBACK(file_save_as)
{
  x_fileselect_save (w_current);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_print)
{
  char *base=NULL, *filename;
  char *ps_filename=NULL;

  g_return_if_fail (w_current->toplevel->page_current->page_filename != NULL);

  /* shortcut */
  filename = w_current->toplevel->page_current->page_filename;

  /* get the base file name */
  if (g_str_has_suffix(filename, ".sch")) {
    /* the filename ends with ".sch", remove it */
    base = g_strndup(filename, strlen(filename) - strlen(".sch"));
  } else {
    /* the filename does not end with .sch */
    base = g_strdup (filename);
  }

  /* add ".ps" tp the base filename */
  ps_filename = g_strconcat (base, ".ps", NULL);
  g_free(base);

  if (output_filename) {
    x_print_setup(w_current, output_filename);
  } else {
    x_print_setup(w_current, ps_filename);
  }

  g_free(ps_filename);
}
/*! \brief File Print command initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Print tool-bar button. The function
 *  just calls the corresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(file_print)
{
  i_callback_file_print(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_write_png)
{
    x_image_setup(w_current, png_image);
}
/*! \brief Export PDF command initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Print tool-bar button. The function
 *  just calls the corresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(write_pdf)
{
   x_image_setup(w_current, pdf_image);

}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  this function closes a window
 */
DEFINE_I_CALLBACK(file_close)
{
    s_log_message(_("Closing Window\n"));
    x_window_close(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function is called when you send a delete event to gschem
 *
 *  \note
 *  Also DON'T ref the widget parameter since they can be null
 *  \todo Need a cleaner way of doing this. This routine is used by the
 *  delete event signals
 */
DEFINE_I_CALLBACK(close)
{
  i_callback_file_close(w_current, 0, widget);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_quit)
{
  x_window_close_all(w_current);
}

/*! \section edit-menu Edit Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_undo)
{

  /* If we're cancelling from a move action, re-wind the
   * page contents back to their state before we started.
   *
   * It "might" be nice to sub-undo rotates / zoom changes
   * made whilst moving components, but when the undo code
   * hits s_page_delete(), the place list objects are free'd.
   * Since they are also contained in the schematic page, a
   * crash occurs when the page objects are free'd.
   * */
  if (w_current->inside_action &&
      (w_current->event_state == MOVE ||
       w_current->event_state == ENDMOVE)) {
    i_callback_cancel (w_current, 0, NULL);
  } else {
    o_undo_callback(w_current, UNDO_ACTION);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
DEFINE_TB_CALLBACK(edit_undo)
{
  if (!w_current->window) return;

  i_callback_edit_undo(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_redo)
{
  o_undo_callback(w_current, REDO_ACTION);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
DEFINE_TB_CALLBACK(edit_redo)
{
  if (!w_current->window) return;
  i_callback_edit_redo(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Select also does not update the middle button shortcut.
 */
DEFINE_I_CALLBACK(edit_select)
{
  o_redraw_cleanstates(w_current);

  /* this is probably the only place this should be */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  w_current->inside_action = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 * since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
DEFINE_TB_CALLBACK (edit_select)
{
  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
    if (!o_invalidate_rubber (w_current)) {
      i_callback_cancel(w_current, 0, NULL);
    }
    i_callback_edit_select(w_current, 0, NULL);
  }
}

/*! \brief Select all objects on page.
 * \par Function Description
 * Sets all objects on page as selected.
 */
DEFINE_I_CALLBACK (edit_select_all)
{
  o_redraw_cleanstates (w_current);
  o_select_visible_unlocked (w_current);

  i_set_state (w_current, SELECT);
  w_current->inside_action = 0;
  i_update_toolbar (w_current);
  i_update_menus (w_current);
}

/*! \brief Deselect all objects on page.
 * \par Function Description
 * Sets all objects on page as deselected.
 */
DEFINE_I_CALLBACK (edit_deselect)
{
  o_redraw_cleanstates (w_current);
  o_select_unselect_all (w_current);

  i_set_state (w_current, SELECT);
  w_current->inside_action = 0;
  i_update_toolbar (w_current);
  i_update_menus (w_current);
}
/*! \brief De-select initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the De-select toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK (deselect)
{
  if (!w_current->window) return;
  i_callback_edit_deselect(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_copy)
{
  i_update_middle_button(w_current, i_callback_edit_copy, _("Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    i_set_state(w_current, STARTCOPY);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_copy_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  i_update_middle_button(w_current, i_callback_edit_copy_hotkey, _("Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    w_current->event_state = COPY;
    o_copy_start(w_current, wx, wy);
    w_current->event_state = ENDCOPY;
    w_current->inside_action = 1;
  }
}
/*! \brief Copy initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Copy toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_copy)
{
  if (!w_current->window) return;

  i_callback_edit_copy(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_mcopy)
{
  i_update_middle_button(w_current, i_callback_edit_copy, _("Multiple Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    i_set_state(w_current, STARTMCOPY);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_mcopy_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  i_update_middle_button(w_current, i_callback_edit_mcopy_hotkey, _("Multiple Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    w_current->event_state = MCOPY;
    o_copy_start(w_current, wx, wy);
    w_current->event_state = ENDMCOPY;
    w_current->inside_action = 1;
  }
}
/*! \brief Multi-Copy initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Multi-Copy toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_mcopy)
{
  if (!w_current->window) return;

  i_callback_edit_mcopy(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_move)
{
  i_update_middle_button(w_current, i_callback_edit_move, _("Move"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    i_set_state(w_current, STARTMOVE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_move_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  i_update_middle_button(w_current, i_callback_edit_move_hotkey, _("Move"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    o_move_start(w_current, wx, wy);
    w_current->event_state = ENDMOVE;
    w_current->inside_action = 1;
  }
}
/*! \brief Move initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Move toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_move)
{
  if (!w_current->window) return;

  i_callback_edit_move(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_delete)
{
  i_update_middle_button(w_current, i_callback_edit_delete, _("Delete"));

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    o_delete_selected(w_current);
    /* if you delete the objects you must go into select
     * mode after the delete */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    i_update_toolbar(w_current);
    i_update_menus(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_edit)
{
  i_update_middle_button(w_current, i_callback_edit_edit, _("Edit"));
  o_edit(w_current, geda_list_get_glist( w_current->toplevel->page_current->selection_list ) );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_pin_type)
{
  i_update_middle_button (w_current, i_callback_edit_pin_type, _("Edit pin type"));

  x_dialog_edit_pin_type (w_current,
                          geda_list_get_glist (w_current->toplevel->
                                               page_current->selection_list));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_text)
{
  OBJECT *object;

  i_update_middle_button(w_current, i_callback_edit_text, _("Edit Text"));
  object = o_select_return_first_object(w_current);
  if (object) {
    if (object->type == OBJ_TEXT) {
      o_text_edit(w_current, object);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_slot)
{
  OBJECT *object;

  object = o_select_return_first_object(w_current);

  i_update_middle_button(w_current, i_callback_edit_slot, _("Slot"));
  if (object) {
    o_slot_start(w_current, object);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_color)
{
  i_update_middle_button(w_current, i_callback_edit_color, _("Color"));

  color_edit_dialog(w_current);
}

/*! \brief Edit Color initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Move toolbar button.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_color)
{
  i_update_middle_button(w_current, i_callback_edit_color, _("Color"));
  color_edit_dialog(w_current);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function rotate all objects in the selection list by 90 degrees.
 *
 */
DEFINE_I_CALLBACK(edit_rotate_90)
{
  /* If inside an appropriate action, send a button 2 released,
   * so rotating will be handled by x_event.c */
  if ( w_current->inside_action &&
       (w_current->event_state == ENDCOMP ||
        w_current->event_state == ENDTEXT ||
        w_current->event_state == ENDMOVE ||
        w_current->event_state == ENDCOPY ||
        w_current->event_state == ENDMCOPY ||
        w_current->event_state == ENDPASTE )) {
      GdkEvent* event;

      event = gdk_event_new(GDK_BUTTON_RELEASE);
      ((GdkEventButton*) event)->button = 2;
      x_event_button_released (NULL, (GdkEventButton *) event, w_current);
      gdk_event_free(event);

      return;
    }

  i_set_state(w_current, ENDROTATEP);
  i_update_middle_button(w_current, i_callback_edit_rotate_90, _("Rotate"));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function rotate all objects in the selection list by 90 degrees.
 *
 */
DEFINE_I_CALLBACK(edit_rotate_90_hotkey)
{
  GList *object_list;
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  /* If inside an appropriate action, send a button 2 released,
   * so rotating will be handled by x_event.c */
  if ( w_current->inside_action &&
       (w_current->event_state == ENDCOMP ||
        w_current->event_state == ENDTEXT ||
        w_current->event_state == ENDMOVE ||
        w_current->event_state == ENDCOPY ||
        w_current->event_state == ENDMCOPY ||
        w_current->event_state == ENDPASTE )) {
      GdkEvent* event;

      event = gdk_event_new(GDK_BUTTON_RELEASE);
      ((GdkEventButton*) event)->button = 2;
      x_event_button_released (NULL, (GdkEventButton *) event, w_current);
      gdk_event_free(event);

      return;
    }

  o_redraw_cleanstates(w_current);

  object_list = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  if (object_list) {
    i_update_middle_button(w_current,
                           i_callback_edit_rotate_90_hotkey, _("Rotate"));
    /* Allow o_rotate_world_update to redraw the objects */
    o_rotate_world_update(w_current, wx, wy, 90, object_list);
  }

  w_current->event_state = SELECT;
  w_current->inside_action = 0;
  i_update_toolbar(w_current);
}

/*! \brief Rotate initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Rotate toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_rotate)
{
  if (!w_current->window) return;

  i_callback_edit_rotate_90(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_mirror)
{
  i_set_state(w_current, ENDMIRROR);
  i_update_middle_button(w_current, i_callback_edit_mirror, _("Mirror"));
}
/*! \brief Mirror initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Mirror toolbar button.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_mirror)
{
  if (!w_current->window) return;
  i_set_state(w_current, ENDMIRROR);
  i_update_middle_button(w_current, i_callback_edit_mirror, _("Mirror"));
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_mirror_hotkey)
{
  GList *object_list;
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);

  object_list = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  if (object_list) {
    i_update_middle_button(w_current,
                           i_callback_edit_mirror_hotkey, _("Mirror"));
    o_mirror_world_update(w_current, wx, wy, object_list);
  }

  w_current->event_state = SELECT;
  w_current->inside_action = 0;
  i_update_toolbar(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function locks all objects in selection list.
 *
 */
DEFINE_I_CALLBACK(edit_lock)
{
  i_update_middle_button(w_current, i_callback_edit_lock, _("Lock"));

  if (o_select_return_first_object(w_current)) {
    o_lock(w_current);
  }
}
/*! \brief Lock initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Lock toolbar button.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_lock)
{
  if (!w_current->window) return;
  i_update_middle_button(w_current, i_callback_edit_lock, _("Lock"));
  if (o_select_return_first_object(w_current)) {
    o_lock(w_current);
  }
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Thus function unlocks all objects in selection list.
 */
DEFINE_I_CALLBACK(edit_unlock)
{
  i_update_middle_button(w_current, i_callback_edit_unlock, _("Unlock"));
  if (o_select_return_first_object(w_current)) {
    o_unlock(w_current);
  }
}
/*! \brief Unlock initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Unlock toolbar button.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_unlock)
{
  if (!w_current->window) return;
  i_update_middle_button(w_current, i_callback_edit_unlock, _("Unlock"));
  if (o_select_return_first_object(w_current)) {
    o_unlock(w_current);
  }
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_translate)
{
  i_update_middle_button(w_current,
                         i_callback_edit_translate, _("Translate"));

  if (w_current->snap == SNAP_OFF) {
    s_log_message(_("WARNING: Do not translate with snap off!\n"));
    s_log_message(_("WARNING: Turning snap on and continuing "
                  "with translate.\n"));
    w_current->snap = SNAP_GRID;
    i_show_state(w_current, NULL); /* update status on screen */
  }

  if (w_current->snap_size != 100) {
    s_log_message(_("WARNING: Snap grid size is "
                  "not equal to 100!\n"));
    s_log_message(_("WARNING: If you are translating a symbol "
                  "to the origin, the snap grid size should be "
                  "set to 100\n"));
  }

  translate_dialog(w_current);
}

DEFINE_I_CALLBACK(edit_invoke_macro)
{
  gtk_widget_show(w_current->macro_box);
  gtk_widget_grab_focus(w_current->macro_entry);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function embedds all objects in selection list
 *
 */
DEFINE_I_CALLBACK(edit_embed)
{
  OBJECT *o_current;

  i_update_middle_button(w_current, i_callback_edit_embed, _("Embed"));
  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, embed each selected component */
    GList *s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      g_assert (o_current != NULL);
      if ( (o_current->type == OBJ_COMPLEX) ||
	   (o_current->type == OBJ_PICTURE) ) {
        o_embed (w_current->toplevel, o_current);
      }
      s_current = g_list_next(s_current);
    }
    o_undo_savestate(w_current, UNDO_ALL);
  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function unembedds all objects in selection list.
 *
 */
DEFINE_I_CALLBACK(edit_unembed)
{
  OBJECT *o_current;

  i_update_middle_button(w_current, i_callback_edit_unembed, _("Unembed"));
  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, unembed each selected component */
    GList *s_current =
      geda_list_get_glist( w_current->toplevel->page_current->selection_list );

    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      g_assert (o_current != NULL);
      if ( (o_current->type == OBJ_COMPLEX) ||
           (o_current->type == OBJ_PICTURE) ) {
        o_unembed (w_current->toplevel, o_current);
      }
      s_current = g_list_next(s_current);
    }
    o_undo_savestate(w_current, UNDO_ALL);
  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function updates components
 *
 */
DEFINE_I_CALLBACK(edit_update)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *selection;
  GList *selected_components = NULL;
  GList *iter;

  g_return_if_fail (w_current != NULL);

  i_update_middle_button(w_current, i_callback_edit_update, _("Update"));
  if (o_select_selected(w_current)) {

    /* Updating components modifies the selection. Therefore, create a
     * new list of only the OBJECTs we want to update from the current
     * selection, then iterate over that new list to perform the
     * update. */
    selection = geda_list_get_glist (toplevel->page_current->selection_list);
    for (iter = selection; iter != NULL; iter = g_list_next (iter)) {
      OBJECT *o_current = (OBJECT *) iter->data;
      if (o_current != NULL && o_current->type == OBJ_COMPLEX) {
        selected_components = g_list_prepend (selected_components, o_current);
      }
    }
    for (iter = selected_components; iter != NULL; iter = g_list_next (iter)) {
      OBJECT *o_current = (OBJECT *) iter->data;
      iter->data = o_update_component (w_current, o_current);
    }
    g_list_free (selected_components);

  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_show_hidden)
{
  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  i_update_middle_button(w_current,
                         i_callback_edit_show_hidden,
                         _("ShowHidden"));

  o_edit_show_hidden (w_current,
                      s_page_objects (w_current->toplevel->page_current));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_find)
{
  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  find_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_hide_text)
{
  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  hide_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_show_text)
{
  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  show_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_autonumber_text)
{
  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  autonumber_text_dialog(w_current);
}
/*! \brief Auto Number initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Auto Number toolbar button. The
 *  function just launches the Fill Type Dialog.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(autonumber)
{
  if (!w_current->window) return;
  if (w_current->inside_action) return;
  autonumber_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_linetype)
{
  line_type_dialog(w_current);
}
/*! \brief Edit Line Type initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Edit Line toolbar button. The
 *  function just launches the Fill Type Dialog.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_linetype)
{
  if (!w_current->window) return;
  line_type_dialog(w_current);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_filltype)
{
  fill_type_dialog(w_current);
}
/*! \brief Edit Fill Type initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Edit Fill toolbar button. The
 *  function just launches the Fill Type Dialog.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(edit_filltype)
{
  if (!w_current->window) return;
  fill_type_dialog(w_current);
}
/*! \section view-menu View Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut doesn't make sense on redraw, just hit right
 *  button
 */
DEFINE_I_CALLBACK(view_redraw)
{
  o_invalidate_all (w_current);
}
/*! \brief Redraw initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Redraw toolbar button.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK (view_redraw)
{
  if (!w_current->window) return;
  o_invalidate_all (w_current);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_full)
{
  /* scroll bar stuff */
  a_zoom(w_current, ZOOM_FULL_DIRECTIVE, DONTCARE, 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_extents)
{
  /* scroll bar stuff */
  a_zoom_extents (w_current,
                  s_page_objects (w_current->toplevel->page_current), 0);
  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_box)
{
  o_select_unselect_all (w_current);
  o_redraw_cleanstates(w_current);
  w_current->inside_action = 0;
  i_set_state(w_current, ZOOMBOXSTART);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_zoom_box_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, FALSE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  a_zoom_box_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ZOOMBOXEND);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_in)
{
  a_zoom(w_current, ZOOM_IN_DIRECTIVE, MENU, 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_out)
{
  a_zoom(w_current, ZOOM_OUT_DIRECTIVE, MENU, 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try
 *  to do
 */
DEFINE_I_CALLBACK(view_zoom_in_hotkey)
{
  a_zoom(w_current, ZOOM_IN_DIRECTIVE, HOTKEY, 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_out_hotkey)
{
  a_zoom(w_current, ZOOM_OUT_DIRECTIVE, HOTKEY, 0);

  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_pan)
{
  o_redraw_cleanstates(w_current);
  w_current->inside_action = 0;
  i_set_state(w_current, STARTPAN);

  /* I don't know if this would get in the way */
  i_update_middle_button(w_current, i_callback_view_pan, _("Pan"));
}

/*! \brief Scheme callback function that moves the viewport to the left.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_left)
{
  a_pan_mouse(w_current, w_current->keyboardpan_gain, 0);
}

/*! \brief Scheme callback function that moves the viewport to the right.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_right)
{
  /* yes, that's a negative sign there */
  a_pan_mouse(w_current, -w_current->keyboardpan_gain, 0);
}

/*! \brief Scheme callback function that moves the viewport up.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_up)
{
  if(w_current != NULL)
    a_pan_mouse(w_current, 0, w_current->keyboardpan_gain);
}

/*! \brief Scheme callback function that moves the viewport down.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_down)
{
  if(w_current != NULL)
    a_pan_mouse(w_current, 0, -w_current->keyboardpan_gain);
    /* yes, that's a negative sign there */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_pan_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, FALSE, &wx, &wy))
    return;

  i_update_middle_button(w_current, i_callback_view_pan_hotkey, _("Pan"));

  a_pan(w_current, wx, wy);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  }
}

/*! \brief Zoom Pan initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Zoom Pan toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(zoom_pan)
{
  if (!w_current->window) return;
  i_callback_view_pan(w_current, 0, NULL);
}
/*! \brief Zoom Box initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Zoom Box toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(zoom_box)
{
  if (!w_current->window) return;
  i_callback_view_zoom_box(w_current, 0, NULL);
}

/*! \brief Zoom Extents initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Zoom Extents toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(zoom_extents)
{
  if (!w_current->window) return;
  i_callback_view_zoom_extents(w_current, 0, NULL);
}

/*! \brief Zoom In initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Zoom In toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(zoom_in)
{
  if (!w_current->window) return;
  i_callback_view_zoom_in(w_current, 0, NULL);
}

/*! \brief Zoom Out initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Zoom Out toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(zoom_out)
{
  if (!w_current->window) return;
  i_callback_view_zoom_out(w_current, 0, NULL);
}

/*! \brief Zoom Limits initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Zoom Limits toolbar button.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(zoom_limits)
{
  a_zoom(w_current, ZOOM_FULL_DIRECTIVE, DONTCARE, 0);
  if (w_current->undo_panzoom)
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \brief Load the Dark color map scheme
 *  \par Function Description
 *       This function loads the Dark color map scheme
 *       based on user input from the keyboard or menu.
 */
DEFINE_I_CALLBACK (view_dark_colors)
{
  /* Change the scheme here */
  x_load_color_scheme(DARK_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
}

/*! \brief Load the Light color map scheme
 *  \par Function Description
 *       This function loads the Light color map scheme
 *       based on user input from the keyboard or menu.
 */
DEFINE_I_CALLBACK (view_light_colors)
{
  /* Change the scheme here */
  x_load_color_scheme(LIGHT_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
}

/*! \brief Load the BW color map scheme
 *  \par Function Description
 *       This function loads the BW color map scheme
 *       based on user input from the keyboard or menu.
 */
DEFINE_I_CALLBACK (view_bw_colors)
{
  /* Change the scheme here */
  x_load_color_scheme(BW_COLOR_MAP); /* call for load */
  o_invalidate_all (w_current);
}

/*! \section page-menu Page Menu Callback Functions */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_manager)
{
  x_pagesel_open (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_next)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  PAGE *p_current = toplevel->page_current;
  PAGE *p_new;
  GList *iter;

  iter = g_list_find( geda_list_get_glist( toplevel->pages ), p_current );
  iter = g_list_next( iter );

  if (iter == NULL) {
    return;
  }

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_next_page(toplevel->pages, p_current);
  } else {
    p_new = (PAGE *)iter->data;
  }

  if (p_new == NULL || p_new == p_current) {
    return;
  }

  x_window_set_current_page (w_current, p_new);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_prev)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  PAGE *p_current = toplevel->page_current;
  PAGE *p_new;
  GList *iter;

  iter = g_list_find( geda_list_get_glist( toplevel->pages ), p_current );
  iter = g_list_previous( iter );

  if ( iter == NULL  )
    return;

  p_new = (PAGE *)iter->data;

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_prev_page(toplevel->pages, p_current);
  } else {
    p_new = (PAGE *)iter->data;
  }

  if (p_new == NULL || p_new == p_current) {
    return;
  }

  x_window_set_current_page (w_current, p_new);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_new)
{
  PAGE *page;

  /* create a new page */
  page = x_window_open_page (w_current, NULL);
  x_window_set_current_page (w_current, page);
  s_log_message (_("New page created [%s]\n"), page->page_filename);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_close)
{
  if (w_current->toplevel->page_current->CHANGED) {
    x_dialog_close_changed_page (w_current, w_current->toplevel->page_current);
  } else {
    x_window_close_page (w_current, w_current->toplevel->page_current);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \bug may have memory leak?
 */
DEFINE_I_CALLBACK(page_revert)
{
  PAGE *page;
  gchar *filename;
  int page_control;
  int up;
  int response;
  GtkWidget* dialog;

  dialog = gtk_message_dialog_new ((GtkWindow*) w_current->main_window,
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_QUESTION,
                                   GTK_BUTTONS_YES_NO,
                                   _("Really revert page?"));

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_YES,
					  GTK_RESPONSE_NO,
					  -1);

  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (response != GTK_RESPONSE_YES )
    return;

  /* save this for later */
  filename = g_strdup (w_current->toplevel->page_current->page_filename);
  page_control = w_current->toplevel->page_current->page_control;
  up = w_current->toplevel->page_current->up;

  /* delete the page, then re-open the file as a new page */
  s_page_delete (w_current->toplevel, w_current->toplevel->page_current);

  page = x_window_open_page (w_current, filename);

  /* make sure we maintain the hierarchy info */
  page->page_control = page_control;
  page->up = up;

  x_window_set_current_page (w_current, page);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_discard)
{
  x_window_close_page (w_current, w_current->toplevel->page_current);
}

/*! \section page-toolbar Page Toolbar Callback Functions */

/*! \brief Open Page Manager initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Page Manager toolbar button.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(page_manager)
{
  x_pagesel_open (w_current);
}

/*! \brief Create New Page initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Page Manager toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(page_new)
{
  i_callback_page_new(w_current, 0, NULL);
}

/*! \brief Switch to Next Page initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Page Manager toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(page_next)
{
  i_callback_page_next(w_current, 0, NULL);
}
/*! \brief Switch to Previous Page initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Page Manager toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(page_prev)
{
  i_callback_page_prev(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_print)
{
  s_page_print_all(w_current->toplevel);
}

/*! \section clipboard-menu Clipboard Menu Callback Functions */
/*! \brief Copy selection to clipboard.
 *  \par Function Description
 * Copies the current selection to the clipboard, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_copy)
{
  if (!o_select_selected (w_current)) return;

  i_update_middle_button (w_current, i_callback_clipboard_copy,
                          _("Copy to clipboard"));

  o_buffer_copy (w_current, 0);
  x_clipboard_set (w_current, object_buffer[0]);
}

/*! \brief Cut selection to clipboard.
 *  \par Function Description
 * Cut the current selection to the clipboard, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_cut)
{
  if (!o_select_selected (w_current)) return;

  i_update_middle_button (w_current, i_callback_clipboard_cut,
                          _("Cut to clipboard"));

  o_buffer_cut (w_current, 0);
  x_clipboard_set (w_current, object_buffer[0]);
}

/*! \brief Start pasting clipboard contents.
 *  \par Function Description
 * Cut the current selection to the clipboard, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_paste)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *object_list = NULL;

  i_update_middle_button (w_current, i_callback_buffer_paste1, _("Paste from clipboard"));

  object_list = x_clipboard_get (w_current);

  if (object_list != NULL) {
    s_delete_object_glist (toplevel, object_buffer[0]);
    object_buffer[0] = object_list;
    o_redraw_cleanstates (w_current);
    w_current->buffer_number = 0;
    w_current->inside_action = 1;
    i_set_state (w_current, STARTPASTE);
  } else {
    i_set_state_msg (w_current, SELECT, _("Empty buffer"));
  }
}

/*! \brief Start pasting clipboard contents (hotkey version)
 *  \par Function Description
 *  It's not entirely clear what the difference is between this and
 *  i_callback_clipboard_paste()...
 */
DEFINE_I_CALLBACK(clipboard_paste_hotkey)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *object_list = NULL;
  int wx, wy;

  if (!x_event_get_pointer_position (w_current, TRUE, &wx, &wy))
    return;

  object_list = x_clipboard_get (w_current);

  if (object_list == NULL) return;
  s_delete_object_glist (toplevel, object_buffer[0]);
  object_buffer[0] = object_list;

  o_buffer_paste_start (w_current, wx, wy, 0);
}

/*! \section buffer-menu Buffer Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy1)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy1, _("Copy 1"));
  o_buffer_copy(w_current, 0);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy2)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy2, _("Copy 2"));
  o_buffer_copy(w_current, 1);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy3)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy3, _("Copy 3"));
  o_buffer_copy(w_current, 2);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy4)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy4, _("Copy 4"));
  o_buffer_copy(w_current, 3);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy5)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy5, _("Copy 5"));
  o_buffer_copy(w_current, 4);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut1)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut1, _("Cut 1"));
  o_buffer_cut(w_current, 0);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut2)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut2, _("Cut 2"));
  o_buffer_cut(w_current, 1);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut3)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut3, _("Cut 3"));
  o_buffer_cut(w_current, 2);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut4)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut4, _("Cut 4"));
  o_buffer_cut(w_current, 3);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut5)
{
  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut5, _("Cut 5"));
  o_buffer_cut(w_current, 4);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste1)
{
  i_update_middle_button(w_current, i_callback_buffer_paste1, _("Paste 1"));
  if (object_buffer[0] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 0;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste2)
{
  i_update_middle_button(w_current, i_callback_buffer_paste2, _("Paste 2"));
  if (object_buffer[1] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 1;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste3)
{
  i_update_middle_button(w_current, i_callback_buffer_paste3, _("Paste 3"));
  if (object_buffer[2] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 2;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste4)
{
  i_update_middle_button(w_current, i_callback_buffer_paste4, _("Paste 4"));
  if (object_buffer[3] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 3;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste5)
{
  i_update_middle_button(w_current, i_callback_buffer_paste5, _("Paste 5"));
  if (object_buffer[4] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 4;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste1_hotkey)
{
  int wx, wy;

  if (object_buffer[0] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste2_hotkey)
{
  int wx, wy;

  if (object_buffer[1] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste3_hotkey)
{
  int wx, wy;

  if (object_buffer[2] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste4_hotkey)
{
  int wx, wy;

  if (object_buffer[3] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 3);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste5_hotkey)
{
  int wx, wy;

  if (object_buffer[4] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 4);
}

/*! \section clipboard-toolbar Clipboard Tool-Bar Callback Functions */

/*! \brief Cut selection to clipboard initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Clipboard Cut toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(clipboard_cut)
{
  i_callback_clipboard_cut(w_current, 0, NULL);
}
/*! \brief Copy selection to clipboard initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Clipboard Copy toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(clipboard_copy)
{
  i_callback_clipboard_copy(w_current, 0, NULL);
}
/*! \brief Paste selection from clipboard initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Clipboard Paste toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(clipboard_paste)
{
  i_callback_clipboard_paste(w_current, 0, NULL);
}

/*! \section add-menu Add Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_component)
{
  o_redraw_cleanstates (w_current);
  x_compselect_open (w_current);

  i_update_middle_button(w_current,
                         i_callback_add_component, _("Component"));

  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
DEFINE_TB_CALLBACK(add_component)
{
  i_callback_add_component(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_attribute)
{
  attrib_edit_dialog(w_current, NULL, FROM_MENU);
  i_update_middle_button(w_current, i_callback_add_attribute,
                         _("Attribute"));

  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_attribute_hotkey)
{
  attrib_edit_dialog(w_current, NULL, FROM_HOTKEY);
  i_update_middle_button(w_current, i_callback_add_attribute_hotkey,
                         _("Attribute"));

  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
}
/*! \brief Add Attribute initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Add Attribute toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(add_attribute)
{
  i_callback_add_attribute(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_net)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);
  o_net_reset(w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_net, _("Net"));
  i_set_state(w_current, STARTDRAWNET);
  i_update_toolbar(w_current);
  /* somewhere you need to nearest point locking... */
  w_current->inside_action = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_net_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);
  o_net_reset(w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_net_hotkey, _("Net"));
  i_set_state(w_current, STARTDRAWNET);
  i_update_toolbar(w_current);

  o_net_start(w_current, wx, wy);

  w_current->event_state=DRAWNET;
  w_current->inside_action = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
DEFINE_TB_CALLBACK(add_net)
{

  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
    i_callback_add_net(w_current, 0, NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_bus)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_bus, _("Bus"));
  i_set_state(w_current, STARTDRAWBUS);
  i_update_toolbar(w_current);

  /* somewhere you need to nearest point locking... */
  w_current->inside_action = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_bus_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_bus_hotkey, _("Bus"));
  i_set_state(w_current, STARTDRAWBUS);
  i_update_toolbar(w_current);

  o_bus_start(w_current, wx, wy);

  w_current->event_state=DRAWBUS;
  w_current->inside_action = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
DEFINE_TB_CALLBACK(add_bus)
{
  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
     i_callback_add_bus(w_current, 0, NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_text)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  w_current->inside_action = 0;
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);

  text_input_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
DEFINE_TB_CALLBACK(add_text)
{
  if (!w_current->window) return;

  i_callback_add_text(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_line)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_line, _("Line"));
  i_set_state(w_current, DRAWLINE);
  w_current->inside_action = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_line_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_line_hotkey, _("Line"));

  o_line_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDLINE);
}

/*! \brief Add Line initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Add Line toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(add_line)
{
  if (!w_current->window) return;

  i_callback_add_line(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_box)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_box, _("Box"));
  w_current->inside_action = 0;
  i_set_state(w_current, DRAWBOX);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_box_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_box_hotkey, _("Box"));

  o_box_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDBOX);
}

/*! \brief Add Box initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Add Box toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(add_box)
{
  if (!w_current->window) return;

  i_callback_add_box(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_picture)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  w_current->inside_action = 0;
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);

  picture_selection_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_picture_hotkey)
{
  i_callback_add_picture(w_current, 0, NULL);
}

/*! \brief Add Picture initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Add Picture toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(add_picture)
{
  if (!w_current->window) return;

  i_callback_add_picture(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_circle)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_circle, _("Circle"));
  w_current->inside_action = 0;
  i_set_state(w_current, DRAWCIRCLE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_circle_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_circle_hotkey,
                         _("Circle"));

  o_circle_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDCIRCLE);
}

/*! \brief Add Circle initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Add Circle toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(add_circle)
{
  if (!w_current->window) return;

  i_callback_add_circle(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_arc)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_arc, _("Arc"));
  w_current->inside_action = 0;
  i_set_state(w_current, DRAWARC);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_arc_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_arc_hotkey, _("Arc"));

  o_arc_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDARC);
}
/*! \brief Add Arc initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Add Arc toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(add_arc)
{
  if (!w_current->window) return;

  i_callback_add_arc(w_current, 0, NULL);
}
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_pin)
{
  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_pin, _("Pin"));
  w_current->inside_action = 0;
  i_set_state(w_current, DRAWPIN);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_pin_hotkey)
{
  int wx, wy;

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_pin_hotkey, _("Pin"));

  o_pin_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDPIN);
}

/*! \brief Add Pin initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Add Pin toolbar button. The
 *  function just calls the cooresponding Menu handler.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(add_pin)
{
  if (!w_current->window) return;

  i_callback_add_pin(w_current, 0, NULL);
}
/*! \section hierarchy-menu Hierarchy Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(hierarchy_down_schematic)
{
  char *attrib=NULL;
  char *current_filename=NULL;
  int count=0;
  OBJECT *object=NULL;
  PAGE *save_first_page=NULL;
  PAGE *parent=NULL;
  PAGE *child = NULL;
  int loaded_flag=FALSE;
  int page_control = 0;
  int pcount = 0;
  int looking_inside=FALSE;

  g_return_if_fail (w_current != NULL);

  object = o_select_return_first_object(w_current);

  /* only allow going into symbols */
  if (object == NULL || object->type != OBJ_COMPLEX)
    return;

  parent = w_current->toplevel->page_current;
  attrib = o_attrib_search_attached_attribs_by_name (object, "source", count);

  /* if above is null, then look inside symbol */
  if (attrib == NULL) {
    attrib =
      o_attrib_search_inherited_attribs_by_name (object, "source", count);
    looking_inside = TRUE;
#if DEBUG
    printf("going to look inside now\n");
#endif
  }

  while (attrib) {

    /* look for source=filename,filename, ... */
    pcount = 0;
    current_filename = u_basic_breakup_string(attrib, ',', pcount);

    /* loop over all filenames */
    while(current_filename != NULL) {
      GError *err = NULL;
      s_log_message(_("Searching for source [%s]\n"), current_filename);
      child = s_hierarchy_down_schematic_single(w_current->toplevel,
                                                current_filename,
                                                parent,
                                                page_control,
                                                HIERARCHY_NORMAL_LOAD,
                                                &err);

      /* s_hierarchy_down_schematic_single() will not zoom the loaded page */
      if (child != NULL) {
        s_page_goto (w_current->toplevel, child);
        a_zoom_extents(w_current,
                       s_page_objects (w_current->toplevel->page_current),
                       A_PAN_DONT_REDRAW);
        o_undo_savestate(w_current, UNDO_ALL);
        s_page_goto (w_current->toplevel, parent);
      }

      /* save the first page */
      if ( !loaded_flag && (child != NULL)) {
        save_first_page = child;
      }

      /* now do some error fixing */
      if (child == NULL) {
        const char *msg = (err != NULL) ? err->message : "Unknown error.";
        char *secondary =
          g_strdup_printf (_("Failed to descend hierarchy into '%s': %s\n\n"
                             "The gschem log may contain more information."),
                           current_filename, msg);

        s_log_message(_("Failed to descend into '%s': %s\n"),
                      current_filename, msg);

        GtkWidget *dialog =
          gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                                  GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_OK,
                                  _("Failed to descend hierarchy."));
        g_object_set (G_OBJECT (dialog), "secondary-text", secondary, NULL);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        g_free (secondary);
        g_error_free (err);

      } else {
        /* this only signifies that we tried */
        loaded_flag = TRUE;
        page_control = child->page_control;
      }

      g_free(current_filename);
      pcount++;
      current_filename = u_basic_breakup_string(attrib, ',', pcount);
    }

    g_free(attrib);
    g_free(current_filename);

    count++;

    /* continue looking outside first */
    if (!looking_inside) {
      attrib =
        o_attrib_search_attached_attribs_by_name (object, "source", count);
    }

    /* okay we were looking outside and didn't find anything,
     * so now we need to look inside the symbol */
    if (!looking_inside && attrib == NULL && !loaded_flag ) {
      looking_inside = TRUE;
#if DEBUG
      printf("switching to go to look inside\n");
#endif
    }

    if (looking_inside) {
#if DEBUG
      printf("looking inside\n");
#endif
      attrib =
        o_attrib_search_inherited_attribs_by_name (object, "source", count);
    }
  }

  if (loaded_flag && (save_first_page != NULL)) {
    x_window_set_current_page (w_current, save_first_page);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  \bug may cause problems with non-directory symbols
 */
DEFINE_I_CALLBACK(hierarchy_down_symbol)
{
  OBJECT *object;
  const CLibSymbol *sym;

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {
      s_log_message(_("Searching for symbol [%s]\n"),
		    object->complex_basename);
      sym = s_clib_get_symbol_by_name (object->complex_basename);
      if (sym == NULL)
	return;
      if (s_clib_symbol_get_filename(sym) == NULL) {
	s_log_message(_("Symbol is not a real file."
			" Symbol cannot be loaded.\n"));
	return;
      }
      s_hierarchy_down_symbol(w_current->toplevel, sym,
			      w_current->toplevel->page_current);
      /* s_hierarchy_down_symbol() will not zoom the loaded page */
      a_zoom_extents(w_current,
                     s_page_objects (w_current->toplevel->page_current),
                     A_PAN_DONT_REDRAW);
      o_undo_savestate(w_current, UNDO_ALL);
      x_window_set_current_page(w_current, w_current->toplevel->page_current);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(hierarchy_up)
{
  PAGE *up_page;

  up_page = s_hierarchy_find_up_page (w_current->toplevel->pages,
                                      w_current->toplevel->page_current);
  if (up_page == NULL) {
    s_log_message(_("Cannot find any schematics above the current one!\n"));
  } else {
    x_window_set_current_page(w_current, up_page);
  }
}
/*! \section Hierarchy-Toolbar Hierarchy Toolbar Callback Functions */

/*! \brief Hierarchy Down Schematic initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Down Schematic toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(down_schematic)
{
  if (!w_current->window) return;
  i_callback_hierarchy_down_schematic(w_current, 0, NULL);
}

/*! \brief Hierarchy Down Symbol initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Down Symbol toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(down_symbol)
{
  if (!w_current->window) return;
  i_callback_hierarchy_down_symbol(w_current, 0, NULL);
}

/*! \brief Hierarchy Up initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Hierarchy Up toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(hierarchy_up)
{
  if (!w_current->window) return;
  i_callback_hierarchy_up(w_current, 0, NULL);
}
/*! \brief Component Documentation initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the See Component Documentation
 *  toolbar button.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(cdocumentation)
{

  char *attrib_doc = NULL;
  OBJECT *object = NULL;
  GError *error = NULL;
  bool result;

  if (!w_current->window) return;

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {

      /* look for "documentation" */
      attrib_doc = o_attrib_search_object_attribs_by_name (object, "documentation", 0);
      if (attrib_doc) {
        g_type_init();
        //result = x_show_uri (w_current, attrib_doc, &error);
        /* Use this instead until debian-gnome work out thier iceweasel issue */
        result = g_app_info_launch_default_for_uri(attrib_doc, NULL, &error);
        if (result) {
          s_log_message("error: %s", error->message);
          g_error_free (error);
        }
        g_free(attrib_doc);
      }
    }
  } else {
    s_log_message(_("No component selected"));

  }
}

/*! \section attributes-menu Attributes Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_attach)
{
  OBJECT *first_object;
  GList *s_current;
  GList *attached_objects = NULL;

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  /* do we want to update the shortcut outside of the ifs? */
  /* probably, if this fails the user may want to try again */
  i_update_middle_button(w_current, i_callback_attributes_attach,
                         _("Attach"));

  /* skip over head */
  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );
  if (!s_current) {
    return;
  }

  first_object = (OBJECT *) s_current->data;
  if (!first_object) {
    return;
  }

  /* skip over first object */
  s_current = g_list_next(s_current);
  while (s_current != NULL) {
    OBJECT *object = s_current->data;
    if (object != NULL) {
      o_attrib_attach (w_current->toplevel, object, first_object, TRUE);
      attached_objects = g_list_prepend (attached_objects, object);
      w_current->toplevel->page_current->CHANGED=1;
    }
    s_current = g_list_next(s_current);
  }

  if (attached_objects != NULL) {
    g_run_hook_object_list (w_current, "%attach-attribs-hook",
                            attached_objects);
    g_list_free (attached_objects);
  }

  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_detach)
{
  GList *s_current;
  OBJECT *o_current;
  GList *detached_attribs = NULL;

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  /* same note as above on i_update_middle_button */
  i_update_middle_button(w_current, i_callback_attributes_detach,
                         _("Detach"));

  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );
  while (s_current != NULL) {
    o_current = (OBJECT *) s_current->data;
    if (o_current) {
      if (o_current->attribs) {
        detached_attribs = g_list_concat (g_list_copy (o_current->attribs),
                                          detached_attribs);
        o_attrib_detach_all (w_current->toplevel, o_current);
        w_current->toplevel->page_current->CHANGED=1;
      }
    }
    s_current = g_list_next(s_current);
  }

  if (detached_attribs != NULL) {
    g_run_hook_object_list (w_current, "%detach-attribs-hook",
                            detached_attribs);
    g_list_free (detached_attribs);
  }

  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_name)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_name,
                         _("ShowN"));

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      if (object->type == OBJ_TEXT)
        o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_value)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_value,
                         _("ShowV"));

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      if (object->type == OBJ_TEXT)
        o_attrib_toggle_show_name_value (w_current, object, SHOW_VALUE);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_both)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_both,
                         _("ShowB"));

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      if (object->type == OBJ_TEXT)
        o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME_VALUE);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_visibility_toggle)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current,
                         i_callback_attributes_visibility_toggle,
                         _("VisToggle"));

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      if (object->type == OBJ_TEXT)
        o_attrib_toggle_visibility (w_current, object);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }
}

/*! \section attributes-toolbar Attributes Toolbar Callback Functions */

/*! \brief Attach Attribute initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Attach Attribute toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(attach_attributes)
{
  if (!w_current->window) return;
  i_callback_attributes_attach(w_current, 0, NULL);
}

/*! \brief Dettach Attribute initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Dettach Attribute toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(detach_attributes)
{
  if (!w_current->window) return;
  i_callback_attributes_detach(w_current, 0, NULL);
}

/*! \brief Attributes Show Value initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Dettach Attribute toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(attributes_show_value)
{
  if (!w_current->window) return;
  i_callback_attributes_show_value(w_current, 0, NULL);
}

/*! \brief Attributes Show Name initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Attributes Show Name toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(attributes_show_name)
{
  if (!w_current->window) return;
  i_callback_attributes_show_name(w_current, 0, NULL);
}

/*! \brief Attributes Show Both Name and Value initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Attributes Show Both toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(attributes_show_both)
{
  if (!w_current->window) return;
  i_callback_attributes_show_both(w_current, 0, NULL);
}

/*! \brief Attributes Visibility Toggle initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Attributes Visibility Toggle toolbar button. The
 *  function just calls the corresponding Menu handler.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(attributes_visibility)
{
  if (!w_current->window) return;
  i_callback_attributes_visibility_toggle(w_current, 0, NULL);
}

/*! \brief Find Attribute initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Find Attribute Toggle toolbar button.
 *
 *  \note   hide_text_dialog(w_current);
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(find_attribute)
{
  if (!w_current->window) return;
  if (w_current->inside_action) return;

  find_text_dialog(w_current);
}
/*! \brief Hide Text initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Hide Text Toggle toolbar button.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(hide_text)
{
  if (!w_current->window) return;
  if (w_current->inside_action) return;

  hide_text_dialog(w_current);
}
/*! \brief Show Text initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Show Text Toggle toolbar button.
 *
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(show_text)
{
  if (!w_current->window) return;
  if (w_current->inside_action) return;

  show_text_dialog(w_current);
}
/*! \section script-menu Script Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  not currently implemented
 */
DEFINE_I_CALLBACK(script_console)
{
  s_log_message(_("Sorry but this is a non-functioning menu option\n"));
}

/*! \section layers-menu Layers Menu Callback Functions */

/*! \section options-menu Options Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat last command doesn't make sense on options either??? (does it?)
 */
DEFINE_I_CALLBACK(options_text_size)
{
  if(w_current != NULL)
    text_size_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_snap_size)
{
  if(w_current != NULL)
    snap_size_dialog(w_current);
}

/*! \brief Multiply by two the snap grid size.
 *  \par Function Description
 *  Callback function for the scale-up snap grid size hotkey.
 *  Multiply by two the snap grid size.
 */
DEFINE_I_CALLBACK(options_scale_up_snap_size)
{
  w_current->snap_size *= 2;
  w_current->toplevel->page_current->CHANGED=1;  /* maybe remove those two lines */
  o_undo_savestate(w_current, UNDO_ALL);

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
}

/*! \brief Divide by two the snap grid size.
 *  \par Function Description
 *  Callback function for the scale-down snap grid size hotkey.
 *  Divide by two the snap grid size (if it's and even number).
 */
DEFINE_I_CALLBACK(options_scale_down_snap_size)
{
  if (w_current->snap_size % 2 == 0)
    w_current->snap_size /= 2;
  w_current->toplevel->page_current->CHANGED=1;  /* maybe remove those two lines */
  o_undo_savestate(w_current, UNDO_ALL);

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat last command doesn't make sense on options either??? (does
 *  it?)
 */
DEFINE_I_CALLBACK(options_afeedback)
{
  if (w_current->action_feedback_mode == BOUNDINGBOX) {
    w_current->action_feedback_mode = OUTLINE;
    s_log_message(_("Action feedback mode set to OUTLINE\n"));
  } else {
    w_current->action_feedback_mode = BOUNDINGBOX;
    s_log_message(_("Action feedback mode set to BOUNDINGBOX\n"));
  }
  if (w_current->inside_action &&
      w_current->toplevel->page_current->place_list != NULL)
    o_place_invalidate_rubber (w_current, FALSE);

  x_menu_set_toggle(w_current, OUTLINE_TOGGLE, w_current->action_feedback_mode);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_grid)
{
  switch (w_current->grid_mode) {
    case GRID_NONE: w_current->grid_mode = GRID_DOTS; break;
    case GRID_DOTS: w_current->grid_mode = GRID_MESH; break;
    case GRID_MESH: w_current->grid_mode = GRID_NONE; break;
  }

  switch (w_current->grid_mode) {
    case GRID_NONE: s_log_message (_("Grid OFF\n"));           break;
    case GRID_DOTS: s_log_message (_("Dot grid selected\n"));  break;
    case GRID_MESH: s_log_message (_("Mesh grid selected\n")); break;
  }

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_snap)
{
  /* toggle to the next snap state */
  w_current->snap = (w_current->snap+1) % SNAP_STATE_COUNT;

  switch (w_current->snap) {
  case SNAP_OFF:
    s_log_message(_("Snap OFF (CAUTION!)\n"));
    x_menu_set_toggle(w_current, SNAP_TOGGLE, FALSE);
    break;
  case SNAP_GRID:
    x_menu_set_toggle(w_current, SNAP_TOGGLE, TRUE);
    s_log_message(_("Snap ON\n"));
    break;
  case SNAP_RESNAP:
    x_menu_set_toggle(w_current, SNAP_TOGGLE, TRUE);
    s_log_message(_("Snap back to the grid (CAUTION!)\n"));
    break;
  default:
    g_critical("options_snap: toplevel->snap out of range: %d\n", w_current->snap);
  }
  i_show_state(w_current, NULL);  /* update status on screen */
  i_update_grid_info (w_current); /* update on screen grid status */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Rubber band is cool !
 *  Added on/off option from the pull down menu
 *  Chris Ellec - January 2001
 */
DEFINE_I_CALLBACK(options_rubberband)
{
  if (w_current->netconn_rubberband) {
    w_current->netconn_rubberband = 0;
    s_log_message(_("Rubber band OFF \n"));
  } else {
    w_current->netconn_rubberband = 1;
    s_log_message(_("Rubber band ON\n"));
  }
  x_menu_set_toggle(w_current, RUBBER_TOGGLE, w_current->netconn_rubberband);
}


/*! \brief callback function for setting the magnetic net option
 *  \par Function Description
 *  This function just toggles a variable to switch the magnetic net
 *  mode ON and OFF
 */
DEFINE_I_CALLBACK(options_magneticnet)
{
  if ((w_current->magnetic_net_mode = !w_current->magnetic_net_mode)) {
    s_log_message(_("magnetic net mode: ON\n"));
  }
  else {
    s_log_message(_("magnetic net mode: OFF\n"));
  }
  x_menu_set_toggle(w_current, MAGNETIC_TOGGLE, w_current->magnetic_net_mode);
  i_show_state(w_current, NULL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_show_log_window)
{
   x_log_open ();
}

/*!
 *  Author: Wiley E. Hill
 *  Date:   Aug 5th, 2012
 *
 *  Description: This function defines a procedure to be called
 *               when the menu option "configure_settings" is selected.
 *
 */
DEFINE_I_CALLBACK(configure_settings)
{
   x_configure_settings(w_current); /* Load and display Dialog */
}
/*! \brief Launch Settings Dialog initiated by ToolBar Button
 *  \par Function Description
 *  This is a callback function for the Preferences toolbar button. The
 *  function calls the dialog constructor.
 *  \note
 *  The widget parameter in this function is a ptr to the toolbar button!
 */
DEFINE_TB_CALLBACK(configure_settings)
{
   x_configure_settings(w_current); /* Load and display Dialog */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is Ales' catch all misc callback
 */
DEFINE_I_CALLBACK(misc)
{
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is Ales' second catch all misc callback
 */
DEFINE_I_CALLBACK(misc2)
{
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is Ales' third catch all misc callback
 */
DEFINE_I_CALLBACK(misc3)
{
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  HACK: be sure that you don't use the widget parameter in this one,
 *  since it is being called with a null, I suppose we should call it
 *  with the right param.
 */
DEFINE_I_CALLBACK(cancel)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GValue value = { 0, };

  if (w_current->event_state == ENDCOMP &&
      w_current->cswindow) {
    /* user hit escape key when placing components */

    /* Undraw any outline of the place list */
    o_place_invalidate_rubber (w_current, FALSE);
    w_current->rubber_visible = 0;

    /* De-select the lists in the component selector */
    x_compselect_deselect (w_current);

    /* Present the component selector again */
    g_value_init (&value, G_TYPE_BOOLEAN);
    g_value_set_boolean (&value, FALSE);
    g_object_set_property (G_OBJECT(w_current->cswindow), "hidden", &value);
  }

  if (w_current->inside_action) {
    /* If we're cancelling from a move action, re-wind the
     * page contents back to their state before we started */
    if (w_current->event_state == MOVE ||
        w_current->event_state == ENDMOVE)
      o_move_cancel (w_current);

    /* If we're cancelling from a grip action, call the specific cancel
     * routine to reset the visibility of the object being modified */
    if (w_current->event_state == GRIPS)
      o_grips_cancel (w_current);
  }

  /* Free the place list and its contents. If we were in a move
   * action, the list (refering to objects on the page) would
   * already have been cleared in o_move_cancel(), so this is OK. */
  s_delete_object_glist(toplevel, toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  /* leave this on for now... but it might have to change */
  /* this is problematic since we don't know what the right mode */
  /* (when you cancel inside an action) should be */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);

  /* clear the key guile command-sequence */
  g_keys_reset (w_current);

  if (w_current->inside_action) {
     o_invalidate_all (w_current);
  }

  w_current->inside_action=0;
}

/*! \section help-menu Help Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(help_about)
{
  about_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(help_hotkeys)
{
  x_dialog_hotkeys(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_show_coord_window)
{
  coord_dialog (w_current, 0, 0);
}

/* these is a special wrapper function which cannot use the above */
/* DEFINE_I_CALLBACK macro */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  When invoked (via signal delete_event), closes the current window
 *  if this is the last window, quit gschem
 *  used when you click the close button on the window which sends a DELETE
 *  signal to the app
 */
bool i_callback_close_wm ( GtkWidget *widget, GdkEvent *event,
                           GSCHEM_TOPLEVEL* w_current )
{
  x_window_close(w_current);

  /* stop further propagation of the delete_event signal for window: */
  /*   - if user has cancelled the close the window should obvioulsy */
  /*   not be destroyed */
  /*   - otherwise window has already been destroyed, nothing more to */
  /*   do */
  return TRUE;
}
