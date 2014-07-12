/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
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

#include <config.h>

#include "gschem.h"
#include <geda_label.h>

/*! \brief Update status bar string
 *
 *  \par Function Description
 *  This function updates the status bar widget with the new string.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] string The new string to be shown in the status bar
 */
static void i_update_status(GschemToplevel *w_current, const char *string)
{
  if (!StatusBar->status_label)
    return;

  if (string) {
    /* NOTE: consider optimizing this if same label */
    geda_label_widget_set_text(StatusBar->status_label, (char*) string);
  }
}

/*! \brief Get string corresponding to the currently selected mode
 *
 *  \par Function Description
 *  Returns a string describing the currently
 *  selected mode.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \returns a string that will only last until the next time
 *   the function is called (which is probably just fine, really)
 *   *EK* Egil Kvaleberg
 */
static const char *i_status_string(GschemToplevel *w_current)
{
  static char *buf = 0;

  switch ( w_current->event_state ) {
    case NONE:
    case STARTROUTENET: /*! \todo */
    case ENDROUTENET: /*! \todo */
      return "";
    case STARTDESELECT:
    case DESELECT:
      return _("Deselect Mode");
    case STARTSELECT:
    case SELECT:
    case SBOX:
    case GRIPS:
      return _("Select Mode");
    case ENDCOMP:
      return _("Component Mode"); /*EK* new */
    case ENDTEXT:
      return _("Text Mode"); /*EK* new */
    case STARTCOPY:
    case ENDCOPY:
      return _("Copy Mode");
    case STARTMOVE:
    case ENDMOVE:
      return _("Move Mode");
    case ENDROTATEP:
      return _("Rotate Mode");
    case ENDMIRROR:
      return _("Mirror Mode");
    case ZOOM:
    case ZOOMBOXEND:
    case ZOOMBOXSTART:
      return _("Zoom Box");
    case STARTPAN:
    case PAN:
    case MOUSEPAN:
      return _("Pan Mode");
    case STARTPASTE:
    case ENDPASTE:
      GEDA_FREE(buf);
      buf = g_strdup_printf(_("Paste %d Mode"), w_current->buffer_number+1);
      return buf;
    case STARTDRAWNET:
    case DRAWNET:
    case NETCONT:
      if (w_current->magnetic_net_mode)
        return _("Magnetic Net Mode");
      else
        return _("Net Mode");
    case STARTDRAWBUS:
    case DRAWBUS:
    case BUSCONT:
      return _("Bus Mode");
    case DRAWLINE:
    case ENDLINE:
      return _("Line Mode");
    case DRAWPATH:
    case PATHCONT:
    case ENDPATH:
      return _("Path Mode");
    case DRAWBOX:
    case ENDBOX:
      return _("Box Mode");
    case DRAWPICTURE:
    case ENDPICTURE:
      return _("Picture Mode");
    case DRAWCIRCLE:
    case ENDCIRCLE:
      return _("Circle Mode");
    case DRAWARC:
    case ENDARC:
      return _("Arc Mode");
    case DRAWPIN:
    case ENDPIN:
      return _("Pin Mode");
    case COPY:
      return _("Copy");
    case MOVE:
      return _("Move");
    case MCOPY:
      return _("Multiple Copy");
    case STARTMCOPY:
    case ENDMCOPY:
      return _("Multiple Copy Mode");
  }
#if DEBUG
  fprintf(stderr, "i_status_string: Invalid state <%d>\n", w_current->event_state);
#endif

  return ""; /* should not happen */
}

/*! \brief Show state field
 *
 *  \par Function Description
 *  Show state field on screen, possibly with the
 *  addition of an extra message
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] message The string to be displayed
 */
void i_show_state(GschemToplevel *w_current, const char *message)
{
  GedaToplevel *toplevel = w_current->toplevel;
  char *what_to_say;
  const char *array[5] = { NULL };
  int i = 3; /* array[4] must be NULL */

  /* Fill in the string array */
  array[i--] = i_status_string(w_current);

  if(toplevel->page_current->show_hidden_text)
    array[i--] = _("Show Hidden");

  if(w_current->snap == SNAP_OFF)
    array[i--] = _("Snap Off");
  else if (w_current->snap == SNAP_RESNAP)
    array[i--] = _("Resnap Active");

  if(message && message[0])
    array[i] = message;

  /* Skip over NULLs */
  while(array[i] == NULL)
    i++;

  what_to_say = g_strjoinv(" - ", (char **) array + i);

  if(w_current->keyaccel_string) {
     char *p = what_to_say;

     what_to_say = g_strdup_printf("%s \t\t %s", w_current->keyaccel_string,
           what_to_say);
     GEDA_FREE(p);
  }

  i_update_status(w_current, what_to_say);
  GEDA_FREE(what_to_say);
}

/*! \brief Set new state, then show state field including some
 *         message
 *
 *  \par Function Description
 *  Set new state, then show state field including some
 *  message.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] newstate The new state
 *  \param [in] message Message to be shown
 *   *EK* Egil Kvaleberg
 */
void i_set_state_msg(GschemToplevel *w_current, enum x_states newstate, const char *message)
{
  w_current->event_state = newstate;
  x_toolbars_update(w_current);
  i_show_state(w_current, message);
}

/*! \brief Set new state, then show state field
 *
 *  \par Function Description
 *  Set new state, then show state field.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] newstate The new state
 *   *EK* Egil Kvaleberg
 */
void i_set_state(GschemToplevel *w_current, enum x_states newstate)
{
  i_set_state_msg(w_current, newstate, NULL);
}

/*! \brief Update sensitivity of the Edit/Paste menu item
 *
 *  \par Function Description
 *  Asynchronous callback to update sensitivity of the Edit/Paste
 *  menu item.
 */
static void clipboard_usable_cb (int usable, void *userdata)
{
  GschemToplevel *w_current = userdata;
  x_menus_sensitivity (w_current, "_Edit/_Paste", usable);
  x_toolbars_set_sensitivities(w_current, CAN_PASTE, usable);
  x_menus_popup_sensitivity (w_current, "/Paste", usable);
}

static bool
selected_at_least_one_text_object(GschemToplevel *w_current)
{
  Object *obj;
  GedaToplevel *toplevel = w_current->toplevel;
  GList *list = geda_list_get_glist(toplevel->page_current->selection_list);

  while(list != NULL) {
    obj = (Object *) list->data;
    if (obj->type == OBJ_TEXT)
      return TRUE;
    NEXT(list);
  }
  return FALSE;
}
static bool
selected_complex_object(GschemToplevel *w_current)
{
  Object *obj;
  GedaToplevel *toplevel = w_current->toplevel;
  GList *list = geda_list_get_glist(toplevel->page_current->selection_list);

  while(list != NULL) {
    obj = (Object *) list->data;
    if (obj->type == OBJ_COMPLEX)
      return TRUE;
    NEXT(list);
  }
  return FALSE;
}

/*! \brief Update sensitivity of relevant menu items
 *
 *  \par Function Description
 *  Update sensitivity of relevant menu & toolbar items.
 *
 *  \param [in] w_current GschemToplevel structure
 */
void i_update_sensitivities(GschemToplevel *w_current)
{
  bool have_text_selected;
  bool have_mutil_pages;
  bool is_complex_selected;
  bool anything_is_selected;

  GedaToplevel *toplevel = w_current->toplevel;

  if (w_current == NULL) {
    u_log_message("Internal Error Detected: <i_update_sensitivities> w_current == NULL\n");
    return;
  }
  if (toplevel->page_current == NULL) {
    u_log_message("Internal Error Detected: <i_update_sensitivities> toplevel->page_current == NULL\n");
    return;
  }

  /* This is improve but still fairly simplistic.  What
   * gets enabled/disabled could be more selective based
   * based on what is in the selection list, WEH
   */
  x_clipboard_query_usable (w_current, clipboard_usable_cb, w_current);

  have_mutil_pages     = g_list_length(geda_list_get_glist(toplevel->pages)) > 1 ? TRUE : FALSE;
  anything_is_selected = o_select_is_selection (w_current);
  is_complex_selected  = selected_complex_object(w_current);
  have_text_selected   = selected_at_least_one_text_object(w_current);


  if ( have_mutil_pages ) {
    x_menus_sensitivity(w_current, "_Page/_Next", TRUE);
    x_menus_sensitivity(w_current, "_Page/_Previous", TRUE);
  }
  else {
    x_menus_sensitivity(w_current, "_Page/_Next", FALSE);
    x_menus_sensitivity(w_current, "_Page/_Previous", FALSE);
  }

  if ( anything_is_selected ) {

    /* since one or more things are selected, we set these TRUE */
    /* These strings should NOT be internationalized */
    if ( is_complex_selected ) {

      x_menus_sensitivity(w_current, "Hie_rarchy/_Down Schematic", TRUE);
      x_menus_sensitivity(w_current, "Hie_rarchy/Down _Symbol", TRUE);
      x_menus_sensitivity(w_current, "Hie_rarchy/D_ocumentation...", TRUE);
      x_menus_sensitivity(w_current, "A_ttributes/_Attach", TRUE);
      x_menus_sensitivity(w_current, "A_ttributes/_Detach", TRUE);

      x_menus_sensitivity(w_current, "_Edit/Slot...", TRUE);
      x_menus_sensitivity(w_current, "_Edit/Edit Pin...", TRUE);
      x_menus_sensitivity(w_current, "_Edit/Unembed Component/Picture", TRUE);
      x_menus_sensitivity(w_current, "_Edit/Update Component", TRUE);

      x_menus_popup_sensitivity(w_current, "Edit pin type...", TRUE);
      x_menus_popup_sensitivity(w_current, "Down Schematic", TRUE);
      x_menus_popup_sensitivity(w_current, "Down Symbol", TRUE);
      /* x_menus_popup_sensitivity(w_current, "/Up", TRUE); */
    }

    if(have_text_selected) {
        x_toolbars_set_sensitivities(w_current, TEXT_ObjectS, TRUE);
        x_menus_sensitivity(w_current, "A_ttributes/Show _Value", TRUE);
        x_menus_sensitivity(w_current, "A_ttributes/Show _Name", TRUE);
        x_menus_sensitivity(w_current, "A_ttributes/Show _Both", TRUE);
        x_menus_sensitivity(w_current, "A_ttributes/_Toggle Visibility", TRUE);
    }

    x_menus_sensitivity(w_current, "_Edit/Cu_t", TRUE);
    x_menus_sensitivity(w_current, "_Edit/_Copy", TRUE);
    x_menus_sensitivity(w_current, "_Edit/_Delete", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Copy Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Multiple Copy Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Move Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Rotate 90 Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Mirror Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Edit...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Edit Text...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Color...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Lock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Fill Type...", TRUE);

    x_menus_sensitivity(w_current, "_Buffer/Copy into 1", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 2", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 3", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 4", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 5", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 1", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 2", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 3", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 4", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 5", TRUE);

    x_menus_sensitivity(w_current, "_Edit/Cu_t", TRUE);
    x_menus_sensitivity(w_current, "_Edit/_Copy", TRUE);
    x_menus_sensitivity(w_current, "_Edit/_Delete", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Copy Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Multiple Copy Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Move Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Rotate 90 Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Mirror Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Edit...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Edit Text...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Color...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Lock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Fill Type...", TRUE);

    x_menus_popup_sensitivity(w_current, "Cut", TRUE);
    x_menus_popup_sensitivity(w_current, "Copy", TRUE);

    x_menus_popup_sensitivity(w_current, "Edit...", TRUE);
    x_menus_popup_sensitivity(w_current, "Duplicate", TRUE);
    x_menus_popup_sensitivity(w_current, "Move", TRUE);
    x_menus_popup_sensitivity(w_current, "Delete", TRUE);

  }
  else { /* Nothing is selected, grey these out */
    /* These strings should NOT be internationalized */

    x_menus_sensitivity(w_current, "_Edit/Cu_t", FALSE);
    x_menus_sensitivity(w_current, "_Edit/_Copy", FALSE);
    x_menus_sensitivity(w_current, "_Edit/_Delete", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Copy Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Multiple Copy Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Move Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Rotate 90 Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Mirror Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit Text...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Slot...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Color...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit Pin...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Lock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Fill Type...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Unembed Component/Picture", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Update Component", FALSE);

    x_menus_sensitivity(w_current, "_Buffer/Copy into 1", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 2", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 3", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 4", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 5", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 1", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 2", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 3", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 4", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 5", FALSE);

    x_menus_sensitivity(w_current, "Hie_rarchy/_Down Schematic", FALSE);
    x_menus_sensitivity(w_current, "Hie_rarchy/Down _Symbol", FALSE);
    x_menus_sensitivity(w_current, "Hie_rarchy/D_ocumentation...", FALSE);

    x_menus_sensitivity(w_current, "A_ttributes/_Attach", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/_Detach", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Value", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Name", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Both", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/_Toggle Visibility", FALSE);

    /*  Menu items for hierarchy added by SDB 1.9.2005.  */
    x_menus_popup_sensitivity(w_current, "Down Schematic", FALSE);
    x_menus_popup_sensitivity(w_current, "Down Symbol", FALSE);
    /* x_menus_popup_sensitivity(w_current, "/Up", FALSE);	*/

    x_menus_popup_sensitivity(w_current, "Edit...", FALSE);
    x_menus_popup_sensitivity(w_current, "Edit pin type...", FALSE);
    x_menus_popup_sensitivity(w_current, "Duplicate", FALSE);
    x_menus_popup_sensitivity(w_current, "Move", FALSE);
    x_menus_popup_sensitivity(w_current, "Delete", FALSE);

    x_menus_popup_sensitivity(w_current, "Cut", FALSE);
    x_menus_popup_sensitivity(w_current, "Copy", FALSE);
  }

  x_menus_sensitivity(w_current, "_Buffer/Paste from 1", (object_buffer[1] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 2", (object_buffer[2] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 3", (object_buffer[3] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 4", (object_buffer[4] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 5", (object_buffer[5] != NULL));

  /* Update sensitivities on the Toolbars */
  x_toolbars_set_sensitivities (w_current, SOME_ObjectS,    anything_is_selected);
  x_toolbars_set_sensitivities (w_current, COMPLEX_ObjectS, is_complex_selected);
  x_toolbars_set_sensitivities (w_current, HAVE_PageS,      have_mutil_pages);
  x_toolbars_set_sensitivities (w_current, TEXT_ObjectS,    have_text_selected);

}

/*! \brief Set filename as gschem window title
 *
 *  \par Function Description
 *  Set filename as gschem window title using
 *  the gnome HID format style.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] string The filename
 */
void i_set_filename(GschemToplevel *w_current, const char *string)
{
  char *print_string=NULL;
  char *filename=NULL;

  if (!w_current->main_window)
    return;
  if (string == NULL)
    return;

  filename = g_path_get_basename(string);

  print_string = g_strdup_printf("%s - gschem", filename);

  gtk_window_set_title(GTK_WINDOW(w_current->main_window), print_string);

  GEDA_FREE(print_string);
  GEDA_FREE(filename);
}

/*! \brief Update the Grid and Snap Display on the gschem Status-Bar
 *
 *  \par Function Description
 *  This function calls the appropriate interface to update the Grid/Snap
 *  label on the status bar.
 *
 *  \param [in] w_current GschemToplevel structure
 */
/*
 * i_pan_world.c:159        i_pan_world_general()
 * i_command.c:2665   i_cmd_do_grid_dots()
 * i_command.c:2674   i_cmd_do_grid_mesh()
 * i_command.c:2683   i_cmd_do_grid_off()
 * i_command.c:2705   i_cmd_do_cycle_grid()
 *
 */
void i_update_grid_info (GschemToplevel *w_current)
{

  x_status_bar_update_grid_label (w_current);

}
