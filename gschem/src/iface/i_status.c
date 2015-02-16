/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: i_status.c
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

#include "gschem.h"
#include "x_menus.h"

#include <geda_label.h>
#include <geda_debug.h>

/** \defgroup Gschem-Status-System Status System
 *  @{
 * \par
 * This module contains routines to update user interface components,
 * i.e. the menus, status-bar, and tool-bars. The module does not set
 * values in widgets, or interact with GUI components, functions in
 * this module call the appropriate GUI providers to perform updates.
 *
*/

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
void i_status_set_state_msg(GschemToplevel *w_current,
                            enum            x_states newstate,
                            const char     *message)
{
  /* The add picture mode is unique because this mode is the only
   * "adder" requiring resources to be freed when transitioning to
   *  any other state */
  if ((w_current->event_state == DRAWPICTURE) ||
      (w_current->event_state == ENDPICTURE))
  {
    if (w_current->current_pixbuf != NULL) {
      GEDA_UNREF(w_current->current_pixbuf);
      w_current->current_pixbuf = NULL;
    }

    GEDA_FREE(w_current->pixbuf_filename);
  }

  if ((newstate != w_current->event_state) || (message != NULL)) {
    w_current->event_state = newstate;
    x_toolbars_update (w_current);
    i_status_show_state (w_current, message);
  }
}

/*! \brief Set new state, then show state field
 *
 *  \par Function Description
 *  Wrapper for i_status_set_state_msg, set new state without specifing
 *  and message string.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] newstate The new state
 *   *EK* Egil Kvaleberg
 */
void i_status_set_state(GschemToplevel *w_current, enum x_states newstate)
{
  i_status_set_state_msg (w_current, newstate, NULL);
}


/*! \brief Update status bar string
 *
 *  \par Function Description
 *  This function updates the status bar widget with the new string.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] string The new string to be shown in the status bar
 */
static void i_status_update_status(GschemToplevel *w_current,
                                   const char *string)
{
  if (!StatusBar->status_label)
    return;

  if (string)
    if (strcmp(geda_label_widget_get_text(StatusBar->status_label), string) != 0) {
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
    case ENDROTATE:
      return _("Rotate Mode");
    case ENDMIRROR:
      return _("Mirror Mode");
    case ZOOM:
    case ZOOMBOXEND:
    case ZOOMBOXSTART:
      return _("Zoom Box");
    case PAN:
      return _("Pan Mode");
    case STARTPASTE:
    case ENDPASTE:
      GEDA_FREE(buf);
      buf = u_string_sprintf(_("Paste %d Mode"), w_current->buffer_number+1);
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
    case STARTBREAK:
      return _("First point?");
    case ENDBREAK:
      return _("Second point?");
    case STARTEXTEND:
      return _("Project Mode");
    case EXTEND:
      return _("Select Projectiles");
    case ENDEXTEND:
      return _("Select Object");
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

#if DEBUG_STATUS

  fprintf(stderr, "%s: Invalid state <%d>\n", __func__, w_current->event_state);

#endif

  return ""; /* should not happen */
}

/*! \brief Show state field
 *
 *  \par Function Description
 *  Show state field in the status bar, possibly with the addition
 *  of an extra message.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] message The string to be displayed
 */
void i_status_show_state(GschemToplevel *w_current, const char *message)
{
  GedaToplevel *toplevel = w_current->toplevel;
  const char   *array[5] = { NULL };
  char         *what_to_say;

  int i = 3; /* array[4] must be NULL */

  /* Fill in the string array */
  array[i--] = i_status_string(w_current);

  if (toplevel->page_current->show_hidden_text)
    array[i--] = _("Show Hidden");

  if (w_current->snap == SNAP_OFF)
    array[i--] = _("Snap Off");
  else if (w_current->snap == SNAP_RESNAP)
    array[i--] = _("Resnap Active");

  if (message && message[0])
    array[i] = message;

  /* Skip over NULLs */
  while(array[i] == NULL)
    i++;

  what_to_say = g_strjoinv(" - ", (char **) array + i);

  if(w_current->keyaccel_string) {

     char *ptr = what_to_say;

     what_to_say = u_string_sprintf("%s\t\t %s", w_current->keyaccel_string,
                                                 what_to_say);
     GEDA_FREE(ptr);
  }

  i_status_update_status(w_current, what_to_say);
  GEDA_FREE(what_to_say);
}

/*! \brief Update Coordinate Display
 *
 *  \par Function Description
 *  Spawn thread to update the Grid and Snap Display
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] w_x       Current x coordinate of pointer in world units.
 *  \param [in] w_y       Current y coordinate of pointer in world units.
 *
 */
void i_status_update_coordinates(GschemToplevel *w_current, int w_x, int w_y)
{
  /* If coordinate display is OFF we do not need to do */
  if (gschem_status_bar_get_coord_mode(w_current->status_bar)) {

    int x1, y1;

    if (w_current->inside_action) {
      x1 = w_current->first_wx;
      y1 = w_current->first_wy;
    }
    else {
      x1 = -0;
      y1 = -0;
    }

    gschem_status_bar_set_coordinates(w_current->status_bar, x1, y1, w_x, w_y);
  }
}

/*! \brief Idle Update the Grid and Snap Display on the gschem Status-Bar
 *
 *  \par Function Description
 *  This function calls the appropriate interface to update the Grid/Snap
 *  label on the status bar.
 *
 *  \param [in] w_current GschemToplevel structure
 */
static bool
i_status_idle_update_grid_info (GschemToplevel *w_current)
{
  if (w_current->status_bar != NULL) {
    x_status_bar_update_grid_label (w_current);
  }
  return FALSE;
}

/*! \brief Schedule Update Grid and Snap Display
 *
 *  \par Function Description
 *  Spawn thread to update the Grid and Snap Display
 *
 *  \param [in] w_current GschemToplevel structure
 */
void i_status_update_grid_info(GschemToplevel *w_current)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    g_idle_add ((GSourceFunc)i_status_idle_update_grid_info, w_current);
  }
#if DEBUG_STATUS
  else {
    BUG_MSG("Bad pointer to top-level");
  }
#endif
}

/** \defgroup status-set-sensitivity Sensitivity Status
 *  @{
 *  \defgroup status-sensitivity-helpers Sensitivity Status Helpers
 *   @{
 *   \par
 *   This sections for helper function for determining how the
 *   Sensitivity should be set.
 */

/*! \brief Update sensitivity of the Edit/Paste menu item
 *  \par Function Description
 *  Asynchronous callback to update sensitivity of the Edit/Paste
 *  menu item.
 */
static void clipboard_usable_cb (int usable, void *userdata)
{
  GschemToplevel *w_current = userdata;
  x_menus_sensitivity (w_current, "_Edit/_Paste clipboard", usable);
  x_toolbars_set_sensitivities(w_current, CAN_PASTE, usable);
  x_menus_popup_sensitivity (w_current, "Paste Clipboard", usable);
}

/*! \brief Can anything selected be hatched for filled?
 *  \par Function Description
 * update sensitivities helper function to determine
 * if any selected objects can be hatched or filled
 */
static bool hatchable_object_selected(GList *list)
{
  Object *obj;

  while(list != NULL) {
    obj = (Object *) list->data;
    if (obj->type == OBJ_BOX || obj->type == OBJ_CIRCLE ||
        obj->type == OBJ_ARC || obj->type == OBJ_PATH)
    {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*! \brief Does anything selected have line-type properties?
 *  \par Function Description
 * update sensitivities helper function to determine
 * if any selected objects have line-type properties
 */
static bool linetype_object_selected(GList *list)
{
  Object *obj;

  while(list != NULL) {
    obj = (Object *) list->data;
    if (obj->type == OBJ_LINE   || obj->type == OBJ_BOX ||
        obj->type == OBJ_CIRCLE || obj->type == OBJ_ARC ||
        obj->type == OBJ_PATH)
    {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*! \brief Is at least one Text object selected?
 *  \par Function Description
 * update sensitivities helper function to determine
 * if any selected objects are Text objects
 */
static bool selected_at_least_one_text_object(GList *list)
{
  Object *obj;

  while(list != NULL) {
    obj = (Object *) list->data;
    if (obj->type == OBJ_TEXT) {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*! \brief Is at least one Complex object selected?
 *  \par Function Description
 * update sensitivities helper function to determine
 * if any selected objects are Complex objects
 */
static bool selected_complex_object(GList *list)
{
  Object *obj;

  while(list != NULL) {
    obj = (Object *) list->data;
    if (obj->type == OBJ_COMPLEX) {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*! \brief Is at least one Picture object selected?
 *  \par Function Description
 * update sensitivities helper function to determine
 * if any selected objects are Picture objects
 */
static bool selected_at_least_one_pic_object(GList *list)
{
  Object *obj;

  while(list != NULL) {
    obj = (Object *) list->data;
    if (obj->type == OBJ_PICTURE) {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*! \brief Is at least one Pin object selected?
 *  \par Function Description
 * update sensitivities helper function to determine
 * if any selected objects are Pin objects
 */
static bool selected_at_least_one_pin_object(GList *list)
{
  Object *obj;

  while(list != NULL) {
    obj = (Object *) list->data;
    if (obj->type == OBJ_PIN) {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/** @} endgroup status-sensitivity-helpers */

/*! \brief Idle Thread Update Sensitivity of relevant menu items
 *
 *  \par Function Description
 *  Update sensitivity of relevant menu & toolbar items.
 *
 *  \param [in] w_current GschemToplevel structure
 *
 *  \warning The menu strings in the function reference menu
 *  path NOT the displayed menu text, therefore these strings
 *  should NOT be internationalized
 *
 * TODO: Fix this ludicrousness
 */
static bool i_status_idle_update_sensitivities(GschemToplevel *w_current)
{
  bool any_object;
  bool can_hatch;
  bool can_edit_line;
  bool complex_selected;
  bool mutil_pages;
  bool text_selected;
  bool pic_selected;
  bool pin_selected;
  bool is_editing_symbol;

  void set_sensitivity_for_complexes (bool state) {

    x_menus_sensitivity(w_current, "_Page/_Down Schematic",   state);
    x_menus_sensitivity(w_current, "_Page/Down _Symbol",      state);
    x_menus_sensitivity(w_current, "_Page/D_ocumentation...", state);

    x_menus_sensitivity(w_current, "_Edit/Slot...",           state);

    x_menus_popup_sensitivity(w_current, "Down Schematic",    state);
    x_menus_popup_sensitivity(w_current, "Down Symbol",       state);
    /* x_menus_popup_sensitivity(w_current, "Up", state); */

    x_menus_sensitivity(w_current, "_Tools/Update Component", state);
  }

  void set_embeded_sensitivities (bool state) {
    x_menus_sensitivity(w_current, "_Tools/Embed Component/Picture", state);
    x_menus_sensitivity(w_current, "_Tools/Unembed Component/Picture", state);
  }

  void set_sensitivity_for_text (bool state) {
    x_menus_sensitivity(w_current, "A_ttributes/Show _Value", state);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Name",  state);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Both",   state);
    x_menus_sensitivity(w_current, "A_ttributes/_Toggle Visibility", state);
  }

  void set_sensitivity_for_buffers (bool state) {

    static bool  last_state = TRUE;
    const  char *buffer_menu;

    char *buffer;
    char  menu_string[48];
    int   index, length;

    for (index = 0; index < 48; index++)
       menu_string[index] = '0';

    buffer_menu = x_menu_get_buffer_menu (w_current);
    buffer      = strcpy(&menu_string[0], buffer_menu);

    length = strlen(buffer);
    menu_string[length++] = '/';
    menu_string[length]   = '\0';

    buffer = strcat(buffer, "Paste from 1");
    x_menus_sensitivity(w_current, buffer, (object_buffer[1] != NULL));
    index = length + 11;         /* set index to where the number is */
    menu_string[index] = '2';
    x_menus_sensitivity(w_current, buffer, (object_buffer[2] != NULL));
    menu_string[index] = '3';
    x_menus_sensitivity(w_current, buffer, (object_buffer[3] != NULL));
    menu_string[index] = '4';
    x_menus_sensitivity(w_current, buffer, (object_buffer[4] != NULL));
    menu_string[index] = '5';
    x_menus_sensitivity(w_current, buffer, (object_buffer[5] != NULL));

    if (state != last_state) {

      while(menu_string[length] != '/')length--;

      menu_string[++length]   = '\0';
      buffer = strcat(buffer, "Copy into 1");
      x_menus_sensitivity(w_current, buffer, state);
      index  = length + 10;       /* set index to where the number is */
      menu_string[index] = '2';
      x_menus_sensitivity(w_current, buffer, state);
      menu_string[index] = '3';
      x_menus_sensitivity(w_current, buffer, state);
      menu_string[index] = '4';
      x_menus_sensitivity(w_current, buffer, state);
      menu_string[index] = '5';
      x_menus_sensitivity(w_current, buffer, state);

      /* Incrementing to the leave "C" */
      menu_string[++length]   = '\0';
      buffer = strcat(buffer, "ut into 1");
      x_menus_sensitivity(w_current, buffer, state);
      index  = length + 8;
      menu_string[index] = '2';
      x_menus_sensitivity(w_current, buffer, state);
      menu_string[index] = '3';
      x_menus_sensitivity(w_current, buffer, state);
      menu_string[index] = '4';
      x_menus_sensitivity(w_current, buffer, state);
      menu_string[index] = '5';
      x_menus_sensitivity(w_current, buffer, state);

      last_state = state;
    }
  }

  GedaToplevel *toplevel = w_current->toplevel;
  GList *list = geda_list_get_glist(toplevel->page_current->selection_list);


  /* This is improved but still fairly simplistic.  What gets enabled/disabled
   * could be more selective based based on what is in the selection list, WEH
   */
  x_clipboard_query_usable (w_current, clipboard_usable_cb, w_current);


  if (w_current->toplevel->page_current == NULL) {
    any_object           = FALSE;
    can_hatch            = FALSE;
    can_edit_line        = FALSE;
    mutil_pages          = FALSE;
    complex_selected     = FALSE;
    is_editing_symbol    = FALSE;
    pic_selected         = FALSE;
    pin_selected         = FALSE;
    text_selected        = FALSE;
  }
  else {
    any_object           = o_select_is_selection (w_current);
    can_hatch            = hatchable_object_selected(list);
    can_edit_line        = linetype_object_selected(list);
    mutil_pages          = g_list_length(geda_list_get_glist(toplevel->pages)) > 1 ? TRUE : FALSE;
    complex_selected     = selected_complex_object(list);
    is_editing_symbol    = s_page_is_symbol_file(Current_Page);
    pic_selected         = selected_at_least_one_pic_object(list);
    pin_selected         = selected_at_least_one_pin_object(list);
    text_selected        = selected_at_least_one_text_object(list);
  }

  if ( mutil_pages ) {
    x_menus_sensitivity(w_current, "_Page/_Next", TRUE);
    x_menus_sensitivity(w_current, "_Page/_Previous", TRUE);
  }
  else {
    x_menus_sensitivity(w_current, "_Page/_Next", FALSE);
    x_menus_sensitivity(w_current, "_Page/_Previous", FALSE);
  }

  if ( any_object  ) {

    /* since one or more objects are selected, we set these TRUE */
    if ( complex_selected ) {
      set_sensitivity_for_complexes (TRUE);
    }
    else {
      set_sensitivity_for_complexes (FALSE);
    }

    if (complex_selected || pic_selected) {
      set_embeded_sensitivities (TRUE);
    }
    else {
      set_embeded_sensitivities (FALSE);
    }

    if (complex_selected  || is_editing_symbol) {
      x_menus_popup_sensitivity(w_current, "Component...",      TRUE);
      x_menus_sensitivity(w_current, "_Edit/Edit Component...", TRUE);
    }
    else {
      x_menus_popup_sensitivity(w_current, "Component...",      FALSE);
      x_menus_sensitivity(w_current, "_Edit/Edit Component...", FALSE);
    }

    if (pin_selected) {
        x_menus_sensitivity(w_current, "_Edit/Edit Pin...", TRUE);
        x_menus_popup_sensitivity(w_current, "Pin type...", TRUE);
    }
    else {
        x_menus_sensitivity(w_current, "_Edit/Edit Pin...",  FALSE);
        x_menus_popup_sensitivity(w_current,  "Pin type...", FALSE);
    }

    if (text_selected) {
      set_sensitivity_for_text (TRUE);
    }
    else {
      set_sensitivity_for_text (FALSE);
    }

    x_menus_sensitivity(w_current, "_Edit/Cu_t clipboard", TRUE);
    x_menus_sensitivity(w_current, "_Edit/_Copy clipboard", TRUE);
    x_menus_sensitivity(w_current, "_Edit/_Delete", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Copy", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Multiple Copy", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Move", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Rotate 90", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Mirror", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Edit...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Edit Text...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Color...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Lock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", TRUE);

    if (can_edit_line) {
      x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", TRUE);
    }
    else {
      x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", FALSE);
    }

    if (can_hatch) {
      x_menus_sensitivity(w_current, "_Edit/Fill Type...", TRUE);
    }
    else {
      x_menus_sensitivity(w_current, "_Edit/Fill Type...", FALSE);
    }

    set_sensitivity_for_buffers(TRUE);

    x_menus_popup_sensitivity(w_current, "Edit",          TRUE);
    x_menus_popup_sensitivity(w_current, "Object...",     TRUE);

    x_menus_popup_sensitivity(w_current, "Delete", TRUE);
    x_menus_popup_sensitivity(w_current, "Copy",   TRUE);
    x_menus_popup_sensitivity(w_current, "MCopy",  TRUE);
    x_menus_popup_sensitivity(w_current, "Move",   TRUE);
    x_menus_popup_sensitivity(w_current, "Mirror", TRUE);
    x_menus_popup_sensitivity(w_current, "Rotate", TRUE);

    x_menus_popup_sensitivity(w_current, "Cut to Clipboard",  TRUE);
    x_menus_popup_sensitivity(w_current, "Copy to Clipboard", TRUE);

  }
  else { /* Nothing is selected, grey these out */

    set_sensitivity_for_complexes (FALSE);
    set_embeded_sensitivities (FALSE);
    set_sensitivity_for_text (FALSE);
    set_sensitivity_for_buffers(FALSE);

    /* Handle special cases first, then follow menu order */

    if (! is_editing_symbol) {
      /* This is not handled in complex because of conditional */
      x_menus_popup_sensitivity(w_current, "Component...",  FALSE);
      x_menus_sensitivity(w_current, "_Edit/Edit Component...", FALSE);
    }

    x_menus_sensitivity(w_current, "_Edit/Cu_t clipboard", FALSE);
    x_menus_sensitivity(w_current, "_Edit/_Copy clipboard", FALSE);
    x_menus_sensitivity(w_current, "_Edit/_Delete", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Copy", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Multiple Copy", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Move", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Rotate 90", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Mirror", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit Text...", FALSE);

    x_menus_sensitivity(w_current, "_Edit/Color...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit Pin...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Lock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Fill Type...", FALSE);

    x_menus_popup_sensitivity(w_current, "Edit",          FALSE);
    //x_menus_popup_sensitivity(w_current, "Object...",     FALSE);
    //x_menus_popup_sensitivity(w_current, "Component...",  FALSE);
    //x_menus_popup_sensitivity(w_current, "Pin type...", FALSE);

    x_menus_popup_sensitivity(w_current, "Delete", FALSE);
    x_menus_popup_sensitivity(w_current, "Copy",   FALSE);
    x_menus_popup_sensitivity(w_current, "MCopy",  FALSE);
    x_menus_popup_sensitivity(w_current, "Move",   FALSE);
    x_menus_popup_sensitivity(w_current, "Mirror", FALSE);
    x_menus_popup_sensitivity(w_current, "Rotate", FALSE);

    x_menus_popup_sensitivity(w_current, "Cut to Clipboard",  FALSE);
    x_menus_popup_sensitivity(w_current, "Copy to Clipboard", FALSE);
  }

  if (complex_selected && text_selected) {
    x_menus_sensitivity(w_current, "A_ttributes/_Attach", TRUE);
  }
  else {
    x_menus_sensitivity(w_current, "A_ttributes/_Attach", FALSE);
  }

  if (complex_selected || text_selected) {
    x_menus_sensitivity(w_current, "A_ttributes/_Detach", TRUE);
  }
  else {
    x_menus_sensitivity(w_current, "A_ttributes/_Detach", FALSE);
  }

  /* Update sensitivities on the Toolbars */
  x_toolbars_set_sensitivities (w_current, CAN_HATCH,      can_hatch);
  x_toolbars_set_sensitivities (w_current, CAN_ELINE,      can_edit_line);
  x_toolbars_set_sensitivities (w_current, COMPLEX_OBJECT, complex_selected);
  x_toolbars_set_sensitivities (w_current, HAVE_PAGES,     mutil_pages);
  x_toolbars_set_sensitivities (w_current, HAVE_PIN,       pin_selected);
  x_toolbars_set_sensitivities (w_current, HAVE_TEXT,      text_selected);

  /* This list must be last*/
  x_toolbars_set_sensitivities (w_current, ANY_OBJECT,     any_object);

  return FALSE;
}

/*! \brief Schedule Update Sensitivity of relevant menu items
 *
 *  \par Function Description
 *  Spawns idle thread to update the sensitivities of widgets.
 *
 *  \param [in] w_current GschemToplevel structure
 */
void i_status_update_sensitivities(GschemToplevel *w_current)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    g_idle_add ((GSourceFunc)i_status_idle_update_sensitivities, w_current);
  }
#if DEBUG_SENSITIVITY
  else {
    BUG_MSG("Bad pointer, w_current == NULL");
  }
#endif
}
/** @} endgroup status-set-sensitivity */

static bool
i_status_idle_thread_update_title (GschemToplevel *w_current)
{
  x_window_update_title(w_current);
  return FALSE;
}

/*! \brief Schedule Set filename as gschem window title
 *
 *  \par Function Description
 *  Spawn thread to update the  window title
 *
 *  \param [in] w_current GschemToplevel structure
 */
void i_status_update_title(GschemToplevel *w_current)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    g_idle_add ((GSourceFunc)i_status_idle_thread_update_title, w_current);
  }
#if DEBUG_SENSITIVITY
  else {
    BUG_MSG("Bad pointer to top-level");
  }
#endif
}

/** @} endgroup Gschem-Status-System */