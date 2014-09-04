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

#include "gschem.h"
#include "x_menus.h"

#include <geda_label.h>
#include <geda_debug.h>

/** \defgroup Gschem-Status-Module Gschem Status Module
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
  w_current->event_state = newstate;
  x_toolbars_update(w_current);
  i_status_show_state(w_current, message);
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
void i_status_set_state(GschemToplevel *w_current, enum x_states newstate)
{
  i_status_set_state_msg(w_current, newstate, NULL);
}


/*! \brief Update status bar string
 *
 *  \par Function Description
 *  This function updates the status bar widget with the new string.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] string The new string to be shown in the status bar
 */
static void
i_status_update_status(GschemToplevel *w_current, const char *string)
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

  i_status_update_status(w_current, what_to_say);
  GEDA_FREE(what_to_say);
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
 * i_pan_world.c:159  i_pan_world_general()
 * i_command.c:2665   i_cmd_do_grid_dots()
 * i_command.c:2674   i_cmd_do_grid_mesh()
 * i_command.c:2683   i_cmd_do_grid_off()
 * i_command.c:2705   i_cmd_do_cycle_grid()
 *
 */
void i_status_update_grid_info (GschemToplevel *w_current)
{
  x_status_bar_update_grid_label (w_current);
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
  x_menus_sensitivity (w_current, "_Edit/_Paste", usable);
  x_toolbars_set_sensitivities(w_current, CAN_PASTE, usable);
  x_menus_popup_sensitivity (w_current, "Paste", usable);
}

/*! \brief Can anything selected be hatched for filled?
 *  \par Function Description
 * update sensitivities helper function to determine
 * if any selected objects can be hatched or filled
 */
static bool
hatchable_object_selected(GList *list)
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
static bool
linetype_object_selected(GList *list)
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
static bool
selected_at_least_one_text_object(GList *list)
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
static bool
selected_complex_object(GList *list)
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

/*! \brief Is at least one Pin object selected?
 *  \par Function Description
 * update sensitivities helper function to determine
 * if any selected objects are Pin objects
 */
static bool
selected_at_least_one_pin_object(GList *list)
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
static bool
i_status_idle_update_sensitivities(GschemToplevel *w_current)
{
  bool any_object;
  bool can_hatch;
  bool can_edit_line;
  bool complex_selected;
  bool mutil_pages;
  bool text_selected;
  bool pin_selected;
  bool is_editing_symbol;

  void set_sensitivity_for_complexes (bool state) {

    x_menus_sensitivity(w_current, "_Page/_Down Schematic",   state);
    x_menus_sensitivity(w_current, "_Page/Down _Symbol",      state);
    x_menus_sensitivity(w_current, "_Page/D_ocumentation...", state);

    x_menus_sensitivity(w_current, "A_ttributes/_Attach",     state);
    x_menus_sensitivity(w_current, "A_ttributes/_Detach",     state);
    x_menus_sensitivity(w_current, "_Edit/Slot...",           state);

    x_menus_popup_sensitivity(w_current, "Down Schematic",    state);
    x_menus_popup_sensitivity(w_current, "Down Symbol",       state);
    /* x_menus_popup_sensitivity(w_current, "Up", state); */

    x_menus_sensitivity(w_current, "_Tools/Unembed Component/Picture", state);
    x_menus_sensitivity(w_current, "_Tools/Update Component", state);
  }

  void set_sensitivity_for_text (bool state) {
    x_menus_sensitivity(w_current, "A_ttributes/Show _Value", state);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Name",  state);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Both",   state);
    x_menus_sensitivity(w_current, "A_ttributes/_Toggle Visibility", state);
  }

  GedaToplevel *toplevel = w_current->toplevel;
  GList *list = geda_list_get_glist(toplevel->page_current->selection_list);


  /* This is improve but still fairly simplistic.  What gets enabled/disabled
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

    if (complex_selected  || is_editing_symbol) {
      x_menus_sensitivity(w_current, "_Edit/Edit Component...", TRUE);
    }
    else {
      x_menus_sensitivity(w_current, "_Edit/Edit Component...", FALSE);
    }

    if (pin_selected) {
        x_menus_sensitivity(w_current, "_Edit/Edit Pin...", TRUE);
        x_menus_popup_sensitivity(w_current, "Edit pin type...", TRUE);
    }
    else {
        x_menus_sensitivity(w_current, "_Edit/Edit Pin...", FALSE);
        x_menus_popup_sensitivity(w_current,  "Edit pin type...", FALSE);
    }

    if (text_selected) {
      set_sensitivity_for_text (TRUE);
    }
    else {
      set_sensitivity_for_text (FALSE);
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

    x_menus_popup_sensitivity(w_current, "Cut",     TRUE);
    x_menus_popup_sensitivity(w_current, "Copy",    TRUE);
    x_menus_popup_sensitivity(w_current, "Edit...", TRUE);
    //x_menus_popup_sensitivity(w_current, "Duplicate", TRUE);
    //x_menus_popup_sensitivity(w_current, "Move",      TRUE);
    x_menus_popup_sensitivity(w_current, "Delete", TRUE);

  }
  else { /* Nothing is selected, grey these out */

    set_sensitivity_for_complexes (FALSE);
    set_sensitivity_for_text (FALSE);

    /* Handle special cases first, then follow menu order */

    if (! is_editing_symbol) {
      /* This is not handled in complex because of conditional */
      x_menus_sensitivity(w_current, "_Edit/Edit Component...", FALSE);
    }

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

    x_menus_sensitivity(w_current, "_Edit/Color...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit Pin...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Lock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Fill Type...", FALSE);

    x_menus_sensitivity(w_current, "_Buffer/Copy into 1", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 2", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 3", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 4", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 5", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 1",  FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 2",  FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 3",  FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 4",  FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 5",  FALSE);

    x_menus_popup_sensitivity(w_current, "Edit...",          FALSE);
    x_menus_popup_sensitivity(w_current, "Edit pin type...", FALSE);
    //x_menus_popup_sensitivity(w_current, "Duplicate",        FALSE);
    //x_menus_popup_sensitivity(w_current, "Move",             FALSE);
    x_menus_popup_sensitivity(w_current,  "Delete",          FALSE);

    x_menus_popup_sensitivity(w_current, "Cut",  FALSE);
    x_menus_popup_sensitivity(w_current, "Copy", FALSE);
  }

  x_menus_sensitivity(w_current, "_Buffer/Paste from 1", (object_buffer[1] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 2", (object_buffer[2] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 3", (object_buffer[3] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 4", (object_buffer[4] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 5", (object_buffer[5] != NULL));

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
 *  Spawn threas to update the  window title
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

/** @} endgroup Gschem-Status-Module */