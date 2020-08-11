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

#include <gschem.h>

#include <geda_label.h>
#include <geda_debug.h>

/** \defgroup Gschem-Status-System Status System
 *  @{
 * \brief Routines to coordinate visual feedback to users
 * \par
 * This module contains routines to update user interface components,
 * i.e. the menus, status-bar, and tool-bars. The module does not set
 * values in widgets, or interact with GUI components, functions in
 * this module call the appropriate GUI providers to perform updates
 * in order to provides visual feedback to users.
 */

/*!
 * \brief Set new state, then show state field including some message
 * \par Function Description
 *  Set new state, then show state field including some
 *  message.
 *
 * \param [in] w_current GschemToplevel structure
 * \param [in] newstate  The new state
 * \param [in] message   Message to be shown
 *
 * *EK* Egil Kvaleberg
 */
void i_status_set_state_msg(GschemToplevel *w_current,
                            enum            x_states newstate,
                            const char     *message)
{
  /* The add picture mode is unique because this mode is the only
   * "adder" requiring resources to be freed when transitioning to
   *  any other state */
  if (w_current->event_state == PICTUREMODE) {
    if (w_current->current_pixbuf != NULL) {
      GEDA_UNREF(w_current->current_pixbuf);
      w_current->current_pixbuf = NULL;
    }

    GEDA_FREE(w_current->pixbuf_filename);
  }

  if (newstate != w_current->event_state) {
    w_current->event_state = newstate;
    x_toolbars_update (w_current);
  }

  i_status_show_state(w_current, message);
}

/*!
 * \brief Set new state, then show state field
 * \par Function Description
 *  Wrapper for i_status_set_state_msg, set new state without specifing
 *  and message string.
 *
 * \param [in] w_current GschemToplevel structure
 * \param [in] newstate  The new state
 *
 * *EK* Egil Kvaleberg
 */
void i_status_set_state(GschemToplevel *w_current, enum x_states newstate)
{
  i_status_set_state_msg (w_current, newstate, NULL);
}

/*!
 * \brief Update status bar string
 * \par Function Description
 *  This function updates the status bar widget with the new string.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] string The new string to be shown in the status bar
 */
static void i_status_update_status(GschemToplevel *w_current,
                                   const char     *string)
{
  if (!StatusBar->status_label) {
    return;
  }

  if (string) {
    if (strcmp(geda_label_widget_get_text(StatusBar->status_label), string) != 0) {
      geda_label_widget_set_text(StatusBar->status_label, string);
    }
  }
}

/*!
 * \brief Get string corresponding to the currently selected mode
 * \par Function Description
 *  Returns a string describing the currently
 *  selected mode.
 *
 * \param [in] w_current GschemToplevel structure
 *
 * \returns a string that will only last until the next time the
 *          function is called (which is probably just fine, really)
 *
 * *EK* Egil Kvaleberg
 */
static const char *i_status_string(GschemToplevel *w_current)
{
  static char *buf = 0;

  switch ( w_current->event_state ) {
    case NONE:                     /* 0 */
    case DESELECT:                 /* 1 */
    case STARTDESELECT:            /* 2 */
      return _("Deselect Mode");
    case SELECT:                   /* 3 */
    case STARTSELECT:              /* 4 */
    case GRIPS:                    /* 5 */
      return _("Select Mode");
    case SBOX:                     /* 6 */
      return _("Select Box Mode");
    case ZOOMBOX:                  /* 11 */
      return _("Zoom Box");
    case COPYMODE:                 /* 9 */
    case MCOPYMODE:                /* 22 */
      return _("Copy to point");
    case ENDROTATE:                /* 27 */
      return _("Rotate Mode");
    case(ENDOFFSET):               /* 28 */
      return _("Side to offset?");
    case ENDMIRROR:                /* 29 */
      return _("Mirror Mode");
    case PAN:                      /* 7 */
      return _("Pan Mode");
    case COMPMODE:                 /* 10 */
      return _("Choose component");
    case NETMODE:                  /* 12 */
      if (w_current->magnetic_net_mode)
        return _("Magnetic Net Mode");
      else
        return _("Net Mode");
    case PINMODE:                  /* 13 */
      return _("Pin Mode");
    case LINEMODE:                 /* 14 */
      return _("Line Mode");
    case BOXMODE:                  /* 15 */
      return _("Box Mode");
    case CIRCLEMODE:               /* 16 */
      return _("Circle Mode");
    case TEXTMODE:                 /* 17 */
      return _("Text Mode");
    case ARCMODE:                  /* 18 */
      return _("Arc Mode");
    case PATHMODE:                 /* 19 */
      return _("Path Mode");
    case PICTUREMODE:              /* 20 */
      return _("Picture Mode");
    case BUSMODE:                  /* 21 */
      return _("Bus Mode");
    case MOVEMODE:                 /* 23 */
      return _("Move Mode");
    case PASTEMODE:                /* 31 */
      GEDA_FREE(buf);
      buf = geda_sprintf(_("Paste %d Mode"), w_current->buffer_number+1);
      return buf;
    case STARTBREAK:               /* 35 */
      return _("First point?");
    case ENDBREAK:                 /* 36 */
      return _("Second point?");
    case STARTEXTEND:              /* 32 */
      return _("Project Mode");
    case EXTEND:                   /* 33 */
      return _("Select Projectiles");
    case ENDEXTEND:                /* 34 */
      return _("Select Object");
  }

#if DEBUG_STATUS

  fprintf(stderr, "%s: Invalid state <%d>\n", __func__, w_current->event_state);

#endif

  return ""; /* should not happen */
}

/*!
 * \brief Display a Message in the Status Bar
 * \par Function Description
 *  This function allows a message to be displayed in the status bar widget
 *  without alterations to the string.
 *
 * \param [in] w_current GschemToplevel structure
 * \param [in] string The message string to be shown in the status bar
 */
void i_status_show_msg(GschemToplevel *w_current, const char *string)
{
  if (StatusBar->status_label) {
    i_status_update_status(w_current, string);
  }
}

/*!
 * \brief Show state field
 * \par Function Description
 *  Show state field in the status bar, possibly with the addition
 *  of an extra message.
 *
 * \param [in] w_current GschemToplevel structure
 * \param [in] message The string to be displayed
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

  if (message && message[0]) {
    array[i] = message;
  }

  /* Skip over NULLs */
  while (array[i] == NULL) i++;

  what_to_say = g_strjoinv(" - ", (char **) array + i);

  if (w_current->keyaccel_string) {

     char *ptr = what_to_say;

     what_to_say =
       geda_sprintf("%s\t%s", w_current->keyaccel_string, what_to_say);

     GEDA_FREE(ptr);
  }

  i_status_update_status(w_current, what_to_say);

  GEDA_FREE(what_to_say);
}

static bool
i_status_idle_thread_update_action (GschemToplevel *w_current)
{
  int index;
  if (w_current->inside_action) {
    index = w_current->action_color;
  }
  else {
    index = 0;
  }
  gschem_status_bar_set_status_text_color (w_current->status_bar, index);
  return FALSE;
}

/*!
 * \brief Update Action State
 * \par Function Description
 *  Sets inside_action to state if not already set and initiates thread
 *  to update the status bar color.
 *
 * \param [in] w_current GschemToplevel structure
 * \param [in] state     Boolean value to set inside_action
 */
void i_status_update_action_state(GschemToplevel *w_current, int state)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    if (w_current->inside_action != state) {
      w_current->inside_action = state;
      gschem_threads_idle_add (i_status_idle_thread_update_action, w_current);
    }
  }
}

/*!
 * \brief Update Coordinate Display
 * \par Function Description
 *  Spawn thread to update the Grid and Snap Display
 *
 * \param [in] w_current GschemToplevel structure
 * \param [in] w_x       Current x coordinate of pointer in world units.
 * \param [in] w_y       Current y coordinate of pointer in world units.
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

/*!
 * \brief Idle Update the Grid and Snap Display on the gschem Status-Bar
 * \par Function Description
 *  This function calls the appropriate interface to update the Grid/Snap
 *  label on the status bar.
 *
 * \param [in] w_current GschemToplevel structure
 */
static bool
i_status_idle_update_grid_info (GschemToplevel *w_current)
{
  if (w_current->status_bar != NULL) {
    x_status_bar_update_grid_label (w_current);
  }
  return FALSE;
}

/*!
 * \brief Schedule Update Grid and Snap Display
 * \par Function Description
 *  Spawn thread to update the Grid and Snap Display
 *
 * \param [in] w_current GschemToplevel structure
 */
void i_status_update_grid_info(GschemToplevel *w_current)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    gschem_threads_idle_add (i_status_idle_update_grid_info, w_current);
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

/*!
 * \brief Update sensitivity of the Edit/Paste menu item
 * \par Function Description
 *  Asynchronous callback to update sensitivity of the Edit/Paste
 *  menu item.
 */
static void clipboard_usable_cb (int usable, void *userdata)
{
  GschemToplevel *w_current = userdata;
  x_menu_sensitivity (w_current, "_Edit/_Paste clipboard", usable);
  x_toolbars_set_sensitivities(w_current, CAN_PASTE, usable);
  x_menu_popup_sensitivity (w_current, "Paste Clipboard", usable);
}

/*!
 * \brief Can anything selected be hatched for filled?
 * \par Function Description
 *  Update sensitivities helper function to determine
 *  if any selected objects can be hatched or filled.
 *
 * \retval TRUE if any object in \a list can be hatched, otherwise FALSE.
 */
static bool hatchable_object_selected(GList *list)
{
  while (list != NULL) {

    GedaObject *obj = (GedaObject*)list->data;

    if (obj->type == OBJ_BOX || obj->type == OBJ_CIRCLE ||
        obj->type == OBJ_ARC || obj->type == OBJ_PATH)
    {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*!
 * \brief Does anything selected have line-type properties?
 * \par Function Description
 *  Update sensitivities helper function to determine
 *  if any selected objects have line-type properties.
 *
 * \retval TRUE if any object in \a list has line-type properties,
 *         otherwise FALSE.
 */
static bool linetype_object_selected(GList *list)
{
  while (list != NULL) {

    GedaObject *obj = (GedaObject*)list->data;

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

/*!
 * \brief Is at least one Text object selected?
 * \par Function Description
 *  Update sensitivities helper function to determine
 *  if any selected objects are Text objects.
 *
 * \retval TRUE if \a list contains a text object, otherwise FALSE.
 */
static bool selected_at_least_one_text_object(GList *list)
{
  while (list != NULL) {

    GedaObject *obj = (GedaObject*)list->data;

    if (obj->type == OBJ_TEXT) {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*!
 * \brief Is at least one Complex object selected?
 * \par Function Description
 *  Update sensitivities helper function to determine if any
 *  selected objects are Complex objects.
 *
 * \retval TRUE if \a list contains a Complex object, otherwise FALSE.
 */
static bool selected_complex_object(GList *list)
{
  while (list != NULL) {

    GedaObject *obj = (GedaObject*)list->data;

    if (obj->type == OBJ_COMPLEX) {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*!
 * \brief Is at least one Picture object selected?
 * \par Function Description
 *  Update sensitivities helper function to determine
 *  if any selected objects are Picture objects.
 *
 * \retval TRUE if \a list contains a Picture object, otherwise FALSE.
 */
static bool selected_at_least_one_pic_object(GList *list)
{
  while (list != NULL) {

    GedaObject *obj = (GedaObject*)list->data;

    if (obj->type == OBJ_PICTURE) {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/*!
 * \brief Is at least one Pin object selected?
 * \par Function Description
 *  Update sensitivities helper function to determine
 *  if any selected objects are Pin objects.
 *
 * \retval TRUE if \a list contains a Pin object, otherwise FALSE.
 */
static bool selected_at_least_one_pin_object(GList *list)
{
  while (list != NULL) {

    GedaObject *obj = (GedaObject*)list->data;

    if (obj->type == OBJ_PIN) {
      return TRUE;
    }
    NEXT(list);
  }
  return FALSE;
}

/** @} endgroup status-sensitivity-helpers */

/*!
 * \brief Idle Thread Update Sensitivity of relevant menu items
 * \par Function Description
 *  Update sensitivity of relevant menu & toolbar items.
 *
 * \param [in] w_current GschemToplevel structure
 *
 * \warning The menu strings in the function reference menu path
 *          NOT the displayed menu text, therefore these strings
 *          should NOT be internationalized.
 *
 * TODO: Fix this ludicrousness, maybe bit flags embedded in each
 *       object.
 */
static bool i_status_idle_update_sensitivities(GschemToplevel *w_current)
{
  bool any_object;
  bool can_hatch;
  bool can_edit_line;
  bool complex_selected;
  bool multi_pages;
  bool text_selected;
  bool pic_selected;
  bool pin_selected;
  bool is_editing_symbol;

  void set_sensitivity_for_complexes (bool state) {

    x_menu_sensitivity(w_current, "_Page/Down _Schematic",   state);
    x_menu_sensitivity(w_current, "_Page/Down S_ymbol",      state);

    x_menu_sensitivity(w_current, "_Edit/_Slot...",          state);

    x_menu_popup_sensitivity(w_current, "Down Schematic",    state);
    x_menu_popup_sensitivity(w_current, "Down Symbol",       state);
    /* x_menu_popup_sensitivity(w_current, "Up", state); */

    x_menu_sensitivity(w_current, "_Tools/Update Component", state);
  }

  void set_embedded_sensitivities (bool state) {
    x_menu_sensitivity(w_current, "_Tools/Embed Component/Picture", state);
    x_menu_sensitivity(w_current, "_Tools/Unembed Component/Picture", state);
  }

  void set_sensitivity_for_text (bool state) {
    x_menu_sensitivity(w_current, "Attri_butes/Show _Value", state);
    x_menu_sensitivity(w_current, "Attri_butes/Show _Name",  state);
    x_menu_sensitivity(w_current, "Attri_butes/Show _Both",   state);
    x_menu_sensitivity(w_current, "Attri_butes/_Toggle Visibility", state);
  }

  void set_sensitivity_for_buffers (bool state) {

    const char *buffer_menu;
    char  menu_string[48];
    int   index;

    for (index = 0; index < 48; index++) {
       menu_string[index] = '0';
    }

    /* Retrieve the path string to menu containing buffers */
    buffer_menu = x_menu_get_buffer_menu (w_current);

    if (buffer_menu)  {

      static bool last_state = TRUE;

      char  *buffer;
      int    length;

      buffer = strcpy(&menu_string[0], buffer_menu);
      length = strlen(buffer);

      menu_string[length++] = '/';   /* Add path seperator */
      menu_string[length]   = '\0';  /* terminate string */

      buffer = strcat(buffer, "Paste from 1");
      x_menu_sensitivity(w_current, buffer, (object_buffer[1] != NULL));
      index = strlen(buffer) - 1; /* set index to where the number is */
      menu_string[index] = '2';
      x_menu_sensitivity(w_current, buffer, (object_buffer[2] != NULL));
      menu_string[index] = '3';
      x_menu_sensitivity(w_current, buffer, (object_buffer[3] != NULL));
      menu_string[index] = '4';
      x_menu_sensitivity(w_current, buffer, (object_buffer[4] != NULL));
      menu_string[index] = '5';
      x_menu_sensitivity(w_current, buffer, (object_buffer[5] != NULL));

      if (state != last_state) {

        menu_string[length] = '\0';
        buffer = strcat(buffer, "Copy into 1");
        x_menu_sensitivity(w_current, buffer, state);
        index = strlen(buffer) - 1; /* set index to where the number is */
        menu_string[index] = '2';
        x_menu_sensitivity(w_current, buffer, state);
        menu_string[index] = '3';
        x_menu_sensitivity(w_current, buffer, state);
        menu_string[index] = '4';
        x_menu_sensitivity(w_current, buffer, state);
        menu_string[index] = '5';
        x_menu_sensitivity(w_current, buffer, state);

        /* Incrementing to the leave "C" */
        menu_string[length] = '\0';
        buffer = strcat(buffer, "Cut into 1");
        x_menu_sensitivity(w_current, buffer, state);
        index = strlen(buffer) - 1;
        menu_string[index] = '2';
        x_menu_sensitivity(w_current, buffer, state);
        menu_string[index] = '3';
        x_menu_sensitivity(w_current, buffer, state);
        menu_string[index] = '4';
        x_menu_sensitivity(w_current, buffer, state);
        menu_string[index] = '5';
        x_menu_sensitivity(w_current, buffer, state);

        last_state = state;
      }
    }
  }

  inline void set_all_need_object_false (void) {
    can_hatch            = FALSE;
    can_edit_line        = FALSE;
    complex_selected     = FALSE;
    pic_selected         = FALSE;
    pin_selected         = FALSE;
    text_selected        = FALSE;
  }

  GedaToplevel *toplevel = w_current->toplevel;

  /* This is improved but still fairly simplistic.  What gets enabled/disabled
   * could be more selective based based on what is in the selection list, WEH
   */
  x_clipboard_query_usable (w_current, clipboard_usable_cb, w_current);

  if (w_current->toplevel->page_current == NULL) {
    any_object           = FALSE;
    is_editing_symbol    = FALSE;
    multi_pages          = FALSE;
    set_all_need_object_false();
  }
  else {

    any_object           = o_select_is_selection (w_current);
    is_editing_symbol    = geda_struct_page_is_symbol_file(Current_Page);
    multi_pages          = g_list_length(geda_toplevel_get_pages(toplevel)) > 1 ? TRUE : FALSE;

    if (any_object) {

      GList *list        = geda_toplevel_struct_get_selection(toplevel);

      can_hatch          = hatchable_object_selected(list);
      can_edit_line      = linetype_object_selected(list);
      complex_selected   = selected_complex_object(list);
      pic_selected       = selected_at_least_one_pic_object(list);
      pin_selected       = selected_at_least_one_pin_object(list);
      text_selected      = selected_at_least_one_text_object(list);
    }
    else {
      set_all_need_object_false();
    }
  }

  if ( multi_pages ) {
    x_menu_sensitivity(w_current, "_Page/_Up", TRUE);
    x_menu_sensitivity(w_current, "_Page/_Down", TRUE);
    x_menu_sensitivity(w_current, "_Page/_Next", TRUE);
    x_menu_sensitivity(w_current, "_Page/_Previous", TRUE);

  }
  else {
    x_menu_sensitivity(w_current, "_Page/_Up", FALSE);
    x_menu_sensitivity(w_current, "_Page/_Down", FALSE);
    x_menu_sensitivity(w_current, "_Page/_Next", FALSE);
    x_menu_sensitivity(w_current, "_Page/_Previous", FALSE);
  }

  if (any_object) {

    /* since one or more objects are selected, we set these TRUE */
    if ( complex_selected ) {
      set_sensitivity_for_complexes (TRUE);
    }
    else {
      set_sensitivity_for_complexes (FALSE);
    }

    if (complex_selected || pic_selected) {
      set_embedded_sensitivities (TRUE);
    }
    else {
      set_embedded_sensitivities (FALSE);
    }

    if (complex_selected  || is_editing_symbol) {
      x_menu_popup_sensitivity(w_current, "Component...",      TRUE);
      x_menu_sensitivity(w_current, "_Edit/Edit Component...", TRUE);
    }
    else {
      x_menu_popup_sensitivity(w_current, "Component...",      FALSE);
      x_menu_sensitivity(w_current, "_Edit/Edit Component...", FALSE);
    }

    if (pin_selected) {
        x_menu_sensitivity(w_current, "_Edit/Edit Pi_n...", TRUE);
        x_menu_popup_sensitivity(w_current, "Pin type...", TRUE);
    }
    else {
        x_menu_sensitivity(w_current, "_Edit/Edit Pi_n...",  FALSE);
        x_menu_popup_sensitivity(w_current,  "Pin type...", FALSE);
    }

    if (text_selected) {
      set_sensitivity_for_text (TRUE);
    }
    else {
      set_sensitivity_for_text (FALSE);
    }

    x_menu_sensitivity(w_current, "_Edit/Cu_t clipboard", TRUE);
    x_menu_sensitivity(w_current, "_Edit/_Copy clipboard", TRUE);
    x_menu_sensitivity(w_current, "_Edit/_Delete", TRUE);
    x_menu_sensitivity(w_current, "_Edit/C_opy", TRUE);
    x_menu_sensitivity(w_current, "_Edit/_Multiple Copy", TRUE);
    x_menu_sensitivity(w_current, "_Edit/Mo_ve", TRUE);
    x_menu_sensitivity(w_current, "_Edit/Rotate _90", TRUE);
    x_menu_sensitivity(w_current, "_Edit/M_irror", TRUE);
    x_menu_sensitivity(w_current, "_Edit/_Edit...", TRUE);
    x_menu_sensitivity(w_current, "_Edit/E_dit Text...", TRUE);
    x_menu_sensitivity(w_current, "_Edit/Co_lor...", TRUE);

    x_menu_sensitivity(w_current, "_Select/Lock", TRUE);
    x_menu_sensitivity(w_current, "_Select/Unlock", TRUE);

    if (can_edit_line) {
      x_menu_sensitivity(w_current, "_Edit/Line _Width & Type...", TRUE);
    }
    else {
      x_menu_sensitivity(w_current, "_Edit/Line _Width & Type...", FALSE);
    }

    if (can_hatch) {
      x_menu_sensitivity(w_current, "_Edit/Fill T_ype...", TRUE);
    }
    else {
      x_menu_sensitivity(w_current, "_Edit/Fill T_ype...", FALSE);
    }

    set_sensitivity_for_buffers(TRUE);

    x_menu_popup_sensitivity(w_current, "Edit",          TRUE);
    x_menu_popup_sensitivity(w_current, "Object...",     TRUE);

    x_menu_popup_sensitivity(w_current, "Delete", TRUE);
    x_menu_popup_sensitivity(w_current, "Copy",   TRUE);
    x_menu_popup_sensitivity(w_current, "MCopy",  TRUE);
    x_menu_popup_sensitivity(w_current, "Move",   TRUE);
    x_menu_popup_sensitivity(w_current, "Mirror", TRUE);
    x_menu_popup_sensitivity(w_current, "Rotate", TRUE);

    x_menu_popup_sensitivity(w_current, "Cut to Clipboard",  TRUE);
    x_menu_popup_sensitivity(w_current, "Copy to Clipboard", TRUE);

  }
  else { /* Nothing is selected, grey these out */

    set_sensitivity_for_complexes (FALSE);
    set_embedded_sensitivities (FALSE);
    set_sensitivity_for_text (FALSE);
    set_sensitivity_for_buffers(FALSE);

    /* Handle special cases first, then follow menu order */

    if (! is_editing_symbol) {
      /* This is not handled in complex because of conditional */
      x_menu_popup_sensitivity(w_current, "Component...",  FALSE);
      x_menu_sensitivity(w_current, "_Edit/Edit Component...", FALSE);
    }

    x_menu_sensitivity(w_current, "_Edit/Cu_t clipboard", FALSE);
    x_menu_sensitivity(w_current, "_Edit/_Copy clipboard", FALSE);
    x_menu_sensitivity(w_current, "_Edit/_Delete", FALSE);
    x_menu_sensitivity(w_current, "_Edit/C_opy", FALSE);
    x_menu_sensitivity(w_current, "_Edit/_Multiple Copy", FALSE);
    x_menu_sensitivity(w_current, "_Edit/Mo_ve", FALSE);
    x_menu_sensitivity(w_current, "_Edit/Rotate _90", FALSE);
    x_menu_sensitivity(w_current, "_Edit/M_irror", FALSE);
    x_menu_sensitivity(w_current, "_Edit/_Edit...", FALSE);
    x_menu_sensitivity(w_current, "_Edit/E_dit Text...", FALSE);

    x_menu_sensitivity(w_current, "_Edit/Co_lor...", FALSE);
    x_menu_sensitivity(w_current, "_Edit/Edit Pi_n...", FALSE);
    x_menu_sensitivity(w_current, "_Edit/Line _Width & Type...", FALSE);
    x_menu_sensitivity(w_current, "_Edit/Fill T_ype...", FALSE);

    x_menu_sensitivity(w_current, "_Select/Lock", FALSE);
    x_menu_sensitivity(w_current, "_Select/Unlock", FALSE);

    x_menu_popup_sensitivity(w_current, "Edit",   FALSE);
    x_menu_popup_sensitivity(w_current, "Delete", FALSE);
    x_menu_popup_sensitivity(w_current, "Copy",   FALSE);
    x_menu_popup_sensitivity(w_current, "MCopy",  FALSE);
    x_menu_popup_sensitivity(w_current, "Move",   FALSE);
    x_menu_popup_sensitivity(w_current, "Mirror", FALSE);
    x_menu_popup_sensitivity(w_current, "Rotate", FALSE);

    x_menu_popup_sensitivity(w_current, "Cut to Clipboard",  FALSE);
    x_menu_popup_sensitivity(w_current, "Copy to Clipboard", FALSE);
  }

  if (any_object && text_selected) {
    x_menu_sensitivity(w_current, "Attri_butes/_Attach", TRUE);
  }
  else {
    x_menu_sensitivity(w_current, "Attri_butes/_Attach", FALSE);
  }

  if (complex_selected || text_selected) {
    x_menu_sensitivity(w_current, "Attri_butes/_Detach", TRUE);
  }
  else {
    x_menu_sensitivity(w_current, "Attri_butes/_Detach", FALSE);
  }

  /* Update sensitivities on the Toolbars */
  x_toolbars_set_sensitivities (w_current, CAN_HATCH,      can_hatch);
  x_toolbars_set_sensitivities (w_current, CAN_ELINE,      can_edit_line);
  x_toolbars_set_sensitivities (w_current, COMPLEX_OBJECT, complex_selected);
  x_toolbars_set_sensitivities (w_current, HAVE_PAGES,     multi_pages);
  x_toolbars_set_sensitivities (w_current, HAVE_PIN,       pin_selected);
  x_toolbars_set_sensitivities (w_current, HAVE_TEXT,      text_selected);

  /* This list must be last*/
  x_toolbars_set_sensitivities (w_current, ANY_OBJECT,     any_object);

  return FALSE;
}

/*!
 * \brief Schedule Update Sensitivity of relevant menu items
 * \par Function Description
 *  Spawns idle thread to update the sensitivities of widgets.
 *
 * \param [in] w_current GschemToplevel structure
 */
void i_status_update_sensitivities(GschemToplevel *w_current)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    gschem_threads_idle_add (i_status_idle_update_sensitivities, w_current);
  }
}

/** @} endgroup status-set-sensitivity */

/*! \internal Ran in idle thread to update the window title bar */
static bool i_status_idle_thread_update_title (GschemToplevel *w_current)
{
  x_window_update_title(w_current);
  return FALSE;
}

/*!
 * \brief Schedule Set filename as gschem window title
 * \par Function Description
 *  Spawn thread to update the window title
 *
 * \param [in] w_current GschemToplevel structure
 */
void i_status_update_title(GschemToplevel *w_current)
{
  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    gschem_threads_idle_add (i_status_idle_thread_update_title, w_current);
  }
}

/** @} endgroup Gschem-Status-System */
