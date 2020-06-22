/* -*- C x_status_bar.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */
/*!
 * \file x_status_bar.c
 * \brief Interface to the "status bar" widget
 */

#include "../../include/gschem.h"

#include <geda_widgets.h>
#include <geda_debug.h>

/** \defgroup status-module Status Module
 *  @{\brief This group implements the #GschemStatusBar class
 *    \ingroup main-window
*/

/* ------------------ Callbacks for Mouse Button Options  ------------------ */

/*!
 * \brief Action clicked on Middle Mouse Options Popup Callback
 * \par Function Description
 *  Called in response to "set-middle-action" being emitted from the
 *  status bar widget. Sets middle mouse button preference variable
 *  to MOUSE_MIDDLE_ACTION and calls for update.
 *
 * \param [in] status_bar Pointer to #GschemStatusBar widget, not used
 * \param [in] data       Pointer to GschemToplevel structure
 */
static
void x_status_bar_set_middle_action(GtkWidget *status_bar, void *data)
{
  GschemToplevel *w_current = data;

  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    w_current->middle_button = MOUSE_MIDDLE_ACTION;
    x_status_bar_update_middle_mouse(w_current, NULL);
  }
  else {
    BUG_MSG("Invalid pointer to top-level structure");
  }
}

/*!
 * \brief Pan clicked on Middle Mouse Options Popup Callback
 * \par Function Description
 *  Called in response to "set-middle-pan" being emitted from the
 *  status bar widget. Sets middle mouse button preference variable
 *  to MOUSE_MIDDLE_PAN and calls for update.
 *
 * \param [in] status_bar Pointer to #GschemStatusBar widget, not used
 * \param [in] data       Pointer to GschemToplevel structure
 */
static void
x_status_bar_set_middle_pan(GtkWidget *status_bar, void *data)
{
  GschemToplevel *w_current = data;

  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    w_current->middle_button = MOUSE_MIDDLE_PAN;
    x_status_bar_update_middle_mouse(w_current, NULL);
  }
  else {
    BUG_MSG("Invalid pointer to top-level structure");
  }
}

/*!
 * \brief Popup clicked on Middle Mouse Options Popup Callback
 * \par Function Description
 *  Called in response to "set-middle-popup" being emitted from the
 *  status bar widget. Sets middle mouse button preference variable
 *  to MOUSE_MIDDLE_POPUP and calls for update.
 *
 * \param [in] status_bar Pointer to #GschemStatusBar widget, not used
 * \param [in] data       Pointer to GschemToplevel structure
 */
static void
x_status_bar_set_middle_popup(GtkWidget *status_bar, void *data)
{
  GschemToplevel *w_current = data;

  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    w_current->middle_button = MOUSE_MIDDLE_POPUP;
    x_status_bar_update_middle_mouse(w_current, NULL);
  }
  else {
    BUG_MSG("Invalid pointer to top-level structure");
  }
}

/*!
 * \brief Repeat clicked on Middle Mouse Options Popup Callback
 * \par Function Description
 *  Called in response to "set-middle-repeat" being emitted from the
 *  status bar widget. Sets middle mouse button preference variable
 *  to MOUSE_MIDDLE_REPEAT and calls for update.
 *
 * \param [in] status_bar Pointer to #GschemStatusBar widget, not used
 * \param [in] data       Pointer to GschemToplevel structure
 */
static void x_status_bar_set_middle_repeat(GtkWidget *status_bar, void *data)
{
  GschemToplevel *w_current = data;

  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    w_current->middle_button = MOUSE_MIDDLE_REPEAT;
    x_status_bar_update_middle_mouse(w_current, NULL);
  }
  else {
    BUG_MSG("Invalid pointer to top-level structure");
  }
}

#ifdef HAVE_LIBSTROKE

/*!
 * \brief Stroke clicked on Middle Mouse Options Popup Callback
 * \par Function Description
 *  Called in response to "set-middle-stroke" being emitted from the
 *  status bar widget. Sets middle mouse button preference variable
 *  to MOUSE_MIDDLE_STROKE and calls for update.
 *
 * \param [in] status_bar Pointer to #GschemStatusBar widget, not used
 * \param [in] data       Pointer to GschemToplevel structure
 */
static void x_status_bar_set_middle_stroke(GtkWidget *status_bar,
                                           void      *data)
{
  GschemToplevel *w_current = data;

  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    w_current->middle_button = MOUSE_MIDDLE_STROKE;
    x_status_bar_update_middle_mouse(w_current, NULL);
  }
  else {
    BUG_MSG("Invalid pointer to top-level structure");
  }
}
#endif

/*!
 * \brief Popup (Menu) clicked on Third Mouse Options Popup Callback
 * \par Function Description
 *  Called in response to "set-third-popup" being emitted from the
 *  status bar widget. Sets third mouse button preference variable
 *  to POPUP_ENABLED and calls for update.
 *
 * \param [in] status_bar Pointer to #GschemStatusBar widget, not used
 * \param [in] data       Pointer to GschemToplevel structure
 */
static void x_status_bar_set_third_popup(GtkWidget *status_bar, void *data)
{
  GschemToplevel *w_current = data;

  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    w_current->third_button = POPUP_ENABLED;
    x_status_bar_update_third_mouse(w_current);
  }
  else {
    BUG_MSG("Invalid pointer to top-level structure");
  }
}

/*!
 * \brief Pan clicked on Third Mouse Options Popup Callback
 * \par Function Description
 *  Called in response to "set-third-pan" being emitted from the
 *  status bar widget. Sets third mouse button preference variable
 *  to MOUSEPAN_ENABLED and calls for update.
 *
 * \param [in] status_bar Pointer to #GschemStatusBar widget, not used
 * \param [in] data       Pointer to GschemToplevel structure
 */
static void x_status_bar_set_third_pan(GtkWidget *status_bar, void *data)
{
  GschemToplevel *w_current = data;

  if (GSCHEM_IS_TOPLEVEL(w_current)) {
    w_current->third_button = MOUSEPAN_ENABLED;
    x_status_bar_update_third_mouse(w_current);
  }
  else {
    BUG_MSG("Invalid pointer to top-level structure");
  }
}

/* ---------------- End Callbacks for Mouse Button Options  ---------------- */

/*!
 * \public
 * \brief Update the grid and snap settings for the gschem status bar
 * \par Function Description
 *  This functions compares the active settings to the current status
 *  settings and updates the status settings if the status settings
 *  are different from the active settings.
 *
 * \param [in] w_current GschemToplevel structure
 */
void x_status_bar_update_grid_label (GschemToplevel *w_current)
{
  if (GSCHEM_IS_STATUS_BAR(StatusBar)) {

    int do_update;
    int grid_mode = w_current->grid_mode;
    int grid_size = x_grid_query_drawn_spacing (w_current);
    int snap_mode = w_current->snap;
    int snap_size = w_current->snap_size;

    /* If sum of differences is non-zero then update */
    do_update = (StatusBar->grid_mode - grid_mode) +
                (StatusBar->grid_size - grid_size) +
                (StatusBar->snap_mode - snap_mode) +
                (StatusBar->snap_size - snap_size);

    if (do_update) {
      StatusBar->grid_mode = grid_mode;
      StatusBar->grid_size = grid_size;
      StatusBar->snap_mode = snap_mode;
      StatusBar->snap_size = snap_size;
      g_signal_emit_by_name(w_current->status_bar, "update-grid-label");
    }
  }
}

/*!
 * \brief Update Middle Mouse Button Settings for the Gschem Status Bar
 * \par Function Description
 *  This function sets the string used to describe the action of the middle
 *  middle mouse button and updates the label on the status bar if the middle
 *  mouse assignment has changed. The function maintained a static integer
 *  to retain the previous setting.
 *
 * \param [in] w_current GschemToplevel structure
 * \param [in] repeat    pointer to label (what will be repeated)
 */
void x_status_bar_update_middle_mouse(GschemToplevel *w_current,
                                      const char     *repeat)
{
  static int previous_setting = -1;

  if (!StatusBar->middle_label)
    return;

  if (w_current->middle_button != MOUSE_MIDDLE_REPEAT) {

    if (w_current->middle_button != previous_setting) {
      switch(w_current->middle_button) {
        /* remove this case eventually and make it a null case */
        case (MOUSE_MIDDLE_ACTION):
          geda_label_widget_set_text(StatusBar->middle_label, _(RC_STR_MID_ACTION));
          previous_setting = MOUSE_MIDDLE_ACTION;
          break;

#ifdef HAVE_LIBSTROKE
        case (MOUSE_MIDDLE_STROKE):
          geda_label_widget_set_text(StatusBar->middle_label, _(RC_STR_MID_STROKE));
          previous_setting = MOUSE_MIDDLE_STROKE;
          break;
#else
        /* remove this case eventually and make it a null case */
        /* Note: We encounter this case if the user set the middle mouse to "stroke"
         * either via an RC-script or the Preferences dialog but source was not
         * compiled on a machine with libstrokes */
        case (MOUSE_MIDDLE_STROKE):
          geda_label_widget_set_text(StatusBar->middle_label, _("none"));
          previous_setting = MOUSE_MIDDLE_STROKE;
          break;
#endif

        case (MOUSE_MIDDLE_PAN):
          geda_label_widget_set_text(StatusBar->middle_label, _(RC_STR_MID_MOUSEPAN));
          previous_setting = MOUSE_MIDDLE_PAN;
          break;

        case (MOUSE_MIDDLE_POPUP):
          geda_label_widget_set_text(StatusBar->middle_label, _(RC_STR_MID_MOUSEPOP));
          previous_setting = MOUSE_MIDDLE_POPUP;
          break;

        default:
          geda_label_widget_set_text(StatusBar->middle_label, _("none"));
          break;
      }
    }
  }
  else {

    char *string;

    string = strcpy (StatusBar->middle_label_text,_(RC_STR_MID_REPEAT));
    strcat (string, "/");
    if (repeat) {
      strcat (string, repeat);
    }
    else {
      strcat (string, _("none"));
    }
    geda_label_widget_set_text(StatusBar->middle_label, string);
    previous_setting = MOUSE_MIDDLE_REPEAT;
  }
}

/*!
 * \brief Update the Third Button settings for the gschem status bar
 * \par Function Description
 *  This functions reloads the string for the third-mouse button
 *  label on the status-bar based on the current setting.
 *
 * \param [in] w_current GschemToplevel structure
 */
void x_status_bar_update_third_mouse (GschemToplevel *w_current)
{
  if (StatusBar->right_label) {

    static int previous_setting = -1;

    if (w_current->third_button != previous_setting) {

      if (w_current->third_button == POPUP_ENABLED) {
        geda_label_widget_set_text(StatusBar->right_label, _("Menu/Cancel"));
      }
      else {
        geda_label_widget_set_text(StatusBar->right_label, _("Pan/Cancel"));
      }
      previous_setting = w_current->third_button;
    }
  }
}

/*!
 * \public
 * \brief Create GschemStatusBar
 * \par Function Description
 *  This functions creates a new instance of the a GschemStatusBar
 *  widget and connects in callback signal handlers.
 *
 * \param [in] w_current GschemToplevel structure
 *
 * \returns The newly created status bar widget
 */
GtkWidget *x_status_bar_create(GschemToplevel *w_current)
{
  GtkWidget *status_bar;

  const char *middle_mouse_text;
  const char *right_mouse_text;

  /* bottom box */
  switch(w_current->middle_button) {

    /* remove this case eventually and make it a null case */
    case (MOUSE_MIDDLE_ACTION):
      middle_mouse_text = _(RC_STR_MID_ACTION);
      break;

#ifdef HAVE_LIBSTROKE
    case (MOUSE_MIDDLE_STROKE):
      middle_mouse_text = _(RC_STR_MID_STROKE);
      break;
#else
      /* remove this case eventually and make it a null case */
    case (MOUSE_MIDDLE_STROKE):
      middle_mouse_text = _("none");
      break;
#endif
    case (MOUSE_MIDDLE_REPEAT):
      middle_mouse_text = _("Repeat/none");
      break;

    case (MOUSE_MIDDLE_PAN):
      middle_mouse_text = _(RC_STR_MID_MOUSEPAN);
      break;

    case (MOUSE_MIDDLE_POPUP):
      middle_mouse_text = _(RC_STR_MID_MOUSEPOP);
      break;

    default:
      middle_mouse_text = _("none");
  }

  if (w_current->third_button == POPUP_ENABLED) {
    right_mouse_text = "Menu/Cancel";
  }
  else {
    right_mouse_text = "Pan/Cancel";
  }

  status_bar = g_object_new (GSCHEM_TYPE_STATUS_BAR,
                             "grid-mode",   w_current->grid_mode,
                             "grid-size",   -1,
                             "left-text",  _("Pick"),
                             "middle-text", middle_mouse_text,
                             "right-text",  right_mouse_text,
                             "snap-mode",   w_current->snap,
                             "snap-size",   w_current->snap_size,
                             "status-text", _("Select Mode"),
                             NULL);

  g_signal_connect (status_bar, "set-middle-action",
                    G_CALLBACK (x_status_bar_set_middle_action),
                    w_current);
  g_signal_connect (status_bar, "set-middle-pan",
                    G_CALLBACK (x_status_bar_set_middle_pan),
                    w_current);
  g_signal_connect (status_bar, "set-middle-popup",
                    G_CALLBACK (x_status_bar_set_middle_popup),
                    w_current);
  g_signal_connect (status_bar, "set-middle-repeat",
                    G_CALLBACK (x_status_bar_set_middle_repeat),
                    w_current);

#ifdef HAVE_LIBSTROKE
  g_signal_connect (status_bar, "set-middle-stroke",
                    G_CALLBACK (x_status_bar_set_middle_stroke),
                    w_current);
#endif

  g_signal_connect (status_bar, "set-third-popup",
                    G_CALLBACK (x_status_bar_set_third_popup),
                    w_current);
  g_signal_connect (status_bar, "set-third-pan",
                    G_CALLBACK (x_status_bar_set_third_pan),
                    w_current);
  return status_bar;
}

/** @} endgroup status-module */
