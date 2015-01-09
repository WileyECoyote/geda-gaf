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

#include <config.h>

#include "gschem.h"

#include <geda_widgets.h>
#include <geda_debug.h>

/** \defgroup status-module Status Module
 *  @{\brief This group implements the #GschemStatusBar class
 *    \ingroup (main-window)
*/

/*! \brief Update the grid and snap settings for the gschem status bar
 *
 *  \par Function Description
 *  This functions compares the active settings to the current status
 *  settings and updates the status settings if the status settings
 *  are different from the active settings.
 *
 *  \param [in] w_current GschemToplevel structure
 */
void
x_status_bar_update_grid_label (GschemToplevel *w_current)
{
  if ( GSCHEM_IS_STATUS_BAR(StatusBar) ) {

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

    if ( do_update ) {
      StatusBar->grid_mode = grid_mode;
      StatusBar->grid_size = grid_size;
      StatusBar->snap_mode = snap_mode;
      StatusBar->snap_size = snap_size;

      g_signal_emit_by_name(w_current->status_bar, "update-grid-label");
    }
  }
}

/*! \brief Update Middle Mouse Button Settings for the Gschem Status Bar
 *
 *  \par Function Description
 *  This function sets the string used to describe the action of the middle
 *  middle mouse button and updates the label on the status bar if the middle
 *  mouse assignment has changed. The function maintained a static integer
 *  to retain the previous setting.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] repeat    pointer to label (what will be repeated)
 */
void
x_status_bar_middle_mouse(GschemToplevel *w_current, const char *repeat)
{

  char *string;
  static int previous_setting = -1;

  if (!StatusBar->middle_label)
    return;

  if ( w_current->middle_button != MOUSE_MIDDLE_REPEAT ) {
    if ( w_current->middle_button != previous_setting ) {
      switch(w_current->middle_button) {
        /* remove this case eventually and make it a null case */
        case( MOUSE_MIDDLE_ACTION ):
          geda_label_widget_set_text(StatusBar->middle_label, _( RC_STR_MID_ACTION ));
          previous_setting = MOUSE_MIDDLE_ACTION;
          break;

#ifdef HAVE_LIBSTROKE
        case( MOUSE_MIDDLE_STROKE ):
          geda_label_widget_set_text(StatusBar->middle_label, _( RC_STR_MID_STROKE ));
          previous_setting = MOUSE_MIDDLE_STROKE;
          break;
#else
        /* remove this case eventually and make it a null case */
        /* Note: We encounter this case if the user set the middle mouse to "stroke"
         * either via an RC-script or the Preferences dialog but source was not
         * compiled on a machine with libstrokes */
        case( MOUSE_MIDDLE_STROKE ):
          geda_label_widget_set_text(StatusBar->middle_label, _("none"));
          previous_setting = MOUSE_MIDDLE_STROKE;
          break;
#endif

        case( MOUSE_MIDDLE_PAN ):
          geda_label_widget_set_text(StatusBar->middle_label, _( RC_STR_MID_MOUSEPAN ));
          previous_setting = MOUSE_MIDDLE_PAN;
          break;

        default:
          geda_label_widget_set_text(StatusBar->middle_label, _("none"));
          break;
      }
    }
  }
  else {
    string = strcpy (StatusBar->middle_label_text,_(RC_STR_MID_REPEAT));
    strcat (string, "/");
    if (repeat) {
      strcat (string, repeat);
    }
    else {
      strcat (string, _("none"));
    }
    geda_label_widget_set_text(StatusBar->middle_label, string );
    previous_setting = MOUSE_MIDDLE_REPEAT;
  }
}

GtkWidget *x_status_bar_create(GschemToplevel *w_current)
{
  GtkWidget *status_bar;

  const char *middle_mouse_text;
  const char *right_mouse_text;

  /* bottom box */
  switch(w_current->middle_button) {

    /* remove this case eventually and make it a null case */
    case( MOUSE_MIDDLE_ACTION ):
      middle_mouse_text = _( RC_STR_MID_ACTION );
      break;

#ifdef HAVE_LIBSTROKE
    case( MOUSE_MIDDLE_STROKE ):
      middle_mouse_text = _( RC_STR_MID_STROKE );
      break;
#else
      /* remove this case eventually and make it a null case */
    case( MOUSE_MIDDLE_STROKE ):
      middle_mouse_text = _("none");
      break;
#endif
    case( MOUSE_MIDDLE_REPEAT ):
      middle_mouse_text = _("Repeat/none");
      break;

    case( MOUSE_MIDDLE_PAN ):
      middle_mouse_text = _( RC_STR_MID_MOUSEPAN );
      break;

    default:
      middle_mouse_text = _("none");
  }

  if (default_third_button == POPUP_ENABLED) {
    right_mouse_text = "Menu/Cancel";
  }
  else {
    right_mouse_text = "Pan/Cancel";
  }

  status_bar = GTK_WIDGET (g_object_new (GSCHEM_TYPE_STATUS_BAR,
                                        "grid-mode",   w_current->grid_mode,
                                        "grid-size",   100,
                                        "left-text",  _("Pick"),
                                        "middle-text", middle_mouse_text,
                                        "right-text",  right_mouse_text,
                                        "snap-mode",   w_current->snap,
                                        "snap-size",   w_current->snap_size,
                                        "status-text", _("Select Mode"),
                                        NULL));

  return status_bar;
}

/** @} endgroup status-module */
