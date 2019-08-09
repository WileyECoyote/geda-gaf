/* -*- C x_scroll.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*!
 * \file x_scroll.c
 * \brief Main Window Auxiliary Module for Scrollbars
 */

#include <math.h>

#include "../../include/gschem.h"
#include <geda_debug.h>

/** \defgroup scrollbars-module Scrollbars Module
 *  @{\brief This group contains functions for the scrollbars
 *    \ingroup main-window
 */

/*! \brief Set Horizontal Scroll Bar Range
 *  \par Function Description
 *   This functions sets the upper and lower limits of the
 *  horizontal scroll bar to the left of right values of the
 *  drawing area.
 */
void x_hscrollbar_set_ranges(GschemToplevel *w_current)
{
  GtkAdjustment        *hadjustment;

  if (w_current->scrollbars == FALSE) {
    return;
  }

  hadjustment =
  gtk_range_get_adjustment(GTK_RANGE(w_current->h_scrollbar));

  hadjustment->lower = w_current->world_left;
  hadjustment->upper = w_current->world_right;

}

/*! \brief Idle Update Horizontal Scroll Bar
 *  \par Function Description
 *   This functions updates the scale of horizontal scroll bar.
 */
static bool
x_hscrollbar_idle_update(GschemToplevel *w_current)
{
  GedaToplevel  *toplevel = w_current->toplevel;
  GtkAdjustment *hadjustment;

  if (w_current->h_scrollbar) {

    hadjustment = gtk_range_get_adjustment (GTK_RANGE (w_current->h_scrollbar));

    hadjustment->value = toplevel->page_current->left;

    hadjustment->page_size = fabs(toplevel->page_current->right -
    toplevel->page_current->left);

    hadjustment->page_increment = hadjustment->page_size - 100.0;

#if DEBUG
    printf("H %f %f\n", hadjustment->lower, hadjustment->upper);
    printf("Hp %f\n", hadjustment->page_size);
#endif

    g_signal_emit_by_name(G_OBJECT(hadjustment), "changed");
    g_signal_emit_by_name(G_OBJECT(hadjustment), "value_changed");
  }


  return FALSE;
}

/*! \brief Schedule Update Horizontal Scroll Bar
 *  \par Function Description
 *   This functions create an idle thread to updates the scale of
 *   horizontal scroll bar.
 */
void x_hscrollbar_update(GschemToplevel *w_current)
{
  if (w_current->scrollbars) {
    gschem_threads_idle_add (x_hscrollbar_idle_update, w_current);
  }

}

/*! \brief Set Vertical Scroll Bar Range
 *  \par Function Description
 *   This functions sets the upper and lower limits of the
 *  vertical scroll bar to the top of bottom values of the
 *  drawing area.
 */
void x_vscrollbar_set_ranges(GschemToplevel *w_current)
{
  if (w_current->scrollbars) {

    GtkAdjustment *vadjustment;

    vadjustment =
    gtk_range_get_adjustment(GTK_RANGE(w_current->v_scrollbar));

    vadjustment->lower = w_current->world_top;
    vadjustment->upper = w_current->world_bottom;
  }
}

/*! \brief Idle Update Vertical Scroll Bar
 *  \par Function Description
 *   This functions updates the scale of vertical scroll bar.
 */
static bool
x_vscrollbar_idle_update(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GtkAdjustment *vadjustment;

  if (w_current->v_scrollbar != NULL) {

    vadjustment =
    gtk_range_get_adjustment(GTK_RANGE(w_current->v_scrollbar));

    vadjustment->page_size = fabs(toplevel->page_current->bottom -
                                  toplevel->page_current->top);

    vadjustment->page_increment = vadjustment->page_size - 100.0;

    vadjustment->value =
    w_current->world_bottom - toplevel->page_current->bottom;

#if DEBUG
    printf("V %f %f\n", vadjustment->lower, vadjustment->upper);
    printf("Vp %f\n", vadjustment->page_size);
#endif

    g_signal_emit_by_name(G_OBJECT(vadjustment), "changed");
    g_signal_emit_by_name(G_OBJECT(vadjustment), "value_changed");
  }

  return FALSE;
}

/*! \brief Schedule Update Vertical Scroll Bar
 *  \par Function Description
 *   This functions starts an idle thread to updates the scale of
 *   vertical scroll bar.
 */
void x_vscrollbar_update(GschemToplevel *w_current)
{
  if (w_current->scrollbars) {
    gschem_threads_idle_add (x_vscrollbar_idle_update, w_current);
  }
}

/*! \brief Update Scroll Bars
 *  \par Function Description
 *   This functions calls the preceding functions; x_hscrollbar_update
 *   and v_hscrollbar_update to update the individual bars.
 */
void x_scrollbars_update(GschemToplevel *w_current)
{
  if (w_current->scrollbars) {
    gschem_threads_idle_add (x_hscrollbar_idle_update, w_current);
    gschem_threads_idle_add (x_vscrollbar_idle_update, w_current);
  }
}

/** @} endgroup scrollbars-module */
