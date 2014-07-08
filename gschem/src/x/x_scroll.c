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
#include <stdio.h>
#include <math.h>

#include "gschem.h"
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

/*! \brief Update Horizontal Scroll Bar
 *  \par Function Description
 *   This functions updates the scale of horizontal scroll bar.
 */
void x_hscrollbar_update(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GtkAdjustment *hadjustment;

  if (w_current->scrollbars == FALSE) {
    return;
  }

  if (w_current->h_scrollbar == NULL) {
    return;
  }

  hadjustment = gtk_range_get_adjustment (GTK_RANGE (w_current->h_scrollbar));

  hadjustment->value = toplevel->page_current->left;

  hadjustment->page_size = fabs(toplevel->page_current->right -
                                toplevel->page_current->left);

  hadjustment->page_increment = hadjustment->page_size - 100.0;

#if DEBUG
  printf("H %f %f\n", hadjustment->lower, hadjustment->upper);
  printf("Hp %f\n", hadjustment->page_size);
#endif

  gtk_signal_emit_by_name(GTK_OBJECT(hadjustment), "changed");
  gtk_signal_emit_by_name(GTK_OBJECT(hadjustment), "value_changed");
}

/*! \brief Set Vertical Scroll Bar Range
 *  \par Function Description
 *   This functions sets the upper and lower limits of the
 *  vertical scroll bar to the top of bottom values of the
 *  drawing area.
 */
void x_vscrollbar_set_ranges(GschemToplevel *w_current)
{
  GtkAdjustment *vadjustment;

  if (w_current->scrollbars == FALSE) {
    return;
  }

  vadjustment =
  gtk_range_get_adjustment(GTK_RANGE(w_current->v_scrollbar));

  vadjustment->lower = w_current->world_top;
  vadjustment->upper = w_current->world_bottom;
}

/*! \brief Update Vertical Scroll Bar
 *  \par Function Description
 *   This functions updates the scale of vertical scroll bar.
 */
void x_vscrollbar_update(GschemToplevel *w_current)
{
  GedaToplevel *toplevel = w_current->toplevel;
  GtkAdjustment *vadjustment;

  if (w_current->scrollbars == FALSE) {
    return;
  }

  if (w_current->v_scrollbar == NULL) {
    return;
  }

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

  gtk_signal_emit_by_name(GTK_OBJECT(vadjustment), "changed");
  gtk_signal_emit_by_name(GTK_OBJECT(vadjustment), "value_changed");
}

/*! \brief Update Scroll Bars
 *  \par Function Description
 *   This functions calls the preceding functions; x_hscrollbar_update
 *   and v_hscrollbar_update to update the individual bars.
 */
void x_scrollbars_update(GschemToplevel *w_current)
{
  if (w_current->scrollbars == FALSE) {
    return;
  }

  x_hscrollbar_update(w_current);
  x_vscrollbar_update(w_current);
}
