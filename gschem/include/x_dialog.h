/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_dialog.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2012-2016 Wiley Edward Hill <wileyhill@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Date: December 26, 2012
 * Contributing Author: Wiley Edward Hill <wileyhill@gmail.com>
 *
 */
/*!
 * \file x_dialog.h
 *
 * \brief header for the Dialog module
 */

#ifndef __X_DIALOG_H__
#define __X_DIALOG_H__

#define LEAVE_ALONE -2  /* Used as a flag for multiple selections */

#include "../../include/geda/geda_dialog_controls.h"

/* Define spacings for dialogs. Defines are in a sperate header,
 * some dialog use only the define, for example x_compselect
 */
#include "gschem_xdefines.h"

/* The header defines the GschemDialog class, Should all gschem dialogs
 * be derived from this class? WEH: May need to derive from lower level
 * for that */
#include "gschem_dialog.h"

/* Include widgets in libgedauio */
#include <geda_widgets.h>

#include "x_console.h"
#include "x_multiattrib.h"
#include "x_pagesel.h"
#include "x_print.h"
#include "x_states.h"

#define TRANSLATE_DIALOG_MAX_ENTRY 10
#define FIND_DIALOG_MAX_ENTRY      20
#define HIDE_DIALOG_MAX_ENTRY      20
#define SHOW_TEXT_DIALOG_MAX_ENTRY 20

typedef struct st_array_data     array_data;
typedef struct st_line_type_data line_type_data;
typedef struct st_fill_type_data fill_type_data;
typedef struct st_pin_type_data  pin_type_data;
typedef struct st_property_data  property_data;

struct st_array_data {

  GtkWidget *cnt_sel_entry;
  GtkWidget *x_size_entry;
  GtkWidget *y_size_entry;

  GtkWidget *row_spin;
  GtkWidget *col_spin;

  GtkWidget *row_off_entry;
  GtkWidget *col_off_entry;

  int wx;
  int wy;

  unsigned int press_hid;
  unsigned int release_hid;

  void (*post_responder) (GschemToplevel *);
  int  (*press_butt)     (GtkWidget *widget, GdkEventButton *, GschemToplevel *);
  int  (*release_butt)   (GtkWidget *widget, GdkEventButton *, GschemToplevel *);
};

struct st_line_type_data {

  GtkWidget *width_entry;
  GtkWidget *line_type;
  GtkWidget *end_type;
  GtkWidget *length_entry;
  GtkWidget *space_entry;

};

struct st_fill_type_data {

  GtkWidget *fill_type;
  GtkWidget *width_entry;
  GtkWidget *angle1_entry;
  GtkWidget *pitch1_entry;
  GtkWidget *angle2_entry;
  GtkWidget *pitch2_entry;

};

struct st_property_data {

  GtkWidget *symbol_entry;
  GtkWidget *device_entry;

  GtkWidget *author_entry;
  GtkWidget *version_entry;
  GtkWidget *version_cb;

  GtkWidget *opt_frame;
  GtkWidget *embed_cb;
  GtkWidget *lock_cb;
  GtkWidget *mirror_cb;

  GtkWidget *ulicense_entry;
  GtkWidget *dlicense_entry;

  GtkWidget *descr_entry;
  GtkWidget *doc_entry;
  GtkWidget *comment_entry;

  GtkWidget *electrical_cb;
  GtkWidget *refdes_combo;
  GtkWidget *refdes_entry;
  GtkWidget *slots_spin;
  GtkWidget *pins_spin;

  GtkWidget *value_entry;
  GtkWidget *foot_entry;
  GtkWidget *spice_entry;
  GtkWidget *mname_entry;

  GtkWidget *elect_table;

  unsigned int ver_handler;
};

struct st_pin_type_data {

  GtkWidget *node_type;
  GtkWidget *number_entry;
  GtkWidget *sequence_spin;
  GtkWidget *label_entry;
  GtkWidget *pin_electrical;
  GtkWidget *pin_mechanical;

  GtkWidget *set_node_type;
  GtkWidget *set_elect_type;
  GtkWidget *set_mech_type;
  GtkWidget *auto_number;
  GtkWidget *auto_sequence;
};

/** \defgroup Settings-Dialogs Settings Dialogs
 *  @{
 *  \ingroup (Standard-Dialogs)
 *  @}
 */
#endif /* __X_DIALOG_H__ */
