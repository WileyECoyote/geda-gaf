/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 4 tab-width: 4 -*- */
/*
 * File: geda_gui_funcs.h
 *
 * gEDA - GPL Electronic Design Automation
 *
 * Copyright (C) 2012-2015 Wiley Edward Hill
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
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 */

#define GetComboBoxActive(widget) gtk_combo_box_get_active ((GtkComboBox*)widget)

#define SetWidgetTip(widget, tip) gtk_widget_set_tooltip_text (GTK_WIDGET(widget), tip);

#define EntrySelectAll(widget)  \
            if (GTK_IS_WIDGET(widget))  { \
              const char *text = GetEntryText(widget); \
              if (text != NULL) { \
                gtk_editable_select_region((GtkEditable*) widget, 0, strlen(text)); \
              } \
            }

#define GetEntryText(widget) gtk_entry_get_text ((GtkEntry*) widget)
#define GetEntryLength(widget) gtk_entry_get_text_length ((GtkEntry*) widget)
#define SetEntryText(widget, text) \
            if (text == NULL) \
              gtk_entry_set_text((GtkEntry*) widget, ""); \
            else \
              gtk_entry_set_text((GtkEntry*) widget, text);

#define GetToggleState(widget) gtk_toggle_button_get_active ((GtkToggleButton*)widget)
#define SetToggleState(widget, state) gtk_toggle_button_set_active ((GtkToggleButton*)widget, state);

#define SetSpinIncrements( name, step, page) gtk_spin_button_set_increments ((GtkSpinButton*)name, step, page);
#define SetSpinDigits( name, digits) gtk_spin_button_set_digits ((GtkSpinButton*)name, digits);
#define SetSpinValue( name, var) gtk_spin_button_set_value ((GtkSpinButton*)name, var);
#define SetupSpinner( name, digits, step, page) SetSpinDigits((GtkSpinButton*)name, digits); \
                                                SetSpinIncrements((GtkSpinButton*)name, step, page);
