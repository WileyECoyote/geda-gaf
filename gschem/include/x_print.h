/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_print.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
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
 */
/*!
 * \file x_print.h
 *
 * \brief header for the Print Dialog module
 */

#ifndef __X_PRINT_H__
#define __X_PRINT_H__

/*
 * PrintDialog class
 */

#define TYPE_PRINT_DIALOG         (print_dialog_get_type())
#define PRINT_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_PRINT_DIALOG, PrintDialog))
#define PRINT_DIALOG_CLASS(class) (G_TYPE_CHECK_CLASS_CAST ((class),  TYPE_PRINT_DIALOG))
#define IS_PRINT_DIALOG(obj)      (is_a_print_dialog((PrintDialog*)obj))

typedef struct _PrintDialogClass PrintDialogClass;
typedef struct _PrintDialog PrintDialog;

struct _PrintDialogClass
{
  GschemDialogClass parent_class;
};

struct _PrintDialog
{
  GschemDialog    parent_instance;
  GedaType        instance_type;
  GtkEntry       *fnfield,   *cmdfield;
  GtkRadioButton *fileradio, *cmdradio;
  GtkButton      *saveasbutton;
  GedaComboBox   *orientcbox, *layoutcbox, *papercbox;
};

GedaType print_dialog_get_type (void) GEDA_CONST;
bool     is_a_print_dialog     (PrintDialog *dialog);

#endif /* !__X_PRINT_H__ */
