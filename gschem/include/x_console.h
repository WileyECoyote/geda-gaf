/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_console.h$"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2015 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
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
 * \file x_console.h
 *
 * \brief header for the Console Dialog module
 */
/** \defgroup Console-Dialog Console Dialog
 *    @{
 *    \ingroup (Standard-Dialogs)
 *    \image html console_dialog.png
 *    \image latex console_dialog.png
 *    @} end group Logging-Handlers
 */

typedef enum {
  CONSOLE_COMMAND_MODE,
  CONSOLE_INPUT_MODE
} ConsoleInputMode;

#define TYPE_CONSOLE         (console_get_type())
#define CONSOLE(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj),   TYPE_CONSOLE, Console))
#define CONSOLE_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST    ((klass), TYPE_CONSOLE, ConsoleClass))
#define IS_CONSOLE(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj),   TYPE_CONSOLE))

typedef struct _ConsoleClass ConsoleClass;
typedef struct _Console      Console;

struct _ConsoleClass {
  GschemDialogClass parent_class;
};

struct _Console {
  GschemDialog parent_instance;
  GtkTextView *textview;

  unsigned int handler;

};

GedaType console_get_type (void) GEDA_CONST;
