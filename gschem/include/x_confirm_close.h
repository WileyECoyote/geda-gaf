/* C header -*- indent-tabs-mode: t; c-basic-offset: 2 tab-width: 2 -*- */
/* "$Id include/x_confirm_close.h $"
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * \file x_confirm_close.h
 *
 * \brief header for the Confirm Close Dialog
 */

/** \defgroup ConfirmCloseDialog Confirm Close Dialog Class
 *  @{
 *  \ingroup Systemic-Dialogs
 *  \image html confirm_close_dialog.png
 *  \image latex confirm_close_dialog.png "Confirm Close" width=14cm
 *  @} end group ConfirmCloseDialog
 */

/*! \class ConfirmCloseDialog x_confirm_close.h "x_confirm_close.h"
 *  \brief Confirm Close Dialog
 *  \par
 *  Confirm Close Dialog ...
 */

#ifndef __CLOSE_CONFIRMATION_H__
#define __CLOSE_CONFIRMATION_H__

#define TYPE_CLOSE_CONFIRMATION_DIALOG            (confirm_close_dialog_get_type ())
#define CLOSE_CONFIRMATION_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_CLOSE_CONFIRMATION_DIALOG, ConfirmCloseDialog))
#define CLOSE_CONFIRMATION_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  TYPE_CLOSE_CONFIRMATION_DIALOG, ConfirmCloseDialogClass))
#define IS_CLOSE_CONFIRMATION_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_CLOSE_CONFIRMATION_DIALOG))
#define IS_CLOSE_CONFIRMATION_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  TYPE_CLOSE_CONFIRMATION_DIALOG))
#define CLOSE_CONFIRMATION_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  TYPE_CLOSE_CONFIRMATION_DIALOG, ConfirmCloseDialogClass))

typedef struct _ConfirmCloseDialog      ConfirmCloseDialog;
typedef struct _ConfirmCloseDialogClass ConfirmCloseDialogClass;

struct _ConfirmCloseDialog
{
  GtkDialog parent;

  GtkListStore *store_unsaved_pages;
};

struct _ConfirmCloseDialogClass
{
  GtkDialogClass parent_class;
};

enum {
  PROP_UNSAVED_PAGE=1,
  PROP_UNSAVED_PAGES,
  PROP_SELECTED_PAGES
};

enum {
  COLUMN_SAVE,
  COLUMN_PAGE,
  NUM_COLUMNS
};

GedaType confirm_close_dialog_get_type (void);

#endif /* __CLOSE_CONFIRMATION_H__ */
