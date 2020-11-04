/* gtksheettypebuiltins.c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#ifndef __GTKSHEET_TYPE_BUILTINS_H__
#define __GTKSHEET_TYPE_BUILTINS_H__

#include <glib-object.h>

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

/* enumerations from "gtksheetwidget.h" */
unsigned int gtk_sheet_attr_type_get_type (void);

#define GTK_TYPE_SHEET_ATTR_TYPE (gtk_sheet_attr_type_get_type())

unsigned int gtk_sheet_state_get_type (void);

#define GTK_TYPE_SHEET_STATE (gtk_sheet_state_get_type())

unsigned int gtk_sheet_border_flags_get_type (void);

#define GTK_TYPE_SHEET_BORDER_FLAGS (gtk_sheet_border_flags_get_type())

unsigned int gtk_sheet_entry_type_get_type (void);

unsigned int gtk_sheet_vertical_justification_get_type (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTKSHEET_TYPE_BUILTINS_H__ */
