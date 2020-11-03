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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#define GTKSHEET_ENABLE_BROKEN

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <glib.h>

#include <gtksheet/gtksheet.h>

/* enumerations from "gtksheet.h" */
GType gtk_sheet_attr_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GTK_SHEET_FOREGROUND, "GTK_SHEET_FOREGROUND", "foreground" },
      { GTK_SHEET_BACKGROUND, "GTK_SHEET_BACKGROUND", "background" },
      { GTK_SHEET_FONT, "GTK_SHEET_FONT", "font" },
      { GTK_SHEET_JUSTIFICATION, "GTK_SHEET_JUSTIFICATION", "justification" },
      { GTK_SHEET_BORDER, "GTK_SHEET_BORDER", "border" },
      { GTK_SHEET_BORDER_COLOR, "GTK_SHEET_BORDER_COLOR", "border-color" },
      { GTK_SHEET_IS_EDITABLE, "GTK_SHEET_IS_EDITABLE", "is-editable" },
      { GTK_SHEET_IS_VISIBLE, "GTK_SHEET_IS_VISIBLE", "is-visible" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GtkSheetAttrType", values);
  }
  return etype;
}

GType gtk_sheet_state_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GTK_SHEET_NORMAL, "GTK_SHEET_NORMAL", "normal" },
      { GTK_SHEET_ROW_SELECTED, "GTK_SHEET_ROW_SELECTED", "row-selected" },
      { GTK_SHEET_COLUMN_SELECTED, "GTK_SHEET_COLUMN_SELECTED", "column-selected" },
      { GTK_SHEET_RANGE_SELECTED, "GTK_SHEET_RANGE_SELECTED", "range-selected" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GtkSheetState", values);
  }
  return etype;
}

GType gtk_sheet_entry_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GTK_SHEET_ENTRY_TYPE_DEFAULT, "GTK_SHEET_ENTRY_TYPE_DEFAULT", "default" },
      { GTK_SHEET_ENTRY_TYPE_GTK_ITEM_ENTRY, "GTK_SHEET_ENTRY_TYPE_GTK_ITEM_ENTRY", "gtk-item-entry" },
      { GTK_SHEET_ENTRY_TYPE_GTK_ENTRY, "GTK_SHEET_ENTRY_TYPE_GTK_ENTRY", "gtk-entry" },
      { GTK_SHEET_ENTRY_TYPE_GTK_TEXT_VIEW, "GTK_SHEET_ENTRY_TYPE_GTK_TEXT_VIEW", "gtk-text-view" },
      { GTK_SHEET_ENTRY_TYPE_GTK_SPIN_BUTTON, "GTK_SHEET_ENTRY_TYPE_GTK_SPIN_BUTTON", "gtk-spin-button" },
      { GTK_SHEET_ENTRY_TYPE_GTK_COMBO_BOX, "GTK_SHEET_ENTRY_TYPE_GTK_COMBO_BOX", "gtk-combo-box" },
      { GTK_SHEET_ENTRY_TYPE_GTK_COMBO_BOX_ENTRY, "GTK_SHEET_ENTRY_TYPE_GTK_COMBO_BOX_ENTRY", "gtk-combo-box-entry" },
      { GTK_SHEET_ENTRY_TYPE_GTK_COMBO, "GTK_SHEET_ENTRY_TYPE_GTK_COMBO", "gtk-combo" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GtkSheetEntryType", values);
  }
  return etype;
}

GType gtk_sheet_vertical_justification_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GTK_SHEET_VERTICAL_JUSTIFICATION_DEFAULT, "GTK_SHEET_VERTICAL_JUSTIFICATION_DEFAULT", "default" },
      { GTK_SHEET_VERTICAL_JUSTIFICATION_TOP, "GTK_SHEET_VERTICAL_JUSTIFICATION_TOP", "top" },
      { GTK_SHEET_VERTICAL_JUSTIFICATION_MIDDLE, "GTK_SHEET_VERTICAL_JUSTIFICATION_MIDDLE", "middle" },
      { GTK_SHEET_VERTICAL_JUSTIFICATION_BOTTOM, "GTK_SHEET_VERTICAL_JUSTIFICATION_BOTTOM", "bottom" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GtkSheetVerticalJustification", values);
  }
  return etype;
}
