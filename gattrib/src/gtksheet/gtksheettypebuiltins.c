
/* Generated data (by glib-mkenums) */

#define GTKSHEET_ENABLE_BROKEN
#include "gtksheet.h"

/* enumerations from "gtksheetwidget.h" */
unsigned int
gtk_sheet_attr_type_get_type (void)
{
  static unsigned int etype = 0;
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
unsigned int
gtk_sheet_state_get_type (void)
{
  static unsigned int etype = 0;
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
unsigned int
gtk_sheet_border_flags_get_type (void)
{
  static unsigned int etype = 0;
  if (etype == 0) {
    static const GFlagsValue values[] = {
      { GTK_SHEET_LEFT_BORDER, "GTK_SHEET_LEFT_BORDER", "left-border" },
      { GTK_SHEET_RIGHT_BORDER, "GTK_SHEET_RIGHT_BORDER", "right-border" },
      { GTK_SHEET_TOP_BORDER, "GTK_SHEET_TOP_BORDER", "top-border" },
      { GTK_SHEET_BOTTOM_BORDER, "GTK_SHEET_BOTTOM_BORDER", "bottom-border" },
      { 0, NULL, NULL }
    };
    etype = g_flags_register_static ("GtkSheetBorderFlags", values);
  }
  return etype;
}

/* Generated data ends here */

