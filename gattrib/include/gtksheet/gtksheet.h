/* GtkSheet widget for Gtk+.
 * Copyright (C) 1999-2001 Adrian E. Feiguin <adrian@ifir.ifir.edu.ar>
 *
 * Based on GtkClist widget by Jay Painter, but major changes.
 * Memory allocation routines inspired on SC (Spreadsheet Calculator)
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

#ifndef __GTK_SHEET_H__
#define __GTK_SHEET_H__

#include <stdint.h>

typedef enum
{
    GTK_SHEET_FOREGROUND,
    GTK_SHEET_BACKGROUND,
    GTK_SHEET_FONT,
    GTK_SHEET_JUSTIFICATION,
    GTK_SHEET_BORDER,
    GTK_SHEET_BORDER_COLOR,
    GTK_SHEET_IS_EDITABLE,
    GTK_SHEET_IS_VISIBLE
}
GtkSheetAttrType;

/**
 * GtkSheetState:
 * @GTK_SHEET_NORMAL: nothing selected
 * @GTK_SHEET_ROW_SELECTED: one row selected
 * @GTK_SHEET_COLUMN_SELECTED: one column selected
 * @GTK_SHEET_RANGE_SELECTED: rectangular area of cells selected
 *
 * Selection state of the #GtkSheet
 *
 **/
typedef enum
{
    GTK_SHEET_NORMAL,
    GTK_SHEET_ROW_SELECTED,
    GTK_SHEET_COLUMN_SELECTED,
    GTK_SHEET_RANGE_SELECTED
} GtkSheetState;

enum
{
    GTK_SHEET_LEFT_BORDER     = 1 << 0,
    GTK_SHEET_RIGHT_BORDER    = 1 << 1,
    GTK_SHEET_TOP_BORDER      = 1 << 2,
    GTK_SHEET_BOTTOM_BORDER   = 1 << 3
};

/**
 * GtkSheetEntryType:
 * @GTK_SHEET_ENTRY_TYPE_DEFAULT: default, applicat. controlled
 * @GTK_SHEET_ENTRY_TYPE_GTK_ITEM_ENTRY: GtkItemEntry
 * @GTK_SHEET_ENTRY_TYPE_GTK_ENTRY: GtkEntry
 * @GTK_SHEET_ENTRY_TYPE_GTK_TEXT_VIEW: GtkTextView
 * @GTK_SHEET_ENTRY_TYPE_GTK_DATA_TEXT_VIEW: GtkDataTextView
 * @GTK_SHEET_ENTRY_TYPE_GTK_SPIN_BUTTON: GtkSpinButton
 * @GTK_SHEET_ENTRY_TYPE_GTK_COMBO_BOX: GtkComboBox
 * @GTK_SHEET_ENTRY_TYPE_GTK_COMBO_BOX_ENTRY: GtkComboBoxEntry
 * @GTK_SHEET_ENTRY_TYPE_GTK_COMBO: GtkCombo, Deprecated
 *
 * Subset of GtkEditable Widgets to allow selecting a widget
 * from glade-3
 *
 **/
typedef enum
{
    GTK_SHEET_ENTRY_TYPE_DEFAULT,
    GTK_SHEET_ENTRY_TYPE_GTK_ITEM_ENTRY,
    GTK_SHEET_ENTRY_TYPE_GTK_ENTRY,
    GTK_SHEET_ENTRY_TYPE_GTK_TEXT_VIEW,
    GTK_SHEET_ENTRY_TYPE_GTK_DATA_TEXT_VIEW,
    GTK_SHEET_ENTRY_TYPE_GTK_SPIN_BUTTON,
    GTK_SHEET_ENTRY_TYPE_GTK_COMBO_BOX,
    GTK_SHEET_ENTRY_TYPE_GTK_COMBO_BOX_ENTRY,
    GTK_SHEET_ENTRY_TYPE_GTK_COMBO,
} GtkSheetEntryType;

/**
 * GtkSheetVerticalJustification:
 * @GTK_SHEET_VERTICAL_JUSTIFICATION_DEFAULT: default
 * @GTK_SHEET_VERTICAL_JUSTIFICATION_TOP: top aligned
 * @GTK_SHEET_VERTICAL_JUSTIFICATION_MIDDLE: middle aligned
 * @GTK_SHEET_VERTICAL_JUSTIFICATION_BOTTOM: bottom aligned
 *
 * Vertical text alignment.
 *
 **/
typedef enum
{
    GTK_SHEET_VERTICAL_JUSTIFICATION_DEFAULT,
    GTK_SHEET_VERTICAL_JUSTIFICATION_TOP,
    GTK_SHEET_VERTICAL_JUSTIFICATION_MIDDLE,
    GTK_SHEET_VERTICAL_JUSTIFICATION_BOTTOM,
} GtkSheetVerticalJustification;


#define G_TYPE_SHEET (gtk_sheet_get_type ())

#define GTK_SHEET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), G_TYPE_SHEET, GtkSheet))

#define GTK_IS_SHEET(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), G_TYPE_SHEET))

#define GTK_SHEET_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), G_TYPE_SHEET, GtkSheetClass))

#define GTK_IS_SHEET_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), G_TYPE_SHEET))

#define GTK_SHEET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), G_TYPE_SHEET, GtkSheetClass))

#define G_TYPE_SHEET_RANGE (gtk_sheet_range_get_type ())

/* Public flags, for compatibility */

#define GTK_SHEET_IS_LOCKED(sheet)            gtk_sheet_locked(sheet)
#define GTK_SHEET_ROW_FROZEN(sheet)          !gtk_sheet_rows_resizable(sheet)
#define GTK_SHEET_COLUMN_FROZEN(sheet)       !gtk_sheet_columns_resizable(sheet)
#define GTK_SHEET_AUTORESIZE(sheet)           gtk_sheet_autoresize(sheet)
#define GTK_SHEET_CLIP_TEXT(sheet)            gtk_sheet_clip_text(sheet)
#define GTK_SHEET_ROW_TITLES_VISIBLE(sheet)   gtk_sheet_row_titles_visible(sheet)
#define GTK_SHEET_COL_TITLES_VISIBLE(sheet)   gtk_sheet_column_titles_visible(sheet)
#define GTK_SHEET_AUTO_SCROLL(sheet)          gtk_sheet_autoscroll(sheet)
#define GTK_SHEET_JUSTIFY_ENTRY(sheet)        gtk_sheet_justify_entry(sheet)

typedef struct _GtkSheet            GtkSheet;
typedef struct _GtkSheetClass       GtkSheetClass;
typedef struct _GtkSheetChild       GtkSheetChild;
typedef struct _GtkSheetRow         GtkSheetRow;
typedef struct _GtkSheetColumn      GtkSheetColumn;
typedef struct _GtkSheetColumnClass GtkSheetColumnClass;
typedef struct _GtkSheetCell        GtkSheetCell;
typedef struct _GtkSheetRange       GtkSheetRange;
typedef struct _GtkSheetButton      GtkSheetButton;
typedef struct _GtkSheetCellAttr    GtkSheetCellAttr;
typedef struct _GtkSheetCellBorder  GtkSheetCellBorder;

#define GTK_SHEET_OPTIMIZE_COLUMN_DRAW  0  /* 0=off, 1=on */

/**
 * GtkSheetChild:
 *
 * The GtkSheetChild struct contains only private data.
 * It should only be accessed through the functions described below.
 */
struct _GtkSheetChild
{
    /*< private >*/
    GtkWidget *widget;
    int x,y;
    int  attached_to_cell;
    int  floating;
    int row, col;
    unsigned short xpadding;
    unsigned short ypadding;
    int  xexpand;
    int  yexpand;
    int  xshrink;
    int  yshrink;
    int  xfill;
    int  yfill;
};

/**
 * GtkSheetButton:
 *
 * The GtkSheetButton struct contains only private data.
 * It should only be accessed through the functions described below.
 */
struct _GtkSheetButton
{
    /*< private >*/
    GtkStateType state;
    char *label;

    int label_visible;
    GtkSheetChild *child;

    GtkJustification justification;
};


/**
 * GtkSheetCellBorder:
 *
 * The GtkSheetCellBorder struct contains only private data.
 * It should only be accessed through the functions described below.
 */
struct _GtkSheetCellBorder
{
    /*< private >*/
    int8_t mask;
    unsigned int width;
    GdkLineStyle line_style;
    GdkCapStyle cap_style;
    GdkJoinStyle join_style;
    GdkColor color;
};

/**
 * GtkSheetCellAttr:
 *
 * The GtkSheetCellAttr struct contains only private data.
 * It should only be accessed through the functions described below.
 */
struct _GtkSheetCellAttr
{
    /*< private >*/
    GtkJustification justification;
    GdkFont *font;
    PangoFontDescription *font_desc;
    GdkColor foreground;
    GdkColor background;
    GtkSheetCellBorder border;
    int is_editable;
    int is_visible;
    int do_font_desc_free;   /* TRUE if font_desc needs free */
};

/**
 * GtkSheetCell:
 *
 * The GtkSheetCell struct contains only private data.
 * It should only be accessed through the functions described below.
 */
struct _GtkSheetCell
{
    /*< private >*/
    GdkRectangle extent;  /* extent of pango layout + cell attributes.border (used for column auto-resize) */

    int row;
    int col;

    GtkSheetCellAttr *attributes;

    char *text;
    void *link;

    char *tooltip_markup; /* tooltip, which is marked up with the Pango text markup language */
    char *tooltip_text;  /* tooltip, without markup */
};

/**
 * GtkSheetRange:
 * @row0: upper left cell
 * @col0: upper left cell
 * @rowi:  lower right cell
 * @coli: lower right cell
 *
 * Defines a rectangular range of cells.
 */
struct _GtkSheetRange
{
    /*< public >*/
    int row0, col0; /* upper-left cell */
    int rowi, coli; /* lower-right cell */
};


/**
 * GtkSheetRow:
 *
 * The GtkSheetRow struct contains only private data.
 * It should only be accessed through the functions described below.
 */
struct _GtkSheetRow
{
    /*< private >*/
    char    *name;
    int      height;
    uint16_t requisition;
    int      top_ypixel;
    int      max_extent_height;  /* := max(Cell.extent.height) */

    GtkSheetButton button;
    int is_sensitive;
    int is_visible;

    char *tooltip_markup; /* tooltip, which is marked up with the Pango text markup language */
    char *tooltip_text;   /* tooltip, without markup */
};

#include "gtksheetcolumn.h"

/**
 * GtkSheet:
 *
 * The GtkSheet struct contains only private data.
 * It should only be accessed through the functions described below.
 */
struct _GtkSheet
{
    GtkContainer container;    /* parent instance */

    uint16_t flags;

    GtkSelectionMode selection_mode;
    int autoresize_columns;
    int autoresize_rows;
    int autoscroll;
    int clip_text;
    int justify_entry;
    int locked;

    unsigned int freeze_count;

    GdkColor bg_color;    /* cell background color */
    GdkColor grid_color;  /* grid color */
    GdkColor tm_color;    /* tooltip marker color */
    int show_grid;

    GList *children;      /* sheet children */

    /* allocation rectangle after the container_border_width
       and the width of the shadow border */
    GdkRectangle internal_allocation;

    char *title;
    char *description;         /* sheet description and further information for application use */

    GtkSheetRow     *row;
    GtkSheetColumn **column;  /* flexible array of column pointers */

    int rows_resizable;
    int columns_resizable;

    /* max number of diplayed cells */
    int maxrow;
    int maxcol;

    /* Displayed range */

    GtkSheetRange view;

    /* sheet data: dynamically allocated array of cell pointers */
    GtkSheetCell ***data;

    /* max number of allocated cells in **data */
    int maxallocrow;
    int maxalloccol;

    /* active cell */
    GtkSheetCell active_cell;
    GtkWidget   *sheet_entry;

    GType entry_type;            /* wanted entry type */
    GType installed_entry_type;  /* installed entry type */

    /* expanding selection */
    GtkSheetCell selection_cell;

    /* timer for automatic scroll during selection */
    int32_t timer;
    /* timer for flashing clipped range */
    int32_t clip_timer;
    int     interval;

    /* global selection button */
    GtkWidget *button;

    /* sheet state */
    GtkSheetState state;

    /* selected range */
    GtkSheetRange range;

    /*the scrolling window and it's height and width to
     * make things a little speedier */
    GdkWindow   *sheet_window;
    unsigned int sheet_window_width;
    unsigned int sheet_window_height;

    /* sheet backing pixmap */
    GdkWindow *pixmap;

    /* offsets for scrolling */
    int hoffset;
    int voffset;

    float old_hadjustment;
    float old_vadjustment;

    /* border shadow style */
    GtkShadowType shadow_type;

    GtkSheetVerticalJustification vjust;   /* default vertical text justification */

    /* Column Titles */
    GdkRectangle column_title_area;
    GdkWindow   *column_title_window;
    int          column_titles_visible;

    /* Row Titles */
    GdkRectangle row_title_area;
    GdkWindow   *row_title_window;
    int          row_titles_visible;

    /*scrollbars*/
    GtkAdjustment *hadjustment;
    GtkAdjustment *vadjustment;

    /* xor GC for the verticle drag line */
    GdkGC *xor_gc;

    /* gc for drawing unselected cells */
    GdkGC *fg_gc;
    GdkGC *bg_gc;

    /* cursor used to indicate dragging */
    GdkCursor *cursor_drag;

    /* the current x-pixel location of the xor-drag vline */
    int x_drag;

    /* the current y-pixel location of the xor-drag hline */
    int y_drag;

    /* current cell being dragged */
    GtkSheetCell drag_cell;
    /* current range being dragged */
    GtkSheetRange drag_range;

    /* clipped range */
    GtkSheetRange clip_range;
};

struct _GtkSheetClass
{
    GtkContainerClass parent_class;

    void (*set_scroll_adjustments)(GtkSheet *sheet,
                                   GtkAdjustment *hadjustment, GtkAdjustment *vadjustment);

    void (*select_row)(GtkSheet *sheet, int row);
    void (*select_column)(GtkSheet *sheet, int column);
    void (*select_range)(GtkSheet *sheet, GtkSheetRange *range);
    void (*clip_range)(GtkSheet *sheet, GtkSheetRange *clip_range);

    void (*resize_range)(GtkSheet *sheet,
                         GtkSheetRange *old_range, GtkSheetRange *new_range);

    void (*move_range)(GtkSheet *sheet,
                       GtkSheetRange *old_range, GtkSheetRange *new_range);

    int  (*traverse)(GtkSheet *sheet,
                        int row, int column, int *new_row, int *new_column);

    int  (*deactivate)(GtkSheet *sheet, int row, int column);
    int  (*activate)(GtkSheet *sheet, int row, int column);

    void (*set_cell)(GtkSheet *sheet, int row, int column);
    void (*clear_cell)(GtkSheet *sheet, int row, int column);
    void (*changed)(GtkSheet *sheet, int row, int column);
    void (*new_column_width)(GtkSheet *sheet, int col, unsigned int width);
    void (*new_row_height)(GtkSheet *sheet, int row, unsigned int height);

    int (*focus_in_event)(GtkSheet *sheet, GdkEventFocus *event);
    int (*focus_out_event)(GtkSheet *sheet, GdkEventFocus *event);

    void (*move_cursor)(GtkSheet *sheet,
                        GtkMovementStep step,
                        int count,
                        int  extend_selection);
};

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

GType      gtk_sheet_get_type       (void);
GType      gtk_sheet_range_get_type (void);

/* create a new sheet */
GtkWidget *gtk_sheet_new       (unsigned int rows, unsigned int columns, const char *title);
void       gtk_sheet_construct (GtkSheet *sheet, unsigned int rows, unsigned int columns, const char *title);

/* create a new browser sheet. It cells can not be edited */
GtkWidget *gtk_sheet_new_browser       (unsigned int rows, unsigned int columns, const char *title);
void       gtk_sheet_construct_browser (GtkSheet *sheet, unsigned int rows, unsigned int columns, const char *title);

/* create a new sheet with custom entry */
GtkWidget *gtk_sheet_new_with_custom_entry       (unsigned int rows, unsigned int columns,
                                                  const char *title, GType entry_type);
void       gtk_sheet_construct_with_custom_entry (GtkSheet *sheet,
                                                  unsigned int rows, unsigned int columns, const char *title, GType entry_type);

/* change scroll adjustments */
void gtk_sheet_set_hadjustment (GtkSheet *sheet, GtkAdjustment *adjustment);
void gtk_sheet_set_vadjustment (GtkSheet *sheet, GtkAdjustment *adjustment);

/* sheet entry */
void       gtk_sheet_change_entry     (GtkSheet *sheet, const GType entry_type);

GType      gtk_sheet_get_entry_type   (GtkSheet *sheet);
GtkWidget *gtk_sheet_get_entry        (GtkSheet *sheet);
GtkWidget *gtk_sheet_get_entry_widget (GtkSheet *sheet);

char *gtk_sheet_get_entry_text        (GtkSheet *sheet);
void  gtk_sheet_set_entry_text        (GtkSheet *sheet, const char *text);
void  gtk_sheet_set_entry_editable    (GtkSheet *sheet, const int  editable);
void  gtk_sheet_entry_select_region   (GtkSheet *sheet, int start_pos, int end_pos);

unsigned long gtk_sheet_entry_signal_connect_changed    (GtkSheet *sheet, GCallback handler);
void          gtk_sheet_entry_signal_disconnect_by_func (GtkSheet *sheet, GCallback handler);

/* Added by Steven Rostedt <steven.rostedt@lmco.com> */
GtkSheetState gtk_sheet_get_state        (GtkSheet *sheet);

unsigned int gtk_sheet_get_rows_count    (GtkSheet *sheet);
unsigned int gtk_sheet_get_columns_count (GtkSheet *sheet);

/* sheet's ranges - Added by Murray Cumming */
void gtk_sheet_get_visible_range       (GtkSheet *sheet, GtkSheetRange *range);
int  gtk_sheet_get_selection           (GtkSheet *sheet, GtkSheetState *state, GtkSheetRange *range);
void gtk_sheet_set_selection_mode      (GtkSheet *sheet, GtkSelectionMode mode);
void gtk_sheet_set_autoresize          (GtkSheet *sheet, int  autoresize);
void gtk_sheet_set_autoresize_columns  (GtkSheet *sheet, int  autoresize);
void gtk_sheet_set_autoresize_rows     (GtkSheet *sheet, int  autoresize);
int  gtk_sheet_autoresize              (GtkSheet *sheet);
int  gtk_sheet_autoresize_columns      (GtkSheet *sheet);
int  gtk_sheet_autoresize_rows         (GtkSheet *sheet);

void gtk_sheet_set_autoscroll(GtkSheet *sheet, int  autoscroll);
int  gtk_sheet_autoscroll(GtkSheet *sheet);
void gtk_sheet_set_clip_text(GtkSheet *sheet, int  clip_text);
int  gtk_sheet_clip_text(GtkSheet *sheet);
void gtk_sheet_set_justify_entry(GtkSheet *sheet, int  justify);
int  gtk_sheet_justify_entry(GtkSheet *sheet);
void gtk_sheet_set_vjustification(GtkSheet *sheet, GtkSheetVerticalJustification vjust);
GtkSheetVerticalJustification gtk_sheet_get_vjustification(GtkSheet *sheet);
void gtk_sheet_set_locked(GtkSheet *sheet, int  locked);
int  gtk_sheet_locked(GtkSheet *sheet);

/* set sheet title */
void gtk_sheet_set_title(GtkSheet *sheet, const char *title);
void gtk_sheet_set_description(GtkSheet *sheet, const char *description);
const char *gtk_sheet_get_description(GtkSheet *sheet, const char *description);

/* freeze all visual updates of the sheet.
 * Then thaw the sheet after you have made a number of changes.
 * The updates will occure in a more efficent way than if you made
 * them on a unfrozen sheet
 */
int  gtk_sheet_is_frozen(GtkSheet *sheet);
void gtk_sheet_freeze(GtkSheet *sheet);
void gtk_sheet_thaw(GtkSheet *sheet);

/* Background colors */
void gtk_sheet_set_background(GtkSheet *sheet, GdkColor *color);
void gtk_sheet_set_grid(GtkSheet *sheet, GdkColor *color);
void gtk_sheet_show_grid(GtkSheet *sheet, int  show);
int  gtk_sheet_grid_visible(GtkSheet *sheet);


/* set/get row title */
void gtk_sheet_set_row_title(GtkSheet *sheet, int row, const char *title);
const char *gtk_sheet_get_row_title(GtkSheet *sheet, int row);

/* set/get button label */
void gtk_sheet_row_button_add_label(GtkSheet *sheet,  int row, const char *label);
const char *gtk_sheet_row_button_get_label(GtkSheet *sheet,  int row);
void gtk_sheet_row_button_justify(GtkSheet *sheet, int row, GtkJustification justification);

/* scroll the viewing area of the sheet to the given column
 * and row; row_align and col_align are between 0-1 representing the
 * location the row should appear on the screnn, 0.0 being top or left,
 * 1.0 being bottom or right; if row or column is negative then there
 * is no change */
void gtk_sheet_moveto(GtkSheet *sheet, int row, int column, int row_align, int col_align);

/* resize column/row titles window */
void gtk_sheet_set_row_titles_width(GtkSheet *sheet, unsigned int width);

/* show/hide column/row titles window */
void gtk_sheet_show_row_titles(GtkSheet *sheet);
void gtk_sheet_hide_row_titles(GtkSheet *sheet);
int  gtk_sheet_row_titles_visible(GtkSheet *sheet);

/* row button sensitivity */

int  gtk_sheet_row_sensitive(GtkSheet *sheet, int row);
void gtk_sheet_row_set_sensitivity(GtkSheet *sheet, int row,  int  sensitive);
void gtk_sheet_rows_set_sensitivity(GtkSheet *sheet, int  sensitive);

/* row resizeability */

int  gtk_sheet_rows_resizable(GtkSheet *sheet);
void gtk_sheet_rows_set_resizable(GtkSheet *sheet, int  resizable);


/* row visibility */
int  gtk_sheet_row_visible(GtkSheet *sheet, int row);
void gtk_sheet_row_set_visibility(GtkSheet *sheet, int row, int  visible);

void gtk_sheet_row_label_set_visibility(GtkSheet *sheet, int row, int  visible);
void gtk_sheet_rows_labels_set_visibility(GtkSheet *sheet, int  visible);


/* sheet tooltips */
char *gtk_sheet_get_tooltip_markup(GtkSheet *sheet);
void  gtk_sheet_set_tooltip_markup(GtkSheet *sheet, const char *markup);
char *gtk_sheet_get_tooltip_text(GtkSheet *sheet);
void  gtk_sheet_set_tooltip_text(GtkSheet *sheet, const char *text);

/* row tooltips */
char *gtk_sheet_row_get_tooltip_markup(GtkSheet *sheet, const int row);
void  gtk_sheet_row_set_tooltip_markup(GtkSheet *sheet, const int row, const char *markup);
char *gtk_sheet_row_get_tooltip_text(GtkSheet *sheet, const int row);
void  gtk_sheet_row_set_tooltip_text(GtkSheet *sheet, const int row, const char *text);

/* cell tooltips */
char *gtk_sheet_cell_get_tooltip_markup (GtkSheet *sheet, const int row, const int col);
void  gtk_sheet_cell_set_tooltip_markup (GtkSheet *sheet,
                                         const int row, const int col, const char *markup);
char *gtk_sheet_cell_get_tooltip_text   (GtkSheet *sheet, const int row, const int col);
void  gtk_sheet_cell_set_tooltip_text   (GtkSheet *sheet,
                                        const int row, const int col, const char *text);

/* selection range. The range gets highlighted, and its bounds are stored in sheet->range  */
void gtk_sheet_select_row(GtkSheet *sheet, int row);
void gtk_sheet_select_column(GtkSheet *sheet, int column);
void gtk_sheet_select_range(GtkSheet *sheet, const GtkSheetRange *range);

/* clipboard */
void gtk_sheet_clip_range(GtkSheet *sheet, const GtkSheetRange *clip_range);
void gtk_sheet_unclip_range(GtkSheet *sheet);
int  gtk_sheet_in_clip(GtkSheet *sheet);

/* get scrollbars adjustment */
GtkAdjustment *gtk_sheet_get_vadjustment(GtkSheet *sheet);
GtkAdjustment *gtk_sheet_get_hadjustment(GtkSheet *sheet);

/* obvious */
void gtk_sheet_unselect_range(GtkSheet *sheet);

/* set active cell where the entry will be displayed
 * returns FALSE if current cell can't be deactivated or
 * requested cell can't be activated */
int  gtk_sheet_set_active_cell(GtkSheet *sheet, int row, int column);
void gtk_sheet_get_active_cell(GtkSheet *sheet, int *row, int *column);

/* movement */
void gtk_sheet_set_tab_direction(GtkSheet *sheet, GtkDirectionType dir);

/* set cell contents and allocate memory if needed */
void gtk_sheet_set_cell(GtkSheet *sheet, int row, int col,
                        GtkJustification justification, const char *text);
void gtk_sheet_set_cell_text(GtkSheet *sheet, int row, int col, const char *text);

/* get cell contents */
char *gtk_sheet_cell_get_text(GtkSheet *sheet, int row, int col);

/* clear cell contents */
void gtk_sheet_cell_clear(GtkSheet *sheet, int row, int column);

/* clear cell contents and remove links */
void gtk_sheet_cell_delete(GtkSheet *sheet, int row, int column);

/* clear range contents. If range==NULL the whole sheet will be cleared */
void gtk_sheet_range_clear(GtkSheet *sheet, const GtkSheetRange *range);

/* clear range contents and remove links */
void gtk_sheet_range_delete(GtkSheet *sheet, const GtkSheetRange *range);

/* get cell state: GTK_STATE_NORMAL, GTK_STATE_SELECTED */
GtkStateType gtk_sheet_cell_get_state(GtkSheet *sheet, int row, int col);

/* Handles cell links */
void  gtk_sheet_link_cell(GtkSheet *sheet, int row, int col, void *link);
void *gtk_sheet_get_link(GtkSheet *sheet, int row, int col);
void  gtk_sheet_remove_link(GtkSheet *sheet, int row, int col);

/* get row and column correspondig to the given position in the screen */
int  gtk_sheet_get_pixel_info(GtkSheet *sheet,
    GdkWindow *window, int x, int y, int *row, int *column);

/* get area of a given cell */
int  gtk_sheet_get_cell_area(GtkSheet *sheet, int row, int column, GdkRectangle *area);

/* set row height */
void gtk_sheet_set_row_height(GtkSheet *sheet, int row, unsigned int height);

/* append ncols columns to the end of the sheet */
void gtk_sheet_add_column(GtkSheet *sheet, unsigned int ncols);

/* append nrows row to the end of the sheet */
void gtk_sheet_add_row(GtkSheet *sheet, unsigned int nrows);

/* insert nrows rows before the given row and pull right */
void gtk_sheet_insert_rows(GtkSheet *sheet, unsigned int row, unsigned int nrows);

/* insert ncols columns before the given col and pull down */
void gtk_sheet_insert_columns(GtkSheet *sheet, unsigned int col, unsigned int ncols);

/* delete nrows rows starting in row */
void gtk_sheet_delete_rows(GtkSheet *sheet, unsigned int row, unsigned int nrows);

/* delete ncols columns starting in col */
void gtk_sheet_delete_columns(GtkSheet *sheet, unsigned int col, unsigned int ncols);

/* set abckground color of the given range */
void gtk_sheet_range_set_background(GtkSheet *sheet,
                                    const GtkSheetRange *urange, const GdkColor *color);

/* set foreground color (text color) of the given range */
void gtk_sheet_range_set_foreground(GtkSheet *sheet,
                                    const GtkSheetRange *urange, const GdkColor *color);

/* set text justification (GTK_JUSTIFY_LEFT, RIGHT, CENTER) of the given range.
 * The default value is GTK_JUSTIFY_LEFT. If autoformat is on, the
 * default justification for numbers is GTK_JUSTIFY_RIGHT */
void gtk_sheet_range_set_justification(GtkSheet *sheet,
                                       const GtkSheetRange *urange, GtkJustification just);

/* set if cell contents can be edited or not in the given range:
 * accepted values are TRUE or FALSE. */
void gtk_sheet_range_set_editable(GtkSheet *sheet,
                                  const GtkSheetRange *urange, int editable);

/* set if cell contents are visible or not in the given range:
 * accepted values are TRUE or FALSE.*/
void gtk_sheet_range_set_visible(GtkSheet *sheet,
                                 const GtkSheetRange *urange, int  visible);

/* set cell border style in the given range.
 * mask values are CELL_LEFT_BORDER, CELL_RIGHT_BORDER, CELL_TOP_BORDER,
 * CELL_BOTTOM_BORDER
 * width is the width of the border line in pixels
 * line_style is the line_style for the border line */
void gtk_sheet_range_set_border(GtkSheet *sheet,
                                const GtkSheetRange *urange, int mask, unsigned int width, int line_style);

/* set border color for the given range */
void gtk_sheet_range_set_border_color(GtkSheet *sheet,
                                      const GtkSheetRange *urange, const GdkColor *color);

/* set font for the given range */
void gtk_sheet_range_set_font(GtkSheet *sheet,
                              const GtkSheetRange *urange, PangoFontDescription *font_desc);

/* get cell attributes of the given cell */
/* TRUE means that the cell is currently allocated */
int  gtk_sheet_get_attributes(GtkSheet *sheet, int row, int col, GtkSheetCellAttr *attributes);


GtkSheetChild *gtk_sheet_put    (GtkSheet *sheet, GtkWidget *child, int x, int y);
void  gtk_sheet_attach_floating (GtkSheet *sheet, GtkWidget *widget, int row, int col);
void  gtk_sheet_attach_default  (GtkSheet *sheet, GtkWidget *widget, int row, int col);
void  gtk_sheet_attach          (GtkSheet *sheet, GtkWidget *widget,
                                 int row, int col, int xoptions, int yoptions, int xpadding, int ypadding);
void  gtk_sheet_move_child      (GtkSheet *sheet, GtkWidget *widget, int x, int y);
const GtkSheetChild *gtk_sheet_get_child_at (GtkSheet *sheet, int row, int col);
void  gtk_sheet_button_attach               (GtkSheet *sheet, GtkWidget *widget, int row, int col);

/*< private >*/

void _gtk_sheet_range_fixup(GtkSheet *sheet, GtkSheetRange *range);

void _gtk_sheet_entry_size_allocate(GtkSheet *sheet);
void _gtk_sheet_button_size_request(GtkSheet *sheet,
                                    GtkSheetButton *button, GtkRequisition *requisition);
void _gtk_sheet_scrollbar_adjust(GtkSheet *sheet);

void _gtk_sheet_recalc_top_ypixels(GtkSheet *sheet);
void _gtk_sheet_recalc_left_xpixels(GtkSheet *sheet);
void _gtk_sheet_recalc_view_range(GtkSheet *sheet);

void _gtk_sheet_reset_text_column(GtkSheet *sheet, int start_column);

void _gtk_sheet_range_draw(GtkSheet *sheet,
                           const GtkSheetRange *range, int  activate_active_cell);
void gtk_sheet_hide_active_cell(GtkSheet *sheet);
void _gtk_sheet_redraw_internal(GtkSheet *sheet,
                                int  reset_hadjustment, int  reset_vadjustment);

void _gtk_sheet_draw_button(GtkSheet *sheet, int row, int col);

GtkSheetEntryType _gtk_sheet_entry_type_from_gtype(GType entry_type);
GType _gtk_sheet_entry_type_to_gtype(GtkSheetEntryType ety);

unsigned int _gtk_sheet_row_default_height(GtkWidget *widget);

void _gtk_sheet_child_hide(GtkSheetChild *child);
void _gtk_sheet_child_show(GtkSheetChild *child);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_SHEET_H__ */


