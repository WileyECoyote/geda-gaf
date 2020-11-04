/* GtkSheetColumn widget for Gtk+.
 * Copyright 2011  Fredy Paquet <fredy@opag.ch>
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

#if defined(GTK_DISABLE_SINGLE_INCLUDES) && !defined (__GTK_H_INSIDE__) && !defined (GTK_COMPILATION) && !defined (__GTKEXTRA_H_INSIDE__)
#   error "Only <gtkextra/gtkextra.h> can be included directly."
#endif

#ifndef __GTK_SHEET_COLUMN_H__
#define __GTK_SHEET_COLUMN_H__


#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

/**
 * GtkSheetColumn:
 *
 * The GtkSheetColumn struct contains only private data.
 * It should only be accessed through the functions described below.
 */
struct _GtkSheetColumn
{
    /*< private >*/
    GtkWidget parent;

    GtkSheet *sheet;  /* the sheet this column belongs to */

    char *title;

    int width;
    unsigned short requisition;
    int left_xpixel;   /* left edge of the column*/
    int max_extent_width;  /* := max(Cell.extent.width) */

    GtkSheetButton button;

#if GTK_SHEET_OPTIMIZE_COLUMN_DRAW>0
    int left_text_column;      /* min left column displaying text on this column */
    int right_text_column;     /* max right column displaying text on this column */
#endif

    GtkJustification justification;    /* horizontal text justification */
    GtkSheetVerticalJustification vjust;   /* vertical text justification */

    int    is_key;             /* marker for key columns */
    int    is_readonly;        /* flag to supersede cell.attributes.is_editable */
    char  *data_type;          /* data type for application use */
    char  *data_format;        /* cell content formatting template */
    char  *description;        /* column description and further information about the column */

    GType entry_type;          /* Column entry_type or G_TYPE_NONE */

    int max_length;            /* maximum character length */
    int max_length_bytes;      /* maximum byte length */
    GtkWrapMode wrap_mode;     /* wrap-mode */
};

struct _GtkSheetColumnClass
{
    GtkWidgetClass parent_class;

    /*< private >*/
};


#define G_TYPE_SHEET_COLUMN \
        (gtk_sheet_column_get_type ())

#define GTK_SHEET_COLUMN(obj) \
        (G_TYPE_CHECK_INSTANCE_CAST ((obj), G_TYPE_SHEET_COLUMN, GtkSheetColumn))

#define GTK_IS_SHEET_COLUMN(obj) \
        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), G_TYPE_SHEET_COLUMN))

#define GTK_SHEET_COLUMN_CLASS(klass) \
        (G_TYPE_CHECK_CLASS_CAST ((klass), G_TYPE_SHEET_COLUMN, GtkSheetColumnClass))

#define GTK_IS_SHEET_COLUMN_CLASS(klass) \
        (G_TYPE_CHECK_CLASS_TYPE ((klass), G_TYPE_SHEET_COLUMN))

#define GTK_SHEET_COLUMN_GET_CLASS(obj) \
        (G_TYPE_INSTANCE_GET_CLASS ((obj), G_TYPE_SHEET_COLUMN, GtkSheetColumnClass))

#define GTK_SHEET_COLUMN_MIN_WIDTH 10
#define GTK_SHEET_COLUMN_DEFAULT_WIDTH 80
#define GTK_SHEET_COLUMN_DEFAULT_JUSTIFICATION GTK_JUSTIFY_LEFT

#define GTK_SHEET_COLUMN_IS_VISIBLE(colptr)  \
        (gtk_widget_get_visible(GTK_WIDGET(colptr)))

#define GTK_SHEET_COLUMN_SET_VISIBLE(colptr, value) \
        (gtk_widget_set_visible(GTK_WIDGET(colptr), value))

#define GTK_SHEET_COLUMN_IS_SENSITIVE(colptr) \
        (gtk_widget_is_sensitive(GTK_WIDGET(colptr)))

#define GTK_SHEET_COLUMN_SET_SENSITIVE(colptr, value) \
        (gtk_widget_set_sensitive(GTK_WIDGET(colptr), value))

/* methods */

GType gtk_sheet_column_get_type(void);

GtkSheetColumn *gtk_sheet_column_get           (GtkSheet *sheet, int col);
int            gtk_sheet_column_get_index      (GtkSheetColumn *colobj);

/* set column width */
void      gtk_sheet_set_column_width           (GtkSheet *sheet, int column, guint width);
const int gtk_sheet_get_column_width           (GtkSheet *sheet, int column);

void             gtk_sheet_column_set_justification(GtkSheet *sheet, int col, GtkJustification just);
GtkJustification gtk_sheet_column_get_justification(GtkSheet *sheet, int col);
void             gtk_sheet_column_set_vjustification(GtkSheet *sheet, int col, GtkSheetVerticalJustification vjust);
GtkSheetVerticalJustification gtk_sheet_column_get_vjustification(GtkSheet *sheet, int col);

/* column properties */
int    gtk_sheet_column_get_iskey              (GtkSheet *sheet, const int col);
void   gtk_sheet_column_set_iskey              (GtkSheet *sheet, const int col, const int is_key);
int    gtk_sheet_column_get_readonly           (GtkSheet *sheet, const int col);
void   gtk_sheet_column_set_readonly           (GtkSheet *sheet, const int col, const int  is_readonly);
char  *gtk_sheet_column_get_format             (GtkSheet *sheet, const int col);
void   gtk_sheet_column_set_format             (GtkSheet *sheet, const int col, const char  *format);
char  *gtk_sheet_column_get_datatype           (GtkSheet *sheet, const int col);
void   gtk_sheet_column_set_datatype           (GtkSheet *sheet, const int col, const char  *data_type);
char  *gtk_sheet_column_get_description        (GtkSheet *sheet, const int col);
void   gtk_sheet_column_set_description        (GtkSheet *sheet, const int col, const char  *description);
GType  gtk_sheet_column_get_entry_type         (GtkSheet *sheet, const int col);
void   gtk_sheet_column_set_entry_type         (GtkSheet *sheet, const int col, const GType entry_type);

/* column tooltips */
char  *gtk_sheet_column_get_tooltip_markup     (GtkSheet *sheet, const int col);
void gtk_sheet_column_set_tooltip_markup       (GtkSheet *sheet, const int col, const char  *markup);
char  *gtk_sheet_column_get_tooltip_text       (GtkSheet *sheet, const int col);
void gtk_sheet_column_set_tooltip_text         (GtkSheet *sheet, const int col, const char  *text);

/* column button sensitivity */

int  gtk_sheet_column_sensitive                (GtkSheet *sheet, int column);
void gtk_sheet_column_set_sensitivity          (GtkSheet *sheet, int column, int  sensitive);
void gtk_sheet_columns_set_sensitivity         (GtkSheet *sheet, int  sensitive);

/* column resizeability */

int  gtk_sheet_columns_resizable               (GtkSheet *sheet);
void gtk_sheet_columns_set_resizable           (GtkSheet *sheet, int  resizable);

/* column visibility */
int  gtk_sheet_column_visible                  (GtkSheet *sheet, int column);
void gtk_sheet_column_set_visibility           (GtkSheet *sheet, int column, int  visible);

void gtk_sheet_column_button_justify           (GtkSheet *sheet, int col, GtkJustification justification);
const char  *gtk_sheet_column_button_get_label (GtkSheet *sheet, int col);
void gtk_sheet_column_label_set_visibility     (GtkSheet *sheet, int col, int  visible);
void gtk_sheet_columns_labels_set_visibility   (GtkSheet *sheet, int  visible);

void gtk_sheet_column_button_add_label         (GtkSheet *sheet,  int col, const char  *label);

void gtk_sheet_set_column_titles_height        (GtkSheet *sheet, guint height);
void gtk_sheet_show_column_titles              (GtkSheet *sheet);
void gtk_sheet_hide_column_titles              (GtkSheet *sheet);
int  gtk_sheet_column_titles_visible           (GtkSheet *sheet);

const char  *gtk_sheet_get_column_title        (GtkSheet *sheet, int column);
void gtk_sheet_set_column_title                (GtkSheet *sheet, int column, const char  *title);

/*< private >*/

int _gtk_sheet_column_left_xpixel              (GtkSheet *sheet, int col);
int _gtk_sheet_column_right_xpixel             (GtkSheet *sheet, int col);

void _gtk_sheet_column_size_request            (GtkSheet *sheet, int col, guint *requisition);
void _gtk_sheet_column_buttons_size_allocate   (GtkSheet *sheet);
void _gtk_sheet_column_button_set              (GtkSheet *sheet, int col);
void _gtk_sheet_column_button_release          (GtkSheet *sheet, int col);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_SHEET_COLUMN_H__ */
