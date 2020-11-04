/* gtkdatatextview - data textview widget, based on GtkTextView
 * Copyright 2013  Fredy Paquet <fredy@opag.ch>
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

#ifndef __GTK_DATA_TEXT_VIEW_H__
#define __GTK_DATA_TEXT_VIEW_H__

#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

#define GTK_TYPE_DATA_TEXT_VIEW              (gtk_data_text_view_get_type ())
#define GTK_DATA_TEXT_VIEW(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_DATA_TEXT_VIEW, GtkDataTextView))
#define GTK_DATA_TEXT_VIEW_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass),  GTK_TYPE_DATA_TEXT_VIEW, GtkDataTextViewClass))
#define GTK_IS_DATA_TEXT_VIEW(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_DATA_TEXT_VIEW))
#define GTK_IS_DATA_TEXT_VIEW_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass),  GTK_TYPE_DATA_TEXT_VIEW))
#define GTK_DATA_TEXT_VIEW_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj),  GTK_TYPE_DATA_TEXT_VIEW, GtkDataTextViewClass))

typedef struct _GtkDataTextView       GtkDataTextView;
typedef struct _GtkDataTextViewClass  GtkDataTextViewClass;

/**
 * GtkDataTextView:
 *
 * The GtkDataTextView struct contains only private data. It should
 * only be accessed through the functions described below.
 */
struct _GtkDataTextView
{
    /*< private >*/
    GtkTextView textview;

    char *description;       /* column description */
    int   max_length;        /* maximum length in characters */
    int   max_length_bytes;  /* maximum length in bytes */
};

struct _GtkDataTextViewClass
{
    GtkTextViewClass parent_class;
};

GType            gtk_data_text_view_get_type    (void) G_GNUC_CONST;
GtkDataTextView *gtk_data_text_view_new         (void);

const char  *gtk_data_text_view_get_description (GtkDataTextView *data_text_view);

void         gtk_data_text_view_set_description (GtkDataTextView *data_text_view,
                                                 const char      *description);

int          gtk_data_text_view_get_max_length  (GtkDataTextView *data_text_view);

void         gtk_data_text_view_set_max_length  (GtkDataTextView *data_text_view,
                                                 int              max_length);

int          gtk_data_text_view_get_max_length_bytes (GtkDataTextView *data_text_view);

void         gtk_data_text_view_set_max_length_bytes (GtkDataTextView *data_text_view,
                                                      int              max_length_bytes);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_DATA_TEXT_VIEW_H__ */
