/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_combobox.h
 * libgedauio - gEDA's library for User Interface Objects
 *
 * Copyright (C) 2002, 2003  Kristian Rietveld <kris@gtk.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com> with
 * many modifications, August 16th, 2014. See corresponding source
 * file for details.
 */
#ifndef __GEDA_COMBO_BOX_H__
#define __GEDA_COMBO_BOX_H__

#include <gtk/gtk.h>

#define GEDA_COMBO_DEFAULT_WRAP 2

typedef enum
{
  GEDA_VIEW_AUTO,
  GEDA_VIEW_TREE,
  GEDA_VIEW_MENU,
} IDE_COMBO_VIEW_STYLE;

#define GEDA_TYPE_COMBO_BOX             (geda_combo_box_get_type ())
#define GEDA_COMBO_BOX(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_COMBO_BOX, GedaComboBox))
#define GEDA_COMBO_BOX_CLASS(vtable)    (G_TYPE_CHECK_CLASS_CAST ((vtable), GEDA_TYPE_COMBO_BOX, GedaComboBoxClass))
#define GEDA_IS_COMBO_BOX(obj)          (is_a_geda_combo_box((GedaComboBox*)obj))
#define GEDA_IS_COMBO_BOX_CLASS(vtable) (G_TYPE_CHECK_CLASS_TYPE ((vtable), GEDA_TYPE_COMBO_BOX))
#define GEDA_COMBO_BOX_GET_CLASS(inst)  (G_TYPE_INSTANCE_GET_CLASS ((inst), GEDA_TYPE_COMBO_BOX, GedaComboBoxClass))

typedef struct _GedaComboBox      GedaComboBox;
typedef struct _GedaComboBoxClass GedaComboBoxClass;
typedef struct _GedaComboBoxData  GedaComboBoxData;

struct _GedaComboBox
{
  GtkBin   parent_instance;
  GedaType instance_type;

  /*< private >*/
  int tip_column;

  GedaComboBoxData *priv;
};

struct _GedaComboBoxClass
{
  GtkBinClass parent_class;

  /* signals */
  void     (* changed)           (GedaComboBox *combo_box);
  void     (* view_changed)      (GedaComboBox *combo_box, unsigned int mode);
  char  *  (* format_entry_text) (GedaComboBox *combo_box, const char *path);

  /* vfuncs */
  char  *  (* get_active_text)   (GedaComboBox *combo_box);

};

#ifdef __cplusplus
extern "C" {
#endif

/* construction */
GedaType      geda_combo_box_get_type                 (void) GEDA_CONST;
bool          is_a_geda_combo_box                     (GedaComboBox  *combo_box);
GtkWidget    *geda_combo_box_new                      (void);
GtkWidget    *geda_combo_box_new_with_entry           (void);
GtkWidget    *geda_combo_box_new_with_model           (GtkTreeModel *model);
GtkWidget    *geda_combo_box_new_with_model_and_entry (GtkTreeModel *model);

/* grids */
int           geda_combo_box_get_wrap_width         (GedaComboBox *combo_box);
void          geda_combo_box_set_wrap_width         (GedaComboBox *combo_box,
                                                     int           width);
int           geda_combo_box_get_row_span_column    (GedaComboBox *combo_box);
void          geda_combo_box_set_row_span_column    (GedaComboBox *combo_box,
                                                     int           row_span);
int           geda_combo_box_get_column_span_column (GedaComboBox *combo_box);
void          geda_combo_box_set_column_span_column (GedaComboBox *combo_box,
                                                     int           column_span);

bool          geda_combo_box_get_add_tearoffs       (GedaComboBox *combo_box);
void          geda_combo_box_set_add_tearoffs       (GedaComboBox *combo_box,
                                                     bool          add_tearoffs);

const char   *geda_combo_box_get_title              (GedaComboBox *combo_box);
void          geda_combo_box_set_title              (GedaComboBox *combo_box,
                                                     const char   *title);

bool          geda_combo_box_get_focus_on_click     (GedaComboBox *combo);
void          geda_combo_box_set_focus_on_click     (GedaComboBox *combo,
                                                     bool          focus_on_click);

void          geda_combo_box_set_tooltip_column     (GedaComboBox *combo,
                                                     int           column);

/* get/set active item */
int           geda_combo_box_get_active             (GedaComboBox   *combo_box);
void          geda_combo_box_set_active             (GedaComboBox   *combo_box,
                                                     int             index);
bool          geda_combo_box_get_active_iter        (GedaComboBox   *combo_box,
                                                     GtkTreeIter     *iter);
void          geda_combo_box_set_active_iter        (GedaComboBox   *combo_box,
                                                     GtkTreeIter     *iter);

/* getters and setters */

GtkTreeModel *geda_combo_box_get_model              (GedaComboBox    *combo_box);
void          geda_combo_box_set_model              (GedaComboBox    *combo_box,
                                                     GtkTreeModel    *model);

GtkTreeViewRowSeparatorFunc geda_combo_box_get_row_separator_func (GedaComboBox               *combo_box);
void                        geda_combo_box_set_row_separator_func (GedaComboBox               *combo_box,
                                                                   GtkTreeViewRowSeparatorFunc func,
                                                                   void                       *data,
                                                                   GDestroyNotify              destroy);

void               geda_combo_box_set_button_sensitivity (GedaComboBox        *combo_box,
                                                          GtkSensitivityType   sensitivity);
GtkSensitivityType geda_combo_box_get_button_sensitivity (GedaComboBox        *combo_box);

bool               geda_combo_box_get_has_entry          (GedaComboBox        *combo_box);
int                geda_combo_box_get_entry_text_column  (GedaComboBox        *combo_box);
void               geda_combo_box_set_entry_text_column  (GedaComboBox        *combo_box,
                                                          int                  text_column);

#if !defined (GEDA_DISABLE_DEPRECATED) || defined (GEDA_COMPILATION)

/* convenience -- text */
GtkWidget    *geda_combo_box_new_text         (void);
void          geda_combo_box_append_text      (GedaComboBox   *combo_box,
                                               const char     *text);
void          geda_combo_box_insert_text      (GedaComboBox   *combo_box,
                                               int             position,
                                               const char     *text);
void          geda_combo_box_prepend_text     (GedaComboBox   *combo_box,
                                               const char     *text);
void          geda_combo_box_remove_index     (GedaComboBox   *combo_box,
                                               int             position);
char         *geda_combo_box_get_active_text  (GedaComboBox   *combo_box);

#endif

/* programmatic control */
void          geda_combo_box_popup                (GedaComboBox *combo_box);
void          geda_combo_box_popdown              (GedaComboBox *combo_box);
AtkObject    *geda_combo_box_get_popup_accessible (GedaComboBox *combo_box);

/* ------------------------ Widget Versions ------------------------ */

/* grids */
int           geda_combo_widget_get_wrap_width         (GtkWidget   *combo_box);
void          geda_combo_widget_set_wrap_width         (GtkWidget   *combo_box,
                                                        int          width);
int           geda_combo_widget_get_row_span_column    (GtkWidget   *combo_box);
void          geda_combo_widget_set_row_span_column    (GtkWidget   *combo_box,
                                                        int          row_span);
int           geda_combo_widget_get_column_span_column (GtkWidget   *combo_box);
void          geda_combo_widget_set_column_span_column (GtkWidget   *combo_box,
                                                        int          column_span);

bool          geda_combo_widget_get_add_tearoffs       (GtkWidget   *combo_box);
void          geda_combo_widget_set_add_tearoffs       (GtkWidget   *combo_box,
                                                        bool         add_tearoffs);

const char   *geda_combo_widget_get_title              (GtkWidget   *combo_box);
void          geda_combo_widget_set_title              (GtkWidget   *combo_box,
                                                        const char  *title);

bool          geda_combo_widget_get_focus_on_click     (GtkWidget   *combo);
void          geda_combo_widget_set_focus_on_click     (GtkWidget   *combo,
                                                        bool         focus_on_click);

void          geda_combo_widget_set_tooltip_column     (GtkWidget   *combo,
                                                        int          column);
/* get/set active item */
int           geda_combo_widget_get_active             (GtkWidget   *combo_box);
void          geda_combo_widget_set_active             (GtkWidget   *combo_box,
                                                        int          index_);
bool          geda_combo_widget_get_active_iter        (GtkWidget   *combo_box,
                                                        GtkTreeIter *iter);
void          geda_combo_widget_set_active_iter        (GtkWidget   *combo_box,
                                                        GtkTreeIter *iter);
/* getters and setters */
GtkTreeModel *geda_combo_widget_get_model              (GtkWidget    *combo_box);
void          geda_combo_widget_set_model              (GtkWidget    *combo_box,
                                                        GtkTreeModel *model);

bool          geda_combo_widget_get_has_entry          (GtkWidget   *combo_box);
GtkWidget    *geda_combo_widget_get_entry              (GtkWidget   *combo_box);
void          geda_combo_widget_set_entry_text_column  (GtkWidget   *combo_box,
                                                        int          text_column);
int           geda_combo_widget_get_entry_text_column  (GtkWidget   *combo_box);

/* programmatic control */
void          geda_combo_widget_popup                  (GtkWidget   *combo_box);
void          geda_combo_widget_popdown                (GtkWidget   *combo_box);
AtkObject    *geda_combo_widget_get_popup_accessible   (GtkWidget   *combo_box);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_COMBO_BOX_H__ */
