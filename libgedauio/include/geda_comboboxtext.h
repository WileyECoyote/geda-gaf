/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_comboboxtext.h
 *
 * GTK - The GIMP Toolkit
 *
 * Copyright (C) 2010 Christian Dywan
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA <http://www.gnu.org/licenses/>.
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com> with
 * modifications, October 5th, 2013. See corresponding source file
 * for details.
 */

#include <gtk/gtk.h>

#include "geda_combobox.h"

#ifndef __GEDA_COMBO_BOX_TEXT_H__
#define __GEDA_COMBO_BOX_TEXT_H__

#define GEDA_TYPE_COMBO_BOX_TEXT                 (geda_combo_box_text_get_type ())
#define GEDA_COMBO_BOX_TEXT(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_COMBO_BOX_TEXT, GedaComboBoxText))
#define GEDA_COMBO_BOX_TEXT_CLASS(klass)         (G_TYPE_CHECK_CLASS_CAST ((klass),  GEDA_TYPE_COMBO_BOX_TEXT, GedaComboBoxTextClass))
#define GEDA_IS_COMBO_BOX_TEXT(obj)              (is_a_geda_combo_box_text((GedaComboBoxText*)obj))
#define GEDA_IS_COMBO_BOX_TEXT_CLASS(klass)      (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEDA_TYPE_COMBO_BOX_TEXT))
#define GEDA_COMBO_BOX_TEXT_GET_CLASS(obj)       (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_COMBO_BOX_TEXT, GedaComboBoxTextClass))

typedef struct _GedaComboBoxText      GedaComboBoxText;
typedef struct _GedaComboBoxTextClass GedaComboBoxTextClass;

struct _GedaComboBoxText
{
  /*< private >*/
  GedaComboBox  parent_instance;
  GedaType      instance_type;
  GtkListStore *store;
  GtkWidget    *tree;
  GtkWidget    *button;
  GtkWidget    *entry;
  int           count;
};

struct _GedaComboBoxTextClass
{
  GedaComboBoxClass parent_class;
};

#ifdef __cplusplus
extern "C" {
#endif

GType        geda_combo_box_text_get_type              (void) GEDA_CONST;
bool         is_a_geda_combo_box_text                  (GedaComboBoxText  *combo_box);
GtkWidget   *geda_combo_box_text_new                   (void);
GtkWidget   *geda_combo_box_text_new_with_entry        (void);
GtkWidget   *geda_combo_box_text_list_new              (void);

/* Short-hand versions */
void         geda_combo_box_text_append                (GedaComboBoxText   *combo_box,
                                                        const char         *text);
void         geda_combo_box_text_insert                (GedaComboBoxText   *combo_box,
                                                        int                 position,
                                                        const char         *text);
void         geda_combo_box_text_prepend               (GedaComboBoxText   *combo_box,
                                                        const char          *text);
void         geda_combo_box_text_remove                (GedaComboBoxText   *combo_box,
                                                        int                 position);
void         geda_combo_box_text_remove_all            (GedaComboBoxText   *combo_box);

void         geda_combo_box_text_list_append           (GedaComboBoxText   *combo_box,
                                                        const char         *text,
                                                        const char         *text2);
void         geda_combo_box_text_list_insert           (GedaComboBoxText   *combo_box,
                                                        int                 position,
                                                        const char         *text,
                                                        const char         *text2);
void         geda_combo_box_text_list_prepend          (GedaComboBoxText   *combo_box,
                                                        const char         *text,
                                                        const char         *text2);

/* These exist to be consistent with Gtk function naming */
void         geda_combo_box_text_append_text           (GedaComboBoxText   *combo_box,
                                                        const char         *text);
void         geda_combo_box_text_insert_text           (GedaComboBoxText   *combo_box,
                                                        int                 position,
                                                        const char         *text);
void         geda_combo_box_text_prepend_text          (GedaComboBoxText   *combo_box,
                                                        const char         *text);
void         geda_combo_box_text_remove_text           (GedaComboBoxText   *combo_box,
                                                        int                 position);
void         geda_combo_box_text_remove_all_text       (GedaComboBoxText   *combo_box);

void         geda_combo_box_text_set_active            (GedaComboBoxText   *combo_box,
                                                        int                 position);
int          geda_combo_box_text_get_active            (GedaComboBoxText   *combo_box);

char        *geda_combo_box_text_get_active_text       (GedaComboBoxText   *combo_box);

bool         geda_combo_box_text_set_active_text       (GedaComboBoxText   *combo_box,
                                                        const char         *text);
void         geda_combo_box_text_set_activate_default  (GedaComboBoxText   *combo_box,
                                                        bool setting);
GtkEntry    *geda_combo_box_text_get_entry             (GedaComboBoxText   *combo_box);

GtkWidget   *geda_combo_box_text_get_entry_widget      (GedaComboBoxText   *combo_box);

/* Widget Receptors - it had better be a GedaComboBoxText widget */

void         geda_combo_box_text_widget_append         (GtkWidget          *widget,
                                                        const char         *text);
void         geda_combo_box_text_widget_insert         (GtkWidget          *widget,
                                                        int                 position,
                                                        const char         *text);
void         geda_combo_box_text_widget_prepend        (GtkWidget          *widget,
                                                        const char         *text);
void         geda_combo_box_text_widget_remove         (GtkWidget          *widget,
                                                        int                 position);
void         geda_combo_box_text_widget_set_active     (GtkWidget          *widget,
                                                        int                 position);
int          geda_combo_box_text_widget_get_active     (GtkWidget          *widget);

char        *geda_combo_box_text_widget_get_active_text(GtkWidget          *widget);

bool         geda_combo_box_text_widget_set_active_text(GtkWidget          *widget,
                                                        const char         *text);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_COMBO_BOX_TEXT_H__ */
