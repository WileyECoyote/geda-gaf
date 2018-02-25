/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_label.h
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this library; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA,
 * <http://www.gnu.org/licenses/>.
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com> with
 * modifications, see the corresponding source file for details.
 *
 */
#ifndef __GEDA_LABEL_H__
#define __GEDA_LABEL_H__

/* When rotating ellipsizable text we want the natural size to request
 * more to ensure the label wont ever ellipsize in an allocation of full natural size.
 * */
#define ROTATION_ELLIPSIZE_PADDING 2

struct _GdkRGBA
{
  double red;
  double green;
  double blue;
  double alpha;
};

typedef struct _GdkRGBA  GdkRGBA;

#define GEDA_TYPE_LABEL            (geda_label_get_type ())
#define GEDA_LABEL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEDA_TYPE_LABEL, GedaLabel))
#define GEDA_LABEL_CLASS(class)    (G_TYPE_CHECK_CLASS_CAST ((class),  GEDA_TYPE_LABEL, GedaLabelClass))
#define GEDA_IS_LABEL(obj)         (is_a_geda_label((GedaLabel*)(obj)))
#define GEDA_IS_LABEL_CLASS(class) (G_TYPE_CHECK_CLASS_TYPE ((class),  GEDA_TYPE_LABEL))
#define GEDA_LABEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEDA_TYPE_LABEL, GedaLabelClass))

#define INVALIDATE_WRAP_WIDTH wrap_width = -1

/* Convenience macros to shorten line lengths */
#define GEDA_VM_LABEL_NEW  geda_visible_mnemonic_label_new
#define GEDA_AV_LABEL_NEW  geda_aligned_visible_label_new
#define GEDA_AM_LABEL_NEW  geda_aligned_mnemonic_label_new
#define GEDA_AVM_LABEL_NEW geda_aligned_visible_mnemonic_label_new

typedef struct _GedaLabel      GedaLabel;
typedef struct _GedaLabelData  GedaLabelData;
typedef struct _GedaLabelClass GedaLabelClass;

struct _GedaLabel
{
  GtkMisc        misc;

  PangoAttrList *attrs;
  PangoAttrList *markup_attrs;
  PangoLayout   *layout;

  char          *label;
  char          *text;
  double         angle;

  int            wrap_width;
  int            width_chars;
  int            max_width_chars;

  /*< private >*/
  GedaLabelData *priv;

};

struct _GedaLabelClass
{
  GtkMiscClass parent_class;

  void (* move_cursor)     (GedaLabel *label, GtkMovementStep step, int count, bool extend_selection);
  void (* copy_clipboard)  (GedaLabel *label);

  /* Hook to customize right-click popup for selectable labels */
  void (* populate_popup)  (GedaLabel  *label, void *menu);

  bool (* activate_link)   (GedaLabel *label, const char *uri);

};

#ifdef __cplusplus
extern "C" {
#endif

GedaType    geda_label_get_type                     (void) GEDA_CONST;
bool        is_a_geda_label                         (GedaLabel      *label);

GtkWidget  *geda_label_new                          (const char     *str) __attribute__((warn_unused_result));
GtkWidget  *geda_bold_label_new                     (const char     *str) __attribute__((warn_unused_result));
GtkWidget  *geda_mnemonic_label_new                 (const char     *str) __attribute__((warn_unused_result));
GtkWidget  *geda_visible_label_new                  (const char     *str) __attribute__((warn_unused_result));
GtkWidget  *geda_visible_bold_label_new             (const char     *str) __attribute__((warn_unused_result));
GtkWidget  *geda_visible_mnemonic_label_new         (const char     *str) __attribute__((warn_unused_result));
GtkWidget  *geda_aligned_label_new                  (const char     *str, float x, float y) __attribute__((warn_unused_result));
GtkWidget  *geda_aligned_bold_label_new             (const char     *str, float x, float y) __attribute__((warn_unused_result));
GtkWidget  *geda_aligned_visible_label_new          (const char     *str, float x, float y) __attribute__((warn_unused_result));
GtkWidget  *geda_aligned_visible_bold_label_new     (const char     *str, float x, float y) __attribute__((warn_unused_result));
GtkWidget  *geda_aligned_mnemonic_label_new         (const char     *str, float x, float y) __attribute__((warn_unused_result));
GtkWidget  *geda_aligned_visible_mnemonic_label_new (const char     *str, float x, float y) __attribute__((warn_unused_result));

/* Properties */

void           geda_label_get_alignment             (GedaLabel      *label,
                                                     float          *xalign,
                                                     float          *yalign);
void           geda_label_set_alignment             (GedaLabel      *label,
                                                     float           xalign,
                                                     float           yalign);
double         geda_label_get_angle                 (GedaLabel      *label);
void           geda_label_set_angle                 (GedaLabel      *label,
                                                     double          angle);
PangoAttrList *geda_label_get_attributes            (GedaLabel      *label);
void           geda_label_set_attributes            (GedaLabel      *label,
                                                     PangoAttrList  *attrs);
const char    *geda_label_get_current_uri           (GedaLabel      *label);
PangoAttrList *geda_label_get_effective_attributes  (GedaLabel      *label);
PangoEllipsizeMode geda_label_get_ellipsize         (GedaLabel      *label);
void               geda_label_set_ellipsize         (GedaLabel      *label,
                                                     PangoEllipsizeMode mode);
int                geda_label_get_cursor_position   (GedaLabel *label);
GtkJustification   geda_label_get_justify           (GedaLabel      *label);
void               geda_label_set_justify           (GedaLabel      *label,
                                                     GtkJustification jtype);
const char    *geda_label_get_label                 (GedaLabel      *label);
void           geda_label_set_label                 (GedaLabel      *label,
                                                     const char     *str);
PangoLayout   *geda_label_get_layout                (GedaLabel      *label);
void           geda_label_get_layout_offsets        (GedaLabel      *label,
                                                     int            *x,
                                                     int            *y);
bool           geda_label_get_line_wrap             (GedaLabel      *label);
void           geda_label_set_line_wrap             (GedaLabel      *label,
                                                     bool            wrap);
PangoWrapMode  geda_label_get_line_wrap_mode        (GedaLabel      *label);
void           geda_label_set_line_wrap_mode        (GedaLabel      *label,
                                                     PangoWrapMode   wrap_mode);
void           geda_label_set_markup                (GedaLabel      *label,
                                                     const char     *str);
void           geda_label_set_markup_with_mnemonic  (GedaLabel      *label,
                                                     const char     *str);
int            geda_label_get_max_width_chars       (GedaLabel      *label);
void           geda_label_set_max_width_chars       (GedaLabel      *label,
                                                     int             n_chars);
char           geda_label_get_mnemonic_char         (GedaLabel      *label);
void           geda_label_set_mnemonic_text         (GedaLabel      *label,
                                                     const char     *str);
unsigned int   geda_label_get_mnemonic_keyval       (GedaLabel      *label);
char           geda_label_get_mnemonic_lower        (GedaLabel      *label);
bool           geda_label_get_mnemonic_visible      (GedaLabel      *label);
void           geda_label_set_mnemonic_visible      (GedaLabel      *label,
                                                     bool            visible);
void     geda_label_set_mnemonics_visible_recursive (GtkWidget      *widget,
                                                     bool            mnemonics_visible);
GtkWidget     *geda_label_get_mnemonic_widget       (GedaLabel      *label);
void           geda_label_set_mnemonic_widget       (GedaLabel      *label,
                                                     GtkWidget      *widget);
void           geda_label_set_pattern               (GedaLabel      *label,
                                                     const char     *pattern);
bool           geda_label_get_selectable            (GedaLabel      *label);
void           geda_label_set_selectable            (GedaLabel      *label,
                                                     bool            setting);
void           geda_label_select_region             (GedaLabel      *label,
                                                     int             start_offset,
                                                     int             end_offset);
int            geda_label_get_selection_bound       (GedaLabel      *label);
bool           geda_label_get_selection_bounds      (GedaLabel      *label,
                                                     int            *start,
                                                     int            *end);
bool           geda_label_get_single_line_mode      (GedaLabel      *label);
void           geda_label_set_single_line_mode      (GedaLabel      *label,
                                                     bool            single_line_mode);
void           geda_label_set_text                  (GedaLabel      *label,
                                                     const char     *str);
const char    *geda_label_get_text                  (GedaLabel      *label);
bool           geda_label_get_track_visited_links   (GedaLabel      *label);
void           geda_label_set_track_visited_links   (GedaLabel      *label,
                                                     bool            track_links);
bool           geda_label_get_use_markup            (GedaLabel      *label);
void           geda_label_set_use_markup            (GedaLabel      *label,
                                                     bool            setting);
bool           geda_label_get_use_underline         (GedaLabel      *label);
void           geda_label_set_use_underline         (GedaLabel      *label,
                                                     bool            setting);
int            geda_label_get_width_chars           (GedaLabel      *label);
void           geda_label_set_width_chars           (GedaLabel      *label,
                                                     int             n_chars);
void           geda_label_report_instances          (void);

/* Widget Versions */

void           geda_label_widget_get_alignment      (GtkWidget      *widget,
                                                     float          *xalign,
                                                     float          *yalign);
void           geda_label_widget_set_alignment      (GtkWidget      *widget,
                                                     float           xalign,
                                                     float           yalign);

double         geda_label_widget_get_angle          (GtkWidget      *widget);
void           geda_label_widget_set_angle          (GtkWidget      *widget,
                                                     double          angle);

PangoEllipsizeMode geda_label_widget_get_ellipsize  (GtkWidget      *widget);
void               geda_label_widget_set_ellipsize  (GtkWidget      *widget,
                                                     PangoEllipsizeMode mode);

GtkJustification   geda_label_widget_get_justify    (GtkWidget      *widget);
void               geda_label_widget_set_justify    (GtkWidget      *widget,
                                                     GtkJustification jtype);
const char   *geda_label_widget_get_label           (GtkWidget      *widget);
void          geda_label_widget_set_label           (GtkWidget      *widget,
                                                     const char     *str);
int           geda_label_widget_get_max_width_chars (GtkWidget      *widget);
void          geda_label_widget_set_max_width_chars (GtkWidget      *widget,
                                                     int             n_chars);

bool           geda_label_widget_get_selectable     (GtkWidget      *widget);
void           geda_label_widget_set_selectable     (GtkWidget      *widget,
                                                     bool            setting);

const char    *geda_label_widget_get_text           (GtkWidget      *widget);
void           geda_label_widget_set_text           (GtkWidget      *widget,
                                                     const char     *str);
bool           geda_label_widget_get_use_markup     (GtkWidget      *widget);
void           geda_label_widget_set_use_markup     (GtkWidget      *widget,
                                                     bool            setting);

bool           geda_label_widget_get_use_underline  (GtkWidget      *widget);
void           geda_label_widget_set_use_underline  (GtkWidget      *widget,
                                                     bool            setting);
int            geda_label_widget_get_width_chars    (GtkWidget      *widget);
void           geda_label_widget_set_width_chars    (GtkWidget      *widget,
                                                     int             n_chars);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GEDA_LABEL_H__ */
