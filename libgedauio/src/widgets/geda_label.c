/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_label.h
 *
 * GTK - The GIMP Toolkit
 *
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
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
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 * with modifications, July 2013.
 */

/*!
 * \brief GedaLabel A Widget to display Text
 * \par
 *  This is a temporary replacement for the stock GtkLabel widget. The
 *  "bulk" of this code is from Gtk+3.7.4, but has been modified to
 *  run under the gtk+2.24.17 library. According to Valgrind: this
 *  version has fewer errors and does not have the memory leaks common
 *  to both the aforementioned versions. The direct leaks are not really
 *  in the Gtklabel widget. The leaks are related to Pango, FontConfig
 *  and somewhere else in GTK, possibly after tweeks by Debi. (note:
 *  After upgrading Cairo, from libcairo2_1.12.2-3 to libcairo2-dbg_1.
 *  12.14-5, and associated dependencies, under Debian, Wheezy->Sid, the
 *  amount of memory reported as "definitely lost" doubled.) This version
 *  implements a work around.
 * \par
 *  See geda_label_ensure_layout. Another suppression was not acceptable,
 *  couldn't let it go, so contrived this version ....
 *
 * \defgroup GedaLabel Text Label
 * @{
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define GTK_COMPILATION 1

#include <geda.h>
#include <geda_standard.h>


#include <glib.h>
#include <glib-object.h>

#include <math.h>

#include <gtk/gtk.h>

#include "geda_gtk_compat.h"
#include "geda_label.h"
#include "geda_imagemenuitem.h"

#include "gettext.h"

#include <geda_debug.h>

#if (( GLIB_MAJOR_VERSION == 2 ) && ( GLIB_MINOR_VERSION < 28 ))
#  include "geda_keysyms.h"
#else
#  include <gdk/gdkkeysyms.h>
#endif

/* GLIB < 2.30 */
#ifndef G_VALUE_INIT
#define G_VALUE_INIT  { 0, { { 0 } } }
#endif

#define PangoFontDescr  PangoFontDescription

struct _GedaLabelPrivate
{
  GedaLabelSelectionInfo *select_info;
  PangoFontMap           *font_map;

  GtkWidget *mnemonic_widget;
  GtkWindow *mnemonic_window;

  unsigned int    mnemonics_visible  : 1;
  unsigned int    jtype              : 2;
  unsigned int    wrap               : 1;
  unsigned int    use_underline      : 1;
  unsigned int    use_markup         : 1;
  unsigned int    ellipsize          : 3;
  unsigned int    single_line_mode   : 1;
  unsigned int    have_transform     : 1;
  unsigned int    in_click           : 1;
  unsigned int    wrap_mode          : 3;
  unsigned int    pattern_set        : 1;
  unsigned int    track_links        : 1;

  unsigned int    mnemonic_keyval;

};

typedef struct
{
  char *uri;
  char *title;     /* the title attribute, used as tooltip */
  bool visited;    /* get set when the link is activated; this flag
                    * gets preserved over later set_markup() calls
                    */
  int start;       /* position of the link in the PangoLayout */
  int end;
} GedaLabelLink;

struct _GedaLabelSelectionInfo
{
  GdkWindow *window;
  int selection_anchor;
  int selection_end;
  GtkWidget *popup_menu;

  GList *links;
  GedaLabelLink *active_link;

  int drag_start_x;
  int drag_start_y;

  unsigned int in_drag      : 1;
  unsigned int select_words : 1;
  unsigned int selectable   : 1;
  unsigned int link_clicked : 1;
};

enum {
  MOVE_CURSOR,
  COPY_CLIPBOARD,
  POPULATE_POPUP,
  ACTIVATE_LINK,
  ACTIVATE_CURRENT_LINK,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_LABEL,
  PROP_ATTRIBUTES,
  PROP_USE_MARKUP,
  PROP_USE_UNDERLINE,
  PROP_JUSTIFY,
  PROP_PATTERN,
  PROP_WRAP,
  PROP_WRAP_MODE,
  PROP_SELECTABLE,
  PROP_MNEMONIC_KEYVAL,
  PROP_MNEMONIC_WIDGET,
  PROP_CURSOR_POSITION,
  PROP_SEL_BOUND,
  PROP_ELLIPSIZE,
  PROP_WIDTH_CHARS,
  PROP_SINGLE_LINE_MODE,
  PROP_ANGLE,
  PROP_MAX_WIDTH,
  PROP_TRACK_VISITED_LINKS
};

static unsigned int signals[LAST_SIGNAL] = { 0 };

static const GdkColor default_link_color = { 0, 0, 0, 0xeeee };
static const GdkColor default_visited_link_color = { 0, 0x5555, 0x1a1a, 0x8b8b };

static void geda_label_set_property          (GObject         *object,
                                              unsigned int     prop_id,
                                              const GValue    *value,
                                              GParamSpec      *pspec);
static void geda_label_get_property          (GObject         *object,
                                              unsigned int     prop_id,
                                              GValue          *value,
                                              GParamSpec      *pspec);
static void geda_label_destroy               (GtkObject *object);
static void geda_label_finalize              (GObject         *object);
static void geda_label_size_allocate         (GtkWidget       *widget,
                                              GtkAllocation   *allocation);
static void geda_label_size_request          (GtkWidget        *widget,
                                              GtkRequisition   *requisition);
static void geda_label_state_changed         (GtkWidget        *widget,
                                              GtkStateType      state);
static void geda_label_style_set             (GtkWidget        *widget,
                                              GtkStyle         *style);
static void geda_label_direction_changed     (GtkWidget        *widget,
                                              GtkTextDirection  direction);
static int  geda_label_expose                (GtkWidget        *widget,
                                              GdkEventExpose   *event);
static bool geda_label_focus                 (GtkWidget       *widget,
                                             GtkDirectionType direction);

static void geda_label_realize               (GtkWidget       *widget);
static void geda_label_unrealize             (GtkWidget       *widget);
static void geda_label_map                   (GtkWidget       *widget);
static void geda_label_unmap                 (GtkWidget       *widget);

static bool geda_label_button_press          (GtkWidget        *widget,
                                              GdkEventButton   *event);
static bool geda_label_button_release        (GtkWidget        *widget,
                                              GdkEventButton   *event);
static bool geda_label_motion                (GtkWidget        *widget,
                                              GdkEventMotion   *event);
static bool geda_label_leave_notify          (GtkWidget        *widget,
                                              GdkEventCrossing *event);

static void geda_label_grab_focus            (GtkWidget       *widget);

static bool geda_label_query_tooltip         (GtkWidget       *widget,
                                              int              x,
                                              int              y,
                                              bool             keyboard_tip,
                                              GtkTooltip      *tooltip);

static void geda_label_set_text_internal          (GedaLabel  *label,
                                                   char       *str);
static void geda_label_set_label_internal         (GedaLabel  *label,
                                                   char       *str);
static void geda_label_set_use_markup_internal    (GedaLabel  *label,
                                                   bool        val);
static void geda_label_set_use_underline_internal (GedaLabel  *label,
                                                   bool        val);
static void geda_label_set_uline_text_internal    (GedaLabel  *label,
                                             const char       *str);
static void geda_label_set_pattern_internal       (GedaLabel  *label,
                                             const char       *pattern,
                                                   bool        is_mnemonic);
static void geda_label_set_markup_internal        (GedaLabel  *label,
                                             const char       *str,
                                                   bool        with_uline);
static void geda_label_recalculate                (GedaLabel  *label);
static void geda_label_screen_changed             (GtkWidget  *widget,
                                                   GdkScreen  *old_screen);
static bool geda_label_popup_menu                 (GtkWidget  *widget);
static void geda_label_create_window              (GedaLabel  *label);
static void geda_label_destroy_window             (GedaLabel  *label);
static bool geda_label_ensure_select_info         (GedaLabel  *label);
static void geda_label_clear_select_info          (GedaLabel  *label);
static void geda_label_update_cursor              (GedaLabel  *label);
static void geda_label_clear_layout               (GedaLabel  *label);
static void geda_label_ensure_layout              (GedaLabel  *label);
static void geda_label_select_region_index        (GedaLabel  *label,
                                                   int         anchor_index,
                                                   int         end_index);

static bool geda_label_mnemonic_activate          (GtkWidget         *widget,
                                                   bool               group_cycling);
static void geda_label_setup_mnemonic             (GedaLabel          *label,
                                                   unsigned int       last_key);
static void geda_label_drag_data_get              (GtkWidget         *widget,
                                                   GdkDragContext    *context,
                                                   GtkSelectionData  *selection_data,
                                                   unsigned int       info,
                                                   unsigned int       time);

static void geda_label_buildable_interface_init   (GtkBuildableIface *iface);
static bool geda_label_buildable_custom_tag_start (GtkBuildable      *buildable,
                                                   GtkBuilder        *builder,
                                                   GObject           *child,
                                                   const char        *tagname,
                                                   GMarkupParser     *parser,
                                                   void             **data);

static void geda_label_buildable_custom_finished  (GtkBuildable      *buildable,
                                                   GtkBuilder        *builder,
                                                   GObject           *child,
                                                   const char        *tagname,
                                                   void              *user_data);

static void connect_mnemonics_visible_notify      (GedaLabel         *label);
static bool separate_uline_pattern                (const char        *str,
                                                   unsigned int      *accel_key,
                                                   char             **new_str,
                                                   char             **pattern);


/* For selectable labels: */
static void geda_label_move_cursor                (GedaLabel        *label,
                                                   GtkMovementStep  step,
                                                   int              count,
                                                   bool             extend_selection);
static void geda_label_copy_clipboard             (GedaLabel        *label);
static void geda_label_select_all                 (GedaLabel        *label);
static void geda_label_do_popup                   (GedaLabel        *label,
                                                   GdkEventButton  *event);
static int geda_label_move_forward_word           (GedaLabel        *label,
                                                   int              start);
static int geda_label_move_backward_word          (GedaLabel        *label,
                                                   int              start);

/* For links: */
static void geda_label_clear_links                (GedaLabel     *label);
static bool geda_label_activate_link              (GedaLabel     *label,
                                                   const char    *uri);

static void geda_label_activate_current_link      (GedaLabel     *label);
static GedaLabelLink *geda_label_get_current_link (GedaLabel     *label);

static void geda_label_get_link_colors            (GtkWidget     *widget,
                                                   GdkColor      *link_color,
                                                   GdkColor      *visited_link_color);

static void emit_activate_link                    (GedaLabel     *label,
                                                   GedaLabelLink *link);

static GtkBuildableIface *buildable_parent_iface = NULL;

static GQuark quark_aux_info = 0;

G_DEFINE_TYPE_WITH_CODE (GedaLabel, geda_label, GTK_TYPE_MISC,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_BUILDABLE,
                                                geda_label_buildable_interface_init))

static void
add_move_binding (GtkBindingSet  *binding_set, unsigned int keyval,
                  unsigned int    modmask,     GtkMovementStep step,
                  int    count)
{
  g_return_if_fail ((modmask & GDK_SHIFT_MASK) == 0);

  gtk_binding_entry_add_signal (binding_set, keyval, modmask,
    "move-cursor", 3,
    G_TYPE_ENUM, step,
    G_TYPE_INT, count,
    G_TYPE_BOOLEAN, FALSE);

  /* Selection-extending version */
  gtk_binding_entry_add_signal (binding_set, keyval, modmask | GDK_SHIFT_MASK,
    "move-cursor", 3,
    G_TYPE_ENUM, step,
    G_TYPE_INT, count,
    G_TYPE_BOOLEAN, TRUE);
}

/*---------------------------- Override Support Function -----------------*/

/* called by: geda_label_size_request */
static int get_label_char_width (GedaLabel *label)
{
  PangoContext *context;

  PangoFontMetrics *metrics;
  int char_width, digit_width, char_pixels, w;

  context = pango_layout_get_context (label->layout);

  metrics = pango_context_get_metrics (context, GTK_WIDGET (label)->style->font_desc, pango_context_get_language (context));

  char_width =  pango_font_metrics_get_approximate_char_width (metrics);
  digit_width = pango_font_metrics_get_approximate_digit_width (metrics);
  char_pixels = MAX (char_width, digit_width);
  pango_font_metrics_unref (metrics);

  if (label->width_chars < 0) {

    PangoRectangle rect;

    pango_layout_set_width (label->layout, -1);
    pango_layout_get_extents (label->layout, NULL, &rect);

    w = char_pixels * MAX (label->max_width_chars, 3);
    w = MIN (rect.width, w);
  }
  else {

    /* enforce minimum width for ellipsized labels at ~3 chars */
    w = char_pixels * MAX (label->width_chars, 3);
  }

  return w;
}

/* called by: geda_label_size_request
 *            geda_label_ensure_layout
 */
static inline GtkWidgetAuxInfo*
geda_widget_get_aux_info (GtkWidget *widget, bool create)
{
  GtkWidgetAuxInfo *aux_info;

  aux_info = g_object_get_qdata (G_OBJECT (widget), quark_aux_info);

  if (!aux_info && create) {

    aux_info = malloc (sizeof(GtkWidgetAuxInfo));

    aux_info->width  = -1;
    aux_info->height = -1;
    aux_info->x      = 0;
    aux_info->y      = 0;
    aux_info->x_set  = FALSE;
    aux_info->y_set  = FALSE;

    g_object_set_qdata (G_OBJECT (widget), quark_aux_info, aux_info);
  }

  return aux_info;
}

static void
geda_label_style_set (GtkWidget *widget,  GtkStyle  *previous_style)
{
  GedaLabel *label = GEDA_LABEL (widget);

  /* We have to clear the layout, fonts etc. may have changed */
  geda_label_clear_layout (label);
  label->INVALIDATE_WRAP_WIDTH;
}

static void
geda_label_direction_changed (GtkWidget  *widget, GtkTextDirection direction)
{
  GedaLabel *label = GEDA_LABEL (widget);

  if (label->layout)
    pango_layout_context_changed (label->layout);

  GTK_WIDGET_CLASS (geda_label_parent_class)->direction_changed (widget, direction);
}

/* Semi-private function used by gtk widgets inheriting from
 * GtkMisc that takes into account both css padding and border
 * and the padding specified with the GtkMisc properties.
 */
void
geda_misc_get_padding_and_border (GtkMisc *misc, GtkBorder *border)
{
  int xpad, ypad;
  float xalign;
  float yalign;

  g_return_if_fail (GTK_IS_MISC (misc));

  gtk_misc_get_padding (misc, &xpad, &ypad);
  border->top += ypad;
  border->left += xpad;
  border->bottom += ypad;
  border->right += xpad;

  gtk_misc_get_alignment (misc, &xalign, &yalign);

  xalign = xalign / 2;
  yalign = yalign / 2;

  border->top    += yalign;
  border->right  += xalign;
  border->bottom += yalign;
  border->left   += xalign;
}

static void
geda_label_update_layout_width (GedaLabel *label)
{
  GedaLabelPrivate *priv;
  GtkWidget        *widget;

  widget = GTK_WIDGET (label);
  priv   = label->priv;

  if (priv == NULL) {
    BUG_MSG ("NULL pointer");
    return;
  }

  if (label->layout == NULL) {
    BUG_MSG ("NULL pointer for layout");
    return;
  }

  if (priv->ellipsize || priv->wrap) {

    GtkAllocation *allocation;
    GtkBorder      border;
    PangoRectangle logical;
    int width, height;

    geda_misc_get_padding_and_border (GTK_MISC (label), &border);

    allocation = geda_get_widget_allocation (widget);

    width  = allocation->width - border.left - border.right;
    height = allocation->height - border.top  - border.bottom;

    if (priv->have_transform) {
      PangoContext *context = gtk_widget_get_pango_context (widget);
      const PangoMatrix *matrix = pango_context_get_matrix (context);
      const double dx = matrix->xx; /* cos (M_PI * angle / 180) */
      const double dy = matrix->xy; /* sin (M_PI * angle / 180) */

      pango_layout_set_width (label->layout, -1);
      pango_layout_get_pixel_extents (label->layout, NULL, &logical);

      if (fabs (dy) < 0.01) {
        if (logical.width > width)
          pango_layout_set_width (label->layout, width * PANGO_SCALE);
      }
      else if (fabs (dx) < 0.01) {
        if (logical.width > height)
          pango_layout_set_width (label->layout, height * PANGO_SCALE);
      }
      else {
        double x0, y0, x1, y1, length;
        bool vertical;
        int cy;

        x0 = width / 2;
        y0 = dx ? x0 * dy / dx : G_MAXDOUBLE;
        vertical = fabs (y0) > height / 2;

        if (vertical)
        {
          y0 = height/2;
          x0 = dy ? y0 * dx / dy : G_MAXDOUBLE;
        }

#if HAVE_HYPOT
        length = 2 * hypot (x0, y0);
#else
        length = 2 * sqrt (x0 * x0 + y0 * y0);
#endif

        pango_layout_set_width (label->layout, rint (length * PANGO_SCALE));
        pango_layout_get_pixel_size (label->layout, NULL, &cy);

        x1 = +dy * cy/2;
        y1 = -dx * cy/2;

        if (vertical) {
          y0 = height/2 + y1 - y0;
          x0 = -y0 * dx/dy;
        }
        else {
          x0 = width/2 + x1 - x0;
          y0 = -x0 * dy/dx;
        }

#if HAVE_HYPOT
        length = length - hypot (x0, y0) * 2;
#else
        length = length - sqrt (x0 * x0 + y0 * y0) * 2;
#endif
        pango_layout_set_width (label->layout, rint (length * PANGO_SCALE));
      }
    }
    else {
      pango_layout_set_width (label->layout, width * PANGO_SCALE);
    }
  }
  else {
    pango_layout_set_width (label->layout, -1);
  }
}

static bool
pango_attribute_merge_filter (PangoAttribute *attribute, void * list)
{
  pango_attr_list_change (list, pango_attribute_copy (attribute));
  return FALSE;
}
/* called by: geda_label_ensure_layout (to merge attributes using above
 *            filter function, there has got to be a better way.)
 */
static void
pango_merge_attribute_list (PangoAttrList *into, PangoAttrList *from)
{
  pango_attr_list_filter (from, pango_attribute_merge_filter, into);
}

/* called by: geda_label_size_request
 *            geda_label_expose
 *
 *            geda_label_get_layout_offsets
 *            geda_label_get_layout
 *            get_cursor_direction
 *            geda_label_draw_cursor
 *            get_layout_index
 *            get_better_cursor
 *            geda_label_move_logically
 *            geda_label_move_visually
 *            geda_label_move_forward_word
 *            geda_label_move_backward_word
 */
static void geda_label_ensure_layout (GedaLabel *label)
{
  GedaLabelPrivate *priv;
  GtkWidget        *widget;
  bool R2L;

  priv = label->priv;
  widget = GTK_WIDGET (label);

  R2L = gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL;

  if (!label->layout) {
    PangoAlignment alignment; /* pango default this to PANGO_ALIGN_LEFT */
    PangoAttrList *attrs;
    PangoContext  *context;

    context = g_object_new (PANGO_TYPE_CONTEXT, NULL);

    /* We Specify our own map to avoid memory leak in FontConfig */
    pango_context_set_font_map (context, label->priv->font_map);

    pango_context_set_font_description (context, widget->style->font_desc);

    if (label->angle != 0.0 && !priv->select_info) {

     /* We rotate the standard singleton PangoContext for the widget, depending
      * on the fact that it's meant pretty much exclusively for our use. */

      PangoMatrix matrix = PANGO_MATRIX_INIT;

      pango_matrix_rotate (&matrix, label->angle);

      pango_context_set_matrix ( context, &matrix);

      priv->have_transform = TRUE;
    }
    else {
      if (priv->have_transform)
        pango_context_set_matrix (context, NULL);

      priv->have_transform = FALSE;
    }

    label->layout = pango_layout_new (context);
    g_object_unref (context);

    if ( label->text )
      pango_layout_set_text (label->layout, label->text, -1);

    if (priv->select_info && priv->select_info->links) {
      GdkColor link_color, visited_color;
      PangoAttribute *attribute;
      GList *list;

      geda_label_get_link_colors (widget, &link_color, &visited_color);
      attrs = pango_attr_list_new ();

      for (list = priv->select_info->links; list; list = list->next) {
        GedaLabelLink *link = list->data;

        attribute = pango_attr_underline_new (TRUE);
        attribute->start_index = link->start;
        attribute->end_index = link->end;
        pango_attr_list_insert (attrs, attribute);

        if (link->visited)
          attribute = pango_attr_foreground_new (visited_color.red,
                                                 visited_color.green,
                                                 visited_color.blue);
          else
            attribute = pango_attr_foreground_new (link_color.red,
                                                   link_color.green,
                                                   link_color.blue);
            attribute->start_index = link->start;
          attribute->end_index = link->end;
        pango_attr_list_insert (attrs, attribute);
      }
    }
    else if (label->markup_attrs || label->attrs)
      attrs = pango_attr_list_new ();
    else
      attrs = NULL;

    if (label->markup_attrs) {
      if (attrs)
        pango_merge_attribute_list (attrs, label->markup_attrs);
      else
        attrs = pango_attr_list_ref (label->markup_attrs);
    }

    if (label->attrs) {
      if (attrs)
        pango_merge_attribute_list (attrs, label->attrs);
      else
        attrs = pango_attr_list_ref (label->attrs);
    }

    if (attrs) {
      pango_layout_set_attributes (label->layout, attrs);
      pango_attr_list_unref (attrs);
    }

    switch (priv->jtype) {
      case GTK_JUSTIFY_LEFT:
        alignment = R2L ? PANGO_ALIGN_RIGHT : PANGO_ALIGN_LEFT;
        break;
      case GTK_JUSTIFY_RIGHT:
        alignment = R2L ? PANGO_ALIGN_LEFT : PANGO_ALIGN_RIGHT;
        break;
      case GTK_JUSTIFY_CENTER:
        alignment = PANGO_ALIGN_CENTER;
        break;
      case GTK_JUSTIFY_FILL:
        alignment = R2L ? PANGO_ALIGN_RIGHT : PANGO_ALIGN_LEFT;
        pango_layout_set_justify (label->layout, TRUE);
        break;
      default:
        alignment = PANGO_ALIGN_LEFT;
    }

    pango_layout_set_alignment (label->layout, alignment);
    pango_layout_set_ellipsize (label->layout, priv->ellipsize);
    pango_layout_set_wrap (label->layout, priv->wrap_mode);
    pango_layout_set_single_paragraph_mode (label->layout, priv->single_line_mode);

    geda_label_update_layout_width (label);

  }
}

/*! \brief GedaLabel Class Initializer
 *
 *  \par Function Description
 *  Function is called to initialize the class instance.
 *
 * \param [in] class A GedaLabelClass Object
 */
static void
geda_label_class_init (GedaLabelClass *class)
{
  GParamSpec     *params;
  GObjectClass   *gobject_class;
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkBindingSet  *binding_set;

  gobject_class  = G_OBJECT_CLASS (class);
  object_class   = GTK_OBJECT_CLASS (class);
  widget_class   = GTK_WIDGET_CLASS (class);

  gobject_class->set_property        = geda_label_set_property;
  gobject_class->get_property        = geda_label_get_property;
  gobject_class->finalize            = geda_label_finalize;

   object_class->destroy             = geda_label_destroy;

  widget_class->size_request         = geda_label_size_request;
  widget_class->size_allocate        = geda_label_size_allocate;
  widget_class->state_changed        = geda_label_state_changed;
  widget_class->style_set            = geda_label_style_set;
  widget_class->query_tooltip        = geda_label_query_tooltip;
  widget_class->direction_changed    = geda_label_direction_changed;
  widget_class->expose_event         = geda_label_expose;
  widget_class->realize              = geda_label_realize;
  widget_class->unrealize            = geda_label_unrealize;
  widget_class->map                  = geda_label_map;
  widget_class->unmap                = geda_label_unmap;
  widget_class->button_press_event   = geda_label_button_press;
  widget_class->button_release_event = geda_label_button_release;
  widget_class->motion_notify_event  = geda_label_motion;
  widget_class->leave_notify_event   = geda_label_leave_notify;
  widget_class->screen_changed       = geda_label_screen_changed;
  widget_class->mnemonic_activate    = geda_label_mnemonic_activate;
  widget_class->drag_data_get        = geda_label_drag_data_get;
  widget_class->grab_focus           = geda_label_grab_focus;
  widget_class->popup_menu           = geda_label_popup_menu;
  widget_class->focus                = geda_label_focus;

  class->move_cursor                 = geda_label_move_cursor;
  class->copy_clipboard              = geda_label_copy_clipboard;
  class->activate_link               = geda_label_activate_link;

  /**
   * GedaLabel::move-cursor:
   * entry: the object which received the signal
   * step: the granularity of the move, as a GtkMovementStep
   * count: the number of step units to move
   * extend_selection: %TRUE if the move should extend the selection
   *
   * The move-cursor signal is a keybinding signal, which gets emitted
   * when the user initiates a cursor movement. If the cursor is not visible
   * in entry, this signal causes the viewport to be moved instead.
   *
   * Applications should not connect to it, but may emit it with
   * g_signal_emit_by_name() if they need to control the cursor
   * programmatically.
   *
   * The default bindings for this signal come in two variants,
   * the variant with the Shift modifier extends the selection,
   * the variant without the Shift modifer does not.
   * There are too many key combinations to list them all here.
   *
   *   -Arrow keys move by individual characters/lines
   *   -Ctrl-arrow key combinations move by words/paragraphs
   *   -Home/End keys move to the ends of the buffer
   *
   */
/*
  signals[MOVE_CURSOR] =
  g_signal_new ( _("move-cursor"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                   G_STRUCT_OFFSET (GedaLabelClass, move_cursor),
                   NULL, NULL,
                   gtk_marshal_VOID__ENUM_INT_BOOLEAN,
                   G_TYPE_NONE, 3,
                   GTK_TYPE_MOVEMENT_STEP,
                   G_TYPE_INT,
                   G_TYPE_BOOLEAN);
*/
  signals[MOVE_CURSOR] =
  g_signal_new ( _("move-cursor"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                   G_STRUCT_OFFSET (GedaLabelClass, move_cursor),
                   NULL, NULL,
                   NULL,
                   G_TYPE_NONE, 3,
                   GTK_TYPE_MOVEMENT_STEP,
                   G_TYPE_INT,
                   G_TYPE_BOOLEAN);
  /**
   * GedaLabel::copy-clipboard:
   * label: the object which received the signal
   *
   * The copy-clipboard signal is a keybinding signal, which gets
   * emitted to copy the selection to the clipboard.
   *
   * The default binding for this signal is Ctrl-c.
   */
  signals[COPY_CLIPBOARD] =
  g_signal_new ( _("copy-clipboard"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                   G_STRUCT_OFFSET (GedaLabelClass, copy_clipboard),
                   NULL, NULL,
                   gtk_marshal_VOID__VOID,
                   G_TYPE_NONE, 0);

  /**
   * GedaLabel::populate-popup:
   * label: The label on which the signal is emitted
   * menu: the menu that is being populated
   *
   * The populate-popup signal gets emitted before showing the
   * context menu of the label. Note that only selectable labels
   * have context menus.
   *
   * If you need to add items to the context menu, connect
   * to this signal and append your menuitems to the menu.
   */
  signals[POPULATE_POPUP] =
  g_signal_new (_("populate-popup"),
                   G_OBJECT_CLASS_TYPE (gobject_class),
                   G_SIGNAL_RUN_LAST,
                   G_STRUCT_OFFSET (GedaLabelClass, populate_popup),
                   NULL, NULL,
                   gtk_marshal_VOID__OBJECT,
                   G_TYPE_NONE, 1,
                   GTK_TYPE_MENU);

  /**
   * GedaLabel::activate-current-link:
   * label: The label on which the signal was emitted
   *
   * A keybinding signal, which gets emitted when the user activates a
   * link in the label.
   *
   * Applications may also emit the signal with g_signal_emit_by_name()
   * if they need to control activation of URIs programmatically.
   *
   * The default bindings for this signal are all forms of the Enter key.
   *
   */
  signals[ACTIVATE_CURRENT_LINK] =
  g_signal_new_class_handler ("activate-current-link",
                              G_TYPE_FROM_CLASS (gobject_class),
                              G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                              G_CALLBACK (geda_label_activate_current_link),
                              NULL, NULL,
                              gtk_marshal_VOID__VOID,
                              G_TYPE_NONE, 0);

  /**
   * GedaLabel::activate-link:
   * label: The label on which the signal was emitted
   * uri: the URI that is activated
   *
   * The signal which gets emitted to activate a URI.
   * Applications may connect to it to override the default behaviour,
   * which is to call gtk_show_uri().
   *
   * Returns: %TRUE if the link has been activated
   *
   */
/*
  signals[ACTIVATE_LINK] =
  g_signal_new ("activate-link",
                G_TYPE_FROM_CLASS (gobject_class),
                G_SIGNAL_RUN_LAST,
                G_STRUCT_OFFSET (GedaLabelClass, activate_link),
                _gtk_boolean_handled_accumulator, NULL,
                gtk_marshal_BOOLEAN__STRING,
                G_TYPE_BOOLEAN, 1, G_TYPE_STRING);
*/
  signals[ACTIVATE_LINK] =
  g_signal_new ("activate-link",
                G_TYPE_FROM_CLASS (gobject_class),
                G_SIGNAL_RUN_LAST,
                G_STRUCT_OFFSET (GedaLabelClass, activate_link),
                NULL, NULL,
                gtk_marshal_BOOLEAN__POINTER,
                G_TYPE_BOOLEAN, 1, G_TYPE_STRING);

  params = g_param_spec_string ("label", _("Label"),
                              _("The text of the label"),
                                "",
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_LABEL, params);

  params =  g_param_spec_boxed ("attributes", _("Attributes"),
                              _("A list of style attributes to apply to the text of the label"),
                                 PANGO_TYPE_ATTR_LIST,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ATTRIBUTES, params);

  params = g_param_spec_boolean ("use-markup", _("Use markup"),
                               _("The text of the label includes XML markup. See pango_parse_markup()"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_USE_MARKUP, params);

  params = g_param_spec_boolean ("use-underline", _("Use underline"),
                               _("If set, underline in text indicates the next character is mnemonic for accelerator key"),
                                  FALSE,
                                  G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class, PROP_USE_UNDERLINE, params);

  params = g_param_spec_enum ("justify", _("Justification"),
                               _("The alignment of the lines in the text of the label relative to each other. This does NOT affect the alignment of the label within its allocation. See GtkMisc::xalign for that"),
                                                      GTK_TYPE_JUSTIFICATION,
                                                      GTK_JUSTIFY_LEFT,
                                                      G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_JUSTIFY, params );

  params = g_param_spec_string ("pattern", _("Pattern"),
                                _("A string with _ characters in positions correspond to characters in the text to underline"),
                                                        NULL,
                                                        (G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class, PROP_PATTERN, params);

  params = g_param_spec_boolean ("wrap", _("Line wrap"),
                               _("If set, wrap lines if the text becomes too wide"),
                                   FALSE,
                                   G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_WRAP, params);

  /**
   * GedaLabel:wrap-mode:
   *
   * If line wrapping is on (see the #GedaLabel:wrap property) this controls
   * how the line wrapping is done. The default is %PANGO_WRAP_WORD, which
   * means wrap on word boundaries.
   *
   */
  params = g_param_spec_enum ("wrap-mode", _("Line wrap mode"),
                               _("If wrap is set, controls how linewrapping is done"),
                                                       PANGO_TYPE_WRAP_MODE,
                                                       PANGO_WRAP_WORD,
                                                       G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_WRAP_MODE, params);

  params = g_param_spec_boolean ("selectable", _("Selectable"),
                               _("Whether the label text can be selected with the mouse"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_SELECTABLE, params);

  params = g_param_spec_uint ("mnemonic-keyval", _("Mnemonic key"),
                            _("The mnemonic accelerator key for this label"),
                               0,
                               G_MAXUINT,
                               GDK_KEY_VoidSymbol,
                              (G_PARAM_READABLE));

  g_object_class_install_property (gobject_class, PROP_MNEMONIC_KEYVAL, params);

  params = g_param_spec_object ("mnemonic-widget", _("Mnemonic widget"),
                              _("The widget to be activated when the label's mnemonic "
                                "key is pressed"),
                                 GTK_TYPE_WIDGET,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_MNEMONIC_WIDGET, params);

  params = g_param_spec_int ("cursor-position", _("Cursor Position"),
                           _("The current position of the insertion cursor in chars"),
                              0,
                              G_MAXINT,
                              0,
                             (G_PARAM_READABLE));

  g_object_class_install_property (gobject_class, PROP_CURSOR_POSITION, params);

  params = g_param_spec_int ("selection-bound", _("Selection Bound"),
                           _("The position of the opposite end of the selection from the cursor in chars"),
                              0,
                              G_MAXINT,
                              0,
                             (G_PARAM_READABLE));

  g_object_class_install_property (gobject_class, PROP_SEL_BOUND, params);

  /**
   * GedaLabel:ellipsize:
   *
   * The preferred place to ellipsize the string, if the label does
   * not have enough room to display the entire string, specified as a
   * PangoEllipsizeMode.
   *
   * Note that setting this property to a value other than
   * %PANGO_ELLIPSIZE_NONE has the side-effect that the label requests
   * only enough space to display the ellipsis "...". In particular, this
   * means that ellipsizing labels do not work well in notebook tabs, unless
   * the tab's GtkNotebook:tab-expand property is set to %TRUE. Other ways
   * to set a label's width are gtk_widget_set_size_request() and
   * geda_label_set_width_chars().
   *
   */
  params = g_param_spec_enum ("ellipsize", _("Ellipsize"),
                            _("The preferred place to break the string, if ellipes needed"),
                               PANGO_TYPE_ELLIPSIZE_MODE,
                               PANGO_ELLIPSIZE_NONE,
                               G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ELLIPSIZE, params);

  /**
   * GedaLabel:width-chars:
   *
   * The desired width of the label, in characters. If this property is set to
   * -1, the width will be calculated automatically.
   *
   * See the section on "text layout" for details of how #GedaLabel:width-chars
   * and #GedaLabel:max-width-chars determine the width of ellipsized and
   * wrapped labels.
   *
   */
  params = g_param_spec_int ("width-chars", _("Width In Characters"),
                           _("The desired width of the label, in characters"),
                             -1,
                              G_MAXINT,
                             -1,
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_WIDTH_CHARS, params);

  /**
   * GedaLabel:single-line-mode:
   *
   * Whether the label is in single line mode. In single line mode,
   * the height of the label does not depend on the actual text, it
   * is always set to ascent + descent of the font. This can be an
   * advantage in situations where resizing the label because of text
   * changes would be distracting, e.g. in a statusbar.
   *
   */
  params = g_param_spec_boolean ("single-line-mode", _("Single Line Mode"),
                               _("Whether the label is in single line mode"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_SINGLE_LINE_MODE, params);

  /**
   * GedaLabel:angle:
   *
   * The angle that the baseline of the label makes with the horizontal,
   * in degrees, measured counterclockwise. An angle of 90 reads from
   * from bottom to top, an angle of 270, from top to bottom. Ignored
   * if the label is selectable, wrapped, or ellipsized.
   *
   */
  params = g_param_spec_double ("angle", _("Angle"),
                              _("Angle at which the label is rotated"),
                                 0.0,
                                 360.0,
                                 0.0,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ANGLE, params);

  /**
   * GedaLabel:max-width-chars:
   *
   * The desired maximum width of the label, in characters. If this property
   * is set to -1, the width will be calculated automatically.
   *
   * See the section on "text layout" for details of how #GedaLabel:width-chars
   * and #GedaLabel:max-width-chars determine the width of ellipsized and
   * wrapped labels.
   *
   */
  params = g_param_spec_int ("max-width-chars", _("Maximum Width In Characters"),
                           _("The desired maximum width of the label, in characters"),
                             -1,
                              G_MAXINT,
                             -1,
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_MAX_WIDTH, params);

  /**
   * GedaLabel:track-visited-links:
   *
   * Set this property to %TRUE to make the label track which links
   * have been clicked. It will then apply the visited-link-color
   * color, instead of link-color.
   *
   */
  params = g_param_spec_boolean ("track-visited-links", _("Track visited links"),
                               _("Whether visited links should be tracked"),
                                  TRUE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_TRACK_VISITED_LINKS, params);


  /*  Key bindings */

  binding_set = gtk_binding_set_by_class (class);

  /* Moving the insertion point */
  add_move_binding (binding_set, GDK_KEY_Right, 0, GTK_MOVEMENT_VISUAL_POSITIONS, 1);

  add_move_binding (binding_set, GDK_KEY_Left, 0, GTK_MOVEMENT_VISUAL_POSITIONS, -1);

  add_move_binding (binding_set, GDK_KEY_KP_Right, 0, GTK_MOVEMENT_VISUAL_POSITIONS, 1);

  add_move_binding (binding_set, GDK_KEY_KP_Left, 0, GTK_MOVEMENT_VISUAL_POSITIONS, -1);

  add_move_binding (binding_set, GDK_KEY_f, GDK_CONTROL_MASK, GTK_MOVEMENT_LOGICAL_POSITIONS, 1);

  add_move_binding (binding_set, GDK_KEY_b, GDK_CONTROL_MASK, GTK_MOVEMENT_LOGICAL_POSITIONS, -1);

  add_move_binding (binding_set, GDK_KEY_Right, GDK_CONTROL_MASK, GTK_MOVEMENT_WORDS, 1);

  add_move_binding (binding_set, GDK_KEY_Left, GDK_CONTROL_MASK, GTK_MOVEMENT_WORDS, -1);

  add_move_binding (binding_set, GDK_KEY_KP_Right, GDK_CONTROL_MASK, GTK_MOVEMENT_WORDS, 1);

  add_move_binding (binding_set, GDK_KEY_KP_Left, GDK_CONTROL_MASK, GTK_MOVEMENT_WORDS, -1);

  /* select all */
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_a, GDK_CONTROL_MASK,
                                "move-cursor", 3,
                                G_TYPE_ENUM, GTK_MOVEMENT_PARAGRAPH_ENDS,
                                G_TYPE_INT, -1,
                                G_TYPE_BOOLEAN, FALSE);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_a, GDK_CONTROL_MASK,
                                "move-cursor", 3,
                                G_TYPE_ENUM, GTK_MOVEMENT_PARAGRAPH_ENDS,
                                G_TYPE_INT, 1,
                                G_TYPE_BOOLEAN, TRUE);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_slash, GDK_CONTROL_MASK,
                                "move-cursor", 3,
                                G_TYPE_ENUM, GTK_MOVEMENT_PARAGRAPH_ENDS,
                                G_TYPE_INT, -1,
                                G_TYPE_BOOLEAN, FALSE);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_slash, GDK_CONTROL_MASK,
                                "move-cursor", 3,
                                G_TYPE_ENUM, GTK_MOVEMENT_PARAGRAPH_ENDS,
                                G_TYPE_INT, 1,
                                G_TYPE_BOOLEAN, TRUE);

  /* unselect all */
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_a, GDK_SHIFT_MASK | GDK_CONTROL_MASK,
                                "move-cursor", 3,
                                G_TYPE_ENUM, GTK_MOVEMENT_PARAGRAPH_ENDS,
                                G_TYPE_INT, 0,
                                G_TYPE_BOOLEAN, FALSE);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_backslash, GDK_CONTROL_MASK,
                                "move-cursor", 3,
                                G_TYPE_ENUM, GTK_MOVEMENT_PARAGRAPH_ENDS,
                                G_TYPE_INT, 0,
                                G_TYPE_BOOLEAN, FALSE);

  add_move_binding (binding_set, GDK_KEY_f, GDK_MOD1_MASK, GTK_MOVEMENT_WORDS, 1);

  add_move_binding (binding_set, GDK_KEY_b, GDK_MOD1_MASK, GTK_MOVEMENT_WORDS, -1);

  add_move_binding (binding_set, GDK_KEY_Home, 0, GTK_MOVEMENT_DISPLAY_LINE_ENDS, -1);

  add_move_binding (binding_set, GDK_KEY_End, 0, GTK_MOVEMENT_DISPLAY_LINE_ENDS, 1);

  add_move_binding (binding_set, GDK_KEY_KP_Home, 0, GTK_MOVEMENT_DISPLAY_LINE_ENDS, -1);

  add_move_binding (binding_set, GDK_KEY_KP_End, 0, GTK_MOVEMENT_DISPLAY_LINE_ENDS, 1);

  add_move_binding (binding_set, GDK_KEY_Home, GDK_CONTROL_MASK, GTK_MOVEMENT_BUFFER_ENDS, -1);

  add_move_binding (binding_set, GDK_KEY_End, GDK_CONTROL_MASK, GTK_MOVEMENT_BUFFER_ENDS, 1);

  add_move_binding (binding_set, GDK_KEY_KP_Home, GDK_CONTROL_MASK, GTK_MOVEMENT_BUFFER_ENDS, -1);

  add_move_binding (binding_set, GDK_KEY_KP_End, GDK_CONTROL_MASK, GTK_MOVEMENT_BUFFER_ENDS, 1);

  /* copy */
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_c, GDK_CONTROL_MASK,
                                "copy-clipboard", 0);

  gtk_binding_entry_add_signal (binding_set, GDK_KEY_Return, 0,
                                "activate-current-link", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_ISO_Enter, 0,
                                "activate-current-link", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_KP_Enter, 0,
                                "activate-current-link", 0);

  g_type_class_add_private (class, sizeof (GedaLabelPrivate));

}

static void
geda_label_set_property (GObject *object,     unsigned int  prop_id,
                         const GValue *value, GParamSpec   *pspec)
{
  GedaLabel *label = GEDA_LABEL (object);

  switch (prop_id)
  {
    case PROP_LABEL:
      geda_label_set_label (label, g_value_get_string (value));
      break;
    case PROP_ATTRIBUTES:
      geda_label_set_attributes (label, g_value_get_boxed (value));
      break;
    case PROP_USE_MARKUP:
      geda_label_set_use_markup (label, g_value_get_boolean (value));
      break;
    case PROP_USE_UNDERLINE:
      geda_label_set_use_underline (label, g_value_get_boolean (value));
      break;
    case PROP_JUSTIFY:
      geda_label_set_justify (label, g_value_get_enum (value));
      break;
    case PROP_PATTERN:
      geda_label_set_pattern (label, g_value_get_string (value));
      break;
    case PROP_WRAP:
      geda_label_set_line_wrap (label, g_value_get_boolean (value));
      break;
    case PROP_WRAP_MODE:
      geda_label_set_line_wrap_mode (label, g_value_get_enum (value));
      break;
    case PROP_SELECTABLE:
      geda_label_set_selectable (label, g_value_get_boolean (value));
      break;
    case PROP_MNEMONIC_WIDGET:
      geda_label_set_mnemonic_widget (label, (GtkWidget*) g_value_get_object (value));
      break;
    case PROP_ELLIPSIZE:
      geda_label_set_ellipsize (label, g_value_get_enum (value));
      break;
    case PROP_WIDTH_CHARS:
      geda_label_set_width_chars (label, g_value_get_int (value));
      break;
    case PROP_SINGLE_LINE_MODE:
      geda_label_set_single_line_mode (label, g_value_get_boolean (value));
      break;
    case PROP_ANGLE:
      geda_label_set_angle (label, g_value_get_double (value));
      break;
    case PROP_MAX_WIDTH:
      geda_label_set_max_width_chars (label, g_value_get_int (value));
      break;
    case PROP_TRACK_VISITED_LINKS:
      geda_label_set_track_visited_links (label, g_value_get_boolean (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_label_get_property (GObject *object, unsigned int  prop_id,
                         GValue  *value,  GParamSpec   *pspec)
{
  GedaLabel *label       = GEDA_LABEL (object);
  GedaLabelPrivate *priv = label->priv;

  switch (prop_id)
    {
    case PROP_LABEL:
      g_value_set_string (value, label->label);
      break;
    case PROP_ATTRIBUTES:
      g_value_set_boxed (value, label->attrs);
      break;
    case PROP_USE_MARKUP:
      g_value_set_boolean (value, priv->use_markup);
      break;
    case PROP_USE_UNDERLINE:
      g_value_set_boolean (value, priv->use_underline);
      break;
    case PROP_JUSTIFY:
      g_value_set_enum (value, priv->jtype);
      break;
    case PROP_WRAP:
      g_value_set_boolean (value, priv->wrap);
      break;
    case PROP_WRAP_MODE:
      g_value_set_enum (value, priv->wrap_mode);
      break;
    case PROP_SELECTABLE:
      g_value_set_boolean (value, geda_label_get_selectable (label));
      break;
    case PROP_MNEMONIC_KEYVAL:
      g_value_set_uint (value, priv->mnemonic_keyval);
      break;
    case PROP_MNEMONIC_WIDGET:
      g_value_set_object (value, (GObject*) priv->mnemonic_widget);
      break;
    case PROP_CURSOR_POSITION:
      g_value_set_int (value, _geda_label_get_cursor_position (label));
      break;
    case PROP_SEL_BOUND:
      g_value_set_int (value, _geda_label_get_selection_bound (label));
      break;
    case PROP_ELLIPSIZE:
      g_value_set_enum (value, priv->ellipsize);
      break;
    case PROP_WIDTH_CHARS:
      g_value_set_int (value, geda_label_get_width_chars (label));
      break;
    case PROP_SINGLE_LINE_MODE:
      g_value_set_boolean (value, geda_label_get_single_line_mode (label));
      break;
    case PROP_ANGLE:
      g_value_set_double (value, geda_label_get_angle (label));
      break;
    case PROP_MAX_WIDTH:
      g_value_set_int (value, geda_label_get_max_width_chars (label));
      break;
    case PROP_TRACK_VISITED_LINKS:
      g_value_set_boolean (value, geda_label_get_track_visited_links (label));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
geda_label_init (GedaLabel *label)
{
  GedaLabelPrivate *priv;

  label->priv = G_TYPE_INSTANCE_GET_PRIVATE (label,
                                             GEDA_TYPE_LABEL,
                                             GedaLabelPrivate);
  priv = label->priv;

  gtk_widget_set_has_window    (GTK_WIDGET (label), FALSE);
  gtk_widget_set_app_paintable (GTK_WIDGET (label), TRUE);
  gtk_widget_set_can_default   (GTK_WIDGET (label), FALSE);

  priv->font_map  = pango_cairo_font_map_new ();

  label->width_chars      = -1;
  label->max_width_chars  = -1;
  label->label            = NULL;

  priv->jtype             = GTK_JUSTIFY_LEFT;
  priv->wrap              = FALSE;
  priv->wrap_mode         = PANGO_WRAP_WORD;
  priv->ellipsize         = PANGO_ELLIPSIZE_NONE;

  priv->use_underline     = FALSE;
  priv->use_markup        = FALSE;
  priv->pattern_set       = FALSE;
  priv->track_links       = TRUE;

  priv->mnemonic_keyval   = GDK_KEY_VoidSymbol;
  label->layout           = NULL;
  label->text             = NULL;
  label->attrs            = NULL;

  priv->mnemonic_widget   = NULL;
  priv->mnemonic_window   = NULL;

  priv->mnemonics_visible = TRUE;

  geda_label_set_text (label, "label");

  {
    AtkObject *obj;
    obj = gtk_widget_get_accessible(GTK_WIDGET(label));
    atk_object_set_name (obj, _("Label"));
  }
}


static void
geda_label_buildable_interface_init (GtkBuildableIface *iface)
{
  buildable_parent_iface = g_type_interface_peek_parent (iface);

  iface->custom_tag_start = geda_label_buildable_custom_tag_start;
  iface->custom_finished = geda_label_buildable_custom_finished;
}

typedef struct {
  GtkBuilder    *builder;
  GObject       *object;
  PangoAttrList *attrs;
} PangoParserData;

static PangoAttribute *
attribute_from_text (GtkBuilder *builder, const char *name,
                     const char *value,   GError    **error)
{
  PangoAttribute *attribute = NULL;
  PangoAttrType   type;
  PangoLanguage  *language;
  PangoFontDescr *font_desc;
  GdkColor       *color;
  GValue          val = G_VALUE_INIT;

  if (!gtk_builder_value_from_string_type (builder,
                                           PANGO_TYPE_ATTR_TYPE,
                                           name, &val, error))
  {
    return NULL;
  }

  type = g_value_get_enum (&val);
  g_value_unset (&val);

  switch (type) {

    /* PangoAttrLanguage */
    case PANGO_ATTR_LANGUAGE:
      if ((language = pango_language_from_string (value))) {
        attribute = pango_attr_language_new (language);
        g_value_init (&val, G_TYPE_INT);
      }
      break;
      /* PangoAttrInt */
      case PANGO_ATTR_STYLE:
        if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_STYLE, value, &val, error))
          attribute = pango_attr_style_new (g_value_get_enum (&val));
        break;
      case PANGO_ATTR_WEIGHT:
        if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_WEIGHT, value, &val, error))
          attribute = pango_attr_weight_new (g_value_get_enum (&val));
        break;
      case PANGO_ATTR_VARIANT:
        if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_VARIANT, value, &val, error))
          attribute = pango_attr_variant_new (g_value_get_enum (&val));
        break;
      case PANGO_ATTR_STRETCH:
        if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_STRETCH, value, &val, error))
          attribute = pango_attr_stretch_new (g_value_get_enum (&val));
        break;
      case PANGO_ATTR_UNDERLINE:
        if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_UNDERLINE, value, &val, NULL))
          attribute = pango_attr_underline_new (g_value_get_enum (&val));
        else {
          /* XXX: allow boolean for backwards compat, so ignore error */
          /* Deprecate this somehow */
          g_value_unset (&val);
          if (gtk_builder_value_from_string_type (builder, G_TYPE_BOOLEAN, value, &val, error))
            attribute = pango_attr_underline_new (g_value_get_boolean (&val));
        }
        break;
      case PANGO_ATTR_STRIKETHROUGH:
        if (gtk_builder_value_from_string_type (builder, G_TYPE_BOOLEAN, value, &val, error))
          attribute = pango_attr_strikethrough_new (g_value_get_boolean (&val));
        break;
      case PANGO_ATTR_GRAVITY:
        if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_GRAVITY, value, &val, error))
          attribute = pango_attr_gravity_new (g_value_get_enum (&val));
        break;
      case PANGO_ATTR_GRAVITY_HINT:
        if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_GRAVITY_HINT,
          value, &val, error))
          attribute = pango_attr_gravity_hint_new (g_value_get_enum (&val));
        break;
        /* PangoAttrString */
        case PANGO_ATTR_FAMILY:
          attribute = pango_attr_family_new (value);
          g_value_init (&val, G_TYPE_INT);
          break;

          /* PangoAttrSize */
          case PANGO_ATTR_SIZE:
            if (gtk_builder_value_from_string_type (builder, G_TYPE_INT,
              value, &val, error))
              attribute = pango_attr_size_new (g_value_get_int (&val));
            break;
          case PANGO_ATTR_ABSOLUTE_SIZE:
            if (gtk_builder_value_from_string_type (builder, G_TYPE_INT,
              value, &val, error))
              attribute = pango_attr_size_new_absolute (g_value_get_int (&val));
            break;

            /* PangoAttrFontDesc */
            case PANGO_ATTR_FONT_DESC:
              if ((font_desc = pango_font_description_from_string (value)))
              {
                attribute = pango_attr_font_desc_new (font_desc);
                pango_font_description_free (font_desc);
                g_value_init (&val, G_TYPE_INT);
              }
              break;

              /* PangoAttrColor */
              case PANGO_ATTR_FOREGROUND:
                if (gtk_builder_value_from_string_type (builder, GDK_TYPE_COLOR,
                  value, &val, error))
                {
                  color = g_value_get_boxed (&val);
                  attribute = pango_attr_foreground_new (color->red, color->green, color->blue);
                }
                break;
              case PANGO_ATTR_BACKGROUND:
                if (gtk_builder_value_from_string_type (builder, GDK_TYPE_COLOR,
                  value, &val, error))
                {
                  color = g_value_get_boxed (&val);
                  attribute = pango_attr_background_new (color->red, color->green, color->blue);
                }
                break;
              case PANGO_ATTR_UNDERLINE_COLOR:
                if (gtk_builder_value_from_string_type (builder, GDK_TYPE_COLOR,
                  value, &val, error))
                {
                  color = g_value_get_boxed (&val);
                  attribute = pango_attr_underline_color_new (color->red, color->green, color->blue);
                }
                break;
              case PANGO_ATTR_STRIKETHROUGH_COLOR:
                if (gtk_builder_value_from_string_type (builder, GDK_TYPE_COLOR,
                  value, &val, error)) {
                  color = g_value_get_boxed (&val);
                attribute = pango_attr_strikethrough_color_new (color->red, color->green, color->blue);
                  }
                  break;

                  /* PangoAttrShape */
                  case PANGO_ATTR_SHAPE:
                    /* Unsupported for now */
                    break;
                    /* PangoAttrFloat */
                    case PANGO_ATTR_SCALE:
                      if (gtk_builder_value_from_string_type (builder, G_TYPE_DOUBLE,
                        value, &val, error))
                        attribute = pango_attr_scale_new (g_value_get_double (&val));
                      break;

                    case PANGO_ATTR_INVALID:
                    case PANGO_ATTR_LETTER_SPACING:
                    case PANGO_ATTR_RISE:
                    case PANGO_ATTR_FALLBACK:
                    default:
                      break;
  }

  g_value_unset (&val);

  return attribute;
}


static void
pango_start_element (GMarkupParseContext *context,
                     const char          *element_name,
                     const char         **names,
                     const char         **values,
                     void                *user_data,
                     GError             **error)
{
  PangoParserData *data = (PangoParserData*)user_data;
  GValue val = G_VALUE_INIT;
  unsigned int i;
  int line_number, char_number;

  if (strcmp (element_name, "attribute") == 0)
  {
    PangoAttribute *attr = NULL;
    const char *name = NULL;
    const char *value = NULL;
    const char *start = NULL;
    const char *end = NULL;
    unsigned int start_val = 0;
    unsigned int end_val   = G_MAXUINT;

    for (i = 0; names[i]; i++)
    {
      if (strcmp (names[i], "name") == 0)
        name = values[i];
      else if (strcmp (names[i], "value") == 0)
        value = values[i];
      else if (strcmp (names[i], "start") == 0)
        start = values[i];
      else if (strcmp (names[i], "end") == 0)
        end = values[i];
      else
      {
        g_markup_parse_context_get_position (context,
                                             &line_number,
                                             &char_number);
        g_set_error (error,
                     GTK_BUILDER_ERROR,
                     GTK_BUILDER_ERROR_INVALID_ATTRIBUTE,
                     "%s:%d:%d '%s' is not a valid attribute of <%s>",
                     "<input>",
                     line_number, char_number, names[i], "attribute");
        return;
      }
    }

    if (!name || !value)
    {
      g_markup_parse_context_get_position (context,
                                           &line_number,
                                           &char_number);
      g_set_error (error,
                   GTK_BUILDER_ERROR,
                   GTK_BUILDER_ERROR_MISSING_ATTRIBUTE,
                   "%s:%d:%d <%s> requires attribute \"%s\"",
                   "<input>",
                   line_number, char_number, "attribute",
                   name ? "value" : "name");
      return;
    }

    if (start)
    {
      if (!gtk_builder_value_from_string_type (data->builder, G_TYPE_UINT,
        start, &val, error))
        return;
      start_val = g_value_get_uint (&val);
      g_value_unset (&val);
    }

    if (end)
    {
      if (!gtk_builder_value_from_string_type (data->builder, G_TYPE_UINT,
        end, &val, error))
        return;
      end_val = g_value_get_uint (&val);
      g_value_unset (&val);
    }

    attr = attribute_from_text (data->builder, name, value, error);

    if (attr)
    {
      attr->start_index = start_val;
      attr->end_index   = end_val;

      if (!data->attrs)
        data->attrs = pango_attr_list_new ();

      pango_attr_list_insert (data->attrs, attr);
    }
  }
  else if (strcmp (element_name, "attributes") == 0)
    ;
  else
    fprintf(stderr, "Unsupported tag for GedaLabel: <%s>\n", element_name);
}

static const GMarkupParser pango_parser =
{
    pango_start_element,
};

static bool
geda_label_buildable_custom_tag_start (GtkBuildable     *buildable,
                                       GtkBuilder       *builder,
                                       GObject          *child,
                                       const char       *tagname,
                                       GMarkupParser    *parser,
                                       void            **data)
{
  if (buildable_parent_iface->custom_tag_start (buildable, builder, child,
    tagname, parser, data))
    return TRUE;

  if (strcmp (tagname, "attributes") == 0) {

    PangoParserData *parser_data;

    parser_data          = calloc (1, sizeof(PangoParserData));
    parser_data->builder = g_object_ref (builder);
    parser_data->object  = g_object_ref (buildable);
   *parser               = pango_parser;
   *data                 = parser_data;
    return TRUE;
  }
  return FALSE;
}

static void
geda_label_buildable_custom_finished (GtkBuildable *buildable,
                                      GtkBuilder   *builder,
                                      GObject      *child,
                                      const char  *tagname,
                                      void *      user_data)
{
  PangoParserData *parser_data;

  buildable_parent_iface->custom_finished (buildable, builder, child,
                                           tagname, user_data);

  if (strcmp (tagname, "attributes") == 0) {

    parser_data = (PangoParserData*)user_data;

    if (parser_data->attrs) {

      geda_label_set_attributes (GEDA_LABEL (buildable), parser_data->attrs);
      pango_attr_list_unref (parser_data->attrs);
    }

    g_object_unref (parser_data->object);
    g_object_unref (parser_data->builder);
    free (parser_data);
  }
}


/*! \brief Create a New Geda Label Object
 *
 *  \par Function Description
 *
 * Creates a new label with the given text inside it. You can
 * pass %NULL to get an empty label widget.
 *
 * \param [in] str The text of the label
 *
 * Return value: the new #GedaLabel
 */
GtkWidget* geda_label_new (const char *str)
{
  GedaLabel *label;

  label = g_object_new (GEDA_TYPE_LABEL, NULL);

  if (str && *str)
    geda_label_set_text (label, str);

  return GTK_WIDGET (label);
}

/*! \brief geda_mnemonic_label_new
 *
 *  \par Function Description
 *
 * Creates a new #GedaLabel, containing the text in str.
 *
 * If characters in str are preceded by an underscore, they are
 * underlined. If you need a literal underscore character in a label, use
 * '__' (two underscores). The first underlined character represents a
 * keyboard accelerator called a mnemonic. The mnemonic key can be used
 * to activate another widget, chosen automatically, or explicitly using
 * geda_label_set_mnemonic_widget().
 *
 * If geda_label_set_mnemonic_widget() is not called, then the first
 * activatable ancestor of the #GedaLabel will be chosen as the mnemonic
 * widget. For instance, if the label is inside a button or menu item,
 * the button or menu item will automatically become the mnemonic widget
 * and be activated by the mnemonic.
 *
 * The string should contain the text of the label, with an underscore in
 * front of the mnemonic character.
 *
 * \param [in] str Mnemonic for new label
 *
 * Return value: the new #GedaLabel
 */
GtkWidget* geda_mnemonic_label_new (const char *str)
{
  GedaLabel *label;

  label = g_object_new (GEDA_TYPE_LABEL, "use-underline", TRUE, NULL);

  if (str && *str)
    geda_label_set_mnemonic_text (label, str);

  return GTK_WIDGET (label);
}

GtkWidget *geda_visible_label_new (const char *str)
{
  GtkWidget *label;
  label = geda_label_new (str);
  g_object_set (label, "visible", TRUE, NULL);
  return label;
}
GtkWidget *geda_visible_mnemonic_label_new (const char *str)
{
  GtkWidget *label;
  label = geda_mnemonic_label_new (str);
  g_object_set (label, "visible", TRUE, NULL);
  return label;
}

GtkWidget *geda_aligned_label_new (const char *str, int x, int y)
{
  GtkWidget *label;
  label = geda_label_new (str);
  gtk_misc_set_alignment(GTK_MISC(label), x, y);
  return label;
}

GtkWidget *geda_aligned_visible_label_new (const char *str, int x, int y)
{
  GtkWidget *label;
  label = geda_label_new (str);
  gtk_misc_set_alignment(GTK_MISC(label), x, y);
  g_object_set (label, "visible", TRUE, NULL);
  return label;
}
GtkWidget *geda_aligned_mnemonic_label_new (const char *str, int x, int y)
{
  GtkWidget *label;
  label = geda_mnemonic_label_new (str);
  gtk_misc_set_alignment(GTK_MISC(label), x, y);
  return label;
}

GtkWidget  *geda_aligned_visible_mnemonic_label_new (const char *str,
                                                     int x, int y)
{
  GtkWidget *label;
  label = geda_mnemonic_label_new (str);
  gtk_misc_set_alignment(GTK_MISC(label), x, y);
  g_object_set (label, "visible", TRUE, NULL);
  return label;
}
static bool
geda_label_mnemonic_activate (GtkWidget *widget, bool group_cycling)
{
  GedaLabel *label = GEDA_LABEL (widget);
  GedaLabelPrivate *priv = label->priv;
  GtkWidget *parent;

  if (priv->mnemonic_widget)
    return gtk_widget_mnemonic_activate (priv->mnemonic_widget, group_cycling);

  /* Try to find the widget to activate by traversing the
   * widget's ancestry.
   */
  parent = gtk_widget_get_parent (widget);

  if (GTK_IS_NOTEBOOK (parent))
    return FALSE;

  while (parent)
  {
    if (gtk_widget_get_can_focus (parent) ||
      (!group_cycling && GTK_WIDGET_GET_CLASS (parent)->activate_signal) ||
      GTK_IS_NOTEBOOK (gtk_widget_get_parent (parent)) ||
      GTK_IS_MENU_ITEM (parent))
      return gtk_widget_mnemonic_activate (parent, group_cycling);
    parent = gtk_widget_get_parent (parent);
  }

  /* barf if there was nothing to activate */
  fprintf(stderr, "Could not find a target for a mnemonic activation.");
  gtk_widget_error_bell (widget);

  return FALSE;
}

static void
geda_label_setup_mnemonic (GedaLabel *label, unsigned int last_key)
{
  GedaLabelPrivate *priv = label->priv;
  GtkWidget *widget = GTK_WIDGET (label);
  GtkWidget *toplevel;
  GtkWidget *mnemonic_menu;

  mnemonic_menu = GEDA_OBJECT_GET_DATA(label, "gtk-mnemonic-menu");

  if (last_key != GDK_KEY_VoidSymbol) {

    if (priv->mnemonic_window) {

      gtk_window_remove_mnemonic  (priv->mnemonic_window, last_key, widget);
      priv->mnemonic_window = NULL;
    }

    if (mnemonic_menu) {
      mnemonic_menu = NULL;
    }
  }

  if (priv->mnemonic_keyval == GDK_KEY_VoidSymbol)
    goto done;

  connect_mnemonics_visible_notify (GEDA_LABEL (widget));

  toplevel = gtk_widget_get_toplevel (widget);

  if (gtk_widget_is_toplevel (toplevel)) {

    GtkWidget *menu_shell;

    menu_shell = gtk_widget_get_ancestor (widget, GTK_TYPE_MENU_SHELL);

    if (menu_shell) {
      mnemonic_menu = menu_shell;
    }

    if (!GTK_IS_MENU (menu_shell)) {
      priv->mnemonic_window = GTK_WINDOW (toplevel);
    }
  }

  done:
  g_object_set_data (G_OBJECT (label), _("gtk-mnemonic-menu"), mnemonic_menu);
}

static void
label_shortcut_setting_apply (GedaLabel *label)
{
  geda_label_recalculate (label);

  if (GTK_IS_ACCEL_LABEL (label))
    gtk_accel_label_refetch (GTK_ACCEL_LABEL (label));
}

static void
label_shortcut_setting_traverse_container (GtkWidget *widget,
                                           void *   data)
{
  if (GEDA_IS_LABEL (widget)) {
    label_shortcut_setting_apply (GEDA_LABEL (widget));
  }
  else if (GTK_IS_CONTAINER (widget)) {
    gtk_container_forall (GTK_CONTAINER (widget),
                          label_shortcut_setting_traverse_container, data);
  }
}

static void
label_shortcut_setting_changed (GtkSettings *settings)
{
  GList *list, *l;

  list = gtk_window_list_toplevels ();

  for (l = list; l ; l = l->next)
    {
      GtkWidget *widget = l->data;

      if (gtk_widget_get_settings (widget) == settings)
        gtk_container_forall (GTK_CONTAINER (widget),
                              label_shortcut_setting_traverse_container, NULL);
    }

  g_list_free (list);
}

static void
mnemonics_visible_apply (GtkWidget *widget,
                         bool   mnemonics_visible)
{
  GedaLabel *label = GEDA_LABEL (widget);
  GedaLabelPrivate *priv = label->priv;

  mnemonics_visible = mnemonics_visible != FALSE;

  if (priv->mnemonics_visible != mnemonics_visible)
    {
      priv->mnemonics_visible = mnemonics_visible;

      geda_label_recalculate (label);
    }
}

static void
label_mnemonics_visible_traverse_container (GtkWidget *widget,
                                            void *   data)
{
  bool mnemonics_visible = GPOINTER_TO_INT (data);

  _geda_label_mnemonics_visible_apply_recursively (widget, mnemonics_visible);
}

void
_geda_label_mnemonics_visible_apply_recursively (GtkWidget *widget,
                                                bool   mnemonics_visible)
{
  if (GEDA_IS_LABEL (widget))
    mnemonics_visible_apply (widget, mnemonics_visible);
  else if (GTK_IS_CONTAINER (widget))
    gtk_container_forall (GTK_CONTAINER (widget),
                          label_mnemonics_visible_traverse_container,
                          GINT_TO_POINTER (mnemonics_visible));
}

static void
label_mnemonics_visible_changed (GtkWindow  *window,
                                 GParamSpec *pspec,
                                 void *    data)
{
  bool mnemonics_visible;

  g_object_get (window, "mnemonics-visible", &mnemonics_visible, NULL);

  gtk_container_forall (GTK_CONTAINER (window),
                        label_mnemonics_visible_traverse_container,
                        GINT_TO_POINTER (mnemonics_visible));
}

static void
geda_label_screen_changed (GtkWidget *widget, GdkScreen *old_screen)
{
  GtkSettings *settings;
  bool shortcuts_connected;

  /* The PangoContext is replaced when the screen changes, so clear the layouts */
  geda_label_clear_layout (GEDA_LABEL (widget));

  if (!gtk_widget_has_screen (widget))
    return;

  settings = gtk_widget_get_settings (widget);

  shortcuts_connected =
  (int)(long)GEDA_OBJECT_GET_DATA (settings, "gtk-label-shortcuts-connected");

  if (! shortcuts_connected) {

      GEDA_SIGNAL_CONNECT (settings, "notify::gtk-enable-mnemonics",
                           G_CALLBACK (label_shortcut_setting_changed),
                           NULL);
      GEDA_SIGNAL_CONNECT (settings, "notify::gtk-enable-accels",
                           G_CALLBACK (label_shortcut_setting_changed),
                           NULL);

      g_object_set_data (G_OBJECT (settings), "gtk-label-shortcuts-connected",
                        (void*)(long)TRUE);
  }

  label_shortcut_setting_apply (GEDA_LABEL (widget));
}


static void
label_mnemonic_widget_weak_notify (void *data, GObject *where_the_object_was)
{
  GedaLabel *label = data;
  GedaLabelPrivate *priv = label->priv;

  priv->mnemonic_widget = NULL;
  g_object_notify (G_OBJECT (label), "mnemonic-widget");
}

/*! \brief geda_label_set_mnemonic_widget
 *
 *  \par Function Description
 *
 * If the label has been set so that it has an mnemonic key (using
 * i.e. geda_label_set_markup_with_mnemonic(),
 * geda_label_set_mnemonic_text(), geda_mnemonic_label_new()
 * or the "use_underline" property) the label can be associated with a
 * widget that is the target of the mnemonic. When the label is inside
 * a widget (like a GtkButton or a GtkNotebook tab) it is
 * automatically associated with the correct widget, but sometimes
 * (i.e. when the target is a GtkEntry next to the label) you need to
 * set it explicitly using this function.
 *
 * The target widget will be accelerated by emitting the
 * GtkWidget::mnemonic-activate signal on it. The default handler for
 * this signal will activate the widget if there are no mnemonic collisions
 * and toggle focus between the colliding widgets otherwise.
 *
 *  \param [in] label:    The GedaLabel object
 *  \param [in] widget:   Target GtkWidget
 */
void
geda_label_set_mnemonic_widget (GedaLabel  *label, GtkWidget *widget)
{
  GedaLabelPrivate *priv;

  g_return_if_fail (GEDA_IS_LABEL (label));

  priv = label->priv;

  if (widget)
    g_return_if_fail (GTK_IS_WIDGET (widget));

  if (priv->mnemonic_widget)
  {
    gtk_widget_remove_mnemonic_label (priv->mnemonic_widget,
                                      GTK_WIDGET (label));
    g_object_weak_unref (G_OBJECT (priv->mnemonic_widget),
                         label_mnemonic_widget_weak_notify,
                         label);
  }
  priv->mnemonic_widget = widget;
  if (priv->mnemonic_widget)
  {
    g_object_weak_ref (G_OBJECT (priv->mnemonic_widget),
                       label_mnemonic_widget_weak_notify,
                       label);
    gtk_widget_add_mnemonic_label (priv->mnemonic_widget, GTK_WIDGET (label));
  }

  g_object_notify (G_OBJECT (label), "mnemonic-widget");
}

/*! \brief geda_label_get_mnemonic_widget
 *
 *  \par Function Description
 *
 * Retrieves the target of the mnemonic (keyboard shortcut) of this
 * label. See geda_label_set_mnemonic_widget().
 *
 *  \param [in] label:    The GedaLabel object
 *
 * Return value: (transfer none): the target of the label's mnemonic,
 *     or %NULL if none has been set and the default algorithm will be used.
 */
GtkWidget *
geda_label_get_mnemonic_widget (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), NULL);

  return label->priv->mnemonic_widget;
}

/*! \brief geda_label_get_mnemonic_keyval
 *
 *  \par Function Description
 *
 * If the label has been set so that it has an mnemonic key this function
 * returns the keyval used for the mnemonic accelerator. If there is no
 * mnemonic set up it returns .
 *
 *  \param [in] label:    The GedaLabel object
 *
 * Returns: GDK keyval usable for accelerators, or
 */
unsigned int
geda_label_get_mnemonic_keyval (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), GDK_KEY_VoidSymbol);

  return label->priv->mnemonic_keyval;
}

static void
geda_label_set_text_internal (GedaLabel *label, char *str)
{
  GedaLabelPrivate *priv = label->priv;
  bool text_changed;
  bool is_selectable;

  text_changed = g_strcmp0 (label->text, str) != 0;

  g_free (label->text);
  label->text = str;

  is_selectable = (priv->select_info && priv->select_info->selectable);

  if (text_changed && is_selectable ) {
    geda_label_select_region_index (label, 0, 0);
  }
}

static void
geda_label_set_label_internal (GedaLabel *label, char *str)
{
  g_free (label->label);

  label->label = str;

  g_object_notify (G_OBJECT (label), "label");
}

static void
geda_label_set_use_markup_internal (GedaLabel *label, bool val)
{
  GedaLabelPrivate *priv = label->priv;

  val = val != FALSE;
  if (priv->use_markup != val)
    {
      priv->use_markup = val;

      g_object_notify (G_OBJECT (label), "use-markup");

    }
}

static void
geda_label_set_use_underline_internal (GedaLabel *label, bool val)
{
  GedaLabelPrivate *priv = label->priv;

  val = val != FALSE;
  if (priv->use_underline != val)
    {
      priv->use_underline = val;

      g_object_notify (G_OBJECT (label), "use-underline");
    }
}

/* Calculates text, attrs and mnemonic_keyval from
 * label, use_underline and use_markup
 */
static void geda_label_recalculate (GedaLabel *label)
{
  GedaLabelPrivate *priv = label->priv;
  unsigned int keyval = priv->mnemonic_keyval;

  geda_label_clear_links (label);

  if (priv->use_markup)
    geda_label_set_markup_internal (label, label->label, priv->use_underline);
  else if (priv->use_underline)
    geda_label_set_uline_text_internal (label, label->label);
  else
  {
    if (!priv->pattern_set) {
      if (label->markup_attrs)
        pango_attr_list_unref (label->markup_attrs);
      label->markup_attrs = NULL;
    }
    geda_label_set_text_internal (label, g_strdup (label->label));
  }

  if (!priv->use_underline)
    priv->mnemonic_keyval = GDK_KEY_VoidSymbol;

  if (keyval != priv->mnemonic_keyval) {
    geda_label_setup_mnemonic (label, keyval);
    g_object_notify (G_OBJECT (label), "mnemonic-keyval");
  }

  geda_label_clear_layout (label);
  geda_label_clear_select_info (label);
  gtk_widget_queue_resize (GTK_WIDGET (label));
}

/*! \brief geda_label_set_text
 *
 *  \par Function Description
 *
 * Sets the text within the #GedaLabel widget. It overwrites any text that
 * was there before.
 *
 * This will also clear any previously set mnemonic accelerators.
 *
 *  \param [in] label:  The GedaLabel object
 *  \param [in] str:    The text to be set
 */
void geda_label_set_text (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  g_object_freeze_notify (G_OBJECT (label));

  geda_label_set_label_internal (label, g_strdup (str ? str : ""));
  geda_label_set_use_markup_internal (label, FALSE);
  geda_label_set_use_underline_internal (label, FALSE);

  geda_label_recalculate (label);

  g_object_thaw_notify (G_OBJECT (label));
}
void geda_label_widget_set_text (GtkWidget *widget, const char *str)
{
  geda_label_set_text((GedaLabel *)widget,str);
}

/*! \brief geda_label_set_attributes
 *
 *  \par Function Description
 *
 * Sets a PangoAttrList; the attributes in the list are applied to the
 * label text.
 *
 * \note The attributes set with this function will be applied
 * and merged with any other attributes previously effected by way
 * of the #GedaLabel:use-underline or #GedaLabel:use-markup properties.
 * While it is not recommended to mix markup strings with manually set
 * attributes, if you must; know that the attributes will be applied
 * to the label after the markup string is parsed.
 *
 *  \param [in] label:  The GedaLabel object
 *  \param [in] attrs:  PangoAttrList structure
 */
void
geda_label_set_attributes (GedaLabel *label, PangoAttrList *attrs)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  if (attrs)
    pango_attr_list_ref (attrs);

  if (label->attrs)
    pango_attr_list_unref (label->attrs);
  label->attrs = attrs;

  g_object_notify (G_OBJECT (label), "attributes");

  geda_label_clear_layout (label);
  gtk_widget_queue_resize (GTK_WIDGET (label));
}

/*! \brief geda_label_get_attributes
 *
 *  \par Function Description
 *
 * Gets the attribute list that was set on the label using
 * geda_label_set_attributes(), if any. This function does
 * not reflect attributes that come from the labels markup
 * (see geda_label_set_markup()). If you want to get the
 * effective attributes for the label, use
 * pango_layout_get_attribute (geda_label_get_layout (label)).
 *
 *  \param [in] label:  The GedaLabel object
 *
 * Return value: (transfer none): the attribute list, or %NULL
 *     if none was set.
 */
PangoAttrList *geda_label_get_attributes (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), NULL);

  return label->attrs;
}

/*! \brief geda_label_set_label
 *
 *  \par Function Description
 *
 * Sets the text of the label. The label is interpreted as
 * including embedded underlines and/or Pango markup depending
 * on the values of the #GedaLabel:use-underline" and
 * #GedaLabel:use-markup properties.
 *
 *  \param [in] label:  The GedaLabel object
 *  \param [in] str:    New text to set for the label
 *
 */
void geda_label_set_label (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  g_object_freeze_notify (G_OBJECT (label));

  geda_label_set_label_internal (label, g_strdup (str ? str : ""));

  geda_label_recalculate (label);

  g_object_thaw_notify (G_OBJECT (label));
}

/*! \brief geda_label_get_label
 *
 *  \par Function Description
 *
 * Fetches the text from a label widget including any embedded
 * underlines indicating mnemonics and Pango markup. (See
 * geda_label_get_text()).
 *
 *  \param [in] label:  The GedaLabel object
 *
 * Return value: the text of the label widget. This string is
 *   owned by the widget and must not be modified or freed.
 */
const char *
geda_label_get_label (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), NULL);

  return label->label;
}

typedef struct
{
  GedaLabel *label;
  GList *links;
  GString *new_str;
  gsize text_len;
} UriParserData;

static void
start_element_handler (GMarkupParseContext  *context,
                       const char          *element_name,
                       const char         **attribute_names,
                       const char         **attribute_values,
                       void *              user_data,
                       GError              **error)
{
  GedaLabelPrivate *priv;
  UriParserData *pdata = user_data;

  if (strcmp (element_name, "a") == 0) {

    GedaLabelLink *link;
    const char *uri = NULL;
    const char *title = NULL;
    bool visited = FALSE;
    int line_number;
    int char_number;
    int i;

    g_markup_parse_context_get_position (context, &line_number, &char_number);

    for (i = 0; attribute_names[i] != NULL; i++) {
      const char *attr = attribute_names[i];

      if (strcmp (attr, "href") == 0)
        uri = attribute_values[i];
      else if (strcmp (attr, "title") == 0)
        title = attribute_values[i];
      else {
        g_set_error (error,
                     G_MARKUP_ERROR,
                     G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE,
                     "Attribute '%s' is not allowed on the <a> tag "
                     "on line %d char %d",
                     attr, line_number, char_number);
        return;
      }
    }

    if (uri == NULL) {
      g_set_error (error,
                   G_MARKUP_ERROR,
                   G_MARKUP_ERROR_INVALID_CONTENT,
                   "Attribute 'href' was missing on the <a> tag "
                   "on line %d char %d",
                   line_number, char_number);
      return;
    }

    visited = FALSE;
    priv = pdata->label->priv;

    if (priv->track_links && priv->select_info) {

      GList *list;

      for (list = priv->select_info->links; list; list = list->next) {

        link = list->data;

        if (strcmp (uri, link->uri) == 0) {
          visited = link->visited;
          break;
        }
      }
    }

    link = g_malloc0 (sizeof(GedaLabelLink));
    link->uri = g_strdup (uri);
    link->title = g_strdup (title);
    link->visited = visited;
    link->start = pdata->text_len;
    pdata->links = g_list_prepend (pdata->links, link);
  }
  else {

    int i;

    g_string_append_c (pdata->new_str, '<');
    g_string_append (pdata->new_str, element_name);

    for (i = 0; attribute_names[i] != NULL; i++) {

      const char *attr  = attribute_names[i];
      const char *value = attribute_values[i];
      char *newvalue;

      newvalue = g_markup_escape_text (value, -1);

      g_string_append_c (pdata->new_str, ' ');
      g_string_append (pdata->new_str, attr);
      g_string_append (pdata->new_str, "=\"");
      g_string_append (pdata->new_str, newvalue);
      g_string_append_c (pdata->new_str, '\"');

      g_free (newvalue);
    }
    g_string_append_c (pdata->new_str, '>');
  }
}

static void
end_element_handler (GMarkupParseContext  *context,
                     const char          *element_name,
                     void *              user_data,
                     GError              **error)
{
  UriParserData *pdata = user_data;

  if (!strcmp (element_name, "a")) {
      GedaLabelLink *link = pdata->links->data;
      link->end = pdata->text_len;
  }
  else {
      g_string_append (pdata->new_str, "</");
      g_string_append (pdata->new_str, element_name);
      g_string_append_c (pdata->new_str, '>');
    }
}

static void
text_handler (GMarkupParseContext  *context,
              const char          *text,
              gsize                 text_len,
              void *              user_data,
              GError              **error)
{
  UriParserData *pdata = user_data;
  char *newtext;

  newtext = g_markup_escape_text (text, text_len);
  g_string_append (pdata->new_str, newtext);
  pdata->text_len += text_len;
  g_free (newtext);
}

static const GMarkupParser markup_parser =
{
  start_element_handler,
  end_element_handler,
  text_handler,
  NULL,
  NULL
};

static bool xml_isspace (char c)
{
  return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
}

static void link_free (GedaLabelLink *link)
{
  g_free (link->uri);
  g_free (link->title);
  g_free (link);
}

static void
geda_label_get_link_colors (GtkWidget  *widget,
                            GdkColor  *link_color,
                            GdkColor  *visited_link_color)
{
  gtk_widget_ensure_style (widget);
  gtk_widget_style_get (widget,
                        "link-color", link_color,
                        "visited-link-color", visited_link_color,
                        NULL);
  if (!link_color)
    link_color = gdk_color_copy (&default_link_color);

  if (!visited_link_color)
    visited_link_color = gdk_color_copy (&default_visited_link_color);
}

static bool
parse_uri_markup (GedaLabel *label,   const char  *str,
                  char     **new_str, GList      **links,
                  GError   **error)
{
  GMarkupParseContext *context = NULL;
  const char *p, *end;
  bool needs_root = TRUE;
  gsize length;
  UriParserData pdata;

  length = strlen (str);
  p = str;
  end = str + length;

  pdata.label = label;
  pdata.links = NULL;
  pdata.new_str = g_string_sized_new (length);
  pdata.text_len = 0;

  while (p != end && xml_isspace (*p))
    p++;

  if (end - p >= 8 && strncmp (p, "<markup>", 8) == 0)
    needs_root = FALSE;

  context = g_markup_parse_context_new (&markup_parser, 0, &pdata, NULL);

  if (needs_root) {
    if (!g_markup_parse_context_parse (context, "<markup>", -1, error))
      goto failed;
  }

  if (!g_markup_parse_context_parse (context, str, length, error))
    goto failed;

  if (needs_root) {
    if (!g_markup_parse_context_parse (context, "</markup>", -1, error))
      goto failed;
  }

  if (!g_markup_parse_context_end_parse (context, error))
    goto failed;

  g_markup_parse_context_free (context);

  *new_str = g_string_free (pdata.new_str, FALSE);
  *links = pdata.links;

  return TRUE;

failed:
  g_markup_parse_context_free (context);
  g_string_free (pdata.new_str, TRUE);
  g_list_foreach (pdata.links, (GFunc) link_free, NULL);
  g_list_free (pdata.links);
  pdata.links = NULL;
  return FALSE;
}

static void geda_label_ensure_has_tooltip (GedaLabel *label)
{
  GList *l;
  bool has_tooltip = FALSE;

  for (l = label->priv->select_info->links; l; l = l->next) {
    GedaLabelLink *link = l->data;
    if (link->title) {
      has_tooltip = TRUE;
      break;
    }
  }

  gtk_widget_set_has_tooltip (GTK_WIDGET (label), has_tooltip);
}

static void
geda_label_set_markup_internal (GedaLabel    *label,
                                const char   *str,
                                bool          with_uline)
{
  GedaLabelPrivate *priv  = label->priv;
  PangoAttrList    *attrs = NULL;
  GError           *error = NULL;
  GList            *links = NULL;
  gunichar          accel = 0;     /* Accelerator Character */
  char             *text  = NULL;
  char             *new_str;

  if (!parse_uri_markup (label, str, &new_str, &links, &error)) {
    fprintf(stderr, "Failed to set text from markup due to error");
    fprintf(stderr, " parsing markup: %s\n", error->message);
    g_error_free (error);
    return;
  }

  if (links) {
    geda_label_ensure_select_info (label);
    priv->select_info->links = g_list_reverse (links);
    geda_label_ensure_has_tooltip (label);
  }

  if (with_uline) {

    bool enable_mnemonics;
    bool auto_mnemonics;

    g_object_get (gtk_widget_get_settings (GTK_WIDGET (label)),
                  "gtk-enable-mnemonics", &enable_mnemonics,
                  "gtk-auto-mnemonics", &auto_mnemonics,
                  NULL);

    if (!(enable_mnemonics && priv->mnemonics_visible &&
      (!auto_mnemonics ||
      (gtk_widget_is_sensitive (GTK_WIDGET (label)) &&
      (!priv->mnemonic_widget ||
      gtk_widget_is_sensitive (priv->mnemonic_widget))))))
    {
      char *tmp;
      char *pattern;
      unsigned int key;

      if (separate_uline_pattern (new_str, &key, &tmp, &pattern)) {
        g_free (new_str);
        new_str = tmp;
        g_free (pattern);
      }
    }
  }

  if (!pango_parse_markup (new_str, -1, with_uline ? '_' : 0, &attrs,
                          &text, with_uline ? &accel : NULL, &error))
  {
    fprintf(stderr, "Failed to set text from markup due to error");
    fprintf(stderr, "parsing markup: %s", error->message);
    g_free (new_str);
    g_error_free (error);
    return;
  }

  g_free (new_str);

  if (text)
    geda_label_set_text_internal (label, text);

  if (attrs) {
    if (label->markup_attrs)
      pango_attr_list_unref (label->markup_attrs);
    label->markup_attrs = attrs;
  }

  if (accel != 0)
    priv->mnemonic_keyval = gdk_keyval_to_lower (gdk_unicode_to_keyval (accel));
  else
    priv->mnemonic_keyval = GDK_KEY_VoidSymbol;
}

/*! \brief geda_label_set_markup
 *
 *  \par Function Description
 *
 * Parses str which is marked up with the Pango text markup language, setting
 * the label's text and attribute list based on the parse results. If the str
 * is external data, you may need to escape it with g_markup_escape_text() or
 * \code
 * |[
 * char *markup;
 *
 * markup = g_markup_printf_escaped ("&lt;span style=\"italic\"&gt;&percnt;s&lt;/span&gt;", str);
 * geda_label_set_markup (GEDA_LABEL (label), markup);
 * g_free (markup);
 * ]|
 * \endcode>
 *  \param [in] label:  The GedaLabel object
 *  \param [in] str:    a markup string
 *
 */
void geda_label_set_markup (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  g_object_freeze_notify (G_OBJECT (label));

  geda_label_set_label_internal (label, g_strdup (str ? str : ""));

  geda_label_set_use_markup_internal (label, TRUE);

  geda_label_set_use_underline_internal (label, FALSE);

  geda_label_recalculate (label);

  g_object_thaw_notify (G_OBJECT (label));
}

/*! \brief geda_label_set_markup_with_mnemonic
 *
 *  \par Function Description
 *
 * Parses str which is marked up with the Pango text markup language,
 * setting the label's text and attribute list based on the parse results.
 * If characters in str are preceded by an underscore, they are underlined
 * indicating that they represent a keyboard accelerator called a mnemonic.
 *
 * The mnemonic key can be used to activate another widget, chosen
 * automatically, or explicitly using geda_label_set_mnemonic_widget().
 *
 *  \param [in] label:  The GedaLabel object
 *  \param [in] str:    The a markup string
 *
 */
void
geda_label_set_markup_with_mnemonic (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  g_object_freeze_notify (G_OBJECT (label));

  geda_label_set_label_internal (label, g_strdup (str ? str : ""));

  geda_label_set_use_markup_internal (label, TRUE);

  geda_label_set_use_underline_internal (label, TRUE);

  geda_label_recalculate (label);

  g_object_thaw_notify (G_OBJECT (label));
}

/*! \brief geda_label_get_text
 *
 *  \par Function Description
 *
 * Fetches the text from a label widget, as displayed on the
 * screen. This does not include any embedded underlines
 * indicating mnemonics or Pango markup. (See geda_label_get_label())
 *
 * Return value: the text in the label widget. This is the internal
 *   string used by the label, and must not be modified.
 *
 *  \param [in] label:  The GedaLabel object
 *
 */
const char *geda_label_get_text (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), NULL);

  return label->text;
}
const char*geda_label_widget_get_text (GtkWidget *widget)
{
  return geda_label_get_text((GedaLabel*) widget);
}

static PangoAttrList *
geda_label_pattern_to_attrs (GedaLabel *label, const char *pattern)
{
  const char *start;
  const char *p = label->text;
  const char *q = pattern;
  PangoAttrList *attrs;

  attrs = pango_attr_list_new ();

  while (1)
  {
    while (*p && *q && *q != '_')
    {
      p = g_utf8_next_char (p);
      q++;
    }
    start = p;
    while (*p && *q && *q == '_')
    {
      p = g_utf8_next_char (p);
      q++;
    }

    if (p > start)
    {
      PangoAttribute *attr = pango_attr_underline_new (PANGO_UNDERLINE_LOW);
      attr->start_index = start - label->text;
      attr->end_index = p - label->text;

      pango_attr_list_insert (attrs, attr);
    }
    else
      break;
  }

  return attrs;
}

static void
geda_label_set_pattern_internal (GedaLabel *label,
                                 const char *pattern,
                                 bool is_mnemonic)
{
  GedaLabelPrivate *priv = label->priv;
  PangoAttrList *attrs;
  bool enable_mnemonics;
  bool auto_mnemonics;

  if (priv->pattern_set)
    return;

  if (is_mnemonic)
  {
    g_object_get (gtk_widget_get_settings (GTK_WIDGET (label)),
                  "gtk-enable-mnemonics", &enable_mnemonics,
                  "gtk-auto-mnemonics", &auto_mnemonics,
                  NULL);

    if (enable_mnemonics && priv->mnemonics_visible && pattern &&
      (!auto_mnemonics ||
      (gtk_widget_is_sensitive (GTK_WIDGET (label)) &&
      (!priv->mnemonic_widget ||
      gtk_widget_is_sensitive (priv->mnemonic_widget)))))
      attrs = geda_label_pattern_to_attrs (label, pattern);
    else
      attrs = NULL;
  }
  else
    attrs = geda_label_pattern_to_attrs (label, pattern);

  if (label->markup_attrs)
    pango_attr_list_unref (label->markup_attrs);

  label->markup_attrs = attrs;
}

/*! \brief geda_label_set_pattern
 *
 *  \par Function Description
 *
 * The pattern of underlines you want under the existing text within the
 * #GedaLabel widget.  For example if the current text of the label says
 * "FooBarBaz" passing a pattern of "___   ___" will underline
 * "Foo" and "Baz" but not "Bar".
 *
 *  \param [in] label:   The GedaLabel object
 *  \param [in] pattern: The pattern as described above
 */
void geda_label_set_pattern (GedaLabel *label, const char *pattern)
{

  g_return_if_fail (GEDA_IS_LABEL (label));

  label->priv->pattern_set = FALSE;

  if (pattern) {

      geda_label_set_pattern_internal (label, pattern, FALSE);
      label->priv->pattern_set = TRUE;
  }
  else {
    geda_label_recalculate (label);
  }

  geda_label_clear_layout (label);

  gtk_widget_queue_resize (GTK_WIDGET (label));
}

/********************* Justification Property *********************/
/*! \brief geda_label_get_justify
 *
 *  \par Function Description
 *
 * Returns the justification of the label. See geda_label_set_justify().
 *
 *  \param [in] label:   The GedaLabel object
 *
 * Return value: GtkJustification
 */
GtkJustification geda_label_get_justify (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), 0);
  return label->priv->jtype;
}
GtkJustification geda_label_widget_get_justify (GtkWidget *widget)
{
  return geda_label_get_justify ( (GedaLabel*) widget);
}

/*! \brief geda_label_set_justify
 *
 *  \par Function Description
 *
 * Sets the alignment of the lines in the text of the label relative to
 * each other. %GTK_JUSTIFY_LEFT is the default value when the
 * widget is first created with geda_label_new(). If you instead want
 * to set the alignment of the label as a whole, use
 * gtk_misc_set_alignment() instead. geda_label_set_justify() has no
 * effect on labels containing only a single line.
 *
 *  \param [in] label:   The GedaLabel object
 *  \param [in] jtype:   The GedaLabel object
 */
void geda_label_set_justify (GedaLabel *label, GtkJustification jtype)
{
  g_return_if_fail (GEDA_IS_LABEL (label));
  g_return_if_fail (jtype >= GTK_JUSTIFY_LEFT && jtype <= GTK_JUSTIFY_FILL);

  if ((GtkJustification) label->priv->jtype != jtype) {

    label->priv->jtype = jtype;

    /* No real need to be this drastic, but easier than duplicating the code */
    geda_label_clear_layout (label);

    g_object_notify (G_OBJECT (label), "justify");

    gtk_widget_queue_resize (GTK_WIDGET (label));

  }
}
void geda_label_widget_set_justify (GtkWidget *widget, GtkJustification jtype)
{
  geda_label_set_justify ((GedaLabel*) widget, jtype);
}

/************************ Ellipsize Property **********************/
/*! \brief geda_label_get_ellipsize
 *
 *  \par Function Description
 *
 * Returns the ellipsizing position of the label. See geda_label_set_ellipsize().
 *
 *  \param [in] label:   The GedaLabel object
 *
 * Return value: PangoEllipsizeMode
 *
 */
PangoEllipsizeMode geda_label_get_ellipsize (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), PANGO_ELLIPSIZE_NONE);
  return label->priv->ellipsize;
}

PangoEllipsizeMode geda_label_widget_get_ellipsize (GtkWidget *widget)
{
  return geda_label_get_ellipsize ( (GedaLabel*) widget);
}

/*! \brief geda_label_set_ellipsize
 *
 *  \par Function Description
 *
 * Sets the mode used to ellipsize (add an ellipsis: "...") to the text
 * if there is not enough space to render the entire string.
 *
 *  \param [in] label:  The GedaLabel object
 *  \param [in] mode:   a PangoEllipsizeMode
 */
void geda_label_set_ellipsize (GedaLabel *label, PangoEllipsizeMode mode)
{
  g_return_if_fail (GEDA_IS_LABEL (label));
  g_return_if_fail (mode >= PANGO_ELLIPSIZE_NONE && mode <= PANGO_ELLIPSIZE_END);

  if ((PangoEllipsizeMode) label->priv->ellipsize != mode) {

    label->priv->ellipsize = mode;

    /* No real need to be this drastic, but easier than duplicating the code */
    geda_label_clear_layout (label);

    g_object_notify (G_OBJECT (label), "ellipsize");

    gtk_widget_queue_resize (GTK_WIDGET (label));
  }
}
void
geda_label_widget_set_ellipsize (GtkWidget *widget, PangoEllipsizeMode mode)
{
  geda_label_set_ellipsize ( (GedaLabel*) widget, mode);
}

/********************** Width Chars Property **********************/

/*! \brief geda_label_get_width_chars
 *
 *  \par Function Description
 *
 * Retrieves the desired width of label, in characters. See
 * geda_label_set_width_chars().
 *
 *  \param [in] label:  The GedaLabel object
 *
 * Return value: the width of the label in characters.
 */
int geda_label_get_width_chars (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), -1);

  return label->width_chars;
}
int geda_label_widget_get_width_chars (GtkWidget *widget)
{
  return geda_label_get_width_chars ( (GedaLabel*) widget);
}

/*! \brief geda_label_set_width_chars
 *
 *  \par Function Description
 * Sets the desired width in characters of label to n_chars.
 *
 *  \param [in] label:   The GedaLabel object
 *  \param [in] n_chars: New desired width, in characters.
 */
void geda_label_set_width_chars (GedaLabel *label, int n_chars)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  if (label->width_chars != n_chars) {

    label->width_chars = n_chars;

    g_object_notify (G_OBJECT (label), "width-chars");

    gtk_widget_queue_resize (GTK_WIDGET (label));
  }
}
void geda_label_widget_set_width_chars (GtkWidget *widget, int n_chars)
{
  geda_label_set_width_chars ( (GedaLabel*) widget, n_chars);
}

/********************* Max Width Chars Property *******************/
/*! \brief geda_label_get_max_width_chars
 *
 *  \par Function Description
 *
 * Retrieves the desired maximum width of label, in characters. See
 * geda_label_set_width_chars().
 *
 *  \param [in] label:   The GedaLabel object
 *
 * Return value: the maximum width of the label in characters.
 *
 */
int geda_label_get_max_width_chars (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), -1);

  return label->max_width_chars;
}
int geda_label_widget_get_max_width_chars (GtkWidget *widget)
{
  return geda_label_get_max_width_chars ( (GedaLabel*) widget);
}

/*! \brief geda_label_set_max_width_chars
 *
 *  \par Function Description
 *
 * Sets the desired maximum width in characters of label to n_chars.
 *
 *  \param [in] label:   The GedaLabel object
 *  \param [in] n_chars: New desired maximum width, in characters.
 */
void geda_label_set_max_width_chars (GedaLabel *label, int n_chars)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  if (label->max_width_chars != n_chars) {

    label->max_width_chars = n_chars;

    g_object_notify (G_OBJECT (label), "max-width-chars");

    gtk_widget_queue_resize (GTK_WIDGET (label));
  }
}
void geda_label_widget_set_max_width_chars (GtkWidget *widget, int n_chars)
{
  geda_label_set_max_width_chars ( (GedaLabel*) widget, n_chars);
}

/************************ Line Wrap Property **********************/
/*! \brief geda_label_set_line_wrap
 *
 *  \par Function Description
 *
 * Toggles line wrapping within the #GedaLabel widget. %TRUE makes it break
 * lines if text exceeds the widget's size. %FALSE lets the text get cut off
 * by the edge of the widget if it exceeds the widget size.
 *
 * Note that setting line wrapping to %TRUE does not make the label
 * wrap at its parent container's width, because GTK+ widgets
 * conceptually can't make their requisition depend on the parent
 * container's size. For a label that wraps at a specific position,
 * set the label's width using gtk_widget_set_size_request().
 *
 *  \param [in] label:  The GedaLabel object
 *  \param [in] wrap:   The desired setting
 */
void
geda_label_set_line_wrap (GedaLabel *label, bool  wrap)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  wrap = wrap != FALSE;

  if (label->priv->wrap != wrap) {

    label->priv->wrap = wrap;

    geda_label_clear_layout (label);

    gtk_widget_queue_resize (GTK_WIDGET (label));

    g_object_notify (G_OBJECT (label), "wrap");
  }
}

/*! \brief geda_label_get_line_wrap
 *
 *  \par Function Description
 *
 * Returns whether lines in the label are automatically wrapped.
 * See geda_label_set_line_wrap().
 *
 *  \param [in] label:  The GedaLabel object
 *
 * Return value: %TRUE if the lines of the label are automatically wrapped.
 */
bool geda_label_get_line_wrap (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), FALSE);

  return label->priv->wrap;
}

/*! \brief geda_label_set_line_wrap_mode
 *
 *  \par Function Description
 *
 * If line wrapping is on (see geda_label_set_line_wrap()) this controls how
 * the line wrapping is done. The default is %PANGO_WRAP_WORD which means
 * wrap on word boundaries.
 *
 *  \param [in] label:     The GedaLabel object
 *  \param [in] wrap_mode: The line wrap_mode setting
 */
void
geda_label_set_line_wrap_mode (GedaLabel *label, PangoWrapMode wrap_mode)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  if (label->priv->wrap_mode != wrap_mode) {

    label->priv->wrap_mode = wrap_mode;

    g_object_notify (G_OBJECT (label), "wrap-mode");

    gtk_widget_queue_resize (GTK_WIDGET (label));
  }
}

/*! \brief geda_label_get_line_wrap_mode
 *
 *  \par Function Description
 *
 * Returns line wrap mode used by the label. See geda_label_set_line_wrap_mode().
 *
 * \param [in] label:  The GedaLabel object
 *
 * Return value: %TRUE if the lines of the label are automatically wrapped.
 *
 */
PangoWrapMode
geda_label_get_line_wrap_mode (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), FALSE);

  return label->priv->wrap_mode;
}

static void
geda_label_destroy (GtkObject *object)
{
  GedaLabel *label = GEDA_LABEL (object);
  geda_label_set_mnemonic_widget (label, NULL);

  GTK_OBJECT_CLASS (geda_label_parent_class)->destroy (object);
}

static void geda_label_finalize (GObject *object)
{
  GedaLabel *label = GEDA_LABEL (object);

  g_free (label->label);
  g_free (label->text);

  if (label->layout) {
    g_object_unref (label->layout);
  }

  if (label->attrs)
    pango_attr_list_unref (label->attrs);

  if (label->markup_attrs)
    pango_attr_list_unref (label->markup_attrs);

  geda_label_clear_links (label);

  g_free (label->priv->select_info);

  if ( label->priv->font_map &&  G_IS_OBJECT(label->priv->font_map) ) {
    g_object_unref (label->priv->font_map);
    label->priv->font_map = NULL;
  }

  G_OBJECT_CLASS (geda_label_parent_class)->finalize (object);
}

static void
geda_label_clear_layout (GedaLabel *label)
{
  if (label->layout){
    g_object_unref (label->layout);
    label->layout = NULL;
  }
}

/* If word wrapping is on, then the height requisition can depend
 * on:
 *   - Any width set on the widget via gtk_widget_set_size_request().
 *   - The padding of the widget (xpad, set by gtk_misc_set_padding)
 *
 * Instead of trying to detect changes to these quantities, if we
 * are wrapping, we just rewrap for each size request. Since size
 * requisitions are cached by the GTK+ core, this is not expensive.
 */

static void
geda_label_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GedaLabel *label = GEDA_LABEL (widget);

  int width, height;

  if (label->priv->wrap_mode) {
    geda_label_clear_layout (label);
  }

  geda_label_ensure_layout (label);

  width  = label->misc.xpad * 2;
  height = label->misc.ypad * 2;

  if (label->priv->have_transform) {

    PangoRectangle     rect;
    PangoContext      *context;
    const PangoMatrix *matrix;

    context = pango_layout_get_context (label->layout);
    matrix  = pango_context_get_matrix (context);

    pango_layout_get_extents (label->layout, NULL, &rect);
    pango_matrix_transform_rectangle (matrix, &rect);
    pango_extents_to_pixels (&rect, NULL);

    requisition->width = width + rect.width;
    requisition->height = height + rect.height;
  }
  else {

    GtkWidgetAuxInfo *aux_info;
    PangoRectangle    logical_rect;

    aux_info = geda_widget_get_aux_info (widget, FALSE);

    pango_layout_get_extents (label->layout, NULL, &logical_rect);

    if ((label->priv->wrap_mode || label->priv->ellipsize ||
         label->width_chars > 0 || label->max_width_chars > 0) &&
         aux_info && aux_info->width > 0)
    {
      width += aux_info->width;
    }
    else if (label->priv->ellipsize ||
             label->width_chars > 0 ||
             label->max_width_chars > 0)
    {
      width += PANGO_PIXELS (get_label_char_width (label));
    }
    else {
      width += PANGO_PIXELS (logical_rect.width);
    }

    free(aux_info);
    aux_info = NULL;

    if (label->priv->single_line_mode) {

      PangoContext     *context;
      PangoFontMetrics *metrics;

      int ascent, descent;

      context = pango_layout_get_context (label->layout);
      metrics = pango_context_get_metrics (context,
                                           widget->style->font_desc,
                                           pango_context_get_language (context));

      ascent  = pango_font_metrics_get_ascent (metrics);
      descent = pango_font_metrics_get_descent (metrics);
      pango_font_metrics_unref (metrics);

      height += PANGO_PIXELS (ascent + descent);
    }
    else {
      height += PANGO_PIXELS (logical_rect.height);
    }

    requisition->width  = width;
    requisition->height = height;
  }
}

static void
geda_label_size_allocate (GtkWidget     *widget,
                          GtkAllocation *allocation)
{
  GedaLabel *label = GEDA_LABEL (widget);
  GedaLabelPrivate *priv = label->priv;

  GTK_WIDGET_CLASS (geda_label_parent_class)->size_allocate (widget, allocation);

  if (label->layout)
    geda_label_update_layout_width (label);

  if (priv->select_info && priv->select_info->window) {
    gdk_window_move_resize (priv->select_info->window,
                            allocation->x,
                            allocation->y,
                            allocation->width,
                            allocation->height);
  }
}

static void
geda_label_update_cursor (GedaLabel *label)
{
  GedaLabelPrivate *priv;
  GtkWidget        *widget;

  priv = label->priv;

  if (!priv->select_info)
    return;

  widget = GTK_WIDGET (label);

  if (gtk_widget_get_realized (widget)) {
    GdkDisplay *display;
    GdkCursor *cursor;

    if (gtk_widget_is_sensitive (widget)) {
      display = gtk_widget_get_display (widget);

      if (priv->select_info->active_link)
        cursor = gdk_cursor_new_for_display (display, GDK_HAND2);
      else if (priv->select_info->selectable)
        cursor = gdk_cursor_new_for_display (display, GDK_XTERM);
      else
        cursor = NULL;
    }
    else
      cursor = NULL;

    gdk_window_set_cursor (priv->select_info->window, cursor);

  }

}

static void
geda_label_state_changed (GtkWidget   *widget, GtkStateType prev_state)
{
  GedaLabel *label;
  label = GEDA_LABEL (widget);

  if (label->priv->select_info) {

    geda_label_select_region (label, 0, 0);
    geda_label_update_cursor (label);
  }

  if (GTK_WIDGET_CLASS (geda_label_parent_class)->state_changed)
    GTK_WIDGET_CLASS (geda_label_parent_class)->state_changed (widget, prev_state);
}

/* called by: geda_label_expose
 *            window_to_layout_coords
 *            layout_to_window_coords
 *            geda_label_get_layout_offsets
 */
static void
get_layout_location (GedaLabel *label, int *xp, int *yp)
{
  GtkAllocation *allocation;
  GtkMisc       *misc;
  GtkWidget     *widget;

  float xalign;
  int req_width, x, y;
  PangoRectangle logical;

  misc   = GTK_MISC (label);
  widget = GTK_WIDGET (label);

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR)
    xalign = misc->xalign;
  else
    xalign = 1.0 - misc->xalign;

  pango_layout_get_pixel_extents (label->layout, NULL, &logical);

  if (label->priv->ellipsize || label->width_chars > 0) {
    int width;

    width = pango_layout_get_width (label->layout);

    req_width = logical.width;
    if (width != -1)
      req_width = MIN(PANGO_PIXELS (width), req_width);
    req_width += 2 * misc->xpad;
  }
  else
    req_width = widget->requisition.width;

  allocation = geda_get_widget_allocation (widget);

  x = floor (allocation->x + (int)misc->xpad + xalign * (allocation->width - req_width));

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR)
    x = MAX (x, allocation->x + misc->xpad);
  else
    x = MIN (x, allocation->x + allocation->width - misc->xpad);
  x -= logical.x;

  /* bgo#315462 - For single-line labels, *do* align the requisition with
   * respect to the allocation, even if we are under-allocated.  For multi-line
   * labels, always show the top of the text when they are under-allocated.  The
   * rationale is this:
   *
   * - Single-line labels appear in GtkButtons, and it is very easy to get them
   *   to be smaller than their requisition.  The button may clip the label, but
   *   the label will still be able to show most of itself and the focus
   *   rectangle.  Also, it is fairly easy to read a single line of clipped text.
   *
   * - Multi-line labels should not be clipped to showing "something in the
   *   middle".  You want to read the first line, at least, to get some context.
   */
  if (pango_layout_get_line_count (label->layout) == 1)
    y = floor (allocation->y + (int)misc->ypad
    + (allocation->height - widget->requisition.height) * misc->yalign);
  else
    y = floor (allocation->y + (int)misc->ypad
    + MAX (((allocation->height - widget->requisition.height) * misc->yalign),
           0));

    if (xp)
      *xp = x;

    if (yp)
      *yp = y;
}

static PangoDirection get_cursor_direction (GedaLabel *label)
{
  GedaLabelSelectionInfo *select_info;
  GSList *l;
  int result;

  select_info = label->priv->select_info;

  if (select_info == NULL) {
    BUG_MSG ("select_info = NULL");
    result = 0;
  }
  else {

    geda_label_ensure_layout (label);

    for (l = pango_layout_get_lines_readonly (label->layout); l; l = l->next)
    {
      PangoLayoutLine *line = l->data;

      /* If priv->select_info->selection_end is at the very end of
       * the line, we don't know if the cursor is on this line or
       * the next without looking ahead at the next line. (End
       * of paragraph is different from line break.) But it's
       * definitely in this paragraph, which is good enough
       * to figure out the resolved direction.
       */
      if (line->start_index + line->length >= select_info->selection_end)
        return line->resolved_dir;
    }
    result = PANGO_DIRECTION_LTR;
  }
  return result;
}
static void
draw_insertion_cursor (GedaLabel      *label,
                       GdkRectangle   *cursor_location,
                       bool            is_primary,
                       PangoDirection  direction,
                       bool            draw_arrow)
{
  GtkWidget *widget = GTK_WIDGET (label);
  GtkTextDirection text_dir;

  if (direction == PANGO_DIRECTION_LTR)
    text_dir = GTK_TEXT_DIR_LTR;
  else
    text_dir = GTK_TEXT_DIR_RTL;

  gtk_draw_insertion_cursor (widget, widget->window, &(widget->allocation),
                             cursor_location,
                             is_primary, text_dir, draw_arrow);
}

static void
geda_label_draw_cursor (GedaLabel  *label, int xoffset, int yoffset)
{
  GtkWidget *widget;

  if (label->priv->select_info == NULL)
    return;

  widget = GTK_WIDGET (label);

  if (gtk_widget_is_drawable (widget))
  {
    PangoDirection keymap_direction;
    PangoDirection cursor_direction;
    PangoRectangle strong_pos, weak_pos;
    bool split_cursor;
    PangoRectangle *cursor1 = NULL;
    PangoRectangle *cursor2 = NULL;
    GdkRectangle cursor_location;
    PangoDirection dir1 = PANGO_DIRECTION_NEUTRAL;
    PangoDirection dir2 = PANGO_DIRECTION_NEUTRAL;

    keymap_direction = gdk_keymap_get_direction (gdk_keymap_get_for_display (gtk_widget_get_display (widget)));
    cursor_direction = get_cursor_direction (label);

    geda_label_ensure_layout (label);

    pango_layout_get_cursor_pos (label->layout,
                                 label->priv->select_info->selection_end,
                                 &strong_pos, &weak_pos);

    g_object_get (gtk_widget_get_settings (widget),
                  "gtk-split-cursor", &split_cursor,  NULL);

    dir1 = cursor_direction;

    if (split_cursor)
    {
      cursor1 = &strong_pos;

      if (strong_pos.x != weak_pos.x ||
        strong_pos.y != weak_pos.y)
      {
        dir2 = (cursor_direction == PANGO_DIRECTION_LTR) ? PANGO_DIRECTION_RTL : PANGO_DIRECTION_LTR;
        cursor2 = &weak_pos;
      }
    }
    else
    {
      if (keymap_direction == cursor_direction)
        cursor1 = &strong_pos;
      else
        cursor1 = &weak_pos;
    }

    cursor_location.x = xoffset + PANGO_PIXELS (cursor1->x);
    cursor_location.y = yoffset + PANGO_PIXELS (cursor1->y);
    cursor_location.width = 0;
    cursor_location.height = PANGO_PIXELS (cursor1->height);

    draw_insertion_cursor (label,
                           &cursor_location, TRUE, dir1,
                           dir2 != PANGO_DIRECTION_NEUTRAL);

    if (dir2 != PANGO_DIRECTION_NEUTRAL)
    {
      cursor_location.x = xoffset + PANGO_PIXELS (cursor2->x);
      cursor_location.y = yoffset + PANGO_PIXELS (cursor2->y);
      cursor_location.width = 0;
      cursor_location.height = PANGO_PIXELS (cursor2->height);

      draw_insertion_cursor (label,
                             &cursor_location, FALSE, dir2,
                             TRUE);
    }
  }
}
static GedaLabelLink *
geda_label_get_focus_link (GedaLabel *label)
{
  GedaLabelSelectionInfo *info = label->priv->select_info;
  GList *l;

  if (!info)
    return NULL;

  if (info->selection_anchor != info->selection_end)
    return NULL;

  for (l = info->links; l; l = l->next)
    {
      GedaLabelLink *link = l->data;
      if (link->start <= info->selection_anchor &&
          info->selection_anchor <= link->end)
        return link;
    }

  return NULL;
}

static int
geda_label_expose (GtkWidget *widget, GdkEventExpose *event)
{
  GedaLabel *label = GEDA_LABEL (widget);
  GedaLabelSelectionInfo *info = label->priv->select_info;
  GtkAllocation allocation;
  GtkStyle *style;
  GtkStateType state;
  int x, y;
  cairo_t *cr;

  geda_label_ensure_layout (label);

  style = widget->style;

  state = gtk_widget_get_state (widget);

  gtk_widget_get_allocation (widget, &allocation);

  if (label->text && (*label->text != '\0')) {

    cr = gdk_cairo_create (event->window);

    get_layout_location (label, &x, &y);

    gtk_paint_layout (style, widget->window, state, FALSE, &event->area,
                      widget, "label", x, y, label->layout);

    if (info && (info->selection_anchor != info->selection_end)) {

      int range[2];
      GdkRegion *clip;

      range[0] = info->selection_anchor;
      range[1] = info->selection_end;

      if (range[0] > range[1])
      {
        int tmp = range[0];
        range[0] = range[1];
        range[1] = tmp;
      }

      clip = gdk_pango_layout_get_clip_region (label->layout, x, y, range, 1);

      gdk_cairo_region (cr, clip);
      cairo_clip (cr);
      gdk_region_destroy (clip);

      cairo_set_source_rgba (cr, 0.25, 0.35, 0.65, 0.8); /* white */

      cairo_paint (cr);

      cairo_set_source_rgba (cr, 1, 1, 1, 0.8); /* white */
      cairo_move_to (cr, x, y);

      pango_cairo_show_layout (cr, label->layout);

    }
    else if (info) {

      GedaLabelLink *focus_link;
      GedaLabelLink *active_link;
      int range[2];
      GdkRegion *clip;
      GdkRectangle   rect;
      GdkColor      *text_color;
      GdkColor      *base_color;
      GdkColor       link_color;
      GdkColor       visited_link_color;

      if (info->selectable &&  gtk_widget_has_focus (widget)) {
        geda_label_draw_cursor (label, x, y);
      }

      focus_link = geda_label_get_focus_link (label);
      active_link = info->active_link;

      if (active_link) {

        range[0] = active_link->start;
        range[1] = active_link->end;

        clip = gdk_pango_layout_get_clip_region (label->layout, x, y, range, 1);

        gdk_cairo_region (cr, clip);
        cairo_clip (cr);

        gdk_region_destroy (clip);

        geda_label_get_link_colors (widget, &link_color, &visited_link_color);
        if (active_link->visited)
          text_color = &visited_link_color;
        else
          text_color = &link_color;

        if (info->link_clicked)
          base_color = &widget->style->base[GTK_STATE_ACTIVE];
        else
          base_color = &widget->style->base[GTK_STATE_PRELIGHT];

        gdk_cairo_set_source_color (cr, base_color);
        cairo_paint (cr);

        gdk_cairo_set_source_color (cr, text_color);

        cairo_move_to (cr, x, y);

        pango_cairo_show_layout (cr, label->layout);

      }

      if (focus_link && gtk_widget_has_focus (widget)) {

        range[0] = focus_link->start;
        range[1] = focus_link->end;

        clip = gdk_pango_layout_get_clip_region (label->layout,
                                                 x, y,
                                                 range,
                                                 1);

        gdk_region_get_clipbox (clip, &rect);

        gtk_paint_focus (widget->style, widget->window, gtk_widget_get_state (widget),
                         &event->area, widget, "label",
                         rect.x, rect.y, rect.width, rect.height);
        gdk_region_destroy (clip);
      }
    }
    cairo_destroy(cr);
  }
  return FALSE;
}

static bool
separate_uline_pattern (const char  *str, unsigned int *accel_key,
                        char       **new_str,
                        char       **pattern)
{
  bool underscore;
  const char *src;
  char *dest;
  char *pattern_dest;

  *accel_key = GDK_KEY_VoidSymbol;
  *new_str = g_new (char, strlen (str) + 1);
  *pattern = g_new (char, g_utf8_strlen (str, -1) + 1);

  underscore = FALSE;

  src = str;
  dest = *new_str;
  pattern_dest = *pattern;

  while (*src) {
    gunichar c;
    const char *next_src;

    c = g_utf8_get_char (src);
    if (c == (gunichar)-1) {
      fprintf(stderr, "Invalid input string");
      g_free (*new_str);
      g_free (*pattern);

      return FALSE;
    }
    next_src = g_utf8_next_char (src);

    if (underscore) {
      if (c == '_')
        *pattern_dest++ = ' ';
      else
      {
        *pattern_dest++ = '_';
        if (*accel_key == GDK_KEY_VoidSymbol)
          *accel_key = gdk_keyval_to_lower (gdk_unicode_to_keyval (c));
      }

      while (src < next_src)
        *dest++ = *src++;

      underscore = FALSE;
    }
    else
    {
      if (c == '_') {
        underscore = TRUE;
        src = next_src;
      }
      else {
        while (src < next_src)
          *dest++ = *src++;

        *pattern_dest++ = ' ';
      }
    }
  }

  *dest = 0;
  *pattern_dest = 0;

  return TRUE;
}

static void
geda_label_set_uline_text_internal (GedaLabel *label, const char *str)
{
  unsigned int accel_key = GDK_KEY_VoidSymbol;
  char *new_str;
  char *pattern;

  g_return_if_fail (GEDA_IS_LABEL (label));
  g_return_if_fail (str != NULL);

  /* Split text into the base text and a separate pattern
   * of underscores.
   */
  if (!separate_uline_pattern (str, &accel_key, &new_str, &pattern))
    return;

  geda_label_set_text_internal (label, new_str);
  geda_label_set_pattern_internal (label, pattern, TRUE);
  label->priv->mnemonic_keyval = accel_key;

  g_free (pattern);
}

/*! \brief geda_label_set_mnemonic_text
 *
 *  \par Function Description
 *
 * Sets the label's text from the string str. If characters in str is
 * preceded by an underscore, they are underlined indicating that they
 * represent a keyboard accelerator called a mnemonic. The mnemonic key
 * can be used to activate another widget, chosen  automatically, or
 * explicitly using geda_label_set_mnemonic_widget().
 *
 * \param [in] label:  The GedaLabel object
 * \param [in] str:    Pointer to a string
 */
void
geda_label_set_mnemonic_text (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL (label));
  g_return_if_fail (str != NULL);

  g_object_freeze_notify (G_OBJECT (label));

  geda_label_set_label_internal (label, g_strdup (str ? str : ""));
  geda_label_set_use_markup_internal (label, FALSE);
  geda_label_set_use_underline_internal (label, TRUE);

  geda_label_recalculate (label);

  g_object_thaw_notify (G_OBJECT (label));
}

static void geda_label_realize (GtkWidget *widget)
{
  GedaLabel *label = GEDA_LABEL (widget);

  GTK_WIDGET_CLASS (geda_label_parent_class)->realize (widget);

  if (label->priv->select_info) {
    geda_label_create_window (label);
  }
}

static void geda_label_unrealize (GtkWidget *widget)
{
  GedaLabel *label = GEDA_LABEL (widget);

  if (label->priv->select_info) {
    geda_label_destroy_window (label);
  }
  GTK_WIDGET_CLASS (geda_label_parent_class)->unrealize (widget);
}

static void geda_label_map (GtkWidget *widget)
{
  GedaLabel *label = GEDA_LABEL (widget);

  GTK_WIDGET_CLASS (geda_label_parent_class)->map (widget);

  if (label->priv->select_info) {
    gdk_window_show (label->priv->select_info->window);
  }
}

static void geda_label_unmap (GtkWidget *widget)
{
  GedaLabel *label = GEDA_LABEL (widget);

  if (label->priv->select_info) {
    gdk_window_hide (label->priv->select_info->window);
  }
  GTK_WIDGET_CLASS (geda_label_parent_class)->unmap (widget);
}

static void window_to_layout_coords (GedaLabel *label, int *x, int *y)
{
  GtkAllocation *allocation;
  int lx, ly;

  /* get layout location in widget->window coords */
  get_layout_location (label, &lx, &ly);

  allocation = geda_get_widget_allocation (label);

  if (x) {
     *x += allocation->x; /* go to widget->window */
     *x -= lx;                   /* go to layout */
  }

  if (y) {
     *y += allocation->y; /* go to widget->window */
     *y -= ly;                   /* go to layout */
  }
}

/*! \retval TRUE if the position was within the layout
 *               otherwise FALSE */

static bool
get_layout_index (GedaLabel *label, int x, int y, int *index)
{
  int trailing = 0;
  const char *cluster;
  const char *cluster_end;
  bool inside;

  *index = 0;

  geda_label_ensure_layout (label);

  window_to_layout_coords (label, &x, &y);

  x *= PANGO_SCALE;
  y *= PANGO_SCALE;

  inside = pango_layout_xy_to_index (label->layout,
                                     x, y,
                                     index, &trailing);

  cluster = label->text + *index;
  cluster_end = cluster;
  while (trailing)
    {
      cluster_end = g_utf8_next_char (cluster_end);
      --trailing;
    }

  *index += (cluster_end - cluster);

  return inside;
}

/*         Selection   this is out of place       */
static void geda_label_select_word (GedaLabel *label)
{
  GedaLabelSelectionInfo *info;
  int min, max;

  info = label->priv->select_info;

  int start_index = geda_label_move_backward_word (label, info->selection_end);
  int end_index   = geda_label_move_forward_word (label, info->selection_end);

  min = MIN (info->selection_anchor, info->selection_end);
  max = MAX (info->selection_anchor, info->selection_end);

  min = MIN (min, start_index);
  max = MAX (max, end_index);

  geda_label_select_region_index (label, min, max);
}

static void geda_label_grab_focus (GtkWidget *widget)
{
  GedaLabel *label;
  GedaLabelPrivate *priv;
  GedaLabelLink *link;

  label = GEDA_LABEL (widget);
  priv = label->priv;

  if (priv->select_info == NULL)
    return;

  GTK_WIDGET_CLASS (geda_label_parent_class)->grab_focus (widget);

  if ( !priv->select_info->selectable &&
        priv->select_info->links && !priv->in_click) {

      link = priv->select_info->links->data;
      priv->select_info->selection_anchor = link->start;
      priv->select_info->selection_end = link->start;

  }
}

static bool
geda_label_focus (GtkWidget *widget, GtkDirectionType direction)
{
  GedaLabel *label;
  GedaLabelSelectionInfo *info;
  GedaLabelLink *focus_link;
  GList *l;

  label = GEDA_LABEL (widget);
  info = label->priv->select_info;

  if (!gtk_widget_is_focus (widget)) {

    gtk_widget_grab_focus (widget);
    if (info) {
      focus_link = geda_label_get_focus_link (label);
      if (focus_link && direction == GTK_DIR_TAB_BACKWARD) {

        l = g_list_last (info->links);
        focus_link = l->data;
        info->selection_anchor = focus_link->start;
        info->selection_end = focus_link->start;

      }
    }

    return TRUE;
  }

  if (!info)
    return FALSE;

  if (info->selectable) {
    int index;

    if (info->selection_anchor != info->selection_end)
      goto out;

    index = info->selection_anchor;

    if (direction == GTK_DIR_TAB_FORWARD)
      for (l = info->links; l; l = l->next) {
        GedaLabelLink *link = l->data;

        if (link->start > index) {

          geda_label_select_region_index (label, link->start, link->start);
          return TRUE;
        }
      }
      else if (direction == GTK_DIR_TAB_BACKWARD)
        for (l = g_list_last (info->links); l; l = l->prev) {
          GedaLabelLink *link = l->data;

          if (link->end < index) {

            geda_label_select_region_index (label, link->start, link->start);
            return TRUE;
          }
        }

        goto out;
  }
  else
  {
    focus_link = geda_label_get_focus_link (label);
    switch (direction) {

      case GTK_DIR_TAB_FORWARD:
        if (focus_link) {

          l = g_list_find (info->links, focus_link);
          l = l->next;

        }
        else
          l = info->links;
        break;

      case GTK_DIR_TAB_BACKWARD:
        if (focus_link) {
          l = g_list_find (info->links, focus_link);
          l = l->prev;
        }
        else
          l = g_list_last (info->links);
        break;

      default:
        goto out;
    }

    focus_link = l->data;
    info->selection_anchor = focus_link->start;
    info->selection_end = focus_link->start;
    gtk_widget_queue_draw (widget);

    return TRUE;

  }

  out:

  return FALSE;
}

bool geda_event_triggers_context_menu (GdkEventButton *event)
{
  if (event->type == GDK_BUTTON_PRESS) {
    if (event->button == 3 &&
      ! (event->state & (GDK_BUTTON1_MASK | GDK_BUTTON2_MASK)))
      return TRUE;

    #ifdef GDK_WINDOWING_QUARTZ
      if (event->button == 1 &&
        ! (event->state & (GDK_BUTTON2_MASK | GDK_BUTTON3_MASK)) &&
        (event->state & GDK_CONTROL_MASK))
        return TRUE;
      #endif
  }

  return FALSE;
}

static bool
geda_label_button_press (GtkWidget *widget, GdkEventButton *event)
{
  GedaLabel *label;
  GedaLabelSelectionInfo *info;
  int index = 0;
  int min, max;
  bool triggers_menu;

  label = GEDA_LABEL (widget);
  info = label->priv->select_info;

  if (info != NULL) {

    triggers_menu = geda_event_triggers_context_menu (event);

    if (info->active_link)
    {
      if (triggers_menu)
      {
        info->link_clicked = 1;
        geda_label_do_popup (label, event);
        return TRUE;
      }
      else if (event->button == GDK_BUTTON_PRIMARY)
      {
        info->link_clicked = 1;
        gtk_widget_queue_draw (widget);
      }
    }

    if (!info->selectable)
      return FALSE;

    info->in_drag = FALSE;
    info->select_words = FALSE;

    if (triggers_menu) {
      geda_label_do_popup (label, event);

      return TRUE;
    }
    else if (event->button == GDK_BUTTON_PRIMARY)
    {
      if (!gtk_widget_has_focus (widget)) {
        label->priv->in_click = TRUE;
        gtk_widget_grab_focus (widget);
        label->priv->in_click = FALSE;
      }

      get_layout_index (label, event->x, event->y, &index);

      min = MIN (info->selection_anchor, info->selection_end);
      max = MAX (info->selection_anchor, info->selection_end);

      if ((info->selection_anchor != info->selection_end) &&
        (event->state & GDK_SHIFT_MASK))
      {
        if (index > min && index < max) {
          /* truncate selection based on wipe direction */
          if (info->selection_anchor < info->selection_end ) /* if L2R*/
            max = index;
          else /* was wiped right to left */
            min = index;
        }
        else {
          /* extend (same as motion) */
          min = MIN (min, index);
          max = MAX (max, index);
        }

        /* ensure the anchor is opposite index */
        if (index == min) {
          int tmp = min;
          min = max;
          max = tmp;
        }

        geda_label_select_region_index (label, min, max);
      }
      else {
        if (event->type == GDK_3BUTTON_PRESS)
          geda_label_select_region_index (label, 0, strlen (label->text));
        else if (event->type == GDK_2BUTTON_PRESS)
          geda_label_select_word (label);
        else if (min < max && min <= index && index <= max)
        {
          info->in_drag = TRUE;
          info->drag_start_x = event->x;
          info->drag_start_y = event->y;
        }
        else {
          /* start a replacement */
          geda_label_select_region_index (label, index, index);
        }
      }

      return TRUE;
    }
  }
  return FALSE;
}

static bool
geda_label_button_release (GtkWidget *widget, GdkEventButton *event)
{
  GedaLabel *label;
  GedaLabelSelectionInfo *info;
  int index;

  label = GEDA_LABEL (widget);
  info  = label->priv->select_info;

  if (info == NULL)
    return FALSE;

  if (info->in_drag) {
      info->in_drag = 0;

      get_layout_index (label, event->x, event->y, &index);
      geda_label_select_region_index (label, index, index);

      return FALSE;
    }

  if (event->button != GDK_BUTTON_PRIMARY)
    return FALSE;

  if (info->active_link &&
      info->selection_anchor == info->selection_end &&
      info->link_clicked)
    {
      emit_activate_link (label, info->active_link);
      info->link_clicked = 0;

      return TRUE;
    }

  /* The goal here is to return TRUE if we ate the
   * button press to start selecting. */
  return TRUE;
}


static void
connect_mnemonics_visible_notify (GedaLabel *label)
{
  GtkWidget *toplevel;
  bool       connected;

  toplevel = gtk_widget_get_toplevel (GTK_WIDGET (label));

  if (!GTK_IS_WINDOW (toplevel))
    return;

  /* always set up this widgets initial value */
  label->priv->mnemonics_visible =
    gtk_window_get_mnemonics_visible (GTK_WINDOW (toplevel));

  connected = (int)(long)
  GEDA_OBJECT_GET_DATA(toplevel, "gtk-label-mnemonics-visible-connected");

  if (!connected) {

    g_signal_connect (toplevel,
                      "notify::mnemonics-visible",
                      G_CALLBACK (label_mnemonics_visible_changed),
                      label);
    g_object_set_data (G_OBJECT (toplevel),
                       "gtk-label-mnemonics-visible-connected",
                       GINT_TO_POINTER (1));
  }
}

static void
append_n_lines (GString *str, const char *text, GSList *lines, int n_lines)
{
  PangoLayoutLine *line;
  int i;

  for (i = 0; i < n_lines; i++) {

    line = lines->data;
    g_string_append_len (str, &text[line->start_index], line->length);
    lines = lines->next;
  }
}

static void limit_layout_lines (PangoLayout *layout)
{
  const char  *text;
  GString     *str;
  GSList      *lines, *elem;
  int          n_lines;

  n_lines = pango_layout_get_line_count (layout);

  if (n_lines >= DRAG_ICON_MAX_LINES) {
    text  = pango_layout_get_text (layout);
    str   = g_string_new (NULL);
    lines = pango_layout_get_lines_readonly (layout);

    /* get first lines */
    elem = lines;
    append_n_lines (str, text, elem, DRAG_ICON_MAX_LINES / 2);

    g_string_append (str, "\n" ELLIPSIS_CHARACTER "\n");

    /* get last lines */
    elem = g_slist_nth (lines, n_lines - DRAG_ICON_MAX_LINES / 2);
    append_n_lines (str, text, elem, DRAG_ICON_MAX_LINES / 2);

    pango_layout_set_text (layout, str->str, -1);
    g_string_free (str, TRUE);
  }
}

/*
 * _gtk_text_util_create_drag_icon
 * @widget: GtkWidget to extract the pango context
 * @text: a #char to render the icon
 * @len: length of @text, or -1 for NUL-terminated text
 *
 * Creates a drag and drop icon from @text.
 *
 * Returns: a #GdkPixmap to use as DND icon
 */
static GdkPixmap *
geda_label_create_drag_icon (GtkWidget *widget, char *text, unsigned int len)
{
  GdkDrawable  *drawable = NULL;
  PangoContext *context;
  PangoLayout  *layout;
  cairo_t      *cr;
  int          pixmap_height, pixmap_width;
  int          layout_width, layout_height;

  g_return_val_if_fail (widget != NULL, NULL);
  g_return_val_if_fail (text != NULL, NULL);

  context = gtk_widget_get_pango_context (widget);
  layout  = pango_layout_new (context);

  pango_layout_set_text (layout, text, len);
  pango_layout_set_wrap (layout, PANGO_WRAP_WORD_CHAR);
  pango_layout_get_size (layout, &layout_width, &layout_height);

  layout_width = MIN (layout_width, DRAG_ICON_MAX_WIDTH * PANGO_SCALE);
  pango_layout_set_width (layout, layout_width);

  limit_layout_lines (layout);

  /* get again layout extents, they may have changed */
  pango_layout_get_size (layout, &layout_width, &layout_height);

  pixmap_width  = layout_width  / PANGO_SCALE + DRAG_ICON_LAYOUT_BORDER * 2;
  pixmap_height = layout_height / PANGO_SCALE + DRAG_ICON_LAYOUT_BORDER * 2;

  drawable = gdk_pixmap_new (widget->window,
                             pixmap_width  + 2,
                             pixmap_height + 2,
                             -1);
  cr = gdk_cairo_create (drawable);

  gdk_cairo_set_source_color (cr, &widget->style->base [gtk_widget_get_state (widget)]);
  cairo_paint (cr);

  gdk_cairo_set_source_color (cr, &widget->style->text [gtk_widget_get_state (widget)]);
  cairo_move_to (cr, 1 + DRAG_ICON_LAYOUT_BORDER, 1 + DRAG_ICON_LAYOUT_BORDER);
  pango_cairo_show_layout (cr, layout);

  cairo_set_source_rgb (cr, 0, 0, 0);
  cairo_rectangle (cr, 0.5, 0.5, pixmap_width + 1, pixmap_height + 1);
  cairo_set_line_width (cr, 1.0);
  cairo_stroke (cr);

  cairo_destroy (cr);
  g_object_unref (layout);

  return drawable;
}
static void
drag_begin_cb (GtkWidget *widget, GdkDragContext *context, void * data)
{
  GedaLabel *label         = GEDA_LABEL (widget);
  GedaLabelPrivate *priv   = label->priv;
  GdkPixmap *pixmap        = NULL;

  g_signal_handlers_disconnect_by_func (widget, drag_begin_cb, NULL);

  if ((priv->select_info->selection_anchor !=
    priv->select_info->selection_end) &&
    label->text)
  {
    int start, end;
    int len;

    start = MIN (priv->select_info->selection_anchor,
                 priv->select_info->selection_end);
    end = MAX (priv->select_info->selection_anchor,
               priv->select_info->selection_end);

    len = strlen (label->text);

    if (end > len)
      end = len;

    if (start > len)
      start = len;

    pixmap = geda_label_create_drag_icon (widget, label->text + start, end - start);
  }

  if (pixmap) {
    gtk_drag_set_icon_pixmap (context,
                              gdk_drawable_get_colormap (pixmap),
                              pixmap,
                              NULL,
                              -2, -2);
  }
  else {
    gtk_drag_set_icon_default (context);
  }
}

static bool geda_label_motion (GtkWidget *widget, GdkEventMotion *event)
{
  GedaLabel *label;
  GedaLabelSelectionInfo *info;
  int index;

  label = GEDA_LABEL (widget);
  info  = label->priv->select_info;

  if (info == NULL)
    return FALSE;

  if (info->links && !info->in_drag) {
    GList *l;
    GedaLabelLink *link;
    bool found = FALSE;

    if (info->selection_anchor == info->selection_end)  {
      if (get_layout_index (label, event->x, event->y, &index))
      {
        for (l = info->links; l != NULL; l = l->next)
        {
          link = l->data;
          if (index >= link->start && index <= link->end)
          {
            found = TRUE;
            break;
          }
        }
      }
    }

    if (found)  {
      if (info->active_link != link)
      {
        info->link_clicked = 0;
        info->active_link = link;
        geda_label_update_cursor (label);
        gtk_widget_queue_draw (widget);
      }
    }
    else {
      if (info->active_link != NULL)
      {
        info->link_clicked = 0;
        info->active_link = NULL;
        geda_label_update_cursor (label);
        gtk_widget_queue_draw (widget);
      }
    }
  }

  if (!info->selectable)
    return FALSE;

  if ((event->state & GDK_BUTTON1_MASK) == 0)
    return FALSE;

  /* Only get here if the mouse is in motion with button 1 down! */

  if (info->in_drag) {
    if (gtk_drag_check_threshold (widget,
      info->drag_start_x,
      info->drag_start_y,
      event->x, event->y))
    {
      GtkTargetList *target_list = gtk_target_list_new (NULL, 0);

      gtk_target_list_add_text_targets (target_list, 0);

      g_signal_connect (widget, "drag-begin",
                        G_CALLBACK (drag_begin_cb), NULL);
      gtk_drag_begin (widget, target_list,
                      GDK_ACTION_COPY,
                      1, (GdkEvent *)event);

      info->in_drag = FALSE;

      gtk_target_list_unref (target_list);
    }
  }
  else {

    get_layout_index (label, event->x, event->y, &index);

    if (info->select_words) {
      int min, max;
      int old_min, old_max;
      int anchor, end;

      min = geda_label_move_backward_word (label, index);
      max = geda_label_move_forward_word (label, index);

      anchor = info->selection_anchor;
      end    = info->selection_end;

      old_min = MIN (anchor, end);
      old_max = MAX (anchor, end);

      if (min < old_min) {
        anchor = min;
        end = old_max;
      }
      else if (old_max < max) {
        anchor = max;
        end = old_min;
      }
      else if (anchor == old_min) {
        if (anchor != min)
          anchor = max;
      }
      else  {
        if (anchor != max)
          anchor = min;
      }
      geda_label_select_region_index (label, anchor, end);
    }
    else {
      geda_label_select_region_index (label, info->selection_anchor, index);
    }
  }

  return TRUE;
}

static bool
geda_label_leave_notify (GtkWidget        *widget,
                        GdkEventCrossing *event)
{
  GedaLabel *label = GEDA_LABEL (widget);

  if (label->priv->select_info) {

    label->priv->select_info->active_link = NULL;
    geda_label_update_cursor (label);
    gtk_widget_queue_draw (widget);
  }

  if (GTK_WIDGET_CLASS (geda_label_parent_class)->leave_notify_event) {
    return GTK_WIDGET_CLASS (geda_label_parent_class)->leave_notify_event (widget, event);
  }
  return FALSE;
}

static void
geda_label_create_window (GedaLabel *label)
{
  GedaLabelPrivate *priv;
  GtkAllocation    *allocation;
  GtkWidget        *widget;
  GdkWindowAttr     attributes;
  int               attributes_mask;

  priv   = label->priv;

  if (priv->select_info == NULL) {
    BUG_MSG ("select_info = NULL");
  }
  else {

    if (priv->select_info->window)
      return;

    widget = GTK_WIDGET (label);

    allocation = geda_get_widget_allocation (label);

    attributes.x                 = allocation->x;
    attributes.y                 = allocation->y;
    attributes.width             = allocation->width;
    attributes.height            = allocation->height;
    attributes.window_type       = GDK_WINDOW_CHILD;
    attributes.wclass            = GDK_INPUT_ONLY;
    attributes.override_redirect = TRUE;
    attributes.event_mask        = gtk_widget_get_events (widget) |
    GDK_BUTTON_PRESS_MASK        |
    GDK_BUTTON_RELEASE_MASK      |
    GDK_LEAVE_NOTIFY_MASK        |
    GDK_BUTTON_MOTION_MASK       |
    GDK_POINTER_MOTION_MASK      |
    GDK_POINTER_MOTION_HINT_MASK;

    attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_NOREDIR;

    if (gtk_widget_is_sensitive (widget)) {

      attributes.cursor = gdk_cursor_new_for_display (gtk_widget_get_display (widget), GDK_XTERM);
      attributes_mask  |= GDK_WA_CURSOR;
    }

    priv->select_info->window =
    gdk_window_new (gtk_widget_get_window (widget), &attributes, attributes_mask);

    gdk_window_set_user_data (priv->select_info->window, widget);
  }
}

static void
geda_label_destroy_window (GedaLabel *label)
{
  GedaLabelSelectionInfo *info;

  if (label->priv->select_info == NULL) {
    BUG_MSG ("select_info = NULL");
  }
  else {
    info = label->priv->select_info;

    if (info->window == NULL)
      return;

    gdk_window_set_user_data (info->window, NULL);
    gdk_window_destroy (info->window);
    info->window = NULL;
  }
}

static bool
geda_label_ensure_select_info (GedaLabel *label)
{
  GedaLabelPrivate *priv = label->priv;

  if (priv->select_info == NULL) {

    priv->select_info = g_malloc0 (sizeof(GedaLabelSelectionInfo));

    gtk_widget_set_can_focus (GTK_WIDGET (label), TRUE);

    if (gtk_widget_get_realized (GTK_WIDGET (label)))
      geda_label_create_window (label);

    if (gtk_widget_get_mapped (GTK_WIDGET (label)))
      gdk_window_show (priv->select_info->window);
  }

  return ( priv->select_info ? TRUE : FALSE );
}

static void
geda_label_clear_select_info (GedaLabel *label)
{
  if ( label->priv->select_info != NULL) {

    if (!label->priv->select_info->selectable &&
      !label->priv->select_info->links)
    {
      geda_label_destroy_window (label);
      g_free (label->priv->select_info);
      label->priv->select_info = NULL;
      gtk_widget_set_can_focus (GTK_WIDGET (label), FALSE);
    }
  }
}

/*********************** Selectable Property **********************/
/*! \brief geda_label_get_selectable
 *
 *  \par Function Description
 *
 * Gets the value set by geda_label_set_selectable().
 *
 * \param [in] label:  The GedaLabel object
 *
 * Return value: %TRUE if the user can copy text from the label
 */
bool geda_label_get_selectable (GedaLabel *label)
{
  GedaLabelSelectionInfo *info;
  g_return_val_if_fail (GEDA_IS_LABEL (label), FALSE);
  info = label->priv->select_info;
  return info && info->selectable;
}

bool geda_label_widget_get_selectable (GtkWidget *widget)
{
  return geda_label_get_selectable ( (GedaLabel*) widget);
}

/*! \brief geda_label_set_selectable
 *
 *  \par Function Description
 *
 * Selectable labels allow the user to select text from the label, for
 * copy-and-paste.
 *
 * \param [in] label:   The GedaLabel object
 * \param [in] setting: %TRUE to allow selecting text in the label
 */
void geda_label_set_selectable (GedaLabel *label, bool setting)
{
  GedaLabelPrivate *priv;
  bool old_setting;

  g_return_if_fail (GEDA_IS_LABEL (label));

  priv = label->priv;

  setting = setting != FALSE;
  old_setting = priv->select_info && priv->select_info->selectable;

  if (setting) {
    geda_label_ensure_select_info (label);
    priv->select_info->selectable = TRUE;
    geda_label_update_cursor (label);
  }
  else {
    if (old_setting) {
      /* unselect, to give up the selection */
      geda_label_select_region (label, 0, 0);

      priv->select_info->selectable = FALSE;
      geda_label_clear_select_info (label);
      geda_label_update_cursor (label);
    }
  }
  if (setting != old_setting) {
    g_object_freeze_notify (G_OBJECT (label));
    g_object_notify (G_OBJECT (label), "selectable");
    g_object_notify (G_OBJECT (label), "cursor-position");
    g_object_notify (G_OBJECT (label), "selection-bound");
    g_object_thaw_notify (G_OBJECT (label));
    gtk_widget_queue_draw (GTK_WIDGET (label));
  }
}
void geda_label_widget_set_selectable (GtkWidget *widget, bool setting)
{
  geda_label_set_selectable ((GedaLabel*) widget, setting);
}

/************************** Angle Property ************************/
/*! \brief geda_label_get_angle
 *
 *  \par Function Description
 *
 * Gets the angle of rotation for the label. See
 * geda_label_set_angle().
 *
 * \param [in] label:   The GedaLabel object
 *
 * Return value: the angle of rotation for the label
 *
 */
double geda_label_get_angle  (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), 0.0);
  return label->angle;
}
double geda_label_widget_get_angle (GtkWidget *widget)
{
  return geda_label_get_angle ((GedaLabel*) widget);
}

/*! \brief geda_label_set_angle
 *
 *  \par Function Description
 *
 * Sets the angle of rotation for the label. An angle of 90 reads from
 * from bottom to top, an angle of 270, from top to bottom. The angle
 * setting for the label is ignored if the label is selectable,
 * wrapped, or ellipsized.
 *
 * The angle is from the baseline of the label from horizontal, in
 * degrees, measured counterclockwise.
 *
 * \param [in] label:   The GedaLabel object
 * \param [in] angle:   The label angle
 */
void
geda_label_set_angle (GedaLabel *label, double angle)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  /* Canonicalize to [0,360]. We don't canonicalize 360 to 0, because
   * double property ranges are inclusive, and changing 360 to 0 would
   * make a property editor behave strangely.
   */
  if (angle < 0 || angle > 360.0)
    angle = angle - 360. * floor (angle / 360.);

  if (label->angle != angle)
    {
      label->angle = angle;

      geda_label_clear_layout (label);
      gtk_widget_queue_resize (GTK_WIDGET (label));

      g_object_notify (G_OBJECT (label), "angle");
    }
}

void geda_label_widget_set_angle (GtkWidget *widget, double angle)
{
  geda_label_set_angle ( (GedaLabel*) widget, angle);
}

static void
geda_label_set_selection_text (GedaLabel *label,
                               GtkSelectionData *selection_data)
{
  GedaLabelSelectionInfo *info = label->priv->select_info;

  if ( info &&  label->text &&
     ( info->selection_anchor != info->selection_end) )
  {
    int start, end;
    int len;

    start = MIN (info->selection_anchor, info->selection_end);
    end   = MAX (info->selection_anchor, info->selection_end);

    len = strlen (label->text);

    if (end > len)
      end = len;

    if (start > len)
      start = len;

    gtk_selection_data_set_text (selection_data,
                                 label->text + start,
                                 end - start);
  }
}

static void
geda_label_drag_data_get (GtkWidget        *widget,
                          GdkDragContext   *context,
                          GtkSelectionData *selection_data,
                          unsigned int      info,
                          unsigned int      time)
{
  geda_label_set_selection_text (GEDA_LABEL (widget), selection_data);
}

static void
get_text_callback (GtkClipboard     *clipboard,
                   GtkSelectionData *selection_data,
                   unsigned int      info,
                   void *          user_data_or_owner)
{
  geda_label_set_selection_text (GEDA_LABEL (user_data_or_owner), selection_data);
}

static void
clear_text_callback (GtkClipboard *clipboard, void * user_data_or_owner)
{
  GedaLabel *label;
  GedaLabelSelectionInfo *info;

  label = GEDA_LABEL (user_data_or_owner);
  info  = label->priv->select_info;

  if (info) {
    info->selection_anchor = info->selection_end;
    gtk_widget_queue_draw (GTK_WIDGET (label));
  }
}

static void geda_label_select_region_index (GedaLabel *label,
                                            int anchor_index,
                                            int end_index)
{
  GedaLabelPrivate *priv;

  g_return_if_fail (GEDA_IS_LABEL (label));

  priv = label->priv;

  if (priv->select_info && priv->select_info->selectable) {
    GtkClipboard *clipboard;

    if (priv->select_info->selection_anchor == anchor_index &&
      priv->select_info->selection_end    == end_index)
      return;

    g_object_freeze_notify (G_OBJECT (label));

    if (priv->select_info->selection_anchor != anchor_index)
      g_object_notify (G_OBJECT (label), "selection-bound");
    if (priv->select_info->selection_end != end_index)
      g_object_notify (G_OBJECT (label), "cursor-position");

    priv->select_info->selection_anchor = anchor_index;
    priv->select_info->selection_end    = end_index;

    if (gtk_widget_has_screen (GTK_WIDGET (label)))
      clipboard = gtk_widget_get_clipboard (GTK_WIDGET (label),
                                            GDK_SELECTION_PRIMARY);
      else
        clipboard = NULL;

      if (anchor_index != end_index) {
        GtkTargetList *list;
        GtkTargetEntry *targets;
        int n_targets;

        list = gtk_target_list_new (NULL, 0);
        gtk_target_list_add_text_targets (list, 0);
        targets = gtk_target_table_new_from_list (list, &n_targets);

        if (clipboard)
          gtk_clipboard_set_with_owner (clipboard,
                                        targets, n_targets,
                                        get_text_callback,
                                        clear_text_callback,
                                        G_OBJECT (label));

          gtk_target_table_free (targets, n_targets);
        gtk_target_list_unref (list);
      }
      else {
        if (clipboard &&
          gtk_clipboard_get_owner (clipboard) == G_OBJECT (label))
          gtk_clipboard_clear (clipboard);
      }

      gtk_widget_queue_draw (GTK_WIDGET (label));

      g_object_thaw_notify (G_OBJECT (label));
  }
}

/*! \brief geda_label_select_region
 *
 *  \par Function Description
 *
 * Selects a range of characters in the label, if the label is selectable.
 * See geda_label_set_selectable(). If the label is not selectable,
 * this function has no effect. If start_offset or
 * end_offset are -1, then the end of the label will be substituted.
 *
 * \param [in] label:        The GedaLabel object
 * \param [in] start_offset: The start offset (in characters not bytes)
 * \param [in] end_offset:   The end offset (in characters not bytes)
 *
 */
void
geda_label_select_region (GedaLabel *label, int start_offset, int end_offset)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  if (label->text && label->priv->select_info) {
    if (start_offset < 0)
      start_offset = g_utf8_strlen (label->text, -1);

    if (end_offset < 0)
      end_offset = g_utf8_strlen (label->text, -1);

    geda_label_select_region_index (label,
                                    g_utf8_offset_to_pointer (label->text, start_offset) - label->text,
                                    g_utf8_offset_to_pointer (label->text, end_offset) - label->text);
  }
}

/*! \brief geda_label_get_selection_bounds
 *
 *  \par Function Description
 *
 * Gets the selected range of characters in the label, returning %TRUE
 * if there's a selection.
 *
 * \param [in] label:  The GedaLabel object
 *
 * \param [out] start: location for start of selection, as a character offset
 * \param [out] end:   location for end of selection, as a character offset
 *
 * Return value: %TRUE if selection is non-empty
 */
bool
geda_label_get_selection_bounds (GedaLabel  *label, int *start, int *end)
{
  GedaLabelSelectionInfo *info;

  g_return_val_if_fail (GEDA_IS_LABEL (label), FALSE);

  info = label->priv->select_info;

  if ( info == NULL) {
    /* not a selectable label */
    if (start)
      *start = 0;
    if (end)
      *end = 0;

    return FALSE;
  }
  else {

    int start_index, end_index;
    int start_offset, end_offset;
    int len;

    start_index = MIN ( info->selection_anchor, info->selection_end);
    end_index   = MAX ( info->selection_anchor, info->selection_end);

    len = strlen (label->text);

    if (end_index > len)
      end_index = len;

    if (start_index > len)
      start_index = len;

    start_offset = g_utf8_strlen (label->text, start_index);
    end_offset   = g_utf8_strlen (label->text, end_index);

    if (start_offset > end_offset) {

      int tmp = start_offset;
      start_offset = end_offset;
      end_offset = tmp;

    }

    if (start)
      *start = start_offset;

    if (end)
      *end = end_offset;

    return start_offset != end_offset;
  }
}

/*! \brief geda_label_get_layout
 *
 *  \par Function Description
 *
 * Gets the PangoLayout used to display the label.
 * The layout is useful to e.g. convert text positions to
 * pixel positions, in combination with geda_label_get_layout_offsets().
 * The returned layout is owned by the label and should not be
 * freed by the caller. The label is free to recreate its layout at
 * any time, so it should be considered read-only.
 *
 * \param [in] label:  The GedaLabel object
 *
 * Return value: (transfer none): the PangoLayout for this label
 */
PangoLayout* geda_label_get_layout (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), NULL);

  geda_label_ensure_layout (label);

  return label->layout;
}

/*! \brief geda_label_get_layout_offsets
 *
 *  \par Function Description
 *
 * Obtains the coordinates where the label will draw the PangoLayout
 * representing the text in the label; useful to convert mouse events
 * into coordinates inside the PangoLayout, e.g. to take some action
 * if some part of the label is clicked. Of course you will need to
 * create a GtkEventBox to receive the events, and pack the label
 * inside it, since labels are a GTK_NO_WINDOW widget. Remember
 * when using the PangoLayout functions you need to convert to
 * and from pixels using PANGO_PIXELS() or PANGO_SCALE.
 *
 * \param [in] label:  The GedaLabel object
 * \param [out] x:     location to store X offset of layout, or %NULL
 * \param [out] y:     location to store Y offset of layout, or %NULL
 */
void
geda_label_get_layout_offsets (GedaLabel *label, int *x, int *y)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  geda_label_ensure_layout (label);

  get_layout_location (label, x, y);
}

/*! \brief geda_label_set_use_markup
 *
 *  \par Function Description
 *
 * Sets whether the text of the label contains markup in Pango's text markup
 * language. See geda_label_set_markup().
 *
 * \param [in] label:   The GedaLabel object
 * \param [in] setting: %TRUE if the label's text should be parsed for markup.
 */
void geda_label_set_use_markup (GedaLabel *label, bool setting)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  g_object_freeze_notify (G_OBJECT (label));

  geda_label_set_use_markup_internal (label, setting);

  geda_label_recalculate (label);

  g_object_thaw_notify (G_OBJECT (label));

}

/*! \brief geda_label_get_use_markup
 *
 *  \par Function Description
 *
 * Returns whether the label's text is interpreted as marked up with
 * the Pango text markup language. See geda_label_set_use_markup ().
 *
 * \param [in] label:   The GedaLabel object
 *
 * Return value: %TRUE if the label's text will be parsed for markup.
 */
bool geda_label_get_use_markup (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), FALSE);

  return label->priv->use_markup;
}

/*! \brief Widget Convenience Versions of label set_use_markup */
void geda_label_widget_set_use_markup (GtkWidget *widget, bool setting)
{
  geda_label_set_use_markup ((GedaLabel*)widget, setting);
}

/*! \brief Widget Convenience Versions of label get_use_markup */
bool geda_label_widget_get_use_markup (GtkWidget *widget)
{
  g_return_val_if_fail (GEDA_IS_LABEL (widget), FALSE);

  return geda_label_get_use_markup ((GedaLabel*)widget);
}

/*! \brief geda_label_set_use_underline
 *
 *  \par Function Description
 *
 * If true, an underline in the text indicates the next character should be
 * used for the mnemonic accelerator key.
 *
 * \param [in] label:   The GedaLabel object
 * \param [in] setting: %TRUE if underlines in the text indicate mnemonics.
 */
void geda_label_set_use_underline (GedaLabel *label, bool setting)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  g_object_freeze_notify (G_OBJECT (label));

  geda_label_set_use_underline_internal (label, setting);

  geda_label_recalculate (label);

  g_object_thaw_notify (G_OBJECT (label));

}

/*! \brief geda_label_get_use_underline
 *
 *  \par Function Description
 *
 * Returns whether an embedded underline in the label indicates a
 * mnemonic. See geda_label_set_use_underline().
 *
 * \param [in] label:   The GedaLabel object
 *
 * Return value: %TRUE whether an embedded underline in the label indicates
 *               the mnemonic accelerator keys.
 */
bool geda_label_get_use_underline (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), FALSE);

  return label->priv->use_underline;
}

/*! \brief geda_label_set_single_line_mode
 *
 *  \par Function Description
 *
 * set %TRUE if the label should be in single line mode
 *
 * Sets whether the label is in single line mode.
 *
 * \param [in] label:              The GedaLabel object
 * \param [in] single_line_mode:   Desired setting
 */
void
geda_label_set_single_line_mode (GedaLabel *label,
                                 bool single_line_mode)
{
  g_return_if_fail (GEDA_IS_LABEL (label));

  single_line_mode = single_line_mode != FALSE;

  if (label->priv->single_line_mode != single_line_mode) {

    label->priv->single_line_mode = single_line_mode;

    geda_label_clear_layout (label);

    gtk_widget_queue_resize (GTK_WIDGET (label));

    g_object_notify (G_OBJECT (label), "single-line-mode");

  }
}

/*! \brief geda_label_get_single_line_mode
 *
 *  \par Function Description
 *
 * Returns whether the label is in single line mode.
 *
 * \param [in] label: The GedaLabel object
 *
 * Return value: %TRUE when the label is in single line mode.
 *
 */
bool geda_label_get_single_line_mode  (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), FALSE);

  return label->priv->single_line_mode;
}

/* Compute the X position for an offset that corresponds to the "more important
 * cursor position for that offset. We use this when trying to guess to which
 * end of the selection we should go to when the user hits the left or
 * right arrow key.
 */
static void
get_better_cursor (GedaLabel *label, int index, int *x, int *y)
{
  GdkKeymap *keymap = gdk_keymap_get_for_display (gtk_widget_get_display (GTK_WIDGET (label)));
  PangoDirection keymap_direction = gdk_keymap_get_direction (keymap);
  PangoDirection cursor_direction = get_cursor_direction (label);
  bool split_cursor;
  PangoRectangle strong_pos, weak_pos;

  g_object_get (gtk_widget_get_settings (GTK_WIDGET (label)),
                "gtk-split-cursor", &split_cursor, NULL);

  geda_label_ensure_layout (label);

  pango_layout_get_cursor_pos (label->layout, index,
                               &strong_pos, &weak_pos);

  if (split_cursor) {
    *x = strong_pos.x / PANGO_SCALE;
    *y = strong_pos.y / PANGO_SCALE;
  }
  else {
    if (keymap_direction == cursor_direction) {
      *x = strong_pos.x / PANGO_SCALE;
      *y = strong_pos.y / PANGO_SCALE;
    }
    else {
      *x = weak_pos.x / PANGO_SCALE;
      *y = weak_pos.y / PANGO_SCALE;
    }
  }
}

static int
geda_label_move_logically (GedaLabel *label, int start, int count)
{
  int offset = g_utf8_pointer_to_offset (label->text, label->text + start);

  if (label->text) {

    PangoLogAttr *log_attrs;
    int n_attrs;
    int length;

    geda_label_ensure_layout (label);

    length = g_utf8_strlen (label->text, -1);

    pango_layout_get_log_attrs (label->layout, &log_attrs, &n_attrs);

    while (count > 0 && offset < length) {
      do
        offset++;
      while (offset < length && !log_attrs[offset].is_cursor_position);

      count--;
    }

    while (count < 0 && offset > 0) {
      do
        offset--;
      while (offset > 0 && !log_attrs[offset].is_cursor_position);

      count++;
    }

    g_free (log_attrs);
  }

  return g_utf8_offset_to_pointer (label->text, offset) - label->text;
}

static int
geda_label_move_visually (GedaLabel *label, int start, int count)
{
  int  index;
  bool split_cursor;
  bool strong;

  GtkSettings *split;
  GtkWidget   *widget;

  index  = start;
  widget = GTK_WIDGET(label);

  split = gtk_widget_get_settings (widget);

  g_object_get (split, "gtk-split-cursor", &split_cursor, NULL);

  while (count != 0) {

    int new_index, new_trailing;

    geda_label_ensure_layout (label);

    if (split_cursor) {
      strong = TRUE;
    }
    else {
      GdkKeymap *keymap = gdk_keymap_get_for_display (gtk_widget_get_display (widget));
      PangoDirection keymap_direction = gdk_keymap_get_direction (keymap);

      strong = keymap_direction == get_cursor_direction (label);
    }

    if (count > 0) {
      pango_layout_move_cursor_visually (label->layout, strong, index, 0, 1, &new_index, &new_trailing);
      count--;
    }
    else {
      pango_layout_move_cursor_visually (label->layout, strong, index, 0, -1, &new_index, &new_trailing);
      count++;
    }

    if (new_index < 0 || new_index == G_MAXINT)
      break;

    index = new_index;

    while (new_trailing--)
      index = g_utf8_next_char (label->text + new_index) - label->text;
  }

  return index;
}

static int
geda_label_move_forward_word (GedaLabel *label, int start)
{
  int new_pos = g_utf8_pointer_to_offset (label->text,
                                          label->text + start);
  int length;

  length = g_utf8_strlen (label->text, -1);

  if (new_pos < length) {

    PangoLogAttr *log_attrs;
    int n_attrs;

    geda_label_ensure_layout (label);

    pango_layout_get_log_attrs (label->layout, &log_attrs, &n_attrs);

    /* Find the next word end */
    new_pos++;

    while (new_pos < n_attrs && !log_attrs[new_pos].is_word_end) {
      new_pos++;
    }

    g_free (log_attrs);
  }

  return g_utf8_offset_to_pointer (label->text, new_pos) - label->text;
}


static int geda_label_move_backward_word (GedaLabel *label, int start)
{
  int new_pos = g_utf8_pointer_to_offset (label->text,
                                          label->text + start);

  if (new_pos > 0) {

    PangoLogAttr *log_attrs;
    int n_attrs;

    geda_label_ensure_layout (label);

    pango_layout_get_log_attrs (label->layout, &log_attrs, &n_attrs);

    new_pos -= 1;

    /* Find the previous word beginning */
    while (new_pos > 0 && !log_attrs[new_pos].is_word_start) {
      new_pos--;
    }

    g_free (log_attrs);
  }

  return g_utf8_offset_to_pointer (label->text, new_pos) - label->text;
}

static void
geda_label_move_cursor (GedaLabel *label, GtkMovementStep step,
                        int        count, bool extend_selection)
{
  GedaLabelSelectionInfo *info;

  int old_pos;
  int new_pos;

  info = label->priv->select_info;
  if (info == NULL)
    return;

  old_pos = new_pos = info->selection_end;

  if (info->selection_end != info->selection_anchor && !extend_selection) {

    /* If we have a current selection and aren't extending it, move to the
     * start/or end of the selection as appropriate */

    switch (step) {

      case GTK_MOVEMENT_VISUAL_POSITIONS:
      {
        int  end_x, end_y;
        int  anchor_x, anchor_y;
        bool end_is_left;

        get_better_cursor (label, info->selection_end, &end_x, &end_y);
        get_better_cursor (label, info->selection_anchor, &anchor_x, &anchor_y);

        end_is_left = (end_y < anchor_y) || (end_y == anchor_y && end_x < anchor_x);

        if (count < 0)
          new_pos = end_is_left ? info->selection_end : info->selection_anchor;
        else
          new_pos = !end_is_left ? info->selection_end : info->selection_anchor;
        break;
      }
      case GTK_MOVEMENT_LOGICAL_POSITIONS:
      case GTK_MOVEMENT_WORDS:
        if (count < 0)
          new_pos = MIN (info->selection_end, info->selection_anchor);
        else
          new_pos = MAX (info->selection_end, info->selection_anchor);
        break;
      case GTK_MOVEMENT_DISPLAY_LINE_ENDS:
      case GTK_MOVEMENT_PARAGRAPH_ENDS:
      case GTK_MOVEMENT_BUFFER_ENDS:
        new_pos = count < 0 ? 0 : (label->text) ? strlen (label->text) : 0;
        break;
      case GTK_MOVEMENT_DISPLAY_LINES:
      case GTK_MOVEMENT_PARAGRAPHS:
      case GTK_MOVEMENT_PAGES:
      case GTK_MOVEMENT_HORIZONTAL_PAGES:
        break;
    }
  }
  else {

    switch (step) {
      case GTK_MOVEMENT_LOGICAL_POSITIONS:
        new_pos = geda_label_move_logically (label, new_pos, count);
        break;
      case GTK_MOVEMENT_VISUAL_POSITIONS:

        new_pos = geda_label_move_visually (label, new_pos, count);

        if (new_pos == old_pos) {

          if (!extend_selection) {

            GtkDirectionType direct;
            bool success;

            direct  = count > 0 ? GTK_DIR_RIGHT : GTK_DIR_LEFT;
            success = gtk_widget_keynav_failed (GTK_WIDGET(label), direct);

            if (!success) {

              GtkWidget *toplevel = gtk_widget_get_toplevel (GTK_WIDGET (label));

              if (toplevel)
                gtk_widget_child_focus (toplevel,
                                        count > 0 ?
                                        GTK_DIR_RIGHT : GTK_DIR_LEFT);
            }
          }
          else {
            gtk_widget_error_bell (GTK_WIDGET (label));
          }
        }
        break;
      case GTK_MOVEMENT_WORDS:
        while (count > 0) {
          new_pos = geda_label_move_forward_word (label, new_pos);
          count--;
        }
        while (count < 0) {
          new_pos = geda_label_move_backward_word (label, new_pos);
          count++;
        }
        if (new_pos == old_pos)
          gtk_widget_error_bell (GTK_WIDGET (label));
        break;
      case GTK_MOVEMENT_DISPLAY_LINE_ENDS:
      case GTK_MOVEMENT_PARAGRAPH_ENDS:
      case GTK_MOVEMENT_BUFFER_ENDS:
        /* FIXME: Can do better here */
        new_pos = count < 0 ? 0 : strlen (label->text);
        if (new_pos == old_pos)
          gtk_widget_error_bell (GTK_WIDGET (label));
        break;
      case GTK_MOVEMENT_DISPLAY_LINES:
      case GTK_MOVEMENT_PARAGRAPHS:
      case GTK_MOVEMENT_PAGES:
      case GTK_MOVEMENT_HORIZONTAL_PAGES:
        break;
    }
  }

  if (extend_selection) {
    geda_label_select_region_index (label, info->selection_anchor, new_pos);
  }
  else {
      geda_label_select_region_index (label, new_pos, new_pos);
  }
}

static void
geda_label_copy_clipboard (GedaLabel *label)
{
  GedaLabelSelectionInfo *info = label->priv->select_info;

  if (label->text && info) {

    GtkClipboard *clipboard;
    int start, end, len;

    start = MIN (info->selection_anchor, info->selection_end);
    end   = MAX (info->selection_anchor,info->selection_end);

    len = strlen (label->text);

    if (end > len)
      end = len;

    if (start > len)
      start = len;

    clipboard =
    gtk_widget_get_clipboard (GTK_WIDGET (label), GDK_SELECTION_CLIPBOARD);

    if (start != end) {
      gtk_clipboard_set_text (clipboard, label->text + start, end - start);
    }
    else {

      GedaLabelLink *link;

      link = geda_label_get_focus_link (label);

      if (link) {
        gtk_clipboard_set_text (clipboard, link->uri, -1);
      }
    }
  }
}

static void geda_label_select_all (GedaLabel *label)
{
  geda_label_select_region_index (label, 0, strlen (label->text));
}

/* Quick hack of a popup menu
 */
static void activate_cb (GtkWidget *menuitem, GedaLabel *label)
{
  const char *signal = GEDA_OBJECT_GET_DATA(menuitem, "gtk-signal");
  g_signal_emit_by_name (label, signal);
}

static void
append_action_signal (GedaLabel   *label,
                      GtkWidget   *menu,
                      const char  *stock_id,
                      const char  *signal,
                      bool         sensitive)
{
  GtkWidget *menuitem = geda_image_menu_item_new_from_stock (stock_id, NULL);

  g_object_set_data (G_OBJECT (menuitem), _("gtk-signal"), (char *)signal);

  g_signal_connect (menuitem, "activate", G_CALLBACK (activate_cb), label);

  gtk_widget_set_sensitive (menuitem, sensitive);

  gtk_widget_show (menuitem);

  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
}

static void
popup_menu_detach (GtkWidget *attach_widget, GtkMenu *menu)
{
  GedaLabel        *label = GEDA_LABEL (attach_widget);
  GedaLabelPrivate *priv  = label->priv;

  if (priv->select_info) {
    priv->select_info->popup_menu = NULL;
  }
}

static void
popup_position_func (GtkMenu *menu, int *x, int *y, bool *push_in, void *data)
{
  GtkAllocation *allocation;
  GtkWidget     *widget;
  GtkRequisition req;
  GdkScreen     *screen;

  label  = GEDA_LABEL (data);
  widget = GTK_WIDGET (label);

  g_return_if_fail (gtk_widget_get_realized (widget));

  allocation = geda_get_widget_allocation (widget);
  screen = gtk_widget_get_screen (widget);
  gdk_window_get_origin (widget->window, x, y);

  gdk_window_get_origin (window, x, y);

  *x += allocation->x;
  *y += allocation->y;

  gtk_widget_size_request (GTK_WIDGET (menu), &req);

  *x += allocation->width / 2;
  *y += allocation->height;

  *x = CLAMP (*x, 0, MAX (0, gdk_screen_get_width (screen) - req.width));
  *y = CLAMP (*y, 0, MAX (0, gdk_screen_get_height (screen) - req.height));
}

static void
open_link_activate_cb (GtkMenuItem *menu_item, GedaLabel *label)
{
  GedaLabelLink *link;

  link = geda_label_get_current_link (label);

  if (link) {
    emit_activate_link (label, link);
  }
}

static void
copy_link_activate_cb (GtkMenuItem *menu_item, GedaLabel *label)
{
  GtkClipboard *clipboard;
  const char *uri;

  uri = geda_label_get_current_uri (label);

  if (uri) {

    clipboard = gtk_widget_get_clipboard (GTK_WIDGET (label), GDK_SELECTION_CLIPBOARD);
    gtk_clipboard_set_text (clipboard, uri, -1);
  }
}

static bool
geda_label_popup_menu (GtkWidget *widget)
{
  geda_label_do_popup (GEDA_LABEL (widget), NULL);

  return TRUE;
}

static void
geda_label_do_popup (GedaLabel *label, GdkEventButton *event)
{
  GedaLabelSelectionInfo *info = label->priv->select_info;
  GtkWidget *menuitem;
  GtkWidget *menu;
  GtkWidget *image;
  bool have_selection;
  GedaLabelLink *link;

  if (!info)
    return;

  if (info->popup_menu) {
    gtk_widget_destroy (info->popup_menu);
  }

  info->popup_menu = menu = gtk_menu_new ();

  gtk_menu_attach_to_widget (GTK_MENU(menu), GTK_WIDGET(label), popup_menu_detach);

  have_selection = info->selection_anchor != info->selection_end;

  if (event) {

      if (info->link_clicked)
        link = info->active_link;
      else
        link = NULL;
  }
  else {
    link = geda_label_get_focus_link (label);
  }

  if (!have_selection && link) {

      /* Open Link */
      menuitem = geda_image_menu_item_new_with_mnemonic (_("_Open Link"));
      gtk_widget_show (menuitem);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

      g_signal_connect (G_OBJECT (menuitem), "activate",
                        G_CALLBACK (open_link_activate_cb), label);

      image = gtk_image_new_from_stock (GTK_STOCK_JUMP_TO, GTK_ICON_SIZE_MENU);
      gtk_widget_show (image);
      geda_image_menu_item_set_image (GEDA_IMAGE_MENU_ITEM (menuitem), image);

      /* Copy Link Address */
      menuitem = geda_image_menu_item_new_with_mnemonic (_("Copy _Link Address"));
      gtk_widget_show (menuitem);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

      g_signal_connect (G_OBJECT (menuitem), "activate",
                        G_CALLBACK (copy_link_activate_cb), label);

      image = gtk_image_new_from_stock (GTK_STOCK_COPY, GTK_ICON_SIZE_MENU);
      gtk_widget_show (image);
      geda_image_menu_item_set_image (GEDA_IMAGE_MENU_ITEM (menuitem), image);
  }
  else {

      append_action_signal (label, menu, GTK_STOCK_CUT, "cut-clipboard", FALSE);
      append_action_signal (label, menu, GTK_STOCK_COPY, "copy-clipboard", have_selection);
      append_action_signal (label, menu, GTK_STOCK_PASTE, "paste-clipboard", FALSE);

      menuitem = geda_image_menu_item_new_from_stock (GTK_STOCK_DELETE, NULL);
      gtk_widget_set_sensitive (menuitem, FALSE);
      gtk_widget_show (menuitem);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

      menuitem = gtk_separator_menu_item_new ();
      gtk_widget_show (menuitem);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

      menuitem = geda_image_menu_item_new_from_stock (GTK_STOCK_SELECT_ALL, NULL);
      g_signal_connect_swapped (menuitem, "activate",  G_CALLBACK (geda_label_select_all), label);
      gtk_widget_show (menuitem);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
    }

  g_signal_emit (label, signals[POPULATE_POPUP], 0, menu);

  if (event) {
    gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
                    NULL, NULL,
                    event->button, event->time);
  }
  else {
    gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
                    popup_position_func, label,
                    0, gtk_get_current_event_time ());
    gtk_menu_shell_select_first (GTK_MENU_SHELL (menu), FALSE);
  }
}

static void
geda_label_clear_links (GedaLabel *label)
{
  GedaLabelSelectionInfo *info = label->priv->select_info;

  if (info) {

    g_list_foreach (info->links, (GFunc) link_free, NULL);
    g_list_free (info->links);
    info->links = NULL;
    info->active_link = NULL;
  }
}

static bool
geda_label_activate_link (GedaLabel *label, const char *uri)
{
  GtkWidget *widget = GTK_WIDGET (label);
  GError    *error = NULL;
  bool       result;

#if HAVE_GTK_SHOW_URI

  result = gtk_show_uri (gtk_widget_get_screen (widget), uri,
                         gtk_get_current_event_time (), &error);

  if (!result) {
    g_error_free (error);
    result = g_app_info_launch_default_for_uri(uri, NULL, &error);
  }

#else

  result = g_app_info_launch_default_for_uri(uri, NULL, &error);

#endif

  if (!result) {
      fprintf(stderr, "Unable to show '%s': %s", uri, error->message);
      g_error_free (error);
  }

  return TRUE;
}

static void
emit_activate_link (GedaLabel *label, GedaLabelLink *link)
{
  GedaLabelPrivate *priv = label->priv;
  bool handled;

  g_signal_emit (label, signals[ACTIVATE_LINK], 0, link->uri, &handled);

  if (handled && priv->track_links && !link->visited) {

    link->visited = TRUE;

    /* FIXME: shouldn't have to redo everything here */
    geda_label_clear_layout (label);
  }
}

static void geda_label_activate_current_link (GedaLabel *label)
{
  GedaLabelLink *link;
  GtkWidget *widget = GTK_WIDGET (label);

  link = geda_label_get_focus_link (label);

  if (link) {

      emit_activate_link (label, link);
  }
  else {

    GtkWidget *toplevel;
    GtkWindow *window;
    GtkWidget *default_widget, *focus_widget;

    toplevel = gtk_widget_get_toplevel (widget);

    if (GTK_IS_WINDOW (toplevel)) {

      window = GTK_WINDOW (toplevel);

      if (window) {

        default_widget = gtk_window_get_default_widget (window);
        focus_widget   = gtk_window_get_focus (window);

        if (default_widget != widget &&
          !(widget == focus_widget &&
          (!default_widget ||
          !gtk_widget_is_sensitive (default_widget))))
          gtk_window_activate_default (window);
      }
    }
  }
}

static GedaLabelLink *
geda_label_get_current_link (GedaLabel *label)
{
  GedaLabelSelectionInfo *info = label->priv->select_info;
  GedaLabelLink *link;

  if (!info)
    return NULL;

  if (info->link_clicked)
    link = info->active_link;
  else
    link = geda_label_get_focus_link (label);

  return link;
}

/*! \brief geda_label_get_current_uri
 *
 *  \par Function Description
 *
 * Returns the URI for the currently active link in the label.
 * The active link is the one under the mouse pointer or, in a
 * selectable label, the link in which the text cursor is currently
 * positioned.
 *
 * This function is intended for use in a GedaLabel::activate-link handler
 * or for use in a GtkWidget::query-tooltip handler.
 *
 *  \param [in] label:       The GedaLabel object
 *
 * Returns: the currently active URI. The string is owned by GTK+ and must
 *   not be freed or modified.
 *
 */
const char *
geda_label_get_current_uri (GedaLabel *label)
{
  GedaLabelLink *link;

  g_return_val_if_fail (GEDA_IS_LABEL (label), NULL);

  link = geda_label_get_current_link (label);

  if (link)
    return link->uri;

  return NULL;
}

/*! \brief geda_label_set_track_visited_links
 *
 *  \par Function Description
 *
 * Sets whether the label should keep track of clicked
 * links (and use a different color for them).
 *
 *  \param [in] label:       The GedaLabel object
 *  \param [in] track_links: %TRUE to track visited links
 */
void
geda_label_set_track_visited_links (GedaLabel *label, bool track_links)
{
  GedaLabelPrivate *priv;

  g_return_if_fail (GEDA_IS_LABEL (label));

  priv = label->priv;

  track_links = track_links != FALSE;

  if (priv->track_links != track_links) {

      priv->track_links = track_links;

      /* FIXME: shouldn't have to redo everything here */
      geda_label_recalculate (label);

      g_object_notify (G_OBJECT (label), "track-visited-links");
    }
}

/*! \brief geda_label_get_track_visited_links
 *
 *  \par Function Description
 *
 * Returns whether the label is currently keeping track
 * of clicked links.
 *
 *  \param [in] label The GedaLabel object
 *
 * Returns: %TRUE if clicked links are remembered
 *
 */
bool geda_label_get_track_visited_links (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL (label), FALSE);
  return label->priv->track_links;
}

static bool
geda_label_query_tooltip (GtkWidget  *widget,
                          int         x,
                          int         y,
                          bool        keyboard_tip,
                          GtkTooltip *tooltip)
{
  GedaLabel *label = GEDA_LABEL (widget);
  GedaLabelSelectionInfo *info = label->priv->select_info;
  int index = -1;
  GList *l;

  if (info && info->links) {

    if (keyboard_tip) {

      if (info->selection_anchor == info->selection_end)
        index = info->selection_anchor;
    }
    else {

      if (!get_layout_index (label, x, y, &index))
        index = -1;
    }

    if (index != -1)  {

      for (l = info->links; l != NULL; l = l->next)  {

        GedaLabelLink *link = l->data;
        if (index >= link->start && index <= link->end)  {

          if (link->title)  {

            gtk_tooltip_set_markup (tooltip, link->title);
            return TRUE;
          }
          break;
        }
      }
    }
  }

  return GTK_WIDGET_CLASS (geda_label_parent_class)->
            query_tooltip (widget, x, y, keyboard_tip, tooltip);
}

int
_geda_label_get_cursor_position (GedaLabel *label)
{
  GedaLabelPrivate *priv = label->priv;

  if (priv->select_info && priv->select_info->selectable)
    return g_utf8_pointer_to_offset (label->text,
                                     label->text + priv->select_info->selection_end);

  return 0;
}

int
_geda_label_get_selection_bound (GedaLabel *label)
{
  GedaLabelPrivate *priv = label->priv;

  if (priv->select_info && priv->select_info->selectable)
    return g_utf8_pointer_to_offset (label->text,
                                     label->text + priv->select_info->selection_anchor);

  return 0;
}
#undef PangoFontDescr
/** @} end group GedaLabel */
