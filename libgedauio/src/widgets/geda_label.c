/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_label.c
 *
 * GTK - The GIMP Toolkit
 *
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
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
 *  This is a replacement for the stock GtkLabel widget. The "bulk"
 *  of this code is from Gtk+3.7.4, but has been modified to run under
 *  the gtk+2.24.17 library. According to Valgrind: this version has
 *  fewer errors and does not have the memory leaks common to both the
 *  aforementioned versions. The direct leaks are not really in the
 *  Gtklabel widget. The leaks are related to Pango, FontConfig and
 *  somewhere else in GTK, possibly after tweeks by Debi. (note: After
 *  upgrading Cairo, from libcairo2_1.12.2-3 to libcairo2-dbg_1.12.14-5,
 *  and associated dependencies, under Debian, Wheezy->Sid, the amount
 *  of memory reported as "definitely lost" doubled.) This version
 *  attempts to implement a work around.
 * \par
 *  See geda_label_ensure_layout. Another suppression was not acceptable,
 *  couldn't let it go, so contrived this version ....
 *
 * \defgroup GedaLabel Text Label
 * @{
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <math.h>

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_accel_label.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_keysyms.h"
#include "../../include/geda_label.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_image_menu_item.h"
#include "../../include/geda_uio_functions.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/* GLIB < 2.30 */
#ifndef G_VALUE_INIT
#define G_VALUE_INIT  { 0, { { 0 } } }
#endif

#define MNEMONIC_MENU_DATA "mnemonic-menu"

#define PangoFontDescr  PangoFontDescription

typedef struct _SelectionInfo SelectionInfo;

struct _GedaLabelData
{
  AtkObject     *accessible;
  SelectionInfo *select_info;
  PangoFontMap  *font_map;

  GtkWidget *mnemonic_widget;
  GtkWindow *mnemonic_window;

  unsigned int  mnemonics_visible  : 1;
  unsigned int  jtype              : 2;
  unsigned int  wrap               : 1;
  unsigned int  use_underline      : 1;
  unsigned int  use_markup         : 1;
  unsigned int  ellipsize          : 3;
  unsigned int  single_line_mode   : 1;
  unsigned int  have_transform     : 1;
  unsigned int  in_click           : 1;
  unsigned int  wrap_mode          : 3;
  unsigned int  pattern_set        : 1;
  unsigned int  track_links        : 1;

  unsigned int  mnemonic_keyval;
};

typedef struct
{
  char *uri;
  char *title;     /* the title attribute, used as tooltip */
  bool  visited;   /* get set when the link is activated; this flag
                    * gets preserved over later set_markup() calls
                    */
  int start;       /* position of the link in the PangoLayout */
  int end;
} GedaLabelLink;

struct _SelectionInfo
{
  GedaLabelLink *active_link;
  GtkWidget     *popup_menu;
  GdkWindow     *window;
  GList         *links;

  int selection_anchor;
  int selection_end;

  int drag_start_x;
  int drag_start_y;

  unsigned int in_drag      : 1;
  unsigned int select_words : 1;
  unsigned int selectable   : 1;
  unsigned int link_clicked : 1;
};

typedef struct {
  GtkBuilder    *builder;
  GObject       *object;
  PangoAttrList *attrs;
} PangoParserData;

typedef struct
{
  GedaLabel *label;
  GList     *links;
  GString   *new_str;
  size_t     text_len;
} UriParserData;

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

static void geda_label_set_property          (GObject          *object,
                                              unsigned int      prop_id,
                                              const GValue     *value,
                                              GParamSpec       *pspec);
static void geda_label_get_property          (GObject          *object,
                                              unsigned int      prop_id,
                                              GValue           *value,
                                              GParamSpec       *pspec);
static void geda_label_destroy               (GtkObject        *object);
static void geda_label_finalize              (GObject          *object);
static void geda_label_size_allocate         (GtkWidget        *widget,
                                              GtkAllocation    *allocation);
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
static bool geda_label_focus                 (GtkWidget        *widget,
                                              GtkDirectionType   direction);

static void geda_label_realize               (GtkWidget        *widget);
static void geda_label_unrealize             (GtkWidget        *widget);
static void geda_label_map                   (GtkWidget        *widget);
static void geda_label_unmap                 (GtkWidget        *widget);

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

static void geda_label_set_text_internal          (GedaLabel         *label,
                                                   char              *str);
static void geda_label_set_label_internal         (GedaLabel         *label,
                                                   char              *str);
static void geda_label_set_use_markup_internal    (GedaLabel         *label,
                                                   bool               val);
static void geda_label_set_use_underline_internal (GedaLabel         *label,
                                                   bool               val);
static void geda_label_set_uline_text_internal    (GedaLabel         *label,
                                                   const char        *str);
static void geda_label_set_pattern_internal       (GedaLabel         *label,
                                                   const char        *pattern,
                                                   bool               is_mnemonic);
static void geda_label_set_markup_internal        (GedaLabel         *label,
                                                   const char        *str,
                                                   bool               with_uline);
static void geda_label_recalculate                (GedaLabel         *label);
static void geda_label_screen_changed             (GtkWidget         *widget,
                                                   GdkScreen         *old_screen);
static bool geda_label_popup_menu                 (GtkWidget         *widget);
static void geda_label_create_window              (GedaLabel         *label);
static void geda_label_destroy_window             (GedaLabel         *label);
static bool geda_label_ensure_select_info         (GedaLabel         *label);
static void geda_label_clear_select_info          (GedaLabel         *label);
static void geda_label_update_cursor              (GedaLabel         *label);
static void geda_label_clear_layout               (GedaLabel         *label);
static void geda_label_ensure_layout              (GedaLabel         *label);
static void geda_label_select_region_index        (GedaLabel         *label,
                                                   int                anchor_index,
                                                   int                end_index);

static bool geda_label_mnemonic_activate          (GtkWidget         *widget,
                                                   bool               group_cycling);
static void geda_label_setup_mnemonic             (GedaLabel         *label,
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
static void label_mnemonics_visible_changed       (GtkWindow         *window,
                                                   GParamSpec        *pspec,
                                                   void              *data);
static void label_shortcut_setting_changed        (GtkSettings       *settings);
static bool separate_uline_pattern                (const char        *str,
                                                   unsigned int      *accel_key,
                                                   char             **new_str,
                                                   char             **pattern);


/* For selectable labels: */
static void geda_label_move_cursor                (GedaLabel         *label,
                                                   GtkMovementStep    step,
                                                   int                count,
                                                   bool               extend_selection);
static void geda_label_copy_clipboard             (GedaLabel         *label);
static void geda_label_select_all                 (GedaLabel         *label);
static void geda_label_do_popup                   (GedaLabel         *label,
                                                   GdkEventButton    *event);
static int geda_label_move_forward_word           (GedaLabel         *label,
                                                   int                start);
static int geda_label_move_backward_word          (GedaLabel         *label,
                                                   int                start);

/* For links: */
static void geda_label_clear_links                (GedaLabel         *label);
static bool geda_label_activate_link              (GedaLabel         *label,
                                                   const char        *uri);

static void geda_label_activate_current_link      (GedaLabel         *label);
static GedaLabelLink *geda_label_get_current_link (GedaLabel         *label);

static void geda_label_get_link_colors            (GtkWidget         *widget,
                                                   GdkColor          *link_color,
                                                   GdkColor          *visited_link_color);

static void geda_label_emit_activate_link         (GedaLabel         *label,
                                                   GedaLabelLink     *link);

static void *geda_label_parent_class = NULL;

static GtkBuildableIface *buildable_parent_iface = NULL;

/* Table of pointers to GedaLabel instances */
static GHashTable *label_hash_table = NULL;

static void
add_move_binding (GtkBindingSet  *binding_set, unsigned int keyval,
                  unsigned int  modmask,     GtkMovementStep step,
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

  GtkStyle *style;

  style   = geda_get_widget_style ((GtkWidget*)label);

  metrics = pango_context_get_metrics (context, style->font_desc, pango_context_get_language (context));

  char_width  =  pango_font_metrics_get_approximate_char_width (metrics);
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

/*! \internal widget_class->style_set */
static void geda_label_style_set (GtkWidget *widget, GtkStyle *previous_style)
{
  GedaLabel *label = (GedaLabel*)widget;

  /* We have to clear the layout, fonts etc. may have changed */
  geda_label_clear_layout (label);
  label->INVALIDATE_WRAP_WIDTH;

  ((GtkWidgetClass*)geda_label_parent_class)->style_set (widget, previous_style);
}

/* called by: geda_label_expose
 *            window_to_layout_coords
 *            layout_to_window_coords
 *            geda_label_get_layout_offsets
 */
static void get_layout_location (GedaLabel *label, int *xp, int *yp)
{
  GtkAllocation  *allocation;
  GtkMisc        *misc;
  GtkRequisition *requisition;
  GtkWidget      *widget;

  float xalign, yalign;

  int req_width, x, y;
  int xpad, ypad;

  PangoRectangle logical;

  misc   = (GtkMisc*)label;
  widget = (GtkWidget*)label;

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) {
    xalign = geda_get_misc_xalign(misc);
  }
  else {
    xalign = 1.0 - geda_get_misc_xalign(misc);
  }

  pango_layout_get_pixel_extents (label->layout, NULL, &logical);

  xpad = geda_get_misc_xpad(misc);
  ypad = geda_get_misc_ypad(misc);

  requisition = geda_get_widget_requisition(widget);

  if (label->priv->ellipsize || label->width_chars > 0) {

    int width = pango_layout_get_width (label->layout);

    req_width = logical.width;

    if (width != -1) {
      req_width = MIN(PANGO_PIXELS (width), req_width);
    }

    req_width += 2 * xpad;
  }
  else {
    req_width = requisition->width;
  }

  allocation = geda_get_widget_allocation (widget);

  x = floor (allocation->x + xpad + xalign * (allocation->width - req_width));

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) {
    x = MAX (x, allocation->x + xpad);
  }
  else {
    x = MIN (x, allocation->x + allocation->width - xpad);
  }

  x -= logical.x;

  yalign = geda_get_misc_yalign(misc);

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
  if (pango_layout_get_line_count (label->layout) == 1) {
    y = floor (allocation->y + ypad +
        (allocation->height - requisition->height) * yalign);
  }
  else {
    y = floor (allocation->y + ypad +
        MAX (((allocation->height - requisition->height) * yalign), 0));
  }

  if (xp) {
    *xp = x;
  }

  if (yp) {
    *yp = y;
  }
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

/* widget_class->query_tooltip */
static bool geda_label_query_tooltip (GtkWidget  *widget,
                                      int         x,
                                      int         y,
                                      bool        keyboard_tip,
                                      GtkTooltip *tooltip)
{
  GedaLabel     *label;
  SelectionInfo *info;

  label = (GedaLabel*)widget;
  info  = label->priv->select_info;

  if (info && info->links) {

    GList *iter;
    int    index = -1;

    if (keyboard_tip) {

      if (info->selection_anchor == info->selection_end) {
        index = info->selection_anchor;
      }
    }
    else {

      if (!get_layout_index (label, x, y, &index)) {
        index = -1;
      }
    }

    if (index != -1)  {

      for (iter = info->links; iter != NULL; iter = iter->next) {

        GedaLabelLink *link = iter->data;

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

  return ((GtkWidgetClass*)geda_label_parent_class)->
            query_tooltip (widget, x, y, keyboard_tip, tooltip);
}

/*! \internal widget_class->direction_changed */
static void
geda_label_direction_changed (GtkWidget *widget, GtkTextDirection direction)
{
  GedaLabel *label = (GedaLabel*)widget;

  if (label->layout) {
    pango_layout_context_changed (label->layout);
  }

  ((GtkWidgetClass*)geda_label_parent_class)->direction_changed (widget, direction);
}

/* Semi-private function used by gtk widgets inheriting from
 * GtkMisc that takes into account both css padding and border
 * and the padding specified with the GtkMisc properties.
 */
void geda_misc_get_padding_and_border (GtkMisc *misc, GtkBorder *border)
{
  int xpad, ypad;
  float xalign;
  float yalign;

  g_return_if_fail (GTK_IS_MISC (misc));

  gtk_misc_get_padding (misc, &xpad, &ypad);
  border->top    += ypad;
  border->left   += xpad;
  border->bottom += ypad;
  border->right  += xpad;

  gtk_misc_get_alignment (misc, &xalign, &yalign);

  xalign = xalign / 2.0;
  yalign = yalign / 2.0;

  border->top    += yalign;
  border->right  += xalign;
  border->bottom += yalign;
  border->left   += xalign;
}

static void geda_label_update_layout_width (GedaLabel *label)
{
  GedaLabelData *priv;

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
    int            width, height;

    geda_misc_get_padding_and_border ((GtkMisc*)label, &border);

    allocation = geda_get_widget_allocation ((GtkWidget*)label);

    width  = allocation->width - border.left - border.right;
    height = allocation->height - border.top  - border.bottom;

    if (priv->have_transform) {

      PangoContext *context     = gtk_widget_get_pango_context ((GtkWidget*)label);
      const PangoMatrix *matrix = pango_context_get_matrix (context);
      const double dx           = matrix->xx; /* cos (M_PI * angle / 180) */
      const double dy           = matrix->xy; /* sin (M_PI * angle / 180) */

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
        bool   is_vertical;
        int    cy, halve_height;

        x0 = width >> 1;
        y0 = dx ? x0 * dy / dx : G_MAXDOUBLE;

        halve_height = height >> 1;  /* Divide by 2 */

        is_vertical = fabs (y0) > halve_height;

        if (is_vertical) {
          y0 = halve_height;
          x0 = dy ? y0 * dx / dy : G_MAXDOUBLE;
        }

#if HAVE_HYPOT
        length = 2 * hypot (x0, y0);
#else
        length = 2 * sqrt (x0 * x0 + y0 * y0);
#endif

        pango_layout_set_width (label->layout, rint (length * PANGO_SCALE));
        pango_layout_get_pixel_size (label->layout, NULL, &cy);

        x1 = +dy * cy / 2;
        y1 = -dx * cy / 2;

        if (is_vertical) {
          y0 = halve_height + y1 - y0;
          x0 = -y0 * dx/dy;
        }
        else {
          x0 = width / 2 + x1 - x0;
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

static bool pango_attribute_merge_filter (PangoAttribute *attribute, void *list)
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
  if (!label->layout) {

    PangoAlignment alignment; /* pango default this to PANGO_ALIGN_LEFT */
    PangoAttrList *attrs;
    PangoContext  *context;
    GedaLabelData *priv;
    GtkStyle      *style;
    GtkWidget     *widget;
    bool           R2L;

    priv    = label->priv;
    widget  = (GtkWidget*)label;

    context = gtk_widget_get_pango_context (widget);
    style   = geda_get_widget_style(widget);

    /* We Specify our own map to avoid memory leak in FontConfig */
    pango_context_set_font_map (context, label->priv->font_map);

    pango_context_set_font_description (context, style->font_desc);

    if (label->angle != 0.0 && !priv->select_info) {

     /* We rotate the standard singleton PangoContext for the widget, depending
      * on the fact that it's meant pretty much exclusively for our use. */

      PangoMatrix matrix = PANGO_MATRIX_INIT;

      pango_matrix_rotate (&matrix, label->angle);

      pango_context_set_matrix (context, &matrix);

      priv->have_transform = TRUE;
    }
    else {
      if (priv->have_transform) {
        pango_context_set_matrix (context, NULL);
      }
      priv->have_transform = FALSE;
    }

    label->layout = pango_layout_new (context);

    if ( label->text ) {
      pango_layout_set_text (label->layout, label->text, -1);
    }

    if (priv->select_info && priv->select_info->links) {

      GList    *list;
      GdkColor  link_color;
      GdkColor  visited_color;

      geda_label_get_link_colors (widget, &link_color, &visited_color);
      attrs = pango_attr_list_new ();

      for (list = priv->select_info->links; list; list = list->next) {

        PangoAttribute *attribute;
        GedaLabelLink  *link;

        link                   = list->data;
        attribute              = pango_attr_underline_new (TRUE);
        attribute->start_index = link->start;
        attribute->end_index   = link->end;

        pango_attr_list_insert (attrs, attribute);

        if (link->visited) {
          attribute = pango_attr_foreground_new (visited_color.red,
                                                 visited_color.green,
                                                 visited_color.blue);
        }
        else {
          attribute = pango_attr_foreground_new (link_color.red,
                                                 link_color.green,
                                                 link_color.blue);
        }

        attribute->start_index = link->start;
        attribute->end_index   = link->end;

        pango_attr_list_insert (attrs, attribute);
      }
    }
    else if (label->markup_attrs || label->attrs) {
      attrs = pango_attr_list_new ();
    }
    else {
      attrs = NULL;
    }

    if (label->markup_attrs) {
      if (attrs) {
        pango_merge_attribute_list (attrs, label->markup_attrs);
      }
      else {
        attrs = pango_attr_list_ref (label->markup_attrs);
      }
    }

    if (label->attrs) {
      if (attrs) {
        pango_merge_attribute_list (attrs, label->attrs);
      }
      else {
        attrs = pango_attr_list_ref (label->attrs);
      }
    }

    if (attrs) {
      pango_layout_set_attributes (label->layout, attrs);
      pango_attr_list_unref (attrs);
    }

    switch (priv->jtype) {
      case GTK_JUSTIFY_LEFT:
        R2L = gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL;
        alignment = R2L ? PANGO_ALIGN_RIGHT : PANGO_ALIGN_LEFT;
        break;

      case GTK_JUSTIFY_RIGHT:
        R2L = gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL;
        alignment = R2L ? PANGO_ALIGN_LEFT : PANGO_ALIGN_RIGHT;
        break;

      case GTK_JUSTIFY_CENTER:
        alignment = PANGO_ALIGN_CENTER;
        break;

      case GTK_JUSTIFY_FILL:
        R2L = gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL;
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

/** \defgroup GedaLabel-GtkObject GedaLabel GtkObject Virtual Overrides
 *  @{
 */

/*! \internal GtkObjectClass::object_class->destroy */
static void geda_label_destroy (GtkObject *object)
{
  GedaLabel   *label = (GedaLabel*)object;
  GtkSettings *settings;
  GtkWidget   *toplevel;

  gtk_widget_set_app_paintable ((GtkWidget*)label, FALSE);

  settings = gtk_widget_get_settings ((GtkWidget*)object);

  if (GEDA_OBJECT_GET_DATA (settings, "label-short-connected")) {

    g_signal_handlers_disconnect_by_func (settings, label_shortcut_setting_changed, NULL);

    GEDA_OBJECT_SET_DATA (settings, (void*)(long)FALSE, "label-short-connected");
  }

  toplevel = gtk_widget_get_toplevel ((GtkWidget*)label);

  if (GEDA_OBJECT_GET_DATA(toplevel, "label-mnemonics-connected")) {

    g_signal_handlers_disconnect_by_func (toplevel, label_mnemonics_visible_changed, label);

    GEDA_OBJECT_SET_DATA (toplevel, (void*)(long)FALSE, "label-mnemonics-connected");
  }

  geda_label_set_mnemonic_widget (label, NULL);

  ((GtkObjectClass*)geda_label_parent_class)->destroy (object);
}

/** @} endgroup GedaLabel-GtkObject */

/** \defgroup GedaLabel-GObject GedaLabel GObject Virtual Overrides
 *  @{
 */

/*! \internal GObjectClass::gobject_class->dispose */
static void geda_label_dispose (GObject *object)
{
  GedaLabel *label = (GedaLabel*)object;

  if (label->attrs) {
    pango_attr_list_unref (label->attrs);
    label->attrs = NULL;
  }

  if (label->markup_attrs) {
    pango_attr_list_unref (label->markup_attrs);
    label->markup_attrs = NULL;
  }

  if (label->layout) {
    g_object_unref (label->layout);
    label->layout = NULL;
  }

  if (label->priv->font_map) {

#ifndef __MINGW32__

    g_object_unref (label->priv->font_map);

#endif

    label->priv->font_map = NULL;
  }

  if (label->priv->accessible){
    atk_object_set_name (label->priv->accessible, "");
    g_object_unref (label->priv->accessible);
    label->priv->accessible = NULL;
  }

  ((GObjectClass*)geda_label_parent_class)->dispose (object);
}

/*! \internal GObjectClass::gobject_class->finalize */
static void geda_label_finalize (GObject *object)
{
  GedaLabel *label = (GedaLabel*)object;

  geda_label_clear_links (label);

  if (g_hash_table_remove (label_hash_table, object)) {

#ifndef DEBUG_GEDA_LABEL

    if (!g_hash_table_size (label_hash_table)) {
      g_hash_table_destroy (label_hash_table);
      label_hash_table = NULL;
    }

#endif /* DEBUG_GEDA_LABEL */

  }

  GEDA_FREE (label->label);
  GEDA_FREE (label->text);

  GEDA_FREE (label->priv->select_info);

  GEDA_FREE(label->priv);

  ((GObjectClass*)geda_label_parent_class)->finalize (object);
}

/*! \internal GObjectClass::gobject_class->get_property */
static void
geda_label_get_property (GObject *object, unsigned int  prop_id,
                         GValue  *value,  GParamSpec   *pspec)
{
  GedaLabel     *label = (GedaLabel*)object;
  GedaLabelData *priv  = label->priv;

  switch (prop_id) {

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
      g_value_set_int (value, geda_label_get_cursor_position (label));
      break;

    case PROP_SEL_BOUND:
      g_value_set_int (value, geda_label_get_selection_bound (label));
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

/*! \internal GObjectClass::gobject_class->set_property */
static void
geda_label_set_property (GObject *object,     unsigned int  prop_id,
                         const GValue *value, GParamSpec   *pspec)
{
  GedaLabel *label = (GedaLabel*)object;

  switch (prop_id) {

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

/** @} endgroup GedaLabel-GObject */

/*!
 * \brief GedaLabel Class Initializer
 * \par Function Description
 *  This function is called to initialize the class instance.
 *
 * \param [in] class      A GedaLabelClass Object
 * \param [in] class_data GedaLabel structure associated with the class
 */
static void
geda_label_class_init  (void *class, void *class_data)
{
  GParamSpec     *params;
  GObjectClass   *gobject_class;
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkBindingSet  *binding_set;
  GedaLabelClass *label_class;

  gobject_class  = (GObjectClass*)class;
  label_class    = (GedaLabelClass*)class;
  object_class   = (GtkObjectClass*)class;
  widget_class   = (GtkWidgetClass*)class;

  gobject_class->dispose             = geda_label_dispose;
  gobject_class->finalize            = geda_label_finalize;
  gobject_class->set_property        = geda_label_set_property;
  gobject_class->get_property        = geda_label_get_property;

  object_class->destroy              = geda_label_destroy;

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

  label_class->move_cursor           = geda_label_move_cursor;
  label_class->copy_clipboard        = geda_label_copy_clipboard;
  label_class->activate_link         = geda_label_activate_link;

  geda_label_parent_class            = g_type_class_peek_parent (class);

  /* signals */

  GedaType type = geda_label_get_type();

  /**
   * GedaLabel::move-cursor:
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
   * entry: the object which received the signal
   * step: the granularity of the move, as a GtkMovementStep
   * count: the number of step units to move
   * extend_selection: %TRUE if the move should extend the selection
   */

  signals[MOVE_CURSOR] =
  g_signal_new ( "move-cursor",
                   type,
                   G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                   G_STRUCT_OFFSET (GedaLabelClass, move_cursor),
                   NULL, NULL,
                   NULL,
                   G_TYPE_NONE, 3,
                   GTK_TYPE_MOVEMENT_STEP,
                   G_TYPE_INT,
                   G_TYPE_BOOLEAN);
  /*!
   * GedaLabel::copy-clipboard:
   * The copy-clipboard signal is a keybinding signal, which gets
   * emitted to copy the selection to the clipboard.
   *
   * The default binding for this signal is Ctrl-c.
   *
   * label: the object which received the signal
   */
  signals[COPY_CLIPBOARD] =
  g_signal_new ( "copy-clipboard",
                   GEDA_TYPE_LABEL,
                   G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                   G_STRUCT_OFFSET (GedaLabelClass, copy_clipboard),
                   NULL, NULL,
                   geda_marshal_VOID__VOID,
                   G_TYPE_NONE, 0);

  /*!
   * GedaLabel::populate-popup:
   * The populate-popup signal gets emitted before showing the
   * context menu of the label. Note that only selectable labels
   * have context menus.
   *
   * If you need to add items to the context menu, connect
   * to this signal and append your menuitems to the menu.
   *
   * label: The label on which the signal is emitted
   * menu: the menu that is being populated
   */
  signals[POPULATE_POPUP] =
  g_signal_new ("populate-popup",
                   type,
                   G_SIGNAL_RUN_LAST,
                   G_STRUCT_OFFSET (GedaLabelClass, populate_popup),
                   NULL, NULL,
                   geda_marshal_VOID__OBJECT,
                   G_TYPE_NONE, 1,
                   GEDA_TYPE_MENU);

  /*!
   * GedaLabel::activate-current-link:
   * A keybinding signal, which gets emitted when the user activates a
   * link in the label.
   *
   * Applications may also emit the signal with g_signal_emit_by_name()
   * if they need to control activation of URIs programmatically.
   *
   * The default bindings for this signal are all forms of the Enter key.
   *
   * label: The label on which the signal was emitted
   */
  signals[ACTIVATE_CURRENT_LINK] =
  g_signal_new_class_handler ("activate-current-link", type,
                              G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                              G_CALLBACK (geda_label_activate_current_link),
                              NULL, NULL,
                              geda_marshal_VOID__VOID,
                              G_TYPE_NONE, 0);

  /*!
   * GedaLabel::activate-link:
   * The signal which gets emitted to activate a URI.
   * Applications may connect to it to override the default behaviour,
   * which is to call gtk_show_uri().
   *
   * label: The label on which the signal was emitted
   * uri: the URI that is activated
   *
   * retval: %TRUE if the link has been activated
   */
  signals[ACTIVATE_LINK] = g_signal_new ("activate-link", type,
                                         G_SIGNAL_RUN_LAST,
                                         G_STRUCT_OFFSET (GedaLabelClass, activate_link),
                                         NULL, NULL,
                                         geda_marshal_BOOL__POINTER,
                                         G_TYPE_BOOLEAN, 1, G_TYPE_STRING);

  params = g_param_spec_string ("label", _("Label"),
                              _("The text of the label"),
                                "",
                                G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_LABEL, params);

  params =  g_param_spec_boxed ("attributes", _("Attributes"),
                              _("List of style attributes to apply to the text of the label"),
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

  /*!
   * GedaLabel::wrap-mode
   *
   * If line wrapping is on (see the #GedaLabel:wrap property) this controls
   * how the line wrapping is done. The default is %PANGO_WRAP_WORD, which
   * means wrap on word boundaries.
   *
   */
  params = g_param_spec_enum ("wrap-mode", _("Line wrap mode"),
                            _("If wrap is set, controls how line wrapping is done"),
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

  /*!
   * GedaLabel::ellipsize
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
   */
  params = g_param_spec_enum ("ellipsize", _("Ellipsize"),
                            _("The preferred place to break the string, if an ellipsis is needed"),
                               PANGO_TYPE_ELLIPSIZE_MODE,
                               PANGO_ELLIPSIZE_NONE,
                               G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ELLIPSIZE, params);

  /*!
   * GedaLabel::width-chars
   *
   * The desired width of the label, in characters. If this property is set to
   * -1, the width will be calculated automatically.
   *
   * See the section on "text layout" for details of how #GedaLabel:width-chars
   * and #GedaLabel:max-width-chars determine the width of ellipsized and
   * wrapped labels.
   */
  params = g_param_spec_int ("width-chars", _("Width In Characters"),
                           _("The desired width of the label, in characters"),
                             -1,
                              G_MAXINT,
                             -1,
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_WIDTH_CHARS, params);

  /*!
   * GedaLabel::single-line-mode
   *
   * Whether the label is in single line mode. In single line mode,
   * the height of the label does not depend on the actual text, it
   * is always set to ascent + descent of the font. This can be an
   * advantage in situations where resizing the label because of text
   * changes would be distracting, e.g. in a statusbar.
   */
  params = g_param_spec_boolean ("single-line-mode", _("Single-Line Mode"),
                               _("Whether the label is in single-line mode"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_SINGLE_LINE_MODE, params);

  /*!
   * GedaLabel::angle
   *
   * The angle that the baseline of the label makes with the horizontal,
   * in degrees, measured counterclockwise. An angle of 90 reads from
   * from bottom to top, an angle of 270, from top to bottom. Ignored
   * if the label is selectable, wrapped, or ellipsized.
   */
  params = g_param_spec_double ("angle", _("Angle"),
                              _("Angle of rotation of the label"),
                                 0.0,
                                 360.0,
                                 0.0,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ANGLE, params);

  /*!
   * GedaLabel::max-width-chars
   *
   * The desired maximum width of the label, in characters. If this property
   * is set to -1, the width will be calculated automatically.
   *
   * See the section on "text layout" for details of how #GedaLabel:width-chars
   * and #GedaLabel:max-width-chars determine the width of ellipsized and
   * wrapped labels.
   */
  params = g_param_spec_int ("max-width-chars",
                           _("Maximum Width In Characters"),
                           _("The desired maximum width of the label, in characters"),
                             -1,
                              G_MAXINT,
                             -1,
                              G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_MAX_WIDTH, params);

  /*!
   * GedaLabel::track-visited-links
   *
   * Set this property to %TRUE to make the label track which links
   * have been clicked. It will then apply the visited-link-color
   * color, instead of link-color.
   */
  params = g_param_spec_boolean ("track-visited-links",
                               _("Track visited links"),
                               _("Whether visited links should be tracked"),
                                  FALSE,
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
}

/*!
 * \brief Type instance initializer for GedaLabel
 * \par Function Description
 *  Type instance initializer for GedaLabel, initializes a new empty
 *  GedaLabel object.
 *
 * \param [in] instance The GedaLabel structure being initialized,
 * \param [in] g_class  The GedaLabel class we are initializing.
 */
static void
geda_label_instance_init(GTypeInstance *instance, void *g_class)
{
  AtkObject     *accessible;
  GedaLabel     *label;
  GedaLabelData *priv;

  label       = (GedaLabel*)instance;
  label->priv = GEDA_MEM_ALLOC0 (sizeof(GedaLabelData));
  priv        = label->priv;

  gtk_widget_set_has_window    ((GtkWidget*)label, FALSE);
  gtk_widget_set_app_paintable ((GtkWidget*)label, TRUE);
  gtk_widget_set_can_default   ((GtkWidget*)label, FALSE);

  priv->font_map          = pango_cairo_font_map_new ();

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
  priv->track_links       = FALSE;

  priv->mnemonic_keyval   = GDK_KEY_VoidSymbol;
  label->layout           = NULL;
  label->text             = NULL;
  label->attrs            = NULL;

  priv->mnemonic_widget   = NULL;
  priv->mnemonic_window   = NULL;

  priv->mnemonics_visible = TRUE;

  if (!label_hash_table) {
    label_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (label_hash_table, label, instance);

  geda_label_set_text (label, "label");

  accessible = gtk_widget_get_accessible((GtkWidget*)label);

  if (accessible) {
    priv->accessible = g_object_ref(accessible);
    atk_object_set_name (priv->accessible, _("Label"));
  }
}

static void
geda_label_buildable_interface_init (GtkBuildableIface *iface)
{
  buildable_parent_iface  = g_type_interface_peek_parent (iface);

  iface->custom_tag_start = geda_label_buildable_custom_tag_start;
  iface->custom_finished  = geda_label_buildable_custom_finished;
}

/*!
 * \brief Retrieve GedaLabel's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaLabel Type identifier. When
 *  first called, the function registers a #GedaLabel in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaLabel and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaLabel.
 */
GedaType
geda_label_get_type (void)
{
  static volatile GedaType geda_label_type = 0;

  if (g_once_init_enter (&geda_label_type)) {

    static const GTypeInfo info = {
      sizeof(GedaLabelClass),
      NULL,                      /* base_init           */
      NULL,                      /* base_finalize       */
      geda_label_class_init,     /* (GClassInitFunc)    */
      NULL,                      /* class_finalize      */
      NULL,                      /* class_data          */
      sizeof(GedaLabel),
      0,                         /* n_preallocs         */
      geda_label_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaLabel");
    type   = g_type_register_static (GTK_TYPE_MISC, string, &info, 0);

    const GInterfaceInfo interface_info = {
      (GInterfaceInitFunc) geda_label_buildable_interface_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_BUILDABLE, &interface_info);

    g_once_init_leave (&geda_label_type, type);
  }

  return geda_label_type;
}

/*!
 * \brief Check if an object is a GedaLabel
 * \par Function Description
 *  Determines if \a label is valid by verifying \a label
 *  is included in the hash table of GedaLabel objects.
 *
 * \return TRUE if \a label is a valid GedaLabel object
 */
bool
is_a_geda_label (GedaLabel *label)
{
  if ((label != NULL) && (label_hash_table != NULL)) {
    return g_hash_table_lookup(label_hash_table, label) ? TRUE : FALSE;
  }
  return FALSE;
}

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
    case PANGO_ATTR_LANGUAGE:  /* PangoAttrLanguage */
      if ((language = pango_language_from_string (value)))
      {
        attribute = pango_attr_language_new (language);
        g_value_init (&val, G_TYPE_INT);
      }
      break;

    case PANGO_ATTR_STYLE: /* PangoAttrInt */
      if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_STYLE, value, &val, error))
      {
        attribute = pango_attr_style_new (g_value_get_enum (&val));
      }
      break;

    case PANGO_ATTR_WEIGHT:
      if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_WEIGHT, value, &val, error))
      {
        attribute = pango_attr_weight_new (g_value_get_enum (&val));
      }
      break;

    case PANGO_ATTR_VARIANT:
      if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_VARIANT, value, &val, error))
      {
        attribute = pango_attr_variant_new (g_value_get_enum (&val));
      }
      break;

    case PANGO_ATTR_STRETCH:
      if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_STRETCH, value, &val, error))
      {
        attribute = pango_attr_stretch_new (g_value_get_enum (&val));
      }
      break;

    case PANGO_ATTR_UNDERLINE:
      if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_UNDERLINE, value, &val, NULL))
      {
        attribute = pango_attr_underline_new (g_value_get_enum (&val));
      }
      else {
        /* XXX: allow boolean for backwards compat, so ignore error */
        g_value_unset (&val);
        if (gtk_builder_value_from_string_type (builder, G_TYPE_BOOLEAN, value, &val, error))
        {
          attribute = pango_attr_underline_new (g_value_get_boolean (&val));
        }
      }
      break;

    case PANGO_ATTR_STRIKETHROUGH:
      if (gtk_builder_value_from_string_type (builder, G_TYPE_BOOLEAN, value, &val, error))
      {
        attribute = pango_attr_strikethrough_new (g_value_get_boolean (&val));
      }
      break;

    case PANGO_ATTR_GRAVITY:
      if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_GRAVITY, value, &val, error))
      {
        attribute = pango_attr_gravity_new (g_value_get_enum (&val));
      }
      break;

    case PANGO_ATTR_GRAVITY_HINT:
      if (gtk_builder_value_from_string_type (builder, PANGO_TYPE_GRAVITY_HINT, value, &val, error))
      {
        attribute = pango_attr_gravity_hint_new (g_value_get_enum (&val));
      }
      break;

    case PANGO_ATTR_FAMILY: /* PangoAttrString */
      attribute = pango_attr_family_new (value);
      g_value_init (&val, G_TYPE_INT);
      break;

    case PANGO_ATTR_SIZE: /* PangoAttrSize */
      if (gtk_builder_value_from_string_type (builder, G_TYPE_INT,
        value, &val, error))
        attribute = pango_attr_size_new (g_value_get_int (&val));
      break;

    case PANGO_ATTR_ABSOLUTE_SIZE:
      if (gtk_builder_value_from_string_type (builder, G_TYPE_INT,
        value, &val, error))
        attribute = pango_attr_size_new_absolute (g_value_get_int (&val));
      break;

    case PANGO_ATTR_FONT_DESC: /* PangoAttrFontDesc */
      if ((font_desc = pango_font_description_from_string (value)))
      {
        attribute = pango_attr_font_desc_new (font_desc);
        pango_font_description_free (font_desc);
        g_value_init (&val, G_TYPE_INT);
      }
      break;

    case PANGO_ATTR_FOREGROUND: /* PangoAttrColor */
      if (gtk_builder_value_from_string_type (builder, GDK_TYPE_COLOR, value, &val, error))
      {
        color = g_value_get_boxed (&val);
        attribute = pango_attr_foreground_new (color->red, color->green, color->blue);
      }
      break;

    case PANGO_ATTR_BACKGROUND:
      if (gtk_builder_value_from_string_type (builder, GDK_TYPE_COLOR, value, &val, error))
      {
        color = g_value_get_boxed (&val);
        attribute = pango_attr_background_new (color->red, color->green, color->blue);
      }
      break;

    case PANGO_ATTR_UNDERLINE_COLOR:
      if (gtk_builder_value_from_string_type (builder, GDK_TYPE_COLOR, value, &val, error))
      {
        color = g_value_get_boxed (&val);
        attribute = pango_attr_underline_color_new (color->red, color->green, color->blue);
      }
      break;

    case PANGO_ATTR_STRIKETHROUGH_COLOR:
      if (gtk_builder_value_from_string_type (builder, GDK_TYPE_COLOR, value, &val, error))
      {
        color = g_value_get_boxed (&val);
        attribute = pango_attr_strikethrough_color_new (color->red, color->green, color->blue);
      }
      break;

    case PANGO_ATTR_SHAPE: /* PangoAttrShape Unsupported for now */
      break;

    case PANGO_ATTR_SCALE: /* PangoAttrFloat */
      if (gtk_builder_value_from_string_type (builder, G_TYPE_DOUBLE, value, &val, error))
      {
        attribute = pango_attr_scale_new (g_value_get_double (&val));
      }
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
  if (strcmp (element_name, "attribute") == 0) {

    PangoParserData *data     = (PangoParserData*)user_data;
    PangoAttribute  *attr     = NULL;

    const char      *name      = NULL;
    const char      *value     = NULL;
    const char      *start     = NULL;
    const char      *end       = NULL;
    unsigned int     start_val = 0;
    unsigned int     end_val   = G_MAXUINT;
    unsigned int     i;

    GValue val = G_VALUE_INIT;
    int line_number, char_number;

    for (i = 0; names[i]; i++) {

      if (strcmp (names[i], "name") == 0) {
        name = values[i];
      }
      else if (strcmp (names[i], "value") == 0) {
        value = values[i];
      }
      else if (strcmp (names[i], "start") == 0) {
        start = values[i];
      }
      else if (strcmp (names[i], "end") == 0) {
        end = values[i];
      }
      else  {
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

    if (!name || !value) {

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

    if (start) {

      if (!gtk_builder_value_from_string_type (data->builder, G_TYPE_UINT,
        start, &val, error))
        return;
      start_val = g_value_get_uint (&val);
      g_value_unset (&val);
    }

    if (end) {

      if (!gtk_builder_value_from_string_type (data->builder, G_TYPE_UINT,
        end, &val, error))
        return;
      end_val = g_value_get_uint (&val);
      g_value_unset (&val);
    }

    attr = attribute_from_text (data->builder, name, value, error);

    if (attr) {

      attr->start_index = start_val;
      attr->end_index   = end_val;

      if (!data->attrs)
        data->attrs = pango_attr_list_new ();

      pango_attr_list_insert (data->attrs, attr);
    }
  }
  else if (strcmp (element_name, "attributes") == 0) {
    ;
  }
  else {
    fprintf(stderr, "Unsupported tag for GedaLabel: <%s>\n", element_name);
  }
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
    parser_data->object  = g_object_ref (G_OBJECT(buildable));
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
  buildable_parent_iface->custom_finished (buildable, builder, child,
                                           tagname, user_data);

  if (strcmp (tagname, "attributes") == 0) {

    PangoParserData *parser_data = (PangoParserData*)user_data;

    if (parser_data->attrs) {

      geda_label_set_attributes (GEDA_LABEL (buildable), parser_data->attrs);
      pango_attr_list_unref (parser_data->attrs);
    }

    g_object_unref (parser_data->object);
    g_object_unref (parser_data->builder);
    free (parser_data);
  }
}

/*!
 * \brief Create a New Geda Label Object
 * \par Function Description
 *  Creates a new label with the given text string. If \a str is
 *  %NULL an empty label widget is created.
 *
 * \param [in] str The text of the label, can be NULL.
 *
 * \return the new #GedaLabel
 */
GtkWidget *geda_label_new (const char *str)
{
  GedaLabel *label;

  label = g_object_new (GEDA_TYPE_LABEL, NULL);

  if (str && *str) {
    geda_label_set_text (label, str);
  }

  return GTK_WIDGET (label);
}


/*!
 * \brief Create a New Bold Geda Label Object
 * \par Function Description
 *  Creates a new label widget with the text marked up as bold. This allows
 *  applications to have bold labels without having to pass markup characters
 *  to I18N handlers. For example:
 * \code
 * |[
 * GtkWidget *label;
 * label = geda_bold_label_new (_(TranslateMe), 0.5, 0.5);
 * ]|
 * \endcode>
 * \param [in] str The text of the label, which should not be NULL.
 *
 * \return new #GedaLabel with alignment and use-markup property set true.
 */
GtkWidget *geda_bold_label_new (const char *str)
{
  GtkWidget     *widget;
  char          *markup;

  widget = geda_label_new (str);

  markup = g_strdup_printf ("<span weight=\"bold\">%s</span>", str);

  geda_label_set_markup ((GedaLabel*)widget, markup);

  g_free (markup);

  return widget;
}

/*!
 * \brief Create a New Geda Label Object with Mnemonic text
 * \par Function Description
 *  Creates a new #GedaLabel, containing the text in str. The characters
 *  in str are preceded by an underscore are underlined. If a literal
 *  underscore character in a label is needed, use '__' (two underscores).
 *  The first underlined character represents a keyboard accelerator called
 *  a mnemonic. The mnemonic key can be used to activate another widget,
 *  chosen automatically, or explicitly using geda_label_set_mnemonic_widget().
 *
 *  If geda_label_set_mnemonic_widget() is not called, then the first
 *  activatable ancestor of the #GedaLabel will be chosen as the mnemonic
 *  widget. For instance, if the label is inside a button or menu item,
 *  the button or menu item will automatically become the mnemonic widget
 *  and be activated by the mnemonic.
 *
 *  The string should contain the text of the label, with an underscore in
 *  front of the mnemonic character.
 *
 * \param [in] str Mnemonic for new label
 *
 * \return new #GedaLabel
 */
GtkWidget *geda_mnemonic_label_new (const char *str)
{
  GedaLabel *label;

  label = g_object_new (GEDA_TYPE_LABEL, "use-underline", TRUE, NULL);

  if (str && *str) {
    geda_label_set_mnemonic_text (label, str);
  }

  return GTK_WIDGET (label);
}

/*!
 * \brief Create a New Visible Geda Label Object
 * \par Function Description
 *  This is the same as geda_label_new except that the GedaLabel
 *  is set visible.
 *
 * \param [in] str The text of the label, can be NULL.
 *
 * \return new #GedaLabel with visible property set true
 */
GtkWidget *geda_visible_label_new (const char *str)
{
  GtkWidget *label;

  label = geda_label_new (str);

  gtk_widget_show (label);

  return label;
}

/*!
 * \brief Create a New Visible Geda Label Object with Bold text
 * \par Function Description
 *  This is the same as geda_bold_label_new except that the GedaLabel
 *  is set visible.
 *
 * \param [in] str The text of the label, which should not be NULL.
 *
 * \return new #GedaLabel with visible property set true and bold text
 */
GtkWidget *geda_visible_bold_label_new (const char *str)
{
  GtkWidget *label;

  label = geda_bold_label_new (str);

  gtk_widget_show (label);

  return label;
}

/*!
 * \brief Create a New Visible Geda Label Object with Mnemonic
 * \par Function Description
 *  This is the same as geda_mnemonic_label_new except that the
 *  GedaLabel is set visible.
 *
 * \param [in] str The text of the label, can be NULL.
 *
 * \return new #GedaLabel with "visible" and "use-underline" property set.
 */
GtkWidget *geda_visible_mnemonic_label_new (const char *str)
{
  GtkWidget *label;

  label = geda_mnemonic_label_new (str);

  gtk_widget_show (label);

  return label;
}

/*!
 * \brief Create a New Aligned Geda Label Object
 * \par Function Description
 *  This is the same as geda_label_new with alignments.
 *
 * \param [in] str The text of the label, can be NULL.
 * \param [in] x   Horizontal alignment factor, from 0 (left) to 1 (right).
 * \param [in] y   Vertical alignment factor, from 0 (top) to 1 (bottom).
 *
 * \return new #GedaLabel with alignment set.
 */
GtkWidget *geda_aligned_label_new (const char *str, float x, float y)
{
  GtkWidget *label;

  label = geda_label_new (str);

  gtk_misc_set_alignment(GTK_MISC(label), x, y);

  return label;
}

/*!
 * \brief Create a New Aligned Geda Label Object
 * \par Function Description
 *  Creates a new aligned label widget with the text marked up as bold.
 *  This allows applications to have bold label without having to pass
 *  markup characters to I18N handlers. For example:
 * \code
 * |[
 * GtkWidget *label;
 * label = geda_aligned_bold_label_new (_(TranslateMe), 0.5, 0.5);
 * ]|
 * \endcode>
 * \param [in] str The text of the label, which should not be NULL.
 * \param [in] x   Horizontal alignment factor, from 0 (left) to 1 (right).
 * \param [in] y   Vertical alignment factor, from 0 (top) to 1 (bottom).
 *
 * \return new #GedaLabel with alignment and use-markup property set true.
 */
GtkWidget *geda_aligned_bold_label_new (const char *str, float x, float y)
{
  GtkWidget *widget;

  widget = geda_bold_label_new (str);

  gtk_misc_set_alignment(GTK_MISC(widget), x, y);

  return widget;
}

/*!
 * \brief Create a New Aligned Geda Label Object
 * \par Function Description
 *  This is the same as geda_aligned_label_new except that the
 *  GedaLabel is set visible.
 *
 * \param [in] str The text of the label, can be NULL.
 * \param [in] x   Horizontal alignment factor, from 0 (left) to 1 (right).
 * \param [in] y   Vertical alignment factor, from 0 (top) to 1 (bottom).
 *
 * \return new #GedaLabel with alignment and visible property set true.
 */
GtkWidget *geda_aligned_visible_label_new (const char *str, float x, float y)
{
  GtkWidget *label;

  label = geda_label_new (str);

  gtk_misc_set_alignment((GtkMisc*)label, x, y);

  gtk_widget_show (label);

  return label;
}

/*!
 * \brief Create a New Aligned Geda Label Object
 * \par Function Description
 *  This is the same as geda_aligned_bold_label_new except that the
 *  GedaLabel is set visible.
 *
 * \param [in] str The text of the label, which should not be NULL.
 * \param [in] x   Horizontal alignment factor, from 0 (left) to 1 (right).
 * \param [in] y   Vertical alignment factor, from 0 (top) to 1 (bottom).
 *
 * \return new #GedaLabel with alignment and visible property set true.
 */
GtkWidget *geda_aligned_visible_bold_label_new (const char *str, float x, float y)
{
  GtkWidget *label;

  label = geda_bold_label_new (str);

  gtk_misc_set_alignment(GTK_MISC(label), x, y);

  gtk_widget_show (label);

  return label;
}

/*!
 * \brief Create a New Aligned Geda Label Object with Mnemonic
 * \par Function Description
 *  This is the same as geda_mnemonic_label_new with alignment.
 *
 * \param [in] str The text of the label with undescore or can be NULL.
 * \param [in] x   Horizontal alignment factor, from 0 (left) to 1 (right).
 * \param [in] y   Vertical alignment factor, from 0 (top) to 1 (bottom).
 *
 * \return new #GedaLabel with alignment and mnemonic label
 */
GtkWidget *geda_aligned_mnemonic_label_new (const char *str, float x, float y)
{
  GtkWidget *label;

  label = geda_mnemonic_label_new (str);

  gtk_misc_set_alignment(GTK_MISC(label), x, y);

  return label;
}

/*!
 * \brief Create a New Aligned Visible Geda Label Object with Mnemonic
 * \par Function Description
 *  This is the same as geda_mnemonic_label_new with alignment except
 *  that the GedaLabel is set visible..
 *
 * \param [in] str The text of the label with undescore or can be NULL.
 * \param [in] x   Horizontal alignment factor, from 0 (left) to 1 (right).
 * \param [in] y   Vertical alignment factor, from 0 (top) to 1 (bottom).
 *
 * \return new #GedaLabel with alignment and mnemonic label annd visible set.
 */
GtkWidget *geda_aligned_visible_mnemonic_label_new (const char *str,
                                                    float x, float y)
{
  GtkWidget *label;

  label = geda_mnemonic_label_new (str);

  gtk_misc_set_alignment(GTK_MISC(label), x, y);

  gtk_widget_show (label);

  return label;
}

static bool
geda_label_mnemonic_activate (GtkWidget *widget, bool group_cycling)
{
  GedaLabel     *label  = GEDA_LABEL (widget);
  GedaLabelData *priv   = label->priv;
  GtkWidget     *parent;

  if (priv->mnemonic_widget) {
    return gtk_widget_mnemonic_activate (priv->mnemonic_widget, group_cycling);
  }

  /* Try to find the widget to activate by traversing the
   * widget's ancestry.
   */
  parent = gtk_widget_get_parent (widget);

  if (GTK_IS_NOTEBOOK (parent)) {
    return FALSE;
  }

  while (parent) {
    if (gtk_widget_get_can_focus (parent) ||
      (!group_cycling && GTK_WIDGET_GET_CLASS (parent)->activate_signal) ||
      GTK_IS_NOTEBOOK (gtk_widget_get_parent (parent)) ||
      GEDA_IS_MENU_ITEM (parent))
      return gtk_widget_mnemonic_activate (parent, group_cycling);
    parent = gtk_widget_get_parent (parent);
  }

  /* barf if there was nothing to activate */
  fprintf(stderr, "Could not find a target for a mnemonic activation.");
  gtk_widget_error_bell (widget);

  return FALSE;
}

/*! \internal called by geda_label_recalculate */
static void
geda_label_setup_mnemonic (GedaLabel *label, unsigned int last_key)
{
  GedaLabelData *priv;
  GtkWidget     *widget;
  GtkWidget     *mnemonic_menu;

  priv          = label->priv;
  widget        = (GtkWidget*)label;
  mnemonic_menu = GEDA_OBJECT_GET_DATA (label, MNEMONIC_MENU_DATA);

  if (last_key != GDK_KEY_VoidSymbol) {

    if (priv->mnemonic_window) {

      gtk_window_remove_mnemonic  (priv->mnemonic_window, last_key, widget);
      priv->mnemonic_window = NULL;
    }

    if (mnemonic_menu) {
      mnemonic_menu = NULL;
    }
  }

  if (priv->mnemonic_keyval != GDK_KEY_VoidSymbol) {

    GtkWidget *toplevel;

    connect_mnemonics_visible_notify ((GedaLabel*)widget);

    toplevel = gtk_widget_get_toplevel (widget);

    if (gtk_widget_is_toplevel (toplevel)) {

      GtkWidget *menu_shell;

      menu_shell = gtk_widget_get_ancestor (widget, GEDA_TYPE_MENU_SHELL);

      if (menu_shell) {

        /* Not sure if this ever works */
        geda_menu_shell_add_mnemonic ((GedaMenuShell*)menu_shell,
                                                      priv->mnemonic_keyval,
                                                      widget);
        mnemonic_menu = menu_shell;
      }

      if (!GEDA_IS_MENU (menu_shell)) {
        priv->mnemonic_window = (GtkWindow*)toplevel;
      }
    }
  }

  g_object_set_data ((GObject*)label, MNEMONIC_MENU_DATA, mnemonic_menu);
}

static void
label_shortcut_setting_apply (GedaLabel *label)
{
  geda_label_recalculate (label);

  if (GTK_IS_ACCEL_LABEL (label)) {
    gtk_accel_label_refetch ((GtkAccelLabel*)label);
  }
  else {
    GEDA_OBJECT_NOTIFY (label, "label");
  }
}

static void
label_shortcut_setting_traverse_container (GtkWidget *widget, void *data)
{
  if (GEDA_IS_LABEL(widget)) {
    label_shortcut_setting_apply ((GedaLabel*)widget);
  }
  else if (GTK_IS_CONTAINER (widget)) {
    geda_container_forall (widget,
                           label_shortcut_setting_traverse_container, data);
  }
}

static void
label_shortcut_setting_changed (GtkSettings *settings)
{
  GList *list, *iter;

  list = gtk_window_list_toplevels ();

  for (iter = list; iter ; iter = iter->next) {

    GtkWidget *widget = iter->data;

    if (gtk_widget_get_settings (widget) == settings) {
        geda_container_forall (widget,
                               label_shortcut_setting_traverse_container, NULL);
    }
  }

  g_list_free (list);
}

static void
mnemonics_visible_apply (GtkWidget *widget, bool mnemonics_visible)
{
  GedaLabel     *label = GEDA_LABEL (widget);
  GedaLabelData *priv  = label->priv;

  mnemonics_visible = mnemonics_visible != FALSE;

  if (priv->mnemonics_visible != mnemonics_visible) {

      priv->mnemonics_visible = mnemonics_visible;

      geda_label_recalculate (label);
  }
}

static void
label_mnemonics_visible_traverse_container (GtkWidget *widget,
                                            void      *data)
{
  bool mnemonics_visible = (int)(long)data;

  geda_label_set_mnemonics_visible_recursive (widget, mnemonics_visible);
}

static void
label_mnemonics_visible_changed (GtkWindow  *window,
                                 GParamSpec *pspec,
                                 void       *data)
{
  bool mnemonics_visible;

  g_object_get (window, "mnemonics-visible", &mnemonics_visible, NULL);

  geda_container_forall (window,
                         label_mnemonics_visible_traverse_container,
                         (void*)(long) (mnemonics_visible));
}

static void
geda_label_screen_changed (GtkWidget *widget, GdkScreen *old_screen)
{
  GtkSettings *settings;
  bool         shortcuts_connected;

  /* The PangoContext is replaced when the screen changes, so clear the layouts */
  geda_label_clear_layout ((GedaLabel*)widget);

  if (!gtk_widget_has_screen (widget))
    return;

  settings = gtk_widget_get_settings (widget);

  shortcuts_connected =
  (int)(long)GEDA_OBJECT_GET_DATA (settings, "label-short-connected");

  if (!shortcuts_connected) {

      GEDA_SIGNAL_CONNECT (settings, "notify::gtk-enable-mnemonics",
                           G_CALLBACK (label_shortcut_setting_changed),
                           NULL);
      GEDA_SIGNAL_CONNECT (settings, "notify::gtk-enable-accels",
                           G_CALLBACK (label_shortcut_setting_changed),
                           NULL);

      g_object_set_data ((GObject*)settings, "label-short-connected",
                        (void*)(long)TRUE);
  }

  label_shortcut_setting_apply ((GedaLabel*)widget);
}

/* Helper called by geda_label_set_mnemonic_widget */
static void
label_mnemonic_widget_weak_notify (void *data, GObject *where_the_object_was)
{
  GedaLabel     *label = data;
  GedaLabelData *priv  = label->priv;

  priv->mnemonic_widget = NULL;

  GEDA_OBJECT_NOTIFY (label, "mnemonic-widget");
}

static void
geda_label_set_text_internal (GedaLabel *label, char *str)
{
  GedaLabelData *priv = label->priv;
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

  GEDA_OBJECT_NOTIFY (label, "label");
}

static void
geda_label_set_use_markup_internal (GedaLabel *label, bool val)
{
  GedaLabelData *priv = label->priv;

  val = val != FALSE;

  if (priv->use_markup != val) {

    priv->use_markup = val;

    GEDA_OBJECT_NOTIFY (label, "use-markup");

  }
}

static void
geda_label_set_use_underline_internal (GedaLabel *label, bool val)
{
  GedaLabelData *priv = label->priv;

  val = val != FALSE;

  if (priv->use_underline != val) {

    priv->use_underline = val;

    GEDA_OBJECT_NOTIFY (label, "use-underline");
  }
}

/* Calculates text, attrs and mnemonic_keyval from
 * label, use_underline and use_markup
 */
static void geda_label_recalculate (GedaLabel *label)
{
  GedaLabelData *priv = label->priv;
  unsigned int keyval = priv->mnemonic_keyval;

  geda_label_clear_links (label);

  if (priv->use_markup) {
    geda_label_set_markup_internal (label, label->label, priv->use_underline);
  }
  else if (priv->use_underline) {
    geda_label_set_uline_text_internal (label, label->label);
  }
  else {

    if (!priv->pattern_set) {
      if (label->markup_attrs)
        pango_attr_list_unref (label->markup_attrs);
      label->markup_attrs = NULL;
    }
    geda_label_set_text_internal (label, geda_strdup (label->label));
  }

  if (!priv->use_underline) {
    priv->mnemonic_keyval = GDK_KEY_VoidSymbol;
  }

  if (keyval != priv->mnemonic_keyval) {
    geda_label_setup_mnemonic (label, keyval);
    GEDA_OBJECT_NOTIFY (label, "mnemonic-keyval");
  }

  geda_label_clear_layout (label);
  geda_label_clear_select_info (label);
  gtk_widget_queue_resize ((GtkWidget*)label);
}

static void
start_element_handler (GMarkupParseContext *context,
                       const char          *element_name,
                       const char         **attribute_names,
                       const char         **attribute_values,
                       void                *user_data,
                       GError              **error)
{
  GedaLabelData *priv;
  UriParserData *pdata = user_data;

  if (strcmp (element_name, "a") == 0) {

    GedaLabelLink *link;
    const char    *uri = NULL;
    const char    *title = NULL;
    bool           visited = FALSE;

    int line_number;
    int char_number;
    int i;

    g_markup_parse_context_get_position (context, &line_number, &char_number);

    for (i = 0; attribute_names[i] != NULL; i++) {

      const char *attr = attribute_names[i];

      if (strcmp (attr, "href") == 0) {
        uri = attribute_values[i];
      }
      else if (strcmp (attr, "title") == 0) {
        title = attribute_values[i];
      }
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

    link          = GEDA_MEM_ALLOC0(sizeof(GedaLabelLink));
    link->uri     = geda_strdup (uri);
    link->title   = geda_strdup (title);
    link->visited = visited;
    link->start   = pdata->text_len;
    pdata->links  = g_list_prepend (pdata->links, link);
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
end_element_handler (GMarkupParseContext *context,
                     const char          *element_name,
                     void                *user_data,
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
text_handler (GMarkupParseContext *context,
              const char          *text,
              size_t               text_len,
              void                *user_data,
              GError             **error)
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

static inline bool xml_isspace (char c)
{
  return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
}

static void link_free (GedaLabelLink *link)
{
  free (link->uri);
  free (link->title);
  free (link);
}

static void
geda_label_get_link_colors (GtkWidget *widget,
                            GdkColor  *link_color,
                            GdkColor  *visited_link_color)
{
  GdkColor *link, *visited;

  gtk_widget_ensure_style (widget);
  gtk_widget_style_get (widget,
                        "link-color", &link,
                        "visited-link-color", &visited,
                        NULL);
  if (link) {
    *link_color = *link;
    gdk_color_free (link);
  }
  else {
    *link_color = default_link_color;
  }

  if (visited) {
    *visited_link_color = *visited;
    gdk_color_free (visited);
  }
  else {
    *visited_link_color = default_visited_link_color;
  }
}

static bool
parse_uri_markup (GedaLabel *label, const char *str, char **new_str,
                  GList    **links, GError    **error)
{
  GMarkupParseContext *context = NULL;
  const char          *p;
  const char          *end;
  bool  needs_root = TRUE;
  size_t length;
  UriParserData pdata;

  length = strlen (str);
  p      = str;
  end    = str + length;

  pdata.label    = label;
  pdata.links    = NULL;
  pdata.new_str  = g_string_sized_new (length);
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
  *links   = pdata.links;

  return TRUE;

failed:
  g_markup_parse_context_free (context);
  g_string_free (pdata.new_str, TRUE);
  g_list_foreach (pdata.links, (GFunc)link_free, NULL);
  g_list_free (pdata.links);
  pdata.links = NULL;
  return FALSE;
}

/*! \internal called by geda_label_set_markup_internal */
static void geda_label_ensure_has_tooltip (GedaLabel *label)
{
  GList *iter;
  bool   has_tooltip = FALSE;

  for (iter = label->priv->select_info->links; iter; iter = iter->next) {

    GedaLabelLink *link = iter->data;

    if (link->title) {
      has_tooltip = TRUE;
      break;
    }
  }

  gtk_widget_set_has_tooltip ((GtkWidget*)label, has_tooltip);
}

/*! \internal called by geda_label_recalculate */
static void
geda_label_set_markup_internal (GedaLabel  *label,
                                const char *str,
                                bool        with_uline)
{
  GedaLabelData *priv  = label->priv;
  PangoAttrList *attrs = NULL;
  GError        *error = NULL;
  GList         *links = NULL;
  gunichar       accel = 0;     /* Accelerator Character */
  char          *text  = NULL;
  char          *new_str;

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

    g_object_get (gtk_widget_get_settings ((GtkWidget*)label),
                  "gtk-enable-mnemonics", &enable_mnemonics,
                  "gtk-auto-mnemonics", &auto_mnemonics,
                  NULL);

    if (!(enable_mnemonics && priv->mnemonics_visible &&
       (!auto_mnemonics ||
       (gtk_widget_is_sensitive ((GtkWidget*)label) &&
       (!priv->mnemonic_widget ||
      gtk_widget_is_sensitive (priv->mnemonic_widget))))))
    {
      char *tmp;
      char *pattern;
      unsigned int key;

      if (separate_uline_pattern (new_str, &key, &tmp, &pattern)) {
        free (new_str);
        new_str = tmp;
        free (pattern);
      }
    }
  }

  if (!pango_parse_markup (new_str, -1, with_uline ? '_' : 0, &attrs,
                          &text, with_uline ? &accel : NULL, &error))
  {
    fprintf(stderr, "Failed to set text from markup due to error");
    fprintf(stderr, " parsing markup: %s", error->message);
    g_free (new_str);
    g_error_free (error);
    return;
  }

  g_free (new_str);

  if (text) {
    geda_label_set_text_internal (label, text);
  }

  if (attrs) {
    if (label->markup_attrs) {
      pango_attr_list_unref (label->markup_attrs);
    }
    label->markup_attrs = attrs;
  }

  if (accel != 0) {
    priv->mnemonic_keyval = gdk_keyval_to_lower (gdk_unicode_to_keyval (accel));
  }
  else {
    priv->mnemonic_keyval = GDK_KEY_VoidSymbol;
  }
}


/*!
 * \brief geda_label_get_text
 * \par Function Description
 *  Fetches the text from a label widget, as displayed on the
 *  screen. This does not include any embedded underlines
 *  indicating mnemonics or Pango markup. (See geda_label_get_label())
 *
 * \return value the text in the label widget. This is the internal
 *         string used by the label, and must not be modified.
 *
 * \param [in] label The GedaLabel object
 */
const char *geda_label_get_text (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), NULL);

  return label->text;
}

static PangoAttrList *
geda_label_pattern_to_attrs (GedaLabel *label, const char *pattern)
{
  const char *start;
  const char *p = label->text;
  const char *q = pattern;
  PangoAttrList *attrs;

  attrs = pango_attr_list_new ();

  while (1) {

    while (*p && *q && *q != '_') {
      p = g_utf8_next_char (p);
      q++;
    }

    start = p;

    while (*p && *q && *q == '_') {
      p = g_utf8_next_char (p);
      q++;
    }

    if (p > start) {

      PangoAttribute *attr = pango_attr_underline_new (PANGO_UNDERLINE_LOW);

      attr->start_index = start - label->text;
      attr->end_index   = p - label->text;

      pango_attr_list_insert (attrs, attr);
    }
    else
      break;
  }

  return attrs;
}

static void
geda_label_set_pattern_internal (GedaLabel  *label,
                                 const char *pattern,
                                 bool        is_mnemonic)
{
  GedaLabelData *priv = label->priv;
  PangoAttrList *attrs;
  bool enable_mnemonics;
  bool auto_mnemonics;

  if (priv->pattern_set) {
    return;
  }

  if (is_mnemonic) {

    g_object_get (gtk_widget_get_settings ((GtkWidget*)label),
                  "gtk-enable-mnemonics", &enable_mnemonics,
                  "gtk-auto-mnemonics", &auto_mnemonics,
                  NULL);

    if (enable_mnemonics && priv->mnemonics_visible && pattern &&
      (!auto_mnemonics ||
      (gtk_widget_is_sensitive ((GtkWidget*)label) &&
      (!priv->mnemonic_widget ||
      gtk_widget_is_sensitive (priv->mnemonic_widget)))))
    {
      attrs = geda_label_pattern_to_attrs (label, pattern);
    }
    else
    {
      attrs = NULL;
    }
  }
  else {
    attrs = geda_label_pattern_to_attrs (label, pattern);
  }

  if (label->markup_attrs) {
    pango_attr_list_unref (label->markup_attrs);
  }

  label->markup_attrs = attrs;
}

static void
geda_label_clear_layout (GedaLabel *label)
{
  if (label->layout) {
    g_object_unref (label->layout);
    label->layout = NULL;
  }
}

/*! \internal widget_class->size_request
 * If word wrapping is on, then the height requisition can depend
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
  GedaLabel     *label = (GedaLabel*)widget;
  GedaLabelData *priv  = label->priv;

  int width, height;
  int xpad, ypad;

  if (priv->wrap_mode) {
    geda_label_clear_layout (label);
  }

  geda_label_ensure_layout (label);

  height = label->misc.ypad * 2;
  gtk_misc_get_padding ((GtkMisc*)label, &xpad, &ypad);

  width  = xpad * 2;

  if (priv->have_transform) {

    PangoRectangle     rect;
    PangoContext      *context;
    const PangoMatrix *matrix;

    context = pango_layout_get_context (label->layout);
    matrix  = pango_context_get_matrix (context);

    pango_layout_get_extents (label->layout, NULL, &rect);
    pango_matrix_transform_rectangle (matrix, &rect);
    pango_extents_to_pixels (&rect, NULL);

    requisition->width  = width + rect.width;
    requisition->height = height + rect.height;
  }
  else {

    GtkWidgetAuxInfo *aux_info;
    PangoRectangle    logical_rect;

    aux_info = geda_widget_get_aux_info (widget, FALSE);

    pango_layout_get_extents (label->layout, NULL, &logical_rect);

    if ((priv->wrap_mode        || priv->ellipsize ||
         label->width_chars > 0 || label->max_width_chars > 0) &&
         aux_info && aux_info->width > 0)
    {
      width += aux_info->width;
    }
    else if (priv->ellipsize        ||
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

    if (priv->single_line_mode) {

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
  GedaLabel     *label = (GedaLabel*)widget;
  GedaLabelData *priv  = label->priv;

  ((GtkWidgetClass*)geda_label_parent_class)->size_allocate (widget, allocation);

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
  GedaLabelData *priv;
  GtkWidget     *widget;

  priv = label->priv;

  if (!priv->select_info)
    return;

  widget = (GtkWidget*)label;

  if (gtk_widget_get_realized (widget)) {

    GdkCursor *cursor;

    if (gtk_widget_is_sensitive (widget)) {

      GdkDisplay *display;

      display = gtk_widget_get_display (widget);

      if (priv->select_info->active_link)
        cursor = gdk_cursor_new_for_display (display, GDK_HAND2);
      else if (priv->select_info->selectable)
        cursor = gdk_cursor_new_for_display (display, GDK_XTERM);
      else
        cursor = NULL;
    }
    else {
      cursor = NULL;
    }

    gdk_window_set_cursor (priv->select_info->window, cursor);

  }
}

static void
geda_label_state_changed (GtkWidget   *widget, GtkStateType prev_state)
{
  GedaLabel *label = (GedaLabel*)widget;

  if (label->priv->select_info) {

    geda_label_select_region (label, 0, 0);
    geda_label_update_cursor (label);
  }

  if (((GtkWidgetClass*)geda_label_parent_class)->state_changed)
    ((GtkWidgetClass*)geda_label_parent_class)->state_changed (widget, prev_state);
}

static PangoDirection get_cursor_direction (GedaLabel *label)
{
  SelectionInfo *select_info;

  int result;

  select_info = label->priv->select_info;

  if (select_info == NULL) {
    BUG_MSG ("select_info = NULL");
    result = 0;
  }
  else {

    GSList *iter;
    GSList *list;

    geda_label_ensure_layout (label);

    list = pango_layout_get_lines_readonly (label->layout);

    for (iter = list; iter; iter = iter->next) {

      PangoLayoutLine *line = iter->data;

      /* If priv->select_info->selection_end is at the very end of
       * the line, we don't know if the cursor is on this line or
       * the next without looking ahead at the next line. (End
       * of paragraph is different from line break.) But it's
       * definitely in this paragraph, which is good enough
       * to figure out the resolved direction.
       */
      if (line->start_index + line->length >= select_info->selection_end) {
        return line->resolved_dir;
      }
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
  GtkWidget *widget = (GtkWidget*)label;
  GtkTextDirection text_dir;

  if (direction == PANGO_DIRECTION_LTR) {
    text_dir = GTK_TEXT_DIR_LTR;
  }
  else {
    text_dir = GTK_TEXT_DIR_RTL;
  }

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

  widget = (GtkWidget*)label;

  if (gtk_widget_is_drawable (widget)) {

    PangoDirection keymap_direction;
    PangoDirection cursor_direction;
    PangoRectangle strong_pos, weak_pos;
    bool split_cursor;
    PangoRectangle *cursor1 = NULL;
    PangoRectangle *cursor2 = NULL;
    GdkRectangle cursor_location;
    PangoDirection dir1;
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

    if (split_cursor) {

      cursor1 = &strong_pos;

      if (strong_pos.x != weak_pos.x || strong_pos.y != weak_pos.y) {
        dir2 = (cursor_direction == PANGO_DIRECTION_LTR) ?
                                    PANGO_DIRECTION_RTL : PANGO_DIRECTION_LTR;
        cursor2 = &weak_pos;
      }
    }
    else {

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

    if (dir2 != PANGO_DIRECTION_NEUTRAL) {

      cursor_location.x = xoffset + PANGO_PIXELS (cursor2->x);
      cursor_location.y = yoffset + PANGO_PIXELS (cursor2->y);
      cursor_location.width = 0;
      cursor_location.height = PANGO_PIXELS (cursor2->height);

      draw_insertion_cursor (label, &cursor_location, FALSE, dir2, TRUE);
    }
  }
}

static GedaLabelLink *
geda_label_get_focus_link (GedaLabel *label)
{
  SelectionInfo *info = label->priv->select_info;
  GList *list;

  if (!info)
    return NULL;

  if (info->selection_anchor != info->selection_end)
    return NULL;

  for (list = info->links; list; list = list->next) {

      GedaLabelLink *link = list->data;

      if (link->start <= info->selection_anchor &&
          info->selection_anchor <= link->end)
        return link;
  }

  return NULL;
}

static int
geda_label_expose (GtkWidget *widget, GdkEventExpose *event)
{
  GedaLabel     *label = (GedaLabel*)widget;
  SelectionInfo *info  = label->priv->select_info;

  geda_label_ensure_layout (label);

  if (label->text && (*label->text != '\0')) {

    cairo_t      *cr;
    GtkStyle     *style;
    GtkStateType  state;

    int x, y, range[2];

    cr    = gdk_cairo_create (event->window);
    style = widget->style;
    state = gtk_widget_get_state (widget);

    get_layout_location (label, &x, &y);

    gtk_paint_layout (style, widget->window, state, FALSE, &event->area,
                      widget, "label", x, y, label->layout);

    if (info && (info->selection_anchor != info->selection_end)) {

      GdkRegion *clip;

      range[0] = info->selection_anchor;
      range[1] = info->selection_end;

      if (range[0] > range[1]) {

        int tmp  = range[0];

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
      GdkRegion     *clip;
      GdkRectangle   rect;
      GdkColor       link_color;
      GdkColor       visited_link_color;

      if (info->selectable &&  gtk_widget_has_focus (widget)) {
        geda_label_draw_cursor (label, x, y);
      }

      focus_link = geda_label_get_focus_link (label);
      active_link = info->active_link;

      if (active_link) {

        GdkColor *base_color;
        GdkColor *text_color;

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

        gtk_paint_focus (widget->style, widget->window, state,
                         &event->area, widget, "label",
                         rect.x, rect.y, rect.width, rect.height);
        gdk_region_destroy (clip);
      }
    }
    cairo_destroy(cr);
  }
  return FALSE;
}

/*! \internal called by:
 *   geda_label_set_markup_internal
 *   geda_label_set_uline_text_internal
 */
static bool
separate_uline_pattern (const char  *str, unsigned int *accel_key,
                        char       **new_str,
                        char       **pattern)
{
  const char *src;
  char       *dest;
  char       *pattern_dest;
  bool        underscore;

  *accel_key = GDK_KEY_VoidSymbol;
  *new_str   = g_malloc ((sizeof(char) * strlen (str)) + 1);
  *pattern   = g_malloc ((sizeof(char) * g_utf8_strlen (str, -1)) + 1);

  src          =  str;
  dest         = *new_str;
  pattern_dest = *pattern;
  underscore   =  FALSE;

  while (*src) {

    gunichar c;
    const char *next_src;

    c = g_utf8_get_char (src);
    if (c == (gunichar) -1) {
      fprintf(stderr, "%s: Invalid input string", __func__);
      g_free (*new_str);
      g_free (*pattern);
      return FALSE;
    }
    next_src = g_utf8_next_char (src);

    if (underscore) {

      if (c == '_') {
        *pattern_dest++ = ' ';
      }
      else {

        *pattern_dest++ = '_';

        if (*accel_key == GDK_KEY_VoidSymbol) {
          *accel_key = gdk_keyval_to_lower (gdk_unicode_to_keyval (c));
        }
      }

      while (src < next_src) {
        *dest++ = *src++;
      }

      underscore = FALSE;
    }
    else {

      if (c == '_') {
        underscore = TRUE;
        src = next_src;
      }
      else {

        while (src < next_src) {
          *dest++ = *src++;
        }

        *pattern_dest++ = ' ';
      }
    }
  }

  /* Add termination character */
  *dest = 0;
  *pattern_dest = 0;

  return TRUE;
}

/*! \internal called by geda_label_recalculate */
static void
geda_label_set_uline_text_internal (GedaLabel *label, const char *str)
{
  if (str != NULL) {

    unsigned int accel_key;
    char *new_str;
    char *pattern;

    /* Split text into the base text and a separate pattern
     * of underscores.
     */
    if (separate_uline_pattern (str, &accel_key, &new_str, &pattern)) {


      geda_label_set_text_internal (label, new_str);
      geda_label_set_pattern_internal (label, pattern, TRUE);
      label->priv->mnemonic_keyval = accel_key;

      g_free (pattern);
    }
  }
}

/*! \internal widget_class->realize */
static void geda_label_realize (GtkWidget *widget)
{
  GedaLabel *label = (GedaLabel*)widget;

  ((GtkWidgetClass*)geda_label_parent_class)->realize (widget);

  if (label->priv->select_info) {
    geda_label_create_window (label);
  }
}

/*! \internal widget_class->unrealize */
static void geda_label_unrealize (GtkWidget *widget)
{
  GedaLabel *label = (GedaLabel*)widget;

  if (label->priv->select_info) {
    geda_label_destroy_window (label);
  }

  ((GtkWidgetClass*)geda_label_parent_class)->unrealize (widget);
}

/*! \internal widget_class->map */
static void geda_label_map (GtkWidget *widget)
{
  GedaLabel *label = (GedaLabel*)widget;

  ((GtkWidgetClass*)geda_label_parent_class)->map (widget);

  if (label->priv->select_info) {
    gdk_window_show (label->priv->select_info->window);
  }
}

/*! \internal widget_class->unmap */
static void geda_label_unmap (GtkWidget *widget)
{
  GedaLabel *label = (GedaLabel*)widget;

  if (label->priv->select_info) {
    gdk_window_hide (label->priv->select_info->window);
  }

  ((GtkWidgetClass*)geda_label_parent_class)->unmap (widget);
}

/* Selection this is out of place */
static void geda_label_select_word (GedaLabel *label)
{
  SelectionInfo *info;
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

/*! \internal  widget_class->grab_focus */
static void geda_label_grab_focus (GtkWidget *widget)
{
  GedaLabel     *label;
  GedaLabelData *priv;
  GedaLabelLink *link;

  label = (GedaLabel*)widget;
  priv  = label->priv;

  if (priv->select_info == NULL)
    return;

  ((GtkWidgetClass*)geda_label_parent_class)->grab_focus (widget);

  if ( !priv->select_info->selectable &&
        priv->select_info->links && !priv->in_click) {

    link = priv->select_info->links->data;
    priv->select_info->selection_anchor = link->start;
    priv->select_info->selection_end = link->start;
  }
}

/*! \internal  widget_class->focus */
static bool
geda_label_focus (GtkWidget *widget, GtkDirectionType direction)
{
  GedaLabel     *label;
  SelectionInfo *info;
  GedaLabelLink *focus_link;
  GList         *iter;

  label = (GedaLabel*)widget;
  info  = label->priv->select_info;

  if (!gtk_widget_is_focus (widget)) {

    gtk_widget_grab_focus (widget);

    if (info) {

      focus_link = geda_label_get_focus_link (label);

      if (focus_link && direction == GTK_DIR_TAB_BACKWARD) {

        iter                   = g_list_last (info->links);
        focus_link             = iter->data;
        info->selection_anchor = focus_link->start;
        info->selection_end    = focus_link->start;

      }
    }

    return TRUE;
  }

  if (!info)
    return FALSE;

  if (info->selectable && info->selection_anchor == info->selection_end) {

    int index = info->selection_anchor;

    if (direction == GTK_DIR_TAB_FORWARD) {

      for (iter = info->links; iter; iter = iter->next) {

        GedaLabelLink *link = iter->data;

        if (link->start > index) {

          geda_label_select_region_index (label, link->start, link->start);
          return TRUE;
        }
      }
    }
    else if (direction == GTK_DIR_TAB_BACKWARD) {

       for (iter = g_list_last (info->links); iter; iter = iter->prev) {

        GedaLabelLink *link = iter->data;

        if (link->end < index) {

          geda_label_select_region_index (label, link->start, link->start);
          return TRUE;
        }
      }
    }
  }
  else {

    bool queue = TRUE;
    focus_link = geda_label_get_focus_link (label);

    switch (direction) {

      case GTK_DIR_TAB_FORWARD:
        if (focus_link) {

          iter = g_list_find (info->links, focus_link);
          iter = iter->next;

        }
        else {
          iter = info->links;
        }
        break;

      case GTK_DIR_TAB_BACKWARD:
        if (focus_link) {
          iter = g_list_find (info->links, focus_link);
          iter = iter->prev;
        }
        else {
          iter = g_list_last (info->links);
        }
        break;

      default:
        queue = FALSE;
    }

    if (queue) {

      focus_link             = iter->data;
      info->selection_anchor = focus_link->start;
      info->selection_end    = focus_link->start;

      gtk_widget_queue_draw (widget);

      return TRUE;
    }
  }

  return FALSE;
}

static bool
geda_event_triggers_context_menu (GdkEventButton *event)
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
  GedaLabel     *label;
  SelectionInfo *info;
  int index = 0;

  label = GEDA_LABEL (widget);
  info  = label->priv->select_info;

  if (info != NULL) {

    bool triggers_menu = geda_event_triggers_context_menu (event);

    if (info->active_link) {

      if (triggers_menu) {

        info->link_clicked = 1;
        geda_label_do_popup (label, event);
        return TRUE;
      }
      else if (event->button == GDK_BUTTON_PRIMARY) {

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
    else if (event->button == GDK_BUTTON_PRIMARY) {

      int min, max;

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
        if (event->type == GDK_3BUTTON_PRESS) {
          geda_label_select_region_index (label, 0, strlen (label->text));
        }
        else if (event->type == GDK_2BUTTON_PRESS) {
          geda_label_select_word (label);
        }
        else if (min < max && min <= index && index <= max) {
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
  GedaLabel     *label;
  SelectionInfo *info;
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
      geda_label_emit_activate_link (label, info->active_link);
      info->link_clicked = 0;
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

  toplevel = gtk_widget_get_toplevel ((GtkWidget*)label);

  if (!GTK_IS_WINDOW (toplevel))
    return;

  /* always set up this widgets initial value */
  label->priv->mnemonics_visible =
    gtk_window_get_mnemonics_visible ((GtkWindow*)toplevel);

  connected = (int)(long)
  GEDA_OBJECT_GET_DATA (toplevel, "label-mnemonics-connected");

  if (!connected) {

    g_signal_connect (toplevel,
                      "notify::mnemonics-visible",
                      G_CALLBACK (label_mnemonics_visible_changed),
                      label);
    g_object_set_data ((GObject*)toplevel,
                       "label-mnemonics-connected",
                       (void*)(long) (1));
  }
}

static void
append_n_lines (GString *str, const char *text, GSList *lines, int n_lines)
{
  int i;

  for (i = 0; i < n_lines; i++) {

    PangoLayoutLine *line = lines->data;

    g_string_append_len (str, &text[line->start_index], line->length);
    lines = lines->next;
  }
}

static void limit_layout_lines (PangoLayout *layout)
{
  int n_lines  = pango_layout_get_line_count (layout);

  if (n_lines >= DRAG_ICON_MAX_LINES) {

    const char *text;
    GString    *str;
    GSList     *lines, *elem;

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

/*!
 * \brief GedaLabel label widget create_drag_icon
 * \par Function Description
 *  Creates a drag and drop icon from \a text.
 *
 *  \param [in]  widget GtkWidget to extract the pango context
 *  \param [in]  text   a char to render the icon
 *  \param [in]  len    length of \a text, or -1 for NUL-terminated text
 *
 * \returns: a GdkPixmap to use as DND icon
 *
 * \todo consider relocating this function
 */
static GdkPixmap *
geda_label_create_drag_icon (GtkWidget *widget, char *text, unsigned int len)
{
  GdkDrawable  *drawable = NULL;
  PangoContext *context;
  PangoLayout  *layout;
  cairo_t      *cr;
  int           pixmap_height, pixmap_width;
  int           layout_width, layout_height;

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

  /* get layout extents again, which may have changed */
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

static void drag_begin_cb (GtkWidget *widget, GdkDragContext *context, void *data)
{
  GedaLabel     *label  = (GedaLabel*)widget;
  GedaLabelData *priv   = label->priv;
  GdkPixmap     *pixmap = NULL;

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

/*! \internal  widget_class->motion_notify_event */
static bool geda_label_motion (GtkWidget *widget, GdkEventMotion *event)
{
  GedaLabel     *label;
  SelectionInfo *info;
  int index;

  label = (GedaLabel*)widget;
  info  = label->priv->select_info;

  if (info == NULL)
    return FALSE;

  if (info->links && !info->in_drag) {

    GedaLabelLink *link;
    bool found = FALSE;

    if (info->selection_anchor == info->selection_end)  {

      if (get_layout_index (label, event->x, event->y, &index)) {

        GList *iter;

        for (iter = info->links; iter != NULL; iter = iter->next) {

          link = iter->data;

          if (index >= link->start && index <= link->end) {
            found = TRUE;
            break;
          }
        }
      }
    }

    if (found)  {

      if (info->active_link != link) {

        info->link_clicked = 0;
        info->active_link  = link;

        geda_label_update_cursor (label);
        gtk_widget_queue_draw (widget);
      }
    }
    else {

      if (info->active_link != NULL) {

        info->link_clicked = 0;
        info->active_link  = NULL;

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

/*! \internal  widget_class->grab_focus */
static bool geda_label_leave_notify (GtkWidget *widget, GdkEventCrossing *event)
{
  GedaLabel *label = (GedaLabel*)widget;

  if (label->priv->select_info) {

    label->priv->select_info->active_link = NULL;
    geda_label_update_cursor (label);
    gtk_widget_queue_draw (widget);
  }

  if (((GtkWidgetClass*)geda_label_parent_class)->leave_notify_event) {
    return ((GtkWidgetClass*)geda_label_parent_class)->leave_notify_event (widget, event);
  }
  return FALSE;
}

/*! \internal Helper for geda_label_clear_select_info */
static void geda_label_destroy_window (GedaLabel *label)
{
  if (label->priv->select_info == NULL) {
    BUG_MSG ("select_info = NULL");
  }
  else {

    SelectionInfo *info;
    GdkWindow     *window;

    info = label->priv->select_info;

    window = geda_get_widget_window(info);

    if (window == NULL)
      return;

    gdk_window_set_user_data (window, NULL);
    gdk_window_destroy (window);
    info->window = NULL;
  }
}

/*! \internal Helper for:
 *
 *     geda_label_ensure_select_info
 *     geda_label_realize
 */
static void geda_label_create_window (GedaLabel *label)
{
  GedaLabelData *priv = label->priv;

  if (priv->select_info == NULL) {
    BUG_MSG ("select_info = NULL");
  }
  else {

    GtkAllocation *allocation;
    GdkWindowAttr  attributes;
    GtkWidget     *widget;
    int            attributes_mask;

    if (priv->select_info->window)
      return;

    widget = (GtkWidget*)label;

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
    gdk_window_new (geda_get_widget_window (widget), &attributes, attributes_mask);

    gdk_window_set_user_data (priv->select_info->window, widget);
  }
}

/*! \internal Helper for geda_label_set_selectable */
static bool geda_label_ensure_select_info (GedaLabel *label)
{
  GedaLabelData *priv = label->priv;

  if (priv->select_info == NULL) {

    priv->select_info = g_malloc0 (sizeof(SelectionInfo));

    gtk_widget_set_can_focus ((GtkWidget*)label, TRUE);

    if (gtk_widget_get_realized ((GtkWidget*)label)) {
      geda_label_create_window (label);
    }

    if (gtk_widget_get_mapped ((GtkWidget*)label)) {

      GdkWindow *window;

      window = geda_get_widget_window(priv->select_info);

      gdk_window_show (window);
    }
  }

  return ( priv->select_info ? TRUE : FALSE );
}

static void geda_label_clear_select_info (GedaLabel *label)
{
  if (label->priv->select_info != NULL) {

    if (!label->priv->select_info->selectable &&
        !label->priv->select_info->links)
    {
      geda_label_destroy_window (label);
      g_free (label->priv->select_info);
      label->priv->select_info = NULL;
      gtk_widget_set_can_focus ((GtkWidget*)label, FALSE);
    }
  }
}

static void geda_label_set_selection_text (GedaLabel *label,
                                           GtkSelectionData *selection_data)
{
  SelectionInfo *info = label->priv->select_info;

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

static void geda_label_drag_data_get (GtkWidget        *widget,
                                      GdkDragContext   *context,
                                      GtkSelectionData *selection_data,
                                      unsigned int      info,
                                      unsigned int      time)
{
  geda_label_set_selection_text (GEDA_LABEL (widget), selection_data);
}

static void get_text_callback (GtkClipboard     *clipboard,
                               GtkSelectionData *selection_data,
                               unsigned int      info,
                               void             *user_data_or_owner)
{
  geda_label_set_selection_text (GEDA_LABEL(user_data_or_owner), selection_data);
}

static void clear_text_callback (GtkClipboard *clipboard, void *user_data_or_owner)
{
  GedaLabel     *label;
  SelectionInfo *info;

  label = GEDA_LABEL (user_data_or_owner);
  info  = label->priv->select_info;

  if (info) {
    info->selection_anchor = info->selection_end;
    gtk_widget_queue_draw ((GtkWidget*)label);
  }
}

static void geda_label_select_region_index (GedaLabel *label,
                                            int anchor_index,
                                            int end_index)
{
  GedaLabelData *priv;

  g_return_if_fail (GEDA_IS_LABEL(label));

  priv = label->priv;

  if (priv->select_info && priv->select_info->selectable) {

    GtkClipboard *clipboard;
    GObject      *gobject;

    gobject = (GObject*)label;

    if (priv->select_info->selection_anchor == anchor_index &&
      priv->select_info->selection_end    == end_index)
      return;

    g_object_freeze_notify (gobject);

    if (priv->select_info->selection_anchor != anchor_index)
      g_object_notify (gobject, "selection-bound");

    if (priv->select_info->selection_end != end_index)
      g_object_notify (gobject, "cursor-position");

    priv->select_info->selection_anchor = anchor_index;
    priv->select_info->selection_end    = end_index;

    if (gtk_widget_has_screen ((GtkWidget*)label)) {
      clipboard = gtk_widget_get_clipboard ((GtkWidget*)label,
                                            GDK_SELECTION_PRIMARY);
    }
    else {
      clipboard = NULL;
    }

    if (anchor_index != end_index) {

      GtkTargetList  *list;
      GtkTargetEntry *targets;
      int             n_targets;

      list = gtk_target_list_new (NULL, 0);
      gtk_target_list_add_text_targets (list, 0);
      targets = gtk_target_table_new_from_list (list, &n_targets);

      if (clipboard) {
        gtk_clipboard_set_with_owner (clipboard,
                                      targets, n_targets,
                                      get_text_callback,
                                      clear_text_callback,
                                      gobject);
      }
      gtk_target_table_free (targets, n_targets);
      gtk_target_list_unref (list);
    }
    else if (clipboard && gtk_clipboard_get_owner (clipboard) == gobject)
    {
      gtk_clipboard_clear (clipboard);
    }

    gtk_widget_queue_draw ((GtkWidget*)label);

    g_object_thaw_notify (gobject);
  }
}

static int geda_label_move_logically (GedaLabel *label, int start, int count)
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

static int geda_label_move_visually (GedaLabel *label, int start, int count)
{
  int  index;
  bool split_cursor;
  bool strong;

  GtkSettings *split;
  GtkWidget   *widget;

  index  = start;
  widget = (GtkWidget*)label;

  split = gtk_widget_get_settings (widget);

  g_object_get (split, "gtk-split-cursor", &split_cursor, NULL);

  while (count != 0) {

    int new_index, new_trailing;

    geda_label_ensure_layout (label);

    if (split_cursor) {
      strong = TRUE;
    }
    else {

      GdkKeymap *keymap;

      keymap = gdk_keymap_get_for_display (gtk_widget_get_display (widget));
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

static int geda_label_move_forward_word (GedaLabel *label, int start)
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

/*! \internal
 *  Computes the X position for an offset that corresponds to
 *  the "more important cursor position for that offset. This is used
 *  when trying to guess which end of the selection the cursor should
 *  go to when the user hits a left or right arrow key.
 */
static void get_better_cursor (GedaLabel *label, int index, int *x, int *y)
{
  GdkKeymap *keymap = gdk_keymap_get_for_display (gtk_widget_get_display ((GtkWidget*)label));
  PangoDirection keymap_direction = gdk_keymap_get_direction (keymap);
  PangoDirection cursor_direction = get_cursor_direction (label);
  bool split_cursor;
  PangoRectangle strong_pos, weak_pos;

  g_object_get (gtk_widget_get_settings ((GtkWidget*)label),
                "gtk-split-cursor", &split_cursor, NULL);

  geda_label_ensure_layout (label);

  pango_layout_get_cursor_pos (label->layout, index, &strong_pos, &weak_pos);

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

static bool movement_visual (GedaLabel *label, SelectionInfo *info)
{
  int  end_x, end_y;
  int  anchor_x, anchor_y;

  get_better_cursor (label, info->selection_end, &end_x, &end_y);
  get_better_cursor (label, info->selection_anchor, &anchor_x, &anchor_y);

  return (end_y < anchor_y) || (end_y == anchor_y && end_x < anchor_x);
}

static void geda_label_move_cursor (GedaLabel *label, GtkMovementStep step,
                                    int        count, bool extend_selection)
{
  SelectionInfo *info;

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
        bool end_is_left = movement_visual(label, info);

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
            success = gtk_widget_keynav_failed ((GtkWidget*)label, direct);

            if (!success) {

              GtkWidget *toplevel = gtk_widget_get_toplevel ((GtkWidget*)label);

              if (toplevel) {
                gtk_widget_child_focus (toplevel,
                                        count > 0 ?
                                        GTK_DIR_RIGHT : GTK_DIR_LEFT);
              }
            }
          }
          else {
            gtk_widget_error_bell ((GtkWidget*)label);
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
        if (new_pos == old_pos) {
          gtk_widget_error_bell ((GtkWidget*)label);
        }
        break;

      case GTK_MOVEMENT_DISPLAY_LINE_ENDS:
      case GTK_MOVEMENT_PARAGRAPH_ENDS:
      case GTK_MOVEMENT_BUFFER_ENDS:
        /* FIXME: Can do better here */
        new_pos = count < 0 ? 0 : strlen (label->text);
        if (new_pos == old_pos) {
          gtk_widget_error_bell ((GtkWidget*)label);
        }
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
  SelectionInfo *info = label->priv->select_info;

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
    gtk_widget_get_clipboard ((GtkWidget*)label, GDK_SELECTION_CLIPBOARD);

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
  const char *signal = GEDA_OBJECT_GET_DATA(menuitem, "eda-signal");
  g_signal_emit_by_name (label, signal);
}

static void append_action_signal (GedaLabel  *label,
                                  GedaMenu   *menu,
                                  const char *stock_id,
                                  const char *signal,
                                  bool        sensitive)
{
  GtkWidget *menuitem = geda_image_menu_item_new_from_stock (stock_id, NULL);

  GEDA_OBJECT_SET_DATA (menuitem, signal, "eda-signal");

  g_signal_connect (menuitem, "activate", G_CALLBACK (activate_cb), label);

  gtk_widget_set_sensitive (menuitem, sensitive);

  gtk_widget_show (menuitem);

  geda_menu_append (menu, menuitem);
}

static void popup_menu_detach (GtkWidget *attach_widget, GedaMenu *menu)
{
  GedaLabel     *label = GEDA_LABEL (attach_widget);
  GedaLabelData *priv  = label->priv;

  if (priv->select_info) {
    priv->select_info->popup_menu = NULL;
  }
}

static void
popup_position_func (GedaMenu *menu, int *x, int *y, bool *push_in, void *data)
{
  GtkAllocation *allocation;
  GdkScreen     *screen;
  GtkWidget     *widget;
  GdkWindow     *window;
  GtkRequisition req;

  widget = (GtkWidget*)data; /* = GEDA_LABEL (data) */

  g_return_if_fail (gtk_widget_get_realized (widget));

  allocation = geda_get_widget_allocation (widget);
  screen     = gtk_widget_get_screen (widget);
  window     = geda_get_widget_window (widget);

  gdk_window_get_origin (window, x, y);

  *x += allocation->x;
  *y += allocation->y;

  gtk_widget_size_request ((GtkWidget*)menu, &req);

  *x += allocation->width / 2;
  *y += allocation->height;

  *x = CLAMP (*x, 0, MAX (0, gdk_screen_get_width (screen) - req.width));
  *y = CLAMP (*y, 0, MAX (0, gdk_screen_get_height (screen) - req.height));
}

/* Helper use by:
 *   1) open_link_activate_cb
 *   2) geda_label_get_current_uri
 */
static GedaLabelLink *geda_label_get_current_link (GedaLabel *label)
{
  SelectionInfo *info = label->priv->select_info;
  GedaLabelLink *link;

  if (!info)
    return NULL;

  if (info->link_clicked)
    link = info->active_link;
  else
    link = geda_label_get_focus_link (label);

  return link;
}

static void open_link_activate_cb (GedaMenuItem *menu_item, GedaLabel *label)
{
  GedaLabelLink *link;

  link = geda_label_get_current_link (label);

  if (link) {
    geda_label_emit_activate_link (label, link);
  }
}

static void copy_link_activate_cb (GedaMenuItem *menu_item, GedaLabel *label)
{
  const char *uri = geda_label_get_current_uri (label);

  if (uri) {

    GtkClipboard *clipboard;

    clipboard = gtk_widget_get_clipboard ((GtkWidget*)label, GDK_SELECTION_CLIPBOARD);
    gtk_clipboard_set_text (clipboard, uri, -1);
  }
}

static bool geda_label_popup_menu (GtkWidget *widget)
{
  geda_label_do_popup (GEDA_LABEL (widget), NULL);

  return TRUE;
}

static void geda_label_do_popup (GedaLabel *label, GdkEventButton *event)
{
  SelectionInfo *info = label->priv->select_info;
  GedaLabelLink *link;
  GedaMenu      *menu;
  GtkWidget     *menuitem;
  bool           have_selection;

  if (!info)
    return;

  if (info->popup_menu) {
    gtk_widget_destroy (info->popup_menu);
  }

  info->popup_menu = geda_menu_new ();

  menu = (GedaMenu*)info->popup_menu;

  geda_menu_attach_to_widget (menu, (GtkWidget*)label, popup_menu_detach);

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

    GtkWidget *image;

    /* Open Link */
    menuitem = geda_image_menu_item_new_with_mnemonic (_("_Open Link"));
    gtk_widget_show (menuitem);
    geda_menu_append (menu, menuitem);

    g_signal_connect (menuitem, "activate",
                      G_CALLBACK (open_link_activate_cb), label);

    image = gtk_image_new_from_stock (GTK_STOCK_JUMP_TO, GTK_ICON_SIZE_MENU);
    gtk_widget_show (image);
    geda_image_menu_item_set_image ((GedaImageMenuItem*)menuitem, image);

    /* Copy Link Address */
    menuitem = geda_image_menu_item_new_with_mnemonic (_("Copy _Link Address"));
    gtk_widget_show (menuitem);
    geda_menu_append (menu, menuitem);

    g_signal_connect (menuitem, "activate",
                      G_CALLBACK (copy_link_activate_cb), label);

    image = gtk_image_new_from_stock (GTK_STOCK_COPY, GTK_ICON_SIZE_MENU);
    gtk_widget_show (image);
    geda_image_menu_item_set_image ((GedaImageMenuItem*)menuitem, image);
  }
  else {

    append_action_signal (label, menu, GTK_STOCK_CUT, "cut-clipboard", FALSE);
    append_action_signal (label, menu, GTK_STOCK_COPY, "copy-clipboard", have_selection);
    append_action_signal (label, menu, GTK_STOCK_PASTE, "paste-clipboard", FALSE);

    menuitem = geda_image_menu_item_new_from_stock (GTK_STOCK_DELETE, NULL);
    gtk_widget_set_sensitive (menuitem, FALSE);
    gtk_widget_show (menuitem);
    geda_menu_append (menu, menuitem);

    menuitem = gtk_separator_menu_item_new ();
    gtk_widget_show (menuitem);
    geda_menu_append (menu, menuitem);

    menuitem = geda_image_menu_item_new_from_stock (GTK_STOCK_SELECT_ALL, NULL);
    g_signal_connect_swapped (menuitem, "activate", G_CALLBACK (geda_label_select_all), label);
    gtk_widget_show (menuitem);
    geda_menu_append (menu, menuitem);
  }

  g_signal_emit (label, signals[POPULATE_POPUP], 0, menu);

  if (event) {
    geda_menu_popup (menu, NULL, NULL, NULL, NULL,
                                      event->button, event->time);
  }
  else {
    geda_menu_popup (menu, NULL, NULL, popup_position_func, label,
                                       0, gtk_get_current_event_time ());
    geda_menu_shell_select_first ((GedaMenuShell*)menu, FALSE);
  }
}

static void geda_label_clear_links (GedaLabel *label)
{
  SelectionInfo *info = label->priv->select_info;

  if (info) {

    g_list_foreach (info->links, (GFunc)link_free, NULL);
    g_list_free (info->links);
    info->links = NULL;
    info->active_link = NULL;
  }
}

static bool geda_label_activate_link (GedaLabel *label, const char *uri)
{
  GError *error  = NULL;
  bool    result;

#if HAVE_GTK_SHOW_URI

  GtkWidget *widget = (GtkWidget*)label;

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

static void geda_label_emit_activate_link (GedaLabel *label, GedaLabelLink *link)
{
  GedaLabelData *priv = label->priv;
  bool handled;

  g_signal_emit (label, signals[ACTIVATE_LINK], 0, link->uri, &handled);

  if (handled && priv->track_links && !link->visited) {

    link->visited = TRUE;

    geda_label_clear_layout (label);
  }
}

static void geda_label_activate_current_link (GedaLabel *label)
{
  GedaLabelLink *link = geda_label_get_focus_link (label);

  if (link) {

      geda_label_emit_activate_link (label, link);
  }
  else {

    GtkWidget *toplevel;

    GtkWidget *widget = (GtkWidget*)label;

    toplevel = gtk_widget_get_toplevel (widget);

    if (GTK_IS_WINDOW (toplevel)) {

      GtkWindow *window = (GtkWindow*)toplevel;

      if (window) {

        GtkWidget *default_widget, *focus_widget;

        default_widget = gtk_window_get_default_widget (window);
        focus_widget   = gtk_window_get_focus (window);

        if (default_widget != widget &&
          !(widget == focus_widget   &&
          (!default_widget ||
          !gtk_widget_is_sensitive (default_widget))))
          gtk_window_activate_default (window);
      }
    }
  }
}

/**
 * \defgroup GedaLabelFunctions GedaLabel Public Functions
 * @{
 * \par Begin Public Accessors
 */

/*!
 * \brief Get the GedaLabel Text Alignment
 * \par Function Description
 *  Retrieves the text within the #GedaLabel widget.
 *
 * \param [in]  label  The GedaLabel object
 * \param [out] xalign Pointer to float to store the horizontal alignment
 * \param [out] yalign Pointer to float to store the vertical alignment
 *
 * \sa geda_label_set_alignment
 */
void geda_label_get_alignment (GedaLabel *label, float *xalign, float *yalign)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  gtk_misc_get_alignment ((GtkMisc*)label, xalign, yalign);
}

/*!
 * \brief Set the GedaLabel Text Alignment
 * \par Function Description
 *  Set the text alignment property of the #GedaLabel widget.
 *
 * \param [in] label  The GedaLabel object
 * \param [in] xalign horizontal alignment, from 0 (left) to 1 (right)
 * \param [in] yalign vertical alignment, from 0 (top) to 1 (bottom)
 *
 * \sa geda_label_get_alignment
 */
void geda_label_set_alignment (GedaLabel *label, float xalign, float yalign)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  gtk_misc_set_alignment ((GtkMisc*)label, xalign, yalign);
}

/************************** Angle Property ************************/

/*!
 * \brief Get GedaLabel angle property
 * \par Function Description
 *  Gets the angle of rotation for the label.
 *
 * \param [in] label The GedaLabel object
 *
 * \returns the angle of rotation for the label
 *
 * \sa geda_label_set_angle
 */
double geda_label_get_angle  (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), 0.0);
  return label->angle;
}

/*!
 * \brief set GedaLabel angle property
 * \par Function Description
 *  Sets the angle of rotation for the label. An angle of 90 reads from
 *  from bottom to top, an angle of 270, from top to bottom. The angle
 *  setting for the label is ignored if the label is selectable,
 *  wrapped, or ellipsized.
 *
 *  The angle is from the baseline of the label from horizontal, in
 *  degrees, measured counterclockwise.
 *
 * \param [in] label   The GedaLabel object
 * \param [in] angle   The label angle
 */
void geda_label_set_angle (GedaLabel *label, double angle)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  /* Canonicalize to [0,360]. We don't canonicalize 360 to 0, because
   * double property ranges are inclusive, and changing 360 to 0 would
   * make a property editor behave strangely.
   */
  if (angle < 0 || angle > 360.0) {
    angle = angle - 360. * floor (angle / 360.);
  }

  if (label->angle != angle) {

    label->angle = angle;

    geda_label_clear_layout (label);

    gtk_widget_queue_resize ((GtkWidget*)label);

    GEDA_OBJECT_NOTIFY (label, "angle");
  }
}

/*!
 * \brief Retrieve PangoAttrList for GedaLabel
 * \par Function Description
 * Gets the attribute list that was set on the label using
 * geda_label_set_attributes(), if any. This function does
 * not reflect attributes that come from the labels markup
 * (see geda_label_set_markup()). If you want to get the
 * effective attributes use geda_label_get_effective_attributes.
 *
 * \param [in] label  The GedaLabel object
 *
 * \returns the attribute list, or %NULL if none was set.
 */
PangoAttrList *geda_label_get_attributes (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), NULL);

  return label->attrs;
}

/*!
 * \brief geda_label_set_attributes
 * \par Function Description
 *  Sets a PangoAttrList; the attributes in the list are applied to the
 *  label text.
 *
 * \note The attributes set with this function will be applied and merged
 *  with any other attributes previously effected by way of the #GedaLabel:
 *  use-underline or #GedaLabel:use-markup properties. While it is not
 *  recommended to mix markup strings with manually set attributes, if you
 *  must; know that the attributes will be applied to the label after the
 *  markup string is parsed.
 *
 * \param [in] label  The GedaLabel object
 * \param [in] attrs  PangoAttrList structure
 */
void geda_label_set_attributes (GedaLabel *label, PangoAttrList *attrs)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  if (attrs) {
    pango_attr_list_ref (attrs);
  }

  if (label->attrs) {
    pango_attr_list_unref (label->attrs);
  }

  label->attrs = attrs;

  geda_label_clear_layout (label);

  gtk_widget_queue_resize ((GtkWidget*)label);

  GEDA_OBJECT_NOTIFY (label, "attributes");
}

/*!
 * \brief geda_label_get_current_uri
 * \par Function Description
 *  Returns the URI for the currently active link in the label.
 *  The active link is the one under the mouse pointer or, in a
 *  selectable label, the link in which the text cursor is currently
 *  positioned.
 *
 *  This function is intended for use in a GedaLabel::activate-link
 *  handler or for use in a GtkWidget::query-tooltip handler.
 *
 * \param [in] label        The GedaLabel object
 *
 * \returns the currently active URI. The string is owned by GTK+ and
 *          must not be freed or modified.
 */
const char *geda_label_get_current_uri (GedaLabel *label)
{
  if (GEDA_IS_LABEL(label)) {

    GedaLabelLink *link;

    link = geda_label_get_current_link (label);

    if (link) {
      return link->uri;
    }
  }
  return NULL;
}

/*!
 * \brief Get cursor position
 * \par Function Description
 *  Returns offset of cursor index position within the label text.
 *
 * \param [in] label The GedaLabel object
 *
 * \returns Offset of text index within the label text
 */
int geda_label_get_cursor_position (GedaLabel *label)
{
  if (GEDA_IS_LABEL(label)) {

    GedaLabelData *priv = label->priv;

    if (priv->select_info && priv->select_info->selectable) {
      return g_utf8_pointer_to_offset (label->text,
                                       label->text + priv->select_info->selection_end);
    }
  }
  return 0;
}

PangoAttrList *geda_label_get_effective_attributes (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), NULL);

  return pango_layout_get_attributes (geda_label_get_layout (label));
}

/************************ Ellipsize Property **********************/

/*!
 * \brief Get the ellipsize property of a GedaLabel
 * \par Function Description
 *  Returns the ellipsizing position of the label. See geda_label_set_ellipsize().
 *
 * \param [in] label  The GedaLabel object
 *
 * \returns PangoEllipsizeMode
 */
PangoEllipsizeMode geda_label_get_ellipsize (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), PANGO_ELLIPSIZE_NONE);

  return label->priv->ellipsize;
}

/*!
 * \brief Set the ellipsize property of a GedaLabel
 * \par Function Description
 *  Sets the mode used to ellipsize (add an ellipsis: "...") to the
 *  text if there is not enough space to render the entire string.
 *
 * \param [in] label  The GedaLabel object
 * \param [in] mode   a PangoEllipsizeMode
 */
void geda_label_set_ellipsize (GedaLabel *label, PangoEllipsizeMode mode)
{
  g_return_if_fail (GEDA_IS_LABEL(label));
  g_return_if_fail (mode >= PANGO_ELLIPSIZE_NONE && mode <= PANGO_ELLIPSIZE_END);

  if ((PangoEllipsizeMode) label->priv->ellipsize != mode) {

    label->priv->ellipsize = mode;

    /* No real need to be this drastic, but easier than duplicating the code */
    geda_label_clear_layout (label);

    gtk_widget_queue_resize ((GtkWidget*)label);

    GEDA_OBJECT_NOTIFY (label, "ellipsize");
  }
}

/********************* Justification Property *********************/

/*!
 * \brief geda_label_get_justify
 * \par Function Description
 *  Returns the justification of the label.
 *
 * \param [in] label The GedaLabel object
 *
 * \returns GtkJustification
 *
 * \sa geda_label_set_alignment
 */
GtkJustification geda_label_get_justify (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), 0);

  return label->priv->jtype;
}

/*!
 * \brief geda_label_set_justify
 * \par Function Description
 *  Sets the alignment of the lines in the text of the label relative to
 *  each other. %GTK_JUSTIFY_LEFT is the default value when the widget
 *  is first created with geda_label_new(). If you instead want to set
 *  the alignment of the label as a whole, use gtk_misc_set_alignment
 *  instead.
 *
 * \note geda_label_set_justify has no effect on labels containing
 *       only a single line.
 *
 * \param [in] label  The GedaLabel object
 * \param [in] jtype  The GedaLabel object
 *
 * \sa geda_label_set_justify
 */
void geda_label_set_justify (GedaLabel *label, GtkJustification jtype)
{
  g_return_if_fail (GEDA_IS_LABEL(label));
  g_return_if_fail (jtype >= GTK_JUSTIFY_LEFT && jtype <= GTK_JUSTIFY_FILL);

  if ((GtkJustification) label->priv->jtype != jtype) {

    label->priv->jtype = jtype;

    /* No real need to be this drastic, but easier than duplicating the code */
    geda_label_clear_layout (label);

    gtk_widget_queue_resize ((GtkWidget*)label);

    GEDA_OBJECT_NOTIFY (label, "justify");
  }
}

/*!
 * \brief geda_label_get_label
 * \par Function Description
 *  Fetches the text from a label widget including any embedded
 *  underlines indicating mnemonics and Pango markup. (See
 *  geda_label_get_text()).
 *
 * \param [in] label  The GedaLabel object
 *
 * \returns the text of the label widget. This string is owned
 *          by the widget and must not be modified or freed.
 */
const char *geda_label_get_label (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), NULL);

  return label->label;
}


/*!
 * \brief geda_label_set_label
 * \par Function Description
 *  Sets the text of the label. The label is interpreted as
 *  including embedded underlines and/or Pango markup depending
 *  on the values of the GedaLabel::use-underline" and
 *  GedaLabel::use-markup properties.
 *
 * \param [in] label  The GedaLabel object
 * \param [in] str    New text to set for the label
 */
void geda_label_set_label (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  g_object_freeze_notify ((GObject*)label);

  geda_label_set_label_internal (label, geda_strdup (str ? str : ""));

  geda_label_recalculate (label);

  g_object_thaw_notify ((GObject*)label);
}

/*!
 * \brief geda_label_get_layout
 * \par Function Description
 *  Gets the PangoLayout used to display the label.
 *  The layout is useful to e.g. convert text positions to
 *  pixel positions, in combination with geda_label_get_layout_offsets().
 *  The returned layout is owned by the label and should not be
 *  freed by the caller. The label is free to recreate its layout at
 *  any time, so it should be considered read-only.
 *
 * \param [in] label  The GedaLabel object
 *
 * \returns the PangoLayout for this label
 */
PangoLayout *geda_label_get_layout (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), NULL);

  geda_label_ensure_layout (label);

  return label->layout;
}

/*!
 * \brief geda_label_get_layout_offsets
 * \par Function Description
 * Obtains the coordinates where the label will draw the PangoLayout
 * representing the text in the label; useful to convert mouse events
 * into coordinates inside the PangoLayout, e.g. to take some action
 * if some part of the label is clicked. Of course you will need to
 * create a GtkEventBox to receive the events, and pack the label
 * inside it, since labels are a GTK_NO_WINDOW widget. Remember
 * when using the PangoLayout functions you need to convert to
 * and from pixels using PANGO_PIXELS() or PANGO_SCALE.
 *
 * \param [in] label  The GedaLabel object
 * \param [out] x     location to store X offset of layout, or %NULL
 * \param [out] y     location to store Y offset of layout, or %NULL
 */
void geda_label_get_layout_offsets (GedaLabel *label, int *x, int *y)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  geda_label_ensure_layout (label);

  get_layout_location (label, x, y);
}

/************************ Line Wrap Property **********************/

/*!
 * \brief geda_label_get_line_wrap
 * \par Function Description
 *  Returns whether lines in the label are automatically wrapped.
 *
 * \param [in] label  The GedaLabel object
 *
 * \retval  %TRUE if the lines of the label are automatically wrapped.
 *
 * \sa geda_label_set_line_wrap
 */
bool geda_label_get_line_wrap (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), FALSE);

  return label->priv->wrap;
}

/*!
 * \brief geda_label_set_line_wrap
 * \par Function Description
 *  Toggles line wrapping within the #GedaLabel widget. %TRUE makes it break
 *  lines if text exceeds the widget's size. %FALSE lets the text get cut off
 *  by the edge of the widget if it exceeds the widget size.
 *
 *  Note that setting line wrapping to %TRUE does not make the label
 *  wrap at its parent container's width, because GTK+ widgets
 *  conceptually can't make their requisition depend on the parent
 *  container's size. For a label that wraps at a specific position,
 *  set the label's width using gtk_widget_set_size_request().
 *
 *  \param [in] label  The GedaLabel object
 *  \param [in] wrap   The desired setting
 */
void geda_label_set_line_wrap (GedaLabel *label, bool wrap)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  wrap = wrap != FALSE;

  if (label->priv->wrap != wrap) {

    label->priv->wrap = wrap;

    geda_label_clear_layout (label);

    gtk_widget_queue_resize ((GtkWidget*)label);

    GEDA_OBJECT_NOTIFY (label, "wrap");
  }
}

/*!
 * \brief geda_label_get_line_wrap_mode
 * \par Function Description
 *  Returns line wrap mode used by the label.
 *
 * \param [in] label  The GedaLabel object
 *
 * \retval %TRUE if the lines of the label are automatically wrapped.
 *
 * \sa geda_label_set_line_wrap_mode
 */
PangoWrapMode geda_label_get_line_wrap_mode (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), FALSE);

  return label->priv->wrap_mode;
}

/*!
 * \brief geda_label_set_line_wrap_mode
 * \par Function Description
 *  If line wrapping is on the this controls how the line wrapping is
 *  performed. The default is %PANGO_WRAP_WORD which means wrap on word
 *  boundaries.
 *
 * \param [in] label     The GedaLabel object
 * \param [in] wrap_mode The line wrap_mode setting
 *
 * \sa geda_label_set_line_wrap
 */
void geda_label_set_line_wrap_mode (GedaLabel *label, PangoWrapMode wrap_mode)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  if (label->priv->wrap_mode != wrap_mode) {

    label->priv->wrap_mode = wrap_mode;

    gtk_widget_queue_resize ((GtkWidget*)label);

    GEDA_OBJECT_NOTIFY (label, "wrap-mode");
  }
}

/*!
 * \brief geda_label_set_markup
 * \par Function Description
 *  Parses str which is marked up with the Pango text markup language, setting
 *  the label's text and attribute list based on the parse results. If the str
 *  is external data, you may need to escape it with g_markup_escape_text() or
 * \code
 * |[
 * char *markup;
 *
 * markup = g_strdup_printf ("<span font=\"14\" color=\"red\"> <b>\tRed: %s</b> </span>", str);
 * geda_label_set_markup (GEDA_LABEL (label), markup);
 * g_free (markup);
 * ]|
 * \endcode>
 *
 * \param [in] label  The GedaLabel object
 * \param [in] str    a markup string
 *
 */
void geda_label_set_markup (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  g_object_freeze_notify ((GObject*)label);

  geda_label_set_label_internal (label, geda_strdup (str ? str : ""));

  geda_label_set_use_markup_internal (label, TRUE);

  geda_label_set_use_underline_internal (label, FALSE);

  geda_label_recalculate (label);

  g_object_thaw_notify ((GObject*)label);
}

/*!
 * \brief geda_label_set_markup_with_mnemonic
 * \par Function Description
 *  Parses str which is marked up with the Pango text markup language,
 *  setting the label's text and attribute list based on the parse results.
 *  If characters in str are preceded by an underscore, they are underlined
 *  indicating that they represent a keyboard accelerator called a mnemonic.
 *
 *  The mnemonic key can be used to activate another widget, chosen
 *  automatically, or explicitly using geda_label_set_mnemonic_widget().
 *
 * \code
 * |[
 * char *markup;
 *
 * markup = g_strdup_printf ("&lt;span style=\"italic\"&gt;%s&lt;/span&gt;", str);
 * geda_label_set_markup_with_mnemonic (GEDA_LABEL (label), markup);
 * g_free (markup);
 * ]|
 * \endcode>
 *
 *  \param [in] label  The GedaLabel object
 *  \param [in] str    The a markup string
 */
void geda_label_set_markup_with_mnemonic (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  g_object_freeze_notify ((GObject*)label);

  geda_label_set_label_internal (label, geda_strdup (str ? str : ""));

  geda_label_set_use_markup_internal (label, TRUE);

  geda_label_set_use_underline_internal (label, TRUE);

  geda_label_recalculate (label);

  g_object_thaw_notify ((GObject*)label);
}

/********************* Max Width Chars Property *******************/

/*!
 * \brief Retrieves the Maximum Width of Characters for a GedaLabel
 * \par Function Description
 *  Retrieves the desired maximum width of label, in characters. See
 *  geda_label_set_width_chars().
 *
 * \param [in] label  The GedaLabel object
 *
 * \returns the maximum width of the label in characters.
 */
int geda_label_get_max_width_chars (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), -1);

  return label->max_width_chars;
}

/*!
 * \brief Set the Maximum Width of Characters of a GedaLabel
 * \par Function Description
 *  Sets the desired maximum width in characters of label to n_chars.
 *
 * \param [in] label    The GedaLabel object
 * \param [in] n_chars  New desired maximum width, in characters.
 */
void geda_label_set_max_width_chars (GedaLabel *label, int n_chars)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  if (label->max_width_chars != n_chars) {

    label->max_width_chars = n_chars;

    gtk_widget_queue_resize ((GtkWidget*)label);

    GEDA_OBJECT_NOTIFY (label, "max-width-chars");
  }
}

/*********************** Mnemonic Properties **********************/

/*!
 * \brief Get the Mnemonic Character from the GedaLabel Text
 * \par Function Description
 *  Retrieves the character after the underscore in the label text.
 *  If an underscore is not present, or if \a label is not a valid
 *  GedaLabel object then this functions returns 0xFF.
 *
 * \param [in] label Pointer to a GedaLabel object
 *
 * \returns mnemonic character or 0xFF.
 */
char geda_label_get_mnemonic_char (GedaLabel *label)
{
  if (GEDA_IS_LABEL(label)) {

    char *str = strstr(label->label, "_");

    if (str) {
      return (char)*(str + 1);
    }
  }

  return 0xFF;
}

/*!
 * \brief geda_label_set_mnemonic_text
 * \par Function Description
 *  Sets the label's text from the string str. If characters in str is
 *  preceded by an underscore, they are underlined indicating that they
 *  represent a keyboard accelerator called a mnemonic. The mnemonic key
 *  can be used to activate another widget, chosen  automatically, or
 *  explicitly using geda_label_set_mnemonic_widget().
 *
 * \param [in] label The GedaLabel object
 * \param [in] str   Pointer to a string
 */
void geda_label_set_mnemonic_text (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL(label));
  g_return_if_fail (str != NULL);

  g_object_freeze_notify ((GObject*)label);

  geda_label_set_label_internal (label, geda_strdup (str ? str : ""));
  geda_label_set_use_markup_internal (label, FALSE);
  geda_label_set_use_underline_internal (label, TRUE);

  geda_label_recalculate (label);

  g_object_thaw_notify ((GObject*)label);
}

/*!
 * \brief geda_label_get_mnemonic_keyval
 * \par Function Description
 *  If the label has been set so that it has an mnemonic key this function
 *  returns the keyval used for the mnemonic accelerator. If there is no
 *  mnemonic set up it returns .
 *
 * \param [in] label   The GedaLabel object
 *
 * \returns GDK keyval usable for accelerators
 */
unsigned int geda_label_get_mnemonic_keyval (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), GDK_KEY_VoidSymbol);

  return label->priv->mnemonic_keyval;
}

/*!
 * \brief Get the Lower case Mnemonic Character
 * \par Function Description
 *  Retrieves the lower case character represented by the key code
 *  value, which could also be the actual mnemonic. For example, if
 *  the label text is "_Gnu", this function returns "g", because the
 *  mnemonic character "G" is (normally) key-code 104, which is the
 *  "g" key plus the SHIFT key modifier.
 *
 * \param [in] label Pointer to a GedaLabel object
 *
 * \returns mnemonic character or 0xFF (GDK_KEY_VoidSymbol).
 */
char geda_label_get_mnemonic_lower (GedaLabel *label)
{
  return (char)geda_label_get_mnemonic_keyval(label);
}

/*!
 * \brief Retrieve the GedaLabel mnemonic_visible property
 * \par Function Description
 *  Retrieves the current mnemonic_visible setting.
 *
 * \param [in] label The GedaLabel object
 *
 * \returns Boolean value of the mnemonic visible setting.
 *
 * \sa geda_label_set_mnemonics_visible_recursive
 */
bool geda_label_get_mnemonic_visible (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), 0);

  return label->priv->mnemonics_visible;
}

/*!
 * \brief Set the GedaLabel mnemonic_visible property
 * \par Function Description
 *  Sets the mnemonic_visible setting of the given \a label.
 *
 * \param [in] label The GedaLabel object
 * \param [in] state Boolean value of the mnemonic visible setting.
 *
 * \sa geda_label_set_mnemonics_visible_recursive
 */
void geda_label_set_mnemonic_visible (GedaLabel *label, bool state)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  mnemonics_visible_apply ((GtkWidget*)label, state);
}

/*!
 * \brief Get the Mnemonic Character from the GedaLabel Text
 * \par Function Description
 *  Applies the visibility setting given by \a mnemonics_visible to
 *  the label widget if \a widget is a label. If the widget object is
 *  a container then the setting is applied to all sub-object of
 *  \a widget.
 *
 * \param [in] widget            Pointer to a GedaLabel object
 * \param [in] mnemonics_visible Visible if TRUE, otherwise FALSE
 *
 * \internal
 * Called by geda_menu_shell_update_mnemonics()
 */
void geda_label_set_mnemonics_visible_recursive (GtkWidget *widget,
                                                 bool       mnemonics_visible)
{
  if (GEDA_IS_LABEL(widget)) {
    mnemonics_visible_apply (widget, mnemonics_visible);
  }
  else if (GTK_IS_CONTAINER (widget)) {
    geda_container_forall (widget,
                           label_mnemonics_visible_traverse_container,
                           (void*)(long) (mnemonics_visible));
  }
}

/******************** mnemonic_widget Property ********************/

/*!
 * \brief geda_label_get_mnemonic_widget
 * \par Function Description
 *  Retrieves the target of the mnemonic (keyboard shortcut) of this
 *  label. See geda_label_set_mnemonic_widget().
 *
 * \param [in] label  The GedaLabel object
 *
 * \returns Target of the label's mnemonic, or %NULL if none has been set
 *          and the default algorithm will be used.
 */
GtkWidget *geda_label_get_mnemonic_widget (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), NULL);

  return label->priv->mnemonic_widget;
}

/*!
 * \brief geda_label_set_mnemonic_widget
 * \par Function Description
 *  If the label has been set so that it has an mnemonic key (using i.e.
 *  geda_label_set_markup_with_mnemonic(), geda_label_set_mnemonic_text(),
 *  geda_mnemonic_label_new() or the "use_underline" property) the label
 *  can be associated with a widget that is the target of the mnemonic.
 *  When the label is inside of a widget (like a GtkButton or a GtkNotebook
 *  tab) it is automatically associated with the correct widget, but some
 *  times the target needs to be set explicitly using this function (i.e.
 *  when the target is a GedaEntry next to the label).
 *
 *  The target widget will be accelerated by emitting the
 *  GtkWidget::mnemonic-activate signal on it. The default handler for this
 *  signal will activate the widget if there are no mnemonic collisions and
 *  toggle focus between the colliding widgets otherwise.
 *
 * \param [in] label    The GedaLabel object
 * \param [in] widget   Target GtkWidget
 */
void geda_label_set_mnemonic_widget (GedaLabel *label, GtkWidget *widget)
{
  GedaLabelData *priv;

  g_return_if_fail (GEDA_IS_LABEL(label));

  priv = label->priv;

  if (priv->mnemonic_widget) {

    gtk_widget_remove_mnemonic_label(priv->mnemonic_widget, (GtkWidget*)label);

    g_object_weak_unref ((GObject*)priv->mnemonic_widget,
                         label_mnemonic_widget_weak_notify,
                         label);
  }

  if (widget) {

    g_return_if_fail (GTK_IS_WIDGET (widget));

    g_object_weak_ref ((GObject*)widget,
                       label_mnemonic_widget_weak_notify,
                       label);

    gtk_widget_add_mnemonic_label (widget, (GtkWidget*)label);
  }

  priv->mnemonic_widget = widget;

  GEDA_OBJECT_NOTIFY (label, "mnemonic-widget");
}

/*!
 * \brief Set GedaLabel Underline Pattern
 * \par Function Description
 *  Sets the pattern of underlines under the existing text within the
 *  #GedaLabel widget. For example if the current text of the label says
 *  "FooBarBaz" passing a pattern of "___   ___" will underline "Foo" and
 *  "Baz" but not "Bar". If \a pattern is NULL then the current will be
 *  removed.
 *
 * \param [in] label   The GedaLabel object
 * \param [in] pattern Pattern as described above or NULL.
 */
void geda_label_set_pattern (GedaLabel *label, const char *pattern)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  label->priv->pattern_set = FALSE;

  if (pattern) {

    geda_label_set_pattern_internal (label, pattern, FALSE);
    label->priv->pattern_set = TRUE;
  }
  else {
    geda_label_recalculate (label);
  }

  geda_label_clear_layout (label);

  gtk_widget_queue_resize ((GtkWidget*)label);

  GEDA_OBJECT_NOTIFY (label, "pattern");
}

/*********************** Selectable Property **********************/

/*!
 * \brief Determine if GedaLabel is selectable
 * \par Function Description
 *  Gets the value set by geda_label_set_selectable().
 *
 * \param [in] label  The GedaLabel object
 *
 * \retval %TRUE if the user can copy text from the label
 */
bool geda_label_get_selectable (GedaLabel *label)
{
  SelectionInfo *info;

  g_return_val_if_fail (GEDA_IS_LABEL(label), FALSE);

  info = label->priv->select_info;

  return info && info->selectable;
}

/*!
 * \brief Set if a GedaLabel Widget can be selected
 * \par Function Description
 *  Selectable labels allow the user to select text from the label, for
 *  copy-and-paste.
 *
 * \param [in] label   The GedaLabel object
 * \param [in] setting: %TRUE to allow selecting text in the label
 */
void geda_label_set_selectable (GedaLabel *label, bool setting)
{
  GedaLabelData *priv;
  bool old_setting;

  g_return_if_fail (GEDA_IS_LABEL(label));

  priv = label->priv;

  setting = setting != FALSE;
  old_setting = priv->select_info && priv->select_info->selectable;

  if (setting) {
    geda_label_ensure_select_info (label);
    priv->select_info->selectable = TRUE;
    geda_label_update_cursor (label);
  }
  else if (old_setting) {
    /* unselect, to give up the selection */
    geda_label_select_region (label, 0, 0);

    priv->select_info->selectable = FALSE;
    geda_label_clear_select_info (label);
    geda_label_update_cursor (label);
  }

  if (setting != old_setting) {
    g_object_freeze_notify ((GObject*)label);
    GEDA_OBJECT_NOTIFY (label, "selectable");
    GEDA_OBJECT_NOTIFY (label, "cursor-position");
    GEDA_OBJECT_NOTIFY (label, "selection-bound");
    g_object_thaw_notify ((GObject*)label);
    gtk_widget_queue_draw ((GtkWidget*)label);
  }
}

/******************** selection_bound Property ********************/

/*!
 * \brief Get selection Bounds
 * \par Function Description
 *  Returns offset of the label text selection anchor.
 *
 * \param [in] label The GedaLabel object
 *
 * \returns Offset of the label text selection
 */
int geda_label_get_selection_bound (GedaLabel *label)
{
  if (GEDA_IS_LABEL(label)) {

    GedaLabelData *priv = label->priv;

    if (priv->select_info && priv->select_info->selectable) {
      return g_utf8_pointer_to_offset (label->text,
                                       label->text + priv->select_info->selection_anchor);
    }
  }

  return 0;
}

/*!
 * \brief geda_label_get_selection_bounds
 * \par Function Description
 *  Gets the selected range of characters in the label, returning %TRUE
 *  if there's a selection.
 *
 * \param [in] label  The GedaLabel object
 *
 * \param [out] start location for start of selection, as a character offset
 * \param [out] end   location for end of selection, as a character offset
 *
 * \retval %TRUE if selection is non-empty
 */
bool geda_label_get_selection_bounds (GedaLabel  *label, int *start, int *end)
{
  SelectionInfo *info;

  g_return_val_if_fail (GEDA_IS_LABEL(label), FALSE);

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

/******************* single_line_mode Property ********************/

/*!
 * \brief geda_label_get_single_line_mode
 * \par Function Description
 *  Returns whether the label is in single line mode.
 *
 * \param [in] label The GedaLabel object
 *
 * \retval %TRUE when the label is in single line mode.
 */
bool geda_label_get_single_line_mode  (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), FALSE);

  return label->priv->single_line_mode;
}

/*!
 * \brief geda_label_set_single_line_mode
 * \par Function Description
 *  Sets whether the label is in single line mode. Set %TRUE if the label
 *  should be in single line mode
 *
 * \param [in] label              The GedaLabel object
 * \param [in] single_line_mode   Desired setting
 */
void geda_label_set_single_line_mode (GedaLabel *label,
                                           bool  single_line_mode)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  single_line_mode = single_line_mode != FALSE;

  if (label->priv->single_line_mode != single_line_mode) {

    label->priv->single_line_mode = single_line_mode;

    geda_label_clear_layout (label);

    gtk_widget_queue_resize ((GtkWidget*)label);

    GEDA_OBJECT_NOTIFY (label, "single-line-mode");
  }
}

/************************* text Property **************************/

/*!
 * \brief Set the GedaLabel Text
 * \par Function Description
 *  Sets the text within the #GedaLabel widget. It overwrites any text
 *  that was there before.
 *
 * \note This will also clear any previously set mnemonic accelerators.
 *
 * \param [in] label  The GedaLabel object
 * \param [in] str    The text to be set
 */
void geda_label_set_text (GedaLabel *label, const char *str)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  g_object_freeze_notify ((GObject*)label);

  geda_label_set_label_internal (label, geda_strdup (str ? str : ""));
  geda_label_set_use_markup_internal (label, FALSE);
  geda_label_set_use_underline_internal (label, FALSE);

  geda_label_recalculate (label);

  g_object_thaw_notify ((GObject*)label);
}

/****************** track_visited_links Property ******************/

/*!
 * \brief Get whether visited links are tracked
 * \par Function Description
 *  Returns whether the label is currently keeping track
 *  of clicked links.
 *
 * \param [in] label The GedaLabel object
 *
 * \returns %TRUE if clicked links are remembered
 */
bool geda_label_get_track_visited_links (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), FALSE);
  return label->priv->track_links;
}

/*!
 * \brief Set whether visited links are tracked
 * \par Function Description
 *  Sets whether the label should keep track of clicked
 *  links (and use a different color for them).
 *
 * \param [in] label        The GedaLabel object
 * \param [in] track_links  %TRUE to track visited links
 */
void geda_label_set_track_visited_links (GedaLabel *label, bool track_links)
{
  if (GEDA_IS_LABEL(label)) {

    GedaLabelData *priv = label->priv;

    track_links = track_links != FALSE;

    if (priv->track_links != track_links) {

      priv->track_links = track_links;

      /* FIXME: shouldn't have to redo everything here */
      geda_label_recalculate (label);

      GEDA_OBJECT_NOTIFY (label, "track-visited-links");
    }
  }
}

/********************** use_markup Property ***********************/

/*!
 * \brief geda_label_get_use_markup
 * \par Function Description
 *  Returns whether the label's text is interpreted as marked up with
 *  the Pango text markup language. See geda_label_set_use_markup ().
 *
 * \param [in] label   The GedaLabel object
 *
 * \retval %TRUE if the label's text will be parsed for markup.
 */
bool geda_label_get_use_markup (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), FALSE);

  return label->priv->use_markup;
}

/*!
 * \brief geda_label_set_use_markup
 * \par Function Description
 *  Sets whether the text of the label contains markup in Pango's text markup
 *  language. See geda_label_set_markup().
 *
 * \param [in] label   The GedaLabel object
 * \param [in] setting %TRUE if the label's text should be parsed for markup.
 */
void geda_label_set_use_markup (GedaLabel *label, bool setting)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  g_object_freeze_notify ((GObject*)label);

  geda_label_set_use_markup_internal (label, setting);

  geda_label_recalculate (label);

  g_object_thaw_notify ((GObject*)label);

}

/********************* use_underline Property *********************/

/*!
 * \brief geda_label_get_use_underline
 * \par Function Description
 *  Returns whether an embedded underline in the label indicates a
 *  mnemonic. See geda_label_set_use_underline().
 *
 * \param [in] label   The GedaLabel object
 *
 * \retval %TRUE whether an embedded underline in the label indicates
 *          the mnemonic accelerator keys.
 */
bool geda_label_get_use_underline (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), FALSE);

  return label->priv->use_underline;
}

/*!
 * \brief geda_label_set_use_underline
 * \par Function Description
 *  If true, an underline in the text indicates the next character should
 *  be used for the mnemonic accelerator key.
 *
 * \param [in] label   The GedaLabel object
 * \param [in] setting %TRUE if underlines in the text indicate mnemonics.
 */
void geda_label_set_use_underline (GedaLabel *label, bool setting)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  g_object_freeze_notify ((GObject*)label);

  geda_label_set_use_underline_internal(label, setting);

  geda_label_recalculate (label);

  g_object_thaw_notify ((GObject*)label);
}

/********************** Width Chars Property **********************/

/*!
 * \brief Get the Width of GedaLabel Characters
 * \par Function Description
 *  Retrieves the desired width of label, in characters.
 *
 * \param [in] label  The GedaLabel object
 *
 * \returns the width of the label in characters.
 *
 * \sa geda_label_set_width_chars
 */
int geda_label_get_width_chars (GedaLabel *label)
{
  g_return_val_if_fail (GEDA_IS_LABEL(label), -1);

  return label->width_chars;
}

/*!
 * \brief Set the Width of Characters for a GedaLabel object
 * \par Function Description
 * Sets the desired width in characters of label to n_chars.
 *
 * \param [in] label   The GedaLabel object
 * \param [in] n_chars New desired width, in characters.
 */
void geda_label_set_width_chars (GedaLabel *label, int n_chars)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

  if (label->width_chars != n_chars) {

    label->width_chars = n_chars;

    gtk_widget_queue_resize ((GtkWidget*)label);

    GEDA_OBJECT_NOTIFY (label, "width-chars");
  }
}

#ifdef DEBUG_GEDA_LABEL

/*!
 * \brief Report GedaLabel Instances
 * \par Function Description
 *  This function can be called after all libgedauio resources
 *  have been released to print a list of pointers to GedaLabel
 *  objects that are still alive or the NULL message if all of
 *  the GedaLabels were released. This is particularly useful
 *  for debugging/developing menu systems.
 */
void geda_label_report_instances (void)
{
  if (label_hash_table) {

    void print_hash(GedaLabel *label, void *value, void *nothing) {
      fprintf(stderr, "label,%p,text=<%s>\n", label, label->text);
    }

    g_hash_table_foreach (label_hash_table, (GHFunc)print_hash, NULL);
    g_hash_table_destroy (label_hash_table);
    label_hash_table = NULL;
  }
  else {
    fprintf(stderr, "%s: the table of labels is NULL\n", __func__);
  }
}

#endif /* DEBUG_GEDA_LABEL */

/*!
 * \brief geda_label_select_region
 * \par Function Description
 *  Selects a range of characters in the label, if the label is selectable.
 *  See geda_label_set_selectable(). If the label is not selectable,
 *  this function has no effect. If start_offset or
 *  end_offset are -1, then the end of the label will be substituted.
 *
 * \param [in] label        The GedaLabel object
 * \param [in] start_offset The start offset (in characters not bytes)
 * \param [in] end_offset   The end offset (in characters not bytes)
 */
void geda_label_select_region (GedaLabel *label, int start_offset, int end_offset)
{
  g_return_if_fail (GEDA_IS_LABEL(label));

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

/**
 * \defgroup GedaLabelWidgets GedaLabel Public Widget Functions
 * @{
 *
 * \par Begin Widget Versions
 */

/*!
 * \brief Get the Text Alignment of a GedaLabel Widget
 * \par Function Description
 *  Retrieves the text alignment property of the #GedaLabel widget.
 *
 * \param [in]  widget The GedaLabel widget
 * \param [out] xalign Pointer to float to store the horizontal alignment
 * \param [out] yalign Pointer to float to store the vertical alignment
 *
 * \sa geda_label_get_alignment
 */
void geda_label_widget_get_alignment (GtkWidget *widget, float *xalign, float *yalign)
{
  g_return_if_fail (GEDA_IS_LABEL(widget));

  gtk_misc_get_alignment ((GtkMisc*)widget, xalign, yalign);
}

/*!
 * \brief Set the Text Alignment of a GedaLabel Widget
 * \par Function Description
 *  Set the text alignment property of the #GedaLabel widget.
 *
 * \param [in] widget The GedaLabel widget
 * \param [in] xalign horizontal alignment, from 0 (left) to 1 (right)
 * \param [in] yalign vertical alignment, from 0 (top) to 1 (bottom)
 *
 * \sa geda_label_set_alignment
 */
void geda_label_widget_set_alignment (GtkWidget *widget, float xalign, float yalign)
{
  g_return_if_fail (GEDA_IS_LABEL(widget));

  gtk_misc_set_alignment ((GtkMisc*)widget, xalign, yalign);
}

/*!
 * \brief Get GedaLabel widget angle property
 * \par Function Description
 *  Widget version of #geda_label_get_angle.
 *
 * \returns the angle of rotation for the label
 *
 * \sa geda_label_get_angle
 */
double geda_label_widget_get_angle (GtkWidget *widget)
{
  return geda_label_get_angle ((GedaLabel*)widget);
}

/*!
 * \brief Set GedaLabel widget angle property
 * \par Function Description
 *  Widget version of #geda_label_set_angle.
 *
 * \sa geda_label_set_angle
 */
void geda_label_widget_set_angle (GtkWidget *widget, double angle)
{
  geda_label_set_angle ((GedaLabel*)widget, angle);
}

/*!
 * \brief Get the ellipsize property of a GedaLabel Widget
 * \par Function Description
 *  Returns the ellipsizing position of the label. See geda_label_set_ellipsize().
 *
 * \param [in] widget The GedaLabel object
 *
 * \returns PangoEllipsizeMode
 *
 * \sa geda_label_get_ellipsize
 */
PangoEllipsizeMode geda_label_widget_get_ellipsize (GtkWidget *widget)
{
  return geda_label_get_ellipsize ((GedaLabel*) widget);
}

/*!
 * \brief Set the ellipsize property of a GedaLabel Widget
 * \par Function Description
 *  Sets the mode used to ellipsize (add an ellipsis: "...") to the
 *  text if there is not enough space to render the entire string.
 *
 * \param [in] widget The GedaLabel object
 * \param [in] mode   a PangoEllipsizeMode
 *
 * \sa geda_label_set_ellipsize
 */
void geda_label_widget_set_ellipsize (GtkWidget *widget, PangoEllipsizeMode mode)
{
  geda_label_set_ellipsize ((GedaLabel*)widget, mode);
}

/*!
 * \brief get label widget justification
 * \par Function Description
 *  Returns the justification of the label.
 *
 * \param [in] widget The GedaLabel widget
 *
 * \returns GtkJustification
 *
 * \sa geda_label_set_justify
 */
GtkJustification geda_label_widget_get_justify (GtkWidget *widget)
{
  return geda_label_get_justify ((GedaLabel*)widget);
}

/*!
 * \brief geda_label_set_justify
 * \par Function Description
 *  Sets the alignment of the lines in the text of the label relative to
 *  each other. %GTK_JUSTIFY_LEFT is the default value when the widget
 *  is first created with geda_label_new(). If you instead want to set
 *  the alignment of the label as a whole, use gtk_misc_set_alignment
 *  instead.
 *
 * \note geda_label_set_justify has no effect on labels containing
 *       only a single line.
 *
 * \param [in] widget The GedaLabel widget
 * \param [in] jtype  The GedaLabel object
 *
 * \sa geda_label_set_justify
 */
void geda_label_widget_set_justify (GtkWidget *widget, GtkJustification jtype)
{
  geda_label_set_justify ((GedaLabel*)widget, jtype);
}

/*!
 * \brief Get the GedaLabel widget label
 * \par Function Description
 *  Fetches the text from a label widget including any embedded
 *  underlines indicating mnemonics and Pango markup.
 *
 * \param [in] label The GedaLabel object
 *
 * \returns the text of the label widget.
 *
 * \sa geda_label_get_label
 */
const char *geda_label_widget_get_label (GtkWidget *label)
{
  return geda_label_get_label((GedaLabel*)label);
}

/*!
 * \brief Set the GedaLabel widget label
 * \par Function Description
 *  Sets the text of the label.
 *
 * \param [in] label  The GedaLabel object
 * \param [in] str    New text to set for the label
 *
 * \sa geda_label_get_label
 */
void geda_label_widget_set_label (GtkWidget *label, const char *str)
{
  return geda_label_set_label ((GedaLabel*)label, str);
}

/*!
 * \brief Retrieves the Maximum Width of Characters for a GedaLabel Widget
 * \par Function Description
 *  Wrapper to retrieves the desired maximum width of label, in characters
 *  casting the GedaLabel to Widget.
 *
 * \param [in] label The GedaLabel widget
 *
 * \returns the maximum width of the label in characters.
 *
 * \sa geda_label_get_max_width_chars
 */
int geda_label_widget_get_max_width_chars (GtkWidget *label)
{
  return geda_label_get_max_width_chars ((GedaLabel*)label);
}

/*!
 * \brief Set the Maximum Width of Characters of a GedaLabel Widget
 * \par Function Description
 *  Wrapper for geda_label_set_max_width_chars.
 *
 * \param [in] label   The GedaLabel widget
 * \param [in] n_chars New desired maximum width, in characters.
 *
 * \sa geda_label_set_max_width_chars
 */
void geda_label_widget_set_max_width_chars (GtkWidget *label, int n_chars)
{
  geda_label_set_max_width_chars ((GedaLabel*)label, n_chars);
}

/*!
 * \brief Determine if a GedaLabel Widget is selectable
 * \par Function Description
 * \param [in] label  The GedaLabel object
 *
 * \retval %TRUE if the user can copy text from the label
 * \sa geda_label_set_selectable
 */
bool geda_label_widget_get_selectable (GtkWidget *label)
{
  return geda_label_get_selectable ((GedaLabel*)label);
}

/*!
 * \brief Set if a GedaLabel Widget can be selected
 * \par Function Description
 * \param [in] label   The GedaLabel object
 * \param [in] setting %TRUE to allow selecting text in the label
 * \sa geda_label_set_selectable
 */
void geda_label_widget_set_selectable (GtkWidget *label, bool setting)
{
  geda_label_set_selectable ((GedaLabel*)label, setting);
}

/*!
 * \brief Get Pointer to GedaLabel Widget Text
 * \par Function Description
 *  Returns a pointer to the text of the GedaLabel. The
 *  string belongs to the widget and should not be released.
 */
const char *geda_label_widget_get_text (GtkWidget *widget)
{
  return geda_label_get_text((GedaLabel*)widget);
}

/*!
 * \brief Set the GedaLabel Widget Text
 * \par Function Description
 *  Sets the text within the #GedaLabel widget.
 *
 * \param [in] widget GedaLabel cast to a Widget
 * \param [in] str    The text to be set
 *
 * \sa geda_label_set_text
 */
void geda_label_widget_set_text (GtkWidget *widget, const char *str)
{
  geda_label_set_text ((GedaLabel*)widget,str);
}

/*!
 *\brief Widget Convenience Versions of label set_use_markup */
void geda_label_widget_set_use_markup (GtkWidget *widget, bool setting)
{
  geda_label_set_use_markup ((GedaLabel*)widget, setting);
}

/*!
 *\brief Widget Convenience Versions of label get_use_markup */
bool geda_label_widget_get_use_markup (GtkWidget *widget)
{
  return geda_label_get_use_markup ((GedaLabel*)widget);
}

/*!
 * \brief Widget Wrapper for geda_label_get_use_underline
 * \par Function Description
 *  See geda_label_get_use_underline.
 *
 * \param [in] widget The GedaLabel widget
 *
 * \retval %TRUE whether an embedded underline in the label indicates
 *               the mnemonic accelerator keys.
 */
bool geda_label_widget_get_use_underline (GtkWidget *widget)
{
  return geda_label_get_use_underline ((GedaLabel*)widget);
}

/*!
 * \brief Widget Wrapper for geda_label_set_use_underline
 * \par Function Description
 *  If true, an underline in the text indicates the next character should
 *  be used for the mnemonic accelerator key.
 *
 * \param [in] widget  The GedaLabel widget
 * \param [in] setting %TRUE if underlines in the text indicate mnemonics.
 */
void geda_label_widget_set_use_underline (GtkWidget *widget, bool setting)
{
  geda_label_set_use_underline ((GedaLabel*)widget, setting);
}

/*!
 * \brief Get the Width of Characters from a GedaLabel widget
 * \par Function Description
 *  Wrapper for geda_label_get_width_chars that accepts a pointer
 *  to a GtkWidget for the GedaLabel object.
 *
 *  \param [in] widget The GedaLabel widget
 */
int geda_label_widget_get_width_chars (GtkWidget *widget)
{
  return geda_label_get_width_chars ((GedaLabel*)widget);
}

/*!
 * \brief Set the Width of Characters for a GedaLabel widget
 * \par Function Description
 *  Sets the desired width in characters of label to n_chars.
 *
 * \param [in] widget The GedaLabel widget
 * \param [in] n_chars New desired width, in characters.
 */
void geda_label_widget_set_width_chars (GtkWidget *widget, int n_chars)
{
  geda_label_set_width_chars ((GedaLabel*)widget, n_chars);
}

#undef PangoFontDescr

/** @} end group GedaLabelWidgets */
/** @} end group GedaLabelFunctions */
/** @} end group GedaLabel */
