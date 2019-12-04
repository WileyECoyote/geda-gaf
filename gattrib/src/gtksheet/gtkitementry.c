/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/**
 * \section gtkitementry
 * \brief short_description: An item entry widget.
 *
 * Originally GtkSheetEntry. This widget allows to change colors and
 * justification and can be dynamically resized. The GtkItemEntry struct
 * contains only private data and should only be accessed through the
 * functions described below.
 */

#include <string.h>
#include <stdint.h>

#include <pango/pango.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include <gtksheet/gtkcompat.h>
#include <gtksheet/gtkitementry.h>

#define MIN_ENTRY_WIDTH  150
#define DRAW_TIMEOUT     20
#define INNER_BORDER     0

/* Initial size of buffer, in bytes */
#define MIN_SIZE 16

/* Maximum size of text buffer, in bytes */
#define MAX_SIZE G_MAXUSHORT

#undef GTK_ITEM_ENTRY_DEBUG

#ifdef DEBUG
#define GTK_ITEM_ENTRY_DEBUG  0  /* define to activate debug output */
#endif

#ifdef GTK_ITEM_ENTRY_DEBUG
#   define GTK_ITEM_ENTRY_DEBUG_JUSTIFICATION 0
#   define GTK_ITEM_ENTRY_DEBUG_ALLOC 0
#   define GTK_ITEM_ENTRY_DEBUG_TEXT 0
#   define GTK_ITEM_ENTRY_DEBUG_DESTUCTION 0  /* destroy, dispose, finalize */
#endif


typedef enum {
    CURSOR_STANDARD,
    CURSOR_DND
} CursorType;

/* GObject, GtkObject methods
 */
static void   gtk_item_entry_class_init(GtkItemEntryClass *klass);
static void   gtk_item_entry_init(GtkItemEntry *entry);
static void   gtk_item_entry_editable_init(GtkEditableClass *iface);

/* GtkWidget methods
 */
static void   gtk_item_entry_realize(GtkWidget *widget);
static void   gtk_item_entry_size_request(GtkWidget *widget,
                                          GtkRequisition *requisition);
static void   gtk_item_entry_size_allocate(GtkWidget *widget,
                                           GtkAllocation *allocation);
static void   gtk_item_entry_draw_frame(GtkWidget *widget);
static void   gtk_item_entry_destroy(GtkObject *object);
static void   gtk_item_entry_dispose(GObject *object);
static void   gtk_item_entry_finalize(GObject *object);
static int   gtk_item_entry_expose(GtkWidget *widget, GdkEventExpose *event);
static void   gtk_item_entry_grab_focus(GtkWidget *widget);
static void   gtk_item_entry_style_set(GtkWidget *widget, GtkStyle *previous_style);
static void   gtk_item_entry_direction_changed(GtkWidget *widget, GtkTextDirection previous_dir);
static void   gtk_item_entry_state_changed(GtkWidget *widget, GtkStateType previous_state);

/* GtkEditable method implementations
 */
static void     gtk_item_entry_insert_text(GtkEditable *editable,
                                           const char *new_text,
                                           int         new_text_length,
                                           int        *position);
static void     gtk_item_entry_delete_text(GtkEditable *editable,
                                           int         start_pos,
                                           int         end_pos);

static void     gtk_item_entry_real_set_position(GtkEditable *editable,
                                                 int         position);
static int     gtk_item_entry_get_position(GtkEditable *editable);

/* Default signal handlers
 */
static void gtk_item_entry_real_insert_text(GtkEditable     *editable,
                                            const char     *new_text,
                                            int             new_text_length,
                                            int            *position);
static void gtk_item_entry_real_delete_text(GtkEditable     *editable,
                                            int             start_pos,
                                            int             end_pos);
static void gtk_item_entry_move_cursor(GtkEntry        *entry,
                                       GtkMovementStep  step,
                                       int              count,
                                       int              extend_selection);
static void gtk_item_entry_insert_at_cursor(GtkEntry        *entry,
                                            const char     *str);
static void gtk_item_entry_delete_from_cursor(GtkEntry        *entry,
                                              GtkDeleteType    type,
                                              int             count);

/* IM Context Callbacks
 */
static void     gtk_item_entry_commit_cb(GtkIMContext *context,
                                         const char  *str,
                                         GtkEntry     *entry);
static void     gtk_item_entry_preedit_changed_cb(GtkIMContext *context,
                                                  GtkEntry     *entry);
static _Bool gtk_item_entry_retrieve_surrounding_cb(GtkIMContext *context,
                                                       GtkEntry     *entry);
static _Bool gtk_item_entry_delete_surrounding_cb(GtkIMContext *context,
                                                     int          offset,
                                                     int          n_chars,
                                                     GtkEntry     *entry);

/* Internal routines
 */
static void         gtk_item_entry_enter_text(GtkEntry       *entry,
                                              const char     *str);
static void         gtk_item_entry_set_positions(GtkEntry    *entry,
                                                 int          current_pos,
                                                 int          selection_bound);
static void         gtk_item_entry_draw_text(GtkEntry        *entry);
static void         gtk_item_entry_draw_cursor(GtkEntry      *entry,
                                               CursorType     type);
static PangoLayout *gtk_item_entry_ensure_layout(GtkEntry    *entry,
                                                 _Bool        include_preedit);
static void         gtk_item_entry_queue_draw(GtkEntry       *entry);

#if GTK_CHECK_VERSION(2,21,0) == 0
static void         gtk_entry_reset_im_context(GtkEntry      *entry);
#endif

static void         gtk_item_entry_recompute(GtkEntry        *entry);
static void         gtk_item_entry_get_cursor_locations(GtkEntry      *entry,
                                                        CursorType     type,
                                                        int           *strong_x,
                                                        int           *weak_x);
static void         gtk_item_entry_adjust_scroll(GtkEntry         *entry);
static int          gtk_item_entry_move_visually(GtkEntry         *editable,
                                                 int               start,
                                                 int               count);
static int          gtk_item_entry_move_logically(GtkEntry        *entry,
                                                  int              start,
                                                  int              count);
static int          gtk_item_entry_move_forward_word(GtkEntry     *entry,
                                                     int           start);
static int          gtk_item_entry_move_backward_word(GtkEntry    *entry,
                                                      int          start);
static void         gtk_item_entry_delete_whitespace(GtkEntry     *entry);
static char        *gtk_item_entry_get_public_chars(GtkEntry      *entry,
                                                    int            start,
                                                    int            end);
static void         gtk_item_entry_update_primary_selection (GtkEntry      *entry);
static void         gtk_item_entry_state_changed            (GtkWidget     *widget,
                                                             GtkStateType   previous_state);
static void         gtk_item_entry_check_cursor_blink       (GtkEntry      *entry);
static void         gtk_item_entry_pend_cursor_blink        (GtkEntry      *entry);
static void        _item_entry_get_text_area_size           (GtkEntry      *entry,
                                                             int           *x,
                                                             int           *y,
                                                             int           *width,
                                                             int           *height);
static void        _item_entry_get_widget_window_size       (GtkWidget     *entry,
                                                             int           *x,
                                                             int           *y,
                                                             int           *width,
                                                             int           *height);

static GtkEntryClass *parent_class = NULL;

GType
gtk_item_entry_get_type(void)
{
  static GType item_entry_type = 0;

  if (!item_entry_type)
  {
    static const GInterfaceInfo item_editable_info =
    {
      (GInterfaceInitFunc)gtk_item_entry_editable_init,    /* interface_init */
      NULL,                                            /* interface_finalize */
      NULL                                             /* interface_data */
    };


    item_entry_type = g_type_register_static_simple(gtk_entry_get_type(),
                                                    "GtkItemEntry",
                                                    sizeof(GtkItemEntryClass),
                                                    (GClassInitFunc)gtk_item_entry_class_init,
                                                    sizeof(GtkItemEntry),
                                                    (GInstanceInitFunc)gtk_item_entry_init,
                                                    0);


    g_type_add_interface_static(item_entry_type,
                                gtk_editable_get_type(),
                                &item_editable_info);

  }

  return item_entry_type;
}

static void
gtk_item_entry_class_init(GtkItemEntryClass *klass)
{
    GObjectClass   *gobject_class = G_OBJECT_CLASS (klass);
    GtkObjectClass *gtk_object_class;
    GtkWidgetClass *widget_class;
    GtkEntryClass  *entry_class;

    gobject_class->dispose  = gtk_item_entry_dispose;
    gobject_class->finalize = gtk_item_entry_finalize;

    gtk_object_class = (GtkObjectClass *)klass;
    widget_class     = (GtkWidgetClass *)klass;
    parent_class     = g_type_class_ref(gtk_entry_get_type());
    entry_class      = (GtkEntryClass *)klass;

    widget_class->realize           = gtk_item_entry_realize;
    widget_class->size_request      = gtk_item_entry_size_request;
    widget_class->size_allocate     = gtk_item_entry_size_allocate;
    widget_class->expose_event      = gtk_item_entry_expose;
    widget_class->grab_focus        = gtk_item_entry_grab_focus;
    widget_class->style_set         = gtk_item_entry_style_set;
    widget_class->direction_changed = gtk_item_entry_direction_changed;
    widget_class->state_changed     = gtk_item_entry_state_changed;

    entry_class->move_cursor        = gtk_item_entry_move_cursor;
    entry_class->insert_at_cursor   = gtk_item_entry_insert_at_cursor;
    entry_class->delete_from_cursor = gtk_item_entry_delete_from_cursor;

    gtk_object_class->destroy       = gtk_item_entry_destroy;
}

static void
gtk_item_entry_editable_init(GtkEditableClass *iface)
{
    iface->do_insert_text = gtk_item_entry_insert_text;
    iface->do_delete_text = gtk_item_entry_delete_text;
    iface->insert_text    = gtk_item_entry_real_insert_text;
    iface->delete_text    = gtk_item_entry_real_delete_text;
    iface->set_position   = gtk_item_entry_real_set_position;
    iface->get_position   = gtk_item_entry_get_position;
}

static void
gtk_item_entry_init(GtkItemEntry *item_entry)
{
    item_entry->justification    = GTK_JUSTIFY_LEFT;
    item_entry->max_length_bytes = 0;
    item_entry->text_max_size    = 0;
    item_entry->item_text_size   = 0;
    item_entry->item_n_bytes     = 0;

    GtkEntry *entry = (GtkEntry*)item_entry;

    entry->has_frame = FALSE;

    g_object_unref(entry->im_context);

    entry->im_context = gtk_im_multicontext_new();

    g_signal_connect(entry->im_context, "commit",
                     G_CALLBACK(gtk_item_entry_commit_cb), entry);

    g_signal_connect(entry->im_context, "preedit_changed",
                     G_CALLBACK(gtk_item_entry_preedit_changed_cb), entry);

    g_signal_connect(entry->im_context, "retrieve_surrounding",
                     G_CALLBACK(gtk_item_entry_retrieve_surrounding_cb), entry);

    g_signal_connect(entry->im_context, "delete_surrounding",
                     G_CALLBACK(gtk_item_entry_delete_surrounding_cb), entry);

}

static void
gtk_item_entry_realize(GtkWidget *widget)
{
    GtkEntry *entry;
    GdkWindowAttr attributes;
    int attributes_mask;

    gtk_widget_set_realized_true(widget);

    entry = (GtkEntry*)widget;

    attributes.window_type = GDK_WINDOW_CHILD;

    _item_entry_get_widget_window_size(widget, &attributes.x, &attributes.y, &attributes.width, &attributes.height);

    attributes.wclass = GDK_INPUT_OUTPUT;
    attributes.visual = gtk_widget_get_visual(widget);
    attributes.colormap = gtk_widget_get_colormap(widget);
    attributes.event_mask = gtk_widget_get_events(widget);
    attributes.event_mask |= (GDK_EXPOSURE_MASK |
                              GDK_BUTTON_PRESS_MASK |
                              GDK_BUTTON_RELEASE_MASK |
                              GDK_BUTTON1_MOTION_MASK |
                              GDK_BUTTON3_MOTION_MASK |
                              GDK_POINTER_MOTION_HINT_MASK |
                              GDK_POINTER_MOTION_MASK |
                              GDK_ENTER_NOTIFY_MASK |
                              GDK_LEAVE_NOTIFY_MASK);

    attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

    gtk_widget_set_window(widget,
	gdk_window_new(gtk_widget_get_parent_window(widget),
	    &attributes, attributes_mask));
    gdk_window_set_user_data(gtk_widget_get_window(widget), entry);

    _item_entry_get_text_area_size(entry, &attributes.x, &attributes.y, &attributes.width, &attributes.height);

    attributes.cursor = gdk_cursor_new(GDK_XTERM);
    attributes_mask |= GDK_WA_CURSOR;

    entry->text_area = gdk_window_new(gtk_widget_get_window(widget),
	&attributes, attributes_mask);
    gdk_window_set_user_data(entry->text_area, entry);

    gdk_cursor_unref(attributes.cursor);

    gtk_widget_set_style(widget,
                         gtk_style_attach(gtk_widget_get_style(widget),
                                          gtk_widget_get_window(widget)));

    gdk_window_set_background(gtk_widget_get_window(widget),
                              &(gtk_widget_get_style(widget)->bg[gtk_widget_get_state(widget)]));

    gdk_window_set_background(entry->text_area,
                              &(gtk_widget_get_style(widget)->bg[gtk_widget_get_state(widget)]));

    gdk_window_show(entry->text_area);

    gtk_im_context_set_client_window(entry->im_context, entry->text_area);

    gtk_item_entry_adjust_scroll(entry);
}

static void
_item_entry_get_borders(GtkEntry *entry, int *xborder, int *yborder)
{
    GtkWidget *widget = (GtkWidget*)entry;
    int focus_width;
    _Bool interior_focus;

    gtk_widget_style_get(widget,
	"interior-focus", &interior_focus,
	"focus-line-width", &focus_width,
	NULL);

    if (entry->has_frame) {

      *xborder = gtk_widget_get_style(widget)->xthickness;
      *yborder = gtk_widget_get_style(widget)->ythickness;
    }
    else {

      *xborder = 0;
      *yborder = 0;
    }

    if (!interior_focus) {

      *xborder += focus_width;
      *yborder += focus_width;
    }
}

static void
gtk_item_entry_size_request(GtkWidget *widget, GtkRequisition *requisition)
{
  GtkEntry *entry = (GtkEntry*)widget;
  PangoFontMetrics *metrics;
  int xborder, yborder;
  PangoContext *context;

  context = gtk_widget_get_pango_context(widget);
  metrics = pango_context_get_metrics(context,
                                      gtk_widget_get_style(widget)->font_desc,
                                      pango_context_get_language(context));

  entry->ascent = pango_font_metrics_get_ascent(metrics);
  entry->descent = pango_font_metrics_get_descent(metrics);

  _item_entry_get_borders(entry, &xborder, &yborder);

  xborder += INNER_BORDER;
  yborder += INNER_BORDER;

  if (entry->width_chars < 0) {
    requisition->width = MIN_ENTRY_WIDTH + xborder * 2;
  }
  else {
    int char_width = pango_font_metrics_get_approximate_char_width(metrics);
    requisition->width = PANGO_PIXELS(char_width) * entry->width_chars + xborder * 2;
  }

  requisition->height = PANGO_PIXELS(entry->ascent + entry->descent) + yborder * 2;

  pango_font_metrics_unref(metrics);
}

static void
_item_entry_get_text_area_size(GtkEntry *entry,
                               int      *x,
                               int      *y,
                               int      *width,
                               int      *height)
{
    int xborder, yborder;
    GtkRequisition requisition;
    GtkAllocation allocation;

    GtkWidget *widget = (GtkWidget*)entry;

    gtk_widget_get_child_requisition(widget, &requisition);

    _item_entry_get_borders(entry, &xborder, &yborder);

    if (x)
      *x = xborder;

    if (y)
      *y = yborder;

    if (width) {

      gtk_widget_get_allocation(widget, &allocation);
      *width = allocation.width - xborder * 2;
    }

    if (height)
	*height = requisition.height - yborder * 2;
}

static void
_item_entry_get_widget_window_size(GtkWidget *widget,
                                   int       *x,
                                   int       *y,
                                   int       *width,
                                   int       *height)
{
    GtkRequisition requisition;
    GtkAllocation allocation;

    /* GtkEntry->is_cell_renderer is a GSEAL()ed structure member.
       It will only be set to TRUE when calling the GtkEntry's GtkCellEditable
       interface function gtk_entry_cell_editable_init().

       This should never be the case in GtkSheet or GtkItemEntry.

       Anyhow, if the above statement wasn't true, solutions could be:
       - ask the Gtk maintainers to add the function
         GtkEntry::_get_widget_window_size() to the public GtkEntry interface
       - last resort work-around:
         use the sealed member anyhow GtkEntry->GSEAL(is_cell_renderer)
       */
#if 0
#    define ENTRY_IS_CELL_RENDERER  entry->is_cell_renderer
#else
#    define ENTRY_IS_CELL_RENDERER  FALSE
#endif

    gtk_widget_get_child_requisition(widget, &requisition);
    gtk_widget_get_allocation(widget, &allocation);

    if (x)
      *x = allocation.x;

    if (y) {

      if (ENTRY_IS_CELL_RENDERER)
        *y = allocation.y;
      else
        *y = allocation.y + (allocation.height - requisition.height) / 2;
    }

    if (width)
      *width = allocation.width;

    if (height) {

      if (ENTRY_IS_CELL_RENDERER)
        *height = allocation.height;
      else
        *height = requisition.height;
    }
}

static void
gtk_item_entry_size_allocate(GtkWidget *widget, GtkAllocation *allocation)
{
    GtkEntry     *entry  = (GtkEntry*)widget;
    GtkItemEntry *ientry = (GtkItemEntry*)widget;

    if (ientry->text_max_size > 0)
      allocation->width = MIN(ientry->text_max_size, allocation->width);

    gtk_widget_set_allocation(widget, allocation);

    if (gtk_widget_get_realized(widget)) {

      /* We call gtk_widget_get_child_requisition, since we want (for
       * backwards compatibility reasons) the realization here to
       * be affected by the usize of the entry, if set
       */
      int x, y, width, height;

      _item_entry_get_widget_window_size(widget, &x, &y, &width, &height);

#if GTK_ITEM_ENTRY_DEBUG_ALLOC>0
      g_debug("gtk_item_entry_size_allocate: _get_widget_window_size (%d, %d, %d, %d)",
      x, y, width, height);
#endif

#if GTK_ITEM_ENTRY_DEBUG_ALLOC>0
      g_debug("gtk_item_entry_size_allocate: move widget window (%d, %d, %d, %d)",
      allocation->x, allocation->y, allocation->width, allocation->height);
#endif

      gdk_window_move_resize(gtk_widget_get_window(widget),
                             allocation->x, allocation->y,
                             allocation->width, allocation->height);

      _item_entry_get_text_area_size(entry, &x, &y, &width, &height);

#if GTK_ITEM_ENTRY_DEBUG_ALLOC>0
      g_debug("gtk_item_entry_size_allocate: _get_text_area_size (%d, %d, %d, %d)",
      x, y, width, height);
#endif


#if GTK_ITEM_ENTRY_DEBUG_ALLOC>0
      g_debug("gtk_item_entry_size_allocate: move text_area (%d, %d, %d, %d)",
      0, allocation->height - height, allocation->width, height);
#endif

      gdk_window_move_resize(entry->text_area,
                             0, allocation->height - height, allocation->width, height);

      gtk_item_entry_recompute(entry);
    }
}

static void
gtk_item_entry_reset_layout(GtkEntry *entry)
{
  if (entry->cached_layout) {
    g_object_unref(entry->cached_layout);
    entry->cached_layout = NULL;
  }
}

static void
gtk_item_entry_draw_frame(GtkWidget *widget)
{
}

static void gtk_item_entry_destroy (GtkObject *object)
{
#if GTK_ITEM_ENTRY_DEBUG_DESTUCTION>0
  GtkItemEntry *ientry = GTK_ITEM_ENTRY(object);

  g_debug("gtk_item_entry_destroy %p %s", ientry,
      gtk_widget_get_name(GTK_WIDGET(ientry)));
#endif

  GTK_OBJECT_CLASS (parent_class)->destroy(object);
}

static void gtk_item_entry_dispose (GObject *object)
{

#if GTK_ITEM_ENTRY_DEBUG_DESTUCTION>0
  GtkItemEntry *ientry = GTK_ITEM_ENTRY(object);
  g_debug("gtk_item_entry_dispose %p", ientry);
#endif

  gtk_item_entry_reset_layout((GtkEntry*)object);

  G_OBJECT_CLASS (parent_class)->dispose(object);
}

static void gtk_item_entry_finalize (GObject *object)
{

#if GTK_ITEM_ENTRY_DEBUG_DESTUCTION > 0
  GtkItemEntry *ientry = GTK_ITEM_ENTRY(object);

  g_debug("gtk_item_entry_finalize %p", ientry);
#endif

  G_OBJECT_CLASS (parent_class)->finalize(object);
}

static int gtk_item_entry_expose(GtkWidget *widget, GdkEventExpose *event)
{
  GtkEntry *entry = GTK_ENTRY(widget);
  float align = 0.0;

  switch(GTK_ITEM_ENTRY(widget)->justification) {
    case GTK_JUSTIFY_LEFT:
      align = 0.0; break;
    case GTK_JUSTIFY_RIGHT:
      align = 1.0; break;
    case GTK_JUSTIFY_CENTER:
      align = 0.5; break;
    case GTK_JUSTIFY_FILL:
      align = 0.5; break;
  }

#if GTK_ITEM_ENTRY_DEBUG_JUSTIFICATION > 0
  g_debug("gtk_item_entry_expose: just %d align %g",
          GTK_ITEM_ENTRY(widget)->justification, align);
#endif

  if (gtk_entry_get_alignment(entry) != align) {
    gtk_entry_set_alignment(entry, align);
  }

  if (gtk_widget_get_window(widget) == event->window) {
    gtk_item_entry_draw_frame(widget);
  }
  else if (entry->text_area == event->window) {

    int area_width, area_height;

    _item_entry_get_text_area_size(entry, NULL, NULL, &area_width, &area_height);

    gdk_draw_rectangle(entry->text_area,
                       gtk_widget_get_style(widget)->bg_gc[gtk_widget_get_state(widget)],
                       TRUE,
                       0, 0, area_width, area_height);

    if ((entry->visible || entry->invisible_char != 0) &&
         gtk_widget_has_focus(widget) &&
         entry->selection_bound == entry->current_pos &&
         entry->cursor_visible) {
      gtk_item_entry_draw_cursor(entry, CURSOR_STANDARD);
    }

    if (entry->dnd_position != -1) {
      gtk_item_entry_draw_cursor(entry, CURSOR_DND);
    }

    gtk_item_entry_draw_text(entry);
  }

  return FALSE;
}

static void gtk_item_entry_grab_focus(GtkWidget *widget)
{
    GtkEntry *entry = GTK_ENTRY(widget);
    int select_on_focus;

    GTK_WIDGET_CLASS(parent_class)->grab_focus(widget);

    g_object_get(gtk_settings_get_default(),
                 "gtk-entry-select-on-focus",
                 &select_on_focus,
                 NULL);

    if (select_on_focus && entry->editable && !entry->in_click) {
      gtk_editable_select_region(GTK_EDITABLE(widget), 0, -1);
    }
}

static void gtk_item_entry_direction_changed(GtkWidget        *widget,
                                             GtkTextDirection  previous_dir)
{
    GtkEntry *entry = GTK_ENTRY(widget);

    gtk_item_entry_recompute(entry);

    GTK_WIDGET_CLASS(parent_class)->direction_changed(widget, previous_dir);
}

static void
gtk_item_entry_state_changed(GtkWidget *widget, GtkStateType previous_state)
{
    GtkEntry *entry = GTK_ENTRY(widget);

    if (gtk_widget_get_realized(widget)) {

	gdk_window_set_background(gtk_widget_get_window(widget),
	    &gtk_widget_get_style(widget)->bg[gtk_widget_get_state(widget)]);
	gdk_window_set_background(entry->text_area,
	    &gtk_widget_get_style(widget)->bg[gtk_widget_get_state(widget)]);
    }

    if (!gtk_widget_is_sensitive(widget)) {

	/* Clear any selection */
	gtk_editable_select_region(GTK_EDITABLE(entry), entry->current_pos, entry->current_pos);
    }

    gtk_widget_queue_draw(widget);
}

/* GtkEditable method implementations
 */
static void gtk_item_entry_insert_text(GtkEditable *editable,
                                       const char  *new_text,
                                       int          new_text_length,
                                       int         *position)
{
    GtkEntry *entry = GTK_ENTRY(editable);
    char buf[64];
    char *text;

    if (*position < 0 || *position > entry->text_length)
      *position = entry->text_length;

    g_object_ref(G_OBJECT(editable));

    if (new_text_length <= 63)
      text = buf;
    else
      text = g_malloc(new_text_length + 1);

    text[new_text_length] = '\0';
    strncpy(text, new_text, new_text_length);

    g_signal_emit_by_name(editable, "insert_text", text, new_text_length, position);

    if (new_text_length > 63)
      g_free(text);

    g_object_unref(editable);
}

static void
gtk_item_entry_delete_text(GtkEditable *editable, int start_pos, int end_pos)
{
    GtkEntry *entry = GTK_ENTRY(editable);

    if (end_pos < 0 || end_pos > entry->text_length)
      end_pos = entry->text_length;

    if (start_pos < 0)
      start_pos = 0;

    if (start_pos > end_pos)
      start_pos = end_pos;

    g_object_ref(editable);

    g_signal_emit_by_name(editable, "delete_text", start_pos, end_pos);

    g_object_unref(editable);
}

static void
gtk_item_entry_style_set(GtkWidget *widget, GtkStyle *previous_style)
{
    GtkEntry *entry = (GtkEntry*)widget;

    if (previous_style && gtk_widget_get_realized(widget)) {

      gtk_item_entry_recompute(entry);

      gdk_window_set_background(gtk_widget_get_window(widget),
                                &gtk_widget_get_style(widget)->bg[gtk_widget_get_state(widget)]);
      gdk_window_set_background(entry->text_area,
                                &gtk_widget_get_style(widget)->bg[gtk_widget_get_state(widget)]);
    }
}

static void
gtk_item_entry_real_set_position(GtkEditable *editable, int position)
{
    GtkEntry *entry = GTK_ENTRY(editable);

    if (position < 0 || position > entry->text_length)
      position = entry->text_length;

    if (position != entry->current_pos ||
        position != entry->selection_bound)
    {
      gtk_entry_reset_im_context(entry);
      gtk_item_entry_set_positions(entry, position, position);
    }
}

static int gtk_item_entry_get_position(GtkEditable *editable)
{
    return GTK_ENTRY(editable)->current_pos;
}

#ifdef GTK_TYPE_ENTRY_BUFFER
/*
 * Get_buffer copied from gtk/gtkentry.c
 */
static GtkEntryBuffer *_item_entry_get_buffer(GtkEntry *entry)
{

  GtkEntryBuffer *buffer = gtk_entry_get_buffer (entry);

  if (!buffer) {

    buffer = gtk_entry_buffer_new(NULL, 0);
    gtk_entry_set_buffer(entry, buffer);
    g_object_unref(buffer);
  }

  return buffer;
}

#endif /* GTK_TYPE_ENTRY_BUFFER */

/* Default signal handlers */

static void gtk_item_entry_real_insert_text(GtkEditable *editable,
                                            const char  *new_text,
                                            int          new_text_length,
                                            int         *position)
{
  int n_chars;
  int text_length;

#ifndef GTK_TYPE_ENTRY_BUFFER
  GtkItemEntry *ientry = GTK_ITEM_ENTRY(editable);
#endif

  GtkEntry *entry = GTK_ENTRY(editable);

  if (new_text_length < 0) {
    text_length = strlen(new_text);
  }
  else {
    text_length = new_text_length;
  }

  n_chars = g_utf8_strlen(new_text, text_length);

  if (entry->text_max_length > 0 && n_chars + entry->text_length > entry->text_max_length)
  {
    gdk_beep();
    n_chars = entry->text_max_length - entry->text_length;

#ifdef GTK_TYPE_ENTRY_BUFFER
  }

  GtkEntryBuffer *buffer = _item_entry_get_buffer(entry);

  gtk_entry_buffer_insert_text(buffer, *position, new_text, n_chars);

#else

    text_length = g_utf8_offset_to_pointer(new_text, n_chars) - new_text;
  }

  if (text_length + ientry->item_n_bytes + 1 > ientry->item_text_size) {

    while (text_length + ientry->item_n_bytes + 1 > ientry->item_text_size) {

      if (ientry->item_text_size == 0) {
        ientry->item_text_size = MIN_SIZE;
      }
      else {

        if (2 * (unsigned int)ientry->item_text_size < MAX_SIZE &&
            2 * (unsigned int)ientry->item_text_size > ientry->item_text_size)
        {
          ientry->item_text_size *= 2;
        }
        else
        {
          ientry->item_text_size = MAX_SIZE;
          if (text_length > (int)ientry->item_text_size - (int)ientry->item_n_bytes - 1)
          {
            text_length = (int)ientry->item_text_size - (int)ientry->item_n_bytes - 1;
            text_length = g_utf8_find_prev_char(new_text, new_text + text_length + 1) - new_text;
            n_chars = g_utf8_strlen(new_text, text_length);
          }
          break;
        }
      }
    }

    entry->text = g_realloc(entry->text, ientry->item_text_size);
  }

  int index;
  index = g_utf8_offset_to_pointer(entry->text, *position) - entry->text;

  g_memmove(entry->text + index + text_length, entry->text + index, ientry->item_n_bytes - index);
  memcpy(entry->text + index, new_text, text_length);

  if (text_length + *position > ientry->item_n_bytes) {
    ientry->item_n_bytes = text_length + *position;
  }

  /* NUL terminate for safety and convenience */
  entry->text[ientry->item_n_bytes] = '\0';

  entry->text_length = strlen(entry->text);

  if (entry->current_pos > *position) {
    entry->current_pos += n_chars;
  }

  if (entry->selection_bound > *position) {
    entry->selection_bound += n_chars;
  }

#endif /* GTK_TYPE_ENTRY_BUFFER */

#if GTK_ITEM_ENTRY_DEBUG_TEXT>0
  g_debug("gtk_item_entry_real_insert_text: n_chars %d %d", n_chars, *position);
#endif

  *position += n_chars;

  gtk_item_entry_recompute(entry);

  g_signal_emit_by_name(editable, "changed");
  g_object_notify(G_OBJECT(editable), "text");
}

static void gtk_item_entry_real_delete_text(GtkEditable *editable,
                                            int          start_pos,
                                            int          end_pos)
{
  GtkEntry *entry = GTK_ENTRY(editable);

  if (start_pos < 0) {
    start_pos = 0;
  }

  if (end_pos < 0 || end_pos > entry->text_length) {
    end_pos = entry->text_length;
  }

  if (start_pos < end_pos) {

#ifdef GTK_TYPE_ENTRY_BUFFER

    GtkEntryBuffer *buffer = _item_entry_get_buffer(entry);
    gtk_entry_buffer_delete_text(buffer, start_pos, end_pos - start_pos);

#else

    GtkItemEntry *ientry;
    int           start_index;
    int           end_index;

    ientry      = GTK_ITEM_ENTRY(editable);
    start_index = g_utf8_offset_to_pointer(entry->text, start_pos) - entry->text;
    end_index   = g_utf8_offset_to_pointer(entry->text, end_pos) - entry->text;

    g_memmove(entry->text + start_index, entry->text + end_index, ientry->item_n_bytes + 1 - end_index);

    ientry->item_n_bytes -= (end_index - start_index);
    entry->text_length   -= (end_pos - start_pos);

    if (entry->current_pos > start_pos) {
      entry->current_pos -= MIN(entry->current_pos, end_pos) - start_pos;
    }

    if (entry->selection_bound > start_pos) {
      entry->selection_bound -= MIN(entry->selection_bound, end_pos) - start_pos;
    }

#endif /* GTK_TYPE_ENTRY_BUFFER */

    /* We might have deleted the selection */
    gtk_item_entry_update_primary_selection(entry);

    gtk_item_entry_recompute(entry);

    g_signal_emit_by_name(editable, "changed");
    g_object_notify(G_OBJECT(editable), "text");
  }
}

/* Compute the X position for an offset that corresponds to the "more important
 * cursor position for that offset. We use this when trying to guess to which
 * end of the selection we should go to when the user hits the left or
 * right arrow key.
 */
static int _item_entry_get_better_cursor_x(GtkEntry *entry, int offset)
{
  GtkTextDirection keymap_direction =
  (gdk_keymap_get_direction(gdk_keymap_get_default()) == PANGO_DIRECTION_LTR) ?
  GTK_TEXT_DIR_LTR : GTK_TEXT_DIR_RTL;
  GtkTextDirection widget_direction = gtk_widget_get_direction(GTK_WIDGET(entry));
  _Bool split_cursor;

  PangoLayout *layout = gtk_item_entry_ensure_layout(entry, TRUE);
  int index = g_utf8_offset_to_pointer(entry->text, offset) - entry->text;

  PangoRectangle strong_pos, weak_pos;

  g_object_get(gtk_widget_get_settings(GTK_WIDGET(entry)),
               "gtk-split-cursor", &split_cursor,
               NULL);

  pango_layout_get_cursor_pos(layout, index, &strong_pos, &weak_pos);

  if (split_cursor)
    return strong_pos.x / PANGO_SCALE;
  else
    return (keymap_direction == widget_direction) ? strong_pos.x / PANGO_SCALE : weak_pos.x / PANGO_SCALE;
}

static void gtk_item_entry_move_cursor(GtkEntry       *entry,
                                       GtkMovementStep step,
                                       int             count,
                                       int             extend_selection)
{
  int new_pos = entry->current_pos;

  gtk_entry_reset_im_context(entry);

  if (entry->current_pos != entry->selection_bound && !extend_selection)
  {
    /* If we have a current selection and aren't extending it, move to the
     * start/or end of the selection as appropriate
     */
    switch(step) {

      case GTK_MOVEMENT_VISUAL_POSITIONS:
      {
        int current_x = _item_entry_get_better_cursor_x(entry, entry->current_pos);
        int bound_x = _item_entry_get_better_cursor_x(entry, entry->selection_bound);

        if (count < 0)
          new_pos = current_x < bound_x ? entry->current_pos : entry->selection_bound;
        else
          new_pos = current_x > bound_x ? entry->current_pos : entry->selection_bound;

        break;
      }
      case GTK_MOVEMENT_LOGICAL_POSITIONS:
      case GTK_MOVEMENT_WORDS:
        if (count < 0)
          new_pos = MIN(entry->current_pos, entry->selection_bound);
        else
          new_pos = MAX(entry->current_pos, entry->selection_bound);
        break;
      case GTK_MOVEMENT_DISPLAY_LINE_ENDS:
      case GTK_MOVEMENT_PARAGRAPH_ENDS:
      case GTK_MOVEMENT_BUFFER_ENDS:
        new_pos = count < 0 ? 0 : entry->text_length;
        break;
      case GTK_MOVEMENT_DISPLAY_LINES:
      case GTK_MOVEMENT_PARAGRAPHS:
      case GTK_MOVEMENT_PAGES:
      case GTK_MOVEMENT_HORIZONTAL_PAGES:
        break;
    }
  }
  else {

    switch(step)
    {
      case GTK_MOVEMENT_LOGICAL_POSITIONS:
        new_pos = gtk_item_entry_move_logically(entry, new_pos, count);
        break;
      case GTK_MOVEMENT_VISUAL_POSITIONS:
        new_pos = gtk_item_entry_move_visually(entry, new_pos, count);
        break;
      case GTK_MOVEMENT_WORDS:
        while (count > 0)
        {
          new_pos = gtk_item_entry_move_forward_word(entry, new_pos);
          count--;
        }
        while (count < 0)
        {
          new_pos = gtk_item_entry_move_backward_word(entry, new_pos);
          count++;
        }
        break;
      case GTK_MOVEMENT_DISPLAY_LINE_ENDS:
      case GTK_MOVEMENT_PARAGRAPH_ENDS:
      case GTK_MOVEMENT_BUFFER_ENDS:
        new_pos = count < 0 ? 0 : entry->text_length;
        break;
      case GTK_MOVEMENT_DISPLAY_LINES:
      case GTK_MOVEMENT_PARAGRAPHS:
      case GTK_MOVEMENT_PAGES:
      case GTK_MOVEMENT_HORIZONTAL_PAGES:
        break;
    }
  }

  if (extend_selection)
    gtk_editable_select_region(GTK_EDITABLE(entry), entry->selection_bound, new_pos);
  else
    gtk_editable_set_position(GTK_EDITABLE(entry), new_pos);

  gtk_item_entry_pend_cursor_blink(entry);
}

static void gtk_item_entry_insert_at_cursor(GtkEntry *entry, const char *str)
{
  GtkEditable *editable = GTK_EDITABLE(entry);
  int pos = entry->current_pos;

  if (entry->editable) {

    gtk_entry_reset_im_context(entry);

    gtk_editable_insert_text(editable, str, -1, &pos);
    gtk_editable_set_position(editable, pos);
  }
}

static void gtk_item_entry_delete_from_cursor(GtkEntry       *entry,
                                              GtkDeleteType   type,
                                              int             count)
{
  GtkEditable *editable = GTK_EDITABLE(entry);
  int start_pos = entry->current_pos;
  int end_pos = entry->current_pos;

  gtk_entry_reset_im_context(entry);

  if (!entry->editable)
    return;

  if (entry->selection_bound != entry->current_pos) {

    gtk_editable_delete_selection(editable);
    return;
  }

  switch(type) {

    case GTK_DELETE_CHARS:
      end_pos = gtk_item_entry_move_logically(entry, entry->current_pos, count);
      gtk_editable_delete_text(editable, MIN(start_pos, end_pos), MAX(start_pos, end_pos));
      break;
    case GTK_DELETE_WORDS:
      if (count < 0) {

        /* Move to end of current word, or if not on a word, end of previous word */
        end_pos = gtk_item_entry_move_backward_word(entry, end_pos);
        end_pos = gtk_item_entry_move_forward_word(entry, end_pos);
      }
      else if (count > 0) {

        /* Move to beginning of current word, or if not on a word, begining of next word */
        start_pos = gtk_item_entry_move_forward_word(entry, start_pos);
        start_pos = gtk_item_entry_move_backward_word(entry, start_pos);
      }

      /* Fall through */
      case GTK_DELETE_WORD_ENDS:
        while (count < 0) {

          start_pos = gtk_item_entry_move_backward_word(entry, start_pos);
          count++;
        }
        while (count > 0)
        {
          end_pos = gtk_item_entry_move_forward_word(entry, end_pos);
          count--;
        }
        gtk_editable_delete_text(editable, start_pos, end_pos);
        break;
      case GTK_DELETE_DISPLAY_LINE_ENDS:
      case GTK_DELETE_PARAGRAPH_ENDS:
        if (count < 0)
          gtk_editable_delete_text(editable, 0, entry->current_pos);
        else
          gtk_editable_delete_text(editable, entry->current_pos, -1);
        break;
      case GTK_DELETE_DISPLAY_LINES:
      case GTK_DELETE_PARAGRAPHS:
        gtk_editable_delete_text(editable, 0, -1);
        break;
      case GTK_DELETE_WHITESPACE:
        gtk_item_entry_delete_whitespace(entry);
        break;
  }

  gtk_item_entry_pend_cursor_blink(entry);
}

/* IM Context Callbacks */

static void gtk_item_entry_commit_cb(GtkIMContext *context,
                                     const char   *str,
                                     GtkEntry     *entry)
{
    gtk_item_entry_enter_text(entry, str);
}

static void
gtk_item_entry_preedit_changed_cb(GtkIMContext *context, GtkEntry *entry)
{
    char *preedit_string;
    int cursor_pos;

    gtk_im_context_get_preedit_string(entry->im_context, &preedit_string, NULL, &cursor_pos);
    entry->preedit_length = strlen(preedit_string);
    cursor_pos = CLAMP(cursor_pos, 0, g_utf8_strlen(preedit_string, -1));
    entry->preedit_cursor = cursor_pos;
    g_free(preedit_string);

    gtk_item_entry_recompute(entry);
}

static _Bool gtk_item_entry_retrieve_surrounding_cb(GtkIMContext *context,
                                                    GtkEntry     *entry)
{
  GtkEntryBuffer *buffer = _item_entry_get_buffer(entry);
  unsigned int bytes;
  char *ptr;

  bytes = gtk_entry_buffer_get_bytes(buffer);

  ptr = g_utf8_offset_to_pointer (entry->text, entry->current_pos);

  gtk_im_context_set_surrounding(context, entry->text, bytes, ptr - entry->text);

  return TRUE;
}

static _Bool gtk_item_entry_delete_surrounding_cb(GtkIMContext *slave,
                                                  int           offset,
                                                  int           n_chars,
                                                  GtkEntry     *entry)
{
  gtk_editable_delete_text(GTK_EDITABLE(entry),
                           entry->current_pos + offset,
                           entry->current_pos + offset + n_chars);

  return TRUE;
}


/* Internal functions
 */

/* Used for im_commit_cb and inserting Unicode chars */
static void gtk_item_entry_enter_text(GtkEntry *entry, const char *str)
{
  GtkEditable *editable = GTK_EDITABLE(entry);
  int tmp_pos;

  if (gtk_editable_get_selection_bounds(editable, NULL, NULL)) {
    gtk_editable_delete_selection(editable);
  }
  else {

    if (entry->overwrite_mode) {
      gtk_item_entry_delete_from_cursor(entry, GTK_DELETE_CHARS, 1);
    }
  }

  tmp_pos = entry->current_pos;
  gtk_editable_insert_text(editable, str, strlen(str), &tmp_pos);
  gtk_editable_set_position(editable, tmp_pos);
}

/* All changes to entry->current_pos and entry->selection_bound
 * should go through this function.
 */
static void gtk_item_entry_set_positions(GtkEntry *entry,
                                         int current_pos,
                                         int selection_bound)
{
  _Bool changed = FALSE;

  g_object_freeze_notify(G_OBJECT(entry));

  if (current_pos != -1 && entry->current_pos != current_pos) {

    entry->current_pos = current_pos;
    changed = TRUE;

    g_object_notify(G_OBJECT(entry), "cursor_position");
  }

  if (selection_bound != -1 && entry->selection_bound != selection_bound) {

    entry->selection_bound = selection_bound;
    changed = TRUE;

    g_object_notify(G_OBJECT(entry), "selection_bound");
  }

  g_object_thaw_notify(G_OBJECT(entry));

  if (changed) {
    gtk_item_entry_recompute(entry);
  }
}

static void _item_entry_update_im_cursor_location(GtkEntry *entry)
{
  GdkRectangle area;
  int strong_x;
  int strong_xoffset;
  int x, y, area_width, area_height;

  gtk_item_entry_get_cursor_locations(entry, CURSOR_STANDARD, &strong_x, NULL);

  _item_entry_get_text_area_size(entry, &x, &y, &area_width, &area_height);

  strong_xoffset = strong_x - entry->scroll_offset;
  if (strong_xoffset < 0) {

    strong_xoffset = 0;
  }
  else if (strong_xoffset > area_width) {

    strong_xoffset = area_width;
  }

  area.x = x + strong_xoffset;
  area.y = y + area_height;
  area.width = area_width;
  area.height = area_height;

  gtk_im_context_set_cursor_location(entry->im_context, &area);
}

static int _item_entry_recompute_idle_func(gpointer data)
{
  GtkEntry *entry;

  GDK_THREADS_ENTER();

  entry = GTK_ENTRY(data);

  gtk_item_entry_adjust_scroll(entry);
  gtk_item_entry_queue_draw(entry);

  entry->recompute_idle = FALSE;

  _item_entry_update_im_cursor_location(entry);

  GDK_THREADS_LEAVE();

  return FALSE;
}

static void gtk_item_entry_recompute(GtkEntry *entry)
{
  gtk_item_entry_reset_layout(entry);
  gtk_item_entry_check_cursor_blink(entry);

  if (!entry->recompute_idle) {

    entry->recompute_idle = g_idle_add_full(G_PRIORITY_HIGH_IDLE + 15, /* between resize and redraw */
                                            _item_entry_recompute_idle_func, entry, NULL);
  }
}

static void _item_entry_append_char(GString *str, gunichar ch, int count)
{
  int i;
  int char_len;
  char buf[7];

  char_len = g_unichar_to_utf8(ch, buf);

  i = 0;
  while (i < count) {
    g_string_append_len(str, buf, char_len);
    ++i;
  }
}

static PangoLayout *
gtk_item_entry_create_layout(GtkEntry *entry, _Bool  include_preedit)
{
  PangoLayout *layout = gtk_widget_create_pango_layout(GTK_WIDGET(entry), NULL);
  PangoAttrList *tmp_attrs = pango_attr_list_new();

  char *preedit_string = NULL;
  int preedit_length = 0;
  PangoAttrList *preedit_attrs = NULL;

  pango_layout_set_single_paragraph_mode(layout, TRUE);

  if (include_preedit) {

    gtk_im_context_get_preedit_string(entry->im_context,
                                      &preedit_string, &preedit_attrs, NULL);
    preedit_length = entry->preedit_length;
  }

  if (preedit_length) {

    GString *tmp_string = g_string_new(NULL);
    GtkEntryBuffer *buffer = _item_entry_get_buffer(entry);

    int cursor_index = g_utf8_offset_to_pointer(entry->text, entry->current_pos) - entry->text;

    if (entry->visible) {


      g_string_prepend_len(tmp_string, entry->text, gtk_entry_buffer_get_bytes(buffer));
      g_string_insert(tmp_string, cursor_index, preedit_string);
    }
    else {

      int ch_len;
      int preedit_len_chars;
      gunichar invisible_char;

      ch_len = g_utf8_strlen(entry->text, gtk_entry_buffer_get_bytes(buffer));
      preedit_len_chars = g_utf8_strlen(preedit_string, -1);
      ch_len += preedit_len_chars;

      if (entry->invisible_char != 0)
        invisible_char = entry->invisible_char;
      else
        invisible_char = ' '; /* just pick a char */

        _item_entry_append_char(tmp_string, invisible_char, ch_len);

      /* Fix cursor index to point to invisible char corresponding
       * to the preedit, fix preedit_length to be the length of
       * the invisible chars representing the preedit
       */
      cursor_index =
      g_utf8_offset_to_pointer(tmp_string->str, entry->current_pos) -
      tmp_string->str;
      preedit_length =
      preedit_len_chars *
      g_unichar_to_utf8(invisible_char, NULL);
    }

    pango_layout_set_text(layout, tmp_string->str, tmp_string->len);

    pango_attr_list_splice(tmp_attrs, preedit_attrs,
                           cursor_index, preedit_length);

    g_string_free(tmp_string, TRUE);
  }
  else {

    if (entry->visible) {

      GtkEntryBuffer *buffer = _item_entry_get_buffer(entry);
      pango_layout_set_text(layout, entry->text, gtk_entry_buffer_get_bytes(buffer));
    }
    else {

      GString *str = g_string_new(NULL);

      gunichar invisible_char;

      if (entry->invisible_char != 0)
        invisible_char = entry->invisible_char;
      else
        invisible_char = ' '; /* just pick a char */

        _item_entry_append_char(str, invisible_char, entry->text_length);
      pango_layout_set_text(layout, str->str, str->len);
      g_string_free(str, TRUE);
    }
  }

  pango_layout_set_attributes(layout, tmp_attrs);

  if (preedit_string) {
    g_free(preedit_string);
  }

  if (preedit_attrs) {
    pango_attr_list_unref(preedit_attrs);
  }

  pango_attr_list_unref(tmp_attrs);

  return layout;
}

static PangoLayout *
gtk_item_entry_ensure_layout(GtkEntry *entry, _Bool  include_preedit)
{
  if (entry->preedit_length > 0 &&
    !include_preedit != !entry->cache_includes_preedit)
    gtk_item_entry_reset_layout(entry);

  if (!entry->cached_layout) {
    entry->cached_layout = gtk_item_entry_create_layout(entry, include_preedit);
    entry->cache_includes_preedit = include_preedit;
  }

  return entry->cached_layout;
}

static void _item_entry_get_layout_position(GtkEntry *entry, int *x, int *y)
{
    PangoLayout     *layout;
    PangoLayoutLine *line;
    PangoRectangle   logical_rect;
    int area_width,  area_height;
    int y_pos;

    layout = gtk_item_entry_ensure_layout(entry, TRUE);

    _item_entry_get_text_area_size(entry, NULL, NULL, &area_width, &area_height);

    area_height = PANGO_SCALE * (area_height);

    line = pango_layout_get_lines(layout)->data;
    pango_layout_line_get_extents(line, NULL, &logical_rect);

    /* Align primarily for locale's ascent/descent */

    y_pos = ((area_height - entry->ascent - entry->descent) / 2 +
            entry->ascent + logical_rect.y);


    /* Now see if we need to adjust to fit in actual drawn string */

    if (logical_rect.height > area_height)
      y_pos = (area_height - logical_rect.height) / 2;
    else if (y_pos < 0)
      y_pos = 0;
    else if (y_pos + logical_rect.height > area_height)
      y_pos = area_height - logical_rect.height;

    y_pos = y_pos / PANGO_SCALE;

    if (x)
      *x = -entry->scroll_offset;

    if (y)
      *y = y_pos;
}

static void gtk_item_entry_draw_text(GtkEntry *entry)
{
  if (!entry->visible && entry->invisible_char == 0)
    return;

  if (gtk_widget_is_drawable(GTK_WIDGET(entry))) {

    GtkWidget *widget;

    PangoLayout *layout = gtk_item_entry_ensure_layout(entry, TRUE);
    int area_width, area_height;

    int x, y;
    int start_pos, end_pos;

    widget = GTK_WIDGET(entry);

    _item_entry_get_layout_position(entry, &x, &y);

    _item_entry_get_text_area_size(entry, NULL, NULL, &area_width, &area_height);


    gdk_draw_layout(entry->text_area,
                    gtk_widget_get_style(widget)->text_gc[gtk_widget_get_state(widget)],
                    x, y,
                    layout);


    if (gtk_editable_get_selection_bounds(GTK_EDITABLE(entry), &start_pos, &end_pos))
    {

      int *ranges;
      int n_ranges, i;

      PangoLayoutLine *line;
      PangoRectangle logical_rect;

      const char *text = pango_layout_get_text(layout);
      int start_index = g_utf8_offset_to_pointer(text, start_pos) - text;
      int end_index = g_utf8_offset_to_pointer(text, end_pos) - text;
      GdkRegion *clip_region = gdk_region_new();
      GdkGC *text_gc;
      GdkGC *selection_gc;

      line = pango_layout_get_lines(layout)->data;

      pango_layout_line_get_x_ranges(line, start_index, end_index, &ranges, &n_ranges);

      pango_layout_get_extents(layout, NULL, &logical_rect);

      if (gtk_widget_has_focus(GTK_WIDGET(entry))) {

        selection_gc = gtk_widget_get_style(widget)->base_gc[GTK_STATE_SELECTED];
        text_gc = gtk_widget_get_style(widget)->text_gc[GTK_STATE_SELECTED];
      }
      else {

        selection_gc = gtk_widget_get_style(widget)->base_gc[GTK_STATE_ACTIVE];
        text_gc = gtk_widget_get_style(widget)->text_gc[GTK_STATE_ACTIVE];
      }

      for (i = 0; i < n_ranges; i++) {

        GdkRectangle rect;

        rect.x = INNER_BORDER - entry->scroll_offset + ranges[2 * i] / PANGO_SCALE;
        rect.y = y;
        rect.width = (ranges[2 * i + 1] - ranges[2 * i]) / PANGO_SCALE;
        rect.height = logical_rect.height / PANGO_SCALE;

        gdk_draw_rectangle(entry->text_area, selection_gc, TRUE,
                           rect.x, rect.y, rect.width, rect.height);

        gdk_region_union_with_rect(clip_region, &rect);
      }

      gdk_gc_set_clip_region(text_gc, clip_region);
      gdk_draw_layout(entry->text_area, text_gc, x, y, layout);
      gdk_gc_set_clip_region(text_gc, NULL);

      gdk_region_destroy(clip_region);
      g_free(ranges);
    }
  }
}

/*
 * From _gtk_get_insertion_cursor_gc
 */

typedef struct _CursorInfo CursorInfo;

struct _CursorInfo
{
    GType for_type;
    GdkGC *primary_gc;
    GdkGC *secondary_gc;
};

static GdkGC *
_item_entry_make_cursor_gc(GtkWidget  *widget,
                           const char *property_name,
                           GdkColor   *fallback)
{
  GdkGCValues gc_values;
  GdkGCValuesMask gc_values_mask;
  GdkColor *cursor_color;

  gtk_widget_style_get(widget, property_name, &cursor_color, NULL);

  gc_values_mask = GDK_GC_FOREGROUND;

  if (cursor_color) {

    gc_values.foreground = *cursor_color;
    gdk_color_free(cursor_color);
  }
  else {
    gc_values.foreground = *fallback;
  }

  gdk_rgb_find_color(gtk_widget_get_style(widget)->colormap,
                     &gc_values.foreground);

  return gtk_gc_get(gtk_widget_get_style(widget)->depth,
                    gtk_widget_get_style(widget)->colormap,
                    &gc_values, gc_values_mask);
}

static GdkGC*
_item_entry_get_insertion_cursor_gc(GtkWidget *widget, _Bool is_primary)
{
  CursorInfo *cursor_info;
  GtkStyle   *style;
  GdkGC      *gc;

  style       = gtk_widget_get_style(widget);
  cursor_info = g_object_get_data((GObject*)style, "gtk-style-cursor-info");

  if (!cursor_info) {

    cursor_info = g_malloc(sizeof(CursorInfo));

    cursor_info->primary_gc   = NULL;
    cursor_info->secondary_gc = NULL;
    cursor_info->for_type     = G_TYPE_INVALID;

    g_object_set_data((GObject*)style, "gtk-style-cursor-info", cursor_info);
  }

  /* We have to keep track of the type because gtk_widget_style_get()
   * can return different results when called on the same property and
   * same style but for different widgets. :-(. That is,
   * GtkEntry::cursor-color = "red" in a style will modify the cursor
   * color for entries but not for text view.
   */
  if (cursor_info->for_type != G_OBJECT_TYPE(widget)) {

    cursor_info->for_type = G_OBJECT_TYPE(widget);

    if (cursor_info->primary_gc) {

      gtk_gc_release(cursor_info->primary_gc);
      cursor_info->primary_gc = NULL;
    }
    if (cursor_info->secondary_gc) {

      gtk_gc_release(cursor_info->secondary_gc);
      cursor_info->secondary_gc = NULL;
    }
  }

  if (is_primary) {

    if (!cursor_info->primary_gc) {
      cursor_info->primary_gc = _item_entry_make_cursor_gc(widget,
                                                           "cursor-color",
                                                           &style->black);
    }
    gc = cursor_info->primary_gc;
  }
  else {

    static GdkColor gray = { 0, 0x8888, 0x8888, 0x8888 };

    if (!cursor_info->secondary_gc) {
      cursor_info->secondary_gc = _item_entry_make_cursor_gc(widget,
                                                             "secondary-cursor-color",
                                                             &gray);
    }
    gc = cursor_info->secondary_gc;
  }

  return g_object_ref(gc);
}

/*
 * From _gtk_draw_insertion_cursor
 */
static void
_item_entry_draw_insertion_cursor(GtkWidget       *widget,
                                  GdkDrawable     *drawable,
                                  GdkGC           *gc,
                                  GdkRectangle    *location,
                                  GtkTextDirection direction,
                                  _Bool            draw_arrow)
{
  float cursor_aspect_ratio;
  int stem_width;
  int arrow_width;
  int i;
  int offset;

  g_return_if_fail(direction != GTK_TEXT_DIR_NONE);

  gtk_widget_style_get(widget, "cursor-aspect-ratio", &cursor_aspect_ratio, NULL);

  stem_width = location->height * cursor_aspect_ratio + 1;
  arrow_width = stem_width + 1;

  /* put (stem_width % 2) on the proper side of the cursor */
  if (direction == GTK_TEXT_DIR_LTR)
    offset = stem_width / 2;
  else
    offset = stem_width - stem_width / 2;

  for (i = 0; i < stem_width; i++)  {
    gdk_draw_line (drawable, gc, location->x + i - offset, location->y,
                                 location->x + i - offset, location->y +
                                 + location->height - 1);
  }

  if (draw_arrow) {

    int x, y;

    if (direction == GTK_TEXT_DIR_RTL) {

      x = location->x - offset - 1;
      y = location->y + location->height - arrow_width * 2 - arrow_width + 1;

      for (i = 0; i < arrow_width; i++)
      {
        gdk_draw_line(drawable, gc,
                      x, y + i + 1,
                      x, y + 2 * arrow_width - i - 1);
        x--;
      }
    }
    else if (direction == GTK_TEXT_DIR_LTR) {

      x = location->x + stem_width - offset;
      y = location->y + location->height - arrow_width * 2 - arrow_width + 1;

      for (i = 0; i < arrow_width; i++)
      {
        gdk_draw_line(drawable, gc,
                      x, y + i + 1,
                      x, y + 2 * arrow_width - i - 1);
        x++;
      }
    }
  }
}

static void
gtk_item_entry_draw_cursor(GtkEntry *entry, CursorType type)
{
  GtkTextDirection keymap_direction =
  (gdk_keymap_get_direction(gdk_keymap_get_default()) == PANGO_DIRECTION_LTR) ?
  GTK_TEXT_DIR_LTR : GTK_TEXT_DIR_RTL;
  GtkTextDirection widget_direction = gtk_widget_get_direction(GTK_WIDGET(entry));

  if (gtk_widget_is_drawable(GTK_WIDGET(entry)) && GTK_ENTRY(entry)->cursor_visible)
  {
    GtkWidget *widget = GTK_WIDGET(entry);
    GdkRectangle cursor_location;
    _Bool split_cursor;

    int xoffset = INNER_BORDER - entry->scroll_offset;
    int strong_x, weak_x;
    int text_area_height;
    GtkTextDirection dir1;
    GtkTextDirection dir2 = GTK_TEXT_DIR_NONE;
    int x1 = 0;
    int x2 = 0;
    GdkGC *gc;

    text_area_height = gdk_window_get_height(entry->text_area);

    gtk_item_entry_get_cursor_locations(entry, type, &strong_x, &weak_x);

    g_object_get(gtk_widget_get_settings(widget),
                 "gtk-split-cursor", &split_cursor,
                 NULL);

    dir1 = widget_direction;

    if (split_cursor) {

      x1 = strong_x;

      if (weak_x != strong_x) {
        dir2 = (widget_direction == GTK_TEXT_DIR_LTR) ? GTK_TEXT_DIR_RTL : GTK_TEXT_DIR_LTR;
        x2   = weak_x;
      }
    }
    else {

      if (keymap_direction == widget_direction)
        x1 = strong_x;
      else
        x1 = weak_x;
    }

    cursor_location.x = xoffset + x1;
    cursor_location.y = INNER_BORDER;
    cursor_location.width = 0;
    cursor_location.height = text_area_height - 2 * INNER_BORDER;

    gc = _item_entry_get_insertion_cursor_gc(widget, TRUE);
    _item_entry_draw_insertion_cursor(widget, entry->text_area, gc,
                                      &cursor_location, dir1,
                                      dir2 != GTK_TEXT_DIR_NONE);
    g_object_unref(gc);

    if (dir2 != GTK_TEXT_DIR_NONE) {

      cursor_location.x = xoffset + x2;
      gc = _item_entry_get_insertion_cursor_gc(widget, FALSE);
      _item_entry_draw_insertion_cursor(widget, entry->text_area, gc,
                                        &cursor_location, dir2,
                                        TRUE);
      g_object_unref(gc);
    }
  }
}

static void
gtk_item_entry_queue_draw(GtkEntry *entry)
{
    if (gtk_widget_get_realized(GTK_WIDGET(entry)))
	gdk_window_invalidate_rect(entry->text_area, NULL, FALSE);
}

#if GTK_CHECK_VERSION(2,21,0) == 0
static void
gtk_entry_reset_im_context(GtkEntry *entry)
{
    if (entry->need_im_reset)
    {
	entry->need_im_reset = 0;
	gtk_im_context_reset(entry->im_context);
    }
}
#endif /* GTK_CHECK_VERSION(2,21,0) == 0 */

static void
gtk_item_entry_get_cursor_locations(GtkEntry   *entry,
                                    CursorType  type,
                                    int        *strong_x,
                                    int        *weak_x)
{
  PangoLayout *layout = gtk_item_entry_ensure_layout(entry, TRUE);

  PangoRectangle strong_pos, weak_pos;
  int index;

  if (type == CURSOR_STANDARD) {

    const char *text;

    text  = pango_layout_get_text(layout);
    index = g_utf8_offset_to_pointer(text, entry->current_pos + entry->preedit_cursor) - text;
  }
  else { /* type == CURSOR_DND */

    index = g_utf8_offset_to_pointer(entry->text, entry->dnd_position) - entry->text;

    if (entry->dnd_position > entry->current_pos)
      index += entry->preedit_length;
  }

  pango_layout_get_cursor_pos(layout, index, &strong_pos, &weak_pos);

  if (strong_x)
    *strong_x = strong_pos.x / PANGO_SCALE;

  if (weak_x)
    *weak_x = weak_pos.x / PANGO_SCALE;
}

static void
gtk_item_entry_adjust_scroll(GtkEntry *entry)
{
  int text_area_width;
  int strong_x, weak_x;
  PangoLayout *layout;
  PangoLayoutLine *line;
  PangoRectangle logical_rect;
  GtkItemEntry *item_entry;
  int text_width;

  if (!gtk_widget_get_realized(GTK_WIDGET(entry)))
    return;

  item_entry = GTK_ITEM_ENTRY(entry);

  text_area_width  = gdk_window_get_width(entry->text_area);
  text_area_width -= 2 * INNER_BORDER;

  layout = gtk_item_entry_ensure_layout(entry, TRUE);
  line = pango_layout_get_lines(layout)->data;

  pango_layout_line_get_extents(line, NULL, &logical_rect);
  text_width = logical_rect.width / PANGO_SCALE + 2; /* 2 for cursor */

  gtk_item_entry_get_cursor_locations(entry, CURSOR_STANDARD, &strong_x, &weak_x);

  /* Display as much text as we can */

  if (gtk_widget_get_direction(GTK_WIDGET(entry)) == GTK_TEXT_DIR_LTR)
  {
    entry->scroll_offset = 0;
    switch(item_entry->justification)
    {

      case GTK_JUSTIFY_FILL:
      case GTK_JUSTIFY_LEFT:

        /* LEFT JUSTIFICATION */

        strong_x -= entry->scroll_offset;
        if (strong_x < 0) {
          entry->scroll_offset += strong_x;
        }
        else if (strong_x > text_area_width) {

          if (item_entry->text_max_size != 0 &&
            text_area_width + 2 <= item_entry->text_max_size)
          {
            GtkAllocation allocation;
            gtk_widget_get_allocation(GTK_WIDGET(entry), &allocation);
            allocation.width += text_width - text_area_width;
            entry->scroll_offset = 0;
            gtk_item_entry_size_allocate(GTK_WIDGET(entry), &allocation);
          }
          else
          {
            entry->scroll_offset += (strong_x - text_area_width) + 1;
          }
        }

        break;

      case GTK_JUSTIFY_RIGHT:

        /* RIGHT JUSTIFICATION FOR NUMBERS */
        if (entry->text) {

          entry->scroll_offset =  -(text_area_width - text_width) + 1;
          if (entry->scroll_offset > 0) {

            if (item_entry->text_max_size != 0 &&
              text_area_width + 2 <= item_entry->text_max_size)
            {
              GtkAllocation allocation;
              gtk_widget_get_allocation(GTK_WIDGET(entry), &allocation);
              allocation.x -= text_width - text_area_width;
              allocation.width += text_width - text_area_width;
              entry->scroll_offset = 0;
              gtk_item_entry_size_allocate(GTK_WIDGET(entry), &allocation);
            }
            else
            {
              entry->scroll_offset = -(text_area_width - strong_x) + 1;
              if (entry->scroll_offset < 0)
                entry->scroll_offset = 0;
            }
          }
        }
        else
          entry->scroll_offset = 0;

        break;
      case GTK_JUSTIFY_CENTER:

        if (entry->text) {

          entry->scroll_offset =  -(text_area_width - text_width) / 2;

          if (entry->scroll_offset > 0) {

            if (item_entry->text_max_size != 0 &&
              text_area_width + 1 <= item_entry->text_max_size)
            {
              GtkAllocation allocation;
              gtk_widget_get_allocation(GTK_WIDGET(entry), &allocation);
              allocation.x += (text_area_width / 2 - text_width / 2);
              allocation.width += text_width - text_area_width;
              entry->scroll_offset = 0;
              gtk_item_entry_size_allocate(GTK_WIDGET(entry), &allocation);
            }
            else
            {
              entry->scroll_offset = -(text_area_width - strong_x) + 1;
              if (entry->scroll_offset < 0)
                entry->scroll_offset = 0;
            }
          }
        }
        else  {
          entry->scroll_offset = 0;
        }
        break;

    }
  }
  else {

    int max_offset = text_width - text_area_width;
    int min_offset = MIN(0, max_offset);

    entry->scroll_offset = CLAMP(entry->scroll_offset, min_offset, max_offset);
  }

  g_object_notify((GObject*)entry, "scroll_offset");
}

static int
gtk_item_entry_move_visually(GtkEntry *entry, int start, int count)
{
    int index;
    PangoLayout *layout = gtk_item_entry_ensure_layout(entry, FALSE);
    const char *text;

    text = pango_layout_get_text(layout);

    index = g_utf8_offset_to_pointer(text, start) - text;

    while (count != 0) {

      int new_index, new_trailing;
      _Bool split_cursor;
      _Bool strong;

      g_object_get(gtk_widget_get_settings(GTK_WIDGET(entry)),
                   "gtk-split-cursor", &split_cursor,
                   NULL);

      if (split_cursor) {
        strong = TRUE;
      }
      else {

        GtkTextDirection keymap_direction =
        (gdk_keymap_get_direction(gdk_keymap_get_default()) == PANGO_DIRECTION_LTR) ?
        GTK_TEXT_DIR_LTR : GTK_TEXT_DIR_RTL;

        strong = keymap_direction == gtk_widget_get_direction(GTK_WIDGET(entry));
      }

      if (count > 0) {
        pango_layout_move_cursor_visually(layout, strong, index, 0, 1, &new_index, &new_trailing);
        count--;
      }
      else {
        pango_layout_move_cursor_visually(layout, strong, index, 0, -1, &new_index, &new_trailing);
        count++;
      }

      if (new_index < 0 || new_index == G_MAXINT)
        break;

      index = new_index;

      while (new_trailing--) index = g_utf8_next_char(entry->text + new_index) - entry->text;
    }

    return g_utf8_pointer_to_offset(text, text + index);
}

static int
gtk_item_entry_move_logically(GtkEntry *entry, int start, int count)
{
  int new_pos = start;

  /* Prevent any leak of information */
  if (!entry->visible) {
    new_pos = CLAMP(start + count, 0, entry->text_length);
  }
  else if (entry->text) {

    PangoLayout *layout = gtk_item_entry_ensure_layout(entry, FALSE);
    PangoLogAttr *log_attrs;
    int n_attrs;

    pango_layout_get_log_attrs(layout, &log_attrs, &n_attrs);

    while (count > 0 && new_pos < entry->text_length) {

      do new_pos++;
      while (new_pos < entry->text_length && !log_attrs[new_pos].is_cursor_position);

      count--;
    }

    while (count < 0 && new_pos > 0) {

      do new_pos--;
      while (new_pos > 0 && !log_attrs[new_pos].is_cursor_position);

      count++;
    }

    g_free(log_attrs);
  }

  return new_pos;
}

static int
gtk_item_entry_move_forward_word(GtkEntry *entry, int start)
{
  int new_pos = start;

  /* Prevent any leak of information */
  if (!entry->visible) {
    new_pos = entry->text_length;
  }
  else if (entry->text && (new_pos < entry->text_length))  {

    PangoLayout *layout = gtk_item_entry_ensure_layout(entry, FALSE);
    PangoLogAttr *log_attrs;
    int n_attrs;

    pango_layout_get_log_attrs(layout, &log_attrs, &n_attrs);

    /* Find the next word end */
    new_pos++;
    while (new_pos < n_attrs && !log_attrs[new_pos].is_word_end) new_pos++;

    g_free(log_attrs);
  }

  return new_pos;
}


static int
gtk_item_entry_move_backward_word(GtkEntry *entry, int start)
{
  int new_pos = start;

  /* Prevent any leak of information */
  if (!entry->visible) {
    new_pos = 0;
  }
  else if (entry->text && start > 0) {

    PangoLayout *layout = gtk_item_entry_ensure_layout(entry, FALSE);
    PangoLogAttr *log_attrs;
    int n_attrs;

    pango_layout_get_log_attrs(layout, &log_attrs, &n_attrs);

    new_pos = start - 1;

    /* Find the previous word beginning */
    while (new_pos > 0 && !log_attrs[new_pos].is_word_start) new_pos--;

    g_free(log_attrs);
  }

  return new_pos;
}

static void
gtk_item_entry_delete_whitespace(GtkEntry *entry)
{
  PangoLayout *layout = gtk_item_entry_ensure_layout(entry, FALSE);
  PangoLogAttr *log_attrs;
  int n_attrs;
  int start, end;

  pango_layout_get_log_attrs(layout, &log_attrs, &n_attrs);

  start = end = entry->current_pos;

  while (start > 0 && log_attrs[start - 1].is_white) start--;

  while (end < n_attrs && log_attrs[end].is_white) end++;

  g_free(log_attrs);

  if (start != end) {
    gtk_editable_delete_text(GTK_EDITABLE(entry), start, end);
  }
}


/*
 * Like gtk_editable_get_chars, but if the editable is not
 * visible, return asterisks; also convert result to UTF-8.
 */
static char *
gtk_item_entry_get_public_chars(GtkEntry *entry, int start, int end)
{
  if (end < 0) {
    end = entry->text_length;
  }

  if (entry->visible) {
    return gtk_editable_get_chars(GTK_EDITABLE(entry), start, end);
  }
  else {

    char *str;
    int i;
    int n_chars = end - start;

    str = g_malloc(n_chars + 1);

    for (i = 0; i < n_chars; i++) {
      str[i] = '*';
    }

    str[i] = '\0';

    return str;
  }
}

static void _item_entry_primary_get_cb(GtkClipboard     *clipboard,
                                       GtkSelectionData *selection_data,
                                       unsigned int      info,
                                       void             *data)
{
    GtkEntry *entry = GTK_ENTRY(data);
    int start, end;

    if (gtk_editable_get_selection_bounds((GtkEditable*)entry, &start, &end))
    {
      char *str = gtk_item_entry_get_public_chars(entry, start, end);
      gtk_selection_data_set_text(selection_data, str, -1);
      g_free(str);
    }
}

static void _item_entry_primary_clear_cb(GtkClipboard *clipboard, void *data)
{
    GtkEntry *entry = GTK_ENTRY(data);

    gtk_editable_select_region((GtkEditable*)entry, entry->current_pos, entry->current_pos);
}

static void gtk_item_entry_update_primary_selection(GtkEntry *entry)
{
  static const GtkTargetEntry targets[] = {
      { "UTF8_STRING", 0, 0 },
      { "STRING", 0, 0 },
      { "TEXT",   0, 0 },
      { "COMPOUND_TEXT", 0, 0 }
  };

  GtkClipboard *clipboard;
  int start, end;

  clipboard = gtk_clipboard_get(GDK_SELECTION_PRIMARY);

  if (gtk_editable_get_selection_bounds(GTK_EDITABLE(entry), &start, &end))
  {
    if (!gtk_clipboard_set_with_owner(clipboard, targets, G_N_ELEMENTS(targets),
      _item_entry_primary_get_cb, _item_entry_primary_clear_cb, (GObject*)entry))
    {
      _item_entry_primary_clear_cb(clipboard, entry);
    }
  }
  else
  {
    if (gtk_clipboard_get_owner(clipboard) == (GObject*)entry) {
      gtk_clipboard_clear(clipboard);
    }
  }
}

/* Public API
 */

GtkWidget *gtk_item_entry_new(void)
{
  return gtk_widget_new(G_TYPE_ITEM_ENTRY, NULL);
}

/*!
 * \brief gtk_item_entry_new_with_max_length
 * \par Function Description
 *  Creates a new #GtkItemEntry with the maximum allowed number
 *  of characters in the contents of the widget. If the current
 *  contents are longer than the given length, then they will be
 *  truncated to fit.
 *
 * \param max the maximum character length of the entry, or 0 for no
 *            maximum. (other than the maximum length of entries.) The
 *            value passed in will be clamped to the range 0-65536.
 *
 * \returns the newly-created #GtkItemEntry widget.
 */
GtkWidget *gtk_item_entry_new_with_max_length(int max)
{
  GtkItemEntry *entry;

  entry = g_object_new(G_TYPE_ITEM_ENTRY, NULL);

  gtk_entry_set_max_length(GTK_ENTRY(entry), max);

  return GTK_WIDGET(entry);
}

/*!
 * \brief gtk_item_entry_set_text
 * \par Function Description
 *  Sets the text in the widget to the given value, replacing the current contents.
 *
 * \param entry         a #GtkItemEntry
 * \param text          the new text
 * \param justification a #GtkJustification : GTK_JUSTIFY_LEFT, GTK_JUSTIFY_RIGHT,
 *                                             GTK_JUSTIFY_CENTER,GTK_JUSTIFY_FILL
 */
void gtk_item_entry_set_text(GtkItemEntry    *item_entry,
                             const char      *text,
                             GtkJustification justification)
{
  g_return_if_fail(GTK_IS_ITEM_ENTRY(item_entry));
  g_return_if_fail(text != NULL);

  GtkEntry *entry = (GtkEntry*)item_entry;

  item_entry->justification = justification;

  /* Actually setting the text will affect the cursor and selection;
   * if the contents don't actually change, this will look odd to the user.
   */
  if (entry->text && strcmp(entry->text, text) == 0) {
    return;
  }

  if (entry->recompute_idle) {
    g_source_remove(entry->recompute_idle);
    entry->recompute_idle = 0;
  }

  if (entry->blink_timeout) {
    g_source_remove(entry->blink_timeout);
    entry->blink_timeout = 0;
  }

  gtk_editable_delete_text((GtkEditable*)entry, 0, -1);

  item_entry->item_n_bytes = 0;    /* rraptor edited */

  if (text[0]) {
    int tmp_pos = 0;
    gtk_editable_insert_text((GtkEditable*)entry, text, -1, &tmp_pos);
  }
}

/*!
 * \brief gtk_item_entry_get_layout_offsets
 * \par Function Description
 * Obtains the position of the #PangoLayout used to render text
 * in the entry, in widget coordinates. Useful if you want to line
 * up the text in an entry with some other text, e.g. when using the
 * entry to implement editable cells in a sheet widget.
 *
 * Also useful to convert mouse events into coordinates inside the
 * #PangoLayout, e.g. to take some action if some part of the entry text
 * is clicked.
 *
 * Note that as the user scrolls around in the entry the offsets will
 * change; you'll need to connect to the "notify::scroll_offset"
 * signal to track this. Remember when using the #PangoLayout
 * functions you need to convert to and from pixels using
 * PANGO_PIXELS() or #PANGO_SCALE.
 *
 * Keep in mind that the layout text may contain a preedit string, so
 * gtk_entry_layout_index_to_text_index() and
 * gtk_entry_text_index_to_layout_index() are needed to convert byte
 * indices in the layout to byte indices in the entry contents.
 *
 * \param entry  a #GtkEntry
 * \param x      location to store X offset of layout, or %NULL
 * \param y      location to store Y offset of layout, or %NULL
 */
void
gtk_item_entry_get_layout_offsets(GtkItemEntry *entry, int *x, int *y)
{
    int text_area_x, text_area_y;

    g_return_if_fail(GTK_IS_ITEM_ENTRY(entry));

    /* this gets coords relative to text area */
    _item_entry_get_layout_position((GtkEntry*)entry, x, y);

    /* convert to widget coords */
    _item_entry_get_text_area_size((GtkEntry*)entry, &text_area_x, &text_area_y, NULL, NULL);

    if (x)
      *x += text_area_x;

    if (y)
      *y += text_area_y;
}

/*!
 * \brief gtk_item_entry_get_max_length_bytes
 * \par Function Description
 *  Retrieves the maximum byte length for the contents of #GtkItemEntry.
 *
 * \param item_entry a #GtkItemEntry
 *
 * \retval maximum byte length or 0.
 *
 * Since: 3.0.6
 */
int
gtk_item_entry_get_max_length_bytes(GtkItemEntry *item_entry)
{
    g_return_val_if_fail(GTK_IS_ITEM_ENTRY(item_entry), 0);
    return item_entry->max_length_bytes;
}

/*!
 * \brief gtk_item_entry_set_max_length_bytes
 * \par Function Description
 *  Sets the maximum byte length for the contents of the
 *  #GtkItemEntry. Existing content will not be truncted.
 *
 * \param item_entry        a #GtkItemEntry
 * \param max_length_bytes  maximum byte length or 0
 *
 * Since: 3.0.6
 */
void gtk_item_entry_set_max_length_bytes(GtkItemEntry *item_entry,
    int max_length_bytes)
{
    g_return_if_fail(item_entry != NULL);
    g_return_if_fail(GTK_IS_ITEM_ENTRY(item_entry));

    if (max_length_bytes < 0)
      max_length_bytes = 0;

    if (max_length_bytes > GTK_ENTRY_BUFFER_MAX_SIZE)
      max_length_bytes = GTK_ENTRY_BUFFER_MAX_SIZE;

    item_entry->max_length_bytes = max_length_bytes;
}

/*!
 * \brief gtk_item_entry_set_justification
 * \par Function Description
 *  Sets justification of the widget to the given value, replacing the current one.
 *
 * \param entry  a #GtkItemEntry
 * \param just   a #GtkJustification : GTK_JUSTIFY_LEFT,GTK_JUSTIFY_RIGHT,GTK_JUSTIFY_CENTER,GTK_JUSTIFY_FILL
 */
void
gtk_item_entry_set_justification(GtkItemEntry *entry, GtkJustification just)
{
    g_return_if_fail(GTK_IS_ITEM_ENTRY(entry));

    entry->justification = just;
}


/* We display the cursor when
 *
 *  - the selection is empty, AND
 *  - the widget has focus
 */

#define CURSOR_ON_MULTIPLIER 0.66
#define CURSOR_OFF_MULTIPLIER 0.34
#define CURSOR_PEND_MULTIPLIER 1.0

static _Bool
_item_entry_cursor_blinks(GtkEntry *entry)
{
  GtkSettings *settings = gtk_widget_get_settings((GtkWidget*)entry);
  _Bool blink;

  if (gtk_widget_has_focus((GtkWidget*)entry) &&
      entry->selection_bound == entry->current_pos)
  {
    g_object_get((GObject*)settings, "gtk-cursor-blink", &blink, NULL);
    return blink;
  }

  return FALSE;
}

static int
_item_entry_get_cursor_time(GtkEntry *entry)
{
    GtkSettings *settings = gtk_widget_get_settings((GtkWidget*)entry);
    int time;

    g_object_get((GObject*)settings, "gtk-cursor-blink-time", &time, NULL);

    return time;
}

static void
_item_entry_show_cursor(GtkEntry *entry)
{
  if (!entry->cursor_visible) {

    entry->cursor_visible = TRUE;

    if (gtk_widget_has_focus((GtkWidget*)entry) && entry->selection_bound == entry->current_pos)
      gtk_widget_queue_draw((GtkWidget*)entry);
  }
}

static void
_item_entry_hide_cursor(GtkEntry *entry)
{
  if (entry->cursor_visible) {

    entry->cursor_visible = FALSE;

    if (gtk_widget_has_focus((GtkWidget*)entry) && entry->selection_bound == entry->current_pos)
      gtk_widget_queue_draw((GtkWidget*)entry);
  }
}

/*
 * Blink!
 */
static int
_item_entry_blink_cb(void *data)
{
    GtkEntry *entry;

    GDK_THREADS_ENTER();

    entry = GTK_ENTRY(data);

    g_assert(gtk_widget_has_focus((GtkWidget*)entry));
    g_assert(entry->selection_bound == entry->current_pos);

    if (entry->cursor_visible) {

    _item_entry_hide_cursor(entry);
    entry->blink_timeout = g_timeout_add_full( 0,
        _item_entry_get_cursor_time(entry) * CURSOR_OFF_MULTIPLIER,
        _item_entry_blink_cb,
        entry,
        NULL);
    }
    else {

      _item_entry_show_cursor(entry);
      entry->blink_timeout =
         g_timeout_add_full( 0,
                             _item_entry_get_cursor_time(entry) * CURSOR_ON_MULTIPLIER,
                             _item_entry_blink_cb,
                             entry,
                             NULL);
    }

    GDK_THREADS_LEAVE();

    /* Remove ourselves */
    return FALSE;
}

static void
gtk_item_entry_check_cursor_blink(GtkEntry *entry)
{
  if (_item_entry_cursor_blinks(entry)) {

    if (!entry->blink_timeout) {

      entry->blink_timeout =
        g_timeout_add_full( 0,
                            _item_entry_get_cursor_time(entry) * CURSOR_ON_MULTIPLIER,
                            _item_entry_blink_cb,
                            entry,
                            NULL);
      _item_entry_show_cursor(entry);
    }
  }
  else {

    if (entry->blink_timeout) {

      g_source_remove(entry->blink_timeout);
      entry->blink_timeout = 0;
    }

    entry->cursor_visible = TRUE;
  }

}

static void
gtk_item_entry_pend_cursor_blink(GtkEntry *entry)
{
  if (_item_entry_cursor_blinks(entry)) {

    if (entry->blink_timeout != 0) {
      g_source_remove(entry->blink_timeout);
    }

    entry->blink_timeout = g_timeout_add_full( 0,
      _item_entry_get_cursor_time(entry) * CURSOR_PEND_MULTIPLIER,
                                              _item_entry_blink_cb,
                                              entry,
                                              NULL);
    _item_entry_show_cursor(entry);
  }
}

/*!
 * \brief gtk_item_set_cursor_visible
 * \par Function Description
 *  Sets the cursor visibility in the widget.
 *
 * \param entry    a #GtkItemEntry
 * \param visible  TRUE for visible or FALSE for invisible
 */
void
gtk_item_entry_set_cursor_visible(GtkItemEntry *entry, _Bool visible)
{
    g_return_if_fail(GTK_IS_ITEM_ENTRY(entry));

    ((GtkEntry*)entry)->cursor_visible = visible;
}

/*!
 * \brief gtk_item_get_cursor_visible
 * \par Function Description
 * Gets the cursor visibility in the widget.
 *
 * \param entry a #GtkItemEntry
 *
 * \returns TRUE for visible or FALSE for invisible
 */
_Bool
gtk_item_entry_get_cursor_visible(GtkItemEntry *entry)
{
    g_return_val_if_fail(GTK_IS_ITEM_ENTRY(entry), FALSE);

    return ((GtkEntry*)entry)->cursor_visible;
}
