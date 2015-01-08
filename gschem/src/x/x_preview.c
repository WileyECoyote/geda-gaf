/* -*- C x_preview.c indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4 -*-
 *
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 1998-2014 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */
/*!
 * \file x_preview.c
 * \brief Preview Widget used in dialog boxes
 */

#include <config.h>

#include "gschem.h"

#include <geda_debug.h>

#define OVER_ZOOM_FACTOR 0.1

enum {
  PROP_FILENAME=1,
  PROP_BUFFER,
  PROP_ACTIVE,
  PROP_LARGE,
};

static GObjectClass *preview_parent_class = NULL;

static void preview_class_init   (PreviewClass   *class);
static void preview_init         (Preview        *preview);
static void preview_set_property (GObject        *object,
                                  unsigned int    property_id,
                                  const GValue   *value,
                                  GParamSpec     *pspec);
static void preview_get_property (GObject        *object,
                                  unsigned int    property_id,
                                  GValue         *value,
                                  GParamSpec     *pspec);
static void preview_dispose      (GObject        *self);
static void preview_finalize     (GObject        *self);

/*! \brief get the filename for the current page
 */
static char*
preview_get_filename (Preview *preview)
{
 Page *page = preview->preview_window->toplevel->page_current;

 g_return_val_if_fail (GEDA_IS_PAGE(page), "");

 return page->filename;
}

/*! \brief Invalidate the Preview Drawing/Viewing area
 *
 *  \par Function Description
 *  This function calls gdk_window_invalidate_rect() with a rect
 *  of NULL, causing the entire drawing area to be invalidated.
 *  The conditional check for the validity of the drawable is
 *  for safety but there is no Error messenger in an else clause,
 *  because the disposer will disconnect this callback before
 *  the preview widget is destroyed, hence, the conditional is
 *  really unnecessary.
 *
 *  \param [in] preview_window  A GschemToplevel object.
 */
void preview_invalidate (GschemToplevel *preview_window)
{
  if (preview_window && GDK_IS_WINDOW(preview_window->window))
    gdk_window_invalidate_rect (preview_window->window, NULL, FALSE);
}

/*! \brief Completes initialitation of the widget after realization.
 *  \par Function Description
 *  This function terminates the initialization of preview's GschemToplevel
 *  and GedaToplevel environments after the widget has been realized.
 *
 *  It creates a preview page in the GedaToplevel environment.
 *
 *  \param [in] widget    The preview widget.
 *  \param [in] user_data Unused user data.
 */
static void
preview_callback_realize (GtkWidget *widget, void *user_data)
{
  Preview *preview = PREVIEW (widget);
  GschemToplevel *preview_window = preview->preview_window;
  GedaToplevel *preview_toplevel = preview_window->toplevel;
  Page *preview_page;

  preview_window->window = preview_window->drawing_area->window;
  gtk_widget_grab_focus (preview_window->drawing_area);

  preview_window->screen_width  = preview_window->drawing_area->allocation.width;
  preview_window->screen_height = preview_window->drawing_area->allocation.height;
  preview_window->screen_width  = preview_window->screen_width;
  preview_window->screen_height = preview_window->screen_height;

  preview_window->drawable = preview_window->window;

  x_window_setup_gc (preview_window);

  preview_page = s_page_new_with_notify (preview_toplevel, "unknown");
  x_window_setup_page(preview_window, preview_page,
                      preview_window->world_left,
                      preview_window->world_right,
                      preview_window->world_top,
                      preview_window->world_bottom);
  o_notify_change_add (preview_page,
                      (ChangeNotifyFunc) preview_invalidate,
                      (ChangeNotifyFunc) preview_invalidate,
                       preview_window);

  s_page_goto (preview_toplevel, preview_page);

  i_zoom_world_extents(preview_window,
                 s_page_get_objects (preview_page),
                 I_PAN_DONT_REDRAW);

  preview_invalidate (preview_window);
}

/*! \brief Redraws the view when widget is exposed.
 *  \par Function Description
 *  Redraws the preview pixmap every time the widget is exposed.
 *
 *  \param [in] widget    The preview widget.
 *  \param [in] event     The event structure.
 *  \param [in] user_data Unused user data.
 *  \returns FALSE to propagate the event further.
 */
static bool
preview_callback_expose (GtkWidget      *widget,
                         GdkEventExpose *event,
                         void           *user_data)
{
  Preview *preview = PREVIEW (widget);
  GschemToplevel *preview_window = preview->preview_window;
  cairo_t *save_cr;

  save_cr = preview_window->cr;

  preview_window->cr = gdk_cairo_create (widget->window);

  x_grid_repaint_background (preview_window, &(event->area));

  o_redraw_rectangle (preview_window, &(event->area));

  cairo_destroy (preview_window->cr);

  preview_window->cr = save_cr;

  return FALSE;
}

/*! \brief Handles the press on a mouse button.
 *  \par Function Description
 *  It handles the user inputs.
 *
 *  Three action are available: zoom in, pan and zoom out on preview display.
 *
 *  \param [in] widget    The preview widget.
 *  \param [in] event     The event structure.
 *  \param [in] user_data Unused user data.
 *  \returns FALSE to propagate the event further.
 */
static bool
preview_callback_button_press (GtkWidget      *widget,
                               GdkEventButton *event,
                               void           *user_data)
{
  Preview *preview = PREVIEW (widget);
  GschemToplevel *preview_window = preview->preview_window;
  int wx, wy;

  if (!preview->active) {
    return TRUE;
  }

  switch (event->button) {
    case 1: /* left mouse button: zoom in */
      i_zoom_world (preview_window, ZOOM_IN_DIRECTIVE, ID_ORIGIN_MOUSE,
              I_PAN_DONT_REDRAW);
      preview_invalidate (preview_window);
      break;
    case 2: /* middle mouse button: pan */
      if (!x_event_get_pointer_position(preview_window, FALSE, &wx, &wy))
        return FALSE;
      i_pan_world (preview_window, wx, wy);
      break;
    case 3: /* right mouse button: zoom out */
      i_zoom_world (preview_window, ZOOM_OUT_DIRECTIVE, ID_ORIGIN_MOUSE,
              I_PAN_DONT_REDRAW);
      preview_invalidate (preview_window);
      break;
  }

  return FALSE;
}

/*! \brief Updates the preview widget.
 *  \par Function Description
 *  This function updates the preview: if the preview is active and a
 *  filename has been given, it opens the file and displays
 *  the contents. Otherwise the display will be a blank page.
 *
 *  \param [in] preview The preview widget.
 */
static void
preview_update (Preview *preview)
{
  GschemToplevel *preview_window  = preview->preview_window;
  GedaToplevel  *preview_toplevel = preview_window->toplevel;
  int left, top, right, bottom;
  int width, height;
  GError *err         = NULL;
  GList  *object_list = NULL;
  Object *text;

  if (preview_toplevel->page_current == NULL) {
    return;
  }

  /* delete everything on the our page object */
  s_page_delete_objects(preview_toplevel->page_current);

  if (preview->active) {

    if (preview->filename != NULL) {
      /* open up file in current page */
      if (!f_open(preview_toplevel, preview_toplevel->page_current, preview->filename, &err))
      {
        text = o_text_new(2, 100, 100, LOWER_MIDDLE, 0, err->message, 10, VISIBLE, SHOW_NAME_VALUE);
        s_page_append_object (preview_toplevel->page_current, text);
        g_error_free(err);
      }
    }

    if (preview->buffer != NULL) {
      err = NULL;
      /* Load the data buffer */
      object_list = o_read_buffer (preview_toplevel, NULL, preview->buffer, -1, _("Preview Buffer"), &err);

      if (err == NULL) {
        s_page_append_list (preview_toplevel->page_current, object_list);
      }
      else {
        text = o_text_new(2, 100, 100, LOWER_MIDDLE, 0, err->message, 10, VISIBLE, SHOW_NAME_VALUE);
        s_page_append_object (preview_toplevel->page_current, text);
        g_error_free(err);
      }
    }

    object_list = (GList*)s_page_get_objects (preview_toplevel->page_current);
    if (o_get_world_bounds_list (object_list, &left, &top, &right, &bottom)) {
      /* Clamp the canvas size to the extents of the page being previewed */
      width  = right - left;
      height = bottom - top;
      preview_window->world_left   = left - ((double)width * OVER_ZOOM_FACTOR);
      preview_window->world_right  = right + ((double)width * OVER_ZOOM_FACTOR);
      preview_window->world_top    = top - ((double)height * OVER_ZOOM_FACTOR);
      preview_window->world_bottom = bottom + ((double)height * OVER_ZOOM_FACTOR);
    }
  }

  /* display current page (possibly empty) */
  i_zoom_world_extents (preview_window,
                  s_page_get_objects (preview_toplevel->page_current),
                  I_PAN_DONT_REDRAW);

  preview_invalidate(preview_window);

}

/*! \brief Function to retrieve PreviewClass's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve PreviewClass's Type identifier. On the first call,
 *  this registers the pagesel in the GedaTypesystem.  Subsequently
 *  the functions returns the saved value from its first execution.
 *
 *  \return the Type identifier associated with PreviewClass.
 */
unsigned int
preview_get_type ()
{
  static unsigned int preview_type = 0;

  if (!preview_type) {
    static const GTypeInfo preview_info = {
      sizeof(PreviewClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) preview_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(Preview),
      0,    /* n_preallocs */
      (GInstanceInitFunc) preview_init,
    };

    preview_type = g_type_register_static (GTK_TYPE_DRAWING_AREA,
                                           "Preview",
                                           &preview_info, 0);
  }

  return preview_type;
}

static void
preview_class_init (PreviewClass *class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);

  preview_parent_class = g_type_class_peek_parent (class);

  gobject_class->set_property = preview_set_property;
  gobject_class->get_property = preview_get_property;
  gobject_class->dispose      = preview_dispose;
  gobject_class->finalize     = preview_finalize;

  g_object_class_install_property (
    gobject_class, PROP_FILENAME,
    g_param_spec_string ("filename",
                         "",
                         "",
                         NULL,
                         G_PARAM_READWRITE));
  g_object_class_install_property (
    gobject_class, PROP_BUFFER,
    g_param_spec_string ("buffer",
                         "",
                         "",
                         NULL,
                         G_PARAM_WRITABLE));
  g_object_class_install_property(
    gobject_class, PROP_ACTIVE,
    g_param_spec_boolean ("active",
                          "",
                          "",
                          FALSE,
                          G_PARAM_READWRITE));

  g_object_class_install_property(
    gobject_class, PROP_LARGE,
    g_param_spec_boolean ("large-size",
                          "",
                          "",
                          FALSE,
                          G_PARAM_WRITABLE));
}

static bool
preview_event_configure (GtkWidget         *widget,
                         GdkEventConfigure *event,
                         void              *user_data)
{
  bool retval;
  GschemToplevel *preview_window = PREVIEW (widget)->preview_window;
  //Page     *preview_page = preview_window->toplevel->page_current;

  retval = x_event_configure (widget, event, preview_window);
  /*
  if (preview_page != NULL) {
    i_zoom_world_extents(preview_window, s_page_get_objects (preview_page), 0);
  }*/
  return retval;
}


static bool
preview_event_scroll (GtkWidget *widget,
                      GdkEventScroll *event,
                      GschemToplevel *w_current)
{
  if (!PREVIEW (widget)->active) {
    return TRUE;
  }
  return x_event_scroll (widget, event, PREVIEW (widget)->preview_window);
}

static void
preview_set_xy (Preview *preview, int x, int y)
{
  GschemToplevel *preview_window = preview->preview_window;

  /* Note: Our World == The Screen */
  preview_window->toplevel->width      = x;
  preview_window->toplevel->height     = y;
  preview_window->screen_width         = x;
  preview_window->screen_height        = y;

  g_object_set (GTK_WIDGET (preview), "width-request",  x,
                                      "height-request", y,
                                       NULL);
}

static void
preview_init (Preview *preview)
{
  struct event_reg_t {
    char *detailed_signal;
    GCallback c_handler;
  } drawing_area_events[] = {
    { "realize",            G_CALLBACK (preview_callback_realize) },
    { "expose_event",       G_CALLBACK (preview_callback_expose) },
    { "button_press_event", G_CALLBACK (preview_callback_button_press) },
    { "configure_event",    G_CALLBACK (preview_event_configure) },
    { "scroll_event",       G_CALLBACK (preview_event_scroll) },
    { NULL, NULL }
  }, *tmp;

  GschemToplevel *preview_window;

  preview_window             = gschem_toplevel_new ();
  preview_window->toplevel   = s_toplevel_new ();

  s_toplevel_set_rendered_bounds_func (preview_window->toplevel,
                                       o_text_get_rendered_bounds,
                                       preview_window);
  i_vars_set (preview_window);

  /* Don't need backups or rc files */
  preview_window->toplevel->open_flags = F_OPEN_RC | F_OPEN_RESTORE_CWD;

  /* be sure to turn off scrollbars */
  preview_window->scrollbars           = FALSE;

  preview_window->net_endpoint_mode    = NET_NONE;
  preview_window->net_midpoint_mode    = NET_NONE;

  /* turn off the grid */
  preview_window->grid_mode            = FALSE;

  /* preview_window windows don't have toolbars */
  preview_window->handleboxes          = FALSE;
  preview_window->toolbars             = FALSE;

  preview_window->drawing_area         = GTK_WIDGET (preview);
  preview->preview_window              = preview_window;

  preview_set_xy (preview, 160, 120);

  preview->active   = FALSE;
  preview->filename = NULL;
  preview->buffer   = NULL;

  gtk_widget_set_events (GTK_WIDGET (preview),
                         GDK_EXPOSURE_MASK       |
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK);

  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++)
  {
    g_signal_connect (preview,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      NULL);
  }
}

static void
preview_resize (Preview *preview, bool large)
{
  int x;
  int y;

  if (large) {
    x = 320;
    y = 240;
  }
  else {
    x = 160;
    y = 120;
  }

  preview_set_xy (preview, x, y);
}

static void
preview_set_property (GObject *object, unsigned int property_id,
                      const GValue *value, GParamSpec *pspec)
{
  GschemToplevel *preview_window;
  Preview *preview;

  preview          = PREVIEW (object);
  preview_window   = preview->preview_window;

  if (preview_window == NULL) {
    BUG_MSG ("preview_window = NULL");
    return;
  }

  switch(property_id) {
      case PROP_FILENAME:
        if (preview->buffer != NULL) {
          GEDA_FREE (preview->buffer);
          preview->buffer = NULL;
          g_object_notify (object, "buffer");
        }
        GEDA_FREE (preview->filename);
        preview->filename = u_string_strdup (g_value_get_string (value));
        //preview_update (preview);
        break;

      case PROP_BUFFER:
        if (preview->filename != NULL) {
          GEDA_FREE (preview->filename);
          preview->filename = NULL;
          g_object_notify (object, "filename");
        }
        GEDA_FREE (preview->buffer);
        preview->buffer = u_string_strdup (g_value_get_string (value));
        //preview_update (preview);
        break;

      case PROP_ACTIVE:
        preview->active = g_value_get_boolean (value);
        preview_update (preview);
        break;
      case PROP_LARGE:
        preview_resize (preview, g_value_get_boolean (value));
        preview_update (preview);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
preview_get_property (GObject     *object,
                      unsigned int property_id,
                      GValue      *value,
                      GParamSpec  *pspec)
{
  Preview *preview = PREVIEW (object);

  switch(property_id) {
      case PROP_FILENAME:
        g_value_set_string (value, preview_get_filename (preview));
        break;
      case PROP_ACTIVE:
        g_value_set_boolean (value, preview->active);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

/*! \brief Dispose of the preview widget.
 *  \par Function Description
 *  This function removes the change notify handlers for the
 *  page \a Object used for the preview widget so that when the
 *  Page is deleted in the finalizer, the callback does not get
 *  called to repaint a not nonextant window.
 *
 *  \param [in] self The preview widget.
 */
static void
preview_dispose  (GObject *self)
{
  Preview *preview = PREVIEW (self);
  GschemToplevel *preview_window = preview->preview_window;
  GedaToplevel   *toplevel;

  if (preview_window != NULL) {
    toplevel = preview_window->toplevel;
    if (GEDA_IS_PAGE(toplevel->page_current)) {
      o_notify_change_remove (toplevel->page_current,
                             (ChangeNotifyFunc) preview_invalidate,
                             (ChangeNotifyFunc) preview_invalidate,
                              preview_window);
    }
  }
}

static void
preview_finalize (GObject *self)
{
  Preview *preview = PREVIEW (self);
  GschemToplevel *preview_window = preview->preview_window;
  GedaToplevel   *toplevel;

  if (preview_window != NULL) {

    toplevel = preview_window->toplevel;

    if (toplevel) {
      s_page_delete (toplevel, toplevel->page_current);
      GEDA_UNREF (preview_window->toplevel);
      preview_window->toplevel = NULL;
    }

    GEDA_UNREF (preview_window->drawing_area);
    preview_window->drawing_area = NULL;
    x_window_free_gc (preview_window);
    GEDA_UNREF (preview_window);
    preview->preview_window = NULL;
  }

  G_OBJECT_CLASS (preview_parent_class)->dispose (self);

}
