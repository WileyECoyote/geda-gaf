/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 2 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_handlebox.c
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 * Copyright (C) 1998 Elliot Lee
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
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
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 *
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include <gtk/gtk.h>

#include "../../include/geda_container.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_handlebox.h"
#include "../../include/geda_invisible.h"
#include "../../include/geda_marshal.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaHandleBox - A Container Widget for toolbars
 * \par
 * A GedaHandleBox is a container object used to hold toolbars. GedaHandleBox
 * is a replacement for the GtkHandleBox because the GtkHandleBox is listed
 * as deprecated. A GedaHandleBox allows toolbars to float or be docked to
 * the edges of a window.
 *
 * \defgroup GedaHandleBox Handle Box
 * @{
 * \par
 * The GedaHandleBox widget is a bin widget allowing a portion of a window
 * to be "torn off". The Widget displays a child and a handle that users can
 * drag to tear off a separate window, the float window, containing the child
 * widget. A thin "ghost" outline is drawn in the original location of the
 * handlebox. By dragging the separate window back to its original location,
 * the floating window can be re-attached.
 * \par
 * When re-attaching the float window to the ghost, the float window must be
 * aligned along one of the edges, the "snap edge", which can be specified
 * by the application programmer explicitly, or the code will determine a
 * reasonable default based on the handle position.
 * \par
 * To make detaching and reattaching the handlebox as minimally confusing
 * as possible to the user, it is important to set the snap edge so that
 * the snap edge does not move when the handlebox is deattached. For
 * instance, if the handlebox is packed at the bottom of a VBox, then
 * when the handlebox is detached, the bottom edge of the handlebox's
 * allocation will remain fixed as the height of the handlebox shrinks,
 * so the snap edge should be set to %GTK_POS_BOTTOM.
 */

struct _GedaHandleBoxData
{
  GtkWidget      *invisible;
  unsigned int settings_signal_id;
  int orig_x;
  int orig_y;
};

enum {
  PROP_0,
  PROP_SHADOW,
  PROP_SHADOW_TYPE,
  PROP_HANDLE_POSITION,
  PROP_SHRINK_DETACHED,
  PROP_SNAP_EDGE,
  PROP_SNAP_EDGE_SET,
  PROP_CHILD_DETACHED
};

#define DRAG_HANDLE_SIZE 10
#define MAX_HANDLE_SIZE  DRAG_HANDLE_SIZE * 4
#define CHILDLESS_SIZE   25
#define GHOST_HEIGHT 3
#define TOLERANCE 5

enum {
  SIGNAL_CHILD_ATTACHED,
  SIGNAL_CHILD_DETACHED,
  SIGNAL_LAST
};

/* The algorithm for docking and redocking implemented here has a couple
 * of nice properties:
 *
 * 1) During a single drag, docking always occurs at the the same cursor
 *    position. This means that the users motions are reversible, and that
 *    you will not undock/dock oscillations.
 *
 * 2) Docking generally occurs at user-visible features. The user, once
 *    they figure out to redock, will have useful information about doing
 *    it again in the future.
 *
 * Please try to preserve these properties if you change the algorithm.
 * (And the current algorithm is far from ideal). Briefly, the current
 * algorithm for deciding whether the handlebox is docked or not:
 *
 * 1) The decision is done by comparing two rectangles - the
 *    allocation if the widget at the start of the drag, and
 *    the boundary of handlebox->bin_window at the start of
 *    of the drag offset by the distance that the cursor has
 *    moved.
 *
 * 2) These rectangles must have one edge, the "snap_edge"
 *    of the handlebox, aligned within TOLERANCE.
 *
 * 3) On the other dimension, the extents of one rectangle
 *    must be contained in the extents of the other,
 *    extended by tolerance. That is, either we can have:
 *
 * <-TOLERANCE-|--------bin_window--------------|-TOLERANCE->
 *         <--------float_window-------------------->
 *
 * or we can have:
 *
 * <-TOLERANCE-|------float_window--------------|-TOLERANCE->
 *          <--------bin_window-------------------->
 */
/************************ REVISION HISTORY *************************
 * Who |   When   |  What (Why)
 * ------------------------------------------------------------------
 * WEH | 03/29/14 | Add case GDK_2BUTTON_PRESS in function
 *                | geda_handle_box_grab_event (so floating toolbars will dock
 *                | when doubled click, removed reference to GDK_2BUTTON_PRESS
 *                | from geda_handle_box_button_press (because this procedure
 *                | never sees a GDK_2BUTTON_PRESS event.) This is broke in Gtk
 *                | version. Simplified conditionals in geda_handle_box_button
 *                | _press, which checked for GDK_2BUTTON_PRESS. Converted
 *                | comments to Doxygen, (was latex?)
 * ------------------------------------------------------------------
 * WEH | 07/29/15 | Remove macro G_DEFINE_TYPE (to fix x64 checks)
 * ------------------------------------------------------------------
 * WEH | 01/02/16 | Delete 3rd argument for geda_handle_box_paint and eliminate
 *                | if !event conditional in geda_handle_box_paint because this
 *                | would have only seg-faulted if geda_handle_box_paint ever
 *                | used the NULL passed from geda_handle_box_expose, removed
 *                | function draw_textured_frame. Revise/edit Doxygen comments.
 *                | Reduce scope of variables in geda_handle_box_button_press.
 *                | Add call gtk_container_propagate_expose in geda_handle_box
 *                | _expose so internals are painted properly.
 * ------------------------------------------------------------------
 * WEH | 03/17/17 | Fix shrink_on_detach property; reduce requisition to bear
 *                | minimum when child not attached. Add PROP_SHRINK_DETACHED
 *                | and make shrink-on-detach a g_object property, add functions;
 *                | geda_handle_box_set_toolbar,
 *                | geda_handle_box_get_shrink_on_detach
 *                | geda_handle_box_set_shrink_on_detach
 * ------------------------------------------------------------------
 * WEH | 03/17/18 | Add "handle-size" property and monitor settings.
 *                | Reorganize functions, add functions;
 *                | geda_handle_widget_set_handle_position,
 *                | geda_handle_widget_set_shadow_type
 *                | geda_handle_widget_set_snap_edge
 *                | geda_handle_widget_set_toolbar
*/

static void geda_handle_box_map           (GtkWidget      *widget);
static void geda_handle_box_unmap         (GtkWidget      *widget);
static void geda_handle_box_realize       (GtkWidget      *widget);
static void geda_handle_box_unrealize     (GtkWidget      *widget);
static void geda_handle_box_style_set     (GtkWidget      *widget,
                                           GtkStyle       *previous_style);
static void geda_handle_box_size_request  (GtkWidget      *widget,
                                           GtkRequisition *requisition);
static void geda_handle_box_size_allocate (GtkWidget      *widget,
                                           GtkAllocation  *real_allocation);
static void geda_handle_box_add           (GtkContainer   *container,
                                           GtkWidget      *widget);
static void geda_handle_box_remove        (GtkContainer   *container,
                                           GtkWidget      *widget);
static void geda_handle_box_draw_ghost    (GedaHandleBox  *handlebox);
static void geda_handle_box_paint         (GtkWidget      *widget,
                                           GdkEventExpose *event);
static bool geda_handle_box_expose        (GtkWidget      *widget,
                                           GdkEventExpose *event);
static bool geda_handle_box_button_press  (GtkWidget      *widget,
                                           GdkEventButton *event);
static bool geda_handle_box_motion        (GtkWidget      *widget,
                                           GdkEventMotion *event);
static bool geda_handle_box_grab_event    (GtkWidget      *widget,
                                           GdkEvent       *event,
                                           GedaHandleBox  *handlebox);
static bool geda_handle_box_delete_event  (GtkWidget      *widget,
                                           GdkEventAny    *event);
static void geda_handle_box_reattach      (GedaHandleBox  *handlebox);
static void geda_handle_box_end_drag      (GedaHandleBox  *handlebox,
                                           unsigned int    time);
static void geda_handle_box_set_property  (GObject        *object,
                                           unsigned int    param_id,
                                           const GValue   *value,
                                           GParamSpec     *pspec);
static void geda_handle_box_get_property  (GObject        *object,
                                           unsigned int    param_id,
                                           GValue         *value,
                                           GParamSpec     *pspec);

static unsigned int handle_box_signals[SIGNAL_LAST] = { 0 };

static void *geda_handle_box_parent_class = NULL;

/* Table of pointers to GedaHandleBox instances */
static GHashTable *handlebox_hash = NULL;

static void
change_handle_size (GedaHandleBox *handlebox)
{
  int size;

  gtk_widget_style_get (GTK_WIDGET(handlebox), "handle-size", &size, NULL);

  if (size > 0) {
    handlebox->handle_size = size;
  }
  else {
    handlebox->handle_size = DRAG_HANDLE_SIZE;
  }
}

/* Callback used when a GtkSettings value changes */
static void
settings_notify_cb (GObject *object, GParamSpec *pspec, GedaHandleBox *handlebox)
{
  const char *name;

  name = g_param_spec_get_name (pspec);

  /* Check if handle-size is what was changed */
  if (!strcmp (name, "handle-size")) {
    change_handle_size (handlebox);
  }
}

static void connect_settings_signal(GedaHandleBox *handlebox)
{
  GedaHandleBoxData *priv = handlebox->priv;

  if (!priv->settings_signal_id) {

    GtkSettings *settings;
    GdkScreen   *screen;

    screen   = gtk_widget_get_screen ((GtkWidget*)handlebox);
    settings = gtk_settings_get_for_screen (screen);

    priv->settings_signal_id = g_signal_connect (settings, "notify",
                                                 G_CALLBACK(settings_notify_cb),
                                                 handlebox);
  }
}

/* Removes the settings signal handler. It's safe to call multiple times */
static void remove_settings_signal (GedaHandleBox *handlebox, GdkScreen *screen)
{
  GedaHandleBoxData *priv = handlebox->priv;

  if (priv->settings_signal_id) {

    GtkSettings *settings;

    settings = gtk_settings_get_for_screen (screen);

    g_signal_handler_disconnect (settings, priv->settings_signal_id);

    priv->settings_signal_id = 0;
  }
}

static int effective_handle_position (GedaHandleBox *handlebox)
{
  int handle_position;

  if (gtk_widget_get_direction ((GtkWidget*)handlebox) == GTK_TEXT_DIR_LTR) {
    handle_position = handlebox->handle_position;
  }
  else {

    switch (handlebox->handle_position) {

      case GTK_POS_LEFT:
        handle_position = GTK_POS_RIGHT;
        break;

      case GTK_POS_RIGHT:
        handle_position = GTK_POS_LEFT;
        break;

      default:
        handle_position = handlebox->handle_position;
        break;
    }
  }

  return handle_position;
}

/* Helper for geda_handle_box_grab_event, removes the grab from
 * handlebox and disconnects the grab handler
 */
static void geda_handle_box_end_drag (GedaHandleBox *handlebox, unsigned int time)
{
  GtkWidget *invisible = handlebox->priv->invisible;

  handlebox->in_drag = FALSE;

  gtk_grab_remove (invisible);
  gdk_pointer_ungrab (time);
  g_signal_handlers_disconnect_by_func (invisible,
                                        G_CALLBACK (geda_handle_box_grab_event),
                                        handlebox);
}

/* Helper for geda_handle_box_grab_event */
static bool geda_handle_box_motion (GtkWidget *widget, GdkEventMotion *event)
{
  GedaHandleBox *handlebox = (GedaHandleBox*)widget;
  GdkGeometry    geometry;
  GdkScreen     *screen;
  GdkScreen     *pointer_screen;

  int new_x, new_y;
  int snap_edge;

  int handle_position;
  bool is_snapped = FALSE;


  if (!handlebox->in_drag)
    return FALSE;

  handle_position = effective_handle_position (handlebox);

  /* Calculate the attachment point on the float, if the float were detached */
  new_x = 0;
  new_y = 0;
  screen = gtk_widget_get_screen (widget);

  gdk_display_get_pointer (gdk_screen_get_display (screen),
                           &pointer_screen,
                           &new_x, &new_y, NULL);

  if (pointer_screen != screen) {

    GedaHandleBoxData *priv = handlebox->priv;

    new_x = priv->orig_x;
    new_y = priv->orig_y;
  }

  new_x += handlebox->float_allocation.x;
  new_y += handlebox->float_allocation.y;

  snap_edge = handlebox->snap_edge;

  if (snap_edge == -1) {

    switch (handle_position) {
      case GTK_POS_LEFT:
      case GTK_POS_RIGHT:
        snap_edge = GTK_POS_TOP;
        break;

      default:
        snap_edge = GTK_POS_LEFT;
        break;
    }
  }

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL) {

    switch (snap_edge) {
      case GTK_POS_LEFT:
        snap_edge = GTK_POS_RIGHT;
        break;

      case GTK_POS_RIGHT:
        snap_edge = GTK_POS_LEFT;
        break;

      default:
        break;
    }
  }

  /* First, check if the snapped edge is aligned */
  switch (snap_edge) {

    case GTK_POS_TOP:
      is_snapped = abs (handlebox->attach_allocation.y - new_y) < TOLERANCE;
      break;

    case GTK_POS_BOTTOM:
      is_snapped = abs (handlebox->attach_allocation.y +
                   (int)handlebox->attach_allocation.height -
                   new_y - (int)handlebox->float_allocation.height) < TOLERANCE;
      break;

    case GTK_POS_LEFT:
      is_snapped = abs (handlebox->attach_allocation.x - new_x) < TOLERANCE;
      break;

    case GTK_POS_RIGHT:
      is_snapped = abs (handlebox->attach_allocation.x +
                   (int)handlebox->attach_allocation.width -
                   new_x - (int)handlebox->float_allocation.width) < TOLERANCE;
      break;
  }

  /* Next, check if coordinates in the other direction are sufficiently aligned */
  if (is_snapped) {

    int float_pos1  = 0;        /* Initialize to suppress warnings */
    int float_pos2  = 0;
    int attach_pos1 = 0;
    int attach_pos2 = 0;

    switch (snap_edge) {

      case GTK_POS_TOP:
      case GTK_POS_BOTTOM:
        attach_pos1 = handlebox->attach_allocation.x;
        attach_pos2 = handlebox->attach_allocation.x + handlebox->attach_allocation.width;
        float_pos1 = new_x;
        float_pos2 = new_x + handlebox->float_allocation.width;
        break;

      case GTK_POS_LEFT:
      case GTK_POS_RIGHT:
        attach_pos1 = handlebox->attach_allocation.y;
        attach_pos2 = handlebox->attach_allocation.y + handlebox->attach_allocation.height;
        float_pos1 = new_y;
        float_pos2 = new_y + handlebox->float_allocation.height;
        break;
    }

    is_snapped = ((attach_pos1 - TOLERANCE < float_pos1) && (attach_pos2 + TOLERANCE > float_pos2)) ||
                 ((float_pos1  - TOLERANCE < attach_pos1) && (float_pos2  + TOLERANCE > attach_pos2));
  }

  if (is_snapped) {

    if (handlebox->child_detached) {

      GtkWidget *child = geda_get_child_widget(handlebox);

      handlebox->child_detached = FALSE;
      gdk_window_hide (handlebox->float_window);
      g_object_set (child, "orientation", handlebox->dock_orientation, NULL);
      gdk_window_reparent (handlebox->bin_window, widget->window, 0, 0);
      handlebox->float_window_mapped = FALSE;
      g_signal_emit (handlebox, handle_box_signals[SIGNAL_CHILD_ATTACHED], 0, child);
      gtk_widget_queue_resize (widget);
    }
  }
  else {

    int width, height;

#if (HAVE_GDK_WINDOW_GET_WIDTH)
    width  = gdk_window_get_width (handlebox->float_window);
    height = gdk_window_get_height (handlebox->float_window);
#else
    gdk_drawable_get_size(handlebox->float_window, &width, &height);
#endif

    new_x += handlebox->deskoff_x;
    new_y += handlebox->deskoff_y;

    switch (handle_position) {

      case GTK_POS_LEFT:
        new_y += ((int)handlebox->float_allocation.height - height) >> 1;  /* Divide by 2 */
        break;

      case GTK_POS_RIGHT:
        new_x += (int)handlebox->float_allocation.width - width;
        new_y += ((int)handlebox->float_allocation.height - height) >> 1;
        break;

      case GTK_POS_TOP:
        new_x += ((int)handlebox->float_allocation.width - width) >> 1;
        break;

      case GTK_POS_BOTTOM:
        new_x += ((int)handlebox->float_allocation.width - width) >> 1;
        new_y += (int)handlebox->float_allocation.height - height;
        break;
    }

    if (handlebox->child_detached) {
      gdk_window_move (handlebox->float_window, new_x, new_y);
      gdk_window_raise (handlebox->float_window);
    }
    else {

      int            width;
      int            height;
      unsigned int   border_2x;
      GtkWidget     *child;
      GtkRequisition child_requisition;

      handlebox->child_detached = TRUE;

      child = geda_get_child_widget(handlebox);

      if (child) {
        gtk_widget_get_child_requisition (child, &child_requisition);
      }
      else {
        child_requisition.width  = 0;
        child_requisition.height = 0;
      }

      border_2x = geda_get_container_border_width(widget) << 1; /* Multiply by 2 */

      width  = child_requisition.width  + border_2x;
      height = child_requisition.height + border_2x;

      if (handle_position == GTK_POS_LEFT ||
          handle_position == GTK_POS_RIGHT) {
        width += handlebox->handle_size;
      }
      else {
        height += handlebox->handle_size;
      }

      gdk_window_move_resize (handlebox->float_window, new_x, new_y, width, height);
      gdk_window_reparent (handlebox->bin_window, handlebox->float_window, 0, 0);
      gdk_window_set_geometry_hints (handlebox->float_window, &geometry, GDK_HINT_POS);
      gdk_window_show (handlebox->float_window);
      handlebox->float_window_mapped = TRUE;

#if 0
      /* this extra move is necessary if we use decorations, or our
       * window manager insists on decorations.
       */
      gdk_display_sync (gtk_widget_get_display (widget));
      gdk_window_move (handlebox->float_window, new_x, new_y);
      gdk_display_sync (gtk_widget_get_display (widget));
#endif  /* 0 */

      g_signal_emit (handlebox,
                     handle_box_signals[SIGNAL_CHILD_DETACHED],
                     0, child);

      geda_handle_box_draw_ghost (handlebox);

      gtk_widget_queue_resize (widget);
    }
  }

  return TRUE;
}

/* Callback for grab event
 * Connected by:    geda_handle_box_button_press
 * Disconnected by: geda_handle_box_end_drag
*/
static bool geda_handle_box_grab_event(GtkWidget     *widget,
                                       GdkEvent      *event,
                                       GedaHandleBox *handlebox)
{
  bool event_handled = FALSE;

  switch (event->type) {

    case GDK_BUTTON_RELEASE:
      if (handlebox->in_drag) { /* sanity check */
        geda_handle_box_end_drag (handlebox, event->button.time);
        event_handled = TRUE;
      }
      break;

    case GDK_2BUTTON_PRESS:
      if (handlebox->child_detached)  {
        geda_handle_box_dock (handlebox);
        event_handled = TRUE;
      }
      break;

    case GDK_MOTION_NOTIFY:
      return geda_handle_box_motion ((GtkWidget*)handlebox, (GdkEventMotion*)event);
      break;

    default:
      break;
  }

  return event_handled;
}

/* Common helper emmits SIGNAL_CHILD_ATTACHED
 *
 * Called by: 1.) geda_handle_box_remove
 *            2.) geda_handle_box_delete_event
 *            3.) geda_handle_box_dock
 */
static void geda_handle_box_reattach (GedaHandleBox *handlebox)
{
  GtkWidget *widget = (GtkWidget*)handlebox;

  if (handlebox->child_detached) {

    handlebox->child_detached = FALSE;

    if (gtk_widget_get_realized (widget)) {

      GtkWidget *child;
      GdkWindow *window;

      gdk_window_hide (handlebox->float_window);

      window = geda_get_widget_window(widget);

      gdk_window_reparent (handlebox->bin_window, window, 0, 0);

      child = geda_get_child_widget(handlebox);

      if (child) {
        g_signal_emit (handlebox,
                       handle_box_signals[SIGNAL_CHILD_ATTACHED],
                       0, child);
      }
    }

    handlebox->float_window_mapped = FALSE;
  }

  if (handlebox->in_drag) {
    geda_handle_box_end_drag (handlebox, GDK_CURRENT_TIME);
  }

  gtk_widget_queue_resize (widget);
}

/** \defgroup geda-handle-box-ccvo GedaHandleBox Container Class Virtual Overrides
  * @{
  */

/*! \internal container_class->add */
static void geda_handle_box_add (GtkContainer *container, GtkWidget *widget)
{
  if (GEDA_IS_HANDLE_BOX(container)) {

    GedaHandleBox *handlebox = (GedaHandleBox*)container;

    gtk_widget_set_parent_window (widget, handlebox->bin_window);

    /* Chain-up to parent class, will get warning if widget->parent != NULL */
    ((GtkContainerClass*)geda_handle_box_parent_class)->add (container, widget);

    g_object_get(widget, "orientation", &handlebox->dock_orientation, NULL);
  }
}

/*! \internal container_class->remove */
static void geda_handle_box_remove (GtkContainer *container, GtkWidget *widget)
{
  ((GtkContainerClass*)geda_handle_box_parent_class)->remove (container, widget);
  geda_handle_box_reattach ((GedaHandleBox*)container);
}

/** @} geda-handle-box-ccvo */

/** \defgroup geda-handle-box-wcvo GedaHandleBox Widget Class Virtual Overrides
  * @{
  */

/*! \internal widget_class->button_press == callback for mouse button press event */
static bool geda_handle_box_button_press (GtkWidget      *widget,
                                          GdkEventButton *event)
{
  bool event_handled = FALSE;

  if ((event->button == 1) && (event->type == GDK_BUTTON_PRESS)) {

    GtkWidget *child;
    bool       in_handle;
    GedaHandleBox *handlebox;
    handlebox = (GedaHandleBox*)widget;

    if (event->window != handlebox->bin_window) {
      return FALSE;
    }

    child = geda_get_child_widget(handlebox);

    if (child) {

      GtkAllocation child_allocation;
      unsigned int  border_widthx2;

      gtk_widget_get_allocation (child, &child_allocation);
      border_widthx2  = geda_get_container_border_width(handlebox) << 1;

      switch (effective_handle_position (handlebox)) {

        case GTK_POS_LEFT:
          in_handle = event->x < handlebox->handle_size;
          break;

        case GTK_POS_TOP:
          in_handle = event->y < handlebox->handle_size;
          break;

        case GTK_POS_RIGHT:
          in_handle = event->x > border_widthx2 + child_allocation.width;
          break;

        case GTK_POS_BOTTOM:
          in_handle = event->y > border_widthx2 + child_allocation.height;
          break;

        default:
          in_handle = FALSE;
          break;
      }
    }
    else {

      in_handle     = FALSE;
      event_handled = TRUE;
    }

    if (in_handle) {

      GedaHandleBoxData *priv;
      GdkCursor         *fleur;
      GtkWidget         *invisible;

      int desk_x, desk_y;
      int root_x, root_y;
      int width, height;

      priv   = handlebox->priv;
      invisible = priv->invisible;

      geda_invisible_set_screen ((GedaInvisible*)invisible,
                                  gtk_widget_get_screen (widget));
      gdk_window_get_deskrelative_origin (handlebox->bin_window, &desk_x, &desk_y);
      gdk_window_get_origin (handlebox->bin_window, &root_x, &root_y);

#if (HAVE_GDK_WINDOW_GET_WIDTH)
      width  = gdk_window_get_width (handlebox->bin_window);
      height = gdk_window_get_height (handlebox->bin_window);
#else
      gdk_drawable_get_size(handlebox->bin_window, &width, &height);
#endif

      priv->orig_x = event->x_root;
      priv->orig_y = event->y_root;

      handlebox->float_allocation.x      = root_x - event->x_root;
      handlebox->float_allocation.y      = root_y - event->y_root;
      handlebox->float_allocation.width  = width;
      handlebox->float_allocation.height = height;

      handlebox->deskoff_x = desk_x - root_x;
      handlebox->deskoff_y = desk_y - root_y;

      if (gdk_window_is_viewable (widget->window)) {

        gdk_window_get_origin (widget->window, &root_x, &root_y);

#if (HAVE_GDK_WINDOW_GET_WIDTH)
        width  = gdk_window_get_width (widget->window);
        height = gdk_window_get_height (widget->window);
#else
        gdk_drawable_get_size(widget->window, &width, &height);
#endif

        handlebox->attach_allocation.x      = root_x;
        handlebox->attach_allocation.y      = root_y;
        handlebox->attach_allocation.width  = width;
        handlebox->attach_allocation.height = height;
      }
      else {

        handlebox->attach_allocation.x      = -1;
        handlebox->attach_allocation.y      = -1;
        handlebox->attach_allocation.width  = 0;
        handlebox->attach_allocation.height = 0;
      }

      handlebox->in_drag = TRUE;

      fleur = gdk_cursor_new_for_display (gtk_widget_get_display (widget),
                                          GDK_FLEUR);

      if (gdk_pointer_grab (invisible->window,
                            FALSE,
                           (GDK_BUTTON1_MOTION_MASK |
                            GDK_POINTER_MOTION_HINT_MASK |
                            GDK_BUTTON_RELEASE_MASK),
                            NULL,
                            fleur,
                            event->time) != 0)
      {
        handlebox->in_drag = FALSE;
      }
      else
      {
        gtk_grab_add (invisible);
        g_signal_connect (invisible, "event",
                          G_CALLBACK (geda_handle_box_grab_event), handlebox);
      }

      gdk_cursor_unref (fleur);
      event_handled = TRUE;
    }
  }
  return event_handled;
}

/*! \internal widget_class->delete_event */
static int geda_handle_box_delete_event (GtkWidget *widget, GdkEventAny  *event)
{
  GedaHandleBox *handlebox = (GedaHandleBox*)widget;

  if (event->window == handlebox->float_window) {
      geda_handle_box_reattach (handlebox);
      return TRUE;
  }

  return FALSE;
}

/*! \internal helper for geda_handle_box_expose */
static void geda_handle_box_draw_ghost (GedaHandleBox *handlebox)
{
  GtkWidget   *widget;
  GtkStateType state;
  unsigned int x;
  unsigned int y;
  unsigned int width;
  unsigned int height;
  int handle_position;

  widget = (GtkWidget*)handlebox;

  handle_position = effective_handle_position (handlebox);

  if (handle_position == GTK_POS_LEFT ||
      handle_position == GTK_POS_RIGHT)
  {
    x = handle_position == GTK_POS_LEFT ? 0 : widget->allocation.width - handlebox->handle_size;
    y = 0;
    width = handlebox->handle_size;
    height = widget->allocation.height;
  }
  else
  {
    x = 0;
    y = handle_position == GTK_POS_TOP ? 0 : widget->allocation.height - handlebox->handle_size;
    width = widget->allocation.width;
    height = handlebox->handle_size;
  }

  state = gtk_widget_get_state (widget);

  gtk_paint_shadow (widget->style, widget->window, state,
                    GTK_SHADOW_ETCHED_IN,
                    NULL, widget, "handle",
                    x,
                    y,
                    width,
                    height);

  if (handle_position == GTK_POS_LEFT ||
      handle_position == GTK_POS_RIGHT)
  {

    int x1;
    int x2;

    if (handle_position == GTK_POS_LEFT) {
      x1 = handlebox->handle_size;
      x2 = widget->allocation.width;
    }
    else {
      x1 = 0;
      x2 = widget->allocation.width - handlebox->handle_size;
    }

    gtk_paint_hline (widget->style, widget->window, state,
                     NULL, widget, "handlebox", x1, x2,
                     widget->allocation.height >> 1);
  }
  else {

    int y1;
    int y2;

    if (handle_position == GTK_POS_TOP) {
      y1 = handlebox->handle_size;
      y2 = widget->allocation.height;
    }
    else {
      y1 = 0;
      y2 = widget->allocation.height - handlebox->handle_size;
    }

    gtk_paint_vline (widget->style, widget->window, state,
                     NULL, widget, "handlebox", y1, y2,
                     widget->allocation.width >> 1);
  }
}

/*! \internal helper for geda_handle_box_expose */
static void geda_handle_box_paint (GtkWidget *widget, GdkEventExpose *event)
{
  GedaHandleBox *handlebox;
  GdkRectangle  *area;
  GdkRectangle   rect;
  GtkOrientation handle_orientation;
  int            handle_position;
  int            width, height;

  area               = &event->area;
  handlebox          = (GedaHandleBox*)widget;
  handle_orientation = GTK_ORIENTATION_VERTICAL;
  handle_position    = effective_handle_position (handlebox);

#if (HAVE_GDK_WINDOW_GET_WIDTH)
  width  = gdk_window_get_width (handlebox->bin_window);
  height = gdk_window_get_height (handlebox->bin_window);
#else
  gdk_drawable_get_size(handlebox->bin_window, &width, &height);
#endif

  /* First draw the exterior box */
  gtk_paint_box (widget->style, handlebox->bin_window,
                 gtk_widget_get_state (widget),
                 handlebox->shadow_type,
                 &event->area, widget, "box",
                 0, 0, -1, -1);

  /* Draw the Handle for the handlebox, currently, the handle is
   * drawn _above_ the relief of the handlebox. The handle could
   * also be drawn on the same level...
   *
   * handlebox->handle_position == GTK_POS_LEFT ? handlebox->handle_size : 0,
   * handlebox->handle_position == GTK_POS_TOP ? handlebox->handle_size : 0,
   * width,
   * height);
   */
  /* Determine coordinates for the "handle" */
  switch (handle_position) {

    case GTK_POS_LEFT:
      rect.x             = 0;
      rect.y             = 0;
      rect.width         = handlebox->handle_size;
      rect.height        = height;
      handle_orientation = GTK_ORIENTATION_VERTICAL;
      break;

    case GTK_POS_RIGHT:
      rect.x             = width - handlebox->handle_size;
      rect.y             = 0;
      rect.width         = handlebox->handle_size;
      rect.height        = height;
      handle_orientation = GTK_ORIENTATION_VERTICAL;
      break;

    case GTK_POS_TOP:
      rect.x             = 0;
      rect.y             = 0;
      rect.width         = width;
      rect.height        = handlebox->handle_size;
      handle_orientation = GTK_ORIENTATION_HORIZONTAL;
      break;

    case GTK_POS_BOTTOM:
      rect.x             = 0;
      rect.y             = height - handlebox->handle_size;
      rect.width         = width;
      rect.height        = handlebox->handle_size;
      handle_orientation = GTK_ORIENTATION_HORIZONTAL;
      break;

    default:
      BUG_IMSG ("unhandler case", handle_position);
      break;
  }

  /* Draw the handle for the handlebox */
  if (gdk_rectangle_intersect (area, &rect, NULL)) {
     gtk_paint_handle (widget->style, handlebox->bin_window,
                       GTK_STATE_NORMAL, GTK_SHADOW_OUT,
                       area, widget, "handlebox",
                       rect.x, rect.y, rect.width, rect.height,
                       handle_orientation);
  }

  /* If a child widget is in the handlebox, aka a Toolbar */
  if (geda_get_child_widget(widget)) {
    GTK_WIDGET_CLASS (geda_handle_box_parent_class)->expose_event (widget, event);
  }
}

/*! \internal widget_class->expose_event */
static bool geda_handle_box_expose (GtkWidget *widget, GdkEventExpose *event)
{
  if (gtk_widget_is_drawable (widget)) {

    GtkWidget *child;

    if (event->window == widget->window) {

      GedaHandleBox *handlebox = (GedaHandleBox*)widget;

      if (handlebox->child_detached) {
        geda_handle_box_draw_ghost (handlebox);
      }
    }
    else {
      geda_handle_box_paint (widget, event);
    }

    child = geda_get_child_widget(widget);

    geda_container_propagate_expose (widget, child, event);
  }

  return TRUE;
}

/*! \internal widget_class->map */
static void geda_handle_box_map (GtkWidget *widget)
{
  GtkBin        *bin;
  GedaHandleBox *handlebox;

  gtk_widget_set_mapped (widget, TRUE);

  bin       = (GtkBin*)widget;
  handlebox = (GedaHandleBox*)widget;

  if (bin->child &&
      gtk_widget_get_visible (bin->child) &&
     !gtk_widget_get_mapped (bin->child))
  {
    gtk_widget_map (bin->child);
  }

  if (handlebox->child_detached && !handlebox->float_window_mapped) {

    /* This would be true only after revealing a hidden toolbar */
    gdk_window_show (handlebox->float_window);
    handlebox->float_window_mapped = TRUE;
  }

  gdk_window_show (handlebox->bin_window);
  gdk_window_show (widget->window);
}

/*! \internal widget_class->unmap */
static void geda_handle_box_unmap (GtkWidget *widget)
{
  GedaHandleBox *handlebox;

  gtk_widget_set_mapped (widget, FALSE);

  handlebox = (GedaHandleBox*)widget;

  gdk_window_hide (widget->window);

  if (handlebox->float_window_mapped) {
      gdk_window_hide (handlebox->float_window);
      handlebox->float_window_mapped = FALSE;
  }

  GTK_WIDGET_CLASS (geda_handle_box_parent_class)->unmap (widget);
}

/*! \internal widget_class->realize */
static void geda_handle_box_realize (GtkWidget *widget)
{
  GdkWindowAttr  attributes;
  GtkAllocation *allocation;
  GedaHandleBox *handlebox;
  GtkWidget     *child;
  GdkWindow     *parent;
  GtkStateType   state;
  int            attributes_mask;

  handlebox = (GedaHandleBox*)widget;

  gtk_widget_set_realized (widget, TRUE);

  allocation = geda_get_widget_allocation (widget);
  parent     = gtk_widget_get_parent_window (widget);

  attributes.x           = allocation->x;
  attributes.y           = allocation->y;
  attributes.width       = allocation->width;
  attributes.height      = allocation->height;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.wclass      = GDK_INPUT_OUTPUT;
  attributes.visual      = gtk_widget_get_visual (widget);
  attributes.colormap    = gtk_widget_get_colormap (widget);
  attributes.event_mask  = gtk_widget_get_events (widget) | GDK_EXPOSURE_MASK;
  attributes_mask        = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
  widget->window         = gdk_window_new (parent, &attributes, attributes_mask);

  gdk_window_set_user_data (widget->window, widget);

  attributes.x           = 0;
  attributes.y           = 0;
  attributes.width       = allocation->width;
  attributes.height      = allocation->height;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask  = (gtk_widget_get_events (widget) |
                            GDK_EXPOSURE_MASK |
                            GDK_BUTTON1_MOTION_MASK |
                            GDK_POINTER_MOTION_HINT_MASK |
                            GDK_BUTTON_PRESS_MASK |
                            GDK_BUTTON_RELEASE_MASK);
  attributes_mask        = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
  handlebox->bin_window  = gdk_window_new (widget->window, &attributes, attributes_mask);

  gdk_window_set_user_data (handlebox->bin_window, widget);

  child = geda_get_child_widget(handlebox);

  if (child) {
    gtk_widget_set_parent_window (child, handlebox->bin_window);
  }

  attributes.x           = 0;
  attributes.y           = 0;
  attributes.width       = widget->requisition.width;
  attributes.height      = widget->requisition.height;
  attributes.window_type = GDK_WINDOW_TOPLEVEL;
  attributes.wclass      = GDK_INPUT_OUTPUT;
  attributes.visual      = gtk_widget_get_visual (widget);
  attributes.colormap    = gtk_widget_get_colormap (widget);
  attributes.event_mask  = (gtk_widget_get_events (widget) |
                            GDK_KEY_PRESS_MASK |
                            GDK_ENTER_NOTIFY_MASK |
                            GDK_LEAVE_NOTIFY_MASK |
                            GDK_FOCUS_CHANGE_MASK |
                            GDK_STRUCTURE_MASK);
  attributes.type_hint    = GDK_WINDOW_TYPE_HINT_TOOLBAR;
  attributes_mask         = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP | GDK_WA_TYPE_HINT;
  handlebox->float_window = gdk_window_new (gtk_widget_get_root_window (widget),
                                            &attributes, attributes_mask);
  gdk_window_set_user_data (handlebox->float_window, widget);
  gdk_window_set_decorations (handlebox->float_window, 0);

  /* Setting GDK_WINDOW_TYPE_HINT_TOOLBAR here results in the toolbar lowering
   * to the bottom window on the display with gtk+-2.0 == 2.24.30, but not with
   * 2.24.10 */

#if GTK_CHECK_VERSION(2,24,30)

  gdk_window_set_type_hint (handlebox->float_window, GDK_WINDOW_TYPE_HINT_DOCK);

#else

  gdk_window_set_type_hint (handlebox->float_window, GDK_WINDOW_TYPE_HINT_DOCK |
                                                     GDK_WINDOW_TYPE_HINT_TOOLBAR);
#endif

  /* Use to work fine, then gtk erratica. Added DOCK hint above and next two lines */
  GtkWidget *topwindow = gtk_widget_get_toplevel (widget);

  gdk_window_set_transient_for (handlebox->float_window, (GdkWindow*)topwindow->window);

  widget->style = gtk_style_attach (widget->style, widget->window);
  state = gtk_widget_get_state (widget);

  gtk_style_set_background (widget->style, widget->window, state);
  gtk_style_set_background (widget->style, handlebox->bin_window, state);
  gtk_style_set_background (widget->style, handlebox->float_window, state);

  gdk_window_set_back_pixmap (widget->window, NULL, TRUE);

  connect_settings_signal(handlebox);
}

/*! \internal widget_class->unrealize */
static void geda_handle_box_unrealize (GtkWidget *widget)
{
  GedaHandleBox *handlebox = (GedaHandleBox*)widget;

  /* Disconnect the settings Monitor */
  remove_settings_signal(handlebox, gtk_widget_get_screen (widget));

  gdk_window_set_user_data (handlebox->bin_window, NULL);
  gdk_window_destroy (handlebox->bin_window);
  handlebox->bin_window = NULL;
  gdk_window_set_user_data (handlebox->float_window, NULL);
  gdk_window_destroy (handlebox->float_window);
  handlebox->float_window = NULL;

  GTK_WIDGET_CLASS (geda_handle_box_parent_class)->unrealize (widget);
}

/*! \internal widget_class->size_allocate */
static void geda_handle_box_size_allocate (GtkWidget     *widget,
                                           GtkAllocation *allocation)
{
  GtkWidget     *child;
  GedaHandleBox *handlebox;
  GtkRequisition child_requisition;
  int            handle_position;

  child     = geda_get_child_widget (widget);
  handlebox = (GedaHandleBox*)widget;

  handle_position = effective_handle_position (handlebox);

  if (child) {
    gtk_widget_get_child_requisition (child, &child_requisition);
  }
  else {
    child_requisition.width  = 0;
    child_requisition.height = 0;
  }

  gtk_widget_set_allocation(widget, allocation);

  if (gtk_widget_get_realized (widget)) {

    GdkWindow *window = geda_get_widget_window(widget);

    gdk_window_move_resize (window,
                            allocation->x,
                            allocation->y,
                            allocation->width,
                            allocation->height);
  }

  if (child && gtk_widget_get_visible (child)) {

    GtkAllocation *child_allocation;
    unsigned int border_width;
    unsigned int border_widthx2;

    child_allocation = geda_get_widget_allocation (child);

    border_width        = geda_get_container_border_width(widget);
    border_widthx2      = border_width << 1;

    child_allocation->x = border_width;
    child_allocation->y = border_width;

    if (handle_position == GTK_POS_LEFT) {
      child_allocation->x += handlebox->handle_size;
    }
    else if (handle_position == GTK_POS_TOP) {
      child_allocation->y += handlebox->handle_size;
    }

    if (handlebox->child_detached) {

      unsigned int float_width;
      unsigned int float_height;

      child_allocation->width  = child_requisition.width;
      child_allocation->height = child_requisition.height;

      float_width  = child_allocation->width + border_widthx2;
      float_height = child_allocation->height + border_widthx2;

      if (handle_position == GTK_POS_LEFT ||
          handle_position == GTK_POS_RIGHT)
      {
        float_width += handlebox->handle_size;
      }
      else {
        float_height += handlebox->handle_size;
      }

      if (gtk_widget_get_realized (widget)) {

        gdk_window_resize (handlebox->float_window,
                           float_width,
                           float_height);
        gdk_window_move_resize (handlebox->bin_window,
                                0,
                                0,
                                float_width,
                                float_height);
      }
    }
    else {

      GtkAllocation *allocated;
      int width;
      int height;

      allocated = geda_get_widget_allocation (widget);
      width     = allocated->width;
      height    = allocated->height;

      child_allocation->width  = MAX (1, (int)width - border_widthx2);
      child_allocation->height = MAX (1, (int)height - border_widthx2);

      if (handle_position == GTK_POS_LEFT || handle_position == GTK_POS_RIGHT)
      {
        child_allocation->width -= handlebox->handle_size;
      }
      else
      {
        child_allocation->height -= handlebox->handle_size;
      }

      if (gtk_widget_get_realized (widget)) {
        gdk_window_move_resize (handlebox->bin_window,
                                0,
                                0,
                                width,
                                height);
      }
    }

    gtk_widget_size_allocate (child, child_allocation);
  }
}

/*! \internal widget_class->size_request */
static void geda_handle_box_size_request (GtkWidget      *widget,
                                          GtkRequisition *requisition)
{
  GtkBin        *bin;
  GedaHandleBox *handlebox;
  GtkRequisition child_requisition;
  int            handle_position;

  bin       = (GtkBin*)widget;
  handlebox = (GedaHandleBox*)widget;

  handle_position = effective_handle_position (handlebox);

  if (handle_position == GTK_POS_LEFT || handle_position == GTK_POS_RIGHT)
  {
    requisition->width = handlebox->handle_size;
    requisition->height = 0;
  }
  else
  {
    requisition->width = 0;
    requisition->height = handlebox->handle_size;
  }

  /* if our child is not visible, we still request its size, since we
   * won't have any useful hint for our size otherwise.
   */
  if (bin->child) {
    gtk_widget_size_request (bin->child, &child_requisition);
  }
  else {
    child_requisition.width  = 0;
    child_requisition.height = 0;
  }

  if (handlebox->child_detached) {

    if (!handlebox->shrink_on_detach) {

      if (handle_position == GTK_POS_LEFT || handle_position == GTK_POS_RIGHT)
      {
        requisition->height += child_requisition.height;
      }
      else
      {
        requisition->width += child_requisition.width;
      }
    }
    else {

      if (handle_position == GTK_POS_LEFT || handle_position == GTK_POS_RIGHT)
      {
        requisition->width  = 0;
        requisition->height = 1;
      }
      else
      {
        requisition->width  = 1;
        requisition->height = 0;
      }
    }
  }
  else {

    unsigned int border_widthx2;

    border_widthx2 = geda_get_container_border_width(widget) << 1;

    requisition->width  += border_widthx2;
    requisition->height += border_widthx2;

    if (bin->child) {
      requisition->width  += child_requisition.width;
      requisition->height += child_requisition.height;
    }
    else {
      requisition->width  += CHILDLESS_SIZE;
      requisition->height += CHILDLESS_SIZE;
    }
  }
}

/*! \internal widget_class->style_set */
static void geda_handle_box_style_set (GtkWidget *widget, GtkStyle *previous_style)
{
  GedaHandleBox *handlebox = (GedaHandleBox*)widget;

  if (gtk_widget_get_realized (widget) &&
      gtk_widget_get_has_window (widget))
  {
    gtk_style_set_background (widget->style, widget->window, widget->state);
    gtk_style_set_background (widget->style, handlebox->bin_window, widget->state);
    gtk_style_set_background (widget->style, handlebox->float_window, widget->state);
  }

  change_handle_size(handlebox);
}

/** @} geda-handle-box-wcvo */

/** \defgroup geda-handle-box-govo GedaHandleBox GObject Virtual Overrides
  * @{
  */

/*! \internal gobject_class->get_property */
static void geda_handle_box_get_property (GObject      *object,
                                          unsigned int  prop_id,
                                          GValue       *value,
                                          GParamSpec   *pspec)
{
  GedaHandleBox *handlebox = (GedaHandleBox*)object;

  switch (prop_id) {

    case PROP_SHADOW:
    case PROP_SHADOW_TYPE:
      g_value_set_enum (value, handlebox->shadow_type);
      break;

    case PROP_HANDLE_POSITION:
      g_value_set_enum (value, handlebox->handle_position);
      break;

    case PROP_SHRINK_DETACHED:
      g_value_set_boolean (value, handlebox->shrink_on_detach);
      break;

    case PROP_SNAP_EDGE:
      g_value_set_enum (value,
                        (handlebox->snap_edge == -1 ?
                        GTK_POS_TOP : handlebox->snap_edge));
      break;
    case PROP_SNAP_EDGE_SET:
      g_value_set_boolean (value, handlebox->snap_edge != -1);
      break;

    case PROP_CHILD_DETACHED:
      g_value_set_boolean (value, handlebox->child_detached);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/*! \internal gobject_class->set_property */
static void geda_handle_box_set_property (GObject      *object,
                                          unsigned int  prop_id,
                                          const GValue *value,
                                          GParamSpec   *pspec)
{
  GedaHandleBox *handlebox = (GedaHandleBox*)object;

  switch (prop_id) {

    case PROP_SHADOW:
    case PROP_SHADOW_TYPE:
      geda_handle_box_set_shadow_type (handlebox, g_value_get_enum (value));
      break;

    case PROP_HANDLE_POSITION:
      geda_handle_box_set_handle_position (handlebox, g_value_get_enum (value));
      break;

    case PROP_SHRINK_DETACHED:
      geda_handle_box_set_shrink_on_detach (handlebox, g_value_get_enum (value));
      break;

    case PROP_SNAP_EDGE:
      geda_handle_box_set_snap_edge (handlebox, g_value_get_boolean (value));
      break;

    case PROP_SNAP_EDGE_SET:
      if (!g_value_get_boolean (value))
        geda_handle_box_set_snap_edge (handlebox, (GtkPositionType)-1);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/*! \internal gobject_class->constructed */
static void
geda_handle_box_constructed (GObject *object)
{
  GedaHandleBox     *handlebox = (GedaHandleBox*)object;
  GedaHandleBoxData *priv = handlebox->priv;

  priv->invisible = geda_invisible_new ();
  gtk_widget_show (priv->invisible);
}

/*! \internal gobject_class->finalize */
static void geda_handle_box_finalize (GObject *object)
{
  GedaHandleBox *handlebox = (GedaHandleBox*)object;

  if (g_hash_table_remove (handlebox_hash, object)) {
    if (!g_hash_table_size (handlebox_hash)) {
      g_hash_table_destroy (handlebox_hash);
      handlebox_hash = NULL;
    }
  }

  g_free (handlebox->priv);

  G_OBJECT_CLASS (geda_handle_box_parent_class)->finalize (object);
}

/** @} geda-handle-box-govo */

/*!
 * \brief GedaHandleBoxClass Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  g_class     GedaHandleBoxClass class being initializing
 * \param [in]  class_data  Associated GedaHandleBoxClass structure
 */
static void geda_handle_box_class_init(void *g_class, void *class_data)
{
  GObjectClass       *object_class;
  GtkWidgetClass     *widget_class;
  GtkContainerClass  *container_class;
  GedaHandleBoxClass *class;
  GParamSpec         *params;

  class           = (GedaHandleBoxClass*)g_class;
  object_class    = (GObjectClass*)class;
  widget_class    = (GtkWidgetClass*)class;
  container_class = (GtkContainerClass*)class;

  object_class->constructed    = geda_handle_box_constructed;
  object_class->finalize       = geda_handle_box_finalize;
  object_class->get_property   = geda_handle_box_get_property;
  object_class->set_property   = geda_handle_box_set_property;

  geda_handle_box_parent_class = g_type_class_peek_parent(class);

  params = g_param_spec_enum ("shadow",
                              NULL,
                            _("Appearance of the shadow that surrounds the container"),
                              GTK_TYPE_SHADOW_TYPE,
                              GTK_SHADOW_OUT,
                              G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_SHADOW, params);

  params = g_param_spec_enum ("shadow-type",
                              NULL,
                            _("Appearance of the shadow that surrounds the container"),
                              GTK_TYPE_SHADOW_TYPE,
                              GTK_SHADOW_OUT,
                              G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_SHADOW_TYPE, params);

  params = g_param_spec_enum ("handle-position",
                               NULL,
                            _("Position of the handle relative to the child toolbar"),
                               GTK_TYPE_POSITION_TYPE,
                               GTK_POS_LEFT,
                               G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_HANDLE_POSITION, params);

  params = g_param_spec_boolean ("shrink",
                               _("Shrink"),
                               _("Shrink when toolbar is detached."),
                                  TRUE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_SHRINK_DETACHED, params);

  params = g_param_spec_enum ("snap-edge",
                               NULL,
                            _("Side of the handlebox that is lined up with the docking point to dock the handlebox"),
                               GTK_TYPE_POSITION_TYPE,
                               GTK_POS_TOP,
                               G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_SNAP_EDGE, params);

  params = g_param_spec_boolean ("snap-edge-set",
                                 NULL,
                               _("Whether to use the value from the snap_edge property or a value derived from handle_position"),
                                 FALSE,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_SNAP_EDGE_SET, params);

  params = g_param_spec_boolean ("child-detached",
                                 NULL,
                               _("Boolean value indicating whether the toolbar is attached or detached."),
                                 FALSE,
                                 G_PARAM_READABLE);

  g_object_class_install_property (object_class, PROP_CHILD_DETACHED, params);

  widget_class->button_press_event = geda_handle_box_button_press;
  widget_class->delete_event       = geda_handle_box_delete_event;
  widget_class->expose_event       = geda_handle_box_expose;
  widget_class->map                = geda_handle_box_map;
  widget_class->unmap              = geda_handle_box_unmap;
  widget_class->realize            = geda_handle_box_realize;
  widget_class->unrealize          = geda_handle_box_unrealize;
  widget_class->size_request       = geda_handle_box_size_request;
  widget_class->size_allocate      = geda_handle_box_size_allocate;
  widget_class->style_set          = geda_handle_box_style_set;

  container_class->add    = geda_handle_box_add;
  container_class->remove = geda_handle_box_remove;

  class->child_attached = NULL;
  class->child_detached = NULL;

  /*!
   * signal "child-attached": GedaComboBox::child-attached:
   * \brief emitted when active item is changed.
   * This signal is emitted when the contents of the
   * handlebox are reattached to the main window.
   *
   * param handlebox: the object which received the signal.
   * param widget:    the child widget of the handlebox.(for backwards-compatibility)
   */
  handle_box_signals[SIGNAL_CHILD_ATTACHED] =
    g_signal_new ("child-attached",
                   G_TYPE_FROM_CLASS (class),
                   G_SIGNAL_RUN_FIRST,
                   G_STRUCT_OFFSET (GedaHandleBoxClass, child_attached),
                   NULL, NULL,
                   geda_marshal_VOID__POINTER,
                   G_TYPE_NONE, 1,
                   G_TYPE_POINTER);

  /*!
   * signal "child-detached": GedaComboBox::child-detached:
   * \brief emitted when active item is changed.
   * This signal is emitted when the contents of the
   * handlebox are detached from the handlebox.
   *
   * param handlebox: the object which received the signal.
   * param widget:    the child widget of the handlebox.(for backwards-compatibility)
   */
  handle_box_signals[SIGNAL_CHILD_DETACHED] =
    g_signal_new ("child-detached",
                   G_TYPE_FROM_CLASS (class),
                   G_SIGNAL_RUN_FIRST,
                   G_STRUCT_OFFSET (GedaHandleBoxClass, child_detached),
                   NULL, NULL,
                   geda_marshal_VOID__POINTER,
                   G_TYPE_NONE, 1,
                   G_TYPE_POINTER);

  params = g_param_spec_int ("handle-size",
                           _("Handle Size"),
                           _("Size of the drag handle in pixels."),
                             0,
                             MAX_HANDLE_SIZE,
                             DRAG_HANDLE_SIZE,
                             G_PARAM_READWRITE);

  gtk_widget_class_install_style_property (widget_class, params);
}

/*!
 * \brief Type instance initializer for GedaHandleBox
 * \par Function Description
 *  Type instance initializer for GedaHandleBox, initializes a new empty
 *  GedaHandleBox object.
 *
 * \param [in] instance The GedaHandleBox structure being initialized,
 * \param [in] g_class  The GedaHandleBox class we are initializing.
 */
static void geda_handle_box_instance_init(GTypeInstance *instance, void *g_class)
{
  GedaHandleBox     *handlebox = (GedaHandleBox*)instance;
  GedaHandleBoxData *priv;

  gtk_widget_set_has_window ((GtkWidget*)handlebox, TRUE);

  priv = g_malloc0 (sizeof(GedaHandleBoxData));

  handlebox->bin_window          = NULL;
  handlebox->float_window        = NULL;
  handlebox->shadow_type         = GTK_SHADOW_OUT;
  handlebox->handle_position     = GTK_POS_LEFT;
  handlebox->handle_size         = DRAG_HANDLE_SIZE;
  handlebox->float_window_mapped = FALSE;
  handlebox->child_detached      = FALSE;
  handlebox->in_drag             = FALSE;
  handlebox->shrink_on_detach    = TRUE;
  handlebox->snap_edge           = -1;
  handlebox->dock_orientation    = GTK_ORIENTATION_HORIZONTAL;

  handlebox->priv = priv;

  if (!handlebox_hash) {
    handlebox_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (handlebox_hash, instance, instance);
}

/**
 * \defgroup GedaHandleBoxFunctions Handle Box Public Functions
 * @{
 *  \par Begin Public Accessors
 */

/*!
 * \brief Retrieve GedaHandleBox's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaHandleBox Type identifier. When
 *  first called, the function registers a #GedaHandleBox in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaHandleBox and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaHandleBox.
 */
GedaType geda_handle_box_get_type (void)
{
  static volatile GedaType geda_handle_box_type = 0;

  if (g_once_init_enter (&geda_handle_box_type)) {

    static const GTypeInfo info = {
      sizeof(GedaHandleBoxClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_handle_box_class_init,      /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaHandleBox),
      0,                               /* n_preallocs         */
      geda_handle_box_instance_init    /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaHandleBox");
    type   = g_type_register_static (GTK_TYPE_BIN, string, &info, 0);

    g_once_init_leave (&geda_handle_box_type, type);
  }

  return geda_handle_box_type;
}

/*!
 * \brief Check if an object is a GedaHandleBox
 * \par Function Description
 *  Determines if \a handlebox is valid by verifying \a handlebox
 *  is included in the hash table of GedaHandleBox objects.
 *
 * \return TRUE if \a handlebox is a valid GedaHandleBox
 */
bool is_a_geda_handle_box (GedaHandleBox *handlebox)
{
  if ((handlebox != NULL) && (handlebox_hash != NULL)) {
    return g_hash_table_lookup(handlebox_hash, handlebox) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Get a New GedaHandleBox Object
 * \par Function Description
 *  Creates and returns a new GedaHandleBox instance
 */
GtkWidget *geda_handle_box_new (void)
{
  return g_object_new (GEDA_TYPE_HANDLE_BOX, NULL);
}

/*!
 * \brief Programmatically dock the child of a GedaHandleBox
 * \par Function Description
 *  Reattaches the child widget to the GedaHandleBox
 */
void geda_handle_box_dock (GedaHandleBox *handlebox)
{
  if (GEDA_IS_HANDLE_BOX(handlebox)) {
    g_object_set (((GtkBin*)handlebox)->child, "orientation", handlebox->dock_orientation, NULL);
    geda_handle_box_reattach (handlebox);
  }
}

/*!
 * \brief geda_handle_box_get_child_detached
 * \par Function Description
 *  Returns whether the handlebox's child is currently detached.
 *
 * \param [in] handlebox    The #GedaHandleBox object
 *
 * \returns %TRUE if the child is currently detached, otherwise %FALSE
 */
bool geda_handle_box_get_child_detached (GedaHandleBox *handlebox)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handlebox), FALSE);

  return handlebox->child_detached;
}

/*!
 * \brief Sets the Handle Position of a GedaHandleBox
 * \par Function Description
 *  Sets the side of the handlebox where the handle is drawn.
 *
 * \param [in] handlebox The #GedaHandleBox object
 * \param [in] position   Side of the handlebox where the handle should
 *                        be drawn.
 *
 *  <DL>
 *    <DT>GTK_POS_LEFT</DT>
 *    <DT>GTK_POS_RIGHT</DT>
 *    <DT>GTK_POS_TOP</DT>
 *    <DT>GTK_POS_BOTTOM</DT>
 *  </DL>
 */
void geda_handle_box_set_handle_position (GedaHandleBox   *handlebox,
                                          GtkPositionType  position)
{
  g_return_if_fail (GEDA_IS_HANDLE_BOX (handlebox));

  if ((GtkPositionType) handlebox->handle_position != position) {
    handlebox->handle_position = position;
    GEDA_OBJECT_NOTIFY (handlebox, "handle-position");
    gtk_widget_queue_resize ((GtkWidget*)handlebox);
  }
}

/*!
 * \brief geda_handle_box_get_handle_position
 * \par Function Description
 *  Gets the handle position of the handle box. See
 *  geda_handle_box_set_handle_position().
 *
 * \param [in] handlebox The #GedaHandleBox object
 *
 * \returns the current handle position.
 */
GtkPositionType geda_handle_box_get_handle_position (GedaHandleBox *handlebox)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handlebox), GTK_POS_LEFT);

  return handlebox->handle_position;
}

/*!
 * \brief Sets the Shadow Type of a GedaHandleBox
 * \par Function Description
 *  Sets the type of shadow to be drawn around the border of the handle box.
 *
 * \param [in] handlebox The #GedaHandleBox object
 * \param [in] type       Enumerated ShadowType:
 *
 *  <DL>
 *    <DT>GTK_SHADOW_NONE</DT>
 *    <DT>GTK_SHADOW_IN</DT>
 *    <DT>GTK_SHADOW_OUT</DT>
 *    <DT>GTK_SHADOW_ETCHED_IN</DT>
 *    <DT>GTK_SHADOW_ETCHED_OUT</DT>
 *  </DL>
 */
void geda_handle_box_set_shadow_type (GedaHandleBox *handlebox, GtkShadowType type)
{
  g_return_if_fail (GEDA_IS_HANDLE_BOX (handlebox));

  if ((GtkShadowType) handlebox->shadow_type != type) {

    handlebox->shadow_type = type;

    GEDA_OBJECT_NOTIFY (handlebox, "shadow-type");

    gtk_widget_queue_resize ((GtkWidget*)handlebox);
  }
}

/*!
 * \brief Get the Shadow Type of a GedaHandleBox
 * \par Function Description
 *  Gets the type of shadow drawn around the handle box. See
 *  geda_handle_box_set_shadow_type().
 *
 * \param [in] handlebox   The #GedaHandleBox object
 *
 * \returns the type of shadow currently drawn around the handle box.
 */
GtkShadowType geda_handle_box_get_shadow_type (GedaHandleBox *handlebox)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handlebox), GTK_SHADOW_ETCHED_OUT);

  return handlebox->shadow_type;
}

/*!
 * \brief Get GedaHandleBox shrink-on-detach Property
 * \par Function Description
 *  Gets the shrink-on-detach property.
 *
 * \param [in] handlebox The #GedaHandleBox object
 *
 * \returns shrink-on-detach property or FALSE.
 */
bool geda_handle_box_get_shrink_on_detach (GedaHandleBox *handlebox)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handlebox), FALSE);

  return handlebox->shrink_on_detach;
}

/*!
 * \brief Set GedaHandleBox shrink-on-detach Property
 * \par Function Description
 *  Sets the shrink-on-detach property. When set, the size of the
 *  handlebox will be reduced to a single pixel whenever the child
 *  widget is detached from the handlebox. While not impossible,
 *  realignment is more challenging to manually re-dock siblings to
 *  the handlebox but removes the unsightly "narrow box" from the
 *  users screen. A child of a GedaHandleBox can easily be docked
 *  by double-clicking on the child handle.
 *
 * \param [in] handlebox The #GedaHandleBox object
 * \param [in] shrink     Whether the handle should shrink or not.
 */
void geda_handle_box_set_shrink_on_detach (GedaHandleBox *handlebox, bool shrink)
{
  g_return_if_fail (GEDA_IS_HANDLE_BOX (handlebox));

  handlebox->shrink_on_detach = shrink ? TRUE : FALSE;
}

/*!
 * \brief Sets the snap edge of a GedaHandleBox
 * \par Function Description
 * The snap edge is the edge of the detached child that must be aligned with
 * the corresponding edge of the "ghost" left behind when the child was detached
 * to reattach the torn-off window. Usually, the snap edge should be chosen so
 * that it stays in the same place on the screen when the handlebox is torn off.
 * If the snap edge is not set, then an appropriate value will be guessed from
 * the handle position. If the handle position is %GTK_POS_RIGHT or %GTK_POS_LEFT,
 * then the snap edge will be %GTK_POS_TOP, otherwise it will be %GTK_POS_LEFT.
 *
 * \param [in] handlebox  The #GedaHandleBox object
 * \param [in] edge       Enumerated GtkPositionType, can be:
 *
 *  <DL>
 *    <DT>GTK_POS_LEFT</DT>
 *    <DT>GTK_POS_RIGHT</DT>
 *    <DT>GTK_POS_TOP</DT>
 *    <DT>GTK_POS_BOTTOM</DT>
 *  </DL>
 */
void geda_handle_box_set_snap_edge (GedaHandleBox *handlebox, GtkPositionType edge)
{
  g_return_if_fail (GEDA_IS_HANDLE_BOX (handlebox));

  if (handlebox->snap_edge != edge) {

    handlebox->snap_edge = edge;

    g_object_freeze_notify ((GObject*)handlebox);
      GEDA_OBJECT_NOTIFY (handlebox, "snap-edge");
      GEDA_OBJECT_NOTIFY (handlebox, "snap-edge-set");
    g_object_thaw_notify ((GObject*)handlebox);
  }
}

/*!
 * \brief geda_handle_box_get_snap_edge
 * \par Function Description
 *  Gets the edge used for determining reattachment of the handle box. See
 *  geda_handle_box_set_snap_edge().
 *
 * \param [in] handlebox  The #GedaHandleBox object
 *
 * \returns the edge used for determining reattachment, or (GtkPositionType)-1
 *          if this is determined (as per default) from the handle position.
 */
GtkPositionType geda_handle_box_get_snap_edge (GedaHandleBox *handlebox)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handlebox), (GtkPositionType)-1);

  return handlebox->snap_edge;
}

/*!
 * \brief Get pointer to the containing Toolbar widget
 * \par Function Description
 *  Gets the child toolbar.
 *
 * \param [in] handlebox    The #GedaHandleBox object
 *
 * \returns pointer to toolbar widget or NULL if handlebox is not a GedaHandleBox.
 */
GtkToolbar *geda_handle_box_get_toolbar (GedaHandleBox *handlebox)
{
  if (GEDA_IS_HANDLE_BOX (handlebox)) {
    return (GtkToolbar*)gtk_bin_get_child((GtkBin*)handlebox);
  }
  else {
    BUG_MSG ("Operative is not a GedaHandleBox");
  }
  return NULL;
}

/*!
 * \brief Set or Replace the Toolbar in a GedaHandleBox
 * \par Function Description
 *  This function allows clients to change the toolbar that is contained
 *  within the #GedaHandleBox. The clients should maintain a reference
 *  to any previous toolbars if needed.
 *
 * \param [in] handlebox The #GedaHandleBox object
 * \param [in] toolbar   Toolbar object to be put in the handlebox
 */
void geda_handle_box_set_toolbar (GedaHandleBox *handlebox, GtkWidget *toolbar)
{
  if (GEDA_IS_HANDLE_BOX (handlebox)) {

    /* Can only have one child, check already have one */
    GtkWidget *current = gtk_bin_get_child((GtkBin*)handlebox);

    if (current) {
      geda_handle_box_remove ((GtkContainer*)handlebox, current);
    }

    geda_handle_box_add ((GtkContainer*)handlebox, toolbar);
  }
  else {
    BUG_MSG ("Operative is not a GedaHandleBox");
  }
}

/**
 * \defgroup GedaHandleBoxWidgets Handle Box Public Widget Functions
 * @{
 *
 * \par Begin Widget Versions
 */

/*!
 * \brief Set GedaHandleBox Widget Handle Position
 * \par Function Description
 *  Sets the side of the handlebox where the handle is drawn.
 *
 * \param [in] handlebox  The #GedaHandleBox object
 * \param [in] position   Side of the handlebox where the handle should
 *                        be drawn.
 *
 * \sa geda_handle_box_set_handle_position
 */
void geda_handle_widget_set_handle_position (GtkWidget       *handlebox,
                                             GtkPositionType  position)
{
  geda_handle_box_set_handle_position ((GedaHandleBox*)handlebox, position);
}

/*!
 * \brief Set GedaHandleBox Widget Shadow Type
 * \par Function Description
 *  Widget version of geda_handle_box_set_shadow_type.
 *
 * \param [in] handlebox  The #GedaHandleBox object
 * \param [in] type       Enumerated ShadowType
 *
 * \see geda_handle_box_set_shadow_type
 */
void geda_handle_widget_set_shadow_type (GtkWidget *handlebox, GtkShadowType type)
{
  geda_handle_box_set_shadow_type ((GedaHandleBox*)handlebox, type);
}

/*!
 * \brief Set GedaHandleBox Widget Shrink on Detach property
 * \par Function Description
 *  Widget version of geda_handle_box_set_shrink_on_detach.
 *
 * \param [in] handlebox The #GedaHandleBox object
 * \param [in] shrink    Whether the handle should shrink or not.
 *
 * \see geda_handle_box_set_shrink_on_detach
 */
void geda_handle_widget_set_shrink_on_detach (GtkWidget *handlebox, bool shrink)
{
  geda_handle_box_set_shrink_on_detach ((GedaHandleBox*)handlebox, shrink);
}

/*!
 * \brief Set GedaHandleBox Widget Snap Edge
 * \par Function Description
 *  The snap edge is the edge of the detached child that must be aligned
 *  with the corresponding edge of the "ghost" left behind when the child
 *  was detached to reattach the torn-off window.
 *
 * \param [in] handlebox The #GedaHandleBox widget
 * \param [in] edge      Enumerated GtkPositionType
 *
 * \see geda_handle_box_set_snap_edge
 */
void geda_handle_widget_set_snap_edge (GtkWidget *handlebox, GtkPositionType edge)
{
  geda_handle_box_set_snap_edge ((GedaHandleBox*)handlebox, edge);
}

/*!
 * \brief Set GedaHandleBox Widget Toolbar
 * \par Function Description
 *  Widget version of geda_handle_box_set_snap_edge.
 *
 * \param [in] handlebox The #GedaHandleBox widget
 * \param [in] toolbar   Toolbar object to be put in the handlebox
 *
 * \see geda_handle_box_set_snap_edge
 */
void geda_handle_widget_set_toolbar (GtkWidget *handlebox, GtkWidget *toolbar)
{
  geda_handle_box_set_toolbar ((GedaHandleBox*)handlebox, toolbar);
}

/** @} end group GedaHandleBoxWidgets */
/** @} end group GedaHandleBoxFunctions */
/** @} end group GedaHandleBox */
