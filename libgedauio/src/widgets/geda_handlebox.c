/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 * Copyright (C) 1998 Elliot Lee
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

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 *
 *  THIS FILE IS LGPL LICENSED, gEDA AS A WHOLE IS GPL LICENSED
 *
 * Adapted for gEDA by Wiley Edward Hill <wileyhill@gmail.com>
 *
 */

#include "config.h"
#include <geda.h>
#include <stdlib.h>

#include "geda_handlebox.h"

#include <gtk/gtk.h>
#include <gtk/gtkwindow.h>

#include "gettext.h"

/**
 * \brief GedaHandleBox - A Container Widget for toolbars
 * \par
 * A GedaHandleBox is a container object used toolbars. The GedaHandleBox
 * is a replacement for the GtkHandleBox because the GtkHandleBox is listed
 * as depreciated. A GedaHandleBox allows toolbars to float or be docked to
 * edges of a window.
 *
 * \defgroup GedaHandleBox Handle Box
 * @{
 */

typedef struct _GedaHandleBoxPrivate GedaHandleBoxPrivate;

struct _GedaHandleBoxPrivate
{
  int orig_x;
  int orig_y;
};

enum {
  PROP_0,
  PROP_SHADOW,
  PROP_SHADOW_TYPE,
  PROP_HANDLE_POSITION,
  PROP_SNAP_EDGE,
  PROP_SNAP_EDGE_SET,
  PROP_CHILD_DETACHED
};

#define DRAG_HANDLE_SIZE 10
#define CHILDLESS_SIZE	25
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
 * WEH | 03/29/14 | Add case GDK_2BUTTON_PRESS for in function
 *                | geda_handle_box_grab_event (so floating toolbars will dock
 *                | when doubled click, removed reference to GDK_2BUTTON_PRESS
 *                | from geda_handle_box_button_press (because this procedure
 *                | never sees a GDK_2BUTTON_PRESS event.) This is broke in Gtk
 *                | version. Simplified conditionals in geda_handle_box_button
 *                | _press, which checked for GDK_2BUTTON_PRESS. Converted
 *                | comments to Doxygen, (was latex?)
*/
static void geda_handle_box_set_property  (GObject        *object,
                                           unsigned int    param_id,
                                           const GValue   *value,
                                           GParamSpec     *pspec);
static void geda_handle_box_get_property  (GObject        *object,
                                           unsigned int    param_id,
                                           GValue         *value,
                                           GParamSpec     *pspec);
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
static void geda_handle_box_draw_ghost    (GedaHandleBox   *handlebox);
static void geda_handle_box_paint         (GtkWidget      *widget,
                                           GdkEventExpose *event,
                                           GdkRectangle   *area);
static bool geda_handle_box_expose        (GtkWidget      *widget,
                                           GdkEventExpose *event);
static bool geda_handle_box_button_press  (GtkWidget      *widget,
                                           GdkEventButton *event);
static bool geda_handle_box_motion        (GtkWidget      *widget,
                                           GdkEventMotion *event);
static bool geda_handle_box_delete_event  (GtkWidget      *widget,
                                           GdkEventAny    *event);
static void geda_handle_box_reattach      (GedaHandleBox  *handlebox);
static void geda_handle_box_end_drag      (GedaHandleBox  *handlebox,
                                           unsigned int    time);

static unsigned int handle_box_signals[SIGNAL_LAST] = { 0 };

G_DEFINE_TYPE (GedaHandleBox, geda_handle_box, GTK_TYPE_BIN)

static void
geda_handle_box_class_init (GedaHandleBoxClass *class)
{
  GObjectClass *gobject_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  gobject_class   = (GObjectClass *) class;
  widget_class    = (GtkWidgetClass *) class;
  container_class = (GtkContainerClass *) class;

  gobject_class->set_property = geda_handle_box_set_property;
  gobject_class->get_property = geda_handle_box_get_property;

  g_object_class_install_property (gobject_class,
                                   PROP_SHADOW,
                                   g_param_spec_enum ("shadow",
                                                      NULL,
                                                      _("Appearance of the shadow that surrounds the container"),
                                                      GTK_TYPE_SHADOW_TYPE,
                                                      GTK_SHADOW_OUT,
                                                      G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_SHADOW_TYPE,
                                   g_param_spec_enum ("shadow-type",
                                                      NULL,
                                                      _("Appearance of the shadow that surrounds the container"),
                                                      GTK_TYPE_SHADOW_TYPE,
                                                      GTK_SHADOW_OUT,
                                                      G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_HANDLE_POSITION,
                                   g_param_spec_enum ("handle-position",
                                                      NULL,
                                                      _("Position of the handle relative to the child widget"),
                                                      GTK_TYPE_POSITION_TYPE,
                                                      GTK_POS_LEFT,
                                                      G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_SNAP_EDGE,
                                   g_param_spec_enum ("snap-edge",
                                                      NULL,
                                                      _("Side of the handlebox that's lined up with the docking point to dock the handlebox"),
                                                      GTK_TYPE_POSITION_TYPE,
                                                      GTK_POS_TOP,
                                                      G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_SNAP_EDGE_SET,
                                   g_param_spec_boolean ("snap-edge-set",
                                                         NULL,
                                                         _("Whether to use the value from the snap_edge property or a value derived from handle_position"),
                                                         FALSE,
                                                         G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_CHILD_DETACHED,
                                   g_param_spec_boolean ("child-detached",
                                                         NULL,
                                                         _("A boolean value indicating whether the handlebox's child is attached or detached."),
                                                         FALSE,
                                                         G_PARAM_READABLE));

  widget_class->map = geda_handle_box_map;
  widget_class->unmap = geda_handle_box_unmap;
  widget_class->realize = geda_handle_box_realize;
  widget_class->unrealize = geda_handle_box_unrealize;
  widget_class->style_set = geda_handle_box_style_set;
  widget_class->size_request = geda_handle_box_size_request;
  widget_class->size_allocate = geda_handle_box_size_allocate;
  widget_class->expose_event = geda_handle_box_expose;
  widget_class->button_press_event = geda_handle_box_button_press;
  widget_class->delete_event = geda_handle_box_delete_event;

  container_class->add = geda_handle_box_add;
  container_class->remove = geda_handle_box_remove;

  class->child_attached = NULL;
  class->child_detached = NULL;

  handle_box_signals[SIGNAL_CHILD_ATTACHED] =
  g_signal_new ("child-attached",
                G_TYPE_FROM_CLASS (class),
                G_SIGNAL_RUN_FIRST,
                G_STRUCT_OFFSET (GedaHandleBoxClass, child_attached),
                NULL,
                NULL,
                g_cclosure_marshal_VOID__POINTER,
                G_TYPE_NONE, 1,
                G_TYPE_POINTER);

  handle_box_signals[SIGNAL_CHILD_DETACHED] =
  g_signal_new ("child-detached",
                G_TYPE_FROM_CLASS (class),
                G_SIGNAL_RUN_FIRST,
                G_STRUCT_OFFSET (GedaHandleBoxClass, child_detached),
                NULL, NULL,
                g_cclosure_marshal_VOID__POINTER,
                G_TYPE_NONE, 1,
                G_TYPE_POINTER);

  g_type_class_add_private (gobject_class, sizeof (GedaHandleBoxPrivate));


}

static GedaHandleBoxPrivate *
geda_handle_box_get_private (GedaHandleBox *handlebox)
{
  return G_TYPE_INSTANCE_GET_PRIVATE (handlebox, GEDA_TYPE_HANDLE_BOX, GedaHandleBoxPrivate);
}

static void
geda_handle_box_init (GedaHandleBox *handle_box)
{
  gtk_widget_set_has_window (GTK_WIDGET (handle_box), TRUE);

  handle_box->bin_window = NULL;
  handle_box->float_window = NULL;
  handle_box->shadow_type = GTK_SHADOW_OUT;
  handle_box->handle_position = GTK_POS_LEFT;
  handle_box->float_window_mapped = FALSE;
  handle_box->child_detached = FALSE;
  handle_box->in_drag = FALSE;
  handle_box->shrink_on_detach = TRUE;
  handle_box->snap_edge = -1;
  handle_box->dock_orientation = GTK_ORIENTATION_HORIZONTAL;
}

static void
geda_handle_box_set_property (GObject  *object, unsigned int prop_id,
                              const GValue *value, GParamSpec *pspec)
{
  GedaHandleBox *handle_box = GEDA_HANDLE_BOX (object);

  switch (prop_id)
  {
    case PROP_SHADOW:
    case PROP_SHADOW_TYPE:
      geda_handle_box_set_shadow_type (handle_box, g_value_get_enum (value));
      break;
    case PROP_HANDLE_POSITION:
      geda_handle_box_set_handle_position (handle_box, g_value_get_enum (value));
      break;
    case PROP_SNAP_EDGE:
      geda_handle_box_set_snap_edge (handle_box, g_value_get_enum (value));
      break;
    case PROP_SNAP_EDGE_SET:
      if (!g_value_get_boolean (value))
        geda_handle_box_set_snap_edge (handle_box, (GtkPositionType)-1);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_handle_box_get_property (GObject        *object,
                             unsigned int     prop_id,
                             GValue          *value,
                             GParamSpec      *pspec)
{
  GedaHandleBox *handle_box = GEDA_HANDLE_BOX (object);

  switch (prop_id)
  {
    case PROP_SHADOW:
    case PROP_SHADOW_TYPE:
      g_value_set_enum (value, handle_box->shadow_type);
      break;
    case PROP_HANDLE_POSITION:
      g_value_set_enum (value, handle_box->handle_position);
      break;
    case PROP_SNAP_EDGE:
      g_value_set_enum (value,
                        (handle_box->snap_edge == -1 ?
                        GTK_POS_TOP : handle_box->snap_edge));
      break;
    case PROP_SNAP_EDGE_SET:
      g_value_set_boolean (value, handle_box->snap_edge != -1);
      break;
    case PROP_CHILD_DETACHED:
      g_value_set_boolean (value, handle_box->child_detached);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

GtkWidget* geda_handle_box_new (void)
{
  return g_object_new (GEDA_TYPE_HANDLE_BOX, NULL);
}


static void geda_handle_box_map (GtkWidget *widget)
{
  GtkBin *bin;
  GedaHandleBox *handlebox;

  gtk_widget_set_mapped (widget, TRUE);

  bin = GTK_BIN (widget);
  handlebox = GEDA_HANDLE_BOX (widget);

  if (bin->child &&
      gtk_widget_get_visible (bin->child) &&
     !gtk_widget_get_mapped (bin->child))
    gtk_widget_map (bin->child);

  if (handlebox->child_detached && !handlebox->float_window_mapped)
    {
      gdk_window_show (handlebox->float_window);
      handlebox->float_window_mapped = TRUE;
    }

  gdk_window_show (handlebox->bin_window);
  gdk_window_show (widget->window);
}

static void geda_handle_box_unmap (GtkWidget *widget)
{
  GedaHandleBox *handlebox;

  gtk_widget_set_mapped (widget, FALSE);

  handlebox = GEDA_HANDLE_BOX (widget);

  gdk_window_hide (widget->window);
  if (handlebox->float_window_mapped)
    {
      gdk_window_hide (handlebox->float_window);
      handlebox->float_window_mapped = FALSE;
    }
}

static void geda_handle_box_realize (GtkWidget *widget)
{
  GdkWindowAttr attributes;
  int attributes_mask;
  GedaHandleBox *handlebox;

  handlebox = GEDA_HANDLE_BOX (widget);

  gtk_widget_set_realized (widget, TRUE);

  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);
  attributes.event_mask = (gtk_widget_get_events (widget)
                           | GDK_EXPOSURE_MASK);
  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
  widget->window = gdk_window_new (gtk_widget_get_parent_window (widget), &attributes, attributes_mask);
  gdk_window_set_user_data (widget->window, widget);

  attributes.x = 0;
  attributes.y = 0;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.event_mask = (gtk_widget_get_events (widget) |
                           GDK_EXPOSURE_MASK |
                           GDK_BUTTON1_MOTION_MASK |
                           GDK_POINTER_MOTION_HINT_MASK |
                           GDK_BUTTON_PRESS_MASK |
                            GDK_BUTTON_RELEASE_MASK);
  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
  handlebox->bin_window = gdk_window_new (widget->window, &attributes, attributes_mask);
  gdk_window_set_user_data (handlebox->bin_window, widget);
  if (GTK_BIN (handlebox)->child)
    gtk_widget_set_parent_window (GTK_BIN (handlebox)->child, handlebox->bin_window);

  attributes.x = 0;
  attributes.y = 0;
  attributes.width = widget->requisition.width;
  attributes.height = widget->requisition.height;
  attributes.window_type = GDK_WINDOW_TOPLEVEL;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);
  attributes.event_mask = (gtk_widget_get_events (widget) |
                           GDK_KEY_PRESS_MASK |
                           GDK_ENTER_NOTIFY_MASK |
                           GDK_LEAVE_NOTIFY_MASK |
                           GDK_FOCUS_CHANGE_MASK |
                           GDK_STRUCTURE_MASK);
  attributes.type_hint = GDK_WINDOW_TYPE_HINT_TOOLBAR;
  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP | GDK_WA_TYPE_HINT;
  handlebox->float_window = gdk_window_new (gtk_widget_get_root_window (widget),
                                     &attributes, attributes_mask);
  gdk_window_set_user_data (handlebox->float_window, widget);
  gdk_window_set_decorations (handlebox->float_window, 0);
  gdk_window_set_type_hint (handlebox->float_window, GDK_WINDOW_TYPE_HINT_TOOLBAR);

  widget->style = gtk_style_attach (widget->style, widget->window);
  gtk_style_set_background (widget->style, widget->window, gtk_widget_get_state (widget));
  gtk_style_set_background (widget->style, handlebox->bin_window, gtk_widget_get_state (widget));
  gtk_style_set_background (widget->style, handlebox->float_window, gtk_widget_get_state (widget));
  gdk_window_set_back_pixmap (widget->window, NULL, TRUE);
}

static void geda_handle_box_unrealize (GtkWidget *widget)
{
  GedaHandleBox *handlebox = GEDA_HANDLE_BOX (widget);

  gdk_window_set_user_data (handlebox->bin_window, NULL);
  gdk_window_destroy (handlebox->bin_window);
  handlebox->bin_window = NULL;
  gdk_window_set_user_data (handlebox->float_window, NULL);
  gdk_window_destroy (handlebox->float_window);
  handlebox->float_window = NULL;

  GTK_WIDGET_CLASS (geda_handle_box_parent_class)->unrealize (widget);
}

static void
geda_handle_box_style_set (GtkWidget *widget, GtkStyle *previous_style)
{
  GedaHandleBox *handlebox = GEDA_HANDLE_BOX (widget);

  if (gtk_widget_get_realized (widget) &&
      gtk_widget_get_has_window (widget))
    {
      gtk_style_set_background (widget->style, widget->window,
				widget->state);
      gtk_style_set_background (widget->style, handlebox->bin_window, widget->state);
      gtk_style_set_background (widget->style, handlebox->float_window, widget->state);
    }
}

static int effective_handle_position (GedaHandleBox *handlebox)
{
  int handle_position;

  if (gtk_widget_get_direction (GTK_WIDGET (handlebox)) == GTK_TEXT_DIR_LTR)
    handle_position = handlebox->handle_position;
  else
    {
      switch (handlebox->handle_position)
	{
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

static void
geda_handle_box_size_request (GtkWidget      *widget,
                              GtkRequisition *requisition)
{
  GtkBin *bin;
  GedaHandleBox *handlebox;
  GtkRequisition child_requisition;
  int handle_position;

  bin = GTK_BIN (widget);
  handlebox = GEDA_HANDLE_BOX (widget);

  handle_position = effective_handle_position (handlebox);

  if (handle_position == GTK_POS_LEFT ||
    handle_position == GTK_POS_RIGHT)
  {
    requisition->width = DRAG_HANDLE_SIZE;
    requisition->height = 0;
  }
  else
  {
    requisition->width = 0;
    requisition->height = DRAG_HANDLE_SIZE;
  }

  /* if our child is not visible, we still request its size, since we
   * won't have any useful hint for our size otherwise.
   */
  if (bin->child)
    gtk_widget_size_request (bin->child, &child_requisition);
  else
  {
    child_requisition.width = 0;
    child_requisition.height = 0;
  }

  if (handlebox->child_detached)
  {
    /* FIXME: This doesn't work currently */
    if (!handlebox->shrink_on_detach)
    {
      if (handle_position == GTK_POS_LEFT ||
        handle_position == GTK_POS_RIGHT)
        requisition->height += child_requisition.height;
      else
        requisition->width += child_requisition.width;
    }
    else
    {
      if (handle_position == GTK_POS_LEFT ||
        handle_position == GTK_POS_RIGHT)
        requisition->height += widget->style->ythickness;
      else
        requisition->width += widget->style->xthickness;
    }
  }
  else
  {
    requisition->width += GTK_CONTAINER (widget)->border_width * 2;
    requisition->height += GTK_CONTAINER (widget)->border_width * 2;

    if (bin->child)
    {
      requisition->width += child_requisition.width;
      requisition->height += child_requisition.height;
    }
    else
    {
      requisition->width += CHILDLESS_SIZE;
      requisition->height += CHILDLESS_SIZE;
    }
  }
}

static void
geda_handle_box_size_allocate (GtkWidget     *widget,
                               GtkAllocation *allocation)
{
  GtkBin *bin;
  GedaHandleBox *handlebox;
  GtkRequisition child_requisition;
  int handle_position;

  bin = GTK_BIN (widget);
  handlebox = GEDA_HANDLE_BOX (widget);

  handle_position = effective_handle_position (handlebox);

  if (bin->child)
    gtk_widget_get_child_requisition (bin->child, &child_requisition);
  else
  {
    child_requisition.width = 0;
    child_requisition.height = 0;
  }

  widget->allocation = *allocation;

  if (gtk_widget_get_realized (widget))
    gdk_window_move_resize (widget->window,
                            widget->allocation.x,
                            widget->allocation.y,
                            widget->allocation.width,
                            widget->allocation.height);


    if (bin->child && gtk_widget_get_visible (bin->child))
    {
      GtkAllocation child_allocation;
      unsigned int border_width;

      border_width = GTK_CONTAINER (widget)->border_width;

      child_allocation.x = border_width;
      child_allocation.y = border_width;
      if (handle_position == GTK_POS_LEFT)
        child_allocation.x += DRAG_HANDLE_SIZE;
      else if (handle_position == GTK_POS_TOP)
        child_allocation.y += DRAG_HANDLE_SIZE;

      if (handlebox->child_detached)
      {
        unsigned int float_width;
        unsigned int float_height;

        child_allocation.width = child_requisition.width;
        child_allocation.height = child_requisition.height;

        float_width = child_allocation.width + 2 * border_width;
        float_height = child_allocation.height + 2 * border_width;

        if (handle_position == GTK_POS_LEFT ||
          handle_position == GTK_POS_RIGHT)
          float_width += DRAG_HANDLE_SIZE;
        else
          float_height += DRAG_HANDLE_SIZE;

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
        child_allocation.width = MAX (1, (int)widget->allocation.width - 2 * border_width);
        child_allocation.height = MAX (1, (int)widget->allocation.height - 2 * border_width);

        if (handle_position == GTK_POS_LEFT ||
          handle_position == GTK_POS_RIGHT)
          child_allocation.width -= DRAG_HANDLE_SIZE;
        else
          child_allocation.height -= DRAG_HANDLE_SIZE;

        if (gtk_widget_get_realized (widget))
          gdk_window_move_resize (handlebox->bin_window,
                                  0,
                                  0,
                                  widget->allocation.width,
                                  widget->allocation.height);
      }

      gtk_widget_size_allocate (bin->child, &child_allocation);
    }
}

static void
geda_handle_box_draw_ghost (GedaHandleBox *handlebox)
{
  GtkWidget *widget;
  unsigned int x;
  unsigned int y;
  unsigned int width;
  unsigned int height;
  int handle_position;

  widget = GTK_WIDGET (handlebox);

  handle_position = effective_handle_position (handlebox);
  if (handle_position == GTK_POS_LEFT ||
    handle_position == GTK_POS_RIGHT)
  {
    x = handle_position == GTK_POS_LEFT ? 0 : widget->allocation.width - DRAG_HANDLE_SIZE;
    y = 0;
    width = DRAG_HANDLE_SIZE;
    height = widget->allocation.height;
  }
  else
  {
    x = 0;
    y = handle_position == GTK_POS_TOP ? 0 : widget->allocation.height - DRAG_HANDLE_SIZE;
    width = widget->allocation.width;
    height = DRAG_HANDLE_SIZE;
  }
  gtk_paint_shadow (widget->style,
                    widget->window,
                    gtk_widget_get_state (widget),
                    GTK_SHADOW_ETCHED_IN,
                    NULL, widget, "handle",
                    x,
                    y,
                    width,
                    height);
  if (handle_position == GTK_POS_LEFT ||
    handle_position == GTK_POS_RIGHT)
    gtk_paint_hline (widget->style,
                     widget->window,
                     gtk_widget_get_state (widget),
                     NULL, widget, "handlebox",
                     handle_position == GTK_POS_LEFT ? DRAG_HANDLE_SIZE : 0,
                     handle_position == GTK_POS_LEFT ? widget->allocation.width : widget->allocation.width - DRAG_HANDLE_SIZE,
                     widget->allocation.height / 2);
    else
      gtk_paint_vline (widget->style,
                       widget->window,
                       gtk_widget_get_state (widget),
                       NULL, widget, "handlebox",
                       handle_position == GTK_POS_TOP ? DRAG_HANDLE_SIZE : 0,
                       handle_position == GTK_POS_TOP ? widget->allocation.height : widget->allocation.height - DRAG_HANDLE_SIZE,
                       widget->allocation.width / 2);
}

static void
draw_textured_frame (GtkWidget *widget, GdkWindow *window, GdkRectangle *rect,
                     GtkShadowType shadow, GdkRectangle *clip,
                     GtkOrientation orientation)
{
  gtk_paint_handle (widget->style, window, GTK_STATE_NORMAL, shadow,
                    clip, widget, "handlebox",
                    rect->x, rect->y, rect->width, rect->height,
                    orientation);
}

void
geda_handle_box_set_shadow_type (GedaHandleBox *handle_box, GtkShadowType type)
{
  g_return_if_fail (GEDA_IS_HANDLE_BOX (handle_box));

  if ((GtkShadowType) handle_box->shadow_type != type)
    {
      handle_box->shadow_type = type;
      g_object_notify (G_OBJECT (handle_box), "shadow-type");
      gtk_widget_queue_resize (GTK_WIDGET (handle_box));
    }
}

/*! \brief geda_handle_box_get_shadow_type
 *
 *  \par Function Description
 *
 * Gets the type of shadow drawn around the handle box. See
 * geda_handle_box_set_shadow_type().
 *
 * \param [in] handle_box    The #GedaHandleBox object
 *
 * Return value: the type of shadow currently drawn around the handle box.
 */
GtkShadowType
geda_handle_box_get_shadow_type (GedaHandleBox *handle_box)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handle_box), GTK_SHADOW_ETCHED_OUT);

  return handle_box->shadow_type;
}

void
geda_handle_box_set_handle_position  (GedaHandleBox    *handle_box,
                                     GtkPositionType  position)
{
  g_return_if_fail (GEDA_IS_HANDLE_BOX (handle_box));

  if ((GtkPositionType) handle_box->handle_position != position)
    {
      handle_box->handle_position = position;
      g_object_notify (G_OBJECT (handle_box), "handle-position");
      gtk_widget_queue_resize (GTK_WIDGET (handle_box));
    }
}

/*! \brief geda_handle_box_get_handle_position
 *
 *  \par Function Description
 *
 * Gets the handle position of the handle box. See
 * geda_handle_box_set_handle_position().
 *
 * \param [in] handle_box    The #GedaHandleBox object
 *
 * Return value: the current handle position.
 */
GtkPositionType
geda_handle_box_get_handle_position (GedaHandleBox *handle_box)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handle_box), GTK_POS_LEFT);

  return handle_box->handle_position;
}

void
geda_handle_box_set_snap_edge        (GedaHandleBox    *handle_box,
                                     GtkPositionType  edge)
{
  g_return_if_fail (GEDA_IS_HANDLE_BOX (handle_box));

  if (handle_box->snap_edge != edge)
    {
      handle_box->snap_edge = edge;

      g_object_freeze_notify (G_OBJECT (handle_box));
      g_object_notify (G_OBJECT (handle_box), "snap-edge");
      g_object_notify (G_OBJECT (handle_box), "snap-edge-set");
      g_object_thaw_notify (G_OBJECT (handle_box));
    }
}

/*! \brief geda_handle_box_get_snap_edge
 *
 *  \par Function Description
 *
 * Gets the edge used for determining reattachment of the handle box. See
 * geda_handle_box_set_snap_edge().
 *
 * \param [in] handle_box    The #GedaHandleBox object
 *
 * Return value: the edge used for determining reattachment, or (GtkPositionType)-1 if this
 *               is determined (as per default) from the handle position.
 */
GtkPositionType
geda_handle_box_get_snap_edge (GedaHandleBox *handle_box)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handle_box), (GtkPositionType)-1);

  return handle_box->snap_edge;
}

/*! \brief geda_handle_box_get_child_detached
 *
 *  \par Function Description
 *
 * Whether the handlebox's child is currently detached.
 *
 * \param [in] handle_box    The #GedaHandleBox object
 *
 * Return value: %TRUE if the child is currently detached, otherwise %FALSE
 *
 */
bool
geda_handle_box_get_child_detached (GedaHandleBox *handle_box)
{
  g_return_val_if_fail (GEDA_IS_HANDLE_BOX (handle_box), FALSE);

  return handle_box->child_detached;
}

static void
geda_handle_box_paint (GtkWidget      *widget, GdkEventExpose *event, GdkRectangle   *area)
{
  GtkBin *bin;
  GedaHandleBox *handlebox;
  int width, height;
  GdkRectangle rect;
  GdkRectangle dest;
  int handle_position;
  GtkOrientation handle_orientation;

  bin       = GTK_BIN (widget);
  handlebox = GEDA_HANDLE_BOX (widget);

  handle_orientation = GTK_ORIENTATION_VERTICAL;
  handle_position    = effective_handle_position (handlebox);

  width  = gdk_window_get_width (handlebox->bin_window);
  height = gdk_window_get_height (handlebox->bin_window);

  if (!event)
    gtk_paint_box (widget->style,
                   handlebox->bin_window,
                   gtk_widget_get_state (widget),
                   handlebox->shadow_type,
                   area, widget, "handlebox_bin",
                   0, 0, -1, -1);
    else
      gtk_paint_box (widget->style,
                     handlebox->bin_window,
                     gtk_widget_get_state (widget),
                     handlebox->shadow_type,
                     &event->area, widget, "handlebox_bin",
                     0, 0, -1, -1);

      /* We currently draw the handle _above_ the relief of the handlebox.
       * it could also be drawn on the same level...
       *
       *	 handlebox->handle_position == GTK_POS_LEFT ? DRAG_HANDLE_SIZE : 0,
       *	 handlebox->handle_position == GTK_POS_TOP ? DRAG_HANDLE_SIZE : 0,
       *	 width,
       *	 height);*/

      switch (handle_position)
      {
        case GTK_POS_LEFT:
          rect.x = 0;
          rect.y = 0;
          rect.width = DRAG_HANDLE_SIZE;
          rect.height = height;
          handle_orientation = GTK_ORIENTATION_VERTICAL;
          break;
        case GTK_POS_RIGHT:
          rect.x = width - DRAG_HANDLE_SIZE;
          rect.y = 0;
          rect.width = DRAG_HANDLE_SIZE;
          rect.height = height;
          handle_orientation = GTK_ORIENTATION_VERTICAL;
          break;
        case GTK_POS_TOP:
          rect.x = 0;
          rect.y = 0;
          rect.width = width;
          rect.height = DRAG_HANDLE_SIZE;
          handle_orientation = GTK_ORIENTATION_HORIZONTAL;
          break;
        case GTK_POS_BOTTOM:
          rect.x = 0;
          rect.y = height - DRAG_HANDLE_SIZE;
          rect.width = width;
          rect.height = DRAG_HANDLE_SIZE;
          handle_orientation = GTK_ORIENTATION_HORIZONTAL;
          break;
        default:
          fprintf (stderr, "Internal Error: <%s><__func__>"
                           "unhandler case for <%d>, line %d.\n",
                           __FILE__, handle_position, __LINE__);
          break;
      }

      if (gdk_rectangle_intersect (event ? &event->area : area, &rect, &dest)) {
        draw_textured_frame (widget, handlebox->bin_window, &rect,
                             GTK_SHADOW_OUT,
                             event ? &event->area : area,
                             handle_orientation);
      }

      if (bin->child && gtk_widget_get_visible (bin->child)) {
        GTK_WIDGET_CLASS (geda_handle_box_parent_class)->expose_event (widget, event);
      }
}

static bool
geda_handle_box_expose (GtkWidget      *widget, GdkEventExpose *event)
{
  GedaHandleBox *handlebox;

  if (gtk_widget_is_drawable (widget))
  {
    handlebox = GEDA_HANDLE_BOX (widget);

    if (event->window == widget->window)
    {
      if (handlebox->child_detached)
        geda_handle_box_draw_ghost (handlebox);
    }
    else
      geda_handle_box_paint (widget, event, NULL);
  }

  return FALSE;
}

static GtkWidget *geda_handle_box_get_invisible (void)
{
  static GtkWidget *handle_box_invisible = NULL;

  if (!handle_box_invisible)
    {
      handle_box_invisible = gtk_invisible_new ();
      gtk_widget_show (handle_box_invisible);
    }

  return handle_box_invisible;
}

static bool
geda_handle_box_grab_event(GtkWidget *widget, GdkEvent *event, GedaHandleBox *handlebox)
{
  bool event_handled = FALSE;

  switch (event->type)
  {
    case GDK_BUTTON_RELEASE:
      if (handlebox->in_drag)		/* sanity check */
      {
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
      return geda_handle_box_motion (GTK_WIDGET (handlebox), (GdkEventMotion *)event);
      break;

    default:
      break;
  }

  return event_handled;
}

static bool
geda_handle_box_button_press (GtkWidget *widget, GdkEventButton *event)
{
  GedaHandleBox *handlebox;
  bool event_handled;
  GdkCursor *fleur;
  int handle_position;

  event_handled = FALSE;

  handlebox = GEDA_HANDLE_BOX (widget);

  handle_position = effective_handle_position (handlebox);

  if (event->button == 1) {

    if (event->type == GDK_BUTTON_PRESS) {

      GtkWidget *child;
      bool in_handle;

      if (event->window != handlebox->bin_window)
        return FALSE;

      child = GTK_BIN (handlebox)->child;

      if (child)
      {

        switch (handle_position)
        {
          case GTK_POS_LEFT:
            in_handle = event->x < DRAG_HANDLE_SIZE;
            break;
          case GTK_POS_TOP:
            in_handle = event->y < DRAG_HANDLE_SIZE;
            break;
          case GTK_POS_RIGHT:
            in_handle = event->x > 2 * GTK_CONTAINER (handlebox)->border_width + child->allocation.width;
            break;
          case GTK_POS_BOTTOM:
            in_handle = event->y > 2 * GTK_CONTAINER (handlebox)->border_width + child->allocation.height;
            break;
          default:
            in_handle = FALSE;
            break;
        }
      }
      else
      {
        in_handle = FALSE;
        event_handled = TRUE;
      }

      if (in_handle)
      {

        GedaHandleBoxPrivate *private = geda_handle_box_get_private (handlebox);
        GtkWidget *invisible = geda_handle_box_get_invisible ();
        int desk_x, desk_y;
        int root_x, root_y;
        int width, height;

        gtk_invisible_set_screen (GTK_INVISIBLE (invisible),
                                  gtk_widget_get_screen (GTK_WIDGET (handlebox)));
        gdk_window_get_deskrelative_origin (handlebox->bin_window, &desk_x, &desk_y);
        gdk_window_get_origin (handlebox->bin_window, &root_x, &root_y);
        width = gdk_window_get_width (handlebox->bin_window);
        height = gdk_window_get_height (handlebox->bin_window);

        private->orig_x = event->x_root;
        private->orig_y = event->y_root;

        handlebox->float_allocation.x = root_x - event->x_root;
        handlebox->float_allocation.y = root_y - event->y_root;
        handlebox->float_allocation.width = width;
        handlebox->float_allocation.height = height;

        handlebox->deskoff_x = desk_x - root_x;
        handlebox->deskoff_y = desk_y - root_y;

        if (gdk_window_is_viewable (widget->window))
        {
          gdk_window_get_origin (widget->window, &root_x, &root_y);
          width = gdk_window_get_width (widget->window);
          height = gdk_window_get_height (widget->window);

          handlebox->attach_allocation.x = root_x;
          handlebox->attach_allocation.y = root_y;
          handlebox->attach_allocation.width = width;
          handlebox->attach_allocation.height = height;
        }
        else
        {
          handlebox->attach_allocation.x = -1;
          handlebox->attach_allocation.y = -1;
          handlebox->attach_allocation.width = 0;
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
  }
  return event_handled;
}

static bool
geda_handle_box_motion (GtkWidget *widget, GdkEventMotion *event)
{
  GedaHandleBox *handlebox = GEDA_HANDLE_BOX (widget);
  int new_x, new_y;
  int snap_edge;
  bool is_snapped = FALSE;
  int handle_position;
  GdkGeometry geometry;
  GdkScreen *screen, *pointer_screen;

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
  if (pointer_screen != screen)
  {
    GedaHandleBoxPrivate *private = geda_handle_box_get_private (handlebox);

    new_x = private->orig_x;
    new_y = private->orig_y;
  }

  new_x += handlebox->float_allocation.x;
  new_y += handlebox->float_allocation.y;

  snap_edge = handlebox->snap_edge;
  if (snap_edge == -1)
    snap_edge = (handle_position == GTK_POS_LEFT ||
    handle_position == GTK_POS_RIGHT) ?
    GTK_POS_TOP : GTK_POS_LEFT;

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL)
    switch (snap_edge)
    {
      case GTK_POS_LEFT:
        snap_edge = GTK_POS_RIGHT;
        break;
      case GTK_POS_RIGHT:
        snap_edge = GTK_POS_LEFT;
        break;
      default:
        break;
    }

    /* First, check if the snapped edge is aligned */
    switch (snap_edge)
    {
      case GTK_POS_TOP:
        is_snapped = abs (handlebox->attach_allocation.y - new_y) < TOLERANCE;
        break;
      case GTK_POS_BOTTOM:
        is_snapped = abs (handlebox->attach_allocation.y + (int)handlebox->attach_allocation.height -
        new_y - (int)handlebox->float_allocation.height) < TOLERANCE;
        break;
      case GTK_POS_LEFT:
        is_snapped = abs (handlebox->attach_allocation.x - new_x) < TOLERANCE;
        break;
      case GTK_POS_RIGHT:
        is_snapped = abs (handlebox->attach_allocation.x + (int)handlebox->attach_allocation.width -
        new_x - (int)handlebox->float_allocation.width) < TOLERANCE;
        break;
    }

    /* Next, check if coordinates in the other direction are sufficiently aligned */
    if (is_snapped)
    {
      int float_pos1  = 0;	/* Initialize to suppress warnings */
      int float_pos2  = 0;
      int attach_pos1 = 0;
      int attach_pos2 = 0;

      switch (snap_edge)
      {
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

      is_snapped = ((attach_pos1 - TOLERANCE < float_pos1) &&
      (attach_pos2 + TOLERANCE > float_pos2)) ||
      ((float_pos1  - TOLERANCE < attach_pos1) &&
      (float_pos2  + TOLERANCE > attach_pos2));
    }

    if (is_snapped) {
      if (handlebox->child_detached) {
        handlebox->child_detached = FALSE;
        gdk_window_hide (handlebox->float_window);
        gdk_window_reparent (handlebox->bin_window, widget->window, 0, 0);
        handlebox->float_window_mapped = FALSE;
        g_signal_emit (handlebox, handle_box_signals[SIGNAL_CHILD_ATTACHED],
                       0, GTK_BIN (handlebox)->child);
        gtk_widget_queue_resize (widget);
      }
    }
    else
    {
      int width, height;

      width = gdk_window_get_width (handlebox->float_window);
      height = gdk_window_get_height (handlebox->float_window);
      new_x += handlebox->deskoff_x;
      new_y += handlebox->deskoff_y;

      switch (handle_position)
      {
        case GTK_POS_LEFT:
          new_y += ((int)handlebox->float_allocation.height - height) / 2;
          break;
        case GTK_POS_RIGHT:
          new_x += (int)handlebox->float_allocation.width - width;
          new_y += ((int)handlebox->float_allocation.height - height) / 2;
          break;
        case GTK_POS_TOP:
          new_x += ((int)handlebox->float_allocation.width - width) / 2;
          break;
        case GTK_POS_BOTTOM:
          new_x += ((int)handlebox->float_allocation.width - width) / 2;
          new_y += (int)handlebox->float_allocation.height - height;
          break;
      }

      if (handlebox->child_detached)
      {
        gdk_window_move (handlebox->float_window, new_x, new_y);
        gdk_window_raise (handlebox->float_window);
      }
      else
      {
        int width;
        int height;
        GtkRequisition child_requisition;

        handlebox->child_detached = TRUE;

        if (GTK_BIN (handlebox)->child)
          gtk_widget_get_child_requisition (GTK_BIN (handlebox)->child, &child_requisition);
        else
        {
          child_requisition.width = 0;
          child_requisition.height = 0;
        }

        width = child_requisition.width + 2 * GTK_CONTAINER (handlebox)->border_width;
        height = child_requisition.height + 2 * GTK_CONTAINER (handlebox)->border_width;

        if (handle_position == GTK_POS_LEFT || handle_position == GTK_POS_RIGHT)
          width += DRAG_HANDLE_SIZE;
        else
          height += DRAG_HANDLE_SIZE;

        gdk_window_move_resize (handlebox->float_window, new_x, new_y, width, height);
        gdk_window_reparent (handlebox->bin_window, handlebox->float_window, 0, 0);
        gdk_window_set_geometry_hints (handlebox->float_window, &geometry, GDK_HINT_POS);
        gdk_window_show (handlebox->float_window);
        handlebox->float_window_mapped = TRUE;
        #if	0
        /* this extra move is necessary if we use decorations, or our
         * window manager insists on decorations.
         */
        gdk_display_sync (gtk_widget_get_display (widget));
        gdk_window_move (handlebox->float_window, new_x, new_y);
        gdk_display_sync (gtk_widget_get_display (widget));
        #endif	/* 0 */
        g_signal_emit (handlebox,
                       handle_box_signals[SIGNAL_CHILD_DETACHED],
                       0,
                       GTK_BIN (handlebox)->child);
        geda_handle_box_draw_ghost (handlebox);

        gtk_widget_queue_resize (widget);
      }
    }

    return TRUE;
}

void geda_handle_box_dock (GedaHandleBox *handlebox) {
  if (GEDA_IS_HANDLE_BOX(handlebox)) {
    g_object_set (GTK_BIN (handlebox)->child, "orientation", handlebox->dock_orientation, NULL);
    geda_handle_box_reattach (handlebox);
  }
}

static void
geda_handle_box_add (GtkContainer *container, GtkWidget *widget)
{
  if (GEDA_IS_HANDLE_BOX(container)) {
   GedaHandleBox *handlebox = GEDA_HANDLE_BOX (container);
   gtk_widget_set_parent_window (widget, handlebox->bin_window);
   GTK_CONTAINER_CLASS (geda_handle_box_parent_class)->add (container, widget);
   g_object_get(widget, "orientation", &handlebox->dock_orientation, NULL);
  }
}

static void
geda_handle_box_remove (GtkContainer *container, GtkWidget *widget)
{
  GTK_CONTAINER_CLASS (geda_handle_box_parent_class)->remove (container, widget);
  geda_handle_box_reattach (GEDA_HANDLE_BOX (container));
}

static int
geda_handle_box_delete_event (GtkWidget *widget, GdkEventAny  *event)
{
  GedaHandleBox *handlebox = GEDA_HANDLE_BOX (widget);

  if (event->window == handlebox->float_window) {
      geda_handle_box_reattach (handlebox);
      return TRUE;
  }
  return FALSE;
}

static void
geda_handle_box_reattach (GedaHandleBox *handlebox)
{
  GtkWidget *widget = GTK_WIDGET (handlebox);

  if (handlebox->child_detached)
  {
    handlebox->child_detached = FALSE;
    if (gtk_widget_get_realized (widget))
    {
      gdk_window_hide (handlebox->float_window);
      gdk_window_reparent (handlebox->bin_window, widget->window, 0, 0);

      if (GTK_BIN (handlebox)->child)
        g_signal_emit (handlebox,
                       handle_box_signals[SIGNAL_CHILD_ATTACHED],
                       0,
                       GTK_BIN (handlebox)->child);

    }
    handlebox->float_window_mapped = FALSE;
  }
  if (handlebox->in_drag)
    geda_handle_box_end_drag (handlebox, GDK_CURRENT_TIME);

  gtk_widget_queue_resize (GTK_WIDGET (handlebox));
}

static void
geda_handle_box_end_drag (GedaHandleBox *handlebox, unsigned int time)
{
  GtkWidget *invisible = geda_handle_box_get_invisible ();

  handlebox->in_drag = FALSE;

  gtk_grab_remove (invisible);
  gdk_pointer_ungrab (time);
  g_signal_handlers_disconnect_by_func (invisible,
                                        G_CALLBACK (geda_handle_box_grab_event),
                                        handlebox);
}
/*! @} end group GedaHandleBox */