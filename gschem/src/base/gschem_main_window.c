/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 Ales Hvezda
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA, <http://www.gnu.org/licenses/>.
 *
 * Contributing Author: Edward Hennessy
 * Date Contributed: October 13th, 2013
 */
/*!
 * \file gschem_main_window.c
 *
 * \brief The Main Window Widget
 */

#include <gschem.h>
#include <geda_keysyms.h>
#include <geda_debug.h>

/** \defgroup Gschem-Main-Window Gschem Main Window
 * @{
 * \brief #GschemMainWindow Class Implmentation
 * \par
 *  This module implements the main window in gschem.
 */

/* Function Prototypes */

static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec);

static void
gschem_main_window_class_init (void *class, void *data);

static void
gschem_main_window_instance_init (GTypeInstance *instance, void *class);

static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec);

static void *gschem_main_window_parent_class = NULL;

/*! \brief Get a property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
get_property (GObject *object, unsigned int param_id, GValue *value, GParamSpec *pspec)
{
  //GschemMainWindow *window = GSCHEM_MAIN_WINDOW (object);

  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

/*!
 * \brief GtkWidget map signal handler
 * \par Function Description
 *  Just before the main window is unmapped.
 *
 *  This typically happens when you call gtk_widget_destroy().
 *
 * \param [in] widget  The GtkWidget being unmapped.
 */
static void
gschem_main_window_map (GtkWidget *widget)
{
  gtk_widget_set_name (widget, "gschem");
  GTK_WIDGET_CLASS (gschem_main_window_parent_class)->map (widget);
}

/*! \brief GtkWidget unmap signal handler
 *
 *  \par Function Description
 *  Just before the main window is unmapped.
 *
 *  This typically happens when you call gtk_widget_destroy().
 *
 *  \param [in] widget  The GtkWidget being unmapped.
 */
static void
gschem_main_window_unmap (GtkWidget *widget)
{
  gtk_widget_set_name (widget, NULL);
  GTK_WIDGET_CLASS (gschem_main_window_parent_class)->unmap (widget);
}

static void
gschem_window_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  if (GTK_IS_WINDOW (widget)) {

    GtkBin      *bin;
    GtkWindow   *window;
    unsigned int border;

    window = (GtkWindow*)widget;
    bin    = (GtkBin*)window;
    border = gtk_container_get_border_width (GTK_CONTAINER (window));

    requisition->width = requisition->height = border << 1;

    if (bin->child && gtk_widget_get_visible (bin->child)) {

      GtkRequisition child_requisition;

      gtk_widget_size_request (bin->child, &child_requisition);

      requisition->width  += child_requisition.width;
      requisition->height += child_requisition.height;
    }
  }
}

static void
gschem_window_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GtkWindow    *window;
  GtkAllocation child_allocation;
  unsigned int  border_2x;
  unsigned int  need_resize;

  window             = GTK_WINDOW (widget);
  widget->allocation = *allocation;

  border_2x = gtk_container_get_border_width (GTK_CONTAINER (window)) << 1;

  if (window->bin.child && gtk_widget_get_visible (window->bin.child))
  {
    int border_width        = GTK_CONTAINER (window)->border_width;
    child_allocation.x      = border_width;
    child_allocation.y      = border_width;
    child_allocation.width  = MAX (1, (int)allocation->width - border_2x);
    child_allocation.height = MAX (1, (int)allocation->height - border_2x);

    gtk_widget_size_allocate (window->bin.child, &child_allocation);
  }

  need_resize = GTK_CONTAINER (window)->need_resize;

  if (need_resize && gtk_widget_get_realized (widget)) {

    GdkWindow *frame;
    int width;
    int height;

    frame  = geda_get_widget_window(widget);
    width  = allocation->width  = child_allocation.width + border_2x;
    height = allocation->height = child_allocation.height + border_2x;

    gdk_window_resize (frame, width, height);
  }
}

/*! \brief Initialize GschemMainWindow class
 *
 *  \param [in]  class       GschemMainWindow being initialized
 *  \param [in]  class_data  (do not use)
 */
static void
gschem_main_window_class_init (void *class, void *class_data)
{
  GObjectClass   *gobject_class   = G_OBJECT_CLASS (class);
  GtkWidgetClass *widget_class    = GTK_WIDGET_CLASS (class);

  gobject_class->get_property     = get_property;
  gobject_class->set_property     = set_property;

  widget_class->map               = gschem_main_window_map;
  widget_class->unmap             = gschem_main_window_unmap;
  widget_class->size_request      = gschem_window_size_request;
  widget_class->size_allocate     = gschem_window_size_allocate;

  gschem_main_window_parent_class = g_type_class_peek_parent (class);
}

/*! \brief Initialize GschemMainWindow instance -NOP
 *
 *  \param [in,out] instance GschemMainWindow being initialized.
 *  \param [in]     class    Class of the type the instance is created for.
 */
static void
gschem_main_window_instance_init (GTypeInstance *instance, void *class)
{
 /* GschemMainWindow *window = (GschemMainWindow*)class*/
}

/*! \brief Get/register GschemMainWindow type.
 */
GedaType gschem_main_window_get_type (void)
{
  static GedaType type = 0;

  if (type == 0) {

    static const GTypeInfo info = {
      sizeof(GschemMainWindowClass),
      NULL,                                      /* base_init */
      NULL,                                      /* base_finalize */
      gschem_main_window_class_init,             /* (GClassInitFunc) */
      NULL,                                      /* class_finalize */
      NULL,                                      /* class_data */
      sizeof(GschemMainWindow),
      0,                                         /* n_preallocs */
      gschem_main_window_instance_init,          /* (GInstanceInitFunc) */
    };

    type = g_type_register_static (GTK_TYPE_WINDOW, "GschemMainWindow", &info, 0);
  }

  return type;
}

/*! \brief Create a new instanceof the GschemMainWindow
 *
 *  \return A new instanceof the GschemMainWindow
 */
GschemMainWindow*
gschem_main_window_new ()
{
  return GSCHEM_MAIN_WINDOW (g_object_new (GSCHEM_TYPE_MAIN_WINDOW,
                                           "type", GTK_WINDOW_TOPLEVEL,
                                           NULL));
}

/*! \brief Set a property
 *
 *  \param [in,out] object
 *  \param [in]     param_id
 *  \param [in]     value
 *  \param [in]     pspec
 */
static void
set_property (GObject *object, unsigned int param_id, const GValue *value, GParamSpec *pspec)
{
  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

GdkWindow *gschem_main_window_get_window (GtkWidget *main_window)
{
  if (GSCHEM_IS_MAIN_WINDOW(main_window)) {
    return geda_get_widget_window (main_window);
  }

  return NULL;
}

#if GTK_MAJOR_VERSION < 3

GtkStyle *gschem_main_window_get_style (GtkWidget *main_window)
{
  if (GSCHEM_IS_MAIN_WINDOW(main_window)) {
    return main_window->style;
  }

  return NULL;
}

#endif

/*!
 * \brief Set the size of the GschemMainWindow widget
 * \par Function Description
 *  Provides functionality similar to gtk_window_resize except that
 *  gdk_display_sync is called to immediately process the request
 *  so that event signals are propagated in a timely manner.
 *
 * \param [in] main_window GschemMainWindow widget,
 * \param [in] width       New widget width.
 * \param [in] height      New widget height.
 */

void
gschem_main_window_set_size (GtkWidget *main_window, int width, int height)
{
  GdkDisplay *display;
  GdkWindow  *window;

  window = geda_get_widget_window(main_window);

  gdk_window_flush(window);

  gtk_window_resize(GTK_WINDOW(main_window), width, height);

  display = gdk_drawable_get_display (window);

  gdk_display_sync (display);
}

/** @} endgroup Gschem-Main-Window */
