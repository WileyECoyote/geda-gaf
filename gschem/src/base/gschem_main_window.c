/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 *
 * Copyright (C) 2013-2016 Ales Hvezda
 * Copyright (C) 2013-2016 gEDA Contributors (see ChangeLog for details)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
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

enum {
  GEOMETRY_SAVE,
  GEOMETRY_RESTORE,
  RESTORE_POSITION,
  LAST_SIGNAL
};

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

static unsigned int main_window_signals[LAST_SIGNAL] = { 0 };

/*!
 * \brief Get a property
 * \par Function Description
 * \param [in]     object
 * \param [in]     param_id
 * \param [in,out] value
 * \param [in]     pspec
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
 *  Just before the main window is mapped.
 *
 * \param [in] widget  The GtkWidget being unmapped.
 */
static void
gschem_main_window_map (GtkWidget *widget)
{
  gtk_widget_set_name (widget, "gschem");

  g_signal_emit (GSCHEM_MAIN_WINDOW (widget),
                 main_window_signals[ GEOMETRY_RESTORE ], 0);

  GTK_WIDGET_CLASS (gschem_main_window_parent_class)->map (widget);
}

/*!
 * \brief GtkWidget unmap signal handler
 * \par Function Description
 *  Just before the main window is unmapped.
 *
 *  This typically happens when you call gtk_widget_destroy().
 *
 * \param [in] widget  The GtkWidget being unmapped.
 */
static void
gschem_main_window_unmap (GtkWidget *widget)
{
  g_signal_emit (GSCHEM_MAIN_WINDOW (widget),
                 main_window_signals[ GEOMETRY_SAVE ], 0);

  gtk_widget_set_name (widget, NULL);
  GTK_WIDGET_CLASS (gschem_main_window_parent_class)->unmap (widget);
}

static void
gschem_window_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GtkBin           *bin;
  GtkWindow        *window;
  unsigned int      border;

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

static void
gschem_window_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GtkWindow     *window;
  GtkAllocation  child_allocation;
  unsigned int   border_2x;
  unsigned int   need_resize;

  window = (GtkWindow*)widget;

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

/*! \brief GtkWidget show signal handler
 *  \par Function Description
 *  Before the Dialog widget is shown, restore previously saved
 *  position and size. Order is import here; RESTORE_POSITION is
 *  emitted after the would-be size has been applied or Gtk might
 *  bound the window to the edge of the screen.
 *
 *  \param [in] widget  The GtkWidget being shown.
 */
static void gschem_main_show (GtkWidget *widget)
{
  gtk_window_set_resizable (GTK_WINDOW(widget), FALSE);

  /* Let Gtk show the window */
  GTK_WIDGET_CLASS (gschem_main_window_parent_class)->show (widget);

  gtk_window_set_resizable (GTK_WINDOW(widget), TRUE);

  g_signal_emit (GSCHEM_MAIN_WINDOW (widget),
                 main_window_signals[ RESTORE_POSITION ], 0);
}

/*! \brief GschemMainWindow "restore_position" class method handler
 *  \par Function Description
 *  Restore main window's last position.
 *
 *  \param [in] main_window The #GschemMainWindow to restore position.
 */
static void
gschem_main_window_restore_position (GschemMainWindow *main_window)
{
  EdaConfig  *cfg;
  GError     *err;
  GtkWindow  *window;
  const char *group;

  int  x, y;
  bool xy_error;

  cfg      = eda_config_get_user_context();
  err      = NULL;
  group    = WINDOW_CONFIG_GROUP;
  window   = GTK_WINDOW(main_window);
  xy_error = FALSE;

  geda_log_v(_("Retrieving main window position.\n"));

  x = eda_config_get_integer (cfg, group, "window-x-position", &err);
  if (err != NULL) {
    geda_utility_log_verbose("%s\n", err->message);
    g_clear_error (&err);
    xy_error = TRUE;
  }

  y = eda_config_get_integer (cfg, group, "window-y-position", &err);
  if (err != NULL) {
    g_clear_error (&err);
    xy_error = TRUE;
  }

  if (xy_error) {
    gtk_window_set_position(window, GTK_WIN_POS_CENTER);
  }
  else {
    gtk_window_move (window, x, y);
  }

#if DEBUG_MAIN_WINDOW
  fprintf(stderr, "%s x=%d, y=%d\n", __func__, x, y);
#endif

}

/*! \brief GschemMainWindow "geometry_restore" class method handler
 *  \par Function Description
 *  Restore main window's last size.
 *
 *  \param [in] main_window  The #GschemMainWindow to restore geometry.
 */
static void
gschem_main_window_geometry_restore (GschemMainWindow *main_window)
{
  EdaConfig  *cfg;
  GError     *err;
  const char *group;

  int  width, height;

  cfg      = eda_config_get_user_context();
  err      = NULL;
  group    = WINDOW_CONFIG_GROUP;

  geda_log_v(_("Retrieving main window geometry.\n"));

  width  = eda_config_get_integer (cfg, group, "window-width", &err);
  if (err != NULL) {
    g_clear_error (&err);
    width = DEFAULT_WINDOW_WIDTH;
  }

  height = eda_config_get_integer (cfg, group, "window-height", &err);
  if (err != NULL) {
    g_clear_error (&err);
    height = DEFAULT_WINDOW_HEIGHT;
  }

  /* If, for any reason, we pass a zero value to gtk_window_resize an error
   * will be generated. We double check these as fail safe because the above
   * conditionals only set default values if an error occurred retrieving
   * settings, so...*/
  if (width == 0 ) {
    width = DEFAULT_WINDOW_WIDTH;
  }
  if (height == 0) {
    height = DEFAULT_WINDOW_HEIGHT;
  }

  gschem_main_window_set_size((GtkWidget*)main_window, width, height);

#if DEBUG_MAIN_WINDOW
  fprintf(stderr, "%s width=%d, height=%d\n", __func__, width, height);
#endif
}

/*! \brief GschemMainWindow "geometry_save" class method handler
 *  \par Function Description
 *  Save the dialog's current position and size to the passed GKeyFile
 *
 *  \param [in] main_window The #GschemMainWindow Dialog to save the geometry.
 */
static void
gschem_main_window_geometry_save (GschemMainWindow *main_window)
{
  EdaConfig *cfg;
  char      *group;
  GtkWindow *window;
  int x, y, width, height;

  cfg    = eda_config_get_user_context ();
  group  = WINDOW_CONFIG_GROUP;
  window = GTK_WINDOW(main_window);

  gtk_window_get_position (window, &x, &y);
  gtk_window_get_size (window, &width, &height);

  /* Save the Window Geometry data */
  eda_config_set_integer (cfg, group, "window-x-position", x);
  eda_config_set_integer (cfg, group, "window-y-position", y);
  eda_config_set_integer (cfg, group, "window-width",      width);
  eda_config_set_integer (cfg, group, "window-height",     height);

#if DEBUG_MAIN_WINDOW
  fprintf(stderr, "%s x=%d, y=%d width=%d, height=%d\n", __func__, x, y, width, height);
#endif

}

/*!
 * \brief Initialize GschemMainWindow class
 * \par Function Description
 * \param [in]  class       GschemMainWindow being initialized
 * \param [in]  class_data  (do not use)
 */
static void
gschem_main_window_class_init (void *class, void *class_data)
{
  GObjectClass          *gobject_class = G_OBJECT_CLASS (class);
  GtkWidgetClass        *widget_class  = GTK_WIDGET_CLASS (class);
  GschemMainWindowClass *win_class     = (GschemMainWindowClass*)class;

  gobject_class->get_property     = get_property;
  gobject_class->set_property     = set_property;

  widget_class->map               = gschem_main_window_map;
  widget_class->unmap             = gschem_main_window_unmap;
  widget_class->show              = gschem_main_show;
  widget_class->size_request      = gschem_window_size_request;
  widget_class->size_allocate     = gschem_window_size_allocate;

  win_class->restore_position     = gschem_main_window_restore_position;
  win_class->geometry_restore     = gschem_main_window_geometry_restore;
  win_class->geometry_save        = gschem_main_window_geometry_save;

  gschem_main_window_parent_class = g_type_class_peek_parent (class);

  GedaType type = gschem_main_window_get_type();

  /*!
   * \brief GschemMainWindow::geometry-restore:
   * \par
   *  The GschemMainWindow::geometry-restore signal cause the dialog to restore
   *  the dialog size when the signal is emitted on the dialog.
   *
   * param [in] chooser the object which received the signal.
   */
  main_window_signals[ GEOMETRY_RESTORE ] = g_signal_new ("geometry-restore", type,
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GschemMainWindowClass,
                                                                       geometry_restore ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      g_cclosure_marshal_VOID__VOID,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);

  /*!
   * \brief GschemMainWindow::geometry-save:
   * \par
   *  The GschemMainWindow::geometry-save signal cause the dialog to save
   *  the dialog size and position with the signal is emitted on the dialog.
   *
   * param [in] chooser the object which received the signal.
   */
  main_window_signals[ GEOMETRY_SAVE ]    = g_signal_new ("geometry-save", type,
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GschemMainWindowClass,
                                                                       geometry_save ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      g_cclosure_marshal_VOID__VOID,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);

  /*!
   * \brief GschemMainWindow::restore-position:
   * \par
   *  The GschemMainWindow::restore-position signal cause the dialog to restore
   *  the dialog position when the signal is emitted on the dialog.
   *
   * param [in] chooser the object which received the signal.
   */
  main_window_signals[ RESTORE_POSITION ] = g_signal_new ("restore-position", type,
                                                      G_SIGNAL_RUN_FIRST,     /*signal_flags */
                                                      G_STRUCT_OFFSET (GschemMainWindowClass,
                                                                       restore_position ),
                                                      NULL, /* accumulator */
                                                      NULL, /* accu_data */
                                                      g_cclosure_marshal_VOID__VOID,
                                                      G_TYPE_NONE,
                                                      1,    /* n_params */
                                                      G_TYPE_STRING);
}

/*!
 * \brief Initialize GschemMainWindow instance -NOP
 * \par Function Description
 * \param [in,out] instance GschemMainWindow being initialized.
 * \param [in]     class    Class of the type the instance is created for.
 */
static void
gschem_main_window_instance_init (GTypeInstance *instance, void *class)
{
 /* GschemMainWindow *window = (GschemMainWindow*)class */
}

/*!
 * \brief Get/register GschemMainWindow type.
 * \par Function Description
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

/*!
 * \brief Create a new instanceof the GschemMainWindow
 * \par Function Description
 * \return A new instanceof the GschemMainWindow
 */
GschemMainWindow*
gschem_main_window_new ()
{
  return GSCHEM_MAIN_WINDOW (g_object_new (GSCHEM_TYPE_MAIN_WINDOW,
                                           "type", GTK_WINDOW_TOPLEVEL,
                                           NULL));
}

/*!
 * \brief Set a property
 * \par Function Description
 * \param [in,out] object
 * \param [in]     param_id
 * \param [in]     value
 * \param [in]     pspec
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
  GdkWindow *window;

  window = geda_get_widget_window(main_window);

  gtk_window_resize(GTK_WINDOW(main_window), width, height);

  gdk_window_process_updates (window, TRUE);

  gdk_display_sync (gdk_drawable_get_display (window));
}

/*!
 * \brief Update the GschemMainWindow widget
 * \par Function Description
 *  This function attempts to force the under-lining GtkWindow to
 *  actually call gtk_window_move_resize by emitting "check-resize"
 *  on the window so that info->resize_width & info->resize_height
 *  get applied to the allocation. Gtk normally delays making the
 *  changes and emitting configure until applications are idle to
 *  avoid performance degradation if "multiple" resizing occur.
 *  But "configure-event" is going to fire and the event and the
 *  allocation will NOT have the final geometry unless GtkWindow
 *  applies the values that were saved in the GtkWindowGeometryInfo
 *  structure. Note that gtk_window_get_size() will return the
 *  info->resize_width/height even before the values are applied.
 *
 * \param [in] main_window GschemMainWindow widget,
 *
 * \remark It is pointless to call this function before the window
 *         is shown, see gtk_window_check_resize().
 */
void
gschem_main_window_update (GtkWidget *main_window)
{
  GdkWindow *window;

  window = geda_get_widget_window(main_window);

  gdk_window_process_updates(window, TRUE);

  gdk_display_sync (gdk_drawable_get_display (window));

  g_signal_emit_by_name(main_window, "check-resize", window);

#if DEBUG_MAIN_WINDOW

  int x, y, width, height;

  gtk_window_get_position ((GtkWindow*)main_window, &x, &y);

  gtk_window_get_size ((GtkWindow*)main_window, &width, &height);

  fprintf(stderr, "%s exit x=%d, y=%d\n", __func__, x, y);

#endif

}

/** @} endgroup Gschem-Main-Window */
