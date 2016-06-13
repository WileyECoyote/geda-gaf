

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include <glib.h>

#include <gtk/gtk.h>

#include "../../include/geda_widget.h"

void
geda_widget_buildable_finish_accelerator (GtkWidget *widget,
                                          GtkWidget *toplevel,
                                          void      *user_data)
{
  AccelGroupParserData *accel_data;
  GSList               *accel_groups;
  GtkAccelGroup        *accel_group;

  g_return_if_fail (GTK_IS_WIDGET (widget));
  g_return_if_fail (GTK_IS_WIDGET (toplevel));
  g_return_if_fail (user_data != NULL);

  accel_data   = (AccelGroupParserData*)user_data;
  accel_groups = gtk_accel_groups_from_object (G_OBJECT (toplevel));

  if (g_slist_length (accel_groups) == 0) {

      accel_group = gtk_accel_group_new ();
      gtk_window_add_accel_group (GTK_WINDOW (toplevel), accel_group);
  }
  else {

      g_assert (g_slist_length (accel_groups) == 1);
      accel_group = g_slist_nth_data (accel_groups, 0);
  }

  gtk_widget_add_accelerator (GTK_WIDGET (accel_data->object),
                              accel_data->signal,
                              accel_group,
                              accel_data->key,
                              accel_data->modifiers,
                              GTK_ACCEL_VISIBLE);

  g_object_unref (accel_data->object);
  g_free (accel_data->signal);
  g_slice_free (AccelGroupParserData, accel_data);
}

static GQuark quark_accel_path = 0;

const char*
geda_widget_get_accel_path (GtkWidget *widget, bool *locked)
{
  AccelPath *apath;

  g_return_val_if_fail (GTK_IS_WIDGET (widget), NULL);

  if (!quark_accel_path) {
    quark_accel_path = g_quark_from_static_string ("gtk-accel-path");
  }

  apath = g_object_get_qdata (G_OBJECT (widget), quark_accel_path);

  if (locked) {
    *locked = apath ? apath->accel_group->lock_count > 0 : TRUE;
  }

  return apath ? g_quark_to_string (apath->path_quark) : NULL;
}

/*! \brief Set the Pointer position relative to widget
 *  Contrast this with the ease of setting the pointer position on
 *  other platforms, this example is for Win32:
 *
 *       ClientToScreen(hWnd, &pt);
 *       SetCursorPos(pt.x,pt.y);
 */
#if GTK_CHECK_VERSION(3, 0, 0)
void
geda_widget_set_pointer_position(GtkWidget *widget, int x, int y)
{
  if (GTK_IS_WIDGET(widget)) {
    GdkDevice        *device;
    GdkDisplay       *display;
    GdkScreen        *screen;
    GdkWindow        *window;
    GdkDeviceManager *mgr;
    int x_root, y_root;

    display = gtk_widget_get_display (widget);
    screen  = gtk_widget_get_screen(s->drawing_area);
    window  = gtk_widget_get_window (widget);
    mgr     = gdk_display_get_device_manager (display);
    device  = gdk_device_manager_get_client_pointer(mgr);
    gdk_window_get_root_coords(window, x, y, &x_root, &y_root);
    gdk_device_warp(device, screen, x, y);
  }
}

#else /* Gtk-2 rendition */

void
geda_widget_set_pointer_position(GtkWidget *widget, int x, int y)
{
  if (GTK_IS_WIDGET(widget)) {

    GdkScreen    *screen;
    GdkDisplay   *display;
    int window_x, window_y;

    gdk_window_get_origin (widget->window, &window_x, &window_y);

    screen  = gtk_widget_get_screen (widget);
    display = gdk_screen_get_display (screen);

    gdk_display_warp_pointer (display, screen, window_x + x, window_y + y);
  }
}

#endif