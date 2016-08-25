

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
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

/*!
 * \brief Modify Widget Background Color
 * \par Function Description
 *  Calls geda_widget_modify_color to modify the
 *  background color attribute of the \a widget.
 *
 * \param[in,out] widget  Pointer to a Widget.
 * \param[in]     state  The state for which the attribute is to be set.
 * \param[in]     color  Pointer to GdkColor RGB color structure.
 *
 * \sa geda_widget_modify_fg geda_widget_modify_color
 */
void
geda_widget_modify_bg (GtkWidget      *widget,
                       GtkStateType    state,
                       const GdkColor *color)
{
  geda_widget_modify_color (GTK_WIDGET(widget),
                            GTK_RC_BG, state, color);
}

/*!
 * \brief GedaEntry Internal Modify Color Component
 * \par Function Description
 * This functions is called to modify different color attributes
 * as specified by the component flag.
 *
 * The GtkRcFlags are:
 *  <DL>
 *    <DT>GTK_RC_FG</DT>
 *    <DT>GTK_RC_BG</DT>
 *    <DT>GTK_RC_TEXT</DT>
 *    <DT>GTK_RC_BASE</DT>
 *  </DL>
 *
 * The GtkStateTypes are:
 *  <DL>
 *    <DT>GTK_STATE_NORMAL</DT>
 *    <DT>GTK_STATE_ACTIVE</DT>
 *    <DT>GTK_STATE_PRELIGHT</DT>
 *    <DT>GTK_STATE_SELECTED</DT>
 *    <DT>GTK_STATE_INSENSITIVE</DT>
 *  </DL>
 *
 * \param[in,out] widget     Pointer to widget being modifid.
 * \param[in]     component  The component that is being modified.
 * \param[in]     state      The state for which the attribute is to be set.
 * \param[in]     color      Pointer to GdkColor RGB color structure.
 */
void
geda_widget_modify_color_component (GtkWidget      *widget,
                                    GtkRcFlags      component,
                                    GtkStateType    state,
                                    const GdkColor *color)
{
  GtkRcStyle *rc_style;

  rc_style = gtk_widget_get_modifier_style (widget);

  if (color) {

    switch (component) {

      case GTK_RC_FG:
        rc_style->fg[state]   = *color;
        break;

      case GTK_RC_BG:
        rc_style->bg[state]   = *color;
        break;

      case GTK_RC_TEXT:
        rc_style->text[state] = *color;
        break;

      case GTK_RC_BASE:
        rc_style->base[state] = *color;
        break;

      default:
        BUG_IMSG ("unhandled case=%d", component);
    }

    rc_style->color_flags[state] |= component;
  }
  else
    rc_style->color_flags[state] &= ~component;

  gtk_widget_modify_style (widget, rc_style);
}

/*!
 * \brief Modify Widget Color Attributes
 * \par Function Description
 *  Validates \a widget and \a state and pass the request to
 *  geda_widget_modify_color_component.
 *
 * \param[in,out] widget     Pointer to a Widget.
 * \param[in]     component  The component that is being modified.
 * \param[in]     state      The state for which the attribute is to be set.
 * \param[in]     color      Pointer to GdkColor RGB color structure.
 *
 * \sa geda_widget_modify_color_component
 */
void
geda_widget_modify_color (GtkWidget      *widget,
                          GtkRcFlags      component,
                          GtkStateType    state,
                          const GdkColor *color)
{
  g_return_if_fail (GTK_IS_WIDGET (widget));

  if (state >= GTK_STATE_NORMAL || state <= GTK_STATE_INSENSITIVE) {
     state = GTK_STATE_NORMAL;
  }

  geda_widget_modify_color_component (widget, component, state, color);

}

/*!
 * \brief Modify Widget Foreground Color
 * \par Function Description
 *  Calls geda_widget_modify_color to modify the
 *  foreground attribute color of the \a widget.
 *
 * \param[in,out] widget Pointer to a widget.
 * \param[in]     state  The state for which the attribute is to be set.
 * \param[in]     color  Pointer to GdkColor RGB color structure.
 *
 * \sa geda_widget_modify_bg geda_widget_modify_color
 */
void
geda_widget_modify_fg (GtkWidget *widget,
                       GtkStateType state,
                       const GdkColor *color)
{
  geda_widget_modify_color (GTK_WIDGET(widget),
                            GTK_RC_FG, state, color);
}

void
geda_widget_modify_normal_bg (GtkWidget *widget, const GdkColor *color)
{
  geda_widget_modify_color (GTK_WIDGET(widget),
                            GTK_RC_BG, GTK_STATE_NORMAL, color);
}

void
geda_widget_modify_normal_fg (GtkWidget *widget, const GdkColor *color)
{
  geda_widget_modify_color (GTK_WIDGET(widget),
                            GTK_RC_FG, GTK_STATE_NORMAL, color);
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