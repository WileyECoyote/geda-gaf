/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License.
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
 */

/*! \file geda_menu.c
 *  \brief GedaMenu Class Module
 */

/** \defgroup geda-menu GedaMenu Object
 * @{
 * \brief Implmentation of #GedaCheckMenuItem Class
 * \par
 * A #GedaMenu is a #GedaMenuShell that implements a drop down menu
 * consisting of a list of #GedaMenuItem objects which can be navigated
 * and activated by the user to perform application functions.
 *
 * A #GedaMenu is most commonly dropped down by activating a
 * #GedaMenuItem in a #GedaMenuBar or popped up by activating a
 * #GedaMenuItem in another #GedaMenu.
 *
 * A #GedaMenu can also be popped up by activating a GedaComboBox.
 * Other composite widgets such as the GtkNotebook can pop up a
 * #GedaMenu as well.
 *
 * Applications can display a #GedaMenu as a popup menu by calling the
 * geda_menu_popup function. The example below shows how an application
 * can pop up a menu when the 3rd mouse button is pressed.
 *
 * ## Connecting the popup signal handler.
 *
 * \code{.c}
 *   /<!---->* connect our handler which will popup the menu *<!---->/
 *   g_signal_connect_swapped (window, "button-press-event",
 *                             G_CALLBACK (my_popup_handler), menu);
 * \endcode
 *
 * ## Signal handler which displays a popup menu.
 *
 * \code{.c}
 * static int
 * my_popup_handler (GtkWidget *widget, GdkEvent *event)
 * {
 *   GedaMenu       *menu;
 *   GdkEventButton *event_button;
 *
 *   /<!---->* The "widget" is the menu that was supplied when
 *    * g_signal_connect_swapped() was called.
 *    *<!---->/
 *   menu = GEDA_MENU (widget);
 *
 *   if (event->type == GDK_BUTTON_PRESS) {
 *
 *       event_button = (GdkEventButton *) event;
 *       if (event_button->button == GDK_BUTTON_SECONDARY) {
 *
 *           geda_menu_popup (menu, NULL, NULL, NULL, NULL,
 *                           event_button->button, event_button->time);
 *           return TRUE;
 *         }
 *     }
 *
 *   return FALSE;
 * }
 * \endcode
 *
 * \class GedaMenu geda_menu.h "include/geda_menu.h"
 * \implements GedaMenuShell
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_accel_label.h"
#include "../../include/geda_check_menu_item.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_keysyms.h"
#include "../../include/geda_label.h"
#include "../../include/geda_menu_enum.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_tearoff_menu_item.h"
#include "../../include/geda_uio_functions.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

#define DEFAULT_POPUP_DELAY    225
#define DEFAULT_POPDOWN_DELAY  1000

#define MAX_OFFSET_PADDING     250

/*! \def NAVIGATION_REGION_OVERSHOOT
 * How much the navigation region extends below the submenu */
#define NAVIGATION_REGION_OVERSHOOT 50

#define MENU_SCROLL_STEP1      8
#define MENU_SCROLL_STEP2     15
#define MENU_SCROLL_FAST_ZONE  8
#define MENU_SCROLL_TIMEOUT1  50
#define MENU_SCROLL_TIMEOUT2  20

typedef struct _MenuAttachData MenuAttachData;

struct _MenuAttachData
{
  GtkWidget *attach_widget;
  MenuDetachFunc detacher;
};

typedef struct
{
  int  left_attach;
  int  right_attach;
  int  top_attach;
  int  bottom_attach;
  int  effective_left_attach;
  int  effective_right_attach;
  int  effective_top_attach;
  int  effective_bottom_attach;
} AttachInfo;

struct _GedaMenuPriv
{
  int  x;
  int  y;
  bool  initially_pushed_in;

  unsigned int settings_signal_id;

  /* info used for the table */
  unsigned int *heights;
  int           heights_length;

  int  monitor_num;

  /* Cached layout information */
  int  n_rows;
  int  n_columns;

  char  *title;

  /* Arrow states */
  GtkStateType lower_arrow_state;
  GtkStateType upper_arrow_state;

  /* navigation region */
  int navigation_x;
  int navigation_y;
  int navigation_width;
  int navigation_height;

  unsigned int have_layout           : 1;
  unsigned int seen_item_enter       : 1;
  unsigned int have_position         : 1;
  unsigned int ignore_button_release : 1;
  unsigned int no_toggle_size        : 1;
  unsigned int touchscreen_mode      : 1;
};

enum {
  MOVE_SCROLL,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_ACTIVE,
  PROP_ACCEL_GROUP,
  PROP_ACCEL_PATH,
  PROP_ATTACH_WIDGET,
  PROP_TEAROFF_STATE,
  PROP_TEAROFF_TITLE,
  PROP_MONITOR,
  PROP_RESERVE_TOGGLE_SIZE
};

enum {
  CHILD_PROP_0,
  CHILD_PROP_LEFT_ATTACH,
  CHILD_PROP_RIGHT_ATTACH,
  CHILD_PROP_TOP_ATTACH,
  CHILD_PROP_BOTTOM_ATTACH
};

static void     geda_menu_set_property      (GObject          *object,
                                             unsigned int      prop_id,
                                             const GValue     *value,
                                             GParamSpec       *pspec);
static void     geda_menu_get_property      (GObject          *object,
                                             unsigned int      prop_id,
                                             GValue           *value,
                                             GParamSpec       *pspec);
static void     geda_menu_set_child_property(GtkContainer     *container,
                                             GtkWidget        *child,
                                             unsigned int      property_id,
                                             const GValue     *value,
                                             GParamSpec       *pspec);
static void     geda_menu_get_child_property(GtkContainer     *container,
                                             GtkWidget        *child,
                                             unsigned int      property_id,
                                             GValue           *value,
                                             GParamSpec       *pspec);
static void     geda_menu_destroy           (GtkObject        *object);
static void     geda_menu_realize           (GtkWidget        *widget);
static void     geda_menu_unrealize         (GtkWidget        *widget);
static void     geda_menu_size_request      (GtkWidget        *widget,
                                             GtkRequisition   *requisition);
static void     geda_menu_size_allocate     (GtkWidget        *widget,
                                             GtkAllocation    *allocation);
static void     geda_menu_paint             (GtkWidget        *widget,
                                             GdkEventExpose   *expose);
static void     geda_menu_show              (GtkWidget        *widget);
static bool     geda_menu_expose            (GtkWidget        *widget,
                                             GdkEventExpose   *event);
static bool     geda_menu_key_press         (GtkWidget        *widget,
                                             GdkEventKey      *event);
static bool     geda_menu_scroll            (GtkWidget        *widget,
                                             GdkEventScroll   *event);
static bool     geda_menu_button_press      (GtkWidget        *widget,
                                             GdkEventButton   *event);
static bool     geda_menu_button_release    (GtkWidget        *widget,
                                             GdkEventButton   *event);
static bool     geda_menu_motion_notify     (GtkWidget        *widget,
                                             GdkEventMotion   *event);
static bool     geda_menu_enter_notify      (GtkWidget        *widget,
                                             GdkEventCrossing *event);
static bool     geda_menu_leave_notify      (GtkWidget        *widget,
                                             GdkEventCrossing *event);
static void     geda_menu_scroll_to         (GedaMenu         *menu,
                                             int               offset);
static void     geda_menu_grab_notify       (GtkWidget        *widget,
                                             bool              was_grabbed);

static void     geda_menu_stop_scrolling         (GedaMenu  *menu);
static void     geda_menu_remove_scroll_timeout  (GedaMenu  *menu);
static bool     geda_menu_scroll_timeout         (void      *data);
static bool     geda_menu_scroll_timeout_initial (void      *data);
static void     geda_menu_start_scrolling        (GedaMenu  *menu);

static void     geda_menu_scroll_item_visible    (GedaMenuShell    *menu_shell,
                                                  GtkWidget        *menu_item);
static void     geda_menu_select_item            (GedaMenuShell    *menu_shell,
                                                  GtkWidget        *menu_item);
static void     geda_menu_real_insert            (GedaMenuShell    *menu_shell,
                                                  GtkWidget        *child,
                                                  int               position);
static void     geda_menu_scrollbar_changed      (GtkAdjustment    *adjustment,
                                                  GedaMenu         *menu);
static void     geda_menu_handle_scrolling       (GedaMenu         *menu,
                                                  int               event_x,
                                                  int               event_y,
                                                  bool              enter,
                                                  bool              motion);
static void     geda_menu_set_tearoff_hints      (GedaMenu         *menu,
                                                  int               width);
static void     geda_menu_style_set              (GtkWidget        *widget,
                                                  GtkStyle         *previous_style);
static bool     geda_menu_focus                  (GtkWidget        *widget,
                                                  GtkDirectionType  direction);
static int      geda_menu_get_popup_delay        (GedaMenuShell    *menu_shell);
static void     geda_menu_move_current           (GedaMenuShell    *menu_shell,
                                                  MenuDirection     direction);
static void     geda_menu_real_move_scroll       (GedaMenu         *menu,
                                                  GtkScrollType     type);

static void     geda_menu_stop_navigating_submenu       (GedaMenu          *menu);
static bool     geda_menu_stop_navigating_submenu_cb    (void              *user_data);
static bool     geda_menu_navigating_submenu            (GedaMenu          *menu,
                                                         int                event_x,
                                                         int                event_y);
static void     geda_menu_set_submenu_navigation_region (GedaMenu          *menu,
                                                         GedaMenuItem      *menu_item,
                                                         GdkEventCrossing  *event);

static void     geda_menu_deactivate     (GedaMenuShell     *menu_shell);
static void     geda_menu_show_all       (GtkWidget         *widget);
static void     geda_menu_hide_all       (GtkWidget         *widget);
static void     geda_menu_position       (GedaMenu          *menu,
                                          bool               set_scroll_offset);
static void     geda_menu_reparent       (GedaMenu          *menu,
                                          GtkWidget         *new_parent,
                                          bool               unrealize);
static void     geda_menu_remove         (GtkContainer      *menu,
                                          GtkWidget         *widget);

static void     geda_menu_update_title   (GedaMenu          *menu);

static void       menu_grab_transfer_window_destroy (GedaMenu *menu);
static GdkWindow *menu_grab_transfer_window_get     (GedaMenu *menu);

static bool  geda_menu_real_can_activate_accel      (GtkWidget     *widget,
                                                     unsigned int   signal_id);
static void geda_menu_refresh_accel_paths           (GedaMenu      *menu,
                                                     bool           group_changed);

static const char attached_menus_key[]  = "attached-menus";
static const char attached_info_key[]   = "menu-child-attach-info-key";
static const char attached_data_key[]   = "menu-attach-data";
static const char explicit_screen_key[] = "menu-explicit-screen";
static const char transfer_window_key[] = "menu-transfer-window";

static const char touchscreen_setting[] = "gtk-touchscreen-mode";

static unsigned int menu_signals[LAST_SIGNAL] = { 0 };

/* Table of pointers to GedaMenu instances */
static GHashTable *menu_hash_table = NULL;

static void *geda_menu_parent_class = NULL;

static void change_touchscreen_mode (GedaMenu *menu)
{
  GedaMenuPriv *priv;
  GtkSettings  *settings;
  unsigned int  touchscreen_mode;

  touchscreen_mode = 0;

  priv = menu->priv;

  settings = gtk_widget_get_settings ((GtkWidget*)menu);

  g_object_get (settings, touchscreen_setting, &touchscreen_mode, NULL);

  priv->touchscreen_mode = touchscreen_mode;

}

/* Callback used when a GtkSettings value changes */
static void settings_notify_cb (GObject *object, GParamSpec *pspec, GedaMenu *menu)
{
  const char *name;

  name = g_param_spec_get_name (pspec);

  /* Check if touchscreen-mode is what was changed */
  if (!strcmp (name, touchscreen_setting)) {
    change_touchscreen_mode (menu);
  }
}

static void connect_settings_signal(GedaMenu *menu)
{
  GedaMenuPriv *priv = menu->priv;

  if (!priv->settings_signal_id) {

    GtkSettings *settings;
    GdkScreen   *screen;

    screen   = gtk_widget_get_screen ((GtkWidget*)menu);
    settings = gtk_settings_get_for_screen (screen);

    priv->settings_signal_id = g_signal_connect (settings, "notify",
                                                 G_CALLBACK(settings_notify_cb),
                                                 menu);
  }
}

/* Removes the settings signal handler. It's safe to call multiple times */
static void remove_settings_signal (GedaMenu *menu, GdkScreen *screen)
{
  GedaMenuPriv *priv = menu->priv;

  if (priv->settings_signal_id) {

    GtkSettings *settings;

    settings = gtk_settings_get_for_screen (screen);

    g_signal_handler_disconnect (settings, priv->settings_signal_id);

    priv->settings_signal_id = 0;
  }
}

static void menu_queue_resize (GedaMenu *menu)
{
  GedaMenuPriv *priv = menu->priv;

  priv->have_layout = FALSE;
  gtk_widget_queue_resize ((GtkWidget*)menu);
}

static AttachInfo *get_attach_info (GtkWidget *child)
{
  AttachInfo *info = GEDA_OBJECT_GET_DATA (child, attached_info_key);

  if (!info) {

    info = GEDA_MEM_ALLOC0 (sizeof(AttachInfo));
    g_object_set_data_full ((GObject*)child, attached_info_key, info,
                            (GDestroyNotify) g_free);
  }

  return info;
}

static bool is_grid_attached (AttachInfo *info)
{
  return (info->left_attach   >= 0 &&
          info->right_attach  >= 0 &&
          info->top_attach    >= 0 &&
          info->bottom_attach >= 0);
}

static void menu_ensure_layout (GedaMenu *menu)
{
  GedaMenuPriv *priv = menu->priv;

  if (!priv->have_layout) {

    GedaMenuShell *menu_shell = (GedaMenuShell*)menu;
    GList         *iter;
    char          *row_occupied;

    int  current_row;
    int  max_right_attach;
    int  max_bottom_attach;

    /* Find extents of gridded portion */
    max_right_attach  = 1;
    max_bottom_attach = 0;

    for (iter = menu_shell->children; iter; iter = iter->next) {

      GtkWidget  *child = iter->data;
      AttachInfo *info  = get_attach_info (child);

      if (is_grid_attached (info)) {
        max_bottom_attach = MAX (max_bottom_attach, info->bottom_attach);
        max_right_attach  = MAX (max_right_attach, info->right_attach);
      }
    }

    /* Find empty rows
     */
    row_occupied = GEDA_MEM_ALLOC0 (max_bottom_attach);

    for (iter = menu_shell->children; iter; iter = iter->next) {

      GtkWidget  *child = iter->data;
      AttachInfo *info  = get_attach_info (child);

      if (is_grid_attached (info)) {

        int  i;

        for (i = info->top_attach; i < info->bottom_attach; i++)
          row_occupied[i] = TRUE;
      }
    }

    /* Lay non-grid-items out in those rows */

    current_row = 0;

    for (iter = menu_shell->children; iter; iter = iter->next) {

      GtkWidget  *child = iter->data;
      AttachInfo *info  = get_attach_info (child);

      if (!is_grid_attached (info)) {

        while (current_row < max_bottom_attach && row_occupied[current_row])
          current_row++;

        info->effective_left_attach   = 0;
        info->effective_right_attach  = max_right_attach;
        info->effective_top_attach    = current_row;
        info->effective_bottom_attach = current_row + 1;

        current_row++;
      }
      else {

        info->effective_left_attach   = info->left_attach;
        info->effective_right_attach  = info->right_attach;
        info->effective_top_attach    = info->top_attach;
        info->effective_bottom_attach = info->bottom_attach;
      }
    }

    g_free (row_occupied);

    priv->n_rows      = MAX (current_row, max_bottom_attach);
    priv->n_columns   = max_right_attach;
    priv->have_layout = TRUE;
  }
}

static inline int geda_menu_get_n_columns (GedaMenu *menu)
{
  menu_ensure_layout (menu);

  return menu->priv->n_columns;
}

static inline int geda_menu_get_n_rows (GedaMenu *menu)
{
  menu_ensure_layout (menu);

  return menu->priv->n_rows;
}

/*! \internal called by:
 * compute_child_offset
 * geda_menu_size_allocate
 * geda_menu_size_request
 * find_child_containing
 * geda_menu_move_current
 */
static void get_effective_child_attach (GtkWidget *child,
                                        int       *l,
                                        int       *r,
                                        int       *t,
                                        int       *b)
{
  GedaMenu   *menu = (GedaMenu*)gtk_widget_get_parent(child);
  AttachInfo *info;

  menu_ensure_layout (menu);

  info = get_attach_info (child);

  if (l) {
    *l = info->effective_left_attach;
  }

  if (r) {
    *r = info->effective_right_attach;
  }

  if (t) {
    *t = info->effective_top_attach;
  }

  if (b) {
    *b = info->effective_bottom_attach;
  }
}

static bool geda_menu_window_event (GtkWidget *window,
                                    GdkEvent  *event,
                                    GtkWidget *menu)
{
  bool handled = FALSE;

  g_object_ref (window);
  g_object_ref (menu);

  switch (event->type) {

    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      handled = gtk_widget_event (menu, event);
      break;

    case GDK_WINDOW_STATE:
      /* Window for the menu has been closed by the display server or by GDK.
       * Update the internal state as if the user had clicked outside the
       * menu
       */
      if (event->window_state.new_window_state & GDK_WINDOW_STATE_WITHDRAWN &&
          event->window_state.changed_mask & GDK_WINDOW_STATE_WITHDRAWN)
      {
        geda_menu_shell_deactivate ((GedaMenuShell*)menu);
      }
      break;

    default:
      break;
  }

  g_object_unref (window);
  g_object_unref (menu);

  return handled;
}

static void geda_menu_window_size_request (GtkWidget      *window,
                                           GtkRequisition *requisition,
                                           GedaMenu       *menu)
{
  GedaMenuPriv *private = menu->priv;

  if (private->have_position) {

    GdkScreen *screen = gtk_widget_get_screen (window);
    GdkRectangle monitor;

    gdk_screen_get_monitor_geometry (screen, private->monitor_num, &monitor);

    if (private->y + requisition->height > monitor.y + monitor.height) {
      requisition->height = monitor.y + monitor.height - private->y;
    }

    if (private->y < monitor.y) {
      requisition->height -= monitor.y - private->y;
    }
  }
}

static void get_arrows_sensitive_area (GedaMenu     *menu,
                                       GdkRectangle *upper,
                                       GdkRectangle *lower)
{
  GtkWidget   *widget = (GtkWidget*)menu;
  GdkWindow   *window;
  GtkStyle    *style;
  unsigned int vertical_padding;

  int  border, bwidth;
  int  width, height;
  int  win_x, win_y;
  int  scroll_arrow_height;

  GtkArrowPlacement arrow_placement;

  window =  geda_get_widget_window(widget);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

  width  = gdk_window_get_width (window);
  height = gdk_window_get_height (window);

#else

  gdk_drawable_get_size(window, &width, &height);

#endif

  gtk_widget_style_get (widget,
                        "vertical-padding",     &vertical_padding,
                        "scroll-arrow-vlength", &scroll_arrow_height,
                        "arrow-placement",      &arrow_placement,
                        NULL);

  bwidth = geda_get_container_border_width (menu);
  style  = geda_get_widget_style (widget);
  border = bwidth + style->ythickness + vertical_padding;

  gdk_window_get_position (window, &win_x, &win_y);

  switch (arrow_placement) {
    case GTK_ARROWS_BOTH:
      if (upper) {
        upper->x = win_x;
        upper->y = win_y;
        upper->width = width;
        upper->height = scroll_arrow_height + border;
      }

      if (lower) {
        lower->x = win_x;
        lower->y = win_y + height - border - scroll_arrow_height;
        lower->width = width;
        lower->height = scroll_arrow_height + border;
      }
      break;

    case GTK_ARROWS_START:
      if (upper) {
        upper->x      = win_x;
        upper->y      = win_y;
        upper->width  = width / 2;
        upper->height = scroll_arrow_height + border;
      }

      if (lower) {
        lower->x      = win_x + width / 2;
        lower->y      = win_y;
        lower->width  = width / 2;
        lower->height = scroll_arrow_height + border;
      }
      break;

    case GTK_ARROWS_END:
      if (upper) {
        upper->x      = win_x;
        upper->y      = win_y + height - border - scroll_arrow_height;
        upper->width  = width / 2;
        upper->height = scroll_arrow_height + border;
      }

      if (lower) {
        lower->x      = win_x + width / 2;
        lower->y      = win_y + height - border - scroll_arrow_height;
        lower->width  = width / 2;
        lower->height = scroll_arrow_height + border;
      }
      break;
  }
}

static void geda_menu_handle_scrolling (GedaMenu *menu,
                                        int       x,
                                        int       y,
                                        bool      enter,
                                        bool      motion)
{
  GedaMenuShell *menu_shell;
  GedaMenuPriv  *priv;
  GdkWindow     *window;
  GdkRectangle   rect;

  bool  in_arrow;
  bool  scroll_fast = FALSE;
  bool  touchscreen_mode;
  int   top_x, top_y;

  priv = menu->priv;

  menu_shell = (GedaMenuShell*)menu;

  touchscreen_mode = priv->touchscreen_mode;

  window = geda_get_widget_window (menu->toplevel);

  gdk_window_get_position (window, &top_x, &top_y);

  x -= top_x;
  y -= top_y;

  /*  upper arrow handling  */

  get_arrows_sensitive_area (menu, &rect, NULL);

  in_arrow = FALSE;
  if (menu->upper_arrow_visible && !menu->tearoff_active &&
    (x >= rect.x) && (x < rect.x + rect.width) &&
    (y >= rect.y) && (y < rect.y + rect.height))
  {
    in_arrow = TRUE;
  }

  if (touchscreen_mode) {
    menu->upper_arrow_prelight = in_arrow;
  }

  if (priv->upper_arrow_state != GTK_STATE_INSENSITIVE) {

    bool  arrow_pressed = FALSE;

    if (menu->upper_arrow_visible && !menu->tearoff_active) {

      if (touchscreen_mode) {

        if (enter && menu->upper_arrow_prelight) {

          if (menu->timeout_id == 0) {

            /* Deselect the active item so that
             * any submenus are popped down
             */
            geda_menu_shell_deselect (menu_shell);

            geda_menu_remove_scroll_timeout (menu);
            menu->scroll_step = -MENU_SCROLL_STEP2; /* always fast */

            if (!motion) {

              /* Only do stuff on click. */
              geda_menu_start_scrolling (menu);
              arrow_pressed = TRUE;
            }
          }
          else {
            arrow_pressed = TRUE;
          }
        }
        else if (!enter) {
          geda_menu_stop_scrolling (menu);
        }
      }
      else  { /* !touchscreen_mode */

        scroll_fast = (y < rect.y + MENU_SCROLL_FAST_ZONE);

        if (enter && in_arrow &&
           (!menu->upper_arrow_prelight || menu->scroll_fast != scroll_fast))
        {
          menu->upper_arrow_prelight = TRUE;
          menu->scroll_fast = scroll_fast;

          /* Deselect the active item so any submenus are popped down */
          geda_menu_shell_deselect (menu_shell);

          geda_menu_remove_scroll_timeout (menu);

          menu->scroll_step = scroll_fast ? -MENU_SCROLL_STEP2 : -MENU_SCROLL_STEP1;

          menu->timeout_id =
          gdk_threads_add_timeout (scroll_fast ?
                                   MENU_SCROLL_TIMEOUT2 : MENU_SCROLL_TIMEOUT1,
                                   geda_menu_scroll_timeout, menu);
        }
        else if (!enter && !in_arrow && menu->upper_arrow_prelight) {
          geda_menu_stop_scrolling (menu);
        }
      }
    }

    /*  geda_menu_start_scrolling() might have hit the top of the
     *  menu, so check if the button is not insensitive before
     *  changing it to something else.
     */
    if (priv->upper_arrow_state != GTK_STATE_INSENSITIVE) {

      GtkStateType arrow_state = GTK_STATE_NORMAL;

      if (arrow_pressed)
        arrow_state = GTK_STATE_ACTIVE;
      else if (menu->upper_arrow_prelight)
        arrow_state = GTK_STATE_PRELIGHT;

      if (arrow_state != priv->upper_arrow_state) {

        priv->upper_arrow_state = arrow_state;

        window = geda_get_widget_window(menu);

        gdk_window_invalidate_rect (window, &rect, FALSE);
      }
    }
  }

  /*  lower arrow handling  */

  get_arrows_sensitive_area (menu, NULL, &rect);

  in_arrow = FALSE;
  if (menu->lower_arrow_visible && !menu->tearoff_active &&
    (x >= rect.x) && (x < rect.x + rect.width) &&
    (y >= rect.y) && (y < rect.y + rect.height))
  {
    in_arrow = TRUE;
  }

  if (touchscreen_mode) {
    menu->lower_arrow_prelight = in_arrow;
  }

  if (priv->lower_arrow_state != GTK_STATE_INSENSITIVE) {

    bool  arrow_pressed = FALSE;

    if (menu->lower_arrow_visible && !menu->tearoff_active) {

      if (touchscreen_mode) {

        if (enter && menu->lower_arrow_prelight) {

          if (menu->timeout_id == 0) {

            /* Deselect the active item so that
             * any submenus are popped down
             */
            geda_menu_shell_deselect (menu_shell);

            geda_menu_remove_scroll_timeout (menu);
            menu->scroll_step = MENU_SCROLL_STEP2; /* always fast */

            if (!motion) {

              /* Only do stuff on click. */
              geda_menu_start_scrolling (menu);
              arrow_pressed = TRUE;
            }
          }
          else {
            arrow_pressed = TRUE;
          }
        }
        else if (!enter) {
          geda_menu_stop_scrolling (menu);
        }
      }
      else  { /* !touchscreen_mode */

        scroll_fast = (y > rect.y + rect.height - MENU_SCROLL_FAST_ZONE);

        if (enter && in_arrow &&
           (!menu->lower_arrow_prelight || menu->scroll_fast != scroll_fast))
        {
          menu->lower_arrow_prelight = TRUE;
          menu->scroll_fast = scroll_fast;

          /* Deselect the active item so that
           * any submenus are popped down
           */
          geda_menu_shell_deselect (menu_shell);

          geda_menu_remove_scroll_timeout (menu);
          menu->scroll_step = scroll_fast ?
                              MENU_SCROLL_STEP2 : MENU_SCROLL_STEP1;

          menu->timeout_id =
          gdk_threads_add_timeout (scroll_fast ?
                                   MENU_SCROLL_TIMEOUT2 : MENU_SCROLL_TIMEOUT1,
                                   geda_menu_scroll_timeout, menu);
        }
        else if (!enter && !in_arrow && menu->lower_arrow_prelight) {

          geda_menu_stop_scrolling (menu);
        }
      }
    }

    /*  geda_menu_start_scrolling() might have hit the bottom of the
     *  menu, so check if the button is not insensitive before
     *  changing it to something else.
     */
    if (priv->lower_arrow_state != GTK_STATE_INSENSITIVE) {

      GtkStateType arrow_state = GTK_STATE_NORMAL;

      if (arrow_pressed) {
        arrow_state = GTK_STATE_ACTIVE;
      }
      else if (menu->lower_arrow_prelight) {
        arrow_state = GTK_STATE_PRELIGHT;
      }

      if (arrow_state != priv->lower_arrow_state) {

        priv->lower_arrow_state = arrow_state;

        window = geda_get_widget_window(menu);

        gdk_window_invalidate_rect (window, &rect, FALSE);
      }
    }
  }
}

static bool pointer_in_menu_window (GtkWidget *widget, double x_root, double y_root)
{
  GedaMenu *menu = (GedaMenu*)widget;

  if (gtk_widget_get_mapped (menu->toplevel)) {

    GedaMenuShell *menu_shell;
    GdkWindow     *window;
    GtkAllocation *allocation;
    int            window_x, window_y;

    window = geda_get_widget_window(menu->toplevel);

    gdk_window_get_position (window, &window_x, &window_y);

    allocation = geda_get_widget_allocation (widget);

    if (x_root >= window_x && x_root < window_x + allocation->width &&
        y_root >= window_y && y_root < window_y + allocation->height)
      return TRUE;

    menu_shell = (GedaMenuShell*)widget;

    if (GEDA_IS_MENU (menu_shell->parent_menu_shell))
      return pointer_in_menu_window (menu_shell->parent_menu_shell,
                                     x_root, y_root);
  }

  return FALSE;
}

static bool geda_menu_button_scroll (GedaMenu *menu, GdkEventButton *event)
{
  if (menu->upper_arrow_prelight || menu->lower_arrow_prelight) {

    if (menu->priv->touchscreen_mode) {

      geda_menu_handle_scrolling (menu,
                                  event->x_root, event->y_root,
                                  event->type == GDK_BUTTON_PRESS,
                                  FALSE);
    }

    return TRUE;
  }

  return FALSE;
}

/*! \internal widget_class->button_press_event */
static bool geda_menu_button_press (GtkWidget *widget, GdkEventButton *event)
{
  GedaMenu *menu;

  if (event->type != GDK_BUTTON_PRESS) {
    return FALSE;
  }

  menu = (GedaMenu*)widget;

  /* Don't pass down to menu shell for presses over scroll arrows */
  if (geda_menu_button_scroll (menu, event))
    return TRUE;

  /*  Don't pass down to menu shell if a non-menuitem part of the menu
   *  was clicked. The check for the event_widget being a GedaMenuShell
   *  works because we have the pointer grabbed on menu_shell->window
   *  with owner_events=TRUE, so all events that are either outside the
   *  menu or on its border are delivered relative to
   *  menu_shell->window.
   */
  if (GEDA_IS_MENU_SHELL (gtk_get_event_widget ((GdkEvent*)event)) &&
      pointer_in_menu_window (widget, event->x_root, event->y_root))
    return TRUE;

  /* Do not chain to parent GedaMenuShell class or else geda_menu_shell_button_press
   * get called twice, and worst; on Windows system the Menu will stay open/dropped
   * down until we loose focus or user "clicks" the menu closed.
   */
  if (!menu->torn_off)
    return FALSE;

  /* Menu torn so chain to parent GedaMenuShell class */
  return ((GtkWidgetClass*)geda_menu_parent_class)->button_press_event (widget, event);
}

/*! \internal widget_class->button_release_event */
static bool geda_menu_button_release (GtkWidget *widget, GdkEventButton *event)
{
  GedaMenuPriv *priv = ((GedaMenu*)widget)->priv;

  if (priv->ignore_button_release) {

    priv->ignore_button_release = FALSE;
    return FALSE;
  }

  if (event->type != GDK_BUTTON_RELEASE) {
    return FALSE;
  }

  /* Don't pass down to menu shell for releases over scroll arrows
   */
  if (geda_menu_button_scroll ((GedaMenu*)widget, event))
    return TRUE;

  /*  Don't pass down to menu shell if a non-menuitem part of the menu
   *  was clicked (see comment in button_press()).
   */
  if (GEDA_IS_MENU_SHELL (gtk_get_event_widget ((GdkEvent*)event)) &&
    pointer_in_menu_window (widget, event->x_root, event->y_root))
  {
    /*  Ugly: make sure menu_shell->button gets reset to 0 when we
     *  bail out early here so it is in a consistent state for the
     *  next button_press/button_release in GedaMenuShell.
     *  See bug #449371.
     */
    if (((GedaMenuShell*)widget)->active) {
       ((GedaMenuShell*)widget)->button = 0;
    }

    return TRUE;
  }

  return ((GtkWidgetClass*)geda_menu_parent_class)->button_release_event (widget, event);
}

/*! \internal widget_class->can_activate_accel */
static bool geda_menu_real_can_activate_accel (GtkWidget   *widget,
                                               unsigned int signal_id)
{
  /* Menu items chain here to figure whether they can activate their
   * accelerators.  Unlike ordinary widgets, menus allow accel
   * activation even if invisible since that's the usual case for
   * submenus/popup-menus. however, the state of the attach widget
   * affects the "activeness" of the menu.
   */
  GtkWidget *awidget = geda_menu_get_attach_widget ((GedaMenu*)widget);

  if (awidget)
    return gtk_widget_can_activate_accel (awidget, signal_id);
  else
    return gtk_widget_is_sensitive (widget);
}

#define THRESHOLD 6
/* helper for
 *  widget_class->enter_notify_event
 *  widget_class->motion_notify
 */
static bool definitely_within_item (GtkWidget *widget, int x, int y)
{
  GdkWindow *window;
  int w, h;

  window = geda_menu_item_get_event_window((GedaMenuItem*)widget);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

  w = gdk_window_get_width (window);
  h = gdk_window_get_height (window);

#else

  gdk_drawable_get_size(window, &w, &h);

#endif

  return (x + THRESHOLD > 0 && x < w) && (y + THRESHOLD > 0 && y < h);
}

/*! \internal widget_class->enter_notify_event */
static bool geda_menu_enter_notify (GtkWidget *widget, GdkEventCrossing *event)
{
  GtkWidget *menu_item;
  bool       touchscreen_mode;

  if (event->mode == GDK_CROSSING_GTK_GRAB   ||
      event->mode == GDK_CROSSING_GTK_UNGRAB ||
      event->mode == GDK_CROSSING_STATE_CHANGED)
    return TRUE;

  g_object_get (gtk_widget_get_settings (widget),
                touchscreen_setting, &touchscreen_mode,
                NULL);

  menu_item = gtk_get_event_widget ((GdkEvent*)event);

  if (GEDA_IS_MENU (widget)) {

    GedaMenuShell *menu_shell = (GedaMenuShell*)widget;

    if (!menu_shell->ignore_enter) {
      geda_menu_handle_scrolling ((GedaMenu*)widget,
                                  event->x_root, event->y_root, TRUE, TRUE);
    }
  }

  if (GEDA_IS_MENU_ITEM (menu_item)) {

    GedaMenu *menu;

    menu = geda_get_widget_parent (menu_item);

    if (!touchscreen_mode) {

      if (GEDA_IS_MENU (menu)) {

        GedaMenuPriv  *priv       = menu->priv;
        GedaMenuShell *menu_shell = (GedaMenuShell*)menu;

        if (priv->seen_item_enter) {

          /* This is the second enter we see for an item on this
           * menu. This means a release should always activate.
           */
          menu_shell->activate_time = 0;
        }
        else if ((event->detail != GDK_NOTIFY_NONLINEAR &&
          event->detail != GDK_NOTIFY_NONLINEAR_VIRTUAL))
        {
          if (definitely_within_item (menu_item, event->x, event->y)) {

            /* This is an actual user-enter,  not a pop-under. In this
             * case, the user must either have entered sufficiently far
             * enough into the item, or have move far enough away from
             * the enter point, see geda_menu_motion_notify.
             */
            menu_shell->activate_time = 0;
          }
        }

        priv->seen_item_enter = TRUE;
      }
    }

    /* If this is a faked enter (see geda_menu_motion_notify), 'widget'
     * will not correspond to the event widget's parent.  Check to see
     * if we are in the parent's navigation region.
     */
    if (GEDA_IS_MENU (menu) &&
        geda_menu_navigating_submenu (menu, event->x_root, event->y_root))
    {
      return TRUE;
    }
  }

  return ((GtkWidgetClass*)geda_menu_parent_class)->enter_notify_event (widget, event);
}

/* geda_menu_paint helper */
static inline void get_arrows_visible_area (GedaMenu     *menu,
                                            GdkRectangle *border,
                                            GdkRectangle *upper,
                                            GdkRectangle *lower,
                                            int          *arrow_space)
{
  GtkStyle         *style;
  GtkWidget        *widget;
  GdkWindow        *window;
  unsigned int      vertical_padding;
  unsigned int      horizontal_padding;
  int               scroll_arrow_height;
  int               border_width;
  GtkArrowPlacement arrow_placement;

  widget = (GtkWidget*)menu;

  gtk_widget_style_get (widget,
                        "vertical-padding",     &vertical_padding,
                        "horizontal-padding",   &horizontal_padding,
                        "scroll-arrow-vlength", &scroll_arrow_height,
                        "arrow-placement",      &arrow_placement,
                        NULL);

  border_width = geda_get_container_border_width (widget);
  style        = geda_get_widget_style  (widget);

  border->x = border_width + style->xthickness + horizontal_padding;
  border->y = border_width + style->ythickness + vertical_padding;

  window = geda_get_widget_window(menu);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

  border->width  = gdk_window_get_width (window);
  border->height = gdk_window_get_height (window);

#else

  gdk_drawable_get_size(window, &border->width, &border->height);

#endif

  int borderx2 = border->x << 1;

  switch (arrow_placement) {

    case GTK_ARROWS_BOTH:
      upper->x      = border->x;
      upper->y      = border->y;
      upper->width  = border->width - borderx2;
      upper->height = scroll_arrow_height;

      lower->x      = border->x;
      lower->y      = border->height - border->y - scroll_arrow_height;
      lower->width  = border->width - borderx2;
      lower->height = scroll_arrow_height;
      break;

    case GTK_ARROWS_START:
      upper->x      = border->x;
      upper->y      = border->y;
      upper->width  = (border->width - borderx2) >> 1;
      upper->height = scroll_arrow_height;

      lower->x      = border->x + upper->width;
      lower->y      = border->y;
      lower->width  = (border->width - borderx2) >> 1;
      lower->height = scroll_arrow_height;
      break;

    case GTK_ARROWS_END:
      upper->x      = border->x;
      upper->y      = border->height - border->y - scroll_arrow_height;
      upper->width  = (border->width - borderx2) >> 1;
      upper->height = scroll_arrow_height;

      lower->x      = border->x + upper->width;
      lower->y      = border->height - border->y - scroll_arrow_height;
      lower->width  = (border->width - borderx2) >> 1;
      lower->height = scroll_arrow_height;
      break;

    default:
       fprintf(stderr, "%s: unexpected case <%d>\n",__func__, arrow_placement);
       upper->x = upper->y = upper->width = upper->height = 0;
       lower->x = lower->y = lower->width = lower->height = 0;
  }

  *arrow_space = scroll_arrow_height - (style->ythickness << 1);
}

static void get_arrows_border (GedaMenu *menu, GtkBorder *border)
{
  unsigned int scroll_arrow_height;
  GtkArrowPlacement arrow_placement;

  gtk_widget_style_get ((GtkWidget*)menu,
                        "scroll-arrow-vlength", &scroll_arrow_height,
                        "arrow_placement", &arrow_placement,
                         NULL);

  switch (arrow_placement) {

    case GTK_ARROWS_BOTH:
      border->top = menu->upper_arrow_visible ? scroll_arrow_height : 0;
      border->bottom = menu->lower_arrow_visible ? scroll_arrow_height : 0;
      break;

    case GTK_ARROWS_START:
      border->top = (menu->upper_arrow_visible ||
                     menu->lower_arrow_visible) ? scroll_arrow_height : 0;
      border->bottom = 0;
      break;

    case GTK_ARROWS_END:
      border->top = 0;
      border->bottom = (menu->upper_arrow_visible ||
                        menu->lower_arrow_visible) ? scroll_arrow_height : 0;
      break;
  }

  border->left = border->right = 0;
}

static void geda_menu_paint (GtkWidget *widget, GdkEventExpose *event)
{
  GedaMenu     *menu;
  GedaMenuPriv *priv;
  GdkWindow    *window;
  GtkStyle     *style;
  GdkRectangle  border;
  GdkRectangle  upper;
  GdkRectangle  lower;
  int           arrow_space;

  menu = (GedaMenu*)widget;
  priv = menu->priv;

  get_arrows_visible_area (menu, &border, &upper, &lower, &arrow_space);

  style  = geda_get_widget_style(widget);
  window = geda_get_widget_window(widget);

  if (event->window == window) {

    float arrow_scaling;
    int   arrow_size;

    gtk_widget_style_get (widget, "arrow-scaling", &arrow_scaling, NULL);

    arrow_size = arrow_scaling * arrow_space;

    gtk_paint_box (style,
                   window,
                   GTK_STATE_NORMAL,
                   GTK_SHADOW_OUT,
                   &event->area, widget, "menu",
                   0, 0, -1, -1);

    if (menu->upper_arrow_visible && !menu->tearoff_active) {

      gtk_paint_box (style,
                     window,
                     priv->upper_arrow_state,
                     GTK_SHADOW_OUT,
                     &event->area, widget, "menu_scroll_arrow_up",
                     upper.x,
                     upper.y,
                     upper.width,
                     upper.height);

      gtk_paint_arrow (style,
                       window,
                       priv->upper_arrow_state,
                       GTK_SHADOW_OUT,
                       &event->area, widget, "menu_scroll_arrow_up",
                       GTK_ARROW_UP,
                       TRUE,
                       upper.x + ((upper.width - arrow_size) >> 1),
                       upper.y + style->ythickness + ((arrow_space - arrow_size) >> 1),
                       arrow_size, arrow_size);
    }

    if (menu->lower_arrow_visible && !menu->tearoff_active) {

      gtk_paint_box (style,
                     window,
                     priv->lower_arrow_state,
                     GTK_SHADOW_OUT,
                     &event->area, widget, "menu_scroll_arrow_down",
                     lower.x,
                     lower.y,
                     lower.width,
                     lower.height);

      gtk_paint_arrow (style,
                       window,
                       priv->lower_arrow_state,
                       GTK_SHADOW_OUT,
                       &event->area, widget, "menu_scroll_arrow_down",
                       GTK_ARROW_DOWN,
                       TRUE,
                       lower.x + ((lower.width - arrow_size) >> 1),
                       lower.y + style->ythickness + ((arrow_space - arrow_size) >> 1),
                       arrow_size, arrow_size);
    }
  }
  else if (event->window == menu->bin_window) {

    int  y = -border.y + menu->scroll_offset;

    if (!menu->tearoff_active) {

      GtkBorder arrow_border;

      get_arrows_border (menu, &arrow_border);
      y -= arrow_border.top;
    }

    gtk_paint_box (style,
                   menu->bin_window,
                   GTK_STATE_NORMAL,
                   GTK_SHADOW_OUT,
                   &event->area, widget, "menu",
                   - border.x, y,
                   border.width, border.height);
  }
}

/*! \internal widget_class->expose */
static bool geda_menu_expose (GtkWidget *widget, GdkEventExpose *event)
{
  if (gtk_widget_is_drawable (widget)) {

    geda_menu_paint (widget, event);

    ((GtkWidgetClass*)geda_menu_parent_class)->expose_event (widget, event);
  }

  return FALSE;
}

/*! \internal widget_class->focus */
static bool
geda_menu_focus (GtkWidget *widget, GtkDirectionType direction)
{
  /* A menu and menu items cannot have focus */
  return FALSE;
}

/*! \internal widget_class->grab_notify */
static void
geda_menu_grab_notify (GtkWidget *widget, bool was_grabbed)
{
  GtkWidget      *toplevel;
  GtkWindowGroup *group;
  GtkWidget      *grab;

  toplevel = gtk_widget_get_toplevel (widget);
  group    = gtk_window_get_group (GTK_WINDOW (toplevel));
  grab     = gtk_window_group_get_current_grab (group);

  if (!was_grabbed) {

    if (GEDA_MENU_SHELL (widget)->active && !GEDA_IS_MENU_SHELL (grab))
      geda_menu_shell_cancel ((GedaMenuShell*)widget);
  }
}

static void
geda_menu_hide_children (GtkWidget *widget)
{
  if (!GTK_IS_CONTAINER(widget)) {
    gtk_widget_hide (widget);
  }
  else {
    geda_container_foreach (widget, geda_menu_hide_children, NULL);
  }
}

/*! \internal widget_class->hide_all */
static void
geda_menu_hide_all (GtkWidget *widget)
{
  /* Hide children, but not self. */
  geda_container_foreach (widget, geda_menu_hide_children, NULL);
}

/*! \internal widget_class->key_press_event */
static bool geda_menu_key_press (GtkWidget *widget, GdkEventKey *event)
{
  geda_menu_stop_navigating_submenu ((GedaMenu*)widget);

  return ((GtkWidgetClass*)geda_menu_parent_class)->key_press_event (widget, event);
}

/*! \internal widget_class->leave_notify */
static bool geda_menu_leave_notify (GtkWidget *widget, GdkEventCrossing *event)
{
  GedaMenu  *menu;
  GtkWidget *event_widget;

  if (event->mode == GDK_CROSSING_GTK_GRAB ||
      event->mode == GDK_CROSSING_GTK_UNGRAB ||
      event->mode == GDK_CROSSING_STATE_CHANGED) {
    return TRUE;
  }

  menu = (GedaMenu*)widget;

  if (geda_menu_navigating_submenu (menu, event->x_root, event->y_root)) {
    return TRUE;
  }

  geda_menu_handle_scrolling (menu, event->x_root, event->y_root, FALSE, TRUE);

  event_widget = gtk_get_event_widget ((GdkEvent*)event);

  if (GEDA_IS_MENU_ITEM (event_widget)) {

    GedaMenuItem  *menu_item  = (GedaMenuItem*)event_widget;
    GedaMenuShell *menu_shell = (GedaMenuShell*)widget;

    /* Check to see if we're leaving an active menu item with a submenu,
     * in which case we enter submenu navigation mode.
     */
    if (menu_shell->active_menu_item != NULL) {

      GtkWidget *submenu;

      submenu = geda_menu_item_get_submenu_widget (menu_item);

      if (submenu &&
        geda_menu_item_get_submenu_placement(menu_item) == MENU_LEFT_RIGHT)
      {
        if (GEDA_MENU_SHELL (submenu)->active) {

          geda_menu_set_submenu_navigation_region (menu, menu_item, event);

          return TRUE;
        }
        else if (menu_item == (GedaMenuItem*)menu_shell->active_menu_item)
        {
          /* We are leaving an active menu item with nonactive submenu.
           * Deselect it so we don't surprise the user with by popping
           * up a submenu _after_ he left the item.
           */
          geda_menu_shell_deselect (menu_shell);
          return TRUE;
        }
      }
    }

    return ((GtkWidgetClass*)geda_menu_parent_class)->leave_notify_event (widget, event);
  }

  return TRUE;
}

static bool geda_menu_has_navigation_triangle (GedaMenu *menu)
{
  GedaMenuPriv *priv;

  priv = menu->priv;

  return priv->navigation_height && priv->navigation_width;
}

/*! \internal widget_class->motion_notify */
static bool geda_menu_motion_notify (GtkWidget *widget, GdkEventMotion *event)
{
  GtkWidget     *menu_item;
  GedaMenu      *menu;
  GedaMenuShell *menu_shell;
  bool           need_enter;

  if (GEDA_IS_MENU (widget)) {

      GedaMenuPriv *priv = GEDA_MENU (widget)->priv;

      if (priv->ignore_button_release)
        priv->ignore_button_release = FALSE;

      geda_menu_handle_scrolling (GEDA_MENU (widget), event->x_root,
                                                      event->y_root,
                                                      TRUE, TRUE);
  }

  /* We received the event for one of two reasons:
   *
   * a) We are the active menu, and did gtk_grab_add()
   * b) The widget is a child of ours, and the event was propagated
   *
   * Since for computation of navigation regions, we want the menu which
   * is the parent of the menu item, for a), we need to find that menu,
   * which may be different from 'widget'.
   */
  menu_item = gtk_get_event_widget ((GdkEvent*) event);

  if (!GEDA_IS_MENU_ITEM (menu_item)) {
    return FALSE;
  }

  menu_shell = geda_get_widget_parent(menu_item);
  menu       = (GedaMenu*)menu_shell;

  if (!GEDA_IS_MENU (menu)) {
    return FALSE;
  }

  if (definitely_within_item (menu_item, event->x, event->y)) {
    menu_shell->activate_time = 0;
  }

  /* Check to see if we are within an active submenu's navigation region
   */
  if (geda_menu_navigating_submenu (menu, event->x_root, event->y_root)) {
    return TRUE;
  }

  /* Make sure we pop down if we enter a non-selectable menu item, so we
   * don't show a submenu when the cursor is outside the stay-up triangle.
   */
  if (!geda_menu_item_is_widget_selectable (menu_item)) {

      /* We really want to deselect, but this gives the menushell code
       * a chance to do some bookkeeping about the menuitem.
       */
      geda_menu_shell_select_item (menu_shell, menu_item);
      return FALSE;
  }

  need_enter = (geda_menu_has_navigation_triangle (menu) || menu_shell->ignore_enter);

  if (need_enter) {

    /* The menu is now sensitive to enter events on its items, but
     * was previously sensitive.  So we fake an enter event.
     */
    int  width, height;

    menu_shell->ignore_enter = FALSE;

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

    width  = gdk_window_get_width (event->window);
    height = gdk_window_get_height (event->window);

#else

    gdk_drawable_get_size(event->window, &width, &height);

#endif

    if (event->x >= 0 && event->x < width &&
        event->y >= 0 && event->y < height)
    {
      GdkEvent *send_event = gdk_event_new (GDK_ENTER_NOTIFY);
      bool      result;

      send_event->crossing.window     = g_object_ref (event->window);
      send_event->crossing.time       = event->time;
      send_event->crossing.send_event = TRUE;
      send_event->crossing.x_root     = event->x_root;
      send_event->crossing.y_root     = event->y_root;
      send_event->crossing.x          = event->x;
      send_event->crossing.y          = event->y;
      send_event->crossing.state      = event->state;

      /* We send the event to 'widget', the currently active menu,
       * instead of 'menu', the menu that the pointer is in. This
       * will ensure that the event will be ignored unless the
       * menuitem is a child of the active menu or some parent
       * menu of the active menu.
       */
      result = gtk_widget_event (widget, send_event);
      gdk_event_free (send_event);

      return result;
    }
  }

  return FALSE;
}

/*! \internal called by:
 * geda_menu_scroll_item_visible
 * geda_menu_scroll_by
 * geda_menu_scroll_to
 */
static bool get_double_arrows (GedaMenu *menu)
{
  GedaMenuPriv     *priv = menu->priv;
  bool              double_arrows;
  GtkArrowPlacement arrow_placement;

  gtk_widget_style_get (GTK_WIDGET (menu),
                        "double-arrows", &double_arrows,
                        "arrow-placement", &arrow_placement,
                        NULL);

  if (arrow_placement != GTK_ARROWS_BOTH) {
    return TRUE;
  }

  return double_arrows || (priv->initially_pushed_in &&
                           menu->scroll_offset != 0);
}

/*! \internal called by:
 * geda_menu_scroll_item_visible
 * geda_menu_real_move_scroll
 */
static bool compute_child_offset (GedaMenu  *menu,
                                  GtkWidget *menu_item,
                                  int       *offset,
                                  int       *height,
                                  bool      *is_last_child)
{
  GedaMenuPriv *priv = menu->priv;
  int  item_top_attach;
  int  item_bottom_attach;
  int  child_offset = 0;
  int  i;

  get_effective_child_attach (menu_item, NULL, NULL,
                              &item_top_attach, &item_bottom_attach);

  /* there is a possibility that we get called before _size_request, so
   * check the height table for safety.
   */
  if (!priv->heights || priv->heights_length < geda_menu_get_n_rows (menu))
  {
    return FALSE;
  }

  /* when we have a row with only invisible children, it's height will
   * be zero, so there's no need to check WIDGET_VISIBLE here
   */
  for (i = 0; i < item_top_attach; i++) {
    child_offset += priv->heights[i];
  }

  if (is_last_child) {
    *is_last_child = (item_bottom_attach == geda_menu_get_n_rows (menu));
  }

  if (offset) {
    *offset = child_offset;
  }

  if (height) {
    *height = priv->heights[item_top_attach];
  }

  return TRUE;
}

/*! \internal called by:
 * widget_class->realize
 * shell_class->geda_menu_select_item
 */
static void geda_menu_scroll_item_visible (GedaMenuShell *menu_shell,
                                           GtkWidget     *menu_item)
{
  GedaMenu *menu;
  int       child_offset, child_height;
  bool      last_child = 0;

  menu = (GedaMenu*)menu_shell;

  /* We need to check if the selected item is fully visible. If not
   * we need to scroll the menu so that it becomes fully visible.
   */

  if (compute_child_offset (menu, menu_item,
                           &child_offset, &child_height, &last_child))
  {
    GdkWindow   *window;
    unsigned int vertical_padding;
    bool         double_arrows;
    int          border_width;
    int          height;
    int          scroll_offset;
    int          ythickness;

    scroll_offset = menu->scroll_offset;

    window =  geda_get_widget_window(menu);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

    height = gdk_window_get_height (window);

#else
    int width;
    gdk_drawable_get_size(window, &width, &height);

#endif

    gtk_widget_style_get ((GtkWidget*)menu,
                          "vertical-padding", &vertical_padding, NULL);

    double_arrows = get_double_arrows (menu);

    border_width = geda_get_container_border_width (menu);
    ythickness   = geda_get_widget_style (menu)->ythickness;

    height -= (border_width + ythickness + vertical_padding) << 1;

    if (child_offset < scroll_offset) {

      /* Ignore the enter event we might get if the pointer is on the menu */
      menu_shell->ignore_enter = TRUE;
      geda_menu_scroll_to (menu, child_offset);
    }
    else {

      GtkBorder arrow_border;
      int       arrow_height;

      arrow_height = 0;

      get_arrows_border (menu, &arrow_border);

      if (!menu->tearoff_active) {
        arrow_height = arrow_border.top + arrow_border.bottom;
      }

      if (child_offset + child_height > scroll_offset + height - arrow_height)
      {
        int new_offset;

        arrow_height = 0;

        if ((!last_child && !menu->tearoff_active) || double_arrows) {
          arrow_height += arrow_border.bottom;
        }

        new_offset = child_offset + child_height - height + arrow_height;

        if (((new_offset > 0) && !menu->tearoff_active) || double_arrows) {

          /* Need upper arrow */
          arrow_height += arrow_border.top;
          new_offset = child_offset + child_height - height + arrow_height;
        }
        /* Ignore the enter event we might get if the pointer is on the menu
         */
        menu_shell->ignore_enter = TRUE;
        geda_menu_scroll_to (menu, new_offset);
      }
    }
  }
}

/*! \internal widget_class->realize */
static void geda_menu_realize (GtkWidget *widget)
{
  GdkWindowAttr  attributes;
  GdkWindow     *window;
  GedaMenu      *menu;
  GList         *children;
  GtkAllocation *allocation;
  unsigned int   attributes_mask;
  unsigned       border_width;
  unsigned int   vertical_padding;
  unsigned int   horizontal_padding;

  GtkBorder arrow_border;

  gtk_widget_set_realized (widget, TRUE);

  allocation = geda_get_widget_allocation (widget);

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x           = allocation->x;
  attributes.y           = allocation->y;
  attributes.width       = allocation->width;
  attributes.height      = allocation->height;
  attributes.wclass      = GDK_INPUT_OUTPUT;
  attributes.visual      = gtk_widget_get_visual (widget);
  attributes.colormap    = gtk_widget_get_colormap (widget);
  attributes.event_mask  = gtk_widget_get_events (widget);

  attributes.event_mask |= (GDK_EXPOSURE_MASK | GDK_KEY_PRESS_MASK |
                            GDK_ENTER_NOTIFY_MASK | GDK_LEAVE_NOTIFY_MASK);

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
  window          = gtk_widget_get_parent_window (widget);
  window          = gdk_window_new (window, &attributes, attributes_mask);

  geda_set_widget_window (widget, window);

  gdk_window_set_user_data (window, widget);

  gtk_widget_style_get (widget,
                        "vertical-padding", &vertical_padding,
                        "horizontal-padding", &horizontal_padding,
                        NULL);

  GtkStyle *style;

  border_width = geda_get_container_border_width(widget);
  style        = geda_get_widget_style(widget);

  attributes.x = border_width + style->xthickness + horizontal_padding;
  attributes.y = border_width + style->ythickness + vertical_padding;

  attributes.width  = MAX (1, allocation->width - attributes.x * 2);
  attributes.height = MAX (1, allocation->height - attributes.y * 2);

  menu = (GedaMenu*)widget;

  get_arrows_border (menu, &arrow_border);

  attributes.y      += arrow_border.top;
  attributes.height -= arrow_border.top;
  attributes.height -= arrow_border.bottom;

  menu->view_window = gdk_window_new (window, &attributes, attributes_mask);
  gdk_window_set_user_data (menu->view_window, menu);

  attributes.x = 0;
  attributes.y = 0;

  GtkRequisition *requisition;

  requisition = geda_get_widget_requisition(widget);

  attributes.width  = MAX (1, allocation->width - (border_width + style->xthickness + horizontal_padding) * 2);
  attributes.height = MAX (1, requisition->height - (border_width + style->ythickness + vertical_padding) * 2);

  menu->bin_window = gdk_window_new (menu->view_window, &attributes, attributes_mask);
  gdk_window_set_user_data (menu->bin_window, menu);

  children = ((GedaMenuShell*)menu)->children;

  while (children) {

      GtkWidget *child;

      child    = children->data;
      children = children->next;

      gtk_widget_set_parent_window (child, menu->bin_window);
  }

  style = gtk_style_attach (style, window);

  geda_set_widget_style (widget, style);

  gtk_style_set_background (style, menu->bin_window,  GTK_STATE_NORMAL);
  gtk_style_set_background (style, menu->view_window, GTK_STATE_NORMAL);
  gtk_style_set_background (style, window,            GTK_STATE_NORMAL);

  if (((GedaMenuShell*)widget)->active_menu_item) {
    geda_menu_scroll_item_visible ((GedaMenuShell*)widget,
                                  ((GedaMenuShell*)widget)->active_menu_item);
  }

  gdk_window_show (menu->bin_window);
  gdk_window_show (menu->view_window);

  connect_settings_signal(menu);
}

static void geda_menu_scroll_by (GedaMenu *menu, int step)
{
  GdkWindow      *window;
  GtkWidget      *widget;
  GtkRequisition *requisition;
  GtkBorder       arrow_border;
  bool            double_arrows;
  int             offset;
  int             view_height;

  widget = GTK_WIDGET (menu);
  offset = menu->scroll_offset + step;

  get_arrows_border (menu, &arrow_border);

  double_arrows = get_double_arrows (menu);

  /* If we scroll upward and the non-visible top part is smaller than
   * the scroll arrow it would be pretty stupid to show the arrow and
   * taking more screen space than just scrolling to the top.
   */
  if (!double_arrows) {
    if ((step < 0) && (offset < arrow_border.top)) {
      offset = 0;
    }
  }

  /* Don't scroll over the top if we weren't before: */
  if ((menu->scroll_offset >= 0) && (offset < 0)) {
    offset = 0;
  }

  window = geda_get_widget_window (widget);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

  view_height = gdk_window_get_height (window);

#else

  int width;
  gdk_drawable_get_size(window, &width, &view_height);

#endif

  requisition = geda_get_widget_requisition(widget);

  if (menu->scroll_offset == 0 && view_height >= requisition->height)
    return;

  /* Don't scroll past the bottom if we weren't before: */
  if (menu->scroll_offset > 0)
    view_height -= arrow_border.top;

  /* When both arrows are always shown, reduce
   * view height even more.
   */
  if (double_arrows)
    view_height -= arrow_border.bottom;

  if ((menu->scroll_offset + view_height <= requisition->height) &&
      (offset + view_height > requisition->height)) {
    offset = requisition->height - view_height;
  }

  if (offset != menu->scroll_offset) {
    geda_menu_scroll_to (menu, offset);
  }
}

/*! \internal widget_class->scroll */
static bool geda_menu_scroll (GtkWidget *widget, GdkEventScroll *event)
{
  GedaMenu *menu = (GedaMenu*)widget;

  switch (event->direction) {

    case GDK_SCROLL_RIGHT:
    case GDK_SCROLL_DOWN:
      geda_menu_scroll_by (menu, MENU_SCROLL_STEP2);
      break;

    case GDK_SCROLL_LEFT:
    case GDK_SCROLL_UP:
      geda_menu_scroll_by (menu, - MENU_SCROLL_STEP2);
      break;
  }

  return TRUE;
}

/*! \internal widget_class->show */
static void geda_menu_show (GtkWidget *widget)
{
  GedaMenu *menu = GEDA_MENU (widget);

  geda_menu_refresh_accel_paths (menu, FALSE);

  ((GtkWidgetClass*)geda_menu_parent_class)->show (widget);
}

/*! \internal widget_class->show_all */
static void geda_menu_show_all (GtkWidget *widget)
{
  /* Show children, but not self. */
  geda_container_foreach (widget, gtk_widget_show_all, NULL);
}

/*! \internal widget_class->size_allocate */
static void geda_menu_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GedaMenu      *menu;
  GedaMenuShell *menu_shell;
  GList         *children;
  GedaMenuPriv  *priv;
  GtkAllocation  child_allocation;
  GtkRequisition child_requisition;

  int  x, y, x2, y2;
  int  width, height;

  unsigned int vertical_padding;
  unsigned int horizontal_padding;

  menu       = (GedaMenu*)widget;
  menu_shell = (GedaMenuShell*)widget;
  priv       = menu->priv;

  widget->allocation = *allocation;
  gtk_widget_get_child_requisition (widget, &child_requisition);

  gtk_widget_style_get (widget,
                        "vertical-padding", &vertical_padding,
                        "horizontal-padding", &horizontal_padding,
                        NULL);

  x  = ((GtkContainer*)menu)->border_width + widget->style->xthickness + horizontal_padding;
  y  = ((GtkContainer*)menu)->border_width + widget->style->ythickness + vertical_padding;

  x2 = x << 1;
  y2 = y << 1;

  width = MAX (1, allocation->width - x2);
  height = MAX (1, allocation->height - y2);

  child_requisition.width  -= x2;
  child_requisition.height -= y2;

  if (menu_shell->active) {
    geda_menu_scroll_to (menu, menu->scroll_offset);
  }

  if (!menu->tearoff_active) {

    GtkBorder arrow_border;

    get_arrows_border (menu, &arrow_border);
    y += arrow_border.top;
    height -= arrow_border.top;
    height -= arrow_border.bottom;
  }

  if (gtk_widget_get_realized (widget)) {

    gdk_window_move_resize (widget->window,
                            allocation->x, allocation->y,
                            allocation->width, allocation->height);

    gdk_window_move_resize (menu->view_window, x, y, width, height);
  }

  if (menu_shell->children) {

    int base_width;

    base_width = width / geda_menu_get_n_columns (menu);
    children   = menu_shell->children;

    while (children) {

      GtkWidget *child;

      child = children->data;
      children = children->next;

      if (gtk_widget_get_visible (child)) {

        int  i;
        int  l, r, t, b;

        get_effective_child_attach (child, &l, &r, &t, &b);

        if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL)
        {
          unsigned int tmp;
          tmp = geda_menu_get_n_columns (menu) - l;
          l = geda_menu_get_n_columns (menu) - r;
          r = tmp;
        }

        child_allocation.width = (r - l) * base_width;
        child_allocation.height = 0;
        child_allocation.x = l * base_width;
        child_allocation.y = 0;

        for (i = 0; i < b; i++) {

          if (i < t)
            child_allocation.y += priv->heights[i];
          else
            child_allocation.height += priv->heights[i];
        }

        geda_menu_item_toggle_size_allocate ((GedaMenuItem*)child,
                                             menu->toggle_size);

        gtk_widget_size_allocate (child, &child_allocation);
        gtk_widget_queue_draw (child);
      }
    }

    /* Resize the item window */
    if (gtk_widget_get_realized (widget)) {

      int  i;
      int  width, height;

      height = 0;
      for (i = 0; i < geda_menu_get_n_rows (menu); i++) {
        height += priv->heights[i];
      }

      width = geda_menu_get_n_columns (menu) * base_width;
      gdk_window_resize (menu->bin_window, width, height);
    }

    if (menu->tearoff_active) {

      if (allocation->height >= widget->requisition.height) {

        if (gtk_widget_get_visible (menu->tearoff_scrollbar)) {

          gtk_widget_hide (menu->tearoff_scrollbar);
          geda_menu_set_tearoff_hints (menu, allocation->width);

          geda_menu_scroll_to (menu, 0);
        }
      }
      else {

        menu->tearoff_adjustment->upper = widget->requisition.height;
        menu->tearoff_adjustment->page_size = allocation->height;

        if (menu->tearoff_adjustment->value + menu->tearoff_adjustment->page_size >
            menu->tearoff_adjustment->upper)
        {
          int value;

          value = menu->tearoff_adjustment->upper - menu->tearoff_adjustment->page_size;

          if (value < 0) {
            value = 0;
          }
          geda_menu_scroll_to (menu, value);
        }

        gtk_adjustment_changed (menu->tearoff_adjustment);

        if (!gtk_widget_get_visible (menu->tearoff_scrollbar)) {

          gtk_widget_show (menu->tearoff_scrollbar);
          geda_menu_set_tearoff_hints (menu, allocation->width);
        }
      }
    }
  }
}

/*! \internal widget_class->size_request */
static void geda_menu_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  int  i;
  GedaMenu      *menu;
  GedaMenuShell *menu_shell;
  GList         *children;
  GedaMenuPriv  *priv;

  unsigned int   max_toggle_size;
  unsigned int   max_accel_width;
  unsigned int   vertical_padding;
  unsigned int   horizontal_padding;
  GtkRequisition child_requisition;

  g_return_if_fail (GEDA_IS_MENU (widget));
  g_return_if_fail (requisition != NULL);

  menu       = (GedaMenu*)widget;
  menu_shell = (GedaMenuShell*)widget;
  priv       = menu->priv;

  requisition->width  = 0;
  requisition->height = 0;

  max_toggle_size = 0;
  max_accel_width = 0;

  g_free (priv->heights);

  priv->heights = GEDA_MEM_ALLOC0 (sizeof(unsigned int) * geda_menu_get_n_rows (menu));
  priv->heights_length = geda_menu_get_n_rows (menu);

  children = menu_shell->children;

  while (children) {

      GtkWidget *child;
      int  part;
      int  toggle_size;
      int  l, r, t, b;
      unsigned short accelerator_width;

      child = children->data;
      children = children->next;

      if (!gtk_widget_get_visible (child)) {
        continue;
      }

      get_effective_child_attach (child, &l, &r, &t, &b);

      /* It's important to size_request the child
       * before doing the toggle size request, in
       * case the toggle size request depends on the size
       * request of a child of the child (e.g. for ImageMenuItem)
       */
       geda_menu_item_set_show_submenu_indicator(GEDA_MENU_ITEM (child), TRUE);
       gtk_widget_size_request (child, &child_requisition);

       geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child), &toggle_size);
       max_toggle_size = MAX (max_toggle_size, toggle_size);
       accelerator_width = geda_menu_item_get_accel_width(GEDA_MENU_ITEM (child));
       max_accel_width = MAX (max_accel_width, accelerator_width);

       part               = child_requisition.width / (r - l);
       requisition->width = MAX (requisition->width, part);

       part             = MAX (child_requisition.height, toggle_size) / (b - t);
       priv->heights[t] = MAX (priv->heights[t], part);
    }

  /* If the menu doesn't include any images or check items then
   * reserve the space so that all menus are consistent. We only
   * do this for 'ordinary' menus, not for combobox  menus or
   * multi-column menus.
   */
  if (max_toggle_size == 0 &&
      geda_menu_get_n_columns (menu) == 1 &&
      !priv->no_toggle_size)
    {
      unsigned int toggle_spacing;
      unsigned int indicator_size;

      gtk_style_get (widget->style,
                     GTK_TYPE_CHECK_MENU_ITEM,
                     "toggle-spacing", &toggle_spacing,
                     "indicator-size", &indicator_size,
                     NULL);

      max_toggle_size = indicator_size + toggle_spacing;
    }

  for (i = 0; i < geda_menu_get_n_rows (menu); i++) {
    requisition->height += priv->heights[i];
  }

  requisition->width += 2 * max_toggle_size + max_accel_width;
  requisition->width *= geda_menu_get_n_columns (menu);

  gtk_widget_style_get (GTK_WIDGET (menu),
                        "vertical-padding", &vertical_padding,
                        "horizontal-padding", &horizontal_padding,
                        NULL);

  requisition->width += (((GtkContainer*)menu)->border_width + horizontal_padding +
                         widget->style->xthickness) * 2;
  requisition->height += (((GtkContainer*)menu)->border_width + vertical_padding +
                          widget->style->ythickness) * 2;

  menu->toggle_size = max_toggle_size;

  /* Do not resize the tearoff if the tearoff is not active because the tearoff
   * will not redraw --is only a background pixmap.
   */
  if (menu->tearoff_active) {
    geda_menu_set_tearoff_hints (menu, requisition->width);
  }
}

/*! \internal widget_class->style_set */
static void geda_menu_style_set (GtkWidget *widget, GtkStyle *previous_style)
{
  GedaMenu *menu = GEDA_MENU (widget);

  if (gtk_widget_get_realized (widget)) {
      gtk_style_set_background (widget->style, menu->bin_window, GTK_STATE_NORMAL);
      gtk_style_set_background (widget->style, menu->view_window, GTK_STATE_NORMAL);
      gtk_style_set_background (widget->style, widget->window, GTK_STATE_NORMAL);
  }

  change_touchscreen_mode(menu);
}

static void menu_grab_transfer_window_destroy (GedaMenu *menu)
{
  GdkWindow *window = g_object_get_data (G_OBJECT (menu), transfer_window_key);

  if (window) {
    gdk_window_set_user_data (window, NULL);
    gdk_window_destroy (window);
    g_object_set_data (G_OBJECT (menu), transfer_window_key, NULL);
  }
}

/*! \internal widget_class->unrealize */
static void geda_menu_unrealize (GtkWidget *widget)
{
  GedaMenu *menu = (GedaMenu*)widget;

  /* Disconnect the settings Monitor */
  remove_settings_signal(menu, gtk_widget_get_screen (GTK_WIDGET (menu)));

  menu_grab_transfer_window_destroy (menu);

  gdk_window_set_user_data (menu->view_window, NULL);
  gdk_window_destroy (menu->view_window);
  menu->view_window = NULL;

  gdk_window_set_user_data (menu->bin_window, NULL);
  gdk_window_destroy (menu->bin_window);
  menu->bin_window = NULL;

  ((GtkWidgetClass*)geda_menu_parent_class)->unrealize (widget);
}

/* object_class->destroy */
static void geda_menu_destroy (GtkObject *object)
{
  GedaMenu *menu = (GedaMenu*)object;

  geda_menu_remove_scroll_timeout (menu);

  /* If has MenuAttachData data, release the data */
  if (g_object_get_data (G_OBJECT (object), attached_data_key)) {
    geda_menu_detach (menu);
  }

  geda_menu_stop_navigating_submenu (menu);

  if (menu->old_active_menu_item) {
    g_object_unref (menu->old_active_menu_item);
    menu->old_active_menu_item = NULL;
  }

  /* Add back the reference count for being a child */
  if (menu->needs_destruction_ref_count) {
    menu->needs_destruction_ref_count = FALSE;
    g_object_ref (object);
  }

  if (menu->toplevel)
  if (menu->toplevel) {
    gtk_widget_destroy (menu->toplevel);
  }

  if (menu->tearoff_window) {
    gtk_widget_destroy (menu->tearoff_window);
    menu->tearoff_window = NULL;
  }

  ((GtkObjectClass*)geda_menu_parent_class)->destroy (object);
}

/*! \internal gobject_class->dispose */
static void geda_menu_dispose (GObject *object)
{
  GedaMenu *menu = (GedaMenu*)object;

  if (menu->accel_group) {
    geda_menu_set_accel_group(menu, NULL);
  }

  ((GObjectClass*)geda_menu_parent_class)->dispose (object);
}

/*! \internal gobject_class->finalize */
static void geda_menu_finalize (GObject *object)
{
  if (g_hash_table_remove (menu_hash_table, object)) {

    /* If object was in hash table then is a GedaMenu */

    GedaMenu *menu = (GedaMenu*)object;
    GedaMenuPriv *priv = menu->priv;

    if (!g_hash_table_size (menu_hash_table)) {
      g_hash_table_destroy (menu_hash_table);
      menu_hash_table = NULL;
    }

    if (priv->heights) {
      g_free (priv->heights);
      priv->heights = NULL;
    }

    if (priv->title) {
      g_free (priv->title);
      priv->title = NULL;
    }

    g_free(menu->priv);
  }

  ((GObjectClass*)geda_menu_parent_class)->finalize (object);
}

static void geda_menu_get_property (GObject     *object,
                                    unsigned int prop_id,
                                    GValue      *value,
                                    GParamSpec  *pspec)
{
  GedaMenu *menu = (GedaMenu*)object;

  switch (prop_id) {

    case PROP_ACTIVE:
      g_value_set_int (value, g_list_index (((GedaMenuShell*)menu)->children, geda_menu_get_active (menu)));
      break;

    case PROP_ACCEL_GROUP:
      g_value_set_object (value, geda_menu_get_accel_group (menu));
      break;

    case PROP_ACCEL_PATH:
      g_value_set_string (value, geda_menu_get_accel_path (menu));
      break;

    case PROP_ATTACH_WIDGET:
      g_value_set_object (value, geda_menu_get_attach_widget (menu));
      break;

    case PROP_TEAROFF_STATE:
      g_value_set_boolean (value, geda_menu_get_tearoff_state (menu));
      break;

    case PROP_TEAROFF_TITLE:
      g_value_set_string (value, geda_menu_get_title (menu));
      break;

    case PROP_MONITOR:
      g_value_set_int (value, geda_menu_get_monitor (menu));
      break;

    case PROP_RESERVE_TOGGLE_SIZE:
      g_value_set_boolean (value, geda_menu_get_reserve_toggle_size (menu));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_menu_set_property (GObject      *object,
                                    unsigned int  prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
  GedaMenu *menu = (GedaMenu*)object;

  switch (prop_id) {

    case PROP_ACTIVE:
      geda_menu_set_active (menu, g_value_get_int (value));
      break;

    case PROP_ACCEL_GROUP:
      geda_menu_set_accel_group (menu, g_value_get_object (value));
      break;

    case PROP_ACCEL_PATH:
      geda_menu_set_accel_path (menu, g_value_get_string (value));
      break;

    case PROP_ATTACH_WIDGET:
    {
      GtkWidget *widget;

      widget = geda_menu_get_attach_widget (menu);

      if (widget) {
        geda_menu_detach (menu);
      }

      widget = (GtkWidget*)g_value_get_object (value);

      if (widget) {
        geda_menu_attach_to_widget (menu, widget, NULL);
      }
    }
    break;

    case PROP_TEAROFF_STATE:
      geda_menu_set_tearoff_state (menu, g_value_get_boolean (value));
      break;

    case PROP_TEAROFF_TITLE:
      geda_menu_set_title (menu, g_value_get_string (value));
      break;

    case PROP_MONITOR:
      geda_menu_set_monitor (menu, g_value_get_int (value));
      break;

    case PROP_RESERVE_TOGGLE_SIZE:
      geda_menu_set_reserve_toggle_size (menu, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/* -------------- GtkContainer Over-rides -------------- */

/* container_class->get_child_property */
static void geda_menu_get_child_property (GtkContainer *container,
                                          GtkWidget    *child,
                                          unsigned int  property_id,
                                          GValue       *value,
                                          GParamSpec   *pspec)
{
  AttachInfo *ai = get_attach_info (child);

  switch (property_id) {

    case CHILD_PROP_LEFT_ATTACH:
      g_value_set_int (value, ai->left_attach);
      break;
    case CHILD_PROP_RIGHT_ATTACH:
      g_value_set_int (value, ai->right_attach);
      break;
    case CHILD_PROP_TOP_ATTACH:
      g_value_set_int (value, ai->top_attach);
      break;
    case CHILD_PROP_BOTTOM_ATTACH:
      g_value_set_int (value, ai->bottom_attach);
      break;

    default:
      GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID (container, property_id, pspec);
      return;
  }
}

/* container_class->set_child_property */
static void geda_menu_set_child_property (GtkContainer *container,
                                          GtkWidget    *child,
                                          unsigned int  property_id,
                                          const GValue *value,
                                          GParamSpec   *pspec)
{
  GedaMenu *menu = (GedaMenu*)container;
  AttachInfo *ai = get_attach_info (child);

  switch (property_id) {

    case CHILD_PROP_LEFT_ATTACH:
      ai->left_attach = g_value_get_int (value);
      break;
    case CHILD_PROP_RIGHT_ATTACH:
      ai->right_attach = g_value_get_int (value);
      break;
    case CHILD_PROP_TOP_ATTACH:
      ai->top_attach = g_value_get_int (value);
      break;
    case CHILD_PROP_BOTTOM_ATTACH:
      ai->bottom_attach = g_value_get_int (value);
      break;

    default:
      GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID (container, property_id, pspec);
      return;
  }

  menu_queue_resize (menu);
}

/* container_class->remove */
static void geda_menu_remove (GtkContainer *container, GtkWidget *widget)
{
  GedaMenu *menu = (GedaMenu*)container;

  g_return_if_fail (GEDA_IS_MENU_ITEM (widget));

  /* Clear out old_active_menu_item if it matches the item we are removing
   */
  if (menu->old_active_menu_item == widget) {

      g_object_unref (menu->old_active_menu_item);
      menu->old_active_menu_item = NULL;
  }

  ((GtkContainerClass*)geda_menu_parent_class)->remove (container, widget);

  g_object_set_data ((GObject*)widget, attached_info_key, NULL);

  menu_queue_resize (menu);
}

/* ---------------- GedaShell Over-rides --------------- */

/* shell_class->deactivate */
static void geda_menu_deactivate (GedaMenuShell *menu_shell)
{
  GtkWidget *parent;

  g_return_if_fail (GEDA_IS_MENU (menu_shell));

  menu_shell->activate_time = 0;

  /* MUST get pointer to parent BEFORE calling geda_menu_popdown */
  parent = menu_shell->parent_menu_shell;

  geda_menu_popdown ((GedaMenu*)menu_shell);

  if (parent) {
    geda_menu_shell_deactivate ((GedaMenuShell*)parent);
  }
}

/* shell_class->get_popup_delay */
static int geda_menu_get_popup_delay (GedaMenuShell *menu_shell)
{
  int popup_delay = 0;

  if (GEDA_IS_MENU(menu_shell)) {
    gtk_widget_style_get ((GtkWidget*)menu_shell,
                          "menu-popup-delay", &popup_delay, NULL);
  }

  return popup_delay;
}

/* shell_class->insert */
static void geda_menu_real_insert (GedaMenuShell *menu_shell,
                                   GtkWidget     *child,
                                   int            position)
{
  GedaMenu   *menu = (GedaMenu*)menu_shell;
  AttachInfo *ai   = get_attach_info (child);

  ai->left_attach   = -1;
  ai->right_attach  = -1;
  ai->top_attach    = -1;
  ai->bottom_attach = -1;

  if (gtk_widget_get_realized ((GtkWidget*)menu_shell)) {
    gtk_widget_set_parent_window (child, menu->bin_window);
  }

  ((GedaMenuShellClass*)geda_menu_parent_class)->insert (menu_shell, child, position);

  menu_queue_resize (menu);
}

static GtkWidget *find_child_containing (GedaMenuShell *menu_shell,
                                         int            left,
                                         int            right,
                                         int            top,
                                         int            bottom)
{
  GList *list;

  /* find a child which includes the area given by
   * left, right, top, bottom.
   */

  for (list = menu_shell->children; list; list = list->next) {

      int  l, r, t, b;

      if (!geda_menu_item_is_widget_selectable (list->data))
        continue;

      get_effective_child_attach (list->data, &l, &r, &t, &b);

      if (l <= left && right <= r && t <= top && bottom <= b)
        return (GtkWidget*)list->data;
    }

  return NULL;
}

/* shell_class->move_current */
static void geda_menu_move_current (GedaMenuShell *menu_shell, MenuDirection direction)
{
  GedaMenu *menu = (GedaMenu*)menu_shell;

  int l, r, t, b;

  if (gtk_widget_get_direction ((GtkWidget*)menu_shell) == GTK_TEXT_DIR_RTL)
  {
    switch (direction) {

      case MENU_DIR_CHILD:
        direction = MENU_DIR_PARENT;
        break;
      case MENU_DIR_PARENT:
        direction = MENU_DIR_CHILD;
        break;
      default: ;
    }
  }

  /* use special table menu key bindings */
  if (menu_shell->active_menu_item && geda_menu_get_n_columns (menu) > 1) {

    GtkWidget *match = NULL;
    int i;

    get_effective_child_attach (menu_shell->active_menu_item, &l, &r, &t, &b);

    if (direction == MENU_DIR_NEXT) {

      for (i = b; i < geda_menu_get_n_rows (menu); i++) {

        match = find_child_containing (menu_shell, l, l + 1, i, i + 1);
        if (match)
          break;
      }

      if (!match) {

        /* wrap around */
        for (i = 0; i < t; i++) {

          match = find_child_containing (menu_shell,
                                         l, l + 1, i, i + 1);
          if (match) {
            break;
          }
        }
      }
    }
    else if (direction == MENU_DIR_PREV) {

      for (i = t; i > 0; i--) {

        match = find_child_containing (menu_shell, l, l + 1, i - 1, i);
        if (match)
          break;
      }

      if (!match) {

        /* wrap around */
        for (i = geda_menu_get_n_rows (menu); i > b; i--) {

          match = find_child_containing (menu_shell,
                                         l, l + 1, i - 1, i);
          if (match) {
            break;
          }
        }
      }
    }
    else if (direction == MENU_DIR_PARENT) {

      /* we go one left if possible */
      if (l > 0) {
        match = find_child_containing (menu_shell, l - 1, l, t, t + 1);
      }

      if (!match) {

        GtkWidget *parent = menu_shell->parent_menu_shell;

        if (!parent || g_list_length (((GedaMenuShell*)parent)->children) <= 1)
          match = menu_shell->active_menu_item;
      }
    }
    else if (direction == MENU_DIR_CHILD) {

      /* we go one right if possible */
      if (r < geda_menu_get_n_columns (menu))
        match = find_child_containing (menu_shell, r, r + 1, t, t + 1);

      if (!match) {

        GtkWidget *parent;
        GtkWidget *submenu;

        parent  = menu_shell->parent_menu_shell;
        submenu = geda_menu_item_get_submenu_widget(((GedaMenuItem*)menu_shell->active_menu_item));

        if (!submenu &&
           (!parent || g_list_length (((GedaMenuShell*)parent)->children) <= 1))
        {
          match = menu_shell->active_menu_item;
        }
      }
    }

    if (match) {
      geda_menu_shell_select_item (menu_shell, match);
      return;
    }
  }

  ((GedaMenuShellClass*)geda_menu_parent_class)->move_current (menu_shell, direction);
}

/* shell_class->select_item */
static void geda_menu_select_item (GedaMenuShell *menu_shell, GtkWidget *menu_item)
{
  GedaMenu *menu = (GedaMenu*)menu_shell;

  if (gtk_widget_get_realized ((GtkWidget*)menu)) {
    geda_menu_scroll_item_visible (menu_shell, menu_item);
  }

  ((GedaMenuShellClass*)geda_menu_parent_class)->select_item (menu_shell, menu_item);
}

/* ------------------ Signal Handlers ------------------ */

/*
 * helper called by child_at & geda_menu_real_move_scroll
 */
static int get_visible_size (GedaMenu *menu)
{
  GtkWidget *widget = (GtkWidget*)menu;
  GtkContainer *container = (GtkContainer*)widget;

  int  menu_height = (widget->allocation.height - 2 * (container->border_width
                                                + widget->style->ythickness));

  if (!menu->tearoff_active) {

    GtkBorder arrow_border;

    get_arrows_border (menu, &arrow_border);
    menu_height -= arrow_border.top;
    menu_height -= arrow_border.bottom;
  }

  return menu_height;
}

static int get_menu_height (GedaMenu *menu)
{
  int  height;
  GtkWidget *widget = (GtkWidget*)menu;

  height  = widget->requisition.height;
  height -= (((GtkContainer*)widget)->border_width + widget->style->ythickness) * 2;

  if (!menu->tearoff_active) {

      GtkBorder arrow_border;

      get_arrows_border (menu, &arrow_border);
      height -= arrow_border.top;
      height -= arrow_border.bottom;
    }

  return height;
}

/* Find the sensitive on-screen child containing \a y, or if none,
 * the nearest selectable onscreen child. (%NULL if none)
 */
static GtkWidget *child_at (GedaMenu *menu, int y)
{
  GedaMenuShell *menu_shell   = (GedaMenuShell*)menu;
  GtkWidget     *child        = NULL;
  int            child_offset = 0;
  GList         *children;
  int  menu_height;
  int  lower, upper;        /* Onscreen bounds */

  menu_height = get_visible_size (menu);
  lower = menu->scroll_offset;
  upper = menu->scroll_offset + menu_height;

  for (children = menu_shell->children; children; children = children->next)
  {
    if (gtk_widget_get_visible (children->data)) {

      GtkRequisition child_requisition;

      gtk_widget_size_request (children->data, &child_requisition);

      if (geda_menu_item_is_widget_selectable (children->data) &&
        child_offset >= lower &&
        child_offset + child_requisition.height <= upper)
      {
        child = children->data;

        if (child_offset + child_requisition.height > y &&
          !GTK_IS_TEAROFF_MENU_ITEM (child))
          return child;
      }

      child_offset += child_requisition.height;
    }
  }

  return child;
}

static void geda_menu_real_move_scroll (GedaMenu *menu, GtkScrollType type)
{
  GedaMenuShell *menu_shell   = (GedaMenuShell*)menu;
  int            page_size    = get_visible_size (menu);
  int            end_position = get_menu_height (menu);

  switch (type) {

    case GTK_SCROLL_PAGE_UP:
    case GTK_SCROLL_PAGE_DOWN:
    {
      int   child_offset = 0;
      int   old_offset;
      int   new_offset;
      int   step;
      bool  old_upper_arrow_visible;

      if (type == GTK_SCROLL_PAGE_UP) {
        step = - page_size;
      }
      else {
        step = page_size;
      }

      if (menu_shell->active_menu_item) {

        int  child_height;

        compute_child_offset (menu, menu_shell->active_menu_item,
                              &child_offset, &child_height, NULL);

        /* increase offset by 1/2 the height of child */
        child_offset += child_height >> 1;           /* Divide by 2 */
      }

      menu_shell->ignore_enter = TRUE;
      old_upper_arrow_visible  = menu->upper_arrow_visible && !menu->tearoff_active;
      old_offset               = menu->scroll_offset;

      new_offset               = menu->scroll_offset + step;
      new_offset               = CLAMP(new_offset, 0, end_position - page_size);

      geda_menu_scroll_to (menu, new_offset);

      if (menu_shell->active_menu_item) {

        GtkBorder  arrow_border;
        GtkWidget *new_child;
        bool       new_upper_arrow_visible;

        new_upper_arrow_visible = menu->upper_arrow_visible && !menu->tearoff_active;

        get_arrows_border (menu, &arrow_border);

        if (menu->scroll_offset != old_offset) {
          step = menu->scroll_offset - old_offset;
        }

        step -= (new_upper_arrow_visible - old_upper_arrow_visible) * arrow_border.top;

        new_child = child_at (menu, child_offset + step);

        if (new_child) {
          geda_menu_shell_select_item (menu_shell, new_child);
        }
      }
    }
    break;

    case GTK_SCROLL_START:
      /* Ignore the enter event we might get if the pointer is on the menu */
      menu_shell->ignore_enter = TRUE;
      geda_menu_scroll_to (menu, 0);
      geda_menu_shell_select_first (menu_shell, TRUE);
      break;

    case GTK_SCROLL_END:
      /* Ignore the enter event we might get if the pointer is on the menu */
      menu_shell->ignore_enter = TRUE;
      geda_menu_scroll_to (menu, end_position - page_size);
      geda_menu_shell_select_last (menu_shell, TRUE);
      break;

    default:
      break;
  }
}

/*! \brief GedaMenu Class Initializer
 *  \par Function Description
 *  Function is called to initialize the class instance.
 *
 * \param [in] class      A GedaMenuClass Object
 * \param [in] class_data GedaMenu structure associated with the class
 */
static void geda_menu_class_init  (void *class, void *class_data)
{
  GObjectClass       *gobject_class    = (GObjectClass*)class;
  GtkObjectClass     *object_class     = (GtkObjectClass*)class;
  GtkWidgetClass     *widget_class     = (GtkWidgetClass*)class;
  GtkContainerClass  *container_class  = (GtkContainerClass*)class;
  GedaMenuShellClass *shell_class      = (GedaMenuShellClass*)class;
  GtkBindingSet      *binding_set;
  GParamSpec         *params;

  gobject_class->dispose              = geda_menu_dispose;
  gobject_class->finalize             = geda_menu_finalize;
  gobject_class->get_property         = geda_menu_get_property;
  gobject_class->set_property         = geda_menu_set_property;

  object_class->destroy               = geda_menu_destroy;

  widget_class->button_press_event    = geda_menu_button_press;
  widget_class->button_release_event  = geda_menu_button_release;
  widget_class->can_activate_accel    = geda_menu_real_can_activate_accel;
  widget_class->enter_notify_event    = geda_menu_enter_notify;
  widget_class->expose_event          = geda_menu_expose;
  widget_class->focus                 = geda_menu_focus;
  widget_class->grab_notify           = geda_menu_grab_notify;
  widget_class->hide_all              = geda_menu_hide_all;
  widget_class->key_press_event       = geda_menu_key_press;
  widget_class->leave_notify_event    = geda_menu_leave_notify;
  widget_class->motion_notify_event   = geda_menu_motion_notify;
  widget_class->realize               = geda_menu_realize;
  widget_class->scroll_event          = geda_menu_scroll;
  widget_class->show                  = geda_menu_show;
  widget_class->show_all              = geda_menu_show_all;
  widget_class->size_allocate         = geda_menu_size_allocate;
  widget_class->size_request          = geda_menu_size_request;
  widget_class->style_set             = geda_menu_style_set;
  widget_class->unrealize             = geda_menu_unrealize;

  container_class->get_child_property = geda_menu_get_child_property;
  container_class->set_child_property = geda_menu_set_child_property;
  container_class->remove             = geda_menu_remove;

  shell_class->deactivate             = geda_menu_deactivate;
  shell_class->get_popup_delay        = geda_menu_get_popup_delay;
  shell_class->insert                 = geda_menu_real_insert;
  shell_class->move_current           = geda_menu_move_current;
  shell_class->select_item            = geda_menu_select_item;
  shell_class->submenu_placement      = GTK_LEFT_RIGHT;

  geda_menu_parent_class = g_type_class_peek_parent (class);

  menu_signals[MOVE_SCROLL] =
    g_signal_new_class_handler ("move-scroll",
                                geda_menu_get_type(),
                                G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                                G_CALLBACK (geda_menu_real_move_scroll),
                                NULL, NULL,
                                geda_marshal_VOID__ENUM,
                                G_TYPE_NONE, 1,
                                GTK_TYPE_SCROLL_TYPE);

  /*!
   * property "active": GedaMenu::active
   * The index of the currently selected menu item, or -1 if no
   * menu item is selected.
   */
  params = g_param_spec_int ("active",
                           _("Active"),
                           _("The currently selected menu item"),
                             -1,
                             G_MAXINT,
                             -1,
                             G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ACTIVE, params);

  /*!
   * property "accel-group": GedaMenu::accel-group
   * The accel group holding accelerators for the menu.
   */
  params = g_param_spec_object ("accel-group",
                              _("Accel Group"),
                              _("The accel group holding accelerators for the menu"),
                                 GTK_TYPE_ACCEL_GROUP,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ACCEL_GROUP, params);

  /*!
   * property "accel-path": GedaMenu::accel-path
   * An accel path used to construct accel paths of child items.
   */
  params = g_param_spec_string ("accel-path",
                              _("Accel Path"),
                              _("An accel path used to construct accel paths of child items"),
                                 NULL,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ACCEL_PATH, params);

  /*!
   * property "attach-widget": GedaMenu::attach-widget
   * \brief Parent widget to which the menu is attached.
   * \par
   *  The widget the menu is attached to. Setting this property attaches
   *  the menu without a #MenuDetachFunc. If you need to use a detacher,
   *  use geda_menu_attach_to_widget() directly.
   */
  params = g_param_spec_object ("attach-widget",
                              _("Attach Widget"),
                              _("The widget the menu is attached to"),
                                 GTK_TYPE_WIDGET,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ATTACH_WIDGET, params);

  /*!
   * property "tearoff-title": GedaMenu::tearoff-title
   * \brief Title displayed if torn-off
   */
  params = g_param_spec_string ("tearoff-title",
                              _("Tearoff Title"),
                              _("A title that may be displayed by the window manager when this menu is torn-off"),
                                 NULL,
                                 G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_TEAROFF_TITLE, params);

  /*!
   * property "tearoff-state": GedaMenu::tearoff-state
   * \brief Boolean that indicates whether the menu is torn-off.
   */
  params = g_param_spec_boolean ("tearoff-state",
                               _("Tearoff State"),
                               _("A boolean that indicates whether the menu is torn-off"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_TEAROFF_STATE, params);

  /*!
   * property "monitor": GedaMenu::monitor
   * \brief The monitor the menu will be popped up on.
   */
  params = g_param_spec_int ("monitor",
                           _("Monitor"),
                           _("The monitor the menu will be popped up on"),
                             -1,
                             G_MAXINT,
                             -1,
                             G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_MONITOR, params);
  /*!
   * property "reserve-toggle-size" GedaMenu::reserve-toggle-size
   * \brief Determines whether to researve space for images.
   * \par
   *  Boolean that indicates whether the menu reserves space for
   *  toggles and icons, regardless of their actual presence.
   *
   *  This property should only be changed from its default value
   *  for special-purposes such as tabular menus. Regular menus that
   *  are connected to a menu bar or context menus should reserve
   *  toggle space for consistency.
   */
  params = g_param_spec_boolean ("reserve-toggle-size",
                               _("Reserve Toggle Size"),
                               _("A boolean that indicates whether the menu reserves space for toggles and icons"),
                                  TRUE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class,
                                   PROP_RESERVE_TOGGLE_SIZE, params);

  /* Widget Properties */

  /*!
   * property "arrow-placement" GedaMenu::arrow-placement
   * \brief determines placement of scroll arrow.
   * \par
   *  Indicates where scroll arrows should be placed.
   */
  params = g_param_spec_enum ("arrow-placement",
                            _("Arrow Placement"),
                            _("Indicates where scroll arrows should be placed"),
                               GTK_TYPE_ARROW_PLACEMENT,
                               GTK_ARROWS_BOTH,
                               G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  /*!
   * property "arrow-scaling": GedaMenu::arrow-scaling
   * \brief scroll arrow size scaling constant.
   * \par
   *  Arbitrary constant to scale down the size of the scroll arrow.
   */
  params = g_param_spec_float ("arrow-scaling",
                             _("Arrow Scaling"),
                             _("Arbitrary constant to scale down the size of the scroll arrow"),
                                0.0,
                                1.0,
                                0.7,
                                G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  params = g_param_spec_boolean ("double-arrows",
                               _("Double Arrows"),
                               _("When scrolling, always show both arrows."),
                                  TRUE,
                                  G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  params = g_param_spec_int ("horizontal-offset",
                           _("Horizontal Offset"),
                           _("When the menu is a submenu, position it this number of pixels offset horizontally"),
                              G_MININT,
                              MAX_OFFSET_PADDING,
                              -2,
                              G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  /*!
   * property "horizontal-padding": GedaMenu::horizontal-padding
   * \brief Controls the horizontal spacing between menu items.
   */
  params = g_param_spec_int ("horizontal-padding",
                           _("Horizontal Padding"),
                           _("Extra space at the left and right edges of the menu"),
                              0,
                              MAX_OFFSET_PADDING,
                              0,
                              G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  params = g_param_spec_int ("vertical-offset",
                           _("Vertical Offset"),
                           _("When the menu is a submenu, position it this number of pixels offset vertically"),
                              G_MININT,
                              MAX_OFFSET_PADDING,
                              0,
                              G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  /*!
   * property "vertical-padding": GedaMenu::vertical-padding
   * \brief Controls extra space at the top and bottom of the menu.
   */
  params = g_param_spec_int ("vertical-padding",
                           _("Vertical Padding"),
                           _("Extra space at the top and bottom of the menu"),
                              0,
                              MAX_OFFSET_PADDING,
                              1,
                              G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  /*!
   * property "menu-popup-delay": GedaMenu::menu-popup-delay
   * \brief
   *  Controls the minimum time the pointer must stay over a menu
   *  item before the submenu appear.
   */
  params = g_param_spec_int ("menu-popup-delay",
                           _("Delay before submenus appear"),
                           _("Minimum time the pointer must stay over a menu item before the submenu appear"),
                              0,
                              INT_MAX,
                              DEFAULT_POPUP_DELAY,
                              G_PARAM_READWRITE);

  gtk_widget_class_install_style_property (widget_class, params);

  /*!
   * property "menu-popdown-delay": GedaMenu::menu-popdown-delay
   * \brief
   *  Controls the time before hiding a submenu when the pointer
   *  is moving towards the submenu.
   */
  params = g_param_spec_int ("menu-popdown-delay",
                           _("Delay before hiding a submenu"),
                           _("The time before hiding a submenu when the pointer is moving towards the submenu"),
                              0,
                              INT_MAX,
                              DEFAULT_POPDOWN_DELAY,
                              G_PARAM_READWRITE);

  gtk_widget_class_install_style_property (widget_class, params);

  /* Container Properties */

  params = g_param_spec_int ("left-attach",
                           _("Left Attach"),
                           _("The column number to attach the left side of the child to"),
                             -1,
                             INT_MAX,
                             -1,
                             G_PARAM_READWRITE);

  gtk_container_class_install_child_property (container_class,
                                              CHILD_PROP_LEFT_ATTACH, params);

  params = g_param_spec_int ("right-attach",
                           _("Right Attach"),
                           _("The column number to attach the right side of the child to"),
                             -1,
                              INT_MAX,
                             -1,
                              G_PARAM_READWRITE);

  gtk_container_class_install_child_property (container_class,
                                              CHILD_PROP_RIGHT_ATTACH, params);

  params = g_param_spec_int ("top-attach",
                           _("Top Attach"),
                           _("The row number to attach the top of the child to"),
                             -1,
                             INT_MAX,
                             -1,
                              G_PARAM_READWRITE);

  gtk_container_class_install_child_property (container_class,
                                              CHILD_PROP_TOP_ATTACH, params);

  params = g_param_spec_int ("bottom-attach",
                           _("Bottom Attach"),
                           _("The row number to attach the bottom of the child to"),
                              -1,
                              INT_MAX,
                              -1,
                              G_PARAM_READWRITE);

  gtk_container_class_install_child_property (container_class,
                                              CHILD_PROP_BOTTOM_ATTACH, params);

  /* Bindings */

  binding_set = gtk_binding_set_by_class (class);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_Up, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_PREV);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KP_Up, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_PREV);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_Down, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_NEXT);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KP_Down, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_NEXT);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_Left, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_PARENT);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KP_Left, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_PARENT);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_Right, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_CHILD);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KP_Right, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_CHILD);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_Home, 0,
                                "move-scroll", 1,
                                GTK_TYPE_SCROLL_TYPE,
                                GTK_SCROLL_START);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KP_Home, 0,
                                "move-scroll", 1,
                                GTK_TYPE_SCROLL_TYPE,
                                GTK_SCROLL_START);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_End, 0,
                                "move-scroll", 1,
                                GTK_TYPE_SCROLL_TYPE,
                                GTK_SCROLL_END);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KP_End, 0,
                                "move-scroll", 1,
                                GTK_TYPE_SCROLL_TYPE,
                                GTK_SCROLL_END);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_Page_Up, 0,
                                "move-scroll", 1,
                                GTK_TYPE_SCROLL_TYPE,
                                GTK_SCROLL_PAGE_UP);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KP_Page_Up, 0,
                                "move-scroll", 1,
                                GTK_TYPE_SCROLL_TYPE,
                                GTK_SCROLL_PAGE_UP);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_Page_Down, 0,
                                "move-scroll", 1,
                                GTK_TYPE_SCROLL_TYPE,
                                GTK_SCROLL_PAGE_DOWN);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KP_Page_Down, 0,
                                "move-scroll", 1,
                                GTK_TYPE_SCROLL_TYPE,
                                GTK_SCROLL_PAGE_DOWN);
}

/*!
 * \brief Type instance initializer for GedaMenu
 * \par Function Description
 *  Type instance initializer for GedaMenu, initializes a new empty
 *  GedaMenu object.
 *
 * \param [in] instance The GedaMenu structure being initialized,
 * \param [in] class    The GedaMenu class we are initializing.
 */
static void
geda_menu_instance_init (GTypeInstance *instance, void *class)
{
  GedaMenu     *menu;
  GedaMenuPriv *priv;
  GtkWindow    *toplevel;

  menu                 = (GedaMenu*)instance;
  menu->priv           = g_malloc0 (sizeof(GedaMenuPriv));
  priv                 = menu->priv;

  menu->parent_menu_item     = NULL;
  menu->old_active_menu_item = NULL;
  menu->accel_group          = NULL;
  menu->position_func        = NULL;
  menu->position_func_data   = NULL;
  menu->toggle_size          = 0;

  toplevel = g_object_new (GTK_TYPE_WINDOW, "type",
                           GTK_WINDOW_POPUP,"child", menu, NULL);

  menu->toplevel = g_object_connect (toplevel,
                                     "signal::event", geda_menu_window_event, menu,
                                     "signal::size-request", geda_menu_window_size_request, menu,
                                     "signal::destroy", gtk_widget_destroyed, &menu->toplevel,
                                     NULL);

  gtk_window_set_resizable ((GtkWindow*)menu->toplevel, FALSE);
  gtk_window_set_mnemonic_modifier ((GtkWindow*)menu->toplevel, 0);

  /* Refloat the menu, so that reference counting for the menu is
   * not affected by it being a child of the toplevel
   */
  g_object_force_floating ((GObject*)menu);
  menu->needs_destruction_ref_count = TRUE;

  menu->view_window          = NULL;
  menu->bin_window           = NULL;

  menu->scroll_offset        = 0;
  menu->scroll_step          = 0;
  menu->timeout_id           = 0;
  menu->scroll_fast          = FALSE;

  menu->tearoff_window       = NULL;
  menu->tearoff_hbox         = NULL;
  menu->torn_off             = FALSE;
  menu->tearoff_active       = FALSE;
  menu->tearoff_adjustment   = NULL;
  menu->tearoff_scrollbar    = NULL;

  menu->upper_arrow_visible  = FALSE;
  menu->lower_arrow_visible  = FALSE;
  menu->upper_arrow_prelight = FALSE;
  menu->lower_arrow_prelight = FALSE;

  priv->upper_arrow_state    = GTK_STATE_NORMAL;
  priv->lower_arrow_state    = GTK_STATE_NORMAL;

  priv->have_layout          = FALSE;
  priv->monitor_num          = -1;

#if (GTK_MAJOR_VERSION == 3)

  GtkStyleContext *context;

  context = gtk_widget_get_style_context ((GtkWidget*)menu);
  gtk_style_context_add_class (context, GTK_STYLE_CLASS_MENU);

#endif

  if (!menu_hash_table) {
    menu_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (menu_hash_table, instance, instance);
}

/*!
 * \brief Retrieve GedaMenu's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaMenu Type identifier. When
 *  first called, the function registers a #GedaMenu in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaMenu and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaMenu.
 */
GedaType geda_menu_get_type (void)
{
  static volatile GedaType geda_menu_type = 0;

  if (g_once_init_enter (&geda_menu_type)) {

    static const GTypeInfo info = {
      sizeof(GedaMenuClass),
      NULL,                      /* base_init           */
      NULL,                      /* base_finalize       */
      geda_menu_class_init,     /* (GClassInitFunc)    */
      NULL,                      /* class_finalize      */
      NULL,                      /* class_data          */
      sizeof(GedaMenu),
      0,                         /* n_preallocs         */
      geda_menu_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaMenu");
    type   = g_type_register_static (GEDA_TYPE_MENU_SHELL, string, &info, 0);

    g_once_init_leave (&geda_menu_type, type);
  }

  return geda_menu_type;
}

bool is_a_geda_menu (GedaMenu *menu)
{
  if ((menu != NULL) && (menu_hash_table != NULL)) {
    return g_hash_table_lookup(menu_hash_table, menu) ? TRUE : FALSE;
  }
  return FALSE;
}

static void
menu_change_screen (GedaMenu *menu, GdkScreen *new_screen)
{
  GedaMenuPriv *private = menu->priv;

  if (gtk_widget_has_screen ((GtkWidget*)menu)) {

    if (new_screen == gtk_widget_get_screen ((GtkWidget*)menu)) {
      return;
    }
  }

  if (menu->torn_off) {
    gtk_window_set_screen ((GtkWindow*)menu->tearoff_window, new_screen);
    geda_menu_position (menu, TRUE);
  }

  gtk_window_set_screen ((GtkWindow*)menu->toplevel, new_screen);
  private->monitor_num = -1;
}

static void
attach_widget_screen_changed (GtkWidget *attach_widget,
                              GdkScreen *previous_screen,
                              GedaMenu  *menu)
{
  if (gtk_widget_has_screen (attach_widget) &&
     !g_object_get_data ((GObject*)menu, explicit_screen_key))
    {
      menu_change_screen (menu, gtk_widget_get_screen (attach_widget));
    }
}

 /*!
 * \brief Attach a GedaMenu to a Widget
 * \par Function Description
 * Attaches the menu to the widget and provides a callback function
 * that will be invoked when the menu calls geda_menu_detach() during
 * its destruction.
 *
 * If the menu is attached to the widget then it will be destroyed
 * when the widget is destroyed, as if it was a child widget.
 * An attached menu will also move between screens correctly if the
 * widgets moves between screens.
 *
 * \param[in] menu          Pointer #GedaMenu object
 * \param[in] attach_widget A GtkWidget the menu will be attached to
 * \param[in] detacher      User function called when the menu calls
 *                          is detached
 */
void
geda_menu_attach_to_widget (GedaMenu       *menu,
                            GtkWidget      *attach_widget,
                            MenuDetachFunc  detacher)
{
  MenuAttachData *data;
  GList          *list;

  g_return_if_fail (GEDA_IS_MENU (menu));
  g_return_if_fail (GTK_IS_WIDGET (attach_widget));

  /* keep this function in sync with gtk_widget_set_parent() */

  data = g_object_get_data ((GObject*)menu, attached_data_key);

  if (data) {

    const char *type_name;

    type_name = g_type_name (G_TYPE_FROM_INSTANCE (data->attach_widget));

    g_warning ("%s: menu already attached to %s", __func__, type_name);

    return;
  }

  g_object_ref_sink (menu);

  data = malloc (sizeof(MenuAttachData));
  data->attach_widget = attach_widget;

  g_signal_connect (attach_widget, "screen-changed",
                    G_CALLBACK (attach_widget_screen_changed), menu);

  attach_widget_screen_changed (attach_widget, NULL, menu);

  data->detacher = detacher;

  g_object_set_data ((GObject*)menu, attached_data_key, data);

  list = g_object_steal_data ((GObject*)attach_widget, attached_menus_key);

  if (!g_list_find (list, menu)) {
    list = g_list_prepend (list, menu);
  }

  g_object_set_data_full ((GObject*)attach_widget, attached_menus_key, list,
                         (GDestroyNotify) g_list_free);

  if (gtk_widget_get_state ((GtkWidget*)menu) != GTK_STATE_NORMAL) {
    gtk_widget_set_state ((GtkWidget*)menu, GTK_STATE_NORMAL);
  }

  if (GEDA_IS_MENU_ITEM(attach_widget)) {

    GedaMenuItem *menu_item;
    GtkWidget    *submenu;

    menu_item = (GedaMenuItem*)attach_widget;

    submenu = geda_menu_item_get_submenu_widget (menu_item);

    if (submenu != (GtkWidget*)menu) {
      geda_menu_item_set_submenu(menu_item, menu);
    }

    menu->parent_menu_item = attach_widget;
  }

  /* we don't need to set the style here, since we are a toplevel widget. */

  /* Fallback title for menu comes from attach widget */
  geda_menu_update_title (menu);

  GEDA_OBJECT_NOTIFY (menu, "attach-widget");
}

/*!
 * \brief Retrieve Widget attached to a GedaMenu
 * \par Function Description
 * Returns the GtkWidget that the menu is attached to.
 *
 * \param[in] menu: a #GedaMenu
 *
 * \returns GtkWidget that the menu is attached to
 */
GtkWidget *geda_menu_get_attach_widget (GedaMenu *menu)
{
  MenuAttachData *data;

  data = g_object_get_data ((GObject*)menu, attached_data_key);

  if (data) {
    return data->attach_widget;
  }

  return NULL;
}

/*!
 * \brief Detach a GedaMenu from a Widget
 * \par Function Description
 * Detaches the menu from the widget to which the menu had been attached.
 * This function will call the callback function, \a detacher, if provided
 * when the geda_menu_attach_to_widget() function was called.
 *
 * \param[in] menu: a #GedaMenu
 */
void geda_menu_detach (GedaMenu *menu)
{
  MenuAttachData *data;
  GList          *list;

  /* keep this function in sync with gtk_widget_unparent() */
  data = g_object_get_data (G_OBJECT (menu), attached_data_key);

  if (!data) {
    return;
  }

  g_object_set_data ((GObject*)menu, attached_data_key, NULL);

  g_signal_handlers_disconnect_by_func (data->attach_widget,
                                       (void*)attach_widget_screen_changed,
                                        menu);

  if (data->detacher) {
    data->detacher (data->attach_widget, menu);
  }

  list = g_object_steal_data ((GObject*)data->attach_widget, attached_menus_key);

  if (list) {
    list = g_list_remove (list, menu);
    g_object_set_data_full ((GObject*)data->attach_widget, attached_menus_key, list,
                            (GDestroyNotify) g_list_free);
  }
  else {
    g_object_set_data ((GObject*)data->attach_widget, attached_menus_key, NULL);
  }

  if (gtk_widget_get_realized ((GtkWidget*)menu)) {
    gtk_widget_unrealize ((GtkWidget*)menu);
  }

  free (data);

  menu->parent_menu_item = NULL;

  /* Fallback title for menu comes from attach widget */
  geda_menu_update_title (menu);

  GEDA_OBJECT_NOTIFY (menu, "attach-widget");
  g_object_unref (menu);
}

GtkWidget *geda_menu_new (void)
{
  return g_object_new (GEDA_TYPE_MENU, NULL);
}

static void geda_menu_tearoff_bg_copy (GedaMenu *menu)
{
  if (menu->torn_off) {

    GdkPixmap *pixmap;
    GdkWindow *window;
    cairo_t   *cr;
    int        width, height;

    menu->tearoff_active = FALSE;
    menu->saved_scroll_offset = menu->scroll_offset;

    window = geda_get_widget_window(menu->tearoff_window);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

    width  = gdk_window_get_width (window);
    height = gdk_window_get_height (window);

#else

    gdk_drawable_get_size(window, &width, &height);

#endif

    pixmap = gdk_pixmap_new (menu->tearoff_window->window, width, height, -1);

    cr = gdk_cairo_create (pixmap);
    /* Let's hope that function never notices we're not passing it a pixmap */
    gdk_cairo_set_source_pixmap (cr, menu->tearoff_window->window, 0, 0);
    cairo_paint (cr);
    cairo_destroy (cr);

    gtk_widget_set_size_request (menu->tearoff_window, width, height);

    gdk_window_set_back_pixmap (menu->tearoff_window->window, pixmap, FALSE);
    g_object_unref (pixmap);
  }
}

static bool popup_grab_on_window (GdkWindow *window,
                                  uint32     activate_time,
                                  bool       grab_keyboard)
{
  if ((gdk_pointer_grab (window, TRUE,
                         GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK |
                         GDK_ENTER_NOTIFY_MASK | GDK_LEAVE_NOTIFY_MASK |
                         GDK_POINTER_MOTION_MASK,
                         NULL, NULL, activate_time) == 0))
  {
    if (!grab_keyboard ||
         gdk_keyboard_grab (window, TRUE, activate_time) == 0)
    {
      return TRUE;
    }
    else
    {

#if GTK_CHECK_VERSION (2,24,0)

      gdk_display_pointer_ungrab (gdk_window_get_display (window),
                                  activate_time);

#else

      GdkDisplay *display;

      display = gdk_drawable_get_display ((GdkDrawable*)window);
      gdk_display_pointer_ungrab (display, activate_time);

#endif

      return FALSE;
    }
  }

  return FALSE;
}

/*! \internal
 * Retrieves or creates a fictitious GdkWindow used to facilitate
 * transferring of xgrab to the popup.
 * \sa notes in geda_menu_popup() for information about the "grab transfer window"
 */
static GdkWindow *menu_grab_transfer_window_get (GedaMenu *menu)
{
  GdkWindow *window = g_object_get_data ((GObject*)menu, transfer_window_key);

  if (!window) {

    GdkWindowAttr attributes;
    int  attributes_mask;

    attributes.x                 = -100;
    attributes.y                 = -100;
    attributes.width             = 10;
    attributes.height            = 10;
    attributes.window_type       = GDK_WINDOW_TEMP;
    attributes.wclass            = GDK_INPUT_ONLY;
    attributes.override_redirect = TRUE;
    attributes.event_mask        = 0;

    attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_NOREDIR;

    window = gdk_window_new (gtk_widget_get_root_window ((GtkWidget*)menu),
                             &attributes, attributes_mask);
    gdk_window_set_user_data (window, menu);

    gdk_window_show (window);

    g_object_set_data ((GObject*)menu, transfer_window_key, window);
  }

  return window;
}

/*!
 * \brief Make a GedaMenu popup
 * \par Function Description
 * Displays a menu and makes it available for selection.  Applications can use
 * this function to display context-sensitive menus, and will typically supply
 * %NULL for the \a parent_menu_shell, \a parent_menu_item, \a func and \a data
 * parameters. The default menu positioning function will position the menu
 * at the current mouse cursor position.
 *
 * The \a button parameter should be the mouse button pressed to initiate
 * the menu popup. If the menu popup was initiated by something other than
 * a mouse button press, such as a mouse button release or a keypress,
 * \param[in] button should be 0.
 *
 * The \a activate_time parameter is used to conflict-resolve initiation of
 * concurrent requests for mouse/keyboard grab requests. To function
 * properly, this needs to be the time stamp of the user event (such as
 * a mouse click or key press) that caused the initiation of the popup.
 * Only if no such event is available, gtk_get_current_event_time() can
 * be used instead.
 *
 * \param[in] menu              Pointer to a #GedaMenu.
 * \param[in] parent_menu_shell the menu shell containing the triggering menu item, or %NULL
 * \param[in] parent_menu_item  the menu item whose activation triggered the popup, or %NULL
 * \param[in] func              a user supplied function used to position the menu, or %NULL
 * \param[in] data              user supplied data to be passed to \a func.
 * \param[in] button            the mouse button which was pressed to initiate the event.
 * \param[in] activate_time     the time at which the activation event occurred.
 */
void geda_menu_popup (GedaMenu         *menu,
                      GtkWidget        *parent_menu_shell,
                      GtkWidget        *parent_menu_item,
                      MenuPositionFunc  func,
                      void             *data,
                      unsigned int      button,
                      uint32            activate_time)
{
  GtkWidget     *widget;
  GtkWidget     *xgrab_shell;
  GtkWidget     *parent;
  GdkEvent      *current_event;
  GedaMenuShell *menu_shell;
  bool           grab_keyboard;
  GtkWidget     *parent_toplevel;

  g_return_if_fail (GEDA_IS_MENU (menu));

  widget     = (GtkWidget*)menu;
  menu_shell = (GedaMenuShell*)menu;

  menu_shell->parent_menu_shell = parent_menu_shell;

  /* Find the last viewable ancestor and make an X grab on it */

  parent      = (GtkWidget*)menu;
  xgrab_shell = NULL;

  while (parent) {

    bool  viewable = TRUE;
    GtkWidget *tmp = parent;

    while (tmp) {

      if (!gtk_widget_get_mapped (tmp)) {

        viewable = FALSE;
        break;
      }
      tmp = tmp->parent;
    }

    if (viewable)
      xgrab_shell = parent;

    parent = ((GedaMenuShell*)parent)->parent_menu_shell;
  }

  /* We want to receive events generated when we map the menu; unfortunately,
   * since there is probably already an implicit grab in place from the
   * button that the user used to pop up the menu, we won't receive then --
   * in particular, the EnterNotify when the menu pops up under the pointer.
   *
   * If we are grabbing on a parent menu shell, no problem; just grab on
   * that menu shell first before popping up the window with owner_events = TRUE.
   *
   * When grabbing on the menu itself, things get more convuluted;
   * we do an explicit grab on a specially created window with
   * owner_events = TRUE, which we override further down with a grab
   * on the menu. (We can't grab on the menu until it is mapped; we
   * probably could just leave the grab on the other window, with a
   * little reorganization of the code in geda_menu*).
   */
  grab_keyboard = geda_menu_shell_get_take_focus (menu_shell);
  gtk_window_set_accept_focus (GTK_WINDOW (menu->toplevel), grab_keyboard);

  if (xgrab_shell && xgrab_shell != widget) {
    ((GedaMenuShell*)xgrab_shell)->have_xgrab =
      popup_grab_on_window (xgrab_shell->window, activate_time, grab_keyboard);
  }
  else {

    GdkWindow *transfer_window;

    xgrab_shell = widget;
    transfer_window = menu_grab_transfer_window_get (menu);
    ((GedaMenuShell*)xgrab_shell)->have_xgrab =
      popup_grab_on_window (transfer_window, activate_time, grab_keyboard);
  }

  if (!((GedaMenuShell*)xgrab_shell)->have_xgrab) {
    /* We failed to make our pointer/keyboard grab. Rather than leaving the user
     * with a stuck up window, we just abort here. Presumably the user will
     * try again.
     */
    menu_shell->parent_menu_shell = NULL;
    menu_grab_transfer_window_destroy (menu);
    return;
  }

  menu_shell->active = TRUE;
  menu_shell->button = button;

  /* If we are popping up the menu from something other than, a button
   * press then, as a heuristic, we ignore enter events for the menu
   * until we get a MOTION_NOTIFY.
   */

  current_event = gtk_get_current_event ();

  if (current_event) {
    if ((current_event->type != GDK_BUTTON_PRESS) &&
        (current_event->type != GDK_ENTER_NOTIFY))
      menu_shell->ignore_enter = TRUE;

    gdk_event_free (current_event);
  }
  else {
    menu_shell->ignore_enter = TRUE;
  }

  if (menu->torn_off) {
    geda_menu_tearoff_bg_copy (menu);
    geda_menu_reparent (menu, menu->toplevel, FALSE);
  }

  parent_toplevel = NULL;

  if (parent_menu_shell) {
    parent_toplevel = gtk_widget_get_toplevel (parent_menu_shell);
  }
  else if (!g_object_get_data ((GObject*)menu, explicit_screen_key)) {

    GtkWidget *attach_widget = geda_menu_get_attach_widget (menu);

    if (attach_widget) {
      parent_toplevel = gtk_widget_get_toplevel (attach_widget);
    }
  }

  /* Set transient for to get the right window group and parent relationship */
  if (GTK_IS_WINDOW (parent_toplevel)) {
    gtk_window_set_transient_for ((GtkWindow*)menu->toplevel,
                                  (GtkWindow*)parent_toplevel);
  }

  menu->parent_menu_item    = parent_menu_item;
  menu->position_func       = func;
  menu->position_func_data  = data;
  menu_shell->activate_time = activate_time;

  /* We need to show the menu here rather in the init function because
   * code expects to be able to tell if the menu is onscreen by
   * looking at the gtk_widget_get_visible (menu)
   */
  gtk_widget_show ((GtkWidget*)menu);

  /* Position the menu, possibly changing the size request
   */
  geda_menu_position (menu, TRUE);

  /* Compute the size of the toplevel and realize it so we
   * can scroll correctly.
   */
  {
    GtkRequisition tmp_request;
    GtkAllocation tmp_allocation = { 0, };

    gtk_widget_size_request (menu->toplevel, &tmp_request);

    tmp_allocation.width = tmp_request.width;
    tmp_allocation.height = tmp_request.height;

    gtk_widget_size_allocate (menu->toplevel, &tmp_allocation);

    gtk_widget_realize ((GtkWidget*)menu);
  }

  geda_menu_scroll_to (menu, menu->scroll_offset);

  /* if no item is selected, select the first one */
  if (!menu_shell->active_menu_item) {

    if (menu->priv->touchscreen_mode) {
      geda_menu_shell_select_first (menu_shell, TRUE);
    }
  }

  /* Once everything is set up correctly, map the toplevel window
   * onto the screen.
   */
  gtk_widget_show (menu->toplevel);

  if (xgrab_shell == widget) {
    popup_grab_on_window (widget->window, activate_time, grab_keyboard); /* Should always succeed */
  }

  gtk_grab_add ((GtkWidget*)menu);

  if (parent_menu_shell) {

    bool  keyboard_mode;

    keyboard_mode = geda_menu_shell_get_keyboard_mode ((GedaMenuShell*)parent_menu_shell);
    geda_menu_shell_set_keyboard_mode (menu_shell, keyboard_mode);
  }
  else if (menu_shell->button == 0) { /* a keynav-activated context menu */
    geda_menu_shell_set_keyboard_mode (menu_shell, TRUE);
  }

  geda_menu_shell_update_mnemonics (menu_shell);
}

/*!
 * \brief Pop down a GedaMenu popup menu
 * \par Function Description
 * Removes the menu from the screen.
 *
 * \param[in] menu: a #GedaMenu
 */
void geda_menu_popdown (GedaMenu *menu)
{
  GedaMenuPriv  *private;
  GedaMenuShell *menu_shell;

  g_return_if_fail (GEDA_IS_MENU (menu));

  menu_shell = (GedaMenuShell*)menu;
  private    = menu->priv;

  menu_shell->parent_menu_shell = NULL;
  menu_shell->active            = FALSE;
  menu_shell->ignore_enter      = FALSE;

  private->have_position        = FALSE;
  private->seen_item_enter      = FALSE;

  geda_menu_stop_scrolling (menu);

  geda_menu_stop_navigating_submenu (menu);

  if (menu_shell->active_menu_item) {

    if (menu->old_active_menu_item) {
      g_object_unref (menu->old_active_menu_item);
    }

    menu->old_active_menu_item = menu_shell->active_menu_item;
    g_object_ref (menu->old_active_menu_item);

    geda_menu_shell_deselect (menu_shell);
  }

  /* The X Grab, if present, will automatically be removed when we hide
   * the window */
  if (menu->toplevel) {
    gtk_widget_hide (menu->toplevel);
    gtk_window_set_transient_for ((GtkWindow*)menu->toplevel, NULL);
  }

  if (menu->torn_off && menu->tearoff_window) {

    gtk_widget_set_size_request (menu->tearoff_window, -1, -1);

    if (GTK_BIN (menu->toplevel)->child) {

      geda_menu_reparent (menu, menu->tearoff_hbox, TRUE);
    }
    else {

      /* We popped up the menu from the tearoff, so we need to
       * release the grab - we aren't actually hiding the menu.
       */
      if (menu_shell->have_xgrab) {

        GdkDisplay *display = gtk_widget_get_display ((GtkWidget*)menu);

        gdk_display_pointer_ungrab (display, GDK_CURRENT_TIME);
        gdk_display_keyboard_ungrab (display, GDK_CURRENT_TIME);
      }
    }

    /* geda_menu_popdown is called each time a menu item is selected
     * from a torn off menu. Only scroll back to the saved position
     * if the non-tearoff menu was popped down.
     */
    if (!menu->tearoff_active) {
      geda_menu_scroll_to (menu, menu->saved_scroll_offset);
    }
    menu->tearoff_active = TRUE;
  }
  else {
    gtk_widget_hide ((GtkWidget*)menu);
  }

  menu_shell->have_xgrab = FALSE;

  gtk_grab_remove ((GtkWidget*)menu);

  menu_grab_transfer_window_destroy (menu);
}

/*!
 * \brief Get if the GedaMenu has an Active child
 * \par Function Description
 *  Position the menu according to its position function. Called
 *  from geda_menu_item.c::geda_menu_item_size_allocate when a
 *  menu-item changes its allocation.
 *
 * \param[in] menu Pointer to a #GedaMenu
 */
void geda_menu_reposition (GedaMenu *menu)
{
  g_return_if_fail (GEDA_IS_MENU (menu));

  if (!menu->torn_off && gtk_widget_is_drawable ((GtkWidget*)menu)) {
    geda_menu_position (menu, FALSE);
  }
}

/*!
 * \brief Get if the GedaMenu has an Active child
 * \par Function Description
 *  Returns the selected menu item from the menu. This is used by
 *  the #GedaOptionMenu.
 *
 * \param[in] menu: a #GedaMenu
 *
 * \returns GedaMenuItem that was last selected in the menu. If a selection
 *          has not yet been made, the first menu item is selected.
 */
GtkWidget *geda_menu_get_active (GedaMenu *menu)
{
  GtkWidget *child;
  GList     *children;

  g_return_val_if_fail (GEDA_IS_MENU (menu), NULL);

  if (!menu->old_active_menu_item) {

    child = NULL;
    children = ((GedaMenuShell*)menu)->children;

    while (children) {

      child = children->data;
      children = children->next;

      if (GTK_BIN (child)->child)
        break;
      child = NULL;
    }

    menu->old_active_menu_item = child;
    if (menu->old_active_menu_item)
      g_object_ref (menu->old_active_menu_item);
  }

  return menu->old_active_menu_item;
}

/*!
 * \brief Set the GedaMenu is Active menu item
 * \par Function Description
 *  Sets the menu item at \a index active in \a menu.
 *
 * \param[in] menu  Pointer to a #GedaMenu
 * \param[in] index Zero based index of the item to be set active
 */
void geda_menu_set_active (GedaMenu *menu, unsigned int index)
{
  GtkWidget *child;
  GList     *tmp_list;

  g_return_if_fail (GEDA_IS_MENU (menu));

  tmp_list = g_list_nth (((GedaMenuShell*)menu)->children, index);

  if (tmp_list) {

    child = tmp_list->data;

    if (GTK_BIN (child)->child) {

      if (menu->old_active_menu_item) {
        g_object_unref (menu->old_active_menu_item);
      }
      menu->old_active_menu_item = child;
      g_object_ref (menu->old_active_menu_item);
    }
  }
}

/*!
 * \brief Get if the GedaMenu Widget has an Active child
 * \par Function Description
 *  Wrapper for geda_menu_get_active that cast menu to a
 *  GedaMenu object.
 *
 * \param[in] menu: a #GedaMenu
 *
 * \returns GedaMenuItem that was last selected in the menu. If a selection
 *          has not yet been made, the first menu item is selected.
 *
 * \sa geda_menu_get_active
 */
GtkWidget *geda_menu_widget_get_active (GtkWidget *menu)
{
  return geda_menu_get_active ((GedaMenu*)menu);
}

/*!
 * \brief Set the GedaMenu Widget is Active menu item
 * \par Function Description
 *  Sets the menu item at \a index active in \a menu widget.
 *
 * \param[in] menu  Pointer to a #GedaMenu
 * \param[in] index Zero based index of the item to be set active
 *
 * \sa geda_menu_set_active
 */
void geda_menu_widget_set_active (GtkWidget *menu, unsigned int index)
{
  geda_menu_set_active ((GedaMenu*)menu, index);
}

/*!
 * \brief Gets a GedaMenu Accelerator Group
 * \par Function Description
 * Gets the GtkAccelGroup which holds global accelerators for the
 * menu.
 *
 * \param[in] menu a #GedaMenu
 *
 * \returns GtkAccelGroup associated with the menu.
 *
 * \sa geda_menu_set_accel_group().
 */
GtkAccelGroup *geda_menu_get_accel_group (GedaMenu *menu)
{
  g_return_val_if_fail (GEDA_IS_MENU (menu), NULL);

  return menu->accel_group;
}

/*!
 * \brief Sets a GedaMenu Accelerator Group
 * \par Function Description
 *  Set the accelerator group that holds global accelerators (should be
 *  added to the corresponding toplevel with gtk_window_add_accel_group().
 *
 * \param[in] menu         A GedaMenu
 * \param[in] accel_group  The accelerate group
 */
void geda_menu_set_accel_group (GedaMenu *menu, GtkAccelGroup *accel_group)
{
  g_return_if_fail (GEDA_IS_MENU (menu));

  if (menu->accel_group != accel_group) {

    /* Unreference the old group */
    if (menu->accel_group) {
      g_object_unref (menu->accel_group);
    }

    menu->accel_group = accel_group;

    /* Add a reference to the new group */
    if (accel_group) {
      g_object_ref (accel_group);
    }

    geda_menu_refresh_accel_paths (menu, TRUE);
  }
}

/*!
 * \brief Gets a GedaMenu Widget Accelerator Group
 * \par Function Description
 *  Widget wrapper for geda_menu_get_accel_group.
 *
 * \param[in] menu a #GedaMenu
 *
 * \returns GtkAccelGroup associated with the menu.
 *
 * \sa geda_menu_widget_set_accel_group().
 */
GtkAccelGroup *geda_menu_widget_get_accel_group (GtkWidget *menu)
{
  return geda_menu_get_accel_group ((GedaMenu*)menu);
}

/*!
 * \brief Sets the GedaMenu Widget Accelerator Group
 * \par Function Description
 *  Widget wrapper for geda_menu_set_accel_group.
 *
 * \param[in] menu         A GedaMenu
 * \param[in] accel_group  The accelerate group
 *
 * \sa
 */
void geda_menu_widget_set_accel_group (GtkWidget *menu, GtkAccelGroup *accel_group)
{
  geda_menu_set_accel_group ((GedaMenu*)menu, accel_group);
}

/*!
 * \brief Get the GedaMenu Widget accelerator path
 * \par Function Description
 *  Retrieves the accelerator path set on the menu widget.
 *
 * \param[in] menu: a valid #GedaMenu
 *
 * \returns accelerator path set on the menu.
 */
const char *geda_menu_widget_get_accel_path (GtkWidget *menu)
{
  return geda_menu_get_accel_path ((GedaMenu*)menu);
}

/*!
 * \brief Sets the GedaMenu Widget accelerator path
 * \par Function Description
 *  Sets an accelerator path for this menu from which accelerator paths
 *  for its immediate children, its menu items, can be constructed.
 * \see geda_menu_set_accel_path.
 *
 * \param[in] menu        a valid #GedaMenu
 * \param[in] accel_path  a valid accelerator path
 */
void geda_menu_widget_set_accel_path (GtkWidget *menu, const char *accel_path)
{
  geda_menu_set_accel_path ((GedaMenu*)menu, accel_path);
}

/*!
 * \brief Get the accelerator path
 * \par Function Description
 *  Retrieves the accelerator path set on the menu.
 *
 * \param[in] menu: a valid #GedaMenu
 *
 * \returns accelerator path set on the menu.
 */
const char *geda_menu_get_accel_path (GedaMenu *menu)
{
  g_return_val_if_fail (GEDA_IS_MENU (menu), NULL);

  return menu->accel_path;
}

/*!
 * \brief Sets an accelerator path
 * \par Function Description
 * Sets an accelerator path for this menu from which accelerator paths
 * for its immediate children, its menu items, can be constructed.
 * The main purpose of this function is to spare the programmer the
 * inconvenience of having to call geda_menu_item_set_accel_path() on
 * each menu item that should support runtime user changable accelerators.
 * Instead, by just calling geda_menu_set_accel_path() on their parent,
 * each menu item of this menu, that contains a label describing its purpose,
 * automatically gets an accel path assigned. For example, a menu containing
 * menu items "New" and "Exit", will, after
 * <b>geda_menu_set_accel_path (menu, "&lt;Gnumeric-Sheet&gt;/File");</b>
 * has been called, assign its items the accel paths:
 * <b>"&lt;Gnumeric-Sheet&gt;/File/New"</b> and <b>"&lt;Gnumeric-Sheet&gt;/File/Exit"</b>.
 *
 * \note that \a accel_path string will be stored in a GQuark. Therefore,
 *  if a static string is passed, some memory can be saved by interning
 *  the string first with g_intern_static_string().
 *
 * \param[in] menu        a valid #GedaMenu
 * \param[in] accel_path  a valid accelerator path
 */
void geda_menu_set_accel_path (GedaMenu *menu, const char *accel_path)
{
  g_return_if_fail (GEDA_IS_MENU (menu));

  if (accel_path) {
    g_return_if_fail (accel_path[0] == '<' && strchr (accel_path, '/')); /* simplistic check */
  }

  menu->accel_path = (char*)g_intern_string (accel_path);

  if (menu->accel_path) {
    geda_menu_refresh_accel_paths (menu, FALSE);
  }
}

typedef struct {
  GedaMenu *menu;
  bool  group_changed;
} AccelPropagation;

/*! \internal Update path prefix of children items */
static void refresh_accel_paths_foreach (GtkWidget *widget, void *data)
{
  if (GEDA_IS_MENU_ITEM (widget)) {  /* should always be true */

    AccelPropagation *prop = data;

    geda_menu_item_refresh_accel_path ((GedaMenuItem*)widget,
                                       prop->menu->accel_path,
                                       prop->menu->accel_group,
                                       prop->group_changed);
  }
}

/*!
 * \internal called by:
 *  geda_menu_set_accel_path
 *  geda_menu_show
 */
static void geda_menu_refresh_accel_paths (GedaMenu *menu, bool group_changed)
{
  g_return_if_fail (GEDA_IS_MENU (menu));

  if (menu->accel_path && menu->accel_group) {

    AccelPropagation prop;

    prop.menu          = menu;
    prop.group_changed = group_changed;

    geda_container_foreach (menu,
                            refresh_accel_paths_foreach,
                           &prop);
  }
}

static void geda_menu_scrollbar_changed (GtkAdjustment *adjustment, GedaMenu *menu)
{
  g_return_if_fail (GEDA_IS_MENU (menu));

  if (adjustment->value != menu->scroll_offset) {
    geda_menu_scroll_to (menu, adjustment->value);
  }
}

static void geda_menu_set_tearoff_hints (GedaMenu *menu, int width)
{
  GdkGeometry geometry_hints;

  if (!menu->tearoff_window) {
    return;
  }

  if (gtk_widget_get_visible (menu->tearoff_scrollbar)) {

    gtk_widget_size_request (menu->tearoff_scrollbar, NULL);
    width += menu->tearoff_scrollbar->requisition.width;
  }

  geometry_hints.min_width = width;
  geometry_hints.max_width = width;

  geometry_hints.min_height = 0;
  geometry_hints.max_height = ((GtkWidget*)menu)->requisition.height;

  gtk_window_set_geometry_hints ((GtkWindow*)menu->tearoff_window,
                                 NULL,
                                 &geometry_hints,
                                 GDK_HINT_MAX_SIZE|GDK_HINT_MIN_SIZE);
}

/*!
 * \brief get the Tear-Off state
 * \par Function Description
 *  Returns whether the menu is torn off.
 * \sa  geda_menu_set_tearoff_state ().
 *
 * \param[in] menu: a #GedaMenu
 *
 * \retval %TRUE if the menu is currently torn off.
 */
bool geda_menu_get_tearoff_state (GedaMenu *menu)
{
  g_return_val_if_fail (GEDA_IS_MENU (menu), FALSE);

  return menu->torn_off;
}

static void geda_menu_update_title (GedaMenu *menu)
{
  if (menu->tearoff_window) {

    const char *title = geda_menu_get_title (menu);

    if (!title) {

      GtkWidget  *attach_widget;

      attach_widget = geda_menu_get_attach_widget (menu);

      if (GEDA_IS_MENU_ITEM (attach_widget)) {

        GtkWidget *child = GTK_BIN (attach_widget)->child;

        if (GTK_IS_LABEL (child)) {
          title = gtk_label_get_text ((GtkLabel*)child);
        }
        else if (GEDA_IS_LABEL (child)) {
          title = geda_label_get_text ((GedaLabel*)child);
        }
      }
    }

    if (title) {
      gtk_window_set_title ((GtkWindow*)menu->tearoff_window, title);
    }
  }
}

static void tearoff_window_destroyed (GtkWidget *widget, GedaMenu *menu)
{
  geda_menu_set_tearoff_state (menu, FALSE);
}

/*!
 * \brief Set the Tear-Off state Property
 * \par Function Description
 *  Programmatically set whether the menu is torn off.
 * \sa  geda_menu_get_tearoff_state ().
 *
 * \param[in] menu      Pointer to a #GedaMenu
 * \param[in] torn_off  whether menu is to be torn off.
 */
void geda_menu_set_tearoff_state (GedaMenu *menu, bool torn_off)
{
  g_return_if_fail (GEDA_IS_MENU (menu));

  if (menu->torn_off != torn_off) {

    menu->torn_off = torn_off;
    menu->tearoff_active = torn_off;

    if (menu->torn_off) {

      GdkWindow *window;
      int        width;
      int        height;

      if (gtk_widget_get_visible ((GtkWidget*)menu)) {
        geda_menu_popdown (menu);
      }

      if (!menu->tearoff_window) {

        GtkWidget *toplevel;

        menu->tearoff_window = g_object_new (GTK_TYPE_WINDOW,
                                             "type", GTK_WINDOW_TOPLEVEL,
                                             "screen", gtk_widget_get_screen (menu->toplevel),
                                             "app-paintable", TRUE,
                                             NULL);

        gtk_window_set_type_hint (GTK_WINDOW (menu->tearoff_window),
                                  GDK_WINDOW_TYPE_HINT_MENU);
        gtk_window_set_mnemonic_modifier (GTK_WINDOW (menu->tearoff_window), 0);

        g_signal_connect (menu->tearoff_window, "destroy",
                          G_CALLBACK (tearoff_window_destroyed), menu);

        g_signal_connect (menu->tearoff_window, "event",
                          G_CALLBACK (geda_menu_window_event), menu);

        geda_menu_update_title (menu);

        gtk_widget_realize (menu->tearoff_window);

        toplevel = geda_menu_get_toplevel (menu);

        if (toplevel != NULL) {
          gtk_window_set_transient_for ((GtkWindow*)menu->tearoff_window,
                                        (GtkWindow*)toplevel);
        }

        menu->tearoff_hbox = gtk_hbox_new (FALSE, FALSE);
        geda_container_add (menu->tearoff_window, menu->tearoff_hbox);

        window = geda_get_widget_window(menu);

        if (window) {

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

          height = gdk_window_get_height (window);

#else

          gdk_drawable_get_size(window, &width, &height);

#endif
        }
        else {
          height = ((GtkWidget*)menu)->requisition.height;
        }

        menu->tearoff_adjustment =
                 geda_adjustment_new (0,
                                      0,
                                      ((GtkWidget*)menu)->requisition.height,
                                      MENU_SCROLL_STEP2,
                                      height >> 1, /* divide by 2 */
                                      height);

        g_object_connect (menu->tearoff_adjustment,
                          "signal::value-changed", geda_menu_scrollbar_changed, menu,
                          NULL);

        menu->tearoff_scrollbar = gtk_vscrollbar_new (menu->tearoff_adjustment);

        gtk_box_pack_end (GTK_BOX (menu->tearoff_hbox),
                          menu->tearoff_scrollbar,
                          FALSE, FALSE, 0);

        if (menu->tearoff_adjustment->upper > height) {
          gtk_widget_show (menu->tearoff_scrollbar);
        }

        gtk_widget_show (menu->tearoff_hbox);
      }

      geda_menu_reparent (menu, menu->tearoff_hbox, FALSE);

      window = geda_get_widget_window(menu);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

      width  = gdk_window_get_width (window);

#else

      gdk_drawable_get_size(window, &width, &height);

#endif

      /* Update menu->requisition */
      gtk_widget_size_request ((GtkWidget*)menu, NULL);

      geda_menu_set_tearoff_hints (menu, width);

      gtk_widget_realize (menu->tearoff_window);

      geda_menu_position (menu, TRUE);

      gtk_widget_show ((GtkWidget*)menu);
      gtk_widget_show (menu->tearoff_window);

      geda_menu_scroll_to (menu, 0);

    }
    else {

      gtk_widget_hide ((GtkWidget*)menu);
      gtk_widget_hide (menu->tearoff_window);
      if (GTK_IS_CONTAINER (menu->toplevel)) {
        geda_menu_reparent (menu, menu->toplevel, FALSE);
      }
      gtk_widget_destroy (menu->tearoff_window);

      menu->tearoff_window     = NULL;
      menu->tearoff_hbox       = NULL;
      menu->tearoff_scrollbar  = NULL;
      menu->tearoff_adjustment = NULL;
    }

    GEDA_OBJECT_NOTIFY (menu, "tearoff-state");
  }
}

/*!
 * \brief Retrieve the menu title
 * \par Function Description
 *  Returns the title of the menu. The string is owned by the widget
 *  and should not be modified or freed.
 *
 * \param[in] menu a #GedaMenu
 *
 * \return menu title or %NULL if the menu has no title has be set.
 *
 * \sa geda_menu_set_title().
 */
const char *geda_menu_get_title (GedaMenu *menu)
{
  GedaMenuPriv *priv;

  g_return_val_if_fail (GEDA_IS_MENU (menu), NULL);

  priv = menu->priv;

  return priv->title;
}

/*!
 * \brief Set the Title for the Menu
 * \par Function Description
 * Sets the title string for the menu. The title is displayed when the menu is
 * shown as a tearoff menu. If \a title is %NULL, the menu will see if it is
 * attached to a parent menu item, and if so it will try to use the same text
 * as that menu item's label.
 *
 * \param[in] menu  GedaMenu
 * \param[in] title string containing the title for the menu.
 */
void geda_menu_set_title (GedaMenu *menu, const char *title)
{
  GedaMenuPriv *priv;
  char *old_title;

  g_return_if_fail (GEDA_IS_MENU (menu));

  priv = menu->priv;

  old_title = priv->title;
  priv->title = geda_strdup (title);
  g_free (old_title);

  geda_menu_update_title (menu);
  GEDA_OBJECT_NOTIFY (menu, "tearoff-title");
}

/*!
 * \brief Get the Toplevel from a GedaMenu Object
 * \par Function Description
 *  Recursively ascends the menu structure until a non-menu
 *  container is found and returns gtk_widget_get_toplevel
 *  if this is a toplevel or NULL if no toplevel is found.
 *
 * \param[in] menu  GedaMenu
 */
GtkWidget *geda_menu_get_toplevel (GedaMenu *menu)
{
  GtkWidget *attach;

  attach = geda_menu_get_attach_widget (GEDA_MENU (menu));

  if (GEDA_IS_MENU_ITEM (attach)) {
    attach = attach->parent;
  }

  if (GEDA_IS_MENU (attach)) {
    return geda_menu_get_toplevel ((GedaMenu*)attach);
  }
  else if (GTK_IS_WIDGET (attach)) {

    GtkWidget *toplevel;

    toplevel = gtk_widget_get_toplevel (attach);

    if (gtk_widget_is_toplevel (toplevel)) {
      return toplevel;
    }
  }

  return NULL;
}

/*!
 * \brief Get GedaMenu parent
 * \par Function Description
 *  Returns the private member parent_menu_item as a Widget.
 *
 * \param [in] menu #GedaMenu Object whose parent is to be retrieved
 */
GtkWidget *geda_menu_get_parent_item (GedaMenu *menu)
{
  g_return_val_if_fail (GEDA_IS_MENU (menu), NULL);
  return menu->parent_menu_item;
}

/*!
 * \brief Set the GedaMenu parent menu item
 * \par Function Description
 *  Set the parent menu item of the GedaMenu object.
 */
void geda_menu_set_parent_item (GedaMenu *menu, GtkWidget *parent)
{
  g_return_if_fail (GEDA_IS_MENU (menu));
  menu->parent_menu_item = parent;
}

/*!
 * \brief Reorder a GedaMenu child widget
 * \par Function Description
 *  Relocates \a child in \a menu the new position. The first position
 *  is zero. No error is generated if child is not a member of the menu.
 *
 * \param [in] menu     #GedaMenu Object
 * \param [in] child    Widget is to be re-position
 * \param [in] position Zero based new position
 */
void geda_menu_reorder_child (GedaMenu *menu, GtkWidget *child, int position)
{
  GedaMenuShell *menu_shell;

  g_return_if_fail (GEDA_IS_MENU (menu));
  g_return_if_fail (GEDA_IS_MENU_ITEM (child));

  menu_shell = (GedaMenuShell*)menu;

  if (g_list_find (menu_shell->children, child)) {

      menu_shell->children = g_list_remove (menu_shell->children, child);
      menu_shell->children = g_list_insert (menu_shell->children, child, position);

      menu_queue_resize (menu);
  }
}

static void geda_menu_do_timeout_scroll (GedaMenu *menu, bool touchscreen_mode)
{
  bool  upper_visible;
  bool  lower_visible;

  upper_visible = menu->upper_arrow_visible;
  lower_visible = menu->lower_arrow_visible;

  geda_menu_scroll_by (menu, menu->scroll_step);

  if (touchscreen_mode &&
      (upper_visible != menu->upper_arrow_visible ||
       lower_visible != menu->lower_arrow_visible))
    {
      /* We are about to hide a scroll arrow while the mouse is pressed,
       * this would cause the uncovered menu item to be activated on
       * button release. Therefore we need to ignore button release here
       */
      ((GedaMenuShell*)menu)->ignore_enter = TRUE;
      menu->priv->ignore_button_release    = TRUE;
    }
}

static bool geda_menu_scroll_timeout (void *data)
{
  GedaMenu *menu;

  menu = (GedaMenu*)data;

  geda_menu_do_timeout_scroll (menu, menu->priv->touchscreen_mode);

  return TRUE;
}

static bool geda_menu_scroll_timeout_initial (void *data)
{
  GedaMenu    *menu;
  unsigned int timeout;

  menu = GEDA_MENU (data);

  g_object_get (gtk_widget_get_settings ((GtkWidget*)menu),
                "gtk-timeout-repeat", &timeout,
                NULL);

  geda_menu_do_timeout_scroll (menu, menu->priv->touchscreen_mode);

  geda_menu_remove_scroll_timeout (menu);

  menu->timeout_id = gdk_threads_add_timeout (timeout,
                                              geda_menu_scroll_timeout,
                                              menu);

  return FALSE;
}

static void geda_menu_start_scrolling (GedaMenu *menu)
{
  unsigned int timeout;

  g_object_get (gtk_widget_get_settings ((GtkWidget*)menu),
                "gtk-timeout-repeat", &timeout,
                NULL);

  geda_menu_do_timeout_scroll (menu, menu->priv->touchscreen_mode);

  menu->timeout_id = gdk_threads_add_timeout (timeout,
                                              geda_menu_scroll_timeout_initial,
                                              menu);
}

static void geda_menu_stop_navigating_submenu (GedaMenu *menu)
{
  GedaMenuPriv *priv = menu->priv;

  priv->navigation_x      = 0;
  priv->navigation_y      = 0;
  priv->navigation_width  = 0;
  priv->navigation_height = 0;

  if (menu->navigation_timeout) {
    g_source_remove (menu->navigation_timeout);
    menu->navigation_timeout = 0;
  }
}

/* When the timeout is elapsed, the navigation region is destroyed
 * and the menuitem under the pointer (if any) is selected.
 */
static bool geda_menu_stop_navigating_submenu_cb (void *user_data)
{
  GedaMenu *menu = user_data;

  geda_menu_stop_navigating_submenu (menu);

  if (gtk_widget_get_realized ((GtkWidget*)menu)) {

    GdkWindow *child_window;

    child_window = gdk_window_get_pointer (menu->bin_window, NULL, NULL, NULL);

    if (child_window) {

      GdkEvent *send_event = gdk_event_new (GDK_ENTER_NOTIFY);

      send_event->crossing.window     = g_object_ref (child_window);
      send_event->crossing.time       = GDK_CURRENT_TIME; /* Bogus */
      send_event->crossing.send_event = TRUE;

      ((GtkWidgetClass*)geda_menu_parent_class)->enter_notify_event ((GtkWidget*)menu, (GdkEventCrossing *)send_event);

      gdk_event_free (send_event);
    }
  }

  return FALSE;
}

static bool geda_menu_navigating_submenu (GedaMenu *menu, int event_x, int event_y)
{
  GedaMenuPriv *priv;
  int width, height;

  if (!geda_menu_has_navigation_triangle (menu)) {
    return FALSE;
  }

  priv   = menu->priv;
  width  = priv->navigation_width;
  height = priv->navigation_height;

  /* check if x/y are in the triangle spanned by the navigation parameters */

  /* 1) Move the coordinates so the triangle starts at 0,0 */
  event_x -= priv->navigation_x;
  event_y -= priv->navigation_y;

  /* 2) Ensure both legs move along the positive axis */
  if (width < 0) {
    event_x = -event_x;
    width = -width;
  }

  if (height < 0) {
    event_y = -event_y;
    height = -height;
  }

  /* 3) Check that the given coordinate is inside the triangle. The formula
   *    is a transformed form of this formula: x/w + y/h <= 1
   */
  if (event_x >= 0 && event_y >= 0 && event_x * height + event_y * width <= width * height)
  {
    return TRUE;
  }
  else
  {
    geda_menu_stop_navigating_submenu (menu);
    return FALSE;
  }
}

static void
geda_menu_set_submenu_navigation_region (GedaMenu         *menu,
                                         GedaMenuItem     *menu_item,
                                         GdkEventCrossing *event)
{
  GdkWindow    *window;
  GtkWidget    *event_widget;
  GtkWidget    *submenu;
  GedaMenuPriv *priv;

  int  submenu_left   = 0;
  int  submenu_right  = 0;
  int  submenu_top    = 0;
  int  submenu_bottom = 0;
  int  width          = 0;
  int  height         = 0;

  g_return_if_fail (event != NULL);

  submenu = geda_menu_item_get_submenu_widget(menu_item);

  if (!submenu)
    return;

  priv         = menu->priv;
  event_widget = gtk_get_event_widget ((GdkEvent*) event);

  gdk_window_get_origin (submenu->window, &submenu_left, &submenu_top);

  window =  geda_get_widget_window(submenu);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

  width  = gdk_window_get_width (window);
  height = gdk_window_get_height (window);

#else

  gdk_drawable_get_size(window, &width, &height);

#endif

  submenu_right  = submenu_left + width;
  submenu_bottom = submenu_top + height;

  window =  geda_get_widget_window(event_widget);

#ifdef HAVE_GDK_WINDOW_GET_WIDTH

  width  = gdk_window_get_width (window);

#else

  gdk_drawable_get_size(window, &width, &height);

#endif

  if (event->x >= 0 && event->x < width) {

    unsigned int popdown_delay;
    unsigned int submenu_direction;

    geda_menu_stop_navigating_submenu (menu);

    /* The navigation region is the triangle closest to the x/y
     * location of the rectangle. This is why the width or height
     * can be negative.
     */
    submenu_direction = geda_menu_item_get_submenu_direction(menu_item);

    if (submenu_direction == GTK_DIRECTION_RIGHT) {

      /* right */
      priv->navigation_x     = submenu_left;
      priv->navigation_width = event->x_root - submenu_left;
    }
    else {

      /* left */
      priv->navigation_x     = submenu_right;
      priv->navigation_width = event->x_root - submenu_right;
    }

    if (event->y < 0) {

      /* top */
      priv->navigation_y      = event->y_root;
      priv->navigation_height = submenu_top - event->y_root - NAVIGATION_REGION_OVERSHOOT;

      if (priv->navigation_height >= 0) {
        return;
      }
    }
    else {

      /* bottom */
      priv->navigation_y      = event->y_root;
      priv->navigation_height = submenu_bottom - event->y_root + NAVIGATION_REGION_OVERSHOOT;

      if (priv->navigation_height <= 0) {
        return;
      }
    }

    gtk_widget_style_get ((GtkWidget*)menu,
                         "menu-popdown-delay", &popdown_delay, NULL);

    menu->navigation_timeout = gdk_threads_add_timeout (popdown_delay,
                                                        geda_menu_stop_navigating_submenu_cb,
                                                        menu);
  }
}

static void geda_menu_position (GedaMenu *menu, bool set_scroll_offset)
{
  GtkWidget    *widget;
  GedaMenuPriv *private;
  GdkScreen    *screen;
  GdkScreen    *pointer_screen;
  int           x, y;
  int           scroll_offset;

  GdkRectangle    monitor;
  GtkRequisition  requisition;

  g_return_if_fail (GEDA_IS_MENU (menu));

  widget = (GtkWidget*)menu;

  screen = gtk_widget_get_screen (widget);

  gdk_display_get_pointer (gdk_screen_get_display (screen),
                           &pointer_screen, &x, &y, NULL);

  /* We need the requisition to figure out the right place to
   * popup the menu. In fact, we always need to ask here, since
   * if a size_request was queued while we weren't popped up,
   * the requisition won't have been recomputed yet.
   */
  gtk_widget_size_request (widget, &requisition);

  if (pointer_screen != screen) {

    /* Pointer is on a different screen; roughly center the
     * menu on the screen. If someone was using multiscreen
     * + Xinerama together they'd probably want something
     * fancier; but that is likely to be vanishingly rare.
     */
    x = MAX (0, (gdk_screen_get_width (screen) - requisition.width) >> 1); /* divide by 2 */
    y = MAX (0, (gdk_screen_get_height (screen) - requisition.height) >> 1); /* divide by 2 */
  }

  private = menu->priv;
  private->monitor_num = gdk_screen_get_monitor_at_point (screen, x, y);

  private->initially_pushed_in = FALSE;

  /* Set the type hint here to allow custom position functions to set a different hint */
  if (!gtk_widget_get_visible (menu->toplevel))  {
    gtk_window_set_type_hint ((GtkWindow*)menu->toplevel, GDK_WINDOW_TYPE_HINT_POPUP_MENU);
  }

  if (menu->position_func) {

    (* menu->position_func) (menu, &x, &y, &private->initially_pushed_in,
                             menu->position_func_data);

    if (private->monitor_num < 0) {
      private->monitor_num = gdk_screen_get_monitor_at_point (screen, x, y);
    }

    gdk_screen_get_monitor_geometry (screen, private->monitor_num, &monitor);
  }
  else {

    int  space_left, space_right, space_above, space_below;
    int  needed_width;
    int  needed_height;
    int  xthickness;
    int  ythickness;
    bool rtl;

    xthickness = widget->style->xthickness;
    ythickness = widget->style->ythickness;
    rtl        = (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL);

    /* The placement of popup menus horizontally works like this (with
     * RTL in parentheses)
     *
     * - If there is enough room to the right (left) of the mouse cursor,
     *   position the menu there.
     *
     * - Otherwise, if if there is enough room to the left (right) of the
     *   mouse cursor, position the menu there.
     *
     * - Otherwise if the menu is smaller than the monitor, position it
     *   on the side of the mouse cursor that has the most space available
     *
     * - Otherwise (if there is simply not enough room for the menu on the
     *   monitor), position it as far left (right) as possible.
     *
     * Positioning in the vertical direction is similar: first try below
     * mouse cursor, then above.
     */
    gdk_screen_get_monitor_geometry (screen, private->monitor_num, &monitor);

    space_left  = x - monitor.x;
    space_right = monitor.x + monitor.width - x - 1;
    space_above = y - monitor.y;
    space_below = monitor.y + monitor.height - y - 1;

    /* position horizontally */

    /* the amount of space we need to position the menu. Note the
     * menu is offset "xthickness" pixels
     */
    needed_width = requisition.width - xthickness;

    if (needed_width <= space_left ||
        needed_width <= space_right)
    {
      if ((rtl  && needed_width <= space_left) ||
         (!rtl && needed_width >  space_right))
      {
        /* position left */
        x = x + xthickness - requisition.width + 1;
      }
      else {

        /* position right */
        x = x - xthickness;
      }

      /* x is clamped on-screen further down */
    }
    else if (requisition.width <= monitor.width) {

      /* the menu is too big to fit on either side of the mouse
       * cursor, but smaller than the monitor. Position it on
       * the side that has the most space
       */
      if (space_left > space_right) {

        /* left justify */
        x = monitor.x;
      }
      else {

        /* right justify */
        x = monitor.x + monitor.width - requisition.width;
      }
    }
    else {/* menu is simply too big for the monitor */

      if (rtl) {

        /* right justify */
        x = monitor.x + monitor.width - requisition.width;
      }
      else {

        /* left justify */
        x = monitor.x;
      }
    }

    /* Position vertically. The algorithm is the same as above, but
     * simpler because we don't have to take RTL into account.
     */
    needed_height = requisition.height - ythickness;

    if (needed_height <= space_above ||
        needed_height <= space_below) {

      if (needed_height <= space_below) {
        y = y - ythickness;
      }
      else  {
        y = y + ythickness - requisition.height + 1;
      }

      y = CLAMP (y, monitor.y, monitor.y + monitor.height - requisition.height);
    }
    else if (needed_height > space_below && needed_height > space_above) {

      if (space_below >= space_above) {
        y = monitor.y + monitor.height - requisition.height;
      }
      else {
        y = monitor.y;
      }
    }
    else {
      y = monitor.y;
    }
  }

  scroll_offset = 0;

  if (private->initially_pushed_in) {

    int menu_height = ((GtkWidget*)menu)->requisition.height;

    if (y + menu_height > monitor.y + monitor.height) {
      scroll_offset -= y + menu_height - (monitor.y + monitor.height);
      y = (monitor.y + monitor.height) - menu_height;
    }

    if (y < monitor.y) {
      scroll_offset += monitor.y - y;
      y = monitor.y;
    }
  }

  x = CLAMP (x, monitor.x, MAX (monitor.x, monitor.x + monitor.width - requisition.width));

  if (((GedaMenuShell*)menu)->active) {

    private->have_position = TRUE;
    private->x = x;
    private->y = y;
  }

  if (y + requisition.height > monitor.y + monitor.height)
    requisition.height = (monitor.y + monitor.height) - y;

  if (y < monitor.y) {
    scroll_offset      += monitor.y - y;
    requisition.height -= monitor.y - y;
    y                   = monitor.y;
  }

  if (scroll_offset > 0) {

    GtkBorder arrow_border;

    get_arrows_border (menu, &arrow_border);
    scroll_offset += arrow_border.top;
  }

  if (((GedaMenuShell*)menu)->active) {

    gtk_window_move ((GtkWindow*)menu->toplevel, x, y);
  }
  else {

    GtkWindow *window = (GtkWindow*)menu->tearoff_window;

    gtk_window_move (window, x, y);

    gtk_window_resize (window, requisition.width, requisition.height);
  }

  if (set_scroll_offset) {
    menu->scroll_offset = scroll_offset;
  }
}

static void geda_menu_remove_scroll_timeout (GedaMenu *menu)
{
  if (menu->timeout_id) {
    g_source_remove (menu->timeout_id);
    menu->timeout_id = 0;
  }
}

static void geda_menu_stop_scrolling (GedaMenu *menu)
{
  geda_menu_remove_scroll_timeout (menu);

  if (!menu->priv->touchscreen_mode) {
    menu->upper_arrow_prelight = FALSE;
    menu->lower_arrow_prelight = FALSE;
  }
}

static void geda_menu_scroll_to (GedaMenu *menu, int offset)
{
  GtkWidget   *widget;
  unsigned int vertical_padding;
  unsigned int horizontal_padding;
  int          border_width;
  int          menu_height;
  int          view_width, view_height;
  int          x, y;
  bool         double_arrows;
  GtkBorder    arrow_border;

  widget = (GtkWidget*)menu;

  if (menu->tearoff_active &&
      menu->tearoff_adjustment &&
     (menu->tearoff_adjustment->value != offset))
  {
    menu->tearoff_adjustment->value =
      CLAMP (offset, 0, menu->tearoff_adjustment->upper -
                        menu->tearoff_adjustment->page_size);
    gtk_adjustment_value_changed (menu->tearoff_adjustment);
  }

  /* Move/resize the viewport according to arrows: */
  view_width  = widget->allocation.width;
  view_height = widget->allocation.height;

  gtk_widget_style_get (widget,
                        "vertical-padding", &vertical_padding,
                        "horizontal-padding", &horizontal_padding,
                        NULL);

  double_arrows = get_double_arrows (menu);

  border_width = ((GtkContainer*)menu)->border_width;
  view_width  -= (border_width + widget->style->xthickness + horizontal_padding) * 2;
  view_height -= (border_width + widget->style->ythickness + vertical_padding) * 2;
  menu_height  = widget->requisition.height -
                 (border_width + widget->style->ythickness + vertical_padding) * 2;

  x = border_width + widget->style->xthickness + horizontal_padding;
  y = border_width + widget->style->ythickness + vertical_padding;

  if (double_arrows && !menu->tearoff_active)  {

    if (view_height < menu_height              ||
       (offset > 0 && menu->scroll_offset > 0) ||
       (offset < 0 && menu->scroll_offset < 0))
    {
      GedaMenuPriv *priv = menu->priv;
      GtkStateType  upper_arrow_previous_state = priv->upper_arrow_state;
      GtkStateType  lower_arrow_previous_state = priv->lower_arrow_state;

      if (!menu->upper_arrow_visible || !menu->lower_arrow_visible) {
        gtk_widget_queue_draw (widget);
      }

      menu->upper_arrow_visible = menu->lower_arrow_visible = TRUE;

      get_arrows_border (menu, &arrow_border);
      y += arrow_border.top;
      view_height -= arrow_border.top;
      view_height -= arrow_border.bottom;

      if (offset <= 0)  {
        priv->upper_arrow_state = GTK_STATE_INSENSITIVE;
      }
      else if (priv->upper_arrow_state == GTK_STATE_INSENSITIVE) {
        priv->upper_arrow_state = menu->upper_arrow_prelight ?
                                  GTK_STATE_PRELIGHT : GTK_STATE_NORMAL;
      }

      if (offset >= menu_height - view_height) {
        priv->lower_arrow_state = GTK_STATE_INSENSITIVE;
      }
      else if (priv->lower_arrow_state == GTK_STATE_INSENSITIVE) {
        priv->lower_arrow_state = menu->lower_arrow_prelight ?
                                  GTK_STATE_PRELIGHT : GTK_STATE_NORMAL;
      }

      if ((priv->upper_arrow_state != upper_arrow_previous_state) ||
          (priv->lower_arrow_state != lower_arrow_previous_state))
      {
        gtk_widget_queue_draw (widget);
      }

      if (upper_arrow_previous_state != GTK_STATE_INSENSITIVE &&
          priv->upper_arrow_state == GTK_STATE_INSENSITIVE)
      {
        /* At the upper border, possibly remove timeout */
        if (menu->scroll_step < 0) {
          geda_menu_stop_scrolling (menu);
          gtk_widget_queue_draw (widget);
        }
      }

      if (lower_arrow_previous_state != GTK_STATE_INSENSITIVE &&
        priv->lower_arrow_state == GTK_STATE_INSENSITIVE)
      {
        /* At the lower border, possibly remove timeout */
        if (menu->scroll_step > 0) {
          geda_menu_stop_scrolling (menu);
          gtk_widget_queue_draw (widget);
        }
      }
    }
    else if (menu->upper_arrow_visible || menu->lower_arrow_visible)
    {
      offset = 0;

      menu->upper_arrow_visible = menu->lower_arrow_visible = FALSE;
      menu->upper_arrow_prelight = menu->lower_arrow_prelight = FALSE;

      geda_menu_stop_scrolling (menu);
      gtk_widget_queue_draw (widget);
    }
  }
  else if (!menu->tearoff_active) {

    bool  last_visible;

    last_visible = menu->upper_arrow_visible;
    menu->upper_arrow_visible = offset > 0;

    /* upper_arrow_visible may have changed, so requery the border */
    get_arrows_border (menu, &arrow_border);
    view_height -= arrow_border.top;

    if ((last_visible != menu->upper_arrow_visible) &&
        !menu->upper_arrow_visible)
    {
      menu->upper_arrow_prelight = FALSE;

      /* If we hid the upper arrow, possibly remove timeout */
      if (menu->scroll_step < 0) {
        geda_menu_stop_scrolling (menu);
        gtk_widget_queue_draw (widget);
      }
    }

    last_visible = menu->lower_arrow_visible;
    menu->lower_arrow_visible = offset < menu_height - view_height;

    /* lower_arrow_visible may have changed, so requery the border */
    get_arrows_border (menu, &arrow_border);
    view_height -= arrow_border.bottom;

    if ((last_visible != menu->lower_arrow_visible) &&
        !menu->lower_arrow_visible)
    {
      menu->lower_arrow_prelight = FALSE;

      /* If we hid the lower arrow, possibly remove timeout */
      if (menu->scroll_step > 0) {
        geda_menu_stop_scrolling (menu);
        gtk_widget_queue_draw (widget);
      }
    }

    y += arrow_border.top;
  }

  /* Scroll the menu: */
  if (gtk_widget_get_realized (widget))
    gdk_window_move (menu->bin_window, 0, -offset);

  if (gtk_widget_get_realized (widget)) {
    gdk_window_move_resize (menu->view_window, x, y,
                            view_width,
                            view_height);
  }

  menu->scroll_offset = offset;
}

/*!
 * \internal Reparent the menu, taking care of the refcounting
 *
 * If unrealize is true we force a unrealize while reparenting the parent.
 * This can help eliminate flicker in some cases.
 *
 * What happens is that when the menu is unrealized and then re-realized,
 * the allocations are as follows:
 *
 *  parent - 1x1 at (0,0)
 *  child1 - 100x20 at (0,0)
 *  child2 - 100x20 at (0,20)
 *  child3 - 100x20 at (0,40)
 *
 * That is, the parent is small but the children are full sized. Then,
 * when the queued_resize gets processed, the parent gets resized to
 * full size.
 *
 * But in order to eliminate flicker when scrolling, gdkgeometry-x11.c
 * contains the following logic:
 *
 * - if a move or resize operation on a window would change the clip
 *   region on the children, then before the window is resized
 *   the background for children is temporarily set to None, the
 *   move/resize done, and the background for the children restored.
 *
 * So, at the point where the parent is resized to final size, the
 * background for the children is temporarily None, and thus they
 * are not cleared to the background color and the previous background
 * (the image of the menu) is left in place.
 */
static void geda_menu_reparent (GedaMenu *menu, GtkWidget *new_parent, bool unrealize)
{
  GtkWidget *widget = (GtkWidget*)menu;
  bool was_floating = g_object_is_floating (menu);

  g_object_ref_sink (menu);

  if (unrealize) {

      g_object_ref (menu);
      geda_container_remove (widget->parent, widget);
      geda_container_add    (new_parent, widget);
      g_object_unref (menu);
  }
  else {
    gtk_widget_reparent (widget, new_parent);
  }

  if (was_floating) {
    g_object_force_floating ((GObject*)menu);
  }
  else {
    g_object_unref (menu);
  }
}

/*!
 * \brief Set the GedaMenu screen
 * \par Function Description
 *  Sets the GdkScreen on which the menu will be displayed. The screen
 *  number is stored using the key explicit_screen_key.
 *
 * \param[in] menu   GedaMenu.
 * \param[in] screen GdkScreen, or %NULL if the screen should be
 *                   determined by the widget the menu is attached to.
 */
void geda_menu_set_screen (GedaMenu *menu, GdkScreen *screen)
{
  g_return_if_fail (GEDA_IS_MENU (menu));
  g_return_if_fail (!screen || GDK_IS_SCREEN (screen));

  g_object_set_data ((GObject*)menu, explicit_screen_key, screen);

  if (screen) {

    menu_change_screen (menu, screen);
  }
  else {

    GtkWidget *attach_widget = geda_menu_get_attach_widget (menu);

    if (attach_widget) {
      attach_widget_screen_changed (attach_widget, NULL, menu);
    }
  }
}

/*!
 * \brief Set the number of monitors
 * \par Function Description
 * Adds a new #GedaMenuItem to a (table) menu. The number of 'cells' that
 * an item will occupy is specified by \a left_attach, \a right_attach,
 * \a top_attach and \a bottom_attach. These each represent the leftmost,
 * rightmost, uppermost and lower column and row numbers of the table.
 * (Columns and rows are indexed from zero).
 *
 * \note that this function is not related to geda_menu_detach().
 *
 * \param[in] menu           Pointer to a #GedaMenu.
 * \param[in] child          Pointer to a #GedaMenuItem.
 * \param[in] left_attach    Column number to attach the left side of the item to.
 * \param[in] right_attach   Column number to attach the right side of the item to.
 * \param[in] top_attach     Row number to attach the top of the item to.
 * \param[in] bottom_attach  Row number to attach the bottom of the item to.
 */
void geda_menu_attach (GedaMenu    *menu,
                       GtkWidget   *child,
                       unsigned int left_attach,
                       unsigned int right_attach,
                       unsigned int top_attach,
                       unsigned int bottom_attach)
{
  GedaMenuShell *menu_shell;

  g_return_if_fail (GEDA_IS_MENU (menu));
  g_return_if_fail (GEDA_IS_MENU_ITEM (child));
  g_return_if_fail (child->parent == NULL ||
                    child->parent == (GtkWidget*)menu);
  g_return_if_fail (left_attach < right_attach);
  g_return_if_fail (top_attach < bottom_attach);

  menu_shell = (GedaMenuShell*)menu;

  if (!child->parent) {

      AttachInfo *ai = get_attach_info (child);

      ai->left_attach   = left_attach;
      ai->right_attach  = right_attach;
      ai->top_attach    = top_attach;
      ai->bottom_attach = bottom_attach;

      menu_shell->children = g_list_append (menu_shell->children, child);

      gtk_widget_set_parent (child, (GtkWidget*)menu);

      menu_queue_resize (menu);
  }
  else {

      gtk_container_child_set ((GtkContainer*)child->parent, child,
                               "left-attach",   left_attach,
                               "right-attach",  right_attach,
                               "top-attach",    top_attach,
                               "bottom-attach", bottom_attach,
                               NULL);
  }
}

/*!
 * \brief Get number of monitors
 * \par Function Description
 * Retrieves the number of the monitor on which to show the menu.
 *
 * \param[in] menu  GedaMenu object
 *
 * \returns number of the monitor on which the menu should
 *          be popped up or -1, if no monitor has been set
 */
int geda_menu_get_monitor (GedaMenu *menu)
{
  GedaMenuPriv *priv;
  g_return_val_if_fail (GEDA_IS_MENU (menu), -1);

  priv = menu->priv;

  return priv->monitor_num;
}

/*!
 * \brief Set the number of monitors
 * \par Function Description
 * This function should be called from a #MenuPositionFunc if the
 * menu should not appear on the same monitor as the pointer. This
 * information can't be reliably inferred from the coordinates returned
 * by a #MenuPositionFunc, since, for very long menus, these coordinates
 * may extend beyond the monitor boundaries or even the screen boundaries.
 *
 * \sa gdk_screen_get_monitor_geometry().
 *
 * \param[in] menu         GedaMenu object
 * \param[in] monitor_num  Number of the monitor on which the menu should
 *                         appear.
 */
void geda_menu_set_monitor (GedaMenu *menu, int monitor_num)
{
  GedaMenuPriv *priv;
  g_return_if_fail (GEDA_IS_MENU (menu));

  priv = menu->priv;

  priv->monitor_num = monitor_num;
}

/*!
 * \brief Get list of menus attached to Widget.
 * \par Function Description
 * Returns a list of the menus which are attached to this widget.
 * This list is owned by the widget and must not be modified.
 *
 * \param[in] widget  GtkWidget object
 *
 * \returns list of menus attached to this widget.
 */
GList *geda_menu_get_for_attach_widget (GtkWidget *widget)
{
  GList *list;

  g_return_val_if_fail (GTK_IS_WIDGET (widget), NULL);

  list = GEDA_OBJECT_GET_DATA (widget, attached_menus_key);

  return list;
}

/*!
 * \brief Set GedaMenu Reserve Toggle Size.
 * \par Function Description
 * Sets whether the menu should reserve space for drawing toggles
 * or icons, regardless of their actual presence.
 *
 * \param[in] menu                 GedaMenu object
 * \param[in] reserve_toggle_size  TRUE is space should be reserved, otherwize FALSE
 */
void geda_menu_set_reserve_toggle_size (GedaMenu *menu,
                                         bool     reserve_toggle_size)
{
  GedaMenuPriv *priv = menu->priv;
  bool  no_toggle_size;

  no_toggle_size = !reserve_toggle_size;

  if (priv->no_toggle_size != no_toggle_size) {

      priv->no_toggle_size = no_toggle_size;

      GEDA_OBJECT_NOTIFY (menu, "reserve-toggle-size");
    }
}

/*!
 * \brief Get GedaMenu Reserve Toggle Size.
 * \par Function Description
 * Returns whether the menu reserves space for toggles and
 * icons, regardless of their actual presence.
 *
 * \param[in] menu  GedaMenu object
 *
 * \returns Whether the menu reserves toggle space
 */
bool geda_menu_get_reserve_toggle_size (GedaMenu *menu)
{
  GedaMenuPriv *priv = menu->priv;

  return !priv->no_toggle_size;
}

/*!
 * \brief Get GedaMenu widget Tear-Off state Property
 * \par Function Description
 *  Retrives the current tearoff state.
 *
 * \param[in] menu Pointer to a #GedaMenu
 */
bool geda_menu_widget_get_tearoff_state (GtkWidget *menu)
{
  return geda_menu_get_tearoff_state ((GedaMenu*)menu);
}

/*!
 * \brief Set GedaMenu widget Tear-Off state Property
 * \par Function Description
 *  Programmatically set whether the menu is torn off.
 * \sa  geda_menu_get_tearoff_state.
 *
 * \param[in] menu      Pointer to a #GedaMenu
 * \param[in] torn_off  whether menu is to be torn off.
 */
void geda_menu_widget_set_tearoff_state (GtkWidget *menu, bool torn_off)
{
  geda_menu_set_tearoff_state ((GedaMenu*)menu, torn_off);
}

/*!
 * \brief Get GedaMenu widget Title Property
 * \par Function Description
 *  Returns the title of the menu, do not free the returned string.
 * \sa  geda_menu_get_title ().
 *
 * \param[in] menu      Pointer to a #GedaMenu
 */
const char *geda_menu_widget_get_title (GtkWidget *menu)
{
  return geda_menu_get_title ((GedaMenu*)menu);
}

/*!
 * \brief Set GedaMenu widget Title Property
 * \par Function Description
 *  Sets the title string for the menu.
 * \sa  geda_menu_set_title.
 *
 * \param[in] menu   Pointer to a #GedaMenu
 * \param[in] title  string containing the title for the menu or NULL.
 */
void geda_menu_widget_set_title (GtkWidget *menu, const char *title)
{
  geda_menu_set_title ((GedaMenu*)menu, title);
}

/** @} end group geda-menu */
