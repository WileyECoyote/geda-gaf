/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA, <http://www.gnu.org/licenses/>.
 */
/*! \file geda_menu_bar.c
 *  \brief GedaMenuBar Class Module
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <ctype.h>

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_keysyms.h"
#include "../../include/geda_label.h"
#include "../../include/geda_menu_bar.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_uio_functions.h"
#include "../../include/gettext.h"

/** \defgroup geda-menu-bar A Menu Bar Object
 * @{
 * \brief Implmentation of GedaMenuBar Class
 * \par Description
 *  The #GedaMenuBar is a subclass of #GedaMenuShell which contains one or
 *  more #GedaMenuItem. The result is a standard menu bar which can hold
 *  many menu items.
 *
 * \sa #GedaMenuShell, #GedaMenu, #GedaMenuItem
 *
 * \class GedaMenuBar geda_menu_bar.h "include/geda_menu_bar.h"
 * \implements GedaMenuShell
 */

#define MENU_BAR_POPUP_DELAY 0
#define BORDER_SPACING  0
#define DEFAULT_IPADDING 1

/* Properties */
enum {
  PROP_0,
  PROP_PACK_DIRECTION,
  PROP_CHILD_PACK_DIRECTION
};

struct _GedaMenuBarPrivate
{
  PackDirection pack_direction;
  PackDirection child_pack_direction;

  unsigned int  settings_signal_id;
  GtkAccelGroup *accel_group;
  char          *accel;
};

static void geda_menu_bar_set_property        (GObject         *object,
                                               unsigned int     prop_id,
                                               const GValue    *value,
                                               GParamSpec      *pspec);
static void geda_menu_bar_get_property        (GObject         *object,
                                               unsigned int     prop_id,
                                               GValue          *value,
                                               GParamSpec      *pspec);

static void geda_menu_bar_size_allocate       (GtkWidget       *widget,
                                               GtkAllocation   *allocation);

static void geda_menu_bar_size_request        (GtkWidget       *widget,
                                               GtkRequisition  *requisition);
#if GTK_MAJOR_VERSION < 3

static int  geda_menu_bar_expose              (GtkWidget       *widget,
                                               GdkEventExpose  *event);
#else

static void geda_menu_bar_get_preferred_width         (GtkWidget      *widget,
                                                       int            *minimum,
                                                       int            *natural);
static void geda_menu_bar_get_preferred_height        (GtkWidget      *widget,
                                                       int            *minimum,
                                                       int            *natural);
static void geda_menu_bar_preferred_width_for_height  (GtkWidget      *widget,
                                                       int             height,
                                                       int            *minimum,
                                                       int            *natural);
static void geda_menu_bar_preferred_height_for_width  (GtkWidget      *widget,
                                                       int             width,
                                                       int            *minimum,
                                                       int            *natural);
static int geda_menu_bar_draw                         (GtkWidget      *widget,
                                                       cairo_t        *cr);
#endif

static bool geda_menu_bar_button_press                (GtkWidget      *widget,
                                                       GdkEventButton *event);
static bool geda_menu_bar_window_key_press_handler    (GtkWidget      *widget,
                                                       GdkEventKey    *event,
                                                       void           *data);
static bool geda_menu_bar_key_press                   (GtkWidget      *widget,
                                                       GdkEventKey    *event);
static void geda_menu_bar_hierarchy_changed           (GtkWidget      *widget,
                                                       GtkWidget      *old_toplevel);
static int  geda_menu_bar_get_popup_delay             (GedaMenuShell  *menu_shell);
static void geda_menu_bar_move_current                (GedaMenuShell  *menu_shell,
                                                       MenuDirection   direction);
static GtkShadowType get_shadow_type                  (GtkWidget      *menubar);

static const char menu_bar_list_key[] = "menu-bar-list";
static const char menu_bar_accel_key[] = "gtk-menu-bar-accel";

static void *geda_menu_bar_parent_class = NULL;

static GHashTable *menu_bar_hash = NULL;

static void change_accel (GedaMenuBar *menubar)
{
  GedaMenuBarPrivate *priv;
  GtkSettings *settings;
  char *accel;

  accel = NULL;

  priv = menubar->priv;

  settings = gtk_widget_get_settings ((GtkWidget*)menubar);

  g_object_get (settings, menu_bar_accel_key, &accel, NULL);

  if (accel && *accel) {

    if (priv->accel) {
      g_free(priv->accel);
    }
    priv->accel = geda_strdup(accel);
  }
  else {
    priv->accel = NULL;
  }
}

/* Callback used when a GtkSettings value changes */
static void settings_notify_cb (GObject     *object,
                                GParamSpec  *pspec,
                                GedaMenuBar *menubar)
{
  const char *name;

  name = g_param_spec_get_name (pspec);

  /* Check if menu-bar-accel is what was changed */
  if (!strcmp (name, menu_bar_accel_key)) {
    change_accel (menubar);
  }
}

static void connect_settings_signal(GedaMenuBar *menubar)
{
  GedaMenuBarPrivate *priv = menubar->priv;

  if (!priv->settings_signal_id) {

    GtkSettings *settings;
    GdkScreen   *screen;

    screen   = gtk_widget_get_screen ((GtkWidget*)menubar);
    settings = gtk_settings_get_for_screen (screen);

    priv->settings_signal_id = g_signal_connect (settings, "notify",
                                                 G_CALLBACK(settings_notify_cb),
                                                 menubar);
  }
}

/* Removes the settings signal handler. It's safe to call multiple times */
static void
remove_settings_signal (GedaMenuBar *menubar, GdkScreen *screen)
{
  GedaMenuBarPrivate *priv = menubar->priv;

  if (priv->settings_signal_id) {

    GtkSettings *settings;

    settings = gtk_settings_get_for_screen (screen);

    g_signal_handler_disconnect (settings, priv->settings_signal_id);

    priv->settings_signal_id = 0;
  }
}

/** \defgroup GedaMenuBar-GtkWidget GedaMenuBar GedaMenuBar Overrides
 *  @{
 */

/* Common helper called by:
 *
 * geda_menu_bar_size_allocate
 * geda_menu_bar_size_request
 * geda_menu_bar_expose
 * geda_menu_bar_draw
 *
 * \param [in] menubar a GedaMenuBar widget
 */
static GtkShadowType get_shadow_type (GtkWidget *menubar)
{
  GtkShadowType shadow_type = GTK_SHADOW_OUT;

  gtk_widget_style_get (menubar, "shadow-type", &shadow_type, NULL);

  return shadow_type;
}

#if GTK_MAJOR_VERSION < 3

/*! \internal Gtk2 widget_class->size_allocate */
static void
geda_menu_bar_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GedaMenuBar        *menu_bar;
  GedaMenuShell      *menu_shell;
  GedaMenuBarPrivate *priv;
  GList              *children;
  GtkAllocation       child_allocation;
  GtkRequisition      child_requisition;
  unsigned int        offset;
  GtkTextDirection    direction;
  int ltr_x, ltr_y;

  menu_bar   = (GedaMenuBar*)widget;
  menu_shell = (GedaMenuShell*)widget;
  priv       = menu_bar->priv;

  direction = gtk_widget_get_direction (widget);

  geda_set_widget_allocation (widget, allocation);

  if (gtk_widget_get_realized (widget)) {

    gdk_window_move_resize (geda_get_widget_window(widget),
                            allocation->x, allocation->y,
                            allocation->width, allocation->height);
  }

  if (menu_shell->children) {

    GtkWidget *child;
    int border_width;
    int ipadding;

    gtk_widget_style_get (widget, "internal-padding", &ipadding, NULL);

    border_width = geda_get_container_border_width (menu_bar);

    child_allocation.x = border_width + ipadding + BORDER_SPACING;
    child_allocation.y = border_width + BORDER_SPACING;

    if (get_shadow_type (widget) != GTK_SHADOW_NONE) {

      GtkStyle *style;

      style = geda_get_widget_style (widget);

      child_allocation.x += style->xthickness;
      child_allocation.y += style->ythickness;
    }

    children = menu_shell->children;

    if (priv->pack_direction == PACK_DIRECTION_LTR ||
        priv->pack_direction == PACK_DIRECTION_RTL)
    {
      child_allocation.height = MAX (1, (int)allocation->height - child_allocation.y * 2);

      offset = child_allocation.x;  /* Window edge to menubar start */
      ltr_x  = child_allocation.x;

      while (children) {

        int toggle_size;

        child    = children->data;
        children = children->next;

        geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child),
                                            &toggle_size);
        gtk_widget_get_child_requisition (child, &child_requisition);

        if (priv->child_pack_direction == PACK_DIRECTION_LTR ||
            priv->child_pack_direction == PACK_DIRECTION_RTL)
        {
          child_requisition.width += toggle_size;
        }
        else
        {
          child_requisition.height += toggle_size;
        }

        /* Support for the right justified help menu */
        if ((children == NULL) && (GEDA_IS_MENU_ITEM(child)) &&
            (geda_menu_item_get_right_justified(GEDA_MENU_ITEM(child))))
        {
          ltr_x = allocation->width -
          child_requisition.width - offset;
        }

        if (gtk_widget_get_visible (child)) {

          if ((direction == GTK_TEXT_DIR_LTR) == (priv->pack_direction == PACK_DIRECTION_LTR))
            child_allocation.x = ltr_x;
          else
            child_allocation.x = allocation->width -
            child_requisition.width - ltr_x;

          child_allocation.width = child_requisition.width;

          geda_menu_item_toggle_size_allocate (GEDA_MENU_ITEM (child),
                                               toggle_size);
          gtk_widget_size_allocate (child, &child_allocation);

          ltr_x += child_allocation.width;
        }
      }
    }
    else {

      child_allocation.width = MAX (1, (int)allocation->width - child_allocation.x * 2);

      offset = child_allocation.y;  /* Window edge to menubar start */
      ltr_y  = child_allocation.y;

      while (children) {

        int toggle_size;

        child = children->data;
        children = children->next;

        geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child),
                                            &toggle_size);
        gtk_widget_get_child_requisition (child, &child_requisition);

        if (priv->child_pack_direction == PACK_DIRECTION_LTR ||
          priv->child_pack_direction == PACK_DIRECTION_RTL)
        {
          child_requisition.width += toggle_size;
        }
        else
        {
          child_requisition.height += toggle_size;
        }

        /* Support for the right justified help menu */
        if ((children == NULL) && (GEDA_IS_MENU_ITEM(child)) &&
            (geda_menu_item_get_right_justified(GEDA_MENU_ITEM(child))))
        {
          ltr_y = allocation->height -
          child_requisition.height - offset;
        }

        if (gtk_widget_get_visible (child)) {

          if ((direction == GTK_TEXT_DIR_LTR) ==
            (priv->pack_direction == PACK_DIRECTION_TTB))
          {
            child_allocation.y = ltr_y;
          }
          else
          {
            child_allocation.y = allocation->height -
            child_requisition.height - ltr_y;
          }
          child_allocation.height = child_requisition.height;

          geda_menu_item_toggle_size_allocate (GEDA_MENU_ITEM (child),
                                               toggle_size);
          gtk_widget_size_allocate (child, &child_allocation);

          ltr_y += child_allocation.height;
        }
      }
    }
  }
}

static void
geda_menu_bar_size_request (GtkWidget *widget,  GtkRequisition *requisition)
{
  requisition->width = 0;
  requisition->height = 0;

  if (gtk_widget_get_visible (widget)) {

    GedaMenuBar        *menu_bar;
    GedaMenuBarPrivate *priv;
    GedaMenuShell      *menu_shell;
    GtkWidget          *child;
    GList              *children;
    GtkRequisition      child_requisition;

    int border_width;
    int ipadding;
    int nchildren;

    menu_bar   = (GedaMenuBar*)widget;
    menu_shell = (GedaMenuShell*)widget;
    priv       = menu_bar->priv;

    nchildren = 0;
    children = menu_shell->children;

    while (children) {

      child = children->data;
      children = children->next;

      if (gtk_widget_get_visible (child)) {

        int toggle_size;

        geda_menu_item_set_show_submenu_indicator(GEDA_MENU_ITEM (child), FALSE);

        gtk_widget_size_request (child, &child_requisition);
        geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child), &toggle_size);

        if (priv->child_pack_direction == PACK_DIRECTION_LTR ||
          priv->child_pack_direction == PACK_DIRECTION_RTL)
          child_requisition.width += toggle_size;
        else
          child_requisition.height += toggle_size;

        if (priv->pack_direction == PACK_DIRECTION_LTR ||
          priv->pack_direction == PACK_DIRECTION_RTL)
        {
          requisition->width += child_requisition.width;
          requisition->height = MAX (requisition->height, child_requisition.height);
        }
        else
        {
          requisition->width = MAX (requisition->width, child_requisition.width);
          requisition->height += child_requisition.height;
        }
        nchildren += 1;
      }
    }

    gtk_widget_style_get (widget, "internal-padding", &ipadding, NULL);

    border_width = geda_get_container_border_width (menu_bar);

    requisition->width  += (border_width + ipadding + BORDER_SPACING) << 1;
    requisition->height += (border_width + ipadding + BORDER_SPACING) << 1;

    if (get_shadow_type (widget) != GTK_SHADOW_NONE) {

      GtkStyle *style;

      style = geda_get_widget_style (widget);

      requisition->width  += style->xthickness << 1; /* Multiply by 2 */
      requisition->height += style->ythickness * 2;
    }
  }
}

/*! \internal Gtk2 widget_class->expose_event */
static int
geda_menu_bar_expose (GtkWidget *widget, GdkEventExpose *event)
{
  if (gtk_widget_is_drawable (widget)) {

    unsigned int border_width;

    border_width = geda_get_container_border_width (widget);

    gtk_paint_box (widget->style,
                   widget->window,
                   gtk_widget_get_state (widget),
                   get_shadow_type (widget),
                   &event->area, widget, "menubar",
                   border_width, border_width,
                   widget->allocation.width - border_width * 2,
                   widget->allocation.height - border_width * 2);

    GTK_WIDGET_CLASS (geda_menu_bar_parent_class)->expose_event (widget, event);
  }

  return FALSE;
}

#else

/*! \internal Gtk3 widget_class->size_allocate */
static void
geda_menu_bar_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GedaMenuShell *menu_shell = GEDA_MENU_SHELL (widget);

  gtk_widget_set_allocation (widget, allocation);

  if (gtk_widget_get_realized (widget)) {
    gdk_window_move_resize (gtk_widget_get_window (widget),
                            allocation->x, allocation->y,
                            allocation->width, allocation->height);
  }

  if (menu_shell->children) {

    GedaMenuBar        *menu_bar;
    GedaMenuBarPrivate *priv;
    Glist              *children;
    GList              *iter;
    GtkWidget          *child;
    GtkStyleContext    *context;
    GArray             *requested_sizes;
    GtkBorder           border;
    GtkStateFlags       flags;
    GtkAllocation       remaining_space;
    unsigned int        border_width;
    unsigned int        border_widthx2;
    unsigned int        toggle_size;

    menu_bar = GEDA_MENU_BAR (widget);
    priv     = menu_bar->priv;
    context  = gtk_widget_get_style_context (widget);
    flags    = gtk_widget_get_state_flags (widget);

    border_width   = geda_get_container_border_width (menu_bar);
    border_widthx2 = border_width << 1;

    gtk_style_context_get_padding (context, flags, &border);

    remaining_space.x      = (border_width + border.left);
    remaining_space.y      = (border_width + border.top);
    remaining_space.width  = allocation->width - border_widthx2 - border.left - border.right;
    remaining_space.height = allocation->height - border_widthx2 - border.top - border.bottom;

    if (get_shadow_type (widget) != GTK_SHADOW_NONE) {

      gtk_style_context_get_border (context, flags, &border);

      remaining_space.x      += border.left;
      remaining_space.y      += border.top;
      remaining_space.width  -= border.left + border.right;
      remaining_space.height -= border.top + border.bottom;
    }

    requested_sizes = g_array_new (FALSE, FALSE, sizeof(GtkRequestedSize));
    children        = menu_shell->priv->children;

    if (priv->pack_direction == PACK_DIRECTION_LTR ||
        priv->pack_direction == PACK_DIRECTION_RTL)
    {
      unsigned int i;
      int          size;
      bool         ltr;

      size = remaining_space.width;
      ltr  = (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) == (priv->pack_direction == PACK_DIRECTION_LTR);

      for (iter = list; children; children = children->next) {

        GtkRequestedSize request;

        child = iter->data;

        if (!gtk_widget_get_visible (child))
          continue;

        request.data = child;
        gtk_widget_get_preferred_width_for_height (child,
                                                   remaining_space.height,
                                                   &request.minimum_size,
                                                   &request.natural_size);
        geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child), &toggle_size);

        request.minimum_size += toggle_size;
        request.natural_size += toggle_size;

        geda_menu_item_toggle_size_allocate (GEDA_MENU_ITEM (child), toggle_size);

        g_array_append_val (requested_sizes, request);

        size -= request.minimum_size;
      }

      size = gtk_distribute_natural_allocation (size,
                                                requested_sizes->len,
                                                (GtkRequestedSize*)requested_sizes->data);

      for (i = 0; i < requested_sizes->len; i++) {

        GtkAllocation child_allocation = remaining_space;
        GtkRequestedSize *request = &g_array_index (requested_sizes, GtkRequestedSize, i);

        child_allocation.width = request->minimum_size;
        remaining_space.width -= request->minimum_size;

        if (i + 1 == requested_sizes->len && GEDA_IS_MENU_ITEM(request->data) &&
            GEDA_MENU_ITEM (request->data)->priv->right_justify)
          ltr = !ltr;

        if (ltr)
          remaining_space.x += request->minimum_size;
        else
          child_allocation.x += remaining_space.width;

        gtk_widget_size_allocate (request->data, &child_allocation);
      }
    }
    else {

      int    size;
      bool   ttb;

      size = remaining_space.height;
      ttb  = (priv->pack_direction == PACK_DIRECTION_TTB);

      for (iter = children; children; children = children->next) {

        GtkRequestedSize request;
        child = iter->data;

        if (!gtk_widget_get_visible (child)) {
          continue;
        }

        request.data = child;
        gtk_widget_get_preferred_height_for_width (child,
                                                   remaining_space.width,
                                                   &request.minimum_size,
                                                   &request.natural_size);
        geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child),
                                           &toggle_size);
        request.minimum_size += toggle_size;
        request.natural_size += toggle_size;

        geda_menu_item_toggle_size_allocate (GEDA_MENU_ITEM(child), toggle_size);

        g_array_append_val (requested_sizes, request);

        size -= request.minimum_size;
      }

      size = gtk_distribute_natural_allocation (size,
                                                requested_sizes->len,
                                                (GtkRequestedSize*)requested_sizes->data);

      for (i = 0; i < requested_sizes->len; i++) {

        GtkAllocation child_allocation = remaining_space;
        GtkRequestedSize *request = &g_array_index (requested_sizes, GtkRequestedSize, i);

        child_allocation.height = request->minimum_size;
        remaining_space.height -= request->minimum_size;

        if (i + 1 == requested_sizes->len && GEDA_IS_MENU_ITEM(request->data) &&
            GEDA_MENU_ITEM(request->data)->priv->right_justify)
        {
          ttb = !ttb;
        }

        if (ttb) {
          remaining_space.y  += request->minimum_size;
        }
        else {
          child_allocation.y += remaining_space.height;
        }

        gtk_widget_size_allocate (request->data, &child_allocation);
      }
    }

    g_array_free (requested_sizes, TRUE);
  }
}

/*! \internal Gtk3 widget_class->size_request */
static void
geda_menu_bar_size_request (GtkWidget      *widget,
                            GtkOrientation  orientation,
                            int             size,
                            int            *minimum,
                            int            *natural)
{
  GedaMenuBar        *menu_bar;
  GedaMenuBarPrivate *priv;
  GedaMenuShell      *menu_shell;
  GList              *children;
  GtkStyleContext    *context;
  GtkBorder           border;
  GtkStateFlags       flags;
  unsigned int        border_widthx2;
  bool                use_toggle_size;
  bool                use_maximize;
  int                 child_minimum;
  int                 child_natural;

  *minimum = 0;
  *natural = 0;

  menu_bar   = (GedaMenuBar*)widget;
  menu_shell = (GedaMenuShell*)widget;
  priv       = menu_bar->priv;

  children   = menu_shell->children;

  if (priv->child_pack_direction == PACK_DIRECTION_LTR ||
    priv->child_pack_direction == PACK_DIRECTION_RTL)
  {
    use_toggle_size = (orientation == GTK_ORIENTATION_HORIZONTAL);
  }
  else
  {
    use_toggle_size = (orientation == GTK_ORIENTATION_VERTICAL);
  }

  if (priv->pack_direction == PACK_DIRECTION_LTR ||
    priv->pack_direction == PACK_DIRECTION_RTL)
  {
    use_maximize = (orientation == GTK_ORIENTATION_VERTICAL);
  }
  else
  {
    use_maximize = (orientation == GTK_ORIENTATION_HORIZONTAL);
  }

  while (children) {

    GtkWidget *child;

    child    = children->data;
    children = children->next;

    if (gtk_widget_get_visible (child)) {

      _gtk_widget_get_preferred_size_for_size (child, orientation, size,
                                               &child_minimum,
                                               &child_natural, NULL, NULL);

      if (use_toggle_size) {

        int toggle_size;

        geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child),
                                            &toggle_size);

        child_minimum += toggle_size;
        child_natural += toggle_size;
      }

      if (use_maximize) {

        *minimum = MAX (*minimum, child_minimum);
        *natural = MAX (*natural, child_natural);
      }
      else {

        *minimum += child_minimum;
        *natural += child_natural;
      }
    }
  }

  context = gtk_widget_get_style_context (widget);
  flags   = gtk_widget_get_state_flags (widget);

  gtk_style_context_get_padding (context, flags, &border);

  if (orientation == GTK_ORIENTATION_HORIZONTAL) {

    *minimum += border.left + border.right;
    *natural += border.left + border.right;
  }
  else {

    *minimum += border.top + border.bottom;
    *natural += border.top + border.bottom;
  }

  border_widthx2 = gtk_container_get_border_width ((GtkContainer*)menu_bar) << 1;

  *minimum += border_widthx2;
  *natural += border_widthx2;

  if (get_shadow_type (widget) != GTK_SHADOW_NONE) {

    gtk_style_context_get_border (context, flags, &border);

    if (orientation == GTK_ORIENTATION_HORIZONTAL) {

      *minimum += border.left + border.right;
      *natural += border.left + border.right;
    }
    else {

      *minimum += border.top + border.bottom;
      *natural += border.top + border.bottom;
    }
  }
}

/*! \internal Gtk3 widget_class->preferred_width */
static void
geda_menu_bar_get_preferred_width (GtkWidget *widget,
                                   int       *minimum,
                                   int       *natural)
{
  geda_menu_bar_size_request (widget,
                              GTK_ORIENTATION_HORIZONTAL, -1, minimum, natural);
}

/*! \internal Gtk3 widget_class->preferred_height */
static void
geda_menu_bar_get_preferred_height (GtkWidget *widget,
                                    int       *minimum,
                                    int       *natural)
{
  geda_menu_bar_size_request (widget,
                              GTK_ORIENTATION_VERTICAL, -1, minimum, natural);
}

/*! \internal Gtk3 widget_class->width_for_height */
static void
geda_menu_bar_preferred_width_for_height (GtkWidget *widget,
                                              int        height,
                                              int       *minimum,
                                              int       *natural)
{
  geda_menu_bar_size_request (widget,
                              GTK_ORIENTATION_HORIZONTAL, height, minimum, natural);
}

/*! \internal Gtk3 widget_class->height_for_width */
static void
geda_menu_bar_preferred_height_for_width (GtkWidget *widget,
                                              int        width,
                                              int       *minimum,
                                              int       *natural)
{
  geda_menu_bar_size_request (widget,
                              GTK_ORIENTATION_VERTICAL, width, minimum, natural);
}

/*! \internal Gtk3 widget_class->draw */
static int geda_menu_bar_draw (GtkWidget *widget, cairo_t *cr)
{
  GtkStyleContext *context;
  int border;
  int borderx2;

  border   = gtk_container_get_border_width ((GtkContainer*)widget);
  borderx2 = border << 1;
  context  = gtk_widget_get_style_context (widget);

  gtk_render_background (context, cr,
                         border, border,
                         gtk_widget_get_allocated_width (widget) - borderx2,
                         gtk_widget_get_allocated_height (widget) - borderx2);

  if (get_shadow_type (widget) != GTK_SHADOW_NONE) {
    gtk_render_frame (context, cr,
                      border, border,
                      gtk_widget_get_allocated_width (widget) - borderx2,
                      gtk_widget_get_allocated_height (widget) - borderx2);
  }

  GTK_WIDGET_CLASS (geda_menu_bar_parent_class)->draw (widget, cr);

  return FALSE;
}

#endif

/** @} endgroup GedaMenuBar-GtkWidget */

static void
geda_menu_bar_get_property (GObject      *object,
                            unsigned int prop_id,
                            GValue       *value,
                            GParamSpec   *pspec)
{
  GedaMenuBar *menubar = GEDA_MENU_BAR (object);

  switch (prop_id) {

    case PROP_PACK_DIRECTION:
      g_value_set_enum (value, geda_menu_bar_get_pack_direction (menubar));
      break;

    case PROP_CHILD_PACK_DIRECTION:
      g_value_set_enum (value, geda_menu_bar_get_child_pack_direction (menubar));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
geda_menu_bar_set_property (GObject      *object,
                            unsigned int  prop_id,
                            const GValue *value,
                            GParamSpec   *pspec)
{
  GedaMenuBar *menubar = GEDA_MENU_BAR (object);

  switch (prop_id) {

    case PROP_PACK_DIRECTION:
      geda_menu_bar_set_pack_direction (menubar, g_value_get_enum (value));
      break;

    case PROP_CHILD_PACK_DIRECTION:
      geda_menu_bar_set_child_pack_direction (menubar, g_value_get_enum (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/*! \internal widget_class->dispose */
static void geda_menu_bar_dispose (GObject *object)
{
  GdkScreen *screen;

  screen = gtk_widget_get_screen (GTK_WIDGET (object));

  remove_settings_signal ((GedaMenuBar*)object, screen);

  G_OBJECT_CLASS (geda_menu_bar_parent_class)->dispose (object);
}

/*! \internal widget_class->finalize */
static void geda_menu_bar_finalize (GObject *object)
{
  GedaMenuBar *menu_bar = (GedaMenuBar*)object;

  if (g_hash_table_remove (menu_bar_hash, object)) {
    if (!g_hash_table_size (menu_bar_hash)) {
      g_hash_table_destroy (menu_bar_hash);
      menu_bar_hash = NULL;
    }
  }

  if (menu_bar->priv->accel) {
    g_free(menu_bar->priv->accel);
  }

  if (menu_bar->priv->accel_group) {
    g_object_unref(menu_bar->priv->accel_group);
  }

  g_free(menu_bar->priv);

  G_OBJECT_CLASS (geda_menu_bar_parent_class)->finalize (object);
}

/*!
 * \brief GedaMenuShell Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class      GedaMenuShell class we are initializing
 * \param [in]  class_data GedaMenuShell structure associated with the class
 */
static void geda_menu_bar_class_init (void *class, void *class_data)
{
  GObjectClass       *object_class;
  GtkWidgetClass     *widget_class;
  GedaMenuShellClass *menu_shell_class;
  GtkBindingSet      *binding_set;
  GParamSpec         *params;

  object_class     = (GObjectClass*) class;
  widget_class     = (GtkWidgetClass*) class;
  menu_shell_class = (GedaMenuShellClass*) class;

  object_class->get_property  = geda_menu_bar_get_property;
  object_class->set_property  = geda_menu_bar_set_property;
  object_class->dispose       = geda_menu_bar_dispose;
  object_class->finalize      = geda_menu_bar_finalize;

  widget_class->size_allocate = geda_menu_bar_size_allocate;

#if GTK_MAJOR_VERSION < 3

  widget_class->size_request  = geda_menu_bar_size_request;
  widget_class->expose_event  = geda_menu_bar_expose;

#else

  widget_class->get_preferred_width            = geda_menu_bar_get_preferred_width;
  widget_class->get_preferred_height           = geda_menu_bar_get_preferred_height;
  widget_class->get_preferred_width_for_height = geda_menu_bar_preferred_width_for_height;
  widget_class->get_preferred_height_for_width = geda_menu_bar_preferred_height_for_width;

  widget_class->draw                           = geda_menu_bar_draw;

#endif

  widget_class->hierarchy_changed              = geda_menu_bar_hierarchy_changed;
  widget_class->button_press_event             = geda_menu_bar_button_press;
  widget_class->key_press_event                = geda_menu_bar_key_press;

  //gtk_widget_class_set_accessible_role (widget_class, ATK_ROLE_MENU_BAR);

  menu_shell_class->submenu_placement = MENU_TOP_BOTTOM;
  menu_shell_class->get_popup_delay = geda_menu_bar_get_popup_delay;
  menu_shell_class->move_current = geda_menu_bar_move_current;

  geda_menu_bar_parent_class = g_type_class_peek_parent (class);

  binding_set = gtk_binding_set_by_class (class);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_Left, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_PREV);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_KP_Left, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_PREV);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_Right, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_NEXT);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_KP_Right, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_NEXT);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_Up, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_PARENT);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_KP_Up, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_PARENT);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_Down, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_CHILD);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_KP_Down, 0,
                                "move-current", 1,
                                G_TYPE_INT,
                                MENU_DIR_CHILD);

  /*!
   * GedaMenuBar:pack-direction:
   *
   * The pack direction of the menubar. It determines how
   * menuitems are arranged in the menubar.
   */
  params = g_param_spec_enum ("pack-direction",
                            _("Pack direction"),
                            _("The pack direction of the menubar"),
                               GTK_TYPE_PACK_DIRECTION,
                               PACK_DIRECTION_LTR,
                               G_PARAM_READWRITE);
  g_object_class_install_property (object_class,PROP_PACK_DIRECTION, params);

  /*!
   * GedaMenuBar:child-pack-direction:
   *
   * The child pack direction of the menubar. It determines how
   * the widgets contained in child menuitems are arranged.
   */
  params = g_param_spec_enum ("child-pack-direction",
                            _("Child Pack direction"),
                            _("The child pack direction of the menubar"),
                               GTK_TYPE_PACK_DIRECTION,
                               PACK_DIRECTION_LTR,
                               G_PARAM_READWRITE);

  g_object_class_install_property (object_class, PROP_CHILD_PACK_DIRECTION, params);

  params = g_param_spec_enum ("shadow-type",
                            _("Shadow type"),
                            _("Style of bevel around the menubar"),
                               GTK_TYPE_SHADOW_TYPE,
                               GTK_SHADOW_OUT,
                               G_PARAM_READABLE);
  gtk_widget_class_install_style_property (widget_class, params);

 /*!
  * GedaMenuBar:internal-padding:
  *
  * Amount of border space between the menubar shadow and the menu items
  */

  params = g_param_spec_int ("internal-padding",
                           _("Internal padding"),
                           _("Amount of border space between the menubar shadow and the menu items"),
                              0,
                              G_MAXINT,
                              0,
                              G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);
}

/*!
 * \brief Type instance initializer for GedaMenuShell
 * \par Function Description
 *  Type instance initializer for GedaMenuShell, initializes a new empty
 *  GedaMenuShell object.
 *
 * \param [in] instance The GedaMenuShell structure being initialized,
 * \param [in] class    The GedaMenuShell class being initializing.
 */
static void
geda_menu_bar_instance_init (GTypeInstance *instance, void *class)
{
  GedaMenuBar *menu_bar = (GedaMenuBar*)instance;

  menu_bar->priv = GEDA_MEM_ALLOC0 (sizeof(GedaMenuBarPrivate));

  if (!menu_bar_hash) {
    menu_bar_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (menu_bar_hash, instance, instance);

#if GTK_MAJOR_VERSION == 3

  GtkStyleContext *context;

  context = gtk_widget_get_style_context (GTK_WIDGET(menu_bar));
  gtk_style_context_add_class (context, GTK_STYLE_CLASS_MENUBAR);

#endif
}

/*!
 * \brief Retrieve GedaMenuBar's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaMenuBar Type identifier. When
 *  first called, the function registers a #GedaMenuBar in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaMenuBar and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \returns GedaType identifier associated with GedaMenuBar.
 */
GedaType geda_menu_bar_get_type (void)
{
  static volatile GedaType geda_menu_bar_type = 0;

  if (g_once_init_enter (&geda_menu_bar_type)) {

    static const GTypeInfo info = {
      sizeof(GedaMenuBarClass),
      NULL,                           /* base_init           */
      NULL,                           /* base_finalize       */
      geda_menu_bar_class_init,       /* (GClassInitFunc)    */
      NULL,                           /* class_finalize      */
      NULL,                           /* class_data          */
      sizeof(GedaMenuBar),
      0,                              /* n_preallocs         */
      geda_menu_bar_instance_init     /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaMenuBar");
    type   = g_type_register_static (GEDA_TYPE_MENU_SHELL, string, &info, 0);

    g_once_init_leave (&geda_menu_bar_type, type);
  }

  return geda_menu_bar_type;
}

/*!
 * \brief Check if an object is a GedaMenuBar
 * \par Function Description
 *  Determines if \a menu_bar is valid by verifying \a menu_bar
 *  is included in the hash table of GedaMenuBar objects.
 *
 * \returns TRUE if \a menu_bar is a valid GedaMenuBar object
 */
bool is_a_geda_menu_bar (GedaMenuBar *menu_bar)
{
  if ((menu_bar != NULL) && (menu_bar_hash != NULL)) {
    return g_hash_table_lookup(menu_bar_hash, menu_bar) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief Creates a new GedaMenuBar
 * \par Function Description
 *  Creates a new #GedaMenuBar object.
 *
 * \returns new menu bar, as a GtkWidget
 */
GtkWidget *geda_menu_bar_new (void)
{
  return g_object_new (GEDA_TYPE_MENU_BAR, NULL);
}

static GList *get_menu_bars (GtkWindow *window) {
  return g_object_get_data (G_OBJECT (window), menu_bar_list_key);
}

static void set_menu_bars (GtkWindow *window, GList *menubars) {
  g_object_set_data (G_OBJECT (window), menu_bar_list_key, menubars);
}

static void activate_child(GtkWidget *menu_item)
{
  if (geda_menu_item_is_widget_selectable(menu_item)) {

    /* The item is not activatable directly, so get parent */
    GtkWidget *parent = gtk_widget_get_parent(menu_item);

    if (GEDA_IS_MENU_SHELL(parent)) {

      GedaMenuShell *menu_shell = (GedaMenuShell*)parent;

      geda_menu_shell_activate(menu_shell);
      geda_menu_shell_set_keyboard_mode(menu_shell, TRUE);
      geda_menu_shell_select_item(menu_shell, menu_item);
    }
  }
}

/*! \internal container_foreach add_to_window */
static void
accelerate_children(GedaMenuItem *menu_item, GtkAccelGroup *accel_group)
{
  if (GEDA_IS_MENU_ITEM(menu_item)) {

    char mnemonic[2];

    mnemonic[0] = geda_menu_item_get_mnemonic(menu_item);

    if (mnemonic[0] > 0) {

      unsigned int keyval;

      mnemonic[0] = toupper(mnemonic[0]);
      mnemonic[1] = 0;

      keyval = gdk_keyval_from_name(&mnemonic[0]);

      if (keyval != GDK_KEY_VoidSymbol) {

        gtk_widget_add_accelerator((GtkWidget*)menu_item,
                                   "activate",
                                   accel_group,
                                   keyval,
                                   GDK_MOD1_MASK,
                                   0);

        g_signal_connect(menu_item, "activate",
                         G_CALLBACK(activate_child), NULL);
      }
    }
  }
}

static void add_to_window (GtkWindow *window, GedaMenuBar *menubar)
{
  GList *menubars = get_menu_bars (window);

  if (!menubars) {

    GtkAccelGroup *accel_group;
    GtkSettings   *settings;
    char          *accel;

    g_signal_connect (window, "key-press-event",
                      G_CALLBACK (geda_menu_bar_window_key_press_handler),
                      NULL);

    accel = NULL;
    settings = gtk_widget_get_settings ((GtkWidget*)menubar);

    g_object_get (settings, menu_bar_accel_key, &accel, NULL);

    accel_group = gtk_accel_group_new();
    gtk_window_add_accel_group (window, accel_group);

    if (menubar->priv->accel) {
      g_free (menubar->priv->accel);
    }
    menubar->priv->accel = accel;

    if (menubar->priv->accel_group) {
      g_object_unref (menubar->priv->accel_group);
    }
    menubar->priv->accel_group = accel_group;
  }

  /* Monitor if "gtk-menu-bar-accel" setting is changed */
  connect_settings_signal(menubar);

  /* Connect an accelerator to items on the Menu Bar */
  geda_container_foreach (menubar, accelerate_children,
                          menubar->priv->accel_group);

  set_menu_bars (window, g_list_append (menubars, menubar));
}

static void remove_from_window (GtkWindow *window, GedaMenuBar *menubar)
{
  GList *menubars = get_menu_bars (window);

  menubars = g_list_remove (menubars, menubar);

  if (!menubars) {

    if (menubar->priv->accel_group) {
      g_object_unref(menubar->priv->accel_group);
      menubar->priv->accel_group = NULL;
    }

    g_signal_handlers_disconnect_by_func (window,
                                          geda_menu_bar_window_key_press_handler,
                                          NULL);
  }

  /* Disconnect the settings Monitor */
  remove_settings_signal(menubar, gtk_widget_get_screen ((GtkWidget*)menubar));

  set_menu_bars (window, menubars);
}

/* button press event handler implements window-dragging using the
 * empty space on the menu bar if the parent shell does not handle
 * the event.
 */
static bool
geda_menu_bar_button_press (GtkWidget *widget, GdkEventButton *event)
{
  bool parent_response;

  if (event->type == GDK_2BUTTON_PRESS || event->type != GDK_BUTTON_PRESS)
    return FALSE;

  /* See if the GedaMenuShell handles the event */
  parent_response = ((GtkWidgetClass*)geda_menu_bar_parent_class)->
                                      button_press_event (widget, event);

  if (parent_response)
    return TRUE;

  /* The shell did not handle the event */

  if (event->button == 1) {

    GtkWidget *toplevel_widget = gtk_widget_get_toplevel (widget);

    if (GTK_WIDGET_TOPLEVEL (toplevel_widget)) {

      GdkWindow *toplevel;

      /* Toplevel to the toplevel should be main application window */
      toplevel = gdk_window_get_toplevel(toplevel_widget->window);

      if (toplevel != NULL) {

        gdk_window_begin_move_drag (toplevel, event->button,
                                    event->x_root,
                                    event->y_root,
                                    event->time);
        return TRUE;
      }
    }
    else {
      g_warning ("Root window of GedaMenuBar is not a top level window");
    }
  }

  return FALSE;
}

/* This function is executed when the subordinate menu shell
 * propagates an event using gtk_widget_event after determining
 * that the key press was not relevant to bindings nor children
 * of that shell.
 */
static bool geda_menu_bar_key_press (GtkWidget *widget, GdkEventKey *event)
{
  GedaMenuShell *menu_shell;

  menu_shell = (GedaMenuShell*)widget;

  bool handled = FALSE;

  if (menu_shell->children) {

    /* See geda_menu_bar_move_current */
    handled = gtk_bindings_activate_event ((GtkObject*)menu_shell, event);

    if (!handled) {

      GList *iter;

      for (iter = menu_shell->children; iter; iter = iter->next) {

        if (GEDA_IS_MENU_ITEM(iter->data)) {

          GedaMenuItem *menu_item = iter->data;

          char mnemonic = geda_menu_item_get_mnemonic(menu_item);
          char key_char = (char)event->keyval;

          if (((mnemonic >> 5) & 1) ^ 1) {
            mnemonic = tolower(mnemonic);
          }

          if (key_char == mnemonic){

            if (geda_menu_item_is_selectable(menu_item)) {
              geda_menu_item_activate_item(menu_item);
            }
            handled = TRUE;
            break;
          }
        }
      }
    }
  }

  return handled;
}

/* This function is called when a main window receives a key-press event
 * Consequently only accelerators here are checked since the destination
 * of the event is unknown.
 */
static bool
geda_menu_bar_window_key_press_handler (GtkWidget   *widget,
                                        GdkEventKey *event,
                                        void        *data)
{
  GList *bars;
  GList *menubars;
  bool   retval;

  retval   = FALSE;
  bars     = geda_menu_bar_get_viewable_menu_bars((GtkWindow*)widget);
  menubars = geda_container_focus_sort ((GtkContainer*)widget, bars,
                                         GTK_DIR_TAB_FORWARD, NULL);
  g_list_free (bars);

  if (menubars) {

    GedaMenuBar *menu_bar;
    unsigned int accel_mods;
    unsigned int default_mask;
    unsigned int event_mods;
    unsigned int keyval;
    char        *accel;

    GdkModifierType mods;

    keyval   = 0;
    mods     = 0;
    menu_bar = menubars->data;
    accel    = menu_bar->priv->accel;

    gtk_accelerator_parse (accel, &keyval, &mods);

    if (keyval == 0) {
      g_warning ("Failed to parse menu bar accelerator '%s'\n", accel);
    }

    default_mask = gtk_accelerator_get_default_mod_mask ();

    event_mods   = event->state & default_mask;
    accel_mods   = mods & default_mask;

    if ((event->keyval == keyval) && (event_mods == accel_mods)) {

      GedaMenuShell *menu_shell = (GedaMenuShell*)menu_bar;

      geda_menu_shell_set_keyboard_mode (menu_shell, TRUE);
      geda_menu_shell_activate (menu_shell);
      geda_menu_shell_select_first (menu_shell, FALSE);

      retval = TRUE;
    }
    g_list_free (menubars);
  }

  return retval;
}

static void
geda_menu_bar_hierarchy_changed (GtkWidget *widget, GtkWidget *old_toplevel)
{
  GtkWidget   *toplevel;
  GedaMenuBar *menubar;

  menubar = GEDA_MENU_BAR (widget);

  toplevel = gtk_widget_get_toplevel (widget);

  if (old_toplevel) {
    remove_from_window (GTK_WINDOW (old_toplevel), menubar);
  }

  if (gtk_widget_is_toplevel (toplevel)) {
    add_to_window ((GtkWindow*)toplevel, menubar);
  }
}

/*!
 * \brief Move GedaMenuBar Focus
 * \par Function Description
 *  Move the focus between menubars in the toplevel.
 *
 * \param[in] menubar Pointer #GedaMenuBar object
 * \param[in] dir     Direction in which to cycle the focus
 */
void geda_menu_bar_cycle_focus (GedaMenuBar *menubar, GtkDirectionType  dir)
{
  GtkWidget    *toplevel    = gtk_widget_get_toplevel (GTK_WIDGET(menubar));
  GedaMenuItem *to_activate = NULL;

  if (gtk_widget_is_toplevel (toplevel)) {

    GList *all_bars;
    GList *menubars;

    all_bars = geda_menu_bar_get_viewable_menu_bars ((GtkWindow*)toplevel);
    menubars = geda_container_focus_sort ((GtkContainer*)toplevel, all_bars,
                                           dir, (GtkWidget*)menubar);
    g_list_free (all_bars);

    if (menubars) {

      GList *current = g_list_find (menubars, menubar);

      if (current && current->next) {

        GedaMenuShell *new_menushell = (GedaMenuShell*)current->next->data;

        if (new_menushell->children) {
          to_activate = new_menushell->children->data;
        }
      }
    }

    g_list_free (menubars);
  }

  geda_menu_shell_cancel ((GedaMenuShell*)menubar);

  if (to_activate) {
    g_signal_emit_by_name (to_activate, "activate-item");
  }
}

static int geda_menu_bar_get_popup_delay (GedaMenuShell *menu_shell)
{
  return MENU_BAR_POPUP_DELAY;
}

/*!
 * \internal
 *  The MenuDirection for GedaMenuBar is not the same orientations
 *  as GedaMenuBar, for example MENU_DIR_PREV on a MenuBar is LEFT
 *  but on GedaMenu MENU_DIR_PREV is up. This function is initiated
 *  when a menu item is selected on a MenuBar and the user presses a
 *  key, then geda_menu_key_press() receives the event and chains up
 *  to parent::key_press_event, which is geda_menu_shell_key_press.
 *  Seeing that there is a parent shell, geda_menu_shell_key_press
 *  propagates the event by calling gtk_widget_event, which invokes
 *  geda_menu_bar_key_press. geda_menu_bar_key_press then calls
 *  gtk_bindings_activate_event before checking with children and
 *  thus we end up here since "move-current" has bindings pointing
 *  to geda_menu_bar_move_current.
 *
 *  The directions are translated here before chaining up to the
 *  parent class GedaMenushell::move_current so that the key press
 *  is ultimately handled by geda_real_menu_shell_move_current.
 *  Seems to be a bit messy but some how works in a round-about
 *  sort of way.
 */
static void
geda_menu_bar_move_current (GedaMenuShell *menu_shell, MenuDirection direction)
{
  GedaMenuBar *menubar = GEDA_MENU_BAR (menu_shell);
  GtkTextDirection text_dir;
  PackDirection pack_dir;

  text_dir = gtk_widget_get_direction ((GtkWidget*)menubar);
  pack_dir = geda_menu_bar_get_pack_direction (menubar);

  if (pack_dir == PACK_DIRECTION_LTR || pack_dir == PACK_DIRECTION_RTL)
  {
    if ((text_dir == GTK_TEXT_DIR_RTL) == (pack_dir == PACK_DIRECTION_LTR))
    {
      if (direction == MENU_DIR_PREV) {
        direction = MENU_DIR_NEXT;
      }
      else if (direction == MENU_DIR_NEXT) {
        direction = MENU_DIR_PREV;
      }
    }
  }
  else
  {
    switch (direction) {

      case MENU_DIR_PARENT:
        if ((text_dir == GTK_TEXT_DIR_LTR) == (pack_dir == PACK_DIRECTION_TTB))
          direction = MENU_DIR_PREV;
        else
          direction = MENU_DIR_NEXT;
        break;

      case MENU_DIR_CHILD:
        if ((text_dir == GTK_TEXT_DIR_LTR) == (pack_dir == PACK_DIRECTION_TTB))
          direction = MENU_DIR_NEXT;
        else
          direction = MENU_DIR_PREV;
        break;

      case MENU_DIR_PREV:
        if (text_dir == GTK_TEXT_DIR_RTL)
          direction = MENU_DIR_CHILD;
        else
          direction = MENU_DIR_PARENT;
        break;

      case MENU_DIR_NEXT:
        if (text_dir == GTK_TEXT_DIR_RTL)
          direction = MENU_DIR_PARENT;
        else
          direction = MENU_DIR_CHILD;
        break;

      default: ;
    }
  }

  GEDA_MENU_SHELL_CLASS (geda_menu_bar_parent_class)->move_current (menu_shell, direction);
}

/*!
 * \brief Retrieve the GedaMenuBar Pack Direction
 * \par Function Description
 *  Retrieves the current pack direction of the menubar.
 *
 * \param[in] menubar Pointer #GedaMenuBar object
 *
 * \returns the pack direction
 *
 * \sa geda_menu_bar_set_pack_direction
 */
PackDirection geda_menu_bar_get_pack_direction (GedaMenuBar *menubar)
{
  g_return_val_if_fail (GEDA_IS_MENU_BAR(menubar), PACK_DIRECTION_LTR);

  return menubar->priv->pack_direction;
}

/*!
 * \brief Set the Pack Direction
 * \par Function Description
 *  Setting to control how items should be packed inside a menubar.
 *
 * \param[in] menubar  Pointer #GedaMenuBar object
 * \param[in] pack_dir Pack Direction
 *
 * \sa geda_menu_bar_get_pack_direction
 */
void geda_menu_bar_set_pack_direction (GedaMenuBar   *menubar,
                                       PackDirection  pack_dir)
{
  GedaMenuBarPrivate *priv;
  GList *list;

  g_return_if_fail (GEDA_IS_MENU_BAR(menubar));

  priv = menubar->priv;

  if (priv->pack_direction != pack_dir) {

    priv->pack_direction = pack_dir;

    gtk_widget_queue_resize ((GtkWidget*)menubar);

    for (list = ((GedaMenuShell*)menubar)->children; list; list = list->next)
    {
      gtk_widget_queue_resize ((GtkWidget*)list->data);
    }

    GEDA_OBJECT_NOTIFY (menubar, "pack-direction");
  }
}

/*!
 * \brief Retrieve the GedaMenuBar Child Pack Direction
 * \par Function Description
 *  Retrieves the pack direction of the menubar's child widget.
 *
 * \param[in] menubar Pointer #GedaMenuBar object
 *
 * \returns the pack direction of the child
 *
 * \sa geda_menu_bar_set_child_pack_direction
 */
PackDirection geda_menu_bar_get_child_pack_direction (GedaMenuBar *menubar)
{
  g_return_val_if_fail (GEDA_IS_MENU_BAR(menubar), PACK_DIRECTION_LTR);

  return menubar->priv->child_pack_direction;
}

/*!
 * \brief Sets the GedaMenuBar Child Pack Direction
 * \par Function Description
 *  Sets how widgets should be packed inside the children of a menubar.
 *
 * \param[in] menubar        Pointer #GedaMenuBar object
 * \param[in] child_pack_dir #PackDirection
 *
 * \sa geda_menu_bar_set_pack_direction
 */
void geda_menu_bar_set_child_pack_direction (GedaMenuBar  *menubar,
                                             PackDirection child_pack_dir)
{
  GedaMenuBarPrivate *priv;
  GList *list;

  g_return_if_fail (GEDA_IS_MENU_BAR(menubar));

  priv = menubar->priv;

  if (priv->child_pack_direction != child_pack_dir) {

    priv->child_pack_direction = child_pack_dir;

    gtk_widget_queue_resize ((GtkWidget*)menubar);

    for (list = ((GedaMenuShell*)menubar)->children; list; list = list->next)
    {
      gtk_widget_queue_resize ((GtkWidget*)list->data);
    }
    GEDA_OBJECT_NOTIFY (menubar, "child-pack-direction");
  }
}

/*!
 * \brief Retrieve Viewable Menu Bars
 * \par Function Description
 *  Returns a list of mapped menu bar widgets.
 *
 * \param[in] window Pointer #GedaMenuBar object
 *
 * \returns list of mapped GedaMenuBar objects
 */
GList *geda_menu_bar_get_viewable_menu_bars (GtkWindow *window)
{
  GList *menu_bars;
  GList *viewable_menu_bars = NULL;

  for (menu_bars = get_menu_bars(window); menu_bars; menu_bars = menu_bars->next)
  {
    GtkWidget *widget = menu_bars->data;
    bool viewable = TRUE;

    while (widget) {

      if (!gtk_widget_get_mapped (widget))
        viewable = FALSE;

      widget = gtk_widget_get_parent (widget);
    }

    if (viewable) {
      viewable_menu_bars = g_list_prepend (viewable_menu_bars, menu_bars->data);
    }
  }

  return g_list_reverse (viewable_menu_bars);
}

/*! \internal helper for show/hide mnemonics */
static void
geda_menu_bar_set_label_mnemonic_visible (GedaMenuBar *menubar, bool state)
{
  if (GEDA_IS_MENU_SHELL(menubar)) {

    if (((GedaMenuShell*)menubar)->children) {

      GList *children = ((GedaMenuShell*)menubar)->children;
      GList *iter;

      for (iter = children; iter; iter = iter->next) {

        GedaMenuItem *menu_item = iter->data;

        if (GEDA_IS_MENU_ITEM(menu_item)) {

          GtkWidget *child;

          child = geda_get_child_widget (menu_item);

          if (GEDA_IS_LABEL(child)) {
            geda_label_set_mnemonic_visible ((GedaLabel*)child, state);
          }
        }
      }
    }
  }
}

/*!
 * \brief Hide Mnemonics on a GedaMenuBar
 * \par Function Description
 *  Hide the mnemonics on a GedaMenuBar.
 */
void geda_menu_bar_hide_mnemonics (GedaMenuBar *menubar)
{
   geda_menu_bar_set_label_mnemonic_visible (menubar, FALSE);
}

/*!
 * \brief Show Mnemonics on a GedaMenuBar
 * \par Function Description
 * Reveals the mnemonics on a GedaMenuBar.
 */
void geda_menu_bar_show_mnemonics (GedaMenuBar *menubar)
{
   geda_menu_bar_set_label_mnemonic_visible (menubar, TRUE);
}

/** @} geda-menu-bar */
