/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
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

/** \defgroup geda-menu-bar A menu bar widget
 * @{
 * \brief Implmentation of GedaMenuBar Class
 * \par Description
 *  The #GedaMenuBar is a subclass of #GedaMenuShell which contains one or
 *  more #GedaMenuItems. The result is a standard menu bar which can hold
 *  many menu items.
 *
 * \sa #GedaMenuShell, #GedaMenu, #GedaMenuItem
 *
 * \class GedaMenuBar geda_menu_bar.h "include/geda_menu_bar.h"
 * \implements GedaMenuShell
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_uio_functions.h"
#include "../../include/geda_keysyms.h"
#include "../../include/geda_menu_bar.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/gettext.h"

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

static void geda_menu_bar_paint               (GtkWidget       *widget,
                                               GdkRectangle    *area);
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

static void geda_menu_bar_hierarchy_changed           (GtkWidget      *widget,
                                                       GtkWidget      *old_toplevel);
static int  geda_menu_bar_get_popup_delay             (GedaMenuShell  *menu_shell);
static void geda_menu_bar_move_current                (GedaMenuShell  *menu_shell,
                                                       MenuDirection   direction);

static GtkShadowType get_shadow_type                  (GedaMenuBar    *menubar);

static void *geda_menu_bar_parent_class = NULL;

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

static void
geda_menu_bar_get_property (GObject    *object,
                            unsigned int       prop_id,
                            GValue     *value,
                            GParamSpec *pspec)
{
  GedaMenuBar *menubar = GEDA_MENU_BAR (object);

  switch (prop_id)
    {
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


#if GTK_MAJOR_VERSION < 3
static void
geda_menu_bar_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GedaMenuBar        *menu_bar;
  GedaMenuShell      *menu_shell;
  GedaMenuBarPrivate *priv;
  GtkWidget          *child;
  GList              *children;
  GtkAllocation       child_allocation;
  GtkRequisition      child_requisition;
  unsigned int        offset;
  GtkTextDirection    direction;
  int ltr_x, ltr_y;
  int ipadding;

  g_return_if_fail (GEDA_IS_MENU_BAR  (widget));
  g_return_if_fail (allocation != NULL);

  menu_bar   = GEDA_MENU_BAR (widget);
  menu_shell = GEDA_MENU_SHELL (widget);
  priv       = menu_bar->priv;

  direction = gtk_widget_get_direction (widget);

  widget->allocation = *allocation;
  if (gtk_widget_get_realized (widget))
    gdk_window_move_resize (widget->window,
                            allocation->x, allocation->y,
                            allocation->width, allocation->height);

    gtk_widget_style_get (widget, "internal-padding", &ipadding, NULL);

  if (menu_shell->children) {

    child_allocation.x = (GTK_CONTAINER (menu_bar)->border_width +
    ipadding +
    BORDER_SPACING);
    child_allocation.y = (GTK_CONTAINER (menu_bar)->border_width +
    BORDER_SPACING);

    if (get_shadow_type (menu_bar) != GTK_SHADOW_NONE)
    {
      child_allocation.x += widget->style->xthickness;
      child_allocation.y += widget->style->ythickness;
    }

    if (priv->pack_direction == PACK_DIRECTION_LTR ||
      priv->pack_direction == PACK_DIRECTION_RTL)
    {
      child_allocation.height = MAX (1, (int)allocation->height - child_allocation.y * 2);

      offset = child_allocation.x;  /* Window edge to menubar start */
      ltr_x = child_allocation.x;

      children = menu_shell->children;

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
        if ((children == NULL) && (GTK_IS_MENU_ITEM(child)) &&
            (geda_menu_item_get_right_justified(GEDA_MENU_ITEM(child))))
        {
          ltr_x = allocation->width -
          child_requisition.width - offset;
        }
        if (gtk_widget_get_visible (child))
        {
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

      children = menu_shell->children;

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
        if ((children == NULL) && (GTK_IS_MENU_ITEM(child)) &&
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
  GedaMenuBar        *menu_bar;
  GedaMenuBarPrivate *priv;
  GedaMenuShell      *menu_shell;
  GtkWidget          *child;
  GList              *children;
  GtkRequisition      child_requisition;
  int ipadding;
  int nchildren;

  g_return_if_fail (GEDA_IS_MENU_BAR (widget));
  g_return_if_fail (requisition != NULL);

  requisition->width = 0;
  requisition->height = 0;

  if (gtk_widget_get_visible (widget)) {

    menu_bar   = GEDA_MENU_BAR (widget);
    menu_shell = GEDA_MENU_SHELL (widget);
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

    requisition->width += (GTK_CONTAINER (menu_bar)->border_width +
    ipadding + BORDER_SPACING) * 2;
    requisition->height += (GTK_CONTAINER (menu_bar)->border_width +
    ipadding + BORDER_SPACING) * 2;

    if (get_shadow_type (menu_bar) != GTK_SHADOW_NONE)
    {
      requisition->width += widget->style->xthickness * 2;
      requisition->height += widget->style->ythickness * 2;
    }
  }
}

static void
geda_menu_bar_paint (GtkWidget *widget, GdkRectangle *area)
{
  g_return_if_fail (GEDA_IS_MENU_BAR (widget));

  if (gtk_widget_is_drawable (widget)) {

    int border;

    border = GTK_CONTAINER (widget)->border_width;

    gtk_paint_box (widget->style, widget->window,
                   gtk_widget_get_state (widget),
                   get_shadow_type (GEDA_MENU_BAR (widget)),
                   area, widget, "menubar",
                   border, border,
                   widget->allocation.width - border * 2,
                   widget->allocation.height - border * 2);
  }
}

static int
geda_menu_bar_expose (GtkWidget *widget, GdkEventExpose *event)
{
  g_return_val_if_fail (GEDA_IS_MENU_BAR (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if (gtk_widget_is_drawable (widget))
    {
      geda_menu_bar_paint (widget, &event->area);

      GTK_WIDGET_CLASS (geda_menu_bar_parent_class)->expose_event (widget, event);
    }

  return FALSE;
}

#else

static void
geda_menu_bar_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GedaMenuBar        *menu_bar;
  GedaMenuShell      *menu_shell;
  GedaMenuBarPrivate *priv;
  int                 toggle_size;

  g_return_if_fail (GEDA_IS_MENU_BAR (widget));
  g_return_if_fail (allocation != NULL);

  menu_bar   = GEDA_MENU_BAR (widget);
  menu_shell = GEDA_MENU_SHELL (widget);
  priv       = menu_bar->priv;

  gtk_widget_set_allocation (widget, allocation);

  if (gtk_widget_get_realized (widget)) {
    gdk_window_move_resize (gtk_widget_get_window (widget),
                            allocation->x, allocation->y,
                            allocation->width, allocation->height);
  }

  if (menu_shell->children) {

    Glist           *children;
    GList           *iter;
    GtkWidget       *child;
    GtkStyleContext *context;
    GArray          *requested_sizes;
    GtkBorder        border;
    GtkStateFlags    flags;
    GtkAllocation    remaining_space;
    unsigned int     border_width;
    unsigned int     border_widthx2;

    context = gtk_widget_get_style_context (widget);
    flags   = gtk_widget_get_state_flags (widget);

    gtk_style_context_get_padding (context, flags, &border);

    border_width   = gtk_container_get_border_width (GTK_CONTAINER (menu_bar));
    border_widthx2 = border_width << 1;

    remaining_space.x      = (border_width + border.left);
    remaining_space.y      = (border_width + border.top);
    remaining_space.width  = allocation->width - border_widthx2 - border.left - border.right;
    remaining_space.height = allocation->height - border_widthx2 - border.top - border.bottom;

    if (get_shadow_type (menu_bar) != GTK_SHADOW_NONE) {

      gtk_style_context_get_border (context, flags, &border);

      remaining_space.x      += border.left;
      remaining_space.y      += border.top;
      remaining_space.width  -= border.left + border.right;
      remaining_space.height -= border.top + border.bottom;
    }

    requested_sizes = g_array_new (FALSE, FALSE, sizeof (GtkRequestedSize));
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
        geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child),
                                            &toggle_size);
        request.minimum_size += toggle_size;
        request.natural_size += toggle_size;

        geda_menu_item_toggle_size_allocate (GEDA_MENU_ITEM (child), toggle_size);

        g_array_append_val (requested_sizes, request);

        size -= request.minimum_size;
      }

      size = gtk_distribute_natural_allocation (size,
                                                requested_sizes->len,
                                                (GtkRequestedSize *) requested_sizes->data);

      for (i = 0; i < requested_sizes->len; i++) {

        GtkAllocation child_allocation = remaining_space;
        GtkRequestedSize *request = &g_array_index (requested_sizes, GtkRequestedSize, i);

        child_allocation.width = request->minimum_size;
        remaining_space.width -= request->minimum_size;

        if (i + 1 == requested_sizes->len && GTK_IS_MENU_ITEM (request->data) &&
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

        if (!gtk_widget_get_visible (child))
          continue;

        request.data = child;
        gtk_widget_get_preferred_height_for_width (child,
                                                   remaining_space.width,
                                                   &request.minimum_size,
                                                   &request.natural_size);
        geda_menu_item_toggle_size_request (GEDA_MENU_ITEM (child),
                                           &toggle_size);
        request.minimum_size += toggle_size;
        request.natural_size += toggle_size;

        geda_menu_item_toggle_size_allocate (GEDA_MENU_ITEM (child), toggle_size);

        g_array_append_val (requested_sizes, request);

        size -= request.minimum_size;
      }

      size = gtk_distribute_natural_allocation (size,
                                                requested_sizes->len,
                                                (GtkRequestedSize *) requested_sizes->data);

      for (i = 0; i < requested_sizes->len; i++) {

        GtkAllocation child_allocation = remaining_space;
        GtkRequestedSize *request = &g_array_index (requested_sizes, GtkRequestedSize, i);

        child_allocation.height = request->minimum_size;
        remaining_space.height -= request->minimum_size;

        if (i + 1 == requested_sizes->len && GTK_IS_MENU_ITEM (request->data) &&
            GEDA_MENU_ITEM (request->data)->priv->right_justify)
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

static void
geda_menu_bar_size_request (GtkWidget      *widget,
                            GtkOrientation orientation,
                            int            size,
                            int           *minimum,
                            int           *natural)
{
  GedaMenuBar        *menu_bar;
  GedaMenuBarPrivate *priv;
  GedaMenuShell      *menu_shell;
  GtkWidget          *child;
  GList              *children;
  GtkStyleContext    *context;
  GtkBorder           border;
  GtkStateFlags       flags;
  unsigned int        border_width;
  bool                use_toggle_size;
  bool                use_maximize;
  int                 child_minimum;
  int                 child_natural;

  *minimum = 0;
  *natural = 0;

  menu_bar   = GEDA_MENU_BAR (widget);
  menu_shell = GEDA_MENU_SHELL (widget);
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

    child = children->data;
    children = children->next;

    if (gtk_widget_get_visible (child)) {

      _gtk_widget_get_preferred_size_for_size (child, orientation, size, &child_minimum, &child_natural, NULL, NULL);

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

  border_width = gtk_container_get_border_width (GTK_CONTAINER (menu_bar));
  *minimum += border_width * 2;
  *natural += border_width * 2;

  if (get_shadow_type (menu_bar) != GTK_SHADOW_NONE) {

    gtk_style_context_get_border (context, flags, &border);

    if (orientation == GTK_ORIENTATION_HORIZONTAL) {

      *minimum += border.left + border.right;
      *natural += border.left + border.right;
    }
    else
    {
      *minimum += border.top + border.bottom;
      *natural += border.top + border.bottom;
    }
  }
}

static void
geda_menu_bar_get_preferred_width (GtkWidget *widget,
                                   int       *minimum,
                                   int       *natural)
{
  geda_menu_bar_size_request (widget, GTK_ORIENTATION_HORIZONTAL, -1, minimum, natural);
}

static void
geda_menu_bar_get_preferred_height (GtkWidget *widget,
                                    int       *minimum,
                                    int       *natural)
{
  geda_menu_bar_size_request (widget, GTK_ORIENTATION_VERTICAL, -1, minimum, natural);
}

static void
geda_menu_bar_preferred_width_for_height (GtkWidget *widget,
                                              int        height,
                                              int       *minimum,
                                              int       *natural)
{
  geda_menu_bar_size_request (widget, GTK_ORIENTATION_HORIZONTAL, height, minimum, natural);
}

static void
geda_menu_bar_preferred_height_for_width (GtkWidget *widget,
                                              int        width,
                                              int       *minimum,
                                              int       *natural)
{
  geda_menu_bar_size_request (widget, GTK_ORIENTATION_VERTICAL, width, minimum, natural);
}

static int
geda_menu_bar_draw (GtkWidget *widget, cairo_t *cr)
{
  GtkStyleContext *context;
  int border;

  border  = gtk_container_get_border_width (GTK_CONTAINER (widget));
  context = gtk_widget_get_style_context (widget);

  gtk_render_background (context, cr,
                         border, border,
                         gtk_widget_get_allocated_width (widget) - border * 2,
                         gtk_widget_get_allocated_height (widget) - border * 2);

  if (get_shadow_type (GEDA_MENU_BAR (widget)) != GTK_SHADOW_NONE) {
    gtk_render_frame (context, cr,
                      border, border,
                      gtk_widget_get_allocated_width (widget) - border * 2,
                      gtk_widget_get_allocated_height (widget) - border * 2);
  }

  GTK_WIDGET_CLASS (geda_menu_bar_parent_class)->draw (widget, cr);

  return FALSE;
}
#endif

static void geda_menu_bar_finalize (GObject *object)
{
  GedaMenuBar *menu_bar = GEDA_MENU_BAR (object);

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
static void
geda_menu_bar_class_init (void *class, void *class_data)
{
  //(GedaMenuBarClass *class)
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

  //gtk_widget_class_set_accessible_role (widget_class, ATK_ROLE_MENU_BAR);

  menu_shell_class->submenu_placement = MENU_TOP_BOTTOM;
  menu_shell_class->get_popup_delay = geda_menu_bar_get_popup_delay;
  menu_shell_class->move_current = geda_menu_bar_move_current;

  geda_menu_bar_parent_class = g_type_class_peek_parent (class);

  binding_set = gtk_binding_set_by_class (class);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_Left, 0,
                                "move-current", 1,
                                GTK_TYPE_MENU_DIRECTION_TYPE,
                                MENU_DIR_PREV);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_KP_Left, 0,
                                "move-current", 1,
                                GTK_TYPE_MENU_DIRECTION_TYPE,
                                MENU_DIR_PREV);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_Right, 0,
                                "move-current", 1,
                                GTK_TYPE_MENU_DIRECTION_TYPE,
                                MENU_DIR_NEXT);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_KP_Right, 0,
                                "move-current", 1,
                                GTK_TYPE_MENU_DIRECTION_TYPE,
                                MENU_DIR_NEXT);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_Up, 0,
                                "move-current", 1,
                                GTK_TYPE_MENU_DIRECTION_TYPE,
                                MENU_DIR_PARENT);

  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_KP_Up, 0,
                                "move-current", 1,
                                GTK_TYPE_MENU_DIRECTION_TYPE,
                                MENU_DIR_PARENT);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_Down, 0,
                                "move-current", 1,
                                GTK_TYPE_MENU_DIRECTION_TYPE,
                                MENU_DIR_CHILD);
  gtk_binding_entry_add_signal (binding_set,
                                GDK_KEY_KP_Down, 0,
                                "move-current", 1,
                                GTK_TYPE_MENU_DIRECTION_TYPE,
                                MENU_DIR_CHILD);

  /**
   * GedaMenuBar:pack-direction:
   *
   * The pack direction of the menubar. It determines how
   * menuitems are arranged in the menubar.
   *
   * Since: 2.8
   */
  params = g_param_spec_enum ("pack-direction",
                            _("Pack direction"),
                            _("The pack direction of the menubar"),
                               GTK_TYPE_PACK_DIRECTION,
                               PACK_DIRECTION_LTR,
                               G_PARAM_READWRITE);
  g_object_class_install_property (object_class,PROP_PACK_DIRECTION, params);

  /**
   * GedaMenuBar:child-pack-direction:
   *
   * The child pack direction of the menubar. It determines how
   * the widgets contained in child menuitems are arranged.
   *
   * Since: 2.8
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

 /**
  * GedaMenuBar:internal-padding:
  *
  * Amount of border space between the menubar shadow and the menu items
  *
  * Recommened to use the standard padding CSS property (through objects
  * like #GtkStyleContext and #GtkCssProvider); the value of this style
  * property is ignored in 3.8.
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
  GedaMenuBar     *menu_bar = (GedaMenuBar*)instance;

  menu_bar->priv          = g_malloc0 (sizeof(GedaMenuBarPrivate));
  menu_bar->instance_type = geda_menu_bar_get_type();

#if GTK_MAJOR_VERSION == 3

  GtkStyleContext *context;

  context = gtk_widget_get_style_context (GTK_WIDGET (menu_bar));
  gtk_style_context_add_class (context, GTK_STYLE_CLASS_MENUBAR);

#endif
}

/*!
 * \brief Retrieve GedaMenuBar's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaMenuBar Type identifier. When
 *  first called, the function registers a #GedaMenuBar in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaMenuBar and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaMenuBar.
 */
GedaType
geda_menu_bar_get_type (void)
{
  static GedaType geda_menu_bar_type = 0;

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

bool is_a_geda_menu_bar (GedaMenuBar *menu_bar)
{
  if (G_IS_OBJECT(menu_bar)) {
    return (geda_menu_bar_get_type() == menu_bar->instance_type);
  }
  return FALSE;
}

/*!
 * \brief Creates a new GedaMenuBar
 * \par Function Description
 *  Creates a new #GedaMenuBar
 *
 * \returns new menu bar, as a GtkWidget
 */
GtkWidget*
geda_menu_bar_new (void)
{
  return g_object_new (GEDA_TYPE_MENU_BAR, NULL);
}

static GList*
get_menu_bars (GtkWindow *window)
{
  return g_object_get_data (G_OBJECT (window), "gtk-menu-bar-list");
}

GList*
geda_menu_bar_get_viewable_menu_bars (GtkWindow *window)
{
  GList *menu_bars;
  GList *viewable_menu_bars = NULL;

  for (menu_bars = get_menu_bars (window); menu_bars; menu_bars = menu_bars->next)
  {
      GtkWidget *widget = menu_bars->data;
      bool viewable = TRUE;

      while (widget) {

	  if (!gtk_widget_get_mapped (widget))
	    viewable = FALSE;

          widget = gtk_widget_get_parent (widget);
	}

      if (viewable)
	viewable_menu_bars = g_list_prepend (viewable_menu_bars, menu_bars->data);
    }

  return g_list_reverse (viewable_menu_bars);
}

static void
set_menu_bars (GtkWindow *window, GList     *menubars)
{
  g_object_set_data (G_OBJECT (window), _("gtk-menu-bar-list"), menubars);
}

static void
add_to_window (GtkWindow  *window, GedaMenuBar *menubar)
{
  GList *menubars = get_menu_bars (window);

  set_menu_bars (window, g_list_prepend (menubars, menubar));
}

static void
remove_from_window (GtkWindow  *window, GedaMenuBar *menubar)
{
  GList *menubars = get_menu_bars (window);

  menubars = g_list_remove (menubars, menubar);
  set_menu_bars (window, menubars);
}

static void
geda_menu_bar_hierarchy_changed (GtkWidget *widget, GtkWidget *old_toplevel)
{
  GtkWidget   *toplevel;
  GedaMenuBar *menubar;

  menubar = GEDA_MENU_BAR (widget);

  toplevel = gtk_widget_get_toplevel (widget);

  if (old_toplevel)
    remove_from_window (GTK_WINDOW (old_toplevel), menubar);

  if (gtk_widget_is_toplevel (toplevel))
    add_to_window (GTK_WINDOW (toplevel), menubar);
}

/*!
 * \brief Move GedaMenuBar Focus
 * \par Function Description
 * Move the focus between menubars in the toplevel.
 *
 * \param[in] menubar Pointer #GedaMenuBar object
 * \param[in] dir     Direction in which to cycle the focus
 */
void
geda_menu_bar_cycle_focus (GedaMenuBar *menubar, GtkDirectionType  dir)
{
  GtkWidget   *toplevel    = gtk_widget_get_toplevel (GTK_WIDGET (menubar));
  GedaMenuItem *to_activate = NULL;

  if (gtk_widget_is_toplevel (toplevel)) {

    GList *tmp_menubars = geda_menu_bar_get_viewable_menu_bars (GTK_WINDOW (toplevel));
    GList *menubars;
    GList *current;

    menubars = geda_container_focus_sort (GTK_CONTAINER (toplevel), tmp_menubars,
                                          dir, GTK_WIDGET (menubar));
    g_list_free (tmp_menubars);

    if (menubars) {

      current = g_list_find (menubars, menubar);

      if (current && current->next) {

        GedaMenuShell *new_menushell = GEDA_MENU_SHELL (current->next->data);
        if (new_menushell->children)
          to_activate = new_menushell->children->data;
      }
    }

    g_list_free (menubars);
  }

  geda_menu_shell_cancel (GEDA_MENU_SHELL (menubar));

  if (to_activate) {
    g_signal_emit_by_name (to_activate, "activate_item");
  }
}

static GtkShadowType
get_shadow_type (GedaMenuBar *menubar)
{
  GtkShadowType shadow_type = GTK_SHADOW_OUT;

  gtk_widget_style_get (GTK_WIDGET (menubar), "shadow-type", &shadow_type, NULL);

  return shadow_type;
}

static int
geda_menu_bar_get_popup_delay (GedaMenuShell *menu_shell)
{
  return MENU_BAR_POPUP_DELAY;
}

static void
geda_menu_bar_move_current (GedaMenuShell *menu_shell,
                            MenuDirection  direction)
{
  GedaMenuBar *menubar = GEDA_MENU_BAR (menu_shell);
  GtkTextDirection text_dir;
  PackDirection pack_dir;

  text_dir = gtk_widget_get_direction (GTK_WIDGET (menubar));
  pack_dir = geda_menu_bar_get_pack_direction (menubar);

  if (pack_dir == PACK_DIRECTION_LTR || pack_dir == PACK_DIRECTION_RTL)
  {
    if ((text_dir == GTK_TEXT_DIR_RTL) == (pack_dir == PACK_DIRECTION_LTR))
    {
      switch (direction)
      {
        case MENU_DIR_PREV:
          direction = MENU_DIR_NEXT;
          break;
        case MENU_DIR_NEXT:
          direction = MENU_DIR_PREV;
          break;
        default: ;
      }
    }
  }
  else
  {
    switch (direction)
    {
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
PackDirection
geda_menu_bar_get_pack_direction (GedaMenuBar *menubar)
{
  g_return_val_if_fail (GEDA_IS_MENU_BAR (menubar), PACK_DIRECTION_LTR);

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
void
geda_menu_bar_set_pack_direction (GedaMenuBar   *menubar,
                                  PackDirection  pack_dir)
{
  GedaMenuBarPrivate *priv;
  GList *list;

  g_return_if_fail (GEDA_IS_MENU_BAR (menubar));

  priv = menubar->priv;

  if (priv->pack_direction != pack_dir) {

    priv->pack_direction = pack_dir;

    gtk_widget_queue_resize (GTK_WIDGET (menubar));

    for (list = GEDA_MENU_SHELL (menubar)->children; list; list = list->next)
    {
      gtk_widget_queue_resize (GTK_WIDGET (list->data));
    }

    g_object_notify (G_OBJECT (menubar), "pack-direction");
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
PackDirection
geda_menu_bar_get_child_pack_direction (GedaMenuBar *menubar)
{
  g_return_val_if_fail (GEDA_IS_MENU_BAR (menubar), PACK_DIRECTION_LTR);

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
void
geda_menu_bar_set_child_pack_direction (GedaMenuBar   *menubar,
                                        PackDirection  child_pack_dir)
{
  GedaMenuBarPrivate *priv;
  GList *list;

  g_return_if_fail (GEDA_IS_MENU_BAR (menubar));

  priv = menubar->priv;

  if (priv->child_pack_direction != child_pack_dir) {

    priv->child_pack_direction = child_pack_dir;

    gtk_widget_queue_resize (GTK_WIDGET (menubar));

    for (list = GEDA_MENU_SHELL (menubar)->children; list; list = list->next)
    {
      gtk_widget_queue_resize (GTK_WIDGET (list->data));
    }
    g_object_notify (G_OBJECT (menubar), "child-pack-direction");
  }
}

/** @} geda-menu-bar */
