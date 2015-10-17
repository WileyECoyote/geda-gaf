/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_imagemenuitem.h
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 2001 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
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
 *
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <geda.h>
#include <geda_standard.h>
#include "geda_imagemenuitem.h"

#include <gtk/gtk.h>

#include "gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaImageMenuItem - A Menu Item with and Image Widget
 * \par
 * A GedaImageMenuItem is a replacement for the GtkMenuItem because the
 * GedaImageMenuItem is listed as depreciated. There is one major differences
 * between a GedaImageMenuItem and a GtkMenuItem, the GedaImageMenuItem does
 * NOT have a "gtk-show-image" property and does NOT enable the menu icons
 * based on system settings. The GtkMenuItem version displays images (after
 * being initialized not to do so) if the systems settings are set to display
 * images because GTK/Gnome apply their setting after programs show their
 * main window *and have already applied any program settings. This is
 * unacceptable behavior, and hence this widget exist as a replacement.
 * \par
 * The visibility is determined by the program alone. If desired, routines
 * can check the system settings and enable the icons as appropriate.
 *
 * \defgroup GedaImageMenuItem  Menu Item with an Image object
 * @{
 */

static void geda_image_menu_item_destroy              (GtkObject        *object);
static void geda_image_menu_item_size_request         (GtkWidget        *widget,
                                                       GtkRequisition   *requisition);
static void geda_image_menu_item_size_allocate        (GtkWidget        *widget,
                                                       GtkAllocation    *allocation);
static void geda_image_menu_item_map                  (GtkWidget        *widget);
static void geda_image_menu_item_remove               (GtkContainer     *container,
                                                       GtkWidget        *child);
static void geda_image_menu_item_toggle_size_request  (GtkMenuItem      *menu_item,
                                                       int              *requisition);
static void geda_image_menu_item_set_label            (GtkMenuItem      *menu_item,
                                                       const char       *label);
static const char *geda_image_menu_item_get_label     (GtkMenuItem      *menu_item);

static void geda_image_menu_item_forall               (GtkContainer     *container,
                                                       bool	         include_internals,
                                                       GtkCallback       callback,
                                                       void *            callback_data);

static void geda_image_menu_item_finalize             (GObject          *object);
static void geda_image_menu_item_set_property         (GObject          *object,
                                                       unsigned int      prop_id,
                                                       const GValue     *value,
                                                       GParamSpec       *pspec);
static void geda_image_menu_item_get_property         (GObject          *object,
                                                       unsigned int      prop_id,
                                                       GValue           *value,
                                                       GParamSpec       *pspec);
/*static void geda_image_menu_item_screen_changed       (GtkWidget        *widget,
                                                       GdkScreen        *previous_screen);
*/
static void geda_image_menu_item_recalculate          (GedaImageMenuItem *image_menu_item);

static void geda_image_menu_item_activatable_interface_init (GtkActivatableIface  *iface);
static void geda_image_menu_item_update                     (GtkActivatable       *activatable,
                                                             GtkAction            *action,
                                                             const char           *property_name);
static void geda_image_menu_item_sync_action_properties     (GtkActivatable       *activatable,
                                                             GtkAction            *action);

enum {
  PROP_0,
  PROP_IMAGE,
  PROP_USE_STOCK,
  PROP_ACCEL_GROUP,
  PROP_SHOW_IMAGE
};

static GtkActivatableIface *parent_activatable_iface;


G_DEFINE_TYPE_WITH_CODE (GedaImageMenuItem, geda_image_menu_item, GTK_TYPE_MENU_ITEM,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_ACTIVATABLE,
                                                geda_image_menu_item_activatable_interface_init))

/*! \brief Type class initializer for GedaImageMenuItem
 *
 *  \par Function Description
 *  Type class initializer for GedaImageMenuItem. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  param [in] klass  The instance of GedaActionClass to be initialized
 */
static void
geda_image_menu_item_class_init (GedaImageMenuItemClass *klass)
{
  GObjectClass      *gobject_class     = (GObjectClass*) klass;
  GtkObjectClass    *object_class      = (GtkObjectClass*) klass;
  GtkWidgetClass    *widget_class      = (GtkWidgetClass*) klass;
  GtkMenuItemClass  *menu_item_class   = (GtkMenuItemClass*) klass;
  GtkContainerClass *container_class   = (GtkContainerClass*) klass;

  object_class->destroy                = geda_image_menu_item_destroy;

  widget_class->size_request           = geda_image_menu_item_size_request;
  widget_class->size_allocate          = geda_image_menu_item_size_allocate;
  widget_class->map                    = geda_image_menu_item_map;

  container_class->forall              = geda_image_menu_item_forall;
  container_class->remove              = geda_image_menu_item_remove;

  menu_item_class->toggle_size_request = geda_image_menu_item_toggle_size_request;
  menu_item_class->set_label           = geda_image_menu_item_set_label;
  menu_item_class->get_label           = geda_image_menu_item_get_label;

  gobject_class->finalize              = geda_image_menu_item_finalize;
  gobject_class->set_property          = geda_image_menu_item_set_property;
  gobject_class->get_property          = geda_image_menu_item_get_property;

  g_object_class_install_property (gobject_class,
                                   PROP_IMAGE,
                                   g_param_spec_object ("image",
                                                      _("Image widget"),
                                                      _("Child widget to appear next to the menu text"),
                                                        GTK_TYPE_WIDGET,
                                                        G_PARAM_WRITABLE));
  /**
   * GedaImageMenuItem:use-stock:
   *
   * If %TRUE, the label set in the menuitem is used as a
   * stock id to select the stock item for the item.
   *
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_USE_STOCK,
                                   g_param_spec_boolean ("use-stock",
                                                       _("Use stock"),
                                                       _("Whether to use the label text to create a stock menu item"),
                                                         FALSE,
                                                         G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  /**
   * GedaImageMenuItem:show-image:
   *
   * If %TRUE, the menu item will ignore the GtkSettings:gtk-menu-images
   * setting and always show the image, if available.
   *
   * Use this property if the menuitem would be useless or hard to use
   * without the image.
   *
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_SHOW_IMAGE,
                                   g_param_spec_boolean ("show-image",
                                                       _("Show image"),
                                                       _("Whether the image will always be shown"),
                                                         FALSE,
                                                         G_PARAM_WRITABLE | G_PARAM_CONSTRUCT));

  /**
   * GedaImageMenuItem:accel-group:
   *
   * The Accel Group to use for stock accelerator keys
   *
   **/
  g_object_class_install_property (gobject_class,
                                   PROP_ACCEL_GROUP,
                                   g_param_spec_object ("accel-group",
                                                      _("Accel Group"),
                                                      _("The Accel Group to use for stock accelerator keys"),
                                                        GTK_TYPE_ACCEL_GROUP,
                                                        G_PARAM_WRITABLE));
}

/*! \brief Initialize GedaImageMenuItem data structure.
 *
 *  \par Function Description
 *  Function tois call after the GedaImageMenuItemClass is created
 *  to initialize the data structure.
 *
 * \param [in] image_menu_item A GedaImageMenuItem object (structure)
 */
static void
geda_image_menu_item_init (GedaImageMenuItem *image_menu_item)
{
  image_menu_item->use_stock = FALSE;
  image_menu_item->label     = NULL;
  image_menu_item->image     = NULL;
}

/*! \brief GedaImageMenuItem Object finalize handler
 *
 *  \par Function Description
 *  Just before the GtkImageMenuItem GObject is finalized, free our
 *  allocated data, and then chain up to the parent handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void
geda_image_menu_item_finalize (GObject *object)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem *)object;

  g_free (image_menu_item->label);
  image_menu_item->label  = NULL;

  G_OBJECT_CLASS (geda_image_menu_item_parent_class)->finalize (object);
}

/*! \brief GedaImageMenuItem GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GedaImageMenuItem's GObject properties
 *
 *  \param [in]  object    The GObject whose properties we are setting
 *  \param [in]  property  The numeric id. under which the property was
 *                         registered with g_object_class_install_property()
 *  \param [in]  value     The GValue the property is being set from
 *  \param [in]  pspec     A GParamSpec describing the property being set
 */
static void
geda_image_menu_item_set_property (GObject       *object,
                                   unsigned int   property,
                                   const GValue  *value,
                                   GParamSpec    *pspec)
{
  GedaImageMenuItem *image_menu_item = GEDA_IMAGE_MENU_ITEM (object);

  switch (property)
    {
    case PROP_IMAGE:
      geda_image_menu_item_set_image (image_menu_item, (GtkWidget *) g_value_get_object (value));
      break;
    case PROP_USE_STOCK:
      geda_image_menu_item_set_use_stock (image_menu_item, g_value_get_boolean (value));
      break;
    case PROP_SHOW_IMAGE:
      geda_image_menu_item_set_show_image (image_menu_item, g_value_get_boolean (value));
      break;
    case PROP_ACCEL_GROUP:
      geda_image_menu_item_set_accel_group (image_menu_item, g_value_get_object (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property, pspec);
      break;
    }
}

/*! \brief GedaImageMenuItem GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GedaImageMenuItem's GObject properties.
 *
 *  \param [in]  object    The GObject whose properties we are getting
 *  \param [in]  property  The numeric id. under which the property was
 *                         registered with g_object_class_install_property()
 *  \param [out] value     The GValue in which to return the value of the property
 *  \param [in]  pspec     A GParamSpec describing the property being got
 */
static void
geda_image_menu_item_get_property (GObject      *object,
                                   unsigned int  property,
                                   GValue       *value,
                                   GParamSpec   *pspec)
{
  GedaImageMenuItem *image_menu_item = GEDA_IMAGE_MENU_ITEM (object);

  switch (property)
    {
    case PROP_IMAGE:
      g_value_set_object (value, geda_image_menu_item_get_image (image_menu_item));
      break;
    case PROP_USE_STOCK:
      g_value_set_boolean (value, geda_image_menu_item_get_use_stock (image_menu_item));
      break;
    case PROP_SHOW_IMAGE:
      g_value_set_boolean (value, geda_image_menu_item_get_show_image (image_menu_item));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property, pspec);
      break;
    }
}

/*! \brief return internal show image property
 *
 *  \par Function Description
 *
 *  This is an internal function to return the show_image property.
 *  The function's address is supplied to external functions.
 *
 *  \param [in] image_menu_item A GedaImageMenuItem object data structure
 */
static bool
show_image (GedaImageMenuItem *image_menu_item)
{
  return image_menu_item->show_image;
}

static void
geda_image_menu_item_map (GtkWidget *widget)
{
  GedaImageMenuItem *image_menu_item = GEDA_IMAGE_MENU_ITEM (widget);

  GTK_WIDGET_CLASS (geda_image_menu_item_parent_class)->map (widget);

  if (image_menu_item->image)
    g_object_set (image_menu_item->image,
                  "visible", show_image (image_menu_item),
                  NULL);
}

static void
geda_image_menu_item_destroy (GtkObject *object)
{
  GedaImageMenuItem *image_menu_item = GEDA_IMAGE_MENU_ITEM (object);

  if (image_menu_item->image)
    gtk_container_remove (GTK_CONTAINER (image_menu_item),
                          image_menu_item->image);

  GTK_OBJECT_CLASS (geda_image_menu_item_parent_class)->destroy (object);
}

static void
geda_image_menu_item_toggle_size_request (GtkMenuItem *menu_item,
                                          int        *requisition)
{
  GedaImageMenuItem *image_menu_item = GEDA_IMAGE_MENU_ITEM (menu_item);
  GtkPackDirection pack_dir;

  if (GTK_IS_MENU_BAR (GTK_WIDGET (menu_item)->parent))
    pack_dir = gtk_menu_bar_get_child_pack_direction (GTK_MENU_BAR (GTK_WIDGET (menu_item)->parent));
  else
    pack_dir = GTK_PACK_DIRECTION_LTR;

  *requisition = 0;

  if (image_menu_item->image && gtk_widget_get_visible (image_menu_item->image))
  {
    GtkRequisition image_requisition;
    unsigned int toggle_spacing;
    gtk_widget_get_child_requisition (image_menu_item->image,
                                      &image_requisition);

    gtk_widget_style_get (GTK_WIDGET (menu_item),
                          "toggle-spacing", &toggle_spacing,
                          NULL);

    if (pack_dir == GTK_PACK_DIRECTION_LTR || pack_dir == GTK_PACK_DIRECTION_RTL)
    {
      if (image_requisition.width > 0) {
        *requisition = image_requisition.width + toggle_spacing;
      }
    }
    else
    {
      if (image_requisition.height > 0) {
        *requisition = image_requisition.height + toggle_spacing;
      }
    }
  }
}

static void
geda_image_menu_item_recalculate (GedaImageMenuItem *image_menu_item)
{
  //GedaImageMenuItemPrivate *priv = GET_PRIVATE (image_menu_item);
  GtkStockItem              stock_item;
  GtkWidget                *image;
  const char               *resolved_label = image_menu_item->label;

  if (image_menu_item->use_stock && image_menu_item->label) {

    if (!image_menu_item->image) {
      image = gtk_image_new_from_stock (image_menu_item->label, GTK_ICON_SIZE_MENU);
      geda_image_menu_item_set_image (image_menu_item, image);
    }

    if (gtk_stock_lookup (image_menu_item->label, &stock_item)) {
      resolved_label = stock_item.label;
    }

    gtk_menu_item_set_use_underline (GTK_MENU_ITEM (image_menu_item), TRUE);
  }

  GTK_MENU_ITEM_CLASS
  (geda_image_menu_item_parent_class)->
   set_label (GTK_MENU_ITEM (image_menu_item), resolved_label);

}

static void
geda_image_menu_item_set_label (GtkMenuItem *menu_item,
                                const char  *label)
{
  GedaImageMenuItem *image_menu_item = GEDA_IMAGE_MENU_ITEM (menu_item);

  if (image_menu_item->label != label) {

    g_free (image_menu_item->label);
    image_menu_item->label = g_strdup (label);

    geda_image_menu_item_recalculate (GEDA_IMAGE_MENU_ITEM (menu_item));

    g_object_notify (G_OBJECT (menu_item), "label");

  }
}

static const char *
geda_image_menu_item_get_label (GtkMenuItem *menu_item)
{
  return GEDA_IMAGE_MENU_ITEM (menu_item)->label;
}

static void
geda_image_menu_item_size_request (GtkWidget      *widget,
                                   GtkRequisition *requisition)
{
  GedaImageMenuItem *image_menu_item;
  int child_width = 0;
  int child_height = 0;
  GtkPackDirection pack_dir;

  if (GTK_IS_MENU_BAR (widget->parent))
    pack_dir = gtk_menu_bar_get_child_pack_direction (GTK_MENU_BAR (widget->parent));
  else
    pack_dir = GTK_PACK_DIRECTION_LTR;

  image_menu_item = GEDA_IMAGE_MENU_ITEM (widget);

  if (image_menu_item->image &&
      gtk_widget_get_visible (image_menu_item->image)) {
      GtkRequisition child_requisition;

      gtk_widget_size_request (image_menu_item->image,
                               &child_requisition);

      child_width = child_requisition.width;
      child_height = child_requisition.height;
  }

  GTK_WIDGET_CLASS (geda_image_menu_item_parent_class)->size_request (widget, requisition);

  /* not done with height since that happens via the
   * toggle_size_request
   */
  if (pack_dir == GTK_PACK_DIRECTION_LTR || pack_dir == GTK_PACK_DIRECTION_RTL) {
    requisition->height = MAX (requisition->height, child_height);
  }
  else {
    requisition->width = MAX (requisition->width, child_width);
  }

  /* Note that GtkMenuShell always size requests before
   * toggle_size_request, so toggle_size_request will be able to use
   * image_menu_item->image->requisition
   */
}

static void
geda_image_menu_item_size_allocate (GtkWidget     *widget,
                                    GtkAllocation *allocation)
{
  GedaImageMenuItem *image_menu_item;
  GtkPackDirection pack_dir;

  if (GTK_IS_MENU_BAR (widget->parent)) {
    pack_dir = gtk_menu_bar_get_child_pack_direction (GTK_MENU_BAR (widget->parent));
  }
  else {
    pack_dir = GTK_PACK_DIRECTION_LTR;
  }

  image_menu_item = GEDA_IMAGE_MENU_ITEM (widget);

  GTK_WIDGET_CLASS (geda_image_menu_item_parent_class)->size_allocate (widget, allocation);

  if (image_menu_item->image && gtk_widget_get_visible (image_menu_item->image))
  {
    int x, y, offset;
    GtkRequisition child_requisition;
    GtkAllocation child_allocation;
    unsigned int horizontal_padding, toggle_spacing;

    gtk_widget_style_get (widget,
                          "horizontal-padding", &horizontal_padding,
                          "toggle-spacing", &toggle_spacing,
                          NULL);

    /* Man this is lame hardcoding action, but I can't
     * come up with a solution that's really better.
     */

    gtk_widget_get_child_requisition (image_menu_item->image,
                                      &child_requisition);

    if (pack_dir == GTK_PACK_DIRECTION_LTR ||
        pack_dir == GTK_PACK_DIRECTION_RTL)
    {
      offset = GTK_CONTAINER (image_menu_item)->border_width +
      widget->style->xthickness;

      if ((gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) ==
        (pack_dir == GTK_PACK_DIRECTION_LTR))
        x = offset + horizontal_padding +
        (GTK_MENU_ITEM (image_menu_item)->toggle_size -
        toggle_spacing - child_requisition.width) / 2;
      else
        x = widget->allocation.width - offset - horizontal_padding -
        GTK_MENU_ITEM (image_menu_item)->toggle_size + toggle_spacing +
        (GTK_MENU_ITEM (image_menu_item)->toggle_size -
        toggle_spacing - child_requisition.width) / 2;

      y = (widget->allocation.height - child_requisition.height) / 2;
    }
    else {
      offset = GTK_CONTAINER (image_menu_item)->border_width +
      widget->style->ythickness;

      if ((gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) ==
        (pack_dir == GTK_PACK_DIRECTION_TTB))
        y = offset + horizontal_padding +
        (GTK_MENU_ITEM (image_menu_item)->toggle_size -
        toggle_spacing - child_requisition.height) / 2;
      else
        y = widget->allocation.height - offset - horizontal_padding -
        GTK_MENU_ITEM (image_menu_item)->toggle_size + toggle_spacing +
        (GTK_MENU_ITEM (image_menu_item)->toggle_size -
        toggle_spacing - child_requisition.height) / 2;

      x = (widget->allocation.width - child_requisition.width) / 2;
    }

    child_allocation.width = child_requisition.width;
    child_allocation.height = child_requisition.height;
    child_allocation.x = widget->allocation.x + MAX (x, 0);
    child_allocation.y = widget->allocation.y + MAX (y, 0);

    gtk_widget_size_allocate (image_menu_item->image, &child_allocation);
  }
}

static void
geda_image_menu_item_forall (GtkContainer   *container,
                            bool	    include_internals,
                            GtkCallback     callback,
                            void *        callback_data)
{
  GedaImageMenuItem *image_menu_item = GEDA_IMAGE_MENU_ITEM (container);

  GTK_CONTAINER_CLASS (geda_image_menu_item_parent_class)->forall (container,
                                                                  include_internals,
                                                                  callback,
                                                                  callback_data);

  if (include_internals && image_menu_item->image)
    (* callback) (image_menu_item->image, callback_data);
}


static void
geda_image_menu_item_activatable_interface_init (GtkActivatableIface  *iface)
{
  parent_activatable_iface = g_type_interface_peek_parent (iface);
  iface->update = geda_image_menu_item_update;
  iface->sync_action_properties = geda_image_menu_item_sync_action_properties;
}

static bool
activatable_update_stock_id (GedaImageMenuItem *image_menu_item, GtkAction *action)
{
  GtkWidget   *image;
  const char *stock_id  = gtk_action_get_stock_id (action);

  image = geda_image_menu_item_get_image (image_menu_item);

  if (GTK_IS_IMAGE (image) &&
    stock_id && gtk_icon_factory_lookup_default (stock_id))
  {
    gtk_image_set_from_stock (GTK_IMAGE (image), stock_id, GTK_ICON_SIZE_MENU);
    return TRUE;
  }

  return FALSE;
}

static bool
activatable_update_gicon (GedaImageMenuItem *image_menu_item, GtkAction *action)
{
  GtkWidget   *image;
  GIcon       *icon     = gtk_action_get_gicon (action);
  const char  *stock_id = gtk_action_get_stock_id (action);

  image = geda_image_menu_item_get_image (image_menu_item);

  if (icon && GTK_IS_IMAGE (image) &&
    !(stock_id && gtk_icon_factory_lookup_default (stock_id))) {
      gtk_image_set_from_gicon (GTK_IMAGE (image), icon, GTK_ICON_SIZE_MENU);
      return TRUE;
  }

  return FALSE;
}

static void
activatable_update_icon_name (GedaImageMenuItem *image_menu_item, GtkAction *action)
{
  GtkWidget  *image;
  const char *icon_name = gtk_action_get_icon_name (action);

  image = geda_image_menu_item_get_image (image_menu_item);

  if (GTK_IS_IMAGE (image) &&
    (gtk_image_get_storage_type (GTK_IMAGE (image)) == GTK_IMAGE_EMPTY ||
    gtk_image_get_storage_type (GTK_IMAGE (image)) == GTK_IMAGE_ICON_NAME))
  {
    gtk_image_set_from_icon_name (GTK_IMAGE (image), icon_name, GTK_ICON_SIZE_MENU);
  }
}

static void
geda_image_menu_item_update (GtkActivatable *activatable,
                             GtkAction      *action,
                             const char     *property_name)
{
  GedaImageMenuItem *image_menu_item;
  bool   use_appearance;

  image_menu_item = GEDA_IMAGE_MENU_ITEM (activatable);

  parent_activatable_iface->update (activatable, action, property_name);

  use_appearance = gtk_activatable_get_use_action_appearance (activatable);
  if (!use_appearance)
    return;

  if (strcmp (property_name, "stock-id") == 0)
    activatable_update_stock_id (image_menu_item, action);
  else if (strcmp (property_name, "gicon") == 0)
    activatable_update_gicon (image_menu_item, action);
  else if (strcmp (property_name, "icon-name") == 0)
    activatable_update_icon_name (image_menu_item, action);
}

static void
geda_image_menu_item_sync_action_properties (GtkActivatable *activatable,
                                             GtkAction      *action)
{
  GedaImageMenuItem *image_menu_item;
  GtkWidget *image;
  bool   use_appearance;

  image_menu_item = GEDA_IMAGE_MENU_ITEM (activatable);

  parent_activatable_iface->sync_action_properties (activatable, action);

  if (!action)
    return;

  use_appearance = gtk_activatable_get_use_action_appearance (activatable);
  if (!use_appearance)
    return;

  image = geda_image_menu_item_get_image (image_menu_item);
  if (image && !GTK_IS_IMAGE (image)) {
    geda_image_menu_item_set_image (image_menu_item, NULL);
    image = NULL;
  }

  if (!image) {
    image = gtk_image_new ();
    gtk_widget_show (image);
    geda_image_menu_item_set_image (GEDA_IMAGE_MENU_ITEM (activatable),
                                    image);
  }

  if (!activatable_update_stock_id (image_menu_item, action) &&
    !activatable_update_gicon (image_menu_item, action))
    activatable_update_icon_name (image_menu_item, action);

  geda_image_menu_item_set_show_image (image_menu_item,
                                       gtk_action_get_always_show_image (action));
}


/*! \brief Create a New GedaImageMenuItem Object
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 * Creates a new #GedaImageMenuItem with an empty label.
 *
 * \returns: a new #GedaImageMenuItem.
 *
 * \sa geda_image_menu_item_new_with_label geda_image_menu_item_new_with_mnemonic
 **/
GtkWidget*
geda_image_menu_item_new (void)
{
  return g_object_new (GEDA_TYPE_IMAGE_MENU_ITEM, NULL);
}

/*! \brief Create a New GedaImageMenuItem Object with a Plain Label
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 * Creates a new #GedaImageMenuItem containing a label.
 *
 * \param [in] label: the text of the menu item.
 *
 * \returns: a new #GedaImageMenuItem.
 *
 * \sa geda_image_menu_item_new_with_mnemonic
 *
 **/
GtkWidget*
geda_image_menu_item_new_with_label (const char *label)
{
  return g_object_new (GEDA_TYPE_IMAGE_MENU_ITEM,
                       "label", label,
                       NULL);
}


/*! \brief Create a New GedaImageMenuItem Object with a Mnemonic Label
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 * Creates a new #GedaImageMenuItem containing a label and setting
 * use-underline proptery to TRUE, so an underscore in the label
 * indicates the mnemonic for the menu item.
 *
 * \param [in] label: the text of the menu item, with an underscore
 *                    in front of the mnemonic character
 *
 * \returns: a new #GedaImageMenuItem
 *
 * \sa geda_image_menu_item_new_with_label
 **/
GtkWidget*
geda_image_menu_item_new_with_mnemonic (const char *label)
{
  return g_object_new (GEDA_TYPE_IMAGE_MENU_ITEM,
                       "use-underline", TRUE,
                       "label", label,
                       NULL);
}

/*! \brief Create a New GedaImageMenuItem Object with a Stock Image
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 *
 * Creates a new #GedaImageMenuItem containing the image and text from a
 * stock item. Some stock ids have preprocessor macros like GTK_STOCK_OK
 * and GTK_STOCK_APPLY.
 *
 * If you want this menu item to have changeable accelerators, then pass in
 * %NULL for accel_group. Next call gtk_menu_item_set_accel_path() with an
 * appropriate path for the menu item, use gtk_stock_lookup() to look up the
 * standard accelerator for the stock item, and if one is found, call
 * gtk_accel_map_add_entry() to register it.
 *
 * \param [in] stock_id:    the name of the stock item.
 * \param [in] accel_group: the GtkAccelGroup to add the menu items
 *                          accelerator to, or %NULL.
 *
 * \returns: a new #GedaImageMenuItem.
 *
 **/
GtkWidget*
geda_image_menu_item_new_from_stock (const char      *stock_id,
                                     GtkAccelGroup   *accel_group)
{
  return g_object_new (GEDA_TYPE_IMAGE_MENU_ITEM,
                       "label", stock_id,
                       "use-stock", TRUE,
                       "accel-group", accel_group,
                       NULL);
}

/*! \brief Set GedaImageMenuItem Use-Stock Property
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 * If %TRUE, the label set in the menuitem is used as a
 * stock id to select the stock item for the item.
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem
 * \param [in] use_stock:       set value
 *
 */
void
geda_image_menu_item_set_use_stock (GedaImageMenuItem *image_menu_item,
                                    bool               use_stock)
{
  g_return_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item));

  if (image_menu_item->use_stock != use_stock)
    {
      image_menu_item->use_stock = use_stock;

      geda_image_menu_item_recalculate (image_menu_item);

      g_object_notify (G_OBJECT (image_menu_item), "use-stock");
    }
}

/*! \brief Get GedaImageMenuItem Use-Stock Property
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 * Checks whether the label set in the menuitem is used as a
 * stock id to select the stock item for the item.
 *
 * Returns: %TRUE if the label set in the menuitem is used as a
 *     stock id to select the stock item for the item
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem
 *
 */
bool
geda_image_menu_item_get_use_stock (GedaImageMenuItem *image_menu_item)
{
  g_return_val_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item), FALSE);

  return image_menu_item->use_stock;
}

/*! \brief Set GedaImageMenuItem Show-Image Property
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 * If %TRUE, the menu item will ignore the GtkSettings:gtk-menu-images
 * setting and always show the image, if available.
 *
 * Use this property if the menuitem would be useless or hard to use
 * without the image.
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem
 * \param [in] always_show:     desired state
 *
 */
void
geda_image_menu_item_set_show_image (GedaImageMenuItem *image_menu_item,
                                           bool         always_show)
{
  g_return_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item));

  if (image_menu_item->show_image != always_show) {
      image_menu_item->show_image  = always_show;

    if (image_menu_item->image) {
      if (show_image (image_menu_item))
        gtk_widget_show (image_menu_item->image);
      else
        gtk_widget_hide (image_menu_item->image);
    }

    g_object_notify (G_OBJECT (image_menu_item), "show-image");
  }
}

/*! \brief Get GedaImageMenuItem Show-Image Property
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 * Returns whether the menu item will ignore the GtkSettings:
 * gtk-menu-images setting and always show the image, if available.
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem
 *
 * \returns: %TRUE if the menu item will always show the image
 *
 */
bool
geda_image_menu_item_get_show_image (GedaImageMenuItem *image_menu_item)
{
  g_return_val_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item), FALSE);

  return image_menu_item->show_image;
}


/*! \brief Add Accel group to a GedaImageMenuItem object
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 *
 * Specifies an accel_group to add the menu items accelerator to
 * (this only applies to stock items so a stock item must already
 * be set, make sure to call geda_image_menu_item_set_use_stock()
 * and gtk_menu_item_set_label() with a valid stock item first).
 *
 * If you want this menu item to have changeable accelerators then
 * you shouldnt need this (see geda_image_menu_item_new_from_stock()).
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem
 * \param [in] accel_group:     the GtkAccelGroup
 *
 */
void
geda_image_menu_item_set_accel_group (GedaImageMenuItem *image_menu_item,
                                      GtkAccelGroup     *accel_group)
{
  GtkStockItem             stock_item;

  /* Silent return for the constructor */
  if (!accel_group)
    return;

  g_return_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item));
  g_return_if_fail (GTK_IS_ACCEL_GROUP (accel_group));

  if (image_menu_item->use_stock &&
      image_menu_item->label &&
      gtk_stock_lookup (image_menu_item->label, &stock_item)) {
    if (stock_item.keyval){
      gtk_widget_add_accelerator (GTK_WIDGET (image_menu_item),
                                  "activate",
                                  accel_group,
                                  stock_item.keyval,
                                  stock_item.modifier,
                                  GTK_ACCEL_VISIBLE);

      g_object_notify (G_OBJECT (image_menu_item), "accel-group");
    }
  }
}

/*! \brief Set/Attach an image object to the menu item
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem.
 * \param [in] image          : a widget to set as the image for the menu item.
 *
 * Sets the image of image_menu_item to the given widget.
 * Note that it depends on the show-menu-images setting whether
 * the image will be displayed or not.
 **/
void
geda_image_menu_item_set_image (GedaImageMenuItem *image_menu_item,
                               GtkWidget          *image)
{
  g_return_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item));

  if (image == image_menu_item->image)
    return;

  if (image_menu_item->image)
    gtk_container_remove (GTK_CONTAINER (image_menu_item),
                          image_menu_item->image);

    image_menu_item->image = image;

  if (image == NULL)
    return;

  gtk_widget_set_parent (image, GTK_WIDGET (image_menu_item));
  g_object_set (image,
                "visible", show_image (image_menu_item),
                "no-show-all", TRUE,
                NULL);

  g_object_notify (G_OBJECT (image_menu_item), "image");
}

/*! \brief Get the image object associated with the menu item
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem
 *
 * Gets the widget that is currently set as the image of
 * image_menu_item.
 *
 * \sa geda_image_menu_item_set_image().
 *
 * \return:the widget set as image of the GedaImageMenuItem
 **/
GtkWidget*
geda_image_menu_item_get_image (GedaImageMenuItem *image_menu_item)
{
  g_return_val_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item), NULL);

  return image_menu_item->image;
}

/*! \brief Remove the image object attached to the menu item
 *  \ingroup GedaImageMenuItem
 *  \par Function Description
 */
static void
geda_image_menu_item_remove (GtkContainer *container,
                            GtkWidget    *child)
{
  GedaImageMenuItem *image_menu_item;

  image_menu_item = GEDA_IMAGE_MENU_ITEM (container);

  if (child == image_menu_item->image) {
    bool widget_was_visible;

    widget_was_visible = gtk_widget_get_visible (child);

    gtk_widget_unparent (child);
    image_menu_item->image = NULL;

    if (widget_was_visible &&
      gtk_widget_get_visible (GTK_WIDGET (container)))
      gtk_widget_queue_resize (GTK_WIDGET (container));

    g_object_notify (G_OBJECT (image_menu_item), "image");
  }
  else
  {
    GTK_CONTAINER_CLASS (geda_image_menu_item_parent_class)->remove (container, child);
  }
}

/*
static void
show_image_change_notify (GedaImageMenuItem *image_menu_item)
{
  if (image_menu_item->image){
    if (show_image (image_menu_item)) {
      gtk_widget_show (image_menu_item->image);
    }
    else {
      gtk_widget_hide (image_menu_item->image);
    }
  }
}

static void
traverse_container (GtkWidget *widget, void *data)
{
  if (GEDA_IS_IMAGE_MENU_ITEM (widget)) {
    show_image_change_notify (GEDA_IMAGE_MENU_ITEM (widget));
  }
  else if (GTK_IS_CONTAINER (widget)) {
    gtk_container_forall (GTK_CONTAINER (widget), traverse_container, NULL);
  }
}


static void
geda_image_menu_item_setting_changed (GtkSettings *settings)
{
  GList *list, *l;

  list = gtk_window_list_toplevels ();

  for (l = list; l; l = l->next) {
    gtk_container_forall (GTK_CONTAINER (l->data), traverse_container, NULL);
  }

  g_list_free (list);
}

static void
geda_image_menu_item_screen_changed (GtkWidget *widget,
                                     GdkScreen *previous_screen)
{
  GtkSettings *settings;
  unsigned int show_image_connection;

  if (!gtk_widget_has_screen (widget))
    return;

  settings = gtk_widget_get_settings (widget);

  show_image_connection =
  (void*)(long) (GEDA_OBJECT_GET_DATA(settings,
                                       "gtk-image-menu-item-connection"));

  if (show_image_connection)
    return;

  show_image_connection =
  g_signal_connect (settings, "notify::gtk-menu-images",
                    G_CALLBACK (geda_image_menu_item_setting_changed), NULL);
  g_object_set_data (G_OBJECT (settings),
                     _("gtk-image-menu-item-connection"),
                                       GUINT_TO_POINTER (show_image_connection));

  show_image_change_notify (GEDA_IMAGE_MENU_ITEM (widget));
}
*/
/** @} end group GedaImageMenuItem */