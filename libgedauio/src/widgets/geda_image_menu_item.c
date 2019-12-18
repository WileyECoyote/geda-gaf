/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 tab-width: 4 -*- */
/* vi: set et ts=4 sw=2 sts=2: */
/*
 * File: geda_image_menu_item.c
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 2001 Red Hat, Inc.
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
 *
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#define WITHOUT_GUILE 1
#include <libgeda/libgeda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_container.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_image_menu_item.h"
#include "../../include/geda_menu_bar.h"
#include "../../include/geda_menu.h"
#include "../../include/gettext.h"

#include <geda_debug.h>

/**
 * \brief GedaImageMenuItem - A Menu Item with and Image Widget
 * \par
 * A GedaImageMenuItem is a replacement for the GtkMenuItem because the
 * GtkImageMenuItem is listed as depreciated. There is one major differences
 * between a GedaImageMenuItem and a GtkMenuItem, the GedaImageMenuItem does
 * NOT have a "gtk-show-image" property and does NOT enable the menu icons
 * based on system settings. The GtkMenuItem version displays images (after
 * being initialized not to do so) if the systems settings are set to display
 * images because GTK/Gnome applies settings after programs show their main
 * window and have already applied any user settings. This is unacceptable
 * behavior, and hence this widget exist as a replacement.
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
static void geda_image_menu_item_toggle_size_request  (GedaMenuItem      *menu_item,
                                                       int              *requisition);
static void geda_image_menu_item_set_label            (GedaMenuItem      *menu_item,
                                                       const char       *label);
static const char *geda_image_menu_item_get_label     (GedaMenuItem      *menu_item);

static void geda_image_menu_item_forall               (GtkContainer     *container,
                                                       bool              include_internals,
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

static void geda_image_menu_item_update               (GtkActivatable       *activatable,
                                                       GtkAction            *action,
                                                       const char           *property_name);
static void geda_image_menu_item_sync_action          (GtkActivatable       *activatable,
                                                       GtkAction            *action);
static void geda_image_menu_item_activatable_init     (GtkActivatableIface  *iface);

enum {
  PROP_0,
  PROP_IMAGE,
  PROP_USE_STOCK,
  PROP_ACCEL_GROUP,
  PROP_SHOW_IMAGE
};

struct _GedaImageMenuItemData
{
  GtkAction *action;
};

static const char str_use_stock[]  = "use-stock";

static GtkActivatableIface *parent_activatable_iface;

static void *geda_image_menu_item_parent_class = NULL;

static GHashTable *image_menu_item_hash = NULL;

static bool activatable_update_stock_id (GedaImageMenuItem *image_menu_item,
                                                 GtkAction *action)
{
  const char *stock_id  = gtk_action_get_stock_id (action);

  if (stock_id && gtk_icon_factory_lookup_default(stock_id)) {

    GtkImage *image;

    image = (GtkImage*)geda_image_menu_item_get_image (image_menu_item);

    if (GTK_IS_IMAGE (image)) {
      gtk_image_set_from_stock (image, stock_id, GTK_ICON_SIZE_MENU);
      return TRUE;
    }
  }

  return FALSE;
}

static bool activatable_update_gicon (GedaImageMenuItem *image_menu_item,
                                              GtkAction *action)
{
  GIcon      *icon;
  GtkImage   *image;
  const char *stock_id;

  icon     = gtk_action_get_gicon (action);
  stock_id = gtk_action_get_stock_id (action);
  image    = (GtkImage*)geda_image_menu_item_get_image (image_menu_item);

  if (icon && GTK_IS_IMAGE (image) &&
    !(stock_id && gtk_icon_factory_lookup_default (stock_id)))
  {
    gtk_image_set_from_gicon (image, icon, GTK_ICON_SIZE_MENU);
    return TRUE;
  }

  return FALSE;
}

static void activatable_update_icon_name (GedaImageMenuItem *image_menu_item,
                                                  GtkAction *action)
{
  GtkImage    *image;
  const char  *icon_name;
  GtkImageType type;

  icon_name = gtk_action_get_icon_name (action);
  image     = (GtkImage*)geda_image_menu_item_get_image (image_menu_item);
  type      = gtk_image_get_storage_type (image);

  if (GTK_IS_IMAGE (image) &&
     (type == GTK_IMAGE_EMPTY || type == GTK_IMAGE_ICON_NAME))
  {
    gtk_image_set_from_icon_name (image, icon_name, GTK_ICON_SIZE_MENU);
  }
}

/*! \internal iface->sync_action_properties */
static void geda_image_menu_item_sync_action (GtkActivatable *activatable,
                                              GtkAction      *action)
{
  GedaImageMenuItem     *image_menu_item;
  GedaImageMenuItemData *priv;
  GtkWidget             *image;

  bool use_appearance;

  image_menu_item = (GedaImageMenuItem*)activatable;
  priv            = image_menu_item->priv;

  /* Chain-up */
  parent_activatable_iface->sync_action_properties (activatable, action);

  if (priv->action) {
    g_object_unref(priv->action);  /* Even if new action is NULL */
  }

  priv->action = action; /* Which could be NULL */

  if (!action) {
    return;
  }

  g_object_get (activatable, "use-action-appearance", &use_appearance, NULL);

  if (use_appearance) {

    image = geda_image_menu_item_get_image (image_menu_item);

    if (image && !GTK_IS_IMAGE (image)) {
      geda_image_menu_item_set_image (image_menu_item, NULL);
      image = NULL;
    }

    if (!image) {
      image = gtk_image_new ();
      gtk_widget_show (image);
      geda_image_menu_item_set_image (image_menu_item, image);
    }

    if (!activatable_update_stock_id (image_menu_item, action) &&
      !activatable_update_gicon (image_menu_item, action))
    {
      activatable_update_icon_name (image_menu_item, action);
    }

    geda_image_menu_item_set_show_image (image_menu_item,
                                         gtk_action_get_always_show_image (action));
  }
}

/*! \internal iface->update */
static void geda_image_menu_item_update (GtkActivatable *activatable,
                                         GtkAction      *action,
                                         const char     *property_name)
{
  bool use_appearance;

  parent_activatable_iface->update (activatable, action, property_name);

  g_object_get (activatable, "use-action-appearance", &use_appearance, NULL);

  if (use_appearance) {

    GedaImageMenuItem *image_menu_item;

    image_menu_item = (GedaImageMenuItem*)activatable;

    if (strcmp (property_name, "stock-id") == 0) {
      activatable_update_stock_id (image_menu_item, action);
    }
    else if (strcmp (property_name, "gicon") == 0) {
      activatable_update_gicon (image_menu_item, action);
    }
    else if (strcmp (property_name, "icon-name") == 0) {
      activatable_update_icon_name (image_menu_item, action);
    }
  }
}

static void geda_image_menu_item_activatable_init (GtkActivatableIface  *iface)
{
  parent_activatable_iface      = g_type_interface_peek_parent (iface);
  iface->update                 = geda_image_menu_item_update;
  iface->sync_action_properties = geda_image_menu_item_sync_action;
}

/*!
 * \brief GedaImageMenuItem GObject property setter function
 * \par Function Description
 *  gobject_class->finalize virtual over-ride setter function for
 *  GedaImageMenuItem's GObject properties.
 *
 * \param [in]  object    The Object whose properties we are setting
 * \param [in]  property  The numeric id. under which the property was
 *                        registered with g_object_class_install_property()
 * \param [in]  value     The GValue the property is being set from
 * \param [in]  pspec     A GParamSpec describing the property being set
 */
static void geda_image_menu_item_set_property (GObject      *object,
                                               unsigned int  property,
                                               const GValue *value,
                                               GParamSpec   *pspec)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)object;

  switch (property) {

    case PROP_IMAGE:
      geda_image_menu_item_set_image (image_menu_item, (GtkWidget*)g_value_get_object (value));
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

/*!
 * \brief GedaImageMenuItem GObject property getter function
 * \par Function Description
 *  gobject_class->finalize virtual over-ride getter function for
 *  GedaImageMenuItem's properties.
 *
 * \param [in]  object    The Object whose properties we are getting
 * \param [in]  property  The numeric id. under which the property was
 *                        registered with g_object_class_install_property()
 * \param [out] value     The GValue in which to return the value of the property
 * \param [in]  pspec     A GParamSpec describing the property being got
 */
static void geda_image_menu_item_get_property (GObject      *object,
                                               unsigned int  property,
                                               GValue       *value,
                                               GParamSpec   *pspec)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)object;

  switch (property) {

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

/*!
 * \brief GedaImageMenuItem Object finalize handler
 * \par Function Description
 *  gobject_class->finalize virtual over-ride called just before the
 *  GedaImageMenuItem GObject is finalized, free our allocated data,
 *  and then chain up to the parent handler.
 *
 * \param [in] object The GObject being finalized.
 */
static void geda_image_menu_item_finalize (GObject *object)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)object;

  if (g_hash_table_remove (image_menu_item_hash, object)) {
    if (!g_hash_table_size (image_menu_item_hash)) {
      g_hash_table_destroy (image_menu_item_hash);
      image_menu_item_hash = NULL;
    }
  }

  GEDA_FREE (image_menu_item->label);
  GEDA_FREE (image_menu_item->priv);

  ((GObjectClass*)geda_image_menu_item_parent_class)->finalize (object);
}

/*!
 * \brief Type class initializer for GedaImageMenuItem
 * \par Function Description
 *  Type class initializer for GedaImageMenuItem. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 * \param [in] class      The instance of GedaActionClass to be initialized
 * \param [in] class_data Not implemented
 */
static void geda_image_menu_item_class_init (void *class, void *class_data)
{
  GObjectClass      *gobject_class     = (GObjectClass*) class;
  GtkObjectClass    *object_class      = (GtkObjectClass*) class;
  GtkWidgetClass    *widget_class      = (GtkWidgetClass*) class;
  GedaMenuItemClass *menu_item_class   = (GedaMenuItemClass*) class;
  GtkContainerClass *container_class   = (GtkContainerClass*) class;
  GParamSpec        *params;

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

  geda_image_menu_item_parent_class    = g_type_class_peek_parent (class);

  /*! property GedaImageMenuItem::image
   *
   * Image widget displayed next to menu item.
   */
  params = g_param_spec_object ("image",
                              _("Image widget"),
                              _("Child widget to appear next to the menu text"),
                                 GTK_TYPE_WIDGET,
                                 G_PARAM_WRITABLE);

  g_object_class_install_property (gobject_class, PROP_IMAGE, params);

  /*! property GedaImageMenuItem::use-stock
   *
   * If %TRUE, the label set in the menuitem is used as a
   * stock id to select the stock item for the item.
   */
  params = g_param_spec_boolean (str_use_stock,
                               _("Use stock"),
                               _("Whether to use the label text to create a stock menu item"),
                                  FALSE,
                                  G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  g_object_class_install_property (gobject_class, PROP_USE_STOCK, params);

  /*! property  GedaImageMenuItem::show-image
   *
   * If %TRUE, the menu item will ignore the GtkSettings:gtk-menu-images
   * setting and always show the image, if available.
   *
   * Use this property if the menuitem would be useless or hard to use
   * without the image.
   */
  params = g_param_spec_boolean ("show-image",
                               _("Show image"),
                               _("Whether the image will always be shown"),
                                  FALSE,
                                  G_PARAM_WRITABLE | G_PARAM_CONSTRUCT);

  g_object_class_install_property (gobject_class, PROP_SHOW_IMAGE, params);

  /*! property GedaImageMenuItem::accel-group
   *
   * The Accel Group to use for stock accelerator keys
   */
  params = g_param_spec_object ("accel-group",
                              _("Accelerator Group"),
                              _("The Accel Group to use for stock accelerator keys"),
                                 GTK_TYPE_ACCEL_GROUP,
                                 G_PARAM_WRITABLE);

  g_object_class_install_property (gobject_class,  PROP_ACCEL_GROUP, params);
}

/*!
 * \brief Initialize GedaImageMenuItem data structure.
 * \par Function Description
 *  Function tois call after the GedaImageMenuItemClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance The GedaImageMenuItem structure being initialized,
 * \param [in] g_class  The GedaImageMenuItem class we are initializing.
 */
static void geda_image_menu_item_init (GTypeInstance *instance, void *g_class)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)instance;

  /* Note data->initialization not required because memset 0 */
  image_menu_item->priv      = GEDA_MEM_ALLOC0(sizeof(GedaImageMenuItemData));
  image_menu_item->use_stock = FALSE;
  image_menu_item->label     = NULL;
  image_menu_item->image     = NULL;

  if (!image_menu_item_hash) {
    image_menu_item_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (image_menu_item_hash, instance, instance);
}

/*!
 * \brief Retrieve GedaImageMenuItem's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaImageMenuItem Type identifier. When
 *  first called, the function registers a #GedaImageMenuItem in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaImageMenuItem and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaImageMenuItem.
 */
GedaType geda_image_menu_item_get_type (void)
{
  static volatile GedaType geda_image_menu_item_type = 0;

  if (g_once_init_enter (&geda_image_menu_item_type)) {

    static const GTypeInfo info = {
      sizeof(GedaImageMenuItemClass),
      NULL,                            /* base_init           */
      NULL,                            /* base_finalize       */
      geda_image_menu_item_class_init, /* (GClassInitFunc)    */
      NULL,                            /* class_finalize      */
      NULL,                            /* class_data          */
      sizeof(GedaImageMenuItem),
      0,                               /* n_preallocs         */
      geda_image_menu_item_init        /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaImageMenuItem");
    type   = g_type_register_static (GEDA_TYPE_MENU_ITEM, string, &info, 0);

    const GInterfaceInfo interface_info = {
      (GInterfaceInitFunc) geda_image_menu_item_activatable_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_ACTIVATABLE, &interface_info);

    g_once_init_leave (&geda_image_menu_item_type, type);
  }

  return geda_image_menu_item_type;
}

/*!
 * \brief Check if an object is a GedaImageMenuItem
 * \par Function Description
 *  Determines if \a image_menu_item is valid by verifying \a image_menu_item
 *  is included in the hash table of GedaImageMenuItem objects.
 *
 * \return TRUE if \a image_menu_item is a valid GedaImageMenuItem
 */
bool is_a_geda_image_menu_item (GedaImageMenuItem *image_menu_item)
{
  if ((image_menu_item != NULL) && (image_menu_item_hash != NULL)) {
    return g_hash_table_lookup(image_menu_item_hash, image_menu_item) ? TRUE : FALSE;
  }
  return FALSE;
}

/*!
 * \brief return internal show image property
 * \par Function Description
 *  This is an internal function to return the show_image property.
 *  The function's address is supplied to external functions.
 *
 * \param [in] image_menu_item Pointer to a GedaImageMenuItem.
 */
static inline bool show_image (GedaImageMenuItem *image_menu_item)
{
  return image_menu_item->show_image;
}

static void geda_image_menu_item_map (GtkWidget *widget)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)widget;

  /* Chain-up first */
  ((GtkWidgetClass*)geda_image_menu_item_parent_class)->map (widget);

  if (image_menu_item->image) {

    if (show_image (image_menu_item)) {
      gtk_widget_show (image_menu_item->image);
    }
    else {
      gtk_widget_hide (image_menu_item->image);
    }
  }
}

/*! \internal GtkObjectClass->destroy */
static void geda_image_menu_item_destroy (GtkObject *object)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)object;

  GtkWidget *child = gtk_bin_get_child((GtkBin*)image_menu_item);

  if (image_menu_item->image) {
    geda_container_remove (image_menu_item, image_menu_item->image);
  }

  if (child) {
    gtk_widget_destroy(child);
  }

  ((GtkObjectClass*)geda_image_menu_item_parent_class)->destroy (object);
}

/*! \internal menu_item_class->toggle_size_request */
static void geda_image_menu_item_toggle_size_request (GedaMenuItem *menu_item,
                                                               int *requisition)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)menu_item;
  GedaMenuBar       *parent;

  PackDirection pack_dir;

  parent = geda_get_widget_parent(menu_item);

  if (GEDA_IS_MENU_BAR (parent)) {
    pack_dir = geda_menu_bar_get_child_pack_direction (parent);
  }
  else {
    pack_dir = PACK_DIRECTION_LTR;
  }

  *requisition = 0;

  if (image_menu_item->image && gtk_widget_get_visible (image_menu_item->image))
  {
    GtkRequisition image_requisition;
    unsigned int   toggle_spacing;

    gtk_widget_get_child_requisition (image_menu_item->image,
                                      &image_requisition);

    gtk_widget_style_get ((GtkWidget*)menu_item,
                          "toggle-spacing", &toggle_spacing,
                          NULL);

    if (pack_dir == PACK_DIRECTION_LTR || pack_dir == PACK_DIRECTION_RTL)
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

static void geda_image_menu_item_recalculate (GedaImageMenuItem *image_menu_item)
{
  const char *resolved_label = image_menu_item->label;

  if (image_menu_item->use_stock && resolved_label) {

    GtkStockItem stock_item;

    if (!image_menu_item->image) {

      GtkWidget *image;

      image = gtk_image_new_from_stock (resolved_label, GTK_ICON_SIZE_MENU);
      geda_image_menu_item_set_image (image_menu_item, image);
    }

    if (gtk_stock_lookup (resolved_label, &stock_item)) {
      resolved_label = stock_item.label;
    }

    geda_menu_item_set_use_underline ((GedaMenuItem*)image_menu_item, TRUE);
  }

  geda_menu_item_set_label((GedaMenuItem*)image_menu_item, resolved_label);

  ((GedaMenuItemClass*)geda_image_menu_item_parent_class)->
    set_label ((GedaMenuItem*)image_menu_item, resolved_label);
}

/*! \internal menu_item_class->set_label */
static void geda_image_menu_item_set_label (GedaMenuItem *menu_item,
                                              const char *label)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)menu_item;

  if (image_menu_item->label != label) {

    g_free (image_menu_item->label);
    image_menu_item->label = geda_strdup (label);

    geda_image_menu_item_recalculate (image_menu_item);

    GEDA_OBJECT_NOTIFY (menu_item, "label");
  }
}

/*! \internal menu_item_class->get_label */
static const char *geda_image_menu_item_get_label (GedaMenuItem *menu_item)
{
  return ((GedaImageMenuItem*)menu_item)->label;
}

/*! \internal widget_class->size_request */
static void geda_image_menu_item_size_request (GtkWidget      *widget,
                                               GtkRequisition *requisition)
{
  GedaImageMenuItem *image_menu_item;
  GedaMenuBar       *parent;

  PackDirection pack_dir;

  int child_width;
  int child_height;

  parent = geda_get_widget_parent(widget);

  if (GEDA_IS_MENU_BAR (parent)) {
    pack_dir = geda_menu_bar_get_child_pack_direction (parent);
  }
  else {
    pack_dir = PACK_DIRECTION_LTR;
  }

  image_menu_item = (GedaImageMenuItem*)widget;

  if (image_menu_item->image && gtk_widget_get_visible (image_menu_item->image))
  {
      GtkRequisition child_requisition;

      gtk_widget_size_request (image_menu_item->image, &child_requisition);

      child_width  = child_requisition.width;
      child_height = child_requisition.height;
  }
  else {
      child_width  = 0;
      child_height = 0;
  }

  ((GtkWidgetClass*)geda_image_menu_item_parent_class)->size_request (widget, requisition);

  /* not done with height since that happens via the toggle_size_reques */
  if (pack_dir == PACK_DIRECTION_LTR || pack_dir == PACK_DIRECTION_RTL) {
    requisition->height = MAX (requisition->height, child_height);
  }
  else {
    requisition->width = MAX (requisition->width, child_width);
  }

  /* Note that GedaMenuShell always size requests before
   * toggle_size_request, so toggle_size_request will be able to use
   * image_menu_item->image->requisition
   */
}

/*! \internal widget_class->size_allocate */
static void geda_image_menu_item_size_allocate (GtkWidget     *widget,
                                                GtkAllocation *allocated)
{
  GedaImageMenuItem *image_menu_item;
  GedaMenuBar       *parent;

  PackDirection   pack_dir;

  parent = geda_get_widget_parent(widget);

  if (GEDA_IS_MENU_BAR (parent)) {
    pack_dir = geda_menu_bar_get_child_pack_direction (parent);
  }
  else {
    pack_dir = PACK_DIRECTION_LTR;
  }

  image_menu_item = (GedaImageMenuItem*)widget;

  /* Chain-up early */
  ((GtkWidgetClass*)geda_image_menu_item_parent_class)->size_allocate (widget, allocated);

  if (image_menu_item->image && gtk_widget_get_visible (image_menu_item->image))
  {
    GtkAllocation  *allocation;
    GtkRequisition  child_requisition;
    GtkAllocation   child_allocation;
    unsigned int    horizontal_padding;
    unsigned int    toggle_spacing;
    unsigned short  toggle_size;
    int x, y, offset;

    gtk_widget_style_get (widget,
                          "horizontal-padding", &horizontal_padding,
                          "toggle-spacing",     &toggle_spacing,
                          NULL);

    allocation = geda_get_widget_allocation (widget);

    /* Man this is lame hardcoding action, but I can't
     * come up with a solution that's really better.
     */
    gtk_widget_get_child_requisition (image_menu_item->image,
                                      &child_requisition);

    toggle_size = geda_menu_item_get_toggle_size((GedaMenuItem*)image_menu_item);

    if (pack_dir == PACK_DIRECTION_LTR ||
        pack_dir == PACK_DIRECTION_RTL)
    {
      offset = ((GtkContainer*)image_menu_item)->border_width +
                                                 widget->style->xthickness;

      if ((gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) ==
          (pack_dir == PACK_DIRECTION_LTR))
      {
        x = offset + horizontal_padding +
            (toggle_size - toggle_spacing - child_requisition.width) / 2;
      }
      else
      {
        x = allocation->width - offset - horizontal_padding -
            toggle_size + toggle_spacing +
           (toggle_size - toggle_spacing - child_requisition.width) / 2;
      }
      y = (allocation->height - child_requisition.height) / 2;
    }
    else {

      offset = ((GtkContainer*)image_menu_item)->border_width +
                                                 widget->style->ythickness;

      if ((gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) ==
          (pack_dir == PACK_DIRECTION_TTB))
      {
        y = offset + horizontal_padding +
            (toggle_size - toggle_spacing - child_requisition.height) / 2;
      }
      else
      {
        y = allocation->height - offset - horizontal_padding -
            toggle_size + toggle_spacing +
           (toggle_size - toggle_spacing - child_requisition.height) / 2;
      }
      x = (allocation->width - child_requisition.width) / 2;
    }

    child_allocation.width  = child_requisition.width;
    child_allocation.height = child_requisition.height;
    child_allocation.x      = allocation->x + MAX (x, 0);
    child_allocation.y      = allocation->y + MAX (y, 0);

    gtk_widget_size_allocate (image_menu_item->image, &child_allocation);
  }
}

/*! GtkContainerClass->forall virtual over-ride */
static void geda_image_menu_item_forall (GtkContainer *container,
                                         bool          include_internals,
                                         GtkCallback   callback,
                                         void         *callback_data)
{
  GedaImageMenuItem *image_menu_item = (GedaImageMenuItem*)container;

  ((GtkContainerClass*)geda_image_menu_item_parent_class)->
    forall (container, include_internals, callback, callback_data);

  if (include_internals && image_menu_item->image) {
    (*callback) (image_menu_item->image, callback_data);
  }
}


/*!
 * \brief Create a New GedaImageMenuItem Object
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  Creates a new #GedaImageMenuItem with an empty label.
 *
 * \returns a new #GedaImageMenuItem.
 *
 * \sa geda_image_menu_item_new_with_label geda_image_menu_item_new_with_mnemonic
 */
GtkWidget *geda_image_menu_item_new (void)
{
  return g_object_new (GEDA_TYPE_IMAGE_MENU_ITEM, NULL);
}

/*!
 * \brief Create a New GedaImageMenuItem Object with a Plain Label
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  Creates a new #GedaImageMenuItem containing a label.
 *
 * \param [in] label: the text of the menu item.
 *
 * \returns a new #GedaImageMenuItem.
 *
 * \sa geda_image_menu_item_new_with_mnemonic
 *
 */
GtkWidget *geda_image_menu_item_new_with_label (const char *label)
{
  return g_object_new (GEDA_TYPE_IMAGE_MENU_ITEM,
                       "label", label,
                       NULL);
}


/*!
 * \brief Create a New GedaImageMenuItem Object with a Mnemonic Label
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  Creates a new #GedaImageMenuItem containing a label and setting
 *  use-underline proptery to TRUE, so an underscore in the label
 *  indicates the mnemonic for the menu item.
 *
 * \param [in] label the text of the menu item, with an underscore
 *                   in front of the mnemonic character
 *
 * \returns a new #GedaImageMenuItem
 *
 * \sa geda_image_menu_item_new_with_label
 */
GtkWidget *geda_image_menu_item_new_with_mnemonic (const char *label)
{
  return g_object_new (GEDA_TYPE_IMAGE_MENU_ITEM,
                       "use-underline", TRUE,
                       "label", label,
                       NULL);
}

/*!
 * \brief Create a New GedaImageMenuItem Object with a Stock Image
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  Creates a new #GedaImageMenuItem containing the image and text from a
 *  stock item. Some stock ids have preprocessor macros like GTK_STOCK_OK
 *  and GTK_STOCK_APPLY.
 *
 *  If you want this menu item to have changeable accelerators, then pass in
 *  %NULL for accel_group. Next call geda_menu_item_set_accel_path() with an
 *  appropriate path for the menu item, use gtk_stock_lookup() to look up the
 *  standard accelerator for the stock item, and if one is found, call
 *  gtk_accel_map_add_entry() to register it.
 *
 * \param [in] stock_id     the name of the stock item.
 * \param [in] accel_group  the GtkAccelGroup to add the menu items
 *                          accelerator to, or %NULL.
 *
 * \returns a new #GedaImageMenuItem.
 */
GtkWidget *geda_image_menu_item_new_from_stock (const char    *stock_id,
                                                GtkAccelGroup *accel_group)
{
  return g_object_new (GEDA_TYPE_IMAGE_MENU_ITEM,
                       "label", stock_id,
                       str_use_stock, TRUE,
                       "accel-group", accel_group,
                       NULL);
}

/*!
 * \brief Set GedaImageMenuItem Use-Stock Property
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  If %TRUE, the label set in the menuitem is used as a stock id to select
 *  the stock item for the item.
 *
 * \param [in] image_menu_item  a #GedaImageMenuItem
 * \param [in] use_stock        set value
 */
void geda_image_menu_item_set_use_stock (GedaImageMenuItem *image_menu_item,
                                                      bool  use_stock)
{
  g_return_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item));

  if (image_menu_item->use_stock != use_stock) {

    image_menu_item->use_stock = use_stock;

    geda_image_menu_item_recalculate (image_menu_item);

    GEDA_OBJECT_NOTIFY (image_menu_item, str_use_stock);
  }
}

/*!
 * \brief Get GedaImageMenuItem Use-Stock Property
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  Checks whether the label set in the menuitem is used as a stock id to
 *  select the stock item for the item.
 *
 * \returns %TRUE if the label set in the menuitem is used as a
 *          stock id to select the stock item for the item
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem
 */
bool geda_image_menu_item_get_use_stock (GedaImageMenuItem *image_menu_item)
{
  g_return_val_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item), FALSE);

  return image_menu_item->use_stock;
}

/*!
 * \brief Set GedaImageMenuItem Show-Image Property
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  If %TRUE, the menu item will ignore the GtkSettings:gtk-menu-images
 *  setting and always show the image, if available.
 *
 *  Use this property if the menuitem would be useless or hard to use
 *  without the image.
 *
 * \param [in] image_menu_item  a #GedaImageMenuItem
 * \param [in] always_show      desired state
 */
void geda_image_menu_item_set_show_image (GedaImageMenuItem *image_menu_item,
                                                       bool  always_show)
{
  g_return_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item));

  if (image_menu_item->show_image != always_show) {

    image_menu_item->show_image  = always_show;

    if (image_menu_item->image) {

      if (show_image (image_menu_item)) {
        gtk_widget_show (image_menu_item->image);
      }
      else {
        gtk_widget_hide (image_menu_item->image);
      }
    }

    GEDA_OBJECT_NOTIFY (image_menu_item, "show-image");
  }
}

/*!
 * \brief Get GedaImageMenuItem Show-Image Property
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  Returns whether the menu item will ignore the GtkSettings:
 *  gtk-menu-images setting and always show the image, if available.
 *
 * \param [in] image_menu_item: a #GedaImageMenuItem
 *
 * \retval %TRUE if the menu item will always show the image
 *
 */
bool geda_image_menu_item_get_show_image (GedaImageMenuItem *image_menu_item)
{
  g_return_val_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item), FALSE);

  return image_menu_item->show_image;
}


/*!
 * \brief Add Accel group to a GedaImageMenuItem object
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  Specify the accel_group to add the menu items accelerator to
 *  (this only applies to stock items so a stock item must already
 *  be set, make sure to call geda_image_menu_item_set_use_stock()
 *  and geda_menu_item_set_label() with a valid stock item first).
 *
 *  If you want this menu item to have changeable accelerators then
 *  you should not need this, see geda_image_menu_item_new_from_stock().
 *
 *  If \a accel_group is NULL, then image_menu_item is removed from
 *  any group of which it is a member.
 *
 * \param [in] image_menu_item a #GedaImageMenuItem
 * \param [in] accel_group     the GtkAccelGroup
 *
 *  \todo Why does this only apply to stock items? Should this not
 *  apply to any item with an accelerator whose parent menu has a
 *  group?
 */
void geda_image_menu_item_set_accel_group (GedaImageMenuItem *image_menu_item,
                                           GtkAccelGroup     *accel_group)
{
  g_return_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item));

  /* Silent return for the constructor */
  if (accel_group) {

    GtkStockItem stock_item;

    g_return_if_fail (GTK_IS_ACCEL_GROUP (accel_group));

    if (image_menu_item->use_stock &&
        image_menu_item->label &&
        gtk_stock_lookup (image_menu_item->label, &stock_item))
    {
      if (stock_item.keyval){
        gtk_widget_add_accelerator ((GtkWidget*)image_menu_item,
                                    "activate",
                                    accel_group,
                                    stock_item.keyval,
                                    stock_item.modifier,
                                    GTK_ACCEL_VISIBLE);

        GEDA_OBJECT_NOTIFY (image_menu_item, "accel-group");
      }
    }
  }
  else {

    GtkWidget *parent;

    parent = geda_get_widget_parent(image_menu_item);

    if (GEDA_IS_MENU(parent)) {

      GtkAccelGroup *accel_group;

      accel_group = geda_menu_widget_get_accel_group (parent);

      if (accel_group) {

        char accel;
        unsigned int keyval;
        GdkModifierType mods;

        accel = geda_menu_item_get_mnemonic ((GedaMenuItem*)image_menu_item);

        gtk_accelerator_parse (&accel, &keyval, &mods);
        gtk_widget_remove_accelerator ((GtkWidget*)image_menu_item, accel_group, keyval, mods);
      }
    }
  }
}

GtkAccelGroup *geda_image_menu_item_get_accel_group (GedaImageMenuItem *image_menu_item)
{
  GtkWidget *widget;

  g_return_val_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item), NULL);

  widget = geda_get_widget_parent(image_menu_item);

  if (GEDA_IS_MENU(widget)) {
    return geda_menu_widget_get_accel_group(widget);
  }
  return NULL;
}

/*!
 * \brief Set/Attach an image object to the menu item
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *
 * \param [in] image_menu_item a #GedaImageMenuItem.
 * \param [in] image           a widget to set as the image for the menu item.
 *
 * Sets the image of image_menu_item to the given widget.
 * Note that it depends on the show-menu-images setting whether
 * the image will be displayed or not.
 */
void geda_image_menu_item_set_image (GedaImageMenuItem *image_menu_item,
                                             GtkWidget *image)
{
  g_return_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item));

  if (image != image_menu_item->image) {

    if (image_menu_item->image) {
      geda_container_remove (image_menu_item, image_menu_item->image);
    }

    image_menu_item->image = image;

    if (image == NULL) {
      image_menu_item->show_image = FALSE;
    }
    else {

      gtk_widget_set_parent (image, (GtkWidget*)image_menu_item);

      g_object_set (image,
                    "visible", show_image (image_menu_item),
                    "no-show-all", TRUE,
                    NULL);

      GEDA_OBJECT_NOTIFY (image_menu_item, "image");
    }
  }
}

/*!
 * \brief Get the image object associated with the menu item
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *
 * \param [in] image_menu_item a #GedaImageMenuItem
 *
 * Gets the widget that is currently set as the image of
 * image_menu_item.
 *
 * \sa geda_image_menu_item_set_image().
 *
 * \returns the widget set as image of the GedaImageMenuItem
 */
GtkWidget *geda_image_menu_item_get_image (GedaImageMenuItem *image_menu_item)
{
  g_return_val_if_fail (GEDA_IS_IMAGE_MENU_ITEM (image_menu_item), NULL);

  return image_menu_item->image;
}

/*!
 * \brief Remove the image object attached to the menu item
 * \ingroup GedaImageMenuItem
 * \par Function Description
 *  GtkContainerClass->remove virtual over-ride
 */
static void geda_image_menu_item_remove (GtkContainer *container, GtkWidget *child)
{
  GedaImageMenuItem *image_menu_item;

  image_menu_item = (GedaImageMenuItem*)container;

  if (child == image_menu_item->image) {

    bool was_visible;

    was_visible = gtk_widget_get_visible (child);

    gtk_widget_unparent (child);
    image_menu_item->image = NULL;

    if (was_visible && gtk_widget_get_visible ((GtkWidget*)container))
    {
      gtk_widget_queue_resize ((GtkWidget*)container);
    }

    GEDA_OBJECT_NOTIFY (image_menu_item, "image");
  }
  else {
    ((GtkContainerClass*)geda_image_menu_item_parent_class)->remove (container, child);
  }
}

/** @} end group GedaImageMenuItem */
