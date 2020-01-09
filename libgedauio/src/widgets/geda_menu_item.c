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
/*! \file geda_menu_item.c
 *  \brief GedaMenuItem Class Module
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_accel_label.h"
#include "../../include/geda_action.h"
#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_keysyms.h"
#include "../../include/geda_menu_enum.h"
#include "../../include/geda_menu.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_separator.h" /* for GEDA_IS_MENU_SEPERATOR */
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_menu_bar.h"
#include "../../include/geda_tearoff_menu_item.h"
#include "../../include/geda_uio_functions.h"
#include "../../include/gettext.h"

/** \defgroup GedaMenuItem GedaMenuItem Object
 * @{
 * \brief A widget used for an item in menus
 * \par
 * The #GedaMenuItem and derivative widgets are the only valid children
 * for menus. Their function is to correctly handle alignment and events
 * and display text and submenus.
 *
 * A GedaMenuItem is derived from GtkBin and therfore can hold any valid
 * child widget, although only a few are really useful.
 *
 * By default, a GedaMenuItem sets a #GedaAccelLabel as its child.
 * GedaMenuItem has direct functions to set the label and its mnemonic.
 * For more advanced label settings, you can fetch the child widget from
 * the GtkBin.
 *
 * An example for setting markup and accelerator on a MenuItem:
 * \code{.c}
 * GtkWidget *child = geda_get_child_widget (menu_item);
 * geda_label_set_markup (GEDA_LABEL(child), "<i>new label</i> with <b>markup</b>");
 * geda_accel_label_set_accel (GEDA_ACCEL_LABEL(child), GDK_KEY_1, 0);
 * \endcode
 *
 * GedaMenuItem as GtkBuildable
 *
 * The GedaMenuItem implementation of the GtkBuildable interface supports
 * adding a submenu by specifying "submenu" as the "type" attribute of
 * a "child" element.
 *
 * An example of UI definition fragment with submenus:
 * \code{.html}
 * <object class="GedaMenuItem">
 *   <child type="submenu">
 *     <object class="GedaMenu"/>
 *   </child>
 * </object>
 * \endcode
 *
 * GedaMenuItem as GtkActivatable
 *
 * GedaMenuItem are typically created using geda_action_create_menu_item,
 * as shown in this example:
 * \code{.html}
 * action = geda_action_new (action_name,     // Action name
 *                           menu_item_name,  // Text
 *                           menu_item_tip,   // Tooltip
 *                           menu_icon_name,  // Icon stock ID
 *                           action_keys);    // Accelerator string
 * menu_item = geda_action_create_menu_item (action);
 * \endcode
 *
 * As a GtkActivatable an action can be associated with GedaMenuItems
 * using gtk_activatable_set_related_action or using the object properties
 * as shown in this example:
 * \code{.html}
 * g_object_set (menu_item, "related-action", action, NULL);
 * \endcode
 *
 * \note Associated actions are NOT referenced by GedaMenuItems but actions
 *       ARE unreferenced when GedaMenuItems are destroyed. This is for the
 *       convenience of applications, so that applications do not need to
 *       keep a list of actions to be destroyed or iterating over menu items
 *       in order to release actions. GedaMenuItems, #GedaAccelLabel and
 *       #GedaAction widgets will be destroyed when the containing application
 *       window is released. If for some reason an application needs to keep
 *       the actions alive, wrap the actions with g_object_ref when passing
 *       the action arguments as shown in this example:
 * \code{.html}
 * menu_item = geda_action_create_menu_item (g_object_ref(action));
 * \endcode
 *
 * \class GedaMenuBar geda_menu_item.h "include/geda_menu_item.h"
 * \implements GedaMenuShell
 *
 * \sa GtkBin, GedaMenuShell, geda_action_create_menu_item
 */

#define MENU_POPUP_DELAY     225

enum {
  ACTIVATE,
  ACTIVATE_ITEM,
  TOGGLE_SIZE_REQUEST,
  TOGGLE_SIZE_ALLOCATE,
  SELECT,
  DESELECT,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_RIGHT_JUSTIFIED,
  PROP_SUBMENU,
  PROP_ACCEL_PATH,
  PROP_LABEL,
  PROP_MNEMONIC,
  PROP_USE_UNDERLINE,

  /* activatable properties */
  PROP_ACTIVATABLE_RELATED_ACTION,
  PROP_ACTIVATABLE_USE_ACTION_APPEARANCE,

  PROP_ACTION_NAME,
  PROP_ACTION_TARGET
};

struct _GedaMenuItemPrivate
{
  GtkWidget *submenu;
  GdkWindow *event_window;

  unsigned short toggle_size;
  unsigned short accelerator_width;

  unsigned int timer;

  char        *accel_path;

  GedaAction  *action;
  void        *action_helper;

  char         mnemonic;

  unsigned int show_submenu_indicator : 1;
  unsigned int submenu_placement      : 1;
  unsigned int submenu_direction      : 1;
  unsigned int right_justify          : 1;
  unsigned int timer_from_keypress    : 1;
  unsigned int from_menubar           : 1;
  unsigned int use_action_appearance  : 1;
  unsigned int reserve_indicator      : 1;
};

static void geda_menu_item_dispose                   (GObject          *object);
static void geda_menu_item_finalize                  (GObject          *object);
static void geda_menu_item_do_set_right_justified    (GedaMenuItem     *menu_item,
                                                      bool              set_justified);
static void geda_menu_item_set_property              (GObject          *object,
                                                      unsigned int      prop_id,
                                                      const GValue     *value,
                                                      GParamSpec       *pspec);
static void geda_menu_item_get_property              (GObject          *object,
                                                      unsigned int      prop_id,
                                                      GValue           *value,
                                                      GParamSpec       *pspec);
//static void geda_menu_item_destroy                 (GtkWidget        *widget);
static void geda_menu_item_realize                   (GtkWidget        *widget);
static void geda_menu_item_unrealize                 (GtkWidget        *widget);
static void geda_menu_item_map                       (GtkWidget        *widget);
static void geda_menu_item_unmap                     (GtkWidget        *widget);
static bool geda_menu_item_enter                     (GtkWidget        *widget,
                                                      GdkEventCrossing *event);
static bool geda_menu_item_leave                     (GtkWidget        *widget,
                                                      GdkEventCrossing *event);
static void geda_menu_item_parent_set                (GtkWidget        *widget,
                                                      GtkWidget        *previous_parent);
static void geda_menu_item_activate_action           (GedaMenuItem     *item);

static void geda_real_menu_item_select               (GedaMenuItem     *item);
static void geda_real_menu_item_deselect             (GedaMenuItem     *item);
static void geda_real_menu_item_activate_item        (GedaMenuItem     *item);
static void geda_real_menu_item_toggle_size_request  (GedaMenuItem     *menu_item,
                                                      int              *requisition);
static void geda_real_menu_item_toggle_size_allocate (GedaMenuItem     *menu_item,
                                                      int               allocation);
static bool geda_menu_item_mnemonic_activate         (GtkWidget        *widget,
                                                      bool              group_cycling);

static void geda_menu_item_ensure_label              (GedaMenuItem     *menu_item);
static int  geda_menu_item_popup_timeout             (void             *data);
static void geda_menu_item_position_menu             (GedaMenu          *menu,
                                                      int              *x,
                                                      int              *y,
                                                      bool             *push_in,
                                                      void             *user_data);
static void geda_menu_item_show_all                  (GtkWidget        *widget);
static void geda_menu_item_hide_all                  (GtkWidget        *widget);
static void geda_menu_item_forall                    (GtkContainer     *container,
                                                      bool              include_internals,
                                                      GtkCallback       callback,
                                                      void             *callback_data);
static bool geda_menu_item_can_activate_accel        (GtkWidget        *widget,
                                                      unsigned          int signal_id);

static void geda_real_menu_item_set_label            (GedaMenuItem     *menu_item,
                                                      const char       *label);
static const char *geda_real_menu_item_get_label     (GedaMenuItem     *menu_item);

static void geda_menu_item_buildable_interface_init  (GtkBuildableIface *iface);
static void geda_menu_item_buildable_add_child       (GtkBuildable      *buildable,
                                                      GtkBuilder        *builder,
                                                      GObject           *child,
                                                      const char        *type);
static void geda_menu_item_buildable_custom_finished (GtkBuildable      *buildable,
                                                      GtkBuilder        *builder,
                                                      GObject           *child,
                                                      const char        *tagname,
                                                      void              *user_data);

static void geda_menu_item_activatable_interface_init (GtkActivatableIface *iface);
static void geda_menu_item_update                     (GtkActivatable      *activatable,
                                                       GtkAction           *action,
                                                       const char          *property_name);
static void geda_menu_item_sync_action_properties     (GtkActivatable      *activatable,
                                                       GtkAction           *action);

static void geda_menu_item_size_allocate              (GtkWidget           *widget,
                                                       GtkAllocation       *allocation);
#if GTK_MAJOR_VERSION < 3

static int  geda_menu_item_expose                     (GtkWidget           *widget,
                                                       GdkEventExpose      *event);
static void geda_menu_item_paint                      (GtkWidget           *widget,
                                                       GdkRectangle        *area);
static void geda_menu_item_size_request               (GtkWidget           *widget,
                                                       GtkRequisition      *requisition);
#else

static bool geda_menu_item_draw                           (GtkWidget          *widget,
                                                           cairo_t            *cr);
static void geda_menu_item_get_preferred_width            (GtkWidget          *widget,
                                                           int                *minimum_size,
                                                           int                *natural_size);
static void geda_menu_item_get_preferred_height           (GtkWidget          *widget,
                                                           int                *minimum_size,
                                                           int                *natural_size);
static void geda_menu_item_get_preferred_height_for_width (GtkWidget          *widget,
                                                           int                 for_size,
                                                           int                *minimum_size,
                                                           int                *natural_size);
static void geda_menu_item_actionable_interface_init  (GtkActionableInterface *iface);

#endif

static unsigned int menu_item_signals[LAST_SIGNAL] = { 0 };

static GtkBuildableIface *parent_buildable_iface;

/* Table of pointers to GedaMenuItem instances */
static GHashTable *menu_item_hash_table = NULL;

static void *geda_menu_item_parent_class = NULL;

static inline void geda_menu_item_disconnect_accelerator(GedaAction *action)
{
  if (GEDA_IS_ACTION (action)) {
    gtk_action_disconnect_accelerator ((GtkAction*)action);
  }
}

static void geda_menu_item_selection_done (GedaMenu *menu, GedaMenuItem *menu_item)
{
  /* A submenu was activated or torn-off, so deselect parent */
  geda_real_menu_item_deselect(menu_item);
}

static void geda_menu_item_detacher (GtkWidget *widget, GedaMenu *menu)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(widget);
  GedaMenuItemPrivate *priv = menu_item->priv;

  g_return_if_fail (priv->submenu == (GtkWidget*)menu);

  g_signal_handlers_disconnect_by_func (menu,
                                        geda_menu_item_selection_done,
                                        menu_item);
  priv->submenu = NULL;
}

#if (GTK_MAJOR_VERSION == 3)

static void geda_menu_item_set_action_name (GtkActionable *actionable,
                                            const char    *action_name)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(actionable);

  if (!menu_item->priv->action_helper) {
    menu_item->priv->action_helper = gtk_action_helper_new (actionable);
  }
  gtk_action_helper_set_action_name (menu_item->priv->action_helper, action_name);
}

static void geda_menu_item_set_action_target_value (GtkActionable *actionable,
                                                    GVariant      *action_target)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(actionable);

  if (!menu_item->priv->action_helper) {
    menu_item->priv->action_helper = gtk_action_helper_new (actionable);
  }
  gtk_action_helper_set_action_target_value (menu_item->priv->action_helper, action_target);
}

static const char *geda_menu_item_get_action_name (GtkActionable *actionable)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(actionable);

  return gtk_action_helper_get_action_name (menu_item->priv->action_helper);
}

static GVariant *geda_menu_item_get_action_target_value (GtkActionable *actionable)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(actionable);

  return gtk_action_helper_get_action_target_value (menu_item->priv->action_helper);
}

static void geda_menu_item_actionable_interface_init (GtkActionableInterface *iface)
{
  iface->set_action_name = geda_menu_item_set_action_name;
  iface->get_action_name = geda_menu_item_get_action_name;
  iface->set_action_target_value = geda_menu_item_set_action_target_value;
  iface->get_action_target_value = geda_menu_item_get_action_target_value;
}

static void geda_menu_item_destroy (GtkWidget *widget)
{
  GedaMenuItem *menu_item = (GedaMenuItem*)widget;
  GtkWidget    *child;

  child = geda_get_child_widget (menu_item);

  if (GEDA_IS_LABEL(child)) {
    geda_container_remove (menu_item, child);
  }

  ((GtkWidgetClass*)geda_menu_item_parent_class)->destroy (widget);
}

#else

static void geda_menu_item_destroy (GtkObject *object)
{
  GedaMenuItem *menu_item = (GedaMenuItem*)object;
  GtkWidget    *child;

  child = geda_get_child_widget (menu_item);

  if (GEDA_IS_LABEL(child)) {
    geda_container_remove (menu_item, child);
  }

  ((GtkObjectClass*)geda_menu_item_parent_class)->destroy (object);
}

#endif

static void geda_menu_item_do_set_right_justified (GedaMenuItem *menu_item,
                                                   bool          right_justified)
{
  GedaMenuItemPrivate *priv = menu_item->priv;

  right_justified = right_justified != FALSE;

  if (priv->right_justify != right_justified) {

    priv->right_justify = right_justified;

    gtk_widget_queue_resize ((GtkWidget*)menu_item);
  }
}

/* GObject Over-rides*/

/*! \internal gobject_class->dispose */
static void geda_menu_item_dispose (GObject *object)
{
  GedaMenuItem        *menu_item = (GedaMenuItem*)object;
  GedaMenuItemPrivate *priv      = menu_item->priv;

  if (priv->action) {
    geda_menu_item_disconnect_accelerator(priv->action);
    gtk_activatable_do_set_related_action ((GtkActivatable*)menu_item, NULL);
    priv->action = NULL;
  }

  if (priv->submenu) {

    GedaMenu *menu = GEDA_MENU(priv->submenu);

    g_signal_handlers_disconnect_by_func (menu,
                                          geda_menu_item_selection_done,
                                          menu_item);
    geda_menu_set_accel_group (menu, NULL);
    geda_menu_detach(menu);

    priv->submenu = NULL;
  }

  ((GObjectClass*)geda_menu_item_parent_class)->dispose (object);
}

/*! \internal gobject_class->finalize */
static void geda_menu_item_finalize (GObject *object)
{
  GedaMenuItem *menu_item = (GedaMenuItem*)object;

  if (g_hash_table_remove (menu_item_hash_table, object)) {
    if (!g_hash_table_size (menu_item_hash_table)) {
      g_hash_table_destroy (menu_item_hash_table);
      menu_item_hash_table = NULL;
    }
  }

  g_free(menu_item->priv);

  ((GObjectClass*)geda_menu_item_parent_class)->finalize (object);
}

static void geda_menu_item_set_property (GObject      *object,
                                         unsigned int  prop_id,
                                         const GValue *value,
                                         GParamSpec   *pspec)
{
  GedaMenuItem *menu_item = (GedaMenuItem*)object;

  switch (prop_id) {

    case PROP_RIGHT_JUSTIFIED:
      geda_menu_item_do_set_right_justified (menu_item, g_value_get_boolean (value));
      break;

    case PROP_SUBMENU:
      geda_menu_item_set_submenu_widget (menu_item, g_value_get_object (value));
      break;

    case PROP_ACCEL_PATH:
      geda_menu_item_set_accel_path (menu_item, g_value_get_string (value));
      break;

    case PROP_LABEL:
      geda_menu_item_set_label (menu_item, g_value_get_string (value));
      break;

    case PROP_MNEMONIC:
      geda_menu_item_set_mnemonic (menu_item, g_value_get_int(value));
      break;

    case PROP_USE_UNDERLINE:
      geda_menu_item_set_use_underline (menu_item, g_value_get_boolean (value));
      break;

    case PROP_ACTIVATABLE_RELATED_ACTION:
      geda_menu_item_set_related_action (menu_item, g_value_get_object (value));
      break;

    case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
      geda_menu_item_set_use_action_appearance (menu_item, g_value_get_boolean (value));
      break;

    case PROP_ACTION_NAME:
#if (GTK_MAJOR_VERSION == 3)
      geda_menu_item_set_action_name ((GtkActionable*)menu_item, g_value_get_string (value));
      break;

    case PROP_ACTION_TARGET:
      geda_menu_item_set_action_target_value ((GtkActionable*)menu_item, g_value_get_variant (value));
#endif
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_menu_item_get_property (GObject     *object,
                                         unsigned int prop_id,
                                         GValue      *value,
                                         GParamSpec  *pspec)
{
  GedaMenuItem *menu_item = (GedaMenuItem*)object;
  GedaMenuItemPrivate *priv = menu_item->priv;

  switch (prop_id) {
    case PROP_RIGHT_JUSTIFIED:
      g_value_set_boolean (value, menu_item->priv->right_justify);
      break;

    case PROP_SUBMENU:
      g_value_set_object (value, geda_menu_item_get_submenu_widget (menu_item));
      break;

    case PROP_ACCEL_PATH:
      g_value_set_string (value, geda_menu_item_get_accel_path (menu_item));
      break;

    case PROP_LABEL:
      g_value_set_string (value, geda_menu_item_get_label (menu_item));
      break;

    case PROP_MNEMONIC:
      g_value_set_int (value, geda_menu_item_get_mnemonic (menu_item));
      break;

    case PROP_USE_UNDERLINE:
      g_value_set_boolean (value, geda_menu_item_get_use_underline (menu_item));
      break;

    case PROP_ACTIVATABLE_RELATED_ACTION:
      g_value_set_object (value, priv->action);
      break;

    case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
      g_value_set_boolean (value, priv->use_action_appearance);
      break;
    case PROP_ACTION_NAME:
#if (GTK_MAJOR_VERSION == 3)
      g_value_set_string (value, gtk_action_helper_get_action_name (priv->action_helper));
      break;

    case PROP_ACTION_TARGET:
      g_value_set_variant (value, gtk_action_helper_get_action_target_value (priv->action_helper));
#endif
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

/* Widget Class Overrides */

/*! \internal widget_class->enter_notify_event */
static bool geda_menu_item_enter (GtkWidget *widget, GdkEventCrossing *event)
{
  g_return_val_if_fail (event != NULL, FALSE);

  return gtk_widget_event (gtk_widget_get_parent (widget), (GdkEvent *) event);
}

/*! \internal widget_class->leave_notify_event */
static bool geda_menu_item_leave (GtkWidget *widget, GdkEventCrossing *event)
{
  g_return_val_if_fail (event != NULL, FALSE);

  return gtk_widget_event (gtk_widget_get_parent (widget), (GdkEvent*) event);
}

/*! \internal widget_class->map */
static void geda_menu_item_map (GtkWidget *widget)
{
  GedaMenuItem *menu_item = (GedaMenuItem*)widget;
  GedaMenuItemPrivate *priv = menu_item->priv;

  ((GtkWidgetClass*)geda_menu_item_parent_class)->map (widget);

  gdk_window_show (priv->event_window);
}

/*! \internal widget_class->unmap */
static void geda_menu_item_unmap (GtkWidget *widget)
{
  GedaMenuItem *menu_item = (GedaMenuItem*)widget;
  GedaMenuItemPrivate *priv = menu_item->priv;

  gdk_window_hide (priv->event_window);

  ((GtkWidgetClass*)geda_menu_item_parent_class)->unmap (widget);
}

/*! \internal widget_class->hide_all */
static void geda_menu_item_hide_all (GtkWidget *widget)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(widget);
  GedaMenuItemPrivate *priv = menu_item->priv;

  gtk_widget_hide (widget);

  /* hide children including submenu */
  geda_container_foreach (widget, gtk_widget_hide_all, NULL);

  if (priv->submenu) {
    gtk_widget_hide_all (priv->submenu);
  }
}

/*! \internal widget_class->show_all */
static void geda_menu_item_show_all (GtkWidget *widget)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(widget);
  GedaMenuItemPrivate *priv = menu_item->priv;

  /* show children including submenu */
  if (priv->submenu) {
    gtk_widget_show_all (priv->submenu);
  }
  geda_container_foreach (widget, gtk_widget_show_all, NULL);

  gtk_widget_show (widget);
}

/*! \internal widget_class->can_activate_accel */
static bool geda_menu_item_can_activate_accel (GtkWidget   *widget,
                                               unsigned int signal_id)
{
  GtkWidget *parent;

  parent = gtk_widget_get_parent (widget);

  /* Chain to the parent GedaMenu for further checks */
  return (gtk_widget_is_sensitive (widget) &&
          gtk_widget_get_visible (widget) &&
          parent && gtk_widget_can_activate_accel (parent, signal_id));
}

/*! \internal widget_class->mnemonic_activate */
static bool geda_menu_item_mnemonic_activate (GtkWidget *widget, bool group_cycling)
{
  GtkWidget *parent = gtk_widget_get_parent (widget);

  if (GEDA_IS_MENU_SHELL (parent)) {

    geda_menu_shell_set_keyboard_mode ((GedaMenuShell*)parent, TRUE);

    if (group_cycling && ((GedaMenuShell*)parent)->active) {

      geda_menu_shell_select_item ((GedaMenuShell*)parent, widget);
    }
  }
  else {
    g_signal_emit (widget, menu_item_signals[ACTIVATE_ITEM], 0);
  }

  return TRUE;
}

/*! \internal widget_class->parent_set */
static void geda_menu_item_parent_set (GtkWidget *widget,
                                       GtkWidget *previous_parent)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(widget);
  GedaMenu     *menu;
  GtkWidget    *parent;

  parent = gtk_widget_get_parent (widget);
  menu   = GEDA_IS_MENU(parent) ? GEDA_MENU(parent) : NULL;

  if (menu) {
    geda_menu_item_refresh_accel_path (menu_item,
                                       menu->accel_path,
                                       menu->accel_group,
                                       TRUE);
  }

  if (((GtkWidgetClass*)geda_menu_item_parent_class)->parent_set) {
    ((GtkWidgetClass*)geda_menu_item_parent_class)->parent_set (widget, previous_parent);
  }
}

/*! \internal widget_class->realize */
static void
geda_menu_item_realize (GtkWidget *widget)
{
  GedaMenuItem        *menu_item   = GEDA_MENU_ITEM(widget);
  GedaMenuItemPrivate *priv        = menu_item->priv;
  GdkWindow           *window;
  GtkAllocation        allocation;
  GdkWindowAttr        attributes;
  int                  attributes_mask;

  gtk_widget_set_realized (widget, TRUE);

  window = gtk_widget_get_parent_window (widget);

  gtk_widget_set_window (widget, window);
  g_object_ref (window);

  gtk_widget_get_allocation (widget, &allocation);

  attributes.x           = allocation.x;
  attributes.y           = allocation.y;
  attributes.width       = allocation.width;
  attributes.height      = allocation.height;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.wclass      = GDK_INPUT_ONLY;
  attributes.event_mask  = (gtk_widget_get_events (widget) |
                                     GDK_BUTTON_PRESS_MASK |
                                     GDK_BUTTON_RELEASE_MASK |
                                     GDK_ENTER_NOTIFY_MASK |
                                     GDK_LEAVE_NOTIFY_MASK |
                                     GDK_POINTER_MOTION_MASK);

  attributes_mask = GDK_WA_X | GDK_WA_Y;

  priv->event_window = gdk_window_new (window, &attributes, attributes_mask);

#if GTK_MAJOR_VERSION < 3

  gdk_window_set_user_data (priv->event_window, widget);
  widget->style = gtk_style_attach (widget->style, widget->window);

#else

  gtk_widget_register_window (widget, priv->event_window);

#endif

}

/*! \internal widget_class->unrealize */
static void geda_menu_item_unrealize (GtkWidget *widget)
{
  GedaMenuItem *menu_item   = GEDA_MENU_ITEM(widget);
  GedaMenuItemPrivate *priv = menu_item->priv;

#if GTK_MAJOR_VERSION < 3

  gdk_window_set_user_data (priv->event_window, NULL);

#else

  gtk_widget_unregister_window (widget, priv->event_window);

#endif

  gdk_window_destroy (priv->event_window);
  priv->event_window = NULL;

  ((GtkWidgetClass*)geda_menu_item_parent_class)->unrealize (widget);
}

/*!
 * \brief GedaMenuSeparator Class Initializer
 * \par Function Description
 *  Function is called to initialize the class instance.
 *
 * \param [in] class      A GedaMenuSeparatorClass Object
 * \param [in] class_data GedaMenuSeparator structure associated with the class
 */
static void geda_menu_item_class_init (void *class, void *class_data)
{
  GObjectClass      *gobject_class   = (GObjectClass*)class;
  GtkWidgetClass    *widget_class    = (GtkWidgetClass*)class;
  GtkContainerClass *container_class = (GtkContainerClass*)class;
  GedaMenuItemClass *menu_item_class = (GedaMenuItemClass*)class;
  GParamSpec        *params;

  gobject_class->dispose           = geda_menu_item_dispose;
  gobject_class->finalize          = geda_menu_item_finalize;
  gobject_class->set_property      = geda_menu_item_set_property;
  gobject_class->get_property      = geda_menu_item_get_property;

#if GTK_MAJOR_VERSION < 3

  GtkObjectClass *object_class     = (GtkObjectClass*)class;
  object_class->destroy            = geda_menu_item_destroy;

#else

  widget_class->destroy            = geda_menu_item_destroy;

#endif

  widget_class->enter_notify_event = geda_menu_item_enter;
  widget_class->leave_notify_event = geda_menu_item_leave;
  widget_class->map                = geda_menu_item_map;
  widget_class->unmap              = geda_menu_item_unmap;
  widget_class->hide_all           = geda_menu_item_hide_all;
  widget_class->show_all           = geda_menu_item_show_all;
  widget_class->can_activate_accel = geda_menu_item_can_activate_accel;
  widget_class->mnemonic_activate  = geda_menu_item_mnemonic_activate;
  widget_class->parent_set         = geda_menu_item_parent_set;
  widget_class->realize            = geda_menu_item_realize;
  widget_class->unrealize          = geda_menu_item_unrealize;
  widget_class->size_allocate      = geda_menu_item_size_allocate;

#if GTK_MAJOR_VERSION < 3

  widget_class->size_request       = geda_menu_item_size_request;
  widget_class->expose_event       = geda_menu_item_expose;

#else

  widget_class->draw                 = geda_menu_item_draw;
  widget_class->get_preferred_width  = geda_menu_item_get_preferred_width;
  widget_class->get_preferred_height = geda_menu_item_get_preferred_height;
  widget_class->get_preferred_height_for_width = geda_menu_item_get_preferred_height_for_width;

  gtk_widget_class_set_accessible_type (widget_class, GEDA_TYPE_MENU_ITEM_ACCESSIBLE);

#endif

  container_class->forall               = geda_menu_item_forall;

  menu_item_class->activate             = geda_menu_item_activate_action;
  menu_item_class->activate_item        = geda_real_menu_item_activate_item;
  menu_item_class->toggle_size_request  = geda_real_menu_item_toggle_size_request;
  menu_item_class->toggle_size_allocate = geda_real_menu_item_toggle_size_allocate;
  menu_item_class->set_label            = geda_real_menu_item_set_label;
  menu_item_class->get_label            = geda_real_menu_item_get_label;
  menu_item_class->select               = geda_real_menu_item_select;
  menu_item_class->deselect             = geda_real_menu_item_deselect;

  menu_item_class->hide_on_activate     = TRUE;

  geda_menu_item_parent_class           = g_type_class_peek_parent(class);

  /*!
   * property "right-justified": GedaMenuItem::right-justified
   * \par
   * Sets whether the menu item appears justified
   * at the right side of a menu bar.
   */
  g_object_class_install_property (gobject_class,
                                   PROP_RIGHT_JUSTIFIED,
                                   g_param_spec_boolean ("right-justified",
                                                       _("Right Justified"),
                                                       _("Sets whether the menu item appears justified at the right side of a menu bar"),
                                                         FALSE,
                                                         G_PARAM_READWRITE));

  /*!
   * property "submenu": GedaMenuItem::submenu
   * \par
   * The submenu attached to the menu item, or %NULL if it has none.
   */
  g_object_class_install_property (gobject_class,
                                   PROP_SUBMENU,
                                   g_param_spec_object ("submenu",
                                                      _("Submenu"),
                                                      _("The submenu attached to the menu item, or NULL if it has none"),
                                                        GTK_TYPE_MENU,
                                                        G_PARAM_READWRITE));

  /*!
   * property "accel-path": GedaMenuItem::accel-path
   * \par
   * Sets the accelerator path of the menu item, through which runtime
   * changes of the menu item's accelerator caused by the user can be
   * identified and saved to persistant storage.
   */
  g_object_class_install_property (gobject_class,
                                   PROP_ACCEL_PATH,
                                   g_param_spec_string ("accel-path",
                                                      _("Accel Path"),
                                                      _("Sets the accelerator path of the menu item"),
                                                        NULL,
                                                        G_PARAM_READWRITE));

  /*!
   * property "label": GedaMenuItem::label
   * \par
   * The text for the child label.
   */
  g_object_class_install_property (gobject_class,
                                   PROP_LABEL,
                                   g_param_spec_string ("label",
                                                      _("Label"),
                                                      _("The text for the child label"),
                                                        "",
                                                        G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
                                   PROP_MNEMONIC,
                                   g_param_spec_string ("mnemonic",
                                                      _("mnemonic"),
                                                      _("The char for the mnemonic"),
                                                        "",
                                                        G_PARAM_READWRITE));
  /*!
   * GedaMenuItem::use-underline
   * \par
   * %TRUE if underlines in the text indicate mnemonics.
   */
  g_object_class_install_property (gobject_class,
                                   PROP_USE_UNDERLINE,
                                   g_param_spec_boolean ("use-underline",
                                                       _("Use underline"),
                                                       _("If set, an underline in the text indicates "
                                                         "the next character should be used for the "
                                                         "mnemonic accelerator key"),
                                                         FALSE,
                                                         G_PARAM_READWRITE));

  g_object_class_override_property (gobject_class,
                                    PROP_ACTIVATABLE_RELATED_ACTION,
                                    "related-action");

  g_object_class_override_property (gobject_class,
                                    PROP_ACTIVATABLE_USE_ACTION_APPEARANCE,
                                    "use-action-appearance");

#if GTK_MAJOR_VERSION == 3

  g_object_class_override_property (gobject_class, PROP_ACTION_NAME, "action-name");

  g_object_class_override_property (gobject_class, PROP_ACTION_TARGET, "action-target");

#endif

  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_float ("arrow-scaling",
                                                             _("Arrow Scaling"),
                                                             _("Amount of space used up by arrow, relative to the menu item's font size"),
                                                               0.0, 2.0, 0.8,
                                                               G_PARAM_READABLE));

  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_int ("arrow-spacing",
                                                           _("Arrow Spacing"),
                                                           _("Space between label and arrow"),
                                                             0,
                                                             INT_MAX,
                                                             10,
                                                             G_PARAM_READABLE));

  /*!
   * GedaMenuItem::horizontal-padding
   * \par
   * Padding to left and right of the menu item.
   */
  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_int ("horizontal-padding",
                                                           _("Horizontal Padding"),
                                                           _("Padding to left and right of the menu item"),
                                                             0,
                                                             INT_MAX,
                                                             3,
                                                             G_PARAM_READABLE));

  gtk_widget_class_install_style_property_parser (widget_class,
                                                  g_param_spec_enum ("selected-shadow-type",
                                                                   _("Selected Shadow Type"),
                                                                   _("Shadow type when item is selected"),
                                                                     GTK_TYPE_SHADOW_TYPE,
                                                                     GTK_SHADOW_NONE,
                                                                     G_PARAM_READABLE),
                                                  gtk_rc_property_parse_enum);

  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_int ("toggle-spacing",
                                                           _("Icon Spacing"),
                                                           _("Space between icon and label"),
                                                             0,
                                                             INT_MAX,
                                                             5,
                                                             G_PARAM_READABLE));

  /*!
   * property "vertical-padding": GedaMenuItem::vertical-padding
   * \brief Controls the vertical spacing between menu items.
   */
  params = g_param_spec_int ("vertical-padding",
                           _("Vertical Padding"),
                           _("Vertical spacing between menu items"),
                              0,
                              100,
                              1,
                              G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  /*!
   * GedaMenuItem::width-chars
   * \par
   * The minimum desired width of the menu item in characters.
   */
  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_int ("width-chars",
                                                           _("Width in Characters"),
                                                           _("The minimum desired width of the menu item in characters"),
                                                             0, INT_MAX, 12,
                                                             G_PARAM_READABLE));
  /* -==- signals -==- */

  /*!
   * GedaMenuItem::activate:
   * \par
   * Emitted when the item is activated.
   *
   * menuitem: the object which received the signal.
   */
  menu_item_signals[ACTIVATE] =
    g_signal_new ("activate",
                  GEDA_TYPE_MENU_ITEM,
                  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (GedaMenuItemClass, activate),
                  NULL, NULL,
                  geda_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);
  widget_class->activate_signal = menu_item_signals[ACTIVATE];

  /*!
   * GedaMenuItem::activate-item:
   * \par
   * Emitted when the item is activated, but also if the menu item has a
   * submenu. For normal applications, the relevant signal is
   * GedaMenuItem::activate.
   *
   * param menuitem the object which received the signal.
   */
  menu_item_signals[ACTIVATE_ITEM] =
    g_signal_new ("activate-item",
                  GEDA_TYPE_MENU_ITEM,
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (GedaMenuItemClass, activate_item),
                  NULL, NULL,
                  geda_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  menu_item_signals[TOGGLE_SIZE_REQUEST] =
    g_signal_new ("toggle-size-request",
                  GEDA_TYPE_MENU_ITEM,
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (GedaMenuItemClass, toggle_size_request),
                  NULL, NULL,
                  geda_marshal_VOID__POINTER,
                  G_TYPE_NONE, 1,
                  G_TYPE_POINTER);

  menu_item_signals[TOGGLE_SIZE_ALLOCATE] =
    g_signal_new ("toggle-size-allocate",
                  GEDA_TYPE_MENU_ITEM,
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (GedaMenuItemClass, toggle_size_allocate),
                  NULL, NULL,
                  geda_marshal_VOID__INT,
                  G_TYPE_NONE, 1,
                  G_TYPE_INT);

  menu_item_signals[SELECT] =
    g_signal_new ("select",
                  GEDA_TYPE_MENU_ITEM,
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (GedaMenuItemClass, select),
                  NULL, NULL,
                  geda_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  menu_item_signals[DESELECT] =
    g_signal_new ("deselect",
                  GEDA_TYPE_MENU_ITEM,
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (GedaMenuItemClass, deselect),
                  NULL, NULL,
                  geda_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);
}

/*!
 * \brief Type instance initializer for GedaMenuItem
 * \par Function Description
 *  Type instance initializer for GedaMenuItem, initializes a new empty
 *  GedaMenuItem object.
 *
 * \param [in] instance The GedaMenuItem structure being initialized,
 * \param [in] class    The GedaMenuItem class being initializing.
 */
static void geda_menu_item_instance_init(GTypeInstance *instance, void *class)
{
  GedaMenuItem        *menu_item = (GedaMenuItem*)instance;
  GedaMenuItemPrivate *priv;

  priv            = g_malloc0 (sizeof(GedaMenuItemPrivate));
  menu_item->priv = priv;

  priv->use_action_appearance = TRUE;
  priv->mnemonic              = (char)GDK_KEY_VoidSymbol;

  gtk_widget_set_has_window ((GtkWidget*)menu_item, FALSE);

  if (gtk_widget_get_direction ((GtkWidget*)menu_item) == GTK_TEXT_DIR_RTL)
  {
    priv->submenu_direction = GTK_DIRECTION_LEFT;
  }
  else
  {
    priv->submenu_direction = GTK_DIRECTION_RIGHT;
  }

#if (GTK_MAJOR_VERSION == 3)

  GtkStyleContext *context;
  context = gtk_widget_get_style_context ((GtkWidget*)menu_item);
  gtk_style_context_add_class (context, GTK_STYLE_CLASS_MENUITEM);

#endif

  if (!menu_item_hash_table) {
    menu_item_hash_table = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (menu_item_hash_table, instance, instance);
}

/*!
 * \brief Retrieve GedaMenuItem's Type identifier
 * \par Function Description
 *  Function to retrieve a #GedaMenuItem Type identifier. When
 *  first called, the function registers a #GedaMenuItem in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaMenuItem and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 * \return GedaType identifier associated with GedaMenuItem.
 */
GedaType geda_menu_item_get_type (void)
{
  static volatile GedaType geda_menu_item_type = 0;

  if (g_once_init_enter (&geda_menu_item_type)) {

    static const GTypeInfo info = {
      sizeof(GedaMenuItemClass),
      NULL,                           /* base_init           */
      NULL,                           /* base_finalize       */
      geda_menu_item_class_init,      /* (GClassInitFunc)    */
      NULL,                           /* class_finalize      */
      NULL,                           /* class_data          */
      sizeof(GedaMenuItem),
      0,                              /* n_preallocs         */
      geda_menu_item_instance_init    /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaMenuItem");
    type   = g_type_register_static (GTK_TYPE_BIN, string, &info, 0);

    const GInterfaceInfo buildable_info = {
      (GInterfaceInitFunc) geda_menu_item_buildable_interface_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_BUILDABLE, &buildable_info);

    const GInterfaceInfo activatable_info = {
      (GInterfaceInitFunc) geda_menu_item_activatable_interface_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_ACTIVATABLE, &activatable_info);

#if (GTK_MAJOR_VERSION == 3)

    const GInterfaceInfo actionable_info = {
      (GInterfaceInitFunc) geda_menu_item_actionable_interface_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_ACTIONABLE, &actionable_info);

#endif

    g_once_init_leave (&geda_menu_item_type, type);
  }

  return geda_menu_item_type;
}

/*!
 * \brief Check if an object is a GedaMenuButton
 * \par Function Description
 *  Determines if \a menu_item is valid by verifying \a menu_item
 *  is included in the hash table of GedaMenuButton objects.
 *
 * \return TRUE if \a menu_item is a valid GedaMenuButton
 */
bool is_a_geda_menu_item (GedaMenuItem *menu_item)
{
  if ((menu_item != NULL) && (menu_item_hash_table != NULL)) {
    return g_hash_table_lookup(menu_item_hash_table, menu_item) ? TRUE : FALSE;
  }

  return FALSE;
}

static void geda_menu_item_accel_width_foreach (GtkWidget *widget, void *data)
{
  unsigned int *width = data;

  if (GEDA_IS_ACCEL_LABEL(widget)) {

    unsigned int w;

    w = geda_accel_label_get_accel_width ((GedaAccelLabel*)widget);

    *width = MAX (*width, w);
  }
  else if (GTK_IS_CONTAINER(widget)) {

    geda_container_foreach (widget,
                            geda_menu_item_accel_width_foreach,
                            data);
  }
}

static void geda_menu_item_buildable_interface_init (GtkBuildableIface *iface)
{
  parent_buildable_iface = g_type_interface_peek_parent (iface);
  iface->add_child       = geda_menu_item_buildable_add_child;
  iface->custom_finished = geda_menu_item_buildable_custom_finished;
}

static void geda_menu_item_buildable_add_child (GtkBuildable *buildable,
                                                GtkBuilder   *builder,
                                                GObject      *child,
                                                const char   *type)
{
  if (type && strcmp (type, "submenu") == 0) {
    geda_menu_item_set_submenu_widget (GEDA_MENU_ITEM(buildable), (GtkWidget*)child);
  }
  else {
    parent_buildable_iface->add_child (buildable, builder, child, type);
  }
}

static void geda_menu_item_buildable_custom_finished (GtkBuildable *buildable,
                                                      GtkBuilder   *builder,
                                                      GObject      *child,
                                                      const char   *tagname,
                                                      void         *user_data)
{
  if (strcmp (tagname, "accelerator") == 0) {

    GedaMenuShell *menu_shell;
    GtkWidget     *toplevel;

    menu_shell = (GedaMenuShell*)gtk_widget_get_parent ((GtkWidget*)buildable);

    if (menu_shell) {

      GtkWidget *attach;

      while (GEDA_IS_MENU(menu_shell) &&
            (attach = geda_menu_get_attach_widget ((GedaMenu*)(menu_shell))) != NULL)
      {
        menu_shell = (GedaMenuShell*)gtk_widget_get_parent (attach);
      }

      toplevel = gtk_widget_get_toplevel ((GtkWidget*)menu_shell);
    }
    else {

      /* Fall back to something ... */
      toplevel = gtk_widget_get_toplevel ((GtkWidget*)buildable);

      g_warning ("found a GedaMenuItem <%s> without a parent GedaMenuShell, assigned accelerators wont work.",
                 gtk_buildable_get_name (buildable));
    }

    /* Feed the correct toplevel to the Widget accelerator parsing code */
    geda_widget_buildable_finish_accelerator ((GtkWidget*)buildable, toplevel, user_data);
  }
  else {
    parent_buildable_iface->custom_finished (buildable, builder, child, tagname, user_data);
  }
}

static void geda_menu_item_activatable_interface_init (GtkActivatableIface *iface)
{
  iface->update = geda_menu_item_update;
  iface->sync_action_properties = geda_menu_item_sync_action_properties;
}

static void activatable_update_label (GedaMenuItem *menu_item, GtkAction *action)
{
  if (GEDA_IS_LABEL (geda_get_child_widget (menu_item))) {

    const char *label;

    label = gtk_action_get_label (action);

    geda_menu_item_set_label (menu_item, label);
  }
}

/*!
 * \internal Get if GedaMenuButton is Empty
 * \par Function Description
 * Determines whether @menu is empty. A menu is considered empty if
 * the only visible children are tearoff menu items or "filler" menu
 * items which were inserted to mark the menu as empty.
 *
 * This function is used by #GtkAction.
 *
 * \param [in] menu a #GedaMenu or %NULL
 *
 * \returns whether @menu is empty.
 */
static bool geda_menu_is_empty (GtkWidget *menu)
{
  GList *children, *iter;
  bool result;

  if (!GTK_IS_CONTAINER(menu)) {
    return FALSE;
  }

  g_return_val_if_fail (GEDA_IS_MENU(menu), TRUE);

  children = geda_container_get_children (menu);

  result   = TRUE;

  for (iter = children; iter; iter = iter->next) {

    if (gtk_widget_get_visible (iter->data)) {

      if (!GTK_IS_TEAROFF_MENU_ITEM (iter->data)) {
        result = FALSE;
        break;
      }
    }
  }

  g_list_free (children);

  return result;
}

static void geda_menu_item_update (GtkActivatable *activatable,
                                   GtkAction      *action,
                                   const char     *property_name)
{
  GedaMenuItem *menu_item = GEDA_MENU_ITEM(activatable);
  GedaMenuItemPrivate *priv = menu_item->priv;

  if (strcmp (property_name, "visible") == 0) {

    bool is_empty;

    is_empty = geda_menu_is_empty (geda_menu_item_get_submenu_widget (menu_item));

    geda_action_sync_menu_visible ((GedaAction*)action, (GtkWidget*)menu_item, is_empty);

  }
  else if (strcmp (property_name, "sensitive") == 0) {

    bool sensitive;

    sensitive = gtk_action_is_sensitive (action);

    gtk_widget_set_sensitive ((GtkWidget*)menu_item, sensitive);

  }
  else if (priv->use_action_appearance) {

    if (strcmp (property_name, "label") == 0) {
      activatable_update_label (menu_item, action);
    }
  }
}

static void geda_menu_item_sync_action_properties (GtkActivatable *activatable,
                                                   GtkAction      *action)
{
  GedaMenuItem        *menu_item;
  GedaMenuItemPrivate *priv;
  GtkWidget           *widget;

  g_return_if_fail (GEDA_IS_MENU_ITEM (activatable));

  menu_item = (GedaMenuItem*)activatable;
  widget    = (GtkWidget*)activatable;

  priv      = menu_item->priv;

  if (!priv->use_action_appearance || !action) {

    GedaAccelLabel *label = geda_get_child_widget (widget);

    if (GEDA_IS_ACCEL_LABEL(label)) {
      geda_accel_label_set_accel_widget (label, widget);
    }
  }

  if (!action) {
    return;
  }

  geda_action_sync_menu_visible ((GedaAction*)action, widget,
    geda_menu_is_empty (geda_menu_item_get_submenu_widget (menu_item)));

  gtk_widget_set_sensitive (widget, gtk_action_is_sensitive (action));

  if (priv->use_action_appearance) {

    GtkWidget *label = geda_get_child_widget (widget);

    /* make sure label is a label, deleting it otherwise */
    if (label && !GEDA_IS_LABEL (label)) {

      gtk_container_remove ((GtkContainer*)menu_item, label);
      label = NULL;
    }

    /* Make sure that menu_item has a label and that the
     * accelerator is set */
    geda_menu_item_ensure_label (menu_item);
    geda_menu_item_set_use_underline (menu_item, TRUE);

    /* Make the label point to the menu_item's label */
    label = geda_get_child_widget (widget);

    if (GEDA_IS_ACCEL_LABEL(label)) {

      const char *accel_path;

      accel_path = gtk_action_get_accel_path (action);

      if (accel_path) {
        geda_accel_label_set_accel_widget ((GedaAccelLabel*)label, NULL);
        geda_accel_label_set_accel_closure ((GedaAccelLabel*)label,
        gtk_action_get_accel_closure (action));
      }
    }

    activatable_update_label (menu_item, action);
  }
}

/* ------------------------------------------------------- */

/*!
 * \brief Creates a new GedaMenuItem
 * \returns the newly created #GedaMenuItem
 */
GtkWidget *geda_menu_item_new (void)
{
  return g_object_new (GEDA_TYPE_MENU_ITEM, NULL);
}

/*!
 * \brief Creates a new GedaMenuItem with label
 * \par Function Description
 *  Creates a new #GedaMenuItem whose child is a #GedaLabel.
 *
 * \param [in] label the text for the label
 *
 * \returns the newly created #GedaMenuItem
 */
GtkWidget *geda_menu_item_new_with_label (const char *label)
{
  return g_object_new (GEDA_TYPE_MENU_ITEM, "label", label, NULL);
}

/*!
 * \brief Creates a new GedaMenuItem with mnemonic
 * \par Function Description
 *  Creates a new #GedaMenuItem containing a mnemonic label.
 *  The label will be created using geda_label_new_with_mnemonic(),
 *  so underscores in \a label indicate the mnemonic for the menu item.
 *
 * \param label The text of the button, with an underscore in front of the
 *              mnemonic character
 *
 * \returns a new #GedaMenuItem
 */
GtkWidget *geda_menu_item_new_with_mnemonic (const char *label)
{
  return g_object_new (GEDA_TYPE_MENU_ITEM,
                       "use-underline", TRUE,
                       "label", label,
                       NULL);
}

/*!
 * \brief Retrieve GedaMenuItem Event Window
 * \par Function Description
 *  Returns the internal event_window of the menu item.
 *
 * \param [in] menu_item a #GedaMenuItem
 */
GdkWindow *geda_menu_item_get_event_window (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), NULL);

  return menu_item->priv->event_window;
}

/*!
 * \brief Set GedaMenuItem submenu
 * \par Function Description
 *  Sets or replaces the submenu of menu item, or removes it when a %NULL
 *  submenu is passed. This function does not call geda_menu_attach_to_widget.
 *  geda_menu_attach_to_widget
 *
 * \param [in] menu_item a #GedaMenuItem
 * \param [in] submenu   A GedaMenu menu, or %NULL
 *
 * \sa geda_menu_item_set_submenu_widget
 */
void geda_menu_item_set_submenu (GedaMenuItem *menu_item, GedaMenu *submenu)
{
  GedaMenuItemPrivate *priv;

  GtkWidget *menu_widget = (GtkWidget*)submenu;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));
  g_return_if_fail (GEDA_IS_MENU(submenu));

  priv = menu_item->priv;

  if (priv->submenu != menu_widget) {

    GtkWidget *widget = (GtkWidget*)menu_item;

    if (priv->submenu) {
      geda_menu_detach ((GedaMenu*)priv->submenu);
    }

    priv->submenu = menu_widget;

    if (gtk_widget_get_parent (widget)) {
      gtk_widget_queue_resize (widget);
    }

    GEDA_OBJECT_NOTIFY (menu_item, "submenu");
  }
}

/*!
 * \brief Set or Clear GedaMenuItem submenu Widget
 * \par Function Description
 *  Sets or replaces the submenu of menu item, or removes it when
 *  \a submenu is %NULL.
 *
 * \param [in] menu_item a #GedaMenuItem
 * \param [in] submenu   the submenu, or %NULL
 *
 * \sa geda_menu_item_set_submenu
 */
void geda_menu_item_set_submenu_widget (GedaMenuItem *menu_item, GtkWidget *submenu)
{
  GedaMenuItemPrivate *priv;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));
  g_return_if_fail (GEDA_IS_MENU(submenu));

  priv = menu_item->priv;

  if (priv->submenu != submenu) {

    GtkWidget *widget = (GtkWidget*)menu_item;

    if (priv->submenu) {
      geda_menu_detach ((GedaMenu*)priv->submenu);
    }

    priv->submenu = submenu;

    if (submenu) {

      geda_menu_attach_to_widget ((GedaMenu*)submenu, widget,
                                  geda_menu_item_detacher);

      /* Connect handler to be called when submenu selection is performed */
      g_signal_connect_after (submenu, "selection-done",
                              G_CALLBACK (geda_menu_item_selection_done),
                              menu_item);
    }

    if (gtk_widget_get_parent (widget)) {
      gtk_widget_queue_resize (widget);
    }

    GEDA_OBJECT_NOTIFY (menu_item, "submenu");
  }
}

/*!
 * \brief geda_menu_item_get_submenu_widget
 * \par Function Description
 *  Gets the submenu underneath this menu item, if any.
 *  See geda_menu_item_set_submenu_widget().
 *
 * \param [in] menu_item Pointer to a #GedaMenuItem
 *
 * \returns submenu for this menu item, or %NULL if none
 */
GtkWidget *geda_menu_item_get_submenu_widget (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), NULL);

  return menu_item->priv->submenu;
}

/*!
 * \brief Get the Menu Item submenu direction
 * \par Function Description
 *  Gets the submenu direction property.
 *
 * \param [in] menu_item Pointer to a #GedaMenuItem
 *
 * \returns submenu_direction
 */
unsigned int geda_menu_item_get_submenu_direction (GedaMenuItem *menu_item)
{
  return menu_item->priv->submenu_direction;
}

/*!
 * \brief Get the Menu Item submenu placement
 * \par Function Description
 *  Gets the submenu placement property.
 *
 * \param [in] menu_item Pointer to a #GedaMenuItem
 *
 * \returns submenu_placement
 */
SubmenuPlacement geda_menu_item_get_submenu_placement (GedaMenuItem *menu_item)
{
  return menu_item->priv->submenu_placement;
}

/*!
 * \brief Set the Menu Item submenu placement
 * \par Function Description
 *  Sets the submenu placement property.
 *
 * \param [in] menu_item Pointer to a #GedaMenuItem
 * \param [in] placement The new SubmenuPlacement to use.
 */
void geda_menu_item_set_submenu_placement (GedaMenuItem    *menu_item,
                                           SubmenuPlacement placement)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  menu_item->priv->submenu_placement = placement;
}

/*!
 * \brief geda_menu_item_select
 * \par Function Description
 *  Emits the GedaMenuItem::select signal on the given item.
 *
 * \param [in] menu_item the menu item
 */
void geda_menu_item_select (GedaMenuItem *menu_item)
{
  GtkWidget *parent;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  g_signal_emit (menu_item, menu_item_signals[SELECT], 0);

  /* Enable themeing of the parent menu item depending on whether
   * something is selected in its submenu - test this? */

  parent = gtk_widget_get_parent((GtkWidget*)menu_item);

  if (GEDA_IS_MENU (parent)) {

    GtkWidget *child_item;

    child_item = geda_menu_get_parent_item((GedaMenu*)parent);

    if (child_item) {
      gtk_widget_queue_draw (child_item);
    }
  }
}

/*!
 * \brief geda_menu_item_deselect
 * \par Function Description
 *  Emits the GedaMenuItem::deselect signal on the given item.
 *
 * \param [in] menu_item the menu item
 */
void geda_menu_item_deselect (GedaMenuItem *menu_item)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  g_signal_emit (menu_item, menu_item_signals[DESELECT], 0);

  if (GEDA_IS_MENU (((GtkWidget*)menu_item)->parent)) {

    GedaMenu *menu = (GedaMenu*)((GtkWidget*)menu_item)->parent;

    if (menu->parent_menu_item) {
      gtk_widget_queue_draw ((GtkWidget*)menu->parent_menu_item);
    }
  }
}

/*!
 * \brief Activate a GedaMenu Item
 * \par Function Description
 *  Emits the GedaMenuItem::activate signal on the given item
 *
 * \param [in] menu_item the menu item
 */
void geda_menu_item_activate (GedaMenuItem *menu_item)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  g_signal_emit (menu_item, menu_item_signals[ACTIVATE], 0);
}

/*!
 * \brief Activate a GedaMenu Item
 * \par Function Description
 *  Emits the GedaMenuItem::activate-item signal on the given
 *  menu item.
 *
 * \param [in] menu_item the menu item
 */
void geda_menu_item_activate_item (GedaMenuItem *menu_item)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  g_signal_emit (menu_item, menu_item_signals[ACTIVATE_ITEM], 0);
}

/*!
 * \brief geda_menu_item_toggle_size_request
 * \par Function Description
 *  Emits the GedaMenuItem::toggle-size-request signal on the given item.
 *
 * \param [in] menu_item   the menu item
 * \param [in] requisition the requisition to use as signal data.
 */
void geda_menu_item_toggle_size_request (GedaMenuItem *menu_item,
                                                  int *requisition)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  g_signal_emit (menu_item, menu_item_signals[TOGGLE_SIZE_REQUEST], 0, requisition);
}

/*!
 * \brief geda_menu_item_toggle_size_allocate
 * \par Function Description
 *  Emits the GedaMenuItem::toggle-size-allocate signal on the given item.
 *
 * \param [in] menu_item        the menu item.
 * \param [in] allocation  the allocation to use as signal data.
 */
void geda_menu_item_toggle_size_allocate (GedaMenuItem *menu_item,
                                                   int  allocation)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  g_signal_emit (menu_item, menu_item_signals[TOGGLE_SIZE_ALLOCATE], 0, allocation);
}

/* Common Helper, called by:
 *
 *    1.) Gtk2 geda_menu_item_size_request
 *    2.) Gtk3 geda_menu_item_draw
 *    3.) Gtk3 geda_menu_item_get_preferred_width
 *    4.) Gtk3 geda_menu_item_real_get_height
 *    5.) Gtk3 geda_menu_item_size_allocate
 */
static void get_arrow_size (GtkWidget *widget, GtkWidget *child, int *size, int *spacing)
{
  PangoContext     *context;
  PangoFontMetrics *metrics;
  float             arrow_scaling;
  int               arrow_spacing;

  gtk_widget_style_get (widget,
                        "arrow-scaling", &arrow_scaling,
                        "arrow-spacing", &arrow_spacing,
                        NULL);

  if (spacing != NULL) {
    *spacing = arrow_spacing;
  }

  context = gtk_widget_get_pango_context (child);

  metrics = pango_context_get_metrics (context,
                                       pango_context_get_font_description (context),
                                       pango_context_get_language (context));

  *size = (PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
           pango_font_metrics_get_descent (metrics)));

  pango_font_metrics_unref (metrics);

  *size = *size * arrow_scaling;
}

/* Common Helper
 * Gtk2 geda_menu_item_size_request
 * Gtk3 geda_menu_item_get_preferred_width
 */
static int get_minimum_width (GtkWidget *widget)
{
  PangoContext     *context;
  PangoFontMetrics *metrics;
  int width;
  int width_chars;

  context = gtk_widget_get_pango_context (widget);

  metrics = pango_context_get_metrics (context,
                                       pango_context_get_font_description (context),
                                       pango_context_get_language (context));

  width = pango_font_metrics_get_approximate_char_width (metrics);

  pango_font_metrics_unref (metrics);

  gtk_widget_style_get (widget, "width-chars", &width_chars, NULL);

  return PANGO_PIXELS (width_chars * width);
}

#if GTK_MAJOR_VERSION < 3

static void geda_menu_item_paint (GtkWidget *widget, GdkRectangle *area)
{
  GedaMenuItem        *menu_item;
  GedaMenuItemPrivate *priv;

  GtkStateType  state_type;
  GtkShadowType selected_shadow_type;

  int width, height;
  int x, y;
  int border_width;

  menu_item = (GedaMenuItem*)widget;
  priv      = menu_item->priv;

  border_width = ((GtkContainer*)widget)->border_width;

  x      = widget->allocation.x + border_width;
  y      = widget->allocation.y + border_width;
  width  = widget->allocation.width  - border_width * 2;
  height = widget->allocation.height - border_width * 2;

  state_type = widget->state;

  if ((state_type == GTK_STATE_PRELIGHT) && (((GtkBin*)menu_item)->child))
  {
    gtk_widget_style_get (widget,
                          "selected-shadow-type", &selected_shadow_type,
                          NULL);
    gtk_paint_box (widget->style,
                   widget->window,
                   GTK_STATE_PRELIGHT,
                   selected_shadow_type,
                   area, widget, "menuitem",
                   x, y, width, height);
  }

  if (priv->submenu && priv->show_submenu_indicator) {

    PangoContext     *context;
    PangoFontMetrics *metrics;
    GtkArrowType      arrow_type;
    GtkShadowType     shadow_type;
    GtkTextDirection  direction;
    unsigned int      horizontal_padding;
    float             arrow_scaling;

    int arrow_extent;
    int arrow_size;
    int arrow_x, arrow_y;

    if (state_type == GTK_STATE_PRELIGHT) {
      shadow_type = GTK_SHADOW_IN;
    }
    else {
      shadow_type = GTK_SHADOW_OUT;
    }

    direction = gtk_widget_get_direction (widget);

    gtk_widget_style_get (widget,
                          "horizontal-padding", &horizontal_padding,
                          "arrow-scaling", &arrow_scaling,
                          NULL);

    context = gtk_widget_get_pango_context (((GtkBin*)menu_item)->child);
    metrics = pango_context_get_metrics (context,
                                         ((GtkWidget*)((GtkBin*)menu_item)->child)->style->font_desc,
                                         pango_context_get_language (context));

    arrow_size = (PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
                                pango_font_metrics_get_descent (metrics)));

    pango_font_metrics_unref (metrics);

    arrow_extent = arrow_size * arrow_scaling;

    if (direction == GTK_TEXT_DIR_LTR) {
      arrow_x    = x + width - horizontal_padding - arrow_extent;
      arrow_type = GTK_ARROW_RIGHT;
    }
    else {
      arrow_x    = x + horizontal_padding;
      arrow_type = GTK_ARROW_LEFT;
    }

    arrow_y = y + (height - arrow_extent) / 2;

    gtk_paint_arrow (widget->style, widget->window,
                     state_type, shadow_type,
                     area, widget, "menuitem",
                     arrow_type, TRUE,
                     arrow_x, arrow_y,
                     arrow_extent, arrow_extent);
  }
  else if (! ((GtkBin*)menu_item)->child ) {

    bool     wide_separators;
    int      separator_height;
    unsigned int horizontal_padding;

    gtk_widget_style_get (widget,
                          "wide-separators",    &wide_separators,
                          "separator-height",   &separator_height,
                          "horizontal-padding", &horizontal_padding,
                          NULL);

    if (wide_separators) {
      gtk_paint_box (widget->style, widget->window,
                     GTK_STATE_NORMAL, GTK_SHADOW_ETCHED_OUT,
                     area, widget, "hseparator",
                     widget->allocation.x + horizontal_padding + widget->style->xthickness,
                     widget->allocation.y + (widget->allocation.height -
                     separator_height -
                     widget->style->ythickness) / 2,
                     widget->allocation.width -
                     2 * (horizontal_padding + widget->style->xthickness),
                     separator_height);
    }
    else {
      gtk_paint_hline (widget->style, widget->window,
                       GTK_STATE_NORMAL, area, widget, "menuitem",
                       widget->allocation.x + horizontal_padding + widget->style->xthickness,
                       widget->allocation.x + widget->allocation.width - horizontal_padding - widget->style->xthickness - 1,
                       widget->allocation.y + (widget->allocation.height -
                       widget->style->ythickness) / 2);
    }
  }
}

static int geda_menu_item_expose (GtkWidget *widget, GdkEventExpose *event)
{
  if (gtk_widget_is_drawable (widget)) {

    geda_menu_item_paint (widget, &event->area);

    ((GtkWidgetClass*)geda_menu_item_parent_class)->expose_event (widget, event);
  }

  return FALSE;
}

/*! \internal Gtk2 widget_class->size_request */
static void geda_menu_item_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GedaMenuItem        *menu_item;
  GedaMenuItemPrivate *priv;
  GtkBin              *bin;
  PackDirection        pack_dir;
  PackDirection        child_pack_dir;
  unsigned int         accel_width;
  unsigned int         horizontal_padding;
  unsigned int         vertical_padding;

  gtk_widget_style_get (widget, "horizontal-padding", &horizontal_padding, NULL);

  bin       = (GtkBin*)widget;
  menu_item = (GedaMenuItem*)widget;
  priv      = menu_item->priv;

  /* If an item on a bar use the parent style */
  if (GEDA_IS_MENU_BAR(widget->parent)) {

    pack_dir         = geda_menu_bar_get_pack_direction ((GedaMenuBar*)widget->parent);
    child_pack_dir   = geda_menu_bar_get_child_pack_direction ((GedaMenuBar*)widget->parent);
    vertical_padding =  0;
  }
  else  {

    pack_dir         = PACK_DIRECTION_LTR;
    child_pack_dir   = PACK_DIRECTION_LTR;

    /* Only add vertical padding in menus, not items on menu bars */
    gtk_widget_style_get (widget, "vertical-padding", &vertical_padding, NULL);
  }

  requisition->width  = (((GtkContainer*)widget)->border_width +
                                         widget->style->xthickness) << 1;
  requisition->height = (((GtkContainer*)widget)->border_width +
                                         widget->style->ythickness) << 1;

  if ((pack_dir == PACK_DIRECTION_LTR || pack_dir == PACK_DIRECTION_RTL) &&
    (child_pack_dir == PACK_DIRECTION_LTR || child_pack_dir == PACK_DIRECTION_RTL))
  {
    requisition->width  += horizontal_padding << 1;
    requisition->height += vertical_padding;
  }
  else if ((pack_dir == PACK_DIRECTION_TTB || pack_dir == PACK_DIRECTION_BTT) &&
    (child_pack_dir == PACK_DIRECTION_TTB || child_pack_dir == PACK_DIRECTION_BTT))
  {
    /* Reverse role of padding */
    requisition->width  += vertical_padding;
    requisition->height += horizontal_padding << 1;
  }

  if (bin->child && gtk_widget_get_visible (bin->child)) {

    GtkRequisition child_requisition;

    gtk_widget_size_request (bin->child, &child_requisition);

    requisition->width  += child_requisition.width;
    requisition->height += child_requisition.height;

    if (priv->submenu && priv->show_submenu_indicator) {

      int arrow_spacing, arrow_size;

      get_arrow_size (widget, bin->child, &arrow_size, &arrow_spacing);

      requisition->width += child_requisition.height;
      requisition->width += arrow_spacing + arrow_size;

      requisition->width = MAX (requisition->width, get_minimum_width (widget));
    }
  }
  else { /* separator item */

    bool wide_separators;
    int  separator_height;

    gtk_widget_style_get (widget,
                          "wide-separators",  &wide_separators,
                          "separator-height", &separator_height,
                          NULL);

    if (wide_separators) {
      requisition->height += separator_height + widget->style->ythickness;
    }
    else {
      requisition->height += widget->style->ythickness << 1;
    }
  }

  accel_width = 0;

  geda_container_foreach (menu_item, geda_menu_item_accel_width_foreach, &accel_width);

  priv->accelerator_width = accel_width;
}

/*! \internal Gtk2 widget_class->size_allocate */
static void geda_menu_item_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GedaMenuItem        *menu_item;
  GedaMenuItemPrivate *priv;
  GtkBin              *bin;
  GtkAllocation        child_allocation;
  GtkTextDirection     direction;
  PackDirection        pack_dir;
  PackDirection        child_pack_dir;

  menu_item = (GedaMenuItem*)widget;
  bin       = (GtkBin*)widget;
  priv      = menu_item->priv;
  direction = gtk_widget_get_direction (widget);

  if (GEDA_IS_MENU_BAR(widget->parent)) {
    pack_dir       = geda_menu_bar_get_pack_direction ((GedaMenuBar*)widget->parent);
    child_pack_dir = geda_menu_bar_get_child_pack_direction ((GedaMenuBar*)widget->parent);
  }
  else {
    pack_dir       = PACK_DIRECTION_LTR;
    child_pack_dir = PACK_DIRECTION_LTR;
  }

  widget->allocation = *allocation;

  if (bin->child) {

    GtkRequisition child_requisition;
    unsigned int   horizontal_padding;

    gtk_widget_style_get (widget, "horizontal-padding", &horizontal_padding, NULL);

    child_allocation.x = ((GtkContainer*)widget)->border_width + widget->style->xthickness;
    child_allocation.y = ((GtkContainer*)widget)->border_width + widget->style->ythickness;

    if ((pack_dir       == PACK_DIRECTION_LTR || pack_dir       == PACK_DIRECTION_RTL) &&
        (child_pack_dir == PACK_DIRECTION_LTR || child_pack_dir == PACK_DIRECTION_RTL))
    {
      child_allocation.x += horizontal_padding;
    }
    else if ((pack_dir       == PACK_DIRECTION_TTB || pack_dir       == PACK_DIRECTION_BTT) &&
             (child_pack_dir == PACK_DIRECTION_TTB || child_pack_dir == PACK_DIRECTION_BTT))
    {
      child_allocation.y += horizontal_padding;
    }

    child_allocation.width  = MAX (1, (int)allocation->width - child_allocation.x * 2);
    child_allocation.height = MAX (1, (int)allocation->height - child_allocation.y * 2);

    if (child_pack_dir == PACK_DIRECTION_LTR || child_pack_dir == PACK_DIRECTION_RTL)
    {
      if ((direction == GTK_TEXT_DIR_LTR) == (child_pack_dir != PACK_DIRECTION_RTL))
      {
        child_allocation.x += menu_item->priv->toggle_size;
      }
      child_allocation.width -= menu_item->priv->toggle_size;
    }
    else
    {
      if ((direction == GTK_TEXT_DIR_LTR) == (child_pack_dir != PACK_DIRECTION_BTT))
      {
        child_allocation.y += menu_item->priv->toggle_size;
      }
      child_allocation.height -= menu_item->priv->toggle_size;
    }

    child_allocation.x += widget->allocation.x;
    child_allocation.y += widget->allocation.y;

    gtk_widget_get_child_requisition (bin->child, &child_requisition);

    if (priv->submenu && priv->show_submenu_indicator) {
      if (direction == GTK_TEXT_DIR_RTL) {
        child_allocation.x += child_requisition.height;
      }
      child_allocation.width -= child_requisition.height;
    }

    if (child_allocation.width < 1) {
      child_allocation.width = 1;
    }

    gtk_widget_size_allocate (bin->child, &child_allocation);
  }

  if (gtk_widget_get_realized (widget)) {
    gdk_window_move_resize (priv->event_window,
                            allocation->x, allocation->y,
                            allocation->width, allocation->height);
  }

  if (priv->submenu) {
    geda_menu_reposition ((GedaMenu*)priv->submenu);
  }
}

/*! \internal Gtk2 menu_item_class->select */
static void geda_real_menu_item_select (GedaMenuItem *menu_item)
{
  GedaMenuItemPrivate *priv;
  bool                 touchscreen_mode;

  priv = menu_item->priv;

  g_object_get (gtk_widget_get_settings ((GtkWidget*)menu_item),
                "gtk-touchscreen-mode", &touchscreen_mode,
                NULL);

  if (!touchscreen_mode && priv->submenu) {

    GtkWidget *submenu       = priv->submenu;
    bool       is_not_mapped = !gtk_widget_get_mapped (submenu);

    if (is_not_mapped) {
      geda_menu_item_popup_submenu (menu_item, TRUE);
    }
    else if (GEDA_IS_TEAROFF_MENU_ITEM(submenu)) {
      if (geda_tearoff_menu_is_active(submenu)) {
        geda_menu_item_popup_submenu (menu_item, TRUE);
      }
    }
  }

  gtk_widget_set_state ((GtkWidget*)menu_item, GTK_STATE_PRELIGHT);
  gtk_widget_queue_draw ((GtkWidget*)menu_item);
}

#else /* GTK_MAJOR_VERSION >= 3.0 */

/*! \internal Gtk3 widget_class->draw */
static bool geda_menu_item_draw (GtkWidget *widget, cairo_t *cr)
{
  GedaMenuItem        *menu_item = (GedaMenuItem*)widget;
  GedaMenuItemPrivate *priv      = menu_item->priv;
  GtkStateFlags        state;
  GtkStyleContext     *context;
  GtkBorder            padding;
  GtkWidget           *child, *parent;
  int x, y, w, h, width, height;

  unsigned int border_width = gtk_container_get_border_width ((GtkContainer*)widget);

  state   = gtk_widget_get_state_flags (widget);
  context = gtk_widget_get_style_context (widget);
  width   = gtk_widget_get_allocated_width (widget);
  height  = gtk_widget_get_allocated_height (widget);

  x       = border_width;
  y       = border_width;
  w       = width - border_width * 2;
  h       = height - border_width * 2;

  child   = geda_get_child_widget (menu_item);
  parent  = gtk_widget_get_parent (widget);

  gtk_style_context_get_padding (context, state, &padding);

  gtk_render_background (context, cr, x, y, w, h);
  gtk_render_frame (context, cr, x, y, w, h);

  if (priv->submenu && !GEDA_IS_MENU_BAR(parent)) {

    int arrow_x, arrow_y;
    int arrow_size;
    GtkTextDirection direction;
    double angle;

    direction = gtk_widget_get_direction (widget);
    get_arrow_size (widget, child, &arrow_size, NULL);

    if (direction == GTK_TEXT_DIR_LTR) {
      arrow_x = x + w - arrow_size - padding.right;
      angle = G_PI / 2;
    }
    else {
      arrow_x = x + padding.left;
      angle = (3 * G_PI) / 2;
    }

    arrow_y = y + (h - arrow_size) / 2;

    gtk_render_arrow (context, cr, angle, arrow_x, arrow_y, arrow_size);
  }
  else if (!child) {

    bool wide_separators;
    int     separator_height;

    gtk_style_context_save (context);
    gtk_style_context_add_class (context, GTK_STYLE_CLASS_SEPARATOR);

    gtk_widget_style_get (widget,
                          "wide-separators",  &wide_separators,
                          "separator-height", &separator_height,
                          NULL);
    if (wide_separators) {
      gtk_render_frame (context, cr,
                        x + padding.left,
                        y + padding.top,
                        w - padding.left - padding.right,
                        separator_height);
    }
    else {
      gtk_render_line (context, cr,
                       x + padding.left,
                       y + padding.top,
                       x + w - padding.right - 1,
                       y + padding.top);
    }

    gtk_style_context_restore (context);
  }

  ((GtkWidgetClass*)geda_menu_item_parent_class)->draw (widget, cr);

  return FALSE;
}

/*! \internal Gtk3 widget_class->get_preferred_width */
static void geda_menu_item_get_preferred_width (GtkWidget *widget,
                                                int       *minimum_size,
                                                int       *natural_size)
{
  GedaMenuItem        *menu_item = (GedaMenuItem*)widget;
  GedaMenuItemPrivate *priv      = menu_item->priv;
  GtkWidget *child;
  GtkWidget *parent;
  unsigned int accel_width;
  unsigned int border_width;
  int  min_width, nat_width;
  GtkStyleContext *context;
  GtkStateFlags state;
  GtkBorder padding;

  parent       = gtk_widget_get_parent (widget);

  border_width = geda_get_container_border_width (widget);

  context      = gtk_widget_get_style_context (widget);
  state        = gtk_widget_get_state_flags (widget);

  gtk_style_context_get_padding (context, state, &padding);

  min_width  = (border_width << 1) + padding.left + padding.right;
  nat_width  = min_width;

  child      = geda_get_child_widget (widget);

  if (child != NULL && gtk_widget_get_visible (child)) {

    GedaMenuItemPrivate *priv = menu_item->priv;
    int child_min, child_nat;

    gtk_widget_get_preferred_width (child, &child_min, &child_nat);

    if ((menu_item->priv->submenu && !GEDA_IS_MENU_BAR(parent)) || priv->reserve_indicator)
    {
      int arrow_spacing, arrow_size;

      get_arrow_size (widget, child, &arrow_size, &arrow_spacing);

      min_width += arrow_size;
      min_width += arrow_spacing;

      min_width = MAX (min_width, get_minimum_width (widget));
      nat_width = min_width;
    }

    min_width += child_min;
    nat_width += child_nat;
  }

  accel_width = 0;
  geda_container_foreach (menu_item,
                          geda_menu_item_accel_width_foreach,
                         &accel_width);
  priv->accelerator_width = accel_width;

  if (minimum_size) {
    *minimum_size = min_width;
  }

  if (natural_size) {
    *natural_size = nat_width;
  }
}

static void geda_menu_item_real_get_height (GtkWidget *widget,
                                            int        for_size,
                                            int       *minimum_size,
                                            int       *natural_size)
{
  GedaMenuItem        *menu_item = (GedaMenuItem*)widget;
  GedaMenuItemPrivate *priv      = menu_item->priv;

  GtkStyleContext *context;
  GtkWidget       *child;
  GtkWidget       *parent;
  GtkBorder        padding;
  GtkStateFlags    state;

  unsigned int accel_width;
  unsigned int border_widthx2;
  int min_height, nat_height;
  int avail_size = 0;

  context = gtk_widget_get_style_context (widget);
  state   = gtk_widget_get_state_flags (widget);
  gtk_style_context_get_padding (context, state, &padding);

  parent = gtk_widget_get_parent (widget);

  border_widthx2 = gtk_container_get_border_width ((GtkContainer*)widget) << 1;
  min_height     = border_widthx2 + padding.top + padding.bottom;

  if (for_size != -1) {
    avail_size = for_size;
    avail_size -= border_widthx2 + padding.left + padding.right;
  }

  nat_height = min_height;

  child = geda_get_child_widget (widget);

  if (child != NULL && gtk_widget_get_visible (child)) {

    int child_min, child_nat;
    int arrow_size = 0;
    int arrow_spacing = 0;

    if ((priv->submenu && !GEDA_IS_MENU_BAR(parent)) || priv->reserve_indicator)
      get_arrow_size (widget, child, &arrow_size, &arrow_spacing);

    if (for_size != -1) {

      avail_size -= (arrow_size + arrow_spacing);
      gtk_widget_get_preferred_height_for_width (child,
                                                 avail_size,
                                                 &child_min,
                                                 &child_nat);
    }
    else {

      gtk_widget_get_preferred_height (child, &child_min, &child_nat);
    }

    min_height += child_min;
    nat_height += child_nat;

    min_height = MAX (min_height, arrow_size);
    nat_height = MAX (nat_height, arrow_size);
  }
  else { /* separator item */

    bool wide_separators;
    int     separator_height;

    gtk_style_context_save (context);
    gtk_style_context_add_class (context, GTK_STYLE_CLASS_SEPARATOR);

    gtk_widget_style_get (widget,
                          "wide-separators",  &wide_separators,
                          "separator-height", &separator_height,
                          NULL);

    if (wide_separators) {

      min_height += separator_height;
      nat_height += separator_height;
    }
    else {

      /* force odd, so that we can have the same space above and
       * below the line.
       */
      if (min_height % 2 == 0)
        min_height += 1;
      if (nat_height % 2 == 0)
        nat_height += 1;
    }

    gtk_style_context_restore (context);
  }

  accel_width = 0;

  geda_container_foreach (menu_item,
                          geda_menu_item_accel_width_foreach,
                         &accel_width);

  priv->accelerator_width = accel_width;

  if (minimum_size)
    *minimum_size = min_height;

  if (natural_size)
    *natural_size = nat_height;
}

/*! \internal Gtk3 widget_class->get_preferred_height */
static void geda_menu_item_get_preferred_height (GtkWidget *widget,
                                                 int       *minimum_size,
                                                 int       *natural_size)
{
  geda_menu_item_real_get_height (widget, -1, minimum_size, natural_size);
}

/*! \internal Gtk3 widget_class->get_preferred_height_for_width */
static void geda_menu_item_get_preferred_height_for_width (GtkWidget *widget,
                                                           int        for_size,
                                                           int       *minimum_size,
                                                           int       *natural_size)
{
  geda_menu_item_real_get_height (widget, for_size, minimum_size, natural_size);
}

/*! \internal Gtk3 widget_class->size_allocate */
static void geda_menu_item_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GedaMenuItem        *menu_item = (GedaMenuItem*)widget;
  GedaMenuItemPrivate *priv      = menu_item->priv;
  GtkWidget           *child;
  GtkWidget           *parent;
  GtkAllocation        child_allocation;
  GtkTextDirection     direction;
  PackDirection        child_pack_dir;

  direction = gtk_widget_get_direction (widget);

  parent    = gtk_widget_get_parent (widget);

  if (GEDA_IS_MENU_BAR(parent)) {

    child_pack_dir = geda_menu_bar_get_child_pack_direction ((GedaMenuBar*)parent);
  }
  else {

    child_pack_dir = PACK_DIRECTION_LTR;
  }

  gtk_widget_set_allocation (widget, allocation);

  child = geda_get_child_widget (widget);

  if (child) {

    GtkStyleContext *context;
    GtkStateFlags    state;
    GtkBorder        padding;
    unsigned int     border_width;

    context = gtk_widget_get_style_context (widget);
    state   = gtk_widget_get_state_flags (widget);
    gtk_style_context_get_padding (context, state, &padding);

    border_width       = gtk_container_get_border_width ((GtkContainer*)widget);
    child_allocation.x = border_width + padding.left;
    child_allocation.y = border_width + padding.top;

    child_allocation.width  = allocation->width - (border_width * 2) -
                              padding.left - padding.right;
    child_allocation.height = allocation->height - (border_width * 2) -
                              padding.top - padding.bottom;

    if (child_pack_dir == PACK_DIRECTION_LTR ||
        child_pack_dir == PACK_DIRECTION_RTL)
    {
      if ((direction == GTK_TEXT_DIR_LTR) == (child_pack_dir != PACK_DIRECTION_RTL))
      {
        child_allocation.x += priv->toggle_size;
      }
      child_allocation.width -= priv->toggle_size;
    }
    else
    {
      if ((direction == GTK_TEXT_DIR_LTR) == (child_pack_dir != PACK_DIRECTION_BTT))
      {
        child_allocation.y += priv->toggle_size;
      }
      child_allocation.height -= priv->toggle_size;
    }

    child_allocation.x += allocation->x;
    child_allocation.y += allocation->y;

    if ((priv->submenu && !GEDA_IS_MENU_BAR(parent)) || priv->reserve_indicator)
    {
      int arrow_spacing, arrow_size;

      get_arrow_size (widget, child, &arrow_size, &arrow_spacing);

      if (direction == GTK_TEXT_DIR_RTL)
        child_allocation.x += arrow_size + arrow_spacing;
      child_allocation.width -= arrow_size + arrow_spacing;
    }

    if (child_allocation.width < 1)
      child_allocation.width = 1;

    gtk_widget_size_allocate (child, &child_allocation);
  }

  if (gtk_widget_get_realized (widget)) {
    gdk_window_move_resize (priv->event_window,
                            allocation->x, allocation->y,
                            allocation->width, allocation->height);
  }

  if (priv->submenu) {
    geda_menu_reposition (GEDA_MENU(priv->submenu));
  }
}

/*! \internal Gtk3 menu_item_class->select */
static void geda_real_menu_item_select (GedaMenuItem *menu_item)
{
  GedaMenuItemPrivate *priv          = menu_item->priv;
  GdkDevice           *source_device = NULL;
  GdkEvent            *current_event;

  current_event = gtk_get_current_event ();

  if (current_event) {
    source_device = gdk_event_get_source_device (current_event);
    gdk_event_free (current_event);
  }

  if ((!source_device ||
        gdk_device_get_source (source_device) != GDK_SOURCE_TOUCHSCREEN) &&
        priv->submenu &&
      (!gtk_widget_get_mapped (priv->submenu) ||
        GEDA_MENU(priv->submenu)->priv->tearoff_active))
  {
    geda_menu_item_popup_submenu (menu_item, TRUE);
  }

  gtk_widget_set_state_flags ((GtkWidget*)menu_item, GTK_STATE_FLAG_PRELIGHT, FALSE);
  gtk_widget_queue_draw ((GtkWidget*)menu_item);
}

#endif /* GTK_MAJOR_VERSION < 3 */

/* Common GedaMenuItemClass signal handlers */

static void geda_menu_item_activate_action (GedaMenuItem *menu_item)
{
  GedaMenuItemPrivate *priv = menu_item->priv;

#if GTK_MAJOR_VERSION == 3
  if (priv->action_helper) {
    gtk_action_helper_activate (GTK_ACTION_HELPER(priv->action_helper));
  }
#endif

  if (priv->action) {
    geda_action_activate (priv->action);
  }
}

/* menu_item_class->activate_item */
static void geda_real_menu_item_activate_item (GedaMenuItem *menu_item)
{
  GedaMenuItemPrivate *priv;
  GtkWidget *parent;
  GtkWidget *widget;

  widget = (GtkWidget*)menu_item;
  parent = gtk_widget_get_parent (widget);
  priv   = menu_item->priv;

  if (parent && GEDA_IS_MENU_SHELL(parent)) {

    GedaMenuShell *parent_menu_shell = (GedaMenuShell*)parent;

    if (priv->submenu == NULL) {
      geda_menu_shell_activate_item (parent_menu_shell, widget, TRUE);
    }
    else {

      geda_menu_shell_activate (parent_menu_shell);
      geda_menu_shell_select_item (parent_menu_shell, widget);
      geda_menu_item_popup_submenu (menu_item, FALSE);
      geda_menu_shell_select_first ((GedaMenuShell*)priv->submenu, TRUE);
    }
  }
}

static void geda_real_menu_item_deselect (GedaMenuItem *menu_item)
{
  GedaMenuItemPrivate *priv = menu_item->priv;
  GtkWidget           *widget;

  if (priv->submenu) {
    geda_menu_item_popdown_submenu (menu_item);
  }

  widget = (GtkWidget*)menu_item;

#if GTK_MAJOR_VERSION < 3
  gtk_widget_set_state (widget, GTK_STATE_NORMAL);
#else
  gtk_widget_unset_state_flags (widget, GTK_STATE_FLAG_PRELIGHT);
#endif

  gtk_widget_queue_draw (widget);

  widget = gtk_widget_get_parent(widget);

  if (GEDA_IS_MENU (widget)) {

    GedaMenu *menu = (GedaMenu*)widget;

    if (menu->parent_menu_item) {
      gtk_widget_queue_draw ((GtkWidget*)menu->parent_menu_item);
    }
  }
}

/*! \internal menu_item_class->toggle_size_request */
static void geda_real_menu_item_toggle_size_request (GedaMenuItem *menu_item,
                                                     int          *requisition)
{
  *requisition = 0;
}

/*! \internal menu_item_class->toggle_size_allocate */
static void geda_real_menu_item_toggle_size_allocate (GedaMenuItem *menu_item,
                                                      int           allocation)
{
  menu_item->priv->toggle_size = allocation;
}

/*! \internal menu_item_class->set_label */
static void geda_real_menu_item_set_label (GedaMenuItem *menu_item, const char *label)
{
  GedaLabel *child;

  geda_menu_item_ensure_label (menu_item);

  child = geda_get_child_widget (menu_item);

  if (GEDA_IS_LABEL (child)) {

    geda_label_set_label (child, label ? label : "");

    menu_item->priv->mnemonic = geda_label_get_mnemonic_char(child);

    GEDA_OBJECT_NOTIFY (menu_item, "label");
  }
}

static const char *geda_real_menu_item_get_label (GedaMenuItem *menu_item)
{
  if (!GEDA_IS_MENU_SEPERATOR (menu_item)) {

    GedaLabel *child;

    geda_menu_item_ensure_label (menu_item);

    child = geda_get_child_widget (menu_item);

    if (GEDA_IS_LABEL (child)) {
      return geda_label_get_label (child);
    }
  }

  return NULL;
}

static void free_timeval (GTimeVal *val)
{
  g_free (val);
}

static void geda_menu_item_real_popup_submenu (GtkWidget *widget, bool remember_time)
{
  GedaMenuItem        *menu_item = (GedaMenuItem*)widget;
  GedaMenuItemPrivate *priv      = menu_item->priv;
  GtkWidget           *parent;

  parent = gtk_widget_get_parent (widget);

  if (gtk_widget_is_sensitive (priv->submenu) && parent) {

    bool take_focus;
    MenuPositionFunc menu_position_func;

    take_focus = geda_menu_shell_get_take_focus ((GedaMenuShell*)parent);
    geda_menu_shell_set_take_focus ((GedaMenuShell*)priv->submenu, take_focus);

    if (remember_time) {

      GTimeVal *popup_time = g_malloc0 (sizeof(GTimeVal));

      g_get_current_time (popup_time);

      g_object_set_data_full ((GObject*)priv->submenu,
                              MENU_POPUP_TIME_KEY, popup_time,
                              (GDestroyNotify) free_timeval);
    }
    else {

      GEDA_OBJECT_SET_DATA (priv->submenu, NULL, MENU_POPUP_TIME_KEY);
    }

    /* geda_menu_item_position_menu positions the submenu from the
     * menuitems position. If the menuitem does not have a window,
     * that does not work. In that case we use the default
     * positioning function instead which places the submenu at the
     * mouse cursor.
     */
    if (gtk_widget_get_window (widget)) {
      menu_position_func = geda_menu_item_position_menu;
    }
    else {
      menu_position_func = NULL;
    }

    geda_menu_popup (GEDA_MENU(priv->submenu),
                     parent,
                     widget,
                     menu_position_func,
                     menu_item,
                     GEDA_MENU_SHELL(parent)->button,
                     0);
  }

  /* Enable themeing of the parent menu item depending on whether
   * its submenu is shown or not.
   */
  gtk_widget_queue_draw (widget);
}

static int geda_menu_item_popup_timeout (void *data)
{
  GedaMenuItem        *menu_item = GEDA_MENU_ITEM(data);
  GedaMenuItemPrivate *priv      = menu_item->priv;
  GtkWidget           *parent;

  parent = gtk_widget_get_parent ((GtkWidget*)menu_item);

  if ((GEDA_IS_MENU_SHELL (parent) && GEDA_MENU_SHELL(parent)->active) ||
      (GEDA_IS_MENU(parent) && GEDA_MENU(parent)->torn_off))
  {
    geda_menu_item_real_popup_submenu ((GtkWidget*)menu_item, TRUE);
    if (priv->timer_from_keypress && priv->submenu)
      GEDA_MENU_SHELL(priv->submenu)->ignore_enter = TRUE;
  }

  priv->timer = 0;

  return FALSE;
}

static int get_popup_delay (GedaMenuItem *menu_item)
{
  GtkWidget *parent;

  parent = gtk_widget_get_parent ((GtkWidget*)menu_item);

  if (GEDA_IS_MENU_SHELL (parent)) {
    return geda_menu_shell_get_popup_delay ((GedaMenuShell*)parent);
  }

  return MENU_POPUP_DELAY;
}

/*!
 * \brief GedaMenuItem Open Submenu
 * \par Function Description
 *  Called by the parent shell to popup the menu of a menu item.
 *
 * \sa geda_menu_item_real_popup_submenu
 */
void geda_menu_item_popup_submenu (GedaMenuItem *menu_item, bool with_delay)
{
  GedaMenuItemPrivate *priv;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  priv = menu_item->priv;

  if (priv->timer) {
    g_source_remove (priv->timer);
    priv->timer = 0;
    with_delay = FALSE;
  }

  if (with_delay) {

    int popup_delay = get_popup_delay (menu_item);

    if (popup_delay > 0) {

      GdkEvent *event = gtk_get_current_event ();

      priv->timer = gdk_threads_add_timeout (popup_delay,
                                             geda_menu_item_popup_timeout,
                                             menu_item);

      if (event &&
          event->type != GDK_BUTTON_PRESS &&
          event->type != GDK_ENTER_NOTIFY)
      {
        priv->timer_from_keypress = TRUE;
      }
      else {
        priv->timer_from_keypress = FALSE;
      }

      if (event) {
        gdk_event_free (event);
      }

      return;
    }
  }

  geda_menu_item_real_popup_submenu ((GtkWidget*)menu_item, FALSE);
}

/*!
 * \brief Pop Down GedaMenuItem Submenu
 * \par Function Description
 *  Closes the submenu if present and removes the timer.
 *
 * \sa geda_real_menu_item_deselect geda_menu_shell_button_release
 */
void geda_menu_item_popdown_submenu (GedaMenuItem *menu_item)
{
  GedaMenuItemPrivate *priv = menu_item->priv;

  if (priv->submenu) {

    GEDA_OBJECT_SET_DATA (priv->submenu, NULL, MENU_POPUP_TIME_KEY);

    if (priv->timer) {

      g_source_remove (priv->timer);
      priv->timer = 0;
    }
    else {
      geda_menu_popdown ((GedaMenu*)priv->submenu);
    }

    gtk_widget_queue_draw ((GtkWidget*)menu_item);
  }
}

#if GTK_MAJOR_VERSION < 3

static void get_offsets (GedaMenu *menu, int *horizontal_offset, int *vertical_offset)
{
  int vertical_padding;
  int horizontal_padding;

  gtk_widget_style_get ((GtkWidget*)menu,
                        "horizontal-offset",   horizontal_offset,
                        "vertical-offset",     vertical_offset,
                        "horizontal-padding", &horizontal_padding,
                        "vertical-padding",   &vertical_padding,
                        NULL);

  *vertical_offset   -= ((GtkWidget*)menu)->style->ythickness;
  *vertical_offset   -= vertical_padding;
  *horizontal_offset += horizontal_padding;
}

static void geda_menu_item_position_menu (GedaMenu  *menu,
                                          int       *x,
                                          int       *y,
                                          bool      *push_in,
                                          void      *user_data)
{
  GedaMenuItem        *menu_item;
  GedaMenuItemPrivate *priv;
  GtkWidget           *widget;
  GedaMenu            *parent_menu;
  GedaMenuItem        *parent_menu_item;
  GdkScreen           *screen;
  GtkTextDirection     direction;
  GdkRectangle         monitor;

  int twidth, theight;
  int tx, ty;
  int monitor_num;
  int horizontal_offset;
  int vertical_offset;
  int parent_xthickness;
  int available_left, available_right;

  g_return_if_fail (menu != NULL);
  g_return_if_fail (x != NULL);
  g_return_if_fail (y != NULL);

  menu_item = (GedaMenuItem*)user_data;
  priv      = menu_item->priv;
  widget    = (GtkWidget*)user_data;

  if (push_in) {
    *push_in = FALSE;
  }

  direction   = gtk_widget_get_direction (widget);

  twidth      = ((GtkWidget*)menu)->requisition.width;
  theight     = ((GtkWidget*)menu)->requisition.height;

  screen      = gtk_widget_get_screen ((GtkWidget*)menu);
  monitor_num = gdk_screen_get_monitor_at_window (screen, priv->event_window);

  if (monitor_num < 0) {
    monitor_num = 0;
  }

  gdk_screen_get_monitor_geometry (screen, monitor_num, &monitor);

  if (!gdk_window_get_origin (widget->window, &tx, &ty)) {
    g_warning ("Menu not on screen");
    return;
  }

  tx += widget->allocation.x;
  ty += widget->allocation.y;

  get_offsets (menu, &horizontal_offset, &vertical_offset);

  available_left  = tx - monitor.x;
  available_right = monitor.x + monitor.width - (tx + widget->allocation.width);

  if (GEDA_IS_MENU_BAR(widget->parent)) {
      priv->from_menubar = TRUE;
  }
  else if (GEDA_IS_MENU(widget->parent)) {

    parent_menu = (GedaMenu*)widget->parent;

    if (parent_menu->parent_menu_item) {
      priv->from_menubar = GEDA_MENU_ITEM(parent_menu->parent_menu_item)->priv->from_menubar;
    }
    else {
      priv->from_menubar = FALSE;
    }
  }
  else {
      priv->from_menubar = FALSE;
  }

  switch (priv->submenu_placement) {

    case MENU_TOP_BOTTOM:

      if (direction == GTK_TEXT_DIR_LTR)  {
        priv->submenu_direction = GTK_DIRECTION_RIGHT;
      }
      else {
        priv->submenu_direction = GTK_DIRECTION_LEFT;
        tx += widget->allocation.width - twidth;
      }

      if ((ty + widget->allocation.height + theight) <= monitor.y + monitor.height)
        ty += widget->allocation.height;
      else if ((ty - theight) >= monitor.y)
        ty -= theight;
      else if (monitor.y + monitor.height - (ty + widget->allocation.height) > ty)
        ty += widget->allocation.height;
      else
        ty -= theight;
      break;

    case MENU_LEFT_RIGHT:

      if (GEDA_IS_MENU(widget->parent)) {

        GtkWidget  *menu_parent;

        parent_menu = (GedaMenu*)widget->parent;
        menu_parent = geda_menu_get_parent_item (parent_menu);

        if (GEDA_IS_MENU_ITEM(menu_parent)) {
          parent_menu_item = (GedaMenuItem*)menu_parent;
        }
        else {
          parent_menu_item = NULL;
        }
      }
      else {
        parent_menu_item = NULL;
      }

      parent_xthickness = widget->parent->style->xthickness;

      if (parent_menu_item && !geda_tearoff_menu_is_torn(widget->parent)) {
        priv->submenu_direction = parent_menu_item->priv->submenu_direction;
      }
      else {
        if (direction == GTK_TEXT_DIR_LTR)
          priv->submenu_direction = GTK_DIRECTION_RIGHT;
        else
          priv->submenu_direction = GTK_DIRECTION_LEFT;
      }

      switch (priv->submenu_direction) {

        case GTK_DIRECTION_LEFT:
          if (tx - twidth - parent_xthickness - horizontal_offset >= monitor.x ||
              available_left >= available_right)
          {
            tx -= twidth + parent_xthickness + horizontal_offset;
          }
          else
          {
            priv->submenu_direction = GTK_DIRECTION_RIGHT;
            tx += widget->allocation.width + parent_xthickness + horizontal_offset;
          }
          break;

        case GTK_DIRECTION_RIGHT:
          if (tx + widget->allocation.width + parent_xthickness + horizontal_offset +
              twidth <= monitor.x + monitor.width || available_right >= available_left)
          {
            tx += widget->allocation.width + parent_xthickness + horizontal_offset;
          }
          else
          {
            priv->submenu_direction = GTK_DIRECTION_LEFT;
            tx -= twidth + parent_xthickness + horizontal_offset;
          }
          break;
      }

      ty += vertical_offset;

      /* If the height of the menu doesn't fit we move it upward. */
      ty = CLAMP (ty, monitor.y, MAX (monitor.y, monitor.y + monitor.height - theight));
      break;
  }

  /* If we have negative, tx, here it is because we can't get
   * the menu all the way on screen. Favor the left portion.
   */
  *x = CLAMP (tx, monitor.x, MAX (monitor.x, monitor.x + monitor.width - twidth));
  *y = ty;

  geda_menu_set_monitor (menu, monitor_num);

  GtkWidget *toplevel = geda_menu_get_toplevel(menu);

  if (toplevel) {
    if (!gtk_widget_get_visible (toplevel)) {
      gtk_window_set_type_hint ((GtkWindow*)toplevel, priv->from_menubar?
      GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU : GDK_WINDOW_TYPE_HINT_POPUP_MENU);
    }
  }
}

#else /* GTK_MAJOR_VERSION at least 3 */

static void get_offsets (GedaMenu *menu, int *horizontal_offset, int *vertical_offset)
{
  GtkStyleContext *context;
  GtkStateFlags    state;
  GtkBorder        padding;

  gtk_widget_style_get ((GtkWidget*)menu,
                        "horizontal-offset", horizontal_offset,
                        "vertical-offset", vertical_offset,
                        NULL);

  context = gtk_widget_get_style_context ((GtkWidget*)menu);
  state   = gtk_widget_get_state_flags ((GtkWidget*)menu);
  gtk_style_context_get_padding (context, state, &padding);

  *vertical_offset   -= padding.top;
  *horizontal_offset += padding.left;
}

static void geda_menu_item_position_menu (GedaMenu  *menu,
                                          int       *x,
                                          int       *y,
                                          bool      *push_in,
                                          void      *user_data)
{
  GedaMenuItem        *menu_item = (GedaMenuItem*)user_data;
  GedaMenuItemPrivate *priv = menu_item->priv;
  GtkAllocation        allocation;
  GtkWidget           *widget;
  GedaMenuItem        *parent_menu_item;
  GtkWidget           *parent;
  GdkScreen           *screen;
  GtkTextDirection    direction;
  GdkRectangle        monitor;

  int monitor_num;
  int horizontal_offset;
  int vertical_offset;
  int available_left, available_right;
  int twidth, theight;
  int tx, ty;

  GtkStyleContext *context;
  GtkStateFlags    state;
  GtkBorder        parent_padding;

  g_return_if_fail (menu != NULL);
  g_return_if_fail (x != NULL);
  g_return_if_fail (y != NULL);

  widget = (GtkWidget*)user_data;

  if (push_in) {
    *push_in = FALSE;
  }

  direction   = gtk_widget_get_direction (widget);

  twidth      = gtk_widget_get_allocated_width ((GtkWidget*)menu);
  theight     = gtk_widget_get_allocated_height ((GtkWidget*)menu);

  screen      = gtk_widget_get_screen ((GtkWidget*)menu);
  monitor_num = gdk_screen_get_monitor_at_window (screen, priv->event_window);

  if (monitor_num < 0) {
    monitor_num = 0;
  }

  gdk_screen_get_monitor_workarea (screen, monitor_num, &monitor);

  if (!gdk_window_get_origin (gtk_widget_get_window (widget), &tx, &ty)) {
    g_warning ("Menu not on screen");
    return;
  }

  gtk_widget_get_allocation (widget, &allocation);

  tx += allocation.x;
  ty += allocation.y;

  get_offsets (menu, &horizontal_offset, &vertical_offset);

  available_left  = tx - monitor.x;
  available_right = monitor.x + monitor.width - (tx + allocation.width);

  parent = gtk_widget_get_parent (widget);
  priv->from_menubar = GEDA_IS_MENU_BAR(parent);

  switch (priv->submenu_placement) {

    case MENU_TOP_BOTTOM:

      if (direction == GTK_TEXT_DIR_LTR) {
        priv->submenu_direction = GTK_DIRECTION_RIGHT;
      }
      else {
        priv->submenu_direction = GTK_DIRECTION_LEFT;
        tx += allocation.width - twidth;
      }

      if ((ty + allocation.height + theight) <= monitor.y + monitor.height)
        ty += allocation.height;
      else if ((ty - theight) >= monitor.y)
        ty -= theight;
      else if (monitor.y + monitor.height - (ty + allocation.height) > ty)
        ty += allocation.height;
      else
        ty -= theight;
      break;

    case MENU_LEFT_RIGHT:

      if (GEDA_IS_MENU(parent)) {
        parent_menu_item = GEDA_MENU_ITEM(((GedaMenu*)parent)->priv->parent_menu_item);
      }
      else {
        parent_menu_item = NULL;
      }

      context = gtk_widget_get_style_context (parent);
      state   = gtk_widget_get_state_flags (parent);

      gtk_style_context_get_padding (context, state, &parent_padding);

      if (parent_menu_item && !((GedaMenu*)parent)->torn_off) {

        priv->submenu_direction = parent_menu_item->priv->submenu_direction;
      }
      else {

        if (direction == GTK_TEXT_DIR_LTR) {
          priv->submenu_direction = GTK_DIRECTION_RIGHT;
        }
        else {
          priv->submenu_direction = GTK_DIRECTION_LEFT;
        }
      }

      switch (priv->submenu_direction) {

        case GTK_DIRECTION_LEFT:
          if (tx - twidth - parent_padding.left - horizontal_offset >= monitor.x ||
              available_left >= available_right)
          {
            tx -= twidth + parent_padding.left + horizontal_offset;
          }
          else
          {
            priv->submenu_direction = GTK_DIRECTION_RIGHT;
            tx += allocation.width + parent_padding.right + horizontal_offset;
          }
          break;

        case GTK_DIRECTION_RIGHT:
          if (tx + allocation.width + parent_padding.right + horizontal_offset + twidth <= monitor.x + monitor.width ||
            available_right >= available_left)
            tx += allocation.width + parent_padding.right + horizontal_offset;
          else
          {
            priv->submenu_direction = GTK_DIRECTION_LEFT;
            tx -= twidth + parent_padding.left + horizontal_offset;
          }
          break;
      }

      ty += vertical_offset;

      /* If the height of the menu doesn't fit we move it upward. */
      ty = CLAMP (ty, monitor.y, MAX (monitor.y, monitor.y + monitor.height - theight));
      break;
  }

  /* If we have negative, tx, here it is because we can't get
   * the menu all the way on screen. Favor the left portion.
   */
  *x = CLAMP (tx, monitor.x, MAX (monitor.x, monitor.x + monitor.width - twidth));
  *y = ty;

  geda_menu_set_monitor (menu, monitor_num);

  if (!gtk_widget_get_visible (menu->priv->toplevel)) {

    gtk_window_set_type_hint ((GtkWindow*)menu->priv->toplevel, priv->from_menubar?
    GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU : GDK_WINDOW_TYPE_HINT_POPUP_MENU);
  }
}

#endif /* else !GTK_MAJOR_VERSION < 3*/

/*!
 * \brief Retrieve the GedaMenuItem mnemonic char
 * \par Function Description
 *  Returns the mnemonic char of the menu item.
 */
char geda_menu_item_get_mnemonic (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), '0');

  return menu_item->priv->mnemonic;
}


/*!
 * \brief  Set the GedaMenuItem mnemonic char
 * \par Function Description
 *  Sets the mnemonic char of the menu item. The char is not checked
 *  and must a validate mnemonic char.
 */
void geda_menu_item_set_mnemonic (GedaMenuItem *menu_item, char mnemonic)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  menu_item->priv->mnemonic = mnemonic;
}

/*!
 * \brief geda_menu_item_set_right_justified
 * \par Function Description
 * Sets whether the menu item appears justified at the right
 * side of a menu bar. This was traditionally done for "Help"
 * menu items, but is now considered a bad idea. (If the widget
 * layout is reversed for a right-to-left language like Hebrew
 * or Arabic, right-justified-menu-items appear at the left.)
 *
 * It is recommended to use with gtk_widget_set_hexpand() and
 * gtk_widget_set_halign().
 *
 * \param [in] menu_item       a GedaMenuItem
 * \param [in] right_justified if %TRUE the menu item will appear at
 *                             the far right if added to a menu bar
 */
void geda_menu_item_set_right_justified (GedaMenuItem *menu_item,
                                         bool          right_justified)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  geda_menu_item_do_set_right_justified (menu_item, right_justified);
}

/*!
 * \brief geda_menu_item_get_right_justified
 * \par Function Description
 * Gets whether the menu item appears justified at the right
 * side of the menu bar.
 *
 * \param [in] menu_item  a GedaMenuItem
 *
 * \returns %TRUE if the menu item will appear at the
 *          far right if added to a menu bar.
 *
 * \sa  geda_menu_item_set_right_justified()
 */
bool geda_menu_item_get_right_justified (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), FALSE);

  return menu_item->priv->right_justify;
}

static void geda_menu_item_accel_name_foreach (GtkWidget *widget, void *data)
{
  const char **path_p = data;

  if (!*path_p) {

    if (GEDA_IS_LABEL (widget)) {

      *path_p = geda_label_get_text ((GedaLabel*)widget);

      if (*path_p && (*path_p)[0] == 0) {
        *path_p = NULL;
      }
    }
    else if (GTK_IS_CONTAINER(widget)) {
      geda_container_foreach (widget,
                              geda_menu_item_accel_name_foreach,
                              data);
    }
  }
}

/*!
 * \brief Refresh GedaMenuItem Accelertor Path
 * \par Function Description
 *  Called from GedaMenu routines as part of a scheme to "auto"
 *  set/update accelertor paths when the accelertor path of the
 *  parent menu is set.
 *  \see geda_menu_set_accel_path
 */
void geda_menu_item_refresh_accel_path (GedaMenuItem  *menu_item,
                                        const char    *prefix,
                                        GtkAccelGroup *accel_group,
                                        bool           group_changed)
{
  GedaMenuItemPrivate *priv;
  GtkWidget  *widget;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  priv   = menu_item->priv;
  widget = (GtkWidget*)menu_item;

  if (!accel_group) {
    gtk_widget_set_accel_path (widget, NULL, NULL);
  }
  else if (GTK_IS_ACCEL_GROUP(accel_group)) {

    const char *path;

    path = geda_widget_get_accel_path (widget, NULL);

    if (!path) { /* no active accel_path yet */

      path = priv->accel_path;

      if (!path && prefix) {

        const char *postfix = NULL;

        /* try to construct one from label text */
        geda_container_foreach (menu_item,
                                geda_menu_item_accel_name_foreach,
                               &postfix);
        if (postfix) {

          char *new_path;

          new_path = g_strconcat (prefix, "/", postfix, NULL);
          path = priv->accel_path = (char*)g_intern_string (new_path);
          g_free (new_path);
        }
      }

      if (path) {
        gtk_widget_set_accel_path (widget, path, accel_group);
      }
    }
    else if (group_changed) {   /* reinstall accelerators */
      gtk_widget_set_accel_path (widget, path, accel_group);
    }
  }
}

/*!
 * \brief geda_menu_item_set_accel_path
 * \par Function Description
 * Set the accelerator path on \a menu_item, through which runtime
 * changes of the menu item's accelerator caused by the user can be
 * identified and saved to persistent storage (see gtk_accel_map_save()
 * on this). To set up a default accelerator for this menu item, call
 * gtk_accel_map_add_entry() with the same \a accel_path. See also
 * gtk_accel_map_add_entry() on the specifics of accelerator paths,
 * and geda_menu_set_accel_path() for a more convenient variant of
 * this function.
 *
 * This function is basically a convenience wrapper that handles
 * calling gtk_widget_set_accel_path() with the appropriate accelerator
 * group for the menu item.
 *
 * Note that you do need to set an accelerator on the parent menu with
 * geda_menu_set_accel_group() for this to work.
 *
 * \note \a accel_path string will be stored in a GQuark. Therefore,
 *        if you pass a static string, you can save some memory by
 *        interning it first with g_intern_static_string().
 *
 * \param [in] menu_item  a GedaMenuItem
 * \param [in] accel_path accelerator path, corresponding to this menu
 *                        item's functionality,or %NULL to unset the
 *                        current path
 */
void geda_menu_item_set_accel_path (GedaMenuItem *menu_item,
                                    const char   *accel_path)
{
  GedaMenuItemPrivate *priv = menu_item->priv;
  GtkWidget *parent;
  GtkWidget *widget;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));
  g_return_if_fail (accel_path == NULL ||
                   (accel_path[0] == '<' && strchr (accel_path, '/')));

  widget = (GtkWidget*)menu_item;

  /* store new path */
  priv->accel_path = (char*)g_intern_string (accel_path);

  /* forget accelerators associated with old path */
  gtk_widget_set_accel_path (widget, NULL, NULL);

  /* install accelerators associated with new path */
  parent = gtk_widget_get_parent (widget);

  if (GEDA_IS_MENU(parent)) {

    GtkAccelGroup *accel_group;

    accel_group = geda_menu_get_accel_group ((GedaMenu*)parent);

    if (accel_group) {
      geda_menu_item_refresh_accel_path (menu_item,
                                         NULL,
                                         accel_group,
                                         FALSE);
    }
  }
}

/*!
 * \brief geda_menu_item_get_accel_path
 * \par Function Description
 *  Retrieve the accelerator path that was previously set on \a menu_item.
 *  See geda_menu_item_set_accel_path() for details.
 *
 * \param [in] menu_item a GedaMenuItem
 *
 * \returns Accelerator path corresponding to this menu
 *          item's functionality, or %NULL if not set
 */
const char *geda_menu_item_get_accel_path (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), NULL);

  return menu_item->priv->accel_path;
}

unsigned short geda_menu_item_get_accel_width (GedaMenuItem  *menu_item)
{
  GedaMenuItemPrivate *priv;

  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), 0);

  priv = menu_item->priv;

  if (!priv->accelerator_width) {

    unsigned int accel_width = 0;

    geda_container_foreach (menu_item,
                            geda_menu_item_accel_width_foreach,
                            &accel_width);

    priv->accelerator_width = accel_width;
  }

  return menu_item->priv->accelerator_width;
}

static void geda_menu_item_forall (GtkContainer *container,
                                   bool          include_internals,
                                   GtkCallback   callback,
                                   void         *callback_data)
{
  GtkWidget *child;

  child = geda_get_child_widget (container);

  if (child) {
    callback (child, callback_data);
  }
}

/*! \internal
 * \param [in] menu_item should be validated by callers */
static void geda_menu_item_ensure_label (GedaMenuItem *menu_item)
{
  if (!geda_get_child_widget (menu_item)) {

    GtkWidget *accel_label;

    accel_label = g_object_new (GEDA_TYPE_ACCEL_LABEL, NULL);

    gtk_misc_set_alignment ((GtkMisc*)accel_label, 0.0, 0.5);

    geda_container_add ((menu_item), accel_label);

    geda_accel_label_set_accel_widget ((GedaAccelLabel*)accel_label,
                                       (GtkWidget*)menu_item);
    gtk_widget_show (accel_label);
  }
}

/*!
 * \brief Get if a GedaMenuItem is Selectable
 * \par Function Description
 *  Returns TRUE if the menu item is selectable. A menu item
 *  is selectable if the item has a child and the item is not
 *  a separator item and must be sensitive and visible. This
 *  function is called when a mnemonic is pressed to verify
 *  that the item whose mnemonic was pressed was selectable.
 *
 * \note This function is not applicable to menu navigation
 *       using the mouse.
 *
 * \internal Callers:
 * geda_menu_shell_activate_mnemonic (mnemonic pressed in menu)
 * geda_menu_bar_key_press           (mnemonic pressed on a menu bar)
 */
bool geda_menu_item_is_selectable (GedaMenuItem  *menu_item)
{
  if (is_a_geda_menu_item(menu_item)) {

    if ((!geda_get_child_widget (menu_item) &&
          GEDA_IS_MENU_SEPERATOR (menu_item)) ||
         !gtk_widget_is_sensitive ((GtkWidget*)menu_item) ||
         !gtk_widget_get_visible ((GtkWidget*)menu_item))
      return FALSE;

    return TRUE;
  }
  return FALSE;
}

/*!
 * \brief Get if a GedaMenuItem Widget is Selectable
 * \par Function Description
 *  Returns TRUE if the menu item is selectable.
 *
 * \see geda_menu_item_is_selectable
 */
bool geda_menu_item_is_widget_selectable (GtkWidget *widget)
{
  if (GEDA_IS_MENU_ITEM(widget)) {

    if ((!geda_get_child_widget (widget) &&
          GEDA_IS_MENU_SEPERATOR (widget)) ||
         !gtk_widget_is_sensitive (widget) ||
         !gtk_widget_get_visible (widget))
      return FALSE;

    return TRUE;
  }

  return FALSE;
}

/*!
 * \brief Retrieve menu item Toggle Size
 * \par Function Description
 *  Returns the toggle size associated with \a menu_item in pixels.
 *
 * \param [in] menu_item a GedaMenuItem
 *
 * \returns The toggle size of the \a menu_item
 */
unsigned short geda_menu_item_get_toggle_size (GedaMenuItem  *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), 0);

  return menu_item->priv->toggle_size;
}

/*!
 * \brief Get menu item from menubar member
 * \par Function Description
 *  Gets the \a menu_item from-menubar member.
 *
 * \param [in] menu_item a GedaMenuItem
 *
 * \retval TRUE if the parent of \a menu_item is MenuBar.
 */
bool geda_menu_item_get_from_menubar (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), FALSE);

  return menu_item->priv->from_menubar;
}

/*!
 * \brief Set the Menu Item Label
 * \par Function Description
 *  Sets \a text on the \a menu_item label
 *
 * \param [in] menu_item a GedaMenuItem
 * \param [in] text      text to be used in the label
 */
void geda_menu_item_set_label (GedaMenuItem *menu_item, const char *text)
{
  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  GEDA_MENU_ITEM_GET_CLASS (menu_item)->set_label (menu_item, text);
}

/*!
 * \brief Retrieve menu item label
 * \par Function Description
 *  Gets text on the \a menu_item label
 *
 * \param [in] menu_item a GedaMenuItem
 *
 * \returns The text in the \a menu_item label. This is the internal
 *          string used by the label, and must not be modified.
 */
const char *geda_menu_item_get_label (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), NULL);

  return GEDA_MENU_ITEM_GET_CLASS (menu_item)->get_label (menu_item);
}

/*!
 * \brief Retrieve menu item label Widget
 * \par Function Description
 *  Gets \a menu_item GedaLabel widget.
 *
 * \param [in] menu_item a GedaMenuItem
 *
 * \returns GedaLabel object.
 */
GtkWidget *geda_menu_item_get_label_widget (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), NULL);

  if (!GEDA_IS_MENU_SEPERATOR (menu_item)) {

    geda_menu_item_ensure_label (menu_item);

    return geda_get_child_widget (menu_item);
  }

  return NULL;
}

/*!
 * \brief Set whether menu item should use underline
 * \par Function Description
 * If true, an underline in the text indicates the next character
 * should be used for the mnemonic accelerator key.
 *
 * \param [in] menu_item a GedaMenuItem
 * \param [in] setting   %TRUE if underlines in the text indicate mnemonics
 */
void geda_menu_item_set_use_underline (GedaMenuItem *menu_item, bool setting)
{
  GtkWidget *child;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  geda_menu_item_ensure_label (menu_item);

  child = geda_get_child_widget (menu_item);

  if (GEDA_IS_LABEL(child)) {

    geda_label_set_use_underline ((GedaLabel*)child, setting);

    GEDA_OBJECT_NOTIFY (menu_item, "use-underline");
  }
}

/*!
 * \brief Get if menu item should use underline
 * \par Function Description
 * Checks if an underline in the text indicates the next character
 * should be used for the mnemonic accelerator key.
 *
 * \param [in] menu_item a GedaMenuItem
 *
 * \returns %TRUE if an embedded underline in the label
 *          indicates the mnemonic accelerator key.
 */
bool geda_menu_item_get_use_underline (GedaMenuItem *menu_item)
{
  GedaLabel *child;

  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), FALSE);

  geda_menu_item_ensure_label (menu_item);

  child = geda_get_child_widget (menu_item);

  if (GEDA_IS_LABEL (child)) {
    return geda_label_get_use_underline (child);
  }

  return FALSE;
}

/*!
 * \brief Set the Active related to the Menu Item
 * \par Function Description
 *  Sets menu_item "related-action" property.
 *
 * \param [in] menu_item a GedaMenuItem
 * \param [in] action    The action related to the menu item
 */
void geda_menu_item_set_related_action (GedaMenuItem *menu_item, GtkAction *action)
{
  GedaMenuItemPrivate *priv = menu_item->priv;

  if (priv->action != (GedaAction*)action) {

    if (priv->action) {
      geda_menu_item_disconnect_accelerator(priv->action);
    }

    if (action) {

      const char *accel_path;

      accel_path = gtk_action_get_accel_path (action);

      if (accel_path) {
        gtk_action_connect_accelerator (action);
        geda_menu_item_set_accel_path (menu_item, accel_path);
      }
    }

    gtk_activatable_do_set_related_action ((GtkActivatable*)menu_item, action);

    priv->action = (GedaAction*)action;
  }
}

/*!
 * \brief Set whether to Reserve space for an Indicator
 * \par Function Description
 *  Sets whether the \a menu_item should reserve space for the submenu
 *  indicator, regardless if it actually has a submenu or not.
 *
 *  There should be little need for applications to call
 *  this functions.
 *
 * \param [in] menu_item a GedaMenuItem
 * \param [in] reserve   a the new value
 */
void geda_menu_item_set_reserve_indicator (GedaMenuItem *menu_item, bool reserve)
{
  GedaMenuItemPrivate *priv;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  priv = menu_item->priv;

  if (priv->reserve_indicator != reserve) {

    priv->reserve_indicator = reserve;
    gtk_widget_queue_resize ((GtkWidget*)menu_item);
  }
}

/*!
 * \brief Get if Reserving space for an Indicator
 * \par Function Description
 *  Returns whether the \a menu_item reserves space for the
 *  submenu indicator, regardless if it has a submenu or not.
 *
 * \param [in] menu_item a GedaMenuItem
 *
 * \returns %TRUE if \a menu_item always reserves space for the
 *          submenu indicator
 */
bool geda_menu_item_get_reserve_indicator (GedaMenuItem *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), FALSE);

  return menu_item->priv->reserve_indicator;
}

/*!
 * \brief Get the GedaMenuItem show_submenu_indicator property
 * \par Function Description
 *  Returns whether the \a menu_item submenu indicator should displayed or not.
 *
 * \param [in] menu_item a GedaMenuItem
 *
 * \returns %TRUE if \a menu_item always reserves space for the
 *          submenu indicator
 */
bool geda_menu_item_get_show_submenu_indicator (GedaMenuItem  *menu_item)
{
  g_return_val_if_fail (GEDA_IS_MENU_ITEM(menu_item), FALSE);

  return menu_item->priv->show_submenu_indicator;
}

/*!
 * \brief Set the GedaMenuItem show_submenu_indicator property
 * \par Function Description
 *  Sets whether the \a menu_item submenu indicator should displayed or not.
 *
 * \param [in] menu_item a GedaMenuItem
 * \param [in] show      Boolean value, if True the indicated will be shown
 */
void geda_menu_item_set_show_submenu_indicator (GedaMenuItem  *menu_item, bool show)
{
  GedaMenuItemPrivate *priv;

  g_return_if_fail (GEDA_IS_MENU_ITEM(menu_item));

  priv = menu_item->priv;

  if (priv->show_submenu_indicator != show) {

    priv->show_submenu_indicator = show;
    gtk_widget_queue_resize ((GtkWidget*)menu_item);
  }
}

/*!
 * \brief Set the Menu Item Use-Action Appearance Property
 * \par Function Description
 *  Sets menu_item "use-action-appearance" property.
 *
 * \param [in] menu_item      A GedaMenuItem
 * \param [in] use_appearance Whether the action appearance should be used
 */
void geda_menu_item_set_use_action_appearance (GedaMenuItem *menu_item,
                                               bool          use_appearance)
{
  GedaMenuItemPrivate *priv = menu_item->priv;

  if (priv->use_action_appearance != use_appearance) {

    priv->use_action_appearance = use_appearance;

    gtk_activatable_sync_action_properties ((GtkActivatable*)menu_item,
                                            (GtkAction*)priv->action);
  }
}

/** @} geda-menu-item */
