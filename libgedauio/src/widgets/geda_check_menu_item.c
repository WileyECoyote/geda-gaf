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

/*! \file geda_check_menu_item.c
 *  \brief GedaCheckMenuItem Class Module
 */

/** \defgroup geda-check-menu-item GedaCheckMenuItem Object
 * @{
 * \brief Implmentation of #GedaCheckMenuItem Class
 *
 * \class GedaCheckMenuItem geda_check_menu_item.h "include/geda_check_menu_item.h"
 * \implements GedaMenuItem
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_gtk_compat.h"
#include "../../include/geda_uio_functions.h"
#include "../../include/geda_menu_enum.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_check_menu_item.h"
#include "../../include/gettext.h"

enum {
  TOGGLED,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_ACTIVE,
  PROP_INCONSISTENT,
  PROP_DRAW_AS_RADIO
};

static int  geda_check_menu_item_expose               (GtkWidget             *widget,
                                                       GdkEventExpose        *event);
static void geda_check_menu_item_activate             (GedaMenuItem           *menu_item);
static void geda_check_menu_item_toggle_size_request  (GedaMenuItem           *menu_item,
                                                       int                   *requisition);
static void geda_check_menu_item_draw_indicator       (GedaCheckMenuItem     *check_menu_item,
                                                       GdkRectangle          *area);
static void geda_real_check_menu_item_draw_indicator  (GedaCheckMenuItem     *check_menu_item,
                                                       GdkRectangle          *area);
static void geda_check_menu_item_set_property         (GObject               *object,
                                                       unsigned int           prop_id,
                                                       const GValue          *value,
                                                       GParamSpec            *pspec);
static void geda_check_menu_item_get_property         (GObject               *object,
                                                       unsigned int           prop_id,
                                                       GValue                *value,
                                                       GParamSpec            *pspec);

static void geda_check_menu_item_activatable_interface_init (GtkActivatableIface  *iface);
static void geda_check_menu_item_update                     (GtkActivatable       *activatable,
                                                             GtkAction            *action,
                                                             const char           *property_name);
static void geda_check_menu_item_sync_action_properties     (GtkActivatable       *activatable,
                                                             GtkAction            *action);

static GtkActivatableIface *parent_activatable_iface;
static unsigned int check_menu_item_signals[LAST_SIGNAL] = { 0 };

static void *geda_check_menu_item_parent_class = NULL;

static GHashTable *check_menu_item_hash = NULL;

static void
geda_check_menu_item_draw_indicator (GedaCheckMenuItem *check_menu_item,
                                     GdkRectangle      *area)
{
  if (GEDA_CHECK_MENU_ITEM_GET_CLASS (check_menu_item)->draw_indicator)
    GEDA_CHECK_MENU_ITEM_GET_CLASS (check_menu_item)->draw_indicator (check_menu_item, area);
}

/* widget_class->expose_event */
static int
geda_check_menu_item_expose (GtkWidget *widget, GdkEventExpose *event)
{
  if (((GtkWidgetClass*)geda_check_menu_item_parent_class)->expose_event) {
    ((GtkWidgetClass*)geda_check_menu_item_parent_class)->expose_event (widget, event);
  }

  geda_check_menu_item_draw_indicator ((GedaCheckMenuItem*)widget, &event->area);

  return FALSE;
}

/* menu_item_class->activate */
static void
geda_check_menu_item_activate (GedaMenuItem *menu_item)
{
  GedaCheckMenuItem *check_menu_item = (GedaCheckMenuItem*)menu_item;
  check_menu_item->active            = !check_menu_item->active;

  geda_check_menu_item_toggled (check_menu_item);
  gtk_widget_queue_draw ((GtkWidget*)check_menu_item);

  ((GedaMenuItemClass*)geda_check_menu_item_parent_class)->activate (menu_item);

  GEDA_OBJECT_NOTIFY (check_menu_item, "active");
}

/* menu_item_class->toggle_size_request */
static void geda_check_menu_item_toggle_size_request (GedaMenuItem *menu_item,
                                                               int *requisition)
{
  unsigned int toggle_spacing;
  unsigned int indicator_size;

  gtk_widget_style_get ((GtkWidget*)menu_item,
                        "toggle-spacing", &toggle_spacing,
                        "indicator-size", &indicator_size,
                        NULL);

  *requisition = indicator_size + toggle_spacing;
}

/* class->draw_indicator */
static void
geda_real_check_menu_item_draw_indicator (GedaCheckMenuItem *check_menu_item,
                                          GdkRectangle      *area)
{
  GtkWidget *widget;

  widget = (GtkWidget*)check_menu_item;

  if (gtk_widget_is_drawable (widget)) {

    GtkStyle *style;

    unsigned int offset;
    unsigned int border_width;
    unsigned int toggle_size;
    unsigned int toggle_spacing;
    unsigned int horizontal_padding;
    unsigned int indicator_size;
    int x, y;

    gtk_widget_style_get (widget,
                          "toggle-spacing",     &toggle_spacing,
                          "horizontal-padding", &horizontal_padding,
                          "indicator-size",     &indicator_size,
                          NULL);

    style        = widget->style;
    border_width = geda_get_container_border_width(check_menu_item);
    offset       = border_width + style->xthickness + 2;
    toggle_size  = geda_menu_item_get_toggle_size((GedaMenuItem*)check_menu_item);

    /* This is always added to x, even when direction is RTL */
    offset += (toggle_size - toggle_spacing - indicator_size) >> 1; /* divide by 2 */

    if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR) {
      x = widget->allocation.x + offset + horizontal_padding;
    }
    else {
      x = widget->allocation.x + widget->allocation.width -
          offset - horizontal_padding - toggle_size + toggle_spacing;
    }

    y = widget->allocation.y + ((widget->allocation.height - indicator_size) >> 1);

    if (check_menu_item->active ||
        check_menu_item->always_show_toggle ||
       (gtk_widget_get_state (widget) == GTK_STATE_PRELIGHT))
    {

      GtkStateType   state_type;
      GtkShadowType  shadow_type;
      GdkWindow     *window;

      if (gtk_widget_is_sensitive (widget)) {
        state_type = gtk_widget_get_state (widget);
      }
      else {
        state_type = GTK_STATE_INSENSITIVE;
      }

      if (check_menu_item->inconsistent)
        shadow_type = GTK_SHADOW_ETCHED_IN;
      else if (check_menu_item->active)
        shadow_type = GTK_SHADOW_IN;
      else
        shadow_type = GTK_SHADOW_OUT;

      window = geda_get_widget_window(widget);

      if (check_menu_item->draw_as_radio) {

        gtk_paint_option (style, window,
                          state_type, shadow_type,
                          area, widget, "option",
                          x, y, indicator_size, indicator_size);
      }
      else {
        gtk_paint_check (style, window,
                         state_type, shadow_type,
                         area, widget, "check",
                         x, y, indicator_size, indicator_size);
      }
    }
  }
}

static void geda_check_menu_item_get_property (GObject      *object,
                                               unsigned int  prop_id,
                                               GValue       *value,
                                               GParamSpec   *pspec)
{
  GedaCheckMenuItem *checkitem = (GedaCheckMenuItem*)object;

  switch (prop_id) {

    case PROP_ACTIVE:
      g_value_set_boolean (value, checkitem->active);
      break;

    case PROP_INCONSISTENT:
      g_value_set_boolean (value, checkitem->inconsistent);
      break;

    case PROP_DRAW_AS_RADIO:
      g_value_set_boolean (value, checkitem->draw_as_radio);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_check_menu_item_set_property (GObject      *object,
                                               unsigned int  prop_id,
                                               const GValue *value,
                                               GParamSpec   *pspec)
{
  GedaCheckMenuItem *checkitem = (GedaCheckMenuItem*)object;

  switch (prop_id) {

    case PROP_ACTIVE:
      geda_check_menu_item_set_active (checkitem, g_value_get_boolean (value));
      break;

    case PROP_INCONSISTENT:
      geda_check_menu_item_set_inconsistent (checkitem, g_value_get_boolean (value));
      break;

    case PROP_DRAW_AS_RADIO:
      geda_check_menu_item_set_draw_as_radio (checkitem, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void geda_check_menu_item_finalize (GObject *object)
{
  //GedaCheckMenuItem *check_menu_item = GEDA_CHECK_MENU_ITEM (object);

  if (g_hash_table_remove (check_menu_item_hash, object)) {
    if (!g_hash_table_size (check_menu_item_hash)) {
      g_hash_table_destroy (check_menu_item_hash);
      check_menu_item_hash = NULL;
    }
  }

  ((GObjectClass*)geda_check_menu_item_parent_class)->finalize (object);
}

/*!
 * \brief GedaCheckMenuItem Type Class Initializer
 * \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 * \param [in]  class       GedaCheckMenuItemClass class we are initializing
 * \param [in]  class_data  GedaCheckMenuItem structure associated with the class
 */
static void geda_check_menu_item_class_init(void *class, void *class_data)
{
  GedaCheckMenuItemClass *check_menu_class;
  GObjectClass           *gobject_class;
  GtkWidgetClass         *widget_class;
  GedaMenuItemClass      *menu_item_class;
  GParamSpec             *params;

  check_menu_class = (GedaCheckMenuItemClass*)class;
  gobject_class    = (GObjectClass*) class;
  widget_class     = (GtkWidgetClass*) class;
  menu_item_class  = (GedaMenuItemClass*) class;

  gobject_class->finalize     = geda_check_menu_item_finalize;
  gobject_class->set_property = geda_check_menu_item_set_property;
  gobject_class->get_property = geda_check_menu_item_get_property;

  widget_class->expose_event = geda_check_menu_item_expose;

  menu_item_class->activate            = geda_check_menu_item_activate;
  menu_item_class->hide_on_activate    = FALSE;
  menu_item_class->toggle_size_request = geda_check_menu_item_toggle_size_request;

  check_menu_class->toggled            = NULL;
  check_menu_class->draw_indicator     = geda_real_check_menu_item_draw_indicator;

  geda_check_menu_item_parent_class    = g_type_class_peek_parent(class);

  params = g_param_spec_boolean ("active",
                               _("Active"),
                               _("Whether the menu item is checked"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_ACTIVE, params);

  params = g_param_spec_boolean ("inconsistent",
                               _("Inconsistent"),
                               _("Whether to display an \"inconsistent\" state"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_INCONSISTENT, params);

  params = g_param_spec_boolean ("draw-as-radio",
                               _("Draw as radio menu item"),
                               _("Whether the menu item looks like a radio menu item"),
                                  FALSE,
                                  G_PARAM_READWRITE);

  g_object_class_install_property (gobject_class, PROP_DRAW_AS_RADIO, params);

  params = g_param_spec_int ("indicator-size",
                           _("Indicator Size"),
                           _("Size of check or radio indicator"),
                              0,
                              G_MAXINT,
                              13,
                              G_PARAM_READABLE);

  gtk_widget_class_install_style_property (widget_class, params);

  check_menu_item_signals[TOGGLED] =
    g_signal_new ("toggled",
                   GEDA_TYPE_CHECK_MENU_ITEM,
                   G_SIGNAL_RUN_FIRST,
                   G_STRUCT_OFFSET (GedaCheckMenuItemClass, toggled),
                   NULL, NULL,
                   geda_marshal_VOID__VOID,
                   G_TYPE_NONE, 0);
}

/*!
 * \brief Initialize new GedaCheckMenuItem data structure instance.
 * \par Function Description
 *  This function is call after the GedaCheckMenuItemClass is created
 *  to initialize the data structure.
 *
 * \param [in] instance  A GedaCheckMenuItem data structure
 * \param [in] class     A GedaCheckMenuItemClass Object
 */
static void geda_check_menu_item_instance_init(GTypeInstance *instance, void *class)
{
  GedaCheckMenuItem *check_menu_item = (GedaCheckMenuItem*)instance;

  if (!check_menu_item_hash) {
    check_menu_item_hash = g_hash_table_new (g_direct_hash, NULL);
  }

  g_hash_table_replace (check_menu_item_hash, instance, instance);

  check_menu_item->active             = FALSE;
  check_menu_item->always_show_toggle = TRUE;
}

static void geda_check_menu_item_activatable_interface_init (GtkActivatableIface  *iface)
{
  parent_activatable_iface      = g_type_interface_peek_parent (iface);
  iface->update                 = geda_check_menu_item_update;
  iface->sync_action_properties = geda_check_menu_item_sync_action_properties;
}

/*!
 * \brief Retrieve GedaCheckMenuItem's Type identifier.
 * \par Function Description
 *  Function to retrieve a #GedaCheckMenuItem type identifier. When
 *  first called, the function registers a #GedaCheckMenuItem in the
 *  GType system to obtain an identifier that uniquely itentifies
 *  a GedaCheckMenuItem and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaCheckMenuItem.
 */
GedaType geda_check_menu_item_get_type (void)
{
  static volatile GedaType geda_check_menu_item_type = 0;

  if (g_once_init_enter (&geda_check_menu_item_type)) {

    static const GTypeInfo info = {
      sizeof(GedaCheckMenuItemClass),
      NULL,                                /* base_init           */
      NULL,                                /* base_finalize       */
      geda_check_menu_item_class_init,     /* (GClassInitFunc)    */
      NULL,                                /* class_finalize      */
      NULL,                                /* class_data          */
      sizeof(GedaCheckMenuItem),
      0,                                   /* n_preallocs         */
      geda_check_menu_item_instance_init   /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaCheckMenuItem");
    type   = g_type_register_static (GEDA_TYPE_MENU_ITEM, string, &info, 0);

    const GInterfaceInfo interface_info = {
      (GInterfaceInitFunc) geda_check_menu_item_activatable_interface_init,
      NULL,
      NULL
    };

    g_type_add_interface_static (type, GTK_TYPE_ACTIVATABLE, &interface_info);

    g_once_init_leave (&geda_check_menu_item_type, type);
  }

  return geda_check_menu_item_type;
}

/*!
 * \brief Check if an object is a GedaCheckMenuItem
 * \par Function Description
 *  Determines if \a check_menu_item is valid by verifying \a check_menu_item
 *  is included in the hash table of GedaCheckMenuItem objects.
 *
 * \retval TRUE if \a check_menu_item is a valid GedaCheckMenuItem
 */
bool is_a_geda_check_menu_item (GedaCheckMenuItem *check_menu_item)
{
  if ((check_menu_item != NULL) && (check_menu_item_hash != NULL)) {
    return g_hash_table_lookup(check_menu_item_hash, check_menu_item) ? TRUE : FALSE;
  }
  return FALSE;
}

static void geda_check_menu_item_update (GtkActivatable *activatable,
                                         GtkAction      *action,
                                         const char     *property_name)
{
  GedaCheckMenuItem *check_menu_item;
  bool use_appearance;

  check_menu_item = (GedaCheckMenuItem*)activatable;

  parent_activatable_iface->update (activatable, action, property_name);

  if (strcmp (property_name, "active") == 0) {

    bool active;

    active = gtk_toggle_action_get_active ((GtkToggleAction*)action);

    gtk_action_block_activate (action);
    geda_check_menu_item_set_active (check_menu_item, active);
    gtk_action_unblock_activate (action);
  }

  g_object_get (activatable, "use-action-appearance", &use_appearance, NULL);

  if (use_appearance) {

    if (strcmp (property_name, "draw-as-radio") == 0) {

      bool draw_as_radio;

      draw_as_radio = gtk_toggle_action_get_draw_as_radio ((GtkToggleAction*)action);

      geda_check_menu_item_set_draw_as_radio (check_menu_item, draw_as_radio);
    }
  }
}

/* TODO GtkAction->GedaAction (maybe) */
static void geda_check_menu_item_sync_action_properties (GtkActivatable *activatable,
                                                         GtkAction      *action)
{
  GedaCheckMenuItem *check_menu_item;

  check_menu_item = (GedaCheckMenuItem*)activatable;

  parent_activatable_iface->sync_action_properties (activatable, action);

  if (GTK_IS_TOGGLE_ACTION (action)) {

    bool active;
    bool use_appearance;

    active = gtk_toggle_action_get_active ((GtkToggleAction*)action);

    gtk_action_block_activate (action);

    geda_check_menu_item_set_active (check_menu_item, active);

    gtk_action_unblock_activate (action);

    g_object_get (activatable, "use-action-appearance", &use_appearance, NULL);

    if (use_appearance) {

      bool draw_as_radio;

      draw_as_radio = gtk_toggle_action_get_draw_as_radio ((GtkToggleAction*)action);

      geda_check_menu_item_set_draw_as_radio (check_menu_item, draw_as_radio);
    }
  }
}

/*!
 * \brief Create new CheckMenuItem
 * \ingroup CheckMenuItem
 * \par Function Description
 * Creates a new #GedaCheckMenuItem containing a label.
 *
 * \returns a new #GedaCheckMenuItem
 */
GtkWidget *geda_check_menu_item_new (void)
{
  return g_object_new (GEDA_TYPE_CHECK_MENU_ITEM, NULL);
}

/*!
 * \brief Create new CheckMenuItem with a label
 * \ingroup CheckMenuItem
 * \par Function Description
 *  Creates a new #GedaCheckMenuItem containing a label.
 *
 * \param [in] label Text for display as the label
 *
 * \returns a new #GedaCheckMenuItem
 */
GtkWidget *geda_check_menu_item_new_with_label (const char *label)
{
  return g_object_new (GEDA_TYPE_CHECK_MENU_ITEM,
                      "label", label,
                       NULL);
}

/*!
 * \brief Create new CheckMenuItem with a mnemonic label
 * \ingroup CheckMenuItem
 * \par Function Description
 *  Creates a new #GedaCheckMenuItem containing a label. The label
 *  will be created using geda_label_new_with_mnemonic(), so under
 *  scores in \a label indicate the mnemonic for the menu item.
 *
 * \param [in] label Text with mnemonic for display as the label
 *
 * \returns a new #GedaCheckMenuItem
 */
GtkWidget *geda_check_menu_item_new_with_mnemonic (const char *label)
{
  return g_object_new (GEDA_TYPE_CHECK_MENU_ITEM,
                      "label", label,
                      "use-underline", TRUE,
                       NULL);
}

/*!
 * \brief Get CheckMenuItem Active
 * \ingroup CheckMenuItem
 * \par Function Description
 *  Returns whether the check menu item is active.
 *
 * \param [in] check_menu_item a #GedaCheckMenuItem
 *
 * \returns %TRUE if the menu item is checked.
 *
 * \sa geda_check_menu_item_set_active
 */
bool geda_check_menu_item_get_active (GedaCheckMenuItem *check_menu_item)
{
  g_return_val_if_fail (GEDA_IS_CHECK_MENU_ITEM (check_menu_item), FALSE);

  return check_menu_item->active;
}

/*!
 * \brief  Set a CheckMenuItem Active
 * \ingroup CheckMenuItem
 * \par Function Description
 *  Programmatically sets \a check_menu_item to the given state.
 *
 * \param [in] check_menu_item a #GedaCheckMenuItem
 * \param [in] is_active       boolean state to be set
 *
 * \sa geda_check_menu_item_get_active
 */
void geda_check_menu_item_set_active (GedaCheckMenuItem *check_menu_item,
                                      bool               is_active)
{
  g_return_if_fail (GEDA_IS_CHECK_MENU_ITEM (check_menu_item));

  is_active = is_active != 0;

  if (check_menu_item->active != is_active) {
    geda_menu_item_activate ((GedaMenuItem*)check_menu_item);
  }
}

/*!
 * \brief Get GedaCheckMenuItem draw-as-radio property
 * \par Function Description
 *  Returns whether \a check_menu_item looks like a #GedaRadioMenuItem
 *
 * \param [in] check_menu_item: a #GedaCheckMenuItem
 *
 * \returns Whether the menu item looks like a #GedaRadioMenuItem
 */
bool geda_check_menu_item_get_draw_as_radio (GedaCheckMenuItem *check_menu_item)
{
  g_return_val_if_fail (GEDA_IS_CHECK_MENU_ITEM (check_menu_item), FALSE);

  return check_menu_item->draw_as_radio;
}

/*!
 * \brief Set GedaCheckMenuItem draw-as-radio property
 * \par Function Description
 *  Set whether \a check_menu_item is drawn like a #GedaRadioMenuItem
 *
 * \param [in] check_menu_item Pointer to a #GedaCheckMenuItem
 * \param [in] draw_as_radio   If TRUE the indicator will be drawn as a radio.
 */
void geda_check_menu_item_set_draw_as_radio (GedaCheckMenuItem *check_menu_item,
                                             bool               draw_as_radio)
{
  g_return_if_fail (GEDA_IS_CHECK_MENU_ITEM (check_menu_item));

  draw_as_radio = draw_as_radio != FALSE;

  if (draw_as_radio != check_menu_item->draw_as_radio) {

    check_menu_item->draw_as_radio = draw_as_radio;

    gtk_widget_queue_draw ((GtkWidget*)check_menu_item);

    GEDA_OBJECT_NOTIFY (check_menu_item, "draw-as-radio");
  }
}

/*!
 * \brief Get GedaCheckMenuItem inconsistent setting
 * \par Function Description
 *  Retrieves the value set by geda_check_menu_item_set_inconsistent().
 *
 * \param [in] check_menu_item Pointer to a #GedaCheckMenuItem
 *
 * \retval %TRUE if inconsistent
 */
bool geda_check_menu_item_get_inconsistent (GedaCheckMenuItem *check_menu_item)
{
  g_return_val_if_fail (GEDA_IS_CHECK_MENU_ITEM (check_menu_item), FALSE);

  return check_menu_item->inconsistent;
}

/*!
 * \brief Set CheckMenuItem Inconsistent
 * \ingroup GedaCheckMenuItem
 * \par Function Description
 *  If the user has selected a range of elements (such as some text or
 *  spreadsheet cells) that are affected by a boolean setting, and the
 *  current values in that range are inconsistent, you may want to
 *  display the check in an "in between" state. This function turns on
 *  "in between" display.  Normally you would turn off the inconsistent
 *  state again if the user explicitly selects a setting. This has to be
 *  done manually, geda_check_menu_item_set_inconsistent() only affects
 *  visual appearance, it doesn't affect the semantics of the widget.
 *
 * \param [in] check_menu_item Pointer to #GedaCheckMenuItem
 * \param [in] setting         %TRUE to display an "inconsistent" third state check
 */
void geda_check_menu_item_set_inconsistent (GedaCheckMenuItem *check_menu_item,
                                            bool               setting)
{
  g_return_if_fail (GEDA_IS_CHECK_MENU_ITEM (check_menu_item));

  setting = setting != FALSE;

  if (setting != check_menu_item->inconsistent) {

    check_menu_item->inconsistent = setting;
    gtk_widget_queue_draw ((GtkWidget*)check_menu_item);
    GEDA_OBJECT_NOTIFY (check_menu_item, "inconsistent");
  }
}

/*!
 * \brief Retrieve GedaCheckMenuItem show toggle setting
 * \par Function Description
 *  Get whether \a menu_item toggle is drawn
 *
 * \param [in] menu_item a #GedaCheckMenuItem
 *
 * \returns value of always_show_toggle
 */
bool geda_check_menu_item_get_show_toggle (GedaCheckMenuItem *menu_item)
{
  if (GEDA_IS_CHECK_MENU_ITEM (menu_item)) {
    return menu_item->always_show_toggle;
  }
  return 0;
}

/*!
 * \brief Set GedaCheckMenuItem show toggle
 * \par Function Description
 *  Set whether \a menu_item toggle is drawn
 *
 * \param [in] menu_item a #GedaCheckMenuItem
 * \param [in] show      %TRUE if the toggle widget is to be displayed
 */
void geda_check_menu_item_set_show_toggle (GedaCheckMenuItem *menu_item,
                                           bool               show)
{
  if (GEDA_IS_CHECK_MENU_ITEM (menu_item)) {
    menu_item->always_show_toggle = show;
  }
}

/*!
 * \brief Toggle a GedaCheckMenuItem
 * \par Function Description
 *  Emits a "toggled" signal on \a check_menu_item.
 *
 * \param [in] check_menu_item a #GedaCheckMenuItem
 */
void geda_check_menu_item_toggled (GedaCheckMenuItem *check_menu_item)
{
  g_signal_emit (check_menu_item, check_menu_item_signals[TOGGLED], 0);
}

/** @} geda-check-menu-item */
