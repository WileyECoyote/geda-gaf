/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Jsh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif

#include <gtk/gtk.h>

#include <geda/geda.h>
#include <geda/geda_standard.h>

#include "../../include/geda_menu.h"
#include "../../include/geda_menu_item.h"
#include "../../include/geda_menu_shell.h"
#include "../../include/geda_option_menu.h"

#include "../../include/geda_keysyms.h"
#include "../../include/gettext.h"

#define CHILD_LEFT_SPACING        4
#define CHILD_RIGHT_SPACING       1
#define CHILD_TOP_SPACING         1
#define CHILD_BOTTOM_SPACING      1

typedef struct _GedaOptionMenuProps GedaOptionMenuProps;

struct _GedaOptionMenuProps
{
  bool interior_focus;
  GtkRequisition indicator_size;
  GtkBorder indicator_spacing;
  int focus_width;
  int focus_pad;
};

static const GedaOptionMenuProps default_props = {
  TRUE,
  { 7, 13 },
  { 7, 5, 2, 2 },       /* Left, right, top, bottom */
  1,
  0
};

static void geda_option_menu_destroy         (GtkObject          *object);
static void geda_option_menu_set_property    (GObject            *object,
                                              unsigned int        prop_id,
                                              const GValue       *value,
                                              GParamSpec         *pspec);
static void geda_option_menu_get_property    (GObject            *object,
                                              unsigned int        prop_id,
                                              GValue             *value,
                                              GParamSpec         *pspec);
static void geda_option_menu_size_request    (GtkWidget          *widget,
                                              GtkRequisition     *requisition);
static void geda_option_menu_size_allocate   (GtkWidget          *widget,
                                              GtkAllocation      *allocation);
static void geda_option_menu_paint           (GtkWidget          *widget,
                                              GdkRectangle       *area);
static int geda_option_menu_expose           (GtkWidget          *widget,
                                              GdkEventExpose     *event);
static int geda_option_menu_button_press     (GtkWidget          *widget,
                                              GdkEventButton     *event);
static int geda_option_menu_key_press        (GtkWidget          *widget,
                                              GdkEventKey        *event);
static void geda_option_menu_selection_done  (GedaMenuShell      *menu_shell,
                                              GedaOptionMenu     *option_menu);
static void geda_option_menu_update_contents (GedaOptionMenu     *option_menu);
static void geda_option_menu_remove_contents (GedaOptionMenu     *option_menu);
static void geda_option_menu_calc_size       (GedaOptionMenu     *option_menu);
static void geda_option_menu_position        (GedaMenu            *menu,
                                              int                *x,
                                              int                *y,
                                              int                *scroll_offet,
                                              void               *user_data);
static void geda_option_menu_show_all        (GtkWidget          *widget);
static void geda_option_menu_hide_all        (GtkWidget          *widget);
static bool geda_option_menu_mnemonic_activate (GtkWidget        *widget,
                                                bool              group_cycling);
static GType geda_option_menu_child_type     (GtkContainer       *container);
static int geda_option_menu_scroll_event     (GtkWidget          *widget,
                                              GdkEventScroll     *event);

enum
{
  CHANGED,
  LAST_SIGNAL
};

enum
{
  PROP_0,
  PROP_MENU
};

static unsigned int signals[LAST_SIGNAL] = { 0 };

static void *geda_option_menu_parent_class = NULL;

static GType
geda_option_menu_child_type (GtkContainer *container)
{
  return G_TYPE_NONE;
}

/*! \brief GedaOptionMenu Type Class Initializer
 *
 *  \par Function Description
 *  Type class initializer called to initialize the class instance.
 *  Overrides parents virtual class methods as needed and registers
 *  GObject signals.
 *
 *  \param [in]  class      GedaOptionMenu class we are initializing
 *  \param [in]  class_data GedaOptionMenu structure associated with the class
 */
static void
geda_option_menu_class_init(void *class, void *class_data)
{
  GObjectClass      *gobject_class;
  GtkObjectClass    *object_class;
  GtkWidgetClass    *widget_class;
  GtkContainerClass *container_class;

  //(GedaOptionMenuClass *class)
  gobject_class    = (GObjectClass*)class;
  object_class     = (GtkObjectClass*)class;
  widget_class     = (GtkWidgetClass*)class;
  container_class  = (GtkContainerClass*)class;

  signals[CHANGED] =
    g_signal_new ("changed",
                  G_OBJECT_CLASS_TYPE (class),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (GedaOptionMenuClass, changed),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  gobject_class->set_property      = geda_option_menu_set_property;
  gobject_class->get_property      = geda_option_menu_get_property;
  object_class->destroy            = geda_option_menu_destroy;

  widget_class->size_request       = geda_option_menu_size_request;
  widget_class->size_allocate      = geda_option_menu_size_allocate;
  widget_class->expose_event       = geda_option_menu_expose;
  widget_class->button_press_event = geda_option_menu_button_press;
  widget_class->key_press_event    = geda_option_menu_key_press;
  widget_class->scroll_event       = geda_option_menu_scroll_event;
  widget_class->show_all           = geda_option_menu_show_all;
  widget_class->hide_all           = geda_option_menu_hide_all;
  widget_class->mnemonic_activate  = geda_option_menu_mnemonic_activate;

  container_class->child_type      = geda_option_menu_child_type;

  geda_option_menu_parent_class    = g_type_class_peek_parent (class);

  g_object_class_install_property (gobject_class,
                                   PROP_MENU,
                                   g_param_spec_object ("menu",
                                                      _("Menu"),
                                                      _("The menu of options"),
                                                        GEDA_TYPE_MENU,
                                                        G_PARAM_READWRITE));

  gtk_widget_class_install_style_property (widget_class,
                       g_param_spec_boxed ("indicator-size",
                                   _("Indicator Size"),
                                   _("Size of dropdown indicator"),
                                   GTK_TYPE_REQUISITION,
                                   G_PARAM_READABLE));

  gtk_widget_class_install_style_property (widget_class,
                       g_param_spec_boxed ("indicator-spacing",
                                   _("Indicator Spacing"),
                                   _("Spacing around indicator"),
                                   GTK_TYPE_BORDER,
                                   G_PARAM_READABLE));
}

/*! \brief Type instance initializer for GedaOptionMenu
 *
 *  \par Function Description
 *  Type instance initializer for GedaOptionMenu, initializes a new empty
 *  GedaOptionMenu object.
 *
 *  \param [in] instance The GedaOptionMenu structure being initialized,
 *  \param [in] class    The GedaOptionMenu class being initializing.
 */
static void
geda_option_menu_instance_init(GTypeInstance *instance, void *class)
{
  GedaOptionMenu *option_menu;
  GtkWidget      *widget;

  option_menu = (GedaOptionMenu*)instance;
  widget      = GTK_WIDGET (instance);

  gtk_widget_set_can_focus (widget, TRUE);
  gtk_widget_set_can_default (widget, FALSE);
  gtk_widget_set_receives_default (widget, FALSE);

  option_menu->instance_type = geda_option_menu_get_type();

  option_menu->menu      = NULL;
  option_menu->menu_item = NULL;
  option_menu->width     = 0;
  option_menu->height    = 0;
}

/*! \brief Retrieve GedaOptionMenu's Type identifier.
 *
 *  \par Function Description
 *  Function to retrieve a #GedaOptionMenu Type identifier. When
 *  first called, the function registers a #GedaOptionMenu in the
 *  GedaType system to obtain an identifier that uniquely itentifies
 *  a GedaOptionMenu and returns the unsigned integer value.
 *  The retained value is returned on all Subsequent calls.
 *
 *  \return GedaType identifier associated with GedaOptionMenu.
 */
GedaType geda_option_menu_get_type (void)
{
  static GedaType geda_option_menu_type = 0;

  if (g_once_init_enter (&geda_option_menu_type)) {

    static const GTypeInfo info = {
      sizeof(GedaOptionMenuClass),
      NULL,                           /* base_init           */
      NULL,                           /* base_finalize       */
      geda_option_menu_class_init,    /* (GClassInitFunc)    */
      NULL,                           /* class_finalize      */
      NULL,                           /* class_data          */
      sizeof(GedaOptionMenu),
      0,                              /* n_preallocs         */
      geda_option_menu_instance_init  /* (GInstanceInitFunc) */
    };

    const char *string;
    GedaType    type;

    string = g_intern_static_string ("GedaOptionMenu");
    type   = g_type_register_static (GTK_TYPE_BUTTON, string, &info, 0);

    g_once_init_leave (&geda_option_menu_type, type);
  }

  return geda_option_menu_type;
}


bool is_a_geda_option_menu (GedaOptionMenu *option_menu)
{
  if (G_IS_OBJECT(option_menu)) {
    return (geda_option_menu_get_type() == option_menu->instance_type);
  }
  return FALSE;
}

GtkWidget*
geda_option_menu_new (void)
{
  return g_object_new (GEDA_TYPE_OPTION_MENU, NULL);
}

GtkWidget*
geda_option_menu_get_menu (GedaOptionMenu *option_menu)
{
  g_return_val_if_fail (GEDA_IS_OPTION_MENU (option_menu), NULL);

  return option_menu->menu;
}

static void
geda_option_menu_detacher (GtkWidget *widget, GedaMenu *menu)
{
  GedaOptionMenu *option_menu;

  g_return_if_fail (GEDA_IS_OPTION_MENU (widget));

  option_menu = GEDA_OPTION_MENU (widget);
  g_return_if_fail (option_menu->menu == (GtkWidget*) menu);

  geda_option_menu_remove_contents (option_menu);
  g_signal_handlers_disconnect_by_func (option_menu->menu,
                    geda_option_menu_selection_done,
                    option_menu);
  g_signal_handlers_disconnect_by_func (option_menu->menu,
                    geda_option_menu_calc_size,
                    option_menu);

  option_menu->menu = NULL;
  g_object_notify (G_OBJECT (option_menu), "menu");
}

void
geda_option_menu_set_menu (GedaOptionMenu *option_menu, GtkWidget *menu)
{
  g_return_if_fail (GEDA_IS_OPTION_MENU (option_menu));
  g_return_if_fail (GEDA_IS_MENU (menu));

  if (option_menu->menu != menu) {

    geda_option_menu_remove_menu (option_menu);

    option_menu->menu = menu;
    geda_menu_attach_to_widget (GEDA_MENU (menu),
                               GTK_WIDGET (option_menu),
                               geda_option_menu_detacher);

    geda_option_menu_calc_size (option_menu);

    g_signal_connect_after (option_menu->menu, "selection-done",
                            G_CALLBACK (geda_option_menu_selection_done),
                            option_menu);

    g_signal_connect_swapped (option_menu->menu, "size-request",
                              G_CALLBACK (geda_option_menu_calc_size),
                              option_menu);

    if (GTK_WIDGET (option_menu)->parent) {
      gtk_widget_queue_resize (GTK_WIDGET (option_menu));
    }
    geda_option_menu_update_contents (option_menu);

    g_object_notify (G_OBJECT (option_menu), "menu");
  }
}

void
geda_option_menu_remove_menu (GedaOptionMenu *option_menu)
{
  g_return_if_fail (GEDA_IS_OPTION_MENU (option_menu));

  if (option_menu->menu) {

    if (GEDA_MENU_SHELL (option_menu->menu)->active)
      geda_menu_shell_cancel (GEDA_MENU_SHELL (option_menu->menu));

    geda_menu_detach (GEDA_MENU (option_menu->menu));
  }
}

void
geda_option_menu_set_history (GedaOptionMenu *option_menu, unsigned int index)
{
  GtkWidget *menu_item;

  g_return_if_fail (GEDA_IS_OPTION_MENU (option_menu));

  if (option_menu->menu) {

    geda_menu_set_active (GEDA_MENU (option_menu->menu), index);
    menu_item = geda_menu_get_active (GEDA_MENU (option_menu->menu));

    if (menu_item != option_menu->menu_item)
      geda_option_menu_update_contents (option_menu);
  }
}

/**
 * geda_option_menu_get_history:
 * @option_menu: a #GedaOptionMenu
 *
 * Retrieves the index of the currently selected menu item. The menu
 * items are numbered from top to bottom, starting with 0.
 *
 * \return index of the selected menu item, or -1 if there are no menu items
 */
int
geda_option_menu_get_history (GedaOptionMenu *option_menu)
{
  GtkWidget *active_widget;

  g_return_val_if_fail (GEDA_IS_OPTION_MENU (option_menu), -1);

  if (option_menu->menu) {

    active_widget = geda_menu_get_active (GEDA_MENU (option_menu->menu));

    if (active_widget)
      return g_list_index (GEDA_MENU_SHELL (option_menu->menu)->children,
                           active_widget);
      else
        return -1;
  }
  else
    return -1;
}

static void
geda_option_menu_set_property (GObject      *object,
                               unsigned int  prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
  GedaOptionMenu *option_menu = GEDA_OPTION_MENU (object);

  switch (prop_id) {
    case PROP_MENU:
      geda_option_menu_set_menu (option_menu, g_value_get_object (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
geda_option_menu_get_property (GObject      *object,
                               unsigned int  prop_id,
                               GValue       *value,
                               GParamSpec   *pspec)
{
  GedaOptionMenu *option_menu = GEDA_OPTION_MENU (object);

  switch (prop_id) {
    case PROP_MENU:
      g_value_set_object (value, option_menu->menu);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
geda_option_menu_destroy (GtkObject *object)
{
  GedaOptionMenu *option_menu = GEDA_OPTION_MENU (object);

  if (option_menu->menu) {
    gtk_widget_destroy (option_menu->menu);
  }

  GTK_OBJECT_CLASS (geda_option_menu_parent_class)->destroy (object);
}

static void
geda_option_menu_get_props (GedaOptionMenu      *option_menu,
                            GedaOptionMenuProps *props)
{
  GtkRequisition *indicator_size;
  GtkBorder *indicator_spacing;

  gtk_widget_style_get (GTK_WIDGET (option_menu),
                        "indicator-size", &indicator_size,
                        "indicator-spacing", &indicator_spacing,
                        "interior-focus", &props->interior_focus,
                        "focus-line-width", &props->focus_width,
                        "focus-padding", &props->focus_pad,
                        NULL);

  if (indicator_size) {
    props->indicator_size = *indicator_size;
  }
  else {
    props->indicator_size = default_props.indicator_size;
  }

  if (indicator_spacing) {
    props->indicator_spacing = *indicator_spacing;
  }
  else {
    props->indicator_spacing = default_props.indicator_spacing;
  }

  gtk_requisition_free (indicator_size);
  gtk_border_free (indicator_spacing);
}

static void
geda_option_menu_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GedaOptionMenu *option_menu = GEDA_OPTION_MENU (widget);
  GedaOptionMenuProps props;
  int tmp;
  GtkRequisition child_requisition = { 0, 0 };

  geda_option_menu_get_props (option_menu, &props);

  if (GTK_BIN(option_menu)->child &&
      gtk_widget_get_visible (GTK_BIN(option_menu)->child))
  {
    gtk_widget_size_request (GTK_BIN (option_menu)->child, &child_requisition);

    requisition->width += child_requisition.width;
    requisition->height += child_requisition.height;
  }

  requisition->width = ((GTK_CONTAINER (widget)->border_width +
                         GTK_WIDGET (widget)->style->xthickness + props.focus_pad) * 2 +
                         MAX (child_requisition.width, option_menu->width) +
                         props.indicator_size.width +
                         props.indicator_spacing.left + props.indicator_spacing.right +
                         CHILD_LEFT_SPACING + CHILD_RIGHT_SPACING + props.focus_width * 2);

  requisition->height = ((GTK_CONTAINER (widget)->border_width +

  GTK_WIDGET (widget)->style->ythickness + props.focus_pad) * 2 +
              MAX (child_requisition.height, option_menu->height) +
              CHILD_TOP_SPACING + CHILD_BOTTOM_SPACING + props.focus_width * 2);

  tmp = (requisition->height - MAX (child_requisition.height, option_menu->height) +
         props.indicator_size.height + props.indicator_spacing.top +
         props.indicator_spacing.bottom);

  requisition->height = MAX (requisition->height, tmp);
}

static void
geda_option_menu_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GtkWidget *child;
  GtkButton *button = GTK_BUTTON (widget);
  GtkAllocation child_allocation;
  GedaOptionMenuProps props;
  int border_width;

  geda_option_menu_get_props (GEDA_OPTION_MENU (widget), &props);
  border_width = GTK_CONTAINER (widget)->border_width;

  widget->allocation = *allocation;

  if (gtk_widget_get_realized (widget)) {
    gdk_window_move_resize (button->event_window,
                            allocation->x + border_width, allocation->y + border_width,
                            allocation->width - border_width * 2, allocation->height - border_width * 2);
  }

  child = GTK_BIN (widget)->child;

  if (child && gtk_widget_get_visible (child)) {

    int xthickness = GTK_WIDGET (widget)->style->xthickness;
    int ythickness = GTK_WIDGET (widget)->style->ythickness;

    child_allocation.x = widget->allocation.x + border_width + xthickness + props.focus_width + props.focus_pad + CHILD_LEFT_SPACING;
    child_allocation.y = widget->allocation.y + border_width + ythickness + props.focus_width + props.focus_pad + CHILD_TOP_SPACING;
    child_allocation.width = MAX (1, allocation->width - (border_width + xthickness + props.focus_width + props.focus_pad) * 2 -
    props.indicator_size.width - props.indicator_spacing.left - props.indicator_spacing.right -
    CHILD_LEFT_SPACING - CHILD_RIGHT_SPACING);
    child_allocation.height = MAX (1, allocation->height - (border_width + ythickness + props.focus_width + props.focus_pad) * 2 -
    CHILD_TOP_SPACING - CHILD_BOTTOM_SPACING);

    if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL)
      child_allocation.x += props.indicator_size.width +
                            props.indicator_spacing.left +
                            props.indicator_spacing.right;

    gtk_widget_size_allocate (child, &child_allocation);
  }
}

static void
geda_option_menu_paint (GtkWidget *widget, GdkRectangle *area)
{
  GdkRectangle button_area;
  GedaOptionMenuProps props;
  int border_width;
  int tab_x;

  g_return_if_fail (GEDA_IS_OPTION_MENU (widget));
  g_return_if_fail (area != NULL);

  if (GTK_WIDGET_DRAWABLE (widget)) {

    border_width = GTK_CONTAINER (widget)->border_width;
    geda_option_menu_get_props (GEDA_OPTION_MENU (widget), &props);

    button_area.x = widget->allocation.x + border_width;
    button_area.y = widget->allocation.y + border_width;
    button_area.width = widget->allocation.width - 2 * border_width;
    button_area.height = widget->allocation.height - 2 * border_width;

    if (!props.interior_focus && gtk_widget_has_focus (widget)) {

      button_area.x += props.focus_width + props.focus_pad;
      button_area.y += props.focus_width + props.focus_pad;
      button_area.width -= 2 * (props.focus_width + props.focus_pad);
      button_area.height -= 2 * (props.focus_width + props.focus_pad);
    }

    gtk_paint_box (widget->style, widget->window,
                   gtk_widget_get_state (widget), GTK_SHADOW_OUT,
                   area, widget, "optionmenu",
                   button_area.x, button_area.y,
                   button_area.width, button_area.height);

    if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL)
      tab_x = button_area.x + props.indicator_spacing.right +
      widget->style->xthickness;
    else
      tab_x = button_area.x + button_area.width -
      props.indicator_size.width - props.indicator_spacing.right -
      widget->style->xthickness;

    gtk_paint_tab (widget->style, widget->window,
                   gtk_widget_get_state (widget), GTK_SHADOW_OUT,
                   area, widget, "optionmenutab",
                   tab_x,
                   button_area.y + (button_area.height - props.indicator_size.height) / 2,
                   props.indicator_size.width, props.indicator_size.height);

    if (gtk_widget_has_focus (widget)) {

      if (props.interior_focus) {

        button_area.x += widget->style->xthickness + props.focus_pad;
        button_area.y += widget->style->ythickness + props.focus_pad;
        button_area.width -= 2 * (widget->style->xthickness + props.focus_pad) +
        props.indicator_spacing.left +
        props.indicator_spacing.right +
        props.indicator_size.width;
        button_area.height -= 2 * (widget->style->ythickness + props.focus_pad);
        if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL)
          button_area.x += props.indicator_spacing.left +
          props.indicator_spacing.right +
          props.indicator_size.width;
      }
      else {

        button_area.x -= props.focus_width + props.focus_pad;
        button_area.y -= props.focus_width + props.focus_pad;
        button_area.width += 2 * (props.focus_width + props.focus_pad);
        button_area.height += 2 * (props.focus_width + props.focus_pad);
      }

      gtk_paint_focus (widget->style, widget->window, gtk_widget_get_state (widget),
                       area, widget, "button",
                       button_area.x,
                       button_area.y,
                       button_area.width,
                       button_area.height);
    }
  }
}

static int
geda_option_menu_expose (GtkWidget *widget, GdkEventExpose *event)
{
  g_return_val_if_fail (GEDA_IS_OPTION_MENU (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if (GTK_WIDGET_DRAWABLE (widget)) {

    geda_option_menu_paint (widget, &event->area);

    if (GTK_BIN (widget)->child) {
      gtk_container_propagate_expose (GTK_CONTAINER (widget),
                                      GTK_BIN (widget)->child,
                                      event);
    }
  }

  return FALSE;
}

static int
geda_option_menu_button_press (GtkWidget *widget, GdkEventButton *event)
{
  GedaOptionMenu *option_menu;
  GtkWidget *menu_item;

  g_return_val_if_fail (GEDA_IS_OPTION_MENU (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  option_menu = GEDA_OPTION_MENU (widget);

  if ((event->type == GDK_BUTTON_PRESS) &&
      (event->button == 1))
  {
    geda_option_menu_remove_contents (option_menu);
    geda_menu_popup (GEDA_MENU (option_menu->menu), NULL, NULL,
                    geda_option_menu_position, option_menu,
                    event->button, event->time);
    menu_item = geda_menu_get_active (GEDA_MENU (option_menu->menu));
    if (menu_item)
      geda_menu_shell_select_item (GEDA_MENU_SHELL (option_menu->menu), menu_item);
    return TRUE;
  }

  return FALSE;
}

static int
geda_option_menu_key_press (GtkWidget *widget, GdkEventKey *event)
{
  GedaOptionMenu *option_menu;
  GtkWidget      *menu_item;

  g_return_val_if_fail (GEDA_IS_OPTION_MENU (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  option_menu = GEDA_OPTION_MENU (widget);

  switch (event->keyval) {

    case GDK_KP_Space:
    case GDK_space:
      geda_option_menu_remove_contents (option_menu);
      geda_menu_popup (GEDA_MENU (option_menu->menu), NULL, NULL,
              geda_option_menu_position, option_menu,
              0, event->time);
      menu_item = geda_menu_get_active (GEDA_MENU (option_menu->menu));
      if (menu_item)
    geda_menu_shell_select_item (GEDA_MENU_SHELL (option_menu->menu), menu_item);
      return TRUE;
    }

  return FALSE;
}

static void
geda_option_menu_selection_done (GedaMenuShell  *menu_shell,
                                 GedaOptionMenu *option_menu)
{
  g_return_if_fail (menu_shell != NULL);
  g_return_if_fail (GEDA_IS_OPTION_MENU (option_menu));

  geda_option_menu_update_contents (option_menu);
}

static void
geda_option_menu_changed (GedaOptionMenu *option_menu)
{
  g_return_if_fail (GEDA_IS_OPTION_MENU (option_menu));

  g_signal_emit (option_menu, signals[CHANGED], 0);
}

static void
geda_option_menu_select_first_sensitive (GedaOptionMenu *option_menu)
{
  if (option_menu->menu) {

    GList *children = GEDA_MENU_SHELL (option_menu->menu)->children;
    int index = 0;

    while (children) {

      if (gtk_widget_get_sensitive (children->data)) {

        geda_option_menu_set_history (option_menu, index);
        return;
      }

      children = children->next;
      index++;
    }
  }
}

static void
geda_option_menu_item_state_changed_cb (GtkWidget      *widget,
                                        GtkStateType    previous_state,
                                        GedaOptionMenu *option_menu)
{
  GtkWidget *child = GTK_BIN (option_menu)->child;

  if (child && gtk_widget_get_sensitive (child) != gtk_widget_is_sensitive (widget))
    gtk_widget_set_sensitive (child, gtk_widget_is_sensitive (widget));
}

static void
geda_option_menu_item_destroy_cb (GtkWidget      *widget,
                                  GedaOptionMenu *option_menu)
{
  GtkWidget *child = GTK_BIN (option_menu)->child;

  if (child) {

    g_object_ref (child);
    geda_option_menu_remove_contents (option_menu);
    gtk_widget_destroy (child);
    g_object_unref (child);

    geda_option_menu_select_first_sensitive (option_menu);
  }
}

static void
geda_option_menu_update_contents (GedaOptionMenu *option_menu)
{
  GtkWidget *child;
  GtkRequisition child_requisition;

  g_return_if_fail (GEDA_IS_OPTION_MENU (option_menu));

  if (option_menu->menu) {

    GtkWidget *old_item = option_menu->menu_item;

    geda_option_menu_remove_contents (option_menu);

    option_menu->menu_item = geda_menu_get_active (GEDA_MENU(option_menu->menu));

    if (option_menu->menu_item) {

      g_object_ref (option_menu->menu_item);
      child = GTK_BIN (option_menu->menu_item)->child;

      if (child) {

        if (!gtk_widget_is_sensitive (option_menu->menu_item))
          gtk_widget_set_sensitive (child, FALSE);
        gtk_widget_reparent (child, GTK_WIDGET (option_menu));
      }

      g_signal_connect (option_menu->menu_item, "state-changed",
                        G_CALLBACK (geda_option_menu_item_state_changed_cb), option_menu);
      g_signal_connect (option_menu->menu_item, "destroy",
                        G_CALLBACK (geda_option_menu_item_destroy_cb), option_menu);

      gtk_widget_size_request (child, &child_requisition);
      gtk_widget_size_allocate (GTK_WIDGET (option_menu),
                                &(GTK_WIDGET (option_menu)->allocation));

      if (GTK_WIDGET_DRAWABLE (option_menu))
        gtk_widget_queue_draw (GTK_WIDGET (option_menu));
    }

    if (old_item != option_menu->menu_item)
      geda_option_menu_changed (option_menu);
  }
}

static void
geda_option_menu_remove_contents (GedaOptionMenu *option_menu)
{
  GtkWidget *child;

  g_return_if_fail (GEDA_IS_OPTION_MENU (option_menu));

  if (option_menu->menu_item) {

    child = GTK_BIN (option_menu)->child;

    if (child) {

      gtk_widget_set_sensitive (child, TRUE);
      gtk_widget_set_state (child, GTK_STATE_NORMAL);
      gtk_widget_reparent (child, option_menu->menu_item);
    }

    g_signal_handlers_disconnect_by_func (option_menu->menu_item,
                                          geda_option_menu_item_state_changed_cb,
                                          option_menu);
    g_signal_handlers_disconnect_by_func (option_menu->menu_item,
                                          geda_option_menu_item_destroy_cb,
                                          option_menu);

    g_object_unref (option_menu->menu_item);
    option_menu->menu_item = NULL;
  }
}

static void
geda_option_menu_calc_size (GedaOptionMenu *option_menu)
{
  GtkWidget *child;
  GList     *children;

  GtkRequisition child_requisition;

  int old_width  = option_menu->width;
  int old_height = option_menu->height;

  g_return_if_fail (GEDA_IS_OPTION_MENU (option_menu));

  option_menu->width = 0;
  option_menu->height = 0;

  if (option_menu->menu) {

    children = GEDA_MENU_SHELL (option_menu->menu)->children;

    while (children) {

      child = children->data;
      children = children->next;

      if (gtk_widget_get_visible (child)) {

        GtkWidget *inner = GTK_BIN (child)->child;

        if (inner) {

          gtk_widget_size_request (inner, &child_requisition);

          option_menu->width = MAX (option_menu->width, child_requisition.width);
          option_menu->height = MAX (option_menu->height, child_requisition.height);
        }
      }
    }
  }

  if (old_width != option_menu->width || old_height != option_menu->height)
    gtk_widget_queue_resize (GTK_WIDGET (option_menu));
}

static void
geda_option_menu_position (GedaMenu  *menu,
                           int       *x,
                           int       *y,
                           bool      *push_in,
                           void      *user_data)
{
  GedaOptionMenu *option_menu;
  GtkWidget      *active;
  GtkWidget      *child;
  GtkWidget      *widget;
  GList          *children;
  GtkRequisition  requisition;

  int screen_width;
  int menu_xpos;
  int menu_ypos;
  int menu_width;

  g_return_if_fail (GEDA_IS_OPTION_MENU (user_data));

  option_menu = GEDA_OPTION_MENU (user_data);
  widget = GTK_WIDGET (option_menu);

  gtk_widget_get_child_requisition (GTK_WIDGET (menu), &requisition);
  menu_width = requisition.width;

  active = geda_menu_get_active (GEDA_MENU (option_menu->menu));
  gdk_window_get_origin (widget->window, &menu_xpos, &menu_ypos);

  /* set combo box type hint for menu popup */
  gtk_window_set_type_hint (GTK_WINDOW (GEDA_MENU (option_menu->menu)->toplevel),
                            GDK_WINDOW_TYPE_HINT_COMBO);

  menu_xpos += widget->allocation.x;
  menu_ypos += widget->allocation.y + widget->allocation.height / 2 - 2;

  if (active != NULL) {

    gtk_widget_get_child_requisition (active, &requisition);
    menu_ypos -= requisition.height / 2;
  }

  children = GEDA_MENU_SHELL (option_menu->menu)->children;
  while (children) {

    child = children->data;

    if (active == child)
      break;

    if (gtk_widget_get_visible (child)) {

      gtk_widget_get_child_requisition (child, &requisition);
      menu_ypos -= requisition.height;
    }

    children = children->next;
  }

  if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL) {
    menu_xpos = menu_xpos + widget->allocation.width - menu_width;
  }

  /* Clamp the position on screen */
  screen_width = gdk_screen_get_width (gtk_widget_get_screen (widget));

  if (menu_xpos < 0) {
    menu_xpos = 0;
  }
  else if ((menu_xpos + menu_width) > screen_width) {
    menu_xpos -= ((menu_xpos + menu_width) - screen_width);
  }

  *x = menu_xpos;
  *y = menu_ypos;
  *push_in = TRUE;
}


static void
geda_option_menu_show_all (GtkWidget *widget)
{
  GtkContainer   *container;
  GedaOptionMenu *option_menu;

  g_return_if_fail (GEDA_IS_OPTION_MENU (widget));
  container = GTK_CONTAINER (widget);
  option_menu = GEDA_OPTION_MENU (widget);

  gtk_widget_show (widget);
  gtk_container_foreach (container, (GtkCallback) gtk_widget_show_all, NULL);

  if (option_menu->menu) {
    gtk_widget_show_all (option_menu->menu);
  }

  if (option_menu->menu_item) {
    gtk_widget_show_all (option_menu->menu_item);
  }
}

static void
geda_option_menu_hide_all (GtkWidget *widget)
{
  GtkContainer *container;

  g_return_if_fail (GEDA_IS_OPTION_MENU (widget));
  container = GTK_CONTAINER (widget);

  gtk_widget_hide (widget);
  gtk_container_foreach (container, (GtkCallback) gtk_widget_hide_all, NULL);
}

static bool
geda_option_menu_mnemonic_activate (GtkWidget *widget, bool group_cycling)
{
  gtk_widget_grab_focus (widget);
  return TRUE;
}

static int
geda_option_menu_scroll_event (GtkWidget *widget, GdkEventScroll *event)
{
  GedaOptionMenu *option_menu = GEDA_OPTION_MENU (widget);
  int index;
  int n_children;
  int index_dir;
  GList *l;
  GedaMenuItem *item;

  index = geda_option_menu_get_history (option_menu);

  if (index != -1) {

    n_children = g_list_length (GEDA_MENU_SHELL(option_menu->menu)->children);

    if (event->direction == GDK_SCROLL_UP)
      index_dir = -1;
    else
      index_dir = 1;


    while (TRUE) {

      index += index_dir;

      if (index < 0)
        break;

      if (index >= n_children)
        break;

      l    = g_list_nth (GEDA_MENU_SHELL(option_menu->menu)->children, index);
      item = GEDA_MENU_ITEM (l->data);

      if (gtk_widget_get_visible (GTK_WIDGET (item)) &&
        gtk_widget_is_sensitive (GTK_WIDGET (item)))
      {
        geda_option_menu_set_history (option_menu, index);
        geda_menu_item_activate (GEDA_MENU_ITEM (item));
        break;
      }

    }
  }

  return TRUE;
}
